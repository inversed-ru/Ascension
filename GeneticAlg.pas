{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit GeneticAlg; ////////////////////////////////////////////////////////////////////
{
>> Version: 2.0

>> Description
   Implementation of a steady state genetic algorithm featuring various selection
   and replacement methods.

>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> How to use
   The user must supply mutation, crossover and solution distance routines via the
   problem definition module.

>> ToDo
    - Bring back features from version 1.5 after testing
    - More stopping criteria:
       - Automatic convergence detection
       - No improvement of best or average score for some N of generations
       - Score standard deviation < threshold
       - Average population distance < threshold
       - Small rate of improvements
    - Correlation measures for mutation and crossover operators - corelation between
      the parents' scores and the child's score

>> Changelog
   2.0 : 2018.09.16  - Many experimental and untested features
                     ~ Cleaned up
                     ~ Freepascal compatibility
   1.5 : 2018.08.24  + Mixed, mixed compound and time dependent inverse rank
                       replacement options
                     +  thermal child acceptance option
                     ~ Selection option instead of the linear topology flag
                     + Fitness-uniform selection
   1.4 : 2015.08.11  + GA2, generational version of the genetic algorithm
   1.3 : 2011.08.28  - Split dimorphic GA into a separate unit
   1.2 : 2011.08.24  + Test implementation of a dimorphic GA
                     ~ Modified reproduction
   1.1 : 2011.08.08  + New reproduction and replacement types
   1.0 : 2011.03.28  ~ Added unit header
                     ~ Slightly reorganized
   0.0 : 2010.11.14  + Initial version
   Notation: + added, - removed, * fixed, ~ changed
}
{$MINENUMSIZE 4}
interface ///////////////////////////////////////////////////////////////////////////
uses
      Math,
      InvSys,
      Arrays,
      Sorting,
      Statistics,
      StringUtils,
      Formatting,
      Common,
      SolutionLists,
      Acceptance,
      Problem,
      Messages;

type
      TReplacement = (
         repWorst = 0, repInvRank, 
         repWorstParent, repRandParent, repSimParent,
         repInflux
         );

      TStoppingCriterion = (scGenerations = 0, scNFE, scScore);

      TGAAcceptance = (gaaElitist = 0, gaaUnconditional);

      TSelection = (selRankProp = 0, selDist, selDistToBest);

      TGAParameters =
         record
         PopSize           :  Integer;
         ReplaceP,
          SelectP          :  Real;
         Selection         :  TSelection;
         MutationRate      :  Real;
         Replacement       :  TReplacement;
         ChildAcceptance   :  TGAAcceptance;
         Stopping          :  TStoppingCriterion;
         MaxGens, MaxNFE   :  Integer;
         ScoreToReach      :  TScore;
         end;

      TGAStatus =
         record
         GenSave,
         GenStatus      :  Integer;
         ShowMessage    :  ProcMessage;
         end;

const
      NoGAStatus  :  TGAStatus =
      (  GenSave        :  0;
         GenStatus      :  0;
         ShowMessage    :  nil;
      );

// Run the genetic algorithm with specified Params, return the Best solution found 
// and the search Stats, report and save the progress according to Status
procedure GeneticAlgorithm(
   var   Best     :  TSolution;
   var   Stats    :  TRunStats;
   const Params   :  TGAParameters;
   const Status   :  TGAStatus);

implementation //////////////////////////////////////////////////////////////////////
uses
      ExtraMath,
      RandVars,
      IniConfigs,
      StringLists;

type
      TSolutionInfo =
         record
         Age      :  Integer;
         end;

      TPopulation =
         record
         SolList        :  TSolutionList;
         SolutionsInfo  :  array of TSolutionInfo;
         NFE,
         NFEPrevGen,
         Generation,
         GenLastUpdate, 
         Improvements   :  Integer;
         TMax, TMin     :  Real;
         end;

      TReprodInfo =
         record
         Parent1,
         Parent2  :  TSolutionIndex;
         end;

      TPopParam = (
         ppN, ppIteration, ppGeneration, ppLastUpdate, ppImprovements);

const
      PopParamName : array [TPopParam] of AnsiString =
         ('Size', 'NFE', 'Generation', 'LastUpdate', 'Accepted');

      PathPopParam     = 'Parameters.txt';
      PathInfo         = 'Info.txt';
      PathStatus       = 'GA_Status.txt';
      NameBest         = 'GA_Best';
      DirPopulation    = 'Population';

{-----------------------<< Population operations >>---------------------------------}

// Initialize the Population with given Params
procedure InitPopulation(
   var   Population  :  TPopulation;
   const Params      :  TGAParameters);
   var
         i           :  Integer;
   begin
   with Population, Params do
      begin
      // Create initial population, initialize solution info
      NewSolList(SolList, PopSize);
      SetLength(SolutionsInfo, PopSize);
      for i := 0 to PopSize - 1 do
         SolutionsInfo[i].Age := 0;

      // Initialize parameters
      NFE           := 0;
      NFEPrevGen    := 0;
      Generation    := 0;
      GenLastUpdate := 0;
      Improvements  := 0;
      end;
   end;


// Increase the age of every solution in a Population
procedure IncreaseAge(
   var   Population  :  TPopulation);
   var
         i           :  Integer;
   begin
   with Population do
      for i := 0 to SolList.N - 1 do
         Inc(SolutionsInfo[i].Age);
   end;

{-----------------------<< Population save / load >>--------------------------------}

// Write a population Parameter with a given Value to fileParam
procedure WriteParameter(
   var   fileParam   :  Text;
         Parameter   :  TPopParam;
         Value       :  Integer);
   const
         ParamFormat = '{<16}= {}';
   begin
   WriteLn( fileParam, Format(ParamFormat, [PopParamName[Parameter], Value]) );
   end;


// Return the path to the solution with a given Index located in Dir
function SolutionPath(
         Index :  Integer;
   const Dir   :  AnsiString
         )     :  AnsiString;
   const
         IndFormat = '{}{>08}';
   begin
   Result := Format(IndFormat, [Dir, Index + 1]);
   end;


// Save the Population to Dir, show any messages via ShowMessage procedure
function SavePopulation(
         Dir         :  AnsiString;
   const Population  :  TPopulation;
         ShowMessage :  ProcMessage
         )           :  Boolean;
   var
         i, j        :  TSolutionIndex;
         fileParams,
         fileInfo    :  Text;
   const
         MsgSaved    =  'Population saved';
         ErrorSave   =  'Failed to save population';
   begin
   Result := Fail;
   while True do with Population, SolList do
      begin
      // Save the best solution
      if TrySaveSolution(NameBest, Best, ShowMessage) = Fail then
   {<---}break;

      // Save the population parameters
      EnforceDirSep(Dir);
      if OpenWrite(fileParams, Dir + PathPopParam) = Success then
         begin
         WriteParameter(fileParams, ppN,            N);
         WriteParameter(fileParams, ppIteration,    NFE);
         WriteParameter(fileParams, ppGeneration,   Generation);
         WriteParameter(fileParams, ppLastUpdate,   GenLastUpdate);
         WriteParameter(fileParams, ppImprovements, Improvements);
         Close(fileParams);
         end
      else
   {<---}break;

      // Save Solution info
      if OpenWrite(fileInfo, Dir + PathInfo) = Success then
         begin
         WriteLn(fileInfo, 'Index', Tab, 'Score', Tab, 'Age');
         for i := 0 to N - 1 do
            begin
            j := RankIndex[i];
            WriteLn(fileInfo,
               i + 1,                Tab,
               SolList._[j].Score,   Tab,
               SolutionsInfo[j].Age  );
            end;
         Close(fileInfo);
         end
      else
   {<---}break;

      // Save all Solutions
      Result := Success;
      for i := 0 to N - 1 do
         if TrySaveSolution(
            SolutionPath(i, Dir), SolList._[ RankIndex[i] ], ShowMessage
            ) = Fail then
            begin
            Result := Fail;
      {<---}break;
            end;
   {<}break;
      end;

   // Display the status message
   if Result = Success then
      TryShowMessage(MsgSaved,  ShowMessage) else
      ShowError(ErrorSave, ShowMessage);
   end;


// Load the Population from Dir, show any messages via ShowMessage procedure
function LoadPopulation(
   var   Population  :  TPopulation;
         Dir         :  AnsiString;
         ShowMessage :  ProcMessage
         )           :  Boolean;
   var
         fileInfo    :  Text;
         i,
         DummyIndex  :  Integer;
         DummyScore  :  TScore;
         LT          :  TLoadTable;
         Errors      :  TStringList;
   const
         MsgLoaded   =  'Population loaded';
         ErrorLoad   =  'Failed to load population';
   begin
   Result := Fail;
   while True do with Population, SolList do
      begin
      // Read the population parameters
      InitLoadTable(LT);
      AddLoadParam(LT, N,               PopParamName[ppN],            nil);
      AddLoadParam(LT, NFE,             PopParamName[ppIteration],    nil);
      AddLoadParam(LT, Generation,      PopParamName[ppGeneration],   nil);
      AddLoadParam(LT, GenLastUpdate,   PopParamName[ppLastUpdate],   nil);
      AddLoadParam(LT, Improvements,    PopParamName[ppImprovements], nil);
      EnforceDirSep(Dir);
      LoadFromConfig(LT, Dir + PathPopParam, Errors);
      if Errors.N <> 0 then
   {<---}break;

      // Load Solution info
      if OpenRead(fileInfo, Dir + PathInfo) = Success then
         begin
         ReadLn(fileInfo);
         for i := 0 to N - 1 do
            ReadLn(fileInfo, DummyIndex, DummyScore, SolutionsInfo[i].Age);
         Close(fileInfo);
         end
      else
         break;

      // Load the population
      Result := Success;
      InitSolList(SolList, N);
      for i := 0 to N - 1 do
         if TryLoadSolution(
            SolList._[i], SolutionPath(i, Dir), ShowMessage
            ) = Fail then
            begin
            Result := Fail;
      {<---}break;
            end;
      if Result = Success then
         SetRanking(SolList);
   {<}break;
      end;

   // Display the status message
   if Result = Success then
      TryShowMessage(MsgLoaded, ShowMessage) else
      ShowError(ErrorLoad, ShowMessage);
   end;

{-----------------------<< Reproduction >>------------------------------------------}

// Return the index of a random solution selected from Population 
// via rank power law with parameter P
function RandPopIndex(
   const Population  :  TPopulation;
   const P           :  Real
         )           :  TSolutionIndex;
   begin
   Result := RandSolIndex(Population.SolList, P);
   end;


// Return two indices of distinct random solutions selected from Population  
// according to the selection setting in Params
procedure TwoRandPopIndices(
   var   i1, i2      :  TSolutionIndex;
   const Population  :  TPopulation;
   const Params      :  TGAParameters);
   var
         i, j, k, M  :  Integer;
         A           :  TIntArrayN;
         D, BestD    :  Real;
         DistToBest  :  TRealArray;
         Order       :  TIntArray;
   begin
   case Params.Selection of
      selRankProp:
         TwoRandSolIndices(i1, i2, Population.SolList, Params.SelectP);
      selDist:
         begin
         InitArrayN(A);
         M := Max(2, Round(Abs(Params.SelectP)));
         repeat
            k := Random(Population.SolList.N);
            AppednUnique(A, k);
         until A.N = M;
         i1 := A._[0];
         i2 := A._[1];
         BestD := Distance(
            Population.SolList._[i1], 
            Population.SolList._[i2]);
         for i := 2 to A.N - 1 do
            for j := 0 to i - 1 do
               begin
               D := Distance(
                  Population.SolList._[ A._[i] ],
                  Population.SolList._[ A._[j] ]);
               if ((Params.SelectP > 0) and (D > BestD)) or
                  ((Params.SelectP < 0) and (D < BestD)) then
                  begin
                  BestD := D;
                  i1 := A._[i];
                  i2 := A._[j];
                  end;
               end;
         end;
      selDistToBest:
         begin
         // #COPYPASTA
         InitArrayN(A);
         M := Max(2, Round(Abs(Params.SelectP)));
         repeat
            k := Random(Population.SolList.N);
            AppednUnique(A, k);
         until A.N = M;
         InitArray(DistToBest, {Len:} M, {Value:} 0);
         for i := 0 to M - 1 do
            DistToBest[i] := Distance(
               Population.SolList._[ A._[i] ],
               Population.SolList.Best);
         if Params.SelectP > 0 then
            OrderRealArray(Order, DistToBest, soDescending) else
            OrderRealArray(Order, DistToBest, soAscending);
         i1 := A._[ Order[0] ];
         i2 := A._[ Order[1] ];
         end;
      else
         Assert(False);
      end;
   end;


// Create the Child via crossover of two distinct random solutions 
// selected from Population according to the selection setting in Params,
// update ReprodInfo. 
procedure Reproduce(
   var   Child       :  TSolution;
   var   ReprodInfo  :  TReprodInfo;
   const Population  :  TPopulation;
   const Params      :  TGAParameters);
   begin
   with ReprodInfo, Population do
      begin
      TwoRandPopIndices(Parent1, Parent2, Population, Params);
      Crossover(Child, SolList._[Parent1], SolList._[Parent2], {RecalcScore:} False);
      Mutate(Child);
      end;
   end;

{-----------------------<< Replacement >>-------------------------------------------}

// Replace a solution with a given Index in the Population by the Child,
// update BestSoFar, display a message via ShowMessage in case of a new best score
procedure ReplaceInPopulation(
   var   Population  :  TPopulation;
   var   BestSoFar   :  TSolution;
         Index       :  TSolutionIndex;
   const Child       :  TSolution;
         ShowMessage :  ProcMessage);
   begin
   with Population do
      begin
      SolutionsInfo[Index].Age := 0;
      if CompareScores(Child, SolList._[Index]) = scoreBetter then
         Inc(Improvements);
      ReplaceSolution(SolList, Index, Child, {IfBetter:} False, nil);
      end;
   TryUpdateBest(BestSoFar, Child, ShowMessage);
   end;


// Return the index of a random parent listed in ReprodInfo
function RandomParent(
   const ReprodInfo  :  TReprodInfo
         )           :  TSolutionIndex;
   begin
   if RandBool then
      Result := ReprodInfo.Parent1 else
      Result := ReprodInfo.Parent2
   end;


// Return the index of the worst parent listed in ReprodInfo,
// break ties randomly
function WorstParent(
   const Population  :  TPopulation;
   const ReprodInfo  :  TReprodInfo
         )           :  TSolutionIndex;
   var
         Comparison  :  TScoreComparison;
   begin
   with Population, ReprodInfo do
      begin
      Comparison := CompareScores(SolList._[Parent1], SolList._[Parent2]);
      if Comparison = scoreWorse then
         Result := Parent1
      else if Comparison = scoreBetter then
         Result := Parent2
      else
         Result := RandomParent(ReprodInfo);
      end;
   end;


// Try replacing a solution from the Population by a Child provided its ReprodInfo. 
// Which solution is replaced and whether the replacement actually takes place is 
// determined by Params. Update BestSoFar, display a message via ShowMessage in 
// case of a new best score.
procedure Replacement(
   var   Population        :  TPopulation;
   var   BestSoFar         :  TSolution;
   const Child             :  TSolution;
   const ReprodInfo        :  TReprodInfo;
   const Params            :  TGAParameters;
         ShowMessage       :  ProcMessage);
   var
         ReplaceIndex,
         ReplaceNew        :  TSolutionIndex;
         WorkSol           :  TSolution;
         Accept            :  Boolean;
   begin
   with Population, SolList, ReprodInfo do
      begin
      // Determine whom to replace
      case Params.Replacement of
         // Worst in population
         repWorst:
            ReplaceIndex := RankIndex[N - 1];
            
         // Inverse rank-proportional
         repInvRank:
            ReplaceIndex := RandPopIndex(Population, -Params.ReplaceP);
            
         // Random parent
         repRandParent:
            ReplaceIndex := RandomParent(ReprodInfo);
            
         // Worst parent
         repWorstParent:
            ReplaceIndex := WorstParent(Population, ReprodInfo);
            
         // Similar parent
         repSimParent:
            case SimilarSolution(Child, SolList._[Parent1], SolList._[Parent2]) of
               0:    ReplaceIndex := Parent1;
               1:    ReplaceIndex := Parent2;
               else  Assert(False);
               end;

         // Replace worst parent. When crossover improves over both parents,
         // also replace the other parent with a new solution.
         repInflux:
            begin
            ReplaceIndex := WorstParent(Population, ReprodInfo);
            if (CompareScores(Child, SolList._[Parent1]) <> scoreWorse) and
               (CompareScores(Child, SolList._[Parent2]) <> scoreWorse) then
               begin
               NewSolution(WorkSol);
               Inc(NFE);
               if ReplaceIndex = Parent1 then
                  ReplaceNew  := Parent2
               else
                  ReplaceNew  := Parent1;
               ReplaceInPopulation(
                  Population, BestSoFar, ReplaceNew, WorkSol, ShowMessage);
               end;
            end;
         else
            Assert(False);
            ReplaceIndex := -1;
         end;

      // Decide whether to accept the child
      case Params.ChildAcceptance of
         gaaElitist:
            Accept :=  CompareScores(Child, SolList._[ReplaceIndex]) <> scoreWorse;
         gaaUnconditional:
            Accept := True;
         else
            Assert(False);
            Accept := False;
         end;
      if Accept then
         ReplaceInPopulation(
            Population, BestSoFar, ReplaceIndex, Child, ShowMessage);
      end;
   end;

{-----------------------<< GA itself >>---------------------------------------------}

// Write the header of the status file FileStatus
procedure WriteHeader(
   var   FileStatus  :  Text);
   begin
   WriteLn(fileStatus,
      'Gen',            Tab,
      'NFE',            Tab,
      'Improvements',   Tab,
      'BestScore',      Tab,
      'MeanScore',      Tab,
      'MedianScore',    Tab,
      'WorstScore',     Tab,
      'StdDevScore',    Tab,
      'MeanDist',       Tab,
      'MeanDistToBest'  );
   end;


// Write a single line with Population parameters to FileStatus
procedure WriteStatus(
   var   FileStatus  :  Text;
   const Population  :  TPopulation);
   var
         Mean, Sigma :  Real;
         Dist,
         DistToBest  :  Real;
   begin
   ScoreStats(Mean, Sigma, Population.SolList);
   with Population, SolList do
      begin
      Dist       := AverageDistance(SolList, {ToBest:} False);
      DistToBest := AverageDistance(SolList, {ToBest:} True);
      WriteLn(fileStatus,
         Generation,                                  Tab,
         NFE,                                         Tab,
         Improvements,                                Tab,
         Best.Score,                                  Tab,
         Mean,                                        Tab,
         SolList._[ RankIndex[N div 2 - 1] ].Score,   Tab,
         SolList._[ RankIndex[N - 1] ].Score,         Tab,
         Sigma,                                       Tab,
         Dist,                                        Tab,
         DistToBest                                   );
      end;
   Flush(fileStatus);
   end;


// Prepare for the next iteration: update NFE, generation, solution ages,
// write the status and save the population if necessary
procedure PrepareNextIter(
   var   Population  :  TPopulation;
   var   FileStatus  :  Text;
   const Status      :  TGAStatus);
   begin
   with Population do
      begin
      Inc(NFE);
      if (NFE - NFEPrevGen) >= SolList.N then with Status do
         begin
         NFEPrevGen := NFE;
         Inc(Generation);
         IncreaseAge(Population);
         if Divisible(Generation, GenStatus) then
            WriteStatus(FileStatus, Population);
         if Divisible(Generation, GenSave) then
            SavePopulation(DirPopulation, Population, Status.ShowMessage);
         end;
      end;
   end;
   
   
// Return whether the stopping criterion specified in Params has been met
function StoppingCriterion(
   const Population  :  TPopulation;
   const Params      :  TGAParameters
         )           :  Boolean;
   begin
   with Population, Params do
      case Params.Stopping of
         scGenerations:
            Result := Generation >= MaxGens;
         scNFE:
            Result := NFE >= MaxNFE;
         scScore:
            Result := CompareScores(
               SolList.Best.Score, ScoreToReach) <> scoreWorse;
         else
            Assert(False, 'Unknown stopping criterion');
            Result := False;
         end;
   end;


// Run the genetic algorithm with specified Params, return the Best solution found 
// and the search Stats, report and save the progress according to Status
procedure GeneticAlgorithm(
   var   Best     :  TSolution;
   var   Stats    :  TRunStats;
   const Params   :  TGAParameters;
   const Status   :  TGAStatus);
   var
         Population  :  TPopulation;
         Child       :  TSolution;
         ReprodInfo  :  TReprodInfo;
         FileStatus  :  Text;
         StatusOK    :  Boolean;
   begin
   // Initialization
   InitPopulation(Population, Params);
   AssignSolution(Best, Population.SolList.Best);

   // Try opening the status file if necessary
   StatusOK := True;
   if Status.GenStatus > 0 then
      StatusOK := TryOpenWrite(FileStatus, PathStatus, Status.ShowMessage);
      
   if StatusOK then
      begin
      // Write the status header and initial statistics
      if Status.GenStatus > 0 then
         begin
         WriteHeader(FileStatus);
         WriteStatus(FileStatus, Population);
         end;                 

      // Steady state GA loop
      repeat
         Reproduce(Child, ReprodInfo, Population, Params);
         Replacement(
            Population, Best, Child, ReprodInfo, Params, Status.ShowMessage);
         PrepareNextIter(Population, FileStatus, Status);
         //TestSolList(Population.SolList); // #DEBUG
      until StoppingCriterion(Population, Params);

      // Run complete
      if Status.GenStatus > 0 then
         Close(FileStatus);
      Stats.NFEFull := Population.NFE;
      Stats.Iters := Population.Generation;
      SavePopulation(DirPopulation, Population, Status.ShowMessage);
      end;
   end;

end.
