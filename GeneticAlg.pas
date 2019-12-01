{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit GeneticAlg; ////////////////////////////////////////////////////////////////////
{
>> Version: 3.0

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
    - More stopping criteria:
       - Automatic convergence detection
       - No improvement of best or average score for some N of generations
       - Score standard deviation < threshold
       - Average population distance < threshold
       - Small rate of improvements
    - Correlation measures for mutation and crossover operators - corelation between
      the parents' scores and the child's score

>> Changelog
   3.0 : 2019.12.01  + Huge amount of new selection, replacement and acceptance
                       options
                     + Multirun statistics collection
   2.1 : 2018.09.27  * NFE initialization
   2.0 : 2018.09.16  - Many experimental and untested features
                     ~ Cleaned up
                     ~ Freepascal compatibility
   1.5 : 2018.08.24  + Mixed, mixed compound and time dependent inverse rank
                       replacement options
                     + Thermal child acceptance option
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
      TSelection = (
         // Score-based
         selRankProp = 0, selFitUniform, selNearRank, 
         // Distance-based
         selDist, selDistToBest,      
         // Topology-based
         selRing, selLeakyRing, selLeakyRingTD, selTorus, selSegment, 
         selSegmentTD, selCrescent, selVarDegreeTD, selHalo, selDirHalo, 
         selThreadedRings, selInterconRings, selThreadedIsles, selBridgedIsles,
         selChainedIsles, selInterconIsles, selRandNearConRings, selRandFarConRings,
         selRandNearConIsles, selRandFarConIsles, selDisconIslesTD);
         
      TReplacement = (
         // Score-based
         repWorst = 0, repWorstParent, repRandParent, repInvRank, repInvRankLn, repInvRankTD,
         // Distance-based
         repSimParent,      repCenParent, 
         repSimWorseParent, repCenWorseParent, 
         repNoveltySim,     repNoveltyCen, 
         repCompoundSimSD,  repCompoundCenSD,
         repCompoundSimRD,  repCompoundCenRD,
         repCompoundSimTD,  repCompoundCenTD,
         repCompSoftSimSD,  repCompSoftCenSD,
         repCompSoftSimRD,  repCompSoftCenRD,
         repCompSoftSimTD,  repCompSoftCenTD,
         // New solution injection
         repInfluxRare, repInfluxRD, repInfluxSD, repInfluxTD,
         // Topology-based
         repDigraph
         );

      TGAAcceptance = (
         gaaElitist = 0, gaaUnconditional, 
         gaaThreshold, gaaThresholdTD, gaaThresholdSD, 
         gaaDistToBestTD,   gaaDistToBestSD,   gaaDistToBestRD,
         gaaDistToParentTD, gaaDistToParentSD, gaaDistToParentRD);

      TStoppingCriterion = (scGenerations = 0, scNFE, scScore);
         
      TGAParameters =
         record
         PopSize           :  Integer;
         ReplaceP,
          SelectP          :  Real;
         Selection         :  TSelection;
         MutationRate      :  Real;
         Replacement       :  TReplacement;
         Acceptance        :  TGAAcceptance;
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
// and the search Stats, update MultirunStats
procedure GeneticAlgorithm(
   var   Best           :  TSolution;
   var   Stats          :  TRunStats;
   var   MultirunStats  :  TMultirunStats;
   const Params         :  TGAParameters;
   const Status         :  TGAStatus);

implementation //////////////////////////////////////////////////////////////////////
uses
      ExtraMath,
      RandVars,
      IniConfigs,
      StringLists;

type
      TSolutionInfo =
         record
         BirthNFE,
         NOps,
         Dominated   :  Integer;
         BirthGen,
         DistToBest  :  Real;
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
         IsNew    :  Boolean;
         end;

      TPopParam = (
         ppN, ppIteration, ppGeneration, ppLastUpdate, ppImprovements);

const
      PopParamName : array [TPopParam] of AnsiString =
         ('Size', 'NFE', 'Generation', 'LastUpdate', 'Accepted');

      PathPopParam     = 'Parameters.txt';
      PathInfo         = 'Info.txt';
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
         with SolutionsInfo[i] do
            begin
            BirthNFE := 0;
            BirthGen := 0;
            NOps     := 0;
            end;

      // Initialize parameters
      NFE           := PopSize;
      NFEPrevGen    := PopSize;
      Generation    := 0;
      GenLastUpdate := 0;
      Improvements  := 0;
      end;
   end;

   
// Return the normalized time or 0 if stopping criterion is scScore
function NormalizedTime(
   const Population  :  TPopulation;
   const Params      :  TGAParameters
         )           :  Real;
   begin
   case Params.Stopping of
      scGenerations: Result := Population.Generation / Params.MaxGens;
      scNFE:         Result := Population.NFE        / Params.MaxNFE;
      scScore:       Result := 0;
      else
         Assert(False);
         Result := 0;
      end;
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
   {<}   break;

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
   {<}   break;

      // Save Solution info
      if OpenWrite(fileInfo, Dir + PathInfo) = Success then
         begin
         WriteLn(fileInfo, 'Index', Tab, 'Score', Tab, 'Age');
         for i := 0 to N - 1 do
            begin
            j := RankIndex[i];
            WriteLn(fileInfo,
               i + 1,                        Tab,
               SolList._[j].Score,           Tab,
               SolutionsInfo[j].BirthNFE,    Tab,
               SolutionsInfo[j].NOps,        Tab);
            end;
         Close(fileInfo);
         end
      else
   {<}   break;

      // Save all Solutions
      Result := Success;
      for i := 0 to N - 1 do
         if TrySaveSolution(
            SolutionPath(i, Dir), SolList._[ RankIndex[i] ], ShowMessage
            ) = Fail then
            begin
            Result := Fail;
      {<}   break;
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
   {<}   break;

      // Load Solution info
      if OpenRead(fileInfo, Dir + PathInfo) = Success then
         begin
         ReadLn(fileInfo);
         for i := 0 to N - 1 do
            begin
            ReadLn(fileInfo, 
               DummyIndex, 
               DummyScore, 
               SolutionsInfo[i].BirthNFE,
               SolutionsInfo[i].NOps);
            SolutionsInfo[i].BirthGen := SolutionsInfo[i].BirthNFE / N;
            end;
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
      {<}   break;
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

function WorstParent(
   const Population  :  TPopulation;
   const ReprodInfo  :  TReprodInfo
         )           :  TSolutionIndex;
   forward;
   

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
         i, j, k, 
         M, L, H     :  Integer;
         A           :  TIntArrayN;
         D, BestD, t :  Real;
         DistToBest  :  TRealArray;
         Order       :  TIntArray;
         OldScore    :  TScore;
         Undo        :  TSAUndo;
         
   // Return the maximal number of step from vertex i
   // on a crescent graph of size N
   function CrescentRadius(
            i, N  :  Integer
            )     :  Integer;
      begin
      Result := Floor(Power(N / 2, 2 * Min(i, N - i) / N));
      end;
   
   begin
   t := NormalizedTime(Population, Params);
   case Params.Selection of
      selRankProp:
         TwoRandSolIndices(i1, i2, Population.SolList, Params.SelectP);
         
      selFitUniform:
         TwoRandFitUniformIndices(i1, i2, Population.SolList);
         
      selNearRank:
         with Population.SolList do
            begin
            k := Random(N - 1);
            i1 := RankIndex[k];
            i2 := RankIndex[k + 1];
            end;
         
      selDist:
         begin
         InitArrayN(A);
         M := Max(2, Round(Abs(Params.SelectP)));
         repeat
            k := Random(Population.SolList.N);
            AppendUnique(A, k);
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
            AppendUnique(A, k);
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
         
      selRing:
         with Population.SolList do
            begin
            i1 := Random(N);
            i2 := (i1 + 1) mod N;
            end;
            
      selLeakyRing:
         with Population.SolList do
            begin
            i1 := Random(N);
            if Random(N) < 1 then
               i2 := (i1 + 1 + Random(N - 1)) mod N else
               i2 := (i1 + 1) mod N;
            end;
            
      selLeakyRingTD:
         with Population.SolList do
            begin
            i1 := Random(N);
            if Random < Power(t, Ln(N) + 1) then
               i2 := (i1 + 1 + Random(N - 1)) mod N else
               i2 := (i1 + 1) mod N;
            end;
            
      selTorus:
         with Population.SolList do
            begin
            M := Round(Sqrt(N));
            Assert(Sqr(M) = N);
            i1 := Random(N);
            if RandBool then
               i2 := M * (i1 div M) + (i1 + 1) mod M else
               i2 := (i1 + M) mod N;
            end;
            
      selSegment, selSegmentTD:
         with Population.SolList do
            begin
            i1 := Random(N);
            if Params.Selection = selSegment then
               M := N div 2 else
               M := Floor(N * (1 - t));
            if i1 < M then
               i2 := Modulo(i1 + RandSign, N) 
            else if (i1 = M    ) and (Random(N - M - 1) < 1) then
               i2 := Modulo(M - 1, N)
            else if (i1 = N - 1) and (Random(N - M - 1) < 1) then
               i2 := 0
            else
               i2 := M + (i1 - M + 1 + Random(N - M - 1)) mod (N - M);
            end;
            
      selCrescent:
         with Population.SolList do
            begin
            i1 := Random(N);
            repeat
               i2 := Modulo(i1 + RandSign * RandRange(1, CrescentRadius(i1, N)), N);
            until ModuloDistance(i1, i2, N) <= CrescentRadius(i2, N);
            end;
            
      selVarDegreeTD:
         with Population.SolList do
            begin
            i1 := Random(N);
            i2 := (i1 + 1 + Random(Floor(Power(N - 1, t)))) mod N;
            end;
            
      selHalo:
         with Population.SolList do
            begin
            M := N div 2;
            i1 := Random(N);
            if i1 < M then
               if Random(M) < 1 then
                  i2 := RandRange(M, N - 1) else
                  i2 := Modulo(i1 + RandSign, M)
            else
               i2 := M + (i1 - M + 1 + Random(N - M - 1)) mod (N - M);
            end;
            
      selDirHalo:
         with Population.SolList do
            begin
            M := N div 2;
            case Random(3) of
               0: begin
                  i1 := Random(M);
                  i2 := Modulo(i1 + 1, M)
                  end;
               1: begin
                  i1 := Random(M);
                  i2 := RandRange(M, N - 1);
                  end;
               2: repeat
                     i1 := RandRange(M, N - 1);
                     i2 := RandRange(M, N - 1);
                  until i1 <> i2;
               end;
            if ((i1 >= M) or (i2 < M)) and 
               (CompareScores(
                  Population.SolList._[i2], 
                  Population.SolList._[i1]) = scoreBetter) then
               Swap(i1, i2);
            end;
            
      selThreadedRings:
         with Population.SolList do
            begin
            M := Round(Sqrt(N));
            Assert(Sqr(M) = N);
            i1 := Random(N);
            if (i1 mod M = 0) and RandBool then
               i2 := Modulo(i1 + RandSign * M, N) else
               i2 := M * (i1 div M) + Modulo(i1 + RandSign, M);
            end;
            
      selInterconRings:
         with Population.SolList do
            begin
            M := Round(Sqrt(N));
            Assert(Sqr(M) = N);
            i1 := Random(N);
            if (i1 mod M = 0) and (Random(1 + M) < M - 1) then
               i2 := ( i1 + M * ( 1 + Random(M - 1) ) ) mod N else
               i2 := M * (i1 div M) + Modulo(i1 + RandSign, M);
            end;
            
      selThreadedIsles:
         with Population.SolList do
            begin
            M := Round(Sqrt(N));
            Assert(Sqr(M) = N);
            i1 := Random(N);
            if (i1 mod M = 0) and (Random(1 + M) < 2) then
               i2 := Modulo(i1 + RandSign * M, N) else
               i2 := M * (i1 div M) + (i1 + 1 + Random(M - 1)) mod M;
            end;
            
      selBridgedIsles:
         with Population.SolList do
            begin
            M := Round(Sqrt(N));
            Assert(Sqr(M) = N);
            i1 := Random(N);
            if      (i1 mod M =      0 ) and (Random(M) < 1) then
               i2 := Modulo(i1 - 1, N) 
            else if (i1 mod M = (M - 1)) and (Random(M) < 1) then
               i2 := Modulo(i1 + 1, N) 
            else
               i2 := M * (i1 div M) + (i1 + 1 + Random(M - 1)) mod M;
            end;
            
      selChainedIsles:
         with Population.SolList do
            begin
            M := Round(Sqrt(N));
            Assert(Sqr(M) = N);
            i1 := Random(N);
            if i1 mod M = 0 then
               i2 := Modulo(i1 + RandSign * (1 + Random(M)), N) 
            else
               repeat
                  i2 := RandRange(M * (i1 div M), M * (1 + i1 div M)) mod N;
               until i2 <> i1;
            end;
            
      selInterconIsles:
         with Population.SolList do
            begin
            M := Round(Sqrt(N));
            Assert(Sqr(M) = N);
            i1 := Random(N);
            if (i1 mod M = 0) and (Random(2) < 1) then
               i2 := Modulo(i1 + (1 + Random(M - 1)) * M, N) else
               i2 := M * (i1 div M) + (i1 + 1 + Random(M - 1)) mod M;
            end;
            
      selRandNearConRings, selRandFarConRings,
      selRandNearConIsles, selRandFarConIsles:
         with Population.SolList do
            begin
            M := Round(Sqrt(N));
            Assert(Sqr(M) = N);
            i1 := Random(N);
            k := M * (i1 div M);
            if Random(M) < 1 then
               if Params.Selection in [selRandNearConRings, selRandNearConIsles] then
                  i2 := Modulo(k + RandSign * M + Random(M), N) else
                  i2 := Modulo(k + RandRange(M, N - 1),      N)
            else 
               if Params.Selection in [selRandNearConRings, selRandFarConRings] then
                  i2 := k + Modulo(i1 + RandSign,            M) else
                  i2 := k + Modulo(i1 + RandRange(1, M - 1), M);
            end;
            
      selDisconIslesTD:
         begin
         M := Floor(Blend(Sqrt(Params.Popsize) + Ord(t > 0), 1, t));
         k := Random(M);
         L := ( k      * Params.Popsize) div M;
         H := ((k + 1) * Params.Popsize) div M - 1;
         repeat
            i1 := RandRange(L, H);
            i2 := RandRange(L, H);
         until i1 <> i2;
         end;

      else
         Assert(False);
      end;
   end;

   
// Create the Child via crossover of two distinct random solutions 
// selected from Population according to the selection setting in Params
// or anew in case of influx replacement, update ReprodInfo. 
procedure Reproduce(
   var   Child       :  TSolution;
   var   ReprodInfo  :  TReprodInfo;
   const Population  :  TPopulation;
   const Params      :  TGAParameters);
   var
         i, j, k, dp :  Integer;
         x           :  Real;
   begin
   with ReprodInfo, Population do
      begin
      // Decide whether the child should be created from scratch
      TwoRandPopIndices(Parent1, Parent2, Population, Params);
      case Params.Replacement of
         repInfluxRD:
            begin
            i := WorstParent(Population, ReprodInfo);
            x := SolList.IndexRank[i] / (Params.PopSize - 1);
            dp := 0;
            end;
         repInfluxSD:
            begin
            i := WorstParent(Population, ReprodInfo);
            if Params.Selection = selTorus then
               begin
               k := Round(Sqrt(Params.PopSize));
               j := i div k;
               x := ModuloDistance(j, 0, k) / (k div 2);
               end
            else
               x := i / (Params.PopSize - 1);
            dp := 1;
            end;
         repInfluxTD:
            begin
            x := 1 - NormalizedTime(Population, Params);
            dp := 1;
            end;
         end;
      if Params.Replacement in [repInfluxRD, repInfluxSD, repInfluxTD] then
         IsNew := Random < Power(x, Ln(Params.PopSize) + dp) else
         IsNew := False;
      
      // Create the child
      if IsNew then
         NewSolution(Child)
      else
         begin
         Crossover(
            Child, SolList._[Parent1], SolList._[Parent2], {RecalcScore:} False);
         Mutate(Child);
         Inc(SolutionsInfo[Parent1].NOps);
         Inc(SolutionsInfo[Parent2].NOps);
         end;
      end;
   end;

{-----------------------<< Replacement >>-------------------------------------------}

// Replace a solution with a given Index in the Population by the Child,
// update BestSoFar, display a message via ShowMessage in case of a new best score.
procedure ReplaceInPopulation(
   var   Population  :  TPopulation;
   var   BestSoFar   :  TSolution;
         Index       :  TSolutionIndex;
   const Child       :  TSolution;
         ShowMessage :  ProcMessage);
   begin
   with Population do
      begin
      with SolutionsInfo[Index] do
         begin
         NOps := 0;
         BirthNFE := NFE;
         BirthGen := BirthNFE / SolList.N;
         end;
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
   
   
// Return the index of the parent listed in ReprodInfo most similar to Child
function SimilarParent(
   const Population  :  TPopulation;
   const ReprodInfo  :  TReprodInfo;
   const Child       :  TSolution
         )           :  TSolutionIndex;
   begin
   with Population, ReprodInfo do
      case SimilarSolution(Child, SolList._[Parent1], SolList._[Parent2]) of
         0:    Result := Parent1;
         1:    Result := Parent2;
         else  Result := -1;
               Assert(False);
         end;
   end;
   
   
// Return the index of the parent listed in ReprodInfo 
// closest to the best in population
function CentralParent(
   const Population  :  TPopulation;
   const ReprodInfo  :  TReprodInfo
         )           :  TSolutionIndex;
   var
         D1, D2      :  TSolutionDistance;
   begin
   with Population, ReprodInfo do
      begin
      D1 := Distance(SolList.Best, SolList._[Parent1]);
      D2 := Distance(SolList.Best, SolList._[Parent2]);
      if D1 < D2 then
         Result := Parent1
      else if D1 > D2 then
         Result := Parent2
      else
         Result := WorstParent(Population, ReprodInfo);
      end;
   end;
   
   
// Return the index of IdParent's partner from ReprodInfo
function OtherParent(
   const ReprodInfo  :  TReprodInfo;
         IdParent    :  TSolutionIndex
         )           :  TSolutionIndex;
   begin
   with ReprodInfo do
      if IdParent = Parent1 then
         Result := Parent2
      else if IdParent = Parent2 then
         Result := Parent1
      else
         begin
         Result := -1;
         Assert(False);
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
         D1, D2, 
         OldD, NewD        :  TSolutionDistance;
         t, x, dp          :  Real;
         k, m, Tier,
         IdMedian, 
         IdWorstParent,
         IdBestParent,
         IdSimParent       :  Integer;
         Cmp1, Cmp2        :  TScoreComparison;
   begin
   with Population, SolList, ReprodInfo do
      begin
      // Determine whom to replace
      t := NormalizedTime(Population, Params);
      IdMedian := RankIndex[N div 2];
      case Params.Replacement of
         // Worst in population
         repWorst:
            ReplaceIndex := RankIndex[N - 1];
            
         // Inverse rank-proportional
         repInvRank:
            ReplaceIndex := RandPopIndex(Population, -Params.ReplaceP);
            
         repInvRankLn:
            ReplaceIndex := RandPopIndex(Population, -(Ln(Params.PopSize) + 1));
            
         // Time-dependent inverse rank-proportional
         repInvRankTD:
            // P(0) = 1, P(1 / 2) = Ln(PopSize) + 1, P(1) = PopSize
            ReplaceIndex := RandPopIndex(
               Population,
               - Exp(
                  Ln(Params.PopSize) * 
                  Power(
                     t,
                     -Log2(
                        Ln(Ln(Params.PopSize) + 1) /
                        Ln(Params.PopSize)
                     )
                  )
               )
            );
            
         // Random parent
         repRandParent:
            ReplaceIndex := RandomParent(ReprodInfo);
            
         // Worst parent
         repWorstParent, repInfluxRD, repInfluxSD, repInfluxTD:
            ReplaceIndex := WorstParent(Population, ReprodInfo);
            
         // Similar parent
         repSimParent:
            ReplaceIndex := SimilarParent(Population, ReprodInfo, Child);
               
         // Central parent
         repCenParent:
            ReplaceIndex := CentralParent(Population, ReprodInfo);
            
         // Compound time-dependent: replace similar / central paprent, 
         // worst paprent, worst in population depending on normalized time
         repCompoundSimTD, repCompoundCenTD:
            if t < (1 - 1 / Params.ReplaceP) then
               if Params.Replacement = repCompoundSimTD then
                  ReplaceIndex := SimilarParent(Population, ReprodInfo, Child) else
                  ReplaceIndex := CentralParent(Population, ReprodInfo)
            else if t < (1 - 1 / Sqr(Params.ReplaceP)) then
               ReplaceIndex := WorstParent(Population, ReprodInfo)
            else
               ReplaceIndex := RankIndex[N - 1];
               
         // Compound rank-dependent: Replace similar / central parent if the 
         // child is not worse than the median, otherwise replace worst parent
         repCompoundSimRD, repCompoundCenRD:
            if CompareScores(Child, SolList._[IdMedian]) <> scoreWorse then
               if Params.Replacement = repCompoundSimRD then
                  ReplaceIndex := SimilarParent(Population, ReprodInfo, Child) else
                  ReplaceIndex := CentralParent(Population, ReprodInfo)
            else
               ReplaceIndex := WorstParent(Population, ReprodInfo);
               
         // Compound position-dependent: Replace similar / central or 
         // worst parent depending on parent positions
         repCompoundSimSD, repCompoundCenSD:
            if (Parent1 < (N div 2)) or (Parent2 < (N div 2)) then
               if Params.Replacement = repCompoundSimSD then
                  ReplaceIndex := SimilarParent(Population, ReprodInfo, Child) else
                  ReplaceIndex := CentralParent(Population, ReprodInfo)
            else
               ReplaceIndex := WorstParent(Population, ReprodInfo);
               
         // Soft compound
         repCompSoftSimSD, repCompSoftCenSD,
         repCompSoftSimRD, repCompSoftCenRD,
         repCompSoftSimTD, repCompSoftCenTD:
            begin
            case Params.Replacement of
               repCompSoftSimTD, repCompSoftCenTD: 
                  x := t;
               repCompSoftSimRD, repCompSoftCenRD:
                  x := IndexRank[ClosestScoreIndex(SolList, Child.Score)] / Params.PopSize;
               repCompSoftSimSD, repCompSoftCenSD:
                  x := Sqrt(IndexRank[Parent1] * IndexRank[Parent2]) / Params.PopSize;
               else
                  x := 0;
                  Assert(False);
               end;
            if Random > Power(x, Ln(Params.PopSize) + 1) then
               if Params.Replacement in [repCompSoftSimTD, repCompSoftSimRD, repCompSoftSimSD] then
                  ReplaceIndex := SimilarParent(Population, ReprodInfo, Child) else
                  ReplaceIndex := CentralParent(Population, ReprodInfo)
            else
               ReplaceIndex := WorstParent(Population, ReprodInfo);
            end;
               
         // Replace worst parent if doing so increases intersolution distance,
         // otherwise replace similar parent
         repNoveltySim:
            begin
            IdWorstParent := WorstParent(Population, ReprodInfo);
            IdBestParent := OtherParent(ReprodInfo, IdWorstParent);
            OldD := Distance(SolList._[IdBestParent], SolList._[IdWorstParent]);
            NewD := Distance(SolList._[IdBestParent], Child);
            if (NewD > OldD) or (Distance(Child, SolList._[IdWorstParent]) <= NewD) then
               ReplaceIndex := IdWorstParent else
               ReplaceIndex := IdBestParent;
            end;
            
         // Replace worst parent if doing so increases the distance to best in
         // population, otherwise replace central parent
         repNoveltyCen:
            begin
            IdWorstParent := WorstParent(Population, ReprodInfo);
            IdBestParent := OtherParent(ReprodInfo, IdWorstParent);
            OldD := Distance(SolList.Best, SolList._[IdWorstParent]);
            NewD := Distance(SolList.Best, Child);
            if (NewD > OldD) or (OldD <= Distance(SolList.Best, SolList._[IdBestParent])) then
               ReplaceIndex := IdWorstParent else
               ReplaceIndex := IdBestParent;
            end;
            
         // Replace similar / central parent if the child is at least as good as the
         // best parent, otherwise replace the worst parent
         repSimWorseParent, repCenWorseParent:
            begin
            Cmp1 := CompareScores(Child, SolList._[Parent1]);
            Cmp2 := CompareScores(Child, SolList._[Parent2]);
            if (Cmp1 <> scoreWorse) and (Cmp2 <> scoreWorse) then
               if Params.Replacement = repSimWorseParent then
                  ReplaceIndex := SimilarParent(Population, ReprodInfo, Child) else
                  ReplaceIndex := CentralParent(Population, ReprodInfo)
            else
               ReplaceIndex := WorstParent(Population, ReprodInfo);
            end;

         // Replace worst parent. When crossover improves over both parents,
         // also replace the other parent with a new solution.
         repInfluxRare:
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
            
         // Replace the parent the other parent points to in a directed graph
         repDigraph:
            begin
            Assert(Params.Selection = selDirHalo);
            ReplaceIndex := Parent2;
            end;
            
         else
            Assert(False);
            ReplaceIndex := -1;
         end;

      // Decide whether to accept the child
      case Params.Acceptance of
         gaaElitist:
            Accept := CompareScores(Child, SolList._[ReplaceIndex]) <> scoreWorse;
            
         gaaUnconditional:
            Accept := True;
            
         gaaThreshold:
            Accept := (CompareScores(Child, SolList._[ReplaceIndex]) <> scoreWorse) or
                      (CompareScores(Child, SolList._[IdMedian    ]) <> scoreWorse);
                      
         gaaThresholdTD:
            begin
            k := RankIndex[ Min(Floor( N * (1 - t) ), N - 1) ];
            Accept := (CompareScores(Child, SolList._[ReplaceIndex]) <> scoreWorse) or
                      (CompareScores(Child, SolList._[k           ]) <> scoreWorse);
            end;
            
         gaaThresholdSD:
            begin
            if Params.Selection = selTorus then
               begin
               m := Round(Sqrt(Params.PopSize));
               x := ModuloDistance(ReplaceIndex div m, 0, m) / (m div 2);
               k := RankIndex[ Round( (N - 1) * x ) ];
               end
            else
               k := RankIndex[ReplaceIndex];
            Accept := (CompareScores(Child, SolList._[ReplaceIndex]) <> scoreWorse) or
                      (CompareScores(Child, SolList._[k           ]) <> scoreWorse);
            end;
            
         gaaDistToParentTD, gaaDistToParentSD, gaaDistToParentRD,
         gaaDistToBestTD,   gaaDistToBestSD,   gaaDistToBestRD:
            if CompareScores(Child, SolList._[ReplaceIndex]) <> scoreWorse then
               Accept := True 
            else
               begin
               case Params.Acceptance of
                  gaaDistToParentTD, gaaDistToBestTD:
                     x := 1 - t;
                  gaaDistToParentSD, gaaDistToBestSD:
                     if Params.Selection = selTorus then
                        begin
                        m := Round(Sqrt(Params.PopSize));
                        x := ModuloDistance(ReplaceIndex div m, 0, m) / (m div 2);
                        end
                     else
                        x := ReplaceIndex / (Params.PopSize - 1);
                  gaaDistToParentRD, gaaDistToBestRD:
                     x := IndexRank[ReplaceIndex] / (Params.PopSize - 1); 
                  else
                     Assert(False);
                     x := 0;
                  end;
               k := OtherParent(ReprodInfo, ReplaceIndex);
               dp := Ord( not (Params.Acceptance in 
                  [gaaDistToParentRD, gaaDistToBestRD]) );
               if Random < Power(x, Ln(Params.PopSize) + dp) then
                  if Params.Acceptance in 
                     [gaaDistToParentTD, gaaDistToParentSD, gaaDistToParentRD] then
                     Accept := 
                        Distance(SolList._[k], Child) > 
                        Distance(SolList._[k], SolList._[ReplaceIndex])
                  else
                     Accept := 
                        Distance(SolList.Best, Child) > 
                        Distance(SolList.Best, SolList._[ReplaceIndex])
               else
                  Accept := False;
               end;
         else
            Assert(False);
            Accept := False;
         end;
      Accept := Accept or IsNew;
      if Accept then
         ReplaceInPopulation(
            Population, BestSoFar, ReplaceIndex, Child, ShowMessage);
      end;
   end;

{-----------------------<< GA itself >>---------------------------------------------}
const
      NStatsFields   =  10;

// Create the header of MultirunStats
procedure WriteHeader(
   var   MultirunStats  :  TMultirunStats);
   begin
   with MultirunStats do
      if NVars = 0 then
         begin
         InitMultirunStats(MultirunStats, NStatsFields);
         SetLength(Header, NStatsFields);
         Header[0] := 'Gen';
         Header[1] := 'NFE';
         Header[2] := 'Improvements';
         Header[3] := 'BestScore';
         Header[4] := 'MeanScore';
         Header[5] := 'MedianScore';
         Header[6] := 'WorstScore';
         Header[7] := 'StdDevScore';
         Header[8] := 'MeanDist';
         Header[9] := 'MeanDistToBest';
         end;
   end;


// Add the current population statistics to MultirunStats
procedure WriteStatus(
   var   MultirunStats  :  TMultirunStats;
   const Population     :  TPopulation);
   var
         Mean, Sigma    :  Real;
         Dist,
         DistToBest     :  Real;
         Data           :  TRealArray;
   begin
   SetLength(Data, NStatsFields);
   ScoreStats(Mean, Sigma, Population.SolList);
   with Population, SolList do
      begin
      Dist       := AverageDistance(SolList, {ToBest:} False);
      DistToBest := AverageDistance(SolList, {ToBest:} True);
      Data[0] := Generation;
      Data[1] := NFE;
      Data[2] := Improvements;
      Data[3] := Best.Score;
      Data[4] := Mean;
      Data[5] := SolList._[ RankIndex[N div 2 - 1] ].Score;
      Data[6] := SolList._[ RankIndex[N - 1] ].Score;
      Data[7] := Sigma;
      Data[8] := Dist;
      Data[9] := DistToBest;
      AddSample(MultirunStats, Data);
      end;
   end;


// Prepare for the next iteration: update NFE, generation, solution ages,
// write the status and save the population if necessary
procedure PrepareNextIter(
   var   Population     :  TPopulation;
   var   MultirunStats  :  TMultirunStats;
   const Status         :  TGAStatus);
   begin
   with Population do
      begin
      Inc(NFE);
      if (NFE - NFEPrevGen) >= SolList.N then with Status do
         begin
         NFEPrevGen := NFE;
         Inc(Generation);
         if Divisible(Generation, GenStatus) then
            WriteStatus(MultirunStats, Population);
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
// and the search Stats, update MultirunStats
procedure GeneticAlgorithm(
   var   Best           :  TSolution;
   var   Stats          :  TRunStats;
   var   MultirunStats  :  TMultirunStats;
   const Params         :  TGAParameters;
   const Status         :  TGAStatus);
   var
         Population     :  TPopulation;
         Child          :  TSolution;
         ReprodInfo     :  TReprodInfo;
   begin
   // Initialization
   InitPopulation(Population, Params);
   AssignSolution(Best, Population.SolList.Best);
      
   // Write the status header and initial statistics
   if Status.GenStatus > 0 then
      begin
      WriteHeader(MultirunStats);
      PrepareNextRun(MultirunStats);
      WriteStatus(MultirunStats, Population);
      end;                 

   // Steady state GA loop
   repeat
      Reproduce(Child, ReprodInfo, Population, Params);
      Replacement(
         Population, Best, Child, ReprodInfo, Params, Status.ShowMessage);
      PrepareNextIter(Population, MultirunStats, Status);
      //TestSolList(Population.SolList); // #DEBUG
   until StoppingCriterion(Population, Params);

   // Run complete
   Stats.NFEFull := Population.NFE;
   Stats.Iters := Population.Generation;
   SavePopulation(DirPopulation, Population, Status.ShowMessage);
   end;

end.
