{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF} {$APPTYPE CONSOLE} {$MINENUMSIZE 4}
program Ascension; //////////////////////////////////////////////////////////////////
{
>> Version: 2.0

>> Description
   Ascension, a general-purpose metaheuristic optimization framework. 
   
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> ToDo
    ! Make sure that crossover is always called in a symmetric way
    ! Config: there should be an option to specify "Auto" instead of a numeric value
    - Bring back more algorithms from 1.8 after testing
    ? Abstract constructive crossover (see 3D Queens, Heilbronn, No 3 in line),
      GRASP-like?
    - Merge all Run** procedures, pre-initialize all load tables
    - Search space cartography via MDS
    - PDMs:
       - Sudoku
       - S-expressions
       - Corewar program generation and optimization
       - register language
       - stack language
       - Time delay embedding
       - Functional networks
       - Proximity maps
       - Graph layout
       - Image compression with PDE point selection
       - Special function approximation
       - Covering codes
    ? Reorganize the units into folders by type (PDMs, heuristics, core, libs)
    ? Change config naming convention to Ada_Style or shorten property names
    ? Rename Distance to SolDistance to avoid confusion with Vectors unit
    - Something like an iterator for accessing the tabu list (may simplify the code)
    - Useful measures like fitness distance correlation, crossover locality
      (correlation between the parent and the child's scores)
    - User-defined callbacks for visualization

>> Changelog
      2.1 : 2019.05.23  + 5 PDMs:
                          + 3D N queens
                          + Chess coverings
                          + Peacable queens
                          + Maximum density still life
                          + Maximum density squarefree arrangements
      2.0 : 2018.09.19  - Most experimental algorithms and features
                        ~ Config format for enumerated parameters
      1.6 : 2013.01.29  + Too much to document
      1.5 : 2012.02.05  + More metaheuristics
      1.4 : 2011.09.26  ~ Moved PDM interface section into an include file
                        + Lots of new experimental metaheuristics
      1.3 : 2011.06.08  + TSolutionList type as a basis for implementation of
                          population-based algorithms
      1.2 : 2011.05.09  + Local Search
      1.1 : 2011.04.04  ~ Changed to console application, reading all parameters from
                          a config file
                        + 'Common' unit reduces copypasta across problems and
                          metaheuristics
                        + New metaheuristics:
                           + Estimation of Distribution
                           + Stochastic Partitioning
                           +     Nested Partitioning
                        + Benchmarking
      0.0 : 2010.11.14  + Initial version
                        + Metaheuristics: GA, SA, TS, GLS
}
/////////////////////////////////////////////////////////////////////////////////////
{$R 'icon.res'}

uses
      InvSys,
      StringLists,
      StringUtils,
      Formatting,
      IniConfigs,
      Math,

      Common,
      Messages,
      Problem,

      GeneticAlg,
      LocalSearchAlg,
      TabuSearchAlg,
      SimAnnealAlg,
      Acceptance;

type
      ProcRun =   procedure (
                     var   Best        :  TSolution;
                     var   Stats       :  TRunStats;
                     const Config      :  TIniConfig;
                           Algorithm   :  TMetaheuristic);

const
      PressToExit     = 'Press ENTER to exit';
      PressToContinue = 'Press ENTER to continue';
      ErrorInvalidEnumSize = 'invalid enum size';

{-----------------------<< UI >>----------------------------------------------------}

// Function for displaying messages during the optimization
procedure ShowMessage(
   const S  :  AnsiString);
   begin
   WriteLn(S);
   end;

{-----------------------<< Parameter checking functions >>--------------------------}
const
      ErrorRange      = 'valid range: [{}, {}]';
      InfinityInt = High(LongInt);
       PlusInf = '+inf';
      MinusInf = '-inf';

// Verify that X lies in the [MinValue, MaxValue] interval, 
// return an error message if this is not the case
function CheckInt(
         X,
         MinValue :  LongInt;
         MaxValue :  LongInt     = InfinityInt
         )        :  AnsiString;

   function FormatInt(
            X  :  LongInt
            )  :  AnsiString;
      begin
      if      X = -InfinityInt then
         Result := MinusInf
      else if X =  InfinityInt then
         Result := PlusInf
      else
         Result := Format('{}', [X]);
      end;

   begin
   if (X >= MinValue) and (X <= MaxValue) then
      Result := ''
   else
      Result := Format(ErrorRange, [FormatInt(MinValue), FormatInt(MaxValue)]);
   end;


// Verify that X lies in the [MinValue, MaxValue] interval, 
// return an error message if this is not the case
function CheckReal(
         X,
         MinValue :  Real;
         MaxValue :  Real      = Infinity
         )        :  AnsiString;

   function FormatReal(
            X  :  Real
            )  :  AnsiString;
      begin
      if      X = NegInfinity then
         Result := MinusInf
      else if X =    Infinity then
         Result := PlusInf
      else
         Result := Format('{:2}', [X]);
      end;

   begin
   if (X >= MinValue) and (X <= MaxValue) then
      Result := ''
   else
      Result := Format(ErrorRange, [FormatReal(MinValue), FormatReal(MaxValue)]);
   end;


// Verify that a number is greater than one
function IntMany(
         VarInt   :  LongInt
         )        :  AnsiString;
   begin
   Result := CheckInt(VarInt, {MinValue:} 2);
   end;


// Verify that a number is positive
function IntPos(
         VarInt   :  LongInt
         )        :  AnsiString;
   begin
   Result := CheckInt(VarInt, {MinValue:} 1);
   end;


// Verify that a number is nonnegative
function IntNonNeg(
         VarInt   :  LongInt
         )        :  AnsiString;
   begin
   Result := CheckInt(VarInt, {MinValue:} 0);
   end;


// Verify that a number is a power of 2
function Int2P(
         VarInt   :  LongInt
         )        :  AnsiString;
   begin
   if VarInt = IntPower(2, Round(Log2(VarInt))) then
      Result := ''
   else
      Result := 'Power of 2 required';
   end;


// Verify that a number lies in the [0 .. 1] interval
function RealUnit(
         VarReal  :  Real
         )        :  AnsiString;
   begin
   Result := CheckReal(VarReal, {MinValue:} 0, {MaxValue:} 1);
   end;


// Verify that a number is nonnegative
function RealNonNeg(
         VarReal  :  Real
         )        :  AnsiString;
   begin
   Result := CheckReal(VarReal, {MinValue:} 0);
   end;


// Get the Index of S in the list of space separated EnumNames
// or return Fail if S is not found
function GetEnumIndex(
   var   Index       :  LongWord;
   const S, 
         EnumNames   :  AnsiString
         )           :  Boolean;
   var
         Work        :  AnsiString;
         Found       :  Integer;
   begin
   if GetPos(Found, EnumNames, S) = Success then
      begin
      Work := CopyRange(EnumNames, {From:} 1, {To:} Found - 1);
      Index := SubstrCount(Work, ' ');
      Result := Success;
      end
   else
      Result := Fail;
   end;


// Get the enumerated type parameter at ptrVar from its Name and the list of
// possible EnumNames, add a message to Errors if it is not found
procedure GetEnum(
         ptrVar         :  Pointer;
   var   Errors         :  TStringList;
   const Name, 
         EnumNames      :  AnsiString);
   var
         Index          :  LongWord;
   const
         ErrorBadEnum   :  AnsiString
                        = 'Invalid enumerated parameter: {}, valid values are: {}';
   begin
   if GetEnumIndex(Index, Name, EnumNames) = Success then
      PLongWord(ptrVar)^ := Index else
      AddToStringList(Errors, Format(ErrorBadEnum, [Name, EnumNames]));
   end;

{-----------------------<< Runing metaheuristics >>---------------------------------}

// Display the Errors and await user response
procedure DisplayErrors(
   const Errors   :  TStringList);
   begin
   DisplayStringList(Errors);
   WriteLn(PressToExit);
   ReadLn;
   end;


// Run the genetic algorithm with parameters specified in Config, return the Best
// solution and run statistics Stats
procedure RunGA(
   var   Best        :  TSolution;
   var   Stats       :  TRunStats;
   const Config      :  TIniConfig;
         Algorithm   :  TMetaheuristic);
   var
         Params      :  TGAParameters;
         Status      :  TGAStatus;
         LT          :  TLoadTable;
         Errors      :  TStringList;
         Sec,
         SSelection,
         SReplacement,
         SAcceptance,
         SStopping   :  AnsiString;
   begin
   Sec := ShortNames[mhGA] + '.';
   Status.ShowMessage := ShowMessage;
   with Params, Status do
      begin
      Assert(SizeOf(Replacement)  = 4, ErrorInvalidEnumSize);
      Assert(SizeOf(Stopping)     = 4, ErrorInvalidEnumSize);
      InitLoadTable(LT);
      AddLoadParam(LT, PopSize,          Sec + 'PopulationSize', IntMany      );
      AddLoadParam(LT, SSelection,       Sec + 'Selection',      nil          );
      AddLoadParam(LT, SelectP,          Sec + 'SelectionP',     nil          );
      AddLoadParam(LT, SReplacement,     Sec + 'Replacement',    nil          );
      AddLoadParam(LT, ReplaceP,         Sec + 'ReplacementP',   RealNonNeg   );
      AddLoadParam(LT, SAcceptance,      Sec + 'Acceptance',     nil          );
      AddLoadParam(LT, SStopping,        Sec + 'StopCriterion',  nil          );
      AddLoadParam(LT, MaxGens,          Sec + 'MaxGens',        IntNonNeg    );
      AddLoadParam(LT, MaxNFE,           Sec + 'MaxNFE',         IntNonNeg    );
      AddLoadParam(LT, GenStatus,        Sec + 'StatusGens',     IntNonNeg    );
      AddLoadParam(LT, GenSave,          Sec + 'SaveGens',       IntNonNeg    );
      AddLoadParam(LT, ScoreToReach,           'ScoreToReach',   nil          );
      LoadFromConfig(LT, Config, Errors);
      GetEnum(@Selection, Errors, SSelection, 
         'RankProp Dist DistToBest');
      GetEnum(@Replacement, Errors, SReplacement, 
         'Worst InvRank WorstParent RandParent SimilarParent Influx');
      GetEnum(@ChildAcceptance, Errors, SAcceptance, 
         'Elitist Unconditional');
      GetEnum(@Stopping, Errors, SStopping, 
         'MaxGens MaxNFE Score');
      if Errors.N = 0 then
         GeneticAlgorithm(Best, Stats, Params, Status)
      else
         DisplayErrors(Errors);
      end;
   end;


// Run the local search with parameters specified in Config, return the Best
// solution and run statistics Stats
procedure RunLS(
   var   Best        :  TSolution;
   var   Stats       :  TRunStats;
   const Config      :  TIniConfig;
         Algorithm   :  TMetaheuristic);
   var
         Params      :  TLSParameters;
         Status      :  TLSStatus;
         LT          :  TLoadTable;
         Errors      :  TStringList;
         Sec, SMode  :  AnsiString;
   begin
   Sec := ShortNames[mhLS] + '.';
   Status.ShowMessage := ShowMessage;
   Status.SaveBest := True;
   with Status do
      begin
      Assert(SizeOf(Params) = 4, ErrorInvalidEnumSize);
      InitLoadTable(LT);
      AddLoadParam(LT, SMode,       Sec + 'Mode'       , nil);
      AddLoadParam(LT, IterStatus,  Sec + 'StatusIters', IntNonNeg);
      LoadFromConfig(LT, Config, Errors);
      GetEnum(@Params, Errors, SMode, 'First Best Chain');
      if Errors.N = 0 then
         LocalSearch(Best, Stats, Params, Status, {RandomInit:} True)
      else
         DisplayErrors(Errors);
      end;
   end;


// Run the simulated annealing with parameters specified in Config, return the Best
// solution and run statistics Stats
procedure RunSA(
   var   Best        :  TSolution;
   var   Stats       :  TRunStats;
   const Config      :  TIniConfig;
         Algorithm   :  TMetaheuristic);
   var
         Params         :  TSAParameters;
         Status         :  TSAStatus;
         LT             :  TLoadTable;
         Errors         :  TStringList;
         Sec,
         ST0Mode,
         SAcceptanceF,
         SSchedule      :  AnsiString;
         RealMaxIters   :  Real;
   begin
   Sec := ShortNames[mhSA] + '.';
   Status.ShowMessage := ShowMessage;
   Status.SaveBest := True;
   with Params, Status do
      begin
      Assert(SizeOf(Acceptance.Style) = 4, ErrorInvalidEnumSize);
      Assert(SizeOf(Schedule.Type_  ) = 4, ErrorInvalidEnumSize);
      Assert(SizeOf(T0Mode          ) = 4, ErrorInvalidEnumSize);
      InitLoadTable(LT);
      AddLoadParam(LT, T0,                Sec + 'T0',              RealNonNeg  );
      AddLoadParam(LT, Tfin,              Sec + 'Tfin',            RealNonNeg  );
      AddLoadParam(LT, dEmin,             Sec + 'dEmin',           RealNonNeg  );
      AddLoadParam(LT, dEmax,             Sec + 'dEmax',           RealNonNeg  );
      AddLoadParam(LT, ST0Mode,           Sec + 'T0Mode',          nil         );
      AddLoadParam(LT, TfinEBased,        Sec + 'TfinEBased'                   );
      AddLoadParam(LT, NReheat,           Sec + 'NReheat',          IntNonNeg  );
      AddLoadParam(LT, FastReheat,        Sec + 'FastReheat'                   );
      AddLoadParam(LT, Smoothing,         Sec + 'Smoothing',       RealNonNeg  );
      AddLoadParam(LT, SAcceptanceF,      Sec + 'Acceptance',      nil         );
      AddLoadParam(LT,  Acceptance.P,     Sec + 'AcceptanceP',     RealNonNeg  );
      AddLoadParam(LT, SSchedule,         Sec + 'Schedule',        nil         );
      AddLoadParam(LT, Schedule.P,        Sec + 'ScheduleP',       RealNonNeg  );
      AddLoadParam(LT, RealMaxIters,      Sec + 'Iterations',      RealNonNeg  );
      AddLoadParam(LT, IterStatus,        Sec + 'StatusIters',      IntNonNeg  );
      AddLoadParam(LT, ScoreToReach,            'ScoreToReach',    nil         );
      LoadFromConfig(LT, Config, Errors);
      GetEnum(@T0Mode, Errors, ST0Mode, 'Manual EBased AutoLow AutoHigh');
      GetEnum(@Acceptance.Style, Errors, SAcceptanceF,
         'Exp Power Tsallis Threshold Barker');
      GetEnum(@Schedule.Type_, Errors, SSchedule, 'Zero Log Power Exp');
      if Errors.N = 0 then
         begin
         MaxIters := Round(RealMaxIters);
         SimulatedAnnealing(Best, Stats, Params, Status);
         end
      else
         DisplayErrors(Errors);
      end;
   end;


// Run the tabu search with parameters specified in Config, return the Best
// solution and run statistics Stats
procedure RunTS(
   var   Best        :  TSolution;
   var   Stats       :  TRunStats;
   const Config      :  TIniConfig;
         Algorithm   :  TMetaheuristic);
   var
         Params      :  TTSParameters;
         Status      :  TTSStatus;
         LT          :  TLoadTable;
         Errors      :  TStringList;
         Sec         :  AnsiString;
   begin
   Sec := ShortNames[mhTS] + '.';
   Status.ShowMessage := ShowMessage;
   Status.SaveBest := True;
   with Params, Status do
      begin
      InitLoadTable(LT);
      AddLoadParam(LT, MaxIters,    Sec + 'Iterations',     IntNonNeg   );
      AddLoadParam(LT, IterStatus,  Sec + 'StatusIters',    IntNonNeg   );
      AddLoadParam(LT, ScoreToReach,      'ScoreToReach',   nil         );
      LoadFromConfig(LT, Config, Errors);
      if Errors.N = 0 then
         TabuSearch(Best, Stats, Params, Status, {RandomInit:} True) else
         DisplayErrors(Errors);
      end;
   end;
   

// Run the metaheuristic specified in a config file
procedure Main;
   const
         PathConfig  = 'config.ini';
         PathRuns    = 'Runs.txt';
         PathBest    = 'Runs_Best';
         ErrorInvalidAlg = 'Invalid algorithm specified';
         ErrorUnknownAlg = 'Unknown metaheuristic';
         RunTable : array [TMetaheuristic] of ProcRun =
           (RunGA, RunSA, RunLS, RunTS);
   var
         Config         :  TIniConfig;
         LT             :  TLoadTable;
         Errors         :  TStringList;
         Algorithm      :  TMetaheuristic;
         SAlgorithm     :  AnsiString;
         NRuns, Run     :  Integer;
         Solution, Best :  TSolution;
         //RunTime,
         //TotalTime      :  TPreciseTime;
         //   TimeLimit   :  Real;
         //UseTimeLimit   :  Boolean;
         Done           :  Boolean;
         Stats          :  TRunStats;
         FileRuns       :  Text;
   begin
   repeat
      // Load the config
      LoadConfig(Config, PathConfig, Errors);
      if Errors.N <> 0 then
         begin
         DisplayErrors(Errors);
   {<}   break;
         end;

      // Load the algorithm
      Assert(SizeOf(Algorithm) = 4, ErrorInvalidEnumSize);
      InitLoadTable(LT);
      AddLoadParam(LT, SAlgorithm,     'Algorithm',      nil         );
      AddLoadParam(LT, NRuns,          'NRuns',          IntPos      );
      //AddLoadParam(LT,    TimeLimit,      'TimeLimit',   RealNonNeg  );
      //AddLoadParam(LT, UseTimeLimit,   'UseTimeLimit'                );
      LoadFromConfig(LT, Config, Errors);
      GetEnum(@Algorithm, Errors, SAlgorithm, 'GA SA LS TS');
      if Errors.N <> 0 then
         begin
         DisplayErrors(Errors);
   {<}   break;
         end;

      // Run the metaheuristic
      if (TryOpenWrite(fileRuns, PathRuns, ShowMessage) = Success) then
         begin
         WriteLn(fileRuns,
            'Run',         Tab,
            'Score',       Tab,
            'NFEfull',     Tab,
            'NFEpartial',  Tab,
            'Iters',       Tab,
            'Time'         );

         Run := 1;
         //StartTiming(TotalTime);
         repeat
            TryShowMessage(
               Format(MsgRunStarted, [ShortNames[Algorithm], Run]), 
               ShowMessage);
            
            Stats := EmptyStats;
            //StartTiming(RunTime);
            RunTable[Algorithm](Solution, Stats, Config, Algorithm);
            //StopTiming(RunTime);
            TryShowMessage(ShortNames[Algorithm] + MsgRunFinished, ShowMessage);
            if (Run = 1) or (CompareScores(Solution, Best) = scoreBetter) then
               begin
               AssignSolution(Best, Solution);
               TrySaveSolution(PathBest, Best, ShowMessage);
               end;
            WriteLn(fileRuns,
               Run,                          Tab,
               FormatScore(Solution.Score),  Tab,
               Stats.NFEfull,                Tab,
               Stats.NFEpartial,             Tab,
               Stats.Iters                   );
               //RunTime.dt                    );
            Flush(fileRuns);

            Inc(Run);
            Done := Run > NRuns;
            //StopTiming(TotalTime);
            //case UseTimeLimit of
            //   False:   Done := Run > NRuns;
            //   True:    Done := TotalTime.dt > TimeLimit;
            //   end;
         until Done;
         Close(fileRuns);
         end;
   until True;
   end;

begin
Randomize;
Main;
end.
