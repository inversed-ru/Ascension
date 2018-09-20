{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit SimAnnealAlg; //////////////////////////////////////////////////////////////////
{
>> Version: 2.0

>> Description
   Implementation of a simulated annealing algorithm featuring various acceptance
   criteria and temperature schedules.

>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> How to use  
   The user must supply MakeNeighbour and UndoSAMove routines via the
   problem definition module.

>> ToDo
    ! Fix the crash when Auto T0 < Tfin
    - Incorporate advanced schedules from version 2.3. Need to check schedules that 
      involve Gaussian integrals after changes in SpecFuncs.
    ? We can estimate the probability of improving the best solution using standard 
      deviation of the score. Adaptive scheme: dLnT ~ 1 / ProbImp.
    - Modified Lam schedule
    - Simulated inertia
    - Measure and log correlation length of the score, maybe use it in adaptation
    - Barker acceptance is atypical and complicates the code. Maybe it should be 
      removed, as it proved to be less efficient.
    - Temperature schedule constants are calculated inside a loop. They should be
      precalculated instead if this impacts the performance.
    - Neighborhood dependent on normalized temperature may be useful for some
      problems (Heilbronn, Lennard-Jones)
    ? Multi-particle modification: there are multiple solutions, but only the best
      one is selected at each iteration. Has no effect at low temperatures.

>> Changelog
   3.0 : 2018.09.20  - Experimental cooling schedules
                     - Experimental algorithms
                     ~ Automatic final temperature selection formula
                     ~ Freepascal compatibility
   2.3 : 2013.02.07  ~ Calibration updated with new filtering methods
   2.2 : 2012.04.29  ~ Cooling schedules reorganized
   2.1 : 2012.04.23  - Thermal hopping and exploration algorithms split into
                       basin hopping unit
   2.0 : 2011.11.27  + Critical temperature calibration
                     - Annealing rewritten: new schedules, acceptance criteria
                     + Automatic initial temperature option
                     + Basin hopping algorithm
   1.2 : 2011.07.22  + New experimental algorithms
   1.0 : 2011.03.28  ~ Added unit header
                     ~ Slightly reorganized
   0.0 : 2010.11.24  + Initial version
   Notation: + added, - removed, * fixed, ~ changed
}
{$MINENUMSIZE 4}
interface ///////////////////////////////////////////////////////////////////////////
uses
      Math,
      Common,
      Messages,
      Acceptance,
      Problem;

type
      TScheduleType = (stZero, stLog, stPower, stExp);

      TSchedule =
         record
         Type_ :  TScheduleType;
         P     :  Real;
         end;

      TSAParameters =
         record
         T0, TFin, Tc,
         dEmax, dEmin   :  Real;
         T0Mode         :  TT0Mode;
         TfinEBased     :  Boolean;
         Smoothing      :  Real;
         Acceptance     :  TAcceptance;
         Schedule       :  TSchedule;
         MaxIters       :  Int64;
         NReheat,
         PopSize        :  Integer;
         FastReheat     :  Boolean;
         ScoreToReach   :  TScore;
         Calibrate      :  Boolean;
         end;

      TSAStatus = TBasicStatus;

// Run the simulated annealing with specified Params, return the Best solution found 
// and the search Stats, report and save the progress according to Status
procedure SimulatedAnnealing(
   var   Best        :  TSolution;
   var   Stats       :  TRunStats;
   const Params      :  TSAParameters;
   const Status      :  TSAStatus);

implementation //////////////////////////////////////////////////////////////////////
uses
      InvSys,
      Arrays,
      ExtraMath,
      RandVars,
      Statistics,
      SpecFuncs,
      SolutionLists; 
const
      PathStatus = 'SA_Status.txt';
      NameBest   = 'SA_Best';


// Return the temperature at relative time t given the initial
// and final temperatures T0 and T1 and the Schedule
function GetTemperature(
         t              :  Real;     // [0 .. 1]
         T0, T1         :  Real;
   const Schedule       :  TSchedule
         )              :  Real;
   var
         Tau, P, A      :  Real;
   begin
   P := Schedule.P;
   case Schedule.Type_ of
      stZero:
         Result := 0;
      stLog:
         begin
         Tau := 1 / 2;
         A := Ln(1 + 1 / Tau) / (T0 / T1 - 1);
         Result := T0 * A / (A + Ln(1 + t / Tau));
         end;
      stPower:
         begin
         Tau := 1 / (Power(T0 / T1, 1 / P) - 1);
         Result := T0 * Power(1 + t / Tau, -P);
         end;
      stExp:
         begin
         Tau := Power(Ln(T0 / T1), -1 / P);
         Result := T0 * Exp(-Power(t / Tau, P));
         end;
      else
         Result := 0;
         Assert(False, 'Unknown schedule');
      end;
   end;


// Return characteristic absolute value of the logarithmic derivative
function LogSlope(
         T0, Tfin, Tc   :  Real;
   const Schedule       :  TSchedule
         )              :  Real;
   var
         P              :  Real;
   begin
   P := Schedule.P;
   case Schedule.Type_ of
      stZero:
         Result := 1;
      stLog:
         Result := Sqrt(T0 / Tfin) / Ln(3);
      stPower:
         Result := P * Sqrt(
                (Power(T0 / Tfin,  1 / P) - 1) *
            (1 - Power(T0 / Tfin, -1 / P)) );
      stExp:
         Result := Power(Ln(T0 / Tfin), 1 / P);
      else
         Result := 0;
         Assert(False, 'Unknown schedule');
      end;
   end;

   
// Return the initial temperature calculated based on Params
function GetT0(
   const Params   :  TSAParameters
         )        :  Real;
   var
         HotIters :  Integer;
   begin
   with Params do
      begin
      case T0Mode of
         t0Manual:
            Result := T0;
         t0EBased:
            Result := dEToT0(dEmax, Acceptance, t0AutoLow);
         t0AutoLow, t0AutoHigh:
            begin
            HotIters := Round(Power(MaxIters / 6, 2 / 3));
            Result := GetAutoT0(t0AutoLow, Acceptance, HotIters);
            end;
         else
            Result := 0;
            Assert(False);
         end;
      end;
   end;


// Return the final temperature calculated based on Params
function GetTfin(
   const Params   :  TSAParameters
         )        :  Real;
   begin
   with Params do
      begin
      Result := Tfin;
      if TfinEBased then
         Result := dEmin / InvProbAccept(Ln(MaxIters + 0.0) / MaxIters, Acceptance);
      end;
   end;


// #HACK Huge, needs simplification
// Run the simulated annealing with specified Params, return the Best solution found 
// and the search Stats, report and save the progress according to Status
procedure SimulatedAnnealing(
   var   Best        :  TSolution;
   var   Stats       :  TRunStats;
   const Params      :  TSAParameters;
   const Status      :  TSAStatus);

   // Write the header of the status file FileStatus
   procedure WriteHeader(
      var   FileStatus  :  Text);
      begin
      WriteLn(FileStatus,
         'Iter',        Tab,
         'BestScore',   Tab,
         'WorkScore',   Tab,
         'T',           Tab,
         'PAccept',     Tab,
         'PImprove',    Tab,
         'AvgScore'     );
      end;

   // Write a single line with annealing parameters to FileStatus
   procedure WriteStatus(
      var   FileStatus  :  Text;
            Iters       :  Int64;
      const Work, Best  :  TSolution;
            T, PAccept,
            PImprove,
            AvgScore    :  Real);
      begin
      WriteLn(FileStatus,
         Iters,         Tab,
         Best.Score,    Tab,
         Work.Score,    Tab,
         T,             Tab,
         PAccept,       Tab,
         PImprove,      Tab,
         AvgScore       );
      Flush(FileStatus);
      end;

   var
         Work                 :  TSolution;
         Iters, SubIters,
         MaxSubIters          :  Int64;
         Stage                :  Integer;
         T, T0Stage,
         UseT0, UseTfin,
         RelTime,
         PAccept,  PImprove,
          Accepted, Improved,
         Tau, Alpha, AvgScore :  Real;
         Undo                 :  TSAUndo;
         OldScore, PrevBest   :  TScore;
         Done                 :  Boolean;
         fileStatus           :  Text;
   begin
   if (Status.IterStatus = 0) or
      TryOpenWrite(fileStatus, PathStatus, Status.ShowMessage) then with Params do
      begin

      // Initialization
      NewSolution(Best);
      PrevBest := Best.Score;
      Iters := 0;
      UseT0   := GetT0  (Params);
      UseTfin := GetTfin(Params);
      if Status.IterStatus <> 0 then
         WriteHeader(fileStatus);

      // Perform all reheat stages
      for Stage := 0 to NReheat - 1 do
         begin
         // Initialize the solution, temperature and statistics
         AssignSolution(Work, Best);
         T0Stage := UseT0 * Power(UseTfin / UseT0, Stage / NReheat);
         if FastReheat then
            MaxSubIters := Round( (NReheat - Stage) *
               2 * MaxIters / (NReheat * (NReheat + 1)) )
         else
            MaxSubIters := Round(MaxIters / NReheat);
         SubIters := 0;
         PAccept  := 1;
         PImprove := 1 / 2;
         AvgScore := Work.Score;
         //Tau := MaxSubIters / LogSlope(T0Stage, UseTfin, Tc, Schedule);
         Tau := Power(MaxSubIters + 0.0, 2 / 3);
         Alpha := Min(1 / (Smoothing * Tau), 1);

         // Run a single annealing stage
         repeat
            // Set the temperature
            Inc(Iters);
            Inc(SubIters);
            RelTime := SubIters / MaxSubIters;
            T := GetTemperature(RelTime, T0Stage, UseTfin, Schedule);

            // Make a candidate solution
            OldScore := Work.Score;
            MakeNeighbour(Work, Undo, T);
            Improved := Ord(CompareScores(Work.Score, OldScore) = scoreBetter);

            // Accept or reject
            if Random < ProbAccept(Work.Score, OldScore, T, Acceptance) then
               begin
               ReplaceIfBetter(Best, Work);
               Accepted := 1;
               end
            else
               begin
               UndoSAMove(Work, Undo);
               Accepted := 0;
               end;

            // Update statistics
            PAccept  := Blend(PAccept,  Accepted,   Alpha);
            PImprove := Blend(PImprove, Improved,   Alpha);
            AvgScore := Blend(AvgScore, Work.Score, Alpha);
            
            // Write status
            if Divisible(Iters, Status.IterStatus) then
               begin
               WriteStatus(fileStatus, Iters, Work, Best, T, PAccept, PImprove, AvgScore);
               if CompareScores(Best.Score, PrevBest) = scoreBetter then
                  begin
                  ShowNewBestScore(Best, Status.ShowMessage);
                  PrevBest := Best.Score;
                  end;
               end;

            Done := SubIters = MaxSubIters;
         until Done;
         end;

      // Run complete
      if Status.IterStatus <> 0 then
         Close(fileStatus);
      Stats.NFEPartial := Iters;
      TrySaveSolution(NameBest, Best, Status.ShowMessage);
      end;
   end;

end.
