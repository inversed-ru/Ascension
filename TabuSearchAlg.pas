{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit TabuSearchAlg; /////////////////////////////////////////////////////////////////
{
>> Version: 1.5

>> Description
   Tabu search escapes local optima by maintaining a tabu list of forbidden moves. At
   each  iteration,  all non-tabu moves are evaluated and the best is  performed  and
   added to the tabu list for Tenure iterations.
   
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> Usage
   By means of a problem definition  unit  the  user must supply  the  functions that
   initialize a tabu list, increase its age, add an item to it, check whether a given
   move is tabu, return the tabu tenure and make a move list.

>> ToDo
    - Inform the user if a solution is stuck (all moves are tabu) via messages and 
      the status file. This could indicate that the tenure is too high.
    - Experiment with cooperative multi-solution schemes.
    - Add Fan and Filter algorithm
    - Tabu search with same structure as in balanced hill climbing
   
>> Changelog
   1.5 : 2019.10.01  + Multirun statistics collection
   1.4 : 2019.08.25  + Cooperative tabu search
   1.3 : 2018.09.18  - Experimental tabu search variants
                     ~ FreePascal compatibility
   1.2 : 2011.09.25  + Beam tabu search
   1.1 : 2011.09.11  + Cooperative tabu search
   1.0 : 2011.03.28  + Unit header
                     ~ Slightly reorganized
   0.0 : 2010.11.22  + Initial version
   Notation: + added, - removed, * fixed, ~ changed  
}
{$MINENUMSIZE 4}
interface ///////////////////////////////////////////////////////////////////////////
uses
      Common,
      Problem;

type
      TTSParameters =
         record
         MaxIters       :  Integer;
         PopSize        :  Integer;
         ScoreToReach   :  TScore;
         end;

      TTSStatus = TBasicStatus;

// Run a tabu search with given Params, return the Best solution found 
// and the search Stats, update MultirunStats. If RandomInit = True, 
// a random initial solution is created, otherwise  the search starts from
// the provided one.
procedure TabuSearch(
   var   Best           :  TSolution;
   var   Stats          :  TRunStats;
   var   MultirunStats  :  TMultirunStats;
   const Params         :  TTSParameters;
   const Status         :  TTSStatus;
         RandomInit     :  Boolean);  
         
// Run a cooperative tabu search with given Params, return the Best   
// solution found, update MultirunStats. If RandomInit = True, a random 
// initial solution is created, otherwise the search starts from the provided one.
procedure CoopTabuSearch(
   var   Best           :  TSolution;
   var   MultirunStats  :  TMultirunStats;
   const Params         :  TTSParameters;
   const Status         :  TTSStatus;
         RandomInit     :  Boolean);

implementation //////////////////////////////////////////////////////////////////////
uses
      InvSys,
      ExtraMath,
      Arrays,
      SolutionLists;

const
      PathStatus = 'TS_Status.txt';
      NameBest   = 'TS_Best';

// Return a random best Move from MoveList that is either not in TabuList 
// or improves the BestSoFar score
procedure GetTabuMove(
   var   Move        :  TMove;
   var   Solution    :  TSolution;
   var   TabuList    :  TTabuList;
   const MoveList    :  TMoveList;
         BestSoFar   :  TScore);
   var
         i           :  Integer;
         BestMoves   :  TMoveList;
         BestScore   :  TScore;
         TrialMove   :  TMove;
         Comparison  :  TScoreComparison;
         IsTabu,
         IsBestest   :  Boolean;
         Undo        :  TMoveUndo;
   begin
   // Search the whole move list for the best moves
   with Solution do
      repeat
         InitMoveList(BestMoves);
         BestScore := 0;
         for i := 0 to MoveList.N - 1 do
            begin
            // Perform a trial move
            TrialMove := MoveList.Moves[i];
            IsTabu := IsMoveTabu(TrialMove, Solution, TabuList);
            PerformMove(Solution, Undo, TrialMove);
            IsBestest := CompareScores(Score, BestSoFar) = scoreBetter;

            // Update the list of best moves
            if not IsTabu or IsBestest then
               begin
               if BestMoves.N = 0 then
                  BestScore := Score;
               Comparison := CompareScores(Score, BestScore);
               if Comparison = scoreBetter then
                  begin
                  BestScore := Score;
                  BestMoves.N := 0;
                  AddMove(BestMoves, TrialMove);
                  end
               else if Comparison = scoreEqual then
                  AddMove(BestMoves, TrialMove);
               end;

            // Restore the initial state
            UndoMove(Solution, Undo);
            end;
         if BestMoves.N = 0 then
            AgeTabuList(TabuList);
      until BestMoves.N <> 0;
      
   // Pick a random best move
   with BestMoves do
      Move := Moves[ Random(N) ];
   end;


// Perform a single tabu search step with Work, update Best. If Best has 
// been improved, return True and display a message via Status.ShowMessage.
function TabuStep(
   var   Work, Best  :  TSolution;
   var   TabuList    :  TTabuList;
         Tenure      :  Integer;
   const Status      :  TTSStatus
         )           :  Boolean;
   var
         Move        :  TMove;
         MoveList    :  TMoveList;
   begin
   // Find a move
   MakeTSMoveList(MoveList, Work);
   GetTabuMove(Move, Work, TabuList, MoveList, Best.Score);

   // Update the tabu list
   AgeTabuList(TabuList);
   AddToTabuList(TabuList, Move, Tenure, Work);

   // Make the move
   PerformMove(Work, Move);
   Result := TryUpdateBest(Best, Work, Status.ShowMessage);
   end;


// Run a tabu search with given Params, return the Best solution found 
// and the search Stats, update MultirunStats. If RandomInit = True, 
// a random initial solution is created, otherwise  the search starts from
// the provided one.
procedure TabuSearch(
   var   Best           :  TSolution;
   var   Stats          :  TRunStats;
   var   MultirunStats  :  TMultirunStats;
   const Params         :  TTSParameters;
   const Status         :  TTSStatus;
         RandomInit     :  Boolean);
   var
         Work           :  TSolution;
         TabuList       :  TTabuList;
         Tenure         :  Integer;
         Iters          :  Integer;
   const
         NStatsFields   =  4;

   // Create the header of MultirunStats
   procedure WriteHeader(
      var   MultirunStats  :  TMultirunStats);
      begin
      with MultirunStats do
         if NVars = 0 then
            begin
            InitMultirunStats(MultirunStats, NStatsFields);
            SetLength(Header, NStatsFields);
            Header[0] := 'Iter';
            Header[1] := 'BestScore';
            Header[2] := 'WorkScore';
            Header[3] := 'Tenure';
            end;
      end;
      
   // Add the current search statistics to MultirunStats
   procedure WriteStatus(
      var   MultirunStats  :  TMultirunStats;
      const Work, Best     :  TSolution;
            Iters, Tenure  :  Integer);
      var   
            Data           :  TRealArray;
      begin
      SetLength(Data, NStatsFields);
      Data[0] := Iters;
      Data[1] := Best.Score;
      Data[2] := Work.Score;
      Data[3] := Tenure;
      AddSample(MultirunStats, Data);
      end;

   begin
   // Initialization
   if RandomInit then
      NewSolution(Best);
   AssignSolution(Work, Best);
   InitTabuList(TabuList);
   Iters := 0;

   // Write the status header
   if Status.IterStatus > 0 then
      begin
      WriteHeader(MultirunStats);
      PrepareNextRun(MultirunStats);
      end;

   repeat
      // Perform a single tabu search step
      Inc(Iters);
      Tenure := TabuTenure(Iters / Params.MaxIters);
      TabuStep(Work, Best, TabuList, Tenure, Status);

      // Write the status
      if Divisible(Iters, Status.IterStatus) then
         WriteStatus(MultirunStats, Work, Best, Iters, Tenure);
   until Iters = Params.MaxIters;

   // Run complete
   Stats.NFEPartial := Iters;
   TrySaveSolution(NameBest, Best, Status.ShowMessage);
   end;


// Run a cooperative tabu search with given Params, return the Best   
// solution found, update MultirunStats. If RandomInit = True, a random 
// initial solution is created, otherwise the search starts from the provided one.
procedure CoopTabuSearch(
   var   Best           :  TSolution;
   var   MultirunStats  :  TMultirunStats;
   const Params         :  TTSParameters;
   const Status         :  TTSStatus;
         RandomInit     :  Boolean);
   var
         Work,
         LocalBest      :  TSolutionList;
         TabuLists      :  array of TTabuList;
         i, j, k,
         IdUpdGlobalBest:  Integer;
         Iters          :  Integer;
         LSStats        :  TRunStats;
         UpdGlobalBest  :  Boolean;
         UpdLocalBest   :  TBoolArray;
   const
         NStatsFields   =  3;
         //LSParams       :  TLSParameters = lsmFast;
 
   // Create the header of MultirunStats
   procedure WriteHeader(
      var   MultirunStats  :  TMultirunStats);
      begin
      with MultirunStats do
         if NVars = 0 then
            begin
            InitMultirunStats(MultirunStats, NStatsFields);
            SetLength(Header, NStatsFields);
            Header[0] := 'Iter';
            Header[1] := 'BestScore';
            Header[2] := 'WorkScore';
            end;
      end;

   // Add the current search statistics to MultirunStats
   procedure WriteStatus(
      var   MultirunStats  :  TMultirunStats;
      const Work           :  TSolutionList;
      const Best           :  TSolution;
            Iters, 
            IdSelected     :  Integer);
      var   
            Data           :  TRealArray;
      begin
      SetLength(Data, NStatsFields);
      Data[0] := Iters;
      Data[1] := Best.Score;
      Data[2] := Work._[IdSelected].Score;
      AddSample(MultirunStats, Data);
      end;

   begin
   // Initialization
   if RandomInit then
      begin
      NewSolList(Work, Params.PopSize);
      AssignSolution(Work.Best, Best);
      end
   else
      begin
      //NewSolution(Best);
      //LocalSearch(Best, LSStats, LSParams, NoStatus, {RandomInit:} False);
      FillSolList(Work, Best, Params.PopSize);
      end;
   AssignSolList(LocalBest, Work);
   SetLength(TabuLists,      Params.PopSize);
   InitArray(UpdLocalBest,   Params.PopSize, False);
   for i := 0 to Params.PopSize - 1 do
      InitTabuList(TabuLists[i]);
   Iters := 0;
   IdUpdGlobalBest := 0;
   UpdGlobalBest := False;

   // Write status header
   if Status.IterStatus > 0 then
      begin
      WriteHeader(MultirunStats);
      PrepareNextRun(MultirunStats);
      end;

   repeat
      Inc(Iters);
      i := Modulo(-Iters, Work.N);

      // Share the local best
      if (i < (Work.N - 1)) and UpdLocalBest[i + 1] then 
         //if False then
         //if CompareScores(LocalBest._[i + 1], LocalBest._[i]) = scoreBetter then
         //if CompareScores(LocalBest._[i + 1], Work._[i]) = scoreBetter then
         if True then
            begin
            AssignSolution(Work._[i], LocalBest._[i + 1]);
            AssignTabuList(TabuLists[i], TabuLists[i + 1]);
            if CompareScores(Work._[i], LocalBest._[i]) = scoreBetter then
               begin
               AssignSolution(LocalBest._[i], Work._[i]);
               UpdLocalBest[i] := True;
               end;
            UpdLocalBest[i + 1] := False;
            end;
         
      // Perform a tabu step
      UpdGlobalBest := TabuStep(Work._[i], Best, 
         TabuLists[i], TabuTenure(1 - i / (Work.N - 1)), Status);
      if UpdGlobalBest then
         IdUpdGlobalBest := i;
      if CompareScores(Work._[i], LocalBest._[i]) = scoreBetter then
         begin
         AssignSolution(LocalBest._[i], Work._[i]);
         UpdLocalBest[i] := True;
         end;

      // Write the status
      if Divisible(Iters, Status.IterStatus) then
         WriteStatus(MultirunStats, Work, Best, Iters, i);
   until Iters = Params.MaxIters;

   // Run complete
   TrySaveSolution(NameBest, Best, Status.ShowMessage);
   end;
   
end.
