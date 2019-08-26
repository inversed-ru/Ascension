{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit TabuSearchAlg; /////////////////////////////////////////////////////////////////
{
>> Version: 1.4

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
// and the search Stats, report and save the progress according to Status. 
// If RandomInit = True, a random initial solution is created, otherwise 
// the search starts from the provided one.
procedure TabuSearch(
   var   Best        :  TSolution;
   var   Stats       :  TRunStats;
   const Params      :  TTSParameters;
   const Status      :  TTSStatus;
         RandomInit  :  Boolean);   
         
// Run a cooperative tabu search with given Params, return the Best   
// solution found, report and save the progress according to Status. 
// If RandomInit = True, a random initial solution is created, otherwise 
// the search starts from the provided one.
procedure CoopTabuSearch(
   var   Best           :  TSolution;
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
// and the search Stats, report and save the progress according to Status. 
// If RandomInit = True, a random initial solution is created, otherwise 
// the search starts from the provided one.
procedure TabuSearch(
   var   Best        :  TSolution;
   var   Stats       :  TRunStats;
   const Params      :  TTSParameters;
   const Status      :  TTSStatus;
         RandomInit  :  Boolean);
   var
         Work        :  TSolution;
         TabuList    :  TTabuList;
         Tenure      :  Integer;
         Iters       :  Integer;
         FileStatus  :  Text;
         StatusOk    :  Boolean;

   // Write the header of the status file FileStatus
   procedure WriteHeader(
      var   FileStatus  :  Text);
      begin
      WriteLn(FileStatus,
         'Iter',         Tab,
         'BestScore',    Tab,
         'WorkScore',    Tab,
         'Tenure'        );
      end;

   // Write the search status data to FileStatus
   procedure WriteStatus(
      var   FileStatus  :  Text;
            Iters       :  Integer;
      const Work, Best  :  TSolution;
            Tenure      :  Integer);
      begin
      WriteLn(FileStatus,
         Iters,         Tab,
         Best.Score,    Tab,
         Work.Score,    Tab,
         Tenure         );
      Flush(FileStatus);
      end;

   begin
   // Initialization
   if RandomInit then
      NewSolution(Best);
   AssignSolution(Work, Best);
   InitTabuList(TabuList);
   Iters := 0;

   // Write the status header
   StatusOk := TryOpenWrite(FileStatus, PathStatus, Status.ShowMessage) = Success;
   if StatusOk then
      WriteHeader(FileStatus);

   if StatusOk then
      begin
      repeat
         // Perform a single tabu search step
         Inc(Iters);
         Tenure := TabuTenure(Iters / Params.MaxIters);
         TabuStep(Work, Best, TabuList, Tenure, Status);

         // Write the status
         if Divisible(Iters, Status.IterStatus) then
            WriteStatus(FileStatus, Iters, Work, Best, Tenure);
      until Iters = Params.MaxIters;

      // Run complete
      Close(FileStatus);
      Stats.NFEPartial := Iters;
      TrySaveSolution(NameBest, Best, Status.ShowMessage);
      end;
   end;


// Run a cooperative tabu search with given Params, return the Best   
// solution found, report and save the progress according to Status. 
// If RandomInit = True, a random initial solution is created, otherwise 
// the search starts from the provided one.
procedure CoopTabuSearch(
   var   Best           :  TSolution;
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
         fileStatus     :  Text;
         LSStats        :  TRunStats;
         StatusOk,
         UpdGlobalBest  :  Boolean;
         UpdLocalBest   :  TBoolArray;
   //const
         //LSParams       :  TLSParameters = lsmFast;

   procedure WriteHeader(
      const fileStatus  :  Text);
      var
            i           :  Integer;
      begin
      Write(fileStatus,
         'Iter'         ,         Tab,
         'BestScore'    ,         Tab);
      for i := 1 to 4 do
         Write(fileStatus, i, Tab);
      WriteLn(fileStatus, 'SelScore');
      end;


   procedure WriteStatus(
      var   fileStatus  :  Text;
            Iters       :  Integer;
      const Work        :  TSolutionList;
      const Best        :  TSolution;
            IdSelected  :  Integer);
      var
            i           :  Integer;
      begin
      Write(fileStatus,
         Iters,         Tab,
         Best.Score,    Tab);
      WriteLn(fileStatus, Work._[IdSelected].Score);
      Flush(fileStatus);
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
   StatusOk := TryOpenWrite(fileStatus, PathStatus, Status.ShowMessage) = Success;
   if StatusOk then
      WriteHeader(fileStatus);

   if StatusOk then
      begin
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
            WriteStatus(fileStatus, Iters, Work, Best, i);
      until Iters = Params.MaxIters;

      // Run complete
      Close(fileStatus);
      TrySaveSolution(NameBest, Best, Status.ShowMessage);
      end;
   end;
   
end.
