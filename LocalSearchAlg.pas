{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit LocalSearchAlg; ////////////////////////////////////////////////////////////////
{
>> Version: 0.7

>> Description
   Implementation of local search supporting variable neighborhood search and several
   search modes.

>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> Notes
   Chain mode is typically the most efficient one. It requires slightly
   more move evaluations, but greatly reduces move generation.
   
>> Usage
   The user must supply MakeLSMoveList, PerformMove and UndoMove routines via the
   problem definition module.
   
>> ToDo
   - Experiment with combo mode to see if it deserves a comeback

>> Changelog
   0.7 : 2019.10.01  + Multirun statistics collection
   0.6 : 2019.05.21  ~ Renamed IsMinimize to Minimization
   0.5 : 2018.09.16  - Combo mode
                     ~ Freepascal compatibility
                     + Improvement section moved from acceptance
   0.4 : 2012.02.22  + Run statistics collection
                     + Improvement procedure
   0.3 : 2011.09.26  + Test version of Combo LS
   0.2 : 2011.08.29  ~ Common interface for classical and smart local search
                     + Variable neighborhood search
   0.1 : 2011.08.15  + Smart local search
   0.0 : 2011.05.09  + Initial version
   Notation: + added, - removed, * fixed, ~ changed
}
{$MINENUMSIZE 4}
interface ///////////////////////////////////////////////////////////////////////////
uses
      Common, 
      Messages,
      Problem;

type
      TLSMode = (lsmFirst = 0, lsmFull, lsmChain);

      TLSParameters = TLSMode;

      TLSStatus = TBasicStatus;
      
      TImproveMode = (imLocal, imStochastic, imSpecific);

      TImproveParams =
         record
         Mode        :  TImproveMode;
         Iters       :  Integer;
         LSMode      :  TLSMode;
         end;

// Perform local search starting from Solution, update Stats and
// MultirunStats. The type of local search is determined by Params. 
// In case of RandomInit, Solution is initialized randomly.
procedure LocalSearch(
   var   Solution       :  TSolution;
   var   Stats          :  TRunStats;
   var   MultirunStats  :  TMultirunStats;
   const Params         :  TLSParameters;
   const Status         :  TLSStatus;
         RandomInit     :  Boolean);

// Apply local search with LSMode to Solution, disregard status and statistics
procedure LSImprovement(
   var   Solution :  TSolution;
         LSMode   :  TLSMode);

// Make Solution's neighbour, accept only in case of improvement. The result 
// indicates the outcome.
function GreedyMove(
   var   Solution :  TSolution
         )        :  Boolean;         

// Improve Solution according to ImproveParams
procedure Improve(
   var   Solution       :  TSolution;
   const ImproveParams  :  TImproveParams);

implementation //////////////////////////////////////////////////////////////////////
uses
      Math,
      InvSys,
      ExtraMath,
      Arrays,
      Sorting;
const
      PathStatus = 'LS_Status.txt';
      NameBest   = 'LS_Best';
      
{-----------------------<< Local Search >>------------------------------------------}

// Search the MoveList for a move that improves the Solution and perform it upon
// finding. In case of FullSearch, the whole move list is scanned for the best move, 
// otherwise the search stops at the first improving move. Trial moves are deleted 
// from MoveList and NFE is updated in the process. The result indicates whether an 
// improving move was found.
function MakeMove(
   var   Solution    :  TSolution;
   var   MoveList    :  TMoveList;
   var   NFE         :  Int64;
         FullSearch  :  Boolean
         )           :  Boolean;
   var
         i           :  Integer;
         BestScore   :  TScore;
         TrialMove,
          BestMove   :  TMove;
         Undo        :  TMoveUndo;
         Found       :  Boolean;
   begin
   // Search for an improving move
   Found := False;
   BestScore := Solution.Score;
   repeat
      // Perform a random trial move
      i := Random(MoveList.N);
      TrialMove := MoveList.Moves[i];
      DelMove(MoveList, i);
      PerformMove(Solution, Undo, TrialMove);
      Inc(NFE);

      // New best move?
      if (CompareScores(Solution.Score, BestScore) = scoreBetter) then
         begin
         BestScore := Solution.Score;
         BestMove := TrialMove;
         Found := True;
         end;

      // Restore initial state
      UndoMove(Solution, Undo);
   until (Found and not FullSearch) or (MoveList.N = 0);

   // Perform improving move if it was found
   if Found then
      PerformMove(Solution, BestMove);
   Result := Found;
   end;


// Perform classic local search starting from Solution, update Stats and 
// MultirunStats. In case of FullSearch, the best move is applied each iteration.
procedure ClassicLocalSearch(
   var   Solution       :  TSolution;
   var   Stats          :  TRunStats;
   var   MultirunStats  :  TMultirunStats;
         FullSearch     :  Boolean;
   const Status         :  TLSStatus);
   var
         MoveList       :  TMoveList;
         Level          :  Integer;
         Found          :  Boolean;
         UseStatus      :  Boolean;
         
   const
         NStatsFields   =  3;

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
            Header[1] := 'Score';
            Header[2] := 'Level';
            end;
      end;

   // Add the current search statistics to MultirunStats
   procedure WriteStatus(
      var   MultirunStats  :  TMultirunStats;
      const Best           :  TSolution;
            Iters, Level   :  Integer);
      var   
            Data           :  TRealArray;
      begin
      SetLength(Data, NStatsFields);
      Data[0] := Iters;
      Data[1] := Best.Score;
      Data[2] := Level;
      AddSample(MultirunStats, Data);
      end;

   begin
   // Write the status header if necessary
   UseStatus := Status.IterStatus <> 0;
   if UseStatus then
      begin
      WriteHeader(MultirunStats);
      PrepareNextRun(MultirunStats);
      end;

   // Perform the search
   with Stats do
      begin
      repeat
         // Try making an improving move
         Inc(Stats.Iters);
         Level := 0;
         Found := False;
         repeat
            Inc(Level);
            MakeLSMoveList(MoveList, Solution, Level);
            if MoveList.N = 0 then
         {<}   break;
            Found := MakeMove(Solution, MoveList, NFEpartial, FullSearch);
         until Found;

         // Write status
         if Divisible(Iters, Status.IterStatus) then
            WriteStatus(MultirunStats, Solution, Iters, Level);
      until not Found;

      // Run complete
      ShowNewBestScore(Solution, Status.ShowMessage);
      end;
   end;


// #HACK huge, may need splitting
// #HACK rewrite using ExtMoveLists
// Perform chain local search starting from Solution, update Stats and
// MultirunStats
procedure ChainLocalSearch(
   var   Solution       :  TSolution;
   var   Stats          :  TRunStats;
   var   MultirunStats  :  TMultirunStats;
   const Status         :  TLSStatus);
   var
         i              :  Integer;
         MoveList,
         Candidates     :  TMoveList;
         Scores         :  TRealArray;
         Ranking        :  TIntArray;
         Order          :  TSortOrder;
         OldScore       :  TScore;
         Undo           :  TMoveUndo;
         NFound         :  Integer;
         Level          :  Integer;
         UseStatus      :  Boolean;
   const
         NStatsFields   =  5;
         
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
            Header[1] := 'Score';
            Header[2] := 'Level';
            Header[3] := 'NTested';
            Header[4] := 'NFound';
            end;
      end;
      
   // Add the current search statistics to MultirunStats
   procedure WriteStatus(
      var   MultirunStats  :  TMultirunStats;
      const Best           :  TSolution;
            Iters,
            Level,
            NTested,
            NFound         :  Integer);
      var   
            Data           :  TRealArray;
      begin
      SetLength(Data, NStatsFields);
      Data[0] := Iters;
      Data[1] := Best.Score;
      Data[2] := Level;
      Data[3] := NTested;
      Data[4] := NFound;
      AddSample(MultirunStats, Data);
      end;

   begin
   // Initialization
   if Minimization then
      Order := soAscending else
      Order := soDescending;

   // Write the status header if necessary
   UseStatus := Status.IterStatus <> 0;
   if UseStatus then
      begin
      WriteHeader(MultirunStats);
      PrepareNextRun(MultirunStats);
      end;

   with Stats do
      begin
      // Search loop
      repeat
         Inc(Iters);
         Level := 0;
         repeat
            // Make the move list
            Inc(Level);
            MakeLSMoveList(MoveList, Solution, Level);
            if MoveList.N = 0 then
         {<}   break;

            // Make a list of candidate moves
            InitMoveList(Candidates);
            SetLength(Scores, MoveList.N);
            OldScore := Solution.Score;
            for i := 0 to MoveList.N - 1 do
               begin
               PerformMove(Solution, Undo, MoveList.Moves[i]);
               if CompareScores(Solution.Score, OldScore) = scoreBetter then
                  begin
                  AddMove(Candidates, MoveList.Moves[i]);
                  Scores[Candidates.N - 1] := Solution.Score - OldScore;
                  end;
               UndoMove(Solution, Undo);
               end;
            SetLength(Scores, Candidates.N);

            // #TODO Verify the equivalence
            //SortOrder(Ranking, @Scores, RealArrayCompare, Candidates.N, Order); // old version
            OrderRealArray(Ranking, Scores, Order);

            Inc(NFEpartial, MoveList.N);

            // Try all candidate moves
            i := 0;
            NFound := 0;
            while (i < Candidates.N) and
               (CompareScores(Scores[ Ranking[i] ], 0) = scoreBetter) do
               begin
               OldScore := Solution.Score;
               PerformMove(Solution, Undo, Candidates.Moves[ Ranking[i] ]);
               if CompareScores(Solution.Score, OldScore) = scoreBetter then
                  Inc(NFound)
               else
                  UndoMove(Solution, Undo);
               Inc(i);
               Inc(NFEpartial);
               end;
         until NFound <> 0;

         // Write status
         if NFound <> 0 then
            ShowNewBestScore(Solution, Status.ShowMessage);
         if Divisible(Iters, Status.IterStatus) then
            WriteStatus(MultirunStats, Solution, Iters, Level, {NTested:} i, NFound);
      until NFound = 0;
      end;
   end;

   
// Perform local search starting from Solution, update Stats and
// MultirunStats. The type of local search is determined by Params. 
// In case of RandomInit, Solution is initialized randomly.
procedure LocalSearch(
   var   Solution       :  TSolution;
   var   Stats          :  TRunStats;
   var   MultirunStats  :  TMultirunStats;
   const Params         :  TLSParameters;
   const Status         :  TLSStatus;
         RandomInit     :  Boolean);
   begin
   // Initialization
   if RandomInit then
      NewSolution(Solution);

   // Run classic or chain local search
   case Params of
      lsmFirst, lsmFull: 
         ClassicLocalSearch(
            Solution, 
            Stats, 
            MultirunStats,
            {FullSearch:} Params = lsmFull, 
            Status
            );
      lsmChain:
         ChainLocalSearch(Solution, Stats, MultirunStats, Status);
      else
         Assert(False);
      end;

   // Run complete
   if Status.SaveBest then
      TrySaveSolution(NameBest, Solution, Status.ShowMessage);
   end;
   
{-----------------------<< Improvement >>-------------------------------------------}

// Make Solution's neighbour, accept only in case of improvement. The result 
// indicates the outcome.
function GreedyMove(
   var   Solution :  TSolution
         )        :  Boolean;
   var
         OldScore :  TScore;
         Undo     :  TSAUndo;
   begin
   OldScore := Solution.Score;
   MakeNeighbour(Solution, Undo, {T:} 0);
   if CompareScores(Solution.Score, OldScore) = scoreWorse then
      begin
      UndoSAMove(Solution, Undo);
      Result := Fail;
      end
   else
      Result := Success;
   end;
   

// Apply local search with LSMode to Solution, disregard status and statistics
procedure LSImprovement(
   var   Solution :  TSolution;
         LSMode   :  TLSMode);
   var
         Stats    :  TRunStats;
         Params   :  TLSParameters;
         Dummy    :  TMultirunStats;
   begin
   Stats  := EmptyStats;
   Params := LSMode;
   LocalSearch(Solution, Stats, Dummy, Params, NoStatus, {RandomInit:} False);
   end;
   

// Improve Solution according to ImproveParams
procedure Improve(
   var   Solution       :  TSolution;
   const ImproveParams  :  TImproveParams);
   var
         i              :  Integer;
   begin
   with ImproveParams do
      case Mode of
         imLocal:
            LSImprovement(Solution, LSMode);
         imStochastic:
            for i := 1 to Iters do
               GreedyMove(Solution);
         imSpecific:
            SpecialImprove(Solution);
         else
            Assert(False);
         end;
   end;


end.
