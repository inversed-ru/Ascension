{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit Common; ////////////////////////////////////////////////////////////////////////
{
>> Version: 0.7

>> Description
   Common routines and types used by various metaheuristics.
   
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

** ToDo
    ? Split move lists and extended move lists sections into separate unit
    ? Use SignMinimize / SignMaximize instead of NormDeltaScore
    ? Rewrite NormDeltaScore using SignMinimize / SignMaximize
    
>> Changelog
   0.7 : 2018.09.18  ~ FreePascal compatibility
                     ~ Renamed TScoreRelation to TScoreComparison
                     + Missing routine comments
   0.6 : 2015.09.14  + SwapSols procedure
   0.5 : 2012.12.21  + DistToNearest function
   0.4 : 2012.02.22  + Extra move list functions
   0.1 : 2011.06.08  - Split 'SolutionList' into separate unit
   0.0 : 2011.03.28  + Initial version
   Notation: + added, - removed, * fixed, ~ changed  
}
{$MINENUMSIZE 4}                                                                                    
interface ///////////////////////////////////////////////////////////////////////////
uses
      Problem,
      Messages;
type
      TMetaheuristic =
        (mhGA, mhSA, mhLS, mhTS);

      PSolution  = ^TSolution;
      PSolutions = ^TSolutions;

      TBasicStatus =
         record
         IterStatus  :  Integer;
         SaveBest    :  Boolean;
         ShowMessage :  ProcMessage;
         end;

      TRunStats =
         record
         NFEfull,
         NFEpartial  :  Int64;
         Iters       :  Integer;
         end;
const
      ShortNames : array [TMetaheuristic] of AnsiString =
        ('GA', 'SA', 'LS', 'TS');

      NoStatus :  TBasicStatus =
         (IterStatus: 0; SaveBest: False; ShowMessage: nil);

      EmptyStats  :  TRunStats = (NFEfull: 0; NFEpartial: 0; Iters: 0);

      SignMinimize = 2 * Ord(IsMinimize) - 1;
      SignMaximize = -SignMinimize;

{-----------------------<< Scores >>------------------------------------------------}
type
      TScoreComparison = (scoreWorse, scoreBetter, scoreEqual);
      
// Return a comparison between two scores taking IsMinimize flag into account.
// LongInt arguments are converted into doubles without loss of information.
function CompareScores(
         Score1, Score2 :  Real
         )              :  TScoreComparison;
         overload;

function CompareScores(
   const Solution1,
         Solution2   :  TSolution
         )           :  TScoreComparison;
         overload;

// Normalized score difference between A and B: Result > 0 <=> A is better than B
function NormDeltaScore(
   const A, B  :  TSolution
         )     :  TScore;
         overload;

// Normalized score difference between A and B: Result > 0 <=> A is better than B
function NormDeltaScore(
         AScore,
         BScore   :  Real
         )        :  Real;
         overload;

{-----------------------<< Solutions >>---------------------------------------------}

// Swap the solutions A and B
procedure SwapSols(
   var   A, B     :  TSolution);

// Replace Solution by Trial if the latter is better,
// return whether the replacement took place
function ReplaceIfBetter(
   var   Solution    :  TSolution;
   const Trial       :  TSolution
         )           :  Boolean;

// Try updating the Best solution with a Trial one,
// display a message via ShowMessage if successful
function TryUpdateBest(
   var   Best        :  TSolution;
   const Trial       :  TSolution;
         ShowMessage :  ProcMessage
         )           :  Boolean;

// Try saving a Solution to the specified Path, return whether the attempt
// was successful. Errors are displayed via ShowMessage.
function TrySaveSolution(
   const Path        :  AnsiString;
   const Solution    :  TSolution;
         ShowMessage :  ProcMessage
         )           :  Boolean;

// Try loading a Solution from the specified Path, return whether the attempt 
// was successful. Errors are displayed via ShowMessage.
function TryLoadSolution(
   var   Solution    :  TSolution;
   const Path        :  AnsiString;
         ShowMessage :  ProcMessage
         )           :  Boolean;

// Return which solution (0 or 1) is closer to the Base solution. 
// Ties are broken at random.
function SimilarSolution(
   const Base, Sol0, Sol1  :  TSolution
         )                 :  Integer;

// Return the minimal distance from X to A and B
function DistToNearest(
   const X, A, B  :  TSolution
         )        :  Real;

// Create the Child by calling crossover with the order of parent arguments chosen
// at random. Recalc indicates whether the score recalculation is necessary.
procedure SymmetricCrossover(
   var   Child     :  TSolution;
   const Parent1,
         Parent2   :  TSolution;
         Recalc    :  Boolean);

{-----------------------<< Move lists >>--------------------------------------------}

// Initialize the MoveList, must be called before use
procedure InitMoveList(
   var   MoveList :  TMoveList);

// MLTo := MLFrom
procedure AssignMoveList(
   var   MLTo     :  TMoveList;
   var   MLFrom   :  TMoveList);

// Set the length of MoveList to the actual number of stored moves
procedure SetTrueLength(
   var   MoveList :  TMoveList);   

// Add a Move to the MoveList
procedure AddMove(
   var   MoveList :  TMoveList;
   const Move     :  TMove);

// Delete a move defined by its Index from the MoveList
procedure DelMove(
   var   MoveList :  TMoveList;
         Index    :  Integer);

{-----------------------<< Extended Move lists >>-----------------------------------}
type
      TExtMove =
         record
         Move     :  TMove;
         Score    :  TScore;
         Index    :  Integer;
         end;

      TExtMoveList =
         record
         N, Worst :  Integer;
         _        :  array of TExtMove;
         end;

// Initialize the MoveList with maximal length N, must be called before use
procedure InitExtMoveList(
   var   MoveList :  TExtMoveList;
         N        :  Integer);

// Try adding a Move with a given Score and Index to an extended move list. The move
// is added  if there is free space or it is better than the worst move in the list.
procedure TryAddExtMove(
   var   MoveList :  TExtMoveList;
   const Move     :  TMove;
         Score    :  TScore;
         Index    :  Integer);

{-----------------------<< Files >>-------------------------------------------------}

// Try opening a text file F located at Path for writing, return operation
// status. Display an error message Msg via ShowMessage on failure.
function TryOpenWrite(
   var   F           :  Text;
   const Path        :  AnsiString;
         ShowMessage :  ProcMessage;
         Msg         :  AnsiString  = ErrorOpenFile
         )           :  Boolean;

// Try opening a text file F located at Path for reading, return operation
// status. Display an error message Msg via ShowMessage on failure.
function TryOpenRead(
   var   F           :  Text;
   const Path        :  AnsiString;
         ShowMessage :  ProcMessage;
         Msg         :  AnsiString  = ErrorOpenFile
         )           :  Boolean;

implementation //////////////////////////////////////////////////////////////////////
uses
      InvSys,
      Math,    // Used: Power
      SpecFuncs,
      Arrays,
      Statistics,
      RandVars;

{-----------------------<< Scores >>------------------------------------------------}

procedure InvertScoreRelation(
   var   ScoreRel :  TScoreComparison);
   begin
   if      ScoreRel  = scoreWorse  then
           ScoreRel := scoreBetter
   else if ScoreRel  = scoreBetter then
           ScoreRel := scoreWorse;
   end;


// Return a comparison between two scores taking IsMinimize flag into account.
// LongInt arguments are converted into doubles without loss of information.
function CompareScores(
         Score1, Score2 :  Real
         )              :  TScoreComparison;
         overload;
   begin
   if      Score1 > Score2 then
      Result := scoreBetter
   else if Score1 < Score2 then
      Result := scoreWorse
   else
      Result := scoreEqual;
   if IsMinimize then
      InvertScoreRelation(Result);
   end;


function CompareScores(
   const Solution1,
         Solution2   :  TSolution
         )           :  TScoreComparison;
         overload;
   begin
   if      Solution1.Score > Solution2.Score then
      Result := scoreBetter
   else if Solution1.Score < Solution2.Score then
      Result := scoreWorse
   else
      Result := scoreEqual;
   if IsMinimize then
      InvertScoreRelation(Result);
   end;


// Normalized score difference between A and B: Result > 0 <=> A is better than B
function NormDeltaScore(
   const A, B  :  TSolution
         )     :  TScore;
         overload;
   begin
   if IsMinimize then
      Result := B.Score - A.Score
   else
      Result := A.Score - B.Score;
   end;


// Normalized score difference between A and B: Result > 0 <=> A is better than B
function NormDeltaScore(
         AScore,
         BScore   :  Real
         )        :  Real;
         overload;
   begin
   if IsMinimize then
      Result := BScore - AScore
   else
      Result := AScore - BScore;
   end;

{-----------------------<< Solutions >>---------------------------------------------}

// Swap the solutions A and B
procedure SwapSols(
   var   A, B     :  TSolution);
   var
         C        :  TSolution;
   begin
   AssignSolution(C, A);
   AssignSolution(A, B);
   AssignSolution(B, C);
   end;


// Replace Solution by Trial if the latter is better,
// return whether the replacement took place
function ReplaceIfBetter(
   var   Solution    :  TSolution;
   const Trial       :  TSolution
         )           :  Boolean;
   begin
   if CompareScores(Trial, Solution) = scoreBetter then
      begin
      AssignSolution(Solution, Trial);
      Result := Success;
      end
   else
      Result := Fail;
   end;


// Try updating the Best solution with a Trial one,
// display a message via ShowMessage if successful
function TryUpdateBest(
   var   Best        :  TSolution;
   const Trial       :  TSolution;
         ShowMessage :  ProcMessage
         )           :  Boolean;
   begin
   Result := ReplaceIfBetter(Best, Trial);
   if Result = Success then
      ShowNewBestScore(Best, ShowMessage);
   end;


// Try saving a Solution to the specified Path, return whether the attempt 
// was successful. Errors are displayed via ShowMessage.
function TrySaveSolution(
   const Path        :  AnsiString;
   const Solution    :  TSolution;
         ShowMessage :  ProcMessage
         )           :  Boolean;
   var
         fileSol     :  Text;
   const
         ErrorSave = 'Failed to save Solution ';
   begin
   Result := TryOpenWrite(
      fileSol, Path + '.' + FileExtension, ShowMessage, ErrorSave);
   if Result = Success then
      begin
      SaveSolution(fileSol, Solution);
      Close(fileSol);
      Result := Success;
      end;
   end;


// Try loading a Solution from the specified Path, return whether the attempt 
// was successful. Errors are displayed via ShowMessage.
function TryLoadSolution(
   var   Solution    :  TSolution;
   const Path        :  AnsiString;
         ShowMessage :  ProcMessage
         )           :  Boolean;
   var
         fileSol     :  Text;
   const
         ErrorLoad = 'Failed to load Solution ';
   begin
   Result := TryOpenRead(
      fileSol, Path + '.' + FileExtension, ShowMessage, ErrorLoad);
   if Result = Success then
      begin
      LoadSolution(Solution, fileSol);
      Close(fileSol);
      Result := Success;
      end;
   end;


// Return which solution (0 or 1) is closer to the Base solution. 
// Ties are broken at random.
function SimilarSolution(
   const Base, Sol0, Sol1  :  TSolution
         )                 :  Integer;
   var
         Dist0, Dist1      :  TSolutionDistance;
   begin
   Dist0 := Distance(Base, Sol0);
   Dist1 := Distance(Base, Sol1);
   if      Dist0 < Dist1 then
      Result := 0
   else if Dist0 > Dist1 then
      Result := 1
   else
      Result := Random(2);
   end;


// Return the minimal distance from X to A and B
function DistToNearest(
   const X, A, B  :  TSolution
         )        :  Real;
   begin
   Result := Min(Distance(X, A), Distance(X, B));
   end;


// Create the Child by calling crossover with the order of parent arguments chosen
// at random. Recalc indicates whether the score recalculation is necessary.
procedure SymmetricCrossover(
   var   Child     :  TSolution;
   const Parent1,
         Parent2   :  TSolution;
         Recalc    :  Boolean);
   begin
   if RandBool then
      Crossover(Child, Parent1, Parent2, Recalc) else
      Crossover(Child, Parent2, Parent1, Recalc);
   end;

{-----------------------<< Move lists >>--------------------------------------------}

// Initialize the MoveList, must be called before use
procedure InitMoveList(
   var   MoveList :  TMoveList);
   begin
   with MoveList do
      begin
      N := 0;
      SetLength(Moves, 0);
      end;
   end;


// MLTo := MLFrom
procedure AssignMoveList(
   var   MLTo     :  TMoveList;
   var   MLFrom   :  TMoveList);
   begin
   MLTo.N     :=      MLFrom.N;
   MLTo.Moves := Copy(MLFrom.Moves, 0, MLFrom.N);
   end;


// Set the length of MoveList to the actual number of stored moves
procedure SetTrueLength(
   var   MoveList :  TMoveList);
   begin
   with MoveList do
      SetLength(Moves, N);
   end;


{.$RANGECHECKS OFF} {.$OVERFLOWCHECKS OFF}
// Add a Move to the MoveList
procedure AddMove(
   var   MoveList :  TMoveList;
   const Move     :  TMove);
   begin
   with MoveList do
      begin
      Inc(N);
      if N > Length(Moves) then
         SetLength(Moves, 2 * N);
      Moves[N - 1] := Move;
      end;
   end;
{.$RANGECHECKS ON} {.$OVERFLOWCHECKS ON}


// Delete a move defined by its Index from the MoveList
procedure DelMove(
   var   MoveList :  TMoveList;
         Index    :  Integer);
   begin
   with MoveList do
      begin
      Dec(N);
      Moves[Index] := Moves[N];
      end;
   end;

{-----------------------<< Extended Move lists >>-----------------------------------}

// Initialize the MoveList with maximal length N, must be called before use
procedure InitExtMoveList(
   var   MoveList :  TExtMoveList;
         N        :  Integer);
   begin
   MoveList.N := 0;
   SetLength(MoveList._, N);
   end;


// Try adding a Move with a given Score and Index to an extended move list. The move
// is added  if there is free space or it is better than the worst move in the list.
procedure TryAddExtMove(
   var   MoveList :  TExtMoveList;
   const Move     :  TMove;
         Score    :  TScore;
         Index    :  Integer);
   var
         i        :  Integer;
         Order    :  TIntArray;
   begin
   with MoveList do
      if N < Length(MoveList._) then
         begin
         // Add move if there is free space
         MoveList._[N].Move  := Move;
         MoveList._[N].Score := Score;
         MoveList._[N].Index := Index;
         if (N = 0) or (CompareScores(Score, MoveList._[Worst].Score) = scoreWorse) then
            Worst := N;
         Inc(N);
         end
      else if CompareScores(Score, MoveList._[Worst].Score) = scoreBetter then
         begin
         // Replace worst move
         MoveList._[Worst].Move  := Move;
         MoveList._[Worst].Score := Score;
         MoveList._[Worst].Index := Index;

         // Find new worst move
         RandPerm(Order, N, {Base:} 0);
         for i := 0 to N - 1 do
            if CompareScores(
               MoveList._[Order[i]].Score,
               MoveList._[   Worst].Score) = scoreWorse then
               Worst := Order[i];
         end;
   end;

{-----------------------<< Files >>-------------------------------------------------}

// Try opening a text file F located at Path for writing, return operation
// status. Display an error message Msg via ShowMessage on failure.
function TryOpenWrite(
   var   F           :  Text;
   const Path        :  AnsiString;
         ShowMessage :  ProcMessage;
         Msg         :  AnsiString  = ErrorOpenFile
         )           :  Boolean;
   begin
   Result := OpenWrite(F, Path);
   if Result = Fail then
      ShowError(Msg + Path, ShowMessage);
   end;


// Try opening a text file F located at Path for reading, return operation
// status. Display an error message Msg via ShowMessage on failure.
function TryOpenRead(
   var   F           :  Text;
   const Path        :  AnsiString;
         ShowMessage :  ProcMessage;
         Msg         :  AnsiString  = ErrorOpenFile
         )           :  Boolean;
   begin
   Result := OpenRead(F, Path);
   if Result = Fail then
      ShowError(Msg + Path, ShowMessage);
   end;

end.
