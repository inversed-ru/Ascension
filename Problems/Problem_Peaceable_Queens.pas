{ 
Copyright (c) Peter Karpov 2010 - 2019.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit Problem; ///////////////////////////////////////////////////////////////////////
{
>> Version: 0.1

>> Description
   Peaceable Queens problem PDM: find the maximal A(n) such that A(n) white and A(n)
   black queens can be placed on a n x n chessboard without queens of opposite colors
   attacking each other. Solution score is the number of such attacks.
   
   Problem-specific constants:
      Size           Board size
      NPieces        Total number of queens
   
   Supported algorithms: LS, TS, SA.

>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> Notes
   Conjecture: A(n) = Floor(7 n^2 / 48) except for n = 5, 9.
   Known lower bounds reached for N up to 20. Best known solutions:
         0   1   2   3   4   5   6   7   8   9
    0 |      0   0   1   2   4   5   7   9  12
   10 | 14  17  21  24  28  32  37  42  47  52 
   20 | 58  64  70  77  84
   
>> References
   https://oeis.org/A250000
   
>> ToDo
   - Implement crossover

>> Changelog
   0.1 : 2019.05.20  - Ascension 2.0 compatibility
                     - Renamed IsMinimize to Minimization
   0.0 : 2016.03.08  - Initial version
}
interface ///////////////////////////////////////////////////////////////////////////
uses
      InvSys;
const
      Minimization   =  True;
      ScoreToReach   =  0;
      FileExtension  =  'txt';
      Dim            =  2;
      Size           =  24;
      NPieces        =  2 * 84;
      Margin         =  2;
type
      // Basic types
      TScore = Integer;

      TDim = 0 .. Dim - 1;
      TCoord = 0 .. Size - 1;
      TCoords = array [TDim] of TCoord;

      TPoints = array [0 .. NPieces - 1] of TCoords;

      TColor = 0 .. 1;

      TCover = array [TCoord, TCoord, TColor] of Integer;

      TSolution =
         record
         Cover    :  TCover;
         Queens   :  TPoints;
         Board    :  array [TCoord, TCoord] of Integer;
         Score    :  TScore;
         end;

      TSolutionDistance = Integer;

      // Local search
      TMove =
         record
         i     :  Integer;
         NewC  :  TCoords;
         end;

      TMoveUndo = TMove;
      TSAUndo   = TMoveUndo;

      // Tabu search
      TTabuList = array [TCoord, TCoord] of Integer;

      // Guided local search
      TGuidePenalty = array [TCoord, TCoord] of Real;

      // Partitions
      TRegion = Nothing;
      TRegionSize = Integer;
      
      // Estimation of distribution
      TDistribution = Nothing;

{$I Interface.inc}
implementation //////////////////////////////////////////////////////////////////////
uses
      Math,             
      ExtraMath,
      Arrays,
      Statistics,
      RandVars,
      Common,
      Messages;
const
      Empty          =  -1;
type
      TExtendCoord   =  0 - Margin .. Size - 1 + Margin;
var
      InsideBoard    :  array [TExtendCoord, TExtendCoord] of Boolean;

{-----------------------<< Problem-specific operations >>---------------------------}

// Return a random board location
function RandPoint   :  TCoords;
   begin
   Result[0] := Random(Size);
   Result[1] := Random(Size);
   end;


// Return a random empty board location 
function RandEmptyPoint(
   const Solution :  TSolution
         )        :  TCoords;
   begin
   repeat
      Result := RandPoint;
   until Solution.Board[Result[0], Result[1]] = Empty;
   end;


// Clear Solution's coverage
procedure ClearCover(
   var   Solution :  TSolution);
   var
         i, j     :  Integer;
         c        :  TColor;
   begin
   for i := 0 to Size - 1 do
      for j := 0 to Size - 1 do
         for c := 0 to 1 do
            Solution.Cover[i, j, c] := 0;
   end;


// Clear Solution's Board
procedure ClearBoard(
   var   Solution :  TSolution);
   var
         i, j     :  Integer;
   begin
   for i := 0 to Size - 1 do
      for j := 0 to Size - 1 do
            Solution.Board[i, j] := Empty;
   end;


{$RANGECHECKS OFF} {$OVERFLOWCHECKS OFF}
// #HOTSPOT N2
// Return the subscore of a square (x, y)
function SubScore(
   const Solution    :  TSolution;
         x, y        :  Integer
         )           :  Integer;
   begin
   with Solution do
      if Board[x, y] = Empty then
         Result := 0 else
         Result := Cover[x, y, 1 - Board[x, y] mod 2];
   end;


// #HOTSPOT N1
// Modify Solution's coverage at a given position by Delta
procedure ModifyPointCover(
   var   Solution    :  TSolution;
         x, y        :  Integer;
         c           :  TColor;
         Delta       :  Integer);
   var
         OldS, NewS  :  Integer;
   begin
   with Solution do
      begin
      OldS := SubScore(Solution, x, y);
      Inc(Cover[x, y, c], Delta);
      NewS := SubScore(Solution, x, y);
      Inc(Score, NewS - OldS);
      end;
   end;


// Modify Solution's coverage at queen's reach from Point by Delta
procedure ModifyQueenCover(
   var   Solution    :  TSolution;
         x, y        :  Integer;
         c           :  TColor;
         Delta       :  Integer);
   var
         t           :  Integer;
   const
         MaxC        =  Size - 1;
   begin
   // Horizontal and vertical directions
   for t := 0 to Size - 1 do
      begin
      ModifyPointCover(Solution, x, t, c, Delta);
      ModifyPointCover(Solution, t, y, c, Delta);
      end;

   // Main diagonal
   for t := -Min(x, y) to MaxC - Max(x, y) do
      ModifyPointCover(Solution, x + t, y + t, c, Delta);

   // Second diagonal
   for t := -Min(MaxC - x, y) to Min(x, MaxC - y) do
      ModifyPointCover(Solution, x - t, y + t, c, Delta);
   end;
{$RANGECHECKS ON} {$OVERFLOWCHECKS ON}


// Modify Solution's board and coverage after queen addition
procedure AddQueen(
   var   Solution    :  TSolution;
         Index       :  Integer;
   const Point       :  TCoords);
   begin
   Solution.Queens[Index] := Point;
   Solution.Board[Point[0], Point[1]] := Index;
   ModifyQueenCover(Solution, Point[0], Point[1], Index mod 2, {Delta:} 1);
   end;


// Modify Solution's board and coverage after queen deletion
procedure DelQueen(
   var   Solution    :  TSolution;
         Index       :  Integer;
   const Point       :  TCoords);
   begin
   Solution.Board[Point[0], Point[1]] := Empty;
   ModifyQueenCover(Solution, Point[0], Point[1], Index mod 2, {Delta:} -1);
   end;
   
{-----------------------<< Scores >>------------------------------------------------}

// Set Solution's score
procedure SetScore(
   var   Solution :  TSolution);
   var
         i        :  Integer;
   begin
   ClearCover(Solution);
   ClearBoard(Solution);
   for i := 0 to NPieces - 1 do with Solution do
      AddQueen(Solution, i, Queens[i]);
   end;


// Return a string representation of Score
function FormatScore(
         Score :  TScore
         )     :  AnsiString;
   begin
   Str(Score, Result);
   end;

{-----------------------<< Solution operations >>-----------------------------------}

// Assign SolFrom to SolTo
procedure AssignSolution(
   var   SolTo    :  TSolution;
   const SolFrom  :  TSolution);
   begin
   SolTo := SolFrom;
   end;


// Save Solution to FileSol
procedure SaveSolution(
   var   FileSol     :  Text;
   const Solution    :  TSolution);
   var
         i, j, k     :  Integer;
   const
         PieceName   :  array [0 .. 1, TColor] of AnsiChar = (('w', 'q'), ('W', 'Q'));
         CoverName   :  array [0 .. 1] of AnsiChar = (' ', '+');
         MaxValue    =  9;
   begin
   WriteLn(FileSol, 'Score: ', Solution.Score);
   WriteLn(FileSol);

   // Text mode
   for i := 0 to Size - 1 do
      begin
      for j := 0 to Size - 1 do
         with Solution do
            begin
            if Board[i, j] <> Empty then
               Write(FileSol, 'BW'[1 + Board[i, j] mod 2] + ' ') else
               Write(FileSol, '. ');
            end;
      WriteLn(FileSol);
      end;
   WriteLn(FileSol);

   // Mathematica array
   WriteLn(FileSol, '{');
   for i := 0 to Size - 1 do
      begin
      Write(FileSol, '{');
      for j := 0 to Size - 1 do
         with Solution do
            begin
            if Board[i, j] <> Empty then
               Write(FileSol, MaxValue * (Board[i, j] mod 2))  else
               Write(FileSol, MaxValue div 2 + (i + j) mod 2);
            if j < (Size - 1) then
               Write(FileSol, ', ');
            end;
      if i < (Size - 1) then
         WriteLn(FileSol, '}, ') else
         WriteLn(FileSol, '}')
      end;
   WriteLn(FileSol, '}');
   WriteLn(FileSol);

   // Coordinates
   for i := 0 to NPieces - 1 do
      begin
      for k := 0 to Dim - 1 do
         Write(fileSol, Solution.Queens[i, k], Tab);
      WriteLn(fileSol);
      end;
   WriteLn(fileSol);

   // Mathematica coordinates
   WriteLn(FileSol, '{');
   with Solution do
      for i := 0 to NPieces - 1 do
         begin
         Write(FileSol, '{', Queens[i, 0], ',', Queens[i, 1], '}');
         if i < (NPieces - 1) then
            WriteLn(FileSol, ',') else
            WriteLn(FileSol);
         end;
   WriteLn(FileSol, '}');
   WriteLn(FileSol);
   end;


// Load Solution from FileSol
procedure LoadSolution(
   var   Solution    :  TSolution;
   var   FileSol     :  Text);
   begin
   Assert(False);
   end;
   
   
// Create a new random Solution
procedure NewSolution(
   var   Solution :  TSolution);
   var
         i, j     :  Integer;
   begin
   with Solution do
      begin
      // Initialize the board
      ClearBoard(Solution);
      ClearCover(Solution);

      // Add the queens
      Score := 0;
      for i := 0 to NPieces - 1 do
         AddQueen(Solution, i, RandEmptyPoint(Solution));
      end;
   end;


// Return the distance between SolutionA and SolutionB
function Distance(
   const Solution1,
         Solution2   :  TSolution
         )           :  TSolutionDistance;
   var
         i, j        :  Integer;
         Sum         :  TSolutionDistance;
   begin
   Sum := 0;
   for i := 0 to Size - 1 do
      for j := 0 to Size - 1 do
         if Solution1.Board[i, j] <> Solution2.Board[i, j] then
            Inc(Sum);
   Result := Sum;
   end;


// Problem-specific improvement
procedure SpecialImprove(
   var   Solution :  TSolution);
   begin
   Assert(False);
   end;

{-----------------------<< Moves >>-------------------------------------------------}

// Make MoveList of Solution
procedure MakeMoveList(
   var   MoveList :  TMoveList;
   const Solution :  TSolution);
   var
         i, j, k  :  Integer;
         Move     :  TMove;
   begin
   InitMoveList(MoveList);
   with Solution do
      for k := 0 to NPieces - 1 do
         begin
         Move.i := k;
         for i := 0 to Size - 1 do
            begin
            Move.NewC[0] := i;
            for j := 0 to Size - 1 do
               begin
               Move.NewC[1] := j;
               if Solution.Board[i, j] = Empty then
                  AddMove(MoveList, Move);
               end;
            end;
         end;
   end;


// Apply Move to Solution
procedure PerformMove(
   var   Solution :  TSolution;
   const Move     :  TMove);
         overload;
   begin
   with Solution, Move do
      begin
      DelQueen(Solution, i, Queens[i]);
      AddQueen(Solution, i, NewC);
      end;
   end;


// Apply Move to Solution, save Undo
procedure PerformMove(
   var   Solution :  TSolution;
   var   Undo     :  TMoveUndo;
   const Move     :  TMove);
         overload;
   begin
   Undo.i := Move.i;
   Undo.NewC := Solution.Queens[Move.i];
   PerformMove(Solution, Move);
   end;


// Make a random Move from Solution
procedure GetRandomMove(
   var   Move     :  TMove;
   const Solution :  TSolution);
   begin
   Move.i := Random(NPieces);
   Move.NewC := RandEmptyPoint(Solution);
   end;


// Undo the last move applied to Solution
procedure UndoMove(
   var   Solution :  TSolution;
   const Undo     :  TMoveUndo);
   begin
   PerformMove(Solution, Undo);
   end;


// Make a MoveList of Solution for use in Local Search. Level determines
// neighborhood size, with 1 being the smallest.
procedure MakeLSMoveList(
   var   MoveList :  TMoveList;
   const Solution :  TSolution;
         Level    :  Integer);
   begin
   InitMoveList(MoveList);
   if Level = 1 then
      MakeMoveList(MoveList, Solution);
   end;

{-----------------------<< Simulated Annealing >>-----------------------------------}

// Apply a random move to Solution, save Undo
procedure MakeNeighbour(
   var   Solution :  TSolution;
   var   Undo     :  TSAUndo;
         T        :  Real);
   var
         Move     :  TMove;
   begin
   GetRandomMove(Move, Solution);
   PerformMove(Solution, Undo, Move);
   end;


// Undo the last move applied to Solution
procedure UndoSAMove(
   var   Solution :  TSolution;
   const Undo     :  TSAUndo);
   begin
   UndoMove(Solution, Undo);
   end;


// Improvement procedure for use in Simulated Annealing
procedure ImprovementSA(
   var   Solution     :  TSolution);
   begin
   end;

{-----------------------<< Tabu Search >>-------------------------------------------}

// ListTo := ListFrom
procedure AssignTabuList(
   var   ListTo   :  TTabuList;
   const ListFrom :  TTabuList);
   begin
   ListTo := ListFrom;
   end;


// Initialize TabuList. Must be called before using the TabuList.
procedure InitTabuList(
   var   TabuList :  TTabuList);
   var
         i, j     :  TCoord;
   begin
   for i := 0 to Size - 1 do
      for j := 0 to Size - 1 do
         Tabulist[i, j] := 0;
   end;


// Perform aging of the TabuList
procedure AgeTabuList(
   var   TabuList :  TTabuList);
   var
         i, j     :  TCoord;
   begin
   for i := 0 to Size - 1 do
      for j := 0 to Size - 1 do
         if Tabulist[i, j] > 0 then
            Dec(Tabulist[i, j]);
   end;


// Add Move that will be applied to Sol to the TabuList with Tenure.
procedure AddToTabuList(
   var   TabuList :  TTabuList;
   const Move     :  TMove;
         Tenure   :  Integer;
   const Sol      :  TSolution);
   begin
   with Sol, Move do
      TabuList[ Queens[i, 0], Queens[i, 1] ] := Tenure;
   end;


// Make a MoveList for Solution for use in Tabu Search
procedure MakeTSMoveList(
   var   MoveList :  TMoveList;
   const Solution  :  TSolution);
   begin
   MakeMoveList(MoveList, Solution);
   end;


// Return whether a Move is in TabuList for Solution
function IsMoveTabu(
   const Move     :  TMove;
   const Solution :  TSolution;
   const TabuList :  TTabuList
         )        :  Boolean;
   begin
   Result := TabuList[ Move.NewC[0], Move.NewC[1] ] <> 0;
   end;


// Return a tabu tenure. t is the normalized time: t = Iter / MaxIters.
function TabuTenure(
         t  :  Real
         )  :  Integer;
   begin
   Result := Round( (1 + Random) * ( 2 + Size * (1 - t) ) );
   end;

{-----------------------<< Initialization >>----------------------------------------}
{$I DummyGA.inc}

var
      i, j     :  Integer;
initialization
for i := 0 - Margin to Size - 1 + Margin do
   for j := 0 - Margin to Size - 1 + Margin do
      InsideBoard[i, j] := InRange(i, 0, Size - 1) and InRange(j, 0, Size - 1);
end.
