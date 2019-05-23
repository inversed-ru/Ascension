{ 
Copyright (c) Peter Karpov 2010 - 2019.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit Problem; ///////////////////////////////////////////////////////////////////////
{
>> Version: 0.2

>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> Description
   This unit implements three related problems:
    - Queen domination (Knight covering): place the minimal number of queens 
      (knights) on N x N board so that every vacant square is attacked or occupied 
      (A075458, A006075).
    - Nondominating queens: place N queens on N x N board so as to maximize the
      number of unattacked vacant squares (A001366).
   The score is the number of unattacked vacant squares.
   
   Problem-specific constants:
      Size           Board size
      Knights        True for knight covering, False for problems involving queens
      NPieces        Number of pieces on the board
      Minimization   True for queen domination and knight covering, False for
                     nondominating queens
   
   Supported algorithms: LS, TS, SA, GA.

>> Notes
   Best known values for the nondominating queens problem:
          1   2   3   4   5   6   7   8   9  10 
    0 |   0   0   0   1   3   5   7  11  18  22 
   10 |  30  36  47  56  72  82  97 111 132 145 
   20 | 170 186 216 240 260 290 324 360 381 420

   Values for n <= 16 are known to be optimal. During a search with SA, BSA, GA2, 
   all of the above values were reached, but no improvements were found.

   Knight covering problem turned out to be very difficult. Reached known results 
   for n <= 23, but again, found no improvements.

   Known values and bounds for the queen domination problem:
         0 1 2 3 4 5 6 7 8 9
     0 |   = = - = = = = + =
    10 | = - = = + + + = = =
    20 | ~ = ~ = ~ = ~ = ~ =
    30 | = = ~ = ~ ~ ~ = ~ =
    40 | ~ = ~ ~ ~ = ~ ~ ~ =
    50 | ~ ~ ~ = ~ ~ ~ = ~ ~
    60 | ~ = ~ ~ ~ = ~ ~ ~ =
    70 | ~ = ~ = ~ ~ ~ = ~ ~
    80 | ~ = ~ ~ ~ = ~ ~ ~ =
    90 | ~ = ~ = ~ ~ ~ = ~ ~
   100 | ~ = ~ ~ ~ = ~ ~ ~ =
   110 | ~ ~ ~ = ~ = ~ = ~ ~
   120 | ~ = ~     = ~     =
   130 | = = ~              

   Notation:
   -  A(n) = Ceil(n / 2) - 1
   =  A(n) = Ceil(n / 2)
   +  A(n) = Ceil(n / 2) + 1
   ~  A(n) is either Ceil(n / 2) or Ceil(n / 2) + 1
   
   Failed to reach any of the lower bounds for n <= 35 marked with ~.

>> References
    - Values of Domination Numbers of the Queen's Graph.
      Patric R. J. Ostergard, William D. Weakley.

>> Changelog
   0.2 : 2019.05.20  - Ascension 2.0 compatibility
                     - Cleanup
   0.1 : 2016.02.28  - Added Knights option for knight domination
   0.0 : 2012.04.15  - Initial version

>> ToDo
    ? Implement independent domination (see the reference, A075324)
}
interface ///////////////////////////////////////////////////////////////////////////
uses
      InvSys;
const
      Minimization   =  True;
      ScoreToReach   =  0;
      FileExtension  =  'txt';
      Dim            =  2;
      Size           =  16;
      NPieces        =  9;
      Knights        =  False;
      Margin         =  2;
type
      // Basic types
      TScore = Integer;

      TDim = 0 .. Dim - 1;
      TCoord = 0 .. Size - 1;
      TCoords = array [TDim] of TCoord;

      TPoints = array [0 .. NPieces - 1] of TCoords;

      TCover = array [TCoord, TCoord] of Integer;

      TSolution =
         record
         Cover    :  TCover;
         Pieces   :  TPoints;
         Attacked,
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
type
      TExtendCoord   =  0 - Margin .. Size - 1 + Margin;
const
      NKnightMoves   =  9;
      KnightMoves    :  array [0 .. NKnightMoves - 1, TDim] of Integer =
                         (( 1,  2), ( 2,  1), ( 1, -2), ( 2, -1),
                          (-1, -2), (-2, -1), (-1,  2), (-2,  1), (0, 0));
var
      InsideBoard    :  array [TExtendCoord, TExtendCoord] of Boolean;

{-----------------------<< Problem-specific operations >>---------------------------}

// Return whether p1 = p2
function EqualPoints(
   const p1, p2   :  TCoords
         )        :  Boolean;
   begin
   Result := (p1[0] = p2[0]) and (p1[1] = p2[1]);
   end;


// Return whether Point is already present in Solution among points [0 .. MaxIndex]
function PointPresent(
   const Point    :  TCoords;
   const Solution :  TSolution;
         MaxIndex :  Integer     =  NPieces - 1
         )        :  Boolean;
   var
         i        :  Integer;
   begin
   Result := False;
   for i := 0 to MaxIndex do
      if EqualPoints(Point, Solution.Pieces[i]) then
         begin
         Result := True;
   {<}   break;
         end;
   end;


// Return a random point
function RandPoint   :  TCoords;
   begin
   Result[0] := Random(Size);
   Result[1] := Random(Size);
   end;


// Return a random point that is different from Solution's points [0 .. MaxIndex]
function UniqueRandPoint(
   const Solution :  TSolution;
         MaxIndex :  Integer
         )        :  TCoords;
   begin
   repeat
      Result := RandPoint;
   until not PointPresent(Result, Solution, MaxIndex);
   end;


// Clear Solution's coverage
procedure ClearCover(
   var   Solution :  TSolution);
   var
         i, j     :  Integer;
   begin
   Solution.Attacked := 0;
   for i := 0 to Size - 1 do
      for j := 0 to Size - 1 do
         Solution.Cover[i, j] := 0;
   end;


{$RANGECHECKS OFF} {$OVERFLOWCHECKS OFF}
// Modify Solution's coverage at a given position by Delta
procedure ModifyPointCover(
   var   Solution    :  TSolution;
         x, y        :  Integer;
         Delta       :  Integer);
   var
         OldC, NewC  :  Integer;
   begin
   with Solution do
      begin
      OldC := Ord(Cover[x, y] <> 0);
      Inc(Cover[x, y], Delta);
      NewC := Ord(Cover[x, y] <> 0);
      Inc(Attacked, NewC - OldC);
      end;
   end;
{$RANGECHECKS ON} {$OVERFLOWCHECKS ON}


// Modify Solution's coverage at queen or knight's reach from Point by Delta
procedure ModifyPieceCover(
   var   Solution    :  TSolution;
         x, y        :  Integer;
         Delta       :  Integer);
   var
         t, nx, ny   :  Integer;
   const
         MaxC        =  Size - 1;
   begin
   if Knights then
      for t := 0 to NKnightMoves - 1 do
         begin
         nx := x + KnightMoves[t, 0];
         ny := y + KnightMoves[t, 1];
         if InsideBoard[nx, ny] then
            ModifyPointCover(Solution, nx, ny, Delta);
         end
   else
      begin
      // Horizontal and vertical directions
      for t := 0 to Size - 1 do
         begin
         ModifyPointCover(Solution, x, t, Delta);
         ModifyPointCover(Solution, t, y, Delta);
         end;

      // Main diagonal
      for t := -Min(x, y) to MaxC - Max(x, y) do
         ModifyPointCover(Solution, x + t, y + t, Delta);

      // Second diagonal
      for t := -Min(MaxC - x, y) to Min(x, MaxC - y) do
         ModifyPointCover(Solution, x - t, y + t, Delta);
      end;

   // Set score
   Solution.Score := Sqr(Size) - Solution.Attacked;
   end;


// Modify Solution's coverage after piece addition
procedure AddPiece(
   var   Solution :  TSolution;
   const Point    :  TCoords);
         overload;
   begin
   ModifyPieceCover(Solution, Point[0], Point[1], {Delta:} 1);
   end;


// Modify Solution's coverage after piece addition
procedure AddPiece(
   var   Solution :  TSolution;
         Index    :  Integer;
   const Point    :  TCoords);
         overload;
   begin
   Solution.Pieces[Index] := Point;
   ModifyPieceCover(Solution, Point[0], Point[1], {Delta:} 1);
   end;


// Modify Solution's coverage after piece deletion
procedure DelPiece(
   var   Solution :  TSolution;
   const Point    :  TCoords);
   begin
   ModifyPieceCover(Solution, Point[0], Point[1], {Delta:} -1);
   end;
   
{-----------------------<< Scores >>------------------------------------------------}

// Set Solution's score
procedure SetScore(
   var   Solution :  TSolution);
   var
         i        :  Integer;
   begin
   ClearCover(Solution);
   for i := 0 to NPieces - 1 do with Solution do
      AddPiece(Solution, Pieces[i]);
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
         c           :  TCoords;
   const
         PieceName   :  array [Boolean] of AnsiChar = ('Q', 'N');
   begin
   WriteLn(FileSol, 'Unattacked: ', Solution.Score);
   WriteLn(FileSol, '  Attacked: ', Solution.Attacked);
   WriteLn(FileSol);

   // Write coverage
   for i := 0 to Size - 1 do
      begin
      for j := 0 to Size - 1 do
         begin
         c[0] := i;
         c[1] := j;
         if PointPresent(c, Solution) then
            Write(FileSol, PieceName[Knights])
         else if Solution.Cover[i, j] = 0 then
            Write(FileSol, ' ')
         else
            Write(FileSol, '.');
         Write(FileSol, ' ');
         end;
      WriteLn(FileSol);
      end;
   WriteLn(FileSol);

   // Write coordinates
   for i := 0 to NPieces - 1 do
      begin
      for k := 0 to Dim - 1 do
         Write(fileSol, Solution.Pieces[i, k], Tab);
      WriteLn(fileSol);
      end;
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
         i        :  Integer;
   begin
   for i := 0 to NPieces - 1 do
      Solution.Pieces[i] := UniqueRandPoint(Solution, i - 1);
   SetScore(Solution);
   end;


// Return the distance between Solution1 and Solution2
function Distance(
   const Solution1,
         Solution2   :  TSolution
         )           :  TSolutionDistance;
   var
         i, j        :  Integer;
         Sum         :  TSolutionDistance;
   begin
   Sum := NPieces;
   for i := 0 to NPieces - 1 do
      for j := 0 to NPieces - 1 do
         if EqualPoints(Solution1.Pieces[i], Solution2.Pieces[j]) then
            begin
            Dec(Sum);
      {<}   break;
            end;
   Result := Sum;
   end;


// Problem-specific improvement
procedure SpecialImprove(
   var   Solution :  TSolution);
   begin
   Assert(False);
   end;

{-----------------------<< Moves >>-------------------------------------------------}

// Make MoveList of a Solution
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
               if not PointPresent(Move.NewC, Solution) then
                  AddMove(MoveList, Move);
               end;
            end;
         end;
   end;


// Apply a Move to the Solution
procedure PerformMove(
   var   Solution :  TSolution;
   const Move     :  TMove);
         overload;
   begin
   if not PointPresent(Move.NewC, Solution) then
      with Solution, Move do
         begin
         DelPiece(Solution, Pieces[i]);
         AddPiece(Solution, i, NewC);
         end;
   end;


// Apply a Move to the Solution, save Undo
procedure PerformMove(
   var   Solution :  TSolution;
   var   Undo     :  TMoveUndo;
   const Move     :  TMove);
         overload;
   begin
   Undo.i := Move.i;
   Undo.NewC := Solution.Pieces[Move.i];
   PerformMove(Solution, Move);
   end;


// Make a random Move from the Solution
procedure GetRandomMove(
   var   Move     :  TMove;
   const Solution :  TSolution);
   begin
   Move.i := Random(NPieces);
   Move.NewC := UniqueRandPoint(Solution, NPieces - 1);
   end;


// Undo the last move applied to the Solution
procedure UndoMove(
   var   Solution :  TSolution;
   const Undo     :  TMoveUndo);
   begin
   PerformMove(Solution, Undo);
   end;


// Make a MoveList of the Solution for use in Local Search. Level determines
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

// Apply a random move to the Solution, save Undo
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


// Undo the last move applied to the Solution
procedure UndoSAMove(
   var   Solution :  TSolution;
   const Undo     :  TSAUndo);
   begin
   UndoMove(Solution, Undo);
   end;

{-----------------------<< Genetic Algorithm >>-------------------------------------}

// Apply mutation operator to the Solution
procedure Mutate(
   var   Solution :  TSolution);
   var
         Move     :  TMove;
   begin
   GetRandomMove(Move, Solution);
   PerformMove(Solution, Move);
   end;


// Apply crossover operator to Parent1 and Parent2, creating the Child. Recalc
// indicates whether score recalculation is strictly necessary.
// Constructive crossover.
procedure Crossover(
   var   Child    :  TSolution;
   const Parent1,
         Parent2  :  TSolution;
         Recalc   :  Boolean);
   var
         i, j     :  Integer;
         Trials   :  TPoints;
         Used     :  array [0 .. 1, 0 .. NPieces - 1] of Boolean;
         Which    :  Integer;
         Cost     :  TRealArray;
         FailCost :  Integer;
   begin
   // Initialize
   Child.Score := 0;
   ClearCover(Child);
   SetLength(Cost, NPieces);
   for j := 0 to 1 do
      for i := 0 to NPieces - 1 do
         Used[j, i] := False;
   if Minimization then
      FailCost := Sqr(Size) else
      FailCost := -1;

   // Construct the child from parent elements
   for i := 0 to NPieces - 1 do
      begin
      // Select points from alternating parents
      Which := i mod 2;
      if Which = 0 then
         Trials := Parent1.Pieces else
         Trials := Parent2.Pieces;

      // Calculate the cost of adding each point
      for j := 0 to NPieces - 1 do
         if Used[Which, j] or PointPresent(Trials[j], Child, i - 1) then
            Cost[j] := FailCost
         else
            begin
            AddPiece(Child, i, Trials[j]);
            Cost[j] := Child.Score;
            DelPiece(Child,    Trials[j]);
            end;

      // Add a point that introduces the least conflicts
      if Minimization then
         j := RandMinIndex(Cost) else
         j := RandMaxIndex(Cost);
      Assert(Cost[j] <> FailCost);
      AddPiece(Child, i, Trials[j]);
      Used[Which, j] := True;
      end;
   end;

{-----------------------<< Tabu Search >>-------------------------------------------}

// ListTo := ListFrom
procedure AssignTabuList(
   var   ListTo   :  TTabuList;
   const ListFrom :  TTabuList);
   begin
   ListTo := ListFrom;
   end;
   

// Initialize the TabuList
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


// Add a Move that will be applied to Sol to the TabuList with Tenure.
procedure AddToTabuList(
   var   TabuList :  TTabuList;
   const Move     :  TMove;
         Tenure   :  Integer;
   const Sol      :  TSolution);
   begin
   with Sol, Move do
      TabuList[ Pieces[i, 0], Pieces[i, 1] ] := Tenure;
   end;


// Make a MoveList for the Solution for use in Tabu Search
procedure MakeTSMoveList(
   var   MoveList :  TMoveList;
   const Solution  :  TSolution);
   begin
   MakeMoveList(MoveList, Solution);
   end;


// Return whether a Move is in the TabuList of the Solution
function IsMoveTabu(
   const Move     :  TMove;
   const Solution :  TSolution;
   const TabuList :  TTabuList
         )        :  Boolean;
   begin
   Result := TabuList[ Move.NewC[0], Move.NewC[1] ] <> 0;
   end;


// Return a tabu tenure. t is normalized time: t = Iter / MaxIters.
function TabuTenure(
         t  :  Real
         )  :  Integer;
   begin
   Result := Round( (1 + Random) * ( 2 + Size * (1 - t) ) );
   end;

{-----------------------<< Initialization >>----------------------------------------}
var
      i, j     :  Integer;
initialization
for i := 0 - Margin to Size - 1 + Margin do
   for j := 0 - Margin to Size - 1 + Margin do
      InsideBoard[i, j] := InRange(i, 0, Size - 1) and InRange(j, 0, Size - 1);
end.
