{ 
Copyright (c) Peter Karpov 2010 - 2019.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit Problem; ///////////////////////////////////////////////////////////////////////
{
>> Version 0.4

>> Description
   Maximum density subsquare-free arrangements problem: construct an N x N binary 
   array with no subsquare having 1 in all four corners. The score is the total 
   number of 1s minus the penalty for forbidden subsquares.
   
   Problem-specific constants:
      Size           Array size
      AxisParallel   Defines whether all or only axis-parallel subsquares are 
                     considered
      FixedNPoints   If set to True, the simulated annealing moves preserve the total
                     number of 1s. This formulation is more efficient.
      NPoints        The total number of 1s when FixedNPoints is true
      SquarePenalty  The penalty for each forbidden square
   
   Supported algorithms: LS, TS, SA, GA.
   
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru
   
>> ToDo
   - No subtriangles
   ? Periodic version

>> References
   https://oeis.org/A227133
   https://oeis.org/A227116

>> Changelog
   0.4 : 2019.05.20 ~ Ascension 2.0 compatibility
                    ~ Cleanup
   0.3 : 2016.05.28 - 3D version
                    + Free orientation version (AxisParallel flag)
                    ~ The set of all squares is now precalculated
                    ~ Optimized by removing unnecessary square checks
                    + Fixed number of points mode (FixedNPoints flag)
   0.2 : 2016.03.03 + 3D version
   0.1 : 2016.02.22 + Two-point move
                    - ForceSymmetry option
   0.0 : 2016.02.18 + Initial version
   Notation: + added, - removed, * fixed, ~ changed  
}
interface ///////////////////////////////////////////////////////////////////////////
const
      Minimization   = False;
      ScoreToReach   = 0;
      FileExtension  = 'txt';
      Size           = 15;
type
      // Basic types
      TScore = Integer;

      TCoord = 0 .. Size - 1;
      TCoords =
         record
         x, y     :  TCoord;
         end;         

      TCell = 0 .. 1;
      TCells = array [TCoord, TCoord] of TCell;

      TSolution =
         record
         Cells       :  TCells;
         NSquares,
         NSet        :  Integer;
         Score       :  TScore;
         end;

      TSolutionDistance = Integer;

      // Local search
      TMove =
         record
         C, C2       :  TCoords;
         NPoints     :  Integer;
         end;

      TMoveUndo =
         record
         Move        :  TMove;
         NSquares,
         NSet,
         Score       :  Integer;
         end;

      TSAUndo = TMoveUndo;

      // Tabu search
      TTabuList = array [TCoord, TCoord, TCell] of Integer;

{$I Interface.inc}
implementation //////////////////////////////////////////////////////////////////////
uses
      InvSys,
      Math,             // Used: Power
      ExtraMath,
      Arrays,
      Statistics,
      RandVars,
      Common,
      Messages;

const
      SquarePenalty  =  2;
      AxisParallel   =  False;
      FixedNPoints   =  True;
      NPoints        =  96;

type
      TSquare  =  array [0 .. 3, 0 .. 1] of Integer;
      TSquares =  array of TSquare;

var
      AllSquares  :  TSquares;
      CellSquares :  array [TCoord, TCoord] of TSquares;

{-----------------------<< Problem-specific >>--------------------------------------}

// Return whether the coordinate i lies within the array
function ValidCoord(
         i     :  Integer
         )     :  Boolean;
   begin
   Result := (i >= 0) and (i < Size);
   end;


// Return whether the Square lies entirely within the array
function ValidSquare(
   const Square   :  TSquare
         )        :  Boolean;
   var
         i, j     :  Integer;
   begin
   Result := True;
   for i := 0 to 3 do
      for j := 0 to 1 do
         Result := Result and ValidCoord(Square[i, j]);
   end;


// Construct a Square from the coordinates of one of its corners and the side vector
procedure MakeSquare(
   var   Square         :  TSquare;
         x, y, dx, dy   :  Integer);
   begin
   Square[0, 0] := x;
   Square[0, 1] := y;
   Square[1, 0] := x + dx;
   Square[1, 1] := y + dy;
   Square[2, 0] := x - dy;
   Square[2, 1] := y + dx;
   Square[3, 0] := x + dx - dy;
   Square[3, 1] := y + dx + dy;
   end;


// #HOTSPOT N1
{$RANGECHECKS OFF} {$OVERFLOWCHECKS OFF}
// Return 1 if a Solution's Square is forbidden, 0 otherwise
function IsBadSquare(
   const Solution :  TSolution;
   const Square   :  TSquare
         )        :  Integer;
   begin
   with Solution do
      if (Cells[Square[0, 0], Square[0, 1]] = 1) and
         (Cells[Square[1, 0], Square[1, 1]] = 1) and
         (Cells[Square[2, 0], Square[2, 1]] = 1) and
         (Cells[Square[3, 0], Square[3, 1]] = 1) then
         Result := 1 else
         Result := 0;
   end;


// Return 1 if a Solution's Square has 1 in the last three corners, 0 otherwise
function IsBadSquare3(
   const Solution :  TSolution;
   const Square   :  TSquare
         )        :  Integer;
   begin
   with Solution do
      if (Cells[Square[1, 0], Square[1, 1]] = 1) and
         (Cells[Square[2, 0], Square[2, 1]] = 1) and
         (Cells[Square[3, 0], Square[3, 1]] = 1) then
         Result := 1 else
         Result := 0;
   end;
{$RANGECHECKS ON} {$OVERFLOWCHECKS ON}


// #HOTSPOT N2
{$RANGECHECKS OFF} {$OVERFLOWCHECKS OFF}
// Return the number of forbidden squares with corner C, assuming 
// that the value at C is 1
function NBadSubSquares(
   const Solution    :  TSolution;
         C           :  TCoords
         )           :  Integer;
   var
         i, Sum      :  Integer;
   begin
   Sum := 0;
   with C do
      for i := 0 to Length(CellSquares[x, y]) - 1 do
         Sum := Sum + IsBadSquare3(Solution, CellSquares[x, y, i]);
   Result := Sum;
   end;
{$RANGECHECKS ON} {$OVERFLOWCHECKS ON}

{-----------------------<< Scores >>------------------------------------------------}

// Set Solution's score
procedure SetScore(
   var   Solution    :  TSolution);
   var
         i, j        :  Integer;
   begin
   with Solution do
      begin
      NSet     := 0;
      NSquares := 0;
      for i := 0 to Size - 1 do
         for j := 0 to Size - 1 do
            NSet := NSet + Cells[i, j];
      for i := 0 to Length(AllSquares) - 1 do
         NSquares := NSquares + IsBadSquare(Solution, AllSquares[i]);
      Score := NSet - NSquares * SquarePenalty;
      end;
   end;
   

// Return a string representation of Score
function FormatScore(
         Score :  TScore
         )     :  AnsiString;
   begin
   Str(Score , Result);
   end;

{-----------------------<< Solution operations >>-----------------------------------}

// Assign SolFrom to SolTo
procedure AssignSolution(
   var   SolTo    :  TSolution;
   const SolFrom  :  TSolution);
   begin
   SolTo := SolFrom;
   end;
   

// Save the Solution to FileSol
procedure SaveSolution(
   var   FileSol     :  Text;
   const Solution    :  TSolution);
   var
         i, j        :  TCoord;
   const
         Chars       :  array [TCell] of AnsiChar = ('.', 'X');
   begin
   WriteLn( FileSol, 'Score    : ', FormatScore(Solution.Score) );
   WriteLn( FileSol, 'ON Cells : ', FormatScore(Solution.NSet) );
   WriteLn( FileSol, 'Squares  : ', FormatScore(Solution.NSquares) );
   for i := 0 to Size - 1 do
      begin
      for j := 0 to Size - 1 do
         Write(FileSol, Chars[ Solution.Cells[i, j] ], ' ');
      WriteLn(FileSol);
      end;
   end;   
   
   
// Load the Solution from FileSol
procedure LoadSolution(
   var   Solution    :  TSolution;
   var   FileSol     :  Text);
   var
         i, j        :  Integer;
         S           :  AnsiString;
   begin
   for i := 1 to 3 do
      ReadLn(FileSol);
   for i := 0 to Size - 1 do
      begin
      ReadLn(FileSol, S);
      for j := 0 to Size - 1 do
         case S[1 + 2 * j] of
            '.':  Solution.Cells[i, j] := 0;
            'X':  Solution.Cells[i, j] := 1;
            else  Assert(False);
            end;
      end;
   SetScore(Solution);
   end;
   
   
// Return the coordinates of a random cell with a specified Value
procedure RandCell(
   var   x, y     :  TCoord;
         Value    :  TCell;
   const Solution :  TSolution);
   begin
   repeat
      x := Random(Size);
      y := Random(Size);
   until Solution.Cells[x, y] = Value;
   end;


// Create a new random Solution
procedure NewSolution(
   var   Solution :  TSolution);
   var
         i, j     :  TCoord;
         k        :  Integer;
   begin
   with Solution do
      if FixedNPoints then
         begin
         for i := 0 to Size - 1 do
            for j := 0 to Size - 1 do
               Cells[i, j] := 0;
         for k := 1 to NPoints do
            begin
            RandCell(i, j, 0, Solution);
            Cells[i, j] := 1;
            end;
         end
      else
         for i := 0 to Size - 1 do
            for j := 0 to Size - 1 do
               Cells[i, j] := Random(2);
   SetScore(Solution);
   end;


// Return the distance between Solution1 and Solution2
function Distance(
   const Solution1,
         Solution2   :  TSolution
         )           :  TSolutionDistance;
   var
         i, j        :  TCoord;
         Sum         :  Integer;
   begin
   Sum := 0;
   for i := 0 to Size - 1 do
      for j := 0 to Size - 1 do
         Sum := Sum + Abs(Solution1.Cells[i, j] - Solution2.Cells[i, j]);
   Result := Sum;
   end;

   
// Problem-specific improvement
procedure SpecialImprove(
   var   Solution :  TSolution);
   begin
   Assert(False);
   end;

   
{-----------------------<< Local Search >>------------------------------------------}
var
      gMoveList   :  TMoveList;

// Invert a Solution's cell with coordinates given by Move
procedure InvertCell(
   var   Solution :  TSolution;
         C        :  TCoords);
   begin
   with Solution, C do
      Cells[x, y] := 1 - Cells[x, y];
   end;


// Invert the point (x, y) and update Solution's score-related fields
procedure InvertAndRecalc(
   var   Solution       :  TSolution;
         C              :  TCoords);
   var
         dSubSquares    :  Integer;
   begin
   with Solution, C do
      begin
      if Cells[x, y] = 0 then
         begin
         InvertCell(Solution, C);
         dSubSquares :=  NBadSubSquares(Solution, C);
         end
      else
         begin
         dSubSquares := -NBadSubSquares(Solution, C);
         InvertCell(Solution, C);
         end;
      NSet := NSet + 2 * Cells[x, y] - 1;
      NSquares := NSquares + dSubSquares;
      Score := NSet - NSquares * SquarePenalty;
      end;
   end;


// Apply a Move to the Solution
procedure PerformMove(
   var   Solution       :  TSolution;
   const Move           :  TMove);
         overload;
   begin
   InvertAndRecalc(Solution, Move.C);
   if Move.NPoints = 2 then
      InvertAndRecalc(Solution, Move.C2);
   end;
   
   
// Apply a Move to the Solution, save Undo
procedure PerformMove(
   var   Solution :  TSolution;
   var   Undo     :  TMoveUndo;
   const Move     :  TMove);
         overload;
   begin
   Undo.Move      := Move;
   Undo.Score     := Solution.Score;
   Undo.NSet      := Solution.NSet;
   Undo.NSquares  := Solution.NSquares;
   PerformMove(Solution, Move);
   end;


// Undo the last move applied to the Solution
procedure UndoMove(
   var   Solution :  TSolution;
   const Undo     :  TMoveUndo);
   begin
   with Undo do
      begin
      InvertCell(Solution, Move.C);
      if Move.NPoints = 2 then
         InvertCell(Solution, Move.C2);
      Solution.Score    := Score;
      Solution.NSet     := NSet;
      Solution.NSquares := NSquares;
      end;
   end;


// Precalculate a static MoveList
procedure PrecalcMoveList(
   var   MoveList :  TMoveList);
   var
         i        :  Integer;
         Move     :  TMove;
   begin
   InitMoveList(MoveList);
   Move.NPoints := 1;
   for i := 0 to Sqr(Size) - 1 do
      begin
      Move.C.x := i mod Size;
      Move.C.y := i div Size;
      AddMove(MoveList, Move);
      end;
   SetTrueLength(MoveList);
   end;


// Make a MoveList for the Solution
procedure MakeMoveList(
   var   MoveList :  TMoveList;
   const Solution :  TSolution;
         Level    :  Integer);
   begin
   InitMoveList(MoveList);
   if Level = 1 then
      AssignMoveList(MoveList, gMoveList);
   end;


// Make a MoveList for the Solution for use in Local Search. Level determines 
// the neighborhood size, with 1 being the smallest.
procedure MakeLSMoveList(
   var   MoveList :  TMoveList;
   const Solution :  TSolution;
         Level    :  Integer); 
   begin
   MakeMoveList(MoveList, Solution, Level);
   end;

{-----------------------<< Simulated Annealing >>-----------------------------------}

// Make a random Move from the Solution
procedure GetRandomMove(
   var   Move     :  TMove;
   const Solution :  TSolution);
   begin
   with Move do
      begin
      if FixedNPoints then
         begin
         Move.NPoints := 2;
         RandCell( C.x,  C.y, 1, Solution);
         RandCell(C2.x, C2.y, 0, Solution);
         end
      else
         begin
         C.x  := Random(Size);
         C.y  := Random(Size);
         C2.x := Random(Size);
         C2.y := Random(Size);
         Move.NPoints := RandRange(1, 2);
         end;
      end;
   end;
   

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

// Apply a mutation operator to the Solution
procedure Mutate(
   var   Solution :  TSolution);
   var
         Undo     :  TSAUndo;
   begin
   MakeNeighbour(Solution, Undo, {T:} 0);
   end;


// Apply a crossover operator to Parent1 and Parent2, creating the Child. Recalc
// indicates whether score recalculation is strictly necessary.
procedure Crossover(
   var   Child     :  TSolution;
   const Parent1,
         Parent2   :  TSolution;
         Recalc    :  Boolean);
   var
         i, j,
         Cut       :  TCoord;
         Horiz     :  Boolean;
   const
         Geometric =  True;
   begin
   Cut := Random(Size - 1);
   Horiz := RandBool;
   for i := 0 to Size - 1 do
      for j := 0 to Size - 1 do
         if Geometric then
            // 1-point
            begin
            if Horiz then
               if i <= Cut then
                  Child.Cells[i, j] := Parent1.Cells[i, j] else
                  Child.Cells[i, j] := Parent2.Cells[i, j]
            else
               if j <= Cut then
                  Child.Cells[i, j] := Parent1.Cells[i, j] else
                  Child.Cells[i, j] := Parent2.Cells[i, j];
            end
         else
            // Uniform
            begin
            if RandBool then
               Child.Cells[i, j] := Parent1.Cells[i, j] else
               Child.Cells[i, j] := Parent2.Cells[i, j];
            end;
   SetScore(Child);
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
         C        :  TCell;
   begin
   for C := 0 to 1 do
      for i := 0 to Size - 1 do
         for j := 0 to Size - 1 do
            TabuList[i, j, C] := 0;
   end;


// Perform aging of the TabuList
procedure AgeTabuList(
   var   TabuList :  TTabuList);
   var
         i, j     :  TCoord;
         C        :  TCell;
   begin
   for C := 0 to 1 do
      for i := 0 to Size - 1 do
         for j := 0 to Size - 1 do
            if TabuList[i, j, C] > 0 then
               Dec(TabuList[i, j, C]);
   end;


// Add a Move that will be applied to Sol to the TabuList with Tenure
procedure AddToTabuList(
   var   TabuList :  TTabuList;
   const Move     :  TMove;
         Tenure   :  Integer;
   const Sol      :  TSolution);
   begin
   with Move.C do
      TabuList[ x, y, Sol.Cells[x, y] ] := Tenure;
   end;


// Make a MoveList for the Solution for use in Tabu Search
procedure MakeTSMoveList(
   var   MoveList :  TMoveList;
   const Solution :  TSolution);
   begin
   MakeMoveList(MoveList, Solution, {Level:} 1);
   end;


// Return whether a Move is in the Solution's TabuList
function IsMoveTabu(
   const Move     :  TMove;
   const Solution :  TSolution;
   const TabuList :  TTabuList
         )        :  Boolean;
   begin
   with Move.C do
      Result := TabuList[ x, y, 1 - Solution.Cells[x, y] ] <> 0;
   end;


// Return a tabu tenure. t is normalized time: t = Iter / MaxIters.
function TabuTenure(
         t  :  Real
         )  :  Integer;
   begin
   Result := 1;
   end;

{-----------------------<< Initialization >>----------------------------------------}

// Precalculate all possible squares
procedure PrecalcAllSquares;
   var
         x, y, dx, dy,
         N              :  Integer;
   begin
   N := 0;
   SetLength(AllSquares, ( Sqr(Size) * (Sqr(Size) - 1) ) div 12);
   for x := 0 to Size - 1 do
      for y := 0 to Size - 1 do
         for dx := 1 to Size - x do
            for dy := 0 to (Size - y) * Ord(not AxisParallel) do
               begin
               MakeSquare(AllSquares[N], x, y, dx, dy);
               if ValidSquare(AllSquares[N]) then
                  Inc(N);
               end;
   SetLength(AllSquares, N);
   end;


// For each array location, precalculate all squares with a corner in it
procedure PrecalcCellSquares;
   var
         x, y, xn, yn,
         N, DiffC       :  Integer;
   begin
   for x := 0 to Size - 1 do
      for y := 0 to Size - 1 do
         begin
         SetLength(CellSquares[x, y], Sqr(Size));
         N := 0;
         for xn := 0 to Size - 1 do
            for yn := 0 to Size - 1 do
               begin
               DiffC := Ord(xn <> x) + Ord(yn <> y);
               if (DiffC > 0) and ((DiffC = 1) or not AxisParallel) then
                  begin
                  MakeSquare(CellSquares[x, y, N], x, y, xn - x, yn - y);
                  if ValidSquare(CellSquares[x, y, N]) then
                     Inc(N);
                  end;
               end;
         SetLength(CellSquares[x, y], N);
         end;
   end;

initialization
PrecalcMoveList(gMoveList);
PrecalcAllSquares;
PrecalcCellSquares;
end.
