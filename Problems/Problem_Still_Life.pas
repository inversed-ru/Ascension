{ 
Copyright (c) Peter Karpov 2010 - 2019.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit Problem; ///////////////////////////////////////////////////////////////////////
{
>> Version 1.2

>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> Description
   Maximal density still life problem PDM: find an N x N stable pattern in Conway's 
   Game of Life having the maximal number of live cells. The solutions are saved in
   Golly format.
   
   Problem-specific constants:
      Size        Pattern size, including a 2 cell dead zone on each side

   Supported algorithms: LS, TS, SA, GA.

>> Notes
   Moves are based on generating random 3x3 blocks such that the central cell is
   stable. This is achieved by creating the required numbers of live cells in the
   neighbourhood - 2 .. 3 for a live cell, 4 .. 6 for a dead one. This method can
   sometimes fail. For example, 2 out of 6 non-isomorphic 6-cell configurations
   are invalid. Nonetheless, this scheme is quite efficient. 
   
   Optimal values:
            0     1     2     3     4     5     6     7     8     9
    0 |           0     4     6     8    16    18    28    36    43
   10 |    54    64    76    90   104   119   136   152   171   190 
   20 |   210   232   253   276   301   326   352   379   407   437
   30 |   467   497   531   563   598   633   668   706   744   782
   40 |   824   864   907   949   993  1039  1085  1132  1181  1229
   50 |  1280  1331  1382  1436  1490  1545  1602  1658  1717  1776

   Considered or tried but failed problems:
    - Oscillators. Not a complete failure, as oscillators of periods 2 and 3 are
      easily found. However, finding oscillators of higher periods proved nigh
      impossible.
    - Spaceships. Only simple patterns were found, namely: glider, light- and
      middleweight spaceships. Perhaps, this problem is not well suited for a
      metaheuristic approach.
    - Perfect eaters capable of absorbing glider from any direction. No such multi-
      directional eaters were found.
    - Methuselahs. Extreme computational cost.
    - Nonstationary maximal density. Obtained patterns are inferior to dense still
      lifes.    

>> ToDo
    - Try different rules (high life)
    - Efficient crossover based on incremental evaluation
    ? Heavier optimization

>> References
    - http://oeis.org/A055397
    - A Complete Solution to the Maximum Density Still Life Problem.
      Geoffrey Chu, Peter J. Stuckey.
    - http://golly.sourceforge.net/

>> Changelog
   1.2 : 2019.05.21  - Separate unit instead of an inc file
                     - Ascension 2.0 compatibility
   1.1 : 2013.05.17  - Incremental cell stillness calculation, huge speedup
                     - Various optimizations
                     - Removed maximal short time density subproblem
                     - Cleanup
   1.0 : 2013.04.28  - Removed failed subproblems
                     - New still life move generation and scoring
   0.5 : 2013.04.22  - Added experimental CA subproblems
   0.4 : 2012.05.02  - Modified scoring for max density still life
                     - Turned off error checking in hotspots
   0.3 : 2012.02.12  - Moved some routines to Problem.pas
                     - Removed oscillator scoring
   0.2 : 2012.02.08  - New scoring for max density still life problem
   0.1 : 2011.08.02  - Split into shell and include file
   0.0 : 2011.06.05  - Initial version
}
interface ///////////////////////////////////////////////////////////////////////////
const
      Minimization   = False;
      ScoreToReach   = 0;
      FileExtension  = 'txt';
      Size           = 32;
type
      // Basic types
      TScore = Real;

      TCoord = 0 .. Size - 1;
      TCoords =
         record
         x, y  :  TCoord;
         end;         
      
      TCell = 0 .. 1;

      TCells = array [TCoord, TCoord] of TCell;
      PCells = ^TCells;

      TSolution =
         record
         Cells,
         Changing    :  TCells;
         X, Y,
         NLive,
         NChanging   :  Integer;
         Score       :  TScore;
         end;

      TSolutionDistance = Integer;

      // Local search
      TMove =
         record
         x, y  :  TCoord;
         end;

      TMoveUndo =
         record
         Move  :  TMove;
         Score :  TScore;
         end;

      TSAUndo = TSolution;

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
      Common;

{-----------------------<< Problem-specific operations >>---------------------------}
const
      DeadZone       =  2;
      TrueSize       =  Size - 2 * DeadZone;
      MinTrueCoord   =  DeadZone;
      MaxTrueCoord   =  Size - 1 - DeadZone;

// Return the number of live cells within the specified rectangle
function NCells(
   const Cells       :  TCells;
         MinX, MaxX,
         MinY, MaxY  :  Integer
         )           :  Integer;
   var
         i, j        :  TCoord;
         Sum         :  Integer;
   begin
   Sum := 0;
   for i := MinX to MaxX do
      for j := MinY to MaxY do
         Sum := Sum + Cells[i, j];
   Result := Sum;
   end;


// Return the total number of live cells
function NTotalCells(
   const Cells       :  TCells
         )           :  Integer;
   begin
   Result := NCells(Cells, MinTrueCoord, MaxTrueCoord,
                           MinTrueCoord, MaxTrueCoord);
   end;


// Return the number of live cells in a block of radius R around X, Y
function NBlockCells(
   const Cells    :  TCells;
         X, Y, R  :  Integer
         )        :  Integer;
   begin
   Result := NCells(Cells, X - R, X + R, Y - R, Y + R);
   end;


{$RANGECHECKS OFF} {$OVERFLOWCHECKS OFF}
// #HOTSPOT N1
// Return 1 if a specified cell will change in the next generation, 0 otherwise
function IsChanging(
   const Cells :  TCells;
         x, y  :  Integer
         )     :  Integer;
   var
         Sum   :  Integer;
         A     :  Boolean;
   begin
   A := Cells[x, y] = 1;
   Sum :=
      Cells[x - 1, y    ] + Cells[x    , y - 1] +
      Cells[x + 1, y    ] + Cells[x    , y + 1] +
      Cells[x - 1, y - 1] + Cells[x + 1, y + 1] +
      Cells[x + 1, y - 1] + Cells[x - 1, y + 1] ;
   Result := Ord( (A xor (Sum = 3)) and (not A or (Sum <> 2)) );
   end;
{$RANGECHECKS ON} {$OVERFLOWCHECKS ON}


// Calculate the map of changing cells of the Solution
procedure CalcChanging(
   var   Solution :  TSolution);
   var
         i, j     :  Integer;
   begin
   with Solution do
      begin
      NChanging := 0;
      for i := 1 to Size - 2 do
         for j := 1 to Size - 2 do
            begin
            Changing[i, j] := IsChanging(Cells, i, j);
            Inc(NChanging, Changing[i, j]);
            end;
      end;
   end;


{$RANGECHECKS OFF} {$OVERFLOWCHECKS OFF}
// #HOTSPOT N2
// Partially recalculate the change map of the Solution within R cells around X, Y
procedure RecalcChanging(
   var   Solution :  TSolution;
         X, Y, R  :  Integer);
   var
         i, j     :  Integer;
   begin
   with Solution do
      begin
      for i := X - R to X + R do
         for j := Y - R to Y + R do
            Dec(NChanging, Changing[i, j]);
      for i := X - R to X + R do
         for j := Y - R to Y + R do
            begin
            Changing[i, j] := IsChanging(Cells, i, j);
            Inc(NChanging, Changing[i, j]);
            end;
      end;
   end;


// #HOTSPOT N3
// Copy a block of cells within radius R of X, Y from A to B
procedure CopyBlock(
   var   B        :  TCells;
   const A        :  TCells;
         X, Y, R  :  Integer);
   var
         i, j  :  Integer;
   begin
   for i := X - R to X + R do
      for j := Y - R to Y + R do
         B[i, j] := A[i, j];
   end;
{$RANGECHECKS ON} {$OVERFLOWCHECKS ON}


// Copy a block of cells and the change map at X, Y from A to B
procedure CopyBlocks(
   var   B     :  TSolution;
   const A     :  TSolution;
         X, Y  :  Integer);
   begin
   CopyBlock(B.Cells,    A.Cells,    X, Y, {R:} 1);
   CopyBlock(B.Changing, A.Changing, X, Y, {R:} 2);
   end;   

{-----------------------<< Scores >>------------------------------------------------}

// Recalculate the Solution's score based on the number of live and changing cells
procedure RecalcScore(
   var   Solution :  TSolution);
   const
         Penalty  =  2;
   begin
   with Solution do
      //Score := NLive - Penalty * NChanging;
      Score := NLive * Exp(-2 * Penalty * NChanging / Sqr(TrueSize));
   end;
   

// Set the Solution's score
procedure SetScore(
   var   Solution :  TSolution);
   begin
   CalcChanging(Solution);
   Solution.NLive := NTotalCells(Solution.Cells);
   RecalcScore(Solution);
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
   

// Save the Solution to FileSol, Golly format
procedure SaveSolution(
   var   FileSol     :  Text;
   const Solution    :  TSolution);
   var
         i, j        :  TCoord;
   const
         Chars       :  array [TCell] of AnsiChar = ('.', '*');
   begin
   with Solution do
      begin
      WriteLn( FileSol, '#Life 1.05');
      WriteLn( FileSol, '#D Score: ',     Score);
      WriteLn( FileSol, '#D Cells: ',     NLive);
      WriteLn( FileSol, '#D Different: ', NChanging);
      WriteLn( FileSol, '#N');
      for i := 0 to Size - 1 do
         begin
         for j := 0 to Size - 1 do
            Write(FileSol, Chars[ Cells[i, j] ]);
         WriteLn(FileSol);
         end;
      end;
   end;
   

// Load a Solution from FileSol
procedure LoadSolution(
   var   Solution    :  TSolution;
   var   FileSol     :  Text);
   begin
   Assert(False);
   end;
   
   
// Return whether a cell with coordinates i, j lies in the border zone
function IsBorderCell(
         i, j  :  TCoord
         )     :  Boolean;
   begin
   Result := not (
      InRange(i, MinTrueCoord, MaxTrueCoord) and
      InRange(j, MinTrueCoord, MaxTrueCoord) );
   end;
   

// Create a new random Solution
procedure NewSolution(
   var   Solution :  TSolution);
   var
         i, j     :  TCoord;
   begin
   for i := 0 to Size - 1 do
      for j := 0 to Size - 1 do
         if IsBorderCell(i, j) then
            Solution.Cells[i, j] := 0 else
            Solution.Cells[i, j] := Random(2);
   SetScore(Solution);
   end;


{$RANGECHECKS OFF} {$OVERFLOWCHECKS OFF}
// Return the number of different cells between Cells1 and Cells2
function NDiffCells(
   const Cells1,
         Cells2   :  TCells
         )        :  Integer;
   var
         i, j     :  TCoord;
         Sum      :  Integer;
   begin
   Sum := 0;
   for i := 0 to Size - 1 do
      for j := 0 to Size - 1 do
         Sum := Sum + Abs(Cells1[i, j] - Cells2[i, j]);
   Result := Sum;
   end;
{$RANGECHECKS ON} {$OVERFLOWCHECKS ON}


// Returnthe distance between Solution1 and Solution2
function Distance(
   const Solution1,
         Solution2   :  TSolution
         )           :  TSolutionDistance;
   begin
   Result := NDiffCells(Solution1.Cells, Solution2.Cells);
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
   var   Cells    :  TCells;
         X, Y     :  TCoord);
         overload;
   begin
   Cells[x, y] := 1 - Cells[x, y];
   end;


procedure InvertCell(
   var   Solution :  TSolution;
         X, Y     :  TCoord);
         overload;
   begin
   InvertCell(Solution.Cells, X, Y);
   end;


procedure InvertCell(
   var   Solution :  TSolution;
   const Move     :  TMove);
         overload;
   begin
   InvertCell(Solution.Cells, Move.X, Move.Y);
   end;
   

// Apply a Move to the Solution
procedure PerformMove(
   var   Solution :  TSolution;
   const Move     :  TMove);
         overload;
   begin
   InvertCell(Solution, Move);
   SetScore(Solution);
   end;
   

// Apply a Move to the Solution, save the Undo
procedure PerformMove(
   var   Solution :  TSolution;
   var   Undo     :  TMoveUndo;
   const Move     :  TMove);
         overload;
   begin
   Undo.Move  := Move;
   Undo.Score := Solution.Score;
   PerformMove(Solution, Move);
   end;


// Undo the last move applied to the Solution
procedure UndoMove(
   var   Solution :  TSolution;
   const Undo     :  TMoveUndo);
   begin
   InvertCell(Solution, Undo.Move);
   Solution.Score := Undo.Score;
   end;


// Precalculate static MoveList
procedure PrecalcMoveList(
   var   MoveList :  TMoveList);
   var
         i, j     :  TCoord;
         Move     :  TMove;
   begin
   InitMoveList(MoveList);
   for i := DeadZone to Size - DeadZone - 1 do
      begin
      Move.x := i;
      for j := DeadZone to Size - DeadZone - 1 do
         begin
         Move.y := j;
         AddMove(MoveList, Move);
         end;
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


// Make a MoveList for the Solution for use in Local Search. Level determines the
// neighborhood size, with 1 being the smallest.
procedure MakeLSMoveList(
   var   MoveList :  TMoveList;
   const Solution :  TSolution;
         Level    :  Integer); 
   begin
   MakeMoveList(MoveList, Solution, Level);
   end;

{-----------------------<< Simulated Annealing >>-----------------------------------}

{$RANGECHECKS OFF} {$OVERFLOWCHECKS OFF}
// Randomly modify the neighborhood of X, Y point so that it contains exactly 
// N live cells, ensure that at least one cell was changed
procedure SetNearCells(
   var   Cells    :  TCells;
         X, Y, N  :  Integer);
   const
         NAround  =  8;
   var
         i, j, k,
         OldCell  :  Integer;
         Indices  :  array [1 .. NAround] of Integer;
         Changed  :  Boolean;
   const
         Shifts   :  array [1 .. NAround, 0 .. 1] of Integer =
                     ( (-1, -1), (0, -1), ( 1, -1), ( 1, 0),
                       ( 1,  1), (0,  1), (-1,  1), (-1, 0) );
   begin
   repeat
      // Set the cell values
      RandPerm(Indices, {Base:} 1);
      Changed := False;
      for k := 1 to NAround do
         begin
         i := X + Shifts[Indices[k], 0];
         j := Y + Shifts[Indices[k], 1];
         OldCell := Cells[i, j];
         Cells[i, j] := Ord(k <= N);
         Changed := Changed or (Cells[i, j] <> OldCell);
         end;
   until Changed;
   end;
{$RANGECHECKS ON} {$OVERFLOWCHECKS ON}


// Apply a random move to the Solution, save the Undo
procedure MakeNeighbour(
   var   Solution    :  TSolution;
   var   Undo        :  TSAUndo;
         T           :  Real);
   var
         N,
         OldSubLive,
         NewSubLive  :  Integer;
   const
         MinCoord    =  MinTrueCoord + 1;
         MaxCoord    =  MaxTrueCoord - 1;
   begin
   // Select the modification coordinates
   Solution.X := RandRange(MinCoord, MaxCoord);
   Solution.Y := RandRange(MinCoord, MaxCoord);

   // Create undo
   with Solution do
      begin
      Undo.X         := X;
      Undo.Y         := Y;
      Undo.NLive     := NLive;
      Undo.NChanging := NChanging;
      Undo.Score     := Score;
      CopyBlocks(Undo, Solution, X, Y);
      end;

   // Generate a random block, update the score
   with Solution do
      begin
      OldSubLive := NBlockCells(Cells, X, Y, {R:} 1);
      Cells[X, Y] := Random(2);
      if Cells[X, Y] = 0 then
         N := RandRange(4, 6) else
         N := RandRange(2, 3);
      SetNearCells(Cells, X, Y, N);
      RecalcChanging(Solution, X, Y, {R:} 2);
      NewSubLive := NBlockCells(Cells, X, Y, {R:} 1);
      NLive := NLive + NewSubLive - OldSubLive;
      RecalcScore(Solution);
      end;
   end;


// Undo the last move applied to the Solution
procedure UndoSAMove(
   var   Solution :  TSolution;
   const Undo     :  TSAUndo);
   begin
   Solution.NLive     := Undo.NLive;
   Solution.NChanging := Undo.NChanging;
   Solution.Score     := Undo.Score;
   CopyBlocks(Solution, Undo, Undo.X, Undo.Y);
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
   Cut := DeadZone + Random(TrueSize - 1);
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


// Age the TabuList
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
   TabuList[ Move.x, Move.y, Sol.Cells[Move.x, Move.y] ] := Tenure;
   end;


// Make a MoveList for the tabu search
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
   Result := TabuList[ Move.x, Move.y, 1 - Solution.Cells[Move.x, Move.y] ] <> 0;
   end;
   
   
// Return a tabu tenure at normalized time t
function TabuTenure(
         t        :  Real     // [0 .. 1]
         )        :  Integer;
   const
         MinTabu  =   6;
         MaxTabu  =  19;
   begin
   Result := Round( MinTabu + (1 - t) * (MaxTabu - MinTabu) * (1 + Random) );
   end;

initialization {--------------------------------------------------------------------}
PrecalcMoveList(gMoveList);
end.
