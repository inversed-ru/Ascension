{ 
Copyright (c) Peter Karpov 2010 - 2019.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit Problem; ///////////////////////////////////////////////////////////////////////
{
>> Version 0.1

>> Description
   3D N queens PDM: place the maximal number of queens on a N x N x N board so that
   no two queens attack each other. In addition to the diagonals, 3D queens also 
   attack along the triagonals. Solution score is the number of attacked queens.
   
   Problem-specific constants:
      Size           Board size
      NQueens        Number of queens
   
   Supported algorithms: LS, TS, SA, GA.
   
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> Notes
   The upper bound is obviously N^2, but usually it cannot be reached. It is 
   conjectured that the perfect solutions only exist if the smallest factor of N is
   greater than 7.

   Best known values:
          0   1   2   3   4   5   6   7   8   9
    0 |       1   1   4   7  13  21  32  48  67
   10 |  91 121 133 169 172 199 241 289 307 361
   20 | 364 405 463 529 532 547                

   The problem is very hard for most of the metaheuristics. Tabu search may be the
   only algorithm that can find optimal solutions.

>> Changelog
   0.1 : 2019.05.20  - Ascension 2.0 compatibility
   0.0 : 2011.08.05  - Initial version

>> ToDo
    - Consider alternative representations and move formulations
    - Profiling, optimization, maybe asm implementation

>> References
    - http://oeis.org/A068940
    - http://recmath.com/contest/AttackingQueens/FinalReport.html    
    - Three Dimensional Queens Problems
      L. Allison, C. N. Yee, M. McGaughey
      http://www.allisons.org/ll/AlgDS/Recn/Queens3D/
}
interface ///////////////////////////////////////////////////////////////////////////
uses
      InvSys;
const
      Minimization = True;
      ScoreToReach = 0;
      FileExtension = 'txt';
      Size    = 8;
      NQueens = 48;
      Dim  = 3;
      NDiags  = 6;
      NTriags = 4;
type
      // Basic types
      TScore = LongInt;

      TDim    = 0 .. Dim - 1;
      TCoord  = 0 .. Size - 1;
      TCoords = array [TDim] of TCoord;
      TQueens = array [0 .. NQueens - 1] of TCoords;

      TProjPoint = array [0 .. 1] of Integer;
      TMonoProj = array [0 .. Dim     - 1] of TProjPoint;
        TDiProj = array [0 .. NDiags  - 1] of TProjPoint;
       TTriProj = array [0 .. NTriags - 1] of TProjPoint;

      TProjections =
         record
         MonoProj :  TMonoProj;
           DiProj :    TDiProj;
          TriProj :   TTriProj;
         end;

      TMonag  = array [0 ..      Size - 1 , 0 ..      Size - 1 ] of Integer;
      TDiag   = array [0 ..      Size - 1 , 0 .. 2 * (Size - 1)] of Integer;
      TTriag  = array [0 .. 2 * (Size - 1), 0 .. 2 * (Size - 1)] of Integer;

      TSolution =
         record
         // Queen coordinates
         x        :  TQueens;

         // Projections
         Monags   :  array [            TDim] of TMonag;
         Diags    :  array [0 .. NDiags  - 1] of TDiag;
         Triags   :  array [0 .. NTriags - 1] of TTriag;

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
      TTabuList = array [TCoord, TCoord, TCoord] of Integer;

{$I Interface.inc}
implementation //////////////////////////////////////////////////////////////////////
uses
      Math,             // Used: Power
      Arrays,
      Statistics,
      RandVars,
      Common,
      Messages;

{-----------------------<< Problem-specific operations >>---------------------------}

// Return an inverted coordinate C
function InverseCoord(
         C  :  TCoord
         )  :  TCoord;
   begin
   Result := Size - 1 - C;
   end;


// Return the sum of C1 and C2 or inverted C2 depending on Inv2
function CoordSum(
         C1, C2   :  TCoord;
         Inv2     :  Boolean
         )        :  Integer;
   begin
   Result := C1;
   if Inv2 then
      Inc( Result, InverseCoord(C2) ) else
      Inc( Result,              C2  );
   end;
   
   
// Return whether the coordinates X1 and X2 are equal   
function EqualCoords(
   const X1, X2   :  TCoords
         )        :  Boolean;
   var
         Equal    :  Boolean;
         k        :  Integer;
   begin
   Equal := True;
   for k := 0 to Dim - 1 do
      if X1[k] <> X2[k] then
         begin
         Equal := False;
   {<}   break;
         end;
   Result := Equal;
   end;


// Get the projections of point X
procedure GetProjections(
   var   Proj        :  TProjections;
   const X           :  TCoords);
   var
         i, j, k, h  :  Integer;
         Inv         :  Boolean;
   begin
   with Proj do
      begin
      // Monagonals
      for k := 0 to Dim - 1 do
         for h := 0 to 1 do
            MonoProj[k, h] := X[(k + 1 + h) mod Dim];

      // Diagonals
      for k := 0 to NDiags - 1 do
         begin
         h := k div 2;
         i := (h + 1) mod Dim;
         j := (h + 2) mod Dim;
         DiProj[k, 0] := X[h];
         DiProj[k, 1] := CoordSum(X[i], X[j], {Inv2:} (k mod 2) = 1);
         end;

      // Triagonals
      for i := 0 to 1 do
         for j := 0 to 1 do
            for k := 0 to 1 do
               begin
               Inv := ( (k = 0) and (i = 1) ) or
                      ( (k = 1) and (j = 1) );
               TriProj[2 * i + j, k] := CoordSum(X[0], X[1 + k], Inv);
               end;
      end;
   end;


// Return the number of conflicts of the Solution's ith queen
function Conflicts(
   const Solution :  TSolution;
         Index    :  Integer
         )        :  Integer;
   var
         Proj     :  TProjections;
         k, Sum   :  Integer;
   begin
   Sum := 0;
   with Solution, Proj do
      begin
      GetProjections(Proj, x[Index]);
      for k := 0 to Dim - 1 do
         Inc(Sum, Monags[ k, MonoProj[k, 0], MonoProj[k, 1] ] - 1);
      for k := 0 to NDiags - 1 do
         Inc(Sum,  Diags[ k,   DiProj[k, 0],   DiProj[k, 1] ] - 1);
      for k := 0 to NTriags - 1 do
         Inc(Sum, Triags[ k,  TriProj[k, 0],  TriProj[k, 1] ] - 1);
      end;
   Result := Sum;
   end;


// Change the Solution's projections by Delta   
procedure ChangeProjections(
   var   Solution :  TSolution;
   const Proj     :  TProjections;
         Delta    :  Integer);
   var
         k        :  Integer;
   begin
   with Solution, Proj do
      begin
      for k := 0 to Dim     - 1 do
         Inc(Monags[ k, MonoProj[k, 0], MonoProj[k, 1] ], Delta);
      for k := 0 to NDiags  - 1 do
         Inc(Diags [ k,   DiProj[k, 0],   DiProj[k, 1] ], Delta);
      for k := 0 to NTriags - 1 do
         Inc(Triags[ k,  TriProj[k, 0],  TriProj[k, 1] ], Delta);
      end;
   end;


// Clear the Solution's queen projections
procedure ClearProjections(
   var   Solution :  TSolution);
   var
         i, j, k  :  Integer;
   begin
   for k := 0 to Dim - 1 do
      for i := 0 to Size - 1 do
         for j := 0 to Size - 1 do
            Solution.Monags[k, i, j] := 0;
   for k := 0 to NDiags - 1 do
      for i := 0 to Size - 1 do
         for j := 0 to 2 * (Size - 1) do
            Solution.Diags[k, i, j] := 0;
   for k := 0 to NTriags - 1 do
      for i := 0 to 2 * (Size - 1) do
         for j := 0 to 2 * (Size - 1) do
            Solution.Triags[k, i, j] := 0;
   end;


// Adjust Solution's projections after adding a Queen
procedure AddQueen(
   var   Solution :  TSolution;
   const Queen    :  TCoords);
         overload;
   var
         Proj     :  TProjections;
   begin
   GetProjections(Proj, Queen);
   ChangeProjections(Solution, Proj, {Delta:} +1);
   end;


// Set Solution's i'th queen, adjust the projections
procedure AddQueen(
   var   Solution :  TSolution;
   const Queen    :  TCoords;
         i        :  Integer);
         overload;
   begin
   Solution.X[i] := Queen;
   AddQueen(Solution, Queen);
   end;


// Adjust Solution's projections after deleting a Queen
procedure DelQueen(
   var   Solution :  TSolution;
   const Queen    :  TCoords);
   var
         Proj     :  TProjections;
   begin
   GetProjections(Proj, Queen);
   ChangeProjections(Solution, Proj, {Delta:} -1);
   end;

{-----------------------<< Scores >>------------------------------------------------}

// Set Solution's score
procedure SetScore(
   var   Solution :  TSolution);
   var
         i        :  Integer;
   begin
   with Solution do
      begin
      ClearProjections(Solution);

      // Add all queens to projections
      for i := 0 to NQueens - 1 do
         AddQueen(Solution, x[i]);

      // Caluclate the number of conflicts
      Score := 0;
      for i := 0 to NQueens - 1 do
         Score := Score + Conflicts(Solution, i);
      Score := Score div 2;
      end;
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
         i, k        :  Integer;
   begin
   WriteLn(FileSol, 'Score: ', Solution.Score);
   for i := 0 to NQueens - 1 do
      begin
      for k := 0 to Dim - 1 do
         Write(FileSol, Solution.x[i, k], Tab);
      WriteLn(FileSol);
      end;
   end;


// Load a Solution from FileSol
procedure LoadSolution(
   var   Solution    :  TSolution;
   var   FileSol     :  Text);
   var
         i, k        :  Integer;
   begin
   ReadLn(FileSol);
   for i := 0 to NQueens - 1 do
      begin
      for k := 0 to Dim - 1 do
         Read(FileSol, Solution.x[i, k]);
      ReadLn(FileSol);
      end;
   end;
   

// Create a new random Solution
procedure NewSolution(
   var   Solution :  TSolution);
   var
         i, k     :  Integer;
   begin
   for k := 0 to Dim - 1 do
      for i := 0 to NQueens - 1 do
         Solution.x[i, k] := Random(Size);
   SetScore(Solution);
   end;


// Return the distance between Solution1 and Solution2.
// O(N^3) time complexity.
function Distance(
   const Solution1,
         Solution2   :  TSolution
         )           :  TSolutionDistance;
   var
         i, j, k     :  Integer;
         C           :  TCoords;
         Dist        :  TSolutionDistance;
         Cube        :  array [TCoord, TCoord, TCoord] of Boolean;
   begin
   // Clear the cube with queen positions
   for i := 0 to Size - 1 do
      for j := 0 to Size - 1 do
         for k := 0 to Size - 1 do
            Cube[i, j, k] := False;

   // Mark the coordinates of Solution1's queens
   for i := 0 to NQueens - 1 do
      begin
      C := Solution1.x[i];
      Cube[ C[0], C[1], C[2] ] := True;
      end;

   // Check whether Solution2's queens match those of Solution1
   Dist := NQueens;
   for i := 0 to NQueens - 1 do
      begin
      C := Solution2.x[i];
      if Cube[ C[0], C[1], C[2] ] then
         Dec(Dist);
      end;
   Result := Dist;
   end;


// Problem-specific improvement
procedure SpecialImprove(
   var   Solution :  TSolution);
   begin
   Assert(False);
   end;

{-----------------------<< Moves >>-------------------------------------------------}

// Make a MoveList of a Solution
procedure MakeMoveList(
   var   MoveList    :  TMoveList;
   const Solution    :  TSolution;
         Level       :  Integer);
   var
         i, j, k, l  :  Integer;
         Move        :  TMove;
   begin
   InitMoveList(MoveList);
   for i := 0 to NQueens - 1 do with Solution do
      begin
      Move.i := i;
      case Level of
         1: for k := 0 to Dim - 1 do
               for j := 0 to Size - 1 do
                  if j <> X[i, k] then
                     begin
                     Move.NewC := X[i];
                     Move.NewC[k] := j;
                     AddMove(MoveList, Move);
                     end;
         2: for k := 0 to Dim - 1 do
               for j := 0 to Size - 1 do
                  for l := 0 to Size - 1 do
                     begin
                     Move.NewC := X[i];
                     Move.NewC[(k + 1) mod 3] := j;
                     Move.NewC[(k + 2) mod 3] := l;
                     if not EqualCoords(Move.NewC, X[i]) then
                        AddMove(MoveList, Move);
                     end;
         3: for j := 0 to Size - 1 do
               for k := 0 to Size - 1 do
                  for l := 0 to Size - 1 do
                     begin
                     Move.NewC[0] := j;
                     Move.NewC[1] := k;
                     Move.NewC[2] := l;
                     if not EqualCoords(Move.NewC, X[i]) then
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
   var
         OldConf,
         NewConf  :  Integer;
   begin
   with Solution, Move do
      begin
      OldConf := Conflicts(Solution, i);
      DelQueen(Solution, X[i]);
      AddQueen(Solution, NewC, i);
      NewConf := Conflicts(Solution, i);
      Score := Score + NewConf - OldConf;
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
   Undo.NewC := Solution.X[Move.i];
   PerformMove(Solution, Move);
   end;


// Make a random Move from Solution
procedure GetRandomMove(
   var   Move     :  TMove;
   const Solution :  TSolution);
   var
         OldC     :  TCoords;
         i, k     :  Integer;
   begin
   Move.i := Random(NQueens);
   OldC := Solution.X[Move.i];
   repeat
      Move.NewC := OldC;
      i := Random(Dim);
      for k := 0 to Random(Dim) do
         Move.NewC[(i + k) mod Dim] := Random(Size);
   until not EqualCoords(Move.NewC, OldC);
   end;


// Undo the last move applied to the Solution
procedure UndoMove(
   var   Solution :  TSolution;
   const Undo     :  TMoveUndo);
   begin
   PerformMove(Solution, Undo);
   end;


// Make a MoveList of a Solution for use in Local Search. Level determines
// neighborhood size, with 1 being the smallest.
procedure MakeLSMoveList(
   var   MoveList :  TMoveList;
   const Solution :  TSolution;
         Level    :  Integer);
   begin
   MakeMoveList(MoveList, Solution, Level);
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

// Apply a mutation operator to the Solution
procedure Mutate(
   var   Solution  :  TSolution);
   var
         Move     :  TMove;
   begin
   GetRandomMove(Move, Solution);
   PerformMove(Solution, Move);
   end;


// Apply a crossover operator to Parent1 and Parent2, creating the Child. 
// Recalc indicates whether score recalculation is strictly necessary.
// Constructive crossover: O(N^4) complexity.
procedure Crossover(
   var   Child    :  TSolution;
   const Parent1,
         Parent2  :  TSolution;
         Recalc   :  Boolean);
   var
         i, j     :  Integer;
         TrialX   :  TQueens;
         X        :  TCoords;
         Used     :  array [0 .. 1, 0 .. NQueens - 1] of Boolean;
         Which    :  Integer;
         Cost     :  TRealArray;
   begin
   // Initialize
   ClearProjections(Child);
   Child.Score := 0;
   SetLength(Cost, NQueens);
   for j := 0 to 1 do
      for i := 0 to NQueens - 1 do
         Used[j, i] := False;

   // Construct the child from parent elements
   for i := 0 to NQueens - 1 do
      begin
      // Select queens from alternating parents
      Which := i mod 2;
      if Which = 0 then
         TrialX := Parent1.X
      else
         TrialX := Parent2.X;

      // Calculate the cost of adding each queen
      for j := 0 to NQueens - 1 do
         if not Used[Which, j] then
            begin
            X := TrialX[j];
            AddQueen(Child, X, i);
            Cost[j] := Conflicts(Child, i);
            DelQueen(Child, X);
            end
         else
            Cost[j] := NQueens;

      // Add the queen that introduces the least conflicts
      j := RandMinIndex(Cost);
      AddQueen(Child, TrialX[j], i);
      Inc( Child.Score, Round(Cost[j]) );
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
   

// Initialize the TabuList, must be called before use
procedure InitTabuList(
   var   TabuList :  TTabuList);
   var
         i, j, k  :  Integer;
   begin
   for i := 0 to Size - 1 do
      for j := 0 to Size - 1 do
         for k := 0 to Size - 1 do
            Tabulist[i, j, k] := 0;
   end;


// Perform aging of the TabuList
procedure AgeTabuList(
   var   TabuList :  TTabuList);
   var
         i, j, k  :  Integer;
   begin
   for i := 0 to Size - 1 do
      for j := 0 to Size - 1 do
         for k := 0 to Size - 1 do
            if Tabulist[i, j, k] > 0 then
               Dec(Tabulist[i, j, k]);
   end;


// Add a Move that will be applied to Sol to the TabuList with Tenure
procedure AddToTabuList(
   var   TabuList :  TTabuList;
   const Move     :  TMove;
         Tenure   :  Integer;
   const Sol      :  TSolution);
   var
         C        :  TCoords;
   begin
   C := Sol.X[Move.i];
   TabuList[ C[0], C[1], C[2] ] := Tenure;
   end;


// Make a MoveList for Solution for use in Tabu Search
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
   with Move do
      Result := TabuList[ NewC[0], NewC[1], NewC[2] ] <> 0;
   end;


// Return a tabu tenure. t is the normalized time: t = Iter / MaxIters.
function TabuTenure(
         t  :  Real
         )  :  Integer;
   begin
   Result := Round( 3 + 3 * Size * Sqr(1 - t) );
   end;

end.
