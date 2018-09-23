{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit Problem; ///////////////////////////////////////////////////////////////////////
{
>> Version: 1.1

>> Description
   N queens PDM: place N queens on a N x N board so that no two queens attack each
   other.

   Supported algorithms:
   - Local Search
   - Tabu Search
   - Simulated Annealing
   - Genetic Algorithm
   
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> Notes
   Permutational formulation is much more efficient than the one based on independent
   coordinates. 

>> Changelog
   1.1 : 2018.09.18 ~ FreePascal compatibility
                    - EDA operators
   1.0 : 2016.04.11 - "set coordinate" move type
                    - SI, SP, GLS sections
                    ~ Sections rearranged and cleaned up
   0.4 : 2012.02.22 ~ Faster move generation using a precalculated move list
   0.3 : 2012.02.06 + Guided move
   0.2 : 2011.12.03 + Swap move type
   0.1 : 2011.05.10 ~ Fast implementation, O(1) conflict check
   0.0 : 2010.11.14 + Initial version
   Notation: + added, - removed, * fixed, ~ changed   
}
interface ///////////////////////////////////////////////////////////////////////////
uses
      InvSys;
const
      IsMinimize     = True;
      FileExtension  = 'txt';
      Size           = 64;
type
      // Basic types
      TScore = Integer;

      TCoord = 0 .. Size - 1;

      TSolution =
         record
         // Queen Y coordinates
         a        :  array [TCoord] of Integer;

         // Horizontal and diagonal projections
         Nx       :  array [TCoord] of Integer;
         Nd1, Nd2 :  array [0 .. 2 * (Size - 1)] of Integer;
         
         Score    :  TScore;
         end;

      TSolutionDistance = Integer;

      // Local search
      TMove =
         record
         i, j  :  TCoord;
         end;

      TMoveUndo = TMove;
      TSAUndo   = TMove;
      
      // Tabu search
      TTabuList = array [TCoord, TCoord] of Integer;

{$I Interface.inc}
implementation //////////////////////////////////////////////////////////////////////
uses
      Math,
      Arrays,
      Statistics,
      RandVars,
      Crossovers,
      Common,
      Messages;

{-----------------------<< Problem-specific operations >>---------------------------}

// Return the number of conflicts of a queen at position i
function Conflicts(
   const Solution    :  TSolution;
         i           :  Integer
         )           :  Integer;
   begin
   with Solution do
      Result :=
         (Nx [     a[i] ]           - 1) +
         (Nd1[ i + a[i] ]           - 1) +
         (Nd2[ i - a[i] + Size - 1] - 1) ;
   end;


// Set the y coordinate of a queen with a given x,
// recalculate the projections and the number of conflicts
procedure SetQueen(
   var   Solution :  TSolution;
         x, y     :  Integer);
   var
         OldConf,
         NewConf  :  Integer;
   begin
   with Solution do
      begin
      OldConf := Conflicts(Solution, x);
      Dec( Nx [    a[x]           ] );
      Dec( Nd1[x + a[x]           ] );
      Dec( Nd2[x - a[x] + Size - 1] );
      a[x] := y;
      Inc( Nx [    y           ] );
      Inc( Nd1[x + y           ] );
      Inc( Nd2[x - y + Size - 1] );
      NewConf := Conflicts(Solution, x);
      Score := Score + NewConf - OldConf;
      end;
   end;


// Problem-specific improvement
procedure SpecialImprove(
   var   Solution :  TSolution);
   begin
   Assert(False);
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
      // Clear projections
      for i := 0 to Size - 1 do
         Nx[i] := 0;
      for i := 0 to 2 * (Size - 1) do
         begin
         Nd1[i] := 0;
         Nd2[i] := 0;
         end;

      // Calculate projections
      for i := 0 to Size - 1 do
         begin
         Inc(Nx [     a[i] ]);
         Inc(Nd1[ i + a[i] ]);
         Inc(Nd2[ i - a[i] + Size - 1 ]);
         end;

      // Caluclate number of conflicts
      Score := 0;
      for i := 0 to Size - 1 do
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

// SolTo := SolFrom
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
         i           :  TCoord;
   begin
   WriteLn(FileSol, 'Score: ', Solution.Score);
   for i := 0 to Size - 1 do
      WriteLn(FileSol, Solution.a[i]);
   end;


// Load a Solution from FileSol
procedure LoadSolution(
   var   Solution    :  TSolution;
   var   FileSol     :  Text);
   var
         i           :  TCoord;
   begin
   ReadLn(FileSol);
   for i := 0 to Size - 1 do
      ReadLn(FileSol, Solution.a[i]);
   SetScore(Solution);
   end;


// Create a new random Solution
procedure NewSolution(
   var   Solution :  TSolution);
   begin
   RandPerm(Solution.A, {Base:} 0);
   SetScore(Solution);
   end;
   

// Return the distance between Solution1 and Solution2
function Distance(
   const Solution1,
         Solution2   :  TSolution
         )           :  TSolutionDistance;
   var
         i           :  TCoord;
         Sum         :  TSolutionDistance;
   begin
   Sum := 0;
   for i := 0 to Size - 1 do
      if Solution1.a[i] <> Solution2.a[i] then
         Inc(Sum);
   Result := Sum;
   end;

{-----------------------<< Moves >>-------------------------------------------------}
var
      gMoveList   :  TMoveList;

// Precalculate MoveList
procedure PrecalcMoveList(
   var   MoveList :  TMoveList);
   var
         i, j     :  Integer;
         Move     :  TMove;
   begin
   InitMoveList(MoveList);
   for i := 0 to Size - 1 do
      begin
      Move.i := i;
      for j := 0 to i - 1 do
         begin
         Move.j := j;
         AddMove(MoveList, Move);
         end;
      end;
   SetTrueLength(MoveList);
   end;


// Make a MoveList for the Solution
procedure MakeMoveList(
   var   MoveList :  TMoveList;
   const Solution :  TSolution;
         Level    :  Integer     =  1);
   begin
   InitMoveList(MoveList);
   if Level = 1 then
      AssignMoveList(MoveList, gMoveList);
   end;


// Apply a Move to the Solution
procedure PerformMove(
   var   Solution :  TSolution;
   const Move     :  TMove);
         overload;
   var
         Temp     :  Integer;
   begin
   with Solution, Move do
      begin
      Temp := A[i];
      SetQueen(Solution, i, A[j]);
      SetQueen(Solution, j, Temp);
      end;
   end;


// Apply a Move to the Solution, save Undo
procedure PerformMove(
   var   Solution :  TSolution;
   var   Undo     :  TMoveUndo;
   const Move     :  TMove);
         overload;
   begin
   Undo := Move;
   PerformMove(Solution, Move);
   end;


// Make a random Move for the Solution
procedure GetRandomMove(
   var   Move     :  TMove;
   const Solution :  TSolution);
   begin
   with Move do
      repeat
         i := Random(Size);
         j := Random(Size);
      until i <> j;
   end;


// Undo the last move applied to Solution
procedure UndoMove(
   var   Solution :  TSolution;
   const Undo     :  TMoveUndo);
   begin
   PerformMove(Solution, Undo);
   end;


// Make a MoveList for the Local Search. Level determines the neighborhood size,
// with 1 being the smallest.
procedure MakeLSMoveList(
   var   MoveList :  TMoveList;
   const Solution :  TSolution;
         Level    :  Integer);
   begin
   MakeMoveList(MoveList, Solution, Level);
   end;

{-----------------------<< Simulated Annealing >>-----------------------------------}

// Apply a random move to the Solution at temperature T, save Undo
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
   PerformMove(Solution, Undo);
   end;

{-----------------------<< Genetic Algorithm >>-------------------------------------}

// Apply a mutation operator to the Solution
procedure Mutate(
   var   Solution :  TSolution);
   var
         Move     :  TMove;
   begin
   GetRandomMove(Move, Solution);
   PerformMove(Solution, Move);
   end;


// Apply a crossover operator to Parent1 and Parent2 to obtain a Child. Recalc
// indicates whether a score recalculation is strictly necessary.
procedure Crossover(
   var   Child    :  TSolution;
   const Parent1,
         Parent2  :  TSolution;
         Recalc   :  Boolean);
   begin
   CrossoverCycle(Child.A, Parent1.A, Parent2.A);
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


// Initialize the TabuList, must be called before use
procedure InitTabuList(
   var   TabuList :  TTabuList);
   var
         i, j     :  TCoord;
   begin
   for i := 0 to Size - 1 do
      for j := 0 to Size - 1 do
         Tabulist[i, j] := 0;
   end;


// Age the TabuList
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


// Add a Move that will be applied to Sol to the TabuList with Tenure
procedure AddToTabuList(
   var   TabuList :  TTabuList;
   const Move     :  TMove;
         Tenure   :  Integer;
   const Sol      :  TSolution);
   begin
   TabuList[ Move.i, Sol.A[Move.i] ] := Tenure;
   TabuList[ Move.j, Sol.A[Move.j] ] := Tenure;
   end;


// Make a MoveList for the tabu search
procedure MakeTSMoveList(
   var   MoveList :  TMoveList;
   const Solution :  TSolution);
   begin
   MakeMoveList(MoveList, Solution);
   end;


// Return whether a Move is in the Solution's TabuList 
function IsMoveTabu(
   const Move     :  TMove;
   const Solution :  TSolution;
   const TabuList :  TTabuList
         )        :  Boolean;
   begin
   with Solution, Move do
      Result := (TabuList[i, A[j]] <> 0) or
                (TabuList[j, A[i]] <> 0);
   end;


// Return a tabu tenure at normalized time t
function TabuTenure(
         t  :  Real     // [0 .. 1]
         )  :  Integer;
   begin
   Result := 5;
   end;

initialization //////////////////////////////////////////////////////////////////////
PrecalcMoveList(gMoveList);
end.
