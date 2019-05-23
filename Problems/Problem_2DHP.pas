{ 
Copyright (c) Peter Karpov 2010 - 2019.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit Problem; ///////////////////////////////////////////////////////////////////////
{
>> Version: 0.5

>> Description
   2D lattice hydrophobic-polar protein folding model PDM. In this model, the 
   proteins are represented by sequences of aminoacids confined to a 2D square grid. 
   The aminoacids can be either hydrophobic (H) or polar (P). The goal is to find a 
   fold (a self-avoiding walk) that maximizes the number of H-H contacts. The score 
   is the number of H-H contacts minus the penalty for self-intersections.
   
   Problem-specific constants:
      StrSequence    Protein sequence
   
   Supported algorithms: LS, TS, SA, GA.
   
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> Notes
   A penalty that clearly separates the scores of valid / invalid solutions is
   2 * Nh + 2 (maximal number of h-h contacts). In practice, penalty = 4 seems
   sufficient.

   Pull moves are very effective, but for the sake of simplicity and uniformity
   another class of moves is used.
   
>> References
   - An Improved Ant Colony Optimisation Algorithm for the 2D HP Protein Folding 
     Problem. Alena Shmygelska, Holger H. Hoos.

>> ToDo
    - Profile
    - Use vector / geometry types for coordinates
    - Faster scoring: even offsets are sufficient for checking self-avoidance, 
      odd for checking adjacency 
    - Faster than N^2 scoring ?
    ? Visualization
    - Fix move generation inefficiency: high level moves may include low level ones
    
>> Changelog
   0.5 : 2019.05.21  ~ Renamed IsMinimize to Minimization
                     - Distance reversal invariance
   0.4 : 2018.09.23  ~ FreePascal compatibility
                     - Sections related to algorithms omitted from Ascension 2.0 
   0.3 : 2015.06.26  ~ Distance invariant to inversion and reversal
   0.2 : 2012.02.08  + Guided move
   0.1 : 2011.12.27  + New crossover: rejoin parent parts using random angle and
                       reflection
                     + New move type: change random positions
                     ~ Faster scoring
                     ~ Different penalty for self-crossing
   0.0 : 2011.08.13  + Initial version
   Notation: + added, - removed, * fixed, ~ changed   
}
interface ///////////////////////////////////////////////////////////////////////////
const
      Minimization   =  False;
      FileExtension  =  'txt';
      MaxMoveSize    =  6;
type
      // Basic types
      TScore = Integer;

      TRelDir = (rdL = 0, rdF, rdR);

      TSolution =
         record
         x        :  array of TRelDir;
         Score    :  TScore;
         end;

      TSolutionDistance = Integer;

      // Local search
      TMove =
         record
         Size        :  Integer;
         Pos         :  array [0 .. MaxMoveSize - 1] of Integer;
         NewDir      :  array [0 .. MaxMoveSize - 1] of TRelDir;
         end;

      TMoveUndo = TSolution;
      TSAUndo   = TMoveUndo;

      // Tabu search
      TTabuList = array of array [TRelDir] of Integer;

      // Guided local search
      TGuidePenalty = array of array [TRelDir] of Real;

      // Partitions
      TRegionElement =
         record
         X        :  TRelDir;
         Wildcard :  Boolean;
         end;

      TRegion = array of TRegionElement;

      TRegionSize = Integer;
      
      // Estimation of distribution
      TDistribution = array of array [TRelDir] of Real;

{$I Interface.inc}
implementation //////////////////////////////////////////////////////////////////////
uses
      InvSys,
      Math,
      Arrays,
      Statistics,
      RandVars,
      Common,
      Messages;
type
      TAbsDir     =  (adL, adU, adR, adD);
      TAcid       =  (acidH, acidP, acidNone, acidOverlap);
      TCoords     =  array [0 .. 1] of Integer;
const
      AcidName    :  array [TAcid] of AnsiChar = ('h', 'p', '.', '#');
      // Global optimum = 21
      StrSequence = 'hhphphphphhhhphppphppphpppphppphppphphhhhphphphphh';
var
      LenSeq      :  Integer;
      Sequence    :  array of TAcid;

{-----------------------<< Problem-specific operations >>---------------------------}
const
      DirDx    :  array [TAbsDir] of Integer = (-1,  0,  1,  0);
      DirDy    :  array [TAbsDir] of Integer = ( 0,  1,  0, -1);
      InvDir   :  array [TRelDir] of TRelDir
               =  (rdR, rdF, rdL);

// Move according to AbsDir given the current coordinates.
procedure MoveAbs(
   var   x, y     :  Integer;
   const AbsDir   :  TAbsDir);
   begin
   x := x + DirDx[AbsDir];
   y := y + DirDy[AbsDir];
   end;


// Move according to RelDir given the current coordinates and AbsDir
procedure MoveRel(
   var   x, y     :  Integer;
   var   AbsDir   :  TAbsDir;
   const RelDir   :  TRelDir);
   const
         TurnMap  :  array [TRelDir, TAbsDir] of TAbsDir
                  =  ( (adD, adL, adU, adR),
                       (adL, adU, adR, adD),
                       (adU, adR, adD, adL) );
   begin
   AbsDir := TurnMap[RelDir, AbsDir];
   MoveAbs(x, y, AbsDir);
   end;


// Return a random turn direction
function RandDir  :  TRelDir;
   begin
   Result := TRelDir(Random(3));
   end;
   

// Problem-specific improvement
procedure SpecialImprove(
   var   Solution :  TSolution);
   begin
   Assert(False);
   end;

{-----------------------<< Solution operations >>-----------------------------------}

procedure SetScore(
   var   Solution    :  TSolution);
         forward;

// Assign SolFrom to SolTo
procedure AssignSolution(
   var   SolTo    :  TSolution;
   const SolFrom  :  TSolution);
   begin
   SolTo.X     := Copy(SolFrom.X);
   SolTo.Score :=      SolFrom.Score;
   end;


// Save the Solution to FileSol
procedure SaveSolution(
   var   fileSol     :  Text;
   const Solution    :  TSolution);
   var
         i, x, y,
         MinX, MinY, // #TEMP use Vec2s instead?
         MaxX, MaxY,
         Size        :  Integer;
         Lattice     :  array of array of AnsiChar;
         AbsDir      :  TAbsDir;
   const
         DirName     :  array [TRelDir] of AnsiChar = ('l', 'f', 'r');
   begin
   // Write the solution and the aminoacid sequence
   WriteLn(fileSol, 'Score: ', Solution.Score);
   WriteLn(fileSol, StrSequence);
   for i := 1 to LenSeq - 2 do
      Write(fileSol, DirName[ Solution.X[i] ]);
   WriteLn(fileSol);

   // Clear the lattice
   Size := 2 * (2 * LenSeq + 1);
   SetLength(Lattice, Size, Size);
   for x := 0 to Size - 1 do
      for y := 0 to Size - 1 do
         Lattice[x, y] := ' ';

   // Construct an ASCII lattice
   AbsDir := adR;
   x := 2 * LenSeq;
   y := 2 * LenSeq;
   MinX := x;
   MaxX := x;
   MinY := y;
   MaxY := y;
   i := 0;
   while True do
      begin
      MoveAbs(x, y, AbsDir);
      MinX := Min(x, MinX); MinY := Min(y, MinY);
      MaxX := Max(x, MaxX); MaxY := Max(y, MaxY);
      if Lattice[x, y] <> ' ' then
         Lattice[x, y] := '#'
      else
         Lattice[x, y] := AcidName[ Sequence[i] ];
      if i = (LenSeq - 1) then
   {<}   break;

      MoveRel(x, y, AbsDir, Solution.X[i]);
      if AbsDir in [adL, adR] then
         Lattice[x, y] := '-'
      else
         Lattice[x, y] := '|';
      Inc(i);
      end;

   // Write the lattice
   WriteLn(fileSol);
   for y := MaxY downto MinY do
      begin
      for x := MinX to MaxX do
         Write(fileSol, Lattice[x, y]);
      WriteLn(fileSol);
      end;
   end;


// Load a Solution from FileSol
procedure LoadSolution(
   var   Solution    :  TSolution;
   Var   fileSol     :  Text);
   var
         i           :  Integer;
         Dir         :  AnsiChar;
   begin
   ReadLn(FileSol);
   ReadLn(FileSol);
   with Solution do
      begin
      SetLength(X, LenSeq - 1);
      X[0] := RandDir;
      for i := 1 to LenSeq - 2 do
         begin
         Read(FileSol, Dir);
         case Dir of
            'l':  X[i] := rdL;
            'f':  X[i] := rdF;
            'r':  X[i] := rdR;
            else  Assert(False);
            end;
         end;
      end;
   SetScore(Solution);
   end;

{-----------------------<< Scores >>------------------------------------------------}

{$RANGECHECKS OFF} {$OVERFLOWCHECKS OFF}
// Set Solution's score
procedure SetScore(
   var   Solution    :  TSolution);
   var
         i, j, Dist  :  Integer;
         Coords      :  array of TCoords;
         AbsDir      :  TAbsDir;
   const
         Penalty     =  4;
   begin
   // Initialize
   Solution.Score := 0;
   SetLength(Coords, LenSeq);
   Coords[0, 0] := 0;
   Coords[0, 1] := 0;
   AbsDir := adR;

   // Build a conformation
   for i := 1 to LenSeq - 1 do
      begin
      // Move according to the direction
      Coords[i] := Coords[i - 1];
      MoveRel(Coords[i, 0], Coords[i, 1], AbsDir, Solution.X[i - 1]);

      // Check self-avoidance and neighbouring hydrophobic acids
      for j := 0 to i - 2 do
         begin
         Dist := Sqr(Coords[i, 0] - Coords[j, 0])
               + Sqr(Coords[i, 1] - Coords[j, 1]);
         if Dist = 0 then
            Dec(Solution.Score, Penalty)
         else if (Dist = 1) and (Sequence[i] = acidH)
                            and (Sequence[j] = acidH) then
            Inc(Solution.Score);
         end;
      end;
   end;
{$RANGECHECKS ON} {$OVERFLOWCHECKS ON}


// Return a string representation of Score
function FormatScore(
         Score :  TScore
         )     :  AnsiString;
   begin
   Str(Score, Result);
   end;

{-----------------------<< Heuristic-unspecific >>----------------------------------}

// Create a new random Solution
procedure NewSolution(
   var   Solution :  TSolution);
   var
         i        :  Integer;
   begin
   SetLength(Solution.X, LenSeq - 1);
   for i := 0 to LenSeq - 2 do
      Solution.X[i] := RandDir;
   SetScore(Solution);
   end;


// Create NewSol by inverting Sol's directions
procedure Invert(
   var   NewSol   :  TSolution;
   const Sol      :  TSolution);
   var
         i        :  Integer;
   begin
   AssignSolution(NewSol, Sol);
   for i := 1 to LenSeq - 2 do
      NewSol.X[i] := InvDir[Sol.X[i]];
   end;


// Return the number of mismatching elements between A and B
function HammingDist(
   const A,
         B     :  TSolution
         )     :  TSolutionDistance;
   var
         i     :  Integer;
   begin
   Result := 0;
   for i := 1 to LenSeq - 2 do
      if A.X[i] <> B.X[i] then
         Inc(Result);
   end;


// Return the distance between Solution1 and Solution2
function Distance(
   const Solution1,
         Solution2   :  TSolution
         )           :  TSolutionDistance;
   var
         Inv2        :  TSolution;
   begin
   Invert(Inv2, Solution2);
   Result := Min(HammingDist(Solution1, Solution2), HammingDist(Solution1, Inv2));
   end;

{-----------------------<< Local Search >>------------------------------------------}

// Make a MoveList for the Solution
procedure MakeMoveList(
   var   MoveList    :  TMoveList;
   const Solution    :  TSolution;
         Level       :  Integer);
   var
         i, j, k     :  Integer;
         Move        :  TMove;
         Different   :  Boolean;
   begin
   InitMoveList(MoveList);
   Move.Size := Level;
   with Solution, Move do
      for k := 1 to LenSeq - 1 - Size do
         begin
         // Set sequential change positions
         Pos[0] := k;
         for i := 1 to Size - 1 do
            Pos[i] := Pos[0] + i;

         // Add direction changes
         for i := 0 to Round(IntPower(3, Size) - 1) do
            begin
            Different := False;
            for j := 0 to Size - 1 do
               begin
               NewDir[j] := TRelDir( ( i div Round( IntPower(3, j) ) ) mod 3 );
               if Solution.X[Pos[j]] <> NewDir[j] then
                  Different := True;
               end;
            if Different then
               AddMove(MoveList, Move);
            end;
         end;
   end;


// Apply a Move to the Solution
procedure PerformMove(
   var   Solution :  TSolution;
   const Move     :  TMove);
         overload;
   var
         i        :  Integer;
   begin
   with Move do
      for i := 0 to Size - 1 do
         Solution.X[Pos[i]] := NewDir[i];
   SetScore(Solution);
   end;


// Apply a Move to the Solution, save Undo
procedure PerformMove(
   var   Solution :  TSolution;
   var   Undo     :  TMoveUndo;
   const Move     :  TMove);
         overload;
   begin
   AssignSolution(Undo, Solution);
   PerformMove(Solution, Move);
   end;


// Make a random Move for the Solution
procedure GetRandomMove(
   var   Move        :  TMove;
   const Solution    :  TSolution);
   var
         i, j        :  Integer;
         Different   :  Boolean;
   const
         MaxSize     =  4;
         ProbSeq     =  1 / 2;
   begin
   with Move do
      begin
      Size := RandRange(1, MaxSize);
      if Random < ProbSeq then
         begin
         // Make sequential change positions
         Pos[0] := RandRange(1, LenSeq - Size - 1);
         for i := 1 to Size - 1 do
            Pos[i] := Pos[0] + i;
         end
      else
         // Make random different change positions
         for i := 0 to Size - 1 do
            repeat
               Different := True;
               Pos[i] := RandRange(1, LenSeq - 2);
               for j := 0 to i - 1 do
                  if Pos[i] = Pos[j] then
                     begin
                     Different := False;
               {<}   break;
                     end;
            until Different;

      // Generate new directions
      Different := False;
      repeat
         for i := 0 to Size - 1 do
            begin
            NewDir[i] := RandDir;
            if NewDir[i] <> Solution.X[Pos[i]] then
               Different := True;
            end;
      until Different;
      end;
   end;
   

// Undo the last move applied to Solution
procedure UndoMove(
   var   Solution :  TSolution;
   const Undo     :  TMoveUndo);
   begin
   AssignSolution(Solution, Undo);
   end;
   

// Make a MoveList for the Local Search. Level determines the neighborhood size,
// with 1 being the smallest.
procedure MakeLSMoveList(
   var   MoveList :  TMoveList;
   const Solution :  TSolution;
         Level    :  Integer);
   const
         MaxLevel =  4;
   begin
   InitMoveList(MoveList);
   if Level <= MaxLevel then
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
   AssignSolution(Solution, Undo);
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
// Rejoin parent parts using a random angle and reflection.
procedure Crossover(
   var   Child    :  TSolution;
   const Parent1,
         Parent2  :  TSolution;
         Recalc   :  Boolean);
   var
         i, Cut   :  Integer;
         Invert   :  Boolean;
   begin
   SetLength(Child.x, LenSeq - 1);
   Cut := RandRange(2, LenSeq - 3);
   Invert := RandBool;
   for i := 0 to LenSeq - 2 do
      if i < Cut then
         Child.x[i] :=        Parent1.x[i]
      else if i = Cut then
         Child.x[i] := RandDir
      else if Invert then
         Child.x[i] := InvDir[Parent2.x[i]]
      else
         Child.x[i] :=        Parent2.x[i];
   SetScore(Child);
   end;

{-----------------------<< Tabu Search >>-------------------------------------------}

// Assign ListFrom to ListTo
procedure AssignTabuList(
   var   ListTo   :  TTabuList;
   const ListFrom :  TTabuList);
   begin
   ListTo := Copy(ListFrom);
   end;


// Initialize the TabuList, must be called before use
procedure InitTabuList(
   var   TabuList :  TTabuList);
   var
         i        :  Integer;
         Dir      :  TRelDir;
   begin
   SetLength(TabuList, LenSeq - 1);
   for i := 0 to LenSeq - 2 do
      for Dir := rdL to rdR do
         Tabulist[i, Dir] := 0;
   end;


// Age the TabuList
procedure AgeTabuList(
   var   TabuList :  TTabuList);
   var
         i        :  Integer;
         Dir      :  TRelDir;
   begin
   for i := 0 to LenSeq - 2 do
      for Dir := rdL to rdR do
         if Tabulist[i, Dir] > 0 then
            Dec(Tabulist[i, Dir]);
   end;


// Add a Move that will be applied to Sol to the TabuList with Tenure
procedure AddToTabuList(
   var   TabuList :  TTabuList;
   const Move     :  TMove;
         Tenure   :  Integer;
   const Sol      :  TSolution);
   var
         i        :  Integer;
   begin
   with Move do
      begin
      for i := 0 to Size - 1 do
         if Sol.X[Pos[i]] <> NewDir[i] then
            TabuList[ Pos[i], Sol.X[ Pos[i] ] ] := Tenure;
      end;
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
   var
         i        :  Integer;
   begin
   Result := False;
   with Move do
      for i := 0 to Size - 1 do
         if TabuList[ Pos[i], NewDir[i] ] <> 0 then
            begin
            Result := True;
      {<}   break;
            end;
   end;


// Return a tabu tenure at normalized time t
function TabuTenure(
         t  :  Real     // [0 .. 1]
         )  :  Integer;
   begin
   Result := Round( 3 + 5 * (1 - t) );
   end;

{-----------------------<< Initialization >>----------------------------------------}
var
      i  :  Integer;
      
initialization
// Convert a character sequence to an acid sequence
LenSeq := Length(StrSequence);
SetLength(Sequence, LenSeq);
for i := 0 to LenSeq - 1 do
   case StrSequence[1 + i] of
      'h':  Sequence[i] := acidH;
      'p':  Sequence[i] := acidP;
      else  Assert(False);
      end;
end.
