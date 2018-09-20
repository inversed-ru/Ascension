{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{////////////////////////////////////////////////////////////////////////////////////
>> Version: 1.0

>> Description
   Problem definition module interface

>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru
   
>> Changelog
   1.0 : 2018.09.16  - Interface for algorithms omitted from Ascension 2.0
                     ~ MakeNeighbour is now temperature-aware
   0.4 : 2012.04.24  + Problem-specific improvement
   0.3 : 2012.02.06  + Guided move
   0.2 : 2011.10.14  ~ Moved to generic types
   0.1 : 2011.08.29
   Notation: + added, - removed, * fixed, ~ changed
}
{-----------------------<< Solution operations >>-----------------------------------}
type
      TSolutions = array of TSolution;

// Assign SolFrom to SolTo
procedure AssignSolution(
   var   SolTo    :  TSolution;
   const SolFrom  :  TSolution);

// Save Solution to fileSol
procedure SaveSolution(
   var   fileSol     :  Text;
   const Solution    :  TSolution);

// Load Solution from fileSol
procedure LoadSolution(
   var   Solution    :  TSolution;
   var   fileSol     :  Text);

// Problem-specific improvement
procedure SpecialImprove(
   var   Solution :  TSolution);   

{-----------------------<< Scores >>------------------------------------------------}

// Return a string representation of Score
function FormatScore(
         Score :  TScore
         )     :  AnsiString;   

{-----------------------<< Heuristic-unspecific >>----------------------------------}
      
// Create new random Solution
procedure NewSolution(
   var   Solution :  TSolution);   

// Return distance between Solution1 and Solution2
function Distance(
   const Solution1,
         Solution2   :  TSolution
         )           :  TSolutionDistance;

{-----------------------<< Move lists >>--------------------------------------------}
type
      TMoveList =
         record
         N     :  Integer;
         Moves :  array of TMove;
         end;

// Apply a Move to a Solution
procedure PerformMove(
   var   Solution :  TSolution;
   const Move     :  TMove);
         overload;

// Apply a Move to a Solution, save the Undo
procedure PerformMove(
   var   Solution :  TSolution;
   var   Undo     :  TMoveUndo;
   const Move     :  TMove);
         overload;   

// Undo the last move applied to a Solution
procedure UndoMove(
   var   Solution :  TSolution;
   const Undo     :  TMoveUndo);

{-----------------------<< Local Search >>------------------------------------------}

// Make a MoveList of a Solution for use in Local Search. Level determines
// neighborhood size, with 1 being the smallest.
procedure MakeLSMoveList(
   var   MoveList :  TMoveList;
   const Solution :  TSolution;
         Level    :  Integer); 

{-----------------------<< Simulated Annealing >>-----------------------------------}

// Apply a random move to the Solution at temperature T, save Undo
procedure MakeNeighbour(
   var   Solution :  TSolution;
   var   Undo     :  TSAUndo;
         T        :  Real);

// Undo the last move applied to a Solution
procedure UndoSAMove(
   var   Solution :  TSolution;
   const Undo     :  TSAUndo);  

{-----------------------<< Genetic Algorithm >>-------------------------------------}

// Apply mutation operator to a Solution
procedure Mutate(
   var   Solution :  TSolution);

// Obtain the Child via crossover between Parent1 and Parent2. Recalc indicates if
// score recalculation is strictly necessary.
procedure Crossover(
   var   Child    :  TSolution;
   const Parent1,
         Parent2  :  TSolution;
         Recalc   :  Boolean);

{-----------------------<< Tabu Search >>-------------------------------------------}

// ListTo := ListFrom
procedure AssignTabuList(
   var   ListTo   :  TTabuList;
   const ListFrom :  TTabuList);

// Initialize the TabuList, must be called before use
procedure InitTabuList(
   var   TabuList :  TTabuList);

// Age the TabuList
procedure AgeTabuList(
   var   TabuList :  TTabuList);

// Add a Move that will be applied to Sol to the TabuList with Tenure
procedure AddToTabuList(
   var   TabuList :  TTabuList;
   const Move     :  TMove;
         Tenure   :  Integer;
   const Sol      :  TSolution);
         
// Make a MoveList for the tabu search
procedure MakeTSMoveList(
   var   MoveList :  TMoveList;
   const Solution  :  TSolution);         

// Return whether a Move is in the Solution's TabuList 
function IsMoveTabu(
   const Move     :  TMove;
   const Solution  :  TSolution;
   const TabuList :  TTabuList
         )        :  Boolean;

// Return a tabu tenure at normalized time t
function TabuTenure(
         t  :  Real     // [0 .. 1]
         )  :  Integer;      
