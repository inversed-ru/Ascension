{ 
Copyright (c) Peter Karpov 2010 - 2019.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit SolutionLists; /////////////////////////////////////////////////////////////////
{
>> Version: 0.6

>> Description
   TSolutionList type and related routines
   
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> ToDo
    - Merge AddSolution and AppendSolution into one procedure with RecalcRanking flag
    - Suitable data structure for rank <-> index conversions
    - Optimize ClosestScoreIndex by using binary search

>> Changelog
   0.6 : 2019.10.24  + IndexRank field, updated by most routines affecting RankIndex
                     + ClosestScoreIndex function
   0.5 : 2018.08.23  * Range check error in RandSolIndex
                     ~ AverageDistance function now has an extra ToBest argument
                     + Fitness uniform selection
                     ~ FreePascal compatibility
   0.4 : 2012.12.18  + AppendSolution procedure
   0.3 : 2012.03.08  - Score skewness statistic
   0.0 : 2011.06.08  + Initial version
   Notation: + added, - removed, * fixed, ~ changed  
}
interface ///////////////////////////////////////////////////////////////////////////
uses
      Arrays,
      Problem,
      Messages;
type
      TSolutionIndex = Integer;

      TSolutionList =
         record
         N              :  TSolutionIndex;
         _              :  TSolutions;
          BestIndex,
         WorstIndex     :  TSolutionIndex;
         RankIndex,
         IndexRank      :  TIntArray;
         Best           :  TSolution;
         end;
      PSolutionList = ^TSolutionList;

{-----------------------<< Basic operations >>--------------------------------------}

// Initialize SolList, must be called before use. Solutions are not initialized.
procedure InitSolList(
   var   SolList  :  TSolutionList;
         N        :  TSolutionIndex = 0);

// Create SolList of size N filled with new solutions and set the ranking.
procedure NewSolList(
   var   SolList  :  TSolutionList;
         N        :  TSolutionIndex);

// Fill SolList with N copies of Solution, set the ranking.
procedure FillSolList(
   var   SolList  :  TSolutionList;
   const Solution :  TSolution;
         N        :  TSolutionIndex);         

// Assign SLFrom to SLTo
procedure AssignSolList(
   var   SLTo     :  TSolutionList;
   const SLFrom   :  TSolutionList);         

{-----------------------<< Ranking >>-----------------------------------------------}

// Sort the solutions from SolList in descending order. Positions of actual solutions
// are not changed, but RankIndex, IndexRank, BestIndex, WorstIndex, Best fields are 
// updated. InsSort specifies whether insertion sort should be used, which might be
// faster when the changes are small.
procedure SetRanking(
   var   SolList  :  TSolutionList;
         InsSort  :  Boolean = False);

// Sort the solutions from SolList in descending order. Positions of the solutions
// in the list are changed and the ranking information is updated.
procedure SortSolList(
   var   SolList  :  TSolutionList);

// Test the validity of SolList's ranking
procedure TestSolList(
   const SolList  :  TSolutionList);

{-----------------------<< Selection >>---------------------------------------------}

// Return the index of a random solution distributed according to a rank power law
// with parameter P
function RandSolIndex(
   const SolList     :  TSolutionList;
         P           :  Real
         )           :  Integer;

// Return two indices of distinct random solutions distributed according to a 
// rank power law with parameter P
procedure TwoRandSolIndices(
   var   i1, i2   :  TSolutionIndex;
   const SolList  :  TSolutionList;
         P        :  Real);

// Return the index of a solution selected using fitness uniform selection
function RandFitUniformIndex(
   const SolList     :  TSolutionList
         )           :  Integer;

// Return two indices of distinct random solutions using fitness uniform selection
procedure TwoRandFitUniformIndices(
   var   i1, i2   :  TSolutionIndex;
   const SolList  :  TSolutionList);
   
// Return the index of a solution from SolList with the closest score
// to the specified value
function ClosestScoreIndex(
   const SolList     :  TSolutionList;
         Score       :  Real
         )           :  Integer;

{-----------------------<< Lists operations >>--------------------------------------}

// Fill SLTo with N best solutions from SLFrom, in order from best to worst
procedure CopyBest(
   var   SLTo     :  TSolutionList;
   const SLFrom   :  TSolutionList;
         N        :  Integer);

// Append solutions from AddList to SolList, recalculate the ranking
procedure AppendSolList(
   var   SolList  :  TSolutionList;
   const AddList  :  TSolutionList);

{-----------------------<< Resizing >>----------------------------------------------}

// Resize the solution list. The ranking is left unchanged.
procedure Resize(
   var   SolList  :  TSolutionList;
         Size     :  Integer);

// Increment the length of SolList. A new element pointing to the new empty solution 
// is added to RankIndex.
procedure IncLen(
   var   SolList  :  TSolutionList);

// Leave only best NewN solutions in SolList, recalculate the ranking.
// Solutions themselves are ordered from best to worst.
procedure TrimSolList(
   var   SolList  :  TSolutionList;
         NewN     :  Integer);

{-----------------------<< Add, delete, replace >>----------------------------------}

// Add Sol to SolList, recalculate the ranking. If Sol is a solution from SolList,
// use CopySolution instead.
procedure AddSolution(
   var   SolList   :  TSolutionList;
   const Sol       :  TSolution);

// Append Sol to SolList. The ranking is left unchanged.
procedure AppendSolution(
   var   SolList   :  TSolutionList;
   const Sol       :  TSolution);

// Make a duplicate of SolList's ith solution, recalculate the ranking.
procedure CopySolution(
   var   SolList  :  TSolutionList;
         i        :  Integer);

// Try adding Sol to SolList without exceeding NMax solutions. If there is free 
// space, Sol is added, otherwise the worst solution is replaced if Sol is better. 
// If the addition was sucessfull, the ranking is recalculated and Success is 
// returned, otherwise SolList is left unchanged and Fail is returned.
function TryAddSolution(
   var   SolList  :  TSolutionList;
   const Sol      :  TSolution;
         NMax     :  TSolutionIndex
         )        :  Boolean;

// Try replacing a solution from SolList specified by Index with Sol, update the
// ranking. When IfBetter = True, replacement only occurs when Sol is better,
// otherwise it is unconditional. The result indicates whether the replacement took 
// place. A message about new best score is displayed via ShowMessage.
function ReplaceSolution(
   var   SolList     :  TSolutionList;
         Index       :  TSolutionIndex;
   const Sol         :  TSolution;
         IfBetter    :  Boolean;
         ShowMessage :  ProcMessage
         )           :  Boolean;

{-----------------------<< Statistics >>--------------------------------------------}

// Extract the Scores of solutions in SolList
procedure ExtractScores(
   var   Scores   :  TRealArray;
   const SolList  :  TSolutionList;
         Sorted   :  Boolean);
         
// Return the Mean and the standard deviation SD of the solution scores from SolList.
procedure ScoreStats(
   var   Mean, SD    :  Real;
   const SolList     :  TSolutionList);
         overload;

// Return the Mean of the solution scores from SolList.
procedure ScoreStats(
   var   Mean        :  Real;
   const SolList     :  TSolutionList);
         overload;         

// Return the average distance between the solutions in SolList, or the 
// average distance to the best solution if ToBest is True. Takes into account only
// N - 1 solution pairs.
function AverageDistance(
   const SolList     :  TSolutionList;
         ToBest      :  Boolean = False
         )           :  Real;

// Return the median distance between solutions in SolList.
// Takes into account only N - 1 solution pairs.
function MedianDistance(
   const SolList     :  TSolutionList
         )           :  Real;

implementation //////////////////////////////////////////////////////////////////////
uses
      InvSys,
      Math,
      Statistics,
      Sorting,
      RandVars,
      Common;

{-----------------------<< Basic operations >>--------------------------------------}

// Initialize SolList, must be called before use. Solutions are not initialized.
procedure InitSolList(
   var   SolList  :  TSolutionList;
         N        :  TSolutionIndex = 0);
   begin
   SolList.N := N;
   SetLength(SolList._,          N);
   SetLength(SolList.RankIndex,  N);
   SetLength(SolList.IndexRank,  N);
   end;


// Create SolList of size N filled with new solutions and set the ranking.
procedure NewSolList(
   var   SolList  :  TSolutionList;
         N        :  TSolutionIndex);
   var
         i        :  TSolutionIndex;
   begin
   InitSolList(SolList, N);
   for i := 0 to N - 1 do
      NewSolution(SolList._[i]);
   SetRanking(SolList);
   end;


// Fill SolList with N copies of Solution, set the ranking.
procedure FillSolList(
   var   SolList  :  TSolutionList;
   const Solution :  TSolution;
         N        :  TSolutionIndex);
   var
         i        :  TSolutionIndex;
   begin
   InitSolList(SolList, N);
   for i := 0 to N - 1 do
      AssignSolution(SolList._[i], Solution);
   SetRanking(SolList);
   end;


// Assign SLFrom to SLTo
procedure AssignSolList(
   var   SLTo     :  TSolutionList;
   const SLFrom   :  TSolutionList);
   var
         i        :  Integer;
   begin
   SLTo. BestIndex := SLFrom. BestIndex;
   SLTo.WorstIndex := SLFrom.WorstIndex;
   SLTo.RankIndex  := Copy(SLFrom.RankIndex);
   SLTo.IndexRank  := Copy(SLFrom.IndexRank);
   AssignSolution(SLTo.Best, SLFrom.Best);
   InitSolList(SLTo, SLFrom.N);
   for i := 0 to SLTo.N - 1 do
      AssignSolution(SLTo._[i], SLFrom._[i]);
   end;

{-----------------------<< Ranking >>-----------------------------------------------}

// Solution comparison function for use in sorting.
function CompareSolutions(
         ptrData  :  Pointer;
         i1, i2   :  Integer
         )        :  TComparison;
   var
         ptrSols  :  PSolutions;
   begin
   ptrSols := PSolutions(ptrData);
   case CompareScores(ptrSols^[i1], ptrSols^[i2]) of
      scoreBetter : Result := cmpGreater;
      scoreWorse  : Result := cmpLess;
      scoreEqual  : Result := cmpEqual;
      else
         begin
         Assert(False, 'Unknown score comparison result');
         Result := cmpEqual;
         end;
      end;
   end;


// Solution swap procedure for use in sorting.
procedure SwapSolutions(
         ptrData  :  Pointer;
         i1, i2   :  Integer);
         overload;
   var
         ptrSols  :  PSolutions;
         Temp     :  TSolution;
   begin
   ptrSols := PSolutions(ptrData);
   AssignSolution(Temp,         ptrSols^[i1]);
   AssignSolution(ptrSols^[i1], ptrSols^[i2]);
   AssignSolution(ptrSols^[i2], Temp);
   end;


// Swap ith and jth solutions in SolList. No rank recalculation.
procedure SwapSolutions(
   var   SolList  :  TSolutionList;
         i, j     :  Integer);
         overload;
   var
         Temp     :  TSolution;
   begin
   if i <> j then
      begin
      AssignSolution(Temp,         SolList._[i]);
      AssignSolution(SolList._[i], SolList._[j]);
      AssignSolution(SolList._[j], Temp);
      end;
   end;


// Sort the solutions from SolList in descending order. Positions of actual solutions
// are not changed, but RankIndex, IndexRank, BestIndex, WorstIndex, Best fields are 
// updated. InsSort specifies whether insertion sort should be used, which might be
// faster when the changes are small.
procedure SetRanking(
   var   SolList  :  TSolutionList;
         InsSort  :  Boolean = False);
   var
         i, N     :  Integer;
   begin
   if SolList.N <> 0 then with SolList do
      begin
      // Calculate the ranking
      if InsSort then
         InsertionSortOrder(
            RankIndex, @SolList._, CompareSolutions, N, soDescending)
      else
         SortOrder(
            RankIndex, @SolList._, CompareSolutions, N, soDescending);
      
      // Fill index to rank array
      if Length(IndexRank) <> N then
         SetLength(IndexRank, N);
      for i := 0 to N - 1 do
         IndexRank[RankIndex[i]] := i;
         
      // Update extra fields
       BestIndex := RankIndex[0];
      WorstIndex := RankIndex[N - 1];
      AssignSolution(Best, SolList._[BestIndex]);
      end;
   end;


// Sort the solutions from SolList in descending order. Positions of the solutions
// in the list are changed and the ranking information is updated.
procedure SortSolList(
   var   SolList  :  TSolutionList);
   var
         i        :  Integer;
   begin
   if SolList.N <> 0 then with SolList do
      begin
      Sort(@SolList._, CompareSolutions, SwapSolutions, N, soDescending);
       BestIndex := 0;
      WorstIndex := N - 1;
      AssignSolution( Best, SolList._[0] );
      for i := 0 to N - 1 do
         begin
         RankIndex[i] := i;
         IndexRank[i] := i;
         end;
      end;
   end;


// Test the validity of SolList's ranking
procedure TestSolList(
   const SolList  :  TSolutionList);
   var
         i        :  Integer;
         Used     :  TBoolArray;
   begin
   with SolList do
      begin
      // Test best and worst indices
      Assert(BestIndex  = RankIndex[    0]);
      Assert(WorstIndex = RankIndex[N - 1]);

      // Check that ranking is a permutation
      SetLength(Used, N);
      for i := 0 to N - 1 do
         Used[i] := False;
      for i := 0 to N - 1 do
         begin
         Assert(not Used[ RankIndex[i] ], 'Ranking is not a permutation');
         Used[ RankIndex[i] ] := True;
         end;

      // Check that ranking specifies sorted solutions
      for i := 0 to N - 2 do
         Assert(
            CompareScores(
               SolList._[ RankIndex[i    ] ],
               SolList._[ RankIndex[i + 1] ]
               ) <> scoreWorse,
            'Invalid ordering');
      end;
   end;

{-----------------------<< Selection >>---------------------------------------------}

// Return the index of a random solution distributed according to a rank power law
// with parameter P
function RandSolIndex(
   const SolList     :  TSolutionList;
         P           :  Real
         )           :  Integer;
   var
         x           :  Real;
   begin
   with SolList do
      begin
      if P >= 0 then
         x := 1 - Power( Random, 1 / ( P + 1) ) else
         x :=     Power( Random, 1 / (-P + 1) );
      Result := RankIndex[ Min(Floor(x * N), N - 1) ]; 
      end;
   end;


// Return two indices of distinct random solutions distributed according to a 
// rank power law with parameter P
procedure TwoRandSolIndices(
   var   i1, i2   :  TSolutionIndex;
   const SolList  :  TSolutionList;
         P        :  Real);
   begin
   i1 := RandSolIndex(SolList, P);
   repeat
      i2 := RandSolIndex(SolList, P);
   until i2 <> i1;
   end;

   
// Return the index of a solution from SolList with the closest score
// to the specified value
// #HACK Slow, should use binary search. Fitness-uniform selection requires
// random tiebreaks, but this is no big deal since FUS is terrible
function ClosestScoreIndex(
   const SolList     :  TSolutionList;
         Score       :  Real
         )           :  Integer;
   var
         dS          :  TRealArray;
         i           :  Integer;
   begin
   SetLength(dS, SolList.N);
   for i := 0 to SolList.N - 1 do
      dS[i] := Abs(Score - SolList._[i].Score);
   Result := RandMinIndex(dS);
   end;
   

// Return the index of a solution selected using fitness uniform selection
function RandFitUniformIndex(
   const SolList     :  TSolutionList
         )           :  Integer;
   var
         S           :  Real;
   begin
   with SolList do
      begin
      S := RandUniform(
         SolList._[WorstIndex].Score,
         SolList._[ BestIndex].Score);
      Result := ClosestScoreIndex(SolList, S);
      end;
   end;


// Return two indices of distinct random solutions using fitness uniform selection
procedure TwoRandFitUniformIndices(
   var   i1, i2   :  TSolutionIndex;
   const SolList  :  TSolutionList);
   begin
   i1 := RandFitUniformIndex(SolList);
   repeat
      i2 := RandFitUniformIndex(SolList);
   until i2 <> i1;
   end;

{-----------------------<< Lists operations >>--------------------------------------}

// Fill SLTo with N best solutions from SLFrom, in order from best to worst
procedure CopyBest(
   var   SLTo     :  TSolutionList;
   const SLFrom   :  TSolutionList;
         N        :  Integer);
   var
         i        :  Integer;
   begin
   // Copy solutions
   InitSolList(SLTo, N);
   for i := 0 to N - 1 do
      begin
      AssignSolution(SLTo._[i], SLFrom._[ SLFrom.RankIndex[i] ]);
      SLTo.RankIndex[i] := i;
      SLTo.IndexRank[i] := i;
      end;
      
   // Update ranking information
   if N <> 0 then with SLTo do
      begin
      BestIndex  := 0;
      WorstIndex := N - 1;
      AssignSolution(Best, SLTo._[0]);
      end;
   end;


// Append solutions from AddList to SolList, recalculate the ranking
procedure AppendSolList(
   var   SolList  :  TSolutionList;
   const AddList  :  TSolutionList);
   var
         i, OldN  :  Integer;
   begin
   with SolList do
      begin
      OldN := N;
      Inc(N, AddList.N);
      SetLength(SolList._, N);
      for i := OldN to N - 1 do
         AssignSolution(SolList._[i], AddList._[i - OldN]);
      end;
   SetRanking(SolList);
   end;

{-----------------------<< Resizing >>----------------------------------------------}

// Resize the solution list. The ranking is left unchanged.
procedure Resize(
   var   SolList  :  TSolutionList;
         Size     :  Integer);
   begin
   SolList.N := Size;
   SetLength(SolList._, Size);
   end;


// Increment the length of SolList. A new element pointing to the new empty solution 
// is added to RankIndex and IndexRank.
procedure IncLen(
   var   SolList  :  TSolutionList);
   begin
   with SolList do
      begin
      Inc(N);
      if N > Length(SolList._) then
         SetLength (SolList._, 2 * N);
      SetLength(RankIndex, N);
      SetLength(IndexRank, N);
      RankIndex[N - 1] := N - 1;
      IndexRank[N - 1] := N - 1;
      end;
   end;


// Leave only best NewN solutions in SolList, recalculate the ranking.
// Solutions themselves are ordered from best to worst.
procedure TrimSolList(
   var   SolList  :  TSolutionList;
         NewN     :  Integer);
   var
         Best     :  TSolutionList;
   begin
   SetRanking(SolList);
   CopyBest(Best, SolList, NewN);
   AssignSolList(SolList, Best);
   end;

{-----------------------<< Add, delete, replace >>----------------------------------}

// Add Sol to SolList, recalculate the ranking. If Sol is a solution from SolList,
// use CopySolution instead.
procedure AddSolution(
   var   SolList   :  TSolutionList;
   const Sol       :  TSolution);
   begin
   IncLen(SolList);
   AssignSolution(SolList._[SolList.N - 1], Sol);
   SetRanking(SolList, {InsSort:} True);
   end;


// #HACK make single AddSolution procedure with RecalkRanking flag?
// Append Sol to SolList. The ranking is left unchanged.
procedure AppendSolution(
   var   SolList   :  TSolutionList;
   const Sol       :  TSolution);
   begin
   with SolList do
      begin
      Inc(N);
      if N > Length(SolList._) then
         SetLength (SolList._, 2 * N);
      AssignSolution(SolList._[N - 1], Sol);
      end;
   end;


// Make a duplicate of SolList's ith solution, recalculate the ranking.
procedure CopySolution(
   var   SolList  :  TSolutionList;
         i        :  Integer);
   begin
   IncLen(SolList);
   AssignSolution(SolList._[SolList.N - 1], SolList._[i]);
   SetRanking(SolList, {InsSort:} True);
   end;


// Try adding Sol to SolList without exceeding NMax solutions. If there is free 
// space, Sol is added, otherwise the worst solution is replaced if Sol is better. 
// If the addition was sucessfull, the ranking is recalculated and Success is 
// returned, otherwise SolList is left unchanged and Fail is returned.
function TryAddSolution(
   var   SolList  :  TSolutionList;
   const Sol      :  TSolution;
         NMax     :  TSolutionIndex
         )        :  Boolean;
   begin
   if NMax = 0 then
      Result := Fail
   else with SolList do
      begin
      if N < NMax then
         begin
         AddSolution(SolList, Sol);
         Result := Success;
         end
      else
         begin
         Result := ReplaceIfBetter(SolList._[WorstIndex], Sol);
         if Result = Success then
            SetRanking(SolList, {InsSort:} True);
         end;
      end;
   end;


// Try replacing a solution from SolList specified by Index with Sol, update the
// ranking. When IfBetter = True, replacement only occurs when Sol is better,
// otherwise it is unconditional. The result indicates whether the replacement took 
// place. A message about new best score is displayed via ShowMessage.
function ReplaceSolution(
   var   SolList     :  TSolutionList;
         Index       :  TSolutionIndex;
   const Sol         :  TSolution;
         IfBetter    :  Boolean;
         ShowMessage :  ProcMessage
         )           :  Boolean;
   begin
   Result := Fail;
   with SolList do
      if (not IfBetter) or (CompareScores(Sol, SolList._[Index]) = scoreBetter) then
         begin
         Result := Success;
         if CompareScores(Sol, Best) = scoreBetter then
            ShowNewBestScore(Sol, ShowMessage);
         AssignSolution(SolList._[Index], Sol);
         SetRanking(SolList, {InsSort:} True);
         end;
   end;

{-----------------------<< Statistics >>--------------------------------------------}

// Extract the Scores of solutions in SolList
procedure ExtractScores(
   var   Scores   :  TRealArray;
   const SolList  :  TSolutionList;
         Sorted   :  Boolean);
   var
         i        :  Integer;
   begin
   SetLength(Scores, SolList.N);
   for i := 0 to SolList.N - 1 do
      if Sorted then
         Scores[i] := SolList._[SolList.RankIndex[i]].Score else
         Scores[i] := SolList._[                  i ].Score;
   end;


// Return the Mean and the standard deviation SD of the solution scores from SolList.
procedure ScoreStats(
   var   Mean, SD    :  Real;
   const SolList     :  TSolutionList);
         overload;
   var
         Scores      :  TRealArray;
   begin
   if SolList.N <> 0 then
      begin
      ExtractScores(Scores, SolList, {Sorted:} False);
      GetMeanStandDev(Mean, SD, Scores);
      end
   else
      begin
      Mean := 0;
      SD   := 0;
      end;
   end;


// Return the Mean of the solution scores from SolList.
procedure ScoreStats(
   var   Mean        :  Real;
   const SolList     :  TSolutionList);
         overload;
   var
         SD          :  Real;
   begin
   ScoreStats(Mean, SD, SolList);
   end;


// Extract distances between the solutions in SolList
procedure ExtractDistances(
   var   Distances   :  TRealArray;
   const SolList     :  TSolutionList;
         ToBest      :  Boolean = False);
   var
         i           :  Integer;
   begin
   with SolList do
      begin
      SetLength(Distances, N - 1);
      for i := 0 to N - 2 do
         if ToBest then
            Distances[i] := Distance(
               SolList._[ RankIndex[0    ] ],
               SolList._[ RankIndex[i + 1] ])
         else
            Distances[i] := Distance(
               SolList._[ RankIndex[i    ] ],
               SolList._[ RankIndex[i + 1] ]);
      end;
   end;


// Return the average distance between the solutions in SolList, or the 
// average distance to the best solution if ToBest is True. Takes into account only
// N - 1 solution pairs.
function AverageDistance(
   const SolList     :  TSolutionList;
         ToBest      :  Boolean = False
         )           :  Real;
   var
         D           :  TRealArray;
   begin
   ExtractDistances(D, SolList, ToBest);
   Result := Mean(D);
   end;


// Return the median distance between the solutions in SolList.
// Takes into account only N - 1 solution pairs.
function MedianDistance(
   const SolList     :  TSolutionList
         )           :  Real;
   var
         D           :  TRealArray;
   begin
   ExtractDistances(D, SolList);
   Result := Median(D);
   end;

end.
