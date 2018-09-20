{ 
Copyright (c) Peter Karpov 2010 - 2017.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit Sorting; ///////////////////////////////////////////////////////////////////////
{
>> Version: 2.0

>> Description
   Universal implementations of insertion sort and Shellsort plus predefined routines 
   for integer and real arrays. Shellsort gap sequence taken from RosettaCode.com.
   Part of InvLibs unit collection.
   
>> Usage
   Call a sorting procedure with a pointer to your data, pointers to comparison and
   swap procedures (see definitions), number of records to sort and the desired 
   order.
   
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> ToDo
   ? Quicksort
   ? Versions that accept a scalar value function instead of a comparison function

>> Changelog
   2.0   : 2017.11.27   ~ Renamed TSortOrder values
                        - RealArrayCompare function removed from the interface
                        - WrongOrder function replaced with an array
                        ~ Rewrote sorting using Shellsort single pass functions
                        + DeleteDuplicates procedures
                        ~ Comments cleanup
                        ~ FreePascal compatibility
   1.7   : 2015.06.26   + OrderRealArray procedure
   1.6   : 2014.10.11   + OrderIntArray procedure
   1.5.3 : 2011.10.14   ~ Moved to generic types
                        * SortInt fixed
   1.5.1 : 2011.07.27   * Sort procedure fixed
   1.5   : 2011.06.14   ~ 'Array sorting' section rewrite
   1.4   : 2011.03.27   + Added insertion sort
                        * Fixed SortOrder
   1.3   : 2011.03.18   ~ SimpleStruct 1.4 compatibility
                        * Fixed SortOrder
   1.0   : 2010.11.14   ~ Changed combsort to Shellsort with a good gap sequence
                        + Added procedure that returns element ordering, but doesn't
                          actually sort the data
   0.0   : 2010.05.05   + Initial version, basic combsort
   Notation: + added, - removed, * fixed, ~ changed
}
interface ///////////////////////////////////////////////////////////////////////////
uses
      Arrays;

type
      TSortOrder = (soAscending, soDescending);

      TComparison = (cmpLess, cmpGreater, cmpEqual);

      ProcCompare =
         function(
               ptrData  :  Pointer;
               i, j     :  Integer
               )        :  TComparison;

      ProcSwap =
         procedure(
               ptrData  :  Pointer;
               i, j     :  Integer);

{-----------------------<< Shell sort >>--------------------------------------------}

// Sort the data of length N located at ptrData using Compare and Swap routines via 
// Shellsort in the specified Order
procedure Sort(
         ptrData     :  Pointer;
         Compare     :  ProcCompare;
         Swap        :  ProcSwap;
         N           :  Integer;
         Order       :  TSortOrder);

// Return the Ordering of the elements in the array of length N located at ptrData 
// sorted via Shellsort in the specified Order
procedure SortOrder(
   var   Ordering    :  TIntArray;
         ptrData     :  Pointer;
         Compare     :  ProcCompare;
         N           :  Integer;
         Order       :  TSortOrder);

{-----------------------<< Insertion sort >>----------------------------------------}

// Sort the data of length N located at ptrData using Compare and Swap routines via 
// isertion sort in the specified Order
procedure InsertionSort(
         ptrData     :  Pointer;
         Compare     :  ProcCompare;
         Swap        :  ProcSwap;
         N           :  Integer;
         Order       :  TSortOrder);

// Return the Ordering of the elements in the array of length N located at ptrData 
// sorted via isertion sort in the specified Order
procedure InsertionSortOrder(
   var   Ordering    :  TIntArray;
         ptrData     :  Pointer;
         Compare     :  ProcCompare;
         N           :  Integer;
         Order       :  TSortOrder);         

{-----------------------<< Predefined Sorting Procedures >>-------------------------}

// Sort A according to Order
procedure SortRealArray(
   var   A     :  TRealArray;
         Order :  TSortOrder);

procedure SortRealArrayN(
   var   A     :  TRealArrayN;
         Order :  TSortOrder);

procedure SortIntArray(
   var   A     :  TIntArray;
         Order :  TSortOrder);

procedure SortIntArrayN(
   var   A     :  TIntArrayN;
         Order :  TSortOrder);

// Return the Ordering of elements of A sorted according to Order
procedure OrderIntArray(
   var   Ordering :  TIntArray;
   const A        :  TIntArray;
         Order    :  TSortOrder);

procedure OrderRealArray(
   var   Ordering :  TIntArray;
   const A        :  TRealArray;
         Order    :  TSortOrder);
         
{-----------------------<< Duplicate removal >>-------------------------------------}
         
// Return all distinct elements of A sorted in ascending order
procedure DeleteDuplicates(
   var   A     :  TIntArray);
         overload;
         
procedure DeleteDuplicates(
   var   A     :  TIntArrayN);
         overload;

implementation //////////////////////////////////////////////////////////////////////

const
   NGaps = 24;
   Gaps : array [0 .. NGaps - 1] of Integer = (
      1, 3, 7, 17, 43, 107, 269, 673, 1693, 4231, 10567, 26417, 66041, 165103,
      412771, 1031923, 2579807, 6449537, 16123841, 40309589, 100773961, 251934929,
      629837311, 1574593277);
      
   WrongCmp : array [TSortOrder] of TComparison = (cmpGreater, cmpLess);

{-----------------------<< Shell sort >>--------------------------------------------}

// Perform a single Shellsort pass with given Gap and Order over the data of length N
// located at ptrData using Compare and Swap routines
procedure ShellsortPass(
         ptrData     :  Pointer;
         Compare     :  ProcCompare;
         Swap        :  ProcSwap;
         N, Gap      :  Integer;
         Order       :  TSortOrder);
   var
         i, j        :  Integer;
   begin
   for i := Gap to N - 1 do
      begin
      j := i;
      while (j >= Gap) and
            (Compare(ptrData, j - Gap, j) = WrongCmp[Order]) do
         begin
         Swap(ptrData, j, j - Gap);
         j := j - Gap;
         end;
      end;
   end;
   

// Perform a single Shellsort pass with given Gap and Order over the Ordering of data
// of length N located at ptrData using Compare function
procedure ShellsortPassOrder(
   var   Ordering    :  TIntArray;
         ptrData     :  Pointer;
         Compare     :  ProcCompare;
         N, Gap      :  Integer;
         Order       :  TSortOrder);
   var
         i, j        :  Integer;
         Temp        :  Integer;
   begin
   for i := Gap to N - 1 do
      begin
      j := i;
      Temp := Ordering[i];
      while (j >= Gap) and
            (Compare(ptrData, Ordering[j - Gap], Temp) = WrongCmp[Order]) do
         begin
         Ordering[j] := Ordering[j - Gap];
         j := j - Gap;
         end;
      Ordering[j] := Temp;
      end;
   end;
   

// Sort the data of length N located at ptrData using Compare and Swap routines via 
// Shellsort in the specified Order
procedure Sort(
         ptrData     :  Pointer;
         Compare     :  ProcCompare;
         Swap        :  ProcSwap;
         N           :  Integer;
         Order       :  TSortOrder);
   var
         k, Gap      :  Integer;
   begin
   k := NGaps;
   repeat
      Dec(k);
      Gap := Gaps[k];
      ShellsortPass(ptrData, Compare, Swap, N, Gap, Order);
   until Gap = 1;
   end;


// Return the Ordering of the elements in the array of length N located at ptrData 
// sorted via Shellsort in the specified Order
procedure SortOrder(
   var   Ordering    :  TIntArray;
         ptrData     :  Pointer;
         Compare     :  ProcCompare;
         N           :  Integer;
         Order       :  TSortOrder);
   var
         k, Gap      :  Integer;
   begin
   SequentialFill(Ordering, N, {Base:} 0);
   k := NGaps;
   repeat
      Dec(k);
      Gap := Gaps[k];
      ShellsortPassOrder(Ordering, ptrData, Compare, N, Gap, Order);
   until Gap = 1;
   end;

{-----------------------<< Insertion sort >>----------------------------------------}

// Sort the data of length N located at ptrData using Compare and Swap routines via 
// isertion sort in the specified Order
procedure InsertionSort(
         ptrData     :  Pointer;
         Compare     :  ProcCompare;
         Swap        :  ProcSwap;
         N           :  Integer;
         Order       :  TSortOrder);
   begin
   ShellsortPass(ptrData, Compare, Swap, N, {Gap:} 1, Order);
   end;


// Return the Ordering of the elements in the array of length N located at ptrData 
// sorted via isertion sort in the specified Order
procedure InsertionSortOrder(
   var   Ordering    :  TIntArray;
         ptrData     :  Pointer;
         Compare     :  ProcCompare;
         N           :  Integer;
         Order       :  TSortOrder);
   begin
   ShellsortPassOrder(Ordering, ptrData, Compare, N, {Gap:} 1, Order);
   end;

{-----------------------<< Predefined Compare and Swap Routines>>-------------------}
// #COPYPASTA

// Compare i-th and j-th elements of a real array located at ptrData 
function RealArrayCompare(
         ptrData  :  Pointer;
         i, j     :  Integer
         )        :  TComparison;
   var
         ptrArray :  PRealArray;
         x1, x2   :  Real;
   begin
   ptrArray := PRealArray(ptrData);
   x1 := ptrArray^[i];
   x2 := ptrArray^[j];
   if      x1 > x2 then
      Result := cmpGreater
   else if x1 < x2 then
      Result := cmpLess
   else
      Result := cmpEqual;
   end;


// Swap i-th and j-th elements of a real array located at ptrData 
procedure RealArraySwap(
         ptrData  :  Pointer;
         i, j     :  Integer);
   var
         ptrArray :  PRealArray;
         Temp     :  Real;
   begin
   ptrArray := PRealArray(ptrData);
   Temp         := ptrArray^[i];
   ptrArray^[i] := ptrArray^[j];
   ptrArray^[j] := Temp;
   end;


// Compare i-th and j-th elements of an integer array located at ptrData
function IntArrayCompare(
         ptrData  :  Pointer;
         i, j     :  Integer
         )        :  TComparison;
   var
         ptrArray :  PIntArray;
         x1, x2   :  Integer;
   begin
   ptrArray := PIntArray(ptrData);
   x1 := ptrArray^[i];
   x2 := ptrArray^[j];
   if      x1 > x2 then
      Result := cmpGreater
   else if x1 < x2 then
      Result := cmpLess
   else
      Result := cmpEqual;
   end;


// Swap i-th and j-th elements of an integer array located at ptrData
procedure IntArraySwap(
         ptrData  :  Pointer;
         i, j     :  Integer);
   var
         ptrArray :  PIntArray;
         Temp     :  Integer;
   begin
   ptrArray := PIntArray(ptrData);
   Temp         := ptrArray^[i];
   ptrArray^[i] := ptrArray^[j];
   ptrArray^[j] := Temp;
   end;

{-----------------------<< Predefined Sorting Procedures >>-------------------------}

// Sort A according to Order
procedure SortRealArray(
   var   A     :  TRealArray;
         Order :  TSortOrder);
   begin
   Sort(@A, RealArrayCompare, RealArraySwap, Length(A), Order);
   end;


procedure SortRealArrayN(
   var   A     :  TRealArrayN;
         Order :  TSortOrder);
   begin
   Sort(@A._, RealArrayCompare, RealArraySwap, A.N, Order);
   end;


procedure SortIntArray(
   var   A     :  TIntArray;
         Order :  TSortOrder);
   begin
   Sort(@A, IntArrayCompare, IntArraySwap, Length(A), Order);
   end;


procedure SortIntArrayN(
   var   A     :  TIntArrayN;
         Order :  TSortOrder);
   begin
   Sort(@A._, IntArrayCompare, IntArraySwap, A.N, Order);
   end;


// Return the Ordering of elements of A sorted according to Order
procedure OrderIntArray(
   var   Ordering :  TIntArray;
   const A        :  TIntArray;
         Order    :  TSortOrder);
   begin
   SortOrder(Ordering, @A, IntArrayCompare, Length(A), Order);
   end;


procedure OrderRealArray(
   var   Ordering :  TIntArray;
   const A        :  TRealArray;
         Order    :  TSortOrder);
   begin
   SortOrder(Ordering, @A, RealArrayCompare, Length(A), Order);
   end;
   
{-----------------------<< Duplicate removal >>-------------------------------------}
   
// Return all distinct elements of A sorted in ascending order
procedure DeleteDuplicates(
   var   A        :  TIntArray);
         overload;
   var
         N, i     :  Integer;
   begin
   SortIntArray(A, soAscending);
   N := 1;
   for i := 1 to Length(A) - 1 do
      if A[i] <> A[i - 1] then
         begin
         A[N] := A[i];
         Inc(N);
         end;
   SetLength(A, N);
   end;
   

procedure DeleteDuplicates(
   var   A     :  TIntArrayN);
         overload;
   begin
   SetTrueLength(A);
   DeleteDuplicates(A._);
   A.N := Length(A._);
   end;

end.
