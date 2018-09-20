{ 
Copyright (c) Peter Karpov 2010 - 2017.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit Arrays; ////////////////////////////////////////////////////////////////////////
{
>> Version: 1.2

>> Description
   Basic array routines. Part of InvLibs unit collection.
   
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> ToDo
   - Repeated append-delete operations worst case complexity analysis
   - Do something with all the copypasta
   ? Split "Permutations" section into Combinatorics unit and implement other 
     permutation-related routines
   - See if any operations are bottlenecks in any programs and optimize them

>> Changelog
   1.2   : 2017.12.15   + DistanceL2 function
   1.1   : 2017.12.08   + RandMinIndex and RandMaxIndex functions
   1.0   : 2017.12.02   ~ Renamed the unit to Arrays
                        ~ Renamed AddToArray to Append and AddUnique to AppendUnique
                          to avoid confusion
                        ~ Renamed Concat to AppendArray
                        ~ Renamed array arithmetics functions
                        ~ Split Basic section into Initialization and Resizing
                        - Moved DeleteDuplicates procedures to Sorting unit to
                          untangle dependencies
                        ~ Statistics and Sorting removed from dependencies
                        - Moved RandPerm and ShuffleArray procedures to RandVars unit
                        ~ Comments cleanup
                        ~ FreePascal compatibility
   0.20  : 2015.04.17   ~ Save / Load section reworked
   0.19  : 2015.03.09   + ExtendArray procedures
   0.18  : 2014.10.06   + LoadIntArray procedure
   0.17  : 2014.09.26   + ValueCount function
                        ~ Renamed MarkEqual procedure to MarkValue
   0.16  : 2014.08.22   + Concat procedure
   0.15  : 2013.12.26   + Permute procedure
                        + InvertPerm for in-place permutation inversion
                        ~ Renamed some permutation routines
   0.14  : 2013.06.16   + MarkEqual procedure
   0.13  : 2013.06.09   + InitArray2D procedure
   0.12  : 2013.05.25   + 2D arrays section
   0.11  : 2013.03.26   + ConvertToReal procedure
   0.10  : 2013.02.06   + SubArray procedure
   0.9   : 2013.01.30   ~ Cleaned up types
                        + Array multiply-add procedure
   0.8.3 : 2011.10.14   ~ Moved to generic types
   0.6   : 2011.06.14   - NAlloc field
                        + TBoolArray type
                        + InitArray procedures
                        ~ Add / Delete section rewritten
                        + Linear search procedures
                        + DeleteDuplicates procedure
   0.5 : 2011.04.15     + "Load, Save" section
                        + More overloaded versions
   0.4 : 2011.03.17     ~ Sectionized
                        + "Permutations" section ("Shuffle" unit assimilated)
                        + Tie breaking in binary search
   0.1 : 2010.05.13     + Binary search
   0.0 : 2010.05.05     + Initial version
   Notation: + added, - removed, * fixed, ~ changed
}
interface ///////////////////////////////////////////////////////////////////////////
type
      TIntArray = array of Integer;
      PIntArray = ^TIntArray;

      TIntArrayN   =
         record
         N     :  Integer;
         _     :  TIntArray;
         end;
      PIntArrayN = ^TIntArrayN;

      TRealArray = array of Real;
      PRealArray = ^TRealArray;

      TRealArrayN   =
         record
         N     :  Integer;
         _     :  TRealArray;
         end;
      PRealArrayN = ^TRealArrayN;

      TBoolArray = array of Boolean;
      PBoolArray = ^TBoolArray;

      TRealArray2D = array of array of Real;
       TIntArray2D = array of array of Integer;

{-----------------------<< Initialization >>----------------------------------------}

// Initialize A with Len elements
procedure InitArrayN(
   var   A     :  TIntArrayN;
         Len   :  Integer          =  0);
         overload;

procedure InitArrayN(
   var   A     :  TRealArrayN;
         Len   :  Integer          =  0);
         overload;

// Initialize A with Len elements with a given Value
procedure InitArray(
   var   A     :  TBoolArray;
         Len   :  Integer;
         Value :  Boolean);
         overload;

procedure InitArray(
   var   A     :  TIntArray;
         Len   :  Integer;
         Value :  Integer);
         overload;

procedure InitArray(
   var   A     :  TRealArray;
         Len   :  Integer;
         Value :  Real);
         overload;
         
// Fill A with sequential integers starting from Base
procedure SequentialFill(
   var   A     :  array of Integer;
         Base  :  Integer);
         overload;

// Return array of length N filled with sequential integers starting from Base
procedure SequentialFill(
   var   A     :  TIntArray;
         N     :  Integer;
         Base  :  Integer);
         overload;

procedure SequentialFill(
   var   A     :  TIntArrayN;
         N     :  Integer;
         Base  :  Integer);
         overload;  
         
{-----------------------<< Conversion >>--------------------------------------------}
         
// Convert integer array A into real array B
procedure ConvertToReal(
   var   B     :  TRealArray;
   const A     :   TIntArray);
         overload;

procedure ConvertToReal(
   var   B     :  TRealArrayN;
   const A     :   TIntArrayN);
         overload;
         
{-----------------------<< Resizing >>----------------------------------------------}

// Resize A to NewLen, fill new elements with Value
procedure ExtendArray(
   var   A           :  TIntArray;
         NewLen      :  Integer;
         Value       :  Integer);
         overload;

procedure ExtendArray(
   var   A           :  TRealArray;
         NewLen      :  Integer;
         Value       :  Real);
         overload;

procedure ExtendArray(
   var   A           :  TBoolArray;
         NewLen      :  Integer;
         Value       :  Boolean);
         overload;

// Set the length of A to its true value
procedure SetTrueLength(
   var   A     :  TIntArrayN);
         overload;  

procedure SetTrueLength(
   var   A     :  TRealArrayN);
         overload;

{-----------------------<< Element swapping and reversal >>-------------------------}

// Swap i-th and j-th elements of A
procedure Swap(
   var   A     :  TRealArrayN;
         i, j  :  Integer);
         overload;

procedure Swap(
   var   A     :  TRealArray;
         i, j  :  Integer);
         overload;

// Reverse [i .. j] section of A. If i >= j, A is left unchanged.
procedure Reverse(
   var   A     :  array of Integer;
         i, j  :  Integer);

{-----------------------<< Arithmetics >>-------------------------------------------}

// Add Value to the elements of A
procedure ArrayAdd(
   var   A     :  TRealArray;
         Value :  Real);

// Substract Value from the elements of A
procedure ArraySub(
   var   A     :  TRealArray;
         Value :  Real);

// Multiply the elements of A by Value
procedure ArrayMul(
   var   A     :  TRealArray;
         Value :  Real);
         
// Divide the elements of A by Value
procedure ArrayDiv(
   var   A     :  TRealArray;
         Value :  Real);

// Add B * C to A
procedure ArrayMulAdd(
   var   A     :  TRealArray;
   const B     :  TRealArray;
         C     :  Real);

// The dot product between A and B
function DotProduct(
   var   A, B  :  TRealArray
         )     :  Real; 

// The Euclidean distance between A and B
function DistanceL2(
   const A, B  :  TRealArray
   )           :  Real;         

{-----------------------<< Element addition and removal >>---------------------------}

// Append Value to A
procedure Append(
   var   A     :  TIntArrayN;
         Value :  Integer);
         overload; 

procedure Append(
   var   A     :  TRealArrayN;
         Value :  Real);
         overload;

// Append B to A
procedure AppendArray(
   var   A           :  TRealArray;
   const B           :  TRealArray);

// Append Value to A if it is distinct from all the elements, 
// return if it was appended. Uses linear search, ~N time.
function AppednUnique(
   var   A        :  TIntArrayN;
         Value    :  Integer
         )        :  Boolean;

// Delete the element with a given Index from A
procedure DeleteIndex(
   var   A     :  TIntArrayN;
         Index :  Integer);
         overload;
         
procedure DeleteIndex(
   var   A     :  TRealArrayN;
         Index :  Integer);
         overload;

// Delete all the elements with a given Value from A.
procedure DeleteValue(
   var   A     :  TIntArray;
         Value :  Integer);
         overload;

procedure DeleteValue(
   var   A     :  TIntArrayN;
         Value :  Integer);
         overload;

{-----------------------<< Permutations >>------------------------------------------}       

// Create NewData by permuting Data according to Perm
procedure Permute(
   var   NewData  :  TIntArray;
   const    Data,
         Perm     :  TIntArray);
         overload;

// Permute Data according to Perm
procedure Permute(
   var   Data     :  TIntArray;
   const Perm     :  TIntArray);
         overload;

// Return the Inverse permutation of A: A[Inverse[X]] = X.
procedure InvertPerm(
   var   Inverse  :  TIntArray;
   const A        :  array of Integer);
         overload;

// Invert permutation A
procedure InvertPerm(
   var   A        :  TIntArray);
         overload;
   
{-----------------------<< Search >>------------------------------------------------}

// Return the result of binary search in the [Left, Right] section of array A sorted 
// in ascending order. If there are multiple occurences of Value in the array, index 
// of any of them may be returned. If the Value is not found, closest greater / 
// lesser element is returned in case of positive / negative TieBreak respectively. 
// It is possible to specify Left = -1, TieBreak = Negative or Right = Length(A), 
// TieBreak = Positive so these out of bounds indices are returned if the Value is 
// lesser or greater than any array element.
function BinarySearch(
   const A           :  TRealArray;
         Left, Right :  Integer;
         Value       :  Real;
         TieBreak    :  Integer
         )           :  Integer;
         overload;

function BinarySearch(
   const A           :  TRealArrayN;
         Left, Right :  Integer;
         Value       :  Real;
         TieBreak    :  Integer
         )           :  Integer;
         overload;

// If A contains Value, return Success and Index of its fisrt occurence, 
// Fail and -1 otherwise
function FindValue(
   var   Index :  Integer;
   const A     :  TIntArray;
         Value :  Integer
         )     :  Boolean;
         overload;

function FindValue(
   var   Index :  Integer;
   const A     :  TIntArrayN;
         Value :  Integer
         )     :  Boolean;
         overload;         

// Return whether A contains Value
function InArray(
   const A     :  TIntArray;
         Value :  Integer
         )     :  Boolean;
         overload;
         
function InArray(
   const A     :  TIntArrayN;
         Value :  Integer
         )     :  Boolean;
         overload;

// Return Marked array in which 1s indicate the occurence of Value in A and the
// rest of the elements are 0
procedure MarkValue(
   var   Marked   :  TIntArray;
   const A        :  TIntArray;
         Value    :  Integer);

// Count the number of occurences of Value in A
function ValueCount(
   const A        :  TIntArray;
         Value    :  Integer
         )        :  Integer;
         
// Return the index of minimal element of A, break ties randomly
function RandMinIndex(
   const A        :  array of Real
         )        :  Integer;
         
// Return the index of maximal element of A, break ties randomly
function RandMaxIndex(
   const A        :  array of Real
         )        :  Integer;

{-----------------------<< 2D arrays >>---------------------------------------------}

// A := B
procedure AssignArray2D(
   var   A        :  TRealArray2D;
   const B        :  TRealArray2D);

// Initialize 2D array
procedure InitArray2D(
   var   A        :  TIntArray2D;
         Nx, Ny   :  Integer;
         Value    :  Integer);

// Transpose B
procedure Transpose(
   var   A        :  TRealArray2D;
   const B        :  TRealArray2D);

// Convert A to its Scanline representation
procedure ToScanline(
   var   Scanline :  TRealArray;
   const A        :  TRealArray2D);   

{-----------------------<< Load, Save >>--------------------------------------------}

// Load A from a text file located at Path
function LoadFromText(
   var   A     :  TRealArrayN;
   const Path  :  AnsiString
         )     :  Boolean;
         overload;

function LoadFromText(
   var   A     :  TRealArray;
   const Path  :  AnsiString
         )     :  Boolean;
         overload;

function LoadFromText(
   var   A     :  TIntArray;
   const Path  :  AnsiString
         )     :  Boolean;
         overload;

// Save A to a text file located at Path
function SaveToText(
   var   A     :  TRealArray;
   const Path  :  AnsiString
         )     :  Boolean;

// Save A to a file located at Path
function SaveToFile(
   var   A     :  TRealArray;
   const Path  :  AnsiString
         )     :  Boolean;

implementation //////////////////////////////////////////////////////////////////////
uses
      InvSys,
      Math;    // MaxIntValue

{-----------------------<< Initialization >>----------------------------------------} 

// Initialize A with Len elements
procedure InitArrayN(
   var   A     :  TIntArrayN;
         Len   :  Integer          =  0);
         overload;
   begin
   A.N := Len;
   SetLength(A._, Len);
   end;


procedure InitArrayN(
   var   A     :  TRealArrayN;
         Len   :  Integer          =  0);
         overload;
   begin
   A.N := Len;
   SetLength(A._, Len);
   end;


// Initialize A with Len elements with a given Value
procedure InitArray(
   var   A     :  TBoolArray;
         Len   :  Integer;
         Value :  Boolean);
         overload;
   var
         i     :  Integer;
   begin
   SetLength(A, Len);
   for i := 0 to Len - 1 do
      A[i] := Value;
   end;


procedure InitArray(
   var   A     :  TIntArray;
         Len   :  Integer;
         Value :  Integer);
         overload;
   var
         i     :  Integer;
   begin
   SetLength(A, Len);
   for i := 0 to Len - 1 do
      A[i] := Value;
   end;


procedure InitArray(
   var   A     :  TRealArray;
         Len   :  Integer;
         Value :  Real);
         overload;
   var
         i     :  Integer;
   begin
   SetLength(A, Len);
   for i := 0 to Len - 1 do
      A[i] := Value;
   end;
   
   
// Fill A with sequential integers starting from Base
procedure SequentialFill(
   var   A     :  array of Integer;
         Base  :  Integer);
         overload;
   var
         i     :  Integer;
   begin
   for i := 0 to Length(A) - 1 do
      A[i] := Base + i;
   end;


// Return array of length N filled with sequential integers starting from Base
procedure SequentialFill(
   var   A     :  TIntArray;
         N     :  Integer;
         Base  :  Integer);
         overload;
   begin
   SetLength(A, N);
   SequentialFill(A, Base);
   end;


procedure SequentialFill(
   var   A     :  TIntArrayN;
         N     :  Integer;
         Base  :  Integer);
         overload;
   begin
   A.N := N;
   SequentialFill(A._, N, Base);
   end;
   
{-----------------------<< Resizing >>----------------------------------------------}

// Resize A to NewLen, fill new elements with Value
procedure ExtendArray(
   var   A           :  TIntArray;
         NewLen      :  Integer;
         Value       :  Integer);
         overload;
   var
         i, OldLen   :  Integer;
   begin
   OldLen := Length(A);
   SetLength(A, NewLen);
   for i := OldLen to NewLen - 1 do
      A[i] := Value;
   end;

procedure ExtendArray(
   var   A           :  TRealArray;
         NewLen      :  Integer;
         Value       :  Real);
         overload;
   var
         i, OldLen   :  Integer;
   begin
   OldLen := Length(A);
   SetLength(A, NewLen);
   for i := OldLen to NewLen - 1 do
      A[i] := Value;
   end;

procedure ExtendArray(
   var   A           :  TBoolArray;
         NewLen      :  Integer;
         Value       :  Boolean);
         overload;
   var
         i, OldLen   :  Integer;
   begin
   OldLen := Length(A);
   SetLength(A, NewLen);
   for i := OldLen to NewLen - 1 do
      A[i] := Value;
   end;


// Set the length of A to its true value
procedure SetTrueLength(
   var   A   :  TIntArrayN);
         overload;
   begin
   SetLength(A._, A.N);
   end;


procedure SetTrueLength(
   var   A     :  TRealArrayN);
         overload;
   begin
   SetLength(A._, A.N);
   end;

{-----------------------<< Conversion >>--------------------------------------------}

// Convert integer array A into real array B of length Len
procedure ConvertToReal(
   var   B     :  TRealArray;
   const A     :   TIntArray;
         Len   :  Integer);
         overload;
   var
         i     :  Integer;
   begin
   SetLength(B, Len);
   for i := 0 to Len - 1 do
      B[i] := A[i];
   end;


// Convert integer array A into real array B
procedure ConvertToReal(
   var   B     :  TRealArray;
   const A     :   TIntArray);
         overload;
   begin
   ConvertToReal(B, A, Length(A));
   end;


procedure ConvertToReal(
   var   B     :  TRealArrayN;
   const A     :   TIntArrayN);
         overload;
   begin
   B.N := A.N;
   ConvertToReal(B._, A._, A.N);
   end;

{-----------------------<< Element swapping and reversal >>-------------------------}
   
// Swap i-th and j-th elements of A
procedure Swap(
   var   A     :  TRealArrayN;
         i, j  :  Integer);
         overload;
   var
         Temp  :  Real;
   begin
   Temp   := A._[i];
   A._[i] := A._[j];
   A._[j] := Temp;
   end;


procedure Swap(
   var   A   :  TRealArray;
         i, j  :  Integer);
         overload;
   var
         Temp  :  Real;
   begin
   Temp := A[i];
   A[i] := A[j];
   A[j] := Temp;
   end;


// Reverse [i .. j] section of A. If i >= j, A is left unchanged.
procedure Reverse(
   var   A     :  array of Integer;
         i, j  :  Integer);
   begin
   while i < j do
      begin
      Swap(A[i], A[j]);
      Inc(i);
      Dec(j);
      end;
   end;

{-----------------------<< Arithmetics >>-------------------------------------------}

// Add Value to the elements of A
procedure ArrayAdd(
   var   A     :  TRealArray;
         Value :  Real);
   var
         i     :  Integer;
   begin
   for i := 0 to Length(A) - 1 do
      A[i] := A[i] + Value;
   end;


// Substract Value from the elements of A
procedure ArraySub(
   var   A     :  TRealArray;
         Value :  Real);
   begin
   ArrayAdd(A, -Value);
   end;
   

// Multiply the elements of A by Value
procedure ArrayMul(
   var   A     :  TRealArray;
         Value :  Real);
   var
         i     :  Integer;
   begin
   for i := 0 to Length(A) - 1 do
      A[i] := A[i] * Value;
   end;


// Divide the elements of A by Value
procedure ArrayDiv(
   var   A     :  TRealArray;
         Value :  Real);
   begin
   ArrayMul(A, 1 / Value);
   end;


// Add B * C to A
procedure ArrayMulAdd(
   var   A     :  TRealArray;
   const B     :  TRealArray;
         C     :  Real);
   var
         i     :  Integer;
   begin
   Assert(Length(A) = Length(B));
   for i := 0 to Length(A) - 1 do
      A[i] := A[i] + B[i] * C;
   end;


// The dot product between A and B
function DotProduct(
   var   A, B  :  TRealArray
         )     :  Real;
   var
         i     :  Integer;
   begin
   Assert(Length(A) = Length(B));
   Result := 0;
   for i := 0 to Length(A) - 1 do
      Result := Result + A[i] * B[i];
   end;
   
   
// The Euclidean distance between A and B
function DistanceL2(
   const A, B  :  TRealArray
   )           :  Real;
   var
         i     :  Integer;
         Sum   :  Real;
   begin
   Assert(Length(A) = Length(B));
   Sum := 0;
   for i := 0 to Length(A) - 1 do
      Sum := Sum + Sqr(A[i] - B[i]);
   Result := Sqrt(Sum);
   end;

{-----------------------<< Element addition and removal >>---------------------------}

// Append Value to A
procedure Append(
   var   A     :  TIntArrayN;
         Value :  Integer);
         overload;
   begin
   with A do
      begin
      Inc(N);
      if N > Length(A._) then
          SetLength(A._, 2 * N);
      A._[N - 1] := Value;
      end;
   end;


procedure Append(
   var   A     :  TRealArrayN;
         Value :  Real);
         overload;
   begin
   with A do
      begin
      Inc(N);
      if N > Length(A._) then
          SetLength(A._, 2 * N);
      A._[N - 1] := Value;
      end;
   end;


// Append B to A
procedure AppendArray(
   var   A           :  TRealArray;
   const B           :  TRealArray);
   var
         i, OldLenA,
               LenB  :  Integer;
   begin
   OldLenA := Length(A);
      LenB := Length(B);
   SetLength(A, OldLenA + LenB);
   for i := 0 to LenB - 1 do
      A[OldLenA + i] := B[i];
   end;


// Append Value to A if it is distinct from all the elements, 
// return if it was appended. Uses linear search, ~N time.
function AppednUnique(
   var   A        :  TIntArrayN;
         Value    :  Integer
         )        :  Boolean;
   var
         Unique   :  Boolean;
   begin
   Unique := not InArray(A, Value);
   Result := Unique;
   if Unique then
      Append(A, Value);
   end;


// Delete the element with a given Index from A
procedure DeleteIndex(
   var   A     :  TIntArrayN;
         Index :  Integer);
         overload;
   begin
   with A do
      begin
      Assert(Index < N);
      Dec(N);
      A._[Index] := A._[N];
      if Length(A._) > (2 * N) then
         SetLength(A._, N);
      end;
   end;


procedure DeleteIndex(
   var   A     :  TRealArrayN;
         Index :  Integer);
         overload;
   begin
   with A do
      begin
      Assert(Index < N);
      Dec(N);
      A._[Index] := A._[N];
      if Length(A._) > (2 * N) then
         SetLength(A._, N);
      end;
   end;


// Delete all the elements with a given Value from A.
procedure DeleteValue(
   var   A     :  TIntArray;
         Value :  Integer);
         overload;
   var
         N, i  :  Integer;
   begin
   N := Length(A);
   i := 0;
   while i < N do
      if A[i] = Value then
         begin
         Dec(N);
         A[i] := A[N];
         end
      else
         Inc(i);
   SetLength(A, N);
   end;


procedure DeleteValue(
   var   A     :  TIntArrayN;
         Value :  Integer);
         overload;
   begin
   SetTrueLength(A);
   DeleteValue(A._, Value);
   A.N := Length(A._);
   end;

{-----------------------<< Permutations >>------------------------------------------}

// Create NewData by permuting Data according to Perm
procedure Permute(
   var   NewData  :  TIntArray;
   const    Data,
         Perm     :  TIntArray);
         overload;
   var
         i, L     :  Integer;
   begin
   L := Length(Data);
   Assert(L = Length(Perm));
   SetLength(NewData, L);
   for i := 0 to L - 1 do
      NewData[i] := Data[Perm[i]];
   end;


// Permute Data according to Perm
procedure Permute(
   var   Data     :  TIntArray;
   const Perm     :  TIntArray);
         overload;
   var
         NewData  :  TIntArray;
   begin
   Permute(NewData, Data, Perm);
   Data := Copy(NewData);
   end;


// Return the Inverse permutation of A: A[Inverse[X]] = X.
procedure InvertPerm(
   var   Inverse  :  TIntArray;
   const A        :  array of Integer);
         overload;
   var
         i        :  Integer;
   begin
   InitArray(Inverse, MaxIntValue(A) + 1, {Value:} -1);
   for i := 0 to Length(A) - 1 do
      Inverse[A[i]] := i;
   end;


// Invert permutation A
procedure InvertPerm(
   var   A        :  TIntArray);
         overload;
   var
         Inv      :  TIntArray;
   begin
   InvertPerm(Inv, A);
   A := Copy(Inv);
   end;

{-----------------------<< Search >>------------------------------------------------}     

// Return the result of binary search in the [Left, Right] section of array A sorted 
// in ascending order. If there are multiple occurences of Value in the array, index 
// of any of them may be returned. If the Value is not found, closest greater / 
// lesser element is returned in case of positive / negative TieBreak respectively. 
// It is possible to specify Left = -1, TieBreak = Negative or Right = Length(A), 
// TieBreak = Positive so these out of bounds indices are returned if the Value is 
// lesser or greater than any array element.
 
// Correctness proof sketch: during all iterations, following two statements hold:
// 1. Desired index lies in [Left, Right] interval
// 2. Each subdivision decreases interval length
function BinarySearch(
   const A           :  TRealArray;
         Left, Right :  Integer;
         Value       :  Real;
         TieBreak    :  Integer
         )           :  Integer;
         overload;
   var
         Middle      :  Integer;
         Shift       :  Integer;
   begin
   Shift := ( 1 - Sign(TieBreak > 0) ) div 2;
   while Left < Right do
      begin
      Middle := Left + (Right - Left + Shift) div 2;
      if      Value < A[Middle] then
         Right := Middle - Shift
      else if Value > A[Middle] then
         Left  := Middle + 1 - Shift
      else
         begin
         Left := Middle;
         break;
         end;
      end;
   Result := Left;
   end;


function BinarySearch(
   const A           :  TRealArrayN;
         Left, Right :  Integer;
         Value       :  Real;
         TieBreak    :  Integer
         )           :  Integer;
         overload;
   begin
   Result := BinarySearch(A._, Left, Right, Value, TieBreak);
   end;


// If A contains Value, return Success and Index of its fisrt occurence, 
// Fail and -1 otherwise
function FindValue(
   var   Index :  Integer;
   const A     :  TIntArray;
         Value :  Integer
         )     :  Boolean;
         overload;
   var
         i     :  Integer;
   begin
   Result := Fail;
   Index := -1;
   for i := 0 to Length(A) - 1 do
      if A[i] = Value then
         begin
         Index := i;
         Result := Success;
   {<---}break;
         end;
   end;


// #COPYPASTA
function FindValue(
   var   Index :  Integer;
   const A     :  TIntArrayN;
         Value :  Integer
         )     :  Boolean;
         overload;
   var
         i     :  Integer;
   begin
   Result := Fail;
   Index := -1;
   for i := 0 to A.N - 1 do
      if A._[i] = Value then
         begin
         Index := i;
         Result := Success;
   {<---}break;
         end;
   end;


// Return whether A contains Value
function InArray(
   const A           :  TIntArray;
         Value       :  Integer
         )           :  Boolean;
         overload;
   var
         Dummy       :  Integer;
   begin
   Result := FindValue(Dummy, A, Value);
   end;


function InArray(
   const A     :  TIntArrayN;
         Value :  Integer
         )     :  Boolean;
         overload;
   var
         Dummy :  Integer;
   begin
   Result := FindValue(Dummy, A, Value);
   end;


// Return Marked array in which 1s indicate the occurence of Value in A and the
// rest of the elements are 0
procedure MarkValue(
   var   Marked   :  TIntArray;
   const A        :  TIntArray;
         Value    :  Integer);
   var
         i, Len   :  Integer;
   begin
   Len := Length(A);
   SetLength(Marked, Len);
   for i := 0 to Len - 1 do
      Marked[i] := Ord(A[i] = Value);
   end;


// Count the number of occurences of Value in A
function ValueCount(
   const A        :  TIntArray;
         Value    :  Integer
         )        :  Integer;
   var
         i        :  Integer;
   begin
   Result := 0;
   for i := 0 to Length(A) - 1 do
      if A[i] = Value then
         Inc(Result);
   end;
   
   
// Return the index of minimal (K = -1) or maximal (K = +1) element of A, 
// break ties randomly
function RandMinMaxIndex(
   const A           :  array of Real;
         K           :  Integer
         )           :  Integer;
   var
         Indices     :  TIntArrayN;
         i           :  Integer;
         MaxValue, x :  Real;
   const
         Len0        =  4;
   begin
   MaxValue := K * A[0];
   SetLength(Indices._, Len0);
   Indices.N := 0;
   for i := 0 to Length(A) - 1 do
      begin
      x := K * A[i];
      if x >= MaxValue then
         begin
         if x > MaxValue then
            begin
            MaxValue := x;
            InitArrayN(Indices);
            end;
         Append(Indices, i);
         end;
      end;
   Result := Indices._[ Random(Indices.N) ];
   end;


// Return the index of minimal element of A, break ties randomly
function RandMinIndex(
   const A        :  array of Real
         )        :  Integer;
   begin
   Result := RandMinMaxIndex(A, -1);
   end;


// Return the index of maximal element of A, break ties randomly
function RandMaxIndex(
   const A        :  array of Real
         )        :  Integer;
   begin
   Result := RandMinMaxIndex(A, +1);
   end;

{-----------------------<< 2D arrays >>---------------------------------------------}

// A := B
procedure AssignArray2D(
   var   A        :  TRealArray2D;
   const B        :  TRealArray2D);
   var
         i, N     :  Integer;
   begin
   N := Length(B);
   SetLength(A, N);
   for i := 0 to N - 1 do
      A[i] := Copy(B[i]);
   end;


// Initialize 2D array
procedure InitArray2D(
   var   A        :  TIntArray2D;
         Nx, Ny   :  Integer;
         Value    :  Integer);
   var
         i, j     :  Integer;
   begin
   SetLength(A, Nx, Ny);
   for i := 0 to Nx - 1 do
      for j := 0 to Ny - 1 do
         A[i, j] := Value;
   end;


// Transpose B
procedure Transpose(
   var   A        :  TRealArray2D;
   const B        :  TRealArray2D);
   var
         i,  j,
         Nx, Ny   :  Integer;
   begin
   Nx := Length(B);
   Ny := Length(B[0]);
   SetLength(A, Ny);
   for j := 0 to Ny - 1 do
      begin
      SetLength(A[j], Nx);
      for i := 0 to Nx - 1 do
         A[j, i] := B[i, j];
      end;
   end;


// Convert A to its Scanline representation
procedure ToScanline(
   var   Scanline :  TRealArray;
   const A        :  TRealArray2D);
   var
         i,  j, k,
         Nx, Ny   :  Integer;
   begin
   Nx := Length(A   );
   Ny := Length(A[0]);
   SetLength(Scanline, Nx * Ny);
   k := 0;
   for j := 0 to Ny - 1 do
      for i := 0 to Nx -1 do
         begin
         Scanline[k] := A[i, j];
         Inc(k);
         end;
   end;

{-----------------------<< Load, Save >>--------------------------------------------}

// Load A from a text file located at Path
function LoadFromText(
   var   A     :  TRealArrayN;
   const Path  :  AnsiString
         )     :  Boolean;
         overload;
   var
         Temp  :  Real;
         Data  :  Text;
   begin
   if OpenRead(Data, Path) = Success then
      begin
      InitArrayN(A);
      repeat
         ReadLn(Data, Temp);
         Append(A, Temp);
      until EoF(Data);
      Close(Data);
      SetTrueLength(A);
      Result := Success;
      end
   else
      Result := Fail;
   end;


function LoadFromText(
   var   A     :  TRealArray;
   const Path  :  AnsiString
         )     :  Boolean;
         overload;
   var
         AN    :  TRealArrayN;
   begin
   if LoadFromText(AN, Path) = Success then
      begin
      A := Copy(AN._);
      Result := Success;
      end
   else
      Result := Fail;
   end;


function LoadFromText(
   var   A     :  TIntArray;
   const Path  :  AnsiString
         )     :  Boolean;
         overload;
   var
         i     :  Integer;
         AN    :  TRealArrayN;
   begin
   if LoadFromText(AN, Path) = Success then
      begin
      SetLength(A, AN.N);
      for i := 0 to AN.N - 1 do
         A[i] := Round(AN._[i]);
      Result := Success;
      end
   else
      Result := Fail;
   end;


// Save A to a text file located at Path
function SaveToText(
   var   A     :  TRealArray;
   const Path  :  AnsiString
         )     :  Boolean;
   var
         i     :  Integer;
         Data  :  Text;
   begin
   if OpenWrite(Data, Path) = Success then
      begin
      for i := 0 to Length(A) - 1 do
         WriteLn(Data, A[i]);
      Close(Data);
      Result := Success;
      end
   else
      Result := Fail;
   end;


// Save A to a file located at Path
function SaveToFile(
   var   A     :  TRealArray;
   const Path  :  AnsiString
         )     :  Boolean;
   var
         i     :  Integer;
         Data  :  TRealFile;
   begin
   if OpenWrite(Data, Path) = Success then
      begin
      for i := 0 to Length(A) - 1 do
         Write(Data, A[i]);
      Close(Data);
      Result := Success;
      end
   else
      Result := Fail;
   end;

end.
