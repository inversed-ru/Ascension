{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit Crossovers; ////////////////////////////////////////////////////////////////////
{
>> Version: 0.4

>> Description
   Implementation of some general crossover routines: cut points, element masks,
   permutational crossovers.

>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> Usage notes
   In all cases when dynamic arrays are passed as var open array parameters, they
   must already have the correct length.

>> ToDo
    - Add cut and splice crossovers
    - Add references
    
>> Changelog
   0.4 : 2018.09.18  ~ FreePascal compatibility
   0.3 : 2017.12.07  - Added crossover for real arrays
   0.2 : 2012.02.07  - Added permutational guided moves
   0.1 : 2011.09.10  - Moved edge recombination into TSP PDU
   0.0 : 2011.09.09  - Initial version
}
interface ///////////////////////////////////////////////////////////////////////////
uses
      Arrays;
type
      TMaskType = (mt1pt, mt2pt, mtUniform);

{-----------------------<< Masks >>-------------------------------------------------}

// Select 2 random cut points for use in 2 point crossover of parents of length Len.
// Elements in the interval [Cut1, Cut2] should belong to one parent, other elements 
// to another parent.
procedure GetCutPoints2pt(
   var   Cut1, Cut2  :  Integer;
         Len         :  Integer);

// Get random Mask for 1 point crossover of parents of length Len.
procedure GetMask1pt(
   var   Mask     :  TBoolArray;
         Len      :  Integer);

// Get random Mask for 2 point crossover of parents of length Len.
procedure GetMask2pt(
   var   Mask           :  TBoolArray;
         Len            :  Integer);

// Get random Mask for uniform crossover, ensuring that it is not degenerate.
procedure GetMaskUniform(
   var   Mask     :  TBoolArray;
         Len      :  Integer);

// Get random Mask for 1 or 2 point crossover of parents of length Len.
procedure GetXoverMask(
   var   Mask     :  TBoolArray;
         Len      :  Integer;
         MaskType :  TMaskType);

{-----------------------<< General purpose crossovers >>----------------------------}         

// General purpose crossover for integer arrays
procedure CrossoverInt(
   var   Child       :  array of Integer;
   const ParentA,
         ParentB     :  array of Integer;
         MaskType    :  TMaskType);
         
// General purpose crossover for real arrays
procedure CrossoverReal(
   var   Child       :  array of Real;
   const ParentA,
         ParentB     :  array of Real;
         MaskType    :  TMaskType);

{-----------------------<< Permutational crossovers >>------------------------------}         

// PMX, Partially-matched crossover. Part of the child comes from one parent, rest of
// the elements come from another with remapping to ensure validity.
procedure CrossoverPM(
   var   Child       :  array of Integer;
   const ParentA,
         ParentB     :  array of Integer;
   const Mask        :  TBoolArray;
         Base        :  Integer           =  0);
         overload;

procedure CrossoverPM(
   var   Child       :  array of Integer;
   const ParentA,
         ParentB     :  array of Integer;
         MaskType    :  TMaskType;
         Base        :  Integer           =  0);
         overload;         

// OX, Ordered crossover. Part of the child comes from one parent, rest of the
// elements come from another parent in order of appearance.
procedure CrossoverOrd(
   var   Child       :  array of Integer;
   const ParentA,
         ParentB     :  array of Integer;
   const Mask        :  TBoolArray;
         Base        :  Integer           =  0);
         overload;

procedure CrossoverOrd(
   var   Child       :  array of Integer;
   const ParentA,
         ParentB     :  array of Integer;
         MaskType    :  TMaskType;
         Base        :  Integer           =  0);
         overload;

// CX, Cycle crossover. Every element inherits its position from one of the parents.
procedure CrossoverCycle(
   var   Child    :  array of Integer;
   const ParentA,
         ParentB  :  array of Integer);

{-----------------------<< Permutational guided moves >>----------------------------}

// Swap element for another one so it would match Guide in the same position.
// Special case of PMX with 1 element inherited from Guide. If NonTrivial is True,
// only moves that change X are considered. Return Fail if X = Guide after the move.
function GuidedMoveSwap(
   var   X           :  array of Integer;
   const Guide       :  array of Integer;
         NonTrivial  :  Boolean
         )           :  Boolean;
         overload;

// This version does not swap the elements, but returns their positions
function GuidedMoveSwap(
   var   i, j        :  Integer;
   const X, Guide    :  array of Integer;
         NonTrivial  :  Boolean
         )           :  Boolean;
         overload;

// Special case of cycle crossover with one cycle inherited from Guide.
// If NonTrivial = True, only cycles that make difference are considered.
// Return Fail if X = Guide after the move.
function GuidedMoveCycle(
   var   X           :  array of Integer;
   const Guide       :  array of Integer;
         NonTrivial  :  Boolean
         )           :  Boolean;

implementation //////////////////////////////////////////////////////////////////////
uses
      InvSys,
      ExtraMath,
      RandVars,
      Statistics;

{-----------------------<< Masks >>-------------------------------------------------}

// Select 2 random cut points for use in 2 point crossover of parents of length Len.
// Elements in the interval [Cut1, Cut2] should belong to one parent, other elements 
// to another parent.
procedure GetCutPoints2pt(
   var   Cut1, Cut2  :  Integer;
         Len         :  Integer);
   begin
   repeat
      Cut1 := Random(Len);
      Cut2 := Random(Len);
   until Abs(Cut1 - Cut2) <> (Len - 1);
   if Cut1 > Cut2 then
      Swap(Cut1, Cut2);
   end;



// Get random Mask for 1 point crossover of parents of length Len.
procedure GetMask1pt(
   var   Mask     :  TBoolArray;
         Len      :  Integer);
   var
         i, Cut   :  Integer;
         Invert   :  Boolean;
   begin
   Cut := Random(Len - 1);
   Invert := RandBool;
   SetLength(Mask, Len);
   for i := 0 to Len - 1 do
      Mask[i] := (i <= Cut) xor Invert;
   end;


// Get random Mask for 2 point crossover of parents of length Len.
procedure GetMask2pt(
   var   Mask           :  TBoolArray;
         Len            :  Integer);
   var
         i, Cut1, Cut2  :  Integer;
         Invert         :  Boolean;
   begin
   GetCutPoints2pt(Cut1, Cut2, Len);
   Invert := RandBool;
   SetLength(Mask, Len);
   for i := 0 to Len - 1 do
      Mask[i] := ( (i >= Cut1) and (i <= Cut2) ) xor Invert;
   end;


// Get random Mask for uniform crossover, ensuring that it is not degenerate.
procedure GetMaskUniform(
   var   Mask     :  TBoolArray;
         Len      :  Integer);
   var
         i        :  Integer;
         Diff     :  Boolean;
   begin
   SetLength(Mask, Len);
   repeat
      Diff := False;
      Mask[0] := RandBool;
      for i := 1 to Len - 1 do
         begin
         Mask[i] := RandBool;
         Diff := Diff or (Mask[i] xor Mask[i - 1]);
         end;
   until Diff or (Len = 1);
   end;


// Get random Mask for 1 or 2 point crossover of parents of length Len.
procedure GetXoverMask(
   var   Mask     :  TBoolArray;
         Len      :  Integer;
         MaskType :  TMaskType);
   begin
   case MaskType of
      mt1pt:      GetMask1pt    (Mask, Len);
      mt2pt:      GetMask2pt    (Mask, Len);
      mtUniform:  GetMaskUniform(Mask, Len);
      else        Assert(False);
      end;
   end;


// Create a Child by copying a region specified by Mask from Parent of 
// the same length. 
procedure CopyRegion(
   var   Child       :  array of Integer;
   const Parent      :  array of Integer;
   const Mask        :  TBoolArray);
   var
         i, Len      :  Integer;
   begin
   Len := Length(Child);
   Assert(Len = Length(Parent));
   for i := 0 to Len - 1 do
      if Mask[i] then
         Child[i] := Parent[i];
   end;

{-----------------------<< General purpose crossovers >>----------------------------}

// Make sure that the length of Child is equal to the lengths of parents, return it  
function CheckedLen(
   const Child,
         ParentA,
         ParentB  :  array of Integer
         )        :  Integer;
         overload;
   begin
   Result := Length(Child);
   Assert(Result = Length(ParentA));
   Assert(Result = Length(ParentB));
   end;
   
   
function CheckedLen(
   const Child,
         ParentA,
         ParentB  :  array of Real
         )        :  Integer;
         overload;
   begin
   Result := Length(Child);
   Assert(Result = Length(ParentA));
   Assert(Result = Length(ParentB));
   end;


// Make sure that the length of A = length of B, return it
function CheckedLen(
   const A, B  :  array of Integer
         )     :  Integer;
         overload;
   begin
          Result := Length(A);
   Assert(Result  = Length(B));
   end;


// General purpose crossover for integer arrays
procedure CrossoverInt(
   var   Child       :  array of Integer;
   const ParentA,
         ParentB     :  array of Integer;
         MaskType    :  TMaskType);
   var
         Mask        :  TBoolArray;
         i, Len      :  Integer;
   begin
   Len := CheckedLen(Child, ParentA, ParentB);
   GetXoverMask(Mask, Len, MaskType);
   for i := 0 to Len - 1 do
      if Mask[i] then
         Child[i] := ParentA[i] else
         Child[i] := ParentB[i];
   end;
   
   
// General purpose crossover for real arrays
procedure CrossoverReal(
   var   Child       :  array of Real;
   const ParentA,
         ParentB     :  array of Real;
         MaskType    :  TMaskType);
   var
         Mask        :  TBoolArray;
         i, Len      :  Integer;
   begin
   Len := CheckedLen(Child, ParentA, ParentB);
   GetXoverMask(Mask, Len, MaskType);
   for i := 0 to Len - 1 do
      if Mask[i] then
         Child[i] := ParentA[i] else
         Child[i] := ParentB[i];
   end;

{-----------------------<< Permutational crossovers >>------------------------------}
// Most of these crossovers are asymetric: statistically X(A, B) may be different
// from X(B, A). In routines that accept Base parameter, it should specify the
// smallest value of the permutations.

// Mark Perm's values specified by Mask in the Used array.
procedure FindUsed(
   var   Used        :  TBoolArray;
   const Perm        :  array of Integer;
   const Mask        :  TBoolArray;
         Base        :  Integer);
   var
         i, Len      :  Integer;
   begin
   Len := Length(Perm);
   InitArray(Used, Len + Base, False);
   for i := 0 to Len - 1 do
      if Mask[i] then
         Used[ Perm[i] ] := True;
   end;
   

// PMX, Partially-matched crossover. Part of the child comes from one parent, rest of
// the elements come from another with remapping to ensure validity.
procedure CrossoverPM(
   var   Child       :  array of Integer;
   const ParentA,
         ParentB     :  array of Integer;
   const Mask        :  TBoolArray;
         Base        :  Integer           =  0);
         overload;
   var
         Map         :  array of Integer;
         Used        :  TBoolArray;
         i, Len      :  Integer;
         X           :  Integer;
   begin
   Len := CheckedLen(Child, ParentA, ParentB);
   Assert(Length(Mask) = Len);

   // Calculate Parent A -> Parent B mapping
   SetLength(Map, Len + Base);
   for i := 0 to Len - 1 do
      Map[ParentA[i]] := ParentB[i];

   // Copy cities from Parent A to crossover region, mark them
   CopyRegion(Child, ParentA, Mask);
   FindUsed(Used, Child, Mask, Base);

   // Rest of the genome: use Parent B, remap duplicates
   for i := 0 to Len - 1 do
      if not Mask[i] then
         begin
         X := ParentB[i];
         while Used[X] do
            X := Map[X];
         Child[i] := X;
         end;
   end;


procedure CrossoverPM(
   var   Child       :  array of Integer;
   const ParentA,
         ParentB     :  array of Integer;
         MaskType    :  TMaskType;
         Base        :  Integer           =  0);
         overload;
   var
         Mask        :  TBoolArray;
   begin
   GetXoverMask(Mask, Length(Child), MaskType);
   CrossoverPM(Child, ParentA, ParentB, Mask, Base);
   end;


// OX, Ordered crossover. Part of the child comes from one parent, rest of the
// elements come from another parent in order of appearance.
procedure CrossoverOrd(
   var   Child          :  array of Integer;
   const ParentA,
         ParentB        :  array of Integer;
   const Mask           :  TBoolArray;
         Base           :  Integer           =  0);
         overload;
   var
         Used           :  TBoolArray;
         i, j, i0, Len  :  Integer;
         PrevMask       :  Boolean;
   begin
   Len := CheckedLen(Child, ParentA, ParentB);
   Assert(Length(Mask) = Len);

   // Copy cities from Parent A to crossover region, mark them
   CopyRegion(Child, ParentA, Mask);
   FindUsed(Used, Child, Mask, Base);

   // Find where Parent A section ends
   i := 0;
   repeat
      PrevMask := Mask[i];
      i := (i + 1) mod Len;
   until (not Mask[i]) and PrevMask;

   // Rest of the genome: sequentially add cities from Parent B, skip duplicates
   i0 := i;
   j  := i;
   repeat
      if not Mask[i] then
         begin
         while Used[ ParentB[j] ] do
            j := (j + 1) mod Len;
         Child[i] := ParentB[j];
         Used[ Child[i] ] := True;
         end;
      i := (i + 1) mod Len;
   until i = i0;
   end;


procedure CrossoverOrd(
   var   Child       :  array of Integer;
   const ParentA,
         ParentB     :  array of Integer;
         MaskType    :  TMaskType;
         Base        :  Integer           =  0);
         overload;
   var
         Mask        :  TBoolArray;
   begin
   GetXoverMask(Mask, Length(Child), MaskType);
   CrossoverOrd(Child, ParentA, ParentB, Mask, Base);
   end;


// Get cycles of permutations A and B
procedure GetCycles(
   var    Cycles     :  TIntArray;
   var   NCycles     :  Integer;
   const A, B        :  array of Integer);
   var
         Len, i, j   :  Integer;
         InvA        :  TIntArray;
   begin
   Len := Length(A);
   InitArray(Cycles, Len, {Value:} -1);
   InvertPerm(InvA, A);
   NCycles := 0;
   for i := 0 to Len - 1 do
      if Cycles[i] < 0 then
         begin
         j := i;
         repeat
            Cycles[j] := NCycles;
            j := InvA[B[j]];
         until j = i;
         Inc(NCycles);
         end;
   end;


// CX, Cycle crossover. Every element inherits its position from one of the parents.
procedure CrossoverCycle(
   var   Child    :  array of Integer;
   const ParentA,
         ParentB  :  array of Integer);
   var
         i, Len,
         NCycles  :  Integer;
         Cycles   :  TIntArray;
         Mask     :  TBoolArray;
   begin
   Len :=   CheckedLen(Child, ParentA, ParentB);
   GetCycles(Cycles, NCycles, ParentA, ParentB);
   GetMaskUniform(Mask, NCycles);
   for i := 0 to Len - 1 do
      if Mask[Cycles[i]] then
         Child[i] := ParentA[i]
      else
         Child[i] := ParentB[i];
   end;

{-----------------------<< Permutational guided moves >>----------------------------}

// Select a random Index in which A and B differ, return Fail if A = B
function RandDiffPos(
   var   Index       :  Integer;
   const A, B        :  array of Integer
         )           :  Boolean;
   var
         i, N, Len   :  Integer;
         Diff        :  TIntArray;
   begin
   // Find all different positions
   N := 0;
   Len := Length(A);
   SetLength(Diff, Len);
   for i := 0 to Len - 1 do
      if A[i] <> B[i] then
         begin
         Diff[N] := i;
         Inc(N);
         end;

   // Select random one
   if N > 0 then
      begin
      Index := Diff[Random(N)];
      Result := Success;
      end
   else
      Result := Fail;
   end;


// Swap element for another one so it would match Guide in the same position.
// Special case of PMX with 1 element inherited from Guide. If NonTrivial is True,
// only moves that change X are considered. Return Fail if X = Guide after the move.
function GuidedMoveSwap(
   var   X           :  array of Integer;
   const Guide       :  array of Integer;
         NonTrivial  :  Boolean
         )           :  Boolean;
         overload;
   var
         i, j        :  Integer;
   begin
   if GuidedMoveSwap(i, j, X, Guide, NonTrivial) = Success then
      begin
      Swap(X[i], X[j]);
      Result := Success;
      end
   else
      Result := Fail;
   end;


// This version does not swap the elements, but returns their positions
function GuidedMoveSwap(
   var   i, j        :  Integer;
   const X, Guide    :  array of Integer;
         NonTrivial  :  Boolean
         )           :  Boolean;
         overload;
   var
         Len         :  Integer;
         Different   :  Boolean;
   begin
   // Select element to swap
   Len := CheckedLen(X, Guide);
   if NonTrivial then
      begin
      i := 0;
      Different := RandDiffPos(i, X, Guide);
      end
   else
      begin
      i := Random(Len);
      Different := X[i] <> Guide[i];
      end;

   // Swap if it makes difference
   if Different then
      begin
      j := 0;
      while X[j] <> Guide[i] do
         Inc(j);
      Result := Success;
      end
   else
      begin
      j := i;
      Result := Fail;
      end;
   end;


// Special case of cycle crossover with one cycle inherited from Guide.
// If NonTrivial = True, only cycles that make difference are considered.
// Return Fail if X = Guide after the move.
function GuidedMoveCycle(
   var   X           :  array of Integer;
   const Guide       :  array of Integer;
         NonTrivial  :  Boolean
         )           :  Boolean;
   var
         i, j, Len,
         NCycles     :  Integer;
         Cycles      :  TIntArray;
         Different   :  Boolean;
   begin
   // Select cycle
   Len := CheckedLen(X, Guide);
   if NonTrivial then
      Different := RandDiffPos(i, X, Guide)
   else
      begin
      i := Random(Len);
      Different := X[i] <> Guide[i];
      end;

   // Copy cycle if it is different
   if Different then
      begin
      GetCycles(Cycles, NCycles, X, Guide);
      for j := 0 to Len - 1 do
         if Cycles[j] = Cycles[i] then
            X[j] := Guide[j];
      Result := Success;
      end
   else
      Result := Fail;
   end;

end.
