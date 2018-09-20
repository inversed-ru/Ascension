{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit HashTables; ////////////////////////////////////////////////////////////////////
{
>> Version: 0.4

>> Description
   (Almost) an abstract hash table, fixed-size or auto-resizable. Part of InvLibs 
   unit collection.
   
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> Usage
   - By default, the table works with objects stored in an array. If you're using 
     some other structure, change the TObjectIndex definition accordingly.
   - You must provide  a procedure  that checks  if an object  is equal to some other
     object in your structure (TProcObjectsEqual).
   - When using  HashTable  with a specified  size,  this  size will be automatically
     rounded up to the next power of two.

>> Design decisions
   - Hash table size is forced  to be a power of two.  It is possible  to apply 'mod'
     operation  to map hash to any range,  but the resulting distribution will not be
     uniform in the general case. 
   - Truly abstract realization using pointers is probably possible, but would likely
     be too cumbersome.

>> Changelog
   0.4   : 2018.09.09 ~ FreePascal compatibility
   0.3.1 : 2011.10.14 ~ Moved to generic types
   0.3   : 2011.03.05 + AddUnique function added
                      ~ Stylistical changes
   0.0   : 2010.05.07 + Initial version
   Notation: + added, - removed, * fixed, ~ changed   
}
interface ///////////////////////////////////////////////////////////////////////////
type
   // Index of your object in your structure. If you're not using an array,
   // change this to anything that allows to identify an object
   TObjectIndex = Integer;

   // Returns if an object specified by ptrObject is the same as an object in
   // ptrStructure specified by Index
   TProcObjectsEqual =
      function (
               ptrStructure   :  Pointer;
               Index          :  TObjectIndex;
               ptrObject      :  Pointer
               )              :  Boolean;

   TChainEntry =
      record
      ObjectIndex    :  TObjectIndex;
      Hash           :  LongWord;     // Unmapped raw 32bit hash
      end;

   THashChain =
      record
      RealLen  :  Integer;
      Chain    :  array of TChainEntry;
      end;

   THashChains =
      array of THashChain;

   THashTable =
      record
      Size, NEntries, Collisions :  Integer;
      AutoResize                 :  Boolean;
      Chains                     :  THashChains;
      end;

{-----------------------<< Hash table >>--------------------------------------------}

// Initialize the hash table (fixed size)
procedure InitHashTable(
   var   HashTable   :  THashTable;
         HashSize    :  Integer);
   overload;

// Initialize the hash table (variable size)
procedure InitHashTable(
   var   HashTable   :  THashTable);
   overload;

// Check if an object is already in the hash table, return its index
function LookupHashTable(
   var   FoundIndex     :  TObjectIndex;
         ptrStructure   :  Pointer;
   const HashTable      :  THashTable;
         ptrObject      :  Pointer;
         Hash           :  LongWord;         // Must be unmapped raw 32bit hash
         ProcEqual      :  TProcObjectsEqual
         )              :  Boolean;

// Add an entry to the hash table
procedure AddToHashTable(
   var   HashTable      :  THashTable;
         ObjectIndex    :  TObjectIndex;
         Hash           :  LongWord);     // Must be unmapped raw 32bit hash

// Add an object to the table if it is not already present, 
// return whether it was added
function AddUnique(
   var   HashTable      :  THashTable;
         ptrObject      :  Pointer;
         ObjectIndex    :  TObjectIndex;
         Hash           :  LongWord;         // Must be unmapped raw 32bit hash
         ptrStructure   :  Pointer;
         ProcEqual      :  TProcObjectsEqual
         )              :  Boolean;

implementation //////////////////////////////////////////////////////////////////////
uses
      invSys,
      BitMath;

{-----------------------<< Hash table >>--------------------------------------------}

// Initialize the hash table (fixed size)
procedure InitHashTable(
   var   HashTable   :  THashTable;
         HashSize    :  Integer);
         overload;
   var
         i           :  Integer;
   begin
   with HashTable do
      begin
      NEntries   := 0;
      Collisions := 0;
      AutoResize := False;
      Size := Ceil2p(HashSize);
      SetLength(Chains, Size);
      for i := 0 to Size - 1 do
         begin
         // #UNOPT set initial chain lengths to 1?
         Chains[i].RealLen := 0;
         SetLength(Chains[i].Chain, 0);
         end;
      end;
   end;


// Initialize the hash table (variable size)
procedure InitHashTable(
   var   HashTable   :  THashTable);
         overload;
   var
         i           :  Integer;
   const
         InitialHashSize = 256;
   begin
   with HashTable do
      begin
      NEntries   := 0;
      Collisions := 0;
      AutoResize := True;
      Size := InitialHashSize;
      SetLength(Chains, Size);
      for i := 0 to Size - 1 do
         begin
         Chains[i].RealLen := 0;
         SetLength(Chains[i].Chain, 0);
         end;
      end;
   end;


// Double the size of the hash table
procedure DoubleHashTable(
   var   HashTable   :  THashTable);
   var
         i, j, NewPos,
         LenChain    :  Integer;
         Mask        :  LongWord;
   begin
   with HashTable do
      begin
      // Double the size
      Size := Size * 2;
      SetLength(Chains, Size);
      Mask := Size - 1;
      for i := Size div 2 to Size - 1 do
         begin
         Chains[i].RealLen := 0;
         SetLength(Chains[i].Chain, 0);
         end;

      // Relocate old records to new half when needed
      for i := 0 to (Size div 2) - 1 do
         begin
         for j := Chains[i].RealLen - 1 downto 0 do
            begin
            NewPos := Chains[i].Chain[j].Hash and Mask;

            // New position not equal to old, relocate
            if NewPos <> i then
               begin

               // Add to new position
               if Chains[NewPos].RealLen <> 0 then
                  Inc(Collisions);
               Inc(Chains[NewPos].RealLen);
               LenChain := Chains[NewPos].RealLen;
               if LenChain > Length(Chains[NewPos].Chain) then
                  SetLength(Chains[NewPos].Chain, 2 * LenChain);
               Chains[NewPos].Chain[LenChain - 1] := Chains[i].Chain[j];

               // Remove from old position
               if Chains[i].RealLen  = 1 then
                  Chains[i].RealLen := 0
               else
                  begin
                  Dec(Chains[i].RealLen);
                  Chains[i].Chain[j] := Chains[i].Chain[Chains[i].RealLen];
                  Dec(Collisions);
                  end;
               end;
            end;
         end;
      end;
   end;


// Add an entry to the hash table
procedure AddToHashTable(
   var   HashTable   :  THashTable;
         ObjectIndex :  TObjectIndex;
         Hash        :  LongWord);    // Must be unmapped raw 32bit hash
   const
         MaxLoad     =  1.0; // Optimal value found in preliminary test
   var
         Mapped,
         LenChain    :  Integer;
   begin
   with HashTable do
      begin
      // Resize table if load is high
      Inc(NEntries);
      if AutoResize and ( (NEntries / Size) > MaxLoad ) then
         DoubleHashTable(HashTable);

      // Add entry
      Mapped := Hash and (Size - 1);
      if Chains[Mapped].RealLen <> 0 then
         Inc(Collisions);
      Inc(Chains[Mapped].RealLen);
      LenChain := Chains[Mapped].RealLen;
      if LenChain > Length(Chains[Mapped].Chain) then
         SetLength(Chains[Mapped].Chain, 2 * LenChain);
      Chains[Mapped].Chain[LenChain - 1].ObjectIndex := ObjectIndex;
      Chains[Mapped].Chain[LenChain - 1].Hash := Hash;
      end;
   end;


// #HOTSPOT
// Check if an object is already in the hash table, return its index
function LookupHashTable(
   var   FoundIndex     :  TObjectIndex;
         ptrStructure   :  Pointer;
   const HashTable      :  THashTable;
         ptrObject      :  Pointer;
         Hash           :  LongWord;         // Must be unmapped raw 32bit hash
         ProcEqual      :  TProcObjectsEqual
         )              :  Boolean;
   var
         Mapped,
         LenChain, i    :  Integer;
         Found          :  Boolean;
   begin
   with HashTable do
      begin
      Found := Fail;
      Mapped := Hash and (Size - 1);
      LenChain := Chains[Mapped].RealLen;
      for i := 0 to LenChain - 1 do
         if ProcEqual(
            ptrStructure, Chains[Mapped].Chain[i].ObjectIndex, ptrObject) then
            begin
            FoundIndex := Chains[Mapped].Chain[i].ObjectIndex;
            Found := Success;
            break;
            end;
      Result := Found;
      end;
   end;


// Add an object to the table if it is not already present, 
// return whether it was added
function AddUnique(
   var   HashTable      :  THashTable;
         ptrObject      :  Pointer;
         ObjectIndex    :  TObjectIndex;
         Hash           :  LongWord;         // Must be unmapped raw 32bit hash
         ptrStructure   :  Pointer;
         ProcEqual      :  TProcObjectsEqual
         )              :  Boolean;
   var
         DummyIndex     :  TObjectIndex;
   begin
   if LookupHashTable(
      DummyIndex, ptrStructure, HashTable, ptrObject, Hash, ProcEqual) then
      Result := Fail
   else
      begin
      AddToHashTable(HashTable, ObjectIndex, Hash);
      Result := Success;
      end;
   end;

end.
 