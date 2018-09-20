{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ASMMODE INTEL} {$ENDIF} 
unit BitMath; ///////////////////////////////////////////////////////////////////////
{
>> Version: 1.3

>> Description
   Bit-level arithmetic functions. Part of InvLibs unit collection.
   
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> References
   Henry S. Warren, Hacker's Delight.
                                                                              
>> ToDo
   - Add more functions

>> Changelog
   1.3 : 2018.09.20  ~ FreePascal compatibility
   1.2 : 2015.07.02  + LowestBit function
                     ~ Renamed HighestSetBit to HighestBit
   1.1 : 2014.01.27  + BitCount64 function
   1.0 : 2013.10.12  + BitShift, BitCount functions
                     ~ Changed to common types
                     ~ Simplified implementation
   0.0 : 2010.05.06  + Initial version
   Notation: + added, - removed, * fixed, ~ changed  
}
interface ///////////////////////////////////////////////////////////////////////////

// Bitshift X left if Shift is positive, right if negative
function BitShift(
         x     :  Integer;
         Shift :  Integer
         )     :  Integer;

// Return the index of the highest set bit in X, or -1 if there are none
function HighestBit(
         x     :  Integer
         )     :  Integer;

// Return the index of the lowest set bit in X, or -1 if there are none
function LowestBit(
         x     :  Integer
         )     :  Integer;

// Floor of base 2 logarithm of X
function FloorLog2(
         x  :  Integer
         )  :  Integer;

// The smallest power of two that is not less than than X
function Ceil2p(
         x     :  Integer
         )     :  Integer;

// The largest power of two that is not greater than than X
function Floor2p(
         x     :  Integer
         )     :  Integer;

// Return the number of set bits in X
function BitCount(
         x  :  Integer
         )  :  Integer;

// Return the number of set bits in X
function BitCount64(
         x        :  Int64
         )        :  Integer;

const
      IntNBytes      =  4;
      IntHighByte    =  IntNBytes - 1;
      Int64NBytes    =  8;
      Int64HighByte  =  Int64NBytes - 1;
      ByteNBits      =  8;
      ByteHighBit    =  ByteNBits - 1;
      IntNBits       =  IntNBytes * ByteNBits;
      IntHighBit     =  IntNBits - 1;
type
      TIntBytes   = array [0 ..   IntHighByte] of Byte;
      TInt64Bytes = array [0 .. Int64HighByte] of Byte;
      PIntBytes   = ^TIntBytes;
      PInt64Bytes = ^TInt64Bytes;
var
      ByteBitCount   :  array [Byte] of Integer;
      RevBytes       :  array [Byte] of Byte;

implementation //////////////////////////////////////////////////////////////////////

{$RANGECHECKS OFF} {$OVERFLOWCHECKS OFF}

// Bitshift X left if Shift is positive, right if negative
function BitShift(
         x     :  Integer;
         Shift :  Integer
         )     :  Integer;
   begin
   if Shift > 0 then
      Result := x shl  Shift else
      Result := x shr -Shift ;
   end;
   

// Return the index of the highest set bit in X, or -1 if there are none
function HighestBit(
         x     :  Integer
         )     :  Integer;
   asm
   bsr   eax   ,  eax
   jnz   @Done
   xor   eax   ,  eax
   dec   eax
   @Done:
   end;


// Return the index of the lowest set bit in X, or -1 if there are none
function LowestBit(
         x     :  Integer
         )     :  Integer;
   asm
   bsf   eax   ,  eax
   jnz   @Done
   xor   eax   ,  eax
   dec   eax
   @Done:
   end;


// Floor of base 2 logarithm of X
function FloorLog2(
         x  :  Integer
         )  :  Integer;
   begin
   Result := HighestBit(x);
   end;


// The smallest power of two that is not less than than X
function Ceil2p(
         x     :  Integer
         )     :  Integer;
   begin
   Result := 1 shl (HighestBit(x - 1) + 1);
   end;


// The largest power of two that is not greater than than X
function Floor2p(
         x     :  Integer
         )     :  Integer;
   begin
   Result := 1 shl HighestBit(x);
   end;


// Return the number of set bits in X
function BitCount(
         x        :  Integer
         )        :  Integer;
   var
         PtrBytes :  PIntBytes;
   begin
   PtrBytes := @x;
   Result :=
      ByteBitCount[PtrBytes[0]] +
      ByteBitCount[PtrBytes[1]] +
      ByteBitCount[PtrBytes[2]] +
      ByteBitCount[PtrBytes[3]] ;
   end;


// Return the number of set bits in X
function BitCount64(
         x        :  Int64
         )        :  Integer;
   var
         PtrBytes :  PInt64Bytes;
   begin
   PtrBytes := @x;
   Result :=
      ByteBitCount[PtrBytes[0]] +
      ByteBitCount[PtrBytes[1]] +
      ByteBitCount[PtrBytes[2]] +
      ByteBitCount[PtrBytes[3]] +
      ByteBitCount[PtrBytes[4]] +
      ByteBitCount[PtrBytes[5]] +
      ByteBitCount[PtrBytes[6]] +
      ByteBitCount[PtrBytes[7]] ;
   end;
   
{$RANGECHECKS ON} {$OVERFLOWCHECKS ON}

{-----------------------<< Initialization >>----------------------------------------}

// Precalculate the reversed byte table
procedure PrecalcRevBytes;
   var
         i, j, b  :  Integer;
         r        :  Byte;
   begin
   for i := 0 to High(Byte) do
      begin
      r := 0;
      for j := 0 to ByteHighBit do
         begin
         b := i and (1 shl j);
         r := r or BitShift(b, ByteHighBit - 2 * j);
         end;
      RevBytes[i] := r;
      end;
   end;


// Precalculate the bit count table
procedure PrecalcBitCount;
   var
         i     :  Integer;
   begin
   ByteBitCount[0] := 0;
   for i := 0 to High(Byte) do
      ByteBitCount[i] := ByteBitCount[i div 2] + (i and 1);
   end;

initialization //////////////////////////////////////////////////////////////////////
PrecalcBitCount;
PrecalcRevBytes;
end.
