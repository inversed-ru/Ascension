{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained 
with the works, so that any entity that uses the works is notified of this 
instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit HashFunctions; ////////////////////////////////////////////////////////////
{
>> Version: 0.2

>> Description
   Hash functions for 8-bit strings. Part of InvLibs unit collection.
   
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> Usage
   Best hash functions based on number of collisions:
      OneAtATime
      Ramakrishna
      Sedgewick
      Larson
      FNV1a
      Bernstein
      KR1313
      KR131313
      KR13131
   Other functions are for testing purposes and not recommended.

>> ToDo
   - General testing framework
   - Xor folding?

>> Changelog
   0.2   : 2018.09.09 ~ FreePascal compatibility
   0.1.1 : 2011.10.14 ~ Moved to generic types
   0.0   : 2010.05.05 + Initial version
   Notation: + added, - removed, * fixed, ~ changed   
}
interface //////////////////////////////////////////////////////////////////////

type
      THash =  LongWord;

      TProcHash =
         function (
            const S     :  AnsiString
                  )     :  THash;

{-----------------------<< Mul-Add >>------------------------------------------}

function HashBernstein(
   const S     :  AnsiString
         )     :  THash;

function HashKR1313(
   const S     :  AnsiString
         )     :  THash;

function HashKR13131(
   const S     :  AnsiString
         )     :  THash;

function HashKR131313(
   const S     :  AnsiString
         )     :  THash;

function HashLarson(
   const S     :  AnsiString
         )     :  THash;

function HashSdbm(
   const S     :  AnsiString
         )     :  THash;

{-----------------------<< Mul-Xor >>------------------------------------------}

function HashBernsteinXor(
   const S     :  AnsiString
         )     :  THash;

function HashFNV1(
   const S     :  AnsiString
         )     :  THash;

function HashFNV1a(
   const S     :  AnsiString
         )     :  THash;

{-----------------------<< Bitshifts >>----------------------------------------}

function HashVakulenko(
   const S     :  AnsiString
         )     :  THash;

function HashRamakrishna(
   const S     :  AnsiString
         )     :  THash;

function HashOneAtATime(
   const S     :  AnsiString
         )     :  THash;

{-----------------------<< Misc >>---------------------------------------------}

function HashSedgewick(
   const S     :  AnsiString
         )     :  THash;

function HashYuriev(
   const S     :  AnsiString
         )     :  THash;

{-----------------------<< Flawed >>-------------------------------------------}

function HashFlawedRot(
   const S     :  AnsiString
         )     :  THash;

{------------------------------------------------------------------------------}

const
      // #TEST
      NHashProcedures   = 15;
      ProcHashTable     : array [1 .. NHashProcedures] of TProcHash =
      (// Good
       HashOneAtATime,
       HashRamakrishna,
       HashSedgewick,
       HashLarson,
       HashFNV1a,
       HashBernstein,
       HashKR1313,
       HashKR131313,
       HashKR13131,
       // So-so
       HashYuriev,
       HashFNV1,
       // Bad
       HashVakulenko,
       HashBernsteinXor,
       HashSdbm,
       // Awful
       HashFlawedRot);

implementation /////////////////////////////////////////////////////////////////

{-----------------------<< Mul-Add >>------------------------------------------}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

function HashBernstein(
   const S     :  AnsiString
         )     :  THash;
   const
         Hash0 =  5381;
         hMul  =  33;
   var
         Hash  :  THash;
         i     :  Integer;
   begin
   Hash := Hash0;
   for i := 1 to Length(S) do
      Hash := hMul * Hash + Ord(S[i]);
   Result := Hash;
   end;


// Kernighan & Ritchie
function HashKR1313(
   const S     :  AnsiString
         )     :  THash;
   const
         Hash0 =  0;
         hMul  =  1313;
   var
         Hash  :  THash;
         i     :  Integer;
   begin
   Hash := Hash0;
   for i := 1 to Length(S) do
      Hash := hMul * Hash + Ord(S[i]);
   Result := Hash;
   end;


function HashKR13131(
   const S     :  AnsiString
         )     :  THash;
   const
         Hash0 =  0;
         hMul  =  13131;
   var
         Hash  :  THash;
         i     :  Integer;
   begin
   Hash := Hash0;
   for i := 1 to Length(S) do
      Hash := hMul * Hash + Ord(S[i]);
   Result := Hash;
   end;


function HashKR131313(
   const S     :  AnsiString
         )     :  THash;
   const
         Hash0 =  0;
         hMul  =  131313;
   var
         Hash  :  THash;
         i     :  Integer;
   begin
   Hash := Hash0;
   for i := 1 to Length(S) do
      Hash := hMul * Hash + Ord(S[i]);
   Result := Hash;
   end;


// Paul Larson
function HashLarson(
   const S     :  AnsiString
         )     :  THash;
   const
         Hash0 =  0;
         hMul  =  101;
   var
         Hash  :  THash;
         i     :  Integer;
   begin
   Hash := Hash0;
   for i := 1 to Length(S) do
      Hash := hMul * Hash + Ord(S[i]);
   Result := Hash;
   end;


function HashSdbm(
   const S     :  AnsiString
         )     :  THash;
   const
         Hash0 =  0;
         hMul  =  65599;
   var
         Hash  :  THash;
         i     :  Integer;
   begin
   Hash := Hash0;
   for i := 1 to Length(S) do
      Hash := hMul * Hash + Ord(S[i]);
   Result := Hash;
   end;

{-----------------------<< Mul-Xor >>------------------------------------------}

function HashBernsteinXor(
   const S     :  AnsiString
         )     :  THash;
   const
         Hash0 =  5381;
         hMul  =  33;
   var
         Hash  :  THash;
         i     :  Integer;
   begin
   Hash := Hash0;
   for i := 1 to Length(S) do
      Hash := (hMul * Hash) xor Ord(S[i]);
   Result := Hash;
   end;


function HashFNV1(
   const S     :  AnsiString
         )     :  THash;
   const
         Hash0 =  2166136261;
         hMul  =  16777619;
   var
         Hash  :  THash;
         i     :  Integer;
   begin
   Hash := Hash0;
   for i := 1 to Length(S) do
      Hash := (hMul * Hash) xor Ord(S[i]);
   Result := Hash;
   end;


// Supposed to be better than basic FNV
function HashFNV1a(
   const S     :  AnsiString
         )     :  THash;
   const
         Hash0 =  2166136261;
         hMul  =  16777619;
   var
         Hash  :  THash;
         i     :  Integer;
   begin
   Hash := Hash0;
   for i := 1 to Length(S) do
      Hash := hMul * ( Hash xor Ord(S[i]) );
   Result := Hash;
   end;

{-----------------------<< Bitshifts >>----------------------------------------}

// Serge Vakulenko
function HashVakulenko(
   const S     :  AnsiString
         )     :  THash;
   const
         Hash0 =  0;
   var
         Hash  :  THash;
         i     :  Integer;
   begin
   Hash := Hash0;
   for i := 1 to Length(S) do
      begin
      Hash := Hash + Ord(S[i]);
      Hash := Hash - ( (Hash shl 13) or (Hash shr 19) );
      end;
   Result := Hash;
   end;


function HashRamakrishna(
   const S     :  AnsiString
         )     :  THash;
   const
         // 1315423911 is used in Arash Partow's library
         // but in my tests there was no significant difference
         Hash0 =  0;
   var
         Hash  :  THash;
         i     :  Integer;
   begin
   Hash := Hash0;
   for i := 1 to Length(S) do
      Hash := Hash xor ( (Hash shl 5) + (Hash shr 2) + Ord(S[i]) );
   Result := Hash;
   end;


function HashOneAtATime(
   const S     :  AnsiString
         )     :  THash;
   const
         Hash0 =  0;
   var
         Hash  :  THash;
         i     :  Integer;
   begin
   Hash := Hash0;
   for i := 1 to Length(S) do
      begin
      Hash := Hash + Ord(S[i]);
      Hash := Hash +   (Hash shl 10);
      Hash := Hash xor (Hash shr 6);
      end;
   Hash := Hash +   (Hash shl 3);
   Hash := Hash xor (Hash shr 11);
   Hash := Hash +   (Hash shl 15);
   Result := Hash;
   end;

{-----------------------<< Misc >>---------------------------------------------}

// Robert Sedgwick, Algorithms in C
// Good constants from Arash Partow's library
function HashSedgewick(
   const S     :  AnsiString
         )     :  THash;
   const
         Hash0 =  0;
         a0    =  63689;
         b     =  378551;
   var
         Hash, 
         a     :  THash;
         i     :  Integer;
   begin
   Hash  := Hash0;
   a     := a0;
   for i := 1 to Length(S) do
      begin
      Hash := a * Hash + Ord(S[i]);
      a := a * b;
      end;
   Result := Hash;
   end;


// Congruential generator proposed by Leonid Yuriev.
function HashYuriev(
   const S     :  AnsiString
         )     :  THash;
   const
         Hash0 =  0;
         hMul  =  1664525;
         hAdd  =  1013904223;
   var
         Hash  :  THash;
         i     :  Integer;
   begin
   Hash := Hash0;
   for i := 1 to Length(S) do
      Hash := hMul * Hash + Ord(S[i]) + hAdd;
   Result := Hash;
   end;

{-----------------------<< Flawed >>-------------------------------------------}

// This hash is sometimes mentioned, but is in fact very bad
function HashFlawedRot(
   const S     :  AnsiString
         )     :  THash;
   const
         Hash0 =  0;
   var
         Hash  :  THash;
         i     :  Integer;
   begin
   Hash := Hash0;
   for i := 1 to Length(S) do
      Hash := (Hash shl 5) xor (Hash shr 27) xor Ord(S[i]) ;
   Result := Hash;
   end;

{$OVERFLOWCHECKS ON}
{$RANGECHECKS ON}
end.
 