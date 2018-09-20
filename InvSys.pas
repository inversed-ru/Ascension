{ 
Copyright (c) Peter Karpov 2010 - 2017.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit InvSys; ////////////////////////////////////////////////////////////////////////
{
>> Version: 1.0
   
>> Description
   Contains some basic convenient constants and routines. Success and Fail constants
   are supposed to be used by functions that return operation status. Part of InvLibs
   unit collection.

>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> Changelog 
   1.0   : 2017.12.02  - Timing and system sections (crossplatform compatibility)
                       - Endline, use Lazarus's LineEnding instead
                       - UniInt function (redundant)
                       - Bipolar type
                       ~ BiInt function renamed to Sign
                       - RandBool function moved to RandVars
                       ~ FreePascal compatibility
   0.11  : 2015.08.05  + ExecNewProcess function
   0.10  : 2015.04.17  + TIntFile and TRealFile types,
                         corresponding OpenRead and OpenWrite functions
   0.9   : 2014.10.14  + RotateLeft, RotateRight procedures
                       ~ Section order
   0.8   : 2013.04.28  + UniInt, BiInt functions
   0.7   : 2011.08.03  + SetConsoleSize procedure
   0.5   : 2011.06.04  + Swap procedures
   0.2   : 2011.03.17  + Bipolar type
   0.0   : 2010.05.05  + Initial version
   Notation: + added, - removed, * fixed, ~ changed
}
interface ///////////////////////////////////////////////////////////////////////////
const
      Tab      =  #9;
      Fail     =  False;
      Success  =  True;

type
      PReal = ^Real;

      TProcedure = procedure;

      Nothing = record end;

      TByteFile = file of Byte;
      TRealFile = file of Real;
       TIntFile = file of Integer;

{-----------------------<< Basic >>-------------------------------------------------}

// Convert True to +1, False to -1
function Sign(
      b  :  Boolean
      )  :  Integer;
      overload;

// Swap a and b
procedure Swap(
   var   a, b     :  Integer);
         overload;

procedure Swap(
   var   a, b     :  Real);
         overload;

procedure Swap(
   var   a, b     :  Pointer);
         overload;

// Cyclic left shift: (a, b, c) -> (b, c, a)
procedure RotateLeft(
   var   a, b, c  :  Integer);

// Cyclic right shift: (a, b, c) -> (c, a, b)
procedure RotateRight(
   var   a, b, c  :  Integer);

{-----------------------<< Files >>-------------------------------------------------}

// Open F at Path for reading, return status
function OpenRead(
   var   F        :  TByteFile;
   const Path     :  AnsiString
         )        :  Boolean;
         overload;

function OpenRead(
   var   F        :  TIntFile;
   const Path     :  AnsiString
         )        :  Boolean;
         overload;

function OpenRead(
   var   F        :  TRealFile;
   const Path     :  AnsiString
         )        :  Boolean;
         overload;

function OpenRead(
   var   F        :  Text;
   const Path     :  AnsiString
         )        :  Boolean;
         overload;

// Open F at Path for writing, return status
function OpenWrite(
   var   F        :  TByteFile;
   const Path     :  AnsiString
         )        :  Boolean;
         overload;

function OpenWrite(
   var   F        :  TIntFile;
   const Path     :  AnsiString
         )        :  Boolean;
         overload;

function OpenWrite(
   var   F        :  TRealFile;
   const Path     :  AnsiString
         )        :  Boolean;
         overload;

function OpenWrite(
   var   F        :  Text;
   const Path     :  AnsiString
         )        :  Boolean;
         overload;

implementation //////////////////////////////////////////////////////////////////////

{-----------------------<< Basic >>-------------------------------------------------}

// Convert True to +1, False to -1
function Sign(
      b  :  Boolean
      )  :  Integer;
      overload;
   begin
   Result := 2 * Ord(b) - 1;
   end;


// #COPYPASTA need generics or preprocessor for other data types
// Swap a and b
procedure Swap(
   var   a, b     :  Integer);
         overload;
   var
         Temp     :  Integer;
   begin
   Temp  := a;
   a     := b;
   b     := Temp;
   end;

procedure Swap(
   var   a, b     :  Real);
         overload;
   var
         Temp     :  Real;
   begin
   Temp  := a;
   a     := b;
   b     := Temp;
   end;

procedure Swap(
   var   a, b        :  Pointer);
         overload;
   var
         Temp        :  Pointer;
   begin
   Temp  := a;
   a     := b;
   b     := Temp;
   end;


// Cyclic left shift: (a, b, c) -> (b, c, a)
procedure RotateLeft(
   var   a, b, c  :  Integer);
   var
         Temp     :  Integer;
   begin
   Temp := a;
   a    := b;
   b    := c;
   c    := Temp
   end;


// Cyclic right shift: (a, b, c) -> (c, a, b)
procedure RotateRight(
   var   a, b, c  :  Integer);
   var
         Temp     :  Integer;
   begin
   Temp := c;
   c    := b;
   b    := a;
   a    := Temp
   end;

{-----------------------<< Files >>-------------------------------------------------}
{$IOCHECKS OFF}

// Open F at Path for reading, return status
function OpenRead(
   var   F        :  TByteFile;
   const Path     :  AnsiString
         )        :  Boolean;
         overload;
   begin
   Assign(F, Path);
   Reset(F);
   if IOResult = 0 then
      Result := Success else
      Result := Fail;
   end;


function OpenRead(
   var   F        :  TRealFile;
   const Path     :  AnsiString
         )        :  Boolean;
         overload;
   begin
   Assign(F, Path);
   Reset(F);
   if IOResult = 0 then
      Result := Success else
      Result := Fail;
   end;


function OpenRead(
   var   F        :  TIntFile;
   const Path     :  AnsiString
         )        :  Boolean;
         overload;
   begin
   Assign(F, Path);
   Reset(F);
   if IOResult = 0 then
      Result := Success else
      Result := Fail;
   end;


function OpenRead(
   var   F        :  Text;
   const Path     :  AnsiString
         )        :  Boolean;
         overload;
   begin
   Assign(F, Path);
   Reset(F);
   if IOResult = 0 then
      Result := Success else
      Result := Fail;
   end;


// Open F at Path for writing, return status
function OpenWrite(
   var   F        :  TByteFile;
   const Path     :  AnsiString
         )        :  Boolean;
         overload;
   begin
   Assign(F, Path);
   Rewrite(F);
   if IOResult = 0 then
      Result := Success else
      Result := Fail;
   end;


function OpenWrite(
   var   F        :  TRealFile;
   const Path     :  AnsiString
         )        :  Boolean;
         overload;
   begin
   Assign(F, Path);
   Rewrite(F);
   if IOResult = 0 then
      Result := Success else
      Result := Fail;
   end;


function OpenWrite(
   var   F        :  TIntFile;
   const Path     :  AnsiString
         )        :  Boolean;
         overload;
   begin
   Assign(F, Path);
   Rewrite(F);
   if IOResult = 0 then
      Result := Success else
      Result := Fail;
   end;


function OpenWrite(
   var   F        :  Text;
   const Path     :  AnsiString
         )        :  Boolean;
         overload;
   begin
   Assign(F, Path);
   Rewrite(F);
   if IOResult = 0 then
      Result := Success else
      Result := Fail;
   end;

{$IOCHECKS ON}

end.
 
