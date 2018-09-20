{ 
Copyright (c) Peter Karpov 2010 - 2017.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit StringUtils; ///////////////////////////////////////////////////////////////////
{
>> Version: 1.13

>> Description
   Various string routines. Works with Win1251 Russian encoding. Part of InvLibs unit
   collection.
   
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> ToDo
   - Procedures vs Functions: analyze actual usage to decide. Routines in question:
       - LowerCase, UpperCase
       - FilterChars, FilterCharsReplace
       - Insert, Replace
       - ReplaceControlChars, StripWhitespace, TrimWhitespace*, MergeSpaces
       - PadLeft, PadRight
   - Test the performance of new functions based on sets
   - Optimize routines which appear to be bottlenecks in other programs
   - Try to get rid of StrUtils, Math, SysUtils dependencies
   - ReverseString
   - Endline style detection, conversion
   ? Incorporate direction into GetPos (extra overloaded version)

>> Changelog
   1.13  : 2017.12.05 ~ Changed SubstrCount and GetPos argument order for consistency
                        with other search and replace routines
                      ~ SubstringAtPosition renamed to SubstrAtPos
                      ~ EnforceDirSlash renamed to EnforceDirSep, now uses
                        platform-specific directory separator
                      ~ Non-breaking space and soft hyphen added to Whitespace
                      + NonWhitespace constant
                      ~ SeekWhitespace, SeekNonWhitespace functions replaced by 
                        SeekChars
                      ~ Freepascal compatibility
   1.12  : 2015.02.07 + IsUpperCase function
   1.11  : 2014.09.24 ~ Renamed MergeMultipleSpaces to MergeSpaces
   1.10  : 2014.08.21 + IsPrefix, IsSuffix functions
   1.9   : 2013.10.21 + Contains function
   1.8.2 : 2011.10.14 ~ Moved to generic types
   1.8.1 : 2011.09.17 * FilterCharsReplace bug (inverted Reject option)
   1.8   : 2011.08.07 ~ Split Format function into separate unit
   1.7   : 2011.07.24 ~ Replaced FillCharacter with DupeString function
   1.6   : 2011.05.15 + Boolean type support for Format function
   1.5   : 2011.04.17 + ReplacePairs and SaveString functions
   1.4   : 2011.03.20 * Sign formating in Format
                      + SubstrCount function
                      * SameText
   1.0   : 2011.03.16 ~ Overall reorganization, sectioning
                      ~ Filtering and related routines use sets instead of tables
                      + Format function
   0.0   : 2010.05.05 + Initial version
   Notation: + added, - removed, * fixed, ~ changed
}
interface ///////////////////////////////////////////////////////////////////////////
type
      CharSet = set of AnsiChar;

const
      EngChars = ['a' .. 'z', 'A' .. 'Z'];
      RusChars =
        ['à', 'á', 'â', 'ã', 'ä', 'å', '¸', 'æ', 'ç', 'è', 'é',
         'ê', 'ë', 'ì', 'í', 'î', 'ï', 'ð', 'ñ', 'ò', 'ó', 'ô',
         'õ', 'ö', '÷', 'ø', 'ù', 'ú', 'ü', 'û', 'ý', 'þ', 'ÿ',
         'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', '¨', 'Æ', 'Ç', 'È', 'É',
         'Ê', 'Ë', 'Ì', 'Í', 'Î', 'Ï', 'Ð', 'Ñ', 'Ò', 'Ó', 'Ô',
         'Õ', 'Ö', '×', 'Ø', 'Ù', 'Ú', 'Ü', 'Û', 'Ý', 'Þ', 'ß'];

      AlphabetChars = EngChars + RusChars;
      Numerals = ['0' .. '9'];

      ControlChars   = [#0 .. #31, #127];
      Whitespace     = [#0 .. #32, #127, #160, #173];
      NonWhitespace  = [#0 .. #255] - Whitespace;

      TableLowerCase = 'abcdefghijklmnopqrstuvwxyzàáâãäå¸æçèéêëìíîïðñòóôõö÷øùúüûýþÿ';
      TableUpperCase = 'ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅ¨ÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÜÛÝÞß';

{-----------------------<< Filtering >>---------------------------------------------}

// Replace upper case characters by lower case
function LowerCase(
   const S        :  AnsiString
         )        :  AnsiString;

// Replace lower case characters by upper case
function UpperCase(
   const S        :  AnsiString
         )        :  AnsiString;

// Return whether C is an upper case character.
// For non-alphabetic characters, False is returned.
function IsUpperCase(
         C        :  AnsiChar
         )        :  Boolean;

// Skip symbols filter. Reject = True means that symbols in the Filter are skipped,
// False - all other symbols are skipped.
procedure FilterChars(
   var   S        :  AnsiString;
   const Filter   :  CharSet;
         Reject   :  Boolean);
         

// Replace symbols filter. Reject = True means that symbols in the Filter are 
// replaced, False - all other symbols are replaced.
procedure FilterCharsReplace(
   var   S           :  AnsiString;
   const Filter      :  CharSet;
         Reject      :  Boolean;
         Replacement :  AnsiChar);

{-----------------------<< Substring manipulation >>--------------------------------}

// Return the substring spanning from IndexFrom to IndexTo
function CopyRange(
   const S           :  AnsiString;
         IndexFrom,
         IndexTo     :  Integer
         )           :  AnsiString;

// Insert Substr into S after Index characters
procedure Insert(
   var   S        :  AnsiString;
         Index    :  Integer;
   const Substr   :  AnsiString);

// Return the prefix of S of given Size
function Prefix(
   const S     :  AnsiString;
         Size  :  Integer
         )     :  AnsiString;

// Return the suffix of S of given Size
function Suffix(
   const S     :  AnsiString;
         Size  :  Integer
         )     :  AnsiString;

// Return suffix of S starting from Index
function SuffixFrom(
   const S     :  AnsiString;
         Index :  Integer
         )     :  AnsiString;

// Return whether SubS is a prefix of S
function IsPrefix(
   const SubS, S  :  AnsiString
         )        :  Boolean;

// Return whether SubS is a suffix of S
function IsSuffix(
   const SubS, S  :  AnsiString
         )        :  Boolean;

{-----------------------<< Search and replace >>------------------------------------}

// Return the index of the first character from Chars found in S starting from Start
// and going in the direction Dir, or a the first out of bounds index if nothing was
// found
function SeekChars(
   const S           :  AnsiString;
   const Chars       :  CharSet;
         Start, Dir  :  Integer
         )           :  Integer;

// Search for Substr within S starting from Start. Return the Index of the first
// match and operation status. If the search fails, Index is left unchanged. If 
// Substr is empty, no match will be found.
function GetPos(
   var   Index       :  Integer;
   const S, Substr   :  AnsiString;
         Start       :  Integer     =  1
         )           :  Boolean;

// Replace all occurences of Old within S with New. Replacement is non-recursive and
// case-sensitive.
function Replace(
   const S,
         Old,
         New         :  AnsiString
         )           :  AnsiString;
         
// Replace all substrings of the form 'OldLeft + ??? + OldRight' within S with New, 
// where ??? is any substring. Replacement is non-recursive and case-sensitive.
function ReplacePairs(
   const S,
         OldLeft,
         OldRight,
         New         :  AnsiString
         )           :  AnsiString;

// Number of occurences of Substr within S (overlaps do not count)
function SubstrCount(
   const S, Substr      :  AnsiString
         )              :  Integer;

// Return whether S contains Substr
function Contains(
   const S, Substr   :  AnsiString
         )           :  Boolean;

{-----------------------<< Comparison >>--------------------------------------------}

// Case-insensitive string comparison
function SameText(
   const S1, S2   :  AnsiString
         )        :  Boolean;

// Indicates whether Substr is located in S at Index
function SubstrAtPos(
   const S, Substr   :  AnsiString;
         Index       :  Integer
         )           :  Boolean;
         
{-----------------------<< Whitespace >>--------------------------------------------}

// Replace linefeeds, tabs and anything < #32 with spaces
procedure ReplaceControlChars(
   var   S  :  AnsiString);

// Remove any whitespace symbols
procedure StripWhitespace(
   var   S  :  AnsiString);

// Remove any whitespace symbols on the left side
procedure TrimWhitespaceLeft(
   var   S  :  AnsiString);

// Remove any whitespace symbols on the right side
procedure TrimWhitespaceRight(
   var   S  :  AnsiString);

// Remove any whitespace symbols on both right and left sides
procedure TrimWhitespaceSides(
   var   S        :  AnsiString);

// Merge runs of multiple spaces into single spaces
procedure MergeSpaces(
   var   S     :  AnsiString);

// Indicates whether S contains any whitespace symbols
function ContainsWhitespace(
   const S  :  AnsiString
         )  :  Boolean;

{-----------------------<< Stuffing >>----------------------------------------------}

// Return the string consisting of N repeats of S.
function DupeString(
   const S        :  AnsiString;
         N        :  Integer
         )        :  AnsiString;

// Pad S at the left with C up to the total length Len. 
// If Length(S) >= Len, S is left unchanged.
function PadLeft(
   const S        :  AnsiString;
         C        :  AnsiChar;
         Len      :  Integer
         )        :  AnsiString;         
         
// Pad S at the right with C up to the total length Len. 
// If Length(S) >= Len, S is left unchanged.
function PadRight(
   const S        :  AnsiString;
         C        :  AnsiChar;
         Len      :  Integer
         )        :  AnsiString;

{-----------------------<< Misc >>--------------------------------------------------}

// Load a string S from a file located at Path, return operation status
function LoadString(
   var   S              :  AnsiString;
   const Path           :  AnsiString;
         StripControl   :  Boolean
         )              :  Boolean;

// Save the string S to a file located at Path, return operation status
function SaveString(
   const S              :  AnsiString;
   const Path           :  AnsiString
         )              :  Boolean;         
         
// Make sure that the directory Dir ends with a directory separator
procedure EnforceDirSep(
   var   Dir   :  AnsiString);

implementation //////////////////////////////////////////////////////////////////////
uses
      SysUtils,      // IntToStr, FloatToStrF
      invSys,
      StrUtils;      // PosEx

{-----------------------<< Filtering >>---------------------------------------------}

// Replace upper case characters by lower case
function LowerCase(
   const S        :  AnsiString
         )        :  AnsiString;
   var
         i, Found :  Integer;
   begin
   Result := S;
   for i := 1 to Length(S) do
      if GetPos(Found, TableUpperCase, Result[i]) = Success then
         Result[i] := TableLowerCase[Found];
   end;


// Replace lower case characters by upper case
function UpperCase(
   const S        :  AnsiString
         )        :  AnsiString;
   var
         i, Found :  Integer;
   begin
   Result := S;
   for i := 1 to Length(S) do
      if GetPos(Found, TableLowerCase, Result[i]) = Success then
         Result[i] := TableUpperCase[Found];
   end;


// Return whether C is an upper case character.
// For non-alphabetic characters, False is returned.
function IsUpperCase(
         C        :  AnsiChar
         )        :  Boolean;
   var
         i        :  Integer;
   begin
   Result := GetPos(i, TableUpperCase, C) = Success;
   end;


// Skip symbols filter. Reject = True means that symbols in the Filter are skipped,
// False - all other symbols are skipped.
procedure FilterChars(
   var   S        :  AnsiString;
   const Filter   :  CharSet;
         Reject   :  Boolean);
   var
         WorkS    :  AnsiString;
         i, Len,
         FinalLen :  Integer;
         ptrFrom,
         ptrTo    :  ^Byte;
   begin
   Len := Length(S);
   SetLength(WorkS, Len);
   ptrFrom := @    S[1];
   ptrTo   := @WorkS[1];
   FinalLen := 0;
   for i := 1 to Len do
      begin
      if (Char(ptrFrom^) in Filter) xor Reject then
         begin
         ptrTo^ := ptrFrom^;
         Inc(ptrTo);
         Inc(FinalLen);
         end;
      Inc(ptrFrom);
      end;
   SetLength(WorkS, FinalLen);
   S := WorkS;
   end;


// Replace symbols filter. Reject = True means that symbols in the Filter are 
// replaced, False - all other symbols are replaced.
procedure FilterCharsReplace(
   var   S           :  AnsiString;
   const Filter      :  CharSet;
         Reject      :  Boolean;
         Replacement :  AnsiChar);
   var
         i, Len      :  Integer;
   begin
   Len := Length(S);
   for i := 1 to Len do
      if (S[i] in Filter) xor not Reject then
         S[i] := Replacement;
   end;

{-----------------------<< Substring manipulation >>--------------------------------}

// Return the substring spanning from IndexFrom to IndexTo
function CopyRange(
   const S           :  AnsiString;
         IndexFrom,
         IndexTo     :  Integer
         )           :  AnsiString;
   begin
   Result := Copy(S, IndexFrom, IndexTo - IndexFrom + 1);
   end;


// Insert Substr into S after Index characters
procedure Insert(
   var   S        :  AnsiString;
         Index    :  Integer;
   const Substr   :  AnsiString);
   begin
   S := CopyRange(S, 1, Index) + Substr + CopyRange( S, Index + 1, Length(S) );
   end;


// Return prefix of S of given Size
function Prefix(
   const S     :  AnsiString;
         Size  :  Integer
         )     :  AnsiString;
   begin
   Result := Copy(S, {Index:} 1, Size);
   end;


// Return the suffix of S of given Size
function Suffix(
   const S     :  AnsiString;
         Size  :  Integer
         )     :  AnsiString;
   begin
   Result := Copy(S, {Index:} Length(S) - Size + 1, Size);
   end;


// Return the suffix of S starting from Index
function SuffixFrom(
   const S     :  AnsiString;
         Index :  Integer
         )     :  AnsiString;
   begin
   Result := CopyRange(S, Index, Length(S));
   end;


// Return whether SubS is a prefix of S
function IsPrefix(
   const SubS, S  :  AnsiString
         )        :  Boolean;
   begin
   Result := SubS = Prefix(S, Length(SubS));
   end;


// Return whether SubS is a suffix of S
function IsSuffix(
   const SubS, S  :  AnsiString
         )        :  Boolean;
   begin
   Result := SubS = Suffix(S, Length(SubS));
   end;

{-----------------------<< Search and replace >>------------------------------------}

// Return the index of the first character from Chars found in S starting from Start
// and going in the direction Dir, or a the first out of bounds index if nothing was
// found
function SeekChars(
   const S           :  AnsiString;
   const Chars       :  CharSet;
         Start, Dir  :  Integer
         )           :  Integer;
   var
         i, Len      :  Integer;
   begin
   Assert(Start <> 0);
   i := Start;
   Len := Length(S);
   while (i > 0) and (i <= Len) and not (S[i] in Chars) do
      i := i + Dir;
   Result := i;
   end;
   

// Search for Substr within S starting from Start. Return the Index of the first
// match and operation status. If the search fails, Index is left unchanged. If 
// Substr is empty, no match will be found.
function GetPos(
   var   Index       :  Integer;
   const S, Substr   :  AnsiString;
         Start       :  Integer     =  1
         )           :  Boolean;
   var
         OldIndex :  Integer;
   begin
   OldIndex := Index;
   Index := PosEx(Substr, S, Start);
   if Index <> 0 then
      Result := Success
   else
      begin
      Index := OldIndex;
      Result := Fail;
      end;
   end;


// Replace all occurences of Old within S with New. Replacement is non-recursive and
// case-sensitive.
function Replace(
   const S,
         Old,
         New         :  AnsiString
         )           :  AnsiString;
   var
         LenOld      :  Integer;
          NowIndex,
         PrevIndex   :  Integer;
   begin
   LenOld := Length(Old);
   PrevIndex := 1;
   Result := '';
   while GetPos(NowIndex, S, Old, {Start:} PrevIndex) = Success do
      begin
      Result := Result + CopyRange(S, PrevIndex, NowIndex - 1) + New;
      PrevIndex := NowIndex + LenOld;
      end;
   Result := Result + CopyRange( S, PrevIndex, Length(S) );
   end;


// Replace all substrings of the form 'OldLeft + ??? + OldRight' within S with New, 
// where ??? is any substring. Replacement is non-recursive and case-sensitive.
function ReplacePairs(
   const S,
         OldLeft,
         OldRight,
         New         :  AnsiString
         )           :  AnsiString;
   var
         LenRight    :  Integer;
          LeftIndex,
         RightIndex,
          PrevIndex  :  Integer;
   begin
   LenRight := Length(OldRight);
   PrevIndex := 1;
   Result := '';
   while (GetPos( LeftIndex, S, OldLeft , {Start:} PrevIndex) = Success) and
         (GetPos(RightIndex, S, OldRight, {Start:} LeftIndex) = Success) do
      begin
      Result := Result + CopyRange(S, PrevIndex, LeftIndex - 1) + New;
      PrevIndex := RightIndex + LenRight;
      end;
   Result := Result + CopyRange( S, PrevIndex, Length(S) );
   end;


// Number of occurences of Substr within S (overlaps do not count)
function SubstrCount(
   const S, Substr      :  AnsiString
         )              :  Integer;
   var
         i, LenSubstr   :  Integer;
   begin
   Result := 0;
   i := 1;
   LenSubstr := Length(Substr);
   while GetPos(i, S, Substr, {Start:} i) = Success do
      begin
      Inc(Result);
      i := i + LenSubstr;
      end;
   end;


// Return whether S contains Substr
function Contains(
   const S, Substr   :  AnsiString
         )           :  Boolean;
   begin
   Result := SubstrCount(S, Substr) > 0;
   end;

{-----------------------<< Comparison >>--------------------------------------------}

// Case-insensitive string comparison
function SameText(
   const S1, S2   :  AnsiString
         )        :  Boolean;
   var
         LowS1,
         LowS2    :  AnsiString;
   begin
   LowS1 := LowerCase(S1);
   LowS2 := LowerCase(S2);
   Result := LowS1 = LowS2;
   end;


// Indicates whether Substr is located in S at Index
// #HOSTSPOT, tried but failed optimizations:
// Using pointers to Delim and Input characters.
function SubstrAtPos(
   const S, Substr   :  AnsiString;
         Index       :  Integer
         )           :  Boolean;
   var
         Scan,
         LenS,
         LenSubstr   :  Integer;
   begin
   Result := False;
   if Index >= 1 then
      begin
      LenS      := Length(S);
      LenSubstr := Length(Substr);
      Scan := 0;
      while ( (Index + Scan) <= LenS ) and (Substr[1 + Scan] = S[Index + Scan]) do
         if Scan = (LenSubstr - 1) then
            begin
            Result := True;
            break;
            end
         else
            Inc(Scan);
      end;
   end;

{-----------------------<< Whitespace >>--------------------------------------------}

// Replace linefeeds, tabs and anything < #32 with spaces
procedure ReplaceControlChars(
   var   S  :  AnsiString);
   begin
   FilterCharsReplace(S, ControlChars, {Reject:} True, ' ');
   end;


// Remove any whitespace symbols
procedure StripWhitespace(
   var   S  :  AnsiString);
   begin
   FilterChars(S, Whitespace, {Reject:} True);
   end;


// Remove any whitespace symbols on the left side
procedure TrimWhitespaceLeft(
   var   S  :  AnsiString);
   var
         i  :  Integer;
   begin
   i := SeekChars(S, NonWhitespace, {Start:} 1, {Dir:} +1);
   S := CopyRange( S, i, Length(S) );
   end;


// Remove any whitespace symbols on the right side
procedure TrimWhitespaceRight(
   var   S  :  AnsiString);
   var
         i  :  Integer;
   begin
   i := SeekChars(S, NonWhitespace, {Start:} Length(S), {Dir:} -1);
   S := Copy(S, 1, i);
   end;


// Remove any whitespace symbols on both right and left sides
procedure TrimWhitespaceSides(
   var   S        :  AnsiString);
   begin
   TrimWhitespaceLeft (S);
   TrimWhitespaceRight(S);
   end;


// Merge runs of multiple spaces into single spaces
procedure MergeSpaces(
   var   S     :  AnsiString);
   var
         WorkS :  AnsiString;
         iFrom,
         iTo,
         Len   :  Integer;
   begin
   Len := Length(S);
   SetLength(WorkS, Len);
   iFrom := 1;
   iTo   := 1;
   while iFrom <= Len do
      begin
      WorkS[iTo] := S[iFrom];
      if S[iFrom] = ' ' then
         begin
         while (iFrom <= Len) and (S[iFrom] = ' ') do
            Inc(iFrom);
         Dec(iFrom);
         end;
      Inc(iTo);
      Inc(iFrom);
      end;
   SetLength(WorkS, iTo - 1);
   S := WorkS;
   end;


// Indicates whether S contains any whitespace symbols
function ContainsWhitespace(
   const S  :  AnsiString
         )  :  Boolean;
   begin
   Result := SeekChars(S, Whitespace, {Start:} 1, {Dir:} +1) <= Length(S);
   end;

{-----------------------<< Stuffing >>----------------------------------------------}

// Return the string consisting of N repeats of S.
function DupeString(
   const S        :  AnsiString;
         N        :  Integer
         )        :  AnsiString;
   var
         Len, i, j:  Integer;
   begin
   Len := Length(S);
   SetLength(Result, N * Len);
   j := 1;
   for i := 1 to N * Len do
      begin
      Result[i] := S[j];
      Inc(j);
      if j > Len then
         j := 1;
      end;
   end;


// Pad S at the left with C up to the total length Len. 
// If Length(S) >= Len, S is left unchanged.
function PadLeft(
   const S        :  AnsiString;
         C        :  AnsiChar;
         Len      :  Integer
         )        :  AnsiString;
   var
         NowLen   :  Integer;
   begin
   NowLen := Length(S);
   if NowLen < Len then
      Result := DupeString(C, Len - NowLen) + S
   else
      Result := S;
   end;


// Pad S at the right with C up to the total length Len. 
// If Length(S) >= Len, S is left unchanged.
function PadRight(
   const S        :  AnsiString;
         C        :  AnsiChar;
         Len      :  Integer
         )        :  AnsiString;
   var
         NowLen   :  Integer;
   begin
   NowLen := Length(S);
   if NowLen < Len then
      Result := S + DupeString(C, Len - NowLen)
   else
      Result := S;
   end;

{-----------------------<< Misc >>--------------------------------------------------}

// Load a string S from a file located at Path, return operation status
function LoadString(
   var   S              :  AnsiString;
   const Path           :  AnsiString;
         StripControl   :  Boolean
         )              :  Boolean;
   var
         FileIn         :  TByteFile;
         Len            :  Integer;
   begin
   if OpenRead(FileIn, Path) then
      begin
      Len := FileSize(FileIn);
      SetLength(S, Len);
      BlockRead(FileIn, S[1], Len);
      if StripControl then
         ReplaceControlChars(S);
      Close(FileIn);
      Result := Success;
      end
   else
      Result := Fail;
   end;


// Save the string S to a file located at Path, return operation status
function SaveString(
   const S              :  AnsiString;
   const Path           :  AnsiString
         )              :  Boolean;
   var
         FileOut        :  Text;
   begin
   if OpenWrite(FileOut, Path) then
      begin
      WriteLn(FileOut, S);
      Close(FileOut);
      Result := Success;
      end
   else
      Result := Fail;
   end;


// Make sure that the directory Dir ends with a directory separator
procedure EnforceDirSep(
   var   Dir   :  AnsiString);
   begin
   if (Dir <> '') and (Dir[Length(Dir)] <> PathDelim) then
      Dir := Dir + PathDelim;
   end;

end.
