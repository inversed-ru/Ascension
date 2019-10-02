{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit StringLists; ///////////////////////////////////////////////////////////////////
{
>> Version: 1.2

>> Description
   Basic string list implementation.

>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> ToDo
   - Use hashing if fast search is required

>> Changelog
   1.2 : 2019.10.02  * Parse procedure
   1.1 : 2018.09.09  ~ FreePascal compatibility
   1.0 : 2014.09.25  ~ Sectionized
                     + ExtractWords procedure
                     + LoadLines procedure
   0.2 : 2013.10.21  + SaveStringList function
   0.1 : 2013.05.23  + Find, AddUnique, Parse procedures
   0.0 : 2011.03.04  + Initial version
   Notation: + added, - removed, * fixed, ~ changed   
}
interface ///////////////////////////////////////////////////////////////////////////
uses
      StringUtils;
type
      TStringList =
         record
         N  :  Integer;
         _  :  array of AnsiString;
         end;

{-----------------------<< Basic operations >>--------------------------------------}

// Initialize StringList
procedure InitStringList(
   var   StringList  :  TStringList);

// Add S to StringList
procedure AddToStringList(
   var   StringList  :  TStringList;
   const S           :  AnsiString);

// Add S to StringList if S is not empty
procedure AddIfNotEmpty(
   var   StringList  :  TStringList;
   const S           :  AnsiString);

// Add S to StringList if it is not there yet. O(StringList.N) time.
procedure AddUnique(
   var   StringList  :  TStringList;
   const S           :  AnsiString);

// Return Index of S in StringList. If S is not found, Index is left unchanged and
// Fail is returned. O(StringList.N) time.
function Find(
   var   Index       :  Integer;
   const StringList  :  TStringList;
   const S           :  AnsiString
         )           :  Boolean;

{-----------------------<< Splitting Strings >>-------------------------------------}

// Parse substrings of S delimited by Delim into StringList
procedure Parse(
   var   StringList  :  TStringList;
   const S, Delim    :  AnsiString);

// Make list of Words from a string S. Words are defined as consecutive sequences of
// characters in WordChars. Transform to lower case if LowCase is True.
procedure ExtractWords(
   var   Words       :  TStringList;
   const S           :  AnsiString;
         LowCase     :  Boolean;
         WordChars   :  CharSet  =  AlphabetChars);   

{-----------------------<< Reading and Writing >>-----------------------------------}

// Save StringList to specified Path, return operation status
function SaveStringList(
   const StringList  :  TStringList;
   const Path        :  AnsiString
         )           :  Boolean;

// Load file at Path line by line as a StringList
procedure LoadLines(
   var   StringList  :  TStringList;
   const Path        :  AnsiString);

// Display StringList in console terminal
procedure DisplayStringList(
   const StringList  :  TStringList);

implementation //////////////////////////////////////////////////////////////////////
uses
      InvSys;

{-----------------------<< Basic operations >>--------------------------------------}

// Initialize StringList
procedure InitStringList(
   var   StringList  :  TStringList);
   begin
   StringList.N := 0;
   SetLength(StringList._, 0);
   end;


// Add S to StringList
procedure AddToStringList(
   var   StringList  :  TStringList;
   const S           :  AnsiString);
   begin
   Inc(StringList.N);
   if StringList.N > Length(StringList._) then
      SetLength(StringList._, 2 * StringList.N);
   StringList._[StringList.N - 1] := S;
   end;
   

// Add S to StringList if S is not empty
procedure AddIfNotEmpty(
   var   StringList  :  TStringList;
   const S           :  AnsiString);
   begin
   if S <> '' then
      AddToStringList(StringList, S);
   end;


// Return Index of S in StringList. If S is not found, Index is left unchanged and
// Fail is returned. O(StringList.N) time.
function Find(
   var   Index       :  Integer;
   const StringList  :  TStringList;
   const S           :  AnsiString
         )           :  Boolean;
   var
         i           :  Integer;
   begin
   Result := Fail;
   for i := 0 to StringList.N - 1 do
      if StringList._[i] = S then
         begin
         Index := i;
         Result := Success;
   {<---}break;
         end;
   end;


// Add S to StringList if it is not there yet. O(StringList.N) time.
procedure AddUnique(
   var   StringList  :  TStringList;
   const S           :  AnsiString);
   var
         Index       :  Integer;
         Found       :  Boolean;
   begin
   Found := Find(Index, StringList, S);
   if not Found then
      AddToStringList(StringList, S);
   end;

{-----------------------<< Splitting Strings >>-------------------------------------}

// Parse substrings of S delimited by Delim into StringList
procedure Parse(
   var   StringList  :  TStringList;
   const S, Delim    :  AnsiString);
   var
         i, j        :  Integer;
         Found       :  Boolean;
         SubS        :  AnsiString;
   begin
   InitStringList(StringList);
   i := 1;
   repeat
      j := 1 + Length(S);
      Found := GetPos(j, S, Delim, {Start:} i);
      SubS := CopyRange(S, i, j - 1);
      AddToStringList(StringList, SubS);
      i := j + Length(Delim);
   until not Found;
   end;


// Make list of Words from a string S. Words are defined as consecutive sequences of
// characters in WordChars. Transform to lower case if LowCase is True.
procedure ExtractWords(
   var   Words       :  TStringList;
   const S           :  AnsiString;
         LowCase     :  Boolean;
         WordChars   :  CharSet  =  AlphabetChars);
   var
         i1, i2, Len :  Integer;
         WorkS       :  AnsiString;
   begin
   // Transform to lower case if required
   if LowCase then
      WorkS := LowerCase(S) else
      WorkS := S;

   // Add words one by one
   InitStringList(Words);
   Len := Length(WorkS);
   i1 := 0;
   repeat
      // Find next alphabetic character
      repeat
         Inc(i1);
      until (i1 = Len) or (WorkS[i1] in WordChars);

      if WorkS[i1] in WordChars then
         begin
         // Seek end of word
         i2 := i1;
         while (i2 < Len) and (WorkS[i2] in WordChars) do
            Inc(i2);
         if not (WorkS[i2] in WordChars) then
            Dec(i2);

         // Add to word list
         AddToStringList(Words, CopyRange(WorkS, i1, i2));
         i1 := i2 + 1;
         if i1 >= Len then
   {<------}break;
         end
      else
   {<---}break;
   until False;
   end;

{-----------------------<< Reading and Writing >>-----------------------------------}

// Load file at Path line by line as a StringList
procedure LoadLines(
   var   StringList  :  TStringList;
   const Path        :  AnsiString);
   var
         TempS       :  AnsiString;
         InFile      :  Text;
   begin
   InitStringList(StringList);
   Assign(InFile, Path);
   Reset(InFile);
   repeat
      ReadLn(InFile, TempS);
      AddToStringList(StringList, TempS);
   until EoF(InFile);
   Close(InFile);
   end;


// Save StringList to specified Path, return operation status
function SaveStringList(
   const StringList  :  TStringList;
   const Path        :  AnsiString
         )           :  Boolean;
   var
         FileOut     :  Text;
         i           :  Integer;
   begin
   if OpenWrite(FileOut, Path) then
      begin
      for i := 0 to StringList.N - 1 do
         WriteLn(FileOut, StringList._[i]);
      Close(FileOut);
      Result := Success;
      end
   else
      Result := Fail;
   end;


// Display StringList in console terminal
procedure DisplayStringList(
   const StringList  :  TStringList);
   var
         i           :  LongInt;
   begin
   for i := 0 to StringList.N - 1 do
      WriteLn(StringList._[i]);
   end;

end.
 