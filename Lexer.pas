{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit Lexer; /////////////////////////////////////////////////////////////////////////
{
>> Version: 1.2

>> Description
   Universal  lexical  analysis  unit:  produces  a sequence  of tokens from a string
   input. The user must supply delimiters that separate lexemes (sequences of symbols
   that form the tokens) and procedures to classify the lexemes into tokens.  Part of
   InvLibs unit collection.
   
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> Usage
   Call Tokenize after setting the delimiters and lexeme classification functions:
   Tokenize(
      @Tokens,    // Pointer to your tokens
      Input,      // Input string
      Delimiters, // TDelimiters variable
      LexerProcs, // TLexerProcs variable
      Errors);    // Returns list of errors
   See IniConfigs unit for a good example.

   Setting delimiters
      There  are  two  kinds  of delimiters:  single  and paired.  Single  delimiters
      separate the lexemes from each  other  like spaces separate the  words.  Paired
      delimiters separate  the  characters  contained  between  the  opening and  the
      closing  delimiter from  other lexemes.  Comments and quoted string literals in
      programming languages are the examples  of paired  delimiters.  There are three
      kinds of paired delimiters:  nesting, non-nesting and double escaped. A nesting
      pair can contain another pair of the same kind inside,  while a non-nesting one
      can't. Double escaped delimiters are used when you need to embed a delimiter as
      a part of the lexeme,  for example to use a quote  inside  of a quote-delimited
      string. This is done by doubling that quote, just like with Pascal strings.

      Call InitDelimiters(Delimiters), then add the delimiters using these 
      procedures:
      
         AddSingleDelimiter(
            Delimiters,
            Lexeme,        // The delimiter itself
            Skip);         // Indicates if the delimiter should be skipped insead of
                           // being passed to the next stage of classification
         AddPairedDelimiter(
            Delimiters,
            OpenBracket,  SkipOpen,    // The opening delimiter and its skip flag
            CloseBracket, SkipClose,   // The closing delimiter and its skip flag
            PairType,                  // Pair type, pairNesting / pairNonNesting
            SkipInside);               // Skip the lexeme inside of the pair

      Whitespace characters (space, tab, line break) with Skip = True can be added by
      calling AddWhitespace(Delimiters);

   Setting lexeme classification functions
      The purpose of  these  functions is to  classify a lexeme  (just a sequence  of
      characters) into a specific  token  type.  Declare a TLexerProcs  variable  and
      assign your functions to its fields:

         GetNextTokenAddress(
            ptrTokens   // Pointer to your tokens
            );          // Return the address of the next token

         // Classify a single-delimited lexeme into a token
         ProcSingleDelim(
            ptrToken,   // Pointer to the token
            Lexeme,     // Lexeme to classify
            Position    // Position of the lexeme (contains line and column fields)
            );          // Return an error message or empty string if succesfull

         // Classify a pair-delimited lexeme into a token
         ProcPairDelim(
            ptrToken,   // Pointer to the token
            Lexeme,     // Lexeme to classify
            Position,   // Position of the lexeme
            DelimOpen,  // Opening delimiter
            DelimClose  // Closing delimiter
            );          // Return an error message or empty string if succesfull

>> Performance
   Speed is increased up to 1.5 times when using FastMM and FastMove units.

>> Design decisions
   Another  approach  to making  a universal  lexer being able to work with any token
   type  is  to have  this  type  and  associated  procedures  in a separate  problem
   definition unit  supplied  by  the  user.  This solution has  several  advantages:
   everything is perfectly  typed, no pointers are passed around, simple. However, it
   has one  critical  flaw:  one lexer  unit  cannot  be used to  tokenize  different
   languages within one program.

>> Acknowledgements
   Thanks  to  Anton Shepelev  for convincing  me to write this unit  and for helpful
   discussion.

>> ToDo 
   - Thorough testing
   - Correctly preprocess all styles of line breaks

>> Changelog 
   1.2   : 2018.09.19 ~ FreePascal compatibility
   1.1.1 : 2011.10.14 ~ Moved to generic types
   1.1   : 2011.03.18 ~ StringUtils 2.0 compatibility
   1.0   : 2011.03.06 + Stable fully featured version
   0.0   : 2011.02.07 + Work on the unit started
   Notation: + added, - removed, * fixed, ~ changed   
}
interface ///////////////////////////////////////////////////////////////////////////
uses
      StringUtils,
      StringLists;

type
      TDelimiterType = (delimSingle, delimOpen, delimClose, delimCloseOpen);

      TPairStyle = (pairNone, pairNesting, pairNonNesting, pairDoubleEscaped);

      TDelimiter =
         record
         DelimType   :  TDelimiterType;
         Lexeme      :  AnsiString;
         LexemePair  :  AnsiString;
         Skip,
         SkipInside  :  Boolean;
         PairStyle   :  TPairStyle;
         end;

      TDelimiters = array of TDelimiter;

      TLexemePosition =
         record
         Line, Column, Index  :  Integer;
         end;

      ProcGetNextTokenAddress =
         function(
                  ptrTokens   :  Pointer
                  )           :  Pointer;

      ProcSingleDelim =
         function(
                  ptrToken       :  Pointer;
            const Lexeme         :  AnsiString;
            const Position       :  TLexemePosition
                  )              :  AnsiString;

      ProcPairDelim =
         function(
                  ptrToken       :  Pointer;
            const Lexeme         :  AnsiString;
            const Position       :  TLexemePosition;
            const DelimOpen,
                  DelimClose     :  AnsiString
                  )              :  AnsiString;

      TLexerProcs =
         record
         GetNextTokenAddress  :  ProcGetNextTokenAddress;
         ProcessSingleDelim   :  ProcSingleDelim;
         ProcessPairDelim     :  ProcPairDelim;
         end;

{-----------------------<< Delimiter lists >>---------------------------------------}

// Initialize the Delimiters
procedure InitDelimiters(
   var   Delimiters  :  TDelimiters);

// Add whitespace characters to the Delimiters
procedure AddWhitespace(
   var   Delimiters  :  TDelimiters);

// Add a single delimiter with specified parameters to Delimiters
procedure AddSingleDelimiter(
   var   Delimiters  :  TDelimiters;
   const Lexeme      :  AnsiString;
         Skip        :  Boolean);

// Add a paired delimiter with specified parameters to Delimiters
procedure AddPairedDelimiter(
   var   Delimiters  :  TDelimiters;
   const  OpenLexeme :  AnsiString;
         SkipOpen    :  Boolean;
   const CloseLexeme :  AnsiString;
         SkipClose   :  Boolean;
         Style       :  TPairStyle;
         SkipInside  :  Boolean);

{-----------------------<< Lexeme positions >>--------------------------------------}

// Convert a lexeme Position to string. UpperCase determines the case 
// of the first character.
function PositionToString(
   const Position    :  TLexemePosition;
         UpperCase   :  Boolean
         )           :  AnsiString;

{-----------------------<< Lexer >>-------------------------------------------------}

// Parse the Input into tokens at ptrTokens according to provided Delimiters 
// and LexerProcs, return the Errors
procedure Tokenize(
         ptrTokens      :  Pointer;
   const Input          :  AnsiString;
   const Delimiters     :  TDelimiters;
         LexerProcs     :  TLexerProcs;
   var   Errors         :  TStringList);

implementation //////////////////////////////////////////////////////////////////////
uses
      invSys;

const
      LineBreak = #10;

{-----------------------<< Delimiters >>--------------------------------------------}

// Create a Delimiter from specified parameters
procedure FormDelimiter(
   var   Delimiter   :  TDelimiter;
   const Lexeme,
         LexemePair  :  AnsiString;
         DelimType   :  TDelimiterType;
         PairStyle   :  TPairStyle;
         Skip,
         SkipInside  :  Boolean);
   begin
   Delimiter.Lexeme     := Lexeme;
   Delimiter.LexemePair := LexemePair;
   Delimiter.DelimType  := DelimType;
   Delimiter.PairStyle  := PairStyle;
   Delimiter.Skip       := Skip;
   Delimiter.SkipInside := SkipInside;
   end;
   

// Transform an opening delimiter to its closing counterpart and vice versa
procedure TogglePair(
   var   Delim :  TDelimiter);
   var
         TempS :  AnsiString;
   begin
   with Delim do
      begin
      Assert(DelimType in [delimOpen, delimClose], 'Paired delimiter expected');
      if DelimType  = DelimOpen then
         DelimType := DelimClose
      else
         DelimType := DelimOpen;
      TempS      := Lexeme;
      Lexeme     := LexemePair;
      LexemePair := TempS;
      end;
   end;

{-----------------------<< Delimiter lists >>---------------------------------------}

// Initialize the Delimiters
procedure InitDelimiters(
   var   Delimiters  :  TDelimiters);
   begin
   SetLength(Delimiters, 0);
   end;


// DelimsFrom to DelimsTo
procedure AssignDelimiters(
   var   DelimsTo    :  TDelimiters;
   const DelimsFrom  :  TDelimiters);
   begin
   DelimsTo := Copy(DelimsFrom);
   end;


// Add a Delimiter to the list of Delimiters
procedure AddDelimiter(
   var   Delimiters  :  TDelimiters;
   const Delimiter   :  TDelimiter);
   var
         N           :  Integer;
   begin
   N := Length(Delimiters);
   SetLength(Delimiters, N + 1);
   Delimiters[N] := Delimiter;
   end;


// Add a single delimiter with specified parameters to Delimiters
procedure AddSingleDelimiter(
   var   Delimiters  :  TDelimiters;
   const Lexeme      :  AnsiString;
         Skip        :  Boolean);
   var
         Delimiter   :  TDelimiter;
   begin
   FormDelimiter(
      Delimiter, Lexeme, {LexemePair:} '',
      delimSingle, pairNone, Skip, {SkipInside:} False);
   AddDelimiter(Delimiters, Delimiter);
   end;


// Add a paired delimiter with specified parameters to Delimiters
procedure AddPairedDelimiter(
   var   Delimiters  :  TDelimiters;
   const OpenLexeme  :  AnsiString;
         SkipOpen    :  Boolean;
   const CloseLexeme :  AnsiString;
         SkipClose   :  Boolean;
         Style       :  TPairStyle;
         SkipInside  :  Boolean);
   var
         Delimiter   :  TDelimiter;
   begin
   // Add an opening delimiter
   FormDelimiter(
      Delimiter, {Lexeme:} OpenLexeme,  {LexemePair:} CloseLexeme,
      delimOpen, Style, SkipOpen,  SkipInside);
   AddDelimiter(Delimiters, Delimiter);

   // Add a closing delimiter
   TogglePair(Delimiter);
   AddDelimiter(Delimiters, Delimiter);
   end;


// Add whitespace characters to the Delimiters
procedure AddWhitespace(
   var   Delimiters  :  TDelimiters);
   begin
   AddSingleDelimiter(Delimiters, LineBreak, {Skip:} True);
   AddSingleDelimiter(Delimiters, ' ',       {Skip:} True);
   AddSingleDelimiter(Delimiters, Tab,       {Skip:} True);
   end;


// Make a list of delimiters that may occur inside of a delimiter pair
procedure MakePairedDelimList(
   var   Delimiters  :  TDelimiters;
   const Delimiter   :  TDelimiter);
   var
         Closing,
         CloseOpen   :  TDelimiter;
   begin
   Assert(Delimiter.DelimType = delimOpen, 'Need an opening delimiter');
   InitDelimiters(Delimiters);

   // Close-open for doubling pairs
   if Delimiter.PairStyle = pairDoubleEscaped then
      begin
      CloseOpen := Delimiter;
      with CloseOpen do
         begin
         DelimType := delimCloseOpen;
         Lexeme := LexemePair + Lexeme;
         end;
      AddDelimiter(Delimiters, CloseOpen);
      end;

   // Closing delimiter
   Closing := Delimiter;
   TogglePair(Closing);
   AddDelimiter(Delimiters, Closing);

   // Opening delimiter for nesting pairs
   if Delimiter.PairStyle = pairNesting then
      AddDelimiter(Delimiters, Delimiter);
   end;

{-----------------------<< Token addition >>----------------------------------------}

// Convert a single delimited Lexeme into a token using LexerProcs and add it to the
// tokens at ptrTokens, return an error message or an empty string
function AddSingleDelimToken(
         ptrTokens   :  Pointer;
   const Lexeme      :  AnsiString;
   const Position    :  TLexemePosition;
         LexerProcs  :  TLexerProcs
         )           :  AnsiString;
   var
         ptrToken    :  Pointer;
   begin
   ptrToken := LexerProcs.GetNextTokenAddress(ptrTokens);
   Result   := LexerProcs.ProcessSingleDelim(ptrToken, Lexeme, Position);
   end;


// Convert a pair delimited Lexeme into a token using LexerProcs and add it to the
// tokens at ptrTokens, return an error message or an empty string
function AddPairDelimToken(
         ptrTokens            :  Pointer;
   const Lexeme               :  AnsiString;
   const Position             :  TLexemePosition;
   const DelimOpen,
         DelimClose           :  AnsiString;
         LexerProcs           :  TLexerProcs
         )                    :  AnsiString;
   var
         ptrToken             :  Pointer;
   begin
   ptrToken := LexerProcs.GetNextTokenAddress(ptrTokens);
   Result   := LexerProcs.ProcessPairDelim(
      ptrToken, Lexeme, Position, DelimOpen, DelimClose);
   end;

{-----------------------<< Lexeme positions >>--------------------------------------}

// Initialize the lexeme Position
procedure InitLexemePosition(
   var   Position :  TLexemePosition);
   begin
   with Position do
      begin
      Index  := 1;
      Line   := 1;
      Column := 1;
      end;
   end;


// Advance the lexeme Position by Step characters
procedure AdvanceLexemePosition(
   var   Position :  TLexemePosition;
         Step     :  Integer);
   begin
   with Position do
      begin
      Inc(Index,  Step);
      Inc(Column, Step);
      end;
   end;


// Convert a lexeme Position to string. UpperCase determines the case 
// of the first character.
function PositionToString(
   const Position    :  TLexemePosition;
         UpperCase   :  Boolean
         )           :  AnsiString;
   const
         LineUpper   =  'Line ';
         LineLower   =  'line ';
         LineWidth   =  3;
   begin
   Str(Position.Line : LineWidth, Result);
   if UpperCase then
      Result := LineUpper + Result else
      Result := LineLower + Result;
   end;

{-----------------------<< Lexer >>-------------------------------------------------}

// Convert line breaks to #10, add a line break at the end
procedure Preprocess(
   var   Processed   :  AnsiString;
   const Input       :  AnsiString);
   begin
   Processed := Input + LineBreak;
   FilterChars(Processed, [#13], {Reject:} True);
   end;


// #HOSTSPOT
// Try to find one of the Delimiters in the Input starting from Pos
// (modified during the search), return Fail if nothing was found
function FindDelimiter(
   var   FoundDelim  :  TDelimiter;
   var   Pos         :  TLexemePosition;
   const Input       :  AnsiString;
   const Delimiters  :  TDelimiters
         )           :  Boolean;
   var
         iDelim      :  Integer;
   begin
   Result := Fail;
   while Pos.Index <= Length(Input) do
      begin
      // Search for delimiters
      for iDelim := 0 to Length(Delimiters) - 1 do
         if SubstrAtPos(Input, Delimiters[iDelim].Lexeme, Pos.Index) then
            begin
            FoundDelim := Delimiters[iDelim];
            Result := Success;
            break;
            end;

      // Adjust the position when line break is encountered
      if Input[Pos.Index] = LineBreak then
         begin
         Inc(Pos.Line);
         Pos.Column := 0;
         end;

      // Exit the loop if found something or continue the search 
      // from the next character
      if Result = Success then
   {<}   break
      else
         AdvanceLexemePosition(Pos, {Step:} 1);
      end;
   end;


// Parse the Input into tokens at ptrTokens according to provided Delimiters 
// and LexerProcs, return the Errors
procedure Tokenize(
         ptrTokens      :  Pointer;
   const Input          :  AnsiString;
   const Delimiters     :  TDelimiters;
         LexerProcs     :  TLexerProcs;
   var   Errors         :  TStringList);
   var
         NowPos, OldPos,
         PairStart,
         LexemeStart    :  TLexemePosition;
         Processed      :  AnsiString;
         Lexeme,
         OpenLexeme     :  AnsiString;
         FoundDelim     :  TDelimiter;
         SeekDelims     :  TDelimiters;
         Nestedness     :  Integer;
         PairJustOpened,
         PairJustClosed,
         InsideOfPair,
         DelimitedSmth  :  Boolean;
         NowError,
         StrNowPos      :  AnsiString;
   const
         ErrorUnmatchedDelimiter = 'Unmatched delimiter';
         ErrorDelimiter          = ': ';
   begin
   Preprocess(Processed, Input);
   Lexeme     := '';
   Nestedness := 0;
   InitStringList(Errors);
   InitLexemePosition(PairStart);
   InitLexemePosition(NowPos);
   InitLexemePosition(OldPos);
   AssignDelimiters(SeekDelims, Delimiters);
   
   while FindDelimiter(FoundDelim, NowPos, Processed, SeekDelims) do
      begin
      // Add the delimiter itself
      if not FoundDelim.Skip then
         begin
         StrNowPos := PositionToString(NowPos, {UpperCase:} True) + ErrorDelimiter;
         NowError := AddSingleDelimToken(
            ptrTokens, FoundDelim.Lexeme, NowPos, LexerProcs);
         if NowError <> '' then
            AddToStringList(Errors, StrNowPos + NowError);
         end;

      // Update the nestedness in paired mode
      if Nestedness <> 0 then
         begin
         if      FoundDelim.DelimType = delimOpen  then
            Inc(Nestedness)
         else if FoundDelim.DelimType = delimClose then
            Dec(Nestedness);
         if Nestedness = 0 then
            AssignDelimiters(SeekDelims, Delimiters);
         end
      else if FoundDelim.DelimType = delimClose then
         AddToStringList(Errors, StrNowPos + ErrorUnmatchedDelimiter)
      else if FoundDelim.DelimType = delimOpen  then
         begin
         Nestedness := 1;
         MakePairedDelimList(SeekDelims, FoundDelim);
         end;

      // Add the delimited lexeme
      PairJustOpened := (Nestedness = 1) and (FoundDelim.DelimType = delimOpen);
      PairJustClosed := (Nestedness = 0) and (FoundDelim.DelimType = delimClose);
      InsideOfPair   := (Nestedness > 0) and not PairJustOpened;
      DelimitedSmth := OldPos.Index <> NowPos.Index;
      if PairJustClosed or (DelimitedSmth and not InsideOfPair) then
         begin
         // Extract the lexeme
         if PairJustClosed then
            LexemeStart := PairStart
         else
            LexemeStart := OldPos;
         Lexeme := CopyRange(
            Processed, {From:} LexemeStart.Index, {To:} NowPos.Index - 1);
         if FoundDelim.PairStyle = pairDoubleEscaped then
            Lexeme := Replace(
                              Lexeme,
               {OldSubstr:}   FoundDelim.Lexeme + FoundDelim.LexemePair,
               {NewSubstr:}   FoundDelim.LexemePair);
         StrNowPos :=
            PositionToString(LexemeStart, {UpperCase:} True) + ErrorDelimiter;

         // Add a single-delimited lexeme
         if not PairJustClosed then
            begin
            NowError := AddSingleDelimToken(
               ptrTokens, Lexeme, LexemeStart, LexerProcs);
            if NowError <> '' then
               AddToStringList(Errors, StrNowPos + NowError);
            end

         // Add a pair-delimited lexeme
         else if not FoundDelim.SkipInside then
            begin
            NowError := AddPairDelimToken(
               ptrTokens, Lexeme, LexemeStart,
               OpenLexeme, FoundDelim.Lexeme, LexerProcs);
            if NowError <> '' then
               AddToStringList(Errors, StrNowPos + NowError);
            end;
         end;

      // Update the position
      AdvanceLexemePosition( NowPos, {Step:} Length(FoundDelim.Lexeme) );
      OldPos := NowPos;

      // Save the pair opening delimiter and the position of lexeme it delimits
      if PairJustOpened then
         begin
         OpenLexeme := FoundDelim.Lexeme;
         PairStart := NowPos;
         end;
      end;
      
   if Nestedness <> 0 then
      begin
      StrNowPos := PositionToString(PairStart, {UpperCase:} True) + ErrorDelimiter;
      AddToStringList(Errors, StrNowPos + ErrorUnmatchedDelimiter);
      end;
   end;

end.
