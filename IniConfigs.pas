{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit IniConfigs; ////////////////////////////////////////////////////////////////////
{
>> Version: 1.3

>> Description
   Unit for working with INI-style config files. Part of InvLibs unit collection.
   
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> Usage
   There are two ways to get values from configs.
   1.  Load a config  from file with LoadConfig,  then use getters  to get individual
   properties. You'll have to do all error checking yourself.
   2.  Make a load table by initializing a TLoadTable variable with InitLoadTable and
   add all your variables  to it using AddLoadParam.  You can specify specific error-
   checking  functions  for  each  variable  or pass a nil if no checking  is needed.
   Finally call LoadFromConfig  and it will try to load all your variables and return
   a list of errors.

>> Performance
   Hash tables  are used for searching  sections  and properties,  so performance  on
   large files is adequate.

>> Design decisions
   While  it is possible  to support  loading  into  variables  of other types (byte,
   smallint, single, ...), it leads to horrible amount of copypasta.

>> ToDo
   - Describe config format in details
   - Parameters with SI prefixes and dimensionality
   - Support for fractions and thousand separators
   - Saving TIniConfig to a file (requires saving all the comments intact)

>> Changelog
   1.3   : 2018.09.09   ~ FreePascal compatibility
                        ~ Arrays 1.2 compatibility
                        + Missing routine comments
   1.2.3 : 2011.10.14   ~ Moved to generic types
   1.2   : 2011.04.02   * Case-sensitivity when reading booleans
   1.1   : 2011.03.18   ~ StringUtils 2.0 compatibility
   1.0   : 2011.03.06   + Stable fully featured version
                        + Config loading, access to individual properties
                        + Automated loading of properties into user variables
                        ~ Uses universal lexer
                        ~ Hash tables for section and property search
   0.0   : 2011.02.01   + Work on the unit started
   Notation: + added, - removed, * fixed, ~ changed   
}
interface ///////////////////////////////////////////////////////////////////////////
uses
      StringUtils,   
      StringLists,
      HashTables,    
      HashFunctions,
      Lexer;

type
      TTokenPosition = TLexemePosition;

      TPropertyType = (ptInt, ptReal, ptString, ptBool);

      TPropertyValue =
         record
         PropertyType   :  TPropertyType;
            IntValue    :  Integer;
           RealValue    :  Real;
           BoolValue    :  Boolean;
         StringValue    :  AnsiString;
         end;

      TProperty =
         record
         Name     :  AnsiString;
         Value    :  TPropertyValue;
         Position :  TTokenPosition;
         end;

      TProperties = array of TProperty;
      PProperties = ^TProperties;

      TSection =
         record
         Name           :  AnsiString;
            NProperties :  Integer;
             Properties :  TProperties;
         HashProperties :  THashTable;
         end;

      TSections = array of TSection;
      PSections = ^TSections;

      TIniConfig =
         record
            NSections   :  Integer;
             Sections   :  TSections;
         HashSections   :  THashTable;
         end;

      TPascalType = (
         pasInteger, pasReal, pasBoolean, pasString, pasOther);

      TLoadParameter =
         record
         ptrVar   :  Pointer;
         VarType  :  TPascalType;
         CfgPath  :  AnsiString;
         ptrCheck :  Pointer;
         end;

      TLoadTable = array of TLoadParameter;

      ProcCheckInt = function(
               VarInt   :  Integer
               )        :  AnsiString;

      ProcCheckReal = function(
               VarReal  :  Real
               )        :  AnsiString;

      ProcCheckString = function(
               VarString   :  AnsiString
               )           :  AnsiString;

      ProcOtherVar = function(
               ptrVar   :  Pointer;
         const S        :  AnsiString
               )        :  AnsiString;

{-----------------------<< Parse, load >>-------------------------------------------}

// Load the Config file located at Path, return the Errors
procedure LoadConfig(
   var   Config      :  TIniConfig;
   const Path        :  AnsiString;
   var   Errors      :  TStringList);

{-----------------------<< Getters >>-----------------------------------------------}

// Get the value of a property with FullName stored in Config or
// return Fail if it does not exist
function GetIntProperty(
   var   IntValue       :  Integer;
   const Config         :  TIniConfig;
         FullName       :  AnsiString
         )              :  Boolean;
         
function GetRealProperty(
   var   RealValue      :  Real;
   const Config         :  TIniConfig;
         FullName       :  AnsiString
         )              :  Boolean;

function GetBoolProperty(
   var   BoolValue      :  Boolean;
   const Config         :  TIniConfig;
         FullName       :  AnsiString
         )              :  Boolean;

function GetStringProperty(
   var   StringValue    :  AnsiString;
   const Config         :  TIniConfig;
         FullName       :  AnsiString
         )              :  Boolean;

{-----------------------<< Load tables >>-------------------------------------------}

// Initialize LoadTable
procedure InitLoadTable(
   var   LoadTable   :  TLoadTable);

// Load the variables specified in LoadTable from Config, return the Errors
procedure LoadFromConfig(
   const LoadTable   :  TLoadTable;
   const Config      :  TIniConfig;
   var   Errors      :  TStringList);
         overload; 

// Load the variables specified in LoadTable from a config file located at 
// PathConfig, return the Errors
procedure LoadFromConfig(
   const LoadTable   :  TLoadTable;
   const PathConfig  :  AnsiString;
   var   Errors      :  TStringList);
         overload;

{-----------------------<< Adding variables to a load table >>----------------------}

// Add a Variable with config file path CfgPath to the LoadTable. ProcCheck
// will be used during the loading to check the validity of the Variable. 
procedure AddLoadParam(
   var   LoadTable   :  TLoadTable;
   var   Variable    :  Integer;
   const CfgPath     :  AnsiString;
         ProcCheck   :  ProcCheckInt);
         overload;

procedure AddLoadParam(
   var   LoadTable   :  TLoadTable;
   var   Variable    :  Real;
   const CfgPath     :  AnsiString;
         ProcCheck   :  ProcCheckReal);
         overload;

procedure AddLoadParam(
   var   LoadTable   :  TLoadTable;
   var   Variable    :  Boolean;
   const CfgPath     :  AnsiString);
         overload;

procedure AddLoadParam(
   var   LoadTable   :  TLoadTable;
   var   Variable    :  AnsiString;
   const CfgPath     :  AnsiString;
         ProcCheck   :  ProcCheckString);
         overload;

// Add a variable pointed at by ptrVar with config file path CfgPath to the
// LoadTable. Process will be used during the loading to process the variable and 
// check its validity.
procedure AddLoadParam(
   var   LoadTable   :  TLoadTable;
         ptrVar      :  Pointer;
   const CfgPath     :  AnsiString;
         Process     :  ProcOtherVar);
         overload;      

{-----------------------<< Tests >>-------------------------------------------------}

// Create a random config file consisting of Size lines and located at Path
procedure MakeRandomConfig(
   const Path  :  AnsiString;
         Size  :  Integer);

implementation //////////////////////////////////////////////////////////////////////
uses
      invSys,
      Arrays,
      RandVars,
      Math;

type
      TTokenType = (
         tokEnd,  tokDelimiter,  tokSection, tokIdentifier,
         tokInt,  tokReal,       tokString,  tokBool);

      TTokenValue =
         record
            IntValue   :  Integer;
           RealValue   :  Real;
           BoolValue   :  Boolean;
         StringValue   :  AnsiString;
         end;

      TToken =
         record
         TokenType   :  TTokenType;
         Value       :  TTokenValue;
         Position    :  TTokenPosition;
         end;

      PToken = ^TToken;

      TTokenStream =
         record
         N, iNowToken   :  Integer;
         _              :  array of TToken;
         end;

      PTokenStream = ^TTokenStream;

      TSectionIndex  = Integer;
      TPropertyIndex = Integer;

const
      InvalidSection    = -1;
      InvalidProperty   = -1;
      TokensValue       = [tokInt, tokReal, tokBool, tokString];
      Delimiter     = '=';
      Quote         = '"';
      Comment       = ';';
      OpenBracket   = '[';
      CloseBracket  = ']';
      DecimalDot    = '.';
      SectionDelimiter  = '.';
      ConstYes          = 'yes';
      ConstNo           = 'no';
      SymbolsFirstNumber      = ['0' .. '9', '.', '-'];
      SymbolsNumber           = ['0' .. '9', '.', '-', '+', 'e', 'E'];
      SymbolsIdentifier       = ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '_'];
      Whitespace              = [#0 .. ' '];
      WhitespaceExceptBreaks  = [#0 .. #9, #11, #12, #14 .. ' '];
      ControlChars            = [#0 .. #31];
      LineBreaks              = [#10, #13];
      LineBreak               = #10;

{-----------------------<< Lexer functions >>---------------------------------------}

// Return the address of the next token, allocate extra space if necessary
function GetNextTokenAddress(
         ptrTokens   :  Pointer
         )           :  Pointer;
   var
         Tokens      :  PTokenStream;
   begin
   Tokens := PTokenStream(ptrTokens);
   Inc(Tokens.N);
   if Tokens.N > Length(Tokens._) then
      SetLength(Tokens._, 2 * Tokens.N);
   Result := @Tokens._[Tokens.N - 1];
   end;


// Classify a single-delimited Lexeme into a token
function ProcessSingleDelim(
         ptrToken       :  Pointer;
   const Lexeme         :  AnsiString;
   const Position       :  TLexemePosition
         )              :  AnsiString;
   var
         Token          :  PToken;
         ErrorCodeInt,
         ErrorCodeReal  :  Integer;
   const
         ErrorUnknownLexeme   = 'unknown lexeme';
         ErrorBadConstant     = 'malformed constant';
   begin
   Result := '';
   Token := PToken(ptrToken);
   Token.Position := Position;

   // Delimiter
   if Lexeme[1] = Delimiter then
      Token.TokenType := tokDelimiter

   // Number
   else if Lexeme[1] in SymbolsFirstNumber then
      begin
      Val(Lexeme, Token.Value. IntValue, ErrorCodeInt);
      Val(Lexeme, Token.Value.RealValue, ErrorCodeReal);
      if ErrorCodeReal = 0 then
         begin
         if ErrorCodeInt = 0 then
            Token.TokenType := tokInt
         else
            Token.TokenType := tokReal;
         end
      else
         Result := ErrorBadConstant;
      end

   // Identifier
   else if Lexeme[1] in SymbolsIdentifier then
      // Boolean constant
      if SameText(Lexeme, ConstYes) or SameText(Lexeme, ConstNo) then
         begin
         Token.TokenType := tokBool;
         Token.Value.BoolValue := SameText(Lexeme, ConstYes);
         end
      else
         // Property
         begin
         Token.TokenType := tokIdentifier;
         Token.Value.StringValue := Lexeme;
         end
   else
      Result := ErrorUnknownLexeme;
   end;


// Classify a pair-delimited lexeme into a token
function ProcessPairDelim(
         ptrToken       :  Pointer;
   const Lexeme         :  AnsiString;
   const Position       :  TLexemePosition;
   const DelimOpen,
         DelimClose     :  AnsiString
         )              :  AnsiString;
   var
         Token          :  PToken;
         TempS          :  AnsiString;
   const
         ErrorUnknownLexeme   = 'unknown lexeme';
         ErrorBadSectionName  = 'bad section name';
   begin
   Result := '';
   Token := PToken(ptrToken);
   Token.Position := Position;

   // String
   if (DelimOpen = Quote) and (DelimClose = Quote) then
      begin
      Token.TokenType := tokString;
      Token.Value.StringValue := Lexeme;
      end

   // Section
   else if (DelimOpen  =  OpenBracket) and
           (DelimClose = CloseBracket) then
      begin
      Token.TokenType := tokSection;
      TempS := Lexeme;
      TrimWhitespaceSides(TempS);
      if ContainsWhitespace(TempS) then
         Result := ErrorBadSectionName
      else
         Token.Value.StringValue := TempS;
      end

   else
      Result := ErrorUnknownLexeme;
   end;


// Set the config language Delimiters for use by the lexer
procedure SetDelimiters(
   var   Delimiters  :  TDelimiters);
   const
         LineBreak   =  #10;
   begin
   InitDelimiters(Delimiters);
   AddWhitespace(Delimiters);
   AddSingleDelimiter(Delimiters, '=', {Skip:} False);
   AddPairedDelimiter(
      Delimiters,
      Quote,               {SkipOpen  :} True,
      Quote,               {SkipClose :} True,
      pairDoubleEscaped,   {SkipInside:} False);
   AddPairedDelimiter(
      Delimiters,
      ';',                 {SkipOpen  :} True,
      LineBreak,           {SkipClose :} True,
      pairNonNesting,      {SkipInside:} True);
   AddPairedDelimiter(
      Delimiters,
      OpenBracket,         {SkipOpen  :} True,
      CloseBracket,        {SkipClose :} True,
      pairNonNesting,      {SkipInside:} False);
   end;


// Set the procedures LexerProcs required by the lexer
procedure SetLexerProcs(
   var   LexerProcs  :  TLexerProcs);
   begin
   LexerProcs.GetNextTokenAddress := GetNextTokenAddress;
   LexerProcs.ProcessSingleDelim  := ProcessSingleDelim;
   LexerProcs.ProcessPairDelim    := ProcessPairDelim;
   end;

{-----------------------<< Tokens >>------------------------------------------------}

// Initialize the tokens
procedure InitTokens(
   var   Tokens  :  TTokenStream);
   begin
   Tokens.N := 0;
   Tokens.iNowToken := 0;
   SetLength(Tokens._, 0);
   end;


// Get the next Token from Tokens and return Success,  
// or return Fail if the last token has been reached
function GetNextToken(
   var   Token    :  TToken;
   var   Tokens   :  TTokenStream
         )        :  Boolean;
   begin
   with Tokens do
      if iNowToken < N then
         begin
         Token := Tokens._[iNowToken];
         Inc(iNowToken);
         Result := Success;
         end
      else
         Result := Fail;
   end;

{-----------------------<< Sections >>----------------------------------------------}

// Return whether the section with a given Index at ptrSections^ is 
// the same as ptrName^
function EqualSections(
         ptrSections :  Pointer;
         Index       :  TObjectIndex;
         ptrName     :  Pointer
         )           :  Boolean;
   begin
   Result := PSections(ptrSections)^[Index].Name = PAnsiString(ptrName)^;
   end;


// Return the index of a section with given SectionName in the Config or
// InvalidSection if it does not exist
function FindSection(
   const Config      :  TIniConfig;
   const SectionName :  AnsiString
         )           :  TSectionIndex;
   begin
   if LookupHashTable(
      Result, @Config.Sections, Config.HashSections,
      @SectionName, HashBernstein(SectionName), EqualSections) = Fail then
      Result := InvalidSection;
   end;


// Add a section with a given SectionName to the Config, 
// or return Fail if it already exists
function AddSection(
   var   Config      :  TIniConfig;
   const SectionName :  AnsiString
         )           :  Boolean;
   begin
   if FindSection(Config, SectionName) = InvalidSection then with Config do
      begin
      AddToHashTable( HashSections, NSections, HashBernstein(SectionName) );
      Inc(NSections);
      if Length(Sections) < NSections then
         SetLength(Sections, 2 * NSections);
      with Sections[NSections - 1] do
         begin
         Name := SectionName;
         NProperties := 0;
         SetLength(Properties, 0);
         InitHashTable(HashProperties);
         end;
      Result := Success;
      end
   else
      Result := Fail;
   end;

{-----------------------<< Properties >>--------------------------------------------}

// Return whether the property with a given Index at ptrProperties^ is 
// the same as ptrName^
function EqualProperties(
         ptrProperties  :  Pointer;
         Index          :  TObjectIndex;
         ptrName        :  Pointer
         )              :  Boolean;
   begin
   Result := PProperties(ptrProperties)^[Index].Name = PAnsiString(ptrName)^;
   end;


// Return the index of a property with given PropertyName in the Config section
// specified by SectionIndex or InvalidProperty if it does not exist
function FindProperty(
   const Config         :  TIniConfig;
         SectionIndex   :  TSectionIndex;
   const PropertyName   :  AnsiString
         )              :  TPropertyIndex;
   begin
   with Config.Sections[SectionIndex] do
      if LookupHashTable(
         Result, @Properties, HashProperties,
         @PropertyName, HashBernstein(PropertyName), EqualProperties) = Fail then
         Result := InvalidProperty;
   end;
   

// Add a property with given name and value to the Config section
// specified by SectionIndex, or return Fail if it already exists
function AddProperty(
   var   Config         :  TIniConfig;
         SectionIndex   :  TSectionIndex;
   const PropertyName   :  AnsiString;
   const TokenValue     :  TToken
         )              :  Boolean;
   begin
   Assert(TokenValue.TokenType in TokensValue, 'Non-value token');
   Assert(SectionIndex < Config.NSections, 'Invalid section index');
   if FindProperty(Config, SectionIndex, PropertyName) = InvalidProperty then
      with Config.Sections[SectionIndex] do
         begin
         AddToHashTable( HashProperties, NProperties, HashBernstein(PropertyName) );
         Inc(NProperties);
         if NProperties > Length(Properties) then
            SetLength(Properties, 2 * NProperties);
         Properties[NProperties - 1].Name := PropertyName;
         with Properties[NProperties - 1].Value do
            case TokenValue.TokenType of
               tokInt:
                  begin
                  PropertyType := ptInt;
                  IntValue  := TokenValue.Value. IntValue;
                  RealValue := TokenValue.Value.RealValue;
                  end;

               tokReal:
                  begin
                  PropertyType := ptReal;
                  RealValue := TokenValue.Value.RealValue;
                  end;

               tokBool:
                  begin
                  PropertyType := ptBool;
                  BoolValue := TokenValue.Value.BoolValue;
                  end;

               tokString:
                  begin
                  PropertyType := ptString;
                  StringValue := TokenValue.Value.StringValue;
                  end;

               else
                  Assert(False, 'Non-value token');
               end;
         Result := Success;
         end
   else
      Result := Fail;
   end;


// Add a property with given name and value to the Config's last section
function AddPropertyLastSec(
   var   Config         :  TIniConfig;
   const PropertyName   :  AnsiString;
   const TokenValue     :  TToken
         )              :  Boolean;
   begin
   Result := AddProperty(Config, Config.NSections - 1, PropertyName, TokenValue);
   end;

{-----------------------<< Config >>------------------------------------------------}

// Initialize the Config, must be called before use
procedure InitConfig(
   var   Config   :  TIniConfig);
   begin
   with Config do
      begin
      NSections := 0;
      SetLength(Sections, 0);
      InitHashTable(HashSections);
      end;
   AddSection(Config, '');
   end;

{-----------------------<< Addressation >>------------------------------------------}

// Split FullName into SectionName and PropertyName
procedure SplitCfgPath(
   var    SectionName   :  AnsiString;
   var   PropertyName   :  AnsiString;
   const     FullName   :  AnsiString);
   var
         PosDelimiter   :  Integer;
   begin
   PosDelimiter := Pos(SectionDelimiter, FullName);
    SectionName := CopyRange(FullName, {From:} 1, {To:} PosDelimiter - 1);
   PropertyName := CopyRange(
      FullName, {From:} PosDelimiter + 1, {To:} Length(FullName) );
   end;


// Get the property and section indices of a property with FullName or 
// return Fail if it does not exist
function GetPropertyAddress(
   var    SectionIndex  :  TSectionIndex;
   var   PropertyIndex  :  TPropertyIndex;
   const Config         :  TIniConfig;
   const FullName       :  AnsiString
         )              :  Boolean;
   var
          SectionName,
         PropertyName   :  AnsiString;
   begin
   SplitCfgPath(SectionName, PropertyName, FullName);
   SectionIndex := FindSection(Config, SectionName);
   if SectionIndex = InvalidSection then
      Result := Fail
   else
      begin
      PropertyIndex := FindProperty(Config, SectionIndex, PropertyName);
      if PropertyIndex = InvalidProperty then
         Result := Fail else
         Result := Success;
      end;
   end;

{-----------------------<< Getters >>-----------------------------------------------}

// Get the value of a property with FullName stored in Config or
// return Fail if it does not exist
function GetIntProperty(
   var   IntValue       :  Integer;
   const Config         :  TIniConfig;
         FullName       :  AnsiString
         )              :  Boolean;
   var
          SectionIndex  :  TSectionIndex;
         PropertyIndex  :  TPropertyIndex;
   begin
   Result := Fail;
   if GetPropertyAddress(SectionIndex, PropertyIndex, Config, FullName) = Success then
      with Config.Sections[SectionIndex].Properties[PropertyIndex] do
         if Value.PropertyType = ptInt then
            begin
            IntValue := Value.IntValue;
            Result := Success;
            end;
   end;


function GetRealProperty(
   var   RealValue      :  Real;
   const Config         :  TIniConfig;
         FullName       :  AnsiString
         )              :  Boolean;
   var
          SectionIndex  :  TSectionIndex;
         PropertyIndex  :  TPropertyIndex;
   begin
   Result := Fail;
   if GetPropertyAddress(SectionIndex, PropertyIndex, Config, FullName) = Success then
      with Config.Sections[SectionIndex].Properties[PropertyIndex] do
         if (Value.PropertyType = ptInt) or (Value.PropertyType = ptReal) then
            begin
            RealValue := Value.RealValue;
            Result := Success;
            end;
   end;


function GetBoolProperty(
   var   BoolValue      :  Boolean;
   const Config         :  TIniConfig;
         FullName       :  AnsiString
         )              :  Boolean;
   var
          SectionIndex  :  TSectionIndex;
         PropertyIndex  :  TPropertyIndex;
   begin
   Result := Fail;
   if GetPropertyAddress(SectionIndex, PropertyIndex, Config, FullName) = Success then
      with Config.Sections[SectionIndex].Properties[PropertyIndex] do
         if Value.PropertyType = ptBool then
            begin
            BoolValue := Value.BoolValue;
            Result := Success;
            end;
   end;


function GetStringProperty(
   var   StringValue    :  AnsiString;
   const Config         :  TIniConfig;
         FullName       :  AnsiString
         )              :  Boolean;
   var
          SectionIndex  :  TSectionIndex;
         PropertyIndex  :  TPropertyIndex;
   begin
   Result := Fail;
   if GetPropertyAddress(SectionIndex, PropertyIndex, Config, FullName) = Success then
      with Config.Sections[SectionIndex].Properties[PropertyIndex] do
         if Value.PropertyType = ptString then
            begin
            StringValue := Value.StringValue;
            Result := Success;
            end;
   end;

{-----------------------<< Parse, load >>-------------------------------------------}

// Parse the Tokens into a Config, return the Errors
procedure Parse(
   var   Config      :  TIniConfig;
   var   Tokens      :  TTokenStream;
   var   Errors      :  TStringList);
   var
         NowToken,
         NextToken   :  TToken;
         StrPosition :  AnsiString;
   const
         ErrorDelimiterExpected  = 'delimiter expected';
         ErrorValueExpected      = 'value expected';
         ErrorUnexpectedToken    = 'unexpected stuff';
         ErrorSectionRedeclared  = 'section redeclared';
         ErrorPropertyRedeclared = 'property redeclared';
         ErrorDelimiter          = ': ';
   begin
   InitConfig(Config);
   InitStringList(Errors);
   while GetNextToken(NowToken, Tokens) = Success do
      begin
      StrPosition :=
         PositionToString(NowToken.Position, {UpperCase:} True) + ErrorDelimiter;

      case NowToken.TokenType of
         tokSection:
            if AddSection(Config, NowToken.Value.StringValue) = Fail then
               AddToStringList(Errors, StrPosition + ErrorSectionRedeclared);

         tokIdentifier:
            if (GetNextToken(NextToken, Tokens) = Success) and
               (NextToken.TokenType = tokDelimiter) then
               begin
               if (GetNextToken(NextToken, Tokens) = Success) and
                  (NextToken.TokenType in TokensValue) then
                  begin
                  if AddPropertyLastSec(
                     Config, NowToken.Value.StringValue, NextToken) = Fail then
                     AddToStringList(Errors, StrPosition + ErrorPropertyRedeclared);
                  end
               else
                  AddToStringList(Errors, StrPosition + ErrorValueExpected);
               end
            else
               AddToStringList(Errors, StrPosition + ErrorDelimiterExpected);

         else
            AddToStringList(Errors, StrPosition + ErrorUnexpectedToken);
         end;
      end;
   end;


// Load the Config file located at Path, return the Errors
procedure LoadConfig(
   var   Config      :  TIniConfig;
   const Path        :  AnsiString;
   var   Errors      :  TStringList);
   var
         Tokens      :  TTokenStream;
         Input       :  AnsiString;
         Delimiters  :  TDelimiters;
         LexerProcs  :  TLexerProcs;
   const
         ErrorFile   = 'Cannot open file ';   
   begin
   if LoadString(Input, Path, {StripControl:} False) then
      begin
      SetDelimiters(Delimiters);
      SetLexerProcs(LexerProcs);
      InitTokens(Tokens);
      Tokenize(@Tokens, Input, Delimiters, LexerProcs, Errors);
      if Errors.N = 0 then
         Parse(Config, Tokens, Errors);
      end
   else
      AddToStringList(Errors, ErrorFile + Path);
   end;

{-----------------------<< Load tables >>-------------------------------------------}

// Initialize LoadTable
procedure InitLoadTable(
   var   LoadTable   :  TLoadTable);
   begin
   SetLength(LoadTable, 0);
   end;


// Load the variables specified in LoadTable from Config, return the Errors
procedure LoadFromConfig(
   const LoadTable   :  TLoadTable;
   const Config      :  TIniConfig;
   var   Errors      :  TStringList);
         overload;
   var
         iVar        :  Integer;
         VarInt      :  Integer;
         VarReal     :  Real;
         VarBool     :  Boolean;
         VarString   :  AnsiString;
         MsgNotFound,
         MsgOverflow :  AnsiString;
         ptrInteger  :  PInteger;
         ptrReal     :  PReal;
         ptrBoolean  :  PBoolean;
         ptrString   :  PAnsiString;
         ErrorCheck  :  AnsiString;
         NowFail     :  Boolean;
   const
         ErrorDelim        = ': ';
         ErrorNotFound     = 'property not found';
         ErrorOverflow     = 'numeric overflow';
         ErrorNoFunction   = 'no function specified';
   begin
   InitStringList(Errors);
   for iVar := 0 to Length(LoadTable) - 1 do with LoadTable[iVar] do
      begin
      // Get parameter from config
      NowFail := False;
      MsgNotFound := CfgPath + ErrorDelim + ErrorNotFound;
      case VarType of
         pasInteger:
            NowFail :=    GetIntProperty(VarInt,    Config, CfgPath) = Fail;
         pasReal:
            NowFail :=   GetRealProperty(VarReal,   Config, CfgPath) = Fail;
         pasBoolean:
            NowFail :=   GetBoolProperty(VarBool,   Config, CfgPath) = Fail;
         pasString, pasOther:
            NowFail := GetStringProperty(VarString, Config, CfgPath) = Fail;
         else
            Assert(False, 'Unknown variable type');
         end;

      if NowFail then
         AddToStringList(Errors, MsgNotFound)
      else
         // Convert parameter into variable
         begin
         MsgOverflow := CfgPath + ErrorDelim + ErrorOverflow;
         // #COPYPASTA need preprocessor
         case VarType of
            pasInteger:
               begin
               ptrInteger  := PInteger(ptrVar);
               ptrInteger^ := VarInt;
               if ptrCheck <> nil then
                  begin
                  ErrorCheck := ProcCheckInt(ptrCheck)(VarInt);
                  if ErrorCheck <> '' then
                     AddToStringList(Errors, CfgPath + ErrorDelim + ErrorCheck);
                  end;
               end;
            pasReal:
               begin
               ptrReal  := PReal(ptrVar);
               ptrReal^ := VarReal;
               if ptrCheck <> nil then
                  begin
                  ErrorCheck := ProcCheckReal(ptrCheck)(VarReal);
                  if ErrorCheck <> '' then
                     AddToStringList(Errors, CfgPath + ErrorDelim + ErrorCheck);
                  end;
               end;
            pasBoolean:
               begin
               ptrBoolean  := PBoolean(ptrVar);
               ptrBoolean^ := VarBool;
               end;
            pasString:
               begin
               ptrString  := PAnsiString(ptrVar);
               ptrString^ := VarString;
               if ptrCheck <> nil then
                  begin
                  ErrorCheck := ProcCheckString(ptrCheck)(VarString);
                  if ErrorCheck <> '' then
                     AddToStringList(Errors, CfgPath + ErrorDelim + ErrorCheck);
                  end;
               end;
            pasOther:
               if ptrCheck = nil then
                  AddToStringList(Errors, CfgPath + ErrorDelim + ErrorNoFunction)
               else
                  begin
                  ErrorCheck := ProcOtherVar(ptrCheck)(ptrVar, VarString);
                  if ErrorCheck <> '' then
                     AddToStringList(Errors, CfgPath + ErrorDelim + ErrorCheck);
                  end;
            else
               Assert(False, 'Unknown variable type');
            end;
         end;
      end;
   end;


// Load the variables specified in LoadTable from a config file located at 
// PathConfig, return the Errors
procedure LoadFromConfig(
   const LoadTable   :  TLoadTable;
   const PathConfig  :  AnsiString;
   var   Errors      :  TStringList);
         overload;
   var
         Config      :  TIniConfig;
   begin
   LoadConfig(Config, PathConfig, Errors);
   if Errors.N = 0 then
      LoadFromConfig(LoadTable, Config, Errors);
   end;

{-----------------------<< Adding variables to a load table >>----------------------}

// Add a variable of type VarType pointed at by ptrVar with config file path CfgPath
// to the LoadTable. Function at ptrCheck will be used during the loading to check
// the validity of the variable. 
procedure AddToLoadTable(
   var   LoadTable   :  TLoadTable;
         ptrVar      :  Pointer;
         VarType     :  TPascalType;
   const CfgPath     :  AnsiString;
         ptrCheck    :  Pointer);
   var
         N           :  Integer;
   begin
   N := Length(LoadTable);
   SetLength(LoadTable, N + 1);
   LoadTable[N].ptrVar     := ptrVar;
   LoadTable[N].VarType    := VarType;
   LoadTable[N].ptrCheck   := ptrCheck;
   LoadTable[N].CfgPath    := CfgPath;
   end;


// #COPYPASTA needs preprocessor
// Add a Variable with config file path CfgPath to the LoadTable. ProcCheck
// will be used during the loading to check the validity of the Variable. 
procedure AddLoadParam(
   var   LoadTable   :  TLoadTable;
   var   Variable    :  Integer;
   const CfgPath     :  AnsiString;
         ProcCheck   :  ProcCheckInt);
         overload;
   begin
   AddToLoadTable(LoadTable, @Variable, pasInteger, CfgPath, @ProcCheck);
   end;


procedure AddLoadParam(
   var   LoadTable   :  TLoadTable;
   var   Variable    :  Real;
   const CfgPath     :  AnsiString;
         ProcCheck   :  ProcCheckReal);
         overload;
   begin
   AddToLoadTable(LoadTable, @Variable, pasReal, CfgPath, @ProcCheck);
   end;


procedure AddLoadParam(
   var   LoadTable   :  TLoadTable;
   var   Variable    :  Boolean;
   const CfgPath     :  AnsiString);
         overload;
   begin
   AddToLoadTable(LoadTable, @Variable, pasBoolean, CfgPath, nil);
   end;


procedure AddLoadParam(
   var   LoadTable   :  TLoadTable;
   var   Variable    :  AnsiString;
   const CfgPath     :  AnsiString;
         ProcCheck   :  ProcCheckString);
         overload;
   begin
   AddToLoadTable(LoadTable, @Variable, pasString, CfgPath, @ProcCheck);
   end;


// Add a variable pointed at by ptrVar with config file path CfgPath to the
// LoadTable. Process will be used during the loading to process the variable and 
// check its validity.
procedure AddLoadParam(
   var   LoadTable   :  TLoadTable;
         ptrVar      :  Pointer;
   const CfgPath     :  AnsiString;
         Process     :  ProcOtherVar);
         overload;
   begin
   AddToLoadTable(LoadTable, ptrVar, pasOther, CfgPath, @Process);
   end;

{-----------------------<< Tests >>-------------------------------------------------}

// Create a random config file consisting of Size lines and located at Path
procedure MakeRandomConfig(
   const Path  :  AnsiString;
         Size  :  Integer);

      function MakeRandString(
               Len         :  Word
               )           :  AnsiString;
         var
               i, NChars   :  Word;
         const
               Chars = 'qwertyuiopasdfghjklzxcvbnm';
         begin
         SetLength(Result, Len);
         NChars := Length(Chars);
         for i := 1 to Len do
            Result[i] := Chars[1 + Random(NChars)];
         end;

   var
         Line           :  Integer;
         RandIndex      :  Word;
         FileConfig     :  Text;
         RandString     :  AnsiString;
         RandValue      :  Real;
         Distribution   :  TRealArray;
   const
         ProbSection = 1 / 10;
         ProbComment = 1 / 4;
         ProbString  = 1 / 4;
         ProbInt     = 1 / 4;
         ProbReal    = 1 / 4;
         RandLen = 8;
         Sigma = 100;
         iSection = 0;
         iComment = 1;
         iString  = 2;
         iInt     = 3;
         iReal    = 4;
         NTypes   = 5;
   begin
   // Initialize the distribution
   SetLength(Distribution, NTypes);
   Distribution[iSection] := ProbSection;
   Distribution[iComment] := ProbComment;
   Distribution[iString ] := ProbString;
   Distribution[iInt    ] := ProbInt;
   Distribution[iReal   ] := ProbReal;

   // Write random lines
   OpenWrite(FileConfig, Path);
   for Line := 1 to Size do
      begin
      RandIndex := RandDiscreteVar(Distribution);
      RandString := MakeRandString(RandLen);
      case RandIndex of
         iSection:
            WriteLn(FileConfig, OpenBracket + RandString + CloseBracket);
         iComment:
            WriteLn(FileConfig, Comment + RandString);
         iInt, iReal, iString:
            begin
            Write(FileConfig, RandString + ' ' + Delimiter + ' ');
            RandValue := RandG({Mean:} 0, Sigma);
            if RandIndex = iInt then
               WriteLn( FileConfig, Round(RandValue) )
            else if RandIndex = iReal then
               WriteLn( FileConfig, RandValue )
            else
               WriteLn(FileConfig, Quote + RandString + Quote);
            end;
         else
            Assert(False, 'Invalid index');
         end;
      end;
   Close(FileConfig);
   end;

end.
