{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit Formatting; ////////////////////////////////////////////////////////////////////
{
>> Version: 0.4

>> Description
   Original string formatting function. Part of InvLibs unit collection.

>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru
   
>> Usage
   Format function takes a format string and a list of arguments as input. The format
   string consists of  any  characters mixed with the format specifiers contained  in 
   curly braces, which are double-escapable. A specifier consists of multiple fields.  
   If a field is not present, the default value is used.
   1. Alignment
         <        Left
         <=       Left  with truncation
         >        Right (default)
         =>       Right with truncation
         ^        Center
         ^=, =^   Center with truncation
         <>       Sign left-aligned, number right-aligned
   2. Sign
         -        Sign only for negative numbers (default)
         +        Sign for both negative and positive numbers
         space    Fill character instead of sign for positive numbers
   3. Fill character
         nothing  Pad with spaces (default)
         0        Left-pad with zeroes, right-pad with spaces
         other    Custom fill character,  must be different from precision  and  type
                  flags
   4. Width
   5. Precision
         :        Number of signs after decimal point,
         ~        Total number of significant digits
   6. Type
         nothing  Acts as 'f' with ':' precision flag, and as 'e' with '~'
         f        Fixed decimal point
         eX       Scientific format. 'e' is lowercase and always followed by sign.  X
                  determines the number of digits in the exponent (4 by default).
                  Alignment is the same as for the number itself, and '0' is used  as
                  a fill character.
         (X)      Appends SI (metric) prefix after an appropriately scaled number.  X
                  specifies the units of measurement appended after the prefix.
         %        Multiply by 100, add '%' symbol
         tX       Thousands separated by symbol X. Single quotes are used if X is not
                  specified.

>> Design decisions
   SysUtils already contains Format function but it was deemed unsatisfactory because
   of its limited options.  A few existing syntaxes of formatting were considered: C,
   C#, Python. C syntax of printf's format tags is rather poor: symmetric things have
   assymetric  meanings.  '-' means left alignment,  while '+' means always preceding
   result with sign - seriously, what kind of logic is that? C# syntax is better, but
   may be cumbersome  because  of use of placeholders  for digits and integer numbers
   for alignment.  New Python syntax appears to be best of the three, so a few things
   were directly taken from it.  Overall,  proposed syntax was designed to be logical
   and easy to use, and flags were selected to be easily readable and memorable.

>> References
   1. printf - C++ Reference
      http://www.cplusplus.com/reference/clibrary/cstdio/printf/
   2. String Formatting in C#
      http://blog.stevex.net/string-formatting-in-csharp/
   3. PEP 3101 - Advanced String Formatting
      http://www.python.org/dev/peps/pep-3101/
                                                                              
>> ToDo
   - Handle +-infinity correctly
   - Add escape sequences like \t, \n
   - Add format-and-write procedure (FWrite, FmtWrite, ...?)
   - Get rid of StrUtils, Math, SysUtils dependencies

>> Changelog
   0.4   : 2018.09.20   ~ FreePascal compatibility
                        ~ StringUtils 1.13 compatibility
   0.3   : 2014.03.09   * Incorrect placement of thousands separators
   0.2.1 : 2011.10.14   ~ Moved to generic types
   0.2   : 2011.08.07   ~ Split from StringUtils
   0.1   : 2011.05.15   + Boolean type support
                        * Sign formating
   0.0   : 2011.03.16   + Initial version
   Notation: + added, - removed, * fixed, ~ changed   
}
interface ///////////////////////////////////////////////////////////////////////////

// Convert Args to strings according to the format specifiers in the format 
// string S and insert them into the corresponding positions in S
function Format(
   const S        :  AnsiString;
   const Args     :  array of const
         )        :  AnsiString;

implementation //////////////////////////////////////////////////////////////////////
uses
      SysUtils,      // IntToStr, FloatToStrF
      invSys,
      Math,          // Ceil, Sign, Log10
      StringUtils;

{-----------------------<< Formatting >>--------------------------------------------}

// Given number of significant digits, returns corresponding number of digits after
// decimal point.
function SignifToFrac(
         X              :  Extended;
         SignifDigits   :  Integer;
         NonNegative    :  Boolean  // Return 0 instead of negative results
         )              :  Integer;
   var
         DigitsBefore   :  Integer;
         Lg             :  Extended;
   begin
   X := Abs(X);
   // In FPC x64, Extended aliases to Double and MinExtended constant does not exist
   if X < MinDouble then 
      DigitsBefore := 0
   else
      begin
      Lg := Log10(X);
      DigitsBefore := Trunc(Lg);
      if Lg >= 0 then
         Inc(DigitsBefore);
      end;
   Result := SignifDigits - DigitsBefore;
   if NonNegative and (Result < 0) then
      Result := 0;
   end;
   

// Format a number with SI (metric) prefixes. SignifDigits specifies what Precision
// means: number of significant digits (True) or digits after the decimal point
// (False).
function FormatSI(
         X              :  Extended;
   const Units          :  AnsiString;
         Precision      :  Integer;
         SignifDigits   :  Boolean;
         Spacer         :  AnsiChar    = ' '
         )              :  AnsiString;
   type
         TSIPrefix =
            record
            Multiplier  :  Real;
            ShortName   :  AnsiString;
            end;
   const
         MaxPrefix   =    8;
         MinPrefix   =  - 8;
         Prefixes    :  array [MinPrefix .. MaxPrefix] of TSIPrefix =
            ( (Multiplier: 1e-24; ShortName: 'y'),
              (Multiplier: 1e-21; ShortName: 'z'),
              (Multiplier: 1e-18; ShortName: 'a'),
              (Multiplier: 1e-15; ShortName: 'f'),
              (Multiplier: 1e-12; ShortName: 'p'),
              (Multiplier: 1e-09; ShortName: 'n'),
              (Multiplier: 1e-06; ShortName: 'u'),
              (Multiplier: 1e-03; ShortName: 'm'),
              (Multiplier: 1    ; ShortName: ' '),
              (Multiplier: 1e+03; ShortName: 'k'),
              (Multiplier: 1e+06; ShortName: 'M'),
              (Multiplier: 1e+09; ShortName: 'G'),
              (Multiplier: 1e+12; ShortName: 'T'),
              (Multiplier: 1e+15; ShortName: 'P'),
              (Multiplier: 1e+18; ShortName: 'E'),
              (Multiplier: 1e+21; ShortName: 'Z'),
              (Multiplier: 1e+24; ShortName: 'Y') );
   var
         i              :  Integer;
         DigitsAfter    :  Integer;
         AbsX           :  Extended;
   begin
   // Find the prefix
   i := MaxPrefix;
   AbsX := Abs(X);
   while (AbsX <= Prefixes[i].Multiplier) and (i > MinPrefix) do
      Dec(i);

   // Format according to the prefix
   X := X / Prefixes[i].Multiplier;
   if SignifDigits then
      DigitsAfter := SignifToFrac(X, Precision, {NonNegative:} True)
   else
      DigitsAfter := Precision;
   Result :=
      FloatToStrF(X, ffFixed, {Precision:} High(Integer), DigitsAfter) +
      Spacer + Prefixes[i].ShortName + Units;
   end;


// Convert Args to strings according to the format specifiers in the format 
// string S and insert them into the corresponding positions in S
function Format(
   const S        :  AnsiString;
   const Args     :  array of const
         )        :  AnsiString;
   type
         TAlign = (alLeft, alRight, alCenter, alLeftRight);

         TSign = (signNegative, signAllways, signSpace);

         TSpecifier =
            (spNone, spFixed, spExp, spPercent, spSI, spThousands);

         TPrecisionType = (prNone, prSignif, prFrac);

         TFormatOptions =
            record
            Align          :  TAlign;
            Truncate       :  Boolean;
            Sign           :  TSign;
                FillChar,
            LeftFillChar,
            ThousandsChar  :  AnsiChar;
               Width,
            ExpWidth,
            Precision      :  Integer;
            PrecisionType  :  TPrecisionType;
            Specifier      :  TSpecifier;
            Units          :  AnsiString;
            end;

   const
         NotSpecified = -1;

   // Get options from the format specifier
   procedure GetFormatOptions(
      var   Options     :  TFormatOptions;
      const FormatStr   :  AnsiString);
      var
            Index,
            IndexClose  :  Integer;
            Flag        :  AnsiString;
      const
            FlagAlighLeft        = '<';
            FlagAlighRight       = '>';
            FlagAlighTruncLeft   = '<=';
            FlagAlighTruncRight  = '=>';
            FlagAlighLeftRight   = '<>';
            FlagAlighCen         = '^';
            FlagAlighTruncCen1   = '^=';
            FlagAlighTruncCen2   = '=^';
            FlagSignAllways      = '+';
            FlagSignNegative     = '-';
            FlagSignSpace        = ' ';
            FlagZeroPad          = '0';
            FlagFrac             = ':';
            FlagSignif           = '~';
            FlagFixed            = 'f';
            FlagExp              = 'e';
            FlagThousands        = 't';
            FlagPercent          = '%';
             OpenDim             = '(';
            CloseDim             = ')';
            NotFill =
              [FlagFrac, FlagSignif, FlagExp, FlagThousands, FlagPercent,
               OpenDim, CloseDim];
            DefaultThousandsChar = '''';
            MaxPrecision = 20 {significant digits};
            DefaultExpWidth = 4;

      // Get a flag from S at position Index. Flags are checked sequentially. If a
      // match is found, Index is incremented by Length(Found). 
      procedure GetField(
         var   Found    :  AnsiString;
         var   Index    :  Integer;
         const S        :  AnsiString;
         const Flags    :  array of AnsiString);
         var
               i        :  Integer;
         begin
         Found := '';
         for i := Low(Flags) to High(Flags) do
            if SubstrAtPos(S, Flags[i], Index) then
               begin
               Found := Flags[i];
               Index := Index + Length(Found);
               break;
               end;
         end;


      // Get a character from S at position Index, increment Index.
      // Do nothing if Index > Length(S).
      procedure GetChar(
         var   Character   :  AnsiChar;
         var   Index       :  Integer;
         const S           :  AnsiString);
         begin
         if Index <= Length(S) then
            begin
            Character := S[Index];
            Inc(Index);
            end;
         end;


      // Get a number from S at position Index, increment Index past the number.
      // Index is not modified and Default is returned if no number is found.
      procedure GetNumber(
         var   Size     :  Integer;
         var   Index    :  Integer;
         const S        :  AnsiString;
               Default  :  Integer     =  NotSpecified);
         var
               TempS    :  AnsiString;
               Len      :  Integer;
               Error    :  Integer;
         begin
         TempS := '';
         Len := Length(S);
         while (Index <= Len) and (S[Index] in Numerals) do
            begin
            TempS := TempS + S[Index];
            Inc(Index);
            end;
         Val(TempS, Size, Error);
         if Error <> 0 then
            Size := Default;
         end;

      begin
      with Options do
         begin
         // Alignment, truncation
         Index := 1;
         Align := alRight;
         Truncate := False;
         GetField(
            Flag, Index, FormatStr,
           [FlagAlighTruncLeft,
            FlagAlighTruncRight,
            FlagAlighLeftRight,
            FlagAlighLeft,
            FlagAlighRight,
            FlagAlighTruncCen1,
            FlagAlighTruncCen2,
            FlagAlighCen]);
         if      (Flag = FlagAlighLeft ) or (Flag = FlagAlighTruncLeft ) then
            Align := alLeft
         else if (Flag = FlagAlighRight) or (Flag = FlagAlighTruncRight) then
            Align := alRight
         else if Flag = FlagAlighLeftRight then
            Align := alLeftRight
         else if (Flag = FlagAlighCen) or
                 (Flag = FlagAlighTruncCen1) or
                 (Flag = FlagAlighTruncCen2) then
            Align := alCenter;
         if (Flag = FlagAlighTruncLeft) or (Flag = FlagAlighTruncRight) or
            (Flag = FlagAlighTruncCen1) or (Flag = FlagAlighTruncCen2) then
            Truncate := True;

         // Sign
         Sign := signNegative;
         GetField(
            Flag, Index, FormatStr,
            [FlagSignNegative, FlagSignAllways, FlagSignSpace]);
         if      Flag = FlagSignAllways then
            Sign := signAllways
         else if Flag = FlagSignSpace   then
            Sign := signSpace;

         // Padding
             FillChar := ' ';
         LeftFillChar := ' ';
         if ( Index <= Length(FormatStr) ) and
            not ( FormatStr[Index] in (Numerals - [FlagZeroPad]) ) and
            not ( FormatStr[Index] in NotFill ) then
            begin
            GetChar(FillChar, Index, FormatStr);
            LeftFillChar := FillChar;
            if FillChar = FlagZeroPad then
               FillChar := ' ';
            end;

         // Width, precision
         GetNumber(Width, Index, FormatStr, {Default:} 0);
         GetField(Flag, Index, FormatStr, [FlagFrac, FlagSignif]);
         if      Flag = FlagFrac   then
            PrecisionType := prFrac
         else if Flag = FlagSignif then
            PrecisionType := prSignif
         else
            PrecisionType := prNone;
         GetNumber(Precision, Index, FormatStr);

         // Specifier
         Specifier := spNone;
         ThousandsChar := DefaultThousandsChar;
         ExpWidth := DefaultExpWidth;
         GetField(
            Flag, Index, FormatStr,
            [FlagFixed, FlagExp, FlagThousands, FlagPercent, OpenDim]);
         if Flag = '' then
            case PrecisionType of
               prFrac   :  Specifier := spFixed;
               prSignif :  Specifier := spExp;
               end
         else if Flag = FlagFixed then
            Specifier := spFixed
         else if Flag = FlagExp then
            begin
            Specifier := spExp;
            GetNumber(ExpWidth, Index, FormatStr, DefaultExpWidth);
            end
         else if Flag = OpenDim then
            begin
            Specifier := spSI;
            if GetPos(IndexClose, FormatStr, CloseDim, {Start:} Index) = Success then
               Units := CopyRange(FormatStr, Index, IndexClose - 1)
            else
               Units := '';
            end
         else if Flag = FlagThousands then
            begin
            Specifier := spThousands;
            GetChar(ThousandsChar, Index, FormatStr);
            end
         else if Flag = FlagPercent then
            Specifier := spPercent;
         end;
      end;


   // Format the sign of Arg according to Options
   function FormatSign(
            Arg      :  Integer;
      const Options  :  TFormatOptions
            )        :  AnsiString;
      begin
      if Arg < 0 then
         Result := '-'
      else
         case Options.Sign of
            signNegative:  Result := '';
            signAllways :  Result := '+';
            signSpace   :  Result := Options.FillChar;
            else
               Assert(False, 'Unknown sign flag');
            end;
      end;


   // Calculate the spacing for alignment
   procedure CalcSpacing(
      var    LeftGap,
              MidGap,
            RightGap    :  Integer;
            Figure,
            Sign        :  AnsiString;
      const Options     :  TFormatOptions);
      var
            SpaceAvail  :  Integer;
      begin
       LeftGap := 0;
        MidGap := 0;
      RightGap := 0;
      SpaceAvail := Options.Width - Length(Sign) - Length(Figure);
      if SpaceAvail > 0 then
         case Options.Align of
            alLeft:
               RightGap := SpaceAvail;
            alRight:
                LeftGap := SpaceAvail;
            alLeftRight:
                 MidGap := SpaceAvail;
            alCenter:
               begin
                LeftGap := SpaceAvail div 2;
               RightGap := SpaceAvail - LeftGap;
               end;
            else
               Assert(False, 'Unknown alignment type');
            end;
      end;


   // Format a number with specified sign according to Options
   function FormatNumber(
            Number      :  AnsiString;
      const Sign        :  AnsiString;
      const Options     :  TFormatOptions
            )           :  AnsiString;
      var
             LeftFill,
              MidFill,
            RightFill   :  AnsiString;
             LeftGap,
              MidGap,
            RightGap,
            i           :  Integer;
      const
            DecimalDot  = '.';
      begin
      // Get rid of the sign if already present
      if Number[1] = '-' then
         Number := SuffixFrom(Number, 2);

      // Thousands separator
      if Options.Specifier = spThousands then
         begin
         if GetPos(i, Number, DecimalDot) = Fail then
            i := Length(Number) + 1;
         Dec(i);
         while True do
            begin
            Dec(i, 3);
            if i < 1 then
         {<---}break
            else
               Insert(Number, i, Options.ThousandsChar);
            end;
         end;

      // Calculate the spacing, fill
      CalcSpacing(LeftGap, MidGap, RightGap, Number, Sign, Options);
       LeftFill := DupeString(Options.LeftFillChar,  LeftGap);
        MidFill := DupeString(Options.LeftFillChar,   MidGap);
      RightFill := DupeString(Options.    FillChar, RightGap);
      Result := LeftFill + Sign + MidFill + Number + RightFill;
      end;


   // Format a real number according to Options
   function FormatReal(
            Arg         :  Extended;
      var   Options     :  TFormatOptions
            )           :  AnsiString;
      var
            Number,
            ExpPart     :  AnsiString;
            Precision,
            Digits      :  Integer;
            ExpIndex    :  Integer;
      const
            ExpChar     = 'e';
            MaxPrecision = 20;
      begin
      // Set default values if precision is not specified
      with Options do
         if Precision = NotSpecified then
            begin
            PrecisionType := prSignif;
            Precision := MaxPrecision;
            Specifier := spExp;
            end;

      // Preprocess argument
      if Options.Specifier = spPercent then
         Arg := 100 * Arg;

      case Options.Specifier of
         spNone, spFixed, spPercent, spThousands:
            begin
            // Transform to string
            // after calculating number of digits after decimal point
            if Options.PrecisionType = prSignif then
               Digits := SignifToFrac(Arg, Options.Precision, {NonNegative:} True)
            else
               Digits := Options.Precision;
            Number := FloatToStrF(
               Arg, ffFixed, {Precision:} High(Integer), Digits);
            end;

         spExp:
            begin
            // Transform to string
            // after calculating number of digits before the exponent
            Precision := Options.Precision;
            if Options.PrecisionType = prFrac then
               Inc(Precision);
            Number := LowerCase(
               FloatToStrF(
                  Arg, ffExponent, Precision, {Digits:} 1
                  )
               );

            // Process the exponent part
            if GetPos(ExpIndex, Number, ExpChar) = Success then with Options do
               begin
               ExpPart := SuffixFrom(Number, ExpIndex + 2);
               if Options.Align in [alLeft, alCenter] then
                  ExpPart := PadRight(ExpPart, FillChar, ExpWidth)
               else
                  ExpPart := PadLeft (ExpPart, '0', ExpWidth);
               Number := Prefix(Number, ExpIndex + 1) + ExpPart;
               end
            else
               Assert(False, 'Exp part not found');
            end;

         spSI:
            with Options do
               Number := FormatSI(
                  Arg, Units, Precision, PrecisionType = prSignif, FillChar);
         else
            Assert(False, 'Unexpected specifier');
         end;
      // Postprocess, format
      if Options.Specifier = spPercent then
         Number := Number + '%';
      Result := FormatNumber(Number, FormatSign(Sign(Arg), Options), Options);
      end;


   // Format integer number according to Options
   function FormatInt(
            Arg         :  Integer;
      var   Options     :  TFormatOptions
            )           :  AnsiString;
      begin
      if Options.Specifier in [spFixed, spExp, spSI, spPercent] then
         Result := FormatReal(Arg, Options)
      else
         Result := FormatNumber(IntToStr(Arg), FormatSign(Arg, Options), Options);
      end;


   // Format string according to Options
   function FormatString(
      const Arg         :  AnsiString;
      const Options     :  TFormatOptions
            )           :  AnsiString;
      var
             LeftFill,
            RightFill   :  AnsiString;
             LeftGap,
              MidGap,
            RightGap,
            Delta       :  Integer;
      begin
      // Calculate spacing, fill
      CalcSpacing(LeftGap, MidGap, RightGap, Arg, {Sign:} '', Options);
      LeftGap := LeftGap + MidGap;
       LeftFill := DupeString(Options.FillChar,  LeftGap);
      RightFill := DupeString(Options.FillChar, RightGap);
      Result := LeftFill + Arg + RightFill;

      // Truncate
      if Options.Truncate then
         case Options.Align of
            alLeft:
               Result := Prefix(Result, Options.Width);
            alRight, alLeftRight:
               Result := Suffix(Result, Options.Width);
            alCenter:
               begin
               Delta := Length(Result) - Options.Width;
               if Delta > 0 then
                  Result := Copy(Result, 1 + Delta div 2, Options.Width);
               end;
            else
               Assert(False, 'Unknown alignment');
            end;
      end;

   const
          OpenArg = '{';
         CloseArg = '}';
          OpenX2 =  OpenArg +  OpenArg;
         CloseX2 = CloseArg + CloseArg;
         BoolName : array [Boolean] of AnsiString = ('False', 'True');
   var
         iArg           :  Integer;
          OpenIndex,
         CloseIndex,
          PrevIndex,
         NextBracket    :  Integer;
         FormattedArg   :  AnsiString;
         ArgOptions     :  TFormatOptions;
         FoundOpen,
         FoundClose     :  Boolean;
   begin
   PrevIndex := 1;
   Result := '';
   iArg := Low(Args);
   repeat
      // Find position of next brace
      FoundOpen  := GetPos( OpenIndex, S, OpenArg,  {Start:} PrevIndex) = Success;
      FoundClose := GetPos(CloseIndex, S, CloseArg, {Start:} PrevIndex) = Success;
      if FoundOpen and FoundClose then
         NextBracket := Min(OpenIndex, CloseIndex)
      else
         NextBracket := Max(OpenIndex, CloseIndex);

      // No brace found, exit
      if not (FoundOpen or FoundClose) then
         break

      // Double-escaped braces
      else if  SubstrAtPos(S,  OpenX2, NextBracket) or
               SubstrAtPos(S, CloseX2, NextBracket) then
         begin
         Result := Result + CopyRange(S, PrevIndex, NextBracket);
         PrevIndex := NextBracket + 2;
         end

      // Single closing brace, copy as is
      else if SubstrAtPos(S, CloseArg, NextBracket) then
         begin
         Result := Result + CopyRange(S, PrevIndex, NextBracket);
         PrevIndex := NextBracket + 1;
         end

      // Single opening brace, format
      else if SubstrAtPos(S,  OpenArg, NextBracket) then
         begin
         GetFormatOptions( ArgOptions, CopyRange(S, OpenIndex + 1, CloseIndex - 1) );
         case Args[iArg].VType of
            vtInteger:
               FormattedArg := FormatInt   (Args[iArg].VInteger,     ArgOptions);
            vtExtended:
               FormattedArg := FormatReal  (Args[iArg].VExtended^,   ArgOptions);
            vtString:
               FormattedArg := FormatString(Args[iArg].VString^,     ArgOptions);
            vtAnsiString:
               FormattedArg := FormatString(
                                 AnsiString(Args[iArg].VAnsiString), ArgOptions);
            vtChar:
               FormattedArg := FormatString(Args[iArg].VChar,        ArgOptions);
            vtBoolean:
               FormattedArg := FormatString(
                                   BoolName[Args[iArg].VBoolean],    ArgOptions);
            else
               Assert(False, 'Unexpected type');
            end;
         Result := Result + CopyRange(S, PrevIndex, OpenIndex - 1) + FormattedArg;
         PrevIndex := CloseIndex + 1;
         Inc(iArg);
         end;
   until ( iArg > High(Args) );
   Result := Result + CopyRange( S, PrevIndex, Length(S) );
   end;

end.
