{ 
Copyright (c) Peter Karpov 2010 - 2019.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit Problem; ///////////////////////////////////////////////////////////////////////
{
>> Version: 0.1

>> Description
   Binary texture generation PDM: generate a binary texture that matches the 
   statistics of a given sample.

   Supported algorithms:
   - Simulated Annealing
   
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru
   
>> ToDo
   ! Multiresolution statistics

>> Changelog
   0.1 : 2018.12.22 + Parameters moved to config
   0.0 : 2018.11.07 + Initial version
   Notation: + added, - removed, * fixed, ~ changed   
}
interface ///////////////////////////////////////////////////////////////////////////
uses
      InvSys,
      Arrays;
const
      Minimization   =  True;
      FileExtension  =  'pbm';
      NHists         =  4;
      MaxChangeSize  =  4;
type
      // Basic types
      TScore = Real;
      
      THist =
         record
         _        :  TIntArray;
         Total    :  Integer;
         InvTotal :  Real;
         end;
      THists = array [1 .. NHists] of THist;
         
      TPixel = Integer; 
      TImage = array of array of TPixel;
         
      TSolution =
         record
         Pixels      :  TImage;
         Hists       :  THists;
         IsChanged   :  array [1 .. NHists] of TBoolArray;
         Undone      :  Boolean;
         IdSave      :  Integer;
         Score       :  TScore;
         end;

      TSolutionDistance = Integer;

      // Local search
      TMove = Nothing;
      TMoveUndo = Nothing;
      
      // Simulated Annealing
      TIdTable = array [1 .. NHists] of TIntArray;
      
      TSAUndo = 
         record
         x, y, PatchSize   :  Integer;
         OldPixels         :  array [0 .. MaxChangeSize - 1, 
                                     0 .. MaxChangeSize - 1] of TPixel;
         OldIds, NewIds,
         ChangedIds        :  TIdTable;
         OldScore          :  TScore;
         end;
      
      // Tabu search
      TTabuList = Nothing;

{$I Interface.inc}
implementation //////////////////////////////////////////////////////////////////////
uses
      Math,
      ExtraMath,
      Statistics,
      Sorting,
      RandVars,
      Formatting,
      Common,
      Messages,
      StringLists,
      IniConfigs;
var
      ImageSize,
      SavePeriod        :  Integer;
      Sample            :  TImage;
      TargetHists       :  THists;
      PathAnim          :  AnsiString;
      UseSamplePatches,
      RandChangeSize    :  Boolean;
type
      TDistFunc = (dfL1, dfL2, dfTriangDiscr, dfHellinger, dfJSD);
      TByteArray = array of Byte;
const
      HistW             :  array [1 .. NHists] of Real = (1, 1, 1, 1);
      HistDistFunc      :  TDistFunc = dfHellinger;

{-----------------------<< Pixel Patches >>-----------------------------------------}

// Convert a patch of Pixels of a given Size located at (x, y) to a histogram index
{$RANGECHECKS OFF} {$OVERFLOWCHECKS OFF} 
function PatchToId(
   const Pixels      :  TImage;
         x, y, Size  :  Integer
         )           :  Integer;
   var
         dx, dy, Sum :  Integer;
   begin
   case Size of
      1: Sum := 
            Pixels[y    , x    ]       ;
      2: Sum := 
            Pixels[y    , x    ] shl 3 + 
            Pixels[y    , x + 1] shl 2 + 
            Pixels[y + 1, x    ] shl 1 + 
            Pixels[y + 1, x + 1]       ;
      3: Sum := 
            Pixels[y    , x    ] shl 8 + 
            Pixels[y    , x + 1] shl 7 + 
            Pixels[y    , x + 2] shl 6 + 
            Pixels[y + 1, x    ] shl 5 + 
            Pixels[y + 1, x + 1] shl 4 +
            Pixels[y + 1, x + 2] shl 3 +
            Pixels[y + 2, x    ] shl 2 + 
            Pixels[y + 2, x + 1] shl 1 +
            Pixels[y + 2, x + 2]       ;
      4: Sum := 
            Pixels[y    , x    ] shl 15 + 
            Pixels[y    , x + 1] shl 14 + 
            Pixels[y    , x + 2] shl 13 + 
            Pixels[y    , x + 3] shl 12 + 
            Pixels[y + 1, x    ] shl 11 + 
            Pixels[y + 1, x + 1] shl 10 +
            Pixels[y + 1, x + 2] shl  9 +
            Pixels[y + 1, x + 3] shl  8 +
            Pixels[y + 2, x    ] shl  7 + 
            Pixels[y + 2, x + 1] shl  6 +
            Pixels[y + 2, x + 2] shl  5 +
            Pixels[y + 2, x + 3] shl  4 +
            Pixels[y + 3, x    ] shl  3 + 
            Pixels[y + 3, x + 1] shl  2 +
            Pixels[y + 3, x + 2] shl  1 +
            Pixels[y + 3, x + 3]        ;
      else
         Sum := 0;
         for dy := 0 to Size - 1 do
            for dx := 0 to Size - 1 do
               Sum := (Sum shl 1) + Pixels[y + dy, x + dx];
      end;
   Result := Sum;
   end;
   
   
// Collect Ids of patches of Pixels of PatchSize overlapping with the 
// square of ChangeSize located at (x, y) and lie within image boundaries
procedure CollectPatchIds(
   var   Ids         :  TIntArray;
   const Pixels      :  TImage;
         x, y, 
         PatchSize,
         ChangeSize  :  Integer);
   var
         i, j, k,
         MinX, MaxX,
         MinY, MaxY  :  Integer;
   begin
   MinX := Max(x - PatchSize + 1, 0);
   MinY := Max(y - PatchSize + 1, 0);
   MaxX := Min(x + ChangeSize - 1, Length(Pixels[0]) - PatchSize);
   MaxY := Min(y + ChangeSize - 1, Length(Pixels   ) - PatchSize);
   SetLength(Ids, (MaxX - MinX + 1) * (MaxY - MinY + 1));
   k := 0;
   for j := MinY to MaxY do
      for i := MinX to MaxX do
         begin
         Ids[k] := PatchToId(Pixels, i, j, PatchSize);
         Inc(k);
         end;
   end;
{$RANGECHECKS ON} {$OVERFLOWCHECKS ON} 
   
   
// Collect Ids of patches of Pixels of all sizes that overlap with the
// square of ChangeSize located at (x, y) and lie within image boundaries
procedure CollectAllPatchIds(
   var   IdTable     :  TIdTable;
   const Pixels      :  TImage;
         x, y, 
         ChangeSize  :  Integer);
   var
         PatchSize   :  Integer;
   begin
   for PatchSize := 1 to NHists do
      CollectPatchIds(IdTable[PatchSize], Pixels, x, y, PatchSize, ChangeSize);
   end;

{-----------------------<< Histograms >>-------------------------------------------}
   
// Return the distance between the elements of Target and Actual histogram 
// located at Index
function HistElemDist(
   const Actual,
         Target   :  THist;
         Index    :  Integer
         )        :  Real;
   var
         A, B, M  :  Real;
   begin
   A := Actual.InvTotal * Actual._[Index];
   B := Target.InvTotal * Target._[Index];
   case HistDistFunc of
      dfL1:         
         Result := Abs(A - B);
      dfL2:         
         Result := Sqr(A - B) * Length(Actual._);
      dfTriangDiscr:    
         Result := SafeDiv(Sqr(A - B), A + B, 0);
      dfHellinger:  
         Result := Sqr(Sqrt(A) - Sqrt(B));
      dfJSD:
         begin
         M := (A + B) / 2;
         if M = 0 then
            Result := 0 else
            Result := A * SafeLn(A / M, 0) + B * SafeLn(B / M, 0);
         end;
      else
         Assert(False);
         Result := 0;
      end;
   end;

   
// Return the distance between the Target and Actual histograms
function HistDist(
   const Actual,
         Target   :  THist
         )        :  Real;
   var
         Sum      :  Real;
         i        :  Integer;
   begin
   Assert(Length(Actual._) = Length(Target._));
   Sum := 0;
   for i := 0 to Length(Actual._) - 1 do
      Sum := Sum + HistElemDist(Actual, Target, i);
   Result := Sum;
   end;
   
   
// Return the subscore calculated only from elements in IdTable
function CalcSubscore(
   const Actual,
         Target   :  THists;
         IdTable  :  TIdTable
         )        :  Real;
   var
         Sum, 
         SubSum   :  Real;
         i, j     :  Integer;
   begin
   Sum := 0;
   for i := 1 to NHists do
      begin
      SubSum := 0;
      for j := 0 to Length(IdTable[i]) - 1 do
         SubSum := SubSum + HistElemDist(Actual[i], Target[i], IdTable[i, j]);
      Sum := Sum + SubSum * HistW[i];
      end;
   Result := Sum;
   end;
   
   
// Assign HistFrom to HistTo
procedure AssignHist(
   var   HistTo   :  THist;
   const HistFrom :  THist);
   begin
   HistTo._        := Copy(HistFrom._);
   HistTo.Total    :=      HistFrom.Total;
   HistTo.InvTotal :=      HistFrom.InvTotal;
   end;
   
   
// Initialize the histogram Hist intended for patches of a given Size
procedure InitHist(
   var   Hist     :  THist;
         Size     :  Integer);
   begin
   InitArray(Hist._, {Len:} 1 shl Sqr(Size), {Value:} 0);
   Hist.Total    := 0;
   Hist.InvTotal := 0;
   end;
   
   
// Calculate the histogram Hist of patches of Pixels of a given Size
procedure CalcHist(
   var   Hist     :  THist;
   const Pixels   :  TImage;
         Size     :  Integer);
   var
         x, y, Id :  Integer;
   begin
   InitHist(Hist, Size);
   for y := 0 to Length(Pixels) - Size do
      for x := 0 to Length(Pixels[y]) - Size do
         begin
         Id := PatchToId(Pixels, x, y, Size);
         Inc(Hist._[Id]);
         Inc(Hist.Total);
         end;
   Hist.InvTotal := 1 / Hist.Total;
   end;

   
// Calculate the histograms Hists of patches of Pixels of all sizes
procedure CalcHists(
   var   Hists    :  THists;
   const Pixels   :  TImage);
   var
         Size     :  Integer;
   begin
   for Size := 1 to NHists do
      CalcHist(Hists[Size], Pixels, Size);
   end;
   
   
// Update histograms Hists by adding Change to ids from IdTable
{$RANGECHECKS OFF} {$OVERFLOWCHECKS OFF} 
procedure UpdateHists(
   var   Hists    :  THists;
   const IdTable  :  TIdTable;
         Change   :  Integer);
   var
         i, j     :  Integer;
   begin
   for i := 1 to NHists do
      for j := 0 to Length(IdTable[i]) - 1 do
         Inc(Hists[i]._[IdTable[i, j]], Change);
   end;
{$RANGECHECKS ON} {$OVERFLOWCHECKS ON} 
   
{-----------------------<< Misc problem-specific routines >>------------------------}
   
// Try to load Pixels from Path, return status
function LoadPBM(
   var   Pixels   :  TImage;
   const Path     :  AnsiString
         )        :  Boolean;
   var
         FileText :  Text;
         Magic    :  AnsiString;
         Char     :  AnsiChar;
         x, y, k,
         SizeX,
         SizeY    :  Integer;
   const
         MSB      =  7;
   begin
   // Try to open the file
   Assign(FileText, Path);
   SetLineBreakStyle(FileText, tlbsLF);
   Reset(FileText);
   //Result := Fail;
   //if OpenRead(FileText, Path) = Fail then
   //{<}exit;

   // Check the magic number
   Magic := '';
   for k := 1 to 2 do
      begin
      Read(FileText, Char);
      Magic := Magic + Char;
      end;
   if Magic <> 'P4' then
      begin
      Close(FileText);
   {<}exit;
      end;

   // Initialize the array
   Read(FileText, SizeX, SizeY, Char);
   SetLength(Pixels, SizeY);
   for y := 0 to SizeY - 1 do
      SetLength(Pixels[y], SizeX);

   // Read the bits
   for y := 0 to SizeY - 1 do
      begin
      k := -1;
      for x := 0 to SizeX - 1 do
         begin
         if k < 0 then
            begin
            k := MSB;
            Read(FileText, Char);
            end;
         Pixels[y, x] := Ord( (Byte(Char) and (1 shl k)) <> 0 );
         Dec(k);
         end;
      end;
   Result := Success;
   end;
   
   
// Convert an image to a byte array
procedure ImageToBytes(
   var   Bytes       :  TByteArray;
   const Pixels      :  TImage);
   var
         x, y, 
         i, j, k,
         LenX, LenY,
         LenBytesX   :  Integer;
         b           :  Byte;
   const
         MSB         =  7;
   begin
   LenY := Length(Pixels);
   LenX := Length(Pixels[0]);
   LenBytesX := (LenX + MSB) div (MSB + 1);
   SetLength(Bytes, LenY * LenBytesX);
   for y := 0 to LenY - 1 do
      for i := 0 to LenBytesX - 1 do
         begin
         b := 0;
         for j := 0 to MSB do
            begin
            x := i * (MSB + 1) + j;
            if x < LenX then
               b := b + Pixels[y, x] shl (MSB - j);
            end;
         Bytes[y * LenBytesX + i] := b;
         end;
   end;
   
   
// Save Pixels to FilePBM
procedure SavePBMToText(
   var   FilePBM  :  Text;
   const Pixels   :  TImage);
   var
         Bytes    :  TByteArray;
         i        :  Integer;
   begin
   Write(FilePBM, 'P4 ', Length(Pixels[0]), ' ', Length(Pixels), ' ');
   ImageToBytes(Bytes, Pixels);
   for i := 0 to Length(Bytes) - 1 do
      Write(FilePBM, AnsiChar(Bytes[i]));
   end;
   
 
// Save Pixels to Path
procedure SavePBM(
   const Path     :  AnsiString;
   const Pixels   :  TImage);
   var
         FilePBM  :  Text;
   begin
   OpenWrite(FilePBM, Path);
   SavePBMToText(FilePBM, Pixels);
   Close(FilePBM);
   end;
   
   
// Problem-specific improvement
procedure SpecialImprove(
   var   Solution :  TSolution);
   begin
   Assert(False);
   end;

{-----------------------<< Scores >>------------------------------------------------}

// Set Solution's score
procedure SetScore(
   var   Solution :  TSolution);
   var
         i        :  Integer;
   begin
   with Solution do
      begin
      Score := 0;
      CalcHists(Hists, Pixels);
      for i := 1 to NHists do
         Score := Score + HistDist(Hists[i], TargetHists[i]) * HistW[i];
      end;
   end;


// Return a string representation of Score
function FormatScore(
         Score :  TScore
         )     :  AnsiString;
   begin
   Str(Score, Result);
   end;

{-----------------------<< Solution operations >>-----------------------------------}

// SolTo := SolFrom
procedure AssignSolution(
   var   SolTo    :  TSolution;
   const SolFrom  :  TSolution);
   var
         i        :  Integer;
   begin
   SetLength(SolTo.Pixels, Length(SolFrom.Pixels));
   for i := 0 to Length(SolFrom.Pixels) - 1 do
      SolTo.Pixels[i] := Copy(SolFrom.Pixels[i]);
   for i := 1 to NHists do
      begin
      AssignHist(SolTo.Hists[i], SolFrom.Hists[i]);
      SolTo.IsChanged[i] := Copy(SolFrom.IsChanged[i]);
      end;
   SolTo.Score  := SolFrom.Score;
   SolTo.Undone := SolFrom.Undone;
   SolTo.IdSave := SolFrom.IdSave;
   end;


// Save the Solution to FileSol
procedure SaveSolution(
   var   FileSol     :  Text;
   const Solution    :  TSolution);
   var
         x, y        :  Integer;
   begin
   WriteLn(FileSol, 'P1 ', ImageSize, ' ', ImageSize, AnsiChar(10));
   for y := 0 to ImageSize - 1 do
      begin
      for x := 0 to ImageSize - 1 do
         Write(FileSol, Solution.Pixels[y, x]);
      WriteLn(FileSol);
      end;
   end;


// Load a Solution from FileSol
procedure LoadSolution(
   var   Solution    :  TSolution;
   var   FileSol     :  Text);
   begin
   Assert(False);
   end;


// Create a new random Solution
procedure NewSolution(
   var   Solution :  TSolution);
   var
         x, y, k  :  Integer;
         P1       :  Real;
   begin
   // Create the array
   SetLength(Solution.Pixels, ImageSize);
   for y := 0 to ImageSize - 1 do
      SetLength(Solution.Pixels[y], ImageSize);
     
   // Fill with noise having the same mean
   P1 := TargetHists[1]._[1] / TargetHists[1].Total;
   for y := 0 to ImageSize - 1 do   
      for x := 0 to ImageSize - 1 do
         Solution.Pixels[y, x] := Ord(Random < P1);
         
   // Initialize histogram change masks
   for k := 1 to NHists do
      InitArray(Solution.IsChanged[k], {Len:} 1 shl Sqr(k), {Value:} False);
         
   Solution.Undone := False;
   Solution.IdSave := 0;
   SetScore(Solution);
   end;
   

// Return the distance between Solution1 and Solution2
function Distance(
   const Solution1,
         Solution2   :  TSolution
         )           :  TSolutionDistance;
   var
         x, y        :  Integer;
         Sum         :  TSolutionDistance;
   begin
   Sum := 0;
   for y := 0 to ImageSize - 1 do
      for x := 0 to ImageSize - 1 do
         if Solution1.Pixels[y, x] <> Solution2.Pixels[y, x] then
            Inc(Sum);
   Result := Sum;
   end;

{-----------------------<< Simulated Annealing >>-----------------------------------}

// Apply a random move to the Solution at temperature T, save Undo
{$RANGECHECKS OFF} {$OVERFLOWCHECKS OFF} 
procedure MakeNeighbour(
   var   Solution    :  TSolution;
   var   Undo        :  TSAUndo;
         T           :  Real);
   var
         Changed     :  Boolean;
         OldValue,
         NewValue    :  TPixel;
         i, j, k, m,
         xs, ys      :  Integer;
         OldSubscore,
         NewSubscore :  Real;
         Path        :  AnsiString;
   begin
   with Solution, Undo do
      begin
      // Save accepted solutions
      if (SavePeriod <> 0) and not Undone then
         begin
         if IdSave mod SavePeriod = 0 then
            SavePBM(Format(PathAnim + '{>08}.pbm', [IdSave div SavePeriod]), Pixels);
         Inc(IdSave);
         end;
      
      // Select a patch to modify, save old Ids
      if RandChangeSize then
         PatchSize := RandRange(1, MaxChangeSize) else
         PatchSize := MaxChangeSize;
      x := RandRange(0, ImageSize - PatchSize);
      y := RandRange(0, ImageSize - PatchSize);
      CollectAllPatchIds(OldIds, Pixels, x, y, PatchSize);
      
      // Change a patch of pixels, save new Ids
      Changed := False;
      repeat
         if UseSamplePatches then
            begin
            xs := RandRange(0, Length(Sample[0]) - PatchSize);
            ys := RandRange(0, Length(Sample   ) - PatchSize);
            end;
         for j := 0 to PatchSize - 1 do
            for i := 0 to PatchSize - 1 do
               begin
               OldValue := Pixels[y + j, x + i];
               if PatchSize = 1 then
                  NewValue := 1 - OldValue
               else if UseSamplePatches then
                  NewValue := Sample[ys + j, xs + i] 
               else
                  NewValue := Random(2);
               Changed := Changed or (OldValue <> NewValue);
               OldPixels[    j,     i] := OldValue;
                  Pixels[y + j, x + i] := NewValue;
               end;
      until Changed;
      CollectAllPatchIds(NewIds, Pixels, x, y, PatchSize);
      
      // #HOTSPOT
      // Find unique changed Ids
      for k := 1 to NHists do
         begin
         Concat(ChangedIds[k], OldIds[k], NewIds[k]);
         m := -1;
         for i := 0 to Length(ChangedIds[k]) - 1 do
            begin
            j := ChangedIds[k, i];
            if not IsChanged[k, j] then
               begin
               Inc(m);
               ChangedIds[k, m] := j;
               IsChanged[k, j] := True;
               end;
            end;
         SetLength(ChangedIds[k], m + 1);
         for i := 0 to m do
            IsChanged[k, ChangedIds[k, i]] := False;
         end;
      
      // Update the histograms and the score
      OldSubscore := CalcSubscore(Hists, TargetHists, ChangedIds);
      UpdateHists(Hists, OldIds, {Change:} -1);
      UpdateHists(Hists, NewIds, {Change:} +1);
      NewSubscore := CalcSubscore(Hists, TargetHists, ChangedIds);
      OldScore := Score;
      Score := Score + (NewSubscore - OldSubscore);
      Undone := False;
      end;
   end;
{$RANGECHECKS ON} {$OVERFLOWCHECKS ON} 


// Undo the last move applied to Solution
procedure UndoSAMove(
   var   Solution :  TSolution;
   const Undo     :  TSAUndo);
   var
         i, j     :  Integer;
   begin
   with Solution, Undo do
      begin
      for j := 0 to PatchSize - 1 do
         for i := 0 to PatchSize - 1 do
            Pixels[y + j, x + i] := OldPixels[j, i];
      UpdateHists(Hists, OldIds, {Change:} +1);
      UpdateHists(Hists, NewIds, {Change:} -1);
      Score := OldScore;
      Undone := True;
      end;
   end;
   
{-----------------------<< Dummy Sections >>----------------------------------------}

{$I DummyLS.inc}
{$I DummyGA.inc}
{$I DummyTS.inc}

/////////////////////////////////////////////////////////////////////////////////////
var
      Config      :  TIniConfig;
      PathInput   :  AnsiString;
      Errors      :  TStringList;
      Failed      :  Boolean;

initialization 
// Read the texture generation parameters
LoadConfig(Config, 'Config.ini', Errors);
Failed := False;
repeat
   if Errors.N > 0 then
      begin
      DisplayStringList(Errors);
      Failed := True;
{<}   break;
      end;
   if GetIntProperty(ImageSize, Config, 'Size') = Fail then
      ImageSize := 128;
   if GetIntProperty(SavePeriod, Config, 'SavePeriod') = Fail then
      SavePeriod := 0;
   if GetStringProperty(PathAnim, Config, 'SavePath') = Fail then
      begin
      PathAnim := '';
      SavePeriod := 0;
      end;
   if GetStringProperty(PathInput, Config, 'Sample') = Fail then
      PathInput := 'Sample.pbm';
   if GetBoolProperty(UseSamplePatches, Config, 'SamplePatches') = Fail then
      UseSamplePatches := True;
   if GetBoolProperty(RandChangeSize, Config, 'RandChangeSize') = Fail then
      RandChangeSize := True;
   if LoadPBM(Sample, PathInput) = Fail then
      begin
      WriteLn('Could not open ' + PathInput);
      Failed := True;
{<}   break;
      end;
until True;

// Calculate the target histograms on success
if Failed then
   begin
   ReadLn;
   exit;
   end
else
   CalcHists(TargetHists, Sample);
end.
