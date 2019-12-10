{ 
Copyright (c) Peter Karpov 2010 - 2019.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit Statistics; ////////////////////////////////////////////////////////////////////
{
>> Version: 2.1

>> Description
   Unit for calculation of various statistical quantities. Part of InvLibs unit 
   collection.
   
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> Notes
   Delphi 7 and FreePascal 3.0.4's implementations of variance and standard deviation
   functions in the Math unit are numerically unstable. Variance may return negative 
   values and StdDev may crash. All related routines (MeanAndStdDev, TotalVariance, 
   ...) are plagued by the same issue and should be avoided entirely. This unit uses 
   StandDev and SqrStandDev in function names to avoid confusion.
   
   Quantile function uses interpolation and thus needs to find two elements, but 
   quickselect normally finds only one. A slight modification of element search 
   seems to do the job, but its correctness remains to be proven. Moreover, even 
   determining the required array index is not trivial. The method that has been 
   reported as the most accurate in [1] is employed.

   RobustMean function is based on a modified one-step estimator, but uses a variable
   rather than fixed rejection threshold. Modification that used the differences 
   between the quartiles and the median instead of MAD was also tested. While it may 
   work better for assymetric distributions, it was found to be less robust for small
   datasets.
   
   Although Mean and Sum functions duplicate the Math unit functionality, they cannot
   be easily removed because of the way Delphi handles overloads.

>> ToDo
    - Prove correctness of quickselect modification used in Quantile function
    - Test RobustMean function
      
>> References
   [1]   Estimation of population percentiles.
         Schoonjans F, De Bacquer D, Schmid P.

>> Changelog
   2.1   : 2019.10.30   + 'Running statistics' section
   2.0   : 2017.12.11   ~ Many routines renamed. Procedures now have the 
                          "Get" prefix, functions do not
                        - RandMinIndex, RandMaxIndex functions moved to 
                          Arrays unit
   1.20  : 2015.06.26   + PowerMean function
   1.19  : 2014.08.27   + 'Correlation' section
   1.18  : 2013.06.14   + Integer version of GetSum
   1.17  : 2013.06.13   + Integer version of Midspread
   1.16  : 2013.06.09   + Normalization argument for RankTransform, overloaded
                          version for integer data
   1.15  : 2013.05.25   + GetVariance function
                        + GetSigma for TRealArray2D
   1.14  : 2013.05.23   + Overloaded versions of GetMean
   1.13  : 2013.03.26   + Weighted average procedure
                        + Overloaded versions of Quantile and GetMedian
                          for ArrayNs and IntArrays
   1.12  : 2013.02.08   + Zero mean normalization type
                        + Overloaded version of Normalize
   1.11  : 2013.01.08   + GetSum function
                        + Unit sum normalization type
   1.10  : 2012.12.21   + RankTransform procedure
   1.9   : 2012.04.18   + GetSigma function
   1.8   : 2011.11.28   + Mean absolute deviation function
   1.7   : 2011.11.15   + New normalization modes
   1.6   : 2011.11.05   ~ Rewritten quantile function to use interpolation
                        + RobustMean function
                        + Normalization
   1.4.1 : 2011.10.14   ~ Moved to generic types
   1.4   : 2011.09.10   ~ RandMinIndex, RandMaxIndex: open arrays, improved speed
   1.3   : 2011.07.11   + Minimum, Maximum for open arrays and ints
   1.2   : 2011.06.14   + Minimum, Maximum, RandMinIndex, RandMaxIndex functions
   1.1   : 2011.06.07   ~ Rewritten 'Mean, Sigma, Skew' section
   1.0   : 2011.04.13   + 'Quantiles' and 'Error Measures' sections
   0.4   : 2011.03.28   + Open array parameters
   0.0   : 2011.01.30   + Initial version
   Notation: + added, - removed, * fixed, ~ changed
}
interface ///////////////////////////////////////////////////////////////////////////
uses
      Arrays;

{-----------------------<< Moments >>-----------------------------------------------}

// Sum of Data elements
function Sum(
   const Data     :  array of Real
         )        :  Real;
         overload;

function Sum(
   const Data     :  array of Integer
         )        :  Integer;
         overload;

// Arithmetic mean of Data
function Mean(
   const Data     :  array of Real
         )        :  Real;
         overload;

function Mean(
   const Data     :  TRealArrayN
         )        :  Real;
         overload;
         
// Return the mean and the bias-corrected variance of Data. 
// SqrStandDev is set to zero if there are less than 2 values.
procedure GetMeanSqrStandDev(
   var   Mean, 
         SqrStandDev :  Real;
   const Data        :  array of Real);

// Return the mean and bias-corrected standard deviation of Data. 
// StandDev is set to zero if there are less than 2 values.
procedure GetMeanStandDev(
   var   Mean,
         StandDev :  Real;
   const Data     :  array of Real);

// Return the variance of Data or zero if there are less than 2 values
function SqrStandDev(
   const Data     :  TRealArray
         )        :  Real;
         overload;
   
function SqrStandDev(
   const Data     :  TRealArray2D
         )        :  Real;
         overload;

// Return the standard deviation of Data or zero if there are less than 2 values
function StandDev(
   const Data     :  TRealArray
         )        :  Real;
         overload;
         
function StandDev(
   const Data     :  TRealArray2D
         )        :  Real;
         overload;

// Return the mean, standard deviation and skewness of Data. StandDev and   
// Skew are set to 0 if there are less than 2 and 3 values respectively.
procedure GetMeanStandDevSkew(
   var   Mean,
         StandDev,
         Skew     :  Real;
   const Data     :  array of Real);
   
{-----------------------<< Running statistics >>------------------------------------}
type 
      TRunningStats = 
         record
         N        :  Integer; // Sample count
         M,                   // Mean
         S, PrevM :  Real;
         end;

// Initialize Stats
procedure InitRunningStats(
   var   Stats    :  TRunningStats);

// Update Stats with sample X
procedure UpdateRunningStats(
   var   Stats    :  TRunningStats;
         X        :  Real);

// Return Stats's standard deviation or 0 if there are less than 2 samples
function StandDev(
   const Stats    :  TRunningStats
         )        :  Real;
         overload;

// Return Stats's standard error or 0 if there are less than 2 samples
function StandError(
   const Stats    :  TRunningStats
         )        :  Real;

{-----------------------<< Other measures of location and scale >>------------------}
         
// Weighted average of X with weights W
function WeightedAverage(
   const X, W  :  TRealArrayN
         )     :  Real;
         overload;

function WeightedAverage(
   const x, w  :  TRealArray
         )     :  Real;
         overload;
         
// Power mean of Data with exponent p
function PowerMean(
   const Data     :  array of Real;
         p        :  Integer
         )        :  Real;
         
// Return the mean of Data excluding outliers.
function RobustMean(
   const Data        :  TRealArray
         )           :  Real;
         
// Mean absolute deviation = Mean(|A - Mean(A)|)
function MeanAbsDeviation(
   const A        :  TRealArray
         )        :  Real;
         
// Median absolute deviation = Median(|A - Median(A)|)
function MedAbsDeviation(
   const A        :  TRealArray
         )        :  Real;

{-----------------------<< Quantiles >>---------------------------------------------}

// Return a quantile corresponding to probability P. Uses interpolation between two
// closest values. Quickselect algorithm, O(N) expected runtime.
function Quantile(
   const A     :  TRealArray;
         P     :  Real        // [0 .. 1]
         )     :  Real;
         overload;

function Quantile(
   const A     :  TRealArrayN;
         P     :  Real        // [0 .. 1]
         )     :  Real;
         overload;

function Quantile(
   const A     :  TIntArray;
         P     :  Real        // [0 .. 1]
         )     :  Real;
         overload;

function Quantile(
   const A     :  TIntArrayN;
         P     :  Real        // [0 .. 1]
         )     :  Real;
         overload;
         
// Median of A, O(N) expected runtime
function Median(
   const A  :  TRealArray
         )  :  Real;
         overload;

function Median(
   const A  :  TRealArrayN
         )  :  Real;
         overload;

function Median(
   const A  :  TIntArray
         )  :  Real;
         overload;

function Median(
   const A  :  TIntArrayN
         )  :  Real;
         overload;

// Interquartile range
function Midspread(
   const A  :  TRealArray
         )  :  Real;
         overload;

function Midspread(
   const A  :  TIntArray
         )  :  Real;
         overload;

{-----------------------<< Min, Max >>----------------------------------------------}         

// Minimal value of the elements of A
function Minimum(
   const A  :  array of Real
         )  :  Real;
         overload;

function Minimum(
   const A  :  array of Integer
         )  :  Integer;
         overload;

// Maximal value of the elements of A
function Maximum(
   const A  :  array of Real
         )  :  Real;
         overload;

function Maximum(
   const A  :  array of Integer
         )  :  Integer;
         overload;

{-----------------------<< Normalization >>-----------------------------------------}
type
      TNormalization = (normUnitSum, normDivMax, normUnit, normNormal, normZeroMean);

// Normalize the Data according to the specified method
procedure Normalize(
   var   Data  :  TRealArray;
         Norm  :  TNormalization);
         overload;

procedure Normalize(
   var   NormData :  TRealArray;
   const Data     :  TRealArray;
         Norm     :  TNormalization);
         overload;

// Perform ranking transformation. Rank of equal items is the mean of their integer
// ranks. Invert changes the sorting direction, Normalized gives [0 .. 1) range.
procedure RankTransform(
   var   Ranks       :  TRealArray;
   const Data        :  TRealArray;
         Invert,
         Normalized  :  Boolean);
         overload;

procedure RankTransform(
   var   Ranks       :  TRealArray;
   const Data        :   TIntArray;
         Invert,
         Normalized  :  Boolean);
         overload;

{-----------------------<< Correlation >>-------------------------------------------}

// Pearson correlation between X and Y
function Corr(
   const X, Y     :  TRealArray
         )        :  Real;

// Spearman's rank correlation between X and Y
function RankCorr(
   const X, Y     :  TRealArray
         )        :  Real;

{-----------------------<< Error measures >>----------------------------------------}
type
      TErrorMeasure = (emRMSE, emMAE, emMdAE, emRMSRE, emMARE, emMdARE);

// Return an error measure between two datasets. Relative error measures are 
// assymetric, X1 stands for the actual and X2 for predicted values.
function ErrorMeasure(
   const X1, X2   :  TRealArray;
         Measure  :  TErrorMeasure
         )        :  Real;

implementation //////////////////////////////////////////////////////////////////////
uses
      Math,
      ExtraMath,  // SignedPower, Clip
      Sorting,
      SpecFuncs;
const
      ErrorSmallArray = 'Too little data';

{-----------------------<< Moments >>-----------------------------------------------}

// Sum of the first Len Data elements
function Sum(
   const Data     :  array of Real;
         Len      :  Integer
         )        :  Real;
         overload;
   var
         i        :  Integer;
         Sum      :  Real;
   begin
   Sum := 0;
   for i := 0 to Len - 1 do
      Sum := Sum + Data[i];
   Result := Sum;
   end;


// Sum of Data elements
function Sum(
   const Data     :  array of Real
         )        :  Real;
         overload;
   begin
   Result := Sum(Data, High(Data) - Low(Data) + 1);
   end;


function Sum(
   const Data     :  array of Integer
         )        :  Integer;
         overload;
   var
         i, Sum   :  Integer;
   begin
   Sum := 0;
   for i := 0 to High(Data) do
      Sum := Sum + Data[i];
   Result := Sum;
   end;
   
   
// Arithmetic mean of the first Len elements of Data
function Mean(
   const Data     :  array of Real;
         Len      :  Integer
         )        :  Real;
         overload;
   begin
   Result := Sum(Data, Len) / Len;
   end;


// Arithmetic mean of Data
function Mean(
   const Data     :  array of Real
         )        :  Real;
         overload;
   var
         N        :  Integer;
   begin
   N := High(Data) - Low(Data) + 1;
   Assert(N > 0, ErrorSmallArray);
   Result := Mean(Data, N);
   end;


function Mean(
   const Data     :  TRealArrayN
         )        :  Real;
         overload;
   begin
   Assert(Data.N > 0, ErrorSmallArray);
   Result := Mean(Data._, Data.N);
   end;


// Return the mean and the bias-corrected variance of Data. 
// SqrStandDev is set to zero if there are less than 2 values.
procedure GetMeanSqrStandDev(
   var   Mean, 
         SqrStandDev :  Real;
   const Data        :  array of Real);
   var
         N, i        :  Integer;
         Sum2        :  Real;
   begin
   N := High(Data) - Low(Data) + 1;
   Assert(N > 0, ErrorSmallArray);
   Mean := Statistics.Mean(Data);
   Sum2 := 0;
   for i := Low(Data) to High(Data) do
      Sum2 := Sum2 + Sqr(Data[i] - Mean);
   if N > 1 then
      SqrStandDev := Sum2 / (N - 1) else
      SqrStandDev := 0;
   end;


// Return the mean and bias-corrected standard deviation of Data. 
// StandDev is set to zero if there are less than 2 values.
procedure GetMeanStandDev(
   var   Mean,
         StandDev :  Real;
   const Data     :  array of Real);
   begin
   GetMeanSqrStandDev(Mean, StandDev, Data);
   StandDev := Sqrt(StandDev);
   end;
   
   
// Return the variance of Data or zero if there are less than 2 values
function SqrStandDev(
   const Data     :  TRealArray
         )        :  Real;
         overload;
   var
         Mean     :  Real;
   begin
   GetMeanSqrStandDev(Mean, Result, Data);
   end;


function SqrStandDev(
   const Data     :  TRealArray2D
         )        :  Real;
         overload;
   var
         Scanline :  TRealArray;
   begin
   ToScanline(Scanline, Data);
   Result := SqrStandDev(Scanline);
   end;
   

// Return the standard deviation of Data or zero if there are less than 2 values
function StandDev(
   const Data     :  TRealArray
         )        :  Real;
         overload;
   begin
   Result := Sqrt(SqrStandDev(Data));
   end;
   

function StandDev(
   const Data     :  TRealArray2D
         )        :  Real;
         overload;
   begin
   Result := Sqrt(SqrStandDev(Data));
   end;


// Return the mean, standard deviation and skewness of Data. StandDev and   
// Skew are set to 0 if there are less than 2 and 3 values respectively.
procedure GetMeanStandDevSkew(
   var   Mean,
         StandDev,
         Skew     :  Real;
   const Data     :  array of Real);
   var
         N, i     :  Integer;
         Variance,
         Sum3,
         Temp     :  Real;
   begin
   N := High(Data) - Low(Data) + 1;
   Assert(N > 0, ErrorSmallArray);

   // Mean, Variance
   GetMeanSqrStandDev(Mean, Variance, Data);
   StandDev := Sqrt(Variance);

   // Skew
   if N > 2 then
      begin
      Sum3 := 0;
      for i := Low(Data) to High(Data) do
         begin
         Temp := Data[i] - Mean;
         Sum3 := Sum3 + Temp * Temp * Temp;
         end;
      if StandDev = 0 then
         Skew := 0
      else
         Skew := ( ( N * Sqrt(N - 1) ) / (N - 2) ) * Sum3 / (Variance * StandDev);
      end
   else
      Skew := 0;
   end;
   
{-----------------------<< Running statistics >>------------------------------------}

// Initialize Stats
procedure InitRunningStats(
   var   Stats    :  TRunningStats);
   begin
   Stats.N := 0;
   Stats.M := 0;
   Stats.S := 0;
   end;
   
   
// Update Stats with sample X
procedure UpdateRunningStats(
   var   Stats    :  TRunningStats;
         X        :  Real);
   begin
   with Stats do
      begin
      Inc(N);
      PrevM := M;
      if N = 1 then
         M := X
      else
         begin
         M := PrevM + (X - PrevM) / N;
         S := S + (X - PrevM) * (X - M);
         end;
      end;
   end;
   
   
// Return Stats's standard deviation or 0 if there are less than 2 samples
function StandDev(
   const Stats    :  TRunningStats
         )        :  Real;
   begin
   with Stats do
      if N > 1 then
         Result := Sqrt( S / (N - 1) ) else
         Result := 0;
   end;
   
   
// Return Stats's standard error or 0 if there are less than 2 samples
function StandError(
   const Stats    :  TRunningStats
         )        :  Real;
   begin
   with Stats do
      if N > 1 then
         Result := Sqrt( S / (N * (N - 1) ) ) else
         Result := 0;
   end;
   
{-----------------------<< Other measures of location and scale >>------------------}

// Weighted average of the first Len elements of X with weights W
function WeightedAverage(
   const x, w        :  TRealArray;
         Len         :  Integer
         )           :  Real;
         overload;
   var
         SumW, SumWX :  Real;
         i           :  Integer;
   begin
   SumW  := 0;
   SumWX := 0;
   for i := 0 to Len - 1 do
      begin
      SumWX := SumWX + w[i] * x[i];
      SumW  := SumW  + w[i];
      end;
   Result := SumWX / SumW;
   end;


// Weighted average of X with weights W
function WeightedAverage(
   const x, w  :  TRealArrayN
         )     :  Real;
         overload;
   begin
   Assert(x.N = w.N);
   Result := WeightedAverage(x._, w._, x.N);
   end;


function WeightedAverage(
   const x, w  :  TRealArray
         )     :  Real;
         overload;
   begin
   Assert(Length(x) = Length(w));
   Result := WeightedAverage(x, w, Length(x));
   end;
   
   
// Power mean of Data with exponent p
function PowerMean(
   const Data     :  array of Real;
         p        :  Integer
         )        :  Real;
   var
         N, i     :  Integer;
         Sum      :  Real;
   begin
   N := High(Data) - Low(Data) + 1;
   Assert(N > 0, ErrorSmallArray);
   Sum := 0;
   for i := Low(Data) to High(Data) do
      Sum := Sum + IntPower(Data[i], p);
   Result := SignedPower(Sum / N, 1 / p);
   end;
   

// Return the mean of Data excluding outliers.
// #TODO try iterative procedure: at each iteration, label all outliers based on
// current mean estimate and compute new estimate. Stop when there are no changes
// in labeling.
function RobustMean(
   const Data           :  TRealArray
         )              :  Real;
   var
         N, NOk, i      :  Integer;
         QuarterZ, MaxZ,
         Med, Sigma,
         MinX, MaxX,
         Sum            :  Real;
   begin
   N := High(Data) - Low(Data) + 1;
   Assert(N > 0, ErrorSmallArray);

   // Determine the bounds for outliers using Chauvenet's criterion:
   // rejection rate for Gaussian distribution is 1 / 2N
   Med := Median(Data);
   QuarterZ := InvNormalCCDF(1 / 4);
       MaxZ := InvNormalCCDF(1 / (2 * N));
   Sigma := MedAbsDeviation(Data) / QuarterZ;
   MinX := Med - MaxZ * Sigma;
   MaxX := Med + MaxZ * Sigma;

   // Find the mean of data except outliers
   Sum := 0;
   NOk := 0;
   for i := Low(Data) to High(Data) do
      if (Data[i] >= MinX) and (Data[i] <= MaxX) then
         begin
         Sum := Sum + Data[i];
         Inc(NOk);
         end;
   Result := Sum / NOk;
   end;
   
   
// Mean absolute deviation = Mean(|A - Mean(A)|)
function MeanAbsDeviation(
   const A        :  TRealArray
         )        :  Real;
   var
         AbsDev   :  TRealArray;
         Mean     :  Real;
         i        :  Integer;
   begin
   AbsDev := Copy(A);
   Mean := Statistics.Mean(A);
   for i := 0 to Length(A) - 1 do
      AbsDev[i] := Abs(A[i] - Mean);
   Result := Statistics.Mean(AbsDev);
   end;
   
   
// Median absolute deviation = Median(|A - Median(A)|)
function MedAbsDeviation(
   const A        :  TRealArray
         )        :  Real;
   var
         AbsDev   :  TRealArray;
         M        :  Real;
         i        :  Integer;
   begin
   AbsDev := Copy(A);
   M := Median(A);
   for i := 0 to Length(A) - 1 do
      AbsDev[i] := Abs(A[i] - M);
   Result := Median(AbsDev);
   end;

{-----------------------<< Quantiles >>---------------------------------------------}

// Return a quantile of the first Len elements of A corresponding to probability P. 
// Uses interpolation between two closest values. Quickselect algorithm, 
// O(N) expected runtime.
// #UNOPT switch to insertion sort after a threshold?
function Quantile(
   const A     :  TRealArray;
         P     :  Real;       // [0 .. 1]
         Len   :  Integer
         )     :  Real;
         overload;

   // Subarray A[Left .. Right] is rearranged: A <= Pivot, Pivot, A >= Pivot.
   // Index of the pivot element is returned.
   function Partition(
      var   A              :  TRealArray;
            Left, Right    :  Integer
            )              :  Integer;
      var
            PivotValue     :  Real;
            PivotIndex,
            IndexL, IndexR :  Integer;
      begin
      PivotIndex := Left + Random(Right - Left + 1);
      PivotValue := A[PivotIndex];
      Swap(A, Left, PivotIndex);
      IndexL := Left;
      IndexR := Right + 1;
      while True do
         begin
         repeat
            Inc(IndexL);
         until (IndexL > Right) or (A[IndexL] >= PivotValue);
         repeat
            Dec(IndexR);
         until A[IndexR] <= PivotValue;
         if IndexL >= IndexR then
      {*}   break;
         Swap(A, IndexL, IndexR);
         end;
      Swap(A, Left, IndexR);
      Result := IndexR;
      end;

   var
         Work           :  TRealArray;
         Index,
         PivotIndex,
         Left, Right    :  Integer;
         RealIndex,
         X, Y, Alpha    :  Real;
   begin
   Assert( (P >=0) and (P <= 1) , 'Invalid quantile probability');

   // Copy into working array
   Work := Copy(A);
   Right := Len - 1;
   Left  := 0;
   Assert(Len > 0, ErrorSmallArray);

   // Set percentile index and blending factor
   RealIndex := Clip(P * Len - 1 / 2, 0, Len - 1);
   Index := Floor(RealIndex);
   Alpha := Frac (RealIndex);

   // Find the quantile
   repeat
      PivotIndex := Partition(Work, Left, Right);
      if RealIndex < PivotIndex then
         Right := PivotIndex - 1
      else
         Left  := PivotIndex + 1;
   until Left >= Right;

   // Interpolate
   X := Work[Index];
   Y := Work[Min(Index + 1, Len - 1)];
   Result := (1 - Alpha) * X + Alpha * Y;
   end;


// Return a quantile corresponding to probability P. Uses interpolation between two
// closest values. Quickselect algorithm, O(N) expected runtime.
function Quantile(
   const A     :  TRealArray;
         P     :  Real        // [0 .. 1]
         )     :  Real;
         overload;
   begin
   Result := Quantile(A, P, Length(A));
   end;


function Quantile(
   const A     :  TRealArrayN;
         P     :  Real        // [0 .. 1]
         )     :  Real;
         overload;
   begin
   Result := Quantile(A._, P, A.N);
   end;


function Quantile(
   const A     :  TIntArray;
         P     :  Real        // [0 .. 1]
         )     :  Real;
         overload;
   var
         RealA :  TRealArray;
   begin
   ConvertToReal(RealA, A);
   Result := Quantile(RealA, P, Length(A));
   end;


function Quantile(
   const A     :  TIntArrayN;
         P     :  Real        // [0 .. 1]
         )     :  Real;
         overload;
   var
         RealA :  TRealArrayN;
   begin
   ConvertToReal(RealA, A);
   Result := Quantile(RealA._, P, A.N);
   end;


// Median of A, O(N) expected runtime
function Median(
   const A  :  TRealArray
         )  :  Real;
         overload;
   begin
   Result := Quantile(A, 1 / 2);
   end;


function Median(
   const A  :  TRealArrayN
         )  :  Real;
         overload;
   begin
   Result := Quantile(A, 1 / 2);
   end;


function Median(
   const A  :  TIntArray
         )  :  Real;
         overload;
   begin
   Result := Quantile(A, 1 / 2);
   end;


function Median(
   const A  :  TIntArrayN
         )  :  Real;
         overload;
   begin
   Result := Quantile(A, 1 / 2);
   end;


// Interquartile range
function Midspread(
   const A  :  TRealArray
         )  :  Real;
         overload;
   begin
   Result := Quantile(A, 3 / 4) - Quantile(A, 1 / 4);
   end;


function Midspread(
   const A  :  TIntArray
         )  :  Real;
         overload;
   var
         B  :  TRealArray;
   begin
   ConvertToReal(B, A);
   Result := Midspread(B);
   end;

{-----------------------<< Min, Max >>----------------------------------------------}

// Minimal value of the elements of A
function Minimum(
   const A  :  array of Real
         )  :  Real;
         overload;
   var
         i  :  Integer;
   begin
   Result := A[0];
   for i := 1 to Length(A) - 1 do
      Result := Min(Result, A[i]);
   end;


function Minimum(
   const A  :  array of Integer
         )  :  Integer;
         overload;
   var
         i  :  Integer;
   begin
   Result := A[0];
   for i := 1 to Length(A) - 1 do
      Result := Min(Result, A[i]);
   end;


// Maximal value of the elements of A
function Maximum(
   const A  :  array of Real
         )  :  Real;
         overload;
   var
         i  :  Integer;
   begin
   Result := A[0];
   for i := 1 to Length(A) - 1 do
      Result := Max(Result, A[i]);
   end;


function Maximum(
   const A  :  array of Integer
         )  :  Integer;
         overload;
   var
         i  :  Integer;
   begin
   Result := A[0];
   for i := 1 to Length(A) - 1 do
      Result := Max(Result, A[i]);
   end;

{-----------------------<< Normalization >>-----------------------------------------}

// Normalize the Data according to the specified method
procedure Normalize(
   var   Data  :  TRealArray;
         Norm  :  TNormalization);
         overload;
   var
         i     :  Integer;
         B, C  :  Real;
   begin
   // Calculate normalization coefficients
   B := 0;
   C := 1;
   case Norm of
      normUnitSum:
         C := Sum(Data);
      normDivMax:
         C := Maximum(Data);
      normUnit:
         begin
         B := Minimum(Data);
         C := Maximum(Data) - B;
         end;
      normNormal:
         GetMeanStandDev(B, C, Data);
      normZeroMean:
         B := Mean(Data);
      else
         Assert(False, 'Unknown normalization');
      end;

   // Normalize
   if C  = 0 then
      C := 1;
   for i := 0 to Length(Data) - 1 do
      Data[i] := (Data[i] - B) / C;
   end;


procedure Normalize(
   var   NormData :  TRealArray;
   const Data     :  TRealArray;
         Norm     :  TNormalization);
         overload;
   begin
   NormData := Copy(Data);
   Normalize(NormData, Norm);
   end;


// Perform ranking transformation. Rank of equal items is the mean of their integer
// ranks. Invert changes the sorting direction, Normalized gives [0 .. 1) range.
procedure RankTransform(
   var   Ranks       :  TRealArray;
   const Data        :  TRealArray;
         Invert,
         Normalized  :  Boolean);
         overload;
   var
         i, j, k,
         Len, Sum    :  Integer;
         Order       :  TIntArray;
         SortedRanks :  TRealArray;
         SortOrder   :  TSortOrder;
         MeanRank,
         NormFac     :  Real;
   begin
   // Calculate integer rankings
   Len := Length(Data);
   if Invert then
      SortOrder := soDescending else
      SortOrder := soAscending;
   OrderRealArray(Order, Data, SortOrder);

   // Calculate sorted fractional rankings
   SetLength(SortedRanks, Len);
   i := 0;
   j := 0;
   repeat
      // Calculate sum of integer ranks of equal items
      Sum := 0;
      while (j < Len) and (Data[Order[i]] = Data[Order[j]]) do
         begin
         Sum := Sum + j;
         Inc(j);
         end;

      // Fill ranks of equal items with their mean integer rank
      MeanRank := Sum / (j - i);
      for k := i to j - 1 do
         SortedRanks[k] := MeanRank;
      i := j;
   until i = Len;

   // Determine ranking of each item
   NormFac := 1;
   if Normalized then
      NormFac := 1 / Len;
   SetLength(Ranks, Len);
   for i := 0 to Len - 1 do
      Ranks[Order[i]] := NormFac * SortedRanks[i];
   end;


// Perform ranking transformation. Rank of equal items is the mean of their integer
// ranks. Invert changes the sorting direction, Normalize gives [0 .. 1) range.
procedure RankTransform(
   var   Ranks       :  TRealArray;
   const Data        :   TIntArray;
         Invert,
         Normalized  :  Boolean);
         overload;
   var
         RealData    :  TRealArray;
   begin
   ConvertToReal(RealData, Data);
   RankTransform(Ranks, RealData, Invert, Normalized);
   end;

{-----------------------<< Correlation >>-------------------------------------------}

// Pearson correlation between X and Y
function Corr(
   const X, Y     :  TRealArray
         )        :  Real;
   var
         Len      :  Integer;
         SDX, SDY :  Real;
         NX, NY   :  TRealArray;
   begin
   Len := Length(X);
   Assert(Length(X) = Length(Y));
   if Len < 2 then
      Result := 0
   else
      begin
      Normalize(NX, X, normZeroMean);
      Normalize(NY, Y, normZeroMean);
      SDX := StandDev(X);
      SDY := StandDev(Y);
      Result := DotProduct(NX, NY) / (SDX * SDY * (Len - 1));
      end;
   end;


// Spearman's rank correlation between X and Y
function RankCorr(
   const X, Y     :  TRealArray
         )        :  Real;
   var
         RX, RY   :  TRealArray;
   begin
   // Alternatively, Spearman's rank correlation can be computed as
   // 1 - 6 * Sum dR_i ^ 2 / (n * (n ^ 2 - 1))
   // However, that formula is only approximate when there are tied ranks
   RankTransform(RX, X, {Invert:} False, {Normalized:} False);
   RankTransform(RY, Y, {Invert:} False, {Normalized:} False);
   Result := Corr(RX, RY);
   end;

{-----------------------<< Error measures >>----------------------------------------}
const
      ErrDiffLen = 'Vectors must have the same length';

// Return an error measure between two datasets. Relative error measures are 
// assymetric, X1 stands for the actual and X2 for predicted values.
function ErrorMeasure(
   const X1, X2   :  TRealArray;
         Measure  :  TErrorMeasure
         )        :  Real;
   var
         Delta    :  TRealArray;
         i, Len   :  Integer;
   const
         ErrUnknownMeasure = 'Unknown error measure';
   begin
   // Initialize
   Assert(Length(X1) = Length(X2), ErrDiffLen);
   Len := Length(X1);
   SetLength(Delta, Len);

   // Calculate error terms
   for i := 0 to Length(X1) - 1 do
      case Measure of
         emMAE, emMdAE:
            Delta[i] := Abs(X1[i] - X2[i]);
         emRMSE:
            Delta[i] := Sqr(X1[i] - X2[i]);            
         emMARE, emMdARE:
            Delta[i] := Abs( (X1[i] - X2[i]) / X1[i] );
         emRMSRE:
            Delta[i] := Sqr( (X1[i] - X2[i]) / X1[i] );
         else
            Assert(False, ErrUnknownMeasure);
         end;

   // Get desired statistics
   case Measure of
      emRMSE, emRMSRE:
         Result := Sqrt( Mean(Delta) );
      emMAE, emMARE:
         Result := Mean(Delta);
      emMdAE, emMdARE:
         Result := Median(Delta);
      else
         Assert(False, ErrUnknownMeasure);
         Result := 0;
      end;
   end;

end.
