{ 
Copyright (c) Peter Karpov 2010 - 2017.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit RandVars; //////////////////////////////////////////////////////////////////////
{
>> Version: 1.9

>> Description
   Unit for generation of random variables with different distributions and different
   kinds of noise. Part of InvLibs unit collection.
   
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> References
    - Peter Karpov.
      Generating generalized Gaussian, Cauchy and related random variables.
      http://inversed.ru/InvMem.htm#InvMem_5
    - Robin Whittle.
      DSP generation of Pink (1/f) Noise.
      http://www.firstpr.com.au/dsp/pink-noise/

>> ToDo
    - Test all the routines

>> Changelog
   1.9 : 2017.12.04  + Permutations section, RandPerm and ShuffleArray procedures
                     + RandBool function
                     ~ RandBipolar renamed to RandSign
                     ~ DiffRand renamed to RandOther
                     ~ TwoDiffRand renamed to RandPair
                     ~ Comments cleanup
                     ~ Freepascal compatibility
   1.8 : 2014.09.28  + RandElement function
   1.7 : 2012.03.12  + DiffRand function
   1.6 : 2012.01.12  + RandUniform function
   1.5 : 2011.10.14  ~ Moved to generic types
                     + TwoDiffRand procedure
   1.3 : 2011.07.19  + RandRange function
   1.2 : 2011.05.14  + 'Geometric' section
   1.1 : 2011.04.27  ~ 'Noise' section rewritten
   1.0 : 2011.04.10  + 'Continous' and 'Noise' sections
   0.0 : 2011.01.30  + Initial version
   Notation: + added, - removed, * fixed, ~ changed
}
interface ///////////////////////////////////////////////////////////////////////////
uses
      Arrays;

{-----------------------<< Selection >>---------------------------------------------}

// Return a or b at random
function SelectRandReal(
         a, b  :  Real
         )     :  Real;

function SelectRandInt(
         a, b  :  Integer
         )     :  Integer;

function SelectRandBool(
         a, b  :  Boolean
         )     :  Boolean;
         
// Return a random element from A
function RandElement(
   const A  :  TIntArray
         )  :  Integer;

{-----------------------<< Discrete >>----------------------------------------------}
     
// Random Boolean
function RandBool : Boolean;

// Return +1 or -1 at random
function RandSign : Integer;

// Random variable uniformly distributed on the interval [Low .. High]
function RandRange(
         Low, High   :  Integer
         )           :  Integer;

// Random variable uniformly distributed on the interval [0 .. Range) and
// distinct from A. If Range is less than 2, 0 is returned.
function RandOther(
         A, Range :  Integer
         )        :  Integer;         

// Return two distinct uniform random variables 0 <= A, B < Range. 
// If range is less than 2, A and B are set to 0.
procedure RandPair(
   var   A, B  :  Integer;
         Range :  Integer);

// Random geometrically distributed variable. PMF is proportional to p^i, i >= 0.
function RandGeometric(
         p  :  Real     // (0 .. 1)
         )  :  Integer; // >= 0

// Random variable from a discrete triangular distribution 
// on the interval [Low .. High]
function RandDiscreteTriangle(
         Low, High   :  Integer
         )           :  Integer;
         
// Random discrete variable with PMF given by Distr
function RandDiscreteVar(
   const Distr       :  array of Real
         )           :  Integer;

{-----------------------<< Continous >>---------------------------------------------}

// Random variable uniformly distributed between A and B
function RandUniform(
         A, B  :  Real
         )     :  Real;

// Gaussian random variable with mean = 0, SD = 1.
// Uses polar form of Box-Muller transform.
function RandGauss   :  Real;

// Random variable from the Laplace distribution 
// with mean = 0, scale = 1, SD = Sqrt(2)
function RandLaplace :  Real;

// Exponential random variable with mean = SD = 1
function RandExp  :  Real;

// Random variable from a generalized normal distribution, 
// PDF(x) ~ Exp(-|x|^P)
function RandGenNormal(
         P  :  Real  =  2
         )  :  Real;

// Random variable from a modified generalized normal distribution, 
// PDF(x) ~ x^P * Exp(-x^P), x > 0
function RandModNormal(
         P  :  Real
         )  :  Real;

// Random variable from a generalized Cauchy distribution, 
// PDF(x) ~ 1 / (1 + |x|^P)
function RandGenCauchy(
         P  :  Real  = 2
         )  :  Real;

// Random variable from a modified generalized Cauchy distribution, 
// PDF(x) ~ x^P / (1 + x^P)^2, x > 0
function RandModCauchy(
         P  :  Real
         )  :  Real;

{-----------------------<< Geometric >>---------------------------------------------}

// Generate a point (x, y) uniformly distributed on the circumference of a 
// unit circle
procedure RandCircle(
   var   x, y     :  Real);

// Generate a point (x, y) uniformly distributed inside of a unit disc
procedure RandDisc(
   var   x, y     :  Real);

// Generate a point (x, y, z) uniformly distributed on the surface of a 
// unit sphere
procedure RandSphere(
   var   x, y, z  :  Real);
   
{-----------------------<< Permutations >>------------------------------------------}       
      
// Randomly permute A
procedure ShuffleArray(
   var   A        :  array of Integer);
   
// Return a random permutation of N sequential integers starting from Base
procedure RandPerm(
   var   A        :  TIntArray;
         N, Base  :  Integer);
         overload;

procedure RandPerm(
   var   A        :  array of Integer;
         Base     :  Integer);
         overload;         

{-----------------------<< Noise >>-------------------------------------------------}
const
      NPink = 7;
type
      TPinkFilter = array [1 .. NPink] of Real;

      TPinkNoise  = TPinkFilter;

      TRedNoise   = Real;

// Initialize the red noise generator
procedure InitRedNoise(
   var   RedNoise   :  TRedNoise);
   
// Generate one sample of the red noise aka Brown(ian) noise, 
// power density ~ 1 / f^2
function GetRedNoise(
   var   Red   :  TRedNoise
         )     :  Real;

// Initialize the pink noise generator
procedure InitPinkNoise(
   var   PinkNoise   :  TPinkNoise);

// Generate one sample of the pink noise, power density ~ 1 / f. 
// Paul Kellet's filtering method is used.
function GetPinkNoise(
   var   Pink  :  TPinkNoise
         )     :  Real;

implementation //////////////////////////////////////////////////////////////////////
uses
      Math;

{-----------------------<< Selection >>---------------------------------------------}

// Return a or b at random
function SelectRandReal(
         a, b  :  Real
         )     :  Real;
   begin
   if RandBool then
      Result := a else
      Result := b;
   end;


function SelectRandInt(
         a, b  :  Integer
         )     :  Integer;
   begin
   if RandBool then
      Result := a else
      Result := b;
   end;


function SelectRandBool(
         a, b  :  Boolean
         )     :  Boolean;
   begin
   if RandBool then
      Result := a else
      Result := b;
   end;
   
   
// Return a random element from A
function RandElement(
   const A  :  TIntArray
         )  :  Integer;
   begin
   Result := A[Random(Length(A))];
   end;

{-----------------------<< Discrete >>----------------------------------------------}

// Random Boolean
function RandBool : Boolean;
   begin
   Result := Random(2) = 0;
   end;
   
   
// Return +1 or -1 at random
function RandSign : Integer;
   begin
   Result := 2 * Random(2) - 1;
   end;


// Random variable uniformly distributed on the interval [Low .. High]
function RandRange(
         Low, High   :  Integer
         )           :  Integer;
   begin
   Result := Low + Random(High - Low + 1);
   end;


// Random variable uniformly distributed on the interval [0 .. Range) and
// distinct from A. If Range is less than 2, 0 is returned.
function RandOther(
         A, Range :  Integer
         )        :  Integer;
   begin
   if Range > 1 then
      repeat
         Result := Random(Range);
      until Result <> A
   else
      Result := 0;
   end;


// Return two distinct uniform random variables 0 <= A, B < Range. 
// If range is less than 2, A and B are set to 0.
procedure RandPair(
   var   A, B  :  Integer;
         Range :  Integer);
   begin
   if Range > 1 then
      repeat
         A := Random(Range);
         B := Random(Range);
      until A <> B
   else
      begin
      A := 0;
      B := 0;
      end;
   end;


// Random geometrically distributed variable. PMF is proportional to p^i, i >= 0.
function RandGeometric(
         p  :  Real     // (0 .. 1)
         )  :  Integer; // >= 0
   begin
   Result := Floor( Ln(1 - Random) / Ln(1 - p) );
   end;


// Random variable from a discrete triangular distribution 
// on the interval [Low .. High]
function RandDiscreteTriangle(
         Low, High   :  Integer
         )           :  Integer;
   var
         Range, Half :  Integer;
   begin
   Assert(High >= Low);
   Range := High - Low + 1;
   Half := (Range + 1) div 2;
   if (Range and 1) <> 0 then
      Result := Low + Random(Half) + Random(Half) else
      Result := Low + Random(Half) + Random(Half + 1);
   end;
   
   
// Random discrete variable with PMF given by Distr
function RandDiscreteVar(
   const Distr       :  array of Real
         )           :  Integer;
   var
         i,
         IndexFail   :  Integer;
         Sum, Norm,
         Rand        :  Real;
   begin
   // Calculate normalization factor
   Sum := 0;
   for i := Low(Distr) to High(Distr) do
      begin
      Assert(Distr[i] >= 0, 'Negative probability');
      Sum := Sum + Distr[i];
      end;
   Norm := 1 / Sum;
   
   // Table lookup
   IndexFail := High(Distr) + 1;
   Result := IndexFail;
   repeat
      Rand := Random;
      for i := Low(Distr) to High(Distr) do
         begin
         Rand := Rand - Norm * Distr[i];
         if Rand < 0 then
            begin
            Result := i;
            break;
            end;
         end;
   until Result <> IndexFail;
   end;

{-----------------------<< Continous >>---------------------------------------------}

// Random variable uniformly distributed between A and B
function RandUniform(
         A, B  :  Real
         )     :  Real;
   begin
   Result := A + Random * (B - A);
   end;


// Gaussian random variable with mean = 0, SD = 1.
// Uses polar form of Box-Muller transform.
function RandGauss   :  Real;
   var
         x, rr       :  Real;
   begin
   repeat
      x := Random;
      rr := x * x + Sqr(Random);
   until (rr <= 1) and (rr <> 0);
   Result := RandSign * x * Sqrt(-2 * Ln(rr) / rr);
   end;


// Random variable from the Laplace distribution 
// with mean = 0, scale = 1, SD = Sqrt(2)
function RandLaplace :  Real;
   begin
   Result := RandSign * Ln(1 - Random);
   end;


// Exponential random variable with mean = SD = 1
function RandExp  :  Real;
   begin
   Result := -Ln(1 - Random);
   end;


// Random variable from a generalized normal distribution, 
// PDF(x) ~ Exp(-|x|^P)
function RandGenNormal(
         P  :  Real  =  2
         )  :  Real;

   function g(
            x, x1,
            P, A  :  Real
            )     :  Real;
      begin
      if x < x1 then
         Result := 1
      else
         Result := A * Power( x, -(P + 1) );
      end;

   var
         x, x1, r,
         A, Q, p1 :  Real;
   begin
   // Calculate the constants
   Q := P + 1;
   A := Exp( (Q / P) * ( Ln(Q / P) - 1 ) );
   x1 := Power( A, 1 / Q );
   p1 := x1 / ( x1 + ( A / P ) * Power(x1, -P) );

   // Rejection sampling
   repeat
      r := Random;
      if r < p1 then
         x := x1 * Random
      else
         x := x1 * Power( Random, -1 / P );
   until Random < ( Exp( -Power(x, P) ) / g(x, x1, P, A) );
   Result := RandSign * x;
   end;


// Random variable from a modified generalized normal distribution, 
// PDF(x) ~ x^P * Exp(-x^P), x > 0
function RandModNormal(
         P  :  Real
         )  :  Real;
   const
         C1 = 0.75;
         C2 = 2.14;
   var
         x, x1, x2,
         r, A, Q, tp,
         S1, S2, S3,
         SAll, p1, p2,
         em1, ep1       :  Real;

   function g(
            x, x1, x2,
            P, A, Q     :  Real
            )           :  Real;
      begin
      if x < x1 then
         Result := Power(x, P)
      else if x < x2 then
         Result := em1
      else
         Result := A * Power(x, -Q);
      end;

   begin
   // Calculate the constants
   em1 := Exp(-1);
   ep1 := Exp( 1);
   Q := C1 + C2 * P;
   A := Exp( (1 + Q / P) * (Ln(1 + Q / P) - 1) );
   x1 := Exp(-1 / P);
   x2 := Power(A * ep1, 1 / Q);
   S1 := Power(x1, P + 1) / (P + 1);
   S2 := em1 * (x2 - x1);
   S3 := A * Power(x2, 1 - Q) / (Q - 1);
   SAll := S1 + S2 + S3;
   p1 := S1 / SAll;
   p2 := S2 / SAll;

   // Rejection sampling
   repeat
      r := Random;
      if r < p1 then
         x := x1 * Power( Random, 1 / (P + 1) )
      else if r < (p1 + p2) then
         x := x1 + Random * (x2 - x1)
      else
         x := x2 * Power( Random, 1 / (1 - Q) );
      tp := Power(x, P);
   until Random < ( tp * Exp(-tp) / g(x, x1, x2, P, A, Q) );
   Result := x;
   end;


// Random variable from a generalized Cauchy distribution, 
// PDF(x) ~ 1 / (1 + |x|^P)
function RandGenCauchy(
         P  :  Real  = 2
         )  :  Real;

   function g(
            x, P  :  Real
            )     :  Real;
      begin
      if x < 1 then
         Result := 1
      else
         Result := Power(x, -P);
      end;

   var
         x, r, S, Q  :  Real;
   begin
   Q := P - 1;
   S := 1 / ( 1 + ( 1 / Q ) );
   repeat
      r := Random;
      if r < S then
         x := Random
      else
         x := Power( Random, -1 / Q );
   until Random < ( ( 1 / ( 1 + Power(x, P) ) ) / g(x, P) );
   Result := RandSign * x;
   end;


// Random variable from a modified generalized Cauchy distribution, 
// PDF(x) ~ x^P / (1 + x^P)^2, x > 0
function RandModCauchy(
         P  :  Real
         )  :  Real;

   function g(
            x, x1, x2, P   :  Real
            )              :  Real;
      begin
      if x < x1 then
         Result := Power(x, P)
      else if x < x2 then
         Result := 1 / 4
      else
         Result := Power(x, -P);
      end;

   var
         x, x1, x2, r,
         S1, S2, S3,
         SAll, p1, p2,
         Q, T           :  Real;
   begin
   // Calculate the constants
   Q := P - 1;
   T := P + 1;
   x1 := Power(1 / 4,  1 / P);
   x2 := Power(1 / 4, -1 / P);
   S1 := Power(4, -T / P) / T;
   S2 := (1 / 4) * ( Power(4, 1 / P ) - Power(4, -1 / P) );
   S3 := Power(4, -Q / P ) / Q;
   SAll := S1 + S2 + S3;
   p1 := S1 / SAll;
   p2 := S2 / SAll;

   // Rejection sampling
   repeat
      r := Random;
      if r < p1 then
         x := x1 * Power( Random, 1 / T )
      else if r < (p1 + p2) then
         x := x1 + Random * (x2 - x1)
      else
         x := x2 * Power( Random, -1 / Q );
   until Random < ( ( Power(x, P) / Sqr( 1 + Power(x, P) ) ) / g(x, x1, x2, P) );
   Result := x;
   end;

{-----------------------<< Geometric >>---------------------------------------------}

// Generate a point (x, y) uniformly distributed on the circumference of a 
// unit circle
procedure RandCircle(
   var   x, y     :  Real);
   var
         a, b, c  :  Real;
   begin
   repeat
      a := 2 * Random - 1;
      b := 2 * Random - 1;
      c := Sqr(a) + Sqr(b);
   until c < 1;
   x := ( Sqr(a) - Sqr(b) ) / c;
   y := 2 * a * b / c;
   end;


// Generate a point (x, y) uniformly distributed inside of a unit disc
procedure RandDisc(
   var   x, y     :  Real);
   begin
   repeat
      x := 2 * Random - 1;
      y := 2 * Random - 1;
   until ( Sqr(x) + Sqr(y) ) <= 1;
   end;


// Generate a point (x, y, z) uniformly distributed on the surface of a
// unit sphere
procedure RandSphere(
   var   x, y, z  :  Real);
   var
         a, b, c  :  Real;
   begin
   repeat
      a := 2 * Random - 1;
      b := 2 * Random - 1;
      c := Sqr(a) + Sqr(b);
   until c < 1;
   x := 2 * a * Sqrt(1 - c);
   y := 2 * b * Sqrt(1 - c);
   z := 1 - 2 * c;
   end;
   
{-----------------------<< Permutations >>------------------------------------------}

// Randomly permute A
procedure ShuffleArray(
   var   A        :  array of Integer);
   var
         k, N     :  Integer;
         Temp     :  Integer;
   begin
   N := Length(A);
   while n > 1 do
      begin
      k := Random(N);
      Dec(N);
      Temp := A[N];
      A[N] := A[k];
      A[k] := Temp;
      end;
   end;


// Return a random permutation of N sequential integers starting from Base
procedure RandPerm(
   var   A        :  TIntArray;
         N, Base  :  Integer);
         overload;
   begin
   SequentialFill(A, N, Base);
   ShuffleArray(A);
   end;


procedure RandPerm(
   var   A        :  array of Integer;
         Base     :  Integer);
         overload;
   begin
   SequentialFill(A, Base);
   ShuffleArray(A);
   end;

{-----------------------<< Noise >>-------------------------------------------------}

// Initialize the red noise generator
procedure InitRedNoise(
   var   RedNoise   :  TRedNoise);
   begin
   RedNoise := 0;
   end;


// Generate one sample of the red noise aka Brown(ian) noise, 
// power density ~ 1 / f^2
function GetRedNoise(
   var   Red   :  TRedNoise
         )     :  Real;
   begin
   Red := Red + RandGauss;
   Result := Red;
   end;


// Initialize the pink noise generator
procedure InitPinkNoise(
   var   PinkNoise   :  TPinkNoise);
   var
         i           :  Integer;
   begin
   for i := 1 to NPink do
      PinkNoise[i] := 0;
   end;


// Generate one sample of the pink noise, power density ~ 1 / f. 
// Paul Kellet's filtering method is used.
function GetPinkNoise(
   var   Pink  :  TPinkNoise
         )     :  Real;
   var
         r, x  :  Real;
         i     :  Integer;
   const
         a     :  TPinkFilter =
                  (0.99886,   0.99332,    0.96900,    0.86650,    0.55000,   -0.7616,     1);
         c     :  TPinkFilter =
                  (0.0555179, 0.0750759,  0.1538520,  0.3104856,  0.5329522, -0.0168980,  0);
         LastC = 0.115926;
         White = 0.5362;
   begin
   r := RandGauss;
   x := White * r;
   for i := 1 to NPink do
      begin
      Pink[i] := a[i] * Pink[i] + c[i] * r;
      x := x + Pink[i];
      end;
   Pink[NPink] := LastC * r;
   Result := x;
   end;

end.
