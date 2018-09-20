{ 
Copyright (c) Peter Karpov 2010 - 2017.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit ExtraMath; /////////////////////////////////////////////////////////////////////
{
>> Version: 1.13

>> Description
   Various mathematical routines. Part of InvLibs unit collection.

>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> ToDo
    - Add Euler-Mascheroni constant and its exponentiated version
    ? Reciprocal versions of the constants (i or r prefix)
    ? Move machine epsilon constants to InvSys

>> References
   - Rosser, J. B., Schoenfeld, L.
     Approximate Formulas for Some Functions of Prime Numbers.

>> Changelog
   1.13   : 2017.12.02   ~ Renamed the unit to ExtraMath
                         ~ Renamed Blended to Blend and Blend to BlendTo
                         ~ Renamed SumDivisors to SumOfDivisors
                         - InRange function (available in Math unit)
                         - mXau (duplicate of mTau)
                         ~ FreePascal compatibility
   1.12   : 2015.12.11   + GetDigits procedure
   1.11   : 2015.06.20   * Bug in Modulo function for x = -M
   1.10   : 2015.04.16   + SignedPower function
   1.9    : 2015.03.09   + LCM function
                         + Prime numbers section
                         + CalcPrimes, CalcPrimesTo functions
                         + Factorize procedure
                         + PrimeCountUpper function
   1.8    : 2014.10.10   + Totient function
                         + SomDivisors function
                         + Modular arithmetics section
   1.7    : 2014.10.06   + Integer version of ClipCyclic
   1.6    : 2014.09.24   + ClipInt function to allow disambiguous calls
   1.5    : 2014.02.08   + RealModulo function
                         * Bug in Modulo function for x = 0, M < 0
                         * ClipCyclic function performance issue
   1.4    : 2013.10.21   + UnitStep function
                         + Median of 3 function
   1.3    : 2013.06.13   ~ New constant naming convention
   1.2    : 2013.04.29   + Digit function
                         + InRange function
   1.1    : 2013.02.06   + Geometric and logarithmic means
   1.0    : 2013.02.03   ~ Reorganized into sections
                         + (Multi)Factorial functions
                         + Binomial coefficients
                         + DivideByGCD procedure
                         ~ Renamed Min3 function to Min
   0.13   : 2012.10.03   + NPairs function
   0.10   : 2012.03.18   + Blending
   Notation: + added, - removed, * fixed, ~ changed                                 
}
interface ///////////////////////////////////////////////////////////////////////////
uses
      Arrays;
const
      // Max relative rounding error aka machine epsilon for Single and Double types
      SinglePrecision   =  5.96e-8;
      DoublePrecision   =  1.11e-16;
        RealPrecision   =  DoublePrecision;
var
      mE, mGR, mTau     :  Real;

{-----------------------<< Integer sequences >>-------------------------------------}

// Factorial of n
function Fac(
         n  :  Integer
         )  :  Integer;

// Double factorial of n
function DoubleFac(
         n  :  Integer
         )  :  Integer;

// k-th multifactorial of n
function MultiFac(
         n, k  :  Integer
         )     :  Integer;

// The number of distinct pairs that can be selected from N objects
function NPairs(
         N  :  Integer
         )  :  Integer;

// Return i-th Fibonacci number. Fibonacci(0) = 0. Works for negative indices.
function Fibonacci(
         i  :  Integer
         )  :  Integer;

// Binomial coefficient C(n, k)
function Binomial(
      n, k  :  Integer
      )     :  Integer;

{-----------------------<< Prime numbers >>-----------------------------------------}

// Calculate first N Primes
procedure CalcPrimes(
   var   Primes   :  TIntArray;
         N        :  Integer);

// Calculate first Primes up to MaxValue
procedure CalcPrimesTo(
   var   Primes   :  TIntArray;
         MaxValue :  Integer);

// Return a factorization of N into primes 
procedure Factorize(
   var   PrimePowers :  TIntArray;
         N           :  Integer);

// Return an upper bound for the prime counting function giving number of primes <= N
function PrimeCountUpper(
         N     :  Integer
         )     :  Integer;

{-----------------------<< Integer Arithmetic >>------------------------------------}

// Return whether a is divisible by b or false in case of b = 0
function Divisible(
         a, b  :  Integer
         )     :  Boolean;

// Greatest common divisor of a and b
function GCD(
         a, b  :  Integer
         )     :  Integer;

// Least common multiple of a and b
function LCM(
         a, b  :  Integer
         )     :  Integer;

// Divide a and b by their greatest common divisor
procedure DivideByGCD(
   var   a, b  :  Integer);

// Euler's totient function: number of positive integers up to N coprime to it
function Totient(
         N     :  Integer
         )     :  Integer;

// Sum of p-th powers of divisors d of N, 1 < d < N
function SumOfDivisors(
         N        :  Integer;
         p        :  Integer
         )        :  Integer;

// Return i-th digit of X in a given Base
function Digit(
         x, i, Base  :  Integer;
         Balanced    :  Boolean
         )           :  Integer;

// Return the Digits of X in a given Base
procedure GetDigits(
   var   Digits      :  TIntArray;
         x, Base     :  Integer;
         Balanced    :  Boolean);

{-----------------------<< Modular Arithmetic >>------------------------------------}

// Return X modulo M. Result has the same sign as M.
function Modulo(
         x, M  :  Integer
         )     :  Integer;

// Return the minimal distance between a and b in a circular space of size M
function ModuloDistance(
         a, b, M  :  Integer
         )        :  Integer;

// Return X modulo M. Result has the same sign as M.
function RealModulo(
         x, M  :  Real
         )     :  Real;

{-----------------------<< Interpolation >>-----------------------------------------}

// Linearly interpolate from the Old (Alpha = 0) towards the New value (Alpha = 1)
procedure BlendTo(
   var   Old      :  Real;
         New      :  Real;
         Alpha    :  Real);

// Linearly interpolate between A (Alpha = 0) and B (Alpha = 1)
function Blend(
         A, B, Alpha :  Real
         )           :  Real;

// Linearly interpolate between A (Alpha = 0) and B (Alpha = 1) in logarithmic scale
function LogBlend(
         A, B, Alpha :  Real
         )           :  Real;

// Geometric mean of A and B
function GeoMean(
         A, B  :  Real
         )     :  Real;

// Logarithmic mean of A and B
function LogMean(
         A, B  :  Real
         )     :  Real;                

{-----------------------<< Safe Functions >>----------------------------------------}

// Return Sqrt(x) or 0 if x < 0
function SafeSqrt(
         x  :  Real
         )  :  Real;

// Return Exp(x) or maximal real value on overflow
function SafeExp(
         x  :  Real
         )  :  Real;

// Return Ln(x) or the Default value if x <= 0
function SafeLn(
         x,
         Default  :  Real
         )        :  Real;

// Return a / b or the Default value if b = 0         
function SafeDiv(
         a, b, Default  :  Real
         )              :  Real;

{-----------------------<< Clipping >>----------------------------------------------}

// Clip x into [xMin, xMax] range
function Clip(
         x, xMin, xMax  :  Real
         )              :  Real;
         overload;

function Clip(
         x, xMin, xMax  :  Integer
         )              :  Integer;
         overload;

// Clip x into [xMin, xMax] range
function ClipInt(
         x, xMin, xMax  :  Integer
         )              :  Integer;

// Clip x into [MinX, MaxX] cyclic range
function ClipCyclic(
         x, MinX, MaxX  :  Real
         )              :  Real;
         overload;

function ClipCyclic(
         x, MinX, MaxX  :  Integer
         )              :  Integer;
         overload;

// Minimum of (a, b, c)
function Min(
         a, b, c  :  Integer
         )        :  Integer;
         overload;

// Maximum of (a, b, c)
function Max(
         a, b, c  :  Integer
         )        :  Integer;
         overload;

// Median of (a, b, c)
function Median(
         a, b, c  :  Integer
         )        :  Integer;              

{-----------------------<< Misc real functions >>-----------------------------------}

// Sign-preserving power, returns Sign(x) * |x| ^ p
function SignedPower(
         x, p  :  Real
         )     :  Real;

// Cube root of x
function CubeRt(
         x  :  Real
         )  :  Real;

// Return 0, 1/2, 1 when x <, =, > 0 respectively
function UnitStep(
      x     :  Real
      )     :  Real;         

implementation //////////////////////////////////////////////////////////////////////
uses
      Math; // Min

{-----------------------<< Integer sequences >>-------------------------------------}

// k-th multifactorial of n
function MultiFac(
         n, k  :  Integer
         )     :  Integer;
   var
         i     :  Integer;
   begin
   i := n;
   n := 1;
   while i > 1 do
      begin
      n := n * i;
      i := i - k;
      end;
   Result := n;
   end;


// Factorial of n
function Fac(
         n  :  Integer
         )  :  Integer;
   begin
   Result := MultiFac(n, 1);
   end;


// Double factorial of n
function DoubleFac(
         n  :  Integer
         )  :  Integer;
   begin
   Result := MultiFac(n, 2);
   end;


// The number of distinct pairs that can be selected from N objects
function NPairs(
         N  :  Integer
         )  :  Integer;
   begin
   Result := (N * (N - 1)) div 2;
   end;


// Return i-th Fibonacci number. Fibonacci(0) = 0. Works for negative indices.
function Fibonacci(
         i  :  Integer
         )  :  Integer;
   begin
   if i < 0 then
      Result := (2 * (-i mod 2) - 1) * Fibonacci(-i)
   else
      Result := Round(IntPower(mGR, i) / Sqrt(5));
   end;


// Binomial coefficient C(n, k)
function Binomial(
      n, k     :  Integer
      )        :  Integer;
   var
      i, g, r  :  Integer;
   begin
   k := Min(k, n - k);
   if k < 0 then
      r := 0
   else
      begin
      r := 1;
      for i := 1 to k do
         begin
         g := GCD(r, i);
         r := (r div g) * (n - k + i) div (i div g);
         end;
      end;
   Result := r;
   end;

{-----------------------<< Prime numbers >>-----------------------------------------}

// Calculate Primes until MaxValue is reached or N numbers are found
procedure CalcPrimeNumbers(
   var   Primes      :  TIntArray;
         N, MaxValue :  Integer);
   var
         m, i, j     :  Integer;
         IsPrime     :  Boolean;
   begin
   SetLength(Primes, 3);
   Primes[0] := 2;
   Primes[1] := 3;
   Primes[2] := 5;
   i := 3;
   m := 7;
   repeat
      // Check primality
      IsPrime := True;
      j := 1;
      repeat
         if (m mod Primes[j]) = 0 then
            begin
            IsPrime := False;
      {<---}break;
            end;
         Inc(j);
      until Sqr(Primes[j]) > m;

      // Add to the list if prime
      if IsPrime then
         begin
         if i > (Length(Primes) - 1) then
            SetLength(Primes, 2 * i);
         Primes[i] := m;
         Inc(i);
         end;
      Inc(m, 2);
   until (i = N) or (m > MaxValue);
   SetLength(Primes, i);
   end;


// Calculate first N Primes
procedure CalcPrimes(
   var   Primes      :  TIntArray;
         N           :  Integer);
   begin
   CalcPrimeNumbers(Primes, N, {MaxValue:} High(Integer));
   end;


// Calculate first Primes up to MaxValue
procedure CalcPrimesTo(
   var   Primes      :  TIntArray;
         MaxValue    :  Integer);
   begin
   CalcPrimeNumbers(Primes, {N:} 0, MaxValue);
   end;


// Return an upper bound for the prime counting function giving number of primes <= N
function PrimeCountUpper(
         N     :  Integer
         )     :  Integer;
   begin
   Result := Floor( (N / Ln(N)) * ( 1 + 3 / (2 * Ln(N)) ) );
   end;


// Return a factorization of N into primes
procedure Factorize(
   var   PrimePowers :  TIntArray;
         N           :  Integer);
   var
          Primes     :  TIntArray;
         NPrimes, i  :  Integer;
   begin
   CalcPrimesTo(Primes, N);
   NPrimes := Length(Primes);
   InitArray(PrimePowers, NPrimes, 0);
   for i := 0 to NPrimes - 1 do
      while (N mod Primes[i]) = 0 do
         begin
         Inc(PrimePowers[i]);
         N := N div Primes[i];
         end;
   end;

{-----------------------<< Integer arithmetic >>------------------------------------}

// Return whether a is divisible by b or false in case of b = 0
function Divisible(
         a, b  :  Integer
         )     :  Boolean;
   begin
   Result := (b <> 0) and ((a mod b) = 0);
   end;


// Greatest common divisor of a and b
function GCD(
         a, b  :  Integer
         )     :  Integer;
   var
         t     :  Integer;
   begin
   while b <> 0 do
      begin
      t := b;
      b := a mod b;
      a := t;
      end;
   Result := a;
   end;


// Least common multiple of a and b
function LCM(
         a, b  :  Integer
         )     :  Integer;
   begin
   Result := (a div GCD(a, b)) * b;
   end;


// Divide a and b by their greatest common divisor
procedure DivideByGCD(
   var   a, b  :  Integer);
   var
         g     :  Integer;
   begin
   g := GCD(a, b);
   a := a div g;
   b := b div g;
   end;


// Euler's totient function: number of positive integers up to N coprime to it
function Totient(
         N     :  Integer
         )     :  Integer;
   var
         i     :  Integer;
   begin
   Result := 1;
   for i := 2 to N - 1 do
      if GCD(i, N) = 1 then
        Inc(Result);
   end;


// Sum of p-th powers of divisors d of N, 1 < d < N
function SumOfDivisors(
         N        :  Integer;
         p        :  Integer
         )        :  Integer;
   var
         i        :  Integer;
   begin
   Result := 0;
   for i := 2 to N - 1 do
      begin
      if (N mod i) = 0 then
         Result := Result + Round(IntPower(i, p));
      end;
   end;


// Return the least significant digit of X in a given Base, right shift X
function ExtractOneDigit(
   var   x        :  Integer;
         Base     :  Integer;
         Balanced :  Boolean
         )        :  Integer;
   var
         Shift    :  Integer;
   begin
   Shift := 0;
   if Balanced then
      begin
      Assert(Base mod 2 = 1, 'Invalid base');
      Shift := Base div 2;
      end;
   Result := Modulo(x + Shift, Base) - Shift;
   x := (x - Result) div Base;
   end;


// Return i-th digit of X in a given Base
function Digit(
         x, i, Base  :  Integer;
         Balanced    :  Boolean
         )           :  Integer;
   var
         j           :  Integer;
   begin
   Result := 0;
   for j := 0 to i do
      Result := ExtractOneDigit(x, Base, Balanced);
   end;


// Return the Digits of X in a given Base
procedure GetDigits(
   var   Digits      :  TIntArray;
         x, Base     :  Integer;
         Balanced    :  Boolean);
   var
         i           :  Integer;
   const
         MaxDigits   =  32;
   begin
   i := 0;
   SetLength(Digits, MaxDigits);
   while x <> 0 do
      begin
      Digits[i] := ExtractOneDigit(x, Base, Balanced);
      Inc(i);
      end;
   SetLength(Digits, i);
   end;

{-----------------------<< Modular Arithmetic >>------------------------------------}

// Return X modulo M. Result has the same sign as M.
function Modulo(
         x, M  :  Integer
         )     :  Integer;
   var
         r     :  Integer;
   begin
   r := x mod M;
   if ((x xor M) < 0) and (r <> 0) then
      r := r + M;
   Result := r;
   end;


// Return the minimal distance between a and b in a circular space of size M
function ModuloDistance(
         a, b, M  :  Integer
         )        :  Integer;
   begin
   Result := Min( Modulo(a - b, M), Modulo(b - a, M) );
   end;


// Return X modulo M. Result has the same sign as M.
function RealModulo(
         x, M  :  Real
         )     :  Real;
   var
         q     :  Integer;
   begin
   q := Floor(x / M);
   Result := x - q * M;
   end;

{-----------------------<< Interpolation >>-----------------------------------------}

// Linearly interpolate from the Old (Alpha = 0) towards the New value (Alpha = 1)
procedure BlendTo(
   var   Old      :  Real;
         New      :  Real;
         Alpha    :  Real);
   begin
   Old := Old + Alpha * (New - Old);
   end;


// Linearly interpolate between A (Alpha = 0) and B (Alpha = 1)
function Blend(
         A, B, Alpha :  Real
         )           :  Real;
   begin
   Result := A + Alpha * (B - A);
   end;


// Linearly interpolate between A (Alpha = 0) and B (Alpha = 1) in logarithmic scale
function LogBlend(
         A, B, Alpha :  Real
         )           :  Real;
   begin
   if A = 0 then
      Result := 0 else
      Result := A * Power(B / A, Alpha);
   end;


// Geometric mean of A and B
function GeoMean(
         A, B  :  Real
         )     :  Real;
   begin
   Result := Sqrt(A * B);
   end;


// Logarithmic mean of A and B
function LogMean(
         A, B  :  Real
         )     :  Real;
   begin
   if (A = 0) or (B = 0) then
      Result := 0
   else if A = B then
      Result := B
   else
      Result := (A - B) / Ln(A / B);
   end;

{-----------------------<< Safe functions >>----------------------------------------}

// Return Sqrt(x) or 0 if x < 0
function SafeSqrt(
         x  :  Real
         )  :  Real;
   begin
   if x < 0 then
      Result := 0 else
      Result := Sqrt(x);
   end;


// Return Ln(x) or the Default value if x <= 0
function SafeLn(
         x,
         Default  :  Real
         )        :  Real;
   begin
   if x > 0 then
      Result := Ln(x) else
      Result := Default;
   end;


// Return Exp(x) or maximal real value on overflow
// #HACK should return infinity on overflow?
function SafeExp(
         x  :  Real
         )  :  Real;
   begin
   if x > Ln(MaxDouble) then
      Result := MaxDouble else
      Result := Exp(x);
   end;


// Return a / b or the Default value if b = 0
function SafeDiv(
         a, b, Default  :  Real
         )              :  Real;
   begin
   if b = 0 then
      Result := Default else
      Result := a / b;
   end;

{-----------------------<< Clipping >>----------------------------------------------}

// Clip x into [xMin, xMax] range
function Clip(
         x, xMin, xMax  :  Real
         )              :  Real;
         overload;
   begin
   if      x < xMin then
      Result := xMin
   else if x > xMax then
      Result := xMax
   else
      Result := x;
   end;


// Clip x into [xMin, xMax] range
function ClipInt(
         x, xMin, xMax  :  Integer
         )              :  Integer;
   begin
   Result := Round(Clip(0.0 + x, 0.0 + xMin, 0.0 + xMax));
   end;


// Clip x into [xMin, xMax] range
function Clip(
         x, xMin, xMax  :  Integer
         )              :  Integer;
         overload;
   begin
   Result := ClipInt(x, xMin, xMax);
   end;


// Clip x into [MinX, MaxX] cyclic range
function ClipCyclic(
         x, MinX, MaxX  :  Real
         )              :  Real;
         overload;
   var
         Range          :  Real;
   begin
   Range := MaxX - MinX;
   Result := MinX + RealModulo(x - MinX, Range)
   end;


function ClipCyclic(
         x, MinX, MaxX  :  Integer
         )              :  Integer;
         overload;
   var
         Range          :  Integer;
   begin
   Range := MaxX - MinX + 1;
   Result := MinX + Modulo(x - MinX, Range)
   end;


// Minimum of (a, b, c)
function Min(
         a, b, c  :  Integer
         )        :  Integer;
         overload;
   begin
   Result := a;
   if b < Result then
      Result := b;
   if c < Result then
      Result := c;
   end;


// Maximum of (a, b, c)
function Max(
         a, b, c  :  Integer
         )        :  Integer;
         overload;
   begin
   Result := a;
   if b > Result then
      Result := b;
   if c > Result then
      Result := c;
   end;


// Median of (a, b, c)
function Median(
         a, b, c  :  Integer
         )        :  Integer;
   begin
   Result := a + b + c - Min(a, b, c) - Max(a, b, c);
   end;

{-----------------------<< Misc real functions >>-----------------------------------}

// Sign-preserving power, returns Sign(x) * |x| ^ p
function SignedPower(
         x, p  :  Real
         )     :  Real;
   begin
   Result := Sign(x) * Power(Abs(x), p);
   end;


// Cube root of x
function CubeRt(
         x  :  Real
         )  :  Real;
   begin
   Result := SignedPower(x, 1 / 3);
   end;


// Return 0, 1/2, 1 when x <, =, > 0 respectively
function UnitStep(
      x     :  Real
      )     :  Real;
   begin
   Result := (1 + Sign(x)) / 2;
   end;

initialization //////////////////////////////////////////////////////////////////////
mGR := (Sqrt(5) + 1) / 2;
mE := Exp(1);
mTau := 2 * Pi;
end.
