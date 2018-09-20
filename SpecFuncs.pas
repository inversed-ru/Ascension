{ 
Copyright (c) Peter Karpov 2010 - 2017.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit SpecFuncs; /////////////////////////////////////////////////////////////////////
{
>> Version: 0.7.1

>> Description
   Approximations of some special mathematical functions. Part of InvLibs unit 
   collection.
   
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> ToDo
    - Add lower branch of LambertW
    - Add more functions

>> References
   [Lambert W]
      On the Lambert W Function.
      R. M. Corless, G. H. Gonnet, D. E. G. Hare, D. J. Jeffrey, D. E. Knuth.
   [Winitzki]
      Uniform approximations for transcendental functions.
      Serge Winitzki.
   [Bagby]
      Calculating Normal Probabilities.
      Bagby, R. J.
   [NDF]
      Normal Distribution Function.
      http://mathworld.wolfram.com/NormalDistributionFunction.html
   [Nemes]
      New asymptotic expansion for the Gamma(z) function.
      Gergo Nemes.
   [Luschny]
      Approximation Formulas for the Factorial Function n!
      http://www.luschny.de/math/factorial/approx/SimpleCases.html
      Peter Luschny.

>> Changelog
   0.7.1 : 2017.11.25  ~ FreePascal compatibility
   0.7   : 2014.01.23  - Gamma function, use RealFac instead
                       - Error function, use normal integral instead
                       ~ Replaced InvErf with InvNormalInt
                       ~ Slightly improved Accuracy of InvNormalInt 
                       ~ Switched to new math constants convention
   0.6.1 : 2013.03.31  * LambertW(0)
   0.6   : 2013.03.18  + Normalized Sinc function
   0.5   : 2013.01.30  ~ Renamed RealFactorial to RealFac
   0.3   : 2012.04.26  + Real factorial and gamma functions
   0.2   : 2011.11.01  + Different forms of normal distribution integrals
                       ~ More precise approximation of Erf and related functions
   0.0   : 2011.07.29  + Initial version
   Notation: + added, - removed, * fixed, ~ changed
}
interface ///////////////////////////////////////////////////////////////////////////

{-----------------------<< Assorted functions >>------------------------------------}

// Upper branch of Lambert's W function aka product logarithm
function LambertW(
         x           :  Real  // [-1 / e .. +inf)
         )           :  Real;

// Normalized Sinc function
function Sinc(
         x  :  Real
         )  :  Real;

// Factorial of real x. Max relative error = 1.3e-8.
function RealFac(
         x     :  Real
         )     :  Real;

{-----------------------<< Normal CDF >>--------------------------------------------}
// Different forms of integrals of normal distribution. Accuracy of employed
// approximations is moderate, but usually should be sufficient.

// Integral [0, x] of standard normal distribution.
// Max absolute error = 3e-5, max relative error = 3e-4.
function NormalInt(
         x     :  Real
         )     :  Real;

// Inverse normal integral. Max relative error = 1.7e-3.
function InvNormalInt(
         x     :  Real
         )     :  Real;

// Normal cumulative distribution function =
// Integral (-inf, x] of standard normal distribution
function NormalCDF(
         x     :  Real
         )     :  Real;

// Normal complimentary cumulative distribution function =
// Integral [x, +inf) of standard normal distribution
function NormalCCDF(
         x     :  Real
         )     :  Real;

// Inverse normal cumulative distribution function
function InvNormalCDF(
         x     :  Real
         )     :  Real;

// Inverse normal complimentary cumulative distribution function
function InvNormalCCDF(
         x     :  Real
         )     :  Real;
         
implementation //////////////////////////////////////////////////////////////////////
uses
      Math,    // Used: Sign
      ExtraMath;
const
      // Determines the accuracy of numeric calculations
      Precision = DoublePrecision;

{-----------------------<< Assorted functions >>------------------------------------}
      
// Upper branch of Lambert's W function aka product logarithm
function LambertW(
         x           :  Real  // [-1 / e .. +inf)
         )           :  Real;
   var
         y, t, s, u,
         LnX1, OldY,
         Epsilon     :  Real;
   begin
   // Initial approximation
   Assert(x >= -1 / mE);
   if x < 0 then
      y := (mE * x) /
         (1 + 1 / 
            ( 1 / Sqrt(2 * mE * x + 2)
            + 1 / (mE - 1)
            - 1 / Sqrt(2) 
            ) 
         )
   else
      // y := Ln(x) - Ln( Ln(x) ) + Ln( Ln(x) ) / Ln(x) is a better approximation for
      // x > 95, use it as a third piece if a faster version is required.
      begin
      LnX1 := Ln(x + 1);
      y := LnX1 * ( 1 - Ln(1 + LnX1) / (2 + LnX1) );
      end;

   // Halley's method
   Epsilon := y * Precision;
   repeat
      t := y * Exp(y) - x;
      s := (y + 2) / ( 2 * (y + 1) );
      u := (y + 1) * Exp(y);
      OldY := y;
      y := y + t / (t * s - u);
   until Abs(OldY - y) <= Epsilon;
   Result := y;
   end;


// Normalized Sinc function
function Sinc(
         x  :  Real
         )  :  Real;
   var
         t  :  Real;
   begin
   t := mTau * x / 2;
   if x = 0 then
      Result := 1 else
      Result := Sin(t) / (t);
   end;


// Factorial of real x. Max relative error = 1.3e-8. 
// Stieltjes's approximation from [Luschny] is used.
function RealFac(
         x     :  Real
         )     :  Real;
   var
         y, K,
         R, p  :  Real;
   const
         Shift =  3;
   begin
   if x < 0 then
      Result := 1 / (RealFac(-x) * Sinc(x))
   else
      begin
      y := x + 1;
      p := 1;
      while y < Shift do
         begin
         p := p * y;
         y := y + 1;
         end;
      K := Sqrt(mTau / y) * Power(y / mE, y);
      R := (1 / 12) / (y + (1 / 30) / (y + (53 / 210) / (y + (195 / 371) / y)));
      Result := K * Exp(R) / p;
      end;
   end;

{-----------------------<< Normal CDF >>--------------------------------------------}
// Different forms of integrals of normal distribution. Accuracy of employed
// approximations is moderate, but usually should be sufficient.

// Integral [0, x] of standard normal distribution.
// Max absolute error = 3e-5, max relative error = 3e-4.
function NormalInt(
         x     :  Real
         )     :  Real;
   var
         t     :  Real;
   begin
   t := x * x;
   Result := Sign(x) * Sqrt
      (  1 -
         ( 7 * Exp( -t / 2 ) +
          16 * Exp( -t * ( 2 - Sqrt(2) ) ) +
               Exp( -t ) * (7 + mTau * t / 8)
         ) / 30
      ) / 2;
   end;


// Inverse normal integral. Max relative error = 1.7e-3.
function InvNormalInt(
         x     :  Real
         )     :  Real;
   var
         t, u  :  Real;
   const
         a     =  27;
   begin
   t := Ln(1 - Sqr(2 * x));
   u := 2 * a / mTau + t;
   Result := Sign(x) * Sqrt(Sqrt(Sqr(u) - a * t) - u);
   end;         


// Normal cumulative distribution function =
// Integral (-inf, x] of standard normal distribution
function NormalCDF(
         x     :  Real
         )     :  Real;
   begin
   Result := 1 / 2 + NormalInt(x);
   end;


// Normal complimentary cumulative distribution function =
// Integral [x, +inf) of standard normal distribution
function NormalCCDF(
         x     :  Real
         )     :  Real;
   begin
   Result := 1 / 2 - NormalInt(x);
   end;


// Inverse normal cumulative distribution function
function InvNormalCDF(
         x     :  Real
         )     :  Real;
   begin
   Result := InvNormalInt(x - 1 / 2);
   end;


// Inverse normal complimentary cumulative distribution function
function InvNormalCCDF(
         x     :  Real
         )     :  Real;
   begin
   Result := InvNormalInt(1 / 2 - x);
   end;

end.
