{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit Acceptance; ////////////////////////////////////////////////////////////////////
{
>> Version: 0.3

>> Description
   Various acceptance functions for annealing-like algorithms and a generic 
   solution improvement procedure.
    
>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru
   
>> ToDo
   ? Remove Barker criterion (inferior performance, complicates the code)

>> Changelog
   0.3 : 2019.05.21  ~ Renamed IsMinimize to Minimization
   0.2 : 2018.09.18  ~ FreePascal compatibility
                     ~ Rewrote the comments
                     - Moved the improvement section to LocalSearchAlg
   0.1 : 2013.02.06  + RunHot procedure
                     + New overloaded version of GetAutoT0
   0.0 : 2012.12.18  + Initial version
   Notation: + added, - removed, * fixed, ~ changed
}
{$MINENUMSIZE 4}
interface ///////////////////////////////////////////////////////////////////////////
uses
      Arrays,
      Problem;

{-----------------------<< Acceptance >>--------------------------------------------}
type
      TAcceptStyle = (asExp, asPower, asTsallis, asThreshold, asBarker);
      TAcceptance =
         record
         Style :  TAcceptStyle;
         P     :  Real;
         end;

      TT0Mode = (t0Manual, t0EBased, t0AutoLow, t0AutoHigh);

// Return acceptance probability given the new and old scores, temperature T and
// Acceptance
function ProbAccept(
         NewScore,
         OldScore    :  TScore;
         T           :  Real;
   const Acceptance  :  TAcceptance
         )           :  Real;

// Inverse acceptance probability function returning the energy difference given
// the acceptance probability Prob and Acceptance
function InvProbAccept(
         Prob        :  Real;
   const Acceptance  :  TAcceptance
         )           :  Real;

// Relative mean thermal energy of Acceptance criterion
function ThermalEnergy(
   const Acceptance  :  TAcceptance
         )           :  Real;

// Convert an energy difference dE to a temperature corresponding to Acceptance
function dEToT0(
         dE          :  Real;
   const Acceptance  :  TAcceptance;
         T0Mode      :  TT0Mode
         )           :  Real;

// Run annealing at infinite temperature for Iters, return energy data.
// Extra initial BurnIn * Iters points are discarded.
procedure RunHot(
   var   E        :  TRealArray;
         Iters    :  Integer;
         BurnIn   :  Real);         

// Return the appropriate initial temperature based on energy values HotE
// recorded at an infinite temperature
function GetAutoT0(
   const HotE        :  TRealArray;
         T0Mode      :  TT0Mode;
   const Acceptance  :  TAcceptance
         )           :  Real;
         overload;

// Automatically determine the initial temperature corresponding to Acceptance based
// on energy values recorded at an infinite temperature for Iters iterations
function GetAutoT0(
         T0Mode      :  TT0Mode;
   const Acceptance  :  TAcceptance;
         Iters       :  Integer
         )           :  Real;
         overload;

// A single simulated annealing step, the result indicates whether the new solution 
// was accepted
function SAStep(
   var   Work        :  TSolution;
   const Acceptance  :  TAcceptance;
         T           :  Real
         )           :  Boolean;

implementation //////////////////////////////////////////////////////////////////////
uses
      Math,
      InvSys,
      Statistics,
      SpecFuncs,
      Common;

{-----------------------<< Acceptance >>--------------------------------------------}

// Return acceptance probability given the new and old scores, temperature T and
// Acceptance
function ProbAccept(
         NewScore,
         OldScore    :  TScore;
         T           :  Real;
   const Acceptance  :  TAcceptance
         )           :  Real;
   var
         dE, x       :  Real;
   begin
   dE := NewScore - OldScore;
   if not Minimization then
      dE := -dE;

   if Acceptance.Style = asBarker then
      // Barker criterion
      begin
      if T = 0 then
         Result := (1 - Sign(dE)) / 2
      else
         begin
         x := dE / T;
         if x > Ln(High(Integer)) then
            Result := Exp(-x)
         else
            Result := 1 / (1 + Exp(x));
         end;
      end
   else
      // Other criteria
      begin
      if dE <= 0 then
         Result := 1
      else if T = 0 then
         Result := 0
      else with Acceptance do
         begin
         x := dE / T;
         if x = Infinity then
            Result := 0
         else
            case Style of
               asExp:
                  Result :=     Exp(-Power(x, P));
               asPower:
                  Result := 1 / (1 + Power(x, P));
               asTsallis:
                  if P = 1 then
                     Result := Exp(-x)
                  else
                     Result := Power(Max(1 - (1 - P) * x, 0), 1 / (1 - P));
               asThreshold:
                  Result := (1 - Sign(x - 1)) / 2;
               else
                  Assert(False);
                  Result := 0;
               end;
         end;
      end;
   end;


// Inverse acceptance probability function returning the energy difference given
// the acceptance probability Prob and Acceptance
function InvProbAccept(
         Prob        :  Real;
   const Acceptance  :  TAcceptance
         )           :  Real;
   begin
   with Acceptance do
      case Style of
         asExp:         Result := Power(-Ln(Prob), 1 / p);
         asPower:       Result := Power(1 / Prob - 1, 1 / p);
         asThreshold:   Result := 1;
         asBarker:      Result := Ln(1 / Prob - 1);
         asTsallis:     if P = 1 then
                           Result := -Ln(Prob)
                        else
                           Result := (1 - Power(Prob, 1 - p)) / (1 - p);         
         else           Result := 0;
                        Assert(False);
         end;
   end;


// Relative mean thermal energy of Acceptance criterion
function ThermalEnergy(
   const Acceptance  :  TAcceptance
         )           :  Real;
   begin
   with Acceptance do
      case Style of
         asExp:         Result := RealFac(1 / p);
         asPower:       Result := 1 / Sinc(1 / p); 
         asTsallis:     Result := 1 / (2 - p);
         asThreshold:   Result := 1;
         asBarker:      Result := Ln(4);
         else           Result := 0;
                        Assert(False);
         end;
   end;


// Convert an energy difference dE to a temperature corresponding to Acceptance
function dEToT0(
         dE          :  Real;
   const Acceptance  :  TAcceptance;
         T0Mode      :  TT0Mode
         )           :  Real;
   begin
   case T0Mode of
      t0AutoLow:
         Result := dE / ThermalEnergy(Acceptance);
      t0AutoHigh:
         if Acceptance.Style = asBarker then
            Result := dE / InvProbAccept(1 / 3, Acceptance) else
            Result := dE / InvProbAccept(1 / 2, Acceptance);
      else
         Result := 0;
         Assert(False);
      end;
   end;


// Run annealing at infinite temperature for Iters, return energy data.
// Extra initial BurnIn * Iters points are discarded.
procedure RunHot(
   var   E        :  TRealArray;
         Iters    :  Integer;
         BurnIn   :  Real);
   var
         i        :  Integer;
         x        :  TSolution;
         Undo     :  TSAUndo;
   begin
   // Burn-in
   NewSolution(x);
   for i := 0 to Round(BurnIn * Iters) - 1 do
      MakeNeighbour(x, Undo, {T:} MaxDouble);

   // Record the energy
   SetLength(E, Iters);
   for i := 0 to Iters - 1 do
      begin
      MakeNeighbour(x, Undo, {T:} MaxDouble);
      E[i] := x.Score;
      end;
   end;


// Return the appropriate initial temperature based on energy values HotE
// recorded at an infinite temperature
function GetAutoT0(
   const HotE        :  TRealArray;
         T0Mode      :  TT0Mode;
   const Acceptance  :  TAcceptance
         )           :  Real;
         overload;
   var
         i, Len      :  Integer;
         dE          :  TRealArray;
         dEScale     :  Real;
   begin
   Len := Length(HotE) - 1;
   SetLength(dE, Len);
   for i := 0 to Len - 1 do
      // #HACK use Differentiate with forward difference instead
      dE[i] := HotE[i + 1] - HotE[i];
   case T0Mode of
      t0AutoLow:  dEScale := 2 * MeanAbsDeviation(dE);
      t0AutoHigh: dEScale := 3 * StandDev(dE);
      else        dEScale := 0;
                  Assert(False);
      end;
   Result := dEToT0(dEScale, Acceptance, T0Mode);
   end;


// Automatically determine the initial temperature corresponding to Acceptance based
// on energy values recorded at an infinite temperature for Iters iterations
function GetAutoT0(
         T0Mode      :  TT0Mode;
   const Acceptance  :  TAcceptance;
         Iters       :  Integer
         )           :  Real;
         overload;
   var
         HotE        :  TRealArray;
   begin
   RunHot(HotE, Iters, {BurnIn:} 1 / 2);
   Result := GetAutoT0(HotE, T0Mode, Acceptance);
   end;


// A single simulated annealing step, the result indicates whether the new solution 
// was accepted
function SAStep(
   var   Work        :  TSolution;
   const Acceptance  :  TAcceptance;
         T           :  Real
         )           :  Boolean;
   var
         OldScore    :  TScore;
         Undo        :  TSAUndo;
   begin
   OldScore := Work.Score;
   MakeNeighbour(Work, Undo, T);
   if Random < ProbAccept(Work.Score, OldScore, T, Acceptance) then
      Result := Success
   else
      begin
      UndoSAMove(Work, Undo);
      Result := Fail;
      end;
   end;

end.
