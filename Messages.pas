{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}
{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}
unit Messages; //////////////////////////////////////////////////////////////////////
{
>> Version: 0.1

> Description
   Unit responsible for displaying text messages

>> Author
   Peter Karpov
   Email    : PeterKarpov@inversed.ru
   Homepage : inversed.ru
   GitHub   : inversed-ru
   Twitter  : @inversed_ru

>> Changelog
   0.1 : 2018.09.18  ~ FreePascal compatibility
                     + Routine comments
   0.0 : 2011.03.19  + Initial version
   Notation: + added, - removed, * fixed, ~ changed   
}
interface ///////////////////////////////////////////////////////////////////////////
uses
      Problem;
const
      ErrorOpenFile   = 'Failed to open ';
      ErrorUndefined  = 'Undefined routine';
      MsgNewBest      = 'New best score: ';
      MsgRunStarted   =  '{} run {} started';
      MsgRunFinished  =  ' run finished';
type
      ProcMessage = procedure(
         const S  :  AnsiString);

// Display a message S via ShowMessage if the latter is not nil
procedure TryShowMessage(
   const S           :  AnsiString;
         ShowMessage :  ProcMessage);

// Display a message S with error tag prepended via ShowMessage 
// if the latter is not nil
procedure ShowError(
   const S           :  AnsiString;
         ShowMessage :  ProcMessage);

// Display a message with NewBest's score via ShowMessage 
// if the latter is not nil
procedure ShowNewBestScore(
   const NewBest     :  TSolution;
         ShowMessage :  ProcMessage);

implementation //////////////////////////////////////////////////////////////////////
uses
      InvSys;

// Display a message S via ShowMessage if the latter is not nil
procedure TryShowMessage(
   const S           :  AnsiString;
         ShowMessage :  ProcMessage);
   begin
   if @ShowMessage <> nil then
      ShowMessage(S);
   end;


// Display a message S with error tag prepended via ShowMessage 
// if the latter is not nil
procedure ShowError(
   const S           :  AnsiString;
         ShowMessage :  ProcMessage);
   const
         ErrorTag = 'ERROR: ';
   begin
   if @ShowMessage <> nil then
      ShowMessage(ErrorTag + S);
   end;


// Display a message with NewBest's score via ShowMessage 
// if the latter is not nil
procedure ShowNewBestScore(
   const NewBest     :  TSolution;
         ShowMessage :  ProcMessage);
   begin
   TryShowMessage(MsgNewBest + FormatScore(NewBest.Score), ShowMessage);
   end;

end.
