{ 
Copyright (c) Peter Karpov 2010 - 2018.

Usage of the works is permitted provided that this instrument is retained with 
the works, so that any entity that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.
}

// ListTo := ListFrom
procedure AssignTabuList(
   var   ListTo   :  TTabuList;
   const ListFrom :  TTabuList);
   begin
   Assert(False, ErrorUndefined);
   end;


// Age the TabuList
procedure InitTabuList(
   var   TabuList :  TTabuList);
   begin
   Assert(False, ErrorUndefined);
   end;


// Perform aging of the _TabuList_
procedure AgeTabuList(
   var   TabuList :  TTabuList);
   begin
   Assert(False, ErrorUndefined);
   end;


// Add a Move that will be applied to Sol to the TabuList with Tenure
procedure AddToTabuList(
   var   TabuList :  TTabuList;
   const Move     :  TMove;
         Tenure   :  Integer;
   const Sol      :  TSolution);
   begin
   Assert(False, ErrorUndefined);
   end;


// Make a MoveList for the tabu search
procedure MakeTSMoveList(
   var   MoveList :  TMoveList;
   const Solution  :  TSolution);
   begin
   Assert(False, ErrorUndefined);
   end;


// Return whether a Move is in the Solution's TabuList 
function IsMoveTabu(
   const Move     :  TMove;
   const Solution  :  TSolution;
   const TabuList :  TTabuList
         )        :  Boolean;
   begin
   Assert(False, ErrorUndefined);
   Result := False;
   end;


// Return a tabu tenure at normalized time t
function TabuTenure(
         t  :  Real     // [0 .. 1]
         )  :  Integer;
   begin
   Assert(False, ErrorUndefined);
   Result := 0;
   end;