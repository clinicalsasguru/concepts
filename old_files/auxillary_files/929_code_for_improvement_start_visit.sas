data temp;
   input usubjid visitnum improvement$;
cards;
001 1 
001 2 I
001 3 I
001 4 I
001 5 I
001 6 I
001 7 I
002 1 
002 2 I
002 3 N
002 4 I
002 5 I
;
run;

proc sort data=temp;
   by usubjid visitnum;
run;

data temp2;
   set temp;
   retain imp_retention;
   by usubjid visitnum;
   if first.usubjid then call missing(imp_retention);
   if improvement="I" and missing(imp_retention) then imp_retention=visitnum;
   else if improvement ne "I" then imp_retention=.;
   if last.usubjid;
   keep imp_retention usubjid;
run;
