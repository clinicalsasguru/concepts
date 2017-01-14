dm log 'clear';
dm output 'clear';

options mautosource sasautos=("D:\Home\dev\compound9\lums" sasautos);
%macro closevts  /* The cmd option makes the macro available to dms */ / cmd; 
  %local i; 
  %do i=1 %to 20;
    next "viewtable:"; end; 
  %end; 
%mend;

dm "keydef F12 '%NRSTR(%closevts);'";


data vs;
   input usubjid paramcd :$8. avisitn adt atm aval;
   target=avisitn*7+1;
   diff=adt-target;
   absdiff=abs(diff);
cards;
001 param 2 16 1 100
001 param 3 22 2 101
001 param 3 22 3 103
001 param 4 32 4 170
001 param 4 32 4 120
001 param 5 37 5 90
001 param 5 35 6 100
001 param 6 45 2 101
001 param 6 41 3 103
001 param 6 43 4 100
;
run;

proc sort data=vs;
   by usubjid paramcd avisitn adt atm;
run;

data vs;
   set vs;
   by usubjid paramcd avisitn adt atm;
   if first.usubjid then vsseq=1;
   else vsseq+1;
run;
*--------------------------------------------------------------------;
*create baseline flag - basetype=original;
*--------------------------------------------------------------------;
proc sort data=vs out=base_orig_pre;
   by usubjid paramcd avisitn adt atm vsseq;
   where avisitn le 4 and aval ne .;*considering all the visits le 4 as pre-treatment records;
run;

data base_orig_pre;
   set base_orig_pre;
   by usubjid paramcd avisitn adt atm vsseq;
   if last.paramcd;
   basedt_orig=adt;
   basetime_orig=atm;
   length basetype $20;
   basetype="ORIGINAL";
   ablfl="Y";
   base=aval;
   keep usubjid paramcd base basedt_orig basetime_orig basetype adt atm vsseq ablfl;
run;

data vs;
   merge vs(in=a) base_orig_pre;
   by usubjid paramcd adt atm vsseq;
run;

data vs;
   set vs;
   by usubjid paramcd adt atm vsseq;
   if first.paramcd then call missing(basedt,basetime_orig);
   if not missing(basedt_orig) then basedt=basedt_orig;
   if not missing(basetime_orig) then basetime=basetime_orig;
   retain basedt basetime;
run;

*----------------------------------------------------------------------------------------;
*create baseline minimum record -serves as minimum baseline record for basetype=minimum;
*----------------------------------------------------------------------------------------;

proc sort data=vs out=base_min_pre;
   by usubjid paramcd descending aval avisitn adt atm vsseq;
   where not missing(aval) and avisitn le 4;
run;

data base_min_pre;
   set base_min_pre;
   by usubjid paramcd descending aval avisitn adt atm vsseq;
   if last.paramcd;
   length basetype $20;
   basetype="MINIMUM";
   ablfl="Y";
   avisitn=-99;
   base=aval;
run;

*----------------------------------------------------------------------------------------;
*create baseline maximum record -serves as maximum baseline record for basetype=maximum;
*----------------------------------------------------------------------------------------;

proc sort data=vs out=base_max_pre;
   by usubjid paramcd aval avisitn adt atm vsseq;
   where not missing(aval) and avisitn le 4;
run;

data base_max_pre;
   set base_max_pre;
   by usubjid paramcd aval avisitn adt atm vsseq;
   if last.paramcd;
   length basetype $20;
   basetype="MAXIMUM";
   ablfl="Y";
   avisitn=-100;
   base=aval;
run;

*--------------------------------------------------------------------;
*set min and max and original baseline records;
*--------------------------------------------------------------------;

data baseline;
   set vs(where=(ablfl="Y")) base_min_pre base_max_pre;
   keep usubjid paramcd base basetype;
run;

*duplicate the source postbaseline records to three sets for different basetypes;

data vs_temp;
   set vs;
   where (((adt=basedt gt .) and (atm gt basetime)) or (adt gt basedt gt .));
run;

data vs03;
   set vs_temp(in=a) vs_temp(in=b) vs_temp(in=c);
   length basetype $20;
   if a then basetype="ORIGINAL";
   else if b then basetype="MINIMUM";
   else if c then basetype="MAXIMUM";
run;

proc sort data=vs03;
   by usubjid paramcd basetype;
run;

proc sort data=baseline;
   by usubjid paramcd basetype;
run;

data vs04;
   merge vs03(drop=base) baseline;
   by usubjid paramcd basetype;
run;

data vs05;
   set vs(where=(avisitn le 4)) vs04 base_min_pre base_max_pre;
run;



%ut_saslogcheck;
