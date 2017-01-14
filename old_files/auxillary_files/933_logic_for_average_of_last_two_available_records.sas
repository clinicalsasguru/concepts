*------------------------------------------------------------;
* Average for visit 2 and 3 ""CYK1DE6S"", ""CYK1DE7S"";
*------------------------------------------------------------;

data cykavg01;
   set lb03;
   where paramcd in ("CYK1DE6S", "CYK1DE7S") and prefl="Y" and not missing(aval);
run;

proc sort data=cykavg01;
   by usubjid paramcd visitnum adt atm;
run;

data cyknrecs;
   set cykavg01 nobs=numobs;
   by usubjid paramcd visitnum adt atm;
   if first.paramcd then nrecs_pre=1;
   else nrecs_pre+1;
   if last.paramcd;
   keep usubjid paramcd nrecs_pre;
run;

data cykavg02;
   merge cykavg01(in=a) cyknrecs(in=b);
   by usubjid paramcd;
   if first.paramcd then do;
   call missing(nrecs,sum);
   end;
   nrecs+1;
run;

data cykavg03;
   set cykavg02;
   by usubjid paramcd;
   if nrecs=nrecs_pre or nrecs=nrecs_pre-1;
   rename aval=s_aval;
run; 

data cykavg;
   set cykavg03;
   length dtype $10 basetype $20;
   by usubjid paramcd visitnum adt atm;
   if first.paramcd then call missing(totrecs,sum_x);
   totrecs+1;
   sum_x+s_aval;
   if last.paramcd ;
   if nmiss(sum_x,totrecs)=0 then aval=sum_x/totrecs;
   dtype="AVERAGE";
   basetype="AVERAGE";
   ablfl="Y";

   if . lt aval lt anrlo then anrind="LOW";
   else if . lt anrlo le aval le anrhi then anrind="NORMAL";
   else if aval gt anrhi gt . then anrind="HIGH";
   drop anrind atm avalc adtm ady lbseq sum_x s_aval;
run;

