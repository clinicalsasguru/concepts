*redacted;

options varlenchk=nowarn;

data adsl;
   set s_adam_i.adsl(where=(randfl='Y'));
   if scomplfl="Y" then dssreasn=1;
run;

data adsl1;
   set adsl;
   length c2 $100;

   if compress(dssfl)="Y" then do;
      dssreasn=-1;
      c2="Yes";
   end;

   if compress(dssfl)='N' then do;
      dssreasn=0;
      c2="No";
   end;
run;

proc sql noprint;
   select n(trt01an) into:_1 - :_2
   from adsl
   group by trt01an;
   select n (usubjid) into :_3
   from adsl;
quit;

*****************Treatment Discontinuation*****;

proc freq data=adsl1 noprint;
   table dssreasn*c2*trt01an / out=dssc (drop=percent);
run;

proc transpose data=dssc out=dsst(drop=_name_ _label_);
   by dssreasn c2;
   id trt01an;
run;

data trdummy;
   length c2 $100;
   dssreasn=-1;
   c2="Yes";
   output;
   dssreasn=0;
   c2="No";
   output;
run;

data dssf;
   merge trdummy dsst;
   by dssreasn c2;
   length c1 $100;
   group=1;
   c1="Study Discontinuation";
run;

************Primary Reason for Discontinuation - Complete*****;

proc freq data=adsl (where=(dssfl="N" and scomplfl="Y")) noprint;
   table dssreasn*trt01an / out=dscc (drop=percent);
run;

proc transpose data=dscc out=dsct(drop=_name_ _label_);
   by dssreasn;
   id trt01an;
run;

data tcdummy;
   length c2 $100;
   dssreasn=1;
   c2="Completed";
run;

data dscf;
   merge tcdummy dsct;
   by dssreasn ;
   length c1 $100;
   group=2;
   if missing(dssreasn) then delete;
   c1="Primary Reason for Discontinuation [1]";
run;

************Primary Reason for Discontinuation - All*****;

proc freq data=adsl (where=(dssfl="Y")) noprint;
   table dssreasn*trt01an / out=discc (drop=percent);
run;

proc transpose data=discc out=disct(drop=_name_ _label_ );
   by dssreasn ;
   id trt01an;
run;

data discdummy;
   length  c2 $100;
   do dssreasn=2 to 11;
      if  dssreasn=2 then c2="Randomized/Registered but Never Received/Dispensed Study Drug";
      if  dssreasn=3 then c2="Adverse Event";
      if  dssreasn=4 then c2="Death";
      if  dssreasn=5 then c2="Lost to Follow-up";
      if  dssreasn=6 then c2="Withdrawal by Subject";
      if  dssreasn=7 then c2="Study Terminated by Sponsor";
      if  dssreasn=8 then c2="Physician Decision";
      if  dssreasn=9 then c2="Non-Compliance with Study Drug";
      if  dssreasn=10 then c2="Pregnancy";
      if  dssreasn=11 then c2="Other";
      output;
   end;
run;

data discf;
   merge discdummy disct;
   by dssreasn;
   length c1 $100;
   group=2;
   if missing(dssreasn) then delete;
   c1="Primary Reason for Discontinuation [1]";
run;

%assignlibs;
data main;
   set compt.dm005t;
   drop ord;
run;

data final;
   set dssf(in=a) dscf(in=b) discf(in=c) ;
   length c1 c2 $100 c3-c5 $30;
   if missing(_1) then _1=0;
   if missing(_2) then _2=0;
   _3=sum(_1,_2);
   if _1=0 then c3="  0";
   else c3=put(_1,3.)||" ("||put(_1/&_1*100,5.1)||"%)";
   if _2=0 then c4="  0";
   else c4=put(_2,3.)||" ("||put(_2/&_2*100,5.1)||"%)";
   if _3=0 then c5="  0";
   else c5=put(_3,3.)||" ("||put(_3/&_3*100,5.1)||"%)";
run;

proc report data=final headline headskip split='*';
   columns group c1 c2-c5;
   define group / group noprint;
   define c1 / group "Parameter" width=40 flow left spacing=0;
   define c2 / display "Category" width=45 flow left spacing=3;
   define c3 / display "*redacted;*(N=&_1)" width=12 left spacing=3;
   define c4 / display "Placebo*(N=&_2)" width=12 left spacing=3;
   define c5 / display "Total*(N=&_3)" width=12 left spacing=3;
   break after group/skip;
run;


proc compare base=main compare=final(keep=group c1-c5) listall;
run;

%dual_devprod;
*redacted;

*redacted;(prog=/sas/projects/8232/8232-cl-0004/progs/qc/tables/dm005t_v.log);
*redacted;(prog=/sas/projects/8232/8232-cl-0004/progs/qc/tables/dm005t_v.sas);

*redacted;(prog=/sas/projects/8232/8232-cl-0004/progs/dev/tables/dm005t.log);
*redacted;(prog=/sas/projects/8232/8232-cl-0004/progs/dev/tables/dm005t.sas);
