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
001 param 4 32 4 120
001 param 4 32 4 170
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

proc sort data=vs out=vs02;
   by usubjid paramcd avisitn absdiff diff adt atm;
run;

data vs02;
   set vs02;
   by usubjid paramcd avisitn absdiff diff adt atm;

   retain minabsdiff;

   if first.absdiff then call missing(visit_counter,absdiff_counter );

   if first.avisitn and last.avisitn then do; visit_counter=1; flag_cond=1; end;*these records would directly qualify for anl01fl;
   if first.avisitn then do;
      call missing(minabsdiff);
      minabsdiff=absdiff; * as we have already sorted by absdiff, the min value will be on first record - will need minabsdiff in next step;
   end;
   
   if first.absdiff then absdiff_counter=1;*check the number of records in each absdiff dates - will need to get the latest record
    and also to check if the records are on the same date and time;
   else absdiff_counter+1;
run;

*delete the records which are not minobs;
data vs03; 
   set vs02;
   if minabsdiff ne absdiff then delete;
run;

*check how many records are present in each minabsdiff;
proc sort data=vs03 out=vs04;
   by usubjid paramcd avisitn minabsdiff absdiff adt atm;
run;

data vs05(keep=usubjid paramcd vsseq anl01fl) dtypeavg(drop=adt atm vsseq);
   set vs04;
   by usubjid paramcd avisitn minabsdiff absdiff adt atm;
   if first.minabsdiff then nrecs=1;
   else nrecs+1;
   lagdate=lag(adt);
   lagtime=lag(atm);
   if first.minabsdiff then do;
      call missing(lagdate,lagtime,temp_aval);
   end;
   temp_aval+aval;
   if last.minabsdiff and ((adt ne lagdate) or ((adt =lagdate) and (atm ne lagtime))) then flag_cond=1;
   if last.minabsdiff and ((adt=lagdate) and (atm = lagtime)) then do; flag_cond=1; dtype="AVERAGE"; aval=temp_aval/nrecs; anl01fl="Y"; output dtypeavg; end;
   else if flag_cond=1 then do; anl01fl="Y"; output vs05; end;
run;

proc sort data=vs;
   by usubjid paramcd vsseq;
run;

data vs_final;
   merge vs(in=a) vs05(in=b);
   by usubjid paramcd vsseq;
   if a;
run;

data vs_final;
   set vs_final dtypeavg;
run;

proc sort data=vs_final;
   by usubjid avisitn adt atm;
run;

%ut_saslogcheck;

*secondary logic - need to be tested (cross-checked) against first logic;

/*---- Start of User Written Code  ----*/ 



data anlflg;
  set &_input;
run;

data anlflg1 anlflg2;
  set anlflg;
  if dtype = " " and paramcd not in ("BMI", "HEIGHTCM", "BMICAT") then output anlflg1;
  else output anlflg2;
run;

/* check for duplicates */
proc sort data=anlflg2 dupout=anlflg2chk nodupkey;
  by usubjid avisitn paramcd;
run;

/* keep anlflg2 to append later and use anlflg1 to derive ANLO1FL flag */

proc sort data=anlflg1;
  by usubjid avisitn paramcd;
run;

data anlflg11 anlflg12;
  set anlflg1;
  by usubjid avisitn paramcd;
  if (first.paramcd =last.paramcd)=1 then output anlflg11;
  else output anlflg12;
run;

proc sort data=anlflg12 nodupkey;
  by usubjid avisitn paramcd;
run;

/* getting unique records per usubjid avisitn paramcd */
data anlflg3;
  merge anlflg1(in=a) anlflg12(in=b);
  by usubjid avisitn paramcd;
  if a and not b;
run;

proc sort data=anlflg3 nodupkey;
  by usubjid avisitn paramcd;  
run;  


/* getting only duplicates */
data anlflg13;
  merge anlflg1(in=a) anlflg12(in=b);
  by usubjid avisitn paramcd;
  if a and b;
run;

data anlflgscr anlflg14;
  set anlflg13;
  if avisit = "Screening" then output anlflgscr;
  else output anlflg14;
run;

data anlflg15;
  set anlflg14;
  if avisit = "Day 1" then wk = 0;
  else do;
    wk = input(substr(avisit,6),8.);
    targetd = (7 * wk) + 1;
  end;
  targetdf = abs(ady - targetd);
run;

proc sort data=anlflg15 out=anlflg5;
  by usubjid avisitn paramcd targetdf descending adt descending atm;
run;

data anlflg51 anlflg52;
  set anlflg5;
   by usubjid avisitn paramcd targetdf descending adt descending atm;
  if (first.atm =last.atm)=1 then output anlflg51;
  else output anlflg52;
run;

proc sort data=anlflg15;
  by usubjid avisitn paramcd targetdf descending adt descending atm; * descending vsseq;
run;

data anlflg1f;
  set anlflg15;
  by usubjid avisitn paramcd targetdf descending adt descending atm; * descending vsseq;
  if first.paramcd then ANL01FL = "Y";
run;

data anlflg2f;
  set anlflg2;
  *ANL01FL = "Y";
run;

data anlflg3f;
  set anlflg3;
  ANL01FL = "Y";
run;

data anlflgf;
  set anlflg1f anlflg2f anlflg3f anlflgscr;
  if avisit in ("Screening") then ANL01FL = " ";              
run;
