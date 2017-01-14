*---------------------------------------;
*alttb;
*---------------------------------------;

data alttb01;
   set lb03;
   where paramcd in ("ALTE03S" "BILIG01S");
   rename aval=s_aval;
   drop avalc lbseq agrpid anrind anrlo ;
run;

proc sort data=alttb01;
   by usubjid visitnum visit adtm adt atm paramcd;
run;

data alttb;
   set alttb01;
   by usubjid visitnum visit adtm adt atm paramcd;
   retain alte03s alt03anrhi;
   length avalc $100;
   if first.atm and last.atm then delete;
   if first.atm then do;
   call missing(alte03s,alt03anrhi);
   alte03s=s_aval;
   alt03anrhi=anrhi;
   end;
   if last.atm ;
   paramcd="ALTTB";
   param="ALT >=3X ULN and total bilirubin >=2X ULN";
   if alte03s >= 3*alt03anrhi gt .  and  s_AVAL >= 2*ANRHI gt . then AVALC = "Y";
   else avalc="N";
   drop s_aval alt: anrhi;
run;


*-------------------------------------------------------------------------------------------;
*extended version - for multiple paramcds to single paramcd;
*-------------------------------------------------------------------------------------------;

*---------------------------------------;
*altsttbr;
*---------------------------------------;

data altsttbr01;
   set lb03;
   where paramcd in ("ASTE01S" "ALTE03S" "BILIG01S"  "INRQ81S");
   rename aval=s_aval;
   drop avalc lbseq agrpid anrind anrlo ;
run;

proc sort data=altsttbr01;
   by STUDYID USUBJID fastrfl VISITNUM VISIT adt atm
   adtm  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT ady prefl paramcd s_aval;
;
run;

proc transpose data=altsttbr01 out=altsttbr01_t_aval(drop=_name_);
   by STUDYID USUBJID fastrfl VISITNUM VISIT adt atm
   adtm  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT ady prefl  ;
   var s_aval;
   id paramcd;
run;

proc transpose data=altsttbr01 out=altsttbr01_t_anrhi(drop=_name_) prefix=anrhi;
   by STUDYID USUBJID fastrfl VISITNUM VISIT adt atm
   adtm  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT ady prefl  ;
   var anrhi;
   id paramcd;

run;

data altsttbr01_m;
   merge altsttbr01_t_aval(in=a) altsttbr01_t_anrhi(in=b);
   by STUDYID USUBJID fastrfl VISITNUM VISIT adt atm
   adtm  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT ady prefl  ;
   if a and b;
run;

data altsttbr;
   set altsttbr01_m;
   length avalc $100 paramcd $8 param $200;

   if missing(bilig01s) and missing(INRQ81S) then delete;
   if missing(aste01s) and missing(alte03s) then delete;

   *"ASTE01S" "ALTE03S" "BILIG01S"  "INRQ81S";

   paramcd="ALTSTTBR";
   param="ALT or AST >=3X ULN and (total bilirubin level >=2X ULN or INR >=1.5X ULN)";

   if ((ASTE01S >= 3*anrhiASTE01S gt .) or (ALTE03S >= 3*anrhiALTE03S gt .)) and
   ((BILIG01S >= 2*anrhiBILIG01S gt .) or (INRQ81S >= 1.5*anrhiINRQ81S gt .)) then avalc = "Y";
   else avalc="N";
run;
