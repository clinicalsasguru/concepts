*redacted;

options validvarname=upcase varlenchk=nowarn;

proc sort data= s_adam_i.adae(where=(saffl='Y' and trtemfl='Y' and aeser="N")) out=adae nodupkey;
   by aebodsys aedecod usubjid;
run;

data adsl;
   set s_adam_i.adsl;
   where saffl="Y";
   output;
   trt01an=3;
   output;
run;

proc sql noprint;
   select n(trt01an) into :_1 - :_3
   from adsl
   group by trt01an;
quit;

%put &_1 &_2 &_3;

proc freq data=adae noprint;
   by  aebodsys aedecod ;
   table trtan/out=aecnt(drop=percent);
run;

*****Exclude the serious evetns that exceed the 5% threshold in any treatment group*****;
data aensergt5;
   set aecnt;
      if trtan=1 then percent=round(count/&_1*100,.1);
      if trtan=2 then percent=round(count/&_2*100,.1);
      if percent>=5 then output;
run;

*******Re-establish the ADAE dataset without the excluded AEs******;

data adae1;
   merge aensergt5(keep=aebodsys aedecod  in=a) adae(in=b);
   by aebodsys  aedecod ;
   if a;
run;

%macro getntrtem (param=, byvars=, tablevars=);

proc sort data=adae1 out=&param nodupkey;
   by usubjid &byvars;
run;

proc freq data=&param noprint;
   table &tablevars / out=&param.c (drop=percent);
run;

proc transpose data=&param.c out=&param.t(drop=_name_ _label_);
   by &byvars;
   id trtan;
run;

data &param.t1;
   set &param.t;
   length soc_pt $200;
   %if &param=ovall %then %do;
      soc_pt="Overall";
      grp=0;
   %end;
   %if &param=soc %then %do;
      soc_pt=aebodsys;
   %end;
   %if &param=pt %then %do;
      soc_pt="  "||aedecod;
   %end;
run;

data &param.f;
   set &param.t1;
   if _1=. then _1=0;
   if _2=. then _2=0;
   _3=sum(_1,_2);
run;

%mend getntrtem;

%getntrtem(param=ovall,byvars=, tablevars=trtan);
%getntrtem(param=soc,byvars=aebodsys, tablevars=aebodsys*trtan);
%getntrtem(param=pt,byvars=aebodsys aedecod , tablevars=aebodsys*aedecod*trtan);

data final1;
   set socf ptf;
   by aebodsys;
   retain grp;
   if first.aebodsys then grp+1;
run;

data final;
   set ovallf final1;
   length  c1-c3 $100;
   if _1=. then _1=0;
   if _2=. then _2=0;
   _3=sum(_1,_2);
   if _1 ne 0 then c1=put(_1,3.)||" ("||put(_1/&_1*100,5.1)||"%)";
   else c1='  0';
   if _2 ne 0 then c2=put(_2,3.)||" ("||put(_2/&_2*100,5.1)||"%)";
   else c2='  0';
   if _3 ne 0 then c3=put(_3,3.)||" ("||put(_3/&_3*100,5.1)||"%)";
   else c3='  0';
   keep grp soc_pt c1-c3;
run;

proc sql noprint;
   select n(soc_pt) into :n_obs
   from final
   where soc_pt ne "Overall";
quit;

%put &n_obs;

%assignlibs;

data compt.ae010t;
   set final;
run;

%*redacted;

%macro report;

%if &n_obs=0 %then %do;

   data nores;
      length comment $200;
      comment="---------------- No Results to Show at This Time ------------";
   run;

   proc report data=nores headline headskip ;
      column comment;
      define comment/ display ' ' width=100 center;
   run;
%end;

%else %do;
   proc report data = final headline headskip missing split='*';
      columns grp soc_pt c1-c3;
      define grp/group noprint;
      define soc_pt / display "System Organ Class*  Preferred Term" width=60 flow left ;
      define c1 / display "*redacted;*(N=&_1)" width=15 left  spacing = 5;    
      define c2 / display "Placebo*(N=&_2)" width=15 left  spacing = 5; 
      define c3 / display "Total*(N=&_3)" width=15 left  spacing = 5;  
      break after grp / skip;
   run;
%end;

%mend report;

%report;

*redacted;
*redacted;

*redacted;(prog=&prog_loc./ae010t.log);
*redacted;(prog=&prog_loc./ae010t.sas, standalone=Y);
