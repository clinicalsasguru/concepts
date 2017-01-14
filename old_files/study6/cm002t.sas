
*redacted;

options validvarname=upcase varlenchk=nowarn;

%assignlibs;

data adcm01;
   set s_adam_i.adcm;
   where saffl="Y" and cmdurfl="Y";
run;

proc sql noprint;
   select count(*) into :nobs from adcm01;
quit;

%macro cm002t;

%if &nobs gt 0 %then %do;
data adcm;
   set adcm01;
   output;
   trtan=3;
   output;
run;

data adsl;
   set s_adam_i.adsl(rename=(trt01an=trtan));
   where saffl="Y";
   keep usubjid trtan;
   output;
   trtan=3;
   output;
run;


*================================================================================;
*get treatment totals into a dataset and into macro variables(for column headers);
*================================================================================;

proc sql;
   create table trttotal_pre as 
      select trtan,
      count(distinct usubjid) as trttotal
      from adsl
      group by trtan;
quit;

*create dummy dataset for treatement totals;

data dummy_pre;
   trttotal=0;
   do trtan=1 to 3;
      output;
   end;
run;

*merge actual counts with dummt counts;

data trttotals;
   merge dummy_pre(in=a) trttotal_pre(in=b);
   by trtan;
run;


*macro variables;
proc sql noprint;
   select count(distinct usubjid) into :n1 from adsl where trtan=1;
   select count(distinct usubjid) into :n2 from adsl where trtan=2;
   select count(distinct usubjid) into :n3 from adsl where trtan=3;
quit;

%put number of subjects in trt1 &n1;
%put number of subjects in trt2 &n2;
%put number of subjects in trt3 &n3;

*=====================================================================================;

data cm_pre01;
   set adcm;
   if missing(cm2atcl) then cm2atcl="Not coded";
   if missing(cm4atcl) then cm4atcl="Not coded";
   if missing(cmdecod) then cmdecod="Not coded";
run;

proc sort data=cm_pre01 out=cm_pre;
   by usubjid trtan;
run;

data cm;
   merge cm_pre(in=a) adsl(in=b);
   by usubjid trtan;
   if a and b;
run;

*================================================================;
*obtaining actual counts-for the table;
*================================================================;

*------------------------------;
*subject level count- top row;
*------------------------------;

proc sql noprint;
   create table sub_count as 
   select "Overall" as label length=200,
   trtan,
   count(distinct usubjid) as count 
   from cm 
   group by trtan;
quit;

*---------------------------------------;
*cm2atcl level counts;
*---------------------------------------;

proc sql noprint;
   create table cm2_count as
      select cm2atcl, trtan,
      count(distinct usubjid) as count
      from cm 
      group by cm2atcl,trtan;
quit;

*--------------------------------------;
*cm4atcl term level counts;
*--------------------------------------;

proc sql noprint;
   create table cm4_count as 
      select cm2atcl,cm4atcl,trtan,
      count(distinct usubjid) as count
      from cm 
      group by cm2atcl,cm4atcl,trtan;
quit;


*--------------------------------------;
*cmdecod level counts;
*--------------------------------------;

proc sql noprint;
   create table cmdecod_count as 
      select cm2atcl,cm4atcl,cmdecod,trtan,
      count(distinct usubjid) as count
      from cm 
      group by cm2atcl,cm4atcl,cmdecod,trtan;
quit;

*---------------------------------;
*put all counts together;
*---------------------------------;

data counts;
   set sub_count cm2_count cm4_count cmdecod_count;
run;

*=========================================================;
*create zero counts if cmdecod is not present in a trt;
*=========================================================;

*get all the available classes and decods;

proc sort data=counts out=dummy_pre(keep=cm2atcl cm4atcl cmdecod label) nodupkey;
   by cm2atcl cm4atcl cmdecod label;
run;

*create a row for each treatment ;

data dummy;
   set dummy_pre;
   count=0;
   do trtan=0 to 3;
         output;
   end;
run;

*merge dummy counts with actual counts;
proc sort data=dummy;
   by cm2atcl cm4atcl cmdecod label trtan;
run;


proc sort data=counts out=counts2;
   by cm2atcl cm4atcl cmdecod label trtan;
run;

data counts3;
   merge dummy(in=a) counts2(in=b);
   by cm2atcl cm4atcl cmdecod label trtan;
run;

*---------------------------------------;
*obtain percentages;
*---------------------------------------;

*merge counts with trttotals dataset, to get denominator values(trt totals);

proc sort data=counts3;
   by trtan;
run;

proc sort data=trttotals;
   by trtan;
run;

data counts4;
   merge counts3(in=a) trttotals(in=b);
   by trtan;
   if a;
run;

data counts4;
   set counts4;
   length count_percent $30;
   if count ne 0 then count_percent=put(count,3.)||" ("||put(count/trttotal*100,5.1)||"%)";
   else count_percent=put(count,3.);
run;

*---------------------------------------------;
*create the label column;
*---------------------------------------------;

data counts5;
   set counts4;
   if cmiss(cm2atcl,cm4atcl,cmdecod)=3 then label=label;
   else if not missing(cm2atcl) and cmiss(cm4atcl,cmdecod)=2 then label=strip(cm2atcl);
   else if cmiss(cm2atcl,cm4atcl)=0 and missing(cmdecod) then label=" "||strip(cm4atcl);
   else if cmiss(cm2atcl,cm4atcl,cmdecod)=0 then label="   "||strip(cmdecod);
run;

*====================================================;
*transpose to obtain treatment as columns;
*====================================================;

proc sort data=counts5;
   by cm2atcl cm4atcl cmdecod label ;
run;

proc transpose data=counts5 out=trans1 prefix=trt;
   by cm2atcl cm4atcl cmdecod label;
   var count_percent;
   id trtan;
run;

data final;
   set trans1;
   keep cm2atcl cm4atcl cmdecod label trt1 trt2 trt3;
run;

proc datasets lib=work nolist;
   modify final;
   rename label=c1 trt1=c2 trt2=c3 trt3=c4;
quit;


*============================================================================;
*report generation;
*============================================================================;

%*redacted;

proc report data=final nowd headline headskip missing spacing=0;
   columns cm2atcl cm4atcl cmdecod c1  c2 c3 c4;
   define cm2atcl/ order noprint;
   define cm4atcl/order noprint;
   define cmdecod/order noprint;
   define c1 /order "Therapeutic Subgroup (ATC 2nd level)" "    Chemical Subgroup (ATC 4th level)"
            "       Preferred WHO name" width=50 flow;
    define c2/"Placebo" "(N=%cmpres(&n1))" width=18  ;
    define c3/"*redacted;" "(N=%cmpres(&n2))" width=18  ;
    define c4/"Total" "(N=%cmpres(&n3))" width=18  ;
   break after cm2atcl/skip;

run;
%end;

%else %do;

   data final;
      length c1-c4 $100;
      call missing(of c1-c4);
   run;

%*redacted;

   data nores;
      length comment $200;
      comment="---------------- No Results to Show at This Time ------------";
   run;

   proc report data=nores headline headskip nowd;
      column comment;
      define comment/ display ' ' width=100 center;
   run;

%end;

data compt.cm002t;
   set final;
   keep c1-c4;
run;

%mend cm002t;

%cm002t;

*redacted;
*redacted;

*redacted;(prog=&prog_loc./cm002t.log);
*redacted;(prog=&prog_loc./cm002t.sas, standalone=Y);

