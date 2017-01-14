
*redacted;

options validvarname=upcase varlenchk=nowarn;

%assignlibs;

*==========================================================;
*macro for flagging first occurences;
*==========================================================;


*-------------------------------;
*macro for any flags;
*-------------------------------;
%macro anyflag(flag=,where=%str(trtemfl="Y"),byvar=,first=);

data adae1 adae_;
   set adae;
   if &where then output adae1;
   else output adae_;
run;

proc sort data=adae1;
   by &byvar.;
run;

data adae1;
   set adae1;
   by &byvar.;
   if last.&first.;
   keep &byvar.;
run;

proc sort data=adae;
   by &byvar.;
run;

data adae;
   merge adae(in=a) adae1(in=b);
   by &byvar.;
   if a and b then &flag="Y";
run;

%mend anyflag;


data adae01;
   set s_adam_i.adae;
   where saffl="Y" and trtemfl="Y";
run;

proc sql noprint;
   select count(*) into :nobs from adae01;
quit;

%macro ae005t;

%if &nobs gt 0 %then %do;
data adae;
   set adae01;
   output;
   trtan=3;
   output;
run;



%anyflag(flag=aoccifl,byvar=usubjid asevn,first=usubjid);
%anyflag(flag=aoccsifl,byvar=usubjid aebodsys asevn ,first=aebodsys);
%anyflag(flag=aoccpifl,byvar=usubjid aebodsys aedecod asevn ,first=aedecod);

data adae_temp;
   set adae;
   output;
   asevn=100;
   output;
run;

data adae;
   set adae_temp;
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

data ae_pre01;
   set adae;
   if missing(aebodsys) then aebodsys="Not coded";
   if missing(aedecod) then aedecod="Not coded";
run;

proc sort data=ae_pre01 out=ae_pre;
   by usubjid trtan;
run;

data ae;
   merge ae_pre(in=a) adsl(in=b);
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
   select "Overall" as label length=200,asevn,trtan,
   "" as aebodsys length=120, "" as aedecod length=120,
   count(distinct usubjid) as count1, count(usubjid) as count2
   from ae 
   where aoccifl="Y"
   group by asevn,trtan;
quit;


proc sql;
   create table soc_count as
      select aebodsys, asevn,trtan,
      count(distinct usubjid) as count1, count(usubjid) as count2
      from ae 
      where aoccsifl="Y"
      group by aebodsys,asevn,trtan;
quit;

*--------------------------------------;
*preferred term level counts;
*--------------------------------------;

proc sql noprint;
   create table pt_count as 
      select aebodsys,aedecod,asevn,trtan,
      count(distinct usubjid) as count1, count(usubjid) as count2
      from ae 
      where aoccpifl="Y"
      group by aebodsys,aedecod,asevn,trtan;
quit;

*---------------------------------;
*put all counts together;
*---------------------------------;

data counts;
   set sub_count soc_count pt_count;
run;

*=========================================================;
*create zero counts if an event is not present in a trt;
*=========================================================;

*get all the available soc-s and pt-s;

proc sort data=counts out=dummy_pre(keep=aebodsys aedecod label) nodupkey;
   by aebodsys aedecod label;
run;

*create a row for each treatment and severity;

data dummy;
   set dummy_pre;
   count1=0;
   count2=0;
   do trtan=0 to 3;
      do asevn=1,2,3,99,100;
         output;
      end;
   end;
run;

*merge dummy counts with actual counts;
proc sort data=dummy;
   by aebodsys aedecod label asevn trtan;
run;


proc sort data=counts out=counts2;
   by aebodsys aedecod label asevn trtan;
run;

data counts3;
   merge dummy(in=a) counts2(in=b);
   by aebodsys aedecod label asevn trtan;
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

data counts4_01;
   merge counts3(in=a) trttotals(in=b);
   by trtan;
   if a;
run;

proc sql noprint;
   select count(*) into :misscat from counts4_01 where asevn=99 and count1 gt 0;
run;

data counts4;
   set counts4_01;
   if (count1=0 and asevn=99) and (not &misscat) then delete;
   length count_percent $30;
   if count1 ne 0 then count_percent=put(count1,3.)||" ("||put(count1/trttotal*100,5.1)||"%)" ||put(count2,3.);
   else count_percent=put(count1,3.);
run;

*---------------------------------------------;
*create the label column;
*---------------------------------------------;

data counts5;
   set counts4;
   if missing(aebodsys) and missing(aedecod) then label=label;
   else if not missing(aebodsys) and missing(aedecod) then label=strip(aebodsys);
   else if not missing(aebodsys) and not missing(aedecod) then label="  "||strip(aedecod);
run;

*====================================================;
*transpose to obtain treatment as columns;
*====================================================;

proc sort data=counts5;
   by aebodsys aedecod asevn label ;
run;

proc transpose data=counts5 out=trans1 prefix=trt;
   by aebodsys aedecod asevn label;
   var count_percent;
   id trtan;
run;


data final;
   set trans1;
   length c2 $20;
   if asevn=1 then c2="Mild";
   else if asevn=2 then c2="Moderate";
   else if asevn=3 then c2="Severe";
   else if asevn=99 then c2="Missing";
   else if asevn=100 then c2="Total";

   keep aebodsys aedecod asevn label trt1 trt2 trt3 c2;
run;

proc sort data=final;
   by aebodsys aedecod label asevn;
run;

proc datasets lib=work;
   modify final;
   rename label=c1 trt1=c3 trt2=c4 trt3=c5;
quit;


*============================================================================;
*report generation;
*============================================================================;

%*redacted;

proc report data=final nowd headline headskip missing spacing=0;
   columns aebodsys aedecod  c1  asevn c2 c3 c4 c5;
   define aebodsys/ order noprint order=data;
   define aedecod/order noprint order=data;
   define asevn /order noprint order=data;
   define c1 /order order=data "System Organ Class" "Preferred Term" width=50 flow;
   define c2/"Maximum" "Severity" width=10;
    define c3/"*redacted;"  "(N=%cmpres(&n1))" width=18  ;
    define c4/"Placebo" "(N=%cmpres(&n2))" width=18  ;
    define c5/"Total" "(N=%cmpres(&n3))" width=18  ;
   break after aedecod/skip;

run;

%end;
%else %do;

   data final;
      length c1-c5 $100;
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

data compt.ae005t;
   set final;
   keep c1-c5;
run;
%mend ae005t;

%ae005t;

*redacted;
