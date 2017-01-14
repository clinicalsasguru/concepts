dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

%let root=D:\Home\dev\compound6;

options mautosource sasautos=("&root.\lums", sasautos);

%assignlibs;

data pre_adae;
   set s_adam_i.adae;
   where saffl="Y" and trtemfl="Y" and aeser="N";
run;

data adsl;
   set s_adam_i.adsl(rename=(trt01an=trtan));
   where saffl="Y";
   keep usubjid trtan;
   output;
   trtan=3;
   output;
run;

*------------------------------------------------------;
*calcualate pre-event percentages;
*------------------------------------------------------;

proc sql;
   create table pre_counts as
   select aebodsys,aedecod,a.trtan, count(distinct usubjid) /denom*100 as percent
   from pre_adae as a
   left join (select trtan,count(distinct usubjid) as denom
               from adsl 
               where trtan in (1,2)
               group by trtan) as b
   on a.trtan=b.trtan
   group by aebodsys,aedecod,a.trtan
   having calculated percent gt 5;


   create table adae01 as 
   select a.* 
   from pre_adae as a
      inner join
      (select distinct aebodsys, aedecod
      from pre_counts) as b
   on a.aebodsys=b.aebodsys and a.aedecod=b.aedecod;
quit;

data adae;
   set adae01;
   output;
   trtan=3;
   output;
run;

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


proc sql noprint;
   select count(*) into :nobs from ae;
quit;

%macro ae010t;

%if &nobs gt 0 %then %do;

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
   from ae 
   group by trtan;
quit;

*---------------------------------------;
*soc level counts;
*---------------------------------------;

proc sql noprint;
   create table soc_count as
      select aebodsys, trtan,
      count(distinct usubjid) as count
      from ae 
      group by aebodsys,trtan;
quit;

*--------------------------------------;
*preferred term level counts;
*--------------------------------------;

proc sql noprint;
   create table pt_count as 
      select aebodsys,aedecod,trtan,
      count(distinct usubjid) as count
      from ae 
      group by aebodsys,aedecod,trtan;
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
   count=0;
   do trtan=0 to 3;
         output;
   end;
run;

*merge dummy counts with actual counts;
proc sort data=dummy;
   by aebodsys aedecod label trtan;
run;


proc sort data=counts out=counts2;
   by aebodsys aedecod label trtan;
run;

data counts3;
   merge dummy(in=a) counts2(in=b);
   by aebodsys aedecod label trtan;
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
   if missing(aebodsys) and missing(aedecod) then label=label;
   else if not missing(aebodsys) and missing(aedecod) then label=strip(aebodsys);
   else if not missing(aebodsys) and not missing(aedecod) then label="  "||strip(aedecod);
run;

*====================================================;
*transpose to obtain treatment as columns;
*====================================================;

proc sort data=counts5;
   by aebodsys aedecod label ;
run;

proc transpose data=counts5 out=trans1 prefix=trt;
   by aebodsys aedecod label;
   var count_percent;
   id trtan;
run;


data final;
   set trans1;
   keep aebodsys aedecod label trt1 trt2 trt3;
run;

proc datasets lib=work;
   modify final;
   rename label=c1 trt1=c2 trt2=c3 trt3=c4;
quit;


*============================================================================;
*report generation;
*============================================================================;

proc report data=final nowd headline headskip missing spacing=0;
   columns aebodsys aedecod c1  c2 c3 c4;
   define aebodsys/ order noprint;
   define aedecod/order noprint;
   define c1 /order "System Organ Class" "Preferred Term" width=50 flow;
    define c2/"Placebo" "(N=%cmpres(&n1))" width=18  ;
    define c3/"*redacted;" "(N=%cmpres(&n2))" width=18  ;
    define c4/"Total" "(N=%cmpres(&n3))" width=18  ;
   break after aebodsys/skip;

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

data compt.ae010t;
   set final;
   keep c1-c4;
run;

%mend ae010t;

%ae010t;

*redacted;
