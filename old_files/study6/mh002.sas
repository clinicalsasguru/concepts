
*redacted;

options validvarname=upcase varlenchk=nowarn;

%assignlibs;


data admh01;
   set s_adam_i.admh;
   where saffl="Y" ;
run;

proc sql noprint;
   select count(*) into :nobs from admh01;
quit;

%macro mh002t;

%if &nobs gt 0 %then %do;
data admh;
   set admh01;
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

data ae_pre01;
   set admh;
   if missing(mhdecod) then mhdecod="Not coded";
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
   select "Overall" as label length=200,
   trtan,
   count(distinct usubjid) as count 
   from ae 
   group by trtan;
quit;

*--------------------------------------;
*preferred term level counts;
*--------------------------------------;

proc sql noprint;
   create table pt_count as 
      select mhdecod,trtan,
      count(distinct usubjid) as count
      from ae 
      group by mhdecod,trtan;
quit;

*---------------------------------;
*put all counts together;
*---------------------------------;

data counts;
   set sub_count pt_count;
run;

*=========================================================;
*create zero counts if an event is not present in a trt;
*=========================================================;

*get all the available soc-s and pt-s;

proc sort data=counts out=dummy_pre(keep=mhdecod label) nodupkey;
   by mhdecod label;
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
   by mhdecod label trtan;
run;


proc sort data=counts out=counts2;
   by mhdecod label trtan;
run;

data counts3;
   merge dummy(in=a) counts2(in=b);
   by mhdecod label trtan;
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
   if missing(mhdecod) then label=label;
   else if not missing(mhdecod) then label=strip(mhdecod);
run;

*====================================================;
*transpose to obtain treatment as columns;
*====================================================;

proc sort data=counts5;
   by mhdecod label ;
run;

proc transpose data=counts5 out=trans1 prefix=trt;
   by mhdecod label;
   var count_percent;
   id trtan;
run;

data trans1;
   set trans1;
   totfreq=input(scan(trt3,2,'(%)'),best.);
run;


data final;
   set trans1;
   keep mhdecod label trt1 trt2 trt3 totfreq ord;
   if label="Overall" then ord=1;
   else ord=2;
run;

proc sort data=final;
   by ord descending totfreq mhdecod label;
run;

proc datasets lib=work;
   modify final;
   rename label=c1 trt1=c2 trt2=c3 trt3=c4;
quit;


*============================================================================;
*report generation;
*============================================================================;

%*redacted;

proc report data=final nowd headline headskip missing spacing=0;
   columns ord totfreq c1  c2 c3 c4;
   define ord/order noprint order=data;
   define totfreq/order noprint order=data;
   define c1 /order "System Organ Class" "Preferred Term" width=50 flow;
    define c2/"Placebo" "(N=%cmpres(&n1))" width=18  ;
    define c3/"*redacted;" "(N=%cmpres(&n2))" width=18  ;
    define c4/"Total" "(N=%cmpres(&n3))" width=18  ;
   break after ord/skip;

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

data compt.mh002t;
   set final;
   keep c1-c4;
run;

%mend mh002t;

%mh002t;

*redacted;
*redacted;

*redacted;(prog=&prog_loc./ae010t.log);
*redacted;(prog=&prog_loc./ae010t.sas, standalone=Y);

*redacted;
