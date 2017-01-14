
*redacted;

options validvarname=upcase varlenchk=nowarn;

%assignlibs;


data adeg01;
   set s_adam_i.adeg;
   where saffl="Y" and paramn=6 and avisitn in (6:30);
run;

data adeg;
   set adeg01;
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

proc sort data=adeg out=eg_pre;
   by usubjid trtan;
run;

data eg;
   merge eg_pre(in=a) adsl(in=b);
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
   select 0 as avalgr1n,avisitn,avisit,atptn,atpt,trtan,
   count(distinct usubjid) as count 
   from eg 
   where not missing(avalgr1n)
   group by avisitn,avisit,atptn,atpt,trtan;

   create table denoms_pre as 
      select distinct avisitn, avisit, atptn, atpt
      from sub_count;
quit;

*---------------------------------------;
*abnormality level counts;
*---------------------------------------;

proc sql noprint;
   create table abn_count as
      select avisitn,avisit,atptn,atpt,avalgr1n,trtan,
      count(distinct usubjid) as count
      from eg 
      group by avisitn,avisit,atptn,atpt,avalgr1n,trtan;
quit;

*---------------------------------;
*put all counts together;
*---------------------------------;

data counts;
   set sub_count abn_count;
run;

*=========================================================;
*create zero counts if an event is not present in a trt;
*=========================================================;

proc sort data=counts out=dummy_pre(keep=avisitn avisit atptn atpt ) nodupkey;
   by avisitn avisit atptn atpt;
run;
*create a row for each treatment and severity;

data dummy;
   set dummy_pre;
   count=0;
   do trtan=1 to 3;
      do avalgr1n=0 to 4;
         output;
      end;
   end;
run;

*merge dummy counts with actual counts;
proc sort data=dummy;
   by avisitn avisit atptn atpt avalgr1n trtan;
run;


proc sort data=counts out=counts2;
   by avisitn avisit atptn atpt avalgr1n trtan;
run;

data counts3;
   merge dummy(in=a) counts2(in=b);
   by avisitn avisit atptn atpt avalgr1n trtan;
run;

*---------------------------------------;
*obtain percentages;
*---------------------------------------;

*merge counts with trttotals dataset, to get denominator values(trt totals);

*----------------------------------------------------;
*create denominators;
*----------------------------------------------------;

data dummy_denoms;
   set denoms_pre;
   trttotal=0;
   do trtan=1 to 3;
      output;
   end;  
run;
    
proc sort data =dummy_denoms;
   by avisitn avisit atptn atpt trtan;
run;

proc sort data =sub_count;
   by avisitn avisit atptn atpt trtan;
run;

data trttotals;
   merge dummy_denoms sub_count(rename=(count=trttotal));
   by avisitn avisit atptn atpt trtan;
run;

proc sort data=trttotals;
   by avisitn avisit atptn atpt;
run;

proc sort data=counts3;
   by avisitn avisit atptn atpt trtan;
run;

data counts4;
   merge counts3(in=a) trttotals(in=b);
   by avisitn avisit atptn atpt trtan;
   if a;
run;

data counts4;
   set counts4;
   length count_percent $30;
   if avalgr1n ne 0 then 
      do;
         if count ne 0 then count_percent=put(count,3.)||" ("||put(count/trttotal*100,5.1)||"%)";
         else count_percent=put(count,3.);
      end;
   else count_percent=put(count,3.);
run;

*---------------------------------------------;
*create the label column;
*---------------------------------------------;

data counts5;
   set counts4;
   length c2 $30;
   if avalgr1n=0 then c2="n";
   else if avalgr1n=1 then c2="<=450";
   else if avalgr1n=2 then c2=">450";
   else if avalgr1n=3 then c2=">480";
   else if avalgr1n=4 then c2=">500";
run;

*====================================================;
*transpose to obtain treatment as columns;
*====================================================;

proc sort data=counts5;
   by avisitn avisit atptn atpt avalgr1n c2 ;
run;

proc transpose data=counts5 out=trans1 prefix=trt;
   by avisitn avisit atptn atpt avalgr1n c2;
   var count_percent;
   id trtan;
run;


data final;
   set trans1;
   keep avisitn avisit atptn atpt avalgr1n c2 trt1 trt2 trt3;
run;

proc datasets lib=work;
   modify final;
   rename avisit=c1 trt1=c3 trt2=c4 trt3=c5;
quit;


*============================================================================;
*report generation;
*============================================================================;

%*redacted;

proc report data=final nowd headline headskip missing spacing=0;
   columns avisitn atptn c1 avalgr1n c2 c3 c4 c5;
   define avisitn/ order noprint;
   define avalgr1n/order noprint;
   define atptn/order noprint;
   define c1 /order "Analysis Visit" width=40 flow;
   define c2/ "Maximum value" "(msec) [1]" width=20;
   define c3/ "*redacted;" "(N=%cmpres(&n1))" width=18  ;
   define c4/"Placebo" "(N=%cmpres(&n2))" width=18  ;
   define c5/"Total" "(N=%cmpres(&n3))" width=18  ;
   break after avisitn/skip;

run;


data compt.eg001t;
   set final;
   keep c1-c5;
run;


*redacted;
*redacted;

*redacted;(prog=&prog_loc./eg001t.log);
*redacted;(prog=&prog_loc./eg001t.sas, standalone=Y);

*redacted;
