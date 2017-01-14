
*redacted;

options validvarname=upcase varlenchk=nowarn;

%assignlibs;

data adcm01;
   set s_adam_i.adex;
   where saffl="Y" ;
run;

proc sql noprint;
   select count(*) into :nobs from adcm01;
quit;

proc format;
   value trt01an
      1=1
      2=2
      3=3;
run;

*=========================================================================;
*macros for categorical and conitnuous data;
*=========================================================================;


%macro count_percent(var=,
                label=,
                group=
               );

*-------------------------------------------;
*obtain non-missing values as denominator;
*-------------------------------------------;

proc sql;
   create table &var._denoms as
   select trt01an,count(distinct usubjid) as trttotal
   from adsl2 
   where &var not in (., 0)
   group by trt01an
   order by trt01an;
quit;

data &var.denoms;
   merge dummytrt &var._denoms;
   by trt01an;
run;

proc summary data=adsl2 completetypes nway;
   class trt01an /preloadfmt;
   class &var./preloadfmt;
   where not missing(&var.);
   format  &var. &var.. trt01an trt01an.;
   output out=&var._stats(drop=_type_ rename=(_freq_=count));
run;

*---------------------------------------;
*merge counts with trtcounts;
*---------------------------------------;

*numerator counts;

proc sort data=&var._stats;
   by trt01an ;
run;

*denominator counts;

proc sort data=&var._denoms;
   by trt01an ;
run;

data &var._stats2;
   merge &var._stats &var._denoms;
   by trt01an ;
run;

*-----------------------------------------------;
*calculate percentages;
*-----------------------------------------------;

data &var._stats3;
   set &var._stats2;
   length label statistic $50 cp $20;

   label="&label.";
   group=&group.;

   intord=&var.;

   statistic=put(&var.,&var._disp.);

   if count ne 0 then cp=put(count,3.)|| ' ('||put(round(count*100/trttotal,0.1),5.1)||'%)';
   else cp=put(count,3.);

run;

proc sort data=&var._stats3;
   by  group intord label statistic;
run;

proc transpose data=&var._stats3 out=&var._stats4;
   by  group intord label statistic;
   var cp;
   id trt01an;
run;

data final_stats_&var.;
   length c1-c5 $200;
   set &var._stats4;
   if intord=0 and compress(_3)="0" then delete;
   c1=label;
   c2=statistic;
   c3=_1;
   c4=_2;
   c5=_3;
   
   keep group  intord c1-c5 ;
run;

proc sort data=final_stats_&var.;
   by  group intord;
run;
%mend;


*=======================================================;
*descriptive stats for numeric variables;
*=======================================================;
%macro descriptive(
      var=,
      label=,
      group=,
      n=,
      mean=,
      sd=,
      min=,
      median=,
      max=
      );

proc summary data=adsl2  nway;
   class trt01an;
   where not missing(&var.);
   var &var.;
   output out=&var._stats(drop=_type_ _freq_)
   n= mean= std= min= median= max= /autoname;
run;

data &var._stats2;
   set &var._stats;
   n=put(&var._n,&n.);
   mean=put(&var._mean,&mean.);
   sd=put(&var._stddev,&sd.);
   min=put(&var._min,&min.);
   median=put(&var._median,&median.);
   max=put(&var._median,&max.);

   drop &var._:;
run;

proc transpose data=&var._stats2 out=&var._stats3(drop=_name_) label=statistic;
   by   trt01an;
   var n mean sd min median max;
   label n="n"
         mean="Mean"
         sd="SD"
         min="Min"
         median="Median"
         max="Max";
run;

data &var._stats4;
   set &var._stats3;
   length label $50;

   label="&label.";
   group=&group.;

   select(statistic);
      when("n") intord=1;
      when ("Mean") intord=2;
      when ("SD") intord=3;
      when ("Min") intord=4;
      when ("Median") intord=5;
      when ("Max") intord=6;
   otherwise;
   end;
run;

proc sort data=&var._stats4;
   by  group intord label statistic;
run;

proc transpose data=&var._stats4 out=_final_stats_&var.;
   by  group intord label statistic;
   var col1;
   id trt01an;
run;

data final_stats_&var.;
   set _final_stats_&var.;
   length c1-c5 $200;
   c1=label;
   c2=statistic;
   c3=_1;
   c4=_2;
   c5=_3;
   keep group  intord c1-c5 ;
run;

%mend;

*---------------------------;
*read input data;
*---------------------------;

data adsl;
   set s_adam_i.adsl;
   where saffl="Y" and not missing(trt01an);
run;


data adsl2;
   set adsl;
   output;
   trt01an=3;
   output;
run;

