dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

%let root=D:\Home\dev\compound6;

options mautosource sasautos=("&root.\lums", sasautos);


%assignlibs;


proc format;
   value trt01an
      1=1
      2=2
      3=3;

   value sexn
      0=0
	   1=1
	   2=2;

   value aracen
      0=0
      1=1
      2=2
      3=3
      4=4;

	value sexn_disp
      0="Missing"
	   1="Male"
	   2="Female";

   value aracen_disp
      0="Missing"
      1="White"
      2="Black or African American"
      3="Asian"
      4="Other";
run;

*---------------------------;
*read input data;
*---------------------------;

data adsl;
	set s_adam_i.adsl;
	where /*fasfl="Y" and */not missing(trt01an);
run;

*------------------------------------;
*creating a numeric variable for agegr;
*------------------------------------;

data adsl;
	set adsl;
	keep usubjid trt01an trt01a sexn  aracen age weightbl  heightbl  bmibl fasfl;
   if missing(sexn) then sexn=0;
   if missing(aracen) then aracen=0;
 run;


*----------------------------------;
*duplicate data for total column;
*----------------------------------;

data adsl2;
	set adsl;
	output;
	trt01an=3;
	output;
run;

*=================================================;
*obtain treatment counts- wise;
*=================================================;

*-------------------------------------------;
*creating a dummy dataset for trt counts;
*-------------------------------------------;

data dummytrt;
	trttotal=0;
	do trt01an=1 to 3;
		output;
	end;
run;


*----------------------------------------;
*obtian treatment - actual counts;
*----------------------------------------;
proc sql;
	create table trttotals_pre as
		select trt01an, count(distinct usubjid) as trttotal
		from adsl2
		group by trt01an;
quit;

*------------------------------------------;
*merge dummy counts with actual counts;
*------------------------------------------;

proc sort data=dummytrt;
	by trt01an ;
run;

proc sort data=trttotals_pre;
	by trt01an ;
run;

data trttotals;
	merge dummytrt trttotals_pre;
	by trt01an ;
run;


*=====================================================;
*obtaining actual counts for required variable;
*=====================================================;
*macro for counts and percentages;

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
*merge sex counts with trtcounts;
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

%count_percent(var=sexn,
			   label=%str(Sex),
			    group=1
			   );


%count_percent(var=aracen,
			   label=%str(Race),
			    group=2
			   );


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

%descriptive(
		var=age,
		label=%str(Age (Years)),
		group=3,
		n=3.,
		mean=5.1,
		sd=5.1,
		min=3.,
		median=5.1,
		max=3.
		);


%descriptive(
		var=weightbl,
		label=%str(Weight (Kg)),
		group=4,
		n=3.,
		mean=6.2,
		sd=6.2,
		min=5.1,
		median=6.2,
		max=5.1
		);

%descriptive(
		var=heightbl,
		label=%str(Height (cm)),
		group=5,
		n=3.,
		mean=5.1,
		sd=5.1,
		min=3.,
		median=5.1,
		max=3.
		);

%descriptive(
		var=bmibl,
		label=%str(BMI (kg/m^2)),
		group=6,
		n=3.,
		mean=6.2,
		sd=6.2,
		min=5.1,
		median=6.2,
		max=3.
		);


options nomprint nosymbolgen;
data final;
	set final_stats_:;
run;

proc sort data=final;
	by group intord;
run;


*====================================================================;
*macro variables of treatment totals for column headers;
*====================================================================;

proc sql noprint;
	select count(distinct usubjid) into :n1 from adsl2 where trt01an=1;
	select count(distinct usubjid) into :n2 from adsl2 where trt01an=2;
	select count(distinct usubjid) into :n3 from adsl2 where trt01an=3;
quit;

%put number of subjects in trt1 : &n1;
%put number of subjects in trt2 : &n2;
%put number of subjects in trt3 : &n3;


%macro aligdec(indsn=,invar=);
   %local i j ;
    %let j = 1 ;
    %do %while(%scan(&invar,&j) ne );
        %let i = %scan(&invar,&j); 
   data _alig0;
      set &indsn end=eof;
      retain maxint 0;
      if index(&i, '(') then dot = min(index(&i, '.'),index(&i, '('));
      else dot = index(&i, '.');
      if dot ne 0 then lenint = length(trim(left(substr(&i, 1, (dot - 1)))));
      else lenint = length(trim(left(&i)));
      maxint = max(maxint, lenint);
      if eof then call symput("maxint",put(maxint,best.));
   run;

   data &indsn (drop=&i);
      set _alig0;
      length &i.c $15;
      if not missing(&i) then do;
        diffint = &maxint - lenint - 1;
      if diffint >= 0 then do;
        &i.c = repeat(" ", diffint)||trim(left(&i));
      end;
        else do;
          &i.c = trim(left(&i));
        end;
      end;
   run;

   %let j = %eval(&j+1) ;
   %end ;
%mend aligdec;
%aligdec (indsn=final,invar=%str(c3 c4 c5));

proc datasets library=work;
   modify final;
   rename c3c=c3 c4c=c4 c5c=c5;
run;

* Printing the final output;
/*%*redacted;*/
proc report data = final center headline headskip nowd split='~' missing spacing=0;
column    c1 group c2 intord c3 c4 c5  ;
	 define group/order noprint;
	 define intord /order noprint;
	 define c1/width=30 "Parameter" order  width = 23 flow spacing = 0;
	 define c2/width=30 "Category/~Statistic"  width = 27 flow spacing = 0;
    define c3/"Placebo" "(N=%cmpres(&n1))" width=18  ;
    define c4/"*redacted;" "(N=%cmpres(&n2))" width=18  ;
    define c5/"Total" "(N=%cmpres(&n3))" width=18  ;
break after group / skip;
run;

