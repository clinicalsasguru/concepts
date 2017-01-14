dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work kill nolist;
run;

libname adam "D:\Home\dev\cdisc_data_setup";
*============================================================;
*counts and percnetages;
*============================================================;

*==========================================================;
*formats for categorical variables display;
*==========================================================;

proc format;
	value sexn
		1=1
		2=2;

	value sex_disp
		1="Male"
		2="Female";

	value racen
		1=1
		2=2
		3=3
		4=4
		5=5
		6=6;

	value race_disp
		1="Caucasian"
		2="African"
		3="White"
		4="Asian"
		5="Hispanic"
		6="Other";

	value bmiblgrpn
	1=1
	2=2
	3=3;

	value bmiblgrp_disp
	1="<25"
	2="25-<30"
	3=">=30";

	value agegrpn
	1=1
	2=2
	3=3;

	value agegrp_disp
	1="<65"
	2="65-80"
	3=">80";

	value trtpn
		0=0
		1=1
		2=2
		3=3;
run;


data adsl;
	set adam.adsl;
	where safety="Y";
	keep usubjid trtp trtpn agegrp agegrpn age
	sex safety race racen bmibl bmiblgrp trtdur weightbl heightbl;
run;

*------------------------------------;
*creating a numeric variable for sex;
*------------------------------------;

data adsl;
	set adsl;
	if sex="F" then sexn=2;
	else if sex="M" then sexn=1;

	if bmiblgrp="<25" then bmiblgrpn=1;
	else if bmiblgrp="25-<30" then bmiblgrpn=2;
	else if bmiblgrp=">=30" then bmiblgrpn=3;
run;


*----------------------------------;
*duplicate data for total column;
*----------------------------------;

data adsl2;
	set adsl;
	output;
	trtpn=3;
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
	do trtpn=0 to 3;
		output;
	end;
run;


*----------------------------------------;
*obtian treatment - actual counts;
*----------------------------------------;
proc sql;
	create table trttotals_pre as
		select trtpn, count(distinct usubjid) as trttotal
		from adsl2
		where safety="Y" 
		group by trtpn;
quit;

*------------------------------------------;
*merge dummy counts with actual counts;
*------------------------------------------;

proc sort data=dummytrt;
	by trtpn ;
run;

proc sort data=trttotals_pre;
	by trtpn ;
run;

data trttotals;
	merge dummytrt trttotals_pre;
	by trtpn ;
run;

*=====================================================;
*obtaining actual counts for required variable;
*=====================================================;
*macro for counts and percentages;

%macro count_percent(var=,
					 label=,
					 group=
					);
proc summary data=adsl2 completetypes nway;
	class trtpn/preloadfmt;
	class &var.n/preloadfmt;
	where not missing(&var.n);
	format trtpn trtpn. &var.n &var.n.;
	output out=&var._stats(drop=_type_ rename=(_freq_=count));
run;


*---------------------------------------;
*merge sex counts with trtcounts;
*---------------------------------------;

*numerator counts;

proc sort data=&var._stats;
	by trtpn ;
run;

*denominator counts;

proc sort data=trttotals;
	by trtpn ;
run;

data &var._stats2;
	merge &var._stats trttotals;
	by trtpn ;
run;

*-----------------------------------------------;
*calculate percentages;
*-----------------------------------------------;

data &var._stats3;
	set &var._stats2;
	length label statistic $50 cp $20;

	label="&label.";
	group=&group.;

	intord=&var.n;

	statistic=put(&var.n,&var._disp.);

	if count ne 0 then cp=put(count,3.)|| ' ('||put(round(count*100/trttotal,0.1),5.1)||'%)';
	else cp=put(count,3.);

/*	cp=put(count,3.)|| ' ('||put(round(count*100/trttotal,0.1),5.1)||'%)';*/
run;

proc sort data=&var._stats3;
	by  group intord label statistic;
run;

proc transpose data=&var._stats3 out=&var._stats4;
	by  group intord label statistic;
	var cp;
	id trtpn;
run;

data final_stats_&var.;
	length c1-c6 $200;
	set &var._stats4;
	c1=label;
	c2=statistic;
	c3=_0;
	c4=_1;
	c5=_2;
	c6=_3;
	keep group  intord c1-c6 ;
run;

proc sort data=final_stats_&var.;
	by  group intord;
run;
%mend;

%count_percent(var=sex,
			   label=%str(Gender),
			    group=3
			   );

%count_percent(var=race,
			   label=%str(Race),
			    group=4
			   );

%count_percent(var=bmiblgrp,
			   label=%str(Baseline BMI group),
			    group=6
			   );
%count_percent(var=agegrp,
			   label=%str(Age Group Category),
			    group=2
			   );

%macro descriptive(
		var=,
		label=,
		group=,
		n=,
		mean=,
		sd=,
		median=,
		q1=,
		q3=
		);

proc summary data=adsl2  nway;
	class trtpn;
	where not missing(&var.);
	var &var.;
	output out=&var._stats(drop=_type_ _freq_)
	n= mean= std= median= q1= q3= /autoname;
run;

data &var._stats2;
	set &var._stats;
	n=put(&var._n,&n.);
	mean=put(&var._mean,&mean.);
	sd=put(&var._stddev,&sd.);
	median=put(&var._median,&median.);
	q1=put(&var._q1,&q1.);
	q3=put(&var._q3,&q3.);

	drop &var._:;
/*	drop age_n age_mean age_q1;*/
run;

proc transpose data=&var._stats2 out=&var._stats3(drop=_name_) label=statistic;
	by   trtpn;
	var n mean sd median q1 q3;
	label n="N-obs"
			mean="Mean"
			sd="SD"
			median="Median"
			q1="Quartile 1"
			q3="Quartile 3";
run;

data &var._stats4;
	set &var._stats3;
	length label $50;

	label="&label.";
	group=&group.;

	select(statistic);
		when("N-obs") intord=1;
		when ("Mean") intord=2;
		when ("SD") intord=3;
		when ("Median") intord=5;
		when ("Quartile 1") intord=4;
		when ("Quartile 3") intord=6;
	otherwise;
	end;
run;

proc sort data=&var._stats4;
	by  group intord label statistic;
run;

proc transpose data=&var._stats4 out=_final_stats_&var.;
	by  group intord label statistic;
	var col1;
	id trtpn;
run;

data final_stats_&var.;
	set _final_stats_&var.;
	length c1-c5 $200;
	c1=label;
	c2=statistic;
	c3=_0;
	c4=_1;
	c5=_2;
	c6=_3;
	keep group  intord c1-c6 ;
run;

%mend;

%descriptive(
		var=age,
		label=%str(Age (in Years)),
		group=1,
		n=4.,
		mean=6.2,
		sd=7.3,
		median=6.2,
		q1=6.2,
		q3=6.2
		);
%descriptive(
		var=weightbl,
		label=%str(Weight (in kgs)),
		group=8,
		n=4.,
		mean=6.2,
		sd=7.3,
		median=6.2,
		q1=6.2,
		q3=6.2
		);

%descriptive(
		var=trtdur,
		label=%str(Treatement duration),
		group=10,
		n=4.,
		mean=6.2,
		sd=7.3,
		median=6.2,
		q1=6.2,
		q3=6.2
		);

options mprint symbolgen;
%descriptive(
		var=bmibl,
		label=%str(Baseline BMI),
		group=7,
		n=4.,
		mean=6.2,
		sd=7.3,
		median=6.2,
		q1=6.2,
		q3=6.2
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
	select count(distinct usubjid) into :n1 from adsl2 where trtpn=0;
	select count(distinct usubjid) into :n2 from adsl2 where trtpn=1;
	select count(distinct usubjid) into :n3 from adsl2 where trtpn=2;
	select count(distinct usubjid) into :n4 from adsl2 where trtpn=3;
quit;

%put number of subjects in trt0 : &n1;
%put number of subjects in trt1 : &n2;
%put number of subjects in trt2 : &n3;
%put number of subjects in trt3 : &n4;

options ps=47 ls=133;

proc report data=final nowd headline headskip spacing=0;
columns  c1 group c2 intord c3 c4 c5 c6;
	define group/order noprint;
	define intord /order noprint;
	define c1/width=30 "" order;
	define c2/width=30 "";
	define c3/"Placebo" "N=(%cmpres(&n1))" width=18 center ;
	define c4/"Xanomeline" "54 mg" "N=(%cmpres(&n2))" width=18 center;
	define c5/"Xanomeline" "81 mg" "N=(%cmpres(&n3))" width=18 center;
	define c6/"Total" "N=(%cmpres(&n4))" width=19 center;

	break after group/skip;

	compute before _page_;
	line @1 "Title1";
	line @1 "Title2";
	line @1 "";
	line @1 133*'-';
	endcomp;

	compute after _page_;
	line @1 133*'-';
	line @1 "footnote1";
	line @1 "footnote2";
	endcomp;

run;
