dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

libname adam "D:\Home\dev\cdisc_data_setup";

*=================================================================;
*descriptive statistics -basics;
*=================================================================;

*---------------------------;
*read input data;
*---------------------------;

data adsl;
	set adam.adsl;
	where safety="Y";
	keep usubjid trtpn trtp age heightbl weightbl trtdur;
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

proc summary data=adsl2  nway;
	class trtpn;
	where not missing(age);
	var age;
	output out=age_stats(drop=_type_ _freq_)
	n= mean= std= median= q1= q3= /autoname;
run;

data age_stats2;
	set age_stats;
	n=put(age_n,3.);
	mean=put(age_mean,5.1);
	sd=put(age_stddev,6.2);
	median=put(age_median,5.1);
	q1=put(age_q1,5.1);
	q3=put(age_q3,5.1);

	drop age_:;
/*	drop age_n age_mean age_q1;*/
run;

proc transpose data=age_stats2 out=age_stats3(drop=_name_) label=statistic;
	by  trtpn;
	var n mean sd median q1 q3;
	label n="N-obs"
			mean="Mean"
			sd="SD"
			median="Median"
			q1="Quartile 1"
			q3="Quartile 3";
run;


data age_stats4;
	set age_stats3;
	label="Age (in yrs)";
	group=1;
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

proc sort data=age_stats4;
	by group intord label statistic;
run;

proc transpose data=age_stats4 out=_final_stats_age;
	by group intord label statistic;
	var col1;
	id trtpn;
run;

data final_stats_age;
	set _final_stats_age;
	length c1-c6 $50;
	c1=label;
	c2=statistic;
	c3=_0;
	c4=_1;
	c5=_2;
	c6=_3;
	keep group  intord c1-c6 ;
run;


*=======================================================;
*stats for height;
*=======================================================;
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
	by ;
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
	length c1-c6 $200;
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
		var=weightbl,
		label=%str(Weight (in kgs)),
		group=5,
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
		group=7,
		n=4.,
		mean=6.2,
		sd=7.3,
		median=6.2,
		q1=6.2,
		q3=6.2
		);

data final;
	set final_stats_age final_stats_weightbl final_stats_trtdur;
run;

proc sort data=final;
	by  group intord;
run;

*==============================================;
*decimal alignment rule;
*==============================================;

/*100     3.    10.      15.*/
/*120.1   5.1   12.1     17.1*/
/* 10.34  6.2   13.2     18.2*/
/*110.563 7.3   14.3     19.3*/
