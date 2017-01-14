dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

libname adam "D:\Home\dev\compound2\data\shared\adam";

proc format;
	value trtpn
		1=1
		2=2
		3=3;
run;

data adsl;
	set adam.adsl;
	where randfl="Y";
run;

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
	n= mean= std= median= q1= q3= min= max=/autoname;
run;

data age_stats2;
	set age_stats;
	if not missing(age_n) then n=put(age_n,4.);
	if not missing(age_mean) then 	mean=put(age_mean,6.1);
	if not missing(age_stddev) then sd=put(age_stddev,7.2);
	if not missing(age_median) then median=put(age_median,7.2);
	if not missing(age_q1) then q1=put(age_q1,7.2);
	if not missing(age_q3) then q3=put(age_q3,7.2);
	if not missing(age_min) then min=put(age_min,6.1);
	if not missing(age_max) then max=put(age_max,6.1);
	
	meansd=strip(mean)||"("||strip(sd)||")";
	minmax=strip(min)||", "||strip(max);
	q1q3=strip(q1)||", "||strip(q3);

	drop age_:;
/*	drop age_n age_mean age_q1;*/
run;

proc transpose data=age_stats2 out=age_stats3(drop=_name_) label=statistic;
	by   trtpn;
	var n meansd median q1q3 minmax;
	label n="N-obs"
			meansd="Mean (SD)"
			median="Median"
			q1q3="Q1, Q3"
			minmax="Min, Max";
run;

data age_stats4;
	set age_stats3;
	length label $50;

	label="Age in years";
	group=1;

	select(statistic);
		when("N-obs") intord=1;
		when ("Mean (SD)") intord=2;
		when ("Median") intord=3;
		when ("Q1, Q3") intord=4;
		when ("Min, Max") intord=5;
	otherwise;
	end;
run;

proc sort data=age_stats4;
	by  group intord label statistic;
run;

proc transpose data=age_stats4 out=_final_stats_age;
	by  group intord label statistic;
	var col1;
	id trtpn;
run;

data final_stats_age;
	set _final_stats_age;
	length c1-c6 $200;
	c1=label;
	c2=statistic;
	c3=_1;
	c4=_2;
	c5=_3;
	keep group  intord c1-c5 ;
run;
