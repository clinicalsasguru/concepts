dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work kill nolist;
run;

*============================================================;
*counts and percnetages;
*============================================================;

*==========================================================;
*formats for categorical variables display;
*==========================================================;

proc format;
	value sex_num
		1=1
		2=2;

	value sex_disp
		1="Male"
		2="Female";

	value race_num
		1=1
		2=2
		3=3
		4=4;
	value race_disp
		1="Asian"
		2="American Indian or Alaska Native"
		3="Black or African American"
		4="White";
run;


data adsl;
	set adam.adsl;
	where saffl="Y";
	keep usubjid trt01pn trt01p agegr1n sex saffl race;
	if trt01pn=81 then trt01pn=3;
	else if trt01pn=54 then trt01pn=2;
	else if trt01pn=0 then trt01pn=1;
run;

*------------------------------------;
*creating a numeric variable for sex;
*------------------------------------;

data adsl;
	set adsl;
	if sex="F" then sex_num=2;
	else if sex="M" then sex_num=1;

	if race="ASIAN" then race_num=1;
	else if race="AMERICAN INDIAN OR ALASKA NATIVE" then race_num=2;
	else if race="BLACK OR AFRICAN AMERICAN" then race_num=3;
	else if race="WHITE" then race_num=4;
run;


*----------------------------------;
*duplicate data for total column;
*----------------------------------;

data adsl2;
	set adsl;
	output;
	trt01pn=4;
	output;
run;

proc sort data=adsl2;
	by trt01pn;
run;


*=================================================;
*obtain treatment counts-agegr1n wise;
*=================================================;

*-------------------------------------------;
*creating a dummy dataset for trt counts;
*-------------------------------------------;

data dummytrt;
	do trt01pn=1 to 4;
		trttotal=0;
		output;
	end;

run;


*----------------------------------------;
*obtian treatment - actual counts;
*----------------------------------------;
proc sql;
	create table trtcounts as
		select trt01pn, count(distinct usubjid) as trttotal
		from adsl2
		where saffl="Y" 
		group by trt01pn;
quit;
*------------------------------------------;
*merge dummy counts with actual counts;
*------------------------------------------;

proc sort data=dummytrt;
	by trt01pn;
run;

proc sort data=trtcounts;
	by trt01pn;
run;

data trtcounts;
	merge dummytrt trtcounts;
	by trt01pn;
run;

*=====================================================;
*obtaining actual counts for required variable;
*=====================================================;

proc summary data=adsl2  nway;
	class trt01pn;
	class sex_num;
	where not missing(sex_num);
	output out=sex_stats(drop=_type_ rename=(_freq_=count));
run;


*---------------------------------------;
*merge sex counts with trtcounts;
*---------------------------------------;

*numerator counts;

proc sort data=sex_stats;
	by trt01pn ;
run;

*denominator counts;

proc sort data=trtcounts;
	by trt01pn;
run;

data sex_stats2;
	merge sex_stats trtcounts;
	by trt01pn ;
run;

*-----------------------------------------------;
*calculate percentages;
*-----------------------------------------------;

data sex_stats3;
	set sex_stats2;
	length label statistic $50 cp $20;

	label="Gender";
	group=2;

	intord=sex_num;

	statistic=put(sex_num,sex_disp.);

	if count ne 0 then cp=put(count,3.)|| ' ('||put(round(count*100/trttotal,0.1),5.1)||'%)';
	else cp=put(count,3.);
run;

proc sort data=sex_stats3;
	by  group intord label statistic;
run;

proc transpose data=sex_stats3 out=sex_stats4;
	by  group intord label statistic;
	var cp;
	id trt01pn;
run;

data final_stats_sex;
	length c1-c6 $50;
	set sex_stats4;
	c1=label;
	c2=statistic;
	c3=_1;
	c4=_2;
	c5=_3;
	c6=_4;
	keep group  intord c1-c6 ;
run;

proc sort data=final_stats_sex;
	by  group intord;
run;

*===========================================;
*race stats;
*===========================================;

proc summary data=adsl2  nway completetypes;*additional logic;
	class trt01pn;
	class race_num/preloadfmt;*additional logic-preloadfmt;
	format race_num race_num.; *additional logic;
	where not missing(race_num);
	output out=race_stats(drop=_type_ rename=(_freq_=count));
run;


*---------------------------------------;
*merge race counts with trtcounts;
*---------------------------------------;

*numerator counts;

proc sort data=race_stats;
	by trt01pn ;
run;

*denominator counts;

proc sort data=trtcounts;
	by trt01pn;
run;

data race_stats2;
	merge race_stats trtcounts;
	by trt01pn ;
run;

*-----------------------------------------------;
*calculate percentages;
*-----------------------------------------------;

data race_stats3;
	set race_stats2;
	length label statistic $50 cp $20;

	label="Race";
	group=3;

	intord=race_num;

	statistic=put(race_num,race_disp.);

	if count ne 0 then cp=put(count,3.)|| ' ('||put(round(count*100/trttotal,0.1),5.1)||'%)';
	else cp=put(count,3.);
run;

proc sort data=race_stats3;
	by  group intord label statistic;
run;

proc transpose data=race_stats3 out=race_stats4;
	by  group intord label statistic;
	var cp;
	id trt01pn;
run;

data final_stats_race;
	length c1-c6 $50;
	set race_stats4;
	c1=label;
	c2=statistic;
	c3=_1;
	c4=_2;
	c5=_3;
	c6=_4;
	keep group  intord c1-c6 ;
run;

proc sort data=final_stats_race;
	by  group intord;
run;
