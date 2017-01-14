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

	value trtpn
		0=0
		1=1
		2=2
		3=3;
run;


data adsl;
	set adam.adsl;
	where safety="Y";
	keep usubjid trtp trtpn agegrp agegrpn sex safety race racen;
run;

*------------------------------------;
*creating a numeric variable for sex;
*------------------------------------;

data adsl;
	set adsl;
	if sex="F" then sexn=2;
	else if sex="M" then sexn=1;
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

proc summary data=adsl2 completetypes nway;
	class trtpn/preloadfmt;
	class sexn/preloadfmt;
	where not missing(sexn);
	format trtpn trtpn. sexn sexn.;
	output out=sex_stats(drop=_type_ rename=(_freq_=count));
run;


*---------------------------------------;
*merge sex counts with trtcounts;
*---------------------------------------;

*numerator counts;

proc sort data=sex_stats;
	by trtpn ;
run;

*denominator counts;

proc sort data=trttotals;
	by trtpn ;
run;

data sex_stats2;
	merge sex_stats trttotals;
	by trtpn ;
run;

*-----------------------------------------------;
*calculate percentages;
*-----------------------------------------------;

data sex_stats3;
	set sex_stats2;
	length label statistic $50 cp $20;

	label="Gender";
	group=2;

	intord=sexn;

	statistic=put(sexn,sex_disp.);

	if count ne 0 then cp=put(count,3.)|| ' ('||put(round(count*100/trttotal,0.1),5.1)||'%)';
	else cp=put(count,3.);

/*	cp=put(count,3.)|| ' ('||put(round(count*100/trttotal,0.1),5.1)||'%)';*/
run;

proc sort data=sex_stats3;
	by  group intord label statistic;
run;

proc transpose data=sex_stats3 out=sex_stats4;
	by  group intord label statistic;
	var cp;
	id trtpn;
run;

data final_stats_sex;
	length c1-c6 $200;
	set sex_stats4;
	c1=label;
	c2=statistic;
	c3=_0;
	c4=_1;
	c5=_2;
	c6=_3;
	keep group  intord c1-c6 ;
run;

proc sort data=final_stats_sex;
	by  group intord;
run;


*macronisation;
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
			    group=2
			   );

%count_percent(var=race,
			   label=%str(Race),
			    group=3
			   );
