dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

libname adam "D:\Home\dev\cdisc_data_setup";

proc format;
	value trtpn
		0=0
		1=1
		2=2
		3=3;
run;

data adsl;
	set adam.adsl;
	where safety="Y";
run;

data adlb_pre;
	set adam.adlbc;
	where safety="Y" and visit ne "UNSCHEDULED";
run;

data adlb;
	merge adlb_pre(in=a) adsl(in=b);
	by usubjid;
	if a and b;
run;

data adlb;
	set adlb;
	output;
	trtpn=3;
	output;
run;

data adsl;
	set adsl;
	output;
	trtpn=3;
	output;
run;

proc sort data=adlb out=adlb_;
	by visitnum visit lbtestcd lbtest;
run;
	

proc summary data=adlb_  nway completetypes;
	by visitnum visit lbtestcd lbtest;
	class trtpn/preloadfmt;
	format trtpn trtpn.;
	where not missing(lbstresn) ;
	var lbstresn;
	output out=_stats(drop=_type_ _freq_)
	n= mean= stddev= median= q1= q3= /autoname;
run;

data _stats2;
	set _stats;

	length n mean median sd q1 q3 $20;

	by visitnum visit lbtestcd lbtest;

	if strip(lbtestcd) not in ("ALB" "ALP" "ALT") then do;
		if not missing(lbstresn_n) then n=put(lbstresn_n,5.);
		if not missing(lbstresn_mean) then mean=put(lbstresn_mean,7.1);

		if not missing(lbstresn_stddev) then sd=put(lbstresn_stddev,8.2);
		else if lbstresn_n=1 then sd="  -";

		if not missing(lbstresn_median) then median=put(lbstresn_median,7.1);
		if not missing(lbstresn_q1) then q1=put(lbstresn_q1,7.1);
		if not missing(lbstresn_q3) then q3=put(lbstresn_q3,7.1);
	end;

	else if strip(lbtestcd) in ("ALB" "ALP" "ALT") then do;
		if not missing(lbstresn_n) then n=put(lbstresn_n,5.);
		if not missing(lbstresn_mean) then mean=put(lbstresn_mean,8.2);

		if not missing(lbstresn_stddev) then sd=put(lbstresn_stddev,9.3);
		else if lbstresn_n=1 then sd="  -";

		if not missing(lbstresn_median) then median=put(lbstresn_median,8.2);
		if not missing(lbstresn_q1) then q1=put(lbstresn_q1,8.2);
		if not missing(lbstresn_q3) then q3=put(lbstresn_q3,8.2);

	end;

	drop lbstresn_:;
run;
/*
proc sql;
	select lbtestcd,max(lbstresn) as maxval, 
	min(lbstresn) as minval
	from adlb
	group by lbtestcd;
quit
*/

proc transpose data=_stats2 out=_stats3(drop=_name_) label=statistic;
	by visitnum visit lbtestcd lbtest  trtpn;
	var n mean sd median q1 q3;
	label n="N-obs"
			mean="Mean"
			sd="SD"
			median="Median"
			q1="Quartile 1"
			q3="Quartile 3";
run;

data _stats4;
	set _stats3;
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

proc sort data=_stats4;
	by visitnum visit lbtestcd lbtest intord statistic;
run;

proc transpose data=_stats4 out=_final_stats_;
	by  visitnum visit lbtestcd lbtest intord statistic;
	var col1;
	id trtpn;
run;

data final_stats_;
	set _final_stats_;
	length c1-c7 $200;
	c1=visit;
	c2=lbtest;
	c3=statistic;
	c4=_0;
	c5=_1;
	c6=_2;
	c7=_3;
	keep intord c1-c7 visitnum visit lbtestcd;
run;


proc sort data=final_stats_ out=final_temp;
	by visitnum visit lbtestcd c2 intord;
run;
*--------------------------------------------------------------;
*macro variables for column headers in proc report;
*--------------------------------------------------------------;

proc sql noprint;
	select count(distinct usubjid) into :n1 from adsl where trtpn=0;
	select count(distinct usubjid) into :n2 from adsl where trtpn=1;
	select count(distinct usubjid) into :n3 from adsl where trtpn=2;
	select count(distinct usubjid) into :n4 from adsl where trtpn=3;

quit;

data final;
	set final_temp;
	if mod(_n_,24)=1 then page+1;
run;

options ls=133 ps=47;

proc report data=final nowd headline headskip spacing=0 nocenter;
	columns page visitnum  c1 lbtestcd c2 intord c3-c7;
	define page/order noprint;
	define visitnum/order noprint;
	define lbtestcd/order noprint;
	define intord/order noprint;
	define c1/order "Visit" width=15 flow;
	define c2/order "Lab Test" width=30 flow;
	define c3/"Statistic" width=12;
	define c4/"Placebo" "N=(%cmpres(&n1))" width=20  ;
	define c5/"Xanomeline" "54 mg" "N=(%cmpres(&n2))" width=20 ;
	define c6/"Xanomeline" "81 mg" "N=(%cmpres(&n3))" width=20 ;
	define c7/"Total"  "N=(%cmpres(&n4))" width=16 ;

	break after visitnum/skip;
	break after c2/skip;

	break after page/page;

	compute before _page_;
		line @1 "Title1";
		line @1 "Title2";
		line @ 1 "";
		line @1 133*'-';
	endcomp;

	compute after _page_;
		line @1 133*'-';
		line @1 "Footnote1";
		line @1 "Footnote2";
		line @1 "Footnote3";
		line @1"";
	endcomp;
run;



