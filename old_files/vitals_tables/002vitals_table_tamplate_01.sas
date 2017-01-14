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
	keep usubjid safety trtpn;
run;

data advs_pre;
	set adam.advs;
	where safety="Y" and (avisitn gt 0 or (avisitn=0 and ady=1));
run;

data advs;
	merge advs_pre(in=a) adsl(in=b);
	by usubjid;
	if a and b;
run;

data advs;
	set advs;
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

proc sort data=advs out=advs_;
	by avisitn avisit atptn atpt paramn param;
run;
	

proc summary data=advs_  nway completetypes;
	by avisitn avisit atptn atpt paramn param;
	class trtpn/preloadfmt;
	format trtpn trtpn.;
	where not missing(aval) ;
	var aval;
	output out=_stats(drop=_type_ _freq_)
	n= mean= stddev= median= q1= q3= /autoname;
run;

data _stats2;
	set _stats;

	length n mean median sd q1 q3 $20;

	by avisitn avisit atptn atpt paramn param;

	if paramn in (1 2 3 6) then do;
		if not missing(aval_n) then n=put(aval_n,3.);
		if not missing(aval_mean) then mean=put(aval_mean,5.1);

		if not missing(aval_stddev) then sd=put(aval_stddev,6.2);
		else if aval_n=1 then sd="  -";

		if not missing(aval_median) then median=put(aval_median,5.1);
		if not missing(aval_q1) then q1=put(aval_q1,5.1);
		if not missing(aval_q3) then q3=put(aval_q3,5.1);
	end;

	else if paramn in (4 5) then do;
		if not missing(aval_n) then n=put(aval_n,3.);
		if not missing(aval_mean) then mean=put(aval_mean,6.2);

		if not missing(aval_stddev) then sd=put(aval_stddev,7.3);
		else if aval_n=1 then sd="  -";

		if not missing(aval_median) then median=put(aval_median,6.2);
		if not missing(aval_q1) then q1=put(aval_q1,6.2);
		if not missing(aval_q3) then q3=put(aval_q3,6.2);

	end;

	drop aval_:;
run;
/*
proc sql;
	select paramn,max(aval) as maxval, 
	min(aval) as minval
	from advs
	group by paramn;
quit
*/

proc transpose data=_stats2 out=_stats3(drop=_name_) label=statistic;
	by avisitn avisit atptn atpt paramn param  trtpn;
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
	by avisitn avisit atptn atpt paramn param intord statistic;
run;

proc transpose data=_stats4 out=_final_stats_;
	by  avisitn avisit atptn atpt paramn param intord statistic;
	var col1;
	id trtpn;
run;

data final_stats_;
	set _final_stats_;
	length c1-c8 $200;
	c1=avisit;
	c2=param;
	c3=atpt;
	c4=statistic;
	c5=_0;
	c6=_1;
	c7=_2;
	c8=_3;
	keep intord c1-c8 avisitn avisit atptn atpt paramn;
run;


proc sort data=final_stats_ out=final_temp;
	by avisitn avisit paramn c2 atptn c3 intord;
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

%put &n1 &n2 &n3 &n4;

data final;
	set final_temp;
	if mod(_n_,24)=1 then page+1;
run;

options ls=133 ps=47;

proc report data=final nowd headline headskip spacing=0 nocenter split="~"  missing;
	columns page avisitn  c1 paramn c2 atptn c3 intord c4-c8;
	define page/order noprint;
	define avisitn/order noprint;
	define atptn/order noprint;
	define paramn/order noprint;
	define intord/order noprint;
	define c1/order "Visit" width=15 flow;
	define c2/order "Vital SIgn" width=28 flow spacing=1;
	define c3/order "Time Point" width=30 flow spacing=1;
	define c4/"Statistic" width=12 spacing=1;
	define c5/"Placebo" "(N=%cmpres(&n1))" width=10  ;
	define c6/"Xano~meline" "54 mg" "(N=%cmpres(&n2))" width=10 ;
	define c7/"Xano~meline" "81 mg" "(N=%cmpres(&n3))" width=10 ;
	define c8/"Total"  "(N=%cmpres(&n4))" width=15 ;

	break after avisitn/skip;
	break after c3/skip;

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





