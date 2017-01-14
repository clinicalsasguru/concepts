dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

libname adam "D:\Home\dev\cdisc_data_setup";

/*libname adam "G:\ANIL\TRAINING_SESSIONS\ADAM_SDTM\ADAM";*/

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


*****************
Baseline  data;
*****************;
data adlb_base;
	set adam.adlbc;
	where safety="Y" and visit ne "UNSCHEDULED"/*and LBBLFL="Y"*/;
run;
proc sort data=adsl;
by usubjid;
run;
proc sort data=adlb_base;
by usubjid;
run;

data adlb_base;
	merge adlb_base(in=a) adsl(in=b);
	by usubjid;
	if a and b;
run;

data adlb_base;
	set adlb_base;
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

proc sort data=adlb_base out=adlb_base;
	by lbtestcd lbtest  visitnum visit ;
run;
	

proc summary data=adlb_base  nway completetypes;
	by lbtestcd lbtest  visitnum visit ;
	class trtpn/preloadfmt;
	format trtpn trtpn.;
	where not missing(lbstresn) ;
	var lbstresn;
	output out=_stats(drop=_type_ _freq_)
	n= result_n mean=result_mean std=result_std median=result_median q1=result_q1 q3=result_q3 ;
/*	n= mean= std= median= q1= q3= /autoname;*/
run;


data _statsbase;
	set _stats;
	by lbtestcd lbtest  visitnum visit ;
	if not missing(result_n) then val_n=put(result_n,3.);
	if not missing(result_mean) then val_mean=put(result_mean,5.1);

	if not missing(result_std) then val_sd=put(result_std,6.2);
/*	else if lbstresn_n=1 then sd="  -";*/

	if not missing(result_median) then val_median=put(result_median,5.1);
	if not missing(result_q1) then val_q1=put(result_q1,5.1);
	if not missing(result_q3) then val_q3=put(result_q3,5.1);

	drop result_:;
run;


*****************
chage data;
*****************;


data adlb_change;
	set adam.adlbc;
	where safety="Y" and (visit ne "UNSCHEDULED" and LBBLFL ne "Y");
run;
proc sort data=adsl;
by usubjid;
run;
proc sort data=adlb_change;
by usubjid;
run;

data adlb_change;
	merge adlb_change(in=a) adsl(in=b);
	by usubjid;
	if a and b;
run;

data adlb_change;
	set adlb_change;
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

proc sort data=adlb_change out=adlb_change;
	by lbtestcd lbtest  visitnum visit ;
run;
	

proc summary data=adlb_change  nway completetypes;
	by lbtestcd lbtest  visitnum visit ;
	class trtpn/preloadfmt;
	format trtpn trtpn.;
	where not missing(lbstresn) ;
	var CHSTRESN;
	output out=_stats_change(drop=_type_ _freq_)
	n= change_n mean=change_mean std=change_std median=change_median q1=change_q1 q3=change_q3 ;
/*	n= mean= std= median= q1= q3= /autoname;*/
run;


data _stats_change;
	set _stats_change;
	by lbtestcd lbtest  visitnum visit ;
	if not missing(change_n) then chg_n=put(change_n,3.);
	if not missing(change_mean) then chg_mean=put(change_mean,5.1);

	if not missing(change_std) then chg_sd=put(change_std,6.2);
/*	else if lbstresn_n=1 then sd="  -";*/

	if not missing(change_median) then chg_median=put(change_median,5.1);
	if not missing(change_q1) then chg_q1=put(change_q1,5.1);
	if not missing(change_q1) then chg_q3=put(change_q1,5.1);

	drop change_:;
run;


proc sort data=_statsbase ;
	by  lbtestcd lbtest  TRTPN visitnum visit;
run;

proc sort data=_stats_change ;
	by  lbtestcd lbtest  TRTPN visitnum visit;
run;

data final;
	merge _statsbase  _stats_change;
	by  lbtestcd lbtest  TRTPN visitnum visit;
run;

data final;
	set final;
	length trtp $30;
	if trtpn=0 then trtp="Placebo";
	else if trtpn=1 then trtp="Xanomeline Low Dose";
	else if trtpn=2 then trtp="Xanomeline High Dose";
	visit="  "||strip(visit);
run;

options ls=153 ps=48;

proc report data=final nowd headline headskip spacing=0 nocenter;
	by lbtestcd lbtest;
	columns  trtpn trtp visitnum visit val_n val_mean val_sd val_q1 val_q3 
	chg_n chg_mean chg_sd chg_q1 chg_q3;
/*	define lbtestcd/order noprint;*/
	define visitnum/order noprint;
	define trtpn/order noprint;
/*	define lbtest/order noprint;*/
	define visit/order "Visit" width=15 flow;
	define trtp/order  noprint;
	define val_n/ width=10;
	define val_mean/width=10;
	define val_sd/width=10;
	define val_q1/width=10;
	define val_q3/width=10;
	define chg_n/ width=10;
	define chg_mean/width=10;
	define chg_sd/width=10;
	define chg_q1/width=10;
	define chg_q3/width=10;

	break after trtpn/skip;

	compute before trtp;
		line @1 trtp $153.;
	endcomp;

	compute before _page_;
		line @1 "Title1";
		line @1 "Title2";
		line @ 1 "";
		line @1 153*'-';
	endcomp;

	compute after _page_;
		line @1 153*'-';
		line @1 "Footnote1";
		line @1 "Footnote2";
		line @1 "Footnote3";
		line @1"";
	endcomp;
run;



