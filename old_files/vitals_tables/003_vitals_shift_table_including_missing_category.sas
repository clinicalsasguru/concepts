dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;
/*libname adam "G:\ANIL\TRAINING_SESSIONS\ADAM_SDTM\ADAM";*/

libname adam "D:\Home\dev\cdisc_data_setup";

proc format;
	value trtpn
		0=0
		1=1
		2=2
		3=3;

	value baselinen (multilabel)
		0=0
		1=1
		2=2
		3=3
		0,1,2,3=4;

	value  postbaselinen (multilabel)
		0=0
		1=1
		2=2
		3=3
		0,1,2,3=4;

run;

data adsl;
	set adam.adsl;
	where safety="Y";
	keep usubjid safety trtpn trtp;
run;


/*create  new_advs dataset with  ANRHI/ANRLO values;*/
data advs_pre;
set adam.advs;
if paramn=1 then do;
	anrhi=140; 
	anrlo=100;
if not missing(aval) then do;
    if aval le anrlo then anrind="Low";
    else if anrlo lt aval le anrhi then anrind="Normal";
    else if aval gt anrhi then anrind="High";
end;
end;
run;


data advs_pre1;
	set advs_pre;
	where safety="Y" and (avisitn in (2 4) or (avisitn=0 and ady=1)) and paramn in (1 2) and atptn=815;
run;

data advs;
	merge advs_pre1(in=a) adsl(in=b);
	by usubjid;
	if a and b;
run;

data baseline_pre postbaseline_pre;
	set advs;
	if visit="BASELINE" then output baseline_pre;
	else if visit in ("WEEK 2" "WEEK 4") then output postbaseline_pre;
run;

data baseline(rename=(anrind=baseline));
	set baseline_pre;
	keep usubjid paramn param anrind atptn atpt trtpn trtp baselinen;
	if upcase(anrind)="LOW" then baselinen=1;
	else if upcase(anrind)="NORMAL" then baselinen=2;
	else if upcase(anrind)="HIGH" then baselinen=3;
run;

data postbaseline(rename=(anrind=postbaseline));
	set postbaseline_pre;
	keep usubjid paramn param anrind atptn atpt visit avisitn avisit trtpn trtp postbaselinen;
	if upcase(anrind)="LOW" then postbaselinen=1;
	else if upcase(anrind)="NORMAL" then postbaselinen=2;
	else if upcase(anrind)="HIGH" then postbaselinen=3;
run;


data adsl_bldummy;
	set adsl(keep=usubjid trtpn trtp);
	length param $100 atpt $30;
	paramn=1;
	param="Systolic Blood Pressure (mmHg)";
	atpt="AFTER LYING DOWN FOR 5 MINUTES";
	atptn=815;
	output;

	paramn=2;
	param="Diastolic Blood Pressure (mmHg)";
	atpt="AFTER LYING DOWN FOR 5 MINUTES";
	atptn=815;
	output;	
run;

proc sort data=baseline;
	by usubjid trtpn trtp paramn param atptn atpt;
run;

proc sort data=adsl_bldummy;
	by usubjid trtpn trtp paramn param atptn atpt;
run;

data baseline_m;
	merge adsl_bldummy(in=a) baseline(in=b);
	by usubjid trtpn trtp paramn param atptn atpt;
run;

data baseline_m;
	set baseline_m;
	length baseline $7;
	if missing(baselinen) then do;
	baselinen=0;
	baseline="Missing";
	end;
run;

*--------------------------------------------;
*post basline dummy;
*--------------------------------------------;

data adsl_pbldummy;
	set adsl(keep=usubjid trtpn trtp);
	length param $100 atpt $30 avisit $16;
	paramn=1;
	avisitn=2;
	avisit="Week 2";
	param="Systolic Blood Pressure (mmHg)";
	atpt="AFTER LYING DOWN FOR 5 MINUTES";
	atptn=815;
	output;

	paramn=1;
	avisitn=4;
	avisit="Week 4";
	param="Systolic Blood Pressure (mmHg)";
	atpt="AFTER LYING DOWN FOR 5 MINUTES";
	atptn=815;
	output;

	paramn=2;
	avisit="Week 2";
	avisitn=2;
	param="Diastolic Blood Pressure (mmHg)";
	atpt="AFTER LYING DOWN FOR 5 MINUTES";
	atptn=815;
	output;	

	paramn=2;
	avisit="Week 4";
	avisitn=4;
	param="Diastolic Blood Pressure (mmHg)";
	atpt="AFTER LYING DOWN FOR 5 MINUTES";
	atptn=815;
	output;	
run;

proc sort data=postbaseline;
	by usubjid trtpn trtp paramn param avisitn avisit atptn atpt;
run;

proc sort data=adsl_pbldummy;
	by usubjid trtpn trtp paramn param avisitn avisit atptn atpt;
run;

data postbaseline_m;
	merge adsl_pbldummy postbaseline;
	by usubjid trtpn trtp paramn param avisitn avisit atptn atpt;;
run;

data postbaseline_m;
	set postbaseline_m;
	length postbaseline $7;
	if missing(postbaselinen) then do;
	postbaselinen=0;
	postbaseline="Missing";
	end;
run;


proc sort data=baseline_m;
	by usubjid paramn param atptn atpt trtpn trtp;
run;

proc sort data=postbaseline_m;
	by usubjid paramn param atptn atpt trtpn trtp;
run;

data blpbl;
	merge baseline_m(in=a) postbaseline_m(in=b);
	by usubjid paramn param atptn atpt trtpn trtp;
	if a and b;
run;


proc sort data=blpbl;
	by paramn param trtpn trtp avisitn avisit atptn atpt;
run;

proc summary data=blpbl nway completetypes;
	by paramn param trtpn trtp avisitn avisit atptn atpt;
	class baselinen/preloadfmt mlf; /* mlf=mlti label format*/
	class postbaselinen/preloadfmt mlf;
	format baselinen baselinen. postbaselinen postbaselinen.;
	output out=counts(drop=_type_ rename=(_freq_=count));
run;

proc transpose data=counts out=test;
	by paramn param trtpn trtp avisitn avisit atptn atpt baselinen;
	var count;
	id postbaselinen;
run;
	
data denom0(rename=(count=denom0))
	denom1(rename=(count=denom1))
	 denom2(rename=(count=denom2))
	 denom3(rename=(count=denom3))
	 denom4(rename=(count=denom4));
	set counts;
	if baselinen="0" and postbaselinen="4" then output denom0;

	if baselinen="1" and postbaselinen="4" then output denom1;
	if baselinen="2" and postbaselinen="4" then output denom2;
	if baselinen="3" and postbaselinen="4" then output denom3;
	if baselinen="4" and postbaselinen="4" then output denom4;

	keep paramn param trtpn trtp avisitn avisit atptn atpt count;
run;


proc sort data=counts;
	by paramn param trtpn trtp avisitn avisit atptn atpt;
run;

proc sort data=denom0;
	by paramn param trtpn trtp avisitn avisit atptn atpt;
run;

proc sort data=denom1;
	by paramn param trtpn trtp avisitn avisit atptn atpt;
run;

proc sort data=denom2;
	by paramn param trtpn trtp avisitn avisit atptn atpt;
run;
proc sort data=denom3;
	by paramn param trtpn trtp avisitn avisit atptn atpt;
run;
proc sort data=denom4;
	by paramn param trtpn trtp avisitn avisit atptn atpt;
run;

data counts2;
	merge counts denom0 denom1 denom2 denom3 denom4;
	by paramn param trtpn trtp avisitn avisit atptn atpt;
run;


*---------------------------------------------------------;
*get macro variables for treatment counts;
*---------------------------------------------------------;

proc sql noprint;
	select count(distinct usubjid) into :n1 from adsl where trtpn=0;
	select count(distinct usubjid) into :n2 from adsl where trtpn=1;
	select count(distinct usubjid) into :n3 from adsl where trtpn=2;
quit;

%put &n1 &n2 &n3;



data counts3;
	set counts2;
	length cp $20 label $15 treatment $50;


********To get 0 (ZEROS) instead of 0(0.00) in the output;

		if baselinen="0" and  postbaselinen in ("0" "1" "2" "3") then do;
			if count ne 0 then
			cp=put(count,3.)||" ("||put(count/denom0*100,5.1)||")";
			else cp=put(count,3.);
		end;

		if baselinen="1" and  postbaselinen in ("0" "1" "2" "3") then do;
			if count ne 0 then
			cp=put(count,3.)||" ("||put(count/denom1*100,5.1)||")";
			else cp=put(count,3.);
		end;

		if baselinen="2" and  postbaselinen in ("0" "1" "2" "3") then do;
			if count ne 0 then 
			cp=put(count,3.)||" ("||put(count/denom2*100,5.1)||")";
			else cp=put(count,3.);
		end;

		if baselinen="3" and  postbaselinen in ("0" "1" "2" "3") then do;
			if count ne 0 then 
			cp=put(count,3.)||" ("||put(count/denom3*100,5.1)||")";
			else cp=put(count,3.);
		end;

		if baselinen="4" or postbaselinen="4" then do;
			if count ne 0 then
			cp=put(count,3.)||" ("||put(count/denom4*100,5.1)||")";
			else cp=put(count,3.);
		end;


	if trtpn=0 then treatment=strip(trtp)||" (N=%cmpres(&n1))";
	if trtpn=1 then treatment=strip(trtp)||" (N=%cmpres(&n2))";
	if trtpn=2 then treatment=strip(trtp)||" (N=%cmpres(&n3))";

	if baselinen="0" then label="Missing";
	if baselinen="1" then label="Low";
	if baselinen="2" then label="Normal";
	if baselinen="3" then label="High";
	if baselinen="4" then label="Total";
run;


proc sort data=counts3;
	by paramn param trtpn treatment avisitn avisit atptn atpt baselinen label;
run;

proc transpose data=counts3 out=trans;
	by paramn param trtpn treatment avisitn avisit atptn atpt baselinen label;
	var cp;
	id postbaselinen;
run;

data final;
	set trans;
	length c1-c10 $200;
	c1=param;
	c2=treatment;
	c3=avisit;
	c4=atpt;
	c5=label;
	c6=_0;
	c7=_1;
	c8=_2;
	c9=_3;
	c10=_4;
	keep c1-c10 paramn avisitn atptn baselinen trtpn;
run;

options ls=133 ps=47 nocenter nonumber nodate;
title;

proc report data=final nowd headline headskip split="~" missing spacing=0;
	columns paramn c1 trtpn c2 avisitn c3 atptn c4 baselinen c5
	("" "Post- Baseline" "" "--" (c6-c10));
	define paramn/order noprint;
	define c1/order width=15 "Parameter" flow;
	define trtpn/order noprint;
	define c2/order "Treatment" width=12 flow spacing=1;
	define avisitn/order noprint;
	define c3/order "Visit" width=10 flow spacing=1;
	define atptn/order noprint;
	define c4/order "Time Point" width=15 flow spacing=1;
	define baselinen/order noprint;
	define c5/"Baseline" width=10 flow spacing=1;
	define c6/"   Missing" "    n (%)" width=12 spacing=1;
	define c7/"     Low" "    n (%)" width=12 spacing=1;

	define c8/"  Normal" "   n (%)" width=12 spacing=1;
	define c9/"   High" "   n (%)" width=12 spacing=1;
	define c10/"  Total" "   n (%)" width=12 spacing=1;

	break after atptn/skip;

	compute before _page_;
		line @1 "Title1";
		line @1 "Title2";
		line @1 133*'-';
	endcomp;

	compute after _page_;
		line @1 133*'-';
		line @1 "Footnote1";
		line @1 "Footnote2";
	endcomp;
run;

