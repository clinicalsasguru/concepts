dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

*======================================================;
*This program is used to create Adverse event table;
*======================================================;

libname sdtm "D:\Home\dev\compound1\data\shared\sdtm";


*------------------------------------------------;
*get treatment column into sdtm.ae from sdtm.dm;
*------------------------------------------------;

proc sort data=sdtm.dm out=dm(keep=usubjid armcd arm);
	by usubjid;
	where armcd ne "SCRNFAIL";
run;

data dm;
	set dm;
	select(upcase(armcd));
		when ("PLACEBO") trtpn=1;
		when ("WONDER10") trtpn=2;
		when ("WONDER20") trtpn=3;
	otherwise;
	end;
	trt=arm;
	drop arm armcd;
run;

*=================================================================================;
*obtain treatment counts for percentages, and
*treatment totals into macro variables for column header;
*=================================================================================;

proc sql;
create table trttotals_pre as
	select trtpn,
	count(distinct usubjid) as trttotal
	from dm
	where not missing(trtpn)
	group by trtpn;
quit;

data dummy_trttotals;
	do trtpn=1 to 3;
		trttotal=0;
		output;
	end;
run;

*merge dummy totals with actual totals(to get zero counts if a trt is not present);

data trttotals;
	merge dummy_trttotals(in=a) trttotals_pre(in=b);
	by trtpn;
run;

*macro variables;
proc sql noprint;
	select count(distinct usubjid) into :n1 from dm where trtpn=1;
	select count(distinct usubjid) into :n2 from dm where trtpn=2;
	select count(distinct usubjid) into :n3 from dm where trtpn=3;
quit;

%put number of subjects in trt1 &n1;
%put number of subjects in trt2 &n2;
%put number of subjects in trt3 &n3;

*=====================================================================================;

proc sort data=sdtm.ae out=ae_pre;
	by usubjid;
run;

data ae_pre;
	set ae_pre;
	if missing(aebodsys) then aebodsys="Not coded";
	if missing(aedecod) then aedecod="Not coded";
run;

*Levels in adverse event classification (Meddra);
*HLT;
*soc;
*pt;
*aeterm;

data ae;
	merge ae_pre(in=a) dm(in=b);
	by usubjid;
	if a and b;
run;

*================================================================;
*obtaining actual counts-for the table;
*================================================================;
/*
Cardiac	MI	Mild	1-Jan
Cardiac	MI	Mild	2-Jan
Cardiac	MI	Moderate	10-Jan
Cardiac	Atrial Flutter		
*/

*------------------------------;
*subject level count- top row;
*------------------------------;

proc sql noprint;
	create table sub_count as 
	select "Patients with atleast one AE" as label length=200,
	trtpn,
	count(distinct usubjid) as count 
	from ae
	group by trtpn;
quit;

*---------------------------------------;
*soc level counts;
*---------------------------------------;

proc sql noprint;
	create table soc_count as
		select aebodsys, trtpn,
		count(distinct usubjid) as count
		from ae
		group by aebodsys,trtpn;
quit;

*--------------------------------------;
*preferred term level counts;
*--------------------------------------;

proc sql noprint;
	create table pt_count as 
		select aebodsys,aedecod,trtpn,
		count(distinct usubjid) as count
		from ae 
		group by aebodsys,aedecod,trtpn;
quit;

*---------------------------------;
*put all counts together;
*---------------------------------;

data counts;
	set sub_count soc_count pt_count;
run;

*=========================================================;
*create zero counts if an event is not present in a trt;
*=========================================================;

*get all the available soc-s and pt-s;

proc sort data=counts out=dummy_pre(keep=aebodsys aedecod label) nodupkey;
	by aebodsys aedecod label;
run;

*create a row for each treatment and severity;

data dummy;
	set dummy_pre;
	count=0;
	do trtpn=1 to 3;
			output;
	end;
run;

*merge dummy counts with actual counts;
proc sort data=dummy;
	by aebodsys aedecod label trtpn;
run;


proc sort data=counts out=counts2;
	by aebodsys aedecod label trtpn;
run;

data counts3;
	merge dummy(in=a) counts2(in=b);
	by aebodsys aedecod label trtpn;
run;

*---------------------------------------;
*obtain percentages;
*---------------------------------------;

*merge counts with trttotals dataset, to get denominator values(trt totals);

proc sort data=counts3;
	by trtpn;
run;

proc sort data=trttotals;
	by trtpn;
run;

data counts4;
	merge counts3(in=a) trttotals(in=b);
	by trtpn;
	if a;
run;

data counts4;
	set counts4;
	percent=round((count/trttotal*100),0.1);
	count_percent=put(count,3.)||" ("||put(percent,5.1)||")";
run;

*---------------------------------------------;
*create the label column;
*---------------------------------------------;

data counts5;
	set counts4;
	if missing(aebodsys) and missing(aedecod) then label=label;
	else if not missing(aebodsys) and missing(aedecod) then label=strip(aebodsys);
	else if not missing(aebodsys) and not missing(aedecod) then label="  "||strip(aedecod);
run;

*====================================================;
*transpose to obtain treatment as columns;
*====================================================;

proc sort data=counts5;
	by aebodsys aedecod label ;
run;

proc transpose data=counts5 out=trans1 prefix=trt;
	by aebodsys aedecod label;
	var count_percent;
	id trtpn;
run;


data final;
	set trans1;
	keep aebodsys aedecod label trt1 trt2 trt3;
run;


*============================================================================;
*report generation;
*============================================================================;

title;
footnote;
options ps=47 ls=133 nonumber nodate;

proc report data=final nowd headline headskip missing  spacing=0 ;
	columns aebodsys aedecod label  trt1 trt2 trt3;
	define aebodsys/ order noprint;
	define aedecod/order noprint;
	define label /order "System Organ Class" "  Preferred Term" width=70 flow;
	define trt1/"Placebo" "N=(%cmpres(&n1))" width=21 center ;
	define trt2/"Miracle Drug" "N=(%cmpres(&n2))" width=21 center;
	define trt3/"Miracle Drug" "N=(%cmpres(&n3))" width=21 center;

	break after aebodsys/skip;

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




		
	


