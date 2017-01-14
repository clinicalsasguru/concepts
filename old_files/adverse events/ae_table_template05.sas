proc datasets library=work mt=data kill nolist;
quit;

dm log 'clear';
dm lst 'clear';
dm log 'preview';

*======================================================;
*This program is used to create Adverse event table;
*======================================================;

libname sdtm "D:\Home\dev\compound1\data\shared\sdtm";


*------------------------------------------------;
*get treatment column into sdtm.ae from sdtm.dm;
*------------------------------------------------;

proc sort data=sdtm.dm out=dm(keep=usubjid armcd arm);
	by usubjid;
	where armcd ne "SCRFAIL";
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

*=====================================================================================;

proc sort data=sdtm.ae out=ae_pre;
	by usubjid;
run;

data ae_pre;
	set ae_pre;
	if missing(aebodsys) then aebodsys="Not coded";
	if missing(aedecod) then aedecod="Not coded";
run;

data ae;
	merge ae_pre(in=a) dm(in=b);
	by usubjid;
	if a and b;
run;


*============================================;
*get the maximum severity;
*============================================;
*---------------------;
*subject level;
*---------------------;

proc sort data=ae out=ae_sub_pre;
	by usubjid aesev;
run;

data ae_sub;
	set ae_sub_pre;
	by usubjid aesev;
	if last.usubjid;
run;

*--------------------------;
*soc level within subject;
*--------------------------;

proc sort data=ae out=ae_soc_pre;
	by usubjid aebodsys aesev;
run;

data ae_soc;
	set ae_soc_pre;
	by usubjid aebodsys aesev;
	if last.aebodsys;
run;

*-------------------------------------------------------------;
*within pterm , within soc and within subject;
*--------------------------------------------------------------;

proc sort data=ae out=ae_pt_pre;
	by usubjid aebodsys aedecod aesev;
run;

data ae_pt;
	set ae_pt_pre;
	by usubjid aebodsys aedecod aesev;
	if last.aedecod;
run;


*================================================================;
*obtaining actual counts-for the table;
*================================================================;

*------------------------------;
*subject level count- top row;
*------------------------------;

proc sql noprint;
	create table sub_count as 
	select "Patients with atleast one AE" as label length=200,
	trtpn,aesev,
	count(distinct usubjid) as count 
	from ae_sub
	group by trtpn,aesev;
quit;

*---------------------------------------;
*soc level counts;
*---------------------------------------;

proc sql noprint;
	create table soc_count as
		select aebodsys, trtpn,aesev,
		count(distinct usubjid) as count
		from ae_soc
		group by trtpn,aebodsys,aesev;
quit;

*--------------------------------------;
*preferred term level counts;
*--------------------------------------;

proc sql noprint;
	create table pt_count as 
		select aebodsys,aedecod,trtpn,aesev,
		count(distinct usubjid) as count
		from ae_pt 
		group by trtpn,aebodsys,aedecod,aesev;
quit;

*---------------------------------;
*put all counts together;
*---------------------------------;

data counts;
	set sub_count soc_count pt_count;
run;



*---------------------------------------------;
*create the label column;
*---------------------------------------------;

data counts2;
	set counts;
	if missing(aebodsys) and missing(aedecod) then label=label;
	else if not missing(aebodsys) and missing(aedecod) then label="  "||strip(aebodsys);
	else if not missing(aebodsys) and not missing(aedecod) then label="    "||strip(aedecod);
run;

*=========================================================;
*create zero counts if an event is not present in a trt;
*=========================================================;

*get all the available soc-s and pt-s;

proc sort data=counts2 out=dummy_pre(keep=aebodsys aedecod label) nodupkey;
	by aebodsys aedecod label;
run;

*create a row for each treatment and severity;

data dummy;
	set dummy_pre;
	length aesev $8;
	count=0;
	do trtpn=1 to 3;
		do aesev="MILD", "MODERATE", "SEVERE";
			output;
		end;
	end;
run;

*merge dummy counts with actual counts;
proc sort data=dummy;
	by aebodsys aedecod label aesev trtpn;
run;

proc sort data=counts2 out=counts3;
	by aebodsys aedecod label aesev trtpn;
run;

data counts4;
	merge dummy(in=a) counts3(in=b);
	by aebodsys aedecod label aesev trtpn;
run;

*================================================;
*calculating percentages;
*================================================;

*merge counts with trttotals dataset, to get denominator values(trt totals);

proc sort data=counts4;
	by trtpn;
run;

proc sort data=trttotals;
	by trtpn;
run;

data counts5;
	merge counts4(in=a) trttotals(in=b);
	by trtpn;
	if a;
run;

data counts6;
	set counts5;
	percent=round(count/trttotal*100,0.1);
	count_percent=put(count,3.)||" ("||put(percent,5.1)||")";
run;

*====================================================;
*transpose to obtain treatment as columns;
*====================================================;

proc sort data=counts6;
	by aebodsys aedecod label aesev;
run;

proc transpose data=counts6 out=trans1;
	by aebodsys aedecod label;
	var count_percent;
	id trtpn aesev;
run;


data final;
	set trans1;
	keep aebodsys aedecod label _1: _2: _3:;
run;

/*
*============================================================================;
*report generation;
*============================================================================;

title;
footnote;
options ps=47 ls=133 nonumber nodate;

proc report data=final nowd headline headskip missing spacing=0;
	columns aebodsys aedecod label aesev trt1 trt2 trt3;
	define aebodsys/ order noprint;
	define aedecod/order noprint;
	define label /order "System Organ Class" "  Preferred Term" width=60 flow;
	define aesev/"Maximum" "Severity" width=8;
	define trt1/"Placebo" "N=(%cmpres(&n1))" width=21 spacing=2;
	define trt2/"Miracle Drug" "N=(%cmpres(&n2))" width=21;
	define trt3/"Miracle Drug" "N=(%cmpres(&n3))" width=21;

	break after aedecod/skip;

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




		
	


*/
