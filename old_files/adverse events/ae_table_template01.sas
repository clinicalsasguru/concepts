dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

*======================================================;
*This program is used to create Adverse event table;
*overview of adverse events;
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
	select count(*) into :n1 from dm where trtpn=1;
	select count(*) into :n2 from dm where trtpn=2;
	select count(*) into :n3 from dm where trtpn=3;
quit;

*=====================================================================================;

*--------------------------;
*ae processing;
*--------------------------;

proc sort data=sdtm.ae out=ae_pre;
	by usubjid;
run;

data ae;
	merge ae_pre(in=a) dm(in=b);
	by usubjid;
	if a and b;
run;

*-----------------------------;
*ds processing;
*-----------------------------;

proc sort data=sdtm.ds out=ds_pre;
	by usubjid;
run;

data ds;
	merge ds_pre(in=a) dm(in=b);
	by usubjid;
	if a and b;
run;


*============================================================;
*creating dummy dataset with all labels;
*============================================================;

data dummy_pre;
	length label $200;
	label="Subjects with atleast one AE"; ord=2;output ;
	label="Subjects with atleast one SAE"; ord=3;output;
	label="Subjects with atleast one drug related AE";ord=4;output;
	label="Subjects who discontinued the study due to an AE";ord=5; output;
	label="Subjects with drug interruption due to an AE"; ord=6; output;
run;

*----------------------------------------------------;
*creating a record for each treatment with zero count;
*----------------------------------------------------;

data dummy;
	set dummy_pre;
	count=0;
	do trtpn=1 to 3;
		output;
	end;
run;

*=====================================================================;
*obtaining actual counts(with processing for each row);
*=====================================================================;

*-----------;
*row 2;
*-----------;

proc sql;
	create table row2 as 
	select trtpn, 2 as ord,
	count(distinct usubjid) as count
	from ae 
	group by trtpn;
quit;

*-----------;
*row 3;
*-----------;

proc sql;
	create table row3 as
	select trtpn, 3 as ord,
	count(distinct usubjid) as count
	from ae
	where aeser="Y"
	group by trtpn;
quit;

*-----------;
*row 4;
*-----------;

proc sql;
	create table row4 as 
	select trtpn, 4 as ord,
	count(distinct usubjid) as count
	from ae
	where upcase(aerel)="POSSIBLY RELATED"
	group by trtpn;
quit;

*---------------------;
*row 5;
*---------------------;

*------------------------------------------;
*check ds for discontinuation due to ae;
*------------------------------------------;

proc sql;
	create table row5 as 
	select trtpn, 5 as ord,
	count(distinct usubjid) as count
	from ds
	where dsdecod="ADVERSE EVENT" and dsterm="DISCONTINUED DUE TO ADVERSE EVENT"
	group by trtpn;
quit;

*--------;
*row 6;
*--------;

proc sql;
	create table row6 as 
	select trtpn, 6 as ord,
	count(distinct usubjid) as count
	from ae
	where aeacn="DRUG INTERRUPTED"
	group by trtpn;
quit;


data allrows;
	set row2 row3 row4 row5 row6;
run;

*------------------------------;
*merge with dummy counts;
*------------------------------;
proc sort data=dummy;
	by ord trtpn;
run;

proc sort data=allrows;
	by ord trtpn;
run;

data allrows2;
	merge dummy(in=a) allrows(in=b);
	by ord trtpn;
run;

*=======================================;
*calculate percentages;
*=======================================;

proc sort data=allrows2;
	by trtpn;
run;

proc sort data=trttotals;
	by trtpn;
run;

data allrows2;
	merge allrows2(in=a) trttotals(in=b);
	by trtpn;
run;

data allrows3;
	set allrows2;
	percent=round(count/trttotal*100,0.1);
	count_percent=put(count,3.)||" ("||put(percent,5.1)||")";
	keep ord label trtpn count_percent;
run;

*-----------------------------------------------------;
*transpose to get treatments as columns;
*-----------------------------------------------------;

proc sort data=allrows3;
	by ord label;
run;

proc transpose data=allrows3 out=final(drop=_:) prefix=trt;
	by ord label;
	var count_percent;
	id trtpn;
run;


*============================================================================;
*report generation;
*============================================================================;

title;
footnote;
options ps=47 ls=133 nonumber nodate nocenter;

proc report data=final nowd headline headskip missing spacing=0;
	columns  label ord  trt1 trt2 trt3;
	define ord /order noprint;
	define label / "" width=70 flow;
	define trt1/"Placebo" "N=(%cmpres(&n1))" width=21 center;
	define trt2/"Miracle Drug" "N=(%cmpres(&n2))" width=21 center;
	define trt3/"Miracle Drug" "N=(%cmpres(&n3))" width=21 center;

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


	






