dm log 'clear';
dm output 'clear';

libname lums "D:\Home\dev\compound10\lums";

options mautosource sasautos=(lums sasautos);
%macro closevts  /* The cmd option makes the macro available to dms */ / cmd; 
  %local i; 
  %do i=1 %to 20;
    next "viewtable:"; end; 
  %end; 
%mend;

dm "keydef F12 '%NRSTR(%closevts);'";

data lb;
input usubjid paramcd $8. visitnum lbseq;
cards;
001 paramcd 1 1
001 paramcd 3 2
001 paramcd 5 3
;
run;

data sv;
input usubjid visitnum;
cards;
001 1
001 2
001 3
001 4
001 5
001 6
;
run;

*=======================================================================================================;
*Method 1 - based on Poorna's logic -consists of lbseq retention after merging with sv;
*=======================================================================================================;

*create a cartesian product of different paramcds and visits of sv;

proc sql;
	create table fullparamcds as
		select a.usubjid,a.paramcd,b.visitnum
		from 
		(select distinct usubjid, paramcd from lb) as a 
		full join
		sv as b 
		on a. usubjid=b.usubjid
		order by usubjid,paramcd,visitnum ;
quit;


*merge with original lab to have full template of subject visits;

data lb01;
	merge lb(in=a) fullparamcds(in=b);
	by usubjid paramcd visitnum;
	if a and b then inoriginal=1;*flag to identify actual lab records;
run;

*retain lbseq from previous record (to get all the other variable values in a later step);

data lb02;
	set lb01;
	by usubjid paramcd visitnum;
	retain old_lbseq;
	if first.paramcd then call missing(old_lbseq);
	if not missing(lbseq) then old_lbseq=lbseq;
	if missing(lbseq) then lbseq=old_lbseq;
run;

*get the variable values based on lbseq retained for the missing visits;

proc sort data=lb02 out=lb_missing_visits;
	by usubjid lbseq;
	where missing(inoriginal);
run;

proc sort data=lb;
	by usubjid lbseq;
run;

data lb_missing_visit02;
	merge lb(in=a) lb_missing_visits(in=b);
	by usubjid lbseq;
	if b;
	length dtype $20;
	dtype="LOCF";
run;

data lb03;
	set lb lb_missing_visit02;
	by usubjid paramcd visitnum;
	drop inoriginal old_lbseq;
run;


%ut_saslogcheck;
