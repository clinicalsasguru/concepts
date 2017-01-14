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
*Method 1 - based on Avinash's logic;
*=======================================================================================================;
proc sort data=lb;
	by usubjid visitnum lbseq;
run;

proc sort data=sv;
	by usubjid visitnum;
run;

*------------------------------------------------;
*get the last visit from sv;
*------------------------------------------------;
*apply any subset conditions as needed to eliminate unnecessary visits like follow-up, unscheduled etc;

data lastvisit;
	set sv;
	by usubjid visitnum;
	if last.usubjid;
	lastvisit=visitnum;
	keep usubjid lastvisit;
run;

*-------------------------------------------------------------;
*processing for locf creation;
*-------------------------------------------------------------;

*merge last visitnum to lb;

data lb01;
	merge lb(in=a) lastvisit;
	by usubjid;
	if a; *keep only records present in lb;
run;

*expand each visit to last possible visit in sv using do loop;

data lbexpand01;
	set lb01(rename=(visitnum=old_visitnum));
	do visitnum=old_visitnum to lastvisit;
		output;
	end;
run;

*identify and delete the actually existing records in lb dataset;

proc sort data=lb out=lbexist01(keep=usubjid paramcd visitnum lbseq);
	by usubjid paramcd visitnum lbseq;
run;

proc sort data=lbexpand01;
	by usubjid paramcd visitnum lbseq;
run;

data lbexpand02;
	merge lbexpand01(in=a) lbexist01(in=b);
	by usubjid paramcd visitnum lbseq;
	if b then delete; *deletes the already existing sequence of a paramcd - step 1 of elimination;
run;

*identify and delete the actually existing visits(previous step was for exact records) in lb dataset;

proc sort data=lb out=lb_exist_visit(keep=usubjid paramcd visitnum) nodupkey;
	by usubjid paramcd visitnum;
run;

data lbexpand03;
	merge lbexpand02(in=a) lb_exist_visit(in=b);
	by usubjid paramcd visitnum;
	if b then delete; *deletes the already existing visits of a paramcd - step 3 of elimination;
run;

*now we need to get the latest visit from all the possible expanded visits;

proc sort data=lbexpand03;
	by usubjid paramcd visitnum lbseq;
run;

data lbexpand04;
	set lbexpand03;
	by usubjid paramcd visitnum lbseq;
	if last.visitnum; *keep only the latest of all the visits from the expansion - step 3 of elimination;
					  *contains the deserved records - set them back to original lb dataset;
	length dtype $20;
	dtype="LOCF";
run;

data lb02;
	set lb lbexpand04;
	by usubjid paramcd visitnum lbseq;
	drop old_visitnum lastvisit;
run;

*----------------------------------------------------------------------------------------------;
*optional - if the visits which are NOT present in sv have to be eliminated;
*----------------------------------------------------------------------------------------------;

proc sort data=lb02;
	by usubjid visitnum paramcd;
run;

data lb03;
	merge lb02(in=a) sv(in=b);
	by usubjid visitnum;
	if a and not b then delete;
run;




%ut_saslogcheck;
