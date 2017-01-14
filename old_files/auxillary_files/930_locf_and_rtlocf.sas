dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work memtype=data kill;
quit;

%macro closevts / cmd;
%local i;
%do i=1 %to 20;
next "Viewtable:"; end;
%end;
%mend;

dm "keydef F12 '%nrstr(%closevts);'";

data vs;
input usubjid visitnum aval;
if usubjid=1 then resvis=5;
else if usubjid=2 then resvis=6;
paramcd="param";
cards;
001 1 90
001 2 91
001 3 92
001 4 93
001 5 94
002 1 100
002 2 101
002 3 102
002 5 104
002 6 105
002 8 108
003 1 201
003 2 202
003 5 205
003 8 206
;

run;

data vs;
set vs;
by usubjid visitnum;
if first.usubjid then vsseq=1;
else vsseq+1;
run;

*--------------------------------------------------------------------------------------------------------------------------;
*001 - subject has no intermediate missing visits, need to get all visits till 8 for locf and 5-8 under rtlocf;
*002 - subject has missing intermediate visits - visit 4 and visit 7 missing - and for locf the subject should have
       records till 8 and should have 6,7,8 under rtlocf;
*---------------------------------------------------------------------------------------------------------------------------;

data sv;
	do usubjid=001 to 003;
		if usubjid=1 then resvid=5;
		else if usubid=2 then resvid=6;
		do visitnum=1 to 8;
			output;
		end;
	end;
run;

*=======================================================================================================;
*Method 1 - based on Avinash's logic;
*=======================================================================================================;
proc sort data=vs;
	by usubjid visitnum vsseq;
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

*merge last visitnum to vs;

data vs01;
	merge vs(in=a) lastvisit;
	by usubjid;
	if a; *keep only records present in vs;
run;

*expand each visit to last possible visit in sv using do loop;

data vsexpand01;
	set vs01(rename=(visitnum=old_visitnum));
	do visitnum=old_visitnum to lastvisit;
		output;
	end;
run;

*identify and delete the actually existing records in vs dataset;

proc sort data=vs out=vsexist01(keep=usubjid paramcd visitnum vsseq);
	by usubjid paramcd visitnum vsseq;
run;

proc sort data=vsexpand01;
	by usubjid paramcd visitnum vsseq;
run;

data vsexpand02;
	merge vsexpand01(in=a) vsexist01(in=b);
	by usubjid paramcd visitnum vsseq;
	if b then delete; *deletes the already existing sequence of a paramcd - step 1 of elimination;
run;

*identify and delete the actually existing visits(previous step was for exact records) in vs dataset;

proc sort data=vs out=vs_exist_visit(keep=usubjid paramcd visitnum) nodupkey;
	by usubjid paramcd visitnum;
run;

data vsexpand03;
	merge vsexpand02(in=a) vs_exist_visit(in=b);
	by usubjid paramcd visitnum;
	if b then delete; *deletes the already existing visits of a paramcd - step 3 of elimination;
run;

*now we need to get the latest visit from all the possible expanded visits;

proc sort data=vsexpand03;
	by usubjid paramcd visitnum vsseq;
run;

data vsexpand04;
	set vsexpand03;
	by usubjid paramcd visitnum vsseq;
	if last.visitnum; *keep only the latest of all the visits from the expansion - step 3 of elimination;
					  *contains the deserved records - set them back to original vs dataset;
	length dtype $20;
	dtype="LOCF";
run;

data vs02;
	set vs vsexpand04;
	by usubjid paramcd visitnum vsseq;
	drop old_visitnum lastvisit;
run;

*=======================================================================================================;
*RTLOCF;
*=======================================================================================================;
proc sort data=vs out=res_vs;
	by usubjid visitnum vsseq;
	where ( . lt visitnum lt resvis);
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

*merge last visitnum to vs;

data res_vs01;
	merge res_vs(in=a) lastvisit;
	by usubjid;
	if a; *keep only records present in vs;
run;

*expand each visit to last possible visit in sv using do loop;

data res_vsexpand01;
	set res_vs01(rename=(visitnum=old_visitnum));
	do visitnum=old_visitnum to lastvisit;
		output;
	end;
run;

*identify and delete the actually existing records in vs dataset;

proc sort data=res_vs out=res_vsexist01(keep=usubjid paramcd visitnum vsseq);
	by usubjid paramcd visitnum vsseq;
run;

proc sort data=res_vsexpand01;
	by usubjid paramcd visitnum vsseq;
run;

data res_vsexpand02;
	merge res_vsexpand01(in=a) res_vsexist01(in=b);
	by usubjid paramcd visitnum vsseq;
	if b then delete; *deletes the already existing sequence of a paramcd - step 1 of elimination;
run;

*identify and delete the actually existing visits(previous step was for exact records) in vs dataset;

proc sort data=res_vs out=res_vs_exist_visit(keep=usubjid paramcd visitnum) nodupkey;
	by usubjid paramcd visitnum;
run;

data res_vsexpand03;
	merge res_vsexpand02(in=a) res_vs_exist_visit(in=b);
	by usubjid paramcd visitnum;
	if b then delete; *deletes the already existing visits of a paramcd - step 3 of elimination;
run;

*now we need to get the latest visit from all the possible expanded visits;

proc sort data=res_vsexpand03;
	by usubjid paramcd visitnum vsseq;
run;

data res_vsexpand04;
	set res_vsexpand03;
	by usubjid paramcd visitnum vsseq;
	if last.visitnum; *keep only the latest of all the visits from the expansion - step 3 of elimination;
					  *contains the deserved records - set them back to original vs dataset;
	length dtype $20;
	dtype="RTLOCF";
run;

data res_vs02;
	set res_vsexpand04;
	by usubjid paramcd visitnum vsseq;
	drop old_visitnum lastvisit;
	if  . lt visitnum lt resvis then delete;
run;

data vs03;
	set vs02 res_vs02;
run;

proc sort data=vs03;
	by usubjid visitnum dtype;
run;
