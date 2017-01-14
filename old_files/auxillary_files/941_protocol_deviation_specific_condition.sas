*==============================================================;
*prdevi10;
"1. For every USUBJID and find the last SV.SVENDTC where SV.VISITNUM<1999 combination in SDTM.SV. 
2. Check that there is no interval of greater than 67 days between ADSL.TR01SDT and the date found in step 1 for which there is no XP.XPCAT = ""ECHOCARDIOGRAM"" based on XP.XPDTC. If there is such an interval of greater 67 days create a record and set AVALC = ""Missing ECHO"".
3. If there a record of SV.VISITNUM = 1999 but no such record in SDTM.XP for which VISITNUM=1999 and XP.XPCAT = ""ECHOCARDIOGRAM"" then create create a record and set AVALC = ""Missing ECHO""."
*==============================================================;

*-----------------------------------------------------;
*get last visit date and 1999 visit date;
*-----------------------------------------------------;

proc sort data=sdtm.sv out=sv10_01(keep=usubjid svendtc visitnum);
	by usubjid svendtc;
run;

data sv10_02 sv10_03;
	set sv10_01;
	if length(svendtc) ge 10 then svendt=input(substrn(svendtc,1,10),?? yymmdd10.);
	format svendt date9.;
	if visitnum lt 1999 then output sv10_02;
	else if visitnum=1999 then output sv10_03;
run;

proc sort data=sv10_02;
	by usubjid svendt;
run;

data sv10_02;
	set sv10_02;
	by usubjid svendt;
	if last.usubjid;
run;


*-----------------------------------------------------;
*get the dates on which echocardiogram was performed;
*-----------------------------------------------------;

proc sort data=sdtm.xp out=xp10_01(keep=usubjid xpdtc visitnum) nodupkey;
	by usubjid visitnum xpdtc;
	where xpcat="ECHOCARDIOGRAM" and xpstat ne "NOT DONE";
run;

data xp10_02 xp10_03;
	set xp10_01;
	if length(xpdtc) ge 10 then xpdt=input(substrn(xpdtc,1,10),?? yymmdd10.);
	format xpdt date9.;
	if visitnum lt 1999 then output xp10_02;
	else if visitnum=1999 then output xp10_03;
run;

*----------------------------------------------------------------------------------------------------------;
*check if echocardiogram was not performed in any interval of 67 days from treatment start and last visit;
*that is, echo is to be performed atleast once in 67 days interval - if not, it is a deviation - atleast 
one such deviation results in population of this paramcd;
*algorithm is to check the intervals between two performed echos - need to incorporate the duration from
treatment start and last echo to last visit also;
*----------------------------------------------------------------------------------------------------------;

data adsl10_01;
	set adam.adsl(keep=usubjid tr01sdt);
	where tr01sdt ne .;
run;

data xp10_04;
	set xp10_02(in=a)
		adsl10_01(in=b)
		sv10_02(in=c);
		by usubjid;
		*get the first treatment start date and last visit date to get the duration of first echo from treatment start
		and last echo to last visit date respectively;
	if b then do;
		xpdt=tr01sdt;
	end;
	if c then do;
		xpdt=svendt;
	end;
	keep usubjid xpdt visitnum;
run;

*keep only the subjects who are treated;

data xp10_05;
	merge xp10_04(in=a) adsl10_01(in=b keep=usubjid);
	by usubjid;
	if a and b;
run;

data xp10_06(where=( . lt tr01sdt le xpdt le svendt)); *keep only those intervals which are in between treatment start and last visit;
	merge xp10_05(in=a) adsl10_01 sv10_02(keep=usubjid svendt);
	by usubjid;
	if a;
run;

*----------------------------------------------------;
*calculate the duration between two echos;
*----------------------------------------------------;

proc sort data=xp10_06;
	by usubjid xpdt;
run;

data xp10_07;
	set xp10_06;
	by usubjid xpdt;
	previous_date=lag(xpdt);
	if first.usubjid then call missing(previous_date);
	if nmiss(previous_date,xpdt)=0 then duration=xpdt-previous_date;
	format previous_date date9.;
run;

data xp10_08;*this dataset will contain the subjects who do not have echo performed in the required 67 days interval;
	set xp10_07;
	by usubjid ;
	where duration gt 67;
	if last.usubjid;
run;

*----------------------------------------------------------------------------------------------------------------;
*second condition is to check if the subject had a 1999 visit and corresponding echo in xp dataset on 1999 visit;
*----------------------------------------------------------------------------------------------------------------;

data xp10_09 xp10_10;
	merge sv10_03(in=a) xp10_03(in=b) adsl10_01;
	by usubjid;
	if missing(xpdt) and not missing(tr01sdt) then output xp10_10;*this dataset will contain the subjects who do not have
	echo performed at visit 1999;
	output xp10_09;
run;

*---------------------------------------------------------------------------------------------------------------;
*if the subject is present in xp10_10(no 1999 echo) or xp10_08(atleaset one echo not in 67 days) dataset then
the subject qualifies for prdevi10 paramcd;
*---------------------------------------------------------------------------------------------------------------;

data prdevi10_pre;
	set xp10_10 xp10_08;
	by usubjid;
	if last.usubjid;
	keep usubjid;
run;

data prdevi10;
	merge prdevi10_pre(in=a) adam.adsl(in=b);
	by usubjid;
	if a;
   %lengths;
   paramcd="PRDEVI10";
   param="MISSING ECHOCARDIOGRAM DEVIATION";
   parcat1="PROGRAMMABLE CRITERIA";
   parcat1n=3;
   avisitn=visitnum;
   avalc= "Missing ECHO";
   asev="MAJOR";
   keep &subjvars ;
run;



		
		




