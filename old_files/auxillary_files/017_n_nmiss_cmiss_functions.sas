dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

*--------------------------------------------------------;
*creating sample data;
*--------------------------------------------------------;

data test;
	input num1-num10 (char1-char10) ($1. +1);
cards;
1 2 . 4 . 6 . 8 9 0 a . c . e . g . i
;
run;

*-----------------------------------------------------;
*n-function - gives number of variables with 
non-missing values;
*can be applied only on numeric variables;
*-----------------------------------------------------;

data n;
	set test;
	nonmissingnum=n(num1,num2,num3,num4,num5,num6,num7,num8,num9,num10);
	nonmissingnum2=n(of num1-num10);
	nonmissingnum3=n(of num1-num3, of num7-num10);

	keep num: non:;
run;

*question- add the responses only if the number of non-missing values are gt 5;

data n2;
	set test;
	if n(of num1-num10) gt 5 then sum=sum(of num1-num10);
run;

*question- calcualte the study day only if non-missing value dates are present;

data n3;
	aestdt='10jan2010'd;
	trtsdt='03jan2010'd;

	format aestdt trtsdt date9.;

	if not missing(aestdt) and not missing(trtsdt) then 
		studyday=aestdt-trtsdt;

	if n(aestdt,trtsdt)=2 then studyday2=aestdt-trtsdt;
run;


*------------------------------------------------------------------;
*nmiss-function - gives number of variables with missing values;
*can be applied only on numeric variables;
*------------------------------------------------------------------;

data nmiss;
	set test;
	missingnum=nmiss(num1,num2,num3,num4,num5,num6,num7,num8,num9,num10);
	missingnum2=nmiss(of num1-num10);

	keep num: missing:;
run;

*question- add the responses only if the number of missing values are lt 6;

data nmiss2;
	set test;
	if nmiss(of num1-num10) lt 6 then sum=sum(of num1-num10);
run;

*question- calcualte the study day only if non-missing values dates are present;

data nmiss3;
	aestdt='10jan2010'd;
	trtsdt='03jan2010'd;
	format aestdt trtsdt yymmdd10.;

	if not missing(aestdt) and not missing(trtsdt) then 
		studyday=aestdt-trtsdt;

	if nmiss(aestdt,trtsdt)=0 then studyday2=aestdt-trtsdt;
run;


*-------------------------------------------------------------------;
*cmiss function- returns the number of variables with missing values;
*applicable only to character variables;
*-------------------------------------------------------------------;

data cmiss;
	set test;
	missingchar=cmiss(char1,char2,char3,char4,char5,char6,char7,char8,char9,char10);
	missingchar2=cmiss(of char1-char10);
/*	character=*/
	keep char: missing:;
run;





