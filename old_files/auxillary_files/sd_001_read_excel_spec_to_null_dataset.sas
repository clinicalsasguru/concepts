*-----------------------------------------------------------------------------------------;
*Further additions;
*check for max length of each variable and change the length of the variable;
*decide to drop the variable based on the role and value level data in each dataset -
	not feasible as spec does not capture the cdisc role of the variable ;
*read in the supp dataset, if availabe, and create the blank dataset there itself;
*keep the variables available in the source dataset only and convert the supp based variables
to suppdataset based on the blank supp dataset.
*-----------------------------------------------------------------------------------------;


dm log 'clear';

libname xxxx "D:\Home\dev\master_files\copied\xxxxx_sdtm\Spec";

*------------------------------------------------------------------------------;
*create attributes from the read spec;
*------------------------------------------------------------------------------;

data shell01;
	set anil.ae_spec;
	where remove ne "Y";
	length text $1000 dollar $10;
	if strip(upcase(sastype))="C" then dollar="$";
	text= strip(variable)||"  length="||strip(dollar)||strip(put(saslength,best.))||"  label ='"||strip(label)||"'";
	*keep order variable text;
run;

proc sort data=shell01;
	by order;
run;

*-----------------------------------------------------------------------------------;
*put attiributes into a macro variable to use in attibute statment;
*-----------------------------------------------------------------------------------;
proc sql noprint;
	select text into :varattrs  separated by "%sysfunc(byte(10))"
	from shell01
	;

	select distinct variable into :varlist separated by "*" 
	from shell01;
quit;

*------------------------------------------------------------------------------------;
*macro to create statments to assign null values to all the variables;
*------------------------------------------------------------------------------------;
%macro assign_missing_vals;
	%let count=%eval(%sysfunc(countw(&varlist.,*)));
	%put &count.;
	%do i=1 %to &count;
	%let var=%scan(&varlist,&i,*);
		if missing(&var.) then call missing(&var.);
	%end;
%mend assign_missing_vals;


*--------------------------------------------------------------------------------------;
*step that creates null dataset;
*--------------------------------------------------------------------------------------;

data ae;
attrib
&varattrs
;*gets all the variable attributes in order of the required final dataset;

%assign_missing_vals; *produces a block of statments to assign null values to each variable;

if 0;*statement always executes as false and ensures no blank (one observation with all missing values) record is output;

run;

proc contents data=ae varnum;
run;
