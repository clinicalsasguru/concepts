*================================================================;
*this macro is used to sort the input dataset based
on the by variable given;
*parameters:
-------------------------------------------------
indsn	: input dataset for sorting
outdsn	: output dataset name after sorting
byvar	: list of by variables for sorting order
nodupkey: pass 'nodupkey' as value to this parameter if you want to delete
		  the duplicate values.
--------------------------------------------------
Note: if nodupkey is used then it creates a dataset with name dups_&outdsn
containing the deleted duplicate values

example macro call:
%sort(indsn=sashelp.class,outdsn=class,byvar=name,nodupkey=nodupkey);
*===================================================================;


%macro _sort(indsn=, outdsn=, byvar=, nodupkey=);

%if &nodupkey= %then %do;
proc sort data=&indsn out=&outdsn;
	by &byvar.;
run;
%end;

%else %if %upcase(&nodupkey)=NODUPKEY %then %do;

proc sort data=&indsn out=&outdsn dupout=dups_&outdsn. nodupkey;
	by &byvar;
run;

%end;

%mend;

