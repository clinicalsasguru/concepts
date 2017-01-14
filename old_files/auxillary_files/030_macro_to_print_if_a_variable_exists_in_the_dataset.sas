%macro checkvar(dsn, var);
proc contents data=&dsn out=_content_ noprint;
run;

proc sql noprint;
	select count(*) into :_content_ from _content_
	where upcase(name)="%upcase(&var.)";
quit;

%put &_content_;

%if &_content_ ne 0 %then %put NOTE:******************* %upcase(&var)  EXISTS in the dataset*****************;

%else %put NOTE: ****************** %upcase(&var) IS NOT PRESENT in the dataset*************************;

proc sql;
	drop table _content_;
quit;

%mend;

%checkvar(dm,usubjid);


