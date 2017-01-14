%macro flatfilc
(lib=, /* libref for input dataset */
dsn=, /* memname for input dataset */
file=); /* filename of output file */
%let lib=%upcase(&lib);
%let dsn=%upcase(&dsn);
proc sql noprint;
select quote(strip(name)),
quote(case when label ne ' ' then
strip(label)
else strip(name)
end),
name
|| case when format ne ' ' then format
when type='num' then 'Best10.'
else "$"||put(length,z3.)||'.'
end
into :names separated by ' "09"x ',
:labels separated by ' "09"x ',
:string separated by ' "09"x '
from dictionary.columns
where libname = "&lib"
and memname = "&dsn";
quit;
data _null_;
set &lib..&dsn;
file print;
if _n_=1 then put &names / &labels;
put &string.;
run;
%put &names. &labels. ;
%mend;

%flatfilc
(lib=SASHELP, /* libref for input dataset */
dsn=CLASS, /* memname for input dataset */
file=);
