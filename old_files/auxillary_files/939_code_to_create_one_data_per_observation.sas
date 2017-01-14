%macro dsn(name);
data &name;
	set sashelp.class;
	where name="&name.";
run;
%mend;

data _null_;
	set sashelp.class;
	call execute('%dsn('||name||');');
run;
