dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

libname adam "D:\Home\dev\cdisc_data_setup";

data class;
	set sashelp.class;
	where age=16 and sex="F";
run;

