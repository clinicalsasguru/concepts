proc sort data=sashelp.class out=class;
by age;
run;

data class;
	set class;
	by age;
	if first.age then ord=1;
	else ord+1;
run;
