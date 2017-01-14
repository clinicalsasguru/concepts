data class;
	set sashelp.class;
	length sex_desc $10;
	sex_desc=ifc(sex="M","Male","Female");
	height_cat=ifn(height gt 60,1,2);
run;
