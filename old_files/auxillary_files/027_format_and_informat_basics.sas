dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

*-----------------------------------------------;
*basic format - for numeric variable;
*-----------------------------------------------;

proc format;
	value sex_4_num
		2="Male"
		1="Female"
		;
run;

data class;
	set sashelp.class;
	if sex="F" then sexn=1;
	else if sex="M" then sexn=2;

	sex_desc_num=put(sexn,sex_4_num.);
run;

*-------------------------------------------------;
*basic format - for character variable;
*-------------------------------------------------;

proc format;
	value $sex_4_char
		"F"="Female"
		"M"="Male";
run;

data class2;
	set class;
	sex_desc_char=put(sex,$sex_4_char.);
run;


*----------------------------------------------------;
*defining default length for the format applied var;
*----------------------------------------------------;

proc format;
	value $sex_4_char (default=10)
		"F"="Female"
		"M"="Male";
run;

data class3;
	set class2;
	sex_desc_char2=put(sex,$sex_4_char.);

	length sex_desc_char3 $15;
	sex_desc_char3=put(sex,$sex_4_char.);

	sex_desc_char4=put(sex,$sex_4_char25.);
run;

*---------------------------------------------------;
*creating a dataset from existing formats;
*---------------------------------------------------;

proc format cntlout=sex_formats;
run;

*---------------------------------------------------;
*retrieve formats from a saved dataset;
*---------------------------------------------------;

proc format cntlin=sex_formats;
run;

*--------------------------------------------------;
*creating formats from dataset;
*--------------------------------------------------;

proc import datafile="D:\Github\project_vs\auxillary_files\027_format_and_informat_basics.xlsx"
		out=formats_pre replace;
		sheet=sheet1;
run;

data formats_pre2;
	set formats_pre;
	rename value=label code=start;
run;

proc format cntlin=formats_pre2;
run;

data race;
	do racen=1 to 5,99;
	output;
	end;
run;

data race2;	
	set race;
	race=put(racen,race.);
run;

*----------------------------------------------------;
*create a informat - to create a numeric variable;
*----------------------------------------------------;

proc format;
	invalue sex
		"F"=1
		"M"=2;
run;

data class4;
	set sashelp.class;
	sexn=input(sex,sex.);
run;

*----------------------------------------------------;
*create a informat - to create a character variable;
*----------------------------------------------------;

proc format;
	invalue $sex
		"F"=1
		"M"=2;
run;

data class5;
	set sashelp.class;
	sexn=input(sex,$sex.);
run;


*----------------------------------------------------;
*create a informat - to create a character variable;
*----------------------------------------------------;

proc format;
	invalue $sex_
		"M"="Male"
		"F"="Female"
		;
run;

data class6;
	set sashelp.class;
	sexn=input(sex,$sex_.);
run;


*--------------------------------------------------------;
*export informats to a dataset;
*--------------------------------------------------------;

proc format cntlout=informats_pre;
run;

data temp_sex_informat;
	set informats_pre;
	where upcase(fmtname)="SEX" and type="I";
run;

proc format cntlin=temp_sex_informat;
run;

data class7;
	set sashelp.class;
	sexn=input(sex,sex.);
run;

*--------------------------------------------------;
*creating informat from dataset;
*--------------------------------------------------;

data test1;
	name="curious";
	output;
	name="kid";
	output;
	name="gmail";
	output;
run;


data temp_sex_informat;
	set formats_pre2(rename=(start=start_ label=label_));
	where upcase(fmtname)="NAME";
	label=start_;
	start=label_;
	type="I";
run;

proc format cntlin=temp_sex_informat;
run;

data test2;
	set test1;
	number=input(name,name.);
run;
