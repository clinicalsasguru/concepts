dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

proc print data=sashelp.class;
run;

proc freq data=sashelp.class noprint;
	tables age*sex/out=sexcounts;
run;

proc sql;
create table sexcounts2 as
	select count(distinct name) as count,age,sex
	from sashelp.class
	group by age,sex;
quit;


data sexdummy;
	length sex $1;
	count=0;
	do age=11 to 16;
		do sex="F","M";
			output;
		end;
	end;
run;

proc sort data=sexdummy;
	by age sex;
run;

proc sort data=sexcounts2;
	by age sex;
run;

data sexcounts3;
	merge sexdummy sexcounts2;
	by age sex;
run;

/*			
class trtpn sex;
class trtpn;
class sex/preloadfmt;
*/

/*
tables trtpn*sex;
*/

proc format;
	value age
	11=11 12=12 13=13 14=14 15=15 16=16;

	value $sex
	"F"="F"
	"M"="M";
run;

proc summary data=sashelp.class completetypes nway;
class age/preloadfmt;
class sex/preloadfmt;
format age age. sex $sex.;
output out=class_stats;
run;
