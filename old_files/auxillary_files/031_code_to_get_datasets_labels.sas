dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill;
quit;


proc sql;
	create table test as
	select memname as dataset, memlabel as description from 
	dictionary.tables 
	where libname="TMP1";
quit;

%let fileloc=%sysget(homedrive)\users\%sysget(username)\desktop;

%put &user.;

proc export data=test outfile="&fileloc.\dataset_labels.csv";
run;
