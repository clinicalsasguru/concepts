dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

data cars;
	set sashelp.cars;
run;

*question-get the total number of observation in a dataset to a macro variable;

proc sql noprint;
	select count(*) into :nobs from cars;
quit;

%put number of observation in cars dataset is &nobs.;


*question-get the number of distinct values present in a variable into a macro variable;

proc sql noprint;
	select count(distinct make) into :nmake from cars;
quit;

%put number of unique car manufacturers &nmake;

*question- get the number of distinct values present which start with 'A';

proc sql noprint;
	select count(distinct make) into :nmake_a from cars
	where substr(make,1,1)="A";
quit;

%put unique manufacturers with "A" &nmake_a;


*question-get the list of unique manufacturers into a macro variable;

proc sql noprint;
	select distinct make into :dist_make separated by ' '
	from cars;
quit;

%put &dist_make.;

*question-get a similar list (as above) separated by comma;

proc sql noprint;
	select distinct make into :dist_make2 separated by ','
	from cars;
quit;

%put &dist_make2;

*question - a series of macro variables (number of cars in each make into a separate macro variable);

proc sql noprint;
	select count(*) into :n1 - :n38 
	from cars
	group by make;
quit;

%put &n1;
%put &n15;
%put &n38;

%put &n1 - &n38;

*usage:
creating macro variables for column headers, and also for calculating percentages etc;









