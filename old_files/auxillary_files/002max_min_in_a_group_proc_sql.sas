dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets lib=work mt=data kill nolist;
quit;

data cars;
	set sashelp.cars;
run;

*===========================================================;
*obtain max/min values within each group using proc sql;
*===========================================================;

*question- obtain the maximum no. of cylinders in each group/make;

proc sql;
	create table max as 
	select make,max(cylinders) as max_cyl
	from cars
	group by make;
quit;

*question- obtain the minimum no. of cylinders in each group/make;

proc sql;
	create table min as 
	select make,min(cylinders) as min_cyl
	from cars
	group by make;
quit;


*question- obtain the average no. of cylinders in each group/make;

proc sql;
	create table avg as 
	select make,put(avg(cylinders),10.1) as avg_cyl
	from cars
	group by make;
quit;

