dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;


data cars;
	set sashelp.cars;
run;

*question-get the number of non-missing observartions in a dataset;

proc sql;
	create table total_cars as
		select count(*) as total_cars_no
		from cars;
quit;


*question-get the number of distinct car make-s from the dataset(number of unique manufacturers);

proc sql;
	create table distinct_cars as
		select count(distinct make) as distinct_car_no
		from cars
		where not missing(make);
quit;

*note: without distinct option sql will count the number of non-missing values in a variable;


data cars2;
	set cars;
	if substr(make,1,1)="A" then make="";
run;

proc sql;
	create table missing as
	select count(*) as missing_make
	from cars2
	where missing(make);
quit;

/*data test;*/
/*set total_cars;*/
/*length label $100;*/
/*label="Total number of cars";*/
/*run;*/

*question- create a character variable 'label' of length 100 in an existing dataset;

proc sql;
	create table total_cars2 as
		select *, "Total number of cars" as label length=100
		from total_cars;
quit;

proc sql;
	create table distinct_cars2 as
	select *,"Number of unique car manufactrers" as label length=100
	from distinct_cars;
quit;

*select only few variables using sql;

proc sql;
	create table cars_sql as
	select make,cylinders
	from sashelp.cars;
quit;


*combine above two questions;

proc sql;
	create table distinct_cars3 as
		select count(distinct make) as distinct_cars_no,
		"Number of unique car manufacturers" as label length=100,
		1 as ord
		from cars
		where not missing(make);
quit;

*question-count the number of cars within in each manufacture;

proc sql;
	create table no_of_cars as 
	select make,count(*) as no_of_cars
	from cars
	where not missing(make)
	group by make;
quit;

*(above counts can be obtained using first. and last. concepts);

*question-count the number of cars within each cylinder type within each make;

proc sql;
	create table no_of_cars2 as
		select make, cylinders,count(*) as no_of_cars
		from cars
		where not missing(make) and not missing(cylinders)
		group by make,cylinders;
quit;

****usage:
these are similar concepts which can be used in any of tables which need counts and percentages;
