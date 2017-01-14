dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill;
quit;

data cars;
	set sashelp.cars;
run;

*====================================================;
*count the number of cars in each make;
*====================================================;

*check distinct values of make;

proc sql;
	select distinct make
	from cars;
quit;

*check the number of distinct make-s;

proc sql;
	select count(distinct make)
	from cars;
quit;

proc sort data=cars;
	by make;
run;

data cars2;
	set cars;
	by make;
	if first.make then makeord=1; *reset the count to 1 for every first record in each make type;
	else makeord+1;*not makeord=makeord+1, use retain for makeord;

	if last.make; *keep only the last record to have a total count of each make;

	keep make makeord;
run;

*========================================================================;
*give a unique numeric number for each make(alphabetical a-1 to z-26)
*========================================================================;

proc sort data=cars;
	by make;
run;

data cars3;
	set cars;
	by make;
	if first.make then makeord+1;*increment by one only when a new make is encountered;
	if first.make;
	keep make makeord;
run;


proc sort data=cars;
	by descending make;
run;

data cars3_2;
	set cars;
	by descending make;
	if first.make then makeord+1;
	keep make makeord;
	if first.make;
run;

*usage:
can be used to create order variables in ae and cm tables(when required);

data ae_pre;
	input aedecod$ count;
	cards;
ae5	7
ae4	5
ae2	3
ae1	2
ae6	2
ae3	1
;
run;

proc sort data=ae_pre;
	by descending count aedecod;
run;

/*data ae;*/
/*	set ae_pre;*/
/*	by descending count aedecod;*/
/*	if first.count then decod_ord=1;*/
/*	else decod_ord+1;*/
/*run;*/

