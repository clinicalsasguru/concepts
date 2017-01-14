dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

*--------------------------------------------------------;
*creating sample data;
*--------------------------------------------------------;

data dates;
	length date $10 ;
	infile cards truncover;
	input usubjid date$;
cards;

1 2010
2 2010-07
3 2010-02
4 2010-12
5 2010-01-01
;
run;

data trtsdt;
	input subjid trtsdt yymmdd10.;
	format trtsdt yymmdd10.;
cards;
1 2010-06-01
2 2010-06-01
3 2010-06-01
4 2010-06-01
5 2010-06-01
;
run;


*------------------------------------------------------;
*separate the date into year, month and date;
*------------------------------------------------------;

data dates2;
	set dates;
	if length(date)=10 then do;
	year=substr(date,1,4);
	month=substr(date,6,2);
	day=substr(date,9,2);
	end;

	else if length(date)=7 then do;
	year=substr(date,1,4);
	month=substr(date,6,2);
	end;

	else if length(date)=4 then do;
	year=substr(date,1,4);
	end;
run;


*========================================================================;
*Imputation Rules:(set 1)
1. If year is missing then year=2010
2. If month is missing then month=Jan
3. If day is missing then day=01;
*========================================================================;

data dates3;
	set dates2;
	if not missing(year) and not missing(month) and not missing(day) then imp_date=catx('-',year,month,day);
	else if not missing(year) and not missing(month) and missing(day) then imp_date=catx('-',year,month,'01');
	else if not missing(year) and missing(month) and missing(day) then imp_date=catx('-',year,'01','01');
	else if missing(year) and missing(month) and missing(day) then imp_date=catx('-','2010','01','01');
run; 

*========================================================================;
*Imputation Rules:(set 2)
1. If year is missing then year=2010
2. If month is missing then month=Dec
3. If day is missing then day=last day of the month
4. If month and day are missing then month=DEC and day=31
*========================================================================;

*---------------------------------;
*intnx function;
*---------------------------------;

data test;
	char_date="2010-12";
	year=substr(char_date,1,4);
	month_pre=substr(char_date,6,2);
     if upcase(strip(month_pre))= "01" then month ="JAN";
     else if upcase(strip(month_pre))= "02" then month ="FEB";
     else if upcase(strip(month_pre))= "03" then month ="MAR";
     else if upcase(strip(month_pre))= "04" then month ="APR";
     else if upcase(strip(month_pre))= "05" then month ="MAY";
     else if upcase(strip(month_pre))= "06" then month ="JUN";
     else if upcase(strip(month_pre))= "07" then month ="JUL";
     else if upcase(strip(month_pre))= "08" then month ="AUG";
     else if upcase(strip(month_pre))= "09" then month ="SEP";
     else if upcase(strip(month_pre))= "10" then month ="OCT";
     else if upcase(strip(month_pre))= "11" then month ="NOV";
     else if upcase(strip(month_pre))= "12" then month ="DEC";

	 intnx=intnx('month',input(cats('01',month,year),date9.),0,'e');

	 format intnx date9.;
run;


data dates4;
	set dates2(rename=(month=month_pre));
     if upcase(strip(month_pre))= "01" then month ="JAN";
     else if upcase(strip(month_pre))= "02" then month ="FEB";
     else if upcase(strip(month_pre))= "03" then month ="MAR";
     else if upcase(strip(month_pre))= "04" then month ="APR";
     else if upcase(strip(month_pre))= "05" then month ="MAY";
     else if upcase(strip(month_pre))= "06" then month ="JUN";
     else if upcase(strip(month_pre))= "07" then month ="JUL";
     else if upcase(strip(month_pre))= "08" then month ="AUG";
     else if upcase(strip(month_pre))= "09" then month ="SEP";
     else if upcase(strip(month_pre))= "10" then month ="OCT";
     else if upcase(strip(month_pre))= "11" then month ="NOV";
     else if upcase(strip(month_pre))= "12" then month ="DEC";


	if not missing(year) and not missing(month) and not missing(day) then imp_date=catx('-',year,month_pre,day);

	else if not missing(year) and not missing(month) and missing(day) 
		then imp_date=put(intnx('month',input(cats('01',month,year),date9.),0,'e'),yymmdd10.);

	else if not missing(year) and missing(month) and missing(day) 
		then imp_date=catx('-',year,'12','31');

	else if missing(year) and missing(month) and missing(day) then imp_date=catx('-','2010','12','31');


run;
