dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;


data date1;
date1="11Dec2014"d; *date constant;
format date1 yymmdd10.;
/*format date1 date9.;*/
run;

*------------------------------------------------;
*extract date, month, year from numeric date;
*-------------------------------------------------;

data date2;
	set date1;
	if not missing(date1) then do;
		year=year(date1);
		month=month(date1);
		day=day(date1);
	end;
run;

*------------------------------------------------;
*extract date, month, year from character date;
*-------------------------------------------------;

data date3;
	set date1;
	length chardate1 $10.;
	if not missing(date1) then chardate1=put(date1,yymmdd10.);
	keep chardate1;
run;

data date4;
	set date3;
	length year $4 month day $2;
	year=substr(chardate1,1,4);
	month=substr(chardate1,6,2);
	day=substr(chardate1,9,2);
run;

*-----------------------------------------------------------------;
*extract date, month, year as numeric vars from character date;
*-----------------------------------------------------------------;

data date5;
	set date3;
	year=input(substr(chardate1,1,4),best.);
	month=input(substr(chardate1,6,2),best.);
	day=input(substr(chardate1,9,2),best.);
run;


*--------------------------------------------------------------------------;
*extract date, month, year as character variables from numeric date;
*--------------------------------------------------------------------------;

data date6;
	set date1;
	length year $4 month day $2;
	if not missing(date1) then do;
		year=put(year(date1),4.);
		month=put(month(date1),2.);
		day=put(day(date1),2.);
	end;
run;

*============================================================;
*character dates - level2 (partial dates);
*============================================================;

data date7;
	length date $10;
	date="2012";
	output;
	date="2012-12";
	output;
	date="2012-12-11";
	output;
	date="";
	output;
run;

*-----------------------------------------------------;
*extract year, month, day as character variables;
*-----------------------------------------------------;

data date8;
	set date7;
	length year $4 month day $2;
	if not missing(date) then do;
	year=substr(date,1,4);
	month=substr(date,6,2);
	day=substr(date,9,2);
	end;
run;

*-----------------------------------------------------;
*extract year, month, day as numeric variables;
*-----------------------------------------------------;

data date9;
	set date7;
	if not missing(date) then do;
	year=input(substr(date,1,4),best.);
	month=input(substr(date,6,2),best.);
	day=input(substr(date,9,2),best.);
	end;
run;

*====================================================================;
*combining the components to full date;
*====================================================================;

data date10;
	day=11;
	month=12;
	year=2012;
run;

data date11;
	set date10;
	date=mdy(month,day,year);
	format date date9.;
run;

data date12;
	day=11;
	month=12;
	year=2012;
	output;

	day=.;
	month=12;
	year=2012;
	output;

	day=.;
	month=.;
	year=2012;
	output;

	day=.;
	month=.;
	year=.;
	output;
run;

data date13;
	set date12;
	if nmiss(month,day,year)=0 then do;
		date=mdy(month,day,year);
	end;

/*	if n(month,day,year)=3 then do;*/
/*		date=mdy(month,day,year);*/
/*	end;*/

	format date date9.;
run;

*---------------------------------------------------;
*year missing -2012
*month missing -july(07)
*date missing - 01;
*---------------------------------------------------;

data date14;
	set date12;
	if missing(year) then year=2012;
	if missing(month) then month=07;
	if missing(day) then day=01;
	
	date=mdy(month,day,year);
	format date date9.;
run;



*=======================================================;
*get last day of the month;
*=======================================================;

data date15;
	length date $10;
	date="";
	output;
	date="2010";
	output;
	date="2012-01";
	output;
	date= "2011-02";
	output;
	date="2012-02";
	output;
	date="2012-04";
	output;
	date="2012-12";
	output;
run;

data date16;
	set date15;
/*	length date_temp $10;*/
/*	date_temp=strip(date)||"-01";*/
/*	date_temp2=input(date_temp,yymmdd10.);*/
/*	format date_temp2 req_date1 date9.;*/
/*	req_date1=intnx('month',date_temp2,0,'end');*/
/*	if length(date)=7 then do;*/
/*	if nmiss(year,month)=0 and missing(day) then do;*/

	date=put(intnx('month',input(catx('-',date,"01"),yymmdd10.),0,'end'),yymmdd10.);

/*	end;*/

run;

	
