dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

*-------------------------;
*wide to long;
*-------------------------;

*example 1;

DATA wide; 
  input famid faminc96 faminc97 faminc98 ; 
CARDS; 
1 40000 40500 41000 
2 45000 45400 45800 
3 75000 76000 77000 
; 
RUN; 

data long1;
	set wide;
	year=96;
	faminc=faminc96;
	output;

	year=97;
	faminc=faminc97;
	output;

	year=98;
	faminc=faminc98;
	output;
	keep famid year faminc;
run;

data long2;
	set wide;
	array fam[96:98] faminc96 faminc97 faminc98;

	do year=96 to 98;
	faminc=fam(year);
	output;
	end;
	keep famid year faminc;
run;




*example 2;

data wide2 ; 
  input famid faminc96 faminc97 faminc98 spend96 spend97 spend98 ; 
cards ; 
1 40000 40500 41000 38000 39000 40000 
2 45000 45400 45800 42000 43000 44000 
3 75000 76000 77000 70000 71000 72000 
; 
RUN ;

data long22;
	set wide2;
	array fam[96:98] faminc96 faminc97 faminc98;
	array exp[96:98] spend96 spend97 spend98;

	do year=96 to 98;
	faminc=fam(year);
	spend=exp(year);
	output;
	end;

	keep famid year faminc spend;
run;


*example 3;

data raw;
subject=1; visit=1; sbp=140; sbpu="mmHg"; dbp=90; dbpu="mmHg"; pulse=72; pulseu="bpm";output;
subject=2; visit=1; sbp=144; sbpu="mmHg"; dbp=80; dbpu="mmHg"; pulse=62; pulseu="bpm";output;
run;

data long23;
	set raw;
	array res[3] sbp dbp pulse;
/*	array units[3] sbpu dbpu pulseu;*/
	array tests[3]  $10 _temporary_ ("SBP" "DBP" "Pulse");
	array units[3] $10 _temporary_ ("mmHg" "mmHg" "bpm");
	do i=1 to 3;
	testcd=tests(i);
	aval=res(i);
	unit=units(i);
	output;
	end;
	keep subject visit aval unit testcd;
run;

