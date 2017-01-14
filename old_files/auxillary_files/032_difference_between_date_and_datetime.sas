dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

data test;
	date="01Jan2015"d;
	datetime="01Jan2015:8:45:15"dt;
	
	date2=datepart(datetime);

	datec=put(date,yymmdd10.);
	datetimec=put(datetime,yymmdd10.);
	date2c=put(date2,yymmdd10.);
run;

