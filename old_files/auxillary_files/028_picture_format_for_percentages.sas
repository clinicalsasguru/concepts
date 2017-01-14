dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

proc format;
	picture prct (default=7)
	0='9.9)' (prefix="(")
	0<-<100= '09.9)' (prefix="(")
	100='0999)' (prefix="(");
run;

*Nine prints zero and zero prints nothing;

data test;
	percent=0; output;
	percent=1.1; output;
	percent=10.9; output;
	percent=50; output;
	percent=99.9; output;
	percent=100; output;
run;

data test2;
	set test;
	length char1 char2 $10;
	char1=put(percent,prct.);

char2="("||put(percent,5.1)||")"; *(  0.0) vs (0.0);

/*	if percent ne 100 then char2="("||put(percent,5.1)||"%)";*/
/*	else char2="("||put(percent,3.)||"%)";*/

run;

/*low-100="cat1"*/
/*100<-<120="cat2"*/
/*120-high="cat3";*/

proc print data=test2;
run;

proc format;
	picture phone(default=13)
	low-high='999)999-9999' (prefix="(");
run;

data phone;
	phone=1234567890;
	char=put(phone,phone.);
run;

