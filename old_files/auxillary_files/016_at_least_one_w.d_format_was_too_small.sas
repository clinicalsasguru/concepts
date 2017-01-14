dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;


*at leas one w.d format was too small;

data test;
	num1=1000;
	char1=put(num1,3.);
run;

data test2;
	num=100.12;
/*	char1=put(num,4.1);*/
/*	char2=put(num,5.1);*/
	char3=put(num,5.2);
/*	char4=put(num,6.2);*/
run;

