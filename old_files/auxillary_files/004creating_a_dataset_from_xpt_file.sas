dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

libname xpt xport "D:\Home\dev\cdisc_data_setup\adae.xpt";

libname kid "D:\Home\dev\cdisc_data_setup";

*create a libname pointing to xpt file (notice the word xport- that is the trick);

proc copy in=xpt out=work;
run;

data kid.adae;
	set adae;
run;
