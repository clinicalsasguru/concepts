dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

libname adam "D:\Home\dev\cdisc_data_setup";

options nonumber nodate nocenter ps=48 ls=153;

proc report data=adam.adsl nowd headline headskip spacing=0;
	column siteid usubjid trtp;
	define siteid/  order noprint;
	define usubjid/"Subject" width=20;
	define trtp/"Treatment" width=40;

/*	break after siteid/page;*/

	compute before _page_;
		line @1 153*'-';
	endcomp;

	compute after _page_;
		line @1 153*'-';
	endcomp;

	compute before siteid;
		line @1 "";
		line @1 "Investigator Site = " siteid $153. ;
		line @1 "";
	endcomp;


run;
