dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

libname sdtm "D:\Home\dev\compound1\data\shared\sdtm";

*===============================================================;
*using cm and suppcm data instead of mh and suppmh data;
*===============================================================;

data cm;
	set sdtm.cm;
run;

data suppcm;
	set sdtm.suppcm;
run;

data suppcm2;
	set suppcm;
	length atc4term $ 200;
	if index(qnam,"ATC4TERM") gt 0 and index(idvar,"CMSEQ") gt 0;
	atc4term=qval;
	cmseq=input(idvarval,best.);
	keep usubjid atc4term cmseq;
run;

proc sort data=cm;
	by usubjid cmseq;
run;

proc sort data=suppcm2;
	by usubjid cmseq;
run;

data cm2;
	merge cm(in=a) suppcm2;
	by usubjid cmseq;
	if a ;
run;
