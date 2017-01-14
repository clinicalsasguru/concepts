dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;


data ae;
	length pterm $200;
	input usubjid aeseq pterm$;
cards;
1 1 pterm1
1 2 pterm2
2 1 pterm3
3 1 pterm4
4 1 pterm20
;
run;

data cm;
	length cmterm $200;
	input usubjid cmseq cmterm$;
cards;
1 1 cmtermxx
1 10 cmterm2
2 12 cmterm6
2 13 cmterm8
3 1  cmterm1
3 2 cmterm2
3 4 cmterm3
3 5 cmterm4
;
run;

data relrec;
	input usubjid  rdomain$ idvar$ idvarval$ relid;
cards;
1 AE aeseq 1 1
1 CM cmseq 1 1
1 AE aeseq 2 2
1 CM cmseq 1 2
2 AE aeseq 1 1
2 CM cmseq 12 1
2 CM cmseq 13 1
3 AE aeseq 1 1
3 DS dsseq 2 1
3 CM cmseq 1 1
3 CM cmseq 2 1
3 CM cmseq 4 1
3 CM cmseq 5 1
;
run;

*question1 - get the concomitant medications used for an adverse event;
*question2 - if there are multiple medications used for a single event then 
	concatenate each of them separated by comma (,) and make a single record for an event;

*===============================================================;
*processing relrec dataset;
*===============================================================;

*---------------------------------------------;
*subset AE and CM based records from relrec;
*---------------------------------------------;

proc sort data=relrec out=rel_aecm;
	by usubjid relid;
	where upcase(rdomain) in ("AE" "CM") and upcase(idvar) in ("CMSEQ" "AESEQ");
run;

*------------------------------------------;
*subset ae based records from rel_aecm;
*subset ae based records from rel_aecm;
*------------------------------------------;

data rel_ae rel_cm;
	set rel_aecm;
	if upcase(rdomain)="AE" then output rel_ae;
	if upcase(rdomain)="CM" then output rel_cm;
run;

*-----------------------------------------;
*create aeseq in rel_ae dataset;
*-----------------------------------------;

data rel_ae2;
	set rel_ae;
	aeseq=input(idvarval,best.);
	keep usubjid relid aeseq;
run;


*----------------------------------------;
*create cmseq in rel_cm dataset;
*----------------------------------------;

data rel_cm2;
	set rel_cm;
	cmseq=input(idvarval,best.);
	keep usubjid relid cmseq;
run;

*---------------------------------------------------;
*ae and cm records are related in relrec by relid;
*merge the above datasets by relid;
*---------------------------------------------------;

proc sort data=rel_ae2;
	by usubjid relid;
run;

Proc sort data=rel_cm2;
	by usubjid relid;
run;

data aeandcm;
	merge rel_ae2(in=a) rel_cm2(in=b);
	by usubjid relid;
	*keep only the records which are found in both the datasets;
	*ie., only the a cm record which is related to ae has to be kept;
	if a and b;
run;

*------------------------------------------------------------------;
*now we have individual cm sequences (link to get the cmterms);
*merge this with cm dataset based on cmterms;
*------------------------------------------------------------------;

proc sort data=cm out=cm2(keep=usubjid cmseq cmterm) ;
	by usubjid cmseq ;
run;

proc sort data=aeandcm;
	by usubjid cmseq;
run;

data cmterms;
	merge aeandcm(in=a) cm2(in=b);
	by usubjid cmseq;
	if a ;
run;

*-----------------------------------------------------------;
*now we will have all the possible cmterms obtained;
*If multiple medications are used for a single AE then concatenate 
all the terms and create a single record for AE;
*-----------------------------------------------------------;

proc sort data=cmterms;
	by usubjid aeseq;
run;

data cmterms2;
	set cmterms;
	by usubjid aeseq;

	retain cmterms_conc;

	length cmterms_conc $400;
	if first.aeseq then call missing(cmterms_conc);
	cmterms_conc=catx(', ',cmterms_conc,cmterm);
	if last.aeseq;
	drop cmterm;
run;


*-----------------------------------------------------------------;
*get the concatenated cmterms onto the ae dataset;
*-----------------------------------------------------------------;

proc sort data=cmterms2;
	by usubjid aeseq;
run;

proc sort data=ae;
	by usubjid aeseq;
run;

data ae2;
	merge ae(in=a) cmterms2(in=b keep=usubjid aeseq cmterms_conc);
	by usubjid aeseq;
	if a;
run;


