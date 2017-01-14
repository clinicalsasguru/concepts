data temp;
	input timec $20.;
	adtm=input(timec,is8601dt.);
	timecc=put(adtm,e8601dt.);
	format adtm e8601dt.;
cards;
2010-10-10T10:10
2010-10-10T10:10:10
;
run;

