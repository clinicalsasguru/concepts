dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

*--------------------------------------------------------;
*creating sample data;
*--------------------------------------------------------;

data test;
	aeid1=.;
	aeid2=2;
	aeid3=3;

	text1="";
	text2="curious";
	text3="";
run;

data test2;
	set test;
	firstnonmiss1=coalesce(aeid1,aeid2,aeid3);
	firstnonmiss2=coalescec(text1,text2,text3);
run;
