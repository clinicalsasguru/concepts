dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

*---------------------------;
*long to wide;
*---------------------------;

data sbp;
input subject $ visit sbp;
datalines;
101 1 160
101 3 140
101 4 130
101 5 120
202 1 141
202 3 161
202 4 171
202 5 181
;
run;

*each subject is supposed to have 5 visits, visit 1 through visit 5;
*here, visit 2 is missing for all the subjects;
*our required output dataset should contain two rows, one for each subject and
6 variables, one for each visit (visit1 - visit5) and subject containg the result of sbp under 
these variables for that visit;

data sbp1;
	set sbp;
	by subject;

	array visits(5) visit1- visit5;
	retain visit1- visit5;

	if first.subject then call missing(of visit1-visit5);
	
	visits(visit)=sbp;
	if last.subject;
	keep subject visit1-visit5;

run;

*----------------------;
*assignment;
*----------------------;

DATA long ; 
  INPUT famid year faminc ; 
CARDS ; 
1 96 40000 
1 97 40500 
1 98 41000 
2 96 45000 
2 97 45400 
2 98 45800 
3 96 75000 
3 97 76000 
3 98 77000 
; 
RUN ;
*faminc96, faminc97,faminc98, faminc99;

