
dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work kill nolist;
run;

libname sdtm "D:\Home\dev\compound1\data\shared\sdtm";

data dm;
	set sdtm.dm;
	where armcd ne "SCRNFAIL";
run;

data height(rename=(vsstresn=height)) 
		weight (rename=(vsstresn=weight));
	set sdtm.vs;
	if vsblfl="Y" and vstestcd="HEIGHT" then output height;
	if vsblfl="Y" and vstestcd="WEIGHT" then output weight;
	keep usubjid vsstresn;
run;

data dm1;
	merge dm(in=a) height weight ;
	by usubjid;
	if a;
run;

*==============================================================================;
*instructions (use dm1 dataset from above step and proceed further)
1. Follow template1, each stat on a row- no concatenation
2. Num vars: age, height, weight
3. Categorical vars: race,ethnic, sex, country(display the categories which are in data)
4. For descriptive statistics, if n is 0 (zero) all the stats should be missing(null - not a period, complete blank)
	else if n=1 then standard deviation should be "  - "(hint: this has to be handled during
	character conversion step)
5. Decimal alignment has to be followed
6. Log should not contain missing values were generated note
7. Create total column also
8. ps=47 and ls=133
9. no group should not break into two pages;
*==================================================================================; 


