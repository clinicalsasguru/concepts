dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

%let root=D:\Home\dev\compound6;

options mautosource sasautos=("&root.\lums", sasautos);

%assignlibs;

data adae01;
   set s_adam_i.adae;
   where saffl="Y" and trtemfl="Y";
run;

data temp;
   length relgr1 $100;
   call missing(relgr1);
 run;

data adae;
   if 0 then set temp;
   set adae01;
   output;
   trtan=3;
   output;
run;

data adsl;
   set s_adam_i.adsl(rename=(trt01an=trtan));
   where saffl="Y";
   keep usubjid trtan;
   output;
   trtan=3;
   output;
run;

proc sql;
   create table trttotals_pre as
   select trtan,count(distinct usubjid) as trttotal
   from adsl
   group by trtan;
quit;

data dummytrt;
   trttotal=0;
   do trtan=1 to 3;
     output;
   end;
run;

data trttotals;
   merge dummytrt trttotals_pre ;
   by trtan;
run;

*============================================================;
*creating dummy dataset with all labels;
*============================================================;

data dummy_pre;
   length label $200;
   label="Adverse Events"; ord=1;output ;
   label="Drug-Related [1] Adverse Events"; ord=2;output;
   label="Deaths";ord=3;output;
   label="Serious Adverse Events [2]";ord=4; output;
   label="Drug-Related [1] Serious Adverse Events [2]"; ord=5; output;
   label="Adverse Events Leading to Permanent Discontinuation of Study Drug";ord=6; output;
   label="Drug-Related [1] Adverse Events Leading to Permanent Discontinuation of Study Drug"; ord=7; output;
run;

*----------------------------------------------------;
*creating a record for each treatment with zero count;
*----------------------------------------------------;

data dummy;
   set dummy_pre;
   count1=0;
   count2=0;
   do trtan=1 to 3;
      output;
   end;
run;

*=====================================================================;
*obtaining actual counts(with processing for each row);
*=====================================================================;

proc sql;
   create table allrows as 
   select trtan, 1 as ord,
   count(distinct usubjid) as count1, count(usubjid ) as count2
   from adae 
   where trtemfl="Y"
   group by trtan

   union all corr

   select trtan, 2 as ord,
   count(distinct usubjid) as count1, count(usubjid ) as count2
   from adae
   where upcase(relgr1)="RELATED"
   group by trtan

   union all corr

   select trtan, 3 as ord,
   count(distinct usubjid) as count1, count(usubjid ) as count2
   from adae
   where aesdth="Y"
   group by trtan

   union all corr

   select trtan, 4 as ord,
   count(distinct usubjid) as count1, count(usubjid ) as count2
   from adae
   where aeser="Y"
   group by trtan

   union all corr

   select trtan, 5 as ord,
   count(distinct usubjid) as count1, count(usubjid ) as count2
   from adae
   where upcase(relgr1)="RELATED" and aeser="Y"
   group by trtan

   union all corr

   select trtan, 6 as ord,
   count(distinct usubjid) as count1, count(usubjid ) as count2
   from adae
   where upcase(aeacn)="DRUG WITHDRAWN"
   group by trtan

   union all corr

   select trtan, 7 as ord,
   count(distinct usubjid) as count1, count(usubjid ) as count2
   from adae
   where upcase(aeacn)="DRUG WITHDRAWN" and upcase(relgr1)="RELATED"
   group by trtan;
quit;


*------------------------------;
*merge with dummy counts;
*------------------------------;
proc sort data=dummy;
   by ord trtan;
run;

proc sort data=allrows;
   by ord trtan;
run;

data allrows2;
   merge dummy(in=a) allrows(in=b);
   by ord trtan;
run;

*=======================================;
*calculate percentages;
*=======================================;

proc sort data=allrows2;
   by trtan;
run;

proc sort data=trttotals;
   by trtan;
run;

data allrows2;
   merge allrows2(in=a) trttotals(in=b);
   by trtan;
run;

data allrows3;
   set allrows2;
   length count_percent $30;
   if count1 ne 0 then count_percent=put(count1,3.)||" ("||put(count1/trttotal*100,5.1)||"%)" ||put(count2,3.);
   else count_percent=put(count1,3.);
   keep ord label trtan count_percent;
run;

*-----------------------------------------------------;
*transpose to get treatments as columns;
*-----------------------------------------------------;

proc sort data=allrows3;
   by ord label;
run;

proc transpose data=allrows3 out=final(drop=_:) prefix=trt;
   by ord label;
   var count_percent;
   id trtan;
run;


proc datasets lib=work;
   modify final;
   rename label=c1 trt1=c2 trt2=c3 trt3=c4;
quit;


*============================================================================;
*report generation;
*============================================================================;

proc report data=final nowd headline headskip missing spacing=0;
   columns ord c1  c2 c3 c4;
   define ord/ order noprint;
   define c1 /order "" width=50 flow;
    define c2/"Placebo" "(N=%cmpres(&n1))" width=18  ;
    define c3/"*redacted;" "(N=%cmpres(&n2))" width=18  ;
    define c4/"Total" "(N=%cmpres(&n3))" width=18  ;
   break after ord/skip;

run;


*redacted;
