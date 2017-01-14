dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

%let root=D:\Home\dev\compound6;

options mautosource sasautos=("&root.\lums", sasautos);


%assignlibs;

data adsl;
   set s_adam_i.adsl;
run;

data adsl;
   set adsl;
   where randfl="Y" and not missing(trt01an);
   output;
   trt01an=3;
   output;
run;

data dummy;
   length c1 c2 $ 100 ;
   c1="Discontinuation"; c2="Yes"; group=1; ord=1; output;
   c1="Discontinuation"; c2="No"; group=1; ord=2; output;
   
   c1="Primary Reason for Discontinuation [1]";c2="Completed"; group=2; 
   ord=0; output;
   c2="Randomized/Registered but Never Received/Dispensed Study Drug";
   ord=1; output;
   c2="Adverse Event"; ord=2;output;
   c2="Death"; ord=3; output;
   c2="Lack of Efficacy"; ord=4; output;
   c2="Lost to Follow-up"; ord=5; output;
   c2="Protocol Violation"; ord=6; output;
   c2="Withdrawal by Subject"; ord=7; output;
   c2="Study Terminated by Sponsor"; ord=8; output;
   c2="Physician Decision"; ord=9; output;
   c2="Non-Compliance with Study Drug"; ord=10; output;
   c2="Pregnancy"; ord=11; output;
   c2="Other"; ord=12; output;
run;


data dummy;
   set dummy;
   count=0;
   do trt01an=1 to 3;
   output;
   end;
run;

*----------------------------------------------------;
*get actual counts;
*----------------------------------------------------;

proc sql;
   create table counts as
      select trt01an, count(distinct usubjid) as count,
      1 as group, 1 as ord
      from adsl
      where dstfl="Y"
      group by trt01an

      union all 

      select trt01an, count(distinct usubjid) as count,
      1 as group, 2 as ord
      from adsl
      where /*dstfl="N" */ missing(dstfl)
      group by trt01an

      union all 

      select trt01an, count(distinct usubjid) as count,
      2 as group, 0 as ord
      from adsl
      where dstfl="N"
      group by trt01an

      union all 

      select trt01an, count(distinct usubjid) as count,
      dstreasn as ord, 2 as group
      from adsl
      where not missing(dstreasn)
      group by dstreasn
      order by group, ord,trt01an ;
quit;


   proc sort data=dummy;
      by group ord trt01an;
   run;

   data counts;
      merge dummy counts;
      by group ord trt01an;
   run;

   *---------------------------------------;
   *get treatment totals for denominators;
   *---------------------------------------;

   proc sql;
      create table denoms as
      select trt01an, count(distinct usubjid) as trttotal
      from adsl
      where randfl="Y"
      group by trt01an;
   quit;

   data dummytrt;
      trttotal=0;
      do trt01an=1 to 3;
      output;
      end;
   run;

   data denoms;
      merge dummytrt denoms;
      by trt01an;
   run;

   proc sort data=counts;
      by trt01an;
   run;

   data counts;
      merge counts denoms;
      by trt01an;
   run;

   data counts;
      set counts;
      if count ne 0 then cp=put(count,3.)||" ("||put(count/trttotal*100,5.1)||"%)";
      else cp=put(count,3.);
   run;

   proc sort data=counts;
      by group ord c1 c2;
   run;

   proc transpose data=counts out=trans;
      by group ord c1 c2;
      var cp;
      id trt01an;
   run;


   data final;
      set trans;
      length c3-c5 $30;
      c3=_1;
      c4=_2;
      c5=_3;
      keep group ord c1-c5;
   run;

*====================================================================;
*macro variables of treatment totals for column headers;
*====================================================================;

proc sql noprint;
   select count(distinct usubjid) into :n1 from adsl where trt01an=1 and randfl="Y";
   select count(distinct usubjid) into :n2 from adsl where trt01an=2 and randfl="Y";
   select count(distinct usubjid) into :n3 from adsl where trt01an=3 and randfl="Y";
quit;

%put number of subjects in trt1 : &n1;
%put number of subjects in trt2 : &n2;
%put number of subjects in trt3 : &n3;



 proc report data = final center headline headskip nowd split='~' missing spacing=0;
   column  group c1 ord c2  c3 c4 c5 ;
    define ord/order noprint;
    define group/ order noprint;
    define c1/width=30 "Parameter" order flow spacing = 0;
    define c2 /width=40 "Category" flow;
    define c3/"Placebo" "(N=%cmpres(&n1))" width=18  spacing=2;
    define c4/"*redacted;" "(N=%cmpres(&n2))" width=18  ;
    define c5/"Total" "(N=%cmpres(&n3))" width=18  ;
    break after group / skip;
run;

