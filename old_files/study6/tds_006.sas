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
   output;
   trt01an=3;
   output;
run;

*----------------------------------------;
*create dummy dataset;
*----------------------------------------;

data dummy;
   length c1 c2 $100;
   count=0;
   c1="Screen Failure (Overall)"; c2="Yes"; group=1; ord=1; output;
   c1="Screen Failure (Overall)"; c2="No"; group=1; ord=2; output;
   c1="Primary Screen Failure Reason (Overall)[1]"; c2="Adverse Event"; 
   group=2; ord=1; output;
   c1="Primary Screen Failure Reason (Overall)[1]"; c2="Lost to Follow-up"; 
   group=2; ord=2; output;
   c1="Primary Screen Failure Reason (Overall)[1]"; c2="Withdrawal by Subject"; 
   group=2; ord=3; output;
   c1="Primary Screen Failure Reason (Overall)[1]"; c2="Study Terminated by Sponsor";
   group=2; ord=4;output;
   c1="Primary Screen Failure Reason (Overall)[1]"; c2="Not Fullfill Inclusion or Exclusion Criteria";
   group=2; ord=5; output;
   c1="Primary Screen Failure Reason (Overall)[1]"; c2="Other";
   group=2; ord=6; output;
run;

*--------------------------------;
*get actual counts;
*--------------------------------;

proc sql;
   create table counts as 
      select 1 as group , 1 as ord , count(distinct usubjid) as count
      from adsl 
      where dsffl="Y"

      union all corr

      select 1 as group , 2 as ord , count(distinct usubjid) as count
      from adsl 
      where dsffl="N"  

      union all corr

      select 2 as group, dsfreasn as ord ,dsfreas, count(distinct usubjid) as count   
      from adsl
      where not missing(dsfreas)
      group by dsfreasn,dsfreas
      order by group, ord;
quit;

proc sql noprint;
   select count(distinct usubjid) into :nsubj
   from adsl
   where icfl="Y";
quit;

proc sort data=dummy;
   by group ord;
run;

data counts;
   merge dummy counts;
   by group ord;
run;

data final;
   set counts;
   length c3 $30;
   if not missing(count) then c3=put(count,3.)||" ("||put(count/&nsubj*100,5.1)||"%)";
   else c3=put(count,3.);
run;

proc report data = final center headline headskip nowd split='~' missing spacing=0;
   column  group c1 ord c2  c3 ;
    define group/order noprint;
    define ord /order noprint;
    define c1/width=60 "Parameter" width = 23 order flow spacing = 0;
    define c2/width=30 "Category"  width = 27 flow spacing = 0;
    define c3/"Total" "(N=%cmpres(&nsubj))" width=18  ;
    break after group / skip;
run;


 

