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

data dummy;
   length c1 $ 100 ;
   c1="Randomized"; ord=1; output;
   c1="Safety Analysis Set[1]"; ord=2; output;
   c1="Full Analysis Set[2]"; ord=3; output;
   c1="Per Protocol Set[3]"; ord=4; output;
   c1="Pharmacokinetics Analysis Set[4]"; ord=5; output;
   c1="Pharmacodynamic Analysis Set[5]"; ord=6; output;
run;

data dummy;
   set dummy;
   count=0;
   do trt01an=1 to 3;
   output;
   end;
run;

*-----------------------------------------------;
*get actual counts;
*-----------------------------------------------;

proc sql;
   create table counts as 
      select trt01an,count(distinct usubjid) as count, 1 as ord 
      from adsl
      where randfl="Y"
      group by trt01an

      union all corr

      select trt01an,count(distinct usubjid) as count, 2 as ord
      from adsl
      where saffl="Y"
      group by trt01an

      union all corr

      select trt01an,count(distinct usubjid) as count, 3 as ord
      from adsl
      where fasfl="Y"
      group by trt01an

      union all corr

      select trt01an,count(distinct usubjid) as count, 4 as ord
      from adsl
      where pprotfl="Y"
      group by trt01an

      union all corr

      select trt01an,count(distinct usubjid) as count, 5 as ord
      from adsl
      where pkasfl="Y"
      group by trt01an

      union all corr

      select trt01an,count(distinct usubjid) as count, 6 as ord
      from adsl
      where pdasfl="Y"
      group by trt01an
      order by ord, trt01an;
   quit;

   proc sort data=dummy;
      by ord trt01an;
   run;

   data counts;
      merge dummy counts;
      by ord trt01an;
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
      by ord c1;
   run;

   proc transpose data=counts out=trans;
      by ord c1;
      var cp;
      id trt01an;
   run;

   data final;
      set trans;
      length c2-c4 $30;
      c2=_1;
      c3=_2;
      c4=_3;
      keep ord c1-c4;
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
   column   ord c1  c2  c3 c4 ;
    define ord/order noprint;
    define c1/width=30 "Analysis Set" order  width = 40 flow spacing = 0;
    define c2/"Placebo" "(N=%cmpres(&n1))" width=18  ;
    define c3/"*redacted;" "(N=%cmpres(&n2))" width=18  ;
    define c4/"Total" "(N=%cmpres(&n3))" width=18  ;
    break after ord / skip;
run;

