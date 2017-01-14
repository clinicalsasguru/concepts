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

*----------------------------------------------------;
*get actual counts;
*----------------------------------------------------;

proc sql;
   create table count as 
      select trt01an, cntrytxt, count(distinct usubjid) as count,
      1 as order, "" as siteid length=8, "" as sitenam length=200
      from adsl
      where randfl="Y"
      group by trt01an, cntrytxt

      union all corr

      select trt01an, cntrytxt, siteid, "  "||strip(sitenam) as sitenam,
      count(distinct usubjid) as count, 2 as order
      from adsl
      where randfl="Y"
      group by cntrytxt, siteid, sitenam,trt01an
      order by cntrytxt, order, siteid, sitenam, trt01an;

      create table dummy as
         select distinct cntrytxt, order, siteid, sitenam
         from count;
quit;

data dummy;
   set dummy;
   count=0;
   do trt01an=1 to 3;
      output;
   end;
run;

proc sort data=dummy;
   by cntrytxt order siteid sitenam trt01an;
run;

proc sort data=count;
   by cntrytxt order siteid sitenam trt01an;
run;

data count;
   merge dummy(in=a) count(in=b);
   by cntrytxt order siteid sitenam trt01an;
run;

*------------------------------------------------------;
*get denominators for calculating percentages;
*------------------------------------------------------;

proc sql;
   create table trttotal as
      select trt01an, count(distinct usubjid) as trttotal
      from adsl
      group by trt01an;
quit;

data dummytrttotal;
   trttotal=0;
   do trt01an=1 to 3;
      output;
   end;
run;

data trttotal;
   merge dummytrttotal trttotal;
   by trt01an;
run;

*-----------------------------------------------------------;
*calculate percentages;
*-----------------------------------------------------------;

proc sort data=count;
   by trt01an;
run;

data count;
   merge count trttotal;
   by trt01an;
run;

data count;
   set count;
   length cp $20;
   if count ne 0 then cp=put(count,3.)||" ("||put(count/trttotal*100,5.1)||"%)";
   else cp=put(count,3.);
run;

proc sort data=count;
   by cntrytxt order siteid sitenam;
run;

proc transpose data=count out=trans;
   by cntrytxt order siteid sitenam;
   var cp;
   id trt01an;
run;

data final;
   set trans;
   length c1 $50 c2 $100 c3-c5 $20;
   c1="Country & site";
   if order=1 then c2=cntrytxt;
   else if order=2 then c2= sitenam;
   c3=_1;
   c4=_2;
   c5=_3;

   keep c1-c5 cntrytxt order siteid sitenam;
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
   column   c1 cntrytxt order siteid c2  c3 c4 c5 ;
    define cntrytxt/order noprint;
    define siteid/ order noprint;
    define order/order noprint;
    define c1/width=30 "Parameter" order flow spacing = 0;
    define c2 /width=40 "Category" flow;
    define c3/"Placebo" "(N=%cmpres(&n1))" width=18  spacing=2;
    define c4/"*redacted;" "(N=%cmpres(&n2))" width=18  ;
    define c5/"Total" "(N=%cmpres(&n3))" width=18  ;
    break after cntrytxt / skip;
run;

   
