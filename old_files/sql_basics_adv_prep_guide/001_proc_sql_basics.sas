dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

%assignlibs;

data adsl;
   set s_adam_i.adsl;
run;

proc sql;
   select distinct trt01an,trt01a length=20
   from s_adam_i.adsl;
quit;

proc freq data=s_adam_i.adsl ;
   *tables trt01an*trt01a/nocum missing;
   *tables trt01an;
   tables trt01an/missing;
run;

proc sql;
   select trt01an,trt01a length=20
   from s_adam_i.adsl;
quit;

proc sql;
   select * 
   from sashelp.class;
quit;

*------------------------;
*where processing;
*------------------------;

proc sql;
   select distinct trt01an,trt01a length=20
   from s_adam_i.adsl
   where sex="F";
quit;

*----------------------;
*order by clause;
*----------------------;

proc sql;
   select usubjid
   from adsl
   where sex="F" and age ne .
   order by age;
quit;

proc sql;
   select usubjid,age
   from adsl
   where sex="F" and age ne .
   order by age;
quit;

proc sql;
   select usubjid,age
   from adsl
   where sex="F" and age ne .
   order by age desc;
quit;


*---------------------------;
*group by clause;
*---------------------------;

proc sql;
   select trt01an, trt01a length=20, count(*) as count
   from adsl
   group by trt01an, trt01a;
quit;

proc sql;
   select trt01an, trt01a length=20,sex, count(*) as count
   from adsl
   group by trt01an, trt01a,sex
   order by sex, trt01an,trt01a;
quit;

*----------------------------------;
*create new columns;
*----------------------------------;

proc sql;
   select usubjid, heightbl, weightbl, heightbl/weightbl as htwt
   from adsl
   where nmiss(heightbl,weightbl)=0;
quit;


proc sql;
   select usubjid, bmibl*2 as bmibl2
   from adsl
   where not missing(bmibl)
   order by bmibl2 desc;
quit;

*length label $20;
*label="this is a label";
*bmibl2=bmibl*2;

proc sql;
   select usubjid, "This is a label" as label length=20, bmibl*2 as bmibl2
   from adsl
   where not missing(bmibl)
   order by bmibl2 desc;
quit;

*----------------------------------;
*case expression;
*----------------------------------;

*if sex="M" then label="This is a male label";
*else if sex="F" then label="This is a female label";

proc sql;
   select usubjid, sex, 

      case 
         when sex="M" then "This is a male label"
         when sex="F" then "This is a female label"
      end as label length=25

    from adsl;
 quit;

proc sql;
   select usubjid, bmibl,
      
      case 
         when (. lt bmibl lt 30) then "Less than 30"
         when (30 le bmibl lt 50) then "30 to less than 50"
         when (bmibl ge 50) then "Greater than 50"
         else ""
      end as bmiblgrp length=25

   from adsl;
quit;

proc sql;
   select usubjid, sex, age,
      
      case 
         when (sex="M" and (. lt age lt 50)) then "Male less than 50"
         when (sex="F" and (. lt age lt 50)) then "Female less than 50"
         when (sex="M" and (age ge 50)) then "Male GE 50"
         when (sex="F" and (age ge 50)) then "Female GE 50"
         else ""
      end as label length=25

   from adsl;
quit;

   

