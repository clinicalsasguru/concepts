proc sql;
   create table pre_counts as
   select aebodsys,aedecod,a.trtan, count(distinct usubjid) /denom*100 as percent,b.denom, count(distinct usubjid) as count
   from ae00_01_pre as a
   left join (select trt01an as trtan,count(distinct usubjid) as denom
               from sl01_01 
               where trt01an in (1,2,3)
               group by trt01an) as b
   on a.trtan=b.trtan
   group by aebodsys,aedecod,a.trtan
   having calculated percent ge 5;


   create table ae00_01 as 
   select a.* 
   from ae00_01_pre as a
      inner join
      (select distinct aebodsys, aedecod
      from pre_counts) as b
   on a.aebodsys=b.aebodsys and a.aedecod=b.aedecod;
quit;
