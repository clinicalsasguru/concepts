dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

data test;
   do i=1 to 15;
      if i lt 11 then country="cntry1";
      else country="cntry2";
      output;
   end;

run;

proc sql;
   create table test2 as 
      select a.*, case
         when count>=10 then a.country
         when count<10 then "SMLLGRP"
         else ''
         end as sitegrp length=20
         from test as a left join
      (select country,count(*) as count
      from test
      group by country) as b
      on a.country=b.country;
quit;
