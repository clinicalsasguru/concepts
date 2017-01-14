dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

data one;
   input x a$;
cards;
1 a
2 b
3 c
;
run;

data two;
   input x b$;
cards;
2 x
3 y
4 v
;
run;


*-----------------------------------------;
*innner join;
*-----------------------------------------;

data innermerge;
   merge one(in=ina) two(in=inb);
   by x;
   if ina and inb;
run;


proc sql;
   create table innersql1 as
      select a.*,b.b
      from one as a
         inner join
         two as b
      on a.x=b.x;
quit;

proc sql;
   create table innersql2 as
      select a.a,b.*
      from one as a
         inner join
         two as b
      on a.x=b.x;
quit;


proc sql;
   create table innersql3 as
      select a.a,b.x,b.b
      from one as a
         inner join
         two as b
      on a.x=b.x;
quit;


proc sql;
   create table innersql4 as
      select b.x,a.a,b.b
      from one as a
         inner join
         two as b
      on a.x=b.x;
quit;

proc sql;
   create table innersql5 as
      select b.x as id label="Identification Number" ,a.a,b.b
      from one as a
         inner join
         two as b
      on a.x=b.x;
quit;

*------------------------------;
*left join;
*------------------------------;

data leftmerge;
   merge one(in=ina) two(in=inb);
   by x;
   if ina;
run;

proc sql;
   create table leftsql as
      select a.x,a.a,b.b
      from one a
         left join
         two b
      on a.x=b.x;
quit;

*--------------------------------------;
*right join;
*--------------------------------------;

data rightmerge;
   merge one(in=ina) two(in=inb);
   by x;
   if inb;
run;

proc sql;
   create table rightsql as
      select b.x as subjid "Subject Id", a.a,b.b
      from one as a
         right join
         two as b
      on a.x=b.x;
quit;

*---------------------------------------------;
*full join;
*---------------------------------------------;

data fullmerge;
   merge one two;
   by x;
run;

data fullmerge1;
   merge one(in=ina) two(in=inb);
   by x;
   if ina or inb;
run;

proc sql;
   create table fullsql as
      select one.a,two.*
      from one
         full join
         two
     on one.x =two.x;
quit;

proc sql;
   create table fullsql1 as
      select coalesce(one.x,two.x) as x,one.a,two.b
      from one
         full join
         two
     on one.x =two.x;
quit;

