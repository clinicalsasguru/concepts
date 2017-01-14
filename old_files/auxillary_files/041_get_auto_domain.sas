libname test "D:\Home\dev\compound6\data\shared\sdtm";
libname sdtm "D:\Home\dev\compound6\data\shared\sdtm\temp";

proc contents data=test._all_ out=temp;
run;

proc sql;
   select distinct memname into :datasets separated by ' '
   from temp;
   select count(distinct memname) into :datasetcount
   from temp;
quit;

%macro domain;
 
 %do _i=1 %to &datasetcount;
 %let dsn=%scan(&datasets,&_i);
 data sdtm.&dsn;
   set test.&dsn;
   domain="&dsn";
 run;

 %end;

 %mend;

 %domain;

  
