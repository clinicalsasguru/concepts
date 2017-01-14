dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill;
quit;


libname temp "D:\Home\dev\compound6\data\shared\sdtm";

proc sql noprint;
   select distinct memname into :dsnlist separated by '*'
   from dictionary.tables
   where upcase(libname)="TEMP";
quit;

%put &dsnlist;
%put &totdsn;

%macro xpt;
 
   %do i=1 %to &sqlobs;

   %let dsn=%scan(&dsnlist,&i,'*');

   libname temp2 xport "D:\Home\dev\compound6\data\shared\sdtm\xpt\&dsn..xpt";

   proc copy in=temp out=temp2;
   select &dsn;
   run;

   quit;
   %end;
%mend;

options mprint;
%xpt;

