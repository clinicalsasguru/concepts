%macro delete_work_formats;
proc sql noprint;
   create table _work_formats as
      select libname, memname, objname, objtype
      from dictionary.catalogs
      where libname="WORK" and memname="FORMATS" ;

    select distinct objname into :_work_formats separated by " "
      from _work_formats
      where upcase(objname) not in ("PERCT");
    select count(*) into :_work_formats_num from _work_formats;
   
quit;

%if &_work_formats_num gt 0 %then %do;
proc catalog cat=work.formats kill force;
delete &_work_formats / et=formatc;
quit;
%end;

%mend delete_work_formats;
