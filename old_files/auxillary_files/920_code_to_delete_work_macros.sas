%macro delete_work_macros;
proc sql noprint;
   create table _work_macros as
      select libname, memname, objname, objtype
      from dictionary.catalogs
      where libname="WORK" and memname="SASMACR" and OBJTYPE="MACRO" 
      and objname not in ("DELETE_WORK_MACROS" "DELETE_WORK_DATASETS"
      "RETRIEVE_SYSTEM_OPTIONS" "LOCATIONS" "PRE_PROCESS_SETUP" "POST_PROCESS_SETUP"
      "DELETE_WORK_FORMATS" "CLOSEVTS" "DELETE_MAC_VARS" "LIBRARIES" "PRIMARY_PRG_HEADER_CHECK" "BATCH2" "DTC_DATE9" "_X_X_DEBUG"
      "TRT_DUPLICATION");

    select distinct objname into :_work_macros separated by " "
      from _work_macros ;

    select count(*) into :_work_macros_num from _work_macros;

quit;

%if &_work_macros_num gt 0 %then %do;
proc catalog cat=work.sasmacr;
delete &_work_macros / et=macro;
quit;
%end;

%mend delete_work_macros;
