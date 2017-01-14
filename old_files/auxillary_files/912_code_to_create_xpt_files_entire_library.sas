
libname inlib "..\..\derived_data";
libname inlib2 "..\..\input_data\sdtm";
libname outlib "..\..\derived_data\xpt";

%let outlib=M:\SDDEXT036\lillyce\ly450190\h6d_mc_lvjj\final_ole\derived_data\xpt;

%macro create_xpt;
proc sql;
  select memname into :ddlst separated by " "
  from sashelp.vstable
  where lowcase(libname) = "inlib";
quit;

%let _i = %eval(1);
  %do %while (%scan(&ddlst, &_i. %str( )) ne %str( ) );
    %let _ddlst = %scan(&ddlst, &_i, %str( ));
   libname xportout xport "&outlib/&_ddlst..xpt";
   proc copy in=inlib out=xportout memtype = data;
     select &_ddlst;
   run;

   %let _i = %eval(&_i+1);
  %end;

%mend create_xpt;

%create_xpt;

libname xportout xport "&outlib/dm.xpt";
   proc copy in=inlib2 out=xportout memtype = data;
     select dm;
   run;

proc printto print = _ibxout_ ;
run;
