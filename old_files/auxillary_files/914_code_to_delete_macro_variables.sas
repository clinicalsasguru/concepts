
%macro delete_mac_vars;
        
data vars;
    set sashelp.vmacro;
run;

data vars ;
    set vars ;
    if scope='GLOBAL' and substrn(upcase(name),1,5) ne "_X_X_";
run ;

data vars ;
    set vars ;
    if index(name,'SYS') >0 then delete;
run;

proc sort data = vars nodupkey ;
    by name  ;
run ;

data _null_;
    set vars;
    put "Now deleting macro variable:" name;
    call execute('%symdel '||trim(left(name))||';');
run;
%mend;
