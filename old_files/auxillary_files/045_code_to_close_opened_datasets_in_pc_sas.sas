
%macro gen_init_001;
%put you are in gen_init_001 macro;

dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

options ps=48 ls=153;

%macro closevts  /* The cmd option makes the macro available to dms */ / cmd; 
  %local i; 
  %do i=1 %to 20;
    next "viewtable:"; end; 
  %end; 
%mend;

dm "keydef F12 '%NRSTR(%closevts);'";

%assignlibs;
%global _x_x_output_name;

%mend gen_init_001;


