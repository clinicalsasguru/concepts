
%macro gen_start_001(output_name=);
options formchar = "|_---|+|---+=|-/\<>*" nodate nonumber minoperator;
title;
%put you are in gen_start_001 macro;

%if &output_name= %then %do;
   %let output_name= %sysget(SAS_EXECFILENAME);
   %let output_name= %sysfunc(substr(&output_name,1,%length(&output_name.)-4));
%end;

%else %if &output_name. in a b c d e %then %do;
   %let output_namex= %sysget(SAS_EXECFILENAME);
   %let output_namex= %sysfunc(substr(&output_namex,1,%length(&output_namex.)-4))&output_name.;
   %let output_name=&output_namex.;
%end;
 
   %let _x_x_output_name=&output_name.;

%put printing to the file &output_name.;
proc printto print="&root.\programs_stat\tfl_output\&output_name..lst" new;
run;

%mend gen_start_001;
