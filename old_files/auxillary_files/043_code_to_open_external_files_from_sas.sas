
%macro gen_stop_001;
%put you are in gen_stop_001 macro;
proc printto;
run;
%let _x_x_file="&root.\programs_stat\tfl_output\&_x_x_output_name..lst";
%put  the entire path is &_x_x_file.;
options xmin noxwait noxsync;
x  ""&root.\programs_stat\tfl_output\&_x_x_output_name..lst"" ;

%mend gen_stop_001;
