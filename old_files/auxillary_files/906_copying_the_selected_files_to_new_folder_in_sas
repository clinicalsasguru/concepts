dm log 'clear';
dm lst 'clear';

*===============================================================================;
*define TFL program and system_files paths here;
*===============================================================================;

%let _x_x_program_loc=%str(put you path here); *no slash at the end;
%let _x_x_extension=%str(rtf);

*============================================================================;
*house-keeping- creating temporary folders, system options;
*============================================================================;


*------------------------------------;
*get run time;
*------------------------------------;

data _null_;
   time=time();
   date=date();
   datetime = compress(put(date,yymmdd10.)||"T"||put(time,tod5.));
   datetime2=compress(datetime,':');
   call symputx('_x_x_run_time',datetime);
   call symputx('_x_x_run_time2',datetime2);

run;

%put &_x_x_run_time.;
options sasautos = ("W:\bums\macro_library"  sasautos) symbolgen mprint mlogic;

options xmin noxwait;

%let _x_x_user=&sysuserid;

%put &_x_x_user;

%macro newfolder(newfld);
%local rc fileref;
%let rc=%sysfunc(filename(fileref,&newfld));
%if %sysfunc(fexist(&fileref)) %then %put NOTE:The directory "&newfld" already EXISTED.;
%else %do;
         %sysexec md "&newfld";
         %put NOTE:The directory "&newfld" has been CREATED.;
      %end;
%let rc=%sysfunc(filename(fileref));
%mend newfolder;

%newfolder(C:\Users\&_x_x_user.\Desktop\dump);
%newfolder(&_x_x_program_loc.\toSDD_b3_&_x_x_extension._files);


libname _temp_ "C:\Users\&_x_x_user.\Desktop\dump";

*==================================================================================;
*Pass the list of programs here(double check the program names!);
*==================================================================================;

data _temp_.programs;
   length program_name $50;
   infile cards truncover;
   input program_name$ 1-30;
   program_name=strip(lowcase(program_name));
   if not missing(program_name);
cards;
l_labs_exc_alt
l_labs_exc_bp
l_labs_exc_ck18
l_labs_exc_elf
l_labs_exc_glug
l_labs_exc_hff
l_labs_exc_trig
;
run;

proc sort data=_temp_.programs nodupkey;
   by program_name;
run;

%macro copyfile(program);
%sysexec copy /Y "&_x_x_program_loc.\&program..&_x_x_extension."  "&_x_x_program_loc.\toSDD_b3_&_x_x_extension._files";
%mend;


data _temp_.pgm_present _temp_.pgm_absent;
   set _temp_.programs;
   if not missing(program_name) then do;
   if fileexist(cats("&_x_x_program_loc.\",program_name,".&_x_x_extension.")) =1 
      then do;
      output _temp_.pgm_present;
      end;
      else do;
      output _temp_.pgm_absent;
      end;
   end;
run;

data _null_;
   set _temp_.pgm_present;
   call execute('%copyfile('||strip(program_name)||');');
run;


%macro deletefolder(path);

%sysexec rd /q /s "&path";

%mend;

*libname _temp_;

*%deletefolder(C:\Users\&_x_x_user.\Desktop\dump);

options noxmin xwait nodate nonumber;

%ut_saslogcheck;

title "list of programs/files not present";
proc print data=_temp_.pgm_absent;
run;
