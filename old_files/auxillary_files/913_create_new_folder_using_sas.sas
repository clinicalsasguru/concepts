*-----------------------------------------------------------------;
*macro to create utility folders - as required;
*-----------------------------------------------------------------;

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
