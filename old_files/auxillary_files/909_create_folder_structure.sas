/*======================================================================================

Utility	   : This program will come in handy when you need to create an exact replica
             of folder structure in another directory/drive

Description: This program creates a file on the user desktop containg the folder
			 structure of a given parent(root) folder

Parameters : root -the root folder path

Important Note: This program has to be used in conjunction with get_folder_structure


Author	   : Anil Babu Adepu
========================================================================================*/

*----------------------------------;
*get user-s system userid;
*----------------------------------;

%let _x_x_user=&sysuserid;

%put &_x_x_user.;


*------------------------------------------------------------------;
*create a dataset containing the folder paths to be created
from the file created using get_folder_structure.sas program;
*------------------------------------------------------------------;

data newfolder;
	infile "C:\Users\&_x_x_user.\Desktop\folder_structure.txt" lrecl=400 missover;
	length newfld$ 400;
	input newfld$;
	if not missing(newfld);
run;

*-------------------------------------------------------------;
*macro to check if a folder already exists, if not, folder is 
created as required(same strucute as in the source root path);
*--------------------------------------------------------------;

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

*------------------------------------------------;
*create macro calls using call execute;
*------------------------------------------------;

options xmin noxwait;

data _null_;
     set newfolder;
     call execute('%newfolder('||newfld||');');
run;

options noxmin xwait;
