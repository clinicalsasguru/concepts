/*======================================================================================

Utility     : This program will come in handy when you need to create an exact replica
             of folder structure in another directory/drive

Description: This program creates a file on the user desktop containg the folder
          structure of a given parent(root) folder

Parameters : root -the root folder path

Important Note: This program has to be used in conjunction with create_folder_structure

Author      : Anil Babu Adepu
========================================================================================*/

*-------------------------------------;
*house keeping;
*-------------------------------------;

dm lst 'clear';
dm log 'clear';
dm log 'preview';

proc datasets library=work mt=data kill;
quit;

*----------------------------------------;
*define the rooth folder path;
*you need to change this as required;
*----------------------------------------;

%let root=%str(D:\Home\dev\compound6);


*----------------------------------;
*get user-s system userid;
*----------------------------------;

%let _x_x_user=&sysuserid;

%put &_x_x_user;

*-------------------------------------------------------;
*actual code that creates the file containing the 
folder structure;
*-------------------------------------------------------;


data _null_;
    fname="tempfile";
    rc=filename(fname,"C:\Users\&_x_x_user.\Desktop\folder_structure.txt");
    if rc = 0 and fexist(fname) then
       rc=fdelete(fname);
    rc=filename(fname);
run;

options noxmin noxwait;

x chdir /d  &root. & 
dir /s /b /o:n /ad  > C:\Users\&_x_x_user.\Desktop\folder_structure.txt;
x popd;
options xmin xwait;
