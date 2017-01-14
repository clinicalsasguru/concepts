*===========================================================================;
*this program is used to illustrate the difference between autocall macros 
and programs used in %include files;
*============================================================================;



*Rule 1: For a macro to be used with autocall library the file name 
and the macro name within the file should be the same.
eg: if you create a macro named _sort in a file and you want use this file
as using autocall facility the file should be _sort.sas;


options mautosource sasautos=("D:\Lesson Files\auxillary_files" sasautos);

%_sort(indsn=sashelp.class,outdsn=class,byvar=name,nodupkey=nodupkey);
