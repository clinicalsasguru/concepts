
HAVE

   d:/pdf/class.pdf
   d:/pdf/cars.pdf

WANT APPEND CARS.PDF TO CLASS.PDF

   d:/pdf/classcars.pdf


SOLUTION

ods pdf file="d:/pdf/class.pdf";
proc print data=sashelp.class;
run;quit;
ods pdf close;


ods pdf file="d:/pdf/cars.pdf";
proc print data=sashelp.cars(obs=20);
run;quit;
ods pdf close;


x "cd d:\pdf";
x "C:\Progra~1\gs\gs9.19\bin\gswin64c.exe -dNOPAUSE -sDEVICE=pdfwrite -sOUTPUTFILE=classcars.pdf -dBATCH class.pdf  cars.pdf";

*you will need to download this file - gswin64c.exe;
