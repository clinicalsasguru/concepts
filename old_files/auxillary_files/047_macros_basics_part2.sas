dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

proc means data=sashelp.class;
   var age;
run;

proc means data=sashelp.class;
   var weight;
run;

proc means data=sashelp.class;
   var height;
run;

%macro means(var=);
   proc means data=sashelp.class;
      var &var;
   run;
%mend means;
options mprint symbolgen;
%means(var=age);
%means(var=weight);
%means(var=height);
options nomprint nosymbolgen;



%macro means2(indsn=,var=);
   proc means data=&indsn.;
      var &var;
   run;
%mend means2;

options mprint symbolgen;
%means2(indsn=sashelp.class, var=age);
%means2(indsn=sashelp.cars, var=cylinders);
options nomprint nosymbolgen;

%macro means3(indsn=,output_name=);
   proc means data=&indsn.;
      *var age;
      *var height;
   %if &output_name=a %then %do;
      var age;
   %end;

   %else %if &output_name=b %then %do;
      var height;
   %end;
   
   run;

   data &output_name.x;
      set sashelp.class;
      keep 
   %if &output_name=a %then %do;
      age name
   %end;

   %else %if &output_name=b %then %do;
     name height
   %end;
   ;

%mend means3;

options mprint sgen mlogic;
%means3(indsn=sashelp.class, output_name=a);
%means3(indsn=sashelp.class, output_name=b);
options nomprint nosgen nomlogic;

