dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

proc print data=sashelp.class;
run;

proc sort data=sashelp.class out=class;
   by name sex age;
run;

proc transpose data=class out=classt;
   by name sex age;
   var height weight;
run;

data classt2;
   set classt;
   length paramcd $8;
   aval=col1;
   paramcd=upcase(_name_);
   drop col1 _name_;
run;



