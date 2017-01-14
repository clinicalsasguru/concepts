dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

proc sort data=sashelp.class out=class;
   by name age sex;
run;

proc transpose data=class out=classt(rename=(_name_=paramcd col1=aval));
   by name age sex;
   var height weight;
run;

data classt2;
   length paramcd $10 ;
   set classt;
   paramcd=upcase(paramcd);
run;

proc sort data=classt2;
   by name age sex;
run;

proc transpose data=classt2 out=retrans(drop=_name_);
   by name age sex;
   var aval;
   id paramcd;
run;

data retrans2;
   set retrans;
   length heightc weightc $10;
   if not missing(height) then heightc=strip(put(height,best.));
   if not missing(weight) then weightc=strip(put(weight,best.));

/*   if . le height lt 55 and sex="F" then heightc=strip(heightc)||"*";*/
/*   if . le height lt 60 and sex="M" then heightc=strip(heightc)||"@";*/

   if . le height lt 55 and sex="F" then heightc=strip(heightc)||"*";
   else if . le height lt 60 and sex="M" then heightc=strip(heightc)||"@";
   else if not missing(height) then heightc=strip(put(height,best.));

      if . le height lt 60 then do;
         if sex="F" then heightc=strip(heightc)||"*";
         else if sex="M" then heightc=strip(heightc)||"@";
      end;

   drop height weight;
run;


