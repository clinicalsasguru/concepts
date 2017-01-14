dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;


data test;
   length text $1000;
   text=repeat('*',600);
   max=ceil(length(text)/200)-1;
   do i=0 to max;
      qval=substr(text,200*i+1,200);
      output;
   end;
   
run;

