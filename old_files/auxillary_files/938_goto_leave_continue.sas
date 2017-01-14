data leave;
   do i=1 to 5;
   if i=3 then leave; *leaves the loop;
   output;
   end;
run;


data continue;
   do i=1 to 5;
   if i=3 then continue;*only the current iteration of the loop is skipped;
   output;
   end;
run;

data goto;
   do i=1 to 5;
   if i=3 then go to test;
   output;
   end;
   test: do;
   i=99;
   output;
   end;
run;
