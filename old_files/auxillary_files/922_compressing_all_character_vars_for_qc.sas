data &rptnm._val; 
   set custom.&rptnm._val;
   array cmprs[*] _character_;
   do i=1 to dim(cmprs);
      cmprs[i]=compress(cmprs[i]);
   end;
   drop i;
run;

*qc dataset;
data ir_&rptnm._val;
    set out.ir_&rptnm._val;
    array cmprs[*] _character_;
   do i=1 to dim(cmprs);
      cmprs[i]=compress(cmprs[i]);
   end;
   drop i;
 run;
 
