data cm;
   length var $100;
   atc_text="Curious(xxx)"||"0A"x||"Kid(yyy)"||"0A"x||"9(zzz)"||"0A"x;
   count=count(atc_text,"0A"x);
run;

proc sql;
      select max(count(atc_text,"0A"x)) into :maxvars separated by ""
      from cm;
quit;

data cm2;
   set cm;
   array temp[*] $200 atc_text1-atc_text&maxvars.;
   array chars[*] $200 atc_code1-atc_code&maxvars.;
   array finds[*] find1-find&maxvars.;
   do i=1 to &maxvars.;
      temp[i]=scan(atc_text,i,"0A"x);
      if index(temp(i),"(") then do;
         chars[i]=scan(temp(i),2,"()");
         temp[i]=scan(temp(i),1,"()");
      end;
      finds[i]=count(temp(i),"0A"x);
      
   end;
run;

      

