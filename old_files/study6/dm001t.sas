dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

%assignlibs;

data adsl;
   set s_adam_i.adsl;
run;


data dummy;
   length c1 $ 100 ;
   c1="Subjects with Informed Consent "; ord=1; output;
   c1="Subjects Entered Pre-Treatment Period"; ord=2; output;
   c1="Subjects Entered Pre-Treatment Period but Discontinued before Randomization [2]"; ord=3; output;
   c1="Discontinued before Randomization [3]"; ord=4; output;
   c1="Randomized "; ord=5; output;
run;

data dummy;
	set dummy;
   count=0;	
run;
*-----------------------------------------------;
*get actual counts;
*-----------------------------------------------;


proc sql;
   create table counts as 
      select count(distinct usubjid) as count, 1 as ord 
      from adsl
      where ICFL="Y"
 

      union all corr

		
      select count(distinct usubjid) as count, 4 as ord 
      from adsl
      where dsffl="Y"
 

      union all corr


		 
      select count(distinct usubjid) as count, 5 as ord 
      from adsl
      where randfl="Y";
   quit;

proc sort data=dummy;
   by ord ;
run;

data counts;
   merge dummy counts;
   by ord ;
run;

data final;
      set counts;
      length c1-c2 $100;
      c2=put(count, best.);
      
      keep ord c1-c2;
   run;
 	
 proc report data = final center headline headskip nowd split='~' missing spacing=0;
   column   ord c1 c2  ;
    define ord/order noprint;
    define c1/ "Analysis Set" order  width = 50 flow spacing = 0;
    define c2/"Total"  width=18  center;
   
    break after ord / skip;
run;

*redacted;
