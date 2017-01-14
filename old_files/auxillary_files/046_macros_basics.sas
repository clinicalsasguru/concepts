%gen_init_001;
*====================================================;
*macros basics; 
*====================================================;

*---------------------------------;
*macro variables creation;
*---------------------------------;

*using proc sql;

proc sql noprint;
   select count(*) into :nobs from sashelp.class;
quit;

%put Total number of observation in the dataset class &nobs;

proc sql noprint;
   select count(*) into :n1-:n2 from sashelp.class
   group by sex;
quit;

%put Total males &n2;
%put Total females &n1;

proc sql noprint;
   select count(*) into :x1-:x99 from sashelp.class
   group by age;
quit;

%put total age 11 &x1;
%put total age 12 &x2;
%put total age 13 &x3;
%put total age 14 &x4;
%put total age 15 &x5;
%put total age 16 &x6;
%put total age 17 &x7;


proc sql noprint;
   select count(*) into :y1-:y99 from sashelp.class
   where age ne 13
   group by age;
quit;

*only 5 categories - so only 5 variables will be created;

%put total age 11 &y1;
%put total age 12 &y2;
%put total age 14 &y3;
%put total age 15 &y4;
%put total age 16 &y5;

*trt1 *trt3 *trt4- :trt1-:trt4; *caution;

proc sql noprint;
   select count(*) into :age16 from sashelp.class where age=16;
   select count(*) into :age17 from sashelp.class where age=17;
quit;

%put age 16 &age16;
%put age 17 &age17;

%let age16=&age16;
%let age17=&age17;

%put age 16 &age16;
%put age 17 &age17;


proc sql noprint;
   select distinct age into :dis_ages separated by ' ' from sashelp.class;
quit;

%put distinct ages : &dis_ages;

proc sql noprint;
   select distinct age into :dis_ages2 separated by '*' from sashelp.class;
quit;

%put distinct ages : &dis_ages2;

*-------------------------------------------------;
*macro variables using data step;
*-------------------------------------------------;
*0 or missing(.) is equal to false;
*anything else is true;

data class;
   set sashelp.class end=last;
   if sex="F" then fcount+1;
   if sex="M" then mcount+1;
   *lasobs=last;
   if last;
   *call symput("malecount", put(mcount,3.));
   *call symput("femalecount", put(fcount,3.));
   call symputx("malecount", mcount);
   call symputx("femalecount", fcount);
run;

%put male count &malecount;
%put female count &femalecount;

 %let name =     curious    ;

 %put *****&name.******;

 %let name2 = %str(    Kid   );

 %put ****&name2*******;

 %let string= %str(proc print; run;);

 %put &string;

 %let name3 =%str(Kid%'s collection);

 %put &name3;

 %let name4=%str(category (%%));

 %put &name4;

 %let formatname = agegr1;
 *agegr1sas;
 *agegr1.sas;
 %put &formatname..sas;

