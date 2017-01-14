
*redacted;

options validvarname=upcase varlenchk=nowarn;

%assignlibs;

data advs01;
   set s_adam_i.advs;
   where saffl="Y" and paramn not in (1,3);
run;

data adsl01;
   set s_adam_i.adsl;
   where saffl="Y" ;
run;

data adsl;
   set adsl01(rename=(trt01an=trtan));
run;

data advs;
   merge advs01(in=a) adsl(in=b);
   by usubjid;
   if a and b;
run;

proc sort data=advs;
   by paramn param trtan trta avisitn avisit;
run;

proc summary data=advs;
   by paramn param trtan trta avisitn avisit;
   var aval;
   output out=aval_stats01 n= mean= std= min= median= max=/autoname;
run;

data aval_stats02;
   set aval_stats01;
   length c2-c7 $20;
   if paramn in (2,4,7) then 
      do;
         if not missing(aval_n) then c2=put(aval_n,4.);
         if not missing(aval_mean) then c3=put(aval_mean,6.2);
         if not missing(aval_stddev) then c4=put(aval_stddev,6.2);
         if not missing (aval_min) then c5=put(aval_min,5.1);
         if not missing(aval_median) then c6=put(aval_median,6.2);
         if not missing(aval_max) then c7=put(aval_max,5.1); 
      end;
   else if paramn in (5,6) then 
       do;
         if not missing(aval_n) then c2=put(aval_n,4.);
         if not missing(aval_mean) then c3=put(aval_mean,5.1);
         if not missing(aval_stddev) then c4=put(aval_stddev,5.1);
         if not missing (aval_min) then c5=put(aval_min,3.);
         if not missing(aval_median) then c6=put(aval_median,5.1);
         if not missing(aval_max) then c7=put(aval_max,3.); 
       end;

   else if paramn in (8) then 
      do;
         if not missing(aval_n) then c2=put(aval_n,4.);
         if not missing(aval_mean) then c3=put(aval_mean,7.3);
         if not missing(aval_stddev) then c4=put(aval_stddev,7.3);
         if not missing (aval_min) then c5=put(aval_min,6.2);
         if not missing(aval_median) then c6=put(aval_median,7.3);
         if not missing(aval_max) then c7=put(aval_max,6.2); 
      end;

   keep paramn param trtan trta avisitn avisit c2-c7;
run;


proc summary data=advs;
   by paramn param trtan trta avisitn avisit;
   where avisitn gt 6;
   var chg;
   output out=chg_stats01 n= mean= std= min= median= max=/autoname;
run;

data chg_stats02;
   set chg_stats01;
   length c8-c13 $20;
   if paramn in (2,4,7) then 
      do;
         if not missing(chg_n) then c8=put(chg_n,4.);
         if not missing(chg_mean) then c9=put(chg_mean,6.2);
         if not missing(chg_stddev) then c10=put(chg_stddev,6.2);
         if not missing (chg_min) then c11=put(chg_min,5.1);
         if not missing(chg_median) then c12=put(chg_median,6.2);
         if not missing(chg_max) then c13=put(chg_max,5.1); 
      end;
   else if paramn in (5,6) then 
       do;
         if not missing(chg_n) then c8=put(chg_n,4.);
         if not missing(chg_mean) then c9=put(chg_mean,5.1);
         if not missing(chg_stddev) then c10=put(chg_stddev,5.1);
         if not missing (chg_min) then c11=put(chg_min,3.);
         if not missing(chg_median) then c12=put(chg_median,5.1);
         if not missing(chg_max) then c13=put(chg_max,3.); 
       end;

   else if paramn in (8) then 
      do;
         if not missing(chg_n) then c8=put(chg_n,4.);
         if not missing(chg_mean) then c9=put(chg_mean,7.3);
         if not missing(chg_stddev) then c10=put(chg_stddev,7.3);
         if not missing (chg_min) then c11=put(chg_min,6.2);
         if not missing(chg_median) then c12=put(chg_median,7.3);
         if not missing(chg_max) then c13=put(chg_max,6.2); 
      end;

   keep paramn param trtan trta avisitn avisit c8-c13;
run;
 
data stats;
   merge aval_stats02 chg_stats02;
   by paramn param trtan trta avisitn avisit;
run;

data final;
   set stats;
   length c1 $50;
   c1="   "||strip(avisit);
run;


proc report data=final nowd headline headskip missing spacing=0 ls=145;
   columns paramn param trtan trta  c1 avisitn ('-Result-' c2-c7) ('-Change-' c8-c13);
   define paramn/ order noprint order=data;
   define param/order noprint order=data;
   define trtan /order noprint order=data;
   define trta /order noprint order=data;
   define avisitn/order noprint order=data;
   define c1 /order order=data "Treatment Group/" "Analysis Visit" width=35 flow;
   define c2 / "  n" width=5 left;
   define c3 / "Mean" width=10;
   define c4 / "SD" width=10;
   define c5 / "Min" width=10;
   define c6 / "Median" width=10;
   define c7 / "Max" width=9;
   define c8 / "  n" width=5 spacing=2 left;
   define c9 / "Mean" width=10;
   define c10 / "SD" width=10;
   define c11 / "Min" width=10;
   define c12/ "Median" width=10;
   define c13/ "Max" width=9;

   break after param/page;
   break after trta / skip;

   compute before _page_;
      length text $145;
      text=repeat('-', (145-length(param))/2)||strip(param)||repeat('-', (145-length(param))/2);
      line @1 text $;
      line @1 "";
   endcomp;

   compute before trta;
      line @2 trta $;
   endcomp;

run;

*redacted;
