proc sql noprint;
      create table _gm as
      select distinct
         paramn
         , paramcd
         , param
         , trt01an
         , trt01a
         , '_9_GM' as stat
         , exp(mean(log(aval))) as value
         , min(aval) as zero
      from
         _adpp (where=(paramcd in ('CMAX','AUCINF','AUCLAST')))
      where pkasfl='Y'
      group by 
         paramn
         , paramcd
         , param
         , trt01an
         , trt01a;
      

   quit;

