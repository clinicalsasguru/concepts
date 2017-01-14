data locfparams;
input usubjid paramcd $8. visitnum lbseq;
avisitn=visitnum;
cards;
001 paramcd 1 1
001 paramcd 3 2
001 paramcd 5 3
;
run;

data sv;
input usubjid visitnum;
avisitn=visitnum;
cards;
001 1
001 2
001 3
001 4
001 5
001 6
001 7
001 8
;
run;

*--------------------------------;
*locf;
*--------------------------------;

proc sort data=locfparams out=locfparams2;
   by usubjid paramcd visitnum;
run;

data locfparams3;
   set locfparams2;
   by usubjid paramcd visitnum;
   *where 3 lt visitnum le 8;
   visitnum2=floor(visitnum);
   length visitnum_c visitnum_c2 $20;
   retain visitnum_c visitnum_c2;
   if first.paramcd then call missing(visitnum_c,visitnum_c2);

   if not missing(avisitn) then do;
   if avisitn=int(avisitn) then visitnum_c=catx('-',visitnum_c,avisitn);
   end;

   visitnum_c2=catx('-',visitnum_c2,visitnum2);
   if last.paramcd;
   keep usubjid paramcd visitnum_c visitnum_c2;
run;

proc sort data=locfparams;
   by usubjid paramcd;
run;

data locfparams4;
   merge locfparams(in=a) locfparams3(in=b);
   by usubjid paramcd;
   if a ;
   visitnum2=floor(visitnum);
run;

data locfparams5;
   set locfparams4;
   length dtype $10;
   if  1 le visitnum2 lt 8 then do;
         avisitn=visitnum2;
         output;
         do visitnum1=visitnum2+1 to 8;
            index1=index(visitnum_c,strip(put(visitnum1,best.)));
            index2=index(visitnum_c2,strip(put(visitnum1,best.)));
            if index1 and index2 then leave;
            else do;
               dtype="LOCF";
               avisitn=visitnum1;
               output;
               if index2 gt 0 then leave;
            end;
         end;
   end;
run;


data locfparams6_;
   set locfparams5;
   length avisit $100;
   if dtype="LOCF";
   if avisitn in (4:8)  then
   avisit="VISIT "||strip(put(avisitn,best.));
   *drop lbseq;
run;

proc sort data=locfparams6_;
   by usubjid paramcd avisitn visitnum;
run;

data locfparams6;
   set locfparams6_;
   by usubjid paramcd avisitn visitnum;
   if last.avisitn;
run;
