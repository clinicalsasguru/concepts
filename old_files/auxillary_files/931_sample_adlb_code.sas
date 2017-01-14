

LIBNAME sdtm "&SDTM" ;
LIBNAME adam "&ADAM" ;
libname metadata "&metadata";
libname comp "&comp";
options mautosource sasautos = ("&bumpath" sasautos) ;
*=================================================;
*Reading input datasets;
*=================================================;

proc sort data=sdtm.lb out=lb(rename=(lbfast=fastrfl));
   by usubjid lbseq;
   where lbstat ne "NOT DONE" and not (LBTESTCD = "GLUC" and LBGRPID = "BG2001-1");
run;

proc sort data=sdtm.supplb out=supplb;
   by usubjid qnam qval;
   where qnam in ("LBGLSTCD" "LBGLSUCD");
run;


data lbglstcd(drop=lbglsucd) lbglsucd(drop=lbtestcd_lab_tst_cd_txt);
   set supplb;
   length lbtestcd_lab_tst_cd_txt $10;
   if not missing(idvarval) then lbseq=input(idvarval,?? best.);
   if qnam="LBGLSTCD" then lbtestcd_lab_tst_cd_txt=qval;
   if qnam="LBGLSUCD" then lbglsucd=input(qval,?? best.);
   if qnam="LBGLSTCD" then output lbglstcd;
   if qnam="LBGLSUCD" then output lbglsucd;
   keep usubjid lbtestcd_lab_tst_cd_txt lbseq lbglsucd;
run;

proc sort data=lbglstcd;
   by usubjid lbseq;
run;

proc sort data=lbglsucd;
   by usubjid lbseq;
run;

data lb temp_lbglstcd temp_lbglsucd;
   merge lb(in=a) lbglstcd(in=b) lbglsucd(in=c);
   by usubjid lbseq;
   if a then output lb;
   if a and not b then output temp_lbglstcd;
   if a and not c then output temp_lbglsucd;
run;

proc sort data=metadata.Lab_ParamCT_Lookup_Table out=pmetadata;
   by lbtestcd_lab_tst_cd_txt cdiscunit;
run;

proc sort data=lb;
   by lbtestcd_lab_tst_cd_txt lbstresu;
run;

data lb01 temp;
   merge lb(in=a) pmetadata(in=b keep=lbtestcd_lab_tst_cd_txt cdiscunit paramcd param 
         parcat2 lab_rslt_typ_txt std_cnvntn_unt_cd rename=(cdiscunit=lbstresu) );
   by lbtestcd_lab_tst_cd_txt lbstresu;
   if a then output lb01;
   if a and not b then output temp;
run; 

data lb01;
   set lb01;
   old_cdiscunit=lbstresu;
run;

proc sort data=lb01;
   by lbtestcd_lab_tst_cd_txt old_cdiscunit;
run;

proc sort data=pmetadata out=pmetadata2;
   by lbtestcd_lab_tst_cd_txt old_cdiscunit;
   where not missing(old_cdiscunit);
run;

data lb02_;
   merge lb01(in=a) pmetadata2(in=b keep=lbtestcd_lab_tst_cd_txt old_cdiscunit paramcd param 
         parcat2 lab_rslt_typ_txt std_cnvntn_unt_cd rename=(paramcd=paramcd_2 param=param_2 parcat2=parcat2_2 
         lab_rslt_typ_txt=lab_rslt_typ_txt_2 std_cnvntn_unt_cd=std_cnvntn_unt_cd_2));
   by lbtestcd_lab_tst_cd_txt old_cdiscunit;
   if a;
   if a and b then do;
      x_flag=1;
      if missing(paramcd) then paramcd=paramcd_2;
      if missing(param) then param=param_2;
      if missing(parcat2) then parcat2=parcat2_2;
      if missing(lab_rslt_typ_txt) then lab_rslt_typ_txt=lab_rslt_typ_txt_2;
      if missing(std_cnvntn_unt_cd) then std_cnvntn_unt_cd=std_cnvntn_unt_cd_2;
   end;
run;

data lb02;
   set lb02_;
   where paramcd not in ("GLUCAGAG" "GLUCZB9S");
   length avisit $100 avalc $100 anrind $20 agrpid $4;
   if visit="SCREENING (VISIT1)" then avisit="VISIT 1";
   else avisit=visit;
   agrpid=lbgrpid;
   if visitnum ne 999 then avisitn=visitnum;

   if length(lbdtc) =19 then do;
      adt=input(substrn(lbdtc,1,10),yymmdd10.);
      atm=input(substrn(lbdtc,12),time8.);
      adtm=input(substrn(lbdtc,1),is8601dt.);
   end;
   else if length(lbdtc)=10 then do;
      adt=input(lbdtc,yymmdd10.);
   end;
   atpt=lbtpt;
   atptn=lbtptnum;
   format adt date9. atm time8. adtm datetime20.;

   if (lab_rslt_typ_txt="N" and not missing(lbstresu)) or
      parcat2 in ("CN" "SI" "SICN" "NM") then do;

      if parcat2 in ("SICN" "SI") then do;
         anrlo=lbstnrlo;
         anrhi=lbstnrhi;
         anrind=lbnrind;
         parcat1=lbcat;
         if not missing(lbstresn) then do; 
         aval=lbstresn;
         anrind=lbnrind;
         end;
         else if index(lbstresc,"<") then do;
               aval=input(compress(lbstresc,'<'),best.)/2;
               avalc=lbstresc;
               if . lt aval lt anrlo then anrind="LOW";
               else if . lt anrlo le aval le anrhi then anrind="NORMAL";
               else if aval gt anrhi gt . then anrind="HIGH";
            end;
         else if index(lbstresc,">") then do;
               aval=input(compress(lbstresc,,'kd'),best.)*1.1;
               avalc=lbstresc;
               if . lt aval lt anrlo then anrind="LOW";
               else if . lt anrlo le aval le anrhi then anrind="NORMAL";
               else if aval gt anrhi gt . then anrind="HIGH";
            end;

      end;

      else if strip(parcat2) in ("NM") then do;;
         anrlo=lbstnrlo;
         anrhi=lbstnrhi;
         anrind=lbnrind;
         parcat1=lbcat;  
         aval=lbstresn;
         anrlo=lbstnrlo;
         anrhi=lbstnrhi;
         anrind=lbnrind;
      end; 


   end;

    else if strip(parcat2) in ("OTH") then do;;
         anrlo=lbstnrlo;
         anrhi=lbstnrhi;
         anrind=lbnrind;
         parcat1=lbcat;  
         anrlo=lbstnrlo;
         anrhi=lbstnrhi;
         anrind=lbnrind;
         avalc=lbstresc;
         aval=lbstresn;
      end; 

    else if strip(parcat2) in ("NNU") then do;;
         anrlo=lbstnrlo;
         anrhi=lbstnrhi;
         anrind=lbnrind;
         parcat1=lbcat;  
         anrlo=lbstnrlo;
         anrhi=lbstnrhi;
         anrind=lbnrind;
         aval=lbstresn;
      end; 

   if visitnum ne int(visitnum) then do;
      if PARAMCD in  ("BILIG01S", "BILIG01C" "BILIG02C", "BILIG02S", "BILDG03C", "BILDG03S") then do;
         if floor(visitnum) =6 then avisit="VISIT 6";
         else if floor(visitnum)=7 then avisit="VISIT 7";
         else if floor(visitnum)=8 then avisit="VISIT 8";
      end;
   end;

         
run;

*======================================================================;
*creating CN records for SI records;
*======================================================================;

data cn01;
   set lb02;
   where parcat2="SI";
   paramtyp="DERIVED";
   drop param paramcd parcat2 avalc;
run;

*--------------------------------------;
*get cn metadata;
*--------------------------------------;

proc sort data=pmetadata out=pmetadata3(keep=lbtestcd_lab_tst_cd_txt std_cnvntn_unt_cd param paramcd parcat2 si_to_cn_cnvrsn_fctr );
   by lbtestcd_lab_tst_cd_txt std_cnvntn_unt_cd;
   where parcat2="CN";
run;

proc sort data=cn01;
   by lbtestcd_lab_tst_cd_txt std_cnvntn_unt_cd;
run;

data cn02;
   merge cn01(in=a) pmetadata3(in=b);
   by lbtestcd_lab_tst_cd_txt std_cnvntn_unt_cd;
   if a;
run;
data cn03;
   set cn02;
   length avalc $100 paramtyp $7;
   paramtyp="DERIVED";
   if indexc(lbstresc,"<>") =0 then do;
       if nmiss(aval,si_to_cn_cnvrsn_fctr)=0 then aval=aval*si_to_cn_cnvrsn_fctr;
   end;

   else do;
   aval=aval*si_to_cn_cnvrsn_fctr;
   if index(lbstresc,"<") gt 0 then
   avalc=compress(catx("","<",strip(put(input(compress(lbstresc,"<"),best.)*si_to_cn_cnvrsn_fctr,best.)))); 
   if index(lbstresc,">") gt 0 then
    avalc=compress(catx("",">",strip(put(input(compress(lbstresc,">"),best.)*si_to_cn_cnvrsn_fctr,best.))));
   end;
   
   if nmiss(anrlo,si_to_cn_cnvrsn_fctr)=0 then anrlo=anrlo*si_to_cn_cnvrsn_fctr;
   if nmiss(anrhi,si_to_cn_cnvrsn_fctr)=0 then anrhi=anrhi*si_to_cn_cnvrsn_fctr;

run;

data lb03;
   set lb02(in=a) cn03(in=b);
   length basetype $20;
   if a then basetype="ORIGINAL";
   if b then basetype="ORIGINAL";
run;

proc sort data=lb03;
   by usubjid;
run;

data lb03;
   merge lb03(in=a) adam.adsl(in=b);
   by usubjid;
   if a;
   agrpid=lbgrpid;
   keep studyid usubjid lbseq paramcd param parcat1 parcat2 aval avalc adt atm adtm visit visitnum avisit avisitn anrlo anrhi anrind
      sex ittfl saffl trt01an trt01a race age agrpid trtsdt trtedt effdt lborres basetype fastrfl trt01an trt01a trt01p trt01pn;
 
run;

data lb03;
   set lb03;
     if upcase(strip(parcat1))= "CHEMISTRY" then parcat1n =1;
     else if upcase(strip(parcat1))= "ENDOCRINE" then parcat1n =2;
     else if upcase(strip(parcat1))= "GENOMICS" then parcat1n =3;
     else if upcase(strip(parcat1))= "HEMATOLOGY" then parcat1n =4;
     else if upcase(strip(parcat1))= "SEROLOGY/VIROLOGY" then parcat1n =5;
     else if upcase(strip(parcat1))= "URINALYSIS" then parcat1n =6;
     else if upcase(strip(parcat1))= "BIOMARKER" then parcat1n =7;
     else if upcase(strip(parcat1))= "COAGULATION" then parcat1n =8;
     else if upcase(strip(parcat1))= "IMMUNOLOGY" then parcat1n =9;


   if upcase(strip(parcat2))= "SI" then parcat2n =1;
   else if upcase(strip(parcat2))= "CN" then parcat2n =2;
   else if upcase(strip(parcat2))= "SICN" then parcat2n =3;
   else if upcase(strip(parcat2))= "NM" then parcat2n =4;
   else if upcase(strip(parcat2))= "NNU" then parcat2n =5;
   else if upcase(strip(parcat2))= "OTH" then parcat2n =6;

   if nmiss(adt,trtsdt)=0 then ady=adt-trtsdt +(adt>=trtsdt);
   if visitnum le 3 then prefl="Y";

run;

   
*=====================================================================================;
*Deriving new param codes;
*=====================================================================================;

*------------------------------------------------------------;
* Average for visit 2 and 3 ""CYK1DE6S"", ""CYK1DE7S"";
*------------------------------------------------------------;

data cykavg01;
   set lb03;
   where paramcd in ("CYK1DE6S", "CYK1DE7S") and prefl="Y" and not missing(aval);
run;

proc sort data=cykavg01;
   by usubjid paramcd visitnum adt atm;
run;

data cyknrecs;
   set cykavg01 nobs=numobs;
   by usubjid paramcd visitnum adt atm;
   if first.paramcd then nrecs_pre=1;
   else nrecs_pre+1;
   if last.paramcd;
   keep usubjid paramcd nrecs_pre;
run;

data cykavg02;
   merge cykavg01(in=a) cyknrecs(in=b);
   by usubjid paramcd;
   if first.paramcd then do;
   call missing(nrecs,sum);
   end;
   nrecs+1;
run;

data cykavg03;
   set cykavg02;
   by usubjid paramcd;
   if nrecs=nrecs_pre or nrecs=nrecs_pre-1;
   rename aval=s_aval;
run; 

data cykavg;
   set cykavg03;
   length dtype $10 basetype $20;
   by usubjid paramcd visitnum adt atm;
   if first.paramcd then call missing(totrecs,sum_x);
   totrecs+1;
   sum_x+s_aval;
   if last.paramcd ;
   if nmiss(sum_x,totrecs)=0 then aval=sum_x/totrecs;
   dtype="AVERAGE";
   basetype="AVERAGE";
   ablfl="Y";

   if . lt aval lt anrlo then anrind="LOW";
   else if . lt anrlo le aval le anrhi then anrind="NORMAL";
   else if aval gt anrhi gt . then anrind="HIGH";
   drop anrind atm avalc adtm ady lbseq sum_x s_aval;
run;




*--------------------------------------;
*eGFR;
*--------------------------------------;
data egfr01;
   set lb03;
   where paramcd="CREAI17C";
   if not missing(aval) then s_aval=round(aval,0.01); 
   drop aval avalc anrind anrlo anrhi ;
run;

%macro egfr(var=);
if SEX='F' then do;
   if . lt s_&var.<=0.7 then do;
      if RACE='BLACK OR AFRICAN AMERICAN' then &var. = 166 * (s_&var./0.7)**(-0.329) * (0.993)**age;
      else if not missing(race) then &var. = 144 * (s_&var. /0.7) ** (-0.329) * (0.993) ** age;
      end;
      else if s_&var.>0.7  then do;
      if RACE='BLACK OR AFRICAN AMERICAN' then &var. = 166 * (s_&var. /0.7) ** (-1.209) * (0.993) ** age;
      else if not missing(race) then &var. = 144 * (s_&var. /0.7) ** (-1.209) * (0.993) ** age;
      end;
   end;

   else if SEX='M' then do;
      if . lt s_&var.<=0.9  then do;
      if RACE='BLACK OR AFRICAN AMERICAN' then &var. = 163 * (s_&var. /0.9) ** (-0.411) * (0.993) ** age;
      else if not missing(race) then &var. = 141 * (s_&var. /0.9) ** (-0.411) * (0.993) ** age;
      end;
      else if s_&var.>0.9  then do;
      if RACE='BLACK OR AFRICAN AMERICAN' then &var. = 163 * (s_&var. /0.9) ** (-1.209) * (0.993) ** age;
      else if not missing(race) then &var. = 141 * (s_&var. /0.9) ** (-1.209) * (0.993) ** age;
      end;
   end;
%mend;

data egfr;
   set egfr01;
   paramcd="eGFR";
   basetyp="ORIGINAL";
   param="Estimated Glomerular Filtration Rate (mL/min/1.73m^2)";
   %egfr(var=aval);

   drop s_aval ;
run;

*---------------------------------------;
*alptbil;
*---------------------------------------;

data alptbil01;
   set lb03;
   where paramcd in ("ALPE13S" "BILIG01S");
   rename aval=s_aval;
   drop avalc lbseq anrind anrlo ;
run;

proc sort data=alptbil01;
   by usubjid visitnum visit adtm adt atm paramcd;
run;

data alptbil;
   set alptbil01;
   length avalc $100;
   by usubjid visitnum visit adtm adt atm paramcd;
   retain alpe13s alp13anrhi;
   if first.atm and last.atm then delete;
   if first.atm then do;
   call missing(alpe13s,alp13anrhi);
   alpe13s=s_aval;
   alp13anrhi=anrhi;
   end;
   if last.atm ;
   paramcd="ALPTBIL";
   param="ALP >2.5X ULN and total bilirubin >2X ULN";
   if alpe13s > 2.5*alp13anrhi and  s_AVAL > 2*ANRHI then AVALC = "Y";
   else avalc="N";
   drop s_aval alp: anrhi ;
run;


*---------------------------------------;
*alttb;
*---------------------------------------;

data alttb01;
   set lb03;
   where paramcd in ("ALTE03S" "BILIG01S");
   rename aval=s_aval;
   drop avalc lbseq agrpid anrind anrlo ;
run;

proc sort data=alttb01;
   by usubjid visitnum visit adtm adt atm paramcd;
run;

data alttb;
   set alttb01;
   by usubjid visitnum visit adtm adt atm paramcd;
   retain alte03s alt03anrhi;
   length avalc $100;
   if first.atm and last.atm then delete;
   if first.atm then do;
   call missing(alte03s,alt03anrhi);
   alte03s=s_aval;
   alt03anrhi=anrhi;
   end;
   if last.atm ;
   paramcd="ALTTB";
   param="ALT >=3X ULN and total bilirubin >=2X ULN";
   if alte03s >= 3*alt03anrhi gt .  and  s_AVAL >= 2*ANRHI gt . then AVALC = "Y";
   else avalc="N";
   drop s_aval alt: anrhi;
run;

*---------------------------------------;
*asttb;
*---------------------------------------;

data asttb01;
   set lb03;
   where paramcd in ("ASTE01S" "BILIG01S");
   rename aval=s_aval;
   drop avalc lbseq agrpid anrind anrlo ;
run;

proc sort data=asttb01;
   by usubjid visitnum visit adtm adt atm paramcd;
run;

data asttb;
   set asttb01;
   by usubjid visitnum visit adtm adt atm paramcd;
   retain aste01s ast03anrhi;
   length avalc $100;
   if first.atm and last.atm then delete;
   if first.atm then do;
   call missing(aste01s,ast03anrhi);
   aste01s=s_aval;
   ast03anrhi=anrhi;
   end;
   if last.atm ;
   paramcd="ASTTB";
   param="AST >3X ULN and total bilirubin >2X ULN";
   if aste01s > 3*ast03anrhi and  s_AVAL > 2*ANRHI then AVALC = "Y";
   else avalc="N";
   drop s_aval ast: anrhi ;
run;


*---------------------------------------;
*altsttbr;
*---------------------------------------;

data altsttbr01;
   set lb03;
   where paramcd in ("ASTE01S" "ALTE03S" "BILIG01S"  "INRQ81S");
   rename aval=s_aval;
   drop avalc lbseq agrpid anrind anrlo ;
run;

proc sort data=altsttbr01;
   by STUDYID USUBJID fastrfl VISITNUM VISIT adt atm
   adtm  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT ady prefl paramcd s_aval;
;
run;

proc transpose data=altsttbr01 out=altsttbr01_t_aval(drop=_name_);
   by STUDYID USUBJID fastrfl VISITNUM VISIT adt atm
   adtm  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT ady prefl  ;
   var s_aval;
   id paramcd;
run;

proc transpose data=altsttbr01 out=altsttbr01_t_anrhi(drop=_name_) prefix=anrhi;
   by STUDYID USUBJID fastrfl VISITNUM VISIT adt atm
   adtm  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT ady prefl  ;
   var anrhi;
   id paramcd;

run;

data altsttbr01_m;
   merge altsttbr01_t_aval(in=a) altsttbr01_t_anrhi(in=b);
   by STUDYID USUBJID fastrfl VISITNUM VISIT adt atm
   adtm  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT ady prefl  ;
   if a and b;
run;

data altsttbr;
   set altsttbr01_m;
   length avalc $100 paramcd $8 param $200;

   if missing(bilig01s) and missing(INRQ81S) then delete;
   if missing(aste01s) and missing(alte03s) then delete;

   *"ASTE01S" "ALTE03S" "BILIG01S"  "INRQ81S";

   paramcd="ALTSTTBR";
   param="ALT or AST >=3X ULN and (total bilirubin level >=2X ULN or INR >=1.5X ULN)";

   if ((ASTE01S >= 3*anrhiASTE01S gt .) or (ALTE03S >= 3*anrhiALTE03S gt .)) and
   ((BILIG01S >= 2*anrhiBILIG01S gt .) or (INRQ81S >= 1.5*anrhiINRQ81S gt .)) then avalc = "Y";
   else avalc="N";
run;

*---------------------------------------;
*altasttb;
*---------------------------------------;
data altasttb01;
   set lb03;
   where paramcd in ("ASTE01S" "ALTE03S" "BILIG01S" );
   rename aval=s_aval;
   drop avalc lbseq agrpid anrind anrlo ;
run;

proc sort data=altasttb01;
   by STUDYID USUBJID fastrfl VISITNUM VISIT parcat2 adt atm
   adtm  parcat1  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT parcat1n parcat2n ady prefl paramcd s_aval;
;
run;

proc transpose data=altasttb01 out=altasttb01_t_aval(drop=_name_);
   by STUDYID USUBJID  fastrfl VISITNUM VISIT parcat2 adt atm
   adtm parcat1  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT parcat1n parcat2n ady prefl;
   var s_aval;
   id paramcd;
run;

proc transpose data=altasttb01 out=altasttb01_t_anrhi(drop=_name_) prefix=anrhi;
   by STUDYID USUBJID  fastrfl VISITNUM VISIT parcat2 adt atm
   adtm parcat1  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT parcat1n parcat2n ady prefl;
   var anrhi;
   id paramcd;
run;


data altasttb01_m;
   merge altasttb01_t_aval(in=a) altasttb01_t_anrhi(in=b);
   by STUDYID USUBJID  fastrfl VISITNUM VISIT parcat2 adt atm
   adtm parcat1  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT parcat1n parcat2n ady prefl;
   if a and b;
run;

data altasttb;
   set altasttb01_m;
   length avalc $100 paramcd $8 param $200;
   if missing(bilig01s) then delete;
   if missing(aste01s) and missing(alte03s) then delete;
   paramcd="ALTASTTB";
   param="(ALT or AST >=3X ULN) and total bilirubin >=2X ULN";
   if ((alte03s >= 3*anrhialte03s>.) or (aste01s >= 3*anrhiaste01s >.)) and
   ((bilig01s >= 2*anrhibilig01s >.)) then avalc = "Y";
   else avalc="N";
run;

*---------------------------------------;
*ALT35TB2;
*---------------------------------------;
data ALT35TB201;
   set lb03;
   where paramcd in ("ALTE03S" "BILIG01S" );
   rename aval=s_aval;
   drop avalc lbseq agrpid anrind anrlo ;
run;

proc sort data=ALT35TB201;
   by STUDYID USUBJID fastrfl VISITNUM VISIT parcat2 adt atm
   adtm  parcat1  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT parcat1n parcat2n ady prefl paramcd s_aval;
;
run;

proc transpose data=ALT35TB201 out=ALT35TB201_t_aval(drop=_name_);
   by STUDYID USUBJID  fastrfl VISITNUM VISIT parcat2 adt atm
   adtm parcat1  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT parcat1n parcat2n ady prefl;
   var s_aval;
   id paramcd;
run;

proc transpose data=ALT35TB201 out=ALT35TB201_t_anrhi(drop=_name_) prefix=anrhi;
   by STUDYID USUBJID  fastrfl VISITNUM VISIT parcat2 adt atm
   adtm parcat1  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT parcat1n parcat2n ady prefl;
   var anrhi;
   id paramcd;
run;


data ALT35TB201_m;
   merge ALT35TB201_t_aval(in=a) ALT35TB201_t_anrhi(in=b);
   by STUDYID USUBJID  fastrfl VISITNUM VISIT parcat2 adt atm
   adtm parcat1  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT parcat1n parcat2n ady prefl;
   if a and b;
run;

data ALT35TB2;
   set ALT35TB201_m;
   length avalc $100 paramcd $8 param $200;
   if missing(bilig01s) then delete;
   if missing(alte03s) then delete;
   paramcd="ALT35TB2";
   param="3X ULN <= ALT <5X ULN and total bilirubin <2X ULN";
   if ((. lt 3*anrhialte03s <= alte03s <= 5*anrhialte03s)) and
   ((. lt bilig01s < 2*anrhibilig01s )) then avalc = "Y";
   else avalc="N";
run;

*---------------------------------------;
*TBALTAST;
*---------------------------------------;
data TBALTAST01;
   set lb03;
   where paramcd in ("ALTE03S" "BILIG01S"  "BILIG01C" "ASTE01S");
   rename aval=s_aval;
   drop avalc lbseq agrpid anrind anrlo ;
run;

proc sort data=TBALTAST01;
   by STUDYID USUBJID fastrfl VISITNUM VISIT  adt atm
   adtm  parcat1  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT parcat1n  ady prefl paramcd s_aval;
;
run;

proc transpose data=TBALTAST01 out=TBALTAST01_t_aval(drop=_name_);
   by STUDYID USUBJID  fastrfl VISITNUM VISIT  adt atm
   adtm parcat1  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT parcat1n  ady prefl;
   var s_aval;
   id paramcd;
run;

proc transpose data=TBALTAST01 out=TBALTAST01_t_anrhi(drop=_name_) prefix=anrhi;
   by STUDYID USUBJID  fastrfl VISITNUM VISIT  adt atm
   adtm parcat1  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT parcat1n ady prefl;
   var anrhi;
   id paramcd;
run;


data TBALTAST01_m;
   merge TBALTAST01_t_aval(in=a) TBALTAST01_t_anrhi(in=b);
   by STUDYID USUBJID  fastrfl VISITNUM VISIT  adt atm
   adtm parcat1  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT parcat1n  ady prefl;
   if a and b;
run;

data TBALTAST;
   set TBALTAST01_m;
   length avalc $100 paramcd $8 param $200;
   if nmiss(bilig01s,alte03s,bilig01c,aste01s) gt 0 then delete;
   paramcd="TBALTAST";
   param="Total bilirubin >=2X ULN and <3 mg/dL, ALT and AST < ULN";
   if ((BILIG01S >= 2*ANRHIBILIG01S) and (BILIG01C <3)
   and (ALTE03S <ANRHIALTE03S) and (ASTE01S <ANRHIASTE01S)) then avalc = "Y";
   else avalc="N";
run;
*------------------------------------------------;
*ALTAST2W;
*------------------------------------------------;

data altast2w01;
   set lb03;
   where paramcd in ("ALTE03S" "ASTE01S") ;*and visitnum=int(visitnum);
   rename aval=s_aval;
   drop avalc lbseq agrpid anrind anrlo ;
run;

proc sort data=altast2w01;
   by STUDYID USUBJID fastrfl VISITNUM VISIT  adt atm
   adtm  parcat1  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT parcat1n parcat2n parcat2  ady prefl paramcd s_aval;
;
run;

proc transpose data=altast2w01 out=altast2w01_t_aval(drop=_name_);
   by STUDYID USUBJID  fastrfl VISITNUM VISIT  adt atm
   adtm parcat1  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT parcat1n parcat2n parcat2 ady prefl;
   var s_aval;
   id paramcd;
run;

proc transpose data=altast2w01 out=altast2w01_t_anrhi(drop=_name_) prefix=anrhi;
   by STUDYID USUBJID  fastrfl VISITNUM VISIT  adt atm
   adtm parcat1  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT parcat1n parcat2n parcat2 ady prefl;
   var anrhi;
   id paramcd;
run;


data altast2w01_m;
   merge altast2w01_t_aval(in=a) altast2w01_t_anrhi(in=b);
   by STUDYID USUBJID  fastrfl VISITNUM VISIT  adt atm
   adtm parcat1  basetype AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT parcat1n parcat2n parcat2 ady prefl;
   if a and b;
run;

data altast2w02;
   set altast2w01_m;
   if nmiss(alte03s,aste01s) gt 0 then delete;
run;

data altast2w02_datepre1;
   set altast2w02;
   by usubjid;
   length avalc_temp $2;
   retain avalc_temp dategt;
   format dategt date9.;
   if first.usubjid then call missing(avalc_temp, dategt);
   if ((alte03s >= 5*ANRHIalte03s gt . ) and (aste01s >= 5*ANRHIaste01S gt . )) then
      do; 
         avalc_temp="Y"; avalc_temp2="Y";  dategt=adt;
      end;
   if avalc_temp="Y";

run;

proc sort data=altast2w02_datepre1;
   by usubjid adt avalc_temp;
run;

data w2_yes;
   set altast2w02_datepre1;
   by usubjid adt avalc_temp ;
   if first.usubjid then do;e_date=.; down_flag=.; end;
   if first.usubjid then e_date=adt;
   retain e_date down_flag;
   format e_date date9.;
   if avalc_temp2 ne "Y" then down_flag=1;
   if down_flag ne 1 then duration=adt-e_date;
   if duration ge 14 then avalc="Y";
   if avalc="Y"; 
   paramcd="ALTAST2W";
   param="ALT or AST >=5X ULN for more than 2 weeks";
   call missing(adt, visit, visitnum, atm,adtm);
run;

proc sql;
   create table w2_no as
      select * from altast2w02 
      where usubjid not in (
      select distinct usubjid 
      from w2_yes);
quit;

proc sort data=w2_no nodupkey;
   by usubjid;
run;

data w2_no;
   set w2_no;
   avalc="N";
   paramcd="ALTAST2W";
   param="ALT or AST >=5X ULN for more than 2 weeks";
run;

proc sql;
   create table w2_no2 as
      select * from w2_no
      where usubjid in (
      select distinct usubjid 
      from adam.adsl
      where randfl="Y");
 quit;

data altast2w;
   set w2_yes w2_no2;
   by usubjid;
   keep STUDYID USUBJID parcat1  AGE SEX RACE TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT SAFFL
   ITTFL EFFDT parcat1n parcat2n parcat2 avalc paramcd param;
run;

*---------------------------------------;
*altbyast;
*---------------------------------------;

data altbyast01;
   set lb03;
   where paramcd in ("ALTE03S" "ASTE01S");
   rename aval=s_aval;
   drop avalc lbseq agrpid anrind;
run;

proc sort data=altbyast01;
   by usubjid avisitn avisit adtm adt atm paramcd;
run;

data altbyast;
   set altbyast01;
   length anrind $20;
   by usubjid avisitn avisit adtm adt atm paramcd;
   retain alte03s alt03anrhi alt03anrlo;
   if first.atm and last.atm then delete;
   if first.atm then do;
   call missing(alte03s,alt03anrhi,alt03anrlo);
   alte03s=s_aval;
   alt03anrhi=anrhi;
   alt03anrlo=anrlo;
   end;
   if last.atm ;
   paramcd="ALTbyAST";
   param="ALT by AST";
   if nmiss(alte03s,s_aval)=0 and s_aval ne 0 then aval=alte03s/s_aval;
   anrlo=alt03anrlo/anrlo;
   anrhi=alt03anrhi/anrhi;

   if . lt aval lt anrlo then anrind="LOW";
   else if . lt anrlo le aval le anrhi then anrind="NORMAL";
   else if aval gt anrhi gt . then anrind="HIGH";

   drop s_aval alt: anrind anrlo anrhi;
run;

*------------------------------------------------------------------------------;
*"LHDLS74S", "LLDLF54S", "LCHDF50S", "LTRIG52S", "LFATA44S";
*------------------------------------------------------------------------------;

data logparam01;
   set lb03;
   where paramcd in  ("HDLS74S", "LDLF54S", "CHOLF50S", "TRIGF52S", "FATAF44S") ;
   drop agrpid avalc anrind;
   rename aval=s_aval;
run;

data logparam;
   set logparam01;
   length anrind $20;
   if s_aval gt 0 then aval=log(s_aval);
   if anrlo gt 0 then anrlo=log(anrlo);
   if anrhi gt 0 then anrhi=log(anrhi);

   if . lt aval lt anrlo then anrind="LOW";
   else if . lt anrlo le aval le anrhi then anrind="NORMAL";
   else if aval gt anrhi then anrind="HIGH";

   if paramcd="HDLS74S" then do; paramcd="LHDLS74S"; param="Log of SERUM HDL Cholesterol 3RD GENERATION, ENZYMATIC (mmol/L)"; end;
   else if paramcd="LDLF54S" then do;  paramcd="LLDLF54S"; param="Log of SERUM LDL Cholesterol FRIEDWALD CALC. (mmol/L)"; end;
   else if paramcd="CHOLF50S" then do; paramcd="LCHDF50S"; param="Log of SERUM Cholesterol (mmol/L)"; end;
   else if paramcd="TRIGF52S" then do; paramcd="LTRIG52S"; param="Log of SERUM Triglycerides (mmol/L)"; end;
   else if paramcd="FATAF44S" then do; paramcd="LFATA44S"; param="Log of SERUM Free Fatty Acid (mmol/L)"; end;
   drop s_aval;
run;

data new_paramcds_pre;
   set logparam(in=x)
       alttb(in=a)
       asttb(in=b)
       altbyast
       alptbil(in=c)
       egfr
       altsttbr(in=d)
       altasttb(in=e)
      ALT35TB2(in=f)
      altast2w(in=g)
      TBALTAST(in=h);
       length basetype $20;
       basetype="ORIGINAL";

       if a or b or c or d or e or f or g or h then do;
         aval_temp_flag=1;
         if avalc="Y" then aval=1;
         else if avalc="N" then aval=0;
         if visitnum=int(visitnum) then do;
            if visitnum ne 999 then avisitn=visitnum;
             if visit="SCREENING (VISIT1)" then avisit="VISIT 1";
             else avisit=visit;
         end;
         else do;
            avisitn=.;
            avisit="";
         end;
      end;

run;

data new_paramcds;
   set new_paramcds_pre ;
run;

data lb04;
   length paramtyp $20  ;
   set lb03(in=a)
       new_paramcds(in=b)
      cykavg;
   if b then paramtyp="DERIVED";
   if a then level=0;
   else if b then level=2;

   if not missing(trtedt) then trtedt5=trtedt+5;
   if . lt ADT le trtedt5 and visitnum > 3 and visitnum <= 8 then ANL03FL = "Y";

   if not missing(anrind) then do;
      if anrind="LOW" then anrindn=1;
      else if anrind="NORMAL" then anrindn=2;
      else if anrind="HIGH" then anrindn=3;
      else if anrind="ABNORMAL" then anrindn=4;
   end;

run;

*--------------------------------------------------------------;
*baseline flag derivation;
*--------------------------------------------------------------;

proc sort data=lb04;
   by usubjid paramcd basetype adtm adt atm lbseq;
run;

data pretrt trt;
   set lb04;
   if prefl="Y" /*and lborres not in ("", "RESULT PENDING")*/ and paramcd not in ("CYK1DE6S", "CYK1DE7S") then output pretrt;
   else output trt;
run;

data pretrt;
   set pretrt; 
   by usubjid paramcd basetype adtm adt atm lbseq;
   if last.basetype then do;
      ablfl="Y";
      basedate=adt;
      basedttime=adtm;
      bnrind=anrind;
      bnrindn=anrindn;
      if parcat2 ne "OTH" and aval_temp_flag ne 1 then base=aval;
      if parcat2 in ("OTH") or paramcd in ("ALTASTTB" "ALPTBIL" "ALTTB" "ASTTB" "ALTSTTBR" 
   "ALT35TB2" "ALTAST2W" "TBALTAST") then basec=avalc; 
   end;
   format basedate date9. basedttime datetime20.;
run;

proc sort data=pretrt out=basedate(keep=usubjid paramcd  basedate basedttime base bnrind
      bnrindn basec ) nodupkey;
   by usubjid paramcd  ;
   where ablfl="Y";
run;

data lb05;
   set pretrt trt;
   by usubjid paramcd basetype adtm adt atm lbseq;
   drop basedate basedttime base basec bnrind bnrindn ;
run;

*-----------------------------------------------------------------;
*deriving avisitn;
*-----------------------------------------------------------------;

data lb05;
   set lb05;
   if avisitn lt 90 then call missing(avisitn);
   if ablfl="Y" then avisit="VISIT 3 (BASELINE)";
   if avisit="VISIT 1" then avisitn=1;
   else if avisit="VISIT 2" then avisitn=2;
   else if avisit in ("VISIT 3 (BASELINE)" "VISIT 3")  then avisitn=3;
   else if avisit="VISIT 4" then avisitn=4;
   else if avisit="VISIT 5" then avisitn=5;
   else if avisit="VISIT 6" then avisitn=6;
   else if avisit="VISIT 7" then avisitn=7;
   else if avisit="VISIT 8" then avisitn=8;
   else if avisit="FOLLOW-UP 1" then avisitn=9;
   else if avisit="FOLLOW-UP 2" then avisitn=10;
run;

*-------------------------------------;
*derving avalcat2;
*-------------------------------------;

data lb05;
   set lb05;
   length avalcat2 $20;
   if paramcd in ('ALTE03S', 'ASTE01S', 'GGTE17S', 'ALPE13S', 'BILIG01S', 'BILDG03S', 'BILIG02S') /*and 3 lt visitnum le 6*/
   and visitnum gt 3
   then do;
      if nmiss(anrlo,aval,anrhi)=0 then do;
         if . < AVAL < ANRLO then do; AVALCAT2 = "<1 LLN"; avalca2n=1; avalca2nt=1; end;
         if ANRLO <= AVAL <= ANRHI then do; AVALCAT2 = "Normal"; avalca2n=2; avalca2nt=0; end;
         if ANRHI < AVAL <= 2*ANRHI  then do; AVALCAT2 = ">1 - 2 ULN"; avalca2n=3; avalca2nt=2; end;
         if 2*ANRHI < AVAL <= 3*ANRHI  then do; AVALCAT2 = ">2 - 3 ULN"; avalca2n=4; avalca2nt=3; end;
         if 3*ANRHI < AVAL <= 4*ANRHI  then do; AVALCAT2 = ">3 - 4 ULN"; avalca2n=5; avalca2nt=4; end;
         if 4*ANRHI < AVAL <= 5*ANRHI  then do; AVALCAT2 = ">4 - 5 ULN"; avalca2n=6; avalca2nt=5; end;
      end;
   end;
run;

*------------------------------------------------------------;
*deriving ittrfl;
*------------------------------------------------------------;
data lb05;
   set lb05;
   if paramcd in  ("HBA1K48C", "HBA1K48S", "ALTE03S", "ALTE03C", "ASTE01S", "ASTE01C", "ALPE13S", "ALPE13C", "GGTE17S", "GGTE17C", "BILIG01S", "BILIG01C", "BILIG02C", "BILIG02S", "BILDG03C", "BILDG03S", "FATAF44C", "FATAF44S", "GLUCF30C", "GLUCF30S", 
   "INSUP28C", "INSUP28S", "ELFSJI4J", "CYK1DE7S", "CYK1DE6S", 'P3NPS27S', 'HYALQ63S', 'TIMPJ13S') then do;
      if ADT > EFFDT > .  then ITTRFL = " ";
      else if . < adt <= effdt then ITTRFL = "Y";
      end;
run;

*=====================================================================;
*deriving level 1 records;
*=====================================================================;

proc sort data=lb05 out=lv1base(drop= agrpid );
   where parcat2 in ("SI" "SICN" "NM" "CN" "NNU" "") and not missing(aval) and prefl ne "Y";
   by usubjid paramcd basetype aval visitnum lbseq;
run;

*-------------------------------------------------------;
*POST BASELINE MIN/POST BASELINE MAX;
*-------------------------------------------------------;

%macro level1(indsn=lv1base,
              where=,
              sortby=,
              avisit=,
              avisitn=,
              dtype=);

proc sort data=lv1base out=lv1base_temp;
   by &sortby.;
   where &where.;
run;

data visit&avisitn.;
   set lv1base_temp;
   length dtype $10;
   by &sortby.;
   if first.basetype;
   dtype="&dtype.";
   avisitn=&avisitn.;
   avisit="&avisit.";
   basetype="ORIGINAL";
   level=1;
   *drop lbseq;
run;
   
%mend level1;

%level1(indsn=lv1base,
              where=%str(visitnum gt 3),
              sortby=%str(usubjid paramcd basetype aval visitnum lbseq),
              avisit=%str(POST BASELINE MIN),
              avisitn=97,
              dtype=MINIMUM);

%level1(indsn=lv1base,
              where=%str(visitnum gt 3),
              sortby=%str(usubjid paramcd basetype descending aval visitnum lbseq),
              avisit=%str(POST BASELINE MAX),
              avisitn=98,
              dtype=MAXIMUM);

%level1(indsn=lv1base,
              where=%str(visitnum gt 3 and visitnum le 6),
              sortby=%str(usubjid paramcd basetype aval visitnum lbseq),
              avisit=%str(POST BASELINE MIN 6 MONTHS),
              avisitn=102,
              dtype=MINIMUM);

%level1(indsn=lv1base,
              where=%str(visitnum gt 3 and visitnum le 6),
              sortby=%str(usubjid paramcd basetype descending aval visitnum lbseq),
              avisit=%str(POST BASELINE MAX 6 MONTHS),
              avisitn=101,
              dtype=MAXIMUM);

%level1(indsn=lv1base,
              where=%str(anl03fl="Y"),
              sortby=%str(usubjid paramcd basetype descending aval visitnum lbseq),
              avisit=%str(POST BASELINE MAX TREATMENT PERIOD),
              avisitn=103,
              dtype=MAXIMUM);

%level1(indsn=lv1base,
              where=%str(anl03fl="Y"),
              sortby=%str(usubjid paramcd basetype aval visitnum lbseq),
              avisit=%str(POST BASELINE MIN TREATMENT PERIOD),
              avisitn=104,
              dtype=MINIMUM);

%level1(indsn=lv1base,
              where=%str(anl03fl="Y" and visitnum le 6),
              sortby=%str(usubjid paramcd basetype descending aval visitnum lbseq),
              avisit=%str(POST BASELINE MAX 6 MONTHS OF TREATMENT PERIOD),
              avisitn=105,
              dtype=MAXIMUM);

%level1(indsn=lv1base,
              where=%str(anl03fl="Y" and visitnum le 6),
              sortby=%str(usubjid paramcd basetype aval visitnum lbseq),
              avisit=%str(POST BASELINE MIN 6 MONTHS OF TREATMENT PERIOD),
              avisitn=106,
              dtype=MINIMUM);

%level1(indsn=lv1base,
              where=%str(visitnum gt 3 and not missing(avalca2nt)),
              sortby=%str(usubjid paramcd basetype descending avalca2nt visitnum lbseq),
              avisit=%str(WORSE POST BASELINE),
              avisitn=99,
              dtype=WORSE);

%level1(indsn=lv1base,
              where=%str(anl03fl="Y" and not missing(avalca2nt)),
              sortby=%str(usubjid paramcd basetype descending avalca2nt visitnum lbseq),
              avisit=%str(WORSE POST BASELINE TREATMENT PERIOD),
              avisitn=107,
              dtype=WORSE);


*===========================================================================;
*Deriving avisitn=110 and avisitn=111 based records;
*===========================================================================;

data avisit110_pre avisit111_pre;
   set lb05;
   *where paramcd in  ("HBA1K48C", "HBA1K48S", "ALTE03S", "ALTE03C", "ASTE01S", "ASTE01C", "ALPE13S", "ALPE13C", "GGTE17S", "GGTE17C", "BILIG01S", "BILIG01C", "BILIG02C", "BILIG02S", "BILDG03C", "BILDG03S", "FATAF44C", "FATAF44S", "GLUCF30C", "GLUCF30S", 
   "INSUP28C", "INSUP28S", "ELFSJI4J", "CYK1DE7S", "CYK1DE6S", 'P3NPS27S', 'HYALQ63S', 'TIMPJ13S');
   where paramcd in ('ALPE13S', 'BILIG01S', 'BILIG02S', 'BILDG03S', 'GGTE17S', 'CYK1DE6S', 'CYK1DE7S' , 'eGFR', 
   'CHOLF50S', 'HDLS74S', 'LDLF54S', 'TRIGF52S', 'FATAF44S', 'BHYXF23S', 'ELFSJI4J', 'HYALQ63S', 'P3NPS27S', 'TIMPJ13S');

run;

proc sort data=avisit110_pre;
   by usubjid paramcd adt;
   where anl03fl="Y" and aval ne .;
run;

data avisit110 avisit111_date2(keep=usubjid paramcd adt date2 rename=(adt=adt10));
   set avisit110_pre;
   length dtype $10;
   by usubjid paramcd basetype adt;
   if last.basetype;
   ablfl="Y";
   avisitn=110;
   base=aval;
   bnrind=anrind;
   bnrindn=anrindn;
   avisit="LAST ON-TREATMENT RESULT";
   date2=intnx('week',ADT,16,"sameday");
   format date2 date9.;
   basetype="LAST ON-TREATMENT";
run;

proc sort data=avisit111_pre;
   by usubjid paramcd;
run;
*---------------------------------;

data avisit111_pre2;
   merge avisit111_pre(in=a) avisit111_date2(in=b);
   by usubjid paramcd;
   if a and b;
   if (. lt adt10 lt adt) and missing(anl03fl) then flag_date2=1;
run;

proc sort data=avisit111_pre2 out=avisit111_pre3;
   by usubjid paramcd adt;
   where (. lt adt10 lt adt) and missing(anl03fl) and not missing(aval);
run;

data avisit111_pre3;
   set avisit111_pre3;
   if nmiss(adt,date2)=0 then diffdate2=abs(adt-date2);
run;

data temp_xx;
   set avisit111_pre3;
   keep usubjid paramcd adt adt10 date2 flag_date2 diffdate2;
run;

proc sort data=avisit111_pre3;
   by usubjid paramcd diffdate2;
run;

data avisit111;
   set avisit111_pre3;
   by usubjid paramcd diffdate2;
   if first.paramcd;
   avisitn=111;
   avisit="FOLLOW-UP RESULT";
   basetype="LAST ON-TREATMENT";
run;

data temp_xxx;
   set avisit111_pre3;
   by usubjid paramcd diffdate2;
   if first.paramcd then tempflg=1;
   avisitn=111;
   avisit="FOLLOW-UP RESULT";
   basetype="LAST ON-TREATMENT";
   keep usubjid paramcd adt adt10 diffdate2 tempflg date2;
run;

data avisit110_base;
   set avisit110;
   keep usubjid paramcd base basedate bnrind bnrindn ;
   base=aval;
   basedate=adt;
   bnrind=anrind;
   bnrindn=anrindn;
run;

data avisit111;
   merge avisit111(in=a) avisit110_base(in=b);
   by usubjid paramcd;
   if a;
run;

*===========================================================================;
*deriving locf records;
*===========================================================================;
data locfparams oth_locf_params;
   set lb05;
   if paramcd in ("HBA1K48C", "HBA1K48S", "ALTE03S", "ALTE03C", "ASTE01S", "ASTE01C", "ALPE13S", "ALPE13C", "GGTE17S",
   "GGTE17C", "BILIG01S", "BILIG01C", "BILIG02C", "BILIG02S", "BILDG03C", "BILDG03S", "FATAF44C", "FATAF44S", "GLUCF30C", "GLUCF30S", 
   "INSUP28C", "INSUP28S", "ELFSJI4J", "CYK1DE7S", "CYK1DE6S", 'P3NPS27S', 'HYALQ63S', 'TIMPJ13S') 
   and visitnum > 3 and visitnum le 8 and not missing(aval) then output locfparams;
   else output oth_locf_params;
run;

*--------------------------------;
*locf;
*--------------------------------;

proc sort data=locfparams out=locfparams2(drop=avisit);
   by usubjid paramcd visitnum;
run;

data locfparams3;
   set locfparams2;
   by usubjid paramcd visitnum;
   where 3 lt visitnum le 8;
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
   if  4 le visitnum2 lt 8 then do;
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



*--------------------------------;
*rttdlocf;
*--------------------------------;
data rttlocfparams oth_rttlocf_params;
   set lb05;
   if paramcd in  ("HBA1K48C", "HBA1K48S", "ALTE03S", "ALTE03C", "ASTE01S", "ASTE01C", "ALPE13S", "ALPE13C", "GGTE17S",
   "GGTE17C", "BILIG01S", "BILIG01C", "BILIG02C", "BILIG02S", "BILDG03C", "BILDG03S", "FATAF44C", "FATAF44S", "GLUCF30C", "GLUCF30S", 
   "INSUP28C", "INSUP28S", "ELFSJI4J", "CYK1DE7S", "CYK1DE6S", 'P3NPS27S', 'HYALQ63S', 'TIMPJ13S') 
 and . lt adt le effdt and visitnum > 3 and visitnum le 8 /*and int(visitnum) = visitnum */ and aval ne . then output rttlocfparams;
   else output oth_rttlocf_params;
run;

proc sort data=rttlocfparams out=rttlocfparams2(drop= avisit);
   by usubjid paramcd visitnum;
run;

data rttlocfparams3;
   set rttlocfparams2;
   by usubjid paramcd visitnum;
   where 3 lt visitnum le 8;
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

proc sort data=rttlocfparams;
   by usubjid paramcd;
run;

data rttlocfparams4;
   merge rttlocfparams(in=a) rttlocfparams3(in=b);
   by usubjid paramcd;
   if a ;
   visitnum2=floor(visitnum);
run;

data rttlocfparams5;
   set rttlocfparams4;
   length dtype $10;
   if  4 le visitnum2 lt 8 then do;
         avisitn=visitnum2;
         output;
         do visitnum1=visitnum2+1 to 8;
            index1=index(visitnum_c,strip(put(visitnum1,best.)));
            index2=index(visitnum_c2,strip(put(visitnum1,best.)));
            if index1 and index2 then leave;
            else do;
               dtype="RTTDLOCF";
               avisitn=visitnum1;
               output;
               if index2 gt 0 then leave;
            end;
         end;
   end;
run;


data rttlocfparams6_;
   set rttlocfparams5;
   length avisit $100;
   if dtype="RTTDLOCF";
   if avisitn in (4:8) then
   avisit="VISIT "||strip(put(avisitn,best.));
   *drop lbseq;
run;

proc sort data=rttlocfparams6_;
   by usubjid paramcd avisitn visitnum;
run;

data rttlocfparams6;
   set rttlocfparams6_;
   by usubjid paramcd avisitn visitnum;
   if last.avisitn;
run;

data locf;
   set rttlocfparams6 locfparams6;
   level=1;
   drop ittrfl anl03fl;
run;

data minmaxrecs;
   set visit97 (drop=ittrfl anl03fl)
       visit98 (drop=ittrfl anl03fl)
       visit99 (drop=ittrfl anl03fl)
       visit101 (drop=ittrfl anl03fl)
       visit102 (drop=ittrfl anl03fl)
       visit103
       visit104
       visit105
       visit106
       visit107 ;
   level=1;
   
run;

data lb06;
   set lb05 locf minmaxrecs;
run;

data lb06;
   set lb06;
   if aval_temp_flag=1 then aval=.;
run;

*============================================================================;
*Baseline based calcualtions;
*============================================================================;

proc sort data=lb06;
   by usubjid paramcd adt adtm;
run;

proc sort data=basedate;
   by usubjid paramcd ;
run;

data lb07_;
   merge lb06(in=a) basedate(in=b) cykavg(in=c keep=usubjid paramcd aval rename=(aval=basecyk));
   by usubjid paramcd ;
   if a;
   if c then do; base=basecyk;end;
   drop basecyk;
run;

data lb07_;
   set lb07_
       avisit110(drop=ittrfl)
       avisit111(drop=ittrfl anl03fl);
run;

data lb07;
   set lb07_(drop=avalcat2 avalca2n);
   length basecat7 basecat1 chgcat1 basecat3 basecat7 basecat9 avalcat5 avalcat4 avalcat6 basecat6 
      avalcat8 avalcat1 avalcat2 avalcat9 $20 crit1- crit5 $40;

      if paramcd in ('ALTE03S', 'ASTE01S', 'GGTE17S', 'ALPE13S', 'BILIG01S', 'BILDG03S', 'BILIG02S') 
         then do;
      if nmiss(anrlo,aval,anrhi)=0 then do;
         if . < AVAL < ANRLO then do; AVALCAT2 = "<1 LLN"; avalca2n=1; avalca2nt=1; end;
         if . lt ANRLO <= AVAL <= ANRHI then do; AVALCAT2 = "Normal"; avalca2n=2; avalca2nt=0; end;
         if . lt  ANRHI < AVAL <= 2*ANRHI  then do; AVALCAT2 = ">1 - 2 ULN"; avalca2n=3; avalca2nt=2; end;
         if . lt 2*ANRHI < AVAL <= 3*ANRHI  then do; AVALCAT2 = ">2 - 3 ULN"; avalca2n=4; avalca2nt=3; end;
         if . lt 3*ANRHI < AVAL <= 4*ANRHI  then do; AVALCAT2 = ">3 - 4 ULN"; avalca2n=5; avalca2nt=4; end;
         if . lt 4*ANRHI < AVAL <= 5*ANRHI  then do; AVALCAT2 = ">4 - 5 ULN"; avalca2n=6; avalca2nt=5; end;
      end;
   end;

   if . lt aval lt anrlo then anrind="LOW";
   else if . lt anrlo le aval le anrhi then anrind="NORMAL";
   else if aval gt anrhi gt . then anrind="HIGH";

   if PARAMCD = "ALPE13S" and PARCAT1 = "CHEMISTRY" and DTYPE = "MAXIMUM" then do;
   if . < AVAL <= ANRHI then do; AVALCAT1 = "<=1X ULN"; avalca1n=1; end;
   if . < ANRHI < AVAL <= 2*ANRHI then do; AVALCAT1 = ">1X - 2X ULN"; avalca1n=2; end;
   if . < base <= ANRHI then do; baseCAT1 = "<=1X ULN"; baseca1n=1; end;
   if . < ANRHI < base <= 2*ANRHI then do; baseCAT1 = ">1X - 2X ULN"; baseca1n=2; end;
   end;

   if paramcd in ("HBA1K48C") then do;
      if . < AVAL <= 6.5 then do; AVALCAT5 = "<=6.5%"; avalca5n=1; end;
      else if AVAL >6.5 then do; AVALCAT5 = ">6.5%"; avalca5n=2; end;
   end;

   if parcat2 ne "OTH" and avisitn ne 110 then do;
      if paramcd not in ('CYK1DE6S', 'CYK1DE7S') then do;
      if nmiss(adt,basedate)=0 and adt gt basedate then do;
         if nmiss(aval,base)=0 then chg=aval-base;
      end;
      end;
      else do;
         if prefl ne "Y" then do;
         if nmiss(aval,base)=0 then chg=aval-base;
         end;
      end;
   end;

   if PARAMCD in ('CYK1DE6S', 'CYK1DE7S') then do;
 
       if . lt base lt anrlo then do; bnrind="LOW"; bnrindn=1; end;
       else if . lt anrlo le base le anrhi then do; bnrind="NORMAL"; bnrindn=2; end;
       else if base gt anrhi then do; bnrind="HIGH"; bnrindn=3; end;

       
       if . lt aval lt anrlo then do; anrind="LOW"; anrindn=1; end;
       else if . lt anrlo le aval le anrhi then do; anrind="NORMAL"; anrindn=2; end;
       else if aval gt anrhi then do; anrind="HIGH"; anrindn=3; end;
   end;
      if not missing(chg) then do;
   If PARAMCD in ('CYK1DE6S', 'CYK1DE7S') then do;
     If CHG <= 0 then do; CHGCAT1 = "<=0U/L"; chgcat1n=1; end;
     else if 0 < CHG <= 50 then do; CHGCAT1 = ">0 to <=50U/L"; chgcat1n=2; end;
     else if 50 < CHG <= 100 then do; CHGCAT1 = ">50U/L to <=100U/L"; chgcat1n=3; end;
     else if 100 < CHG <= 150 then do; CHGCAT1 = ">100U/L to <=150U/L"; chgcat1n=4; end;
     else if CHG > 150 then do; CHGCAT1 = ">150U/L"; chgcat1n=5; end;
   end;
   end;
   
   if PARAMCD in ('ELFSJI4J', 'P3NPS27S', 'HYALQ63S', 'TIMPJ13S') and not missing(chg) then do;
     if . lt CHG < 0 then do; CHGCAT1 = "<0"; chgcat1n=6; end;
     else if 0 <= CHG < 1 then do; CHGCAT1 = ">=0 to <1"; chgcat1n=7; end;
     else if 1 <= CHG < 2 then do; CHGCAT1 = ">=1 to <2"; chgcat1n=8; end;
     else if CHG >= 2 then do; CHGCAT1 = ">=2"; chgcat1n=9; end;
   end;



   if paramcd in ('ALTE03S', 'ALTE03C', 'ASTE01S', 'ASTE01C') then do;
      If . lt BASE <= 1*ANRHI then do; BASECAT7 = "<= ULN"; baseca7n=1; end;
      else If BASE > 1*ANRHI then do; BASECAT7 = "> ULN"; baseca7n=2; end;
   end;

   if nmiss(aval,anrhi)=0 and anrhi ne 0 then r2anrhi=aval/anrhi;

   if paramcd in ('ALTE03S', 'ASTE01S', 'GGTE17S', 'ALPE13S', 'BILIG01S', 'BILDG03S', 'BILIG02S' )  then do;
      if . < BASE < ANRLO then do; BASECAT3 = "<1 LLN"; baseca3n=1; end;
      else if ANRLO <= BASE <= ANRHI then do; BASECAT3 = "Normal"; baseca3n=2; end;
      else if ANRHI < BASE <= 2*ANRHI  then do; BASECAT3 = ">1 - 2 ULN"; baseca3n=3; end;
      else if BASE > 2*ANRHI  then do; BASECAT3 = ">2 ULN"; baseca3n=4; end;
   end;

   if PARAMCD = "HBA1K48C" then do;
      if . < AVAL <7 then do; AVALCAT4 = "<7%"; avalca4n=1; end;
      else if AVAL >=7 then do; AVALCAT4 = ">=7%"; avalca4n=2; end;
   end;
   if nmiss(aval,anrhi)=0 then do;
   if PARAMCD in ('ALTE03S', 'ALTE03C') and AVAL > 1*ANRHI then do; CRIT1 = "ALT >1X ULN"; critfl="Y"; end;
   if PARAMCD in ('ASTE01S', 'ASTE01C') and AVAL > 1*ANRHI then do; CRIT1 = "AST >1X ULN"; critfl="Y"; end;
   if PARAMCD in ('ALPE13C', 'ALPE13S') and AVAL > 1.5*ANRHI then do; CRIT1 = "ALP >1.5X ULN"; critfl="Y"; end;
   if PARAMCD in ('BILIG01S', 'BILIG01C') and AVAL > 1*ANRHI then do; CRIT1 = "Total Bilirubin >1X ULN"; critfl="Y"; end;

   If PARAMCD in ('ALTE03S', 'ALTE03C') and AVAL > 2*ANRHI then do; CRIT2 = "ALT >2X ULN"; crit2fl="Y"; end;
   If PARAMCD in ('ASTE01S', 'ASTE01C') and AVAL > 2*ANRHI then do; CRIT2 = "AST >2X ULN"; crit2fl="Y"; end;
   If PARAMCD in ('BILIG01S', 'BILIG01C') and AVAL > 2*ANRHI then do; CRIT2 = "Total Bilirubin >2X ULN"; crit2fl="Y"; end;

   If PARAMCD in ('ALTE03S', 'ALTE03C') and AVAL >= 3*ANRHI then do; CRIT3 = "ALT >=3X ULN"; crit3fl="Y"; end;
   If PARAMCD in ('ASTE01S', 'ASTE01C') and AVAL >= 3*ANRHI then do; CRIT3 = "AST >=3X ULN"; crit3fl="Y"; end;
   If PARAMCD in ('ALPE13C', 'ALPE13S') and AVAL > 3*ANRHI then do; CRIT3 = "ALP >3X ULN"; crit3fl="Y"; end;
   If PARAMCD = 'BILIG01C' and AVAL >= 3 then do; CRIT3 = "Total Bilirubin >=3 mg/dL"; crit3fl="Y"; end;

   if PARAMCD in ('ALTE03S', 'ALTE03C') and AVAL >= 5*ANRHI then do; CRIT4 = "ALT >=5X ULN"; crit4fl="Y"; end;
   if PARAMCD in ('ASTE01S', 'ASTE01C') and AVAL >= 5*ANRHI then do; CRIT4 = "AST >=5X ULN"; crit4fl="Y"; end;

   if PARAMCD in ('ALTE03S', 'ALTE03C') and AVAL >= 8*ANRHI then do; CRIT5 = "ALT >=8X ULN"; crit5fl="Y"; end;
   if PARAMCD in ('ASTE01S', 'ASTE01C') and AVAL >= 8*ANRHI then do; CRIT5 = "AST >=8X ULN"; crit5fl="Y"; end;

   end;

   if paramcd in ('ALTE03S', 'ALTE03C', 'ASTE01S', 'ASTE01C') then do;
   if . lt AVAL < 2*ANRHI then do; AVALCAT8 = "<2X ULN"; avalca8n=1; end;
   if . lt  2*ANRHI <= AVAL < 3*ANRHI then do; AVALCAT8 = ">=2X ULN to <3X ULN"; avalca8n=2; end;
   if . lt 3*ANRHI <= AVAL < 5*ANRHI then do; AVALCAT8 = ">=3X ULN to <5X ULN"; avalca8n=3; end;
   if . lt 5*ANRHI <= AVAL < 8*ANRHI then do; AVALCAT8 = ">=5X ULN to <8X ULN"; avalca8n=4; end;
   if . lt AVAL >= 8*ANRHI then do; AVALCAT8 = ">=8X ULN";avalca8n=5; end;
   end;

   if paramcd in("ELFSJI4J", 'P3NPS27S', 'HYALQ63S', 'TIMPJ13S') then do;
      if . < AVAL < 7.7 then do; AVALCAT6 = "<7.7"; avalca6n=4; end;
      Else if 7.7 <=AVAL < 9.8 then do; AVALCAT6 = ">=7.7 to <9.8"; avalca6n=5; end;
      Else if 9.8 <=AVAL < 11.3 then do; AVALCAT6 = ">=9.8 to <11.3"; avalca6n=6; end;
      Else if AVAL >=11.3 then do; AVALCAT6 = ">=11.3"; avalca6n=7; end;

      if . < base < 7.7 then do; baseCAT6 = "<7.7"; baseca6n=4; end;
      Else if 7.7 <=base < 9.8 then do; baseCAT6 = ">=7.7 to <9.8"; baseca6n=5; end;
      Else if 9.8 <=base < 11.3 then do; baseCAT6 = ">=9.8 to <11.3"; baseca6n=6; end;
      Else if base >=11.3 then do; baseCAT6 = ">=11.3"; baseca6n=7; end;
   end;

   else if paramcd in  ('CYK1DE6S', 'CYK1DE7S') then do;
      if . < aval le 200 then do; avalCAT6 = "<=200U/L"; avalca6n=1; end;
      else if 200 < aval <= 350 then do; avalCAT6 = ">200U/L to <=350U/L"; avalca6n=2; end;
      else if aval >350 then do; avalCAT6 = ">350U/L"; avalca6n=3; end;

       if . < base le 200 then do; baseCAT6 = "<=200U/L"; baseca6n=1; end;
      else if 200 < base <= 350 then do; baseCAT6 = ">200U/L to <=350U/L"; baseca6n=2; end;
      else if base >350 then do; baseCAT6 = ">350U/L"; baseca6n=3; end;

   end;


   if PARAMCD in  ('CYK1DE6S', 'CYK1DE7S')  then do;
      if . < AVAL <= 200 then do; AVALCAT9 = "<=200U/L"; avalca9n=1; end;
      else if AVAL > 200 then do; AVALCAT9 = ">200U/L"; avalca9n=2; end;
   end;

   if PARAMCD in  ('CYK1DE6S', 'CYK1DE7S')  then do;
      if . < base <= 200 then do; baseCAT9 = "<=200U/L"; baseca9n=1; end;
      else if base > 200 then do; baseCAT9 = ">200U/L"; baseca9n=2; end;
   end;

   if PARAMCD in ("CYK1DE6S", "CYK1DE7S", "ELFSJI4J", 'P3NPS27S', 'HYALQ63S', 'TIMPJ13S') then do;
   if PREFL ne "Y" and not missing(CHGCAT1) and not missing(BASECAT6) then SHIFT3 = catx(" to ", BASECAT6, CHGCAT1);
   end; 

   if not missing(aval) and not missing(anrhi) then do;
   If PARAMCD in ('ALTE03S', 'ALTE03C') and AVAL > 1*ANRHI  then do; CRIT1 = "ALT >1X ULN"; crit1fl="Y"; end;
   If PARAMCD in ('ASTE01S', 'ASTE01C') and AVAL > 1*ANRHI  then do; CRIT1 = "AST >1X ULN"; crit1fl="Y"; end;
   If PARAMCD in ('ALPE13C', 'ALPE13S') and AVAL > 1.5*ANRHI then do; CRIT1 = "ALP >1.5X ULN"; crit1fl="Y"; end;
   If PARAMCD in ('BILIG01S', 'BILIG01C') and AVAL > 1*ANRHI then do; CRIT1 = "Total Bilirubin >1X ULN"; crit1fl="Y"; end;
   end;

   if paramcd in ("CYK1DE6S", "CYK1DE7S") and dtype="AVERAGE" then call missing(visit,visitnum,adt);
   *if parcat2="CN" and (avisitn in (1:90) or avisitn=999 ) then call missing(basetype);
   if paramcd in ("CYK1DE6S", "CYK1DE7S") and avisitn not in (110,111) then basetype="AVERAGE";
    if paramcd in ("CYK1DE6S", "CYK1DE7S", "ELFSJI4J", 'P3NPS27S', 'HYALQ63S', 'TIMPJ13S') then do;
      if PREFL ne 'Y' and not missing(AVALCAT6) and not missing(BASECAT6) then SHIFT2=catx(" to ", BASECAT6, AVALCAT6) ;
   end;
   if parcat2="CN" then paramtyp="DERIVED";
   if prefl ne "Y" and not missing(anrind) and not missing(bnrind) then SHIFT1=catx(" to ", BNRIND, ANRIND) ;
   trtpn=trt01pn;
   trtan=trt01an;
   trta=trt01a;
   trtp=trt01p;
   
   if paramcd="ALTAST2W" then call missing(basetype);
run;

data lb07m lb07nm;
   set lb07;
   if not missing(avisitn) and dtype not in ("LOCF" "RTTDLOCF") then output lb07nm;
   else output lb07m;
run;

proc sort data=lb07nm; 
   by usubjid paramcd avisitn descending avisit adt adtm ;
run;

data lb07m;
   set lb07m;
   if not missing(avisitn) then do;
         if dtype in ("LOCF" "RTTDLOCF") then anl01fl="Y";
   end;
run;

data lb07nm;
   set lb07nm;
   by usubjid paramcd avisitn descending avisit adt adtm ;
   if first.avisitn /*and avisitn ne .*/ then anl01fl="Y";
run;

data lb08;
   set lb07m lb07nm ;
   if visitnum le 3 and ablfl ne "Y" then anl01fl="";
run;


data lb08_1 lb08_2;
   set lb08;
   if shift1 in ("NORMAL to LOW" 'NORMAL to HIGH') then output lb08_1;
   else output lb08_2;
run;

proc sort data=lb08_1;
   by usubjid paramcd shift1 adtm avisitn;
run;

data lb08_1;
   set lb08_1;
   by usubjid paramcd shift1 adtm avisitn;
   if first.shift1 then anl02fl="Y";
run;

data lb09;
   set lb08_1 lb08_2;
   if avisit="VISIT 3 (BASELINE) " then avisit="BASELINE";

run;

proc sort data=lb09;
   by STUDYID USUBJID PARCAT1N visitnum AVISITN PARAMCD ANL01FL ANL03FL ADT dtype;
   where not missing(aval) or not missing(avalc);
run;

   

data lb10;
   retain STUDYID USUBJID LBSEQ AGRPID PARAMCD PARAM PARAMTYP PARCAT1 PARCAT1N PARCAT2 PARCAT2N ADT ATM ADTM ADY
   VISIT VISITNUM AVISIT AVISITN AVAL AVALC AVALCAT1 AVALCA1N ANRLO ANRHI ANRIND ANRINDN BASE BASEC BNRIND
   BNRINDN CHG CHGCAT1 CHGCAT1N SHIFT1 SHIFT2 DTYPE ABLFL ANL01FL ANL02FL BASETYPE PREFL SAFFL FASTRFL TRTP 
   TRTPN TRTA TRTAN ITTFL R2ANRHI AVALCAT2 AVALCA2N BASECAT3 BASECA3N AVALCAT4 AVALCA4N AVALCAT5 AVALCA5N ITTRFL
   CRIT1 CRIT1FL CRIT2 CRIT2FL CRIT3 CRIT3FL CRIT4 CRIT4FL CRIT5 CRIT5FL ANL03FL AVALCAT6 AVALCA6N BASECAT6 BASECA6N
   BASECAT7 BASECA7N AVALCAT8 AVALCA8N SHIFT3 AVALCAT9 AVALCA9N BASECAT9 BASECA9N;
   set lb09;
   keep STUDYID USUBJID LBSEQ AGRPID PARAMCD PARAM PARAMTYP PARCAT1 PARCAT1N PARCAT2 PARCAT2N ADT ATM ADTM ADY
   VISIT VISITNUM AVISIT AVISITN AVAL AVALC AVALCAT1 AVALCA1N ANRLO ANRHI ANRIND ANRINDN BASE BASEC BNRIND
   BNRINDN CHG CHGCAT1 CHGCAT1N SHIFT1 SHIFT2 DTYPE ABLFL ANL01FL ANL02FL BASETYPE PREFL SAFFL FASTRFL TRTP 
   TRTPN TRTA TRTAN ITTFL R2ANRHI AVALCAT2 AVALCA2N BASECAT3 BASECA3N AVALCAT4 AVALCA4N AVALCAT5 AVALCA5N ITTRFL
   CRIT1 CRIT1FL CRIT2 CRIT2FL CRIT3 CRIT3FL CRIT4 CRIT4FL CRIT5 CRIT5FL ANL03FL AVALCAT6 AVALCA6N BASECAT6 BASECA6N
   BASECAT7 BASECA7N AVALCAT8 AVALCA8N SHIFT3 AVALCAT9 AVALCA9N BASECAT9 BASECA9N;
run;

data comp.adlb;
   set lb10;
run;

%ut_saslogcheck;
