
%macro dtc_date9(datevar=);
   &datevar._old=&datevar.;
   &datevar._temp_date=compress(&datevar.,,'kd');
   &datevar._temp_day=substrn(&datevar._temp_date,7,2);
   &datevar._temp_month=substrn(&datevar._temp_date,5,2);
   &datevar._temp_year=substrn(&datevar._temp_date,1,4);
   if missing(&datevar._temp_day) then &datevar._temp_day="--";
   if missing(&datevar._temp_month) then &datevar._temp_month="---";
   if missing(&datevar._temp_year) then &datevar._temp_year="----";
   if &datevar._temp_month="01" then &datevar._temp_month="JAN";
   else if &datevar._temp_month="02" then &datevar._temp_month="FEB";
   else if &datevar._temp_month="03" then &datevar._temp_month="MAR";
   else if &datevar._temp_month="04" then &datevar._temp_month="APR";
   else if &datevar._temp_month="05" then &datevar._temp_month="MAY";
   else if &datevar._temp_month="06" then &datevar._temp_month="JUN";
   else if &datevar._temp_month="07" then &datevar._temp_month="JUL";
   else if &datevar._temp_month="08" then &datevar._temp_month="AUG";
   else if &datevar._temp_month="09" then &datevar._temp_month="SEP";
   else if &datevar._temp_month="10" then &datevar._temp_month="OCT";
   else if &datevar._temp_month="11" then &datevar._temp_month="NOV";
   else if &datevar._temp_month="12" then &datevar._temp_month="DEC";

   &datevar.=cats(&datevar._temp_day,&datevar._temp_month,&datevar._temp_year);
   drop &datevar._temp_:;

   %mend;

data temp;
   set s_sdtm_i.mh;
   %dtc_date9(datevar=mhstdtc);
   %dtc_date9(datevar=mhendtc);

run;

proc sort dat=temp;
   by usubjid mhstdtc_old mhstdtc;
run;




