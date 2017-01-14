dm 'log; clear; lst; clear; log; preview;';

data ho;
input usubjid hoseq hostdt hoendt;
format hostdt hoendt date9.;
trtan=1;
cards;
001 1 20454 20454
001 2 20455 20456
001 3 20457 20458
001 4 20458 20459
001 5 20460 20460
001 6 20460 20461
002 1 20454 .
;
run;

*----------------------------------------------------------------------------------
"The number of days of hospitalization is calculated as  (Date of discharge – Date of Admission + 1).
if admission of hospitalization is not the same as date of discharge of previous record or if admission
is the same as discharge of the same  then (AENDT - ASTDT + 1) ,  
 if admission of hospitalization is the same as date of discharge of previous record then (AENDT - ASTDT )
and is counted as the same hospitalization as the previous one.

In case of missing dates, the hospitalization duration will be imputed by the
average duration per stay derived from the subjects with non-missing duration 
within the same treatment group (Actual treatment group will be used).
*-----------------------------------------------------------------------------------;

proc sort data=ho;
   by usubjid hostdt hoendt;
run;

*----------------------------------------------------------------;
*get the previous discharge date onto the current record;
*----------------------------------------------------------------;

data ho01;
   set ho;
   by usubjid hostdt hoendt;
   if missing(hoendt) then missing_enddate_flag=1;
   retain previous_discharge;
   previous_discharge=lag(hoendt);
   if first.usubjid then call missing(previous_discharge);

   format previous_discharge date9.;
run;

*------------------------------------------------------------------;
*calculate durations;
*------------------------------------------------------------------;
data ho02;
   set ho01;
   by usubjid hostdt hoendt;
   if (first.usubjid) or (hostdt=hoendt) or (hostdt ne previous_discharge ne .) then do;
      if nmiss(hostdt,hoendt)=0 then hodur=hoendt-hostdt+1;
   end;
   else if nmiss(hostdt,previous_discharge)=0 and (hostdt=previous_discharge) then do;
      hodur=hoendt-hostdt;
   end;
   
   if first.usubjid then hosp_counter=1;
   else do;
      if not(hostdt=previous_discharge) then hosp_counter+1;
   end;
run;

*----------------------------------------------------------------------;
*get the average duration for missing dated hospitalizations;
*---------------------------------------------------------------------;

proc sql;
   create table average_hospdur as
      select trtan,round(avg(hodur),0.1) as average_hospdur
      from ho02
      group by trtan;
quit;

proc sort data=ho02;
   by trtan;
run;

data ho03;
   merge ho02 average_hospdur;
   by trtan;
   if missing_enddate_flag=1 and missing(hodur) then hodur=average_hospdur;
run;



%ut_saslogcheck;
*---------------------------------------------------------------------------------------------------------------------;
*sandhya's approach;
*---------------------------------------------------------------------------------------------------------------------;

data ho1;
	set ho;
	by usubjid;
    endt=lag(hoendt);
	if first.usubjid then endt=hoendt;
	format endt date9.;
	if ~missing(hoendt) and ~missing(hostdt) then do;
	if (hostdt ne endt) or (hostdt= hoendt) then dur=hoendt-hostdt+1;
	else if hostdt = endt then dur=hoendt-hostdt;
	end;
run;


data ho2(keep=usubjid trtan hostdt hoendt dur) tot(keep=trtan imp_dur);
	set ho1;
	by trtan usubjid;
	retain tot_dur tot_sub;
	if first.trtan then do tot_dur=.;
						tot_sub=1;
							
	end;
	if First.usubjid then subno=1;
	if ~missing(dur) then do;
	if first.usubjid and first.trtan then tot_sub=subno;
		else tot_sub+subno;

	if ~missing(dur) then  tot_dur+dur;

	end;
	output ho2;	
	if last.trtan then  do imp_dur=tot_dur/tot_sub;
	output tot;
	end;
run;

data final;
	merge ho2(in=a) tot(in=b);
	by trtan ;
	if a;
	if missing(dur) then dur=imp_dur;
	drop imp_dur;
run;
