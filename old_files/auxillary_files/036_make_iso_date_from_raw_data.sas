dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

libname raw "D:\Home\dev\compound6\data\shared\raw";

data dm;
	set raw.dm_1;
run;
/*
December 15, 2003 13:14:17
Complete date/time
2003-12-15T13:14:17
2
December 15, 2003 13:14
Unknown seconds
2003-12-15T13:14
3
December 15, 2003 13
Unknown minutes and seconds
2003-12-15T13
4
December 15, 2003
Unknown time
2003-12-15
5
December, 2003
Unknown day and time
2003-12
6
2003
Unknown month, day, and time
2003
*/
*---------------------------------------------------------------*;
* make_dtc_date.sas is a SAS macro that creates a SDTM --DTC date
* within a SAS datastep when provided the pieces of the date in 
* separate SAS variables.
* MACRO PARAMETERsS:
* dtcdate = SDTM --DTC date variable desired
* year = year variable
* month = month variable 
* day = day variable
* hour = hour variable
* minute = minute variable 
* second = second variable
*---------------------------------------------------------------*; 
%macro make_dtc_date(dtcdate=, year=., month=., day=., 
                     hour=., minute=., second=.); 

    ** in a series of if-then-else statements, determine where the
    ** smallest unit of date and time is present and then construct a DTC
    ** date based on the non-missing date variables.;

    if (&second ne .) then 
        &dtcdate = put(&year,z4.) || "-" || put(&month,z2.) || "-" 
                           || put(&day,z2.) || "T" || put(&hour,z2.) || ":" 
                           || put(&minute,z2.) || ":" || put(&second,z2.); 
    else if (&minute ne .) then 
        &dtcdate = put(&year,z4.) || "-" || put(&month,z2.) || "-" 
                           || put(&day,z2.) || "T" || put(&hour,z2.) || ":" 
                           || put(&minute,z2.); 
    else if (&hour ne .) then 
        &dtcdate = put(&year,z4.) || "-" || put(&month,z2.) || "-" 
                           || put(&day,z2.) || "T" || put(&hour,z2.); 
    else if (&day ne .) then 
        &dtcdate = put(&year,z4.) || "-" || put(&month,z2.) || "-" 
                           || put(&day,z2.); 
    else if (&month ne .) then 
        &dtcdate = put(&year,z4.) || "-" || put(&month,z2.); 
    else if (&year ne .) then 
        &dtcdate = put(&year,z4.); 
    else if (&year = .) then 
        &dtcdate = ""; 

    ** remove duplicate blanks and replace space with a dash;
    if &dtcdate ne "" then
        &dtcdate = translate(trim(compbl(&dtcdate)),'-',' ');

%mend make_dtc_date;


data dm;
	set dm;
	%make_dtc_date(dtcdate=brthdtc,year=brthdt_y, month=brthdt_m, day=brthdt_d);
run;

