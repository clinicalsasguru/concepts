*---------------------------------------;
*subset events with total count ge 5;
*---------------------------------------;

%macro pvalue(trt=);
proc sort data=yesno_base out=yes_no_1&trt.;
    by aesoc aedecod label;
    where trtan in (1,&trt.);
run;

proc sort data=ge5;
    by aesoc aedecod label;
run;

data yes_no_1&trt.;
    merge yes_no_1&trt.(in=a) ge5(in=b);
    by aesoc aedecod label;
    if a and b;
run;

*-------------------------------------------------------;
*create fisher or chi-square flag;
*-------------------------------------------------------;

proc sort data=yes_no_1&trt. out=c_f_test_1&trt.(keep=aesoc aedecod label) nodupkey;
    by aesoc aedecod label;
    where count lt 5;
run;

data yes_no_1&trt.;
    merge yes_no_1&trt.(in=a) c_f_test_1&trt.(in=b);
    by aesoc aedecod label;
    if b then c_f_test="F";
    else c_f_test="C";
run;

proc sql noprint;
    select count(*) into :chi_exist from yes_no_1&trt. where c_f_test="C";
    select count(*) into :fisher_exist from yes_no_1&trt. where c_f_test="F";
quit;

%put observations satisfy chisquare condition &chi_exist;

%put observations satisfy fisher condition &fisher_exist;

%if &fisher_exist gt 0 %then %do;

*--------------------------------------------;
*levels check;
*--------------------------------------------;

data yes_no_1&trt._f_temp;
    set yes_no_1&trt.;
    where c_f_test="F";
run;

proc sort data=yes_no_1&trt._f_temp;
    by aesoc aedecod label event;
run;

data yes_no_1&trt._f_temp_x;
    set yes_no_1&trt._f_temp;
    by aesoc aedecod label event;
    where count=0;
    if trtan=1 and event=0 then event0_level1=1;
    if trtan=&trt. and event=0 then event0_level&trt=1;

    if trtan=1 and event=1 then event1_level1=1;
    if trtan=&trt. and event=1 then event1_level&trt=1;

    if last.event;

    if nmiss(event0_level1,event0_level&trt)=0 then event0=event0_level1 + event0_level&trt;
    if nmiss(event1_level1,event1_level&trt)=0 then event1=event1_level1 + event1_level&trt;

    if event0 ge 2 or event1 ge 2;
    keep aesoc aedecod label;
run;

data yes_no_1&trt._f_temp;
    merge yes_no_1&trt._f_temp(in=a) yes_no_1&trt._f_temp_x(in=b);
    if a;
    if b then leveltest="Fail";
    else leveltest="Pass";
run;

data yes_no_1&trt._f_temp;
    set yes_no_1&trt._f_temp;;
    if leveltest="Pass";
run;

proc freq data=yes_no_1&trt._f_temp ;
    by aesoc aedecod label;
    weight count;
    tables trtan*event/missing fisher;
    output out=fisher_1&trt(keep= aesoc aedecod label xp2_fish rename=(xp2_fish=pvalue&trt)) fisher ;
run;


proc sort data=final_counts;
    by aesoc aedecod label;
run;

data final_counts;
    merge final_counts(in=a) fisher_1&trt;
    by aesoc aedecod label;
    if a;
run;
%end;


%if &chi_exist gt 0 %then %do;

*--------------------------------------------;
*levels check;
*--------------------------------------------;

data yes_no_1&trt._c_temp;
    set yes_no_1&trt.;
    where c_f_test="C";
run;

proc sort data=yes_no_1&trt._c_temp;
    by aesoc aedecod label event;
run;

data yes_no_1&trt._c_temp_x;
    set yes_no_1&trt._c_temp;
    by aesoc aedecod label event;
    where count=0;
    if trtan=1 and event=0 then event0_level1=1;
    if trtan=&trt. and event=0 then event0_level&trt=1;

    if trtan=1 and event=1 then event1_level1=1;
    if trtan=&trt. and event=1 then event1_level&trt=1;

    if last.event;

    event0=event0_level1 + event0_level&trt;
    event1=event1_level1 + event1_level&trt;

    if event0 ge 2 or event1 ge 2;
    keep aesoc aedecod label;
run;

data yes_no_1&trt._c_temp;
    merge yes_no_1&trt._c_temp(in=a) yes_no_1&trt._c_temp_x(in=b);
    if a;
    if b then leveltest="Fail";
    else leveltest="Pass";
run;

data yes_no_1&trt._c_temp;
    set yes_no_1&trt._c_temp;;
    if leveltest="Pass";
run;

proc freq data=yes_no_1&trt._c_temp ;
    by aesoc aedecod label;
    weight count;
    tables trtan*event/missing chisq ;
    output out=chi_1&trt(keep= aesoc aedecod label p_pchi rename=(p_pchi=pvalue&trt)) chisq ;
run;

data final_counts;
    merge final_counts(in=a) chi_1&trt;
    by aesoc aedecod label;
    if a;
run;

%end;

%if &chi_exist =0 and &fisher_exist = 0 %then %do;
data final_counts;
    set final_counts;
    pvalue2=.;
    pvalue3=.;
run;
%end;


%mend;
