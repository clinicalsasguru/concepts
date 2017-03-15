*==================================================================================================;
*little house keeping;
*==================================================================================================;
dm log 'clear';
dm lst 'clear';

proc datasets library=work mt=data kill;
quit;

%macro closevts  /* The cmd option makes the macro available to dms */ / cmd; 
  %local i; 
  %do i=1 %to 20;
    next "viewtable:"; end; 
  %end; 
%mend;

dm "keydef F12 '%NRSTR(%closevts);'";
options nomprint nosymbolgen nomlogic noxsync noxwait xmin;

proc printto print="&root.production\macros\tracker_to_checks.lst"
             log="&root.production\macros\tracker_to_checks.log" new;
run;     
 
libname track "&root.temporary\anil";

*================================================================================================;
*defining some global macro variables to control the process;
*================================================================================================;

%let _x_x_reread_tracker            =*;
%let _x_x_restrict_logcheck         =N;
%let _x_x_restrict_lstcheck         =N;
%let _x_x_restrict_inputdatacheck   =N;
%let _x_x_restrict_datecheck        =N;
%let _x_x_restrict_type             =tables listings figures;
%let _x_x_user                      =&sysuserid.;


*-----------------------------------------------;
*get the current run time;
*-----------------------------------------------;

data _null_;
    time=time();
    date=date();
    datetime = compress(put(date,yymmdd10.)||"T"||put(time,tod5.));
    datetime2=compress(datetime,':');
    call symputx('_x_x_run_time',datetime);
    call symputx('_x_x_run_time2',datetime2);

run;

*------------------------------------------------------------------;
*create macro variables for each output type restriction;
*------------------------------------------------------------------;

%let _x_x_type_count=%sysfunc(countw(&_x_x_restrict_type));

%macro restrict_types;

%do _xxi=1 %to &_x_x_type_count;

    %global _x_x_restrict_type&_xxi.; *assign as global first to make use of these outside this macro;

    %let _x_x_restrict_type&_xxi.=%scan(&_x_x_restrict_type,&_xxi,%str( ));

%end;

%mend restrict_types;

%restrict_types;



*-----------------------------------------------------------------;
*macro to create utility folders - as required;
*-----------------------------------------------------------------;

%macro newfolder(newfld);
%local rc fileref;
%let rc=%sysfunc(filename(fileref,&newfld));
%if %sysfunc(fexist(&fileref)) %then %put NOTE:The directory "&newfld" already EXISTED.;
%else %do;
         %sysexec md "&newfld";
         %put NOTE:The directory "&newfld" has been CREATED.;
      %end;
%let rc=%sysfunc(filename(fileref));
%mend newfolder;

%newfolder(C:\Users\&_x_x_user.\Desktop\_x_x_dump);* for temporary files;
%newfolder(&root.documentation\tracking\study_status);





*================================================================================================;
*initial processing for obtaining tracker related information;
*================================================================================================;

libname track "&root.\temporary\anil";

data t01;  
    set track.tracker;
run;

*===================================================================================================;
*section for creating hyperlinks;
*===================================================================================================;

data t02;
    set t01(rename=(prod_track_date=prod_track qc_track_date=qc_track));
    length  prod_program 
            prod_lst prod_log qc_program qc_lst qc_log rtf
            fprod_prog fqc_prog fprod_lst fqc_lst fprod_log fqc_log frtf_report
            prod_path qc_path output_path
            $200 prod_track_date qc_track_date $20;

    *define locations of the files;

    prod_path="&root.production\"||strip(output_type)||"s\";
    qc_path="&root.qc\"||strip(output_type)||"s\";
    output_path="&root.output\production_out\";

    *define different file types;

    prod_program=strip(output_file_name)||".sas";
    prod_lst=strip(output_file_name)||".lst";
    prod_log=strip(output_file_name)||".log";
    qc_program=strip(output_file_name)||"_qc.sas";
    qc_lst=strip(output_file_name)||"_qc.lst";
    qc_log=strip(output_file_name)||"_qc.log";
    rtf=strip(output_file_name)||".rtf";

    *create full paths for all the files;

    fprod_prog=cats(prod_path,prod_program);
    fqc_prog=cats(qc_path,qc_program);
    fprod_lst=cats(prod_path,prod_lst);
    fqc_lst=cats(qc_path,qc_lst);
    fprod_log=cats(prod_path,prod_log);
    fqc_log=cats(qc_path,qc_log);
    frtf_report=cats(output_path,rtf);

    if not missing(qc_track) then qc_track_date=put(qc_track,yymmdd10.);
    if not missing(prod_track) then prod_track_date=put(prod_track,yymmdd10.);

    *create hyperlink variables when the file is present;
    *using arrays as the same logic is required for different file types;

    array files[*]      fprod_prog      fqc_prog      fprod_lst       fqc_lst      fprod_log      fqc_log      frtf_report;
    array hyper[*] $200 hyper_prod_prog hyper_qc_prog hyper_prod_lst  hyper_qc_lst hyper_prod_log hyper_qc_log hyper_rtf_report;
    array short[*]      prod_program    qc_program    prod_lst        qc_lst       prod_log       qc_log       rtf; 
    
    do i=1 to dim(files);
        if fileexist(files[i]) then do;
        hyper[i]='=hyperlink("'||strip(files[i])||'","'||strip(short[i])||'")';
        end;
        else do;
        hyper[i]="Not-found";
        end;
    end;
    qclst=qc_lst;

    array lcase[*] fprod_prog fqc_prog fprod_log fqc_log fprod_lst fqc_lst frtf_report;

    do j=1 to dim(lcase);
        lcase(j)=lowcase(lcase(j));
    end;

    *drop i j prod_: qc_: rtf output_path;
run;

*===================================================================================================;
*log checks section;
*===================================================================================================;

%inc "&root\Utility\Macros\Quintiles\QCHECK\toinclude\*.sas";
%*------------------------------------------------------------------------------------------;
%*macro has three levels;
%*level 1 checks if log check has been restricted with the global parameter;
%*level 2 is for the number of types of outputs requested - iterates for each type- t/f/l;
%*level 3 is production and qc checks - iterates twice-once for production and once for qc;
%*all the information is collated into a single dataset - one dataset for summary and 
  second for findings in each output;
%*------------------------------------------------------------------------------------------;

%macro prodqclogcheck(prodqc=,type=,prefix=);

%if &_x_x_restrict_logcheck. ne Y %then %do;*process only if log check is not restricted;

    %do _xxj=1 %to &_x_x_type_count;*iterate for each output type;

        %let _xxj_type=&&_x_x_restrict_type&_xxj.;*identify the type;
            
            %do _xxk=1 %to 2;*iterate twice in a type -once for production and once for qc;
                
                %if &_xxk=1 %then %let _xxk_location=production;
                %else %if &_xxk=2 %then %let _xxk_location=qc;

                %LogCheck (Dir=%STR(&root.&_xxk_location.\&_xxj_type.), ynzeroobs=n);

                %*----------------------------------------------------------------------------------------;
                %if %sysfunc(exist(Logcheck_summary)) %then %do;*for summary collation;
                    
                    data logcheck_summary;
                        set logcheck_summary 
                            _summary
                            ;
                        filename=lowcase(filename);
                    run;

                %end;
                %else %do;

                    data logcheck_summary;
                        set _summary;
                        filename=lowcase(filename);
                    run;

                %end;
                %*----------------------------------------------------------------------------------------;

                %*----------------------------------------------------------------------------------------;

                %if %sysfunc(exist(Logcheck_findings)) %then %do;*for findings collation;
                    
                    data logcheck_findings;
                        set logcheck_findings 
                            _findings
                            ;
                        filename=lowcase(filename);
                    run;

                %end;
                %else %do;

                    data logcheck_findings;
                        set _findings;
                        filename=lowcase(filename);
                    run;

                %end;
                %*----------------------------------------------------------------------------------------;
                
                proc datasets library=work mtype=data;*deleting the temporary datasets between each iteration;
                  delete _summary _findings;
                run;
                quit;
            %end;
    %end;
%end;



%mend prodqclogcheck;

*------------------------------------------------------------------------------------;
*supperss log from logcheck macro: this inherently creates some warnings and notes;
*------------------------------------------------------------------------------------;

filename junk dummy;
proc printto log=junk;
run;

%prodqclogcheck


proc printto log="&root.production\macros\tracker_to_checks.log";
run;
filename junk;

*======================================================================================================;
*lst files check for compare issues;
*======================================================================================================;


%macro qclstcheck;
data comp_clearbase;
    length program_name output_name $50 text $256 filename $200;
    call missing(program_name,output_name,text,nobs_R, nobs_M, nobs_c,nvars_mismatch,occurrence,flag,filename);
run;

data comp_notclearbase;
    length program_name output_name $50 text $256 filename $200;
    call missing(program_name,output_name,text,nobs_R, nobs_M, nobs_c,nvars_mismatch,occurrence,flag,filename);
run;


%if &_x_x_restrict_lstcheck. ne Y %then %do;*process only if lst check is not restricted;

    %do _xxj=1 %to &_x_x_type_count;*iterate for each output type;

        %let _xxj_type=&&_x_x_restrict_type&_xxj.;*identify the type;
               
            %let _x_x_sys_files_loc=%STR(&root.qc\&_xxj_type.);

            
            data &_xxj_type.lstpresent &_xxj_type.lstabsent;
                set t02;
                if fileexist(cats("&_x_x_sys_files_loc.\",output_file_name,"_qc.lst")) =1 
                    then do;
                    output &_xxj_type.lstpresent;
                end;
                else do;
                    output &_xxj_type.lstabsent;
                end;
            run;


            data _null_;
             set &_xxj_type.lstpresent;
             call execute('%lst_file_check('||strip(scan(qclst,1,'.'))||','||strip(output_file_name)||',N);');
            run;
            
    %end;
%end;


%mend qclstcheck;

%qclstcheck;

*=======================================================================================================;
*section to read input datasets used by different programs;
*=======================================================================================================;


data input_datasets;
    length filename input_datasets $200;
    call missing(filename,input_datasets);
run;
*-----------------------------------------------------------------------------------;
*macro for reading the input datasets used by a program;
*-----------------------------------------------------------------------------------;


%macro scaninputdataset(program);
data &_xxj_type._&_xxk_location._temp_input;
	infile "&program" truncover lrecl=256 end=last;
	length text req_line $256 program char $50 filename $200;
	input text 1-256;
	program="&program.";
    if index(compress(_infile_),"DerivedDataSetsUsed:") then delete;
	find1=index(upcase(_infile_),"SDTM.");
	find2=index(upcase(_infile_),"ADAM.");
	find3=index(upcase(_infile_),"DERIVED.") ;
	find4=index(upcase(_infile_),"RAW.");
	if find1 gt 0 then do;
		start=findc(_infile_,'(; ',find1+length('SDTM.'));
        if start=0 then start=length(_infile_)+1;
        char=substrn(upcase(_infile_),find1,start-find1);
		req_line=compress(_infile_);
	end;
    if find2 gt 0 then do;
        start=findc(_infile_,'(; ','s',find2+length('ADAM.'));
        if start=0 then start=length(_infile_)+1;
        startx=anycntrl(_infile_,find2+length('ADAM.'));
        char=substrn(upcase(_infile_),find2,start-find2);
		req_line=compress(_infile_);
	end;

	if find3 gt 0 then do;
		start=findc(_infile_,'(; ',find3+length('DERIVED.'));
        if start=0 then start=length(_infile_)+1;
        char=substrn(upcase(_infile_),find3,start-find3);
		req_line=compress(_infile_);
	end;

	if find4 gt 0 then do;
		start=findc(_infile_,'(; ',find4+length('RAW.'));
        if start=0 then start=length(_infile_)+1;
        char=substrn(upcase(_infile_),find4,start-find4);
		req_line=compress(_infile_);
    end;

if find1 gt 0 or find2 gt 0 or find3 gt 0 or find4 gt 0;

    filename=lowcase("&program");
if filename ne "" and char ne "";

keep filename char;
run;

proc sort data=&_xxj_type._&_xxk_location._temp_input;
    by filename;
run;

%*step to create one record per program with all the datasets separated by comma;

data &_xxj_type._&_xxk_location._temp_input;
    set &_xxj_type._&_xxk_location._temp_input;
    by filename;
    length input_datasets $200;
    retain input_datasets;
    if first.filename then do;
        call missing(input_datasets);
        input_datasets=strip(char);
    end;
    else do;
        if index(input_datasets,char)=0 then input_datasets=strip(input_datasets)||","||strip(char);
    end;

    if last.filename;
    drop char;
run;

proc append base=input_datasets data=&_xxj_type._&_xxk_location._temp_input;
run;


proc datasets library=work mtype=data;*deleting the temporary datasets between each iteration;
  delete &_xxj_type._&_xxk_location._temp_input;
run;
quit;
%mend scaninputdataset;

%macro getinputdata;

%if &_x_x_restrict_inputdatacheck. ne Y %then %do;*process only if lst check is not restricted;

    %do _xxj=1 %to &_x_x_type_count;%*iterate for each output type;

            %let _xxj_type=&&_x_x_restrict_type&_xxj.;%*identify the type;

            %do _xxk=1 %to 2;%*iterate twice in a type -once for production and once for qc;
                
                %if &_xxk=1 %then %do;
                        %let _xxk_location=production;
                        %let _xxk_extension=;
                %end;
                %else %if &_xxk=2 %then %do;
                        %let _xxk_location=qc;
                        %let _xxk_extension=_qc;%*qc programs will need an extension of _qc;
                %end;

                   
                %let _x_x_sys_files_loc=%STR(&root.&_xxk_location.\&_xxj_type.);

                
                data &_xxj_type.&_xxk_location.saspresent &_xxj_type.&_xxk_location.sasabsent;
                    set t02;
                    length filename $200;
                    filename=cats("&_x_x_sys_files_loc.\",output_file_name,"&_xxk_extension..sas");

                    if fileexist(cats("&_x_x_sys_files_loc.\",output_file_name,"&_xxk_extension..sas")) =1 
                        then do;
                        output &_xxj_type.&_xxk_location.saspresent;
                    end;
                    else do;
                        output &_xxj_type.&_xxk_location.sasabsent;
                    end;
                run;


                data _null_;
                 set &_xxj_type.&_xxk_location.saspresent;
                 call execute('%scaninputdataset('||strip(filename)||');');
                run;
              %end;
            
    %end;

/*    proc sort data=input_datasets nodupkey;*/
/*        by filename;*/
/*        where not missing(filename) and not missing(input_datasets);*/
/*    run;*/
%end;

%mend getinputdata;


%getinputdata;


*=======================================================================================================;
*section to read the time stamps of multiple files;
*=======================================================================================================;

*-----------------------------------------------------------------------------------;
*macro to get the file modified dates (of all files in a folder) into a dataset;
*-----------------------------------------------------------------------------------;


%macro modified_dates(prodqc=,filetype=);
data &prodqc._&filetype._files(rename=(program=program_name main_prg_date=modified_date));
	infile "C:\Users\&_x_x_user.\Desktop\_x_x_dump\&prodqc._contents_&filetype..txt" truncover;
	length program_name $50 test $10 program main_prg_date $50 ;
	input test$ 1-10 @;
	if not missing(input(test, ?? ddmmyy10.)) then do;
	input @1 main_prg_mod_date ddmmyy10.  main_prg_mod_time & time10. file_size : comma32. program_name;
	end;
	if upcase(scan(program_name,2,".")) in ("LOG" "LST" "SAS" "RTF") ;
	format  main_prg_mod_date date9.  main_prg_mod_time time8.;
	program=program_name;
	main_prg_date=put(main_prg_mod_date,yymmdd10.)||"T"||put(main_prg_mod_time,tod8.);

	keep program main_prg_date;
run;


%mend modified_dates;

*---------------------------------------------------------------------------------------------;
*macro to get the modified dates of contents of all requested folders;
*---------------------------------------------------------------------------------------------;

data modified_dates;
    length program_name modified_date $50 filename $200;
    call missing(program_name,filename,modified_date);
run;

 
%macro getmoddate;


%if &_x_x_restrict_datecheck. ne Y %then %do;*process only if log check is not restricted;

    %do _xxj=1 %to &_x_x_type_count;*iterate for each output type;

        %let _xxj_type=&&_x_x_restrict_type&_xxj.;*identify the type;
            
            %do _xxk=1 %to 2;*iterate twice in a type -once for production and once for qc;
                
                %if &_xxk=1 %then %let _xxk_location=production;
                %else %if &_xxk=2 %then %let _xxk_location=qc;

                %let _x_x_moddate_location=%str(.\&_xxk_location.\&_xxj_type.);
                x CHDIR /d  &_x_x_moddate_location. & 
                dir /T:W  /A:-D  > C:\Users\&_x_x_user.\Desktop\_x_x_dump\&_xxk_location._contents_&_xxj_type..txt;
                %modified_dates(prodqc=&_xxk_location.,filetype=&_xxj_type.);
              
                data &_xxk_location._&_xxj_type._files;
                    set &_xxk_location._&_xxj_type._files;
                    length filename $200;
                    filename="&root.&_xxk_location.\&_xxj_type.\"||strip(program_name);
                    filename=lowcase(filename);
                run;

                proc append base=modified_dates data=&_xxk_location._&_xxj_type._files;
                run;
                proc datasets library=work mtype=data;*deleting the temporary datasets between each iteration;
                  delete &_xxk_location._&_xxj_type._files;
                run;
                quit;
            %end;
    %end;
    *---------------------------------------------------------------------------------------;
    *get rtf modified date for each output present;
    *---------------------------------------------------------------------------------------;
    %let _x_x_moddate_location=%str(.\output\production_out);
    x CHDIR /d  &_x_x_moddate_location. & 
                    dir /T:W  /A:-D  > C:\Users\&_x_x_user.\Desktop\_x_x_dump\production_contents_RTF.txt;
    %modified_dates(prodqc=production,filetype=RTF);

    data production_RTF_files;
        set production_RTF_files;
        length filename $200;
        filename="&root.output\production_out\"||strip(program_name);
        filename=lowcase(filename);
    run;

    proc append base=modified_dates data=production_RTF_files;
    run;

%end;



%mend getmoddate;

%getmoddate;




*=======================================================================================================;
*processing - to get the information into a single file;
*=======================================================================================================;

proc sort data=modified_dates;
    by filename;
run;

proc sort data=input_datasets;
    by filename;
run;

proc sort data=comp_clearbase;
    by filename;
run;

proc sort data=comp_notclearbase;
    by filename;
run;

proc sort data=logcheck_summary;
    by filename;
run;

proc sort data=logcheck_findings;
    by filename;
run;

*--------------------------------------------------------------------------;
*processing for programs;
*--------------------------------------------------------------------------;

data prog_01;
    merge modified_dates input_datasets;
    by filename;
    if index(filename,".sas") gt 0;
run;

data prod_prog_01(rename=(filename=fprod_prog input_datasets=prod_datasets modified_date=prod_prog_date))
     qc_prog_01(rename=(filename=fqc_prog input_datasets=qc_datasets modified_date=qc_prog_date));
    set prog_01;
    if index(filename,"\production\") then output prod_prog_01;
    if index(filename,"\qc\") then output qc_prog_01;
    keep filename input_datasets modified_date;
run;

*--------------------------------------------------------------------------;
*processing for log files;
*--------------------------------------------------------------------------;

proc sort data=logcheck_summary out=logcheck_summary02 nodupkey;
    by filename;
run;

data log_01;
    merge modified_dates logcheck_summary02(keep=filename issue);
    by filename;
    if index(filename,".log") gt 0;
run;


data prod_log_01(rename=(filename=fprod_log modified_date=prod_log_date issue=prod_log_issue))
     qc_log_01(rename=(filename=fqc_log modified_date=qc_log_date issue=qc_log_issue));
    set log_01;
    if index(filename,"\production\") then output prod_log_01;
    if index(filename,"\qc\") then output qc_log_01;
    keep filename modified_date issue;
run;


*--------------------------------------------------------------------------;
*processing for lst files;
*--------------------------------------------------------------------------;

data lst_01;
    merge modified_dates comp_clearbase(keep=filename in=b) comp_notclearbase(keep=filename in=c);
    by filename;
    if index(filename,".lst") gt 0;
    if not b then issue=1;
run;


data prod_lst_01(rename=(filename=fprod_lst modified_date=prod_lst_date))
     qc_lst_01(rename=(filename=fqc_lst modified_date=qc_lst_date issue=qc_lst_issue));
    set lst_01;
    if index(filename,"\production\") then output prod_lst_01;
    if index(filename,"\qc\") then output qc_lst_01;
    keep filename modified_date issue;
run;

*---------------------------------------------------------------------------------------;
*processing for rtf files;
*---------------------------------------------------------------------------------------;

data rtf_files;
    set production_rtf_files;
    keep frtf_report rtf_date;
    rtf_date=modified_date;
    frtf_report=filename;
run;

*----------------------------------------------------------------------------------;
*bring together all dataset and merge one by one;
*----------------------------------------------------------------------------------;
data t03;
    set t02;
run;

%macro merging(master=t03,transact=,byvar=);
proc sort data=&master;
    by &byvar.;
run;

proc sort data=&transact;
    by &byvar.;
run;

data &master;
    merge &master(in=a) &transact;
    by &byvar.;
    if a;
run;

%mend merging;

%merging(master=t03,transact=prod_prog_01,byvar=fprod_prog);
%merging(master=t03,transact=qc_prog_01,byvar=fqc_prog);

%merging(master=t03,transact=prod_log_01,byvar=fprod_log);
%merging(master=t03,transact=qc_log_01,byvar=fqc_log);

%merging(master=t03,transact=prod_lst_01,byvar=fprod_lst);
%merging(master=t03,transact=qc_lst_01,byvar=fqc_lst);

%merging(master=t03,transact=rtf_files,byvar=frtf_report);


*==================================================================================;
*creating a separate dataset for each check;
*==================================================================================;

data logcheck_findings;
    set logcheck_findings;
    output_file_name=scan(filename,-1,'\');
run;

*---------------------------------------------------------------------------;
*missing files;
*---------------------------------------------------------------------------;
*--------------------------------------------------------;
*prod log files missing;
*--------------------------------------------------------;

data prod_log_missing;
    set t03;
    where hyper_prod_prog ne "Not-found" and missing(prod_log_date);
run;

*--------------------------------------------------------;
*qc log files missing;
*--------------------------------------------------------;

data qc_log_missing;
    set t03;
    where hyper_qc_prog ne "Not-found" and missing(qc_log_date);
run;

*--------------------------------------------------------;
*qc lst files missing;
*--------------------------------------------------------;

data qc_lst_missing;
    set t03;
    where hyper_qc_prog ne "Not-found" and missing(qc_lst_date);
run;

*-------------------------------------------------------;
*prod programs not-found;
*-------------------------------------------------------;

data prod_prog_missing;
    set t03;
    where hyper_prod_prog = "Not-found";
run;

*-------------------------------------------------------;
*qc programs not-found;
*-------------------------------------------------------;

data qc_prog_missing;
    set t03;
    where hyper_qc_prog = "Not-found";
run;

*------------------------------------------------------------------------------------;
*log checks;
*------------------------------------------------------------------------------------;

*-------------------------------;
*prod log issue;
*-------------------------------;

data prod_log_issue;
    set t03;
    where prod_log_issue=1;
    keep fprod_prog output_file_name hyper_prod_prog hyper_prod_log;
run;

*-------------------------------;
*qc log issue;
*-------------------------------;

data qc_log_issue;
    set t03;
    where qc_log_issue=1;
    keep fqc_prog output_file_name hyper_qc_prog hyper_qc_log;
run;

*------------------------------------------------------------------------------------;
*clean compare;
*------------------------------------------------------------------------------------;

*------------------------;
*not clear compare;
*------------------------;

data notclear_compare;
    set t03;
    where qc_lst_date ne "" and qc_lst_issue = 1;
    *keep output_file_name hyper_qc_lst;
run;

*-----------------------------;
*clear compare;
*-----------------------------;

data clear_compare;
    set t03;
    where qc_lst_date ne "" and qc_lst_issue ne 1;
    *keep output_file_name hyper_qc_lst hyper_rtf_report;
run;

*----------------------------------------------------------------------------;
*run time issues;
*----------------------------------------------------------------------------;

*----------------------------------;
*qc not run after production;
*----------------------------------;

data qcltprod;
    set t03;
    where prod_log_date gt qc_log_date gt "";
    *keep output_file_name prod_log_date qc_log_date;
run;

*=======================================================================================================;
*dash board for status;
*=======================================================================================================;

*--------------------------------------------------------------;
*present;
*--------------------------------------------------------------;

proc sort data=t03(keep=priority output_type output_file_name) out=tfl_totals_pre nodupkey;
    by priority output_type output_file_name;
    where not missing(output_type) and not missing(output_file_name);
run;

proc sql;
    create table tfl_totals as
        select priority,output_type, count(distinct output_file_name) as tfl_total
        from tfl_totals_pre
        group by priority,output_type;
quit;

*--------------------------------------------------------------;
*validated;
*--------------------------------------------------------------;

proc sort data=t03 out=validated_tfl_totals_pre nodupkey;
    by priority output_type output_file_name;
    where not missing(output_type) and not missing(output_file_name) and hyper_rtf_report ne "Not-found" and qc_lst_issue ne 1
        and qc_lst_date ne "" and prod_log_date ne "";
run;

proc sql;
    create table validated_tfl_totals as
        select priority,output_type, count(distinct output_file_name) as validated_tfl_total
        from validated_tfl_totals_pre
        group by priority,output_type;
quit;



*------------------------------------------------------------------;
*user level output details;
*------------------------------------------------------------------;

*------------------------------------------;
*list of programmers;
*------------------------------------------;
proc sql;
    create table users as
        select distinct compress(programmed_by) as programmer length=30
        from t03 
        
        union corr

        select distinct compress(validated_by) as programmer length=30
        from t03
        order by programmer;
quit;

data user_totals_dummy;
    set users;
    length output_type prodqc $20;
    retain count 0;
    do output_type="Table","Listing","Figure";
        do prodqc="Production", "QC";
            output;
        end;
    end;
run;
        
*----------------------------------------;
*get counts by each programmer;
*----------------------------------------;

proc sql;
    create table user_totals_pre as
        select distinct compress(programmed_by) as programmer length=30, propcase(output_type) as output_type length=20,
        "Production" as prodqc length=20, count(distinct output_file_name) as count
        from t03
        where hyper_prod_prog ne "Not-found"
        group by compress(programmed_by),propcase(output_type)

        union all corr

        select distinct compress(validated_by) as programmer length=30, propcase(output_type) as output_type  length=20,
        "QC" as prodqc length=20, count(distinct output_file_name) as count
        from t03
        where hyper_qc_prog ne "Not-found"
        group by compress(validated_by),propcase(output_type)
        order by programmer,output_type,prodqc;
quit;

proc sort data=user_totals_dummy;
    by programmer output_type prodqc;
run;
 
data user_totals_pre2;
    merge user_totals_dummy user_totals_pre;
    by programmer output_type prodqc;
run;

proc transpose data=user_totals_pre2 out=user_totals;
    by programmer output_type;
    id prodqc;
    var count;
run;



data track.t03;
    set t03;
run;


proc datasets;
    modify t03;
    attrib _all_ label='';
run;
quit;

%let _x_x_trackorder= priority output_type output_reference title output_file_name
                      hyper_rtf_report
                      hyper_prod_prog hyper_prod_log 
                      hyper_qc_prog hyper_qc_log hyper_qc_lst
                      prod_log_issue qc_log_issue
                      qc_lst_issue
                      prod_log_date qc_log_date rtf_date
                      programmed_by validated_by
                      prod_datasets qc_datasets
                      prod_track_date qc_track_date
;
                      
data t04;
    retain &_x_x_trackorder;
    keep &_x_x_trackorder;
    set t03;
run;

data track.t04;
    set t04;
run;

*------------------------------------------------------------------------------;
*dataset to create hyperlinks for paths of key locations;
*------------------------------------------------------------------------------;
data paths;
    length hyperlink $200;
    hyperlink='=hyperlink("'||"&root.production\tables"  ||'","'||"Open production tables location"  ||'")';
    output;
    hyperlink='=hyperlink("'||"&root.production\listings"  ||'","'||"Open production listings location"  ||'")';
    output;
    hyperlink='=hyperlink("'||"&root.production\figures"  ||'","'||"Open production figures location"  ||'")';
    output;
    hyperlink='=hyperlink("'||"&root.production\macros"  ||'","'||"Open production macros location"  ||'")';
    output;
    hyperlink="";
    output;

    hyperlink='=hyperlink("'||"&root.qc\tables"  ||'","'||"Open qc tables location"  ||'")';
    output;
    hyperlink='=hyperlink("'||"&root.qc\listings"  ||'","'||"Open qc listings location"  ||'")';
    output;
    hyperlink='=hyperlink("'||"&root.qc\figures"  ||'","'||"Open qc figures location"  ||'")';
    output;
    hyperlink='=hyperlink("'||"&root.qc\macros"  ||'","'||"Open qc macros location"  ||'")';
    output;
    hyperlink="";
    output;

    hyperlink='=hyperlink("'||"&root.documentation\tracking"  ||'","'||"Open tracker location"  ||'")';
    output;
    hyperlink='=hyperlink("'||"&root.documentation\tracking\study_status"  ||'","'||"Open study status excel location"  ||'")';
    output;

run;

ods listing close;
ods tagsets.excelxp file="&root.documentation\tracking\study_status\study_status_&_x_x_run_time2..xls"
    options(autofilter='all' );

*----------------------------------------------------;
*summary sheet;
*----------------------------------------------------;

ods tagsets.excelxp options (sheet_name="Master" absolute_column_width="15");

proc print data=t04 label noobs 
                    style(DATA)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt just=l}
                    style(HEADER)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt };
    var &_x_x_trackorder;
run;

*-----------------------------------------------------;
*paths;
*-----------------------------------------------------;

ods tagsets.excelxp options (sheet_name="path_hyperlinks" absolute_column_width="30");

proc print data=paths noobs
                style(DATA)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt just=l}
                style(HEADER)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt };
    var hyperlink;
run;

*--------------------------------------------------------------;
*present;
*--------------------------------------------------------------;
ods tagsets.excelxp options (sheet_name="total_tfls" absolute_column_width="15");

proc print data=tfl_totals noobs
                style(DATA)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt just=l}
                style(HEADER)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt };
    var priority output_type tfl_total;
run;

data validated_tfl_totals2;
    merge tfl_totals validated_tfl_totals;
    by priority output_type;
run;

*--------------------------------------------------------------;
*validated;
*--------------------------------------------------------------;
ods tagsets.excelxp options (sheet_name="validated" absolute_column_width="15");
proc print data=validated_tfl_totals2 noobs
                style(DATA)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt just=l}
                style(HEADER)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt };
    var priority output_type tfl_total validated_tfl_total;
run;



*------------------------------------------------------------;
*prod log issues;
*------------------------------------------------------------;

data prod_log_issue;
    set t03;
    where prod_log_issue=1;
    keep priority output_type output_file_name hyper_prod_log prod_log hyper_to_log_finding ;
    hyper_to_log_finding= '``=HYPERLINK("#'||"'Log_findings'!"|| '" & ADDRESS(MATCH($e'||strip(put(_n_+1,best.))||', Log_findings!$A:$A, 0), 1),'||'"'|| strip(prod_log)||'")';
run;

ods tagsets.excelxp options (sheet_name="Prod_log_issues" absolute_column_width="15");

proc print data=prod_log_issue noobs
                style(DATA)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt just=l}
                style(HEADER)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt };
    var priority output_type output_file_name hyper_prod_log prod_log hyper_to_log_finding;
run;

*------------------------------------------------------------;
*qc log issues;
*------------------------------------------------------------;

data qc_log_issue;
    set t03;
    where qc_log_issue=1;
    keep priority output_type output_file_name hyper_qc_log qc_log hyper_to_log_finding ;
    hyper_to_log_finding= '``=HYPERLINK("#'||"'Log_findings'!"|| '" & ADDRESS(MATCH($e'||strip(put(_n_+1,best.))||', Log_findings!$A:$A, 0), 1),'||'"'|| strip(qc_log)||'")';
run;

ods tagsets.excelxp options (sheet_name="qc_log_issues" absolute_column_width="15");

proc print data=qc_log_issue noobs
                style(DATA)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt just=l}
                style(HEADER)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt };
    var priority output_type output_file_name hyper_qc_log qc_log hyper_to_log_finding;
run;


*------------------------;
*not clear compare;
*------------------------;
ods tagsets.excelxp options (sheet_name="notclear_compare" absolute_column_width="15");

proc print data=notclear_compare noobs
                style(DATA)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt just=l}
                style(HEADER)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt };
    var priority output_type output_file_name title hyper_qc_lst hyper_rtf_report hyper_prod_prog hyper_qc_prog  ;
run;

*------------------------;
* clear compare;
*------------------------;
ods tagsets.excelxp options (sheet_name="clear_compare" absolute_column_width="15");

proc print data=clear_compare noobs
                style(DATA)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt just=l}
                style(HEADER)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt };
    var priority output_type output_file_name title  hyper_rtf_report hyper_qc_lst hyper_prod_prog hyper_qc_prog 
    prod_log_issue qc_log_issue;
run;

*----------------------------------;
*qc not run after production;
*----------------------------------;

ods tagsets.excelxp options (sheet_name="qc lt prod" absolute_column_width="15");

proc print data=qcltprod noobs
                style(DATA)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt just=l}
                style(HEADER)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt };
    var priority output_type output_file_name prod_log_date qc_log_date hyper_rtf_report hyper_prod_prog hyper_qc_prog;
run;

*------------------------------------------------------------------;
*prod log missing;
*------------------------------------------------------------------;

ods tagsets.excelxp options (sheet_name="prod_log_missing" absolute_column_width="15");

proc print data=prod_log_missing noobs
                style(DATA)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt just=l}
                style(HEADER)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt };
    var priority output_type output_file_name prod_prog_date;
run;


*------------------------------------------------------------------;
*qc log missing;
*------------------------------------------------------------------;

ods tagsets.excelxp options (sheet_name="qc_log_missing" absolute_column_width="15");

proc print data=qc_log_missing noobs
                style(DATA)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt just=l}
                style(HEADER)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt };
    var priority output_type output_file_name qc_prog_date;
run;

*--------------------------------------------------------;
*qc lst files missing;
*--------------------------------------------------------;

ods tagsets.excelxp options (sheet_name="qc_lst_missing" absolute_column_width="15");

proc print data=qc_lst_missing noobs
                style(DATA)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt just=l}
                style(HEADER)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt };
    var priority output_type output_file_name qc_prog_date;
run;

*-------------------------------------------------------;
*prod programs not-found;
*-------------------------------------------------------;

ods tagsets.excelxp options (sheet_name="prod_prog_missing" absolute_column_width="15");

proc print data=prod_prog_missing noobs
                style(DATA)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt just=l}
                style(HEADER)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt };
    var priority output_type output_file_name title ;
run;

*-------------------------------------------------------;
*qc programs not-found;
*-------------------------------------------------------;

ods tagsets.excelxp options (sheet_name="qc_prog_missing" absolute_column_width="15");

proc print data=qc_prog_missing noobs
                style(DATA)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt just=l}
                style(HEADER)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt };
    var priority output_type output_file_name title;
run;



*-----------------------------------------------------;
*log findings;
*-----------------------------------------------------;


ods tagsets.excelxp options (sheet_name="Log_findings" absolute_column_width="15,100");

proc print data=logcheck_findings noobs
                style(DATA)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt just=l}
                style(HEADER)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt };
    var output_file_name line;
run;


ods tagsets.excelxp close;
ods listing;

proc printto;
run;

filename _logchk "&root.production\macros\tracker_to_checks.log";
%gen_logcheck(logref=_logchk);
filename _logchk;

    
