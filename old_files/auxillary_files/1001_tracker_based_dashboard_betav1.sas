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

*================================================================================================;
*defining some global macro variables to control the process;
*================================================================================================;

%let _x_x_reread_tracker            =*;
%let _x_x_restrict_logcheck         =Y;
%let _x_x_restrict_lstcheck         =Y;
%let _x_x_restrict_inputdatacheck   =N;
%let _x_x_restrict_datecheck        =Y;
%let _x_x_restrict_type             =tables listings;
%let _x_x_user                      =&sysuserid.;


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
    set t01;
    length  prod_program 
            prod_lst prod_log qc_program qc_lst qc_log rtf
            fprod_prog fqc_prog fprod_lst fqc_lst fprod_log fqc_log frtf_report
            prod_path qc_path output_path
            $200;

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
    drop i prod_: qc_: rtf output_path frtf_report;
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
proc printto  log=junk; 
run;

%prodqclogcheck

proc printto;
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


            data &_xxj_type.check01;
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
if filename ne "" and char ne "";

    filename=lowcase("&program");
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

proc append base=input_datasets data=&_xxj_type._&_xxk_location._temp_input force;
run;

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

    proc sort data=input_datasets nodupkey;
        by filename;
        where not missing(filename) and not missing(input_datasets);
    run;
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
	if not missing(input(test, ?? mmddyy10.)) then do;
	input @1 main_prg_mod_date mmddyy10.  main_prg_mod_time & time10. file_size : comma32. program_name;
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



%gen_logcheck;
    
