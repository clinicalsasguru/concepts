dm lst 'clear';
dm log 'clear';

proc datasets library=work mt=data kill;
quit;

%let path=%str(\Production\Listings);


filename programs pipe "dir /b ""&path\%str(*).sas"" " ;

data programs;

  infile programs truncover;

  input filename $100. ;

  filename=substr(filename,1,length(filename)-4);
run;

data clearbase;
	length program char $50 req_line $256;
	call missing(program,req_line, char);
run;

data notclearbase;
	length program char $50 req_line $256;
	call missing(program,req_line);
run;

%macro linesizecheck(program);
data clear notclear;
	infile "&path.\&program..sas" truncover lrecl=256 end=last;
	length text req_line $256 program char $50;
	input text 1-256;
	program="&program.";
	find1=index(upcase(_infile_),"SDTM.");
	find2=index(upcase(_infile_),"ADAM.");
	find3=index(upcase(_infile_),"DERIVED.") ;
	find4=index(upcase(_infile_),"RAW.");
	if find1 gt 0 then do;
		start=findc(_infile_,'(; ',find1+length('SDTM.'));
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
        char=substrn(upcase(_infile_),find3,start-find3);
		req_line=compress(_infile_);
	end;

	if find4 gt 0 then do;
		start=findc(_infile_,'(; ',find4+length('RAW.'));
        char=substrn(upcase(_infile_),find4,start-find4);
		req_line=compress(_infile_);
    end;

if find1 gt 0 or find2 gt 0 or find3 gt 0 or find4 gt 0;
*keep program req_line char;
run;

proc append base=clearbase data=clear force;
run;

proc append base=notclearbase data=notclear force;
run;
%mend;

data check;
     set programs;
     if strip(filename)="l1626_1_09";
     call execute('%linesizecheck('||strip(filename)||');');
run;

*---------------------------------------------;
*dataset containing the program with clear log;
*----------------------------------------------;
data clearbase;
	set clearbase;
	if missing(program) then delete;
run;

*--------------------------------------------------;
*dataset containing the program with NOT clear log;
*--------------------------------------------------;

data notclearbase;
	set notclearbase;
	if missing(program) then delete;
	*if flg ne 1;
run;
