%let path=D:\Lesson Files\auxillary_files;
dm 'log; file "&path\logdump1.log" replace';

filename temp1 "&path\logdump1.log";
data test;
	infile temp1 end=last;
	input;
	if index(_infile_,"ERROR") then errorcount+1;
	if index(upcase(_infile_),"UNINIT") then uninitcount+1;
	if last then do;
	put "Number of errors" errorcount;
	put "Number of uninitialised" uninitcount;

	end;
run;
