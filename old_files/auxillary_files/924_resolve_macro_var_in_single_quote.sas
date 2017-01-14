dm output 'clear';

keypoint:
put %unquote(%bquote(')-awstitle "This is &compound."  %bquote('));
resolved string:
-awstitle "This is compound10"  
link to the article:http://support.sas.com/resources/papers/proceedings15/2221-2015.pdf;

options ls=max;

%let compound=compound10;
data test;
	infile "D:\Home\dev\master_files\sasv9.cfg" lrecl=256 missover truncover;
	file "D:\Home\dev\master_files\sasv9_test.cfg";
	input text  $256.;
	put _infile_;
	index=indexw(compress(_infile_),compress("/*DO NOT EDIT BELOW THIS LINE - INSTALL Application edits below this line*/"));
	if index then do;
		put;
		put "/*soa-------------------------------------------------------------------------*/";
		put '-SET SASROOT "G:\Program Files\SASHome\SASFoundation\9.3 " ';
		put %unquote(%bquote(')-awstitle "This is &compound."  %bquote('));
		put "-awsdef 0 0 100 100";
		put '-SET MYSASFILES "D:\Home\dev\&compound." ';
		put '-SASUSER "D:\Home\dev\&compound." ';

		put "-autoexec D:\Home\dev\&compound.\lums\autoexec.sas";

		put "/*-----------------------------------------------------------------------eoa*/";

		put;
		put;

	end;
run;
