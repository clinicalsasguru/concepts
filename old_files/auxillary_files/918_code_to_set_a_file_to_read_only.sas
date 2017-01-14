%macro fileattr(EXT=,_Folder=,_File=,_attr=WRITE);
%let _FullPth = &_Folder.\&_File..&EXT.  ;

* Set Extension Flag to be mis-sing ;
%let EXT_FL=. ;
%if (%upcase(&EXT.)=RTF or %upcase(&EXT.)=DOC or %upcase(&EXT.)=XLS
    or %upcase(&EXT.)=XLSX) %then %let EXT_FL=1;

%if &EXT_FL.=. %then
    %do ;
      %put %str(WARN)%str(ING:: Extension Flag is not Valid) ;
    %end ;

%if &EXT_FL.=1 %then
    %do ;
        %if %upcase(&_attr.) eq READ  %then
            %do ;
              x "attrib +R &_FullPth." ; * Enables READ only protection ;
            %end ;

        %if %upcase(&_attr.) ne READ %then
            %do ;
              x "attrib -R &_FullPth." ; * Removes READ only protection ;
            %end ;
    %end ;
%mend fileattr ;
