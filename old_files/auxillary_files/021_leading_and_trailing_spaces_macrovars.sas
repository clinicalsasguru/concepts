
%let name=       anil   ;

%put &name;

%put ignores leading and trailing spaces *&name*;


%let name=%str(       anil    );

%put keep the spaces with str  *&name*;

%put remove the spaces with macro cmpres fn *%cmpres(&name)*;


*-----------------------------------------------;
*require a semicolon in macro variable value
*-----------------------------------------------;

%let name=%str(       anil;   );

%put &name;

%let name=%str(  Race   n(%%));

%put &name;

