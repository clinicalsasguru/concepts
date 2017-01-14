
%macro aligdec(indsn=,invar=);
   %local i j ;
    %let j = 1 ;
    %do %while(%scan(&invar,&j) ne );
        %let i = %scan(&invar,&j); 
   data _alig0;
      set &indsn end=eof;
      retain maxint 0;
      if index(&i, '(') then dot = min(index(&i, '.'),index(&i, '('));
      else dot = index(&i, '.');
      if dot ne 0 then lenint = length(trim(left(substr(&i, 1, (dot - 1)))));
      else lenint = length(trim(left(&i)));
      maxint = max(maxint, lenint);
      if eof then call symput("maxint",put(maxint,best.));
   run;

   data &indsn (drop=&i);
      set _alig0;
      length &i.c $15;
      if not missing(&i) then do;
        diffint = &maxint - lenint - 1;
      if diffint >= 0 then do;
        &i.c = repeat(" ", diffint)||trim(left(&i));
      end;
        else do;
          &i.c = trim(left(&i));
        end;
      end;
   run;

   %let j = %eval(&j+1) ;
   %end ;
%mend aligdec;
