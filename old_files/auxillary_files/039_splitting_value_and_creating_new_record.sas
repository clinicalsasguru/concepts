%macro splitcoval;

%if &covallen. gt 1 %then %do;
   data co(rename=(covalx=coval));
      set co(rename=(coval=covaltemp));
      covalx=substrn(covaltemp,1,200);
      array coval[&covallen.] $ 200;
      do i=1 to &covallen.;
         coval[i]=substrn(covaltemp,200*i+1,200);
      end;
   run;
%end;

%mend;

%splitcoval;
