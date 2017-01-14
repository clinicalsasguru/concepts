
data final ;
    set final ;
    if ord = 1 then order = 1 ;
    	else order = 2 ;
run;

data new1;
  length aebodsys1 label_ $250;
  set final;
  by socord aebodsys ptord aedecod ;
  retain pg 0 new1 0;
  %indent(variable  = c1, split=%nrbquote(#), column_width=44, indent_first= 0, 
         indent_rest=0, hyphen= no);
  if new_c1 ne '' then label_ = new_c1;
  if (new1 + countc(label_,'#') - 1 > 21 ) then do;
    pg + 1;
    new1 = 1 ;
      if index(label_,' ') = 1 and label_ ne '' then aebodsys1 = trim(left(aebodsys))||" (Continued)";
      %indent(variable  = aebodsys1 , split=%nrbquote(#), column_width=44, indent_first= 0, 
         indent_rest=1, hyphen= no);
    if index(label_,' ') = 1 and label_ ne '' and index(new_aebodsys1,'#') > 1 then do;
      new1 = new1 + countc(new_aebodsys1,'#') - 1 + countc(label_,'#');
      end;
      if index(label_,' ') > 1 and countc(label_,'#') > 1 then do;
      new1 = new1 + countc(label_,'#') - 1;
      end;
  end;
  else do;
    new1 + 1;
      if index(label_,' ') = 1 and label_ ne '' and countc(label_,'#') > 1 then do;
      new1 = new1 + countc(label_,'#') - 1 ;
      end;

      if index(label_,' ') > 1 and index(label_,'#') > 1 then do;
      new1 = new1 + countc(label_,'#') - 1;
      end;
  end; 
  if last.aebodsys then new1 + 1;
run;

proc sort data = new1 out = new1_srt ;
      by socord aebodsys ptord aedecod ;
run;

proc sort data = new1 out = new1_srt_;
      by pg socord aebodsys ptord aedecod;
run;

data new2(rename = (new_label_1 = label_1));
  set new1_srt_;
  by pg socord aebodsys ptord aedecod;
  x3=1000;
  %indent(variable  = aebodsys , split=%nrbquote(#), column_width=44, indent_first= 0, 
         indent_rest=3, hyphen= no);
  if first.pg and not missing(new_aebodsys) and pg > 0 then do;
    if label_ ne new_aebodsys then do;
      label_1 = trim(left(aebodsys))||" (Continued)";
        %indent(variable  = label_1 , split=%nrbquote(#), column_width=44, indent_first= 0, 
         indent_rest=0, hyphen= no);
      output;
    end;
  end;
  keep pg aebodsys aedecod new_label_1 x3 new_aebodsys socord ptord;
run;

data test ;
    set new1 new2 ;
run;

data testx ;
    set test ;
    if missing ( label_ ) then label_ = label_1 ;
run;

proc sort data = testx out= final_all ;
    by pg order socord aebodsys ptord aedecod ;
run; 
