      if sasfmt^=. then
         do;
            fmt='8.'||scan(put(sasfmt,best.),2,'.');
            fmt1='8.'||scan(put((sasfmt+0.1),best.),2,'.');
            rndn   = 10**(-((sasfmt - int(sasfmt))*10));
            rndn1   = 10**(-(((sasfmt - int(sasfmt))+0.1)*10));
         end;
      else
         do;
            fmt= '8.0';
            fmt1='8.1';
            rndn   = 0.1;
            rndn1   = 0.1;
         end;
         
      array aval{8} aval_n aval_mn aval_std aval_min aval_max aval_med aval_q1 aval_q3;
      array chg {8} chg_n chg_mn chg_std chg_min chg_max chg_med chg_q1 chg_q3;
      array avalf {8} $ ca_n ca_mean ca_std ca_min ca_max ca_med ca_q1 ca_q3;
      array chgf  {8} $ cc_n cc_mean cc_std cc_min cc_max cc_med cc_q1 cc_q3;

      do i=1 to 8;
         if i=1 then
            do; *conversion of n to character;
               if aval{i} ne . then
                  avalf{i}=put(aval{i}, 3.0);

               if chg{i} ne . then
                  chgf{i}=put(chg{i}, 3.0);
            end;
         else if (i=4 or i=5) then
            do;*conversion of min and max to character - number of decimals as in data;
               if aval{i} ne . then
                  avalf{i}=putn(round(aval{i}, rndn), fmt);

               if chg{i} ne . then
                  chgf{i}=putn(round(chg{i}, rndn), fmt);
            end;
         else
            do;*conversion of mean, std, median, q1, q3 to character - number of decimals as in data + 1;
               if aval{i} ne . then
                  avalf{i}=putn(round(aval{i}, rndn1), fmt1);

               if chg{i} ne . then
                  chgf{i}=putn(round(chg{i}, rndn1), fmt1);
            end;
      end;
   run;         
