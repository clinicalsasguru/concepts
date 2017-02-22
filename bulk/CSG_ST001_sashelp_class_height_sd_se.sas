
%gen_init_001;

*--------------------------------------------------------------------;
comment- Create a temporary copy of the permanent dataset;
*--------------------------------------------------------------------;

data class01;
    set sashelp.class;
run;

*--------------------------------------------------------------------;
comment- calculate the sum for height variable using data step;
comment- also get the number of contributing observations;
*--------------------------------------------------------------------;

data class02;
    set class01;
    height_sum +height; *using sum statement add the height values into a new variable called height_mean;
    if not missing(height) then height_n+1; *using sum statement to ge the number of non-missing height values;
run;

data class03;
    set class02 end=last; *using end= option of set statement to select only the last observation;
    if last; *this statment is equivalent to using last=1 - in sas any numeric value other than 0 or missing considered as true;
    keep height_sum height_n;
run;

data class04;
    set class03;
    height_mean=height_sum/height_n;
run;

*--------------------------------------------------------------------;
comment- calculate variance;
*--------------------------------------------------------------------;

data class05;
    if _n_=1 then set class04; *get mean values into the original dataset;
    set class01;
    if nmiss(height,height_mean)=0 then diff=height_mean - height;*get the individual value deviation from the mean;
    if not missing(diff) then diff_squared=diff**2;*square the deviation;
    diff_squared_sum+diff_squared;
run;

data class06;
    set class05 end=last;*note that end= is an option and last is a temporary variable;
    if last;
    if not missing(diff_squared_sum) and height_n gt 1 then do;*variance/sd can only be calculated only if there are atleast two non-missing observations;
        height_variance=diff_squared_sum/(height_n-1); *for sample variance we need to use n-1: and be careful of the parenthesis;
    end;

run;
*--------------------------------------------------------------------;
comment- calculate standard deviation;
*--------------------------------------------------------------------;

data class07;
    set class06;
    if not missing(height_variance) then height_sd=height_variance**0.5;*raised to power of 0.5 means square root;
    keep height_sd height_n;
run;

*--------------------------------------------------------------------;
comment- calculate standard error of mean;
*--------------------------------------------------------------------;

data class08;
    set class07;
    if height_sd ne . and height_n ge 1 then height_se=height_sd/(height_n**0.5);
run;

*--------------------------------------------------------------------;
*comment- cross check with proc means;
*--------------------------------------------------------------------;

proc means data=sashelp.class n mean std var stderr;
var height;
run;

%gen_term_001;

