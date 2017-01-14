dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

*-----------------------------------------------;
*create a temporary dataset of sashelp.cars;
*-----------------------------------------------;

data cars;
	set sashelp.cars;
run;

*question-concatenate all the models(separated by a delimiter)
in a make and make a single record for each make;

proc sort data=cars out=cars1(keep=make model);
	by make model;
run;

data cars2;
	set cars1;
	by make model;

	length temp1 concatenated_var $1500;
	retain temp1 concatenated_var  ;

	if first.make then concatenated_var="";*if first.make then call missing(concatenated_var);
	temp1=concatenated_var;
	concatenated_var=catx(', ',temp1,model);
	*concatenated_var=catx(', ',"","3.5RL nav");
	*concatenated_var="3.5RL";
	
run;

data cars3;
	set cars2;
	by make model;
	if last.make;
	keep make concatenated_var;
run;

*usage: concatenate different ae terms within a aegrpid;
*		concatenate different cm terms used for a adverse event;

	
data catx;
	var1='a';
	var2='b';
	var3='c';
	var4='';
	var5='e';

concat1=var1||', '||var2||', '||var3||', '||var4||', '||var5;
catx1=catx(', ',var1,var2,var3,var4,var5);
*catx1=catx(', ',of var6-var10);

concat2=var4||', '||var1;
catx2=catx(', ',var4,var1);
run;
