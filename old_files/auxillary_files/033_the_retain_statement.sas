dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;
/*
Questions:
It would be useful to compare information from one visit to another,
for a given patient.
1. How many days were there from the previous visit? 

2. Did this patient’s blood pressure go up or down when compared to previous visit(s)?
*/

*----------------------------------------;
*pdv understanding;
*----------------------------------------;

data pdv1;
	input visit;
cards;
1
2
.
4
;
run;

data pdv2;
	length param $15;
	input visit param$;
cards;
1 systolicBP
2 diastolicBP
3 pulse
;
run;

*-----------------------------------------;
*DATA Step without a RETAIN Statement;
*-----------------------------------------;

data withOutRetain;
   put "before the input statement:  " _all_ ;
   input x;
   put "after the input statement:   " _all_ /;
datalines;
1
2
.
4
;
run;

*set statement based example;

data class;
   put "before the input statement:  " _all_ ;
   set sashelp.class(obs=3);
   x=99;
   put "after the input statement:   " _all_ /;
run;

*----------------------------------;

data withRetain;
   retain x;
   put "before the input statement:  " _all_;
   input x;
   put "after the input statement:   " _all_ /;
datalines;
1
2
.
4
;
run;

data class;
   retain x;
   put "before the input statement:  " _all_ ;
   set sashelp.class(obs=3);
   x=99;
   put "after the input statement:   " _all_ /;
run;

*------------------------------------;
*real time use of retain(locf);
*------------------------------------;

*without retain;

data carryForward1;
   put "before input:      " _all_ ;
   input x;
   if x ne . then old_x = x;
   else x = old_x;
   put "after assignment:  " _all_ /;
datalines;
1
2
.
4
;
run;

*with retain;

data carryForward2;
   retain old_x;
   put "before input:      " _all_ ;
   input x;
   if x ne . then old_x = x;
   else x = old_x;
   put "after assignment:  " _all_ /;
datalines;
1
2
.
.
4
;
run;

*------------------------------------;
*generating sequential numbers;
*------------------------------------;

*without retain;

data seq1;
	input x;
cards;
1
3
5
;
run;

data seq2;
	set seq1;
	visit=visit+1;
run;

data seq3;
	set seq1;
	retain visit;
	visit=visit+1;
run;

/*1 1*/
/*1 2*/
/*1 3*/
/*2 1*/
/*2 2*/
/*2 3*/
/*3 1*/
/*3 2*/
/*3 3*/

/*x visit*/
/*. .*/
/*1 .+1 .*/
/*1     .*/
/*3 .+1 . */

data seq3_1;
	set seq1;
	retain visit 0;
	visit=visit+1;
run;

/*x v(calc)  v */
/*. .        0*/
/*1 0+1      1*/
/*1          1*/
/*3 1+1      2*/
/*3          2*/
/*5 2+1      3*/


data seq4;
	set seq1;
	visit +1;
run;

