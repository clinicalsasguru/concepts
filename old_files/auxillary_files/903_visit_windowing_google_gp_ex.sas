data TEST;
input PATNO LABVAL ;
cards;
 11   7
 11   5
 11   27
 11   29
 11   30

 22   6
 22   5
 33   27
 33   29
 33   31
;
run;
data xx;
 set test;
     if  4 <=labval<=10 then vrange=7;
else if 12 <=labval<=18 then vrange=15;
else if 25 <=labval<=35 then vrange=28;
distance=abs(labval-vrange);
run;

proc sort;
by patno vrange distance descending labval;
run;

data xx1;
 set xx;
by patno vrange distance descending labval;
if first.vrange;
run;

proc sort data=test;
by patno labval;
run;

proc sort data=xx1 out=xx2;
by patno labval;
run;

data need;
 merge test xx2 (keep=patno labval vrange);
by patno labval;
run;

proc print;
run;
