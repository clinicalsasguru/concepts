

%macro descriptive(
		lbtestcd=,
		label=,
		group=,
		n=,
		mean=,
		sd=,
		median=,
		q1=,
		q3=
		);

proc sort data=adlb out=adlb_&lbtestcd.;
	by visitnum visit;
	where upcase(lbtestcd)=upcase("&lbtestcd.");
run;
	

proc summary data=adlb_&lbtestcd.  nway;
	by visitnum visit;
	class trtpn;
	where not missing(lbstresn) ;
	var lbstresn;
	output out=&lbtestcd._stats(drop=_type_ _freq_)
	n= mean= std= median= q1= q3= /autoname;
run;

data &lbtestcd._stats2;
	set &lbtestcd._stats;
	by visitnum visit;
	if not missing(lbstresn_n) then n=put(lbstresn_n,&n.);
	else n=put(0, &n.);
	if not missing(lbstresn_mean) then mean=put(lbstresn_mean,&mean.);

	if not missing(lbstresn_stddev) then sd=put(lbstresn_stddev,&sd.);
	else if lbstresn_n=1 then sd="   -";

	if not missing(lbstresn_median) then median=put(lbstresn_median,&median.);
	if not missing(lbstresn_q1) then q1=put(lbstresn_q1,&q1.);
	if not missing(lbstresn_q3) then q3=put(lbstresn_q3,&q3.);

	drop lbstresn_:;
run;

proc transpose data=&lbtestcd._stats2 out=&lbtestcd._stats3(drop=_name_) label=statistic;
	by visitnum visit  trtpn;
	var n mean sd median q1 q3;
	label n="N-obs"
			mean="Mean"
			sd="SD"
			median="Median"
			q1="Quartile 1"
			q3="Quartile 3";
run;

data &lbtestcd._stats4;
	set &lbtestcd._stats3;
	length label $50;

	label="&label.";
	group=&group.;

	select(statistic);
		when("N-obs") intord=1;
		when ("Mean") intord=2;
		when ("SD") intord=3;
		when ("Median") intord=5;
		when ("Quartile 1") intord=4;
		when ("Quartile 3") intord=6;
	otherwise;
	end;
run;

proc sort data=&lbtestcd._stats4;
	by visitnum visit group intord label statistic;
run;

proc transpose data=&lbtestcd._stats4 out=_final_stats_&lbtestcd.;
	by  visitnum visit group intord label statistic;
	var col1;
	id trtpn;
run;

data final_stats_&lbtestcd.;
	set _final_stats_&lbtestcd.;
	length c1-c6 $200;
	c1=label;
	c2=statistic;
	c3=_0;
	c4=_1;
	c5=_2;
	c6=_3;
	keep group  intord c1-c6 visitnum visit;
run;

%mend;

%descriptive(
		lbtestcd=ALT,
		label=%str(Age (in Years)),
		group=1,
		n=4.,
		mean=6.2,
		sd=7.3,
		median=6.2,
		q1=6.2,
		q3=6.2
		);
