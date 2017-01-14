**** INPUT SAMPLE ADVERSE EVENT DATA WHERE SUBJECT = PATIENT ID
**** AND ADVERSE_EVENT = ADVERSE EVENT TEXT.;
data aes;
input @1 subject $3.
@5 adverse_event $15.;
datalines;
101 Headache
102 Rash
102 Fatal MI
102 Abdominal Pain
102 Constipation
;
run;

**** INPUT SAMPLE DEATH DATA WHERE SUBJECT = PATIENT NUMBER AND
**** DEATH = 1 IF PATIENT DIED, 0 IF NOT.;
data death;
input @1 subject $3.
@5 death 1.;
datalines;
101 0
102 0
;
run;
**** SET DEATH = 1 FOR PATIENTS WHO HAD ADVERSE EVENTS THAT
**** RESULTED IN DEATH.;
data aes;
merge demog aes;
by subject;
if adverse_event = "Fatal MI" then
death = 1;
run;
proc print
data = aes;
run;

/*Notice how the “Abdominal Pain” and “Constipation” observations also were flagged as
death = 1. This happens because, by design, SAS automatically retains the values of all
variables brought into a DATA step via the MERGE, SET, or UPDATE statement. In
this example, because the “death” variable already exists in the “death” data set, that
variable gets retained. When you assign death = 1 for “Fatal MI,” the subsequent records
within that BY group are also set to 1.*/
