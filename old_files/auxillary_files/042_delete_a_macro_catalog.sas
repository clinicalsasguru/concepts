%sysmacdelete test;

proc catalog cat=work.sasmacr;

  contents;

  run;

  delete test / entrytype=macro;

  run;

  contents;

  run;

quit;
