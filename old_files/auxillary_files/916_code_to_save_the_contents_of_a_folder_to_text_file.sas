x CHDIR /d &&_x_x_&filetype._files_loc. & 
dir /T:W  /A:-D  > C:\Users\&_x_x_user.\Desktop\dump\contents_&filetype..txt;


data &filetype._files(rename=(program=program_name main_prg_date=&filetype._date));
    infile "C:\Users\&_x_x_user.\Desktop\dump\contents_&filetype..txt" truncover;
    length program_name $50 test $10 program $30;
    input test$ 1-10 @;
    if not missing(input(test, ?? mmddyy10.)) then do;
    input @1 main_prg_mod_date mmddyy10.  main_prg_mod_time & time10. file_size : comma32. program_name;
    end;
    if upcase(scan(program_name,2,"."))="%Upcase(&filetype.)";
    format  main_prg_mod_date date9.  main_prg_mod_time time8.;
    program=scan(program_name,1,".");
    main_prg_date=put(main_prg_mod_date,yymmdd10.)||"T"||put(main_prg_mod_time,tod5.);

    keep program main_prg_date;
run;

