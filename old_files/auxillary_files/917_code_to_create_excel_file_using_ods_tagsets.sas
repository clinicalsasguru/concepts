ods tagsets.excelxp file="&_x_x_program_loc\batch_runs_info\batch_run_&_x_x_run_time2..xls";

ods tagsets.excelxp options (sheet_name="Summary");

proc print data=_x_x_excel label noobs 
                    style(DATA)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt just=l}
                    style(HEADER)={rules=all frame=box font_face=courier font_size=12pt background=white borderwidth=0.5pt };
    var description count;
    label description="Description"
        count="No. of files";
run;
ods tagsets.excelxp close;
