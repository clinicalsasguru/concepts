
%macro indent(variable, split=%nrbquote(/), column_width=25, indent_first=0,
indent_rest=&indent_first, hyphen=yes);
attrib new_&variable length=$200 &variable.piece length=$&column_width. rest length=$200;
rest=&variable;
if not missing(rest) then do until (missing(rest));
indent=ifn(missing(new_&variable), &indent_first, &indent_rest);
stringsize=&column_width-indent;
select;
%if &hyphen=yes %then %str(
when (notalpha(substr(rest, stringsize-1, 3))=0) do;
&variable.piece=cat(substrn(repeat('', indent), 2), substr(rest, 1, stringsize-1), '-');
cutoff=stringsize;
end;);
%if &hyphen^=yes %then %str(
when (notalpha(substr(rest, 1, stringsize))=0) do;
&variable.piece=cat(substrn(repeat('', indent), 2), substr(rest, 1, stringsize));
cutoff=stringsize+1;
end;);
when (findc(char(rest, stringsize+1), '_-', 'bsp')) do;
&variable.piece=cat(substrn(repeat('', indent), 2), substr(rest, 1, stringsize));
cutoff=stringsize+1;
end;
otherwise do;
&variable.piece=cat(substrn(repeat('', indent), 2), substr(rest, 1, findc(substrn(rest, 1, stringsize),
'_-', 'bsp')));
cutoff=findc(substrn(rest, 1, stringsize), '_-', 'bsp')+1;
end;
end;
rest=strip(substr(rest, cutoff));
new_&variable=catt(new_&variable, &variable.piece, "&split");
end;
drop indent stringsize &variable.piece cutoff rest;
%mend indent;
