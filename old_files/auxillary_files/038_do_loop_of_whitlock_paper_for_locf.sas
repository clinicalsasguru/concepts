dm log 'clear';
dm lst 'clear';
dm log 'preview';

proc datasets library=work mt=data kill nolist;
quit;

data weight ;
   do pt = 1 to 2 ;
   do visit = 1 to 5 ;
   weight = ceil ( 200 * ranuni ( 2000 ) ) ;
   if weight < 150 then weight = . ;
   output ;
   end ;
   end ;
run ;

data postweight ;
   do until ( last.pt ) ;
   set weight ( where = ( visit > 3 ) ) ;
   by pt ;
   if weight ^= . then post = weight ;
   output ;
   end ;
run;
