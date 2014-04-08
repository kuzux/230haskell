begin 
   x1 := 0 ;
   y1 := 0 ;
   x2 := 1 ;
   y2 := 1 ;
   
   S := ( x1 - x2 ) * ( x1 - x2 ) +  ( y1 - y2 ) * ( y1 - y2 ) ;
   d := S ;
   n := 50 ;
   while ( n ) do begin
     d :=  (d + S / d ) / 2 ;
     n := n - 1 
   end ;
   print d 
end 
