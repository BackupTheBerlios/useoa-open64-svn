  
      subroutine head(x1,y1,x2,y2)
 
         double precision v3,v4,x1,x2,y1,y2

         v3=x1*x2
         v4=x1*v3
         y1=v3*x2*v4
         y2=sin(v4)

       end subroutine
