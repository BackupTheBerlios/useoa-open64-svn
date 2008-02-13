 subroutine foo(x,y) 
   double precision, dimension(2) :: x
   double precision y
   y=x(1)*x(2)
 end subroutine

 subroutine head(x,y) 
   double precision, dimension(2) :: x, px
   double precision y, py
   px(1)=1.0
   px(2)=2.0
   call foo(x,y)
   call foo(px,py)
   call foo(px,py)
 end subroutine


