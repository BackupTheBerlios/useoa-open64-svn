



       subroutine bar(c,d) 
          double precision c,d
          d=c*c

c                    mUses                => mDefs
c                    ==============================
c                    [1: bar::*c,           [2: bar::*d,
c                        foo::*a,               foo::*b
c                        head::*x,              head::*y,
c                        head::*x()]            head::*y()]
c
c                    ImplicitRemoves:
c                    ================
c      

       
       end subroutine

       subroutine foo(a,b) 
          double precision a,b
          call bar(a,b)
       end subroutine

       subroutine head(x,y) 
          double precision, dimension(1) :: x
          double precision, dimension(1) :: y
          call foo(x(1),y(1))
       end subroutine


