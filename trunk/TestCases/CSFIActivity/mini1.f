      subroutine foo(x,y)
        double precision x
        double precision y
        y=x*2
      end subroutine

      
      subroutine head(x,y)
        double precision, dimension(2) :: x
        double precision y
        double precision, dimension(2) :: p,q
        integer k,l
c$openad INDEPENDENT(x)
        k=1
        call foo(x(k),y)
        p(1)=1.0
        l=1
        call foo(p(k),q(l))
c$openad DEPENDENT(y)
       end subroutine

