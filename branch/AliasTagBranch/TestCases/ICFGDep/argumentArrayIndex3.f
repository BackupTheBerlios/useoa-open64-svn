!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Alias Analysis results for this TestCase are not proper
! This ICFGDep Results are not correct.     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
      

      subroutine foo(a,b) 
       double precision a
       double precision b
       b=a*2
       
c                        mUses                => mDefs
c                        ==============================
c                        [1: foo::*a,         => [2: foo::*b, 
c                            head::*x(),             head::*y,
c                            head::*x,               head::q() ]
c                            head::p()]
c
c                        ImplicitRemoves: 
c                        ================
c 

      end subroutine

c$openad XXX Template ad_template.f
      subroutine head(x,y) 
       double precision, dimension(2) :: x
       double precision y
       double precision, dimension(2) :: p,q
       integer k,l
       call foo(x(k),y)

       call foo(p(k),q(l))
      end subroutine
