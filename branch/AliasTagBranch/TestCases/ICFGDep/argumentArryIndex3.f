      subroutine foo(a,b)
       double precision a
       double precision b

       b=a*2             
c                            mUses   =>  mDefs
c                            ========================================
c                            [*a]    =>  *b
c
c                            ImplicitRemoves:
c                            =================
 

      end subroutine

c$openad XXX Template ad_template.f
      subroutine head(x,y)
       double precision, dimension(2) :: x
       double precision y
       double precision, dimension(2) :: p,q
       integer k,l
c$openad INDEPENDENT(x)


c
c                            ImplicitRemoves:
c                            =================


       call foo(x(k),y)     


c                            mUses             =>  mDefs
c                            ========================================
c
c                            ImplicitRemoves:
c                            =================

       call foo(p(k),q(l))   


c                            mUses             =>  mDefs
c                            ========================================
c
c                            ImplicitRemoves:
c                            =================

c$openad DEPENDENT(y)
      end subroutine


