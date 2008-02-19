!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that involves aliasing due to a reference parameter.
!
! AliasPairs : (t1,*a,*b)      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine head(x, f) 
       double precision :: x
       double precision :: f
       double precision t1, t2, t3

       t1=x*f                     ! AliasTag("head::t1") => (1,MUST)
                                  ! AliasTag("head::*x") => (2,MAY)
                                  ! AliasTag("head::x")  => (3,MUST)
                                  ! AliasTag("head::*f") => (4,MAY)
                                  ! AliasTag("head::f")  => (5,MUST)
                                  
                                  
       call bar(t1,t1)            ! AliasTag("bar::*a") => (1,MAY)
                                  ! AliasTag("bar::a")  => (8,MUST)
                                  ! AliasTag("bar::*b") => (1,MAY)
                                  ! AliasTag("bar::b")  => (9,MUST)
                                  ! AliasTag("bar::t1") => (1,MAY)
                                  
                                  
       t3=f*30                    ! AliasTag("head::t3") => (6,MUST)
                                  
       f=t1+t2                    ! AliasTag("head::t2") => (7,MUST) 
                                  
       end subroutine

       subroutine bar(a,b) 
                                  
       double precision a,b

       b = a                  

       return
       end
