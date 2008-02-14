!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that involves possible aliasing of actual parameters 
! due to multiple calls to the same procedure 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine head(x, f) 
       double precision :: x
       double precision :: f
       double precision t1, t2, t3

       t1=x*f                      ! AliasTag("t1") => (1,MUST)
                                   ! AliasTag("*x") => (2,MUST)
                                   ! AliasTag("x")  => (3,MUST)
                                   ! AliasTag("*f") => (4,MUST)
                                   ! AliasTag("f")  => (5,MUST)
                                   
       call bar(t1,t2)             ! AliasTag("t2") => (6,MUST)

                                   
       t3=f*30                     ! AliasTag("t3") => (7,MUST) 
                                   
       call bar(t3,t2)             ! AliasTag("t3") => (1,MUST)
                                   
       f=t1+t2               

       end subroutine

       subroutine bar(a,b)  
       double precision a,b

       b = a                       ! AliasTag("*a") => (1,MUST)
                                   ! AliasTag("a")  => (8,MUST)
                                   ! AliasTag("*b") => (6,MUST)
                                   ! AliasTag("b")  => (9,MUST)

       return
       end
