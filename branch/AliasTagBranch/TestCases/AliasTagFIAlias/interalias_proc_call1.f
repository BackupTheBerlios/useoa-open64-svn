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

       t1=x*f                     ! AliasTag("t1") => (1,MUST)
                                  ! AliasTag("*x") => (2,MUST)
                                  ! AliasTag("x")  => (3,MUST)
                                  ! AliasTag("*f") => (4,MUST)
                                  ! AliasTag("f")  => (5,MUST)
                                  
                                  
       call bar(t1,t1)        

                                  
                                  
       t3=f*30                    ! AliasTag("t3") => (6,MUST)
                                  
       f=t1+t2                    ! AliasTag("t2") => (7,MUST) 
                                  
       end subroutine

       subroutine bar(a,b) 
                                  
       double precision a,b

       b = a                      ! AliasTag("*a") => (1,MUST)
                                  ! AliasTag("a")  => (8,MUST)
                                  ! AliasTag("*b") => (1,MUST)
                                  ! AliasTag("b")  => (9,MUST)

       return
       end
