!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that involves aliasing of a reference parameter to a 
! global variable.
!
! AliasPairs: 1. (t1,a)  
!             2. (g1,b)      
!             3. (g,c)      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       program testing
       double precision g,g1
       common /cpad/ g,g1
       double precision t1, t2
       call head(t1, t2)                 ! AliasTag("t1") => (1,MUST)
                                         ! AliasTag("t2") => (2,MUST)
                                         
       end

       subroutine head(x, f)           
       common /cpad/ g,g1 
       double precision :: x
       double precision :: f
       double precision t1, t2, t3

       t1=x*f                            ! AliasTag("t1") => (3,MUST)
                                         ! AliasTag("*x") => (4,MUST)
                                         ! AliasTag("x")  => (5,MUST)
                                         ! AliasTag("*f") => (6,MUST)
                                         ! AliasTag("f")  => (7,MUST)

                                         
       g=1.0                             ! AliasTag("g")  => (8,MUST)
       
       g1=2.0                            ! AliasTag("g1") => (9,MUST)
       
       call bar(t1,g1,g)               

                                         
       t3=f*30*g                         ! AliasTag("t3") => (10,MUST)
                                         
       f=t1+t2                           ! AliasTag("t2") => (11,MUST)

       end subroutine

       subroutine bar(a,b,c)             
                                         
       common /cpad/ g,g1
       double precision a,b,c

       b = a + g + g1 + c                ! AliasTag("g")  => (8,MUST)
                                         ! AliasTag("g1") => (9,MUST)
                                         ! AliasTag("*a") => (3,MUST)
                                         ! AliasTag("a")  => (12,MUST)
                                         ! AliasTag("*c") => (8,MUST)
                                         ! AliasTag("c")  => (13,MUST)
                                         ! AliasTag("*b") => (9,MUST)
                                         ! AliasTag("b")  => (14,MUST)
       
       return
       end

