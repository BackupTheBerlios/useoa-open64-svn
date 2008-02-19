!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that involves aliasing of a reference parameter to a 
! global variable.
!
! AliasPairs: 1. (t1,*a)  
!             2. (g1,*b)      
!             3. (g,*c)      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       program testing
       double precision g,g1
       common /cpad/ g,g1
       double precision t1, t2
       call head(t1, t2)                 ! AliasTag("testing::t1") => (1,MUST)
                                         ! AliasTag("testing::t2") => (2,MUST)
                                         
       end

       subroutine head(x, f)           
       common /cpad/ g,g1 
       double precision :: x
       double precision :: f
       double precision t1, t2, t3

       t1=x*f                            ! AliasTag("head::t1") => (3,MUST)
                                         ! AliasTag("head::*x") => (4,MUST)
                                         ! AliasTag("head::x")  => (5,MUST)
                                         ! AliasTag("head::*f") => (6,MUST)
                                         ! AliasTag("head::f")  => (7,MUST)

                                         
       g=1.0                             ! AliasTag("testing::g")  => (8,MUST)
       
       g1=2.0                            ! AliasTag("testing::g1") => (9,MUST)
       
       call bar(t1,g1,g)                 ! AliasTag("bar::*a") => (3,MUST)
                                         ! AliasTag("bar::a")  => (10,MUST)
                                         ! AliasTag("bar::*c") => (8,MUST)
                                         ! AliasTag("bar::c")  => (11,MUST)
                                         ! AliasTag("bar::*b") => (9,MUST)
                                         ! AliasTag("bar::b")  => (12,MUST)

                                         
       t3=f*30*g                         ! AliasTag("head::t3") => (13,MUST)
                                         
       f=t1+t2                           ! AliasTag("head::t2") => (14,MUST)

       end subroutine

       subroutine bar(a,b,c)             
                                         
       common /cpad/ g,g1
       double precision a,b,c

       b = a + g + g1 + c               
       
       return
       end

