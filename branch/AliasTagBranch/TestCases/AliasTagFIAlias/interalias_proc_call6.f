!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that involves possible aliasing of actual parameters 
! due to multiple calls to the same procedure 
!
! AliasPairs : 1. (t1,t3,*a)
!              2. (t2,*b)
!      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine head(x, f) 
       double precision :: x
       double precision :: f
       double precision t1, t2, t3

       t1=x*f               
                                   ! AliasTag("head::t1") => (1,MUST)
                                   ! AliasTag("head::*x") => (2,MUST)
                                   ! AliasTag("head::x")  => (3,MUST)
                                   ! AliasTag("head::*f") => (4,MUST)
                                   ! AliasTag("head::f")  => (5,MUST)
                                   
       call bar(t1,t2)            
                                   ! AliasTag("head::t2") => (6,MUST)
                                   ! AliasTag("bar::*a") => (1,MUST)
                                   ! AliasTag("bar::a")  => (8,MUST)
                                   ! AliasTag("bar::*b") => (6,MUST)
                                   ! AliasTag("bar::b")  => (9,MUST)


                                   
       t3=f*30                    
                                   ! AliasTag("head::t3") => (7,MUST) 
                                   
       call bar(t3,t2)            
                                   ! AliasTag("head::t3") => (1,MAY)
                                   ! AliasTag("head::t1") => (1,MAY)
                                   ! AliasTag("head::*a") => (1,MAY)
                                   
       f=t1+t2               

       end subroutine

       subroutine bar(a,b)  
       double precision a,b

       b = a                      

       return
       end
