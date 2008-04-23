!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that involves possible aliasing of actual parameters 
! due to multiple calls to the same procedure  (Aliasing of Scalars)
!
! Aliased MREs : 1. (t1,t3,*a)
!                2. (t2,*b)
!      
! Comments: Because of the Flow-Insensitivity, group (*a, t1,t3) get the
!           MayTag.
! FIXME: the comments next to the code are not correct, MMS 4/22/08
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine head(x, f) 
       double precision :: x
       double precision :: f
       double precision t1, t2, t3

       t1=x*f               
                                   ! AliasTag("head::t1") => (1,MUST)
                                   ! AliasTag("head::*x") => (2,MAY)
                                   ! AliasTag("head::x")  => (3,MUST)
                                   ! AliasTag("head::*f") => (4,MAY)
                                   ! AliasTag("head::f")  => (5,MUST)
                                   
       call bar(t1,t2)            
                                   
                                   ! AliasTag("bar::*a") => (1,MAY)
                                   ! AliasTag("bar::a")  => (8,MUST)
                                   ! AliasTag("bar::*b") => (6,MAY)
                                   ! AliasTag("bar::b")  => (9,MUST)


                                   
       t3=f*30                    
                                   ! AliasTag("head::t3") => (7,MUST) 
                                   
       call bar(t3,t2)            
                                   ! AliasTag("head::t3") => (1,MAY)
                                   
       f=t1+t2               

       end subroutine

       subroutine bar(a,b)  
       double precision a,b

       b = a                      

       return
       end
