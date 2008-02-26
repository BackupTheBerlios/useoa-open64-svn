!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that involves possible aliasing of actual parameters 
! due to multiple calls to the same procedure  (Aliasing of Arrays)
!
! AliasPairs : 1. (*x(),*p(),*a)
!              2. (*y,*q(),b)
!      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Problem: Intutive analysis does not match with the actual output.
! AliasTags of head::*x, head::*x() does not overlap in the output of
! AliasTagFIAlias.      


        subroutine foo(a,b)
          double precision a
          double precision b

          b=a*2

        end subroutine

c$openad XXX Template ad_template   

        subroutine head(x,y)
          double precision, dimension(2) :: x
          double precision y
          double precision, dimension(2) :: p,q
          integer k,l
c$openad INDEPENDENT(x)

          call foo(x(k),y)        ! AliasTag("head::*x")   => ({1,2},MUST)
                                  ! AliasTag("head::*x()") => (2,MAY)   
                                  ! AliasTag("head::x")    => (3,MUST) 
                                  ! AliasTag("foo::*a")    => (2,MAY)
                                  ! AliasTag("foo::a")     => (4,MUST)
                                  
                                  ! AliasTag("head::*y")   => (5,MAY)
                                  ! AliasTag("head::y")    => (6,MUST)
                                  ! AliasTag("foo::*b")    => (5,MAY)
                                  ! AliasTag("foo::b")     => (7,MUST)
  

          call foo(p(k),q(l))     ! AliasTag("head::p()")  => (2,MAY)    
                                  ! AliasTag("head::p")    => ({8,2}, MUST)
                                  ! AliasTag("head::q()")  => (5,MAY)
                                  ! AliasTag("head::q")    => ((9,5), MUST)

c$openad DEPENDENT(y)
 
        end subroutine
