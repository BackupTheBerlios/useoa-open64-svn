!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that involves possible aliasing of actual parameters
! due to multiple calls to the same procedure  (Aliasing of arrays)
!
! AliasPairs : 1. (t1,t3,*a)
!              2. (t2,*b)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Problem: Intutive Analysis does not match with the AliasTagFIAlias
! output because AliasTag for (head::px, head::*x and foo::*x) remain 
! MUST in the AliasTagFIAlias output.      


         subroutine foo(x,y) 
           double precision, dimension(2) :: x
           double precision y

           y=x(1)*x(2)    ! AliasTag("foo::x()") => (4,MAY)

         end subroutine

         subroutine head(x,y) 
           double precision, dimension(2) :: x, px
           double precision y, py
           
           px(1)=1.0       ! AliasTag("head::px()")   => (1,MAY) 
                           ! AliasTag("head::px")     => ({1,2}, MUST)

           px(2)=2.0       

           call foo(x,y)   ! AliasTag("head::*x")  => ({3,4}, MUST)
                           ! AliasTag("head::x")   => (5, MUST)
                           ! AliasTag("head::*y")  => (6,MUST)
                           ! AliasTag("head::y")   => (7,MUST)
                           ! AliasTag("foo::*x")   => ({3,4},MUST)
                           ! AliasTag("foo::x")    => (8,MUST)
                           

                           

           call foo(px,py)     ! AliasTag("head::px") => ((3,4), MAY)
                               ! AliasTag("head::*x") => ((3,4), MAY)
                               ! AliasTag("foo::*x")  => ((3,4), MAY)
           
         end subroutine


