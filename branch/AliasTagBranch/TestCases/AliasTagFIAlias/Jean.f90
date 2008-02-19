

         subroutine foo(x,y) 
           double precision, dimension(2) :: x
           double precision y

           y=x(1)*x(2)    ! AliasTag("foo::x()") => (4,MAY)

         end subroutine

         subroutine head(x,y) 
           double precision, dimension(2) :: x, px
           double precision y, py
           
           px(1)=1.0       ! AliasTag("head::px()")   => (1,MUST) 
                           ! AliasTag("head::px")     => ({1,2}, MUST)

           px(2)=2.0       

           call foo(x,y)   ! AliasTag("head::*x")  => ({3,4}, MUST)
                           ! AliasTag("head::x")   => (4, MUST)
                           ! AliasTag("head::*y")  => (5,MUST)
                           ! AliasTag("head::y")   => (6,MUST)
                           ! AliasTag("foo::*x")   => ({3,4},MUST)
                           ! AliasTag("foo::x")    => (7,MUST)
                           

                           

           call foo(px,py)     ! AliasTag("head::px") => ((3,4), MAY)
                               ! AliasTag("head::px") => (3, MAY)  
                               ! AliasTag("head::*x") => ((3,4), MAY)
                               ! AliasTag("foo::*x")  => ((3,4), MAY)
           
         end subroutine


