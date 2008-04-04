


       subroutine foo(m,n) 
 
         double precision, dimension(2) :: m 
         double precision :: n 
 
         n = m(i)*m(2)   
 
       end subroutine     
 
       subroutine head(x,y) 
 
         double precision, dimension(2) :: x,px 
         double precision :: y,py 
 
         call foo( x, y )
            ! Alias Analysis Output                      => AliasTagXAIF Output 
            ! AliasTag("foo::m")   => (1, MUST)          => ( 1, vTag(1), Must ) ) 
            ! AliasTag("foo::*m")  => ({7,8,9}, MAY)     => ( 2, vTag(7,8,9), May ) )  
            ! AliasTag("foo::*m()")=> (8, MAY)           => ( 3, vTag(8), May ) )     
            ! AliasTag("foo::x")   => (4, MUST)          => ( 4, vTag(4), Must ) ) 
            ! AliasTag("foo::*x")  => ({7,8,9}, MAY)     => ( 2, vTag(7,8,9), May ) ) 
 
 
            ! AliasTag("foo::n")   => (5, MUST)          => ( 5, vTag(5), Must ) ) 
            ! AliasTag("foo::*n")  => ({10,11}, MAY)     => ( 6, vTag(10,11), May ) ) 
            ! AliasTag("head::y")  => (7, MUST)          => ( 7, vTag(7), May ) ) 
            ! AliasTag("head::*y") => ({10,11}, MAY)     => ( 6, vTag(10,11), May ) ) 
            ! AliasTag("head::px") => (9, MUST)          => ( 8, vTag(9), May ) ) 
  
           ! AliasTag("head::py") => (11, MUST)         => ( 9, vTag(11), May ) ) 
 
 
 
       end subroutine 
