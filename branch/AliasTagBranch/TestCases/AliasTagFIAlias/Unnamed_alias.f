! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Aliasing of Unnamed and Named References due to multiple 
! Function calls.      
!
! AliasPairs: 1. (i, UnNamedRef(2), *a1)
!             2. (UnnamedRef(i+j), j, *a2))      
!      
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      



       subroutine foo(a1)  
         integer a1
         print *, a1   
       end subroutine

       subroutine bar(a2)  
         integer a2
         print *, a2   
       end subroutine

       program head
         integer i,j
         
         i=1               ! AliasTag("head::i")  => (1,MUST) 
         
         
         j=4               ! AliasTag("head::j")  => (2,MUST)
         
         
         call foo(i)       ! AliasTag("foo::*a1")  => (1,MUST)
                           ! AliasTag("foo::a1")   => (3,MUST)
                           
                           
         call foo(2)       ! AliasTag("head::Unnamed(2)") => (1,MAY)
                           ! AliasTag("head::i")          => (1,MAY)
                           ! AliasTag("foo::*a1")         => (1,MAY)

                           
         call foo(i+j)     ! AliasTag("head::Unnamed(i+j)") => (1,MAY)
         
         
         call bar(j)       ! AliasTag("bar::*a2")   => (2,MUST)
                           ! AliasTag("bar::a2")    => (4,MUST)
         
         
         call bar(3)       ! AliasTag("head::Unnamed(3)")  => (2,MAY)
                           ! AliasTag("head::j")           => (2,MAY)
                           ! AliasTag("bar::*a2")          => (2,MAY)
         
       end
