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
         print *, a1       ! AliasTag("*a1") => (1,MAY)
                           ! AliasTag("a1")  => (4,MUST)
       end subroutine

       subroutine bar(a2)  
         integer a2
         print *, a2       ! AliasTag("*a2") => (2,MAY)
                           ! AliasTag("a2")  => (3,MUST)
       end subroutine

       program head
         integer i,j
         
         i=1               ! AliasTag("i")            => (1,MAY) 
         
         j=4               ! AliasTag("j")            => (2,MAY)
         
         call foo(i) 
         
         call foo(2)       ! AliasTag("Unnamed(2)")   => (1,MAY)

         call foo(i+j)     ! AliasTag("Unnamed(i+j)") => (1,MAY)
         
         call bar(j)  
         
         call bar(3)       ! AliasTag("Unnamed(3)")   => (2,MAY)
         
       end
