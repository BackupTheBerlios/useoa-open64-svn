! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
! Simple aliasing due to pointer
!
! AliasPairs: (*p,q)
!      
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
      
      program foo
           integer, pointer :: p
           integer, target  :: q
           integer :: t
           
           p=>q      ! AliasTag("foo::p")  = (1,MUST)
                     ! AliasTag("foo::q")  = (2,MUST)
                     
           t = p     ! AliasTag("foo::*p") = (2,MUST)
                     ! AliasTag("foo::t")  = (3,MUST)
                     
      end program
