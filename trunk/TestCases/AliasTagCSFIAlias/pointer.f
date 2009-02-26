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
                     
           t = p     ! AliasTag("foo::*p") = ({2,3},MAY)
                     ! AliasTag("foo::t")  = (4,MUST)

           q = 5     ! AliasTag("foo:q") = (2,MUST)
                     
      end program
