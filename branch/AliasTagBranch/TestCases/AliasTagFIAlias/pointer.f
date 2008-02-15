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
           
           p=>q      ! AliasTag("p")  = (1,MUST)
                     ! AliasTag("q")  = (2,MUST)
                     
           t = p     ! AliasTag("*p") = (2,MUST)
                     ! AliasTag("t")  = (3,MUST)
                     
      end program
