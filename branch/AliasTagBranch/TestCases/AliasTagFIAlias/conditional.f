! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Aliasing in the presence of intraprocedural control flow      
!      
! AliasPairs : (*q,r,s)       
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      

      program main
          integer, pointer :: q
          integer, target  :: r,s
          integer :: t
          
          if( t < 5 ) then     ! AliasTag("t") => (1,MUST)
              
              q=>r             ! AliasTag("q") => (2,MUST)
                               ! AliasTag("r") => (3,MUST)
                               
          else 
              
              q=>s             ! AliasTag("s") => (4,MUST)
              
          endif

          t=q                  ! AliasTag(*q) => (3,MAY)
                               ! AliasTag(r)  => (3,MAY)
                               ! AliasTag(s)  => (3,MAY)
                               ! AliasTag(t)  => (1,MUST)
          
      end program
