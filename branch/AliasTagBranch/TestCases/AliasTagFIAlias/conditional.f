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
                               ! AliasTag("r") => (3,MAY)
                               
          else 
              
              q=>s             ! AliasTag("s") => (4,MAY)
              
          endif

          t=q                  ! AliasTag(*q) => (3,MAY)
          
      end program
