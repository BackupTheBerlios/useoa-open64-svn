! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Aliasing in the presence of intraprocedural control flow      
!      
! Aliased MREs : (*q,r,s)       
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      

      program main
          integer, pointer :: q
          integer, target  :: r,s
          integer :: t
          
          if( t < 5 ) then     ! AliasTag("t") => (1,MUST)
              
              q=>r             ! AliasTag("q") => (2,MUST)
              r=24             ! AliasTag("r") => (3,MUST)
                               
          else 
              
              q=>s             ! AliasTag("q") => (2,MUST)           
              s=42             ! AliasTag("s") => (4,MUST)
              
          endif

          t=q                  ! AliasTag(*q) => ({3,4,5},MAY)
          
      end program
