

 
       program main 
 
         integer, pointer :: q 
         integer, target  :: r,s 
         integer          :: t 
 
         ! Alias Analysis Output                      => AliasTagXAIF Output 
 
         if ( t < 5 ) then    
         ! AliasTag("t")       => (1, Must)           => ( 1, vTag(1), Must ) 
   
              q=>r
         ! AliasTag("q")       => (2, Must)           => ( 2, vTag(2), Must ) 
         ! AliasTag("r")       => (3, Must)           => ( 3, vTag(3), Must ) 
         else  
  
              q=>s   
         ! AliasTag("s")       => (4, Must)           => ( 4, vTag(4), Must ) 
 
         end if 
 
         t=q   
        ! AliasTag("*q")      => ((3,4,5), May)      => ( 5, vTag(3,4,5), May ) 
 
      end program 
 


