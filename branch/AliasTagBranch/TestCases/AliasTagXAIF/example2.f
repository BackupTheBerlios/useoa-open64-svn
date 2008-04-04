


        program foo 
 
         integer, pointer :: p 
         integer, target  :: q 
         integer          :: t 
 
         p=>q            
          ! Alias Analysis Output                      => AliasTagXAIF Output 
          ! AliasTag("foo::p")    => (1, Must)         => ( 1, vTag(1), Must ) ) 
          ! AliasTag("foo::q")    => (2, Must)         => ( 2, vTag(2), Must ) ) 
 
         t = p   
         ! AliasTag("foo::*p")   => ((2,3), May)      => ( 3, vTag(2,3), May ) ) 
         ! AliasTag("foo::t")    => (4, Must)         => ( 4, vTag(4), Must ) ) 
  
       end program 

