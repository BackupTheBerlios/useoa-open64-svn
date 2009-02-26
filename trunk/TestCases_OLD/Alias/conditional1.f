
      ! alias analysis results along conditional paths
      ! AliasPairs:  (r, s, *q) => 3
      
       program main
           double precision, pointer :: p,q
           double precision, target  :: r,s,t,x
           
           if ( x .ge. 5 ) then        ! AliasTag("x") => {1, MUST}
               
               
                q=>r                   ! AliasTag("q") => {2, MUST}
                                       ! AliasTag("r") => {3, MUST}
                                       
           else
                q=>s                   ! AliasTag("q") => {2, MUST}
                                       ! AliasTag("s") => {4, MUST}
                
           endif
           
           t=q                         ! AliasTag("t") => {4, MUST}
                                       ! AliasTag("*q") =>{{3,4}, MUST}

       end program
