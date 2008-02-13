
      ! alias analysis results along conditional paths
      ! AliasPairs:  (*p,x) => 2,   (*q,s,t) => 4
      
       program main
           double precision, pointer :: p,q
           double precision, target  :: r,s,t,x
           
           p=>x                        ! (p,1), (x,2)
           
           q=>t                        ! (q,3), (t,4)
           
           if ( x .ge. 5 ) then        ! (x,2)
               
                q=p                    ! (*q,4), (*p,2)
           else
                q=>s                   ! (q,3), (s,4)
           
           endif 
           
           t=q                         ! (t,4), (*q,4)

       end program
