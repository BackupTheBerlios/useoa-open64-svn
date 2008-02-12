
      ! alias analysis results along conditional paths
       
       program main
           double precision, pointer :: p,q
           double precision, target  :: r,s,t,x
           r=10
           s=20
           t=30
           x=20

           p=>x
           if ( x .ge. 5 ) then
                q=p
           else
                q=>s
           endif 
           q=>t

       end program
