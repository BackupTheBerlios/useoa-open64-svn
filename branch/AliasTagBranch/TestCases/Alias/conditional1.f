! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! pointer p is pointing at unknown location on
! one path and then location r on the other path
! I dont know if this is a valid program
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
       
       program main
           double precision, pointer :: p
           double precision, target  :: r,t
           double precision :: x
           if ( x .ge. 5 ) then     ! {x,1}
                x=10
           else
                p=>r                ! {p,2}, { (*p,r), 3 }  
           endif 
                                    ! {p,2}, { (*p,r), 3 }, {x,1}
                                    
           t=p                      ! {t,4}, {p,2}, { (*p,r), 3 }, {x,1} 
       end program
