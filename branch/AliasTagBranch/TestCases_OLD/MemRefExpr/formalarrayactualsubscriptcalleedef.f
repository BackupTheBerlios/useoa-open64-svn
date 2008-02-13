

      subroutine head(x) 
       double precision, dimension(2) :: x 
 
       call bar(x(i)) 
      end subroutine 
 
      subroutine bar(a) 
       double precision a 
       double precision b 
 
        a = b 
       return 
       end 
