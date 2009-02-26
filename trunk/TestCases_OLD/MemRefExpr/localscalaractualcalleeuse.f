
      subroutine head() 
       double precision x 
 
       call bar(x) 
      end subroutine 
 
      subroutine bar(a) 
       double precision a 
       double precision b 
 
       b = a 
       return 
       end 
