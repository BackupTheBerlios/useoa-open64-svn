
      subroutine head(x) 
       double precision, dimension(2), intent(out) :: x 
       integer i 
       i=2 
       call bar(x(i)) 
      end subroutine 
