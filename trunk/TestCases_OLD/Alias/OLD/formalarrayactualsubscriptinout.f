
      subroutine head(x)
       double precision, dimension(2), intent(inout) :: x
       integer i
       i=2
       call bar(x(i))
      end subroutine


