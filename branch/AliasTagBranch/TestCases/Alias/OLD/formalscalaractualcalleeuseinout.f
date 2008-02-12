
      subroutine head(x)
       double precision x

       call bar(x)
      end subroutine

      subroutine bar(a)
       double precision, intent(inout) :: a
       double precision b

       b = a
       return
       end


