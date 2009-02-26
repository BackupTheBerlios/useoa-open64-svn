
      subroutine head(x)
       double precision, dimension(10) :: x

       call bar(x)
      end subroutine

      subroutine bar(a)
       double precision, dimension(10) :: a
       double precision b

       b = a(2:4)
       return
       end


