
      subroutine head(x)
       double precision x

       call bar(x)
      end subroutine

      subroutine bar(a)
       double precision, intent(out) :: a
       double precision b

       a = b
       return
       end


