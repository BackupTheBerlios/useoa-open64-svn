
      function stringcmp(s,t) result(res)
        character( * ) :: s
        character( * ) :: t
        integer :: res
        integer :: i
        i = 0
        do while (s(i) .eq. t(i))
          if (s(i) .ne. '\0') then
            res = 0
            stop
          endif
          i = i + 1
        enddo
        res = 1

      end function stringcmp
      
      program main
        character( * ), parameter :: a = 'source'
        character( * ), parameter :: b = 'dest'
        integer :: retVal
        retVal = 0

        retVal = stringcmp(a,b)
      end program main
