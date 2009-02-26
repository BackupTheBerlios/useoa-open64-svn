
      function stringcmp(s,t) result(res)
        character( * ) :: s
        character( * ) :: t
        integer :: res
        integer :: i
        i = 0

!          MOD()  = (),       LMOD() = (i)
!          REF()  = (),       LREF() = ()
!          USE()  = (),       LUSE() = ()
!          DEF()  = (),       LDEF() = ()
        
        do while (s(i) .eq. t(i))

!          MOD()  = (),       LMOD() = ()
!          REF()  = (),       LREF() = (s,t)
!          USE()  = (),       LUSE() = (s(),t())
!          DEF()  = (),       LDEF() = ()

!          if (s(i) .ne. '\0') then

!          MOD()  = (),       LMOD() = ()
!          REF()  = (),       LREF() = (s)
!          USE()  = (),       LUSE() = (s())
!          DEF()  = (),       LDEF() = ()

            res = 0

!          MOD()  = (),       LMOD() = (res)
!          REF()  = (),       LREF() = ()
!          USE()  = (),       LUSE() = ()
!          DEF()  = (),       LDEF() = ()

!          endif
          i = i + 1

!          MOD()  = (),       LMOD() = (i)
!          REF()  = (),       LREF() = ()
!          USE()  = (),       LUSE() = (i)
!          DEF()  = (),       LDEF() = ()

        end do
        res = 1

      end function stringcmp
      
      program main
        character( * ), parameter :: a = 'source'
        character( * ), parameter :: b = 'dest'
        integer :: retVal
        retVal = 0

        retVal = stringcmp(a,b)

!          MOD()  = (),          LMOD() = ()
!          REF()  = (a,b),       LREF() = (a,b)
!          USE()  = (*a,*b),     LUSE() = (*a,*b)
!          DEF()  = (),          LDEF() = ()

      end program main
