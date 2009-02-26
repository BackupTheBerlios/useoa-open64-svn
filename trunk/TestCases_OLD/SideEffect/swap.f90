!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Swap Function.
!   a[] is pass by reference
!   i, i+1 are pass by value
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        subroutine swap(v,i,j)
          integer v(5), i, j
          integer temp

          temp = v(i)
          v(i)=v(j)
          v(j)=temp
        end subroutine swap

        program main
          integer :: a(5)
          integer :: i, j

          do i=1,4
            call swap(a,i,i+1)
          end do
        end program main


