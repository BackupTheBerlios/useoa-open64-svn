
       ! Logical AND
c$openad XXX Template ad_template.f
       subroutine head(x,y)
         double precision, dimension(1), intent(inout) :: x
         double precision, dimension(1), intent(inout) :: y
         logical l
c$openad INDEPENDENT(x)
         l = .true.
         if (l .and. x(1) .gt. 0.0D0) then  ! l = .true.
             y(1)=x(1)*4                    ! StmtHandle(0)
         else
             y(1)=x(1)*2                    ! Stmthandle(0)
         end if
c$openad DEPENDENT(y)
       end subroutine
