! LinearityAnalysis for Automatic Differentiation
!
!Result Should be:
!<<a,a>,linear>, <<a,y>,nonlinear>, <<a,x>,nonlinear>
!<<y,y>,nonlinear>, <<y,x>,nonlinear>
!      
        program main
          double precision :: a, b, c, x, y, z
          a = 0.0
          b = 0.0
          c = 0.0
          x = 0.0
          y = 0.0
          z = 0.0

          a = 0
          y = x
          do i=1,3
            a = a + y
            y = y * x
          enddo
        end program main
        
