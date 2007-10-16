      PROGRAM test_timer
      DOUBLE PRECISION start_time, stop_time
      REAL adder
      INTEGER i
      adder = 1.0
      CALL timer(start_time)
      DO i = 1, 100000
         adder = adder + 1.0
      ENDDO
      CALL timer(stop_time)
      WRITE(*,10) i-1, stop_time - start_time
 10   FORMAT('Elapsed time (in seconds) for doing ',i6,' FLOPS: ',e10.4)
      STOP
      END
