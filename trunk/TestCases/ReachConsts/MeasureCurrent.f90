! PLM 07/26/06  Reference http://www.kcl.ac.uk/kis/support/cit/fortran/f2003_book_examples/ch1006.f90


PROGRAM C1006
IMPLICIT NONE
REAL , DIMENSION(-20:20) :: Current 
REAL :: Resistance
INTEGER :: Voltage 
  PRINT *,' Type in the resistance'
  READ *, Resistance

  !Reaching Constants: Voltage = TOP
  DO  Voltage = -20,20
   
    !Reaching Constant Voltage = BOTTOM, Resistance = TOP, Current(*) = TOP
    Current(Voltage)=Voltage/Resistance

    !Reaching Constants: Voltage = BOTTOM, Current(*)  = TOP
    PRINT *, Voltage, ' ', Current(Voltage)
  ENDDO
END PROGRAM C1006
