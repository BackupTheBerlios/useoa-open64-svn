! PLM 07/26/06  Reference http://www.kcl.ac.uk/kis/support/cit/fortran/f2003_book_examples/ch0801.f90
! 

PROGRAM ch0801
IMPLICIT NONE
!
! Example of a Fortran program to calculate net pay
! given an employee's gross pay
!
REAL          :: Gross_wage, Net_wage, Tax
REAL          :: Tax_rate = 0.25
INTEGER       :: Personal_allowance = 4800
CHARACTER*60   :: Their_Name
  PRINT *,'Input employees name'

  !Reaching Constants: Their_Name = TOP
  READ *,Their_Name
  PRINT *,'Input Gross wage'

  !Reaching Constants: Gross_wage = TOP
  READ *,Gross_wage

  !Reaching Constant: Tax_rate = 0.25, Personal_allowance = 4800
  !Reaching Constant: Gross_wage = TOP, Tax = TOP
  Tax = (Gross_wage - Personal_allowance) * Tax_rate

  !Reaching Constants: Net_wage = TOP, Gross-wage = TOP, Tax = TOP
  Net_wage = Gross_wage - Tax

  !Reaching Constants: Their_Name = TOP
  PRINT *,'Employee: ',Their_Name

  !Reaching Constants: Gross_wage = TOP
  PRINT *,'Gross Pay: ', Gross_wage

  !Reaching Constants: Tax = TOP
  PRINT *,'Tax: ',Tax

  !Reaching Constants: Net_wage = TOP
  PRINT *,'Net Pay:',Net_wage
END PROGRAM ch0801
