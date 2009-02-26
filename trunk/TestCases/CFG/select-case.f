! Reference:
! http://www.cs.mtu.edu/~shene/COURSES/cs201/NOTES/chap03/grade-1.html


        PROGRAM  LetterGrade
          IMPLICIT   NONE

          REAL              :: Mark1, Mark2, Mark3
          REAL              :: Average
          CHARACTER(LEN=2)  :: Grade

          Average = (Mark1 + Mark2 + Mark3) / 3.0

          SELECT CASE (NINT(Average))     ! round Average before use
              CASE (:59)                   ! <= 59 -------------> F
                 Grade = 'F '
              CASE (60:64)                 ! >= 60 and <= 64 ---> D
                 Grade = 'D '
              CASE (65:69)                 ! >= 65 and <= 69 ---> CD
                 Grade = 'CD'
              CASE (70:74)                 ! >= 70 and <= 74 ---> C
                 Grade = 'C '
              CASE (75:79)                 ! >= 75 and <= 79 ---> BC
                 Grade = 'BC'
              CASE (80:84)                 ! >= 80 and <= 84 ---> B
                 Grade = 'B '
              CASE (85:89)                 ! >= 84 and <= 89 ---> AB
                 Grade = 'AB'
              CASE DEFAULT                 ! >= 90 -------------> A
                 Grade = 'A '
          END SELECT

        END PROGRAM

