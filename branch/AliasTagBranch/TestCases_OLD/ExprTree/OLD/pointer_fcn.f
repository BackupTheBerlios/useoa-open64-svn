
      module definition
         INTERFACE
            FUNCTION POINTER_FCN(X)
              REAL X
              REAL, POINTER :: POINTER_FCN
            END FUNCTION
         END INTERFACE
      end module

      subroutine pointer_assignment
         real, pointer :: p
         real A
         p=>POINTER_FCN(A)
      end subroutine
