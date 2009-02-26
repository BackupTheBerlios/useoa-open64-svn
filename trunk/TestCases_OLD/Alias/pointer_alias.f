
      subroutine foo
          double precision, pointer :: p
          double precision, target  :: t
          double precision          :: a
          p=>t    ! { p, 1 } , { (*p,t), 2 }
          a=p     ! { a, 3 } , { p, 1 } , { (*p,t), 2 }
      end subroutine
