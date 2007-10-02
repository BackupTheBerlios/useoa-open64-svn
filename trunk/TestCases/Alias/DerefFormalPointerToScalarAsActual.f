

          subroutine foo(p) 
              double precision, target :: t 
              double precision, pointer :: p 
              p=>t 
              call bar(p) 
          end subroutine 
 
          subroutine bar(q) 
              double precision, pointer :: q 
 
          end subroutine 
 
