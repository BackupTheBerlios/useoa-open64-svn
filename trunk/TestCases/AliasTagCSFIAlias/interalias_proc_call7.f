!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that involves possible aliasing of actual parameters 
! due to multiple calls to the same procedure  (Aliasing of Arrays)
!
! AliasPairs : 1. (*x(),*p(),*a)
!              2. (*y,*q(),b)
!      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        subroutine foo(a,b)
          double precision a
          double precision b

          b=a*2

        end subroutine

c$openad XXX Template ad_template   

        subroutine head(x,y)
          double precision, dimension(2) :: x
          double precision y
          double precision, dimension(2) :: p,q
          integer k,l
c$openad INDEPENDENT(x)

          call foo(x(k),y)   

          call foo(p(k),q(l))

c$openad DEPENDENT(y)
 
        end subroutine


! AliasTagFIAlias
! ===============

! MemRefHandle => AliasTag
! ========================
! MemRefHandle(B) => 19
! MemRefHandle(A) => 18
! MemRefHandle(K) => 11
! MemRefHandle(Y) => 9
! MemRefHandle(L) => 13


! MemRefExpr => AliasTag
! ======================
! NamedRef(A) => 3, Must, Local
! NamedRef(B) => 5, Must, Local
! NamedRef(X) => 7, Must, Local
! NamedRef(Y) => 9, Must, Local
! NamedRef(K) => 11,Must, Local
! NamedRef(L) => 13,Must, Local
! NamedRef(P) => (14,15,18), May, Local
! NamedRef(Q) => (16,17,19), May, Local
! *NamedRef(A)=> (18), May, Local
! *NamedRef(B)=> (19), May, Local
! *NamedRef(X)=> (18,20), May, Local
! *NamedRef(Y)=> (19), May, Local
! NamedRef(P)[]=>(18), May, Local
! NamedRef(Q)[]=>(19), May, Local
! *NamedRef(X)[]=>(18), May, Local








