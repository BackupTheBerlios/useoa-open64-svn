!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that involves possible aliasing of actual parameters
! due to multiple calls to the same procedure  (Aliasing of arrays)
!
! AliasPairs : 1. (t1,t3,*a)
!              2. (t2,*b)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Problem: Intutive Analysis does not match with the AliasTagFIAlias
! output because AliasTag for (head::px, head::*x and foo::*x) remain 
! MUST in the AliasTagFIAlias output.      


         subroutine foo(a,b) 
           double precision, dimension(2) :: a
           double precision b

           b=a(1)*a(2)    ! AliasTag("foo::x()") => (4,MAY)

         end subroutine

         subroutine head(x,y) 
           double precision, dimension(2) :: x, px
           double precision y, py
           
           call foo(x,y)   ! AliasTag("head::*x")  => ({1,2}, MUST)
                           ! AliasTag("head::x")   => (3, MUST)
                           ! AliasTag("foo::*x()") => {2, MAY}
                           ! AliasTag("foo::*x")   => {{1,2}, MUST}
                           ! AliasTag("foo::x")    => (4, MUST)
                           
                           ! AliasTag("head::*y")  => (5,MUST)
                           ! AliasTag("head::y")   => (6,MUST)
                           ! AliasTag("foo::*y")   => (5,MUST)
                           ! AliasTag("foo::y")    => (7,MUST)

                           

           call foo(px,py)     ! AliasTag("head::px") => ((1,2), MAY)
                               ! AliasTag("foo::py")  => (5, MUST)

         end subroutine



! MemRefHandle => AliasTags
! =========================
! MemRefHandle(B)   => (12,13), May
! MemRefHandle(A(1))=> (14),    May
! MemRefHandle(A(2))=> (14),    May
! MemRefHandle(X)   => (7),     Must
! MemRefHandle(Y)   => (9),     Must


! MemRefExpr => AliasTags
! =======================
! NamedRef(A) => (3),        Must, Local
! NamedRef(B) => (5),        Must, Local
! NamedRef(X) => (7),        Must, Local
! NamedRef(Y) => (9),        Must, Local
! NamedRef(PX)=> (10,11,14), May,  Local
! NamedRef(PY)=> (13),       Must, Local
! *NamedRef(A)=> (10,11,14), May,  Local
! *NamedRef(B)=> (12,13),    May,  Local
! *NamedRef(X)=> (10,11),    May,  Local
! *NamedRef(Y)=> (12,13),    May,  Local
! *NamedRef(A)[]=>(14)       May, Local











