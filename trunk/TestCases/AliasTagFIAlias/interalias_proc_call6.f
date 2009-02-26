!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that involves possible aliasing of actual parameters 
! due to multiple calls to the same procedure  (Aliasing of Scalars)
!
! Aliased MREs : 1. (t1,t3,*a)
!                2. (t2,*b)
!      
! Comments: Because of the Flow-Insensitivity, group (*a, t1,t3) get the
!           MayTag.
! FIXME: the comments next to the code are not correct, MMS 4/22/08
! FIXED: PLM 4/23/08
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine head(x, f) 
       double precision :: x
       double precision :: f
       double precision t1, t2, t3

       t1=x*f               
                                   
       call bar(t1,t2)            
                                   
       t3=f*30                    
                                   
       call bar(t3,t2)            
                                   
       f=t1+t2               

       end subroutine

       subroutine bar(a,b)  
       double precision a,b

       b = a                      

       return
       end



! AliasTagFIAlias Output
! =======================

! MemRefHandle => AliasTag
! ========================
! MemRefHandle(T1) = 7, Must
! MemRefHandle(X)  = 15, May
! MemRefHandle(F)  = 16, May
! MemRefHandle(T3) = 10, Must
! MemRefHandle(F)  = 16, May
! MemRefHandle(T2) = 9, Must
! MemRefHandle(B)  = (8,9), May
! MemRefHandle(A)  = (6,7,10), May

! MemRefExpr => AliasTag
! ======================
! NamedRef(X) => 3, Must, Local
! NamedRef(F) => 5, Must, Local
! NamedRef(T1)=> 7, Must, Local
! NamedRef(T2)=> 9, Must, Local
! NamedRef(T3)=> 10, Must, Local
! NamedRef(A) => 12, Must, Local
! NamedRef(B) => 14, Must, Local
! *NamedRef(X) => 15, May, Local
! *NamedRef(F) => 16, May, Local
! *NamedRef(A) => (6,7,10), May, Local
! *NamedRef(B) => (8,9), May, Local




