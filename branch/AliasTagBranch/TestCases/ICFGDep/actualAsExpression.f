! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Expression as an actual parameter
!
! Note:    Except strictlyLocal all definitions are mayDefs.
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



      PROGRAM MAIN 
      INTEGER N,X,L 
      COMMON /GLOBALS/ N 
c$openad INDEPENDENT(L) 
 
      CALL SUB1(L+1)                    

c                    mUses                => mDefs
c                    ==============================
c                    [MAIN::L,            => MAIN::(L+1) which is temp]
c
c                    ImplicitRemoves:
c                    ================
c                    [ ]



      X=N 


c                    mUses                => mDefs
c                    ==============================
c                    [N                   => MAIN::X]
c
c                    ImplicitRemoves:
c                    ================
c                    [X]


c$openad DEPENDENT(X) 
      END 
 
      SUBROUTINE SUB1(F) 
      INTEGER N,F 
      COMMON /GLOBALS/ N 
      N=F 


c                    mUses                => mDefs
c                    ==============================
c                    [SUB1::*F,           => N ]
c
c                    ImplicitRemoves:
c                    ================
c                    [ ]

      END 
 



