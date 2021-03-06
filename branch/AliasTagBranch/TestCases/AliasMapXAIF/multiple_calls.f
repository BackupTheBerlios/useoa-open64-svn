! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   AliasMapXAIF
!                   ============
! Features:
!           1. Aliasing due to multiple function calls
!
! Testing :
!          [X] Virtual LocTuple range.
!          [X] Partial Flags
!
! Status /Issues:
!
! Note   : We dont have analysis to distinguish different array index access. 
!          Therefore, a(i) and a(2) gets the same AliasTags.
!
! Note: * Only StrictlyLocal memory references are Must, all other 
!         memory references are "May".
!
!       * All the MemRefHandles for which AliasTagsSet is empty
!          are Mapped to AliasMapXAIF setId = 0, [jean's Suggestion, 
!          April 2008].
!
!
! Author : Priyadarshini Malusare, Argonne National Laboratory, April 8th 2008
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! Example:
! =======

      subroutine foo(m,n) 
         double precision, dimension(2) :: m 
         double precision :: n 
         integer:: i,j
         n = m(i)*m(2)   
      end subroutine     
 
      subroutine head(x,y) 
         double precision, dimension(2) :: x,px 
         double precision :: y,py 
         call foo( x, y )
         call foo( px, py )     
      end subroutine 



! Analysis :
! =========


       ! ======== AliasMapXAIF Results ==============
       !
       !   [  MemRefHandle => SetId ]
       !   ===========================
       ! MemRefHandle("N")    => SetId(1)
       ! MemRefHandle("M(I)") => SetId(2)
       ! MemRefHandle("I")    => SetId(3)
       ! MemRefhandle("M(2)") => SetId(2)
       ! MemRefHandle("X")    => SetId(4)
       ! MemRefHandle("Y")    => SetId(5)
       ! MemRefHandle(&PX)    => SetId(0)
       ! MemRefHandle(&PY)    => SetId(0)

       !   [  SetId  =>  Virtual Address ]
       ! ==================================
       ! SetId(0) => { }
       ! SetId(1) => { LocTuple(14:14, May) }
       ! SetId(2) => { LocTuple(15:15, May) }
       ! SetId(3) => { LocTuple(16:16, May) }
       ! SetId(4) => { LocTuple(9:9,   Must) }
       ! SetId(5) => { LocTuple(11:11, Must) }


