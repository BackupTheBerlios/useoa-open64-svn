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
! Note:  To Date April 8th 2008, all memory references are "May".
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


       ! ======== AliasTagFIAlias =================
       ! MemRefHandle("N")    => AliasTagResults(8, May)
       ! MemRefHandle("M(I)") => AliasTagResults(9, May)
       ! MemRefHandle("I")    => AliastagResults(4 May)
       ! MemRefhandle("M(2)") => AliasTagResults(9, May)
       ! MemRefHandle("X")    => AliasTagResults(5, May)
       ! MemRefHandle("Y")    => AliasTagResults(6, May)

       ! ======== AliasMapXAIF Results ==============
       !
       !   [  MemRefHandle => SetId ]
       !   ===========================
       ! MemRefHandle("N")    => SetId(1)
       ! MemRefHandle("M(I)") => SetId(2)
       ! MemRefHandle("I")    => SetId(3)
       ! MemRefhandle("M(2)") => SetId(4)
       ! MemRefHandle("X")    => SetId(5)
       ! MemRefHandle("Y")    => SetId(6)


       !   [  SetId  =>  Virtual Address ]
       ! ==================================
       ! SetId(1) => { LocTuple(8:8, May) }
       ! SetId(2) => { LocTuple(9:9, May) }
       ! SetId(3) => { LocTuple(4:4, May) }
       ! SetId(4) => { LocTuple(9:9, May) }
       ! SetId(5) => { LocTuple(5:5, May) }
       ! Setid(6) => { LocTuple(6:6, May) }


