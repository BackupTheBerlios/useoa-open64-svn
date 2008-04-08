! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   AliasMapXAIF
!                   ============
! Features:
!           1. Pointer Aliasing with local variables
!
! Testing :
!          [X] Virtual LocTuple range.
!          [X] Partial Flags
!
! Status : MemRefHandle for P and *P. Need to talk to Michelle.
!
! Note   : Following Analysis does not take into consideration of 
!          srictly local modification algorithm. For the reference
!          I would write what Michelle and I discussed in the meeting
!          
!          AliasTag Results with Strictly Local Modiciations for the following example
!          - MemRefHandle(q) => (2, Must)
!          - MemRefHandle(*p) =>  ((2,3), May)
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



! Example :
! ==========

        program foo 
 
         integer, pointer :: p 
         integer, target  :: q 
         integer          :: t 
 
         p=>q            
         t = p   
  
       end program 




! Analysis :
! =========

       ! ========= AliasTagFIAlias Results =========
       ! 
       !   [ MemRefExpr => AliasTag ]
       !   ==========================
       ! 
       ! NamedRef("p")          => (1, Must)
       ! Deref(NamedRef("p"))   => (2, May)
       ! NamedRef("q")          => (2, May)
       ! NamedRef("t")          => (3, Must)
       !
       !   [ MemRefHandle => AliasTag]
       !
       ! MemRefHandle("p")      => (1, Must)
       ! MemRefHandle("p")      => (2, Must)
       ! MemRefHandle("q")      => (2, Must)
       ! MemRefHandle("t")      => (3, Must)





       ! ======== AliasMapXAIF Results ==============
       !
       !   [  MemRefHandle => SetId ]
       !   ===========================
       ! MemRefHandle("p")   => SetId(1)
       ! MemRefHamdle("*p")  => SetId(2)
       ! MemRefHandle("q")   => SetId(2)
       ! MemRefHandle("t")   => SetId(3)
       ! 
       !   [  SetId  =>  Virtual Address ]
       ! ==================================
       ! SetId(1) => { LocTuple(1:1, Must) }
       ! SetId(2) => { LocTuple(2:2, May} }
       ! SetId(3) => { LocTuple(3:3, Must)}

