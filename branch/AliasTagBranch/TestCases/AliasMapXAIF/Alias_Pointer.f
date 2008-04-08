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
! Status /Issues: 
!          I am not sure if this is a valid question or not.
!
!          OpenAnalysis knows the difference between MemRefHandle for P and *P ?
!          How would OpenADFortTk know about it ? It needs to do
!          some processing.
!
!          Jean says he would find out that information based on whirl.
!          - i.e. pstore is always P
!          - everything else is *P  Is that true ?
!
! Note   : Following Analysis does not take into consideration of 
!          srictly local modification algorithm. For the reference
!          I would write what Michelle and I discussed in the meeting
!          
!          AliasTag Results with Strictly Local Modiciations 
!          for the following example
!          - MemRefHandle(q) => (2, Must)
!          - MemRefHandle(*p) =>  ((2,3), May)
!         
!          - At this point everything is May
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



! Example :
! ==========

        program foo 
 
         integer, pointer :: p 
         integer, target  :: q 
         integer          :: t 
 
         p=>q            
         t=p   
  
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
       ! MemRefHandle("p")      => (1, Must)  , this is p
       ! MemRefHandle("p")      => (2, Must)  , this is *p
       ! MemRefHandle("t")      => (3, Must)





       ! ======== AliasMapXAIF Results ==============
       !
       !   [  MemRefHandle => SetId ]
       !   ===========================
       ! MemRefHandle("p")   => SetId(1)  , this is p
       ! MemRefHamdle("p")   => SetId(2)  , this is *p
       ! MemRefHandle("t")   => SetId(3)
       ! 
       !   [  SetId  =>  Virtual Address ]
       ! ==================================
       ! SetId(1) => { LocTuple(1:1, Must) }
       ! SetId(2) => { LocTuple(2:2, May} }
       ! SetId(3) => { LocTuple(3:3, Must)}

