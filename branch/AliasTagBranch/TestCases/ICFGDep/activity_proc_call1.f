!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! formal reference parameters are pointers, are considered as mayDefs if 
! their target is unknown. e.g. Parameter 'f'.      
! mayDefs are not killed at the program point and thus not included in
! the ImplicitRemoves. 
!
!
! formal reference parameters for which the target is known are treated
! as MustDefs. e.g. Parameter 'B'.
! MustDefs are considered as killset at the program point and included in 
! the ImplicitRemoves      
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      

       subroutine head(x, f) 
       double precision :: x
       double precision :: f
       double precision t1, t2, t3


       t1=x*f
       
c                           mUses           => mDefs
c                           ========================
c                           [1: head::x]    => [2: head::t1, 
c                                                  bar::*a ]
c
c                           [3: head::*f]   => [2: head::t1, 
c                                                  bar::*a ]
c      
c                           ImplicitRemoves:   
c                           ===============
c                           [2: head::t1,
c                               bar::*a ]  

       


       call bar(t1,t2)




       t3=f*30
       
c                           mUses          => mDefs
c                           ==========================
c                           [3: head::*f]  => [4: head::t3]
c
c                           ImplicitRemoves:
c                           ===============
c                           [4: head::t3]



       f=t1+t2

c                           mUses            => mDefs
c                           ===================== 
c                           [2: head::t1     => [3: head::*f]
c                               bar::*a ]                   
c
c                           [4: head::t2,    => [3: head::*f]
c                               bar::*b ]  

c                           ImplicitRemoves: 
c                           ================
c      


       end subroutine




       subroutine bar(a,b)
       double precision a,b
       b = a
       
c                           mUses            => mDefs
c                           ======================
c                           [2: head::t1     => [4: head::t2
c                               bar::*a             bar::*b] 

c                           ImplicitRemoves:
c                           ================
c                           [4: head::t2,
c                               bar::*b]


       return

       end subroutine



