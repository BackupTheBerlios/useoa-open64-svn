


      module globals

       double precision aGlobal
       
      end module


      
      subroutine foo(a,b) 

       double precision, dimension(2) :: a
       double precision b
       b=a(1)*a(2)
       
c                        mUses :            =>  mDefs :
c                        ============================================
c                        [1 : foo::*a           [2 : foo::*b, 
c                             foo::*a()              global::aGlobal,
c                             head::*x,              bar::*q ]
c                             bar::*p ]  
c            
c
c                        ImplicitRemoves: 
c                        ======================
c                        [ 2 : foo::*b, 
c                              global::aGlobal, 
c                              bar::*q ]

      end subroutine


      
      subroutine head(x,y) 

          use globals

           double precision, dimension(2) :: x
           double precision, dimension(1) :: y
           call foo(x,aGlobal)
           
           y(1)=aGlobal
           
c                       mUses :                 =>   mDefs :   
c                       ============================================== 
c                       [2: global::aGlobal,         [3: head::*y
c                           foo::*b,                     head::*y() ]
c                           bar::*q ]         
c
c
c                       ImplicitRemoves: 
c                       ================
c

      end subroutine


      
      subroutine bar(p,q) 
        double precision, dimension(2) :: p
        double precision q
        call foo(p,q)    
        
      end subroutine
