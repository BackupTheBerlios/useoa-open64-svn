

      subroutine head(x,y)
      double precision x(4),y(4)
      double precision c,d
      double precision p
      p=2.0

c                    ImplicitRemoves:
c                    ================
c                    [1: foo::*a,
c                        head::*x(),
c                        foo::*b,
c                        head::p]
c
      
      call foo(x(1),x(2),c,d)
      call foo(x(1),p,c,d)

      y(3)=c*d

c                    mUses           => mDefs
c                    ==============================
c                    [2: head::c,    => [3: head::*y,
c                        foo::*c]           head::*y()]
c         
c                    [4: head::d,    => [3: head::*y,
c                        head::*d]          head::*y()] 
c
c                    ImplicitRemoves:
c                    ================
c                   
      

      y(4)=c+d

c                    mUses           => mDefs
c                    ==============================
c                    [2: head::c,    => [3: head::*y,
c                        foo::*c]           head::*y()]
c
c                    [4: head::d,    => [3: head::*y,
c                        head::*d]          head::*y()]
c
c                    ImplicitRemoves:
c                    ================
c

      end subroutine head

      subroutine foo(a,b,c,d) 
      double precision a,b,c,d
      c=sin(a*b)

c                    mUses                => mDefs
c                    ==============================
c                    [1: foo::*a,            [2: head::*c,
c                        head::*x(),             head::c]
c                        foo::*b,                
c                        head::p,
c
c                    ImplicitRemoves:
c                    ================
c                    [2: head::*c,
c                        head::c ]
c



      d=cos(a+b) 

c                    mUses                => mDefs
c                    ==============================
c                    [1: foo::*a,            [4: head::*d,
c                        head::*x(),             head::d]
c                        foo::*b,
c                        head::p]
c
c                    ImplicitRemoves:
c                    ================
c                    [4: head::*d,
c                        head::d ]


      end subroutine
