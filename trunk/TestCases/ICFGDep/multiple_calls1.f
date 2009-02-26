
      subroutine foo()
          double precision ::x,a,y
          call bar(x,a,y)
          call bar(x,a,y)
      end subroutine


      subroutine bar(f,b,g)
          double precision :: f,b,g
          g=b


c                    mUses                => mDefs
c                    ==============================
c                    [2: bar::*b,            [3: bar::*g
c                        foo::a,                 foo::y]
c
c                    ImplicitRemoves:
c                    ================
c                    [3: bar::*g
c                        foo::y]



          b=f

c                    mUses                => mDefs
c                    ==============================
c                    [1: bar::*f,            [2: bar::*b
c                        foo::x,                 foo::a]
c
c                    ImplicitRemoves:
c                    ================
c                    [2: bar::*b
c                        foo::a]


      end subroutine
      
