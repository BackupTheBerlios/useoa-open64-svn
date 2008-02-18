
      subroutine foo
          double precision :: x,a,b
          call bar(x,a,b)
          call bar(x,a,b)
      end subroutine

      subroutine bar(f,b,g)
          double precision :: f,b,g
          b=f

c                mUses                => mDefs
c                ==============================
c                [1: bar::*f,            [2: bar::*b
c                    foo::x,                 foo::a]
c
c                ImplicitRemoves:
c                ================
c                [2: bar::*b
c                    foo::a]
          

          
          g=b


c                mUses                => mDefs
c                ==============================
c                [2: bar::*b,            [3: bar::*g
c                    foo::a,                 foo::b]
c
c                ImplicitRemoves:
c                ================
c                [3: bar::*g
c                    foo::b]


      end subroutine
