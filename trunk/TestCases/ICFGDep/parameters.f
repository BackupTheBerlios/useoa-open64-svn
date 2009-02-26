
      subroutine foo
           double precision :: x,y
           y=x

c                      mUses            => mDefs
c                      ==============================
c                      [1: foo::x,      => [2: foo::y,
c                          bar::*xx]           bar::*yy]
c
c                      ImplicitRemoves:
c                      ================
c                      [2: foo::y,
c                          bar::*yy]

 
           call bar(x,y)
      end subroutine

      subroutine bar(xx,yy)
           double precision :: xx,yy


           t=yy


c                      mUses           => mDefs
c                      ================================
c                      [1: bar::*yy,      [2: bar::t]
c                          foo::y]
c
c                      ImplicitRemoves:
c                      ================
c                      [2: bar::t]
c



           xx=t


c                      mUses              => mDefs
c                      =====================================
c                      [1: bar::t]           [2: bar::*xx,
c                                                foo::x]
c    
c                      ImplicitRemoves:
c                      ================
c                      [2: bar::*xx,
c                          foo::x]
c


      end subroutine

