      subroutine head(x,y)
c  x and y should be IN and OUT parameters 
         double precision x
         double precision y
         double precision a, b, c

         a = x * 5

c                      mUses          => mDefs
c                      ==============================
c                      [1: head::*x]  => [2: head::a]
c
c                      ImplicitRemoves:
c                      ================
c                      [2: head::a]
c


         
         b = a

c                      mUses         => mDefs
c                      ==============================
c                      [1: head::a]  => [2: head::b]
c
c                      ImplicitRemoves:
c                      ================
c                      [2: head::b]
c



         if (a > 10) then
            c = 3

c                      ImplicitRemoves:
c                      ================
c                      [3: head::c]
c


         else
            c = a * (-1)


c                      mUses         => mDefs
c                      ==============================
c                      [1: head::a]  => [3: head::c]
c
c                      ImplicitRemoves:
c                      ================
c                      [3: head::c]
c




         end if
         y = 3 * b + c


c                      mUses         => mDefs
c                      ==============================
c                      [1: head::b]  => [2: head::*y]
c                      [3: head::c]  => [2: head::*y]
c
c                      ImplicitRemoves:
c                      ================
c


       end subroutine

