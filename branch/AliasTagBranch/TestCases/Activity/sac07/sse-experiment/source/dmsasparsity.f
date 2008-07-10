      subroutine dmsasps(nx,ny,irn,icn,nz)
      integer nx, ny, elem, irn(*),icn(*),nz

c     **********
c     Ali Bouaricha  and Jorge More

      integer i, j, k

c     Computation of the function over the lower
c     triangular elements.

      ELEM = 0
      NZ = 0
      do 50 j = 0, ny
         do 40 i = 0, nx
            k = nx*(j-1) + i
	    ELEM = ELEM + 1
            if (i .ge. 1 .and. j .ge. 1) then
	       NZ = NZ + 1
               IRN(NZ) = ELEM
	       ICN(NZ) = K
            end if
            if (i .lt. nx .and. j .gt. 0) then
	       NZ = NZ + 1
               IRN(NZ) = ELEM
	       ICN(NZ) = K+1
            end if
            if (i .gt. 0 .and. j .lt. ny) then
	       NZ = NZ + 1
               IRN(NZ) = ELEM
	       ICN(NZ) = K+NX
            end if
   40    continue
   50 continue

c     Computation of the function and the gradient over the upper
c     triangular elements.

      do 70 j = 1, ny + 1
         do 60 i = 1, nx + 1
            k = nx*(j-1) + i
	    ELEM = ELEM + 1
            if (i .le. nx .and. j .gt. 1) then
	       NZ = NZ + 1
               IRN(NZ) = ELEM
	       ICN(NZ) = K-NX
            end if
            if (i .gt. 1 .and. j .le. ny) then
	       NZ = NZ + 1
               IRN(NZ) = ELEM
	       ICN(NZ) = K-1
            end if
            if (i .le. nx .and. j .le. ny) then
	       NZ = NZ + 1
               IRN(NZ) = ELEM
	       ICN(NZ) = K
            end if
   60    continue
   70 continue

      end
