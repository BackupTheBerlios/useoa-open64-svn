!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine head(nx,ny,x,fvec,r)
      integer nx,ny
      double precision r
      double precision x(nx*ny),fvec(nx*ny)
      double precision t1,t2

c$openad INDEPENDENT(x)
      t1=x(1)*f
      call bar(nx,ny,x,fvec,t1,t2)
      t3=fvec(1)*30
      fvec(1)=t1+t2
c$openad DEPENDENT(fvec)

      end subroutine

      subroutine bar(nx,ny,x,fvec,a,b)
      integer nx,ny
      double precision x(nx*ny),fvec(nx*ny)
      double precision a,b

      do 60 i=1, nx*ny
        fvec(i) = x(1)
   60 continue

      b = a  
      return
      end
