c**********************************************************************
      subroutine nnn(n,nf,nb)
      implicit double precision(a-h,o-z)
c
c        subroutine determines forward index nf, back index nb
c        for interior x difference at point n
c
      nf=n+1
      nb=n-1
      return 
      end
