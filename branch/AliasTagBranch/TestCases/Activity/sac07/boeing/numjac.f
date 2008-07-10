c**********************************************************************
      subroutine numjac
      implicit double precision(a-h,o-z)
      common //
     1 u(190),eq(190),eqold(190),a(190,95),eqplus(190),
     2 b(10),bth(10),ff(3,6,10),gg(3,6,10),hh(3,6,10),
     3 mc,nc,dx,dy,ddx,ddy,ddx2,ddy2,mc1,nc1,mc2,mc3,
     4 theta1,gamma,gm1,xmach,ncf,mcncf,
     5 qinc,dinc,pinc,einc,ainc,
     6 htot,swall,dstag,emaxs(200),eqmaxs(200),delta,
     7 kdiag,kblk,ksize,kblk2,kblk3
     8 ,itmax,sstol,alpha,nprint,emax,eqmax
     9 ,kprint,dtheta,icont,xend,xincr
      dimension ffplus(3),ggplus(3),hhplus(3),kbegin(5),eqp(3)
      real timvec(2),etime,dummy
      external etime
c
c         compute rowwise numerical jacobian.
c         successively perturb each variable (column) which appears
c         in an equation (row).
c
c         note:  in the banded solver (ICBAND) storage scheme, the
c         matrix entry that would be a(i,j) in dense storage mode
c         is stored in a(j-i+kdiag), where kdiag is suitably chosen
c         to represent the diagonal entry (kdiag is in common)
c
      dummy=etime(timvec)
      t1=timvec(1)
c         zero out jacobian
      do 10 k=1,ksize
      do 10 m=1,mcncf
10    a(m,k)=0.
c
c         loop on columns m=1,mc
c
      do 1000 m=1,mc
c
c         loop on n=1 to nc
c           n=1 wall 3 eqns
c           n=2 to nc1 3 eqns
c           n=nc 4 eqns
c
      kdiagb=(m-1)*ncf+1
c
c         wall eqns
c
      if(m .ne. 1)go to 50
c         special case m=1 , stag pt, gives diagonal block
      do 40 i=1,3
40    a(i,kdiag)=1.
      go to 65
50    continue
c         derivs wrt flow variables
      kbegin(1)=kdiagb-kblk
      kbegin(2)=kdiagb
      kbegin(3)=kdiagb+3
      kbegin(4)=kdiagb+6
      kbegin(5)=kdiagb+kblk
      if(m .eq. mc)kbegin(5)=kdiagb-kblk2
      do 60 i=1,5
60    call jacob(1,m,kbegin(i),kbegin(i)+2)
65    continue
c         derivs wrt shock locations
      call mshk(m,mf,mb)
      ilast=mf-mb+1
      do 70 i=1,ilast
      kbegin(i)=(mb+i-1)*ncf
      call jacob(1,m,kbegin(i),kbegin(i))
70    continue
c
c
c         interior points
c
c
      do 900 n=2,nc1
      kdiagb=(m-1)*ncf+3*(n-1)+1
c
c         variables at n-1,m affect ff(ii,n-1,m)
c
      kbeg=kdiagb-3
      kend=kbeg+2
      do 100 k=kbeg,kend
      usave=u(k)
      u(k)=u(k)+delta
      call eval(n-1,m,1,0,ffplus(1),dum1,dum2)
      do 110 ii=1,3
      idiag=(m-1)*ncf+(n-1)*3+ii
      index=k-idiag+kblk3+1
110   a(idiag,index)=-(ffplus(ii)-ff(ii,n-1,m))*ddx2/delta
      u(k)=usave
100   continue
c
c         variables at n,m-1 affect gg(ii,n,m-1)
c         special case m=mc
c
      if(m .eq. 1) go to 201
      factor=-1.
      if(m .eq. mc) factor=-4.
      kbeg=kdiagb-kblk
      kend=kbeg+2
      do 200 k=kbeg,kend
      usave=u(k)
      u(k)=u(k)+delta
      call eval(n,m-1,2,0,dum1,ggplus(1),dum2)
      do 210 ii=1,3
      idiag=(m-1)*ncf+(n-1)*3+ii
      index=k-idiag+kblk3+1
210   a(idiag,index)=factor*(ggplus(ii)-gg(ii,n,m-1))*ddy2/delta
      u(k)=usave
200   continue
201   continue
c
c         variables at n,m affect hh(ii,n,m)
c         special case m=mc gg(ii,n,m) also affected
      kbeg=kdiagb
      kend=kbeg+2
      iflag=0
      if(m .eq. 1)iflag=1
      do 300 k=kbeg,kend
      usave=u(k)
      u(k)=u(k)+delta
      call eval(n,m,3,iflag,dum1,dum2,hhplus(1))
      if(m .eq. mc)call eval(n,m,2,0,dum1,ggplus(1),dum2)
      do 310 ii=1,3
      idiag=(m-1)*ncf+(n-1)*3+ii
      index=k-idiag+kblk3+1
      a(idiag,index)=(hhplus(ii)-hh(ii,n,m))/delta
      if(m .eq. mc)a(idiag,index)=a(idiag,index)
     1 +3.*(ggplus(ii)-gg(ii,n,m))*ddy2/delta
310   continue
      u(k)=usave
300   continue
c
c         variables at n,m+1 affect gg(ii,n,m+1)
c         special case m=1--h affected but not g
c
      if(m .eq. mc)go to 401
      kbeg=kdiagb+kblk
      kend=kbeg+2
      do 400 k=kbeg,kend
      usave=u(k)
      u(k)=u(k)+delta
      if(m .ne. 1)call eval(n,m+1,2,0,dum1,ggplus(1),dum2)
      if(m .eq. 1)call eval(n,m,3,1,dum1,dum2,hhplus(1))
      do 410 ii=1,3
      idiag=(m-1)*ncf+(n-1)*3+ii
      index=k-idiag+kblk3+1
      if(m .ne. 1)a(idiag,index)=(ggplus(ii)-gg(ii,n,m+1))*ddy2/delta
      if(m .eq. 1)a(idiag,index)=(hhplus(ii)-hh(ii,n,1))/delta
410   continue
      u(k)=usave
400   continue
401   continue
c
c         variables at n+1,m affect ff(ii,n+1,m)
c
      kbeg=kdiagb+3
      kend=kbeg+2
      do 500 k=kbeg,kend
      usave=u(k)
      u(k)=u(k)+delta
      call eval(n+1,m,1,0,ffplus(1),dum1,dum2)
      do 510 ii=1,3
      idiag=(m-1)*ncf+(n-1)*3+ii
      index=k-idiag+kblk3+1
      a(idiag,index)=(ffplus(ii)-ff(ii,n+1,m))*ddx2/delta
510   continue
      u(k)=usave
500   continue
c
c         variables at n,m-2 affect gg(ii,n,mc-2) at m=mc only
c
      if(m .ne. mc)go to 601
      kbeg=kdiagb-kblk2
      kend=kbeg+2
      do 600 k=kbeg,kend
      usave=u(k)
      u(k)=u(k)+delta
      call eval(n,mc2,2,0,dum1,ggplus(1),dum2)
      do 610 ii=1,3
      idiag=(m-1)*ncf+(n-1)*3+ii
      index=k-idiag+kblk3+1
      a(idiag,index)=(ggplus(ii)-gg(ii,n,mc2))*ddy2/delta
610   continue
      u(k)=usave
600    continue
601   continue
c
c         derivs wrt shock locations
c
      call mshk(m,mf,mb)
      do 700 k=mb,mf
      kb=k*ncf
      usave=u(kb)
      u(kb)=u(kb)+delta
      call single(n,m,eqp)
      do 710 ii=1,3
      idiag=(m-1)*ncf+(n-1)*3+ii
      index=kb-idiag+kblk3+1
      a(idiag,index)=(eqp(ii)-eq(idiag))/delta
710   continue
      u(kb)=usave
700   continue
701   continue
c
c        end loop on n
c
900   continue
c
c
c         equations at the shock
c
c
      kdiagb=(m-1)*ncf+3*(nc-1)+1
c
c         derivs wrt flow variables
c
      kbegin(1)=kdiagb-kblk
      kbegin(2)=kdiagb-6
      kbegin(3)=kdiagb-3
      kbegin(4)=kdiagb
      kbegin(5)=kdiagb+kblk
      if(m .eq. mc)kbegin(5)=kdiagb-kblk2
      istart=1
      if(m .eq. 1)istart=2
      do 750 i=istart,5
750   call jacob(nc,m,kbegin(i),kbegin(i)+2)
c
c         derivs wrt shock locations
c
      call mshk(m,mf,mb)
      ilast=mf-mb+1
      do 770 i=1,ilast
      kbegin(i)=(mb+i-1)*ncf
      call jacob(nc,m,kbegin(i),kbegin(i))
770   continue
c
c         end loop on m
c
1000  continue
      dummy=etime(timvec)
      t2=timvec(1)
      write(6,5000)t2-t1
5000  format(/," numjac time in seconds=",f10.4)
      return
      end
