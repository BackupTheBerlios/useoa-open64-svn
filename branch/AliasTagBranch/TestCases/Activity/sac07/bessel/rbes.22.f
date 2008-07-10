      subroutine g$rbes$22(g$p$, n, z, g$z, ldg$z, zjn, g$zjn, ldg$zjn, 
     *zjnp, zyn, g$zyn, ldg$zyn, zynp)
C       
C       Formal zyn is active.
C       Formal zjn is active.
C       Formal z is active.
C       
        integer g$p$
        integer g$pmax$
        parameter (g$pmax$ = 1)
        integer g$i$
        real r$1
        real r$0
        real g$const(g$pmax$)
        real g$den(g$pmax$)
        real g$d2np3(g$pmax$)
        real g$term(g$pmax$)
        real g$u(g$pmax$)
        real g$c(g$pmax$)
        real g$top(g$pmax$)
        real g$zsq(g$pmax$)
        real g$factor(g$pmax$)
        real g$delf(g$pmax$)
        real g$by(g$pmax$)
        real g$ay(g$pmax$)
        real g$bj(g$pmax$)
        real g$aj(g$pmax$)
        real g$zinv(g$pmax$)
        real g$cz(g$pmax$)
        real g$sz(g$pmax$)
        real g$zyn(ldg$zyn)
        real g$zjn(ldg$zjn)
        real g$z(ldg$z)
        integer ldg$z
        integer ldg$zjn
        integer ldg$zyn
C       
C       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C       SAVE                                                              MJU3-1
C *    2
C       ***********************************************************************
C       ***  THIS SUBROUTINE CALCULATES THE REGULAR RICATTI-BESSEL FUNCTION ZJN
C       ***AND ITS DERIVATIVE ZJNP AND THE IRREGULAR RICATTI-BESSEL FUNCTION
C       ***ZYN AND ITS DERIVATIVE ZYNP OF ORDER N FOR ARGUMENT Z.  THIS IS
C       ***A COPY OF LIGHTS ROUTINE ATTRIBUTED TO R.GORDON.
C       ***********************************************************************
        data zero, half, one, two, three /0.0, 0.5, 1.0, 2.0, 3.0/
C       ***EVALUATE TRIG FUNCTIONS
c$openad INDEPENDENT(z)
        if (g$p$ .gt. g$pmax$) then
          print *, 'Parameter g$p is greater than g$pmax.'
          stop
        endif
C       sz = sin(z)
        do 99957 g$i$ = 1, g$p$
          g$sz(g$i$) = cos(z) * g$z(g$i$)
99957   continue
        sz = sin(z)
C       cz = cos(z)
        do 99956 g$i$ = 1, g$p$
          g$cz(g$i$) = -sin(z) * g$z(g$i$)
99956   continue
        cz = cos(z)
C       zinv = one / z
        r$0 = one / z
        do 99955 g$i$ = 1, g$p$
          g$zinv(g$i$) = (-r$0 / (z)) * g$z(g$i$)
99955   continue
        zinv = r$0
        aj = sz
        do 99954 g$i$ = 1, g$p$
          g$aj(g$i$) = g$sz(g$i$)
99954   continue
C       bj = sz * zinv - cz
        do 99953 g$i$ = 1, g$p$
          g$bj(g$i$) = zinv * g$sz(g$i$) + sz * g$zinv(g$i$) + (-g$cz(g$
     *i$))
99953   continue
        bj = sz * zinv - cz
C       ay = -cz
        do 99952 g$i$ = 1, g$p$
          g$ay(g$i$) = -g$cz(g$i$)
99952   continue
        ay = -cz
C       by = -sz - cz * zinv
        do 99951 g$i$ = 1, g$p$
          g$by(g$i$) = -g$sz(g$i$) + (-zinv * g$cz(g$i$)) + (-cz * g$zin
     *v(g$i$))
99951   continue
        by = -sz - (cz * (zinv))
        if (n - 1) 1, 2, 3
C       ***FUNCTIONS OF ZERO ORDER
1       zjn = aj
        do 99950 g$i$ = 1, g$p$
          g$zjn(g$i$) = g$aj(g$i$)
99950   continue
        zjnp = cz
        zyn = ay
        do 99949 g$i$ = 1, g$p$
          g$zyn(g$i$) = g$ay(g$i$)
99949   continue
        zynp = sz
        return
C       ***FUNCTIONS OF ORDER ONE
2       zjn = bj
        do 99948 g$i$ = 1, g$p$
          g$zjn(g$i$) = g$bj(g$i$)
99948   continue
        zjnp = aj - zinv * bj
        zyn = by
        do 99947 g$i$ = 1, g$p$
          g$zyn(g$i$) = g$by(g$i$)
99947   continue
        zynp = ay - zinv * by
        return
C       ***FUNCTIONS OF ORDER GREATER THAN ONE
3       xn = n
C       delf = zinv + zinv
        do 99946 g$i$ = 1, g$p$
          g$delf(g$i$) = (1.0 + 1.0) * g$zinv(g$i$)
99946   continue
        delf = zinv + zinv
C       factor = delf + zinv
        do 99945 g$i$ = 1, g$p$
          g$factor(g$i$) = g$delf(g$i$) + g$zinv(g$i$)
99945   continue
        factor = delf + zinv
C       ***TEST TO SEE IF RECURRENCE CAN BE USED
        if (z .gt. xn) then
          goto 100
        endif
C       ***IN NON CLASSICAL REGION USE RECURSION RELATION FOR IRREGULAR
C       ***SOLUTION AND ASCENDING POWER SERIES FOR REGULAR SOLUTION.
C       ***THE COEFFICIENT,C, IN FRONT OF POWER SERIES IS CALCULATED DURING
C       ***FORWARD RECURRENCE.
C       zsq = z * z
        do 99944 g$i$ = 1, g$p$
          g$zsq(g$i$) = (z + z) * g$z(g$i$)
99944   continue
        zsq = z * z
        top = xn + xn
        do 99943 g$i$ = 1, g$p$
          g$top(g$i$) = 0.0
99943   continue
        xj = three
C       c = zsq / three
        do 99942 g$i$ = 1, g$p$
          g$c(g$i$) = (1.0d0 / three) * g$zsq(g$i$)
99942   continue
        c = zsq / three
C       ***IRREGULAR SOLUTION
C       25      zyn = by * factor - ay
25      do 99941 g$i$ = 1, g$p$
          g$zyn(g$i$) = factor * g$by(g$i$) + by * g$factor(g$i$) + (-g$
     *ay(g$i$))
99941   continue
        zyn = by * factor - ay
        ay = by
        do 99940 g$i$ = 1, g$p$
          g$ay(g$i$) = g$by(g$i$)
99940   continue
        by = zyn
        do 99939 g$i$ = 1, g$p$
          g$by(g$i$) = g$zyn(g$i$)
99939   continue
C       factor = factor + delf
        do 99938 g$i$ = 1, g$p$
          g$factor(g$i$) = g$factor(g$i$) + g$delf(g$i$)
99938   continue
        factor = factor + delf
        xj = xj + two
C       c = c * (z / xj)
        r$0 = z / xj
        do 99937 g$i$ = 1, g$p$
          g$c(g$i$) = r$0 * g$c(g$i$) + c * (1.0d0 / xj) * g$z(g$i$)
99937   continue
        c = c * r$0
        if (xj .lt. top) then
          goto 25
        endif
        zynp = ay - zinv * by * xn
C       ***REGULAR SOLUTION
C       factor = -half * zsq
        do 99936 g$i$ = 1, g$p$
          g$factor(g$i$) = -half * g$zsq(g$i$)
99936   continue
        factor = -half * (zsq)
        u = one
        do 99935 g$i$ = 1, g$p$
          g$u(g$i$) = 0.0
99935   continue
        term = one
        do 99934 g$i$ = 1, g$p$
          g$term(g$i$) = 0.0
99934   continue
        du = one
        dterm = one
C       d2np3 = top + three
        do 99933 g$i$ = 1, g$p$
          g$d2np3(g$i$) = g$top(g$i$)
99933   continue
        d2np3 = top + three
        den = d2np3
        do 99932 g$i$ = 1, g$p$
          g$den(g$i$) = g$d2np3(g$i$)
99932   continue
        dfact = zero
35      dfact = dfact + one
C       term = term * (factor / (dfact * den))
        r$0 = dfact * den
        r$1 = factor / r$0
        do 99931 g$i$ = 1, g$p$
          g$term(g$i$) = r$1 * g$term(g$i$) + (term * (1.0d0 / r$0) * g$
     *factor(g$i$)) + (term * (-r$1 / (r$0)) * dfact * g$den(g$i$))
99931   continue
        term = term * r$1
C       u = u + term
        do 99930 g$i$ = 1, g$p$
          g$u(g$i$) = g$u(g$i$) + g$term(g$i$)
99930   continue
        u = u + term
C       den = den + two
        do 99929 g$i$ = 1, g$p$
          g$den(g$i$) = g$den(g$i$)
99929   continue
        den = den + two
        dterm = dterm * (factor / (dfact * den))
        du = du + dterm
C       ***TEST CONVERGENCE TO SINGLE PRECISION ACCURACY
        if (abs(term) .gt. 1.0d-9) then
          goto 35
        endif
C       zjn = u * c
        do 99928 g$i$ = 1, g$p$
          g$zjn(g$i$) = c * g$u(g$i$) + u * g$c(g$i$)
99928   continue
        zjn = u * c
        zjnp = (xn + one) * zjn * zinv - (z * du / d2np3) * c
        return
C       ***FOR CLASSICAL CASE USE FORWARD RECURSION FOR REGULAR
C       ***AND IRREGULAR SOLUTIONS
C       100     const = zinv * xn
100     do 99927 g$i$ = 1, g$p$
          g$const(g$i$) = xn * g$zinv(g$i$)
99927   continue
        const = zinv * xn
C       top = const + const
        do 99926 g$i$ = 1, g$p$
          g$top(g$i$) = (1.0 + 1.0) * g$const(g$i$)
99926   continue
        top = const + const
C       200     aj = factor * bj - aj
200     do 99925 g$i$ = 1, g$p$
          g$aj(g$i$) = bj * g$factor(g$i$) + factor * g$bj(g$i$) + (-g$a
     *j(g$i$))
99925   continue
        aj = factor * bj - aj
C       ay = factor * by - ay
        do 99924 g$i$ = 1, g$p$
          g$ay(g$i$) = by * g$factor(g$i$) + factor * g$by(g$i$) + (-g$a
     *y(g$i$))
99924   continue
        ay = factor * by - ay
C       factor = factor + delf
        do 99923 g$i$ = 1, g$p$
          g$factor(g$i$) = g$factor(g$i$) + g$delf(g$i$)
99923   continue
        factor = factor + delf
        if (factor .gt. top) then
          goto 250
        endif
C       bj = factor * aj - bj
        do 99922 g$i$ = 1, g$p$
          g$bj(g$i$) = aj * g$factor(g$i$) + factor * g$aj(g$i$) + (-g$b
     *j(g$i$))
99922   continue
        bj = factor * aj - bj
C       by = factor * ay - by
        do 99921 g$i$ = 1, g$p$
          g$by(g$i$) = ay * g$factor(g$i$) + factor * g$ay(g$i$) + (-g$b
     *y(g$i$))
99921   continue
        by = factor * ay - by
C       factor = factor + delf
        do 99920 g$i$ = 1, g$p$
          g$factor(g$i$) = g$factor(g$i$) + g$delf(g$i$)
99920   continue
        factor = factor + delf
        if (factor .lt. top) then
          goto 200
        endif
        zjn = bj
        do 99919 g$i$ = 1, g$p$
          g$zjn(g$i$) = g$bj(g$i$)
99919   continue
        zjnp = aj - const * bj
        zyn = by
        do 99918 g$i$ = 1, g$p$
          g$zyn(g$i$) = g$by(g$i$)
99918   continue
        zynp = ay - const * by
        return
250     zjn = aj
        do 99917 g$i$ = 1, g$p$
          g$zjn(g$i$) = g$aj(g$i$)
99917   continue
        zjnp = bj - const * aj
        zyn = ay
        do 99916 g$i$ = 1, g$p$
          g$zyn(g$i$) = g$ay(g$i$)
99916   continue
        zynp = by - const * ay
c$openad DEPENDENT(zjn)
c$openad DEPENDENT(zyn)
      end
