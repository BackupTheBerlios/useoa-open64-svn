      subroutine g$nnn$0(g$p$, n, nf, nb)
C       
C       
        integer g$p$
        integer g$pmax$
        parameter (g$pmax$ = 190)
        integer g$i$
C       
        implicit double precision (a-h, o-z)
C       
C       subroutine determines forward index nf, back index nb
C       for interior x difference at point n
C       
        if (g$p$ .gt. g$pmax$) then
          print *, 'Parameter g$p is greater than g$pmax.'
          stop
        endif
        nf = n + 1
        nb = n - 1
        return
      end
