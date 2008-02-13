! similar to what we see in CG
        subroutine conj_grad(x,r)
        double precision x(*), r(*)
        integer j

        do j=1,10
          r(j) = x(j)
        enddo

        return
        end
