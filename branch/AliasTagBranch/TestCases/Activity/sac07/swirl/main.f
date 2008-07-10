        program main    
        character*(*) task
        integer n, ldfjac, nint
        parameter (n=56, ldfjac = 56)
        double precision eps
        double precision x(n), fvec(n), fjac(ldfjac,n)        

        call dsfdfj(n,x,fvec,fjac,ldfjac,task,eps,nint)

        end
