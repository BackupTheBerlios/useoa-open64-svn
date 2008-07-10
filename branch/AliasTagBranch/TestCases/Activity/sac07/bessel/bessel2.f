	parameter(n=100)
	parameter(step=0.1)

        real g$z,g$zjn,g$zyn
        
        g$z = 1.0

	do L=0,10
		print *, 'L=',L
		z = 0.1
		do i=1,n
			call g$rbes$22(1,L,z,g$z,1,zjn,g$zjn,1,
     +                                 zjnp,zyn,g$zyn,1,zynp)
			write(6,100) z,zjn,g$zjn,zyn,g$zyn
			z = z + step
		enddo
	enddo

  100	format(f10.3,4e15.5)
	stop
	end
