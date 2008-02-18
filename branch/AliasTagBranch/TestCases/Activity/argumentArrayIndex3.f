c$openad XXX Template ad_template.f


      subroutine foo(a,b) 
       double precision a
       double precision b
c                            [u: *a] [v: *a]         [iA: *a]
       b=a*2
c                            [u: *b] [v: *a,*b]      [iA: *b]
      end subroutine

c$openad XXX Template ad_template.f
      subroutine head(x,y) 
       double precision, dimension(2) :: x
       double precision y
       double precision, dimension(2) :: p,q
       integer k,l
c$openad INDEPENDENT(x)
c                            [u: x] [v: x]          [iA: x]
       call foo(x(k),y)
c                            [u: y] [v: x,y]        [iA: y]
       call foo(p(k),q(l))
c                            [u: y] [v: x,y,p,q]    [iA: y]
c$openad DEPENDENT(y)
      end subroutine
