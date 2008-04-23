!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program that involves aliasing of a reference parameter to a 
! global variable.
!
! AliasPairs: 1. (t1,*a)  
!             2. (g1,*b)      
!             3. (g,*c)      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       program testing
       double precision g,g1
       common /cpad/ g,g1
       double precision t1, t2
       call head(t1, t2)  
                                         
       end

       subroutine head(x, f)           
       common /cpad/ g,g1 
       double precision :: x
       double precision :: f
       double precision t1, t2, t3

       t1=x*f                           
                                         
       g=1.0     
       
       g1=2.0    
       
       call bar(t1,g1,g)                
                                         
       t3=f*30*g                        
                                         
       f=t1+t2                          

       end subroutine

       subroutine bar(a,b,c)             
                                         
       common /cpad/ g,g1
       double precision a,b,c

       b = a + g + g1 + c               
       
       return
       end


! AliasTagFIAlias
! ===============

! MemRefHandle(T1) => 14   Must
! MemRefHandle(X)  => 5,6  May
! MemRefHandle(F)  => 7,8  May
! MemRefHandle(G)  => 1    May
! MemRefHandle(G1) => 2    May
! MemRefHandle(T3) => 18   May
! MemRefHandle(T2) => 16   Must
! MemRefHandle(B)  => 2    May
! MemRefHandle(C)  => 1    May
! MemRefHandle(A)  => 13,14 May


! NamedRef(G) => 1, May, Non-Local
! NamedRef(G1)=> 2, May, Non-Local
! NamedRef(T1)=> 6, Must,Local
! NamedRef(T2)=> 8, Must,Local
! NamedRef(X) => 10,Must,Local
! NamedRef(F) => 12,Must,Local 
! NamedRef(T1)=> 14,Must,Local
! NamedRef(T2)=> 16,Must,Local
! NamedRef(T3)=> 18,Must,Local
! NamedRef(A) => 20,Must,Local
! NamedRef(B) => 22,Must,Local
! NamedRef(C) => 24,Must,Local
! *NamedRef(X)=> 5,6,May,Local
! *NamedRef(F)=> 7,8,May,Local
! *NamedRef(A)=> (13,14)May,Local
! *NamedRef(B)=> 2, May, Local
! *NamedRef(C)=> 1, May, Local
