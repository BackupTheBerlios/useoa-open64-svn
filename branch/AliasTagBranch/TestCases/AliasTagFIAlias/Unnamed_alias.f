! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Aliasing of Unnamed and Named References due to multiple 
! Function calls.      
!
! AliasPairs: 1. (i, UnNamedRef(2), *a1)
!             2. (UnnamedRef(i+j), j, *a2))      
!      
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      

! Need to find out why a1 and a2 are non-local, because they are
! formal reference parameters to foo and bar


       subroutine foo(a1)  
         integer a1
         print *, a1   
       end subroutine

       subroutine bar(a2)  
         integer a2
         print *, a2   
       end subroutine

       program head
         integer i,j
         
         i=1    
         
         j=4       
         
         call foo(i)   
                           
         call foo(2)      

         call foo(i+j) 
         
         call bar(j) 
         
         call bar(3)    
         
       end


! AliasTagFIAlias Output
! ======================

! MemRefHandle => AliasTag
! ========================

! MemRefHandle(I)  => 6,     Must
! MemRefHandle(J)  => 8,     Must
! MemRefHandle(2)  => (5,6), May
! MemRefHandle(I+J)=> (5,6), May
! MemRefHandle(I)  => 6,     Must
! MemRefHandle(J)  => 8,     Must
! MemRefHandle(3)  => (7,8), May

! MemRefExpr => AliasTag
! ======================

! NamedRef(A1) => 3, May, non-local
! NamedRef(A2) => 4, May, non-local
! NamedRef(I)  => 6, Must,local
! NamedRef(J)  => 8, Must, Local
! UnnamedRef(2)=>(5,6), May, Local
! UnnamedRef(I+J)=>(5,6), May, Local
! UnnamedRef(3)=> (7,8), May, Local
! *NamedRef(A1)=> (5,6), May, non-local
! *NamedRef(A2)=> (7,8), May, non-local






