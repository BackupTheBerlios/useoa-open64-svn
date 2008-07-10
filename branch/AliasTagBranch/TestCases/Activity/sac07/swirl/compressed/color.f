      subroutine seq(n,indrow,jpntr,indcol,ipntr,list,ngrp,maxgrp,
     *               iwa)
      integer n,maxgrp
      integer indrow(*),jpntr(n+1),indcol(*),ipntr(*),list(n),
     *        ngrp(n),iwa(n)
c     **********
c
c     subroutine seq
c
c     Given the sparsity pattern of an m by n matrix A, this
c     subroutine determines a consistent partition of the
c     columns of A by a sequential algorithm.
c
c     A consistent partition is defined in terms of the loopless
c     graph G with vertices a(j), j = 1,2,...,n where a(j) is the
c     j-th column of A and with edge (a(i),a(j)) if and only if
c     columns i and j have a non-zero in the same row position.
c
c     A partition of the columns of A into groups is consistent
c     if the columns in any group are not adjacent in the graph G.
c     In graph-theory terminology, a consistent partition of the
c     columns of A corresponds to a coloring of the graph G.
c
c     The subroutine examines the columns in the order specified
c     by the array list, and assigns the current column to the
c     group with the smallest possible number.
c
c     Note that the value of m is not needed by seq and is
c     therefore not present in the subroutine statement.
c
c     The subroutine statement is
c
c       subroutine seq(n,indrow,jpntr,indcol,ipntr,list,ngrp,maxgrp,
c                      iwa)
c
c     where
c
c       n is a positive integer input variable set to the number
c         of columns of A.
c
c       indrow is an integer input array which contains the row
c         indices for the non-zeroes in the matrix A.
c
c       jpntr is an integer input array of length n + 1 which
c         specifies the locations of the row indices in indrow.
c         The row indices for column j are
c
c               indrow(k), k = jpntr(j),...,jpntr(j+1)-1.
c
c         Note that jpntr(n+1)-1 is then the number of non-zero
c         elements of the matrix A.
c
c       indcol is an integer input array which contains the
c         column indices for the non-zeroes in the matrix A.
c
c       ipntr is an integer input array of length m + 1 which
c         specifies the locations of the column indices in indcol.
c         The column indices for row i are
c
c               indcol(k), k = ipntr(i),...,ipntr(i+1)-1.
c
c         Note that ipntr(m+1)-1 is then the number of non-zero
c         elements of the matrix A.
c
c       list is an integer input array of length n which specifies
c         the order to be used by the sequential algorithm.
c         The j-th column in this order is list(j).
c
c       ngrp is an integer output array of length n which specifies
c         the partition of the columns of A. Column jcol belongs
c         to group ngrp(jcol).
c
c       maxgrp is an integer output variable which specifies the
c         number of groups in the partition of the columns of A.
c
c       iwa is an integer work array of length n.
c
c     Argonne National Laboratory. MINPACK Project. July 1983.
c     Thomas F. Coleman, Burton S. Garbow, Jorge J. More'
c
c     **********
      integer ic,ip,ir,j,jcol,jp
c
c     Initialization block.
c
      maxgrp = 0
      do 10 jp = 1, n
         ngrp(jp) = n
         iwa(jp) = 0
   10    continue
c
c     Beginning of iteration loop.
c
      do 60 j = 1, n
         jcol = list(j)
c
c        Find all columns adjacent to column jcol.
c
c        Determine all positions (ir,jcol) which correspond
c        to non-zeroes in the matrix.
c
         do 30 jp = jpntr(jcol), jpntr(jcol+1)-1
            ir = indrow(jp)
c
c           For each row ir, determine all positions (ir,ic)
c           which correspond to non-zeroes in the matrix.
c
            do 20 ip = ipntr(ir), ipntr(ir+1)-1
               ic = indcol(ip)
c
c              Array iwa marks the group numbers of the
c              columns which are adjacent to column jcol.
c
               iwa(ngrp(ic)) = j
   20          continue
   30       continue
c
c        Assign the smallest un-marked group number to jcol.
c
         do 40 jp = 1, maxgrp
            if (iwa(jp) .ne. j) go to 50
   40       continue
         maxgrp = maxgrp + 1
   50    continue
         ngrp(jcol) = jp
   60    continue
c
c        End of iteration loop.
c
      return
c
c     Last card of subroutine seq.
c
      end
      subroutine srtdat(n,nnz,indrow,indcol,jpntr,iwa)
      integer n,nnz
      integer indrow(nnz),indcol(nnz),jpntr(n+1),iwa(n)
c     **********
c
c     subroutine srtdat
c
c     Given the non-zero elements of an m by n matrix A in
c     arbitrary order as specified by their row and column
c     indices, this subroutine permutes these elements so
c     that their column indices are in non-decreasing order.
c
c     On input it is assumed that the elements are specified in
c
c           indrow(k),indcol(k), k = 1,...,nnz.
c
c     On output the elements are permuted so that indcol is
c     in non-decreasing order. In addition, the array jpntr
c     is set so that the row indices for column j are
c
c           indrow(k), k = jpntr(j),...,jpntr(j+1)-1.
c
c     Note that the value of m is not needed by srtdat and is
c     therefore not present in the subroutine statement.
c
c     The subroutine statement is
c
c       subroutine srtdat(n,nnz,indrow,indcol,jpntr,iwa)
c
c     where
c
c       n is a positive integer input variable set to the number
c         of columns of A.
c
c       nnz is a positive integer input variable set to the number
c         of non-zero elements of A.
c
c       indrow is an integer array of length nnz. On input indrow
c         must contain the row indices of the non-zero elements of A.
c         On output indrow is permuted so that the corresponding
c         column indices of indcol are in non-decreasing order.
c
c       indcol is an integer array of length nnz. On input indcol
c         must contain the column indices of the non-zero elements
c         of A. On output indcol is permuted so that these indices
c         are in non-decreasing order.
c
c       jpntr is an integer output array of length n + 1 which
c         specifies the locations of the row indices in the output
c         indrow. The row indices for column j are
c
c               indrow(k), k = jpntr(j),...,jpntr(j+1)-1.
c
c         Note that jpntr(1) is set to 1 and that jpntr(n+1)-1
c         is then nnz.
c
c       iwa is an integer work array of length n.
c
c     Subprograms called
c
c       FORTRAN-supplied ... max
c
c     Argonne National Laboratory. MINPACK Project. July 1983.
c     Thomas F. Coleman, Burton S. Garbow, Jorge J. More'
c
c     **********
      integer i,j,k,l
c
c     Store in array iwa the counts of non-zeroes in the columns.
c
      do 10 j = 1, n
         iwa(j) = 0
   10    continue
      do 20 k = 1, nnz
         iwa(indcol(k)) = iwa(indcol(k)) + 1
   20    continue
c
c     Set pointers to the start of the columns in indrow.
c
      jpntr(1) = 1
      do 30 j = 1, n
         jpntr(j+1) = jpntr(j) + iwa(j)
         iwa(j) = jpntr(j)
   30    continue
      k = 1
c
c     Begin in-place sort.
c
   40 continue
         j = indcol(k)
         if (k .ge. jpntr(j)) then
c
c           Current element is in position. Now examine the
c           next element or the first un-sorted element in
c           the j-th group.
c
            k = max(k+1,iwa(j))
         else
c
c           Current element is not in position. Place element
c           in position and make the displaced element the
c           current element.
c
            l = iwa(j)
            iwa(j) = iwa(j) + 1
            i = indrow(k)
            indrow(k) = indrow(l)
            indcol(k) = indcol(l)
            indrow(l) = i
            indcol(l) = j
            end if
         if (k .le. nnz) go to 40
      return
c
c     Last card of subroutine srtdat.
c
      end
      subroutine ido(m,n,indrow,jpntr,indcol,ipntr,ndeg,list,
     *               maxclq,iwa1,iwa2,iwa3,iwa4)
      integer m,n,maxclq
      integer indrow(*),jpntr(n+1),indcol(*),ipntr(m+1),ndeg(n),
     *        list(n),iwa1(0:n-1),iwa2(n),iwa3(n),iwa4(n)
c     **********
c
c     subroutine ido
c
c     Given the sparsity pattern of an m by n matrix A, this
c     subroutine determines an incidence-degree ordering of the
c     columns of A.
c
c     The incidence-degree ordering is defined for the loopless
c     graph G with vertices a(j), j = 1,2,...,n where a(j) is the
c     j-th column of A and with edge (a(i),a(j)) if and only if
c     columns i and j have a non-zero in the same row position.
c
c     The incidence-degree ordering is determined recursively by
c     letting list(k), k = 1,...,n be a column with maximal
c     incidence to the subgraph spanned by the ordered columns.
c     Among all the columns of maximal incidence, ido chooses a
c     column of maximal degree.
c
c     The subroutine statement is
c
c       subroutine ido(m,n,indrow,jpntr,indcol,ipntr,ndeg,list,
c                      maxclq,iwa1,iwa2,iwa3,iwa4)
c
c     where
c
c       m is a positive integer input variable set to the number
c         of rows of A.
c
c       n is a positive integer input variable set to the number
c         of columns of A.
c
c       indrow is an integer input array which contains the row
c         indices for the non-zeroes in the matrix A.
c
c       jpntr is an integer input array of length n + 1 which
c         specifies the locations of the row indices in indrow.
c         The row indices for column j are
c
c               indrow(k), k = jpntr(j),...,jpntr(j+1)-1.
c
c         Note that jpntr(n+1)-1 is then the number of non-zero
c         elements of the matrix A.
c
c       indcol is an integer input array which contains the
c         column indices for the non-zeroes in the matrix A.
c
c       ipntr is an integer input array of length m + 1 which
c         specifies the locations of the column indices in indcol.
c         The column indices for row i are
c
c               indcol(k), k = ipntr(i),...,ipntr(i+1)-1.
c
c         Note that ipntr(m+1)-1 is then the number of non-zero
c         elements of the matrix A.
c
c       ndeg is an integer input array of length n which specifies
c         the degree sequence. The degree of the j-th column
c         of A is ndeg(j).
c
c       list is an integer output array of length n which specifies
c         the incidence-degree ordering of the columns of A. The j-th
c         column in this order is list(j).
c
c       maxclq is an integer output variable set to the size
c         of the largest clique found during the ordering.
c
c       iwa1,iwa2,iwa3, and iwa4 are integer work arrays of length n.
c
c     Subprograms called
c
c       MINPACK-supplied ... numsrt
c
c       FORTRAN-supplied ... max
c
c     Argonne National Laboratory. MINPACK Project. August 1984.
c     Thomas F. Coleman, Burton S. Garbow, Jorge J. More'
c
c     **********
      integer ic,ip,ir,jcol,jp,
     *        maxinc,maxlst,ncomp,numinc,numlst,numord,numwgt
c
c     Sort the degree sequence.
c
      call numsrt(n,n-1,ndeg,-1,iwa4,iwa2,iwa3)
c
c     Initialization block.
c
c     Create a doubly-linked list to access the incidences of the
c     columns. The pointers for the linked list are as follows.
c
c     Each un-ordered column ic is in a list (the incidence list)
c     of columns with the same incidence.
c
c     iwa1(numinc) is the first column in the numinc list
c     unless iwa1(numinc) = 0. In this case there are
c     no columns in the numinc list.
c
c     iwa2(ic) is the column before ic in the incidence list
c     unless iwa2(ic) = 0. In this case ic is the first
c     column in this incidence list.
c
c     iwa3(ic) is the column after ic in the incidence list
c     unless iwa3(ic) = 0. In this case ic is the last
c     column in this incidence list.
c
c     If ic is an un-ordered column, then list(ic) is the
c     incidence of ic to the graph induced by the ordered
c     columns. If jcol is an ordered column, then list(jcol)
c     is the incidence-degree order of column jcol.
c
      maxinc = 0
      do 10 jp = n, 1, -1
         ic = iwa4(jp)
         iwa1(n-jp) = 0
         iwa2(ic) = 0
         iwa3(ic) = iwa1(0)
         if (iwa1(0) .gt. 0) iwa2(iwa1(0)) = ic
         iwa1(0) = ic
         iwa4(jp) = 0
         list(jp) = 0
   10    continue
c
c     Determine the maximal search length for the list
c     of columns of maximal incidence.
c
      maxlst = 0
      do 20 ir = 1, m
         maxlst = maxlst + (ipntr(ir+1) - ipntr(ir))**2
   20    continue
      maxlst = maxlst/n
      maxclq = 0
      numord = 1
c
c     Beginning of iteration loop.
c
   30 continue
c
c        Choose a column jcol of maximal degree among the
c        columns of maximal incidence maxinc.
c
   40    continue
            jp = iwa1(maxinc)
            if (jp .gt. 0) go to 50
            maxinc = maxinc - 1
            go to 40
   50    continue
         numwgt = -1
         do 60 numlst = 1, maxlst
            if (ndeg(jp) .gt. numwgt) then
               numwgt = ndeg(jp)
               jcol = jp
               end if
            jp = iwa3(jp)
            if (jp .le. 0) go to 70
   60       continue
   70    continue
         list(jcol) = numord
c
c        Update the size of the largest clique
c        found during the ordering.
c
         if (maxinc .eq. 0) ncomp = 0
         ncomp = ncomp + 1
         if (maxinc+1 .eq. ncomp) maxclq = max(maxclq,ncomp)
c
c        Termination test.
c
         numord = numord + 1
         if (numord .gt. n) go to 100
c
c        Delete column jcol from the maxinc list.
c
         if (iwa2(jcol) .eq. 0) then
            iwa1(maxinc) = iwa3(jcol)
         else
            iwa3(iwa2(jcol)) = iwa3(jcol)
            end if
         if (iwa3(jcol) .gt. 0) iwa2(iwa3(jcol)) = iwa2(jcol)
c
c        Find all columns adjacent to column jcol.
c
         iwa4(jcol) = n
c
c        Determine all positions (ir,jcol) which correspond
c        to non-zeroes in the matrix.
c
         do 90 jp = jpntr(jcol), jpntr(jcol+1)-1
            ir = indrow(jp)
c
c           For each row ir, determine all positions (ir,ic)
c           which correspond to non-zeroes in the matrix.
c
            do 80 ip = ipntr(ir), ipntr(ir+1)-1
               ic = indcol(ip)
c
c              Array iwa4 marks columns which are adjacent to
c              column jcol.
c
               if (iwa4(ic) .lt. numord) then
                  iwa4(ic) = numord
c
c                 Update the pointers to the current incidence lists.
c
                  numinc = list(ic)
                  list(ic) = list(ic) + 1
                  maxinc = max(maxinc,list(ic))
c
c                 Delete column ic from the numinc list.
c
                  if (iwa2(ic) .eq. 0) then
                     iwa1(numinc) = iwa3(ic)
                  else
                     iwa3(iwa2(ic)) = iwa3(ic)
                     end if
                  if (iwa3(ic) .gt. 0) iwa2(iwa3(ic)) = iwa2(ic)
c
c                 Add column ic to the numinc+1 list.
c
                  iwa2(ic) = 0
                  iwa3(ic) = iwa1(numinc+1)
                  if (iwa1(numinc+1) .gt. 0) iwa2(iwa1(numinc+1)) = ic
                  iwa1(numinc+1) = ic
                  end if
   80          continue
   90       continue
c
c        End of iteration loop.
c
         go to 30
  100 continue
c
c     Invert the array list.
c
      do 110 jcol = 1, n
         iwa2(list(jcol)) = jcol
  110    continue
      do 120 jp = 1, n
         list(jp) = iwa2(jp)
  120    continue
      return
c
c     Last card of subroutine ido.
c
      end
      subroutine numsrt(n,nmax,num,mode,index,last,next)
      integer n,nmax,mode
      integer num(n),index(n),last(0:nmax),next(n)
c     **********.
c
c     subroutine numsrt
c
c     Given a sequence of integers, this subroutine groups
c     together those indices with the same sequence value
c     and, optionally, sorts the sequence into either
c     ascending or descending order.
c
c     The sequence of integers is defined by the array num,
c     and it is assumed that the integers are each from the set
c     0,1,...,nmax. On output the indices k such that num(k) = l
c     for any l = 0,1,...,nmax can be obtained from the arrays
c     last and next as follows.
c
c           k = last(l)
c           while (k .ne. 0) k = next(k)
c
c     Optionally, the subroutine produces an array index so that
c     the sequence num(index(i)), i = 1,2,...,n is sorted.
c
c     The subroutine statement is
c
c       subroutine numsrt(n,nmax,num,mode,index,last,next)
c
c     where
c
c       n is a positive integer input variable.
c
c       nmax is a positive integer input variable.
c
c       num is an input array of length n which contains the
c         sequence of integers to be grouped and sorted. It
c         is assumed that the integers are each from the set
c         0,1,...,nmax.
c
c       mode is an integer input variable. The sequence num is
c         sorted in ascending order if mode is positive and in
c         descending order if mode is negative. If mode is 0,
c         no sorting is done.
c
c       index is an integer output array of length n set so
c         that the sequence
c
c               num(index(i)), i = 1,2,...,n
c
c         is sorted according to the setting of mode. If mode
c         is 0, index is not referenced.
c
c       last is an integer output array of length nmax + 1. The
c         index of num for the last occurrence of l is last(l)
c         for any l = 0,1,...,nmax unless last(l) = 0. In
c         this case l does not appear in num.
c
c       next is an integer output array of length n. If
c         num(k) = l, then the index of num for the previous
c         occurrence of l is next(k) for any l = 0,1,...,nmax
c         unless next(k) = 0. In this case there is no previous
c         occurrence of l in num.
c
c     Argonne National Laboratory. MINPACK Project. July 1983.
c     Thomas F. Coleman, Burton S. Garbow, Jorge J. More'
c
c     **********
      integer i,j,jinc,jl,ju,k,l
c
c     Determine the arrays next and last.
c
      do 10 i = 0, nmax
         last(i) = 0
   10    continue
      do 20 k = 1, n
         l = num(k)
         next(k) = last(l)
         last(l) = k
   20    continue
      if (mode .eq. 0) return
c
c     Store the pointers to the sorted array in index.
c
      i = 1
      if (mode .gt. 0) then
         jl = 0
         ju = nmax
         jinc = 1
      else
         jl = nmax
         ju = 0
         jinc = -1
         end if
      do 50 j = jl, ju, jinc
         k = last(j)
   30    continue
            if (k .eq. 0) go to 40
            index(i) = k
            i = i + 1
            k = next(k)
            go to 30
   40    continue
   50    continue
      return
c
c     Last card of subroutine numsrt.
c
      end
      subroutine degr(n,indrow,jpntr,indcol,ipntr,ndeg,iwa)
      integer n
      integer indrow(*),jpntr(n+1),indcol(*),ipntr(*),ndeg(n),iwa(n)
c     **********
c
c     subroutine degr
c
c     Given the sparsity pattern of an m by n matrix A,
c     this subroutine determines the degree sequence for
c     the intersection graph of the columns of A.
c
c     In graph-theory terminology, the intersection graph of
c     the columns of A is the loopless graph G with vertices
c     a(j), j = 1,2,...,n where a(j) is the j-th column of A
c     and with edge (a(i),a(j)) if and only if columns i and j
c     have a non-zero in the same row position.
c
c     Note that the value of m is not needed by degr and is
c     therefore not present in the subroutine statement.
c
c     The subroutine statement is
c
c       subroutine degr(n,indrow,jpntr,indcol,ipntr,ndeg,iwa)
c
c     where
c
c       n is a positive integer input variable set to the number
c         of columns of A.
c
c       indrow is an integer input array which contains the row
c         indices for the non-zeroes in the matrix A.
c
c       jpntr is an integer input array of length n + 1 which
c         specifies the locations of the row indices in indrow.
c         The row indices for column j are
c
c               indrow(k), k = jpntr(j),...,jpntr(j+1)-1.
c
c         Note that jpntr(n+1)-1 is then the number of non-zero
c         elements of the matrix A.
c
c       indcol is an integer input array which contains the
c         column indices for the non-zeroes in the matrix A.
c
c       ipntr is an integer input array of length m + 1 which
c         specifies the locations of the column indices in indcol.
c         The column indices for row i are
c
c               indcol(k), k = ipntr(i),...,ipntr(i+1)-1.
c
c         Note that ipntr(m+1)-1 is then the number of non-zero
c         elements of the matrix A.
c
c       ndeg is an integer output array of length n which
c         specifies the degree sequence. The degree of the
c         j-th column of A is ndeg(j).
c
c       iwa is an integer work array of length n.
c
c     Argonne National Laboratory. MINPACK Project. July 1983.
c     Thomas F. Coleman, Burton S. Garbow, Jorge J. More'
c
c     **********
      integer ic,ip,ir,jcol,jp
c
c     Initialization block.
c
      do 10 jp = 1, n
         ndeg(jp) = 0
         iwa(jp) = 0
   10    continue
c
c     Compute the degree sequence by determining the contributions
c     to the degrees from the current(jcol) column and further
c     columns which have not yet been considered.
c
      do 40 jcol = 2, n
         iwa(jcol) = n
c
c        Determine all positions (ir,jcol) which correspond
c        to non-zeroes in the matrix.
c
         do 30 jp = jpntr(jcol), jpntr(jcol+1)-1
            ir = indrow(jp)
c
c           For each row ir, determine all positions (ir,ic)
c           which correspond to non-zeroes in the matrix.
c
            do 20 ip = ipntr(ir), ipntr(ir+1)-1
               ic = indcol(ip)
c
c              Array iwa marks columns which have contributed to
c              the degree count of column jcol. Update the degree
c              counts of these columns as well as column jcol.
c
               if (iwa(ic) .lt. jcol) then
                  iwa(ic) = jcol
                  ndeg(ic) = ndeg(ic) + 1
                  ndeg(jcol) = ndeg(jcol) + 1
                  end if
   20          continue
   30       continue
   40    continue
      return
c
c     Last card of subroutine degr.
c
      end
      subroutine slo(n,indrow,jpntr,indcol,ipntr,ndeg,list,
     *               maxclq,iwa1,iwa2,iwa3,iwa4)
      integer n,maxclq
      integer indrow(*),jpntr(n+1),indcol(*),ipntr(*),ndeg(n),
     *        list(n),iwa1(0:n-1),iwa2(n),iwa3(n),iwa4(n)
c     **********
c
c     subroutine slo
c
c     Given the sparsity pattern of an m by n matrix A, this
c     subroutine determines the smallest-last ordering of the
c     columns of A.
c
c     The smallest-last ordering is defined for the loopless
c     graph G with vertices a(j), j = 1,2,...,n where a(j) is the
c     j-th column of A and with edge (a(i),a(j)) if and only if
c     columns i and j have a non-zero in the same row position.
c
c     The smallest-last ordering is determined recursively by
c     letting list(k), k = n,...,1 be a column with least degree
c     in the subgraph spanned by the un-ordered columns.
c
c     Note that the value of m is not needed by slo and is
c     therefore not present in the subroutine statement.
c
c     The subroutine statement is
c
c       subroutine slo(n,indrow,jpntr,indcol,ipntr,ndeg,list,
c                      maxclq,iwa1,iwa2,iwa3,iwa4)
c
c     where
c
c       n is a positive integer input variable set to the number
c         of columns of A.
c
c       indrow is an integer input array which contains the row
c         indices for the non-zeroes in the matrix A.
c
c       jpntr is an integer input array of length n + 1 which
c         specifies the locations of the row indices in indrow.
c         The row indices for column j are
c
c               indrow(k), k = jpntr(j),...,jpntr(j+1)-1.
c
c         Note that jpntr(n+1)-1 is then the number of non-zero
c         elements of the matrix A.
c
c       indcol is an integer input array which contains the
c         column indices for the non-zeroes in the matrix A.
c
c       ipntr is an integer input array of length m + 1 which
c         specifies the locations of the column indices in indcol.
c         The column indices for row i are
c
c               indcol(k), k = ipntr(i),...,ipntr(i+1)-1.
c
c         Note that ipntr(m+1)-1 is then the number of non-zero
c         elements of the matrix A.
c
c       ndeg is an integer input array of length n which specifies
c         the degree sequence. The degree of the j-th column
c         of A is ndeg(j).
c
c       list is an integer output array of length n which specifies
c         the smallest-last ordering of the columns of A. The j-th
c         column in this order is list(j).
c
c       maxclq is an integer output variable set to the size
c         of the largest clique found during the ordering.
c
c       iwa1,iwa2,iwa3, and iwa4 are integer work arrays of length n.
c
c     Subprograms called
c
c       FORTRAN-supplied ... min
c
c     Argonne National Laboratory. MINPACK Project. August 1984.
c     Thomas F. Coleman, Burton S. Garbow, Jorge J. More'
c
c     **********
      integer ic,ip,ir,jcol,jp,mindeg,numdeg,numord
c
c     Initialization block.
c
      mindeg = n
      do 10 jp = 1, n
         iwa1(jp-1) = 0
         iwa4(jp) = n
         list(jp) = ndeg(jp)
         mindeg = min(mindeg,ndeg(jp))
   10    continue
c
c     Create a doubly-linked list to access the degrees of the
c     columns. The pointers for the linked list are as follows.
c
c     Each un-ordered column ic is in a list (the degree list)
c     of columns with the same degree.
c
c     iwa1(numdeg) is the first column in the numdeg list
c     unless iwa1(numdeg) = 0. In this case there are
c     no columns in the numdeg list.
c
c     iwa2(ic) is the column before ic in the degree list
c     unless iwa2(ic) = 0. In this case ic is the first
c     column in this degree list.
c
c     iwa3(ic) is the column after ic in the degree list
c     unless iwa3(ic) = 0. In this case ic is the last
c     column in this degree list.
c
c     If ic is an un-ordered column, then list(ic) is the
c     degree of ic in the graph induced by the un-ordered
c     columns. If jcol is an ordered column, then list(jcol)
c     is the smallest-last order of column jcol.
c
      do 20 jp = 1, n
         numdeg = ndeg(jp)
         iwa2(jp) = 0
         iwa3(jp) = iwa1(numdeg)
         if (iwa1(numdeg) .gt. 0) iwa2(iwa1(numdeg)) = jp
         iwa1(numdeg) = jp
   20    continue
      maxclq = 0
      numord = n
c
c     Beginning of iteration loop.
c
   30 continue
c
c        Choose a column jcol of minimal degree mindeg.
c
   40    continue
            jcol = iwa1(mindeg)
            if (jcol .gt. 0) go to 50
            mindeg = mindeg + 1
            go to 40
   50    continue
         list(jcol) = numord
c
c        Mark the size of the largest clique
c        found during the ordering.
c
         if (mindeg+1 .eq. numord .and. maxclq .eq. 0)
     *       maxclq = numord
c
c        Termination test.
c
         numord = numord - 1
         if (numord .eq. 0) go to 80
c
c        Delete column jcol from the mindeg list.
c
         iwa1(mindeg) = iwa3(jcol)
         if (iwa3(jcol) .gt. 0) iwa2(iwa3(jcol)) = 0
c
c        Find all columns adjacent to column jcol.
c
         iwa4(jcol) = 0
c
c        Determine all positions (ir,jcol) which correspond
c        to non-zeroes in the matrix.
c
         do 70 jp = jpntr(jcol), jpntr(jcol+1)-1
            ir = indrow(jp)
c
c           For each row ir, determine all positions (ir,ic)
c           which correspond to non-zeroes in the matrix.
c
            do 60 ip = ipntr(ir), ipntr(ir+1)-1
               ic = indcol(ip)
c
c              Array iwa4 marks columns which are adjacent to
c              column jcol.
c
               if (iwa4(ic) .gt. numord) then
                  iwa4(ic) = numord
c
c                 Update the pointers to the current degree lists.
c
                  numdeg = list(ic)
                  list(ic) = list(ic) - 1
                  mindeg = min(mindeg,list(ic))
c
c                 Delete column ic from the numdeg list.
c
                  if (iwa2(ic) .eq. 0) then
                     iwa1(numdeg) = iwa3(ic)
                  else
                     iwa3(iwa2(ic)) = iwa3(ic)
                     end if
                  if (iwa3(ic) .gt. 0) iwa2(iwa3(ic)) = iwa2(ic)
c
c                 Add column ic to the numdeg-1 list.
c
                  iwa2(ic) = 0
                  iwa3(ic) = iwa1(numdeg-1)
                  if (iwa1(numdeg-1) .gt. 0) iwa2(iwa1(numdeg-1)) = ic
                  iwa1(numdeg-1) = ic
                  end if
   60          continue
   70       continue
c
c        End of iteration loop.
c
         go to 30
   80 continue
c
c     Invert the array list.
c
      do 90 jcol = 1, n
         iwa2(list(jcol)) = jcol
   90    continue
      do 100 jp = 1, n
         list(jp) = iwa2(jp)
  100    continue
      return
c
c     Last card of subroutine slo.
c
      end
      subroutine setr(m,n,indrow,jpntr,indcol,ipntr,iwa)
      integer m,n
      integer indrow(*),jpntr(n+1),indcol(*),ipntr(m+1),iwa(m)
c     **********
c
c     subroutine setr
c
c     Given a column-oriented definition of the sparsity pattern
c     of an m by n matrix A, this subroutine determines a
c     row-oriented definition of the sparsity pattern of A.
c
c     On input the column-oriented definition is specified by
c     the arrays indrow and jpntr. On output the row-oriented
c     definition is specified by the arrays indcol and ipntr.
c
c     The subroutine statement is
c
c       subroutine setr(m,n,indrow,jpntr,indcol,ipntr,iwa)
c
c     where
c
c       m is a positive integer input variable set to the number
c         of rows of A.
c
c       n is a positive integer input variable set to the number
c         of columns of A.
c
c       indrow is an integer input array which contains the row
c         indices for the non-zeroes in the matrix A.
c
c       jpntr is an integer input array of length n + 1 which
c         specifies the locations of the row indices in indrow.
c         The row indices for column j are
c
c               indrow(k), k = jpntr(j),...,jpntr(j+1)-1.
c
c         Note that jpntr(n+1)-1 is then the number of non-zero
c         elements of the matrix A.
c
c       indcol is an integer output array which contains the
c         column indices for the non-zeroes in the matrix A.
c
c       ipntr is an integer output array of length m + 1 which
c         specifies the locations of the column indices in indcol.
c         The column indices for row i are
c
c               indcol(k), k = ipntr(i),...,ipntr(i+1)-1.
c
c         Note that ipntr(1) is set to 1 and that ipntr(m+1)-1 is
c         then the number of non-zero elements of the matrix A.
c
c       iwa is an integer work array of length m.
c
c     Argonne National Laboratory. MINPACK Project. July 1983.
c     Thomas F. Coleman, Burton S. Garbow, Jorge J. More'
c
c     **********
      integer ir,jcol,jp
c
c     Store in array iwa the counts of non-zeroes in the rows.
c
      do 10 ir = 1, m
         iwa(ir) = 0
   10    continue
      do 20 jp = 1, jpntr(n+1)-1
         iwa(indrow(jp)) = iwa(indrow(jp)) + 1
   20    continue
c
c     Set pointers to the start of the rows in indcol.
c
      ipntr(1) = 1
      do 30 ir = 1, m
         ipntr(ir+1) = ipntr(ir) + iwa(ir)
         iwa(ir) = ipntr(ir)
   30    continue
c
c     Fill indcol.
c
      do 50 jcol = 1, n
         do 40 jp = jpntr(jcol), jpntr(jcol+1)-1
            ir = indrow(jp)
            indcol(iwa(ir)) = jcol
            iwa(ir) = iwa(ir) + 1
   40       continue
   50    continue
      return
c
c     Last card of subroutine setr.
c
      end
      subroutine dsfdsp(n,nint,nnz,indrow,indcol)
      integer n, nint, nnz
      integer indrow(*), indcol(*)
c     **********
c
c     Subroutine dsfdsp
c
c     This subroutine defines the sparsity pattern of the Jacobian
c     matrix for the swirling flow between disks problem.
c
c     The subroutine statement is
c
c       subroutine dsfdsp(n,nint,nnz,indrow,indcol)
c
c     where
c
c       n is an integer variable.
c         On entry n is the number of variables. n = 14*nint.
c         On exit n is unchanged.
c
c       nint is an integer variable.
c         On entry nint is the number of subintervals in the k-stage
c            collocation.
c         On exit nint is unchanged.
c
c       nnz is an integer variable.
c         On entry nnz need not be specified
c         On exit nnz is set to the number of nonzero index pairs
c            in the sparsity structure.
c
c       indrow is an integer array of dimension at least nnz.
c         On entry indrow need not be specified.
c         On exit indrow contains the row indices of the nonzeros
c            in the sparsity structure of the Jacobian matrix.
c
c       indcol is an integer array of dimension at least nnz.
c         On entry indcol need not be specified.
c         On exit indcol contains the column indices of the nonzeros
c            in the sparsity structure of the Jacobian matrix.
c
c     MINPACK-2 Project. November 1993.
c     Argonne National Laboratory and University of Minnesota.
c     Brett M. Averick, R. S. Maier, G. L. Xue, R. G. Carter.
c
c     **********
      integer bc, cpts, dim, fdeg, gdeg, mdeg, npi
      parameter (bc=3,cpts=4,fdeg=4,gdeg=2,mdeg=4)
      parameter (dim=mdeg+cpts-1,npi=2*cpts+gdeg+fdeg)

      integer eqn1, eqn2, i, j, k, m, var1, var2

c     Compute the sparsity structure.

      nnz = 0

c     Nonzeroes contributed by boundary equations at t = 0.

      indrow(1) = 1
      indcol(1) = 1
      indrow(2) = 2
      indcol(2) = 2
      indrow(3) = 3
      indcol(3) = cpts + fdeg + 1
      nnz = 3

c     Nonzeroes contributed by collocation equations.

      do 40 i = 1, nint
         var1 = (i-1)*npi
         eqn1 = var1 + bc
         var2 = var1 + cpts + fdeg
         eqn2 = eqn1 + cpts
         do 30 k = 1, cpts
            do 10 j = 1, cpts + fdeg
               nnz = nnz + 1
               indrow(nnz) = eqn1 + k
               indcol(nnz) = var1 + j
               nnz = nnz + 1
               indrow(nnz) = eqn2 + k
               indcol(nnz) = var1 + j
   10       continue
            do 20 j = 1, cpts + gdeg
               nnz = nnz + 1
               indrow(nnz) = eqn1 + k
               indcol(nnz) = var2 + j
               nnz = nnz + 1
               indrow(nnz) = eqn2 + k
               indcol(nnz) = var2 + j
   20       continue
   30    continue
   40 continue

c     Nonzeroes contributed by continuity equations.

      do 90 i = 1, nint - 1
         var1 = (i-1)*npi
         eqn1 = var1 + bc + 2*cpts
         var2 = var1 + fdeg + cpts
         eqn2 = eqn1 + fdeg
         do 60 m = 1, fdeg
            nnz = nnz + 1
            indrow(nnz) = eqn1 + m
            indcol(nnz) = var1 + npi + m
            do 50 j = m, cpts + fdeg
               nnz = nnz + 1
               indrow(nnz) = eqn1 + m
               indcol(nnz) = var1 + j
   50       continue
   60    continue
         do 80 m = 1, gdeg
            nnz = nnz + 1
            indrow(nnz) = eqn2 + m
            indcol(nnz) = var2 + npi + m
            do 70 j = m, cpts + gdeg
               nnz = nnz + 1
               indrow(nnz) = eqn2 + m
               indcol(nnz) = var2 + j
   70       continue
   80    continue
   90 continue

c     Nonzeroes contributed by boundary equations at t = 1.

      var1 = n - npi
      eqn1 = var1 + bc + 2*cpts
      var2 = var1 + fdeg + cpts
      eqn2 = eqn1 + fdeg
      do 110 m = 1, 2
         do 100 j = m, cpts + fdeg
            nnz = nnz + 1
            indrow(nnz) = eqn1 + m
            indcol(nnz) = var1 + j
  100    continue
  110 continue
      do 120 j = 1, cpts + gdeg
         nnz = nnz + 1
         indrow(nnz) = n
         indcol(nnz) = var2 + j
  120 continue

      end

      subroutine dsm(m,n,npairs,indrow,indcol,ngrp,maxgrp,mingrp,
     *               info,ipntr,jpntr,iwa,liwa)
      integer m,n,npairs,maxgrp,mingrp,info,liwa
      integer indrow(npairs),indcol(npairs),ngrp(n),
     *        ipntr(m+1),jpntr(n+1),iwa(liwa)
c     **********
c
c     subroutine dsm
c
c     Given the sparsity pattern of an m by n matrix A, this
c     subroutine determines a partition of the columns of A
c     consistent with the direct determination of A.
c
c     The sparsity pattern of the matrix A is specified by
c     the arrays indrow and indcol. On input the indices
c     for the non-zero elements of A are
c
c           indrow(k),indcol(k), k = 1,2,...,npairs.
c
c     The (indrow,indcol) pairs may be specified in any order.
c     Duplicate input pairs are permitted, but the subroutine
c     eliminates them.
c
c     The subroutine partitions the columns of A into groups
c     such that columns in the same group do not have a
c     non-zero in the same row position. A partition of the
c     columns of A with this property is consistent with the
c     direct determination of A.
c
c     The subroutine statement is
c
c       subroutine dsm(m,n,npairs,indrow,indcol,ngrp,maxgrp,mingrp,
c                      info,ipntr,jpntr,iwa,liwa)
c
c     where
c
c       m is a positive integer input variable set to the number
c         of rows of A.
c
c       n is a positive integer input variable set to the number
c         of columns of A.
c
c       npairs is a positive integer input variable set to the
c         number of (indrow,indcol) pairs used to describe the
c         sparsity pattern of A.
c
c       indrow is an integer array of length npairs. On input indrow
c         must contain the row indices of the non-zero elements of A.
c         On output indrow is permuted so that the corresponding
c         column indices are in non-decreasing order. The column
c         indices can be recovered from the array jpntr.
c
c       indcol is an integer array of length npairs. On input indcol
c         must contain the column indices of the non-zero elements of
c         A. On output indcol is permuted so that the corresponding
c         row indices are in non-decreasing order. The row indices
c         can be recovered from the array ipntr.
c
c       ngrp is an integer output array of length n which specifies
c         the partition of the columns of A. Column jcol belongs
c         to group ngrp(jcol).
c
c       maxgrp is an integer output variable which specifies the
c         number of groups in the partition of the columns of A.
c
c       mingrp is an integer output variable which specifies a lower
c         bound for the number of groups in any consistent partition
c         of the columns of A.
c
c       info is an integer output variable set as follows. For
c         normal termination info = 1. If m, n, or npairs is not
c         positive or liwa is less than max(m,6*n), then info = 0.
c         If the k-th element of indrow is not an integer between
c         1 and m or the k-th element of indcol is not an integer
c         between 1 and n, then info = -k.
c
c       ipntr is an integer output array of length m + 1 which
c         specifies the locations of the column indices in indcol.
c         The column indices for row i are
c
c               indcol(k), k = ipntr(i),...,ipntr(i+1)-1.
c
c         Note that ipntr(m+1)-1 is then the number of non-zero
c         elements of the matrix A.
c
c       jpntr is an integer output array of length n + 1 which
c         specifies the locations of the row indices in indrow.
c         The row indices for column j are
c
c               indrow(k), k = jpntr(j),...,jpntr(j+1)-1.
c
c         Note that jpntr(n+1)-1 is then the number of non-zero
c         elements of the matrix A.
c
c       iwa is an integer work array of length liwa.
c
c       liwa is a positive integer input variable not less than
c         max(m,6*n).
c
c     Subprograms called
c
c       MINPACK-supplied ... degr,ido,numsrt,seq,setr,slo,srtdat
c
c       FORTRAN-supplied ... max
c
c     Argonne National Laboratory. MINPACK Project. December 1984.
c     Thomas F. Coleman, Burton S. Garbow, Jorge J. More'
c
c     **********
      integer i,ir,j,jp,k,maxclq,nnz,numgrp
c
c     Check the input data.
c
      info = 0
      if (m .lt. 1 .or. n .lt. 1 .or. npairs .lt. 1 .or.
     *    liwa .lt. max(m,6*n)) return
      do 10 k = 1, npairs
         info = -k
         if (indrow(k) .lt. 1 .or. indrow(k) .gt. m .or.
     *       indcol(k) .lt. 1 .or. indcol(k) .gt. n) return
   10    continue
      info = 1
c
c     Sort the data structure by columns.
c
      call srtdat(n,npairs,indrow,indcol,jpntr,iwa)
c
c     Compress the data and determine the number of
c     non-zero elements of A.
c
      do 20 i = 1, m
         iwa(i) = 0
   20    continue
      nnz = 0
      do 40 j = 1, n
         k = nnz
         do 30 jp = jpntr(j), jpntr(j+1)-1
            ir = indrow(jp)
            if (iwa(ir) .ne. j) then
               nnz = nnz + 1
               indrow(nnz) = ir
               iwa(ir) = j
               end if
   30       continue
         jpntr(j) = k + 1
   40    continue
      jpntr(n+1) = nnz + 1
c
c     Extend the data structure to rows.
c
      call setr(m,n,indrow,jpntr,indcol,ipntr,iwa)
c
c     Determine a lower bound for the number of groups.
c
      mingrp = 0
      do 50 i = 1, m
         mingrp = max(mingrp,ipntr(i+1)-ipntr(i))
   50    continue
c
c     Determine the degree sequence for the intersection
c     graph of the columns of A.
c
      call degr(n,indrow,jpntr,indcol,ipntr,iwa(5*n+1),iwa(n+1))
c
c     Color the intersection graph of the columns of A
c     with the smallest-last (SL) ordering.
c
      call slo(n,indrow,jpntr,indcol,ipntr,iwa(5*n+1),iwa(4*n+1),
     *         maxclq,iwa(1),iwa(n+1),iwa(2*n+1),iwa(3*n+1))
      call seq(n,indrow,jpntr,indcol,ipntr,iwa(4*n+1),ngrp,maxgrp,
     *         iwa(n+1))
      mingrp = max(mingrp,maxclq)
c
c     Exit if the smallest-last ordering is optimal.
c
      if (maxgrp .eq. mingrp) return
c
c     Color the intersection graph of the columns of A
c     with the incidence-degree (ID) ordering.
c
      call ido(m,n,indrow,jpntr,indcol,ipntr,iwa(5*n+1),iwa(4*n+1),
     *         maxclq,iwa(1),iwa(n+1),iwa(2*n+1),iwa(3*n+1))
      call seq(n,indrow,jpntr,indcol,ipntr,iwa(4*n+1),iwa(1),numgrp,
     *         iwa(n+1))
      mingrp = max(mingrp,maxclq)
c
c     Retain the better of the two orderings so far.
c
      if (numgrp .lt. maxgrp) then
         maxgrp = numgrp
         do 60 j = 1, n
            ngrp(j) = iwa(j)
   60       continue
c
c        Exit if the incidence-degree ordering is optimal.
c
         if (maxgrp .eq. mingrp) return
         end if
c
c     Color the intersection graph of the columns of A
c     with the largest-first (LF) ordering.
c
      call numsrt(n,n-1,iwa(5*n+1),-1,iwa(4*n+1),iwa(2*n+1),iwa(n+1))
      call seq(n,indrow,jpntr,indcol,ipntr,iwa(4*n+1),iwa(1),numgrp,
     *         iwa(n+1))
c
c     Retain the best of the three orderings and exit.
c
      if (numgrp .lt. maxgrp) then
         maxgrp = numgrp
         do 70 j = 1, n
            ngrp(j) = iwa(j)
   70       continue
         end if
      return
c
c     Last card of subroutine dsm.
c
      end
