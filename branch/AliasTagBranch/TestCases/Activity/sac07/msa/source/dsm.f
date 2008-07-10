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
