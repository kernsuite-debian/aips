      SUBROUTINE ZDOPR3 (IVOL, IBMLUN, NCOPY, FILNAM, ISIZE, INBLK,
     *   IERR)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C   ZDOPR3 will read a bit map such as produced by PRTPL and plot it
C   on the VERSATEC printer-plotter.  This subroutine can be used
C   to replace ZDOPRT in PRTPL when output to the VERSATEC is not
C   spooled.  This routine expects the device name of the VERSATEC
C   to be LPA0:.  Edit the DATA statement in this routine if you have
C   a different device name.
C     INPUTS:
C        IVOL      volume no. of bit map disk (1 rel) I
C        IBMLUN    bit map logical unit number. I
C        NCOPY     NOT IMPLIMENTED
C        FILNAM    physical file name of bit map. I(12)
C        ISIZE     I    size of INBLK in words
C      IN/Out:
C        INBLK     I(*)    scratch buffer
C     OUTPUTS:
C        IERR      error return code.
C                    0 - good.
C                   >0 - an error occurred.
C   VAX VERSION
C-----------------------------------------------------------------------
      REAL       READ
      INTEGER    INBLK(1), IOBLK(132), SP(12), ISIZE, N1, FILNAM(12)
      INTEGER    LP(12), IFCA(6), IFCB(6), LPA0(12)
      INTEGER    IOSB(4), IBMIND, IERR, IRESET, N0, IFORM, ICLEAR
      INTEGER    N7, N132, N264, IEOT, NCOPY
      INTEGER    ILEN, INPOS, IOPOS, IBMLUN, IVOL, IPLOT
      INTEGER    IRRN(2), IMLINE, ILINE, NBYTES
      LOGICAL    EXCL, NOMAP, WAIT, MAP
      DOUBLE PRECISION
     *       RBYTES
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (EXCL, WAIT, MAP)
      DATA IPLOT /0/, IRESET /2/, ICLEAR /4/, IEOT /8/, IFORM /16/
      DATA N0, N1, N7, N132, N264 / 0, 1, 7, 132, 264/
      DATA LPA0 /'LP','A0',': ',9*'  '/
      DATA READ /'READ'/
      DATA EXCL /.TRUE./,     NOMAP /.FALSE./
C-----------------------------------------------------------------------
C                                       Open the bit map file.
      CALL ZOPEN (IBMLUN, IBMIND, IVOL, FILNAM, MAP, EXCL, WAIT, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT(N7)
         GO TO 999
C                                       Assign chan to plotter, init.
 10   CONTINUE
      CALL ZQASSN (IFCB, LPA0, NOMAP, IERR)
      IF (IERR.NE.0) GO TO 15
      CALL ZQIOV (IRESET, IFCB, IOBLK, N0, IOSB, IERR)
      IF (IERR.NE.0) GO TO 15
      CALL ZQIOV (IFORM, IFCB, IOBLK, N0, IOSB, IERR)
      IF (IERR.NE.0) GO TO 15
      CALL ZQIOV (ICLEAR, IFCB, IOBLK, N0, IOSB, IERR)
      IF (IERR.EQ.0) GO TO 20
15       CONTINUE
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (N7)
         GO TO 990
C                                       Initial buffer counters.
 20   CONTINUE
      INPOS = 257
      IOPOS = 1
      IRRN(1) = 1
      IRRN(2) = 0
      NBYTES = 2 * ISIZE
      ILINE = 0
      CALL ZMI3 (READ, IBMLUN, IBMIND, IRRN, NBYTES, INBLK, N1, IERR)
      IF (IERR.EQ.0) CALL ZWAIT (IBMLUN, IBMIND, N1, IERR)
      IF (IERR.NE.0) GO TO 920
      IRRN(1) = IRRN(1) + NBYTES/NBPS
      IMLINE = INBLK(1)
      RBYTES = 512.0D0 + 264.0D0 * IMLINE - NBYTES
C                                       See if we need to read new buff
 100  CONTINUE
         IF (INPOS.LE.ISIZE) GO TO 110
            IF (RBYTES.LT.NBYTES) NBYTES = RBYTES + 0.1
            CALL ZMI3 (READ, IBMLUN, IBMIND, IRRN, NBYTES, INBLK, N1,
     *         IERR)
            IF (IERR.EQ.0) CALL ZWAIT (IBMLUN, IBMIND, N1, IERR)
            IF (IERR.NE.0) GO TO 920
            RBYTES = RBYTES - NBYTES
            IRRN(1) = IRRN(1) + NBYTES/NBPS
            INPOS = 1
C                                       Determine length to copy.
 110     ILEN = MIN (ISIZE+1-INPOS, 133-IOPOS)
         CALL ZBYTFL (ILEN, INBLK(INPOS), IOBLK(IOPOS))
         IOPOS = IOPOS + ILEN
         INPOS = INPOS + ILEN
C                                       Write to VERSATEC spool file.
         IF (IOPOS.LE.132) GO TO 100
            CALL ZQIOV (IPLOT, IFCB, IOBLK, N264, IOSB, IERR)
            ILINE = ILINE + 1
            IOPOS = 1
            IF (ILINE.LT.IMLINE) GO TO 100
C                                       print 1000 lines of blank
C                                       to move page at correct rate
            CALL FILL (N132, N0, IOBLK)
            DO 130 I = 1,1000
               CALL ZQIOV (IPLOT, IFCB, IOBLK, N264, IOSB, IERR)
               IF (IERR.NE.0) GO TO 940
 130           CONTINUE
            GOTO 990
C                                       Error handling.
 920  WRITE (MSGTXT,1920) IERR
      CALL MSGWRT (N7)
      GO TO 990
 940  WRITE (MSGTXT,1940)
      CALL MSGWRT (N7)
C                                       Issue final form feed and
C                                       close files.
 990  CALL ZQIOV (IFORM, IFCB, IOBLK, N0, IOSB, IERR)
      CALL ZQIOV (IRESET, IFCB, IOBLK, N0, IOSB, IERR)
      CALL ZQIOV (IEOT, IFCB, IOBLK, N0, IOSB, IERR)
      CALL ZQDASS (IFCB, NOMAP, IERR)
      CALL ZQCLOS (IFCA, NOMAP, IERR)
      CALL ZCLOSE (IBMLUN,IBMIND,IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZDOPRT: UNABLE TO OPEN BIT MAP FILE. IERR=',I4)
 1010 FORMAT ('ZDOPRT: UNABLE TO OPEN THE PLOTTER. IERR=', I4)
 1020 FORMAT (A2)
 1110 FORMAT (66A2)
 1920 FORMAT ('ZDOPRT: ZFI3 READ ERROR. IERR=',I4)
 1940 FORMAT ('ZDOPRT: FORTRAN WRITE ERROR TO PLOTTER')
      END
