      SUBROUTINE FINDVO (MXNUM, DISK, CNO, CATBLK, ISUB, IFQ, NSOUWD,
     *   SOUWAN, BTIME, ETIME, NVORNG, VORNG)
C-----------------------------------------------------------------------
C! finds selected ranges of visibility record numbers
C# UV
C-----------------------------------------------------------------------
C;  Copyright (C) 2011
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
C   Checks the NX table to match up the start position in the UV data
C   with the file offset
C   Inputs:
C      MXNUM    I        Maximum number intervals allowed
C      DISK     I        Disk file number
C      CNO      I        Catalog number
C      CATBLK   I(*)     Data set header
C      ISUB     I        Subarray
C      IFQ      I        FreqID
C      NSOUWD   I        Number sources in list
C      SOUWAN   I(*)     Source numbers wanted
C      BTIME    R        Start time
C      ETIME    R        Start time
C   In/Out in common
C      NVORNG   I        Number VO range's
C      VORNG    I(2,*)   Range of vis number (VO really vis#-1)
C-----------------------------------------------------------------------
      INTEGER   MXNUM, disk, cno, catblk(256), ISUB, IFQ, NSOUWD,
     *   SOUWAN(*), NVORNG, VORNG(2,*)
      REAL      BTIME, ETIME
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NXBUFF(512), VER, IERR, NXKOLS(MAXNXC), NXNUMV(MAXNXC),
     *   INXRNO, IDSOUR, SUBARR, FREQID, VSTART, VEND, I, IROW, NROW,
     *   LUNNX, VOIN(2), LUNTMP
      REAL      TIME, DTIME, EPS
      LOGICAL   INONE
      INCLUDE 'INCS:DMSG.INC'
      DATA EPS /1.157E-6/
C-----------------------------------------------------------------------
      VOIN(1) = VORNG(1,1)
      VOIN(2) = VORNG(2,1)
C                                       open index table
      VER = 0
      LUNNX = LUNTMP (1)
      CALL NDXINI ('READ', NXBUFF, DISK, CNO, VER, CATBLK, LUNNX,
     *   INXRNO, NXKOLS, NXNUMV, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN NX TABLE'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       read
      NROW = NXBUFF(5)
      NVORNG = 0
      INONE = .FALSE.
      DO 20 IROW = 1,NROW
         INXRNO = IROW
         CALL TABNDX ('READ', NXBUFF, INXRNO, NXKOLS, NXNUMV, TIME,
     *      DTIME, IDSOUR, SUBARR, VSTART, VEND, FREQID, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ NX TABLE'
            CALL MSGWRT (8)
            GO TO 30
         ELSE IF (IERR.EQ.0) THEN
            IF (((ISUB.LE.0) .OR. (SUBARR.LE.0) .OR. (ISUB.EQ.SUBARR))
     *         .AND. ((IFQ.LE.0) .OR. (FREQID.LE.0) .OR.
     *         (IFQ.EQ.FREQID))) THEN
               IF ((BTIME.LE.TIME+DTIME/2.0-EPS) .AND.
     *            (ETIME.GE.TIME-DTIME/2.0) .AND. (VEND.GE.VOIN(1)))
     *            THEN
                  IF (NSOUWD.GT.0) THEN
                     DO 10 I = 1,NSOUWD
                        IF (IDSOUR.EQ.SOUWAN(I)) GO TO 15
 10                     CONTINUE
                     INONE = .FALSE.
                     GO TO 20
                     END IF
 15               IF (.NOT.INONE) THEN
                     INONE = .TRUE.
                     NVORNG = NVORNG + 1
                     IF (NVORNG.GT.MXNUM) THEN
                        MSGTXT = 'FINDVO: TOO MANY SCANS INCLUDED'
                        CALL MSGWRT (7)
                        NVORNG = 1
                        VORNG(1,1) = VOIN(1)
                        VORNG(2,1) = VOIN(2)
                        GO TO 30
                        END IF
                     VORNG(1,NVORNG) = MAX (VSTART, VOIN(1))
                     VORNG(2,NVORNG) = VEND
                  ELSE
                     VORNG(2,NVORNG) = VEND
                     END IF
                  END IF
               END IF
            END IF
 20      CONTINUE
C                                       close and exit
 30   CALL TABNDX ('CLOS', NXBUFF, INXRNO, NXKOLS, NXNUMV, TIME, DTIME,
     *   IDSOUR, SUBARR, VSTART, VEND, FREQID, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FNDVO: ERROR',I4,' ON ',A)
      END
