      SUBROUTINE YCOVER (OVER, BUF, IERR)
C-----------------------------------------------------------------------
C! checks for overlapped images on the TV by quadrant
C# Y0 TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1996
C;  Associated Universities, Inc. Washington DC, USA.
C;  Eric W. Greisen
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
C   YCOVER checks to see if there are partially replaced images in any
C   of the TV planes currently visible by quadrant
C   Outputs:
C      OVER   L(4)     T => there are in quadr. I
C      BUF    I(512)   scratch
C      IERR   I        Error code: 0 => ok, other catlg IO error
C   XAS (SSS) Y routine: for image catalog in XAS or disk
C-----------------------------------------------------------------------
      LOGICAL   OVER(4)
      INTEGER   BUF(512), IERR
C
      LOGICAL   L1, L2, L3, L4
      INTEGER   ILUN, IIND, IP, IQ, II, JJ, I, J, LL, I2TMP1, QBLC(4,2),
     *   QTRC(4,2), NR, NR0, ZAND, DAT(4), OPCODE, NWSD
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA ILUN /20/
C-----------------------------------------------------------------------
C                                       on disk
      IF (TVIMGC.LE.0) THEN
         CALL ICOPEN (1, ILUN, IIND, IERR)
         IF (IERR.NE.0) GO TO 999
         LL = (NIMAGE - 1) / 51
         QBLC(1,1) = TVSPLT(1)
         QTRC(1,1) = MAXXTV(1)
         QBLC(1,2) = TVSPLT(2)
         QTRC(1,2) = MAXXTV(2)
         QBLC(2,1) = 1
         QTRC(2,1) = TVSPLT(1)
         QBLC(2,2) = TVSPLT(2)
         QTRC(2,2) = MAXXTV(2)
         QBLC(3,1) = 1
         QTRC(3,1) = TVSPLT(1)
         QBLC(3,2) = 1
         QTRC(3,2) = TVSPLT(2)
         QBLC(4,1) = TVSPLT(1)
         QTRC(4,1) = MAXXTV(1)
         QBLC(4,2) = 1
         QTRC(4,2) = TVSPLT(2)
C                                       Loop over quadrants
         DO 50 IQ = 1,4
            OVER(IQ) = .FALSE.
C                                       Loop over visible planes
 10         DO 40 IP = 1,NGRAY
               I2TMP1 = 2 ** (IP-1)
               IF (ZAND (TVLIMG(IQ), I2TMP1).EQ.0) GO TO 40
               NR0 = 1 + (IP-1) * (1 + NIMAGE + LL)
C                                       Loop over occupied slots
               DO 30 I = 1,NIMAGE
                  IF (MOD(I,51).EQ.1) THEN
                     NR = NR0 + (I-1)/51
                     CALL ZFIO ('READ', ILUN, IIND, NR, BUF(1), IERR)
                     IF (IERR.NE.0) GO TO 900
                     END IF
                  II = 5 * MOD (I-1, 51) + 2
                  IF (BUF(II).LE.0) GO TO 30
                  IF (QBLC(IQ,1).GT.BUF(II+3)) GO TO 30
                  IF (QBLC(IQ,2).GT.BUF(II+4)) GO TO 30
                  IF (QTRC(IQ,1).LT.BUF(II+1)) GO TO 30
                  IF (QTRC(IQ,2).LT.BUF(II+2)) GO TO 30
C                                       Loop over more recent ones
                  DO 25 J = 1,NIMAGE
                     IF (MOD(J,51).EQ.1) THEN
                        NR = NR0 + (J-1)/51
                        CALL ZFIO ('READ', ILUN, IIND, NR, BUF(257),
     *                     IERR)
                        IF (IERR.NE.0) GO TO 900
                        END IF
                     JJ = 5 * MOD (J-1, 51) + 258
                     IF (BUF(JJ).LE.BUF(II)) GO TO 25
                     IF (QBLC(IQ,1).GT.BUF(JJ+3)) GO TO 25
                     IF (QBLC(IQ,2).GT.BUF(JJ+4)) GO TO 25
                     IF (QTRC(IQ,1).LT.BUF(JJ+1)) GO TO 25
                     IF (QTRC(IQ,2).LT.BUF(JJ+2)) GO TO 25
                        L1 = (BUF(JJ+1).GE.BUF(II+1)) .AND.
     *                     (BUF(JJ+1).LE.BUF(II+3))
                        L2 = (BUF(JJ+3).GE.BUF(II+1)) .AND.
     *                     (BUF(JJ+3).LE.BUF(II+3))
                        L3 = (BUF(JJ+2).GE.BUF(II+2)) .AND.
     *                     (BUF(JJ+2).LE.BUF(II+4))
                        L4 = (BUF(JJ+4).GE.BUF(II+2)) .AND.
     *                     (BUF(JJ+4).LE.BUF(II+4))
                        OVER(IQ) = (L1 .AND. L3) .OR. (L2 .AND. L4)
     *                     .OR. (L1 .AND. L4) .OR. (L2 .AND. L3)
                        IF (OVER(IQ)) GO TO 50
 25                  CONTINUE
 30               CONTINUE
 40            CONTINUE
 50         CONTINUE
         GO TO 900
C                                       in XAS
      ELSE
         CALL FILL (4, 0, DAT)
         OPCODE = 34
         CALL ZSSSXF (OPCODE, DAT, 0, BUF, NWSD, IERR)
         IF (IERR.EQ.0) THEN
            CALL ZI8IL (4, 1, BUF, BUF)
            DO 60 IQ = 1,4
               OVER(IQ) = BUF(IQ).GT.0
 60            CONTINUE
         ELSE
            IERR = 2
            WRITE (MSGTXT,1200) IERR
            CALL MSGWRT (6)
            END IF
         GO TO 999
         END IF
C                                       Close cat
 900  CALL ZCLOSE (ILUN, IIND, IQ)
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('YCOVER: XAS IMAGE CATALOG RETURNS ERROR',I5)
      END
