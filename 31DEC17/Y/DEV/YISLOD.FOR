      SUBROUTINE YISLOD (OP, BUF, IERR)
C-----------------------------------------------------------------------
C! loads/unloads program memory of NRAO-ISU device
C# Y3 TV-appl
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
C   YISLOD loads the program memory of the NRAO Image Storage Unit.
C   It loads the program memory beginning at an address and with a byte
C   count and program found in the file ID1000tv where tv is the TV #.
C   Input:
C      OP     C*4       'READ' : ISU -> disk, 'WRIT'
C   Outputs:
C      BUF    I(256)    Scratch
C      IERR   I         Error code: 0 => ok, 2 => input error
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   BUF(256), IERR
C
      INTEGER   N, ADDR, NWORDS, GOADDR, IN, IG, IREC, LREC
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF ((ISUNUM.GT.0) .AND. (NTVDEV.GT.0)) GO TO 10
         IF (ISUNUM.LE.0) WRITE (MSGTXT,1000)
         IF (NTVDEV.LE.0) WRITE (MSGTXT,1001)
         IERR = 2
         GO TO 990
C                                       Read control parms
 10   IREC = 2
      CALL ZFIO ('READ', TVLUN2, TVIND2, IREC, BUF, IERR)
      IF (IERR.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) N, IREC, IERR
         GO TO 990
C                                       parms
 20   ADDR = BUF(1)
      NWORDS = BUF(2)
      GOADDR = BUF(3)
      LREC = (NWORDS - 1) / 256 + 3
      DO 40 IREC = 3,LREC
         IF (OP.EQ.'READ') GO TO 30
            CALL ZFIO ('READ', TVLUN2, TVIND2, IREC, BUF, IERR)
            IF (IERR.EQ.0) GO TO 30
               WRITE (MSGTXT,1010) N, IREC, IERR
               GO TO 990
 30      IN = MIN (256, NWORDS)
         IG = 0
         IF (IREC.EQ.LREC) IG = GOADDR
         CALL YISMPM (OP, ADDR, IN, IG, BUF, IERR)
         IF (IREC.NE.LREC) ADDR = ADDR + 512
         NWORDS = NWORDS - 256
         IF (IERR.EQ.0) GO TO 35
            WRITE (MSGTXT,1030) N, IREC, IERR
            GO TO 990
 35      IF (OP.EQ.'WRIT') GO TO 40
            CALL ZFIO ('WRIT', TVLUN2, TVIND2, IREC, BUF, IERR)
            IF (IERR.EQ.0) GO TO 40
               WRITE (MSGTXT,1010) N, IREC, IERR
               GO TO 990
 40      CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('THERE IS NO IMAGE STORAGE UNIT CONFIGURED')
 1001 FORMAT ('YOU HAVE NOT BEEN ASSIGNED A TV DEVICE')
 1010 FORMAT ('UNABLE TO OPEN ISU DISK',I3,' RECORD',I7,' ERROR',I5)
 1030 FORMAT ('UNABLE TO WRITE TO ISU',I3,' RECORD',I7,' ERROR',I5)
      END
