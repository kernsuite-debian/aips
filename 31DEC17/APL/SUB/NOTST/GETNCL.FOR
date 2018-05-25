      SUBROUTINE GETNCL (DISK, CNO, CATB, ISUB, TABTYP, CALBUF,
     *   NXBUF, IRET)
C-----------------------------------------------------------------------
C! Make list of time intervals in an NX table with CL table records
C# UV EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2013
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
C   GETNCL makes a list of the times in the NX table and of the
C   corresponding record numbers in the CL or SN table
C   Inputs:
C      DISK     I        Disk number
C      CNO      I        Catalog number
C      CATB     I(*)     header
C      ISUB     I        Subarray
C      TABTYP   C*2      table type 'SN' -> okay no NX table
C   Outout
C      CALBUF   I(512)   work buffer for CL table reads
C      NXBUF    I(512)   work buffer for NX table reads
C      IRET     I        error code
C   Output to DNXCL.INC
C
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, CATB(256), ISUB, CALBUF(*), NXBUF(*), IRET
      CHARACTER TABTYP*2
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LUN, LUNTMP, SUBKOL, TIMKOL, RECI(50+15*MAXIF), I, LAST,
     *   J
      DOUBLE PRECISION RECD(50+15*MAXIF)
      REAL      RTIME
      INCLUDE 'INCS:DNXC.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (TIMKOL,CLKOLS(CTDTIM)), (SUBKOL,CLKOLS(CTISUB))
      EQUIVALENCE (RECI, RECD)
C-----------------------------------------------------------------------
      IRET = 0
C                                       get NX table limits
      CURNXC = 0
C                                       single source - ignore scans
      IF (TABTYP.EQ.'SN') THEN
         NUMNXC = 2
         TIMNXC(1) = -1.0
         TIMNXC(2) = 10000.0
         RECNXC(1) = 1
C                                       scan list
      ELSE
         LUN = LUNTMP (1)
         CALL GETNX (LUN, DISK, CNO, CATB, ISUB, NXBUF, NUMNXC, TIMNXC)
         IF (NUMNXC.EQ.0) THEN
            MSGTXT = 'NX TABLE ERROR - CANNOT DO DOCAL'
            IRET = 8
            GO TO 990
C                                       get corresponding record numbers
         ELSE
            LAST = 0
            TIMNXC(1) = TIMNXC(1) - 1.0 / 24.0
            TIMNXC(NUMNXC) = TIMNXC(NUMNXC) + 1.0 / 24.0
            CALL FILL (NUMNXC, 0, RECNXC)
            DO 90 I = 1,NCLINR
               ICLRNO = I
               CALL TABIO ('READ', 0, ICLRNO, RECD, CALBUF, IRET)
               IF (IRET.GT.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READING ' // TABTYP //
     *               ' TABLE'
                  GO TO 990
               ELSE IF (IRET.EQ.0) THEN
C                                       Check SUBARRAY
                  IF ((RECI(SUBKOL).NE.SUBARR) .AND.
     *               (RECI(SUBKOL).GT.0)) GO TO 90
C                                       find time
                  RTIME = RECD(TIMKOL)
                  IF ((LAST.LE.0) .OR. (RTIME.LT.TIMNXC(LAST)) .OR.
     *               (RTIME.GE.TIMNXC(LAST+1))) THEN
                     DO 30 J = 1,NUMNXC
                        IF ((RTIME.GE.TIMNXC(J)) .AND.
     *                     (RTIME.LT.TIMNXC(J+1))) THEN
                           LAST = J
                           RECNXC(J) = I
                           GO TO 90
                           END IF
 30                     CONTINUE
                     END IF
                  END IF
 90            CONTINUE
            END IF
         END IF
      RECNXC(NUMNXC) = NCLINR + 1
      RECNXC(NUMNXC+1) = NCLINR + 1
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETNCL: ERROR',I4,' ON ',A)
      END
