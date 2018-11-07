      SUBROUTINE GETHDR (LUN, CAT, ERROR)
C-----------------------------------------------------------------------
C! get the catalog header for an open file (WaWa)
C# IO-WaWa
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
C   WaWa IO system: Retrieve the catalog header block for a file that
C   is already open (via FILOPN or OPENCF)
C   Inputs:
C      LUN            I      Logical Unit No. of file
C   Outputs:
C      CAT(256)       I      Returned Header block
C      ERROR          I      Error code: 0 => ok
C                                        1 => file not open
C                                       10 => catlg error
C-----------------------------------------------------------------------
      INTEGER   LUN, CAT(256), ERROR
C
      INTEGER   CATER, NOTOPN, WARN, NSUB, IERR, IFIL
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DBUF.INC'
      DATA NOTOPN, CATER, WARN /1, 10, 6/
C-----------------------------------------------------------------------
C                                       Is file open: get ID
      ERROR = NOTOPN
      DO 10 IFIL = 1,EFIL
         IF (FILTAB(POLUN,IFIL).EQ.LUN) GO TO 20
 10      CONTINUE
      GO TO 999
C                                       Read catalog block
 20   NSUB = 2
      ERROR = 0
      CALL CATIO ('READ', FILTAB(POVOL,IFIL), FILTAB(POCAT,IFIL), CAT,
     *   'REST', WBUFF, IERR)
      IF ((IERR.EQ.0) .OR. (IERR.EQ.WARN)) GO TO 999
         ERROR = CATER
         WRITE (MSGTXT,1020) SUBNAM(NSUB), FILTAB(POVOL,IFIL),
     *      FILTAB(POCAT,IFIL), IERR
         CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('GETHDR: ',A,' READ ERROR VOL',I2,' SLOT',I4,' ERROR',I6)
      END
