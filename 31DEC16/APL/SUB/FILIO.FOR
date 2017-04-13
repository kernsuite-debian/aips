      SUBROUTINE FILIO (OP, LUN, REC, DATA, ERROR)
C-----------------------------------------------------------------------
C! reads/writes 256-integer record to non-map file opened by FILOPN
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
C   WaWa IO system:  Read or Write a single record from/to a non-map
C   file which has been opened with FILOPN (256 integers).  Adds a
C   'READ' status to catlg on first call.
C   Inputs:
C      OP          C*4      READ or WRIT
C      LUN         I        File Logical Unit Number
C      REC         I        Which record out of file (1-relative)
C   In/Out:
C      DATA(256)   I        Data record to input or output
C   Output:
C      ERROR       I        Error return from ZFI3
C                           0 => o.k.
C                           1 => file not open
C                           2 => input error e.g. file not opened for
C                                desired operation
C                           3 => i/o error
C                           4 => end of file
C                           5 => beginning of medium
C                           6 => end of medium (from IO system)
C                          10 => catalog error
C   Written by WaWa (4/21/81)
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   LUN, REC, DATA(256), ERROR
C
      CHARACTER CHTM12*12, CHTM6*6, DUM2*2
      INTEGER   IFIL, NOTOPN, BADOP, CATER, WARN, IOP, IERR, IDUM
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA NOTOPN, BADOP, CATER, WARN /1,2,10,9/
C-----------------------------------------------------------------------
C                                       Is file open?
      ERROR = NOTOPN
      DO 10 IFIL = 1,5
         IF (FILTAB(POLUN,IFIL).EQ.LUN) GO TO 20
 10      CONTINUE
      GO TO 999
C                                       Good OPCODE?
 20   ERROR = BADOP
      IOP = 0
      IF (OP.EQ.'WRIT') IOP = 1
      IF (OP.EQ.'READ') IOP = 2
      IF (IOP.LE.0) GO TO 999
C                                       Need to set status, IOP?
 40   IF ((FILTAB(POIOP,IFIL).EQ.1) .OR. (FILTAB(POIOP,IFIL).EQ.2))
     *   GO TO 50
         FILTAB(POIOP,IFIL) = IOP
         CALL CATDIR ('CSTA', FILTAB(POVOL,IFIL), FILTAB(POCAT,IFIL),
     *      CHTM12, CHTM6, IDUM, DUM2, IDUM, REED, WBUFF, IERR)
         IF ((IERR.EQ.0) .OR. (IERR.EQ.WARN)) GO TO 50
            WRITE (MSGTXT,1040) FILTAB(POVOL,IFIL),
     *         FILTAB(POCAT,IFIL), IERR
            CALL MSGWRT (8)
            ERROR = CATER
            GO TO 999
C                                       Do it!
 50   CALL ZFIO (OP, LUN, FILTAB(POFIN,IFIL), REC, DATA, ERROR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('FILIO: CAN''T SET INITIAL STATUS ON VOL',I2, ' CAT',I4,
     *   ' ERROR',I6)
      END
