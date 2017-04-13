      SUBROUTINE ZTXOPN (OPCODE, LUN, FIND, OUTFIL, APPEND, IERR)
C-----------------------------------------------------------------------
C! open a text file for read or write
C# Z Text
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   Open a text file.
C   Inputs:
C      OPCODE   C*4    Open for 'READ' 'QRED' 'WRIT' or 'QWRT'
C      LUN      I      Logical unit number
C      OUTFIL   C*(*)  Physical file name: with logical:name
C                      - if no colon - full name from ZFULLN assumed
C      APPEND   L      If true append new text to end of old file.
C                         (OPCODE='WRIT' or 'QWRT' only).
C   Outputs:
C      FIND     I      Index in FTAB for LUN
C      IERR     I      Error return code: 0 => no error
C                         1 => error in inputs
C                         2 => LUN already in use
C                         3 => no room for LUN in FTAB
C                         4 => trouble translating logical
C                         5 => file already exists
C                         6 => open error
C   Generic version --- uses ZTXOP2 for actual open.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, OUTFIL*(*)
      INTEGER   LUN, FIND, IERR
      LOGICAL   APPEND
C
      INTEGER   I, JERR, J
      LOGICAL   MAP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA MAP /.FALSE./
C-----------------------------------------------------------------------
C                                       Check inputs.
      IERR = 1
C                                       Valid OPCODE?
      IF ((OPCODE.NE.'READ') .AND. (OPCODE.NE.'QRED') .AND.
     *    (OPCODE.NE.'WRIT') .AND. (OPCODE.NE.'QWRT')) THEN
         WRITE (MSGTXT,1000) OPCODE
         GO TO 995
         END IF
C                                       Valid LUN?
      IF ((LUN.LE.0) .OR. (LUN.GT.128)) THEN
         WRITE (MSGTXT,1001) LUN
         GO TO 995
         END IF
C                                       Valid device type?
      IF (DEVTAB(LUN).NE.3) THEN
         WRITE (MSGTXT,1010) LUN, DEVTAB(LUN)
         GO TO 995
         END IF
C                                       Allocate area in FTAB for LUN.
      CALL LSERCH ('OPEN', LUN, FIND, MAP, JERR)
      IF (JERR.NE.0) THEN
         IERR = 3
C                                       LUN already in use.
         IF (JERR.EQ.2) THEN
            IERR = 2
            WRITE (MSGTXT,1022) LUN
C                                       No room in FTAB for LUN.
         ELSE IF (JERR.EQ.3) THEN
            WRITE (MSGTXT,1023) LUN
         ELSE
            WRITE (MSGTXT,1020) JERR, 'OPEN', LUN
            END IF
         GO TO 995
         END IF
      IERR = 0
C                                       Call 2nd level Z routine for
C                                       name translation and actual open
      CALL ZTXOP2 (OPCODE, LUN, FIND, OUTFIL, APPEND, IERR)
C                                       store file name
      IF (IERR.EQ.0) THEN
         CALL FSERCH (FIND, I, J, JERR)
         IF (JERR.EQ.0) THEN
            IF (I.EQ.1) DEVNAM(J) = OUTFIL
            IF (I.EQ.2) NONNAM(J) = OUTFIL
            IF (I.EQ.3) MAPNAM(J) = OUTFIL
            END IF
         GO TO 999
         END IF
C                                       Error.  Remove FTAB entry.
 990  CALL LSERCH ('CLOS', LUN, FIND, MAP, JERR)
      IF (JERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1990) JERR, 'CLOS', LUN
C
 995  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTXOPN: UNKNOWN OPCODE = ',A4)
 1001 FORMAT ('ZTXOPN: INVALID LUN = ',I2)
 1010 FORMAT ('ZTXOPN: INVALID DEVTAB(',I2,') = ',I1)
 1020 FORMAT ('ZTXOPN: LSERCH ERROR ',I2,' ON ',A4,' LUN = ',I2)
 1022 FORMAT ('ZTXOPN: LUN = ',I2,' ALREADY OPENED IN FTAB')
 1023 FORMAT ('ZTXOPN: NO ROOM IN FTAB FOR LUN = ',I2)
 1990 FORMAT ('ZTXOPN: LSERCH ERROR ',I2,' ON ',A4,' LUN = ',I2)
      END
