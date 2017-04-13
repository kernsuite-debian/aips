      SUBROUTINE ZLPOPN (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C! open a line-printer text file
C# Z Printer
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002
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
C   Open a line printer file.
C   Input:
C      LUN     I      Logical unit number (1 only)
C   Output:
C      FIND    I      Index in FTAB to file control block for LUN
C      IERR    I      Error return code: 0 => no error
C                         1 => error in inputs
C                         2 => LUN already in use
C                         3 => no room for LUN in FTAB
C                         4 => trouble translating logical
C                         5 => file already exists
C                         6 => open error
C   Generic version - uses ZFULLN, ZLPOP2.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      INTEGER   FLEN, ITRIM, IOSVAL, I, J
      CHARACTER FILNAM*256
      LOGICAL   EXISTS, MAP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Generate unique file name.
      CALL ZFULLN (' ', 'LPRNTR', 'ZLPOPN', FILNAM, IERR)
      FLEN = ITRIM (FILNAM)
      IF ((IERR.NE.0) .OR. (FLEN.LE.0)) THEN
         WRITE (MSGTXT,1000) IERR
         IERR = 4
         GO TO 995
         END IF
C                                       Make sure doesn't already exist
      INQUIRE (FILE=FILNAM, EXIST=EXISTS)
      IF (EXISTS) THEN
         IERR = 5
         WRITE (MSGTXT,1010)
         CALL MSGWRT (6)
         WRITE (MSGTXT,1011) FILNAM(1:MIN(FLEN,56))
         IF (FLEN.GT.56) THEN
            CALL MSGWRT (6)
            WRITE (MSGTXT,1011) FILNAM(57:)
            END IF
         GO TO 995
C                                       Open temporary file name.
      ELSE
         CALL ZLPOP2 (1, FILNAM, IOSVAL)
         IF (IOSVAL.NE.0) THEN
            IERR = 6
            WRITE (MSGTXT,1020) IOSVAL
            CALL MSGWRT (6)
            WRITE (MSGTXT,1011) FILNAM(1:MIN(FLEN,56))
            IF (FLEN.GT.56) THEN
               CALL MSGWRT (6)
               WRITE (MSGTXT,1011) FILNAM(57:)
               END IF
            CALL MSGWRT (6)
            CALL ZERROR ('ZLPOP2', IOSVAL, ' ', -999, .FALSE.)
            GO TO 999
         ELSE
            REWIND (1)
            WRITE (MSGTXT,1030) FILNAM(1:MIN (FLEN, 40))
            CALL MSGWRT (2)
            IF (FLEN.GT.40) THEN
               WRITE (MSGTXT,1031) FILNAM(41:FLEN)
               CALL MSGWRT (2)
               END IF
            END IF
         END IF
C                                       Allocate area in FTAB for LUN.
      MAP = .FALSE.
      CALL LSERCH ('OPEN', LUN, FIND, MAP, IERR)
      IF (IERR.NE.0) THEN
C                                       LUN already in use.
         IF (IERR.EQ.2) THEN
            WRITE (MSGTXT,1051) LUN
C                                       No room in FTAB for LUN.
         ELSE IF (IERR.EQ.3) THEN
            WRITE (MSGTXT,1052) LUN
         ELSE
            WRITE (MSGTXT,1050) IERR, 'OPEN', LUN, MAP
            END IF
         GO TO 995
         END IF
C                                       store file name
      CALL FSERCH (FIND, I, J, IERR)
      IF (IERR.EQ.0) THEN
         IF (I.EQ.1) DEVNAM(J) = FILNAM
         IF (I.EQ.2) NONNAM(J) = FILNAM
         IF (I.EQ.3) MAPNAM(J) = FILNAM
      ELSE
         WRITE (MSGTXT,1100) FIND, IERR
         CALL MSGWRT (6)
         END IF
      GO TO 999
C
 995  CALL MSGWRT (6)
C                                       Exit.
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZLPOPN: TROUBLE FORMING TEMPORARY FILE NAME',I6)
 1010 FORMAT ('ZLPOPN: FILE ALREADY EXISTS FOR LINE PRINTER FILE =')
 1011 FORMAT (8X,A)
 1020 FORMAT ('ZLPOPN: FORTRAN OPEN ERROR = ',I6,
     *   ' ON LINE PRINTER FILE =')
 1030 FORMAT ('ZLPOPN: printer file = ',A)
 1031 FORMAT (A)
 1050 FORMAT ('ZLPOPN: LSERCH ERROR ',I2,' ON ',A4,' LUN = ',I2,
     *   ' MAP = ',L1)
 1051 FORMAT ('ZLPOPN: LUN = ',I2,' ALREADY OPENED IN FTAB')
 1052 FORMAT ('ZLPOPN: NO ROOM IN FTAB FOR LUN = ',I2)
 1100 FORMAT ('ZLPOPN WARNING: FIND =',I5,' FSERCH ERR=',I5)
      END
