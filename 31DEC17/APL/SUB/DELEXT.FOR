      SUBROUTINE DELEXT (TYPE, IVOL, SLOT, STAT, BUFF1, BUFF2, IVER,
     *   IERR)
C-----------------------------------------------------------------------
C! removes an extension file from the header in the catalog file
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
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
C   DELEXT will delete an extension file from the catalog header.  This
C   consists of decreasing the version number by 1 if the requested
C   version is now the highest one in the header.  The header block is
C   read from the catalog, revised, and replaced.  The initial
C   catalog status is cleared or preserved.
C   Inputs:
C      TYPE    C*2      extension file type, example PL, HI.
C      IVOL    I        disk volume number 1-9.
C      SLOT    I        the catalog slot number of the primary file
C                       with which the extension file is associated.
C                       This value can be found using CATDIR.
C      STAT    C*4      Current file status: READ or WRIT to be cleared
C                       RDRD or WRWR => current status to remain
C      BUFF1   I(256)   work buffer for I/0.
C      BUFF2   I(256)   second work buffer for I/O.
C      IVER    I        version to delete ( 0 => highest);
C   Output:
C      IERR    I        error code. 0 = OK
C                                   1 = CATIO read error.
C                                   2 = CATIO write error.
C                                   3 = No extension file of type TYPE
C                                   4 = error in STAT
C                                   5 = IVER > highest in header
C                                   6 = READ status not left properly
C-----------------------------------------------------------------------
      CHARACTER TYPE*2, STAT*4
      INTEGER   IVOL, IERR, IVER, SLOT, BUFF1(256), BUFF2(256)
C
      INTEGER   IER, I, I20, VER, NEXTF
      CHARACTER CHTEMP*2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       check STAT
      IERR = 4
      IF ((STAT.NE.'READ') .AND. (STAT.NE.'WRIT') .AND. (STAT.NE.'RDRD')
     *   .AND. (STAT.NE.'WRWR')) GO TO 999
C                                       read catalog block
      IERR = 1
      VER = 0
C                                       on 'READ', remove read status
      IF ((STAT.NE.'READ') .AND. (STAT.NE.'RDRD')) GO TO 10
         CALL CATDIR ('CSTA', IVOL, SLOT, ' ', ' ', 0, ' ', 0,
     *      'CLRD', BUFF2, IER)
         CALL CATIO ('READ', IVOL, SLOT, BUFF1, 'WRIT', BUFF2, IER)
         IF (IER.EQ.0) GO TO 15
            WRITE (MSGTXT,1000) IER
            IF (IER.EQ.5) WRITE (MSGTXT,1001)
            GO TO 980
C                                       on WRIT: just read it now
 10   CONTINUE
         CALL CATIO ('READ', IVOL, SLOT, BUFF1, 'REST', BUFF2, IER)
         IF ((IER.EQ.0) .OR. (IER.EQ.6)) GO TO 15
            WRITE (MSGTXT,1000) IER
            GO TO 980
C                                       Find extension TYPE, update ver
 15   IERR = 0
      CALL FXHDEX (BUFF1)
      NEXTF = KHEXT + KIEXTN - 1
      DO 20 I = KHEXT,NEXTF
         I20 = KIVER + I - KHEXT
         IF (BUFF1(I20).EQ.0) GO TO 20
            CALL H2CHR (2, 1, BUFF1(I), CHTEMP)
            IF (CHTEMP.NE.TYPE) GO TO 20
               VER = BUFF1(I20)
               GO TO 30
 20      CONTINUE
C                                       File of this type not found.
      IERR = 3
      WRITE (MSGTXT,1020) TYPE
      GO TO 980
C                                       Bad version number
 30   IF (IVER.LE.VER) GO TO 40
         IERR = 5
         WRITE (MSGTXT,1030) IVER
         GO TO 980
C                                       Decrement version #
 40   IF ((IVER.LE.0) .OR. (IVER.EQ.VER)) BUFF1(I20) = BUFF1(I20) - 1
C                                       Write or update header.
      IF (STAT.NE.'WRWR') THEN
         CALL CATIO ('UPDT', IVOL, SLOT, BUFF1, 'CLWR', BUFF2, IER)
      ELSE
         CALL CATIO ('UPDT', IVOL, SLOT, BUFF1, 'REST', BUFF2, IER)
         END IF
      IF ((IER.EQ.0) .OR. (IER.EQ.10)) GO TO 50
         IERR = 2
         WRITE (MSGTXT,1040) IER
         GO TO 980
C                                       restore 'READ' status
 50   IF (STAT.NE.'RDRD') GO TO 999
         CALL CATDIR ('CSTA', IVOL, SLOT, ' ', ' ', 0, ' ', 0, 'READ',
     *      BUFF2, IER)
         IF ((IER.EQ.0) .OR. (IER.EQ.9)) GO TO 999
            IERR = 6
            WRITE (MSGTXT,1050) IER
            CALL MSGWRT (8)
            GO TO 999
C                                       error print, clear catalog
 980  CALL MSGWRT (8)
      CALL CATDIR ('CSTA', IVOL, SLOT, ' ', ' ', 0, ' ', 0, 'CLWR',
     *   BUFF2, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DELEXT: CATIO READ ERROR OR TOO BUSY',I5)
 1001 FORMAT ('DELEXT CAN''T REMOVE EXTENSION FROM HEADER, FILE TOO',
     *   ' BUSY')
 1020 FORMAT ('DELEXT: NO EXTENSION FILE OF TYPE ',A2)
 1030 FORMAT ('DELEXT: EXTENSION VERSION',I5,' NOT IN HEADER')
 1040 FORMAT ('DELEXT: CATIO WRITE ERROR',I5)
 1050 FORMAT ('DELEXT: UNABLE TO RESTORE READ STATUS, ERROR',I5)
      END
