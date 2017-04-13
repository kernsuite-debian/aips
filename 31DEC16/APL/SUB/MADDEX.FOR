      SUBROUTINE MADDEX (TYPE, IVOL, SLOT, CATBLK, IWBUFF, SAVE, STAT,
     *   VER, IERR)
C-----------------------------------------------------------------------
C! adds extension file to catalog header
C# EXT-util catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997
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
C   MADDEX will add an extension file to the catalog header.  This
C   involves incrementing the version number if an extension file of
C   the TYPE already exists, or adding an extension file with version
C   number 1.  This routine is designed to work in conjuction with
C   MAPOPN and MAPCLS.  MAPOPN should be called with the 'WRIT' option
C   before this routine is called.  If this routine fails for some
C   reason  then the write status will be cleared unless STAT = 'UPDT'
C   Inputs:
C      TYPE    C*2     extension file type, example 'PL', 'HI'
C      IVOL    I       disk volume number
C      SLOT    I       the catalog slot number of the primary file
C                      with which the extension file is associated.
C                      This value can be found using CATDIR.
C      CATBLK  I(256)  current catalog block.
C      IWBUFF  I(256)  second work buffer for I/O.
C      SAVE    L       if true the header block will be saved,
C                      if false and STAT equals 'writ' then the
C                      header will not be saved.  The header is
C                      always saved if STAT = 'READ'.
C      STAT    C*4     final STAT to leave map file, ie
C                      'READ' or 'WRIT'. 'UPDT' -> writ no clear
C   Output:
C      VER     I       version number of extension file.
C      IERR    I       error code. 0 = OK
C                                  1 = Invalid STAT.
C                                  2 = CATIO write error.
C                                  3 = Extension file area full (can be
C                                       either max types (20) exceeded,
C                                       or max number (46655) exceeded).
C-----------------------------------------------------------------------
      CHARACTER STAT*4, TYPE*2
      INTEGER   IVOL, IERR, VER, SLOT, CATBLK(256), IWBUFF(256)
      LOGICAL   SAVE
C
      CHARACTER CHTM12*12, CHTM6*6, CTEMP*2
      INTEGER   IERR2, NEXTF, I, I20, IER
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      VER = 0
      IERR = 0
C                                       Invalid STAT.
      IF ((STAT.EQ.'READ') .OR. (STAT.EQ.'WRIT') .OR. (STAT.EQ.'UPDT'))
     *   GO TO 10
         WRITE (MSGTXT,1000) STAT
         IERR = 1
         GO TO 980
C                                       Find extension TYPE, update ver
 10   CALL FXHDEX (CATBLK)
      NEXTF = KHEXT + KIEXTN - 1
      DO 20 I = KHEXT,NEXTF
         CALL H2CHR (2, 1, CATBLK(I), CTEMP)
         IF (CTEMP.NE.TYPE) GO TO 20
            I20 = KIVER + I - KHEXT
            IF (CATBLK(I20).GE.46655) GO TO 35
               CATBLK(I20) = CATBLK(I20) + 1
               GO TO 40
 20      CONTINUE
C                                       No TYPE file. Find 1st slot.
      DO 30 I = KHEXT,NEXTF
         I20 = KIVER + I - KHEXT
         IF (CATBLK(I20).GT.0) GO TO 30
            CALL CHR2H (2, TYPE, 1, CATBLK(I))
            CATBLK(I20) = 1
            GO TO 40
 30      CONTINUE
C                                       File space full (too many types
C                                       of extension file)
      WRITE (MSGTXT,1034) KIEXTN
      IERR = 3
      GO TO 980
C                                       File space full (too many
C                                       instances of extension files)
C                                       NB hardwired number here, above
 35   WRITE (MSGTXT,1035) 46655
      IERR = 3
      GO TO 980
C
 40   VER = CATBLK(I20)
      IF ((.NOT.SAVE) .AND. (STAT.NE.'READ')) GO TO 999
C                                       Write or update header.
         CALL CATIO ('UPDT', IVOL, SLOT, CATBLK, 'REST', IWBUFF, IERR2)
         IF ((IERR2.EQ.0) .OR. (IERR2.GT.4)) GO TO 50
            WRITE (MSGTXT,1040) IERR2
            IERR = 2
            GO TO 980
C                                       Change to READ if requested
 50      IF (STAT.NE.'READ') GO TO 999
            CHTM12 = ' '
            CHTM6 = ' '
            CTEMP = ' '
            CALL CATDIR ('CSTA', IVOL, SLOT, CHTM12, CHTM6, 0, CTEMP,
     *         0, 'CLWR', IWBUFF, IER)
            CALL CATDIR ('CSTA', IVOL, SLOT, CHTM12, CHTM6, 0, CTEMP,
     *         0, STAT, IWBUFF, IER)
            GO TO 999
C                                       error print, clear catalog
 980  CALL MSGWRT (7)
      IF (STAT.NE.'UPDT') THEN
         CHTM12 = ' '
         CHTM6 = ' '
         CTEMP = ' '
         CALL CATDIR ('CSTA', IVOL, SLOT, CHTM12, CHTM6, 0, CTEMP,
     *      0, 'CLWR', IWBUFF, IER)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MADDEX: INVALID STATUS PARAMETER: ',A4)
 1034 FORMAT ('MADDEX: TOO MANY TYPES OF EXTENSION FILE (MAX ',I4,')')
 1035 FORMAT ('MADDEX: TOO MANY EXTENSION FILES (MAX',I7,')')
 1040 FORMAT ('MADDEX: CATIO WRITE ERROR',I5)
      END
