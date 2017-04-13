      SUBROUTINE ALLTAB (NONOT, NOTTYP, LUNOLD, LUNNEW, VOLOLD, VOLNEW,
     *   CNOOLD, CNONEW, CATNEW, BUFF1, BUFF2, IRET)
C-----------------------------------------------------------------------
C! Copies all table extension files from one catalog slot to another
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2003, 2010, 2013
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
C   ALLTAB copies all Table extension file(s).  The output files must
C   be new - old ones cannot be rewritten.  The output file must be
C   opened WRIT in the catalog and will have its CATBLK updated on disk.
C   NX tables are never copied.
C   Inputs:
C      NONOT   I       Number of "Forbidden" types to copy.
C      NOTTYP  C*2(*)  Table types to ignore.
C      LUNOLD  I       LUN for old file
C      LUNNEW  I       LUN for new file
C      VOLOLD  I       Disk number for old file.
C      VOLNEW  I       Disk number for new file.
C      CNOOLD  I       Catalog slot number for old file
C      CNONEW  I       Catalog slot number for new file
C   In/out:
C      CATNEW  I(256)  Catalog header for new file.
C   Output:
C      BUFF1   I(1024) Work buffer
C      BUFF2   I(1024) Work buffer
C      IRET    I       Return error code  0 => ok, otherwise TABCOP
C                                             or 10*CATIO error.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(*)*2
      INTEGER   NONOT, LUNOLD, LUNNEW, VOLOLD, VOLNEW, CNOOLD, CNONEW,
     *   BUFF1(*), BUFF2(*), CATNEW(256), IRET
C
      INCLUDE 'INCS:PHDR.INC'
      INTEGER   IVERI, IVERO, IER, NONON, IEXT, JEXT, OLDNUM(NIEXTN)
      LOGICAL   TABLE, EXIST, FITASC
      CHARACTER READ*4, REST*4, OLDTYP(NIEXTN)*2, NONTAB(20)*2, BLANK*4
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C                                       Non table extension files
      DATA NONON /3/
      DATA NONTAB /'HI','PL','SL',17*'  '/
      DATA READ, REST, BLANK  /'READ','REST','    '/
C-----------------------------------------------------------------------
      IRET = 0
      IF ((VOLOLD.EQ.VOLNEW) .AND. (CNOOLD.EQ.CNONEW)) GO TO 999
C                                       Get old CATBLK in BUFF2.
      CALL CATIO (READ, VOLOLD, CNOOLD, BUFF2, REST, BUFF1, IRET)
      IF ((IRET.GE.1) .AND. (IRET.LE.4)) THEN
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (6)
         IRET = 10 * IRET
         GO TO 999
      ELSE
         IRET = 0
         END IF
C                                       header ext format
      CALL FXHDEX (BUFF2)
C                                       Get extension file info
      DO 40 IEXT = 1,KIEXTN
         OLDTYP(IEXT) = BLANK
         CALL H2CHR (2, 1, BUFF2(KHEXT+IEXT-1), OLDTYP(IEXT))
         OLDNUM(IEXT) = BUFF2(KIVER+IEXT-1)
C                                       Do NOT copy NX tables
         IF (OLDTYP(IEXT).EQ.'NX') OLDNUM(IEXT) = 0
C                                       Check if on non table list
         DO 20 JEXT = 1,NONON
            IF (OLDTYP(IEXT).EQ.NONTAB(JEXT)) OLDNUM(IEXT) = 0
 20         CONTINUE
C                                       Check "forbidden" types
         IF (NONOT.LE.0) GO TO 40
            DO 30 JEXT = 1,NONOT
               IF (OLDTYP(IEXT).EQ.NOTTYP(JEXT)) OLDNUM(IEXT) = 0
 30            CONTINUE
 40      CONTINUE
C                                       Loop, copying tables.
      DO 100 IEXT = 1,KIEXTN
         IF (OLDNUM(IEXT).LE.0) GO TO 100
C                                       Copy each table independently
         DO 90 JEXT = 1,OLDNUM(IEXT)
C                                       See if files exist and are
C                                       wanted.
            CALL ISTAB (OLDTYP(IEXT), VOLOLD, CNOOLD, JEXT, LUNOLD,
     *         BUFF1, TABLE, EXIST, FITASC, IER)
            IF (TABLE .AND. EXIST .AND. (IER.EQ.0)) THEN
               IVERI = JEXT
               IVERO = JEXT
C                                       Copy
               CALL TABCOP (OLDTYP(IEXT), IVERI, IVERO, LUNOLD, LUNNEW,
     *            VOLOLD, VOLNEW, CNOOLD, CNONEW, CATNEW, BUFF1, BUFF2,
     *            IER)
               IRET = MAX (IER, IRET)
               END IF
 90         CONTINUE
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ALLTAB: ERROR',I5,' READING OLD CATBLK')
      END
