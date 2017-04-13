      SUBROUTINE RMEXT (DISK, CNO, TYPE, VER, CATBLK, BUFFER, IERR)
C-----------------------------------------------------------------------
C! Delete an extension file and update CATBLK on disk
C#
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2000, 2009, 2010, 2014
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
C   RMEXT deletes an extension file of specified type and version and
C   Modifies the catalog header suitably and updates the version on
C   the disk.
C   Inputs:
C      DISK     I        Disk number
C      CNO      I        Catalog slot number
C      TYPE     I        Extension type code (e.g. 'SN')
C      VER      I        Version number, 0=all.
C   Input/Output:
C      CATBLK   I(256)   Catalog header record.
C      BUFFER   I(256)   I/O buffer.
C   Output:
c      IERR     I        Error code, 0 => OK, else failed.
C   NOTE: May use AIPS LUN 28.
C-----------------------------------------------------------------------
      CHARACTER TYPE*2
      INTEGER   DISK, CNO, VER, CATBLK(256), BUFFER(256), IERR
C
      INTEGER   NFILES, IFILE, NDEL, IV, IVER, LUN, JERR, KVER, LUNTMP
      CHARACTER FNAME*48, CHTMP*2
      LOGICAL   TABLE, EXIST, FITASC
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      LUN = LUNTMP (1)
C                                       Find number of files
      IERR = 0
      NFILES = 0
      CALL FXHDEX (CATBLK)
      DO 10 IFILE = 1,KIEXTN
         CALL H2CHR (2, 1, CATBLK(KHEXT+IFILE-1), CHTMP)
         IF (CHTMP.EQ.TYPE) THEN
            IV = IFILE
            NFILES = CATBLK(KIVER+IFILE-1)
            END IF
 10      CONTINUE
      IF (NFILES.LE.0) GO TO 999
C                                       Destroy file(s)
      NDEL = 1
      KVER = VER
      IF (VER.LE.0) THEN
         NDEL = NFILES
         KVER = 1
         END IF
      DO 50 IFILE = 1,NDEL
         IVER = KVER + IFILE - 1
         CALL ZPHFIL (TYPE, DISK, CNO, IVER, FNAME, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
         CALL ZDESTR (DISK, FNAME, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040) IERR
            GO TO 990
            END IF
 50      CONTINUE
C                                       Mark in header.
C                                       Deleted all.
      IF (VER.LE.0) THEN
         CATBLK(KIVER+IV-1) = 0
         CALL CHR2H (2, '  ', 1, CATBLK(KHEXT+IV-1))
C                                       Removed one file.
      ELSE IF (VER.GE.NFILES) THEN
C                                       Find highest numbered extant
C                                       file.
         DO 80 IFILE = 1,NFILES
            IVER = NFILES - IFILE + 1
            CALL ISTAB (TYPE, DISK, CNO, IVER, LUN, BUFFER, TABLE,
     *         EXIST, FITASC, JERR)
            IF (EXIST .AND. (JERR.EQ.0)) THEN
               CATBLK(KIVER+IV-1) = IVER
               GO TO 100
               END IF
 80         CONTINUE
C                                       None left
         CATBLK(KIVER+IV-1) = 0
         CALL CHR2H (2, '  ', 1, CATBLK(KHEXT+IV-1))
         END IF
C                                       Update CATBLK on disk
 100  CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'REST', BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('RMEXT: ZPHFIL ERROR =',I3)
 1040 FORMAT ('RMEXT: ZDESTR ERROR =',I3)
 1050 FORMAT ('RMEXT: ERROR =',I3,' UPDATING CATBLK')
      END
