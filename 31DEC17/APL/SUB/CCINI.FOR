      SUBROUTINE CCINI (LUN, NCOL, VOL, CNO, VER, CATBLK, BUF, IERR)
C-----------------------------------------------------------------------
C! creates and/or opens a CC (components) extension table
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   CCINI creates and/or opens for writing (and reading) a specified
C   CC (components table) file.
C   Inputs:
C      LUN     I         Logical unit number to use
C      VOL     I         Disk number
C      CNO     I         Catalog number
C   In/out:
C      NCOL    I         Number of columns: 3 or 7 are allowed.
C      VER     I         Input: desired version number 0 -> new
C                        Output: that used
C      CATBLK  I(256)    File catalog header block
C   Output:
C      BUF     I(768)    First 512 words required for later calls to
C                        TABIO
C      IERR    I         Error codes from TABINI or TABIO
C-----------------------------------------------------------------------
      INTEGER   LUN, NCOL, VOL, CNO, VER, CATBLK(256), BUF(768), IERR
C
      INTEGER   IRNO, NKEY, NREC, ITITLE(8)
      HOLLERITH HTITLE(8)
      CHARACTER WRITE*4, CC*2, TTITLE*32, CTITLE(7)*8, UNITS(7)*8,
     *   TITLE*24
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (ITITLE, HTITLE)
      DATA CC, WRITE /'CC','WRIT'/
      DATA TTITLE /'AIPS CC CLEAN COMPONENTS TABLE  '/
      DATA CTITLE /'FLUX    ', 'DELTAX  ', 'DELTAY  ', 'MAJOR AX',
     *   'MINOR AX', 'POSANGLE', 'TYPE OBJ'/
      DATA UNITS /'JY      ', 5*'DEGREES ', 'CODE    '/
C-----------------------------------------------------------------------
C                                       Init parameters
      IF (NCOL.LE.3) NCOL = 3
      IF (NCOL.GT.3) NCOL = 7
      NKEY = 1
      NREC = 500
C                                       Data = 7 R's   x 1
      CALL FILL (256, 0, BUF(513))
      CALL FILL (7, 12, BUF(641))
C                                       create/open
      CALL TABINI (WRITE, CC, VOL, CNO, VER, CATBLK, LUN, NKEY, NREC,
     *   NCOL, BUF(513), BUF(1), IERR)
C                                       New file created
      IF (IERR.GE.0) GO TO 999
C                                       write column titles
         DO 20 IRNO = 1,NCOL
            TITLE = CTITLE(IRNO)
            CALL CHR2H (24, TITLE, 1, HTITLE)
            CALL TABIO (WRITE, 3, IRNO, ITITLE, BUF, IERR)
            IF (IERR.NE.0) GO TO 999
 20         CONTINUE
C                                       write units
         DO 30 IRNO = 1,NCOL
            TITLE = UNITS(IRNO)
            CALL CHR2H (24, TITLE, 1, HTITLE)
            CALL TABIO (WRITE, 4, IRNO, ITITLE, BUF, IERR)
            IF (IERR.NE.0) GO TO 999
 30         CONTINUE
C                                       sort order
         BUF(43) = -257
C                                       table title
         CALL CHR2H (32, TTITLE, 1, HTITLE)
         CALL COPY (8, ITITLE, BUF(101))
C
 999  RETURN
      END
