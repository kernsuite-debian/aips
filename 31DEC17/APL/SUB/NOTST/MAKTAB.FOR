      SUBROUTINE MAKTAB (SRTORD, VOL, CNO, VER, CATBLK, LUN, DATP,
     *   IBLK, IERR)
C-----------------------------------------------------------------------
C! Create and initialize table from data in common /TABHDR/ (FITS)
C# EXT-util FITS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999, 2016
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
C  This routine will create a table file and initialize the header
C  values of the table from the data in common TABHDR
C  Inputs:
C     SRTORD    I     Logical column sorted on. 0=unknown, LT 0 = desc.
C     VOL       I     Disk volume number of parent file.
C     CNO       I     Catalog number of parent file.
C     VER       I     Version number of table file. 0=use latest + 1.
C     CATBLK    I(256)   Catalog header of parent file.
C     LUN       I     Logical unit number to open table file.
C  Output:
C     DATP      I(128,2)   Data pointers used in table file control.
C     IBLK      I(512)     IO buffer for table file.
C     IERR      I          Error code.: -1 => FILE WAS CREATED
C-----------------------------------------------------------------------
      INTEGER   SRTORD, VOL, CNO, VER, CATBLK(256), LUN, DATP(128,2),
     *   IBLK(*), IERR, IRNO, TITLES, UNITS, I, NREC, TTCODE(128)
      HOLLERITH HSTRNG(6)
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DTHD.INC'
      DATA TITLES, UNITS /3,4/
C-----------------------------------------------------------------------
C                                       Zero out control block.
      CALL FILL (256, 0, DATP)
C                                       Ensure small integer data
C                                       types don't get transferred
C                                       to output table type array
      CALL FILL (128, 0, TTCODE)
      DO 10 I = 1, ITNCOL
         TTCODE(I) = TFCODE(I)
         IF (MOD(TFCODE(I),10).EQ.6) THEN
            TTCODE(I) = TFCODE(I) - 2
            END IF
 10      CONTINUE
      CALL COPY (ITNCOL, TTCODE, DATP(1,2))
C                                       create/open
      NREC = MIN (NAXISI(2), 5000)
      CALL TABINI ('WRIT', ITYPE, VOL, CNO, VER, CATBLK, LUN, ITANKY,
     *   NREC, ITNCOL, DATP, IBLK, IERR)
      IF (IERR.EQ.-1) THEN
C                                       Write column values
         DO 30 I = 1,ITNCOL
            IRNO = I
C                                       Write column titles.
            CALL CHR2H (24, TTYPE(I), 1, HSTRNG)
            CALL TABIO ('WRIT', TITLES, IRNO, HSTRNG, IBLK, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       write units
            CALL CHR2H (8, TUNIT(I), 1, HSTRNG)
            CALL TABIO ('WRIT', UNITS, IRNO, HSTRNG, IBLK, IERR)
            IF (IERR.NE.0) GO TO 999
 30         CONTINUE
C                                       sort order
         IBLK(43) = SRTORD
C                                       table title
         CALL CHR2H (24, EXTNAM, 1, IBLK(101))
         IERR = -1
         END IF
C
 999  RETURN
      END
