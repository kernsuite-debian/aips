      SUBROUTINE GETHUT (NCOL, DATP, BUFFER, COLHDR, UNITS, IFORMT,
     *   IFLEN, IERR)
C-----------------------------------------------------------------------
C! returns column titles, units, types, lengths in logical column order
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004
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
C   This routine will return a lot of information about the columns
C   in a table file in logical order.
C   Input:
C      NCOL   I           Number of columns.
C      DATP   I(128,2)    Table file data pointers.
C  In/Out:
C      BUFFER I(512)      I/O buffer.
C  Output:
C      COLHDR R(6,128)    Column headers (Ascii).
C      UNITS  R(2,128)    Physical units.
C      IFORMT I(128)      Format codes:
C                            1 = Double Precision.
C                            2 = Real.
C                            3 = Character String (4 per Real).
C                            6 = Integer.
C      IFLEN  I(128)      Lengths.  Zero for everything except char.
C      IERR   I           Error code. 0=ok.
C-----------------------------------------------------------------------
      INTEGER   NCOL, DATP(128,2), BUFFER(512), IFORMT(128), IFLEN(128),
     *   IERR
      CHARACTER COLHDR(*)*(*), UNITS(*)*(*)
C
      INTEGER   ICOL, ITITLE, IUNITS, NT, NA
      HOLLERITH HTEMP(6)
      INCLUDE 'INCS:DMSG.INC'
      DATA ITITLE, IUNITS /3,4/
C-----------------------------------------------------------------------
      IERR = 1
C                                       Buffers probably not big enuf.
      IF (NCOL.LE.128) GO TO 10
         WRITE (MSGTXT,1000) NCOL
         CALL MSGWRT (6)
         GO TO 999
C
 10   DO 20 ICOL = 1,NCOL
         CALL TABIO ('READ', ITITLE, ICOL, HTEMP, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Remove any Nulls
         NT = 24
         CALL SPFIL (HTEMP, NT, NA)
         CALL H2CHR (24, 1, HTEMP, COLHDR(ICOL))
         CALL TABIO ('READ', IUNITS, ICOL, HTEMP, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Remove any Nulls
         NT = 8
         CALL SPFIL (HTEMP, NT, NA)
         CALL H2CHR (8, 1, HTEMP, UNITS(ICOL))
         IFORMT(ICOL) = MOD (DATP(ICOL,2), 10)
         IFLEN(ICOL) = DATP(ICOL,2) / 10
 20      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETHUT: BUFFER SIZE IS 128, NUMBER OF COLUMNS IS ',I6)
      END
