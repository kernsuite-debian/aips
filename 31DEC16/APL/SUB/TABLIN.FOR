      SUBROUTINE TABLIN (NAXIS1, FDVEC, TBIND, NCNTR, TAPBUF, LINE,
     *   IERR)
C-----------------------------------------------------------------------
C! reads a line from the data portion of a FITS extension of type TABLE
C# FITS tape EXT-appl
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
C   This routine will read a line from the data section of a FITS
C   extension file of type TABLE.  Limited to tables lines <= 2880 chars
C   Inputs:
C      NAXIS1   I      Length of table line.
C      FDVEC    I(50)  File descriptor vector for TAPIO input
C   In/Out:
C      NCNTR    I      Current character position in buffer.
C      TBIND    I      Buffer pointer in TAPBUF
C      TAPBUF   I(?)   Tape I/O buffer.
C   Outputs:
C      LINE     C*(*)  Output of table line in local characters.
C      IERR     I      Error code. 0=ok else TAPIO error.
C-----------------------------------------------------------------------
      CHARACTER LINE*(*)
      INTEGER   NAXIS1, FDVEC(50), TBIND, NCNTR, TAPBUF(*), IERR
C
      INTEGER   IELIN, ILEN, ISTART
C-----------------------------------------------------------------------
C                                       Copy line or partial line from
C                                       buffer.
      IELIN = NCNTR + NAXIS1 - 1
      IELIN = MIN (2880, IELIN)
      ILEN = IELIN - NCNTR + 1
      IF (ILEN.GT.0) CALL ZC8CL (ILEN, NCNTR, TAPBUF(TBIND), LINE)
C                                       Read next block.
      IF (ILEN.LT.NAXIS1) THEN
         CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 999
         ISTART = ILEN + 1
         ILEN = NAXIS1 - ILEN
         NCNTR = 1
         CALL ZC8CL (ILEN, NCNTR, TAPBUF(TBIND), LINE(ISTART:))
         END IF
C
      NCNTR = NCNTR + ILEN
C
 999  RETURN
      END
