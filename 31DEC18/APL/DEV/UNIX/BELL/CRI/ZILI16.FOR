      SUBROUTINE ZILI16 (NINT, INB, NP, OUTB)
C-----------------------------------------------------------------------
C! convert local integers to 16-bit FITS integers in a buffer
C# Binary
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
C   Convert a buffer of local integers to a buffer of standard 16-bit,
C   2's complement integers.
C   Inputs:
C      NINT   I      Number of integers to convert
C      INB    I(*)   Input buffer (start at index 1)
C      NP     I      Starting index in the output buffer (1-relative)
C                    in units of 16-bit integers
C   Output:
C      OUTB   I(*)   Output buffer
C   UNICOS version.
C-----------------------------------------------------------------------
      INTEGER   NINT, INB(*), NP, OUTB(*)
C
      INTEGER   IX, IXBYT, I, ILOCAL
C-----------------------------------------------------------------------
C                                       Sanity test.
      IF (NINT.LE.0) GO TO 999
C                                       Get pointer to first output
C                                       word.
      IX = (NP - 1) / 4 + 1
C                                       If not on an output word
C                                       boundary, circular shift to
C                                       preserve the first part of
C                                       output word.
      IXBYT = NP - (IX - 1) * 4 - 1
      IF (IXBYT.GT.0) THEN
         DO 10 I = 1,IXBYT
            OUTB(IX) = SHIFT (OUTB(IX), 16)
 10         CONTINUE
         END IF
C                                       Loop through the input buffer.
      DO 20 I = 1,NINT
         ILOCAL = INB(I)
         ILOCAL = ILOCAL .AND. 177777B
         OUTB(IX) = SHIFTL (OUTB(IX), 16) .OR. ILOCAL
         IXBYT = IXBYT + 1
         IF (IXBYT.GE.4) THEN
            IXBYT = 0
            IX = IX + 1
            END IF
 20      CONTINUE
C                                       Clean up if not a full word.
      IF (IXBYT.NE.0) THEN
         DO 30 I = IXBYT+1,4
            OUTB(IX) = SHIFTL (OUTB(IX), 16)
 30         CONTINUE
         END IF
C                                       Exit.
 999  RETURN
      END
