      SUBROUTINE ZR32RL (NVAL, NP, INB, OUTB)
C-----------------------------------------------------------------------
C! convert 32-bit IEEE floating buffer to local REAL values
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
C   Converts from 32 bit IEEE floating format to local single precision.
C
C   The IEEE format is:
C
C                1         2         3
C      01234567890123456789012345678901
C      seeeeeeeemmmmmmmmmmmmmmmmmmmmmmm
C
C   where sign = -1 _*_* s, exponent = eee..., mantissa = 1.mmmmm...
C
C   The value is given by:
C
C      value = sign _* 2 _*_*(exp-127) _* mantissa
C
C   Note: these values have a "hidden" bit and must always be normalized
C   The IEEE nan (not a number) values are used to indicate an invalid
C   number; a value with all bits set is recognized as a "nan".
C
C   The AIPS internal format for an invalid number is the value which
C   has the same bit pattern as 'INDE'.
C
C   The IEEE special values (-0., +/- Infty) are not recognized.
C
C   INPUTS:
C      NVAL     I*2        Number of values to convert
C      NP       I*2        First value in INB to convert
C      INB      R*4(*)     32 bit IEEE format values
C   OUTPUT:
C      OUTB     R*4(*)     Local format values ("nan" values are
C                          replaced with 'INDE')
C
C   UNICOS version (unpacks & converts).
C-----------------------------------------------------------------------
      INTEGER   NVAL, NP
      INTEGER   INB(*), OUTB(*)
C
      INTEGER   LOOP, TEMP, I, J, K
      INTEGER   I4NAN
      REAL      INDE, R4NAN
      EQUIVALENCE (I4NAN, R4NAN)
C                                       Equivalence should yield
C                                       R4NAN = FFFFFFFF
      DATA INDE, I4NAN /'INDE', X'FFFFFFFF'/
C-----------------------------------------------------------------------
C                                       Unpack.
CDIR$ IVDEP
      DO 10 LOOP = 1,(NVAL+1)/2
         I = LOOP + NP - 1
         K = 2 * LOOP
         J = K - 1
         OUTB(J) = AND (INB(I), X'FFFFFFFF00000000')
         OUTB(K) = SHIFTL (INB(I), 32)
 10      CONTINUE
C                                       Convert.
      DO 20 LOOP = 1,NVAL
         TEMP = OUTB(LOOP)
         OUTB(LOOP) = OR (OR (AND (OUTB(LOOP), X'8000000000000000'),
     *      SHIFTR (AND (OUTB(LOOP), X'7F80000000000000'), 7) +
     *      SHIFTL (16258, 48)), OR (
     *      SHIFTR (AND (OUTB(LOOP), X'007FFFFF00000000'), 8),
     *      X'0000800000000000'))
         OUTB(LOOP) = CVMGN (OUTB(LOOP), 0, SHIFTL(TEMP, 1))
C     DO 10 LOOP = 1,NVAL
C                                       Checked for blanked.
C        OUTB(LOOP) = INDE
C        IF (INB(LOOP+NP-1).NE.R4NAN) OUTB(LOOP) = INB(LOOP+NP-1)
C 10     CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
C-----------------------------------------------------------------------
