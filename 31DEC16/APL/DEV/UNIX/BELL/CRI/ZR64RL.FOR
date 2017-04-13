      SUBROUTINE ZR64RL (NVAL, NP, INB, OUTB)
C-----------------------------------------------------------------------
C! convert 64-bit IEEE floating-point buffer to local "DOUBLE PRECISION"
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
C   Converts from 64 bit IEEE floating format data to local double
C   precision (or corresponding 64 bit precision).
C
C   The IEEE format is:
C
C                1         2         3         4         5         6
C      0123456789012345678901234567890123456789012345678901234567890123
C      seeeeeeeeeeemmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
C
C   where sign = -1 _*_* s, exponent = eee..., mantissa = 1.mmmmm...
C
C   The value is given by:
C
C      value = sign _* 2 _*_*(exp-1023) _* mantissa
C
C   Note: these values have a "hidden" bit and must always be normalized
C   The IEEE nan (not a number) values are used to indicate an invalid
C   number; a value with all bits set is recognized as a "nan".
C
C   The AIPS internal format for an invalid number is the value which
C   has the same bit pattern as 'INDE    '.
C
C   The IEEE special values (-0., +/- Infty) are not recognized.
C
C   A multiplication by a factor of 4.0 converts between VAX G and IEEE
C   64 bit formats.
C
C   Inputs:
C      NVAL   I      Number of values to convert
C      NP     I      First value in INB to convert
C      INB    D(*)   64-bit IEEE format values
C   OUTPUT:
C      OUTB   D(*)   Local format values ("nan" values are replaced
C                    with AIPS' D.P. blank = 'INDE    ')
C   UNICOS version.
C-----------------------------------------------------------------------
      INTEGER   NVAL, NP
      DOUBLE PRECISION INB(*), OUTB(*)
C
      INTEGER   LOOP, I4NAN, J, I8TEST(2), I, IP, ZAND
      DOUBLE PRECISION R8TEST
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (I8TEST, R8TEST)
C                                       = 7FF00000
      DATA I4NAN /2146435072/
C-----------------------------------------------------------------------
      IF (NVAL.LE.0) GO TO 999
C                                       Does BYTFLP need a check?
C                                       no place to test at the moment
      IP = 1
            LOOP = 4 * NVAL
            CALL ZBYTFL (LOOP, INB(NP), INB(NP))
            DO 10 LOOP = 1,NVAL
               J = LOOP + NP - 1
               R8TEST = INB(J)
               I = ZAND (I4NAN, I8TEST(IP))
               IF (I.EQ.0) THEN
                  OUTB(LOOP) = 0.0D0
               ELSE IF (I.EQ.I4NAN) THEN
                  OUTB(LOOP) = DBLANK
               ELSE
                  OUTB(LOOP) = INB(J)
                  END IF
 10            CONTINUE
C
 999  RETURN
      END
