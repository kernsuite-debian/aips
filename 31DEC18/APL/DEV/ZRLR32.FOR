      SUBROUTINE ZRLR32 (NVAL, NP, INB, OUTB)
C-----------------------------------------------------------------------
C! converts buffer of local REAL values to IEEE 32-bit floating-point
C# Binary
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2003
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
C   Converts from local single precision to 32 bit IEEE floating format.
C
C   The IEEE format is:
C
C                1         2         3
C      01234567890123456789012345678901
C      seeeeeeeemmmmmmmmmmmmmmmmmmmmmmm
C
C   where sign = -1 ** s, exponent = eee..., mantissa = 1.mmmmm...
C
C   The value is given by:
C
C      value = sign * 2 **(exp-127) * mantissa
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
C   A multiplication by a factor of 4.0 converts between VAX F and IEEE
C   32 bit formats.
C
C   Inputs:
C      NVAL   I      Number of values to convert
C      NP     I      First value in OUTB for result
C      INB    R(*)   Local format values
C   Output:
C      OUTB   R(*)   32-bit IEEE FORMAT values ('INDE' values are
C                    replaced with "nan")
C   Generic version - does IEEE and VAX F formats for 32-bit machines,
C   is stubbed with STOP for all others.
C-----------------------------------------------------------------------
      INTEGER   NVAL, NP
      REAL      INB(*), OUTB(*)
C
      INTEGER   LOOP, I4NAN, J
      REAL      R4NAN
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (I4NAN, R4NAN)
C                                       Equivalence should yield
C                                       R4NAN = FFFFFFFF
      DATA I4NAN /-1/
C-----------------------------------------------------------------------
      IF (NVAL.LE.0) GO TO 999
C                                       only works for 32-bit words
      IF (NBITWD.NE.32) THEN
         WRITE (MSGTXT,1000) NBITWD
         CALL MSGWRT (10)
         STOP 'ZRLR32 NEEDS LOCAL DEVELOPMENT'
      ELSE
C                                       IEEE floating-point - copy
         IF (SPFRMT.EQ.1) THEN
            DO 10 LOOP = 1,NVAL
               J = LOOP + NP - 1
               IF (INB(LOOP).EQ.FBLANK) THEN
                  OUTB(J) = R4NAN
               ELSE
                  OUTB(J) = INB(LOOP)
                  END IF
 10            CONTINUE
C                                       Flip Bytes & words
            CALL ZBFLIP (4, NVAL, OUTB(NP), OUTB(NP))
C                                       VAX F - multiply by 4
         ELSE IF (SPFRMT.EQ.2) THEN
            DO 20 LOOP = 1,NVAL
               J = LOOP + NP - 1
               IF (INB(LOOP).EQ.FBLANK) THEN
                  OUTB(J) = R4NAN
               ELSE
                  OUTB(J) = INB(LOOP) * 0.250
                  END IF
 20            CONTINUE
            CALL ZBFLIP (4, NVAL, OUTB(NP), OUTB(NP))
C                                       VAX G, IBM not easy
         ELSE
            WRITE (MSGTXT,1020) SPFRMT
            CALL MSGWRT (10)
            STOP 'ZRLR32 NEEDS LOCAL DEVELOPMENT'
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZRLR32: REQUIRES LOCAL DEVELOPMENT FOR NBITWD =',I7)
 1020 FORMAT ('ZRLR32: REQUIRES LOCAL DEVELOPMENT FOR SPFRMT =',I7)
      END
