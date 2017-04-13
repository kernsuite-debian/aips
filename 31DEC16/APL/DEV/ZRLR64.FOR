      SUBROUTINE ZRLR64 (NVAL, NP, INB, OUTB)
C-----------------------------------------------------------------------
C! convert buffer of local double precision values to IEEE 64-bit float.
C# Z Binary
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
C   Converts from local double precision (or corresponding 64 bit
C   precision) to 64 bit IEEE floating format.
C
C   The IEEE format is:
C
C                1         2         3         4         5         6
C      0123456789012345678901234567890123456789012345678901234567890123
C      seeeeeeeeeeemmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
C
C   where sign = -1 ** s, exponent = eee..., mantissa = 1.mmmmm...
C
C   The value is given by:
C
C      value = sign * 2 **(exp-1023) * mantissa
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
C      NP     I      First location in OUTB for results
C      INB    D(*)   Local format values
C   Output:
C      OUTB   D(*)   64-bit IEEE format values ('INDE    ' values are
C                    replaced with "nan")
C   Generic version  - does IEEE and VAX G formats for 32-bit machines,
C   is stubbed with STOP for all others.
C-----------------------------------------------------------------------
      INTEGER   NVAL, NP
      DOUBLE PRECISION INB(*), OUTB(*)
C
      INTEGER   LOOP, I4NAN(2), J
      DOUBLE PRECISION R8NAN
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (I4NAN, R8NAN)
C                                       Equivalence should yield
C                                       R8NAN = FFFFFFFFFFFFFFFF
      DATA I4NAN /-1, -1/
C-----------------------------------------------------------------------
      IF (NVAL.LE.0) GO TO 999
C                                       only works for 32-bit words
      IF ((NBITWD.NE.32) .OR. (NWDPDP.NE.2)) THEN
         WRITE (MSGTXT,1000) NBITWD, NWDPDP
         CALL MSGWRT (10)
         STOP 'ZRLR64 NEEDS LOCAL DEVELOPMENT'
C                                       IEEE floating-point - copy
      ELSE
         IF (DPFRMT.EQ.1) THEN
            DO 10 LOOP = 1,NVAL
               J = LOOP + NP - 1
               IF (INB(LOOP).EQ.DBLANK) THEN
                  OUTB(J) = R8NAN
               ELSE
                  OUTB(J) = INB(LOOP)
                  END IF
 10            CONTINUE
C                                       Flip bytes & words if necessary
            CALL ZBFLIP (8, NVAL, OUTB(NP), OUTB(NP))
C                                       VAX G - multiply by 4
         ELSE IF (DPFRMT.EQ.3) THEN
            DO 20 LOOP = 1,NVAL
               J = LOOP + NP - 1
               IF (INB(LOOP).EQ.DBLANK) THEN
                  OUTB(J) = R8NAN
               ELSE
                  OUTB(J) = INB(LOOP) * 0.250D0
                  END IF
 20            CONTINUE
            CALL ZBFLIP (8, NVAL, OUTB(NP), OUTB(NP))
C                                       VAX F, IBM not easy
         ELSE
            WRITE (MSGTXT,1020) DPFRMT
            CALL MSGWRT (10)
            STOP 'ZRLR64 NEEDS LOCAL DEVELOPMENT'
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZR64RL: REQUIRES LOCAL DEVELOPMENT FOR NBITWD =',I3,
     *   ' NWDPDP =',I2)
 1020 FORMAT ('ZR64RL: REQUIRES LOCAL DEVELOPMENT FOR DPFRMT =',I7)
      END
