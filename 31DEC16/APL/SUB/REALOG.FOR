      SUBROUTINE REALOG (IOP, RV, DV, IV)
C-----------------------------------------------------------------------
C! converts numbers between floating and an integer version of their log
C# TV-util Utility
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
C   REALOG converts between real numbers and an integer*2 representation
C   of the logarithm (base e) of the number.
C   Inputs:
C      IOP  I       1 => R4 -> IV,    2 => R8 -> IV
C                   3 => IV -> R4,    4 => IV -> R8
C   In/out:
C      RV   R       Single precision real value
C      DV   D       Double precision real value
C      IV   I(4)    Log base e representation as follows:
C                        IV(1) = sign of number (-1, 0, 1)
C                        IV(2) = log(V) * 1000.
C                        IV(3) = remainder * 100000.
C                        IV(4) = remainder of that * 100000.
C-----------------------------------------------------------------------
      INTEGER   IOP, IV(4)
      REAL      RV
      DOUBLE PRECISION DV
C
      DOUBLE PRECISION DT, DL
C-----------------------------------------------------------------------
      IF ((IOP.LT.1) .OR. (IOP.GT.4)) GO TO 999
C                                       Real to integer
      IF (IOP.GT.2) GO TO 20
         CALL FILL (3, 0, IV)
         IF (IOP.EQ.2) IV(4) = 0
         IF ((IOP.EQ.1) .AND. (RV.EQ.0.0)) GO TO 999
         IF ((IOP.EQ.2) .AND. (DV.EQ.0.0D0)) GO TO 999
         IF (IOP.EQ.1) DT = RV
         IF (IOP.EQ.2) DT = DV
         IV(1) = 1
         IF (DT.LT.0.0D0) IV(1) = -1
         DT = DT * IV(1)
         DL = LOG (DT) * 1.0D3
         IV(2) = DL
         DL = (DL - IV(2)) * 1.0D5
         IV(3) = DL
         IF (IOP.EQ.1) GO TO 999
            DL = (DL - IV(3)) * 1.0D5
            IV(4) = DL
            GO TO 999
C                                       Integer to real
 20   CONTINUE
         DL = IV(2) / 1.0D3 + IV(3) / 1.0D8
         IF (IOP.EQ.4) DL = DL + IV(4) / 1.0D13
         DT = IV(1) * EXP (DL)
         IF (IOP.EQ.3) RV = DT
         IF (IOP.EQ.4) DV = DT
C
 999  RETURN
      END
