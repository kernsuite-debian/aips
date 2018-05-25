      SUBROUTINE ZHEX10 (HVAL, IVAL, IERR)
C-----------------------------------------------------------------------
C! Compute the decimal equivalent of an extended hexadecimal (36) number
C# Z Math
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
C   ZHEX10 computes the decimal equivalent of an extended hexadecimal
C   (base 36) number.
C   Input:
C      HVAL   C*(*)   String containing hexadecimal value.  May contain
C                     leading or trailing blanks or tabs.
C   Outputs:
C      IVAL   I       Decimal equivalent of HVAL.
C      IERR   I       Error status, 0: success
C                        1: illegal hexadecimal digit
C   Notes: Treats lower case extended hexadecimal digits as equivalent
C   to upper case.
C-----------------------------------------------------------------------
      INTEGER   IERR, IVAL
      CHARACTER HVAL*(*)
C
      INTEGER   J, K, J1, J2
      CHARACTER HC*1, HEXC*36
      DATA HEXC /'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C-----------------------------------------------------------------------
C                                       Initialize
      IERR = 0
      CALL TXTLEN (HVAL, J1, J2)
C                                       Process the hexadecimal digits
C                                       from left to right
      IVAL = 0
      DO 20 J = J1,J2
         IVAL = 36 * IVAL
         HC = HVAL(J:J)
         CALL CHLTOU (1, HC)
         K = INDEX (HEXC, HC)
C                                       Illegal character
         IF (K.LT.1) THEN
            IVAL = -1
            IERR = 1
            GO TO 999
         ELSE
            IVAL = IVAL + (K-1)
            END IF
 20      CONTINUE
C
 999  RETURN
      END
