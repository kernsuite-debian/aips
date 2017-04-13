      SUBROUTINE ZKDUMP (I1, I2, K, C)
C-----------------------------------------------------------------------
C! display portions of an array in various Fortran formats
C# Z Service
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
C   Dump portions of an array in a variety of formats (e.g., Fortran
C   I, E, A and hexidecimal).
C   Inputs:
C      I1    I      First subscript in integer array to dump
C      I2    I      Last subscript in integer array to dump
C      K     I(*)   Integer array
C      C     R(*)   Real array equivalenced to K in calling routine
C   Generic version - uses ZHEX, assumes 32-bit integers
C-----------------------------------------------------------------------
      REAL      C(*)
      INTEGER   I1, I2, K(*)
C
      INTEGER   I, J, IT
      CHARACTER CVAL*4, HVAL*8, CT*1
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (I2.LT.I1) GO TO 999
         DO 30 I = I1,I2
            CVAL = ' '
            DO 15 J = 1,4
               CALL ZGETCH (IT, C(I), J)
               CT = CHAR (IT)
               IF ((CT.GE.' ') .AND. (CT.LE.'z')) CVAL(J:J) = CT
 15            CONTINUE
            CALL ZHEX (K(I), 8, HVAL)
            WRITE (MSGTXT,1015) I, K(I), HVAL, CVAL, C(I)
            CALL MSGWRT (3)
 30         CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1015 FORMAT (I6,3X,I14,3X,A8,3X,'''',A4,'''',3X,1PE12.5)
      END
