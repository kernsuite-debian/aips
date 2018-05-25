      SUBROUTINE ALSH2C (HOLL, NDIM, DIM, STRING)
C-----------------------------------------------------------------------
C! Convert hollerith to character with possible multiple dimensionality
C# Utility 
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
C   Convert a hollerith to a string (possibly an array).
C   This routine should deal with either the case that the output string
C   is declared either as C*(DIM1*DIM2) or C(DIM1)*DIM2.
C   Inputs:
C      HOLL     H(*)    Hollerith version of character string.
C      NDIM     I       Number of dimensions
C      DIM      I(*)    Dimensionality array
C   Outputs:
C      STRING   C(*)*?  A string, possibly an array.
C      IERR     I       Error code, 0=OK. 1= file not open
C-----------------------------------------------------------------------
      CHARACTER STRING(*)*(*)
      INTEGER   NDIM, DIM(*)
      HOLLERITH HOLL(*)
C
      INTEGER   SLEN, LOOP, NVAL, INDEX
C-----------------------------------------------------------------------
C                                       Total length
      NVAL = 1
      DO 10 LOOP = 1,NDIM
         NVAL = NVAL * MAX (1, DIM(LOOP))
 10      CONTINUE
C                                       Check string length
      SLEN = LEN (STRING(1))
      IF (SLEN .GE. NVAL) THEN
C                                       STRING passed as one string
         CALL H2CHR (NVAL, 1, HOLL, STRING)
      ELSE
C                                       STRING passed as array.
         NVAL = NVAL / MAX (1, DIM(1))
         INDEX = 1
         DO 100 LOOP = 1,NVAL
            CALL H2CHR (SLEN, INDEX, HOLL, STRING(LOOP))
            INDEX = INDEX + SLEN
 100        CONTINUE
         END IF
C
 999  RETURN
      END
