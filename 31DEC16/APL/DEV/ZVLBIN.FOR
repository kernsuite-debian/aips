      SUBROUTINE ZVLBIN (NVAL, ARRAY, RARR, DARR, IARR, IRET)
C-----------------------------------------------------------------------
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
C  Routine to perform the equivalence necessary for NRAO/SAO DECODE
C  format VLBI data.
C
C    Inputs:
C      NVAL           I           No. integer*2 words in ARRAY
C      ARRAY(*)       I*2         Integer*2 array read from disc
C    Outputs:
C      DARR(*)        D           Double precision output array
C      RARR(*)        R           Real output array
C      IARR(*)        I           Integer output array
C      IRET           I           Error flag
C-----------------------------------------------------------------------
      INTEGER   NVAL, IRET, IARR(*)
      INTEGER*2 ARRAY(*)
      REAL      RARR(*)
      DOUBLE PRECISION DARR(*)
C
      INTEGER   I4, I8, I
      INTEGER*2 ITEMP(1024)
      REAL      TEMP(512)
      DOUBLE PRECISION DTEMP(256)
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (ITEMP, TEMP, DTEMP)
C-----------------------------------------------------------------------
      IRET = 0
      IF ((NVAL.LE.0) .OR. (NVAL.GT.1024)) THEN
         WRITE (MSGTXT,1000) NVAL
         IRET = 1
         GO TO 990
         END IF
C                                       Copy integer array
      DO 100 I = 1, NVAL
         ITEMP(I) = ARRAY(I)
         IARR(I) = ARRAY(I)
 100     CONTINUE
C                                       Copy real array
      I4 = NVAL / 2
      DO 200 I = 1, I4
         RARR(I) = TEMP(I)
 200     CONTINUE
C                                       Copy double precision array
      I8 = NVAL / 4
      DO 300 I = 1, I8
         DARR(I) = DTEMP(I)
 300     CONTINUE
      GO TO 999
C
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZVLBIN: ERROR, # I*2 VALUES INCORRECT: NVAL = ',I10)
      END
