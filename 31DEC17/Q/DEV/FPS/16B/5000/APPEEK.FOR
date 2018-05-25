      SUBROUTINE APPEEK(BUFFER)
C-----------------------------------------------------------------------
C! Routine to examine the memory of the AP.
C# AP-appl
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
C----------------------------------------------------------------------
C
C     APPEEK ALLOWS THE DEBUG EXAMINATION OF AP VARAIBLES.
C     VARIABLES ARE SET BY THE DEBUGGER.
C
C      LOOP = LOOP COUNTER
C      NLOOP = MAXIMUM NUMBER OF LOOPS
C      XAPLOC = R*8 START ADDRESS, CONVERTED TO P I*4 APLOC
C      NUM = I*2 UNSIGNED NUMBER OF LOCATIONS TO READ
C     ITYP = 1 FOR I*2 VALUES, 2 FOR R*4 VALUES, 3 TO DUMP SPAD
C            SPAD DUMP WILL BE TO SPDUMP.
C
C     THE RESULTS ARE RETURNED IN BUFFER
C
C      PROGRAMMER = W. D. COTTON, AUGUST 1980.
C----------------------------------------------------------------------
      INTEGER*2 LOOP,NLOOP,APLOC(2),NUB,ITYP,SPDUMP(16)
      REAL*4 BUFFER(1),OP8TO4
      REAL*8 XAPLOC
      DATA OP8TO4 /'8TO4'/
C----------------------------------------------------------------------
      CALL APWR
      NLOOP = 50
      XAPLOC = 0.0
      NUM = 0
      ITYP = 2
      DO 100 LOOP = 1 , NLOOP
C                                     CHANGE VALUES HERE
   50    CONTINUE
         IF ( NUM .LE. 0 ) GO TO 100
            CALL ZR8P4 (OP8TO4, APLOC, XAPLOC)
            IF(ITYP.LE.2) CALL APGET(BUFFER,APLOC,NUM,ITYP)
            CALL APWD
C                                       READ SPADS IF REQUESTED
      IF (ITYP .NE. 3) GO TO 60
      DO 65 I = 1 , 16
         CALL APGSP(II,I-1)
         CALL APWD
         SPDUMP(I) = II
   65    CONTINUE
C                                      EXAMINE RESULTS HERE
   60       CONTINUE
  100       CONTINUE
  999 RETURN
      END
