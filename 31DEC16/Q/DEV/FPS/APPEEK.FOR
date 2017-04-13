      SUBROUTINE APPEEK(BUFFER)
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
C     APPEEK allows the debug examination of ap varaibles.
c     variables are set by the debugger.
C
C      LOOP = Loop counter
C      NLOOP = Maximum number of loops
C      APLOC = Start address
C      NUM =  Number of locations to read
C      ITYP = 1 for I*2 values, 2 for real values, 3 to dump SPAD
C            SPAD dump will be to SPDUMP.
C     the results are returned in BUFFER
C      Programmer = W. D. Cotton, August 1980.
C----------------------------------------------------------------------
      INTEGER LOOP,NLOOP,APLOC,NUB,ITYP,SPDUMP(16),NUM,I,II
      REAL    BUFFER(*)
C----------------------------------------------------------------------
      CALL QWR
      NLOOP = 50
      APLOC = 0
      NUM = 0
      ITYP = 2
      DO 100 LOOP = 1,NLOOP
C                                     Change values here
   50    CONTINUE
         IF (NUM .LE. 0) GO TO 100
            IF (ITYP.LE.2) CALL APGET(BUFFER,APLOC,NUM,ITYP)
            CALL APWD
C                                       Read SPADS if requested
      IF (ITYP .NE. 3) GO TO 60
      DO 65 I = 1,16
         CALL QGSP(II,I-1)
         CALL QWD
         SPDUMP(I) = II
   65    CONTINUE
C                                      Examine results here
   60       CONTINUE
  100       CONTINUE
C
  999 RETURN
      END
