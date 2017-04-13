      SUBROUTINE FXHDEX (CATBLK)
C-----------------------------------------------------------------------
C! Corrects header format from 20 to 50 extension file types
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1997
C;  Associated Universities, Inc. Washington DC, USA.
C;  Eric W. Greisen
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
C   FXHDEX checks the header to see if it is in the old format with a
C   maximum of 20 extension file types or the new (18-Mar-1997) format
C   with 50.  It changes old ones to new ones.
C   In/out::
C      CATBLK   I(256)   File header
C-----------------------------------------------------------------------
      INTEGER   CATBLK(256)
C
      INTEGER   J, I, K
      CHARACTER TEST*2
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      J = KHEXT + 20 - 1
C                                       check 3 types for legitimate
C                                       characters since DEC machines
C                                       can have some possible
C                                       ambiguities, but there will
C                                       never be two types with > 8000
C                                       versions
      DO 10 I = 1,3
         J = J + 1
C                                       old version # in range ?
         IF (CATBLK(J).GT.46656) GO TO 999
         IF (CATBLK(J).LT.0) GO TO 999
C                                       test characters
         CALL H2CHR (2, 1, CATBLK(J), TEST)
         IF (TEST.EQ.'  ') GO TO 10
         IF ((TEST(:1).GE.'A') .AND. (TEST(:1).LE.'Z') .AND.
     *      (TEST(2:).GE.'A') .AND. (TEST(2:).LE.'Z')) GO TO 10
C                                       old format
            CALL COPY (20, CATBLK(KHEXT+20), CATBLK(KIVER))
            K = KIEXTN - 20
            CALL FILL (K, 0, CATBLK(KIVER+20))
            CALL RFILL (K, HBLANK, CATBLK(KHEXT+20))
            GO TO 999
 10      CONTINUE
C
 999  RETURN
      END

