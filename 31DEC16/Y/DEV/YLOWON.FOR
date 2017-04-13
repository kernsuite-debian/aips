      SUBROUTINE YLOWON (MASK, ICHAN)
C-----------------------------------------------------------------------
C! select least on bit in a bit mask integer
C# Y0 TV-util
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
C   YLOWON returns a zero-relative least bit on number up to NGRAY.
C   If MASK=0, it returns 0.  If MASK > 2 ** (NGRAY-1) it returns NGRAY.
C   Since some TVs can't have more than one channel on at a time in
C   each color, determine the lowest channel that is flagged ON in MASK,
C   and return it (zero relative) in ICHAN.  If MASK is clear, set ICHAN
C   to be a 0.
C   Inputs:
C      MASK    I   IIS type channel mask
C   Output:
C      ICHAN   I   # of lowest bit set in MASK
C-----------------------------------------------------------------------
      INTEGER   MASK, ICHAN
      INTEGER   BITS(15), ZAND, I
      INCLUDE 'INCS:DTVC.INC'
      DATA BITS /1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384/
C-----------------------------------------------------------------------
      ICHAN = NGRAY
      IF (MASK.EQ.0) ICHAN = 0
C
      DO 10 I = 1,NGRAY
         IF (ZAND(MASK, BITS(I)).EQ.0) GO TO 10
             ICHAN = I - 1
             GO TO 999
 10      CONTINUE
C
 999  RETURN
      END
