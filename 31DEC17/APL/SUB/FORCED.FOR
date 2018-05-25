      LOGICAL FUNCTION FORCED (N, FLDSZ)
C-----------------------------------------------------------------------
C! decides if a field size should be forced to use DFT
C# Imaging
C-----------------------------------------------------------------------
C;  Copyright (C) 2004, 2006, 2011
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
C   Returns .TRUE. is an image size should use DFT modeling methods -
C   small (<= 128) or non-power of 2 will set it true.
C   Inputs:
C      N        I        Number of fields
C      FLDSZ    I(2,N)   Image sizes (x,y)
C   Output:
C      FORCED   L      TRUE => force DFT, else choose as usual
C-----------------------------------------------------------------------
      INTEGER   N, FLDSZ(2,*)
C
      INTEGER   I, J, SMAX, SMIN, SAVG, K, M
      LOGICAL   MSG
      INCLUDE 'INCS:DMSG.INC'
      SAVE MSG
      DATA MSG /.TRUE./
C-----------------------------------------------------------------------
      FORCED = .FALSE.
      SMAX = 0
      SMIN = 10000000
      SAVG = 0
      K = 0
      DO 20 M = 1,N
         DO 10 I = 1,2
            IF (FLDSZ(I,M).GT.0) THEN
               IF (FLDSZ(I,M).LT.768) THEN
                  CALL POWER2 (FLDSZ(I,M), J)
                  IF (FLDSZ(I,M).NE.J) FORCED = .TRUE.
                  END IF
               SMAX = MAX (SMAX, FLDSZ(I,M))
               SMIN = MIN (SMIN, FLDSZ(I,M))
               SAVG = SAVG + FLDSZ(I,M)
               K = K + 1
               END IF
 10         CONTINUE
 20      CONTINUE
      IF (K.GT.0) THEN
         SAVG = SAVG / K
         IF (SAVG.LE.128) FORCED = .TRUE.
         IF (SMIN.LT.64) FORCED = .TRUE.
         IF ((.NOT.FORCED) .AND. (SMAX.GT.8192)) THEN
            FORCED = .TRUE.
            MSGTXT = 'FORCED: LARGE IMAGES SHOULD USE DFT SUBTRACTION'
            IF (MSG) CALL MSGWRT (5)
            MSG = .FALSE.
            END IF
         END IF
C
 999  RETURN
      END
