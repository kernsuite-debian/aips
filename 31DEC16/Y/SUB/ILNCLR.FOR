      SUBROUTINE ILNCLR (COLOR, NPOINT, NEND, SLOPE, OFFSET, GAMMA,
     *   BUFFER, IERR)
C-----------------------------------------------------------------------
C! computes and loads a piecewise linear OFM to the TV
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2008
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
C   ILNCLR computes a piecewise linear OFM and writes it to the TV.
C   If NEND(NPOINT) is <= OFMINP/2 or /3 or /4, the table is repeated
C   an approprite number of times.
C   Inputs:
C      COLOR   I            color bit mask: RGB = 421
C      NPOINT  I            # of segments
C      NEND    I            end points of segments
C      SLOPE   R(NPOINT)    slopes of segments
C      OFFSET  R(NPOINT)    offsets of segments
C      GAMMA   R            power applied to colors (1 /gamma)
C   Output:
C      BUFFER  R(TVMOFM)    scratch buffer
C      IERR    I            error code of ZM70XF : 0 - ok
C   Form is C = (i-1)*SLOPE + OFFSET with 0 <= C <= 1.0.
C   Used to be called YLNCLR.
C-----------------------------------------------------------------------
      INTEGER   COLOR, NPOINT, NEND(*), IERR
      REAL      SLOPE(*), OFFSET(*), GAMMA, BUFFER(*)
C
      INTEGER   I1, I2, K, I, II, JJ, KK
      REAL      S, OF, C
      LOGICAL   VRTRTC
      INCLUDE 'INCS:DTVC.INC'
      DATA VRTRTC /.TRUE./
C-----------------------------------------------------------------------
      K = OFMINP + 1
      CALL RFILL (K, 0.0, BUFFER)
      I2 = 0
C                                        loop over segments
      DO 20 K = 1,NPOINT
         I1 = I2 + 1
         I2 = NEND(K)
         S = SLOPE(K)
         OF = OFFSET(K)
         I1 = MAX (1, I1)
         IF ((I2.GE.I1) .AND. ((S.NE.0.0) .OR. (OF.NE.0.0))) THEN
            IF (ABS(GAMMA-1.0).GT.0.02) THEN
               DO 10 I = I1,I2
                  C = (I-1)*S + OF
                  IF (C.GE.1.0) THEN
                     BUFFER(I) = 1.0
                  ELSE IF (C.GT.0.0) THEN
                     BUFFER(I) = C ** GAMMA
                     END IF
 10               CONTINUE
            ELSE
               DO 15 I = I1,I2
                  C = (I-1)*S + OF
                  IF (C.GE.0.0) BUFFER(I) = MIN (1.0, C)
 15               CONTINUE
               END IF
            END IF
 20      CONTINUE
C                                        expand to fill
      BUFFER(1) = 0.
      K = NEND(NPOINT)
      JJ = K
      KK = (OFMINP+1) / K
      DO 21 II = 2,KK
         CALL RCOPY (K, BUFFER, BUFFER(JJ+1))
         JJ = JJ + K
 21      CONTINUE
C
      CALL YOFM ('WRIT', COLOR, VRTRTC, BUFFER, IERR)
C
 999  RETURN
      END
