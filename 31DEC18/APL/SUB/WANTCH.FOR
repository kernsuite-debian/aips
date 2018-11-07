      SUBROUTINE WANTCH (CHNS, ICHN, WT)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   Routine to decide if a channel is wanted, based on the CHNSEL adverb
C   Inputs:
C      CHNS(3,20)   I    Channel selector, start, stop and increment
C                        channel numbers
C      ICHN         I    The current channel number
C   Input/output
C      WT           R    The data weight, will be set to zero
C                        if channel is deselected.
C-----------------------------------------------------------------------
      INTEGER CHNS(3,20), ICHN
      REAL    WT
C
      INTEGER I, J, NINCR, NJUMP, K
      REAL    SAVWT
C-----------------------------------------------------------------------
      SAVWT = WT
      WT = 0.0
      DO 20 I = 1,20
         IF ((CHNS(2,I).GT.0) .AND. (ICHN.GE.CHNS(1,I)) .AND.
     *      (ICHN.LE.CHNS(2,I))) THEN
            K = MAX (1, CHNS(3,I))
            IF ((ICHN.EQ.CHNS(1,I)) .OR. (K.EQ.1)) THEN
               WT = SAVWT
               GO TO 999
               END IF
            NINCR = CHNS(2,I) - CHNS(1,I) + 1
            NJUMP = (NINCR / K) + 1
            DO 10 J = 1,NJUMP
               IF (ICHN.EQ.CHNS(1,I)+J*K) THEN
                  WT = SAVWT
                  GO TO 999
                  END IF
 10            CONTINUE
            END IF
 20      CONTINUE
C
 999  RETURN
      END
