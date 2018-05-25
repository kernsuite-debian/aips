      SUBROUTINE SETANT (N, XANT, XBAS, NXANT, NXBAS, IANT, IBAS, DESEL)
C-----------------------------------------------------------------------
C! Builds a list of antennas and baselines from standard adverbs
C# UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1998
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
C   Converts standard adverbs ANTENNAS and BASELINE into integer lists,
C   counts, and the DESEL flag.
C   Inputs:
C      N       I      Dimension of XANT, etc.
C      XANT    R(N)   ANTENNAS adverb
C      XBAS    R(N)   BASELINE adverb
C   Outputs:
C      NXANT   I      Number of unique antennas in IANT
C      NXBAS   I      Number of unique antennas in IBAS
C      IANT    I(N)   List of unique antenna numbers
C      IBAS    I(N)   List of unique antenna numbers
C      DESEL   L      These lists are deselected (T) or selected (F)
C-----------------------------------------------------------------------
      INTEGER   N, NXANT, NXBAS, IANT(*), IBAS(*)
      REAL      XANT(*), XBAS(*)
      LOGICAL   DESEL
C
      INTEGER   I, II, J, IROUND
C-----------------------------------------------------------------------
      DESEL = .FALSE.
      NXANT = 0
      NXBAS = 0
      DO 40 I = 1,N
         II = IROUND (XANT(I))
         IF (II.NE.0) THEN
            II = ABS (II)
            DO 10 J = 1,NXANT
               IF (II.EQ.IANT(J)) GO TO 20
 10            CONTINUE
            NXANT = NXANT + 1
            IANT(NXANT) = II
            IF (XANT(I).LT.0.0) DESEL = .TRUE.
            END IF
 20      II = IROUND (XBAS(I))
         IF (II.NE.0) THEN
            II = ABS (II)
            DO 30 J = 1,NXBAS
               IF (II.EQ.IBAS(J)) GO TO 40
 30            CONTINUE
            NXBAS = NXBAS + 1
            IBAS(NXBAS) = II
            IF (XBAS(I).LT.0.0) DESEL = .TRUE.
            END IF
 40      CONTINUE
C
 999  RETURN
      END

