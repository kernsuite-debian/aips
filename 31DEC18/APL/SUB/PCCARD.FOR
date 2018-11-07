      SUBROUTINE PCCARD (GROUP, CARD, PCMATX, CDMATX, PVMATX)
C-----------------------------------------------------------------------
C! parses PCi_j, CDi_j, PVi_j FITS cards
C# FITS Parsing
C-----------------------------------------------------------------------
C;  Copyright (C) 2003, 2012
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
C   PCCARD parses PCi_j and CDi_j cards.  If there is any problem, such
C   as a format error in the card, then the output is left unchanged.
C   Inputs:
C      GROUP    I        1 for Random group, 0 otherwise (corrects
C                        subscripts by subtracting GROUP)
C      CARD     C*80     The FITS card image
C   In/Out
C      PCMATX   R(7,7)   The PC matrix values
C      CDMATX   R(7,7)   The CD matrix values
C      PVMATX   R(7,7)   The PV matrix values
C-----------------------------------------------------------------------
      INTEGER   GROUP
      CHARACTER CARD*80
      REAL      PCMATX(7,7), CDMATX(7,7), PVMATX(7,7)
C
      INTEGER   KBP, I, J, N
      CHARACTER CTEMP*4
      DOUBLE PRECISION X
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       get subscripts
      N = INDEX (CARD(:8), '_')
      IF (N.LE.3) GO TO 999
      CTEMP = CARD(3:N-1)
      KBP = 1
      CALL GETNUM (CTEMP, 4, KBP, X)
      IF (X.EQ.DBLANK) GO TO 999
      I = X + 0.01 - GROUP
      IF ((I.LE.0) .OR. (I.GT.7)) GO TO 999
      CTEMP = CARD(N+1:8)
      KBP = 1
      CALL GETNUM (CTEMP, 4, KBP, X)
      IF (X.EQ.DBLANK) GO TO 999
      J = X + 0.01 - GROUP
      IF ((J.LE.0) .OR. (J.GT.7)) GO TO 999
C                                       get value
      N = INDEX (CARD, '=')
      IF (N.LE.8) GO TO 999
      KBP = N + 1
      CALL GETNUM (CARD, 80, KBP, X)
      IF (X.NE.DBLANK) THEN
         IF (CARD(:2).EQ.'CD') CDMATX(I,J) = X
         IF (CARD(:2).EQ.'PC') PCMATX(I,J) = X
         IF (CARD(:2).EQ.'PV') PVMATX(I,J) = X
         END IF
C
 999  RETURN
      END
