      SUBROUTINE HIAD80 (HLUN, IST, CARD, HBLK, IERR)
C-----------------------------------------------------------------------
C! puts an 80-character card image into a history file as required
C# History
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
C   HIAD80 puts an 80-character card image into a history file.  It
C   actually puts 0 (CARD all blank), 1 (<= 72 chars), or 2 cards in
C   the file.
C   Inputs:
C      HLUN   I         LUN of open history file
C      IST    I         Start character position in card
C      CARD   C*80      80-character packed "card"
C   In/out:
C      HBLK   I(256)    HI I/O buffer
C   Output:
C      IERR   I         Error code of HIADD
C-----------------------------------------------------------------------
      CHARACTER CARD*80
      INTEGER   HLUN, IST, HBLK(256), IERR
C
      INTEGER   I, II, NA, JTRIM
      CHARACTER HILINE*72
C-----------------------------------------------------------------------
      IERR = 0
      IF ((IST.LT.1) .OR. (IST.GT.80)) GO TO 999
      NA = JTRIM (CARD)
      IF (NA.GT.80) NA = 80
      IF (NA.LT.IST) GO TO 999
C                                       First card
      II = 81 - IST
      IF (II.GT.72) II = 72
      HILINE = CARD(IST:IST+II-1)
      CALL HIADD (HLUN, HILINE, HBLK, IERR)
      IF ((IERR.NE.0) .OR. (IST.GT.8)) GO TO 999
C                                       Any more characters?
      II = 72 + IST
      IF (NA.LT.II) GO TO 999
C                                       Second card
      HILINE = 'CONTINUED:'
      I = 81 - II
      HILINE(11:) = CARD(II:II+I-1)
      CALL HIADD (HLUN, HILINE, HBLK, IERR)
C
 999  RETURN
      END
