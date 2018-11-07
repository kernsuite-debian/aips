      SUBROUTINE RLHIST (OPCODE)
C-----------------------------------------------------------------------
C! initializes or closes GNU readline history
C# Terminal
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   RLHIST initializes user history (OPCODE="OPEN") or closes user
C   history (OPCODE="CLOS").  Also calls initializer for TAB completions
C   (for OPCODE="OPEN").
C
C   Input:
C      OPCODE C*4    Operation (OPEN or CLOS)
C   Output:
C      None
C
C   JAU 11 Aug 95: Jeff Uphoff, NRAO.
C   JAU 22 Sep 95: Jeff Uphoff, NRAO.  Added TAB completion init.
C-----------------------------------------------------------------------
      CHARACTER  OPCODE*4
      CHARACTER  PNAM*48
      HOLLERITH  PNAMH(12)
      INTEGER    IERR
C
      INCLUDE 'INCS:DMSG.INC'
C
      IF (OPCODE.EQ.'OPEN') THEN
C                                       Build filename for history
         CALL ZPHFIL ('RL', 1, 0, NPOPS, PNAM, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL CHR2H (48, PNAM, 1, PNAMH)
C                                       Recover previous history
         CALL ZGRLHI (1, PNAMH, IERR)
         IF (IERR.EQ.0) THEN
            MSGTXT = 'Previous session command-line history recovered.'
         ELSE
            MSGTXT = 'Previous session command-line history *not*' //
     *         ' recovered.'
            END IF
         CALL MSGWRT (2)
C                                       Initialize TAB expansions
         CALL ZGRLTC (IERR)
         IF (IERR.EQ.0) THEN
            MSGTXT = 'TAB-key completions enabled,' //
     *         ' type HELP READLINE for details.'
         ELSE
            MSGTXT = 'TAB-key completions *not* enabled.'
            END IF
         CALL MSGWRT (2)
         GO TO 999
         END IF
C
      IF (OPCODE.EQ.'CLOS') THEN
         CALL ZPHFIL ('RL', 1, 0, NPOPS, PNAM, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL CHR2H (48, PNAM, 1, PNAMH)
C                                       Save new history
         CALL ZGRLHI (2, PNAMH, IERR)
         IF (IERR.EQ.0) THEN
            MSGTXT = 'Session command-line history saved.'
         ELSE
            MSGTXT = 'Session command-line history *not* saved.'
            END IF
         CALL MSGWRT (2)
         GO TO 999
         END IF
C
 999  RETURN
      END
