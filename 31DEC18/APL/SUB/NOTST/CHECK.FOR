      SUBROUTINE CHECK (SUBR, NUM, ASSERT, IRET)
C-----------------------------------------------------------------------
C! Assertion checking subroutine
C# UTIL
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
C   Set IRET to 0 if ASSERT is true otherwise print a diagnostic
C   message and set IRET to a non-zero number.
C
C   Inputs:
C      SUBR     C*(*)       Name of subroutine in which assertion is
C                           made.
C      NUM      I           Checkpoint number
C      ASSERT   L           Assertion
C
C   Output:
C      IRET     I           Status: 0 - assertion true
C                                 999 - assertion false
C
C   Usage notes:
C      This subroutine is intended as a tool for uncovering logic flaws
C      in AIPS programs.  CHECK should be called with a boolean
C      expression that is assumed to hold at that point.  If the
C      expression evaluates to false then it can be inferred that there
C      is an error in the logic of the calling program.  To be of most
C      use in finding logic errors, each call to CHECK in a given
C      subroutine should be supplied with the name of the calling
C      subroutine and a unique checkpoint number, both of which will
C      be written to the message file should the assertion be false.
C
C      CHECK should not be used in computationally intensive code
C      neither should it be used to check conditions that may
C      legitimately be expected to be false during the normal
C      operation of a program (eg. ranges of user-supplied data).
C-----------------------------------------------------------------------
      CHARACTER SUBR*(*)
      INTEGER   NUM, IRET
      LOGICAL   ASSERT
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (ASSERT) THEN
         IRET = 0
      ELSE
         WRITE (MSGTXT, 1000) NUM, SUBR
         CALL MSGWRT (9)
         MSGTXT = 'An erroneous condition has been detected in this '
     *            // 'program. Report'
         CALL MSGWRT (9)
         MSGTXT = 'this message to the designated AIP and include '
     *            // 'the checkpoint'
         CALL MSGWRT (9)
         MSGTXT = 'information given above. Save the data on which '
     *            // 'you were working'
         CALL MSGWRT (9)
         MSGTXT = 'and record the task inputs in case they are needed '
     *            // 'to reproduce'
         CALL MSGWRT (9)
         MSGTXT = 'the problem.'
         CALL MSGWRT (9)
         IRET = 999
      END IF
C-----------------------------------------------------------------------
 1000 FORMAT ('ASSERTION ERROR: CHECKPOINT ', I3, ' IN ', A)
      END
