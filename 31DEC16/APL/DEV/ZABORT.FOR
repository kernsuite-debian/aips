      SUBROUTINE ZABORT (TSKNAM, ACTION)
C-----------------------------------------------------------------------
C! establishes or carries out (when appropriate) abort handling
C# Z System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004
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
C   ZABORT will take one of two actions depending on the value of
C   "action".  If "action" is zero, it establishes abort handling
C   to clean up for programs in the event of an ABORTASK or otherwise
C   fatal signal.  Hangup, interrupt (except for AIPS or BATER) and
C   quit are ignored.  If running under the control of a debugger, it
C   simply returns (to avoid affecting the debugger signal handling).
C   If "action" is non-zero, it will issue an illegal instruction in
C   order to induce the abort handler.  This is mostly to get a
C   traceback for debugging purposes (e.g., when an invalid argument
C   to a subroutine is detected).
C   Inputs:
C      TSKNAM   C*6   Program name
C      ACTION   I     Action indicator code:
C                        0 => establish abort handling
C                        1 => invoke an illegal instruction
C   Output:
C      none
C   Generic version - calls ZABOR2 after tests on TSKNAM.
C-----------------------------------------------------------------------
      CHARACTER TSKNAM*(*)
      INTEGER   ACTION
C
      INTEGER   I, TTYP
      HOLLERITH HTASK(2)
      INCLUDE 'INCS:DSAT.INC'
      INCLUDE 'INCS:VSAT.INC'
C-----------------------------------------------------------------------
C                                       is it a known stand-alone?
      TTYP = 0
      DO 10 I = 1,NSANAM
         IF (TSKNAM.EQ.SATASK(I)) TTYP = 1
 10      CONTINUE
C                                       pass that on to system-dependent
      CALL CHR2H (8, TSKNAM, 1, HTASK)
      CALL ZABOR2 (HTASK, TTYP, ACTION)
C
 999  RETURN
      END
