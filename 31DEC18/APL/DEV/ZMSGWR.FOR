      SUBROUTINE ZMSGWR (MSGBUF, MSGLEV)
C-----------------------------------------------------------------------
C! call MSGWRT based on call arguments - for C routines to call MSGWRT
C# Z2 Service
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
C   Issues a call to MSGWRT with with the message contained in MSGBUF.
C   This is only called by routines that cannot otherwise access MSGTXT
C   as stored in /MSGCOM/ and call MSGWRT themselves (e.g., routines
C   written in "C").
C   Inputs:
C      MSGBUF   H(20)   Buffer containing message (80 characters)
C      MSGLEV   I       Message level argument for MSGWRT
C   Output:
C      None
C   Generic version - uses H2CHR, JTRIM, MSGWRT.
C-----------------------------------------------------------------------
      HOLLERITH MSGBUF(20)
      INTEGER   MSGLEV
C
      INTEGER   I, JTRIM
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL H2CHR (80, 1, MSGBUF, MSGTXT)
C                                       Blank fill starting at position
C                                       of first NUL, if any.
      I = JTRIM (MSGTXT)
C                                       Issue the message via MSGWRT.
      IF (I.GT.0) CALL MSGWRT (MSGLEV)
C
 999  RETURN
      END
