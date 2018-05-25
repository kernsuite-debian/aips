      SUBROUTINE ZVTPGC (THOSTS, THLEN, NTH, FCB, IERR)
C-----------------------------------------------------------------------
C! close & reopen connection in server (real-TP) to client (virtual-TP)
C# Tape Z2
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
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
C   ZVTPGC opens the connection in TPMON to the remote machine which is
C   running the AIPS Virtual tape code.  This differs from ZVTPO3 in
C   that the socket is already there and an old connection must be
C   closed before a new one can be accepted.  Called by ZVTPRX.
C   Inputs:
C      THOSTS H(*)  up to 512 hosts or pseudo-wildcards
C      THLEN  I     Length of each character string (fixed)
C      NTH    I     Number of hosts to check
C                   (see TPMON.FOR local include for details)
C   Inputs/Outputs:
C      FCB   I(*)   File descriptor
C   Outputs
C      IERR  I      Error: 0 => okay
C   Generic version - stub
C-----------------------------------------------------------------------
      HOLLERITH THOSTS(*)
      INTEGER   THLEN, NTH, FCB(*), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 8
      WRITE (MSGTXT,1000)
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZVTPGC: REQUIRES DEVELOPMENT; GENERIC IS A STUB')
      END
