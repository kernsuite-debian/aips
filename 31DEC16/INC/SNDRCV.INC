C
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
C       SNDRCV.INC - Include file for S6DRCV program.
C
        IMPLICIT INTEGER (A-Z)
        INTEGER   BYTCNT, RECCNT, TBYTES, TRECNT
C
C       Parameters.
C
        PARAMETER EOT = 4               ! End of transmission.
        PARAMETER ENQ = 5               ! Enquire.
        PARAMETER ACK = 6               ! Acknowlegment.
        PARAMETER CR  = 13              ! Carriage return.
        PARAMETER NAK = 21              ! Negative Acknowlegment.
        PARAMETER SYN = 22              ! Synchronize byte.
        PARAMETER CAN = 24              ! Cancel transmission.
        PARAMETER EOF = 26              ! End of file.
        PARAMETER BUFSIZ = 256          ! Buffer size.
        PARAMETER RLIMIT = 8            ! Maximum # of retries.
        LOGICAL*1 RBUFF(BUFSIZ)         ! Receive buffer.
        LOGICAL*1 XBUFF(BUFSIZ)         ! Transmit buffer.
        LOGICAL*1 TBUFF(100)            ! Temporary buffer.
C
C       Logical Unit Numbers (LUNs) and Event Flags (EFNs).
C
        PARAMETER IOLUN = 1             ! File I/O LUN.
        PARAMETER TILUN = 5             ! Terminal input LUN.
        PARAMETER TOLUN = 6             ! Terminal output LUN.
        PARAMETER TIEFN = 5             ! Terminal input EFN.
        PARAMETER TOEFN = 6             ! Terminal output EFN.
C
C       The filename buffer.
C
        LOGICAL*1 FILNAM(40)            ! Store file name here.
C
C       Modes for Get/Send.
C
        PARAMETER IN=1, OUT=2
C
C       Logicals.
C
        LOGICAL FILOPN                  ! File open flag.
C
C       Commons.
C
        COMMON /BUFFER/ RBUFF, XBUFF, TBUFF, FILNAM
        COMMON /COUNTS/ FLOW, BCCERR, DAOERR, VERERR, TMOCNT
        COMMON /FLAGS/  FILOPN
C
C       End of SNDRCV.INC
C
