      SUBROUTINE EVTHDR (LOPCOD, LEVENT, VERS, IBUF, NBUF, NBYTES,
     *   IRET)
C-----------------------------------------------------------------------
C! Encode/decode an AIPS XDR event header containing name and vers. no.
C# IO-remote
C-----------------------------------------------------------------------
C;  Copyright (C) 1997
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
C   Encode/decode an XDR AIPS event header containing event name
C   and version number.
C   Inputs:
C      LOPCOD     C*6     Opcode ('ENCODE' or 'DECODE')
C   Input/output:
C      LEVENT     C*8     Event name
C      VERS       R       Event version number.
C      IBUF       I(*)    XDR buffer
C      NBUF       I       Dimension of IBUF
C   Output:
C      NBYTES     I       Current XDR byte offset in IBUF
C      IRET       I       Return code (0=> ok; else error)
C-----------------------------------------------------------------------
      CHARACTER LOPCOD*6, LEVENT*8
      REAL      VERS
      INTEGER   NBUF, NBYTES, IRET
      INTEGER   IBUF(NBUF)
C
      CHARACTER LOPT(2)*6
      INTEGER   IERR, IOPT, NLEN, JTRIM
      INCLUDE 'INCS:DMSG.INC'
      DATA LOPT /'ENCODE', 'DECODE'/
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Encode or decode ?
      IF (LOPCOD.EQ.'ENCODE') THEN
         IOPT = 1
      ELSE
         IOPT = 2
         END IF
C                                       Encode or decode event name
      NBYTES = 0
      IF (IOPT.EQ.1) NLEN = JTRIM (LEVENT)
      IF (IOPT.EQ.2) NLEN = 8
      CALL ZXDRST (LOPT(IOPT), LEVENT, NLEN, IBUF, NBUF, NBYTES, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1050) IERR, LOPT(IOPT)
         GO TO 990
         END IF
C                                       Encode or decode version number
      NLEN = 1
      CALL ZXDRFP (LOPT(IOPT), VERS, NLEN, IBUF, NBUF, NBYTES, IERR)
      IF (IERR.NE.0) THEN
         IRET = 2
         WRITE (MSGTXT,1100) IERR, LOPT(IOPT)
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C                                       Exit
 999  RETURN
C----------------------------------------------------------------------
1050  FORMAT ('EVTHDR: ERROR',I4,' IN EVENT NAME ',A6)
1100  FORMAT ('EVTHDR: ERROR',I4,' IN VERSION NO. ',A6)
      END
