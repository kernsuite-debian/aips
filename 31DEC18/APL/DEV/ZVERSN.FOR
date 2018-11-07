      SUBROUTINE ZVERSN
C-----------------------------------------------------------------------
C! returns code for OLD,NEW,TST based on matching RLSNAM to logicals
C# Z Service
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2002
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
C   Determine the default AIPS version (OLD or NEW or TST).  Error
C   messages are only written to the terminal since this routine is
C   called before message handling via MSGWRT is established.
C   Input in common /DCHCOM/:
C      RLSNAM   C**   i.e. '15APR90 '
C   Output in common /MSGCOM/:
C      MSGVER   I     For accounting: 0 fails, 1 OLD, 2 NEW, 3 TST
C                        4 CVX
C   Generic version.
C-----------------------------------------------------------------------
      INTEGER   L, XLEN, XLNB, IERR, ITRIM, CLEN
      CHARACTER LOGNAM*48, XLATED*256
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      MSGVER = 0
      CLEN = ITRIM (RLSNAM)
      IF (CLEN.LE.0) GO TO 999
C                                       Translate 'OLD', 'NEW' and 'TST'
C                                       and look for a match.
      XLEN = LEN (XLATED)
      LOGNAM = 'TST'
      CALL ZTRLOG (3, LOGNAM, XLEN, XLATED, XLNB, IERR)
      IF (IERR.EQ.0) THEN
         L = INDEX (XLATED(:XLNB), RLSNAM(:CLEN))
         IF (L.GT.0) THEN
            MSGVER = 3
            GO TO 999
            END IF
         END IF
      LOGNAM = 'NEW'
      CALL ZTRLOG (3, LOGNAM, XLEN, XLATED, XLNB, IERR)
      IF (IERR.EQ.0) THEN
         L = INDEX (XLATED(:XLNB), RLSNAM(:CLEN))
         IF (L.GT.0) THEN
            MSGVER = 2
            GO TO 999
            END IF
         END IF
      LOGNAM = 'OLD'
      CALL ZTRLOG (3, LOGNAM, XLEN, XLATED, XLNB, IERR)
      IF (IERR.EQ.0) THEN
         L = INDEX (XLATED(:XLNB), RLSNAM(:CLEN))
         IF (L.GT.0) THEN
            MSGVER = 1
            GO TO 999
            END IF
         END IF
      XLEN = LEN (XLATED)
      LOGNAM = 'CVX'
      CALL ZTRLOG (3, LOGNAM, XLEN, XLATED, XLNB, IERR)
      IF (IERR.EQ.0) THEN
         L = INDEX (XLATED(:XLNB), RLSNAM(:CLEN))
         IF (L.GT.0) THEN
            MSGVER = 4
            GO TO 999
            END IF
         END IF
C
 999  RETURN
      END
