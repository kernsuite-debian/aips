      SUBROUTINE ZMYVER
C-----------------------------------------------------------------------
C! returns OLD, NEW, or TST based on translation of logical AIPS_VERSION
C# Service
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2002
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
C   Output in common /DCHCOM/:
C      VERNAM   C*4   String = 'OLD:', 'NEW:' or 'TST:'
C   Generic version.
C-----------------------------------------------------------------------
      CHARACTER HOLD*4, HNEW*4, HTST*4, HCVX*4
      INTEGER   LLEN, XLEN, XLNB, IERR, DLEN
      CHARACTER LOGNAM*48, XLATED*256, XDEF*256
      INCLUDE 'INCS:DDCH.INC'
      DATA HOLD, HNEW, HTST, HCVX /'OLD:', 'NEW:', 'TST:','CVX:'/
C-----------------------------------------------------------------------
      LLEN = LEN (LOGNAM)
      XLEN = LEN (XLATED)
C                                       Translate default AIPS version.
      LOGNAM = 'AIPS_VERSION'
      CALL ZTRLOG (LLEN, LOGNAM, XLEN, XDEF, XLNB, IERR)
C                                       Trouble translating logical
      IF (IERR.NE.0) THEN
         WRITE (6,1000)
         VERNAM = HNEW
C                                       Translate 'OLD', 'NEW' and 'TST'
C                                       and look for a match.
      ELSE
         DLEN = XLNB
C                                       Translate 'TST'.
         LOGNAM = 'TST'
         CALL ZTRLOG (LLEN, LOGNAM, XLEN, XLATED, XLNB, IERR)
         IF ((IERR.EQ.0) .AND. (XLATED(1:XLNB).EQ.XDEF(1:DLEN))) THEN
            VERNAM = HTST
            GO TO 999
            END IF
C                                       Translate 'NEW'.
         LOGNAM = 'NEW'
         CALL ZTRLOG (LLEN, LOGNAM, XLEN, XLATED, XLNB, IERR)
         IF ((IERR.EQ.0) .AND. (XLATED(1:XLNB).EQ.XDEF(1:DLEN))) THEN
            VERNAM = HNEW
            GO TO 999
            END IF
C                                       Translate 'OLD'.
         LOGNAM = 'OLD'
         CALL ZTRLOG (LLEN, LOGNAM, XLEN, XLATED, XLNB, IERR)
         IF ((IERR.EQ.0) .AND. (XLATED(1:XLNB).EQ.XDEF(1:DLEN))) THEN
            VERNAM = HOLD
            GO TO 999
            END IF
C                                       Translate 'CVX'.
         LOGNAM = 'CVX'
         CALL ZTRLOG (LLEN, LOGNAM, XLEN, XLATED, XLNB, IERR)
         IF ((IERR.EQ.0) .AND. (XLATED(1:XLNB).EQ.XDEF(1:DLEN))) THEN
            VERNAM = HCVX
            GO TO 999
            END IF
C                                       No match (assume NEW).
         WRITE (6,1010) XDEF(1:DLEN)
         VERNAM = HNEW
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (' ZMYVER: INVALID AIPS_VERSION = ',A,' (USING NEW)')
 1000 FORMAT (' ZMYVER: TROUBLE TRANSLATING LOGICAL = AIPS_VERSION',
     *   ' (USING NEW)')
      END
