      SUBROUTINE HIADD (HLUN, CARD, BUFFER, IERR)
C-----------------------------------------------------------------------
C! adds a history record ("card" = 72 characters) to a history file
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
C   HIADD adds a history card to a history file.  IO takes place only
C   if necessary. Thus UPDATE = .TRUE. on HICLOS is required.
C   Inputs:
C      HLUN    I         LUN of HI file (must be open!!)
C      CARD    C*72      new card
C   In/out:
C      BUFFER  I(256)    HI work buffer
C   Output:
C      IERR    I         Error return: 0 => ok, other set by HIIO
C-----------------------------------------------------------------------
      CHARACTER CARD(1)*72
      INTEGER   HLUN, BUFFER(256), IERR
C
      INTEGER   HPTR, IER, INREC, INCNT, INMAX, NP
      INCLUDE 'INCS:DHIS.INC'
C-----------------------------------------------------------------------
C                                        locate in HITAB
      IERR = 1
      CALL HILOCT ('SRCH', HLUN, HPTR, IER)
      IF (IER.NE.0) GO TO 999
C                                        needed params
      INREC = HITAB (HPTR+6)
      INCNT = HITAB (HPTR+2)
      INMAX = (INCNT-1) / NHILPR + 1
C                                        add to existing file
      IF (INCNT.LE.0) GO TO 30
C                                        dont have current record
         IF (INREC.EQ.INMAX) GO TO 20
            CALL HIIO ('READ', HPTR, INMAX, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 999
            GO TO 30
C                                        record full: update file
 20      IF (MOD(INCNT,NHILPR).NE.0) GO TO 30
            CALL HIIO ('WRIT', HPTR, INMAX, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 999
C                                        update pointers, copy text
 30   INCNT = MAX (0, INCNT) + 1
      HITAB (HPTR+2) = INCNT
      HITAB (HPTR+6) = (INCNT-1) / NHILPR + 1
      NP = 5 + NHIWPL * MOD (INCNT-1, NHILPR)
      CALL CHR2H (72, CARD, 1, BUFFER(NP))
      IERR = 0
C
 999  RETURN
      END
