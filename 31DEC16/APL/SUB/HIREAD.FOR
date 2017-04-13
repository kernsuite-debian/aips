      SUBROUTINE HIREAD (HLUN, HIREC, CARD, BUFFER, IERR)
C-----------------------------------------------------------------------
C! Reads next history card from a history file
C# History IO-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2013
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
C   HIADD reads next history card from a history file.
C   IO takes place only if necessary.
C   Inputs:
C      HLUN     I        Lun of HI file (must be open!!)
C   In/out:
C      HIREC    I        Logical rec no to read,
c                           incremented by one on out
C      BUFFER   I(256)   HI work buffer
C   Output:
C      CARD     C*72     History card
c      IERR     I        0 => ok, other set by HIIO
C-----------------------------------------------------------------------
      INTEGER   HLUN, HIREC, BUFFER(256), IERR
      CHARACTER CARD*(*)
C
      INTEGER   HPTR, IER, PHREC, SKPREC, NP
      INCLUDE 'INCS:DHIS.INC'
C-----------------------------------------------------------------------
C                                        locate in HITAB
      IERR = 1
      CALL HILOCT ('SRCH', HLUN, HPTR, IER)
      IF (IER.NE.0) GO TO 999
C                                        needed params
      PHREC = HITAB (HPTR+6)
      SKPREC = ((HIREC-1)/NHILPR)+1
C                                        Have correct physical record?
 20   IF (PHREC.LT.SKPREC) THEN
         PHREC = PHREC + 1
         CALL HIIO ('READ', HPTR, PHREC, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
         GO TO 20
         END IF
C                                        update pointers, copy text
      NP = 5 + NHIWPL * MOD (HIREC-1, NHILPR)
      CALL H2CHR (72, 1, BUFFER(NP), CARD)
      HITAB(HPTR+6) = PHREC
      HIREC = HIREC+1
      IERR = 0
C
 999  RETURN
      END
