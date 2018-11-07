      SUBROUTINE HICLOS (HLUN, UPDATE, BUFFER, IERR)
C-----------------------------------------------------------------------
C! closes a history file, flushing the buffer if desired
C# History
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997
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
C   HICLOS closes a history file updating it if requested.
C   Inputs:
C      HLUN    I         file LUN (already open!!)
C      UPDATE  L         T => write last record & update pointers
C   In/out:
C      BUFFER  I(256)    HI work buffer
C   Output:
C      IERR    I         error code : 0 - ok
C                                     1 - LUN not open
C                                   2-6 - ZFIO errors
C-----------------------------------------------------------------------
      INTEGER   HLUN, BUFFER(256), IERR
      LOGICAL   UPDATE
C
      INTEGER   HPTR, IER, HIND, INREC, LSIZE
      CHARACTER HNAME*48
      INCLUDE 'INCS:DHIS.INC'
C-----------------------------------------------------------------------
C                                        locate in HITAB
      IERR = 1
      CALL HILOCT ('SRCH', HLUN, HPTR, IER)
      IF (IER.NE.0) GO TO 999
C                                        do update
      IERR = 0
      IF (UPDATE) THEN
         INREC = HITAB (HPTR+6)
         IF (INREC.GT.1) THEN
            CALL HIIO ('WRIT', HPTR, INREC, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 50
            END IF
         IF (INREC.NE.1) THEN
            CALL HIIO ('READ', HPTR, 1, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 50
            END IF
C                                       compression?
         IF (HITAB(HPTR+3)-HITAB(HPTR+2).GT.10*NHILPR) THEN
            LSIZE = (HITAB(HPTR+2)-1) / NHILPR + 12
            CALL ZPHFIL ('HI', HITAB(HPTR+4), HITAB(HPTR+5), 1, HNAME,
     *         IERR)
            CALL ZCMPRS (HITAB(HPTR+4), HNAME, HLUN, LSIZE, IERR)
            HITAB(HPTR+3) = LSIZE * NHILPR
            END IF
C                                       update pointers
         BUFFER(1) = HITAB (HPTR+2)
         BUFFER(2) = HITAB (HPTR+3)
         CALL HIIO ('WRIT', HPTR, 1, BUFFER, IERR)
         END IF
C                                        do close
 50   HIND = HITAB (HPTR+1)
      CALL FILL (NHIWRD, 0, HITAB(HPTR))
      CALL ZCLOSE (HLUN, HIND, IER)
C
 999  RETURN
      END
