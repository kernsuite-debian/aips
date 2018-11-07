      SUBROUTINE HIOPEN (HLUN, KVOL, CNO, BUFFER, IERR)
C-----------------------------------------------------------------------
C! opens a history file, preparing common pointers and reading record 1
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
C   HIOPEN opens a history file in HITAB and FTAB
C   Inputs:
C      HLUN    I         desired lun
C      KVOL    I         disk number
C      CNO     I         catalog number
C   Output:
C      BUFFER  I(256)    first record in HI file
C      IERR    I         0 - ok
C                        1 - lun in use
C                        2 - no room for lun
C                        3 - other ZOPEN errors
C                        4 - IO errors
C                        5 - file does not exist
C-----------------------------------------------------------------------
      INTEGER   HLUN, KVOL, CNO, BUFFER(256), IERR
C
      CHARACTER HNAME*48
      INTEGER   IND, HPTR, IER
      LOGICAL   T, F
      INCLUDE 'INCS:DHIS.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                        open in HITAB
      CALL HILOCT ('OPEN', HLUN, HPTR, IERR)
      IF (IERR.NE.0) GO TO 999
      HITAB(HPTR+4) = KVOL
      HITAB(HPTR+5) = CNO
      HITAB(HPTR+6) = 1
C                                        open physical file
      CALL ZPHFIL ('HI', KVOL, CNO, 1, HNAME, IERR)
      CALL ZOPEN (HLUN, IND, KVOL, HNAME, F, T, T, IERR)
      IF (IERR.EQ.0) GO TO 20
         IER = IERR
         IF ((IER.NE.1) .AND. (IER.NE.5)) IERR = 3
         IF (IER.EQ.5) IERR = 2
         IF (IER.EQ.2) IERR = 5
         GO TO 900
C                                        read record one
 20   CALL ZFIO ('READ', HLUN, IND, 1, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 30
         IERR = 4
         GO TO 900
C                                        fill in HITAB
 30   HITAB(HPTR+1) = IND
      HITAB(HPTR+2) = BUFFER(1)
      HITAB(HPTR+3) = BUFFER(2)
      HITAB(HPTR+4) = KVOL
      HITAB(HPTR+5) = CNO
      HITAB(HPTR+6) = 1
      BUFFER(3) = HPTR
      GO TO 999
C                                        error
 900  CALL FILL (NHIWRD, 0, HITAB(HPTR))
      IF (IERR.EQ.4) CALL ZCLOSE (HLUN, IND, IER)
C
 999  RETURN
      END
