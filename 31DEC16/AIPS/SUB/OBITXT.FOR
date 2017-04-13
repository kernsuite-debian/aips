      SUBROUTINE OBITXT (NOBLNK, LUNL, INDL, LUNR, INDR, LINE, IERR)
C-----------------------------------------------------------------------
C! does text output for 2 files - used by OBIT verbs
C# POPS-appl TEXT
C-----------------------------------------------------------------------
C;  Copyright (C) 2011
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
C   Writes line to 2 open text files
C   Inputs:
C      NOBLNK   L      T => remove any blanks
C      LUNL     I       LUN 1
C      INDL     I       FTAB pointer 1
C      LUNR     I       LUN 2
C      INDR     I       FTAB pointer 2
C      LINE     C*(*)   text line
C   Outputs:
C      IERR     I       > 0 => error, message written
C-----------------------------------------------------------------------
      LOGICAL   NOBLNK
      INTEGER   LUNL, INDL, LUNR, INDR, IERR
      CHARACTER LINE*(*)
C
      INTEGER   I, J, JJ, JTRIM
      CHARACTER OUTLIN*200
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       squeeze embedded blanks
      JJ = JTRIM (LINE)
      IF (NOBLNK) THEN
         OUTLIN = ' '
         J = 0
         DO 10 I = 1,JJ
            IF (LINE(I:I).NE.' ') THEN
               J = J + 1
               OUTLIN(J:J) = LINE(I:I)
               END IF
 10         CONTINUE
      ELSE
         OUTLIN = LINE
         J = JTRIM (OUTLIN)
         END IF
C                                       now write
      CALL ZTXIO ('WRIT', LUNL, INDL, OUTLIN(:J), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'WRITING LOG FILE'
         GO TO 970
         END IF
      CALL ZTXIO ('WRIT', LUNR, INDR, OUTLIN(:J), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'WRITING RUN FILE'
         GO TO 970
         END IF
      GO TO 999
C
 970  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OBITXT ERROR',I5,' ON ',A)
      END

