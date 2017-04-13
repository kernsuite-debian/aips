      SUBROUTINE PTPARM (NPARMS, RPARM, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! starts tasks, getting parameters and task ID number, does accounting
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 2002
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
C   PTPARM uses the activator task number to return parameters to AIPS
C   adverbs.  Call it before DIETSK and never if RQUICK was true.
C   Inputs:
C      NPARMS  I          Number of real variables wanted
C      RPARM   R(NPARMS)  Parameters received
C   Output:
C      SCRTCH  I(256)     Scratch buffer
C      IERR    I          Error code: 0 -> ok
C                                     2 -> disk troubles
C-----------------------------------------------------------------------
      INTEGER   NPARMS, SCRTCH(256), IERR
      REAL      RPARM(*)
C
      INTEGER   FINDTD, ISCR, I, IER, J, LUNTD, NBL, JBL
      REAL      RSCR
      LOGICAL   T, F
      CHARACTER PHNAME*48, PTYPE*2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (RSCR, ISCR)
      DATA LUNTD, PTYPE /13, 'TD'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      NBL = 4
C                                       set initial values
      IERR = 0
C                                       open task data file
      CALL ZPHFIL (PTYPE, 1, 0, 4, PHNAME, IER)
      CALL ZOPEN (LUNTD, FINDTD, 1, PHNAME, F, T, T, IER)
      IF (IER.NE.0) THEN
         IERR = 2
         GO TO 999
         END IF
C                                       read directory record
      CALL ZFIO ('READ', LUNTD, FINDTD, 1, SCRTCH, IER)
      IF (IER.NE.0) GO TO 980
      NBL = MAX (4, SCRTCH(256))
C                                       set values, read pars
      JBL = NBL*(NPOPS-1) + 2
      CALL ZFIO ('READ', LUNTD, FINDTD, JBL, SCRTCH, IER)
      IF (IER.NE.0) GO TO 980
C                                       Get task parms
      IF (NPARMS.GT.0) THEN
         J = 10
         CALL FILL (246, 0, SCRTCH(11))
         DO 50 I = 1,NPARMS
            J = J + 1
            IF (J.GT.256) THEN
               CALL ZFIO ('WRIT', LUNTD, FINDTD, JBL, SCRTCH, IER)
               IF (IER.NE.0) GO TO 980
               J = 1
               JBL = JBL + 1
               CALL FILL (256, 0, SCRTCH)
               END IF
            RSCR = RPARM(I)
            SCRTCH(J) = ISCR
 50         CONTINUE
         CALL ZFIO ('WRIT', LUNTD, FINDTD, JBL, SCRTCH, IER)
         IF (IER.NE.0) GO TO 980
         END IF
C                                       close file
 60   CALL ZCLOSE (LUNTD, FINDTD, IER)
C                                       message: start
      MSGTXT = 'returns adverbs to AIPS'
      CALL MSGWRT (2)
      GO TO 999
C                                       Errors
 980  IERR = 2
 990  CALL ZCLOSE (LUNTD, FINDTD, IER)
C
 999  RETURN
      END
