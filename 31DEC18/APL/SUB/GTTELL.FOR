      SUBROUTINE GTTELL (NPARMS, OPTELL, PARMS, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! gets any parameters sent to task by AIPS verb TELL
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   GTTELL gets any parameters sent to the current task by AIPS verb
C   TELL.  All entries for the task in the TC file are cleared and the
C   most recent is returned to the calling routine.
C   Inputs:
C      NPARMS   I         Number REAL parameters
C                         < 0 => clean out all pending TELLs for this
C                                task.  Return nothing.
C   Output:
C      OPTELL   C*4       Opcode from TELL
C      PARMS    R(*)      Returned parameters
C      SCRTCH   I(256)    Scratch buffer
C      IERR     I         0 => okay (no parms)
C                         1 => okay (got parms)
C                         2 => TELL orders quit
C                         3 => TELL orders abort
C   Note: if GTTELL encounters an internal error, i.e. file open, read,
C   etc. failure, it returns IERR = 0 after emitting a message.
C-----------------------------------------------------------------------
      INTEGER   NPARMS, SCRTCH(256), IERR
      CHARACTER OPTELL*4
      REAL      PARMS(*)
C
      INTEGER   JERR, NX, IXL, IX, I, LTI(2), LUNTC, FINDTC, IREC
      LOGICAL   T, F, GOTONE(16)
      CHARACTER PHNAME*48, TNAME*6
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNTC /13/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open TC file
      CALL ZPHFIL ('TC', 1, 0, 1, PHNAME, JERR)
      CALL ZOPEN (LUNTC, FINDTC, 1, PHNAME, F, T, T, JERR)
      IF (JERR.EQ.0) GO TO 20
         WRITE (MSGTXT,1000) JERR
         CALL MSGWRT (6)
         GO TO 999
C                                       Read directory record
 20   IREC = (NPOPS - 1) / 2 + 1
      CALL ZFIO ('READ', LUNTC, FINDTC, IREC, SCRTCH, JERR)
      IF (JERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) JERR
         CALL MSGWRT (6)
         GO TO 900
C                                       Are we marked there?
 30   NX = 0
      IXL = (NPOPS + 1 - 2*IREC) * 128 + 1
      IX = IXL - 8
      DO 35 I = 1,16
         IX = IX + 8
         CALL H2CHR (6, 1, SCRTCH(IX), TNAME)
         GOTONE(I) = (TSKNAM.EQ.TNAME) .AND. ((SCRTCH(IX+4).EQ.NLUSER)
     *      .OR. (NPARMS.LT.0))
         IF (GOTONE(I)) NX = NX + 1
 35      CONTINUE
      IF (NX.EQ.0) GO TO 900
      LTI(1) = 0
      LTI(2) = 0
      IX = IXL - 6
      NX = 0
      DO 45 I = 1,16
         IX = IX + 8
         IF (GOTONE(I)) THEN
            IF (SCRTCH(IX).LT.LTI(1)) GO TO 45
            IF ((SCRTCH(IX).GT.LTI(1)) .OR. (SCRTCH(IX+1).GE.LTI(2)))
     *         THEN
               LTI(1) = SCRTCH(IX)
               LTI(2) = SCRTCH(IX+1)
               NX = I
               END IF
            END IF
 45      CONTINUE
      IF (NX.GT.0) GO TO 50
         WRITE (MSGTXT,1045)
         CALL MSGWRT (6)
         GO TO 900
C                                       Clear entries
 50   IX = IXL - 8
      DO 55 I = 1,16
         IX = IX + 8
         IF (GOTONE(I)) CALL FILL (8, 0, SCRTCH(IX))
 55      CONTINUE
      CALL ZFIO ('WRIT', LUNTC, FINDTC, IREC, SCRTCH, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1055) JERR
         CALL MSGWRT (6)
         GO TO 900
         END IF
C                                       Read data
      IF (NPARMS.GE.0) THEN
         IREC = 18 + (NPOPS-1) * 16 + NX
         CALL ZFIO ('READ', LUNTC, FINDTC, IREC, SCRTCH, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1020) JERR
            CALL MSGWRT (6)
            GO TO 900
            END IF
 70      CALL H2CHR (4, 1, SCRTCH(1), OPTELL)
         I = NPARMS
         IF (I.GT.0) CALL COPY (I, SCRTCH(2), PARMS)
         IERR = 1
         IF (OPTELL.EQ.'QUIT') IERR = 2
         IF (OPTELL.EQ.'ABOR') IERR = 3
         END IF
C                                       Close file
 900  CALL ZCLOSE (LUNTC, FINDTC, JERR)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GTTELL: ERROR',I7,' OPENING TELL COMMUNICATION FILE')
 1020 FORMAT ('GTTELL: ERROR',I7,' READING TELL COMMUNICATION FILE')
 1045 FORMAT ('GTTELL: LOGIC ERROR WITH TIMES')
 1055 FORMAT ('GTTELL: ERROR',I7,' WRITING TELL COMMUNICATION FILE')
      END
