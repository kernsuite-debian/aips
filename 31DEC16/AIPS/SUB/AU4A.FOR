      SUBROUTINE AU4A (BRANCH)
C-----------------------------------------------------------------------
C! verbs for listing disk contents (also AU3)
C# POPS-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2012-2013
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
C   AU3A contains the disk management verbs ALLDEST (verb to destroy
C   all or most of a users data files) and SCRDEST (destroy all
C   scratch files created by a specific task).  SAVDEST destroy user's
C   SAVE/GET files.
C   Inputs:
C      BRANCH I   1 = M2CAT
C                 2 = M3CAT
C                 3 = M4CAT
C                 4 = MOCAT
C                 5 = U2CAT
C                 6 = U3CAT
C                 7 = U4CAT
C                 8 = UOCAT
C                 9 = M5CAT
C                19 = U5CAT
C   Common:  /CAPL/
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER LOCNAM*12, LOCCLS*6, LOCTYP*2, PRGNAM*6, CDUM*1
      REAL      RDUM
      INTEGER   IBLK(256), ISCR(256), I, QUICK, IVOL, IUSER, POTERR,
     *   IDUM, LOCSEQ, IERR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DERR.INC'
      COMMON /AIPSCR/ IBLK, ISCR
      DATA PRGNAM /'AU4A  '/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.10)) GO TO 999
C-----------------------------------------------------------------------
C                                       M2CAT, U2CAT
C                                       quick listing maps, UV disk 2
C-----------------------------------------------------------------------
C                                       Determine disk to use
 100  IF ((BRANCH.EQ.1) .OR. (BRANCH.EQ.5)) THEN
         CALL ADVERB ('IN2DISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      ELSE IF ((BRANCH.EQ.2) .OR. (BRANCH.EQ.6)) THEN
         CALL ADVERB ('IN3DISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      ELSE IF ((BRANCH.EQ.3) .OR. (BRANCH.EQ.7)) THEN
         CALL ADVERB ('IN4DISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      ELSE IF ((BRANCH.EQ.4) .OR. (BRANCH.EQ.8)) THEN
         CALL ADVERB ('OUTDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      ELSE IF ((BRANCH.EQ.9) .OR. (BRANCH.EQ.10)) THEN
         CALL ADVERB ('IN5DISK', 'I', 1, 0, IVOL, RDUM, CDUM)
         END IF
      IF (ERRNUM.NE.0) GO TO 980
      IUSER = NLUSER
      QUICK = 1
      CALL ADVERB ('DOALPHA', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF (RDUM.GT.0.0) QUICK = 2 * QUICK
      I = 0
      IF ((BRANCH.LE.4) .OR. (BRANCH.EQ.9)) THEN
         LOCTYP = 'MA'
      ELSE
         LOCTYP = 'UV'
         END IF
      LOCNAM = ' '
      LOCCLS = ' '
      LOCSEQ = 0
      CALL CATLST (IVOL, LOCNAM, LOCCLS, LOCSEQ, LOCTYP, IUSER, QUICK,
     *   I, IBLK, ISCR, IERR)
      GO TO 999
C-----------------------------------------------------------------------
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.GT.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
      END
