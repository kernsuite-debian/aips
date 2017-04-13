      SUBROUTINE TOUCH
C-----------------------------------------------------------------------
C! updates times in the catalog file for all scratch files
C# Catalog Header
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
C   updates the times in the catalog file for all scratch files in the
C   DFIL common.
C   Inputs via common DFIL.INC:
C      NSCR     I        Number of scratch files
C      SCRVOL   I(128)   Scratch file disks
C      SCRCNO   I(128)   Scratch file catalog numbers
C-----------------------------------------------------------------------
      INTEGER   IVOL, CNO, BUFF(256), IERR, NREC, CMAX, CMOD, NIND,
     *   CIND, CLUN, NWPL, ISCR, NLPR, NTIME, IT(6), MSL
      LOGICAL   WASOPN
      DOUBLE PRECISION JD, JDLAST
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      SAVE JDLAST
      DATA NTIME, CLUN /2, 15/
      DATA JDLAST /0.0D0/
C-----------------------------------------------------------------------
      CALL ZDATE (IT(1))
      CALL ZTIME (IT(4))
      CALL DAT2JD (IT, JD)
      IF (JD-JDLAST.LT.10.0/60.0/24.0) GO TO 999
      JDLAST = JD
      MSL = MSGSUP
      MSGSUP = 32000
      DO 100 IVOL = 1,NVOL
         WASOPN = .FALSE.
         DO 90 ISCR = 1,NSCR
            IF (SCRVOL(ISCR).EQ.IVOL) THEN
C                                       open catalog file
               IF (.NOT.WASOPN) THEN
                  CALL CATOPN (IVOL, CIND, BUFF, CMAX, IERR)
                  IF (IERR.NE.0) GO TO 100
                  WASOPN = .TRUE.
                  END IF
C                                       check validity of CNO
               CNO = SCRCNO(ISCR)
               IF ((CNO.LT.0) .OR. (CNO.GT.CMAX)) GO TO 90
C                                       get directory block
               NWPL = 10
               NLPR = 256 / NWPL
               CMOD = (CNO - 1) / NLPR
               NREC = 2 + CMOD
               NIND = 1 + NWPL * (CNO - NLPR*CMOD - 1)
               CALL ZFIO ('READ', CLUN, CIND, NREC, BUFF, IERR)
               IF (IERR.NE.0) GO TO 95
C                                       return directory to disk
C                                       with latest time
               CALL CATIME (1, BUFF(NIND+NTIME), IT)
               CALL ZFIO ('WRIT', CLUN, CIND, NREC, BUFF, IERR)
               IF (IERR.NE.0) GO TO 95
               END IF
 90         CONTINUE
C                                       close catalog
 95      IF (WASOPN) CALL ZCLOSE (CLUN, CIND, IERR)
 100     CONTINUE
      MSGSUP = MSL
C
 999  RETURN
      END
