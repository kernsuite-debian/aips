      SUBROUTINE AU9C (BRANCH)
C-----------------------------------------------------------------------
C! verbs to set initial guesses for slice model fits using TEK graphics
C# POPS-appl Graphics Slice
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000
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
C   AU9C is a package for interactive determination of initial guesses
C   for 1D and 2D (someday soon) Gaussian fitting.
C   Inputs:
C      BRANCH   I   1 = TKSET set guess all components.
C                   2 = TKSET1 set guess one component.
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER PRGNAM*6, CDUM*1
      INTEGER   INGAUS, INODIM, ISTART, IEND, IERR, IDUM, I, J
      REAL      RDUM, GP(2,4), GW(3,4), GM(4)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTKS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGNAM /'AU9C  '/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.2)) GO TO 999
      ERRNUM = 37
      IF (NTKDEV.LE.0) GO TO 980
      ERRNUM = 0
C                                       Check inputs
      CALL ADVERB ('NGAUSS', 'I', 1, 0, INGAUS, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF (INGAUS.LT.1) INGAUS = 1
      IF (INGAUS.GT.4) INGAUS = 4
      CALL ADVERB ('GPOS', 'R', 8, 0, IDUM, GP, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('GWIDTH', 'R', 12, 0, IDUM, GW, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('GMAX', 'R', 4, 0, IDUM, GM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
C                                       Open Tektonix device
      CALL ZTKOPN (IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TEK OPEN ERROR'
         CALL MSGWRT (6)
         GO TO 975
         END IF
C                                       Set location common.
      CALL SLOCIN (INODIM)
C
      CALL ZTKCLS (IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TEK CLOSE ERROR'
         CALL MSGWRT (6)
         GO TO 975
         END IF
C
      GO TO (100, 200), BRANCH
C-----------------------------------------------------------------------
C                                       TKSET (set all components)
C-----------------------------------------------------------------------
 100  CONTINUE
         ISTART = 1
         IEND = INGAUS
         CALL RFILL (8, 0.0, GP)
         CALL RFILL (12, 0.0, GW)
         CALL RFILL (4, 0.0, GM)
         GO TO 220
C-----------------------------------------------------------------------
C                                       TKSET1 (set 1 component)
C-----------------------------------------------------------------------
 200  CONTINUE
         IF ((SP.LT.1) .OR. (STACK(SP).EQ.2)) THEN
            ERRNUM = 8
            GO TO 980
            END IF
         ISTART = V(SP) + .01
         IF ((ISTART.GT.INGAUS) .OR. (ISTART.LT.1)) GO TO 970
         IEND = ISTART
         SP = SP - 1
C                                       Set initial center, halfwidth,
C                                       amp, using tektronix cursor.
 220     IF (INODIM.EQ.1) THEN
            DO 230 I = ISTART,IEND
               DO 229 J = 1,4
                  CALL ZTKOPN (IERR)
                  IF (IERR.NE.0) THEN
                     MSGTXT = 'TEK OPEN ERROR'
                     CALL MSGWRT (6)
                     GO TO 975
                     END IF
                  CALL SET1DG (I, I, GP, GW, GM, IERR)
                  CALL ZTKCLS (IDUM)
                  IF (IERR.EQ.0) GO TO 230
                  MSGTXT = 'Let''s try again'
                  IF (J.LT.4) CALL MSGWRT (2)
 229              CONTINUE
               IF (I.EQ.ISTART) GO TO 975
               GO TO 240
 230           CONTINUE
 240        CALL ADVRBS ('GPOS', 'R', 8, 0, IDUM, GP, CDUM)
            IF (ERRNUM.NE.0) GO TO 980
            CALL ADVRBS ('GWIDTH', 'R', 12, 0, IDUM, GW, CDUM)
            IF (ERRNUM.NE.0) GO TO 980
            CALL ADVRBS ('GMAX', 'R', 4, 0, IDUM, GM, CDUM)
            END IF
         GO TO 980
C-----------------------------------------------------------------------
C                                       Error handling.
 970  WRITE (MSGTXT,1970) ISTART, INGAUS
      CALL MSGWRT (6)
C
 975  ERRNUM = 101
 980  IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1970 FORMAT ('COMPONENT',I7,' OUT OF RANGE 1 -',I4)
      END
