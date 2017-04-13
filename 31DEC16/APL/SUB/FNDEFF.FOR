      SUBROUTINE FNDEFF (NIF, FF, RXBAND, NANT, CALIN, BUFF, VEFF)
C-----------------------------------------------------------------------
C! find the efficiencies to apply with these data
C# Utility UV VLA Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2014
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
C   Finds efficiencies from table for EVLA
C   Inputs:
C      NIF      I      Number IFs
C      FF       D(*)   Frequencies of IFs GHz
C      RXBAND   C*(*)  Data are all in this receiver band:
C                         4 P L S C X U K A Q are the known ones
C      NANT     I      Number antennas
C      CALIN    C*48   text file of efficiencies
C   Outputs:
C      BUFF     R(*)   scratch buffer for table reading
C      VEFF     R(*)   Efficiencies (2,MAXIF,MAXANT)
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NIF, NANT
      DOUBLE PRECISION FF(*)
      CHARACTER RXBAND*(*), CALIN*(*)
      REAL      BUFF(3,MAXIF,*), VEFF(2,MAXIF,*)
C                                       default table number values
      INTEGER   NEFF
      PARAMETER (NEFF = 42)
C
      INTEGER   I, J, LUN, LUNTMP, MSGSAV, IANT, KBLIM, KBP, IA, IA1,
     *   IA2, IERR, FIND, JTRIM, LRX
      DOUBLE PRECISION X
      CHARACTER LINE*132
      REAL      TF(NEFF), TE(NEFF), D, W, VV, FREQ, RR, LL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C                                       default table frequencies
      DATA TF / 1.0,  1.1,  1.2,  1.3,  1.4,  1.5,  1.6,  1.7,  1.8,
     *    1.9,  2.0,  2.0,  2.3,  2.7,  3.0,  3.5,  3.7,  4.0,  4.0,
     *    5.0,  6.0,  7.0,  8.0,  8.0, 12.0, 12.0, 13.0, 14.0, 15.0,
     *   16.0, 17.0, 18.0, 19.0, 24.0, 26.0, 26.5, 28.0, 33.0, 38.0,
     *   40.0, 43.0, 48.0/
C                                       Rick Perley efficiencies
      DATA TE /0.45, 0.48, 0.48, 0.45, 0.46, 0.45, 0.43, 0.44, 0.44,
     *   0.49, 0.48, 0.52, 0.52, 0.51, 0.53, 0.55, 0.53, 0.54, 0.55,
     *   0.54, 0.56, 0.62, 0.64, 0.60, 0.60, 0.65, 0.65, 0.62, 0.58,
     *   0.59, 0.60, 0.60, 0.57, 0.52, 0.48, 0.50, 0.49, 0.42, 0.35,
     *   0.29, 0.28, 0.26/
C-----------------------------------------------------------------------
      LRX = JTRIM (RXBAND)
      LUN = LUNTMP (2)
      IERR = -2
      MSGSAV = MSGSUP
      MSGSUP = 32000
      IF (CALIN.NE.' ') THEN
         CALL ZTXOPN ('QRED', LUN, FIND, CALIN, .FALSE., IERR)
         IF (IERR.EQ.0) THEN
            CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
            IF (IERR.EQ.0) THEN
               IF (LINE(:18).NE.'; VLA.EFFICIENCIES') IERR = -1
               END IF
            CALL ZTCLOS (LUN, FIND, I)
            END IF
         END IF
      IF (IERR.NE.0) THEN
         CALIN = 'AIPSIONS:VLA.EFFICIENCIES'
         CALL ZTXOPN ('QRED', LUN, FIND, CALIN, .FALSE., IERR)
         IF (IERR.EQ.0) THEN
            CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
            IF (IERR.EQ.0) THEN
               IF (LINE(:18).NE.'; VLA.EFFICIENCIES') IERR = -1
               END IF
            CALL ZTCLOS (LUN, FIND, I)
            END IF
         END IF
C                                       text file present
      MSGSUP = MSGSAV
      IF (IERR.EQ.0) THEN
         I = 3 * MAXIF * NANT
         CALL RFILL (I, 0.0, BUFF)
         CALL ZTXOPN ('QRED', LUN, FIND, CALIN, .FALSE., IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'RE-OPENING EFFICIENCIES FILE'
            CALL MSGWRT (7)
            GO TO 200
            END IF
 10      CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
         IF (IERR.EQ.0) THEN
            KBLIM = JTRIM (LINE)
            IF ((KBLIM.GT.0) .AND. (LINE(:1).NE.';') .AND.
     *         (RXBAND.EQ.LINE(:LRX))) THEN
C                                       parse
               KBP = LRX + 1
               CALL GETNUM (LINE, KBLIM, KBP, X)
               IF (X.EQ.DBLANK) GO TO 10
               IANT = X + 0.1
               IF (IANT.GT.NANT) GO TO 10
               CALL GETNUM (LINE, KBLIM, KBP, X)
               IF (X.EQ.DBLANK) GO TO 10
               FREQ = X
               CALL GETNUM (LINE, KBLIM, KBP, X)
               IF (X.EQ.DBLANK) GO TO 10
               RR = X
               CALL GETNUM (LINE, KBLIM, KBP, X)
               IF (X.EQ.DBLANK) GO TO 10
               LL = X
               IF (IANT.LE.0) THEN
                  IA1 = 1
                  IA2 = NANT
               ELSE
                  IA1 = IANT
                  IA2 = IANT
                  END IF
               DO 40 IA = IA1,IA2
                  DO 30 I = 1,NIF
                     IF (FREQ.LE.FF(I)) THEN
                        BUFF(1,I,IA) = FREQ
                        BUFF(2,I,IA) = RR
                        BUFF(3,I,IA) = LL
                     ELSE IF (BUFF(1,I,IA).EQ.0.0) THEN
                        BUFF(1,I,IA) = FREQ
                        BUFF(2,I,IA) = RR
                        BUFF(3,I,IA) = LL
                     ELSE IF (BUFF(1,I,IA).GT.0.0) THEN
                        D = FREQ - BUFF(1,I,IA)
                        IF (D.LE.0.0) THEN
                           VEFF(1,I,IA) = (RR + BUFF(2,I,IA)) / 2.0
                           VEFF(2,I,IA) = (LL + BUFF(3,I,IA)) / 2.0
                        ELSE
                           W = (FREQ - FF(I)) / D
                           VEFF(1,I,IA) = W * BUFF(2,I,IA) +
     *                        (1.0-W) * RR
                           VEFF(2,I,IA) = W * BUFF(3,I,IA) +
     *                        (1.0-W) * LL
                           END IF
                        BUFF(1,I,IA) = -1.0
                        END IF
 30                  CONTINUE
 40               CONTINUE
               END IF
            GO TO 10
         ELSE
            CALL ZTCLOS (LUN, FIND, I)
            IF (IERR.NE.2) THEN
               WRITE (MSGTXT,1000) IERR, 'READING EFFICIENCIES FILE'
               CALL MSGWRT (7)
               GO TO 200
            ELSE
               MSGTXT = 'Using efficiencies table from text file'
               CALL MSGWRT (6)
               I = JTRIM (CALIN)
               MSGTXT = '   CALIN = ''' // CALIN(:I) // ''''
               CALL MSGWRT (6)
               IERR = 0
               DO 60 IA = 1,NANT
                  DO 50 I = 1,NIF
                     IF (BUFF(1,I,IA).GT.0.0) THEN
                        VEFF(1,I,IA) = BUFF(2,I,IA)
                        VEFF(2,I,IA) = BUFF(3,I,IA)
                        BUFF(1,I,IA) = -1.0
                        END IF
 50                  CONTINUE
 60               CONTINUE
               END IF
            END IF
         END IF
C                                       default table
 200  IF (IERR.NE.0) THEN
         MSGTXT = 'Using default efficiencies table'
         CALL MSGWRT (6)
         DO 240 I = 1,NIF
            IF (FF(I).LT.TF(1)) THEN
               VV = TE(1)
            ELSE IF (FF(I).GE.TF(NEFF)) THEN
               VV = TE(NEFF)
            ELSE
               DO 210 J = 1,NEFF-1
                  IF ((FF(I).GE.TF(J)) .AND. (FF(I).LT.TF(J+1))) THEN
                     D = TF(J+1) - TF(J)
                     IF (D.LE.0.0) THEN
                        VV = TE(J)
                     ELSE
                        W = (TF(J+1) - FF(I)) / D
                        VV = W * TE(J) + (1.0-W) * TE(J+1)
                        END IF
                     GO TO 220
                     END IF
 210              CONTINUE
               END IF
 220        DO 230 J = 1,NANT
               VEFF(1,I,J) = VV
               VEFF(2,I,J) = VV
 230           CONTINUE
 240        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FNDEFF: ERROR',I4,1X,A)
      END
