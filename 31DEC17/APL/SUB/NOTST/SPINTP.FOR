      SUBROUTINE SPINTP (NFREQ, VIS, WTS, NEWWTS, ALLFLG)
C-----------------------------------------------------------------------
C! Deal with flagged channels when frequency shifting
C# UV IO-appl Calibration Spectral
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
C  Routine to replace flagged channels with nearest neighbours
C
C  Inputs:
C    NFREQ          I         # channels in spectrum
C    VIS(2,*)       R         Input spectrum
C    WTS(*)         R         Input spectral weights
C
C  Outputs:
C    VIS(2,*)       R         Interpolated spectrum
C    NEWWTS(*)      R         Corrected weights
C    ALLFLG         L         If .TRUE. then whole spectrum flagged
C-----------------------------------------------------------------------
      REAL      VIS(2,*), WTS(*), NEWWTS(*), MXWT, WT1, WT2, AVGWTS,
     *   V1R, V2R, V1I, V2I, NAVG
      INTEGER   IFRQ, SCHAN, LCHAN, NEGWTS, POSWTS, I,
     *   NINTP, CHAN1, CHAN2, NFREQ, J
      LOGICAL   INTERP, ALLFLG, GOTNEG, BEGIN, END
      INCLUDE 'INCS:PUVD.INC'
C-----------------------------------------------------------------------
      CALL RCOPY (NFREQ, WTS, NEWWTS)
C                                       Determine if interpolation
C                                       necessary
      MXWT = -1.0E10
      INTERP = .FALSE.
      NEGWTS = 0
      ALLFLG = .FALSE.
      POSWTS = 0
      AVGWTS = 0.0
      BEGIN = .FALSE.
      END = .FALSE.
      DO 50 IFRQ = 1, NFREQ
         IF (WTS(IFRQ) .LE. 0.0) THEN
            INTERP = .TRUE.
            NEGWTS = NEGWTS + 1
            END IF
         IF (WTS(IFRQ) .GT. 0.0) THEN
            MXWT = MAX(MXWT,WTS(IFRQ))
            AVGWTS = AVGWTS + WTS(IFRQ)
            POSWTS = POSWTS + 1
            END IF
 50      CONTINUE
C                                       No interpolation
      IF (.NOT. INTERP) GO TO 999
C                                       Totally flagged, totally
C                                       defined as > 50%
      IF (NEGWTS .GT. (NFREQ/2)) THEN
         ALLFLG = .TRUE.
         GO TO 999
         END IF
C                                       Determine average positive
C                                       weight
      AVGWTS = AVGWTS / POSWTS
C                                       Select range to interpolate
C                                       over - this could be done
C                                       multiple times
      SCHAN = 0
      LCHAN = 0
      CHAN1 = 1
      CHAN2 = NFREQ
      GOTNEG = .FALSE.
 100  DO 200 I = CHAN1, CHAN2
         IF ((WTS(I).LE.0.0) .AND. (I.GT.LCHAN) .AND.
     *      (.NOT.GOTNEG)) THEN
            SCHAN = I - 1
            GOTNEG = .TRUE.
            END IF
         IF ((WTS(I).GT.0.0) .AND. (I.GT.SCHAN) .AND. GOTNEG) THEN
            LCHAN = I
            GO TO 250
            END IF
 200     CONTINUE
      IF (GOTNEG .AND. (LCHAN.EQ.0)) THEN
         LCHAN = NFREQ
         GO TO 250
         END IF
      GO TO 999
C                                       # channels to interpolate
 250  NINTP = LCHAN - SCHAN
C                                       2 special cases, beginning and
C                                       end of spectrum.
      BEGIN = SCHAN .EQ. 0
      END   = LCHAN .EQ. NFREQ
      IF (BEGIN .AND. END) THEN
         ALLFLG = .TRUE.
         GO TO 999
         END IF
C
      CHAN1 = LCHAN
      CHAN2 = NFREQ
      GOTNEG = .FALSE.
C                                       Interpolate
      IF (SCHAN .GT. 0) THEN
         V1R = VIS(1,SCHAN)
         V1I = VIS(2,SCHAN)
      ELSE
         V1R = 0.0
         V1I = 0.0
      END IF
      V2R = VIS(1,LCHAN)
      V2I = VIS(2,LCHAN)
      IF (BEGIN) THEN
C                                       Average first 5 good channels
C                                       to provide new values at beginning
         J = LCHAN + 4
         V2R = 0.0
         V2I = 0.0
         NAVG = 0.0
         DO 270 I = LCHAN, J
            IF (WTS(I).GT.0.0) THEN
               V2R = V2R + VIS(1,I)
               V2I = V2I + VIS(2,I)
               NAVG = NAVG + 1.0
               END IF
  270       CONTINUE
         V2R = V2R / NAVG
         V2I = V2I / NAVG
         END IF
C
      IF (END) THEN
C                                       Average last 5 good channels
C                                       to provide new values at end
         J = SCHAN - 4
         V1R = 0.0
         V1I = 0.0
         NAVG = 0.0
         DO 280 I = J, SCHAN
            IF (WTS(I).GT.0.0) THEN
               V1R = V1R + VIS(1,I)
               V1I = V1I + VIS(2,I)
               NAVG = NAVG + 1.0
               END IF
  280       CONTINUE
         V1R = V1R / NAVG
         V1I = V1I / NAVG
         END IF
C
      DO 300 I = 1, NINTP
         WT1 = REAL(LCHAN - (SCHAN + I)) / REAL(LCHAN - SCHAN)
         WT2 = 1.0 - WT1
         IF ((SCHAN .EQ. 0) .OR. (BEGIN)) THEN
            WT1 = 0.0
            WT2 = 1.0
            END IF
         IF (END) THEN
            WT1 = 1.0
            WT2 = 0.0
            END IF
         VIS(1,I+SCHAN) = WT1*V1R + WT2*V2R
         VIS(2,I+SCHAN) = WT1*V1I + WT2*V2I
         NEWWTS(I+SCHAN)   = AVGWTS
 300     CONTINUE
C                                       Loop back ?
      IF (LCHAN .LT. NFREQ) THEN
         LCHAN = 0
         GO TO 100
         END IF
C                                       Finish
 999  RETURN
      END
