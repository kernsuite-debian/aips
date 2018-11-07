      SUBROUTINE YCNECT (X1, Y1, X2, Y2, IC, BUFFER, IERR)
C-----------------------------------------------------------------------
C! write line segment between 2 points on TV
C# TV-util
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
C   YCNECT writes a line segment on the TV.  This version will work on
C   all TVs.  It is called a Y routine to allow the use of hardware
C   vector generators on those TVs equiped with them.
C   Inputs:
C      X1      I      start X position
C      Y1      I      start Y position
C      X2      I      end X position
C      Y2      I      end Y position
C      IC      I      Channel (1 to NGRAY+NGRAPH)
C      BUFFER  I(*)   Buffer contains desired intensity (size > max
C                     horizontal or vertical line, e.g., 1280)
C   Output:
C      IERR    I      error code : 0 => ok
C   Special IVAS version: uses Hitachi graphics processor to generate
C   the lines on graphics planes.  Generic routine for gray planes.
C-----------------------------------------------------------------------
      INTEGER   X1, Y1, X2, Y2, IC, BUFFER(*), IERR
C
      REAL      POS, POSINC
      LOGICAL   T, F
      INTEGER   SS, ES, SL, EL, DELTAX, DELTAY, STEP, IANGL, LINE,
     *   LENGTH, SAMPLE, IX, IY, IROUND, IBITS, X0, Y0, MODE, JERR
      INTEGER   FIVASGPHMOVE, FIVASGPHVALUE, FIVASGPHLINE
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Generic for grey-scale planes
      IF (IC.GT.NGRAY) GO TO 100
      IERR = 2
      IF ((TVXMOD.LE.0) .AND. (TVYMOD.LE.0)) GO TO 999
      SS = X1
      ES = X2
      SL = Y1
      EL = Y2
      STEP = 1
      DELTAX = ABS(ES-SS) + 1
      DELTAY = ABS(EL-SL) + 1
C                                        Horizontal lines
      IF (TVXMOD.LE.0) GO TO 50
      IF ((DELTAY.GT.DELTAX) .AND. (TVYMOD.GT.0)) GO TO 50
C                                        check order
         IF (SS.GT.ES) THEN
            SS = X2
            ES = X1
            SL = Y2
            EL = Y1
            END IF
         IF (SL.GT.EL) STEP = -1
         POSINC = REAL(DELTAX) / REAL(DELTAY)
         IF (SS.EQ.ES) POSINC = 0.0
         POS = REAL(SS)
         IANGL = 0
         IF (TVXMOD.EQ.2) IANGL = 2
C                                        manual DO loop
         LINE = SL
 20      CONTINUE
            IX = IROUND (POS)
            POS = POS + POSINC
            LENGTH = IROUND (POS) - IX
            LENGTH = MAX (1, LENGTH)
            IF (TVXMOD.EQ.2) IX = IX + LENGTH - 1
            CALL YIMGIO ('WRIT', IC, IX, LINE, IANGL, LENGTH, BUFFER,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            LINE = LINE + STEP
            IF (((STEP.LT.0) .AND. (LINE.GE.EL)) .OR. ((STEP.GT.0) .AND.
     *         (LINE.LE.EL))) GO TO 20
         GO TO 999
C                                        Vertical lines
 50   CONTINUE
         IF (SL.LE.EL) GO TO 60
            SS = X2
            ES = X1
            SL = Y2
            EL = Y1
 60      IF (SS.GT.ES) STEP = -1
         POSINC = REAL(DELTAY) / REAL(DELTAX)
         IF (SL.EQ.EL) POSINC = 0.0
         POS = REAL(SL)
         IANGL = 1
         IF (TVYMOD.EQ.2) IANGL = 3
C                                        manual DO loop
         SAMPLE = SS
 70      CONTINUE
            IY = IROUND (POS)
            POS = POS + POSINC
            LENGTH = IROUND (POS) - IY
            LENGTH = MAX (1, LENGTH)
            IF (TVYMOD.EQ.2) IY = IY + LENGTH - 1
            CALL YIMGIO ('WRIT', IC, SAMPLE, IY, IANGL, LENGTH, BUFFER,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            SAMPLE = SAMPLE + STEP
            IF (((SAMPLE.GE.ES) .AND. (STEP.LT.0)) .OR. ((SAMPLE.LE.ES)
     *         .AND. (STEP.GT.0))) GO TO 70
         GO TO 999
C                                       Graphics plans
 100  CALL YVRTR (F, IERR)
      IF (IERR.NE.0) GO TO 999
      IBITS = 2 ** (IC - NGRAY - 1)
      IF (BUFFER(1).EQ.0) IBITS = 15 - IBITS
C                                       Graphics uses 0,0 as BLC
      X0 = X1 - 1
      Y0 = Y1 - 1
      MODE = 0
      JERR = FIVASGPHMOVE (X0, Y0, MODE)
      IF (JERR.NE.0) GO TO 900
      JERR = FIVASGPHVALUE (IBITS, IBITS)
      IF (JERR.NE.0) GO TO 900
      X0 = X2 - 1
      Y0 = Y2 - 1
      MODE = 1
      IF (BUFFER(1).EQ.0) MODE = 2
      JERR = FIVASGPHLINE (X0, Y0, MODE)
      IF (JERR.EQ.0) GO TO 999
C                                       errors
 900  CALL YDOERR ('YCNECT', JERR, IERR)
C
 999  RETURN
      END
