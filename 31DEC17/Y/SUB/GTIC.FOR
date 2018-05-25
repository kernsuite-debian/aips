      SUBROUTINE GTIC (ILABEL, BLC, TRC, NTRC, XTRC, PIXR, IBUFF, IERR)
C-----------------------------------------------------------------------
C! Puts tick marks on external step wedges for grey-scale plotting
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2002, 2014-2015
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
C   plots tick marks and labels on step wedges for GREYS
C   Inputs:
C      BLC     R(2)     Bottom left corner - image
C      TRC     R(2)     Top right corner - image
C      NTRC    R(2)     Top right corner - full field
C      XTRC    R(2)     Top right corner - ifield less wedge
C      PIXR    R(2)     Intensity range in wedge
C   In/out:
C      IBUFF   I(256)   Plot buffer
C   Output:
C      IERR    I        Error code: 2 => IO error in plotting
C-----------------------------------------------------------------------
      INTEGER   ILABEL, IBUFF(256), IERR
      REAL      BLC(2), TRC(2), NTRC(2), XTRC(2), PIXR(2)
C
      INTEGER   IAXIS, I, IX, IXL, ITRY, ILEN, ILMAX, FRMT, IFRMT,
     *   LLABEL
      DOUBLE PRECISION PMIN, PMAX, DEG, DEGC, XINTER(24), DEGU, DEGL
      LOGICAL   NONUM
      CHARACTER PREFIX*5, SPRTXT*20
      REAL      PR, DX, DY, X, Y
      INCLUDE 'INCS:DMSG.INC'
      DATA XINTER /0.001D0,0.002D0,0.005D0,0.01D0,0.02D0,0.05D0,0.1D0,
     *   0.2D0,0.5D0,1.D0,2.D0,5.D0,10.D0,20.D0,50.D0,100.D0,200.D0,
     *   500.D0,1000.D0,2000.D0,5000.D0,10000.D0,20000.D0,50000.D0/
C-----------------------------------------------------------------------
C                                       Corners give which axis has it
      IERR = 0
      LLABEL = MOD (ABS(ILABEL), 100)
      IF (LLABEL.LE.1) GO TO 999
      IAXIS = 0
      IF (XTRC(2).NE.NTRC(2)) IAXIS = 1
      IF (XTRC(1).NE.NTRC(1)) IAXIS = 2
      IF ((IAXIS.NE.1) .AND. (IAXIS.NE.2)) GO TO 999
C                                       get intensity range
      PMAX = PIXR(2)
      PMIN = PIXR(1)
      PR = PIXR(2) - PIXR(1)
      CALL METSCL (ILABEL, PR, PREFIX, NONUM)
C
      IF (.NOT.NONUM) THEN
         PMIN = PMIN * PR / (PIXR(2) - PIXR(1))
         PMAX = PMAX * PR / (PIXR(2) - PIXR(1))
         IX = 8
         IF (IAXIS.EQ.1) IX = 5
         IXL = 2
 10      DO 20 ITRY = 1,24
            DEG = XINTER(ITRY)
            DEGC = INT (PMAX / DEG) * DEG
            IF (DEGC.GT.PMAX) DEGC = DEGC - DEG
            I = (DEGC - PMIN) / DEG + 1
            IF ((I.GT.IXL) .AND. (I.LE.IX)) GO TO 30
 20         CONTINUE
         IF (IXL.EQ.0) THEN
            NONUM = .TRUE.
         ELSE
            IXL = 0
            IX = IX + 3
            GO TO 10
            END IF
         END IF
C                                       fails
 30   IF (NONUM) THEN
         PMIN = 0.0001D0
         PMAX = 0.9999D0
         DEG = 0.1D0
         WRITE (MSGTXT,1020)
         CALL MSGWRT (6)
         END IF
C                                       plot range
      DEGC = INT (PMAX / DEG) * DEG
      IF (DEGC.GT.PMAX) DEGC = DEGC - DEG
      DEGU = DEGC
      DEGC = INT (PMIN / DEG) * DEG
      IF (DEGC.LT.PMIN) DEGC = DEGC + DEG
      DEGL = DEGC
      IXL = (DEGU - DEGL) / DEG + 1.001
C                                       Horizontal
      DEGC = DEGL - DEG
      IF (IAXIS.EQ.1) THEN
         DO 50 I = 1,IXL
            DEGC = DEGC + DEG
            X = (DEGC - PMIN) / (PMAX - PMIN) * (TRC(1) - BLC(1)) +
     *         BLC(1)
            Y = (XTRC(2) + NTRC(2)) / 2.0
            CALL GPOS (X, Y, IBUFF, IERR)
            IF (IERR.NE.0) GO TO 980
            Y = NTRC(2)
            CALL GVEC (X, Y, IBUFF, IERR)
            IF (IERR.NE.0) GO TO 980
C                                       label
            IF ((.NOT.NONUM) .AND. (LLABEL.GE.3)) THEN
               PR = DEGC
               IF (ITRY.GT.9) IFRMT = FRMT (13, 3, PR, SPRTXT)
               IF (ITRY.LT.4) WRITE (SPRTXT,1030) DEGC
               IF ((ITRY.GE.4) .AND. (ITRY.LE.6)) WRITE (SPRTXT,1031)
     *            DEGC
               IF ((ITRY.GE.7) .AND. (ITRY.LE.9)) WRITE (SPRTXT,1032)
     *            DEGC
C                                       Trim blanks from RA/DEC
               CALL CHTRIM (SPRTXT, 13, SPRTXT, ILEN)
               DX = 0.5 - ILEN
               DY = 0.5
               CALL GCHAR (ILEN, 0, DX, DY, SPRTXT, IBUFF, IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
 50         CONTINUE
C                                       Vertical
      ELSE
C                                       max length of label
         IF ((.NOT.NONUM) .AND. (LLABEL.GE.3)) THEN
            PR = DEGU
            IF (ITRY.GT.9) IFRMT = FRMT (13, 3, PR, SPRTXT)
            IF (ITRY.LT.4) WRITE (SPRTXT,1030) DEGU
            IF ((ITRY.GE.4) .AND. (ITRY.LE.6)) WRITE (SPRTXT,1031) DEGU
            IF ((ITRY.GE.7) .AND. (ITRY.LE.9)) WRITE (SPRTXT,1032) DEGU
            CALL CHTRIM (SPRTXT, 13, SPRTXT, ILEN)
            PR = DEGL
            IF (ITRY.GT.9) IFRMT = FRMT (13, 3, PR, SPRTXT)
            IF (ITRY.LT.4) WRITE (SPRTXT,1030) DEGL
            IF ((ITRY.GE.4) .AND. (ITRY.LE.6)) WRITE (SPRTXT,1031) DEGL
            IF ((ITRY.GE.7) .AND. (ITRY.LE.9)) WRITE (SPRTXT,1032) DEGL
            CALL CHTRIM (SPRTXT, 13, SPRTXT, ILMAX)
            ILMAX = MAX (ILMAX, ILEN)
            END IF
         DO 90 I = 1,IXL
            DEGC = DEGC + DEG
            Y = (DEGC - PMIN) / (PMAX - PMIN) * (TRC(2) - BLC(2)) +
     *         BLC(2)
            X = (XTRC(1) + NTRC(1)) / 2.0
            CALL GPOS (X, Y, IBUFF, IERR)
            IF (IERR.NE.0) GO TO 980
            X = NTRC(1)
            CALL GVEC (X, Y, IBUFF, IERR)
            IF (IERR.NE.0) GO TO 980
C                                       label
            IF ((.NOT.NONUM) .AND. (LLABEL.GE.3)) THEN
               PR = DEGC
               IF (ITRY.GT.9) IFRMT = FRMT (13, 3, PR, SPRTXT)
               IF (ITRY.LT.4) WRITE (SPRTXT,1030) DEGC
               IF ((ITRY.GE.4) .AND. (ITRY.LE.6)) WRITE (SPRTXT,1031)
     *            DEGC
               IF ((ITRY.GE.7) .AND. (ITRY.LE.9)) WRITE (SPRTXT,1032)
     *            DEGC
C                                       Trim blanks from RA/DEC
               CALL CHTRIM (SPRTXT, 13, SPRTXT, ILEN)
               DX = 0.5 + ILMAX - ILEN
               DY = -0.5
               CALL GCHAR (ILEN, 0, DX, DY, SPRTXT, IBUFF, IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
 90         CONTINUE
         END IF
      GO TO 999
C                                       error
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (7)
      IERR = 2
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('TICK ALGORITHM FAILS ON STEP WEDGE - continuing')
 1030 FORMAT (F12.3)
 1031 FORMAT (F12.2)
 1032 FORMAT (F12.1)
 1980 FORMAT ('GTIC: STEP WEDGE LABEL/TICK WRITING ERROR =',I5)
      END
