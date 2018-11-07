      SUBROUTINE XTRFIT (NPAR, NSTDS, XY, AD, NAMES, DOPRT, REFPIX,
     *   REFVAL, REFINC, ANGLE, A, B, RES, IERR)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999-2000, 2015
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
C   Finds the xy to RA,Dec transformation using a user supplied list
C   of standard star positions.  The function that is fit is a linear
C   function (NPAR=3) or a linear plus quadratic (NPAR=6) in the tangent
C   plane.  Axis increments, rotation angle, and axis offets are found.
C   If the user requests the transformation to be applied, these values
C   are put in the image header.
C   Input:
C      NPAR     I             Number of paramters to fit (if not 6 then
C                             3)
C      NSTDS    I             Number of stars
C      XY       R(2,NSTDS)    Pixel containing the centroid of the star
C      AD       R(2,NSTDS)    Angle containing the centroid of the star
C                             (degrees)
C      NAMES    C(20,NSTDS)   Names of stars
C      DOPRT    L             If true print fit and residuals
C      REFPIX   D(2)          Referenc Pixel Location
C   Input/Output:
C      REFVAL   D(2)          Angle of center of image (degrees)
C   Output:
C      REFINC   D(2)          Pixel Increment of image (degrees)
C      ANGLE    D             Orientation angle of image (degrees)
C      A        D(6)          Model parameters for RA
C      B        D(6)          Model parameters for Declination
C      RES      R(2,NSTDS)    Residuals from fit (degrees)
C      IERR     I             Error return code
C-----------------------------------------------------------------------
      INTEGER   NPAR, NSTDS, IERR
      CHARACTER NAMES(*)*20
      LOGICAL   DOPRT
      REAL      XY(2,*), AD(2,*), RES(2,*)
      DOUBLE PRECISION REFVAL(2), REFINC(2), REFPIX(2), ANGLE
C
      INTEGER   MXSTFT
      PARAMETER (MXSTFT=1000)
      DOUBLE PRECISION RADIAN, PI, DEGRES
      PARAMETER (RADIAN=5.729577951D1, PI=3.1415926536,
     *   DEGRES=0.017453292519943295769)
C
      INTEGER   NP, LDFJAC, LWA, NST, INF, IPVT(6), I, K, NTOT, ILSQ,
     *   NLSQ, NPRT
      REAL   R1, R2, R3, RTEMP1, RTEMP2, ANGL1, ANGL2, X00(1000,6)
      DOUBLE PRECISION TOL, FVEC(MXSTFT), FJAC(6,6),
     *   ACEN, DCEN, AX, AY, DIFF(MXSTFT), SDCEN, CDCEN,
     *   DENOM(MXSTFT), DIRCOS(3,3), A(6), B(6), WA(2, MXSTFT)
      LOGICAL   WT
      EXTERNAL XTRFCN
      COMMON /XYAD/ X00
      SAVE     NPRT
      INCLUDE 'INCS:DMSG.INC'
      DATA     NPRT/0/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Do not weight stars in fit
      WT = .FALSE.
C                                       Then do a 3 parameter fit
      IF (NPAR.NE.6.) THEN
         NP = 3
         NLSQ = 1
C                                       6 parameter fit
      ELSE
         NP = 6
         NLSQ = 2
         END IF
      TOL = 1.D-10
C                                       Set Weight array size
      LWA = 2 * MXSTFT
      LDFJAC = NP
C                                       Can only fit MXSTFT stars
      NST = MIN (MXSTFT, NSTDS)
C                                       If enought stars
      IF (NP.GT.NST) THEN
         MSGTXT = 'XTRFIT: TOO FEW STARS FOR FIT'
         IERR = 3
         GO TO 960
         END IF
C                                       User input field center (degrees)
      ACEN = REFVAL(1) * DEGRES
C                                       Convert Center angle to radians
      DCEN = REFVAL(2) * DEGRES
C                                       Set up initialize guesses
      DO 210 I = 1,NST
         DO 210 K = 1,6
            X00(I,K) = 0.
 210     CONTINUE
      DO 215 I = 1,6
         A(I) = 0.
         B(I) = 0.
 215     CONTINUE
      A(3) = 1.
      B(2) = 1.
C                              Find the transformation constants:
C                                ILSQ=1: First pass through LSQ; uses
C                                estimated values from ACEN and DCEN.
C                                ILSQ=2: Second pass through LSQ;uses
C                                ACEN=A(1) and DCEN=B(1)
      ILSQ = 1
 220  CONTINUE
C                                      Set a couple constants
      SDCEN = SIN(DCEN)
      CDCEN = COS(DCEN)
C                                      For all stars
      DO 250 I = 1,NST
C                                      Work with differences from center
         DIFF(I)  = (AD(1,I)*DEGRES) - ACEN
         DENOM(I) = (SIN(AD(2,I)*DEGRES)*SDCEN) +
     *              (COS(AD(2,I)*DEGRES)*CDCEN)*COS(DIFF(I))
C                                      skip star with ra of  de a row, exit
         IF (DENOM(I).EQ.0.) THEN
            WRITE (MSGTXT,9900) I
            IERR = 5
            GO TO 960
            END IF
         X00(I,1) = (COS(AD(2,I)*DEGRES)*SIN(DIFF(I)))/DENOM(I)
         X00(I,2) = XY(1,I) - REFPIX(1)
         X00(I,3) = XY(2,I) - REFPIX(2)
C                              NLSQ selects linear or quad. solution
         IF (NLSQ.EQ.2) THEN
            X00(I,4) = X00(I,2)**2
            X00(I,5) = X00(I,3)**2
            X00(I,6) = X00(I,2)*X00(I,3)
            END IF
 250     CONTINUE
C                                       Call function to calc fit in ra
      CALL LMSTR1 (XTRFCN, NST, NP, A, FVEC, FJAC, LDFJAC, TOL, INF,
     *   IPVT, WA, LWA)
      IF ((INF.EQ.0).OR.(INF.EQ.4)) THEN
         MSGTXT = 'XTRFIT: RA FIT FAILED (ARE STARS ALIGNED?)'
         IERR = 4
         GO TO 960
         END IF
C                                      For Dec
      DO 320 I=1,NST
         X00(I,1) = ((SIN(AD(2,I)*DEGRES)*CDCEN) -
     *               (COS(AD(2,I)*DEGRES)*SDCEN*COS(DIFF(I))))/DENOM(I)
 320     CONTINUE
C                                       Call function to calc fit in ra
      CALL LMSTR1 (XTRFCN, NST, NP, B, FVEC, FJAC, LDFJAC, TOL, INF,
     *   IPVT, WA, LWA)
      IF ((INF.EQ.0).OR.(INF.EQ.4)) THEN
         MSGTXT = 'XTRFIT: DEC FIT FAILED (ARE STARS ALIGNED?)'
         IERR = 4
         GO TO 960
         END IF
C                                 Compute coord. increments, rot. angle
      ANGL1 = ATAN(B(3)/A(3))
      ANGL2 = ATAN(-A(2)/B(2))
      REFINC(1) = .5*(A(3)/COS(ANGL1) + B(3)/SIN(ANGL1))*RADIAN
      REFINC(2) = .5*(B(2)/COS(ANGL2) - A(2)/SIN(ANGL2))*RADIAN
      ANGLE = 0.5*(ANGL1 + ANGL2)
      IF ((ILSQ.EQ.2) .AND. (ABS((ANGL1-ANGL2)*RADIAN).GT.0.05)) THEN
         WRITE (MSGTXT,1330) ANGL1*RADIAN, ANGL2*RADIAN
         CALL MSGWRT (6)
         END IF
C                              If only first least squares trial
      IF ((ILSQ.EQ.2) .AND. (DOPRT)) THEN
         MSGTXT = 'Actual fit parameter values:'
         CALL MSGWRT (2)
         MSGTXT = 'Number     X_fit         Y_fit'
         CALL MSGWRT (2)
         DO 340 I = 1,6
            WRITE (MSGTXT,1335) I, A(I), B(I)
            CALL MSGWRT (4)
 340        CONTINUE
         MSGTXT = 'Fit and residuals from the calibration:'
         CALL MSGWRT (2)
         WRITE (MSGTXT,3810)
         CALL MSGWRT (2)
         WRITE (MSGTXT,3820)
         CALL MSGWRT (2)
         END IF
C                              Direction cosines for inverse transform
      DIRCOS(1,1) = -SIN(ACEN)
      DIRCOS(1,2) = -(COS(ACEN))*(SIN(DCEN))
      DIRCOS(1,3) = (COS(ACEN))*(COS(DCEN))
      DIRCOS(2,1) = COS(ACEN)
      DIRCOS(2,2) = -(SIN(ACEN))*(SIN(DCEN))
      DIRCOS(2,3) = (SIN(ACEN))*(COS(DCEN))
      DIRCOS(3,2) = COS(DCEN)
      DIRCOS(3,3) = SIN(DCEN)
C                              Compute residuals and RA and DEC
      NTOT = NST + 1
C                              for all standards, calc residuals
      DO 500 I=1,NTOT
C                              If on last star
         IF (I.GE.NTOT) THEN
C                              Coordinate of image center
            AX = A(1)
            AY = B(1)
         ELSE
C                              Jump out if first fit
            IF (ILSQ.EQ.1) GO TO 500
            AX = A(1) + A(2)*X00(I,3) + A(3)*X00(I,2)
            AY = B(1) + B(2)*X00(I,3) + B(3)*X00(I,2)
C                              If a second order fit
            IF (NLSQ.EQ.2) THEN
               AX = AX + A(4)*X00(I,4) + A(5)*X00(I,5) + A(6)*X00(I,6)
               AY = AY + B(4)*X00(I,4) + B(5)*X00(I,5) + B(6)*X00(I,6)
               END IF
            END IF
C                              Calc orientation
         R1 = AX*DIRCOS(1,1) + AY*DIRCOS(1,2) + DIRCOS(1,3)
         R2 = AX*DIRCOS(2,1) + AY*DIRCOS(2,2) + DIRCOS(2,3)
         R3 = AY*DIRCOS(3,2) + DIRCOS(3,3)
         AX = ATAN(ABS(R2/R1))
C                              Handle +/- 180 cases
         IF (R2.LE.0.) THEN
            IF (R1.LT.0.) THEN
               AX = AX + PI
            ELSE
               AX = 2.*PI - AX
               END IF
         ELSE
            IF (R1.LT.0.) THEN
               AX = PI - AX
               END IF
C                              Else R1, R2 positve, AX unchanged
C                              End if quadrant problem
            END IF
         AY = ATAN(R3*COS(AX)/R1)
C                              If really a star
         IF (I.LT.NTOT) THEN
C                              calc residuals in degrees
            RES(1,I) = AX*RADIAN - AD(1,I)
            RES(2,I) = AY*RADIAN - AD(2,I)
C                              residuals in arc seconds
            RTEMP1   = RES(1,I) * 3600. * COS (AD(2,I)*DEGRES)
            RTEMP2   = RES(2,I) * 3600.
C                              Tell Residuals
            WRITE (MSGTXT,4800) NAMES(I), AX*RADIAN, RTEMP1,
     *         AY*RADIAN, RTEMP2
            IF (DOPRT) CALL MSGWRT (2)
         ELSE
            A(1) = AX
            B(1) = AY
            END IF
C                                End of all star residuals loop
 500     CONTINUE
C                                If finished first fit
      IF (ILSQ.LE.1) THEN
C                                prepare to do second fit
         ILSQ = 2
         ACEN = A(1)
         DCEN = B(1)
         GO TO 220
         END IF
C                                Write coeff.
      IF (A(1).GT. 2.*PI) A(1) = A(1) - (2.*PI)
      WRITE (MSGTXT,5200)
      ANGLE = ANGLE * RADIAN
      IF (DOPRT) CALL MSGWRT (2)
C                                Put ref. params in output values
      REFVAL(1) = A(1)*RADIAN
      REFVAL(2) = B(1)*RADIAN
C                                       Print Inc in arc sec/pixel
      RTEMP1    = REFINC(1) * 3600.
      WRITE (MSGTXT,5210) 'X', REFVAL(1), REFPIX(1), RTEMP1, ANGLE
      IF (DOPRT) CALL MSGWRT (2)
      RTEMP1 = REFINC(2) * 3600.
      WRITE (MSGTXT,5210) 'Y', REFVAL(2), REFPIX(2), RTEMP1, ANGLE
      IF (DOPRT) CALL MSGWRT (2)
C                                Error handling
 960  IF (IERR.NE.0) THEN
C                                Only repeat messages 5 times
         IF (NPRT.LT.5) THEN
            CALL MSGWRT (5)
            NPRT = NPRT + 1
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1330 FORMAT ('WARNING: X_FIT ANGLE',F9.3,' Y_FIT ANGLE',F9.3,' DIFFER')
 1335 FORMAT (I4,2X,2(1PE14.5))
 3810 FORMAT ('  Name',7X,'X        X Resid',8X,'Y        Y Resid')
 3820 FORMAT (11X,'(deg)       (secs)',6X,'(deg)       (secs)')
 4800 FORMAT (2X, A4, 2X, F10.4, F10.2, 4X, F10.4, F10.2)
 5200 FORMAT (' Axis   Coord value    at pixel   Coord incr      Rotat')
 5210 FORMAT (3X,A1,1X,1PD16.9,0PF10.3,F13.5,F11.4)
 9900 FORMAT ('XTRFIT: ON STAR',I4,' TRIED TO DIVIDE BY ZERO')
      END
      SUBROUTINE XTRFCN (NPNTS, NPAR, A, FVEC, FJROW, IFLAG)
C-----------------------------------------------------------------------
C   Computes the values and Jacobian of the model.
C   Inputs:
C      NPNTS   I         No. of data points
C      NPAR    I         No. of parameters in fit
C      A       D(NPAR)   Fit coefficients
C      IFLAG   I         Type of operation: 1=value,
C                           N=Jacobian of row N-1
C   Outputs:
C      FVEC    D(NPNTS)   Model-Data for each point
C      FJROW   D(NPAR)    The function partials
C-----------------------------------------------------------------------
      INTEGER   NPNTS, NPAR, IFLAG, I, K
      REAL      X00(1000,6)
      DOUBLE PRECISION  A(*), FVEC(*), FJROW(*)
      COMMON /XYAD/ X00
C-----------------------------------------------------------------------
C                                        Find (Model - Data) values
      IF (IFLAG.LE.1) THEN
         DO 100 I = 1,NPNTS
            FVEC(I) = A(1) + A(2)*X00(I,3) + A(3)*X00(I,2) - X00(I,1)
            IF (NPAR.GT.3) THEN
               DO 50 K = 4,NPAR
                  FVEC(I) = FVEC(I) + A(K)*X00(I,K)
 50               CONTINUE
               END IF
 100        CONTINUE
C                                        Find IFLAG-1 row of Jacobian
      ELSE
         I = IFLAG - 1
         FJROW(1) = 1.
         FJROW(2) = X00(I,3)
         FJROW(3) = X00(I,2)
         IF (NPAR.GT.3) THEN
            DO 250 K = 4,NPAR
               FJROW(K) = X00(I,K)
 250           CONTINUE
            END IF
         END IF
C
 999  RETURN
      END
