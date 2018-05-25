LOCAL INCLUDE 'PCFITR.INC'
LOCAL END
LOCAL INCLUDE 'PCFITD.INC'
      INCLUDE 'INCS:PPCV.INC'
      INTEGER   SPMAX
      PARAMETER (SPMAX = MAXTON*MAXIF)
C
      INTEGER   ITTER, NITTER
      DOUBLE PRECISION QDATA(SPMAX), UDATA(SPMAX), DELTAF(SPMAX)
      COMMON /PCFITD/ DELTAF, QDATA, UDATA, ITTER, NITTER
LOCAL END
      SUBROUTINE PCFITR (PCNPOL, PCNIF, IFSTEP, NUMTON, PCFREQ, PCREAL,
     *   PCIMAG, PRTLEV, PCDELY, PCPHAS, ERDELY, ERPHAS, WEIGHT, IRET)
C-----------------------------------------------------------------------
C! Fit a Pulse cal spectrm for delay and phase
C# VLB Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2016-2017
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
C   A package of routines to fit a pulse-cal delay and phase
C   Inputs:
C      PCNPOL   I      Number polarizations
C      PCNIF    I      Number of IFs
C      IFSTEP   I      Bunch IFSTEP IFs together
C      NUMTON   I      Number of tones
C      PCFREQ   D(*)   Pulse cal frequencies (2,MAXTON,MAXIF)
C      PRTLEV   R      0 - no messages, 1 -
C   In/out
C      PCREAL   R(*)   Pulse cal real part (2,MAXTON,MAXIF)
C      PCIMAG   R(*)   Pulse cal imaginary part (2,MAXTON,MAXIF)
C                        On output - residual values
C   Outputs:
C      PCDELY   R(*)   Delay (2,MAXIF)
C      PCPHAS   R(*)   Phase in radians (2,MAXIF)
C      IRET     I      Number of IFs that failed to fit (delay and
C                         phase are magic blanked)
C-----------------------------------------------------------------------
      INCLUDE 'PCFITD.INC'
      INTEGER   PCNPOL, PCNIF, IFSTEP, NUMTON, IRET
      DOUBLE PRECISION  PCFREQ(2,MAXTON,*)
      REAL      PCREAL(2,MAXTON,*), PCIMAG(2,MAXTON,*), PCDELY(2,*),
     *   PCPHAS(2,*), PRTLEV, ERDELY(2,*), ERPHAS(2,*), WEIGHT(2,*)
C
      EXTERNAL PCFUNC
      INTEGER   NLIST, MAXPRM
      PARAMETER (NLIST = 10000, MAXPRM = 2)
C
      INTEGER   IPOL, LIF, MIF, K, ITC, NSUM, ICH, PHICH(NLIST),
     *   PHLIF(NLIST), IIF, INFO, IPVT(2), I, J, JNPARM, JNPTS, LCH
      DOUBLE PRECISION REFREQ, VALVAR(2), FJAC(2,2), FVEC(SPMAX), TOL,
     *   ERRVAR(MAXPRM)
      REAL      DDSUM, DDSUMS, PHSUM, PHSUMS, PHLAST, PHDIFF(NLIST),
     *   PHASES(NLIST), DD, DDFACT, AMP, RMS, PHNOW, PD, VALUE, FF(3)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA FF /5.0, 4.0, 3.0/
C-----------------------------------------------------------------------
      NITTER = 100
      ITC = (NUMTON + 1) / 2
      IRET = 0
      DO 90 IPOL = 1,PCNPOL
         DO 80 LIF = 1,PCNIF,IFSTEP
            REFREQ = PCFREQ(IPOL,ITC,LIF)
            K = 0
            MIF = MIN (LIF + IFSTEP - 1, PCNIF)
            DDSUM = 0.0
            DDSUMS = 0.0
            PHSUM = 0.0
            PHSUMS = 0.0
            NSUM = 0
            DO 30 IIF = LIF,MIF
               PHLAST = -10000.
C                                       omit last one
               DO 20 ICH = 1,NUMTON-1
                  IF ((PCREAL(IPOL,ICH,IIF).NE.FBLANK) .AND.
     *               (PCIMAG(IPOL,ICH,IIF).NE.FBLANK)) THEN
                     K = K + 1
                     AMP = SQRT (PCREAL(IPOL,ICH,IIF)**2 +
     *                  PCIMAG(IPOL,ICH,IIF)**2)
                     QDATA(K) = PCREAL(IPOL,ICH,IIF) / AMP
                     UDATA(K) = PCIMAG(IPOL,ICH,IIF) / AMP
                     DELTAF(K) = (PCFREQ(IPOL,ICH,IIF) - REFREQ) *
     *                  1.0D-9
                     IF (PHLAST.LT.-9000.) THEN
                        PHLAST = ATAN2 (UDATA(K), QDATA(K))
                        PHDIFF(K) = -10000.
                        PHASES(K) = PHLAST * RAD2DG
                        PHICH(K) = ICH
                        PHLIF(K) = IIF
                        LCH = ICH
                     ELSE
                        PHNOW = ATAN2 (UDATA(K), QDATA(K))
                        PD = ((PHNOW - PHLAST) * RAD2DG) / (ICH-LCH)
                        IF (PD.GT.180.) THEN
                           PD = PD - 360.0
                        ELSE IF (PD.LT.-180.0) THEN
                           PD = PD + 360.0
                           END IF
                        IF (K.LE.NLIST) THEN
                           PHASES(K) = PHNOW * RAD2DG
                           PHDIFF(K) = PD
                           PHICH(K) = ICH
                           PHLIF(K) = IIF
                           END IF
                        PHSUM = PHSUM + PD
                        PHSUMS = PHSUMS + PD * PD
                        DDFACT = (ICH-LCH) / (360.0 * 1.D-9 *
     *                     (PCFREQ(IPOL,ICH,IIF)-PCFREQ(IPOL,LCH,IIF)))
                        DD = PD * DDFACT
                        DDSUM = DDSUM + DD
                        DDSUMS = DDSUMS + DD * DD
                        NSUM = NSUM + 1
                        LCH = ICH
                        PHLAST = PHNOW
                        END IF
                  ELSE
                     PHLAST = -10000.
                     END IF
 20               CONTINUE
 30            CONTINUE
C                                       got none
            IF (K.LE.2) THEN
               IRET = IRET + (MIF-LIF+1)
               DO 35 IIF = LIF,MIF
                  PCPHAS(IPOL,IIF) = FBLANK
                  PCDELY(IPOL,IIF) = FBLANK
                  WEIGHT(IPOL,IIF) = 0.0
                  DO 34 ICH = 1,NUMTON
                     PCREAL(IPOL,ICH,IIF) = FBLANK
                     PCIMAG(IPOL,ICH,IIF) = FBLANK
 34               CONTINUE
 35            CONTINUE
C                                       fit this one
            ELSE
               PHSUM = PHSUM / NSUM
               PHSUMS = PHSUMS / NSUM - PHSUM * PHSUM
               PHSUMS = SQRT (MAX (0.0, PHSUMS))
C                                       save init guess
               DDSUM = DDSUM / NSUM
               VALVAR(1) = DDSUM
               DDSUMS = DDSUMS / NSUM - DDSUM * DDSUM
               DDSUMS = SQRT (MAX (0.0, DDSUMS))
               LCH = (K + 1) / 2
               PHNOW = ATAN2 (UDATA(LCH), QDATA(LCH))
               VALVAR(2) = PHNOW - TWOPI * VALVAR(1) * DELTAF(LCH)
               VALVAR(2) = MOD (VALVAR(2), TWOPI)
C                                       lobe issues
               DO 40 J = 1,K
                  IF (PHDIFF(J).GT.-9000.) THEN
                     IF (ABS(PHDIFF(J)+360.-PHSUM).LT.
     *                  ABS(PHDIFF(J)-PHSUM)) THEN
                        PHDIFF(J) = PHDIFF(J) + 360.0
                     ELSE IF (ABS(PHDIFF(J)-360.-PHSUM).LT.
     *                  ABS(PHDIFF(J)-PHSUM)) THEN
                        PHDIFF(J) = PHDIFF(J) - 360.0
                        END IF
                     END IF
 40               CONTINUE
               I = 0
 45            RMS = PHSUMS
               VALUE = PHSUM
               IF (I.LT.3) THEN
                  I = I + 1
                  PHSUM = 0.0
                  PHSUMS = 0.0
                  DDSUM = 0.0
                  DDSUMS = 0.0
                  NSUM = 0
                  RMS = FF(I) * RMS
                  DO 50 J = 1,K
                     PD = PHDIFF(J)
                     IF ((PD.GT.-9000.) .AND. (ABS(PD-VALUE).LT.RMS))
     *                  THEN
                        ICH = PHICH(J)
                        IIF = PHLIF(J)
                        PHSUM = PHSUM + PD
                        PHSUMS = PHSUMS + PD * PD
                        NSUM = NSUM + 1
                        DDFACT = 1.0 / (360.0 * 1.D-9 *
     *                     (PCFREQ(IPOL,ICH,IIF) -
     *                     PCFREQ(IPOL,ICH-1,IIF)))
                        DD = PD * DDFACT
                        DDSUM = DDSUM + DD
                        DDSUMS = DDSUMS + DD * DD
                        END IF
 50                  CONTINUE
                  IF (NSUM.GT.2) THEN
                     PHSUM = PHSUM / NSUM
                     PHSUMS = PHSUMS / NSUM - PHSUM * PHSUM
                     PHSUMS = SQRT (MAX (0.0, PHSUMS))
                     DDSUM = DDSUM / NSUM
                     VALVAR(1) = DDSUM
                     DDSUMS = DDSUMS / NSUM - DDSUM * DDSUM
                     DDSUMS = SQRT (MAX (0.0, DDSUMS))
                     LCH = (K + 1) / 2
                     PHNOW = ATAN2 (UDATA(LCH), QDATA(LCH))
                     VALVAR(2) = PHNOW - TWOPI * VALVAR(1) * DELTAF(LCH)
                     VALVAR(2) = MOD (VALVAR(2), TWOPI)
                     GO TO 45
                     END IF
                  END IF
               WRITE (MSGTXT,1050) IPOL, LIF, MIF
               IF (PRTLEV.GT.0.5) CALL MSGWRT (3)
               WRITE (MSGTXT,1051) VALVAR(2)*RAD2DG, VALVAR(1), DDSUMS
               IF (PRTLEV.GT.1.5) CALL MSGWRT (3)
               TOL = 1.D-5
               JNPTS = 2 * K
               JNPARM = 2
               ITTER = 0
               CALL PCALMS (PCFUNC, JNPTS, JNPARM, VALVAR, FVEC,
     *            FJAC, MAXPRM, TOL, INFO, IPVT)
               IF (INFO.EQ.-1) THEN
                  MSGTXT = 'Number of iterations exceeded when' //
     *               ' trying to fit'
               ELSE
                  WRITE (MSGTXT,1052) INFO
                  END IF
               IF ((INFO.LE.0) .OR. (INFO.GT.3)) THEN
                  IRET = IRET + (MIF-LIF+1)
                  IF (PRTLEV.GT.0.5) CALL MSGWRT (6)
                  DO 55 IIF = LIF,MIF
                     PCPHAS(IPOL,IIF) = FBLANK
                     PCDELY(IPOL,IIF) = FBLANK
                     WEIGHT(IPOL,IIF) = 0.0
                     DO 54 ICH = 1,NUMTON
                        PCREAL(IPOL,ICH,IIF) = FBLANK
                        PCIMAG(IPOL,ICH,IIF) = FBLANK
 54                     CONTINUE
 55                  CONTINUE
C                                       return answer
               ELSE
                  TOL = 0.0D0
                  DO 60 K = 1,JNPTS
                     TOL = TOL + FVEC(K) * FVEC(K)
 60                  CONTINUE
                  TOL = 2.0D0 * TOL / JNPTS
                  TOL = SQRT (MAX (0.0D0, TOL))
                  WRITE (MSGTXT,1060) VALVAR(2)*RAD2DG, VALVAR(1), TOL
                  IF (PRTLEV.GT.0.5) CALL MSGWRT (3)
C                                       errors
                  CALL PCERRS (JNPTS, JNPARM, VALVAR, IPVT, FJAC, FVEC,
     *               ERRVAR)
                  WRITE (MSGTXT,1061) ERRVAR(2)*RAD2DG, ERRVAR(1)
                  IF (PRTLEV.GT.0.5) CALL MSGWRT (3)

                  DO 70 IIF = LIF,MIF
                     PCPHAS(IPOL,IIF) = VALVAR(2)
                     PCDELY(IPOL,IIF) = VALVAR(1) * 1.0D-9
                     ERPHAS(IPOL,IIF) = ERRVAR(2)
                     ERDELY(IPOL,IIF) = ERRVAR(1) * 1.0D-9
                     WEIGHT(IPOL,IIF) = 1.0 / TOL / TOL
                     DO 65 ICH = 1,NUMTON
                        IF ((PCREAL(IPOL,ICH,IIF).NE.FBLANK) .AND.
     *                     (PCIMAG(IPOL,ICH,IIF).NE.FBLANK)) THEN
                           AMP = SQRT (PCREAL(IPOL,ICH,IIF)**2 +
     *                        PCIMAG(IPOL,ICH,IIF)**2)
                           PHLAST = ATAN2 (PCIMAG(IPOL,ICH,IIF),
     *                        PCREAL(IPOL,ICH,IIF))
                           PHSUM = VALVAR(2) + TWOPI * VALVAR(1) *
     *                        (PCFREQ(IPOL,ICH,IIF) - REFREQ) * 1.0D-9
                           PCREAL(IPOL,ICH,IIF) = AMP*COS(PHLAST-PHSUM)
                           PCIMAG(IPOL,ICH,IIF) = AMP*SIN(PHLAST-PHSUM)
                           END IF
 65                     CONTINUE
 70                  CONTINUE
                  END IF
               END IF
 80         CONTINUE
 90      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('Fitting polarization',I2,'  IFs',I3,' -',I3)
 1051 FORMAT ('Phase    ',F8.2,' deg  Delay    ',F10.3,' +-',F8.3,
     *   ' ns guess')
 1052 FORMAT ('FITTING ROUTINE RETURNS ERROR CODE',I3)
 1060 FORMAT ('Phase    ',F8.2,' deg  Delay    ',F10.3,' ns  rms',F6.3)
 1061 FORMAT ('Phase Err',F8.2,' deg  Delay Err',F10.3,' ns')
      END
      SUBROUTINE PCALMS (FCN, M, N, X, FVEC, FJAC, LDFJAC, TOL, INFO,
     *   IPVT)
C-----------------------------------------------------------------------
C   XGALMS provides an extra interface to the math routine LMSTR1
C   and holds the WORK array (for overlay purposes)
C   Inputs:
C      FCN      EXT      Function to evaluate the model
C      M        I        Number data points (adj. array dim.)
C      N        I        Number of unknowns (adj. array dim.)
C      LDFJAC   I        Number points on first axis of FJAC (adj.
C                           array dim.)
C      TOL      D        Tolerance desired
C   In/out:
C      X        D(N)     Initial guess/ answer
C      FVEC     D(M)     Function (Data - model) evaluation
C      FJAC     D(N,N)   Work matrix
C      INFO     I        Error code: 1 - 3 good, 0 bad input,
C                           4 orthogonal, 5 - 7 poor fit
C      IPVT     D(N)     Permutation matrix
C   See precursor remarks to LMSTR1 or LMSTR for details.
C-----------------------------------------------------------------------
      EXTERNAL  FCN
      INTEGER   M, N, LDFJAC, INFO, IPVT(N)
      DOUBLE PRECISION X(N), FVEC(M), FJAC(LDFJAC,N), TOL
C
      INTEGER   LWA
      DOUBLE PRECISION WA(10000)
      DATA LWA /10000/
C-----------------------------------------------------------------------
C                                       It's just a dummy routine
      CALL LMSTR1 (FCN, M, N, X, FVEC, FJAC, LDFJAC, TOL, INFO, IPVT,
     *   WA, LWA)
C
 999  RETURN
      END
      SUBROUTINE PCFUNC (M, N, VALVAR, FVEC, FJROW, IFLAG)
C-----------------------------------------------------------------------
C   This routine is called by the Argonne package to calculate the
C   difference between the current fit and the actual data OR the
C   Jacobian for this difference.
C   Inputs:
C      M        I      Number of data points in Q plus U
C      N        I      No. of parameters (adj. array dim.;
C                        NCOMPS * 4)
C      VALVAR   D(N)   parameters of components being fitted
C                        GMAX(1), GPOS(1), GWIDTH(1), GMAX(2), ...
C      IFLAG    I      1 = calculate difference for current guess.
C                      2 = calculate jacobian for current guess.
C    COMMON GDATA
C      QDATA    D(*)   Original PCREAL data points this group
C      UDATA    D(*)   Original PCIMAG data points.this group
C      ITTER    I      number of calls to evaluate FVEC.
C   Outputs:
C      FVEC     D(M)   Q the U data points minus function evaluated
C                      for current guess.
C      FJROW1   D(N)   Row (IFLAG - 1) of Jacobian.
C-----------------------------------------------------------------------
      INTEGER   N, M, IFLAG
      DOUBLE PRECISION VALVAR(N), FVEC(M), FJROW(N)
C
      INCLUDE 'PCFITD.INC'
      INTEGER   M2, ID
      DOUBLE PRECISION QQ, UU
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      M2 = M / 2
C                                       Determine difference between
C                                       data and current fit.
      IF (IFLAG.LE.1) THEN
         ITTER = ITTER + 1
         IF (ITTER.GT.NITTER) THEN
            IFLAG = -1
            GO TO 999
            END IF
C                                       Q residual
         DO 20 ID = 1,M2
            FVEC(ID) = QDATA(ID)
            IF (FVEC(ID).EQ.FBLANK) THEN
               FVEC(ID) = 0.0D0
            ELSE
               QQ = COS (TWOPI * DELTAF(ID) * VALVAR(1) + VALVAR(2))
               FVEC(ID) = FVEC(ID) - QQ
               END IF
 20         CONTINUE
C                                       U residual
         DO 40 ID = M2+1,M
            FVEC(ID) = UDATA(ID-M2)
            IF (FVEC(ID).EQ.FBLANK) THEN
               FVEC(ID) = 0.0D0
            ELSE
               UU = SIN (TWOPI * DELTAF(ID-M2) * VALVAR(1) + VALVAR(2))
               FVEC(ID) = FVEC(ID) - UU
               END IF
 40         CONTINUE
C                                       Calculate Jacobian.
      ELSE
         ID = IFLAG - 1
C                                       try negating
         IF (ID.LE.M2) THEN
            QQ = TWOPI * DELTAF(ID) * VALVAR(1) + VALVAR(2)
            FJROW(1) = SIN (QQ) * TWOPI * DELTAF(ID)
            FJROW(2) = SIN (QQ)
         ELSE
            QQ = TWOPI * DELTAF(ID-M2) * VALVAR(1) + VALVAR(2)
            FJROW(1) = - COS (QQ) * TWOPI * DELTAF(ID-M2)
            FJROW(2) = - COS (QQ)
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE PCERRS (INPTS, NP, PARMS, IPVT, FJAC, FVEC, EPARMS)
C-----------------------------------------------------------------------
C   Find errors
C   Inputs:
C      INPTS    I      Number data samples
C      NP       I      Number parameters
C      PARMS    D(2)   Answers
C      IPVT     I(*)   from LMSTR1
C      FJAC     D(*)   from LMSTR1
C      FVEC     D(*)   from LMSTR1 (residuals)
C   Output:
C      EPARMS   D(2)   Errors
C-----------------------------------------------------------------------
      INTEGER   MAXPRM
      PARAMETER (MAXPRM=2)
C
      INTEGER   INPTS, NP, IPVT(*)
      DOUBLE PRECISION PARMS(*), FJAC(MAXPRM,MAXPRM), FVEC(*), EPARMS(*)
C
      INTEGER   JP
      DOUBLE PRECISION ENORM, FNORM, TOL, WORK(200), EPSILN,
     *   SFJAC(MAXPRM,MAXPRM)
C-----------------------------------------------------------------------
      TOL = 1.D-5
      FNORM = ENORM (INPTS, FVEC)
      JP = MAXPRM * MAXPRM
      CALL DPCOPY (JP, FJAC, SFJAC)
      EPSILN = FNORM / SQRT (REAL(INPTS-JP))
      JP = MAXPRM
      CALL COVAR (JP, SFJAC, JP, IPVT, TOL, WORK)
      EPSILN = FNORM / SQRT (REAL(INPTS-JP))
      EPARMS(1) = EPSILN * SQRT (SFJAC(1,1))
      EPARMS(2) = EPSILN * SQRT (SFJAC(2,2))
C
 999  RETURN
      END
