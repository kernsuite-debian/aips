      SUBROUTINE CLBPA (IS, JS, VOBS, VWT, CREAL, CIMAG, CWT, REFAN,
     *   MAXFRQ, MAXIFS, MAXBL, NUMANT, NUMBL, NUMIF, REFANT, AVGIF,
     *   DOL1, DOGCON, GAERR, MODE, MINNO, CONFAC, SNRMIN, PRTLV,
     *   RMSLIM, FFLIM, FFLAST, CLOSER, TIME, PRTSOU, IC, GSOLV, HSIZE,
     *   HFRAC, NRMS, ARMS, ARMSS)
C-----------------------------------------------------------------------
C! Gain Soln: Call gain solution routine and fill in solns.
C# UV Calibration EXT-appl AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2004-2006, 2008-2009, 2014
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
C   CLBPA does least squares solutions for phase and optionally
C   amplitude.  Four methods are available, "normal", "L1", "amplitude
C   constrained", and "normal" with limited sets of solutions.
C      All frequencies in each IF and at all times are assumed to have
C   been averaged.  If AVGIF is true then the data is assumed to have
C   been averaged in frequency and the solution found for the first IF
C   is copied to all IFs.
C   Input:
C      IS          I(*) First ant. of baseline numbers
C      JS          I(*) 2nd ant. of baseline numbers
C      VOBS        R(2,MAXBL,MAXFRQ) Visibility array
C      MAXFRQ      I    Maximum number of frequency channels.
C      MAXIFS      I    Maximum number of IFs
C      MAXBL       I    Maximum number of baselines
C      NUMANT      I    Number of antennas
C      NUMBL       I    Number of baselines
C      NUMIF       I    Number of IFs
C      REFANT      I    Reference antenna to use.
C      AVGIF       L    If true average IFs.
C      DOL1        L    If true, use L1 solution.
C      DOGCON      L    If true, use constrained soln
C      GAERR(*)    R    The estimated variance of the antenna
C                       amplitudes; 1 per antenna.
C      MODE        I    Solution mode; 0= full gain, 1=phase
C                       2=phase(ignore amp), 3=full, constrain
C                       amplitude, 4-7 as 0-3 with robust methods
c                       + 10 * (weight mod code)
C      MINNO       I    Minimum number of antannas allowed
C      CONFAC      R    Factor for amp. penalty term.
C      SNRMIN      R    Minimum SNR allowed.
C      PRTLV       I    Print level: 0 none, 1 statistics if exceed
C                       limits, 2 individual overflows, 3 SNRs,
C                       4 all individual overflows, 5 solutions, 6 lots
c      RMSLIM   R        Don't print individual closure errors unless
C                        they exceed RMSLIM * expected RMS.
C      FFLIM    R        Limit clip to max (std, FFLIM) * rms
C      FFLAST   R        Mark bad those data with closure error >
C                        FFLAST * rms closure error on last iteration
C      CLOSER   R(2,2)   If closure errors exceed these limits, then
C                        messages may be produced.  (i,j) where i=1 for
C                        statistics = 2 for overflow; j = 1 amp, 2 phase
C                        > 10**8 => no test
C      TIME     D        Time in days of data, used for labeling.
C      PRTSOU   C*16     Current source name.
C      IC       I        Stokes' number passed, -1 => both as separate
C                        sets of frequencies; 0=> data for poln.
C                        averaged. 1 = R, 2 = L, 3 = I
C      GSOLV    L(*)     List of antennas for which to solve for gains
C                        GSOLV(1) = false means rest are all false
C      HSIZE    I        Size of histogram of fraction deleted
C   In/out:
C      VWT      R(2,MAXBL,MAXFRQ) Visibility weights
C                        output: any flagging returned
C      HFRAC    I(*)     histogram of fraction "discarded"
C      NRMS     R        Count of RMS of fit (closure)
C      ARMS     R        Sum of RMS of fit (closure)
C      ARMSS    R        Sum of RMS*RMS of fit (closure)
C   Output:
C      CREAL    R(2,NUMIF,NUMANT)   Real part of solution
C      CIMAG    R(2,NUMIF,NUMANT)   Imag part of solution
C      CWT      R(2,NUMIF,NUMANT)   Weights = SNR, -1 failed,
C                                      -2 no data
C      REFAN    I(2,NUMIF)          Reference antennas used
C-----------------------------------------------------------------------
      INTEGER   MAXFRQ, MAXIFS, MAXBL, IS(*), JS(*), NUMBL, NUMIF,
     *   NUMANT, REFANT, IC, REFAN(2,*), MODE, MINNO, PRTLV,
     *   HSIZE, HFRAC(*), NRMS, IERR
      LOGICAL   AVGIF, DOL1, DOGCON, GSOLV(*)
      REAL      VOBS(2,MAXBL,MAXFRQ), VWT(2,MAXBL,MAXFRQ),
     *   CREAL(2,MAXIFS,*), CIMAG(2,MAXIFS,*), CWT(2,MAXIFS,*),
     *   GAERR(*), SNRMIN, CONFAC, CLOSER(2,2), RMSLIM, FFLIM, FFLAST,
     *   ARMS, ARMSS
      DOUBLE PRECISION TIME
      CHARACTER PRTSOU*16
C
      INTEGER   IREF, CIF, IBL, IANT, IST, JST, KST, NST, ISTOFF,
     *   LPRTLV, KDAY, KHR, KMN, KSEC, I, J, IFR
      LOGICAL   INIT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER   NBL, IIS(MXBASE), JJS(MXBASE), LBL, LWT, LMODE
      REAL      GAIN(2,MAXANT), SNR(MAXANT), WTBT(MXBASE), AMP, WGT,
     *   TIMEX, FRAC, RMS, TEMP, XXX, SWT(MAXANT)
      INCLUDE 'INCS:GAIN.INC'
C-----------------------------------------------------------------------
      INIT = .TRUE.
      NST = 1
      IF (IC.LT.0) NST = 2
      LPRTLV = MAX (0, PRTLV-3)
      IF (RMSLIM.LE.0.0) RMSLIM = 2.5
C                                       interpret mode
      LWT = MODE / 10
      LMODE = MOD (MODE, 10)
      IF (LMODE.GT.3) LMODE = LMODE - 4
C                                       Write time if requested
      IF (LPRTLV.GE.1) THEN
         KDAY = TIME
         TIMEX = (TIME - KDAY) * 24.
         KHR = TIMEX
         TIMEX = (TIMEX - KHR) * 60.
         KMN = TIMEX
         TIMEX = (TIMEX - KMN) * 60.
         KSEC = TIMEX + 0.5
         IF (KSEC.EQ.60) THEN
            KSEC = 0
            KMN = KMN + 1
            IF (KMN.EQ.60) THEN
               KMN = 0
               KHR = KHR + 1
               IF (KHR.EQ.60) THEN
                  KHR = 0
                  KDAY = KDAY + 1
                  END IF
               END IF
            END IF
         WRITE (MSGTXT,2001) KDAY, KHR, KMN, KSEC, PRTSOU
         CALL MSGWRT (2)
         END IF
C                                       (Loop) over Stokes' type
      DO 600 JST = 1,NST
         IST = IC
         IF (IC.LE.0) IST = JST
C                                       Ipol soln:
         IF (IC.EQ.3) IST = 1
C                                       Trap funny business with Stokes'
         ISTOFF = 0
         IF ((IC.LT.0) .AND. (JST.EQ.2)) ISTOFF = NUMIF
C                                       Loop over IF
         DO 500 CIF = 1,NUMIF
C                                       Copy data to XOBS
            NBL = 0
            DO 50 IBL = 1,NUMBL
               WGT = VWT(1,IBL,CIF+ISTOFF)
               I = IS(IBL)
               J = JS(IBL)
               IF ((WGT.GT.1.0E-20) .AND. ((.NOT.GSOLV(1)) .OR.
     *            (GSOLV(1+I)) .OR. (GSOLV(1+J)))) THEN
                  NBL = NBL + 1
                  XOBS(1,NBL) = VOBS(1,IBL,CIF+ISTOFF)
                  XOBS(2,NBL) = VOBS(2,IBL,CIF+ISTOFF)
                  WTBT(NBL) = WGT
                  IIS(NBL) = I
                  JJS(NBL) = J
                  END IF
 50            CONTINUE
C                                       Blank output
            DO 60 IANT = 1,NUMANT
               CREAL(IST,CIF,IANT) = FBLANK
               CIMAG(IST,CIF,IANT) = FBLANK
               CWT(IST,CIF,IANT) = 0.0
               SWT(IANT) = 0.0
 60            CONTINUE
C                                       do we havae data
            DO 70 IBL = 1,NUMBL
               XXX = XOBS(1,IBL)*XOBS(1,IBL) + XOBS(2,IBL)*XOBS(2,IBL)
               IF (XXX*WTBT(IBL).GT.1.E-12) THEN
                  SWT(IIS(IBL)) = SWT(IIS(IBL)) + 1.0
                  SWT(JJS(IBL)) = SWT(JJS(IBL)) + 1.0
                  END IF
 70            CONTINUE
            REFAN(IST,CIF) = 0
            IREF = REFANT
C                                       Do solution
            IF ((.NOT.AVGIF) .OR. (CIF.LE.1)) THEN
C                                       normal but on subset
               IF (GSOLV(1)) THEN
                  CALL GCALC2 (XOBS, IIS, JJS, WTBT, NBL, REFANT, MODE,
     *               MINNO, GAIN, GSOLV(2), IREF, HESS, LPRTLV, FFLIM,
     *               FFLAST, FRAC, RMS, IERR)
C                                       exact solutions: 1 BASELINE
               ELSE IF ((NBL.EQ.1) .OR. (NBL.EQ.2)) THEN
                  CALL ECALC (INIT, XOBS, IIS, JJS, WTBT, NBL, REFANT,
     *               MODE, MINNO, GAIN, IREF, LPRTLV, IERR)
                   FRAC = 0.0
                   RMS = 0.0
C                                       Amplitude constrained
               ELSE IF (DOGCON) THEN
                  CALL NCALC (INIT, XOBS, IIS, JJS, WTBT, NBL, REFANT,
     *               MODE, MINNO, GAERR, CONFAC, GAIN, IREF, LPRTLV,
     *               FFLIM, FFLAST, FRAC, RMS, IERR)
C                                       L1 solution
               ELSE IF (DOL1) THEN
                  CALL GCALC1 (INIT, XOBS, IIS, JJS, WTBT, NBL, REFANT,
     *               MODE, MINNO, GAIN, IREF, LPRTLV, FFLIM, FFLAST,
     *               FRAC, RMS, IERR)
C                                       Normal
               ELSE
                  CALL GCALC (INIT, XOBS, IIS, JJS, WTBT, NBL, REFANT,
     *               MODE, MINNO, GAIN, IREF, HESS, LPRTLV, FFLIM,
     *               FFLAST, FRAC, RMS, IERR)
                  END IF
               NRMS = NRMS + 1
               ARMS = ARMS + RMS
               ARMSS = ARMSS + RMS * RMS
C                                       return flagging and
C                                       restore true weights
               LBL = 0
               DO 90 IBL = 1,NUMBL
                  WGT = VWT(1,IBL,CIF+ISTOFF)
                  I = IS(IBL)
                  J = JS(IBL)
                  IF ((WGT.GT.1.0E-20) .AND. ((.NOT.GSOLV(1)) .OR.
     *               (GSOLV(1+I)) .OR. (GSOLV(1+J)))) THEN
                     LBL = LBL + 1
                     IF (WTBT(LBL).LE.0.0) THEN
                        VWT(1,IBL,CIF+ISTOFF) = WTBT(LBL)
                     ELSE IF (LWT.EQ.1) THEN
                        WTBT(LBL) = WTBT(LBL) ** 2
                     ELSE IF (LWT.EQ.2) THEN
                        WTBT(LBL) = WTBT(LBL) ** 4
                     ELSE IF (LWT.EQ.3) THEN
                        WTBT(LBL) = VWT(1,IBL,CIF+ISTOFF)
                        IF (LMODE.EQ.2) THEN
                           XXX = SQRT (XOBS(1,LBL)**2 + XOBS(2,LBL)**2)
                           WTBT(LBL) = WTBT(LBL) * XXX * XXX
                           END IF
                        END IF
                     END IF
 90               CONTINUE
C                                       Solution failed
               IF (IERR.NE.0) THEN
                  TEMP = -1.0
                  IF (IERR.EQ.1) TEMP = -2.0
                  DO 100 IANT = 1,NUMANT
                     IF (SWT(IANT).LE.0.0) THEN
                        CWT(IST,CIF,IANT) = -2.0
                     ELSE
                        CWT(IST,CIF,IANT) = TEMP
                        END IF
 100                 CONTINUE
                  GO TO 155
                  END IF
               IFR = FRAC * HSIZE + 1.9801
               HFRAC(1) = HFRAC(1) + 1
               HFRAC(IFR) = HFRAC(IFR) + 1
C                                       Convert amplitude to 1/amp
C                                       to correct data.
               DO 110 IANT = 1,NUMANT
                  AMP = GAIN(1,IANT)*GAIN(1,IANT) +
     *                  GAIN(2,IANT)*GAIN(2,IANT)
                  IF (AMP.LT.1.0E-20) AMP = 1.0
                  GAIN(1,IANT) = GAIN(1,IANT) / AMP
                  GAIN(2,IANT) = GAIN(2,IANT) / AMP
 110              CONTINUE
C                                       Compute SNRs
               KST = IST
C                                       Trap for labeling Ipol data.
               IF (IC.EQ.3) KST = 5
               CALL CLBSNR (XOBS, IIS, JJS, WTBT, NBL, NUMANT, GAIN,
     *            SNR, CLOSER, SNRMIN, TIME, CIF, KST, GWORK, PRTLV,
     *            RMSLIM, PRTSOU)
               END IF
C                                       Save results
            DO 150 IANT = 1,NUMANT
C                                       antenna not solved, pass
               IF ((GSOLV(1)) .AND. (.NOT.GSOLV(IANT+1))) THEN
                  CREAL(IST,CIF,IANT) = 1.0
                  CIMAG(IST,CIF,IANT) = 0.0
                  CWT(IST,CIF,IANT) = MAX (SNRMIN + 1.0, SNR(IANT))
C                                       solution okay
               ELSE IF (SNR(IANT).GE.SNRMIN) THEN
                  CREAL(IST,CIF,IANT) = GAIN(1,IANT)
                  CIMAG(IST,CIF,IANT) = GAIN(2,IANT)
                  CWT(IST,CIF,IANT) = SNR(IANT)
C                                       solution failed
               ELSE IF (SWT(IANT).LE.0.0) THEN
                  CWT(IST,CIF,IANT) = -2.0
                  END IF
 150           CONTINUE
            REFAN(IST,CIF) = IREF
C                                       IC=0 means Stokes' averaged.
 155        IF (IC.EQ.0) THEN
               DO 160 IANT = 1,NUMANT
                  CREAL(2,CIF,IANT) = CREAL(1,CIF,IANT)
                  CIMAG(2,CIF,IANT) = CIMAG(1,CIF,IANT)
                  CWT(2,CIF,IANT) = CWT(1,CIF,IANT)
 160              CONTINUE
               REFAN(2,CIF) = REFAN(1,CIF)
               END IF
 500        CONTINUE
 600     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 2001 FORMAT ('Time =',I4,'/',I2.2,2(':',I2.2),'   Source = ''',A,'''')
      END
