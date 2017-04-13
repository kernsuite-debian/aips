      SUBROUTINE NEVAL (X, W, STEP, VOBS, IS, JS, IA, WT, NUMBL, NTD,
     *   MODE, PRTLV, GAERR, CONFAC, G, S, PENSUM)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004
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
C   NEVAL takes a test step in the values in G and computes the
C   function being minimized by NCALC.
C   Inputs:
C     X(*)           D    Model parameters
C     W              D    Step size to take, fraction of STEP
C     STEP(*)        D    Step for each of X.
C     VOBS(2,*)      R    Visibilities
C     IS(*)          I    First ant. of baseline numbers
C     JS(*)          I    2nd ant. of baseline numbers
C     IA(*)          I    Actual antenna order number of each parameter.
C     WT(*)          R    Weights
C     NUMBL          I    Number of visibilities
C     NTD            I    Number of antennas with data
C     MODE           I    Solution mode: 0 = full gain soln.
C                                    1 = phase only keep ampl. info.
C                                    2 = phase only discard ampl. info.
C                                    3 = full gain with amplitudes
C                                        constrained.
C     PRTLV          I    Print level, gives some results if .ge. 2
C     GAERR(*)       R    Standard deviations of the assumed
C                         distributions of the amplitudes of the gains.
C                         Used to constrain the solutions.
C     CONFAC         R    Factor to multiply times the penalty term to
C                        increase or decrease its importance.
C     G(2,*)         R    Complex antenna gains to be applied.
C   Output:
C     S             R    Value of function being minimized, chi-squares
C                        sum plus penalty terms.
C     PENSUM        R    Penalty term added
C-----------------------------------------------------------------------
      DOUBLE PRECISION X(*), STEP(*), W, S, CHI2, PENSUM, AMP
      REAL      WT(*), GAERR(*), CONFAC, C, PH
      INTEGER   IS(*), JS(*), IA(*), NUMBL, MODE, I, NTD, K, J,
     *   PRTLV
      REAL      VOBS(2,*), G(2,*), Z(2)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      PH = 0.0
      AMP = 1.0
      PENSUM = 0.0D0
      DO 10 I = 1,NTD
         K = IA(I)
         IF (MODE.EQ.0.OR.MODE.EQ.3) AMP = X(NTD+I-1) - W *
     *      STEP(NTD+I-1)
         IF (I.GT.1) PH = X(I-1) - W * STEP(I-1)
         G(1,K) = AMP * COS (PH)
         G(2,K) = AMP * SIN (PH)
         IF (MODE.EQ.3) PENSUM = PENSUM +
     *      ((AMP - 1.0D0) / GAERR(K))**2
 10      CONTINUE
      CHI2 = 0.0D0
      DO 20 K = 1,NUMBL
         IF (WT(K).GT.0.) THEN
            I = IS(K)
            J = JS(K)
            Z(1) = VOBS(1,K) - (G(1,I)*G(1,J) + G(2,I)*G(2,J))
            Z(2) = VOBS(2,K) - (G(1,J)*G(2,I) - G(2,J)*G(1,I))
            CHI2 = CHI2 + WT(K) * (Z(1)*Z(1) + Z(2)*Z(2))
            END IF
 20      CONTINUE
      C = 0.1 * CONFAC
      S = CHI2
      IF (MODE.EQ.3) S = S + C * (PENSUM - NTD)**2
      IF (PRTLV.GE.2) THEN
         WRITE (MSGTXT,1020) W, S
         CALL MSGWRT (3)
         WRITE (MSGTXT,1021)  CHI2, PENSUM
         IF (MODE.EQ.3) CALL MSGWRT (3)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT (' NEVAL: Stepsize =',1PE15.5,', function =',E15.5)
 1021 FORMAT (' NEVAL: Chi**2 =',1PE15.5,', penalty =',E15.5)
      END
