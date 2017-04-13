      SUBROUTINE DTCALS (JSOU, JANT, JSUBA, JFQ, JIF, JNDXCE, TIME,
     *   DELAY, DPHASE, DRATE, DACCEL, DFEED, DGAIN, DTSYS, DAMPL,
     *   DGERR)
C-----------------------------------------------------------------------
C! Compute simulated calibration errors at a given time
C# UV Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2007, 2012
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
C   Compute simulated calibration errors at a given time
C   Input:
C      JSOU    I        Source index
C      JANT    I        Antenna index
C      JSUBA   I        Subarray index
C      JFQ     I        Freqid. index
C      JIF     I        IF number.
C      JNDXCE  I        Cal. error index
C      TIME    R        Time (in days wrt ref. date)
C   Output:
C      DELAY   D(2)     Group delay (RCP, LCP) (seconds)
C      DPHASE  D(2)     Phase delay (RCP, LCP) (radians)
C      DRATE   D(2)     Phase rate (RCP, LCP) rad/s.
C      DACCEL  D(2)     Phase acceleration (RCP, LCP) rad/s/s.
C      DFEED   D(2,2)   Feed D-terms (Re,Im; RCP,LCP)
C      DGAIN   D(2)     Point source sensitivity (RCP, LCP) (K/Jy)
C      DTSYS   D(2)     System temperature (RCP, LCP) (K)
C      DAMPL   D(2)     Square root (DGAIN * DTSYS)
C      DGERR   D(2)     Multiplicative gain error factor (RCP,LCP)
C----------------------------------------------------------------------
      DOUBLE PRECISION DELAY(2), DPHASE(2), DRATE(2), DACCEL(2),
     *   DFEED(2,2), DGAIN(2), DTSYS(2), DAMPL(2), DGERR(2)
      REAL      TIME
      INTEGER   JSOU, JANT, JSUBA, JFQ, JIF, JNDXCE
C
      INTEGER   MXCHEB
      PARAMETER (MXCHEB = 100)
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DTPM.INC'
      DOUBLE PRECISION DCHEB(MXCHEB), DCHEBD(MXCHEB,2), DK(6), DA, DB,
     *   DX, DAF, DPF, DTIME, DJUNK, DRA, DDEC, DC
      REAL      PARM(MAXERP)
      INTEGER   ITYPE, J, N, IRET, K, IROUND
C----------------------------------------------------------------------
C                                       Initialization
      DTIME = TIME
      CALL DFILL (2, 0.0D0, DELAY)
      CALL DFILL (2, 0.0D0, DPHASE)
      CALL DFILL (2, 0.0D0, DRATE)
      CALL DFILL (2, 0.0D0, DACCEL)
      CALL DFILL (4, 0.0D0, DFEED)
      CALL DFILL (2, 1.0D0, DGAIN)
      CALL DFILL (2, 0.0D0, DTSYS)
      CALL DFILL (2, 0.0D0, DAMPL)
      CALL DFILL (2, 1.0D0, DGERR)
C                                       general defaults
      IF (JNDXCE.LE.0) THEN
         IF (DEFGAN(JIF).GT.0.0) THEN
            DGAIN(1) = DEFGAN(JIF)
            DGAIN(2) = DEFGAN(JIF)
            END IF
         IF (DEFSYS(JIF).GT.0.0) THEN
            DTSYS(1) = DEFSYS(JIF)
            DTSYS(2) = DEFSYS(JIF)
            END IF
         DAMPL(1) = SQRT (DTSYS(1) * DGAIN(1))
         DAMPL(2) = SQRT (DTSYS(2) * DGAIN(2))
      ELSE
C                                       Loop over Stokes
         DO 900 J = 1, 2
C======================================================================
C                                       Delay
            CALL RCOPY (MAXERP, RDTDEL(1,JIF,J,JNDXCE), PARM)
            ITYPE = IROUND (PARM(1))
C                                       Case delay_type of:
C                                       1: Chebyshev polynomial
            IF (ITYPE.EQ.1) THEN
C                                       Order, period.
               N = IROUND (PARM(2))
               DA = 0.0D0
               DB = PARM(3)
               DX = MOD (TIME, PARM(3))
C                                       Compute polynomial value.
               CALL CHEBY (DA, DB, DX, DCHEB, N, IRET)
               IF (IRET.NE.0)  GO TO 900
C                                       Scale and add offset,
C                                       convert to sec
               DELAY(J) = (DCHEB(N+1) * PARM(4) + PARM(5)) * 1.0D-9
C                                       2: Perturbed orbit error
            ELSE IF (ITYPE.EQ.2) THEN
               CALL DCOPY (6, DTORB(1,JANT,JSUBA), 1, DK, 1)
C                                       Convert to radians
               DRA = DTSRA(JSOU) * DG2RAD
               DDEC = DTSDEC(JSOU) * DG2RAD
               DO 100 K = 3,6
                  DK(K) = DK(K) * DG2RAD
100               CONTINUE
C
               CALL ORBERR (DK(1), DK(2), DK(3), DK(4), DK(5), DK(6),
     *            DTIME, DTFREQ(JIF,JFQ), DDEC, DRA, PARM(2), PARM(3),
     *            PARM(4), PARM(5), DELAY(J), DJUNK)
               END IF
C======================================================================
C                                       Phase, rate and acceleration
            CALL RCOPY (MAXERP, RDTPHS(1,JIF,J,JNDXCE), PARM)
            ITYPE = IROUND (PARM(1))
C                                       Case phase_type of:
C                                       1: Chebyshev polynomial
            IF (ITYPE.EQ.1) THEN
C                                       Order, period.
               N = IROUND (PARM(2))
               DA = 0.0
               DB = PARM(3)
               DX = MOD (TIME, PARM(3))
C                                       Compute polynomial value.
               CALL CHEBY (DA, DB, DX, DCHEB, N, IRET)
               IF (IRET.NE.0) GO TO 900
C                                       Scale and add offset,
C                                       covert to rad.
               DPHASE(J) = (DCHEB(N+1) * PARM(4) + PARM(5)) * DG2RAD
C                                       Compute rate and acceleration
               CALL CHEBYD (DA, DB, DX, DCHEBD, MXCHEB, N, IRET)
               IF (IRET.NE.0) GO TO 900
C                                       Rate rad/s
               DC = 2.0D0 / (PARM(3) * 86400.0)
               DRATE(J) = DC * DCHEBD(N+1,1) * PARM(4) * DG2RAD
C                                       Acceleration rad/s/s
               DACCEL(J) = DC * DC * DCHEBD(N+1,2) * PARM(4) * DG2RAD
C                                       2: Perturbed orbit error
C                                       Rate, acceleration: NYI
            ELSE IF (ITYPE.EQ.2) THEN
               CALL DCOPY (6, DTORB(1,JANT,JSUBA), 1, DK, 1)
C                                       Convert to radians
               DRA = DTSRA(JSOU) * DG2RAD
               DDEC = DTSDEC(JSOU) * DG2RAD
               DO 200 K = 3,6
                  DK(K) = DK(K) * DG2RAD
200               CONTINUE
C
               CALL ORBERR (DK(1), DK(2), DK(3), DK(4), DK(5), DK(6),
     *            DTIME, DTFREQ(JIF,JFQ), DDEC, DRA, PARM(2), PARM(3),
     *            PARM(4), PARM(5), DJUNK, DPHASE(J))
               END IF
C=====================================================================
C                                       Feed D-terms
            CALL RCOPY (MAXERP, RDTFED(1,JIF,J,JNDXCE), PARM)
            ITYPE = IROUND (PARM(1))
C                                       Case feed_type of:
C                                       1: Const. D-terms
            IF (ITYPE.EQ.1) THEN
C                                       Amplitude, phase of D-term
               DAF = PARM(2)
               DPF = PARM(3) * DG2RAD
C                                       Convert to (Re,Im)
               DFEED(1,J) = DAF * COS (DPF)
               DFEED(2,J) = DAF * SIN (DPF)
               END IF
C======================================================================
C                                       Point source sensitivity
C                                       (gain in K/Jy)
            CALL RCOPY (MAXERP, RDTGAN(1,JIF,J,JNDXCE), PARM)
            ITYPE = IROUND (PARM(1))
C                                       Case gain_type of:
C                                       1: Constant value.
            IF (ITYPE.EQ.1) THEN
               DGAIN(J) = PARM(2)
            ELSE IF (DEFGAN(JIF).GT.0.0) THEN
               DGAIN(J) = DEFGAN(JIF)
               END IF
C======================================================================
C                                       Tsys
            CALL RCOPY (MAXERP, RDTTSY(1,JIF,J,JNDXCE), PARM)
            ITYPE = IROUND (PARM(1))
C                                       Case Tsys_type of:
C                                       1: Chebyshev polynomial
C                                          (absolute value)
            IF (ITYPE.EQ.1) THEN
C                                       Order, period.
               N = IROUND (PARM(2))
               DA = 0.0
               DB = PARM(3)
               DX = MOD (TIME, PARM(3))
C                                       Compute polynomial value.
               CALL CHEBY (DA, DB, DX, DCHEB, N, IRET)
               IF (IRET.NE.0) GO TO 900
C                                       Scale and add offset
               DTSYS(J) = ABS (DCHEB(N+1)) * PARM(4) + PARM(5)
C                                       default
            ELSE IF (DEFSYS(JIF).GT.0.0) THEN
               DTSYS(J) = DEFSYS(JIF)
               END IF
C======================================================================
C                                       Gain error factor
            CALL RCOPY (MAXERP, RDTGER(1,JIF,J,JNDXCE), PARM)
            ITYPE = IROUND (PARM(1))
C                                       Case gain_err_type of:
C                                       1: Chebyshev polynomial
            IF (ITYPE.EQ.1) THEN
C                                       Order, period
               N = IROUND (PARM(2))
               DA = 0.0
               DB = PARM(3)
               DX = MOD (TIME, PARM(3))
C                                       Compute polynomial value
               CALL CHEBY (DA, DB, DX, DCHEB, N, IRET)
               IF (IRET.NE.0) GO TO 900
C                                       Scale and add offset
               DGERR(J) = DCHEB(N+1) * PARM(4) + PARM(5)
C                                       Enforce positivity
               DGERR(J) = MAX (DGERR(J), 0.0D0)
               END IF
C======================================================================
C                                       Set amplitude gain factor
            DAMPL(J) = SQRT (DTSYS(J) * DGAIN(J))
900         CONTINUE
         END IF
C                                       Exit
999   RETURN
      END
