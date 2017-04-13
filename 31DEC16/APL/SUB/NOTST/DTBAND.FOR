      SUBROUTINE DTBAND (INDX, JIF, TIME, DSHIFT, BAND)
C-----------------------------------------------------------------------
C! Compute simulated bandpass response
C# UV Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2007, 2015
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
C   Compute simulated bandpass response
C   Inputs:
C      INDX    I      Index in cal. error arrays for this (ant, fqid)
C      JIF     I      IF number.
C      TIME    R      Time (days since reference day).
C      DSHIFT  D      Frequency shift (channels)
C   Output:
C      BAND    C(2,*) Complex bandpass response (RCP,LCP).
C----------------------------------------------------------------------
      DOUBLE PRECISION DSHIFT
      COMPLEX BAND(2,*)
      REAL TIME
      INTEGER INDX, JIF
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DTPM.INC'
      INCLUDE 'INCS:DDCH.INC'
      LOGICAL WVAR
      CHARACTER LTYPE*8
      COMPLEX ZSCALE
      DOUBLE PRECISION DA, DB, DX, DCHEB(100), DAMP, DFACT, DPHS,
     *   DRE, DIM, DSAMP, DSUMR, DSUMI, DTVAR, DFW
      REAL PARM(MAXERP), XRE(MAXCIF), XIM(MAXCIF), A, B
      INTEGER J, K, NBUTW, NCHEB, ITYPE, NMAX, IRET
      INTEGER IROUND
      SAVE XRE, XIM
C----------------------------------------------------------------------
C                                       Loop over (RCP, LCP)
      DO 500 J = 1, 2
C                                       Initialize
         DO 50 K = 1, NDTCHN
            BAND(J,K) = (1.0, 0.0)
50          CONTINUE
C                                       Compute BP response
         CALL RCOPY (MAXERP, RDTBP(1,JIF,J,INDX), PARM)
         ITYPE = IROUND (PARM(1))
C                                       Case BP_type of:
C                                       1: Butterworth amplitude,
C                                          Chebyshev phase
         IF (ITYPE.EQ.1) THEN
C                                       Butterworth order
            NBUTW = IROUND (PARM(2))
C                                       Chebyshev order (phase)
            NCHEB = IROUND (PARM(3))
C                                       Compute sinusoidal time
C                                       and frequency variability
C                                       factor if requested
            WVAR = ((PARM(6).GT.0.0).AND.(PARM(7).GT.0.0)
     *         .AND.(PARM(8).GT.0.0).AND.(PARM(9).GT.0.0))
            IF (WVAR) THEN
               DFW = PARM(8) * SIN (TWOPI * TIME / PARM(9)) + 1.0D0
               DFW = DFW * PARM(7)
               END IF
C                                       Loop over freq. channel
            DSUMR = 0.0D0
            DSUMI = 0.0D0
            DSAMP = 0.0D0
            DO 200 K = 1, NDTCHN
C                                       BP amplitude
               DFACT = (K - DSHIFT) / DBLE (NDTCHN)
               DAMP =  1.0 / SQRT (1.0 + DFACT ** (2 * NBUTW))
C                                       BP phase
               DA = 0.0D0
               DB = NDTCHN
               DX = K - DSHIFT
               CALL CHEBY (DA, DB, DX, DCHEB, NCHEB, IRET)
               IF (IRET.NE.0) GO TO 200
               DPHS = DCHEB(NCHEB+1) * PARM(4) * DG2RAD
C                                       Time/freq. variability
               IF (WVAR) THEN
                  DTVAR = PARM(6) * SIN (TWOPI * K / DFW) + 1.0D0
                  DAMP = DAMP * DTVAR
                  DPHS = DPHS * DTVAR
                  END IF
C                                       Re, Im
               DRE = DAMP * COS (DPHS)
               DIM = DAMP * SIN (DPHS)
               BAND(J,K) = CMPLX (DRE, DIM)
               DSAMP = DSAMP + DAMP
               DSUMR = DSUMR + COS (DPHS)
               DSUMI = DSUMI + SIN (DPHS)
200            CONTINUE
C                                       Normalise to unit amplitude
C                                       and zero mean phase.
            IF (PARM(5).EQ.1) THEN
               DPHS = ATAN2 (DSUMI, DSUMR)
               ZSCALE = CMPLX (COS (DPHS), -SIN (DPHS))
               ZSCALE = ZSCALE * NDTCHN / DSAMP
               DO 220 K = 1, NDTCHN
                  BAND(J,K) = BAND(J,K) * ZSCALE
220               CONTINUE
               END IF
            END IF
C                                       2: Linear slope in time
C                                       and frequency
         IF (ITYPE.EQ.2) THEN
            DO 250 K = 1, NDTCHN
               DX = (K - DSHIFT) / NDTCHN
               DAMP = 1.0 - PARM(2) * DX - PARM(3) * TIME
               DPHS = PARM(2) * DX + PARM(3) * TIME
               DPHS = DPHS * 0.5
               DRE = DAMP * COS (DPHS)
               DIM = DAMP * SIN (DPHS)
               BAND(J,K) = CMPLX (DRE, DIM)
250            CONTINUE
            END IF
C                                       3: Polynomial BP (A&P)
         IF (ITYPE.EQ.3) THEN
            LTYPE = 'CHEB_AP'
            A = 1.0
            B = NDTCHN
            CALL BPCOEF (LTYPE, PARM(2), PARM(6), 1, 1, 4, FBLANK,
     *         DSHIFT, XRE, XIM, 1, 1, 1, NDTCHN, A, B, 0,
     *         .FALSE., DCHEB, 100, NMAX, .FALSE., IRET)
            IF (IRET.EQ.0) THEN
               DO 300 K = 1, NDTCHN
                  BAND(J,K) = CMPLX (XRE(K), XIM(K))
300               CONTINUE
               END IF
            END IF
C
500      CONTINUE
      RETURN
      END
