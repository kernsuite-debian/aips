      SUBROUTINE ORIPOL
C-----------------------------------------------------------------------
C! Fills polarization correction table for orientation-elip. model
C# UV EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2010, 2012, 2014, 2017
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
C   Fills polarization correction table for orientation-elip. model
C   Inputs:
C      PANGLE   R(*)   Parallactic angles of the antennas (Rad)
C   Input from common: (DSEL.INC)
C      LAMBDA   R(*)   Wavelength of each channel and IF (m)
C      NLAMDA   I      Number channels in 1-d LAMBDA
C      IFR      R(*)   Faraday rotation of each antenna (rad/m**2)
C      PREFA    I      Polarization reference antenna
C      POLPD    R(*)   'R-L' phase difference (rad)
C   Output:
C      IERR     I      Return error code, 0=>OK else error.
C                        1=table too small, 2=multiple subarrays,
C                        10 = unknown polarization parameterization,
C                        otherwise GETANT error.
C   Output to common DSEL.INC
C      POLCAL   R(2,*)   Polarization correction values in order:
C                        By baseline
C                           By IF (EIF-BIF+1)
C                              A 4x4 complex matrix to be multiplied by
C                                  the observed polarization vector
C                                  (RR,LL,RL,LR) to produce the
C                                  corrected data.
C   Indexing scheme: an entry defined by ant1<ant2 starts in element:
C      (((ant1-1)*numant-((ant1+1)*ant1)/2 + ant2) - 1) + 1
C-----------------------------------------------------------------------
C      REAL      PANGLE(*)
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DPDC.INC'
      INTEGER   IIF, IANT1, IANT2, LIMIT, BLNDX, LIMIT2, I, BLPNT,
     *   LENTRY, LDA, N, INFO, IPVT(4), JOB, LOFF, ICH, IC1, IC2, IFQ,
     *   KR, KL
      REAL      ANG
      DOUBLE PRECISION SR(MAXANT), DR(MAXANT), SL(MAXANT), DL(MAXANT),
     *   ROOT2, PD
      COMPLEX  PA(MAXANT), CPA(MAXANT), PR, PL, RS(MAXANT), RD(MAXANT),
     *   LS(MAXANT), LD(MAXANT), PPSTAR, PRREF, PLREF,  WORK(4), DET(2),
     *   BLMAT(4,4), PDROT, PDROTC
      LONGINT   POLPTR
C-----------------------------------------------------------------------
      ROOT2 = 1.0 / SQRT (2.0)
      IF (PDVER.GT.0) THEN
         IC1 = BCHAN
         IC2 = ECHAN
      ELSE
         IC1 = 1
         IC2 = 1
         IFQ = (BCHAN + ECHAN) / 2
         END IF
C                                       Extract parameters and
C                                       precompute some values.
C                                       Loop over IF
      LENTRY = PCLIF * PCLCH * 32
      LIMIT = NSTNS - 1
      DO 500 IIF = BIF,EIF
         DO 490 ICH = IC1,IC2
            IF (PDVER.GT.0) THEN
               IFQ = ICH
               DO 20 I = 1,NSTNS
                  KR = (4 * PDNUMF * PDNUMI) * (I - 1) +
     *               2 * PDNUMF * (IIF - 1) + 2 * (ICH -1) + 1
                  KL = KR + (2 * PDNUMF * PDNUMI)
C                                        After some trig manipulations
C                                        factorize the model into
C                                        antenna based factors.
                  SR(I) = COS (DTERMS(PDTERM+KR)) +
     *               SIN (DTERMS(PDTERM+KR))
                  DR(I) = COS (DTERMS(PDTERM+KR)) -
     *               SIN (DTERMS(PDTERM+KR))
                  SL(I) = COS (DTERMS(PDTERM+KL)) +
     *               SIN (DTERMS(PDTERM+KL))
                  DL(I) = COS (DTERMS(PDTERM+KL)) -
     *               SIN (DTERMS(PDTERM+KL))
                  PR    = CMPLX (COS (2.0*DTERMS(PDTERM+KR+1)),
     *               SIN (2.0*DTERMS(PDTERM+KR+1)))
                  PL    = CMPLX (COS (2.0*DTERMS(PDTERM+KL+1)),
     *               -SIN (2.0*DTERMS(PDTERM+KL+1)))
                  RS(I) = CMPLX (ROOT2*SR(I), 0.0D0)
                  RD(I) = CMPLX (ROOT2*DR(I), 0.0D0) * PR
                  LS(I) = CMPLX (ROOT2*SL(I), 0.0D0) * PL
                  LD(I) = CMPLX (ROOT2*DL(I), 0.0D0)
 20               CONTINUE
C                                       R-L phase difference
               KR = (4 * PDNUMF * PDNUMI) * (PREFA - 1) +
     *            2 * PDNUMF * (IIF - 1) + 2 * (ICH -1) + 1
               KL = KR + (2 * PDNUMF * PDNUMI)
               PD = POLPD(ICH+(IIF-1)*PDNUMF)
               PRREF = CMPLX (COS (DTERMS(PDTERM+KR+1)),
     *            SIN (DTERMS(PDTERM+KR+1)))
               PLREF = CMPLX (COS (DTERMS(PDTERM+KL+1)+PD),
     *            -SIN (DTERMS(PDTERM+KL+1)+PD))
            ELSE
               DO 30 I = 1,NSTNS
C                                        After some trig manipulations
C                                        factorize the model into
C                                        antenna based factors.
                  SR(I) = COS (STNELP(1,IIF,I)) + SIN (STNELP(1,IIF,I))
                  DR(I) = COS (STNELP(1,IIF,I)) - SIN (STNELP(1,IIF,I))
                  SL(I) = COS (STNELP(2,IIF,I)) + SIN (STNELP(2,IIF,I))
                  DL(I) = COS (STNELP(2,IIF,I)) - SIN (STNELP(2,IIF,I))
                  PR    = CMPLX (COS (2.0*STNORI(1,IIF,I)),
     *               SIN (2.0*STNORI(1,IIF,I)))
                  PL    = CMPLX (COS (2.0*STNORI(2,IIF,I)),
     *               -SIN (2.0*STNORI(2,IIF,I)))
                  RS(I) = CMPLX (ROOT2*SR(I), 0.0D0)
                  RD(I) = CMPLX (ROOT2*DR(I), 0.0D0) * PR
                  LS(I) = CMPLX (ROOT2*SL(I), 0.0D0) * PL
                  LD(I) = CMPLX (ROOT2*DL(I), 0.0D0)
 30               CONTINUE
C                                       R-L phase difference
               PD = POLPD(IIF)
               PRREF = CMPLX (COS (STNORI(1,IIF,PREFA)),
     *            SIN (STNORI(1,IIF,PREFA)))
               PLREF = CMPLX (COS (STNORI(2,IIF,PREFA)+PD),
     *            -SIN (STNORI(2,IIF,PREFA)+PD))
               END IF
            PDROT = CMPLX (COS (PD), SIN (PD))
            PDROTC = CMPLX (COS (PD), -SIN (PD))
C                                       Paralactic angle, Faraday
C                                       rotation terms
            LOFF = (IIF - 1) * NLAMDA + IFQ
            DO 40 I = 1,NSTNS
               IF (IFR(I).NE.FBLANK) THEN
                  ANG =  -(PANGLE(I) + LAMBDA(LOFF)**2 * IFR(I))
               ELSE
                  ANG = -PANGLE(I)
                  END IF
               PA(I) = CMPLX (COS (2.0*ANG), -SIN (2.0*ANG))
               CPA(I) = CONJG (PA(I))
 40            CONTINUE
C                                       Compute correction matrices.
            LENTRY = 32 * PCLCH * PCLIF
            LIMIT = NSTNS - 1
            DO 480 IANT1 = 1,LIMIT
               LIMIT2 = IANT1 + 1
               DO 470 IANT2 = LIMIT2,NSTNS
C                                       Set baseline index
                  BLNDX = ((IANT1-1)*NSTNS) - (((IANT1-1)*IANT1)/2) +
     *               IANT2
                  BLPNT = LENTRY * (BLNDX-1) + (IIF-BIF)*32*PCLCH + 1
     *               + (ICH-IC1) * 32
C                                       Matrix terms
                  BLMAT(1,1) = RS(IANT1) * CONJG(RS(IANT2))
                  BLMAT(1,2) = RD(IANT1) * CONJG(RD(IANT2)) * PA(IANT1)
     *               * CPA(IANT2)
                  BLMAT(1,3) = RS(IANT1) * CONJG(RD(IANT2)) * CPA(IANT2)
                  BLMAT(1,4) = RD(IANT1) * CONJG(RS(IANT2)) * PA(IANT1)
                  BLMAT(2,1) = LS(IANT1) * CONJG(LS(IANT2)) * CPA(IANT1)
     *               * PA(IANT2)
                  BLMAT(2,2) = LD(IANT1) * CONJG(LD(IANT2))
                  BLMAT(2,3) = LS(IANT1) * CONJG(LD(IANT2)) * CPA(IANT1)
                  BLMAT(2,4) = LD(IANT1) * CONJG(LS(IANT2)) * PA(IANT2)
                  PPSTAR = PRREF * CONJG (PLREF)
                  BLMAT(3,1) = PPSTAR * RS(IANT1) * CONJG(LS(IANT2))
     *               * PA(IANT2)
                  BLMAT(3,2) = PPSTAR * RD(IANT1) * CONJG(LD(IANT2))
     *               * PA(IANT1)
                  BLMAT(3,3) = PPSTAR * RS(IANT1) * CONJG(LD(IANT2))
                  BLMAT(3,4) = PPSTAR * RD(IANT1) * CONJG(LS(IANT2))
     *               * PA(IANT1) * PA(IANT2)
                  PPSTAR = PLREF * CONJG (PRREF)
                  BLMAT(4,1) = PPSTAR * LS(IANT1) * CONJG(RS(IANT2))
     *               * CPA(IANT1)
                  BLMAT(4,2) = PPSTAR * LD(IANT1) * CONJG(RD(IANT2))
     *               * CPA(IANT2)
                  BLMAT(4,3) = PPSTAR * LS(IANT1) * CONJG(RD(IANT2))
     *               * CPA(IANT1) * CPA(IANT2)
                  BLMAT(4,4) = PPSTAR * LD(IANT1) * CONJG(RS(IANT2))
C                                       Invert baseline-IF matrix.
                  LDA = 4
                  N = 4
C                                       Factorize matrix
                  CALL CGEFA (BLMAT, LDA, N, IPVT, INFO)
C                                       Check for singularity
                  IF (INFO.EQ.0) THEN
                     JOB = 1
                     CALL CGEDI (BLMAT, LDA, N, IPVT, DET, WORK, JOB)
C                                       Rotate RL, LR by PD
                     BLMAT(3,1) = PDROT * BLMAT(3,1)
                     BLMAT(3,2) = PDROT * BLMAT(3,2)
                     BLMAT(3,3) = PDROT * BLMAT(3,3)
                     BLMAT(3,4) = PDROT * BLMAT(3,4)
                     BLMAT(4,1) = PDROTC * BLMAT(4,1)
                     BLMAT(4,2) = PDROTC * BLMAT(4,2)
                     BLMAT(4,3) = PDROTC * BLMAT(4,3)
                     BLMAT(4,4) = PDROTC * BLMAT(4,4)
                  ELSE
C                                       Deal with singular matrix
                     WRITE (MSGTXT,1500) IANT1, IANT2, IIF, ICH
                     CALL MSGWRT (8)
C                                       Zero matrix
                     CALL RFILL (32, 0.0, BLMAT)
                     END IF
                  POLPTR = PPOLCL + BLPNT
                  CALL RCOPY (32, BLMAT, POLCAL(POLPTR))
 470              CONTINUE
 480           CONTINUE
 490        CONTINUE
 500     CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1500 FORMAT ('SINGULAR MATRIX ON',I3,' -',I3,' IF,CH=',I3,I6,
     *   ' WILL ZERO')
      END
