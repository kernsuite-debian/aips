      SUBROUTINE VLBPOL
C-----------------------------------------------------------------------
C! Fills polarization correction table for VLBI polarization data
C# UV EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2007, 2010, 2012, 2017-2018
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
C   Fills polarization correction table from info in AN table for VLBI
C   polarization data.
C   Note:the elements of POLCAL are adjusted such that when DATPOL
C   rotates the values of RL and LR by the parallactic + Faraday
C   rotation angles that the result will be correct.
C   Inputs:
C      PANGLE   R(*)     Parallactic angles of the antennas (Rad)
C   Input from common: (DSEL.INC)
C      LAMBDA   R(*)     Wavelength of each channel and IF (m)
C      NLAMDA   I        Number channels in 1-d LAMBDA
C      IFR      R(*)     Faraday rotation of each antenna (rad/m**2)
C   Output to common DSEL.INC
C      POLCAL   R(2,*)   Polarization correction Values in order:
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
      INTEGER   IIF, IANT1, IANT2, LIMIT, BLNDX, LIMIT2, BLPNT, LENTRY,
     *   II, LOFF, IC1, IC2, ICH, KR1, KR2, KL1, KL2, IFQ
      REAL      ANG1, ANG2, XSUM, XDIF, SXSUM, CXSUM, SXDIF, CXDIF,
     *   C2X1, S2X1, C2X2, S2X2, C2XDIF, S2XDIF
      COMPLEX   DR1, DR2, DL1, DL2, BLMAT(4,4), CD, CF
      LONGINT   POLPTR
      LOGICAL   FLAGED
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DPDC.INC'
C-----------------------------------------------------------------------
      CALL RFILL (32, 0.0, BLMAT)
      DO 30 II = 1,4
         BLMAT(II,II) = CMPLX (1.0, 0.0)
 30      CONTINUE
      IF (PDVER.GT.0) THEN
         IC1 = BCHAN
         IC2 = ECHAN
      ELSE
         IC1 = 1
         IC2 = 1
         IFQ = (BCHAN + ECHAN) / 2
         END IF
C                                       Compute correction matrices.
      LENTRY = PCLIF * PCLCH * 32
      LIMIT = NSTNS - 1
      DO 500 IANT1 = 1,LIMIT
         LIMIT2 = IANT1 + 1
         DO 490 IANT2 = LIMIT2,NSTNS
C                                       Set baseline index
            BLNDX = ((IANT1-1)*NSTNS) - (((IANT1-1)*IANT1)/2) + IANT2
C                                       Loop over IF
            DO 120 IIF = BIF,EIF
               BLPNT = LENTRY * (BLNDX-1) + (IIF-BIF)*32*PCLCH
               POLPTR = PPOLCL + BLPNT + 1 - 32
               IF (PDVER.GT.0) THEN
                  KR1 = (4 * PDNUMF * PDNUMI) * (IANT1 - 1) +
     *               2 * PDNUMF * (IIF - 1) + 2 * (BCHAN -1) + 1
                  KR2 = (4 * PDNUMF * PDNUMI) * (IANT2 - 1) +
     *               2 * PDNUMF * (IIF - 1) + 2 * (BCHAN -1) + 1
                  KL1 = KR1 + (2 * PDNUMF * PDNUMI)
                  KL2 = KR2 + (2 * PDNUMF * PDNUMI)
                  END IF
               DO 110 ICH = IC1,IC2
                  IF (PDVER.GT.0) IFQ = ICH
                  POLPTR = POLPTR + 32
                  FLAGED = .FALSE.
C                                       D-terms: continuum
                  IF (PDVER.LE.0) THEN
                     IF ((STNELP(1,IIF,IANT1).EQ.FBLANK) .OR.
     *                  (STNELP(2,IIF,IANT1).EQ.FBLANK) .OR.
     *                  (STNELP(1,IIF,IANT2).EQ.FBLANK) .OR.
     *                  (STNELP(2,IIF,IANT2).EQ.FBLANK) .OR.
     *                  (STNORI(1,IIF,IANT1).EQ.FBLANK) .OR.
     *                  (STNORI(2,IIF,IANT1).EQ.FBLANK) .OR.
     *                  (STNORI(1,IIF,IANT2).EQ.FBLANK) .OR.
     *                  (STNORI(2,IIF,IANT2).EQ.FBLANK)) THEN
                        FLAGED = .TRUE.
                     ELSE
                        DR1 = CMPLX (STNELP(1,IIF,IANT1),
     *                     STNORI(1,IIF,IANT1))
                        DL1 = CMPLX (STNELP(2,IIF,IANT1),
     *                     STNORI(2,IIF,IANT1))
                        DR2 = CMPLX (STNELP(1,IIF,IANT2),
     *                     STNORI(1,IIF,IANT2))
                        DL2 = CMPLX (STNELP(2,IIF,IANT2),
     *                     STNORI(2,IIF,IANT2))
                        END IF
                  ELSE
                     IF ((DTERMS(PDTERM+KR1).EQ.FBLANK) .OR.
     *                  (DTERMS(PDTERM+KR1+1).EQ.FBLANK) .OR.
     *                  (DTERMS(PDTERM+KL1).EQ.FBLANK) .OR.
     *                  (DTERMS(PDTERM+KL1+1).EQ.FBLANK) .OR.
     *                  (DTERMS(PDTERM+KR2).EQ.FBLANK) .OR.
     *                  (DTERMS(PDTERM+KR2+1).EQ.FBLANK) .OR.
     *                  (DTERMS(PDTERM+KL2).EQ.FBLANK) .OR.
     *                  (DTERMS(PDTERM+KL2+1).EQ.FBLANK)) THEN
                        FLAGED = .TRUE.
                     ELSE
                        DR1 = CMPLX (DTERMS(PDTERM+KR1),
     *                     DTERMS(PDTERM+KR1+1))
                        DL1 = CMPLX (DTERMS(PDTERM+KL1),
     *                     DTERMS(PDTERM+KL1+1))
                        DR2 = CMPLX (DTERMS(PDTERM+KR2),
     *                     DTERMS(PDTERM+KR2+1))
                        DL2 = CMPLX (DTERMS(PDTERM+KL2),
     *                     DTERMS(PDTERM+KL2+1))
                        END IF
                     KR1 = KR1 + 2
                     KL1 = KL1 + 2
                     KR2 = KR2 + 2
                     KL2 = KL2 + 2
                     END IF
C                                       D terms: line
C                                       Paralactic angle, Faraday
C                                       rotation terms
C                                       NOTE: DATPOL will flag the data
C                                       if either IFR is blank so we
C                                       just ignore it here.
                  LOFF = (IIF - 1) * NLAMDA + IFQ
                  IF (IFR(IANT1).NE.FBLANK) THEN
                     ANG1 = PANGLE(IANT1) + LAMBDA(LOFF)**2 * IFR(IANT1)
                  ELSE
                     ANG1 = PANGLE(IANT1)
                     END IF
                  IF (IFR(IANT2).NE.FBLANK) THEN
                     ANG2 = PANGLE(IANT2) + LAMBDA(LOFF)**2 * IFR(IANT2)
                  ELSE
                     ANG2 = PANGLE(IANT2)
                     END IF
                  XSUM = ANG1 + ANG2
                  XDIF = ANG1 - ANG2
                  CXSUM = COS (XSUM)
                  SXSUM = SIN (XSUM)
                  CXDIF = COS (XDIF)
                  SXDIF = SIN (XDIF)
C                                       Linear approximation
                  IF ((MOD(DOPOL,10).LE.1) .OR. (MOD(DOPOL,10).EQ.6))
     *               THEN
                     IF (FLAGED) THEN
                        BLMAT(3,1) = CMPLX (FBLANK, 0.0)
                        BLMAT(3,2) = CMPLX (FBLANK, 0.0)
                        BLMAT(3,3) = CMPLX (FBLANK, 0.0)
C                                       2nd cross.
                        BLMAT(4,1) = CMPLX (FBLANK, 0.0)
                        BLMAT(4,2) = CMPLX (FBLANK, 0.0)
                        BLMAT(4,4) = CMPLX (FBLANK, 0.0)
C                                       Add correction terms:
                     ELSE
C                                       1st cross.
                        BLMAT(3,1) = -CONJG (DL2) * CMPLX (CXDIF,-SXDIF)
                        BLMAT(3,2) = -DR1 * CMPLX (CXDIF, SXDIF)
                        BLMAT(3,3) = CMPLX (CXSUM, -SXSUM)
C                                       2nd cross.
                        BLMAT(4,1) = -DL1 * CMPLX (CXDIF, -SXDIF)
                        BLMAT(4,2) = -CONJG (DR2) * CMPLX (CXDIF, SXDIF)
                        BLMAT(4,4) = CMPLX (CXSUM, SXSUM)
                        END IF
C                                       Full 2nd order correction
                  ELSE
                     IF (FLAGED) THEN
                        CALL RFILL (32, FBLANK, BLMAT)
                     ELSE
                        C2X1 = COS (2.0 * ANG1)
                        S2X1 = SIN (2.0 * ANG1)
                        C2X2 = COS (2.0 * ANG2)
                        S2X2 = SIN (2.0 * ANG2)
                        C2XDIF = COS (2.0 * XDIF)
                        S2XDIF = SIN (2.0 * XDIF)
                        CD = DR1 * DL1 + CONJG (DR2) * CONJG (DL2)
                        CF = 1.0 / (1.0 - 2.0 * CD)
C                                       1st par.
                        BLMAT(1,1) = CF * (1.0 - CD)
                        BLMAT(1,2) = CF * (DR1 * CONJG (DR2) *
     *                     CMPLX (C2XDIF, S2XDIF))
                        BLMAT(1,3) = CF * (-CONJG (DR2) *
     *                     CMPLX (C2X2, -S2X2))
                        BLMAT(1,4) = CF * (-DR1 * CMPLX (C2X1, S2X1))
C                                       2nd par..
                        BLMAT(2,1) = CF * (DL1 * CONJG (DL2) *
     *                     CMPLX (C2XDIF, -S2XDIF))
                        BLMAT(2,2) = CF * (1.0 - CD)
                        BLMAT(2,3) = CF * (-DL1 * CMPLX (C2X1, -S2X1))
                        BLMAT(2,4) = CF * (-CONJG (DL2) *
     *                     CMPLX (C2X2, S2X2))
C                                       1st cross.
                        BLMAT(3,1) = CF * (-CONJG (DL2) *
     *                     CMPLX (CXDIF, -SXDIF))
                        BLMAT(3,2) = CF * (-DR1 * CMPLX (CXDIF, SXDIF))
                        BLMAT(3,3) = CF * (1.0 - CD) *
     *                     CMPLX (CXSUM, -SXSUM)
                        BLMAT(3,4) = CF * (CONJG (DL2) * DR1 *
     *                     CMPLX (CXSUM, SXSUM))
C                                       2nd cross.
                        BLMAT(4,1) = CF * (-DL1 * CMPLX (CXDIF, -SXDIF))
                        BLMAT(4,2) = CF * (-CONJG (DR2) *
     *                     CMPLX (CXDIF, SXDIF))
                        BLMAT(4,3) = CF * (DL1 * CONJG (DR2) *
     *                     CMPLX (CXSUM, -SXSUM))
                        BLMAT(4,4) = CF * (1.0 - CD) *
     *                     CMPLX (CXSUM, SXSUM)
                        END IF
                     END IF
                  CALL RCOPY (32, BLMAT, POLCAL(POLPTR))
 110              CONTINUE
 120           CONTINUE
 490        CONTINUE
 500     CONTINUE
C                                       Exit
 999  RETURN
      END
