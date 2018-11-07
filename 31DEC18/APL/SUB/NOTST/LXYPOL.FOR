      SUBROUTINE LXYPOL (IERR)
C-----------------------------------------------------------------------
C! Fills polarization correction table for AT like linear polarization.
C# UV EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2010, 2012, 2014, 2017-2018
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
C   Fills polarization correction table from info in AN table for Linear
C   polarization feeds (XY).
C   Inputs:
C      PANGLE  R(*)      Parallactic angles of the antennas (Rad)
C   Output:
C      IERR    I         Return error code, 0=>OK else error.
C                        1=table too small, 2=multiple subarrays,
C                        10 = unknown polarization parameterization,
C                        otherwise GETANT error.
C   Output to common /SELCAL/:
C      PARTIM  R         Time of current parallactic angles. (-1.0E10)
C      PARSOU  I         Source ID for current parallactic angles. (-10)
C      POLCAL  R(2,*)    Polarization correction
C                        Values in order:
C                        By baseline
C                           By IF (EIF-BIF+1)
C                              A 4x4 complex matrix to be multiplied by
C                                  the observed polarization vector
C                                  (RR,LL,RL,LR) to produce the
C                                  corrected data.
C                       Indexing scheme: an entry defined by ant1<ant2
C                       starts in element:
C         (((ant1-1)*numant-((ant1+1)*ant1)/2 + ant2) - 1) + 1
C-----------------------------------------------------------------------
C      REAL      PANGLE(*)
      INTEGER   IERR
C
      INTEGER   IIF, IPOL, IANT1, IANT2, LIMIT, BLNDX, LIMIT2, BLPNT,
     *   IBLPNT, IPOFF(4,2), IP1, IP2, LENTRY, II, JJ, LDA, N, IPVT(4),
     *   JOB, INFO, ICH, IC1, IC2, KX1, KX2, KY1, KY2, IFQ, KK
      REAL    PDIF, PSUM, TDIF, TSUM, SPDIF, CPDIF, SPSUM, STDIF,
     *   STSUM, CTDIF, CTSUM, CPSUM, PA1, PA2, XMAT(2,16)
      COMPLEX   WORK(4), DET(2), DX1, DX2, DY1, DY2, BLMAT(4,4),
     *   DMAT(4,4), TMAT1(4,4), TMAT2(4,4), TMAT3(4,4)
      LONGINT   POLPTR
      LOGICAL   FLAGED
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DPDC.INC'
      EQUIVALENCE (DMAT, XMAT)
      DATA IPOFF /1,2,1,2, 1,2,2,1/
C     DATA ORIFAC /-1.0, 1.0/
C-----------------------------------------------------------------------
C                                       Initialize parallactic angle
C                                       time, source
      IERR = 0
      PA1 = -1.0E10
      PA2 = -1.0E10
      PARTIM = -1.0E10
      PARSOU = -10
C                                       Only 1 subarray
      IF (SUBARR.EQ.0) THEN
         IERR = 2
         MSGTXT = 'ERROR: CAN ONLY DO POLARIZATION CORRECTION FOR 1 '
     *      // 'SUBARRAY'
         GO TO 990
         END IF
      IF (PDVER.GT.0) THEN
         IC1 = BCHAN
         IC2 = ECHAN
      ELSE
         IC1 = 1
         IC2 = 1
         IFQ = (BCHAN + ECHAN) / 2
         END IF
C                                       Compute correction matrices.
      LENTRY = 32 * PCLCH * PCLIF
      LIMIT = NSTNS - 1
      DO 600 IANT1 = 1,LIMIT
         LIMIT2 = IANT1 + 1
         DO 590 IANT2 = LIMIT2,NSTNS
C                                       Set baseline index
            BLNDX = ((IANT1-1)*NSTNS) - (((IANT1-1)*IANT1)/2) + IANT2
C                                       Compute matrix to convert
C                                       XX,YY,XY,YX to RR,LL.RL,LR
C                                       Don't redo if within 1 deg.
            IF ((ABS ((PANGLE(IANT1)-PA1)).GT.0.017) .OR.
     *          (ABS ((PANGLE(IANT2)-PA2)).GT.0.017)) THEN
               CPSUM = COS (PANGLE(IANT1) + PANGLE(IANT2))
               CPDIF = COS (PANGLE(IANT1) - PANGLE(IANT2))
               SPSUM = SIN (PANGLE(IANT1) + PANGLE(IANT2))
               SPDIF = SIN (PANGLE(IANT1) - PANGLE(IANT2))
C                                       Matrix to convert from I etc to
C                                       XX,YY etc.
               TMAT1(1,1) = CMPLX (CPDIF, 0.0)
               TMAT1(2,1) = CMPLX (SPSUM, 0.0)
               TMAT1(3,1) = CMPLX (-CPSUM, 0.0)
               TMAT1(4,1) = CMPLX (0.0, -SPDIF)
               TMAT1(1,2) = CMPLX (CPDIF, 0.0)
               TMAT1(2,2) = CMPLX (-SPSUM, 0.0)
               TMAT1(3,2) = CMPLX (CPSUM, 0.0)
               TMAT1(4,2) = CMPLX (0.0, -SPDIF)
               TMAT1(1,3) = CMPLX (-SPDIF, 0.0)
               TMAT1(2,3) = CMPLX (-CPSUM, 0.0)
               TMAT1(3,3) = CMPLX (-SPSUM, 0.0)
               TMAT1(4,3) = CMPLX (0.0, -CPDIF)
               TMAT1(1,4) = CMPLX (SPDIF, 0.0)
               TMAT1(2,4) = CMPLX (-CPSUM, 0.0)
               TMAT1(3,4) = CMPLX (-SPSUM, 0.0)
               TMAT1(4,3) = CMPLX (0.0, CPDIF)
C                                       Invert
               LDA = 4
               N = 4
C                                       Factorize matrix
               CALL CGEFA (TMAT1, LDA, N, IPVT, INFO)
C                                       Check for singularity
               IF (INFO.EQ.0) THEN
                  JOB = 1
                  CALL CGEDI (TMAT1, LDA, N, IPVT, DET, WORK, JOB)
C                                       Deal with singular matrix
               ELSE
                  IERR = 9
                  WRITE (MSGTXT,1500) IANT1, IANT2, IIF
                  CALL MSGWRT (8)
                  GO TO 999
                  END IF
C                                       Matrix to convert I,Q,U and V to
C                                       RR,LL,RL,LR that would have been
C                                       observed including the effects
C                                       of phase calibration.
C                                       NOTE: CPSUM and SPSUM should be
C                                       the cosine and sine of twice the
C                                       parallactic angle of the
C                                       reference antenna used for phase
C                                       calibrations.
               CPSUM = COS (2.0*PANGLE(IANT1))
               SPSUM = SIN (2.0*PANGLE(IANT1))
               TMAT2(1,1) = CMPLX (0.5, 0.0)
               TMAT2(2,1) = CMPLX (0.0, 0.0)
               TMAT2(3,1) = CMPLX (0.0, 0.0)
               TMAT2(4,1) = CMPLX (0.5, 0.0)
               TMAT2(1,2) = CMPLX (0.5, 0.0)
               TMAT2(2,2) = CMPLX (0.0, 0.0)
               TMAT2(3,2) = CMPLX (0.0, 0.0)
               TMAT2(4,2) = CMPLX (-0.5, 0.0)
               TMAT2(1,3) = CMPLX (0.0, 0.0)
               TMAT2(2,3) = CMPLX (0.5*CPSUM, -0.5*SPSUM)
               TMAT2(3,3) = CMPLX (0.5*SPSUM, 0.5*CPSUM)
               TMAT2(4,3) = CMPLX (0.0, 0.0)
               TMAT2(1,4) = CMPLX (0.0, 0.0)
               TMAT2(2,4) = CMPLX (0.5*CPSUM, 0.5*SPSUM)
               TMAT2(3,4) = CMPLX (0.5*SPSUM, -0.5*CPSUM)
               TMAT2(4,4) = CMPLX (0.0, 0.0)
C                                       Multiply TMAT2 by TMAT1
               DO 60 II = 1,4
                  DO 50 JJ = 1,4
                     TMAT3(II,JJ) = TMAT2(1,JJ)*TMAT1(II,1) +
     *                  TMAT2(2,JJ)*TMAT1(II,2) +
     *                  TMAT2(3,JJ)*TMAT1(II,3) +
     *                  TMAT2(4,JJ)*TMAT1(II,4)
 50                  CONTINUE
 60               CONTINUE
               END IF
C                                       Loop over IF, channel
            DO 580 IIF = BIF,EIF
               BLPNT = LENTRY * (BLNDX-1) + (IIF-BIF)*32*PCLCH
               POLPTR = PPOLCL + BLPNT + 1 - 32
               IF (PDVER.GT.0) THEN
                  KX1 = (4 * PDNUMF * PDNUMI) * (IANT1 - 1) +
     *               2 * PDNUMF * (IIF - 1) + 2 * (BCHAN -1) + 1
                  KX2 = (4 * PDNUMF * PDNUMI) * (IANT2 - 1) +
     *               2 * PDNUMF * (IIF - 1) + 2 * (BCHAN -1) + 1
                  KY1 = KX1 + (2 * PDNUMF * PDNUMI)
                  KY2 = KX2 + (2 * PDNUMF * PDNUMI)
                  END IF
               DO 570 ICH = IC1,IC2
                  IF (PDVER.GT.0) IFQ = ICH
                  FLAGED = .FALSE.
                  POLPTR = POLPTR + 32
C                                       Linear approximation:
                  IF (SOLTYP.EQ.1) THEN
                     IF (PDVER.LE.0) THEN
                        IF ((STNELP(1,IIF,IANT1).EQ.FBLANK) .OR.
     *                     (STNELP(2,IIF,IANT1).EQ.FBLANK) .OR.
     *                     (STNELP(1,IIF,IANT2).EQ.FBLANK) .OR.
     *                     (STNELP(2,IIF,IANT2).EQ.FBLANK) .OR.
     *                     (STNORI(1,IIF,IANT1).EQ.FBLANK) .OR.
     *                     (STNORI(2,IIF,IANT1).EQ.FBLANK) .OR.
     *                     (STNORI(1,IIF,IANT2).EQ.FBLANK) .OR.
     *                     (STNORI(2,IIF,IANT2).EQ.FBLANK)) THEN
                           FLAGED = .TRUE.
                           DX1 = CMPLX (0.0, 0.0)
                           DY1 = CMPLX (0.0, 0.0)
                           DX2 = CMPLX (0.0, 0.0)
                           DY2 = CMPLX (0.0, 0.0)
                        ELSE
                           DX1 = CMPLX (STNELP(1,IIF,IANT1),
     *                        STNORI(1,IIF,IANT1))
                           DY1 = CMPLX (STNELP(2,IIF,IANT1),
     *                        STNORI(2,IIF,IANT1))
                           DX2 = CMPLX (STNELP(1,IIF,IANT2),
     *                        STNORI(1,IIF,IANT2))
                           DY2 = CMPLX (STNELP(2,IIF,IANT2),
     *                        STNORI(2,IIF,IANT2))
                           END IF
                     ELSE
                        IF ((DTERMS(PDTERM+KX1).EQ.FBLANK) .OR.
     *                     (DTERMS(PDTERM+KX1+1).EQ.FBLANK) .OR.
     *                     (DTERMS(PDTERM+KY1).EQ.FBLANK) .OR.
     *                     (DTERMS(PDTERM+KY1+1).EQ.FBLANK) .OR.
     *                     (DTERMS(PDTERM+KX2).EQ.FBLANK) .OR.
     *                     (DTERMS(PDTERM+KX2+1).EQ.FBLANK) .OR.
     *                     (DTERMS(PDTERM+KY2).EQ.FBLANK) .OR.
     *                     (DTERMS(PDTERM+KY2+1).EQ.FBLANK)) THEN
                           FLAGED = .TRUE.
                           DX1 = CMPLX (0.0, 0.0)
                           DY1 = CMPLX (0.0, 0.0)
                           DX2 = CMPLX (0.0, 0.0)
                           DY2 = CMPLX (0.0, 0.0)
                        ELSE
                           DX1 = CMPLX (DTERMS(PDTERM+KX1),
     *                        DTERMS(PDTERM+KX1+1))
                           DY1 = CMPLX (DTERMS(PDTERM+KY1),
     *                        DTERMS(PDTERM+KY1+1))
                           DX2 = CMPLX (DTERMS(PDTERM+KX2),
     *                        DTERMS(PDTERM+KX2+1))
                           DY2 = CMPLX (DTERMS(PDTERM+KY2),
     *                        DTERMS(PDTERM+KY2+1))
                           END IF
                        KX1 = KX1 + 2
                        KY1 = KY1 + 2
                        KX2 = KX2 + 2
                        KY2 = KY2 + 2
                        END IF
                     CALL RFILL (32, 0.0, DMAT)
                     DO 70 II = 1,4
                        DMAT(II,II) = (1.0, 0.0)
 70                     CONTINUE
C                                       Instrumental terms
                     IF (FLAGED) THEN
                        DMAT(3,1) = CMPLX (FBLANK, 0.0)
                        DMAT(3,2) = CMPLX (FBLANK, 0.0)
                        DMAT(4,1) = CMPLX (FBLANK, 0.0)
                        DMAT(4,2) = CMPLX (FBLANK, 0.0)
                     ELSE
                        DMAT(3,1) = (DX1 + CONJG (DY2))
                        DMAT(3,2) = DMAT(3,1)
                        DMAT(4,1) = (DY1 + CONJG (DX2))
                        DMAT(4,2) = DMAT(4,1)
                        END IF
                  ELSE
C                                       Orientation - ellipticity:
                     IBLPNT = 1
                     DO 100 IPOL = 1,4
                        IP1 = IPOFF(IPOL,1)
                        IP2 = IPOFF(IPOL,2)
C                                       Feed constants
                        IF (PDVER.LE.0) THEN
                           IF ((STNELP(IP1,IIF,IANT1).EQ.FBLANK) .OR.
     *                        (STNELP(IP2,IIF,IANT1).EQ.FBLANK) .OR.
     *                        (STNELP(IP1,IIF,IANT2).EQ.FBLANK) .OR.
     *                        (STNELP(IP2,IIF,IANT2).EQ.FBLANK) .OR.
     *                        (STNORI(IP1,IIF,IANT1).EQ.FBLANK) .OR.
     *                        (STNORI(IP2,IIF,IANT1).EQ.FBLANK) .OR.
     *                        (STNORI(IP1,IIF,IANT2).EQ.FBLANK) .OR.
     *                        (STNORI(IP2,IIF,IANT2).EQ.FBLANK)) THEN
                              FLAGED = .TRUE.
                           ELSE
                              PDIF = STNORI(IP1,IIF,IANT1) -
     *                           STNORI(IP2,IIF,IANT2)
                              PSUM = STNORI(IP1,IIF,IANT1) +
     *                           STNORI(IP2,IIF,IANT2)
                              TDIF = STNELP(IP1,IIF,IANT1) -
     *                           STNELP(IP2,IIF,IANT2)
                              TSUM = STNELP(IP1,IIF,IANT1) +
     *                           STNELP(IP2,IIF,IANT2)
                              END IF
                        ELSE
                           IF (IP1.EQ.1) THEN
                              KK = KX1
                           ELSE
                              KK = KY1
                              END IF
                           IF (IP2.EQ.1) THEN
                              JJ = KX2
                           ELSE
                              JJ = KY2
                              END IF
                           IF ((DTERMS(PDTERM+KK).EQ.FBLANK) .OR.
     *                        (DTERMS(PDTERM+KK+1).EQ.FBLANK) .OR.
     *                        (DTERMS(PDTERM+JJ).EQ.FBLANK) .OR.
     *                        (DTERMS(PDTERM+JJ+1).EQ.FBLANK) .OR.
     *                        (DTERMS(PDTERM+KK).EQ.FBLANK) .OR.
     *                        (DTERMS(PDTERM+KK+1).EQ.FBLANK) .OR.
     *                        (DTERMS(PDTERM+JJ).EQ.FBLANK) .OR.
     *                        (DTERMS(PDTERM+JJ+1).EQ.FBLANK)) THEN
                              FLAGED = .TRUE.
                           ELSE
                              PDIF = DTERMS(PDTERM+KK+1) -
     *                           DTERMS(PDTERM+JJ+1)
                              PSUM = DTERMS(PDTERM+KK+1) +
     *                           DTERMS(PDTERM+JJ+1)
                              TDIF = DTERMS(PDTERM+KK) -
     *                           DTERMS(PDTERM+JJ)
                              TSUM = DTERMS(PDTERM+KK) +
     *                           DTERMS(PDTERM+JJ)
                              END IF
                           END IF
                        IF (.NOT.FLAGED) THEN
                           SPDIF = 0.5 * SIN (PDIF)
                           CPDIF = 0.5 * COS (PDIF)
                           SPSUM = 0.5 * SIN (PSUM)
                           CPSUM = 0.5 * COS (PSUM)
                           STDIF = SIN (TDIF)
                           CTDIF = COS (TDIF)
                           STSUM = SIN (TSUM)
                           CTSUM = COS (TSUM)
C                                       Matrix terms
                           XMAT(1,IBLPNT) =   CPDIF * (CTDIF - STSUM)
                           XMAT(2,IBLPNT) =   SPDIF * (STSUM - CTDIF)
                           XMAT(1,IBLPNT+4) = CPDIF * (CTDIF + STSUM)
                           XMAT(2,IBLPNT+4) = SPDIF * (CTDIF + STSUM)
                           XMAT(1,IBLPNT+8) = CPSUM * (CTSUM - STDIF)
                           XMAT(2,IBLPNT+8) = SPSUM * (STDIF - CTSUM)
                           XMAT(1,IBLPNT+12) = CPSUM * (CTSUM +
     *                        STDIF)
                           XMAT(2,IBLPNT+12) = SPSUM * (STDIF +
     *                        CTSUM)
                           END IF
                        IBLPNT = IBLPNT + 1
 100                    CONTINUE
C                                       Invert baseline-IF matrix.
                     IF (.NOT.FLAGED) THEN
                        LDA = 4
                        N = 4
C                                       Factorize matrix
                        CALL CGEFA (DMAT, LDA, N, IPVT, INFO)
C                                       Check for singularity
                        IF (INFO.EQ.0) THEN
                           JOB = 1
                           CALL CGEDI (DMAT, LDA, N, IPVT, DET, WORK,
     *                        JOB)
C                                       Deal with singular matrix
                        ELSE
                           WRITE (MSGTXT,1500) IANT1, IANT2, IIF
                           CALL MSGWRT (8)
C                                       Zero matrix
                           CALL RFILL (32, FBLANK, DMAT)
                           END IF
C                                       Add corrections for
C                                       orientation angle removed in
C                                       calibration
C                    DO 120 IPOL = 1,4
C                       IP1 = IPOFF(IPOL,1)
C                       IP2 = IPOFF(IPOL,2)
C                       PDIF = ORIFAC(IP1) * STNORI(IP1,IIF,IANT1) -
C    *                     ORIFAC(IP2) * STNORI(IP2,IIF,IANT2)
C                       CPDIF = COS (PDIF)
C                       SPDIF = SIN (PDIF)
C                                       Correct column of matrix
C                       JP = BLPNT + (IPOL - 1) * 4
C                       DO 110 LOOP = 1,4
C                          TEMPR = POLCAL(1,JP)
C                          TEMPI = POLCAL(2,JP)
C                          POLCAL(1,JP) = TEMPR * CPDIF - TEMPI * SPDIF
C                          POLCAL(2,JP) = TEMPR * SPDIF + TEMPI * CPDIF
C                          JP = JP + 1
C110                       CONTINUE
C120                    CONTINUE
C                                       End of solution type specific
C                                       parameterization.
                        END IF
                     END IF
C                                       Multiply TMAT3 by DMAT to
C                                       produce properly modified
C                                       RR,LL,RL,LR
                  IF (.NOT.FLAGED) THEN
                     DO 220 II = 1,4
                        DO 210 JJ = 1,4
                           BLMAT(II,JJ) = TMAT3(1,JJ)*DMAT(II,1) +
     *                        TMAT3(2,JJ)*DMAT(II,2) +
     *                        TMAT3(3,JJ)*DMAT(II,3) +
     *                        TMAT3(4,JJ)*DMAT(II,4)
 210                       CONTINUE
 220                    CONTINUE
                  ELSE
                     CALL RFILL (32, FBLANK, BLMAT)
                     END IF
                  CALL RCOPY (32, BLMAT, POLCAL(POLPTR))
C
 570              CONTINUE
 580           CONTINUE
 590        CONTINUE
 600     CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1500 FORMAT ('SINGULAR MATRIX ON ',I3,' - ',I3,' IF =',I3,' WILL ZERO')
      END
