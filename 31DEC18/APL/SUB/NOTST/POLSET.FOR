      SUBROUTINE POLSET (IERR)
C-----------------------------------------------------------------------
C! Fills polarization correction table from info in AN table.
C# UV EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2007, 2010, 2012, 2014-2015, 2017-2018
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
C   Fills polarization correction table from info in AN table.
C   Inputs from common
C      STNEPL   R(2,*)   Feed real/elipticity (poln, IF)
C      STNORI   R(2,*)   Feed imag/orientation (poln, IF)
C      STNPST   C*8      Feed solution type:
C                           'APPROX  ' => linear approximation
C                           'ORI-ELP ' => orientation-ellipticity
C                           'X-Y LIN ' => lin. approx. for lin.
C                              polarized (X-Y) data.
C                           'VLBI' => lin. approx for VLBI.
C   Output:
C      IERR    I         Return error code, 0=>OK else error.
C                        1=table too small, 2=multiple subarrays,
C                        5=FREQID incompatibility
C                        10 = unknown polarization parameterization,
C                        otherwise GETANT error.
C   Output to common /CPLINF/:
C      PARTIM  R         Time of current parallactic angles. (-1.0E10)
C      PARSOU  I         Source ID for current parallactic angles. (-10)
C      PREFA   I         Poln. ref antenna 'ORI-' only
C      POLPD   R(if)    'R-L' phase difference 'ORI-' only
C                            Nchan * Nif in line case
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
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER CHSTYP(4)*8, PDSTYP*8
      INTEGER   IIF, IANT1, IANT2, LIMIT, BLNDX, LIMIT2, KK, BLPNT,
     *   LENTRY, II, NUMBER, IALUN, IPDRNO, SUBA, PDKOLS(9), PDNUMV(9),
     *   PDBUFF(512), FREQID, REFANT(2), NWORDS, ICH, KR1, KR2, KL1,
     *   KL2, LUNTMP, KL
      REAL      PHDIFF(MAXCIF), DTEMP(2,MAXCIF)
      COMPLEX   DR1, DR2, DL1, DL2, BLMAT(4,4)
      LONGINT   PDPTR, POLPTR
      SAVE PHDIFF, DTEMP
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DPDC.INC'
      DATA CHSTYP /'APPROX  ','ORI-ELP','X-Y LIN ', 'VLBI'/
C-----------------------------------------------------------------------
C                                       Initialize parallactic angle
C                                       time, source
      PARTIM = -1.0E10
      PARSOU = -10
C                                       Only 1 subarray
      IF (SUBARR.EQ.0) THEN
         IERR = 2
         MSGTXT = 'ERROR: CAN ONLY DO POLARIZATION CORRECTION FOR 1 '
     *      // 'SUBARRAY'
         GO TO 990
         END IF
C                                       Get antenna info
      CALL GETANT (IUDISK, IUCNO, SUBARR, CATUV, POLCAL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       line or continuum
      IF (MOD(DOPOL,10).LT.6) THEN
         CALL FNDEXT ('PD', CATUV, II)
         IF ((PDVER.GT.II) .OR. (PDVER.LE.0)) PDVER = II
      ELSE
         PDVER = 0
         END IF
C                                       Check FREQID compatibility
      IF ((FQIDAN.GT.0) .AND. (FRQSEL.GT.0) .AND. (FQIDAN.NE.FRQSEL)
     *   .AND. (PDVER.LE.0)) THEN
         MSGTXT = 'WARNING - POTENTIAL FATAL ERROR'
         CALL MSGWRT (8)
         MSGTXT = '  The polarization information in your AN table'
         CALL MSGWRT (8)
         WRITE (MSGTXT,1050) FQIDAN
         CALL MSGWRT (8)
         WRITE (MSGTXT,1060) FRQSEL
         CALL MSGWRT (8)
         IERR = 5
         GO TO 999
         END IF
C                                       get PD data
      IF (PDVER.GT.0) THEN
         IALUN = LUNTMP (1)
         CALL PDINI ('READ', PDBUFF, IUDISK, IUCNO, PDVER, CATUV, IALUN,
     *      IPDRNO, PDKOLS, PDNUMV, PDNUMA, PDNUMP, PDNUMI, PDNUMF,
     *      PDSTYP, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPENING PD TABLE'
            GO TO 990
            END IF
         NWORDS = 2 * PDNUMF * PDNUMI * PDNUMP * PDNUMA + 2048
         NWORDS = (NWORDS - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', 'POLSET', NWORDS, DTERMS, PDTERM, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'FAILED TO GET REQUESTED MEMORY FOR PD FILE'
            GO TO 990
            END IF
         LIMIT = PDBUFF(5)
         KK = 2 * PDNUMF * PDNUMI * PDNUMP
         DO 20 II = 1,LIMIT
            CALL TABPD ('READ', PDBUFF, IPDRNO, PDKOLS, PDNUMV, PDNUMI,
     *         PDNUMF, PDNUMP, IANT1, SUBA, FREQID, REFANT, PHDIFF,
     *         DTEMP, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ PD FILE'
               GO TO 990
               END IF
            IF ((SUBARR.GT.0) .AND. (SUBA.GT.0) .AND.
     *         (SUBA.NE.SUBARR)) GO TO 20
            IF ((FRQSEL.GT.0) .AND. (FREQID.GT.0) .AND.
     *         (FREQID.NE.FRQSEL)) GO TO 20
            PDPTR = 1 + PDTERM + (IANT1-1) * KK
            CALL RCOPY (KK, DTEMP, DTERMS(PDPTR))
            PREFA = REFANT(1)
            IF ((IANT1.EQ.PREFA) .AND. (SOLTYP.EQ.2)) THEN
               KL = PDNUMF * PDNUMI
               CALL RCOPY (KL, PHDIFF, POLPD)
               END IF
 20         CONTINUE
         CALL TABPD ('CLOS', PDBUFF, IPDRNO, PDKOLS, PDNUMV, PDNUMI,
     *      PDNUMF, PDNUMP, IANT1, SUBA, FREQID, REFANT, PHDIFF,
     *      DTEMP, KK)
         STNPST = PDSTYP
      ELSE
         PDNUMF = 1
         END IF
C                                       Determine solution type:
      SOLTYP = 0
      IF (STNPST.EQ.CHSTYP(1)) SOLTYP = 1
      IF (STNPST.EQ.CHSTYP(2)) SOLTYP = 2
      IF (STNPST.EQ.CHSTYP(3)) SOLTYP = 3
      IF (STNPST.EQ.CHSTYP(4)) SOLTYP = 4
C                                       Unknown solution type
      IF (SOLTYP.EQ.0) THEN
         MSGTXT = 'UNKNOWN POLARIZATION PARAMETERIZATION =''' //
     *      STNPST // ''''
         IERR = 10
         CALL MSGWRT (8)
         MSGTXT = 'NO USABLE POLCAL INFORMATION AVAILABLE IN AN TABLE'
         CALL MSGWRT (8)
         MSGTXT = 'PERHAPS DOPOL SHOULD BE FALSE'
         GO TO 990
         END IF
C                                       Get reference antenna, phase
C                                       differences if any from AN
C                                       table. 'ORI-' only
      IF ((SOLTYP.EQ.2) .AND. (PDVER.LE.0)) THEN
         IALUN = LUNTMP (1)
         CALL PDRGET (IUDISK, IUCNO, SUBARR, IALUN, CATUV, EIF,
     *      PREFA, POLPD, PDBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       create memory for POLCAL
      PCLIF = EIF - BIF + 1
      PCLCH = 1
      IF (PDVER.GT.0) PCLCH = ECHANS - BCHANS + 1
      NUMBER = (NSTNS * (NSTNS+1)) / 2
      NWORDS = 32 * PCLIF * PCLCH * NUMBER
      NWORDS = (NWORDS - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'POLSET', NWORDS, POLCAL, PPOLCL, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'FAILED TO GET REQUESTED MEMORY FOR POLCAL'
         GO TO 990
         END IF
C                                       Do only continuum 'APPR' here
      IF (SOLTYP.NE.1) GO TO 999
      IERR = 0
      CALL RFILL (32, 0.0, BLMAT)
      DO 30 II = 1,4
         BLMAT(II,II) = CMPLX (1.0, 0.0)
 30      CONTINUE
C                                       Compute correction matrices.
      LENTRY = PCLIF * PCLCH * 32
      LIMIT = NSTNS - 1
      DO 200 IANT1 = 1,LIMIT
         LIMIT2 = IANT1 + 1
         DO 190 IANT2 = LIMIT2,NSTNS
C                                       Set baseline index
            BLNDX = ((IANT1-1)*NSTNS) - (((IANT1-1)*IANT1)/2) + IANT2
C                                       Loop over IF: continuum
            IF (PDVER.LE.0) THEN
               DO 110 IIF = BIF,EIF
C                                       Linear approximation:
                  IF ((STNELP(1,IIF,IANT1).EQ.FBLANK) .OR.
     *               (STNELP(2,IIF,IANT1).EQ.FBLANK) .OR.
     *               (STNELP(1,IIF,IANT2).EQ.FBLANK) .OR.
     *               (STNELP(2,IIF,IANT2).EQ.FBLANK) .OR.
     *               (STNORI(1,IIF,IANT1).EQ.FBLANK) .OR.
     *               (STNORI(2,IIF,IANT1).EQ.FBLANK) .OR.
     *               (STNORI(1,IIF,IANT2).EQ.FBLANK) .OR.
     *               (STNORI(2,IIF,IANT2).EQ.FBLANK)) THEN
                     BLMAT(3,1) = CMPLX (FBLANK, 0.0)
                     BLMAT(3,2) = CMPLX (FBLANK, 0.0)
                     BLMAT(4,1) = CMPLX (FBLANK, 0.0)
                     BLMAT(4,2) = CMPLX (FBLANK, 0.0)
                  ELSE
                     DR1 = CMPLX (STNELP(1,IIF,IANT1),
     *                  STNORI(1,IIF,IANT1))
                     DL1 = CMPLX (STNELP(2,IIF,IANT1),
     *                  STNORI(2,IIF,IANT1))
                     DR2 = CMPLX (STNELP(1,IIF,IANT2),
     *                  STNORI(1,IIF,IANT2))
                     DL2 = CMPLX (STNELP(2,IIF,IANT2),
     *                  STNORI(2,IIF,IANT2))
                     BLMAT(3,1) = -0.5 * (DR1 + CONJG (DL2))
                     BLMAT(3,2) = BLMAT(3,1)
                     BLMAT(4,1) = -0.5 * (DL1 + CONJG (DR2))
                     BLMAT(4,2) = BLMAT(4,1)
                     END IF
                  BLPNT = (LENTRY * (BLNDX-1)) + (IIF-BIF) * 32 + 1
                  POLPTR = PPOLCL + BLPNT
                  CALL RCOPY (32, BLMAT, POLCAL(POLPTR))
 110              CONTINUE
C                                       spectral and IF: line
            ELSE
               DO 130 IIF = BIF,EIF
                  KR1 = (4 * PDNUMF * PDNUMI) * (IANT1 - 1) +
     *               2 * PDNUMF * (IIF - 1) + 2 * (BCHANS -1) + 1
                  KR2 = (4 * PDNUMF * PDNUMI) * (IANT2 - 1) +
     *               2 * PDNUMF * (IIF - 1) + 2 * (BCHANS -1) + 1
                  KL1 = KR1 + (2 * PDNUMF * PDNUMI)
                  KL2 = KR2 + (2 * PDNUMF * PDNUMI)
                  BLPNT = LENTRY * (BLNDX-1) + (IIF-BIF)*32*PCLCH
                  POLPTR = PPOLCL + BLPNT + 1
                  DO 120 ICH = BCHANS,ECHANS
                     IF ((DTERMS(PDTERM+KR1).EQ.FBLANK) .OR.
     *                  (DTERMS(PDTERM+KR1+1).EQ.FBLANK) .OR.
     *                  (DTERMS(PDTERM+KL1).EQ.FBLANK) .OR.
     *                  (DTERMS(PDTERM+KL1+1).EQ.FBLANK) .OR.
     *                  (DTERMS(PDTERM+KR2).EQ.FBLANK) .OR.
     *                  (DTERMS(PDTERM+KR2+1).EQ.FBLANK) .OR.
     *                  (DTERMS(PDTERM+KL2).EQ.FBLANK) .OR.
     *                  (DTERMS(PDTERM+KL2+1).EQ.FBLANK)) THEN
                        BLMAT(3,1) = CMPLX (FBLANK, 0.0)
                        BLMAT(3,2) = CMPLX (FBLANK, 0.0)
                        BLMAT(4,1) = CMPLX (FBLANK, 0.0)
                        BLMAT(4,2) = CMPLX (FBLANK, 0.0)
                     ELSE
                        DR1 = CMPLX (DTERMS(PDTERM+KR1),
     *                     DTERMS(PDTERM+KR1+1))
                        DL1 = CMPLX (DTERMS(PDTERM+KL1),
     *                     DTERMS(PDTERM+KL1+1))
                        DR2 = CMPLX (DTERMS(PDTERM+KR2),
     *                     DTERMS(PDTERM+KR2+1))
                        DL2 = CMPLX (DTERMS(PDTERM+KL2),
     *                     DTERMS(PDTERM+KL2+1))
                        BLMAT(3,1) = -0.5 * (DR1 + CONJG (DL2))
                        BLMAT(3,2) = BLMAT(3,1)
                        BLMAT(4,1) = -0.5 * (DL1 + CONJG (DR2))
                        BLMAT(4,2) = BLMAT(4,1)
                        END IF
                     CALL RCOPY (32, BLMAT, POLCAL(POLPTR))
                     KR1 = KR1 + 2
                     KL1 = KL1 + 2
                     KR2 = KR2 + 2
                     KL2 = KL2 + 2
                     POLPTR = POLPTR + 32
 120                 CONTINUE
 130              CONTINUE
               END IF
 190        CONTINUE
 200     CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('POLSET ERROR',I4,' ON ',A)
 1050 FORMAT ('  was set with FREQID ',I3,' this is being applied')
 1060 FORMAT ('  to FREQID ',I3,' data. Suggest you rerun PCAL')
      END
