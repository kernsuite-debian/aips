      SUBROUTINE DATCAL (IA1, IA2, ISUBA, ICORID, TIME, TMINT, VIS,
     *   DROP, IERR)
C-----------------------------------------------------------------------
C! Applies calibration to data
C# Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2007, 2009-2012, 2015
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
C   Applies calibration to data.
C   Inputs:
C      IA1    I      First antenna number
C      IA2    I      Second antenna number
C      ISUBA  I      Subarray number
C      ICORID I      Correlation id.
C      TIME   R      Time of record (days)
C      TMINT  R      Integration time for current vis. rec. (days)
C      VIS    R(*,*) Input visibility array (not yet converted to
C                    output form.
C   Inputs from common /SELCAL/:
C      DOCAL   L     If true do antenna calibration.
C      DOBL    L     If true do baseline calibration.
C      DOWTCL  L     If true calibrate weights.
C      CURCAL  R(*)  Current calibration information.
C                     Values in order:
C                     By antenna (NUMANT)
C                        By IF (EIF-BIF+1)
C                           By Polarization (NUMPOL)
C                               Real part, imaginary part,
C                               cos(delta), sin(delta), rate
C                     Where delta is the phase change between
C                     channels and rate is the fringe rate in
C                     radians/day
C      LCUCAL   I     Number of values in CURCAL per entry (5)
C      POLOFF   I(4,2)Offsets from the beginning of an IF entry in
C                     CURCAL for a given polarization.  The first
C                     dimension is the polarization pixel number and
C                     the second is the antenna number of a baseline
C                     (e.g. first or second = 1 or 2).
C      CALTIM   R(3)  Time of two cal. entries; third value is
C                     current lowest upper time
C      LCALTM    R    Time of current calibration.
C      BLFAC     R(*) Baseline dependent factors including GMMOD.
C                     Indexing scheme: an entry defined by ant1<ant2
C                     starts in element:
C         lentry * (((ant1-1)*numant-((ant1+1)*ant1)/2 + ant2) - 1) + 1
C                       where lentry = 2 * NUMPOL * (EIF-BIF+1)
C                       An entry contains the values in order:
C                       By IF (NUMIF)
C                          By Polarization (NUMPOL)
C                              Real part, imaginary part.
C                     Applied only to cross corelation data if DOBL
C      LAMBDA  R(*)   List of wavelengths (meters) for each channel
C                     and IF
C      NLAMDA  I      Number of entries in LAMBDA for each IF.
C     Output:
C      DROP    L      True if data all flagged.
C      IERR    I      Return code, 0=OK, else CGASET error number.
C     Output to common:
C      CNTREC  I(2,3) Record counts:
C                     (1&2,1) Previously flagged (partly, fully)
C                     (1&2,2) Flagged due to gains (part, full)
C                     (1&2,3) Good selected (part, full)
C-----------------------------------------------------------------------
      INTEGER   IA1, IA2, ISUBA, ICORID, IERR
      REAL      TIME, TMINT, VIS(*)
      LOGICAL   DROP
C
      INTEGER   INDXA1, INDXA2, ASIZE, IIF, IPOL, IFQ, INCPX, IOFF,
     *   JOFF, INDEX, JNDXA1, JNDXA2, BLNDX, LENTRY, BLPNT, MAXPOL,
     *   IDNDX, ITFILT, BLPINC
      LOGICAL   BADBL, SOMFLG, ALLFLG, SMPFLG, ALPFLG, ALLDED,
     *   SOMBAD, CCOR, DODERA, DOVLB, DODISP, BADDSP
      REAL      TVR, TVI, TVR1, GR, GI, DGR, DGI, PHASE, GRD, GID,
     *   CP, SP, GWT, CATUR(256), RATE, ARG, RFACT, DFACT, DPHAS, DELAYD
      DOUBLE PRECISION CATUD(128), DSFACT, DBITS
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      EQUIVALENCE (CATUV, CATUR, CATUD)
C-----------------------------------------------------------------------
C                                       See if new time - update cal.
      IF (TIME.GT.LCALTM) THEN
         CALL CGASET (TIME, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
      BLPINC = 0
      IF (DOBL) BLPINC = 2
      IF (BCHANS.LE.0) BCHANS = BCHAN
      IF (ECHANS.LE.0) ECHANS = ECHAN
C                                       Set antenna indices
      ASIZE = NUMPOL * (EIF - BIF + 1) * LCUCAL
      JNDXA1 = (IA1 - 1) * ASIZE + 1
      JNDXA2 = (IA2 - 1) * ASIZE + 1
C                                       Set baseline index
      BLNDX = ((IA1-1)*NUMANT) - (((IA1-1)*IA1)/2) + IA2
      LENTRY = 2 * (EIF - BIF + 1) * NUMPOL
      BLPNT = 1
      IF (DOBL) BLPNT = LENTRY * (BLNDX-1) + 1
C                                       Check if cross correlation
      CCOR = IA1.NE.IA2
C                                       Init. flagged flags
      ALLFLG = .TRUE.
      ALLDED = .TRUE.
      ALPFLG = .TRUE.
      SOMFLG = .FALSE.
      SMPFLG = .FALSE.
C                                       Check if delay or rate
C                                       corrections.
      DODERA = .FALSE.
      DOVLB = .FALSE.
C                                       handle 1 solution for both pols
      MAXPOL = MAX (1, MIN (KNCOR, NUMPOL*NUMPOL))
      DO 25 IIF = BIF,EIF
         DO 20 IPOL = 1,MAXPOL
            INDXA1 = JNDXA1 + POLOFF(IPOL,1)
            INDXA2 = JNDXA2 + POLOFF(IPOL,2)
            IF (DOCAL) DOVLB = (CURCAL(INDXA1+2).NE.0.0) .OR.
     *         (CURCAL(INDXA1+3).NE.0.0) .OR.
     *         (CURCAL(INDXA2+2).NE.0.0) .OR.
     *         (CURCAL(INDXA2+3).NE.0.0)
            IF (DOVLB) DODERA = .TRUE.
 20         CONTINUE
         JNDXA1 = JNDXA1 + LCUCAL * NUMPOL
         JNDXA2 = JNDXA2 + LCUCAL * NUMPOL
 25      CONTINUE
C                                       DODERA is now true if there are
C                                       non-zero delay corrections for
C                                       any selected polarization in
C                                       any selected IF.
      DODISP = (DDELAY(IA1).NE.0.0) .OR. (DDELAY(IA2).NE.0.0)
C                                       DODISP is now true if either
C                                       dispersive delay is not zero
      BADDSP = (DDELAY(IA1).EQ.FBLANK) .OR. (DDELAY(IA2).EQ.FBLANK)
C                                       BADDSP is now true if either
C                                       dispersive delay is blanked
C
C                                       Reset antenna indices
      JNDXA1 = (IA1 - 1) * ASIZE + 1
      JNDXA2 = (IA2 - 1) * ASIZE + 1
C                                       Set visibility increment
      INCPX = CATUV(KINAX)
C                                       Compressed data expanded
      IF (INCPX.EQ.1) INCPX = 3
C                                       Loop thru IF
      DO 300 IIF = BIF,EIF
         IOFF = (IIF-1) * KNCIF
C                                       Loop thru polarization
         DO 200 IPOL = 1,KNCOR
C                                       Check baseline flags
            BADBL = (DOBL) .AND. (BLFAC(BLPNT).EQ.FBLANK) .AND.
     *         (IPOL.LE.2) .AND. (CCOR)
            JOFF = IOFF + (IPOL-1) * KNCS
C                                       Handle 1 Solution for 2 polzns.
            INDXA1 = JNDXA1 + POLOFF(MIN(IPOL,MAXPOL),1)
            INDXA2 = JNDXA2 + POLOFF(MIN(IPOL,MAXPOL),2)
            GR = 1.0
            GI = 0.0
            DGR = 1.0
            DGI = 0.0
            GWT = 0.0
            DELAYD = 0.0
            IF (BADBL) GO TO 60
C                                       Check if baseline only wanted.
            IF (DOCAL) THEN
C                                       Check IF flags
               IF ((CURCAL(INDXA1).EQ.FBLANK) .OR.
     *             (CURCAL(INDXA2).EQ.FBLANK)) GO TO 60
C                                       Set gains
               GR = CURCAL(INDXA1) * CURCAL(INDXA2) +
     *            CURCAL(INDXA1+1) * CURCAL(INDXA2+1)
               GI = CURCAL(INDXA2) * CURCAL(INDXA1+1) -
     *            CURCAL(INDXA1) * CURCAL(INDXA2+1)
               END IF
C                                       Baseline calibration for
C                                       first two poln. only.
            IF ((DOBL) .AND. (IPOL.LE.2) .AND. (CCOR)) THEN
               TVR = GR
               GR = GR * BLFAC(BLPNT) + GI * BLFAC(BLPNT+1)
               GI = GI * BLFAC(BLPNT) - TVR * BLFAC(BLPNT+1)
               END IF
C                                       "Weight" calibration
            IF (DOWTCL) THEN
               GWT = (GR*GR + GI*GI)
               IF (GWT.GT.1.0E-10) GWT = 1.0 / GWT
            ELSE
               GWT = 1.0
               END IF
C                                       See if delay-rate corrections
C                                       wanted.
            IF (DODERA) THEN
C                                       Delay correction
               DELAYD = CURCAL(INDXA1+2) - CURCAL(INDXA2+2)
C               DGR = CURCAL(INDXA1+2) * CURCAL(INDXA2+2) +
C     *            CURCAL(INDXA1+3) * CURCAL(INDXA2+3)
C               DGI = CURCAL(INDXA2+2) * CURCAL(INDXA1+3) -
C     *            CURCAL(INDXA1+2) * CURCAL(INDXA2+3)
               DGR = COS (DELAYD)
               DGI = SIN (DELAYD)
C                                       Apply fringe rate
               RATE = CURCAL(INDXA1+3) - CURCAL(INDXA2+3)
               PHASE = RATE * (TIME - LCALTM)
C                                       Apply decorrelation
C                                       corrections as applicable
               IDNDX = (ICORID - 1) * MAXIF + IIF
               ARG = 0.5 * ATAN2 (DGI, DGR)
C                                       VLBA-only corrections
               IF (DODSM(IDNDX).AND.(ICQVBA(ISUBA).EQ.1)) THEN
C                                       Spectral averaging correction
                  IF ((ABS (ARG).GT.1.0E-5).AND.
     *               (NXDSM(IDNDX).GT.1)) THEN
                     DFACT = NXDSM(IDNDX) * SIN (ARG / NXDSM(IDNDX)) /
     *                  SIN (ARG)
                     DFACT = ABS (DFACT)
                     GR = GR * DFACT
                     GI = GI * DFACT
                     END IF
C                                       Segmentation loss correction
C                                       Compute residual delay in bits
                  DBITS = (2.0 * ARG) / (DELFAC(IIF) * DBTVBA(IDNDX))
C                                       Segmentation loss
                  CALL FXSEG (LTPVBA(IDNDX), NFTVBA(IDNDX), DBITS,
     *               DSFACT, IERR)
                  IF (IERR.NE.0) GO TO 999
C                                       Correct gain factors
                  GR = GR / DSFACT
                  GI = GI / DSFACT
C                                       Spectral averaging correction
C                                       all other cases
C                                       needed only when averaging post
C                                       correlation - dont have now
C              ELSE IF (ABS (SIN (ARG)).GT.1.0E-5) THEN
C                 DFACT = ARG / SIN (ARG)
C                 DFACT = ABS (DFACT)
C                 GR = GR * DFACT
C                 GI = GI * DFACT
                  END IF
C                                       Compute rate smearing
C                                       correction (includes
C                                       the VLBA OVLB filters)
               IF (DORSM) THEN
C                                       Default is boxcar smoothing
                  ITFILT = 0
C                                       Check if VLBA filter used
                  IF (DODSM(IDNDX).AND.(ICQVBA(ISUBA).EQ.1)) THEN
                     ITFILT = ITFVBA(IDNDX)
                     END IF
C                                       Compute loss factor
                  CALL TFILTR (ITFILT, RATE, TMINT, RFACT, IERR)
                  IF (IERR.NE.0) GO TO 999
                  GR = GR * RFACT
                  GI = GI * RFACT
                  END IF
C                                       Caution if VLBA corrections
C                                       not possible due to missing CQ
C                                       table or CQ table entries
               IF ((ICQVBA(ISUBA).EQ.1).AND.(.NOT.DODSM(IDNDX)).AND.
     *            WRNVBA(ISUBA).AND.
     *            ((ABS(ARG).GT.0.0).OR.(ABS(RATE).GT.0.0))) THEN
                  WRITE (MSGTXT,1030) ISUBA
                  CALL MSGWRT (8)
                  WRITE (MSGTXT,1035)
                  CALL MSGWRT (8)
                  WRITE (MSGTXT,1040)
                  CALL MSGWRT (8)
C                                       Only print warning once
                  WRNVBA(ISUBA) = .FALSE.
                  END IF
C                                       Correct for frequency offset.
               IF ((BCHANS.GT.1) .OR. (ABS (CATUR(KRCRP+KLOCFY) - 1.0)
     *            .GT. 0.0001)) THEN
                  DPHAS = DELAYD * (BCHANS-CATUR(KRCRP+KLOCFY))
                  PHASE = PHASE + DPHAS
                  END IF
               CP = COS (PHASE)
               SP = SIN (PHASE)
               TVR = GR * CP - GI * SP
               TVI = GR * SP + GI * CP
               GR = TVR
               GI = TVI
               END IF
   60       CONTINUE
C                                       Loop thru channel.
            DO 80 IFQ = BCHANS,ECHANS
               INDEX = (JOFF + (IFQ-1) * KNCF) * INCPX + 1
               TVR = GR * VIS(INDEX) + GI * VIS(INDEX+1)
               TVI = GR * VIS(INDEX+1) - GI * VIS(INDEX)
C                                       Apply dispersive delay
C                                       correction or kill data point
C                                       dispersive delay is blanked
               IF (DODISP) THEN
                  IF (BADDSP) THEN
                     GWT = 0.0
                  ELSE
                     PHASE = LAMBDA(NLAMDA * (IIF - 1) + IFQ)
     *                       * (DDELAY(IA1) - DDELAY(IA2))
                     GRD = COS (PHASE)
                     GID = SIN (PHASE)
                     TVR1 = TVR
                     TVR = GRD * TVR1 + GID * TVI
                     TVI = GRD * TVI - GID * TVR1
                     END IF
                  END IF
               SMPFLG = SMPFLG .OR. (VIS(INDEX+2) .LE. 0.0)
               ALPFLG = ALPFLG .AND. (VIS(INDEX+2) .LE. 0.0)
               SOMFLG = SOMFLG .OR. (GWT .LE. 0.0)
               ALLFLG = ALLFLG .AND. (GWT .LE. 0.0)
               ALLDED = ALLDED .AND. ((VIS(INDEX+2) .LE. 0.0)
     *                                .OR. (GWT .LE. 0.0))
               VIS(INDEX) = TVR
               VIS(INDEX+1) = TVI
               VIS(INDEX+2) = VIS(INDEX+2) * GWT
C                                       Rotate phase for next if we have
C                                       delay corrections
               IF (DODERA) THEN
                  TVR = (GR * DGR - GI * DGI)
                  TVI = (GR * DGI + GI * DGR)
                  GR = TVR
                  GI = TVI
               END IF
   80       CONTINUE
C                                       Handle 1 solution and 2 polzn
            IF (IPOL.EQ.1.AND.NUMPOL.GT.1) BLPNT = BLPNT + BLPINC
  200    CONTINUE
C                                       Setup for next IF
         JNDXA1 = JNDXA1 + LCUCAL * NUMPOL
         JNDXA2 = JNDXA2 + LCUCAL * NUMPOL
         BLPNT = BLPNT + BLPINC
 300     CONTINUE
C                                       Increment counts of the
C                                       good, bad and the ugly.
      SOMFLG = SOMFLG .AND. (.NOT.ALLFLG)
      SMPFLG = SMPFLG .AND. (.NOT.ALPFLG)
      SOMBAD = (SOMFLG.OR.SMPFLG) .AND. (.NOT.ALLDED)
      IF (SMPFLG) CNTREC(1,1) = CNTREC(1,1) + 1
      IF (SOMFLG) CNTREC(1,2) = CNTREC(1,2) + 1
      IF (SOMBAD) CNTREC(1,3) = CNTREC(1,3) + 1
      IF (ALPFLG) CNTREC(2,1) = CNTREC(2,1) + 1
      IF (ALLFLG) CNTREC(2,2) = CNTREC(2,2) + 1
      IF ((.NOT.ALLDED) .AND. (.NOT.SOMBAD))
     *   CNTREC(2,3) = CNTREC(2,3) + 1
      DROP = ALLDED
      GO TO 999
C
 999  RETURN
C----------------------------------------------------------------------
1030  FORMAT ('** WARNING - Subarray',I4,' contains VLBA data, but',
     *   ' there is a')
1035  FORMAT ('** problem with the CQ table.  No VLBA decorrelation',
     *   ' corrections')
1040  FORMAT ('** will be implimented.  See EXPLAIN FITLD for',
     *   ' more information.')
      END
