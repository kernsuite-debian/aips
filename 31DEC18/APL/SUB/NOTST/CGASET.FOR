      SUBROUTINE CGASET (TIME, IERR)
C-----------------------------------------------------------------------
C! Maintains calibration values in an array in common
C# Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-1999, 2010, 2013, 2015, 2018
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
C   Gets next set of calibration data in CURCAL, does linear
C   interpolation in time between time entries in CALTAB.
C   Calls BLSET to fill or initialize BLFAC and apply and baseline
C   dependent calibration then enters any rate corrections.
C      If the preceeding or following entry is for the current source
C   then only entries for that source is used.
C   Inputs:
C      TIME    R        Current time (of data) in days.
C   Inputs from commons in DSEL.INC:
C      CALTAB  R(*,2)   Cal. table from gain table file
C                       Values in order:
C                       By antenna (NUMANT)
C                          By IF (NUMIF)
C                             By Polarization (NUMPOL)
C                                 Real part, imaginary part,
C                                 group delay, phase rate, ref. ant.
C      LCLTAB  I        Number of values in CALTAB per entry (5)
C      IFRTAB  R(*,2)   Ionospheric Faraday rotation from cal table,
C                       listed by antenna number
C      DDTAB   R(2,*,2) Dispersive delay from cal table listed by
C                       antenna number, polarization
C      CALTIM  R(3)     1: time of latest cal record earlier than TIME
C                       2: time of earliest cal record later than TIME
C                       3: time of earliest cal record later than TIME
C      CIDSOU  I(2)     Previous/next source ID number using ICALPn as
C                       a pointer.
C      CURSOU  I        Current source ID number.
C      IBLP1   I        Pointer in BLTAB, if < 0 then BLFAC needs
C                       to be initialized by BLSET.
C      DOCAL   L        If true then apply antenna based calibration.
C      DOBL    L        If true then apply baseline based calibration.
C      RATFAC  R(*)     IF scaling factor to convert s/s to rad/day
C      DELFAC  R(*)     IF scaling factor to convert s to rad/channel
C      DXTIME  R        Integration time of the data in days.
C   Output:
C      IERR    I        Return error code, 0=>OK else error.
C   Output to commons in DSEL.INC:
C      LCALTM  R        Time of current calibration.
C      CURCAL  R(*)     Current calibration information; includes gain
C                       normalization corrections.
C                       Values in order:
C                       By antenna (NUMANT)
C                          By IF (EIF-BIF+1)
C                             By Polarization (NUMPOL)
C                                 Real part, imaginary part,
C                                 cos(delta), sin(delta), rate
C                       Where delta is the phase change between
C                       channels and rate is the fringe rate in
C                       radians/day
C      IFR     R(*)     Current ionospheric rotation measure for each
C                       antenna
C      DDELAY  R(2,*)   Current dispersive delay radians per meter for
C                       each polarization, antenna
C      BLFAC   R(*)     Baseline dependent factors.
C                       Indexing scheme: an entry defined by ant1<ant2
C                       starts in element:
C         lentry * (((ant1-1)*numant-((ant1+1)*ant1)/2 + ant2) - 1) + 1
C                       where lentry = 2 * NUMPOL * (EIF-BIF+1)
C                       An entry contains the values in order:
C                       By IF (NUMIF)
C                          By Polarization (NUMPOL)
C                              Real part, imaginary part.
C-----------------------------------------------------------------------
      INTEGER   IERR
      REAL      TIME
C
      INTEGER   IANT, IIF, IPOL, INDEX, JNDEX, J
      LOGICAL   GOOD1, GOOD2, DOXDR, WASOK
      REAL      WTT1, WTT2, V1R, V1I, V2R, V2I, PHASE1, PHASE2, CP1,
     *   CP2, SP1, SP2, G1R, G1I, G2R, G2I, DELTA,
     *   CATR(256), WT1, WT2, AMP1, AMP2, AMPNEW, AMPCOR
      LOGICAL NEWCAL
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATR)
      SAVE DOXDR
      DATA DOXDR /.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
      NEWCAL = .FALSE.
C                                       Only doing baseline corrections?
      IF (.NOT.DOCAL) THEN
         LCALTM = TIME
C                                       Baseline (BL) correction factors
         IF (DOBL) CALL BLSET (TIME, IERR)
         GO TO 999
         END IF
C                                       See if time for new table entry
C      IF ((NEXTCL.LE.NCLINR) .AND. (TIME.GT.CALTIM(3))) THEN
      IF (TIME.GT.CALTIM(3)) THEN
         CALL CSLGET (TIME, IERR)
         IF (IERR.NE.0) GO TO 999
         NEWCAL = .TRUE.
         END IF
C                                       See if calibration needs update;
C                                       every 0.1 of solution interval.
      DELTA = (TIME - LCALTM)
      IF ((.NOT. NEWCAL)
     *    .AND. (DELTA.LE.0.03*(CALTIM(2)-CALTIM(1)))) GO TO 999
      LCALTM = TIME
C                                       Initialize indices for
C                                       CURCAL and CALTAB
      INDEX = 1
      JNDEX = 1
C                                       Loop thru antennas
      WASOK = .FALSE.
      DO 500 IANT = 1,NUMANT
C                                       Set interpolation weights.
         WTT1 = 0.0
         IF (TIME.LT.TIMECL(2,IANT)) THEN
            WASOK = .TRUE.
            IF (TIMECL(2,IANT).GT.TIMECL(1,IANT)) WTT1 = (TIMECL(2,IANT)
     *         - TIME) / (TIMECL(2,IANT) - TIMECL(1,IANT))
            END IF
         WTT2 = 1.0 - WTT1
C                                       Set Faraday RM
         IF ((IFRTAB(IANT,1).NE.FBLANK)
     *       .AND. (IFRTAB(IANT,2).NE.FBLANK)) THEN
            IFR(IANT) = WTT1 * IFRTAB(IANT,1)
     *                  + WTT2 * IFRTAB(IANT,2)
         ELSE IF (IFRTAB(IANT,1).NE.FBLANK) THEN
            IFR(IANT) = IFRTAB(IANT,1)
         ELSE IF (IFRTAB(IANT,2).NE.FBLANK) THEN
            IFR(IANT) = IFRTAB(IANT,2)
         ELSE
            IFR(IANT) = FBLANK
            END IF
C                                       Set dispersive delay
         DO 20 J = 1,2
            IF ((DDTAB(J,IANT,1).NE.FBLANK)
     *         .AND. (DDTAB(J,IANT,2).NE.FBLANK)) THEN
               DDELAY(J,IANT) = VELITE * TWOPI * (WTT1 * DDTAB(J,IANT,1)
     *            + WTT2 * DDTAB(J,IANT,2))
            ELSE IF (DDTAB(J,IANT,1).NE.FBLANK) THEN
               DDELAY(J,IANT) = VELITE * TWOPI * DDTAB(J,IANT,1)
            ELSE IF (DDTAB(J,IANT,2).NE.FBLANK) THEN
               DDELAY(J,IANT) = VELITE * TWOPI * DDTAB(J,IANT,2)
            ELSE
               DDELAY(J,IANT) = FBLANK
               END IF
 20         CONTINUE
C                                       Loop thru IF
         DO 400 IIF = BIF,EIF
C                                       Loop thru polarization
            DO 300 IPOL = 1,NUMPOL
C                                       Initialize soln with blanks
               CURCAL(INDEX) = FBLANK
               CURCAL(INDEX+1) = FBLANK
               CURCAL(INDEX+2) = FBLANK
               CURCAL(INDEX+3) = FBLANK
               CURCAL(INDEX+4) = FBLANK
C                                       Check for blanked soln.
               GOOD1 = (CALTAB(JNDEX,1).NE.FBLANK) .AND.
     *                 (CALTAB(JNDEX+1,1).NE.FBLANK) .AND.
     *                 (CALTAB(JNDEX+2,1).NE.FBLANK) .AND.
     *                 (CALTAB(JNDEX+3,1).NE.FBLANK) .AND.
     *                 (WTT1.GT.0.0)
               GOOD2 = (CALTAB(JNDEX,2).NE.FBLANK) .AND.
     *                 (CALTAB(JNDEX+1,2).NE.FBLANK) .AND.
     *                 (CALTAB(JNDEX+2,2).NE.FBLANK) .AND.
     *                 (CALTAB(JNDEX+3,2).NE.FBLANK) .AND.
     *                 (WTT2.GT.0.0)
               IF (.NOT.(GOOD1.OR.GOOD2)) GO TO 290
C                                       Different reference antennas
C                                       Use closest
               IF ((ABS (CALTAB(JNDEX+4,1)-CALTAB(JNDEX+4,2)).GE.0.5)
     *            .AND. GOOD1 .AND. GOOD2) THEN
                  GOOD1 = WTT1 .GE. WTT2
                  GOOD2 = WTT1 .LT. WTT2
                  END IF
               WT1 = WTT1
               WT2 = WTT2
               IF (.NOT.GOOD1) THEN
                  WT1 = 0.0
                  WT2 = 1.0
                  END IF
               IF (.NOT.GOOD2) THEN
                  WT1 = 1.0
                  WT2 = 0.0
                  END IF
C                                       Set values, initial gain.
               G1R = 0.0
               G1I = 0.0
               G2R = 0.0
               G2I = 0.0
               IF (GOOD1) THEN
                  G1R = CALTAB(JNDEX,1)
                  G1I = CALTAB(JNDEX+1,1)
                  END IF
               IF (GOOD2) THEN
                  G2R = CALTAB(JNDEX,2)
                  G2I = CALTAB(JNDEX+1,2)
                  END IF
               V1R = G1R
               V1I = G1I
               V2R = G2R
               V2I = G2I
C                                       Check if fringe rates given
               IF ((CALTAB(JNDEX+3,1).NE.0.0) .OR.
     *            (CALTAB(JNDEX+3,2).NE.0.0)) THEN
                  PHASE1 = 0.0
                  PHASE2 = 0.0
                  IF (GOOD1) PHASE1 = CALTAB(JNDEX+3,1) * (TIME -
     *               TIMECL(1,IANT)) * RATFAC(IIF)
                  IF (GOOD2) PHASE2 = CALTAB(JNDEX+3,2) * (TIME -
     *               TIMECL(2,IANT)) * RATFAC(IIF)
                  CP1 = COS (PHASE1)
                  CP2 = COS (PHASE2)
                  SP1 = SIN (PHASE1)
                  SP2 = SIN (PHASE2)
                  DOXDR = DOXDR .OR. (SP1.NE.0.0) .OR. (SP2.NE.0.0)
                  V1R = G1R * CP1 - G1I * SP1
                  V1I = G1R * SP1 + G1I * CP1
                  V2R = G2R * CP2 - G2I * SP2
                  V2I = G2R * SP2 + G2I * CP2
                  END IF
C                                       Amplitude and phase
               CURCAL(INDEX) = WT1 * V1R + WT2 * V2R
               CURCAL(INDEX+1) = WT1 * V1I + WT2 * V2I
               AMP1 = SQRT (V1R*V1R + V1I*V1I)
               AMP2 = SQRT (V2R*V2R + V2I*V2I)
               AMPNEW = SQRT (CURCAL(INDEX)*CURCAL(INDEX) +
     *            CURCAL(INDEX+1)*CURCAL(INDEX+1))
               IF (AMPNEW.LT.1.0E-20) AMPNEW = 1.0E-20
               AMPCOR = (WT1*AMP1 + WT2*AMP2) / AMPNEW
C                                       Control amplitudes
               CURCAL(INDEX) = CURCAL(INDEX) * AMPCOR
               CURCAL(INDEX+1) = CURCAL(INDEX+1) * AMPCOR
C                                       Init. delay to 0.
               CURCAL(INDEX+2) = 0.0
C                                       Rate
               CURCAL(INDEX+3) = (WT1 * CALTAB(JNDEX+3,1) +
     *            WT2 * CALTAB(JNDEX+3,2)) * RATFAC(IIF)
C                                       Delay
               IF ((CALTAB(JNDEX+2,1).NE.0.0) .OR.
     *            (CALTAB(JNDEX+2,2).NE.0.0)) THEN
                  PHASE1 = (WT1 * CALTAB(JNDEX+2,1) +
     *               WT2 * CALTAB(JNDEX+2,2)) * DELFAC(IIF)
                  CURCAL(INDEX+2) = PHASE1
                  DOXDR = DOXDR .OR. (PHASE1.NE.0.0)
                  END IF
C                                       Update indices
 290           INDEX = INDEX + LCUCAL
               JNDEX = JNDEX + LCLTAB
 300           CONTINUE
 400        CONTINUE
 500     CONTINUE
C                                       Baseline correction factors
C                                       Init BLFAC
      IF (DOXDR .OR. DOBL .OR. (IBLP1.LT.0)) CALL BLSET (TIME, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
