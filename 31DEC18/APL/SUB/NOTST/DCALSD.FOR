      SUBROUTINE DCALSD (IA2, TIME, RADAT, DECDAT, VIS, DROP, IERR)
C-----------------------------------------------------------------------
C! Apply Single dish calibration to data.
C# Calibration Sdish
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   Single dish calibration routine
C   Applies calibration to data.
C    Inputs:
C      IA2        I    Beam number
C      TIME       R    Time of record (days)
C      RADAT      R    RA of datum (deg)
C      DECDAT     R    Declination of datum (deg)
C      VIS(*,*)   R    Input visibility array (not yet converted to
C                      output form.
C     Inputs from common /SELCAL/:
C      DOCAL        L    If true do beam calibration.
C      CURCAL(*)    R    Current calibration information.
C                        Values in order:
C                        By beam (NUMANT)
C                           By IF (EIF-BIF+1)
C                              By Polarization (NUMPOL)
C                                  Amplitude factor,
C                                  offset (before factor)
C                                  RA correction
C                                  Dec correction.
C      LCUCAL       I    Number of values in CURCAL per entry (4)
C      POLOFF(4,2)  I    Offsets from the beginning of an IF entry in
C                        CURCAL for a given polarization.  The first
C                        dimension is the polarization pixel number and
C                        the second is the antenna number of a baseline
C                        (e.g. first or second = 1 or 2).
C      CALTIM(3)    R    Time of two cal. entries; third value is
C                        time of current values.
C      LCALTM       R    Time of current calibration.
C     Output:
C      DROP         L    True if data all flagged.
C      IERR         I    Return code, 0=OK, else CGASET error number.
C     Output to common:
C      CNTREC(2,3)  I    Record counts:
C                        (1&2,1) Previously flagged (partly, fully)
C                        (1&2,2) Flagged due to gains (part, full)
C                        (1&2,3) Good selected (part, full)
C-----------------------------------------------------------------------
      INTEGER   IA2, IERR, INDXA2, ASIZE, IIF, IPOL, IFQ,
     *   INCPX, IOFF, JOFF, INDEX, JNDXA2
      LOGICAL   DROP, T, F, SOMFLG, ALLFLG, SMPFLG, ALPFLG,
     *   SOMBAD, ALLBAD, PFLG
      REAL      TIME, RADAT, DECDAT, VIS(*),
     *   TVR, TVI, GM, GA, RACOR, DECOR, GWT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       See if new time - update cal.
      IERR = 0
      IF (TIME.GT.LCALTM) CALL SDCSET (TIME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set beam index
      ASIZE = NUMPOL * (EIF - BIF + 1) * LCUCAL
      JNDXA2 = (IA2 - 1) * ASIZE + 1
C                                       Init. flagged flags
      ALLFLG = T
      ALPFLG = T
      SOMFLG = F
      SMPFLG = F
C                                       Position corrections
      IF ((CURCAL(JNDXA2+2).NE.FBLANK) .AND.
     *   (CURCAL(JNDXA2+3).NE.FBLANK)) THEN
         RACOR = CURCAL(JNDXA2+2) / COS (DECDAT*1.7453293E-2)
         DECOR = CURCAL(JNDXA2+3)
         RADAT = RADAT + RACOR
         DECDAT = DECDAT + DECOR
         END IF
C                                       Set visibility increment
      INCPX = CATUV(KINAX)
C                                       Loop thru IF
      DO 300 IIF = BIF,EIF
         IOFF = (IIF-1) * KNCIF
C                                       Loop thru polarization
         DO 200 IPOL = 1,KNCOR
            JOFF = IOFF + (IPOL-1) * KNCS
            INDXA2 = JNDXA2 + POLOFF(IPOL,2)
            GM = 1.0
            GA = 0.0
            GWT = 0.0
C                                       Check IF flags
            IF ((CURCAL(INDXA2).EQ.FBLANK) .OR.
     *          (CURCAL(INDXA2+1).EQ.FBLANK)) GO TO 60
C                                       Set corrections
               GM = CURCAL(INDXA2)
               GA = CURCAL(INDXA2+1)
C                                       "Weight" calibration
               GWT = GM
               IF (GWT.GT.1.0E-10) GWT = 1.0 / (GWT * GWT)
C                                       Loop thru channel.
 60         DO 100 IFQ = BCHAN,ECHAN
               INDEX = (JOFF + (IFQ-1) * KNCF) * INCPX + 1
               TVR = GM * (VIS(INDEX) + GA)
               TVI = GM * (VIS(INDEX+1) - GA)
               PFLG = VIS(INDEX+2).LE.0.0
               SMPFLG = SMPFLG .OR. PFLG
               ALPFLG = ALPFLG .AND. PFLG
               SOMFLG = SOMFLG .OR. ((GWT.LE.0.0) .AND. (.NOT.PFLG))
               ALLFLG = ALLFLG .AND. ((GWT.LE.0.0) .AND. (.NOT.PFLG))
               VIS(INDEX) = TVR
               VIS(INDEX+1) = TVI
               VIS(INDEX+2) = VIS(INDEX+2) * GWT
 100           CONTINUE
 200        CONTINUE
C                                       Setup for next IF
         JNDXA2 = JNDXA2 + LCUCAL * NUMPOL
 300     CONTINUE
C                                       Increment counts of the
C                                       good, bad and the ugly.
      SOMFLG = SOMFLG .AND. (.NOT.ALLFLG)
      SMPFLG = SMPFLG .AND. (.NOT.ALPFLG)
      ALLBAD =  ALLFLG .OR. ALPFLG
      SOMBAD = (SOMFLG.OR.SMPFLG) .AND. (.NOT.ALLBAD)
      IF (SMPFLG) CNTREC(1,1) = CNTREC(1,1) + 1
      IF (SOMFLG) CNTREC(1,2) = CNTREC(1,2) + 1
      IF (SOMBAD) CNTREC(1,3) = CNTREC(1,3) + 1
      IF (ALPFLG) CNTREC(2,1) = CNTREC(2,1) + 1
      IF (ALLFLG) CNTREC(2,2) = CNTREC(2,2) + 1
      IF ((.NOT.ALLBAD) .AND. (.NOT.SOMBAD))
     *   CNTREC(2,3) = CNTREC(2,3) + 1
      DROP = ALLBAD
      GO TO 999
C
 999  RETURN
      END
