      SUBROUTINE BLSET (TIME, IERR)
C-----------------------------------------------------------------------
C! Fills current baseline calibration table
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2000, 2012
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
C   Fills current baseline correction table (BLFAC) by interpolation.
C   Inputs:
C      TIME    R        Current time (of data) in days.
C     Inputs from common /SELCAL/:
C      BLTAB   R(*,2)   Baseline table from BL table file.
C                       The second dimension corresponds to the time
C                       before and after the current data time.
C                       Indexing scheme: an entry defined by ant1<ant2
C                       starts in element:
C         lentry * (((ant1-1)*numant-((ant1+1)*ant1)/2 + ant2) - 1) + 1
C                       where lentry = 2 * NUMPOL * (EIF-BIF+1)
C                       An entry contains the values in order:
C                       By IF (NUMIF)
C                          By Polarization (NUMPOL)
C                              Real part, imaginary part.
C      BLTIM   R(3)     Time of two cal. entries; third value is
C                       time of current values.
C      IBLP1   I        Pointer in BLTAB, BLTIM to previous time.
C      IBLP2   I        Pointer in BLTAB, BLTIM to next time.
C      DOBL    L        If true then use baseline correction table,
C                       else return
C   Output:
C      IERR    I        Return error code, 0=>OK else error.
C   Output to common /SELCAL/:
C      BLFAC   R        Baseline dependent factors as (real, imag)
C                       Includes GMMOD correction if necessary.
C                       Addressing is like BLTAB.
C-----------------------------------------------------------------------
      INTEGER   IERR
      REAL      TIME
C
      INTEGER   LOOP, NUMBER, JBLP1, JBLP2
      LOGICAL   GOOD1, GOOD2
      REAL      WTT1, WTT2, G1R, G1I, G2R, G2I, CATR(256), WT1, WT2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATR)
C-----------------------------------------------------------------------
      IERR = 0
C                                       Check if baseline calibration
      IF (.NOT.DOBL) THEN
         IBLP1 = 1
         GO TO 999
         END IF
C                                       size of array
      NUMBER = (NUMANT * (NUMANT+1)) / 2
      NUMBER = NUMBER * (EIF-BIF+1) * NUMPOL * 2
C                                       Set pointer
      JBLP2 = MAX (1, IBLP2)
C                                       See if time for new table entry
      IF ((TIME.GT.BLTIM(JBLP2)) .OR. (IBLP2.LT.0))
     *   CALL BLGET (TIME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Reset pointers
      JBLP1 = MAX (1, IBLP1)
      JBLP2 = MAX (1, IBLP2)
      IF (IBLP1.LT.0) IBLP1 = -2
      IF (IBLP2.LT.0) IBLP2 = -2
      BLTIM(3) = TIME
C                                       Set interpolation weights.
      WTT1 = 0.0
      IF ((IBLP1.GT.0) .AND. (BLTIM(JBLP1).NE.BLTIM(JBLP2)))
     *   WTT1 = (BLTIM(JBLP2) - TIME) / (BLTIM(JBLP2) -
     *      BLTIM(JBLP1))
      WTT2 = 1.0 - WTT1
C                                       See if only BL cal. before data
      IF (IBLP2.LT.0) WTT1 = 1.0
      IF (IBLP2.LT.0) WTT2 = 0.0
      DO 500 LOOP = 1,NUMBER,2
C                                       Initialize soln with blanks
         BLFAC(LOOP) = FBLANK
         BLFAC(LOOP+1) = FBLANK
C                                       Check for blanked soln.
         GOOD1 = (BLTAB(LOOP,JBLP1).NE.FBLANK) .AND.
     *           (BLTAB(LOOP+1,JBLP1).NE.FBLANK) .AND.
     *           (WTT1.GT.0.0)
         GOOD2 = (BLTAB(LOOP,JBLP2).NE.FBLANK) .AND.
     *           (BLTAB(LOOP+1,JBLP2).NE.FBLANK) .AND.
     *           (WTT2.GT.0.0)
         IF (.NOT.(GOOD1.OR.GOOD2)) GO TO 500
            WT1 = WTT1
            WT2 = WTT2
            IF (.NOT.GOOD1) WT1 = 0.0
            IF (.NOT.GOOD1) WT2 = 1.0
            IF (.NOT.GOOD2) WT1 = 1.0
            IF (.NOT.GOOD2) WT2 = 0.0
C                                       Set values
            G1R = BLTAB(LOOP,JBLP1)
            G1I = BLTAB(LOOP+1,JBLP1)
            G2R = BLTAB(LOOP,JBLP2)
            G2I = BLTAB(LOOP+1,JBLP2)
C                                       Interpolate
            BLFAC(LOOP) = WT1 * G1R + WT2 * G2R
            BLFAC(LOOP+1) = WT1 * G1I + WT2 * G2I
 500        CONTINUE
      GO TO 999
C                                       Initialize BLFAC, reset IBLP1
C 600  IBLP1 = 1
C      DO 650 LOOP = 1,NUMBER,2
C         BLFAC(LOOP) = 1.0
C         BLFAC(LOOP+1) = 0.0
C 650     CONTINUE
C
 999  RETURN
      END
