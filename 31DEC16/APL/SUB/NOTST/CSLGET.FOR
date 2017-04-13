      SUBROUTINE CSLGET (TIME, IERR)
C-----------------------------------------------------------------------
C! Reads CL (or SN) table and sets up for interpolation.
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-2000, 2006, 2012-2013, 2015
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
C   Sets up for interpolation in cal (CL) table, reads values from cal
C   table.  Assumes only valid, selected data in open cal table.  Uses
C   calls to TABIO directly rather than TABCAL for efficiency.
C   Inputs:
C      TIME     R        Current data time.
C   Inputs from common /SELCAL/:
C      GMMOD    R        Mean gain modulus correction, 0=>none.
C      CLBUFF   I(*)     Cal table I/O TABIO buffer
C      ICLRNO   I        Current cal record number
C      NCLINR   I        Number of cal records in file.
C      NUMANT   I        Number of antennas
C      NUMPOL   I        Number of IFs per group (polarizations)
C      NUMIF    I        Number of IFs.
C      GMMOD    R        Mean cal modulus
C   Output:
C      IERR     I        Return error code 0=>OK, else failed.
C   Output to common /SELCAL/:
C      CALTAB   R(*,2)   Cal. table from cal table file
C                        Includes GMMOD correction if necessary.
C                        Values in order:
C                        By antenna (NUMANT)
C                           By IF (NUMIF)
C                              By Polarization (NUMPOL)
C                                  Real part, imaginary part,
C                                  group delay, phase rate, ref. ant.
C      LCLTAB   I        Number of values in CALTAB per entry (5)
C      IFRTAB   R(*,2)   Ionospheric Faraday rotation measure from
C                        cal table. Values listed by antenna.
C      DDTAB    R(*,2)   Dispersive delays from cal table, listed by
C                        antenna
C      CALTIM   R(3)     1: time of latest cal record before TIME
C                        2: time of earliest cal record after TIME
C                        3: time of earliest cal record after TIME
C      CIDSOU   I(2)     Previous/next source ID number using ICALPn as
C                        a pointer.
C-----------------------------------------------------------------------
      REAL      TIME
      INTEGER   IERR
C
C                                       Length mil-sec, sec,2 hour(days)
      REAL      SECOND, MILSEC, TWOHRS, TEPS
      PARAMETER (SECOND=1.0/86400.0, MILSEC=SECOND/1.E3, TWOHRS=1.0/12.)
c
      INCLUDE   'INCS:PUVD.INC'
      INTEGER   I, JCOP, ANTNO, IFNO, LGREC, LGRECF, TIMKOL, SOUKOL,
     *   ANTKOL, SUBKOL, FRQKOL, IFRKOL, DDLKOL, DDDKOL,
     *   RE1KOL, IM1KOL, DL1KOL, RA1KOL, RF1KOL, WT1KOL,
     *   RE2KOL, IM2KOL, DL2KOL, RA2KOL, RF2KOL, WT2KOL,
     *   RECI(50+15*MAXIF), IFOFF, INDEX, LIMIT1, LIMIT2, NBLANK
      LOGICAL   GOT1, GOTSOM
      DOUBLE PRECISION RECD(50+15*MAXIF)
      REAL     RECR(50+15*MAXIF), GMMI, WT1, WT2
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DNXC.INC'
      EQUIVALENCE (RECI, RECR, RECD)
      EQUIVALENCE (TIMKOL,CLKOLS(CTDTIM)),  (SOUKOL,CLKOLS(CTISID)),
     *   (ANTKOL,CLKOLS(CTIANT)), (SUBKOL,CLKOLS(CTISUB)),
     *   (FRQKOL,CLKOLS(CTIFQI)), (IFRKOL,CLKOLS(CTRIFR)),
     *   (DDLKOL,CLKOLS(CTRDDL)), (DDDKOL,CLKOLS(CTRDDD)),
     *   (RE1KOL,CLKOLS(CTRRE1)), (IM1KOL,CLKOLS(CTRIM1)),
     *   (RA1KOL,CLKOLS(CTRRA1)), (DL1KOL,CLKOLS(CTRDE1)),
     *   (RE2KOL,CLKOLS(CTRRE2)), (IM2KOL,CLKOLS(CTRIM2)),
     *   (RA2KOL,CLKOLS(CTRRA2)), (DL2KOL,CLKOLS(CTRDE2)),
     *   (RF1KOL,CLKOLS(CTIRF1)), (RF2KOL,CLKOLS(CTIRF2)),
     *   (WT1KOL,CLKOLS(CTRWE1)), (WT2KOL,CLKOLS(CTRWE2))
C-----------------------------------------------------------------------
C                                       Gain modulus correction
      TEPS = 5.0 * MILSEC
      GMMI = 1.0
      IF (GMMOD.GT.1.0E-10) GMMI = 1.0 / GMMOD
C                                       pointers
      LGREC = NUMPOL * LCLTAB
      LGRECF = LGREC * (EIF - BIF + 1)
      JCOP = LCLTAB * MIN (2, MAX (1, NUMPOL))
C                                       new scan - blank fill
      IF ((CURNXC.LE.0) .OR. (TIME.LT.TIMNXC(CURNXC)) .OR.
     *   (TIME.GE.TIMNXC(CURNXC+1))) THEN
         DO 10 I = 1,NUMNXC
            IF ((TIME.GE.TIMNXC(I)) .AND. (TIME.LT.TIMNXC(I+1)))
     *         CURNXC = I
 10         CONTINUE
         NBLANK = NUMANT * (EIF-BIF+1) * NUMPOL * LCLTAB
         IF (NBLANK.GT.XCTBSZ) THEN
            MSGTXT = 'TOO MANY ANTENNAS*IFS*POLARIZATIONS FOR CODE'
            CALL MSGWRT (8)
            IERR = 8
            GO TO 999
            END IF
         CALL RFILL (NBLANK, FBLANK, CALTAB(1,1))
         CALL RFILL (NBLANK, FBLANK, CALTAB(1,2))
         NBLANK = 2 * MAXANT
         CALL RFILL (NBLANK, -1.E10, TIMECL)
         CALL RFILL (NBLANK, FBLANK, IFRTAB)
         CALL RFILL (NBLANK, FBLANK, DDTAB)
         ICALP1 = 1
         ICALP2 = 2
C                                       later call: shift times
      ELSE
         DO 30 I = 1,NUMANT
C                                       2nd time exceeded - shift down
            IF ((TIME.GT.TIMECL(2,I)) .AND. (TIMECL(2,I).GT.-100.)) THEN
               TIMECL(1,I) = TIMECL(2,I)
               IFRTAB(I,1) = IFRTAB(I,2)
               DDTAB(I,1)  = DDTAB(I,2)
               DO 20 IFNO = BIF,EIF
                  INDEX = LGRECF * (I-1) +  LGREC * (IFNO-BIF) + 1
                  CALL RCOPY (JCOP, CALTAB(INDEX,2), CALTAB(INDEX,1))
 20               CONTINUE
               END IF
 30         CONTINUE
         END IF
C                                       Read until selected time.
      LIMIT1 = RECNXC(CURNXC)
      IF (LIMIT1.LE.0) LIMIT1 = RECNXC(MAX(1,CURNXC-1))
      LIMIT1 = MAX (1, LIMIT1)
      LIMIT2 = RECNXC(CURNXC+1) - 1
      IF (LIMIT2.LT.LIMIT1) LIMIT2 = RECNXC(MIN(CURNXC+2,NUMNXC))
      IF (LIMIT2.LT.LIMIT1) LIMIT2 = MIN (NCLINR, RECNXC(NUMNXC)-1)
      LIMIT2 = MIN (NCLINR, LIMIT2)
      DO 90 I = LIMIT1,LIMIT2
         ICLRNO = I
         CALL TABIO ('READ', 0, ICLRNO, RECR, CLBUFF, IERR)
         IF (IERR.LT.0) GO TO 90
         IF (IERR.NE.0) GO TO 990
C                                       Check SUBARRAY
         IF ((RECI(SUBKOL).NE.SUBARR) .AND. (RECI(SUBKOL).GT.0))
     *      GO TO 70
C                                       Check FRQSEL
         IF ((RECI(FRQKOL).NE.FRQSEL) .AND. (RECI(FRQKOL).GT.0) .AND.
     *      (FRQSEL.GT.0)) GO TO 70
C                                       Check source
         IF ((RECI(SOUKOL).NE.CURSOU) .AND. (RECI(SOUKOL).GT.0) .AND.
     *      (CURSOU.GT.0)) GO TO 70
C                                       antenna number
         ANTNO = RECI(ANTKOL)
C                                       time -> include this one
         IF (TIME.GE.TIMECL(2,ANTNO)) THEN
            IF (TIMECL(1,ANTNO).GT.-100.) THEN
               TIMECL(1,ANTNO) = TIMECL(2,ANTNO)
               IFRTAB(ANTNO,1) = IFRTAB(ANTNO,2)
               DDTAB(ANTNO,1)  = DDTAB(ANTNO,2)
               DO 50 IFNO = BIF,EIF
                  INDEX = LGRECF * (ANTNO-1) +  LGREC * (IFNO-BIF) + 1
                  CALL RCOPY (JCOP, CALTAB(INDEX,2), CALTAB(INDEX,1))
 50               CONTINUE
               END IF
C                                       Fill in values
            GOT1 = .TRUE.
            IF (IFRKOL.GT.0) THEN
               IFRTAB(ANTNO,2) = RECR(IFRKOL)
            ELSE
               IFRTAB(ANTNO,2) = 0.0
               END IF
            IF (DDLKOL.GT.0) THEN
               DDTAB(ANTNO,2) = RECR(DDLKOL)
            ELSE
               DDTAB(ANTNO,2) = 0.0
               END IF
            TIMECL(2, ANTNO) = RECD(TIMKOL)
C                                       Loop over IF
            DO 60 IFNO = BIF,EIF
               IFOFF = IFNO - 1
               INDEX = LGRECF * (ANTNO-1) +  LGREC * (IFNO-BIF) + 1
               WT1 = RECR(WT1KOL+IFOFF)
               CALTAB(INDEX,2)   = RECR(RE1KOL+IFOFF)
               CALTAB(INDEX+1,2) = RECR(IM1KOL+IFOFF)
               CALTAB(INDEX+2,2) = RECR(DL1KOL+IFOFF)
               CALTAB(INDEX+3,2) = RECR(RA1KOL+IFOFF)
               CALTAB(INDEX+4,2) = RECI(RF1KOL+IFOFF)
               IF (WT1.LE.0.0) THEN
                  CALTAB(INDEX,2) = FBLANK
                  CALTAB(INDEX+1,2) = FBLANK
               ELSE
                  IF (CALTAB(INDEX,2).NE.FBLANK) CALTAB(INDEX,2) =
     *               CALTAB(INDEX,2) * GMMI
                  IF (CALTAB(INDEX+1,2).NE.FBLANK) CALTAB(INDEX+1,2) =
     *               CALTAB(INDEX+1,2) * GMMI
                  END IF
C                                       Second polarization
               IF (NUMPOL.GE.2) THEN
                  INDEX = INDEX + LCLTAB
                  WT2 = RECR(WT2KOL+IFOFF)
                  CALTAB(INDEX,2)   = RECR(RE2KOL+IFOFF)
                  CALTAB(INDEX+1,2) = RECR(IM2KOL+IFOFF)
                  CALTAB(INDEX+2,2) = RECR(DL2KOL+IFOFF)
                  CALTAB(INDEX+3,2) = RECR(RA2KOL+IFOFF)
                  CALTAB(INDEX+4,2) = RECI(RF2KOL+IFOFF)
                  IF (WT2.LE.0.0) THEN
                     CALTAB(INDEX,2) = FBLANK
                     CALTAB(INDEX+1,2) = FBLANK
                  ELSE
                     IF (CALTAB(INDEX,2).NE.FBLANK) CALTAB(INDEX,2) =
     *                  CALTAB(INDEX,2) * GMMI
                     IF (CALTAB(INDEX+1,2).NE.FBLANK) CALTAB(INDEX+1,2)
     *                  = CALTAB(INDEX+1,2) * GMMI
                     END IF
                  END IF
 60            CONTINUE
            IF (TIMECL(1,ANTNO).LE.-100.) THEN
               TIMECL(1,ANTNO) = TIMECL(2,ANTNO)
               IFRTAB(ANTNO,1) = IFRTAB(ANTNO,2)
               DDTAB(ANTNO,1)  = DDTAB(ANTNO,2)
               DO 65 IFNO = BIF,EIF
                  INDEX = LGRECF * (ANTNO-1) +  LGREC * (IFNO-BIF) + 1
                  CALL RCOPY (JCOP, CALTAB(INDEX,2), CALTAB(INDEX,1))
 65               CONTINUE
               END IF
            GO TO 90
            END IF

C                                       this one not needed - done ?
C                                       set restart mark
 70      GOTSOM = .FALSE.
         DO 80 ANTNO = 1,MAXANT
            IF (TIMECL(1,ANTNO).GE.-100.0) THEN
               IF (TIME.GE.TIMECL(2,ANTNO)+TEPS) GO TO 90
               IF ((TIME.LE.TIMECL(2,ANTNO)+TEPS) .AND.
     *            (TIME.GE.TIMECL(1,ANTNO)-TEPS)) GOTSOM = .TRUE.
               END IF
 80         CONTINUE
C                                       no more to fill in
         IF (GOTSOM) GO TO 100
 90      CONTINUE
C                                       Finished file
 100  CALTIM(3) = 1.0E10
      CALTIM(2) = 1.0E10
      CALTIM(1) = -1.0E10
C     TEPS = 0.4*SECOND
      DO 110 ANTNO = 1,NUMANT
         IF (TIMECL(1,ANTNO).GE.-100.0) THEN
            IF (TIME.GE.TIMECL(1,ANTNO)-TEPS) CALTIM(1) =
     *         MAX (CALTIM(1), TIMECL(1,ANTNO)-TEPS)
            IF (TIME.LE.TIMECL(2,ANTNO)+TEPS) CALTIM(2) =
     *         MIN (CALTIM(2), TIMECL(2,ANTNO)+TEPS)
            END IF
 110     CONTINUE
      IF ((CALTIM(1).LT.-1000.0) .AND. (CALTIM(2).GT.10000.0)) THEN
         CALTIM(1) = TIME - TEPS
         CALTIM(2) = TIME + TEPS
         NBLANK = NUMANT * (EIF-BIF+1) * NUMPOL * LCLTAB
         CALL RFILL (NBLANK, FBLANK, CALTAB(1,1))
         CALL RFILL (NBLANK, FBLANK, CALTAB(1,2))
         NBLANK = 2 * MAXANT
         CALL RFILL (NBLANK, -1.E10, TIMECL)
         CALL RFILL (NBLANK, FBLANK, IFRTAB)
         CALL RFILL (NBLANK, FBLANK, DDTAB)
      ELSE IF (CALTIM(1).LT.-1000.0) THEN
         CALTIM(1) = TIME - TEPS
      ELSE IF (CALTIM(2).GT.10000.0) THEN
         CALTIM(2) = TIME + TEPS
         END IF
      CALTIM(3) = CALTIM(2)
      GO TO 999
C                                       TABIO error
 990  WRITE (MSGTXT,1990) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('CSLGET: ERROR ',I3,' READING CALIBRATION TABLE')
      END
