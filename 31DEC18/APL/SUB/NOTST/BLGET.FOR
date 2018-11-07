      SUBROUTINE BLGET (TIME, IERR)
C-----------------------------------------------------------------------
C! Sets up for interpolation in baseline (BL) table
C# Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2007, 2012
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
C    Sets up for interpolation in baseline (BL) table, reads values
C    from BL table.
C    Inputs:
C       TIME     R        Current data time.
C    Inputs from common /SELCAL/:
C       BLBUFF   I(*)     BL table I/O TABIO buffer
C       IBLRNO   I        Current BL record number
C       NBLINR   I        Number of BL records in file.
C       NUMANT   I        Number of antennas
C       NUMPOL   I        Number of polarizations
C       NUMIF    I        Number of IFs.
C    Output:
C       IERR     I       Return error code 0=>OK, else failed.
C    Output to common /SELCAL/:
C       BLTAB    R(*,2)  Baseline table from BL table file.
C                        The second dimension corresponds to the time
C                        before and after the current data time.
C                        Indexing scheme: an entry defined by ant1<ant2
C                        starts in element:
C          lentry * (((ant1-1)*numant-((ant1+1)*ant1)/2 + ant2) - 1) + 1
C                        where lentry = 2 * NUMPOL * (EIF-BIF+1)
C                        An entry contains the values in order:
C                        By IF (NUMIF)
C                           By Polarization (NUMPOL)
C                               Real part, imaginary part.
C       BLTIM    R(3)     Time of two BL entries; third value is
C                         time of current values.
C       IBLP1    I       Pointer in BLTAB, BLTIM to previous time.
C       IBLP2    I       Pointer in BLTAB, BLTIM to next time.
C   Useage note:  The BL table is open/initialized in GAININ and
C   closed in UVGET.
C-----------------------------------------------------------------------
      REAL      TIME
      INTEGER   IERR
C
      INTEGER   IFNO, LENTRY, ANT1, ANT2, BLNDX, IFOFF,
     *   TIMKOL, SOUKOL, SUBKOL, ANT1KL, ANT2KL, REM1KL, IMM1KL,
     *   REA1KL, IMA1KL, REM2KL, IMM2KL, REA2KL, IMA2KL, FRQKOL
      INTEGER   I, INDEX, LIMIT, NBLANK, NUMBL
      LOGICAL   T, F, GOT1
      DOUBLE PRECISION CURTIM
      INCLUDE 'INCS:PUVD.INC'
      INTEGER  RECI(6+8*MAXIF)
      REAL     RECR(6+8*MAXIF)
      DOUBLE PRECISION RECD(6+8*MAXIF)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (RECI, RECR, RECD)
      EQUIVALENCE (BLKOLS(1), TIMKOL), (BLKOLS(2), SOUKOL),
     *   (BLKOLS(3), SUBKOL), (BLKOLS(4),ANT1KL), (BLKOLS(5), ANT2KL),
     *   (BLKOLS(6), FRQKOL), (BLKOLS(7), REM1KL), (BLKOLS(8),IMM1KL),
     *   (BLKOLS(9), REA1KL), (BLKOLS(10), IMA1KL),
     *   (BLKOLS(11), REM2KL), (BLKOLS(12), IMM2KL),
     *   (BLKOLS(13), REA2KL), (BLKOLS(14),IMA2KL)
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Check if want to keep contents
C                                       of BLTAB
      IF (IBLRNO.GE.NBLINR) GO TO 999
C                                       Check if first call
      IF (IBLP2.GT.0) GO TO 300
C                                       Blank fill BLTAB
      NUMBL = (NUMANT * (NUMANT+1)) / 2
      NBLANK = NUMBL * (EIF-BIF+1) * NUMPOL * 2
      DO 40 I = 1,NBLANK
         BLTAB(I,1) = FBLANK
         BLTAB(I,2) = FBLANK
 40      CONTINUE
C                                       Read until selected time.
      LENTRY = 2 * NUMPOL * (EIF - BIF + 1)
      IBLP1 = 2
      IBLP2 = 1
      GOT1 = F
      LIMIT = IBLRNO
      DO 80 I = LIMIT,NBLINR
         IBLRNO = I
         CALL TABIO ('READ', 0, IBLRNO, RECR, BLBUFF, IERR)
         IF (IERR.LT.0) GO TO 80
         IF (IERR.NE.0) GO TO 990
C                                       Check SUBARRAY
         IF ((RECI(SUBKOL).NE.SUBARR) .AND. (RECI(SUBKOL).GT.0) .AND.
     *      (SUBARR.GT.0)) GO TO 100
C                                       Check FRQSEL
         IF ((RECI(FRQKOL).NE.FRQSEL) .AND. (RECI(FRQKOL).GT.0) .AND.
     *      (FRQSEL.GE.0)) GO TO 100
C                                       See if time past data time.
         IF (RECR(TIMKOL).GT.TIME) GO TO 100
C                                       Fill in values
         GOT1 = T
         BLTIM(1) = RECR(TIMKOL)
C                                       Loop over IF
         ANT1 = RECI(ANT1KL)
         ANT2 = RECI(ANT2KL)
         BLNDX = ((ANT1-1)*NUMANT) - ((ANT1-1)*ANT1)/2 + ANT2
         INDEX = LENTRY * (BLNDX - 1) + 1
         DO 70 IFNO = BIF,EIF
            IFOFF = IFNO - 1
            BLTAB(INDEX,1) = RECR(REM1KL+IFOFF)
            BLTAB(INDEX+1,1) = RECR(IMM1KL+IFOFF)
            INDEX = INDEX + 2
            IF (NUMPOL.GT.1) THEN
C                                       Second polarization
               BLTAB(INDEX,1) = RECR(REM2KL+IFOFF)
               BLTAB(INDEX+1,1) = RECR(IMM2KL+IFOFF)
               INDEX = INDEX + 2
               END IF
 70         CONTINUE
 80      CONTINUE
C                                       After time, see if have before
 100  IF (GOT1) GO TO 300
C                                       Only have bl after data time.
      IBLP1 = -1
      IBLP2 = 1
      GO TO 310
C                                       Next record
C                                       Initialize BLTAB
 300  IBLP1 = IBLP2
      IBLP2 = 3 - IBLP2
C                                       Check if BL table exhausted,
C                                       If so just mark tables,
      IF (IBLRNO.GE.NBLINR) IBLP2 = -1
      IF (IBLRNO.GE.NBLINR) GO TO 999
C                                       Blank fill table
 310  NBLANK = NUMANT * (EIF-BIF+1) * NUMPOL * 2
      DO 340 I = 1,NBLANK
         BLTAB(I,IBLP2) = FBLANK
 340     CONTINUE
      LIMIT = IBLRNO
      DO 380 I = LIMIT,NBLINR
         IBLRNO = I
         CALL TABIO ('READ', 0, IBLRNO, RECR, BLBUFF, IERR)
         IF (IERR.LT.0) GO TO 380
         IF (IERR.NE.0) GO TO 990
C                                       Check SUBARRAY
         IF ((RECI(SUBKOL).NE.SUBARR) .AND. (RECI(SUBKOL).GT.0) .AND.
     *      (SUBARR.GT.0)) GO TO 380
C                                       Check FRQSEL
         IF ((RECI(FRQKOL).NE.FRQSEL) .AND. (RECI(FRQKOL).GT.0) .AND.
     *      (FRQSEL.GT.0)) GO TO 380
         IF (I.EQ.LIMIT) CURTIM = RECR(TIMKOL)
C                                       See if time past data time.
         IF (RECR(TIMKOL).GT.CURTIM) GO TO 400
         BLTIM(IBLP2) = CURTIM
C                                       Fill in values
         GOT1 = T
C                                       Loop over IF
         ANT1 = RECI(ANT1KL)
         ANT2 = RECI(ANT2KL)
         BLNDX = ((ANT1-1)*NUMANT) - ((ANT1+1)*ANT1)/2 + ANT2
         INDEX = LENTRY * (BLNDX - 1) + 1
         DO 370 IFNO = BIF,EIF
            IFOFF = IFNO - 1
            BLTAB(INDEX,IBLP2) = RECR(REM1KL+IFOFF)
            BLTAB(INDEX+1,IBLP2) = RECR(IMM1KL+IFOFF)
            INDEX = INDEX + 2
            IF (NUMPOL.GT.1) THEN
C                                       Second polarization
               BLTAB(INDEX,IBLP2) = RECR(REM2KL+IFOFF)
               BLTAB(INDEX+1,IBLP2) = RECR(IMM2KL+IFOFF)
               INDEX = INDEX + 2
               END IF
 370        CONTINUE
 380     CONTINUE
C                                       Finished file
      IBLRNO = NBLINR
      GO TO 999
C                                       Check if correct time
 400  IF (CURTIM.LT.TIME) GO TO 300
      GO TO 999
C                                       TABIO error
 990  WRITE (MSGTXT,1400) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1400 FORMAT ('BLGET: ERROR ',I3,' READING BASELINE TABLE')
      END
