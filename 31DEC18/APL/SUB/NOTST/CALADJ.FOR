      SUBROUTINE CALADJ (IUDISK, IUCNO, CATBLK, SUBARR, SNVER, REFANT,
     *   REFUSE, REFUSS, SMOTIM, IRET)
C-----------------------------------------------------------------------
C! Adjusts solution (SN) table phases to a common reference antenna.
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2005, 2011-2012, 2015
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
C   CALADJ massages the solutions so that interpolation between points
C   is reasonable.  An antenna is referenced to the reference antenna
C   by smoothing/interpolation the existing solution between antennas to
C   times where the desired reference antenna was not present.
C   Input:
C      IUDISK  I       Disk number
C      IUCNO   I       Catalog slot number
C      CATBLK  I(256)  Catalog header.
C      SUBARR  I       Subarray number, 0=> all
C      SNVER   I       SN table version number to adjust.
C      REFANT  I       Reference antenna, 0=>pick
C      REFUSE  I(*)    Useage counts for reference antennas
C   Input/output
C      SMOTIM  R(3)    Smoothing times in days to use for determining
C                      reference phases.
C   Output:
C      IRET   I     Return error code. 0 => OK, otherwise error.
C
C   Note:
C      CALADJ dynamically allocates workspace depending on the size of
C      the SN table to be adjusted.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IUDISK, IUCNO, CATBLK(256), SUBARR, SNVER, REFANT,
     *   REFUSE(*), REFUSS(MAXANT,*), IRET
      REAL      SMOTIM(3)
C
      INCLUDE 'INCS:PSNTAB.INC'
      CHARACTER COLHED(2)*24, BNDCOD(MAXIF)*8
      INTEGER   ANT, REF, TAKOLS(2), LUN, LOOP, NKEY, LKEY, FRQSEL,
     *   IFLOOP, KEY(2,2), IIVER, NIF, IERR, KOLS(18), MXINDX, REFTMP,
     *   LIMS1, LIMS2, LOOPSA, NUMNOD, LUN2, ISNRNO, MXCNT,
     *   CLKOLS(MAXCLC), CLNUMV(MAXCLC), NUMANT, NUMPOL, NUMIF,
     *   MBKOLS(14), KEYSUB(2,2)
      LOGICAL   T, F, ISAPPL, DOIT
      REAL      FKEY(2,2), GMMOD
      DOUBLE PRECISION RANOD(25), DECNOD(25)
      INTEGER   NUMROW
      LONGINT   OFFSET
      REAL      WORK(1)
      LOGICAL   NOTREF
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INTEGER   ISBAND(MAXIF)
      DOUBLE PRECISION FOFF(MAXIF), FREQIF
      REAL FINC(MAXIF)
C
      INTEGER   BUFSZ
C
      INTEGER   TBBFSZ
      PARAMETER (TBBFSZ = 512)
      INTEGER   BUFFER(TBBFSZ)
      INCLUDE 'INCS:ZPBUFSZ.INC'
      DATA NKEY, LKEY /2,24/
      DATA T, F /.TRUE.,.FALSE./
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /1,1,1,1/
      DATA COLHED /'TIME', 'ANTENNA NO.'/
C-----------------------------------------------------------------------
C                                       Initialize OFFSET; if OFFSET = 0
C                                       then workspace has not been
C                                       allocated
      OFFSET = 0
C                                       NOTREF indicates that some
C                                       antenna solutions were not
C                                       adjusted.
      NOTREF = .FALSE.
C                                       See if any work to be done.
      DOIT = F
      MXCNT = 0
      DO 5 LOOP = 1,MAXANT
         DOIT = DOIT .OR. ((REFUSE(LOOP).GT.0) .AND. (LOOP.NE.REFANT))
         IF (REFUSE(LOOP).GT.MXCNT) MXINDX = LOOP
         IF (REFUSE(LOOP).GT.MXCNT) MXCNT = REFUSE(LOOP)
 5       CONTINUE
      IF (.NOT.DOIT) GO TO 999
C                                       If no REFANT specified pick the
C                                       one with the most solutions.
      REFTMP = REFANT
      IF (REFTMP.LE.0) REFTMP = MXINDX
      REFANT = REFTMP
C                                       Check again if any work to do
      DOIT  = F
      DO 10 LOOP = 1,MAXANT
         DOIT = DOIT .OR. ((REFUSE(LOOP).GT.0) .AND. (LOOP.NE.REFTMP))
 10      CONTINUE
      IF (.NOT.DOIT) GO TO 999
C                                       Message
      WRITE (MSGTXT,2000) REFTMP
      CALL MSGWRT (6)
      LUN = 29
      LUN2 = 30
C                                       Get IF frequencies
      FOFF(1) = 0.0
      IIVER = 1
      IF (NUMIF.GT.1)
     *   CALL CHNDAT ('READ', BUFFER, IUDISK, IUCNO, IIVER, CATBLK, LUN,
     *      NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
C                                       Open solution table
C                                       Reformat?
      CALL SNREFM (IUDISK, IUCNO, SNVER, CATBLK, LUN, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL SNINI ('READ', BUFFER, IUDISK, IUCNO, SNVER, CATBLK, LUN,
     *   ISNRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.GT.0) GO TO 990
C
      NUMROW = BUFFER(5)
C                                       Set column pointers for sort
      CALL FNDCOL (NKEY, COLHED, LKEY, T, BUFFER, TAKOLS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 980
         END IF
C                                       Close
      CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1015) IRET
         GO TO 980
         END IF
C                                       Sort to time-ant order.
      KEY(1,1) = TAKOLS(1)
      KEY(2,1) = TAKOLS(1)
      KEY(1,2) = TAKOLS(2)
      KEY(2,2) = TAKOLS(2)
      CALL TABSRT (IUDISK, IUCNO, 'SN', SNVER, SNVER, KEY, KEYSUB, FKEY,
     *   BUFFER, CATBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 980
         END IF
C                                       Allocate workspace for CALREF.
C                                       If each of the 5 scratch arrays
C                                       has NUMROW then CALREF is
C                                       guaranteed to have enough space
C                                       to re-refence the table. In most
C                                       cases this will be less than a
C                                       megabyte of data so the
C                                       allocation should rarely fail.
C                                       A single array is allocated and
C                                       divided up for the CALREF calls.
      BUFSZ = MAX (5 * NUMROW, UVBFSL)
      BUFSZ = (BUFSZ - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'CALADJ', BUFSZ, WORK, OFFSET, IRET)
      IF (IRET .NE. 0) THEN
         MSGTXT = 'Could not allocate enough workspace to reference'
         CALL MSGWRT (6)
         MSGTXT = 'solutions to a common antenna. Solutions will not'
         CALL MSGWRT (6)
         MSGTXT = 'be adjusted. If this is not acceptable then either'
         CALL MSGWRT (6)
         MSGTXT = 'increase SOLINT or shutdown some applications and'
         CALL MSGWRT (6)
         MSGTXT = 'run the program again.'
         IRET = 0
         GO TO 999
         END IF
C                                       Find number of subarrays
      IF (SUBARR.GT.0) THEN
         LIMS1 = SUBARR
         LIMS2 = SUBARR
      ELSE
         LIMS1 = 1
         CALL FNDEXT ('AN', CATBLK, LIMS2)
         LIMS2 = MAX (1, LIMS2)
         END IF
C                                       Open for write
      CALL SNINI ('WRIT', BUFFER, IUDISK, IUCNO, SNVER, CATBLK, LUN,
     *   ISNRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       Set column pointers, for each
C                                       IF: antenna, ref. antenna,
C                                       subarray, weight, time,
C                                       real, imag, delay, rate.
      KOLS(1) = CLKOLS(SNIANT)
      KOLS(2) = CLKOLS(SNIRF1)
      KOLS(3) = CLKOLS(SNISUB)
      KOLS(4) = CLKOLS(SNRWE1)
      KOLS(5) = CLKOLS(SNDTIM)
      KOLS(6) = CLKOLS(SNRRE1)
      KOLS(7) = CLKOLS(SNRIM1)
      KOLS(8) = CLKOLS(SNRDE1)
      KOLS(9) = CLKOLS(SNRRA1)
      KOLS(10) = CLKOLS(SNIANT)
      KOLS(11) = CLKOLS(SNIRF2)
      KOLS(12) = CLKOLS(SNISUB)
      KOLS(13) = CLKOLS(SNRWE2)
      KOLS(14) = CLKOLS(SNDTIM)
      KOLS(15) = CLKOLS(SNRRE2)
      KOLS(16) = CLKOLS(SNRIM2)
      KOLS(17) = CLKOLS(SNRDE2)
      KOLS(18) = CLKOLS(SNRRA2)
C                                       Equivalent for multi-band delay
C                                       re-referencing.
      MBKOLS(1) = CLKOLS(SNIANT)
      MBKOLS(2) = CLKOLS(SNIRF1)
      MBKOLS(3) = CLKOLS(SNISUB)
      MBKOLS(4) = CLKOLS(SNRWE1)
      MBKOLS(5) = CLKOLS(SNDTIM)
      MBKOLS(6) = CLKOLS(SNRMD1)
      MBKOLS(7) = CLKOLS(SNRDS1)
      MBKOLS(8) = CLKOLS(SNIANT)
      MBKOLS(9) = CLKOLS(SNIRF2)
      MBKOLS(10) = CLKOLS(SNISUB)
      MBKOLS(11) = CLKOLS(SNRWE2)
      MBKOLS(12) = CLKOLS(SNDTIM)
      MBKOLS(13) = CLKOLS(SNRMD2)
      MBKOLS(14) = CLKOLS(SNRDS2)
C                                       Smoothing times
      IF (SMOTIM(1).LT.1.0E-6) SMOTIM(1) = 1.0E-6
      IF (SMOTIM(2).LT.1.0E-6) SMOTIM(2) = 1.0E-6
      IF (SMOTIM(3).LT.1.0E-6) SMOTIM(3) = 1.0E-6
C                                       Re-reference multi-band delays
C                                       first. Routine MBDREF does NOT
C                                       change the reference antenna,
C                                       leaves that to CALREF.
C                                       Loop over subarrays
      DO 60 LOOPSA = LIMS1,LIMS2
C                                       Loop over reference antennas
C                                       used.
         REF = REFTMP
         DO 50 LOOP = 1,MAXANT
            IF ((REFUSS(LOOP,LOOPSA).GT.0) .AND. (LOOP.NE.REF)) THEN
               ANT = LOOP
C                                       First Stokes'
               CALL MBDREF (ANT, REF, LOOPSA, MBKOLS(1), BUFFER,
     *            SMOTIM, NUMROW, WORK(OFFSET+1), WORK(OFFSET+NUMROW+1),
     *            WORK(OFFSET+2*NUMROW+1), WORK(OFFSET+3*NUMROW+1),
     *            IRET)
               IF (IRET.GT.0) GO TO 990
C                                       Second Stokes'
               IF (NUMPOL.GT.1) THEN
                  CALL MBDREF (ANT, REF, LOOPSA, MBKOLS(8), BUFFER,
     *               SMOTIM, NUMROW, WORK(OFFSET+1),
     *               WORK(OFFSET+NUMROW+1), WORK(OFFSET+2*NUMROW+1),
     *               WORK(OFFSET+3*NUMROW+1), IRET)
                  IF (IRET.GT.0) GO TO 990
                  END IF
               END IF
   50       CONTINUE
   60    CONTINUE
C                                       Loop over IFs
      DO 200 IFLOOP = 1,NUMIF
         FREQIF = FREQ + FOFF(IFLOOP)
C                                       Loop over subarrays
         DO 110 LOOPSA = LIMS1,LIMS2
C                                       Loop over reference antennas
C                                       used.
            REF = REFTMP
            DO 100 LOOP = 1,MAXANT
               IF ((REFUSS(LOOP,LOOPSA).GT.0) .AND. (LOOP.NE.REF)) THEN
                  ANT = LOOP
C                                       First Stokes'
                  CALL CALREF (ANT, REF, LOOPSA, KOLS(1), FREQIF,
     *               SMOTIM, NUMROW, BUFFER, WORK(OFFSET+1),
     *               WORK(OFFSET+NUMROW+1), WORK(OFFSET+2*NUMROW+1),
     *               WORK(OFFSET+3*NUMROW+1), WORK(OFFSET+4*NUMROW+1),
     *               IRET)
C                                       Inability to connect ANT to
C                                       REF is not necessarily an
C                                       error
                  IF (IRET.EQ.1) THEN
                     IRET = 0
                     NOTREF = .TRUE.
                     END IF
                  IF (IRET.GT.0) GO TO 990
C                                       Second Stokes'
                  IF (NUMPOL.GT.1) THEN
                     CALL CALREF (ANT, REF, LOOPSA, KOLS(10), FREQIF,
     *                  SMOTIM, NUMROW, BUFFER, WORK(OFFSET+1),
     *                  WORK(OFFSET+NUMROW+1), WORK(OFFSET+2*NUMROW+1),
     *                  WORK(OFFSET+3*NUMROW+1),
     *                  WORK(OFFSET+4*NUMROW+1), IRET)
C                                       Inability to connect ANT to
C                                       REF is not necessarily an
C                                       error
                     IF (IRET.EQ.1) THEN
                        IRET = 0
                        NOTREF = .TRUE.
                        END IF
                     IF (IRET.GT.0) GO TO 990
                     END IF
                  END IF
 100           CONTINUE
 110        CONTINUE
C                                       Update column pointers for IF
         KOLS(2) = KOLS(2) + 1
         KOLS(4) = KOLS(4) + 1
         KOLS(6) = KOLS(6) + 1
         KOLS(7) = KOLS(7) + 1
         KOLS(8) = KOLS(8) + 1
         KOLS(9) = KOLS(9) + 1
         KOLS(11) = KOLS(11) + 1
         KOLS(13) = KOLS(13) + 1
         KOLS(15) = KOLS(15) + 1
         KOLS(16) = KOLS(16) + 1
         KOLS(17) = KOLS(17) + 1
         KOLS(18) = KOLS(18) + 1
 200     CONTINUE
C                                       Warn user if re-referencing was
C                                       partially successful
      IF (NOTREF) THEN
         MSGTXT = 'Some solutions were not adjusted to the common'
         CALL MSGWRT (6)
         MSGTXT = 'reference antenna. This may be because one or more'
         CALL MSGWRT (6)
         MSGTXT = 'antennas were not used. Check your data before'
         CALL MSGWRT (6)
         MSGTXT = 'proceeding.'
         CALL MSGWRT (6)
      END IF
C                                       Close table
 900  CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IRET)
      GO TO 999
C                                       Error
 980  CALL MSGWRT (8)
 990  WRITE (MSGTXT,1990)
      CALL MSGWRT (8)
C
 999  CONTINUE
C                                       De-allocate any dynamic
C                                       workspace. Ignore errors.
      IF (OFFSET .NE. 0) THEN
         CALL ZMEMRY ('FREE', 'CALADJ', BUFSZ, WORK, OFFSET, IERR)
         END IF
C
      RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR ',I5,' FINDING SN TABLE COLUMNS')
 1015 FORMAT ('TABIO ERROR ',I5,' CLOSING SN TABLE')
 1020 FORMAT ('TABSRT ERROR ',I5,' SORTING SN TABLE')
 1990 FORMAT ('ERROR OCCURED IN CALADJ')
 2000 FORMAT ('Adjusting solutions to a common reference antenna =',I3)
      END
