      SUBROUTINE ACTFRQ (IANT, SOURNU, TIME, FRQSEL, IF, ICHAN, DISKIN,
     *   CNOIN, NUMCL, FIRST, LAST, FREQA, IRET)
C-----------------------------------------------------------------------
C! Determines true frequency of any spectral channel at any time.
C# Calibration, Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000, 2012, 2015
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
C   Routine to determine the frequency of any channel in any IF at any
C   time.
C   Inputs:
C      IANT     I       Antenna number of first of baseline pair
C      SOURNU   I       Source number
C      TIME     R       Time of record (days)
C      FRQSEL   I       Freq ID of current record.
C      IF       I       IF number requested
C      ICHAN    I       Channel number requested
C      DISKIN   I       Data disk number
C      CNOIN    I     Data catalogue number
C      NUMCL    I       CL table to use for time varying freq. info
C      LAST     L       If TRUE close down open CL table
C   Input from common
C      CATBLK   I(256)   Catalog header
C  Input/Output
C    FIRST        L       Must be TRUE for first call, routine will then
C                         set to false - useful for internal bookeeping
C  Output:
C    FREQA        D       Actual freq value
C    IRET         I       Error code: 0 => OK
C                                     anything else is bad
C
C  The actual frequency is determined by:
C      Reference freq in catalogue header
C    + offset for the IF
C    + offset for the source
C    + time variable offset for the source/antenna
C    + offset for the channel from the reference
C
C N.B. ACTFRQ assumes that the CL table has been sorted to time
C      increasing order - if not it will fail, with an appropriate
C      error message.
C
C Uses LUN 49 and 50 - requires their use throughout the operation, if
C there is conflict - these can be changed by changing the data
C statement below.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IANT, SOURNU, IF, ICHAN, DISKIN, CNOIN, FRQSEL,
     *   NUMCL, IRET
      LOGICAL FIRST, LAST
      DOUBLE PRECISION FREQA
C
      CHARACTER COLHED(MAXCLC)*24, KEYW(4)*8, VELTYP*8, VELDEF*8,
     *   SOUNAM*16, CALCOD*4, BNDCOD(MAXIF)*8
      INTEGER   I, J,  NKEY, NREC, NCOL, DATP(128,2), IPOINT,
     *   KOLS(MAXCLC), KEYTYP(4), KLOCS(4), KEYVAL(6), NUMANT, NUMPOL,
     *   NUMBBC, BUFF1(512), CVER, ILUN,  NUMIF, ISBAND(MAXIF), INOGRP,
     *   SUFQID, ISURNO, CLLUN, SUKOLS(MAXSUC), SUNUMV(MAXSUC), NSOURC,
     *   IDSOU, QUAL, ICLRNO, NCLINR, CLBUFF(512), CLKOLS(MAXCLC),
     *   CLNUMV(MAXCLC), CURRNO(MAXANT), IFNO, LIMIT, REC2(22*60*MAXIF),
     *   TIMKOL, SOUKOL, ANTKOL, DOPKOL, INTKOL, IFOFF, OLDSOU, KANT,
     *   LASTAN, SVER
      LOGICAL   T, MULTI, RDSU, CLCARE
      REAL      GMMOD, FINC(MAXIF), FLUX(4,MAXIF), CURCLI(MAXANT),
     *   CURLOO(MAXANT,MAXIF), KEYVAR(6), REC4(11+30*MAXIF), TIMLOW,
     *   TIMHI, TIME
      DOUBLE PRECISION REFF, FOFF(MAXIF), FREQO(MAXIF), BANDW, RAEPO,
     *   DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL(MAXIF),
     *   RESTFQ(MAXIF), PMRA, PMDEC, FREQS, CURCLT(MAXANT),
     *   REC8(6+15*MAXIF), KEYVAD
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (KEYVAL, KEYVAR)
      EQUIVALENCE (REC2, REC4, REC8)
      EQUIVALENCE (TIMKOL,CLKOLS(1)),  (INTKOL,CLKOLS(2)),
     *   (SOUKOL,CLKOLS(3)),  (ANTKOL,CLKOLS(4)),
     *   (DOPKOL,CLKOLS(9))
      SAVE FREQS, OLDSOU, CURRNO, CURCLT, CURCLI, CURLOO, CLBUFF,
     *   LASTAN
      DATA COLHED /'TIME', 'TIME INTERVAL', 'SOURCE ID',
     *   'ANTENNA NO.', 'SUBARRAY', 'FREQ ID', 'I.FAR.ROT',
     *   'GEODELAY', 'DOPPOFF', 'ATMOS', 'DATMOS', 'MBDELAY1',
     *   'CLOCK 1', 'DCLOCK 1', 'DISP 1', 'DDISP 1', 'REAL1',
     *   'IMAG1', 'RATE 1', 'DELAY 1', 'WEIGHT 1', 'REFANT 1',
     *   'MBDELAY2', 'CLOCK 2', 'DCLOCK 2', 'DISP 2', 'DDISP 2',
     *   'REAL2', 'IMAG2', 'RATE 2', 'DELAY 2', 'WEIGHT 2',
     *   'REFANT 2' /
      DATA KEYW /'NO_ANT', 'NO_POL', 'NO_IF', 'MGMOD'/
      DATA T /.TRUE./
      DATA ILUN, CLLUN /50, 49/
C-----------------------------------------------------------------------
C                                       Multi source file?
      CALL MULSDB (CATBLK, MULTI)
C                                       Set base ref. freq.
      REFF = CATD(KDCRV+JLOCF)
C                                       Get IF offset info.
      CVER = 1
      CALL CHNDAT ('READ', BUFF1, DISKIN, CNOIN, CVER, CATBLK,
     *   ILUN, NUMIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
      IF (IRET.NE.0) THEN
        WRITE (MSGTXT,1000) IRET
        GO TO 990
        END IF
C                                       add up cpts so far
      FREQA = REFF + FOFF(IF)
C                                       Initialize other cpts
      FREQS = 0.D0
C                                       Peculiar source offset
      IF (MULTI) THEN
         RDSU = FIRST
         IF (OLDSOU.NE.SOURNU) RDSU = .TRUE.
         IF (RDSU) THEN
C                                       Open SU table
            SVER = 1
            CALL SOUINI ('READ', BUFF1, DISKIN, CNOIN, SVER, CATBLK,
     *         ILUN, INOGRP, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS,
     *         SUNUMV, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'SU', SVER
               GO TO 990
               END IF
C                                       Get number of sources.
            NSOURC = BUFF1(5)
C                                       Loop through records
            DO 100 I = 1,NSOURC
               IRET = 1
C                                       Read record
               ISURNO = I
               CALL TABSOU ('READ', BUFF1, ISURNO, SUKOLS, SUNUMV,
     *            IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW,
     *            RAEPO, DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS,
     *            LSRVEL, RESTFQ, PMRA, PMDEC, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1020) IRET, 'SU'
                  GO TO 990
                  END IF
C
               IF (SOURNU.EQ.IDSOU) THEN
                  FREQS = FREQO(IF)
                  OLDSOU = IDSOU
                  END IF
 100           CONTINUE
            IF (IRET.LE.0) THEN
               CALL TABIO ('CLOS', 0, I, BUFF1, BUFF1, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1040) IRET, 'SU'
                  GO TO 990
                  END IF
               END IF
            END IF
         END IF
C                                       add up cpts so far
      FREQA = FREQA + FREQS
C                                       Check the CL sort order
      IF (MULTI) THEN
         IF (FIRST) THEN
C                                       Open Calibration table
            NKEY = 0
            NREC = 0
            NCOL = 0
            ICLRNO = 1
            CALL TABINI ('READ', 'CL', DISKIN, CNOIN, NUMCL, CATBLK,
     *         ILUN, NKEY, NREC, NCOL, DATP, CLBUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET, 'CL', NUMCL
               GO TO 990
               END IF
C                                       Get number of scans
            NCLINR = CLBUFF(5)
C                                       Check if empty
            IF (NCLINR.LE.0) THEN
               WRITE (MSGTXT,1030)
               IRET = 1
               GO TO 990
               END IF
C                                       Get column pointers
            NKEY = MAXCLC
            CALL FNDCOL (NKEY, COLHED, 24, T, CLBUFF, KOLS, IRET)
            IF ((IRET.GE.1) .AND. (IRET.LE.10)) GO TO 999
            IRET = 0
            CALL FILL (NKEY, 0, CLKOLS)
            CALL FILL (NKEY, 0, CLNUMV)
            DO 110 J = 1,NKEY
               IPOINT = KOLS(J)
               IF (IPOINT.NE.0) THEN
                  CLKOLS(J) = DATP(IPOINT,1)
                  CLNUMV(J) = DATP(IPOINT,2) / 10
                  END IF
 110           CONTINUE
C                                       Table keywords
            NKEY = 4
            CALL TABKEY ('READ', KEYW, NKEY, CLBUFF, KLOCS, KEYVAL,
     *         KEYTYP, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Retrieve keyword values
C                                       No. antennas.
            IPOINT = KLOCS(1)
            IF (IPOINT.GT.0) NUMANT = KEYVAL(IPOINT)
C                                       No. IFs per pair.
            IPOINT = KLOCS(2)
            IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       No. IF pairs.
            IPOINT = KLOCS(3)
            IF (IPOINT.GT.0) NUMBBC = KEYVAL(IPOINT)
C                                       Gain modulus
            IPOINT = KLOCS(4)
            IF (IPOINT.GT.0) THEN
               IF (KEYTYP(4).EQ.1) THEN
                  CALL RCOPY (2, KEYVAR(IPOINT), KEYVAD)
                  GMMOD = KEYVAD
               ELSE
                  GMMOD = KEYVAR(IPOINT)
                  END IF
               END IF
C                                       check order
            IF (CLBUFF(43).EQ.KOLS(1)) THEN
               CLCARE = .FALSE.
               IF (CLBUFF(44).EQ.KOLS(4)) CLCARE = .TRUE.
C                                       Close table
               CALL TABIO ('CLOS', 0, ICLRNO, CLBUFF, CLBUFF, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1040) IRET, 'CL'
                  GO TO 990
                  END IF
               GO TO 200
               END IF
C                                       Close table
            CALL TABIO ('CLOS', 0, ICLRNO, CLBUFF, CLBUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1040) IRET, 'CL'
               GO TO 990
               END IF
            MSGTXT = 'ACTFRQ: CL table is in wrong sort order'
            IRET = 1
            GO TO 990
C                                       Open for use
 200        NKEY = 0
            NREC = 0
            NCOL = 0
            ICLRNO = 1
            CALL TABINI ('READ', 'CL', DISKIN, CNOIN, NUMCL, CATBLK,
     *         CLLUN, NKEY, NREC, NCOL, DATP, CLBUFF, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1010) IRET, 'CL', NUMCL
               GO TO 990
               END IF
            WRITE (MSGTXT,1050) NUMCL
            CALL MSGWRT (6)
C                                       Get number of scans
            NCLINR = CLBUFF(5)
            END IF
         END IF
C                                       Is there a time variable
C                                       offset for this source/antenna/
C                                       time?
      IF (MULTI) THEN
C                                       Init offset matrix
         IF (FIRST) THEN
            LASTAN = 0
            DO 220 I = 1, MAXANT
               DO 210 J = 1, MAXIF
                  CURLOO(I,J) = 0.0
 210              CONTINUE
 220           CONTINUE
            END IF
C                                       Do we need to look through
C                                       table
         IF (CURRNO(IANT).GT.0) THEN
            IF (TIME.LE.(CURCLT(IANT) + 0.5*CURCLI(IANT))) THEN
               GO TO 400
               END IF
            END IF
C                                       Read until selected time.
         IF (CURRNO(IANT).EQ.0) THEN
            LIMIT = 1
         ELSE
            LIMIT = CURRNO(IANT)
            END IF
         DO 300 I = LIMIT, NCLINR
            ICLRNO = I
            CALL TABIO ('READ', 0, ICLRNO, REC4, CLBUFF, IRET)
            IF (IRET.LT.0) GO TO 300
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1020) IRET, 'CL'
               GO TO 990
               END IF
C                                       See if correct source
            IF (REC2(SOUKOL).NE.SOURNU) GO TO 300
            KANT = REC2(ANTKOL)
C                                       Check time
            TIMLOW = REC8(TIMKOL) - (REC4(INTKOL)/2.0)
            TIMHI  = REC8(TIMKOL) + (REC4(INTKOL)/2.0)
            IF ( (TIME.LE.TIMHI) .AND. (TIME.GE.TIMLOW) ) THEN
C                                       Loop over IF
               DO 250 IFNO = 1, NUMIF
                  IFOFF = IFNO - 1
                  CURLOO(KANT,IFNO) = REC4(DOPKOL+IFOFF)
 250              CONTINUE
               CURCLT(KANT) = REC8(TIMKOL)
               CURCLI(KANT) = REC4(INTKOL)
               CURRNO(KANT) = ICLRNO
               LASTAN = KANT
               END IF
C                                       If time-ant order then
C                                       we need to read through and
C                                       find the antenna of interest
            IF (CLCARE) THEN
               IF (LASTAN.GE.IANT) GO TO 400
               GO TO 300
               END IF
            IF (REC8(TIMKOL) .GT. TIME) GO TO 400
 300        CONTINUE
         END IF
C                                       add up cpts so far
 400  IF (MULTI) FREQA = FREQA + CURLOO(IANT,IF)
C                                       Determine the channel
C                                       offset.
      FREQA = FREQA + (ICHAN - CATR(KRCRP+JLOCF)) * FINC(IF)
      FIRST = .FALSE.
C                                       If last record, tidy up
      IF (LAST .AND. MULTI) THEN
C                                       Close table
         CALL TABIO ('CLOS', 0, ICLRNO, CLBUFF, CLBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ACTFRQ: ERROR',I3,' GETTING FREQ. INFO. WITH CHNDAT')
 1010 FORMAT ('ACTFRQ: ERROR',I3,' OPENING ',A2,' TABLE, VERSION',I5)
 1020 FORMAT ('ACTFRQ: ERROR',I3,' READING ',A2,' TABLE')
 1030 FORMAT ('ACTFRQ: EMPTY CL TABLE')
 1040 FORMAT ('ACTFRQ: ERROR',I3,' CLOSING ',A2,' TABLE')
 1050 FORMAT ('Using CL table ',I3,' to obtain time dependent freq',
     *   ' offsets')
      END

