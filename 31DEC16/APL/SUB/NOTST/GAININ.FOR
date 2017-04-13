      SUBROUTINE GAININ (IERR)
C-----------------------------------------------------------------------
C! Initializes calibration table for application.
C# EXT-appl Calibration UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2011-2013, 2015
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
C   Initializes Cal file, and prepares gain table to be applied.
C   If there is no CL file DOCAL is set to .FALSE.
C   For single source data files an SN table will be used rather than a
C   CL table.
C   Opens gain (CL or SN) and baseline (BL) tables if necessary.
C   Inputs from common /SELCAL/
C      CLUSE    I       Cal file version number (CL or SN) to init
C   Output:
C      IERR     I       Return code, 0=>OK, otherwise CL table
C                        exists but cannot be read.
C   Output to common /SELCAL/:
C      RATFAC   R(*)    IF scaling factor to convert s/s to rad/day
C      DELFAC   R(*)    IF scaling factor to convert s to rad/channel
C      LAMBDA   R(*)    Table of wavelengths (in meters) for each
C                        channel and IF ((IF-1)*NLAMDA+CH)
C      ICLRNO   I       Current cal record number
C      NCLINR   I       Number of gain records in file.
C      NBLINR   I       Number of BL records in file.
C      NUMANT   I       Number of antennas
C      NUMPOL   I       Number of polarizations
C      NUMIF    I       Number of IFs.
C      GMMOD    R       Mean gain modulus
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'INCS:DSEL.INC'
      CHARACTER TABTYP*2, KEYW(4)*8, COLHED(CTIRF2)*24
      INTEGER   JERR, J, MSGSAV, NKEY, NREC, NCOL, DATP(128,2), VER, I,
     *   IPOINT, KOLS(CTIRF2), KEYTYP(4), KLOCS(4), KEYVAL(6), KEY(2,2),
     *   BLTIMK, MUMPOL, NEED, TABSIZ, ISBAND(MAXIF), IOFF, KEYSUB(2,2)
      LOGICAL   T, F, SINGLE, EXIST, FITASC, TABLE
      REAL      CATUR(256), FKEY(2,2), KEYVR(6), FINC(MAXIF)
      DOUBLE PRECISION   FREQIF(MAXIF), CATUD(128), KEYVAD
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATUD, CATUR, CATUV)
      EQUIVALENCE (FREQIF, UBUFF)
      EQUIVALENCE (KEYVAL, KEYVR)
C                                       Note this list must be
C                                       coordinated with parameters in
C                                       DSEL.INC
      DATA COLHED /'TIME                    ',
     *   'TIME INTERVAL           ',
     *   'SOURCE ID               ', 'ANTENNA NO.             ',
     *   'SUBARRAY                ', 'FREQ ID                 ',
     *   'I.FAR.ROT               ',
     *   'DISP 1                  ', 'DDISP 1                 ',
     *   'REAL1                   ', 'IMAG1                   ',
     *   'DELAY 1                 ', 'RATE 1                  ',
     *   'WEIGHT 1                ', 'REFANT 1                ',
     *   'REAL2                   ', 'IMAG2                   ',
     *   'DELAY 2                 ', 'RATE 2                  ',
     *   'WEIGHT 2                ', 'REFANT 2                '/
      DATA KEYW /'NO_ANT  ', 'NO_POL  ', 'NO_IF   ', 'MGMOD   '/
      DATA T, F /.TRUE.,.FALSE./
      DATA TABSIZ /XBTBSZ/
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
C                                       Initialize cal table pointers
      ICALP1 = -1
      ICALP2 = -1
      IBLP1 = -1
      IBLP2 = -1
C                                       Initialize flag counts (in DSEL)
      CALL FILL (6, 0, CNTREC)
C                                       Initialize time for cal. table.
      LCALTM = -1.0E20
      CALTIM(1) = -1.0E20
      CALTIM(2) = -1.0E20
      CALTIM(3) = -1.0E20
      BLTIM(1) = -1.0E20
      BLTIM(2) = -1.0E20
      BLTIM(3) = -1.0E20
      I = 2 * MAXANT
      CALL RFILL (I, -1.0E20, TIMECL)
      NEXTCL = 1
C                                       Set maximum size of
C                                       CURCAL and CALTAB
C                                       (number of gains; 5 words ea.)
      LCUCAL = 5
      LCLTAB = 5
C                                       Init polarization offset Indexs
C                                       Is this right?
      CALL FILL (8, 0, POLOFF)
C                                       Input in I, Q, U, V =>1 gain
C                                       If stokes are RR, LL, RL, LR
      IF (ICOR0.LE.0) THEN
C                                       create I, Q, U, V from Circular
         POLOFF(2,1) = LCUCAL
         POLOFF(4,1) = LCUCAL
         POLOFF(2,2) = LCUCAL
         POLOFF(3,2) = LCUCAL
         END IF
C                                       Get frequency scaling factors
      VER = 1
      CALL CHNDAT ('READ', CLBUFF, IUDISK, IUCNO, VER, CATUV, ICLUN,
     *   NUMIF, FREQIF, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000)
         GO TO 990
         END IF
      DO 20 I = 1,NUMIF
         FREQIF(I) = CATUD(KDCRV+JLOCF) + FREQIF(I)
         RATFAC(I) = TWOPI * FREQIF(I) * 86400.0
         DELFAC(I) = TWOPI * FINC(I)
 20      CONTINUE
C                                       Set VLBA delay decorrelation
C                                       parameters
      CALL DSMEAR (IUDISK, IUCNO, CATUV, ANBUFF, IANLUN, FRQSEL, FINC,
     *   LTPVBA, NXDSM, DBTVBA, NFTVBA, ICQVBA, ITFVBA, TVGVBA, DODSM,
     *   WRNVBA, MXDSVB, MAXSB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set bandwidth
      MAXCLR = XCTBSZ
      MAXCLR = MAXCLR / LCUCAL
C                                       Tabulate wavelength
      NLAMDA = ECHAN
      IF (NLAMDA*EIF.GT.MAXCIF) THEN
         WRITE (MSGTXT,1020) ECHAN, EIF, MAXCIF
         IERR = 8
         GO TO 990
         END IF
      DO 40 I = BIF,EIF
         IOFF = (I - 1) * NLAMDA
         DO 30 J = BCHAN,ECHAN
            LAMBDA(IOFF+J) = VELITE / (FREQIF(I) + FINC(I) *
     *         (J-CATUR(KRCRP+JLOCF)))
 30         CONTINUE
 40      CONTINUE

C                                       See if single or multi source
      CALL MULSDB (CATUV, SINGLE)
      SINGLE = .NOT.SINGLE
      IF (.NOT.SINGLE) THEN
         CALL ISTAB ('SU', IUDISK, IUCNO, 1, ICLUN, CLBUFF, TABLE,
     *      EXIST, FITASC, JERR)
         SINGLE = (JERR.NE.0) .OR. (.NOT.(EXIST.AND.TABLE))
         END IF
C                                       Baseline (BL) table.
C                                       Note: this is here because of a
C                                       conflict LUNs in sorting with
C                                       ICLUN.
      DOBL = BLVER.GE.0
      IF (DOBL) THEN
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL BLREFM (IUDISK, IUCNO, BLVER, CATUV, IBLUN, IERR)
         IF (IERR.NE.0) THEN
            MSGSUP = MSGSAV
            WRITE (MSGTXT,1220) IERR
            CALL MSGWRT (8)
            END IF
         CALL BLINI ('READ', BLBUFF, IUDISK, IUCNO, BLVER, CATUV, IBLUN,
     *      IBLRNO, BLKOLS, BLNUMV, NUMANT, NUMPOL, NUMIF, JERR)
         MSGSUP = 0
         IF (JERR.NE.0) DOBL = F
         IF (JERR.NE.0) BLVER = -1
         IF (JERR.EQ.0) NBLINR = BLBUFF(5)
         END IF
      IF (.NOT.(DOCAL.OR.DOBL)) GO TO 999
C                                       Make sure tables big enough.
      NEED = (NUMANT * (NUMANT-1)) / 2
C                                       Only RR and LL polarizations
      MUMPOL = NUMPOL
      IF (MUMPOL.GT.2) MUMPOL = 2
      NEED = (NEED * MUMPOL * NUMIF) * 2
C                                       Doing BL table
      IF (DOBL) THEN
C                                       Baseline table too small
         IF (TABSIZ.LT.NEED) THEN
            IERR = 9
            WRITE (MSGTXT,1080)
            CALL MSGWRT (8)
            WRITE (MSGTXT,1081) NEED
            GO TO 990
            END IF
C                                       Get time column pointer
         NKEY = 1
         CALL FNDCOL (NKEY, COLHED, 24, T, BLBUFF, BLTIMK, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.10)) GO TO 999
         IERR = 0
C                                       Sort to time order if necessary
         IF (BLBUFF(43).NE.BLTIMK) THEN
C                                       Close table
            CALL TABIO ('CLOS', 0, IBLRNO, UBUFF, BLBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            KEY(1,1) = BLTIMK
            KEY(2,1) = 0
            KEY(1,2) = BLTIMK
            KEY(2,2) = 0
C                                       Sort
            CALL TABSRT (IUDISK, IUCNO, 'BL', BLVER, BLVER, KEY, KEYSUB,
     *         FKEY, BLBUFF, CATUV, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Reopen table
            CALL BLINI ('READ', BLBUFF, IUDISK, IUCNO, BLVER, CATUV,
     *         IBLUN,IBLRNO, BLKOLS, BLNUMV, NUMANT, NUMPOL, NUMIF,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         END IF
C                                       Open Calibration table
 100  IF (DOCAL) THEN
         TABTYP = 'CL'
         IF (SINGLE) TABTYP = 'SN'
C                                       Check table format
         MSGSAV = MSGSUP
         MSGSUP = 32000
         IF (SINGLE) THEN
            CALL SNREFM (IUDISK, IUCNO, CLUSE, CATUV, ICLUN, IERR)
            IF (IERR.NE.0) THEN
               MSGSUP = MSGSAV
               WRITE (MSGTXT,1200) IERR
               GO TO 990
               END IF
         ELSE
            CALL CLREFM (IUDISK, IUCNO, CLUSE, CATUV, ICLUN, IERR)
            IF (IERR.NE.0) THEN
               MSGSUP = MSGSAV
               WRITE (MSGTXT,1210) IERR
               GO TO 990
               END IF
            END IF
         MSGSUP = MSGSAV
         NKEY = 0
         NREC = 0
         NCOL = 0
         ICLRNO = 1
         CALL TABINI ('READ', TABTYP, IUDISK, IUCNO, CLUSE, CATUV,
     *      ICLUN, NKEY, NREC, NCOL, DATP, CLBUFF, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1100) IERR, TABTYP, CLUSE
            IF (SINGLE) THEN
               CALL MSGWRT (8)
               MSGTXT = 'CHECK DOCALIB, SHOULD BE FALSE IF NO SN TABLE'
               END IF
            GO TO 990
            END IF
C                                       Get number of scans
         NCLINR = CLBUFF(5)
C                                       If NOT empty
         IF (NCLINR.GT.0) THEN
C                                       Get column pointers
            NKEY = CTIRF2
            CALL FNDCOL (NKEY, COLHED, 24, T, CLBUFF, KOLS, IERR)
            IF ((IERR.GE.1) .AND. (IERR.LE.10)) GO TO 999
            IERR = 0
            CALL FILL (NKEY, 0, CLKOLS)
            CALL FILL (NKEY, 0, CLNUMV)
C                                       Decode Column type and location
            DO 160 J = 1,NKEY
               IPOINT = KOLS(J)
C                                       If Column is in the table
               IF (IPOINT.GT.0) THEN
                  CLKOLS(J) = DATP(IPOINT,1)
                  CLNUMV(J) = DATP(IPOINT,2) / 10
C                                       Else, column not in table
               ELSE
C                                       Set indexs to null values
                  CLKOLS(J) = -1
                  CLNUMV(J) =  0
                  END IF
 160           CONTINUE
C                                       Table keywords
            NKEY = 4
            CALL TABKEY ('READ', KEYW, NKEY, CLBUFF, KLOCS, KEYVAL,
     *         KEYTYP, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Retrieve keyword values
C                                       No. antennas.
            IPOINT = KLOCS(1)
            IF (IPOINT.GT.0) NUMANT = KEYVAL(IPOINT)
C                                       No. IFs per pair.
            IPOINT = KLOCS(2)
            IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       No. IF pairs.
            IPOINT = KLOCS(3)
            IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
C                                       Gain modulus
            IPOINT = KLOCS(4)
            IF (IPOINT.GT.0) THEN
               IF (KEYTYP(4).EQ.1) THEN
                  CALL RCOPY (NWDPDP, KEYVR(IPOINT), KEYVAD)
                  GMMOD = KEYVAD
               ELSE
                  GMMOD = KEYVR(IPOINT)
                  END IF
               END IF
C                                       Sort to time-ant if necessary
            IF ((CLBUFF(43).NE.KOLS(1)) .OR. (CLBUFF(44).NE.KOLS(4)))
     *         THEN
C                                       Close table
               CALL TABIO ('CLOS', 0, ICLRNO, UBUFF, CLBUFF, IERR)
               IF (IERR.NE.0) GO TO 999
               KEY(1,1) = KOLS(1)
               KEY(2,1) = 0
               KEY(1,2) = KOLS(4)
               KEY(2,2) = 0
C                                       Sort
               CALL TABSRT (IUDISK, IUCNO, TABTYP, CLUSE, CLUSE, KEY,
     *            KEYSUB, FKEY, CLBUFF, CATUV, IERR)
               IF (IERR.NE.0) GO TO 999
C                                       Reopen table
               CALL TABINI ('READ', TABTYP, IUDISK, IUCNO, CLUSE, CATUV,
     *            ICLUN, NKEY, NREC, NCOL, DATP, CLBUFF, IERR)
               IF (IERR.GT.0) THEN
                  WRITE (MSGTXT,1100) IERR, TABTYP, CLUSE
                  GO TO 990
                  END IF
               END IF
C                                       set up CLNX table
            CALL GETNCL (IUDISK, IUCNO, CATUV, SUBARR, TABTYP, CLBUFF,
     *          UBUFF, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1160) IERR
               GO TO 990
               END IF
C                                       No Cal file
         ELSE
            CALL TABIO ('CLOS', 0, ICLRNO, UBUFF, CLBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            DOCAL = F
            MSGTXT = TABTYP // ' TABLE IS EMPTY: SETTING DOCAL FALSE'
            CALL MSGWRT (6)
            END IF
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GAININ: CAN NOT READ CH/FQ TABLE')
 1020 FORMAT ('GAININ: ECHAN',I6,' * EIF',I5,' > MAXCIF',I7)
 1080 FORMAT ('INTERNAL TABLES TOO SMALL FOR BASELINE CORRECTION:')
 1081 FORMAT ('NEED XBTBSZ AT LEAST ',I8)
 1100 FORMAT ('GAININ: ERROR',I3,' OPENING ',A2,' TABLE, VERSION',I5)
 1160 FORMAT ('GAININ: ERROR',I3,' MAKING INDEX TO CL TABLE')
 1200 FORMAT ('GAININ: ERROR',I3,' REFORMATTING SN TABLE')
 1210 FORMAT ('GAININ: ERROR',I3,' REFORMATTING CL TABLE')
 1220 FORMAT ('GAININ: ERROR',I3,' REFORMATTING BL TABLE, CONTINUING')
      END
