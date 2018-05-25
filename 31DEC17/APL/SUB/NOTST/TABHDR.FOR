      SUBROUTINE TABHDR (FDVEC, TBIND, ICARD, HLUN, HBLK, TABTYP,
     *   NUMKEY, KEYWRD, KEYVAL, KEYCHR, KEYTYP, TAPBUF, FITBLK, IERR)
C-----------------------------------------------------------------------
C! Reads a FITS table header.
C# EXT-util FITS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2003, 2005, 2007, 2010, 2016
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
C   TABHDR will read a FITS table header until an END card is found.
C   It will unpack and interpret each card image and put the data
C   for the symbols it recognizes into a set of output arrays.
C   Selected 'SINGLDSH' keywords are kept; the rest are ignored
C   Inputs:
C      FDVEC    I(50)    File descriptor vector for TAPIO input
C      ICARD    I        The card number (1-36) in block to interpret
C      HLUN     I        History file LUN.
C      HBLK     I(256)   History file I/O buffer.
C      TABTYP   I        Table type 0=>FITS ASCII, 1=> FITS 3-D
C   In/Out:
C      TBIND    I        Buffer pointer in FITBLK
C      TAPBUF   I(*)     TAPIO i/o buffer in use
C      FITBLK   C*2880   A block of FITS header data.
C      NUMKEY   I        on Input the max. number of keywords, on
C                        output the number of arbitrary keyword/value
C                        pairs found.
C   Outputs:
C      KEYWRD   C(*)*8   Keywords
C      KEYVAL   D(*)     List of arbitrary numeric/logical values:
C                           KEYVAL(n) => Value in D.
C                           logicals coded as -1.0=>F, 1.0=>T.
C      KEYCHR   C(*)*8   List of character keyword values values.
C      KEYTYP   I(*)     Keyword type codes.
C      IERR     I        error code 0=ok. 1=error.  -1 too many columns
C   Commons:
C      /EXTHDR/    Extension file values.
C      /THDR/      Header values for a tables extension file.
C-----------------------------------------------------------------------
      CHARACTER KEYWRD(*)*8, KEYCHR(*)*8, FITBLK*(*)
      INTEGER   FDVEC(50), TBIND, ICARD, HLUN, HBLK(256), NUMKEY,
     *   TABTYP, KEYTYP(*), TAPBUF(*), IERR
      DOUBLE PRECISION KEYVAL(*)
C
      INTEGER   MXBIN, NN
      PARAMETER (MXBIN=24, NN=35)
      CHARACTER SYMBOL*8, STR*80, TWORD(NN)*8, CARD*80, HILINE*72,
     *   HISTRY*8, COMENT*8, SNGDSH*8, CVER*1, BINTYP(MXBIN)*24,
     *   BINCOD(MXBIN)*2
      DOUBLE PRECISION X
      INTEGER   TLIMIT(NN), N, I, TABNO, NBYT, MAXKEY, NPNT, NCHAR,
     *   IHIERR, KTYPE, COUNT, IST, HERR, JERR, NRCX, IBIN, ISL, JTRIM,
     *   JT, IAX, KBP, NPNTS
      LOGICAL   END, ISHIS, WARN
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DBHD.INC'
      DATA  HISTRY,     COMENT,     SNGDSH
     *    /'HISTORY ', 'COMMENT ', 'SINGLDSH'/
C                                       Special keywords
      DATA TWORD /   'TFIELDS ', 'EXTNAME ', 'EXTVER  ', 'EXTLEVEL',
     *   'TTYPE   ', 'TBCOL   ', 'TFORM   ', 'TUNIT   ', 'TSCAL   ',
     *   'TZERO   ', 'TNULL   ', 'AUTHOR  ', 'REFERENC', 'NMATRIX ',
     *   'DATE-OBS', 'TELESCOP', 'OBSERVER', 'MAXIS   ', 'CTYPE   ',
     *   'CDELT   ', 'CRPIX   ', 'CRVAL   ', 'TMATX   ', 'TDIM    ',
     *   'xCTYP   ', 'xCRVL   ', 'xCDLT   ', 'xCRPX   ', 'xCROT   ',
     *   'BUNIT   ', 'EPOCH   ', 'EQUINOX ', 'OBSRA   ', 'OBSDEC  ',
     *   'OBJECT  '/
      DATA TLIMIT / 4*0, 7*127, 6*0, 6*127, 127, -127, -127, -127, -127,
     *   -127, 6*0/
      DATA BINTYP /'ARRAY_GEOMETRY', 'FREQUENCY', 'SOURCE',
     *   'GAIN_CURVE', 'INTERFEROMETER_MODEL', 'ANTENNA', 'CALIBRATION',
     *   'PHASE-CAL', 'PHASE_CAL', 'WEATHER', 'FLAG', 'BANDPASS',
     *   'BASELINE',  'CALC', 'VLBA_SAMPLER', 'VLBA_EPHEMERIS',
     *   'UV_DATA', 'TAPE_STATISTICS','MODEL_COMPS', 'SPACECRAFT_ORBIT',
     *   'SYSTEM_TEMPERATURE', 'GATEMODL', 'GATEDUTY', 'BANDPASS TABLE'/
      DATA BINCOD /'AG', 'FR', 'SO',
     *    'GN', 'IM', 'AT', 'CA',
     *    'PH', 'PH', 'WR', 'FL', 'BT',
     *    'BC', 'CT', 'VS', 'VE',
     *    'UT', 'VT', 'MC', 'OB',
     *    'TS', 'GM', 'GD', 'BT' /
C-----------------------------------------------------------------------
      MAXKEY = NUMKEY
      NUMKEY = 0
      NRCX = 1
      WARN = .TRUE.
      IHIERR = 0
      HERR = 0
C                                       Loop for all keywords
      DO 800 I = 1,32000
         ICARD = ICARD + 1
C                                       Read a card.
         CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL GTWCRD (ICARD, NN, TLIMIT, TWORD, FITBLK, NPNT, CARD,
     *      SYMBOL, TABNO, N, IAX, CVER, END, JERR)
         IF (END) GO TO 990
C                                       Unrecognized Keyword =
         IF (JERR.EQ.1) THEN
C                                       Check keyword limit
            IF (NUMKEY.LT.MAXKEY) THEN
               CALL GETKEY (SYMBOL, CARD, NPNT, NUMKEY, KEYWRD,
     *            KEYVAL, KEYCHR, KEYTYP, JERR)
C                                       Invalid code
               IF ((KEYTYP(NUMKEY).LE.0) .OR. (KEYTYP(NUMKEY).GT.6))
     *            THEN
                  MSGTXT = 'TABHDR: KEYWORD ' // SYMBOL //
     *               ' OF INDETERMINATE DATA TYPE'
                  CALL MSGWRT (6)
                  NUMKEY = NUMKEY - 1
                  END IF
C                                       Blew keyword limit
            ELSE
               IF (WARN) THEN
                  WRITE (MSGTXT,1000) MAXKEY
                  CALL MSGWRT (6)
                  WARN = .FALSE.
                  END IF
               END IF
C                                       Not normal KEYWORD=value card.
         ELSE IF ((JERR.NE.0) .OR. (CVER.NE.' ')) THEN
C                                       Process selected SINGLDSH
C                                       keywords.
            IF (CARD(1:8).EQ.SNGDSH) THEN
               CALL SDTCRD (ICARD, FITBLK, MAXKEY, NUMKEY, KEYWRD,
     *            KEYVAL, KEYCHR, KEYTYP, WARN, NRCX)
C                                       Table type 'SD'
               ITYPE = 'SD'
C                                       No "Keyword =" put card in
C                                       History.
            ELSE
               JT = JTRIM (CARD)
               IF ((HERR.EQ.0) .AND. (CARD.NE.' ')) THEN
                  ISHIS = (CARD(:8).EQ.HISTRY) .OR. (CARD(:8).EQ.COMENT)
     *               .OR. (CARD(1:8).EQ.' ')
                  JT = JTRIM (SYMBOL)
                  IF ((SYMBOL.NE.' ') .AND. (.NOT.ISHIS)) THEN
                     MSGTXT = 'TABHDR: UNKNOWN SYMBOL ' // SYMBOL
                     CALL MSGWRT (6)
                     END IF
                  IHIERR = IHIERR + 1
                  IF (IHIERR.LE.0) THEN
                     HILINE = TSKNAM //
     *                  '/ UNKNOWN CARDS IN TABLE HEADER'
                     CALL HIADD (HLUN, HILINE, HBLK, HERR)
                     END IF
                  IST = 1
                  IF (ISHIS) IST = 9
                  CALL HIAD80 (HLUN, IST, CARD, HBLK, HERR)
                  END IF
               END IF
C                                       Decode special table keyword.
C                                       Number of fields.
         ELSE IF (TABNO.EQ.1) THEN
            CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 790
            ITNCOL = X + .5
            IF (ITNCOL.GT.127) THEN
               WRITE (MSGTXT,1010) ITNCOL
               CALL MSGWRT (7)
               MSGTXT = 'WILL KEEP ONLY THE FIRST 127 COLUMNS'
               CALL MSGWRT (7)
               ITNCOL = 127
               END IF
C                                       Extension name.
         ELSE IF (TABNO.EQ.2) THEN
            NBYT = 24
            CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
            EXTNAM = STR(1:NCHAR)
C                                       If standard AIPS table type
C                                       then EXTNAM is of the form
C                                       'AIPS XX' where XX is the
C                                       table type.
            IF ((EXTNAM(1:5).NE.'AIPS ').AND.(EXTNAM(1:4).NE.'VLA '))
     *         THEN
               ITYPE = 'UK'
            ELSE
               ITYPE = STR(6:7)
               END IF
C                                       Check if VLBA interchange
C                                       binary tables.
            IF (ITYPE.EQ.'UK') THEN
               DO 20 IBIN = 1,MXBIN
                  IF (EXTNAM(1:16).EQ.BINTYP(IBIN)(1:16)) THEN
                     ITYPE = BINCOD(IBIN)
                     END IF
 20               CONTINUE
               END IF
C                                       Extension version.
         ELSE IF (TABNO.EQ.3) THEN
            CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 790
            EXTVER = X + .01
C                                       Extension level.
         ELSE IF (TABNO.EQ.4) THEN
            CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 790
            EXTLEV = X + .01
C                                       Field type.
         ELSE IF (TABNO.EQ.5) THEN
            NBYT = 24
            CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
            TTYPE(N) = STR(1:NCHAR)
C                                       Starting column.
         ELSE IF (TABNO.EQ.6) THEN
            CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 790
            TBCOL(N) = X + .5
C                                       Field format.
         ELSE IF (TABNO.EQ.7) THEN
            NBYT = 8
            CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
            TFORM(N) = STR(1:NCHAR)
C                                       Decode format.
C                                       ASCII table
            IF (TABTYP.EQ.0) THEN
               CALL TABFRM (TFORM(N), TWIDTH(N), TFRAC(N), KTYPE, JERR)
               TFCODE(N) = KTYPE + 10
               IF (KTYPE.EQ.3) TFCODE(N) = 3 + 10 * TWIDTH(N)
C                                       3-D table
            ELSE IF (TABTYP.EQ.1) THEN
               CALL TABF3D (TFORM(N), COUNT, KTYPE, JERR)
               TFCODE(N) = KTYPE + 10 * COUNT
               END IF
C                                       Field units.
         ELSE IF (TABNO.EQ.8) THEN
            NBYT = 8
            CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
            TUNIT(N) = STR(1:NCHAR)
C                                       Field scale.
         ELSE IF (TABNO.EQ.9) THEN
            CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 790
            TSCAL(N) = X
C                                       Field offset.
         ELSE IF (TABNO.EQ.10) THEN
            CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 790
            TZERO(N) = X
C                                       Field null value.
         ELSE IF (TABNO.EQ.11) THEN
            NBYT = 8
            CALL GETSTR (CARD, 24, NBYT, NPNT, STR, NCHAR)
            TNULL(N) = STR(1:NCHAR)
C                                       AUTHOR or REFERENC (Ignore)
         ELSE IF ((TABNO.EQ.12) .OR. (TABNO.EQ.13)) THEN
C                                       # of matrices (VLBA)
         ELSE IF (TABNO.EQ.14) THEN
            CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 790
            NMATRX = X + .01
C                                       Date of observation (VLBA)
         ELSE IF (TABNO.EQ.15) THEN
            NBYT = 24
            CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
            CALL DATFST ('F2L', STR)
            NCHAR = JTRIM (STR)
            DATOBS = STR(:NCHAR)
C                                       Telescope (VLBA)
         ELSE IF (TABNO.EQ.16) THEN
            NBYT = 24
            CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
            TELESC = STR(1:NCHAR)
C                                       Observer (VLBA)
         ELSE IF (TABNO.EQ.17) THEN
            NBYT = 24
            CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
            OBSRVR = STR(1:NCHAR)
C                                       # axes in data matrix (VLBA)
         ELSE IF (TABNO.EQ.18) THEN
            CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 790
            IF (N.EQ.0) THEN
               MAXIS = X + .01
            ELSE
               MAXISI(N) = X + .01
               END IF
C                                       Axis type. (VLBA)
         ELSE IF (TABNO.EQ.19) THEN
            NBYT = 24
            CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
            CTYPE(N) = STR(1:NCHAR)
C                                       Axis increment (VLBA)
         ELSE IF (TABNO.EQ.20) THEN
            CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 790
            CDELT(N) = X
C                                       Axis ref. pixel (VLBA)
         ELSE IF (TABNO.EQ.21) THEN
            CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 790
            CRPIX(N) = X
C                                       Value at ref pixel (VLBA)
         ELSE IF (TABNO.EQ.22) THEN
            CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 790
            CRVAL(N) = X
C                                       Column which is a data matrix
         ELSE IF (TABNO.EQ.23) THEN
            CALL GETLG (CARD, 80, NPNT, ISL)
            IF (ISL.EQ.0) TMATX(N) = .FALSE.
            IF (ISL.EQ.1) TMATX(N) = .TRUE.
C                                       TDIMnn card - get axes
         ELSE IF (TABNO.EQ.24) THEN
            IF (TTYPE(N)(:8).NE.'VISIBILI') THEN
               WRITE (MSGTXT,1240) N, TTYPE(N)
               CALL MSGWRT (6)
            ELSE
               TMATX(N) = .TRUE.
               NBYT = 80
               CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
C                                       parse out dimensions
               N = 0
               KBP = 2
 40            IF (STR(KBP:KBP).NE.')') THEN
                  N = N + 1
                  CALL GETINT (STR, NCHAR, KBP, MAXISI(N))
                  IF (MAXISI(N).GT.0) GO TO 40
                  N = N - 1
                  END IF
               MAXIS = N
               END IF
C                                       coord type
         ELSE IF (TABNO.EQ.25) THEN
            IF ((.NOT.TMATX(N)) .OR. (IAX.GT.MAXIS)) GO TO 780
            NBYT = 24
            CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
            CTYPE(IAX) = STR(1:NCHAR)
C                                       coord ref value
         ELSE IF (TABNO.EQ.26) THEN
            IF ((.NOT.TMATX(N)) .OR. (IAX.GT.MAXIS)) GO TO 780
            CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 790
            CRVAL(IAX) = X
C                                       coord increment
         ELSE IF (TABNO.EQ.27) THEN
            IF ((.NOT.TMATX(N)) .OR. (IAX.GT.MAXIS)) GO TO 780
            CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 790
            CDELT(IAX) = X
C                                       coord ref pix
         ELSE IF (TABNO.EQ.28) THEN
            IF ((.NOT.TMATX(N)) .OR. (IAX.GT.MAXIS)) GO TO 780
            CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 790
            CRPIX(IAX) = X
C                                       coord rotation
         ELSE IF (TABNO.EQ.29) THEN
            IF ((.NOT.TMATX(N)) .OR. (IAX.GT.MAXIS)) GO TO 780
            CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 790
            CROTA(IAX) = X
C                                       BUNIT
         ELSE IF (TABNO.EQ.30) THEN
            NBYT = 24
            CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
            BUNIT = STR(1:NCHAR)
C                                       EPOCH/EQUINOX
         ELSE IF ((TABNO.EQ.31) .OR. (TABNO.EQ.32)) THEN
            NPNTS = NPNT
            CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.NE.DBLANK) THEN
               CEPOCH = X + 0.01
C                                       try character string
            ELSE
               NPNT = NPNTS
               NBYT = 24
               CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
               IF (INDEX(STR,'1950').GT.0) THEN
                  CEPOCH = 1950.0
               ELSE IF (INDEX(STR,'2000').GT.0) THEN
                  CEPOCH = 2000.0
               ELSE
                  GO TO 790
                  END IF
               END IF
C                                       observed RA
         ELSE IF (TABNO.EQ.33) THEN
            CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 790
            OBSRA = X
C                                       observed Declination
         ELSE IF (TABNO.EQ.34) THEN
            CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 790
            OBSDEC = X
C                                       OBJECT
         ELSE IF (TABNO.EQ.35) THEN
            NBYT = 24
            CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
            OBJECT = STR(1:NCHAR)
            END IF
         GO TO 800
C                                       axis coords not right
 780     IF (.NOT.TMATX(N)) THEN
            WRITE (MSGTXT,1780) N, SYMBOL
         ELSE
            WRITE (MSGTXT,1781) IAX, MAXIS
            END IF
         CALL MSGWRT (7)
         GO TO 800
C                                       BAD VALUE
 790     MSGTXT = 'TABHDR: NUMBER OUT OF RANGE ON ' // SYMBOL
         CALL MSGWRT (6)
 800     CONTINUE
      GO TO 990
C                                       Set common number of keywords
 990  ITANKY = NUMKEY
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TABHDR: BLEW NO. OF KEYWORD LIMIT OF ',I5,' IGNORE REST')
 1010 FORMAT ('TABHDR:',I5,' COLUMNS EXCEEDS AIPS LIMIT OF 127')
 1240 FORMAT ('TABHDR: column',I3,' type ',A8,' TDIM ignored')
 1780 FORMAT ('TABHDR: COLUMN',I3,' NOT DIMENSIONS FOR COORD ',A)
 1781 FORMAT ('TABHDR: COORDINATE AXIS',I4,' LARGER THAN MAX',I3)
      END
