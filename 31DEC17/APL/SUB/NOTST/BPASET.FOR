      SUBROUTINE BPASET (IERR)
C-----------------------------------------------------------------------
C! Sets up the bandpass table array for use by DATBND.
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000-2001, 2005-2006, 2009-2012, 2015
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
C   Sets up the bandpass table array for use by DATBND, also sets
C   up the arrays necessary for VLBA bandpass corrections.
C   Output:
C      IERR     I        Return error code 0=>OK, else failed.
C   Inputs from common
C      BPBUFF   I(512)   BP table I/O TABIO buffer
C      NBPINR   I        Number of BP records in file.
C      NANTBP   I        Number of antennas
C      NPOLBP   I        Number of IFs per group (polarizations)
C      NIFBP    I        Number of IFs.
C      NCHNBP   I        Number of channels
C   Output to common
C      PBPBUF   LI       Pointer to start of actual BPBUF
C      BPBUF    R(*)     Dynamic memory to hold BP corrs with initial
C                        set of corrections
C      PVLBUF   LI       Pointer to start of actual VLBUF if VLBA
C      VLBUF    R(*)     Dynamic memory to hold VLBA BP corrs
C      ANTNX    LI(2,*)  Index array contains 1 or 2 pointers for each
C                        antenna, if pointer = 0 antenna not present 1
C                        or 2 times.  Pointer is to time (vis 2 later).
C      MXANUM   I        The maximum antenna number
C      ANTREC   I(2,*)   First and last BP rec # for each antenna
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      CHARACTER KEYS(6)*24, BNDCOD(MAXIF)*8
      INTEGER   IERR, ANTNO, SUBA, NIO, SOURID, BPREF(2), IFRQ, IFNO,
     *   LENBU, LRECS, LRECW, KLOCT, KLOCA, JERR, OANTNO, FREQID,
     *   KOLS(12), KEY(2,2), NKEY, I, J, INDEX, CVER, ICHLUN, ANVER,
     *   LNX, INXW, LNXW, NWD, LRECWT, LRECBP, LRECA, KEYSUB(2,2)
      LONGINT   BIND, VIND, IIND, INXP, LNXP, JNDEX, INX
      LOGICAL   SORT, TABEND, SOMANT, ANTFLG
      REAL      INTERV, FKEY(2,2), WTP1, WTP2, RTMP, BNDPAS(2*MAXCIF),
     *   WEIGHT(2*MAXIF), WTS(MAXCIF)
      DOUBLE PRECISION    TIME, ARRLON
      SAVE BNDPAS, WTS
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DBPC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DCVL.INC'
      INCLUDE 'INCS:DANT.INC'
C      DATA T, F /.TRUE.,.FALSE./
      DATA NKEY /6/
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
      DATA KEYS /'ANTENNA ', 'REFANT 1 ', 'SUBARRAY ', 'TIME ',
     *   'REAL 1 ', 'IMAG 1 '/
C-----------------------------------------------------------------------
C                                       Is this VLBA data?
      ANVER = 1
      CALL ANTINI ('READ', BPBUFF, IUDISK, IUCNO, ANVER, CATUV, IPLUN,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READ', 'AN TABLE'
         GO TO 990
         END IF
      CALL TABIO ('CLOS', 1, IANRNO, BPBUFF, BPBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      ISVLA = (ANAME(1:4).NE.'VLBA') .AND. (ANAME(1:3).NE.'EVN')
      ISVLBA = ANAME(1:4).EQ.'VLBA'
      IF (ISVLBA) THEN
         I = MAXANT * MAXIF
         CALL RFILL (I, 0.0, AVDELI)
         END IF
C                                      Init various things in the
C                                      common
      CALL RFILL (MAXANT, 0.0, CURSHF)
      CALL FILL (4, 0, BPGOT)
      MXANUM = 0
C                                      Check if need all antennas
      SOMANT = .FALSE.
      DO 10 I = 1,MAXANT
         IF (ANTENS(I).NE.0) SOMANT = .TRUE.
 10      CONTINUE
C                                      Sort BP table, reformat
      CALL BPREFM (IUDISK, IUCNO, BPVER, CATUV, IPLUN, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1005) IERR
         CALL MSGWRT (8)
         END IF
      CALL BPINI ('READ', BPBUFF, IUDISK, IUCNO, BPVER, CATUV, IPLUN,
     *   IBPRNO, BPKOLS, BPNUMV, NANTBP, NPOLBP, NIFBP, NCHNBP, BCHNBP,
     *   NUMSHF, LOWSHF, DELSHF, LTYPBP, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN', 'BP TABLE'
         GO TO 990
         END IF
      WPOLY = LTYPBP.NE.' '
C                                       Check sizes
      NWD = NPOLBP * NIFBP * NCHNBP
      IF ((NIFBP.GT.MAXIF) .OR. (NCHNBP.GT.MAXCHA) .OR. (NWD.GT.MAXCIF))
     *   THEN
         IERR = 1
         MSGTXT = 'BPASET: SINGLE VISIBILITY TOO BIG FOR BUFFERS'
         GO TO 990
         END IF
C                                       allocate memory
      IF (PBPBUF.EQ.0) THEN
C                                       interpolation areas
         IF (DOBAND.GT.1) THEN
            NWD = NANTBP * (2 * NPOLBP * NIFBP * NCHNBP + 2)
            NWD = (NWD + NPOLBP * NIFBP * NANTBP) * 2
            NWD = (NWD - 1) / 1024 + 1
            CALL ZMEMRY ('GET ', 'BPASET', NWD, BPIBUF(1), PBPIBF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) IERR, NWD
               GO TO 990
               END IF
            END IF
C                                       applied area(s)
         NWD = NANTBP * (2 * NPOLBP * NIFBP * NCHNBP + 2)
         IF ((WPOLY) .OR. (ISVLBA)) NWD = 2 * NWD
         NWD = (NWD - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', 'BPASET', NWD, BPBUF(1), PBPBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR, NWD
            GO TO 990
            END IF
         NWD = NWD * 1024
         IF ((WPOLY) .OR. (ISVLBA)) THEN
            NWD = NWD / 2
            PVLBUF = PBPBUF + NWD
         ELSE
            PVLBUF = PBPBUF
            END IF
         END IF
C                                       Set column pointers for sort
      CALL FNDCOL (NKEY, KEYS, 24, .TRUE., BPBUFF, KOLS, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'FIND', 'BP COLUMNS'
         GO TO 990
         END IF
C                                       Close
      CALL TABIO ('CLOS', 0, IBPRNO, BPBUFF, BPBUFF, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CLOS', 'BP TABLE'
         GO TO 990
         END IF
C                                       Sort to ant-time order if
C                                       BPAVG is true
C                                       See if sort necessary
      SORT = (BPBUFF(43).NE.KOLS(1)) .OR. (BPBUFF(44).NE.KOLS(4))
C                                       sort it
      IF (SORT) THEN
         KEY(1,1) = KOLS(1)
         KEY(2,1) = KOLS(1)
         KEY(1,2) = KOLS(4)
         KEY(2,2) = KOLS(4)
         CALL TABSRT (IUDISK, IUCNO, 'BP', BPVER, BPVER, KEY, KEYSUB,
     *      FKEY, BPBUFF, CATUV, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'SORT', 'BP TABLE'
            GO TO 990
            END IF
         END IF
C                                       Open BP table for read
      CALL BPINI ('READ', BPBUFF, IUDISK, IUCNO, BPVER, CATUV, IPLUN,
     *   IBPRNO, BPKOLS, BPNUMV, NANTBP, NPOLBP, NIFBP, NCHNBP, BCHNBP,
     *   NUMSHF, LOWSHF, DELSHF, LTYPBP, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN', 'BP TABLE'
         GO TO 990
         END IF
      NBPINR = BPBUFF(5)
C                                       Do we need to initialize the
C                                       frequency shifting stuff?
      IF (ISVLBA) THEN
         USEAP = .FALSE.
C                                       Get frequency info.
         CVER = 1
         ICHLUN = 50
         CALL CHNDAT ('READ', BNDPAS, IUDISK, IUCNO, CVER, CATUV,
     *      ICHLUN, CNNIF, CFOFF, CSBAND, CFINC, BNDCOD, FRQSEL, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ', 'FQ TABLE'
            GO TO 990
            END IF
C                                       Fill AN information
C                                       into common in DANS.INC
         CALL GETANT (IUDISK, IUCNO, SUBARR, CATUV, BNDPAS, IERR)
         IF (IERR .NE. 0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ', 'AN TABLE'
            GO TO 990
            END IF
C                                       Correct station positions for
C                                       centre array offset if non-zero
         MSTNS = 0
C                                       use right-hand system
C                                       (DETRAT has been fixed to RH)
         ARRLON = 0.0D0
         IF ((ABS(CNTRX).GT.1.D2) .AND. (ABS(CNTRY).GT.1.D2) .AND.
     *      (ABS(CNTRZ).GT.1.D2)) ARRLON = ATAN2 (CNTRY, CNTRX)
         DO 30 I = 1,MAXANT
            IF ((TELNO(I).GT.0) .AND. (TELNO(I).LT.MAXANT)) THEN
               MSTNS = MAX (MSTNS, TELNO(I))
               CFIRST(TELNO(I)) = .TRUE.
               ANTX(TELNO(I)) = CNTRX + STNX(TELNO(I))*COS(ARRLON) -
     *            STNY(TELNO(I)) * SIN(ARRLON)
               ANTY(TELNO(I)) = CNTRY + STNY(TELNO(I))*COS(ARRLON) +
     *            STNX(TELNO(I)) * SIN(ARRLON)
               ANTZ(TELNO(I)) = CNTRZ + STNZ(TELNO(I))
               END IF
 30         CONTINUE
         OLDCSU = -1
         END IF
C                                       Create scratch file for
C                                       BP data
      LENBU = 1
C                                       Determine size and define
C                                       random pointers
      LRECW = NPOLBP * NCHNBP * NIFBP
      LRECBP = 2 * LRECW
      LRECWT = 0
      IF (DOBAND.GT.1) LRECWT = NPOLBP * NIFBP
      LRECA = LRECBP + 2
      LRECS = LRECA + LRECWT
      KLOCT = 0
      KLOCA = 1
C                                       Define scratch data increments
      KSNCF = 2
      KSNCIF = NCHNBP * KSNCF
      KSNCS =  NIFBP * KSNCIF
      CALL FILL (2*MAXANT, 0, ANTREC)
      DO 40 I = 1,MAXANT
         ANTNX(1,I) = 0
         ANTNX(2,I) = 0
         ANTNX(3,I) = 0
 40      CONTINUE
      BIND = 1 + PBPBUF
      VIND = 1 + PVLBUF
      IIND = 1 + PBPIBF
C                                       If BPAVG then average table
C                                       into scratch file
      IF (DOBAND.EQ.1) THEN
         OANTNO = -1
         CALL RFILL (NWD, 0.0, BPBUF(BIND))
         CALL RFILL (LRECW, 0.0, WTS)
         NIO = 1
C                                       Average the data
 60      CALL TABBP ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP,
     *      NCHNBP, NPOLBP, TIME, INTERV, SOURID, SUBA, ANTNO, CHNBND,
     *      BPFREQ, FREQID, BPREF, WEIGHT, BNDPAS, JERR)
         ANTFLG = JERR.LT.0
         TABEND = .FALSE.
         IF (JERR.GT.0) THEN
            IERR = JERR
            WRITE (MSGTXT,1000) IERR, 'READ', 'BP TABLE'
            GO TO 990
            END IF
C                                       Check freq id
         IF (((FREQID.NE.FRQSEL) .AND. (FREQID.GT.0) .AND.
     *      (FRQSEL.GT.0)) .OR. ((SUBARR.GT.0) .AND.
     *      (SUBA.NE.SUBARR) .AND. (SUBA.GT.0))) THEN
            IF (IBPRNO.GT.NBPINR) THEN
               TABEND = .TRUE.
               GO TO 150
               END IF
            GO TO 60
            END IF
C                                      Need this antenna ?
         IF (SOMANT) THEN
            DO 80 I = 1,MAXANT
               IF ((ANTNO.EQ.ABS(ANTENS(I))) .AND. (.NOT.DOAWNT))
     *            GO TO 90
               IF ((ANTNO.EQ.ABS(ANTENS(I))) .AND. DOAWNT) GO TO 95
 80            CONTINUE
            IF (.NOT.DOAWNT) GO TO 95
C                                       Decide if at end of table
 90         IF (IBPRNO.GT.NBPINR) THEN
               TABEND = .TRUE.
               GO TO 150
               END IF
            GO TO 60
            END IF
C                                      Write random parms
 95      IF ((ANTNO.NE.OANTNO) .AND. (OANTNO.NE.-1)) GO TO 150
 100     OANTNO = ANTNO
         IF (ANTNO.GT.MXANUM) MXANUM = ANTNO
         BPBUF(BIND+KLOCT) = TIME
         BPBUF(BIND+KLOCA) = ANTNO + 0.01 * MAX (SUBARR-1, 0)
         IF (JERR.NE.-3) THEN
            DO 130 IFNO = 1,NIFBP
               IF (ISVLBA) THEN
                  RTMP = BPFREQ(IFNO)
                  AVDELI(ANTNO,IFNO) = MAX(AVDELI(ANTNO,IFNO),RTMP)
                  END IF
               INXW = (IFNO-1) * NCHNBP
               INX = (IFNO-1) * NCHNBP * 2 - 1
               INXP = INX + BIND + 1
               LNXW = INXW + NCHNBP * NIFBP
               LNX = INX + NCHNBP * NIFBP * 2
               LNXP = LNX + BIND + 1
               WTP1 = MAX (0.0, WEIGHT(IFNO))
               WTP2 = MAX (0.0, WEIGHT(IFNO+NIFBP))
               DO 120 IFRQ = 1,NCHNBP
                  INXW = INXW + 1
                  INX = INX + 2
                  INXP = INXP + 2
                  IF ((JERR.NE.-1) .AND. (BNDPAS(INX).NE.FBLANK) .AND.
     *               (BNDPAS(INX+1).NE.FBLANK)) THEN
                     BPBUF(INXP) = BPBUF(INXP) + BNDPAS(INX) * WTP1
                     BPBUF(INXP+1) = BPBUF(INXP+1) + BNDPAS(INX+1) *
     *                  WTP1
                     WTS(INXW) = WTS(INXW) + WTP1
                     END IF
C                                       Second polarization
                  IF (NPOLBP.GT.1) THEN
                     LNXW = LNXW + 1
                     LNX = LNX + 2
                     LNXP = LNXP + 2
                     IF ((JERR.NE.-2) .AND. (BNDPAS(LNX).NE.FBLANK)
     *                  .AND. (BNDPAS(LNX+1).NE.FBLANK)) THEN
                        BPBUF(LNXP) = BPBUF(LNXP) + BNDPAS(LNX) * WTP2
                        BPBUF(LNXP+1) = BPBUF(LNXP+1) + BNDPAS(LNX+1) *
     *                     WTP2
                        WTS(LNXW) = WTS(LNXW) + WTP2
                        END IF
                     END IF
 120              CONTINUE
 130           CONTINUE
            END IF
C                                       Decide if at end of table
         IF (IBPRNO.LE.NBPINR) GO TO 60
         TABEND = .TRUE.
C                                       Normalize table
 150     INDEX = 0
         JNDEX = BIND
         LNX = NCHNBP * NIFBP
         INX = 2 * NCHNBP * NIFBP + BIND
         DO 190 IFNO = 1,NIFBP
            DO 180 IFRQ = 1,NCHNBP
               INDEX = INDEX + 1
               JNDEX = JNDEX + 2
               IF ((ANTFLG .AND. ((BPBUF(JNDEX).EQ.0.0) .AND.
     *            (BPBUF(JNDEX+1).EQ.0.0))) .OR.
     *            (WTS(INDEX).LT.1E-8)) THEN
                  BPBUF(JNDEX) = FBLANK
                  BPBUF(JNDEX+1) = FBLANK
               ELSE
                  BPBUF(JNDEX) = BPBUF(JNDEX) / WTS(INDEX)
                  BPBUF(JNDEX+1) = BPBUF(JNDEX+1) / WTS(INDEX)
                  END IF
C                                       Second polarization
               IF (NPOLBP.GT.1) THEN
                  LNX = LNX + 1
                  INX = INX + 2
                  IF ((ANTFLG .AND. ((BPBUF(INX).EQ.0.0) .AND.
     *               (BPBUF(INX+1).EQ.0.0))) .OR.
     *               (WTS(LNX).LT.1E-8)) THEN
                     BPBUF(INX) = FBLANK
                     BPBUF(INX+1) = FBLANK
                  ELSE
                     BPBUF(INX) = BPBUF(INX) / WTS(LNX)
                     BPBUF(INX+1) = BPBUF(INX+1) / WTS(LNX)
                     END IF
                  END IF
 180           CONTINUE
 190        CONTINUE
         ANTNX(1,OANTNO) = BIND
C                                       Copy data to large VLB buffer
         IF (ISVLBA) CALL RCOPY (LRECA, BPBUF(BIND), VLBUF(VIND))
         BIND = BIND + LRECA
         VIND = VIND + LRECA
         CALL RFILL (LRECW, 0.0, WTS)
         ANTFLG = .FALSE.
         IF (.NOT.TABEND) GO TO 100
C                                       If interpolating or taking
C                                       the nearest bandpass cal
C                                       make list of rec numbers
C                                       get 1st time
      ELSE
         DO 290 I = 1,NBPINR
            CALL TABBP ('READ', BPBUFF, IBPRNO, BPKOLS, BPNUMV, NIFBP,
     *         NCHNBP, NPOLBP, TIME, INTERV, SOURID, SUBA, ANTNO,
     *         CHNBND, BPFREQ, FREQID, BPREF, WEIGHT, BNDPAS, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ', 'BP TABLE'
               GO TO 990
               END IF
C                                       Check freqid
            IF ((FREQID.NE.FRQSEL) .AND. (FREQID.GT.0) .AND.
     *         (FRQSEL.GT.0)) GO TO 290
            IF ((SUBA.NE.SUBARR) .AND. (SUBA.GT.0) .AND. (SUBARR.GT.0))
     *         GO TO 290
            IF (ANTREC(1,ANTNO).LE.0) ANTREC(1,ANTNO) = I
            ANTREC(2,ANTNO) = I
C                                      Need this antenna ?
            IF (SOMANT) THEN
               DO 210 J = 1,MAXANT
                  IF ((ANTNO.EQ.ABS(ANTENS(I))) .AND. (.NOT.DOAWNT))
     *               GO TO 290
                  IF ((ANTNO.EQ.ABS(ANTENS(J))) .AND. DOAWNT) GO TO 220
 210              CONTINUE
               IF (DOAWNT) GO TO 290
               END IF
C                                      Save first record in core
 220        IF (ANTREC(1,ANTNO).EQ.ANTREC(2,ANTNO)) THEN
               ANTNX(2,ANTNO) = IIND
               ANTNX(1,ANTNO) = BIND
               BPIBUF(IIND+KLOCT) = TIME
               BPIBUF(IIND+KLOCA) = ANTNO + 0.01 * MAX (SUBARR-1, 0)
               CALL RCOPY (LRECBP, BNDPAS, BPIBUF(IIND+2))
               CALL RCOPY (LRECWT, WEIGHT, BPIBUF(IIND+2+LRECBP))
               CALL RCOPY (LRECA, BPIBUF(IIND), BPBUF(BIND))
               IF (ISVLBA) CALL RCOPY (LRECA, BPIBUF(IIND), VLBUF(VIND))
               IIND = IIND + LRECS
               VIND = VIND + LRECA
               BIND = BIND + LRECA
               END IF
C                                       copy second for DOBAND>1
            IF (ANTREC(1,ANTNO).EQ.ANTREC(2,ANTNO)-1) THEN
               ANTNX(3,ANTNO) = IIND
               BPIBUF(IIND+KLOCT) = TIME
               BPIBUF(IIND+KLOCA) = ANTNO + 0.01 * MAX (SUBARR-1, 0)
               CALL RCOPY (LRECBP, BNDPAS, BPIBUF(IIND+2))
               CALL RCOPY (LRECWT, WEIGHT, BPIBUF(IIND+2+LRECBP))
               IIND = IIND + LRECS
               END IF
            IF (ANTNO.GT.MXANUM) MXANUM = ANTNO
            IF (ISVLBA) THEN
               DO 230 IFNO = 1,NIFBP
                  RTMP = BPFREQ(IFNO)
                  AVDELI(ANTNO,IFNO) = MAX(AVDELI(ANTNO,IFNO),RTMP)
 230              CONTINUE
               END IF
 290        CONTINUE
         END IF
      IERR = MAX (0, IERR)
      GO TO 999
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPASET: ERROR',I5,' ',A,'ING ',A)
 1005 FORMAT ('BPASET: ERROR',I4,' UPDATING BP FORMAT, CONTINUING')
 1010 FORMAT ('BPASET: ERROR',I5,' ALLOCATING',I8,' WORDS OF MEMORY')
      END
