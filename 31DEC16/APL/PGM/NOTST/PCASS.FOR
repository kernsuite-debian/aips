LOCAL INCLUDE 'PCDATA.INC'
      INCLUDE 'INCS:PPCV.INC'
      DOUBLE PRECISION TIME, CABCAL, PCFREQ(2,MAXTON,MAXIF)
      INTEGER   PCNPOL, PCNIF, NUMTON, PCBUFF(512), PCNUMV(MAXPCC),
     *   PCKOLS(MAXPCC), PCRNO, SOUNUM, ANTNUM, ISUB, IDFREQ, NPCINR
      REAL TIMINT, STATE(2,4,MAXIF), PCREAL(2,MAXTON,MAXIF),
     *   PCIMAG(2,MAXTON,MAXIF), PCRATE(2,MAXTON,MAXIF)
      COMMON /PCDATA/ PCFREQ, TIME, CABCAL, STATE, PCREAL, PCIMAG,
     *   PCRATE, PCBUFF, PCKOLS, PCNUMV, PCNPOL, PCNIF, NUMTON, PCRNO,
     *   TIMINT, SOUNUM, ANTNUM, ISUB, IDFREQ, NPCINR
LOCAL END
LOCAL INCLUDE 'PCSCAN.INC'
      INTEGER   SSCAN(3,200), NOSCAN
      REAL      TSCAN(2,200)
      COMMON /PCSCAN/ TSCAN, SSCAN, NOSCAN
LOCAL END
LOCAL INCLUDE 'PCBPASS.INC'
      INCLUDE 'INCS:DCHND.INC'
      INTEGER   BPBUFF(512), IBPRNO, BPKOLS(MAXBPC), BPNUMV(MAXBPC),
     *   BPNIF, BPNCHN, BPNPOL, BPFQID, BPREFA, BPNUMA, BPSUBA
      REAL      BANDW, WEIGHT(2*MAXIF)
      DOUBLE PRECISION CHSHFT(MAXIF)
      COMMON /PCBPAS/ CHSHFT, BPBUFF, WEIGHT, BANDW, IBPRNO, BPKOLS,
     *   BPNUMV, BPNIF, BPNCHN, BPNPOL, BPFQID, BPREFA, BPNUMA, BPSUBA
LOCAL END
LOCAL INCLUDE 'PCASS.INC'
      INCLUDE 'INCS:PUVD.INC'
      REAL      XSEQ, XDISK, XVER, XFQ, XSUB, BPARM(10)
      HOLLERITH XINNAM(3), XINCLS(2), CALSOR(4,30)
      CHARACTER INNAM*12, INCLS*6
      COMMON /INPARM/ XINNAM, XINCLS, XSEQ, XDISK, XVER, XFQ, XSUB,
     *   CALSOR, BPARM
      COMMON /CHPARM/ INNAM, INCLS
C
      INTEGER   INSEQ, INDISK, INVERS, CNO, OUVER, SCRTCH(512)
      COMMON /PCASSC/ SCRTCH, INSEQ, INDISK, INVERS, CNO, OUVER
LOCAL END
      PROGRAM PCASS
C-----------------------------------------------------------------------
C! Finds bandpass table spectra from PC table spectra
C# EXT-appl Calibration Table
C-----------------------------------------------------------------------
C;  Copyright (C) 2016
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
C   Smooths/interpolates bandpass table to regular intervals
C   Inputs from user
C      INNAME.....Input UV file name (name).      Standard defaults.
C      INCLASS....Input UV file name (class).     Standard defaults.
C      INSEQ......Input UV file name (seq. #).    0 => highest.
C      INDISK.....Disk drive # of input UV file.  0 => any.
C      INVERS.....Specifies the version of the PC table to be read as
C                 input.   0 -> highest.
C                 The output version is always highest + 1.
C      FREQID.....Frequency ID - 0 all
C      SUBARRAY...Subarray number - 0 all
C      CALSOUR....Calibration sources - all ' ' -> all
C      SOLINT.....Integration time in minutes (<= 0 -> scan)
C-----------------------------------------------------------------------
      INTEGER   IRET
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'PCASS.INC'
C-----------------------------------------------------------------------
C                                       Get parms, open things
      CALL PCASSI (IRET)
C                                       do bandpass
      IF (IRET.EQ.0) CALL PCASST (IRET)
C                                       close down
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE PCASSI (IERR)
C-----------------------------------------------------------------------
C   PCASSI performs initialization for AIPS task PCASS.  It gets the
C   adverbs, opens the catalog file for 'WRIT' (eventually), sorts and
C   opens the PC input file, and determines the scan structure applying
C   to the calibration sources
C   Output:
C      IERR    I      Error code: 0 => keep going, else quit.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'PCASS.INC'
      INCLUDE 'PCDATA.INC'
      INCLUDE 'PCSCAN.INC'
      INCLUDE 'PCBPASS.INC'
      INCLUDE 'INCS:DSEL.INC'
      CHARACTER INTYP*2, STAT*4, PRGN*6, KEYSPC(2)*24
      INTEGER   IROUND, PCLUN, JERR, KEY(2,2), NKEY, KOLS(2), J1, J2,
     *   I, KEYSUB(2,2), IROW, BUFFER(612), VER, LUN, IDSOUR, JSUB,
     *   IFQID, VSTART, VEND, NROW, NPARMS, BPLUN
      REAL      FKEY(2,2), TEPS, DTIME, RTIME
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PCLUN, BPLUN, LUN, INTYP /87, 88, 43, 'UV'/
      DATA PRGN /'PCASS '/
      DATA NKEY /2/
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
      DATA KEYSPC /'ANTENNA_NO', 'TIME '/
C                                       8 milliseconds
      DATA TEPS /9.E-8/
C-----------------------------------------------------------------------
C                                       AIPS init
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      NCFILE = 0
      NSCR = 0
C                                       get adverbs
      NPARMS = 140
      IERR = 0
      CALL GTPARM (PRGN, NPARMS, RQUICK, XINNAM, PCBUFF, JERR)
      IF (JERR.NE.0) THEN
         RQUICK = .TRUE.
         IERR = 8
         IF (JERR.EQ.1) THEN
            GO TO 999
         ELSE
            WRITE (MSGTXT,1000) JERR, 'GET INPUT ADVERBS'
            CALL MSGWRT (8)
            END IF
         END IF
C                                       restart AIPS
      IF (RQUICK) CALL RELPOP (IERR, PCBUFF, JERR)
      IF (IERR.NE.0) GO TO 999
C                                       Hollerith -> Char
      CALL H2CHR (12, 1, XINNAM, INNAM)
      CALL H2CHR (6, 1, XINCLS, INCLS)
      INSEQ = IROUND (XSEQ)
      INDISK = IROUND (XDISK)
      INVERS = IROUND (XVER)
      CNO = 1
      CALL CATDIR ('SRCH', INDISK, CNO, INNAM, INCLS, INSEQ, INTYP,
     *   NLUSER, STAT, PCBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, INNAM, INCLS, INSEQ, INTYP,
     *      INDISK
         GO TO 990
         END IF
C                                       Get catblk, mark file write
      CALL CATIO ('READ', INDISK, CNO, CATBLK, 'WRIT', PCBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READ CATALOG HEADER'
         GO TO 990
         END IF
      NCFILE = 1
      FVOL(1) = INDISK
      FCNO(1) = CNO
      FRW(1) = 1
      CALL UVPGET (IERR)
C                                       DSEL parms
      IUDISK = INDISK
      IUCNO = CNO
      CALL COPY (256, CATBLK, CATUV)
C                                       Open PC file
      CALL PCINI ('READ', PCBUFF, INDISK, CNO, INVERS, CATBLK, PCLUN,
     *   PCRNO, PCKOLS, PCNUMV, PCNPOL, PCNIF, NUMTON, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN INPUT PC TABLE'
         GO TO 990
         END IF
C                                       Set column pointers for sort
      CALL FNDCOL (NKEY, KEYSPC, 24, .TRUE., PCBUFF, KOLS, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'FIND PC COLUMNS'
         GO TO 990
         END IF
C                                       Sort to ant-time order
      J1 = 1
      J2 = 2
      IF ((PCBUFF(43).NE.KOLS(J1)) .OR. (PCBUFF(44).NE.KOLS(J2))) THEN
C                                       Close
         CALL TABIO ('CLOS', 0, PCRNO, PCBUFF, PCBUFF, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'CLOSE PC TABLE'
            GO TO 990
            END IF
C                                       sort
         KEY(1,1) = KOLS(J1)
         KEY(2,1) = KOLS(J1)
         KEY(1,2) = KOLS(J2)
         KEY(2,2) = KOLS(J2)
         CALL TABSRT (INDISK, CNO, 'PC', INVERS, INVERS, KEY, KEYSUB,
     *      FKEY, PCBUFF, CATBLK, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'SORT PC TABLE'
            GO TO 990
            END IF
C                                       Re-open PC table for read
         CALL PCINI ('READ', PCBUFF, INDISK, CNO, INVERS, CATBLK, PCLUN,
     *      PCRNO, PCKOLS, PCNUMV, PCNPOL, PCNIF, NUMTON, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN SORTED PC TABLE'
            GO TO 990
            END IF
         END IF
C                                       number of records
      NPCINR = PCBUFF(5)
      FRQSEL = IROUND (XFQ)
      IF (FRQSEL.LE.0) FRQSEL = 0
      SUBARR = IROUND (XSUB)
      IF (SUBARR.LE.0) SUBARR = 0
      XSUB = SUBARR
C                                       find cal sources
      DO 20 I= 1,30
         CALL H2CHR (16, 1, CALSOR(1,I), SOURCS(I))
         CALSOU(I) = SOURCS(I)
 20      CONTINUE
      IXLUN = 28
      CALL SOUFIL (IERR)
C                                       find scan structure
      VER = 1
      CALL NDXINI ('READ', BUFFER, INDISK, CNO, VER, CATBLK, LUN,
     *   INXRNO, NXKOLS, NXNUMV, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING INDEX TABLE'
         GO TO 990
         END IF
      NROW = BUFFER(5)
      NOSCAN = 0
      DO 100 IROW = 1,NROW
         CALL TABNDX ('READ', BUFFER, INXRNO, NXKOLS, NXNUMV, RTIME,
     *      DTIME, IDSOUR, JSUB, VSTART, VEND, IFQID, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READING INDEX TABLE'
            GO TO 990
            END IF
C                                       subarray, fqid match
         IF ((SUBARR.GT.0) .AND. (JSUB.GT.0) .AND. (JSUB.NE.SUBARR))
     *      GO TO 100
         IF ((FRQSEL.GT.0) .AND. (IFQID.GT.0) .AND. (IFQID.NE.FRQSEL))
     *      GO TO 100
C                                       desired source
         IF (NCALWD.GT.0) THEN
            DO 30 I = 1,NCALWD
               IF (IDSOUR.EQ.CALWAN(I)) THEN
                  IF (.NOT.DOCWNT) GO TO 100
                  GO TO 40
                  END IF
 30            CONTINUE
            IF (DOCWNT) GO TO 100
            END IF
 40      NOSCAN = NOSCAN + 1
         TSCAN(1,NOSCAN) = RTIME - 0.5 * DTIME - TEPS
         TSCAN(2,NOSCAN) = RTIME + 0.5 * DTIME + TEPS
         SSCAN(1,NOSCAN) = IDSOUR
         SSCAN(2,NOSCAN) = JSUB
         SSCAN(3,NOSCAN) = IFQID
 100     CONTINUE
      CALL TABNDX ('CLOS', BUFFER, INXRNO, NXKOLS, NXNUMV, TIME, DTIME,
     *   IDSOUR, JSUB, VSTART, VEND, IFQID, IERR)
C                                       set up BP parameters
      IF (JLOCIF.GE.0) THEN
         BPNIF = CATBLK(KINAX+JLOCIF)
      ELSE
         BPNIF = 1
         END IF
      BPNCHN = CATBLK(KINAX+JLOCF)
      BPNPOL = MIN (2, CATBLK(KINAX+JLOCS))
      BPREFA = 0
C                                       get FQ settings first
      VER = 1
      CALL CHNDAT ('READ', BPBUFF, INDISK, CNO, VER, CATBLK, BPLUN,
     *   NUMIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READING FQ TABLE FOR FREQUENCIES'
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PCASSI: ERROR',I5,' ON ',A)
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,1X,A2,
     *   'DISK=',I2)
      END
      SUBROUTINE PCASST (IERR)
C-----------------------------------------------------------------------
C   PCASST uses the open PC table to generate a bandpass table
C   Output:
C      IERR   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'PCASS.INC'
      INCLUDE 'PCDATA.INC'
      INCLUDE 'PCBPASS.INC'
      INCLUDE 'PCSCAN.INC'
      INTEGER   IANT, ISCAN, JREC, I, K, LIF, LTONE, LPOL, J, IFSTEP,
     *   PCOUNT(2,MAXTON,MAXIF), BPLUN, NUMSHF, BCHAN
      LONGINT   BPPTR
      LOGICAL   FIRST
      REAL      BNDPAS(2), PCSUM(3,2,MAXTON,MAXIF), PCDELY(2,MAXIF),
     *   PCPHAS(2,MAXIF), ERDELY(2,MAXIF), ERPHAS(2,MAXIF), PRTLEV,
     *   LOWSHF, DELSHF
      DOUBLE PRECISION PCFRQS(2,MAXTON,MAXIF)
      CHARACTER LBPTYP*8
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA BPLUN /88/
C-----------------------------------------------------------------------
      IFSTEP = 1
      PRTLEV = 0.0
C                                       memory for BNDPAS
      J = (2 * BPNCHN * BPNIF * BPNPOL - 1) / 1024 + 4
      CALL ZMEMRY ('GET ', 'PCASST', J, BNDPAS, BPPTR, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'GETTING DYNAMIC MEMORY'
         GO TO 990
         END IF
C                                       get max ant number
      PCRNO = NPCINR
      CALL TABPC ('READ', PCBUFF, PCRNO, PCKOLS, PCNUMV, PCNPOL,
     *   TIME, TIMINT, SOUNUM, ANTNUM, ISUB, IDFREQ, CABCAL,
     *   STATE, PCFREQ, PCREAL, PCIMAG, PCRATE, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READ PC TABLE MAX REC'
         GO TO 990
         END IF
      BPNUMA = ANTNUM
C                                       create new BP table
      OUVER = 0
      BCHAN = 1
      NUMSHF = 2
      LOWSHF = 0.0
      DELSHF = 0.0
      LBPTYP = ' '
      CALL BPINI ('WRIT', BPBUFF, INDISK, CNO, OUVER, CATBLK, BPLUN,
     *   IBPRNO, BPKOLS, BPNUMV, BPNUMA, BPNPOL, BPNIF, BPNCHN, BCHAN,
     *   NUMSHF, LOWSHF, DELSHF, LBPTYP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CREATING NEW BP TABLE'
         GO TO 990
         END IF
C                                       loop through PC table
      JREC = 0
      K = 2 * MAXTON * MAXIF
      DO 200 IANT = 1,BPNUMA
         CALL FILL (K, 0, PCOUNT)
         CALL RFILL (3*K, 0.0, PCSUM)
         ISCAN = 0
         FIRST = .TRUE.
 10      PCRNO = JREC + 1
         IF (PCRNO.GT.NPCINR) GO TO 100
         CALL TABPC ('READ', PCBUFF, PCRNO, PCKOLS, PCNUMV, PCNPOL,
     *      TIME, TIMINT, SOUNUM, ANTNUM, ISUB, IDFREQ, CABCAL,
     *      STATE, PCFREQ, PCREAL, PCIMAG, PCRATE, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ PC TABLE'
            GO TO 990
            END IF
         IF (IERR.LT.0) THEN
            JREC = JREC + 1
            GO TO 10
            END IF
         IF (ANTNUM.NE.IANT) GO TO 100
C                                       find which scan
 15      IF (ISCAN.EQ.0) THEN
            DO 20 I = 1,NOSCAN
               IF ((TIME.GE.TSCAN(1,I)) .AND. (TIME.LE.TSCAN(2,I)))
     *            ISCAN = I
 20            CONTINUE
            IF (ISCAN.EQ.0) THEN
               JREC = JREC + 1
               GO TO 10
               END IF
            FIRST = .TRUE.
            END IF
C                                       add in
         IF (TIME.LE.TSCAN(2,ISCAN)) THEN
            CALL PCFITR (PCNPOL, PCNIF, IFSTEP, NUMTON, PCFREQ, PCREAL,
     *         PCIMAG, PRTLEV, PCDELY, PCPHAS, ERDELY, ERPHAS, WEIGHT,
     *         IERR)
            DO 40 LIF = 1,PCNIF
               DO 35 LTONE = 1,NUMTON
                  DO 30 LPOL = 1,PCNPOL
                     IF (FIRST) PCFRQS(LPOL,LTONE,LIF) =
     *                  PCFREQ(LPOL,LTONE,LIF)
                     IF ((PCREAL(LPOL,LTONE,LIF).NE.FBLANK) .AND.
     *                  (PCIMAG(LPOL,LTONE,LIF).NE.FBLANK)) THEN
                        PCOUNT(LPOL,LTONE,LIF) = 1 +
     *                     PCOUNT(LPOL,LTONE,LIF)
                        PCSUM(1,LPOL,LTONE,LIF) =
     *                     PCSUM(1,LPOL,LTONE,LIF) +
     *                     SQRT (PCREAL(LPOL,LTONE,LIF)**2 +
     *                     PCIMAG(LPOL,LTONE,LIF)**2)
                        PCSUM(2,LPOL,LTONE,LIF) = PCREAL(LPOL,LTONE,LIF)
     *                     + PCSUM(2,LPOL,LTONE,LIF)
                        PCSUM(3,LPOL,LTONE,LIF) = PCIMAG(LPOL,LTONE,LIF)
     *                     + PCSUM(3,LPOL,LTONE,LIF)
                        END IF
 30                  CONTINUE
 35               CONTINUE
 40            CONTINUE
            JREC = JREC + 1
            FIRST = .FALSE.
            GO TO 10
C                                       scan done - average
         ELSE
            J = 0
            DO 60 LIF = 1,PCNIF
               DO 55 LTONE = 1,NUMTON
                  DO 50 LPOL = 1,PCNPOL
                     IF (PCOUNT(LPOL,LTONE,LIF).GT.0) THEN
                        PCSUM(1,LPOL,LTONE,LIF) =
     *                     PCSUM(1,LPOL,LTONE,LIF) /
     *                     PCOUNT(LPOL,LTONE,LIF)
                        PCSUM(2,LPOL,LTONE,LIF) =
     *                     PCSUM(2,LPOL,LTONE,LIF) /
     *                     PCOUNT(LPOL,LTONE,LIF)
                        PCSUM(3,LPOL,LTONE,LIF) =
     *                     PCSUM(3,LPOL,LTONE,LIF) /
     *                     PCOUNT(LPOL,LTONE,LIF)
                        J = J + 1
                     ELSE
                        PCSUM(1,LPOL,LTONE,LIF) = FBLANK
                        PCSUM(2,LPOL,LTONE,LIF) = FBLANK
                        PCSUM(3,LPOL,LTONE,LIF) = FBLANK
                        END IF
 50                  CONTINUE
 55               CONTINUE
 60            CONTINUE
            IF (J.GT.0) THEN
               CALL BPWRIT (IANT, TSCAN(1,ISCAN), SSCAN(1,ISCAN), PCSUM,
     *            PCFRQS, BPNCHN, BPNIF, BPNPOL, BNDPAS(1+BPPTR),
     *            IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
            ISCAN = 0
            CALL FILL (K, 0, PCOUNT)
            CALL RFILL (3*K, 0.0, PCSUM)
            GO TO 15
            END IF
C                                       antenna or whole file done
 100     J = 0
         DO 120 LIF = 1,PCNIF
            DO 115 LTONE = 1,NUMTON
               DO 110 LPOL = 1,PCNPOL
                  IF (PCOUNT(LPOL,LTONE,LIF).GT.0) THEN
                     PCSUM(1,LPOL,LTONE,LIF) = PCSUM(1,LPOL,LTONE,LIF) /
     *                  PCOUNT(LPOL,LTONE,LIF)
                     PCSUM(2,LPOL,LTONE,LIF) = PCSUM(2,LPOL,LTONE,LIF) /
     *                  PCOUNT(LPOL,LTONE,LIF)
                     PCSUM(3,LPOL,LTONE,LIF) = PCSUM(3,LPOL,LTONE,LIF) /
     *                  PCOUNT(LPOL,LTONE,LIF)
                     J = J + 1
                  ELSE
                     PCSUM(1,LPOL,LTONE,LIF) = FBLANK
                     PCSUM(2,LPOL,LTONE,LIF) = FBLANK
                     PCSUM(3,LPOL,LTONE,LIF) = FBLANK
                     END IF
 110              CONTINUE
 115           CONTINUE
 120        CONTINUE
         IF (J.GT.0) THEN
            CALL BPWRIT (IANT, TSCAN(1,ISCAN), SSCAN(1,ISCAN), PCSUM,
     *         PCFRQS, BPNCHN, BPNIF, BPNPOL, BNDPAS(1+BPPTR), IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         IF (PCRNO.GT.NPCINR) GO TO 900
 200     CONTINUE
C                                       close tables
 900  CALL TABIO ('CLOS', 0, IBPRNO, BPBUFF, BPBUFF, IERR)
      CALL TABIO ('CLOS', 0, PCRNO, PCBUFF, PCBUFF, IERR)
      IERR = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PCASST: ERROR',I4,' ON ',A)
      END
      SUBROUTINE BPWRIT (IANT, TSCAN, SSCAN, PCSUM, PCFRQS, NC, NI, NP,
     *   BNDPAS, IERR)
C-----------------------------------------------------------------------
C   BPWRIT fills in a BP table record interpolating from PC freqs
C   Inputs:
C      IANT     I      Antenna number
C      TSCAN    R(2)   Time range
C      SSCAN    I(3)   source number, subarray, fqid
C      PCSUM    R(*)   Average PC amplitude (3,2,MAXTON,MAXIF)
C      PCFRQS   D(*)   Frequencies of PC data (2,MAXTON,MAXIF)
C      NC       I      Number channel in BNDPAS
C      NI       I      Number IFs in BNDPAS
C      NP       I      Number polarizations in BNDPAS
C   Outputs:
C      BNDPAS   R(*)   bandpass function (2,NC,NI,NP)
C      IERR     I      error code from TABBP
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PPCV.INC'
      INTEGER   IANT, SSCAN(3), NC, NI, NP, IERR
      REAL      TSCAN(2), PCSUM(3,2,MAXTON,*), BNDPAS(2,NC,NI,NP)
      DOUBLE PRECISION PCFRQS(2,MAXTON,*)
C
      INCLUDE 'PCASS.INC'
      INCLUDE 'PCBPASS.INC'
      INCLUDE 'PCDATA.INC'
      INTEGER   LCHAN, LIF, LP, SOURID, REFANT, NS(2)
      REAL      BPAMP(3,2), INTERV, BPS(2), FACT, AMP, PHAS
      DOUBLE PRECISION CFREQ, BPTIME
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      SOURID = SSCAN(1)
      BPSUBA = SSCAN(2)
      BPFQID = SSCAN(3)
      BPTIME = (TSCAN(1) + TSCAN(2)) / 2.0
      INTERV = TSCAN(2) - TSCAN(1)
      REFANT = 0
      DO 50 LIF = 1,NI
         WEIGHT(LIF) = 1.0
         IF (NP.EQ.2) WEIGHT(LIF+NI) = 1.0
         NS(1) = 0
         NS(2) = 0
         BPS(1) = 0.0
         BPS(2) = 0.0
         DO 30 LCHAN = 1,NC
            CFREQ = CATD(KDCRV+JLOCF) + (LCHAN - CATR(KRCRP+JLOCF)) *
     *         FINC(LIF) + FOFF(LIF)
            CALL GETVAL (BPARM(3), CFREQ, NUMTON, PCFRQS(1,1,LIF),
     *         PCSUM(1,1,1,LIF), NP, BPAMP)
            DO 20 LP = 1,NP
               IF (BPAMP(1,LP).EQ.FBLANK) THEN
                  BNDPAS(1,LCHAN,LIF,LP) = FBLANK
                  BNDPAS(2,LCHAN,LIF,LP) = FBLANK
               ELSE
                  IF (BPARM(1).GT.0.0) THEN
                     AMP = SQRT (BPAMP(2,LP)**2 + BPAMP(3,LP)**2)
                  ELSE
                     AMP = BPAMP(1,LP)
                     END IF
                  IF (BPARM(2).GT.0.0) THEN
                     PHAS = ATAN2 (BPAMP(3,LP), BPAMP(2,LP))
                  ELSE
                     PHAS = 0
                     END IF
                  BNDPAS(1,LCHAN,LIF,LP) = AMP * COS (PHAS)
                  BNDPAS(2,LCHAN,LIF,LP) = AMP * SIN (PHAS)
                  IF ((LCHAN.GT.1) .AND.(LCHAN.LT.NC)) THEN
                     NS(LP) = NS(LP) + 1
                     BPS(LP) = BPS(LP) + AMP
                     END IF
                  END IF
 20            CONTINUE
 30         CONTINUE
C                                       normalize
         DO 45 LP = 1,NP
            IF (NS(LP).GT.0) THEN
               FACT = BPS(LP) / NS(LP)
               IF (FACT.LE.0.0) FACT = 1.0
               DO 40 LCHAN = 1,NC
                  IF (BNDPAS(1,LCHAN,LIF,LP).NE.FBLANK) THEN
                     BNDPAS(1,LCHAN,LIF,LP) = BNDPAS(1,LCHAN,LIF,LP) /
     *                  FACT
                     BNDPAS(2,LCHAN,LIF,LP) = BNDPAS(2,LCHAN,LIF,LP) /
     *                  FACT
                     END IF
 40               CONTINUE
               END IF
 45         CONTINUE
 50      CONTINUE
      CALL TABBP ('WRIT', BPBUFF, IBPRNO, BPKOLS, BPNUMV, BPNIF, BPNCHN,
     *   BPNPOL, BPTIME, INTERV, SOURID, BPSUBA, IANT, BANDW, CHSHFT,
     *   BPFQID, REFANT, WEIGHT, BNDPAS, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'WRITING BP TABLE'
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPWRIT: ERROR',I4,' ON ',A)
      END
      SUBROUTINE GETVAL (DOSQRT, CFREQ, NUMTON, PCFRQS, PCSUM, NP,
     *   BPAMP)
C-----------------------------------------------------------------------
C   GETVAL finds the frequencies closest to the channel frequency in
C   the PC table and then interpolates to find the bandpass value
C   Inputs
C      DOSQRT   R          > 0 -> take SQRT of amp
C      CFREQ    D          channel frequency
C      NUMTON   I          Number of tones
C      PCFRQS   D(2,*)     PC table frequencies
C      PCSUM    R(3,2,*)   PC table amplitudes, real, imag
C      NP       I          Number polarizatiosn
C   Outputs
C      BPAMP    R(3,2)     BP correction value (sqrt (interp PCSUM))
C-----------------------------------------------------------------------
      DOUBLE PRECISION CFREQ, PCFRQS(2,*)
      INTEGER   NUMTON, NP
      REAL      DOSQRT, PCSUM(3,2,*), BPAMP(3,*)
C
      INCLUDE 'INCS:PPCV.INC'
      INTEGER   LP, J1, J2, J3, I
      DOUBLE PRECISION DIFFS(MAXTON), DMIN, ARG(3), FUNC(3), FITPAR,
     *   DMAX
      REAL      AMP, PHAS, RAMP, IAMP
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      DO 50 LP = 1,NP
         BPAMP(1,LP) = FBLANK
         BPAMP(2,LP) = FBLANK
         BPAMP(3,LP) = FBLANK
         DO 10 I = 1,NUMTON
            IF (PCSUM(1,LP,I).NE.FBLANK) THEN
               DIFFS(I) = ABS (CFREQ - PCFRQS(LP,I))
            ELSE
               DIFFS(I) = 1.0D30
               END IF
 10         CONTINUE
C                                       find min
         J1 = 0
         J2 = 0
         J3 = 0
         DMIN = 1.0D20
         DO 15 I = 1,NUMTON
            IF (DIFFS(I).LT.DMIN) THEN
               J1 = I
               DMIN = DIFFS(I)
               END IF
 15         CONTINUE
         DMIN = 1.0D20
         DO 20 I = 1,NUMTON
            IF ((DIFFS(I).LT.DMIN) .AND. (I.NE.J1)) THEN
               J2 = I
               DMIN = DIFFS(I)
               END IF
 20         CONTINUE
         DMIN = 1.0D20
         DO 25 I = 1,NUMTON
            IF ((DIFFS(I).LT.DMIN) .AND. (I.NE.J1) .AND. (I.NE.J2)) THEN
               J3 = I
               DMIN = DIFFS(I)
               END IF
 25         CONTINUE
C                                       do we have all 3?
         IF (J3.LE.0) THEN
            IF (J2.LE.0) THEN
               IF (J1.NE.0) THEN
                  BPAMP(1,LP) = PCSUM(1,LP,J1)
                  AMP = SQRT (PCSUM(2,LP,J1)**2 + PCSUM(3,LP,J1)**2)
                  IF (DOSQRT.GT.0.0) THEN
                     BPAMP(1,LP) = SQRT (BPAMP(1,LP))
                     AMP = SQRT (AMP)
                     END IF
                  PHAS = ATAN2 (PCSUM(3,LP,J1), PCSUM(2,LP,J1))
                  BPAMP(2,LP) = AMP * COS (PHAS)
                  BPAMP(3,LP) = AMP * SIN (PHAS)
                  END IF
            ELSE
               BPAMP(1,LP) = PCSUM(1,LP,J1) + (CFREQ - PCFRQS(LP,J1)) *
     *            (PCSUM(1,LP,J2) - PCSUM(1,LP,J1)) /
     *            (PCFRQS(LP,J2) - PCFRQS(LP,J1))
               IF (DOSQRT.GT.0.0) BPAMP(1,LP) = SQRT (BPAMP(1,LP))
               RAMP = PCSUM(2,LP,J1) + (CFREQ - PCFRQS(LP,J1)) *
     *            (PCSUM(2,LP,J2) - PCSUM(2,LP,J1)) /
     *            (PCFRQS(LP,J2) - PCFRQS(LP,J1))
               IAMP = PCSUM(3,LP,J1) + (CFREQ - PCFRQS(LP,J1)) *
     *            (PCSUM(3,LP,J2) - PCSUM(3,LP,J1)) /
     *            (PCFRQS(LP,J2) - PCFRQS(LP,J1))
               AMP = SQRT (RAMP*RAMP + IAMP*IAMP)
               IF (DOSQRT.GT.0.0) AMP = SQRT (AMP)
               PHAS = ATAN2 (IAMP, RAMP)
               BPAMP(2,LP) = AMP * COS (PHAS)
               BPAMP(3,LP) = AMP * SIN (PHAS)
               END IF
C                                       yes
         ELSE
            ARG(1) = CFREQ - PCFRQS(LP,J1)
            ARG(2) = CFREQ - PCFRQS(LP,J2)
            ARG(3) = CFREQ - PCFRQS(LP,J3)
            DMIN = ARG(1) * ARG(3) * (ARG(3)-ARG(1))
            DMAX = ARG(1) * ARG(2) * (ARG(2)-ARG(1))
            FUNC(1) = PCSUM(1,LP,J1)
            FUNC(2) = PCSUM(1,LP,J2)
            FUNC(3) = PCSUM(1,LP,J3)
            FITPAR =
     *         ((FUNC(1)*ARG(2)*ARG(2) - FUNC(2)*ARG(1)*ARG(1)) * DMIN -
     *         (FUNC(1)*ARG(3)*ARG(3) - FUNC(3)*ARG(1)*ARG(1)) * DMAX) /
     *         ((ARG(2)**2-ARG(1)**2)*DMIN - (ARG(3)**2-ARG(1)**2)*DMAX)
            IF (FITPAR.GT.0.0D0) THEN
               BPAMP(1,LP) = FITPAR
               IF (DOSQRT.GT.0.0) BPAMP(1,LP) = SQRT (BPAMP(1,LP))
               END IF
            FUNC(1) = PCSUM(2,LP,J1)
            FUNC(2) = PCSUM(2,LP,J2)
            FUNC(3) = PCSUM(2,LP,J3)
            RAMP =
     *         ((FUNC(1)*ARG(2)*ARG(2) - FUNC(2)*ARG(1)*ARG(1)) * DMIN -
     *         (FUNC(1)*ARG(3)*ARG(3) - FUNC(3)*ARG(1)*ARG(1)) * DMAX) /
     *         ((ARG(2)**2-ARG(1)**2)*DMIN - (ARG(3)**2-ARG(1)**2)*DMAX)
            FUNC(1) = PCSUM(3,LP,J1)
            FUNC(2) = PCSUM(3,LP,J2)
            FUNC(3) = PCSUM(3,LP,J3)
            IAMP =
     *         ((FUNC(1)*ARG(2)*ARG(2) - FUNC(2)*ARG(1)*ARG(1)) * DMIN -
     *         (FUNC(1)*ARG(3)*ARG(3) - FUNC(3)*ARG(1)*ARG(1)) * DMAX) /
     *         ((ARG(2)**2-ARG(1)**2)*DMIN - (ARG(3)**2-ARG(1)**2)*DMAX)
            AMP = SQRT (RAMP*RAMP + IAMP*IAMP)
            IF (DOSQRT.GT.0.0) AMP = SQRT (AMP)
            PHAS = ATAN2 (IAMP, RAMP)
            BPAMP(2,LP) = AMP * COS (PHAS)
            BPAMP(3,LP) = AMP * SIN (PHAS)
            END IF
 50      CONTINUE
C
 999  RETURN
      END

