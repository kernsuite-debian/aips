LOCAL INCLUDE 'PRTSY.INC'
      HOLLERITH XNAMIN(3), XCLSIN(2), XSOUR(4,30), XCALC, XLPNAM(12),
     *   XOPTYP, XOPCOD, XCALIN(12), XCALOU(12)
      REAL      DSKIN, SEQIN, XINVER, XQUAL, XBAND, XFREQ, XFQID, XSUBA,
     *   XTIME(8), XFGVER, DOCRT, FACTOR(10)
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, XINVER, XSOUR,
     *   XQUAL, XCALC, XBAND, XFREQ, XFQID, XSUBA, XTIME, XFGVER, DOCRT,
     *   XLPNAM, XOPTYP, XOPCOD, FACTOR, XCALIN, XCALOU
      INTEGER   INDISK, INCNO, INVER, INSEQ, CDVER, DTYPE, CTYPE,
     *   LFGVER, SCRTCH(256), BUFFI(512), LUNP, FINDP, IPCNT, JP0
      REAL      BTIME, ETIME
      CHARACTER NAMEIN*12, CLASIN*6, LPNAME*48, OPTYPE*4, OPCODE*4,
     *   SRCS(1000)*16, CALIN*48, CALOUT*48
      COMMON /PARMS/ INDISK, INCNO, INSEQ, INVER, CDVER, DTYPE, CTYPE,
     *   BTIME, ETIME, SCRTCH, BUFFI, LFGVER, LUNP, FINDP, IPCNT, JP0
      COMMON /PARMC/ NAMEIN, CLASIN, LPNAME, OPTYPE, OPCODE, CALIN,
     *   CALOUT, SRCS
LOCAL END
LOCAL INCLUDE 'PRTSYUE.INC'
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION DDMIN(6,MAXIF,MAXANT), DDMAX(6,MAXIF,MAXANT),
     *   DDSUM(6,MAXIF,MAXANT), DDSUMS(6,MAXIF,MAXANT),
     *   DDCNT(6,MAXIF,MAXANT)
      REAL      TCAL(4,MAXIF,MAXANT)
      INTEGER   DDNIF, DDNANT, DDNPOL
      COMMON /DDVALS/ DDMIN, DDMAX, DDSUM, DDSUMS, DDCNT, TCAL, DDNIF,
     *   DDNANT, DDNPOL
LOCAL END
LOCAL INCLUDE 'REPSYTAB.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER MAXSCN
      PARAMETER (MAXSCN = 1001)
C
      INTEGER   NRECSY, ISYRNO, SYKOLS(12), SYNUMV(12), LUNSY, SSYVER,
     *   NRECAN(MAXANT), MAXREC, SSCAN(MAXSCN), NOSCAN, ARECAN(MAXANT),
     *   NRECSO(MAXSCN), ARECSO(MAXSCN), SRECSO(MAXSCN)
      REAL      TSCAN(MAXSCN), TTSCAN(MAXSCN), PDIFF(2,MAXIF),
     *   PSUM(2,MAXIF), PGAIN(2,MAXIF)
      EQUIVALENCE (NRECSO, TSCAN), (ARECSO, TTSCAN)
      COMMON /REPSYT/ TSCAN, TTSCAN, NRECSY, ISYRNO, SYKOLS, SYNUMV,
     *   LUNSY, SSYVER, NRECAN, MAXREC, SSCAN, NOSCAN, PDIFF, PSUM,
     *   PGAIN, ARECAN, SRECSO
LOCAL END
      PROGRAM PRTSY
C-----------------------------------------------------------------------
C! Task to print statistics from SY table
C# EXT-util Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 2012, 2014-2017
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
C   PRTSY will convert an EVLA SY (SysPower) table plus the CalDevice
C   CD table into a Tsys column in a TY table.
C   Inputs:   (from AIPS)
C      INNAME    R(3)   name of primary file.
C      INCLASS   R(2)   class of primary file.
C      INSEQ     R      sequence number of primary file.
C      INDISK    R      disk volume number. 0 means try all.
C      INVERS    R      MF file version number
C      OUTVERS   R      ST file version number
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   IRET
C
      REAL      SYDATA(2), SYAVG(2)
      LONGINT   PSYDAT, PSYAVG
      INTEGER   NWORDS, I
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'PRTSY.INC'
      INCLUDE 'REPSYTAB.INC'
      INCLUDE 'PRTSYUE.INC'
      DATA PRGNAM /'PRTSY'/
C-----------------------------------------------------------------------
C                                       start up
      CALL PRTSYI (PRGNAM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       get statistics
      IF (CTYPE.LT.1) THEN
         CALL PRTSYD (IRET)
         IF (IRET.NE.0) GO TO 990
C                                       display statistics
         CALL PRTCHP (IRET)
         IF (IRET.NE.0) GO TO 990
         CALL PRTSYP (IRET)
         IF (IRET.NE.0) GO TO 990
C                                       robust averages on scans
      ELSE IF (CTYPE.LE.3) THEN
         NWORDS = (2 * MAXREC * NUMPOL * NUMIF - 1) / 1024 + 3
         CALL ZMEMRY ('GET ', TSKNAM, NWORDS, SYDATA, PSYDAT, IRET)
         IF (IRET.EQ.0) THEN
            NOSCAN = MAX (1, NOSCAN)
            NWORDS = (6 * NOSCAN * NUMANT * NUMPOL * NUMIF - 1) / 1024
     *         + 3
            CALL ZMEMRY ('GET ', TSKNAM, NWORDS, SYAVG, PSYAVG, IRET)
            END IF
         IF (IRET.NE.0) THEN
            MSGTXT = 'FAILED TO GET DYNAMIC MEMORY'
            CALL MSGWRT (8)
            GO TO 990
            END IF
         CALL PRTSYA (NUMPOL, NUMIF, NOSCAN, NUMANT, SYDATA(1+PSYDAT),
     *      SYAVG(1+PSYAVG), IRET)
         IF (IRET.NE.0) GO TO 990
         IF (CTYPE.EQ.1) THEN
            CALL PRTCH1 (NUMPOL, NUMIF, NOSCAN, NUMANT,IRET)
            IF (IRET.EQ.0) CALL PRTSY1 (NUMPOL, NUMIF, NOSCAN, NUMANT,
     *         SYAVG(1+PSYAVG), IRET)
         ELSE IF (CTYPE.EQ.2) THEN
            CALL PRTCH2 (NUMPOL, NUMIF, NOSCAN, NUMANT, IRET)
            IF (IRET.EQ.0) CALL PRTSY2 (NUMPOL, NUMIF, NOSCAN, NUMANT,
     *         SYAVG(1+PSYAVG), IRET)
         ELSE IF (CTYPE.EQ.3) THEN
            CALL PRTCH3 (NUMPOL, NUMIF, NOSCAN, NUMANT, IRET)
            IF (IRET.EQ.0) CALL PRTSY3 (NUMPOL, NUMIF, NOSCAN, NUMANT,
     *         SYAVG(1+PSYAVG), IRET)
            END IF
C                                       remove sorted SY version
         CALL TABIO ('CLOS', 0, ISYRNO, BUFFI, BUFFI, I)
         CALL FNDEXT ('SY', CATBLK, I)
         IF ((SSYVER.EQ.I) .AND. (SSYVER.NE.INVER)) CALL DELEXT ('SY',
     *      INDISK, INCNO, 'WRWR', BUFFI, SCRTCH, SSYVER, I)
C                                       robust average on source
      ELSE
         NWORDS = (2 * MAXREC * NUMPOL * NUMIF - 1) / 1024 + 3
         CALL ZMEMRY ('GET ', TSKNAM, NWORDS, SYDATA, PSYDAT, IRET)
         IF (IRET.EQ.0) THEN
            NOSCAN = MAX (1, NOSCAN)
            NWORDS = (6 * NOSCAN * NUMANT * NUMPOL * NUMIF - 1) / 1024
     *         + 3
            CALL ZMEMRY ('GET ', TSKNAM, NWORDS, SYAVG, PSYAVG, IRET)
            END IF
         IF (IRET.NE.0) THEN
            MSGTXT = 'FAILED TO GET DYNAMIC MEMORY'
            CALL MSGWRT (8)
            GO TO 990
            END IF
         CALL PRTSYS (NUMPOL, NUMIF, NOSCAN, NUMANT, SYDATA(1+PSYDAT),
     *      SYAVG(1+PSYAVG), IRET)
         IF (IRET.NE.0) GO TO 990
         CALL PRTCH4 (NUMPOL, NUMIF, NOSCAN, NUMANT, SYAVG(1+PSYAVG),
     *      IRET)
         IF (IRET.NE.0) GO TO 990
         CALL PRTSY4 (NUMPOL, NUMIF, NOSCAN, NUMANT, SYAVG(1+PSYAVG),
     *      IRET)
C                                       write efficiencies
         IF ((IRET.LE.0) .AND. (CTYPE.EQ.5)) CALL PRTSYO (NUMPOL, NUMIF,
     *      NOSCAN, NUMANT, FRQSEL, SYAVG(1+PSYAVG), IRET)
C                                       remove sorted SY version
         CALL TABIO ('CLOS', 0, ISYRNO, BUFFI, BUFFI, I)
         CALL FNDEXT ('SY', CATBLK, I)
         IF ((SSYVER.EQ.I) .AND. (SSYVER.NE.INVER)) CALL DELEXT ('SY',
     *      INDISK, INCNO, 'WRWR', BUFFI, SCRTCH, SSYVER, I)
         END IF
C
C                                       close LP
 990  IF (LUNP.GT.0) THEN
         CALL LPCLOS (LUNP, FINDP, IPCNT, I)
         IF (I.GT.0) IRET = MAX (I, IRET)
         END IF
      IRET = MAX (0, IRET)
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE PRTSYI (PRGNAM, IRET)
C-----------------------------------------------------------------------
C   Inputs
C      PRGNAM   C*6   Task name
C   Outputs:
C      IRET     I     > 0 +> die on error
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*(*)
      INTEGER   IRET
C
      INTEGER   NTYPE, NCODE
      PARAMETER (NTYPE=7, NCODE=5)
C
      INTEGER   NPARMS, IERR, I, IROUND, LUN, LUNTMP, SUBA, FREQID
      LOGICAL   MATCH
      CHARACTER TYPIN*2, STAT*4, TYPES(NTYPE)*4, CODES(NCODE)*4
      INCLUDE 'PRTSY.INC'
      INCLUDE 'REPSYTAB.INC'
      INCLUDE 'PRTSYUE.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA TYPES /'DIFS', 'SUMS', 'TSYS', 'DRMS', 'SRMS', 'TRMS',
     *   'RMS '/
      DATA CODES /'SCAN', 'IFS ', 'ANTS', 'SORC', 'EFFS'/
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      LUNP = -1
C                                       get parameters, resume aips
      NPARMS = 192
      IRET = 0
      CALL GTPARM (PRGNAM, NPARMS, RQUICK, XNAMIN, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'GETTING ADVERBS'
         CALL MSGWRT (8)
         IRET = 8
         END IF
      IF ((IRET.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   DOCRT = MIN (-1.0, DOCRT)
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
      IF (DOCRT.GT.0.0) RQUICK = .FALSE.
      IF (RQUICK) RQUICK = LPNAME.NE.' '
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 990
      IRET = 8
C                                       interpret parameters
      CALL H2CHR (48, 1, XCALIN, CALIN)
      CALL H2CHR (48, 1, XCALOU, CALOUT)
      CALL H2CHR (12, 1, XNAMIN, NAMEIN)
      CALL H2CHR (6, 1, XCLSIN, CLASIN)
      INSEQ = SEQIN + 0.1
      INDISK = DSKIN + 0.1
      INVER = XINVER + 0.1
      CALL H2CHR (4, 1, XCALC, SELCOD)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      SELQUA = IROUND (XQUAL)
      SUBARR = IROUND (XSUBA)
      SUBARR = MAX (1, SUBARR)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      DTYPE = 0
      DO 20 I = 1,NTYPE
         IF (OPTYPE.EQ.TYPES(I)) DTYPE = I
 20      CONTINUE
      CTYPE = 0
      DO 21 I = 1,NCODE
         IF (OPCODE.EQ.CODES(I)) CTYPE = I
 21      CONTINUE
      BTIME = ((XTIME(4)/60.+XTIME(3))/60.0 + XTIME(2))/24.0 + XTIME(1)
      ETIME = ((XTIME(8)/60.+XTIME(7))/60.0 + XTIME(6))/24.0 + XTIME(5)
      IF (BTIME.EQ.0.0) BTIME = -1000.
      IF (ETIME.EQ.0.0) ETIME = 10000.
C                                       Check Rick's usage
      IF (CTYPE.EQ.5) THEN
         IF ((CALOUT.EQ.' ') .OR. (FACTOR(5).LE.0.0) .OR.
     *      (FACTOR(7).LE.0.0)) THEN
            MSGTXT = 'WE NEED SOME CALOUT AND AN ANTENNA AND A ' //
     *         'SOURCE TO DIVIDE'
            IERR = 10
            GO TO 990
            END IF
         END IF
C                                       Get CATBLK from old file.
      INCNO = 1
      TYPIN = 'UV'
      CALL CATDIR ('SRCH', INDISK, INCNO, NAMEIN, CLASIN, INSEQ, TYPIN,
     *   NLUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLASIN, INSEQ, INDISK, NLUSER
         GO TO 990
         END IF
      STAT = 'READ'
      IF (CTYPE.GT.0) STAT = 'WRIT'
      CALL CATIO ('READ', INDISK, INCNO, CATBLK, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'GETTING CATBLK'
         GO TO 990
         END IF
      NCFILE = 1
      FCNO(NCFILE) = INCNO
      FVOL(NCFILE) = INDISK
      FRW(NCFILE) = 0
      IF (CTYPE.GT.0) FRW(NCFILE) = 1
      CALL UVPGET (IRET)
      CALL COPY (256, CATBLK, CATUV)
      CALL FNDEXT ('FG', CATBLK, I)
      LFGVER = IROUND (XFGVER)
      IF (I.LE.0) FGVER = 0
      LFGVER = MIN (LFGVER, I)
      JP0 = 0
      IF (ICOR0.EQ.-2) JP0 = 1
      IF (ICOR0.EQ.-5) JP0 = 2
      IF (ICOR0.EQ.-6) JP0 = 3
C                                       Save relevant pointers for
C                                       flagging
      KLOCFQ = ILOCFQ
      KLOCIF = JLOCIF
      KLOCFY = JLOCF
      LRECIN = LREC
      CALL FNDEXT ('AN', CATBLK, I)
      IF ((SUBARR.GT.I) .OR. (SUBARR.LT.0)) SUBARR = 0
      IUDISK = INDISK
      IUCNO = INCNO
      IFLUN = 30
      KNCOR = NCOR
      KCOR0 = ICOR0
      KNCF = INCF / CATUV(KINAX)
      KNCIF = INCIF / CATUV(KINAX)
      KNCS = INCS / CATUV(KINAX)
      UBUFSZ = UVBFSL * 2
C                                       DSEL common
      UNAME = NAMEIN
      UCLAS = CLASIN
      UDISK = INDISK
      IUDISK = INDISK
      USEQ = INSEQ
      IUSEQ = INSEQ
      IUCNO = INCNO
      IXLUN = LUNTMP (1)
      CALL SOUFIL (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'GETTING SOURCE LIST'
         GO TO 990
         END IF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
C                                       Find specified FQ id
      CALL FQMATC (INDISK, INCNO, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FRQSEL, IERR)
      IF ((.NOT.MATCH) .OR. (IERR.NE.0)) THEN
         WRITE (MSGTXT,1000) IERR, 'GETTING DESIRED FREQID'
         GO TO 990
         END IF
C                                       version numbers
      CALL FNDEXT ('SY', CATBLK, I)
      IF ((INVER.LE.0) .OR. (INVER.GT.I)) INVER = I
      CALL FNDEXT ('CD', CATBLK, CDVER)
      IF ((INVER.LE.0) .OR. (CDVER.LE.0)) THEN
         MSGTXT = 'YOU MUST HAVE AN SY AND CD TABLE TO RUN THIS TASK'
         GO TO 990
         END IF
C                                       get Tcal values
      SUBA = SUBARR
      FREQID = 1
      CALL GETCDS (INDISK, INCNO, CDVER, SUBA, FREQID, CATBLK, TCAL,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING THE TCAL VALUES'
         GO TO 990
         END IF
C                                       open SY table to read
      LUNSY = 29
      CALL SYINI ('READ', BUFFI, INDISK, INCNO, INVER, CATBLK, LUNSY,
     *   ISYRNO, SYKOLS, SYNUMV, NUMANT, NUMPOL, NUMIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING THE SY TABLE FOR READ'
         GO TO 990
         END IF
      NRECSY = BUFFI(5)
C                                       prepare for detailed
      IF (CTYPE.GT.0) THEN
         CALL SYPREP (IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'PREPING THE SY TABLE FOR REPORTS'
            GO TO 990
            END IF
         CALL SYSORC (IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING THE SOURCE TABLE'
            GO TO 990
            END IF
         END IF
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PRTSYI: ERROR',I4,1X,A)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
      END
      SUBROUTINE SYPREP (IRET)
C-----------------------------------------------------------------------
C   SYPREP makes a sorted copy of the SY table, reads it counting the
C   number of records for each antenna, and returns the count array as
C   well as its maximum.  It also reads the NX table getting scan info.
C   Outputs:
C      IRET   I   Error code: > 0 -> quit
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'PRTSY.INC'
      INCLUDE 'REPSYTAB.INC'
      INTEGER   KEY(2,2), KEYSUB(2,2), LANT, SUBA, FREQID, IREC, SOURID,
     *   ANTNO, VER, IDSOUR, ISUB, VSTART, VEND, NREC, BUFFER(512), LUN,
     *   LUNTMP, FGKEY(2,2), K, J, CALTYP
      LOGICAL   WANT
      REAL      FKEY(2,2), TIMEI, CTIME, TEPS
      DOUBLE PRECISION TIME
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      DATA FGKEY /5,0, 1,0/
      DATA KEY  /4,0, 1,0/
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
C                                       8 milliseconds
      DATA TEPS /9.E-8/
C-----------------------------------------------------------------------
C                                       close the SY table to sort it
      CALL TABIO ('CLOS', 0, ISYRNO, BUFFI, BUFFI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT CLOSING OF INPUT SY TABLE'
         GO TO 990
         END IF
      CALL FNDEXT ('SY', CATBLK, SSYVER)
      SSYVER = SSYVER + 1
C                                       flag in copy init
      IF (LFGVER.GT.0) THEN
         FGVER = LFGVER
         IFLUN = 30
         CALL FLGINI ('READ', FGBUFF, INDISK, INCNO, FGVER, CATUV,
     *      IFLUN, IFGRNO, FGKOLS, FGNUMV, K)
         IF (K.NE.0) LFGVER = 0
C                                       Resort if necessary.
         IF ((LFGVER.GT.0) .AND. (FGBUFF(43).NE.FGKEY(1,1))) THEN
C                                       Sort to time order.
            CALL TABIO ('CLOS', 0, K, UBUFF, FGBUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'CLOSE FG TABLE TO SORT'
               GO TO 990
               END IF
            CALL TABSRT (IUDISK, IUCNO, 'FG', FGVER, FGVER, FGKEY,
     *         KEYSUB, FKEY, FGBUFF, CATUV, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'SORT FG TABLE'
               GO TO 990
               END IF
C                                       Re initialize.
            CALL FLGINI ('READ', FGBUFF, INDISK, INCNO, FGVER, CATUV,
     *         IFLUN, IFGRNO, FGKOLS, FGNUMV, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RE-OPEN FLAG TABLE'
               GO TO 990
               END IF
            END IF
         END IF
C                                       flag in copy, sort in place
      IF (CTYPE.GT.3) KEY(1,1) = 3
      IF (LFGVER.GT.0) THEN
         CALL SYFSEL (INDISK, INCNO, INDISK, INCNO, INVER, SSYVER,
     *      CATUV, CATBLK, BTIME, ETIME, BUFFI, BUFFER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'COPY/FLAG SY TABLE'
            GO TO 990
            END IF
         CALL TABSRT (INDISK, INCNO, 'SY', SSYVER, SSYVER, KEY, KEYSUB,
     *      FKEY, BUFFI, CATBLK, IRET)
C                                       no flag, sort in copy
      ELSE
         CALL TABSRT (INDISK, INCNO, 'SY', INVER, SSYVER, KEY, KEYSUB,
     *      FKEY, BUFFI, CATBLK, IRET)
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'SORTING INPUT SY TABLE'
         GO TO 990
         END IF
C                                       reopen
      CALL SYINI ('READ', BUFFI, INDISK, INCNO, SSYVER, CATBLK, LUNSY,
     *   ISYRNO, SYKOLS, SYNUMV, NUMANT, NUMPOL, NUMIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING SORTED SY TABLE FOR READ'
         GO TO 990
         END IF
      NRECSY = BUFFI(5)
C                                       scan based medians
      IF (CTYPE.LE.3) THEN
         CALL FILL (MAXANT, 0, NRECAN)
         CALL FILL (MAXANT, 0, ARECAN)
         LANT = 0
C                                       read loop
         DO 50 IREC = 1,NRECSY
C                                       read
            ISYRNO = IREC
            CALL TABSY ('READ', BUFFI, ISYRNO, SYKOLS, SYNUMV, NUMPOL,
     *         NUMIF, TIME, TIMEI, CALTYP, SOURID, ANTNO, SUBA, FREQID,
     *         PDIFF, PSUM, PGAIN, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING THE SORTED SY TABLE'
               GO TO 990
            ELSE IF (IRET.EQ.0) THEN
               IF (ANTNO.EQ.LANT) THEN
                  NRECAN(LANT) = NRECAN(LANT) + 1
                  IF ((TIME.GE.BTIME) .AND. (TIME.LE.ETIME))
     *               ARECAN(LANT) = ARECAN(LANT) + 1
               ELSE IF (ANTNO.GT.LANT) THEN
                  LANT = ANTNO
                  NRECAN(LANT) = 1
                  IF ((TIME.GE.BTIME) .AND. (TIME.LE.ETIME))
     *               ARECAN(LANT) = 1
               ELSE
                  IRET = 99
                  MSGTXT = 'SYPREP: SORTED SY TABLE NOT IN SORT ORDER!!'
                  GO TO 990
                  END IF
               END IF
 50         CONTINUE
         MAXREC = 0
         DO 60 LANT = 1,MAXANT
            MAXREC = MAX (MAXREC, ARECAN(LANT))
 60         CONTINUE
         IF (MAXREC.LE.0) THEN
            IRET = 98
            MSGTXT = 'SYPREP: FOUND NO SY RECORDS'
            GO TO 990
            END IF
C                                       read NX table
         VER = 1
         LUN = LUNTMP (1)
         CALL NDXINI ('READ', BUFFER, INDISK, INCNO, VER, CATBLK, LUN,
     *      INXRNO, NXKOLS, NXNUMV, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING INDEX TABLE'
            GO TO 90
            END IF
         NREC = BUFFER(5)
         NOSCAN = 1
         TSCAN(1) = -1.0
         TSCAN(2) = 10000.
         DO 80 IREC = 1,NREC
            CALL TABNDX ('READ', BUFFER, INXRNO, NXKOLS, NXNUMV, CTIME,
     *         TIMEI, IDSOUR, ISUB, VSTART, VEND, FREQID, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING INDEX TABLE'
               GO TO 90
            ELSE IF (IRET.EQ.0) THEN
               IF (((ISUB.LE.0) .OR. (SUBARR.LE.0) .OR.
     *            (ISUB.EQ.SUBARR)).AND. (CTIME.GE.BTIME) .AND.
     *            (CTIME.LE.ETIME)) THEN
                  IF (NOSCAN.EQ.1) THEN
                     TSCAN(NOSCAN) = CTIME - 0.5 * TIMEI - TEPS
                  ELSE
                     TSCAN(NOSCAN) = (TSCAN(NOSCAN) + CTIME - 0.5*TIMEI)
     *                  / 2.0
                     END IF
                  TTSCAN(NOSCAN) = CTIME
                  SSCAN(NOSCAN) = IDSOUR
                  SSCAN(NOSCAN+1) = 0
                  TSCAN(NOSCAN+1) = CTIME + 0.5 * TIMEI + TEPS
                  NOSCAN = NOSCAN + 1
                  IF (NOSCAN.GE.MAXSCN) THEN
                     MSGTXT = 'SYPREP: TOO MANY SCANS FOR OUR TABLES'
                     CALL MSGWRT (6)
                     GO TO 90
                     END IF
                  END IF
               END IF
 80         CONTINUE
C
 90      IF (IRET.NE.0) THEN
            CALL MSGWRT (6)
            NOSCAN = 1
            TSCAN(1) = -1.0
            TSCAN(2) = 10000.
            END IF
         CALL TABNDX ('CLOS', BUFFER, INXRNO, NXKOLS, NXNUMV, CTIME,
     *      TIMEI, IDSOUR, ISUB, VSTART, VEND, FREQID, IREC)
C                                       source based
      ELSE
         LANT = -1
         NOSCAN = 0
         CALL FILL (MAXSCN, 0, NRECSO)
         CALL FILL (MAXSCN, 0, ARECSO)
         WANT = NSOUWD.LE.0
C                                       read loop
         DO 150 IREC = 1,NRECSY
C                                       read
            ISYRNO = IREC
            CALL TABSY ('READ', BUFFI, ISYRNO, SYKOLS, SYNUMV, NUMPOL,
     *         NUMIF, TIME, TIMEI, CALTYP, SOURID, ANTNO, SUBA, FREQID,
     *         PDIFF, PSUM, PGAIN, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING THE SORTED SY TABLE'
               GO TO 990
            ELSE IF (IRET.EQ.0) THEN
               IF (SOURID.EQ.LANT) THEN
                  IF (WANT) THEN
                     NRECSO(NOSCAN) = NRECSO(NOSCAN) + 1
                     IF ((TIME.GE.BTIME) .AND. (TIME.LE.ETIME))
     *                  ARECSO(NOSCAN) = ARECSO(NOSCAN) + 1
                     END IF
               ELSE IF (SOURID.GT.LANT) THEN
                  LANT = SOURID
                  IF ((SOURID.GT.0) .AND. (NSOUWD.GT.0)) THEN
                     WANT = .NOT.DOSWNT
                     DO 110 J = 1,NSOUWD
                        IF (SOUWAN(J).EQ.SOURID) WANT = DOSWNT
 110                    CONTINUE
                     END IF
                  IF (WANT) THEN
                     NOSCAN = NOSCAN + 1
                     NRECSO(NOSCAN) = 1
                     IF ((TIME.GE.BTIME) .AND. (TIME.LE.ETIME))
     *                  ARECSO(NOSCAN) = 1
                     SSCAN(NOSCAN) = SOURID
                     SRECSO(NOSCAN) = IREC
                     END IF
               ELSE
                  IRET = 99
                  MSGTXT = 'SYPREP: SORTED SY TABLE NOT IN SORT ORDER!!'
                  GO TO 990
                  END IF
               END IF
 150        CONTINUE
         MAXREC = 0
         DO 160 LANT = 1,NOSCAN
            MAXREC = MAX (MAXREC, ARECSO(LANT))
 160        CONTINUE
         IF (MAXREC.LE.0) THEN
            IRET = 98
            MSGTXT = 'SYPREP: FOUND NO SY RECORDS'
            GO TO 990
            END IF
         END IF
      IRET = 0
      GO TO 999
C                                       SY table error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SYPREP: ERROR',I4,' ON ',A)
      END
      SUBROUTINE SYSORC (IRET)
C-----------------------------------------------------------------------
C   SYSORC gets the list of source names (if any)
C   Outputs
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'PRTSY.INC'
      INTEGER   VERS, BUFFER(512), LUN, NUMIF, LUNTMP, FREQID, ISURNO,
     *   SUKOLS(MAXSUC), SUNUMV(MAXSUC), IREC, NREC, IDSOU, QUAL, I
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION FREQO(MAXIF), BANDW, RAEPO, DECEPO, EPOCH, RAAPP,
     *   DECAPP, RAOBS, DECOBS, LSRVEL(MAXIF), LRESTF(MAXIF), PMRA,
     *   PMDEC
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
      DO 10 I = 1,1000
         SRCS(I) = ' '
 10      CONTINUE
C                                       single source?
      CALL FNDEXT ('SU', CATBLK, VERS)
      IF (VERS.LE.0) THEN
         CALL H2CHR (8, 1, CATH(KHOBJ), SRCS(1))
C                                       real source table
      ELSE
         LUN = LUNTMP (1)
         CALL SOUINI ('READ', BUFFER, INDISK, INCNO, VERS, CATBLK, LUN,
     *      NUMIF, VELTYP, VELDEF, FREQID, ISURNO, SUKOLS, SUNUMV, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING THE SU TABLE'
            GO TO 990
            END IF
         NREC = BUFFER(5)
         DO 20 IREC = 1,NREC
            CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *         SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF,
     *         PMRA, PMDEC, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING THE SU TABLE'
               GO TO 999
               END IF
            IF ((IDSOU.GT.0) .AND. (IDSOU.LE.1000) .AND. (IRET.EQ.0))
     *         SRCS(IDSOU) = SOUNAM
 20         CONTINUE
         CALL TABSOU ('CLOS', BUFFER, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF,
     *      PMRA, PMDEC, IRET)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SYSORC: ERROR',I4,' ON ',A)
      END
      SUBROUTINE PRTSYD (IRET)
C-----------------------------------------------------------------------
C   PRTSYD determines statistics of the SY table
C   Outputs:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SUBA, FREQID, IREC, LL, SOURID, ANTNO, LIF, LP, I,
     *   CALTYP
      REAL      TIMEI, TSYS, TC
      DOUBLE PRECISION TIME, IRNO
      LOGICAL   WANT
      INCLUDE 'PRTSY.INC'
      INCLUDE 'REPSYTAB.INC'
      INCLUDE 'PRTSYUE.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
      IRNO = 0.0D0
      IREC = 6 * MAXIF * MAXANT
      DDNIF = 0
      DDNANT = 0
      DDNPOL = 0
      CALL DFILL (IREC, 1.0D10, DDMIN)
      CALL DFILL (IREC, -1.0D10, DDMAX)
      CALL DFILL (IREC, 0.0D0, DDSUM)
      CALL DFILL (IREC, 0.0D0, DDSUMS)
      CALL DFILL (IREC, 0.0D0, DDCNT)
C                                       read/write loop
      DO 50 IREC = 1,NRECSY
C                                       read
         ISYRNO = IREC
         CALL TABSY ('READ', BUFFI, ISYRNO, SYKOLS, SYNUMV, NUMPOL,
     *      NUMIF, TIME, TIMEI, CALTYP, SOURID, ANTNO, SUBA, FREQID,
     *      PDIFF, PSUM, PGAIN, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING THE SY TABLE'
            GO TO 980
            END IF
C                                       include?
         WANT = IRET.EQ.0
         IF ((TIME.LT.BTIME) .OR. (TIME.GT.ETIME)) WANT = .FALSE.
         IF ((SOURID.GT.0) .AND. (NSOUWD.GT.0)) THEN
            WANT = .NOT.DOSWNT
            DO 10 I = 1,NSOUWD
               IF (SOUWAN(I).EQ.SOURID) WANT = DOSWNT
 10            CONTINUE
            END IF
         IF ((SUBA.GT.0) .AND. (SUBARR.GT.0) .AND. (SUBA.NE.SUBARR))
     *      WANT = .FALSE.
         IF ((FREQID.GT.0) .AND. (FRQSEL.GT.0) .AND. (FRQSEL.NE.FREQID))
     *      WANT = .FALSE.
         IF (.NOT.WANT) GO TO 50
C                                       convert
         DO 30 LIF = 1,NUMIF
            DO 20 LP = 1,NUMPOL
               IF (CALTYP.EQ.1) THEN
                  TC = TCAL(LP+2,LIF,ANTNO)
               ELSE
                  TC = TCAL(LP,LIF,ANTNO)
                  END IF
               IF ((TC.NE.FBLANK) .AND. (TC.GT.0.0) .AND.
     *            (PDIFF(LP,LIF).NE.FBLANK) .AND.
     *            (PSUM(LP,LIF).NE.FBLANK) .AND. (PDIFF(LP,LIF).GT.0.0)
     *            .AND. (PDIFF(LP,LIF).LT.PSUM(LP,LIF))) THEN
                  TSYS = TC * PSUM(LP,LIF) / 2.0 / PDIFF(LP,LIF)
                  LL = (LP - 1) * 3 + 1
                  IF (PDIFF(LP,LIF).LT.DDMIN(LL,LIF,ANTNO))
     *               DDMIN(LL,LIF,ANTNO) = PDIFF(LP,LIF)
                  IF (PDIFF(LP,LIF).GT.DDMAX(LL,LIF,ANTNO))
     *               DDMAX(LL,LIF,ANTNO) = PDIFF(LP,LIF)
                  DDSUM(LL,LIF,ANTNO) = DDSUM(LL,LIF,ANTNO) +
     *               PDIFF(LP,LIF)
                  DDSUMS(LL,LIF,ANTNO) = DDSUMS(LL,LIF,ANTNO) +
     *               PDIFF(LP,LIF) * PDIFF(LP,LIF)
                  DDCNT(LL,LIF,ANTNO) = DDCNT(LL,LIF,ANTNO) + 1.0D0
                  LL = LL + 1
                  IF (PSUM(LP,LIF).LT.DDMIN(LL,LIF,ANTNO))
     *               DDMIN(LL,LIF,ANTNO) = PSUM(LP,LIF)
                  IF (PSUM(LP,LIF).GT.DDMAX(LL,LIF,ANTNO))
     *               DDMAX(LL,LIF,ANTNO) = PSUM(LP,LIF)
                  DDSUM(LL,LIF,ANTNO) = DDSUM(LL,LIF,ANTNO) +
     *               PSUM(LP,LIF)
                  DDSUMS(LL,LIF,ANTNO) = DDSUMS(LL,LIF,ANTNO) +
     *               PSUM(LP,LIF) * PSUM(LP,LIF)
                  DDCNT(LL,LIF,ANTNO) = DDCNT(LL,LIF,ANTNO) + 1.0D0
                  LL = LL + 1
                  IF (TSYS.LT.DDMIN(LL,LIF,ANTNO))
     *               DDMIN(LL,LIF,ANTNO) = TSYS
                  IF (TSYS.GT.DDMAX(LL,LIF,ANTNO))
     *               DDMAX(LL,LIF,ANTNO) = TSYS
                  DDSUM(LL,LIF,ANTNO) = DDSUM(LL,LIF,ANTNO) +
     *               TSYS
                  DDSUMS(LL,LIF,ANTNO) = DDSUMS(LL,LIF,ANTNO) +
     *               TSYS * TSYS
                  DDCNT(LL,LIF,ANTNO) = DDCNT(LL,LIF,ANTNO) + 1.0D0
                  DDNIF = MAX (DDNIF, LIF)
                  DDNANT = MAX (DDNANT, ANTNO)
                  DDNPOL = MAX (DDNPOL, LP)
                  IRNO = IRNO + 1.0D0
                  END IF
 20            CONTINUE
 30         CONTINUE
 50      CONTINUE
      CALL TABIO ('CLOS', 0, ISYRNO, BUFFI, BUFFI, IREC)
      WRITE (MSGTXT,1050) IRNO, INVER
      CALL MSGWRT (3)
      GO TO 999
C
 980  CALL MSGWRT (8)
      CALL TABIO ('CLOS', 0, ISYRNO, BUFFI, BUFFI, IREC)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PRTSYD: ERROR',I4,1X,A)
 1050 FORMAT ('Used',F12.0,' samples (IF/pol/times) from SY version',I4)
      END
      SUBROUTINE PRTSYP (IRET)
C-----------------------------------------------------------------------
C   PRTSYP displays statistics of the SY table
C   Outputs:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER PTYPE(3)*4, TITL1*90, TITL2*90, LINE*90, CSCR*90
      INTEGER   LA, LI, LL, I, NC, IREC, NACROS, PAGE, IP, PT1, PT2
      DOUBLE PRECISION TAMIN(6), TAMAX(6), TASUM(6), TASUMS(6),
     *   TACNT(6), DMIN, DMAX, DAVG, DRMS, DAMIN(6,MAXANT),
     *   DAMAX(6,MAXANT), DASUM(6,MAXANT), DASUMS(6,MAXANT),
     *   DACNT(6,MAXANT)
      INCLUDE 'PRTSY.INC'
      INCLUDE 'PRTSYUE.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA PTYPE /'Pdif', 'Psum', 'Tsys'/
C-----------------------------------------------------------------------
      IREC = 6 * MAXANT
      CALL DFILL (IREC, 1.0D10, DAMIN)
      CALL DFILL (IREC, -1.0D10, DAMAX)
      CALL DFILL (IREC, 0.0, DASUM)
      CALL DFILL (IREC, 0.0, DASUMS)
      CALL DFILL (IREC, 0.0, DACNT)
      IREC = 6
      CALL DFILL (IREC, 1.0D10, TAMIN)
      CALL DFILL (IREC, -1.0D10, TAMAX)
      CALL DFILL (IREC, 0.0, TASUM)
      CALL DFILL (IREC, 0.0, TASUMS)
      CALL DFILL (IREC, 0.0, TACNT)
C                                       antenna based values
      DO 30 LA = 1,DDNANT
         DO 20 LI = 1,DDNIF
            DO 10 LL = 1,6
               IF (DDCNT(LL,LI,LA).GT.0.0D0) THEN
                  DACNT(LL,LA) = DACNT(LL,LA) + DDCNT(LL,LI,LA)
                  DASUM(LL,LA) = DASUM(LL,LA) + DDSUM(LL,LI,LA)
                  DASUMS(LL,LA) = DASUMS(LL,LA) + DDSUMS(LL,LI,LA)
                  DAMIN(LL,LA) = MIN (DAMIN(LL,LA), DDMIN(LL,LI,LA))
                  DAMAX(LL,LA) = MAX (DAMAX(LL,LA), DDMAX(LL,LI,LA))
C                                       overall values
                  TACNT(LL) = TACNT(LL) + DDCNT(LL,LI,LA)
                  TASUM(LL) = TASUM(LL) + DDSUM(LL,LI,LA)
                  TASUMS(LL) = TASUMS(LL) + DDSUMS(LL,LI,LA)
                  TAMIN(LL) = MIN (TAMIN(LL), DDMIN(LL,LI,LA))
                  TAMAX(LL) = MAX (TAMAX(LL), DDMAX(LL,LI,LA))
                  END IF
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE
C                                       init printer
C                                       Open output device
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, LUNP, FINDP, NACROS, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         CALL MSGWRT (8)
         GO TO 999
         END IF
      NACROS = 82
      PAGE = 0
      IF (DTYPE.LE.0) THEN
         PT1 = 1
         PT2 = 3
      ELSE
         PT1 = DTYPE
         PT2 = DTYPE
         END IF
      DO 200 I = PT1,PT2
         IPCNT = 998
         TITL1 = '   ****  Printing ' // PTYPE(I) //
     *      ' by antenna and IF  ****'
         TITL2 = 'Ant IF' // '    avg    rms    min    max      num'
         IF (DDNPOL.EQ.2) TITL2(46:) =
     *      '    avg    rms    min    max      num'
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, IPCNT, PAGE, CSCR, IRET)
         IF (IRET.NE.0) GO TO 960
         DO 130 LA = 1,DDNANT
            DO 120 LI = 1,DDNIF
               WRITE (LINE,1100) LA, LI
               IP = 7
               DO 110 LL = I,I+3,3
                  NC = DDCNT(LL,LI,LA) + 0.1D0
                  IF (NC.LE.0) THEN
                     WRITE (LINE(IP:),1105) NC
                  ELSE
                     DMIN = DDMIN(LL,LI,LA)
                     DMAX = DDMAX(LL,LI,LA)
                     DAVG = DDSUM(LL,LI,LA) / DDCNT(LL,LI,LA)
                     DRMS = DDSUMS(LL,LI,LA) / DDCNT(LL,LI,LA) -
     *                  DAVG * DAVG
                     DRMS = SQRT (MAX (0.0D0, DRMS))
                     IF (I.EQ.1) THEN
                        WRITE (LINE(IP:),1111) DAVG, DRMS, DMIN, DMAX,
     *                     NC
                     ELSE IF (I.EQ.2) THEN
                        WRITE (LINE(IP:),1112) DAVG, DRMS, DMIN, DMAX,
     *                     NC
                     ELSE
                        WRITE (LINE(IP:),1113) DAVG, DRMS, DMIN, DMAX,
     *                     NC
                        END IF
                     END IF
                  IP = IP + 39
 110              CONTINUE
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, CSCR, IRET)
               IF (IRET.NE.0) GO TO 960
 120           CONTINUE
 130        CONTINUE
         IPCNT = 998
         TITL1 = '   ****  Printing ' // PTYPE(I) //
     *      ' by antenna only  ****'
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, IPCNT, PAGE, CSCR, IRET)
         IF (IRET.NE.0) GO TO 960
         DO 150 LA = 1,DDNANT
            WRITE (LINE,1100) LA
            IP = 7
            DO 140 LL = I,I+3,3
               NC = DACNT(LL,LA) + 0.1D0
               IF (NC.LE.0) THEN
                  WRITE (LINE(IP:),1105) NC
               ELSE
                  DMIN = DAMIN(LL,LA)
                  DMAX = DAMAX(LL,LA)
                  DAVG = DASUM(LL,LA) / DACNT(LL,LA)
                  DRMS = DASUMS(LL,LA) / DACNT(LL,LA) - DAVG * DAVG
                  DRMS = SQRT (MAX (0.0D0, DRMS))
                  IF (I.EQ.1) THEN
                     WRITE (LINE(IP:),1111) DAVG, DRMS, DMIN, DMAX,
     *                  NC
                  ELSE IF (I.EQ.2) THEN
                     WRITE (LINE(IP:),1112) DAVG, DRMS, DMIN, DMAX,
     *                  NC
                  ELSE
                     WRITE (LINE(IP:),1113) DAVG, DRMS, DMIN, DMAX,
     *                  NC
                     END IF
                  END IF
               IP = IP + 39
 140           CONTINUE
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *         LINE, IPCNT, PAGE, CSCR, IRET)
            IF (IRET.NE.0) GO TO 960
 150        CONTINUE
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, CSCR, IRET)
         IF (IRET.NE.0) GO TO 960
         LINE = 'AllAnt'
         IP = 7
         DO 160 LL = I,I+3,3
            NC = TACNT(LL) + 0.1D0
            IF (NC.LE.0) THEN
               WRITE (LINE(IP:),1105) NC
            ELSE
               DMIN = TAMIN(LL)
               DMAX = TAMAX(LL)
               DAVG = TASUM(LL) / TACNT(LL)
               DRMS = TASUMS(LL) / TACNT(LL) - DAVG * DAVG
               DRMS = SQRT (MAX (0.0D0, DRMS))
               IF (I.EQ.1) THEN
                  WRITE (LINE(IP:),1111) DAVG, DRMS, DMIN, DMAX, NC
               ELSE IF (I.EQ.2) THEN
                  WRITE (LINE(IP:),1112) DAVG, DRMS, DMIN, DMAX, NC
               ELSE
                  WRITE (LINE(IP:),1113) DAVG, DRMS, DMIN, DMAX, NC
                  END IF
               END IF
            IP = IP + 39
 160        CONTINUE
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, CSCR, IRET)
         IF (IRET.NE.0) GO TO 960
 200     CONTINUE
C
 960  IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1960) IRET
         CALL MSGWRT (8)
      ELSE
         IRET = 0
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('ERROR',I4,' OPENING THE PRINTER DEVICE/FILE')
 1100 FORMAT (2I3)
 1105 FORMAT (28X,I9)
 1111 FORMAT (4F7.3,I9)
 1112 FORMAT (4F7.2,I9)
 1113 FORMAT (4F7.1,I9)
 1960 FORMAT ('ERROR',I5,' FROM PRINTER CODE')
      END
      SUBROUTINE PRTSYA (NP, NIF, NSC, NA, SYDATA, SYAVG, IRET)
C-----------------------------------------------------------------------
C   PRTSYA does the robust averaging of the SY data on a scan basis or
C   over all included data
C   Inputs:
C      NP       I      Number polarizations
C      NIF      I      Number IFs
C      NSC      I      Number scans
C      NA       I      Number antennas
C   Outputs:
C      SYDATA   R(*)   Work area to do averaging
C      SYAVG    R(*)   Resultant averages per scan
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   NP, NIF, NSC, NA, IRET
      REAL      SYDATA(2,NP,NIF,*), SYAVG(6,NP,NIF,NA,*)
C
      INTEGER   NITER, MAXSAM, MAXT
      PARAMETER (NITER=9, MAXSAM=10000, MAXT=25000)
      INCLUDE 'PRTSY.INC'
      INCLUDE 'REPSYTAB.INC'
      INCLUDE 'PRTSYUE.INC'
      INTEGER   IANT, SOURID, ANTNO, SUBA, FREQID, SORCS(MAXT), ISCN,
     *   J, IREC1, IREC2, NREC, JP, JIF, I, IREC, NV, IR, IROUND, CALTYP
      LOGICAL   WANT
      REAL      TIMEI, TB, TE, VD, VS, MEDIAN, TT, SD, SS, ST,
     *   DVD(MAXSAM), DVS(MAXSAM), DVT(MAXSAM)
      DOUBLE PRECISION TIME, TIMES(MAXT)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
      I = 6 * NP * NIF *  NA * NSC
      CALL RFILL (I, FBLANK, SYAVG)
      ISYRNO = 1
      DO 100 IANT = 1,NA
         NREC = 0
         IF (NRECAN(IANT).GT.0) THEN
            DO 30 I = 1,NRECAN(IANT)
               CALL TABSY ('READ', BUFFI, ISYRNO, SYKOLS, SYNUMV,
     *            NUMPOL, NUMIF, TIME, TIMEI, CALTYP, SOURID, ANTNO,
     *            SUBA, FREQID, PDIFF, PSUM, PGAIN, IRET)
               IF (IRET.GT.0) THEN
                  WRITE (MSGTXT,1000) IRET,
     *               'READING THE SORTED SY TABLE'
                  GO TO 990
               ELSE IF (IRET.LT.0) THEN
                  GO TO 30
               ELSE IF (ANTNO.NE.IANT) THEN
                  MSGTXT = 'TABLE READING IS OFF'
                  IRET = 99
                  GO TO 990
                  END IF
C                                       include?
               WANT = (TIME.GE.BTIME) .AND. (TIME.LE.ETIME)
               IF ((SOURID.GT.0) .AND. (NSOUWD.GT.0)) THEN
                  WANT = .NOT.DOSWNT
                  DO 10 J = 1,NSOUWD
                     IF (SOUWAN(J).EQ.SOURID) WANT = DOSWNT
 10                  CONTINUE
                  END IF
               IF ((SUBA.GT.0) .AND. (SUBARR.GT.0) .AND.
     *            (SUBA.NE.SUBARR)) WANT = .FALSE.
               IF ((FREQID.GT.0) .AND. (FRQSEL.GT.0) .AND.
     *            (FRQSEL.NE.FREQID)) WANT = .FALSE.
               IF ((WANT) .AND. (NREC.LT.MAXT)) THEN
                  NREC = NREC + 1
                  TIMES(NREC) = TIME
                  SORCS(NREC) = SOURID
                  DO 20 JIF = 1,NIF
                     DO 15 JP = 1,NP
                        SYDATA(1,JP,JIF,NREC) = PDIFF(JP,JIF)
                        SYDATA(2,JP,JIF,NREC) = PSUM(JP,JIF)
 15                     CONTINUE
 20                  CONTINUE
                  END IF
 30            CONTINUE
            END IF
         IF (NREC.GT.0) THEN
            DO 80 ISCN = 1,NSC-1
C                                       find range
               TB = TSCAN(ISCN)
               TE = TSCAN(ISCN+1)
               IREC2 = NREC
               IREC1 = 1
               DO 35 J = 1,NREC
                  IF (TIMES(J).LE.TB) THEN
                     IREC1 = J
                  ELSE
                     IF (((SORCS(J).NE.SSCAN(ISCN)) .AND.
     *                  (SSCAN(ISCN).GT.0)) .OR. (TIMES(J).GT.TE)) THEN
                        IREC2 = J - 1
                        GO TO 40
                        END IF
                     END IF
 35               CONTINUE
 40            DO 70 JIF = 1,NIF
                  DO 60 JP = 1,NP
                     NV = 0
                     DO 45 IREC = IREC1,IREC2
                        IF (SORCS(IREC).EQ.SSCAN(ISCN)) THEN
                           VD = SYDATA(1,JP,JIF,IREC)
                           VS = SYDATA(2,JP,JIF,IREC)
                           IF ((VD.NE.FBLANK) .AND. (VS.NE.FBLANK)) THEN
                              IF (CALTYP.EQ.1) THEN
                                 TT = TCAL(JP+2,JIF,IANT)
                              ELSE
                                 TT = TCAL(JP,JIF,IANT)
                                 END IF
                              IF (TT.EQ.FBLANK) TT = 1.0
                              IF ((VD.GT.0.0) .AND. (VD.LT.VS) .AND.
     *                           (NV.LT.MAXSAM)) THEN
                                 NV = NV + 1
                                 DVD(NV) = VD
                                 DVS(NV) = VS
                                 DVT(NV) = TT * VS / (2.0 * VD)
                                 END IF
                              END IF
                           END IF
 45                     CONTINUE
                     IF (NV.LE.0) THEN
                        SYAVG(1,JP,JIF,IANT,ISCN) = FBLANK
                        SYAVG(2,JP,JIF,IANT,ISCN) = FBLANK
                        SYAVG(3,JP,JIF,IANT,ISCN) = FBLANK
                        SYAVG(4,JP,JIF,IANT,ISCN) = FBLANK
                        SYAVG(5,JP,JIF,IANT,ISCN) = FBLANK
                        SYAVG(6,JP,JIF,IANT,ISCN) = FBLANK
                     ELSE
                        SYAVG(1,JP,JIF,IANT,ISCN) = MEDIAN (NV, DVD)
                        SYAVG(2,JP,JIF,IANT,ISCN) = MEDIAN (NV, DVS)
                        SYAVG(3,JP,JIF,IANT,ISCN) = MEDIAN (NV, DVT)
                        VD = SYAVG(1,JP,JIF,IANT,ISCN)
                        VS = SYAVG(2,JP,JIF,IANT,ISCN)
                        TT = SYAVG(3,JP,JIF,IANT,ISCN)
                        SD = 0.0
                        SS = 0.0
                        ST = 0.0
                        DO 50 J = 1,NV
                           SD = SD + (DVD(J) - VD) * (DVD(J) - VD)
                           SS = SS + (DVS(J) - VS) * (DVS(J) - VS)
                           ST = ST + (DVT(J) - TT) * (DVT(J) - TT)
 50                        CONTINUE
                        SYAVG(4,JP,JIF,IANT,ISCN) = SQRT (SD/NV)
                        SYAVG(5,JP,JIF,IANT,ISCN) = SQRT (SS/NV)
                        SYAVG(6,JP,JIF,IANT,ISCN) = SQRT (ST/NV)
                        END IF
 60                  CONTINUE
 70               CONTINUE
 80            CONTINUE
            END IF
 100     CONTINUE
C                                       normalize: by IF
      IR = IROUND (FACTOR(4))
      IF ((IR.GT.0) .AND. (IR.LE.NIF)) THEN
         DO 140 ISCN = 1,NSC-1
            DO 135 IANT = 1,NA
               DO 130 JP = 1,NP
                  DO 125 J = 1,3
                     TT = SYAVG(J,JP,IR,IANT,ISCN)
                     DO 124 JIF = 1,NIF
                        VD = SYAVG(J,JP,JIF,IANT,ISCN)
                        VS = SYAVG(J+3,JP,JIF,IANT,ISCN)
                        IF ((VD.NE.FBLANK) .AND. (TT.NE.FBLANK) .AND.
     *                     (TT.GT.0.0)) THEN
                           SYAVG(J,JP,JIF,IANT,ISCN) = VD / TT
                           SYAVG(J+3,JP,JIF,IANT,ISCN) = VS / TT
                        ELSE
                           SYAVG(J,JP,JIF,IANT,ISCN) = FBLANK
                           SYAVG(J+3,JP,JIF,IANT,ISCN) = FBLANK
                           END IF
 124                    CONTINUE
 125                 CONTINUE
 130              CONTINUE
 135           CONTINUE
 140        CONTINUE
         END IF
C                                       normalize: by antenna
      IR = IROUND (FACTOR(5))
      IF ((IR.GT.0) .AND. (IR.LE.NA) .AND. (ARECAN(IR).GT.0)) THEN
         DO 160 ISCN = 1,NSC-1
            DO 155 JIF = 1,NIF
               DO 150 JP = 1,NP
                  DO 145 J = 1,3
                     TT = SYAVG(J,JP,JIF,IR,ISCN)
                     DO 144 IANT = 1,NA
                        VD = SYAVG(J,JP,JIF,IANT,ISCN)
                        VS = SYAVG(J+3,JP,JIF,IANT,ISCN)
                        IF ((VD.NE.FBLANK) .AND. (TT.NE.FBLANK) .AND.
     *                     (TT.GT.0.0)) THEN
                           SYAVG(J,JP,JIF,IANT,ISCN) = VD / TT
                           SYAVG(J+3,JP,JIF,IANT,ISCN) = VS / TT
                        ELSE
                           SYAVG(J,JP,JIF,IANT,ISCN) = FBLANK
                           SYAVG(J+3,JP,JIF,IANT,ISCN) = FBLANK
                           END IF
 144                    CONTINUE
 145                 CONTINUE
 150              CONTINUE
 155           CONTINUE
 160        CONTINUE
         END IF
C                                       normalize: by SCAN
      IR = IROUND (FACTOR(6))
      IF ((IR.GT.0) .AND. (IR.LE.NSC-1)) THEN
         DO 180 IANT = 1,NA
            DO 175 JIF = 1,NIF
               DO 170 JP = 1,NP
                  DO 165 J = 1,3
                     TT = SYAVG(J,JP,JIF,IANT,IR)
                     DO 164 ISCN = 1,NSC-1
                        VD = SYAVG(J,JP,JIF,IANT,ISCN)
                        VS = SYAVG(J+3,JP,JIF,IANT,ISCN)
                        IF ((VD.NE.FBLANK) .AND. (TT.NE.FBLANK) .AND.
     *                     (TT.GT.0.0)) THEN
                           SYAVG(J,JP,JIF,IANT,ISCN) = VD / TT
                           SYAVG(J+3,JP,JIF,IANT,ISCN) = VS / TT
                        ELSE
                           SYAVG(J,JP,JIF,IANT,ISCN) = FBLANK
                           SYAVG(J+3,JP,JIF,IANT,ISCN) = FBLANK
                           END IF
 164                    CONTINUE
 165                 CONTINUE
 170              CONTINUE
 175           CONTINUE
 180        CONTINUE
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PRTSYA: ERROR',I4,' ON ',A)
      END
      SUBROUTINE PRTSY1 (NP, NIF, NSC, NA, SYAVG, IRET)
C-----------------------------------------------------------------------
C   PRTSY1 does the display of the data: by IF, ant across, scan down
C   Inputs:
C      NP       I      Number polarizations
C      NIF      I      Number IFs
C      NSC      I      Number scans
C      NA       I      Number antennas
C      SYAVG    R(*)   Resultant averages per scan
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   NP, NIF, NSC, NA, IRET
      REAL      SYAVG(6,NP,NIF,NA,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   JP, JIF, NACROS, PAGE, NANT, MXS, K, ANTS(MAXANT),
     *   JTRIM, I, J, LANT, LA1, LA2, ITT(4), IROUND, IP, JO, ICOUNT,
     *   CRTLIM
      CHARACTER TITL1*132, TITL2*132, LINE*132, SRC*16, CSCR*132,
     *   POLS(4)*1
      LOGICAL  WANT
      REAL      DIFMAX, SUMMAX, SYSMAX, T, U, V, FACT, SCL
      INCLUDE 'REPSYTAB.INC'
      INCLUDE 'PRTSY.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA POLS /'R', 'L', 'X', 'Y'/
C-----------------------------------------------------------------------
      CRTLIM = ABS (CRTMAX) - 4
C                                       which antennas
      NANT = 0
      DO 10 I = 1,NA
         IF (ARECAN(I).GT.0) THEN
            NANT = NANT + 1
            ANTS(NANT) = I
            END IF
 10      CONTINUE
C                                       length of source name
      MXS = 0
      DO 20 I = 1,1000
         J = JTRIM (SRCS(I))
         MXS = MAX (MXS, J)
 20      CONTINUE
C                                       init printer
C                                       Open output device
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, LUNP, FINDP, NACROS, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING THE PRINTER'
         GO TO 990
         END IF
      PAGE = 0
      LANT = (NACROS - MXS - 10) / 4
C                                       find scaling
      JO = 0
      IF (DTYPE.GT.3) JO = 3
      DIFMAX = 0.0
      SUMMAX = 0.0
      SYSMAX = 0.0
      DO 40 J = 1,NSC-1
         DO 35 I = 1,NANT
            K = ANTS(I)
            DO 30 JIF = 1,NIF
               DO 25 JP = 1,NP
                  U = SYAVG(1+JO,JP,JIF,K,J)
                  V = SYAVG(2+JO,JP,JIF,K,J)
                  T = SYAVG(3+JO,JP,JIF,K,J)
                  IF (U.NE.FBLANK) DIFMAX = MAX (DIFMAX, U)
                  IF (V.NE.FBLANK) SUMMAX = MAX (SUMMAX, V)
                  IF (T.NE.FBLANK) SYSMAX = MAX (SYSMAX, T)
 25               CONTINUE
 30            CONTINUE
 35         CONTINUE
 40      CONTINUE
      IF (DIFMAX.EQ.0.0) DIFMAX = 1.0
      IF (SUMMAX.EQ.0.0) SUMMAX = 1.0
      IF (SYSMAX.EQ.0.0) SYSMAX = 1.0
      I = LOG10 (DIFMAX) + 6.0
      J = LOG10 (SUMMAX) + 6.0
      K = LOG10 (SYSMAX) + 6.0
      DIFMAX = 10.0 ** (8-I)
      SUMMAX = 10.0 ** (8-J)
      SYSMAX = 10.0 ** (8-K)
C                                       PDIFF
      IF ((DTYPE.NE.2) .AND. (DTYPE.NE.3) .AND. (DTYPE.NE.5) .AND.
     *   (DTYPE.NE.6)) THEN
         FACT = FACTOR(1)
         IF (FACT.LE.0.0) FACT = 1
         DIFMAX = DIFMAX * FACT
         IPCNT = 998
         ICOUNT = 0
         DO 70 JP = 1,NP
            DO 65 JIF = 1,NIF
               IF (((DOCRT.LE.0) .AND. (IPCNT.GT.PRTMAX/2)) .OR.
     *            ((DOCRT.GT.0) .AND. (IPCNT.GT.CRTLIM))) THEN
                  IPCNT = 998
                  ICOUNT = 0
                  END IF
               LA2 = 0
 50            LA1 = LA2 + 1
               LA2 = MIN (LA1 + LANT - 1, NANT)
               SCL = 1000.0 / DIFMAX
               IF ((SCL.LT.1010.0) .AND. (SCL.GT.0.01)) THEN
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1050) 'Pdif', POLS(JP+JP0), JIF, SCL
                  ELSE
                     WRITE (TITL1,1050) 'Pdif RMS', POLS(JP+JP0), JIF,
     *                  SCL
                     END IF
               ELSE
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1051) 'Pdif', POLS(JP+JP0), JIF, SCL
                  ELSE
                     WRITE (TITL1,1051) 'Pdif RMS', POLS(JP+JP0), JIF,
     *                  SCL
                     END IF
                  END IF
               IP = 10 + MXS
               WRITE (TITL2,1055)
               WRITE (TITL2(IP:),1056) (ANTS(I), I = LA1,LA2)
               LINE = ' '
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, CSCR, IRET)
               IF (IRET.NE.0) GO TO 960
               IF (ICOUNT.NE.0) THEN
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL1, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL2, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  END IF
               ICOUNT = ICOUNT + 1
               DO 60 J = 1,NSC-1
                  WANT = .TRUE.
                  IF ((SSCAN(J).GT.0) .AND. (NSOUWD.GT.0)) THEN
                     WANT = .NOT.DOSWNT
                     DO 51 K = 1,NSOUWD
                        IF (SOUWAN(K).EQ.SSCAN(J)) WANT = DOSWNT
 51                     CONTINUE
                     END IF
                  IF (WANT) THEN
                     SRC = SRCS(SSCAN(J))
                     T = TTSCAN(J)
                     CALL TODHMS (T, ITT)
                     WRITE (LINE,1060) ITT(2), ITT(3), ITT(4), SRC(:MXS)
                     IP = 10 + MXS
                     DO 55 I = LA1,LA2
                        V = SYAVG(1+JO,JP,JIF,ANTS(I),J)
                        IF (V.NE.FBLANK) THEN
                           K = IROUND (V * DIFMAX)
                           WRITE (LINE(IP:),1061) K
                        ELSE
                           LINE(IP:) = ' ---'
                           END IF
                        IP = IP + 4
 55                     CONTINUE
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, CSCR, IRET)
                     IF (IRET.NE.0) GO TO 960
                     ICOUNT = ICOUNT + 1
                     END IF
 60               CONTINUE
               IF (LA2.LT.NANT) GO TO 50
 65            CONTINUE
 70         CONTINUE
         END IF
C                                       PSUM
      IF ((DTYPE.NE.1) .AND. (DTYPE.NE.3) .AND. (DTYPE.NE.4) .AND.
     *   (DTYPE.NE.6)) THEN
         FACT = FACTOR(2)
         IF (FACT.LE.0.0) FACT = 1
         SUMMAX = SUMMAX * FACT
         IPCNT = 998
         ICOUNT = 0
         DO 170 JP = 1,NP
            DO 165 JIF = 1,NIF
               IF (((DOCRT.LE.0) .AND. (IPCNT.GT.PRTMAX/2)) .OR.
     *            ((DOCRT.GT.0) .AND. (IPCNT.GT.CRTLIM))) THEN
                  IPCNT = 998
                  ICOUNT = 0
                  END IF
               LA2 = 0
 150           LA1 = LA2 + 1
               LA2 = MIN (LA1 + LANT - 1, NANT)
               IP = 10 + MXS
               SCL = 1000.0 / SUMMAX
               IF ((SCL.LT.1010.0) .AND. (SCL.GT.0.01)) THEN
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1050) 'Psum', POLS(JP+JP0), JIF, SCL
                  ELSE
                     WRITE (TITL1,1050) 'Psum RMS', POLS(JP+JP0), JIF,
     *                  SCL
                     END IF
               ELSE
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1051) 'Psum', POLS(JP+JP0), JIF, SCL
                  ELSE
                     WRITE (TITL1,1051) 'Psum RMS', POLS(JP+JP0), JIF,
     *                  SCL
                     END IF
                  END IF
               WRITE (TITL2,1055)
               WRITE (TITL2(IP:),1056) (ANTS(I), I = LA1,LA2)
               LINE = ' '
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, CSCR, IRET)
               IF (IRET.NE.0) GO TO 960
               IF (ICOUNT.NE.0) THEN
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL1, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL2, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  END IF
               ICOUNT = ICOUNT + 1
               DO 160 J = 1,NSC-1
                  WANT = .TRUE.
                  IF ((SSCAN(J).GT.0) .AND. (NSOUWD.GT.0)) THEN
                     WANT = .NOT.DOSWNT
                     DO 151 K = 1,NSOUWD
                        IF (SOUWAN(K).EQ.SSCAN(J)) WANT = DOSWNT
 151                    CONTINUE
                     END IF
                  IF (WANT) THEN
                     SRC = SRCS(SSCAN(J))
                     T = TTSCAN(J)
                     CALL TODHMS (T, ITT)
                     WRITE (LINE,1060) ITT(2), ITT(3), ITT(4), SRC(:MXS)
                     IP = 10 + MXS
                     DO 155 I = LA1,LA2
                        V = SYAVG(2+JO,JP,JIF,ANTS(I),J)
                        IF (V.NE.FBLANK) THEN
                           K = IROUND (V * SUMMAX)
                           WRITE (LINE(IP:),1061) K
                        ELSE
                           LINE(IP:) = ' ---'
                           END IF
                        IP = IP + 4
 155                    CONTINUE
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, CSCR, IRET)
                     IF (IRET.NE.0) GO TO 960
                     ICOUNT = ICOUNT + 1
                     END IF
 160              CONTINUE
               IF (LA2.LT.NANT) GO TO 150
 165           CONTINUE
 170        CONTINUE
         END IF
C                                       PSYS
      IF ((DTYPE.NE.1) .AND. (DTYPE.NE.2) .AND. (DTYPE.NE.4) .AND.
     *   (DTYPE.NE.5)) THEN
         FACT = FACTOR(3)
         IF (FACT.LE.0.0) FACT = 1
         SYSMAX = SYSMAX * FACT
         IPCNT = 998
         ICOUNT = 0
         DO 270 JP = 1,NP
            DO 265 JIF = 1,NIF
               IF (((DOCRT.LE.0) .AND. (IPCNT.GT.PRTMAX/2)) .OR.
     *            ((DOCRT.GT.0) .AND. (IPCNT.GT.CRTLIM))) THEN
                  IPCNT = 998
                  ICOUNT = 0
                  END IF
               LA2 = 0
 250           LA1 = LA2 + 1
               LA2 = MIN (LA1 + LANT - 1, NANT)
               IP = 10 + MXS
               SCL = 1000.0 / SYSMAX
               IF ((SCL.LT.1010.0) .AND. (SCL.GT.0.01)) THEN
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1050) 'Psys', POLS(JP+JP0), JIF, SCL
                  ELSE
                     WRITE (TITL1,1050) 'Psys RMS', POLS(JP+JP0), JIF,
     *                  SCL
                     END IF
               ELSE
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1051) 'Psys', POLS(JP+JP0), JIF, SCL
                  ELSE
                     WRITE (TITL1,1051) 'Psys RMS', POLS(JP+JP0), JIF,
     *                  SCL
                     END IF
                  END IF
               WRITE (TITL2,1055)
               WRITE (TITL2(IP:),1056) (ANTS(I), I = LA1,LA2)
               LINE = ' '
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, CSCR, IRET)
               IF (IRET.NE.0) GO TO 960
               IF (ICOUNT.NE.0) THEN
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL1, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL2, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  END IF
               ICOUNT = ICOUNT + 1
               DO 260 J = 1,NSC-1
                  WANT = .TRUE.
                  IF ((SSCAN(J).GT.0) .AND. (NSOUWD.GT.0)) THEN
                     WANT = .NOT.DOSWNT
                     DO 251 K = 1,NSOUWD
                        IF (SOUWAN(K).EQ.SSCAN(J)) WANT = DOSWNT
 251                    CONTINUE
                     END IF
                  IF (WANT) THEN
                     SRC = SRCS(SSCAN(J))
                     T = TTSCAN(J)
                     CALL TODHMS (T, ITT)
                     WRITE (LINE,1060) ITT(2), ITT(3), ITT(4), SRC(:MXS)
                     IP = 10 + MXS
                     DO 255 I = LA1,LA2
                        T = SYAVG(3+JO,JP,JIF,ANTS(I),J)
                        IF (T.NE.FBLANK) THEN
                           K = IROUND (T * SYSMAX)
                           WRITE (LINE(IP:),1061) K
                        ELSE
                           LINE(IP:) = ' ---'
                           END IF
                        IP = IP + 4
 255                    CONTINUE
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, CSCR, IRET)
                     IF (IRET.NE.0) GO TO 960
                     ICOUNT = ICOUNT + 1
                     END IF
 260              CONTINUE
               IF (LA2.LT.NANT) GO TO 250
 265           CONTINUE
 270        CONTINUE
         END IF
C
 960  IF (IRET.LT.0) THEN
         MSGTXT = 'Stopping at your request'
         CALL MSGWRT (6)
         IRET = 0
      ELSE IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'PRINTER ERROR'
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PRTSY1: ERROR',I4,' ON ',A)
 1050 FORMAT ('*******  Printing ',A,' (',A,') for IF',I3,
     *   '   1000 =',F10.4,'  *******')
 1051 FORMAT ('*******  Printing ',A,' (',A,') for IF',I3,
     *   '   1000 =',1PE14.6,'  *******')
 1055 FORMAT ('  Time   Source')
 1056 FORMAT (30I4)
 1060 FORMAT (2(I2.2,':'),I2.2,1X,A)
 1061 FORMAT (I4)
      END
      SUBROUTINE PRTSY2 (NP, NIF, NSC, NA, SYAVG, IRET)
C-----------------------------------------------------------------------
C   PRTSY2 does the display of the data: by ant, IF across, scan down
C   Inputs:
C      NP       I      Number polarizations
C      NIF      I      Number IFs
C      NSC      I      Number scans
C      NA       I      Number antennas
C      SYAVG    R(*)   Resultant averages per scan
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   NP, NIF, NSC, NA, IRET
      REAL      SYAVG(6,NP,NIF,NA,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   JP, JIF, NACROS, PAGE, NANT, MXS, K, ANTS(MAXANT),
     *   JTRIM, I, J, LIF, LI1, LI2, ITT(4), IROUND, IP, JO, ICOUNT,
     *   CRTLIM
      CHARACTER TITL1*132, TITL2*132, LINE*132, SRC*16, CSCR*132,
     *   POLS(4)*1
      LOGICAL   WANT
      REAL      DIFMAX, SUMMAX, SYSMAX, T, U, V, FACT, SCL
      INCLUDE 'REPSYTAB.INC'
      INCLUDE 'PRTSY.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA POLS /'R', 'L', 'X', 'Y'/
C-----------------------------------------------------------------------
      CRTLIM = ABS (CRTMAX) - 4
C                                       which antennas
      NANT = 0
      DO 10 I = 1,NA
         IF (ARECAN(I).GT.0) THEN
            NANT = NANT + 1
            ANTS(NANT) = I
            END IF
 10      CONTINUE
C                                       length of source name
      MXS = 0
      DO 20 I = 1,1000
         J = JTRIM (SRCS(I))
         MXS = MAX (MXS, J)
 20      CONTINUE
C                                       init printer
C                                       Open output device
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, LUNP, FINDP, NACROS, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING THE PRINTER'
         GO TO 990
         END IF
      PAGE = 0
      LIF = (NACROS - MXS - 10) / 5
C                                       find scaling
      DIFMAX = 0.0
      SUMMAX = 0.0
      SYSMAX = 0.0
      JO = 0
      IF (DTYPE.GT.3) JO = 3
      DO 40 J = 1,NSC-1
         DO 35 I = 1,NANT
            K = ANTS(I)
            DO 30 JIF = 1,NIF
               DO 25 JP = 1,NP
                  U = SYAVG(1+JO,JP,JIF,K,J)
                  V = SYAVG(2+JO,JP,JIF,K,J)
                  T = SYAVG(3+JO,JP,JIF,K,J)
                  IF (U.NE.FBLANK) DIFMAX = MAX (DIFMAX, U)
                  IF (V.NE.FBLANK) SUMMAX = MAX (SUMMAX, V)
                  IF (T.NE.FBLANK) SYSMAX = MAX (SYSMAX, T)
 25               CONTINUE
 30            CONTINUE
 35         CONTINUE
 40      CONTINUE
      IF (DIFMAX.EQ.0.0) DIFMAX = 1.0
      IF (SUMMAX.EQ.0.0) SUMMAX = 1.0
      IF (SYSMAX.EQ.0.0) SYSMAX = 1.0
      I = LOG10 (DIFMAX) + 6.0
      J = LOG10 (SUMMAX) + 6.0
      K = LOG10 (SYSMAX) + 6.0
      DIFMAX = 10.0 ** (9-I)
      SUMMAX = 10.0 ** (9-J)
      SYSMAX = 10.0 ** (9-K)
C                                       PDIFF
      IF ((DTYPE.NE.2) .AND. (DTYPE.NE.3) .AND. (DTYPE.NE.5) .AND.
     *   (DTYPE.NE.6)) THEN
         FACT = FACTOR(1)
         IF (FACT.LE.0.0) FACT = 1
         DIFMAX = DIFMAX * FACT
         ICOUNT = 0
         IPCNT = 998
         DO 70 JP = 1,NP
            DO 65 I = 1,NANT
               IF (((DOCRT.LE.0) .AND. (IPCNT.GT.PRTMAX/2)) .OR.
     *            ((DOCRT.GT.0) .AND. (IPCNT.GT.CRTLIM))) THEN
                  IPCNT = 998
                  ICOUNT = 0
                  END IF
               LI2 = 0
 50            LI1 = LI2 + 1
               LI2 = MIN (LI1 + LIF - 1, NIF)
               SCL = 10000.0 / DIFMAX
               IF ((SCL.LT.10100.0) .AND. (SCL.GT.0.01)) THEN
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1050) 'Pdif', POLS(JP+JP0), ANTS(I),
     *                  SCL
                  ELSE
                     WRITE (TITL1,1050) 'Pdif RMS', POLS(JP+JP0),
     *                  ANTS(I), SCL
                     END IF
               ELSE
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1051) 'Pdif', POLS(JP+JP0), ANTS(I),
     *                  SCL
                  ELSE
                     WRITE (TITL1,1051) 'Pdif RMS', POLS(JP+JP0),
     *                  ANTS(I), SCL
                     END IF
                  END IF
               IP = 10 + MXS
               WRITE (TITL2,1055)
               WRITE (TITL2(IP:),1056) (K, K = LI1,LI2)
               LINE = ' '
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, CSCR, IRET)
               IF (IRET.NE.0) GO TO 960
               IF (ICOUNT.NE.0) THEN
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL1, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL2, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  END IF
               ICOUNT = ICOUNT + 1
               DO 60 J = 1,NSC-1
                  WANT = .TRUE.
                  IF ((SSCAN(J).GT.0) .AND. (NSOUWD.GT.0)) THEN
                     WANT = .NOT.DOSWNT
                     DO 51 K = 1,NSOUWD
                        IF (SOUWAN(K).EQ.SSCAN(J)) WANT = DOSWNT
 51                     CONTINUE
                     END IF
                  IF (WANT) THEN
                     SRC = SRCS(SSCAN(J))
                     T = TTSCAN(J)
                     CALL TODHMS (T, ITT)
                     WRITE (LINE,1060) ITT(2), ITT(3), ITT(4), SRC(:MXS)
                     IP = 10 + MXS
                     DO 55 JIF = LI1,LI2
                        V = SYAVG(1+JO,JP,JIF,ANTS(I),J)
                        IF (V.NE.FBLANK) THEN
                           K = IROUND (V * DIFMAX)
                           WRITE (LINE(IP:),1061) K
                        ELSE
                           LINE(IP:) = ' ----'
                           END IF
                        IP = IP + 5
 55                     CONTINUE
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, CSCR, IRET)
                     IF (IRET.NE.0) GO TO 960
                     ICOUNT = ICOUNT + 1
                     END IF
 60               CONTINUE
               IF (LI2.LT.NIF) GO TO 50
 65            CONTINUE
 70         CONTINUE
         END IF
C                                       PSUM
      IF ((DTYPE.NE.1) .AND. (DTYPE.NE.3) .AND. (DTYPE.NE.4) .AND.
     *   (DTYPE.NE.6)) THEN
         FACT = FACTOR(2)
         IF (FACT.LE.0.0) FACT = 1
         SUMMAX = SUMMAX * FACT
         IPCNT = 998
         ICOUNT = 0
         DO 170 JP = 1,NP
            DO 165 I = 1,NANT
               IF (((DOCRT.LE.0) .AND. (IPCNT.GT.PRTMAX/2)) .OR.
     *            ((DOCRT.GT.0) .AND. (IPCNT.GT.CRTLIM))) THEN
                  IPCNT = 998
                  ICOUNT = 0
                  END IF
               LI2 = 0
 150           LI1 = LI2 + 1
               LI2 = MIN (LI1 + LIF - 1, NIF)
               IP = 10 + MXS
               SCL = 10000.0 / SUMMAX
               IF ((SCL.LT.10100.0) .AND. (SCL.GT.0.01)) THEN
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1050) 'Psum', POLS(JP+JP0), ANTS(I),
     *                  SCL
                  ELSE
                     WRITE (TITL1,1050) 'Psum RMS', POLS(JP+JP0),
     *                  ANTS(I), SCL
                     END IF
               ELSE
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1051) 'Psum', POLS(JP+JP0), ANTS(I),
     *                  SCL
                  ELSE
                     WRITE (TITL1,1051) 'Psum RMS', POLS(JP+JP0),
     *                  ANTS(I), SCL
                     END IF
                  END IF
               WRITE (TITL2,1055)
               WRITE (TITL2(IP:),1056) (K, K = LI1,LI2)
               LINE = ' '
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, CSCR, IRET)
               IF (IRET.NE.0) GO TO 960
               IF (ICOUNT.NE.0) THEN
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL1, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL2, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  END IF
               ICOUNT = ICOUNT + 1
               DO 160 J = 1,NSC-1
                  WANT = .TRUE.
                  IF ((SSCAN(J).GT.0) .AND. (NSOUWD.GT.0)) THEN
                     WANT = .NOT.DOSWNT
                     DO 151 K = 1,NSOUWD
                        IF (SOUWAN(K).EQ.SSCAN(J)) WANT = DOSWNT
 151                    CONTINUE
                     END IF
                  IF (WANT) THEN
                     SRC = SRCS(SSCAN(J))
                     T = TTSCAN(J)
                     CALL TODHMS (T, ITT)
                     WRITE (LINE,1060) ITT(2), ITT(3), ITT(4), SRC(:MXS)
                     IP = 10 + MXS
                     DO 155 JIF = LI1,LI2
                        V = SYAVG(2+JO,JP,JIF,ANTS(I),J)
                        IF (V.NE.FBLANK) THEN
                           K = IROUND (V * SUMMAX)
                           WRITE (LINE(IP:),1061) K
                        ELSE
                           LINE(IP:) = ' ----'
                           END IF
                        IP = IP + 5
 155                    CONTINUE
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, CSCR, IRET)
                     IF (IRET.NE.0) GO TO 960
                     ICOUNT = ICOUNT + 1
                     END IF
 160              CONTINUE
               IF (LI2.LT.NIF) GO TO 150
 165           CONTINUE
 170        CONTINUE
         END IF
C                                       PSYS
      IF ((DTYPE.NE.1) .AND. (DTYPE.NE.2) .AND. (DTYPE.NE.4) .AND.
     *   (DTYPE.NE.5)) THEN
         FACT = FACTOR(3)
         IF (FACT.LE.0.0) FACT = 1
         SYSMAX = SYSMAX * FACT
         IPCNT = 998
         ICOUNT = 0
         DO 270 JP = 1,NP
            DO 265 I = 1,NANT
               IF (((DOCRT.LE.0) .AND. (IPCNT.GT.PRTMAX/2)) .OR.
     *            ((DOCRT.GT.0) .AND. (IPCNT.GT.CRTLIM))) THEN
                  IPCNT = 998
                  ICOUNT = 0
                  END IF
               LI2 = 0
 250           LI1 = LI2 + 1
               LI2 = MIN (LI1 + LIF - 1, NIF)
               IP = 10 + MXS
               SCL = 10000.0 / SYSMAX
               IF ((SCL.LT.10100.0) .AND. (SCL.GT.0.01)) THEN
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1050) 'Psys', POLS(JP+JP0), ANTS(I),
     *                  SCL
                  ELSE
                     WRITE (TITL1,1050) 'Psys RMS', POLS(JP+JP0),
     *                  ANTS(I), SCL
                     END IF
               ELSE
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1051) 'Psys', POLS(JP+JP0), ANTS(I),
     *                  SCL
                  ELSE
                     WRITE (TITL1,1051) 'Psys RMS', POLS(JP+JP0),
     *                  ANTS(I), SCL
                     END IF
                  END IF
               WRITE (TITL2,1055)
               WRITE (TITL2(IP:),1056) (K, K = LI1,LI2)
               LINE = ' '
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, CSCR, IRET)
               IF (IRET.NE.0) GO TO 960
               IF (ICOUNT.NE.0) THEN
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL1, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL2, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  END IF
               ICOUNT = ICOUNT + 1
               DO 260 J = 1,NSC-1
                  WANT = .TRUE.
                  IF ((SSCAN(J).GT.0) .AND. (NSOUWD.GT.0)) THEN
                     WANT = .NOT.DOSWNT
                     DO 251 K = 1,NSOUWD
                        IF (SOUWAN(K).EQ.SSCAN(J)) WANT = DOSWNT
 251                    CONTINUE
                     END IF
                  IF (WANT) THEN
                     SRC = SRCS(SSCAN(J))
                     T = TTSCAN(J)
                     CALL TODHMS (T, ITT)
                     WRITE (LINE,1060) ITT(2), ITT(3), ITT(4), SRC(:MXS)
                     IP = 10 + MXS
                     DO 255 JIF = LI1,LI2
                        T = SYAVG(3+JO,JP,JIF,ANTS(I),J)
                        IF (T.NE.FBLANK) THEN
                           K = IROUND (T * SYSMAX)
                           WRITE (LINE(IP:),1061) K
                        ELSE
                           LINE(IP:) = ' ----'
                           END IF
                        IP = IP + 5
 255                    CONTINUE
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, CSCR, IRET)
                     IF (IRET.NE.0) GO TO 960
                     ICOUNT = ICOUNT + 1
                     END IF
 260              CONTINUE
               IF (LI2.LT.NIF) GO TO 250
 265           CONTINUE
 270        CONTINUE
         END IF
C
 960  IF (IRET.LT.0) THEN
         MSGTXT = 'Stopping at your request'
         CALL MSGWRT (6)
         IRET = 0
      ELSE IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'PRINTER ERROR'
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PRTSY2: ERROR',I4,' ON ',A)
 1050 FORMAT ('*******  Printing ',A,' (',A,') for antenna',I3,
     *   '   10000 =',F11.4,'  *******')
 1051 FORMAT ('*******  Printing ',A,' (',A,') for antenna',I3,
     *   '   10000 =',1PE14.6,'  *******')
 1055 FORMAT ('  Time   Source')
 1056 FORMAT (25I5)
 1060 FORMAT (2(I2.2,':'),I2.2,1X,A)
 1061 FORMAT (I5)
      END
      SUBROUTINE PRTSY3 (NP, NIF, NSC, NA, SYAVG, IRET)
C-----------------------------------------------------------------------
C   PRTSY3 does the display of the data: by scan, IF across, ant down
C   Inputs:
C      NP       I      Number polarizations
C      NIF      I      Number IFs
C      NSC      I      Number scans
C      NA       I      Number antennas
C      SYAVG    R(*)   Resultant averages per scan
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   NP, NIF, NSC, NA, IRET
      REAL      SYAVG(6,NP,NIF,NA,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   JP, JIF, NACROS, PAGE, NANT, MXS, K, ANTS(MAXANT),
     *   JTRIM, I, J, LIF, LI1, LI2, ITT(4), IROUND, IP, JO, ICOUNT,
     *   CRTLIM
      CHARACTER TITL1*132, TITL2*132, LINE*132, SRC*16, CSCR*132,
     *   POLS(4)*1
      LOGICAL   WANT
      REAL      DIFMAX, SUMMAX, SYSMAX, T, U, V, FACT, SCL
      INCLUDE 'REPSYTAB.INC'
      INCLUDE 'PRTSY.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA POLS /'R', 'L', 'X', 'Y'/
C-----------------------------------------------------------------------
      CRTLIM = ABS (CRTMAX) - 4
C                                       which antennas
      NANT = 0
      DO 10 I = 1,NA
         IF (NRECAN(I).GT.0) THEN
            NANT = NANT + 1
            ANTS(NANT) = I
            END IF
 10      CONTINUE
C                                       length of source name
      MXS = 0
      DO 20 I = 1,1000
         J = JTRIM (SRCS(I))
         MXS = MAX (MXS, J)
 20      CONTINUE
C                                       init printer
C                                       Open output device
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, LUNP, FINDP, NACROS, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING THE PRINTER'
         GO TO 990
         END IF
      PAGE = 0
      LIF = (NACROS - 1) / 5
C                                       find scaling
      DIFMAX = 0.0
      SUMMAX = 0.0
      SYSMAX = 0.0
      JO = 0
      IF (DTYPE.GT.3) JO = 3
      DO 40 J = 1,NSC-1
         DO 35 I = 1,NANT
            K = ANTS(I)
            DO 30 JIF = 1,NIF
               DO 25 JP = 1,NP
                  U = SYAVG(1+JO,JP,JIF,K,J)
                  V = SYAVG(2+JO,JP,JIF,K,J)
                  T = SYAVG(3+JO,JP,JIF,K,J)
                  IF (U.NE.FBLANK) DIFMAX = MAX (DIFMAX, U)
                  IF (V.NE.FBLANK) SUMMAX = MAX (SUMMAX, V)
                  IF (T.NE.FBLANK) SYSMAX = MAX (SYSMAX, T)
 25               CONTINUE
 30            CONTINUE
 35         CONTINUE
 40      CONTINUE
      IF (DIFMAX.EQ.0.0) DIFMAX = 1.0
      IF (SUMMAX.EQ.0.0) SUMMAX = 1.0
      IF (SYSMAX.EQ.0.0) SYSMAX = 1.0
      I = LOG10 (DIFMAX) + 6.0
      J = LOG10 (SUMMAX) + 6.0
      K = LOG10 (SYSMAX) + 6.0
      DIFMAX = 10.0 ** (9-I)
      SUMMAX = 10.0 ** (9-J)
      SYSMAX = 10.0 ** (9-K)
C                                       PDIFF
      IF ((DTYPE.NE.2) .AND. (DTYPE.NE.3) .AND. (DTYPE.NE.5) .AND.
     *   (DTYPE.NE.6)) THEN
         FACT = FACTOR(1)
         IF (FACT.LE.0.0) FACT = 1
         DIFMAX = DIFMAX * FACT
         ICOUNT = 0
         IPCNT = 998
         DO 70 JP = 1,NP
            DO 65 J = 1,NSC-1
               WANT = .TRUE.
               IF ((SSCAN(J).GT.0) .AND. (NSOUWD.GT.0)) THEN
                  WANT = .NOT.DOSWNT
                  DO 45 K = 1,NSOUWD
                     IF (SOUWAN(K).EQ.SSCAN(J)) WANT = DOSWNT
 45                  CONTINUE
                  END IF
               IF (.NOT.WANT) GO TO 65
               SRC = SRCS(SSCAN(J))
               T = TTSCAN(J)
               CALL TODHMS (T, ITT)
               IF (((DOCRT.LE.0) .AND. (IPCNT.GT.PRTMAX/2)) .OR.
     *            ((DOCRT.GT.0) .AND. (IPCNT.GT.CRTLIM))) THEN
                  IPCNT = 998
                  ICOUNT = 0
                  END IF
               LI2 = 0
 50            LI1 = LI2 + 1
               LI2 = MIN (LI1 + LIF - 1, NIF)
               SCL = 10000.0 / DIFMAX
               IF ((SCL.LT.10100.0) .AND. (SCL.GT.0.01)) THEN
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1050) 'Pdif', POLS(JP+JP0), J, ITT(2),
     *                  ITT(3), ITT(4), SRC(:MXS), SCL
                  ELSE
                     WRITE (TITL1,1050) 'Pdif RMS', POLS(JP+JP0), J,
     *                  ITT(2), ITT(3), ITT(4), SRC(:MXS), SCL
                     END IF
               ELSE
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1051) 'Pdif', POLS(JP+JP0), J, ITT(2),
     *                  ITT(3), ITT(4), SRC(:MXS), SCL
                  ELSE
                     WRITE (TITL1,1051) 'Pdif RMS', POLS(JP+JP0), J,
     *                  ITT(2), ITT(3), ITT(4), SRC(:MXS), SCL
                     END IF
                  END IF
               WRITE (TITL2,1055) (K, K = LI1,LI2)
               LINE = ' '
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, CSCR, IRET)
               IF (IRET.NE.0) GO TO 960
               IF (ICOUNT.NE.0) THEN
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL1, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL2, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  END IF
               ICOUNT = ICOUNT + 1
               DO 60 I = 1,NANT
                  WRITE (LINE,1060) ANTS(I)
                  IP = 5
                  DO 55 JIF = LI1,LI2
                     V = SYAVG(1+JO,JP,JIF,ANTS(I),J)
                     IF (V.NE.FBLANK) THEN
                        K = IROUND (V * DIFMAX)
                        WRITE (LINE(IP:),1061) K
                     ELSE
                        LINE(IP:) = ' ----'
                        END IF
                     IP = IP + 5
 55                  CONTINUE
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               LINE, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  ICOUNT = ICOUNT + 1
 60               CONTINUE
               IF (LI2.LT.NIF) GO TO 50
 65            CONTINUE
 70         CONTINUE
         END IF
C                                       PSUM
      IF ((DTYPE.NE.1) .AND. (DTYPE.NE.3) .AND. (DTYPE.NE.4) .AND.
     *   (DTYPE.NE.6)) THEN
         FACT = FACTOR(2)
         IF (FACT.LE.0.0) FACT = 1
         SUMMAX = SUMMAX * FACT
         IPCNT = 998
         DO 170 JP = 1,NP
            DO 165 J = 1,NSC-1
               WANT = .TRUE.
               IF ((SSCAN(J).GT.0) .AND. (NSOUWD.GT.0)) THEN
                  WANT = .NOT.DOSWNT
                  DO 145 K = 1,NSOUWD
                     IF (SOUWAN(K).EQ.SSCAN(J)) WANT = DOSWNT
 145                 CONTINUE
                  END IF
               IF (.NOT.WANT) GO TO 165
               SRC = SRCS(SSCAN(J))
               T = TTSCAN(J)
               CALL TODHMS (T, ITT)
               IF (((DOCRT.LE.0) .AND. (IPCNT.GT.PRTMAX/2)) .OR.
     *            ((DOCRT.GT.0) .AND. (IPCNT.GT.CRTLIM))) THEN
                  IPCNT = 998
                  ICOUNT = 0
                  END IF
               LI2 = 0
 150           LI1 = LI2 + 1
               LI2 = MIN (LI1 + LIF - 1, NIF)
               IP = 10 + MXS
               SCL = 10000.0 / SUMMAX
               IF ((SCL.LT.10100.0) .AND. (SCL.GT.0.01)) THEN
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1050) 'Psum', POLS(JP+JP0), J, ITT(2),
     *                  ITT(3), ITT(4), SRC(:MXS), SCL
                  ELSE
                     WRITE (TITL1,1050) 'Psum rms', POLS(JP+JP0), J,
     *                  ITT(2), ITT(3), ITT(4), SRC(:MXS), SCL
                     END IF
               ELSE
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1051) 'Psum', POLS(JP+JP0), J, ITT(2),
     *                  ITT(3), ITT(4), SRC(:MXS), SCL
                  ELSE
                     WRITE (TITL1,1051) 'Psum rms', POLS(JP+JP0), J,
     *                  ITT(2), ITT(3), ITT(4), SRC(:MXS), SCL
                     END IF
                  END IF
               WRITE (TITL2,1055) (K, K = LI1,LI2)
               LINE = ' '
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, CSCR, IRET)
               IF (IRET.NE.0) GO TO 960
               IF (ICOUNT.NE.0) THEN
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL1, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL2, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  END IF
               ICOUNT = ICOUNT + 1
               DO 160 I = 1,NANT
                  WRITE (LINE,1060) ANTS(I)
                  IP = 5
                  DO 155 JIF = LI1,LI2
                     V = SYAVG(2+JO,JP,JIF,ANTS(I),J)
                     IF (V.NE.FBLANK) THEN
                        K = IROUND (V * SUMMAX)
                        WRITE (LINE(IP:),1061) K
                     ELSE
                        LINE(IP:) = ' ----'
                        END IF
                     IP = IP + 5
 155                 CONTINUE
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               LINE, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  ICOUNT = ICOUNT + 1
 160              CONTINUE
               IF (LI2.LT.NIF) GO TO 150
 165           CONTINUE
 170        CONTINUE
         END IF
C                                       PSYS
      IF ((DTYPE.NE.1) .AND. (DTYPE.NE.2) .AND. (DTYPE.NE.4) .AND.
     *   (DTYPE.NE.5)) THEN
         FACT = FACTOR(3)
         IF (FACT.LE.0.0) FACT = 1
         SYSMAX = SYSMAX * FACT
         IPCNT = 998
         DO 270 JP = 1,NP
            DO 265 J = 1,NSC-1
               WANT = .TRUE.
               IF ((SSCAN(J).GT.0) .AND. (NSOUWD.GT.0)) THEN
                  WANT = .NOT.DOSWNT
                  DO 245 K = 1,NSOUWD
                     IF (SOUWAN(K).EQ.SSCAN(J)) WANT = DOSWNT
 245                 CONTINUE
                  END IF
               IF (.NOT.WANT) GO TO 265
               IF (((DOCRT.LE.0) .AND. (IPCNT.GT.PRTMAX/2)) .OR.
     *            ((DOCRT.GT.0) .AND. (IPCNT.GT.CRTLIM))) THEN
                  IPCNT = 998
                  ICOUNT = 0
                  END IF
               SRC = SRCS(SSCAN(J))
               T = TTSCAN(J)
               CALL TODHMS (T, ITT)
               LI2 = 0
 250           LI1 = LI2 + 1
               LI2 = MIN (LI1 + LIF - 1, NIF)
               SCL = 10000.0 / SYSMAX
               IF ((SCL.LT.10100.0) .AND. (SCL.GT.0.01)) THEN
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1050) 'Psys', POLS(JP+JP0), J, ITT(2),
     *                  ITT(3), ITT(4), SRC(:MXS), SCL
                  ELSE
                     WRITE (TITL1,1050) 'Psys RMS', POLS(JP+JP0), J,
     *                  ITT(2), ITT(3), ITT(4), SRC(:MXS), SCL
                     END IF
               ELSE
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1051) 'Psys', POLS(JP+JP0), J, ITT(2),
     *                  ITT(3), ITT(4), SRC(:MXS), SCL
                  ELSE
                     WRITE (TITL1,1051) 'Psys RMS', POLS(JP+JP0), J,
     *                  ITT(2), ITT(3), ITT(4), SRC(:MXS), SCL
                     END IF
                  END IF
               WRITE (TITL2,1055) (K, K = LI1,LI2)
               LINE = ' '
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, CSCR, IRET)
               IF (IRET.NE.0) GO TO 960
               IF (ICOUNT.NE.0) THEN
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL1, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL2, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  END IF
               ICOUNT = ICOUNT + 1
               DO 260 I = 1,NANT
                  WRITE (LINE,1060) ANTS(I)
                  IP = 5
                  DO 255 JIF = LI1,LI2
                     T = SYAVG(3+JO,JP,JIF,ANTS(I),J)
                     IF (T.NE.FBLANK) THEN
                        K = IROUND (T * SYSMAX)
                        WRITE (LINE(IP:),1061) K
                     ELSE
                        LINE(IP:) = ' ----'
                        END IF
                     IP = IP + 5
 255                 CONTINUE
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               LINE, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  ICOUNT = ICOUNT + 1
 260              CONTINUE
               IF (LI2.LT.NIF) GO TO 250
 265           CONTINUE
 270        CONTINUE
         END IF
C
 960  IF (IRET.LT.0) THEN
         MSGTXT = 'Stopping at your request'
         CALL MSGWRT (6)
         IRET = 0
      ELSE IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'PRINTER ERROR'
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PRTSY3: ERROR',I4,' ON ',A)
 1050 FORMAT ('*******  Printing ',A,' (',A,') for scan',I3,' at ',
     *   2(I2.2,':'),I2.2,' source ',A,'   10000 =',F11.4,'  *******')
 1051 FORMAT ('*******  Printing ',A,' (',A,') for scan',I3,' at ',
     *   2(I2.2,':'),I2.2,' source ',A,'   10000 =',1PE14.6,'  *******')
 1055 FORMAT ('Ant',1X,25I5)
 1060 FORMAT (I3)
 1061 FORMAT (I5)
      END
      SUBROUTINE PRTSYS (NP, NIF, NSC, NA, SYDATA, SYAVG, IRET)
C-----------------------------------------------------------------------
C   PRTSYS does the robust averaging of the SY data on a source basis or
C   over all included data
C   Inputs:
C      NP       I      Number polarizations
C      NIF      I      Number IFs
C      NSC      I      Number sources
C      NA       I      Number antennas
C   Outputs:
C      SYDATA   R(*)   Work area to do averaging
C      SYAVG    R(*)   Resultant averages per scan
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   NP, NIF, NSC, NA, IRET
      REAL      SYDATA(2,NP,NIF,*), SYAVG(6,NP,NIF,NA,*)
C
      INTEGER   NITER, MAXSAM, MAXT
      PARAMETER (NITER=9, MAXSAM=10000, MAXT=25000)
      INCLUDE 'PRTSY.INC'
      INCLUDE 'REPSYTAB.INC'
      INCLUDE 'PRTSYUE.INC'
      INTEGER   IANT, SOURID, ANTNO, SUBA, FREQID, SORCS(MAXT), J, NREC,
     *   JP, JIF, I, IREC, NV, IR, IROUND, ISRC, SR, CALTYP
      LOGICAL   WANT
      REAL      TIMEI, VD, VS, MEDIAN, TT, SD, SS, ST,
     *   DVD(MAXSAM), DVS(MAXSAM), DVT(MAXSAM)
      DOUBLE PRECISION TIME, TIMES(MAXT)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
      I = 6 * NP * NIF * NA * NSC
      CALL RFILL (I, FBLANK, SYAVG)
      ISYRNO = 1
      DO 100 ISRC = 1,NOSCAN
         NREC = 0
         ISYRNO = SRECSO(ISRC)
         DO 30 I = 1,NRECSO(ISRC)
            CALL TABSY ('READ', BUFFI, ISYRNO, SYKOLS, SYNUMV, NUMPOL,
     *         NUMIF, TIME, TIMEI, CALTYP, SOURID, ANTNO, SUBA, FREQID,
     *         PDIFF, PSUM, PGAIN, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET,
     *            'READING THE SORTED SY TABLE'
               GO TO 990
            ELSE IF (IRET.LT.0) THEN
               GO TO 30
            ELSE IF (SOURID.NE.SSCAN(ISRC)) THEN
               MSGTXT = 'TABLE READING IS OFF'
               IRET = 99
               GO TO 990
               END IF
C                                       include?
            WANT = (TIME.GE.BTIME) .AND. (TIME.LE.ETIME)
            IF ((SOURID.GT.0) .AND. (NSOUWD.GT.0)) THEN
               WANT = .NOT.DOSWNT
               DO 10 J = 1,NSOUWD
                  IF (SOUWAN(J).EQ.SOURID) WANT = DOSWNT
 10               CONTINUE
               END IF
            IF ((SUBA.GT.0) .AND. (SUBARR.GT.0) .AND.
     *         (SUBA.NE.SUBARR)) WANT = .FALSE.
            IF ((FREQID.GT.0) .AND. (FRQSEL.GT.0) .AND.
     *         (FRQSEL.NE.FREQID)) WANT = .FALSE.
            IF ((WANT) .AND. (NREC.LT.MAXT)) THEN
               NREC = NREC + 1
               TIMES(NREC) = TIME
               SORCS(NREC) = ANTNO
               DO 20 JIF = 1,NIF
                  DO 15 JP = 1,NP
                     SYDATA(1,JP,JIF,NREC) = PDIFF(JP,JIF)
                     SYDATA(2,JP,JIF,NREC) = PSUM(JP,JIF)
 15                  CONTINUE
 20               CONTINUE
               END IF
 30         CONTINUE
         IF (NREC.GT.0) THEN
            DO 80 IANT = 1,NA
               DO 70 JIF = 1,NIF
                  DO 60 JP = 1,NP
                     NV = 0
                     DO 45 IREC = 1,NREC
                        IF (SORCS(IREC).EQ.IANT) THEN
                           VD = SYDATA(1,JP,JIF,IREC)
                           VS = SYDATA(2,JP,JIF,IREC)
                           IF ((VD.NE.FBLANK) .AND. (VS.NE.FBLANK)) THEN
                              IF (CALTYP.EQ.1) THEN
                                 TT = TCAL(JP+2,JIF,IANT)
                              ELSE
                                 TT = TCAL(JP,JIF,IANT)
                                 END IF
                              IF (TT.EQ.FBLANK) TT = 1.0
                              IF ((VD.GT.0.0) .AND. (VD.LT.VS) .AND.
     *                           (NV.LT.MAXSAM)) THEN
                                 NV = NV + 1
                                 DVD(NV) = VD
                                 DVS(NV) = VS
                                 DVT(NV) = TT * VS / (2.0 * VD)
                                 END IF
                              END IF
                           END IF
 45                     CONTINUE
                     IF (NV.LE.0) THEN
                        SYAVG(1,JP,JIF,IANT,ISRC) = FBLANK
                        SYAVG(2,JP,JIF,IANT,ISRC) = FBLANK
                        SYAVG(3,JP,JIF,IANT,ISRC) = FBLANK
                        SYAVG(4,JP,JIF,IANT,ISRC) = FBLANK
                        SYAVG(5,JP,JIF,IANT,ISRC) = FBLANK
                        SYAVG(6,JP,JIF,IANT,ISRC) = FBLANK
                     ELSE
                        SYAVG(1,JP,JIF,IANT,ISRC) = MEDIAN (NV, DVD)
                        SYAVG(2,JP,JIF,IANT,ISRC) = MEDIAN (NV, DVS)
                        SYAVG(3,JP,JIF,IANT,ISRC) = MEDIAN (NV, DVT)
                        VD = SYAVG(1,JP,JIF,IANT,ISRC)
                        VS = SYAVG(2,JP,JIF,IANT,ISRC)
                        TT = SYAVG(3,JP,JIF,IANT,ISRC)
                        SD = 0.0
                        SS = 0.0
                        ST = 0.0
                        DO 50 J = 1,NV
                           SD = SD + (DVD(J) - VD) * (DVD(J) - VD)
                           SS = SS + (DVS(J) - VS) * (DVS(J) - VS)
                           ST = ST + (DVT(J) - TT) * (DVT(J) - TT)
 50                        CONTINUE
                        SYAVG(4,JP,JIF,IANT,ISRC) = SQRT (SD/NV)
                        SYAVG(5,JP,JIF,IANT,ISRC) = SQRT (SS/NV)
                        SYAVG(6,JP,JIF,IANT,ISRC) = SQRT (ST/NV)
                        END IF
 60                  CONTINUE
 70               CONTINUE
 80            CONTINUE
            END IF
 100     CONTINUE
C                                       normalize: by IF
      SR = IROUND (FACTOR(7))
      IR = IROUND (FACTOR(4))
      IF ((IR.GT.0) .AND. (IR.LE.NIF)) THEN
         DO 140 ISRC = 1,NSC
            DO 135 IANT = 1,NA
               DO 130 JP = 1,NP
                  DO 125 J = 1,3
                     TT = SYAVG(J,JP,IR,IANT,ISRC)
                     DO 124 JIF = 1,NIF
                        VD = SYAVG(J,JP,JIF,IANT,ISRC)
                        VS = SYAVG(J+3,JP,JIF,IANT,ISRC)
                        IF ((VD.NE.FBLANK) .AND. (TT.NE.FBLANK) .AND.
     *                     (TT.GT.0.0)) THEN
                           SYAVG(J,JP,JIF,IANT,ISRC) = VD / TT
                           SYAVG(J+3,JP,JIF,IANT,ISRC) = VS / TT
                        ELSE
                           SYAVG(J,JP,JIF,IANT,ISRC) = FBLANK
                           SYAVG(J+3,JP,JIF,IANT,ISRC) = FBLANK
                           END IF
 124                    CONTINUE
 125                 CONTINUE
 130              CONTINUE
 135           CONTINUE
 140        CONTINUE
         END IF
C                                       normalize: by antenna
      IR = IROUND (FACTOR(5))
      IF ((IR.GT.0) .AND. (IR.LE.NA)) THEN
         DO 160 ISRC = 1,NSC
            DO 155 JIF = 1,NIF
               DO 150 JP = 1,NP
                  DO 145 J = 1,3
                     TT = SYAVG(J,JP,JIF,IR,ISRC)
                     DO 144 IANT = 1,NA
                        VD = SYAVG(J,JP,JIF,IANT,ISRC)
                        VS = SYAVG(J+3,JP,JIF,IANT,ISRC)
                        IF ((VD.NE.FBLANK) .AND. (TT.NE.FBLANK) .AND.
     *                     (TT.GT.0.0)) THEN
                           SYAVG(J,JP,JIF,IANT,ISRC) = VD / TT
                           SYAVG(J+3,JP,JIF,IANT,ISRC) = VS / TT
                        ELSE
                           SYAVG(J,JP,JIF,IANT,ISRC) = FBLANK
                           SYAVG(J+3,JP,JIF,IANT,ISRC) = FBLANK
                           END IF
 144                    CONTINUE
 145                 CONTINUE
 150              CONTINUE
 155           CONTINUE
 160        CONTINUE
         END IF
C                                       normalize: by SOURCE
      IR = 0
      IF (SR.GT.0) THEN
         DO 163 ISRC = 1,NSC
            IF (SR.EQ.SSCAN(ISRC)) IR = ISRC
 163        CONTINUE
         END IF
      IF ((IR.GT.0) .AND. (IR.LE.NSC)) THEN
         DO 180 IANT = 1,NA
            DO 175 JIF = 1,NIF
               DO 170 JP = 1,NP
                  DO 165 J = 1,3
                     TT = SYAVG(J,JP,JIF,IANT,IR)
                     DO 164 ISRC = 1,NSC
                        VD = SYAVG(J,JP,JIF,IANT,ISRC)
                        VS = SYAVG(J+3,JP,JIF,IANT,ISRC)
                        IF ((VD.NE.FBLANK) .AND. (TT.NE.FBLANK) .AND.
     *                     (TT.GT.0.0)) THEN
                           SYAVG(J,JP,JIF,IANT,ISRC) = VD / TT
                           SYAVG(J+3,JP,JIF,IANT,ISRC) = VS / TT
                        ELSE
                           SYAVG(J,JP,JIF,IANT,ISRC) = FBLANK
                           SYAVG(J+3,JP,JIF,IANT,ISRC) = FBLANK
                           END IF
 164                    CONTINUE
 165                 CONTINUE
 170              CONTINUE
 175           CONTINUE
 180        CONTINUE
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PRTSYS: ERROR',I4,' ON ',A)
      END
      SUBROUTINE PRTSY4 (NP, NIF, NSC, NA, SYAVG, IRET)
C-----------------------------------------------------------------------
C   PRTSY4 does the display of the data: by source, IF across, ant down
C   Inputs:
C      NP       I      Number polarizations
C      NIF      I      Number IFs
C      NSC      I      Number sources
C      NA       I      Number antennas
C      SYAVG    R(*)   Resultant averages per scan
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   NP, NIF, NSC, NA, IRET
      REAL      SYAVG(6,NP,NIF,NA,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   JP, JIF, NACROS, PAGE, NANT, MXS, K, ANTS(MAXANT),
     *   JTRIM, I, J, LIF, LI1, LI2, IROUND, IP, JO, SR, ICOUNT, CRTLIM
      CHARACTER TITL1*132, TITL2*132, LINE*132, SRC*16, CSCR*132,
     *   POLS(4)*1
      REAL      DIFMAX, SUMMAX, SYSMAX, T, U, V, FACT, SCL
      INCLUDE 'REPSYTAB.INC'
      INCLUDE 'PRTSY.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA POLS /'R', 'L', 'X', 'Y'/
C-----------------------------------------------------------------------
      CRTLIM = ABS (CRTMAX) - 4
C                                       length of source name
      MXS = 0
      DO 20 I = 1,1000
         J = JTRIM (SRCS(I))
         MXS = MAX (MXS, J)
 20      CONTINUE
      SR = IROUND (FACTOR(7))
C                                       init printer
C                                       Open output device
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, LUNP, FINDP, NACROS, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING THE PRINTER'
         GO TO 990
         END IF
      PAGE = 0
      LIF = (NACROS - 1) / 5
C                                       find scaling
      DIFMAX = 0.0
      SUMMAX = 0.0
      SYSMAX = 0.0
      JO = 0
      IF (DTYPE.GT.3) JO = 3
      CALL FILL (NA, 0, NRECAN)
      DO 40 J = 1,NSC
         DO 35 I = 1,NA
            DO 30 JIF = 1,NIF
               DO 25 JP = 1,NP
                  U = SYAVG(1+JO,JP,JIF,I,J)
                  V = SYAVG(2+JO,JP,JIF,I,J)
                  T = SYAVG(3+JO,JP,JIF,I,J)
                  IF ((U.NE.FBLANK) .OR. (V.NE.FBLANK) .OR.
     *               (T.NE.FBLANK)) THEN
                     NRECAN(I) = NRECAN(I) + 1
                     IF (U.NE.FBLANK) DIFMAX = MAX (DIFMAX, U)
                     IF (V.NE.FBLANK) SUMMAX = MAX (SUMMAX, V)
                     IF (T.NE.FBLANK) SYSMAX = MAX (SYSMAX, T)
                     END IF
 25               CONTINUE
 30            CONTINUE
 35         CONTINUE
 40      CONTINUE
      IF (DIFMAX.EQ.0.0) DIFMAX = 1.0
      IF (SUMMAX.EQ.0.0) SUMMAX = 1.0
      IF (SYSMAX.EQ.0.0) SYSMAX = 1.0
      I = LOG10 (DIFMAX) + 6.0
      J = LOG10 (SUMMAX) + 6.0
      K = LOG10 (SYSMAX) + 6.0
      DIFMAX = 10.0 ** (9-I)
      SUMMAX = 10.0 ** (9-J)
      SYSMAX = 10.0 ** (9-K)
C                                       which antennas
      NANT = 0
      DO 10 I = 1,NA
         IF (NRECAN(I).GT.0) THEN
            NANT = NANT + 1
            ANTS(NANT) = I
            END IF
 10      CONTINUE
C                                       PDIFF
      IF ((DTYPE.NE.2) .AND. (DTYPE.NE.3) .AND. (DTYPE.NE.5) .AND.
     *   (DTYPE.NE.6)) THEN
         FACT = FACTOR(1)
         IF (FACT.LE.0.0) FACT = 1
         DIFMAX = DIFMAX * FACT
         IPCNT = 998
         DO 70 JP = 1,NP
            DO 65 J = 1,NSC
               IF (SSCAN(J).EQ.SR) GO TO 65
               SRC = SRCS(SSCAN(J))
               IF (((DOCRT.LE.0) .AND. (IPCNT.GT.PRTMAX/2)) .OR.
     *            ((DOCRT.GT.0) .AND. (IPCNT.GT.CRTLIM))) THEN
                  IPCNT = 998
                  ICOUNT = 0
                  END IF
               LI2 = 0
 50            LI1 = LI2 + 1
               LI2 = MIN (LI1 + LIF - 1, NIF)
               SCL = 10000.0 / DIFMAX
               IF ((SCL.LT.10100.0) .AND. (SCL.GT.0.01)) THEN
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1050) 'Pdif', POLS(JP+JP0), SSCAN(J),
     *                  SRC(:MXS), SCL
                  ELSE
                     WRITE (TITL1,1050) 'Pdif RMS', POLS(JP+JP0),
     *                  SSCAN(J), SRC(:MXS), SCL
                     END IF
               ELSE
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1051) 'Pdif', POLS(JP+JP0), SSCAN(J),
     *                  SRC(:MXS), SCL
                  ELSE
                     WRITE (TITL1,1051) 'Pdif RMS', POLS(JP+JP0),
     *                  SSCAN(J), SRC(:MXS), SCL
                     END IF
                  END IF
               WRITE (TITL2,1055) (K, K = LI1,LI2)
               LINE = ' '
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, CSCR, IRET)
               IF (IRET.NE.0) GO TO 960
               IF (ICOUNT.NE.0) THEN
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL1, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL2, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  END IF
               ICOUNT = ICOUNT + 1
               DO 60 I = 1,NANT
                  WRITE (LINE,1060) ANTS(I)
                  IP = 5
                  DO 55 JIF = LI1,LI2
                     V = SYAVG(1+JO,JP,JIF,ANTS(I),J)
                     IF (V.NE.FBLANK) THEN
                        K = IROUND (V * DIFMAX)
                        WRITE (LINE(IP:),1061) K
                     ELSE
                        LINE(IP:) = ' ----'
                        END IF
                     IP = IP + 5
 55                  CONTINUE
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               LINE, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  ICOUNT = ICOUNT + 1
 60               CONTINUE
               IF (LI2.LT.NIF) GO TO 50
 65            CONTINUE
 70         CONTINUE
         END IF
C                                       PSUM
      IF ((DTYPE.NE.1) .AND. (DTYPE.NE.3) .AND. (DTYPE.NE.4) .AND.
     *   (DTYPE.NE.6)) THEN
         FACT = FACTOR(2)
         IF (FACT.LE.0.0) FACT = 1
         SUMMAX = SUMMAX * FACT
         IPCNT = 998
         DO 170 JP = 1,NP
            DO 165 J = 1,NSC
               IF (SSCAN(J).EQ.SR) GO TO 165
               SRC = SRCS(SSCAN(J))
               IF (((DOCRT.LE.0) .AND. (IPCNT.GT.PRTMAX/2)) .OR.
     *            ((DOCRT.GT.0) .AND. (IPCNT.GT.CRTLIM))) THEN
                  IPCNT = 998
                  ICOUNT = 0
                  END IF
               LI2 = 0
 150           LI1 = LI2 + 1
               LI2 = MIN (LI1 + LIF - 1, NIF)
               IP = 10 + MXS
               SCL = 10000.0 / SUMMAX
               IF ((SCL.LT.10100.0) .AND. (SCL.GT.0.01)) THEN
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1050) 'Psum', POLS(JP+JP0), SSCAN(J),
     *                  SRC(:MXS), SCL
                  ELSE
                     WRITE (TITL1,1050) 'Psum rms', POLS(JP+JP0),
     *                  SSCAN(J), SRC(:MXS), SCL
                     END IF
               ELSE
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1051) 'Psum', POLS(JP+JP0), SSCAN(J),
     *                  SRC(:MXS), SCL
                  ELSE
                     WRITE (TITL1,1051) 'Psum rms', POLS(JP+JP0),
     *                  SSCAN(J), SRC(:MXS), SCL
                     END IF
                  END IF
               WRITE (TITL2,1055) (K, K = LI1,LI2)
               LINE = ' '
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, CSCR, IRET)
               IF (IRET.NE.0) GO TO 960
               IF (ICOUNT.NE.0) THEN
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL1, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL2, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  END IF
               ICOUNT = ICOUNT + 1
               DO 160 I = 1,NANT
                  WRITE (LINE,1060) ANTS(I)
                  IP = 5
                  DO 155 JIF = LI1,LI2
                     V = SYAVG(2+JO,JP,JIF,ANTS(I),J)
                     IF (V.NE.FBLANK) THEN
                        K = IROUND (V * SUMMAX)
                        WRITE (LINE(IP:),1061) K
                     ELSE
                        LINE(IP:) = ' ----'
                        END IF
                     IP = IP + 5
 155                 CONTINUE
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               LINE, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  ICOUNT = ICOUNT + 1
 160              CONTINUE
               IF (LI2.LT.NIF) GO TO 150
 165           CONTINUE
 170        CONTINUE
         END IF
C                                       PSYS
      IF ((DTYPE.NE.1) .AND. (DTYPE.NE.2) .AND. (DTYPE.NE.4) .AND.
     *   (DTYPE.NE.5)) THEN
         FACT = FACTOR(3)
         IF (FACT.LE.0.0) FACT = 1
         SYSMAX = SYSMAX * FACT
         IPCNT = 998
         DO 270 JP = 1,NP
            DO 265 J = 1,NSC
               IF (SSCAN(J).EQ.SR) GO TO 265
               IF (((DOCRT.LE.0) .AND. (IPCNT.GT.PRTMAX/2)) .OR.
     *            ((DOCRT.GT.0) .AND. (IPCNT.GT.CRTLIM))) THEN
                  IPCNT = 998
                  ICOUNT = 0
                  END IF
               SRC = SRCS(SSCAN(J))
               LI2 = 0
 250           LI1 = LI2 + 1
               LI2 = MIN (LI1 + LIF - 1, NIF)
               SCL = 10000.0 / SYSMAX
               IF ((SCL.LT.10100.0) .AND. (SCL.GT.0.01)) THEN
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1050) 'Psys', POLS(JP+JP0), SSCAN(J),
     *                  SRC(:MXS), SCL
                  ELSE
                     WRITE (TITL1,1050) 'Psys RMS', POLS(JP+JP0),
     *                  SSCAN(J), SRC(:MXS), SCL
                     END IF
               ELSE
                  IF (DTYPE.LE.3) THEN
                     WRITE (TITL1,1051) 'Psys', POLS(JP+JP0), SSCAN(J),
     *                  SRC(:MXS), SCL
                  ELSE
                     WRITE (TITL1,1051) 'Psys RMS', POLS(JP+JP0),
     *                  SSCAN(J), SRC(:MXS), SCL
                     END IF
                  END IF
               WRITE (TITL2,1055) (K, K = LI1,LI2)
               LINE = ' '
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, IPCNT, PAGE, CSCR, IRET)
               IF (IRET.NE.0) GO TO 960
               IF (ICOUNT.NE.0) THEN
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL1, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               TITL2, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  END IF
               ICOUNT = ICOUNT + 1
               DO 260 I = 1,NANT
                  WRITE (LINE,1060) ANTS(I)
                  IP = 5
                  DO 255 JIF = LI1,LI2
                     T = SYAVG(3+JO,JP,JIF,ANTS(I),J)
                     IF (T.NE.FBLANK) THEN
                        K = IROUND (T * SYSMAX)
                        WRITE (LINE(IP:),1061) K
                     ELSE
                        LINE(IP:) = ' ----'
                        END IF
                     IP = IP + 5
 255                 CONTINUE
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               LINE, IPCNT, PAGE, CSCR, IRET)
                  IF (IRET.NE.0) GO TO 960
                  ICOUNT = ICOUNT + 2
 260              CONTINUE
               IF (LI2.LT.NIF) GO TO 250
 265           CONTINUE
 270        CONTINUE
         END IF
C
 960  IF (IRET.LT.0) THEN
         MSGTXT = 'Stopping at your request'
         CALL MSGWRT (6)
         IRET = 0
      ELSE IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'PRINTER ERROR'
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PRTSY4: ERROR',I4,' ON ',A)
 1050 FORMAT ('*******  Printing ',A,' (',A,') for source',I5,2x,A,
     *   '   10000 =',F11.4,'  *******')
 1051 FORMAT ('*******  Printing ',A,' (',A,') for source',I5,2X,A,
     *   '   10000 =',1PE14.6,'  *******')
 1055 FORMAT ('Ant',1X,25I5)
 1060 FORMAT (I3)
 1061 FORMAT (I5)
      END
      SUBROUTINE PRTSYO (NP, NIF, NSC, NA, FRQSEL, SYAVG, IRET)
C-----------------------------------------------------------------------
C   PRTSYO: reads an efficiency table, computes a new one and writes it
C   out.
C   Inputs:
C      NP       I      Number polarizations
C      NIF      I      Number IFs
C      NSC      I      Number sources
C      NA       I      Number antennas
C      SYAVG    R(*)   Resultant averages per scan
C   Outputs:
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   NP, NIF, NSC, NA, FRQSEL, IRET
      REAL     SYAVG(6,NP,NIF,NA,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   K, JTRIM, I, J, IROUND, SR, LUNTMP, VER, ISBAND(MAXIF),
     *   IAR, ISRC, IR, LUN
      CHARACTER LINE*132, BNDCOD(MAXIF)*8, RXBAND*2
      REAL      U, V, VEFF(2,MAXIF,MAXANT), BUFF(3,MAXIF,MAXANT),
     *   FINC(MAXIF)
      DOUBLE PRECISION FOFF(MAXIF)
      INCLUDE 'REPSYTAB.INC'
      INCLUDE 'PRTSY.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       close actual printer
      IF (LUNP.GT.0) CALL LPCLOS (LUNP, FINDP, IPCNT, IRET)
C                                       get efficiencies from file or ??
      VER = 1
      LUN = LUNTMP (1)
      CALL CHNDAT ('READ', BUFF, INDISK, INCNO, VER, CATBLK, LUN,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINDING FREQUENCIES'
         GO TO 990
         END IF
      DO 10 J = 1,NIF
         FOFF(J) = (FOFF(J) + CATD(KDCRV+JLOCF)) / 1.D9
 10      CONTINUE
      CALL GETBND (NIF, FOFF, BNDCOD, J, RXBAND)
      CALL FNDEFF (NIF, FOFF, RXBAND, NA, CALIN, BUFF, VEFF)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINDING INPUT EFFICIENCIES'
         GO TO 990
         END IF
C                                       open output file
      LUNP = LUNTMP (2)
      CALL ZTXOPN ('WRIT', LUNP, FINDP, CALOUT, .TRUE., IRET)
      IF (IRET.NE.0) THEN
         LUNP = 0
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT TEXT FILE'
         GO TO 990
         END IF
C                                       reference antenna
      IAR = FACTOR(5) + 0.1
      SR = IROUND (FACTOR(7))
      IR = 0
      DO 30 ISRC = 1,NSC
         IF (SR.EQ.SSCAN(ISRC)) IR = ISRC
30       CONTINUE
      SR = 3 - IR
C                                       loop IFs
      DO 50 J = 1,NIF
         DO 40 I = 1,NA
            IF (NRECAN(I).GT.0) THEN
               IF ((SYAVG(2,1,J,I,SR).NE.FBLANK) .AND.
     *            (SYAVG(2,2,J,I,SR).NE.FBLANK)) THEN
                  U = SYAVG(2,1,J,I,SR) * VEFF(1,J,IAR)
                  V = SYAVG(2,2,J,I,SR) * VEFF(2,J,IAR)
                  WRITE (LINE,1010) RXBAND, I, FOFF(J), U, V
                  K = JTRIM (LINE)
                  CALL ZTXIO ('WRIT', LUNP, FINDP, LINE(:K), IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT FILE'
                     GO TO 990
                     END IF
                  END IF
               END IF
 40         CONTINUE
 50      CONTINUE
      CALL ZTXCLS (LUNP, FINDP, IRET)
      LUNP = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PRTSYO: ERROR',I4,' ON ',A)
 1010 FORMAT (A2,I6,F11.3,F9.2,F10.2)
      END
      SUBROUTINE SYFSEL (DISKI, CNOI, DISKO, CNOO, VERI, VERO, CATIN,
     *   CATOUT, TB, TE, BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C   Copies a subset of IFs in a SY table, can also modify the FQ ID
C   Inputs:
C      DISKI    I        Input volume number
C      CNOI     I        Input catalog number
C      DISKO    I        Output volume number
C      CNOO     I        Output catalog number
C      VER      I        Version to check/modify
C      CATIN    I(256)   Input catalog header
C      CATOUT   I(256)   Output catalog header
C      TB       R        Beginning time in days
C      TE       R        Ending time in days
C   Input/Output:
C      BUFFER   I(*)     Work buffer
C      OBUFF    I(*)     Work buffer
C   Output:
C      IRET     I        Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOI, DISKO, CNOO, VERI, VERO, CATIN(256),
     *   CATOUT(256), BUFFER(*), OBUFF(*), IRET
      REAL      TB, TE
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXSYC
      PARAMETER (MAXSYC=13)
C
      INTEGER   ISYRNO, SYKOLS(MAXSYC), SYNUMV(MAXSYC), NUMANT, NUMPOL,
     *   NUMIF, OKOLS(MAXSYC), ONUMV(MAXSYC), NSYROW, I, SOURID, ANTNO,
     *   SUBA, FREQID, OSYRNO, JIF, IPOL, OVER, NDEL, NTOT, JRET, NPART,
     *   LUNI, LUNO, LUNTMP, CALTYP
      REAL      PDIFF(2,MAXIF), PSUM(2,MAXIF), PGAIN(2,MAXIF), TIMEI
      DOUBLE PRECISION TIME
      LOGICAL   REFMT, GOTONE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
C-----------------------------------------------------------------------
C                                       open flag table
      TMFLST = -1.E20
      NUMFLG = 0
      IFGRNO = 1
      NDEL = 0
      NTOT = 0
      NPART = 0
C                                       Open SY file
      LUNI = LUNTMP (1)
      CALL SYINI ('READ', BUFFER, DISKI, CNOI, VERI, CATIN, LUNI,
     *   ISYRNO, SYKOLS, SYNUMV, NUMANT, NUMPOL, NUMIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       # rows in old table
      NSYROW = BUFFER(5)
      REFMT = .FALSE.
C                                       Open up new SY table
      OVER = VERO
      LUNO = LUNTMP (1)
      CALL SYINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OSYRNO, OKOLS, ONUMV, NUMANT, NUMPOL, NUMIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1,NSYROW
         CALL TABSY ('READ', BUFFER, ISYRNO, SYKOLS, SYNUMV, NUMPOL,
     *      NUMIF, TIME, TIMEI, CALTYP, SOURID, ANTNO, SUBA, FREQID,
     *      PDIFF, PSUM, PGAIN, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1030) IRET
            GO TO 990
            END IF
C                                       flag info
         CALL SYFLG (NUMPOL, NUMIF, TIME, SOURID, ANTNO, SUBA, FREQID,
     *      PDIFF, PSUM, PGAIN, NPART, JRET)
         IF (JRET.GT.0) THEN
            IRET = JRET
            GO TO 999
            END IF
C                                       Time selection
         IF ((TIME.LT.TB) .OR. (TIME.GT.TE)) IRET = -1
         IF ((JRET.EQ.-1) .AND. (IRET.EQ.0)) THEN
            IRET = -1
            NDEL = NDEL + 1
            END IF
C                                       Is this record selected ?
         IF (IRET.LT.0) THEN
            REFMT = .TRUE.
C                                       Re-number IF's
         ELSE
            GOTONE = .FALSE.
            DO 90 JIF = 1,NUMIF
               DO 80 IPOL = 1,NUMPOL
                  IF (PDIFF(IPOL,JIF).NE.FBLANK) GOTONE = .TRUE.
                  IF (PSUM(IPOL,JIF).NE.FBLANK) GOTONE = .TRUE.
 80               CONTINUE
 90            CONTINUE
            IF (GOTONE) THEN
               NTOT = NTOT + 1
               CALL TABSY ('WRIT', OBUFF, OSYRNO, OKOLS, ONUMV, NUMPOL,
     *            NUMIF, TIME, TIMEI, CALTYP, SOURID, ANTNO, SUBA,
     *            FREQID, PDIFF, PSUM, PGAIN, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1040) IRET
                  GO TO 990
                  END IF
            ELSE
               REFMT = .TRUE.
               END IF
            END IF
 100     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ISYRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OSYRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted SY', DISKI, CNOI, VERI,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied SY', DISKI, CNOI, VERI, DISKO,
     *         CNOO, OVER
            END IF
         CALL MSGWRT (3)
         NTOT = NTOT + NDEL
         WRITE (MSGTXT,1101) NDEL, NTOT
         CALL REFRMT (MSGTXT, '_', I)
         CALL MSGWRT (3)
         WRITE (MSGTXT,1102) NPART
         CALL REFRMT (MSGTXT, '_', I)
         IF (NPART.GT.0) CALL MSGWRT (3)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SYFSEL: ERROR ',I3,' INITING OLD TABLE')
 1020 FORMAT ('SYFSEL: ERROR ',I3,' INITING NEW TABLE')
 1030 FORMAT ('SYFSEL: ERROR ',I3,' READING OLD TABLE')
 1040 FORMAT ('SYFSEL: ERROR ',I3,' WRITING NEW TABLE')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
 1101 FORMAT ('__Fully deleted',I10,' of',I12,' SY records applying',
     *   ' flag table')
 1102 FORMAT ('__Partly flagged',I10,' SY records applying flag table')
      END
      SUBROUTINE SYFLG (NPOL, NIF, TIME, SOURID, ANTNO, SUBA, FREQID,
     *   PDIFF, PSUM, PGAIN, NPART, IRET)
C-----------------------------------------------------------------------
C   Flags a TY table row based on the flags in FG table loaded to Common
C   Inputs:
C      NPOL     I      Number polarizations in TY data
C      NIF      I      Number of IFs in those data
C      TIME     R      Time of table row
C      SOURID   I      Source number of row
C      ANTNO    I      Antenna number of row
C      SUBA     I      Subarray of row
C      FREQID   I      Frequency ID if row
C   In/Out:
C      PDIFF    R(*)   Pon-Poff
C      PSUM     R(*)   Pon+Poff
C      PGAIN    R(*)   Post detection gains
C      NPART    I      count of partly flagged records
C   Inputs from include DSEL.INC:
C      NUMFLG     I    Number of flagging entries.
C      TMFLST     R    Time of last visibility for which flagging
C                      was checked.
C      FLGSOU(*)  I    Source id numbers to flag, 0=all.
C      FLGANT(*)  I    Antenna numbers to flag, 0=all.
C      FLGBAS(*)  I    Baseline (A1*32768+A2) numbers to flag, 0=all.
C      FLGSUB(*)  I    Subarray numbers to flag, 0=all.
C      FLGFQD(*)  I    Freqid numbers to flag, <=0=all.
C                      Following should have defaults filled in.
C      FLGBIF(*)  I    First IF to flag.
C      FLGEIF(*)  I    Highest IF to flag.
C      FLGBCH(*)  I    First channel to flag.
C      FLGECH(*)  I    Highest channel to flag.
C      FLGPOL(4,*)L    Flags for the polarizations, should correspond
C                      to selected polarization types.
C   Output:
C      IRET     I      0 -> okay, -1 -> all flagged
C-----------------------------------------------------------------------
      INTEGER   NPOL, NIF, SOURID, ANTNO, SUBA, FREQID, NPART, IRET
      REAL      PDIFF(2,*), PSUM(2,*), PGAIN(2,*)
      DOUBLE PRECISION TIME
C
      INTEGER   IFLAG, FLGA, JPOLN, JIF, LIMF1, LIMF2, IPOLPT
      REAL      RTIME
      LOGICAL   PART
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      RTIME = TIME
      IF (TMFLST.LT.TIME) CALL NXTFLG (RTIME, .TRUE., IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NUMFLG.LE.0) GO TO 999
      PART = .FALSE.
C                                       loop over current flags
      DO 50 IFLAG = 1,NUMFLG
C                                       Check time if needed
         IF ((TIME.LT.FLGTST(IFLAG)) .OR. (TIME.GT.FLGTND(IFLAG)))
     *      GO TO 50
C                                       Check source
         IF ((FLGSOU(IFLAG).NE.SOURID) .AND. (FLGSOU(IFLAG).NE.0) .AND.
     *      (SOURID.NE.0)) GO TO 50
C                                       Check antenna
         FLGA = FLGANT(IFLAG)
         IF ((FLGA.NE.0) .AND. (FLGA.NE.ANTNO)) GO TO 50
C                                       Check subarray
         IF ((FLGSUB(IFLAG).GT.0) .AND. (FLGSUB(IFLAG).NE.SUBA))
     *      GO TO 50
C                                       Check freqid.: may be changed
C                                       from input to 1 already
         IF ((FLGFQD(IFLAG).GT.0) .AND. (FLGFQD(IFLAG).NE.FREQID) .AND.
     *      (FREQID.GT.0)) GO TO 50
C                                       Some data to be flagged
C                                       Set limits
         LIMF1 = FLGBIF(IFLAG)
         LIMF2 = FLGEIF(IFLAG)
C                                       Loop over polarizations
         IPOLPT = ABS(KCOR0) - 1
         IF (KCOR0.LT.-4) IPOLPT = IPOLPT - 4
         DO 40 JPOLN = 1,NPOL
            IF (FLGPOL(JPOLN+IPOLPT,IFLAG)) THEN
               PART = .TRUE.
C                                       Loop over IF
               DO 30 JIF = LIMF1,LIMF2
                  PDIFF(JPOLN,JIF) = FBLANK
                  PSUM(JPOLN,JIF) = FBLANK
                  PGAIN(JPOLN,JIF) = FBLANK
 30               CONTINUE
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       Check if data all flagged
      IF (PART) NPART = NPART + 1
      DO 70 JPOLN = 1,NPOL
         DO 60 JIF = 1,NIF
            IF ((PDIFF(JPOLN,JIF).NE.FBLANK) .OR.
     *         (PSUM(JPOLN,JIF).NE.FBLANK)) GO TO 999
 60         CONTINUE
 70      CONTINUE
      IF (PART) NPART = NPART - 1
      IRET = -1
C
 999  RETURN
      END
      SUBROUTINE PRTCHP (IRET)
C-----------------------------------------------------------------------
C   PRTCHP counts line of print for PRTSYP
C   Outputs:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER SCRSTR*90, CTEMP*4
      INTEGER   I, PT1, PT2, NCOUNT, TTY(2)
      INCLUDE 'PRTSY.INC'
      INCLUDE 'PRTSYUE.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF ((DOCRT.GT.0.0) .OR. (LPNAME.NE.' ')) GO TO 999
      MSGTXT = 'Checking count of lines for direct output to printer'
      CALL MSGWRT (2)
      NCOUNT = 0
      IF (DTYPE.LE.0) THEN
         PT1 = 1
         PT2 = 3
      ELSE
         PT1 = DTYPE
         PT2 = DTYPE
         END IF
      DO 20 I = PT1,PT2
         IPCNT = 998
         NCOUNT = NCOUNT + 1
         NCOUNT = NCOUNT + DDNANT * DDNIF
         NCOUNT = NCOUNT + 1
         NCOUNT = NCOUNT + DDNANT
         NCOUNT = NCOUNT + 2
 20      CONTINUE
C
      IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) THEN
         IF (NCOUNT.GT.1000) IRET = -1
      ELSE IF (NCOUNT.GT.500) THEN
         TTY(1) = 5
         CALL ZOPEN (TTY(1), TTY(2), 1, SCRSTR, .FALSE., .FALSE.,
     *      .TRUE., IRET)
         MSGTXT = 'PROBLEM OPENING TERMINAL'
         IF (IRET.GT.0) GO TO 990
         WRITE (SCRSTR,1020) NCOUNT
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRSTR, IRET)
         MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
         IF (IRET.GT.0) GO TO 990
         SCRSTR = 'Do you really want to print this much??' //
     *      ' Enter Y or y if so'
         CALL INQSTR (TTY, SCRSTR, 1, CTEMP, IRET)
         IF (IRET.GT.0) GO TO 990
         IF ((CTEMP(:1).NE.'y') .AND. (CTEMP(:1).NE.'Y')) THEN
            SCRSTR = 'Good choice - save trees'
            IRET = -1
         ELSE
            SCRSTR = 'OKAY, printing anyway'
            END IF
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRSTR, I)
         CALL ZCLOSE (TTY(1), TTY(2), I)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Requested print job is',I10,' lines long!')
      END
      SUBROUTINE PRTCH1 (NP, NIF, NSC, NA, IRET)
C-----------------------------------------------------------------------
C   PRTCH1 checks the display of the data: by IF, ant across, scan down
C   Inputs:
C      NP       I      Number polarizations
C      NIF      I      Number IFs
C      NSC      I      Number scans
C      NA       I      Number antennas
C   Output:
C      IRET     I      Error code: -1 -> too many lines for user
C-----------------------------------------------------------------------
      INTEGER   NP, NIF, NSC, NA, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   JP, JIF, NACROS, NANT, MXS, K, ANTS(MAXANT), I, J, LANT,
     *   LA1, LA2, NCOUNT, TTY(2), ICOUNT
      CHARACTER SCRSTR*132, CTEMP*4
      LOGICAL   WANT
      INCLUDE 'REPSYTAB.INC'
      INCLUDE 'PRTSY.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF ((DOCRT.GT.0.0) .OR. (LPNAME.NE.' ')) GO TO 999
      MSGTXT = 'Checking count of lines for direct output to printer'
      CALL MSGWRT (2)
      NCOUNT = 0
C                                       which antennas
      NANT = 0
      DO 10 I = 1,NA
         IF (ARECAN(I).GT.0) THEN
            NANT = NANT + 1
            ANTS(NANT) = I
            END IF
 10      CONTINUE
      NACROS = 132
      LANT = (NACROS - MXS - 10) / 4
C                                       PDIFF
      IF ((DTYPE.NE.2) .AND. (DTYPE.NE.3) .AND. (DTYPE.NE.5) .AND.
     *   (DTYPE.NE.6)) THEN
         ICOUNT = 0
         DO 70 JP = 1,NP
            DO 65 JIF = 1,NIF
               IF (ICOUNT.GT.PRTMAX/2) THEN
                  NCOUNT = NCOUNT + PRTMAX - ICOUNT - 4
                  ICOUNT = 0
                  END IF
               LA2 = 0
 50            LA1 = LA2 + 1
               LA2 = MIN (LA1 + LANT - 1, NANT)
               NCOUNT = NCOUNT + 1
               IF (ICOUNT.NE.0) THEN
                  NCOUNT = NCOUNT + 2
                  ICOUNT = ICOUNT + 2
                  END IF
               ICOUNT = ICOUNT + 1
               DO 60 J = 1,NSC-1
                  WANT = .TRUE.
                  IF ((SSCAN(J).GT.0) .AND. (NSOUWD.GT.0)) THEN
                     WANT = .NOT.DOSWNT
                     DO 51 K = 1,NSOUWD
                        IF (SOUWAN(K).EQ.SSCAN(J)) WANT = DOSWNT
 51                     CONTINUE
                     END IF
                  IF (WANT) THEN
                     NCOUNT = NCOUNT + 1
                     ICOUNT = ICOUNT + 1
                     END IF
 60               CONTINUE
               IF (LA2.LT.NANT) GO TO 50
 65            CONTINUE
 70         CONTINUE
         END IF
C                                       PSUM
      IF ((DTYPE.NE.1) .AND. (DTYPE.NE.3) .AND. (DTYPE.NE.4) .AND.
     *   (DTYPE.NE.6)) THEN
         ICOUNT = 0
         DO 170 JP = 1,NP
            DO 165 JIF = 1,NIF
               IF (ICOUNT.GT.PRTMAX/2) THEN
                  NCOUNT = NCOUNT + PRTMAX - ICOUNT - 4
                  ICOUNT = 0
                  END IF
               LA2 = 0
 150           LA1 = LA2 + 1
               LA2 = MIN (LA1 + LANT - 1, NANT)
               NCOUNT = NCOUNT + 1
               IF (ICOUNT.NE.0) THEN
                  NCOUNT = NCOUNT + 2
                  ICOUNT = ICOUNT + 2
                  END IF
               ICOUNT = ICOUNT + 1
               DO 160 J = 1,NSC-1
                  WANT = .TRUE.
                  IF ((SSCAN(J).GT.0) .AND. (NSOUWD.GT.0)) THEN
                     WANT = .NOT.DOSWNT
                     DO 151 K = 1,NSOUWD
                        IF (SOUWAN(K).EQ.SSCAN(J)) WANT = DOSWNT
 151                    CONTINUE
                     END IF
                  IF (WANT) THEN
                     NCOUNT = NCOUNT + 1
                     ICOUNT = ICOUNT + 1
                     END IF
 160              CONTINUE
               IF (LA2.LT.NANT) GO TO 150
 165           CONTINUE
 170        CONTINUE
         END IF
C                                       PSYS
      IF ((DTYPE.NE.1) .AND. (DTYPE.NE.2) .AND. (DTYPE.NE.4) .AND.
     *   (DTYPE.NE.5)) THEN
         ICOUNT = 0
         DO 270 JP = 1,NP
            DO 265 JIF = 1,NIF
               IF (ICOUNT.GT.PRTMAX/2) THEN
                  NCOUNT = NCOUNT + PRTMAX - ICOUNT - 4
                  ICOUNT = 0
                  END IF
               LA2 = 0
 250           LA1 = LA2 + 1
               LA2 = MIN (LA1 + LANT - 1, NANT)
               NCOUNT = NCOUNT + 1
               IF (ICOUNT.NE.0) THEN
                  NCOUNT = NCOUNT + 2
                  ICOUNT = ICOUNT + 2
                  END IF
               ICOUNT = ICOUNT + 1
               DO 260 J = 1,NSC-1
                  WANT = .TRUE.
                  IF ((SSCAN(J).GT.0) .AND. (NSOUWD.GT.0)) THEN
                     WANT = .NOT.DOSWNT
                     DO 251 K = 1,NSOUWD
                        IF (SOUWAN(K).EQ.SSCAN(J)) WANT = DOSWNT
 251                    CONTINUE
                     END IF
                  IF (WANT) THEN
                     NCOUNT = NCOUNT + 1
                     ICOUNT = ICOUNT + 1
                     END IF
 260              CONTINUE
               IF (LA2.LT.NANT) GO TO 250
 265           CONTINUE
 270        CONTINUE
         END IF
      IF (NCOUNT.GT.500) THEN
         TTY(1) = 5
         CALL ZOPEN (TTY(1), TTY(2), 1, SCRSTR, .FALSE., .FALSE.,
     *      .TRUE., IRET)
         MSGTXT = 'PROBLEM OPENING TERMINAL'
         IF (IRET.GT.0) GO TO 990
         WRITE (SCRSTR,1270) NCOUNT
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRSTR, IRET)
         MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
         IF (IRET.GT.0) GO TO 990
         SCRSTR = 'Do you really want to print this much??' //
     *      ' Enter Y or y if so'
         CALL INQSTR (TTY, SCRSTR, 1, CTEMP, IRET)
         IF (IRET.GT.0) GO TO 990
         IF ((CTEMP(:1).NE.'y') .AND. (CTEMP(:1).NE.'Y')) THEN
            SCRSTR = 'Good choice - save trees'
            IRET = -1
         ELSE
            SCRSTR = 'OKAY, printing anyway'
            END IF
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRSTR, I)
         CALL ZCLOSE (TTY(1), TTY(2), I)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1270 FORMAT ('Requested print job is',I10,' lines long!')
      END
      SUBROUTINE PRTCH2 (NP, NIF, NSC, NA, IRET)
C-----------------------------------------------------------------------
C   PRTCH2 checks the display of the data: by ant, IF across, scan down
C   Inputs:
C      NP       I      Number polarizations
C      NIF      I      Number IFs
C      NSC      I      Number scans
C      NA       I      Number antennas
C   Output
C      IRET     I      Error code: -1 -> too many lines for user
C-----------------------------------------------------------------------
      INTEGER   NP, NIF, NSC, NA, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   JP, NACROS, NANT, MXS, K, ANTS(MAXANT), I, J, LIF, LI1,
     *   LI2, ICOUNT, NCOUNT, TTY(2)
      CHARACTER SCRSTR*132, CTEMP*4
      LOGICAL   WANT
      INCLUDE 'REPSYTAB.INC'
      INCLUDE 'PRTSY.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF ((DOCRT.GT.0.0) .OR. (LPNAME.NE.' ')) GO TO 999
      MSGTXT = 'Checking count of lines for direct output to printer'
      CALL MSGWRT (2)
      NCOUNT = 0
C                                       which antennas
      NANT = 0
      DO 10 I = 1,NA
         IF (ARECAN(I).GT.0) THEN
            NANT = NANT + 1
            ANTS(NANT) = I
            END IF
 10      CONTINUE
      NACROS = 132
      LIF = (NACROS - MXS - 10) / 5
C                                       PDIFF
      IF ((DTYPE.NE.2) .AND. (DTYPE.NE.3) .AND. (DTYPE.NE.5) .AND.
     *   (DTYPE.NE.6)) THEN
         ICOUNT = ICOUNT + 1
         DO 70 JP = 1,NP
            DO 65 I = 1,NANT
               IF (ICOUNT.GT.PRTMAX/2) THEN
                  NCOUNT = NCOUNT + PRTMAX - ICOUNT - 4
                  ICOUNT = 0
                  END IF
               LI2 = 0
 50            LI1 = LI2 + 1
               LI2 = MIN (LI1 + LIF - 1, NIF)
               NCOUNT = NCOUNT + 1
               IF (ICOUNT.NE.0) THEN
                  NCOUNT = NCOUNT + 2
                  ICOUNT = ICOUNT + 2
                  END IF
               ICOUNT = ICOUNT + 1
               DO 60 J = 1,NSC-1
                  WANT = .TRUE.
                  IF ((SSCAN(J).GT.0) .AND. (NSOUWD.GT.0)) THEN
                     WANT = .NOT.DOSWNT
                     DO 51 K = 1,NSOUWD
                        IF (SOUWAN(K).EQ.SSCAN(J)) WANT = DOSWNT
 51                     CONTINUE
                     END IF
                  IF (WANT) THEN
                     NCOUNT = NCOUNT + 1
                     ICOUNT = ICOUNT + 1
                     END IF
 60               CONTINUE
               IF (LI2.LT.NIF) GO TO 50
 65            CONTINUE
 70         CONTINUE
         END IF
C                                       PSUM
      IF ((DTYPE.NE.1) .AND. (DTYPE.NE.3) .AND. (DTYPE.NE.4) .AND.
     *   (DTYPE.NE.6)) THEN
         ICOUNT = 0
         DO 170 JP = 1,NP
            DO 165 I = 1,NANT
               IF (ICOUNT.GT.PRTMAX/2) THEN
                  NCOUNT = NCOUNT + PRTMAX - ICOUNT - 4
                  ICOUNT = 0
                  END IF
               LI2 = 0
 150           LI1 = LI2 + 1
               LI2 = MIN (LI1 + LIF - 1, NIF)
               NCOUNT = NCOUNT + 1
               IF (ICOUNT.NE.0) THEN
                  NCOUNT = NCOUNT + 2
                  ICOUNT = ICOUNT + 2
                  END IF
               ICOUNT = ICOUNT + 1
               DO 160 J = 1,NSC-1
                  WANT = .TRUE.
                  IF ((SSCAN(J).GT.0) .AND. (NSOUWD.GT.0)) THEN
                     WANT = .NOT.DOSWNT
                     DO 151 K = 1,NSOUWD
                        IF (SOUWAN(K).EQ.SSCAN(J)) WANT = DOSWNT
 151                    CONTINUE
                     END IF
                  IF (WANT) THEN
                     NCOUNT = NCOUNT + 1
                     ICOUNT =  ICOUNT + 1
                     END IF
 160              CONTINUE
               IF (LI2.LT.NIF) GO TO 150
 165           CONTINUE
 170        CONTINUE
         END IF
C                                       PSYS
      IF ((DTYPE.NE.1) .AND. (DTYPE.NE.2) .AND. (DTYPE.NE.4) .AND.
     *   (DTYPE.NE.5)) THEN
         ICOUNT = 0
         DO 270 JP = 1,NP
            DO 265 I = 1,NANT
               IF (ICOUNT.GT.PRTMAX/2) THEN
                  NCOUNT = NCOUNT + PRTMAX - ICOUNT - 4
                  ICOUNT = 0
                  END IF
               LI2 = 0
 250           LI1 = LI2 + 1
               LI2 = MIN (LI1 + LIF - 1, NIF)
               NCOUNT = NCOUNT + 1
               IF (ICOUNT.NE.0) THEN
                  NCOUNT = NCOUNT + 2
                  ICOUNT = ICOUNT + 2
                  END IF
               ICOUNT = ICOUNT + 1
               DO 260 J = 1,NSC-1
                  WANT = .TRUE.
                  IF ((SSCAN(J).GT.0) .AND. (NSOUWD.GT.0)) THEN
                     WANT = .NOT.DOSWNT
                     DO 251 K = 1,NSOUWD
                        IF (SOUWAN(K).EQ.SSCAN(J)) WANT = DOSWNT
 251                    CONTINUE
                     END IF
                  IF (WANT) THEN
                     NCOUNT = NCOUNT + 1
                     ICOUNT = ICOUNT + 1
                     END IF
 260              CONTINUE
               IF (LI2.LT.NIF) GO TO 250
 265           CONTINUE
 270        CONTINUE
         END IF
      IF (NCOUNT.GT.500) THEN
         TTY(1) = 5
         CALL ZOPEN (TTY(1), TTY(2), 1, SCRSTR, .FALSE., .FALSE.,
     *      .TRUE., IRET)
         MSGTXT = 'PROBLEM OPENING TERMINAL'
         IF (IRET.GT.0) GO TO 990
         WRITE (SCRSTR,1270) NCOUNT
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRSTR, IRET)
         MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
         IF (IRET.GT.0) GO TO 990
         SCRSTR = 'Do you really want to print this much??' //
     *      ' Enter Y or y if so'
         CALL INQSTR (TTY, SCRSTR, 1, CTEMP, IRET)
         IF (IRET.GT.0) GO TO 990
         IF ((CTEMP(:1).NE.'y') .AND. (CTEMP(:1).NE.'Y')) THEN
            SCRSTR = 'Good choice - save trees'
            IRET = -1
         ELSE
            SCRSTR = 'OKAY, printing anyway'
            END IF
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRSTR, I)
         CALL ZCLOSE (TTY(1), TTY(2), I)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1270 FORMAT ('Requested print job is',I10,' lines long!')
      END
      SUBROUTINE PRTCH3 (NP, NIF, NSC, NA, IRET)
C-----------------------------------------------------------------------
C   PRTCH3 does the display of the data: by scan, IF across, ant down
C   Inputs:
C      NP       I      Number polarizations
C      NIF      I      Number IFs
C      NSC      I      Number scans
C      NA       I      Number antennas
C   Output
C      IRET     I      Error code: -1 -> too many lines for user
C-----------------------------------------------------------------------
      INTEGER   NP, NIF, NSC, NA, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   JP, NACROS, NANT, K, ANTS(MAXANT), I, J, LIF, LI1, LI2,
     *   NCOUNT, TTY(2), ICOUNT
      CHARACTER SCRSTR*132, CTEMP*4
      LOGICAL   WANT
      INCLUDE 'REPSYTAB.INC'
      INCLUDE 'PRTSY.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF ((DOCRT.GT.0.0) .OR. (LPNAME.NE.' ')) GO TO 999
      MSGTXT = 'Checking count of lines for direct output to printer'
      CALL MSGWRT (2)
      NCOUNT = 0
C                                       which antennas
      NANT = 0
      DO 10 I = 1,NA
         IF (NRECAN(I).GT.0) THEN
            NANT = NANT + 1
            ANTS(NANT) = I
            END IF
 10      CONTINUE
      NACROS = 132
      LIF = (NACROS - 1) / 5
C                                       PDIFF
      IF ((DTYPE.NE.2) .AND. (DTYPE.NE.3) .AND. (DTYPE.NE.5) .AND.
     *   (DTYPE.NE.6)) THEN
         ICOUNT = 0
         DO 70 JP = 1,NP
            DO 65 J = 1,NSC-1
               WANT = .TRUE.
               IF ((SSCAN(J).GT.0) .AND. (NSOUWD.GT.0)) THEN
                  WANT = .NOT.DOSWNT
                  DO 45 K = 1,NSOUWD
                     IF (SOUWAN(K).EQ.SSCAN(J)) WANT = DOSWNT
 45                  CONTINUE
                  END IF
               IF (.NOT.WANT) GO TO 65
               IF (ICOUNT.GT.PRTMAX/2) THEN
                  NCOUNT = NCOUNT + PRTMAX - ICOUNT - 4
                  ICOUNT = 0
                  END IF
               LI2 = 0
 50            LI1 = LI2 + 1
               LI2 = MIN (LI1 + LIF - 1, NIF)
               NCOUNT = NCOUNT + 1 + NANT
               IF (ICOUNT.NE.0) ICOUNT = ICOUNT + 2
               ICOUNT = ICOUNT + 1 + NANT
               IF (LI2.LT.NIF) GO TO 50
 65            CONTINUE
 70         CONTINUE
         END IF
C                                       PSUM
      IF ((DTYPE.NE.1) .AND. (DTYPE.NE.3) .AND. (DTYPE.NE.4) .AND.
     *   (DTYPE.NE.6)) THEN
         NCOUNT = NCOUNT + PRTMAX - ICOUNT - 4
         ICOUNT = 0
         DO 170 JP = 1,NP
            DO 165 J = 1,NSC-1
               WANT = .TRUE.
               IF ((SSCAN(J).GT.0) .AND. (NSOUWD.GT.0)) THEN
                  WANT = .NOT.DOSWNT
                  DO 145 K = 1,NSOUWD
                     IF (SOUWAN(K).EQ.SSCAN(J)) WANT = DOSWNT
 145                 CONTINUE
                  END IF
               IF (.NOT.WANT) GO TO 165
               IF (ICOUNT.GT.PRTMAX/2) THEN
                  NCOUNT = NCOUNT + PRTMAX - ICOUNT - 4
                  ICOUNT = 0
                  END IF
               LI2 = 0
 150           LI1 = LI2 + 1
               LI2 = MIN (LI1 + LIF - 1, NIF)
               IF (ICOUNT.NE.0) ICOUNT = ICOUNT + 2
               NCOUNT = NCOUNT + 1 + NANT
               ICOUNT = ICOUNT + 1 + NANT
               IF (LI2.LT.NIF) GO TO 150
 165           CONTINUE
 170        CONTINUE
         END IF
C                                       PSYS
      IF ((DTYPE.NE.1) .AND. (DTYPE.NE.2) .AND. (DTYPE.NE.4) .AND.
     *   (DTYPE.NE.5)) THEN
         NCOUNT = NCOUNT + PRTMAX - ICOUNT - 4
         ICOUNT = 0
         DO 270 JP = 1,NP
            DO 265 J = 1,NSC-1
               WANT = .TRUE.
               IF ((SSCAN(J).GT.0) .AND. (NSOUWD.GT.0)) THEN
                  WANT = .NOT.DOSWNT
                  DO 245 K = 1,NSOUWD
                     IF (SOUWAN(K).EQ.SSCAN(J)) WANT = DOSWNT
 245                 CONTINUE
                  END IF
               IF (.NOT.WANT) GO TO 265
               IF (ICOUNT.GT.PRTMAX/2) THEN
                  NCOUNT = NCOUNT + PRTMAX - ICOUNT - 4
                  ICOUNT = 0
                  END IF
               LI2 = 0
 250           LI1 = LI2 + 1
               LI2 = MIN (LI1 + LIF - 1, NIF)
               IF (ICOUNT.NE.0) ICOUNT = ICOUNT + 2
               NCOUNT = NCOUNT + 1 + NANT
               ICOUNT = ICOUNT + 1 + NANT
               IF (LI2.LT.NIF) GO TO 250
 265           CONTINUE
 270        CONTINUE
         END IF
C                                       check count
      IF (NCOUNT.GT.500) THEN
         TTY(1) = 5
         CALL ZOPEN (TTY(1), TTY(2), 1, SCRSTR, .FALSE., .FALSE.,
     *      .TRUE., IRET)
         MSGTXT = 'PROBLEM OPENING TERMINAL'
         IF (IRET.GT.0) GO TO 990
         WRITE (SCRSTR,1270) NCOUNT
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRSTR, IRET)
         MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
         IF (IRET.GT.0) GO TO 990
         SCRSTR = 'Do you really want to print this much??' //
     *      ' Enter Y or y if so'
         CALL INQSTR (TTY, SCRSTR, 1, CTEMP, IRET)
         IF (IRET.GT.0) GO TO 990
         IF ((CTEMP(:1).NE.'y') .AND. (CTEMP(:1).NE.'Y')) THEN
            SCRSTR = 'Good choice - save trees'
            IRET = -1
         ELSE
            SCRSTR = 'OKAY, printing anyway'
            END IF
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRSTR, I)
         CALL ZCLOSE (TTY(1), TTY(2), I)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1270 FORMAT ('Requested print job is',I10,' lines long!')
      END
      SUBROUTINE PRTCH4 (NP, NIF, NSC, NA, SYAVG, IRET)
C-----------------------------------------------------------------------
C   PRTCH4 checks the display of the data: by source, IF across,
C   ant down
C   Inputs:
C      NP       I      Number polarizations
C      NIF      I      Number IFs
C      NSC      I      Number sources
C      NA       I      Number antennas
C   Output
C      IRET     I      Error code: -1 -> too many lines for user
C-----------------------------------------------------------------------
      INTEGER   NP, NIF, NSC, NA, IRET
      REAL      SYAVG(6,NP,NIF,NA,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   JP, NACROS, NANT, ANTS(MAXANT), I, J, LIF, LI1, LI2,
     *   IROUND, SR, NCOUNT, TTY(2), ICOUNT, JO, JIF
      REAL      T, U, V
      CHARACTER SCRSTR*132, CTEMP*4
      INCLUDE 'REPSYTAB.INC'
      INCLUDE 'PRTSY.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF ((DOCRT.GT.0.0) .OR. (LPNAME.NE.' ')) GO TO 999
      MSGTXT = 'Checking count of lines for direct output to printer'
      CALL MSGWRT (2)
      NCOUNT = 0
      NACROS = 132
      LIF = (NACROS - 1) / 5
      SR = IROUND (FACTOR(7))
      JO = 0
      IF (DTYPE.GT.3) JO = 3
      CALL FILL (NA, 0, NRECAN)
      DO 40 J = 1,NSC
         DO 35 I = 1,NA
            DO 30 JIF = 1,NIF
               DO 25 JP = 1,NP
                  U = SYAVG(1+JO,JP,JIF,I,J)
                  V = SYAVG(2+JO,JP,JIF,I,J)
                  T = SYAVG(3+JO,JP,JIF,I,J)
                  IF ((U.NE.FBLANK) .OR. (V.NE.FBLANK) .OR.
     *               (T.NE.FBLANK)) THEN
                     NRECAN(I) = NRECAN(I) + 1
                     END IF
 25               CONTINUE
 30            CONTINUE
 35         CONTINUE
 40      CONTINUE
C                                       which antennas
      NANT = 0
      DO 10 I = 1,NA
         IF (NRECAN(I).GT.0) THEN
            NANT = NANT + 1
            ANTS(NANT) = I
            END IF
 10      CONTINUE
C                                       PDIFF
      IF ((DTYPE.NE.2) .AND. (DTYPE.NE.3) .AND. (DTYPE.NE.5) .AND.
     *   (DTYPE.NE.6)) THEN
         ICOUNT = 0
         DO 70 JP = 1,NP
            DO 65 J = 1,NSC
               IF (SSCAN(J).EQ.SR) GO TO 65
               IF (ICOUNT.GT.PRTMAX/2) THEN
                  NCOUNT = NCOUNT + PRTMAX - ICOUNT - 4
                  ICOUNT = 0
                  END IF
               LI2 = 0
 50            LI1 = LI2 + 1
               LI2 = MIN (LI1 + LIF - 1, NIF)
               IF (ICOUNT.NE.0) ICOUNT = ICOUNT + 2
               NCOUNT = NCOUNT + NANT + 1
               ICOUNT = ICOUNT + NANT + 1
               IF (LI2.LT.NIF) GO TO 50
 65            CONTINUE
 70         CONTINUE
         END IF
C                                       PSUM
      IF ((DTYPE.NE.1) .AND. (DTYPE.NE.3) .AND. (DTYPE.NE.4) .AND.
     *   (DTYPE.NE.6)) THEN
         ICOUNT = 0
         DO 170 JP = 1,NP
            DO 165 J = 1,NSC
               IF (SSCAN(J).EQ.SR) GO TO 165
               IF (ICOUNT.GT.PRTMAX/2) THEN
                  NCOUNT = NCOUNT + PRTMAX - ICOUNT - 4
                  ICOUNT = 0
                  END IF
               LI2 = 0
 150           LI1 = LI2 + 1
               LI2 = MIN (LI1 + LIF - 1, NIF)
               IF (ICOUNT.NE.0) ICOUNT = ICOUNT + 2
               NCOUNT = NCOUNT + 1 + NANT
               ICOUNT = ICOUNT + 1 + NANT
               IF (LI2.LT.NIF) GO TO 150
 165           CONTINUE
 170        CONTINUE
         END IF
C                                       PSYS
      IF ((DTYPE.NE.1) .AND. (DTYPE.NE.2) .AND. (DTYPE.NE.4) .AND.
     *   (DTYPE.NE.5)) THEN
         ICOUNT = 0
         DO 270 JP = 1,NP
            DO 265 J = 1,NSC
               IF (SSCAN(J).EQ.SR) GO TO 265
               IF (ICOUNT.GT.PRTMAX/2) THEN
                  NCOUNT = NCOUNT + PRTMAX - ICOUNT - 4
                  ICOUNT = 0
                  END IF
               LI2 = 0
 250           LI1 = LI2 + 1
               LI2 = MIN (LI1 + LIF - 1, NIF)
               IF (ICOUNT.NE.0) ICOUNT = ICOUNT + 2
               NCOUNT = NCOUNT + 1 + NANT
               ICOUNT = ICOUNT + 1 + NANT
               IF (LI2.LT.NIF) GO TO 250
 265           CONTINUE
 270        CONTINUE
         END IF
C                                       check count
      IF (NCOUNT.GT.500) THEN
         TTY(1) = 5
         CALL ZOPEN (TTY(1), TTY(2), 1, SCRSTR, .FALSE., .FALSE.,
     *      .TRUE., IRET)
         MSGTXT = 'PROBLEM OPENING TERMINAL'
         IF (IRET.GT.0) GO TO 990
         WRITE (SCRSTR,1270) NCOUNT
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRSTR, IRET)
         MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
         IF (IRET.GT.0) GO TO 990
         SCRSTR = 'Do you really want to print this much??' //
     *      ' Enter Y or y if so'
         CALL INQSTR (TTY, SCRSTR, 1, CTEMP, IRET)
         IF (IRET.GT.0) GO TO 990
         IF ((CTEMP(:1).NE.'y') .AND. (CTEMP(:1).NE.'Y')) THEN
            SCRSTR = 'Good choice - save trees'
            IRET = -1
         ELSE
            SCRSTR = 'OKAY, printing anyway'
            END IF
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRSTR, I)
         CALL ZCLOSE (TTY(1), TTY(2), I)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1270 FORMAT ('Requested print job is',I10,' lines long!')
      END
