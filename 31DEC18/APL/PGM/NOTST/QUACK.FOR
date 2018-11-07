LOCAL INCLUDE 'QUACK.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXSTOK, XOPCOD,
     *   XREAS(6)
      REAL      XSIN, XDISIN, XSUBAR, xbif, xeif, XBAND, XFREQ, XFQID,
     *   TIMER(8), XANTS(50), XBASE(50), XFGVER, APARM(10), BADD(10),
     *   BUFF1(4096), BUFF2(4096), TBEG, TEND
      INTEGER   SEQIN, DISKIN, SUBAR, FGVER, NANT, ANTS(50), BIF, EIF,
     *   NID, ID(300), NUMHIS, JBUFSZ, FRQSEL, NUMAN(513)
      INTEGER MAXBAS
      PARAMETER (MAXBAS = 500)
      INTEGER XA1(MAXBAS), XA2(MAXBAS), NBASE
C
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, OPCODE*4, REASON*24,
     *   HISCRD(10)*64, STOKES*4
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSTOK, XBIF, XEIF,
     *   XXSOUR, XSUBAR, XBAND, XFREQ, XFQID,TIMER, XANTS, XBASE,
     *   XFGVER, XOPCOD, XREAS, APARM, BADD,
     *   TBEG, TEND, SEQIN, DISKIN, SUBAR, FGVER, NANT, ANTS, NID, ID,
     *   NUMHIS, FRQSEL, NUMAN, XA1, XA2, NBASE, BIF, EIF
      COMMON /CHARPM/ NAMEIN, CLAIN, XSOUR, OPCODE, REASON, HISCRD,
     *   STOKES
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
LOCAL END
      PROGRAM QUACK
C-----------------------------------------------------------------------
C! Flags a selected portion of each specified scan in a uv data set
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-1999, 2002, 2006-2007, 2011-2012, 2016,
C;  Copyright (C) 2018
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
C   QUACK flags a selected portion of scans in uv data.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      SOURCES        XSOUR         Sources selected/deselected
C      SUBARRAY       SUBAR         Subarray number 0=>all.
C      SELBAND      SELBAN      Bandwidth in FQ table
C      SELFREQ      SELFRQ      Freq in FQ table
C      FREQID       FFQID      Freq ID to flag
C      TIMERANG       TIMER         timerange to consider
C      ANTENNAS       ANTS          Antennas to flag
C      FLAGVER        FGVER         Flag table version number
C      OPCODE         OPCODE        Opcode
C      REASON         REASON        Reason for flagging
C      APARM(10)      APARM         Control values
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER IRET
      INCLUDE 'QUACK.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'QUACK '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL QUAKIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Flag data
      CALL QUAKUV (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL QUAKHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE QUAKIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   QUAKIN gets input parameters for QUACK.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                6 => invalid inputs
C                                7 = No data selected.
C                                8 => can't start
C   Output in common:
C      NID     I      Number of values in ID
C      ID      I(300) Source IDs selected
C      NANT    I      Number of values in ANTS
C      ANTS    I(50)  Number of antennas selected.
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER  STAT*4, PTYPE*2, CTIME*8, CDATE*12
      INTEGER   OLDCNO, IROUND, NPARM, I, IERR, QUAL(30), NSOUR,
     *   LUNFQ, LUNAN, IERANT, IFGVER, DATE(3), TIME(3)
      LOGICAL   T, NOSOUR, MATCH
      REAL      SELBAN
      DOUBLE PRECISION SELFRQ
      INCLUDE 'QUACK.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA QUAL /30*-1/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T, BUFF1)
      CALL VHDRIN
      JBUFSZ = 4096 * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 270
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      CALL H2CHR (4, 1, XXSTOK, STOKES)
      CALL H2CHR (24, 1, XREAS, REASON)
      IF (REASON.EQ.' ') THEN
         CALL ZDATE (DATE)
         CALL ZTIME (TIME)
         DATE(1) = -DATE(1)
         CALL TIMDAT (TIME, DATE, CTIME, CDATE)
         WRITE (REASON,1001) TSKNAM, CDATE(:9), CTIME
         END IF
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
      NOSOUR = .TRUE.
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
         NOSOUR = NOSOUR .AND. (XSOUR(I).EQ.' ')
 20      CONTINUE
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      SUBAR = IROUND (XSUBAR)
      FGVER = IROUND (XFGVER)
      BIF = XBIF + 0.1
      EIF = XEIF + 0.1
C                                       Defaults

      IF (OPCODE.EQ.' ') OPCODE = 'BEG '
      IF (APARM(1).LE.0.000001) APARM(1) = 2.0
      IF (APARM(2).LE.0.000001) APARM(2) = 0.1
C                                       Validity check.
      IF (((OPCODE.NE.'END ') .AND. (OPCODE.NE.'BEG ')) .AND.
     *   (OPCODE.NE.'ENDB') .AND. (OPCODE.NE.'TAIL')) THEN
         MSGTXT = 'UNKNOWN OPCODE = ' // OPCODE
         JERR = 6
         GO TO 990
         END IF
C                                       Get CATBLK from file.
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'WRIT', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
      ELSE
         BIF = MAX (1, BIF)
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         END IF
C                                       Determine FG max
      CALL FNDEXT ('FG', CATBLK, IFGVER)
      IF (FGVER.LE.0) FGVER = MAX (1, IFGVER)
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.LT.0) FRQSEL = -1
      IF (FRQSEL.GE.0) THEN
         LUNFQ = 40
         CALL FQMATC (DISKIN, OLDCNO, CATBLK, LUNFQ, SELBAN, SELFRQ,
     *      MATCH, FRQSEL, JERR)
         IF (.NOT.MATCH) THEN
            WRITE (MSGTXT,1070)
            JERR = 1
            GO TO 990
            END IF
         IF (JERR.GT.0) GO TO 999
         END IF
C                                       Look up sources selected.
      IF (NOSOUR) THEN
         NID = 0
         ID(1) = 0
      ELSE
         NSOUR = 30
         NID = 300
         CALL SOURNU (XSOUR, QUAL, NSOUR, DISKIN, OLDCNO, NID,
     *      BUFF1, ID, JERR)
         IF (JERR.GT.0) GO TO 999
C                                       Check that sources found.
         IF (JERR.LT.0) THEN
            JERR = 7
            MSGTXT = 'NONE OF THE SPECIFIED SOURCES WERE FOUND'
            GO TO 990
            END IF
         END IF
C                                       Get number(s) of antennas.
C                                       Since GETNAN will return
C                                       a reasonable default -
C                                       ignore error code.
      LUNAN = 30
      CALL GETNAN (DISKIN, OLDCNO, CATBLK, LUNAN, BUFF1, NUMAN, IERANT)
C                                       Convert to baseline antennas.
      NBASE = MAXBAS
      CALL AN10RS (NUMAN, SUBAR, XANTS, XBASE, NBASE, XA1, XA2)
C
      JERR = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('QUAKIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1001 FORMAT (A6,A9,1X,A8)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
      END
      SUBROUTINE QUAKUV (IRET)
C-----------------------------------------------------------------------
C   QUAKUV writes entries in the specified FG table.
C   routine and then writes the modified data if requested.
C   Input in common:
C      FGVER   I      FG version number
C      NID     I      Number of values in ID
C      ID      I(300) Source IDs selected
C      NANT    I      Number of values in ANTS
C      ANTS    I(50)  Number of antennas selected.
C   Output:
C      IRET   I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   LUNNX, LUNFG, BUFFNX(4096), BUFFFG(4096), INXRNO,
     *   IFGRNO, NUMNX, NXVER, ISCAN, IDSOUR, SUBA, VSTART, VEND, FQID,
     *   IBASL, ANT(2), IFS(2), CHANS(2), LOOP, NUMFLG, FGSOUR
      LOGICAL   PFLAGS(4)
      REAL      TIME, DTIME, TMR(2), TIMOFF, TIMBEG, TIMEND, SUMFLG,
     *   BACKS
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NXKOLS(MAXNXC), NXNUMV(MAXNXC), FGKOLS(MAXFGC),
     *   FGNUMV(MAXFGC)
      CHARACTER LSTOK*4
      INCLUDE 'QUACK.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (BUFFNX, BUFF1), (BUFFFG, BUFF2)
      DATA LUNNX, LUNFG /25, 26/
C-----------------------------------------------------------------------
C                                       Explicit initialisation.
      ANT(1) = 0
      ANT(2) = 0
      IFS(1) = BIF
      IFS(2) = EIF
      CHANS(1) = 1
      CHANS(2) = 0
C                                       stokes
      LSTOK = STOKES
      IF ((STOKES.EQ.' ') .OR. (STOKES.EQ.'FULL')) THEN
         LSTOK = '1111'
      ELSE IF ((INDEX(STOKES,'0').EQ.0) .AND. (INDEX(STOKES,'1').EQ.0))
     *   THEN
         LSTOK = '0000'
         IF (ICOR0.LT.-4) THEN
            IF (STOKES.EQ.'HALF') THEN
               LSTOK = '1100'
            ELSE
               IF (MOD(INDEX(STOKES,'XX'),2).GT.0) LSTOK(1:1) = '1'
               IF (MOD(INDEX(STOKES,'YY'),2).GT.0) LSTOK(2:2) = '1'
               IF (MOD(INDEX(STOKES,'XY'),2).GT.0) LSTOK(3:3) = '1'
               IF (MOD(INDEX(STOKES,'YX'),2).GT.0) LSTOK(4:4) = '1'
               END IF
         ELSE IF (ICOR0.LT.0) THEN
            IF (STOKES.EQ.'HALF') THEN
               LSTOK = '1100'
            ELSE
               IF (MOD(INDEX(STOKES,'RR'),2).GT.0) LSTOK(1:1) = '1'
               IF (MOD(INDEX(STOKES,'LL'),2).GT.0) LSTOK(2:2) = '1'
               IF (MOD(INDEX(STOKES,'RL'),2).GT.0) LSTOK(3:3) = '1'
               IF (MOD(INDEX(STOKES,'LR'),2).GT.0) LSTOK(4:4) = '1'
               END IF
         ELSE
            IF (INDEX(STOKES,'I').GT.0) LSTOK(1:1) = '1'
            IF (INDEX(STOKES,'Q').GT.0) LSTOK(2:2) = '1'
            IF (INDEX(STOKES,'U').GT.0) LSTOK(3:3) = '1'
            IF (INDEX(STOKES,'V').GT.0) LSTOK(4:4) = '1'
            END IF
         END IF
      DO 20 LOOP = 1, 4
        PFLAGS(LOOP) = LSTOK(LOOP:LOOP).EQ.'1'
  20    CONTINUE
C                                       Timerange
      TBEG = TIMER(1) + (TIMER(2) / 24.0) + (TIMER(3) / (24.0 * 60.0)) +
     *   (TIMER(4) / (24.0 * 3600.0))
      TEND = TIMER(5) + (TIMER(6) / 24.0) + (TIMER(7) / (24.0 * 60.0)) +
     *   (TIMER(8) / (24.0 * 3600.0))
C                                       All times
      IF (TBEG.LE.0.0000001) TBEG = -1.E5
      IF ((TEND.LE.0.0000001) .OR. (TBEG.GT.TEND)) TEND = 1.0E5
      TIMOFF = APARM(1) / (24.0 * 60.0)
      TIMBEG = APARM(2) / (24.0 * 60.0)
      TIMEND = TIMBEG
C                                       Back start .5 sec or nothing
      BACKS = 5.78704E-6
      IF (APARM(3).LT.-0.0001) THEN
         BACKS = 0.
      ELSE IF (APARM(3) .GT. 0.0001) THEN
         BACKS = APARM(3) / 24. / 60. / 60.
         END IF
      SUMFLG = 0.0
      NUMFLG = 0
C                                       Open NX table
      NXVER = 1
      CALL NDXINI ('READ', BUFFNX, FVOL(NCFILE), FCNO(NCFILE), NXVER,
     *   CATBLK, LUNNX, INXRNO, NXKOLS, NXNUMV, IRET)
C                                       Table present?
      IF (IRET.EQ.2) THEN
         MSGTXT = 'USE TASK INDXR TO CREATE AN INDEX (NX) TABLE'
         GO TO 990
         END IF
      IF (IRET.NE.0) GO TO 999
      NUMNX = BUFFNX(5)
C                                       Open FG table
      CALL FGREFM (FVOL(NCFILE), FCNO(NCFILE), FGVER, CATBLK, LUNFG,
     *   IRET)
      CALL FLGINI ('WRIT', BUFFFG, FVOL(NCFILE), FCNO(NCFILE), FGVER,
     *   CATBLK, LUNFG, IFGRNO, FGKOLS, FGNUMV, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Loop over scans
      DO 500 ISCAN = 1,NUMNX
         INXRNO = ISCAN
         CALL TABNDX ('READ', BUFFNX, INXRNO, NXKOLS, NXNUMV, TIME,
     *      DTIME, IDSOUR, SUBA, VSTART, VEND, FQID, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Want this one?
         IF (((TIME-0.5*DTIME).LT.TBEG) .OR.
     *      ((TIME+0.5*DTIME).GT.TEND) .OR. ((SUBA.NE.SUBAR) .AND.
     *      (SUBAR.GT.0) .AND. (SUBA.GT.0))) GO TO 500
         IF ((FRQSEL.GT.0) .AND. (FQID.GT.0) .AND. (FQID.NE.FRQSEL))
     *      GO TO 500
         IF (NID.GT.0) THEN
            DO 50 LOOP = 1,NID
               IF (IDSOUR.EQ.ID(LOOP)) GO TO 60
 50            CONTINUE
            GO TO 500
            END IF
C                                       Set flag time range
 60      FGSOUR = IDSOUR
C                                       End good.
         IF (OPCODE.EQ.'END ') THEN
            TMR(1) = TIME - 0.5 * DTIME
C                                       Back start time off by BACKS sec.
            TMR(1) = TMR(1) - BACKS
            TMR(2) = TIME + 0.5 * DTIME - TIMOFF
C                                       Beginning bad.
         ELSE IF (OPCODE.EQ.'BEG ') THEN
            TMR(1) = TIME - 0.5 * DTIME
            TMR(2) = TMR(1) + TIMBEG
C                                       Back start time off by BACKS sec.
            TMR(1) = TMR(1) - BACKS
C                                       end bad.
         ELSE IF (OPCODE.EQ.'ENDB') THEN
            TMR(2) = TIME + 0.5 * DTIME
            TMR(1) = TMR(2) - TIMEND
C                                       Stop time later by BACKS sec.
            TMR(2) = TMR(2) + BACKS
C                                       Time past end bad
         ELSE IF (OPCODE.EQ.'TAIL') THEN
            FGSOUR = 0
            TMR(1) = TIME + 0.5 * DTIME
            TMR(2) = TMR(1) + TIMEND
            TMR(1) = TMR(1) + BACKS
            END IF
         IF (TMR(2).LE.TMR(1)) GO TO 500
C                                       Record time flagged
         SUMFLG = SUMFLG + (TMR(2) - TMR(1)) * 1440.0
         NUMFLG = NUMFLG + 1
C                                       Deal with antennas-baselines
C                                       selected
C                                       ANTENNAS=BASELINE=0
         IF (NBASE.EQ.1 .AND. (XA1(1).EQ.0 .AND. XA2(1).EQ.0)) THEN
            ANT(1) = 0
            ANT(2) = 0
C                                       Write table
            CALL TABFLG ('WRIT', BUFFFG, IFGRNO, FGKOLS, FGNUMV, FGSOUR,
     *         SUBA, FRQSEL, ANT, TMR, IFS, CHANS, PFLAGS, REASON, IRET)
            IF (IRET.NE.0) GO TO 999
         ELSE
            DO 100 IBASL = 1, NBASE
               ANT(1) = XA1(IBASL)
               ANT(2) = XA2(IBASL)
C                                       Write table
               CALL TABFLG ('WRIT', BUFFFG, IFGRNO, FGKOLS, FGNUMV,
     *            FGSOUR, SUBA, FRQSEL, ANT, TMR, IFS, CHANS, PFLAGS,
     *            REASON, IRET)
               IF (IRET.NE.0) GO TO 999
 100           CONTINUE
            END IF
 500     CONTINUE
C                                       Tell how much flagged
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),1500) SUMFLG, NUMFLG
      MSGTXT = HISCRD(NUMHIS)
      CALL MSGWRT (4)
C                                       Mark FG table as unsorted
      BUFFFG(43) = 0
      BUFFFG(44) = 0
C                                       Close tables.
      CALL TABIO ('CLOS', 0, INXRNO, BUFFNX, BUFFNX, IRET)
      CALL TABIO ('CLOS', 0, IFGRNO, BUFFFG, BUFFFG, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1500 FORMAT ('Flagged ',F8.2,' min. of data in ',I5,' scans')
      END
      SUBROUTINE QUAKHI
C-----------------------------------------------------------------------
C   QUAKHI updates history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, LABEL*8, CTIME*8, CDATE*12
      INTEGER   LUN, IERR, I, DATE(3), TIME(3), IC1, IC2
      LOGICAL   T
      INCLUDE 'QUACK.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Open history file.
      CALL HIOPEN (LUN, FVOL(NCFILE), FCNO(NCFILE), BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME, CDATE)
      WRITE (HILINE,1010) TSKNAM, RLSNAM, CDATE, CTIME
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       OPCODE
      WRITE (HILINE,2000) TSKNAM, OPCODE
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Sources
      IF (NID.LE.0) THEN
         WRITE (HILINE,3000) TSKNAM
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
      ELSE
C                                       1st 2 and label.
         WRITE (HILINE,3003) TSKNAM, XSOUR(1), XSOUR(2)
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       Rest of sources
         IF (NID.GT.2) THEN
            DO 20 I = 3,NID,2
               WRITE (HILINE,3004) TSKNAM, XSOUR(I), XSOUR(I+1)
               CALL HIADD (LUN, HILINE, BUFF1, IERR)
               IF (IERR.NE.0) GO TO 200
 20            CONTINUE
            END IF
         END IF
C                                       TIMERANG
      CALL HITIME (TBEG, TEND, LUN, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       ANTENNAS
      IC1 = MIN (15, NANT)
      IC1 = MAX (1, IC1)
      IF (NANT.GE.0) THEN
         WRITE (HILINE,3005) TSKNAM, (ANTS(IC2), IC2=1,IC1)
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      IF (NANT.GT.15) THEN
         IC1 = MIN (30, NANT)
         WRITE (HILINE,3006) TSKNAM, (ANTS(IC2), IC2=16,IC1)
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      IF (NANT.GT.30) THEN
         IC1 = MIN (45, NANT)
         WRITE (HILINE,3006) TSKNAM, (ANTS(IC2), IC2=31,IC1)
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      IF (NANT.GT.45) THEN
         IC1 = MIN (50, NANT)
         WRITE (HILINE,3006) TSKNAM, (ANTS(IC2), IC2=46,IC1)
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Subarray
         WRITE (HILINE,2003) TSKNAM, SUBAR
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       FQ ID
         WRITE (HILINE,2008) TSKNAM, FRQSEL
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       FG version
         WRITE (HILINE,2004) TSKNAM, FGVER
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       REASON
         WRITE (HILINE,2005) TSKNAM, REASON
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       APARM
         IF (OPCODE.EQ.'END ') THEN
            WRITE (HILINE,2006) TSKNAM, APARM(1)
         ELSE IF (OPCODE.EQ.'BEG ') THEN
            WRITE (HILINE,2007) TSKNAM, APARM(2)
         ELSE IF (OPCODE.EQ.'ENDB') THEN
            WRITE (HILINE,2010) TSKNAM, APARM(2)
            END IF
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                      Add any other history.
      IF (NUMHIS.LE.0) GO TO 200
         LABEL = TSKNAM // '  '
         DO 50 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
 50         CONTINUE
C                                       Close HI file
 200  CALL HICLOS (LUN, T, BUFF1, IERR)
C                                        Update CATBLK.
      CALL CATIO ('UPDT', FVOL(NCFILE), FCNO(NCFILE), CATBLK, 'REST',
     *   BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('QUAKHI: ERROR',I3,' OPENING HISTORY FILE')
 1010 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 2000 FORMAT (A6,' OPCODE =''',A4,'''')
 2003 FORMAT (A6,' SUBARRAY =',I4)
 2004 FORMAT (A6,' FGVER =',I4,' / FG table')
 2005 FORMAT (A6,' REASON = ''',A,''' / Reason for flagging')
 2006 FORMAT (A6,' APARM(1) =',F10.5,' / Good data at end of scan')
 2007 FORMAT (A6,' APARM(2) =',F10.5,' / Bad data at start of scan')
 2008 FORMAT (A6,' Freq. ID = ',I4)
 2010 FORMAT (A6,' APARM(2) =',F10.5,' / Bad data at end of scan')
 3000 FORMAT (A6,' SOURCES = '' '' / All sources')
 3003 FORMAT (A6,' SOURCES = ''',A16,''',''',A16,'''')
 3004 FORMAT (A6,'          ,''',A16,''',''',A16,'''')
 3005 FORMAT (A6, ' ANTENNAS = ',15(I2,','))
 3006 FORMAT (A6, '            ',15(I2,','))
      END

