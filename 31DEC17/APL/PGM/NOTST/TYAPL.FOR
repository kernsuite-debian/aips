LOCAL INCLUDE 'TYAPL.INC'
C                                       Local include for TYAPL
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2), XXTYPE,
     *   XOPTYP, XCALIN(12)
      REAL      XSIN, XDISIN, XFQID, XSUBA, XFLAG, XDOFLG, XINV, XINV2,
     *   XSOUT, XDISO, REWAY(2), DOWAY, CUTOFF, XCENT,
     *   SCRBUF(256), BUFF2(UVBFSS), CURTIM, DIFPIX
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ, ILOCWT,
     *   CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO,
     *   LRECO, NRPRMI, NRPRMO, OLDCNO, NEWCNO, INVER1, INVER2, NANT,
     *   NIF, NPOL, NMSG, EXTRAP, CLVERI
      LOGICAL   ISCOMP, DOGAIN
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, XTYPE*2,
     *   OPTYPE*4, CALIN*48
      DOUBLE PRECISION JD, JD0
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XFQID, XSUBA, XFLAG,
     *   XDOFLG, XXTYPE, XINV, XINV2, XNAMOU, XCLAOU, XSOUT, XDISO,
     *   XOPTYP, REWAY, DOWAY, CUTOFF, XCENT, XCALIN
      COMMON /TYAPLP/ CATOLD, JD, JD0, SEQIN, SEQOUT, DISKIN, DISKO,
     *   ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECO,
     *   NRPRMI, NRPRMO, ISCOMP, OLDCNO, NEWCNO, INVER1, INVER2, CURTIM,
     *   NANT, NIF, NPOL, NMSG, EXTRAP, CLVERI, DOGAIN, DIFPIX
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, XTYPE, OPTYPE,
     *   CALIN
      COMMON /BUFRS/ SCRBUF, BUFF2, JBUFSZ
C                                       End local include for TYAPL
LOCAL END
LOCAL INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:PUVD.INC'
C                                       set flag parameters
      INTEGER   MAXFLG
C                                       MAXFLG= max. no. flags active
      PARAMETER (MAXFLG=600001)
LOCAL END
LOCAL INCLUDE 'TYTABS.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PTYTAB.INC'
      INTEGER   TYBUF1(512), TYBUF2(512), TYRNO1, TYRNO2, TYKOL1(13),
     *   TYNUM1(13), TYKOL2(13), TYNUM2(13), LSTSOU, MKRNO1, MKRNO2,
     *   TREC1A(MAXANT), TREC1B(MAXANT), TREC2A(MAXANT), TREC2B(MAXANT),
     *   SKIP1(2,MAXIF,MAXANT), SKIP2(2,MAXIF,MAXANT), ASKIP1(MAXANT),
     *   ASKIP2(MAXANT), HSKIP1(MAXANT), HSKIP2(MAXANT)
      REAL      TS1A(2,MAXIF,MAXANT), TS1B(2,MAXIF,MAXANT),
     *   TS2A(2,MAXIF,MAXANT), TS2B(2,MAXIF,MAXANT), T1A(MAXANT),
     *   T1B(MAXANT), T2A(MAXANT), T2B(MAXANT),
     *   TCAL(4,MAXIF,MAXANT), PD1A(2,MAXIF,MAXANT),
     *   PD1B(2,MAXIF,MAXANT), PD2A(2,MAXIF,MAXANT),
     *   PD2B(2,MAXIF,MAXANT), PG1A(2,MAXIF,MAXANT),
     *   PG1B(2,MAXIF,MAXANT), PG2A(2,MAXIF,MAXANT),
     *   PG2B(2,MAXIF,MAXANT), AVTS1A(2,MAXIF), AVTS1B(2,MAXIF),
     *   AVTS2A(2,MAXIF), AVTS2B(2,MAXIF), AVPD1A(2,MAXIF),
     *   AVPD1B(2,MAXIF), AVPD2A(2,MAXIF), AVPD2B(2,MAXIF),
     *   AVPG1A(2,MAXIF), AVPG1B(2,MAXIF), AVPG2A(2,MAXIF),
     *   AVPG2B(2,MAXIF), VEFF(2,MAXIF,MAXANT)
      COMMON /TYVALS/ TYBUF1, TYBUF2, TS1A, TS1B, TS2A, TS2B, PD1A,
     *   PD1B, PD2A, PD2B, TCAL, T1A, T1B, T2A, T2B, PG1A, PG1B, PG2A,
     *   PG2B, TYRNO1, TYRNO2, TYKOL1, TYNUM1, TYKOL2, TYNUM2,LSTSOU,
     *   MKRNO1, MKRNO2, VEFF, TREC1A, TREC1B, TREC2A, TREC2B, SKIP1,
     *   SKIP2, ASKIP1, ASKIP2, HSKIP1, HSKIP2, AVTS1A, AVTS1B, AVTS2A,
     *   AVTS2B, AVPD1A, AVPD1B, AVPD2A, AVPD2B, AVPG1A, AVPG1B, AVPG2A,
     *   AVPG2B
LOCAL END
      PROGRAM TYAPL
C-----------------------------------------------------------------------
C! remove/apply nominal sensitivities to VLA data
C# Utility UV VLA Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2007-2017
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
C   TYAPL undoes the nominal sensitivities from one TY table and
C   applies those from another TY table.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      SUBARRAY       SUBARR        Subarray to do
C      FREQID         FRQSEL        FREQID to do
C      INVERS         INVER         Undo this version (0 none)
C      IN2VERS        INVER2        Apply this version (0 none)
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'TYAPL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA PRGM /'TYAPL '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL TYAPIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      IF ((OPTYPE.NE.'CL') .AND. (OPTYPE.NE.'CLP')) THEN
         CALL TYAPDO (IRET)
      ELSE
         CALL TYAPCL (IRET)
         END IF
      IF (IRET.NE.0) GO TO 990
      IF (EXTRAP.GT.0) THEN
         WRITE (MSGTXT,1000) EXTRAP
         CALL MSGWRT (6)
         END IF
      CALL TYAPHI
      IF (OPTYPE(:2).NE.'CL') CALL COPTAB (IRET)
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRBUF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('WARNING:',I12,' samples had to be extrapolated in time')
      END
      SUBROUTINE TYAPIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   TYAPIN gets input parameters for TYAPL and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      INCLUDE 'TYAPL.INC'
      INCLUDE 'TYTABS.INC'
      CHARACTER STAT*4, BLANK*6, PTYPE*2, KEYWRD*8, FRMATC*8,
     *   BNDCOD(MAXIF)*8, RXBAND*2
      INTEGER   IROUND, NPARM, IERR, INCX, I, VALUE, LOCS, KEYTYP, J,
     *   LUN(2), NUMAN(513), KEY(2,2), IP, IIF, NUMKEY, IANT, VER,
     *   ISBAND(MAXIF), KEYSUB(2,2)
      REAL      CATR(256), RPARM(20), FKEY(2,2), FINC(MAXIF)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128), FOFF(MAXIF)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA BLANK, LUN /' ', 59,60/
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
C                                       SY format change date
      DATA FRMATC /'20110223'/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      FGVER = -1
      NMSG = 0
      CURTIM = -1.E5
      JBUFSZ = UVBFSS * 2
      LSTSOU = -999
      CALL JULDAY (FRMATC, JD0)
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 39
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, SCRBUF, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (2, 1, XXTYPE, XTYPE)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      CALL H2CHR (48, 1, XCALIN, CALIN)
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
      IF (REWAY(1).LE.0.0) REWAY(1) = 1.0
      IF (REWAY(2).LE.0.0) REWAY(2) = 1.0
      IF ((OPTYPE.EQ.'PGN') .OR. (OPTYPE.EQ.'CL') .OR.
     *   (OPTYPE.EQ.'CLP')) THEN
         MSGTXT = 'WEIGHTS WILL *NOT* BE COMPUTED'
         IF (DOWAY.GE.0.0) CALL MSGWRT (7)
         DOWAY = -1.0
      ELSE
         OPTYPE = ' '
         END IF
      DOGAIN = (OPTYPE.EQ.'PGN') .OR. (OPTYPE.EQ.'CLP')
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
      DOACOR = .TRUE.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
      CALL H2CHR (8, 1, CATH(KHDOB), KEYWRD)
      CALL JULDAY (KEYWRD, JD)
      FGVER = IROUND (XFLAG)
      CALL FNDEXT ('FG', CATBLK, I)
      IF (FGVER.GT.I) FGVER = I
      IF (FGVER.LE.0) FGVER = -1
      XFLAG = FGVER
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       number antennas
      CALL GETNAN (DISKIN, OLDCNO, CATBLK, LUN, SCRBUF, NUMAN, JERR)
      IF (JERR.NE.0) GO TO 999
      NANT = NUMAN(1+SUBARR)
      NPOL = CATBLK(KINAX+JLOCS)
      NPOL = MIN (2, NPOL)
      NIF = 1
      IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
      INVER1 = IROUND (XINV)
      INVER2 = IROUND (XINV2)
C                                       which type of table?
      CALL FNDEXT ('TY', CATBLK, I)
      CALL FNDEXT ('SY', CATBLK, J)
      IF (XTYPE.EQ.'SY') THEN
         I = J
      ELSE IF (XTYPE.NE.'TY') THEN
         IF (J.GT.0) THEN
            XTYPE = 'SY'
            I = J
         ELSE IF (I.GT.0) THEN
            XTYPE = 'TY'
         ELSE
            MSGTXT = 'NO SY OR TY TABLES: I QUIT'
            JERR = 10
            GO TO 990
            END IF
         END IF
C                                       header keyword
      IF (XTYPE.EQ.'TY') THEN
         IF (OPTYPE.EQ.'PGN') OPTYPE = ' '
         IF (OPTYPE.EQ.'CLP') OPTYPE = 'CL'
         KEYWRD = 'CORRCOEF'
         NUMKEY = 1
         CALL CATKEY ('REED', DISKIN, OLDCNO, KEYWRD, NUMKEY, LOCS,
     *      VALUE, KEYTYP, SCRBUF, IERR)
         IF ((IERR.NE.0) .OR. (VALUE.EQ.0)) THEN
            MSGTXT = 'DON''T KNOW IF INPUT FILE IS CORR COEF OR NOT'
            CALL MSGWRT (6)
            LOCS = 1
            VALUE = 0
            KEYTYP = 4
            END IF
C                                       default true for EVLA
      ELSE
         KEYWRD = 'CROSSPOW'
         NUMKEY = 1
         CALL CATKEY ('REED', DISKIN, OLDCNO, KEYWRD, NUMKEY, LOCS,
     *      VALUE, KEYTYP, SCRBUF, IERR)
         IF ((IERR.NE.0) .OR. (ABS(VALUE).GT.1) .OR. (KEYTYP.EQ.0)) THEN
            MSGTXT = 'DON''T KNOW IF INPUT FILE IS CROSS POWER OR NOT'
            CALL MSGWRT (6)
            LOCS = 1
            VALUE = 1
            KEYTYP = 4
            END IF
         IF (VALUE.EQ.0) THEN
            MSGTXT = 'DATA ARE CROSS POWERS BUT WEIGHTS ARE REAL'
            CALL MSGWRT (6)
            MSGTXT = 'TYAPL SHOULD BE APPLIED BEFORE REWAY'
            CALL MSGWRT (6)
            MSGTXT = 'DOING WHAT YOU ASKED ANYWAY!'
            CALL MSGWRT (6)
            END IF
         END IF
C                                       default function
      IF ((INVER1.LE.0) .AND. (INVER2.LE.0)) THEN
         IF (VALUE.EQ.1) THEN
            INVER2 = I
         ELSE IF (VALUE.EQ.-1) THEN
            INVER1 = I
         ELSE
            MSGTXT = 'NO GUIDANCE FROM YOU EITHER SO I QUIT'
            JERR = 10
            GO TO 990
            END IF
         END IF
      IF (INVER1.GT.0) THEN
         IF (INVER1.GT.I) THEN
            WRITE (MSGTXT,1045) INVER1, I
            JERR = 10
            GO TO 990
            END IF
         IF (VALUE.GE.1) THEN
            IF (XTYPE.EQ.'TY') THEN
               MSGTXT = 'DATA ARE CORR COEF - WRONG TO REMOVE' //
     *            ' A TY TABLE'
            ELSE
               MSGTXT = 'DATA ARE CROSS-POWER - WRONG TO REMOVE' //
     *            ' AN SY TABLE'
               END IF
            JERR = 10
            GO TO 990
            END IF
         VALUE = 1
         END IF
      IF (INVER2.GT.0) THEN
         IF (INVER2.GT.I) THEN
            WRITE (MSGTXT,1045) INVER2, I
            JERR = 10
            GO TO 990
            END IF
         IF (VALUE.LT.0) THEN
            IF (XTYPE.EQ.'TY') THEN
               MSGTXT = 'DATA ARE DECI-JY - WRONG TO APPLY' //
     *            ' A TY TABLE'
            ELSE
               MSGTXT = 'DATA ARE GAIN CAL''D - WRONG TO APPLY' //
     *            ' AN SY TABLE'
               END IF
            JERR = 10
            GO TO 990
            END IF
         VALUE = -1
         END IF
C                                       Freq id
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.LE.0) FRQSEL = 1
C                                       now using cal system -
C                                       UVGET makes header
      IF (OPTYPE(:2).NE.'CL') THEN
         CALL UVGET ('INIT', RPARM, SCRBUF, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1035) JERR
            GO TO 990
            END IF
         CALL UVGET ('CLOS', RPARM, SCRBUF, IERR)
         END IF
C                                       Save input file info
      INCX = CATBLK(KINAX)
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                       Read CD table
      IF (XTYPE.EQ.'SY') THEN
         IF (DOGAIN) THEN
            I = 4 * MAXIF * MAXANT
            CALL RFILL (I, 1.0, TCAL)
         ELSE
            I = 0
            CALL GETCDS (DISKIN, OLDCNO, I, SUBARR, FRQSEL, CATOLD,
     *         TCAL, JERR)
            IF (JERR.NE.0) GO TO 999
            END IF
C                                       get efficiencies
         I = 2 * MAXIF * MAXANT
         CALL RFILL (I, 1.0, VEFF)
         VER = 1
         CALL CHNDAT ('READ', TYBUF1, DISKIN, OLDCNO, VER, CATOLD, LUN,
     *      NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, JERR)
         IF (JERR.EQ.0) THEN
            DO 10 J = 1,NIF
               FOFF(J) = (FOFF(J) + CATD(KDCRV+JLOCF)) / 1.D9
 10            CONTINUE
            CALL GETBND (NIF, FOFF, BNDCOD, J, RXBAND)
            CALL FNDEFF (NIF, FOFF, RXBAND, NANT, CALIN, BUFF2, VEFF)
            END IF
         JERR = 0
         END IF
C                                       open and sort TY table(s)
      IF ((INVER1.GT.0) .AND. (XTYPE.EQ.'TY')) THEN
         CALL TYINI ('READ', TYBUF1, DISKIN, OLDCNO, INVER1, CATOLD,
     *      LUN(1), TYRNO1, TYKOL1, TYNUM1, IP, IIF, JERR)
         IF (JERR.NE.0) GO TO 999
         KEY(1,2) = TYIANT
         KEY(1,1) = TYRTIM
         IF ((TYBUF1(43).NE.KEY(1,1)) .OR. (TYBUF1(44).NE.KEY(1,2)))
     *      THEN
            CALL TABIO ('CLOS', 0, TYRNO1, TYBUF1, TYBUF1, JERR)
            CALL TABSRT (DISKIN, OLDCNO, 'TY', INVER1, INVER1, KEY,
     *         KEYSUB, FKEY, TYBUF1, CATOLD, JERR)
            IF (JERR.NE.0) GO TO 999
            CALL TYINI ('READ', TYBUF1, DISKIN, OLDCNO, INVER1, CATOLD,
     *         LUN(1), TYRNO1, TYKOL1, TYNUM1, IP, IIF, JERR)
            IF (JERR.NE.0) GO TO 999
            END IF
         IF ((IP.NE.NPOL) .OR. (IIF.NE.NIF)) THEN
            MSGTXT = 'FIRST TY TABLE NO MATCH FOR POL AND/OR IFS'
            JERR = 10
            GO TO 999
            END IF
      ELSE IF ((INVER1.GT.0) .AND. (XTYPE.EQ.'SY')) THEN
         CALL SYINI ('READ', TYBUF1, DISKIN, OLDCNO, INVER1, CATOLD,
     *      LUN(1), TYRNO1, TYKOL1, TYNUM1, IANT, IP, IIF, JERR)
         IF (JERR.NE.0) GO TO 999
         KEY(1,2) = 4
         KEY(1,1) = 1
         IF ((TYBUF1(43).NE.KEY(1,1)) .OR. (TYBUF1(44).NE.KEY(1,2)))
     *      THEN
            CALL TABIO ('CLOS', 0, TYRNO1, TYBUF1, TYBUF1, JERR)
            CALL TABSRT (DISKIN, OLDCNO, 'SY', INVER1, INVER1, KEY,
     *         KEYSUB, FKEY, TYBUF1, CATOLD, JERR)
            IF (JERR.NE.0) GO TO 999
            CALL SYINI ('READ', TYBUF1, DISKIN, OLDCNO, INVER1, CATOLD,
     *         LUN(1), TYRNO1, TYKOL1, TYNUM1, IANT, IP, IIF, JERR)
            IF (JERR.NE.0) GO TO 999
            END IF
         IF ((IP.NE.NPOL) .OR. (IIF.NE.NIF)) THEN
            MSGTXT = 'FIRST SY TABLE NO MATCH FOR POL AND/OR IFS'
            JERR = 10
            GO TO 999
            END IF
         END IF
      IF ((INVER2.GT.0) .AND. (XTYPE.EQ.'TY')) THEN
         CALL TYINI ('READ', TYBUF2, DISKIN, OLDCNO, INVER2, CATOLD,
     *      LUN(2), TYRNO2, TYKOL2, TYNUM2, IP, IIF, JERR)
         IF (JERR.NE.0) GO TO 999
         KEY(1,2) = TYIANT
         KEY(1,1) = TYRTIM
         IF ((TYBUF2(43).NE.KEY(1,1)) .OR. (TYBUF2(44).NE.KEY(1,2)))
     *      THEN
            CALL TABIO ('CLOS', 0, TYRNO2, TYBUF2, TYBUF2, JERR)
            CALL TABSRT (DISKIN, OLDCNO, 'TY', INVER2, INVER2, KEY,
     *         KEYSUB, FKEY, TYBUF2, CATOLD, JERR)
            IF (JERR.NE.0) GO TO 999
            CALL TYINI ('READ', TYBUF2, DISKIN, OLDCNO, INVER2, CATOLD,
     *         LUN(2), TYRNO2, TYKOL2, TYNUM2, IP, IIF, JERR)
            IF (JERR.NE.0) GO TO 999
            END IF
         IF ((IP.NE.NPOL) .OR. (IIF.NE.NIF)) THEN
            MSGTXT = 'SECOND TY TABLE NO MATCH FOR POL AND/OR IFS'
            JERR = 10
            GO TO 999
            END IF
      ELSE IF ((INVER2.GT.0) .AND. (XTYPE.EQ.'SY')) THEN
         CALL SYINI ('READ', TYBUF2, DISKIN, OLDCNO, INVER2, CATOLD,
     *      LUN(2), TYRNO2, TYKOL2, TYNUM2, IANT, IP, IIF, JERR)
         IF (JERR.NE.0) GO TO 999
         KEY(1,2) = 4
         KEY(1,1) = 1
         IF ((TYBUF2(43).NE.KEY(1,1)) .OR. (TYBUF2(44).NE.KEY(1,2)))
     *      THEN
            CALL TABIO ('CLOS', 0, TYRNO2, TYBUF2, TYBUF2, JERR)
            CALL TABSRT (DISKIN, OLDCNO, 'SY', INVER2, INVER2, KEY,
     *         KEYSUB, FKEY, TYBUF2, CATOLD, JERR)
            IF (JERR.NE.0) GO TO 999
            CALL SYINI ('READ', TYBUF2, DISKIN, OLDCNO, INVER2, CATOLD,
     *         LUN(2), TYRNO2, TYKOL2, TYNUM2, IANT, IP, IIF, JERR)
            IF (JERR.NE.0) GO TO 999
            END IF
         IF ((IP.NE.NPOL) .OR. (IIF.NE.NIF)) THEN
            MSGTXT = 'SECOND SY TABLE NO MATCH FOR POL AND/OR IFS'
            JERR = 10
            GO TO 999
            END IF
         END IF
C                                       pass through
      IF (XTYPE.EQ.'SY') THEN
         I = 2 * MAXIF * MAXANT
         CALL FILL (I, 0, SKIP1)
         CALL FILL (I, 0, SKIP2)
         CALL FILL (MAXANT, 0, ASKIP1)
         CALL FILL (MAXANT, 0, ASKIP2)
         IF ((CUTOFF.GT.0.0) .AND. (CUTOFF.LT.1.0)) THEN
            CALL DOSKIP (JERR)
            IF (JERR.NE.0) GO TO 999
            END IF
         END IF
C                                       Put new values in CATBLK.
      IF (OPTYPE(:2).NE.'CL') THEN
         IF (JLOCF.LT.0) XCENT = -1.
         IF (XCENT.GT.0.0) THEN
            INCX = CATBLK(KINAX+JLOCF) / 2 + 1
            DIFPIX = INCX - CATR(KRCRP+JLOCF)
            CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + CATR(KRCIC+JLOCF) *
     *         DIFPIX
            CATR(KRCRP+JLOCF) = INCX
         ELSE
            DIFPIX = 0.0
            END IF
         CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT,
     *      SEQOUT)
         CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = SEQOUT
C                                       read compressed => write compr.
         IF (ISCOMP) THEN
            CATBLK(KINAX) = 1
            I = CATBLK(KIPCN)
            CALL CHR2H (8, 'WEIGHT  ', 1, CATBLK(KHPTP+2*I))
            CALL CHR2H (8, 'SCALE   ', 1, CATBLK(KHPTP+2*I+2))
            CATBLK(KIPCN) = I + 2
            ILOCWT = I
            END IF
C                                       Create output file.
         CCNO = 1
         FRW(NCFILE+1) = 3
         JERR = 4
         CALL UVCREA (DISKO, CCNO, SCRBUF, IERR)
         IF (IERR.NE.0) THEN
            IF (IERR.NE.2) THEN
               WRITE (MSGTXT,1050) IERR
               GO TO 990
               END IF
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
            IF ((CCNO.NE.OLDCNO) .OR. (DISKO.NE.DISKIN)) THEN
               WRITE (MSGTXT,1060)
               GO TO 990
               END IF
C                                       Recover existing CATBLK
            FRW(NCFILE+1) = 2
            CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', SCRBUF,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1065) IERR
               CALL MSGWRT (6)
               END IF
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKO
         FCNO(NCFILE) = CCNO
         FRW(NCFILE) = FRW(NCFILE) - 1
         NEWCNO = CCNO
C                                       Save output file info
         CALL UVPGET (JERR)
         IF (JERR.NE.0) GO TO 999
         INCX = CATBLK(KINAX)
         LRECO = LREC
         NRPRMO = NRPARM
         INCSO = INCS / INCX
         INCFO = INCF / INCX
         INCIFO = INCIF / INCX
C                                       Copy any header keywords
         CALL KEYCOP (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
         IF ((XTYPE.NE.'SY') .OR. (DOWAY.GE.0.0)) CALL CATKEY ('WRIT',
     *      DISKO, NEWCNO, KEYWRD, NUMKEY, LOCS, VALUE, KEYTYP, SCRBUF,
     *      IERR)
         END IF
C                                        Put input file in READ
      PTYPE = 'UV'
      STAT = 'READ'
      IF (OPTYPE(:2).EQ.'CL') STAT = 'WRIT'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, SCRBUF, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      IF (OPTYPE(:2).EQ.'CL') FRW(NCFILE) = 1
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TYAPIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1045 FORMAT ('TY VERSION',I5,' EXCEEDS MAX',I5)
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('TYAPIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE TYAPDO (IRET)
C-----------------------------------------------------------------------
C   TYAPDO sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
C   Input in common:
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER OFILE*48
      INTEGER   IPTRO, LUNO, INDO, ILENBU, KBIND, NIOUT, NIOLIM, IA1,
     *   IA2, BO, VO, XCOUNT, NCORO, NCOPY, CATMP(256), RNXRET
      LOGICAL   T, F
      INCLUDE 'TYAPL.INC'
      INCLUDE 'TYTABS.INC'
      REAL      VIS(UVBFSS), RESULT(UVBFSS), RPARM(20)
      DOUBLE PRECISION UVSCAL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA LUNO /17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Number of visibilities in input
C                                       and output files.
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NCOPY = LRECO - NRPRMO
      CALL FILL (NANT, -1, TREC1A)
      CALL FILL (NANT, -1, TREC1B)
      CALL FILL (NANT, -1, TREC2A)
      CALL FILL (NANT, -1, TREC2B)
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
      CALL UVPGET (IRET)
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
      XCOUNT = 0
C                                       make an index table
      CALL RNXGET (DISKIN, OLDCNO, CATOLD)
      CALL RNXINI (DISKO, NEWCNO, CATBLK, RNXRET)
      IF ((FREQ.GT.0.0D0) .AND. (UVFREQ.GT.0.0D0)) THEN
         UVSCAL = FREQ / UVFREQ
      ELSE
         UVSCAL = 1.0D0
         END IF
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
C                                       Loop over buffer
      ELSE IF (IRET.EQ.0) THEN
         IF (ILOCB.GE.0) THEN
            IA2 = RPARM(1+ILOCB) + 0.1
            IA1 = IA2 / 256
            IA2 = IA2 - IA1*256
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            END IF
         RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
         RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
         RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
C                                       call user routine
         CALL TYAPPL (RPARM(1+ILOCT), IA1, IA2, VIS, RPARM, RESULT,
     *      IRET)
C                                       Error (fatal)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1120) XTYPE, IRET
            GO TO 990
C                                       Copy to output.
         ELSE IF (IRET.EQ.0) THEN
            XCOUNT = XCOUNT + 1.0D0
            CALL RCOPY (NRPRMI, RPARM, BUFF2(IPTRO))
C                                       update NX table
            CALL RNXUPD (RPARM, RNXRET)
C                                       Compressed
            IF (ISCOMP) THEN
               CALL ZUVPAK (NCORO, RESULT, BUFF2(IPTRO+ILOCWT),
     *            BUFF2(IPTRO+NRPRMO))
            ELSE
               CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
               END IF
            IPTRO = IPTRO + LRECO
            NIOUT = NIOUT + 1
            END IF
C                                       Write vis record.
         IF (NIOUT.GE.NIOLIM) THEN
            CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1150) IRET
               GO TO 990
               END IF
            IPTRO = KBIND
            NIOUT = 0
            END IF
C                                       Read next buffer.
         GO TO 100
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       Close files
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
C                                       close NX table
      IRET = 0
      CALL RNXCLS (RNXRET)
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TYAPDO: ERROR',I3,' OPEN/INIT INPUT VIS FILE')
 1010 FORMAT ('TYAPDO: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('TYAPDO: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1100 FORMAT ('TYAPDO: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('TYAPDO: ',A,'APPL ERROR',I3)
 1150 FORMAT ('TYAPDO: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE TYAPHI
C-----------------------------------------------------------------------
C   TYAPHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      INTEGER   NONOT
      PARAMETER (NONOT=20)
C
      CHARACTER NOTTYP(NONOT)*2, HILINE*72, POL(2)*1
      INTEGER   LUN1, LUN2, IERR, I, LF, LP, IP
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'TYAPL.INC'
      INCLUDE 'TYTABS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1, LUN2 /27,28/
      DATA POL /'R','L'/
      DATA NOTTYP /'AN','BP','CL','CQ','FG','GC','IM','MC','PC','SN',
     *    'SU','TY','WX','BL','CP','PD','NX','CH','SY','CD'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       output file written
      IF ((OPTYPE.NE.'CL') .AND. (OPTYPE.NE.'CLP')) THEN
C                                       Copy/open history file.
         CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *      SCRBUF, BUFF2, IERR)
         IF (IERR.GT.2) THEN
            WRITE (MSGTXT,1000) IERR, 'COPY HISTORY FILE'
            CALL MSGWRT (6)
            GO TO 100
            END IF
C                                       New history
         CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *      IERR)
         IF (IERR.NE.0) GO TO 100
         CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *      BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         IF (OPTYPE.EQ.'PGN') THEN
            HILINE = TSKNAM // '/ Post-gain only was applied'
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
C                                       CL table fuck up
      ELSE
         CALL HIOPEN (LUN2, DISKIN, OLDCNO, BUFF2, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN OLD HISTORY FILE'
            CALL MSGWRT (6)
            GO TO 100
            END IF
         IF (OPTYPE.EQ.'CLP') THEN
            HILINE = TSKNAM // '/ Post-gain only was applied'
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
         END IF
C                                       calibration history
      CALL CALHIS (LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       versions applied
      IF (INVER1.GT.0) THEN
         WRITE (HILINE,1010) TSKNAM, INVER1, XTYPE, 'removed'
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         IF (XTYPE.EQ.'SY') THEN
            DO 30 I = 1,MAXANT
               IF (HSKIP1(I).GT.0) THEN
                  WRITE (HILINE,1015) TSKNAM, I, HSKIP1(I)
                  CALL HIADD (LUN2, HILINE, BUFF2, IERR)
                  IF (IERR.NE.0) GO TO 100
                  MSGTXT = HILINE(9:)
                  CALL MSGWRT (2)
C                                       details
                  HILINE(27:) = ' '
                  IP = 28
                  DO 29 LF = 1,NIF
                     DO 28 LP = 1,NPOL
                        IF (SKIP1(LP,LF,I).EQ.1) THEN
                           WRITE (HILINE(IP:),1016) LF, POL(LP)
                           IP = IP + 5
                           IF (IP.GE.61) THEN
                              CALL HIADD (LUN2, HILINE, BUFF2, IERR)
                              IF (IERR.NE.0) GO TO 100
                              MSGTXT = HILINE(9:)
                              CALL MSGWRT (2)
                              IP = 28
                              HILINE(27:) = ' '
                              END IF
                          END IF
 28                    CONTINUE
 29                 CONTINUE
                  IF (IP.GT.28) THEN
                     CALL HIADD (LUN2, HILINE, BUFF2, IERR)
                     IF (IERR.NE.0) GO TO 100
                     MSGTXT = HILINE(9:)
                     CALL MSGWRT (2)
                     END IF
                  END IF
 30            CONTINUE
            END IF
         END IF
      IF (INVER2.GT.0) THEN
         WRITE (HILINE,1011) TSKNAM, INVER2, XTYPE, 'applied'
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         IF (XTYPE.EQ.'SY') THEN
            DO 40 I = 1,MAXANT
               IF (HSKIP2(I).GT.0) THEN
                  WRITE (HILINE,1015) TSKNAM, I, HSKIP2(I)
                  CALL HIADD (LUN2, HILINE, BUFF2, IERR)
                  IF (IERR.NE.0) GO TO 100
                  MSGTXT = HILINE(9:)
                  CALL MSGWRT (2)
C                                       details
                  HILINE(27:) = ' '
                  IP = 28
                  DO 39 LF = 1,NIF
                     DO 38 LP = 1,NPOL
                        IF (SKIP2(LP,LF,I).EQ.1) THEN
                           WRITE (HILINE(IP:),1016) LF, POL(LP)
                           IP = IP + 5
                           IF (IP.GE.61) THEN
                              CALL HIADD (LUN2, HILINE, BUFF2, IERR)
                              IF (IERR.NE.0) GO TO 100
                              MSGTXT = HILINE(9:)
                              CALL MSGWRT (2)
                              IP = 28
                              HILINE(27:) = ' '
                              END IF
                          END IF
 38                    CONTINUE
 39                 CONTINUE
                  IF (IP.GT.28) THEN
                     CALL HIADD (LUN2, HILINE, BUFF2, IERR)
                     IF (IERR.NE.0) GO TO 100
                     MSGTXT = HILINE(9:)
                     CALL MSGWRT (2)
                     END IF
                  END IF
 40            CONTINUE
            END IF
         END IF
C                                       New weights
      IF (DOWAY.GE.0.0) THEN
         HILINE = TSKNAM // '/ New data weights computed'
      ELSE
         HILINE = TSKNAM // '/ New data weights were not computed'
         END IF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       added scaling
      IF ((XTYPE.EQ.'SY') .AND. ((REWAY(1).NE.1.0) .OR.
     *   (REWAY(2).NE.1.0))) THEN
         WRITE (HILINE,1020) TSKNAM, REWAY(1)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (HILINE,1021) TSKNAM, REWAY(2)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       Close HI file
 100  CALL HICLOS (LUN2, .TRUE., BUFF2, IERR)
C                                       Copy tables
      IF (OPTYPE(:2).NE.'CL') THEN
C                                       Copy tables
         CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *      NEWCNO, CATBLK, BUFF2(1025), BUFF2, IERR)
         IF (IERR.GT.2) THEN
            MSGTXT = 'TYAPHI: ERROR COPYING TABLES TO OUTPUT UV'
            CALL MSGWRT (6)
            END IF
C                                       Update CATBLK.
         CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', SCRBUF,
     *      IERR)
      ELSE
         CALL CATIO ('UPDT', DISKIN, OLDCNO, CATBLK, 'REST', SCRBUF,
     *      IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TYAPHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'INVERS  =',I5,5X,'/ ',A,' file version ',A)
 1011 FORMAT (A6,'IN2VERS =',I5,5X,'/ ',A,' file version ',A)
 1015 FORMAT (A6,'/ ANTENNA',I3,' skipped SY values for',I4,
     *   ' IFs/polarizations')
 1016 FORMAT (I3,A1)
 1020 FORMAT (A6,'REWAY(1)=',F8.4,5X,'/ additional vis scale factor')
 1021 FORMAT (A6,'REWAY(2)=',F8.4,5X,'/ additional weight scale factor')
      END
      SUBROUTINE TYAPPL (T, IA1, IA2, VIS, RPARM, RESULT, IRET)
C-----------------------------------------------------------------------
C   Manages the corrections for the Tsys or SY
C   Inputs:
C      T       R    Time in days since 0 IAT on the first day for
C                   which there is data, the julian day corresponding
C                   to this day can be obtained in D   form by:
C                   CALL JULDAY (CATH(KHDOB),XDAY) where XDAY will
C                   be the Julian day number.
C      IA1     I    First antenna number
C      IA2     I    Second antenna number
C      RPARM   R(*) Random parameter array which includes U,V,W etc
C                   but also any other random parameters.
C      VIS     R(3,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C   Inputs from COMMON:
C      RA         D       Right ascension (1950) of phase center. (deg)
C      DEC        D       Declination (1950) of phase center. (deg)
C      FREQ       D       Frequency of observation (Hz)
C      NRPARM     I       # random parameters.
C      NCOR       I       # correlators
C      CATBLK     I(256)  Catalog header record. See Going Aips for
C                         details.
C      NRPRMI     I    Input number of random parameters.
C      INCSI      I    Input Stokes' increment in vis.
C      INCFI      I    Input frequency increment in vis.
C      INCIFI     I    Input IF increment in vis.
C      LRECO      I    Output file record length
C      NRPRMO     I    Output number of random parameters.
C      INCSO      I    Output Stokes' increment in vis.
C      INCFO      I    Output frequency increment in vis.
C      INCIFO     I    Output IF increment in vis.
C   Output:
C      T          R    Time in same units as input.
C      RPARM      R    Modified random parameter array.
C      RESULT     R(3,*) Output visibilities selected in frequency.
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C   Output in COMMON:
C      CATBLK    I         Catalog header block
C-----------------------------------------------------------------------
      INTEGER   IA1, IA2, IRET
      REAL      T, VIS(3,*), RPARM(*), RESULT(3,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'TYAPL.INC'
      INCLUDE 'TYTABS.INC'
      INTEGER   JIF, JF, JS, MIF, MF, MS, INDEXO, INDEXI
      REAL      VFAC(4,MAXIF), WFAC(4,MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       check/get TY values
      IF (T.GT.CURTIM) THEN
         IF (XTYPE.EQ.'TY') THEN
            CALL GETTY (T, IRET)
         ELSE
            CALL GETSY (T, IRET)
            END IF
         IF (IRET.NE.0) GO TO 999
         CURTIM = T
         END IF
C                                       get current values
      IF (XTYPE.EQ.'TY') THEN
         CALL GETTYF (T, IA1, IA2, VFAC, WFAC)
      ELSE
         CALL GETSYF (T, IA1, IA2, VFAC, WFAC)
         END IF
C                                       pointers to traverse the data
      MS = 1
      MIF = 1
      MF = 1
      IF (JLOCS.GE.0) MS = CATBLK(KINAX+JLOCS)
      IF (JLOCIF.GE.0) MIF = CATBLK(KINAX+JLOCIF)
      IF (JLOCF.GE.0) MF = CATBLK(KINAX+JLOCF)
      DO 40 JIF = 1,MIF
         DO 30 JF = 1,MF
            DO 20 JS = 1,MS
               INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *            (JS-1) * INCSI + 1
               INDEXO = (JIF-1) * INCIFO + (JF-1) * INCFO +
     *            (JS-1) * INCSO + 1
C                                       Scale vis and weights
               IF ((VFAC(JS,JIF).NE.FBLANK) .AND.
     *            (WFAC(JS,JIF).NE.FBLANK)) THEN
                  RESULT(1,INDEXO) = VIS(1,INDEXI) * VFAC(JS,JIF)
                  RESULT(2,INDEXO) = VIS(2,INDEXI) * VFAC(JS,JIF)
                  IF (DOWAY.GE.0.0) THEN
C                                       use integration time if possible
                     IF (ILOCIT.GE.0) THEN
                        RESULT(3,INDEXO) = RPARM(1+ILOCIT)*WFAC(JS,JIF)
                     ELSE
                        RESULT(3,INDEXO) = VIS(3,INDEXI) * WFAC(JS,JIF)
                        END IF
                     IF (VIS(3,INDEXI).LE.0.0) RESULT(3,INDEXO) =
     *                  - ABS (RESULT(3,INDEXO))
                  ELSE
                     RESULT(3,INDEXO) = VIS(3,INDEXI)
                     END IF
               ELSE
                  RESULT(1,INDEXO) = VIS(1,INDEXI)
                  RESULT(2,INDEXO) = VIS(2,INDEXI)
                  RESULT(3,INDEXO) = -ABS (VIS(3,INDEXI))
                  END IF
 20            CONTINUE
 30         CONTINUE
 40      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE GETTY (T, IRET)
C-----------------------------------------------------------------------
C   GETTY fills in the 2 sets of TY values for each of the removal and
C   application of nominal sensitivities.
C   Inputs:
C      T      R   New time
C   Outputs:
C      IRET   I   IO error in TY reads
C   Assumes that CURSOU, SUBARR, FRQSEL are set to apply to the current
C   data.
C-----------------------------------------------------------------------
      REAL      T
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'TYAPL.INC'
      INCLUDE 'TYTABS.INC'
      INTEGER   I, RNOMAX, SOURID, ANTNO, SUBA, FREQID, INEED, IA,
     *   INRNO, SYRNO, II
      REAL      TIME, TIMEI, TSYS(2,MAXIF), TANT(2,MAXIF)
      LOGICAL   NEWSOU, NEED, ANEED(MAXANT)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       remove nom sens
      IF (INVER1.GT.0) THEN
         NEWSOU = CURSOU.NE.LSTSOU
         IF (NEWSOU) THEN
            CALL RFILL (MAXANT, 1.E5, T1A)
            CALL RFILL (MAXANT, -1.E5, T1B)
            I = 2 * MAXANT * MAXIF
            CALL RFILL (I, FBLANK, TS1A)
            CALL RFILL (I, FBLANK, TS1B)
            NEED = .TRUE.
            INEED = NANT
            CALL LFILL (MAXANT, .TRUE., ANEED)
         ELSE
            I = 2 * MAXIF
            NEED = .FALSE.
            INEED = 0
            CALL LFILL (MAXANT, .FALSE., ANEED)
            DO 10 ANTNO = 1,NANT
               IF (T.GT.T1B(ANTNO)) THEN
                  NEED = .TRUE.
                  ANEED(ANTNO) = .TRUE.
                  INEED = INEED + 1
                  T1A(ANTNO) = T1B(ANTNO)
                  CALL RCOPY (I, TS1B(1,1,ANTNO), TS1A(1,1,ANTNO))
                  END IF
 10            CONTINUE
            END IF
         END IF
      IF ((INVER1.GT.0) .AND. (NEED)) THEN
         RNOMAX = TYBUF1(5)
 20      IRET = 999
         IF (TYRNO1.LE.RNOMAX) CALL TABTY ('READ', TYBUF1, TYRNO1,
     *      TYKOL1, TYNUM1, NPOL, NIF, TIME, TIMEI, SOURID, ANTNO,
     *      SUBA, FREQID, TSYS, TANT, IRET)
         IF (IRET.EQ.0) THEN
            IF ((SUBA.GT.0) .AND. (SUBARR.GT.0) .AND. (SUBA.NE.SUBARR))
     *         GO TO 20
            IF ((FREQID.GT.0) .AND. (FRQSEL.GT.0) .AND.
     *         (FREQID.NE.FRQSEL)) GO TO 20
C                                       record useful
            IF ((SOURID.LE.0) .OR. (CURSOU.LE.0) .OR.
     *         (SOURID.EQ.CURSOU)) THEN
               IF (T.GT.T1B(ANTNO)) THEN
C                                       keep as first time
                  I = 2 * NIF
                  IF ((T1A(ANTNO).GT.1.E4) .OR.
     *               ((TIME.GT.T1A(ANTNO)) .AND. (T.GE.TIME))) THEN
                     T1A(ANTNO) = TIME
                     CALL RCOPY (I, TSYS, TS1A(1,1,ANTNO))
                     TREC1A(ANTNO) = TYRNO1-1
                     END IF
C                                       keep as second time
                  IF (T.GT.T1B(ANTNO)) THEN
                     T1B(ANTNO) = TIME
                     CALL RCOPY (I, TSYS, TS1B(1,1,ANTNO))
                     TREC1B(ANTNO) = TYRNO1-1
                     IF ((T.GE.T1A(ANTNO)) .AND. (T.LE.T1B(ANTNO)) .AND.
     *                  (ANEED(ANTNO))) THEN
                        ANEED(ANTNO) = .FALSE.
                        INEED = INEED - 1
                        END IF
                     END IF
                  GO TO 20
               ELSE
                  TYRNO1 = TYRNO1 - 1
                  END IF
C                                       another source
            ELSE
               IF (TIME.LT.T) GO TO 20
               TYRNO1 = TYRNO1 - 1
               END IF
C                                       error
         ELSE
            IF (IRET.NE.999) GO TO 999
            IRET = 0
            END IF
C                                       check for missing antennas
         IF (INEED.GT.0) THEN
            DO 90 IA = 1,NANT
C                                       the antenna has occurred
               IF (((TREC1A(IA).GT.0) .OR. (TREC1B(IA).GT.0)) .AND.
     *            (ANEED(IA))) THEN
                  INRNO = MIN (TREC1A(IA), TREC1B(IA)) + 1
                  INRNO = MAX (INRNO, 1)
                  DO 80 II = INRNO,RNOMAX
                     SYRNO = II
                     CALL TABTY ('READ', TYBUF1, SYRNO, TYKOL1, TYNUM1,
     *                  NPOL, NIF, TIME, TIMEI, SOURID, ANTNO, SUBA,
     *                  FREQID, TSYS, TANT, IRET)
                     IF (IRET.GT.0) GO TO 999
                     IF (IRET.LT.0) GO TO 80
                     IF (ANTNO.NE.IA) GO TO 80
                     IF ((SUBA.GT.0) .AND. (SUBARR.GT.0) .AND.
     *                  (SUBA.NE.SUBARR)) GO TO 80
                     IF ((FREQID.GT.0) .AND. (FRQSEL.GT.0) .AND.
     *                  (FREQID.NE.FRQSEL)) GO TO 80
C                                       record useful
                     IF ((SOURID.GT.0) .AND. (CURSOU.GT.0) .AND.
     *                  (SOURID.NE.CURSOU)) GO TO 80
C                                       keep as first time
                     I = 2 * NIF
                     IF ((T1A(ANTNO).GT.1.E4) .OR.
     *                  ((TIME.GT.T1A(ANTNO)) .AND. (T.GE.TIME))) THEN
                        T1A(ANTNO) = TIME
                        CALL RCOPY (I, TSYS, TS1A(1,1,ANTNO))
                        TREC1A(ANTNO) = SYRNO-1
                        END IF
C                                       keep as second time
                     IF (T.GT.T1B(ANTNO)) THEN
                        T1B(ANTNO) = TIME
                        CALL RCOPY (I, TSYS, TS1B(1,1,ANTNO))
                        TREC1B(ANTNO) = SYRNO-1
                        IF ((T.GE.T1A(ANTNO)) .AND. (T.LE.T1B(ANTNO))
     *                     .AND. (ANEED(ANTNO))) THEN
                           ANEED(ANTNO) = .FALSE.
                           INEED = INEED - 1
                           GO TO 90
                           END IF
                     ELSE
                        IF (TIME.GT.T1B(ANTNO)) GO TO 90
                        END IF
 80                  CONTINUE
               ELSE
                  IF (ANEED(IA)) INEED = INEED - 1
                  END IF
 90            CONTINUE
            END IF
         END IF
C                                       apply new nom sens
      IF (INVER2.GT.0) THEN
         NEWSOU = CURSOU.NE.LSTSOU
         IF (NEWSOU) THEN
            CALL RFILL (MAXANT, 1.E5, T2A)
            CALL RFILL (MAXANT, -1.E5, T2B)
            I = 2 * MAXANT * MAXIF
            CALL RFILL (I, FBLANK, TS2A)
            CALL RFILL (I, FBLANK, TS2B)
            NEED = .TRUE.
            INEED = NANT
            CALL LFILL (MAXANT, .TRUE., ANEED)
         ELSE
            I = 2 * MAXIF
            NEED = .FALSE.
            INEED = 0
            CALL LFILL (MAXANT, .FALSE., ANEED)
            DO 110 ANTNO = 1,NANT
               IF (T.GT.T2B(ANTNO)) THEN
                  NEED = .TRUE.
                  ANEED(ANTNO) = .TRUE.
                  INEED = INEED + 1
                  T2A(ANTNO) = T2B(ANTNO)
                  CALL RCOPY (I, TS2B(1,1,ANTNO), TS2A(1,1,ANTNO))
                  END IF
 110           CONTINUE
            END IF
         END IF
      IF ((INVER2.GT.0) .AND. (NEED)) THEN
         RNOMAX = TYBUF2(5)
 120     IRET = 999
         IF (TYRNO2.LE.RNOMAX) CALL TABTY ('READ', TYBUF2, TYRNO2,
     *      TYKOL2, TYNUM2, NPOL, NIF, TIME, TIMEI, SOURID, ANTNO,
     *      SUBA, FREQID, TSYS, TANT, IRET)
         IF (IRET.EQ.0) THEN
            IF ((SUBA.GT.0) .AND. (SUBARR.GT.0) .AND. (SUBA.NE.SUBARR))
     *         GO TO 120
            IF ((FREQID.GT.0) .AND. (FRQSEL.GT.0) .AND.
     *         (FREQID.NE.FRQSEL)) GO TO 120
C                                       record useful
            IF ((SOURID.LE.0) .OR. (CURSOU.LE.0) .OR.
     *         (SOURID.EQ.CURSOU)) THEN
               IF (T.GT.T2B(ANTNO)) THEN
C                                       keep as first time
                  I = 2 * NIF
                  IF ((T2A(ANTNO).GT.1.E4) .OR.
     *               ((TIME.GT.T2A(ANTNO)) .AND. (T.GE.TIME))) THEN
                     T2A(ANTNO) = TIME
                     CALL RCOPY (I, TSYS, TS2A(1,1,ANTNO))
                     TREC2A(ANTNO) = TYRNO2-1
                     END IF
C                                       keep as second time
                  IF (T.GT.T2B(ANTNO)) THEN
                     T2B(ANTNO) = TIME
                     CALL RCOPY (I, TSYS, TS2B(1,1,ANTNO))
                     TREC2B(ANTNO) = TYRNO2-1
                     IF ((T.GE.T2A(ANTNO)) .AND. (T.LE.T2B(ANTNO)) .AND.
     *                  (ANEED(ANTNO))) THEN
                        ANEED(ANTNO) = .FALSE.
                        INEED = INEED - 1
                        END IF
                     END IF
                  GO TO 120
               ELSE
                  TYRNO2 = TYRNO2 - 1
                  END IF
C                                       another source
            ELSE
               IF (TIME.LT.T) GO TO 120
               TYRNO2 = TYRNO2 - 1
               END IF
C                                       error
         ELSE
            IF (IRET.NE.999) GO TO 999
            IRET = 0
            END IF
C                                       check for missing antennas
         IF (INEED.GT.0) THEN
            DO 190 IA = 1,NANT
C                                       the antenna has occurred
               IF (((TREC2A(IA).GT.0) .OR. (TREC2B(IA).GT.0)) .AND.
     *            (ANEED(IA))) THEN
                  INRNO = MIN (TREC2A(IA), TREC2B(IA)) + 1
                  INRNO = MAX (INRNO, 1)
                  DO 180 II = INRNO,RNOMAX
                     SYRNO = II
                     CALL TABTY ('READ', TYBUF2, SYRNO, TYKOL2, TYNUM2,
     *                  NPOL, NIF, TIME, TIMEI, SOURID, ANTNO, SUBA,
     *                  FREQID, TSYS, TANT, IRET)
                     IF (IRET.GT.0) GO TO 999
                     IF (IRET.LT.0) GO TO 180
                     IF (ANTNO.NE.IA) GO TO 180
                     IF ((SUBA.GT.0) .AND. (SUBARR.GT.0) .AND.
     *                  (SUBA.NE.SUBARR)) GO TO 180
                     IF ((FREQID.GT.0) .AND. (FRQSEL.GT.0) .AND.
     *                  (FREQID.NE.FRQSEL)) GO TO 180
C                                       record useful
                     IF ((SOURID.GT.0) .AND. (CURSOU.GT.0) .AND.
     *                  (SOURID.NE.CURSOU)) GO TO 180
C                                       keep as first time
                     I = 2 * NIF
                     IF ((T2A(ANTNO).GT.1.E4) .OR.
     *                  ((TIME.GT.T2A(ANTNO)) .AND. (T.GE.TIME))) THEN
                        T2A(ANTNO) = TIME
                        CALL RCOPY (I, TSYS, TS2A(1,1,ANTNO))
                        TREC2A(ANTNO) = SYRNO-1
                        END IF
C                                       keep as second time
                     IF (T.GT.T2B(ANTNO)) THEN
                        T2B(ANTNO) = TIME
                        CALL RCOPY (I, TSYS, TS2B(1,1,ANTNO))
                        TREC2B(ANTNO) = SYRNO-1
                        IF ((T.GE.T2A(ANTNO)) .AND. (T.LE.T2B(ANTNO))
     *                     .AND. (ANEED(ANTNO))) THEN
                           ANEED(ANTNO) = .FALSE.
                           INEED = INEED - 1
                           GO TO 190
                           END IF
                     ELSE
                        IF (TIME.GT.T2B(ANTNO)) GO TO 190
                        END IF
 180                 CONTINUE
               ELSE
                  IF (ANEED(IA)) INEED = INEED - 1
                  END IF
 190           CONTINUE
            END IF
         END IF
      LSTSOU = CURSOU
C
 999  RETURN
      END
      SUBROUTINE GETTYF (T, IA1, IA2, VFAC, WFAC)
C-----------------------------------------------------------------------
C   GETTYF interpolates as needed in the tables of Nominal sensitivities
C   and returns the factors to be applied
C   Inputs:
C      T      R
C      IA1    I
C      IA2    I
C   Outputs:
C      VFAC   R(4,*)   Visibility multiplier (pol, IF)
C      WFAC   R(4,*)   Weight multiplier (pol, IF)
C-----------------------------------------------------------------------
      REAL      T, VFAC(4,*), WFAC(4,*)
      INTEGER   IA1, IA2
C
      INTEGER   I, LF
      REAL      F1, F2, TR1, TR2, TL1, TL2, TEPS, TENSEC, TINY
      INCLUDE 'TYAPL.INC'
      INCLUDE 'TYTABS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      I = 4 * MAXIF
      CALL RFILL (I, 1.0, VFAC)
      CALL RFILL (I, 1.0, WFAC)
      TENSEC = 10.1 / (24.0 * 3600.0)
      TINY = TENSEC / 300.0
C                                       remove a nom sensitivity
      IF (INVER1.GT.0) THEN
         TEPS = MIN (6.*TENSEC, MAX (TENSEC, T1B(IA1)-T1B(IA1)))
         IF ((T.LT.T1A(IA1)-TINY) .OR. (T.GT.T1B(IA1)+TINY)) THEN
            EXTRAP = EXTRAP + 1
            IF ((T.LT.T1A(IA1)-TEPS) .OR. (T.GT.T1B(IA1)+TEPS)) THEN
               WRITE (MSGTXT,1000) T, IA1, T1A(IA1), T1B(IA1)
               IF (NMSG.LT.500) CALL MSGWRT (7)
               NMSG = NMSG + 1
               END IF
            END IF
         TEPS = MIN (6.*TENSEC, MAX (TENSEC, T1B(IA2)-T1B(IA2)))
         IF ((T.LT.T1A(IA2)-TINY) .OR. (T.GT.T1B(IA2)+TINY)) THEN
            EXTRAP = EXTRAP + 1
            IF ((T.LT.T1A(IA2)-TEPS) .OR. (T.GT.T1B(IA2)+TEPS)) THEN
               WRITE (MSGTXT,1000) T, IA2, T1A(IA2), T1B(IA2)
               IF (NMSG.LT.500) CALL MSGWRT (7)
               NMSG = NMSG + 1
               END IF
            END IF
         F1 = 0.0
         IF (T1A(IA1).NE.T1B(IA1)) F1 = (T - T1A(IA1)) /
     *      (T1B(IA1) - T1A(IA1))
         F2 = 0.0
         IF (T1A(IA2).NE.T1B(IA2)) F2 = (T - T1A(IA2)) /
     *      (T1B(IA2) - T1A(IA2))
         DO 20 LF = 1,NIF
            IF ((TS1A(1,LF,IA1).NE.FBLANK) .AND.
     *         (TS1B(1,LF,IA1).NE.FBLANK)) THEN
               TR1 = (1.0-F1) * TS1A(1,LF,IA1) + F1 * TS1B(1,LF,IA1)
            ELSE IF ((F1.LE.0.333) .AND. (TS1A(1,LF,IA1).NE.FBLANK))
     *         THEN
               TR1 = TS1A(1,LF,IA1)
            ELSE IF ((F1.GE.0.667) .AND. (TS1B(1,LF,IA1).NE.FBLANK))
     *         THEN
               TR1 = TS1B(1,LF,IA1)
            ELSE
               TR1 = FBLANK
               END IF
            IF ((TS1A(1,LF,IA2).NE.FBLANK) .AND.
     *         (TS1B(1,LF,IA2).NE.FBLANK)) THEN
               TR2 = (1.0-F2) * TS1A(1,LF,IA2) + F2 * TS1B(1,LF,IA2)
            ELSE IF ((F2.LE.0.333) .AND. (TS1A(1,LF,IA2).NE.FBLANK))
     *         THEN
               TR2 = TS1A(1,LF,IA2)
            ELSE IF ((F2.GE.0.667) .AND. (TS1B(1,LF,IA2).NE.FBLANK))
     *         THEN
               TR2 = TS1B(1,LF,IA2)
            ELSE
               TR2 = FBLANK
               END IF
            IF ((TS1A(2,LF,IA1).NE.FBLANK) .AND.
     *         (TS1B(2,LF,IA1).NE.FBLANK)) THEN
               TL1 = (1.0-F1) * TS1A(2,LF,IA1) + F1 * TS1B(2,LF,IA1)
            ELSE IF ((F1.LE.0.333) .AND. (TS1A(2,LF,IA1).NE.FBLANK))
     *         THEN
               TL1 = TS1A(2,LF,IA1)
            ELSE IF ((F1.GE.0.667) .AND. (TS1B(2,LF,IA1).NE.FBLANK))
     *         THEN
               TL1 = TS1B(2,LF,IA1)
            ELSE
               TL1 = FBLANK
               END IF
            IF ((TS1A(2,LF,IA2).NE.FBLANK) .AND.
     *         (TS1B(2,LF,IA2).NE.FBLANK)) THEN
               TL2 = (1.0-F2) * TS1A(2,LF,IA2) + F2 * TS1B(2,LF,IA2)
            ELSE IF ((F2.LE.0.333) .AND. (TS1A(2,LF,IA2).NE.FBLANK))
     *         THEN
               TL2 = TS1A(2,LF,IA2)
            ELSE IF ((F2.GE.0.667) .AND. (TS1B(2,LF,IA2).NE.FBLANK))
     *         THEN
               TL2 = TS1B(2,LF,IA2)
            ELSE
               TL2 = FBLANK
               END IF
            IF ((TR1.NE.FBLANK) .AND. (TR2.NE.FBLANK)) THEN
               VFAC(1,LF) = 1.0 / SQRT (TR1 * TR2)
               WFAC(1,LF) = TR1 * TR2
            ELSE
               VFAC(1,LF) = FBLANK
               WFAC(1,LF) = FBLANK
               END IF
            IF ((TL1.NE.FBLANK) .AND. (TL2.NE.FBLANK)) THEN
               VFAC(2,LF) = 1.0 / SQRT (TL1 * TL2)
               WFAC(2,LF) = TL1 * TL2
            ELSE
               VFAC(2,LF) = FBLANK
               WFAC(2,LF) = FBLANK
               END IF
            IF ((TR1.NE.FBLANK) .AND. (TL2.NE.FBLANK)) THEN
               VFAC(3,LF) = 1.0 / SQRT (TR1 * TL2)
               WFAC(3,LF) = TR1 * TL2
            ELSE
               VFAC(3,LF) = FBLANK
               WFAC(3,LF) = FBLANK
               END IF
            IF ((TL1.NE.FBLANK) .AND. (TR2.NE.FBLANK)) THEN
               VFAC(4,LF) = 1.0 / SQRT (TL1 * TR2)
               WFAC(4,LF) = TL1 * TR2
            ELSE
               VFAC(4,LF) = FBLANK
               WFAC(4,LF) = FBLANK
               END IF
 20         CONTINUE
         END IF
C                                       apply second
      IF (INVER2.GT.0) THEN
         TEPS = MIN (6.*TENSEC, MAX (TENSEC, T2B(IA1)-T2B(IA1)))
         IF ((T.LT.T2A(IA1)-TINY) .OR. (T.GT.T2B(IA1)+TINY)) THEN
            EXTRAP = EXTRAP + 1
            IF ((T.LT.T2A(IA1)-TEPS) .OR. (T.GT.T2B(IA1)+TEPS)) THEN
               WRITE (MSGTXT,1020) T, IA1, T2A(IA1), T2B(IA1)
               IF (NMSG.LT.500) CALL MSGWRT (7)
               NMSG = NMSG + 1
               END IF
            END IF
         TEPS = MIN (6.*TENSEC, MAX (TENSEC, T2B(IA2)-T2B(IA2)))
         IF ((T.LT.T2A(IA2)-TINY) .OR. (T.GT.T2B(IA2)+TINY)) THEN
            EXTRAP = EXTRAP + 1
            IF ((T.LT.T2A(IA2)-TEPS) .OR. (T.GT.T2B(IA2)+TEPS)) THEN
               WRITE (MSGTXT,1020) T, IA2, T2A(IA2), T2B(IA2)
               IF (NMSG.LT.500) CALL MSGWRT (7)
               NMSG = NMSG + 1
               END IF
            END IF
         F1 = 0.0
         IF (T2A(IA1).NE.T2B(IA1)) F1 = (T - T2A(IA1)) /
     *      (T2B(IA1) - T2A(IA1))
         F2 = 0.0
         IF (T2A(IA2).NE.T2B(IA2)) F2 = (T - T2A(IA2)) /
     *      (T2B(IA2) - T2A(IA2))
         DO 40 LF = 1,NIF
            IF ((TS2A(1,LF,IA1).NE.FBLANK) .AND.
     *         (TS2B(1,LF,IA1).NE.FBLANK)) THEN
               TR1 = (1.0-F1) * TS2A(1,LF,IA1) + F1 * TS2B(1,LF,IA1)
            ELSE IF ((F1.LE.0.333) .AND. (TS2A(1,LF,IA1).NE.FBLANK))
     *         THEN
               TR1 = TS2A(1,LF,IA1)
            ELSE IF ((F1.GE.0.667) .AND. (TS2B(1,LF,IA1).NE.FBLANK))
     *         THEN
               TR1 = TS2B(1,LF,IA1)
            ELSE
               TR1 = FBLANK
               END IF
            IF ((TS2A(1,LF,IA2).NE.FBLANK) .AND.
     *         (TS2B(1,LF,IA2).NE.FBLANK)) THEN
               TR2 = (1.0-F2) * TS2A(1,LF,IA2) + F2 * TS2B(1,LF,IA2)
            ELSE IF ((F2.LE.0.333) .AND. (TS2A(1,LF,IA2).NE.FBLANK))
     *         THEN
               TR2 = TS2A(1,LF,IA2)
            ELSE IF ((F2.GE.0.667) .AND. (TS2B(1,LF,IA2).NE.FBLANK))
     *         THEN
               TR2 = TS2B(1,LF,IA2)
            ELSE
               TR2 = FBLANK
               END IF
            IF ((TS2A(2,LF,IA1).NE.FBLANK) .AND.
     *         (TS2B(2,LF,IA1).NE.FBLANK)) THEN
               TL1 = (1.0-F1) * TS2A(2,LF,IA1) + F1 * TS2B(2,LF,IA1)
            ELSE IF ((F1.LE.0.333) .AND. (TS2A(2,LF,IA1).NE.FBLANK))
     *         THEN
               TL1 = TS2A(2,LF,IA1)
            ELSE IF ((F1.GE.0.667) .AND. (TS2B(2,LF,IA1).NE.FBLANK))
     *         THEN
               TL1 = TS2B(2,LF,IA1)
            ELSE
               TL1 = FBLANK
               END IF
            IF ((TS2A(2,LF,IA2).NE.FBLANK) .AND.
     *         (TS2B(2,LF,IA2).NE.FBLANK)) THEN
               TL2 = (1.0-F2) * TS2A(2,LF,IA2) + F2 * TS2B(2,LF,IA2)
            ELSE IF ((F2.LE.0.333) .AND. (TS2A(2,LF,IA2).NE.FBLANK))
     *         THEN
               TL2 = TS2A(2,LF,IA2)
            ELSE IF ((F2.GE.0.667) .AND. (TS2B(2,LF,IA2).NE.FBLANK))
     *         THEN
               TL2 = TS2B(2,LF,IA2)
            ELSE
               TL2 = FBLANK
               END IF
            IF ((TR1.NE.FBLANK) .AND. (TR2.NE.FBLANK) .AND.
     *         (VFAC(1,LF).NE.FBLANK)) THEN
               VFAC(1,LF) = VFAC(1,LF) * SQRT (TR1 * TR2)
               WFAC(1,LF) = WFAC(1,LF) / (TR1 * TR2)
            ELSE
               VFAC(1,LF) = FBLANK
               WFAC(1,LF) = FBLANK
               END IF
            IF ((TL1.NE.FBLANK) .AND. (TL2.NE.FBLANK) .AND.
     *         (VFAC(2,LF).NE.FBLANK)) THEN
               VFAC(2,LF) = VFAC(2,LF) * SQRT (TL1 * TL2)
               WFAC(2,LF) = WFAC(2,LF) / (TL1 * TL2)
            ELSE
               VFAC(2,LF) = FBLANK
               WFAC(2,LF) = FBLANK
               END IF
            IF ((TR1.NE.FBLANK) .AND. (TL2.NE.FBLANK) .AND.
     *         (VFAC(3,LF).NE.FBLANK)) THEN
               VFAC(3,LF) = VFAC(3,LF) * SQRT (TR1 * TL2)
               WFAC(3,LF) = WFAC(3,LF) / (TR1 * TL2)
            ELSE
               VFAC(3,LF) = FBLANK
               WFAC(3,LF) = FBLANK
               END IF
            IF ((TL1.NE.FBLANK) .AND. (TR2.NE.FBLANK) .AND.
     *         (VFAC(4,LF).NE.FBLANK)) THEN
               VFAC(4,LF) = VFAC(4,LF) * SQRT (TL1 * TR2)
               WFAC(4,LF) = WFAC(4,LF) / (TL1 * TR2)
            ELSE
               VFAC(4,LF) = FBLANK
               WFAC(4,LF) = FBLANK
               END IF
 40         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TY T1=',F11.6,' AN',I3,' NOT IN',2F14.6)
 1020 FORMAT ('TY T2=',F11.6,' AN',I3,' NOT IN',2F14.6)
      END
      SUBROUTINE GETSY (T, IRET)
C-----------------------------------------------------------------------
C   GETSY fills in the 2 sets of SY values for each of the removal and
C   application of nominal sensitivities.
C   Inputs:
C      T      R   New time
C   Outputs:
C      IRET   I   IO error in SY reads
C   Assumes that CURSOU, SUBARR, FRQSEL are set to apply to the current
C   data.
C-----------------------------------------------------------------------
      REAL      T
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'TYAPL.INC'
      INCLUDE 'TYTABS.INC'
      INTEGER   I, J, RNOMAX, SOURID, ANTNO, SUBA, FREQID, INEED, IA,
     *   INRNO, SYRNO, II, CALTYP
      REAL      TIMEI, PDIF(2,MAXIF), PSUM(2,MAXIF), PGAIN(2,MAXIF)
      DOUBLE PRECISION TIME
      LOGICAL   NEWSOU, NEED, ANEED(MAXANT), DUNSUN
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      SAVE DUNSUN
      DATA DUNSUN /.FALSE./
C-----------------------------------------------------------------------
C                                       remove nom sens
      IF (INVER1.GT.0) THEN
         NEWSOU = CURSOU.NE.LSTSOU
         IF (NEWSOU) THEN
            NEED = .TRUE.
            INEED = NANT
            CALL LFILL (MAXANT, .TRUE., ANEED)
            CALL RFILL (MAXANT, 1.E5, T1A)
            CALL RFILL (MAXANT, -1.E5, T1B)
            I = 2 * MAXANT * MAXIF
            CALL RFILL (I, FBLANK, TS1A)
            CALL RFILL (I, FBLANK, TS1B)
            CALL RFILL (I, FBLANK, PD1A)
            CALL RFILL (I, FBLANK, PD1B)
            CALL RFILL (I, FBLANK, PG1A)
            CALL RFILL (I, FBLANK, PG1B)
         ELSE
            I = 2 * MAXIF
            NEED = .FALSE.
            INEED = 0
            CALL LFILL (MAXANT, .FALSE., ANEED)
            DO 10 ANTNO = 1,NANT
               IF (T.GT.T1B(ANTNO)) THEN
                  NEED = .TRUE.
                  ANEED(ANTNO) = .TRUE.
                  INEED = INEED + 1
                  T1A(ANTNO) = T1B(ANTNO)
                  CALL RCOPY (I, TS1B(1,1,ANTNO), TS1A(1,1,ANTNO))
                  CALL RCOPY (I, PD1B(1,1,ANTNO), PD1A(1,1,ANTNO))
                  CALL RCOPY (I, PG1B(1,1,ANTNO), PG1A(1,1,ANTNO))
                  END IF
 10            CONTINUE
            END IF
         END IF
      IF ((INVER1.GT.0) .AND. (NEED)) THEN
         RNOMAX = TYBUF1(5)
 20      IRET = 999
         IF (TYRNO1.LE.RNOMAX) CALL TABSY ('READ', TYBUF1, TYRNO1,
     *      TYKOL1, TYNUM1, NPOL, NIF, TIME, TIMEI, CALTYP, SOURID,
     *      ANTNO, SUBA, FREQID, PDIF, PSUM, PGAIN, IRET)
         IF (IRET.EQ.0) THEN
            IF ((SUBA.GT.0) .AND. (SUBARR.GT.0) .AND. (SUBA.NE.SUBARR))
     *         GO TO 20
            IF ((FREQID.GT.0) .AND. (FRQSEL.GT.0) .AND.
     *         (FREQID.NE.FRQSEL)) GO TO 20
C                                       record useful
            IF ((SOURID.LE.0) .OR. (CURSOU.LE.0) .OR.
     *         (SOURID.EQ.CURSOU)) THEN
               IF ((CALTYP.EQ.1) .AND. (.NOT.DUNSUN)) THEN
                  MSGTXT = 'YOU SHOULD NOT USE TYAPL ON SOLAR DATA'
                  CALL MSGWRT (8)
                  DUNSUN = .TRUE.
                  END IF
               IF (T.GT.T1B(ANTNO)) THEN
C                                       convert units, check
                  DO 30 I = 1,NIF
                     DO 25 J = 1,2
                        IF (DOGAIN) THEN
                           IF ((PGAIN(J,I).EQ.FBLANK) .OR.
     *                        (PGAIN(J,I).LE.0.0)) THEN
                              PDIF(J,I) = FBLANK
                              PSUM(J,I) = FBLANK
                              PGAIN(J,I) = FBLANK
                           ELSE
                              PDIF(J,I) = 1.0
                              PSUM(J,I) = 2.0
                              END IF
                        ELSE IF ((PDIF(J,I).EQ.FBLANK) .OR.
     *                     (PSUM(J,I).EQ.FBLANK) .OR.
     *                     (TCAL(J,I,ANTNO).EQ.FBLANK) .OR.
     *                     (PGAIN(J,I).EQ.FBLANK) .OR.
     *                     (PGAIN(J,I).LE.0.0) .OR. (PDIF(J,I).LE.0.0)
     *                     .OR. PSUM(J,I).LE.PDIF(J,I)) THEN
                           PDIF(J,I) = FBLANK
                           PSUM(J,I) = FBLANK
                           PGAIN(J,I) = FBLANK
                        ELSE
                           PSUM(J,I) = PSUM(J,I) * TCAL(J,I,ANTNO) / 2.0
     *                        / PDIF(J,I)
                           PDIF(J,I) = PDIF(J,I) / TCAL(J,I,ANTNO)
                           END IF
 25                     CONTINUE
 30                  CONTINUE
C                                       keep as first time
                  I = 2 * NIF
                  IF ((T1A(ANTNO).GT.1.E4) .OR.
     *               ((TIME.GT.T1A(ANTNO)) .AND. (T.GE.TIME))) THEN
                     T1A(ANTNO) = TIME
                     CALL RCOPY (I, PSUM, TS1A(1,1,ANTNO))
                     CALL RCOPY (I, PDIF, PD1A(1,1,ANTNO))
                     CALL RCOPY (I, PGAIN, PG1A(1,1,ANTNO))
                     TREC1A(ANTNO) = TYRNO1-1
                     END IF
C                                       keep as second time
                  IF (T.GT.T1B(ANTNO)) THEN
                     T1B(ANTNO) = TIME
                     CALL RCOPY (I, PSUM, TS1B(1,1,ANTNO))
                     CALL RCOPY (I, PDIF, PD1B(1,1,ANTNO))
                     CALL RCOPY (I, PGAIN, PG1B(1,1,ANTNO))
                     TREC1B(ANTNO) = TYRNO1-1
                     IF ((T.GE.T1A(ANTNO)) .AND. (T.LE.T1B(ANTNO)) .AND.
     *                  (ANEED(ANTNO))) THEN
                        ANEED(ANTNO) = .FALSE.
                        INEED = INEED - 1
                        END IF
                     END IF
                  GO TO 20
               ELSE
                  TYRNO1 = TYRNO1 - 1
                  END IF
C                                       another source
            ELSE
               IF (TIME.LT.T) GO TO 20
               TYRNO1 = TYRNO1 - 1
               END IF
C                                       error
         ELSE
            IF (IRET.NE.999) GO TO 999
            IRET = 0
            END IF
C                                       check for missing antennas
         IF (INEED.GT.0) THEN
            DO 90 IA = 1,NANT
C                                       the antenna has occurred
               IF (((TREC1A(IA).GT.0) .OR. (TREC1B(IA).GT.0)) .AND.
     *            (ANEED(IA)) .AND. (ASKIP1(IA).LT.1)) THEN
                  INRNO = MIN (TREC1A(IA), TREC1B(IA)) + 1
                  INRNO = MAX (INRNO, 1)
                  DO 80 II = INRNO,RNOMAX
                     SYRNO = II
                     CALL TABSY ('READ', TYBUF1, SYRNO, TYKOL1, TYNUM1,
     *                  NPOL, NIF, TIME, TIMEI, CALTYP, SOURID, ANTNO,
     *                  SUBA, FREQID, PDIF, PSUM, PGAIN, IRET)
                     IF (IRET.GT.0) GO TO 999
                     IF (IRET.LT.0) GO TO 80
                     IF (ANTNO.NE.IA) GO TO 80
                     IF ((SUBA.GT.0) .AND. (SUBARR.GT.0) .AND.
     *                  (SUBA.NE.SUBARR)) GO TO 80
                     IF ((FREQID.GT.0) .AND. (FRQSEL.GT.0) .AND.
     *                  (FREQID.NE.FRQSEL)) GO TO 80
C                                       record useful
                     IF ((SOURID.GT.0) .AND. (CURSOU.GT.0) .AND.
     *                  (SOURID.NE.CURSOU)) GO TO 80

C                                       convert units, check
                     DO 50 I = 1,NIF
                        DO 45 J = 1,2
                           IF (DOGAIN) THEN
                              IF ((PGAIN(J,I).EQ.FBLANK) .OR.
     *                           (PGAIN(J,I).LE.0.0)) THEN
                                 PDIF(J,I) = FBLANK
                                 PSUM(J,I) = FBLANK
                                 PGAIN(J,I) = FBLANK
                              ELSE
                                 PDIF(J,I) = 1.0
                                 PSUM(J,I) = 2.0
                                 END IF
                           ELSE IF ((PDIF(J,I).EQ.FBLANK) .OR.
     *                        (PSUM(J,I).EQ.FBLANK) .OR.
     *                        (TCAL(J,I,ANTNO).EQ.FBLANK) .OR.
     *                        (PGAIN(J,I).EQ.FBLANK) .OR.
     *                        (PGAIN(J,I).LE.0.0) .OR.
     *                        (PDIF(J,I).LE.0.0).OR.
     *                        (PSUM(J,I).LE.PDIF(J,I))) THEN
                              PDIF(J,I) = FBLANK
                              PSUM(J,I) = FBLANK
                              PGAIN(J,I) = FBLANK
                           ELSE
                              PSUM(J,I) = PSUM(J,I) * TCAL(J,I,ANTNO) /
     *                           2.0 / PDIF(J,I)
                              PDIF(J,I) = PDIF(J,I) / TCAL(J,I,ANTNO)
                              END IF
 45                        CONTINUE
 50                     CONTINUE
C                                       keep as first time
                     I = 2 * NIF
                     IF ((T1A(ANTNO).GT.1.E4) .OR.
     *                  ((TIME.GT.T1A(ANTNO)) .AND. (T.GE.TIME))) THEN
                        T1A(ANTNO) = TIME
                        CALL RCOPY (I, PSUM, TS1A(1,1,ANTNO))
                        CALL RCOPY (I, PDIF, PD1A(1,1,ANTNO))
                        CALL RCOPY (I, PGAIN, PG1A(1,1,ANTNO))
                        TREC1A(ANTNO) = SYRNO-1
                        END IF
C                                       keep as second time
                     IF (T.GT.T1B(ANTNO)) THEN
                        T1B(ANTNO) = TIME
                        CALL RCOPY (I, PSUM, TS1B(1,1,ANTNO))
                        CALL RCOPY (I, PDIF, PD1B(1,1,ANTNO))
                        CALL RCOPY (I, PGAIN, PG1B(1,1,ANTNO))
                        TREC1B(ANTNO) = SYRNO-1
                        IF ((T.GE.T1A(ANTNO)) .AND. (T.LE.T1B(ANTNO))
     *                     .AND. (ANEED(ANTNO))) THEN
                           ANEED(ANTNO) = .FALSE.
                           INEED = INEED - 1
                           GO TO 90
                           END IF
                     ELSE
                        IF (TIME.GT.T1B(ANTNO)) GO TO 90
                        END IF
 80                  CONTINUE
               ELSE
                  IF (ANEED(IA)) INEED = INEED - 1
                  END IF
 90            CONTINUE
            END IF
C                                       get averages
         IF (CUTOFF.GT.0.0) THEN
            CALL SYAVER (NANT, NIF, SKIP1, TS1A, PD1A, PG1A, AVTS1A,
     *         AVPD1A, AVPG1A)
            CALL SYAVER (NANT, NIF, SKIP1, TS1B, PD1B, PG1B, AVTS1B,
     *         AVPD1B, AVPG1B)
            END IF
         END IF
C                                       apply new nom sens
      IF (INVER2.GT.0) THEN
         NEWSOU = CURSOU.NE.LSTSOU
         IF (NEWSOU) THEN
            NEED = .TRUE.
            INEED = NANT
            CALL LFILL (MAXANT, .TRUE., ANEED)
            CALL RFILL (MAXANT, 1.E5, T2A)
            CALL RFILL (MAXANT, -1.E5, T2B)
            I = 2 * MAXANT * MAXIF
            CALL RFILL (I, FBLANK, TS2A)
            CALL RFILL (I, FBLANK, TS2B)
            CALL RFILL (I, FBLANK, PD2A)
            CALL RFILL (I, FBLANK, PD2B)
            CALL RFILL (I, FBLANK, PG2A)
            CALL RFILL (I, FBLANK, PG2B)
         ELSE
            I = 2 * MAXIF
            NEED = .FALSE.
            INEED = 0
            CALL LFILL (MAXANT, .FALSE., ANEED)
            DO 110 ANTNO = 1,NANT
               IF (T.GT.T2B(ANTNO)) THEN
                  NEED = .TRUE.
                  ANEED(ANTNO) = .TRUE.
                  INEED = INEED + 1
                  T2A(ANTNO) = T2B(ANTNO)
                  CALL RCOPY (I, TS2B(1,1,ANTNO), TS2A(1,1,ANTNO))
                  CALL RCOPY (I, PD2B(1,1,ANTNO), PD2A(1,1,ANTNO))
                  CALL RCOPY (I, PG2B(1,1,ANTNO), PG2A(1,1,ANTNO))
                  END IF
 110           CONTINUE
            END IF
         END IF
      IF ((INVER2.GT.0) .AND. (NEED)) THEN
         RNOMAX = TYBUF2(5)
 120     IRET = 999
         IF (TYRNO2.LE.RNOMAX) CALL TABSY ('READ', TYBUF2, TYRNO2,
     *      TYKOL2, TYNUM2, NPOL, NIF, TIME, TIMEI, CALTYP, SOURID,
     *      ANTNO, SUBA, FREQID, PDIF, PSUM, PGAIN, IRET)
         IF (IRET.EQ.0) THEN
            IF ((SUBA.GT.0) .AND. (SUBARR.GT.0) .AND. (SUBA.NE.SUBARR))
     *         GO TO 120
            IF ((FREQID.GT.0) .AND. (FRQSEL.GT.0) .AND.
     *         (FREQID.NE.FRQSEL)) GO TO 120
C                                       record useful
            IF ((SOURID.LE.0) .OR. (CURSOU.LE.0) .OR.
     *         (SOURID.EQ.CURSOU)) THEN
               IF ((CALTYP.EQ.1) .AND. (.NOT.DUNSUN)) THEN
                  MSGTXT = 'YOU SHOULD NOT USE TYAPL ON SOLAR DATA'
                  CALL MSGWRT (8)
                  DUNSUN = .TRUE.
                  END IF
               IF (T.GT.T2B(ANTNO)) THEN
C                                       convert units, check
                  DO 130 I = 1,NIF
                     DO 125 J = 1,2
                        IF (DOGAIN) THEN
                           IF ((PGAIN(J,I).EQ.FBLANK) .OR.
     *                        (PGAIN(J,I).LE.0.0)) THEN
                              PDIF(J,I) = FBLANK
                              PSUM(J,I) = FBLANK
                              PGAIN(J,I) = FBLANK
                           ELSE
                              PDIF(J,I) = 1.0
                              PSUM(J,I) = 2.0
                              END IF
                        ELSE IF ((PDIF(J,I).EQ.FBLANK) .OR.
     *                     (PSUM(J,I).EQ.FBLANK) .OR.
     *                     (TCAL(J,I,ANTNO).EQ.FBLANK) .OR.
     *                     (PGAIN(J,I).EQ.FBLANK) .OR.
     *                     (PGAIN(J,I).LE.0.0) .OR. (PDIF(J,I).LE.0.0)
     *                     .OR. PSUM(J,I).LE.PDIF(J,I)) THEN
                           PDIF(J,I) = FBLANK
                           PSUM(J,I) = FBLANK
                           PGAIN(J,I) = FBLANK
                        ELSE
                           PSUM(J,I) = PSUM(J,I) * TCAL(J,I,ANTNO) / 2.0
     *                        / PDIF(J,I)
                           PDIF(J,I) = PDIF(J,I) / TCAL(J,I,ANTNO)
                           END IF
 125                    CONTINUE
 130                 CONTINUE
C                                       keep as first time
                  I = 2 * NIF
                  IF ((T2A(ANTNO).GT.1.E4) .OR.
     *               ((TIME.GT.T2A(ANTNO)) .AND. (T.GE.TIME))) THEN
                     T2A(ANTNO) = TIME
                     CALL RCOPY (I, PSUM, TS2A(1,1,ANTNO))
                     CALL RCOPY (I, PDIF, PD2A(1,1,ANTNO))
                     CALL RCOPY (I, PGAIN, PG2A(1,1,ANTNO))
                     TREC2A(ANTNO) = TYRNO2-1
                     END IF
C                                       keep as second time
                  IF (T.GT.T2B(ANTNO)) THEN
                     T2B(ANTNO) = TIME
                     CALL RCOPY (I, PSUM, TS2B(1,1,ANTNO))
                     CALL RCOPY (I, PDIF, PD2B(1,1,ANTNO))
                     CALL RCOPY (I, PGAIN, PG2B(1,1,ANTNO))
                     TREC2B(ANTNO) = TYRNO2-1
                     IF ((T.GE.T2A(ANTNO)) .AND. (T.LE.T2B(ANTNO)) .AND.
     *                  (ANEED(ANTNO))) THEN
                        ANEED(ANTNO) = .FALSE.
                        INEED = INEED - 1
                        END IF
                     END IF
                  GO TO 120
               ELSE
                  TYRNO2 = TYRNO2 - 1
                  END IF
C                                       another source
            ELSE
               IF (TIME.LT.T) GO TO 120
               TYRNO2 = TYRNO2 - 1
               END IF
C                                       error
         ELSE
            IF (IRET.NE.999) GO TO 999
            IRET = 0
            END IF
C                                       check for missing antennas
         IF (INEED.GT.0) THEN
            DO 190 IA = 1,NANT
C                                       the antenna has occurred
               IF (((TREC2A(IA).GT.0) .OR. (TREC2B(IA).GT.0)) .AND.
     *            (ANEED(IA)) .AND. (ASKIP2(IA).LT.1)) THEN
                  INRNO = MIN (TREC2A(IA), TREC2B(IA)) + 1
                  INRNO = MAX (INRNO, 1)
                  DO 180 II = INRNO,RNOMAX
                     SYRNO = II
                     CALL TABSY ('READ', TYBUF2, SYRNO, TYKOL2, TYNUM2,
     *                  NPOL, NIF, TIME, TIMEI, CALTYP, SOURID, ANTNO,
     *                  SUBA, FREQID, PDIF, PSUM, PGAIN, IRET)
                     IF (IRET.GT.0) GO TO 999
                     IF (IRET.LT.0) GO TO 180
                     IF (ANTNO.NE.IA) GO TO 180
                     IF ((SUBA.GT.0) .AND. (SUBARR.GT.0) .AND.
     *                  (SUBA.NE.SUBARR)) GO TO 180
                     IF ((FREQID.GT.0) .AND. (FRQSEL.GT.0) .AND.
     *                  (FREQID.NE.FRQSEL)) GO TO 180
C                                       record useful
                     IF ((SOURID.GT.0) .AND. (CURSOU.GT.0) .AND.
     *                  (SOURID.NE.CURSOU)) GO TO 180

C                                       convert units, check
                     DO 150 I = 1,NIF
                        DO 145 J = 1,2
                           IF (DOGAIN) THEN
                              IF ((PGAIN(J,I).EQ.FBLANK) .OR.
     *                           (PGAIN(J,I).LE.0.0)) THEN
                                 PDIF(J,I) = FBLANK
                                 PSUM(J,I) = FBLANK
                                 PGAIN(J,I) = FBLANK
                              ELSE
                                 PDIF(J,I) = 1.0
                                 PSUM(J,I) = 2.0
                                 END IF
                           ELSE IF ((PDIF(J,I).EQ.FBLANK) .OR.
     *                        (PSUM(J,I).EQ.FBLANK) .OR.
     *                        (TCAL(J,I,ANTNO).EQ.FBLANK) .OR.
     *                        (PGAIN(J,I).EQ.FBLANK) .OR.
     *                        (PGAIN(J,I).LE.0.0) .OR.
     *                        (PDIF(J,I).LE.0.0).OR.
     *                        (PSUM(J,I).LE.PDIF(J,I))) THEN
                              PDIF(J,I) = FBLANK
                              PSUM(J,I) = FBLANK
                              PGAIN(J,I) = FBLANK
                           ELSE
                              PSUM(J,I) = PSUM(J,I) * TCAL(J,I,ANTNO) /
     *                           2.0 / PDIF(J,I)
                              PDIF(J,I) = PDIF(J,I) / TCAL(J,I,ANTNO)
                              END IF
 145                       CONTINUE
 150                    CONTINUE
C                                       keep as first time
                     I = 2 * NIF
                     IF ((T2A(ANTNO).GT.1.E4) .OR.
     *                  ((TIME.GT.T2A(ANTNO)) .AND. (T.GE.TIME))) THEN
                        T2A(ANTNO) = TIME
                        CALL RCOPY (I, PSUM, TS2A(1,1,ANTNO))
                        CALL RCOPY (I, PDIF, PD2A(1,1,ANTNO))
                        CALL RCOPY (I, PGAIN, PG2A(1,1,ANTNO))
                        TREC2A(ANTNO) = SYRNO-1
                        END IF
C                                       keep as second time
                     IF (T.GT.T2B(ANTNO)) THEN
                        T2B(ANTNO) = TIME
                        CALL RCOPY (I, PSUM, TS2B(1,1,ANTNO))
                        CALL RCOPY (I, PDIF, PD2B(1,1,ANTNO))
                        CALL RCOPY (I, PGAIN, PG2B(1,1,ANTNO))
                        TREC2B(ANTNO) = SYRNO-1
                        IF ((T.GE.T2A(ANTNO)) .AND. (T.LE.T2B(ANTNO))
     *                     .AND. (ANEED(ANTNO))) THEN
                           ANEED(ANTNO) = .FALSE.
                           INEED = INEED - 1
                           GO TO 190
                           END IF
                     ELSE
                        IF (TIME.GT.T2B(ANTNO)) GO TO 190
                        END IF
 180                 CONTINUE
               ELSE
                  IF (ANEED(IA)) INEED = INEED - 1
                  END IF
 190           CONTINUE
            END IF
C                                       get averages
         IF (CUTOFF.GT.0.0) THEN
            CALL SYAVER (NANT, NIF, SKIP2, TS2A, PD2A, PG2A, AVTS2A,
     *         AVPD2A, AVPG2A)
            CALL SYAVER (NANT, NIF, SKIP2, TS2B, PD2B, PG2B, AVTS2B,
     *         AVPD2B, AVPG2B)
            END IF
         END IF
      LSTSOU = CURSOU
C
 999  RETURN
      END
      SUBROUTINE SYAVER (NANT, NIF, SKIP, TS, PD, PG, AVTS, AVPD, AVPG)
C-----------------------------------------------------------------------
C   SYAVER finds the average of things to use for the "skipped" antennas
C   Inputs:
C      NANT   I      Number antennas
C      NIF    I      Number IFs
C      SKIP   I(*)   Skip or use this pol, IF, antenna
C      TS     R(*)   Tsys
C      PD     R(*)   Pdif gain - Pdif/Tcal
C      PG     R(*)   Pgain
C   Outputs:
C      AVTS   R(*)   Tsys average
C      AVPD   R(*)   Pdif gain average
C      PG     R(*)   Pgain average
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NANT, NIF, SKIP(2,MAXIF,*)
      REAL      TS(2,MAXIF,*), PD(2,MAXIF,*), PG(2,MAXIF,*), AVTS(2,*),
     *   AVPD(2,*), AVPG(2,*)
C
      INTEGER   JS, JI, JA
      REAL      SS, SD, SG, NS, ND, NG
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      DO 50 JI = 1,NIF
         DO 40 JS = 1,2
            SS = 0.0
            SD = 0.0
            SG = 0.0
            NS = 0.0
            ND = 0.0
            NG = 0.0
            DO 20 JA = 1,NANT
               IF (SKIP(JS,JI,JA).LE.0) THEN
                  IF (TS(JS,JI,JA).NE.FBLANK) THEN
                     SS = SS + TS(JS,JI,JA)
                     NS = NS + 1.0
                     END IF
                  IF (PD(JS,JI,JA).NE.FBLANK) THEN
                     SD = SD + PD(JS,JI,JA)
                     ND = ND + 1.0
                     END IF
                  IF (PG(JS,JI,JA).NE.FBLANK) THEN
                     SG = SG + PG(JS,JI,JA)
                     NG = NG + 1.0
                     END IF
                  END IF
 20            CONTINUE
            IF (NS.GT.0.0) THEN
               AVTS(JS,JI) = SS / NS
            ELSE
               AVTS(JS,JI) = FBLANK
               END IF
            IF (ND.GT.0.0) THEN
               AVPD(JS,JI) = SD / ND
            ELSE
               AVPD(JS,JI) = FBLANK
               END IF
            IF (NG.GT.0.0) THEN
               AVPG(JS,JI) = SG / NG
            ELSE
               AVPG(JS,JI) = FBLANK
               END IF
 40         CONTINUE
 50      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE GETSYF (T, IA1, IA2, VFAC, WFAC)
C-----------------------------------------------------------------------
C   GETSYF interpolates as needed in the tables of Nominal sensitivities
C   and returns the factors to be applied
C   Inputs:
C      T      R        time (days)
C      IT     R        integration time (sec)
C      IA1    I        antenna 1
C      IA2    I        antenna 2
C   Outputs:
C      VFAC   R(4,*)   Visibility multiplier (pol, IF)
C      WFAC   R(4,*)   Weight multiplier (pol, IF)
C-----------------------------------------------------------------------
      REAL      T, VFAC(4,*), WFAC(4,*)
      INTEGER   IA1, IA2
C
      INTEGER   I, LF
      REAL      F1, F2, TR1, TR2, TL1, TL2, DR1, DR2, DL1, DL2, WS, VS,
     *   CATR(256), GR1, GR2, GL1, GL2, TEPS, TENSEC, TINY, VF
      INCLUDE 'TYAPL.INC'
      INCLUDE 'TYTABS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (CATBLK, CATR)
C-----------------------------------------------------------------------
      I = 4 * MAXIF
      CALL RFILL (I, 1.0, VFAC)
      CALL RFILL (I, 1.0, WFAC)
      TENSEC = 10.1 / (24.0 * 3600.0)
      TINY = TENSEC / 300.0
C                                       scaling factors: ???????????
C                                       need bandwidth for WS
      WS = ABS (CATR(KRCIC+JLOCF)) * REWAY(2) * 0.0539
      VS = REWAY(1) * 5.622 / 0.932 / 1.176
C                                       remove a nom sensitivity
      IF (INVER1.GT.0) THEN
         TEPS = MIN (6.*TENSEC, MAX (TENSEC, T1B(IA1)-T1B(IA1)))
         IF ((T.LT.T1A(IA1)-TINY) .OR. (T.GT.T1B(IA1)+TINY)) THEN
            EXTRAP = EXTRAP + 1
            IF ((T.LT.T1A(IA1)-TEPS) .OR. (T.GT.T1B(IA1)+TEPS)) THEN
               WRITE (MSGTXT,1000) T, IA1, T1A(IA1), T1B(IA1)
               IF (NMSG.LT.500) CALL MSGWRT (7)
               NMSG = NMSG + 1
               END IF
            END IF
         TEPS = MIN (6.*TENSEC, MAX (TENSEC, T1B(IA2)-T1B(IA2)))
         IF ((T.LT.T1A(IA2)-TINY) .OR. (T.GT.T1B(IA2)+TINY)) THEN
            EXTRAP = EXTRAP + 1
            IF ((T.LT.T1A(IA2)-TEPS) .OR. (T.GT.T1B(IA2)+TEPS)) THEN
               WRITE (MSGTXT,1000) T, IA2, T1A(IA2), T1B(IA2)
               IF (NMSG.LT.500) CALL MSGWRT (7)
               NMSG = NMSG + 1
               END IF
            END IF
         F1 = 0.0
         IF (T1A(IA1).NE.T1B(IA1)) F1 = (T - T1A(IA1)) /
     *      (T1B(IA1) - T1A(IA1))
         F2 = 0.0
         IF (T1A(IA2).NE.T1B(IA2)) F2 = (T - T1A(IA2)) /
     *      (T1B(IA2) - T1A(IA2))
         DO 20 LF = 1,NIF
            IF (SKIP1(1,LF,IA1).GE.1) THEN
               IF ((AVTS1A(1,LF).NE.FBLANK) .AND.
     *            (AVTS1B(1,LF).NE.FBLANK)) THEN
                  TR1 = (1.0-F1) * AVTS1A(1,LF) + F1 * AVTS1B(1,LF)
                  DR1 = (1.0-F1) * AVPD1A(1,LF) + F1 * AVPD1B(1,LF)
                  GR1 = (1.0-F1) * AVPG1A(1,LF) + F1 * AVPG1B(1,LF)
               ELSE IF (AVTS1A(1,LF).NE.FBLANK) THEN
                  TR1 = AVTS1A(1,LF)
                  DR1 = AVPD1A(1,LF)
                  GR1 = AVPG1A(1,LF)
               ELSE IF (AVTS1B(1,LF).NE.FBLANK) THEN
                  TR1 = AVTS1B(1,LF)
                  DR1 = AVPD1B(1,LF)
                  GR1 = AVPG1B(1,LF)
               ELSE
                  TR1 = 1.0
                  DR1 = 1.0
                  GR1 = 1.0/64.0
                  END IF
            ELSE IF ((TS1A(1,LF,IA1).NE.FBLANK) .AND.
     *         (TS1B(1,LF,IA1).NE.FBLANK)) THEN
               TR1 = (1.0-F1) * TS1A(1,LF,IA1) + F1 * TS1B(1,LF,IA1)
               DR1 = (1.0-F1) * PD1A(1,LF,IA1) + F1 * PD1B(1,LF,IA1)
               GR1 = (1.0-F1) * PG1A(1,LF,IA1) + F1 * PG1B(1,LF,IA1)
            ELSE IF ((F1.LE.0.333) .AND. (TS1A(1,LF,IA1).NE.FBLANK))
     *         THEN
               TR1 = TS1A(1,LF,IA1)
               DR1 = PD1A(1,LF,IA1)
               GR1 = PG1A(1,LF,IA1)
            ELSE IF ((F1.GE.0.667) .AND. (TS1B(1,LF,IA1).NE.FBLANK))
     *         THEN
               TR1 = TS1B(1,LF,IA1)
               DR1 = PD1B(1,LF,IA1)
               GR1 = PG1B(1,LF,IA1)
            ELSE
               TR1 = FBLANK
               DR1 = FBLANK
               GR1 = FBLANK
               END IF
            IF (SKIP1(1,LF,IA2).GE.1) THEN
               IF ((AVTS1A(1,LF).NE.FBLANK) .AND.
     *            (AVTS1B(1,LF).NE.FBLANK)) THEN
                  TL2 = (1.0-F1) * AVTS1A(1,LF) + F1 * AVTS1B(1,LF)
                  DL2 = (1.0-F1) * AVPD1A(1,LF) + F1 * AVPD1B(1,LF)
                  GL2 = (1.0-F1) * AVPG1A(1,LF) + F1 * AVPG1B(1,LF)
               ELSE IF (AVTS1A(1,LF).NE.FBLANK) THEN
                  TL2 = AVTS1A(1,LF)
                  DL2 = AVPD1A(1,LF)
                  GL2 = AVPG1A(1,LF)
               ELSE IF (AVTS1B(1,LF).NE.FBLANK) THEN
                  TL2 = AVTS1B(1,LF)
                  DL2 = AVPD1B(1,LF)
                  GL2 = AVPG1B(1,LF)
               ELSE
                  TL2 = 1.0
                  DL2 = 1.0
                  GL2 = 1.0/64.0
                  END IF
            ELSE IF ((TS1A(1,LF,IA2).NE.FBLANK) .AND.
     *         (TS1B(1,LF,IA2).NE.FBLANK)) THEN
               TR2 = (1.0-F2) * TS1A(1,LF,IA2) + F2 * TS1B(1,LF,IA2)
               DR2 = (1.0-F2) * PD1A(1,LF,IA2) + F2 * PD1B(1,LF,IA2)
               GR2 = (1.0-F2) * PG1A(1,LF,IA2) + F2 * PG1B(1,LF,IA2)
            ELSE IF ((F2.LE.0.333) .AND. (TS1A(1,LF,IA2).NE.FBLANK))
     *         THEN
               TR2 = TS1A(1,LF,IA2)
               DR2 = PD1A(1,LF,IA2)
               GR2 = PG1A(1,LF,IA2)
            ELSE IF ((F2.GE.0.667) .AND. (TS1B(1,LF,IA2).NE.FBLANK))
     *         THEN
               TR2 = TS1B(1,LF,IA2)
               DR2 = PD1B(1,LF,IA2)
               GR2 = PG1B(1,LF,IA2)
            ELSE
               TR2 = FBLANK
               DR2 = FBLANK
               GR2 = FBLANK
               END IF
            IF (SKIP1(2,LF,IA1).GE.1) THEN
               IF ((AVTS1A(2,LF).NE.FBLANK) .AND.
     *            (AVTS1B(2,LF).NE.FBLANK)) THEN
                  TL1 = (1.0-F1) * AVTS1A(2,LF) + F1 * AVTS1B(2,LF)
                  DL1 = (1.0-F1) * AVPD1A(2,LF) + F1 * AVPD1B(2,LF)
                  GL1 = (1.0-F1) * AVPG1A(2,LF) + F1 * AVPG1B(2,LF)
               ELSE IF (AVTS1A(2,LF).NE.FBLANK) THEN
                  TL1 = AVTS1A(2,LF)
                  DL1 = AVPD1A(2,LF)
                  GL1 = AVPG1A(2,LF)
               ELSE IF (AVTS1B(2,LF).NE.FBLANK) THEN
                  TL1 = AVTS1B(2,LF)
                  DL1 = AVPD1B(2,LF)
                  GL1 = AVPG1B(2,LF)
               ELSE
                  TL1 = 1.0
                  DL1 = 1.0
                  GL1 = 1.0/64.0
                  END IF
            ELSE IF ((TS1A(2,LF,IA1).NE.FBLANK) .AND.
     *         (TS1B(2,LF,IA1).NE.FBLANK)) THEN
               TL1 = (1.0-F1) * TS1A(2,LF,IA1) + F1 * TS1B(2,LF,IA1)
               DL1 = (1.0-F1) * PD1A(2,LF,IA1) + F1 * PD1B(2,LF,IA1)
               GL1 = (1.0-F1) * PG1A(2,LF,IA1) + F1 * PG1B(2,LF,IA1)
            ELSE IF ((F1.LE.0.333) .AND. (TS1A(2,LF,IA1).NE.FBLANK))
     *         THEN
               TL1 = TS1A(2,LF,IA1)
               DL1 = PD1A(2,LF,IA1)
               GL1 = PG1A(2,LF,IA1)
            ELSE IF ((F1.GE.0.667) .AND. (TS1B(2,LF,IA1).NE.FBLANK))
     *         THEN
               TL1 = TS1B(2,LF,IA1)
               DL1 = PD1B(2,LF,IA1)
               GL1 = PG1B(2,LF,IA1)
            ELSE
               TL1 = FBLANK
               DL1 = FBLANK
               GL1 = FBLANK
               END IF
            IF (SKIP1(2,LF,IA2).GE.1) THEN
               IF ((AVTS1A(2,LF).NE.FBLANK) .AND.
     *            (AVTS1B(2,LF).NE.FBLANK)) THEN
                  TL2 = (1.0-F1) * AVTS1A(2,LF) + F1 * AVTS1B(2,LF)
                  DL2 = (1.0-F1) * AVPD1A(2,LF) + F1 * AVPD1B(2,LF)
                  GL2 = (1.0-F1) * AVPG1A(2,LF) + F1 * AVPG1B(2,LF)
               ELSE IF (AVTS1A(2,LF).NE.FBLANK) THEN
                  TL2 = AVTS1A(2,LF)
                  DL2 = AVPD1A(2,LF)
                  GL2 = AVPG1A(2,LF)
               ELSE IF (AVTS1B(2,LF).NE.FBLANK) THEN
                  TL2 = AVTS1B(2,LF)
                  DL2 = AVPD1B(2,LF)
                  GL2 = AVPG1B(2,LF)
               ELSE
                  TL2 = 1.0
                  DL2 = 1.0
                  GL2 = 1.0/64.0
                  END IF
            ELSE IF ((TS1A(2,LF,IA2).NE.FBLANK) .AND.
     *         (TS1B(2,LF,IA2).NE.FBLANK)) THEN
               TL2 = (1.0-F2) * TS1A(2,LF,IA2) + F2 * TS1B(2,LF,IA2)
               DL2 = (1.0-F2) * PD1A(2,LF,IA2) + F2 * PD1B(2,LF,IA2)
               GL2 = (1.0-F2) * PG1A(2,LF,IA2) + F2 * PG1B(2,LF,IA2)
            ELSE IF ((F2.LE.0.333) .AND. (TS1A(2,LF,IA2).NE.FBLANK))
     *         THEN
               TL2 = TS1A(2,LF,IA2)
               DL2 = PD1A(2,LF,IA2)
               GL2 = PG1A(2,LF,IA2)
            ELSE IF ((F2.GE.0.667) .AND. (TS1B(2,LF,IA2).NE.FBLANK))
     *         THEN
               TL2 = TS1B(2,LF,IA2)
               DL2 = PD1B(2,LF,IA2)
               GL2 = PG1B(2,LF,IA2)
            ELSE
               TL2 = FBLANK
               DL2 = FBLANK
               GL2 = FBLANK
               END IF
            IF ((TR1.EQ.FBLANK) .OR. (TR2.EQ.FBLANK)) THEN
               VFAC(1,LF) = FBLANK
               WFAC(1,LF) = FBLANK
            ELSE IF (OPTYPE.EQ.'PGN') THEN
               VFAC(1,LF) = GR1 * GR2 * 4096.
               IF (JD.LT.JD0) VFAC(1,LF) = 32768. / (GR1 * GR2)
               WFAC(1,LF) = 1.0
            ELSE
               VF = SQRT (VEFF(1,LF,IA1) * VEFF(1,LF,IA2))
               VFAC(1,LF) = VF * SQRT (DR1 * DR2) / VS
               IF (JD.LT.JD0) VFAC(1,LF) = VFAC(1,LF) * 32768./(GR1*GR2)
               WFAC(1,LF) = (TR1 * TR2) / WS / (VF**2)
               END IF
            IF ((TL1.EQ.FBLANK) .OR. (TL2.EQ.FBLANK)) THEN
               VFAC(2,LF) = FBLANK
               WFAC(2,LF) = FBLANK
            ELSE IF (OPTYPE.EQ.'PGN') THEN
               VFAC(2,LF) = GL1 * GL2 * 4096.
               IF (JD.LT.JD0) VFAC(2,LF) = 32768. / (GL1 * GL2)
               WFAC(2,LF) = 1.0
            ELSE
               VF = SQRT (VEFF(2,LF,IA1) * VEFF(2,LF,IA2))
               VFAC(2,LF) = VF * SQRT (DL1 * DL2) / VS
               IF (JD.LT.JD0) VFAC(2,LF) = VFAC(2,LF) * 32768./(GL1*GL2)
               WFAC(2,LF) = (TL1 * TL2) / WS / (VF**2)
               END IF
            IF ((TR1.EQ.FBLANK) .OR. (TL2.EQ.FBLANK)) THEN
               VFAC(3,LF) = FBLANK
               WFAC(3,LF) = FBLANK
            ELSE IF (OPTYPE.EQ.'PGN') THEN
               VFAC(3,LF) = GR1 * GL2 * 4096.
               IF (JD.LT.JD0) VFAC(3,LF) = 32768. / (GR1 * GL2)
               WFAC(3,LF) = 1.0
            ELSE
               VF = SQRT (VEFF(1,LF,IA1) * VEFF(2,LF,IA2))
               VFAC(3,LF) = VF * SQRT (DR1 * DL2) / VS
               IF (JD.LT.JD0) VFAC(3,LF) = VFAC(3,LF) * 32768./(GR1*GL2)
               WFAC(3,LF) = (TR1 * TL2) / WS / (VF**2)
               END IF

            IF ((TL1.EQ.FBLANK) .OR. (TR2.EQ.FBLANK)) THEN
               VFAC(4,LF) = FBLANK
               WFAC(4,LF) = FBLANK
            ELSE IF (OPTYPE.EQ.'PGN') THEN
               VFAC(4,LF) = GL1 * GR2 * 4096.
               IF (JD.LT.JD0) VFAC(4,LF) = 32768. / (GL1 * GR2)
               WFAC(4,LF) = 1.0
            ELSE
               VF = SQRT (VEFF(2,LF,IA1) * VEFF(1,LF,IA2))
               VFAC(4,LF) = VF * SQRT (DL1 * DR2) / VS
               IF (JD.LT.JD0) VFAC(4,LF) = VFAC(4,LF) * 32768./(GL1*GR2)
               WFAC(4,LF) = (TL1 * TR2) / WS / (VF**2)
               END IF
 20         CONTINUE
         END IF
C                                       apply second
      IF (INVER2.GT.0) THEN
         TEPS = MIN (6.*TENSEC, MAX (TENSEC, T2B(IA1)-T2B(IA1)))
         IF ((T.LT.T2A(IA1)-TINY) .OR. (T.GT.T2B(IA1)+TINY)) THEN
            EXTRAP = EXTRAP + 1
            IF ((T.LT.T2A(IA1)-TEPS) .OR. (T.GT.T2B(IA1)+TEPS)) THEN
               WRITE (MSGTXT,1020) T, IA1, T2A(IA1), T2B(IA1)
               IF (NMSG.LT.500) CALL MSGWRT (7)
               NMSG = NMSG + 1
               END IF
            END IF
         TEPS = MIN (6.*TENSEC, MAX (TENSEC, T2B(IA2)-T2B(IA2)))
         IF ((T.LT.T2A(IA2)-TINY) .OR. (T.GT.T2B(IA2)+TINY)) THEN
            EXTRAP = EXTRAP + 1
            IF ((T.LT.T2A(IA2)-TEPS) .OR. (T.GT.T2B(IA2)+TEPS)) THEN
               WRITE (MSGTXT,1020) T, IA2, T2A(IA2), T2B(IA2)
               IF (NMSG.LT.500) CALL MSGWRT (7)
               NMSG = NMSG + 1
               END IF
            END IF
         F1 = 0.0
         IF (T2A(IA1).NE.T2B(IA1)) F1 = (T - T2A(IA1)) /
     *      (T2B(IA1) - T2A(IA1))
         F2 = 0.0
         IF (T2A(IA2).NE.T2B(IA2)) F2 = (T - T2A(IA2)) /
     *      (T2B(IA2) - T2A(IA2))
         DO 40 LF = 1,NIF
            IF (SKIP2(1,LF,IA1).GE.1) THEN
               IF ((AVTS2A(1,LF).NE.FBLANK) .AND.
     *            (AVTS2B(1,LF).NE.FBLANK)) THEN
                  TR1 = (1.0-F1) * AVTS2A(1,LF) + F1 * AVTS2B(1,LF)
                  DR1 = (1.0-F1) * AVPD2A(1,LF) + F1 * AVPD2B(1,LF)
                  GR1 = (1.0-F1) * AVPG2A(1,LF) + F1 * AVPG2B(1,LF)
               ELSE IF (AVTS2A(1,LF).NE.FBLANK) THEN
                  TR1 = AVTS2A(1,LF)
                  DR1 = AVPD2A(1,LF)
                  GR1 = AVPG2A(1,LF)
               ELSE IF (AVTS2B(1,LF).NE.FBLANK) THEN
                  TR1 = AVTS2B(1,LF)
                  DR1 = AVPD2B(1,LF)
                  GR1 = AVPG2B(1,LF)
               ELSE
                  TR1 = 1.0
                  DR1 = 1.0
                  GR1 = 1.0/64.0
                  END IF
            ELSE IF ((TS2A(1,LF,IA1).NE.FBLANK) .AND.
     *         (TS2B(1,LF,IA1).NE.FBLANK)) THEN
               TR1 = (1.0-F1) * TS2A(1,LF,IA1) + F1 * TS2B(1,LF,IA1)
               DR1 = (1.0-F1) * PD2A(1,LF,IA1) + F1 * PD2B(1,LF,IA1)
               GR1 = (1.0-F1) * PG2A(1,LF,IA1) + F1 * PG2B(1,LF,IA1)
            ELSE IF ((F1.LE.0.333) .AND. (TS2A(1,LF,IA1).NE.FBLANK))
     *         THEN
               TR1 = TS2A(1,LF,IA1)
               DR1 = PD2A(1,LF,IA1)
               GR1 = PG2A(1,LF,IA1)
            ELSE IF ((F1.GE.0.667) .AND. (TS2B(1,LF,IA1).NE.FBLANK))
     *         THEN
               TR1 = TS2B(1,LF,IA1)
               DR1 = PD2B(1,LF,IA1)
               GR1 = PG2B(1,LF,IA1)
            ELSE
               TR1 = FBLANK
               DR1 = FBLANK
               GR1 = FBLANK
               END IF
            IF (SKIP2(1,LF,IA2).GE.1) THEN
               IF ((AVTS2A(1,LF).NE.FBLANK) .AND.
     *            (AVTS2B(1,LF).NE.FBLANK)) THEN
                  TR2 = (1.0-F1) * AVTS2A(1,LF) + F1 * AVTS2B(1,LF)
                  DR2 = (1.0-F1) * AVPD2A(1,LF) + F1 * AVPD2B(1,LF)
                  GR2 = (1.0-F1) * AVPG2A(1,LF) + F1 * AVPG2B(1,LF)
               ELSE IF (AVTS2A(1,LF).NE.FBLANK) THEN
                  TR2 = AVTS2A(1,LF)
                  DR2 = AVPD2A(1,LF)
                  GR2 = AVPG2A(1,LF)
               ELSE IF (AVTS2B(1,LF).NE.FBLANK) THEN
                  TR2 = AVTS2B(1,LF)
                  DR2 = AVPD2B(1,LF)
                  GR2 = AVPG2B(1,LF)
               ELSE
                  TR2 = 1.0
                  DR2 = 1.0
                  GR2 = 1.0/64.0
                  END IF
            ELSE IF ((TS2A(1,LF,IA2).NE.FBLANK) .AND.
     *         (TS2B(1,LF,IA2).NE.FBLANK)) THEN
               TR2 = (1.0-F2) * TS2A(1,LF,IA2) + F2 * TS2B(1,LF,IA2)
               DR2 = (1.0-F2) * PD2A(1,LF,IA2) + F2 * PD2B(1,LF,IA2)
               GR2 = (1.0-F2) * PG2A(1,LF,IA2) + F2 * PG2B(1,LF,IA2)
            ELSE IF ((F2.LE.0.333) .AND. (TS2A(1,LF,IA2).NE.FBLANK))
     *         THEN
               TR2 = TS2A(1,LF,IA2)
               DR2 = PD2A(1,LF,IA2)
               GR2 = PG2A(1,LF,IA2)
            ELSE IF ((F2.GE.0.667) .AND. (TS2B(1,LF,IA2).NE.FBLANK))
     *         THEN
               TR2 = TS2B(1,LF,IA2)
               DR2 = PD2B(1,LF,IA2)
               GR2 = PG2B(1,LF,IA2)
            ELSE
               TR2 = FBLANK
               DR2 = FBLANK
               GR2 = FBLANK
               END IF
            IF (SKIP2(2,LF,IA1).GE.1) THEN
               IF ((AVTS2A(2,LF).NE.FBLANK) .AND.
     *            (AVTS2B(2,LF).NE.FBLANK)) THEN
                  TL1 = (1.0-F1) * AVTS2A(2,LF) + F1 * AVTS2B(2,LF)
                  DL1 = (1.0-F1) * AVPD2A(2,LF) + F1 * AVPD2B(2,LF)
                  GL1 = (1.0-F1) * AVPG2A(2,LF) + F1 * AVPG2B(2,LF)
               ELSE IF (AVTS2A(2,LF).NE.FBLANK) THEN
                  TL1 = AVTS2A(2,LF)
                  DL1 = AVPD2A(2,LF)
                  GL1 = AVPG2A(2,LF)
               ELSE IF (AVTS2B(2,LF).NE.FBLANK) THEN
                  TL1 = AVTS2B(2,LF)
                  DL1 = AVPD2B(2,LF)
                  GL1 = AVPG2B(2,LF)
               ELSE
                  TL1 = 1.0
                  DL1 = 1.0
                  GL1 = 1.0/64.0
                  END IF
            ELSE IF ((TS2A(2,LF,IA1).NE.FBLANK) .AND.
     *         (TS2B(2,LF,IA1).NE.FBLANK)) THEN
               TL1 = (1.0-F1) * TS2A(2,LF,IA1) + F1 * TS2B(2,LF,IA1)
               DL1 = (1.0-F1) * PD2A(2,LF,IA1) + F1 * PD2B(2,LF,IA1)
               GL1 = (1.0-F1) * PG2A(2,LF,IA1) + F1 * PG2B(2,LF,IA1)
            ELSE IF ((F1.LE.0.333) .AND. (TS2A(2,LF,IA1).NE.FBLANK))
     *         THEN
               TL1 = TS2A(2,LF,IA1)
               DL1 = PD2A(2,LF,IA1)
               GL1 = PG2A(2,LF,IA1)
            ELSE IF ((F1.GE.0.667) .AND. (TS2B(2,LF,IA1).NE.FBLANK))
     *         THEN
               TL1 = TS2B(2,LF,IA1)
               DL1 = PD2B(2,LF,IA1)
               GL1 = PG2B(2,LF,IA1)
            ELSE
               TL1 = FBLANK
               DL1 = FBLANK
               GL1 = FBLANK
               END IF
            IF (SKIP2(2,LF,IA2).GE.1) THEN
               IF ((AVTS2A(2,LF).NE.FBLANK) .AND.
     *            (AVTS2B(2,LF).NE.FBLANK)) THEN
                  TL2 = (1.0-F1) * AVTS2A(2,LF) + F1 * AVTS2B(2,LF)
                  DL2 = (1.0-F1) * AVPD2A(2,LF) + F1 * AVPD2B(2,LF)
                  GL2 = (1.0-F1) * AVPG2A(2,LF) + F1 * AVPG2B(2,LF)
               ELSE IF (AVTS2A(2,LF).NE.FBLANK) THEN
                  TL2 = AVTS2A(2,LF)
                  DL2 = AVPD2A(2,LF)
                  GL2 = AVPG2A(2,LF)
               ELSE IF (AVTS2B(2,LF).NE.FBLANK) THEN
                  TL2 = AVTS2B(2,LF)
                  DL2 = AVPD2B(2,LF)
                  GL2 = AVPG2B(2,LF)
               ELSE
                  TL2 = 1.0
                  DL2 = 1.0
                  GL2 = 1.0/64.0
                  END IF
            ELSE IF ((TS2A(2,LF,IA2).NE.FBLANK) .AND.
     *         (TS2B(2,LF,IA2).NE.FBLANK)) THEN
               TL2 = (1.0-F2) * TS2A(2,LF,IA2) + F2 * TS2B(2,LF,IA2)
               DL2 = (1.0-F2) * PD2A(2,LF,IA2) + F2 * PD2B(2,LF,IA2)
               GL2 = (1.0-F2) * PG2A(2,LF,IA2) + F2 * PG2B(2,LF,IA2)
            ELSE IF ((F2.LE.0.333) .AND. (TS2A(2,LF,IA2).NE.FBLANK))
     *         THEN
               TL2 = TS2A(2,LF,IA2)
               DL2 = PD2A(2,LF,IA2)
               GL2 = PG2A(2,LF,IA2)
            ELSE IF ((F2.GE.0.667) .AND. (TS2B(2,LF,IA2).NE.FBLANK))
     *         THEN
               TL2 = TS2B(2,LF,IA2)
               DL2 = PD2B(2,LF,IA2)
               GL2 = PG2B(2,LF,IA2)
            ELSE
               TL2 = FBLANK
               DL2 = FBLANK
               GL2 = FBLANK
               END IF
            IF ((TR1.EQ.FBLANK) .OR. (TR2.EQ.FBLANK) .OR.
     *         (VFAC(1,LF).EQ.FBLANK)) THEN
               VFAC(1,LF) = FBLANK
               WFAC(1,LF) = FBLANK
            ELSE IF (OPTYPE.EQ.'PGN') THEN
               IF (JD.LT.JD0) THEN
                  VFAC(1,LF) = VFAC(1,LF) * (GR1 * GR2) / 32768.
               ELSE
                  VFAC(1,LF) = VFAC(1,LF) / (GR1 * GR2 * 4096.)
                  END IF
               WFAC(1,LF) = 1.0
            ELSE
               VF = SQRT (VEFF(1,LF,IA1) * VEFF(1,LF,IA2))
               VFAC(1,LF) = VFAC(1,LF) / SQRT (DR1 * DR2) * VS/VF
               IF (JD.LT.JD0) VFAC(1,LF) = VFAC(1,LF) * (GR1*GR2)/32768.
               WFAC(1,LF) = WFAC(1,LF) / (TR1 * TR2) * WS * (VF**2)
               END IF
            IF ((TL1.EQ.FBLANK) .OR. (TL2.EQ.FBLANK) .OR.
     *         (VFAC(2,LF).EQ.FBLANK)) THEN
               VFAC(2,LF) = FBLANK
               WFAC(2,LF) = FBLANK
            ELSE IF (OPTYPE.EQ.'PGN') THEN
               IF (JD.LT.JD0) THEN
                  VFAC(2,LF) = VFAC(2,LF) * (GL1 * GL2) / 32768.
               ELSE
                  VFAC(2,LF) = VFAC(2,LF) / (GL1 * GL2 * 4096.)
                  END IF
               WFAC(2,LF) = 1.0
            ELSE
               VF = SQRT (VEFF(2,LF,IA1) * VEFF(2,LF,IA2))
               VFAC(2,LF) = VFAC(2,LF) / SQRT (DL1 * DL2) * VS/VF
               IF (JD.LT.JD0) VFAC(2,LF) = VFAC(2,LF) * (GL1*GL2)/32768.
               WFAC(2,LF) = WFAC(2,LF) / (TL1 * TL2) * WS * (VF**2)
               END IF
            IF ((TR1.EQ.FBLANK) .OR. (TL2.EQ.FBLANK) .OR.
     *         (VFAC(3,LF).EQ.FBLANK)) THEN
               VFAC(3,LF) = FBLANK
               WFAC(3,LF) = FBLANK
            ELSE IF (OPTYPE.EQ.'PGN') THEN
               IF (JD.LT.JD0) THEN
                  VFAC(3,LF) = VFAC(3,LF) * (GR1 * GL2) / 32768.
               ELSE
                  VFAC(3,LF) = VFAC(3,LF) / (GR1 * GL2 * 4096.)
                  END IF
               WFAC(3,LF) = 1.0
            ELSE
               VF = SQRT (VEFF(1,LF,IA1) * VEFF(2,LF,IA2))
               VFAC(3,LF) = VFAC(3,LF) / SQRT (DR1 * DL2) * VS/VF
               IF (JD.LT.JD0) VFAC(3,LF) = VFAC(3,LF) * (GR1*GL2)/32768.
               WFAC(3,LF) = WFAC(3,LF) / (TR1 * TL2) * WS * (VF**2)
               END IF
            IF ((TL1.EQ.FBLANK) .OR. (TR2.EQ.FBLANK) .OR.
     *         (VFAC(4,LF).EQ.FBLANK)) THEN
               VFAC(4,LF) = FBLANK
               WFAC(4,LF) = FBLANK
            ELSE IF (OPTYPE.EQ.'PGN') THEN
               IF (JD.LT.JD0) THEN
                  VFAC(4,LF) = VFAC(4,LF) * (GL1 * GR2) / 32768.
               ELSE
                  VFAC(4,LF) = VFAC(4,LF) / (GL1 * GR2 * 4096.)
                  END IF
               WFAC(4,LF) = 1.0
            ELSE
               VF = SQRT (VEFF(2,LF,IA1) * VEFF(1,LF,IA2))
               VFAC(4,LF) = VFAC(4,LF) / SQRT (DL1 * DR2) * VS/VF
               IF (JD.LT.JD0) VFAC(4,LF) = VFAC(4,LF) * (GL1*GR2)/32768.
               WFAC(4,LF) = WFAC(4,LF) / (TL1 * TR2) * WS * (VF**2)
               END IF
 40         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SY T1=',F11.6,' AN',I3,' NOT IN',2F14.6)
 1020 FORMAT ('SY T2=',F11.6,' AN',I3,' NOT IN',2F14.6)
      END
      SUBROUTINE DATFLG (RPARM, VIS, DROP, IERR)
C-----------------------------------------------------------------------
C   Flags data specified in flagging table
C   Inputs:
C      RPARM(*)   R    Random parameter array
C      VIS(3,*)   R    Visibility array
C   Inputs from include DSEL.INC:
C      CURSOU     I    Current source number
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
C      RPARM(*)   R    Random parameter array
C      VIS(3,*)   R    Visibility array
C      DROP       L    True if data all flagged.
C      IERR       I    Return code, 0=OK, else NXTFLG error number.
C-----------------------------------------------------------------------
      REAL      RPARM(*), VIS(3,*)
      LOGICAL   DROP
      INTEGER   IERR
C
      INTEGER   IFLAG, KBASE, A1, A2, FLGA, SUBA, JIF, JCHAN, JPOLN,
     *   LIMF1, LIMF2, LIMC1, LIMC2, IFADD, INDEX, STADD, IPOLPT, LFQ
      LOGICAL   GOOD
      REAL      TIME, SUM
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      DROP = .FALSE.
C                                       Check if new time
      TIME = RPARM(1+ILOCT)
      IF (TMFLST.LT.TIME) CALL NXTFLG (TIME, .FALSE., IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Check if there are current flags
      IF (NUMFLG.LE.0) GO TO 999
C                                       Loop thru flagging criteria
      IF (ILOCB.GE.0) THEN
         KBASE = RPARM(1+ILOCB) + 0.1
         A1 = KBASE / 256
         A2 = KBASE - 256 * A1
         SUBA = (RPARM(1+ILOCB) - KBASE) * 100.0 + 1.5
      ELSE
         A1 = RPARM(1+ILOCA1) + 0.1
         A2 = RPARM(1+ILOCA2) + 0.1
         SUBA = RPARM(1+ILOCSA) + 0.1
         END IF
      KBASE = 32768 * MIN (A1,A2)  + MAX (A1,A2)
      DO 500 IFLAG = 1,NUMFLG
C                                       Check time if needed
         IF (.NOT.TIMORD) THEN
            IF ((TIME.LT.FLGTST(IFLAG)) .OR. (TIME.GT.FLGTND(IFLAG)))
     *         GO TO 500
            END IF
C                                       Check source
         IF ((FLGSOU(IFLAG).NE.CURSOU) .AND. (FLGSOU(IFLAG).NE.0) .AND.
     *      (CURSOU.NE.0)) GO TO 500
C                                       Check antenna
         FLGA = FLGANT(IFLAG)
         IF ((FLGA.NE.0) .AND. (FLGA.NE.A1) .AND. (FLGA.NE.A2))
     *      GO TO 500
C                                       Check baseline
         IF ((FLGBAS(IFLAG).NE.0) .AND. (FLGBAS(IFLAG).NE.KBASE))
     *      GO TO 500
C                                       Check subarray
         IF ((FLGSUB(IFLAG).GT.0) .AND. (FLGSUB(IFLAG).NE.SUBA))
     *      GO TO 500
C                                       Check freqid.: may be changed
C                                       from input to 1 already
         IF (ILOCFQ.GE.0) THEN
            IF (FRQSEL.GT.0) THEN
               LFQ = FRQSEL
            ELSE
               LFQ = RPARM(1+ILOCFQ) + 0.1
               END IF
            IF ((FLGFQD(IFLAG).GT.0) .AND. (FLGFQD(IFLAG).NE.LFQ) .AND.
     *         (LFQ.GT.0)) GO TO 500
            END IF
C                                       Some data to be flagged
C                                       Set limits
         LIMF1 = FLGBIF(IFLAG)
         LIMF2 = FLGEIF(IFLAG)
         LIMC1 = FLGBCH(IFLAG)
         LIMC2 = FLGECH(IFLAG)
C                                       Loop over polarizations
         IPOLPT = ABS(KCOR0) - 1
         IF (KCOR0.LT.-4) IPOLPT = IPOLPT - 4
         DO 400 JPOLN = 1,KNCOR
            IF (FLGPOL(JPOLN+IPOLPT,IFLAG)) THEN
               STADD = (JPOLN-1) * KNCS + 1
C                                       Loop over IF
               DO 300 JIF = LIMF1,LIMF2
                  INDEX = STADD + (JIF-1) * KNCIF + (LIMC1-1) * KNCF
                  IF (LIMC1.EQ.LIMC2) THEN
C                                       Single channel
                     VIS(3,INDEX) = - ABS (VIS(3,INDEX))
                  ELSE
C                                       Loop over channel
                     DO 200 JCHAN = LIMC1,LIMC2
C                                       Flag
                        VIS(3,INDEX) = - ABS (VIS(3,INDEX))
                        INDEX = INDEX + KNCF
 200                    CONTINUE
                     END IF
 300              CONTINUE
               END IF
 400        CONTINUE
 500     CONTINUE
C                                       Check if data all bad
      GOOD = .FALSE.
C                                       Loop over IF
      DO 530 JIF = BIF,EIF
         IFADD = (JIF-1) * KNCIF + 1
C                                       Loop over polarizations
         DO 520 JPOLN = 1,KNCOR
            INDEX = IFADD + (JPOLN-1) * KNCS + (BCHAN-1) * KNCF
C                                       Single channel
            IF (BCHAN.EQ.ECHAN) THEN
               GOOD = GOOD .OR. (VIS(3,INDEX).GT.0.0)
C                                       Multiple channels
            ELSE
               SUM = 0.0
               DO 510 JCHAN = BCHAN,ECHAN
                  SUM = SUM + MAX (0.0, VIS(3,INDEX))
                  INDEX = INDEX + KNCF
 510              CONTINUE
               GOOD = GOOD .OR. (SUM.GT.0.0)
               END IF
 520        CONTINUE
 530     CONTINUE
      DROP = .NOT.GOOD
C
 999  RETURN
      END
      SUBROUTINE NXTFLG (TIME, TABLE, IERR)
C-----------------------------------------------------------------------
C   Updates flagging tables in common fron an FG table.
C   Inputs:
C      TIME     R      Current time (days) for flag entries
C      TABLE    L      If table true then ignore baseline dependent
C                      and channel dependent flags
C   Inputs from common /CFMINF/(INCLUDEs C/DSEL.INC):
C      NUMFLG   I      number of current FLAG entries.
C      FGKOLS   I(MAXFGC)   The column pointer array in order, SOURCE,
C                      SUBARRAY, FREQID, ANTS, TIMERANG, IFS, CHANS,
C                      PFLAGS, REASON
C      FGNUMV   I(MAXFGC)   Element count for each column
C      IFGRNO   I      Current FLAG file record.
C   Output to common /CFMINF/:
C      NUMFLG   I      Number of flagging entries.
C      TMFLST   R      Time of last visibility for which flagging
C                      was checked.
C      FLGSOU   I(*)   Source id numbers to flag, 0=all.
C      FLGANT   I(*)   Antenna numbers to flag, 0=all.
C      FLGBAS   I(*)   Baseline (A1*32768+A2) numbers to flag, 0=all.
C      FLGSUB   I(*)   Subarray numbers to flag, 0=all.
C      FLGFQD   I(*)   Freqid numbers to flag, <=0=all.
C                      Following should have defaults filled in.
C      FLGBIF   I(*)   First IF to flag.
C      FLGEIF   I(*)   Highest IF to flag.
C      FLGBCH   I(*)   First channel to flag.
C      FLGECH   I(*)   Highest channel to flag.
C      FLGPOL   L(4,*)   Flags for the polarizations, should correspond
C                      to selected polarization types.
C      FLGTST   R(*)   Start time of flag.
C      FLGTND   R(*)   End time of flag.
C   Output:
C      IERR     I      Return code, 0=OK, else TABIO error number.
C                         10 => too many flags
C-----------------------------------------------------------------------
      REAL      TIME
      LOGICAL   TABLE
      INTEGER   IERR
C
      INTEGER   J, NDROP, LIMIT, RECI(30), MXFLG, SOUKOL, SUBKOL,
     *   FRQKOL, ANTKOL, TIMKOL, IFKOL, CHKOL, POLKOL, REAKOL, A1, A2,
     *   IT, I4, NFGREC, I, LIMIT4
      REAL      RECORD(31)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (RECORD, RECI)
      EQUIVALENCE (FGKOLS(1), SOUKOL), (FGKOLS(2), SUBKOL),
     *   (FGKOLS(3), FRQKOL), (FGKOLS(4), ANTKOL), (FGKOLS(5),TIMKOL),
     *   (FGKOLS(6), IFKOL),  (FGKOLS(7), CHKOL), (FGKOLS(8), POLKOL),
     *   (FGKOLS(9), REAKOL)
      DATA I4 /4/
C-----------------------------------------------------------------------
      IERR = 0
      MXFLG = MAXFLG
      TMFLST = TIME
C                                       Check if any flags expired.
C                                       Check if any flags expired.
 10   NDROP = 0
C                                       Find highest number expired flag
      IF ((NUMFLG.GT.0) .AND. (TIMORD)) THEN
         DO 20 I = 1,NUMFLG
            IF (FLGTND(I).LT.TIME) NDROP = I
 20         CONTINUE
         END IF
C                                       Compress, dropping flag.
      IF (NDROP.GT.0) THEN
         IF (NDROP.LT.NUMFLG) THEN
            LIMIT = NDROP + 1
            DO 150 I = LIMIT,NUMFLG
               IT = I - 1
               FLGTST(IT) = FLGTST(I)
               FLGTND(IT) = FLGTND(I)
               FLGSOU(IT) = FLGSOU(I)
               FLGANT(IT) = FLGANT(I)
               FLGFQD(IT) = FLGFQD(I)
               FLGBAS(IT) = FLGBAS(I)
               FLGSUB(IT) = FLGSUB(I)
               FLGBIF(IT) = FLGBIF(I)
               FLGEIF(IT) = FLGEIF(I)
               FLGBCH(IT) = FLGBCH(I)
               FLGECH(IT) = FLGECH(I)
               FLGPOL(1,IT) = FLGPOL(1,I)
               FLGPOL(2,IT) = FLGPOL(2,I)
               FLGPOL(3,IT) = FLGPOL(3,I)
               FLGPOL(4,IT) = FLGPOL(4,I)
 150           CONTINUE
            END IF
         NUMFLG = NUMFLG - 1
         GO TO 10
         END IF
C                                       Find next valid flag.
      NFGREC = FGBUFF(5)
C                                       Check if list exhausted
      IF (IFGRNO.GT.NFGREC) GO TO 999
C                                       Loop through records
 310  LIMIT4 = IFGRNO
      DO 360 I = LIMIT4,NFGREC
         IFGRNO = I
         IERR = 1
C                                       Read record.
         CALL TABIO ('READ', 0, IFGRNO, RECORD, FGBUFF, IERR)
C                                       Check if flagged
         IF (IERR.LT.0) GO TO 360
C                                       Check error
         IF (IERR.GT.0) GO TO 999
C                                       Check time.
         IF (TIMORD) THEN
            IF (TIME.LT.RECORD(TIMKOL)) GO TO 999
            IF (TIME.GT.RECORD(TIMKOL+1)) GO TO 360
            END IF
C                                       Check FQ ID.
         IF (RECI(FRQKOL).GT.0) THEN
            IF ((RECI(FRQKOL).NE.FRQSEL) .AND. (FRQSEL.GT.0) .AND.
     *         (RECI(FRQKOL).GT.0)) GO TO 360
            END IF
C                                       Check that starting IF
C                                       is in range
         IF ((RECI(IFKOL).GT.0).AND.
     *      (RECI(IFKOL).GT.CATUV(KINAX+KLOCIF))) GO TO 360
C                                       Check that starting
C                                       channel is in range
         IF ((RECI(CHKOL).GT.0).AND.
     *      (RECI(CHKOL).GT.CATUV(KINAX+KLOCFY))) GO TO 360
C                                       Does source number matter?
         IF ((RECI(SOUKOL).LE.0) .OR. (NSOUWD.LE.0)) GO TO 500
C                                       Search source lists
C                                       in UVCOP, SOUWAN is
C                                       list of wanted sources
         DO 340 J = 1,NSOUWD
            IF (RECI(SOUKOL).EQ.SOUWAN(J)) GO TO 500
 340        CONTINUE
 360     CONTINUE
      IERR = 0
      GO TO 999
C                                       Next entry
 500  NUMFLG = NUMFLG + 1
C                                       Check if too big
      IERR = 0
C                                       Fill in tables
      FLGTST(NUMFLG) = RECORD(TIMKOL)
      FLGTND(NUMFLG) = RECORD(TIMKOL+1)
      FLGSOU(NUMFLG) = RECI(SOUKOL)
      FLGFQD(NUMFLG) = RECI(FRQKOL)
      A1 = MIN (RECI(ANTKOL), RECI(ANTKOL+1))
      A2 = MAX (RECI(ANTKOL), RECI(ANTKOL+1))
      IF (A1.LE.0) THEN
         FLGANT(NUMFLG) = A2
         FLGBAS(NUMFLG) = 0
      ELSE
         FLGANT(NUMFLG) = RECI(ANTKOL)
         FLGBAS(NUMFLG) = A1*32768 + A2
         END IF
      FLGSUB(NUMFLG) = RECI(SUBKOL)
      FLGBIF(NUMFLG) = RECI(IFKOL)
      FLGEIF(NUMFLG) = RECI(IFKOL+1)
      IF (FLGBIF(NUMFLG).LE.0) FLGBIF(NUMFLG) = 1
      IF (FLGEIF(NUMFLG).LE.0) THEN
         IF (KLOCIF.GT.0) FLGEIF(NUMFLG) = CATUV (KINAX+KLOCIF)
         IF (KLOCIF.LE.0) FLGEIF(NUMFLG) = 1
         END IF
      FLGBCH(NUMFLG) = RECI(CHKOL)
      FLGECH(NUMFLG) = MIN (CATUV(KINAX+KLOCFY), RECI(CHKOL+1))
      IF (FLGBCH(NUMFLG).LE.0) FLGBCH(NUMFLG) = 1
      IF (FLGECH(NUMFLG).LE.0) FLGECH(NUMFLG) = CATUV (KINAX+KLOCFY)
C                                       Ensure that IF and channel
C                                       selection are in range
      FLGEIF(NUMFLG) = MIN (FLGEIF(NUMFLG), CATUV(KINAX+KLOCIF))
      FLGECH(NUMFLG) = MIN (FLGECH(NUMFLG), CATUV(KINAX+KLOCFY))
C
      CALL LG2BIT (I4, FLGPOL(1,NUMFLG), RECI(POLKOL), -1)
C                                       table ignores baseline based
C                                       and channel based
      IF (TABLE) THEN
         IF ((FLGBAS(NUMFLG).NE.0) .OR. (FLGBCH(NUMFLG).NE.1) .OR.
     *      (FLGECH(NUMFLG).NE.CATUV(KINAX+KLOCFY))) NUMFLG = NUMFLG - 1
         END IF
C                                       test for at limit
      IF (NUMFLG.GT.MXFLG-1) THEN
         IERR = 10
         WRITE (MSGTXT,1500) MXFLG-1
         GO TO 990
         END IF
C                                       Increment flag counter
      IFGRNO = IFGRNO + 1
C                                       Loop back for next
      IF (IFGRNO.LE.NFGREC) GO TO 310
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1500 FORMAT ('TOO MANY FLAGS AT SAME TIME (>',I7,')')
      END
      SUBROUTINE TYAPCL (IRET)
C-----------------------------------------------------------------------
C   TYAPCL is written under protest to apply the gain factors to a CL
C   table rather than ti the data.  This is bad because
C      (1) Abrupt jumps may occur in the gains at any time although
C          they are more likely as scan boundaries.  A CL table can
C          handle jumps at scan boundaries, but only if the original
C          scan structure is known.  That is often not the case despite
C          valiant efforts in the code to try to insure this.
C      (2) The TY and SY tables provide good estimates of the data
C          weights.  If the data are not copied, then this capability
C          is lost since it would in any case require a complete
C          re-write of the data set.
C      (3) Real data weights (after computation in FILLM or TYAPL or
C          REWAY) need to have any residual amplitude gains applied to
C          them.  This is because the residual gains are due to errors
C          in the recorded Tcal's mostly.  But if the large gain factors
C          from TYAPL are mixed with the other corrections (as they are
C          in a CL application), then any correction to weights is in
C          error.
C   TYAPCL copies the highest CL table version applying gains found
C   for the times and antennas found in the CL table.
C   Outputs:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   CLBUFI(512), CLBUFO(512), NUMANT, NUMIF, NTERM, ICLRNO,
     *   CLKOLI(MAXCLC), CLNUMI(MAXCLC), CLKOLO(MAXCLC), CLNUMO(MAXCLC),
     *   VER, LUNI, LUNO, SOURID, ANTNO, SUBA, FREQID, REFA(2,MAXIF),
     *   IREC, NREC, NUMPOL
      REAL      GMMOD, TIMEI, IFR, DOPOFF(MAXIF), ATMOS(2), DATMOS(2),
     *   MBDELY(2), CLOCK(2), DCLOCK(2), DISP(2), DDISP(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *   WEIGHT(2,MAXIF)
      DOUBLE PRECISION TIME, GEODLY(10)
      INCLUDE 'TYAPL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNI, LUNO /76, 77/
C-----------------------------------------------------------------------
      MSGTXT = '*** Doing CL-table method: read limitations in' //
     *   ' HELP file ***'
      CALL MSGWRT (2)
C                                       find highest CL table
      CALL FNDEXT ('CL', CATBLK, CLVERI)
      IF (CLVERI.LE.0) THEN
         MSGTXT = 'NO CL TABLE IN INPUT FILE: QUIT'
         IRET = 10
         GO TO 990
         END IF
      CALL CALINI ('READ', CLBUFI, DISKIN, OLDCNO, CLVERI, CATBLK, LUNI,
     *   ICLRNO, CLKOLI, CLNUMI, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT CL TABLE'
         GO TO 990
         END IF
      VER = CLVERI + 1
      CALL CALINI ('WRIT', CLBUFO, DISKIN, OLDCNO, VER, CATBLK, LUNO,
     *   ICLRNO, CLKOLO, CLNUMO, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT CL TABLE'
         GO TO 990
         END IF
      NREC = CLBUFI(5)
      DO 100 IREC = 1,NREC
         ICLRNO = IREC
         CALL TABCAL ('READ', CLBUFI, ICLRNO, CLKOLI, CLNUMI, NUMPOL,
     *      NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *      GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *      DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING OLD CL TABLE'
            GO TO 990
            END IF
         CALL TYAPPC (TIME, ANTNO, NUMPOL, NUMIF,CREAL, CIMAG, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'FINDING GAIN FACTORS'
            GO TO 990
            END IF
         ICLRNO = IREC
         CALL TABCAL ('WRIT', CLBUFO, ICLRNO, CLKOLO, CLNUMO, NUMPOL,
     *      NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *      GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *      DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING NEW CL TABLE'
            GO TO 990
            END IF
 100     CONTINUE
      CALL TABCAL ('CLOS', CLBUFI, ICLRNO, CLKOLI, CLNUMI, NUMPOL,
     *   NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *   GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *   DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING OLD CL TABLE'
         CALL MSGWRT (7)
         END IF
      CALL TABCAL ('CLOS', CLBUFO, ICLRNO, CLKOLO, CLNUMO, NUMPOL,
     *   NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *   GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *   DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING NEW CL TABLE'
         CALL MSGWRT (7)
         IRET = 0
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TYAPCL: ERROR',I4,' ON ',A)
      END
      SUBROUTINE TYAPPC (TIME, ANTNO, NUMPOL, NUMIF, CREAL, CIMAG, IRET)
C-----------------------------------------------------------------------
C   Finds the correction for a specific time and antenna and applies it
C   Inputs:
C      TIME     D        Time (days)
C      ANTNO    I        Antenna number
C      NUMPOL   I        Number polarizations
C      NUMIF    I        Number IFs
C   In/out:
C      CREAL    R(2,*)   Real part of CL gain
C      CIMAG    R(2,*)   Imaginary part of CL gain
C-----------------------------------------------------------------------
      DOUBLE PRECISION TIME
      INTEGER   ANTNO, NUMPOL, NUMIF, IRET
      REAL      CREAL(2,*), CIMAG(2,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'TYAPL.INC'
      INCLUDE 'TYTABS.INC'
      INTEGER   JIF, JS
      REAL      T, VFAC(2,MAXIF)
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       check/get TY values
      T = TIME
      IF (T.GT.CURTIM) THEN
         IF (XTYPE.EQ.'TY') THEN
            CALL GETTY (T, IRET)
         ELSE
            CALL GETSY (T, IRET)
            END IF
         IF (IRET.NE.0) GO TO 999
         CURTIM = T
         END IF
C                                       get current values
      IF (XTYPE.EQ.'TY') THEN
         CALL GETTYC (T, ANTNO, VFAC)
      ELSE
         CALL GETSYC (T, ANTNO, VFAC)
         END IF
C                                       correct values
      DO 40 JIF = 1,NUMIF
         DO 30 JS = 1,NUMPOL
            IF (VFAC(JS,JIF).NE.FBLANK) THEN
               CREAL(JS,JIF) = CREAL(JS,JIF) * VFAC(JS,JIF)
               CIMAG(JS,JIF) = CIMAG(JS,JIF) * VFAC(JS,JIF)
            ELSE
               CREAL(JS,JIF) = FBLANK
               CIMAG(JS,JIF) = FBLANK
               END IF
 30         CONTINUE
 40      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE GETTYC (T, IA1, VFAC)
C-----------------------------------------------------------------------
C   GETTYC interpolates as needed in the tables of Nominal sensitivities
C   and returns the factors to be applied
C   Inputs:
C      T      R
C      IA1    I
C   Outputs:
C      VFAC   R(2,*)   Visibility multiplier (pol, IF)
C-----------------------------------------------------------------------
      REAL      T, VFAC(2,*)
      INTEGER   IA1
C
      INTEGER   I, LF
      REAL      F1, TR1, TL1, TEPS, TENSEC, TINY
      INCLUDE 'TYAPL.INC'
      INCLUDE 'TYTABS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      I = 2 * MAXIF
      CALL RFILL (I, 1.0, VFAC)
      TENSEC = 10.1 / (24.0 * 3600.0)
      TINY = TENSEC / 300.0
C                                       remove a nom sensitivity
      IF (INVER1.GT.0) THEN
         TEPS = MIN (6.*TENSEC, MAX (TENSEC, T1B(IA1)-T1B(IA1)))
         IF ((T.LT.T1A(IA1)-TINY) .OR. (T.GT.T1B(IA1)+TINY)) THEN
            EXTRAP = EXTRAP + 1
            IF ((T.LT.T1A(IA1)-TEPS) .OR. (T.GT.T1B(IA1)+TEPS)) THEN
               WRITE (MSGTXT,1000) T, IA1, T1A(IA1), T1B(IA1)
               IF (NMSG.LT.500) CALL MSGWRT (7)
               NMSG = NMSG + 1
               END IF
            END IF
         F1 = 0.0
         IF (T1A(IA1).NE.T1B(IA1)) F1 = (T - T1A(IA1)) /
     *      (T1B(IA1) - T1A(IA1))
         DO 20 LF = 1,NIF
            IF ((TS1A(1,LF,IA1).NE.FBLANK) .AND.
     *         (TS1B(1,LF,IA1).NE.FBLANK)) THEN
               TR1 = (1.0-F1) * TS1A(1,LF,IA1) + F1 * TS1B(1,LF,IA1)
            ELSE IF ((F1.LE.0.333) .AND. (TS1A(1,LF,IA1).NE.FBLANK))
     *         THEN
               TR1 = TS1A(1,LF,IA1)
            ELSE IF ((F1.GE.0.667) .AND. (TS1B(1,LF,IA1).NE.FBLANK))
     *         THEN
               TR1 = TS1B(1,LF,IA1)
            ELSE
               TR1 = FBLANK
               END IF
            IF ((TS1A(2,LF,IA1).NE.FBLANK) .AND.
     *         (TS1B(2,LF,IA1).NE.FBLANK)) THEN
               TL1 = (1.0-F1) * TS1A(2,LF,IA1) + F1 * TS1B(2,LF,IA1)
            ELSE IF ((F1.LE.0.333) .AND. (TS1A(2,LF,IA1).NE.FBLANK))
     *         THEN
               TL1 = TS1A(2,LF,IA1)
            ELSE IF ((F1.GE.0.667) .AND. (TS1B(2,LF,IA1).NE.FBLANK))
     *         THEN
               TL1 = TS1B(2,LF,IA1)
            ELSE
               TL1 = FBLANK
               END IF
            IF (TR1.NE.FBLANK) THEN
               VFAC(1,LF) = 1.0 / SQRT (TR1)
            ELSE
               VFAC(1,LF) = FBLANK
               END IF
            IF (TL1.NE.FBLANK) THEN
               VFAC(2,LF) = 1.0 / SQRT (TL1)
            ELSE
               VFAC(2,LF) = FBLANK
               END IF
 20         CONTINUE
         END IF
C                                       apply second
      IF (INVER2.GT.0) THEN
         TEPS = MIN (6.*TENSEC, MAX (TENSEC, T2B(IA1)-T2B(IA1)))
         IF ((T.LT.T2A(IA1)-TINY) .OR. (T.GT.T2B(IA1)+TINY)) THEN
            EXTRAP = EXTRAP + 1
            IF ((T.LT.T2A(IA1)-TEPS) .OR. (T.GT.T2B(IA1)+TEPS)) THEN
               WRITE (MSGTXT,1020) T, IA1, T2A(IA1), T2B(IA1)
               IF (NMSG.LT.500) CALL MSGWRT (7)
               NMSG = NMSG + 1
               END IF
            END IF
         F1 = 0.0
         IF (T2A(IA1).NE.T2B(IA1)) F1 = (T - T2A(IA1)) /
     *      (T2B(IA1) - T2A(IA1))
         DO 40 LF = 1,NIF
            IF ((TS2A(1,LF,IA1).NE.FBLANK) .AND.
     *         (TS2B(1,LF,IA1).NE.FBLANK)) THEN
               TR1 = (1.0-F1) * TS2A(1,LF,IA1) + F1 * TS2B(1,LF,IA1)
            ELSE IF ((F1.LE.0.333) .AND. (TS2A(1,LF,IA1).NE.FBLANK))
     *         THEN
               TR1 = TS2A(1,LF,IA1)
            ELSE IF ((F1.GE.0.667) .AND. (TS2B(1,LF,IA1).NE.FBLANK))
     *         THEN
               TR1 = TS2B(1,LF,IA1)
            ELSE
               TR1 = FBLANK
               END IF
            IF ((TS2A(2,LF,IA1).NE.FBLANK) .AND.
     *         (TS2B(2,LF,IA1).NE.FBLANK)) THEN
               TL1 = (1.0-F1) * TS2A(2,LF,IA1) + F1 * TS2B(2,LF,IA1)
            ELSE IF ((F1.LE.0.333) .AND. (TS2A(2,LF,IA1).NE.FBLANK))
     *         THEN
               TL1 = TS2A(2,LF,IA1)
            ELSE IF ((F1.GE.0.667) .AND. (TS2B(2,LF,IA1).NE.FBLANK))
     *         THEN
               TL1 = TS2B(2,LF,IA1)
            ELSE
               TL1 = FBLANK
               END IF
            IF ((TR1.NE.FBLANK) .AND. (VFAC(1,LF).NE.FBLANK)) THEN
               VFAC(1,LF) = VFAC(1,LF) * SQRT (TR1)
            ELSE
               VFAC(1,LF) = FBLANK
               END IF
            IF ((TL1.NE.FBLANK) .AND. (VFAC(2,LF).NE.FBLANK)) THEN
               VFAC(2,LF) = VFAC(2,LF) * SQRT (TL1)
            ELSE
               VFAC(2,LF) = FBLANK
               END IF
 40         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TY T1=',F11.6,' AN',I3,' NOT IN',2F14.6)
 1020 FORMAT ('TY T2=',F11.6,' AN',I3,' NOT IN',2F14.6)
      END
      SUBROUTINE GETSYC (T, IA1, VFAC)
C-----------------------------------------------------------------------
C   GETSYF interpolates as needed in the tables of Nominal sensitivities
C   and returns the factors to be applied
C   Inputs:
C      T      R        time (days)
C      IA1    I        antenna 1
C   Outputs:
C      VFAC   R(2,*)   Visibility multiplier (pol, IF)
C-----------------------------------------------------------------------
      REAL      T, VFAC(2,*)
      INTEGER   IA1
C
      INTEGER   I, LF
      REAL      F1, TR1, TL1, DR1, DL1, VS, GR1, GL1, TEPS, TENSEC, TINY
      INCLUDE 'TYAPL.INC'
      INCLUDE 'TYTABS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      I = 2 * MAXIF
      CALL RFILL (I, 1.0, VFAC)
      TENSEC = 10.1 / (24.0 * 3600.0)
      TINY = TENSEC / 300.0
C                                       scaling factors: ???????????
      VS = REWAY(1) * 5.622 / 0.932 / 1.176
C                                       remove a nom sensitivity
      IF (INVER1.GT.0) THEN
         TEPS = MIN (6.*TENSEC, MAX (TENSEC, T1B(IA1)-T1B(IA1)))
         IF ((T.LT.T1A(IA1)-TINY) .OR. (T.GT.T1B(IA1)+TINY)) THEN
            EXTRAP = EXTRAP + 1
            IF ((T.LT.T1A(IA1)-TEPS) .OR. (T.GT.T1B(IA1)+TEPS)) THEN
               WRITE (MSGTXT,1000) T, IA1, T1A(IA1), T1B(IA1)
               IF (NMSG.LT.500) CALL MSGWRT (7)
               NMSG = NMSG + 1
               END IF
            END IF
         F1 = 0.0
         IF (T1A(IA1).NE.T1B(IA1)) F1 = (T - T1A(IA1)) /
     *      (T1B(IA1) - T1A(IA1))
         DO 20 LF = 1,NIF
            IF (SKIP1(1,LF,IA1).GE.1) THEN
               IF ((AVTS1A(1,LF).NE.FBLANK) .AND.
     *            (AVTS1B(1,LF).NE.FBLANK)) THEN
                  TR1 = (1.0-F1) * AVTS1A(1,LF) + F1 * AVTS1B(1,LF)
                  DR1 = (1.0-F1) * AVPD1A(1,LF) + F1 * AVPD1B(1,LF)
                  GR1 = (1.0-F1) * AVPG1A(1,LF) + F1 * AVPG1B(1,LF)
               ELSE IF (AVTS1A(1,LF).NE.FBLANK) THEN
                  TR1 = AVTS1A(1,LF)
                  DR1 = AVPD1A(1,LF)
                  GR1 = AVPG1A(1,LF)
               ELSE IF (AVTS1B(1,LF).NE.FBLANK) THEN
                  TR1 = AVTS1B(1,LF)
                  DR1 = AVPD1B(1,LF)
                  GR1 = AVPG1B(1,LF)
               ELSE
                  TR1 = 1.0
                  DR1 = 1.0
                  GR1 = 1.0/64.0
                  END IF
            ELSE IF ((TS1A(1,LF,IA1).NE.FBLANK) .AND.
     *         (TS1B(1,LF,IA1).NE.FBLANK)) THEN
               TR1 = (1.0-F1) * TS1A(1,LF,IA1) + F1 * TS1B(1,LF,IA1)
               DR1 = (1.0-F1) * PD1A(1,LF,IA1) + F1 * PD1B(1,LF,IA1)
               GR1 = (1.0-F1) * PG1A(1,LF,IA1) + F1 * PG1B(1,LF,IA1)
            ELSE IF ((F1.LE.0.333) .AND. (TS1A(1,LF,IA1).NE.FBLANK))
     *         THEN
               TR1 = TS1A(1,LF,IA1)
               DR1 = PD1A(1,LF,IA1)
               GR1 = PG1A(1,LF,IA1)
            ELSE IF ((F1.GE.0.667) .AND. (TS1B(1,LF,IA1).NE.FBLANK))
     *         THEN
               TR1 = TS1B(1,LF,IA1)
               DR1 = PD1B(1,LF,IA1)
               GR1 = PG1B(1,LF,IA1)
            ELSE
               TR1 = FBLANK
               DR1 = FBLANK
               GR1 = FBLANK
               END IF
            IF (SKIP1(2,LF,IA1).GE.1) THEN
               IF ((AVTS1A(2,LF).NE.FBLANK) .AND.
     *            (AVTS1B(2,LF).NE.FBLANK)) THEN
                  TL1 = (1.0-F1) * AVTS1A(2,LF) + F1 * AVTS1B(2,LF)
                  DL1 = (1.0-F1) * AVPD1A(2,LF) + F1 * AVPD1B(2,LF)
                  GL1 = (1.0-F1) * AVPG1A(2,LF) + F1 * AVPG1B(2,LF)
               ELSE IF (AVTS1A(2,LF).NE.FBLANK) THEN
                  TL1 = AVTS1A(2,LF)
                  DL1 = AVPD1A(2,LF)
                  GL1 = AVPG1A(2,LF)
               ELSE IF (AVTS1B(2,LF).NE.FBLANK) THEN
                  TL1 = AVTS1B(2,LF)
                  DL1 = AVPD1B(2,LF)
                  GL1 = AVPG1B(2,LF)
               ELSE
                  TL1 = 1.0
                  DL1 = 1.0
                  GL1 = 1.0/64.0
                  END IF
            ELSE IF ((TS1A(2,LF,IA1).NE.FBLANK) .AND.
     *         (TS1B(2,LF,IA1).NE.FBLANK)) THEN
               TL1 = (1.0-F1) * TS1A(2,LF,IA1) + F1 * TS1B(2,LF,IA1)
               DL1 = (1.0-F1) * PD1A(2,LF,IA1) + F1 * PD1B(2,LF,IA1)
               GL1 = (1.0-F1) * PG1A(2,LF,IA1) + F1 * PG1B(2,LF,IA1)
            ELSE IF ((F1.LE.0.333) .AND. (TS1A(2,LF,IA1).NE.FBLANK))
     *         THEN
               TL1 = TS1A(2,LF,IA1)
               DL1 = PD1A(2,LF,IA1)
               GL1 = PG1A(2,LF,IA1)
            ELSE IF ((F1.GE.0.667) .AND. (TS1B(2,LF,IA1).NE.FBLANK))
     *         THEN
               TL1 = TS1B(2,LF,IA1)
               DL1 = PD1B(2,LF,IA1)
               GL1 = PG1B(2,LF,IA1)
            ELSE
               TL1 = FBLANK
               DL1 = FBLANK
               GL1 = FBLANK
               END IF
            IF (TR1.EQ.FBLANK) THEN
               VFAC(1,LF) = FBLANK
            ELSE IF (OPTYPE.EQ.'CLP') THEN
               VFAC(1,LF) = GR1 * 64.
               IF (JD.LT.JD0) VFAC(1,LF) = 256. / GR1
            ELSE
               VFAC(1,LF) = SQRT (DR1 / VS * VEFF(1,LF,IA1))
               IF (JD.LT.JD0) VFAC(1,LF) = VFAC(1,LF) * 256. / GR1
               END IF
            IF (TL1.EQ.FBLANK) THEN
               VFAC(2,LF) = FBLANK
            ELSE IF (OPTYPE.EQ.'CLP') THEN
               VFAC(2,LF) = GL1 * 64.
               IF (JD.LT.JD0) VFAC(2,LF) = 256. / GL1
            ELSE
               VFAC(2,LF) = SQRT (DL1 / VS * VEFF(2,LF,IA1))
               IF (JD.LT.JD0) VFAC(2,LF) = VFAC(2,LF) * 256. / GL1
               END IF
 20         CONTINUE
         END IF
C                                       apply second
       IF (INVER2.GT.0) THEN
         TEPS = MIN (6.*TENSEC, MAX (TENSEC, T2B(IA1)-T2B(IA1)))
         IF ((T.LT.T2A(IA1)-TINY) .OR. (T.GT.T2B(IA1)+TINY)) THEN
            EXTRAP = EXTRAP + 1
            IF ((T.LT.T2A(IA1)-TEPS) .OR. (T.GT.T2B(IA1)+TEPS)) THEN
               WRITE (MSGTXT,1020) T, IA1, T2A(IA1), T2B(IA1)
               IF (NMSG.LT.500) CALL MSGWRT (7)
               NMSG = NMSG + 1
               END IF
            END IF
         F1 = 0.0
         IF (T2A(IA1).NE.T2B(IA1)) F1 = (T - T2A(IA1)) /
     *      (T2B(IA1) - T2A(IA1))
         DO 40 LF = 1,NIF
            IF (SKIP2(1,LF,IA1).GE.1) THEN
               IF ((AVTS2A(1,LF).NE.FBLANK) .AND.
     *            (AVTS2B(1,LF).NE.FBLANK)) THEN
                  TR1 = (1.0-F1) * AVTS2A(1,LF) + F1 * AVTS2B(1,LF)
                  DR1 = (1.0-F1) * AVPD2A(1,LF) + F1 * AVPD2B(1,LF)
                  GR1 = (1.0-F1) * AVPG2A(1,LF) + F1 * AVPG2B(1,LF)
               ELSE IF (AVTS2A(1,LF).NE.FBLANK) THEN
                  TR1 = AVTS2A(1,LF)
                  DR1 = AVPD2A(1,LF)
                  GR1 = AVPG2A(1,LF)
               ELSE IF (AVTS2B(1,LF).NE.FBLANK) THEN
                  TR1 = AVTS2B(1,LF)
                  DR1 = AVPD2B(1,LF)
                  GR1 = AVPG2B(1,LF)
               ELSE
                  TR1 = 1.0
                  DR1 = 1.0
                  GR1 = 1.0/64.0
                  END IF
            ELSE IF ((TS2A(1,LF,IA1).NE.FBLANK) .AND.
     *         (TS2B(1,LF,IA1).NE.FBLANK)) THEN
               TR1 = (1.0-F1) * TS2A(1,LF,IA1) + F1 * TS2B(1,LF,IA1)
               DR1 = (1.0-F1) * PD2A(1,LF,IA1) + F1 * PD2B(1,LF,IA1)
               GR1 = (1.0-F1) * PG2A(1,LF,IA1) + F1 * PG2B(1,LF,IA1)
            ELSE IF ((F1.LE.0.333) .AND. (TS2A(1,LF,IA1).NE.FBLANK))
     *         THEN
               TR1 = TS2A(1,LF,IA1)
               DR1 = PD2A(1,LF,IA1)
               GR1 = PG2A(1,LF,IA1)
            ELSE IF ((F1.GE.0.667) .AND. (TS2B(1,LF,IA1).NE.FBLANK))
     *         THEN
               TR1 = TS2B(1,LF,IA1)
               DR1 = PD2B(1,LF,IA1)
               GR1 = PG2B(1,LF,IA1)
            ELSE
               TR1 = FBLANK
               DR1 = FBLANK
               GR1 = FBLANK
               END IF
            IF (SKIP2(2,LF,IA1).GE.1) THEN
               IF ((AVTS2A(2,LF).NE.FBLANK) .AND.
     *            (AVTS2B(2,LF).NE.FBLANK)) THEN
                  TL1 = (1.0-F1) * AVTS2A(2,LF) + F1 * AVTS2B(2,LF)
                  DL1 = (1.0-F1) * AVPD2A(2,LF) + F1 * AVPD2B(2,LF)
                  GL1 = (1.0-F1) * AVPG2A(2,LF) + F1 * AVPG2B(2,LF)
               ELSE IF (AVTS2A(2,LF).NE.FBLANK) THEN
                  TL1 = AVTS2A(2,LF)
                  DL1 = AVPD2A(2,LF)
                  GL1 = AVPG2A(2,LF)
               ELSE IF (AVTS2B(2,LF).NE.FBLANK) THEN
                  TL1 = AVTS2B(2,LF)
                  DL1 = AVPD2B(2,LF)
                  GL1 = AVPG2B(2,LF)
               ELSE
                  TL1 = 1.0
                  DL1 = 1.0
                  GL1 = 1.0/64.0
                  END IF
            ELSE IF ((TS2A(2,LF,IA1).NE.FBLANK) .AND.
     *         (TS2B(2,LF,IA1).NE.FBLANK)) THEN
               TL1 = (1.0-F1) * TS2A(2,LF,IA1) + F1 * TS2B(2,LF,IA1)
               DL1 = (1.0-F1) * PD2A(2,LF,IA1) + F1 * PD2B(2,LF,IA1)
               GL1 = (1.0-F1) * PG2A(2,LF,IA1) + F1 * PG2B(2,LF,IA1)
            ELSE IF ((F1.LE.0.333) .AND. (TS2A(2,LF,IA1).NE.FBLANK))
     *         THEN
               TL1 = TS2A(2,LF,IA1)
               DL1 = PD2A(2,LF,IA1)
               GL1 = PG2A(2,LF,IA1)
            ELSE IF ((F1.GE.0.667) .AND. (TS2B(2,LF,IA1).NE.FBLANK))
     *         THEN
               TL1 = TS2B(2,LF,IA1)
               DL1 = PD2B(2,LF,IA1)
               GL1 = PG2B(2,LF,IA1)
            ELSE
               TL1 = FBLANK
               DL1 = FBLANK
               GL1 = FBLANK
               END IF
            IF ((TR1.EQ.FBLANK) .OR.(VFAC(1,LF).EQ.FBLANK)) THEN
               VFAC(1,LF) = FBLANK
            ELSE IF (OPTYPE.EQ.'CLP') THEN
               IF (JD.LT.JD0) THEN
                  VFAC(1,LF) = VFAC(1,LF) * GR1 / 256.
               ELSE
                  VFAC(1,LF) = VFAC(1,LF) / (GR1 * 64.)
                  END IF
            ELSE
               VFAC(1,LF) = VFAC(1,LF) / SQRT (DR1 * VS /VEFF(1,LF,IA1))
               IF (JD.LT.JD0) VFAC(1,LF) = VFAC(1,LF) * GR1 / 256.
               END IF
            IF ((TL1.EQ.FBLANK) .OR. (VFAC(2,LF).EQ.FBLANK)) THEN
               VFAC(2,LF) = FBLANK
            ELSE IF (OPTYPE.EQ.'CLP') THEN
               IF (JD.LT.JD0) THEN
                  VFAC(2,LF) = VFAC(2,LF) * GL1 / 256.
               ELSE
                  VFAC(2,LF) = VFAC(2,LF) / (GL1 * 64.)
                  END IF
            ELSE
               VFAC(2,LF) = VFAC(2,LF) / SQRT (DL1 * VS /VEFF(2,LF,IA1))
               IF (JD.LT.JD0) VFAC(2,LF) = VFAC(2,LF) * GL1 / 256.
               END IF
 40         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SY T1=',F11.6,' AN',I3,' NOT IN',2F14.6)
 1020 FORMAT ('SY T2=',F11.6,' AN',I3,' NOT IN',2F14.6)
      END
      SUBROUTINE DOSKIP (IRET)
C-----------------------------------------------------------------------
C   DOSKIP looks through the 1 or 2 SY tables to see if there enough
C   valid entries for doing the correction (as function of antenna, IF,
C   and polarization) or whether that antenna should be passed through
C   with 1.0 for everything.
C   Outputs:
C      IRET   C   > 0 => error reading the SY tables
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'TYAPL.INC'
      INCLUDE 'TYTABS.INC'
      INTEGER   I, J, RNOMAX, SOURID, SUBA, FREQID, IA, IREC, CALTYP
      REAL      TIMEI, PDIF(2,MAXIF), PSUM(2,MAXIF), PGAIN(2,MAXIF)
      DOUBLE PRECISION TIME
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       removal SY table
      IF (INVER1.GT.0) THEN
         I = 2 * MAXANT * MAXIF
         CALL RFILL (I, 0.0, TS1A)
         CALL RFILL (I, 0.0, TS1B)
         RNOMAX = TYBUF1(5)
         DO 50 IREC = 1,RNOMAX
            TYRNO1 = IREC
            CALL TABSY ('READ', TYBUF1, TYRNO1, TYKOL1, TYNUM1, NPOL,
     *         NIF, TIME, TIMEI, CALTYP, SOURID, IA, SUBA, FREQID, PDIF,
     *         PSUM, PGAIN, IRET)
            IF (IRET.EQ.0) THEN
               IF (((SUBA.LE.0) .OR. (SUBARR.LE.0) .OR.
     *            (SUBA.EQ.SUBARR)) .AND. ((FREQID.LE.0) .OR.
     *            (FRQSEL.LE.0) .OR. (FREQID.EQ.FRQSEL))) THEN
C                                       convert units, check
                  DO 30 I = 1,NIF
                     DO 20 J = 1,2
                        IF (DOGAIN) THEN
                           IF ((PGAIN(J,I).EQ.FBLANK) .OR.
     *                        (PGAIN(J,I).LE.0.0)) THEN
                              TS1B(J,I,IA) = TS1B(J,I,IA) + 1.0
                           ELSE
                              TS1A(J,I,IA) = TS1A(J,I,IA) + 1.0
                              END IF
                        ELSE IF ((PDIF(J,I).EQ.FBLANK) .OR.
     *                     (PSUM(J,I).EQ.FBLANK) .OR.
     *                     (TCAL(J,I,IA).EQ.FBLANK) .OR.
     *                     (PGAIN(J,I).EQ.FBLANK) .OR.
     *                     (PGAIN(J,I).LE.0.0) .OR. (PDIF(J,I).LE.0.0)
     *                     .OR. PSUM(J,I).LE.PDIF(J,I)) THEN
                           TS1B(J,I,IA) = TS1B(J,I,IA) + 1.0
                        ELSE
                           TS1A(J,I,IA) = TS1A(J,I,IA) + 1.0
                           END IF
 20                     CONTINUE
 30                  CONTINUE
                  END IF
C                                       disk error
            ELSE
               WRITE (MSGTXT,1000) IRET, 1, 'READING SY TABLE DATA'
               GO TO 990
               END IF
 50         CONTINUE
         TYRNO1 = 1
         DO 80 IA = 1,MAXANT
            ASKIP1(IA) = 1
            HSKIP1(IA) = 0
            DO 70 I = 1,NIF
               DO 60 J = 1,2
                  TS1B(J,I,IA) = TS1B(J,I,IA) + TS1A(J,I,IA)
                  IF (TS1B(J,I,IA).GT.0.5) THEN
                     IF (TS1A(J,I,IA)/TS1B(J,I,IA).LT.CUTOFF) THEN
                        SKIP1(J,I,IA) = 1
                        HSKIP1(IA) = HSKIP1(IA) + 1
                     ELSE
                        ASKIP1(IA) = 0
                        END IF
                  ELSE
                     SKIP1(J,I,IA) = 2
                     END IF
 60               CONTINUE
 70            CONTINUE
 80         CONTINUE
         END IF
C                                       application SY table
      IF (INVER2.GT.0) THEN
         I = 2 * MAXANT * MAXIF
         CALL RFILL (I, 0.0, TS1A)
         CALL RFILL (I, 0.0, TS1B)
         RNOMAX = TYBUF2(5)
         DO 150 IREC = 1,RNOMAX
            TYRNO2 = IREC
            CALL TABSY ('READ', TYBUF2, TYRNO2, TYKOL2, TYNUM2, NPOL,
     *         NIF, TIME, TIMEI, CALTYP, SOURID, IA, SUBA, FREQID, PDIF,
     *         PSUM, PGAIN, IRET)
            IF (IRET.EQ.0) THEN
               IF (((SUBA.LE.0) .OR. (SUBARR.LE.0) .OR.
     *            (SUBA.EQ.SUBARR)) .AND. ((FREQID.LE.0) .OR.
     *            (FRQSEL.LE.0) .OR. (FREQID.EQ.FRQSEL))) THEN
C                                       convert units, check
                  DO 130 I = 1,NIF
                     DO 120 J = 1,2
                        IF (DOGAIN) THEN
                           IF ((PGAIN(J,I).EQ.FBLANK) .OR.
     *                        (PGAIN(J,I).LE.0.0)) THEN
                              TS1B(J,I,IA) = TS1B(J,I,IA) + 1.0
                           ELSE
                              TS1A(J,I,IA) = TS1A(J,I,IA) + 1.0
                              END IF
                        ELSE IF ((PDIF(J,I).EQ.FBLANK) .OR.
     *                     (PSUM(J,I).EQ.FBLANK) .OR.
     *                     (TCAL(J,I,IA).EQ.FBLANK) .OR.
     *                     (PGAIN(J,I).EQ.FBLANK) .OR.
     *                     (PGAIN(J,I).LE.0.0) .OR. (PDIF(J,I).LE.0.0)
     *                     .OR. PSUM(J,I).LE.PDIF(J,I)) THEN
                           TS1B(J,I,IA) = TS1B(J,I,IA) + 1.0
                        ELSE
                           TS1A(J,I,IA) = TS1A(J,I,IA) + 1.0
                           END IF
 120                    CONTINUE
 130                 CONTINUE
                  END IF
C                                       disk error
            ELSE
               WRITE (MSGTXT,1000) IRET, 2, 'READING SY TABLE DATA'
               GO TO 990
               END IF
 150        CONTINUE
         TYRNO2 = 1
         DO 180 IA = 1,MAXANT
            ASKIP2(IA) = 1
            HSKIP2(IA) = 0
            DO 170 I = 1,NIF
               DO 160 J = 1,2
                  TS1B(J,I,IA) = TS1B(J,I,IA) + TS1A(J,I,IA)
                  IF (TS1B(J,I,IA).GT.0.5) THEN
                     IF (TS1A(J,I,IA)/TS1B(J,I,IA).LT.CUTOFF) THEN
                        SKIP2(J,I,IA) = 1
                        HSKIP2(IA) = HSKIP2(IA) + 1
                     ELSE
                        ASKIP2(IA) = 0
                        END IF
                  ELSE
                     SKIP2(J,I,IA) = 2
                     END IF
 160              CONTINUE
 170           CONTINUE
 180        CONTINUE
         END IF
C
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DOSKIP ERROR',I5,' TABLE',I2,' ON ',A)
      END
      SUBROUTINE COPTAB (IRET)
C-----------------------------------------------------------------------
C   Updates tables for selection by IF
C   Inputs in common:
C      BIF   I  First IF
C      EIF   I  Highest IF selected
C      FQOFF D  Frequency offset
C      SELIF L  Select IFs or not
C   Output:
C      IRET  I  Return code, 0=>OK
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IERR, LUN1, LUN2, VER, NVER, OFQID, ISUB, JSUB, IV,
     *   TFLAG, BPOL, EPOL, BVER, IBUFF1(512), IBUFF2(512), NA, AN(50),
     *   IROUND
      LOGICAL   TABLE, EXIST, FITASC
      DOUBLE PRECISION T1, T2, FQOFF
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'TYAPL.INC'
      INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DFLG.INC'
      DATA LUN1, LUN2 /28,29/
      DATA NA, AN /51*0/
C-----------------------------------------------------------------------
      FQOFF = 0.0D0
      IF (SUBARR.GT.0) THEN
         ISUB = SUBARR
         JSUB = 1
      ELSE
         ISUB = 0
         JSUB = -1
         END IF
      TFLAG = IROUND (XDOFLG)
C                                       STOKES selection not allowed
C                                       these ok even for 1 pol data
      BPOL = 1
      EPOL = 2
C                                       allow 15 min extra to be sure
C                                       that one gets all needed rows
      T1 = -100.0
      T2 = 10000.
C                                       reopen flag table
      IF ((DOFLAG) .AND. (FGVER.GE.1)) THEN
         IFLUN = 30
         CALL FLGINI ('READ', FGBUFF, IUDISK, IUCNO, FGVER, CATUV,
     *      IFLUN, IFGRNO, FGKOLS, FGNUMV, IRET)
         IF (IRET.GT.0) THEN
            MSGTXT = 'COULD NOT OPEN FLAG TABLE IN COPTAB'
            CALL MSGWRT (8)
            DOFLAG = .FALSE.
            END IF
         END IF
C                                       Revise tables: note depends on
C                                       doing loops 0 times if none
C                                       AN tables
C                                       Reference frequency in AN table
      CALL FNDEXT ('AN', CATOLD, NVER)
      IF (JSUB.EQ.1) NVER = 1
      DO 20 VER = 1,NVER
         IV = VER
         IF (SUBARR.GT.0) IV = SUBARR
         CALL ISTAB ('AN', DISKIN, FCNO(2), IV, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL ANSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), IV, VER, CATOLD, CATBLK, LUN1, LUN2, BIF,
     *      EIF, FQOFF, DOPOL, IBUFF1, IBUFF2, IRET)
 20      CONTINUE
      OFQID = FRQSEL
C                                       BP tables
      CALL FNDEXT ('BP', CATOLD, NVER)
      DO 120 VER = 1,NVER
         CALL ISTAB ('BP', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL BPSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, BCHAN, ECHAN, T1, T2, OFQID, ISUB, JSUB, IBUFF1,
     *      IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 120     CONTINUE
C                                       CL tables
      CALL FNDEXT ('CL', CATOLD, NVER)
      DO 140 VER = 1,NVER
         CALL ISTAB ('CL', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
C                                       do NOT select on sources
         IF (EXIST .AND. (IERR.EQ.0)) CALL CLSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, T1, T2, 0, SOUWAN, AN, NA, ISUB, JSUB,
     *      IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 140     CONTINUE
C                                       CD tables
      CALL FNDEXT ('CD', CATUV, NVER)
      DO 145 VER = 1,NVER
         CALL ISTAB ('CD', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
C                                       do not select on antenna
         IF (EXIST.AND.(IERR.EQ.0)) CALL CDSEL (DISKIN, FCNO(2), DISKO,
     *      FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL, BIF,
     *      EIF, OFQID, AN, 0, ISUB, JSUB, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 145     CONTINUE
C                                       CP tables
      CALL FNDEXT ('CP', CATOLD, NVER)
      DO 150 VER = 1,NVER
         CALL ISTAB ('CP', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST.AND.(IERR.EQ.0)) CALL CPSEL (DISKIN, FCNO(2), DISKO,
     *      FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BIF, EIF, BCHAN,
     *      ECHAN, OFQID, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 150     CONTINUE
C                                       CQ tables
      CALL FNDEXT ('CQ', CATOLD, NVER)
      DO 160 VER = 1, NVER
         CALL ISTAB ('CQ', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST.AND.(IERR.EQ.0)) CALL CQSEL (DISKIN, FCNO(2), DISKO,
     *      FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BIF, EIF, OFQID,
     *      ISUB, JSUB, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 160     CONTINUE
C                                       FG tables
      CALL FNDEXT ('FG', CATOLD, NVER)
C                                       copy FG tables if flaging was
C                                       not applied
      CALL FNDEXT ('FG', CATUV, NVER)
      IF ((FGVER.GT.0) .AND. (NVER.GT.0)) THEN
         BVER = FGVER + 1
         IF (NVER.LE.FGVER) THEN
            MSGTXT = 'WARNING: NO FG TABLES ARE COPIED SINCE HIGHEST' //
     *         ' WAS APPLIED'
            NVER = 0
         ELSE
            WRITE (MSGTXT,1160) BVER, NVER
            END IF
         CALL MSGWRT (6)
         END IF
      DO 180 VER = BVER,NVER
         CALL ISTAB ('FG', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0) .AND. (VER.GT.FGVER))
     *      CALL FGSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER, CATOLD,
     *      CATBLK, LUN1, LUN2, BIF, EIF, BCHAN, ECHAN,  T1, T2, OFQID,
     *      ISUB, JSUB, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 180     CONTINUE
C                                       GC tables
      CALL FNDEXT ('GC', CATOLD, NVER)
      DO 200 VER = 1,NVER
         CALL ISTAB ('GC', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL GCSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BCHAN, ECHAN, BIF, EIF, OFQID, AN, NA, ISUB, JSUB, IBUFF1,
     *      IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 200     CONTINUE
C                                       IM tables
      CALL FNDEXT ('IM', CATOLD, NVER)
      DO 220 VER = 1,NVER
         CALL ISTAB ('IM', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL IMSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, T1, T2, NSOUWD, SOUWAN, AN, NA, ISUB, JSUB,
     *      IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 220     CONTINUE
C                                       MC tables
      CALL FNDEXT ('MC', CATOLD, NVER)
      DO 240 VER = 1,NVER
         CALL ISTAB ('MC', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL MCSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, T1, T2, NSOUWD, SOUWAN, AN, NA, ISUB, JSUB,
     *      IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 240     CONTINUE
C                                       PC tables
      CALL FNDEXT ('PC', CATOLD, NVER)
      DO 260 VER = 1,NVER
         CALL ISTAB ('PC', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL PCSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, T1, T2, NSOUWD, SOUWAN, AN, NA, ISUB, JSUB,
     *      IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 260     CONTINUE
C                                       PD tables
      CALL FNDEXT ('PD', CATOLD, NVER)
      DO 270 VER = 1,NVER
         CALL ISTAB ('PD', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL PDSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, BCHAN, ECHAN, OFQID, ISUB, JSUB, IBUFF1, IBUFF2,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
 270     CONTINUE
C                                       SN tables
      CALL FNDEXT ('SN', CATOLD, NVER)
      DO 280 VER = 1,NVER
         CALL ISTAB ('SN', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) THEN
            IF ((DOFLAG) .AND. (MOD(TFLAG/2,2).NE.1)) THEN
               CALL SNFSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER,
     *            CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL, BIF, EIF,
     *            OFQID, T1, T2, ISUB, JSUB, IBUFF1, IBUFF2, IRET)
            ELSE
               CALL SNSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER,
     *            CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL, BIF, EIF,
     *            OFQID, T1, T2, ISUB, JSUB, IBUFF1, IBUFF2, IRET)
               END IF
            IF (IRET.GT.0) GO TO 999
            END IF
 280     CONTINUE
C                                       SY tables
      CALL FNDEXT ('SY', CATUV, NVER)
      DO 290 VER = 1,NVER
         CALL ISTAB ('SY', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) THEN
            IF ((DOFLAG) .AND. (MOD(TFLAG,2).NE.1)) THEN
               CALL SYFSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER, CATUV,
     *            CATBLK, LUN1, LUN2, BPOL, EPOL, BIF, EIF, OFQID, T1,
     *            T2, NSOUWD, SOUWAN, AN, NA, ISUB, JSUB, IBUFF1,
     *            IBUFF2, IRET)
            ELSE
               CALL SYSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER, CATUV,
     *            CATBLK, LUN1, LUN2, BPOL, EPOL, BIF, EIF, OFQID, T1,
     *            T2, NSOUWD, SOUWAN, AN, NA, ISUB, JSUB, IBUFF1,
     *            IBUFF2, IRET)
               END IF
            IF (IRET.GT.0) GO TO 999
            END IF
 290     CONTINUE
C                                       SU tables: all sources
      CALL FNDEXT ('SU', CATOLD, NVER)
      DO 300 VER = 1,NVER
         CALL ISTAB ('SU', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL SUSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BIF, EIF,
     *      OFQID, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 300     CONTINUE
C                                       TY tables
      CALL FNDEXT ('TY', CATOLD, NVER)
      DO 320 VER = 1,NVER
         CALL ISTAB ('TY', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) THEN
            IF ((DOFLAG) .AND. (MOD(TFLAG,2).NE.1)) THEN
               CALL TYFSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER,
     *            CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL, BIF, EIF,
     *            OFQID, T1, T2, AN, NA, ISUB, JSUB, IBUFF1, IBUFF2,
     *            IRET)
            ELSE
               CALL TYSEL (DISKIN, FCNO(2), DISKO, FCNO(1), VER,
     *            CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL, BIF, EIF,
     *            OFQID, T1, T2, AN, NA, ISUB, JSUB, IBUFF1, IBUFF2,
     *            IRET)
               END IF
            IF (IRET.GT.0) GO TO 999
            END IF
 320     CONTINUE
C                                       WX tables
      CALL FNDEXT ('WX', CATOLD, NVER)
      DO 340 VER = 1,NVER
         CALL ISTAB ('WX', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL WXSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, T1, T2, AN,
     *      NA, ISUB, JSUB, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 340     CONTINUE
C                                       BL tables
      CALL FNDEXT ('BL', CATOLD, NVER)
      DO 350 VER = 1,NVER
         CALL ISTAB ('BL', DISKIN, FCNO(2), VER, LUN1, IBUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL BLSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BPOL, EPOL,
     *      AN, NA, ISUB, JSUB, BIF, EIF, OFQID, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 350     CONTINUE
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, NEWCNO, DIFPIX, IBUFF1, IBUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'TYAPHI: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1160 FORMAT ('WARNING: ONLY FG TABLE VERSIONS',I4,' TO',I4,' COPIED')
      END
      SUBROUTINE TYFSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, TB, TE, AN, NA, ISUB,
     *   JSUB, BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C   Copies a subset of IFs in a TY table, can also modify the FQ ID
C   Inputs:
C      DISKI    I        Input volume number
C      CNOI     I        Input catalog number
C      DISKO    I        Output volume number
C      CNOO     I        Output catalog number
C      VER      I        Version to check/modify
C      CATIN    I(256)   Input catalog header
C      LUNI     I    p   LUN to use
C      LUNO     I        LUN to use
C      BIF      I        Start IF number
C      EIF      I        End IF number
C      IFQID    I        FQ ID to select (output value is 1)
C                        if <= 0 then output value unchanged.
C      TB       D        Beginning time in days
C      TE       D        Ending time in days
C      AN       I(*)     Array of selected antennas
C      NA       I        Number of selected antennas
C      ISUB     I        Selected subarray (= 0 any)
C      JSUB     I        Output subarray number (< 0 => NO CHANGE)
C   Input/Output:
C      CATOUT   I(256)   Output catalog header
C      BUFFER   I(*)     Work buffer
C      OBUFF    I(*)     Work buffer
C   Output:
C      IRET     I        Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, AN(8), NA, ISUB, JSUB,
     *   BUFFER(*), OBUFF(*), IRET
      DOUBLE PRECISION TB, TE
C
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ITYRNO, TYKOLS(MAXTYC), TYNUMV(MAXTYC), NUMPOL, NUMIF,
     *   OKOLS(MAXTYC), ONUMV(MAXTYC), NTYROW, I, OVER, SOURID, ANTNO,
     *   SUBA, FREQID, OTYRNO, NEWNIF, IIF, JIF, IPOL, K, NDEL, NTOT,
     *   JRET, LBPOL, NEWPOL
      LOGICAL   REFMT, GOTIT, GOTONE
      REAL      TIME, TIMEI, TSYS(2,MAXIF), TANT(2,MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:DFLG.INC'
C-----------------------------------------------------------------------
C                                       open flag table
      TMFLST = -1.E20
      NUMFLG = 0
      IFGRNO = 1
      NDEL = 0
      NTOT = 0
C                                       Open TY file
      CALL TYINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI, ITYRNO,
     *   TYKOLS, TYNUMV, NUMPOL, NUMIF, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       New no. of IFs
      NEWNIF = MAX (MIN (NUMIF, EIF) - BIF + 1, 0)
      LBPOL = MAX (1, BPOL)
      NEWPOL = MIN (2, MIN (NUMPOL, EPOL)) - LBPOL + 1
      IF (NEWPOL.LE.0) THEN
         NEWPOL = NUMPOL
         LBPOL = 1
         END IF
      REFMT = (NEWNIF.NE.NUMIF) .OR. (NEWPOL.NE.NUMPOL)
C                                       # rows in old table
      NTYROW = BUFFER(5)
C                                       Open up new TY table
      OVER = VER
      CALL TYINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OTYRNO, OKOLS, ONUMV, NEWPOL, NEWNIF, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1,NTYROW
         CALL TABTY ('READ', BUFFER, ITYRNO, TYKOLS, TYNUMV, NUMPOL,
     *      NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, TSYS, TANT,
     *      IRET)
C                                       Error reading table
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
C                                       check subarray, time
         ELSE IF (IRET.EQ.0) THEN
            CALL TYFLG (NUMPOL, NUMIF, TIME, SOURID, ANTNO, SUBA,
     *         FREQID, TSYS, TANT, JRET)
            IF (JRET.GT.0) THEN
               IRET = JRET
               GO TO 999
               END IF
            IF ((SUBA.GT.0) .AND. (ISUB.GT.0) .AND. (ISUB.NE.SUBA))
     *         IRET = -1
            IF ((TIME.LT.TB) .OR. (TIME.GT.TE)) IRET = -1
            IF ((IFQID.GT.0) .AND. (FREQID.GT.0) .AND.
     *         (IFQID.NE.FREQID)) IRET = -1
C                                       antenna
            IF ((NA.GT.0) .AND. (ANTNO.GT.0)) THEN
               GOTIT = .FALSE.
               DO 30 K = 1,NA
                  GOTIT = GOTIT .OR. (ANTNO.EQ.AN(K))
 30               CONTINUE
               IF (.NOT.GOTIT) IRET = -1
               END IF
            IF ((JRET.EQ.-1) .AND. (IRET.EQ.0)) THEN
               IRET = -1
               NDEL = NDEL + 1
               END IF
            END IF
C                                       Is this record selected ?
         IF ((IRET.LT.0) .OR. (NEWNIF.EQ.0)) THEN
            REFMT = .TRUE.
C                                       Select IFs
         ELSE
            GOTONE = .FALSE.
            DO 90 JIF = 1,NEWNIF
               IIF = JIF + BIF - 1
               DO 80 IPOL = 1,NEWPOL
                  K = IPOL + LBPOL - 1
                  TSYS(IPOL,JIF) = TSYS(K,IIF)
                  TANT(IPOL,JIF) = TANT(K,IIF)
                  IF (TSYS(IPOL,JIF).NE.FBLANK) GOTONE = .TRUE.
                  IF (TANT(IPOL,JIF).NE.FBLANK) GOTONE = .TRUE.
 80               CONTINUE
 90            CONTINUE
C                                       Write new one
            IF (GOTONE) THEN
               IF (IFQID.GT.0) FREQID = 1
               IF (JSUB.GE.0) SUBA = JSUB
               NTOT = NTOT + 1
               CALL TABTY ('WRIT', OBUFF, OTYRNO, OKOLS, ONUMV, NUMPOL,
     *            NEWNIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *            TSYS, TANT, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1020) IRET
                  GO TO 990
                  END IF
            ELSE
               REFMT = .TRUE.
               END IF
            END IF
 100     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ITYRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OTYRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted TY', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied TY', DISKI, CNOI, VER, DISKO,
     *         CNOO, OVER
            END IF
         CALL MSGWRT (3)
         NTOT = NTOT + NDEL
         WRITE (MSGTXT,1101) NDEL, NTOT
         CALL REFRMT (MSGTXT, '_', I)
         CALL MSGWRT (3)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('TYFSEL: ERROR ',I3,' RETURNED FROM TYINI')
 1020 FORMAT ('TYFSEL: ERROR ',I3,' RETURNED FROM TABTY')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
 1101 FORMAT ('__Fully deleted',I10,' of',I12,' TY records applying',
     *   ' flag table')
      END
      SUBROUTINE TYFLG (NPOL, NIF, TIME, SOURID, ANTNO, SUBA,
     *   FREQID, TSYS, TANT, IRET)
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
C      TSYS     R(*)   System temperature array - flagged -> FBLANK
C      TANT     R(*)   Antenna temperature array
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
      INTEGER   NPOL, NIF, SOURID, ANTNO, SUBA, FREQID, IRET
      REAL      TIME, TSYS(2,*), TANT(2,*)
C
      INTEGER   IFLAG, FLGA, JPOLN, JIF, LIMF1, LIMF2, IPOLPT
      INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (TMFLST.LT.TIME) CALL NXTFLG (TIME, .TRUE., IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NUMFLG.LE.0) GO TO 999
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
C                                       Loop over IF
               DO 30 JIF = LIMF1,LIMF2
                  TSYS(JPOLN,JIF) = FBLANK
                  TANT(JPOLN,JIF) = FBLANK
 30               CONTINUE
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       Check if data all
      DO 70 JPOLN = 1,NPOL
         DO 60 JIF = 1,NIF
            IF ((TSYS(JPOLN,JIF).NE.FBLANK) .OR.
     *         (TANT(JPOLN,JIF).NE.FBLANK)) GO TO 999
 60         CONTINUE
 70      CONTINUE
      IRET = -1
C
 999  RETURN
      END
      SUBROUTINE SNFSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, TB, TE, ISUB, JSUB,
     *   BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C   Copies a subset of IFs in a SN table, can also modify the FQ ID
C   Applies flagging as well
C   Inputs:
C      DISKI           I       Input volume number
C      CNOI            I       Input catalog number
C      DISKO           I       Output volume number
C      CNOO            I       Output catalog number
C      VER             I       Version to check/modify
C      CATIN(256)      I       Input catalog header
C      CATOUT(256)     I       Output catalog header
C      LUNI            I       LUN to use
C      LUNO            I       LUN to use
C      BIF             I       Start IF number
C      EIF             I       End IF number
C      IFQID           I       FQ ID to select (output value is 1)
C                              if <= 0 then output value unchanged.
C      TB       D        Beginning time in days
C      TE       D        Ending time in days
C      ISUB     I        Selected subarray (= 0 any)
C      JSUB     I        Output subarray number (< 0 => NO CHANGE)
C   Input/Output:
C      BUFFER          I(*)    Work buffer
C      OBUFF           I(*)    Work buffer
C   Output:
C      IRET            I       Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, ISUB, JSUB, BUFFER(*),
     *   OBUFF(*), IRET
      DOUBLE PRECISION TB, TE
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ISNRNO, SNKOLS(MAXSNC), SNNUMV(MAXSNC), NUMANT, NUMPOL,
     *   NUMIF, NUMNOD, OKOLS(MAXSNC), ONUMV(MAXSNC), NSNROW, I, OVER,
     *   SOURID, ANTNO, SUBA, FREQID, NODENO, REFA(2,MAXIF), OSNRNO,
     *   NEWNIF, IIF, JIF, IPOL, NDEL, NTOT, JRET, LBPOL, NEWPOL, K,
     *   LBIF
      LOGICAL   ISAPPL, REFMT, GOTONE
      REAL      GMMOD, RANOD(25), DECNOD(25), TIMEI, IFR, MBDELY(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *   WEIGHT(2,MAXIF), DISP(2), DDISP(2)
      DOUBLE PRECISION TIME
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:DFLG.INC'
C-----------------------------------------------------------------------
C                                       open flag table
      TMFLST = -1.E20
      NUMFLG = 0
      IFGRNO = 1
      NDEL = 0
      NTOT = 0
C                                       Open SN file
      CALL SNINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       New no. of IFs
      LBIF = MAX (1, BIF)
      NEWNIF = MIN (NUMIF, EIF) - BIF + 1
      IF (NEWNIF.LE.0) THEN
         NEWNIF = NUMIF
         LBIF = 1
         END IF
      LBPOL = MAX (1, BPOL)
      NEWPOL = MIN (2, MIN (NUMPOL, EPOL)) - LBPOL + 1
      IF (NEWPOL.LE.0) THEN
         NEWPOL = NUMPOL
         LBPOL = 1
         END IF
      REFMT = (NEWNIF.NE.NUMIF) .OR. (NEWPOL.NE.NUMPOL)
C                                       # rows in old table
      NSNROW = BUFFER(5)
C                                       Open up new SN table
      OVER = VER
      CALL SNINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OSNRNO, OKOLS, ONUMV, NUMANT, NEWPOL, NEWNIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1,NSNROW
         CALL TABSN ('READ', BUFFER, ISNRNO, SNKOLS, SNNUMV, NUMPOL,
     *      TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *      MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *      REFA, IRET)
C                                       check subarray and FQ
         IF (IRET.EQ.0) THEN
            CALL SNFLG (NUMPOL, NUMIF, TIME, SOURID, ANTNO, SUBA,
     *         FREQID, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, JRET)
            IF (JRET.GT.0) THEN
               IRET = JRET
               GO TO 999
               END IF
            IF ((SUBA.GT.0) .AND. (ISUB.GT.0) .AND. (ISUB.NE.SUBA))
     *         IRET = -1
            IF ((IFQID.GT.0) .AND. (FREQID.GT.0) .AND.
     *         (IFQID.NE.FREQID)) IRET = -1
            IF ((TIME.LT.TB) .OR. (TIME.GT.TE)) IRET = -1
            IF ((JRET.EQ.-1) .AND. (IRET.EQ.0)) THEN
               IRET = -1
               NDEL = NDEL + 1
               END IF
            END IF
C                                       Is this record selected ?
         IF ((IRET.LT.0) .OR. (NEWNIF.EQ.0)) THEN
            REFMT = .TRUE.
C                                       Error reading table
         ELSE IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
C                                       Select IFs
         ELSE
            GOTONE = .FALSE.
            DO 90 JIF = 1,NEWNIF
               IIF = JIF + LBIF - 1
               DO 80 IPOL = 1,NEWPOL
                  K = IPOL + LBPOL - 1
                  CREAL(IPOL,JIF) = CREAL(K,IIF)
                  CIMAG(IPOL,JIF) = CIMAG(K,IIF)
                  DELAY(IPOL,JIF) = DELAY(K,IIF)
                  RATE(IPOL,JIF) = RATE(K,IIF)
                  WEIGHT(IPOL,JIF) = WEIGHT(K,IIF)
                  REFA(IPOL,JIF) = REFA(K,IIF)
                  IF (CREAL(IPOL,JIF).NE.FBLANK) GOTONE = .TRUE.
 80               CONTINUE
 90            CONTINUE
C                                       Write new one
            IF (GOTONE) THEN
               IF (IFQID.GT.0) FREQID = 1
               IF (JSUB.GE.0) SUBA = JSUB
               NTOT = NTOT + 1
               CALL TABSN ('WRIT', OBUFF, OSNRNO, OKOLS, ONUMV, NEWPOL,
     *            TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *            MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *            WEIGHT, REFA, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1020) IRET
                  GO TO 990
                  END IF
            ELSE
               REFMT = .TRUE.
               END IF
            END IF
 100     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OSNRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted SN', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied SN', DISKI, CNOI, VER, DISKO,
     *         CNOO, OVER
            END IF
         CALL MSGWRT (3)
         NTOT = NTOT + NDEL
         WRITE (MSGTXT,1101) NDEL, NTOT
         CALL REFRMT (MSGTXT, '_', I)
         CALL MSGWRT (3)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SNSEL: ERROR ',I3,' RETURNED FROM SNINI')
 1020 FORMAT ('SNSEL: ERROR ',I3,' RETURNED FROM TABSN')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
 1101 FORMAT ('__Fully deleted',I10,' of',I12,' SN records applying',
     *   ' flag table')
      END
      SUBROUTINE SNFLG (NPOL, NIF, TIME, SOURID, ANTNO, SUBA,
     *   FREQID, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
C-----------------------------------------------------------------------
C   Flags a SN table row based on the flags in FG table loaded to Common
C   Inputs:
C      NPOL     I        Number polarizations in TY data
C      NIF      I        Number of IFs in those data
C      TIME     D        Time of table row
C      SOURID   I        Source number of row
C      ANTNO    I        Antenna number of row
C      SUBA     I        Subarray of row
C      FREQID   I        Frequency ID if row
C   In/Out:
C      CREAL    R(2,*)   Real part of solution
C      CIMAG    R(2,*)   Imaginary part of solution
C      DELAY    R(2,*)   Delay
C      RATE     R(2,*)   Rate
C      WEIGHT   R(2,*)   Solution weight
C      REFA     I(2,*)   Reference antenna
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
      DOUBLE PRECISION TIME
      INTEGER   NPOL, NIF, SOURID, ANTNO, SUBA, FREQID, REFA(2,*), IRET
      REAL      CREAL(2,*), CIMAG(2,*), DELAY(2,*), RATE(2,*),
     *   WEIGHT(2,*)
C
      INTEGER   IFLAG, FLGA, JPOLN, JIF, LIMF1, LIMF2, IPOLPT
      REAL      RTIME
      INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      RTIME = TIME
      IF (TMFLST.LT.TIME) CALL NXTFLG (RTIME, .TRUE., IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NUMFLG.LE.0) GO TO 999
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
C                                       Loop over IF
               DO 30 JIF = LIMF1,LIMF2
                  CREAL(JPOLN,JIF) = FBLANK
                  CIMAG(JPOLN,JIF) = FBLANK
                  DELAY(JPOLN,JIF) = FBLANK
                  RATE(JPOLN,JIF) = FBLANK
                  WEIGHT(JPOLN,JIF) = 0.0
                  REFA(JPOLN,JIF) = 0
 30               CONTINUE
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       Check if data all gone
      DO 70 JPOLN = 1,NPOL
         DO 60 JIF = 1,NIF
            IF ((CREAL(JPOLN,JIF).NE.FBLANK) .AND.
     *         (CIMAG(JPOLN,JIF).NE.FBLANK)) GO TO 999
 60         CONTINUE
 70      CONTINUE
      IRET = -1
C
 999  RETURN
      END
      SUBROUTINE SYFSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, TB, TE, NSOU, SOUIND,
     *   AN, NA, ISUB, JSUB, BUFFER, OBUFF, IRET)
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
C      LUNI     I        LUN to use
C      LUNO     I        LUN to use
C      BPOL     I        First polarization to copy
C      EPOL     I        Last polarization to copy
C      BIF      I        Start IF number
C      EIF      I        End IF number
C      IFQID    I        FQ ID to select (set to 1 on output)
C                           if <= 0 then output value unchanged.
C      TB       D        Beginning time in days
C      TE       D        Ending time in days
C      NSOU     I        Number of selected sources
C      SOUIND   I(*)     Array of sources indexes selected
C      AN       I(*)     Array of selected antennas
C      NA       I        Number of selected antennas
C      ISUB     I        Selected subarray (= 0 any)
C      JSUB     I        Output subarray number (< 0 => NO CHANGE)
C   Input/Output:
C      BUFFER   I(*)     Work buffer
C      OBUFF    I(*)     Work buffer
C   Output:
C      IRET     I        Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, AN(*), NA, SOUIND(*),
     *   NSOU, ISUB, JSUB, BUFFER(*), OBUFF(*), IRET
      DOUBLE PRECISION TB, TE
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXSYC
      PARAMETER (MAXSYC=13)
C
      INTEGER   ISYRNO, SYKOLS(MAXSYC), SYNUMV(MAXSYC), NUMANT, NUMPOL,
     *   NUMIF, OKOLS(MAXSYC), ONUMV(MAXSYC), NSYROW, I, SOURID, ANTNO,
     *   SUBA, FREQID, OSYRNO, NEWNIF, IIF, JIF, IPOL, K, OVER, LBIF,
     *   NEWPOL, LBPOL, NDEL, NTOT, JRET, NPART, CALTYP
      REAL      PDIFF(2,MAXIF), PSUM(2,MAXIF), PGAIN(2,MAXIF), TIMEI
      DOUBLE PRECISION TIME
      LOGICAL   GOTIT, REFMT, GOTONE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'PUVCOP.INC'
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
      CALL SYINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI, ISYRNO,
     *   SYKOLS, SYNUMV, NUMANT, NUMPOL, NUMIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       New no. of IF's
      LBIF = MAX (1, BIF)
      NEWNIF = MIN (NUMIF, EIF) - LBIF + 1
      IF (NEWNIF.LE.0) THEN
         LBIF = 1
         NEWNIF = NUMIF
         END IF
      LBPOL = MAX (1, BPOL)
      NEWPOL = MIN (2, MIN (NUMPOL, EPOL)) - LBPOL + 1
      IF (NEWPOL.LE.0) THEN
         NEWPOL = NUMPOL
         LBPOL = 1
         END IF
      REFMT = (NEWNIF.NE.NUMIF) .OR. (NEWPOL.NE.NUMPOL)
C                                       # rows in old table
      NSYROW = BUFFER(5)
C                                       Open up new SY table
      OVER = VER
      CALL SYINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OSYRNO, OKOLS, ONUMV, NUMANT, NEWPOL, NEWNIF, IRET)
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
C                                       Sources selection
         IF ((NSOU.GT.0) .AND. (SOURID.GT.0)) THEN
            GOTIT = .FALSE.
            DO 20 K = 1,NSOU
               GOTIT = GOTIT .OR. (SOURID.EQ.SOUIND(K))
 20            CONTINUE
            IF (.NOT.GOTIT) IRET = -1
            END IF
C                                       Antennas selection
         IF ((NA.GT.0) .AND. (ANTNO.GT.0)) THEN
            GOTIT = .FALSE.
            DO 30 K = 1,NA
               GOTIT = GOTIT .OR. (ANTNO.EQ.AN(K))
 30            CONTINUE
            IF (.NOT.GOTIT) IRET = -1
            END IF
C                                       FQ selection
         IF ((IFQID.GT.0) .AND. (FREQID.GT.0) .AND. (IFQID.NE.FREQID))
     *      IRET = -1
C                                       Suba selection
         IF ((SUBA.GT.0) .AND. (ISUB.GT.0) .AND. (ISUB.NE.SUBA))
     *      IRET = -1
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
            DO 90 JIF = 1,NEWNIF
               IIF = JIF + BIF - 1
               DO 80 IPOL = 1,NEWPOL
                  K = IPOL + LBPOL - 1
                  PDIFF(IPOL,JIF) = PDIFF(K,IIF)
                  PSUM(IPOL,JIF) = PSUM(K,IIF)
                  PGAIN(IPOL,JIF) = PGAIN(K,IIF)
                  IF (PDIFF(IPOL,JIF).NE.FBLANK) GOTONE = .TRUE.
                  IF (PSUM(IPOL,JIF).NE.FBLANK) GOTONE = .TRUE.
 80               CONTINUE
 90            CONTINUE
            IF (GOTONE) THEN
               IF (IFQID.GT.0) FREQID = 1
               IF (JSUB.GE.0) SUBA = JSUB
               NTOT = NTOT + 1
               CALL TABSY ('WRIT', OBUFF, OSYRNO, OKOLS, ONUMV, NEWPOL,
     *            NEWNIF, TIME, TIMEI, CALTYP, SOURID, ANTNO, SUBA,
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
            WRITE (MSGTXT,1100) 'Reformatted SY', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied SY', DISKI, CNOI, VER, DISKO,
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
      SUBROUTINE SYFLG (NPOL, NIF, TIME, SOURID, ANTNO, SUBA,
     *   FREQID, PDIFF, PSUM, PGAIN, NPART, IRET)
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
      INCLUDE 'PUVCOP.INC'
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
