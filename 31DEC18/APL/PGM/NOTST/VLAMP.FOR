LOCAL INCLUDE 'VLAMP.INC'
C                                       Local include for VLAMP
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XCALOU(12)
      REAL      XSIN, XDISIN, XFQID, XSUBA, XFLAG, XINV, XGAINV, XINV2,
     *   BADD(10),
     *   SCRBUF(256), BUFF2(UVBFSS), CURTIM, FINC(MAXIF), CENCH
      INTEGER   SEQIN, DISKIN, JBUFSZ, OLDCNO, NANT, ISYVER, ICLVER,
     *   IGCVER, OSYVER, OCLVER, OGCVER, NIF, NPOL, EXTRAP,
     *   NMSG, TXLUN, TXIND, CURSOU, SUBARR, FGVER, FRQSEL,
     *   ISBAND(MAXIF), ANTOK(MAXANT), IBUFF2(UVBFSS)
      CHARACTER NAMEIN*12, CLAIN*6, CALOUT*48, BANDC*4
      DOUBLE PRECISION JD, JD0, FOFF(MAXIF)
      EQUIVALENCE (IBUFF2, BUFF2)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XFQID, XSUBA, XFLAG,
     *   XINV, XGAINV, XINV2, XCALOU, BADD
      COMMON /VLAMPP/ JD, JD0, FOFF, FINC, ISBAND, ANTOK, SEQIN,
     *   DISKIN, OLDCNO, CURTIM, NANT, NIF, NPOL, ISYVER, ICLVER,
     *   IGCVER, OSYVER, OCLVER, OGCVER, EXTRAP, NMSG, TXIND, TXLUN,
     *   CURSOU, SUBARR, FGVER, FRQSEL, CENCH
      COMMON /CHARPM/ NAMEIN, CLAIN, CALOUT, BANDC
      COMMON /BUFRS/ SCRBUF, BUFF2, JBUFSZ
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
C                                       End local include for VLAMP
LOCAL END
LOCAL INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:PUVD.INC'
C                                       set flag parameters
      INTEGER   MAXFLG
C                                       MAXFLG= max. no. flags active
      PARAMETER (MAXFLG=100001)
LOCAL END
LOCAL INCLUDE 'TYTABS.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PGCV.INC'
      INTEGER   SYBUFF(512), ISYRNO, SYKOLS(12), SYNUMV(12), LSTSOU,
     *   TRECA(MAXANT), TRECB(MAXANT)
      REAL      TS1A(2,MAXIF,MAXANT), TS1B(2,MAXIF,MAXANT),
     *   T1A(MAXANT), T1B(MAXANT), TCAL(4,MAXIF,MAXANT),
     *   VEFF(2,MAXIF,MAXANT), PSYS(2,MAXIF,MAXANT)
      COMMON /SYVALS/ SYBUFF, TS1A, TS1B, TCAL, T1A, T1B, PSYS, ISYRNO,
     *   SYKOLS, SYNUMV, LSTSOU, VEFF, TRECA, TRECB
      INTEGER   GCBUFF(512), GCKOLS(MAXGCC), GCNUMV(MAXGCC), IGCRNO,
     *   CLBUFF(512), ICLRNO, CLKOLS(MAXCLC), CLNUMV(MAXCLC)
      REAL      CLAMPS(2,MAXIF,MAXANT)
      COMMON /CLVALS/ CLAMPS, CLBUFF, GCBUFF, CLKOLS, CLNUMV, GCKOLS,
     *   GCNUMV, ICLRNO, IGCRNO
      INTEGER   MAXSCN
      PARAMETER (MAXSCN=5000)
      REAL      TIMENX(2,MAXSCN)
      INTEGER   SRCNX(MAXSCN), MAXNX, CURNX
      COMMON /NXVALS/ TIMENX, SRCNX, MAXNX, CURNX
LOCAL END
      PROGRAM VLAMP
C-----------------------------------------------------------------------
C! determine phased VLA sensitivity for VLBI
C# Utility UV VLA Calibration VLBI
C-----------------------------------------------------------------------
C;  Copyright (C) 2013-2018
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
C   VLAMP sums the nominal sensitivities from an SY table and forms
C   ANTAB-format values of Tsys and gain.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      SUBARRAY       SUBARR        Subarray to do
C      FREQID         FRQSEL        FREQID to do
C      INVERS         ISYVER        SY table version
C      GAINVER        ICLVER        CL table version
C      IN2VERS        IGCVER        GC table version
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'VLAMP.INC'
      INCLUDE 'TYTABS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'VLAMP '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL VLAMPI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL VLAMPD (IRET)
      IF (IRET.EQ.0) THEN
         IF (EXTRAP.GT.0) THEN
            WRITE (MSGTXT,1000) EXTRAP
            CALL MSGWRT (6)
            END IF
         CALL VLAMPH
         END IF
      CALL VLAMPC
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRBUF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('Note:',I12,' samples had to be extrapolated in time')
      END
      SUBROUTINE VLAMPI (PRGN, JERR)
C-----------------------------------------------------------------------
C   VLAMPI gets input parameters for VLAMP and creates an output file
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
C      INCIFO  I  Output IF's increment in vis.
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      INCLUDE 'VLAMP.INC'
      INCLUDE 'TYTABS.INC'
      CHARACTER STAT*4, PTYPE*2, KEYWRD*8, FRMATC*8, BNDCOD(MAXIF)*8,
     *   CALIN*48
      INTEGER   IROUND, NPARM, IERR, I, J, LUN(3), KEY(2,2), IP, IIF,
     *   IANT, VER, KEYSUB(2,2), NTERM
      REAL      FKEY(2,2), GMMOD
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANS.INC'
      DATA LUN /59,60,61/
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
C                                       SY format change date
      DATA FRMATC /'20110223'/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      TXIND = -1
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
      NPARM = 35
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
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (48, 1, XCALOU, CALOUT)
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
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
      CALL H2CHR (8, 1, CATH(KHDOB), KEYWRD)
      CALL JULDAY (KEYWRD, JD)
      FGVER = IROUND (XFLAG)
      CALL FNDEXT ('FG', CATBLK, I)
      IF (FGVER.GT.I) FGVER = I
      IF (FGVER.LE.0) FGVER = -1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       number antennas
      CALL GETANT (DISKIN, OLDCNO, SUBARR, CATBLK, IBUFF2, JERR)
      IF (JERR.NE.0) GO TO 999
      NANT = NSTNS
      CALL FILL (MAXANT, -1, ANTOK)
      DO 10 I = 1,NANT
         IF ((STNNAM(I).NE.'OUT') .AND. (STNNAM(I).NE.' '))
     *      ANTOK(I) = 1
 10      CONTINUE
      NPOL = CATBLK(KINAX+JLOCS)
      NPOL = MIN (2, NPOL)
      NIF = 1
      IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
C                                       SY Table version - flagging
      ISYVER = IROUND (XINV)
      CALL FNDEXT ('SY', CATBLK, J)
      IF (J.LE.0) THEN
         MSGTXT = 'NO SY TABLES: I QUIT'
         JERR = 10
         GO TO 990
         END IF
      IF ((ISYVER.LE.0) .OR. (ISYVER.GT.J)) ISYVER = J
      OSYVER = J + 1
      IF (FGVER.LE.0) OSYVER = J
C                                       CL
      ICLVER = IROUND (XGAINV)
      CALL FNDEXT ('CL', CATBLK, J)
      IF (J.LE.0) THEN
         MSGTXT = 'NO CL TABLES: I QUIT'
         JERR = 10
         GO TO 990
         END IF
      IF ((ICLVER.LE.0) .OR. (ICLVER.GT.J)) ICLVER = J
      OCLVER = J
C                                       GC
      IGCVER = IROUND (XINV2)
      CALL FNDEXT ('GC', CATBLK, J)
      IF (J.LE.0) THEN
         MSGTXT = 'NO GC TABLES: I WILL NO DO AS MUCH'
         CALL MSGWRT (6)
         END IF
      IF ((IGCVER.LE.0) .OR. (IGCVER.GT.J)) IGCVER = J
      OGCVER = J
C                                       Freq id
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.LE.0) FRQSEL = 1
C                                       Save input file info
C                                       Read CD table
      I = 0
      CALL GETCDS (DISKIN, OLDCNO, I, SUBARR, FRQSEL, CATBLK,
     *   TCAL, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       get frequencies, efficiencies
      I = 2 * MAXIF * MAXANT
      CALL RFILL (I, 1.0, VEFF)
      BANDC = ' '
      VER = 1
      CENCH = CATBLK(KINAX+JLOCF) / 2.0 + 1.0
      CALL CHNDAT ('READ', SYBUFF, DISKIN, OLDCNO, VER, CATBLK, LUN,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, JERR)
      IF (JERR.EQ.0) THEN
         DO 30 J = 1,NIF
            FOFF(J) = (FOFF(J) + CATD(KDCRV+JLOCF) + FINC(J) *
     *         (CENCH - CATR(KRCRP+JLOCF))) / 1.D9
 30         CONTINUE
         CALL GETBND (NIF, FOFF, BNDCOD, JERR, BANDC)
         CALIN = ' '
         CALL FNDEFF (NIF, FOFF, BANDC, NSTNS, CALIN, IBUFF2, VEFF)
         END IF
      JERR = 0
C                                       NX table info
      CALL GETNX (DISKIN, OLDCNO, CATBLK, SUBARR, CLBUFF, MAXNX, TIMENX,
     *   SRCNX, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       open and sort CL table
      CALL CALINI ('READ', CLBUFF, DISKIN, OLDCNO, ICLVER, CATBLK,
     *   LUN(2), ICLRNO, CLKOLS, CLNUMV, IANT, IP, IIF, NTERM, GMMOD,
     *   JERR)
      IF (JERR.NE.0) GO TO 999
      KEY(1,2) = 4
      KEY(1,1) = 1
      IF ((CLBUFF(43).NE.KEY(1,1)) .OR. (CLBUFF(44).NE.KEY(1,2)))
     *   THEN
         CALL TABIO ('CLOS', 0, ICLRNO, CLBUFF, CLBUFF, JERR)
         CALL TABSRT (DISKIN, OLDCNO, 'CL', ICLVER, ICLVER, KEY,
     *      KEYSUB, FKEY, CLBUFF, CATBLK, JERR)
         IF (JERR.NE.0) GO TO 999
         CALL CALINI ('READ', CLBUFF, DISKIN, OLDCNO, ICLVER, CATBLK,
     *      LUN(2), ICLRNO, CLKOLS, CLNUMV, IANT, IP, IIF, NTERM, GMMOD,
     *      JERR)
         IF (JERR.NE.0) GO TO 999
         END IF
      IF ((IP.NE.NPOL) .OR. (IIF.NE.NIF)) THEN
         MSGTXT = 'CL TABLE NO MATCH FOR POL AND/OR IFS'
         JERR = 10
         GO TO 999
         END IF
C                                       open GC table
      IF (IGCVER.GT.0) THEN
         CALL GCINI ('READ', GCBUFF, DISKIN, OLDCNO, IGCVER, CATBLK,
     *      LUN(3), IGCRNO, GCKOLS, GCNUMV, IP, IIF, NTERM, JERR)
         IF (JERR.NE.0) GO TO 999
         IF ((IP.NE.NPOL) .OR. (IIF.NE.NIF)) THEN
            MSGTXT = 'CL TABLE NO MATCH FOR POL AND/OR IFS'
            JERR = 10
            GO TO 999
            END IF
         END IF
C                                       open and sort SY table
      CALL SYINI ('READ', SYBUFF, DISKIN, OLDCNO, ISYVER, CATBLK,
     *   LUN(1), ISYRNO, SYKOLS, SYNUMV, IANT, IP, IIF, JERR)
      IF (JERR.NE.0) GO TO 999
      KEY(1,2) = 4
      KEY(1,1) = 1
      IF ((SYBUFF(43).NE.KEY(1,1)) .OR. (SYBUFF(44).NE.KEY(1,2)))
     *   THEN
         CALL TABIO ('CLOS', 0, ISYRNO, SYBUFF, SYBUFF, JERR)
         CALL TABSRT (DISKIN, OLDCNO, 'SY', ISYVER, ISYVER, KEY,
     *      KEYSUB, FKEY, SYBUFF, CATBLK, JERR)
         IF (JERR.NE.0) GO TO 999
         CALL SYINI ('READ', SYBUFF, DISKIN, OLDCNO, ISYVER, CATBLK,
     *      LUN(1), ISYRNO, SYKOLS, SYNUMV, IANT, IP, IIF, JERR)
         IF (JERR.NE.0) GO TO 999
         END IF
      IF ((IP.NE.NPOL) .OR. (IIF.NE.NIF)) THEN
         MSGTXT = 'SY TABLE NO MATCH FOR POL AND/OR IFS'
         JERR = 10
         GO TO 999
         END IF
C                                       Put input file in READ
      PTYPE = 'UV'
      STAT = 'WRIT'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, SCRBUF, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 1
C                                       open text file
      TXLUN = 3
      CALL ZTXOPN ('WRIT', TXLUN, TXIND, CALOUT, .FALSE., JERR)
      IF (JERR.NE.0) THEN
         TXIND = -1
         MSGTXT = 'UNABLE TO OPEN TEXT FILE'
         GO TO 990
         END IF
C                                       write initial info to text file
      CALL TXINIT (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       flag the SY table
      IF (OSYVER.NE.ISYVER) THEN
         CALL TABIO ('CLOS', 0, ISYRNO, SYBUFF, SYBUFF, JERR)
         CALL SYFSEL (DISKIN, OLDCNO, ISYVER, OSYVER, CATBLK,
     *      SYBUFF, IBUFF2, JERR)
         IF (JERR.NE.0) GO TO 999
         CALL SYINI ('READ', SYBUFF, DISKIN, OLDCNO, OSYVER, CATBLK,
     *      LUN(1), ISYRNO, SYKOLS, SYNUMV, IANT, IP, IIF, JERR)
         IF (JERR.NE.0) GO TO 999
         END IF
      JERR = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VLAMPI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
      END
      SUBROUTINE GETNX (DISK, CNO, CATBLK, ISUB, BUFFER, NOSCAN, TSCAN,
     *   SSCAN, IRET)
C-----------------------------------------------------------------------
C   GETNX reads the NX table and makes a list of scan boundaries
C   Inputs:
C      DISK     I        Disk number
C      CNO      I        Catalog number
C      CATBLK   I(*)     Header
C      ISUB     I        Limit to subarray ISUB - 0 -> all
C   Outputs
C      BUFFER   I(512)   Scratch buffer
C      NOSCAN   I        Number of times in TSCAN
C      TSCAN    R(2,*)   Times of scan boundaries
C      SSCAN    I(*)     source number for each scan
C      IRET     I        error code
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, CATBLK(256), ISUB, BUFFER(*), NOSCAN,
     *   SSCAN(*), IRET
      REAL      TSCAN(2,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   VER, INXRNO, NXKOLS(MAXNXC), NXNUMV(MAXNXC), LUN,
     *   LUNTMP, IDSOUR, SUBARR, VSTART, VEND, FREQID, NROW, IROW
      REAL      TIME, DTIME
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      VER = 1
      LUN = LUNTMP (1)
      CALL NDXINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN, INXRNO,
     *   NXKOLS, NXNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INDEX TABLE'
         GO TO 900
         END IF
      NROW = BUFFER(5)
      NOSCAN = 0
      DO 100 IROW = 1,NROW
         CALL TABNDX ('READ', BUFFER, INXRNO, NXKOLS, NXNUMV, TIME,
     *      DTIME, IDSOUR, SUBARR, VSTART, VEND, FREQID, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING INDEX TABLE'
            GO TO 900
            END IF
         IF ((ISUB.LE.0) .OR. (SUBARR.LE.0) .OR. (ISUB.EQ.SUBARR)) THEN
            NOSCAN = NOSCAN + 1
            TSCAN(1,NOSCAN) = TIME - 0.5 * DTIME
            TSCAN(2,NOSCAN) = TIME + 0.5 * DTIME
            SSCAN(NOSCAN) = IDSOUR
            END IF
 100     CONTINUE
C
 900  IF (IRET.NE.0) THEN
         CALL MSGWRT (6)
         NOSCAN = 0
         END IF
      CALL TABNDX ('CLOS', BUFFER, INXRNO, NXKOLS, NXNUMV, TIME, DTIME,
     *   IDSOUR, SUBARR, VSTART, VEND, FREQID, IROW)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETNX ERROR',I4,' ON ',A)
      END
      SUBROUTINE TXINIT (IRET)
C-----------------------------------------------------------------------
C   TXINIT write the text file header for VLAMP
C   Inputs from COMMON:
C      TXLUN   I   LUN for text file
C      TXIND   I   file pointer for text file
C   Outputs:
C      IRET    I   > 0 => fatal error
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER OLINE*132, OBS*8, DATEOB*8, CT1*12, CT2*12, SBC*1,
     *   POLC(2)*3
      INTEGER   JT, JTRIM, LUN, LUNTMP, LF, IP, J, IP0
      INCLUDE 'VLAMP.INC'
      INCLUDE 'TYTABS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA POLC /'RCP','LCP'/
C-----------------------------------------------------------------------
C                                       date header
      CALL VLDATE (OLINE)
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      OLINE = '! '
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Tsys
      OLINE = '! ----- Tsys information for Y -----'
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      OLINE = '! This section can be read by ANTAB in AIPS'
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      OLINE = '! System temperatures are given in channel order' //
     *   ' left to right'
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      OLINE = '!    followed by min and max number antennas' //
     *   ' contributing to the averages'
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      OLINE = '! A system temperature of 999.9 indicates a bad' //
     *   ' data point.'
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      OLINE = '! Times are in UT'
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      OLINE = '! Scan headings are Station id, observer, Source, ' //
     *   'calcode, and scan time range'
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      OLINE = '! "Channel" header: Chan Pol SB CentFreq Tcal(ant 1)' //
     *   ' band'
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      OLINE = '!'
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       first scan source
      JT = SRCNX(1)
      LUN = LUNTMP (1)
      CALL GETSOU (JT, DISKIN, OLDCNO, CATBLK, LUN, IRET)
      IF (IRET.EQ.11) THEN
         SNAME = '????????'
         WRITE (MSGTXT,1000) CURSOU
         CALL MSGWRT (6)
      ELSE IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING SOURCE INFORMATION'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL H2CHR (8, 1, CATH(KHOBS), OBS)
      CALL H2CHR (8, 1, CATH(KHDOB), DATEOB)
      JT = JTRIM (SNAME)
      JT = MAX (JT, 8)
      CALL TFORMF (DATEOB, TIMENX(1,1), CT1)
      CALL TFORMF (DATEOB, TIMENX(2,1), CT2)
      WRITE (OLINE,1010) OBS, SNAME(:JT), CALCOD, CT1, CT2
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       frequencies
      IF (CATD(KDCRV+JLOCS).EQ.-1.0D0) THEN
         IP0 = 0
      ELSE
         IP0 = 1
         END IF
      J = 0
      WRITE (OLINE,1019) CENCH
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      DO 30 LF = 1,NIF
         DO 20 IP = 1,NPOL
            J = J + 1
            IF (ISBAND(LF).GE.0) THEN
               SBC = 'U'
            ELSE
               SBC = 'L'
               END IF
            WRITE (OLINE,1020) J, POLC(IP+IP0), SBC, FOFF(LF)*1.D3,
     *         TCAL(IP,LF,1), BANDC(:2)
            JT = JTRIM (OLINE)
            CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
            IF (IRET.NE.0) GO TO 990
 20         CONTINUE
 30      CONTINUE
      OLINE = '!'
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      OLINE = 'TSYS  timeoff = 0.0  Y   FT = 1.0/'
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      OLINE = '!'
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      GO TO 999
C
 990  WRITE (MSGTXT,1990) IRET, 'WRITING OUTPUT TEXT FILE'
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TXINIT: SOURCE ID',I8,' NOT RECOGNIZED')
 1010 FORMAT ('! Y  ',A8,' ''',A,'''/''',A4,'''   ',A,'/',A)
 1019 FORMAT ('!!!  Frequencies are for spectral channel',F9.2)
 1020 FORMAT ('! ',I3,1X,A3,1X,A1,F10.2,'MHz',F7.2,2X,A2)
 1990 FORMAT ('TXINIT: ERROR',I4,' ON ',A)
      END
      SUBROUTINE VLDATE (OLINE)
C-----------------------------------------------------------------------
C   VLDATE processes the date information to make an output string
C   Outputs:
C      OLINE   C*(*)   Date information
C-----------------------------------------------------------------------
      CHARACTER OLINE*(*)
C
      INTEGER   IDATE(3,2), ITIME(4,2), DAYN(2), I, J, K, DAYMTH(12,2)
      REAL      TT(2)
      CHARACTER DTEMP*8, MONTH(12)*3
      INCLUDE 'VLAMP.INC'
      INCLUDE 'TYTABS.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA DAYMTH /31,28,31,30,31,30,31,31,30,31,30,31,
     *             31,29,31,30,31,30,31,31,30,31,30,31/
      DATA MONTH /'JAN','FEB','MAR','APR','MAY','JUN','JUL',
     *   'AUG','SEP','OCT','NOV','DEC'/
C-----------------------------------------------------------------------
      TT(1) = TIMENX(1,1)
      TT(2) = TIMENX(2,MAXNX)
C                                       figure out dates
      DO 50 I = 1,2
         CALL H2CHR (8, 1, CATH(KHDOB), DTEMP)
         CALL DATEST (DTEMP, IDATE(1,I))
         CALL DAYNUM (IDATE(1,I), IDATE(3,I), IDATE(2,I), DAYN(I))
C                                       next day(s)?
         J = TT(I)
         IF (J.GT.0) THEN
            DAYN(I) = DAYN(I) + J
            IDATE(3,I) = IDATE(3,I) + J
            TT(I) = TT(I) - J
 20         IF (MOD(IDATE(1,I),4).EQ.0) THEN
               K = 2
            ELSE
               K = 1
               END IF
C                                       next month?
            IF (IDATE(3,I).GT.DAYMTH(IDATE(2,I),K)) THEN
               IDATE(3,I) = IDATE(3,I) - DAYMTH(IDATE(2,I),K)
               IDATE(2,I) = IDATE(2,I) + 1
C                                       year?
               IF (IDATE(2,I).GT.12) THEN
                  IDATE(2,I) = IDATE(2,I)- 12
                  IDATE(1,I) = IDATE(1,I) + 1
                  DAYN(I) = DAYN(I) - 364 - K
                  END IF
               GO TO 20
               END IF
            END IF
C                                       times
         CALL TODHMS (TT(I), ITIME(1,I))
 50      CONTINUE
C                                       format
      WRITE (OLINE,1050) IDATE(1,1), MONTH(IDATE(2,1)), IDATE(3,1),
     *   DAYN(1), ITIME(2,1), ITIME(3,1), ITIME(4,1), IDATE(1,2),
     *   MONTH(IDATE(2,2)), IDATE(3,2), DAYN(2), ITIME(2,2), ITIME(3,2),
     *   ITIME(4,2)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('! For UT timerange: ',I4,A3,I2.2,'/',I3.3,' at ',
     *   2(I2.2,':'),I2.2,' to ',I4,A3,I2.2,'/',I3.3,' at ',2(I2.2,':'),
     *   I2.2)
      END
      SUBROUTINE TFORMF (DATEOB, TIME, CT)
C-----------------------------------------------------------------------
C   TFORMF formats the time as day number in year, HH MM SS
C   Inputs:
C      DATEOB   C*8    Date of observation in header
C      TIME     R      Time in hours wrt to date obs
C   Outputs:
C      CT       C*12   DDD HH:MM:SS
C-----------------------------------------------------------------------
      CHARACTER DATEOB*8, CT*(*)
      REAL      TIME
C
      INTEGER   ITT(4), DNUM
C-----------------------------------------------------------------------
      CALL DATEST (DATEOB, ITT)
      CALL DAYNUM (ITT(1), ITT(3), ITT(2), DNUM)
      CALL TODHMS (TIME, ITT)
      ITT(1) = ITT(1) + DNUM
      WRITE (CT,1000) ITT
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I3.3,'-',I2.2,2(':',I2.2))
      END
      SUBROUTINE VLAMPD (IRET)
C-----------------------------------------------------------------------
C   VLAMPD reads through the CL table, for each time it finds the SY
C   table values which apply, and writes the T_pa(t).
C   Outputs:
C      IRET   I   > 0 => failure
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'TYTABS.INC'
      INCLUDE 'VLAMP.INC'
      INTEGER   CLMAX, IP, LIF, I, NS, J, DATP(128,2), LRNO, NROW, KOL,
     *   RTYPE, NSMIN, NSMAX, IROW, CNT(MAXIF,MAXANT,4)
      REAL      SUM, VAL(2*MAXIF), T, SENS(MAXIF), SUM1, SUM2
      LOGICAL   DOSOUR
      CHARACTER KEY*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      CLMAX = CLBUFF(5)
      ICLRNO = 0
      I = MAXANT*MAXIF*4
      CALL FILL (I, 0, CNT)
 10   IF (ICLRNO.LT.CLMAX) THEN
         CALL GETCLV (T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'GETTING CL VALUES FROM GETCLV'
            GO TO 990
            END IF
         DOSOUR = CURSOU.NE.LSTSOU
         CALL GETSYV (T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'GETTING SY VALUES FROM GETSY'
            GO TO 990
            END IF
         J = 0
         NSMIN = NANT + 1
         NSMAX = 0
         DO 50 LIF = 1,NIF
            DO 40 IP = 1,NPOL
               SUM = 0.0
               NS = 0
               DO 30 I = 1,NANT
                  IF ((CLAMPS(IP,LIF,I).NE.FBLANK) .AND.
     *               (PSYS(IP,LIF,I).NE.FBLANK) .AND.
     *               (CLAMPS(IP,LIF,I).GT.0.0) .AND.
     *               (PSYS(IP,LIF,I).GT.0.0)) THEN
                     NS = NS + 1
C                                       CLAMPS is squared gain
                     SUM = SUM + 1.0 / (CLAMPS(IP,LIF,I) *
     *                  PSYS(IP,LIF,I))
                     CNT(LIF,I,IP) = CNT(LIF,I,IP) + 1
                     END IF
 30               CONTINUE
               J = J + 1
               NSMIN = MIN (NS, NSMIN)
               NSMAX = MAX (NS, NSMAX)
               IF (NS.GT.0) THEN
                  VAL(J) = NS / SUM
               ELSE
                  VAL(J) = 999.90
                  END IF
 40            CONTINUE
 50         CONTINUE
C                                       output values
         CALL TXOUTS (DOSOUR, T, J, VAL, NSMIN, NSMAX, IRET)
         GO TO 10
         END IF
C                                       close CL, SY
      CALL TABIO ('CLOS', 0, ISYRNO, BUFF2, SYBUFF, I)
      CALL TABIO ('CLOS', 0, ICLRNO, BUFF2, CLBUFF, I)
C                                       polarization 1
C                                       estimate # antennas
      CALL RFILL (2*NIF, 0.0, VAL)
      DO 120 LIF = 1,NIF
         J = LIF + NIF
         DO 100 I = 1,NANT
            IF (CNT(LIF,I,1).GT.0) VAL(J) = VAL(J) + 1.0
 100        CONTINUE
 120     CONTINUE
      CALL FILL (MAXIF, 0, CNT)
      KEY = 'SENS_1'
      CALL FNDCOL (1, KEY, 8, .TRUE., GCBUFF, KOL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINDING SENS_1 COLUMN IN GC TABLE'
         GO TO 990
         END IF
      NROW = GCBUFF(5)
      CALL ZFIO ('READ', GCBUFF(81), GCBUFF(82), GCBUFF(45), DATP, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING GC TABLE FOR DATP'
         GO TO 990
         END IF
      DO 140 IROW = 1,NROW
         CALL GETCOL (IROW, KOL, DATP, LRNO, GCBUFF, RTYPE, SENS,
     *      BUFF2, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING GC TABLE WITH GETCOL'
            GO TO 990
         ELSE IF (IRET.EQ.0) THEN
            DO 130 LIF = 1,NIF
               J = LIF + NIF
               IF ((VAL(J).GT.0.0) .AND. (SENS(LIF).NE.FBLANK) .AND.
     *            (SENS(LIF).GT.0.0)) THEN
                  VAL(LIF) = VAL(LIF) + SENS(LIF)
                  CNT(LIF,1,1) = CNT(LIF,1,1) + 1
                  END IF
 130           CONTINUE
            END IF
 140     CONTINUE
      T = 0.0
      SUM1 = 0.0
      DO 150 LIF = 1,NIF
         IF (CNT(LIF,1,1).GT.0) THEN
            VAL(LIF) = VAL(LIF) * VAL(LIF+NIF) / CNT(LIF,1,1)
            SUM1 = SUM1 + VAL(LIF)
            T = T + 1.0
            END IF
 150     CONTINUE
      IF (T.GT.0.0) SUM1 = SUM1 / T
C                                       polarization 2
      SUM2 = 0.0
      IF (NPOL.GT.1) THEN
C                                       estimate # antennas
         CALL RFILL (2*NIF, 0.0, VAL)
         DO 220 LIF = 1,NIF
            J = LIF + NIF
            DO 200 I = 1,NANT
               IF (CNT(LIF,I,2).GT.0) VAL(J) = VAL(J) + 1.0
 200           CONTINUE
 220        CONTINUE
         CALL FILL (MAXIF, 0, CNT)
         KEY = 'SENS_2'
         CALL FNDCOL (1, KEY, 8, .TRUE., GCBUFF, KOL, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET,
     *         'FINDING SENS_2 COLUMN IN GC TABLE'
            GO TO 990
            END IF
         NROW = GCBUFF(5)
         CALL ZFIO ('READ', GCBUFF(81), GCBUFF(82), GCBUFF(45), DATP,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING GC TABLE FOR DATP'
            GO TO 990
            END IF
         DO 240 IROW = 1,NROW
            CALL GETCOL (IROW, KOL, DATP, LRNO, GCBUFF, RTYPE, SENS,
     *         BUFF2, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING GC TABLE WITH GETCOL'
               GO TO 990
            ELSE IF (IRET.EQ.0) THEN
               DO 230 LIF = 1,NIF
                  J = LIF + NIF
                  IF ((VAL(J).GT.0.0) .AND. (SENS(LIF).NE.FBLANK) .AND.
     *               (SENS(LIF).GT.0.0)) THEN
                     VAL(LIF) = VAL(LIF) + SENS(LIF)
                     CNT(LIF,1,1) = CNT(LIF,1,1) + 1
                     END IF
 230              CONTINUE
               END IF
 240        CONTINUE
         T = 0.0
         SUM2 = 0.0
         DO 250 LIF = 1,NIF
            IF (CNT(LIF,1,1).GT.0) THEN
               VAL(LIF) = VAL(LIF) * VAL(LIF+NIF) / CNT(LIF,1,1)
               SUM2 = SUM2 + VAL(LIF)
               T = T + 1.0
               END IF
 250        CONTINUE
         CALL TABIO ('CLOS', 0, IGCRNO, BUFF2, GCBUFF, I)
         IF (T.GT.0.0) SUM2 = SUM2 / T
         END IF
C                                       write GC information
      CALL TXSENS (SUM1, SUM2, IRET)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VLAMPD: ERROR',I5,' ON ',A)
      END
      SUBROUTINE TXSENS (VAL1, VAL2, IRET)
C-----------------------------------------------------------------------
C   TXSENS write the text file sensitivity line
C   Inputs:
C      VAL     R   value
C   Outputs:
C      IRET    I   error code
C-----------------------------------------------------------------------
      INTEGER   IRET
      REAL      VAL1, VAL2
C
      INCLUDE 'VLAMP.INC'
      INTEGER   JT, JTRIM, I
      CHARACTER OLINE*132
C----------------------------------------------------------------------
      OLINE = '!'
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      OLINE = '/'
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      OLINE = '! ----- Gain information for Y -----'
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      OLINE = '!'
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       data line
      IF (NPOL.LT.2) THEN
         WRITE (OLINE,1000) VAL1
      ELSE
         WRITE (OLINE,1001) VAL1, VAL2
         END IF
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (OLINE,1010)
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (OLINE,1020) BANDC(:JTRIM(BANDC))
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      GO TO 995
C
 990  WRITE (MSGTXT,1990) IRET, 'WRITING GC VALUES TO TEXT FILE'
      CALL MSGWRT (8)
C
 995  CALL ZTXCLS (TXLUN, TXIND, I)
      TXIND = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Y GAIN ALTAZ DPFU=',F8.4)
 1001 FORMAT ('Y GAIN ALTAZ DPFU=',F8.4,' ,',F8.4)
 1010 FORMAT ('   POLY = 1.0   /')
 1020 FORMAT ('! BAND = ''',A,'''')
 1990 FORMAT ('TXSENS ERROR',I4,' ON ',A)
      END
      SUBROUTINE TXOUTS (DOSOUR, TIME, NVAL, VALS, NMIN, NMAX, IRET)
C-----------------------------------------------------------------------
C   TXOUTS outputs the data values perhaps preceded by a scan line
C   Inputs:
C      DOSOUR   L      T => do scan lines
C      TIME     R      Time of observation
C      NVAL     I      Number of values
C      VALS     R(*)   Values to output
C      NMIN     I      Minimum # values in average
C      NMAX     I      Maximum # values in average
C   Outputs:
C      IRET     I      > 0 => fatal error
C-----------------------------------------------------------------------
      LOGICAL   DOSOUR
      REAL      TIME, VALS(*)
      INTEGER   NVAL, NMIN, NMAX, IRET
C
      CHARACTER OLINE*1100, OBS*8, DATEOB*8, CT1*12, CT2*12
      INTEGER   JT, JTRIM, I
      REAL      TEPS
      INCLUDE 'VLAMP.INC'
      INCLUDE 'TYTABS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IF (DOSOUR) THEN
         OLINE = '!'
         JT = JTRIM (OLINE)
         CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
         IF (IRET.NE.0) GO TO 990
C                                       find scan
         TEPS = -5.0 / (24.0 * 3600.0)
         DO 10 CURNX = 1,MAXNX
            IF (CURSOU.EQ.SRCNX(CURNX)) THEN
               IF ((TIME-TIMENX(1,CURNX).GE.TEPS) .AND.
     *            (TIMENX(2,CURNX)-TIME.GE.TEPS)) GO TO 20
               END IF
 10         CONTINUE
         WRITE (MSGTXT,1010) CURSOU, TIME
         CALL MSGWRT (6)
 20      CALL H2CHR (8, 1, CATH(KHOBS), OBS)
         CALL H2CHR (8, 1, CATH(KHDOB), DATEOB)
         JT = JTRIM (SNAME)
         JT = MAX (JT, 8)
         IF (CURNX.LE.MAXNX) THEN
            CALL TFORMF (DATEOB, TIMENX(1,CURNX), CT1)
            CALL TFORMF (DATEOB, TIMENX(2,CURNX), CT2)
         ELSE
            CALL TFORMF (DATEOB, TIME, CT1)
            CALL TFORMF (DATEOB, TIME, CT2)
            END IF
         WRITE (OLINE,1020) OBS, SNAME(:JT), CALCOD, CT1, CT2
         JT = JTRIM (OLINE)
         CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
C                                       data line
      CALL TFORMD (DATEOB, TIME, OLINE)
      WRITE (OLINE(14:),1030) (VALS(I), I = 1,NVAL)
      JT = JTRIM (OLINE)
      WRITE (OLINE(JT+1:),1031) NMIN, NMAX
      JT = JTRIM (OLINE)
      CALL ZTXIO ('WRITE', TXLUN, TXIND, OLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      GO TO 999
C
 990  WRITE (MSGTXT,1990) IRET, 'WRITING OUTPUT TEXT FILE'
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('TXOUTS: CANNOT FIND SOURCE',I4,' AT',F10.6)
 1020 FORMAT ('! Y  ',A8,' ''',A,'''/''',A4,'''   ',A,'/',A)
 1030 FORMAT (180F6.1)
 1031 FORMAT (' !',I3,'-',I2)
 1990 FORMAT ('TXINIT: ERROR',I4,' ON ',A)
      END
      SUBROUTINE TFORMD (DATEOB, TIME, CT)
C-----------------------------------------------------------------------
C   TFORMF formats the time as day number in year, HH MM SS
C   Inputs:
C      DATEOB   C*8    Date of observation in header
C      TIME     R      Time in hours wrt to date obs
C   Outputs:
C      CT       C*12   DDD HH:MM:SS
C-----------------------------------------------------------------------
      CHARACTER DATEOB*8, CT*(*)
      REAL      TIME
C
      INTEGER   ITT(4), DNUM
      REAL      TEMP
C-----------------------------------------------------------------------
      CALL DATEST (DATEOB, ITT)
      CALL DAYNUM (ITT(1), ITT(3), ITT(2), DNUM)
      CALL TODHMS (TIME, ITT)
      TEMP = TIME
      ITT(1) = TEMP
      TEMP = (TEMP - ITT(1)) * 24.0
      ITT(2) = TEMP
      TEMP = (TEMP - ITT(2)) * 60.0
      ITT(1) = ITT(1) + DNUM
      WRITE (CT,1000) ITT(1), ITT(2), TEMP
      IF (CT(8:8).EQ.' ') CT(8:8) = '0'
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I3.3,1X,I2.2,':',F6.3)
      END
      SUBROUTINE VLAMPH
C-----------------------------------------------------------------------
C   VLAMPH copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72
      INTEGER   LUN, IERR, JTRIM, J
      INCLUDE 'VLAMP.INC'
      INCLUDE 'TYTABS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN /27/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       output file written
      CALL HIOPEN (LUN, DISKIN, OLDCNO, IBUFF2, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN OLD HISTORY FILE'
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       versions applied
      WRITE (HILINE,1010) TSKNAM, ISYVER
      CALL HIADD (LUN, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1011) TSKNAM, ICLVER
      CALL HIADD (LUN, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1012) TSKNAM, IGCVER
      CALL HIADD (LUN, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
      IF (FGVER.GT.0) THEN
         WRITE (HILINE,1015) TSKNAM, FGVER
         CALL HIADD (LUN, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       output file
      J = JTRIM (CALOUT)
      WRITE (HILINE,1020) TSKNAM, CALOUT(:J)
      CALL HIADD (LUN, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Close HI file
 100  CALL HICLOS (LUN, .TRUE., IBUFF2, IERR)
C                                       Copy tables
      CALL CATIO ('UPDT', DISKIN, OLDCNO, CATBLK, 'REST', SCRBUF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VLAMPH: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'INVERS  =',I5,5X,'/ SY file version')
 1011 FORMAT (A6,'GAINVER =',I5,5X,'/ CL file version')
 1012 FORMAT (A6,'IN2VERS =',I5,5X,'/ GC file version')
 1015 FORMAT (A6,'FLAGVER =',I5,5X,'/ Flag file version applied to SY')
 1020 FORMAT (A6,'OUTTEXT =''',A,'''')
      END
      SUBROUTINE GETSYV (T, IRET)
C-----------------------------------------------------------------------
C   GETSYV fills in the set of SY values for nominal sensitivities.
C   It then computes PSYS for time T.
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
      INCLUDE 'VLAMP.INC'
      INCLUDE 'TYTABS.INC'
      INTEGER   I, J, RNOMAX, SOURID, ANTNO, SUBA, FREQID, INEED, IA,
     *   INRNO, SYRNO, II, CALTYP
      REAL      TIMEI, PDIF(2,MAXIF), PSUM(2,MAXIF), PGAIN(2,MAXIF),
     *   TEPS, TINY, TENSEC, F1
      DOUBLE PRECISION TIME
      LOGICAL   NEWSOU, NEED, ANEED(MAXANT)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      SAVE SYRNO
      DATA SYRNO /1/
C-----------------------------------------------------------------------
C                                       remove nom sens
      IF (ISYVER.GT.0) THEN
         NEWSOU = CURSOU.NE.LSTSOU
         IF (NEWSOU) THEN
            NEED = .TRUE.
            INEED = NANT
            CALL LFILL (MAXANT, .TRUE., ANEED)
            CALL RFILL (MAXANT, 1.E5, T1A)
            DO 5 ANTNO = 1,NANT
               IF (ANTOK(ANTNO).LE.0) ANEED(ANTNO) = .FALSE.
 5             CONTINUE
            CALL RFILL (MAXANT, -1.E5, T1B)
            I = 2 * MAXANT * MAXIF
            CALL RFILL (I, FBLANK, TS1A)
            CALL RFILL (I, FBLANK, TS1B)
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
      IF ((ISYVER.GT.0) .AND. (NEED)) THEN
         RNOMAX = SYBUFF(5)
 20      IRET = 999
         IF (SYRNO.LE.RNOMAX) CALL TABSY ('READ', SYBUFF, SYRNO,
     *      SYKOLS, SYNUMV, NPOL, NIF, TIME, TIMEI, CALTYP, SOURID,
     *      ANTNO, SUBA, FREQID, PDIF, PSUM, PGAIN, IRET)
         IF (IRET.EQ.0) THEN
            IF ((SUBA.GT.0) .AND. (SUBARR.GT.0) .AND. (SUBA.NE.SUBARR))
     *         GO TO 20
            IF ((FREQID.GT.0) .AND. (FRQSEL.GT.0) .AND.
     *         (FREQID.NE.FRQSEL)) GO TO 20
C                                       record useful
            IF ((SOURID.LE.0) .OR. (CURSOU.LE.0) .OR.
     *         (SOURID.EQ.CURSOU)) THEN
               IF (T.GT.T1B(ANTNO)) THEN
C                                       convert units, check
                  DO 30 I = 1,NIF
                     DO 25 J = 1,2
                        IF ((PDIF(J,I).EQ.FBLANK) .OR.
     *                     (PSUM(J,I).EQ.FBLANK) .OR.
     *                     (TCAL(J,I,ANTNO).EQ.FBLANK) .OR.
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
                     TRECA(ANTNO) = SYRNO-1
                     END IF
C                                       keep as second time
                  IF (T.GT.T1B(ANTNO)) THEN
                     T1B(ANTNO) = TIME
                     CALL RCOPY (I, PSUM, TS1B(1,1,ANTNO))
                     TRECB(ANTNO) = SYRNO-1
                     IF ((T.GE.T1A(ANTNO)) .AND. (T.LE.T1B(ANTNO)) .AND.
     *                  (ANEED(ANTNO))) THEN
                        ANEED(ANTNO) = .FALSE.
                        INEED = INEED - 1
                        END IF
                     END IF
                  GO TO 20
               ELSE
                  SYRNO = SYRNO - 1
                  END IF
C                                       another source
            ELSE
               IF (TIME.LT.T) GO TO 20
               SYRNO = SYRNO - 1
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
               IF (((TRECA(IA).GT.0) .OR. (TRECB(IA).GT.0)) .AND.
     *            (ANEED(IA)) .AND. (ANTOK(IA).GT.0)) THEN
                  INRNO = MIN (TRECA(IA), TRECB(IA)) + 1
                  INRNO = MAX (INRNO, 1)
                  DO 80 II = INRNO,RNOMAX
                     SYRNO = II
                     CALL TABSY ('READ', SYBUFF, SYRNO, SYKOLS, SYNUMV,
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
                           IF ((PDIF(J,I).EQ.FBLANK) .OR.
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
                        TRECA(ANTNO) = SYRNO-1
                        END IF
C                                       keep as second time
                     IF (T.GT.T1B(ANTNO)) THEN
                        T1B(ANTNO) = TIME
                        CALL RCOPY (I, PSUM, TS1B(1,1,ANTNO))
                        TRECB(ANTNO) = SYRNO-1
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
      LSTSOU = CURSOU
C                                       Now find the system temperatures
      TENSEC = 10.1 / (24.0 * 3600.0)
      TINY = MAX (TENSEC / 100.0, TIMEI/2.0)
      DO 200 IA = 1,NANT
C                                       check times again
         TEPS = MIN (6.*TENSEC, MAX (TENSEC, T1B(IA)-T1A(IA)))
         IF (ANTOK(IA).GT.0) THEN
            IF ((T.LT.T1A(IA)-TINY) .OR. (T.GT.T1B(IA)+TINY)) THEN
               EXTRAP = EXTRAP + 1
               IF ((T.LT.T1A(IA)-TEPS) .OR. (T.GT.T1B(IA)+TEPS)) THEN
                  IF (NMSG.LT.100) THEN
                     WRITE (MSGTXT,1000) T, IA, T1A(IA), T1B(IA)
                     CALL MSGWRT (7)
                     NMSG = NMSG + 1
                     END IF
                  END IF
               END IF
            END IF
         F1 = 0.0
         IF (T1A(IA).NE.T1B(IA)) F1 = (T - T1A(IA)) /
     *      (T1B(IA) - T1A(IA))
         DO 150 I = 1,NIF
            DO 140 J = 1,NPOL
               IF ((TS1A(J,I,IA).NE.FBLANK) .AND.
     *            (TS1B(J,I,IA).NE.FBLANK)) THEN
                  PSYS(J,I,IA) = (1.0-F1) * TS1A(J,I,IA) +
     *               F1 * TS1B(J,I,IA)
               ELSE IF ((TS1A(J,I,IA).NE.FBLANK) .AND. (F1.LE.0.3)) THEN
                  PSYS(J,I,IA) = TS1A(J,I,IA)
               ELSE IF ((TS1B(J,I,IA).NE.FBLANK) .AND. (F1.GE.0.7)) THEN
                  PSYS(J,I,IA) = TS1B(J,I,IA)
               ELSE
                  PSYS(J,I,IA) = FBLANK
                  END IF
 140           CONTINUE
 150        CONTINUE
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SY T1=',F11.6,' AN',I3,' NOT IN',2F14.6)
      END
      SUBROUTINE GETCLV (T, IRET)
C-----------------------------------------------------------------------
C   GETCLV finds the next time in the CL table and returns that time
C   in the call sequence and the CL table amplitudes in COMMON.  It
C   also gets source info for any new source
C   Outputs:
C      T      R   Time in days
C      IRET   I   > 0 => error
C-----------------------------------------------------------------------
      REAL      T
      INTEGER   IRET
C
      INCLUDE 'TYTABS.INC'
      INCLUDE 'VLAMP.INC'

      INTEGER   IREC, NREC, JREC, SOURID, ANTNO, SUBA, FREQID, I, LF,
     *   REFA(2,MAXIF), FOUND(MAXANT), LUN, LUNTMP, IP, FREC, GOTONE
      REAL      TIMEI, IFR, DOPOFF(MAXIF), ATMOS(2), DATMOS(2),
     *   MBDELY(2), CLOCK(2), DCLOCK(2), DISP(2), DDISP(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *   WEIGHT(2,MAXIF)
      DOUBLE PRECISION TIME, GEODLY(10), TT, DT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
 10   NREC = CLBUFF(5)
      JREC = ICLRNO + 1
      DT = 0.11D0 / (24.0D0 * 3600.0D0)
      FREC = 0
      DO 100 IREC = JREC,NREC
         ICLRNO = IREC
         CALL TABCAL ('READ', CLBUFF, ICLRNO, CLKOLS, CLNUMV, NPOL,
     *      NIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *      GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *      DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING OLD CL TABLE'
            GO TO 990
            END IF
C                                       match ?
         IF ((SUBARR.GT.0) .AND. (SUBA.GT.0) .AND. (SUBARR.NE.SUBA))
     *      GO TO 100
         IF ((FRQSEL.GT.0) .AND. (FREQID.GT.0) .AND. (FRQSEL.NE.FREQID))
     *      GO TO 100
C                                       first in this group
         IF (FREC.LE.0) THEN
            FREC = IREC
            TT = TIME
            T = TIME
            I = 2 * MAXIF * NANT
            CALL RFILL (I, FBLANK, CLAMPS)
            CALL FILL (NANT, 0, FOUND)
            GOTONE = -1
            CURSOU = SOURID
            IF (CURSOU.NE.LSTSOU) THEN
               LUN = LUNTMP (1)
               CALL GETSOU (CURSOU, DISKIN, OLDCNO, CATBLK, LUN, IRET)
               IF (IRET.EQ.11) THEN
                  SNAME = '????????'
                  WRITE (MSGTXT,1010) CURSOU
                  CALL MSGWRT (6)
               ELSE IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READING SOURCE INFORMATION'
                  GO TO 990
                  END IF
               END IF
C                                       are we done?
         ELSE
            IF (TIME-TT.GT.DT) THEN
               ICLRNO = IREC - 1
               GO TO 999
               END IF
            IF (SOURID.NE.CURSOU) THEN
               WRITE (MSGTXT,1015) SOURID, CURSOU
               ICLRNO = IREC - 1
               GO TO 990
               END IF
            IF (FOUND(ANTNO).GT.0) THEN
               WRITE (MSGTXT,1020) ANTNO
               ICLRNO = IREC - 1
               GO TO 990
               END IF
            END IF
C                                       not done save data
         DO 50 LF = 1,NIF
            DO 40 IP = 1,NPOL
               IF ((CREAL(IP,LF).NE.FBLANK) .AND.
     *            (CIMAG(IP,LF).NE.FBLANK)) THEN
                  CLAMPS(IP,LF,ANTNO) = CREAL(IP,LF)**2 +
     *               CIMAG(IP,LF)**2
                  FOUND(ANTNO) = 1
                  GOTONE = 1
                  END IF
 40            CONTINUE
 50         CONTINUE
 100     CONTINUE
      IF (GOTONE.LE.0) GO TO 10
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETCLV: ERROR',I4,' ON ',A)
 1010 FORMAT ('GETCLV: SOURCE ID',I8,' NOT RECOGNIZED')
 1015 FORMAT ('GETCLV: TWO SOURCE IDS IN SAME TIME',2I8)
 1020 FORMAT ('GETCLV: ANTENNA',I4,' TWO VALUES WITHIN ONE TIME')
      END
      SUBROUTINE VLAMPC
C-----------------------------------------------------------------------
C   VLAMPC closes the text file and deletes any special SY tables
C-----------------------------------------------------------------------
C
      INCLUDE 'VLAMP.INC'
      INTEGER   IERR, JERR
      CHARACTER PHNAME*48
C-----------------------------------------------------------------------
C                                       close text file
      IF (TXIND.GT.0) THEN
         CALL ZTXCLS (TXLUN, TXIND, IERR)
         TXIND = -1
         END IF
C                                       delete SY table
      CALL FNDEXT ('SY', CATBLK, IERR)
      IF ((OSYVER.GT.ISYVER) .AND. (OSYVER.EQ.IERR)) THEN
         CALL ZPHFIL ('SY', DISKIN, OLDCNO, OSYVER, PHNAME, IERR)
         CALL ZDESTR (DISKIN, PHNAME, IERR)
         CALL DELEXT ('SY', DISKIN, OLDCNO, 'WRWR', CATBLK, SCRBUF,
     *      OSYVER, JERR)
         IF ((IERR.NE.0) .OR. (JERR.NE.0)) THEN
            WRITE (MSGTXT,1000) IERR, 'DELETING FLAGGED SY TABLE'
            IF (IERR.GT.0) CALL MSGWRT (7)
            WRITE (MSGTXT,1000) JERR, 'REMOVING FLAGGED SY TABLE' //
     *         ' FROM HEADER'
            IF (JERR.GT.0) CALL MSGWRT (7)
         ELSE
            WRITE (MSGTXT,1010) OSYVER
            CALL MSGWRT (2)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VLAMPC: ERROR',I4,' ON ',A)
 1010 FORMAT ('VLAMPC deleted flagged SY table version',I4)
      END
      SUBROUTINE SYFSEL (DISK, CNO, IVER, OVER, CATBLK, BUFFER, OBUFF,
     *   IRET)
C-----------------------------------------------------------------------
C   Copies an SY table applying data flags
C   Inputs:
C      DISKI    I        Input volume number
C      CNOI     I        Input catalog number
C      IVER     I        Version to check/modify
C      OVER     I        output version
C      CATBLK   I(256)   Input/output catalog header
C   Input/Output:
C      BUFFER   I(*)     Work buffer
C      OBUFF    I(*)     Work buffer
C   Output:
C      IRET     I        Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, IVER, CATBLK(256), BUFFER(*), OBUFF(*), IRET
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
      LUNI = LUNTMP (1)
      CALL SYINI ('READ', BUFFER, DISK, CNO, IVER, CATBLK, LUNI, ISYRNO,
     *   SYKOLS, SYNUMV, NUMANT, NUMPOL, NUMIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       # rows in old table
      NSYROW = BUFFER(5)
C                                       Open up new SY table
      LUNO = LUNTMP (1)
      CALL SYINI ('WRIT', OBUFF, DISK, CNO, OVER, CATBLK, LUNO,
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
         GOTONE = .FALSE.
         DO 90 JIF = 1,NUMIF
            DO 80 IPOL = 1,NUMPOL
               IF (PDIFF(IPOL,JIF).NE.FBLANK) GOTONE = .TRUE.
               IF (PSUM(IPOL,JIF).NE.FBLANK) GOTONE = .TRUE.
 80            CONTINUE
 90         CONTINUE
         IF (GOTONE) THEN
            NTOT = NTOT + 1
            CALL TABSY ('WRIT', OBUFF, OSYRNO, OKOLS, ONUMV, NUMPOL,
     *         NUMIF, TIME, TIMEI, CALTYP, SOURID, ANTNO, SUBA, FREQID,
     *         PDIFF, PSUM, PGAIN, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1040) IRET
               GO TO 990
               END IF
         ELSE
            REFMT = .TRUE.
            END IF
 100     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ISYRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OSYRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted SY', DISK, CNO, IVER,
     *         OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied SY', DISK, CNO, IVER, OVER
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
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to vers',I4)
 1101 FORMAT ('__Fully deleted',I10,' of',I12,' SY records applying',
     *   ' flag table')
 1102 FORMAT ('__Partly flagged',I10,' SY records applying flag table')
      END
      SUBROUTINE SYFLG (NUMPOL, NUMIF, TIME, SOURID, ANTNO, SUBA,
     *   FREQID, PDIFF, PSUM, PGAIN, NPART, IRET)
C-----------------------------------------------------------------------
C   Flags a TY table row based on the flags in FG table loaded to Common
C   Inputs:
C      NUMPOL   I      Number polarizations in TY data
C      NUMIF    I      Number of IFs in those data
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
      INTEGER   NUMPOL, NUMIF, SOURID, ANTNO, SUBA, FREQID, NPART, IRET
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
         IF (IPOLPT.GT.4) IPOLPT = IPOLPT - 4
         DO 40 JPOLN = 1,NUMPOL
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
      DO 70 JPOLN = 1,NUMPOL
         DO 60 JIF = 1,NUMIF
            IF ((PDIFF(JPOLN,JIF).NE.FBLANK) .OR.
     *         (PSUM(JPOLN,JIF).NE.FBLANK)) GO TO 999
 60         CONTINUE
 70      CONTINUE
      IF (PART) NPART = NPART - 1
      IRET = -1
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
C   **** REVISED VERSION NOT DEPENDING ON DSEL.INC ***
C   **** IGNORES SOURCE, FQ, IF SELECTIONS
C-----------------------------------------------------------------------
      REAL      TIME
      LOGICAL   TABLE
      INTEGER   IERR
C
      INTEGER   NDROP, LIMIT, RECI(30), MXFLG, SOUKOL, SUBKOL, FRQKOL,
     *   ANTKOL, TIMKOL, IFKOL, CHKOL, POLKOL, REAKOL, A1, A2, IT, I4,
     *   NFGREC, I, LIMIT4
      REAL      RECORD(31)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'PUVCOP.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
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
 360     CONTINUE
      IERR = 0
      GO TO 999
C                                       Next entry
      NUMFLG = NUMFLG + 1
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
         IF (JLOCIF.GT.0) FLGEIF(NUMFLG) = CATBLK (KINAX+JLOCIF)
         IF (JLOCIF.LE.0) FLGEIF(NUMFLG) = 1
         END IF
      FLGBCH(NUMFLG) = RECI(CHKOL)
      FLGECH(NUMFLG) = MIN (CATBLK(KINAX+JLOCF), RECI(CHKOL+1))
      IF (FLGBCH(NUMFLG).LE.0) FLGBCH(NUMFLG) = 1
      IF (FLGECH(NUMFLG).LE.0) FLGECH(NUMFLG) = CATBLK (KINAX+JLOCF)
C                                       Ensure that IF and channel
C                                       selection are in range
      FLGEIF(NUMFLG) = MIN (FLGEIF(NUMFLG), CATBLK(KINAX+JLOCIF))
      FLGECH(NUMFLG) = MIN (FLGECH(NUMFLG), CATBLK(KINAX+JLOCF))
C
      CALL LG2BIT (I4, FLGPOL(1,NUMFLG), RECI(POLKOL), -1)
C                                       table ignores baseline based
C                                       and channel based
      IF (TABLE) THEN
         IF ((FLGBAS(NUMFLG).NE.0) .OR. (FLGBCH(NUMFLG).NE.1) .OR.
     *      (FLGECH(NUMFLG).NE.CATBLK(KINAX+JLOCF))) NUMFLG = NUMFLG - 1
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
