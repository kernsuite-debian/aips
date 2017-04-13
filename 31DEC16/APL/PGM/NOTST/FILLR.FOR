LOCAL INCLUDE 'FILLR.INC'
C                                       Local include for FILLR
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ITAPE, NFILES, JQUAL, DISKO, SEQOUT, CATBLK(256), NCHAN,
     *   NPOLN, NIF, NUMHIS, JBUFSZ, NANT, BIF, EIF, CURSOU, NSOUR,
     *   SOURID(300), NXBUFF(512), SUBUFF(512), CLBUFF(1024), NXKOLS(7),
     *   NXNUMV(7), SUKOLS(MAXSUC), SUNUMV(MAXSUC), CLKOLS(MAXCLC),
     *   CLNUMV(MAXCLC), INXRNO, ISURNO, ICLRNO, MAXKVS
      LOGICAL   DOALL
      REAL      XTAPE, XNFILE, XDOALL, QUAL, XNPOI, XBIF, XEIF, XSOUT,
     *   XDISO, DPARM(10), BANDW, BUFFER(UVBFSS), XIAT, XUT1, POLARX,
     *   POLARY
      HOLLERITH XSOURC(2), XBAND, XVLAOB(2), XVLAMO, XNAMOU(3),
     *   XCLAOU(2)
      CHARACTER SOURC*8, BAND*4, VLAOBS*6, VLAMOD*2, NAMOUT*12,
     *   CLAOUT*6, OBSR*8, OBSDAT*8, HISCRD(10)*64, ANTNAM(30)*8,
     *   SULIST(300)*8, CALKOD*1
      DOUBLE PRECISION ANTLOC(3,30), GST0, RMJAD, RAAPP, DECAPP
      COMMON /FLRCHR/ SOURC, BAND, VLAOBS, VLAMOD, NAMOUT, CLAOUT,
     *   OBSR, OBSDAT, HISCRD, ANTNAM, SULIST, CALKOD
      COMMON /ANTS/ ANTLOC, GST0, RMJAD, RAAPP, DECAPP,
     *   XIAT, XUT1, POLARX, POLARY, NANT
      COMMON /BUFRS/ BUFFER, JBUFSZ
      COMMON /INPARM/ XTAPE, XNFILE, XDOALL,
     *   XSOURC, XBAND, QUAL, XVLAOB, XVLAMO,
     *   XNPOI, XBIF, XEIF, XNAMOU, XCLAOU, XSOUT, XDISO, DPARM,
     *   BANDW, DOALL,
     *   ITAPE, NFILES, JQUAL, MAXKVS, SEQOUT, DISKO,
     *   NCHAN, NPOLN, NIF, BIF, EIF, NUMHIS
      COMMON /FLRINF/ INXRNO, ISURNO, ICLRNO,
     *   CURSOU, NSOUR, SOURID, NXBUFF, SUBUFF, CLBUFF, NXKOLS, NXNUMV,
     *   SUKOLS, SUNUMV, CLKOLS, CLNUMV
      COMMON /MAPHDR/ CATBLK
C                                                          End FILLR.
LOCAL END
LOCAL INCLUDE 'MC.INC'
C                                                          Include MC.
C                                       Local include for MODCOMP VLA
      INTEGER   MCLUSE, MCLSUB, MCLNF1, MCLSTR, NOFILE,
     *          TAPBFZ, TAPIND, RECORD(100000), TAPBUF(4096),
     *          MCFNRC, MCFLIN(20), RECANT(50), BCODE, PASFLG(2),
     *          IFFLAG(4,50), FDVEC(50), JA1, JA2, ISIDEB, NUMCOR,
     *          CHNOFF, TBUFF(2048), IVELTY, IVELDF
      INTEGER   MCLMAX, MCLNF2, MCLF2B, PECNT, ANCNT
      LOGICAL   DOCHK, ISHANN, ISNORM
      REAL      RECTIM, REC4(4), REC4UV(2,28), SRECUV(2,28)
      DOUBLE PRECISION YCLFRE(4), YCLTIM, TESTF(4), CHNSEP, REC8(14),
     *          SREC8(6)
      CHARACTER XCLSOR*8, MCFNAM(20)*8, MODE*2
      COMMON /MODCOM/ YCLFRE, YCLTIM, TESTF, CHNSEP, REC8, SREC8,
     *   RECTIM, REC4, REC4UV, SRECUV,
     *   MCLMAX, MCLNF2, MCLF2B, PECNT, ANCNT,
     *   DOCHK, ISHANN, ISNORM,
     *   MCLUSE, MCLSUB, MCLNF1, MCLSTR, NOFILE,
     *   TAPBFZ, TAPIND, MCFNRC, MCFLIN, RECANT, PASFLG, BCODE,
     *   IFFLAG, FDVEC, RECORD, TAPBUF, TBUFF, JA1, JA2, ISIDEB, NUMCOR,
     *   CHNOFF, IVELTY, IVELDF
      COMMON /MODCHR/ XCLSOR, MCFNAM, MODE
C                                                          End MC.
LOCAL END
      PROGRAM FILLR
C-----------------------------------------------------------------------
C! Reads VLA Modcomp archive tapes (pre 1988)
C# Tape UV VLA
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2000, 2004, 2009, 2012, 2015
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
C   FILLR copies selected data from a VLA Modcomp archive tape to an
C   AIPS uv database.  Multi or single source files can be written.
C
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INTAPE         ITAPE         Tape drive number.
C      NFILES         NFILES        Number of files to skip.
C      DOALL          DOALL         If true write all sources to file.
C      SOURCE         SOURCE        Source name ' '=>all
C      BAND           BAND          Frequency band code (L,C,U,K)
C      QUAL           JQUAL         Source qualifier, -1=>all
C      VLAOBS         VLAOBS        VLA Observing prgm name. eg (AJ99)
C                                   6 characters
C      VLAMODE        VLAMOD        VLA Observing mode '  '=>normal
C      NPOINTS        MAXKVS        Max. number of 1000's vis.
C      BIF            BIF           Start IF
C      EIF            EIF           End IF
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is source name
C      OUTCLASS       CLAOUT        Class of the output uv file.
C                                   Default 'UVDATA'
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      DPARM(10)      DPARM         User specified array.
C                                   1 => Avg. time (seconds)
C                                   2 => max. IF flag allowable (def 3)
C                                   3 => max. sum of IF flags per
C                                        baseline (def 4)
C                                   4 => shadow flagging limit
C                                   5 => # channels requested.
C                                   6 => Subarray number 0=>any.
C                                   7 => reference day offset
C                                   8 => CL table incr. (def=5)
C                                   9 => No. files to read.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'FILLR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'FILLR '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      CALL FLRLRN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Read tape.
      CALL FLRUV (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL FLRHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE FLRLRN (PRGN, JERR)
C-----------------------------------------------------------------------
C   FLRLRN gets input parameters for FILLR and other setup.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                4 => error creating output file.
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in FILLR for more details.
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   JERR
      INTEGER   NPARM, IROUND, IERR
      INCLUDE 'FILLR.INC'
      INCLUDE 'MC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
      PECNT = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
      NSOUR = 0
C                                       Initialize antenna common
      NANT = 0
      GST0 = 0.0D0
      XIAT = 0.0
      XUT1 = 0.0
C                                       Get input parameters.
C                                       Fixed PPM 1996.09.30: was 28
      NPARM = 30
      CALL GTPARM (PRGN, NPARM, RQUICK, XTAPE, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
      IF (JERR.NE.0) GO TO 999
C                                       Crunch input parameters.
      ITAPE = IROUND (XTAPE)
      NFILES = IROUND (XNFILE)
      JQUAL = IROUND (QUAL)
      DOALL = XDOALL.GT.0.0
      MAXKVS = IROUND (XNPOI)
      MAXKVS = MAX (1, MAXKVS)
      SEQOUT = IROUND (XSOUT)
      DISKO = IROUND (XDISO)
      BIF = IROUND (XBIF)
      EIF = IROUND (XEIF)
C                                       Convert characters
      CALL H2CHR (8, 1, XSOURC, SOURC)
      CALL H2CHR (4, 1, XBAND, BAND)
      CALL H2CHR (6, 1, XVLAOB, VLAOBS)
      CALL H2CHR (2, 1, XVLAMO, VLAMOD)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C                                       Set DPARM defaults
C                                       Max. if flag
      IF (DPARM(2).LE.0.0) DPARM(2) = 3.001
C                                       Max. baseline (sum if) flag
      IF (DPARM(3).LE.0.0) DPARM(3) = 4.001
C                                       CL table increment
      IF (DPARM(8).LE.1.0E-10) DPARM(8) = 5.0
C                                       Number of files to read.
      IF (DPARM(9).LT.0.5) DPARM(9) = 1.0E10
      JERR = 0
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLRLRN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
      END
      SUBROUTINE FLRUV (IRET)
C-----------------------------------------------------------------------
C   FLRUV reads data from tape and writes it to disk.  The output file
C   is created after the first valid record is found.  For multisource
C   files, Index, CL and SoUrce files are written on the fly.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER SELCHR*20
      INTEGER   IRET, IPTRO, LUNO, IA1, IA2, NIOUT, INDO, KBIND, LSTSOU,
     *   SUBARR, NUMVIS, XCOUNT, VISLIM, FSTVIS, LSTVIS
      REAL      DUM, LSTIME, TIMEC, TCHANG, TLIM, DTIME, OLDTIM, TIMCLI,
     *   TIMCLS, SELECT(10)
      LOGICAL   T, F, START
      INCLUDE 'FILLR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'MC.INC'
      DATA LUNO /16/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      ICLRNO = -1
      START = T
      NIOUT = 0
      NUMVIS = 0
      XCOUNT = 0
      IPTRO = 1
      VISLIM = 1200 * MAXKVS
      FSTVIS = 1
      TIMCLI = DPARM(8) / 1440.0
      TIMCLS = -1.0E20
C                                       Loop
 100  CONTINUE
 110     NUMVIS = NUMVIS + 1
C                                      Open or get next vis. record.
         CALL FLRDAT (NUMVIS, BUFFER(IPTRO+ILOCU),
     *      BUFFER(IPTRO+ILOCV), BUFFER(IPTRO+ILOCW),
     *      BUFFER(IPTRO+ILOCT), IA1, IA2, BUFFER(IPTRO+NRPARM),
     *      SELECT, SELCHR, IRET)
C                                       Fill in baseline.
         BUFFER(IPTRO+ILOCB) = IA1 * 256 + IA2
C                                       Fill in source ID.
         IF (DOALL) BUFFER(IPTRO+ILOCSU) = CURSOU
C                                       Branch on his return
         IF (IRET) 120, 140, 130
C                                       IRET=-1 => Initialize
C                                       IRET=-2 => end of data.
 120        IF (IRET.EQ.-2) GO TO 200
C                                       Create output file, init etc
C                                       if not done before.
            IF (START) CALL FLRCRE (LUNO, INDO, KBIND, IRET)
            IF (IRET.NE.0) GO TO 999
            START = F
            IPTRO = KBIND
C                                       Setup for INDEX file
            LSTIME = RECTIM
            OLDTIM = RECTIM
            LSTSOU = 1
            TLIM = DPARM(1)
            IF (TLIM.LT.1.) TLIM = 30.0
            TLIM = 6.0 * TLIM / (24.0 * 60.0 * 60.0)
C                                       Note: first call to FLRDAT is
C                                       a dummy to get initialization
C                                       info from tape.
            GO TO 110
C                                       Error (fatal)
 130        WRITE (MSGTXT,1130) IRET
            GO TO 990
C                                       Got datum
 140     CONTINUE
         XCOUNT = XCOUNT + 1
         IF (.NOT.DOALL) GO TO 155
C                                       Check if new scan
C                                       If new source or time gap
C                                       more than 3 * navg int.
         TCHANG = BUFFER(IPTRO+ILOCT) - LSTIME
         IF ((CURSOU.EQ.LSTSOU) .AND. (TCHANG.LE.TLIM)) GO TO 150
            LSTVIS = XCOUNT - 1
            TIMEC = (LSTIME + OLDTIM) * 0.5
            DTIME = LSTIME - OLDTIM
            SUBARR = 1
            CALL TABNDX ('WRIT', NXBUFF, INXRNO, NXKOLS, NXNUMV, TIMEC,
     *         DTIME, LSTSOU, SUBARR, FSTVIS, LSTVIS, 1, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Final CL entry for scan.
            IF (LSTIME.GT.OLDTIM)
     *         CALL FLRCAL (LSTIME, TIMCLI, LSTSOU, DISKO, CCNO,
     *            TIMCLS, IRET)
            IF (IRET.NE.0) GO TO 999
            TIMCLS = -1.0E20
            LSTSOU = CURSOU
            OLDTIM = LSTIME
            FSTVIS = LSTVIS + 1
C                                       Check if time for CL entry
 150     IF (BUFFER(IPTRO+ILOCT).LE.TIMCLS) GO TO 155
            CALL FLRCAL (BUFFER(IPTRO+ILOCT), TIMCLI, CURSOU, DISKO,
     *         CCNO, TIMCLS, IRET)
            IF (IRET.NE.0) GO TO 999
            TIMCLS = BUFFER(IPTRO+ILOCT) + TIMCLI - 1.16E-5
C                                       See if full
 155     IF (XCOUNT.GT.VISLIM) GO TO 200
C                                       Reset last time
         LSTIME = BUFFER(IPTRO+ILOCT)
         IPTRO = IPTRO + LREC
         NIOUT = NIOUT + 1
C                                       Write vis record
         CALL UVDISK ('WRIT', LUNO, INDO, BUFFER, NIOUT, KBIND, IRET)
C                                       Check for end.
         IF (NIOUT.LE.0) GO TO 200
            IF (IRET.EQ.0) GO TO 160
               WRITE (MSGTXT,1150) IRET
               GO TO 990
 160        IPTRO = KBIND
            NIOUT = 0
         GO TO 100
C                                       Final call to FLRDAT.
 200     NUMVIS = -1
         CALL FLRDAT (NUMVIS, DUM, DUM, DUM, DUM, IA1, IA2, BUFFER,
     *      SELECT, SELCHR, IRET)
         IF (IRET.LE.0) GO TO 210
            WRITE (MSGTXT,1130) IRET
            GO TO 990
C                                       Finish write
 210  NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFFER, NIOUT, KBIND, IRET)
      IF (IRET.EQ.0) GO TO 220
         WRITE (MSGTXT,1150) IRET
         GO TO 990
C                                       Compress output file.
 220  NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       Give message about data read.
      WRITE (MSGTXT,1230) XCOUNT, NOFILE
      IF (XCOUNT.GE.VISLIM) WRITE (MSGTXT,1231) XCOUNT
      CALL MSGWRT (6)
C                                       Close file
      CALL ZCLOSE (LUNO, INDO, IRET)
C                                       Parity error count.
      IF (PECNT.LE.0) GO TO 300
         WRITE (MSGTXT,1232) PECNT
         CALL MSGWRT (6)
C                                       Close table files.
 300  IF (.NOT.DOALL) GO TO 999
C                                       Last index record
         LSTVIS = XCOUNT - 1
         TIMEC = (LSTIME + OLDTIM) * 0.5
         DTIME = LSTIME - OLDTIM
         SUBARR = 1
         IF (FSTVIS.GT.XCOUNT) FSTVIS = XCOUNT
         CALL TABNDX ('WRIT', NXBUFF, INXRNO, NXKOLS, NXNUMV, TIMEC,
     *      DTIME, CURSOU, SUBARR, FSTVIS, LSTVIS, 1, IRET)
C                                       Close tables
         CALL TABIO ('CLOS', 0, INXRNO, BUFFER, NXBUFF, IRET)
         CALL TABIO ('CLOS', 0, ISURNO, BUFFER, SUBUFF, IRET)
         CALL TABIO ('CLOS', 0, ICLRNO, BUFFER, CLBUFF, IRET)
         IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1130 FORMAT ('FLRUV: FLRDAT ERROR',I3)
 1150 FORMAT ('FLRUV: ERROR',I3,' WRITING VIS FILE')
 1230 FORMAT ('Read ',I9,' visibilities from',I3,' files')
 1231 FORMAT ('Reached vis. limit of ',I9,', data may be lost')
 1232 FORMAT ('Encountered ',I7,' parity errors on tape')
      END
      SUBROUTINE FLRCRE (LUNO, INDO, KBIND, IRET)
C-----------------------------------------------------------------------
C   FLRCRE fills in the output catalog header record, creates the
C   output file and initializes the I/O.
C    Input:
C     LUNO    I    LUN for I/O
C    Input from D/CMC.INC common:
C     IVELTY      I    Velocity type 1=geocentric, 2=topocentric
C                      3 = barycentric, 4=LSR, 0=none
C     IVELDF      I    Velocity definintion, 1=radio, 2=optical, 0=none.
C    Output:
C     INDO    I    FTAB pointer for I/O
C     KBIND   I    buffer pointer
C     IRET    I    Return error code, 0=>OK, else failed.
C-----------------------------------------------------------------------
      CHARACTER  OLDNAM*12, DEFCLS*6, VELTYP*8, VELDEF*8, OFILE*48,
     *   CHVEL(6)*8, BNDCOD(2)*8
      HOLLERITH  CATH(256)
      INTEGER   LUNO, INDO, KBIND, IRET, JERR, LUN, ISBAND(2), IERR
      INTEGER   BO, VO, LENBU, INOGRP, FQVER, SOUVER, NXVER, SUFQID
      REAL      CATR(256), FINC(2)
      DOUBLE PRECISION    CATD(128), FOFF(2)
      LOGICAL   T
      INCLUDE 'FILLR.INC'
      INCLUDE 'MC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATR, CATD, CATH)
      DATA VO, BO, LENBU /0, 1, 1/
      DATA CHVEL /'GEOCENTR', 'TOPOCENT', 'BARYCENT',
     *   'LSR     ', 'RADIO   ','OPTICAL '/
      DATA ISBAND /1,1/
      DATA BNDCOD /2*' '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Create new header.
      CALL FLRHED (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get uv header info and
C                                       verify header structure.
      CALL UVPGET (JERR)
C                                       Put new values in CATBLK.
C                                       Get naming defaults
      OLDNAM = SOURC // '    '
      DEFCLS = 'UVDATA'
      CALL MAKOUT (OLDNAM, '      ', 0, DEFCLS, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
C                                       Image type ='UV'
      CALL CHR2H (2, 'UV', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = SEQOUT
C                                       Create output file.
      CCNO = 1
      CALL UVCREA (DISKO, CCNO, BUFFER, IRET)
      IF (IRET.EQ.0) GO TO 40
            WRITE (MSGTXT,1000) IRET
            GO TO 990
C                                       Mark in /CFILES/
 40   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = 2
C                                       Get SEQ. no. used.
      SEQOUT = CATBLK(KIIMS)
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, T, T, IRET)
      IF (IRET.LE.0) GO TO 60
         WRITE (MSGTXT,1040) IRET
         GO TO 990
C                                       Init vis file for write
C                                       LREC = length of output rec.
 60   LENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LREC, LENBU, JBUFSZ,
     *   BUFFER, BO, KBIND, IRET)
      IF (IRET.EQ.0) GO TO 80
         WRITE (MSGTXT,1020) IRET
         GO TO 990
 80   CONTINUE
      IRET = 0
C                                       Create/fill Channel table.
C                                       Max of 2 IF's only
      LUN = 28
C                                       Frequency offset
      FOFF(1) = 0.0D0
      FOFF(2) = (YCLFRE(2) - YCLFRE(1)) * 1.0D9
C                                       Channel increment. Assumed
C                                       equal for both IFs
      FINC(1) = CATR(KRCIC+2)
      FINC(2) = FINC(1)
C
      FQVER = 1
      CALL CHNDAT ('WRIT', SUBUFF, DISKO, CCNO, FQVER, CATBLK, LUN,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, 1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create SOURCE and INDEX tables
      IF (.NOT.DOALL) GO TO 999
C                                       SOURCE table
         LUN = 28
         INOGRP = NIF
         VELTYP = '        '
         IF (IVELTY.GT.0) VELTYP = CHVEL(IVELTY)
         VELDEF = '        '
         IF (IVELDF.GT.0) VELDEF = CHVEL(IVELDF+4)
         SOUVER = 1
         SUFQID = -1
         CALL SOUINI ('WRIT', SUBUFF, DISKO, CCNO, SOUVER, CATBLK, LUN,
     *      INOGRP, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Write first SOURCE record
         CALL FLRSOU (IERR)
C                                       INDEX table
         LUN = 29
         NXVER = 1
         CALL NDXINI ('WRIT', NXBUFF, DISKO, CCNO, NXVER, CATBLK, LUN,
     *      INXRNO, NXKOLS, NXNUMV, IRET)
         IF (IRET.NE.0) GO TO 999
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1040 FORMAT ('ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('ERROR',I3,' INIT-FOR-WRITE VIS FILE')
      END
      SUBROUTINE FLRSOU (IRET)
C-----------------------------------------------------------------------
C   Routine to write source info to SU table
C   Puts source info into commons.
C    Input from COMMON
C      ISURNO        I     Next source record in table
C      SUBUFF        I     Buffer  etc for source file.
C      NSOUR         I     Number of sources in source list
C      SULIST(300)   C*8   Names of sources in list
C      IDSOUR(300)   I     Ids of sources on list.
C      XCLSOU        C*8   Source name packed.
C      qual                not yet implemented
C      CALKOD        C*1   Calibrator code
C    Input from MODCOMP record:
C      FREQ          D     Frequency
C      BANDW         D     Bandwidth
C      RA            D     Right Ascension (deg) at mean epoch
C      DEC           D     Declination (deg) at mean epoch
C      epoch               not yet implemented
C      RAAPP         D     Right ascension (deg) apparent
C      DECAPP        D     Declination (deg) apparent
C      LSRVEL(*)     D     LSR velocity
C    Output in common:
C      CURSOU        I     Current source ID number.
C      ISURNO        I     Next source record in table
C      NSOUR         I     Number of sources in source list
C      SULIST(300)   C*8   Names of sources in list
C      IDSOUR(300)   I     Ids of sources on list.
C    Output:
C     IRET           I     Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER  SORNAM*16, CALCOD*4, CHTMP*16
      INTEGER   I, IRET, IDSOU, IQUAL, BWCODE, IPSUB
      LOGICAL   EQUAL, ISLINE
      REAL      FLUX(4,2),  BANTOT(10)
      DOUBLE PRECISION    BANDWW, RAEPO, DECEPO, EPOCH, YTEMP(5), R2D,
     *   LSRVEL(2), PMRA, PMDEC, FRQADJ, FREQO(2), RESTFQ(2), RAOBS,
     *   DECOBS
      INCLUDE 'FILLR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'MC.INC'
      DATA BANTOT /50.0E6, 25.0E6, 12.0E6, 6.0E6, 3.0E6, 1.5E6,
     *   0.8E6, 0.4E6, 0.2E6, 0.1E6/
C-----------------------------------------------------------------------
      IRET = 0
      YTEMP(1) = 1.0D0
      R2D = 180.0D0 / (4.0D0 * ATAN (YTEMP(1)))
C                                       Get source info.
C                                       Source
      SOURCE(1:8) = XCLSOR(1:8)
      SOURC(1:8) = XCLSOR(1:8)
      IPSUB = RECORD(MCLSTR+11) + MCLSTR + 1
C                                       Position
      RA = REC8(1) * R2D
      DEC = REC8(2) * R2D
C                                       Apparent position
      RAAPP = REC8(3) * R2D
      DECAPP = REC8(4) * R2D
C                                       Bandwidth
      BWCODE = RECORD(IPSUB+72) / 256
      BANDW = BANTOT (BWCODE+1)
      IF (NCHAN.GT.1) BANDW = CHNSEP
C                                       Line or continuum?
      CALL H2CHR (2, 1, RECORD(MCLSTR+9), CHTMP)
      ISLINE = CHTMP(1:2).EQ.'LI'
      IF (ISLINE) BANDW = CHNSEP
C                                       See if already have SOURCE
      IF (NSOUR.LE.0) GO TO 100
      DO 10 I = 1,NSOUR
         CURSOU = I
         EQUAL = SULIST(I)(1:8) .EQ. XCLSOR
         IF (EQUAL) GO TO 999
 10      CONTINUE
C                                       New source
C                                       Send user a message.
 100  WRITE (MSGTXT,1021) XCLSOR, OBSDAT, YCLFRE(1)
      CALL MSGWRT (6)
      IDSOU = ISURNO
      CURSOU = IDSOU
      NSOUR = NSOUR + 1
      SOURID(NSOUR) = CURSOU
      SORNAM = XCLSOR // '        '
      SULIST(NSOUR) = XCLSOR
      IQUAL = 0
      FLUX(1,1) = 0.0
      FLUX(2,1) = 0.0
      FLUX(3,1) = 0.0
      FLUX(4,1) = 0.0
      FLUX(1,2) = 0.0
      FLUX(2,2) = 0.0
      FLUX(3,2) = 0.0
      FLUX(4,2) = 0.0
      CALCOD = CALKOD // '   '
      FRQADJ = CHNSEP * CHNOFF
      IF (ISIDEB.EQ.2) FRQADJ = - FRQADJ
      FREQO(1) = (YCLFRE(1) * 1.0D9 + FRQADJ) - FREQ
      FREQO(2) = FREQO(1)
C                                       LSR velocity.
      LSRVEL(1) = REC8(11) * 1.0D3
      LSRVEL(2) = REC8(12) * 1.0D3
C                                       Line rest frequency
      RESTFQ(1) = REC8(13) * 1.0D6
      RESTFQ(2) = REC8(14) * 1.0D6
      BANDWW = BANDW
      RAEPO = RA
      DECEPO = DEC
      EPOCH = 1950.0
      PMRA = 0.0D0
      PMDEC = 0.0D0
      RAOBS = RAEPO
      DECOBS = DECEPO
      CALL TABSOU ('WRIT', SUBUFF, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *   SORNAM, IQUAL, CALCOD, FLUX, FREQO, BANDWW, RAEPO, DECEPO,
     *   EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *   PMDEC, IRET)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1021 FORMAT ('Found ',A8,' observed on ',A8,' at ',F10.5,' GHz')
      END
      SUBROUTINE FLRCAL (TIME, DT, IDSOUR, DISK, CNO, TIMNXT, IRET)
C-----------------------------------------------------------------------
C   Routine to create/fill dummy CL table.
C    Input:
C     TIME      R     Time (days)
C     DT        R     Time interval (days)
C     IDSOUR    I     Source number
C     DISK      I     Disk number of file
C     CNO       I     Catalog slot number of file
C    Input/Output:
C    Output:
C     TIMNXT    R     Time of next record
C     IRET      I     Return code, 0=>OK, else TABCAL or CALINI error.
C    Passed thru common:
C     CATBLK(*) I     Catalog header block
C     CLBUFF(*) I     Buffer for CL table
C     ICLRNO    I     Pointer for next record, if  > 0 on input then
C                     create/open the file.
C     CLKOLS(39)   I   The column pointer array in order.
C     CLNUMV(39)   I   Element count in each column.
C-----------------------------------------------------------------------
      INTEGER   IDSOUR, DISK, CNO, IRET
      REAL      TIME, DT, TIMNXT
      INTEGER   VER, IANT, NUMPOL, SUBA, LUN, NUMANT
      REAL      GMMOD
      DOUBLE PRECISION    TIME8, FRQADJ
      INTEGER   REFA(2,2), NTERM
      REAL      DOPOFF(2), ATMOS, DATMOS, MBDELY(2), CLOCK(2),
     *   DCLOCK(2), DISP(2), DDISP(2), CREAL(2,2), CIMAG(2,2),
     *   CDELAY(2,2), CRATE(2,2), WEIGHT(2,2)
      DOUBLE PRECISION    GEODLY(3)
      INCLUDE 'FILLR.INC'
      INCLUDE 'MC.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA VER,NUMANT /1,28/
      DATA GMMOD /1.0/
      DATA REFA /4*0/
      DATA GEODLY /3*0.0D0/
      DATA DOPOFF, MBDELY, CLOCK, DCLOCK /8*0.0/
      DATA ATMOS, DATMOS, DISP, DDISP /6*0.0/
      DATA CIMAG, CDELAY, CRATE, WEIGHT /16*0.0/
      DATA CREAL /4*1.0/
C-----------------------------------------------------------------------
      NUMPOL = NPOLN / 2
      IF (NUMPOL.LE.0) NUMPOL = 1
      SUBA = 1
C                                       See if time to create
      IF (ICLRNO.GT.0) GO TO 100
         LUN = 26
         NTERM = 1
         CALL CALINI ('WRIT', CLBUFF, DISK, CNO, VER, CATBLK, LUN,
     *      ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NIF, NTERM, GMMOD,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Write record
 100  TIME8 = TIME
C                                       Doppler offset
      FRQADJ = CHNSEP * CHNOFF
      IF (ISIDEB.EQ.2) FRQADJ = - FRQADJ
      DOPOFF(1) = (YCLFRE(1) * 1.0D9 + FRQADJ) - FREQ
      DOPOFF(2) = DOPOFF(1)
      DO 200 IANT = 1,NUMANT
         CALL TABCAL ('WRIT', CLBUFF, ICLRNO, CLKOLS, CLNUMV, NUMPOL,
     *      NIF, TIME8, DT, IDSOUR, IANT, SUBA, 1, 0.0, GEODLY,
     *      DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP, DDISP,
     *      CREAL, CIMAG, CDELAY, CRATE, WEIGHT, REFA, IRET)
         IF (IRET.NE.0) GO TO 999
 200     CONTINUE
C                                       Set time for next
      TIMNXT = TIME + DT
C
 999  RETURN
      END
      SUBROUTINE FLRHIS
C-----------------------------------------------------------------------
C   FLRHIS creates and fills a history file and creates and fills
C   the ANtenna file if any antenna info given.
C-----------------------------------------------------------------------
      CHARACTER   HILINE*72, ATIME*8, ADATE*12, LABEL*8, TELE*8, OBSVR*8
      HOLLERITH CATH(256)
      INTEGER   LUN, I, IERR, TIME(3), DATE(3)
      REAL      CATR(256)
      DOUBLE PRECISION CATD(128)
      LOGICAL   T
      INCLUDE 'FILLR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'MC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATH, CATR, CATD)
      DATA LUN /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Create/open hist. file.
      CALL HICREA (LUN, DISKO, CCNO, CATBLK, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
C                                       Get current date/time.
 10   CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
C                                       Write first record.
      WRITE (HILINE,1010) TSKNAM, NLUSER, ADATE, ATIME
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       New history
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN,
     *   BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       INTAPE, NFILES
      WRITE (HILINE,2001) TSKNAM, ITAPE, NFILES
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       BAND,QUAL
      WRITE (HILINE,2002) TSKNAM, BAND, JQUAL
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       VLAMODE
      WRITE (HILINE,2008) TSKNAM, VLAMOD
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       NPOINTS
      WRITE (HILINE,2003) TSKNAM, MAXKVS
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       SOURCE
      WRITE (HILINE,2004) TSKNAM, SOURCE
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       User options
C                                       Integration time
      WRITE (HILINE,2009) TSKNAM, DPARM(1)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Max. IF flags
      WRITE (HILINE,2010) TSKNAM, DPARM(2), DPARM(3)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Shadow flagging
      WRITE (HILINE,2011) TSKNAM, DPARM(4)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Subarray
      WRITE (HILINE,2013) TSKNAM, DPARM(6)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Day offset
      WRITE (HILINE,2014) TSKNAM, DPARM(7)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       CL table increment
      WRITE (HILINE,2015) TSKNAM, DPARM(8)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Hanning
      IF (ISHANN) THEN
         WRITE (HILINE,2016) TSKNAM
         CALL HIADD (LUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                       Autocorrelation normalization
      IF (ISNORM) THEN
         WRITE (HILINE,2017) TSKNAM
         CALL HIADD (LUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                       Number of visibilities, files
      WRITE (HILINE,2005) TSKNAM, CATBLK(KIGCN), NOFILE
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Telescope, observer name.
      CALL H2CHR (8, 1, CATH(KHTEL), TELE)
      CALL H2CHR (8, 1, CATH(KHOBS), OBSVR)
      WRITE (HILINE,2006) TSKNAM, TELE, OBSVR
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       AIPS release
      WRITE (HILINE,2007) TSKNAM, RLSNAM
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      Add any history from common
      IF (NUMHIS.LE.0) GO TO 20
         WRITE (LABEL,1015) TSKNAM
         DO 15 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 20
 15         CONTINUE
C                                       Close HI file
 20   CALL HICLOS (LUN, T, BUFFER, IERR)
C                                       Write ANtenna file.
      CALL ANTFLR
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISKO, CCNO, CATBLK, 'REST', BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLRHIS: ERROR',I3,' CREATE/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ IMAGE CREATED BY USER',I5,' AT ',A12,2X,A8)
 1015 FORMAT (A6,' /')
 2001 FORMAT (A6,' INTAPE =',I3,', NFILES =',I5)
 2002 FORMAT (A6,' BAND = ''',A1,''', QUAL = ',I5)
 2003 FORMAT (A6,' NPOINTS = ',I6,' / MAX. 1000S VIS.')
 2004 FORMAT (A6,' SOURCE = ',1H',A8,1H')
 2005 FORMAT (A6,' / NUMBER OF VISIBILITIES =',I9,' FROM',I3,' FILES')
 2006 FORMAT (A6,' / TELESCOPE = ',A8,' PROGRAM = ',A8)
 2007 FORMAT (A6,' RELEASE = ''',A7,' ''')
 2008 FORMAT (A6,' VLAMODE =',1H',A2,1H')
 2009 FORMAT (A6,' DPARM(1) =', F7.3,' / INTEGRATION (SEC)')
 2010 FORMAT (A6,' DPARM(2) =', F5.0,', DPARM(3) =',F5.0,
     *   ' / MAX. IF, COR, FLAGS')
 2011 FORMAT (A6,' DPARM(4) =', F5.0,
     *   ' / .LT. 0 => NO SHADOW FLAG')
 2013 FORMAT (A6,' DPARM(6) =', F5.0, ' / SUBARRAY')
 2014 FORMAT (A6,' DPARM(7) =', F5.0, ' / REF. DAY OFFSET')
 2015 FORMAT (A6,' DPARM(8) =', F7.2, ' / CL TABLE TIME INCREMENT')
 2016 FORMAT (A6,' / DATA HAS BEEN HANNING SMOOTHED')
 2017 FORMAT (A6,' / DATA HAS BEEN NORMALIZED BY AUTOCORRELATIONS')
      END
      SUBROUTINE ANTFLR
C-----------------------------------------------------------------------
C   ANTFLR creates and fills the antenna file from info in common.
C-----------------------------------------------------------------------
      HOLLERITH CATH(256)
      INTEGER   IERR, VER, LUN, I, NAT
      REAL      CATR(256)
      DOUBLE PRECISION JD, GASTM
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'FILLR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATR, CATH)
      DATA LUN /27/
C-----------------------------------------------------------------------
C                                       GST0 = Greenwich sidereal time
C                                         (in degrees) at IAT midnight
C                                         on the reference date
C                                         (OBSDAT)
C                                       XIAT = The IAT-UTC time
C                                          correction for the reference
C                                          date. ( seconds of time)
C                                       XUT1 = The UT1-UTC time
C                                          correction for ref. date.
C                                       NANT = number of antennas.
C                                       ANTLOC(i,n) is the position of
C                                          antenna n in meters from
C                                          the array reference center.
C                                          i=1=X, i=2=Y, i=3=Z.
C                                          (see Going AIPS)
C                                       ANTNAM(n) is the antenna
C                                          name, up to 8 char
C                                       Setup for AN table initization
         POLRXY(1) = POLARX
         POLRXY(2) = POLARY
         UT1UTC = XUT1
         DATUTC = XIAT
         ANAME = 'VLA     '
         ARRAYC(1) = -1601162.D0
         ARRAYC(2) =  -5042003.D0
         ARRAYC(3) =  3554915.D0
C                                       Get GST0 and Earth rotation rate
         CALL H2CHR (8, 1, CATH(KHDOB), RDATE)
         CALL JULDAY (RDATE, JD)
         CALL GSTROT (JD, GSTIA0, GASTM, DEGPDY)
         SAFREQ = FREQ
         VER = 1
         IF (NANT.GT.1) NANT = 28
         NAT = MAX (NANT, 1)
         ANFQID = -1
         TIMSYS = 'IAT'
         NUMORB = 0
         NOPCAL = 2
         ANTNIF = NIF
         XYZHAN = 'RIGHT'
C                                       Create/init file
         CALL ANTINI ('WRIT', BUFFER, DISKO, CCNO, VER, CATBLK, LUN,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       init basic AN record
         ANNAME = '        '
         STAXOF = 0.0
         STAXYZ(1) = 0.0D0
         STAXYZ(2) = 0.0D0
         STAXYZ(3) = 0.0D0
         ORBPRM(1) = 0.0D0
         NOSTA = 0
         MNTSTA = 0
         POLAA = 0.0
         POLAB = 0.0
         DIAMAN = 25
C                                       Ellipticity, orientation.
C                                       RCP
         POLCA(1) = 0.785398164
         POLCA(2) = 0.0
         POLCA(3) = 0.785398164
         POLCA(4) = 0.0
C                                       LCP
         POLCB(1) = -0.785398164
         POLCB(2) = 0.0
         POLCB(3) = -0.785398164
         POLCB(4) = 0.0
         POLTYA = 'R '
         POLTYB = 'L '
         IF (NANT.LE.0) GO TO 30
C                                       AN records
         DO 20 I = 1,NANT
            STAXYZ(1) = ANTLOC(1,I)
            STAXYZ(2) = ANTLOC(2,I)
            STAXYZ(3) = ANTLOC(3,I)
            NOSTA = I
            ANNAME = ANTNAM(I)
            IANRNO = I
            CALL TABAN ('WRIT', BUFFER, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
            IF (IERR.NE.0) GO TO 990
 20         CONTINUE
C                                       Fill in header and close
 30      CALL TABAN ('CLOS', BUFFER, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) GO TO 990
         GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1020) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('ERROR ',I3,' OCCURED WRITING ANTENNA TABLE')
      END
      SUBROUTINE FLRHED (IRET)
C-----------------------------------------------------------------------
C   FLRHED is a routine in which the catalog header is constructed.
C    Input:
C     CATBLK(256)    I     Output catalog header, also CATR, CATD
C                          The OUTNAME, OUTCLASS, OUTSEQ are entered
C                          elsewhere.
C    Input from MODCOMP record:
C     RA             D     Filled in CATD if DOALL=false
C     DEC            D     Filled in CATD if DOALL=false
C    Output:
C     CATBLK(256)    I     Modified output catalog header.
C     IRET           I     Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER RTYPES(7)*8, TYPES(7)*8, UNITS*8, TELE*8, INSTR*8
      HOLLERITH CATH(256)
      INTEGER   I, NAXIS, NRAN, NDIM(7), INDEX, IRET
      REAL      CATR(256), CRPIX(7), CRINC(7), EPOCH
      DOUBLE PRECISION    CATD(128), CRVAL(7)
      INCLUDE 'FILLR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'MC.INC'
      EQUIVALENCE  (CATBLK, CATH, CATR, CATD)
C                                         No. random parameters.
      DATA NRAN /6/
C                                         Rand. parm. names.
      DATA RTYPES /'UU-L    ','VV-L    ','WW-L    ',
     *   'TIME1   ','BASELINE','SOURCE  ','        '/
C                                       Uniform axes.
C                                         No. axes.
      DATA NAXIS /6/
C                                         Axes names.
      DATA TYPES /'COMPLEX ','STOKES  ','FREQ    ',
     *   'IF      ','RA      ','DEC     ','        '/
C                                         Axis dimensions
      DATA NDIM /3,4,1,1,1,1,1/
C                                         Reference values
      DATA CRVAL /1.0D0, -1.0D0, 0.0D0, 1.0D0, 3*0.0D0/
C                                         Reference pixel.
      DATA CRPIX /7*1.0/
C                                         Coordinate increment.
      DATA CRINC /1.0, -1.0, 5*1.0/
C                                       Epoch of position.
      DATA EPOCH /1950.0/
C                                       Units
      DATA UNITS /'JY      '/
C                                       Telescope/instrument
      DATA TELE, INSTR /'VLA     ','VLA     '/
C-----------------------------------------------------------------------
C                                       Zero fill CATBLK
      CALL FILL (256, 0, CATBLK)
C                                       Fill axis arrays.
C                                       Random axis names
      DO 10 I = 1,KIPTPN
         INDEX = KHPTP + (I-1) * 2
         CALL CHR2H (8, RTYPES(I), 1, CATH(INDEX))
 10      CONTINUE
C                                       Uniform axes
      DO 30 I = 1,KICTPN
C                                       Init dimension
         CATBLK(KINAX+I-1) = NDIM(I)
C                                       Init. increment.
         CATR(KRCIC+I-1) = CRINC(I)
C                                       Init. rotation.
         CATR(KRCRT+I-1) = 0.0
C                                       Init. ref pixel.
         CATR(KRCRP+I-1) = CRPIX(I)
C                                       Init. ref value.
         CATD(KDCRV+I-1) = CRVAL(I)
C                                       Fill axis type from
C                                       TYPES
         INDEX = KHCTP + (I-1) * 2
         CALL CHR2H (8, TYPES(I), 1, CATH(INDEX))
 30      CONTINUE
C                                       Fill in values.
C                                       Fill other character strings.
C                                       Set number of axes.
      CATBLK(KIDIM) = NAXIS
      CATBLK(KIPCN) = NRAN
C                                       User ID
      CATBLK(KIIMU) = NLUSER
C                                       Miscellaneous items.
C                                       Epoch.
      CATR(KREPO) = EPOCH
C                                       Convolving beam
      CATR(KRBMJ) = 0.0
      CATR(KRBMN) = 0.0
      CATR(KRBPA) = 0.0
      CATBLK(KINIT) = 0
C                                       Max. min.
      CATR(KRDMX) = 0.0
      CATR(KRDMN) = 0.0
C                                       Shift
      CATR(KRXSH) = 0.0
      CATR(KRYSH) = 0.0
C                                       "Old" (observed) position.
      CATD(KDORA) = 0.0D0
      CATD(KDODE) = 0.0D0
C                                       Rest Frequency
      CATD(KDRST) = 0.0D0
C                                       Alternate ref. value & pixel
      CATD(KDARV) = 0.0D0
      CATR(KRARP) = 0.0
      CATBLK(KIALT) = 0
C                                       Sort order ('TB'=>time baseline)
      CALL CHR2H (2, 'TB', 1, CATH(KITYP))
C                                       No magic value blanking.
      CATR(KRBLK) = 0.0
C                                       Units
      CALL CHR2H (8, UNITS, 1, CATH(KHBUN))
C-----------------------------------------------------------------------
C                                       Enter values for data:
C                                       RA = Right ascension (1950)
C                                          in degrees.
C                                       DEC = Declination in degrees.
C                                       FREQ = frequency of obs in Hz.
C                                       BANDW = bandwidth or channel
C                                           separation.
C                                       NCHAN = Number of freq chan.
C                                       NPOLN = number of polarization
C                                            correlators.
C                                       OBSDAT = Reference date of time
C                                          tags for data as "dd/mm/yy"
C                                       SOURCE = source name (8 char)
C                                       OBSR = Observers name (8 char)
C                                       Insert values in header.
C                                       Number of vis.
      CATBLK(KIGCN) = 1.2D3 * MAXKVS
C                                       Position.
      CATD(KDCRV+4) = RA
      CATD(KDCRV+5) = DEC
C                                       For multi source zero
      IF (DOALL) CATD(KDCRV+4) = 0.0D0
      IF (DOALL) CATD(KDCRV+5) = 0.0D0
C                                       Frequency
      CATD(KDCRV+2) = FREQ
C                                       Bandwidth.
      CATR(KRCIC+2) = BANDW
C                                       Correction for lower sideband
      IF ((ISIDEB.EQ.2) .AND. (NCHAN.GT.1)) CATR(KRCIC+2) = -BANDW
C                                       Number of polarizations.
      CATBLK(KINAX+1) = NPOLN
C                                       Number of frequencies.
      CATBLK(KINAX+2) = NCHAN
C                                       Channel offset
      CATR(KRCRP+2) = 1.0 - CHNOFF
C                                       Number of IFs
      CATBLK(KINAX+3) = NIF
C                                       Observing date.
      CALL CHR2H (8, OBSDAT, 1, CATH(KHDOB))
C                                       Object.
      CALL CHR2H (8, SOURC, 1, CATH(KHOBJ))
      IF (DOALL) CALL CHR2H (8, 'MULTI   ', 1, CATH(KHOBJ))
C                                       Telescope.
      CALL CHR2H (8, TELE, 1, CATH(KHTEL))
C                                       Receiver
      CALL CHR2H (8, INSTR, 1, CATH(KHINS))
C                                       Observer's name.
      CALL CHR2H (8, OBSR, 1, CATH(KHOBS))
C                                       Finished.
      IRET = 0
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE FLRDAT (NUMVIS, U, V, W, TIME, IA1, IA2, VIS,
     *   SELECT, SELCHR, IRET)
C-----------------------------------------------------------------------
C  FLRDAT reads MODCOMP data and returns one visibility at a time.
C  Note: on the first call (NUMVIS=1) the necessary information to
C  define the output file should be filled into the appropriate commons
C  and a return code of -1 returned.  This will cause the output file to
C  be created and I/O initialized but no data will be written as a
C  result of this call.  When all data is exhausted an IRET = -2
C  on return indicates this.  No data will be written to the output file
C  unless IRET=0.
C  Inputs:
C  NUMVIS     I    Visibility number, -1=> final call, no data
C                  passed but allows any operations to be completed.
C                  If NUMVIS = 1 then the information about the file
C                  should be determined and a IRET=-1 but no data
C                  returned.
C  Inputs from COMMON
C  NRPARM     I    # random parameters.
C  CATBLK(256)I    Catalog header record. See Going AIPS for details.
C
C  Output:
C  U          R    U in wavelengths at the reference frequency.
C  V          R    V in wavelengths
C  W          R    W in wavelengths
C  TIME       R    Time in days since the midnight at the start of
C                  the reference date.
C  IA1        I    Antenna number of the first antenna.
C  IA2        I    Antenna number of the second antenna.
C                  NOTE: IA2 MUST be greater that IA1
C  VIS(3,*)   R    Visibilities.  The first dimension is the COMPLEX
C                  axis in the order Real part, Imaginary part, weight.
C                  The order of the following visibilities is defined
C                  by variables in COMMOM /UVHDR/ (originally
C                  specified in FLDHDR).  The order number for Stokes
C                  parameters is JLOCS and the order number for
C                  frequency is given by JLOCF.  The lower order number
C                  increases faster in the array.
C  SELECT(10) R    Data selection array; this array is maintained by
C                  FLRDAT.
C  SELCHR*(*) C    Character portion of Data selection array
C  IRET       I    Return code  -2 => End of data.
C                               -1 => Initialize output, should only be
C                                     returned on NUMVIS=1.
C                                0 => valid data
C                               >0 => error, terminate.
C
C  Output in COMMON
C  NUMHIS     I    # history entries (max. 10)
C  HISCRD(NUMHIS) C   History records
C  CATBLK     I    Catalog header block
C
C-----------------------------------------------------------------------
      CHARACTER SELCHR*20, XLSOU*8, CHTMP*8
      INTEGER    MCHAN, MPOLN
      INTEGER    IA1, IA2, IRET, IFIL, IERR, JNCMS, MMAXIF,
     *   IP, IF, INCMS, NCHIF, IPANT, IPSUB, IA, IROUND, BWCODE
      INTEGER   JNDEX, KNDEX, INDEX, NSHAD
      LOGICAL   F, ISLINE, GOOD, EQUAL
      INTEGER   NUMVIS
      REAL   CATR(256), U, V, W, TIME, VIS(3,10), SCLREC,
     *   RECU(50), RECV(50), RECW(50), BANTOT(10), SELECT(10),
     *   SHADOW, SFREQ
      DOUBLE PRECISION CATD(128), XDAY, XRDAY, YTEMP(4), XDAT
      SAVE NSHAD, SHADOW, INCMS
      INCLUDE 'FILLR.INC'
      INCLUDE 'MC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA F /.FALSE./
      DATA BANTOT /50.0E6, 25.0E6, 12.5E6, 6.25E6, 3.125E6, 1.5625E6,
     *   0.78125E6, 0.1953125E6, 0.1953125E6, 0.1953125E6/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check NUMVIS
      IF (NUMVIS.GT.2) GO TO 100
C                                       Special case for second call.
      IF (NUMVIS.EQ.2) GO TO 130
      IF (NUMVIS.LT.0) GO TO 900
C                                       Initialize shadowed data counter
      NSHAD = 0
C                                       Set selection criteria.
      PASFLG(1) = IROUND (DPARM(2))
      PASFLG(2) = IROUND (DPARM(3))
C                                       Source name.
      SELCHR(1:8) = SOURC
C                                       Source qualifier
      SELECT(3) = QUAL
C                                       Observing band
      SELCHR(9:9) = BAND(1:1)
C                                       Observing program name
      SELCHR(10:15) = VLAOBS
C                                       Integration time
C                                       Subtract 2 seconds.
      SELECT(7) = ((DPARM(1)-2.0) / 86400.0) *
     *   2.0 * 3.14159265358979323846D0
C                                       Observing mode
      SELCHR(16:17) = VLAMOD
C                                       Subarray number
      SELECT(9) = DPARM(6)
C                                       Selection by no. channels
      SELECT(10) = DPARM(5)
C                                       Set shadowing limit in metres
C                                       and tell user what happened
      IF (DPARM(4).GE.0.0) GO TO 10
         SHADOW = 0.0
         GO TO 30
 10   IF (DPARM(4).GE.25.0) GO TO 20
         SHADOW = 25.0
         GO TO 30
 20   SHADOW = DPARM(4)
 30   WRITE (MSGTXT,1000) SHADOW
      CALL MSGWRT (6)
C                                       Initialize /MODCOM/
C                                       Size of RECORD (words)
      MCLMAX = 100000
C                                       Offset in tape record
      MCLSTR = 4
C                                       Number of FORMAT 1 records
C                                       stacked
      MCLNF1 = 0
C                                       Number of FORMAT 2 card images
      MCLNF2 = 0
C                                       Number extra FORMAT 2 bytes.
      MCLF2B = 0
C                                       Number of files read.
      NOFILE = 0
C                                       Initialize, open, position tape.
C                                       Open tape
C                                       Initialize FDVEC
      CALL FILL (50, 0, FDVEC)
      FDVEC(1) = 129 - ITAPE
      FDVEC(2) = 4096
      FDVEC(3) = 4096 * (NBITWD/8)
      FDVEC(5) = ITAPE
      CALL TAPIO ('OPRD', FDVEC, TAPBUF, TAPIND, IRET)
      IF (IRET.LE.1) GO TO 40
         WRITE (MSGTXT,1010) IRET
         GO TO 990
C                                       Skip to correct file.
 40   IF (NFILES.GT.0)
     *   CALL ZTAPE ('ADVF', FDVEC(1), FDVEC(40), NFILES, IRET)
      IFIL = 1 - NFILES
      IF (NFILES.LE.0)
     *   CALL ZTAPE ('BAKF', FDVEC(1), FDVEC(40), IFIL, IRET)
      IF (IRET.EQ.0) GO TO 50
         WRITE (MSGTXT,1020) IRET
         GO TO 990
C                                       Find first data record.
 50   CALL MCREC ('INIT', SELECT, SELCHR, IERR)
      IRET = IERR
      IF (IERR.EQ.4) IRET = -2
C                                       Possible message if no data
C                                       found.
      WRITE (MSGTXT,1030)
      IF (IERR.NE.0) GO TO 990
      JA1 = 1
      JA2 = 2
C                                       Get source info.
C                                       Source
      SOURCE = XCLSOR
      SOURC = XCLSOR
      IPSUB = RECORD(MCLSTR+11) + MCLSTR + 1
C                                       Position
      RA = SREC8(3)
      DEC = SREC8(4)
C                                       Apparent position
      RAAPP = SREC8(5)
      DECAPP = SREC8(6)
C                                       Bandwidth
      BWCODE = RECORD(IPSUB+72) / 256
      BANDW = BANTOT (BWCODE+1)
C                                       Line or continuum?
      CALL H2CHR (2, 1, RECORD(MCLSTR+9), CHTMP)
      ISLINE = CHTMP(1:2) .EQ. 'LI'
      IF (ISLINE) BANDW = CHNSEP
C                                       Fill in true selection criteria
C                                       Band
      BAND = SELCHR(9:9) // '   '
C                                       Source
      IF (.NOT.DOALL) SELCHR(1:8) = XCLSOR
C                                       Observing program
      CALL H2CHR (6, 1, RECORD(IPSUB+8), VLAOBS)
      SELCHR(10:15) = VLAOBS
C                                       Subarray
      SELECT(9) = RECORD(IPSUB)
      DPARM(6) = SELECT(9)
C
C                                       Number of antennas in subarray
      NANT = RECORD(MCLSTR+14)
C                                       Increment
      INCMS = 3
      IF (ISLINE) INCMS = 2
C                                       initialize commons
C                                       Reference MJAD
C                                       Get MJAD and Obs. date.
      CALL ZR8P4 ('4IB8', RECORD(MCLSTR+5), XRDAY)
C                                       Reference day offset.
      XRDAY = XRDAY + IROUND (DPARM(7))
      XDAT = XRDAY + 2400000.5D0
C                                       Convert Julian date to calender
      CALL GREG (XDAT, OBSDAT)
C                                       NPOLN
      NPOLN = 4
C                                       One polarization for line.
      IF (ISLINE) NPOLN = 1
C                                       NCHAN, first continuum.
      NCHAN = 1
C                                       IF selection, assumed NIF <= 2
      IF (RECORD(MCLSTR+20).GT.0) THEN
C                                       2 IF's available
         MMAXIF = 2
         IF (BIF.LT.0 .OR. BIF.GT.2) BIF = 1
         IF (EIF.LT.0 .OR. EIF.GT.2) EIF = 2
C
         IF (BIF.EQ.0) THEN
            BIF = 1
            EIF = 2
         ELSE IF (BIF.EQ.1) THEN
            IF (EIF.EQ.0) EIF = 2
         ELSE
            EIF = 2
         END IF
         NIF = EIF - BIF + 1
      ELSE
C                                       1 IF available
         MMAXIF = 1
         BIF = 1
         EIF = 1
         NIF = 1
      END IF
      WRITE (MSGTXT,1040) BIF, EIF
      CALL MSGWRT (6)
C                                       Frequency
      FREQ = 1.0D9 * YCLFRE(BIF)
C                                       Time
      CALL ZR8P4 ('4IB8', RECORD(MCLSTR+5), XDAY)
      YTEMP(1) = SREC8(1) / (2.0D0 * 3.14159265358979323846D0)
      RECTIM = YTEMP(1) + (XDAY - XRDAY)
C                                       Spectral line
      IF (ISLINE) THEN
         NCHAN = NUMCOR
         BIF = 1
         EIF = NCHAN
      END IF
C                                       Convert SHADOW to lambdas**2
C                                       Note that shadowing done with
C                                       more conservative IF freq.
      SFREQ = MAX (YCLFRE(1), YCLFRE(2))
      SHADOW = (SHADOW * SFREQ / 2.997925E-1)**2
C                                       User name.
      OBSR = '        '
      CALL H2CHR (6, 1, RECORD(IPSUB+8), OBSR(1:6))
C                                       Send user a message.
      WRITE (MSGTXT,1050) VLAOBS, RECORD(MCLSTR+4)
      CALL MSGWRT (6)
C                                       Done
      IRET = -1
      GO TO 999
C                                       Next record
 100  CONTINUE
C                                       Determine antennas
      JA2 = JA2 + 1
      IF (JA2.GT.NANT) JA1 = JA1 + 1
      IF (JA2.GT.NANT) JA2 = JA1 + 1
C                                       Check if time for a read.
      IF ((JA1.LE.(NANT+1)) .AND. (JA2.LE.NANT)) GO TO 200
C                                       Look for next data
C                                       Save old source name
      XLSOU = XCLSOR
      CALL MCREC ('READ', SELECT, SELCHR, IERR)
C                                       Check for end of data.
      IRET = IERR
      IF (IERR.EQ.4) IRET = -2
C                                       Quit if too many parity errors
C                                       But keep what you have
      IF (IERR.EQ.3) IRET = -2
      IF (IERR.NE.0) GO TO 999
C                                       Check if multi source
      IF (.NOT.DOALL) GO TO 130
C                                       Check if new source
         EQUAL = XLSOU(1:8) .EQ. XCLSOR(1:8)
C                                       If different source call FLRSOU
         IF (.NOT.EQUAL) CALL FLRSOU (IRET)
         IF (IRET.NE.0) GO TO 999
 130  JA1 = 1
      JA2 = 2
C                                       Get number of antennas
      NANT = RECORD(MCLSTR+14)
C                                       Get parameters for MC record.
      IPSUB = RECORD(MCLSTR+11) + MCLSTR + 1
C                                       Time
      CALL ZR8P4 ('4IB8', RECORD(MCLSTR+5), XDAY)
      YTEMP(1) = SREC8(1) / (2.0D0 * 3.14159265358979323846D0)
      RECTIM = YTEMP(1) + (XDAY - XRDAY)
C                                       Visibility scaling
      SCLREC = 2.0D0 ** (RECORD(IPSUB+15) - 15)
C                                       Antenna U,V,W
      DO 150 IA = 1,NANT
         IPANT = RECORD(MCLSTR+13) + (IA-1) * RECORD(MCLSTR+12) +
     *      MCLSTR + 1
C                                       Antenna number
         RECANT(IA) = RECORD(IPANT) / 256
C                                       u,v,w - convert to wavelength
         RECU(IA) = SRECUV(1,IA) * YCLFRE(1)
         RECV(IA) = SRECUV(2,IA) * YCLFRE(1)
         RECW(IA) = RECORD(IPANT+3) * YCLFRE(1) * 16.0
 150     CONTINUE
      ANCNT = -1
C                                       Get visibility record.
 200  CONTINUE
      IA1 = RECANT(JA1)
      IA2 = RECANT(JA2)
      ANCNT = ANCNT + 1
      U = RECU(JA1) - RECU(JA2)
      V = RECV(JA1) - RECV(JA2)
      W = RECW(JA1) - RECW(JA2)
      TIME = RECTIM
      INDEX = 0
      IF (.NOT.ISLINE) JNDEX = RECORD(MCLSTR+17) +
     *   ANCNT * NPOLN * INCMS + MCLSTR - 2
      IF (ISLINE) JNDEX = RECORD(MCLSTR+17) + ANCNT * INCMS * (NCHAN+1)
     *   + MCLSTR + 1
      KNDEX = JNDEX + 2
      IF (ISLINE) KNDEX = JNDEX
      JNCMS = INCMS
C                                       One count per vis for line.
      IF (ISLINE) JNCMS = 0
C                                       Get number of channels/IFs
      NCHIF = MAX (NCHAN, MMAXIF)
      GOOD = F
      DO 240 IF = 1,NCHIF
         IF ( (IF.GE.BIF) .AND. (IF.LE.EIF) ) THEN
            DO 220 IP = 1,NPOLN
               INDEX = INDEX + 1
               JNDEX = JNDEX + INCMS
               KNDEX = KNDEX + JNCMS
               VIS(1,INDEX) = RECORD(JNDEX) * SCLREC
               VIS(2,INDEX) = RECORD(JNDEX+1) * SCLREC
               VIS(3,INDEX) = RECORD(KNDEX)
C                                       Look for good data, only
C                                       data that has all polns. at
C                                       all channels or IF's flagged
C                                       is chucked out
               GOOD = GOOD .OR. (VIS(3,INDEX).GT.0.0)
 220        CONTINUE
         END IF
C                                       Update index for 2'nd IF pair
         IF (.NOT.ISLINE) THEN
            JNDEX = RECORD(MCLSTR+19) + ANCNT * INCMS * NPOLN
     *              + MCLSTR - 2
            KNDEX = JNDEX + 2
         END IF
 240  CONTINUE
C                                       Drop shadowed visibilities.
C                                       If one channel or IF is
C                                       shadowed, all are dropped
      IF ((U**2 + V**2) .LE. SHADOW) THEN
         NSHAD = NSHAD + 1
         GO TO 100
      END IF
C                                       Check if valid data
      IF (.NOT.GOOD) GO TO 100
C                                       Flip baseline if necessary.
      MCHAN = NCHAN * NIF
      MPOLN = NPOLN
      IF (IA1.GT.IA2) CALL MCFLIP (IA1, IA2, MCHAN, MPOLN, U, V, W, VIS)
      IRET = 0
      GO TO 999
C                                       Close MODCOMP file.
 900  CALL TAPIO ('CLOS', FDVEC, TAPBUF, TAPIND, IRET)
      IRET = 0
C                                       Tell no. shadowed vis.
      WRITE (MSGTXT,1060) NSHAD, SQRT(SHADOW)/1000.0
      CALL MSGWRT (6)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Shadow flag limit = ', 1PE10.3, ' metres')
 1010 FORMAT ('ERROR',I7,' OPENING MODCOMP TAPE')
 1020 FORMAT ('ERROR',I7,' POSITIONING TAPE')
 1030 FORMAT ('NO DATA FOUND MEETING SELECTION CRITERIA')
 1040 FORMAT ('Selected BIF = ', I1, '  EIF = ', I1)
 1050 FORMAT ('Program = ',A6,' Tape revision number =',I5)
 1060 FORMAT ('Dropped ' , I5, ' shadowed vis at limit ', 1PE11.4,
     *        ' k-lambda')
      END
      SUBROUTINE MCREC (OPCODE, SELECT, SELCHR, IRET)
C-----------------------------------------------------------------------
C   MCREC reads records from a MODCOMP file and converts it into local
C   data.  FORMAT 2 (system file) records are digested internally and
C   antenna information is placed in common /ANTS/.
C   FORMAT 1 (data) records will be selected by data selection criteria
C   obtained from common and desired data records will be returned.
C   Various data validity checks will be made.
C    Inputs:
C     OPCODE    C    Opcode 'READ' or 'INIT'.  INIT will set up I/O and
C                    begin processing data, returning the first selected
C                    FORMAT 1 record.
C     SELECT(10)R    Selection parameters.
C                       3 => source qualifier, -1 => all
C                       7 => Averaging time (rad.)
C                       9 => subarray, 0 => any
C     SELCHR*20 C    Character selection info:
C                     (1:8) = source name, (blank means any)
C                     (9:9) = Frequency band code (blank means any)
C                     (10:15) = observing program name "
C                     (16:17) = VLA observing mode     "
C    Output in common:
C     RECORD(*) I    Data record (integer and character).
C     REC8(*)   D    Double precision data:
C                    1 = RA (1950) radians
C                    2 = Dec (1950) radians
C                    3 = RA (apparent) radians
C                    4 = Dec (apparent) radians
C                    5 = signed sum of LOs for IF A
C                    6 = signed sum of LOs for IF B
C                    7 = signed sum of LOs for IF C
C                    8 = signed sum of LOs for IF D
C                    9 = End IAT (radians) of last Modcomp rec.
C                    10 = End LST (radians) of last Modcomp rec.
C                    11 = Velocity of source IF A km/s
C                    12 = Velocity of source IF B km/s
C                    13 = Rest frequency of line IF A MHz
C                    14 = Rest frequency of line IF B MHz
C     SREC8(*)  D    Summed/averaged double precision values:
C                    1 = end IAT (radians)
C                    2 = end LST (radians)
C    REC4(*)    R    Single precision values:
C                    1 = Stop LST, radians
C                    2 = Start LST, radians.
C                    3 = refractivity (n-1)
C                    4 = zenith atmos. phase delay (nsec)
C    REC4UV(2,*)R    Record u,v for each antenna.
C    SRECUV(2,*)R    Summed/averaged u,v for each antenna.
C    Output:
C     IRET      I    Return error code, 0=> valid data, 4=>end of file.
C-----------------------------------------------------------------------
      INTEGER   IRET, TYPE
      CHARACTER SELCHR*20, OPCODE*4, OP*4
      REAL      CATR(256), SELECT(10)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'FILLR.INC'
      INCLUDE 'MC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
C-----------------------------------------------------------------------
      OP = OPCODE
C                                       Read record
 20   CALL MCRECO (OP, SELECT, SELCHR, TYPE, IRET)
      IF (IRET.NE.4) GO TO 30
C                                       See if more files wanted.
         IF ((NOFILE+0.5).GE.DPARM(9)) GO TO 30
C                                       EOF, check next file.
         CALL MCRECO ('INIT', SELECT, SELCHR, TYPE, IRET)
         IF (IRET.EQ.0) GO TO 20
C                                       Error - call it quits
         IRET = 4
         GO TO 999
C                                       Check for end of data.
 30   IF (IRET.NE.0) GO TO 999
C                                       Deal with FORMAT 2 records.
      IF (TYPE.EQ.1) GO TO 999
C                                       Get antenna info
         CALL MCANT
         OP = 'READ'
         MCLNF2 = 0
         MCLF2B = 0
C                                       Look for first data record.
         GO TO 20
C
 999  RETURN
      END
      SUBROUTINE MCRECO (OPCODE, SELECT, SELCHR, TYPE, IRET)
C-----------------------------------------------------------------------
C   MCRECO returns a selected, averaged Modcomp archive record of FORMAT
C   (TYPE) 1 or 2.  RECORD returned in /MODCOM/ (includes
C   DMC.INC,CMC.INC).
C    Inputs:
C     OPCODE     C*4  OPCODE 'READ' => read record, 'INIT' = Initialize
C     SELECT(10)R    Selection parameters.
C                       3 => source qualifier, -1 => all
C                       7 => Averaging time (rad.)
C                       9 => subarray, 0 => any
C     SELCHR*20 C    Character selection info:
C                     (1:8) = source name, (blank means any)
C                     (9:9) = Frequency band code (blank means any)
C                     (10:15) = observing program name "
C                     (16:17) = VLA observing mode     "
C    Output:
C     TYPE       I    FORMAT number 1 (data) or 2 (MODCOMP files)
C     IRET       I    return code. 0=>OK, 4=>end of data,
C                     10=data not Modcomp archive.
C-----------------------------------------------------------------------
      CHARACTER SELCHR*20, OPCODE*4, CTEMP*20
      INTEGER    IRET, IPSUB, TYPE, ISUBAR, ITEMP(10), IPTREC, NWDREC,
     *   LASTCH
      LOGICAL    T, F, DOINIT, ISCO, ISLI, WANTED, NEXTR,
     *   GOT1, GOT2, PERROR
      REAL      SELECT(10)
      DOUBLE PRECISION    RMJAD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'MC.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Initialize FORMAT pointer.
 50   LASTCH = 0
      MCLNF1 = 0
      MCFNRC = 0
      GOT1 = F
      GOT2 = F
      PERROR = F
C                                       Check opcode
      DOINIT = OPCODE.EQ.'INIT'
      IF (.NOT.DOINIT) GO TO 140
C                                       Init I/O (using TAPIO)
         DOCHK = T
         NOFILE = NOFILE + 1
C                                       Get next MODCOMP record.
 100  CALL MCREAD (IRET)
      DOCHK = T
C                                       Check end of file, PE
      PERROR = IRET.EQ.3
      IF ((IRET.EQ.3) .OR. (IRET.EQ.4)) GO TO 995
      IF (IRET.EQ.0) GO TO 140
         WRITE (MSGTXT,1100) IRET, OPCODE
         GO TO 990
C                                       If not first record of block,
C                                       skip.
 140  IF (TBUFF(3).NE.1) GO TO 100
C                                       Validity checks.
C                                       Get MJAD
      CALL ZR8P4 ('4IB8', TBUFF(MCLSTR+5), RMJAD)
C                                       FORMAT number.
      TYPE = TBUFF(MCLSTR+3)
      IF (TYPE.GT.2) GO TO 100
C                                       Now different things for
C                                       different FORMATs.
      IF (TYPE.EQ.2) GO TO 700
C                                       If already have FORMAT 2 data
C                                       return.
      IF (GOT2) GO TO 995
C
      IF (.NOT.DOCHK) GO TO 170
C                                       This test only good for FORMAT 1
C                                       Control program ID must be
C                                       "CO" or "LI"
      CALL ZILI16 (1, TBUFF(MCLSTR+9), 1, ITEMP)
      CALL ZC8CL (2, 1, ITEMP, CTEMP)
      ISCO = CTEMP(1:2).EQ.'CO'
      ISLI = CTEMP(1:2).EQ.'LI'
      IF (ISCO.OR.ISLI) GO TO 160
C                                       Doesn't look like data.
         IRET = 10
         WRITE (MSGTXT,1140)
         GO TO 990
C                                       FORMAT (=TYPE) 1
C                                       Convert 1st block to local form.
 160     CALL MCFMT1
         DOCHK = F
C                                       Data selection.
C                                       Check if data wanted.
 170     CALL MCWANT (SELECT, SELCHR, WANTED, NEXTR)
         IF ((.NOT.WANTED) .AND. (.NOT.GOT1)) GO TO 100
C                                       See if wrong subarray
         ISUBAR = SELECT(9) + 0.5
         IPSUB = MCLSTR + TBUFF(MCLSTR+11) + 1
         IF ((.NOT.WANTED) .AND. (ISUBAR.NE. TBUFF(IPSUB))) GO TO 100
C                                       See if current average complete
         IF (NEXTR .OR. (.NOT.WANTED)) GO TO 300
C                                       Accumulate first block
         GOT1 = T
         IPTREC = 1
         CALL MCSUM (ISLI, IPTREC)
         MCLNF1 = MCLNF1 + 1
         NWDREC = IPTREC
C                                       Loop over remaining blocks
C                                       Check if logical record finished
 180        IF (TBUFF(3).EQ.TBUFF(4)) GO TO 100
C                                       Read next block.
            CALL MCREAD (IRET)
C                                       Check end of file, PE
            PERROR = IRET.EQ.3
            IF ((IRET.EQ.3) .OR. (IRET.EQ.4)) GO TO 995
            IF (IRET.EQ.0) GO TO 200
               WRITE (MSGTXT,1100) IRET, 'READ'
               GO TO 990
C                                       Accumulate
 200        CALL MCSUM (ISLI, IPTREC)
            NWDREC = IPTREC
            GO TO 180
C                                       Finished accumulation,
C                                       normalize RECORD.
 300     IF (MCLNF1.GT.1) CALL MCNORM (ISLI, NWDREC)
C                                       Done - return
         GO TO 995
C                                       FORMAT (=TYPE) 2
C                                       If filling FORMAT 1 record
C                                       finish it.
 700  IF (GOT1) GO TO 300
C                                       Convert block to local form.
      CALL MCFMT2 (LASTCH)
      GOT2 = T
C                                       Loop til FORMAT 2 records
C                                       finished
C                                       Check if last block.
C                                       if so read next record
      IF (TBUFF(3).EQ.TBUFF(4)) GO TO 100
C                                       Read next block.
      CALL MCREAD (IRET)
C                                       Check end of file,PE
      PERROR = IRET.EQ.3
      IF ((IRET.EQ.3) .OR. (IRET.EQ.4)) GO TO 995
      IF (IRET.EQ.0) GO TO 700
         WRITE (MSGTXT,1100) IRET, 'READ'
         GO TO 990
C                                       Error
 990  CALL MSGWRT (8)
C                                       Restore proper TYPE
 995  TYPE = 1
      IF (GOT2) TYPE = 2
C                                       Handle parity errors
      IF (PERROR) PECNT = PECNT + 1
C                                       Stop after 50 parity errors
      IF (PERROR .AND. (PECNT.LE.50)) GO TO 50
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('MCRECO: ERROR',I3,2X,A4,'ING MODCOMP TAPE')
 1140 FORMAT ('MCRECO: DATA DOES NOT APPEAR TO BE MODCOMP ARCHIVE')
      END
      SUBROUTINE MCREAD (IRET)
C-----------------------------------------------------------------------
C   MCREAD reads a MODCOMP VLA archive format record and converts it
C   into local short integers; leave data in buffer TBUFF.
C    Output:
C     IRET       I    Return code, 0=>OK, 4=>EOF, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   IRET, NWORDS, TEMP(4)
      INCLUDE 'MC.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Do transfer
      MSGSUP = 1000
      CALL TAPIO ('READ', FDVEC, TAPBUF, TAPIND, IRET)
      MSGSUP = 0
      IF (IRET.EQ.10) IRET = 0
C                                       Check EOF or parity error.
      IF ((IRET.EQ.3) .OR. (IRET.EQ.4)) GO TO 999
      IF (IRET.EQ.0) GO TO 50
         WRITE (MSGTXT,1000) IRET, 'READ'
         GO TO 990
C                                       Undo DEC-10 tape format
 50   NWORDS = 1
      CALL ZRDMF (NWORDS, TAPBUF(TAPIND), TEMP, 1)
      NWORDS = TEMP(2)
C                                       Check limits
      IF (NWORDS.GT.(FDVEC(2)/5)) NWORDS = FDVEC(2) / 5
      CALL ZRDMF (NWORDS, TAPBUF(TAPIND), TBUFF, 1)
C                                       Done
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MCREAD: ERROR',I4,1X,A4,'ING MODCOMP VLA ARCHIVE TAPE')
      END
      SUBROUTINE MCFMT1
C-----------------------------------------------------------------------
C   MCFMT1 converts the first block of a MODCOMP VLA archive FORMAT 1
C   tape record to local data types.  Data is presumed to be in the
C   form of 2 bytes per local short integer such that integers are
C   correct.  Characters are returned as hollerith strings. Uses TBUFF
C   array in /MODCOM/ as the input and output array.
C   Output record in local form.  Real and double precision values are
C   converted to arrays of the appropriate type and these values are not
C   kept in local form in RECORD.  (See MCREC for details.)
C-----------------------------------------------------------------------
      INTEGER   INDEX, NCONV, IAANT, IA, ITEMP(10), IPSUB, KANT, LANT
      CHARACTER CBUFF*8
      INCLUDE 'MC.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       First do characters, must undo
C                                       correction to local integers.
C                                       Data type (LI, CO)
      INDEX = MCLSTR + 9
      NCONV = 1
      CALL ZILI16 (NCONV, TBUFF(INDEX), 1, TBUFF(INDEX))
      NCONV = 2
      CALL ZC8CL (NCONV, 1, TBUFF(INDEX), CBUFF)
      CALL CHR2H (NCONV, CBUFF, 1, TBUFF(INDEX))
C                                       Source name
      IPSUB = MCLSTR + TBUFF(MCLSTR+11) + 1
      INDEX = IPSUB + 2
      NCONV = 4
      CALL ZILI16 (NCONV, TBUFF(INDEX), 1, ITEMP)
      NCONV = 8
      CALL ZC8CL (NCONV, 1, ITEMP, CBUFF)
      CALL CHR2H (NCONV, CBUFF, 1, TBUFF(INDEX))
C                                       Program name
      INDEX = IPSUB + 8
      NCONV = 3
      CALL ZILI16 (NCONV, TBUFF(INDEX), 1, ITEMP)
      NCONV = 6
      CALL ZC8CL (NCONV, 1, ITEMP, CBUFF)
      CALL CHR2H (NCONV, CBUFF, 1, TBUFF(INDEX))
C                                       Observing mode
      INDEX = IPSUB + 12
      NCONV = 1
      CALL ZILI16 (NCONV, TBUFF(INDEX), 1, ITEMP)
      NCONV = 2
      CALL ZC8CL (NCONV, 1, ITEMP, CBUFF)
      CALL CHR2H (NCONV, CBUFF, 1, TBUFF(INDEX))
C                                       Calcode
      INDEX = IPSUB + 13
      NCONV = 1
      CALL ZILI16 (NCONV, TBUFF(INDEX), 1, ITEMP)
      NCONV = 2
      CALL ZC8CL (NCONV, 1, ITEMP, CBUFF)
      CALL CHR2H (NCONV, CBUFF, 1, TBUFF(INDEX))
C                                       Single precision reals.
C                                       Start, stop LST (rad)
      INDEX = IPSUB + 18
      NCONV = 2
      CALL ZIPACK ('LTOS', NCONV*2, TBUFF(INDEX), 1, TBUFF(INDEX))
      CALL ZRM2RL (NCONV, TBUFF(INDEX), REC4(1))
C                                       refractivity, zenith path delay
      INDEX = IPSUB + 62
      NCONV = 2
      CALL ZIPACK ('LTOS', NCONV*2, TBUFF(INDEX), 1, TBUFF(INDEX))
      CALL ZRM2RL (NCONV, TBUFF(INDEX), REC4(3))
C                                       Precessed u,v
      IAANT = MCLSTR + TBUFF(MCLSTR+13) + 1
      NCONV = 2
      KANT = TBUFF(MCLSTR+14)
      LANT = TBUFF(MCLSTR+12)
      DO 100 IA = 1,KANT
         INDEX = IAANT + 22 + (IA-1) * LANT
         CALL ZIPACK ('LTOS', NCONV*2, TBUFF(INDEX), 1, TBUFF(INDEX))
         CALL ZRM2RL (NCONV, TBUFF(INDEX), REC4UV(1,IA))
 100     CONTINUE
C
C                                       Double precision reals.
C
C                                       RA(1950), Dec(1950), RA(app),
C                                       Dec(app), LO1(GHz), LO2, LO3,
C                                       LO4, IAT end(rad), LST end(rad)
      INDEX = IPSUB + 22
      NCONV = 20
      CALL ZILI16 (NCONV*4, TBUFF(INDEX), 1, TBUFF(INDEX))
      CALL ZBYTFL (NCONV*4, TBUFF(INDEX), TBUFF(INDEX))
      NCONV = 10
      CALL ZDM2DL (NCONV, TBUFF(INDEX), REC8(1))
C                                       Velocities, rest freq.
      INDEX = IPSUB + 80
      NCONV = 8
      CALL ZILI16 (NCONV, TBUFF(INDEX), 1, TBUFF(INDEX))
      CALL ZBYTFL (NCONV, TBUFF(INDEX), TBUFF(INDEX))
      NCONV = 4
      CALL ZDM2DL (NCONV, TBUFF(INDEX), REC8(11))
C                                       Done
 999  RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE MCFMT2 (LASTCH)
C-----------------------------------------------------------------------
C   MCFMT2 converts a block of a MODCOMP VLA archive FORMAT 2
C   tape record to 80 character local logical records.
C   Data is presumed to be in the  form of local short
C   integer such that integers are  correct.  Each logical record is
C   a packed string.
C    Inputs:
C     LASTCH       I    Position in RECORD of last character written.
C                       should be set to 0 before first call for a
C                       given FORMAT 2 logical record (1 or more tape
C                       blocks)
C    Output:
C     LASTCH       I    Position in RECORD of last character written.
C                       Will contain the address - DON'T touch!
C-----------------------------------------------------------------------
      INTEGER   IAREC, NBYTE, NWORD, ITEMP, ITEMP2, ILOOP, INDEX
      INTEGER   LASTCH, MAXMOR, IRECPT
      INCLUDE 'MC.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Loop over sub records
      DO 200 ILOOP = 1,7
C                                       Save file names
         INDEX = MCLSTR + 11 + (ILOOP-1) * 5
C                                       Check if first block of file
         IF ((TBUFF(INDEX+4).GT.1) .AND. (MCFNRC.GT.0)) GO TO 40
C                                       Save file name
            MCFNRC = MCFNRC + 1
            IF (MCFNRC.GT.20) GO TO 999
            CALL ZILI16 (4, TBUFF(INDEX), 1, TBUFF(INDEX))
            CALL ZC8CL (8, 1, TBUFF(INDEX), MCFNAM(MCFNRC))
C                                       Set up for new file.
            MCLNF2 = MCLNF2 + 1
            MCLF2B = 0
            MCFLIN(MCFNRC) = MCLNF2 + 1
            LASTCH = 0
C                                       Base address of first record.
 40      IAREC = MCLSTR + 65 + 128 * (ILOOP - 1)
C                                       Number of bytes to crunch.
         NBYTE = 256
C                                       Pointer for RECORD
         IRECPT = (MCFLIN(MCFNRC)-1)
         IRECPT = IRECPT * (80 / 4) + 1
C                                       Make sure there is room in
C                                       RECORD
         MAXMOR = MCLMAX - MCLNF2*80 - MCLF2B - 80
         IF (MAXMOR.LT.0) MAXMOR = 0
         IF (NBYTE.GT.MAXMOR) NBYTE = MAXMOR
         IF (NBYTE.LE.0) GO TO 999
C                                       Flip bytes if necessary
         NWORD = NBYTE / 2
         CALL ZILI16 (NWORD, TBUFF(IAREC), 1, TBUFF(IAREC))
C                                       Un pack to RECORD
         CALL ZMCACL (NBYTE, TBUFF(IAREC), RECORD(IRECPT), LASTCH)
C                                       Update card count
         ITEMP = NBYTE + MCLF2B
         ITEMP2 = ITEMP / 80
         MCLNF2 = MCLNF2 + (NBYTE + MCLF2B) / 80
         MCLF2B = ITEMP - ITEMP2*80
 200     CONTINUE
C                                       Done
 999  RETURN
      END
      SUBROUTINE MCANT
C-----------------------------------------------------------------------
C   MCANT reads MODCOMP VLA archive FORMAT 2 records and extracts
C   desired information about the antennas and stuffs it into the
C   COMMON /ANTS/.
C    Inputs from common:
C     RECORD(*)    I   MODCOMP file records as 80 byte packed strings.
C     NREC         I   Number of card images.
C    Output in common /ANTS/
C     XIAT       R    The conversion from IAT to UTC
C     ANTLOC(3,*)  D    Antenna X,Y,Z (meters)
C     ANTNAM(2,*)  R    Antenna name as packed string.
C-----------------------------------------------------------------------
      CHARACTER LINE*80, ARRAY*8, ANTE*8, BASE*8, VLA*4, CHTMP*12
      INTEGER   IANT, IFILE, MANT, IA, KBP, KBPLIM, IREC
      INTEGER   INDEX, RECINC
      LOGICAL   EQUAL
      DOUBLE PRECISION    YANT, X, Y, Z, CLIGHT, TEMP(10)
      INCLUDE 'FILLR.INC'
      INCLUDE 'MC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA CLIGHT /2.997924562D8/
      DATA KBPLIM /80/
      DATA   ARRAY,      ANTE,       BASE,   VLA
     *   /'ARRAY   ', 'ANTENNAS', 'BASELINE','VLA:'/
C-----------------------------------------------------------------------
      RECINC = 20
C                                       Loop looking for ARRAY, ANTENNAS
C                                       or BASELINE files.
      DO 500 IFILE = 1,MCFNRC
C                                       Set first record of file.
         IREC = MCFLIN(IFILE)
         INDEX = (IREC-1) * RECINC + 1
C                                       Check ARRAY
         EQUAL = ARRAY .EQ. MCFNAM(IFILE)
         IF (EQUAL) GO TO 100
C                                       Check ANTENNA
         EQUAL = ANTE .EQ. MCFNAM(IFILE)
         IF (EQUAL) GO TO 200
C                                       Check ARRAY
         EQUAL = BASE .EQ. MCFNAM(IFILE)
         IF (EQUAL) GO TO 300
C                                       No match - ignore
         GO TO 500
C                                       ARRAY information
C                                       First rec UT1, IAT-UTC
 100        CALL H2CHR (80, 1, RECORD(INDEX), LINE)
            KBP = 1
            CALL GETNUM (LINE, KBPLIM, KBP, TEMP(1))
            IF (TEMP(1).EQ.DBLANK) GO TO 490
            CALL GETNUM (LINE, KBPLIM, KBP, TEMP(2))
            IF (TEMP(2).EQ.DBLANK) GO TO 490
            CALL GETNUM (LINE, KBPLIM, KBP, TEMP(3))
            IF (TEMP(3).EQ.DBLANK) GO TO 490
            XUT1 = TEMP(1) * (RMJAD - TEMP(2))
            XIAT = TEMP(3)
C                                       Second record, Polar X,Y etc.
            IREC = IREC + 1
            INDEX = (IREC-1) * RECINC + 1
            CALL H2CHR (80, 1, RECORD(INDEX), LINE)
            KBP = 1
            CALL GETNUM (LINE, KBPLIM, KBP, TEMP(1))
            IF (TEMP(1).EQ.DBLANK) GO TO 490
            CALL GETNUM (LINE, KBPLIM, KBP, TEMP(2))
            IF (TEMP(2).EQ.DBLANK) GO TO 490
            POLARX = TEMP(1)
            POLARY = TEMP(2)
            GO TO 500
C                                       ANTENNA records
 200        MANT = 28
            DO 250 IA = 1,MANT
               CALL H2CHR (80, 1, RECORD(INDEX), LINE)
               KBP = 1
               CALL GETNUM (LINE, KBPLIM, KBP, YANT)
               IF (YANT.EQ.DBLANK) GO TO 490
               IANT = YANT + 0.1
               KBP = KBP + 7
               CALL H2CHR (4, 1, RECORD(INDEX), CHTMP)
               IF ((IANT.LE.0) .OR. (IANT.GT.MANT)) GO TO 240
                  ANTNAM(IANT) = VLA // CHTMP(1:4)
 240           IREC = IREC + 1
               INDEX = (IREC-1) * RECINC + 1
 250           CONTINUE
            GO TO 500
C                                       BASELINE records.
 300        MANT = 28
            DO 350 IA = 1,MANT
C                                       Free field, use GETNUM
               CALL H2CHR (80, 1, RECORD(INDEX), LINE)
               KBP = 1
               CALL GETNUM (LINE, KBPLIM, KBP, YANT)
               IF (YANT.EQ.DBLANK) GO TO 490
               CALL GETNUM (LINE, KBPLIM, KBP, X)
               IF (X.EQ.DBLANK) GO TO 490
               CALL GETNUM (LINE, KBPLIM, KBP, Y)
               IF (Y.EQ.DBLANK) GO TO 490
               CALL GETNUM (LINE, KBPLIM, KBP, Z)
               IF (Z.EQ.DBLANK) GO TO 490
               IANT = YANT + 0.1
               IF ((IANT.LE.0) .OR. (IANT.GT.MANT)) GO TO 340
C                                       Convert positions to meters
                  ANTLOC(1,IANT) = X * CLIGHT * 1.0D-9
                  ANTLOC(2,IANT) = Y * CLIGHT * 1.0D-9
                  ANTLOC(3,IANT) = Z * CLIGHT * 1.0D-9
 340           IREC = IREC + 1
               INDEX = (IREC-1) * RECINC + 1
 350           CONTINUE
            GO TO 500
C
 490        MSGTXT = 'PROBLEM PARSING ANT DATA'
            CALL MSGWRT (7)
 500        CONTINUE
      WRITE (MSGTXT,1500)
      CALL MSGWRT (6)
      GO TO 999
C                                       Done
 999  RETURN
C-----------------------------------------------------------------------
 1500 FORMAT ('Found modcomp system files')
      END
      SUBROUTINE MCWANT (SELECT, SELCHR, WANTED, NEXTR)
C-----------------------------------------------------------------------
C   MCWANT determines if the current FORMAT 1 record meets the selection
C   criteria; and, if it does, should it be included in the current
C   accumulation.  Uses /MODCOM/ heavily.
C      Also call MCLINE to derive line info from header.
C    Inputs:
C     SELECT(10)R    Selection parameters.
C                       3 => source qualifier, -1 => all
C                       7 => Averaging time (rad.)
C                       9 => subarray, 0 => any
C     SELCHR*20 C    Character selection info:
C                     (1:8) = source name, (blank means any)
C                     (9:9) = Frequency band code (blank means any)
C                     (10:15) = observing program name "
C                     (16:17) = VLA observing mode     "
C    Output:
C     WANTED     L    If true, the data meets the selection criteria.
C     NEXTR      L    If false, the current record goes into the
C                     current accumulation.
C    Output in common /MODCOM/
C     TESTF(4)   D    The frequencies (GHz) of the 4 IFs.
C-----------------------------------------------------------------------
      CHARACTER SELCHR*20, FCODE(10)*2, BANDAC*2, BANDBD*2, CHTMP*8
      INTEGER   IPSUB, INDEX, IQUAL, IB, NCODE, ISUBAR, IROUND, INCH
      LOGICAL   WANTED, NEXTR, T, F, EQUAL, ISBLNK, ISLAG
      REAL      SELECT(10), FLIM(10)
      DOUBLE PRECISION    XTIME, FOFFST(6)
      INCLUDE 'MC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'FILLR.INC'
      DATA NCODE /9/
      DATA FCODE /    'P', 'L', 'S', 'C',  'X', 'U',  'K', 3*' '/
      DATA FLIM /0.1, 1.0, 2.0, 4.0, 6.0, 10.0, 16.0, 26.0, 2*1.0E6/
      DATA FOFFST /25.0D-3, 25.0D-3, 40.0D-3, 25.0D-3, 40.0D-3,
     *   25.0D-3/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Initialize results
      WANTED = F
      NEXTR = T
      IPSUB = MCLSTR + TBUFF(MCLSTR+11) + 1
C                                       Check program name.
C                                       See if blank
      IF (SELCHR(10:15).EQ.'      ') GO TO 50
        CALL H2CHR (6, 1, TBUFF(IPSUB+8), CHTMP)
        IF (CHTMP(1:6).NE.SELCHR(10:15))  GO TO 999
C                                       Check source.
 50   IF (SELCHR(1:8).EQ.'        ') GO TO 60
        CALL H2CHR (8, 1, TBUFF(IPSUB+2), CHTMP)
        IF (SELCHR(1:8).NE.CHTMP(1:8))  GO TO 999
C                                       Check mode.
 60   IF (SELCHR(16:17).EQ.'  ') GO TO 70
        CALL H2CHR (2, 1, TBUFF(IPSUB+12), CHTMP)
        IF (SELCHR(16:17).NE.CHTMP(1:2))  GO TO 999
C                                       Check subarray
 70   ISUBAR = SELECT(9) + 0.5
      EQUAL = (ISUBAR.LE.0) .OR. (ISUBAR.EQ.TBUFF(IPSUB))
        IF (.NOT.EQUAL)  GO TO 999
C                                       Check no. channels
      INCH = IROUND (SELECT(10))
      IF ((TBUFF(IPSUB+16).NE.INCH) .AND. (INCH.NE.0)) GO TO 999
C                                       Check qualifier
      IQUAL = IROUND (SELECT(3))
      IF (.NOT.((IQUAL.EQ.TBUFF(IPSUB+6)) .OR. (IQUAL.LE.0)))
     *   GO TO 999
C                                       Get calcode
      CALL H2CHR (1, 1, TBUFF(IPSUB+13), CALKOD)
C                                       Check band.
C                                       Get frequencies
      BCODE = TBUFF(IPSUB+72) / 256
      DO 100 IB = 1,4
         TESTF(IB) = REC8(IB+4) + FOFFST(BCODE+1)
 100     CONTINUE
C                                       Get bands (Default=P)
      BANDAC = FCODE(1)
      BANDBD = FCODE(1)
      DO 110 IB = 1,NCODE
         IF (TESTF(1).GT.FLIM(IB)) BANDAC = FCODE(IB)
         IF (TESTF(2).GT.FLIM(IB)) BANDBD = FCODE(IB)
 110     CONTINUE
C                                       Check if no code specified
      ISBLNK = SELCHR(9:9) .EQ. ' '
C                                       If none specified take this one
      IF (ISBLNK) SELCHR(9:9) = BANDAC(1:1)
C                                       Check AC band only for present
      IF (SELCHR(9:9).NE.BANDAC(1:1)) GO TO 999
C                                       Crack line info from header.
      CALL MCLINE (SELCHR, ISLAG)
C                                       Ignore lag data.
      IF (ISLAG) GO TO 999
C                                       Passed all tests
      WANTED = T
C-----------------------------------------------------------------------
C                                       Data wanted, check if time for
C                                       new integration.
C                                       Save subarray.
      SELECT(9) = TBUFF(IPSUB)
C                                       Save no. channels.
      SELECT(10) = TBUFF(IPSUB+16)
C                                       Check if any data accumulated
      NEXTR = MCLNF1.NE.0
C                                       Start accumulation.
      IF (MCLNF1.EQ.0) GO TO 999
C                                       See if same integration
      INDEX = 1 + MCLSTR
      IPSUB = MCLSTR + TBUFF(INDEX+10) + 1
C                                       Get time.
      XTIME = REC8(9)
C                                       Check time
      NEXTR = (ABS (XTIME-YCLTIM) .GT. SELECT(7))
      IF (NEXTR) GO TO 999
C                                       Check source.
      CALL H2CHR (8, 1, TBUFF(IPSUB+2), CHTMP)
      NEXTR = CHTMP(1:8) .NE. XCLSOR(1:8)
      IF (NEXTR) GO TO 999
C                                       Check subarray.
      NEXTR = MCLSUB .NE. TBUFF(IPSUB)
      IF (NEXTR) GO TO 999
C                                       Check frequency
      NEXTR = ((ABS (YCLFRE(1)-TESTF(1))/TESTF(1)) .GT. 1.0E-3)
      IF (NEXTR) GO TO 999
C                                       Goes in the current accumulation
C
 999  RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE MCSUM (ISLINE, IPTREC)
C-----------------------------------------------------------------------
C   MCSUM accumulates MODCOMP VLA archive FORMAT 1 data from TBUFF
C   into RECORD.  Uses /MODCOM/ heavily.
C    Input:
C     ISLINE  L    If true data is line format.
C     IPTREC  I    Pointer to the next word in RECORD to use, should be
C                  1 on first call.
C    Output:
C     IPTREC  I    Pointer to the next word in RECORD to use.
C-----------------------------------------------------------------------
      INTEGER   LTBLK, IPSUB, IPANT, IA, NANT, TINDEX, ISTAT, JSTAT
      INTEGER   IPTREC, INDEX, NWORDS, LOOP
      LOGICAL   ISLINE, NOTNO1
      INCLUDE 'MC.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       See if first entry.
      NOTNO1 = IPTREC.GT.1
      IF (MCLNF1.GT.0) GO TO 500
C                                       NOTE: will get here only for
C                                       first tape block; all non
C                                       integer data is assumed in this
C                                       block.
C                                       First entry, initialize.
C                                       Zero fill.
      DO 10 LOOP = 1,MCLMAX
         RECORD(LOOP) = 0
 10      CONTINUE
      DO 20 LOOP = 1,28
         SRECUV(1,LOOP) = 0.0
         SRECUV(2,LOOP) = 0.0
 20      CONTINUE
      SREC8(1) = 0.0D0
      SREC8(2) = 0.0D0
C                                       Copy first tape block
      LTBLK = MCLSTR + TBUFF(MCLSTR+17)
      CALL COPY (LTBLK, TBUFF, RECORD(IPTREC))
C                                       Update next word pointer
      IPTREC = IPTREC + LTBLK
C                                       Initialize integration info.
C                                       Time
      IPSUB = MCLSTR + RECORD(MCLSTR+11) + 1
      YCLTIM = REC8(9)
C                                       Frequencies.
      YCLFRE(1) = TESTF(1)
      YCLFRE(2) = TESTF(2)
      YCLFRE(3) = TESTF(3)
      YCLFRE(4) = TESTF(4)
C                                       Source
      INDEX = IPSUB + 2
      CALL H2CHR (8, 1, RECORD(INDEX), XCLSOR)
C                                       Subarray
      MCLSUB = RECORD(IPSUB)
C***???   ???????
C      GO TO 570
C-----------------------------------------------------------------------
C                                       Accumulate, check if first block
C                                       of logical record
 500  IF (NOTNO1) GO TO 600
C                                       First tape block, update
C                                       control info.
C***???
C                                       Accumulate count
C      IPSUB = MCLSTR + RECORD(MCLSTR+11) + 1
C      RECORD(IPSUB+17) = RECORD(IPSUB+17)
C                                       Sum end IAT time
      SREC8(1) = SREC8(1) + REC8(9)
C                                       Sum end LST time.
      SREC8(2) = SREC8(2) + REC8(10)
C                                       Save position
      SREC8(3) = REC8(1)
      SREC8(4) = REC8(2)
      SREC8(5) = REC8(3)
      SREC8(6) = REC8(4)
C                                       Sum u,v,(w)
      NANT = RECORD(MCLSTR+14)
      DO 550 IA = 1,NANT
C                                       integer values - don't do -
C                                       might overflow.
C                                       Floating values
C                                       Precessed U,V
         SRECUV(1,IA) = SRECUV(1,IA) + REC4UV(1,IA)
         SRECUV(2,IA) = SRECUV(2,IA) + REC4UV(2,IA)
 550     CONTINUE
C                                       Antenna no, IF flags. (570)
         NANT = RECORD(MCLSTR+14)
         DO 580 IA = 1,NANT
            IPANT = RECORD(MCLSTR+13) + (IA-1) * RECORD(MCLSTR+12) +
     *         MCLSTR + 1
C                                       Antenna number
            RECANT(IA) = TBUFF(IPANT) / 256
C                                       IF flags - 4 bits per IF
            ISTAT = TBUFF(IPANT+4)
C                                       D IF
            JSTAT = ISTAT / 4096
            IFFLAG(4,IA) = JSTAT
C                                       C IF
            ISTAT = ISTAT - JSTAT * 4096
            JSTAT = ISTAT / 256
            IFFLAG(3,IA) = JSTAT
C                                       B IF
            ISTAT = ISTAT - JSTAT * 256
            JSTAT = ISTAT / 16
            IFFLAG(2,IA) = JSTAT
C                                       A IF
            ISTAT = ISTAT - JSTAT * 16
            IFFLAG(1,IA) = ISTAT
 580        CONTINUE
C                                       Accumulate data
      INDEX = MCLSTR + RECORD(MCLSTR+17) + 1
      TINDEX = INDEX
C                                       Last 2 words ignored
      NWORDS = TBUFF(2) * 2  - INDEX - 1
      CALL IACC (ISLINE, NWORDS, TINDEX, INDEX)
C                                       Update next word pointer
      IPTREC = INDEX + NWORDS
C                                       Done - return
      GO TO 999
C                                       Subsequent records.
C                                       Accumulate data
 600  INDEX = IPTREC
      TINDEX = 1 + MCLSTR
      NWORDS = TBUFF(2) * 2 - MCLSTR - 2
      CALL IACC (ISLINE, NWORDS, TINDEX, INDEX)
C                                       Update next word pointer
      IPTREC = INDEX + NWORDS
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE MCNORM (ISLINE, NWDREC)
C-----------------------------------------------------------------------
C   MCNORM normalizes the accumulated MODCOMP VLA archive FORMAT 1
C   record in array RECORD.  Mostly the correlator values  and times
C   are averaged.  Uses /MODCOM/ heavily.
C    Inputs:
C     ISLINE  L    If true data is line format.
C     NWDREC  I    The number of I   words in RECORD
C-----------------------------------------------------------------------
      INTEGER   IPSUB, NMREC, NANT, IPANT, IA, NWDREC, INDEX, NWORDS
      LOGICAL   ISLINE
      INCLUDE 'MC.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Get no. records accum.
      IPSUB = MCLSTR + RECORD(MCLSTR+11) + 1
      NMREC = MCLNF1
C                                       If only 1 don't bother
      IF (NMREC.LE.1) GO TO 200
C                                       Average end IAT time
      SREC8(1) = SREC8(1) / NMREC
C                                       Average end LST time.
      SREC8(2) = SREC8(2) / NMREC
C                                       Average u,v,w
      IPANT = MCLSTR + RECORD(MCLSTR+13) + 1
      NANT = RECORD(MCLSTR+14)
      DO 50 IA = 1,NANT
C                                       integer values (NOT averaged)
C         RECORD(IPANT+1) = RECORD(IPANT+1) / NMREC
C         RECORD(IPANT+2) = RECORD(IPANT+2) / NMREC
C         RECORD(IPANT+3) = RECORD(IPANT+3) / NMREC
C                                       Floating values
C                                       Precessed U,V
         SRECUV(1,IA) = SRECUV(1,IA) / NMREC
         SRECUV(2,IA) = SRECUV(2,IA) / NMREC
C                                       Update pointer
         IPANT = IPANT + RECORD(MCLSTR+12)
  50     CONTINUE
C                                       Average data
      INDEX = MCLSTR + RECORD(MCLSTR+17) + 1
      NWORDS = NWDREC - INDEX
      CALL IDIV (ISLINE, NWORDS, RECORD(INDEX))
C                                       Cleanup
C                                       Reset no. records stacked.
 200  MCLNF1 = 0
C
 999  RETURN
      END
      SUBROUTINE IACC (ISLINE, NWORD, TINDEX, INDEX)
C-----------------------------------------------------------------------
C   IACC accumulates MODCOMP visibility data.
C    Input:
C     ISLINE   L    If true data is line format.
C     NWORD    I    Number of short integers to accumulate.
C     TBUFF(*)I    Array to the added to accumulator. (from common)
C     TINDEX   I    Start index in input record.
C     RECORD(*)I    Accumulation array. (from common)
C     INDEX    I    Start index in output array, also used to determine
C                   position in the data array.
C    Output:
C     RECORD(*)I    Accumulation array.
C-----------------------------------------------------------------------
      INTEGER   TINDEX,
     *   I, IFPNT(4,4), IFPA1, IFPA2, SEQ, IA, IB, ITEST, NANT, LREC,
     *   ILEFT, ICONCH, IDATY, IP, IPTWT, IPSUB
      INTEGER   NWORD, INDEX, OUTPTR, INPTR
      LOGICAL   ISLINE
      INCLUDE 'MC.INC'
      DATA IFPNT /1,3,1,3, 1,3,3,1, 2,4,2,4, 2,4,4,2/
C-----------------------------------------------------------------------
      NANT = RECORD(MCLSTR+14)
      IPSUB = RECORD(MCLSTR+11) + MCLSTR + 1
      IPTWT = IPSUB + 17
      INPTR = TINDEX
      OUTPTR = INDEX
C                                       Determine start antennas etc.
      SEQ = (INDEX - RECORD(MCLSTR+17) - MCLSTR - 1)
C                                       Check BD ifs
      ICONCH = 0
      IF (INDEX.GE.(MCLSTR+RECORD(MCLSTR+19))) ICONCH = 2
      IF (ICONCH.EQ.2) SEQ = (INDEX - RECORD(MCLSTR+19) - MCLSTR - 1)
      LREC = 12
      IF (ISLINE) LREC = NUMCOR * 2 + 2
      ILEFT = MOD (SEQ, LREC)
      SEQ = (SEQ / LREC) + 1
      ITEST = 1
      DO 50 I = 1,NANT
         IA = I
         ITEST = ITEST + NANT - I
         IF (SEQ.LT.ITEST) GO TO 60
 50      CONTINUE
 60      IB = SEQ - ITEST + NANT + 1
C                                       Check data type
      IF (ISLINE) GO TO 500
C                                       Continuum format data.
C                                       Loop accumulating data
      DO 200 I = 1,NWORD
C                                       Polarization type
         IP = ILEFT / 3 + 1
C                                       Check flags
         IFPA1 = IFPNT(IP,1+ICONCH)
         IFPA2 = IFPNT(IP,2+ICONCH)
         IF ((IFFLAG(IFPA1,IA)+IFFLAG(IFPA2,IB).GT.PASFLG(2)) .OR.
     *      (IFFLAG(IFPA1,IA).GT.PASFLG(1)) .OR.
     *      (IFFLAG(IFPA2,IB).GT.PASFLG(1))) GO TO 185
C                                       Determine real, imag, wt
            IDATY = MOD (ILEFT, 3) + 1
            GO TO (100, 100, 180), IDATY
C                                       Real or imag.
 100        RECORD(OUTPTR) = RECORD(OUTPTR) + TBUFF(INPTR)
            GO TO 190
C                                       Weight (sum of counts)
 180        RECORD(OUTPTR) = RECORD(OUTPTR) + 1
 185        IF (ILEFT.LT.(LREC-1)) GO TO 190
C                                       Update antennas
               IB = IB + 1
               IF (IB.GT.NANT) IA = IA + 1
               IF (IB.GT.NANT) IB = IA + 1
C                                       Check if gone to BD ifs.
               IF (IA.LT.NANT) GO TO 190
C                                       Now B-D ifs.
                  ICONCH = 2
                  IA = 1
                  IB = 2
 190        CONTINUE
C                                       Get set for next
         ILEFT = ILEFT + 1
         ILEFT = MOD (ILEFT, LREC)
         INPTR = INPTR + 1
         OUTPTR = OUTPTR + 1
 200     CONTINUE
      GO TO 999
C                                       Line records.
C                                       Get length of vis, rec
 500  CONTINUE
C                                       All same IF
      IFPA1 = IFPNT(1,1)
      IFPA2 = IFPNT(1,2)
C                                       Loop accumulating data
      DO 700 I = 1,NWORD
         IF (ILEFT.GT.1) GO TO 670
C                                       Sum counts in "pipeline words"
C                                       Check flagging
         IF ((IFFLAG(IFPA1,IA)+IFFLAG(IFPA2,IB).LE.PASFLG(2)) .AND.
     *      (IFFLAG(IFPA1,IA).LE.PASFLG(1)) .AND.
     *      (IFFLAG(IFPA2,IB).LE.PASFLG(1)))
     *         RECORD(OUTPTR) = RECORD(OUTPTR) + 1
            GO TO 680
C                                       Accumulate
C                                       Check flagging
 670     IF ((IFFLAG(IFPA1,IA)+IFFLAG(IFPA2,IB).LE.PASFLG(2)) .AND.
     *      (IFFLAG(IFPA1,IA).LE.PASFLG(1)) .AND.
     *      (IFFLAG(IFPA2,IB).LE.PASFLG(1)))
     *         RECORD(OUTPTR) = RECORD(OUTPTR) + TBUFF(INPTR)
C                                       Get set for next
 680     OUTPTR = OUTPTR + 1
         ILEFT = ILEFT + 1
         ILEFT = MOD (ILEFT, LREC)
         INPTR = INPTR + 1
C                                       Check if first word of vis.
         IF (ILEFT.GT.0) GO TO 700
C                                       Update antennas
            IB = IB + 1
            IF (IB.GT.NANT) IA = IA + 1
            IF (IB.GT.NANT) IB = IA + 1
            IF (IA.GE.NANT) GO TO 999
 700     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE IDIV (ISLINE, NWORD, ARRAY)
C-----------------------------------------------------------------------
C   IDIV divides summed data by the counts.
C    Input:
C     ISLINE       L    If true data is line format.
C     NWORD        I    Number of short integers to process.
C     ARRAY(NWORD) I    Array.
C    Output:
C     ARRAY(NWORD) I    Array.
C-----------------------------------------------------------------------
      INTEGER   JDIV, ARRAY(1), J, NCOR
      INTEGER   NWORD, I, LREC
      LOGICAL   ISLINE
      REAL      DIV
      INCLUDE 'MC.INC'
C-----------------------------------------------------------------------
      LREC = 3
      IF (ISLINE) LREC = NUMCOR * 2 + 2
C                                       Decide type
      IF (ISLINE) GO TO 500
C                                       Continuum data
      DO 100 I=1,NWORD,LREC
         JDIV = ARRAY(I+2)
         IF (JDIV.LE.1) GO TO 100
         ARRAY(I) = ARRAY(I) / JDIV
         ARRAY(I+1) = ARRAY(I+1) / JDIV
 100     CONTINUE
      GO TO 999
C                                       Line data.
 500  NCOR = NUMCOR * 2 + 2
      DO 600 I=1,NWORD,LREC
         IF (ARRAY(I).LE.1) GO TO 600
         DIV = 1.0 / ARRAY(I)
         DO 550 J = 3,NCOR
            ARRAY(I+J-1) = ARRAY(I+J-1) * DIV
 550        CONTINUE
 600     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE MCFLIP (IA1, IA2, NCHAN, NPOLN, U, V, W, VIS)
C-----------------------------------------------------------------------
C   MCFLIP reverses a baseline if IA1 is greater than IA2.
C    Inputs:
C     IA1         I    Number of first antenna.
C     IA2         I    Number of second antenna.
C     NCHAN       I    Number of frequency channels.
C     NPOLN       I    Number of polarization channels (RR, LL, RL, LR)
C     U,V,W       R    Spatial frequency coordinates.
C     VIS(3,NPOLN,NCHAN) R    Visibility data (real, imaginary, weight)
C-----------------------------------------------------------------------
      INTEGER   NCHAN, NPOLN
      INTEGER   IA1, IA2, ITEMP, IFREQ
      REAL      U, V, W, VIS(3,NPOLN,NCHAN), TEMPR, TEMPI, TEMPW
C-----------------------------------------------------------------------
C                                       Check antenna numbers
      IF (IA1.LT.IA2) GO TO 999
C                                       Flip antenna numbers.
      ITEMP  = IA1
      IA1 = IA2
      IA2 = ITEMP
C                                       Flip u,v,w
      U = - U
      V = - V
      W = - W
C                                       Loop thru frequency
C                                       Conjugate data.
      DO 100 IFREQ = 1,NCHAN
C                                       RR pol.
         VIS(2,1,IFREQ) = -VIS(2,1,IFREQ)
         IF (NPOLN.LE.1) GO TO 100
C                                       LL pol.
         VIS(2,2,IFREQ) = -VIS(2,2,IFREQ)
         IF (NPOLN.LE.2) GO TO 100
C                                       RL, LR pol. (switch)
         TEMPR = VIS(1,3,IFREQ)
         TEMPI = VIS(2,3,IFREQ)
         TEMPW = VIS(3,3,IFREQ)
         VIS(1,3,IFREQ) = VIS(1,4,IFREQ)
         VIS(2,3,IFREQ) = -VIS(2,4,IFREQ)
         VIS(3,3,IFREQ) = VIS(3,4,IFREQ)
         VIS(1,4,IFREQ) = TEMPR
         VIS(2,4,IFREQ) = -TEMPI
         VIS(3,4,IFREQ) = TEMPW
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE MCLINE (SELCHR, ISLAG)
C-----------------------------------------------------------------------
C   MCLINE determines the number of spectral channels, channel
C   separations etc. from the obscure parameters in the header.
C   Assumes that the first block of a logical record is in the tape
C   buffer TBUFF.
C    Inputs:
C     SELCHR*20 C    Character selection info:
C                     (1:8) = source name, (blank means any)
C                     (9:9) = Frequency band code (blank means any)
C                     (10:15) = observing program name "
C                     (16:17) = VLA observing mode     "
C    Output:
C     ISLAG       L    If true data is lag rather than frequency
C    Output to D/CMC.INC common:
C     NUMCOR      I    Number of complex values per baseline.
C     ISIDEB      I    Sideband 1=upper, 2 = lower
C     MODE        C    2 Character correlator mode.
C     CHNOFF      I    Offset of the first channel recorded from the
C                      first channel correlated.
C     ISHANN      I    If true Hanning smoothing was applied.
C     ISNORM      I    If true data normalized by autocorrelations.
C     CHNSEP      D    Channel spacing (Hz)
C     IVELTY      I    Velocity type 1=geocentric, 2=topocentric
C                      3 = barycentric, 4=LSR, 0=none
C     IVELDF      I    Velocity definintion, 1=radio, 2=optical, 0=none.
C-----------------------------------------------------------------------
      CHARACTER SELCHR*20, CHVEL(7)*2, CHCODE(5)*1, CHTMP*12
      INTEGER   IPSUB, I, INDEX, MODFAC, NCONV, ITEMP(10)
      LOGICAL   ISLAG, EQUAL, ISLINE
      DOUBLE PRECISION BWTAB(10)
      INCLUDE 'MC.INC'
      DATA CHVEL /'  ','G ','T ','B ','L ','V ','Z '/
      DATA BWTAB /3.125D6, 7.8125D5, 1.95313D5, 4.8828D4, 1.2207D4,
     *   3.052D3, 1.526D3, 1.526D3, 0.763D3, 0.381D3/
      DATA CHCODE /'H','B','L','X','U'/
C-----------------------------------------------------------------------
C                                       Subarray area pointer.
      IPSUB = MCLSTR + TBUFF(MCLSTR+11) + 1
C                                       Number of channels
      NUMCOR = TBUFF(IPSUB+16)
C                                       Mode
      CALL ZILI16 (1, TBUFF(IPSUB+97), 1, ITEMP)
      CALL ZC8CL (2, 1, ITEMP, MODE)
      ISIDEB = 1
      CHNSEP = 0.0D0
      CHNOFF = 0
      ISHANN = .FALSE.
      ISNORM = .FALSE.
      ISLAG = .FALSE.
      IVELTY = 0
      IVELDF = 0
C                                       Check if line data
      CALL H2CHR (2, 1, TBUFF(MCLSTR+9), CHTMP)
      ISLINE = CHTMP(1:2).EQ.'LI'
C                                       Don't bother further for
C                                       continuum.
      IF (.NOT.ISLINE) GO TO 999
C                                       Mode factor:
      READ (MODE,1000) MODFAC
C                                       Hanning, etc
      CALL ZILI16 (2, TBUFF(IPSUB+99), 1, ITEMP)
      CALL ZC8CL (4, 1, ITEMP(1), CHTMP(1:4))
      NCONV = 4
      DO 10 I = 1,3
         ISHANN = ISHANN .OR. (CHCODE(1) .EQ. CHTMP(I:I))
         ISNORM = ISNORM .OR. (CHCODE(2) .EQ. CHTMP(I:I))
         ISLAG = ISLAG .OR. (CHCODE(3) .EQ. CHTMP(I:I))
 10      CONTINUE
      IF (ISLAG) GO TO 999
C                                       Band code
      BCODE = TBUFF(IPSUB+72) / 256
C                                       Frequency increment
      CHNSEP = BWTAB(BCODE+1) * MODFAC
      IF (ISHANN) CHNSEP = CHNSEP * 2.0D0
C                                       Channel offset (l.s. byte)
      CHNOFF  = TBUFF(IPSUB+103)
      INDEX = CHNOFF / 256
      CHNOFF = CHNOFF - 256 * INDEX
C                                       Sideband X, U =>lower
C                                       (this assumes MCWANT)
      ISIDEB = 1
      EQUAL = SELCHR(9:9) .EQ. CHCODE(4)(1:1)
      IF (EQUAL) ISIDEB = 2
      EQUAL = SELCHR(9:9) .EQ. CHCODE(5)(1:1)
      IF (EQUAL) ISIDEB = 2
C                                       Velocity
      CALL ZILI16 (1, TBUFF(IPSUB+96), 1, ITEMP)
      CALL ZC8CL (1, 1, ITEMP(1), CHTMP)
      IVELTY = 0
      IVELDF = 0
      IF (CHTMP(1:2).EQ.CHVEL(2)) IVELTY = 1
      IF (CHTMP(1:2).EQ.CHVEL(3)) IVELTY = 2
      IF (CHTMP(1:2).EQ.CHVEL(4)) IVELTY = 3
      IF (CHTMP(1:2).EQ.CHVEL(5)) IVELTY = 4
      CALL ZC8CL (1, 2, ITEMP(1), CHTMP)
      IF (CHTMP(1:2).EQ.CHVEL(6)) IVELDF = 1
      IF (CHTMP(1:2).EQ.CHVEL(7)) IVELDF = 2
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I1)
      END
