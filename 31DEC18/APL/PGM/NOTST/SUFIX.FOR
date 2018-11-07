LOCAL INCLUDE 'SUFIX.INC'
C                                       Local include for SUFIX
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XNAMOU(3),
     *   XCLAOU(2), XXSTOK, XSNAME(4), XSCODE
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XBCHAN, XECHAN, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), XDOAC, XSOUT,
     *   XDISO, TB(30), TE(30), COORD(6), BADD(10),
     *   SCRBUF(256), BUFF2(UVBFSS)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ, ILOCWT,
     *   CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO,
     *   LRECO, NRPRMI, NRPRMO, OLDCNO, NEWCNO, NEWNUM, NTIMES
      LOGICAL   ISCOMP, OLDSRC
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, SNAME*16,
     *   SCODE*4
      DOUBLE PRECISION NEWRA, NEWDEC
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XXSTOK, XTIME, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF, XBCHAN,
     *   XECHAN, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND,
     *   XBPVER, XSMOTH, XDOAC, XNAMOU, XCLAOU, XSOUT, XDISO, TB, TE,
     *   XSNAME, XSCODE, COORD, BADD
      COMMON /SUFIXP/ CATOLD, NEWRA, NEWDEC, SEQIN, SEQOUT, DISKIN,
     *   DISKO, ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO,
     *   LRECO, NRPRMI, NRPRMO, ISCOMP, OLDCNO, NEWCNO, NEWNUM, NTIMES,
     *   OLDSRC
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, SNAME, SCODE
      COMMON /BUFRS/ SCRBUF, BUFF2, JBUFSZ
C                                       End local include for SUFIX
LOCAL END
      PROGRAM SUFIX
C-----------------------------------------------------------------------
C! Changes to new source number in up to 30 time ranges
C# Utility UV UV-util VLA VLB Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2009-2012, 2018
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
C   SUFIX copies a data set with calibration and changes the source
C   number assigned to the vis within up to 30 time ranges.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C   full set of calibration adverbs
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      KEYSTRNG       SNAME         new source name
C      SOUCODE        SCODE         new source cal code
C      COORDINA       COORD         source coordinates
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'SUFIX.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'SUFIX '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL SUFIXI (PRGM, IRET)
C                                       Call routine that does it
      IF (IRET.EQ.0) CALL SUFIXD (IRET)
C                                       history, copy tables
C                                       fix output SU table
      IF (IRET.EQ.0) CALL SUFIXH
C                                       Close down files, etc.
      CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE SUFIXI (PRGN, JERR)
C-----------------------------------------------------------------------
C   SUFIXI gets input parameters for SUFIX and creates an output file
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
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCX, I, NFREQ, LUN
      LOGICAL   MATCH
      REAL      CATR(256), RPARM(20)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128), DCOORD(6)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'SUFIX.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA BLANK  /' '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 246
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
      CALL H2CHR (4, 1, XCALC, SELCOD)
      CALL H2CHR (4, 1, XXSTOK, STOKES)
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      SELQUA = IROUND (XQUAL)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
      CALL H2CHR (16, 1, XSNAME, SNAME)
      CALL H2CHR (4, 1, XSCODE, SCODE)
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOACOR = XDOAC.GT.0.0
C                                       Set time range.
      CALL RCOPY (8, XTIME, TIMRNG)
      IF ((TIMRNG(1)+TIMRNG(2)+TIMRNG(3)+TIMRNG(4)) .EQ.0.0)
     *   TIMRNG(1)=-1.0E6
      IF ((TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8)) .EQ.0.0)
     *   TIMRNG(5)=1.0E6
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
      DOPOL = IROUND(XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
      PDVER = IROUND (XPDVER)
      DOAPPL = .FALSE.
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LT.0) SUBARR = 0
      FGVER = IROUND (XFLAG)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
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
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Channel selection?
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
      ELSE
         BIF = IROUND (XBIF)
         EIF = IROUND (XEIF)
         BIF = MIN (MAX (1, BIF), CATBLK(KINAX+JLOCIF))
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         END IF
      NFREQ = CATBLK(KINAX+JLOCF)
      BCHAN = IROUND (XBCHAN)
      ECHAN = IROUND (XECHAN)
      IF ((BCHAN.LE.0) .OR. (BCHAN.GT.NFREQ)) BCHAN = 1
      IF ((ECHAN.LE.0) .OR. (ECHAN.GT.NFREQ)) ECHAN = NFREQ
      IF (BCHAN.GT.ECHAN) THEN
         MSGTXT = 'INVALID BCHAN AND ECHAN'
         CALL MSGWRT (6)
         JERR = 1
         GO TO 990
         END IF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, OLDCNO, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       time ranges
      NTIMES = 30
      DO 20 I = 1,30
         IF ((TB(I).LE.0.0) .OR. (TE(I).LE.0.0) .OR. (TE(I).LT.TB(I)))
     *      THEN
            NTIMES = I - 1
            GO TO 25
            END IF
 20      CONTINUE
 25   IF (NTIMES.LT.1) THEN
         JERR = 10
         MSGTXT = 'NO VALID TIME RANGES SPECIFIED'
         GO TO 990
         END IF
C                                       get new source number
      DO 30 I = 1,6
         DCOORD(I) = COORD(I)
 30      CONTINUE
      NEWRA = DCOORD(1)*15.0D0 + DCOORD(2)/4.0D0 + DCOORD(3)/240.0D0
      NEWDEC = ABS (DCOORD(4)) + ABS (DCOORD(5)) / 60.0D0 +
     *   ABS (DCOORD(6)) / 3600.0D0
      IF ((DCOORD(4).LT.0.0) .OR. (DCOORD(5).LT.0.0) .OR.
     *   (DCOORD(6).LT.0.0)) NEWDEC = -NEWDEC
      CALL SUCHEK (SNAME, SCODE, NEWRA, NEWDEC, DISKIN, OLDCNO, NEWNUM,
     *   JERR)
      IF (JERR.GT.0) THEN
         MSGTXT = 'SUCHEK REPORTS FAILURE - QUITTING'
         GO TO 990
         END IF
      OLDSRC = NEWNUM.LT.0
      NEWNUM = ABS (NEWNUM)
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, SCRBUF, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1035) JERR
         GO TO 990
         END IF
      CALL UVGET ('CLOS', RPARM, SCRBUF, IERR)
C                                       Save input file info
      INCX = CATBLK(KINAX)
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
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
         CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', SCRBUF, IERR)
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
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, 'READ', SCRBUF, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
C                                       Copy any header keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SUFIXI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('SUFIXI: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE SUFIXD (IRET)
C-----------------------------------------------------------------------
C   SUFIXD sends uv data one point at a time to the user supplied
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
      INTEGER   IPTRO, LUNO, INDO, ILENBU, KBIND, NIOUT, NIOLIM, BO, VO,
     *   NUMVIS, XCOUNT, NCORO, NCOPY, CATMP(256), RNXRET
      LOGICAL   T, F
      INCLUDE 'SUFIX.INC'
      REAL      VIS(UVBFSS), RPARM(20)
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
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
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
      NUMVIS = 0
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
         NUMVIS = NUMVIS + 1
         RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
         RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
         RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
C                                       call user routine
         CALL SUFIXS (NUMVIS, RPARM(1+ILOCT), RPARM(1+ILOCSU), IRET)
C                                       Error (fatal)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1120) IRET
            GO TO 990
C                                       Copy to output.
         ELSE IF (IRET.EQ.0) THEN
            XCOUNT = XCOUNT + 1.0D0
            CALL RCOPY (NRPRMI, RPARM, BUFF2(IPTRO))
C                                       update NX table
            CALL RNXUPD (RPARM, RNXRET)
C                                       Compressed
            IF (ISCOMP) THEN
               CALL ZUVPAK (NCORO, VIS, BUFF2(IPTRO+ILOCWT),
     *            BUFF2(IPTRO+NRPRMO))
            ELSE
               CALL RCOPY (NCOPY, VIS, BUFF2(IPTRO+NRPRMO))
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
 1000 FORMAT ('SUFIXD: ERROR',I3,' OPEN/INIT INPUT VIS FILE')
 1010 FORMAT ('SUFIXD: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('SUFIXD: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1100 FORMAT ('SUFIXD: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('SUFIXD: SUFIXS ERROR',I3)
 1150 FORMAT ('SUFIXD: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE SUFIXH
C-----------------------------------------------------------------------
C   SUFIXH copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, DSIGN*1
      INTEGER   LUN1, LUN2, IERR, RH, RM, DD, DM
      REAL      RS, DS
      DOUBLE PRECISION D
      INCLUDE 'SUFIX.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1, LUN2 /27,28/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   SCRBUF, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
C                                       calibration history
      CALL CALHIS (LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       source stuff
      IF (.NOT.OLDSRC) THEN
         WRITE (HILINE,1100) TSKNAM, SNAME
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (HILINE,1101) TSKNAM, SCODE
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (HILINE,1102) TSKNAM, NEWNUM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         D = NEWRA / 15.0D0
         RH = D
         D = (D - RH) * 60.0D0
         RM = D
         RS = (D - RM) * 60.0D0
         D = NEWDEC
         DSIGN = ' '
         IF (D.LT.0.0) DSIGN = '-'
         DD = D
         D = (D - DD) * 60.0D0
         DM = D
         DS = (D - DM) * 60.0D0
         WRITE (HILINE,1103) TSKNAM, RH, RM, RS, DSIGN, DD, DM, DS
         IF (HILINE(23:23).EQ.' ') HILINE(23:23) = '0'
         IF (HILINE(37:37).EQ.' ') HILINE(37:37) = '0'
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
      ELSE
         WRITE (HILINE,1110) TSKNAM, NEWNUM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       Close HI file
 100  CALL HICLOS (LUN2, .TRUE., BUFF2, IERR)
C                                       Copy tables
      CALL COPTAB (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'SUFIXH: ERROR COPYING TABLES TO OUTPUT UV'
         CALL MSGWRT (6)
         END IF
      IF (.NOT.OLDSRC) CALL SUPDAT (DISKO, NEWCNO, SNAME, SCODE,
     *   NEWRA, NEWDEC, NEWNUM, IERR)
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', SCRBUF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SUFIXH: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1100 FORMAT (A6,'NEWNAME = ''',A,'''  / new source name')
 1101 FORMAT (A6,'NEWCODE = ''',A,'''',12X,'  / new source calcode')
 1102 FORMAT (A6,'NEWNUM  = ',I5,12X,'  / new source number')
 1103 FORMAT (A6,'NEWCOORD= ',I2.2,':',I2.2,':',F6.3,1X,A1,I2.2,':',
     *   I2.2,':',F5.2,'  / new source coordinates')
 1110 FORMAT (A6,'NEWNUM  = ',I5,12X,'  / old source number matched')
      END
      SUBROUTINE SUFIXS (NUMVIS, T, S, IRET)
C-----------------------------------------------------------------------
C   Fixes source number if T in specified ranges
C   Inputs:
C      NUMVIS   I       Visibility number
C      T        R       Time in days since 0 IAT on the first day for
C                       which there is data
C      S        R       Source number
C   From Common:
C      TB       R(30)   Start times
C      TE       R(30)   End times
C      NTIMES   I       Number valid times in TB, TE
C      NEWNUM   I       New source number
C   Output:
C      S         R      Source number
C      IRET      I      Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IRET
      REAL      T, S
C
      INTEGER   I
      INCLUDE 'SUFIX.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (NUMVIS.GT.0) THEN
         DO 10 I = 1,NTIMES
            IF ((T.GE.TB(I)) .AND. (T.LE.TE(I))) THEN
               S = NEWNUM
               GO TO 999
               END IF
 10         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE SUCHEK (SNAME, SCODE, SRA, SDEC, DISK, CNO, NEWNUM,
     *   IERR)
C-----------------------------------------------------------------------
C   SUCHEK looks through the source table and finds the highest source
C   number within the table.  It also checks that SNAME does not occur
C   in the table.  If it does it will alter SNAME to avoid a conflict.
C   Inputs:
C      DISK     I      Disk volume number
C      CNO      I      Catalog number
C   In/Out
C      SNAME    C*16   Source name
C   Output:
C      NEWNUM   I      New source number to use
C      IERR     I      Error code
C-----------------------------------------------------------------------
      CHARACTER SNAME*(*), SCODE*(*)
      DOUBLE PRECISION SRA, SDEC
      INTEGER   DISK, CNO, NEWNUM, IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SUBUFF(512), VER, LUN, NUMIF, FREQID, ISURNO, NREC,
     *   SUKOLS(MAXSUC), SUNUMV(MAXSUC), JTRIM, IREC, NFIX, IDSOU,
     *   QUAL
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4, EHEX*36
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION DEPS, FREQO(MAXIF), BANDW, RAEPO, DECEPO, RAAPP,
     *   DECAPP, LSRVEL(MAXIF), LRESTF(MAXIF), PMRA, PMDEC, EPOCH,
     *   RAOBS, DECOBS
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA EHEX /'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C-----------------------------------------------------------------------
      DEPS = 0.1D0 / 3600.0D0
      VER = 1
      LUN = 28
      CALL SOUINI ('READ', SUBUFF, DISK, CNO, VER, CATBLK, LUN, NUMIF,
     *   VELTYP, VELDEF, FREQID, ISURNO, SUKOLS, SUNUMV, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING INPUT SU TABLE'
         GO TO 990
         END IF
      NREC = SUBUFF(5)
      NFIX = 0
      IREC = JTRIM (SNAME)
      IF (SNAME.EQ.' ') SNAME = 'NEW SOURCE'
C                                       read SU table
 10   NEWNUM = 0
      DO 30 IREC = 1,NREC
         ISURNO = IREC
         CALL TABSOU ('READ', SUBUFF, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *      PMDEC, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READING INPUT SU TABLE'
            GO TO 990
            END IF
         NEWNUM = MAX (NEWNUM, IDSOU)
         IF (SNAME.EQ.SOUNAM) THEN
            IF ((SCODE.EQ.CALCOD) .AND. (ABS(SRA-RAEPO).LE.DEPS) .AND.
     *         (ABS(SDEC-DECEPO).LE.DEPS)) THEN
               NEWNUM = -IDSOU
               WRITE (MSGTXT,1010) IDSOU
               CALL MSGWRT (5)
               GO TO 40
               END IF
            WRITE (MSGTXT,1011) SNAME, SOUNAM
            CALL MSGWRT (7)
            NFIX = NFIX + 1
            IF (NFIX.LT.37) THEN
               SNAME(16:16) = EHEX(NFIX:NFIX)
               GO TO 10
            ELSE
               MSGTXT = 'RENAMING HAS FAILED - QUITTING'
               IERR = 10
               GO TO 990
               END IF
            END IF
 30      CONTINUE
C                                       a new number
      NEWNUM = NEWNUM + 1
      WRITE (MSGTXT,1030) NEWNUM
      CALL MSGWRT (3)
C                                       close table
 40   CALL TABSOU ('CLOS', SUBUFF, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *   SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *   EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *   PMDEC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CLOSING INPUT SU TABLE'
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SUCHEK ERROR',I4,' ON ',A)
 1010 FORMAT ('Requested source matches number',I4,
     *   '; will use that one')
 1011 FORMAT ('REQUESTED NAME ''',A,''' MATCHES EXISTING ''',A,'''')
 1030 FORMAT ('Using new source number',I4)
      END
      SUBROUTINE SUPDAT (DISK, CNO, SNAME, SCODE, NEWRA, NEWDEC, NEWNUM,
     *   IERR)
C-----------------------------------------------------------------------
C   SUCHEK looks through the source table and finds the highest source
C   number within the table.  It also checks that SNAME does not occur
C   in the table.  If it does it will alter SNAME to avoid a conflict.
C   Inputs:
C      DISK     I      Disk volume number
C      CNO      I      Catalog number
C      SNAME    C*16   Source name
C      SCODE    C*4    New source calcode
C      NEWRA    D      New source RA
C      NEWDEC   D      New source Dec
C      NEWNUM   I      New source number to use
C   Output:
C      IERR     I      Error code
C-----------------------------------------------------------------------
      CHARACTER SNAME*(*), SCODE*(*)
      INTEGER   DISK, CNO, NEWNUM, IERR
      DOUBLE PRECISION NEWRA, NEWDEC
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SUBUFF(512), VER, LUN, NUMIF, FREQID, ISURNO, NREC,
     *   SUKOLS(MAXSUC), SUNUMV(MAXSUC), IDSOU, QUAL
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION FREQO(MAXIF), BANDW, RAEPO, DECEPO, RAAPP,
     *   DECAPP, LSRVEL(MAXIF), LRESTF(MAXIF), PMRA, PMDEC, EPOCH,
     *   RAOBS, DECOBS
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      VER = 1
      LUN = 28
      CALL SOUINI ('WRIT', SUBUFF, DISK, CNO, VER, CATBLK, LUN, NUMIF,
     *   VELTYP, VELDEF, FREQID, ISURNO, SUKOLS, SUNUMV, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING OUTPUT SU TABLE'
         GO TO 990
         END IF
      NREC = SUBUFF(5)
      ISURNO = NREC + 1
      IDSOU = NEWNUM
      SOUNAM = SNAME
      QUAL = 0
      CALCOD = SCODE
      CALL RFILL (4*NUMIF, 0.0, FLUX)
      CALL DFILL (NUMIF, 0.0D0, FREQO)
      BANDW = 0.0D0
      RAEPO = NEWRA
      DECEPO = NEWDEC
      RAAPP = 0.0D0
      DECAPP = 0.0D0
      EPOCH = CATR(KREPO)
      CALL DFILL (NUMIF, 0.0D0, LSRVEL)
      CALL DFILL (NUMIF, 0.0D0, LRESTF)
      PMRA = 0.0D0
      PMDEC = 0.0
      RAOBS = RAEPO
      DECOBS = DECEPO
      CALL TABSOU ('WRIT', SUBUFF, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *   SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *   EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *   PMDEC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'WRITING OUTPUT SU TABLE'
         GO TO 990
         END IF
      CALL TABSOU ('CLOS', SUBUFF, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *   SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *   EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *   PMDEC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CLOSING OUTPUT SU TABLE'
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SUPDAT ERROR',I4,' ON ',A)
      END
