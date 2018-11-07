LOCAL INCLUDE 'NOIFS.INC'
C                                       Local include for NOIFS
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XNAMOU(3),
     *   XCLAOU(2)
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XBCHAN, XECHAN, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), XDOAC, XSOUT,
     *   XDISO, XCENT, BADD(10),  SCRBUF(256), BUFF2(UVBFSS)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ, ILOCWT,
     *   CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO,
     *   LRECO, NRPRMI, NRPRMO, OLDCNO, NEWCNO, CATOUT(256), OBC(MAXIF),
     *   OINC(MAXIF)
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      DOUBLE PRECISION FRMIN
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XTIME, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF, XBCHAN, XECHAN,
     *   XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER,
     *   XSMOTH, XDOAC, XNAMOU, XCLAOU, XSOUT, XDISO, XCENT, BADD
      COMMON /NOIFSP/ CATOLD, CATOUT, FRMIN, SEQIN, SEQOUT, DISKIN,
     *   DISKO, ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO,
     *   LRECO, NRPRMI, NRPRMO, ISCOMP, OLDCNO, NEWCNO, OBC, OINC
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT
      COMMON /BUFRS/ SCRBUF, BUFF2, JBUFSZ
C                                       End local include for NOIFS
LOCAL END
      PROGRAM NOIFS
C-----------------------------------------------------------------------
C! Replaces all IFs with a single spectrum
C# Utility UV
C-----------------------------------------------------------------------
C;  Copyright (C) 2010-2015, 2017-2018
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
C   NOIFS replaces all IFs with a single UV spectrum, averaging
C   overlapped channels and flagging missing ones.
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
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'NOIFS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'NOIFS '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL NOIFIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL NOIFUV (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL NOIFHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE NOIFIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   NOIFIN gets input parameters for NOIFS and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IRET    I    Error code: 0 => ok
C                               5 => catalog troubles
C                               8 => can't start
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
      INTEGER   IRET
      CHARACTER PRGN*6
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCX, I, NFREQ, LUN, VER, NNIF,
     *   ISBAND(MAXIF)
      LOGICAL   MATCH
      REAL      CATR(256), RPARM(20), FINC(MAXIF)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128), FOFF(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'NOIFS.INC'
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
      IRET = 0
C                                       Get input parameters.
      NPARM = 175
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SCRBUF, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
      STOKES = ' '
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
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
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
         IRET = 1
         GO TO 990
         END IF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, OLDCNO, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FRQSEL, IRET)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         IRET = 1
         GO TO 990
         END IF
      IF (IRET.GT.0) GO TO 999
C                                       get freq information
      VER = 1
      CALL CHNDAT ('READ', BUFF2, DISKIN, OLDCNO, VER, CATBLK, LUN,
     *   NNIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL FNDFRQ (BCHAN, ECHAN, BIF, EIF, FOFF, FINC, OBC, OINC, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL COPY (256, CATBLK, CATOUT)
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, SCRBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1035) IRET
         GO TO 990
         END IF
      CALL UVGET ('CLOS', RPARM, SCRBUF, IERR)
C                                       Save input file info
      INCX = CATBLK(KINAX)
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
      CALL COPY (256, CATOUT, CATBLK)
      IF (JLOCF.LT.0) XCENT = -1.
      IF (XCENT.GT.0.0) THEN
         INCX = CATBLK(KINAX+JLOCF) / 2 + 1
         CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + CATR(KRCIC+JLOCF) *
     *      (INCX-CATR(KRCRP+JLOCF))
         CATR(KRCRP+JLOCF) = INCX
         END IF
      FRMIN = CATD(KDCRV+JLOCF)
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
      IRET = 4
      CALL UVCREA (DISKO, CCNO, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1050) IERR
         ELSE
            WRITE (MSGTXT,1060)
            END IF
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
      NEWCNO = CCNO
C                                       Save output file info
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
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
      IRET = 0
      SEQOUT = CATBLK(KIIMS)
C                                       Copy any header keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
      CALL COPY (256, CATBLK, CATOUT)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NOIFIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY NOT OVERWRITE EXISTING FILE.  QUITTING')
      END
      SUBROUTINE NOIFUV (IRET)
C-----------------------------------------------------------------------
C   NOIFUV sends uv data one point at a time to the user supplied
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
     *   NUMVIS, XCOUNT, NCORO, NCOPY, RNXRET
      LOGICAL   T, F
      INCLUDE 'NOIFS.INC'
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
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NCOPY = LRECO - NRPRMO
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
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
      CALL RNXINI (DISKO, NEWCNO, CATOUT, RNXRET)
      IF ((FRMIN.GT.0.0D0) .AND. (UVFREQ.GT.0.0D0)) THEN
         UVSCAL = FRMIN / UVFREQ
      ELSE
         MSGTXT = 'WARNING: UVW SCALING INDETERMINATE'
         CALL MSGWRT (7)
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
         CALL NOIFDO (VIS, RESULT)
C                                       Copy to output.

         XCOUNT = XCOUNT + 1.0D0
         CALL RCOPY (NRPRMI, RPARM, BUFF2(IPTRO))
C                                       update NX table
         CALL RNXUPD (RPARM, RNXRET)
C                                       Compressed
         IF (ISCOMP) THEN
            CALL ZUVPAK (NCORO, RESULT, BUFF2(IPTRO+ILOCWT),
     *         BUFF2(IPTRO+NRPRMO))
         ELSE
            CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
            END IF
         IPTRO = IPTRO + LRECO
         NIOUT = NIOUT + 1
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
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATOUT, IRET)
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
 1000 FORMAT ('NOIFUV: ERROR',I3,' OPEN/INIT INPUT VIS FILE')
 1010 FORMAT ('NOIFUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('NOIFUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1100 FORMAT ('NOIFUV: ERROR',I3,' READING VIS FILE')
 1150 FORMAT ('NOIFUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE NOIFHI
C-----------------------------------------------------------------------
C   NOIFHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72
      INTEGER   LUN1, LUN2, IERR, I, J
      INCLUDE 'NOIFS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA LUN1, LUN2 /27,28/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATOUT,
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
C                                       channel positions
      DO 20 I = BIF,EIF
         J = I - BIF + 1
         WRITE (HILINE,1010) TSKNAM, I, OBC(J), OINC(J), I
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
 20      CONTINUE
C                                       Close HI file
 100  CALL HICLOS (LUN2, .TRUE., BUFF2, IERR)
C                                       Copy tables: local version
      CALL COPTAB (DISKIN, OLDCNO, DISKO, NEWCNO, CATOUT, OBC, OINC,
     *   IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'NOIFHI: ERROR COPYING TABLES TO OUTPUT UV'
         CALL MSGWRT (6)
         END IF
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATOUT, 'REST', SCRBUF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NOIFHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A,'CHANS(',I2,') =',I6,',',I3,' / channel start, incr',
     *   ' IF',I3)
      END
      SUBROUTINE NOIFDO (VIS, RESULT)
C-----------------------------------------------------------------------
C   This does the spectral building
C   Inputs:
C      VIS     R(3,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C   Inputs from COMMON:
C      CATBLK     I(256)  Catalog header record. See Going Aips for
C                         details.
C      INCSI      I    Input Stokes' increment in vis.
C      INCFI      I    Input frequency increment in vis.
C      INCIFI     I    Input IF increment in vis.
C   Output:
C      RESULT     R(3,*) Output visibilities selected in frequency.
C-----------------------------------------------------------------------
      REAL      VIS(3,*), RESULT(3,*)
C
      INTEGER   JIF, JF, JS, NIF, NF, NS, INDEXO, INDEXI, NT
      REAL      W
      INCLUDE 'NOIFS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       pointers to traverse the data
      NS = 1
      NIF = 1
      NF = 1
      IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
      IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
      IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
      NT = NS * CATOUT(KINAX+JLOCF)
      CALL RFILL (3*NT, 0.0, RESULT)
      DO 40 JS = 1,NS
         DO 30 JIF = 1,NIF
            DO 20 JF = 1,NF
               INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *            (JS-1) * INCSI + 1
               W = VIS(3,INDEXI)
               IF (W.GT.0.0) THEN
                  INDEXO = JS + ((JF-1)*OINC(JIF) + OBC(JIF) - 1) * NS
                  RESULT(1,INDEXO) = RESULT(1,INDEXO) + W*VIS(1,INDEXI)
                  RESULT(2,INDEXO) = RESULT(2,INDEXO) + W*VIS(2,INDEXI)
                  RESULT(3,INDEXO) = RESULT(3,INDEXO) + W
                  END IF
 20            CONTINUE
 30         CONTINUE
 40      CONTINUE
      DO 50 INDEXO = 1,NT
         W = RESULT(3,INDEXO)
         IF (W.GT.0) THEN
            RESULT(1,INDEXO) = RESULT(1,INDEXO) / W
            RESULT(2,INDEXO) = RESULT(2,INDEXO) / W
            END IF
 50      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE COPTAB (DISKIN, CNOIN, DISKOU, CNOOUT, CATO, OBC, OINC,
     *   IRET)
C-----------------------------------------------------------------------
C   Updates tables for selection by IF etc
C   NOIFS version changes a lot!
C   Inputs:
C      DISKIN   I   Input disk number
C      CNOIN    I   Input catalog number
C      DISKOU   I   Output disk number
C      CNOOUT   I   Output catalog number
C   Inputs in common:
C      BIF   I  First IF
C      EIF   I  Highest IF selected
C      FQOFF D  Frequency offset
C      SELIF L  Select IFs or not
C   Output:
C      IRET  I  Return code, 0=>OK
C-----------------------------------------------------------------------
      INTEGER   DISKIN, CNOIN, DISKOU, CNOOUT, CATO(256), OBC(*),
     *   OINC(*), IRET
C
      INCLUDE 'INCS:DSEL.INC'
      REAL      FINC(MAXIF), CATOR(256)
      LOGICAL   TABLE, EXIST, FITASC, SELIF, ISBAND(MAXIF), MULTI
      CHARACTER NOTTYP(21)*2
      INTEGER   IERR, LUN1, LUN2, VER, NVER, OFQID, ISUB, JSUB, AN(50),
     *   NA, NNIF, I, FREQID, BUFF1(512), BUFF2(512), ISURNO, SIDSOU,
     *   SQUAL, SUFQID, NSOURC, INOGRP, KOLS(MAXSUC), NUMV(MAXSUC),
     *   NONOT, EIFSAV, CATOUT(256), NUMF, BPOL, EPOL, BVER
      CHARACTER VELTYP*8, VELDEF*8, SSNAME*16, SCALCO*4, BNDCOD(MAXIF)*8
      DOUBLE PRECISION SBANDW, SRAEPO, SDECEP, SEPOCH, SRAAPP, SDECAP,
     *   SPMRA, SPMDEC, SLSRVE(MAXIF), SFREQO(MAXIF), SLREST(MAXIF),
     *   TIME1, TIME2, FOFF(MAXIF), FQOFF, CATOD(128), SRAOBS, SDECOB
      REAL     SFLUX(4,MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATOUT, CATOR, CATOD)
      DATA LUN1, LUN2 /28, 29/
      DATA NONOT, NOTTYP /21, 'NX','FQ','CH','CL','SN','SU','FG','BP',
     *   'IM','CQ','PC','TY','GC','MC','WX','BL','AN','CP','PD','SY',
     *   'CD'/
C-----------------------------------------------------------------------
C                                       Single source now?
      CALL COPY (256, CATO, CATOUT)
      MULTI = ILOCSU.GT.0
      EIFSAV = EIF
      EIF = BIF
C                                       Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKOU, CNOIN,
     *   CNOOUT, CATOUT, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'FLGIHI: ERROR COPYING TABLES'
         CALL MSGWRT (6)
         END IF
C                                       more complex tables
      MSGTXT = 'Updating tables for IF/FREQID/channel selection'
      CALL MSGWRT (4)
      ISUB = 0
      JSUB = -1
      NA = 0
C                                       STOKES selection not allowed
C                                       these ok even for 1 pol data
      BPOL = 1
      EPOL = 2
C                                       allow 15 min extra to be sure
C                                       that one gets all needed rows
      TIME1 = TSTART - 0.0104D0
      TIME2 = TEND + 0.0104D0
      SELIF = (BIF.GT.1) .OR. (EIF.LT.CATUV(KINAX+KLOCIF)) .OR.
     *   (FRQSEL.GT.0)
      CALL FILL (50, 0, AN)
C                                       FQ table
      IF (JLOCIF.GT.0) THEN
         CALL DFILL (MAXIF, 0.0D0, SFREQO)
C                                       Multi to single source
         IF ((KLOCSU.GE.0) .AND. (ILOCSU.LT.0)) THEN
C                                       Open file
            CALL SOUINI ('READ', BUFF2, IUDISK, IUCNO, 1, CATUV, LUN1,
     *         INOGRP, VELTYP, VELDEF, SUFQID, I, KOLS, NUMV, IRET)
            IF (IRET.NE.0) GO TO 20
C                                       Get number of sources.
            NSOURC = BUFF2(5)
C                                       Loop looking for source.
            DO 10 I = 1,NSOURC
               ISURNO = I
               CALL TABSOU ('READ', BUFF2, ISURNO, KOLS, NUMV, SIDSOU,
     *            SSNAME, SQUAL, SCALCO, SFLUX, SFREQO, SBANDW, SRAEPO,
     *            SDECEP, SEPOCH, SRAAPP, SDECAP, SRAOBS, SDECOB,
     *            SLSRVE, SLREST, SPMRA, SPMDEC, IRET)
               IF (IRET.GT.0) CALL DFILL (MAXIF, 0.0D0, SFREQO)
               IF ((SIDSOU.EQ.SOUWAN(1)) .OR. (IRET.GT.0)) GO TO 15
 10            CONTINUE
C                                       Didn't find
            CALL DFILL (MAXIF, 0.0D0, SFREQO)
C                                       Close file
 15         CALL TABIO ('CLOS', 0, I, BUFF2, BUFF2, IRET)
            END IF
C                                       Read old
 20      VER = 1
         CALL CHNDAT ('READ', BUFF1, DISKIN, CNOIN, VER, CATUV, LUN1,
     *      NNIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Fixup
         NNIF = 1
         FOFF(1) = 0.0D0
C                                       Output ref IF = 1
         CATOD(KDCRV+JLOCIF) = 1.0D0
         CATOR(KRCRP+JLOCIF) = 1.0
         FINC(1) = CATOR(KRCIC+JLOCF)
C                                       Rewrite new
         VER = 1
         FREQID = 1
         CALL CHNDAT ('WRIT', BUFF1, DISKOU, CNOOUT, VER, CATOUT, LUN1,
     *      NNIF, FOFF, ISBAND(BIF), FINC, BNDCOD, FREQID, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Reference frequency in AN table
C                                       IF selection
      CALL FNDEXT ('AN', CATUV, NVER)
      DO 100 VER = 1,NVER
         CALL ISTAB ('AN', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF ((EXIST) .AND. (IERR.EQ.0)) CALL ANSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, VER, CATUV, CATOUT, LUN1, LUN2, BIF,
     *      EIF, FQOFF, DOPOL, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 100     CONTINUE
C                                       Revise tables:
      OFQID = FRQSEL
C                                       CD tables
      CALL FNDEXT ('CD', CATUV, NVER)
      DO 145 VER = 1,NVER
         CALL ISTAB ('CD', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
C                                       do not select on antenna
         IF (EXIST.AND.(IERR.EQ.0)) CALL CDSEL (DISKIN, CNOIN, DISKOU,
     *      CNOOUT, VER, CATUV, CATOUT, LUN1, LUN2, BPOL, EPOL, BIF,
     *      EIF, OFQID, AN, 0, ISUB, JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 145     CONTINUE
C                                       CL tables
      CALL FNDEXT ('CL', CATUV, NVER)
      IF (.NOT.MULTI) NVER = 0
C                                       write null table
      VER = 1
      CALL ISTAB ('CL', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *   EXIST, FITASC, IERR)
C                                       do NOT select on sources
      IF (EXIST .AND. (IERR.EQ.0)) THEN
         IF (MULTI) THEN
            CALL CLNULL (DISKIN, CNOIN, DISKOU, CNOOUT, VER, CATUV,
     *         CATOUT, LUN1, LUN2, BPOL, EPOL, BIF, EIF, OFQID, TIME1,
     *         TIME2, 0, SOUWAN, AN, NA, ISUB, JSUB, BUFF1, BUFF2, IRET)
         ELSE
            CALL CL2FO (DISKIN, CNOIN, VER, LUN1, CATUV, DISKOU, CNOOUT,
     *         LUN2, CATOUT, SOUWAN, BUFF1, BUFF2, IRET)
            END IF
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       CP tables
      CALL FNDEXT ('CP', CATUV, NVER)
      NUMF = CATOUT(KINAX+JLOCF)
      DO 150 VER = 1,NVER
         CALL ISTAB ('CP', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST.AND.(IERR.EQ.0)) CALL CPSEL (DISKIN, CNOIN, DISKOU,
     *      CNOOUT, VER, CATUV, CATOUT, LUN1, LUN2, BIF, EIF, BCHAN,
     *      ECHAN, OFQID, OBC, OINC, NUMF, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 150     CONTINUE
C                                       CQ tables
      CALL FNDEXT ('CQ', CATUV, NVER)
      DO 160 VER = 1,NVER
         CALL ISTAB ('CQ', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST.AND.(IERR.EQ.0)) CALL CQSEL (DISKIN, CNOIN, DISKOU,
     *      CNOOUT, VER, CATUV, CATOUT, LUN1, LUN2, BIF, EIF, OFQID,
     *      ISUB, JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 160     CONTINUE
C                                       FG tables
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
         CALL ISTAB ('FG', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0) .AND. (VER.GT.FGVER))
     *      CALL FGNSEL (DISKIN, CNOIN, DISKOU, CNOOUT, VER, CATUV,
     *      CATOUT, LUN1, LUN2, BIF, EIF, BCHAN, ECHAN, TIME1, TIME2,
     *      OFQID, ISUB, JSUB, OINC, OBC, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 180     CONTINUE
C                                       GC tables
      CALL FNDEXT ('GC', CATUV, NVER)
      DO 200 VER = 1,NVER
         CALL ISTAB ('GC', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL GCSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATOUT, LUN1, LUN2, BPOL, EPOL,
     *      BCHAN, ECHAN, BIF, EIF, OFQID, AN, NA, ISUB, JSUB, BUFF1,
     *      BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 200     CONTINUE
C                                       IM tables
      CALL FNDEXT ('IM', CATUV, NVER)
      DO 220 VER = 1,NVER
         CALL ISTAB ('IM', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL IMSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATOUT, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, NSOUWD, SOUWAN, AN, NA, ISUB,
     *      JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 220     CONTINUE
C                                       MC tables
      CALL FNDEXT ('MC', CATUV, NVER)
      DO 240 VER = 1,NVER
         CALL ISTAB ('MC', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL MCSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATOUT, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, NSOUWD, SOUWAN, AN, NA, ISUB,
     *      JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 240     CONTINUE
C                                       PC tables
      CALL FNDEXT ('PC', CATUV, NVER)
      DO 260 VER = 1,NVER
         CALL ISTAB ('PC', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL PCSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATOUT, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, NSOUWD, SOUWAN, AN, NA, ISUB,
     *      JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 260     CONTINUE
C                                       SU tables
      IF (ILOCSU.GE.0) THEN
         CALL FNDEXT ('SU', CATUV, NVER)
         DO 300 VER = 1,NVER
            CALL ISTAB ('SU', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *         EXIST, FITASC, IERR)
            IF (EXIST .AND. (IERR.EQ.0)) CALL SUSEL (DISKIN, CNOIN,
     *         DISKOU, CNOOUT, VER, CATUV, CATOUT, LUN1, LUN2, BIF, EIF,
     *         OFQID, BUFF1, BUFF2, IRET)
            IF (IRET.GT.0) GO TO 999
 300        CONTINUE
         END IF
C                                       SY tables
      CALL FNDEXT ('SY', CATUV, NVER)
      DO 310 VER = 1,NVER
         CALL ISTAB ('SY', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL SYSEL (DISKIN, CNOIN, DISKOU,
     *      CNOOUT, VER, CATUV, CATOUT, LUN1, LUN2, BPOL, EPOL, BIF,
     *      EIF, OFQID, TIME1, TIME2, NSOUWD, SOUWAN,  AN, NA, ISUB,
     *      JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 310     CONTINUE
C                                       TY tables
      CALL FNDEXT ('TY', CATUV, NVER)
      DO 320 VER = 1,NVER
         CALL ISTAB ('TY', DISKIN, CNOIN, VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL TYSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATOUT, LUN1, LUN2, BPOL, EPOL,
     *      BIF, EIF, OFQID, TIME1, TIME2, AN, NA, ISUB, JSUB, BUFF1,
     *      BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 320     CONTINUE
C                                       WX tables
      CALL FNDEXT ('WX', CATUV, NVER)
      DO 340 VER = 1,NVER
         CALL ISTAB ('WX', DISKIN, FCNO(2), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL WXSEL (DISKIN, CNOIN,
     *      DISKOU, CNOOUT, VER, CATUV, CATOUT, LUN1, LUN2, TIME1,
     *      TIME2, AN, NA, ISUB, JSUB, BUFF1, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 340     CONTINUE
C
      EIF = EIFSAV
      CALL COPY (256, CATOUT, CATO)
C
 999  RETURN
C-----------------------------------------------------------------------
 1160 FORMAT ('WARNING: ONLY FG TABLE VERSIONS',I4,' TO',I4,' COPIED')
      END
      SUBROUTINE FNDFRQ (BCHAN, ECHAN, BIF, EIF, FOFF, FINC, OBC, OINC,
     *   IRET)
C-----------------------------------------------------------------------
C   plan the output frequency axis
C   Inputs:
C      BCHAN    I      begin channel
C      ECHAN    I      end channel
C      BIF      I      begin IF
C      EIF      I      end if
C      FOFF     D(*)   Frequency offsets
C      FINC     R(*)   Frequency increments
C   In/out common:
C      CATBLK   I(*)   Header in/ output header desired
C   Output:
C      OBC      I(*)   Start channel for each input IF
C      OINC     I(*)   Increment for each input IF (usually 1)
C      IRET     I      > 0 => no match found
C-----------------------------------------------------------------------
      INTEGER   BCHAN, ECHAN, BIF, EIF, OBC(*), OINC(*), IRET
      DOUBLE PRECISION FOFF(*)
      REAL      FINC(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   I, IROUND, J
      REAL      FMIN, TEMP
      DOUBLE PRECISION FRBCN(MAXIF), FRMIN, FRMAX, FRECN(MAXIF)
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      FMIN = 1.E10
      FRMIN = 1.E20
      FRMAX = -FRMIN
      DO 10 I = BIF,EIF
         FMIN = MIN (ABS(FINC(I)), FMIN)
         FRBCN(I) = CATD(KDCRV+JLOCF) + FOFF(I) + FINC(I) *
     *      (BCHAN-CATR(KRCRP+JLOCF))
         FRECN(I) = CATD(KDCRV+JLOCF) + FOFF(I) + FINC(I) *
     *      (ECHAN-CATR(KRCRP+JLOCF))
         FRMIN = MIN (FRMIN, FRBCN(I))
         FRMIN = MIN (FRMIN, FRECN(I))
         FRMAX = MAX (FRMAX, FRBCN(I))
         FRMAX = MAX (FRMAX, FRECN(I))
 10      CONTINUE
      IF (FMIN.LE.0.0) THEN
         MSGTXT = 'FREQ INCREMENT ERROR'
         GO TO 990
         END IF
C                                       check and position IFs
      DO 20 I = BIF,EIF
         J = IROUND (FINC(I)/FMIN)
         TEMP = ABS (FINC(I) - J*FMIN)
         IF (TEMP.GT.0.05*FMIN) THEN
            WRITE (MSGTXT,1010) I
            GO TO 990
            END IF
         OINC(I-BIF+1) = J
         TEMP = (FRBCN(I) - FRMIN) / FMIN
         J = IROUND (TEMP)
         IF (ABS(J-TEMP).GT.0.25) THEN
            WRITE (MSGTXT,1015) I
            GO TO 990
            END IF
         TEMP = (FRECN(I) - FRMIN) / FMIN
         J = IROUND (TEMP)
         IF (ABS(J-TEMP).GT.0.25) THEN
            WRITE (MSGTXT,1015) I
            GO TO 990
            END IF
         J = (FRBCN(I) - FRMIN) / FMIN + 1.5D0
         OBC(I-BIF+1) = J
 20      CONTINUE
C                                       header
      J = (FRMAX - FRMIN) / FMIN + 1.5D0
      CATD(KDCRV+JLOCF) = FRMIN
      CATR(KRCIC+JLOCF) = FMIN
      CATR(KRCRP+JLOCF) = 1.0
      CATBLK(KINAX+JLOCF) = J
      IF (J.GT.MAXCHA) THEN
         WRITE (MSGTXT,1020) J
         GO TO 990
         END IF
      IF (JLOCIF.GE.0) THEN
         CATD(KDCRV+JLOCIF) = 1.0D0
         CATR(KRCIC+JLOCIF) = 1.0
         CATR(KRCRP+JLOCIF) = 1.0
         CATBLK(KINAX+JLOCIF) = 1
         END IF
      IRET = 0
      GO TO 999
C
 990  IRET = 8
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('FNDFRQ: AXIS',I2,' INCREMENT UNUSABLE')
 1015 FORMAT ('FNDFRQ: AXIS',I2,' CENTER FREQUENCY NOT ON CHANNEL')
 1020 FORMAT ('FNDFRQ: NUMBER CHANNELS OUT',I7,' IS TOO LARGE')
      END
      SUBROUTINE CPSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BIF, EIF, BCHAN, ECHAN, IFQID, OBC, OINC, NUMF,
     *   BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C   Copies and renumbers the IFs and channels in an CP table; can also
C   modify the FQ ID.
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
C      BIF      I        Start IF number
C      EIF      I        End IF number
C      BCHAN    I        First channel
C      ECHAN    I        Last channel
C      IFQID    I        FQ ID to select (output FQID will be 1)
C                          if <= 0 then output FQ ID unchanged.
C   Input/Output:
C      BUFFER   I(512)   Work buffer
C      OBUFF    I(512)   Work buffer
C   Output:
C      IRET     I        Error, 0 => OK, -1 => okay but no copy
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BIF, EIF, BCHAN, ECHAN, IFQID, OBC(*), OINC(*),
     *   NUMF, BUFFER(*), OBUFF(*), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXCPC
      PARAMETER (MAXCPC = 6)
C
      REAL      CFLUX(4,MAXCIF), DFLUX(4,MAXCIF)
      INTEGER   ICPRNO, CPKOLS(MAXCPC), CPNUMV(MAXCPC), OKOLS(MAXCPC),
     *   ONUMV(MAXCPC), NCPROW, OCPRNO, FREQID, NUMIF, NUMFRQ, I,
     *   J, K, JJ, NEWIF, NEWFRQ, IFA, IFB, IFREQA, IFREQB, INX, LNX,
     *   OVER, SUID, FOPEN, IFLUX(MAXCIF), NF
      LOGICAL   REFMT
      CHARACTER SOURCE*16
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      REFMT = .FALSE.
      FOPEN = 0
C                                       Open CP file
      CALL CPINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI, ICPRNO,
     *   CPKOLS, CPNUMV, NUMIF, NUMFRQ, FREQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN INPUT TABLE'
         GO TO 990
         END IF
      IF (IFQID.GT.0) THEN
         IF ((FREQID.GT.0) .AND. (FREQID.NE.IFQID)) THEN
            CALL TABIO ('CLOS', 0, ICPRNO, BUFFER, BUFFER, IRET)
            IRET = -1
            GO TO 999
         ELSE
            REFMT = FREQID.NE.1
            FREQID = 1
            END IF
         END IF
      FOPEN = 1
C                                       new channel numbers
      IFREQA = MAX (BCHAN, 1)
      IFREQB = MIN (NUMFRQ, ECHAN)
      NF = IFREQB - IFREQA + 1
      NEWFRQ = NUMF
C                                       New CP table will cover
C                                       IF range [IFA,IFB].
      IFA = MAX (BIF, 1)
      IFB = MIN (NUMIF, EIF)
      NEWIF = 1
      IF (NEWFRQ.NE.NUMFRQ*NUMIF) REFMT = .TRUE.
C                                       # rows in old table
      NCPROW = BUFFER(5)
C                                       Open up new CP table
      OVER = VER
      CALL CPINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OCPRNO, OKOLS, ONUMV, NEWIF, NEWFRQ, FREQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT TABLE'
         GO TO 990
         END IF
      FOPEN = 2
C                                       Loop and copy
      DO 300 I = 1,NCPROW
         CALL TABCP ('READ', BUFFER, ICPRNO, CPKOLS, CPNUMV, NUMIF,
     *      NUMFRQ, SOURCE, SUID, CFLUX, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ OLD TABLE'
            GO TO 990
C                                       Copy this record
         ELSE
            CALL RFILL (4*NUMF, 0.0, DFLUX)
            CALL FILL (NUMF, 0, IFLUX)
            DO 30 J = 1,NUMIF
               INX = OBC(J) - OINC(J)
               JJ = BIF + J - 1
               LNX = (JJ - 1) * NUMFRQ + IFREQA - 1
               IF (INX.NE.LNX) REFMT = .TRUE.
C                                       Renumber IFs/FREQ's
               DO 20 K = 1,NF
                  INX = INX + OINC(J)
                  LNX = LNX + 1
                  DFLUX(1,INX) = DFLUX(1,INX) + CFLUX(1,LNX)
                  DFLUX(2,INX) = DFLUX(2,INX) + CFLUX(2,LNX)
                  DFLUX(3,INX) = DFLUX(3,INX) + CFLUX(3,LNX)
                  DFLUX(4,INX) = DFLUX(4,INX) + CFLUX(4,LNX)
                  IFLUX(INX) = IFLUX(INX) + 1
 20               CONTINUE
 30            CONTINUE
            DO 40 J = 1,NEWFRQ
               IF (IFLUX(J).GT.1) THEN
                  DFLUX(1,J) = DFLUX(1,J) / IFLUX(J)
                  DFLUX(2,J) = DFLUX(2,J) / IFLUX(J)
                  DFLUX(3,J) = DFLUX(3,J) / IFLUX(J)
                  DFLUX(4,J) = DFLUX(4,J) / IFLUX(J)
                  END IF
 40            CONTINUE
C                                       Write output record.
            CALL TABCP ('WRIT', OBUFF, OCPRNO, OKOLS, ONUMV, NEWIF,
     *         NEWFRQ, SOURCE, SUID, DFLUX, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE NEW TABLE'
               GO TO 990
               END IF
            END IF
 300     CONTINUE
C                                       Close both tables
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1300) 'Reformatted CP', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1300) 'Copied CP', DISKI, CNOI, VER, DISKO,
     *         CNOO, OVER
            END IF
         CALL MSGWRT (3)
         END IF
      GO TO 995
C                                       Error
 990  CALL MSGWRT (6)
C                                       close
 995  IF (FOPEN.GT.0) CALL TABIO ('CLOS', 0, ICPRNO, BUFFER, BUFFER, I)
      IF (FOPEN.GT.1) CALL TABIO ('CLOS', 0, OCPRNO, OBUFF, OBUFF, J)
      IF (IRET.EQ.0) IRET = MAX (I, J)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CPSEL: ERROR ',I3,1X,A)
 1300 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
      END
      SUBROUTINE FGNSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BIF, EIF, BCHAN, ECHAN, TB, TE, IFQID, ISUB, JSUB,
     *   OINC, OBC, BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C   Copies and renumbers the IFs in an FG table, can also modify the
C   FQ ID.
C   NOIFS version - redoes the channels and IFs
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
C      BCHAN           I       First channel
C      ECHAN           I       Last channel
C      TB       D        Beginning time in days
C      TE       D        Ending time in days
C      IFQID           I       FQ ID to select (set to 1 on output)
C                              if <= 0 then output value unchanged.
C      ISUB     I        Selected subarray (= 0 any)
C      JSUB     I        Output subarray number (< 0 => NO CHANGE)
C   Input/Output:
C      BUFFER          I(*)    Work buffer
C      OBUFF           I(*)    Work buffer
C   Output:
C      IRET            I       Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BIF, EIF, BCHAN, ECHAN, IFQID, ISUB, JSUB,
     *   OINC(*), OBC(*), BUFFER(*), OBUFF(*), IRET
      DOUBLE PRECISION TB, TE
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER REASON*24
      INTEGER   OVER, IFGRNO, FGKOLS(MAXFGC), FGNUMV(MAXFGC), NCHAN,
     *   OKOLS(MAXFGC), ONUMV(MAXFGC), NFGROW, I, OFGRNO, LBIF, LEIF,
     *   SOURID, SUBA, FREQID, ANTS(2), IFS(2), CHANS(2), J
      LOGICAL PFLAGS(4), DROP, REFMT
      REAL    TIMER(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Open FG file
      CALL FLGINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI,
     *  IFGRNO, FGKOLS, FGNUMV, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       # rows in old table
      NFGROW = BUFFER(5)
      REFMT = JSUB.GE.0
C                                       Open up new FG table
      OVER = VER
      CALL FLGINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *  OFGRNO, OKOLS, ONUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1, NFGROW
         CALL TABFLG ('READ', BUFFER, IFGRNO, FGKOLS, FGNUMV,
     *      SOURID, SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS,
     *      REASON, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
         DROP = IRET.LT.0
C                                       Is this FQ ID selected ?
         DROP = DROP .OR. ((IFQID.GT.0) .AND. (IFQID.NE.FREQID) .AND.
     *      (FREQID.GT.0))
C                                       Is this FQ ID selected ?
         DROP = DROP .OR. ((ISUB.GT.0) .AND. (ISUB.NE.SUBA) .AND.
     *      (SUBA.GT.0))
C                                       timerange
         DROP = DROP .OR. (TB.GT.TIMER(2)) .OR. (TE.LT.TIMER(1))
C                                       IFs, channels
         DROP = DROP .OR. ((CHANS(1).GT.0) .AND. (CHANS(1).GT.ECHAN))
         DROP = DROP .OR. ((CHANS(2).GT.0) .AND. (CHANS(2).LT.BCHAN))
         DROP = DROP .OR. ((IFS(1).GT.0) .AND. (IFS(1).GT.EIF))
         DROP = DROP .OR. ((IFS(2).GT.0) .AND. (IFS(2).LT.BIF))
C                                       May ignore some records
         IF (DROP) THEN
            REFMT = .TRUE.
C                                       Write new one
         ELSE
            IF (IFQID.GT.0) FREQID = 1
            IF (JSUB.GE.0) SUBA = JSUB
C                                       renumber IFs and channels
            IFS(1) = MAX (BIF, IFS(1))
            IFS(2) = MIN (EIF, IFS(2))
            IF (IFS(2).LT.IFS(1)) IFS(2) = EIF
            CHANS(1) = MAX (BCHAN, CHANS(1))
            CHANS(2) = MIN (ECHAN, CHANS(2))
            IF (CHANS(2).LT.CHANS(1)) CHANS(2) = ECHAN
            NCHAN = (CHANS(2) - CHANS(1))
            LBIF = IFS(1)
            LEIF = IFS(2)
            IFS(1) = 1
            IFS(2) = 1
            DO 90 J = LBIF,LEIF
               CHANS(1) = OBC(J-BIF+1)
               CHANS(2) = CHANS(1) + NCHAN * OINC(J-BIF+1)
               CALL TABFLG ('WRIT', OBUFF, OFGRNO, OKOLS, ONUMV, SOURID,
     *            SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1020) IRET
                  GO TO 990
                  END IF
 90            CONTINUE
            END IF
 100     CONTINUE
C                                       Close both tables
      CALL TABIO ('CLOS', 0, IFGRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OFGRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted FG', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied FG', DISKI, CNOI, VER, DISKO,
     *         CNOO, OVER
            END IF
         CALL MSGWRT (3)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('FGSEL: ERROR ',I3,' RETURNED FROM FLGINI')
 1020 FORMAT ('FGSEL: ERROR ',I3,' RETURNED FROM TABFLG')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
      END
