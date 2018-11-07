LOCAL INCLUDE 'FLOPM.INC'
C                                       Local include for FLOPM
      INCLUDE 'INCS:ZPBUFSZ.INC'
      DOUBLE PRECISION UVSCAL
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XNAMOU(3),
     *   XCLAOU(2), XOPTYP
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XDOBND, XBPVER, XSMOTH(3), XDOAC, XSOUT, XDISO,
     *   BADD(10),   SCRBUF(256), BUFF2(UVBFSS), STARTT, ENDT
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ, ILOCWT,
     *   CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO,
     *   LRECO, NRPRMI, NRPRMO, OLDCNO, NEWCNO
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, OPTYPE*4
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XTIME, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XDOAC,
     *   XOPTYP, XNAMOU, XCLAOU, XSOUT, XDISO, BADD
      COMMON /FLOPMP/ CATOLD, UVSCAL, SEQIN, SEQOUT, DISKIN, DISKO,
     *   ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECO,
     *   NRPRMI, NRPRMO, ISCOMP, OLDCNO, NEWCNO, STARTT, ENDT
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, OPTYPE
      COMMON /BUFRS/ SCRBUF, BUFF2, JBUFSZ
C                                       End local include for FLOPM
LOCAL END
      PROGRAM FLOPM
C-----------------------------------------------------------------------
C! Allows user to provide subroutine to operate on UV data base
C# Utility UV UV-util VLA VLB Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2007, 2009-2012, 2015, 2018
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
C   FLOPM is a special purpose task to reverse the order of the spectral
C   channels and the sign of the phase.
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
      INCLUDE 'FLOPM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'FLOPM '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL FLOPMI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL FLOPMU (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL FLOPMH
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE FLOPMI (PRGN, JERR)
C-----------------------------------------------------------------------
C   FLOPMI gets input parameters for FLOPM and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCX, I, NFREQ, LUN, K
      LOGICAL   MATCH
      REAL      CATR(256), RPARM(20)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128), INFREQ
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'FLOPM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DOUBLE PRECISION FOFF(MAXIF)
      INTEGER   ISBAND(MAXIF)
      REAL      FINC(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
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
      NPARM = 173
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
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
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
      CALL RFILL (8, 0.0, TIMRNG)
      TEND = 9999999.
      TSTART = -TEND
      IF ((XTIME(1)+XTIME(2)+XTIME(3)+XTIME(4)) .EQ.0.0)
     *   XTIME(1)=-1.0E6
      IF ((XTIME(5)+XTIME(6)+XTIME(7)+XTIME(8)) .EQ.0.0)
     *   XTIME(5)=1.0E6
      STARTT = XTIME(1) + XTIME(2) / 24. + XTIME(3) / (24. * 60.) +
     *   XTIME(4) / (24. * 60. * 60.)
      ENDT = XTIME(5) + XTIME(6) / 24. + XTIME(7) / (24. * 60.) +
     *   XTIME(8) / (24. * 60. * 60.)
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
      INFREQ = FREQ
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
      BCHAN = 1
      ECHAN = NFREQ
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
C                                       get freq info
      IF ((OPTYPE.EQ.'IFTO') .OR. (OPTYPE.EQ.'IFON')) THEN
         I = 1
         CALL CHNDAT ('READ', BUFF2, DISKIN, OLDCNO, I, CATBLK, LUN, K,
     *      FOFF, ISBAND, FINC, BNDCOD, FRQSEL, JERR)
         IF (JERR.GT.0) THEN
            MSGTXT = 'ERROR READING FQ FILE'
            GO TO 990
            END IF
         UVSCAL = (FREQ + FOFF(EIF)) / (FREQ + FOFF(BIF))
      ELSE
         UVSCAL = 1.0D0
         END IF
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
C                                       header corrections
      IF (OPTYPE.NE.'VLAE') THEN
         IF (OPTYPE.NE.'IFON') THEN
            CATR(KRCIC+JLOCF) = -CATR(KRCIC+JLOCF)
            CATR(KRCRP+JLOCF) = CATBLK(KINAX+JLOCF) + 1 -
     *         CATR(KRCRP+JLOCF)
            CATR(KRARP) = CATBLK(KINAX+JLOCF) + 1 - CATR(KRARP)
            END IF
         CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) * UVSCAL
         END IF
C                                       actual scaling
      UVSCAL = CATD(KDCRV+JLOCF) / INFREQ
C                                       Put input file in READ
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
 1000 FORMAT ('FLOPMI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('FLOPMI: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE FLOPMU (IRET)
C-----------------------------------------------------------------------
C   FLOPMU sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER OFILE*48
      INTEGER   IPTRO, LUNO, INDO, ILENBU, KBIND, NIOUT, NIOLIM, BO, VO,
     *   NUMVIS, XCOUNT, NCORO, NCOPY, CATMP(256), RNXRET
      LOGICAL   T, F
      INCLUDE 'FLOPM.INC'
      REAL      VIS(UVBFSS), RESULT(UVBFSS), RPARM(20)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
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
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
C                                       Loop over buffer
      ELSE IF (IRET.EQ.0) THEN
         NUMVIS = NUMVIS + 1
C                                       call user routine
         CALL FLOPMD (NUMVIS, RPARM(1+ILOCT), VIS, RESULT, IRET)
C                                       Error (fatal)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1120) IRET
            GO TO 990
C                                       Copy to output.
         ELSE IF (IRET.EQ.0) THEN
            XCOUNT = XCOUNT + 1.0D0
            RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
            RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
            RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
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
C                                       Final call to FLOPMD.
      NUMVIS = -1
      CALL FLOPMD (NUMVIS, RPARM(1+ILOCT), SCRBUF, RESULT, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
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
 1000 FORMAT ('FLOPMU: ERROR',I3,' OPEN/INIT INPUT VIS FILE')
 1010 FORMAT ('FLOPMU: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('FLOPMU: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1100 FORMAT ('FLOPMU: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('FLOPMU: FLOPMD ERROR',I3)
 1150 FORMAT ('FLOPMU: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE FLOPMH
C-----------------------------------------------------------------------
C   FLOPMH copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      INTEGER   NONOT
      PARAMETER (NONOT=16)
C
      CHARACTER HILINE*72, NOTTYP(NONOT)*2
      INTEGER   LUN1, LUN2, IERR, I, J, K, VER, NNIF, NREC, LBUFF(512),
     *   HIF, NVER, L
      INCLUDE 'FLOPM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INTEGER   FREQTM, FQSID(MAXIF), ITEMP, SOURID, ANTNO, SUBA,
     *   FREQID, IDSOU, QUAL, REFA(2,MAXIF), NTERM, ISURNO, NFREQ,
     *   SUKOLS(MAXSUC), SUNUMV(MAXSUC), IFGRNO, FGKOLS(MAXFGC),
     *   FGNUMV(MAXFGC), ANTS(2), IFS(2), CHANS(2)
      DOUBLE PRECISION FQFRQ(MAXIF), DTEMP, TIME, GEODLY(10),
     *   FREQO(MAXIF), BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   LSRVEL(MAXIF), LRESTF(MAXIF), PMRA, PMDEC, IF1NEW, RAOBS,
     *   DECOBS
      REAL      FQCHB(MAXIF), FQTBW(MAXIF), RTEMP, TIMEI, LIFR, ATMOS,
     *   DATMOS, MBDELY(2), CLOCK(2), DCLOCK(2), DISP(2), DDISP(2),
     *   DOPOFF(MAXIF), CREAL(2,MAXIF),CIMAG(2,MAXIF), DELAY(2,MAXIF),
     *   RATE(2,MAXIF), WEIGHT(2,MAXIF), FLUX(4,MAXIF), TIMER(2)
      LOGICAL   PFLAGS(4)
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4, REASON*24,
     *   BNDCOD(MAXIF)*8
      DATA LUN1, LUN2 /27,28/
C                                       all IF-dependent tables
      DATA NOTTYP /'NX','CH','SN', 'BP', 'IM', 'CQ', 'PC', 'TY', 'GC',
     *   'MC', 'WX', 'BL', 'AT', 'CS', 'GA', 'OF'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   SCRBUF, LBUFF, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, LBUFF,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, LBUFF,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
C                                       calibration history
      CALL CALHIS (LUN2, LBUFF, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1010) TSKNAM, OPTYPE
      CALL HIADD (LUN2, HILINE, LBUFF, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       time corrected
      IF ((STARTT.GE.0.0) .OR. (ENDT.LE.1.E5)) THEN
         HILINE = TSKNAM // '/ time range used for correction:'
         CALL HIADD (LUN2, HILINE, LBUFF, IERR)
         IF (IERR.NE.0) GO TO 100
         CALL HITIME (STARTT, ENDT, LUN2, LBUFF, IERR)
         END IF
C                                       Close HI file
 100  CALL HICLOS (LUN2, .TRUE., LBUFF, IERR)
C                                       Lie to COPTAB to stop copying
      IF ((OPTYPE.NE.'IFTO') .AND. (OPTYPE.NE.'IFON')) THEN
C                                       need to copy CL table 1 ??
C         DOCAL = .TRUE.
         DOBL = .TRUE.
         DOBAND = 1
C                                       Copy tables
         CALL COPTAB (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
C                                       copy only non-IF tables + FQ
      ELSE
         CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *      NEWCNO, CATBLK, BUFF2, LBUFF, IERR)
         END IF
      IF (IERR.GT.2) THEN
         MSGTXT = 'FLOPMH: ERROR COPYING TABLES TO OUTPUT UV'
         CALL MSGWRT (6)
         END IF
C                                       update FQ table
      IF (OPTYPE.NE.'VLAE') THEN
         VER = 1
         CALL FQINI ('WRIT', LBUFF, DISKO, NEWCNO, VER, CATBLK, LUN1,
     *      IFQRNO, FQKOLS, FQNUMV, NNIF, IERR)
         IF (IERR.GT.0) GO TO 999
         NREC = LBUFF(5)
         HIF = NNIF / 2
         DO 120 I = 1,NREC
            IFQRNO = I
            CALL TABFQ ('READ', LBUFF, IFQRNO, FQKOLS, FQNUMV, NNIF,
     *         FREQTM, FQFRQ, FQCHB, FQTBW, FQSID, BNDCOD, IERR)
            IF (OPTYPE.NE.'IFON') THEN
               DO 110 J = 1,NNIF
                  FQCHB(J) = -FQCHB(J)
 110              CONTINUE
               END IF
            IF ((OPTYPE.EQ.'IFTO') .OR. (OPTYPE.EQ.'IFON')) THEN
               IF1NEW = FQFRQ(NNIF)
               DO 115 J = 1,HIF
                  K = NNIF + 1 - J
                  DTEMP = FQFRQ(J) - IF1NEW
                  FQFRQ(J) = FQFRQ(K) - IF1NEW
                  FQFRQ(K) = DTEMP
                  RTEMP = FQCHB(J)
                  FQCHB(J) = FQCHB(K)
                  FQCHB(K) = RTEMP
                  RTEMP = FQTBW(J)
                  FQTBW(J) = FQTBW(K)
                  FQTBW(K) = RTEMP
                  ITEMP = FQSID(J)
                  FQSID(J) = FQSID(K)
                  FQSID(K) = ITEMP
 115              CONTINUE
               END IF
            IFQRNO = I
            CALL TABFQ ('WRIT', LBUFF, IFQRNO, FQKOLS, FQNUMV, NNIF,
     *         FREQTM, FQFRQ, FQCHB, FQTBW, FQSID, BNDCOD, IERR)
 120        CONTINUE
         CALL TABIO ('CLOS', 0, IFQRNO, LBUFF, LBUFF, IERR)
         END IF
C                                       do a few IF-dependent tables
      IF ((OPTYPE.EQ.'IFTO') .OR. (OPTYPE.EQ.'IFON')) THEN
C                                       update SU table
         CALL FNDEXT ('SU', CATBLK, NVER)
         DO 230 VER = 1,NVER
C                                       get header values
            CALL SOUINI ('READ', LBUFF, DISKO, NEWCNO, VER, CATBLK,
     *         LUN1, NNIF, VELTYP, VELDEF, FREQID, ISURNO, SUKOLS,
     *         SUNUMV, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TABIO ('CLOS', 0, ISURNO, LBUFF, LBUFF, IERR)
            CALL SOUINI ('WRIT', LBUFF, DISKO, NEWCNO, VER, CATBLK,
     *         LUN1, NNIF, VELTYP, VELDEF, FREQID, ISURNO, SUKOLS,
     *         SUNUMV, IERR)
            NREC = LBUFF(5)
            HIF = NNIF / 2
            DO 220 I = 1,NREC
               ISURNO = I
               CALL TABSOU ('READ', LBUFF, ISURNO, SUKOLS, SUNUMV,
     *            IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW,
     *            RAEPO, DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS,
     *            LSRVEL, LRESTF, PMRA, PMDEC, IERR)
               DO 215 J = 1,HIF
                  K = NNIF + 1 - J
                  DO 210 L = 1,4
                     RTEMP = FLUX(L,J)
                     FLUX(L,J) = FLUX(L,K)
                     FLUX(L,K) = RTEMP
 210                 CONTINUE
                  DTEMP = FREQO(J)
                  FREQO(J) = FREQO(K)
                  FREQO(K) = DTEMP
                  DTEMP = LSRVEL(J)
                  LSRVEL(J) = LSRVEL(K)
                  LSRVEL(K) = DTEMP
                  DTEMP = LRESTF(J)
                  LRESTF(J) = LRESTF(K)
                  LRESTF(K) = DTEMP
 215              CONTINUE
               ISURNO = I
               CALL TABSOU ('WRIT', LBUFF, ISURNO, SUKOLS, SUNUMV,
     *            IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW,
     *            RAEPO, DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS,
     *            LSRVEL, LRESTF, PMRA, PMDEC, IERR)
 220           CONTINUE
            CALL TABIO ('CLOS', 0, ISURNO, LBUFF, LBUFF, IERR)
 230        CONTINUE
C                                       update CL table
         CALL FNDEXT ('CL', CATBLK, NVER)
         DO 330 VER = 1,NVER
C                                       get header values
            CALL CALINI ('READ', LBUFF, DISKO, NEWCNO, VER, CATBLK,
     *         LUN1, ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NNIF,
     *         NTERM, GMMOD, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TABIO ('CLOS', 0, ICLRNO, LBUFF, LBUFF, IERR)
            CALL CALINI ('WRIT', LBUFF, DISKO, NEWCNO, VER, CATBLK,
     *         LUN1, ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NNIF,
     *         NTERM, GMMOD, IERR)
            IF (IERR.GT.0) GO TO 999
            IF (IERR.GT.0) GO TO 999
            NREC = LBUFF(5)
            HIF = NNIF / 2
            DO 320 I = 1,NREC
               ICLRNO = I
               CALL TABCAL ('READ', LBUFF, ICLRNO, CLKOLS, CLNUMV,
     *            NUMPOL, NNIF, TIME, TIMEI, SOURID, ANTNO, SUBA,
     *            FREQID, LIFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY,
     *            CLOCK, DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *            WEIGHT, REFA, IERR)
               DO 315 J = 1,HIF
                  K = NNIF + 1 - J
                  DO 310 L = 1,2
                     ITEMP = REFA(L,J)
                     REFA(L,J) = REFA(L,K)
                     REFA(L,K) = ITEMP
                     RTEMP = CREAL(L,J)
                     CREAL(L,J) = CREAL(L,K)
                     CREAL(L,K) = RTEMP
                     RTEMP = CIMAG(L,J)
                     CIMAG(L,J) = CIMAG(L,K)
                     CIMAG(L,K) = RTEMP
                     RTEMP = DELAY(L,J)
                     DELAY(L,J) = DELAY(L,K)
                     DELAY(L,K) = RTEMP
                     RTEMP = RATE(L,J)
                     RATE(L,J) = RATE(L,K)
                     RATE(L,K) = RTEMP
                     RTEMP = WEIGHT(L,J)
                     WEIGHT(L,J) = WEIGHT(L,K)
                     WEIGHT(L,K) = RTEMP
 310                 CONTINUE
                  RTEMP = DOPOFF(J)
                  DOPOFF(J) = DOPOFF(K)
                  DOPOFF(K) = RTEMP
 315              CONTINUE
               ICLRNO = I
               CALL TABCAL ('WRIT', LBUFF, ICLRNO, CLKOLS, CLNUMV,
     *            NUMPOL, NNIF, TIME, TIMEI, SOURID, ANTNO, SUBA,
     *            FREQID, LIFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY,
     *            CLOCK, DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *            WEIGHT, REFA, IERR)
 320           CONTINUE
            CALL TABIO ('CLOS', 0, ISURNO, LBUFF, LBUFF, IERR)
 330        CONTINUE
C                                       update FG table
         NFREQ = CATBLK(KINAX+JLOCF)
         CALL FNDEXT ('FG', CATBLK, NVER)
         DO 430 VER = 1,NVER
C                                       get header values
            CALL FLGINI ('READ', LBUFF, DISKO, NEWCNO, VER, CATBLK,
     *         LUN1, IFGRNO, FGKOLS, FGNUMV, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TABIO ('CLOS', 0, ICLRNO, LBUFF, LBUFF, IERR)
            CALL FLGINI ('WRIT', LBUFF, DISKO, NEWCNO, VER, CATBLK,
     *         LUN1, IFGRNO, FGKOLS, FGNUMV, IERR)
            IF (IERR.GT.0) GO TO 999
            NREC = LBUFF(5)
            DO 420 I = 1,NREC
               IFGRNO = I
               CALL TABFLG ('READ', LBUFF, IFGRNO, FGKOLS, FGNUMV,
     *            SOURID, SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS,
     *            REASON, IERR)
               IF (CHANS(1).LE.0) CHANS(1) = 1
               IF (CHANS(2).LT.CHANS(1)) CHANS(2) = NFREQ
               IF (IFS(1).LE.0) IFS(1) = 1
               IF (IFS(2).LT.IFS(1)) IFS(2) = NNIF
               IF (OPTYPE.NE.'IFON') THEN
                  ITEMP = CHANS(1)
                  CHANS(1) = NFREQ + 1 - CHANS(2)
                  CHANS(2) = NFREQ + 1 - ITEMP
                  END IF
               ITEMP = IFS(1)
               IFS(1) = NNIF + 1 - IFS(2)
               IFS(2) = NNIF + 1 - ITEMP
               IFGRNO = I
               CALL TABFLG ('WRIT', LBUFF, IFGRNO, FGKOLS, FGNUMV,
     *            SOURID, SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS,
     *            REASON, IERR)
 420           CONTINUE
            CALL TABIO ('CLOS', 0, ISURNO, LBUFF, LBUFF, IERR)
 430        CONTINUE
         END IF
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', SCRBUF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLOPMH: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'OPTYPE  = ''',A,'''  / VLAE => special corrections')
      END
      SUBROUTINE FLOPMD (NUMVIS, TIME, VIS, RESULT, IRET)
C-----------------------------------------------------------------------
C   This does the work.
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      TIME    R    Time
C      VIS     R(3,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C   Output:
C      RESULT     R(3,*) Output visibilities selected in frequency.
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IRET
      REAL      TIME, VIS(3,*), RESULT(3,*)
C
      INTEGER   JIF, JF, JS, NIF, NF, NS, INDEXO, INDEXI
      INCLUDE 'FLOPM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (NUMVIS.GT.0) THEN
C                                       pointers to traverse the data
         NS = 1
         NIF = 1
         NF = 1
         IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
         IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
         IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
         IF ((TIME.LT.STARTT) .OR. (TIME.GT.ENDT)) THEN
            DO 40 JIF = 1,NIF
               DO 30 JF = 1,NF
                  DO 20 JS = 1,NS
                     INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *                  (JS-1) * INCSI + 1
                     INDEXO = (JIF-1) * INCIFO + (JF-1) * INCFO +
     *                  (JS-1) * INCSO + 1
                     RESULT(1,INDEXO) = VIS(1,INDEXI)
                     RESULT(2,INDEXO) = VIS(2,INDEXI)
                     RESULT(3,INDEXO) = VIS(3,INDEXI)
 20                  CONTINUE
 30               CONTINUE
 40            CONTINUE
         ELSE IF (OPTYPE.EQ.'VLAE') THEN
            DO 140 JIF = 1,NIF
               DO 130 JF = 1,NF
                  DO 120 JS = 1,NS
                     INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *                  (JS-1) * INCSI + 1
                     INDEXO = (JIF-1) * INCIFO + (NF-JF) * INCFO +
     *                  (JS-1) * INCSO + 1
                     RESULT(1,INDEXO) = VIS(1,INDEXI)
                     RESULT(2,INDEXO) = -VIS(2,INDEXI)
                     RESULT(3,INDEXO) = VIS(3,INDEXI)
 120                 CONTINUE
 130              CONTINUE
 140           CONTINUE
         ELSE IF (OPTYPE.EQ.'IFON') THEN
            DO 240 JIF = 1,NIF
               DO 230 JF = 1,NF
                  DO 220 JS = 1,NS
                     INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *                  (JS-1) * INCSI + 1
                     INDEXO = (NIF-JIF) * INCIFO + (JF-1) * INCFO +
     *                  (JS-1) * INCSO + 1
                     RESULT(1,INDEXO) = VIS(1,INDEXI)
                     RESULT(2,INDEXO) = VIS(2,INDEXI)
                     RESULT(3,INDEXO) = VIS(3,INDEXI)
 220                 CONTINUE
 230              CONTINUE
 240           CONTINUE
         ELSE IF (OPTYPE.EQ.'IFTO') THEN
            DO 340 JIF = 1,NIF
               DO 330 JF = 1,NF
                  DO 320 JS = 1,NS
                     INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *                  (JS-1) * INCSI + 1
                     INDEXO = (NIF-JIF) * INCIFO + (NF-JF) * INCFO +
     *                  (JS-1) * INCSO + 1
                     RESULT(1,INDEXO) = VIS(1,INDEXI)
                     RESULT(2,INDEXO) = VIS(2,INDEXI)
                     RESULT(3,INDEXO) = VIS(3,INDEXI)
 320                 CONTINUE
 330              CONTINUE
 340           CONTINUE
         ELSE
            DO 440 JIF = 1,NIF
               DO 430 JF = 1,NF
                  DO 420 JS = 1,NS
                     INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *                  (JS-1) * INCSI + 1
                     INDEXO = (JIF-1) * INCIFO + (NF-JF) * INCFO +
     *                  (JS-1) * INCSO + 1
                     RESULT(1,INDEXO) = VIS(1,INDEXI)
                     RESULT(2,INDEXO) = VIS(2,INDEXI)
                     RESULT(3,INDEXO) = VIS(3,INDEXI)
 420                 CONTINUE
 430              CONTINUE
 440           CONTINUE
            END IF
C                                       last call - no vis
      ELSE
         END IF
C
 999  RETURN
      END
