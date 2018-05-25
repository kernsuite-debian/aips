LOCAL INCLUDE 'BPWGT.INC'
C                                       Local include for BPWGT
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XNAMOU(3),
     *   XCLAOU(2), XXSTOK
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XBCHAN, XECHAN, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), XDOAC, XSOUT,
     *   XDISO, XWGTIT, XCENT,
     *   SCRBUF(256), BUFF2(UVBFSS), DIFPIX
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ, ILOCWT, NWGTIT,
     *   CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO,
     *   LRECO, NRPRMI, NRPRMO, OLDCNO, NEWCNO
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XXSTOK, XTIME, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF, XBCHAN,
     *   XECHAN, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND,
     *   XBPVER, XSMOTH, XDOAC, XNAMOU, XCLAOU, XSOUT, XDISO, XWGTIT,
     *   XCENT
      COMMON /BPWGTP/ CATOLD, SEQIN, SEQOUT, DISKIN, DISKO, ILOCWT,
     *   INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECO, NRPRMI,
     *   NRPRMO, ISCOMP, OLDCNO, NEWCNO, DIFPIX
      COMMON /WGTIT/ NWGTIT
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT
      COMMON /BUFRS/ SCRBUF, BUFF2, JBUFSZ
C                                       End local include for BPWGT
LOCAL END
      PROGRAM BPWGT
C-----------------------------------------------------------------------
C! Calibrate data and modify channel weights by the bandpass correction
C# Utility UV UV-util VLA VLB Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2013-2014
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
C   BPWGT copies the UV data set applying calibration and modifying the
C   weights by the bandpass correction on a channel-by-channel basis.
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
      INCLUDE 'BPWGT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'BPWGT '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL BPWTIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL BPWTUV (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL BPWTHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE BPWTIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   BPWTIN gets input parameters for BPWGT and creates an output file
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
C   See prologue comments in BPWGT for more details.
C
C   To change the adverb list sent to this task change:
C   1)  the inputs file.
C   2)  the contents of COMMON /INPARM/.  Remember all adverbs are sent
C       as R, INNAME etc. are 12 char. 3 words;
C       INCLASS etc. are 6 char., 2 words.
C       Values will be filled into COMMON /INPARM/ in the order
C       specified in the inputs file.
C   3)  If the first adverb is not INNAME (NAMEIN) then replace
C       NAMEIN in the call to GTPARM with the name of the first
C       adverb.
C   4)  Change the value of NPARM sent to GTPARM to the number of
C       R words desired.
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCX, I, NFREQ, LUN
      LOGICAL   MATCH
      REAL      CATR(256), RPARM(20)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'BPWGT.INC'
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
      NPARM = 167
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
      IF (DOBAND.LE.0) THEN
         MSGTXT = 'YOU MUST APPLY A BANPASS CORRECTION'
         JERR = 10
         GO TO 990
         END IF
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      NWGTIT = IROUND (XWGTIT)
      NWGTIT = MAX (0, MIN (2, NWGTIT))
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
      IF (JLOCF.LT.0) XCENT = -1.
      IF (XCENT.GT.0.0) THEN
         INCX = CATBLK(KINAX+JLOCF) / 2 + 1
         DIFPIX = INCX - CATR(KRCRP+JLOCF)
         CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + CATR(KRCIC+JLOCF) *
     *      DIFPIX
         CATR(KRCRP+JLOCF) = INCX
      ELSE
         DIFPIX = 0.0
         END IF
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
 1000 FORMAT ('BPWTIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('BPWTIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE BPWTUV (IRET)
C-----------------------------------------------------------------------
C   BPWTUV sends uv data one point at a time to the user supplied
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
      INCLUDE 'BPWGT.INC'
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
         CALL DIDDLE (NUMVIS, VIS, RESULT, IRET)
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
 200  NIOUT = - NIOUT
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
 1000 FORMAT ('BPWTUV: ERROR',I3,' OPEN/INIT INPUT VIS FILE')
 1010 FORMAT ('BPWTUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('BPWTUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1100 FORMAT ('BPWTUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('BPWTUV: DIDDLE ERROR',I3)
 1150 FORMAT ('BPWTUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE BPWTHI
C-----------------------------------------------------------------------
C   BPWTHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      INTEGER   LUN1, LUN2, IERR
C
      CHARACTER HICARD*72
      INTEGER   BUFF1(512)
      INCLUDE 'BPWGT.INC'
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
      WRITE (HICARD,1010) TSKNAM, NWGTIT
      CALL HIADD (LUN2, HICARD, BUFF2, IERR)
C                                       Close HI file
 100  CALL HICLOS (LUN2, .TRUE., BUFF2, IERR)
C                                       Copy tables
      CALL COPTAB (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'BPWTHI: ERROR COPYING TABLES TO OUTPUT UV'
         CALL MSGWRT (6)
         END IF
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, NEWCNO, DIFPIX, BUFF1, BUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'BPWTHI: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', SCRBUF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPWTHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'WEIGHTIT =',I2,'   / weight = bp**(0.5 ** weightit')
      END
      SUBROUTINE DIDDLE (NUMVIS, VIS, RESULT, IRET)
C-----------------------------------------------------------------------
C   This moves VIS to RESULT only.  The real work is done in the local
C   version of DATBND.
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      VIS     R(3,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C   Inputs from COMMON:
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
C      RESULT     R(3,*) Output visibilities selected in frequency.
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IRET
      REAL      VIS(3,*), RESULT(3,*)
C
      INTEGER   JIF, JF, JS, NIF, NF, NS, INDEXO, INDEXI
      INCLUDE 'BPWGT.INC'
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
         DO 40 JIF = 1,NIF
            DO 30 JF = 1,NF
               DO 20 JS = 1,NS
                  INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *               (JS-1) * INCSI + 1
                  INDEXO = (JIF-1) * INCIFO + (JF-1) * INCFO +
     *               (JS-1) * INCSO + 1
                  RESULT(1,INDEXO) = VIS(1,INDEXI)
                  RESULT(2,INDEXO) = VIS(2,INDEXI)
                  RESULT(3,INDEXO) = VIS(3,INDEXI)
 20               CONTINUE
 30            CONTINUE
 40         CONTINUE
C                                       last call - no vis
      ELSE
         END IF
C
 999  RETURN
      END
      SUBROUTINE DATBND (TIME, IA1, IA2, VIS, IERR)
C-----------------------------------------------------------------------
C   Routine which applies the bandpass correction.
C   ********** special version applies BP to weights only *************
C   Inputs:
C      TIME     R        Time of visibility data (in days)
C      IA1      I        Antenna number 1
C      IA2      I        Antenna number 2
C   Inputs from common:
C      BPBUFF   R(*)     Large array containing bandpass spectra for
C                        all antennas
C      ANTPNT   LI(2,2)  Pointer giving the start address of the
C                        specified antennas BP spectra within BPBUFF
C      DOBAND   I        Method of BP application
C   In/Outputs:
C      VIS      R(*)     Array of visibility data (corrected on out)
C   Output:
C      IERR     I        If > 0, error returned from BPGET
C   Output to common:
C      CNTREC  I(2,3)  Record counts:
C                        (1&2,1) Previously flagged (partly, fully)
C                        (1&2,2) Flagged due to gains (part, full)
C                        (1&2,3) Good selected (part, full)
C   NOTE: This routine applies the bandpass correction for formulae:
C      (1) Cross-power:   Scorr   =  (1/Sant_1) * (1/Sant_2) * Sobs
C      (2) Total-power:   Scorr   =  (Son / Soff) - 1.0
C-----------------------------------------------------------------------
      REAL      TIME, VIS(*)
      INTEGER   IA1, IA2, IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   INCPX, IFRQ, IIF, IOFF, DINDX, CHOFF, NCOR, KLOCS, JERR,
     *   TCOR0, IPOL, JPOL1(4), JPOL2(4), LRECS, LUNSRC, IFLAG(2),
     *   JFLAG(2), IROUND, KLOCF, ITMP, JTMP, NMAX, LBCH, LECH, CH1,
     *   CH2, NCHS(4)
      LONGINT   I1OFF, I2OFF, L1OFF, L2OFF, I1D, I2D, L1D, L2D
      REAL      GR1, GI1, AMP12, AMP22, REAL1, IMAG1, REAL2, IMAG2,
     *   RTMP, BPARR1(6), BPARR2(6), AMPS(4), TVR, TVI, POW
      HOLLERITH CATH(256)
      LOGICAL   FLAGD1, FLAGDA, IQUV, CORRP, T, F, WAUTO, FLAGD0,
     *   FAILED, FIRST
      DOUBLE PRECISION CATD(128), SHIFT1(MAXIF), SHIFT2(MAXIF), RATE,
     *   TFRQ, DPOLYN(MAXCHA)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DBPC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DCVL.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      INTEGER   NWGTIT
      COMMON /WGTIT/ NWGTIT
      SAVE FIRST
      EQUIVALENCE (CATH, CATD, CATUV)
      DATA T, F, FIRST /.TRUE., .FALSE., .TRUE./
C-----------------------------------------------------------------------
      IF (FIRST) THEN
         MSGTXT = 'Using special version of DATBND'
         CALL MSGWRT (2)
         FIRST = .FALSE.
         END IF
      POW = 0.5 ** NWGTIT
C                                       Check sizes
      IF ((NIFBP.GT.MAXIF) .OR. (NCHNBP.GT.MAXCHA) .OR.
     *   (NPOLBP*NIFBP*NCHNBP.GT.MAXCIF)) THEN
         IERR = 1
         MSGTXT = 'DATBND:VISIBILITIES TOO BIG FOR BUFFERS'
         GO TO 990
         END IF
      IF (BCHANS.LE.0) BCHANS = BCHAN
      IF (ECHANS.LE.0) ECHANS = ECHAN
C                                       Average amplitude range
      ITMP = CATUV(KINAX+KLOCFY)
      IF (ISVLA) THEN
         CH1 = (ITMP+1)/8 + 1
         CH2 = ITMP - ((ITMP+1)/8)
      ELSE
         CH1 = 1
         CH2 = ITMP
         END IF
      CALL FILL (4, 0, NCHS)
      CALL RFILL (4, 0.0, AMPS)
      IF (DOWTCL) THEN
         LBCH = MIN (CH1, BCHANS)
         LECH = MAX (CH2, ECHANS)
      ELSE
         LBCH = BCHANS
         LECH = ECHANS
         END IF
C                                       Determine shift needed from BP
C                                       table
      CALL DFILL (MAXIF, 0.0D0, SHIFT1)
      CALL DFILL (MAXIF, 0.0D0, SHIFT2)
C
      IF (ISVLBA) THEN
         CALL AXEFND (8, 'FREQ    ', CATUV(KIDIM), CATH(KHCTP), KLOCF,
     *      JERR)
         IF ((RAEPO.EQ.0.D0) .AND. (DECEPO.EQ.0.D0)) THEN
            LUNSRC = 49
            CALL GETSOU (CURSOU, IUDISK, IUCNO, CATUV, LUNSRC, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1040) IERR
               GO TO 990
               END IF
            END IF
         DO 20 IIF = 1,CNNIF
            TFRQ = CATD(KDCRV+KLOCF) + CFOFF(IIF) + FREQO(IIF)
            CALL DETRAT (TIME, RAEPO, DECEPO, ANTX(IA1), ANTY(IA1),
     *         ANTZ(IA1), TFRQ, RATE)
            SHIFT1(IIF) = RATE / CFINC(IIF)
            IF (IA2.NE.IA1) THEN
               CALL DETRAT (TIME, RAEPO, DECEPO, ANTX(IA2), ANTY(IA2),
     *            ANTZ(IA2), TFRQ, RATE)
               SHIFT2(IIF) = RATE / CFINC(IIF)
               END IF
 20         CONTINUE
         END IF
C                                       Get the bandpass spectra
      CALL BPGET (TIME, IA1, IA2, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      FAILED = IERR.LT.0
      IERR = 0
C                                       Check STOKES parms
      CALL AXEFND (8, 'STOKES  ', CATUV(KIDIM), CATH(KHCTP), KLOCS,
     *   JERR)
      NCOR = CATUV(KINAX+KLOCS)
      IF (CATD(KDCRV+KLOCS).GT.0.0D0) TCOR0 = CATD(KDCRV+KLOCS) + 0.5D0
      IF (CATD(KDCRV+KLOCS).LT.0.0D0) TCOR0 = CATD(KDCRV+KLOCS) - 0.5D0
      IQUV = TCOR0.GE.1
      CORRP = (TCOR0.LE.-1) .AND. (TCOR0.GT.-5)
C                                       Set visibility increment
      INCPX = CATUV(KINAX)
C                                       Compressed data expanded
      IF (INCPX.EQ.1) INCPX = 3
C                                       Set up base pointers
      I1OFF = ANTPNT(1,1)
      I2OFF = ANTPNT(2,1)
      L1OFF = ANTPNT(1,1) + PVLBUF - PBPBUF
      L2OFF = ANTPNT(2,1) + PVLBUF - PBPBUF
C                                       polarization pointers
      DO 30 IPOL = 1,NCOR
         IF (CORRP) THEN
C                                       RR or LL
            IF (IPOL.LE.2) THEN
               JPOL1(IPOL) = IPOL
               JPOL2(IPOL) = IPOL
C                                       RL
            ELSE IF (IPOL.EQ.3) THEN
               JPOL1(IPOL) = 1
               JPOL2(IPOL) = 2
C                                       LR
            ELSE IF (IPOL.EQ.4) THEN
               JPOL1(IPOL) = 2
               JPOL2(IPOL) = 1
               END IF
C                                       For IQUV always use Stokes I
C                                       bandpass
         ELSE IF (IQUV) THEN
            JPOL1(IPOL) = 1
            JPOL2(IPOL) = 1
            END IF
 30      CONTINUE
C                                       Flag on total failure
      IF (FAILED) THEN
         FLAGD1 = T
         FLAGDA = T
         DO 45 IIF = BIF,EIF
            IOFF = (IIF-1) * KNCIF
            DO 40 IFRQ = BCHANS,ECHANS
               DO 35 IPOL = 1,NCOR
C                                       Index for weight
                  DINDX = ((IOFF + (IPOL-1) * KNCS) +
     *               (IFRQ-1) * KNCF) * INCPX + 3
                  VIS(DINDX) = -1.0
 35               CONTINUE
 40            CONTINUE
 45         CONTINUE
C                                       Shift the bandpasses to the
C                                       appropriate value. First set
C                                       flagged channels.
      ELSE
         FLAGD1 = F
         FLAGDA = T
         IFLAG(1) = 0
         IFLAG(2) = 0
         JFLAG(1) = NCHNBP + 1
         JFLAG(2) = NCHNBP + 1
         IF (ISVLBA) THEN
            IFLAG(1) = IROUND (AVDELI(IA1,1)) + 1
            JFLAG(1) = NCHNBP - IFLAG(1) + 1
            IF (ABS (SHIFT1(1)).GT.0.0) THEN
               RTMP = ABS (SHIFT1(1))
               ITMP = 1 + IROUND (RTMP)
               IFLAG(1) = MAX(IFLAG(1), ITMP)
               JTMP = NCHNBP - IFLAG(1) + 1
               JFLAG(1) = MIN(JFLAG(1), JTMP)
               END IF
            IF (IA1.NE.IA2) THEN
               IFLAG(2) = IROUND(AVDELI(IA2,1)) + 1
               JFLAG(2) = NCHNBP - IFLAG(2) + 1
               IF (ABS (SHIFT2(1)).GT.0.0) THEN
                  RTMP = ABS (SHIFT2(1))
                  ITMP = 1 + IROUND (RTMP)
                  IFLAG(2) = MAX(IFLAG(2), ITMP)
                  JTMP = NCHNBP - IFLAG(2) + 1
                  JFLAG(2) = MIN(JFLAG(2), JTMP)
                  END IF
               END IF
            END IF
C                                       Case BP entry type of:
C                                       1: Polynomial BP
         IF (WPOLY) THEN
            WAUTO = (IA1.EQ.IA2)
            DO 60 IPOL = 1,NPOLBP
               DO 50 IIF = 1,NIFBP
                  I1D = I1OFF + (IIF-1) * KSNCIF + (IPOL-1) * KSNCS
                  I2D = I2OFF + (IIF-1) * KSNCIF + (IPOL-1) * KSNCS
                  L1D = L1OFF + (IIF-1) * KSNCIF + (IPOL-1) * KSNCS
                  L2D = L2OFF + (IIF-1) * KSNCIF + (IPOL-1) * KSNCS
                  CALL BPCOEF (LTYPBP, BPBUF(I1D), BPBUF(I1D+1),
     *               KSNCF, KSNCF, NCHNBP, FBLANK, SHIFT1(IIF),
     *               VLBUF(L1D), VLBUF(L1D+1), KSNCF, KSNCF,
     *               1, NCHNBP, 1.0, FLOAT(NCHNBP), 0, WAUTO,
     *               DPOLYN, MAXCHA, NMAX, .FALSE., IERR)
                  IF (IERR.NE.0) GO TO 999
                  IF (.NOT.WAUTO) CALL BPCOEF (LTYPBP, BPBUF(I2D),
     *               BPBUF(I2D+1), KSNCF, KSNCF, NCHNBP, FBLANK,
     *               SHIFT2(IIF), VLBUF(L2D), VLBUF(L2D+1), KSNCF,
     *               KSNCF, 1, NCHNBP, 1.0, FLOAT(NCHNBP), 0, WAUTO,
     *               DPOLYN, MAXCHA, NMAX, .FALSE., IERR)
                  IF (IERR.NE.0) GO TO 999
 50               CONTINUE
 60            CONTINUE
C                                       2: Standard BP entry:
         ELSE IF (ISVLBA) THEN
            LRECS = NIFBP * NPOLBP * NCHNBP * 2
            IF ((ABS(SHIFT1(1) - CURSHF(IA1))) .GT. 0.1) THEN
               CALL RCOPY (LRECS, BPBUF(I1OFF), VLBUF(L1OFF))
               CURSHF(IA1) = SHIFT1(1)
               CALL BPSHFT (VLBUF(L1OFF), NPOLBP, NIFBP, NCHNBP,
     *            KSNCS, KSNCIF, KSNCF, SHIFT1, NUMSHF)
               END IF
            IF (IA1.NE.IA2) THEN
               IF ((ABS(SHIFT2(1) - CURSHF(IA2))) .GT. 0.1) THEN
                  CALL RCOPY (LRECS, BPBUF(I2OFF), VLBUF(L2OFF))
                  CURSHF(IA2) = SHIFT2(1)
                  CALL BPSHFT (VLBUF(L2OFF), NPOLBP, NIFBP, NCHNBP,
     *               KSNCS, KSNCIF, KSNCF, SHIFT2, NUMSHF)
                  END IF
               END IF
            END IF
C                                       Endcase (BP_entry type)
C
C                                       Additional offset to cope
C                                       with the possibility that
C                                       BCHAN and BCHNBP differ
         CHOFF = 1 - BCHNBP
C                                       Check bandpass and data match
         IF (BCHNBP.GT.BCHAN) THEN
            WRITE (MSGTXT,1010) BCHAN
            CALL MSGWRT (7)
            WRITE (MSGTXT,1020) BCHNBP
            CALL MSGWRT (7)
            WRITE (MSGTXT,1030)
            IERR = 5
            GO TO 990
            END IF
         DO 150 IIF = BIF,EIF
            IOFF = (IIF-1) * KNCIF
            DO 120 IFRQ = LBCH,LECH
C                                       Generate the correction arrays
               DO 100 IPOL = 1,NPOLBP
                  I1D = I1OFF + (IFRQ-1+CHOFF) * KSNCF +
     *               (IIF-1) * KSNCIF + (IPOL-1) * KSNCS
                  L1D = I1D - I1OFF + L1OFF
                  DINDX = IPOL * 3 - 2
                  BPARR1(DINDX+2) = 1.0
                  BPARR2(DINDX+2) = 1.0
C                                       cross power
                  I2D = I2OFF + (IFRQ-1+CHOFF) * KSNCF +
     *               (IIF-1) * KSNCIF + (IPOL-1) * KSNCS
                  L2D = I2D - I2OFF + L2OFF
                  IF (ISVLBA .OR. WPOLY) THEN
                     AMP12 = (VLBUF(L1D)   * VLBUF(L1D) +
     *                  VLBUF(L1D+1) * VLBUF(L1D+1))
                     AMP22 = (VLBUF(L2D)   * VLBUF(L2D) +
     *                  VLBUF(L2D+1) * VLBUF(L2D+1))
                     IF (IFRQ.LE.IFLAG(1)) VLBUF(L1D) = FBLANK
                     IF (IFRQ.LE.IFLAG(2)) VLBUF(L2D) = FBLANK
                     IF (IFRQ.GE.JFLAG(1)) VLBUF(L1D) = FBLANK
                     IF (IFRQ.GE.JFLAG(2)) VLBUF(L2D) = FBLANK
                     FLAGD0 = F
                     IF ((AMP12.LE.0.0) .OR. (VLBUF(L1D).EQ.FBLANK)
     *                  .OR. (VLBUF(L1D+1).EQ.FBLANK)) THEN
                        BPARR1(DINDX+2) = -1.0
                        AMP12 = 1.0
                        FLAGD0 = T
                        END IF
                     IF ((AMP22.LE.0.0) .OR. (VLBUF(L2D).EQ.FBLANK)
     *                  .OR. (VLBUF(L2D+1).EQ.FBLANK)) THEN
                        BPARR2(DINDX+2) = -1.0
                        AMP22 = 1.0
                        FLAGD0 = T
                        END IF
                     IF (FLAGD0) THEN
                        FLAGD1 = T
                     ELSE
                        FLAGDA = F
                        END IF
                     BPARR1(DINDX) = VLBUF(L1D)   / AMP12
                     BPARR1(DINDX+1) = VLBUF(L1D+1) / AMP12
                     BPARR2(DINDX) = VLBUF(L2D)   / AMP22
                     BPARR2(DINDX+1) = VLBUF(L2D+1) / AMP22
                  ELSE
                     AMP12 = (BPBUF(I1D)   * BPBUF(I1D) +
     *                  BPBUF(I1D+1) * BPBUF(I1D+1))
                     AMP22 = (BPBUF(I2D)   * BPBUF(I2D) +
     *                  BPBUF(I2D+1) * BPBUF(I2D+1))
                     FLAGD0 = F
                     IF ((AMP12.LE.0.0) .OR. (BPBUF(I1D).EQ.FBLANK)
     *                  .OR. (BPBUF(I1D+1).EQ.FBLANK)) THEN
                        BPARR1(DINDX+2) = -1.0
                        AMP12 = 1.0
                        FLAGD0 = T
                        END IF
                     IF ((AMP22.LE.0.0) .OR. (BPBUF(I2D).EQ.FBLANK)
     *                  .OR. (BPBUF(I2D+1).EQ.FBLANK)) THEN
                        BPARR2(DINDX+2) = -1.0
                        AMP22 = 1.0
                        FLAGD0 = T
                        END IF
                     IF (FLAGD0) THEN
                        FLAGD1 = T
                     ELSE
                        FLAGDA = F
                        END IF
                     BPARR1(DINDX) = BPBUF(I1D)   / AMP12
                     BPARR1(DINDX+1) = BPBUF(I1D+1) / AMP12
                     BPARR2(DINDX) = BPBUF(I2D)   / AMP22
                     BPARR2(DINDX+1) = BPBUF(I2D+1) / AMP22
                     END IF
 100              CONTINUE
C                                       Apply the correction
               DO 110 IPOL = 1,NCOR
C                                       Index for visibility
                  DINDX = ((IOFF + (IPOL-1) * KNCS) +
     *               (IFRQ-1) * KNCF) * INCPX + 1
                  I1D = 3*JPOL1(IPOL) - 2
                  I2D = 3*JPOL2(IPOL) - 2
C                                       Do the correction
                  REAL1 = BPARR1(I1D)
                  IMAG1 = BPARR1(I1D+1)
                  REAL2 = BPARR2(I2D)
                  IMAG2 = BPARR2(I2D+1)
                  GR1 = (REAL1*REAL2 + IMAG1*IMAG2)
                  GI1 = (REAL2*IMAG1 - REAL1*IMAG2)
C                                       scale visibilities
                  TVR = GR1 * VIS(DINDX) + GI1 * VIS(DINDX+1)
                  TVI = GR1 * VIS(DINDX+1) - GI1 * VIS(DINDX)
                  VIS(DINDX) = TVR
                  VIS(DINDX+1) = TVI
C                                       weights
                  IF ((BPARR1(I1D+2).LE.0.0) .OR.
     *               (BPARR2(I2D+2).LE.0.0)) THEN
                     VIS(DINDX+2) = -1.0
                  ELSE
                     RTMP = (GR1 * GR1 + GI1 * GI1) ** POW
                     IF (RTMP.LE.0.0) THEN
                        VIS(DINDX+2) = -1.0
                     ELSE
                        VIS(DINDX+2) = VIS(DINDX+2) / RTMP
                        END IF
                     END IF
 110              CONTINUE
 120           CONTINUE
 150        CONTINUE
         END IF
C                                       Update error counts
      IF (FLAGD1) THEN
         IF (FLAGDA) THEN
            CNTREC(2,2) = CNTREC(2,2) + 1
         ELSE
            CNTREC(1,2) = CNTREC(1,2) + 1
            END IF
         END IF
      GO TO 999
C
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DATBND: ERROR ',I3,' RETURNED FROM BPGET')
 1010 FORMAT ('DATBND: START CHANNEL IN DATA = ',I3)
 1020 FORMAT ('DATBND: START CHANNEL IN BP TABLE = ',I3)
 1030 FORMAT ('DATBND: CANNOT DO BANDPASS CORRECTION - RESET BP TABLE')
 1040 FORMAT ('DATBND: ERROR ',I2,' DETERMINING SOURCE POSITION')
      END
