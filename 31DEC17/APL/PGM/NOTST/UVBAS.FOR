LOCAL INCLUDE 'UVBAS.INC'
C                                       Local include for UVBAS
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XNAMOU(3),
     *   XCLAOU(2)
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XDOBND, XBPVER, XSMOTH(3), XDOAC, XSOUT, XDISO, XBCHAN,
     *   XECHAN, APARM(10), BUFF1(UVBFSS), BUFF2(UVBFSS)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, LBCHAN, LECHAN,
     *   JBUFSZ, ILOCWT, CATOLD(256), INCSI, INCFI, INCIFI, INCSO,
     *   INCFO, INCIFO, LRECI, LRECO, NRPRMI, NRPRMO, OLDCNO, NEWCNO,
     *   LBIF
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XTIME, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XDOAC,
     *   XNAMOU, XCLAOU, XSOUT, XDISO, XBCHAN, XECHAN, APARM
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /INFO/ CATOLD, SEQIN, SEQOUT, DISKIN, DISKO, LBCHAN,
     *   LECHAN, ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO,
     *   LRECI, LRECO, NRPRMI, NRPRMO, ISCOMP, OLDCNO, NEWCNO, LBIF
LOCAL END
      PROGRAM UVBAS
C-----------------------------------------------------------------------
C! Averages several channels and subtracts from uv data.
C# Utility UV UV-util VLA VLB SPECTRAL
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2008-2011
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
C   UVBAS averages a set of channels and subtracts them from another
C   range of channels.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      BCHAN          LBCHAN         Lowest channel number.
C      ECHAN          LECHAN         Highest channel number.
C      APARM(10)      APARM         Specification of region to average
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'UVBAS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'UVBAS '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UVBAIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Filter data.
      CALL UVBAUV (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL UVBAHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE UVBAIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   UVBAIN gets input parameters for UVBAS and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      LBCHAN   I  Lowest channel number to write.
C      LECHAN   I  Highest channel number to write.
C      ISCOMP  L  If true data is compressed
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCX, I, LUN
      LOGICAL   T, MATCH
      REAL      RPARM(20), CATR(256)
      HOLLERITH CATH(256)
      INCLUDE 'UVBAS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATR, CATH)
      DATA BLANK  /' '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 174
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
      CALL H2CHR (4, 1, XCALC, SELCOD)
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
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOACOR = XDOAC.GT.0.0
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
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
C                                       Must specify baseline windows
C                                       (APARM).
      IF ((APARM(1).LE.0.1) .OR. (APARM(2).LE.0.1) .OR.
     *   (APARM(3).LE.0.1) .OR. (APARM(4).LE.0.1)) THEN
         JERR = 9
         MSGTXT = 'YOU MUST FULLY SPECIFY AVERAGING WINDOWS (APARM)'
         GO TO 990
         END IF
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
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
      LBIF = BIF
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
C                                       Set BCHAN,ECHAN
      LBCHAN = IROUND (XBCHAN)
      LBCHAN = MAX (LBCHAN, 1)
      LBCHAN = MIN (LBCHAN, CATBLK(KINAX+JLOCF))
      LECHAN = IROUND (XECHAN)
      IF (LECHAN.LE.0) LECHAN = CATBLK(KINAX+JLOCF)
      LECHAN = MAX (LECHAN, LBCHAN)
      LECHAN = MIN (LECHAN, CATBLK(KINAX+JLOCF))
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1035) IERR
         GO TO 990
         END IF
      CALL UVGET ('CLOS', RPARM, BUFF1, IERR)
C                                       Save input file info
      INCX = CATBLK(KINAX)
      LRECI = LREC
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
C                                       Frequency axis
      CATBLK(KINAX+JLOCF) = LECHAN - LBCHAN + 1
      CATR(KRCRP+JLOCF) = CATR(KRCRP+JLOCF) - LBCHAN + 1
      CATR(KRARP) = CATR(KRARP) - LBCHAN + 1
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
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
         CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', BUFF1, IERR)
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
     *   PTYPE, NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
C                                       copy header keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVBAIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('UVBAIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE UVBAUV (IRET)
C-----------------------------------------------------------------------
C   UVBAUV sends uv data one point at a time to the filtering
C   routine and then writes the modified data if requested.
C   Input in common:
C      BCHAN   I  Lowest channel number to write.
C      ECHAN   I  Highest channel number to write.
C      LRECI   I  Input file record length
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
      INCLUDE 'UVBAS.INC'
      REAL      VIS(UVBFSS), RESULT(UVBFSS), RPARM(20)
      DOUBLE PRECISION UVSCAL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (VIS, BUFF1)
      DATA LUNO /17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Number of visibilities in input
C                                       and output files.
      NCORO = (LRECO - NRPRMO)
      IF (.NOT.ISCOMP) NCORO = NCORO / 3
      NCOPY = LRECO - NRPRMO
C                                       Open and init for read
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
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
C                                       Loop over
      ELSE IF (IRET.EQ.0) THEN
         NUMVIS = NUMVIS + 1
         RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
         RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
         RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
         CALL UVBASE (NUMVIS, VIS, RESULT, IRET)
C                                       Branch on his return
C                                       Error (fatal)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1120) IRET
            GO TO 990
C                                       Copy to output.
         ELSE IF (IRET.EQ.0) THEN
            XCOUNT = XCOUNT + 1.0D0
C                                       update NX table
            CALL RNXUPD (RPARM, RNXRET)
            CALL RCOPY (NRPRMI, RPARM, BUFF2(IPTRO))
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
C                                       flush UV buffers
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       close NX table
      CALL RNXCLS (RNXRET)
C                                       Close files
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
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
 1000 FORMAT ('UVBAUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('UVBAUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('UVBAUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('UVBAUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('UVBAUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('UVBAUV: UVBASE ERROR',I3)
 1150 FORMAT ('UVBAUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE UVBAHI
C-----------------------------------------------------------------------
C   UVBAHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72
      INTEGER   LUN1, LUN2, IERR
      LOGICAL   T
      INCLUDE 'UVBAS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
C                                       calibration adverbs
      CALL CALHIS (LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       BCHAN,ECHAN
      WRITE (HILINE,2000) TSKNAM, LBCHAN, LECHAN
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       APARM
      WRITE (HILINE,2001) TSKNAM, APARM(1), APARM(2), APARM(3), APARM(4)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
C                                        Copy tables
      CALL COPTAB (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'UVBAHI: ERROR COPYING TABLES TO OUTPUT'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST',
     *   BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVBAHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 1200 FORMAT ('UVBAHI: ERROR COPYING TABLES')
 2000 FORMAT (A6,' BCHAN =',I4,', ECHAN =',I4,' / Channel selection')
 2001 FORMAT (A6,' APARM =',3(F6.0,','),F6.0,' / Baseline windows')
      END
      SUBROUTINE UVBASE (NUMVIS, VIS, RESULT, IRET)
C-----------------------------------------------------------------------
C   Routine to average channels, interpolate and subtract.
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      VIS     R(3,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C                   NOTE: INCX may be any value .GE. 2
C   Inputs from COMMON:
C      APARM   R(10) 1&2, 3&4 give boxes to average.
C      BCHAN   I  Lowest channel number to write.
C      ECHAN   I  Highest channel number to write.
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C   Output:
C      RESULT  R(3,*) Output visibilities selected in frequency.
C      IRET    I  Return code  -1 => don't write
C                               0 => OK
C                              >0 => error, terminate.
C   Output in COMMON:
C      CATBLK    I         Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IRET
      REAL      VIS(3,*), RESULT(3,*)
C
      INTEGER   LIMIT1, LIMIT2, LOOP, NFREQ, NIF, NPOLN, INDEX, OFF, IS,
     *   IIF, INDEX2
      REAL      SUMRB, SUMIB, COUNTB, SUMRE, SUMIE, COUNTE, COUNT,
     *   AVGR, AVGI, AVGA, AVGP, AVGRB, AVGIB, AVGAB, AVGPB, AVGRE,
     *   AVGIE, AVGAE, AVGPE, WTE, WTB, TWOPI, PHSCHK, CHB, CHE, CHSEP
      LOGICAL   FLGWT
      INCLUDE 'UVBAS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      SAVE NPOLN, NFREQ, NIF, CHB, CHE, CHSEP, PHSCHK, TWOPI
C-----------------------------------------------------------------------
      IF (NUMVIS.EQ.1) THEN
C                                       Set up on first call
         TWOPI = 8.0 * ATAN (1.0)
         PHSCHK = 4.0 * ATAN (1.0)
         NPOLN = CATBLK(KINAX+JLOCS)
         NFREQ = CATBLK(KINAX+JLOCF)
         IF (JLOCIF.GT.0) THEN
            NIF = CATBLK(KINAX+JLOCIF)
         ELSE
            NIF = 1
            END IF
         CHB = (APARM(2) + APARM(1)) / 2.0
         CHE = (APARM(4) + APARM(3)) / 2.0
         CHSEP = CHE - CHB
         CHSEP = MAX (1.0, CHSEP)
         END IF
      IRET = 0
      IF (NUMVIS.GT.0) THEN
C                                       Loop over IF
         DO 400 IIF = 1,NIF
C                                       Loop over Stokes
            DO 350 IS = 1,NPOLN
C                                       Offset in Stokes and IF
               OFF = (IS - 1) * INCSI + (IIF - 1) * INCIFI + 1
               LIMIT1 = APARM(1) + 0.5
               LIMIT2 = APARM(2) + 0.5
               SUMRB = 0.0
               SUMIB = 0.0
               COUNTB = 0.0
               SUMRE = 0.0
               SUMIE = 0.0
               COUNTE = 0.0
               INDEX = OFF + (LIMIT1-1) * INCFI
C                                       Interpolate in Amp and phase.
C                                       Sum Start patch
               DO 100 LOOP = LIMIT1,LIMIT2
                  IF (VIS(3,INDEX).GT.0.0) THEN
                     SUMRB = SUMRB + VIS(1,INDEX)
                     SUMIB = SUMIB + VIS(2,INDEX)
                     COUNTB = COUNTB + 1.0
                     END IF
                  INDEX = INDEX + INCFI
 100              CONTINUE
               LIMIT1 = APARM(3) + 0.5
               LIMIT2 = APARM(4) + 0.5
               INDEX = OFF + (LIMIT1-1) * INCFI
C                                       Sum end patch
               DO 200 LOOP = LIMIT1,LIMIT2
                  IF (VIS(3,INDEX).GT.0.0) THEN
                     SUMRE = SUMRE + VIS(1,INDEX)
                     SUMIE = SUMIE + VIS(2,INDEX)
                     COUNTE = COUNTE + 1.0
                     END IF
                  INDEX = INDEX + INCFI
 200              CONTINUE
C                                       Beginning average
               AVGRB = 0.0
               AVGIB = 0.0
               COUNT = 0.0
               IF (COUNTB.GT.0.1) THEN
                  AVGRB = SUMRB / COUNTB
                  AVGIB = SUMIB / COUNTB
                  COUNT = 1.0
                  END IF
               AVGAB = SQRT (AVGRB*AVGRB + AVGIB*AVGIB)
               AVGPB = ATAN2 (AVGIB, AVGRB+1.0E-20)
C                                       End average
               AVGRE = 0.0
               AVGIE = 0.0
               IF (COUNTE.GT.0.1) THEN
                  AVGRE = SUMRE / COUNTE
                  AVGIE = SUMIE / COUNTE
                  COUNT = COUNT + 1.0
                  END IF
               AVGAE = SQRT (AVGRE*AVGRE + AVGIE*AVGIE)
               AVGPE = ATAN2 (AVGIE, AVGRE+1.0E-20)
C                                       To interpolate in phase - assume
C                                       difference is less than 180 deg.
               IF (ABS (AVGPE-AVGPB) .GT. PHSCHK) AVGPE = AVGPE - TWOPI
     *            * NINT ((AVGPE-AVGPB) / TWOPI)
               FLGWT = COUNT.LE.0.1
               INDEX = OFF + (LBCHAN - 1) * INCFI
               INDEX2 =  (IS - 1) * INCSO + (IIF - 1) * INCIFO + 1
      INCLUDE 'INCS:ZVND.INC'
               DO 300 LOOP = LBCHAN,LECHAN
C                                       Interpolate correct weights
                  WTB = 0.0
                  IF (LOOP.LE.CHB) THEN
C                                       At beginning
                     WTB = 1.0
                  ELSE IF (LOOP.GE.CHE) THEN
C                                       At end
                     WTB = 0.0
                  ELSE
C                                       Between - interpolate
                     WTB = (CHE-LOOP) / CHSEP
                     END IF
                  WTE = 1.0 - WTB
C                                       Get interpolated value
                  AVGA = WTB * AVGAB + WTE * AVGAE
                  AVGP = WTB * AVGPB + WTE * AVGPE
C                                       Convert to real, imaginary
                  AVGR = AVGA * COS (AVGP)
                  AVGI = AVGA * SIN (AVGP)
C                                       Subtract from vis.
                  RESULT(1,INDEX2) = VIS(1,INDEX) - AVGR
                  RESULT(2,INDEX2) = VIS(2,INDEX) - AVGI
                  RESULT(3,INDEX2) = VIS(3,INDEX)
                  IF (FLGWT) RESULT(3,INDEX2) = - ABS (VIS(3,INDEX))
                  INDEX = INDEX + INCFI
                  INDEX2 = INDEX2 + INCFO
 300              CONTINUE
 350           CONTINUE
 400        CONTINUE
         END IF
C
 999  RETURN
      END
