LOCAL INCLUDE 'UVAVG.INC'
C                                       Local include for UVAVG
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   CATIN(256), SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS,
     *   JBUFSZ, NANT, ILOCWT, MTYPE, NVOUT, LUNO, INDI, INDO,
     *   OLDCNO, NEWCNO, LRECO, NRPRMO, RNXRET, NSUBAR
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XNAMOU(3),
     *   XCLAOU(2), XOPCOD, XOPTYP
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, OPCODE*4,
     *   OPTYPE*4, HISCRD(10)*64, ONAME*48
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XBCHAN, XECHAN, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), XDOAC, XSOUT,
     *   XDISO, XINC, YINC, ZINC, XCENT, SCRBUF(256), BUFF2(UVBFSS),
     *   DIFPIX
      LOGICAL   LTB, ISCOMP
      COMMON /BUFRS/ SCRBUF, BUFF2, JBUFSZ
      COMMON /OLDHDR/ CATIN, LTB, NANT
      COMMON /INPTS/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XTIME, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF, XBCHAN, XECHAN,
     *   XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER,
     *   XSMOTH, XDOAC, XNAMOU, XCLAOU, XSOUT, XDISO, XINC, YINC, ZINC,
     *   XOPCOD, XOPTYP, XCENT
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, OPCODE, OPTYPE,
     *   HISCRD, ONAME
      COMMON /NEW/ SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS, ILOCWT, MTYPE,
     *   NVOUT, ISCOMP, LUNO, INDI, INDO, OLDCNO, NEWCNO, LRECO, NRPRMO,
     *   RNXRET, NSUBAR, DIFPIX
LOCAL END
      PROGRAM UVAVG
C-----------------------------------------------------------------------
C! Time average or merge uv data.
C# UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2006-2012, 2014-2016
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
C   The primary function of UVAVG is to average data which it does
C   if any OPCODE except 'MERG' is used.
C
C   UVAVG averages a uv data set to a maximum time given by YINC.
C   Data can be in either 'TB' or 'BT' order.  The accumulation buffer
C   is dynamic and may grow to the size needed.
C   The data is averaged weighted by the data weights.  The PARMS are
C   averaged weighted by the largest weight in the data record except
C   the baseline number, for which the last one is passed.
C
C   For OPCODE='MERG', UVAVG performs the merge function previously
C   by VBMRG.  The time specified by YINC is taken to be the average
C   time of the input data.  In each input time on each baseline,
C   only one data point is selected and passed.  This is useful for
C   VLB data processed with multiple passes so that there are multiple
C   copies of some data.  It is also useful for putting together
C   polarization data sets when different polarizations were processed
C   separately.
C
C   If XINC if greater than 1, only every XINC'th group of output
C   records (eg all baselines for an average interval) will be written.
C   This should be useful in creating small data sets that still have
C   about the uv coverage of a large data set and still has points
C   appropriately alligned for self cal.  Might be used, for example
C   in the first iterations of hybrid mapping.
C
C   Based on program AVER
C   Inputs:
C   AIPS adverb  Prg. name.          Description.
C   INNAME         NAMEIN        Name of input UV data.
C   INCLASS        CLAIN         Class of input UV data.
C   INSEQ          SEQIN         Seq. of input UV data.
C   INDISK         DISKIN        Disk number of input UV data.
C   OUTNAME        NAMOUT        Name of the output uv file.
C   OUTCLASS       CLAOUT        Class of the output uv file.
C   OUTSEQ         SEQOUT        Seq. number of output uv data.
C   OUTDISK        DISKO         Disk number of the output file.
C   YINC           YINC          Integration time (sec) min=0.2
C   XINC           XINC          Write only XINC'th output recs.
C   ZINC           ZINC          Input averaging time (sec)
C   OPCODE         OPCODE        Average or merge.
C   UVAVG Programmer R. C. Walker, Feb. 1984.
C   Removed adjustment of average time by 0.95  Dec 29, 1991 RCW
C   Tried to avoid thrashing on SPARCs by changing ACSIZE from
C      5000000 to 2500000 - worked!   Dec. 29, 1991 RCW
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGM*6
      INCLUDE 'UVAVG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'UVAVG '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UVAVIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Average or merge data
      CALL UVAVUV (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       HI, tables
      CALL UVAVHI
C                                       close down
 990  CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE UVAVIN (PRGM, JERR)
C-----------------------------------------------------------------------
C   UVAVIN gets input parameters for UVAVG and creates an output file
C   if necessary.
C   Inputs:  PRGM   C*6      Task name
C   Output:  JERR   I        Error code: quit if > 0.
C-----------------------------------------------------------------------
      CHARACTER STAT*4, PRGM*6, UTYPE*2
      INTEGER   IROUND, JERR, NPARM, IERR, NUMAN(513), LUN, ITEMP, I,
     *   NFREQ, INCX
      REAL      FACT, RTEMP, RPARM(20), CATR(256)
      DOUBLE PRECISION CATD(128)
      LOGICAL   F, MATCH
      HOLLERITH CATH(256)
      INCLUDE 'UVAVG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATH, CATR, CATD)
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Initialize I/O
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
      CALL SELINI
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
      NUMHIS = 0
C                                       Get input parameters.
      NPARM = 19 + 151
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMEI, SCRBUF, IERR)
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
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
      YINC = MAX (0.2, YINC) / 86400.0
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      SELQUA = IROUND (XQUAL)
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
C                                       Check OPTYPE
      MTYPE = 0
      IF (OPTYPE.EQ.'CROS') MTYPE = 1
      IF (OPTYPE.EQ.'AUTO') MTYPE = 2
      IF ((OPCODE.EQ.'MERG') .AND. (OPTYPE.EQ.'AUTO')) DOACOR = .TRUE.
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
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, SCRBUF, IERR)
C                                       Error finding file.
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', SCRBUF, IERR)
C                                       Error copying CATBLK.
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Copy old CATBLK
      CALL COPY (256, CATBLK, CATIN)
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
      LTB = F
      NANT = 2
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
C                                       Check if sort order 'TB' or 'BT'
      IF ((ISORT.NE.'BT') .AND. (ISORT.NE.'TB')) THEN
         WRITE (MSGTXT,1050) ISORT
         JERR = 5
         GO TO 990
         END IF
C                                       need max ant number for TB
C                                       If 'TB', see if accum. buffers
C                                       will be large enough
      LTB = (ISORT.EQ.'TB')

C                                       Get number of antennas from AN
C                                       file.
      LUN = 29
      CALL GETNAN (DISKIN, OLDCNO, CATIN, LUN, SCRBUF, NUMAN, IERR)
      IF (IERR.EQ.0) THEN
         IF (SUBARR.GT.0) THEN
            NANT = NUMAN(1+SUBARR)
         ELSE
            NANT = 0
            DO 52 I = 1,NUMAN(1)
               NANT = MAX (NANT, NUMAN(1+I))
 52            CONTINUE
            END IF
         NSUBAR = NUMAN(1)
         END IF
C                                       If failed, assume 28.
      IF ((IERR.NE.0) .OR. (NANT.EQ.0)) THEN
         WRITE (MSGTXT,1052)
         CALL MSGWRT (6)
         NANT = 28
         END IF
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, BUFF2, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1035) JERR
         GO TO 990
         END IF
      CALL UVGET ('CLOS', RPARM, BUFF2, IERR)
C                                       restore subarray if needed
      SUBARR = IROUND (XSUBA)
C                                       Put new values in CATBLK.
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
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMOUT, CLAOUT,
     *   SEQOUT)
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
C                                       Adjust output size if ZINC is
C                                       non-zero
      IF ((ZINC.GT.0.0) .AND. (YINC.GT.0.0)) THEN
         FACT = 3 * ZINC / (YINC * 86400.0)
         FACT = MIN (1.0, FACT)
         ITEMP = CATBLK(KIGCN)
         RTEMP = ITEMP
         RTEMP = RTEMP * FACT
         CATBLK(KIGCN) = IROUND(RTEMP)
         END IF
      NVOUT = CATBLK(KIGCN)
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CCNO, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1055) IERR
            GO TO 990
            END IF
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
         IF ((CCNO.NE.OLDCNO) .OR. (DISKO.NE.DISKIN)) THEN
            MSGTXT = 'UVAVIN: MAY OVERWRITE INPUT FILE ONLY.  QUITTING'
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
      SEQOUT = CATBLK(KIIMS)
C                                       Save output file info
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      LRECO = LREC
      NRPRMO = NRPARM
C                                        Put input file in READ
      UTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, 'READ', SCRBUF, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
C                                       copy header keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVAVIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('UVAVIN: ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,
     *   ' DISK=',I3,' USID=',I5)
 1035 FORMAT ('UVAVIN: UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('UVAVIN: ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('UVAVIN: SORT ORDER ',A2,' NOT BT OR TB AS REQUIRED')
 1052 FORMAT ('UVAVIN: COULD NOT GET NUMBER OF ANTENNAS FROM AN FILE',
     *   ' - ASSUME 28')
 1055 FORMAT ('UVAVIN: ERROR',I3,' CREATING OUTPUT FILE')
 1065 FORMAT ('UVAVIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE UVAVUV (IRET)
C-----------------------------------------------------------------------
C   UVAVUV sends uv data one point at a time to the average/merge
C   routine which also writes the modified data if requested.
C   Input in common:
C      ISCOMP   L   If true data is compressed
C   Output:
C      IRET     I   Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER    IRET
C
      INTEGER    IPTRO, ILENBU, NIOLIM, NIOUT, KBIND, IA1, IA2, ISUB,
     *   VO, BO, NUMVIS, XCOUNT, CATMP(256), NWORDS, ISUB1, ISUB2,
     *   TOTVIS
      LOGICAL   T, F
      INCLUDE 'UVAVG.INC'
      REAL      BASEL, VIS(UVBFSS), RPARM(20), VBUFF(2)
      DOUBLE PRECISION UVSCAL
      LONGINT   VBPTR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DRNX.INC'
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      LUNO = 17
      IF (SUBARR.GT.0) THEN
         ISUB1 = SUBARR
         ISUB2 = SUBARR
      ELSE
         ISUB1 = 1
         ISUB2 = NSUBAR
         END IF
C                                       make buffer
      NWORDS = 1
      IF (LTB) NWORDS = (NANT * (NANT+1)) / 2
      NWORDS = ((1 + NRPRMO + 3* ((LRECO-NRPRMO)/CATBLK(KINAX))) *
     *   (NWORDS + 3) - 1) / 1024 + 2
      CALL ZMEMRY ('GET ', 'UVAVUV', NWORDS, VBUFF, VBPTR, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'ALLOCATE MEMORY'
         GO TO 990
         END IF
      NWORDS = NWORDS * 1024
C                                       Open vis file for write
C                                       for IND=2 only
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, ONAME, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, ONAME, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT'
         GO TO 990
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT WRITE'
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       make an index table
      CALL RNXGET (DISKIN, OLDCNO, CATIN)
      CALL RNXINI (DISKO, NEWCNO, CATBLK, RNXRET)
C                                       adjust min record gap since
C                                       we know what it should be
      RNXEPS = MAX (RNXEPS, 0.9*YINC)
C                                       uvw scale factor
      IF ((FREQ.GT.0.0D0) .AND. (UVFREQ.GT.0.0D0)) THEN
         UVSCAL = FREQ / UVFREQ
      ELSE
         UVSCAL = 1.0D0
         END IF
C                                       Init counters.
      TOTVIS = 0
C                                       Loop
      DO 110 SUBARR = ISUB1,ISUB2
C                                       defend cat header from UVGET
         CALL COPY (256, CATBLK, CATMP)
C                                       Open and init for read
         CALL UVGET ('INIT', RPARM, VIS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT INPUT'
            GO TO 990
            END IF
         CALL COPY (256, CATMP, CATBLK)
         NUMVIS = 0
C                                       Read vis. record.
 100     CALL UVGET ('READ', RPARM, VIS, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ INPUT'
            GO TO 990
C                                       Loop over buffer
         ELSE IF (IRET.EQ.0) THEN
            IF (ILOCB.GE.0) THEN
               BASEL = RPARM(1+ILOCB)
               IA1 = BASEL / 256. + 0.1
               IA2 = BASEL - IA1*256. + 0.1
               ISUB = (BASEL - IA1*256 - IA2) * 100.0 + 1.1
            ELSE
               IA1 = RPARM(1+ILOCA1) + 0.1
               IA2 = RPARM(1+ILOCA2) + 0.1
               ISUB = RPARM(1+ILOCSA) + 0.1
               END IF
            NUMVIS = NUMVIS + 1
            RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
            RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
            RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
            CALL UVAVDO (NUMVIS, RPARM(1+ILOCT), IA1, IA2, ISUB, VIS,
     *         RPARM, NWORDS, VBUFF(1+VBPTR), NIOLIM, IPTRO, NIOUT,
     *         XCOUNT, IRET)
C                                       Error (fatal)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
               END IF
            GO TO 100
            END IF
C                                       Final call to UVAVDO.
         NUMVIS = -2
         IF (SUBARR.EQ.ISUB2) NUMVIS = -1
         CALL UVAVDO (NUMVIS, RPARM(1+ILOCT), IA1, IA2, ISUB, VIS,
     *      RPARM, NWORDS, VBUFF(1+VBPTR), NIOLIM, IPTRO, NIOUT, XCOUNT,
     *      IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1120) IRET
            GO TO 990
            END IF
         TOTVIS = TOTVIS + XCOUNT
         NVOUT = NVOUT - XCOUNT
C                                       Close input file
         CALL UVGET ('CLOS', RPARM, VIS, IRET)
 110     CONTINUE
C                                       close NX table
      CALL RNXCLS (RNXRET)
C                                       Compress and close
C                                       output file.
      NVIS = TOTVIS
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
C
      CALL ZMEMRY ('FREE', 'UVAVUV', NWORDS, VBUFF, VBPTR, IRET)
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
 1000 FORMAT ('UVAVUV: ERROR',I3,' DOING ',A)
 1120 FORMAT ('UVAVUV: UVAVDO ERROR',I3)
      END
      SUBROUTINE UVAVHI
C-----------------------------------------------------------------------
C   UVAVHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, LABEL*8
      INTEGER   LUN1, LUN2, IERR, I
      INCLUDE 'UVAVG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27, 28/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   SCRBUF, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 20
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
C                                       calibration history
      CALL CALHIS (LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      Add any other history
      IF (NUMHIS.GT.0) THEN
         WRITE (LABEL,1010) TSKNAM
         DO 15 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 20
 15         CONTINUE
         END IF
C                                       Close HI file
 20   CALL HICLOS (LUN2, .TRUE., BUFF2, IERR)
C                                       Copy tables
      CALL COPTAB (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'UVAVHI: ERROR COPYING TABLES TO OUTPUT'
         CALL MSGWRT (6)
         END IF
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, NEWCNO, DIFPIX, BUFF2(1025), BUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'UVAVHI: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', SCRBUF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVAVHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/')
      END
      SUBROUTINE UVAVDO (NUMVIS, T, IA1, IA2, ISUB, VIS, RPARM, ACSIZE,
     *   VBUFF, NIOLIM, IPTRO, NIOUT, XCOUNT, IRET)
C-----------------------------------------------------------------------
C   UVAVDO averages a uv data set in time.
C   Inputs:
C      NUMVIS     I    Visibility number, -1=> final call, no data
C                      passed but allows any operations to be completed.
C                      Data sent back will be wirtten to output file.
C      T          R    Time in days since 0 IAT on the first day for
C                      which there is data.
C      IA1        I    First antenna number
C      IA2        I    Second antenna number
C      ISUB       I    Subarray number
C      ACSIZE     I    Size of VBUFF
C      NIOLIM     I    Maximum number or records per buffer.
C      IPTRO      I    Output pointer - initialize before first call.
C      NIOUT      I    Number of records in buffer.  Init before first
C                                 call
C      XCOUNT     I    Output record counter.  Init before first call.
C   Inputs from COMMON
C      NRPARM     I    # random parameters.
C      NCOR       I    # correlators
C      CATBLK(256)I    Catalog header record.
C   In/Out:
C      RPARM    R(*)     Modified random parameter array. NB U,V,W, time
C                        and baseline should not be modified in RPARM
C      VIS      R(3,*)   Vis data in order real, imaginary, weight (Jy)
C      VBUFF    R(*)     Averaging buffer
C   Output:
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C   Output in COMMON
C      NUMHIS     I    # history entries (max. 10)
C      HISCRD(NUMHIS) C   History records
C      CATBLK     I    Catalog header block
C   Programmer: R. C. Walker  Feb. 1984
C-----------------------------------------------------------------------
      INTEGER   IA1, IA2, ISUB, ACSIZE, IRET, NIOLIM, IPTRO, NIOUT
      REAL      T, VIS(3,*), RPARM(*), VBUFF(*)
      INTEGER   NUMVIS, XCOUNT
C
C                                       Accumulation buffer
C                                       index size.
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MXINDX
      PARAMETER (MXINDX = MXBASE)
      CHARACTER OUTRAN(20)*8
      INTEGER   IIREC, IIREC2, ILEN, LUNSS, JERR, JRET, INDEX, IDAY,
     *   IBAS, BLCUR, KBAS, INDEX2, NIOFLS, INDEXV, I, KBIND, MCOR,
     *   NMCOR, LBAS, KBUFF, ITIME, IWORK(MXINDX), ITT, NCOPY, ANVER,
     *   CURSOU, LSTSOU, CURSUB, LSTSUB, KREC, K, KLIM, ANOTA(20), IP,
     *   I2TMP
      LOGICAL   MRG, ITPRT, DOTIME, DOGRID, DOGRIT
      REAL      DIV, WGT, TLAST, CT, TCHK, AMP1, AMP2, OPARM(20), TAVG,
     *   TIMES(3,MXINDX), WORK(2,MXINDX), TSUM, WSUM
      DOUBLE PRECISION X8, NXINC, XINC8
      INCLUDE 'UVAVG.INC'
      REAL      RESULT(UVBFSS), DTUTC
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      SAVE    NCOPY, DTUTC, TLAST, BLCUR, MCOR, MRG, NMCOR, LBAS,
     *   XINC8, IIREC, IIREC2, ILEN, NXINC, LSTSOU, KLIM, ANOTA, DOTIME,
     *   DOGRID, DOGRIT, TCHK, LSTSUB
      DATA LUNSS /27/
C-----------------------------------------------------------------------
      IRET = -1
C                                       Initial call
      IF (NUMVIS.EQ.1) THEN
C                                       Number of visibilities in input
C                                       and output files.
         NCOPY = LRECO - NRPRMO
C                                       Get data time - UTC
         DTUTC = 0.0
         ANVER = ISUB
         CALL ANTINI ('READ', VBUFF, FVOL(NCFILE), FCNO(NCFILE), ANVER,
     *      CATIN, LUNSS, IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0,
     *      DEGPDY, SAFREQ, RDATE, POLRXY, UT1UTC, DTUTC, TIMSYS,
     *      ANAME, XYZHAN, TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, JERR)
         DTUTC = DTUTC / 86400.0
         CALL TABIO ('CLOS', 1, IANRNO, VBUFF, VBUFF, JERR)
C                                       warning/type random parameters
         CALL LISRAN (OUTRAN, ANOTA)
C                                       Check if merge or average.
         MRG = OPCODE.EQ.'MERG'
         DOTIME = OPCODE.EQ.'TIME'
         DOGRID = OPCODE.EQ.'GRID'
         DOGRIT = OPCODE.EQ.'GRIT'
C                                       Set counters etc.
         XCOUNT = 0
         TLAST = -1.0
         BLCUR = -1
         TCHK = -1.0
         MCOR = (LRECO - NRPRMO) / CATBLK(KINAX)
         NMCOR = MCOR * 3
         LBAS = 1
         IF (LTB) LBAS = NANT*(NANT+1)/2
         XINC8 = MAX (1.0, XINC)
         CURSOU = -1
         IF (ILOCSU.GE.0) CURSOU = RPARM(1+ILOCSU) + 0.5
         LSTSOU = CURSOU
         CURSUB = ISUB
         LSTSUB = CURSUB
C                                       Limit to accumulation buffer.
         KLIM = ACSIZE - (1 + NRPRMO + NMCOR)
C                                       For sorts
         ILEN = 3
         I2TMP = MXINDX * ILEN
         CALL RFILL (I2TMP, 0.0, TIMES)
C                                       Reset accumulation buffer
         I2TMP = ACSIZE
         CALL RFILL (I2TMP, 0.0, VBUFF)
C                                       Set up to write first record
C                                       and avoid round off problems.
         NXINC = -0.999D0
         END IF
C                                       Have incoming data
      IF (NUMVIS.GE.0) THEN
C                                       Be sure antenna numbers are
C                                       in the expected order.
         IF (IA1.GT.IA2) THEN
            WRITE (MSGTXT,1000) IA1, IA2
            CALL MSGWRT (8)
            IRET = 7
            GO TO 999
            END IF
C                                       If merging but not this type
C                                       write it out
         IF (MRG) THEN
            IF (((MTYPE.EQ.1) .AND. (IA1.EQ.IA2)) .OR.
     *         ((MTYPE.EQ.2) .AND. (IA1.NE.IA2))) THEN
C                                       update NX table
               CALL RNXUPD (RPARM, RNXRET)
C                                       move data
               CALL RCOPY (NRPRMO, RPARM, BUFF2(IPTRO))
               IF (ISCOMP) THEN
                  DO 50 I = 1, MCOR
                     INDEX2 = (I-1)*3
                     RESULT(INDEX2+1) = VIS(1,I)
                     RESULT(INDEX2+2) = VIS(2,I)
                     RESULT(INDEX2+3) = VIS(3,I)
 50                  CONTINUE
                  CALL ZUVPAK (NCOPY, RESULT, BUFF2(IPTRO+ILOCWT),
     *               BUFF2(IPTRO+NRPRMO))
               ELSE
                  DO 60 I = 1,MCOR
                     INDEX2 = IPTRO + NRPRMO + (I-1)*3 - 1
                     BUFF2(INDEX2+1) = VIS(1,I)
                     BUFF2(INDEX2+2) = VIS(2,I)
                     BUFF2(INDEX2+3) = VIS(3,I)
 60                  CONTINUE
                  END IF
C                                       Update counters.
               XCOUNT = XCOUNT + 1
C                                       Writing beyond EOF ?
               IF (XCOUNT.GT.NVOUT) THEN
                  K = MAX (5000, NVOUT/10)
                  CALL UVSIZE (LRECO, K, IP)
                  CALL ZEXPND (LUNO, DISKO, ONAME, IP, IRET)
                  IF (IRET.EQ.0) THEN
                     NVOUT = NVOUT + K
                  ELSE
                     WRITE (MSGTXT,1060) IRET
                     CALL MSGWRT (8)
                     GO TO 999
                     END IF
                  END IF
C
               IPTRO = IPTRO + LRECO
               NIOUT = NIOUT + 1
C                                       Write output
               IF (NIOUT.GE.NIOLIM) THEN
C                                       Use NIOUT instead of NIOLIM
C                                       so last record ok.
                  CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOUT,
     *               KBIND, JRET)
                  IF (JRET.NE.0) THEN
                     WRITE (MSGTXT,1113) JRET
                     CALL MSGWRT (8)
                     IRET = 5
                     GO TO 999
                     END IF
                  IPTRO = KBIND
                  NIOLIM = NIOUT
                  NIOUT = 0
                  END IF
               GO TO 999
               END IF
            END IF
C                                       Get UT from data time
         CT = T - DTUTC
C                                       Get baseline.
         IBAS = 32768 * IA1 + IA2
C                                       Be sure of sort order.
         IF ((CT.LT.TCHK) .AND. (IBAS.EQ.BLCUR)) THEN
            WRITE (MSGTXT,1090)
            CALL MSGWRT (8)
            IRET = 4
            GO TO 999
            END IF
C                                       Source Number
         CURSOU = -1
         IF (ILOCSU.GE.0) CURSOU = RPARM(1+ILOCSU) + 0.5
         CURSUB = ISUB
         END IF
C                                       Check if output needed.
      IF ((CT.GT.TLAST) .OR. (NUMVIS.LT.0) .OR.
     *   ((.NOT.LTB) .AND. (IBAS.NE.BLCUR)) .OR.
     *   (CURSOU.NE.LSTSOU) .OR. (CURSUB.NE.LSTSUB)) THEN
C                                       No output for first record.
         IF (NUMVIS.NE.1) THEN
C                                       Test if output should be
C                                       skipped.
            NXINC = NXINC + 1.D0
            IF ((XINC.LE.1.5) .OR. (MOD(NXINC,XINC8).LE.0.5D0)) THEN
C                                       Loop through the accumulation
C                                       buffer, extracting an index
C                                       of time and baseline
C                                       keys, which are later sorted.
               IIREC = LBAS
C                                       Warning if MXINDX exceeded.
               IF (IIREC.GT.MXINDX) THEN
                  WRITE (MSGTXT,1160) IIREC
                  CALL MSGWRT (8)
                  IRET = 10
                  GO TO 999
                  END IF
               TSUM = 0.0
               WSUM = 0.0
               DO 100 K = 1,IIREC
                  KREC = (K-1) * (1 + NRPRMO + NMCOR) + 1
                  IF (KREC.LE.KLIM) THEN
                     WGT = VBUFF(KREC)
                     TIMES(3,K) = K
                     IF (WGT.GT.0.0) THEN
                        IF (DOTIME) THEN
                           WSUM = WSUM + WGT
                           TSUM = TSUM + VBUFF(KREC+ILOCT+1)
                           END IF
                        TIMES(1,K) = VBUFF(KREC+ILOCT+1) / WGT
                        TIMES(2,K) = 4096.0*IA1 + IA2
                     ELSE
                        TIMES(1,K) = 0.0
                        TIMES(2,K) = 0.0
                        END IF
                     END IF
  100             CONTINUE
               IF (WSUM.GT.0.0) TSUM = TSUM / WSUM
C                                       Sort index of keys TIMES
C                                       to TB or BT order.
               IIREC2 = IIREC + 2
               IF ((.NOT.DOTIME) .AND. (.NOT.DOGRID)) THEN
                  IF (LTB) THEN
                     CALL OSORT (TIMES, IIREC, IIREC2, 1, 2, ILEN, WORK,
     *                  IWORK, JERR)
                  ELSE IF (IIREC.GT.1) THEN
                     CALL OSORT (TIMES, IIREC, IIREC2, 2, 1, ILEN, WORK,
     *                  IWORK, JERR)
                     IF (JERR.GT.0) THEN
                        WRITE (MSGTXT,1104)
                        CALL MSGWRT (8)
                        IRET = 8
                        GO TO 999
                        END IF
                     END IF
                  END IF
C                                       Now write in time order.
               ITPRT = .TRUE.
               DO 126 ITIME = 1,IIREC
                  ITT  = IIREC - ITIME + 1
                  KREC = TIMES(3,ITT)
                  KBUFF = (KREC-1) * (1+NRPRMO+NMCOR) + 1
C                                       Weight for RPARM.
C                                       Will be 0.0 if no valid data
                  WGT = 0.0
                  IF (KBUFF.LE.KLIM) WGT = VBUFF(KBUFF)
C                                       No data - no output
                  IF (WGT.GT.0.0) THEN
C                                       Normalize RPARM's
C                                       Re: WGT for merge will be 1.0
                     DO 105 IP = 1,NRPRMO
                        IF (ANOTA(IP).EQ.2) THEN
                           OPARM(IP) = VBUFF(KBUFF+IP) / WGT
                        ELSE
                           OPARM(IP) = VBUFF(KBUFF+IP)
                           END IF
 105                    CONTINUE
                     IF (DOTIME) THEN
                        OPARM(1+ILOCT) = TSUM
                     ELSE IF (DOGRID) THEN
                        OPARM(1+ILOCT) = TAVG
                     ELSE
                        OPARM(1+ILOCT) = TIMES(1,ITT)
                        END IF
C                                       update NX table
                     CALL RNXUPD (OPARM, RNXRET)
                     CALL RCOPY (NRPRMO, OPARM, BUFF2(IPTRO))
C                                       Compressed?
                     IF (ISCOMP) THEN
C                                       Normalize data. Correlator loop
                        DO 110 I = 1,MCOR
                           INDEX2 = (I-1) * 3
                           INDEXV = KBUFF + NRPRMO + (I-1) * 3
                           IF (VBUFF(INDEXV+3).GT.0.0) THEN
                              DIV = 1.0 / VBUFF(INDEXV+3)
                              IF (MRG) DIV = 1.0
                              RESULT(INDEX2+1) = VBUFF(INDEXV+1) * DIV
                              RESULT(INDEX2+2) = VBUFF(INDEXV+2) * DIV
                              RESULT(INDEX2+3) = VBUFF(INDEXV+3)
C                                       No data
                           ELSE
                              RESULT(INDEX2+1) = 0.0
                              RESULT(INDEX2+2) = 0.0
                              RESULT(INDEX2+3) = -1.0
                              END IF
 110                       CONTINUE
C                                       Pack/copy to output buffer
                        INDEX2 = IPTRO + NRPRMO + (I-1)*3 - 1
                        CALL ZUVPAK (NCOPY, RESULT, BUFF2(IPTRO+ILOCWT),
     *                     BUFF2(IPTRO+NRPRMO))
                     ELSE
C                                       Uncompressed data:
C                                       Normalize data. Correlator loop
                        DO 115 I = 1,MCOR
                           INDEX2 = IPTRO + NRPRMO + (I-1)*3 - 1
                           INDEXV = KBUFF + NRPRMO + (I-1)*3
                           IF (VBUFF(INDEXV+3).GT.0.0) THEN
                              DIV = 1.0 / VBUFF(INDEXV+3)
                              IF (MRG) DIV = 1.0
                              BUFF2(INDEX2+1) = VBUFF(INDEXV+1) * DIV
                              BUFF2(INDEX2+2) = VBUFF(INDEXV+2) * DIV
                              BUFF2(INDEX2+3) = VBUFF(INDEXV+3)
C                                       No data
                           ELSE
                              BUFF2(INDEX2+1) = 0.0
                              BUFF2(INDEX2+2) = 0.0
                              BUFF2(INDEX2+3) = -1.0
                              END IF
 115                       CONTINUE
                        END IF
C                                        Update counters.
                     XCOUNT = XCOUNT + 1
C                                        Writing beyond EOF ?
                     IF (XCOUNT.GT.NVOUT) THEN
                        K = MAX (5000, NVOUT/10)
                        CALL UVSIZE (LRECO, K, IP)
                        CALL ZEXPND (LUNO, DISKO, ONAME, IP, IRET)
                        IF (IRET.EQ.0) THEN
                           NVOUT = NVOUT + K
                        ELSE
                           WRITE (MSGTXT,1060) IRET
                           CALL MSGWRT (8)
                           GO TO 999
                           END IF
                        END IF
                     IPTRO = IPTRO + LRECO
                     NIOUT = NIOUT + 1
C                                        Write output
C                                        Only actually write when
C                                        the buffer is full.
                     IF (NIOUT.GE.NIOLIM) THEN
C                                        Use NIOUT instead of NIOLIM
C                                        so last record ok.
                        CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOUT,
     *                     KBIND, JRET)
                        IF (JRET.NE.0) THEN
                           WRITE (MSGTXT,1113) JRET
                           CALL MSGWRT (8)
                           IRET = 5
                           GO TO 999
                           END IF
                        IPTRO = KBIND
                        NIOLIM = NIOUT
                        NIOUT = 0
                        END IF
C                                        Jump here if no data for bas.
                     END IF
C                                        End of baseline loop.
  126             CONTINUE
C                                        Jump here if output not
C                                        written
               END IF
C                                        Close up after last record.
            IF (NUMVIS.EQ.-1) THEN
C                                       First write any unwritten
C                                       data.
               IF (NIOUT.GT.0) THEN
                  CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOUT, KBIND,
     *               JRET)
                  IF (JRET.NE.0) THEN
                     WRITE (MSGTXT,1113) JRET
                     CALL MSGWRT (8)
                     IRET = 9
                     GO TO 999
                     END IF
                  END IF
C                                        Then flush buffers to output
               NIOFLS = 0
               CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOFLS, KBIND,
     *            JRET)
               IF (JRET.NE.0) THEN
                  WRITE (MSGTXT,1130) JRET
                  CALL MSGWRT (8)
                  IRET = 6
                  GO TO 999
                  END IF
C                                        Tell history if merge or avg.
               IF (MRG) THEN
                  NUMHIS = NUMHIS + 1
                  WRITE (HISCRD(NUMHIS),1140)
               ELSE
                  YINC = YINC * 86400.0
                  NUMHIS = NUMHIS + 1
                  WRITE (HISCRD(NUMHIS),1135) YINC
                  END IF
C                                        Tell history about reduced
C                                        output.
               IF (XINC.GT.1.0) THEN
                  NUMHIS = NUMHIS + 1
                  WRITE (HISCRD(NUMHIS),1145) XINC
                  END IF
C                                        Write no. written to history.
               NUMHIS = NUMHIS + 1
               WRITE (HISCRD(NUMHIS),1150) XCOUNT, LSTSUB
               WRITE (MSGTXT,1150) XCOUNT, LSTSUB
               CALL MSGWRT (5)
               GO TO 999
C                                        Write no. written to history.
            ELSE IF (NUMVIS.EQ.-2) THEN
               NUMHIS = NUMHIS + 1
               WRITE (HISCRD(NUMHIS),1150) XCOUNT, LSTSUB
               WRITE (MSGTXT,1150) XCOUNT, LSTSUB
               CALL MSGWRT (5)
               END IF
            END IF
C                                        Set up for next integration
CRCW                                     Removed 0.95 here
         IF ((LTB) .AND. (.NOT.DOGRID) .AND. (.NOT.DOGRIT)) THEN
            TLAST = CT + YINC
         ELSE
            IDAY = CT
            X8 = (CT-IDAY) / YINC
            TLAST = IDAY + DINT (X8) * YINC + YINC
            TAVG = TLAST - YINC/2.0
            END IF
         TCHK = TLAST - 1.1 * YINC
C                                        Reset accumulators, etc.
         I2TMP = LBAS * (1+NRPRMO+NMCOR)
         IF (I2TMP.GT.ACSIZE) I2TMP = ACSIZE
         CALL RFILL (I2TMP, 0.0, VBUFF)
         BLCUR = IBAS
         LSTSOU = CURSOU
         LSTSUB = CURSUB
         END IF
C                                        Accumulate current datum.
C                                        Get baseline number for accum.
      KBAS = 1
      IF (LTB) KBAS = IA2*(IA2-1)/2 + IA1
C                                        Get index of first element of
C                                        accumulation buffer for this
C                                        baseline.  That element will
C                                        accumulate the weight for PARMS
      KBUFF = (KBAS - 1) * (NRPRMO + 1 + NMCOR) + 1
C                                        Check that accumulation
C                                        buffer is not exceeded.
      IF (KBUFF.GT.KLIM) THEN
         WRITE (MSGTXT,1180) KBUFF
         CALL MSGWRT (8)
         IRET = 12
         GO TO 999
         END IF
C                                        Initialize parms weight.
      WGT = 0.0
      DO 220 I = 1,MCOR
         INDEX = KBUFF + NRPRMO + (I-1)*3
C                                        Check weight.
         IF (VIS(3,I).GT.0.0) THEN
C                                        Accumulate vis data.
            IF (MRG) THEN
C                                         Merge
C                                         Take correlator with the
C                                         highest weight.  Take latest
C                                         if equal weight unless there
C                                         is a big amplitude difference
               IF (VIS(3,I).EQ.VBUFF(INDEX+3)) THEN
                  AMP1 = VBUFF(INDEX+1)*VBUFF(INDEX+1) +
     *                   VBUFF(INDEX+2)*VBUFF(INDEX+2)
                  AMP2 = VIS(1,I)*VIS(1,I) + VIS(2,I)*VIS(2,I)
                  IF (AMP1.LT.0.25*AMP2) VBUFF(INDEX+3) = -10
                  IF (AMP2.LT.0.25*AMP1) VIS(3,I) = -10
                  END IF
               IF (VIS(3,I).GT.VBUFF(INDEX+3)) THEN
                  VBUFF(INDEX+1) = VIS(1,I)
                  VBUFF(INDEX+2) = VIS(2,I)
                  VBUFF(INDEX+3) = VIS(3,I)
C                                        Get PARMS from last record
C                                        from which any data is
C                                        accepted.
                  WGT = 1
                  END IF
            ELSE
C                                       Average
               VBUFF(INDEX+1) = VBUFF(INDEX+1) + VIS(1,I)*VIS(3,I)
               VBUFF(INDEX+2) = VBUFF(INDEX+2) + VIS(2,I)*VIS(3,I)
               VBUFF(INDEX+3) = VBUFF(INDEX+3) + VIS(3,I)
C                                         Use largest weight for uv etc
               WGT = MAX (WGT, VIS(3,I))
               END IF
            END IF
 220     CONTINUE
C                                         Accumulate PARMS only if some
C                                         data was good.
      IF (WGT.GT.0.0) THEN
C                                        For merge.
         IF (MRG) THEN
            DO 230 IP = 1,NRPRMO
               VBUFF(KBUFF+IP) = RPARM(IP)
 230           CONTINUE
C                                        Keep accumulated weight = 1
C                                        Unless there is no data.
            VBUFF(KBUFF) = WGT
         ELSE
            DO 240 IP = 1,NRPRMO
               IF (ANOTA(IP).EQ.2) THEN
                  VBUFF(KBUFF+IP) = VBUFF(KBUFF+IP) + RPARM(IP)*WGT
               ELSE IF (ANOTA(IP).EQ.1) THEN
                  VBUFF(KBUFF+IP) = VBUFF(KBUFF+IP) + RPARM(IP)
               ELSE
                  IF (VBUFF(KBUFF+IP).EQ.0.0) VBUFF(KBUFF+IP) =
     *               RPARM(IP)
                  END IF
 240           CONTINUE
C                                        Accumulate weights.
            VBUFF(KBUFF) = VBUFF(KBUFF) + WGT
C                                        Don't weight baseline number,
C                                        source, FQ id. or int. time
C           VBUFF(KBUFF+ILOCB+1) = RPARM(ILOCB+1)
C           IF (ILOCSU.GE.0) VBUFF(KBUFF+ILOCSU+1) = RPARM(ILOCSU+1)
C           IF (ILOCFQ.GE.0) VBUFF(KBUFF+ILOCFQ+1) = RPARM(ILOCFQ+1)
C           IF (ILOCIT.GE.0) VBUFF(KBUFF+ILOCIT+1) =
C    *         VBUFF(KBUFF+ILOCIT+1) + RPARM(ILOCIT+1)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVAVDO: ANTENNA NUMBERS NOT IN INCREASING ORDER',2I3)
 1060 FORMAT ('UVAVDO: ERROR',I6,' EXPANDING OUTPUT FILE')
 1090 FORMAT ('UVAVDO: RECORDS NOT IN TIME (TB) ORDER')
 1104 FORMAT ('UVAVDO: SOMETHING WRONG WITH SORT')
 1113 FORMAT ('UVAVDO: ERROR',I3,' WRITING VIS FILE')
 1130 FORMAT ('UVAVDO: ERROR',I3,' CLOSING VIS FILE')
 1135 FORMAT ('UVAVDO: Average time=',F9.2,' sec.')
 1140 FORMAT ('UVAVDO: Data merged by UVAVG')
 1145 FORMAT ('UVAVDO: only one in ',F6.0,' output records actually',
     *        ' written.')
 1150 FORMAT ('UVAVDO: ',I10,' Visibility records written, subarray',I3)
 1160 FORMAT ('UVAVDO: Parameter MXINDX too small (',I5,' )')
 1180 FORMAT ('UVAVDO: Parameter ACSIZE too small (',I8,' )')
      END
