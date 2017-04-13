LOCAL INCLUDE 'UVRFI.INC'
C                                       Local include for UVRFI
C
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   CATIN(256), SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS,
     *   JBUFSZ, NANT, ILOCWT, NVOUT, LUNO, INDI, INDO,
     *   OLDCNO, NEWCNO, LRECO, NRPRMO, RNXRET
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XNAMOU(3),
     *   XCLAOU(2), XOPCOD, XOPTYP
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, OPCODE*4,
     *   OPTYPE*4, HISCRD(10)*64, ONAME*48
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XBCHAN, XECHAN, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), XDOAC,
     *   XSOUT, XDISO, XINC, YINC, ZINC, SCRBUF(256), BUFF2(UVBFSS),
     *   APARM(10), DTIME, DFREQ, FLMAX
Cnew
      REAL SMOTH(3)
      REAL FLUX, GAIN
      INTEGER   HFREQ, NCOMP, NATT, NITER, NOSUB, NOSUB2, NINTR
      LOGICAL   LTB, ISCOMP, FITRIN
      COMMON /BUFRS/ SCRBUF, BUFF2, JBUFSZ
      COMMON /OLDHDR/ CATIN, LTB, NANT
      COMMON /INPTS/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XTIME, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF, XBCHAN, XECHAN,
     *   XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER,
     *   XSMOTH, XDOAC, XNAMOU, XCLAOU, XSOUT, XDISO, XINC, YINC, ZINC,
     *   XOPCOD, APARM, XOPTYP, FLMAX, FLUX, GAIN,
     *   HFREQ, NCOMP, NATT, NITER, NOSUB, NOSUB2, NINTR, DTIME, DFREQ
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, OPCODE, OPTYPE,
     *   HISCRD, ONAME
      COMMON /NEW/ SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS, ILOCWT,
     *   NVOUT, ISCOMP, LUNO, INDI, INDO, OLDCNO, NEWCNO, LRECO, NRPRMO,
     *   RNXRET, FITRIN,
CNEW
     *   SMOTH
LOCAL END
      PROGRAM UVRFI
C-----------------------------------------------------------------------
C! Mitigate RFI by Fourier transform or fitting circle.
C# UV
C-----------------------------------------------------------------------
C;  Copyright (C) 2009-2012, 2015-2016
C;  Associated Univercities, Inc. Washington DC, USA.
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
C   The UVRFI  mitigates the RFI by Fourier transform the time sequence
C   of the visibilities  (for each baseline) and following CLEAN of the
C   spectral components; or by fitting circle at the RE/IM vis. plane.
C   The true visibility (from the sky source) appears at zero frequency,
C   as the result of the fringe stopping procedure at the correlator.
C   The RFI visibility from the Earth located sources  should appear at
C   non zero frequency as the result of the fringe stopping procedure
C   at the correlator, which shifts the initial zero fringe rate
C   (the RFI source is immovable relatively the array).
C   The RFI visibility from a satalite  located source  should appear at
C   non zero frequency: difference of the fringe rate from the faster
C   moving satalite and the result of the fringe stopping procedure
C   at the correlator.
C   UVRFI.FOR uses UVAVG.FOR as the initial Fortran codes.
C   UVRFI accumulates a uv data set at a maximum time given by YINC.
C   Data must be 'BT' order!!!!!!!.  The accumulation buffer
C   is dynamic and may grow to the size needed.
C   The data is averaged weighted by the data weights.  The PARMS are
C   averaged weighted by the largest weight in the data record except
C   the baseline number, for which the last one is passed.

C   If XINC if greater than 1, only every XINC'th record  inside of the
C   accumulating interval will be accumulated

C   This version as I discovered 03/25/11 includes:
C        the smothing of  amplitude and
C        commented simulation of 2 complex exponents
C   This is a copy of  UVRFI.FOR at he system as March 25 2011.
C   including the AMP smothing to put it in the system!!!!
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
C   UVRFI Programmer L.R. Kogan Dec 2009
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGM*6
      INCLUDE 'UVRFI.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'UVRFI '/
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
C   UVAVIN gets input parameters for UVRFI and creates an output file
C   if necessary.
C   Inputs:  PRGM   C*6      Task name
C   Output:  JERR   I        Error code: quit if > 0.
C-----------------------------------------------------------------------
      CHARACTER STAT*4, PRGM*6, UTYPE*2
      INTEGER   IROUND, JERR, NPARM, IERR, NUMAN(513), LUN, ITEMP, I,
     *   NFREQ
      REAL      FACT, RTEMP, RPARM(20), YINCS
      LOGICAL   F, MATCH
      HOLLERITH CATH(256)
Cnew
      REAL CATR(256)
      DOUBLE PRECISION CATD(128)
C
      INCLUDE 'UVRFI.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
Cnew
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
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
C                                       Get input parameters.
C      NPARM = 19 + 149
C                                       add APARM, PDVER
      NPARM = 19 + 150 + 10
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
C                                       store YINC in sec
      YINCS = YINC
      YINC = MAX (0.2, YINC) / 86400.0
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
C                                       default CEXP
      IF (OPCODE .NE. 'CIRC') OPCODE = 'CEXP'
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      SELQUA = IROUND (XQUAL)

C                                       CEXP
      IF (OPCODE .EQ. 'CEXP') THEN
         DTIME = APARM(1)
         HFREQ = APARM(2)
         IF (HFREQ .EQ. 0) HFREQ = 1.0/APARM(1)/2.0/APARM(3)
         DFREQ = APARM(3)
         NCOMP = APARM(4)
         IF (NCOMP .EQ. 0) NCOMP =1
         NOSUB = APARM(5)
         NOSUB2 = APARM(9)
         NATT = APARM(10)
         IF (NATT .EQ. 0) NATT = 1
         FLMAX = APARM(6)
         IF (FLMAX .LT. 0.001) FLMAX = 10000
         GAIN = APARM(7)
         IF (GAIN .LT. 0.01) GAIN = 0.1
         FLUX = APARM(8)
         END IF
C                                       number of time intervals used
C                                       to esimate the dynamic memory
      NINTR = (YINCS + 1) / ZINC
C                                       CIRC
      IF (OPCODE .EQ. 'CIRC') THEN
         NITER = APARM(1)
         IF (NITER .EQ. 0) NITER = 3
         FITRIN = APARM(2) .LT. 0.5
         END IF
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
C
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
C      CALL RCOPY (3, XSMOTH, SMOOTH)
Cnew
      CALL RCOPY (3, XSMOTH, SMOTH)
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
Cnew
C                                       copy the CATBLK to use in image
C                                       output file
C   Do I need it? if I have CATIN?
C      CALL COPY (256, CATBLK, CATSAV)
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       check sort order
      IF (ISORT(1:1).NE.'B') THEN
         WRITE (MSGTXT,1100) ISORT
         JERR = 1
         GO TO 990
         END IF
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
C                                       Put new values in CATBLK.
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
         FACT = ZINC / (YINC * 86400.0)
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
C
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
 1100 FORMAT ('You have sort order ',A2,
     *   '. Sort the data to BT by UVSRT')
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
     *   VO, BO, NUMVIS, XCOUNT, CATMP(256), NWORDS
      LOGICAL   T, F
      INCLUDE 'UVRFI.INC'
      REAL      BASEL, VIS(UVBFSS), RPARM(20), VBUFF(2)
      DOUBLE PRECISION UVSCAL
      LONGINT   VBPTR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      LUNO = 17
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT INPUT'
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
C                                       make buffer
      NWORDS = NINTR
      IF (LTB) NWORDS = NWORDS * (NANT * (NANT+1)) / 2
C                                       Eric added 1024 to fix the
C                                       problem near ZEXPAND
C                                       I changed NWORDS=1 to
C                                       NWORDS=NINTR
      NWORDS = ((1 + NRPRMO + 3* ((LRECO-NRPRMO)/CATBLK(KINAX))) *
     *   (NWORDS + 3)) / 1024 + 2
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
      IF ((FREQ.GT.0.0D0) .AND. (UVFREQ.GT.0.0D0)) THEN
         UVSCAL = FREQ / UVFREQ
      ELSE
         UVSCAL = 1.0D0
         END IF
C                                       Init counters.
      NUMVIS = 0
C                                       Loop
C                                       Read vis. record.
 100  CONTINUE
C                                       Read vis. record.
         CALL UVGET ('READ', RPARM, VIS, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ INPUT'
            GO TO 990
C                                       Loop over buffer
         ELSE IF (IRET.EQ.0) THEN
            IF (ILOCB.GE.0) THEN
               BASEL = RPARM(1+ILOCB)
               IA1 = BASEL / 256. + 0.1
               IA2 = BASEL - IA1*256. + 0.1
               ISUB = (BASEL - IA1*256 - IA2) * 100.0 + 1.001
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
      NUMVIS = -1
      CALL UVAVDO (NUMVIS, RPARM(1+ILOCT), IA1, IA2, ISUB, VIS, RPARM,
     *   NWORDS, VBUFF(1+VBPTR), NIOLIM, IPTRO, NIOUT, XCOUNT, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
         END IF
C                                       Close input file
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
C                                       close NX table
      CALL RNXCLS (RNXRET)
C                                       Compress and close
C                                       output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
C                                       close output map
C   INTEGER   IVOL, IDSLOT, IDLUN, IDIND, CATBLK(256), IDBUFF(256), IERR
C   LOGICAL   SAVE
C      CALL MAPCLS ('INIT', IDVOL, IDSLOT, IDLUN, IDIND, CATBLK, SAVE,
C     *   IDBUFF, IERR)


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
      INCLUDE 'UVRFI.INC'
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
C   Programmer: L. Kogan DEC 2009
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IA1, IA2, ISUB, ACSIZE, NIOLIM, IPTRO, NIOUT,
     *   XCOUNT, IRET
      REAL      T, VIS(3,*), RPARM(*), VBUFF(*)
C                                       For average in intervals
      INTEGER MAMAV
      PARAMETER (MAMAV = 1000)
      REAL      AMPSAV, AMP, FIARR(MAMAV), AMPSUM(MAMAV),
     *   X0ST(MAMAV), Y0ST(MAMAV)
      INTEGER   NINTER, ILEFT(300), IRIGHT(300), ISOL(300)
C
      INTEGER   NOSUBI
      REAL      MEANAT, MEANAC, MEANRE, MEANIM
C                                       for subroutine CIRC
      INTEGER   KTIME, NTIME, INTIME, LTIME, HTIME
C
      INTEGER NBUFF

      INTEGER MAXAR
      PARAMETER (MAXAR = 1000)
      INTEGER   MINT(MAXAR)
      REAL      FUNCX(MAXAR), FUNCY(MAXAR), WEIGHT(MAXAR)
C
      INTEGER NUMFLG
      PARAMETER (NUMFLG = 1024)
      INTEGER L, IFLAG, KFLAG, NFLAG(NUMFLG)
      REAL PREV, CURR, NEXT
C
C                                       Accumulation buffer
C                                       index size.
      INTEGER   MXINDX
      PARAMETER (MXINDX = 1136)
      CHARACTER OUTRAN(20)*8
      INTEGER   IIREC, IIREC2, ILEN, LUNSS, JERR, JRET, INDEX,
     *   IBAS, BLCUR, KBAS, INDEX2, NIOFLS, INDEXV, I, KBIND, MCOR,
     *   NMCOR, LBAS, KBUFF, ITIME, IWORK(MXINDX), ITT, NCOPY, ANVER,
     *   CURSOU, LSTSOU, CURSUB, LSTSUB, KREC, K, KLIM, ANOTA(20), IP,
     *   I2TMP

      LOGICAL   ITPRT, SMALL
      REAL      WGT, TLAST, CT, TCHK, OPARM(20),
     *   TIMES(3,MXINDX), WORK(2,MXINDX)
      REAL      X0, Y0, RADI, DELTA
      DOUBLE PRECISION NXINC, XINC8
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVRFI.INC'
      REAL      RESULT(UVBFSS), DTUTC
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      SAVE    NCOPY, DTUTC, TLAST, BLCUR, MCOR, NMCOR, LBAS, TCHK,
     *   XINC8, IIREC, IIREC2, ILEN, NXINC, LSTSOU, KLIM, ANOTA, LSTSUB
      DATA LUNSS /27/
C-----------------------------------------------------------------------
      IRET = -1
C                                       Initial call
      IF (NUMVIS.EQ.1) THEN
         NUMHIS = 0
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
      IF (NUMVIS.NE.-1) THEN
C                                       Be sure antenna numbers are
C                                       in the expected order.
         IF (IA1.GT.IA2) THEN
            WRITE (MSGTXT,1000) IA1, IA2
            CALL MSGWRT (8)
            IRET = 7
            GO TO 999
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
      IF ((CT.GT.TLAST) .OR. (NUMVIS.EQ.-1) .OR.
     *   ((.NOT.LTB) .AND. (IBAS.NE.BLCUR)) .OR.
     *   (CURSOU.NE.LSTSOU) .OR. (CURSUB.NE.LSTSUB)) THEN
C
C                                       store the number of time
C                                       intervals
         NTIME = KTIME
C                                       zero time counter in the
C                                       interval of averaging
         KTIME = 0
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
               DO 100 K = 1,IIREC
                  KREC = (K-1) * (1 + NRPRMO + NMCOR) + 1
                  IF (KREC.LE.KLIM) THEN
                     WGT = VBUFF(KREC)
                     TIMES(3,K) = K
                     IF (WGT.GT.0.0) THEN
                        TIMES(1,K) = VBUFF(KREC+ILOCT+1) / WGT
                        TIMES(2,K) = 4096.* IA1 + IA2
                     ELSE
                        TIMES(1,K) = 0.0
                        TIMES(2,K) = 0.0
                        END IF
                     END IF
  100             CONTINUE
C                                       Sort index of keys TIMES
C                                       to TB or BT order.
               IIREC2 = IIREC + 2
               IF (LTB) THEN
                  CALL OSORT (TIMES, IIREC, IIREC2, 1, 2, ILEN, WORK,
     *               IWORK, JERR)
               ELSE IF (IIREC.GT.1) THEN
                  CALL OSORT (TIMES, IIREC, IIREC2, 2, 1, ILEN, WORK,
     *               IWORK, JERR)
                  IF (JERR.GT.0) THEN
                     WRITE (MSGTXT,1104)
                     CALL MSGWRT (8)
                     IRET = 8
                     GO TO 999
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
C                                       the record correspons to the
C                                       previous baseline!
C
                  IF (WGT.GT.0.0 .AND. KBUFF.EQ.NBUFF) THEN
C                                       Normalize RPARM's
C                                       Re: WGT for merge will be 1.0
                     DO 105 IP = 1,NRPRMO
                        IF (ANOTA(IP).EQ.2) THEN
                           OPARM(IP) = VBUFF(KBUFF+IP) / WGT
                        ELSE
                           OPARM(IP) = VBUFF(KBUFF+IP)
                           END IF
 105                    CONTINUE
C                                       update NX table
                     CALL RNXUPD (OPARM, RNXRET)
                     CALL RCOPY (NRPRMO, OPARM, BUFF2(IPTRO))
C                                       Mean amplitude through the
C                                       channels
                        MEANAC = 0
                        L = 0
                        IFLAG = 0
                        DO 109 I = 1,MCOR
C                                       cycle by time inside of time
C                                       interval
C                                       mean value of re/im through the
C                                       time
                           MEANRE = 0
                           MEANIM = 0
                           DO 106 INTIME = 1, NTIME
                              INDEXV = KBUFF + NRPRMO + (I-1) * 3
     *                           + (INTIME-1)*MCOR*3
C                                       sum RE and IM through time
                              MEANRE = MEANRE + VBUFF(INDEXV+1)
                              MEANIM = MEANIM + VBUFF(INDEXV+2)
  106                         CONTINUE
C                                       mean amplitude through the time
                           MEANAT = SQRT(MEANRE**2 + MEANIM**2)/NTIME
C                                       sum of the mean amplitude
C                                       through the channels
                           MEANAC = MEANAC + MEANAT
C                                       start picking up the 3 channels
C                                       peaks
                           L = L + 1
                           IF (L .EQ. 1) PREV = MEANAT
                           IF (L .EQ. 2) CURR = MEANAT
                           IF (L .EQ. 3) THEN
                              NEXT = MEANAT
C                              IF (CURR .GT. 2.2*PREV  .AND.
C     *                           CURR .GT. 2.2*NEXT) THEN

                              IF ((CURR .GT. 1.3*PREV  .AND.
     *                           CURR .GT. 1.3*NEXT) .OR.
     *                          (CURR .LT. 0.7*PREV  .AND.
     *                           CURR .LT. 0.7*NEXT)) THEN

                                 L = 1
                                 PREV = NEXT
C                                       number the flagged channel
                                 IFLAG = IFLAG + 1
C                                       flag channel numbers
                                 NFLAG(IFLAG) = I - 1
                              ELSE
                                 L = 2
                                 PREV = CURR
                                 CURR = NEXT
                                 END IF
                              END IF
C                                       end picking up the 3 channels
C                                       peaks
  109                      CONTINUE
C                                       number of flagged channels
                        KFLAG = IFLAG
C                                       mean amplitude through the
C                                       channels
CHERE WE CONSIDER THAT MCOR INCLUDES ONLY CHANNELS => NO POLARIZATIONS
                        MEANAC = MEANAC / MCOR
C
                        DO 110 I = 1,MCOR
C                                       moved the following card lower
C                           INDEX2 = (I-1) * 3
C                                       cycle by time inside of time
C                                       interval
                           LTIME = 0
C                                       mean value of re/im through the
C                                       time
                           MEANRE = 0
                           MEANIM = 0
                           DO 107 INTIME = 1, NTIME
                              INDEXV = KBUFF + NRPRMO + (I-1) * 3
     *                           + (INTIME-1)*MCOR*3
C                                       sum RE and IM through time
                              IF (VBUFF(INDEXV+3).GT.0.0 .AND.
     *                           (VBUFF(INDEXV+1)**2 +
     *                            VBUFF(INDEXV+2)**2).GT.0.0
     *                           .AND. OPCODE .EQ. 'CIRC') THEN
                                 LTIME = LTIME + 1
                                 MINT(LTIME) = INTIME
                                 FUNCX(LTIME) = VBUFF(INDEXV+1)
                                 FUNCY(LTIME) = VBUFF(INDEXV+2)
                                 END IF
                              IF (OPCODE .EQ. 'CEXP') THEN
                                 FUNCX(INTIME) = VBUFF(INDEXV+1)
                                 FUNCY(INTIME) = VBUFF(INDEXV+2)
                                 WEIGHT(INTIME) = VBUFF(INDEXV+3)
C                                       sum RE and IM through time
                                 MEANRE = MEANRE + VBUFF(INDEXV+1)
                                 MEANIM = MEANIM + VBUFF(INDEXV+2)
                                 END IF
  107                         CONTINUE
                           IF (OPCODE .EQ. 'CIRC') NTIME = LTIME
                           IF (NTIME .NE. 0) THEN
C                                       zero the flagged channels
                              DO 120 IFLAG = 1, KFLAG
                                 IF (I .EQ. NFLAG(IFLAG)) THEN
                                    X0 = 0
                                    Y0 = 0
                                    GO TO 130
                                    END IF
  120                            CONTINUE

C                                       algorithm of fitting a circle
                              IF (OPCODE .EQ. 'CIRC') THEN
C                                       fit the shifted circle to find
C                                       its center X0, Y0
                                 CALL CIRC (FUNCX, FUNCY, NTIME, NITER,
     *                              MINT, FITRIN, X0, Y0, RADI,
     *                              DELTA, IRET)
                                 IF (IRET .NE. 0) THEN
                                    X0 = 0
                                    Y0 = 0
                                    IRET = 0
                                    END IF
                                 END IF
C                                       algorithm of subtructing
C                                       the complex exponents
                              IF (OPCODE .EQ. 'CEXP') THEN
C                                       mean amplitude through the time
C????the following card is not required
                                 MEANAT = SQRT(MEANRE**2 + MEANIM**2)
     *                              /NTIME
C                                       fource subraction if the channel
C                                       amplitude much bigger the mean
C                                       value
                                 SMALL = MEANAT .LE. FLMAX
                                 IF (SMALL) THEN
                                    NOSUBI = NOSUB
                                 ELSE
                                    NOSUBI = 0
                                    END IF
C
                                 HTIME = NTIME/2
C                                       Fource NTIME to be odd
                                 IF (NTIME .EQ. 2*HTIME) HTIME = HTIME-1
C
C
                                 CALL CMPEXP (FUNCX, FUNCY, WEIGHT,
     *                              HTIME, DTIME, HFREQ, DFREQ, NCOMP,
     *                              NATT, FLUX, GAIN, NOSUBI, NOSUB2,
     *                              SMALL, X0, Y0, IRET)
                                 FIARR(I) = ATAN2(Y0,X0)
                                 END IF
C                                       go here if the channel (I) is
C                                       flagged because the 3 point peak
  130                         CONTINUE
C-----------------start sum  solution amplitudes
                          IF (SMOTH(1) .GT.0.5) THEN
C                              NINTER = 50
                              NINTER = SMOTH(2)
                              ILEFT(I) = MAX(1, I-NINTER/2)
                              IRIGHT(I) = MIN(MCOR, I+NINTER/2 - 1)

                              AMP = SQRT(X0*X0 +Y0*Y0)

                              IF (I .EQ. 1) THEN
                                 AMPSUM(I) = AMP
                                 IF (AMP .NE. 0) THEN
                                    ISOL(I) = 1
                                 ELSE
                                    ISOL(I) = 0
                                    END IF
                              ELSE
                                 AMPSUM(I) = AMPSUM(I-1) + AMP
                                 IF (AMP .NE. 0) THEN
                                    ISOL(I) = ISOL(I-1) + 1
                                 ELSE
                                    ISOL(I) = ISOL(I-1)
                                    END IF
                                 END IF
                           ELSE
C                                       No smoth; so store X0, Y0
                              X0ST(I) = X0
                              Y0ST(I) = Y0
                              END IF

C                                       END OF NTIME .NE. 0
                           END IF
  110                         CONTINUE
C                                       arrays of accumulated amplitudes,
C                                       LEFT, RIGHT edge of intervals
C                                       and numbers of non zero
C                                       solutions are created
            DO 111 I = 1,MCOR
C                                       find the average amplitude for
C                                       the interval near the point I
               IF (SMOTH(1) .GT.0.5) THEN
                  AMPSAV = (AMPSUM(IRIGHT(I)) - AMPSUM(ILEFT(I))) /
     *            (ISOL(IRIGHT(I)) - ISOL(ILEFT(I)))
                  X0 = AMPSAV*COS(FIARR(I))
                  Y0 = AMPSAV*SIN(FIARR(I))
               ELSE
C                                       No smoth, so restore X0, Y0
                  X0 = X0ST(I)
                  Y0 = Y0ST(I)
                  END IF
C-----------------stop average solution amplitudes
                           IF (NTIME .NE. 0) THEN
                              IF (ISCOMP) THEN
C                                       Compressed
                                 INDEX2 = (I-1) * 3
                                 RESULT(INDEX2+1) = X0
                                 RESULT(INDEX2+2) = Y0
                                 RESULT(INDEX2+3) = 1
                                 IF (IRET .LT. 0) RESULT(INDEX2+3) = -1
                              ELSE
C                                       uncompressed
                                 INDEX2 = IPTRO + NRPRMO + (I-1)*3 - 1
                                 BUFF2(INDEX2+1) = X0
                                 BUFF2(INDEX2+2) = Y0
                                 BUFF2(INDEX2+3) = 1
                                 IF (IRET .LT. 0) BUFF2(INDEX2+3) = -1
                                 END IF
C??????????????
                           ELSE
C                                       compressed
                              IF (ISCOMP) THEN
                                 INDEX2 = (I-1) * 3
                                 RESULT(INDEX2+1) = 0
                                 RESULT(INDEX2+2) = 0
                                 RESULT(INDEX2+3) = -1
                              ELSE
C                                       uncompressed
                                 INDEX2 = IPTRO + NRPRMO + (I-1)*3 - 1
                                 BUFF2(INDEX2+1) = 0
                                 BUFF2(INDEX2+2) = 0
                                 BUFF2(INDEX2+3) = -1
                                 END IF
C???????????????????
                              END IF
  111                      CONTINUE
C                                       Pack/copy to output buffer
                     IF (ISCOMP) THEN
C                                       Compressed
C?????????????????I doubt the following card is necessary. But!!!
C                        INDEX2 = IPTRO + NRPRMO + (I-1)*3 - 1
                        CALL ZUVPAK (NCOPY, RESULT, BUFF2(IPTRO+ILOCWT),
     *                     BUFF2(IPTRO+NRPRMO))
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
C                                       Tell history about averaging
C                                       time
               YINC = YINC * 86400.0
               NUMHIS = NUMHIS + 1
               WRITE (HISCRD(NUMHIS),1135) YINC
C                                        Tell history about reduced
C                                        output.
               IF (XINC.GT.1.0) THEN
                  NUMHIS = NUMHIS + 1
                  WRITE (HISCRD(NUMHIS),1145) XINC
                  END IF
C                                        Write no. written to history.
               NUMHIS = NUMHIS + 1
               WRITE (HISCRD(NUMHIS),1150) XCOUNT
               WRITE (MSGTXT,1150) XCOUNT
               CALL MSGWRT (5)
               GO TO 999
               END IF
            END IF
C                                        Set up for next integration
CRCW                                     Removed 0.95 here
C         IF (LTB) THEN
C            TLAST = CT + YINC
C         ELSE
C            IDAY = CT
C            X8 = (CT-IDAY) / YINC
C            TLAST = IDAY + DINT (X8) * YINC + YINC
C            END IF
CLK                                      Comment the previous lines
C                                        and put the following one
         TLAST = CT + YINC
         TCHK = TLAST - 1.1 * YINC
C                                        Reset accumulators, etc.
         I2TMP = LBAS * (1+NRPRMO+NMCOR)
         IF (I2TMP.GT.ACSIZE) I2TMP = ACSIZE
         CALL RFILL (I2TMP, 0.0, VBUFF)
         BLCUR = IBAS
         LSTSOU = CURSOU
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
C                                       store KBUFF for the following
C                                       reduction of the stored time
C                                       sets
      NBUFF = KBUFF
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
C                                       time counter inside of the
C                                       averaging interval
      KTIME = KTIME + 1
C                                       MCOR is number of polarization
      DO 220 I = 1,MCOR
C                                       index to store sequent times
         INDEX = KBUFF + NRPRMO + (I-1)*3 + (KTIME-1)*MCOR*3
C                                        Check weight.
         IF (VIS(3,I).GT.0.0) THEN
C                                       Store  vis data.
C                                       no multiply by weight
C            VBUFF(INDEX+1) = VIS(1,I)*VIS(3,I)
C            VBUFF(INDEX+2) = VIS(2,I)*VIS(3,I)
            VBUFF(INDEX+1) = VIS(1,I)
            VBUFF(INDEX+2) = VIS(2,I)
            VBUFF(INDEX+3) = VIS(3,I)
C                                         Use largest weight for uv etc
            WGT = MAX (WGT, VIS(3,I))
            END IF
 220     CONTINUE
C                                         Accumulate PARMS only if some
C                                         data was good.
      IF (WGT.GT.0.0) THEN
C
         DO 240 IP = 1,NRPRMO
            IF (ANOTA(IP).EQ.2) THEN
               VBUFF(KBUFF+IP) = VBUFF(KBUFF+IP) + RPARM(IP)*WGT
            ELSE IF (ANOTA(IP).EQ.1) THEN
               VBUFF(KBUFF+IP) = VBUFF(KBUFF+IP) + RPARM(IP)
            ELSE
               IF (VBUFF(KBUFF+IP).EQ.0.0) VBUFF(KBUFF+IP) =
     *            RPARM(IP)
               END IF
  240       CONTINUE
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
C                                       close output map
C      CALL MAPCLS ('INIT', KVOL, IDSLOT, IDLUN,
C     *      IDIND, CATBLK, SAVEIM, WBI256, JERR)
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
 1145 FORMAT ('UVAVDO: only one in ',F6.0,' output records actually',
     *        ' written.')
 1150 FORMAT ('UVAVDO: ',I10,' Visibility records written')
 1160 FORMAT ('UVAVDO: Parameter MXINDX too small (',I5,' )')
 1180 FORMAT ('UVAVDO: Parameter ACSIZE too small (',I8,' )')
      END
      SUBROUTINE CIRC (FUNCX, FUNCY, NFI, NITER, MINT, FITRIN,
     *   X0, Y0, RADI, DELTA, IRET)
C-----------------------------------------------------------------------
C     Routine to fit a circle of radius R its center at X0, Y0
C     and increment of radius DELTA
C     to the data by non linear Least Square method
C     The circle is expected as:
C     (FUNCX-X0)^2 + (FUNCY-Y0)^2 = [R0+DELTA*(MINP(I)-1)]^2
C   Input:
C      FUNCX   R(*)  Array of X
C      FUNCY   R(*)  Array of Y
C      NFI     I     Number of points at arrays FUNCX, FUNCY, MINT
C      NITER   I     Number of iteration at the NLEASQ
C      MINT    I(*)  Array of sequence data, to calculate the radius
C                    variation at the case of skiping some points
C      FITRIN  L     Fit the circle radius increment?
C   Output:
C      X0      R     Found solution for X0
C      Y0      R     Found solution for Y0
C      RADI    R     Found solution for the initial radius
C      DELTA   R     The radius increment
C      IRET    I     Error; 0 => OK
C-----------------------------------------------------------------------
      INTEGER NFI, NITER, IRET, NFIT
      REAL    FUNCX(*), FUNCY(*)
      INTEGER MINT(*)
      REAL    X0, Y0, RADI, DELTA
      REAL    VARRES, FITPAR(20)
      INTEGER IFIT, IFI, MIFI, KFIT, IKFIT, ITER
      REAL      R(20), MATR(400), COEFF(20), NOBS, SUM, SSQ, RIGHT,
     *   VX(20), SSQRES, VARY, FIT, FUNCX2, FUNCY2
      LOGICAL   FITRIN
C      INCLUDE 'INCS:DMSG.INC'
C      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       initial value for X0, Y0
      X0 = 0
      Y0 = 0
      DELTA = 0
      ITER = 1
C                                       cycle by iterations
C
   50 CONTINUE
C                                       number of fit parameters:
      NFIT = 3
      IF (ITER .GT. 1 .AND. FITRIN) NFIT = 4
C
      IRET = 0
C                                       Force result vector R(NFIT),
C                                       matrix M(NFIT*NFIT) to zero
      DO 20 IFIT = 1, NFIT
         R(IFIT) = 0.0
         DO 10 KFIT = 1, NFIT
            IKFIT = IFIT + (KFIT - 1)*NFIT
            MATR (IKFIT) = 0.0
 10         CONTINUE
 20      CONTINUE
C                                       Prepare the coefficients at
C                                       and matrix MATR(NFIT*NFIT)
C                                       for routine LEASQR
      SUM = 0.0
      SSQ = 0.0
      NOBS = 0.0

C                  The linearized equations:
C  (2*FUNCX-X0) * X0 + (2*FUNCY-Y0) * Y0 + 1 * R^2
C  + [2*R*(MINT(I)-1)+DELTA*(MINT(I)-1)^2] * DELTA = FUNCX^2+FUNCY^2
C  MINT(I) describe the skiping points.
C
      DO 80 IFI = 1, NFI
C                                       COEFFs near X0, Y0, R, DELTA
         COEFF(1) = 2*FUNCX(IFI) - X0
         COEFF(2) = 2*FUNCY(IFI) - Y0
         COEFF(3) = 1
         MIFI = MINT(IFI) - 1
C
         IF (NFIT .EQ. 4) THEN
            COEFF(4) = 2*RADI*MIFI + DELTA*MIFI*MIFI
         ELSE
            COEFF(4) = 0
            END IF
         FUNCX2 = FUNCX(IFI)*FUNCX(IFI)
         FUNCY2 = FUNCY(IFI)*FUNCY(IFI)
         RIGHT = FUNCX2 + FUNCY2
C
         SUM = SUM + RIGHT
         SSQ = SSQ + RIGHT*RIGHT
         NOBS = NOBS + 1
C
         DO 60 IFIT = 1, NFIT
            R(IFIT) = R(IFIT) + RIGHT * COEFF(IFIT)
C                                       calculate upper/right
C                                       triangle of MATR
            DO 40 KFIT = IFIT, NFIT
               IKFIT = IFIT + (KFIT-1)*NFIT
               MATR(IKFIT) = MATR(IKFIT) +
     *            COEFF(IFIT) * COEFF(KFIT)
   40          CONTINUE
   60       CONTINUE
   80    CONTINUE
C
      CALL LEASQR (NFIT, NOBS, SUM, SSQ, R, MATR, FITPAR, VX, SSQRES,
     *   VARRES, VARY, FIT, IRET)
      VARRES = SQRT(VARRES)
C                                       solutions
      X0 = FITPAR(1)
      Y0 = FITPAR(2)
      IF (FITPAR(3) .GT. 0) THEN
         RADI = SQRT(FITPAR(3))
      ELSE
         X0 = 0
         Y0 = 0
         GO TO 999
         END IF
C
      DELTA = 0
      IF (NFIT .EQ. 4) DELTA = FITPAR(4)
      ITER = ITER + 1
C                                       back to the next iteration?
      IF (ITER .LE. NITER) GO TO 50
C
  999 RETURN
      END
      SUBROUTINE CMPEXP (FUNRE, FUNIM, WEIGHT, HTIME, DTIME, HFREQ,
     *   DFREQ, NITER, NATT, FLUX, GAIN, NOSUB, NOSUB2, SMALL, X0, Y0,
     *   IRET)
C-----------------------------------------------------------------------
C     Routine to subtract the complex number (X0,Y0) from the sum of
C     three (including the (X0,Y0)) complex exponents, two of which
C     are variable at time.
C     The Fourier trasform of the given sum (FUNRE, FUNIM) (Dirty MAP-DM)
C     is evaluated.
C     Then the position of the maximum of amplitude is determined and
C     the complex number at this maximum is multiplied by the
C     DB (Dirty Beam) and subtracted from the DM
C   Input:
C      FUNRE     R(*)  Array of real part of the time sequence
C      FUNIM     R(*)  Array of imaginary part of the time sequence
C      WEIGHT    R(*)  Array of weights
C      2*HTIME+1 I     Number of points at arrays FUNRE, FUNIM, TIME
C      DTIME     R     Step in time
C      2*HFREQ+1 I     Number of points at the frequency axis
C      DFREQ     R     Step in frequency
C      NITER     I     Number of iterations: number of subtracted
C                      complex exponents
C      FLUX      R     Minimum flux to stop iterations
C      GAIN      R     Gain at the clean cycle
C      NOSUB     I     half width of spectrum near zero, where
C                      no subtraction
C      NOSUB2    I     half width of spectrum near the maximum found
C                      at the first iteration, where  no subtraction
C                      (for other than the first iteration)
C   Output:
C      X0        R     Found solution for X0
C      Y0        R     Found solution for Y0
C      IRET      I     Error; 0 => OK
C-----------------------------------------------------------------------
      INTEGER   INDEX
      INTEGER HTIME, HFREQ, NITER, NOSUB, NOSUB2, LEFT, RIGHT,
     *   LEFT2, RIGHT2, KITER, NATT, IRET
      LOGICAL SMALL, CLEAN
      REAL    FUNRE(*), FUNIM(*), WEIGHT(*), SUMW, DFREQ, DTIME,
     *        FLUX, GAIN
      REAL    X0, Y0, RE0, IM0
      INTEGER HFREQR, FFREQR, IFREQ, KFREQ, ITIME, FTIME, FFREQ
      INTEGER KTIME, KMAXAM, KOFMAX, ITER, IFR1IT
      REAL    FREQ, TIME, ARG
      INTEGER SIZOUT, SIZEDB
      PARAMETER (SIZOUT = 2001)
      PARAMETER (SIZEDB = 2*SIZOUT)
      REAL    FOUTRE(SIZOUT),  FOUTIM(SIZOUT), DBRE(SIZEDB),
     *   DBIM(SIZEDB)
      REAL AMPMAX, AMP, REFAMP, IMFAMP, REFMUL, IMFMUL
Cfor simulation
C      REAL REC1, REC2, IMC1, IMC2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       initiate x0, y0
      X0 = 0
      Y0 = 0
C                                       initialize the attemp number
      KITER = 1
C                                       Evaluate the Dirty Beam with
C                                       double size to cover all shifts
C-------------------------start of Dirty Beam----------------------
      FTIME = 2*HTIME + 1
      FFREQ = 2*HFREQ + 1
      HFREQR = 2*HFREQ
      FFREQR = 2*HFREQR + 1
Cstart simulation
C                                       simulate the input data as sum
C                                       of 2 complex components
C                                       1. at zero frequency
C                                       2. at the given frequency 0.1Hz
C The component at zero frequency
C      REC1 = 0.4
C      IMC1 = 0.84
C The component at frequency ~0.1Hz
C      REC2 = -0.5
C      IMC2 = -0.81
C      DO 5 ITIME = 1, FTIME
C         ARG = TWOPI *(ITIME-HTIME-1) * 0.1 * 0.102
C                                       dt     f
C         FUNRE(ITIME) = REC1 + REC2*COS(ARG) - IMC2*SIN(ARG)
C         FUNIM(ITIME) = IMC1 + REC2*SIN(ARG) + IMC2*COS(ARG)
C
C    5    CONTINUE
Cstop simulation

C                                       calculate sum of weights
      SUMW = 0
      DO 10 ITIME = 1, FTIME
         SUMW = SUMW + WEIGHT(ITIME)
   10    CONTINUE
C
      DO 40 IFREQ = 1, FFREQR
         DBRE(IFREQ) = 0
         DBIM(IFREQ) = 0
         KFREQ = IFREQ - HFREQR - 1
         FREQ  = KFREQ * DFREQ
         DO 20 ITIME = 1, FTIME
            KTIME = ITIME - HTIME -1
            TIME = KTIME * DTIME
            ARG = -TWOPI * FREQ * TIME
            DBRE(IFREQ) = DBRE(IFREQ) + WEIGHT(ITIME)*COS(ARG)
            DBIM(IFREQ) = DBIM(IFREQ) + WEIGHT(ITIME)*SIN(ARG)
   20       CONTINUE
C?????????????What if SUMW=0
        DBRE(IFREQ) = DBRE(IFREQ)/SUMW
        DBIM(IFREQ) = DBIM(IFREQ)/SUMW
   40   CONTINUE
C-------------------------end   of Dirty Beam----------------------
      ITER = 1
C                                       Fourier transform (Dirty Map)
      DO 80 IFREQ = 1, FFREQ
         FOUTRE(IFREQ) = 0
         FOUTIM(IFREQ) = 0
         KFREQ = IFREQ - HFREQ -1
         FREQ  = KFREQ * DFREQ
C
         DO 60 ITIME = 1, FTIME
            KTIME = ITIME - HTIME -1
            TIME = KTIME * DTIME
            ARG = -TWOPI * FREQ * TIME
            FOUTRE(IFREQ) = FOUTRE(IFREQ) + WEIGHT(ITIME)*
     *         (FUNRE(ITIME)*COS(ARG) - FUNIM(ITIME)*SIN(ARG))
            FOUTIM(IFREQ) = FOUTIM(IFREQ) + WEIGHT(ITIME)*
     *         (FUNRE(ITIME)*SIN(ARG) + FUNIM(ITIME)*COS(ARG))
   60       CONTINUE
         FOUTRE(IFREQ) = FOUTRE(IFREQ)/SUMW
         FOUTIM(IFREQ) = FOUTIM(IFREQ)/SUMW
C                                       find the vector average of
C                                       the input data
         IF (IFREQ .EQ. (HFREQ+1)) THEN
            RE0 = FOUTRE(IFREQ)
            IM0 = FOUTIM(IFREQ)
            END IF
   80    CONTINUE
C                                       INDEX determines the beginning
C                                       of the attempt(group)
      INDEX = 1
C                                       cicle by iteration

  100 CONTINUE
C                                       Find position of maximum of
C                                       amplitude and value of the
C                                       the spectrum there,
C                                       and mean spectrum value
      AMPMAX = -1.0E+10
C
      IF (INDEX .EQ. 1) THEN
C                                       The first iteration of the
C                                       new group
         INDEX = 2
         DO 120 IFREQ = 1, FFREQ
C
            IF (KITER .EQ. 1) THEN
C                                       CLEAN at the first attempt
               CLEAN = ABS(IFREQ-HFREQ-1) .GE. NOSUB
            ELSE
C                                       CLEAN at the second attempt
               CLEAN =((IFREQ .GT. RIGHT) .OR. (IFREQ .LE. LEFT))
     *         .AND.
     *         ABS(IFREQ-HFREQ-1) .GE. NOSUB
               END IF
C
            AMP = SQRT (FOUTRE(IFREQ)*FOUTRE(IFREQ) +
     *      FOUTIM(IFREQ)*FOUTIM(IFREQ))
C                                       find position and value of
C                                       maximum looking only in the
C                                       vicinity of the maximum
C                                       position found at the first
C                                       iteration IFR1IT
            IF (AMP .GT. AMPMAX .AND. CLEAN)
     *         THEN
C                                       IF (AMP .GT. AMPMAX) THEN
               AMPMAX = AMP
               KMAXAM = IFREQ
               REFAMP = FOUTRE(IFREQ)
               IMFAMP = FOUTIM(IFREQ)
               END IF
  120       CONTINUE
C                                       position of the found muximum
C                 fource KMAXAM=225,,,,,
C               KMAXAM = 225
C               REFAMP = FOUTRE(KMAXAM)
C               IMFAMP = FOUTIM(KMAXAM)
C               AMPMAX = SQRT(REFAMP**2+IMFAMP**2)
C   end fource
         IFR1IT = KMAXAM
         IF (KITER .EQ. 1) THEN
            LEFT = MAX(1, (IFR1IT - NOSUB2))
            RIGHT = MIN(FFREQ, (IFR1IT + NOSUB2))
         ELSE
            LEFT2 = MAX(1, (IFR1IT - NOSUB2))
            RIGHT2 = MIN(FFREQ, (IFR1IT + NOSUB2))
            END IF
C                                       flag if the fringe rate is too
C                                       small and the channel amplitude
C                                       is very big
         IF (((IFR1IT-HFREQ-1)*DFREQ .LT. 0.5/(2*HTIME*DTIME)) .AND.
     *         .NOT. SMALL) THEN
            X0 = 0
            Y0 = 0
            GO TO 999
            END IF
      ELSE
C                                       The other than the first
C                                       iteration of the group
         DO 125 IFREQ = 1, FFREQ
            IF (KITER .EQ. 1) THEN
C                                       CLEAN at the first attempt
               CLEAN=((IFREQ .GT. LEFT) .AND. (IFREQ .LE. RIGHT))
     *            .AND. ABS(IFREQ-HFREQ-1) .GE. NOSUB
            ELSE
C                                       CLEAN at the second attempt
               CLEAN =((IFREQ .GT. RIGHT) .OR. (IFREQ .LE. LEFT))
     *         .AND.
     *         (((IFREQ .GT. LEFT2) .AND. (IFREQ .LE. RIGHT2))
     *         .AND.
     *         ABS(IFREQ-HFREQ-1) .GE. NOSUB)
               END IF
            AMP = SQRT (FOUTRE(IFREQ)*FOUTRE(IFREQ) +
     *      FOUTIM(IFREQ)*FOUTIM(IFREQ))
C                                       find position and value of
C                                       maximum looking only in the
C                                       vicinity of the maximum
C                                       position found at the first
C                                       iteration IFR1IT
            IF (AMP .GT. AMPMAX .AND. CLEAN) THEN
               AMPMAX = AMP
               KMAXAM = IFREQ
               REFAMP = FOUTRE(IFREQ)
               IMFAMP = FOUTIM(IFREQ)
               END IF
  125       CONTINUE
C                 fource KMAXAM=225,,,,,
C               KMAXAM = 225
C               REFAMP = FOUTRE(KMAXAM)
C               IMFAMP = FOUTIM(KMAXAM)
C               AMPMAX = SQRT(REFAMP**2+IMFAMP**2)
C   end fource
         END IF
C                                       end of IF the first/other iters
      IF (ITER .GT. NITER) THEN
         GO TO 999
      ELSE
         IF ((AMPMAX .LT. FLUX)
     *      .OR. (SQRT(X0*X0+Y0*Y0) .GT. 5*AMPMAX)) THEN
C                                       switch back to the beginning
C                                       of the following froup
            INDEX = 1
            KITER = KITER + 1
            IF (KITER .GT. NATT) THEN
               IF (ITER .EQ. 1) THEN
                  X0 = RE0
                  Y0 = IM0
                  GO TO 999
               ELSE
                  GO TO 999
                  END IF
               END IF
            END IF
         END IF
C                                       Subtract the dirty beam
C                                       multiplyed by the value of
C                                       RE/IM at max amplitude from
C                                       RE/IM
      DO 140 IFREQ = 1, FFREQ
C                                       KOFMAX position of RMTF's max
C                                       relatively AMP max
         KOFMAX = IFREQ - KMAXAM + HFREQR + 1
C                                       Multiply the positioned RMTF
C                                       by the spectrum value at the
C                                       amplitude maximum
         REFMUL = REFAMP*DBRE(KOFMAX) - IMFAMP*DBIM(KOFMAX)
         IMFMUL = REFAMP*DBIM(KOFMAX) + IMFAMP*DBRE(KOFMAX)
C                                       Subtract the production of
C                                       the spectrum
         FOUTRE(IFREQ) = FOUTRE(IFREQ) - REFMUL * GAIN
         FOUTIM(IFREQ) = FOUTIM(IFREQ) - IMFMUL * GAIN
C
C                                       the found value at the
C                                       frequency equaled zero
         IF (IFREQ .EQ. (HFREQ+1)) THEN
            X0 = FOUTRE(IFREQ)
            Y0 = FOUTIM(IFREQ)
            END IF
  140    CONTINUE
C
      ITER = ITER + 1
      GO TO 100
C
  999 RETURN
      END






