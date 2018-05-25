LOCAL INCLUDE 'DECOR.INC'
C                                       Local include for DECOR
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXCALC, XXSTOK ,
     *   XNAMOU(3), XCLAOU(2)
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XBIF, XEIF, XBCHAN, XECHAN, XSUBA, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), XSOUT, XDISO,
     *   BUFF1(UVBFSS), BUFF2(UVBFSS)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, BSTOK, ESTOK,
     *   NUMHIS, JBUFSZ, ILOCWT, CATOLD(256), CATNEW(256), INCSI, INCFI,
     *   INCIFI, INCSO, INCFO, INCIFO, LRECI, LRECO, NRPRMI, NRPRMO,
     *   OLOCU, OLOCV, OLOCW, OLOCT, OLOCB, OLOCSU, OLOCFQ, OLOCA1,
     *   OLOCA2, OLOCSA
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, XCALCO*4, XSTOK*4,
     *   NAMOUT*12, CLAOUT*6, HISCRD(10)*64
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, XQUAL,
     *   XXCALC, XTIME, XXSTOK, XBAND, XFREQ, XFQID, XBIF, XEIF, XBCHAN,
     *   XECHAN, XSUBA, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG,
     *   XDOBND, XBPVER, XSMOTH, XNAMOU, XCLAOU, XSOUT, XDISO
      COMMON /DECORP/ SEQIN, SEQOUT, DISKIN, DISKO, BSTOK, ESTOK,
     *   NUMHIS, ILOCWT, CATOLD, CATNEW, INCSI, INCFI, INCIFI, INCSO,
     *   INCFO, INCIFO, LRECI, LRECO, NRPRMI, NRPRMO, OLOCU, OLOCV,
     *   OLOCW, OLOCT, OLOCB, OLOCSU, OLOCFQ, OLOCA1, OLOCA2, OLOCSA,
     *   ISCOMP
      COMMON /CHARPM/ NAMEIN, CLAIN, XSOUR, XCALCO, XSTOK, NAMOUT,
     *   CLAOUT, HISCRD
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
LOCAL END
      PROGRAM DECOR
C-----------------------------------------------------------------------
C! Measures the decorrelation between channels and IF of uv data
C# Task UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2006, 2009-2010, 2015
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
C   Determines decorrelation between channels and IFs
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input UV data.
C      STOKES         BSTOK,ESTOK   Stokes' type(s) to test.
C      BIF            BIF           First IF to test
C      EIF            EIF           Highest IF to test
C      BCHAN          BCHAN         First Channel to test
C      ECHAN          ECHAN         Highest Channel to test
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'DECOR.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA PRGM /'DECOR '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL DECOIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Process.
      CALL DECOUV (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL DECOHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE DECOIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   DECOIN gets input parameters for DECOR and creates an output file.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      BSTOK   I  First Stokes' parameter to use (number in data base)
C      ESTOK   I  Highest Stokes' parameter to use.
C      BIF     I  First IF to test
C      EIF     I  Highest IF to test
C      BCHAN   I  First channel to test
C      ECHAN   I  Highest channel to test
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C      ISCOMP  L  If true data is compressed
C      OLOCU   I  Output U pointer
C      OLOCV   I  Output V pointer
C      OLOCW   I  Output W pointer
C      OLOCT   I  Output Time pointer
C      OLOCB   I  Output Baseline pointer
C      OLOCSU  I  Output Source ID pointer
C      OLOCFQ  I  Output FQ id pointer
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in DECOR for more details.
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER  STAT*4, BLANK*6, PTYPE*2
      INTEGER   OLDCNO, IROUND, NPARM, IERR, I, LUN, INCX, INDEX
      REAL      CATR(256), RPARM(20)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128), DTEMP
      LOGICAL   T, ISIQUV, MATCH
      INCLUDE 'DECOR.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA BLANK  /'      '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 164
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
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      CALL H2CHR (4, 1, XXCALC, XCALCO)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 20      CONTINUE
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
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
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Determine stokes' parameter
C                                       requested:
      BSTOK = 0
      ESTOK = 0
      ISIQUV = CATD(KDCRV+JLOCS) .GT. 0.0D0
C                                       Data RR,LL,RL,LR:
      IF ((.NOT.ISIQUV) .AND. (BSTOK.LE.0)) THEN
         IF (XSTOK.EQ.'RR  ') BSTOK = (CATD(KDCRV+JLOCS) + 1.0D0)
     *      + CATR(KRCRP+JLOCS)
         IF (XSTOK.EQ.'LL  ') BSTOK = (CATD(KDCRV+JLOCS) + 2.0D0)
     *      + CATR(KRCRP+JLOCS)
         IF (XSTOK.EQ.'RL  ') BSTOK = (CATD(KDCRV+JLOCS) + 3.0D0)
     *      + CATR(KRCRP+JLOCS)
         IF (XSTOK.EQ.'LR  ') BSTOK = (CATD(KDCRV+JLOCS) + 4.0D0)
     *      + CATR(KRCRP+JLOCS)
         ESTOK = BSTOK
         END IF
C                                       Data IQUV
      IF ((.NOT.ISIQUV) .AND. (BSTOK.LE.0)) THEN
         IF (XSTOK.EQ.'Q   ') BSTOK = (CATD(KDCRV+JLOCS) - 2.0D0)
     *      + CATR(KRCRP+JLOCS)
         IF (XSTOK.EQ.'U   ') BSTOK = (CATD(KDCRV+JLOCS) - 3.0D0)
     *      + CATR(KRCRP+JLOCS)
         IF (XSTOK.EQ.'V   ') BSTOK = (CATD(KDCRV+JLOCS) - 4.0D0)
     *      + CATR(KRCRP+JLOCS)
         ESTOK = BSTOK
         END IF
C                                       Stokes I is the default
      IF ((XSTOK.EQ.'I') .OR. (BSTOK.LE.ECHAN)) THEN
         BSTOK = 1
         ESTOK = 2
C                                       Data IQUV?
         IF (ISIQUV) ESTOK = 1
         END IF
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DO 70 I = 1,30
         SOURCS(I) = XSOUR(I)
 70      CONTINUE
      SELQUA = IROUND (XQUAL)
      SELCOD = XCALCO
      CALL RCOPY (8, XTIME, TIMRNG)
      STOKES = '    '
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MAX (1, MIN (ECHAN, CATBLK(KINAX+JLOCF)))
      IF (JLOCIF.GE.0) THEN
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         EIF = IROUND (XEIF)
         IF (BIF.GT.EIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MAX (1, MIN (EIF, CATBLK(KINAX+JLOCIF)))
      ELSE
         BIF = 1
         EIF = 1
         END IF
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
      CALL FILL (50, 0, ANTENS)
      SUBARR = IROUND (XSUBA)
      FGVER = IROUND (XFLAG)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      DOACOR = T
      DOXCOR = T
C                                       Spectral smoothing
      CALL RCOPY (3, XSMOTH, SMOOTH)
C                                       Rate error amplitude correction
C                                       assuming 2 s integration
      DXTIME = 2.0 / 86400.0
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, OLDCNO, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1070)
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       Changes to CATBLK:
C                                       Only one output Stokes'
      DTEMP = CATD(KDCRV+JLOCS) + (BSTOK-1) * CATR(KRCIC+JLOCS)
      IF (XSTOK.EQ.'I   ') DTEMP = 1.0D0
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, BUFF2, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1035) JERR
         GO TO 990
         END IF
      CALL UVGET ('CLOS', RPARM, BUFF2, IERR)
C                                       Changes to CATBLK:
C                                       Only one output Stokes'
      CATBLK(KINAX+JLOCS) = 1
      CATD(KDCRV+JLOCS) = DTEMP
C                                       only one IF, channel
      CATBLK(KINAX+JLOCF) = 1
      IF (JLOCIF.GT.0) CATBLK(KINAX+JLOCIF) = 1
C                                       Always write uncompressed data
C                                       (there is no gain for this task)
      IF (CATBLK(KINAX).LE.1) THEN
C                                       Remove Weight and Scale random
C                                       parms.
C                                       Find weight and scale.
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            JERR = 9
            GO TO 990
            END IF
C                                       Change random par names
            INDEX = KHPTP + 2 * ILOCWT
            CALL CHR2H (8, 'REMOVED ', 1, CATH(INDEX))
            CALL CHR2H (8, 'REMOVED ', 1, CATH(INDEX+2))
C                                       If these are the last two random
C                                       parameters drop them.
            IF (ILOCWT.GE.(CATBLK(KIPCN)-2)) CATBLK(KIPCN) =
     *         CATBLK(KIPCN) - 2
         END IF
C                                       Reset Complex axis length
      CATBLK(KINAX) = 3
C                                       Change units
      CALL CHR2H (8, 'DECORREL', 1, CATH(KHBUN))
C                                       Put new name in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
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
C                                       Save output file info
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      INCX = CATBLK(KINAX)
      LRECO = LREC
      NRPRMO = NRPARM
      INCSO = INCS / INCX
      INCFO = INCF / INCX
      INCIFO = INCIF / INCX
C                                       Output random parameter
C                                       pointers.
      OLOCU = ILOCU
      OLOCV = ILOCV
      OLOCW = ILOCW
      OLOCT = ILOCT
      OLOCB = ILOCB
      OLOCSU = ILOCSU
      OLOCFQ = ILOCFQ
      OLOCA1 = ILOCA1
      OLOCA2 = ILOCA2
      OLOCSA = ILOCSA
C                                       Save output CATBLK
      CALL COPY (256, CATBLK, CATNEW)
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      SEQOUT = CATNEW(KIIMS)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DECOIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('DECOIN: UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('DECOIN: ERROR',I3,' UPDATING NEW CATBLK')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
      END
      SUBROUTINE DECOUV (IRET)
C-----------------------------------------------------------------------
C   DECOUV sends uv data one point at a time to the decorrelation
C   routine and then writes the modified data if requested.
C   Input in common:
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER OFILE*48
      INTEGER   IPTRO, LUNO, INDO, ILENBU, KBIND, NIOUT, NIOLIM, IA1,
     *   IA2, INCX, BO, VO,  NUMVIS, XCOUNT, NCORI, NCORO, NCOPY
      LOGICAL   T, F
      REAL      BASEN, RPARM(50), RESULT(3)
      DOUBLE PRECISION UVSCAL
      INCLUDE 'DECOR.INC'
      INCLUDE 'INCS:PUVD.INC'
      REAL      VIS(3*MAXCIF)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNO /17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open input file
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.GT.1) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      UVSCAL = FREQ / UVFREQ
C                                       Save input file info (as
C                                       returned by UVGET).
      INCX = CATBLK(KINAX)
      LRECI = LREC
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                       Number of visibilities in input
C                                       and output files.
      NCORI = (LRECI - NRPRMI) / CATBLK(KINAX)
      NCORO = (LRECO - NRPRMO) / CATNEW(KINAX)
      NCOPY = LRECO - NRPRMO
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
C                                       Loop
 100  CONTINUE
C                                       Read vis. record.
         CALL UVGET ('READ', RPARM, VIS, IRET)
C                                       Out of data?
         IF (IRET.LT.0) GO TO 200
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
         IF (ILOCB.GE.0) THEN
            BASEN = RPARM(1+ILOCB)
            IA1 = BASEN / 256. + 0.1
            IA2 = BASEN - IA1*256. + 0.1
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            END IF
         NUMVIS = NUMVIS + 1
C                                      Call decorrelation routine.
         CALL DECORR (NUMVIS, VIS, INCX, RESULT, IRET)
C                                       Branch on his return
C                                       Error (fatal)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1120) IRET
            GO TO 990
C                                       Copy to output.
         ELSE IF (IRET.EQ.0) THEN
            XCOUNT = XCOUNT + 1.0D0
C                                       Have to do random parameters 1
C                                       at a a time
C                                       Init random parameters with 0
            CALL RFILL (NRPRMO, 0.0, BUFF2(IPTRO))
            BUFF2(IPTRO+OLOCU) = RPARM(1+ILOCU) * UVSCAL
            BUFF2(IPTRO+OLOCV) = RPARM(1+ILOCV) * UVSCAL
            BUFF2(IPTRO+OLOCW) = RPARM(1+ILOCW) * UVSCAL
            BUFF2(IPTRO+OLOCT) = RPARM(1+ILOCT)
            IF (OLOCB.GE.0) THEN
               BUFF2(IPTRO+OLOCB) = RPARM(1+ILOCB)
            ELSE
               BUFF2(IPTRO+OLOCA1) = RPARM(1+ILOCA1)
               BUFF2(IPTRO+OLOCA2) = RPARM(1+ILOCA2)
               BUFF2(IPTRO+OLOCSA) = RPARM(1+ILOCSA)
               END IF
            IF (OLOCSU.GE.0) BUFF2(IPTRO+OLOCSU) = CURSOU
            IF (OLOCFQ.GE.0) BUFF2(IPTRO+OLOCFQ) = FRQSEL
            CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
            IPTRO = IPTRO + LRECO
            NIOUT = NIOUT + 1
C                                       Write vis record.
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1150) IRET
                  GO TO 990
                  END IF
               IPTRO = KBIND
               NIOUT = 0
               END IF
            END IF
C                                       Read next vis.
         GO TO 100
C                                       Final call to DECORR.
 200  NUMVIS = -1
      CALL DECORR (NUMVIS, BUFF1, INCX, RESULT, IRET)
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
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATNEW, IRET)
C                                       Close files
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DECOUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('DECOUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('DECOUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1100 FORMAT ('DECOUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('DECOUV: DECORR ERROR',I3)
 1150 FORMAT ('DECOUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE DECOHI
C-----------------------------------------------------------------------
C   DECOHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(18)*2, HILINE*72, LABEL*8
      INTEGER   LUN1, LUN2, IERR, I, NONOT, I1, I2
      LOGICAL   T
      INCLUDE 'DECOR.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
C                                       all IF-dependent tables
      DATA NONOT, NOTTYP /18, 'NX','CH','SN', 'FG', 'BP', 'IM', 'CQ',
     *   'PC', 'TY', 'GC', 'MC', 'WX', 'BL', 'AT', 'CS', 'GA', 'OF',
     *   'FQ'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(NCFILE),
     *   FCNO(NCFILE-1), CATNEW, BUFF1, BUFF2, IERR)
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
C                                       TIMERANG
      CALL HITIME (TSTART, TEND, LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Stokes'
      WRITE (HILINE,2010) TSKNAM, XSTOK
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       IF range
      WRITE (HILINE,2001) TSKNAM, BIF, EIF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Chan range
      WRITE (HILINE,2002) TSKNAM, BCHAN, ECHAN
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Subarray
      WRITE (HILINE,2003) TSKNAM, SUBARR
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Flagging
      IF (FGVER.GT.0) THEN
         WRITE (HILINE,2004) TSKNAM, FGVER
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                        Spectral smoothing
      IF (SMOOTH(1).GT.0.5) THEN
         I1 = SMOOTH(1) + 0.5
         I2 = SMOOTH(3) + 0.5
         WRITE (HILINE,2013) TSKNAM, I1, SMOOTH(2), I2
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Calibration
C                                       Table
      IF (DOCAL) THEN
         WRITE (HILINE,2005) TSKNAM, CLUSE
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Polzn correction
      IF (XDOPOL.GT.0.0) THEN
         WRITE (HILINE,2016) TSKNAM, DOPOL
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       BL table
      IF (XBLVER.GE.0.0) THEN
         WRITE (HILINE,2017) TSKNAM, BLVER
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       BP table
      IF (XDOBND.GT.0.0) THEN
         WRITE (HILINE,2018) TSKNAM, DOBAND
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         WRITE (HILINE,2019) TSKNAM, BPVER
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                      Add any other history.
      IF (NUMHIS.LE.0) GO TO 200
         WRITE (LABEL,1010) TSKNAM
         DO 50 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
 50         CONTINUE
C                                       Close HI file
 200   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO,
     *   FCNO(2), FCNO(1), CATNEW, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATNEW, 'REST',
     *   BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DECOHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 1200 FORMAT ('DECOHI: ERROR COPYING TABLES')
 2001 FORMAT (A6,' BIF =',I4,', EIF =',I4,'/ IF range')
 2002 FORMAT (A6,' BCHAN =',I4,', ECHAN =',I4,'/ Chan range')
 2003 FORMAT (A6,' SUBARRAY =',I4)
 2004 FORMAT (A6,' / Edited using FG table version',I3)
 2005 FORMAT (A6,' GAINUSE =',I3,' / CL table')
 2010 FORMAT (A6,' STOKES = ''',A4,''' / Stokes type')
 2013 FORMAT (A6,' SMOOTH = ',I1,',',F6.1,',',I4,
     *   ' / Spectral smoothing parms')
 2016 FORMAT (A6,' DOPOL=',I2,', polarization correction made')
 2017 FORMAT (A6,' BL table ',I3,' applied to data')
 2018 FORMAT (A6,' BP correction done, DOBAND = ',I2)
 2019 FORMAT (A6,' BP correction used BP table ',I2)
      END
      SUBROUTINE DECORR (NUMVIS, VIS, INCX, RESULT, IRET)
C-----------------------------------------------------------------------
C   Determine decorrelation of selected data.
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      U       R    U in wavelengths
C      V       R    V in wavelengths
C      W       R    W in wavelengths
C      T       R    Time in days since 0 IAT on the reference day.
C      IA1     I    First antenna number
C      IA2     I    Second antenna number
C      RPARM   R(*) Random parameter array which includes U,V,W etc
C                   but also any other random parameters.
C      VIS     R(INCX,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C                   NOTE: INCX may be any value .GE. 2
C   Inputs from COMMON:
C      NRPARM     I       # random parameters.
C      NCOR       I       # correlators
C      CATNEW     I(256)  Catalog header record. See Going Aips for
C                         details.
C      BSTOK      I  First Stokes' parameter to use (number in data
C                    base)
C      ESTOK      I  Highest Stokes' parameter to use.
C      LRECI      I  Input file record length
C      NRPRMI     I  Input number of random parameters.
C      INCSI      I  Input Stokes' increment in vis.
C      INCFI      I  Input frequency increment in vis.
C      INCIFI     I  Input IF increment in vis.
C      LRECO      I  Output file record length
C      NRPRMO     I  Output number of random parameters.
C      INCSO      I  Output Stokes' increment in vis.
C      INCFO      I  Output frequency increment in vis.
C      INCIFO     I  Output IF increment in vis.
C   Output:
C      U          R    U in wavelengths
C      V          R    V in wavelengths
C      W          R    W in wavelengths
C      T          R    Time in same units as input.
C      RPARM      R    Modified random parameter array.
C      RESULT  R(INCX,*) Output visibilities selected in frequency.
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C
C   Output in COMMON:
C      NUMHIS    I         # history entries (max. 10)
C      HISCRD    C(NUMHIS) History records
C      CATNEW    I         Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, INCX, IRET
      REAL      VIS(INCX,*), RESULT(INCX,*)
C
      INTEGER  LOOPIF, LOOPF, LOOPS, INDEX, COUNT, NUMCHN, NUMBIF
      REAL     AMP, SUMAMP, SUMREV, SUMIMV, XDECOR, AMPV, WT
      INCLUDE 'DECOR.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      SAVE NUMCHN, NUMBIF
C-----------------------------------------------------------------------
      IRET = 0
      IF (NUMVIS.LE.0) GO TO 999
C                                       Setup on first call
      IF (NUMVIS.EQ.1) THEN
         NUMCHN = CATBLK(KINAX+JLOCS)
         NUMBIF = 1
         IF (JLOCIF.GT.0) NUMBIF = CATBLK(KINAX+JLOCIF)
         END IF
C                                       Do scalar and vector amplitude
C                                       averages.
      COUNT = 0
      SUMAMP = 0.0
      SUMREV = 0.0
      SUMIMV = 0.0
      DO 300 LOOPS = BSTOK,ESTOK
         DO 200 LOOPIF = 1,NUMBIF
            DO 100 LOOPF = 1,NUMCHN
               INDEX = 1 + (LOOPF-1) * INCFI + (LOOPIF-1) * INCIFI +
     *            (LOOPS-1) * INCSI
               IF (VIS(3,INDEX).GT.1.0E-25) THEN
                  COUNT = COUNT + 1
C                                       Scalar sum
                  AMP = SQRT (VIS(1,INDEX) * VIS(1,INDEX) +
     *               VIS(2,INDEX) * VIS(2,INDEX))
                  SUMAMP = SUMAMP + AMP
C                                       Vector sum
                  SUMREV = SUMREV + (VIS(1,INDEX))
                  SUMIMV = SUMIMV + (VIS(2,INDEX))
                  END IF
 100           CONTINUE
 200        CONTINUE
 300     CONTINUE
C                                       Return decorrelation ratio
      IF (COUNT.GT.0) THEN
         AMP = SUMAMP / COUNT
         IF (AMP.GT.1.0E-25) THEN
            SUMREV = SUMREV / COUNT
            SUMIMV = SUMIMV / COUNT
            AMPV = SQRT (SUMREV*SUMREV + SUMIMV*SUMIMV)
            XDECOR = AMPV / AMP
            WT = 1.0
         ELSE
            XDECOR = 0.0
            WT = 0.0
            END IF
C                                       No valid data selected.
      ELSE
         XDECOR = 0.0
         WT=0.0
         END IF
      RESULT(1,1) = XDECOR
      RESULT(2,1) = 0.0
      RESULT(3,1) = WT
 999  RETURN
C-----------------------------------------------------------------------
      END
