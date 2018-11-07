LOCAL INCLUDE 'RDCAL.INC'
C                                                          Include RDCAL
C                                       Local include for RDCAL
C                                       NOTE: uses PARAMETER in DGDS.INC
      CHARACTER NAME1*12, NAME2*12, NAME3*12,
     *   CLASS1*6, CLASS2*6, CLASS3*6, MODFIL*48
      INTEGER   NSCANS, LINT(200), ILOCV1, ILOCV2, ILOCV3, ILOCV4,
     *   NINT, GVOL, GCNO, GVER, NGREC, GBUF(768), GCBLK(256), INITED,
     *   CATBLK(256), JBUFSZ, ISCRAT, CNO1, CNO2, CNO3, MODVOL, FRQSEL
      HOLLERITH XNAME1(3), XNAME2(3), XNAME3(3), XCLAS1(2),
     *   XCLAS2(2), XCLAS3(2)
      REAL   SPARE(5), SEQ1, SEQ2, SEQ3, DISK1,
     *   DISK2, DISK3, XNCOMP(MAXFLD), BLMIN, BLMAX, EPS, XREF,
     *   XMINNO, XPR, XMODE, AVAC, XLEN, DOMOD, POINT, FLUX, DELTAX,
     *   DELTAY, WTRED, DOCAT, BADD(10), XVER, XNMAP,
     *   RBUF(4096), TAU, SCANL(200), TFIRST, TLAST, GM
      DOUBLE PRECISION GS, CNTA, S1, S2, SN
      COMMON /PRM/ XNAME1, XCLAS1, SEQ1, DISK1, XNAME2, XCLAS2,
     *   SEQ2, DISK2, XVER, XNMAP, XNAME3, XCLAS3, SEQ3, DISK3,
     *   XNCOMP, BLMIN, BLMAX, EPS, XREF, XMINNO, XPR, XMODE,
     *   AVAC, XLEN, DOMOD, POINT, FLUX, DELTAX, DELTAY, WTRED,
     *   SPARE, DOCAT, BADD, FRQSEL
      COMMON /BUFRS/ RBUF
      COMMON /SCLIST/ SCANL, TAU, NSCANS, LINT, JBUFSZ, ISCRAT, CNO1,
     *   CNO2, CNO3
      COMMON /FILCHR/ MODFIL, NAME1, NAME2, NAME3, CLASS1, CLASS2,
     *   CLASS3
      COMMON /FILINF/ MODVOL, ILOCV1, ILOCV2, ILOCV3, ILOCV4,
     *   NINT
      COMMON /GFIL/ TFIRST, TLAST, GS, CNTA, GM, GVOL, GCNO, GVER,
     *   NGREC, GBUF, GCBLK
      COMMON /GED/ S1, S2, SN, INITED
      COMMON /MAPHDR/CATBLK
C                                                          End RDCAL
LOCAL END
LOCAL INCLUDE 'RDCAL2.INC'
C                                       Local include for GA table
      INTEGER   IFLG(2), BFLG(24), KV, IREFS(2), GREC(256)
      REAL      T1, T2
      COMPLEX   G(28,2)
      EQUIVALENCE (GREC(1), G), (GREC(113), IFLG), (GREC(115), BFLG),
     *   (GREC(139), KV), (GREC(140), T1), (GREC(141), T2),
     *   (GREC(142), IREFS)
LOCAL END
      PROGRAM RDCAL
C-----------------------------------------------------------------------
C! Self-calibrates data given a model of the source.
C# UV Calibration AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   Experimental self-calibration program for the AIPS data reduction
C   system.
C   WARNING: this task contains FREDCODE.
C   Input parameters:
C      AIPS Adverb  Internal Name           Usage
C      INNAME        NAME1        Descriptors of visibility input
C      INCLASS       CLASS1            file.
C      INSEQ         SEQ1
C      INDISK        DISK1
C      IN2NAME       NAME2        Descriptors of model input file
C      IN2CLASS      CLASS2
C      IN2SEQ        SEQ2
C      IN2DISK       DISK2
C      INVER         XVER
C      NMAPS         MFIELD       Number of input files
C      OUTNAME       NAME3        Descriptors of visibility
C      OUTCLASS      CLASS3            output file.
C      OUTSEQ        SEQ3
C      OUTDISK       DISK3
C      NCOMP         XNCOMP(16)   Number of CLEAN components to use
C                                 If neg. use up to ABS(NCOMP) but stop
C                                 at first negative. One for each of up
C                                 to 16 fields.
C      APARM                      Options.
C      BPARM                      Options.
C      DOCAT                      Switch for cat. gain file.
C      BADDISK       IBAD         Disk no. to avoid for scratch file.
C
C-----------------------------------------------------------------------
      CHARACTER GNAM*48
      INTEGER   WBUF(256), BUF1(256), BUF2(256),
     *   IER, IRET, III
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'RDCAL.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (BUF1, RBUF(1)), (BUF2, RBUF(513))
C-----------------------------------------------------------------------
C                                       Initialization:
      IRET = 0
      INITED = 1
      CNTA = 0D0
      GS = 0D0
C                                       Get the user-defined parameters:
      CALL SCLPRM (WBUF, IER)
      IF (IER.EQ.0) GO TO 10
         WRITE (MSGTXT,1010) IER
         CALL MSGWRT (8)
         IRET = 8
 10   IF (RQUICK) CALL RELPOP (IRET, WBUF, IER)
      IF (IRET.NE.0) GO TO 995
      IRET = 8
C                                       SCFIND tries to locate the
C                                       scan boundaries, and it fills in
C                                       the tables in the SCLIST common
C                                       with its deductions.
C                                       It also creates the cataloged
C                                       visibility output file and
C                                       the extension gain file.
C
      CALL SCFIND (IER)
      IF (IER.NE.0) GO TO 995
      CALL ASCTEL (III, IER)
      IF (IER.NE.0) GO TO 995
C                                       Divide data by model
      CALL SCLMOD (IER)
      IF (IER.NE.0) GO TO 995
C                                       Make gain solutions.
      CALL SOLVE (IER)
      IF (IER.NE.0) GO TO 995
C                                       Make corrections to data and
C                                       write history.
      CALL VISCOR (IER)
      IF (IER.NE.0) GO TO 995
      IRET = 0
C
C                                       If DOCAT has been set to false,
C                                       then destroy the gain extension
C                                       file and remove mention of it
C                                       from the catalog block of the
C                                       output file.
      CALL ZPHFIL ('GA', GVOL, GCNO, GVER, GNAM, IER)
      IF (DOCAT.GT.0.) GO TO 995
         CALL ZDESTR (GVOL, GNAM, IER)
         CALL DELEXT ('GA', GVOL, GCNO, 'WRIT', BUF1, BUF2, GVER, IER)
         CALL CATIO ('READ', GVOL, GCNO, BUF1, 'WRIT', BUF2, IER)
C
 995  CALL DIE (IRET, WBUF)
C
 999  STOP
C-----------------------------------------------------------------------
 1010 FORMAT ('TROUBLE IN GTPARM, IER=',I2)
      END
      SUBROUTINE SCLPRM (IBUF, IER)
C-----------------------------------------------------------------------
C   Subroutine to get and validate self-cal parameters
C   Programmer : Stuart Button - University of Toronto   June 1981
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   IER, MODE, IBUF(*), I, NPARM, IROUND
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'RDCAL.INC'
      DATA PRGNAM /'RDCAL '/
C-----------------------------------------------------------------------
      CALL ZDCHIN (.TRUE., IBUF)
      CALL VHDRIN
      NSCR = 0
      NCFILE = 0
      FRQSEL = -1
      NPARM = 70
      CALL GTPARM (PRGNAM, NPARM, RQUICK, XNAME1, GBUF, IER)
      IF (IER.NE.0) GO TO 999
C                                       Convert characters
      CALL H2CHR (12, 1, XNAME1, NAME1)
      CALL H2CHR (6, 1, XCLAS1, CLASS1)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLASS2)
      CALL H2CHR (12, 1, XNAME3, NAME3)
      CALL H2CHR (6, 1, XCLAS3, CLASS3)
      WRITE (MSGTXT,4000)
      CALL MSGWRT (3)
      DO 210 I = 1,10
         IBAD(I) = IROUND (BADD(I))
  210    CONTINUE
      IF (BLMAX.EQ.0.) BLMAX = 1.0E10
      IF ((BLMIN.GE.0.) .AND. (BLMIN.LT.BLMAX)) GO TO 250
         WRITE (MSGTXT,1250) BLMIN,BLMAX
         CALL MSGWRT (8)
         WRITE (MSGTXT,1255)
         CALL MSGWRT (8)
         IER = 1
         GO TO 999
 250  IF (EPS.EQ.0.) EPS = 0.05
      MODE = XMODE - 1
      IF ((XMODE.LE.0.) .OR. (XMODE.GT.3.)) MODE = 1
      XMODE = MODE
      IF ((XREF.LT.0.) .OR. (XREF.GT.28.)) XREF = 0.
      IF ((MODE.EQ.0) .AND. (XMINNO.EQ.0.)) XMINNO = 4.
      IF ((MODE.GT.0) .AND. (XMINNO.EQ.0.)) XMINNO = 3.
      IF (XPR.LT.0.) XPR = 0.
C                                       Always catalog if no model
      IF (POINT.LT.-0.1) DOCAT = 1.0
      IF (POINT.LE.0.) GO TO 999
         IF (FLUX.EQ.0.) FLUX = 1.
C
 999  RETURN
C-----------------------------------------------------------------------
 1250 FORMAT ('SCLPRM: BLMIN=',E10.3,' BLMAX=',E10.3,' KILOLAMBDA')
 1255 FORMAT ('SCLPRM: BASELINE LIMITS DO NOT MAKE SENSE')
 4000 FORMAT ('You are using a non-standard program')
      END
      SUBROUTINE SCFIND (IER)
C-----------------------------------------------------------------------
C  SCFIND tries to locate scan boundaries in the visibility data base
C  by detecting gaps (the data are assumed to be ordered by time).  The
C  scan boundary information so deduced is entered into the arrays in
C  the SCLIST common area.
C  Output: IER on return, is zero unless some problem was encountered.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(1)*2, TYPTMP*2
      HOLLERITH CATH(256)
      LOGICAL   T, F
      INTEGER   VO, BO, VISNO, KAP, IAD, IMAX, IMIN, IC,
     *   NBINS, LOC, NDT, ITEMP, VCOUNT
      INTEGER   C(256), FIND, BIND, BUF1(512), BUF2(512), IER,
     *   BUF3(300), KBUFSZ,  IROUND, IBIN, NONOT,
     *   IDISK, INDG, IP, IRECL, ISEQ, ISEQ1, ITAU, IVER,
     *   K, LENBU, LUN, LUNG, NB1, NIO, I, TBLK(256)
      REAL   CATR(256), LTIME, RTEMP, AA, BB, TIME, TM, TOL,
     *   XCRIT, DT, CRITT
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'RDCAL.INC'
      INCLUDE 'RDCAL2.INC'
      EQUIVALENCE (CATBLK, C, CATR, CATD, CATH)
      EQUIVALENCE (BUF1, RBUF)
      EQUIVALENCE (BUF2, RBUF(1025))
      EQUIVALENCE (BUF3, RBUF(2049))
      DATA T, F /.TRUE.,.FALSE./
      DATA LTIME /-1E30/
      DATA NONOT, NOTTYP /0,'  '/
C-----------------------------------------------------------------------
      IER = 0
      TOL = 10.
      VISNO = 0
C                               Open the visibility data input
C                               file and pluck parameters from
C                               its catalog block.
      ISEQ1 = IROUND (SEQ1)
      IDISK = IROUND (DISK1)
      LUN = 16
      TYPTMP = 'UV'
      CALL MAPOPN ('READ', IDISK, NAME1, CLASS1, ISEQ1, TYPTMP, NLUSER,
     *   LUN, FIND, CNO1, CATBLK, RBUF, IER)
      IF (IER.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IER
         GO TO 990
 10   DISK1 = IDISK
C                                       Mark file in /CFILES/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = IDISK
      FCNO(NCFILE) = CNO1
      FRW(NCFILE) = 0
C                                       Disallow packed uv data
      IF (CATBLK(KINAX).EQ.1) THEN
         IER = 9
         MSGTXT = 'ERROR: I cannot process packed UV data, use SPLIT'
         GO TO 990
         END IF
C                                       Get uv data header info.
      CALL UVPGET (IER)
C                                       Check sort order.
      IF (ISORT(1:1).NE.'T') THEN
         IER = 9
         WRITE (MSGTXT,1010) ISORT
         CALL MSGWRT (8)
         WRITE (MSGTXT,1011)
         GO TO 990
         END IF
 20   FREQ = FREQ * 1D-3
C                                        Get set to read:
      ILOCV1 = NRPARM
      ILOCV2 = ILOCV1 + CATBLK(KINAX)
      ILOCV3 = ILOCV2 + CATBLK(KINAX)
      ILOCV4 = ILOCV3 + CATBLK(KINAX)
      JBUFSZ = 2 * 4096 / 2
      VO = 0
      BO = 1
C                                       Single buffer.
      KBUFSZ = 2 * JBUFSZ
      LENBU = 0
      CALL UVINIT ('READ', LUN, FIND, NVIS, VO, LREC, LENBU, KBUFSZ,
     *   RBUF, BO, BIND, IER)
      IF (IER.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IER
         GO TO 990
C                                       Assign AP
 30   CALL QINIT (0, 0, KAP)
C                                       Data reading loop:
      VCOUNT = -2
 100  CALL UVDISK ('READ', LUN, FIND, RBUF, NIO, BIND, IER)
      IF (IER.EQ.0) GO TO 110
         WRITE (MSGTXT,1030) IER
         GO TO 990
 110  IF (NIO.EQ.0) GO TO 300
      IP = BIND
      DO 200 I = 1, NIO
C                                        Time in seconds, to an additive
C                                        const:
         TIME = RBUF(IP+ILOCT) * 86400.
         VISNO = VISNO + 1
         IF (TIME.GE.LTIME-TOL) GO TO 115
            WRITE (MSGTXT,1040)
            CALL MSGWRT (8)
            WRITE (MSGTXT,1041)
            CALL MSGWRT (8)
            WRITE (MSGTXT,1042) VISNO, TIME, LTIME
            IER = 10
            GO TO 990
 115     IF (TIME.EQ.LTIME) GO TO 120
            DT = TIME-LTIME
            VCOUNT = VCOUNT + 1
            IF (VCOUNT.GT.32700) GO TO 120
               IAD = VCOUNT
               IF (VCOUNT.LT.0) DT = 0.
               IF (VCOUNT.GE.0) CALL QPUT (DT, IAD, 1, 2)
 120     LTIME = TIME
         IP = IP + LREC
 200     CONTINUE
      GO TO 100
C
 300  CALL MAPCLS ('READ', IDISK, CNO1, LUN, FIND, CATBLK, F, RBUF, IER)
      NCFILE = NCFILE - 1
      DISK1 = IDISK
      SEQ1 = ISEQ1
      CALL QWD
      NBINS = 1201
      NDT = IAD + 1
      NINT = NDT + 1
      IF (XPR.LT.1.0) GO TO 310
         WRITE (MSGTXT,1111) NINT
         CALL MSGWRT (8)
 310  IMAX = NDT
      IF (NBINS.GT.NDT) IMAX = NBINS
      IMIN = IMAX + 1
      IC = IMIN + 1
      RTEMP = 0.5
      CALL QPUT (RTEMP, IMIN, 1, 2)
      RTEMP = 1200.5
      CALL QPUT (RTEMP, IMAX, 1, 2)
      CALL QWD
      CALL QVCLR (IC, 1, NBINS)
      CALL QHIST (0, 1, IC, NDT, NBINS, IMAX, IMIN)
      CALL QWR
      CALL QMAXV (IC, 1, IMAX, NBINS)
      CALL QWR
      ITEMP = 15
      CALL QGSP (LOC, ITEMP)
      CALL QWR
      CALL QGET (AA, IMAX, 1, 2)
      CALL QGET (BB, IMIN, 1, 2)
      CALL QWD
      IF (LOC.GT.NBINS) LOC = LOC - IC + 1
      IF (XPR.LT.1.0) GO TO 320
         WRITE (MSGTXT,5533) AA, BB
         CALL MSGWRT (8)
 320  CALL QGET (RBUF, IC, NBINS, 2)
      IF (XPR.LT.1.0) GO TO 330
         WRITE (MSGTXT,3333) LOC
         CALL MSGWRT (8)
 330  TAU = LOC
      IF (LOC.GT.10) TAU = LOC / 10 * 10.
      IF (TAU.LT.10.) TAU = 10.
      ITAU = TAU
      WRITE (MSGTXT,1050) ITAU
      CALL MSGWRT (8)
C                        In case the solution interval length
C                        set by the user is equal to zero, set
C                        it a bit larger than the integration time,
C                        unless the deduced integration time looks
C                        as though it may be in error.  In that case,
C                        use 1 min. solution intervals.
      IF ((XLEN.LE.0.) .AND. (ITAU.GE.60)) XLEN = 1.
      IF ((XLEN.LE.0.) .AND. (ITAU.LT.60)) XLEN = TAU/60. * 0.75
      XCRIT = 5.
      IF (ITAU.GT.250.) XCRIT = 2.
      CRITT = XCRIT * ITAU
      NSCANS = 1
      NB1 = NBINS
      WRITE (MSGTXT,8000)
      IF (XPR.GE.1.0) CALL MSGWRT (8)
      DO 500 IBIN = 1,NB1
         IF (RBUF(IBIN).EQ.0.) GO TO 500
         IF (IBIN.GT.CRITT) NSCANS = NSCANS + RBUF(IBIN) + .5
         IF (NSCANS.GT.200) GO TO 610
         IF (XPR.LT.1.0) GO TO 500
            WRITE (MSGTXT,8001) IBIN, RBUF(IBIN)
            CALL MSGWRT (8)
 500     CONTINUE
      IF (NSCANS.GT.1) WRITE (MSGTXT,1060) NSCANS
      IF (NSCANS.EQ.1) WRITE (MSGTXT,1070)
      CALL MSGWRT (8)
C                                Find the number of the final
C                                integration period of each scan.  Also
C                                get the length, in seconds, of each
C                                scan, excluding the first half of the
C                                first integration period and the last
C                                half of the last integration period.
      K = 0
      SCANL(1) = 0.
      DO 600 I = 1,NSCANS
         IAD = I - 1
         CALL QGET (TM, IAD, 1, 2)
         CALL QWD
         IF (TM.LT.CRITT) GO TO 590
            K = K + 1
            LINT(K) = I
            SCANL(K+1) = 0.
            GO TO 600
 590     SCANL(K+1) = SCANL(K+1) + TM
 600     CONTINUE
      LINT(NSCANS) = NINT
      IF (XPR.LT.1.0) GO TO 610
         WRITE (MSGTXT,4544)
         CALL MSGWRT (8)
         DO 605 I = 1,NSCANS
            WRITE (MSGTXT,4545) I, LINT(I), SCANL(I)
            CALL MSGWRT (8)
 605        CONTINUE
C                                       Give up AP.
 610  CALL QRLSE
C                                       Create visibility output file.
      CALL COPY (256,CATBLK,TBLK)
      IDISK = IROUND (DISK3)
      ISEQ = IROUND (SEQ3)
      CALL MAKOUT (NAME1, CLASS1, ISEQ1, '      ', NAME3, CLASS3, ISEQ)
C                                     Put file info in CATBLK
      CALL CHR2H (12, NAME3, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLASS3, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = ISEQ
C                                       If no model, make minimal file
C      IF (POINT.LT.-0.1) CATBLK(KIGCN) = 1
      CALL UVCREA (IDISK, CNO3, RBUF, IER)
      IF (IER.EQ.0) GO TO 620
         WRITE (MSGTXT,1080) IER
         GO TO 990
 620  LUN = 16
      ISEQ = CATBLK(KIIMS)
C                                       Put info in /CFILES/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = IDISK
      FCNO(NCFILE) = CNO3
      FRW(NCFILE) = 2
      SEQ3 = ISEQ
      DISK3 = IDISK
C                                       Create gain extension file.
C                                       The file will be deleted later
C                                       on if DOCAT is set to false.
      DO 619 I = 1,256
         GCBLK(I) = CATBLK(I)
 619     CONTINUE
      GVER = 0
      LUNG = 17
      GVOL = IDISK
      GCNO = CNO3
      IRECL = 256
      NGREC = NINT
      IF (XLEN.GT.TAU) NGREC = NINT * (TAU/60.) / XLEN
      NGREC = MAX (1, NGREC)
      NGREC = MIN (1440, NGREC)
C                                       This guess is too large so:
      NGREC = 16
      CALL EXTINI ('WRIT', 'GA', GVOL, GCNO, GVER, GCBLK, LUNG, INDG,
     *   IRECL, NGREC, GBUF, IER)
      IF (IER.EQ.0) GO TO 630
         WRITE (MSGTXT,1090) IER
         GO TO 990
 630  CALL EXTIO ('CLOS', LUNG, INDG, 0, GREC, GBUF, IER)
C                                       Copy the antenna file.
      IVER = 0
      IDISK = DISK1
      CALL ALLTAB (NONOT, NOTTYP, 16, 17, IDISK, GVOL,
     *   CNO1, GCNO, GCBLK, BUF1, BUF2, IER)
      IER = 0
      CALL COPY (256,TBLK,CATBLK)
      GO TO 999
C
 990  CALL MSGWRT (8)
      IER = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SCFIND: TROUBLE IN MAPOPN, IER=',I2)
 1010 FORMAT ('SCFIND: YOUR VISIBILITIES ARE MISORDERED, SORT=',A2)
 1011 FORMAT ('SCFIND: RUN UVSRT, SPECIFYING  SORT=''TB''.')
 1020 FORMAT ('SCFIND: TROUBLE IN UVINIT, IER=',I2)
 1030 FORMAT ('SCFIND: TROUBLE IN UVDISK, IER=',I2)
 1040 FORMAT ('SCFIND: YOUR VISIBILITIES ARE MISORDERED, IN SPITE OF')
 1041 FORMAT ('SCFIND: THE FACT THAT THE CATALOG FLAG SAYS TB ORDER.')
 1042 FORMAT ('SCFIND: AT VIS',I8,' TIME, LTIME',2(1PE15.8))
 1050 FORMAT ('SCFIND: Looks as though the integration time is',I5,
     *   ' sec.')
 1060 FORMAT ('SCFIND: There must have been about',I4,' scans.')
 1070 FORMAT ('SCFIND: There must have been only one scan.')
 1080 FORMAT ('SCFIND: UNABLE TO CREATE NEW UV FILE, IER=', I2)
 1090 FORMAT ('SCFIND: TROUBLE CREATING GAIN EXT''N FILE, IER=',
     *   I2)
 1111 FORMAT (' Nint=',I6)
 3333 FORMAT (I6)
 5533 FORMAT ('Max, min ',1PE15.5,E15.5)
 8000 FORMAT ('   Bin',5X,'Count')
 8001 FORMAT (I6,F10.2)
 4544 FORMAT ('Scan #','   Num',9X,'Length')
 4545 FORMAT (2I6,F15.2)
      END
      SUBROUTINE CNV (XBUF, IP, TIME, I, J, U, V, W, WT, VIS)
C-----------------------------------------------------------------------
C   Converts data to the form desired internally, assumes RR,LL,RL,LR
C    Inputs:
C     XBUF(*)      R    The I/O buffer
C     IP           I    Pointer in XBUF to start of vis record
C    Outputs:
C     TIME         R    Time in seconds
C     I            I    First antenna number
C     J            I    Second antenna number
C     U            R    U in wavelengths
C     V            R    V in wavelengths
C     W            R    W in wavelengths
C     WT(*)        R    Weights of the visibilities
C     VIS(*)       C*8  Array of the visibilities.
C-----------------------------------------------------------------------
      INTEGER   IBL, I, J, K, IP
      LOGICAL   FLAG
      REAL      XBUF(*), WT(*), TIME, U, V, W
      COMPLEX   VIS(*)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'RDCAL.INC'
C-----------------------------------------------------------------------
      TIME = XBUF(IP+ILOCT) * 86400.
      IBL = XBUF(IP+ILOCB)
      I = IBL / 256
      J = IBL - 256*I
      FLAG= ((I.LE.0) .OR. (I.GT.28) .OR. (J.LE.0) .OR. (J.GT.28)
     *   .OR. (I.GE.J))
      U = XBUF(IP+ILOCU)
      V = XBUF(IP+ILOCV)
      W = XBUF(IP+ILOCW)
      K = IP + ILOCV1
      VIS(1) = CMPLX (XBUF(K), XBUF(K+1))
      IF (FLAG) XBUF(K+2) = -ABS (XBUF(K+2))
      WT(1) = MAX (0.0, XBUF(K+2))
      IF (NCOR.EQ.1) GO TO 999
         K = IP + ILOCV2
         VIS(2) = CMPLX (XBUF(K), XBUF(K+1))
         IF (FLAG) XBUF(K+2) = -ABS (XBUF(K+2))
         WT(2) = MAX (0.0, XBUF(K+2))
         IF (NCOR.EQ.2) GO TO 999
            K = IP+ILOCV3
            VIS(3) = CMPLX (XBUF(K), XBUF(K+1))
            IF (FLAG) XBUF(K+2) = -ABS (XBUF(K+2))
            WT(3) = MAX (0.0, XBUF(K+2))
            IF (NCOR.EQ.3) GO TO 999
               K = IP + ILOCV4
               VIS(4) = CMPLX (XBUF(K), XBUF(K+1))
               IF (FLAG) XBUF(K+2) = -ABS (XBUF(K+2))
               WT(4) = MAX (0.0, XBUF(K+2))
C
 999  RETURN
      END
      SUBROUTINE SOLVE (IER)
C-----------------------------------------------------------------------
C   SOLVE reads the visibility input file incrementally, at each
C   increment locating all of the data for a solution interval, then
C   computing the gain solutions for that interval and writing a gain
C   record.  All of the solution intervals are dealt with in one call
C   to SOLVE.  IER, on return, is set to zero unless some problem was
C   recognized.  The maximum permissible number of (unaveraged)
C   visibility points is set by the DATA statement for NMAX and by the
C   corresponding dimensioning of the arrays VOBS, VMOD, WORK, I1, I2
C   U, V, W, and WT.
C-----------------------------------------------------------------------
      LOGICAL   T, F, IFFLAG(28,2), BLFLAG(378,2)
      INTEGER   C(256), FIND, BIND, NO(378),
     *   I1(500), I2(500), I, IDISK, IERR, INDG,
     *   IPR, IRECL, IREF, J, K, KB, KBMX, NV, LUN, LUNG, MINNO, MODE,
     *   NREXT, IARG, IER, LENBU, NIO, KBUFSZ, NMAX, KK
      INTEGER   KVIS, KTOT, KTOTP1, VO, BO
      REAL      CATR(128), WTS(4), WT(500,2), BLMN2, BLMX2,
     *   DT, PSP2, TIME, TIME1, TIME2, UU, VV, WW, XX, UUVV(2, 500)
      DOUBLE PRECISION CATD(64)
      COMPLEX   VIS(4), VOBS(500,2), WORK(500)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'RDCAL.INC'
      INCLUDE 'RDCAL2.INC'
      EQUIVALENCE (CATBLK, C, CATR, CATD)
      DATA T, F /.TRUE.,.FALSE./
      DATA NMAX /500/
C-----------------------------------------------------------------------
      IER = 0
      IERR = 0
      BLMN2 = 1E6 * BLMIN**2
      BLMX2 = 1E6 * BLMAX**2
      MODE = XMODE
      IREF = XREF
      MINNO = XMINNO
      IPR = XPR
      IDISK = DISK1
      LUN = 16
      CALL ZOPEN (LUN, FIND, MODVOL, MODFIL, T, F, T, IER)
      IF (IER.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IER
         GO TO 990
 10   KTOT = NVIS
      VO = 0
      BO = 1
      LENBU = 1
      KBUFSZ = JBUFSZ * 2
      CALL UVINIT ('READ', LUN, FIND, NVIS, VO, LREC, LENBU, KBUFSZ,
     *   RBUF, BO, BIND, IER)
      IF (IER.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) IER
         IERR = 1
         GO TO 990
C                                       Open the gain extension file.
 20   LUNG = 17
      IRECL = 256
      IARG = NGREC / 2
      NREXT = MAX (1, IARG)
      CALL EXTINI ('WRIT', 'GA', GVOL, GCNO, GVER, GCBLK, LUNG, INDG,
     *   IRECL, NREXT, GBUF, IER)
      IF (IER.EQ.0) GO TO 30
         WRITE (MSGTXT,1040) IER
         GO TO 990
C                                       Go to work.
C                                       Loop to execute when the data
C                                       are to be averaged over time
C                                       intervals of approximate
C                                       length XLEN:
 30   NV = 0
      NIO = 1
      DT = 60. * XLEN
      KTOTP1 = KTOT + 1
      TIME2 = -1E10
      DO 390 KVIS = 1,KTOTP1
         CALL UVDISK ('READ', LUN, FIND, RBUF, NIO, BIND, IER)
         IF (NIO.EQ.0) GO TO 310
         CALL CNV (RBUF, BIND, TIME, I, J, UU, VV, WW, WTS, VIS)
         IF (KVIS.EQ.1) TFIRST = TIME
         IF ((NV.GT.0) .AND. (TIME.LE.TIME2)) GO TO 320
            IF (NV.EQ.0) GO TO 315
 310        KK = 0
            IF (KBMX.EQ.0) GO TO 311
            DO 309 K = 1,KBMX
               IF (NO(K).EQ.0) GO TO 309
C                                       Compress out missing data
               KK = KK + 1
               I1(KK) = I1(K)
               I2(KK) = I2(K)
               WT(KK,1) = WT(K,1)
               WT(KK,2) = WT(K,2)
C                                       Average U and V in interval
               UUVV(1,KK) = UUVV(1,K)/(WT(K,1)+WT(K,2))
               UUVV(2,KK) = UUVV(2,K)/(WT(K,1)+WT(K,2))
C                                       Average observed values
               IF (WT(K,1).GT.0.) VOBS(KK,1) = VOBS(K,1) / WT(K,1)
               IF (WT(K,2).GT.0.) VOBS(KK,2) = VOBS(K,2) / WT(K,2)
 309           CONTINUE
C                                       Check TELL
 311        CALL ASCTEL (IPR, IER)
            IF (IER.NE.0) GO TO 999
C                                       Do gain solutions
            CALL SOLVE2 (TIME, I1, I2, WT, VOBS, UUVV, KK, IREFS, G,
     *         IFFLAG, MODE, IREF, MINNO, IPR, NMAX, WORK, BLFLAG, IER)
            IF (IER.NE.0) GO TO 999
            CALL LG2BIT (56, IFFLAG, IFLG, 1)
            CALL LG2BIT (756, BLFLAG, BFLG, 1)
            KV = KVIS - 1
            CALL EXTIO ('WRIT', LUNG, INDG, 0, GREC, GBUF, IER)
C                                       Check if done
            IF (NIO.EQ.0) GO TO 500
            NV = 0
 315        TIME1 = TIME
            TIME2 = TIME + DT
            KBMX = 0
            DO 319 K = 1,378
               NO(K) = 0
               VOBS(K,1) = CMPLX (0.0, 0.0)
               VOBS(K,2) = CMPLX (0.0, 0.0)
               WT(K,1) = 0.
               WT(K,2) = 0.
               UUVV(1, K) = 0.
               UUVV(2, K) = 0.
 319           CONTINUE
 320     CONTINUE
         IF (WTS(1)+WTS(2).EQ.0.) GO TO 390
         PSP2 = UU**2 + VV**2
         IF ((EPS.EQ.0.) .AND.
     *      ((PSP2.LT.BLMN2) .OR. (PSP2.GT.BLMX2))) GO TO 390
         NV = NV + 1
         IF (NV.EQ.1) T1 = TIME
         T2 = TIME
         KB = J - 28 + I * (55-I)/2
         KBMX = MAX (KB, KBMX)
         NO(KB) = NO(KB) + 1
         I1(KB) = I
         I2(KB) = J
         XX = 1.
         IF ((PSP2.LT.BLMN2) .OR. (PSP2.GT.BLMX2)) XX = EPS
         WT(KB,1) = WT(KB,1) + XX*WTS(1)
         WT(KB,2) = WT(KB,2) + XX*WTS(2)
         VOBS(KB,1) = VOBS(KB,1) + XX*WTS(1)*VIS(1)
         VOBS(KB,2) = VOBS(KB,2) + XX*WTS(2)*VIS(2)
         UUVV(1, KB) = UUVV(1, KB) + UU * (WTS(1)+WTS(2))
         UUVV(2, KB) = UUVV(2, KB) + VV * (WTS(1)+WTS(2))
 390     CONTINUE
C
C                                       Close vis. file.
 500  CALL ZCLOSE (LUN, FIND, IER)
      TLAST = TIME
C                                       Close gain file.
      CALL EXTIO ('CLOS', LUNG, INDG, 0, GREC, GBUF, IER)
C                                       Write message to user
      WRITE (MSGTXT,1500)
      CALL MSGWRT (1)
      GO TO 995
 990  CALL MSGWRT (8)
 995  IER = IERR
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SOLVE: TROUBLE IN MAPOPN, IER=',I2)
 1010 FORMAT ('SOLVE: TROUBLE IN UVINIT, IER=',I2)
 1020 FORMAT ('SOLVE: TROUBLE IN UVDISK, IER=',I2)
 1030 FORMAT ('SOLVE: ARRAY DIMENSIONS EXCEEDED.  USE THE',
     *   ' AVERAGING OPTION.')
 1040 FORMAT ('SOLVE: TROUBLE OPENING GAIN FILE, IER=',I2)
 1500 FORMAT ('Finished gain solutions')
      END
      SUBROUTINE SOLVE2 (TIME, I1, I2, WT, VOBS, UUVV, NV, IREFS, G,
     *   IFFLAG, MODE, IREF, MINNO, IPR, NMAX, WORK, BLFLAG, IERR)
C-----------------------------------------------------------------------
C   Sets up to call appropriate gain solution routine and flagging
C   routines.
C    Inputs:
C     TIME        R    Time of the data (seconds)
C     I1(*)       I    Array of first antenna numbers.
C     I2(*)       I    Array of second antenna numbers.
C     WT(*)       R    Array of visibility weights.
C     VOBS(*)     C*4  Complex normalized visibility.
C     UUVV(2,*)   R    UV positions of data
C     NV          I    Number of observations.
C     IREFS(2)    I    Desired reference antennas (1 per IF).
C     MODE        I    Solution mode: 0 = full gain soln.
C                                     1 = phase only keep ampl. info.
C                                     2 = phase only discard ampl. info.
C     MINNO       I    Minimun number oa antennas allowed for soln.
C     IPR         I    Print flag,    0 = none
C                                     1 = print (MSGWRT) soln.
C                                     2 = print data plus soln.
C     NMAX        I    Number of baselines
C     WORK(NMAX)  C*8  Work array
C    Outputs:
C     G(*)        C*4  Complex antenna gains to be applied.
C     IFFLAG(*)   L    IF flag array
C     IREF        I    Reference antenna used.
C     BLFLAG(*)   L    Baseline flag array
C     IERR        I    TELL told to abort
C-----------------------------------------------------------------------
      INTEGER   NMAX, NV
      LOGICAL   IFFLAG(28,2), BLFLAG(378,2), T, F
      INTEGER   I1(NMAX), I2(NMAX), IREFS(2), IER, K, L, LIM, M, NREF,
     *   MXANTS, MAXBL, IPR, IREF, MINNO, MODE, NOBS, IERR,
     *   UIND(0:501)
      REAL      WT(NMAX,2), TIME, SW, UWRK(2, 502), UUVV(2, 500)
      REAL      UVSAME
      COMPLEX   VOBS(NMAX,2), WORK(NMAX), G(28,2)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'RDCAL.INC'
      DATA T, F /.TRUE.,.FALSE./
C                                       Set max. number antennas, bl
      DATA MXANTS, MAXBL /28, 378/
C-----------------------------------------------------------------------
C                                       Section to average 2 i.f.'s:
      IERR = 0
      LIM = 2
C                                       Quantize U and V
      UVSAME = SPARE(1)
      IF (UVSAME .LE. 0.) UVSAME = 2.
      DO 5 L = 1, NV
         K = UUVV(1,L)/UVSAME
         UUVV(1,L) = FLOAT(K)
         K = UUVV(2,L)/UVSAME
         UUVV(2,L) = FLOAT(K)
 5    CONTINUE
      CALL OSORTI (UUVV, NV, NV+2, 1, 2, 2, UWRK, UIND, IERR)
      IF (AVAC.LE.0.) GO TO 25
         LIM = 1
         DO 10 K = 1,NV
            SW = WT(K,1) + WT(K,2)
C                                      If AVAC and AVG have both been
C                                      specified, both weights may be
C                                      zero:
            IF (SW.EQ.0.) GO TO 10
            VOBS(K,1) = (WT(K,1)*VOBS(K,1) + WT(K,2)*VOBS(K,2)) / SW
            WT(K,1) = 0.5 * SW
 10         CONTINUE
C                                       Section to call the gain
C                                       solution subroutine:
 25   DO 100 L = 1,LIM
         IF ((L.EQ.2) .AND. (IPR.GT.0)) CALL ASCTEL (IPR, IERR)
         IF (IERR.GT.0) GO TO 999
         WRITE (MSGTXT,1025) L
         IF (IPR.GT.0) CALL MSGWRT (2)
         DO 30 M = 1,MXANTS
            IFFLAG(M,L) = F
 30         CONTINUE
         DO 35 M = 1,MAXBL
            BLFLAG(M,L) = F
 35         CONTINUE
         NOBS = NV
         CALL GCALC (VOBS(1,L), I1, I2, WT(1,L),
     *      UUVV, UIND, WTRED,
     *      NOBS, IREF, MODE, G(1,L), IPR, IER, NREF, MINNO, WORK)
         DO 36 M = 1,MXANTS
            IF (((REAL (G(M,L))**2) + (AIMAG (G(M,L))**2))
     *         .LE.1.0E-30) GO TO 36
               G(M,L) = 1. / G(M,L)
 36         CONTINUE
         IREFS(L) = NREF
         DO 40 M = 1,MXANTS
            IF (IER.NE.0) IFFLAG(M,L) = T
            IF ((REAL(G(M,L)).EQ.1.) .AND. (AIMAG(G(M,L)).EQ.0.)
     *         .AND. (M.NE.NREF)) IFFLAG(M,L) = T
 40         CONTINUE
         IF (IER.NE.0) GO TO 60
            IF ((MODE.EQ.0) .AND. (SPARE(4).GT.0.)) CALL GEDIT
     *         (G(1,L), IFFLAG(1,L), SPARE(4))
            IF (SPARE(2).GT.0.) CALL FLAG (TIME, L, I1, I2, WT(1,L),
     *         VOBS(1,L), NV, G(1,L), IFFLAG(1,L), BLFLAG(1,L),
     *         SPARE(3))
 60   IF (MODE.NE.0) GO TO 100
C                                       Modcomp doesn't allow complex
C                                       comparisons in IF statements:
            DO 70 M = 1,MXANTS
               IF ((IFFLAG(M,L)) .OR. ((REAL(G(M,L)).EQ.1.) .AND.
     *            (AIMAG(G(M,L)).EQ.0.))) GO TO 70
                  CNTA = CNTA + 1D0
                  GS = GS + ABS (G(M,L))
 70            CONTINUE
 100     CONTINUE
      IF (LIM.NE.1) GO TO 999
         DO 110 M = 1,MXANTS
            G(M,2) = G(M,1)
            IFFLAG(M,2) = IFFLAG(M,1)
 110        CONTINUE
         IF (AVAC.LE.0.) GO TO 130
            DO 120 K = 1,MAXBL
               BLFLAG(K,2) = BLFLAG(K,1)
 120           CONTINUE
 130     IREFS(2) = IREFS(1)
C
 999  RETURN
C-----------------------------------------------------------------------
 1025 FORMAT ('******** IF ',I2,' **********')
      END
      SUBROUTINE VISCOR (IRET)
C-----------------------------------------------------------------------
C   VISCOR will read in uv data, apply appropriate gains from selfcal
C   solution, and write out to new uv file.  Also writes history file.
C   Programmer : Stuart Button - University of Toronto,June 1981,May 82
C-----------------------------------------------------------------------
      CHARACTER ITYP*2, TYPTMP*2
      LOGICAL   T, F, IFFLAG(28,2), BLFLAG(378,2)
      INTEGER   MMODE, C(256), FIND, BIND, WBUFF(256), HILUN, HILUN2,
     *   HIBUF(256), FIND2, BIND2, IFL, NUMREC, IRET, IROUND,  IDISK,
     *   IDISK2, IER, IRECL, ISEQ, IVER, J, K, KB, LUN, LUN2, LUNG, NIO,
     *   INDG, I, M, NITER, KVIS, KTOT, NFLG(4,4), VO, BO
      REAL      IBUF(2048), OBUF(2048), TBEG, TFIN, CATR(256),
     *   WTS(4), TIME, UU, VV, WW, GMEAN
      CHARACTER HILINE*72
      DOUBLE PRECISION CATD(128)
      LOGICAL   PTMOD, NOMOD, CLNMOD
      COMPLEX   VIS(4), Z
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'RDCAL.INC'
      INCLUDE 'RDCAL2.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (CATBLK, C, CATR, CATD)
      EQUIVALENCE (GBUF(255), GMEAN),
     *   (GBUF(254), IFL),
     *   (GBUF(252), TBEG),
     *   (GBUF(250), TFIN)
      EQUIVALENCE (RBUF, IBUF),
     *   (RBUF(2049), OBUF)
      DATA T, F /.TRUE., .FALSE./
      DATA NFLG /16*0/
C-----------------------------------------------------------------------
      IRET = 8
C                                       Find mean modulus of the
C                                       gain solutions.
      GM = 1.
      IF ((XMODE.EQ.0.0) .AND. (DOMOD.LE.0.0) .AND. (CNTA.GT.0.0))
     *   GM = GS / CNTA
C                                       If input data already divided by
C                                       model don't write output.
C      IF (POINT.LT.-0.1) GO TO 300
C                                       Open uncorrected uv file
      ISEQ = SEQ1
      IDISK = DISK1
      LUN = 16
      TYPTMP = 'UV'
      CALL MAPOPN ('READ', IDISK, NAME1, CLASS1, ISEQ, TYPTMP, NLUSER,
     *   LUN, FIND, CNO1, CATBLK, IBUF, IER)
      IF (IER.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IER
         CALL MSGWRT (8)
         GO TO 999
C                                       Mark file in /CFILES/
 10   NCFILE = NCFILE + 1
      FVOL(NCFILE) = IDISK
      FCNO(NCFILE) = CNO1
      FRW(NCFILE) = 0
      BO = 1
      VO = 0
      NIO = 1
      CALL UVINIT ('READ', LUN, FIND, NVIS, VO, LREC, NIO, JBUFSZ, IBUF,
     *   BO, BIND, IER)
      IF (IER.EQ.0) GO TO 15
         WRITE (MSGTXT,1010) IER
         CALL MSGWRT (8)
         GO TO 999
C                                       Open and initialize the output
C                                       file:
 15   IDISK = GVOL
      ISEQ = IROUND (SEQ3)
      LUN2 = 17
      CNO3 = GCNO
      TYPTMP = 'UV'
      CALL MAPOPN ('INIT', IDISK, NAME3, CLASS3, ISEQ, TYPTMP, NLUSER,
     *   LUN2, FIND2, CNO2, GCBLK, OBUF, IER)
      IF( IER.EQ.0) GO TO 30
         WRITE (MSGTXT,1030) IER
         CALL MSGWRT (8)
         GO TO 999
 30   NIO = 1
      CALL UVINIT ('WRIT', LUN2, FIND2, NVIS, VO, LREC, NIO, JBUFSZ,
     *   OBUF, BO, BIND2, IER)
      IF (IER.EQ.0) GO TO 40
         WRITE (MSGTXT,1040) IER
         CALL MSGWRT (8)
         GO TO 998
C                                       Open the gain extension file:
 40   LUNG = 18
      IRECL = 256
      CALL EXTINI ('READ', 'GA', GVOL, GCNO, GVER, GCBLK, LUNG, INDG,
     *   IRECL, 0, GBUF, IER)
      IF (IER.EQ.0) GO TO 50
         WRITE (MSGTXT,1050) IER
         CALL MSGWRT (8)
         GO TO 998
C                                       Enter the mean modulus of
C                                       the gain solutions into GBUF:
 50   GMEAN = GM
C                                       Read the first gain record:
      KVIS = 0
      NUMREC = 0
      CALL EXTIO ('READ', LUNG, INDG, NUMREC, GREC, GBUF, IER)
      IF (IER.EQ.0) GO TO 80
         WRITE (MSGTXT,1120) IER
         CALL MSGWRT (8)
         GO TO 990
 80   CALL LG2BIT (56, IFFLAG, IFLG, -1)
      CALL LG2BIT (756, BLFLAG, BFLG, -1)
      IF (XMODE.NE.0.) GO TO 100
C                                       Modcomp doesn't allow complex
C                                       comparisons in IF statements:
         DO 90 M = 1,28
            IF ((REAL(G(M,1)).NE.1.) .OR. (AIMAG(G(M,1)).NE.0.))
     *         G(M,1) = G(M,1) / GM
            IF ((REAL(G(M,2)).NE.1.) .OR. (AIMAG(G(M,2)).NE.0.))
     *         G(M,2) = G(M,2) / GM
 90         CONTINUE
 100  CONTINUE
C                                        Read one vis record
      NIO = 1
      CALL UVDISK ('READ', LUN, FIND, IBUF, NIO, BIND, IER)
      IF (IER.EQ.0) GO TO 110
         WRITE (MSGTXT,1110) IER
         CALL MSGWRT (8)
         GO TO 997
 110  KVIS = KVIS + 1
      IF (NIO.EQ.0) GO TO 900
         IF (KVIS.LE.KV) GO TO 120
C                                        If necessary read next gain
C                                        record to use for corrections
C                                        up to uv record KV
            CALL EXTIO ('READ', LUNG, INDG, NUMREC, GREC, GBUF, IER)
            IF ((IER.EQ.0) .OR. (IER.EQ.4)) GO TO 115
               WRITE (MSGTXT,1120) IER
               CALL MSGWRT (8)
               GO TO 990
 115        CALL LG2BIT (56, IFFLAG, IFLG, -1)
            CALL LG2BIT (756, BLFLAG, BFLG, -1)
            IF (XMODE.NE.0.) GO TO 120
               DO 116 M = 1,28
                  IF ((REAL(G(M,1)).NE.1.) .OR. (AIMAG(G(M,1)).NE.0.))
     *               G(M,1) = G(M,1) / GM
                  IF ((REAL(G(M,2)).NE.1.) .OR. (AIMAG(G(M,2)).NE.0.))
     *               G(M,2) = G(M,2) / GM
 116              CONTINUE
 120  CALL CNV (IBUF, BIND, TIME, I, J, UU, VV, WW, WTS, VIS)
      CALL RCOPY (LREC, IBUF(BIND), OBUF(BIND2))
      KB = J - 28 + I * (55-I) / 2
C                                       NFLG(I,J) counts the number of
C                                       flagged RR, LL, RL, and LR
C                                       flagged points for I=1,2,3,4,
C                                       resp.  J=1 is the counter for
C                                       previously flagged points,
C                                       J=2 for points deleted because
C                                       of trouble in the gain solution
C                                       routine, J=3 for points which
C                                       which are edited out in FLAG,
C                                       and J=4 for the points which
C                                       remain.
C                                       Correct RR
      K = BIND2 + ILOCV1
      IF (OBUF(K+2).LE.0.) GO TO 135
      IF (BLFLAG(KB,1)) GO TO 136
      IF ((IFFLAG(J,1)) .OR. (IFFLAG(I,1))) GO TO 137
      Z = G(I,1) * CONJG (G(J,1)) * VIS(1)
      OBUF(K) = REAL(Z)
      OBUF(K+1) = AIMAG(Z)
      GO TO 140
 135     NFLG(1,1) = NFLG(1,1) + 1
         GO TO 140
 136     NFLG(1,3) = NFLG(1,3) + 1
         GO TO 138
 137     NFLG(1,2) = NFLG(1,2) + 1
 138     OBUF(K+2) = -OBUF(K+2)
 140  CONTINUE
C                                       Correct LL
      IF (NCOR.EQ.1) GO TO 160
      K = BIND2 + ILOCV2
      IF (OBUF(K+2).LE.0.) GO TO 155
      IF (BLFLAG(KB,2)) GO TO 156
      IF ((IFFLAG(J,2)) .OR. (IFFLAG(I,2))) GO TO 157
      Z = G(I,2) * CONJG (G(J,2)) * VIS(2)
      OBUF(K) = REAL(Z)
      OBUF(K+1) = AIMAG(Z)
      GO TO 160
 155     NFLG(2,1) = NFLG(2,1) + 1
         GO TO 160
 156     NFLG(2,3) = NFLG(2,3) + 1
         GO TO 158
 157     NFLG(2,2) = NFLG(2,2) + 1
 158     OBUF(K+2) = -OBUF(K+2)
 160  CONTINUE
C                                       If no cross cors skip RL LR
      IF (NCOR.LE.2) GO TO 200
C                                       Correct RL
      K = BIND2 + ILOCV3
      IF (OBUF(K+2).LE.0.) GO TO 175
      IF ((BLFLAG(KB,1)) .OR. (BLFLAG(KB,2))) GO TO 176
      IF ((IFFLAG(J,2)) .OR. (IFFLAG(I,1))) GO TO 177
      Z = G(I,1) * CONJG (G(J,2)) * VIS(3)
      OBUF(K) = REAL(Z)
      OBUF(K+1) = AIMAG(Z)
      GO TO 180
 175     NFLG(3,1) = NFLG(3,1) + 1
         GO TO 180
 176     NFLG(3,3) = NFLG(3,3) + 1
         GO TO 178
 177     NFLG(3,2) = NFLG(3,2) + 1
 178     OBUF(K+2) = -OBUF(K+2)
 180  CONTINUE
C                                       Correct LR
      K = BIND2 + ILOCV4
      IF (OBUF(K+2).LE.0.) GO TO 195
      IF ((BLFLAG(KB,1)) .OR. (BLFLAG(KB,2))) GO TO 196
      IF ((IFFLAG(J,1)) .OR. (IFFLAG(I,2))) GO TO 197
      Z = G(I,2) * CONJG (G(J,1)) * VIS(4)
      OBUF(K) = REAL(Z)
      OBUF(K+1) = AIMAG(Z)
      GO TO 200
 195     NFLG(4,1) = NFLG(4,1) + 1
         GO TO 200
 196     NFLG(4,3) = NFLG(4,3) + 1
         GO TO 198
 197     NFLG(4,2) = NFLG(4,2) + 1
 198     OBUF(K+2) = -OBUF(K+2)
 200  CONTINUE
C                                        Write out corrected record
      NIO = 1
      CALL UVDISK ('WRIT', LUN2, FIND2, OBUF, NIO, BIND2, IER)
      IF (IER.EQ.0) GO TO 100
         WRITE (MSGTXT,1250) IER
         CALL MSGWRT (8)
         GO TO 997
C                                       We're finished, now, correcting
C                                       the data.
 900  KTOT = NVIS
      DO 251 I = 1,NCOR
         NFLG(I,4) = KTOT - NFLG(I,1) - NFLG(I,2) - NFLG(I,3)
 251     CONTINUE
      WRITE (MSGTXT,1904)
      CALL MSGWRT (5)
      WRITE (MSGTXT,1905) (NFLG(J,1), J = 1,4)
      CALL MSGWRT (5)
      WRITE (MSGTXT,1906) (NFLG(J,2), J = 1,4)
      CALL MSGWRT (5)
      WRITE (MSGTXT,1907) (NFLG(J,3), J = 1,4)
      CALL MSGWRT (5)
      WRITE (MSGTXT,1908) (NFLG(J,4), J = 1,4)
      CALL MSGWRT (5)
C
 997  CALL UVDISK ('FLSH', LUN2, FIND2, OBUF, NIO, BIND2, IER)
      IF (IER.NE.0) IRET = 8
      CALL ZCLOSE (LUN2, FIND2, IER)
C
 998  CALL ZCLOSE (LUN, FIND, IER)
 990  CALL EXTIO ('CLOS', LUNG, INDG, NUMREC, GREC, GBUF, IER)
C                                       Call EXTINI with op-code 'WRIT',
C                                       so that the mean modulus of the
C                                       gain solutions and start/stop
C                                       times will be entered into the
C                                       file when it is closed.
 300  LUNG = 18
      IRECL = 256
      CALL EXTINI ('WRIT', 'GA', GVOL, GCNO, GVER, GCBLK, LUNG, INDG,
     *   IRECL, NGREC, GBUF, IER)
      IF (IER.EQ.0) GO TO 995
         WRITE (MSGTXT,1050) IER
         CALL MSGWRT (8)
         GO TO 999
C                                       Before closing the gain file,
C                                       compute a flag telling what
C                                       if's have gain solutions:  1=>R,
C                                       2=>L, and 3=>both R & L.
 995  IFL = 3
      IF ((NFLG(1,4).GT.0) .AND. (NFLG(2,4).LE.0)) IFL = 1
      IF ((NFLG(1,4).LE.0) .AND. (NFLG(2,4).GT.0)) IFL = 2
C                                       Start/stop times in GBUF
      TFIN = TLAST
      TBEG = TFIRST
      GMEAN = GM
      CALL EXTIO ('CLOS', LUNG, INDG, 0, GREC, GBUF, IER)
      CALL HIINIT (2)
      IRET = 0
C                                        Copy old hist file
      IDISK = IROUND (DISK1)
      HILUN = 27
      HILUN2 = 28
      IDISK2 = IROUND (DISK3)
      IRET = 0
      CALL HISCOP (HILUN, HILUN2, IDISK, IDISK2, CNO1, CNO3, GCBLK,
     *   WBUFF, HIBUF, IER)
      IF (IER.GE.3) GO TO 985
      ISEQ = IROUND (SEQ1)
C                                        Write old uv file info to hist
      IDISK = IROUND (DISK1)
      CALL HENCO1 (TSKNAM, NAME1, CLASS1, ISEQ, IDISK, HILUN2,
     *   HIBUF, IER)
C                                       Check if no model or point model
      PTMOD = POINT .GT. 0.1
      NOMOD = POINT .LT. -0.1
      CLNMOD = .NOT. (PTMOD.OR.NOMOD)
C                                        Write clean map file info
      ISEQ = IROUND (SEQ2)
      IDISK = IROUND (DISK2)
      IF (CLNMOD)CALL HENCO2 (TSKNAM, NAME2, CLASS2, ISEQ, IDISK,
     *   HILUN2, HIBUF, IER)
C                                        Write new uv file info to hist
      ISEQ = IROUND (SEQ3)
      IDISK = IROUND (DISK3)
      CALL HENCOO (TSKNAM, NAME3, CLASS3, ISEQ, IDISK, HILUN2,
     *   HIBUF, IER)
C                                        Write selfcal parms to HI file
      DO 921 IFL = 1,MFIELD
         NITER = XNCOMP(IFL)
         IVER = IROUND (XVER)
         WRITE (HILINE,1920) IFL, NITER, IVER
         IF (CLNMOD) CALL HIADD (HILUN2, HILINE, HIBUF, IER)
         IF (IER.EQ.0) GO TO 921
            WRITE (MSGTXT,1915) IER
            CALL MSGWRT (8)
            GO TO 985
 921        CONTINUE
 920  WRITE (HILINE,1925) BLMIN, BLMAX
      CALL HIADD (HILUN2, HILINE, HIBUF, IER)
      IF (IER.EQ.0) GO TO 925
         WRITE (MSGTXT,1915) IER
         CALL MSGWRT (8)
         GO TO 985
 925  WRITE (HILINE,1930) XREF, XMINNO
      CALL HIADD (HILUN2, HILINE, HIBUF, IER)
      IF (IER.EQ.0) GO TO 930
         WRITE (MSGTXT,1915) IER
         CALL MSGWRT (8)
         GO TO 985
 930  IF (.NOT.PTMOD) GO TO 945
         WRITE (HILINE,1940) FLUX
         CALL HIADD (HILUN2, HILINE, HIBUF, IER)
         IF (IER.EQ.0) GO TO 940
            WRITE (MSGTXT,1915) IER
            CALL MSGWRT (8)
            GO TO 985
 940     WRITE (HILINE,1945) DELTAX,DELTAY
         CALL HIADD (HILUN2, HILINE, HIBUF, IER)
         IF (IER.EQ.0) GO TO 945
            WRITE (MSGTXT,1915) IER
            CALL MSGWRT (8)
            GO TO 985
 945  IF (AVAC.LE.0.) GO TO 950
         WRITE (HILINE,1950)
         CALL HIADD (HILUN2, HILINE, HIBUF, IER)
         IF (IER.EQ.0) GO TO 950
            WRITE (MSGTXT,1915) IER
            CALL MSGWRT (8)
            GO TO 985
 950  WRITE (HILINE,1960) XLEN
      CALL HIADD (HILUN2, HILINE, HIBUF, IER)
      IF (IER.EQ.0) GO TO 953
         WRITE (MSGTXT,1915) IER
         CALL MSGWRT (8)
         GO TO 985
C                                       Solution mode.
 953  MMODE = XMODE + 1
      WRITE (HILINE,2960) MMODE
      CALL HIADD (HILUN2, HILINE, HIBUF, IER)
      IF (IER.EQ.0) GO TO 955
         WRITE (MSGTXT,1915) IER
         CALL MSGWRT (8)
         GO TO 985
 955  IF ((DOMOD.LE.0.0) .OR. (XMODE.NE.0.0)) GO TO 960
         WRITE (HILINE,1955)
         CALL HIADD (HILUN2, HILINE, HIBUF, IER)
         IF (IER.EQ.0) GO TO 960
            WRITE (MSGTXT,1915) IER
            CALL MSGWRT (8)
            GO TO 985
 960  IF (SPARE(1).NE.1.) GO TO 961
         WRITE (HILINE,2900)
         CALL HIADD (HILUN2, HILINE, HIBUF, IER)
 961  IF (SPARE(2).LE.0.) GO TO 962
         WRITE (HILINE,2901) SPARE(3)
         CALL HIADD (HILUN2, HILINE, HIBUF, IER)
 962  IF ((SPARE(4).LE.0.) .OR. (XMODE.NE.0.)) GO TO 963
         WRITE (HILINE,2902) SPARE(4)
         CALL HIADD (HILUN2, HILINE, HIBUF, IER)
C                                       No-gain-normalize flag.
 963  WRITE (HILINE,2903) TSKNAM, DOMOD
      CALL HIADD (HILUN2, HILINE, HIBUF, IER)
      WRITE (HILINE,2904)
      CALL HIADD (HILUN2, HILINE, HIBUF, IER)
      WRITE (HILINE,2905) (NFLG(J,1), J = 1,4)
      CALL HIADD (HILUN2, HILINE, HIBUF, IER)
      WRITE (HILINE,2906) (NFLG(J,2), J = 1,4)
      CALL HIADD (HILUN2, HILINE, HIBUF, IER)
      WRITE (HILINE,2907) (NFLG(J,3), J = 1,4)
      CALL HIADD (HILUN2, HILINE, HIBUF, IER)
      WRITE (HILINE,2908) (NFLG(J,4), J = 1,4)
      CALL HIADD (HILUN2, HILINE, HIBUF, IER)
 985  CALL HICLOS (HILUN2, T, HIBUF, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VISCOR: UNABLE TO OPEN OLD UVFILE, IER=', I2)
 1010 FORMAT ('VISCOR: UNABLE TO INIT OLD UV FILE, IER=',I2)
 1030 FORMAT ('VISCOR: UNABLE TO OPEN OUTPUT FILE, IER=', I2)
 1040 FORMAT ('VISCOR: UNABLE TO INIT OUTPUT FILE, IER=',I2)
 1050 FORMAT ('VISCOR: UNABLE TO INIT GAIN EXT''N FILE, IER=',I2)
 1110 FORMAT ('VISCOR: ERROR READING OLD UV DATA, IER=',I2)
 1120 FORMAT ('VISCOR: ERROR READING GAIN RECORD, IER=',I2)
 1250 FORMAT ('VISCOR: ERROR WRITING NEW UV DATA, IER=',I2)
 1904 FORMAT ('VISCOR:',30X,'   RR  ','   LL  ','   RL  ','   LR  ')
 1905 FORMAT ('VISCOR:   Points previously flagged:',4I7)
 1906 FORMAT ('VISCOR:   Flagged for bad gain soln:',4I7)
 1907 FORMAT ('VISCOR:           Points edited out:',4I7)
 1908 FORMAT ('VISCOR:            Points remaining:',4I7)
 1915 FORMAT ('VISCOR: ERROR ADDING LINE TO HISTORY FILE, IER=',I2)
 1920 FORMAT ('RDCAL NCOMP(',I2,')=',I5,', VER=',I3,
     *   ' /No. of CLEAN comp''s used')
 1925 FORMAT ('RDCAL BLMIN=',E10.3,' BLMAX=',E10.3,
     *   ' /Baseline limits (klambda)')
 1930 FORMAT ('RDCAL REFANT=',F3.0,' MINANT=',F3.0,
     *   ' /Reference ant and min # ants for solution')
 1940 FORMAT ('RDCAL /Solved assuming a ',F8.3,' Jy point source')
 1945 FORMAT ('RDCAL /East offset= ',F8.3,'"  North offset= ',F8.3,
     *   '"  from map center')
 1950 FORMAT ('RDCAL /RR and LL averaged before gain solution '
     *   'calculated')
 1955 FORMAT ('RDCAL /Gain normalization disabled')
 1960 FORMAT ('RDCAL /Data averaged for',F5.1,
     *   ' minutes for each gain solution')
 2900 FORMAT ('RDCAL /L1 minimization routine used to compute gains')
 2901 FORMAT ('RDCAL /Points more than ',F6.2,
     *   ' std. dev. deviant flagged')
 2902 FORMAT ('RDCAL /Flag IF''s whose amplitude corr ',
     *   '> ',F6.2,' std dev deviant from mean')
 2903 FORMAT (A6,' APARM(10)=',F5.2,' / .le. 0 => gain normalized')
 2904 FORMAT ('RDCAL /',34X,'   RR  ','   LL  ','   RL  ','   LR  ')
 2905 FORMAT ('RDCAL /       Points previously flagged:',4I7)
 2906 FORMAT ('RDCAL /Pts flagged due to bad gain soln:',4I7)
 2907 FORMAT ('RDCAL /               Points edited out:',4I7)
 2908 FORMAT ('RDCAL /                Points remaining:',4I7)
 2960 FORMAT ('RDCAL APARM(7)=',I5,' / Solution type')
      END
      SUBROUTINE FLAG (TIME, IFNO, I1, I2, WT, VOBS, NV, G, IFFLAG,
     *   BLFLAG, XSTD)
C-----------------------------------------------------------------------
C   Flags data on an IF basis
C    Inputs:
C     TIME        R    Time of the data (seconds)
C     INFO        I    IF number
C     I1(*)       I    Array of first antenna numbers.
C     I2(*)       I    Array of second antenna numbers.
C     WT(*)       R    Array of visibility weights.
C     VOBS(*)     C*4  Complex normalized visibility.
C     NV          I    Number of observations.
C     G(*)        C*4  Complex antenna gains to be applied.
C     XSTD        R    Minimum deviation in sigma to flag
C    Outputs:
C     IFFLAG(*)   L    IF flag array
C     BLFLAG(*)   L    Baseline flag array
C-----------------------------------------------------------------------
      CHARACTER IFNAM(2)*2
      INTEGER   NV
      LOGICAL   T, F, IFFLAG(1), BLFLAG(1)
      INTEGER   I1(NV), I2(NV), IFNO, J, K, KB, N, NF, I
      REAL      WT(NV), TIME, XTIME, D, S, SIGMA, XSTD
      COMPLEX   R, VOBS(NV), VMOD, G(28)
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA IFNAM /'RR','LL'/
C-----------------------------------------------------------------------
      IF (NV.LE.0) GO TO 999
C                                       Compute the (unweighted)
C                                       rms visibility residual.
      S = 0.
      N = 0
      VMOD = CMPLX (1.0, 0.0)
      DO 10 K = 1,NV
         I = I1(K)
         J = I2(K)
         IF ((IFFLAG(I)) .OR. (IFFLAG(J)) .OR. (WT(K).LT.1.)) GO TO 10
            N = N + 1
            R = VMOD - G(J) * CONJG (G(I)) * VOBS(K)
            S = S + REAL (R)**2 + AIMAG (R)**2
 10      CONTINUE
      IF (N.LT.2) GO TO 999
      SIGMA = SQRT (S/(N-1))
C                                       Flag any data more than XSTD
C                                       standard deviations discrepant.
      DO 20 KB = 1,378
         BLFLAG(KB) = F
 20      CONTINUE
      IF (SIGMA.EQ.0.) GO TO 999
      NF = 0
      DO 30 K = 1,NV
         I = I1(K)
         J = I2(K)
         IF ((IFFLAG(I)) .OR. (IFFLAG(J)) .OR. (WT(K).LT.1.)) GO TO 30
            R = VMOD - G(J) * CONJG (G(I)) * VOBS(K)
            D = ABS (R) / SIGMA
            IF (D.LE.ABS (XSTD)) GO TO 30
               NF = NF + 1
               KB = J - 28 + I*(55-I)/2
               BLFLAG(KB) = T
               XTIME = TIME / 86400.
               WRITE (MSGTXT,1001) XTIME, I, J, IFNAM(IFNO), D
               CALL MSGWRT (8)
 30      CONTINUE
      IF (NF.EQ.0) GO TO 999
         IF (NF.EQ.1) WRITE (MSGTXT,1002)
         IF (NF.GT.1) WRITE (MSGTXT,1000) NF
         CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I5,' Visibilities flagged.')
 1002 FORMAT ('  One visibility flagged.')
 1001 FORMAT (F10.5,I5,'-',I2,1X,A1,' flagged.',F15.3,' sigma.')
      END
      SUBROUTINE GEDIT (G, IFFLAG, XSTD)
C-----------------------------------------------------------------------
C   Flags data on IFs which differ by more than XSTD sigma from the
C   other IF.
C    Inputs:
C     G(*)        C*4  Complex antenna gains.
C     XSTD        R    Minimum deviation in sigma to flag
C    Outputs:
C     IFFLAG(*)   L    IF flag array
C-----------------------------------------------------------------------
      LOGICAL   IFFLAG(*), T
      INTEGER   I, MXANTS
      REAL      A, AM, D, SIGMA, XSTD
      COMPLEX   G(*)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'RDCAL.INC'
      DATA T /.TRUE./
C                                       Set maximum number of antennas
      DATA MXANTS /28/
C-----------------------------------------------------------------------
      IF (INITED.EQ.0) GO TO 5
         SN = 0.D0
         S1 = 0.D0
         S2 = 0.D0
         INITED = 0
 5    DO 20 I = 1,MXANTS
C                                       Modcomp doesn't allow complex
C                                       comparisons in IF statements:
         IF ((REAL(G(I)).EQ.1.) .AND. (AIMAG(G(I)).EQ.0.)) GO TO 10
            SN = SN + 1.D0
            A = ABS (G(I))
            S1 = S1 + A
            S2 = S2 + A**2
            GO TO 20
 10      IFFLAG(I) = T
 20      CONTINUE
      IF (SN.LE.0D0) GO TO 999
      AM = S1 / SN
      SIGMA = S2 - 2.*AM*S1 + SN*AM**2
      IF (SIGMA.LE.0.) GO TO 999
      SIGMA = SQRT (SIGMA/SN)
      DO 30 I = 1,MXANTS
         IF (IFFLAG(I)) GO TO 30
            D = ABS (G(I)) - AM
            IF (ABS(D)/SIGMA.GT.XSTD) IFFLAG(I) = T
 30      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE GCALC (VOBS, I1, I2, WT,
     *  UUVV, UIND, WTRED,
     *  NOBS, IREF, MODE, G, IPR,
     *   IER, NREF, MINNO, WORK)
C-----------------------------------------------------------------------
C   GCALC computes antenna gains given visibilities divided by the
C   model values.
C   Inputs:
C    VOBS(2,*)   R    Normalized visibility (real, imaginary)
C    I1(*)       I    Array of first antenna numbers.
C    I2(*)       I    Array of second antenna numbers.
C    WT(*)       R    Array of visibility weights.
C    UUVV(2, *)  R    UV Array
C    UIND(*)     I    Index to UV array
C    NOBS        I    Number of observations.
C    IREF        I    Desired reference antenna.
C    MODE        I    Solution mode: 0 = full gain soln.
C                                    1 = phase only keep ampl. info.
C                                    2 = phase only discard ampl. info.
C    IPR         I    Print flag,    0 = none
C                                    1 = print (MSGWRT) soln.
C                                    2 = print data plus soln.
C    MINNO       I    Min. number of antennas acceptable.
C   Outputs:
C    G(2,*)      R    Antenna gains to be applied (real, imaginary)
C    NREF        I    Reference antenna used.
C    IER         I    Return error code 0 = OK
C                                       1 = no valid data
C                                       2 = (not used)
C                                       3 = too few antennas
C-----------------------------------------------------------------------
      REAL      UUVV(2, 500), WTRED
      COMPLEX G(*), Z, GLAST(28)
      COMPLEX GRADA(28), HESSA(28,28, 2), GRADB(28), HESSB(28,28, 2),
     *          RESID, TRESID, COM0, COM1, WA, WB,
     *   GREAT(28), LESS(28), VOBS(*), WK, WORK(*)
      INTEGER URED, NRED
      REAL      WT(*), SWT(28), AMP, GD, PH, QQ, RMS, S, SUMWT, TOL,
     *   W, X, XX, XXX, XFAIL, XFAIL1, XFAIL2, WW, SUMWTA,
     *   WX, WNTRED, WRED
      INTEGER   I1(*), I2(*), I, II, IT, J, JJ, K, NF, NIT, NT, NTD,
     *   NTM1, MXANTS, IPR, IREF, IER, MINNO, MODE, NOBS, NREF, ITMAX,
     *   IDUMC, NFAIL, IFAIL, UIND(0:501), TIND
      LOGICAL   CONVGD, TREDUN(28)
      INCLUDE 'INCS:DMSG.INC'
C                                       Set max number of antennas.
      DATA MXANTS /28/
      DATA IDUMC /0/
C-----------------------------------------------------------------------
C                                       Compute relative weights
C                                       redundant and not redundant
C                                       baselines
      WNTRED = 1.
      WRED   = WTRED
      IF (WTRED .GT. 10000.) THEN
         WNTRED = 0.
         WRED   = 1.
      ENDIF
C
      COM0 = CMPLX (0., 0.)
      COM1 = CMPLX (1., 0.)
      IDUMC = IDUMC + 1
      ITMAX = 60
      TOL = 5.0E-6
      NT = 0
      NTD = 0
      SUMWT = 0.
      DO 10 I = 1,MXANTS
         SWT(I) = 0.
 10      CONTINUE
      IF (NOBS.LE.0) GO TO 7001
      WRITE (MSGTXT,1000)
      IF (IPR.GE.2) CALL MSGWRT (2)
      DO 40 K = 1,NOBS
         TIND = UIND(K)
         IF (IPR.GE.2) WRITE (MSGTXT,1001) TIND, VOBS(TIND),
     *      I1(TIND), I2(TIND), WT(TIND)
         IF (IPR.GE.2) CALL MSGWRT (2)
         IF (WT(TIND).EQ.0.) GO TO 40
            I = I1(TIND)
            J = I2(TIND)
            NT = MAX (NT, J)
            XXX = ABS(VOBS(TIND))
            IF (XXX.LE.1.0E-20) WT(TIND) = 0.
            SWT(I) = SWT(I) + WT(TIND)
            SWT(J) = SWT(J) + WT(TIND)
            SUMWT = SUMWT + WT(K)
            IF (WT(TIND).EQ.0.) GO TO 40
               IF (MODE.NE.2) GO TO 35
                  VOBS(TIND) = VOBS(TIND) / XXX
 35            WORK(TIND) = WT(TIND) * VOBS(TIND)
 40      CONTINUE
      DO 50 I = 1,MXANTS
         G(I) = COM1
         GLAST(I) = COM1
         IF (SWT(I).GT.0.) NTD = NTD + 1
 50      CONTINUE
      IF (SUMWT.EQ.0.) GO TO 7001
      NREF = IREF
      IF ((NREF.GE.1) .AND. (NREF.LE.MXANTS) .AND. (SWT(NREF).NE.0.))
     *   GO TO 100
         XX = 0.
         DO 90 I = 1,NT
            IF (SWT(I).LE.XX) GO TO 90
               XX = SWT(I)
               NREF = I
 90         CONTINUE
 100  NTM1 = NT - 1
      IF (NTD.LT.MINNO) GO TO 7003
      NF = NTD - 1
C                                       Print statistics
      IF (IPR.LT.1) GO TO 170
         WRITE (MSGTXT,1002)
         CALL MSGWRT (2)
         S = 0.
         DO 160 K = 1,NOBS
            TIND = UIND(K)
            IF (WT(TIND).GT.0.) THEN
               II = I1(TIND)
               JJ = I2(TIND)
               RESID = VOBS(TIND) - G(II) * CONJG (G(JJ))
               QQ = REAL (RESID * CONJG(RESID))
               S = S + WT(TIND) * QQ
            ENDIF
 160     CONTINUE
         RMS = SQRT (S/SUMWT)
         IT = 0
         WRITE (MSGTXT,1003) IT, S, RMS
         CALL MSGWRT (2)
 170  DO 310 IT = 1,ITMAX
         IF (IT.GT.15) TOL = (IT - 10) * 1.0E-6
         NIT = IT
C                                       Following for amplitude and
C                                       phase solution.
            IF (IT.EQ.1) W = .5
            IF (IT.EQ.2) W = .75
            IF (IT.GT.2) W = .9
            IF (IT.GT.10) W = .5
C            IF (NTD.LE.6) W = .25
            DO 190 I = 1,NT
               GRADA(I) = COM0
               GRADB(I) = COM0
               DO 180 J = 1, NT
                  DO 175 K = 1, 2
                     HESSA(I,J, K) = COM0
                     HESSB(I,J, K) = COM0
 175               CONTINUE
 180           CONTINUE
 190        CONTINUE
            K = 1
            SUMWTA = 0.
C                                       Loop over visibilities
C                                       Translate to Fortran
 9220       CONTINUE
C            DO 220 WHILE (K .LE. NOBS)
               URED = K
C                                       Find redundant baselines
 9200          CONTINUE
C                                       Translate to Fortran
C               DO 200 WHILE ((UUVV(1,UIND(URED+1)).EQ.UUVV(1,UIND(K)))
C     *                 .AND. (UUVV(2,UIND(URED+1)).EQ.UUVV(2,UIND(K))))
                     URED = URED + 1
                     IF ((UUVV(1,UIND(URED+1)).EQ.UUVV(1,UIND(K)))
     *                  .AND. UUVV(2,UIND(URED+1)) .EQ.
     *                  UUVV(2,UIND(K)))  GO TO 9200
  200          CONTINUE
               NRED = URED - K + 1
               TRESID = COM0
C                                       Compute grad/hess of sum
C                                       RESID**2
              IF (NRED .EQ. 1) THEN
                  WX = WNTRED
              ELSE
                  WX = WNTRED + WRED
              ENDIF
              DO 209 J = K, URED
                  TIND = UIND(J)
                  II = I1(TIND)
                  JJ = I2(TIND)
                  WW =  WX * WT(TIND)
                  RESID = VOBS(TIND) - G(II) * CONJG (G(JJ))
                  TRESID = TRESID + WW * RESID
                  SUMWTA = SUMWTA + WW
                  WW = 2. * WW
C                                       Gradients
                  GRADA(II) = GRADA(II) - WW*RESID*G(JJ)
                  GRADA(JJ) = GRADA(JJ) - WW*CONJG(RESID)*G(II)
C                                       Hessian (realreal,realimag)
                  HESSA(II,JJ,1) = HESSA(II,JJ,1) + WW*(G(II)*G(JJ) -
     *               RESID)
                  HESSA(II,II,1) = HESSA(II,II,1) +
     *               WW*G(JJ)*CONJG(G(JJ))
                  HESSA(JJ,JJ,1) = HESSA(JJ,JJ,1) +
     *               WW*G(II)*CONJG(G(II))
C                                       Hessian (imagimag,imagreal)
                  HESSA(II,JJ,2) = HESSA(II,JJ,2) - WW*
     *                  (CONJG(G(II)*G(JJ)) + RESID)
                  HESSA(II,II,2) = HESSA(II,II,2) +
     *               WW*G(JJ)*CONJG(G(JJ))
                  HESSA(JJ,JJ,2) = HESSA(JJ,JJ,2) +
     *               WW*G(II)*CONJG(G(II))
 209           CONTINUE
C                                       If redundant baselines arrange
C                                       that average visibility doesn't
C                                       change
               IF (NRED .LE. 1) GOTO 220
                  WA = WRED * TRESID/SUMWTA
                  DO 212 I = 1, NT
                     GREAT(I) = COM0
                     LESS(I)  = COM0
                     TREDUN(I) = .FALSE.
 212              CONTINUE
                  DO 215 J = K, URED
                     TIND = UIND(J)
                     II = I1(TIND)
                     JJ = I2(TIND)
                     TREDUN(II) = .TRUE.
                     TREDUN(JJ) = .TRUE.
                     WW = WT(TIND)
                     WB = 2. * WW * WA
                     GREAT(II) = GREAT(II) + WW*G(JJ)
                     LESS(JJ)  = LESS(JJ)  + WW*G(II)
                     GRADB(II) = GRADB(II) - WB * G(JJ)
                     GRADB(JJ) = GRADB(JJ) - CONJG(WB) * G(II)
                     HESSB(II,JJ,1) = HESSB(II,JJ,1) - WB
                     HESSB(II,JJ,2) = HESSB(II,JJ,2) -CONJG(WB)
 215              CONTINUE
                  WW = 2. * WRED * WX/SUMWTA
                  DO 217 I = 1, NT
                     IF (TREDUN(I)) THEN
                     DO 216 J = I, NT
                        IF (TREDUN(J)) THEN
                        HESSB(I,J,1) = HESSB(I,J,1) + WW * (
     *                  GREAT(I)*LESS(J) + LESS(I)*GREAT(J) +
     *                  GREAT(I)*CONJG(GREAT(J))+LESS(I)*CONJG(LESS(J)))
                        HESSB(I,J,2) = HESSB(I,J,2) -  WW * (
     *                  CONJG(GREAT(I)*LESS(J) + LESS(I)*GREAT(J))+
     *                  GREAT(I)*CONJG(GREAT(J))+LESS(I)*CONJG(LESS(J)))
                        ENDIF
 216                 CONTINUE
                     ENDIF
 217              CONTINUE
 220           K = K + NRED
               IF (K.LE.NOBS) GO TO 9220
C                                       Take difference of G,H
               DO 222 I = 1, NT
                  GRADA(I) = GRADA(I) - GRADB(I)
                  HESSA(I,I,1) = HESSA(I,I,1) - HESSB(I,I,1)
                  HESSA(I,I,2) = HESSA(I,I,2) - HESSB(I,I,2)
                  DO 221 J = I+1, NT
                     HESSA(I,J,1) = HESSA(I,J,1) - HESSB(I,J,1)
                     HESSA(I,J,2) = HESSA(I,J,2) - HESSB(I,J,2)
 221              CONTINUE
 222           CONTINUE
C                                       If phase only, force the
C                                       gradient to be perpendicular
C                                       to current direction of G
               IF (MODE.GT.0) THEN
                  DO 223 I = 1, NT
                     WW = REAL(GRADA(I)) * REAL(G(I)) +
     *                    AIMAG(GRADA(I)) * AIMAG(G(I))
                     GRADA(I) = GRADA(I) - WW * G(I)/(G(I)*CONJG(G(I)))
 223              CONTINUE
               ENDIF
C                                       Calculate step size
               WX = 0.
               WW = 0.
               DO 225 I = 1, NT
                  WW = WW + GRADA(I) * CONJG(GRADA(I))
                  WX = WX + REAL(GRADA(I))**2  * REAL(HESSA(I,I,1)) +
     *                   AIMAG(GRADA(I))**2 * REAL(HESSA(I,I,2))
               DO 224 J = I+1, NT
                  WX = WX
     *            +2.*REAL(GRADA(I))*REAL(GRADA(J))*REAL(HESSA(I,J,1))
     *            +2.*REAL(GRADA(I))*AIMAG(GRADA(J))*AIMAG(HESSA(I,J,1))
     *            +2.*AIMAG(GRADA(I))*AIMAG(GRADA(J))*REAL(HESSA(I,J,2))
     *            +2.*AIMAG(GRADA(I))*REAL(GRADA(J))*AIMAG(HESSA(I,J,2))
 224           CONTINUE
 225        CONTINUE
            IF (WX .LT. 1.E-16) THEN
               CONVGD = .TRUE.
               GO TO 281
            ENDIF
            WW = - WW * W/WX
C                                       If amp+phase modify G directly
            IF (MODE .LE. 0.) THEN
               DO 226 I = 1, NT
                  G(I) = G(I) + WW * GRADA(I)
 226           CONTINUE
            ELSE
c                                       Force change in phase only
               DO 230 I = 1, NT
                  WK = 1. + WW * GRADA(I)/G(I)
                  G(I) = G(I) * WK/ABS(WK)
 230           CONTINUE
            ENDIF
C                                       Convergence test
 270     CONVGD = .TRUE.
         NFAIL = 0
         IFAIL = 0
         XFAIL = 0.0
         DO 280 I = 1,NT
            IF (SWT(I).EQ.0) GO TO 275
            IF (IT.GT.30) GO TO 272
            IF (.NOT.CONVGD) GO TO 275
               X = TOL * (5E-7 + ABS (G(I)))
               CONVGD = ((ABS (REAL (G(I))-REAL (GLAST(I))).LE.X) .AND.
     *            (ABS (AIMAG (G(I))-AIMAG (GLAST(I))).LE.X))
C               IF (ABS (REAL (G(I))-REAL (GLAST(I))).GT.X) CONVGD =
C               .FALSE.
C               IF (ABS (AIMAG (G(I))-AIMAG (GLAST(I))).GT.X) CONVGD =
C               .FALSE.
               GO TO 275
 272        CONTINUE
               XFAIL1 = ABS(G(I)-GLAST(I))
               X = TOL * (5E-7 + ABS(G(I)))
               IF (XFAIL1.LE.X) GO TO 275
                  CONVGD = .FALSE.
                  NFAIL = NFAIL + 1
                  IF (XFAIL1/X.LT.XFAIL) GO TO 275
                     XFAIL = XFAIL1/X
                     IFAIL = I
 275        GLAST(I) = G(I)
 280        CONTINUE
 281        IF (IPR.LT.1) GO TO 300
C                                       Print statistics
         S = 0.
         DO 290 K = 1,NOBS
            IF (WT(K).EQ.0.) GO TO 290
               II = I1(K)
               JJ = I2(K)
               JJ = I2(K)
C                                       stmt., unless it's split up:
              RESID = VOBS(K) - G(II) * CONJG(G(JJ))
               QQ = REAL(RESID*CONJG(RESID))
               S = S + WT(K) * QQ
 290        CONTINUE
         RMS = SQRT (S/SUMWT)
         RMS = SQRT (S/SUMWT)
         CALL MSGWRT (2)
 300     CONTINUE
         IF (CONVGD) GO TO 7000
         IF (IT.LE.30) GO TO 310
            IF (NFAIL.GT.1) GO TO 310
            IF (XFAIL.LE.10.) GO TO 7000
 310  CONTINUE
      GO TO 7002
7000  IER = 0
C                                       Refer solutions to reference
C                                       antenna.
      WRITE (MSGTXT,1090)
      IF (IPR.GE.1) CALL MSGWRT (2)
      DO 600 I = 1,NT
         IF ((I.EQ.NREF) .OR. (SWT(I).EQ.0.)) GO TO 600
            G(I) = G(I) / G(NREF) * ABS (G(NREF))
600      CONTINUE
      DO 601 I = 1,NT
         IF (I.EQ.NREF) G(I) = ABS (G(I))
         IF ((IPR.LT.1) .OR. (SWT(I).EQ.0.)) GO TO 601
C                                       Print results.
            AMP = ABS (G(I))
            PH = 57.2958 * ATAN2 (AIMAG (G(I)), REAL (G(I)))
            WRITE (MSGTXT,1091) I, AMP, PH
            CALL MSGWRT (2)
601      CONTINUE
      GO TO 999
7001  IER = 1
      GO TO 999
7002  IER = 2
      GO TO 999
7003  IER = 3
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Obs#',3X,'Real',7X,'Imag',7X,'ant',4X,'weight')
 1001 FORMAT (I4,2(1PE12.3),2I3,1PE12.3)
 1002 FORMAT (' Iter',3X,'Sum',12X,'RMS')
 1003 FORMAT (I5,2(1PE15.5))
 1090 FORMAT ('  Ant',8X,'G-amp',5X,'G-phase')
 1091 FORMAT (I5,F12.5,F12.2)
      END
      SUBROUTINE SCLMOD (IRET)
C-----------------------------------------------------------------------
C   SCLMOD divides the CLEAN model visibilities into the data.
C   If no model is found then the data is not divided by the model.
C   Inputs: from commons
C     XNCOMP(*) R    Number of components to be divided (1 per field)
C     DISKIN    R    Input file disk number.
C     CNO1      I    Input file catalog number.
C     DISK2     R    CLEAN file disk number.
C     CNO2      I    CLEAN file catalog number.
C     XNMAP     R    Number of model files.
C     POINT     R    If .lt. 0 use no model, if .gt. 0 use point model
C     FLUX      R    Pt. model flux density (Jy)
C     DELTAX    R    Pt. model RA(x) offset
C     DELTAY    R    Pt. model Dec(y) offset
C   Output:
C     IRET      I    Return code, 0 => ok, otherwise not.
C-----------------------------------------------------------------------
      INTEGER   IRET, IDISK, MODEL, METHOD, ISEQ2, IDISK2, VER,
     *   ISTOKE, DISKO, ISCR, CHAN, NCHAN, UVBLK(256), KBUFSZ, I,
     *   IROUND, IERR
      LOGICAL   DOMSG, T, F
      REAL      BUFF1(4096), BUFF2(4096)
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NITER(MAXFLD), BITER(MAXFLD)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'RDCAL.INC'
C
      REAL RBUF2(MAXIF)
      DATA DOMSG, T, F /.TRUE.,.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Mark input file a file to use
C                                       in case no model is found.
      IDISK = IROUND (DISK1)
      MODVOL = IDISK
      CALL ZPHFIL ('UV', IDISK, CNO1, 1, MODFIL, IRET)
      IRET = 0
C                                       See if division by model needed
      IF (POINT.LT.-0.01) GO TO 999
C                                       Set model and method
C                                       (Options limited by sort order.)
      MODEL = 1
      METHOD = -1
C                                       Check for point source
      DOPTMD = POINT .GT. 0.0
      PTFLX = FLUX
      PTRAOF = DELTAX
      PTDCOF = DELTAY
C                                       Point model only
      PARMOD(1) = 0.0
      FACGRD = 1.0
C                                       Get info on model file(s)
      IDISK2 = IROUND (DISK2)
      ISEQ2 = IROUND (SEQ2)
      VER = IROUND (XVER)
      MFIELD = IROUND (XNMAP)
      IF (MFIELD.LE.0) MFIELD = 1
      NONEG = F
      DO 10 I = 1,MFIELD
         BITER(I) = 1
         NITER(I) = ABS (XNCOMP(I)) + 0.1
         NONEG = (XNCOMP(I).LE.-1.0) .OR. NONEG
 10      CONTINUE
      IF (.NOT.DOPTMD) CALL SETGDS (NAME2, CLASS2, ISEQ2, IDISK2,
     *   MFIELD, VER, NITER, BITER, MODEL, METHOD, BUFF1, BUFF2,
     *   ISTOKE, IRET)
      IF (IRET.NE.0) GO TO 999
      DISK2 = IDISK2
      SEQ2 = ISEQ2
      XVER = VER
C                                       Divide data by model
      DISKO = 0
      ISCR = 0
      CHAN = 1
      NCHAN = 1
      KBUFSZ = 4096 * 2
      CALL COPY (256, CATBLK, UVBLK)
      NGRDAT = F
      CALL UVMDIV (IDISK, CNO1, DISKO, ISCR, MODEL,
     *   METHOD, DOMSG, CHAN, NCHAN, UVBLK,
     *   KBUFSZ, FRQSEL, BUFF1, BUFF2, RBUF, RBUF2, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get true  values of NCOMP
      DO 20 I = 1,MFIELD
         XNCOMP(I) = NSUBG(I) - 1
 20      CONTINUE
C                                       Model divided by data now
C                                       in scratch file.
      MODVOL = SCRVOL(ISCR)
      CALL ZPHFIL ('SC', MODVOL, SCRCNO(ISCR), 1, MODFIL, IERR)
C
 999  RETURN
      END
      SUBROUTINE ASCTEL (IPR, IRET)
C-----------------------------------------------------------------------
C   ASCTEL checks the TELL file to see if anything is waiting for RDCAL.
C   If so, it picks up the parms (via GTTELL), interprets them, and
C   sends them back to the rest of the task to handle.
C   Output:  IPR     I     Print level (0,1,2)
C            IRET    I     0 => okay, 99 => abort now
C-----------------------------------------------------------------------
      CHARACTER OPTELL*4
      INTEGER   IPR, IRET
      INTEGER   NPARMS, SCRTCH(256), IERR, IROUND
      REAL      XPRLEV
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'RDCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /ASCTLL/ XPRLEV
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      IRET = 0
      NPARMS = 1
      CALL GTTELL (NPARMS, OPTELL, XPRLEV, SCRTCH, IERR)
      WRITE (MSGTXT,1000) OPTELL
      IF (IERR.EQ.2) WRITE (MSGTXT,1001) OPTELL
      IF (IERR.GE.1) CALL MSGWRT (4)
      IF (IERR.EQ.3) IRET = 99
      IF (IERR.NE.1) GO TO 999
C                                       install parms
      IF (XPRLEV.GT.-1.0E-5) XPR = XPRLEV
      IPR = IROUND (XPR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Received ',A4,' operation from TELL')
 1001 FORMAT ('Ignored ',A4,' operation from TELL  **********')
      END
      SUBROUTINE OSORTI (A, N, NP2, KEY1, KEY2, LEN, WK, IP, IER)
C-----------------------------------------------------------------------
C! does quick sort on array of vectors, then reorders by calling PERMAT
C# Sort UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   OSORT does a quick sort on the keys and a permutation vector and
C   then reorders the data with PERMAT.  Data sorted into descending
C   order of the keys.
C   Input:
C      A     R(LEN,N)   Array of data to be sorted.
C      N     I         Number of records to be sorted
C      NP2   I         Number of words in work arrays = N+2
C      KEY1  I         Word in record of slower varying key
C      KEY2  I         Word in record of faster varying key
C      LEN   I         Length of record in R words (must be .LE. 2048)
C   Output:
C      WK    R(2,NP2)  Work buffer for keys.
C      IP    I(NP2)    Work buffer for permutation vector.
C      A     R(LEN,N)  Array of data sorted.
C      IER   I         Error code,   0 => OK
C                                    1 => input error (LEN > 2048)
C   Adapted from ACM alg. #347 - June 1981
C-----------------------------------------------------------------------
      INTEGER   N, LEN, NP2, IP(NP2), KEY1, KEY2, IER
      REAL      A(LEN,N), WK(2,NP2)
C
      INTEGER   TP, TTP, IU(20), IL(20), I, IJ, J, K, KK, L, M
      REAL      T(2), TT(2)
C-----------------------------------------------------------------------
C                                       Check length
      IER = 1
      IF (LEN.GT.2048) GO TO 999
      IER = 0
C                                       Build array of keys to sort
C                                       Minus sign makes order descend.
C                                       Note "blockers" put at ends.
      DO 5 KK = 1,N
         I = KK + 1
         IP(I) = KK
         WK(1,I) = -A(KEY1,KK)
         WK(2,I) = -A(KEY2,KK)
 5       CONTINUE
      WK(1,1) = -1.0E20
      WK(1,N+2) = 1.E20
      M = 1
      I = 2
      J = N + 1
C                                       Sort subgroup
 10   CONTINUE
         IF (I.GE.J) GO TO 240
C
 20      CONTINUE
            K = I
            IJ = (J+I)/2
            T(1) = WK(1,IJ)
            T(2) = WK(2,IJ)
            TP = IP(IJ)
            IF (WK(1,I)-T(1)) 70,40,50
 40            IF (WK(2,I).LE.T(2)) GO TO 70
 50               DO 60 KK = 1,2
                     WK(KK,IJ) = WK(KK,I)
                     WK(KK,I) = T(KK)
                     T(KK) = WK(KK,IJ)
 60                  CONTINUE
                  IP(IJ) = IP(I)
                  IP(I) = TP
                  TP = IP(IJ)
C
 70         L = J
            IF (WK(1,J)-T(1)) 90,80,160
 80            IF (WK(2,J).GE.T(2)) GO TO 160
 90               DO 100 KK = 1,2
                     WK(KK,IJ) = WK(KK,J)
                     WK(KK,J) = T(KK)
                     T(KK) = WK(KK,IJ)
 100                 CONTINUE
                  IP(IJ) = IP(J)
                  IP(J) = TP
                  TP = IP(IJ)
                  IF (WK(1,I)-T(1)) 160,110,120
 110                 IF (WK(2,I).LE.T(2)) GO TO 160
 120                    DO 130 KK = 1,2
                           WK(KK,IJ) = WK(KK,I)
                           WK(KK,I) = T(KK)
                           T(KK) = WK(KK,IJ)
 130                       CONTINUE
                        IP(IJ) = IP(I)
                        IP(I) = TP
                        TP = IP(IJ)
                        GO TO 160
C
 140                 CONTINUE
                        DO 150 KK = 1,2
                           WK(KK,L) = WK(KK,K)
                           WK(KK,K) = TT(KK)
 150                       CONTINUE
                        IP(L) = IP(K)
                        IP(K) = TTP
C                                       Put substring on stack
 160        L = L - 1
               IF (L.GT.0) GO TO 165
                  IER = 3
                  GO TO 999
 165           IF (WK(1,L)-T(1)) 180,170,160
 170           IF (WK(2,L).GT.T(2)) GO TO 160
 180              TT(1) = WK(1,L)
                  TT(2) = WK(2,L)
                  TTP = IP(L)
 200              K = K + 1
                     IF (WK(1,K)-T(1)) 200,210,220
 210                 IF (WK(2,K).LT.T(2)) GO TO 200
 220                      IF (K.LE.L) GO TO 140
                              IF (L-I.LE.J-K) GO TO 230
                                 IL(M) = I
                                 IU(M) = L
                                 I = K
                                 M = M + 1
                                 GO TO 250
 230                          CONTINUE
                                 IL(M) = K
                                 IU(M) = J
                                 J = L
                                 M = M + 1
                                 GO TO 250
C                                       Next substring from stack
 240        CONTINUE
               M = M - 1
               IF (M.EQ.0) GO TO 900
               I = IL(M)
               J = IU(M)
C                                       Straight insertion sort done
C                                       if <= 10 elements in group
 250        IF (J-I.GE.11) GO TO 20
         IF (I.EQ.2) GO TO 10
      I = I-1
 260  I = I+1
         IF (I.EQ.J) GO TO 240
            T(1) = WK(1,I+1)
            T(2) = WK(2,I+1)
            TP = IP(I+1)
            IF (WK(1,I)-T(1)) 260,280,290
 280           IF (WK(2,I).LE.T(2)) GO TO 260
 290              K = I
C
 300              CONTINUE
                     WK(1,K+1) = WK(1,K)
                     WK(2,K+1) = WK(2,K)
                     IP(K+1) = IP(K)
                     K = K-1
                     IF (K.GT.0) GO TO 310
                        IER = 4
                        GO TO 999
 310                 IF (T(1)-WK(1,K)) 300,320,330
 320                    IF (T(2).LE.WK(2,K)) GO TO 300
 330                       WK(1,K+1) = T(1)
                           WK(2,K+1) = T(2)
                           IP(K+1) = TP
                           GO TO 260
C                                       Permute full matrix finally
 900  CONTINUE
C
 999  RETURN
      END
