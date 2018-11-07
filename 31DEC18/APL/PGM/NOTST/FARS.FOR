LOCAL INCLUDE 'FARS.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   RMTSIZ
      PARAMETER (RMTSIZ = MAXIMG)
C
      INTEGER   HSIZE, FSIZE, HSIZER, FSIZER, NITER, KMN, KMX,
     *   DOPRT
      REAL      FLUX, GAIN, CELL, RERMT(RMTSIZ), IMRMT(RMTSIZ),
     *   GAUSS(RMTSIZ)
      INTEGER   CATI1(256), CATI2(256), CATB(256,2), CATIO(256,2)
      REAL      CATR1(256), CATR2(256)
      HOLLERITH CATH1(256), CATH2(256)
      DOUBLE PRECISION CATD1(128), CATD2(128)
      REAL      IROW(MABFSS,4), FREQ1, FREQ2, LNFREQ, FMAX1,
     *   FMAX2, CPARM(10), XN(2), XX(2), BMAX
      CHARACTER NAMIN(2)*12, CLSIN(2)*6, NAMOUT(2)*12, CLSOUT(2)*6,
     *   INFILE*48
      LOGICAL   BFLAG, CLFOUR, UNCLFO, DOCONV, DOGAUS, DORES, CLPLRE,
     *   CLONLY
      INTEGER   IBLC(7,2), ITRC(7,2), NPI(7,2), NPO(7), FBL(2), ILUN(2),
     *   IIND(2), OLUN(2), OIND(2), ISSEQ(2), IDSEQ(2), NAX, NRDIM(2),
     *   IPOINT, FCVOL(4), FCCNO(4), HISTDO
C
      INTEGER   NL2AX, L2SIZE
      PARAMETER (L2SIZE = 16384)
      REAL      ARRL2(L2SIZE), W(L2SIZE), WSUM, L2MEAN
      COMMON /FARSYN/ NL2AX, ARRL2, W, RERMT, IMRMT, WSUM, L2MEAN,
     *   GAUSS
C
      COMMON /FARSCM/ CATB, CATIO, IROW, FREQ1, FREQ2, LNFREQ,
     *   FMAX1, FMAX2, CPARM, HSIZE, FSIZE, HSIZER, FSIZER,
     *   CELL, FLUX, GAIN, CLFOUR, UNCLFO, DOCONV, DOGAUS, DORES,
     *   CLPLRE, CLONLY, NITER, XN, XX, BFLAG, FBL, IBLC, ITRC, NPI,
     *   NPO, ILUN, IIND, OLUN, OIND, ISSEQ, IDSEQ, NAX, NRDIM, IPOINT,
     *   FCVOL, FCCNO, HISTDO, BMAX, KMN, KMX, DOPRT
      COMMON /FARSCH/ NAMIN, CLSIN, NAMOUT, CLSOUT, INFILE
      EQUIVALENCE (CATI1, CATR1, CATD1, CATH1, CATB(1,1))
      EQUIVALENCE (CATI2, CATR2, CATD2, CATH2, CATB(1,2))
      INCLUDE 'INCS:DCAT.INC'
LOCAL END
      PROGRAM FARS
C-----------------------------------------------------------------------
C! carry out Faraday rotation measure synthesis.
C# Map-util SPECTRAL POLARIZATION ANALYSIS
C-----------------------------------------------------------------------
C;  Copyright (C) 2009-2015, 2017
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
C   FARS carries out Faraday rotation measure synthesis.
C   Inputs: (from AIPS)
C   USERID......The ID of the owner of the images. 0 => current
C               user, 32000 => any user.
C   INNAME......First image name.     Standard defaults.
C   INCLASS.....First image class.    Standard defaults.
C   INSEQ.......First image seq. #.   0 => highest.
C   INDISK......Disk drive # for the first image.  0 => any.
C   IN2NAME.....Second image name.    Standard defaults.
C   IN2CLASS....Second image class.   Standard defaults.
C   IN2SEQ......Second image seq. #.  0 => highest.
C   IN2DISK.....Disk drive # for the second image.  0 => any.
C   OUTNAME.....Output image name.     Standard defaults.
C   OUTSEQ......Output image seq. #.   0 => highest.
C   OUTDISK.....Output disk. 0 => highest with space.
C   DOALIGN.....Controls how the four images are to be aligned (see HELP
C               DOALIGN).  True (>.1) means that the images must agree
C               in their coordinates, though not necessarily in the
C               reference pixel position.  Alignment is by coordinate
C               values (if DOALIGN > -0.1) or by offsets from the
C               reference pixel positions (if DOALIGN <= -0.1).  NOTE:
C               all real axes (>1 point) are aligned.  If DOALIGN = -2,
C               the headers are ignored and the images are aligned at
C               pixel (1,1,...).
C   OUTNAME.....Output image name.    Standard defaults.
C   OUTCLASS....Output image class.   Standard behavior with default.
C               The noise image has the 6th character of class set to N.
C   OUTSEQ......Output image seq. #.  0 => highest unique.
C   OUTDISK.....Output disk number. 0 => highest with space.
C   BLC.........Bottom left corner of the 1st input image. The other
C               images are aligned by coordinates (see DOALIGN) on all
C               axes having > 1 point.  The other images may have fewer
C               real axes than the 1st.  The 4 windows must have the
C               same dimension on the first 2 axes, but the task will
C               select a smaller window than was specified if needed to
C               overlap the 4 images.
C   TRC.........Top right corner of input images. (See BLC.)
C   OPCODE......is not used yet
C   APARM.......Parameters needed for algorithm:
C     APARM(1)  Number of pixels at half of Fourier transform output
C     APARM(2)  cell size in 1/m^2
C     APARM(3)  >0 RMTF output
C     APARM(4)  0=> CLEANed Fourier transform
C               1=> unCLEANed Fourier  transform
C               2=> 3 parameters of the only maximum at the the Fourier
C                   transform
C     APARM(5)  0=> original(shifted back) RE/IM are sent out
C               1=> the shifted RE/IM are sent out
C               2=> amplitudes of the data are sent out
C     APARM(6)  is not used
C     APARM(7)  0=> convolve the clean components
C               1=> no convolve
C     APARM(8)  0=> use the Gaussian as the convolve function
C               1=> use the Re of RMTF as the convolve function
C     APARM(9)  full width of Gaussian convolve function, at 0.5
C               level, in 1/m^2,  0 => 1
C     APARM(10) send residual to the output?
C                   1 => yes
C                   0 => regular output
C   DOHIST.....-2 => copy 1st HI only
C              -3 => no copy of HI
C- uses the COMB.FOR as initial codes !---------------------------------
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   IRET, IERR, IWIN(4), ISWIN(4), NBY, NX, NY, IDEPTH(5),
     *   IBLKOF, NBYBUF, ISPOS(2), IDPOS(2), I3, I4, I5, I6, I7, I, IY,
     *   MI
      INCLUDE 'FARS.INC'
      REAL      XBUFF(MABFSS,2)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGNAM /'FARS  '/
      DATA ISWIN /0,0,0,0/
C-----------------------------------------------------------------------
C                                        Initialize input maps
      IRET = 16
      CALL FARSIN (PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL FILL (2, 1, ISPOS)
      CALL FILL (2, 1, IDPOS)
C                                       Do-loop limits: window
      NBY = 2
      NBYBUF = MABFSS * NBY
C                                       make a correct value
C                                       corresponded to the actual
C                                       first axis pixels
      NPO(1) = CATBLK(KINAX)
      NX = NPO(1)
      NY = NPO(2)
      MI = (1024 * 1024) / (NX * NY)
      MI = MAX (1, MI)
      DO 70 I7 = 1,NPO(7)
      DO 70 I6 = 1,NPO(6)
      DO 70 I5 = 1,NPO(5)
      DO 70 I4 = 1,NPO(4)
      DO 70 I3 = 1,NPO(3)
         WRITE (MSGTXT,1000) I3
         IF (MOD(I3-1,MI).EQ.0) CALL MSGWRT (2)
C                                        Initialize input maps
         DO 10 I = 1,2
            CALL FILL (5, 1, IDEPTH(1))
            IF (NRDIM(I).GE.3) IDEPTH(1) = I3 - 1 + IBLC(3,I)
            IF (NRDIM(I).GE.4) IDEPTH(2) = I4 - 1 + IBLC(4,I)
            IF (NRDIM(I).GE.5) IDEPTH(3) = I5 - 1 + IBLC(5,I)
            IF (NRDIM(I).GE.6) IDEPTH(4) = I6 - 1 + IBLC(6,I)
            IF (NRDIM(I).GE.7) IDEPTH(5) = I7 - 1 + IBLC(7,I)
            CALL COMOFF (NAX, NPI(1,I), IDEPTH, IBLKOF, IERR)
            IF (IERR.NE.0) GO TO 995
            IBLKOF = IBLKOF + 1
            IWIN(1) = IBLC(1,I)
            IWIN(2) = IBLC(2,I)
            IWIN(3) = ITRC(1,I)
            IWIN(4) = ITRC(2,I)
            CALL MINIT ('READ', ILUN(I), IIND(I), NPI(1,I), NPI(2,I),
     *         IWIN, IROW(1,I), NBYBUF, IBLKOF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) I, IERR
               GO TO 980
               END IF
 10         CONTINUE
C                                        Initialize output file
         IDEPTH(1) = I3
         IDEPTH(2) = I4
         IDEPTH(3) = I5
         IDEPTH(4) = I6
         IDEPTH(5) = I7
         CALL COMOFF (NAX, NPO, IDEPTH, IBLKOF, IERR)
         IF (IERR.NE.0) GO TO 995
         IBLKOF = IBLKOF + 1
         ISWIN(1) = 1
         ISWIN(2) = 1
         ISWIN(3) = NPO(1)
         ISWIN(4) = NPO(2)
C
         DO 20 I = 1,2
            CALL MINIT ('WRIT', OLUN(I), OIND(I), NX, NY, ISWIN,
     *         XBUFF(1,I), NBYBUF, IBLKOF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1020) I, IERR
               GO TO 980
               END IF
 20         CONTINUE
C                                        Loop over rows
         DO 50 IY = 1,NY
            DO 30 I = 1,2
               CALL MDISK ('READ', ILUN(I), IIND(I), IROW(1,I),
     *            ISPOS(I), IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1030) IY, I, IERR
                  GO TO 980
                  END IF
 30            CONTINUE
C
            DO 40 I = 1,2
               CALL MDISK ('WRIT', OLUN(I), OIND(I), XBUFF(1,I),
     *            IDPOS(I), IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1040) IY, I, IERR
                  GO TO 980
                  END IF
 40            CONTINUE
C
            CALL FARSCL (IROW(ISPOS(1),1), IROW(ISPOS(2),2), CPARM,
     *         XBUFF(IDPOS(1),1), XBUFF(IDPOS(2),2))
 50         CONTINUE
C                                        Write the last buffer
         DO 60 I = 1,2
            CALL MDISK ('FINI', OLUN(I), OIND(I), XBUFF(1,I),
     *         IDPOS(I), IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1060) I, IERR
               GO TO 980
               END IF
 60         CONTINUE
 70      CONTINUE
C                                        Create and write HI file
      CALL FARSHI (IRET)
      GO TO 995
C                                        Error return
 980  CALL MSGWRT (7)
C
 995  CALL DIE (IRET, IROW)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('PROCESSING ROW',I7)
 1010 FORMAT ('COULD NOT INITIALIZE INPUT FILE #',I2,'  IER=',I7)
 1020 FORMAT ('COULD NOT INITIALIZE OUTPUT FILE 3',I2,'  IER=',I7)
 1030 FORMAT ('COULD NOT READ LINE',I4,'  MAP #',I2,'  IER=',I7)
 1040 FORMAT ('COULD NOT WRITE LINE',I4,'  MAP #',I2,'  IER=',I7)
 1060 FORMAT ('COULD NOT WRITE LAST LINE, MAP #',I2,'.  IER=',I7)
      END
      SUBROUTINE FARSIN (PRGNAM, IERR)
C-----------------------------------------------------------------------
C   FARSIN gets the inputs for FARS, opens and checks the input images,
C   creates the output image(s), and prepares parameters in common for
C   the later stages of FARS.
C   Inputs:
C      PRGNAM   C*6   Program name
C   Outputs into FARS.INC:
C      NL2AX    I       Number of points at the array ARRL2
C      ARRL2    R(*)    Array of lambda squares
C      W        R(*)    Array of weghts
C      L2MEAN   R       Mean value of lambda square
C   Outputs:
C      IER      I     Error return: 0-->  Okay
C                        3-->  Cannot create and open output file
C                        2-->  Cannot open either input map
C                        1-->  Error in getting input parameters
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   IERR
C
      CHARACTER IDCDEF*6, PHNAME*48, MTYPE*2, STOKES*8, CHTMP*8,
     *   CHTMP1*8
      INTEGER   I, IRETCD, INPRMS, NB, INC, J, IROUND, K
      LOGICAL   REDUCE, T
      REAL      EPS, X, AXV
      REAL      RERMTF, IMRMTF, FI, ARG, HWIDTH
      REAL      XSEQ1, XDSK1, XSEQ2, XDSK2, SEQOUT, DISOUT, APARM(10),
     *   BLC(7), TRC(7), GRIDCR, DOHIST, GAINN, NNITER, FLUXX, L2MIN,
     *   L2MAX, PRTLEV
      HOLLERITH XNAM1(3), XCLS1(2), XNAM2(3), XCLS2(2), XNAMOU(3),
     *   XINFIL(12)
      DOUBLE PRECISION    DAXV
C
      DOUBLE PRECISION FREQV, IQFREQ
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'FARS.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   BUFFER(1024), FQKOLS(MAXFQC), FQNUMV(MAXFQC), IFQRNO,
     *   IFQ, FQID, IFSIDE, FQVER, NUMIF, NUMREC, FREQAX, FQAX
      REAL      IFCHW, IFTBW, A, B
      DOUBLE PRECISION IFFREQ
      CHARACTER BNDCOD*8
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      COMMON /INPARM/ XNAM1, XCLS1, XSEQ1, XDSK1, XNAM2, XCLS2,
     *   XSEQ2, XDSK2, XNAMOU, SEQOUT, DISOUT, XINFIL, GRIDCR, BLC, TRC,
     *   APARM, GAINN, NNITER, FLUXX, DOHIST, PRTLEV
      DATA EPS /0.2/
      DATA T /.TRUE./
      DATA STOKES /'STOKES  '/
C-----------------------------------------------------------------------
C                                        Initialize file and header I/O
      ILUN(1) = 17
      ILUN(2) = 18
      OLUN(1) = 21
      OLUN(2) = 22
C
      CALL ZDCHIN (T)
      CALL VHDRIN
      NSCR = 0
      NCFILE = 0
C                                        Get inputs from AIPS
      INPRMS = 61
      CALL GTPARM (PRGNAM, INPRMS, RQUICK, XNAM1, IROW, IERR)
      IRETCD = IERR
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (7)
         END IF
      IF (RQUICK) CALL RELPOP (IRETCD, IROW, I)
      IF (IRETCD.NE.0) GO TO 999
C                                       Change input OPCODE - char
      HISTDO = IROUND (DOHIST)
      DOPRT = IROUND (PRTLEV)
C                                       size of the Fourier transform
      HSIZE = APARM(1)
      FSIZE = 2*HSIZE + 1
C                                       Number of iteration at
C                                       components subtraction
      NITER = NNITER
      IF (NITER.EQ.0) NITER = 1
      FLUX = FLUXX
      GAIN = GAINN
      IF (GAIN.EQ.0) GAIN = 0.1
C                                       different options of the outputs
C                                       APARM(4)
      CLFOUR = APARM(4).LE.0.0
      UNCLFO = APARM(4).GT.0.0
C                                       send CLEAN + RESIDUAL to
C                                       output: APARM(10)=0
      CLPLRE = IROUND(APARM(10)).LE.0
C                                       send CLEAN ONLY to
C                                       output: APARM(10)=1
      CLONLY = IROUND(APARM(10)).EQ.1
C                                       send RESIDUAL of clean to
C                                       output: APARM(10)=2
      DORES = IROUND(APARM(10)).GE.2
C                                       Send convolved clean components
C                                       to the output images or just
C                                       the components themself
      DOCONV = APARM(7).LT.0.5
C                                       If NOCLEAN (just FOURIER) or
C                                       use the 3 parameters, then
C                                       no CONVOLUTION
      IF (APARM(4).GT.0.0) DOCONV = .FALSE.
C                                       Use the Gaussian convolve
C                                       function or the RE of RMTF
      DOGAUS = APARM(8).LT.0.5
C                                        Transfer inputs into integer
C                                        variables
      FCVOL(1) = IROUND (XDSK1)
      FCVOL(2) = IROUND (XDSK2)
      FCVOL(3) = IROUND (DISOUT)
      FCVOL(4) = IROUND (DISOUT)
      ISSEQ(1) = IROUND (XSEQ1)
      ISSEQ(2) = IROUND (XSEQ2)
      IDSEQ(1) = IROUND (SEQOUT)
      IDSEQ(2) = IROUND (SEQOUT)
C                                       Hollerith -> Char.
      CALL H2CHR (12, 1, XNAM1, NAMIN(1))
      CALL H2CHR (6, 1, XCLS1, CLSIN(1))
      CALL H2CHR (12, 1, XNAM2, NAMIN(2))
      CALL H2CHR (6, 1, XCLS2, CLSIN(2))
C
      CALL H2CHR (12, 1, XNAMOU, NAMOUT(1))
      CLSOUT(1) = ' '
      CALL H2CHR (12, 1, XNAMOU, NAMOUT(2))
      CLSOUT(2) = ' '
C                                       infile
      CALL H2CHR (48, 1, XINFIL, INFILE)
C                                        Open both input maps
      MTYPE = 'MA'
C
      DO 15 I = 1,2
         CALL MAPOPN ('READ', FCVOL(I), NAMIN(I), CLSIN(I), ISSEQ(I),
     *      MTYPE, NLUSER, ILUN(I), IIND(I), FCCNO(I), CATBLK,
     *      IROW(1,I), IERR)
         CALL COPY (256, CATBLK, CATB(1,I))
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1025) I,IERR
            GO TO 980
            END IF
         NCFILE = NCFILE + 1
         FRW(NCFILE) = 0
         FVOL(NCFILE) = FCVOL(I)
         FCNO(NCFILE) = FCCNO(I)
         NB = CATBLK(KIDIM)
         CALL FILL (KICTPN, 1, NPI(1,I))
         CALL COPY (NB, CATBLK(KINAX), NPI(1,I))
         NRDIM(I) = NB
         DO 10 J = 1,NB
            NPI(J,I) = MAX (1, NPI(J,I))
            IF (NPI(J,I).LE.1) NRDIM(I) = NRDIM(I) - 1
 10         CONTINUE
 15      CONTINUE
C                                        Set up some needed header vals
      REDUCE = .FALSE.
      NAX = CATI1(KIDIM)
      CALL WINDOW (NAX, NPI(1,1), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 20 I = 1,7
         IBLC(I,1) = BLC(I) + EPS
         ITRC(I,1) = TRC(I) + EPS
         NPO(I) = ITRC(I,1) - IBLC(I,1) + 1
 20      CONTINUE
C                                       Is the first axis 'freq'
      CALL H2CHR (8, 1, CATH(KHCTP), CHTMP)
      IF ((CHTMP(:4).NE.'FREQ') .AND. (CHTMP(:4).NE.'FQID')) THEN
         IERR = 5
         MSGTXT = 'The first axis is not FREQ. Run the task TRANS'
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Are input maps coincident?
C                                       Set 2nd map corners
      INC = 2
      K = 2
      CALL COPY (256, CATB(1,K), CATBLK)
      DO 25 I = 1,7
         J = I - 1
C                                       null axis
         IBLC(I,K) = 1
         ITRC(I,K) = 1
         IF (NPI(I,K).GT.1) THEN
            AXV = CATR(KRCRP+J) - CATR1(KRCRP+J) + IBLC(I,1)
            IF (GRIDCR.LT.-1.5) AXV = IBLC(I,1)
            IF (GRIDCR.GT.-0.1) THEN
               DAXV = CATD1(KDCRV+J) + (IBLC(I,1) -
     *            CATR1(KRCRP+J)) * CATR1(KRCIC+J)
               IF (CATR(KRCIC+J).EQ.0.0) GO TO 30
               AXV = (DAXV - CATD(KDCRV+J)) / CATR(KRCIC+J) +
     *            CATR(KRCRP+J)
               END IF
            IBLC(I,K) = IROUND (AXV)
            IF ((GRIDCR.GE.0.1) .AND. (ABS(AXV-IBLC(I,K)).GT.EPS))
     *         GO TO 30
            ITRC(I,K) = IBLC(I,K) + ITRC(I,1) - IBLC(I,1)
C                                       smaller subimage required?
            IF (IBLC(I,K).LT.1) THEN
               IBLC(I,1) = IBLC(I,1) + 1 - IBLC(I,K)
               BLC(I) = IBLC(I,1)
               REDUCE = .TRUE.
               IBLC(I,K) = 1
               END IF
            IF (ITRC(I,K).GT.NPI(I,K)) THEN
               ITRC(I,1) = ITRC(I,1) + NPI(I,K) - ITRC(I,K)
               TRC(I) = ITRC(I,1)
               REDUCE = .TRUE.
               ITRC(I,K) = NPI(I,K)
               END IF
            IF (IBLC(I,1).GT.ITRC(I,1)) GO TO 30
            NPO(I) = ITRC(I,1) - IBLC(I,1) + 1
C                                        Check coincidence
            IF (GRIDCR.GE.0.1) THEN
               IPOINT = KHCTP+J*INC
               CALL H2CHR (8, 1, CATH(IPOINT), CHTMP)
               CALL H2CHR (8, 1, CATH1(IPOINT), CHTMP1)
               IF (CHTMP.NE.CHTMP1) GO TO 30
               X = EPS * EPS * ABS(CATR1(KRCIC+J))
               IF (ABS(CATR(KRCIC+J)-CATR1(KRCIC+J)).GT.X)
     *            GO TO 30
               IF (ABS(CATR(KRCRT+J)-CATR1(KRCRT+J)).GT.1.)
     *            GO TO 30
               END IF
            END IF
 25      CONTINUE
      GO TO 40
C                                        Maps not coincident
 30   WRITE (MSGTXT,1075) I
      GO TO 980
C                                       Create output map
C                                       Get stokes value of input maps
C                                       Insert proper Stokes value
C                                       Create set of selected lambda^2
C                                       using the FQ table
 40   I = 0
      L2MEAN = 0
      WSUM = 0
      L2MIN = 1.0E10
      L2MAX = 1.0E-10
      FQVER = 1
      CALL AXEFND (4, 'FQID', CATBLK(KIDIM), CATH(KHCTP), FQAX,
     *   IERR)
      IF (IERR.NE.0) FQVER = -1
      CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATH(KHCTP), FREQAX,
     *   IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'NO FREQUENCY AXIS FOUND: QUITTING'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       open FQ table
      IF (FQVER.GT.0) THEN
         CALL FQINI ('READ', BUFFER, FCVOL(1), FCCNO(1), FQVER, CATBLK,
     *      ILUN(1), IFQRNO, FQKOLS, FQNUMV, NUMIF, IERR)
C                                       Get number of records
         NUMREC = BUFFER(5)
         IF (NUMREC.LE.0) GO TO 999
         NL2AX = NPO(1)
C                                       get the array of weights
         IF (INFILE.EQ.' ') THEN
            CALL RFILL (NL2AX, 1.0, W)
         ELSE
            CALL GETW (INFILE, NL2AX, W, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
C                                       read FQ table
         DO 45 IFQRNO = 1,NUMREC
            IFQ = IFQRNO
            CALL TABFQ ('READ', BUFFER, IFQ, FQKOLS, FQNUMV, NUMIF,
     *         FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IERR)
            IF (IERR.NE.0) GO TO 900
            IQFREQ = (FQID - CATD(KDCRV+FQAX)) / CATR(KRCIC+FQAX) +
     *         CATR(KRCRP+FQAX)
            I = IQFREQ + 1.1D0 - IBLC(1,1)
            IF ((I.GE.1) .AND. (I.LE.NL2AX)) THEN
               WSUM = WSUM + W(I)
               FREQV = IFFREQ + CATD1(KDCRV+FREQAX)
               ARRL2(I) = (299792458.D0/FREQV)**2
               IF (ARRL2(I).GT.L2MAX) L2MAX = ARRL2(I)
               IF (ARRL2(I).LT.L2MIN) L2MIN = ARRL2(I)
               L2MEAN = L2MEAN + W(I)*ARRL2(I)
               END IF
 45         CONTINUE
C                                      Close table.
         CALL TABIO ('CLOS', 0, IFQRNO, BUFFER, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       no FQ table
      ELSE
         IERR = 0
         NL2AX = NPO(1)
C                                       get the array of weights
         IF (INFILE.EQ.' ') THEN
            CALL RFILL (NL2AX, 1.0, W)
         ELSE
            CALL GETW (INFILE, NL2AX, W, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         DO 50 I = 1, NL2AX
            WSUM = WSUM + W(I)
C            FREQV = CATD1(KDCRV) + (I-CATR1(KRCRP))*CATR1(KRCIC)
            FREQV = CATD1(KDCRV) + (I-CATR1(KRCRP)+IBLC(1,1)-1)*
     *         CATR1(KRCIC)
            ARRL2(I) = (299792458.D0/FREQV)**2
            IF (ARRL2(I).GT.L2MAX) L2MAX = ARRL2(I)
            IF (ARRL2(I).LT.L2MIN) L2MIN = ARRL2(I)
            L2MEAN = L2MEAN + W(I)*ARRL2(I)
 50         CONTINUE
         END IF
C                                       cell size at the output
      CELL = APARM(2)
      IF (CELL.LT.0.01) CELL = PI/(4*(L2MAX-L2MIN))
      APARM(2) = CELL
      CALL RCOPY (10, APARM, CPARM)
C
      L2MEAN = L2MEAN / WSUM
C                                       Printout the value of L2MEAN
      WRITE (MSGTXT,1050) L2MEAN
      CALL MSGWRT (4)
C                                       Subtract the mean L^2
      DO 55 I = 1, NL2AX
         ARRL2(I) = ARRL2(I) - L2MEAN
 55      CONTINUE
C                                       calculate RMTF with double
C                                       points for the component
C                                       located at the edge
      HSIZER = 2*HSIZE
      FSIZER = 2*HSIZER + 1
      CALL RFILL (FSIZER, 0.0, GAUSS)
      DO 65 K = 1,FSIZER
         RERMTF = 0
         IMRMTF = 0
         FI = 2.0 * (K-HSIZER-1) * CELL
         DO 60 I = 1,NL2AX
            ARG = FI * ARRL2(I)
C                                       DFT with exp-(ARG)
C                                       calculate RMTF
            RERMTF = RERMTF + W(I) * COS(ARG)
            IMRMTF = IMRMTF - W(I) * SIN(ARG)
 60         CONTINUE
         RERMT(K) = RERMTF / WSUM
         IMRMT(K) = IMRMTF / WSUM
 65      CONTINUE
C                                       normalize beam
C                                       should be normailzed already
      BMAX = RERMT(HSIZER+1)
      DO 66 K = 1,FSIZER
         RERMT(K) = RERMT(K) / BMAX
         IMRMT(K) = IMRMT(K) / BMAX
 66      CONTINUE
C                                       Gaussian convolution
      IF (DOGAUS) THEN
C                                       Half width of the Gaussian
         HWIDTH = APARM(9) / 2.0
C                                       Find fullwidth
         IF (HWIDTH.LT.0.2D0) THEN
            B = 1.0
            K = HSIZER + 1
            DO 70 I = 1,HSIZER
               A = (RERMT(K-I) + RERMT(K+I)) / 2.0
               IF (A.GT.0.5D0) THEN
                  B = A
               ELSE
                  HWIDTH = (I - (0.5D0-A) / (B-A)) * CELL
                  GO TO 71
                  END IF
 70            CONTINUE
            HWIDTH = 1
            END IF
 71      APARM(9) = 2.0 * HWIDTH
         WRITE (MSGTXT,1071) APARM(9)
         CALL MSGWRT (4)
         CPARM(9) = APARM(9)
         KMN = FSIZER
         KMX = 1
         DO 75 K = 1,FSIZER
            FI = (K-HSIZER-1) * CELL / HWIDTH
            IF (ABS(FI).LT.5.) THEN
               GAUSS(K) = EXP (-0.6931472D0 * (FI**2))
               KMN = MIN (KMN, K)
               KMX = K
               END IF
 75         CONTINUE
      ELSE
         KMN = HSIZER+1 - HSIZE
         KMX = HSIZER+1 + HSIZE
         END IF
C
      MSGTXT = 'FARSIN: input maps coincident on reduced subimage only'
      IF (REDUCE) CALL MSGWRT (6)
      XX(1) = -1.0E30
      XX(2) = -1.0E30
      XN(1) = 1.0E30
      XN(2) = 1.0E30
      BFLAG = .FALSE.
      CALL COPY (256, CATI1, CATBLK)
      INC = 2
      DO 85 I = 1,NAX
         IPOINT = KHCTP+(I-1)*INC
         CALL H2CHR (8, 1, CATH(IPOINT), CHTMP)
         IF (CHTMP.EQ.STOKES) GO TO 95
 85      CONTINUE
      GO TO 110
C                                       get rid of Stokes axis
 95   IF (I.LT.NAX) THEN
         K = I - 1
         DO 100 J = I,NAX-1
            CATBLK(KINAX+K) = MAX (1, CATBLK(KINAX+J))
            CATD(KDCRV+K) = CATD(KDCRV+J)
            CATR(KRCRP+K) = CATR(KRCRP+J)
            CATR(KRCIC+K) = CATR(KRCIC+J)
            CATR(KRCRT+K) = CATR(KRCRT+J)
            CATH(KHCTP+2*K) = CATH(KHCTP+2*J)
            CATH(KHCTP+2*K+1) = CATH(KHCTP+2*J+1)
            K = K + 1
 100        CONTINUE
         DO 101 K = 0,6
            CATBLK(KINAX+K) = MAX (1, CATBLK(KINAX+K))
 101        CONTINUE
         END IF
      CATBLK(KIDIM) = NAX - 1
C                                        Default out class
 110  IDCDEF = 'FARSre'
      IF (APARM(5).GE.1.5) IDCDEF = 'FARSam'
C                                        Set header values for output
C                                        map
      CALL MAKOUT (NAMIN(1), CLSIN(1), ISSEQ(1), IDCDEF, NAMOUT(1),
     *   CLSOUT(1), IDSEQ(1))
      CALL CHR2H (12, NAMOUT(1), KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLSOUT(1), KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = IDSEQ(1)
      CATBLK(KIIMU) = NLUSER
C                                       set all axis for the output file
      CALL SUBHDR (BLC, TRC, 1.0, 1.0)
C                                       reset the first axis FARADROT
      IF (CLFOUR .OR. UNCLFO) THEN
C                                       the whole Fourier with or
C                                       withot CLEAN
         CALL CHR2H (8, 'FARADROT', 1, CATH(KHCTP))
         CATR(KRCRP) = 1
         CATR(KRCIC) = CELL
         CATD(KDCRV) = -HSIZE * CELL
         CATBLK(KINAX) = FSIZE
      ELSE
C                                       the parameters of the only
C                                       maximum at the  Fourier
         CALL CHR2H (8, 'FARADROT', 1, CATH(KHCTP))
         CATR(KRCRP) = 1
         CATR(KRCIC) = 1
         CATD(KDCRV) = 1
         CATBLK(KINAX) = 3
         END IF
C                                        Put in map units
      CALL MCREAT (FCVOL(3), FCCNO(3), IROW, IERR)
      IDSEQ(1) = CATI1(KIIMS)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1110) IERR
         GO TO 980
         END IF
      NCFILE = NCFILE + 1
      FRW(NCFILE) = 2
      FVOL(NCFILE) = FCVOL(3)
      FCNO(NCFILE) = FCCNO(3)
C                                       Copy header keywords from first
C                                       input.
      CALL KEYPCP (FCVOL(1), FCCNO(1), FCVOL(3), FCCNO(3), 0, ' ', IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1135) IERR
         CALL MSGWRT (7)
         END IF
C                                       make slice files
      CALL FARSLI (FCVOL(3), FCCNO(3), HSIZE, RERMT, IMRMT, IERR)
C                                        Open the output file
      CALL ZPHFIL ('MA', FCVOL(3), FCCNO(3), 1, PHNAME, IERR)
      CALL ZOPEN (OLUN(1), OIND(1), FCVOL(3), PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1136) IERR
         GO TO 980
         END IF
      CALL COPY (256, CATBLK, CATIO(1,1))
C                                       output map 2
      IDCDEF = 'FARSim'
      IF (APARM(5).GE.1.5) IDCDEF = 'FARSph'
      CALL MAKOUT (NAMIN(1), CLSIN(1), ISSEQ(1), IDCDEF, NAMOUT(2),
     *   CLSOUT(2), IDSEQ(2))
      CALL CHR2H (12, NAMOUT(2), KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLSOUT(2), KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = IDSEQ(2)
      CATBLK(KIIMU) = NLUSER
      CALL MCREAT (FCVOL(4), FCCNO(4), IROW, IERR)
      IDSEQ(2) = CATI1(KIIMS)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1110) IERR
         GO TO 980
         END IF
      NCFILE = NCFILE + 1
      FRW(NCFILE) = 2
      FVOL(NCFILE) = FCVOL(4)
      FCNO(NCFILE) = FCCNO(4)
      IF (APARM(5).GE.1.5) CALL CHR2H (8, 'DEGREES ', 1, CATH(KHBUN))
C                                       Copy header keywords from first
C                                       input.
      CALL KEYPCP (FCVOL(1), FCCNO(1), FCVOL(4), FCCNO(4), 0, ' ',
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1135) IERR
         CALL MSGWRT (7)
         END IF
C                                       slice files
      CALL FARSLI (FCVOL(4), FCCNO(4), HSIZE, RERMT, IMRMT, IERR)
C                                        Open the output file
      CALL ZPHFIL ('MA', FCVOL(4), FCCNO(4), 1, PHNAME, IERR)
      CALL ZOPEN (OLUN(2), OIND(2), FCVOL(4), PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1136) IERR
         GO TO 980
         END IF
      CALL COPY (256, CATBLK, CATIO(1,2))
      GO TO 999
C-----------------------------------------------------------------------
C                                        Error returns
C
 900  WRITE (MSGTXT,1900) IERR
 980  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('FARSIN: COULD NOT GET PARMS.  IER=',I7)
 1025 FORMAT ('FARSIN: COULD NOT OPEN MAP # ',I1,'  IER=',I7)
 1050 FORMAT ('L2MEAN = ',  F10.5, ' m^2')
 1071 FORMAT ('FARSIN: using restoring Gaussian of FWHM',F9.3,' 1/m^2')
 1075 FORMAT ('FARSIN: INPUT MAPS ARE NOT COINCIDENT ON AXIS',I2)
 1110 FORMAT ('FARSIN: COULD NOT CREATE OUTPUT MAP.  IER=',I7)
 1135 FORMAT ('FARSIN: ERROR ', I3,' COPYING KEYWORDS - CONTINUING')
 1136 FORMAT ('FARSIN: COULD NOT OPEN OUTPUT MAP.  IER=',I7)
 1900 FORMAT ('TABIO: ERROR = ', I3)
      END
      SUBROUTINE FARSLI (DISK, CNO, HSIZE, RERMT, IMRMT, IRET)
C-----------------------------------------------------------------------
C   FARSLI writes 4 slice files containing the beam: real, imag, amp, p
C   Inputs:
C      DISK    I      Disk of file
C      CNO     I      Catalog number of file
C      HSIZE   I      Size parameter
C      RERMT   R(*)   Real part of beam
C      IMRMT   R(*)   Imaginary part of beam
C   Output:
C      IRET    I      Error code
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, HSIZE, IRET
      REAL      RERMT(*), IMRMT(*)
C
      INTEGER   IVER, NRPBLK, INOSL, NREC, IWBLK(512), LUN, IND, I, IP,
     *   KREC, IREC, K
      REAL      RWBLK(512), RMIN, RMAX, AA, PP
      DOUBLE PRECISION DWBLK(256)
      HOLLERITH HWBLK(512)
      EQUIVALENCE (DWBLK, RWBLK, IWBLK, HWBLK)
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUN /73/
C-----------------------------------------------------------------------
      IVER = 0
C                                       real part
      IVER = IVER + 1
      NRPBLK = 256
      INOSL = 2 * HSIZE + 1
      NREC = (INOSL - 1) / NRPBLK + 3
      CALL FILL (256, 0, IWBLK)
      CALL EXTINI ('WRIT', 'SL', DISK, CNO, IVER, CATBLK, LUN, IND,
     *   NRPBLK, NREC, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT SL FILE'
         GO TO 990
         END IF
      WRITE (MSGTXT,1005) IVER, 'real part'
      CALL MSGWRT (3)
C                                       update header record
      CALL ZFIO ('READ', LUN, IND, 1, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ INITIAL SL HEADER'
         GO TO 990
         END IF
      CALL CHR2H (6, TSKNAM, 1, HWBLK(30))
      CALL ZDATE (IWBLK(33))
      CALL ZTIME (IWBLK(36))
      IWBLK(57) = INOSL
      IWBLK(58) = 0
      IWBLK(59) = NREC
      CALL ZFIO ('WRIT', LUN, IND, 1, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'UPDATE SL HEADER'
         GO TO 990
         END IF
C                                       Put inputs in 2nd record.
      CALL FILL (256, 0, IWBLK)
      CALL CHR2H (6, TSKNAM, 1, HWBLK(1))
      CALL ZDATE (IWBLK(4))
      CALL ZTIME (IWBLK(7))
      IWBLK(10) = 23
      RWBLK(11) = NLUSER
      RWBLK(18) = DISK
      CALL RFILL (14, 1.0, RWBLK(19))
      RWBLK(26) = 2 * HSIZE + 1
      CALL CHR2H (4, 'AVER', 1, HWBLK(33))
      RMAX = -1.E10
      RMIN = 1.E10
      DO 10 I = HSIZE+1,3*HSIZE
         RMIN = MIN (RMIN, RERMT(I))
         RMAX = MAX (RMAX, RERMT(I))
 10      CONTINUE
      RWBLK(34) = RMIN
      RWBLK(35) = RMAX
      CALL ZFIO ('WRIT', LUN, IND, 2, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE RECORD 2 IN SL FILE'
         GO TO 990
         END IF
      KREC = (INOSL - 1) / NRPBLK + 3
      IP = HSIZE + 1
      DO 20 IREC = 3,KREC
         CALL RFILL (256, 0.0, RWBLK)
         DO 15 K = 1,256
            RWBLK(K) = RERMT(IP)
            IP = IP + 1
            IF (IP.GT.3*HSIZE) GO TO 16
 15         CONTINUE
 16      CALL ZFIO ('WRIT', LUN, IND, IREC, IWBLK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE DATA RECORD IN SL FILE'
            GO TO 990
            END IF
 20      CONTINUE
      CALL ZCLOSE (LUN, IND, I)
C                                       imaginary
      IVER = IVER + 1
      NRPBLK = 256
      INOSL = 2 * HSIZE + 1
      NREC = (INOSL - 1) / NRPBLK + 3
      CALL FILL (256, 0, IWBLK)
      CALL EXTINI ('WRIT', 'SL', DISK, CNO, IVER, CATBLK, LUN, IND,
     *   NRPBLK, NREC, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT SL FILE'
         GO TO 990
         END IF
      WRITE (MSGTXT,1005) IVER, 'imaginary part'
      CALL MSGWRT (3)
C                                       update header record
      CALL ZFIO ('READ', LUN, IND, 1, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ INITIAL SL HEADER'
         GO TO 990
         END IF
      CALL CHR2H (6, TSKNAM, 1, HWBLK(30))
      CALL ZDATE (IWBLK(33))
      CALL ZTIME (IWBLK(36))
      IWBLK(57) = INOSL
      IWBLK(58) = 0
      IWBLK(59) = NREC
      CALL ZFIO ('WRIT', LUN, IND, 1, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'UPDATE SL HEADER'
         GO TO 990
         END IF
C                                       Put inputs in 2nd record.
      CALL FILL (256, 0, IWBLK)
      CALL CHR2H (6, TSKNAM, 1, HWBLK(1))
      CALL ZDATE (IWBLK(4))
      CALL ZTIME (IWBLK(7))
      IWBLK(10) = 23
      RWBLK(11) = NLUSER
      RWBLK(18) = DISK
      CALL RFILL (14, 1.0, RWBLK(19))
      RWBLK(26) = 2 * HSIZE + 1
      CALL CHR2H (4, 'AVER', 1, HWBLK(33))
      RMAX = -1.E10
      RMIN = 1.E10
      DO 30 I = HSIZE+1,3*HSIZE
         RMIN = MIN (RMIN, IMRMT(I))
         RMAX = MAX (RMAX, IMRMT(I))
 30      CONTINUE
      RWBLK(34) = RMIN
      RWBLK(35) = RMAX
      CALL ZFIO ('WRIT', LUN, IND, 2, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE RECORD 2 IN SL FILE'
         GO TO 990
         END IF
      KREC = (INOSL - 1) / NRPBLK + 3
      IP = HSIZE + 1
      DO 40 IREC = 3,KREC
         CALL RFILL (256, 0.0, RWBLK)
         DO 35 K = 1,256
            RWBLK(K) = IMRMT(IP)
            IP = IP + 1
            IF (IP.GT.3*HSIZE) GO TO 36
 35         CONTINUE
 36      CALL ZFIO ('WRIT', LUN, IND, IREC, IWBLK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE DATA RECORD IN SL FILE'
            GO TO 990
            END IF
 40      CONTINUE
      CALL ZCLOSE (LUN, IND, I)
C                                       amplitude
      IVER = IVER + 1
      NRPBLK = 256
      INOSL = 2 * HSIZE + 1
      NREC = (INOSL - 1) / NRPBLK + 3
      CALL FILL (256, 0, IWBLK)
      CALL EXTINI ('WRIT', 'SL', DISK, CNO, IVER, CATBLK, LUN, IND,
     *   NRPBLK, NREC, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT SL FILE'
         GO TO 990
         END IF
      WRITE (MSGTXT,1005) IVER, 'amplitude'
      CALL MSGWRT (3)
C                                       update header record
      CALL ZFIO ('READ', LUN, IND, 1, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ INITIAL SL HEADER'
         GO TO 990
         END IF
      CALL CHR2H (6, TSKNAM, 1, HWBLK(30))
      CALL ZDATE (IWBLK(33))
      CALL ZTIME (IWBLK(36))
      IWBLK(57) = INOSL
      IWBLK(58) = 0
      IWBLK(59) = NREC
      CALL ZFIO ('WRIT', LUN, IND, 1, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'UPDATE SL HEADER'
         GO TO 990
         END IF
C                                       Put inputs in 2nd record.
      CALL FILL (256, 0, IWBLK)
      CALL CHR2H (6, TSKNAM, 1, HWBLK(1))
      CALL ZDATE (IWBLK(4))
      CALL ZTIME (IWBLK(7))
      IWBLK(10) = 23
      RWBLK(11) = NLUSER
      RWBLK(18) = DISK
      CALL RFILL (14, 1.0, RWBLK(19))
      RWBLK(26) = 2 * HSIZE + 1
      CALL CHR2H (4, 'AVER', 1, HWBLK(33))
      RMAX = -1.E10
      RMIN = 1.E10
      DO 50 I = HSIZE+1,3*HSIZE
         AA = SQRT (RERMT(I)*RERMT(I) + IMRMT(I)*IMRMT(I))
         RMIN = MIN (RMIN, AA)
         RMAX = MAX (RMAX, AA)
 50      CONTINUE
      RWBLK(34) = RMIN
      RWBLK(35) = RMAX
      CALL ZFIO ('WRIT', LUN, IND, 2, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE RECORD 2 IN SL FILE'
         GO TO 990
         END IF
      KREC = (INOSL - 1) / NRPBLK + 3
      IP = HSIZE + 1
      DO 60 IREC = 3,KREC
         CALL RFILL (256, 0.0, RWBLK)
         DO 55 K = 1,256
            AA = SQRT (RERMT(IP)*RERMT(IP) + IMRMT(IP)*IMRMT(IP))
            RWBLK(K) = AA
            IP = IP + 1
            IF (IP.GT.3*HSIZE) GO TO 56
 55         CONTINUE
 56      CALL ZFIO ('WRIT', LUN, IND, IREC, IWBLK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE DATA RECORD IN SL FILE'
            GO TO 990
            END IF
 60      CONTINUE
      CALL ZCLOSE (LUN, IND, I)
C                                       phase
      IVER = IVER + 1
      NRPBLK = 256
      INOSL = 2 * HSIZE + 1
      NREC = (INOSL - 1) / NRPBLK + 3
      CALL FILL (256, 0, IWBLK)
      CALL EXTINI ('WRIT', 'SL', DISK, CNO, IVER, CATBLK, LUN, IND,
     *   NRPBLK, NREC, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT SL FILE'
         GO TO 990
         END IF
      WRITE (MSGTXT,1005) IVER, 'phase'
      CALL MSGWRT (3)
C                                       update header record
      CALL ZFIO ('READ', LUN, IND, 1, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ INITIAL SL HEADER'
         GO TO 990
         END IF
      CALL CHR2H (6, TSKNAM, 1, HWBLK(30))
      CALL ZDATE (IWBLK(33))
      CALL ZTIME (IWBLK(36))
      IWBLK(57) = INOSL
      IWBLK(58) = 0
      IWBLK(59) = NREC
      CALL ZFIO ('WRIT', LUN, IND, 1, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'UPDATE SL HEADER'
         GO TO 990
         END IF
C                                       Put inputs in 2nd record.
      CALL FILL (256, 0, IWBLK)
      CALL CHR2H (6, TSKNAM, 1, HWBLK(1))
      CALL ZDATE (IWBLK(4))
      CALL ZTIME (IWBLK(7))
      IWBLK(10) = 23
      RWBLK(11) = NLUSER
      RWBLK(18) = DISK
      CALL RFILL (14, 1.0, RWBLK(19))
      RWBLK(26) = 2 * HSIZE + 1
      CALL CHR2H (4, 'AVER', 1, HWBLK(33))
      RMAX = -1.E10
      RMIN = 1.E10
      DO 70 I = HSIZE+1,3*HSIZE
         PP = ATAN2 (IMRMT(I), RERMT(I)) * RAD2DG
         RMIN = MIN (RMIN, PP)
         RMAX = MAX (RMAX, PP)
 70      CONTINUE
      RWBLK(34) = RMIN
      RWBLK(35) = RMAX
      CALL ZFIO ('WRIT', LUN, IND, 2, IWBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE RECORD 2 IN SL FILE'
         GO TO 990
         END IF
      KREC = (INOSL - 1) / NRPBLK + 3
      IP = HSIZE + 1
      DO 80 IREC = 3,KREC
         CALL RFILL (256, 0.0, RWBLK)
         DO 75 K = 1,256
            PP = ATAN2 (IMRMT(IP), RERMT(IP)) * RAD2DG
            RWBLK(K) = PP
            IP = IP + 1
            IF (IP.GT.3*HSIZE) GO TO 76
 75         CONTINUE
 76      CALL ZFIO ('WRIT', LUN, IND, IREC, IWBLK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE DATA RECORD IN SL FILE'
            GO TO 990
            END IF
 80      CONTINUE
      CALL ZCLOSE (LUN, IND, I)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FARSLI: ERROR',I4,' ON ',A)
 1005 FORMAT ('Created slice file version',I2,' for Beam ',A)
      END
      SUBROUTINE FARSCL (V1, V2, APARM, R1, R2)
C-----------------------------------------------------------------------
C   FARSCL does a peak-in-the-amplitude Clean of a data row.
C   Inputs:
C      V1     R(NX)   The pixel values of the input map 1 NX = NL2AX
C      V2     R(NX)   The pixel values of the input map 2
C      APARM  R(10)   Up to 10 inputs parameters: RMTF?, amp/phase?
C   Outputs:
C      R1     R(NX)   The real part or the amplitude
C      R2     R(NX)   The imaginary part or the phase
C-----------------------------------------------------------------------
      REAL      V1(*), V2(*), R1(*), R2(*), APARM(10)
C
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   ARSIZE
      PARAMETER (ARSIZE = MAXIMG)
C
      INTEGER   I, K, L, KL, KMAXAM, ITER, KOFMAX, BLANK, KOFF
      REAL      RC1(ARSIZE), RC2(ARSIZE), REF, IMF, ARG, FI, PHASE,
     *   PHASEC, REFF, IMFF, AMPMAX, RTEMP, REFAR(ARSIZE),
     *   IMFAR(ARSIZE), AMP(ARSIZE), TEMPRE, TEMPIM, REFAMP, IMFAMP,
     *   REFMUL, IMFMUL,RECOMP, IMCOMP, RECOUT, IMCOUT, AMPRES, RERES,
     *   IMRES, CARG, SARG
      INCLUDE 'FARS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      BLANK = 0
C                                       weight/test input data
      DO 20 I = 1,NL2AX
         IF ((V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK)) THEN
            V1(I) = 0
            V2(I) = 0
         ELSE
            BLANK = BLANK + 1
C                                       to get RMTF
            IF (APARM(3).GT.0.0) THEN
               V1(I) = W(I)
               V2(I) = 0
            ELSE
               V1(I) = W(I) * V1(I)
               V2(I) = W(I) * V2(I)
               END IF
            END IF
 20      CONTINUE
C                                       not enough points
      IF (BLANK.LE.2) THEN
         CALL RFILL (FSIZE, FBLANK, R1)
         CALL RFILL (FSIZE, FBLANK, R2)
         BFLAG = .TRUE.
         GO TO 999
         END IF
C                                       evaluate the Fourier transorm of
C                                       the complex array V1(I)+J*V2(I)
      AMPMAX = -1.0E+10
      DO 60 K = 1,FSIZE
         REF = 0
         IMF = 0
         FI = 2.0 * (K-HSIZE-1) * CELL
         DO 50 I = 1,NL2AX
            ARG = FI * ARRL2(I)
            CARG = COS (ARG)
            SARG = SIN (ARG)
C                                       DFT with exp-(ARG)
            REF = REF + V1(I)*CARG + V2(I)*SARG
            IMF = IMF + V2(I)*CARG - V1(I)*SARG
 50         CONTINUE
         REF = REF / WSUM / BMAX
         IMF = IMF / WSUM / BMAX
         REFAR(K) = REF
         IMFAR(K) = IMF
C                                       convert to the AMP, PHASE
         AMP(K) = SQRT (REF*REF + IMF*IMF)
         PHASE = ATAN2 (IMF, REF)
C                                       Find position of maximum of
C                                       amplitude
         IF (AMP(K).GT.AMPMAX) THEN
            AMPMAX = AMP(K)
            KMAXAM = K
C                                       store the value of REF, IMF
            REFAMP = REF
            IMFAMP = IMF
            END IF
C                                       subtract the phase at L2MEAN
C                                       to get the original data
C                                       corresponded to not shifted
C                                       L^2 set
         PHASEC = PHASE - 2 * FI * L2MEAN
         REFF = AMP(K) * COS (PHASEC)
         IMFF = AMP(K) * SIN (PHASEC)
C                                       store the output at
C                                       the TEMPRE, TEMPIM
C                                       shifted back data (original
         IF (APARM(5).LE.0.0) THEN
            TEMPRE = REFF
            TEMPIM = IMFF
C                                       shifted data
         ELSE IF (APARM(5).LT.1.5) THEN
            TEMPRE = REF
            TEMPIM = IMF
C                                       Amplitudes
         ELSE
            TEMPRE = REFF
            TEMPIM = IMFF
            END IF
C
C                                       record into output
C                                       Full Fourier transform without
C                                       CLEAN
         IF (UNCLFO) THEN
            R1(K) = TEMPRE
            R2(K) = TEMPIM
         ELSE
            R1(K) = 0
            R2(K) = 0
            END IF
  60     CONTINUE
C                                       Clean
      IF (.NOT.UNCLFO) THEN
         ITER = 0
C                                       cycle by iteration(components)
 100     IF ((ITER.LT.NITER) .AND. (AMPMAX.GE.FLUX)) THEN
            ITER = ITER + 1
C                                       component to be subtracted
            RECOMP = GAIN * REFAMP
            IMCOMP = GAIN * IMFAMP
            PHASE  = ATAN2 (IMFAMP, REFAMP)
C                                       subtract the phase at L2MEAN
C                                       to get the original component
C                                       corresponded to not shifted
C                                       L^2 set
            FI = (KMAXAM-HSIZE-1) * CELL
            PHASEC = PHASE - 2 * FI * L2MEAN
            REFF = AMPMAX * COS(PHASEC)
            IMFF = AMPMAX * SIN(PHASEC)
            RECOUT = GAIN * REFF
            IMCOUT = GAIN * IMFF
C                                       iteration (component) is found
            R1(KMAXAM) = R1(KMAXAM) + RECOUT
            R2(KMAXAM) = R2(KMAXAM) + IMCOUT
            IF (DOPRT.GT.0) THEN
               WRITE (MSGTXT,1100)
               IF (ITER.EQ.1) CALL MSGWRT (4)
               WRITE (MSGTXT,1101) ITER, AMPMAX, PHASE*RAD2DG, KMAXAM
               CALL MSGWRT (4)
               END IF
C                                       Subtract the RMTF multiplied by
C                                       the value of RE/IM at max amp
C                                       from RE,IM of the subtracted
C                                       component located at
C                                       K = KMAXAM
            AMPMAX = -1.0E+10
            KOFF = KMAXAM - HSIZER - 1
            DO 120 K = 1,FSIZE
C                                       KOFMAX position of RMTF's max
C                                       relatively AMP max
               KOFMAX = K - KOFF
C                                       The RE, Im parts for the current
C                                       Multiply the positioned RMTF
C                                       by the subtracted component
               REFMUL = RECOMP*RERMT(KOFMAX) - IMCOMP*IMRMT(KOFMAX)
               IMFMUL = RECOMP*IMRMT(KOFMAX) + IMCOMP*RERMT(KOFMAX)
C                                       Subtract the component
               REFAR(K) = REFAR(K) - REFMUL
               IMFAR(K) = IMFAR(K) - IMFMUL
C                                       Find position of maximum of
C                                       amplitude
               AMP(K) = SQRT (REFAR(K)*REFAR(K) + IMFAR(K)*IMFAR(K))
               IF (AMP(K).GT.AMPMAX) THEN
                  AMPMAX = AMP(K)
                  KMAXAM = K
                  REFAMP = REFAR(K)
                  IMFAMP = IMFAR(K)
                  END IF
  120          CONTINUE
            GO TO 100
            END IF
C
C                                       End of CLEAN
C                                       Convolve the found set of
C                                       components with the RE of RMTF
C                                       or with the given Gaussian
         IF (DOCONV .AND. (CLONLY .OR. CLPLRE)) THEN
            CALL RFILL (FSIZE, 0.0, RC1)
            CALL RFILL (FSIZE, 0.0, RC2)
            DO 140 L = 1,FSIZE
               IF ((R1(L).NE.0.0) .OR. (R2(L).NE.0.0)) THEN
                  DO 130 KL = KMN,KMX
C                                       KL position of RMTF's max
C                                       relatively K
                     K = L - KL + HSIZER+1
                     IF ((K.GE.1) .AND. (K.LE.FSIZE)) THEN
                        IF (DOGAUS) THEN
                           RC1(K) = RC1(K) + GAUSS(KL)*R1(L)
                           RC2(K) = RC2(K) + GAUSS(KL)*R2(L)
                        ELSE
                           RC1(K) = RC1(K) + RERMT(KL)*R1(L)
                           RC2(K) = RC2(K)+ RERMT(KL)*R2(L)
                           END IF
                        END IF
 130                 CONTINUE
                  END IF
 140           CONTINUE
C                                       move back
            DO 160 K = 1, FSIZE
               R1(K) = RC1(K)
               R2(K) = RC2(K)
 160           CONTINUE
            END IF
C                                       residuals in output
         IF ((DORES) .OR. (CLPLRE)) THEN
            DO 180 K = 1,FSIZE
               PHASE = ATAN2 (IMFAR(K), REFAR(K))
               AMPRES = SQRT (IMFAR(K)**2 + REFAR(K)**2)
C                                       subtract the phase at L2MEAN
C                                       to get the original component
C                                       corresponded to not shifted
C                                       L^2 set
               FI = (K-HSIZE-1) * CELL
               PHASEC = PHASE - 2 * FI * L2MEAN
C                                       Store the shifted back residual
               RERES = AMPRES * COS(PHASEC)
               IMRES = AMPRES * SIN(PHASEC)
C                                       Send the residual into the
C                                       output
               IF (DORES) THEN
                  R1(K) = RERES
                  R2(K) = IMRES
C                                       Send the residual plus convolved
C                                       into the output
               ELSE IF (CLPLRE) THEN
                  R1(K) = R1(K) + RERES
                  R2(K) = R2(K) + IMRES
                  END IF
 180           CONTINUE
            END IF
         END IF
C                                       calculate amplitudes
      IF (APARM(5).GE.1.5) THEN
         DO 200 K = 1, FSIZE
            RTEMP = R1(K)
            R1(K) = SQRT (R1(K)*R1(K) + R2(K)*R2(K))
            R2(K) = RAD2DG * ATAN2(R2(K), RTEMP)
 200        CONTINUE
         END IF
C                                       Common processing
      DO 910 I = 1,FSIZE
C                                       Get max/min
         IF (R1(I).NE.FBLANK) THEN
            XX(1) = MAX (XX(1), R1(I))
            XN(1) = MIN (XN(1), R1(I))
         ELSE
            BFLAG = .TRUE.
            END IF
C
         IF (R2(I).NE.FBLANK) THEN
            XX(2) = MAX (XX(2), R2(I))
            XN(2) = MIN (XN(2), R2(I))
         ELSE
            BFLAG = .TRUE.
            END IF
 910     CONTINUE
      IF (ITER.GT.0) DOPRT = DOPRT - 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (' Iter  Amplitude  Phase Pixel')
 1101 FORMAT (I5,1PE11.3,0PF7.1,I6)
      END
      SUBROUTINE FARSHI (IRET)
C-----------------------------------------------------------------------
C   FARSHI creates and writes the HI file associated with task FARS.
C   Outputs:
C      IRET   I      > 0 => output all blanks
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER HILINE*72, FTYPE(2,2)*9, NOTTYP*2, CTIME*8, CDATE*12
      INTEGER   NHISTF, IHDLUN, IHSLUN, I, IER, IERR, IBUFF1(256), J,
     *    IBUFF2(256), IPTR, ITEMP, FU, TIME(3), DATE(3)
      LOGICAL   TRUE, FALSE
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'FARS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (IBUFF1(1),IROW(1,1)),  (IBUFF2(1),IROW(257,1))
      DATA NHISTF, IHDLUN, IHSLUN /2,27,28/
      DATA TRUE, FALSE /.TRUE.,.FALSE./
      DATA FTYPE /'Real','Imaginary', 'Amplitude', 'Phase'/
      DATA NOTTYP /'CC'/
C-----------------------------------------------------------------------
C                                       Initialize HITAB
      CALL HIINIT (NHISTF)
      IRET = 0
      FU = 1
      IF (CPARM(5).GE.1.5) FU = 2
      DO 200 J = 2,1,-1
C                                       Test validity of result
         IF (XX(J).LT.XN(J)) THEN
            WRITE (MSGTXT,1000) FTYPE(J,FU)
            CALL MSGWRT (6)
            BFLAG = .TRUE.
         ELSE IF (XX(J).EQ.XN(J)) THEN
            WRITE (MSGTXT,1001) FTYPE(J,FU), XX(J)
            CALL MSGWRT (6)
            END IF
C                                       Insert header parameters
         CALL COPY (256, CATIO(1,J), CATBLK)
         CATR(KRDMN) = XN(J)
         CATR(KRDMX) = XX(J)
C                                        Put in inhibited pixel value
         CATR(KRBLK) = 0.0
         IF (BFLAG) CATR(KRBLK) = FBLANK
C                                       Create and open new HI file
         CALL HICREA (IHDLUN, FCVOL(2+J), FCCNO(2+J), CATBLK, IBUFF2,
     *      IERR)
         IPTR = IBUFF2(3)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) FTYPE(J,FU), IERR
            CALL MSGWRT (7)
            GO TO 200
            END IF
C                                       Copy both input HI files
         DO 50 I = 1,2
            IF (HISTDO.GE.-3+I) THEN
               WRITE (HILINE,1020) I
               CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
               IF (IERR.NE.0) GO TO 190
               ITEMP = HITAB(IPTR+2)
               CALL HIOPEN (IHSLUN, FCVOL(I), FCCNO(I), IBUFF1, IERR)
               IF (IERR.NE.0) GO TO 30
               CALL HICOPY (IHSLUN, IBUFF1, IHDLUN, IBUFF2, IERR)
               CALL HICLOS (IHSLUN, FALSE, IBUFF1, IER)
               IF (IERR.EQ.0) GO TO 50
                  IF (IERR.GE.100) HITAB(IPTR+2) = ITEMP
 30               WRITE (MSGTXT,1030) I, IERR
                  CALL MSGWRT (6)
                  WRITE (HILINE,1031)
                  CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
                  IF (IERR.NE.0) GO TO 190
            ELSE
               WRITE (HILINE,1032) TSKNAM, I
               CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
               IF (IERR.NE.0) GO TO 190
               END IF
 50         CONTINUE
         WRITE (HILINE,1050)
         CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         CALL ZDATE (DATE)
         CALL ZTIME (TIME)
         CALL TIMDAT (TIME, DATE, CTIME, CDATE)
         WRITE (HILINE,1055) TSKNAM, RLSNAM, CDATE, CTIME
         CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       Add input parameters
         CALL HENCO1 (TSKNAM, NAMIN(1), CLSIN(1), ISSEQ(1), FCVOL(1),
     *      IHDLUN, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         CALL HENCO2 (TSKNAM, NAMIN(2), CLSIN(2), ISSEQ(2), FCVOL(2),
     *      IHDLUN, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         CALL HENCOO (TSKNAM, NAMOUT(J), CLSOUT(J), IDSEQ(J),
     *      FCVOL(2+J), IHDLUN, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,1094) TSKNAM, (IBLC(I,1), I = 1,7)
         CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,1096) TSKNAM, (ITRC(I,1), I = 1,7)
         CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       Parameters
         I = CPARM(1) + 0.1
         WRITE (HILINE,1101) TSKNAM, I
         CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,1102) TSKNAM, CPARM(2)
         CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         IF (CPARM(3).GT.0.0) THEN
            WRITE (HILINE,1103) TSKNAM, 3, 1, 'output is RMTF (beam)'
         ELSE
            WRITE (HILINE,1103) TSKNAM, 3, 0, 'output is normal data'
            END IF
         CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         IF (CPARM(4).GT.0.0) THEN
            WRITE (HILINE,1103) TSKNAM, 4, 1, 'output is not Cleaned'
         ELSE
            WRITE (HILINE,1103) TSKNAM, 4, 0, 'output is Cleaned'
            END IF
         CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         IF (CPARM(5).GE.1.5) THEN
            WRITE (HILINE,1103) TSKNAM, 5, 2, 'output is Amp/phase'
         ELSE IF (CPARM(5).GE.0.01) THEN
            WRITE (HILINE,1103) TSKNAM, 5, 1, 'output is shifted Re/Im'
         ELSE
            WRITE (HILINE,1103) TSKNAM, 5, 0,
     *         'output is unshifted Re/Im'
            END IF
         CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         IF (DOCONV) THEN
            WRITE (HILINE,1103) TSKNAM, 7, 0, 'output is convolved'
         ELSE
            WRITE (HILINE,1103) TSKNAM, 7, 1, 'output is not convolved'
            END IF
         CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         IF (DOCONV) THEN
            IF (DOGAUS) THEN
               WRITE (HILINE,1103) TSKNAM, 8, 0,
     *            'convolved with Gaussian'
            ELSE
               WRITE (HILINE,1103) TSKNAM, 8, 1,
     *            'convolved with Real(RMTF)'
               END IF
            CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            END IF
         IF ((DOCONV)  .AND. (DOGAUS)) THEN
            WRITE (HILINE,1109) TSKNAM, CPARM(9)
            CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            END IF
         IF (CLPLRE) THEN
            WRITE (HILINE,1103) TSKNAM, 10, 0,
     *         'output is Clean + residual'
         ELSE IF (CLONLY) THEN
            WRITE (HILINE,1103) TSKNAM, 10, 1,
     *         'output is Clean only, no residual'
         ELSE IF (DORES) THEN
            WRITE (HILINE,1103) TSKNAM, 10, 2,
     *         'output is residual only'
            END IF
         CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       gain, flux, niter
         WRITE (HILINE,1111) TSKNAM, GAIN
         CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,1112) TSKNAM, NITER
         CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,1113) TSKNAM, FLUX
         CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       Close HI file
 190     CALL HICLOS (IHDLUN, TRUE, IBUFF2, IER)
         WRITE (MSGTXT,1900) FTYPE(J,FU)
         IF ((IER.NE.0) .OR. (IERR.NE.0)) WRITE (MSGTXT,1901)
     *      FTYPE(J,FU), IERR, IER
         CALL MSGWRT (2)
C                                        Copy tables
         CALL ALLTAB (1, NOTTYP, IHSLUN, IHDLUN, FCVOL, FCVOL(J+2),
     *      FCCNO, FCCNO(J+2), CATBLK, IBUFF1, IBUFF2, IERR)
         IF (IERR.GT.2) THEN
            MSGTXT = 'ERROR COPYING TABLE FILES'
            CALL MSGWRT (6)
            END IF
C                                       Close map file
         CALL MAPCLS ('INIT', FCVOL(2+J), FCCNO(2+J), OLUN(J),
     *      OIND(J), CATBLK, TRUE, IBUFF2, IERR)
         NCFILE = NCFILE - 1
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1910) FTYPE(J,FU), IERR
            CALL MSGWRT (6)
            END IF
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OUTPUT ',A,' FILE COMPLETELY BLANKED',
     *   ' -- saving it anyway')
 1001 FORMAT ('Output ',A,' FILE constant at',1PE13.5,
     *   ' -- saving it anyway')
 1010 FORMAT ('FARSHI: could not create HISTORY FOR ',A,' FILE.  IER=',
     *   I5)
 1020 FORMAT (31X,'/ History of input map', I2)
 1030 FORMAT ('WARNING COULD NOT OPEN/COPY INPUT HI FILE=',I2,
     *   '.  IER=',I7)
 1031 FORMAT (31X,'/ NO HISTORY TO BE FOUND')
 1032 FORMAT (A6,'/ No history copied from file',I2)
 1050 FORMAT (31X,'/ End of old histories')
 1055 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',A12,2X,A8)
 1094 FORMAT (A6,'BLC=',7I5,' / Bottom left corner')
 1096 FORMAT (A6,'TRC=',7I5,' / Top right corner')
 1101 FORMAT (A6,'APARM(1)  =',I5,10X,'/ # pixels at half FFT output')
 1102 FORMAT (A6,'APARM(2)  =',1PE11.4,4X,'/ output cell size 1/m^2')
 1103 FORMAT (A6,'APARM(',I1,')  =',I5,10X,'/ ',A)
 1109 FORMAT (A6,'APARM(9)  =',1PE11.4,4X,
     *   '/ full width Gauss conv func 1/m^2')
 1111 FORMAT (A6,'GAIN =',F6.2,'  / loop gain in Clean')
 1112 FORMAT (A6,'NITER=',I6,'  / iteration limit in Clean')
 1113 FORMAT (A6,'FLUX =',1PE11.3,'  / lowest flux in Clean')
 1900 FORMAT ('History file created and written for ',A,' file')
 1901 FORMAT ('HISTORY FILE FOR ',A,' INCOMPLETE.  IERRS =',2I7)
 1910 FORMAT ('WARNING FARSHI COULD NOT CLOSE OUTPUT ',A,
     *   ' FILE.  IER=',I7)
      END
      SUBROUTINE SOLIN (ARG, FUNC, NFIT, NEQUAT, FITPAR, VARRES, IRET)
C-----------------------------------------------------------------------
C   Routine to fit a polynom to the data by Least Square method
C   Input:
C      ARG     R(*)  Array of data arguments
C      FUNC    R(*)  Array of data function
C      NFIT    I     Number of parameters to fit
C      NEQUAT  I     Total number of points at arrays ARG and FUNC
C   Output:
C      FITPAR  R(*)  Array of found parameters of fitting function
C      VARRES  R     SQRT of residuals
C      IRET    I     Error; 0 => OK
C-----------------------------------------------------------------------
      INTEGER  NFIT, NEQUAT, I, IFIT, KFIT, IKFIT, IRET
      REAL     ARG(*), FUNC(*), R(20), MATR(400), NOBS, SUM,
     *   SSQ, VX(20), SSQRES, VARRES, VARY, FIT, FITPAR(*)
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Force result vector R(NFIT),
C                                       matrix M(NFIT*NFIT) to zero
      DO 20 IFIT = 1, NFIT
         R(IFIT) = 0.0
         DO 10 KFIT = 1, NFIT
            IKFIT = IFIT + (KFIT - 1)*NFIT
            MATR (IKFIT) = 0.0
 10         CONTINUE
 20      CONTINUE
      SUM = 0.0
      SSQ = 0.0
      NOBS = 0.0
C                                       Prepare result vector R(NFIT)
C                                       and matrix MATR(NFIT*NFIT)
C                                       for routine LEASQR

      DO 80 I = 1, NEQUAT
         NOBS = NOBS + 1
         SUM = SUM + FUNC(I)
         SSQ = SSQ + FUNC(I)*FUNC(I)
C
         DO 60 IFIT = 1, NFIT
            R(IFIT) = R(IFIT) + FUNC(I) * (ARG(I) ** (IFIT - 1))
C                                       calculate upper/right
C                                       triangle of MATR
            DO 40 KFIT = IFIT, NFIT
               IKFIT = IFIT + (KFIT-1)*NFIT
               MATR(IKFIT) = MATR(IKFIT) +
     *            (ARG(I) ** (IFIT - 1)) * (ARG(I) ** (KFIT - 1))
   40          CONTINUE
   60       CONTINUE
 80      CONTINUE
C
      CALL LEASQR (NFIT, NOBS, SUM, SSQ, R, MATR, FITPAR, VX, SSQRES,
     *   VARRES, VARY, FIT, IRET)
      VARRES = SQRT(VARRES)
  999 RETURN
      END
      SUBROUTINE GETW (FILE, NWEIGT, WEIGHT, IERR)
C-----------------------------------------------------------------------
C  This subroutine reads, from an input file specified by name "file",
C  the sequence of given number (NL2AX) weights.
C  Inputs:
C    FILE     C*48  File name
C    NWEIGT   Number of required weights
C  Outputs in common:
C    WEIGHT    Array of read weights
C  Outputs:
C    IERR     I     Return code, 0=>OK
C-----------------------------------------------------------------------
      CHARACTER FILE*48
      INTEGER   NWEIGT, IERR
      REAL WEIGHT(*)
      INTEGER   FIND,  NBYTES, KBP
      LOGICAL   F
C
      INTEGER   LUN, ILINE, IWEIGT, NLINE, KWEIGT, JT, JTRIM
      DOUBLE PRECISION X
      CHARACTER LINE*80
C
      INCLUDE 'FARS.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Open input text file for read
      LUN = 10
      CALL ZTXOPN ('READ', LUN, FIND, FILE, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1001)
         GO TO 990
         END IF
C                                       Get weight values
      KBP = 1
      NBYTES = 80

C                                       Read antenna info
      IWEIGT = 0
      KWEIGT = 0
      ILINE = 0
C                                       next line
   20 CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
C                                       get the file end
      IF (IERR.EQ.2) THEN
         IERR = 0
         GO TO 970
         END IF
      IF (IERR.NE.0) GO TO 980
      JT = JTRIM (LINE)
C                                       skip comments
      IF (LINE(1:1).EQ.';')  GO TO 20
      ILINE = ILINE + 1
C                                       Get values
      KBP = 1
C                                       cycle in the line
   30 CALL GETNUM (LINE, NBYTES, KBP, X)
C                                       go to the next line
      IF (KBP.GT.NBYTES) GO TO 20

      IWEIGT = IWEIGT + 1
      KWEIGT = KWEIGT + 1
C
      IF (KWEIGT.LT.IBLC(1,1)) THEN
         GO TO 30
      ELSE
         IF (KWEIGT.EQ.IBLC(1,1)) IWEIGT = 1
         END IF
C                                       reach the required number of
C                                       weghts
      IF (IWEIGT.GT.NWEIGT) GO TO 970
      WEIGHT(IWEIGT) = X

      WRITE (MSGTXT,1005) ILINE, IWEIGT,  WEIGHT(IWEIGT)
      CALL MSGWRT (6)
C                                       next number in the line
      GO TO 30
C                                       Number of lines in the
C                                       input file
  970 NLINE = ILINE
C                                       close input file
      CALL ZTXCLS (LUN, FIND, IERR)
C
      GO TO 999
C                                       Infile read error
 980  WRITE (MSGTXT,1003) IERR
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('ERROR ',I3,' OPENING WEIGHT TEXT FILE')
 1003 FORMAT ('ERROR ',I3,' READING ANTENNA INFO TEXT FILE')
 1005 FORMAT ('Line number= ', I5, ' Weight number= ', I5,
     *   ' Weight= ', F11.0)
      END
      SUBROUTINE LEASQR (NP, N, SUM, SSQ, R, M, X, VX, SSQRES,
     *   VARRES, VARY, FIT, IERR)
C-----------------------------------------------------------------------
C   The private version excludes the print out when there is a problem
C   LEASQR does the matrix inversion and other necessary tasks
C   involved in a least squares analysis.
C   Given:
C          NP        I     Number of parameters.
C          N         R     The number of observations.
C          SUM       R     Error sum.
C          SSQ       R     Square error sum.
C          R(NP)     R     Results vector.
C
C   Given and returned:
C          M(NP,NP)  R     On input, the upper triangular part contains
C                          the design matrix.  This is not changed.
C                          On output, the lower triangular part contains
C                          the covariance matrix.  Diagonal elements of
C                          the covariance matrix are stored in VX.
C
C   Returned:
C          X(NP)     R     Vector holding the least squares solution.
C          VX(NP)    R     Variance of the best fit parameters.
C          SSQRES    R     Sum of squares of the residuals.
C          VARRES    R     Variance of the residuals.
C          VARY      R     Variance of the error values.
C          FIT       R     Goodness of fit parameter, lies between 0
C                          and 1.
C          IERR      I     Error status, 0 means successful.
C                             1 - nonspecific error return,
C                             2 - insufficient degrees of freedom.
C
C     Called:
C          none
C
C     Algorithm:
C          LU-triangular factorization with scaled partial pivoting.
C          The sub-diagonal triangular matrix contains the scaling
C          factors used at each step in the Gaussian elimination.  Row
C          interchanges are recorded in vectors MXS and SXM.
C             During forward substitution, the pivoting and Gaussian
C          elimination operations performed on matrix M are applied to
C          vector R.  Vector X holds the intermediate result.
C             On backward substitution, successive elements of the
C          solution vector, X, are calculated by substitution of the
C          preceding elements into the equations of the upper triangular
C          factorization of the design matrix.
C
C     Notes:
C       1) Strictly speaking, the design matrix will usually contain
C          rows of zeroes and therefore be singular.  This arises if no
C          observations sensitive to a particular parameter have been
C          done.
C             In practice, any such singularities are ignored and the
C          associated parameters remain undetermined.
C
C       2) The covariance matrix is the inverse of M(i,j) multiplied by
C          the variance of the residuals.  It is obtained by forward and
C          backward substitution on the columns of the unit matrix.
C
C       3) Two statement functions, C, and SC have been employed to
C          partially alleviate the problems posed by passing arrays in
C          FORTRAN.  The design/covariance matrix m(i,j) is copied into
C          the working vector s(i).  This is addressed by using C, and
C          SC in an attempt to make it look like the matrix that it
C          actually represents.
C
C       4) The maximum size problem that LEASQR can handle is set by
C          parameter MX.
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1988/Sep/29. Code last modified; 1989/Nov/22.
C-----------------------------------------------------------------------
C     Parameter which determines the maximum size problem.
      INTEGER   MX
      PARAMETER (MX = 50)

      INTEGER   C, I, IERR, ITEMP, J, K, MXS(MX), NF, NP, PIVOT,
     *          SXM(MX)
      REAL      COLMAX, DTEMP, FIT, M(NP,NP), N, R(NP), RESIDU, RLEN,
     *          ROWMAX(MX), S(MX*MX), SC, RTEMP, SSQ, SSQRES, SUM,
     *          VARRES, VARY, VX(NP), W(MX), X(NP)

      INCLUDE 'INCS:DMSG.INC'

C     Statement functions for array manipulation, see note 3 above.
      C(I,J)  = NP*(I-1) + J
      SC(I,J) = S(C(I,J))
C-----------------------------------------------------------------------
C  Initialize.
C     Anticipate and return immediately on error.
      IERR = 1

C     Initialize arrays.
      DO 40 I = 1, NP
C        Vector which records row interchanges.
         MXS(I) = I

C        The solution and variance vectors.
         X(I)  = 0.0
         VX(I) = 0.0

C        Copy the design matrix and zero the covariance matrix.
         DO 10 J = 1, I-1
            M(I,J) = 0.0
            S(C(I,J)) = M(J,I)
 10         CONTINUE
         DO 20 J = I, NP
            S(C(I,J)) = M(I,J)
 20         CONTINUE

C        Find the maximum absolute element in each row.
         ROWMAX(I) = 0.0
         DO 30 J = 1, NP
            ROWMAX(I) = MAX(ROWMAX(I), ABS(SC(I,J)))
 30         CONTINUE
 40      CONTINUE

      VARY   = 0.0
      SSQRES = 0.0
      VARRES = 0.0
      FIT    = 0.0


C     Find the number of degrees of freedom.
      NF = N
      DO 60 I = 1, NP
         IF (ROWMAX(I).NE.0.0) THEN
            NF = NF - 1
         ELSE IF (R(I).NE.0.0) THEN
C           Any row of zeroes must extend to the results vector.
C            WRITE (MSGTXT,50) I
C 50         FORMAT ('LEASQR: Design matrix inconsistency in row',I4)
C            CALL MSGWRT (6)
         END IF
 60   CONTINUE

      IF (NF.LE.1) THEN
C         WRITE (MSGTXT,70)
C 70      FORMAT ('LEASQR: Insufficient degrees of freedom.')
C         CALL MSGWRT (6)
         IERR = 2
         RETURN
      END IF


C  Factorize the matrix.
      DO 120 K = 1, NP
C        Check for a row of zeroes.
         IF (ROWMAX(K).EQ.0.0) GO TO 120

C        A non-zero row maximum implies non-zero diagonal element.
         IF (SC(K,K).EQ.0.0) THEN
C            WRITE (MSGTXT,50) MXS(K)
C            CALL MSGWRT (6)
            GO TO 120
         END IF

C        Decide whether to pivot.
         COLMAX = ABS(SC(K,K))/ROWMAX(K)
         PIVOT = K
         DO 80 I = K+1, NP
            IF (ROWMAX(I).NE.0.0) THEN
               IF (ABS(SC(I,K))/ROWMAX(I).GT.COLMAX) THEN
                  COLMAX = ABS(SC(I,K))/ROWMAX(I)
                  PIVOT = I
               END IF
            END IF
 80      CONTINUE

         IF (PIVOT.GT.K) THEN
C           We must pivot, interchange the rows of the design matrix.
            DO 90 J = 1, NP
               DTEMP = SC(PIVOT,J)
               S(C(PIVOT,J)) = SC(K,J)
               S(C(K,J)) = DTEMP
 90         CONTINUE

C           Don't forget the vector of row maxima.
            DTEMP = ROWMAX(PIVOT)
            ROWMAX(PIVOT) = ROWMAX(K)
            ROWMAX(K) = DTEMP

C           Record the interchange for later use.
            ITEMP = MXS(PIVOT)
            MXS(PIVOT) = MXS(K)
            MXS(K) = ITEMP
         END IF

C        Gaussian elimination.
         DO 110 I = K+1, NP
C           Nothing to do if SC(i,k) is zero.
            IF (SC(I,K).NE.0.0) THEN
C              Save the scaling factor.
               S(C(I,K)) = SC(I,K)/SC(K,K)

C              Subtract rows.
               DO 100 J = K+1, NP
                  S(C(I,J)) = SC(I,J) - SC(I,K)*SC(K,J)
 100           CONTINUE
            END IF
 110     CONTINUE
 120  CONTINUE

C     MXS(i) records which row of M corresponds to row i of SC.
C     SXM(i) records which row of S corresponds to row i of M.
      DO 130 I = 1, NP
         SXM(MXS(I)) = I
 130  CONTINUE


C  Solve the normal equations.
      DO 150 I = 1, NP
C        Forward substitution.
         W(I) = R(MXS(I))
         DO 140 J = 1, I-1
            W(I) = W(I) - SC(I,J)*W(J)
 140     CONTINUE
 150  CONTINUE

      DO 170 I = NP, 1, -1
C        Backward substitution.
         IF (SC(I,I).NE.0.0) THEN
            DO 160 J = I+1, NP
               W(I) = W(I) - SC(I,J)*W(J)
 160        CONTINUE
            W(I) = W(I)/SC(I,I)
         END IF
         X(I) = W(I)
 170  CONTINUE

C     Check that the solution is acceptable.
      RLEN = 0.0
      RESIDU = 0.0
      DO 200 I = 1, NP
         RTEMP = 0.0
         DO 180 J = 1, I-1
            RTEMP = RTEMP + M(J,I)*X(J)
 180     CONTINUE
         DO 190 J = I, NP
            RTEMP = RTEMP + M(I,J)*X(J)
 190     CONTINUE

         RLEN = RLEN + R(I)**2
         RESIDU = RESIDU + (RTEMP - R(I))**2
 200  CONTINUE

      IF (RESIDU.GT.0.001*RLEN) THEN
C         WRITE (MSGTXT,210) RESIDU/RLEN
C 210     FORMAT ('LEASQR: The solution is discrepant at',E8.1)
C         CALL MSGWRT (6)
         RETURN
      END IF


C  Determine goodness-of-fit estimates, and statistical errors.
      SSQRES = SSQ
      DO 220 I = 1, NP
         SSQRES = SSQRES - X(I)*R(I)
 220  CONTINUE
      IF (SSQRES.LT.0.0) SSQRES = 0.0

      VARRES = SSQRES/NF
      VARY = (SSQ - SUM*SUM/N)/(N - 1.0)
      FIT = 1.0
      IF (VARY.NE.0.0) FIT = 1.0 - SSQRES/(SSQ - SUM*SUM/N)

C     Determine the covariance matrix.
      DO 280 K = 1, NP
C        Forward substitution affects only that part of W() below the
C        first non-zero entry.
         DO 230 I = 1, SXM(K)-1
            W(I) = 0.0
 230     CONTINUE
         W(SXM(K)) = 1.0

         DO 250 I = SXM(K)+1, NP
C           Forward substitution.
            W(I) = 0.0
            DO 240 J = SXM(K), I-1
               W(I) = W(I) - SC(I,J)*W(J)
 240        CONTINUE
 250     CONTINUE

         DO 270 I = NP, K, -1
            IF (SC(I,I).NE.0.0) THEN
C              Backward substitution.
               DO 260 J = I+1, NP
                  W(I) = W(I) - SC(I,J)*W(J)
 260           CONTINUE
               W(I) = W(I)/SC(I,I)
            END IF

            IF (I.NE.K) THEN
C              Off diagonal elements of the covariance matrix.
               M(I,K) = VARRES*W(I)
            ELSE IF (I.EQ.K) THEN
C              Diagonal elements of the covariance matrix.
               VX(K)  = VARRES*W(I)
            END IF
 270     CONTINUE
 280  CONTINUE
C                                       Successful completion.
      IERR = 0
      RETURN
      END
C
      SUBROUTINE TABINI (OPCODE, PTYP, VOL, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
C-----------------------------------------------------------------------
C   FARS special version
C   TABINI creates/opens a table extension file.  If a file is created,
C   it is cataloged by a call to CATIO which saves the updated CATBLK.
C   Input:
C      OPCODE   C*4       Operation code, 'READ' => read only,
C                                         'WRIT' => read/write
C      PTYP     I         Physical extension type (eg. 'CC')
C      VOL      I         Disk volume number
C      CNO      I         Catalog slot number
C      CATBLK   I(256)    Catalog block of cataloged file.
C      LUN      I         Logical unit number to use.
C   In/out:
C      VER      I         Version number: (<= 0 => write a new one,
C                         read the latest one), returns one used.
C      NKEY     I         Maximum number of keyword/value pairs
C                         input: used in create, checked on write old
C                         (0 => any, <= actual ok); output: actual
C      NREC     I         Number of logical rec. for create/extend
C                         if 0 on write, reset to 100.
C      NCOL     I         Number of logical columns (does not include
C                         selection column).  Input: used in create,
C                         checked on write old (0=>any, <= actual ok);
C                         output: actual
C      DATP     I(128,2)  DATP(*,1) address pointers (output only)
C                         DATP(*,2) column data type codes. Input:
C                         used in create only; output: actual.
C      BUFFER   I(512)    Work buffer: only 512 now needed
C   Output:
C      IERR     I         Return error code. 0 => OK
C                                        -1 => OK, created new file
C                                         1 => bad input.
C                                         2 => could not find or open
C                                         3 => I/O problem.
C                                         4 => create problem.
C                                         5 => not a table file
C   Usage notes:
C   For sequential access, TABINI leaves pointers for TABIO such that,
C   if IRNO <= 0, reads will begin at the start of the file and writes
C   will begin after the last previous record.  Cataloged file should
C   be marked 'WRIT' if the file is to be created.
C
C   Header record:
C   Each extension file using this system must have the first physical
C   (512 bytes) record containing necessary information. The full table
C   file format is described in Going AIPS.  The user must read this
C   section to understand fully how to use such files.  The header
C   record contains the following:
C
C  I   word(s)          Description
C  1              Number 256-word records now in file
C  2
C  3              Max number rows allowed in current file
C  4
C  5              Number rows (logical records) now in file
C  6
C  7              Number of bytes/value (2 for TA files)
C  8              Number values / logical (# Is / row for TA)
C  9              > 0 => number rows / physical record
C                 < 0 => number physical records / row
C 10              Number logical columns / row
C 11 - 16         Creation date: ZDATE(11), ZTIME(14)
C 17 - 28     H   Physical file name (set on each TABINI call)
C 29 - 30     H   Creation task name
C 31
C 32              Disk number
C 33 - 38         Last access date: ZDATE(33), ZTIME(36)
C 39 - 40     H   Last access task name
C 42              Number logical records to extend file if needed
C 43              Sort order: logical column # of primary sorting
C 44              Sort order: logical column # of secondary sorting
C                      0 => unknown, < 0 => descending order
C 45              Disk record number for column data pointers (2)
C 46              Disk record number for row selection strings (3)
C 47              Disk record number for 1st record of titles (5)
C 48              Disk record number for 1st record of units
C 49              Disk record number for 1st record of keywords
C 50              Disk record number for 1st record of table data
C 51              DATPTR (row selection column)
C 52              Maximum number of keyword/value pairs allowed
C 53              Current number of keyword/value pairs in file
C 54 - 56         "*AIPS TABLE*" packed string to verify that table.
C 57 - 59
C 60              If 1 then then table cannot be written as FITS ASCII
C 61              Number of selection strings now in file
C 62              Next available R   address for a selection string
C 63              First R   address of selection string 1
C 64              First R   address of selection string 2
C 65              First R   address of selection string 3
C 66              First R   address of selection string 4
C 67              First R   address of selection string 5
C 68              First R   address of selection string 6
C 69              First R   address of selection string 7
C 70              First R   address of selection string 8
C********** for TABIO / TABINI use only **********
C 71              IOP : 1 => read, 2 => writ
C 72              Number I   words per logical record
C 73              Current table row physical record in BUFFER
C 74
C 75              Current table row logical record in BUFFER
C 76
C 77              Type of current record in BUFFER
C 78              Current control physical record number in BUFFER
C 79              Current control logical record number in BUFFER
C 80              Type of current control record in BUFFER
C 81              LUN
C 82              FTAB pointer of open file
C***********
C 83 -100         Reserved
C***********
C101 -128     H   Table title
C129 -256         lookup table as COLPTR(logical column) = phys column
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, PTYP*2
      INTEGER   VOL, CNO, VER, CATBLK(256), LUN, NKEY, NREC, NCOL,
     *   DATP(128,2), BUFFER(512), IERR
C
      LOGICAL   EQUAL, OLD, TABLE, EXIST, FITASC, FORGOT
      CHARACTER ATLAB*12, CHTEMP*2, PHNAME*48, OP*4, FNAME*12, FCLASS*6,
     *   FTYPE*2, STATUS*4
      INTEGER   IND, IOP, NEXT, I, J, L, NVER, IP, LP, LREC, NLPR,
     *   IER, JERR, I4T, KREC, ISIZE, LSIZE, FSEQ, FUSID
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA ATLAB /'*AIPS TABLE*'/
C-----------------------------------------------------------------------
      OLD = .TRUE.
      TABLE = .TRUE.
      EXIST = .FALSE.
      IND = 0
C                                       Check OPCODE
      IOP = 0
      IERR = 1
      IF (OPCODE.EQ.'READ') IOP = 1
      IF (OPCODE.EQ.'WRIT') IOP = 2
      IF (IOP.EQ.0) THEN
         WRITE (MSGTXT,1010) OPCODE
         GO TO 990
         END IF
C                                       Check if file exists.
      CALL FXHDEX (CATBLK)
      NEXT = KIEXTN + 1
      IERR = 2
      DO 20 I = 1,KIEXTN
         CALL H2CHR (2, 1, CATBLK(KHEXT+I-1), CHTEMP)
         IF (PTYP.EQ.CHTEMP) THEN
            NEXT = I
            GO TO 50
         ELSE
            IF ((CHTEMP.EQ.' ') .OR. (CATBLK(KHEXT+I-1).EQ.0)) NEXT =
     *         MIN (NEXT, I)
            END IF
 20      CONTINUE
C                                       Catalog block full?
      IF (NEXT.GT.KIEXTN) THEN
         MSGTXT = 'TABINI: EXTENSION FILE LIST FULL'
         GO TO 990
         END IF
C                                       Some old version exists.
 50   CONTINUE
         NVER = CATBLK(KIVER+NEXT-1)
         IF (VER.LE.0) VER = NVER + IOP - 1
C                                       See if requested version exists
         CALL ISTAB (PTYP, VOL, CNO, VER, LUN, BUFFER, TABLE, EXIST,
     *      FITASC, JERR)
         IF (JERR.NE.0) THEN
            IERR = 3
            MSGTXT = 'TABINI: I/O ERROR FROM ISTAB ON ' // PTYP
            GO TO 990
            END IF
C                                       Forgotten version?
C                                       Delete forgotten file if
C                                       writing, keep if reading.
         FORGOT = EXIST .AND. ((VER.GT.NVER) .OR. (JERR.NE.0))
         EXIST = EXIST .AND. (JERR.EQ.0)
         IF (EXIST .AND. ((OPCODE.EQ.'READ') .OR. (.NOT.FORGOT)))
     *      GO TO 140
C                                       None exist: ok on write only
            IF (OPCODE.EQ.'READ') THEN
C                                       add IERR=1. LK March 4, 2011
               IERR = 1
               WRITE (MSGTXT,1060) PTYP, VER
               CALL MSGWRT (6)
               MSGTXT = 'FARS will use the first axis of header'
               GO TO 990
               END IF
C                                       write any ver
C            IF (VER.GT.NVER) VER = NVER + 1
            IF (VER.GT.46655) THEN
               MSGTXT = 'CANNOT CREATE MORE THAN 46655 VERSIONS OF' //
     *            ' AN EXT. FILE'
               GO TO 990
               END IF
C                                       CREATE new file.
C                                       Parse the data structure
      CALL FILL (256, 0, BUFFER)
      CALL FILL (128, 0, DATP)
C                                       Add AIPS table label
      CALL CHR2H (12, ATLAB, 1, BUFFER(54))
      IP = 1
      LP = 1
      DO 120 I = 1,7
         IF (I.EQ.6) GO TO 120
         DO 110 J = 1,NCOL
C                                       Found column of right type
            IF (MOD(DATP(J,2), 10).EQ.I) THEN
C                                       DATP(J,1) = Pointer in array
C                                       of appropriate type.
               DATP(J,1) = IP
               BUFFER(128+J) = LP
               LP = LP + 1
C                                       Get length of array.
               L = DATP(J,2) / 10
               IF (I.EQ.3) L = (L-1) / 4 + 1
               IF (I.EQ.7) L = (L-1) / NBITWD + 1
               IF (DATP(J,2).LT.10) L = 0
C                                       Set pointer for next entry.
               IP = IP + L
C                                       If L>1 and I .NE. 3 then the
C                                       file cannot be written as
C                                       FITS ASCII
               IF ((L.GT.1) .AND. (I.NE.3)) BUFFER(60) = 1
               END IF
 110        CONTINUE
C                                       Set pointer for next type.
         IF (I.EQ.1) IP = (IP-1) * NWDPDP + 1
 120     CONTINUE
C                                       error in data types
      IF (LP.EQ.NCOL+1) GO TO 130
         WRITE (MSGTXT,1120)
         IERR = 1
         GO TO 990
C                                       select column pointers
 130  LREC = 2 * IP
      BUFFER(128+LP) = LP
      DATP(LP,1) = IP
      DATP(LP,2) = 9
C                                       record pointers
      BUFFER(45) = 2
      BUFFER(46) = 3
      BUFFER(47) = 5
      BUFFER(48) = 6 + (NCOL - 1) / (256 / 6)
      BUFFER(49) = BUFFER(48) + 1 + (NCOL - 1) / (256 / 2)
      BUFFER(50) = BUFFER(49) + 1 + (NKEY - 1) / (256 / 5)
      KREC = BUFFER(50) - 1
C                                       file size
      IF ((NREC.LE.0) .OR. (NREC.GT.10000)) NREC = 100
      NLPR = 512.0 / LREC
      IF (NLPR.LE.0) NLPR = -(LREC / 512.0 + 0.9999)
      IF (NLPR.GT.0) ISIZE = KREC + 1 + (NREC-1)/NLPR
      IF (NLPR.LE.0) ISIZE = KREC - NLPR * NREC
      IERR = 4
      CALL ZPHFIL (PTYP, VOL, CNO, VER, PHNAME, IER)
      CALL CHR2H (48, PHNAME, 1, BUFFER(17))
C                                       Delete forgotten old file on
C                                       write.
      IF (FORGOT .AND. (OPCODE.EQ.'WRIT')) THEN
         WRITE (MSGTXT,1125) PTYP, VER
         CALL MSGWRT (6)
         CALL ZDESTR (VOL, PHNAME, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1126) IER, PTYP, VER
            GO TO 990
            END IF
         END IF
C                                       Create file
      CALL ZCREAT (VOL, PHNAME, ISIZE, .FALSE., LSIZE, IER)
      IF (IER.EQ.0) OLD = .FALSE.
      IF (IER.LE.1) GO TO 140
         WRITE (MSGTXT,1130) IER, PTYP, VER
         IF (IER.EQ.5) WRITE (MSGTXT,1131) PTYP, VER, VOL
         GO TO 990
 140  ISIZE = LSIZE
C                                       Catalog ext. file
      IF ((.NOT.OLD) .OR. FORGOT) THEN
C                                       Correct max table number in
C                                       header .
         CALL CHR2H (2, PTYP, 1, CATBLK(KHEXT+NEXT-1))
         CATBLK(KIVER+NEXT-1) = MAX (VER, CATBLK(KIVER+NEXT-1))
         IF (OPCODE.EQ.'WRIT') THEN
            CALL CATIO ('UPDT', VOL, CNO, CATBLK, 'REST', BUFFER(257),
     *         IER)
C                                       File being read; some trickery
C                                       is called for here.
         ELSE
            CALL H2CHR (12, KHIMNO, CATBLK(KHIMN), FNAME)
            CALL H2CHR (6, KHIMCO, CATBLK(KHIMC), FCLASS)
            CALL H2CHR (2, KHPTYO, CATBLK(KHPTY), FTYPE)
            FSEQ = CATBLK(KIIMS)
            FUSID = CATBLK(KIIMU)
            STATUS = 'CLRD'
            CALL CATDIR ('CSTA', VOL, CNO, FNAME, FCLASS, FSEQ, FTYPE,
     *         FUSID, STATUS, BUFFER(257), IER)
            STATUS = 'READ'
            IF (IER.EQ.0)
     *         CALL CATIO ('UPDT', VOL, CNO, CATBLK, STATUS,
     *            BUFFER(257), IER)
            END IF
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1140) IER, PTYP, VER
            GO TO 990
            END IF
         END IF
C                                       OPEN file.
      CALL ZPHFIL (PTYP, VOL, CNO, VER, PHNAME, IER)
      CALL CHR2H (48, PHNAME, 1, BUFFER(17))
      EQUAL = IOP.EQ.2
      IERR = 2
      CALL ZOPEN (LUN, IND, VOL, PHNAME, .FALSE., EQUAL, .TRUE., IER)
      IF (IER.GT.0) THEN
         IND = 0
         WRITE (MSGTXT,1200) IER, PTYP, VER
         GO TO 990
         END IF
C                                       Message about recovering old file
      IF (FORGOT .AND. (OPCODE.EQ.'READ')) THEN
         WRITE (MSGTXT,1201) PTYP, VER
         CALL MSGWRT (6)
         END IF
C                                       If new file, fill in header.
      IF (OLD) GO TO 220
         BUFFER(1) = ISIZE
C                                       determine # log. max in file
         IF (NLPR.GE.0) THEN
            BUFFER(3) = NLPR * (ISIZE - KREC)
         ELSE
            BUFFER(3) = (ISIZE - KREC) / ABS(NLPR)
            END IF
         BUFFER(5) = 0
         CALL CHR2H (6, TSKNAM, 1, BUFFER(29))
         BUFFER(7) = 2
         BUFFER(8) = DATP(NCOL+1,1)
         BUFFER(9) = NLPR
         BUFFER(10) = NCOL
         CALL ZDATE (BUFFER(11))
         CALL ZTIME (BUFFER(14))
         BUFFER(32) = VOL
         BUFFER(42) = NREC
         BUFFER(51) = BUFFER(8)
         LP = 256 / 5
         BUFFER(52) = LP * (BUFFER(50) - BUFFER(49))
         BUFFER(62) = 1
         BUFFER(63) = 1
         I = 28
         CALL RFILL (I, HBLANK, BUFFER(101))
C                                       Write header.
         IERR = 3
         OP = 'WRIT'
         CALL ZFIO ('WRIT', LUN, IND, 1, BUFFER, IER)
         IF (IER.NE.0) GO TO 980
         CALL ZFIO ('WRIT', LUN, IND, BUFFER(45), DATP, IER)
         IF (IER.NE.0) GO TO 980
C                                       write null records
         CALL RFILL (256, HBLANK, BUFFER(257))
         I4T = BUFFER(46)
         IP = BUFFER(50) - BUFFER(46)
         DO 215 I = 1,IP
            CALL ZFIO ('WRIT', LUN, IND, I4T, BUFFER(257), IER)
            IF (IER.NE.0) GO TO 980
            I4T = I4T + 1
 215        CONTINUE
         GO TO 230
C                                       Read header, pointers
 220  CONTINUE
         IERR = 3
         OP = 'READ'
         CALL ZFIO ('READ', LUN, IND, 1, BUFFER, IER)
         IF (IER.NE.0) GO TO 980
C                                       Not table format
         IF (.NOT.TABLE) THEN
            IERR = 5
            MSGTXT = 'TABINI: FILE NOT A TABLE FILE'
            GO TO 990
            END IF
         CALL ZFIO ('READ', LUN, IND, BUFFER(45), DATP, IER)
         IF (IER.NE.0) GO TO 980
         CALL CHR2H (48, PHNAME, 1, BUFFER(17))
C                                       If write - fill info
 230  IF (OPCODE.EQ.'WRIT') THEN
         BUFFER(32) = VOL
         CALL ZDATE (BUFFER(33))
         CALL ZTIME (BUFFER(36))
         CALL CHR2H (6, TSKNAM, 1, BUFFER(39))
         IF ((NREC.LE.0) .OR. (NREC.GT.10000)) NREC = 100
         BUFFER(42) = NREC
C                                       Check structure parms
         IF (OLD) THEN
            IF ((NKEY.GT.BUFFER(52)) .OR. (NCOL.GT.BUFFER(10))) THEN
               WRITE (MSGTXT,1230) NKEY, BUFFER(52), NCOL, BUFFER(10)
               IERR = 1
               GO TO 990
               END IF
            END IF
         END IF
C                                       Check record length.
      NLPR = BUFFER(9)
C                                       return parms
      NKEY = BUFFER(52)
      NCOL = BUFFER(10)
C                                       set up I/O
      BUFFER(71) = IOP
      BUFFER(72) = BUFFER(8)
      BUFFER(73) = 0
      BUFFER(75) = BUFFER(5)
      IF (IOP.EQ.1) BUFFER(75) = 0
      BUFFER(77) = -1
      BUFFER(78) = 0
      BUFFER(79) = 0
      BUFFER(80) = -1
      BUFFER(81) = LUN
      BUFFER(82) = IND
      IERR = 0
      IF (.NOT.OLD) IERR = -1
      GO TO 999
C                                       Error
 980  WRITE (MSGTXT,1980) OP, IER
 990  CALL MSGWRT (6)
      IF (IND.GT.0) CALL ZCLOSE (LUN, IND, IER)
      IF (.NOT.OLD) THEN
         CALL H2CHR (48, 1, BUFFER(17), PHNAME)
         CALL ZDESTR (VOL, PHNAME, IER)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('TABINI: UNKNOWN OPCODE: ',A4)
 1060 FORMAT ('TABINI: REQUESTED ',A2,' FILE ',I3,' DOES NOT EXIST')
 1120 FORMAT ('TABINI: SOME ERROR IN LIST OF TABLE DATA TYPES')
 1125 FORMAT ('TABINI: Deleting old ',A,' table version',I4)
 1126 FORMAT ('TABINI: ERROR ',I3,' DELETING OLD ',A,
     *   ' TABLE VERSION',I4)
 1130 FORMAT ('TABINI: ERROR',I3,' CREATING FILE ',A2,' NO ',I3)
 1131 FORMAT ('TABINI: PROHIBITED FROM CREATING FILE ',A2,' NO ',I3,
     *   ' ON DISK',I3)
 1140 FORMAT ('TABINI: ERROR',I3,' CATALOGING FILE ',A2,' NO ',I3)
 1200 FORMAT ('TABINI: ERROR',I3,' OPENING FILE ',A2,' NO. ',I3)
 1201 FORMAT ('TABINI: Recovering old ',A,' table version',I4)
 1230 FORMAT ('TABINI: KEY COUNTS',2I7,' OR COLS',2I7,' DON''T MATCH')
 1980 FORMAT ('TABINI: ',A4,' ERROR',I5)
      END
