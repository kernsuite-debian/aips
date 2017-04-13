LOCAL INCLUDE 'AFARS.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   HSIZE, FSIZE, HSIZER, FSIZER
      REAL      CELL
      INTEGER   CATI1(256), CATI2(256), CATB(256,2), CATIO(256,3)
      REAL      CATR1(256), CATR2(256)
      HOLLERITH CATH1(256), CATH2(256)
      DOUBLE PRECISION CATD1(128), CATD2(128)
      REAL      IROW(MABFSS,2), ACUT, ABL(2), XN(3), XX(3)
      CHARACTER NAMIN(2)*12, CLSIN(2)*6, NAMOUT(3)*12, CLSOUT(3)*6,
     *   OPCOD*4
      LOGICAL   BFLAG(3), AMPHAS
      INTEGER   IBLC(7,2), ITRC(7,2), NPI(7,2), NPO(7), FBL(2), ILUN(2),
     *   IIND(2), OLUN(3), OIND(3), ISSEQ(2), IDSEQ(3), NAX, NRDIM(2),
     *   CTYPE, IUSER, IPOINT, FCVOL(5), FCCNO(5), HISTDO, USEOLD
      COMMON /FARSCM/ CATB, CATIO, IROW, ACUT, ABL, HSIZE, FSIZE,
     *   HSIZER, FSIZER, CELL, XN, XX, BFLAG, AMPHAS, FBL, IBLC, ITRC,
     *   NPI, NPO, ILUN, IIND, OLUN, OIND, ISSEQ, IDSEQ, NAX, NRDIM,
     *   CTYPE, IUSER, IPOINT, FCVOL, FCCNO, HISTDO, USEOLD
      COMMON /FARSCH/ NAMIN, CLSIN, NAMOUT, CLSOUT, OPCOD
      EQUIVALENCE (CATI1, CATR1, CATD1, CATH1, CATB(1,1))
      EQUIVALENCE (CATI2, CATR2, CATD2, CATH2, CATB(1,2))
      INCLUDE 'INCS:DCAT.INC'
LOCAL END
      PROGRAM AFARS
C-----------------------------------------------------------------------
C! use outputs of FARS to find maximum position and its Amp & Phase.
C# Map-util SPECTRAL POLARIZATION ANALYSIS
C-----------------------------------------------------------------------
C;  Copyright (C) 2011-2013, 2015
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
C   AFARS can be ran after FARS to carry out additional data reduction.
C   Inputs: (from AIPS)
C   INNAME......First image name.     Standard defaults.
C   INCLASS.....First image class.    Standard defaults.
C   INSEQ.......First image seq. #.   0 => highest.
C   INDISK......Disk drive # for the first image.  0 => any.
C   IN2NAME.....Second image name.    Standard defaults.
C   IN2CLASS....Second image class.   Standard defaults.
C   IN2SEQ......Second image seq. #.  0 => highest.
C   IN2DISK.....Disk drive # for the second image.  0 => any.
C   OUT2NAME....Second output image name.     Standard defaults.
C   OUT2CLAS....Second output image class.    Standard defaults.
C   OUT2SEQ.....Second output image seq. #.   0 => highest.
C   OUT2DISK....Second output disk. 0 => highest with space.
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
C   OPCODE......Amplmax or phase to send to the second cube
C   DOHIST.....-2 => copy 1st HI only
C              -3 => no copy of HI
C- uses the FARS.FOR as initial codes !---------------------------------
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   IRET, IERR, IWIN(4), ISWIN(4), NBY, NXI, NYI, IDEPTH(5),
     *   IBLKOF, NBYBUF, ISPOS(2), IDPOS(3), I3, I4, I5, I6, I7, I, IY,
     *   NF, INY
      INCLUDE 'AFARS.INC'
      REAL      XBUFF(MABFSS,3)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGNAM /'AFARS '/
      DATA ISWIN /0,0,0,0/
C-----------------------------------------------------------------------
C                                        Initialize input maps
      IRET = 16
      CALL FARSIN (PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Do-loop limits: window
      NBY = 2
      NBYBUF = MABFSS * NBY
C                                       make a correct value
C                                       corresponded to the actual
C                                       first axis pixels
      NF = NPO(1)
      NXI = CATBLK(KINAX)
      NYI = CATBLK(KINAX+1)
      CALL COPY (7, CATBLK(KINAX), NPO)
      DO 80 I7 = 1,NPO(7)
      DO 80 I6 = 1,NPO(6)
      DO 80 I5 = 1,NPO(5)
      DO 80 I4 = 1,NPO(4)
      DO 80 I3 = 1,NPO(3)
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
         ISWIN(3) = NXI
         ISWIN(4) = NYI
         DO 10 I = 1,3
            CALL MINIT ('WRIT', OLUN(I), OIND(I), NXI, NYI, ISWIN,
     *         XBUFF(1,I), NBYBUF, IBLKOF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1020) I, IERR
               GO TO 980
               END IF
 10         CONTINUE
         DO 60 INY = 1,NYI
            DO 15 I = 1,3
               CALL MDISK ('WRIT', OLUN(I), OIND(I), XBUFF(1,I),
     *            IDPOS(I), IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1040) INY, I, IERR
                  GO TO 980
                  END IF
 15            CONTINUE
C                                        Initialize input maps
            DO 20 I = 1,2
               CALL FILL (5, 1, IDEPTH(1))
               IF (NRDIM(I).GE.3) IDEPTH(1) = INY - 1 + IBLC(3,I)
               IF (NRDIM(I).GE.4) IDEPTH(2) = I3 - 1 + IBLC(4,I)
               IF (NRDIM(I).GE.5) IDEPTH(3) = I4 - 1 + IBLC(5,I)
               IF (NRDIM(I).GE.6) IDEPTH(4) = I5 - 1 + IBLC(6,I)
               IF (NRDIM(I).GE.7) IDEPTH(5) = I6 - 1 + IBLC(7,I)
               CALL COMOFF (NAX, NPI(1,I), IDEPTH, IBLKOF, IERR)
               IF (IERR.NE.0) GO TO 995
               IBLKOF = IBLKOF + 1
               IWIN(1) = IBLC(1,I)
               IWIN(2) = IBLC(2,I)
               IWIN(3) = ITRC(1,I)
               IWIN(4) = ITRC(2,I)
               CALL MINIT ('READ', ILUN(I), IIND(I), NPI(1,I), NPI(2,I),
     *            IWIN, IROW(1,I), NBYBUF, IBLKOF, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1010) I, IERR
                  GO TO 980
                  END IF
 20            CONTINUE
C                                        Loop over rows
            DO 50 IY = 1,NXI
               DO 30 I = 1,2
                  CALL MDISK ('READ', ILUN(I), IIND(I), IROW(1,I),
     *               ISPOS(I), IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1030) IY, I, IERR
                     GO TO 980
                     END IF
 30               CONTINUE
C                                       do real work
               CALL XFARSN (IROW(ISPOS(1),1), IROW(ISPOS(2),2),
     *            XBUFF(IDPOS(1),1), XBUFF(IDPOS(2),2),
     *            XBUFF(IDPOS(3),3))
               IDPOS(1) = IDPOS(1) + 1
               IDPOS(2) = IDPOS(2) + 1
               IDPOS(3) = IDPOS(3) + 1
 50            CONTINUE
 60         CONTINUE
C                                        Write the last buffer
         DO 70 I = 1,3
            CALL MDISK ('FINI', OLUN(I), OIND(I), XBUFF(1,I),
     *         IDPOS(I), IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1060) I, IERR
               GO TO 980
               END IF
 70         CONTINUE
 80      CONTINUE
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
 1010 FORMAT ('COULD NOT INITIALIZE INPUT FILE #',I2,'  IER=',I7)
 1020 FORMAT ('COULD NOT INITIALIZE OUTPUT FILE #',I2,'  IER=',I7)
 1030 FORMAT ('COULD NOT READ LINE',I4,'  MAP #',I2,'  IER=',I7)
 1040 FORMAT ('COULD NOT WRITE LINE',I4,'  MAP #',I2,'  IER=',I7)
 1060 FORMAT ('COULD NOT WRITE LAST LINE, MAP #',I2,'.  IER=',I7)
      END
      SUBROUTINE FARSIN (PRGNAM, IER)
C-----------------------------------------------------------------------
C   FARSIN gets the inputs for AFARS, opens and checks the input images,
C   creates the output image(s), and prepares parameters in common for
C   the later stages of AFARS.
C   Inputs:
C      PRGNAM   C*6   Program name
C   Outputs:
C      IER      I     Error return: 0-->  Okay
C                        3-->  Cannot create and open output file
C                        2-->  Cannot open either input map
C                        1-->  Error in getting input parameters
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   IER
C
      CHARACTER IDCDEF*6, PHNAME*48, MTYPE*2, STOKES*8, CHTMP*8,
     *   CHTMP1*8, UNITS(2)*8
      INTEGER   I, IERR, IRETCD, INPRMS, NB, INC, J, I1, IROUND, K
      LOGICAL   REDUCE, T
      REAL      STOKI(2), STOKO, EPS, X, AXV
      REAL      XSEQ1, XDSK1, XSEQ2, XDSK2, SEQOUT, DISOUT, BPARM(10),
     *   BLC(7), TRC(7), GRIDCR, DOHIST
      HOLLERITH XNAM1(3), XCLS1(2), XNAM2(3), XCLS2(2), XNAMOU(3),
     *   XOPCOD
      DOUBLE PRECISION    DAXV
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'AFARS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      COMMON /INPARM/ XNAM1, XCLS1, XSEQ1, XDSK1, XNAM2, XCLS2, XSEQ2,
     *   XDSK2, XNAMOU, SEQOUT, DISOUT, GRIDCR, BLC, TRC, XOPCOD, BPARM,
     *   DOHIST
      DATA EPS /0.2/
      DATA T /.TRUE./
      DATA STOKES /'STOKES  '/
C-----------------------------------------------------------------------
C                                        Initialize file and header I/O
      ILUN(1) = 17
      ILUN(2) = 18
      OLUN(1) = 21
      OLUN(2) = 22
      OLUN(3) = 23
C
      CALL ZDCHIN (T)
      CALL VHDRIN
      NSCR = 0
      NCFILE = 0
      IER = 0
C                                        Get inputs from AIPS
      INPRMS = 46
      CALL GTPARM (PRGNAM, INPRMS, RQUICK, XNAM1, IROW, IERR)
      IRETCD = IERR
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (7)
         END IF
      IF (RQUICK) CALL RELPOP (IRETCD, IROW, IERR)
      IF (IRETCD.NE.0) GO TO 980
C                                       Change input OPCODE - char
      HISTDO = IROUND (DOHIST)
      CALL H2CHR (4, 1, XOPCOD, OPCOD)
      CTYPE = 1
      ACUT = BPARM(1)
      ACUT = MAX (0.0, ACUT)
      IF (ABS(BPARM(2)+1111.0).LT.0.5) BPARM(2) = FBLANK
      IF (ABS(BPARM(3)+1111.0).LT.0.5) BPARM(3) = FBLANK
      ABL(1) = BPARM(2)
      ABL(2) = BPARM(3)
C                                        Transfer inputs into integer
C                                        variables
      FCVOL(1) = IROUND (XDSK1)
      FCVOL(2) = IROUND (XDSK2)
      FCVOL(3) = IROUND (DISOUT)
      FCVOL(4) = IROUND (DISOUT)
      FCVOL(5) = IROUND (DISOUT)
      ISSEQ(1) = IROUND (XSEQ1)
      ISSEQ(2) = IROUND (XSEQ2)
      IDSEQ(1) = IROUND (SEQOUT)
      IDSEQ(2) = IROUND (SEQOUT)
      IDSEQ(3) = IROUND (SEQOUT)
      IUSER = NLUSER
      USEOLD = IROUND (BPARM(5))
C                                       Hollerith -> Char.
      CALL H2CHR (12, 1, XNAM1, NAMIN(1))
      CALL H2CHR (6, 1, XCLS1, CLSIN(1))
      CALL H2CHR (12, 1, XNAM2, NAMIN(2))
      CALL H2CHR (6, 1, XCLS2, CLSIN(2))
C
      CALL H2CHR (12, 1, XNAMOU, NAMOUT(1))
      NAMOUT(2) = NAMOUT(1)
      NAMOUT(3) = NAMOUT(1)
      CLSOUT(1) = ' '
      CLSOUT(2) = ' '
      CLSOUT(3) = ' '
C                                        Open both input maps
      MTYPE = 'MA'
C
      DO 35 I = 1,2
         CALL MAPOPN ('READ', FCVOL(I), NAMIN(I), CLSIN(I), ISSEQ(I),
     *      MTYPE, IUSER, ILUN(I), IIND(I), FCCNO(I), CATBLK,
     *      IROW(1,I), IERR)
         CALL COPY (256, CATBLK, CATB(1,I))
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1025) I,IERR
            CALL MSGWRT (7)
            GO TO 960
            END IF
         NCFILE = NCFILE + 1
         FRW(NCFILE) = 0
         FVOL(NCFILE) = FCVOL(I)
         FCNO(NCFILE) = FCCNO(I)
         CALL H2CHR (8, 1, CATH(KHBUN), UNITS(I))
         NB = CATBLK(KIDIM)
         CALL FILL (KICTPN, 1, NPI(1,I))
         CALL COPY (NB, CATBLK(KINAX), NPI(1,I))
         NRDIM(I) = NB
         DO 30 J = 1,NB
            NPI(J,I) = MAX (1, NPI(J,I))
            IF (NPI(J,I).LE.1) NRDIM(I) = NRDIM(I) - 1
 30         CONTINUE
 35      CONTINUE
      AMPHAS = UNITS(1).NE.UNITS(2)
C                                        Set up some needed header vals
      REDUCE = .FALSE.
      NAX = CATI1(KIDIM)
      CALL WINDOW (NAX, NPI(1,1), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 960
      DO 40 I = 1,7
         IBLC(I,1) = BLC(I) + EPS
         ITRC(I,1) = TRC(I) + EPS
         NPO(I) = ITRC(I,1) - IBLC(I,1) + 1
 40      CONTINUE
C                                       Are input maps coincident?
C                                       Set 2nd map corners
      INC = 2
      K = 2
      CALL COPY (256, CATB(1,K), CATBLK)
      DO 45 I = 1,7
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
               IF (CATR(KRCIC+J).EQ.0.0) GO TO 55
               AXV = (DAXV - CATD(KDCRV+J)) / CATR(KRCIC+J) +
     *            CATR(KRCRP+J)
               END IF
            IBLC(I,K) = IROUND (AXV)
            IF ((GRIDCR.GE.0.1) .AND. (ABS(AXV-IBLC(I,K)).GT.EPS))
     *         GO TO 55
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
            IF (IBLC(I,1).GT.ITRC(I,1)) GO TO 55
            NPO(I) = ITRC(I,1) - IBLC(I,1) + 1
C                                        Check coincidence
            IF (GRIDCR.GE.0.1) THEN
               IPOINT = KHCTP+J*INC
               CALL H2CHR (8, 1, CATH(IPOINT), CHTMP)
               CALL H2CHR (8, 1, CATH1(IPOINT), CHTMP1)
               IF (CHTMP.NE.CHTMP1) GO TO 55
               X = EPS * EPS * ABS(CATR1(KRCIC+J))
               IF (ABS(CATR(KRCIC+J)-CATR1(KRCIC+J)).GT.X)
     *            GO TO 55
               IF (ABS(CATR(KRCRT+J)-CATR1(KRCRT+J)).GT.1.)
     *            GO TO 55
               END IF
            END IF
 45      CONTINUE
      GO TO 60
C                                        Maps not coincident
 55   WRITE (MSGTXT,1075) I
      CALL MSGWRT (7)
      GO TO 960
C                                       Create output map
 60   MSGTXT = 'FARSIN: input maps coincident on reduced subimage only'
      IF (REDUCE) CALL MSGWRT (6)
      XX(1) = -1.0E30
      XX(2) = -1.0E30
      XX(3) = -1.0E30
      XN(1) = 1.0E30
      XN(2) = 1.0E30
      XN(3) = 1.0E30
      BFLAG(1) = .FALSE.
      BFLAG(2) = .FALSE.
      BFLAG(3) = .FALSE.
      CALL COPY (256, CATI1, CATBLK)
      INC = 2
      STOKO = -1
      DO 85 I = 1,NAX
         IPOINT = KHCTP+(I-1)*INC
         CALL H2CHR (8, 1, CATH(IPOINT), CHTMP)
         IF (CHTMP.EQ.STOKES) GO TO 95
 85      CONTINUE
C                                        No Stokes axis
      STOKI(1) = -10
      GO TO 110
 95   I1 = KDCRV + I - 1
      STOKI(1) = CATD(I1) + CATR(KRCIC+I-1) * (IBLC(I,1) -
     *   CATR(KRCRP+I-1))
      STOKI(2) = CATD2(I1) + CATR2(KRCIC+I-1) * (IBLC(I,2) -
     *   CATR2(KRCRP+I-1))
C                                        Various cases
      IF (STOKI(1).EQ.STOKI(2)) STOKO = STOKI(1)
C
      IF (NPO(I).LE.1) THEN
         CATD(I1) = STOKO
         CATR(KRCRP+I-1) = 1.0
         END IF
C                                        Default out class
 110  IDCDEF = 'AFARrm'
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
C                                       CELL and FSIZE from the header
      CELL = CATR(KRCIC)
      FSIZE = CATBLK(KINAX)
C                                       FSIZE is an odd number
      HSIZE = FSIZE/2
C                                        move axes 2-7 to 1-6
      J = 0
      K = CATBLK(KIDIM) - 1
      DO 115 I = 1,K
         CATBLK(KINAX+J) = MAX (1, CATBLK(KINAX+I))
         CATD(KDCRV+J) = CATD(KDCRV+I)
         CATR(KRCRP+J) = CATR(KRCRP+I)
         CATR(KRCIC+J) = CATR(KRCIC+I)
         CATR(KRCRT+J) = CATR(KRCRT+I)
         CATH(KHCTP+2*J) = CATH(KHCTP+2*I)
         CATH(KHCTP+2*J+1) = CATH(KHCTP+2*I+1)
         CALL H2CHR (8, 1, CATH(KHCTP+2*J), CHTMP)
         IF (CHTMP.NE.STOKES) J = J + 1
 115     CONTINUE
      CATBLK(KIDIM) = J
      DO 116 K = 0,6
         IF (K.LT.J) THEN
            CATBLK(KINAX+K) = MAX (1, CATBLK(KINAX+K))
         ELSE
            CATBLK(KINAX+K) = 1
            END IF
 116     CONTINUE
C                                        Put in map units
      CALL MCREAT (FCVOL(3), FCCNO(3), IROW, IERR)
      IDSEQ(1) = CATI1(KIIMS)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1110) IERR
         CALL MSGWRT (8)
         GO TO 950
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
C                                        Open the output file
      CALL ZPHFIL ('MA', FCVOL(3), FCCNO(3), 1, PHNAME, IERR)
      CALL ZOPEN (OLUN(1), OIND(1), FCVOL(3), PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1136) IERR
         CALL MSGWRT (7)
         GO TO 950
         END IF
      CALL COPY (256, CATBLK, CATIO(1,1))
C                                       output map 2
      IDCDEF = 'AFARam'
      IF (OPCOD.EQ.'CMPL') IDCDEF = 'AFARre'
      CALL MAKOUT (NAMIN(1), CLSIN(1), ISSEQ(1), IDCDEF, NAMOUT(2),
     *   CLSOUT(2), IDSEQ(2))
      CALL CHR2H (12, NAMOUT(2), KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLSOUT(2), KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = IDSEQ(2)
      CATBLK(KIIMU) = NLUSER
      CALL CHR2H (8, 'JY/BEAM ', 1, CATH(KHBUN))
      CALL MCREAT (FCVOL(4), FCCNO(4), IROW, IERR)
      IDSEQ(2) = CATI1(KIIMS)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1110) IERR
         CALL MSGWRT (8)
         GO TO 950
         END IF
      NCFILE = NCFILE + 1
      FRW(NCFILE) = 2
      FVOL(NCFILE) = FCVOL(4)
      FCNO(NCFILE) = FCCNO(4)
C                                       Copy header keywords from first
C                                       input.
      CALL KEYPCP (FCVOL(1), FCCNO(1), FCVOL(4), FCCNO(4), 0, ' ',
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1135) IERR
         CALL MSGWRT (7)
         END IF
C                                        Open the output file
      CALL ZPHFIL ('MA', FCVOL(4), FCCNO(4), 1, PHNAME, IERR)
      CALL ZOPEN (OLUN(2), OIND(2), FCVOL(4), PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1136) IERR
         CALL MSGWRT (7)
         GO TO 950
         END IF
      CALL COPY (256, CATBLK, CATIO(1,2))
C                                       output map 3
      IDCDEF = 'AFARph'
      IF (OPCOD.EQ.'CMPL') IDCDEF = 'AFARim'
      CALL MAKOUT (NAMIN(1), CLSIN(1), ISSEQ(1), IDCDEF, NAMOUT(3),
     *   CLSOUT(3), IDSEQ(3))
      CALL CHR2H (12, NAMOUT(3), KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLSOUT(3), KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = IDSEQ(3)
      CATBLK(KIIMU) = NLUSER
C
      IF (OPCOD.NE.'CMPL') THEN
         CALL CHR2H (8, 'DEGREES ', 1, CATH(KHBUN))
      ELSE
         CALL CHR2H (8, 'JY/BEAM ', 1, CATH(KHBUN))
         END IF
      CALL MCREAT (FCVOL(5), FCCNO(5), IROW, IERR)
      IDSEQ(3) = CATI1(KIIMS)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1110) IERR
         CALL MSGWRT (8)
         GO TO 950
         END IF
      NCFILE = NCFILE + 1
      FRW(NCFILE) = 2
      FVOL(NCFILE) = FCVOL(5)
      FCNO(NCFILE) = FCCNO(5)
C                                       Copy header keywords from first
C                                       input.
      CALL KEYPCP (FCVOL(1), FCCNO(1), FCVOL(5), FCCNO(5), 0, ' ',
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1135) IERR
         CALL MSGWRT (7)
         IERR = 0
         END IF
C                                        Open the output file
      CALL ZPHFIL ('MA', FCVOL(5), FCCNO(5), 1, PHNAME, IERR)
      CALL ZOPEN (OLUN(3), OIND(3), FCVOL(5), PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1136) IERR
         CALL MSGWRT (7)
         GO TO 950
         END IF
      CALL COPY (256, CATBLK, CATIO(1,3))
C
      GO TO 999
C-----------------------------------------------------------------------
C                                        Error returns
C                                        Failed to create output file
 950  IER = IER + 1
C                                        Error in MAPOPN
 960  IER = IER + 1
C                                        Error in getting parameters
 980  IER = IER + 1
C
      WRITE (MSGTXT,1900) IERR
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('FARSIN: COULD NOT GET PARMS.  IER=',I7)
 1025 FORMAT ('FARSIN: COULD NOT OPEN MAP # ',I1,'  IER=',I7)
 1075 FORMAT ('FARSIN: INPUT MAPS ARE NOT COINCIDENT ON AXIS',I2)
 1110 FORMAT ('FARSIN: COULD NOT CREATE OUTPUT MAP.  IER=',I7)
 1135 FORMAT ('FARSIN: ERROR ', I3,' COPYING KEYWORDS - CONTINUING')
 1136 FORMAT ('FARSIN: COULD NOT OPEN OUTPUT MAP.  IER=',I7)
 1900 FORMAT ('TABIO: ERROR = ', I3)
      END
      SUBROUTINE XFARSN (V1, V2, R1, R2, R3)
C-----------------------------------------------------------------------
C   XFARSN combines two pixel rows V1 and V2 with noise rows S1 and S2
C   to form result row R1 and noise row R2.  The algorithm is specified
C   by FARS and any parameters are stored in the array C.
C   Inputs:
C      V1     R(*)   The pixel values of the input map 1 * = FSIZE
C      V2     R(*)   The pixel values of the input map 2
C   Outputs:
C      R1     R      The position of the maximum
C      R2     R      The amplitude or real at that point
C      R3     R      The phase or imaginary at that point
C-----------------------------------------------------------------------
      REAL      V1(*), V2(*), R1, R2, R3
C
      INTEGER   K, KMAXAM, NEQUAT, NFIT, IPOIN, IRET, IP1, IP2
      REAL      PHASE, AMP, RMAXAM, POSMAX, FUNC(25), ARGUM(25),
     *   FITPAR(3), VARRES, AMPMAX, TREAL, TIMAG
C
      INCLUDE 'AFARS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IF (USEOLD.LE.0) THEN
         IP1 = 2
         IP2 = 4
      ELSE
         IP1 = 1
         IP2 = 5
         END IF
C                                       read the input files of
C                                       amplitue and phase
      AMPMAX = 1.0E-20
      KMAXAM = -1
      IRET = 0
      DO 10 K = 1,FSIZE
         IF (V1(K).NE.FBLANK) THEN
            IF (AMPHAS) THEN
               AMP = V1(K)
            ELSE
               AMP = SQRT (V1(K)*V1(K) + V2(K)*V2(K))
               END IF
C                                       Find position of maximum of
C                                       amplitude
            IF (AMP.GT.AMPMAX) THEN
               AMPMAX = AMP
               KMAXAM = K
               END IF
            END IF
 10      CONTINUE
      IF (KMAXAM.LE.0) THEN
         POSMAX = ABL(1)
         PHASE = ABL(2)
         TREAL = 0.0
         TIMAG = 0.0
      ELSE
         IF (AMPHAS) THEN
            PHASE = V2(KMAXAM)
            TREAL = AMPMAX * COS (DG2RAD * PHASE)
            TIMAG = AMPMAX * SIN (DG2RAD * PHASE)
         ELSE
            PHASE = RAD2DG * ATAN2 (V2(KMAXAM), V1(KMAXAM))
            TREAL = V1(KMAXAM)
            TIMAG = V2(KMAXAM)
            END IF
C
C                                       make more precise position of
C                                       the maximum at amplitude, only
C
C                                       take 5 (+-2) points near maximum
         NEQUAT = 5
         NFIT = 3
         DO 20 IPOIN = IP1,IP2
            K = KMAXAM + IPOIN - 3
C                                       K is outside of range
            IF ((K.LT.1) .OR. (K.GT.FSIZE)) IRET = 1
C
            ARGUM(IPOIN) = IPOIN - 3
            IF (V1(K).EQ.FBLANK) THEN
               NFIT = 0
            ELSE
               IF (AMPHAS) THEN
                  FUNC(IPOIN) = V1(K)
               ELSE
                  FUNC(IPOIN) = SQRT (V1(K)*V1(K) + V2(K)*V2(K))
                  END IF
               END IF
 20         CONTINUE
C                                       find the three coefficients of
C                                       parabolla by least square
C                                       Y = A*X*X + B*X +C
C                                       C= FITPAR(1), B=FITPAR(2)
C                                       A = FITPAR(3)
         RMAXAM = 0.0
         IF ((NFIT.EQ.3) .AND. (IRET.EQ.0) .AND. (USEOLD.LE.1)) THEN
            IF (USEOLD.LE.0) THEN
               CALL QCUBE (FUNC(2), RMAXAM, AMPMAX)
               TREAL = AMPMAX * COS (DG2RAD * PHASE)
               TIMAG = AMPMAX * SIN (DG2RAD * PHASE)
            ELSE
               CALL SOLIN (ARGUM, FUNC, NFIT, NEQUAT, FITPAR, VARRES,
     *            IRET)
C                                       the more precise max position
               IF (IRET.EQ.0) THEN
                  RMAXAM = -FITPAR(2) / (2*FITPAR(3))
                  IF (ABS(RMAXAM).GT.1.0) THEN
                     RMAXAM = 0.0
                  ELSE
                     AMPMAX = FITPAR(1) - FITPAR(2)*FITPAR(2)/4.0
     *                  /FITPAR(3)
                     TREAL = AMPMAX * COS (DG2RAD * PHASE)
                     TIMAG = AMPMAX * SIN (DG2RAD * PHASE)
                     END IF
                  END IF
               END IF
            END IF
         RMAXAM = RMAXAM + KMAXAM + IBLC(1,1) - 1
C                                       max position in FARADROT (1/l^2)
C                                       Dec 30: Position of maximum at
C                                       cells relatively the center of
C                                       the input data  NPI(1,1)/2 + 1
C      POSMAX = (RMAXAM - NPI(1,1)/2 - 1) * CELL
C                                       correct coordinate computation
C                                       drop assumptions.
         POSMAX = (RMAXAM - CATR1(KRCRP)) * CELL + CATD1(KDCRV)
C                                       change the value of POSMAX
C                                       if AMPL is considered small
         IF (AMPMAX.LE.ACUT) THEN
            POSMAX = ABL(1)
            PHASE = ABL(2)
            END IF
         END IF
C                                       position (RM) of maximum
      R1 = POSMAX
      IF (OPCOD.EQ.'CMPL') THEN
         R2 = TREAL
         R3 = TIMAG
      ELSE
         R2 = AMPMAX
         R3 = PHASE
         END IF
C                                       Get max/min
      IF (R1.NE.FBLANK) THEN
         XX(1) = MAX (XX(1), R1)
         XN(1) = MIN (XN(1), R1)
      ELSE
         BFLAG(1) = .TRUE.
         END IF
      IF (R2.NE.FBLANK) THEN
         XX(2) = MAX (XX(2), R2)
         XN(2) = MIN (XN(2), R2)
      ELSE
         BFLAG(2) = .TRUE.
         END IF
      IF (R3.NE.FBLANK) THEN
         XX(3) = MAX (XX(3), R3)
         XN(3) = MIN (XN(3), R3)
      ELSE
         BFLAG(3) = .TRUE.
         END IF
C
 999  RETURN
      END
      SUBROUTINE FARSHI (IRET)
C-----------------------------------------------------------------------
C   FARSHI creates and writes the HI file associated with task AFARS.
C   Outputs:
C      IRET   I      > 0 => output all blanks
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER HILINE*72, FTYPE(5)*10, NOTTYP*2, CTIME*8, CDATE*12
      INTEGER   NHISTF, IHDLUN, IHSLUN, I, IER, IERR, IBUFF1(256), J,
     *    IBUFF2(256), IPTR, ITEMP, FU, TIME(3), DATE(3)
      INCLUDE 'AFARS.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (IBUFF1(1),IROW(1,1)),  (IBUFF2(1),IROW(257,1))
      DATA NHISTF, IHDLUN, IHSLUN /2,27,28/
      DATA FTYPE /'RotMeasure','Amplitude', 'Phase','Real','Imaginary'/
      DATA NOTTYP /'CC'/
C-----------------------------------------------------------------------
C                                       Initialize HITAB
      CALL HIINIT (NHISTF)
      IRET = 0
      DO 200 J = 3,1,-1
         FU = J
         IF ((OPCOD.EQ.'CMPL') .AND. (J.GT.1)) FU = FU + 2
C                                       Test validity of result
         IF (XX(J).LT.XN(J)) THEN
            WRITE (MSGTXT,1000) FTYPE(FU)
            CALL MSGWRT (6)
            BFLAG(J) = .TRUE.
         ELSE IF (XX(J).EQ.XN(J)) THEN
            WRITE (MSGTXT,1001) FTYPE(FU), XX(J)
            CALL MSGWRT (6)
            END IF
C                                       Insert header parameters
         CALL COPY (256, CATIO(1,J), CATBLK)
         CATR(KRDMN) = XN(J)
         CATR(KRDMX) = XX(J)
C                                        Put in inhibited pixel value
         CATR(KRBLK) = 0.0
         IF (BFLAG(J)) CATR(KRBLK) = FBLANK
         IF (J.EQ.1) CALL CHR2H (8, 'RAD/M/M ', 1, CATH(KHBUN))
C                                       Create and open new HI file
         CALL HICREA (IHDLUN, FCVOL(2+J), FCCNO(2+J), CATBLK, IBUFF2,
     *      IERR)
         IPTR = IBUFF2(3)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) FTYPE(J), IERR
            CALL MSGWRT (7)
            GO TO 200
            END IF
C                                       Copy both input HI files
         IF (HISTDO.GE.-2) THEN
            I = 1
            WRITE (HILINE,1020) I
            CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            ITEMP = HITAB(IPTR+2)
            CALL HIOPEN (IHSLUN, FCVOL(I), FCCNO(I), IBUFF1, IERR)
            IF (IERR.NE.0) GO TO 30
            CALL HICOPY (IHSLUN, IBUFF1, IHDLUN, IBUFF2, IERR)
            CALL HICLOS (IHSLUN, .FALSE., IBUFF1, IER)
            IF (IERR.EQ.0) GO TO 50
            IF (IERR.GE.100) HITAB(IPTR+2) = ITEMP
 30            WRITE (MSGTXT,1030) I, IERR
               CALL MSGWRT (6)
               WRITE (HILINE,1031)
               CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
               IF (IERR.NE.0) GO TO 190
 50         I = 2
            WRITE (HILINE,1032) TSKNAM, I
            CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            END IF
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
         WRITE (HILINE,1097) TSKNAM, ACUT
         CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         IF (J.EQ.1) THEN
            WRITE (HILINE,1098) TSKNAM, ABL(1)
            IF (ABL(1).EQ.FBLANK) WRITE (HILINE,1100) TSKNAM
            CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
         ELSE IF (FU.EQ.3) THEN
            WRITE (HILINE,1099) TSKNAM, ABL(2)
            IF (ABL(1).EQ.FBLANK) WRITE (HILINE,1100) TSKNAM
            CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            END IF
C                                       Close HI file
 190     CALL HICLOS (IHDLUN, .TRUE., IBUFF2, IER)
         WRITE (MSGTXT,1900) FTYPE(FU)
         IF ((IER.NE.0) .OR. (IERR.NE.0)) WRITE (MSGTXT,1901)
     *      FTYPE(J), IERR, IER
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
     *      OIND(J), CATBLK, .TRUE., IBUFF2, IERR)
         NCFILE = NCFILE - 1
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1910) FTYPE(J), IERR
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
 1097 FORMAT (A6,'BPARM(1)=',1PE13.5,' / clip level')
 1098 FORMAT (A6,'BPARM(2)=',F12.5,' / RM value when clipped')
 1099 FORMAT (A6,'BPARM(3)=',F12.5,' / phase value when clipped')
 1100 FORMAT (A6,'  / Magic value blanks used when image clipped')
 1900 FORMAT ('History file created and written for ',A,' file')
 1901 FORMAT ('HISTORY FILE FOR ',A,' INCOMPLETE.  IERRS =',2I7)
 1910 FORMAT ('WARNING FARSHI COULD NOT CLOSE OUTPUT ',A,
     *   ' FILE.  IER=',I7)
      END
      SUBROUTINE QCUBE (D, X, DM)
C-----------------------------------------------------------------------
C   Does an exact cubic fit to 3 values from consecutive pixels
C   Inputs
C      D        R(3)   values to fit (at x = -1,0,1)
C   Outputs
C      X    R   X position of maximum
C      DM   R   Maximum D
C-----------------------------------------------------------------------
      REAL      D(3), X, DM
C
      REAL      FITPAR(3), P
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      X = 0.0
      DM = D(2)
      IF ((D(1).NE.FBLANK) .AND. (D(2).NE.FBLANK) .AND.
     *   (D(3).NE.FBLANK)) THEN
         FITPAR(1) = D(2)
         FITPAR(2) = (D(3) - D(1)) / 2.0
         FITPAR(3) = (D(3) + D(1)) / 2.0 - D(2)
         IF (FITPAR(3).NE.0.0) THEN
            P = -FITPAR(2) / (2.0 * FITPAR(3))
            IF (ABS(P).LT.1.0) THEN
               X = P
               DM = FITPAR(1) + FITPAR(2) * X + FITPAR(3) * X * X
               END IF
            END IF
         END IF
C
 999  RETURN
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
C
  999 RETURN
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
 10      CONTINUE
         DO 20 J = I, NP
            S(C(I,J)) = M(I,J)
 20      CONTINUE

C        Find the maximum absolute element in each row.
         ROWMAX(I) = 0.0
         DO 30 J = 1, NP
            ROWMAX(I) = MAX(ROWMAX(I), ABS(SC(I,J)))
 30      CONTINUE
 40   CONTINUE

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
