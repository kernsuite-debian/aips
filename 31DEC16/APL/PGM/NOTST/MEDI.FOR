LOCAL INCLUDE 'MEDI.INC'
      INTEGER   CATI1(256), CATI2(256), CATI3(256), CATI4(256),
     *   CATB(256,4), CATIO(256,2)
      REAL      CATR1(256), CATR2(256), CATR3(256), CATR4(256)
      HOLLERITH CATH1(256), CATH2(256), CATH3(256), CATH4(256)
      DOUBLE PRECISION CATD1(128),  CATD2(128), CATD3(128), CATD4(128)
      REAL      IROW(8192,4), FREQ1, FREQ2, LNFREQ, FMIN,
     *   FMAX, CPARM(20), XN(2), XX(2)
      CHARACTER NAMIN(4)*12, CLSIN(4)*6, NAMOUT(2)*12, CLSOUT(2)*6,
     *   ALGO*4
      LOGICAL   XBLFLG, BFLAG, DOFILE(6), PLAIN
      INTEGER   IBLC(7,4), ITRC(7,4), NPI(7,4), NPO(7), FBL(2), ILUN(4),
     *   IIND(4), OLUN(2), OIND(2), ISSEQ(4), IDSEQ(2), NAX, NRDIM(4),
     *   CTYPE, IUSER, IPOINT, FCVOL(6), FCCNO(6)
      COMMON /MEDICM/ CATB, CATIO, IROW, FREQ1, FREQ2, LNFREQ,
     *   FMIN, FMAX, CPARM, XN, XX, XBLFLG, BFLAG, FBL, IBLC, ITRC,
     *   NPI, NPO, ILUN, IIND, OLUN, OIND, ISSEQ, IDSEQ, NAX, NRDIM,
     *   CTYPE, IUSER, IPOINT, DOFILE, PLAIN, FCVOL, FCCNO
      COMMON /MEDICH/ NAMIN, CLSIN, NAMOUT, CLSOUT, ALGO
      EQUIVALENCE (CATI1, CATR1, CATD1, CATH1, CATB(1,1))
      EQUIVALENCE (CATI2, CATR2, CATD2, CATH2, CATB(1,2))
      EQUIVALENCE (CATI3, CATR3, CATD3, CATH3, CATB(1,3))
      EQUIVALENCE (CATI4, CATR4, CATD4, CATH4, CATB(1,4))
      INCLUDEW 'INCS:DCAT.INC'
      INTEGER IMEDIA, IGNORE, NOISEV, CLIPMN, CLIPMX, ISNR, NOISEO
      PARAMETER (IMEDIA=1, IGNORE=-999, NOISEV=11, CLIPMN=9, CLIPMX=10)
      PARAMETER (ISNR=15, NOISEO=13)
LOCAL END
      PROGRAM MEDI
C-----------------------------------------------------------------------
C! Combine four input maps by median filtering to make output map.
C# Map-util SPECTRAL POLARIZATION ANALYSIS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2009-2010, 2015
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
C  MEDI is a task in which four input images are combined on
C  a pixel by pixel level to produce an output image and an optional
C  deviation image.   The MEDIan, like the AVERAGE, is used to estimate
C  the most likely (true) value of an observable.  The average is a
C  more efficient estimator of a value when the noise of the measurement
C  is gaussian distributed with zero mean.   However in the presence of
C  outliers, the MEDIAN is a more efficient estimator.
C
C  MEDIAN Details:  In the case of one or two un-blanked pixel values,
C  the MEDIAN is the average of the un-blanked values.  The deviation
C  image pixel is blanked.  In the case of 3 un-blanked pixel values,
C  The middle of the three values is the median, and the deviation
C  image contains the average of the high and low pixels, minus the
C  middle pixel.   In the case of 4 un-blanked input values, the median
C  is the average of the middle two values and the deviation image is
C  the average of the high and low values, minus the average of the
C  middle two values.
C
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
C   IN3NAME.....First noise image name.     Standard defaults.
C   IN3CLASS....First noise image class.    Standard defaults.
C   IN3SEQ......First noise image seq. #.   0 => highest.
C   IN3DISK.....Disk # for first noise image.  0 => any.
C   IN4NAME.....Second noise image name.    Standard defaults.
C   IN4CLASS....Second noise image class.   Standard defaults.
C   IN4SEQ......Second noise image seq. #.  0 => highest.
C   IN4DISK.....Disk # for second noise image.  0 => any.
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
C   OUTCLASS....Output image class.   Standard behavior with default =
C               either the output STOKES in string form or the OPCODE if
C               the output STOKES is the same as the first input image.
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
C   APARM.......Parameters needed for algorithm:
C      APARM(1), APARM(2), APARM(3), APARM(4) used to scale images
C           APARM(1) = 0 => APARM(1) = 1.0
C           APARM(2) = 0 => APARM(2) = 1.0
C           APARM(3) = 0 => APARM(3) = 1.0
C           APARM(4) = 0 => APARM(4) = 1.0
C      APARM(8) >  0  => Use 0.0 for clipped & illegal values
C               <= 0  => Use blanking for clipped & illegal values
C      APARM(9)  = Clip if MAP(I) < APARM(9),  image units.
C      APARM(10) = Clip if MAP(I) > APARM(10), image units.
C           0 means no clippng.
C   BPARM.......Parameters needed noise calculation and control:
C      BPARM(1) = 1-sigma level on input maps.
C                 0 => ignore noise
C      BPARM(2) NOT USED
C      BPARM(3) = false (<= 0) => output normal image
C               = true  (>  0) => output normal and sigma image
C                 Blanking is the same for both settings of B(3).
C      BPARM(4) <= 0.5  => Blank output map using input map values
C               else    => Blank output map using output map sigma
C               >= 1.5  => Blank output map using output noise image
C      BPARM(5) = Error on output map value above which output
C                 pixel is blanked (if BPARM(4) = 1) 0 -> FMIN
C               = S/N ratio of output map value below which output
C                 pixel is blanked (if BPARM(4) = 2) 0 -> 0.05
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   IRET, IERR, IWIN(4), ISWIN(4), NBY, NX, NY, IDEPTH(5),
     *   IBLKOF, NBYBUF, ISPOS(4), IDPOS(2), I3, I4, I5, I6, I7, I, IY
      REAL      XBUFF(8192,2)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'MEDI.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGNAM /'MEDI  '/
      DATA ISWIN /0,0,0,0/
C-----------------------------------------------------------------------
C                                        Initialize input maps
      IRET = 16
      CALL MEDIIN (PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL PMEDIN (CPARM, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL FILL (4, 1, ISPOS)
      CALL FILL (2, 1, IDPOS)
C                                        Do-loop limits: window
      NBY = 2
      NBYBUF = 8192 * NBY
      NX = NPO(1)
      NY = NPO(2)
      DO 70 I7 = 1,NPO(7)
      DO 70 I6 = 1,NPO(6)
      DO 70 I5 = 1,NPO(5)
      DO 70 I4 = 1,NPO(4)
      DO 70 I3 = 1,NPO(3)
C                                        Initialize input maps
         DO 10 I = 1,4
            IF (DOFILE(I)) THEN
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
     *            IWIN, IROW(1,I), NBYBUF, IBLKOF, IERR)
C                                        If error reading file, ignore
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1010) I, IERR
                  DOFILE(I) = .FALSE.
                  END IF
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
         DO 20 I = 1,2
            IF (DOFILE(I+4)) THEN
               CALL MINIT ('WRIT', OLUN(I), OIND(I), NX, NY, ISWIN,
     *            XBUFF(1,I), NBYBUF, IBLKOF, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1020) I, IERR
                  GO TO 980
                  END IF
               END IF
 20         CONTINUE
C                                        Loop over rows
         CALL FILL (4, 1, ISPOS)
C                                        IF no file, blank row
         IF (.NOT.DOFILE(3)) CALL RFILL (NX, FBLANK, IROW(1,3))
         IF (.NOT.DOFILE(4)) CALL RFILL (NX, FBLANK, IROW(1,4))
         DO 50 IY = 1,NY
            DO 30 I = 1,4
               IF (DOFILE(I)) THEN
                  CALL MDISK ('READ', ILUN(I), IIND(I), IROW(1,I),
     *               ISPOS(I), IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1030) IY, I, IERR
                     GO TO 980
                     END IF
                  END IF
 30            CONTINUE
            DO 40 I = 1,2
               IF (DOFILE(I+4)) THEN
                  CALL MDISK ('WRIT', OLUN(I), OIND(I), XBUFF(1,I),
     *               IDPOS(I), IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1040) IY, I, IERR
                     GO TO 980
                     END IF
                  END IF
 40            CONTINUE
            CALL XMEDIN (NX, IROW(ISPOS(1),1), IROW(ISPOS(2),2),
     *         IROW(ISPOS(3),3), IROW(ISPOS(4),4), CPARM,
     *         XBUFF(IDPOS(1),1), XBUFF(IDPOS(2),2))
 50         CONTINUE
C                                        Write the last buffer
         DO 60 I = 1,2
            IF (DOFILE(I+4)) THEN
               CALL MDISK ('FINI', OLUN(I), OIND(I), XBUFF(1,I),
     *            IDPOS(I), IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1060) I, IERR
                  GO TO 980
                  END IF
               END IF
 60         CONTINUE
 70      CONTINUE
C                                        Create and write HI file
      CALL MEDIHI (IRET)
      GO TO 995
C                                        Error return
 980  CALL MSGWRT (7)
C
 995  CALL DIE (IRET, IROW)
C
 999  STOP
C-----------------------------------------------------------------------
 1010 FORMAT ('COULD NOT INITIALIZE INPUT FILE #',I2,'  IER=',I7)
 1020 FORMAT ('COULD NOT INITIALIZE OUTPUT FILE 3',I2,'  IER=',I7)
 1030 FORMAT ('COULD NOT READ LINE',I4,'  MAP #',I2,'  IER=',I7)
 1040 FORMAT ('COULD NOT WRITE LINE',I4,'  MAP #',I2,'  IER=',I7)
 1060 FORMAT ('COULD NOT WRITE LAST LINE, MAP #',I2,'.  IER=',I7)
      END
      SUBROUTINE MEDIIN (PRGNAM, IER)
C-----------------------------------------------------------------------
C   MEDIIN gets the inputs for MEDI, opens and checks the input images,
C   creates the output image(s), and prepares parameters in common for
C   the later stages of MEDI.
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
      CHARACTER IDCDEF*6, PHNAME*48, MTYPE*2, STOKES*8, COPCOD(1)*4,
     *   CHTMP*8, CHTMP1*8
      INTEGER   I, IERR, IRETCD, INPRMS, NB, INC, J, I1, IROUND, K
      LOGICAL   REDUCE, T
      REAL      STOKI(2), STOKO, EPS, X, AXV
      REAL      XSEQ1, XDSK1, XSEQ2, XDSK2, XSEQ3, XDSK3, XSEQ4, XDSK4,
     *   SEQOUT, DSKOUT, C(20), BLC(7), TRC(7), GRIDCR
      HOLLERITH XNAM1(3), XCLS1(2), XNAM2(3), XCLS2(2), XNAM3(3),
     *   XCLS3(2), XNAM4(3), XCLS4(2), XNAMOU(3), XCLSOU(2)
      DOUBLE PRECISION    DAXV
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'MEDI.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /INPARM/ XNAM1, XCLS1, XSEQ1, XDSK1, XNAM2, XCLS2, XSEQ2,
     *   XDSK2, XNAM3, XCLS3, XSEQ3, XDSK3, XNAM4, XCLS4, XSEQ4, XDSK4,
     *   GRIDCR, XNAMOU, XCLSOU, SEQOUT, DSKOUT, BLC, TRC, C
      DATA EPS /0.2/
      DATA T /.TRUE./
      DATA STOKES /'STOKES  '/
      DATA COPCOD /'MEDI'/
C-----------------------------------------------------------------------
C                                        Initialize file and header I/O
      ILUN(1) = 17
      ILUN(2) = 18
      ILUN(3) = 19
      ILUN(4) = 20
      OLUN(1) = 21
      OLUN(2) = 22
C
      CALL ZDCHIN (T, IROW)
      CALL VHDRIN
      NSCR = 0
      NCFILE = 0
      IER = 0
C                                        Get inputs from AIPS
      INPRMS = 70
      CALL GTPARM (PRGNAM, INPRMS, RQUICK, XNAM1, IROW, IERR)
      IRETCD = IERR
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (7)
         END IF
      IF (RQUICK) CALL RELPOP (IRETCD, IROW, IERR)
      IF (IRETCD.NE.0) GO TO 980
C                                       Change input OPCODE - char
      CTYPE = 1
      ALGO(1:1) = 'M'
      ALGO(2:2) = 'E'
      ALGO(3:3) = 'D'
      ALGO(4:4) = 'I'
      CALL RCOPY (20, C, CPARM)
      PLAIN = .FALSE.
C                                        Open 4 input files
      DOFILE(1) = CPARM(1).NE.IGNORE
      DOFILE(2) = CPARM(2).NE.IGNORE
      DOFILE(3) = CPARM(3).NE.IGNORE
      DOFILE(4) = CPARM(4).NE.IGNORE
C                                        Open output file
      DOFILE(5) = .TRUE.
C                                        Should Noise image be saved?
      DOFILE(6) = CPARM(NOISEO).GT.0.0
C                                        Transfer inputs into integer
C                                        variables
      FCVOL(1) = IROUND (XDSK1)
      FCVOL(2) = IROUND (XDSK2)
      FCVOL(3) = IROUND (XDSK3)
      FCVOL(4) = IROUND (XDSK4)
      ISSEQ(1) = IROUND (XSEQ1)
      ISSEQ(2) = IROUND (XSEQ2)
      ISSEQ(3) = IROUND (XSEQ3)
      ISSEQ(4) = IROUND (XSEQ4)
      FCVOL(5) = IROUND (DSKOUT)
      FCVOL(6) = IROUND (DSKOUT)
      IDSEQ(1) = IROUND (SEQOUT)
      IDSEQ(2) = IROUND (SEQOUT)
      IUSER = NLUSER
C                                       Hollerith -> Char.
      CALL H2CHR (12, 1, XNAM1, NAMIN(1))
      CALL H2CHR (6, 1, XCLS1, CLSIN(1))
      CALL H2CHR (12, 1, XNAM2, NAMIN(2))
      CALL H2CHR (6, 1, XCLS2, CLSIN(2))
      CALL H2CHR (12, 1, XNAM3, NAMIN(3))
      CALL H2CHR (6, 1, XCLS3, CLSIN(3))
      CALL H2CHR (12, 1, XNAM4, NAMIN(4))
      CALL H2CHR (6, 1, XCLS4, CLSIN(4))
      CALL H2CHR (12, 1, XNAMOU, NAMOUT(1))
      CALL H2CHR (6, 1, XCLSOU, CLSOUT(1))
      CALL H2CHR (12, 1, XNAMOU, NAMOUT(2))
      CALL H2CHR (6, 1, XCLSOU, CLSOUT(2))
C                                        Open both input maps
      MTYPE = 'MA'
      DO 35 I = 1,4
         IF (DOFILE(I)) THEN
            CALL MAPOPN ('READ', FCVOL(I), NAMIN(I), CLSIN(I), ISSEQ(I),
     *         MTYPE, IUSER, ILUN(I), IIND(I), FCCNO(I), CATBLK,
     *         IROW(1,I), IERR)
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
            NB = CATBLK(KIDIM)
            CALL FILL (KICTPN, 1, NPI(1,I))
            CALL COPY (NB, CATBLK(KINAX), NPI(1,I))
            NRDIM(I) = NB
            DO 30 J = 1,NB
               NPI(J,I) = MAX (1, NPI(J,I))
               IF (NPI(J,I).LE.1) NRDIM(I) = NRDIM(I) - 1
 30            CONTINUE
            END IF
 35      CONTINUE
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
      DO 70 K = 2,4
         IF (DOFILE(K)) THEN
            CALL COPY (256, CATB(1,K), CATBLK)
            DO 65 I = 1,7
               J = I - 1
C                                       null axis
               IBLC(I,K) = 1
               ITRC(I,K) = 1
               IF (NPI(I,K).GT.1) THEN
                  AXV = CATR(KRCRP+J) - CATR1(KRCRP+J) + IBLC(I,1)
                  IF (GRIDCR.LT.-1.5) AXV = IBLC(I,1)
                  IF (GRIDCR.GT.-0.1) THEN
                     DAXV = CATD1(KDCRV+J) + (IBLC(I,1) -
     *                  CATR1(KRCRP+J)) * CATR1(KRCIC+J)
                     IF (CATR(KRCIC+J).EQ.0.0) GO TO 75
                     AXV = (DAXV - CATD(KDCRV+J)) / CATR(KRCIC+J) +
     *                  CATR(KRCRP+J)
                     END IF
                  IBLC(I,K) = IROUND (AXV)
                  IF ((GRIDCR.GE.0.1) .AND. (ABS(AXV-IBLC(I,K)).GT.EPS))
     *               GO TO 75
                  ITRC(I,K) = IBLC(I,K) + ITRC(I,1) - IBLC(I,1)
C                                       smaller subimage required?
                  IF (IBLC(I,K).LT.1) THEN
                     IBLC(I,1) = IBLC(I,1) + 1 - IBLC(I,K)
                     BLC(I) = IBLC(I,1)
                     IF (K.GT.2) IBLC(I,2) = IBLC(I,2) + 1 - IBLC(I,K)
                     IF (K.GT.3) IBLC(I,3) = IBLC(I,3) + 1 - IBLC(I,K)
                     REDUCE = .TRUE.
                     IBLC(I,K) = 1
                     END IF
                  IF (ITRC(I,K).GT.NPI(I,K)) THEN
                     ITRC(I,1) = ITRC(I,1) + NPI(I,K) - ITRC(I,K)
                     TRC(I) = ITRC(I,1)
                     IF (K.GT.2) ITRC(I,2) = ITRC(I,2) + NPI(I,K) -
     *                  ITRC(I,K)
                     IF (K.GT.3) ITRC(I,3) = ITRC(I,3) + NPI(I,K) -
     *                  ITRC(I,K)
                     REDUCE = .TRUE.
                     ITRC(I,K) = NPI(I,K)
                     END IF
                  IF (IBLC(I,1).GT.ITRC(I,1)) GO TO 75
                  NPO(I) = ITRC(I,1) - IBLC(I,1) + 1
C                                        Check coincidence
                  IF (GRIDCR.GE.0.1) THEN
                     IPOINT = KHCTP+J*INC
                     CALL H2CHR (8, 1, CATH(IPOINT), CHTMP)
                     CALL H2CHR (8, 1, CATH1(IPOINT), CHTMP1)
                     IF (CHTMP.NE.CHTMP1) GO TO 75
                     X = EPS * EPS * ABS(CATR1(KRCIC+J))
                     IF (ABS(CATR(KRCIC+J)-CATR1(KRCIC+J)).GT.X)
     *                  GO TO 75
                     IF (ABS(CATR(KRCRT+J)-CATR1(KRCRT+J)).GT.1.)
     *               GO TO 75
                     END IF
                  END IF
 65            CONTINUE
            END IF
 70      CONTINUE
      GO TO 80
C                                        Maps not coincident
 75   WRITE (MSGTXT,1075) I
      CALL MSGWRT (7)
      GO TO 960
C                                       Create output map
C                                       Get stokes value of input maps
C                                       Insert proper Stokes value
 80   MSGTXT = 'MEDIIN: input maps coincident on reduced subimage only'
      IF (REDUCE) CALL MSGWRT (6)
      XX(1) = -1.0E30
      XX(2) = -1.0E30
      XN(1) = 1.0E30
      XN(2) = 1.0E30
      BFLAG = .FALSE.
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
      IF (NPO(I).LE.1) CATD1(I1) = STOKO
      IF ((CTYPE.NE.5) .OR. ((STOKI(1).EQ.2) .AND. (STOKI(2).EQ.3)))
     *   GO TO 110
         WRITE (MSGTXT,1100)
         CALL MSGWRT (6)
C                                        Default out class
 110  IDCDEF = ' '
      J = STOKO
      IF (STOKO.EQ.STOKI(1)) J = 1
      IF (J.LE.4) IDCDEF(:4) = COPCOD(CTYPE)
C                                        Set header values for output
C                                        map
      CALL MAKOUT (NAMIN(1), CLSIN(1), ISSEQ(1), IDCDEF, NAMOUT(1),
     *   CLSOUT(1), IDSEQ(1))
      CALL CHR2H (12, NAMOUT(1), KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLSOUT(1), KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = IDSEQ(1)
      CATBLK(KIIMU) = NLUSER
      CALL SUBHDR (BLC, TRC, 1.0, 1.0)
C                                        Put in map units
      CALL MCREAT (FCVOL(5), FCCNO(5), IROW, IERR)
      IDSEQ(1) = CATI1(KIIMS)
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
      CALL KEYCOP (FCVOL(1), FCCNO(1), FCVOL(5), FCCNO(5), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1135) IERR
         CALL MSGWRT (7)
C                                       Let it slide.
         IERR = 0
         END IF
C                                        Open the output file
      CALL ZPHFIL ('MA', FCVOL(5), FCCNO(5), 1, PHNAME, IERR)
      CALL ZOPEN (OLUN(1), OIND(1), FCVOL(5), PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1136) IERR
         CALL MSGWRT (7)
         GO TO 950
         END IF
      CALL COPY (256, CATBLK, CATIO(1,1))
C                                       Create noise image
      IF (DOFILE(6)) THEN
         NAMOUT(2) = NAMOUT(1)
         CALL CHR2H (12, NAMOUT(2), KHIMNO, CATH(KHIMN))
         CLSOUT(2) = CLSOUT(1)
         CLSOUT(2)(1:1) = 'D'
         CLSOUT(2)(2:2) = 'E'
         CLSOUT(2)(3:3) = 'V'
         CLSOUT(2)(4:4) = 'I'
         CALL CHR2H (6, CLSOUT(2), KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = IDSEQ(2)
         CATBLK(KIIMU) = NLUSER
         CALL MCREAT (FCVOL(6), FCCNO(6), IROW, IERR)
         IDSEQ(2) = CATI1(KIIMS)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1110) IERR
            CALL MSGWRT (8)
            GO TO 950
            END IF
         NCFILE = NCFILE + 1
         FRW(NCFILE) = 2
         FVOL(NCFILE) = FCVOL(6)
         FCNO(NCFILE) = FCCNO(6)
C                                       Copy header keywords from first
C                                       input.
         CALL KEYCOP (FCVOL(1), FCCNO(1), FCVOL(6), FCCNO(6), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1135) IERR
            CALL MSGWRT (7)
C                                       Let it slide.
            IERR = 0
            END IF
C                                        Open the output file
         CALL ZPHFIL ('MA', FCVOL(6), FCCNO(6), 1, PHNAME, IERR)
         CALL ZOPEN (OLUN(2), OIND(2), FCVOL(6), PHNAME, T, T, T, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1136) IERR
            CALL MSGWRT (7)
            GO TO 950
            END IF
         CALL COPY (256, CATBLK, CATIO(1,2))
         END IF
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
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('MEDIIN: COULD NOT GET PARMS.  IER=',I7)
 1025 FORMAT ('MEDIIN: COULD NOT OPEN MAP # ',I1,'  IER=',I7)
 1075 FORMAT ('MEDIIN: INPUT MAPS ARE NOT COINCIDENT ON AXIS',I2)
 1100 FORMAT ('WARNING: angle map output not Polarization Angle')
 1110 FORMAT ('MEDIIN: COULD NOT CREATE OUTPUT MAP.  IER=',I7)
 1135 FORMAT ('MEDIIN: ERROR ', I3,' COPYING KEYWORDS - CONTINUING')
 1136 FORMAT ('MEDIIN: COULD NOT OPEN OUTPUT MAP.  IER=',I7)
      END
      SUBROUTINE PMEDIN (C, IER)
C-----------------------------------------------------------------------
C   PMEDIN writes output describing the algorithm, prepares default
C   values and obtains necessary parameters for MEDIN.
C   Inputs:
C      C(20)     R     The algorithm parameters
C   Outputs:
C      IER       I     Error code: 0 --> okay
C                                  1 --> not okay
C      FREQ1     R     Frequency of first map
C      FREQ2     R     Frequency of second map
C      LNFREQ    R     Log(FREQ1/FREQ2)
C      FBLANK    R     Blanking value used in scratch file
C      FMIN      R     Minimum Value to be used in all maps
C      FMAX      R     Maximum Value to be used in all maps
C-----------------------------------------------------------------------
      INTEGER   IER, I
      REAL      C(20)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'MEDI.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      FMIN = MAX (ABS(CATR1(KRDMX)), ABS(CATR1(KRDMN)))
      FMAX = MAX (ABS(CATR2(KRDMX)), ABS(CATR2(KRDMN)))
C                                       Initialize
      IER = 0
      XBLFLG = C(8).LE.0.0
C                                       Median Value 'MEDI'
C                                       For up to 4 input images
      DO 299 I = 1,4
C                                       If scaling constants zero => 1.
         IF (C(I).EQ.0.0) C(I) = 1.0
C                                       If not ignoring input
         IF (C(I).NE.IGNORE) THEN
            WRITE (MSGTXT,1280) C(I), I
            CALL MSGWRT (2)
            END IF
 299     CONTINUE

C                                       Any blanking
      FMIN = C(CLIPMN)
      FMAX = C(CLIPMX)
      IF (XBLFLG) THEN
         MSGTXT = 'Magic blanking used for clipped & illegal values'
      ELSE
         MSGTXT = 'Zero replaces clipped & illegal values'
         END IF
      CALL MSGWRT (2)
C                                       Blanking is on input map values
      IF (C(14).LE.0.5) THEN
         IF ((C(CLIPMN).NE.0.0) .OR. (C(CLIPMX).NE.0.0)) THEN
            WRITE (MSGTXT,1902) C(CLIPMN), C(CLIPMX)
            CALL MSGWRT (2)
            END IF
C                                       Blanking is on output S/N
      ELSE IF (C(14).GE.1.5) THEN
         WRITE (MSGTXT,1905) C(ISNR)
         IF (C(ISNR).GT.0.0) CALL MSGWRT (2)
C                                       Blanking is on output sigma
      ELSE
         WRITE (MSGTXT,1910) C(ISNR)
         IF (C(ISNR).GT.0.0) CALL MSGWRT (2)
         END IF
C                                       Map noise numbers
      IF (C(NOISEV).GT.0.0) THEN
         WRITE (MSGTXT,1920) C(NOISEV)
         CALL MSGWRT (2)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1280 FORMAT ('Median:',1PE11.3,'*Map(',I1,')')
 1902 FORMAT ('Clipping based on',1PE11.3,' in Map(1) or ',1PE11.3,
     *   ' in Map(2)')
 1905 FORMAT ('Blanking done if output S/N is less than',F8.4)
 1910 FORMAT ('Blanking done if output sigma exceeds',F8.4)
 1920 FORMAT ('Using input MAPs noise level',1PE11.3)
      END
      SUBROUTINE XMEDIN (NX, V1, V2, S1, S2, C, R1, R2)
C-----------------------------------------------------------------------
C   XMEDIN combines two pixel rows V1 and V2 with noise rows S1 and S2
C   to form result row R1 and noise row R2.  The algorithm is always
C   median filtering of the 4 values.
C   Inputs:
C      NX     I       Number of pixels in each vector.
C      V1     R(NX)   The pixel values of the input map 1
C      V2     R(NX)   The pixel values of the input map 2
C      S1     R(NX)   The noise values of the input map 1
C      S2     R(NX)   The noise values of the input map 2
C      C      R(20)   Up to 20 inputs parameters
C   Outputs:
C      R1     R(NX)   The result of the combination
C      R2     R(NX)   The noise of the combination
C-----------------------------------------------------------------------
      INTEGER   NX
      REAL      V1(*), V2(*), S1(*), S2(*), R1(*), R2(*), C(20)
C
      INTEGER   I, NVALUE
      REAL      SN, BRES, VALUES(10)
      LOGICAL   CLPSIG, CLPSN
      INCLUDE 'MEDI.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Clip on Signal Image/Noise value
      CLPSN  = (C(14).GE.1.5) .AND. (C(ISNR).GT.0.0)
C                                       Clip on Noise Image /Noise value
      CLPSIG = (C(14).LT.1.5) .AND. (C(14).GT.0.5) .AND.(C(ISNR).GT.0.0)
C                                       Value for blanked on output
      BRES = FBLANK
      IF (.NOT.XBLFLG) BRES = 0.0
C                                       'MEDI'
      DO 100 I = 1,NX
C                                       Prepare to count values
         NVALUE = 1
         VALUES(NVALUE) = C(1)*V1(I)
C                                       if not blanked value
         IF ((V1(I).NE.FBLANK) .AND. (C(1) .NE. IGNORE) .AND.
     *       (FMIN.EQ.0. .OR. VALUES(NVALUE).GE.FMIN) .AND.
     *       (FMAX.EQ.0. .OR. VALUES(NVALUE).LE.FMAX))
     *      NVALUE = NVALUE + 1
C                                       save in non-blanked slot
         VALUES(NVALUE) = C(2)*V2(I)
C                                       if not blanked value
         IF ((V2(I).NE.FBLANK) .AND. (C(2) .NE. IGNORE) .AND.
     *       (FMIN.EQ.0. .OR. VALUES(NVALUE).GE.FMIN) .AND.
     *       (FMAX.EQ.0. .OR. VALUES(NVALUE).LE.FMAX))
     *      NVALUE = NVALUE + 1
C                                       save in non-blanked slot
         VALUES(NVALUE) = C(3)*S1(I)
C                                       if not blanked value
         IF ((S1(I).NE.FBLANK) .AND. (C(3) .NE. IGNORE) .AND.
     *       (FMIN.EQ.0. .OR. VALUES(NVALUE).GE.FMIN) .AND.
     *       (FMAX.EQ.0. .OR. VALUES(NVALUE).LE.FMAX))
     *      NVALUE = NVALUE + 1
C                                       save in non-blanked slot
         VALUES(NVALUE) = C(4)*S2(I)
C                                       if not blanked value
         IF ((S2(I).NE.FBLANK) .AND. (C(4) .NE. IGNORE) .AND.
     *       (FMIN.EQ.0. .OR. VALUES(NVALUE).GE.FMIN) .AND.
     *       (FMAX.EQ.0. .OR. VALUES(NVALUE).LE.FMAX))
     *      NVALUE = NVALUE + 1
C                                       fix value count
         NVALUE = NVALUE - 1
C                                       if no unblanked values
         IF (NVALUE .LE. 0) THEN
            R1(I) = FBLANK
            R2(I) = FBLANK
C                                       if only one value
         ELSE IF (NVALUE .EQ. 1) THEN
            R1(I) = VALUES(1)
            R2(I) = FBLANK
         ELSE
C                                       if more than 1 value
            CALL EXCLUD( NVALUE, VALUES, R1(I), R2(I))
         END IF
C
 100     CONTINUE
      GO TO 900
C                                       Common processing
 900  DO 910 I = 1,NX
C                                       if clip on singal/noise
         IF (CLPSN .AND. R1(I).NE.FBLANK) THEN
            SN = ABS (R1(I)) / C(NOISEV)
            IF (SN.LT.C(ISNR)) R1(I) = FBLANK
C                                       else if clip on noise/noise
         ELSE IF (CLPSIG) THEN
            IF (R2(I).EQ.FBLANK) THEN
               R1(I) = FBLANK
            ELSE
               SN = ABS (R2(I)) / C(NOISEV)
               IF ( SN.GT.C(ISNR)) R1(I) = FBLANK
               END IF
            ENDIF
C                                       if blank, flag both
         IF (R1(I).EQ.FBLANK) R1(I) = BRES
         IF (R2(I).EQ.FBLANK) R2(I) = BRES
C                                       Get max/min
         IF (R1(I).NE.FBLANK) THEN
            XX(1) = MAX (XX(1), R1(I))
            XN(1) = MIN (XN(1), R1(I))
         ELSE
            BFLAG = .TRUE.
            END IF
         IF (R2(I).NE.FBLANK) THEN
            XX(2) = MAX (XX(2), R2(I))
            XN(2) = MIN (XN(2), R2(I))
         END IF
 910     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE EXCLUD ( NVALUE, VALUES, OUTVAL, OUTVL2)
C-----------------------------------------------------------------------
C   EXCLUD averages several pixel values, excluding the extreama.  For a
C   Small number (ie 2, 3 or 4) this function is the same as the median.
C   Inputs:
C      NVALUE   I       Number of values to examine
C      VALUES   R()     Number of values to average with extreama exclud
C   Output:
C      OUTVAL   R       Value of array with extreama excluded
C      OUTVL2   R       sum of values not used
C-----------------------------------------------------------------------
      INTEGER   NVALUE
      REAL VALUES(*), OUTVAL, OUTVL2
C-----------------------------------------------------------------------
      INTEGER   I
      REAL      SUM, MINVAL, MAXVAL
C-----------------------------------------------------------------------
C                                       Initialize sum and extreama
      SUM    = VALUES(1)
      MINVAL = VALUES(1)
      MAXVAL = VALUES(1)
C                                       for all other values
      DO 100 I = 2, NVALUE
         SUM = SUM + VALUES(I)
         IF (MINVAL .LT. VALUES(I)) THEN
            MINVAL = VALUES(I)
         ELSE IF (MAXVAL .GT. VALUES(I)) THEN
            MAXVAL = VALUES(I)
         END IF
 100     CONTINUE
C                                       If possible to exclude 2
      IF (NVALUE .GT. 2) THEN
C                                       Exclude min and max
         OUTVAL = (SUM - MINVAL - MAXVAL) / (NVALUE-2.)
         OUTVL2 = (SUM / NVALUE) - OUTVAL
C                                       If only a pair of values
      ELSE IF (NVALUE .EQ. 2) THEN
C                                       Just average
         OUTVAL = SUM * 0.5
         OUTVL2 = SUM - OUTVAL
      ELSE
C                                       Else output value
         OUTVAL = SUM
C                                       blank second value
         OUTVL2 = 0
      END IF
C
 999  RETURN
      END
      SUBROUTINE MEDIHI (IRET)
C-----------------------------------------------------------------------
C   MEDIHI creates and writes the HI file associated with task MEDI.
C   Outputs:
C      IRET   I      > 0 => output all blanks
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER HILINE*72, ABUNIT(3)*8, FTYPE(2)*8, NOTTYP*2
      INTEGER   NHISTF, IHDLUN, IHSLUN, I, IER, IERR, IBUFF1(256), J,
     *    IBUFF2(256), IPTR, ITEMP, FU
      REAL      TEMP
      LOGICAL   TRUE, FALSE
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'MEDI.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (IBUFF1(1),IROW(1,1)),  (IBUFF2(1),IROW(257,1))
      DATA NHISTF, IHDLUN, IHSLUN /2,27,28/
      DATA ABUNIT /'ANGLE   ', 'RADIANS ', 'ROT.MS. '/
      DATA TRUE, FALSE /.TRUE.,.FALSE./
      DATA FTYPE /'IMAGE','DEVI '/
      DATA NOTTYP /'CC'/
C-----------------------------------------------------------------------
C                                       Initialize HITAB
      CALL HIINIT (NHISTF)
      IRET = 0
      DO 200 J = 2,1,-1
         IF (DOFILE(4+J)) THEN
C                                       Test validity of result
            IF (XX(J).LT.XN(J)) THEN
               IRET = 4
               WRITE (MSGTXT,1000) FTYPE(J)
               CALL MSGWRT (8)
               GO TO 999
            ELSE IF (XX(J).EQ.XN(J)) THEN
               WRITE (MSGTXT,1001) FTYPE(J), XX(J)
               CALL MSGWRT (6)
               END IF
C                                       Insert header parameters
            CALL COPY (256, CATIO(1,J), CATBLK)
            CATR(KRDMN) = XN(J)
            CATR(KRDMX) = XX(J)
C                                        Put in inhibited pixel value
            CATR(KRBLK) = 0.0
            IF (BFLAG) CATR(KRBLK) = FBLANK
C                                       Fix strange units
            FU = 0
            TEMP = 90. / 3.14159
            IF (FU.GT.0) CALL CHR2H (8, ABUNIT(FU), 1, CATH(KHBUN))
C                                       Create and open new HI file
            CALL HICREA (IHDLUN, FCVOL(4+J), FCCNO(4+J), CATBLK, IBUFF2,
     *         IERR)
            IPTR = IBUFF2(3)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) FTYPE(J), IERR
               CALL MSGWRT (7)
               GO TO 200
               END IF
C                                       Copy both input HI files
            DO 50 I = 1,4
               IF (DOFILE(I)) THEN
                  WRITE (HILINE,1020) I
                  CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
                  IF (IERR.NE.0) GO TO 190
                  ITEMP = HITAB(IPTR+2)
                  CALL HIOPEN (IHSLUN, FCVOL(I), FCCNO(I), IBUFF1,IERR)
                  IF (IERR.NE.0) GO TO 30
                  CALL HICOPY (IHSLUN, IBUFF1, IHDLUN, IBUFF2, IERR)
                  CALL HICLOS (IHSLUN, FALSE, IBUFF1, IER)
                  IF (IERR.EQ.0) GO TO 50
                     IF (IERR.GE.100) HITAB(IPTR+2) = ITEMP
 30                  WRITE (MSGTXT,1030) I, IERR
                     CALL MSGWRT (6)
                     WRITE (HILINE,1031)
                     CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
                     IF (IERR.NE.0) GO TO 190
                  END IF
 50            CONTINUE
            WRITE (HILINE,1050)
            CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            WRITE (HILINE,1055) TSKNAM, RLSNAM, IUSER
            CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
C                                       Add input parameters
            CALL HENCO1 (TSKNAM, NAMIN(1), CLSIN(1), ISSEQ(1), FCVOL(1),
     *         IHDLUN, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            IF (DOFILE(2))
     *         CALL HENCO2 (TSKNAM, NAMIN(2), CLSIN(2), ISSEQ(2),
     *         FCVOL(2), IHDLUN, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            IF (DOFILE(3))
     *         CALL HENCO3 (TSKNAM, NAMIN(3), CLSIN(3), ISSEQ(3),
     *         FCVOL(3), IHDLUN, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            IF (DOFILE(4))
     *         CALL HENCO4 (TSKNAM, NAMIN(4), CLSIN(4), ISSEQ(4),
     *         FCVOL(4), IHDLUN, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            CALL HENCOO (TSKNAM, NAMOUT(J), CLSOUT(J), IDSEQ(J),
     *         FCVOL(4+J), IHDLUN, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            WRITE (HILINE,1094) TSKNAM, (IBLC(I,1), I = 1,7)
            CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            WRITE (HILINE,1096) TSKNAM, (ITRC(I,1), I = 1,7)
            CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            IF ((NRDIM(1).GT.2) .AND. (NRDIM(2).LE.2)) THEN
               WRITE (HILINE,1098) TSKNAM
               CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
               END IF
C                                       Parameters
            WRITE (HILINE,1101) TSKNAM, (I, CPARM(I), I = 1,2)
            CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            WRITE (HILINE,1101) TSKNAM, (I, CPARM(I), I = 3,4)
            CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            WRITE (HILINE,1102) TSKNAM, (I, CPARM(I), I = 5,6)
            CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
C                                       blanking
            IF (CPARM(8).LE.0.0) WRITE (HILINE,1110) TSKNAM
            IF (CPARM(8).GT.0.0) WRITE (HILINE,1111) TSKNAM
            CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
C                                       clip on inputs
            IF ((CPARM(CLIPMN).NE.0.0) .OR.
     *          (CPARM(CLIPMX).NE.0.0)) THEN
               WRITE (HILINE,1112) TSKNAM,CPARM(CLIPMN),CPARM(CLIPMX)
               CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
               IF (IERR.NE.0) GO TO 190
               END IF
C                                       Noise value
            IF (CPARM(NOISEV).NE.0.0) THEN
               WRITE (HILINE,1130) TSKNAM, CPARM(NOISEV), 1
               CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
               END IF
            IF (IERR.NE.0) GO TO 190
C                                       Noise limit
            IF ((CPARM(13).GT.0.0) .AND. (CPARM(16).GT.0.0)) THEN
               WRITE (HILINE,1131) TSKNAM, CPARM(16)
               CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
               IF (IERR.NE.0) GO TO 190
               END IF
C                                       Noise clipping on SNR
            IF ((CPARM(14).GT.0.5) .OR. (CPARM(15).GT.0.0)) THEN
               WRITE (HILINE,1140) TSKNAM, CPARM(15)
               IF (CPARM(14).GE.1.5) WRITE (HILINE,1141) TSKNAM,
     *            CPARM(15)
               CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
               IF (IERR.NE.0) GO TO 190
               END IF
C                                       Close HI file
 190        CALL HICLOS (IHDLUN, TRUE, IBUFF2, IER)
            WRITE (MSGTXT,1900) FTYPE(J)
            IF ((IER.NE.0) .OR. (IERR.NE.0)) WRITE (MSGTXT,1901)
     *         FTYPE(J), IERR, IER
            CALL MSGWRT (2)
C                                        Copy tables
            CALL ALLTAB (1, NOTTYP, IHSLUN, IHDLUN, FCVOL(1),
     *         FCVOL(4+J), FCCNO(1), FCCNO(4+J), CATBLK, IBUFF1, IBUFF2,
     *         IERR)
            IF (IERR.GT.2) THEN
               MSGTXT = 'ERROR COPYING TABLE FILES'
               CALL MSGWRT (6)
               END IF
C                                       Close map file
            CALL MAPCLS ('INIT', FCVOL(4+J), FCCNO(4+J), OLUN(J),
     *         OIND(J), CATBLK, TRUE, IBUFF2, IERR)
            NCFILE = NCFILE - 1
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1910) FTYPE(J), IERR
               CALL MSGWRT (6)
               END IF
            END IF
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OUTPUT ',A5,' FILE COMPLETELY BLANKED -- QUITTING')
 1001 FORMAT ('Output ',A5,' FILE constant at',1PE13.5,
     *   ' -- saving it anyway')
 1010 FORMAT ('MEDIHI: could not create HISTORY FOR ',A5,' FILE.  IER=',
     *   I5)
 1020 FORMAT (31X,'/ History of input map', I2)
 1030 FORMAT ('WARNING COULD NOT OPEN/COPY INPUT HI FILE=',I2,
     *   '.  IER=',I7)
 1031 FORMAT (31X,'/ NO HISTORY TO BE FOUND')
 1050 FORMAT (31X,'/ End of old histories')
 1055 FORMAT (A6,'RELEASE=',A7,' '' USERID=',I5)
 1094 FORMAT (A6,'BLC=',7I5,' / Bottom left corner')
 1096 FORMAT (A6,'TRC=',7I5,' / Top right corner')
 1098 FORMAT (A6,'/ Single plane of Map 2 applied to all planes of',
     *   ' Map 1')
 1101 FORMAT (A6,2('A(',I1,')=',1PE12.4,2X))
 1102 FORMAT (A6,2('A(',I1,')=',1PE12.4,2X),' / clip limits')
 1110 FORMAT (A6,'/ Undefined pixels magic-value blanked')
 1111 FORMAT (A6,'/ Undefined pixels set to zero')
 1112 FORMAT (A6,' > ',1PE12.4,' < ',1PE12.4,'/ Clip levels')
 1130 FORMAT (A6,'SIGMA=',1PE12.4,'  /Input images noise level')
 1131 FORMAT (A6,'B(6)=',1PE12.4,'  / Output sigma limit')
 1140 FORMAT (A6,'B(5)=',1PE12.4,'  / Max unblanked noise')
 1141 FORMAT (A6,'B(5)=',1PE12.4,'  / Min unblanked sig/noise')
 1900 FORMAT ('History file created and written for ',A5,' file')
 1901 FORMAT ('HISTORY FILE FOR ',A5,' INCOMPLETE.  IERRS =',2I7)
 1910 FORMAT ('WARNING MEDIHI COULD NOT CLOSE OUTPUT ',A5,
     *   ' FILE.  IER=',I7)
      END
