LOCAL INCLUDE 'COMB.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   CATI1(256), CATI2(256), CATI3(256), CATI4(256),
     *   CATB(256,4), CATIO(256,2)
      REAL      CATR1(256), CATR2(256), CATR3(256), CATR4(256)
      HOLLERITH CATH1(256), CATH2(256), CATH3(256), CATH4(256)
      DOUBLE PRECISION CATD1(128),  CATD2(128), CATD3(128), CATD4(128)
      REAL      IROW(MABFSS,4), FREQ1, FREQ2, LNFREQ, FMAX1,
     *   FMAX2, CPARM(20), XN(2), XX(2), CBMULT(4)
      CHARACTER NAMIN(4)*12, CLSIN(4)*6, NAMOUT(2)*12, CLSOUT(2)*6,
     *   ALGO*4
      LOGICAL   XBLFLG, BFLAG, DOFILE(6), PLAIN
      INTEGER   IBLC(7,4), ITRC(7,4), NPI(7,4), NPO(7), FBL(2), ILUN(4),
     *   IIND(4), OLUN(2), OIND(2), ISSEQ(4), IDSEQ(2), NAX, NRDIM(4),
     *   CTYPE, IUSER, IPOINT, FCVOL(6), FCCNO(6), HISTDO, NAX2
      COMMON /COMBCM/ CATB, CATIO, IROW, FREQ1, FREQ2, LNFREQ,
     *   FMAX1, FMAX2, CPARM, XN, XX, XBLFLG, BFLAG, FBL, IBLC, ITRC,
     *   NPI, NPO, ILUN, IIND, OLUN, OIND, ISSEQ, IDSEQ, NAX, NRDIM,
     *   CTYPE, IUSER, IPOINT, DOFILE, PLAIN, FCVOL, FCCNO, HISTDO,
     *   CBMULT, NAX2
      COMMON /COMBCH/ NAMIN, CLSIN, NAMOUT, CLSOUT, ALGO
      EQUIVALENCE (CATI1, CATR1, CATD1, CATH1, CATB(1,1))
      EQUIVALENCE (CATI2, CATR2, CATD2, CATH2, CATB(1,2))
      EQUIVALENCE (CATI3, CATR3, CATD3, CATH3, CATB(1,3))
      EQUIVALENCE (CATI4, CATR4, CATD4, CATH4, CATB(1,4))
      INCLUDE 'INCS:DCAT.INC'
LOCAL END
      PROGRAM COMB
C-----------------------------------------------------------------------
C! Combine two input maps in various ways, producing an output map.
C# Map-util SPECTRAL POLARIZATION ANALYSIS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2001, 2004, 2008-2009, 2011-2013, 2015,
C;  Copyright (C) 2017-2018
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
C   COMB is a general program in which two input images and two optional
C   noise images are combined on a pixel by pixel level to produce a
C   third image and option noise image.  It can combine 2 n-dimensional
C   images or one 2-dimensional image with each plane of an
C   n-dimensional image.
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
C   OPCODE......The combination algorithm specification: (A=APARM)
C      ='SUM ': Linear sum   A(1)*MAP(1) + A(2)*MAP(2) + A(3)
C      ='DIV ': Division     A(1)*MAP(1) / MAP(2) + A(2)
C      ='SPIX': Sp. Index    A(1)*SP.IND(MAP(1),MAP(2)) + A(2)
C      ='POLI': Pol. Inten.  A(1)*SQRT(MAP(1)**2 + MAP(2)**2))
C                               + A(2)
C      ='POLC': Pol. Inten.  A(1)*SQRT(MAP(1)**2 + MAP(2)**2))*C
C                               + A(2)  where C = noise correction
C      ='POLA': Pol. Angle   A(1)*ATAN2(MAP(2),MAP(1)) + A(2)
C                            where A(3) < SQRT (MAP(1)**2 + MAP(2)**2)
C                            MAP(1)=QPOL, MAP(2)=UPOL usually.
C      ='MULT': Multiplic.   A(1)*MAP(1)*MAP(2) + A(2)
C      ='OPTD': Opacity      A(1) * LN (A(3)*MAP(1)/MAP(2)+A(4))
C                               + A(2)
C      ='CLIP': Clipping     MAP(1) except where A(1) > MAP(2)
C                               > A(2)  or  A(1) < A(2) and
C                               either MAP(2) < A(1) or > A(2)
C      ='REAL': Rect. conv.  A(1)*(MAP(1)*COS(A(2)*MAP(2)) + A(3)
C      ='IMAG': Rect. conv.  A(1)*(MAP(1)*SIN(A(2)*MAP(2)) + A(3)
C                               (MAP(2) assumed in degrees)
C      ='MEAN': weighted mean A(1)*MAP(1) + A(2)*MAP(2)
C                               take one MAP(n) if other blanked
C      ='RM  ': Rot. Meas.   A(1)*Rot.Meas.Map + A(2)
C                               test angle diff +- 180,360 to bring
C                               closer to A(3) (in degrees) RM in
C                               RADians/M/M with A(1)=1 (A(2) in
C                               RAD/M/M).  A(2) used also to bring angle
C                               diff closer to A(3).
C      ='SUM ': As SUMM, but different treatment of blank points:
C   APARM.......Parameters needed for algorithm:
C      APARM(1), APARM(2), APARM(3), APARM(4) used as above.
C           APARM(1) = 0 => APARM(1) = 1.0 (except 'POLA','CLIP')
C           APARM(1) = 0 => APARM(1) = 28.648 (for 'POLA')
C           APARM(2) = 0 => APARM(2) = 1.0 (FOR 'SUM ')
C           APARM(3) = 0 => APARM(3) = 1.0 (for 'OPTD')
C      APARM(8) >  0  => Use 0.0 for clipped & illegal values
C               <= 0  => Use blanking for clipped & illegal values
C      APARM(9) = Clip if Abs (MAP(1)) < APARM(9) - image units.
C      APARM(10) = Clip if Abs(MAP(2)) < APARM(10) - image units.
C           There are no defaults for APARM(9) and (10) and a zero
C           value means no clipping.  Used only if BPARM(4) <= 0.5
C   BPARM.......Parameters needed noise calculation and control:
C      BPARM(1) = 1-sigma level on 1st input map.
C                 0 => ignore noise, -1 => use third image
C      BPARM(2) = 1-sigma level on 2nd input map.
C                 0 => ignore noise, -1 => use fourth image
C                 Zero is not allowed for 'POLC' and whenever
C                 BPARM(3) > 0 or BPARM(4) > 1.5.
C      BPARM(3) = false (<= 0) => output normal image
C               = true  (>  0) => output normal and sigma image
C                 Blanking is the same for both settings of B(3).
C      BPARM(4) <= 0.5  => Blank output map using input map values
C               else    => Blank output map using output map sigma
C               >= 1.5  => Blank output map using output map S/N.
C      BPARM(5) = Error on output map value above which output
C                 pixel is blanked (if BPARM(4) = 1) 0 -> FMAX1
C               = S/N ratio of output map value below which output
C                 pixel is blanked (if BPARM(4) = 2) 0 -> 0.05
C      BPARM(6) = Maximum value of sigma to be outputted (used if
C                 BPARM(3) > 0 only)  0 -> FMAX1;  360. for POLA.
C                 This is not a good value for some other OPCODEs.
C   DOHIST.....-2 => copy 1st HI only
C              -3 => no copy of HI
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   IRET, IERR, IWIN(4), ISWIN(4), NBY, NX, NY, IDEPTH(5),
     *   IBLKOF, NBYBUF, ISPOS(4), IDPOS(2), I3, I4, I5, I6, I7, I, IY
      INCLUDE 'COMB.INC'
      REAL      XBUFF(MABFSS,2), TEMP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGNAM /'COMB  '/
      DATA ISWIN /0,0,0,0/
C-----------------------------------------------------------------------
C                                        Initialize input maps
      IRET = 16
      CALL COMBIN (PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL PCOMBN (CTYPE, CPARM, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL FILL (4, 1, ISPOS)
      CALL FILL (2, 1, IDPOS)
C                                        Do-loop limits: window
      NBY = 2
      NBYBUF = MABFSS * NBY
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
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1010) I, IERR
                  GO TO 980
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
         IF (.NOT.DOFILE(3)) CALL RFILL (NX, CPARM(11), IROW(1,3))
         TEMP = CBMULT(2) * CPARM(12)
         IF (.NOT.DOFILE(4)) CALL RFILL (NX, TEMP, IROW(1,4))
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
            IF (PLAIN) THEN
               CALL COMBN (NX, IROW(ISPOS(1),1), IROW(ISPOS(2),2),
     *            CTYPE, CPARM, XBUFF(IDPOS(1),1))
            ELSE
               CALL XCOMBN (NX, IROW(ISPOS(1),1), IROW(ISPOS(2),2),
     *            IROW(ISPOS(3),3), IROW(ISPOS(4),4), CTYPE, CPARM,
     *            XBUFF(IDPOS(1),1), XBUFF(IDPOS(2),2))
               END IF
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
      CALL COMBHI (IRET)
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
      SUBROUTINE COMBIN (PRGNAM, IER)
C-----------------------------------------------------------------------
C   COMBIN gets the inputs for COMB, opens and checks the input images,
C   creates the output image(s), and prepares parameters in common for
C   the later stages of COMB.
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
      CHARACTER IDCDEF*6, PHNAME*48, MTYPE*2, ALGOS(14)*4, BUNIT(8)*8,
     *   STOKES*8, COPCOD(14)*4, CSTOK(6)*4, CHTMP*8, CHTMP1*8
      INTEGER   I, IERR, IRETCD, INPRMS, NB, NALGOS, INC, J, I1, IROUND,
     *   K
      LOGICAL   REDUCE, T
      REAL      STOKI(2), STOKO, EPS, X, AXV
      REAL      XSEQ1, XDSK1, XSEQ2, XDSK2, XSEQ3, XDSK3, XSEQ4, XDSK4,
     *   SEQOUT, DSKOUT, C(20), BLC(7), TRC(7), GRIDCR, DOHIST
      HOLLERITH XNAM1(3), XCLS1(2), XNAM2(3), XCLS2(2), XNAM3(3),
     *   XCLS3(2), XNAM4(3), XCLS4(2), XNAMOU(3), XCLSOU(2), XALGO
      DOUBLE PRECISION    DAXV
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'COMB.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /INPARM/ XNAM1, XCLS1, XSEQ1, XDSK1, XNAM2, XCLS2, XSEQ2,
     *   XDSK2, XNAM3, XCLS3, XSEQ3, XDSK3, XNAM4, XCLS4, XSEQ4, XDSK4,
     *   GRIDCR, XNAMOU, XCLSOU, SEQOUT, DSKOUT, BLC, TRC, XALGO, C,
     *   DOHIST
      DATA EPS /0.2/
      DATA T /.TRUE./
      DATA NALGOS, ALGOS /14, 'SUM ','DIV ','SPIX','POLI','POLA',
     *   'MULT','OPTD','CLIP','REAL','IMAG','MEAN','RM  ','POLC','SUMM'/
      DATA BUNIT  /'JY/BEAM ', 'RATIO   ', 'SP INDEX',
     *             'JY/BEAM ', 'DEGREES ', 'PRODUCT ',
     *             'LN RATIO', 'RAD/M/M '/
      DATA STOKES /'STOKES  '/
      DATA COPCOD /'SUM ','DIV ','SPIX','POLI','POLA','MULT',
     *   'OPTD','CLIP','REAL','IMAG','MEAN','ROTM','POLC','SUMM'/
      DATA CSTOK /'PPOL','FPOL','PANG','SPIX','OPTD','ROTM'/
C-----------------------------------------------------------------------
C                                        Initialize file and header I/O
      ILUN(1) = 17
      ILUN(2) = 18
      ILUN(3) = 19
      ILUN(4) = 20
      OLUN(1) = 21
      OLUN(2) = 22
C
      CALL ZDCHIN (T)
      CALL VHDRIN
      NSCR = 0
      NCFILE = 0
      IER = 0
C                                        Get inputs from AIPS
      INPRMS = 72
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
      CTYPE = 0
      CALL H2CHR (4, 1, XALGO, ALGO)
      DO 20 I = 1,NALGOS
         IF (ALGO.EQ.ALGOS(I)) CTYPE = I
 20      CONTINUE
      IF (CTYPE.LE.0) THEN
         WRITE (MSGTXT,1020) ALGO
         CALL MSGWRT (8)
         GO TO 980
         END IF
      CALL RCOPY (20, C, CPARM)
      PLAIN = (CPARM(13).LE.0.0) .AND. (CPARM(14).LE.0.5) .AND.
     *   (CTYPE.NE.13)
      IF ((.NOT.PLAIN) .AND. ((CPARM(11).EQ.0.0) .OR.
     *   (CPARM(12).EQ.0.0))) THEN
         MSGTXT = 'SOME NOISE MUST BE SPECIFIED IN BPARM(1 and 2)'
         CALL MSGWRT (8)
         IER = 8
         GO TO 999
         END IF
      DOFILE(1) = .TRUE.
      DOFILE(2) = .TRUE.
      DOFILE(3) = CPARM(11).LT.0.0
      DOFILE(4) = CPARM(12).LT.0.0
      DOFILE(5) = .TRUE.
      DOFILE(6) = CPARM(13).GT.0.0
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
      CALL RFILL (4, 0.0, CBMULT)
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
            CALL H2CHR (8, 1, CATH(KHBUN), CHTMP)
            CALL CHLTOU (8, CHTMP)
            IF (CHTMP.EQ.'JY/BEAM') CBMULT(I) = CATR(KRBMJ)*CATR(KRBMN)
            END IF
 35      CONTINUE
      IF ((CBMULT(1).LE.0.0) .OR. (CTYPE.EQ.6)) THEN
         CALL RFILL (4, 1.0, CBMULT)
      ELSE
         DO 36 I = 2,4
            IF ((DOFILE(I)) .AND. (CBMULT(I).NE.1.0) .AND.
     *         (CBMULT(1).NE.1.0)) THEN
               CBMULT(I) = CBMULT(I) / CBMULT(1)
               IF (CBMULT(I).LE.0.0) CBMULT(I) = 1.0
               CBMULT(I) = 1.0 / CBMULT(I)
            ELSE
               CBMULT(I) = 1.0
               END IF
 36         CONTINUE
         CBMULT(1) = 1.0
         END IF
C                                        Set up some needed header vals
      REDUCE = .FALSE.
      NAX = CATI1(KIDIM)
      NAX2 = CATI2(KIDIM)
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
 80   MSGTXT = 'COMBIN: input maps coincident on reduced subimage only'
      IF (REDUCE) CALL MSGWRT (6)
      XX(1) = -1.0E30
      XX(2) = -1.0E30
      XN(1) = 1.0E30
      XN(2) = 1.0E30
      BFLAG = .FALSE.
      CALL COPY (256, CATI1, CATBLK)
      INC = 2
      IF (CTYPE.EQ.8) GO TO 110
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
C                                       default output Stokes
      IF ((STOKI(1).GT.0.) .OR. (STOKI(2).GT.0.)) THEN
         STOKO = 1.0
      ELSE IF ((STOKI(1).LT.-4.) .AND. (STOKI(2).LT.-4.)) THEN
         STOKO = -5
      ELSE
         STOKO = -1
         END IF
C                                        Various cases
      IF (STOKI(1).EQ.STOKI(2)) STOKO = STOKI(1)
      IF ((CTYPE.EQ.12) .AND. (STOKI(1).EQ.7) .AND. (STOKI(2).EQ.7))
     *   STOKO = 10.0
      IF ((STOKI(1).EQ.2.0) .AND. (STOKI(2).EQ.3.0)) THEN
         IF ((CTYPE.EQ.4) .OR. (CTYPE.EQ.13)) STOKO = 5.0
         IF (CTYPE.EQ.5) STOKO = 7.0
         END IF
      IF ((STOKI(1).EQ.5.0) .AND. (STOKI(2).EQ.1.0) .AND.
     *   (CTYPE.EQ.2)) STOKO = 6.0
      IF ((STOKI(2).EQ.1.0) .AND. (STOKI(1).NE.0.0) .AND.
     *   (STOKI(1).LT.5.0) .AND. (CTYPE.EQ.2)) STOKO = STOKI(1)
      IF ((CTYPE.EQ.3) .AND. (STOKI(1).EQ.STOKI(2))) STOKO = 8.0
      IF ((CTYPE.EQ.7) .AND. (STOKI(1).EQ.STOKI(2))) STOKO = 9.0
      IF ((CTYPE.EQ.9) .AND. (STOKI(1).EQ.5) .AND. (STOKI(2).EQ.7))
     *   STOKO = 2
      IF ((CTYPE.EQ.10) .AND. (STOKI(1).EQ.5) .AND. (STOKI(2).EQ.7))
     *   STOKO = 3
      IF (NPO(I).LE.1) THEN
         CATD(I1) = STOKO
         CATR(KRCRP+I-1) = 1.0
         END IF
      IF ((CTYPE.NE.5) .OR. ((STOKI(1).EQ.2) .AND. (STOKI(2).EQ.3)))
     *   GO TO 110
         WRITE (MSGTXT,1100)
         CALL MSGWRT (6)
C                                        Default out class
 110  IDCDEF = ' '
      J = STOKO
      IF (STOKO.EQ.STOKI(1)) J = 1
      IF (J.GT.4) IDCDEF(:4) = CSTOK(J-4)
      IF (J.LE.4) IDCDEF(:4) = COPCOD(CTYPE)
      IF ((J.EQ.5) .AND. (CTYPE.EQ.13)) IDCDEF(5:5) = 'C'
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
      IF (CTYPE.EQ.12) CALL CHR2H (8, BUNIT(8), 1, CATH(KHBUN))
      IF ((CTYPE.NE.1) .AND. (CTYPE.NE.4) .AND. (CTYPE.LT.8))
     *   CALL CHR2H (8, BUNIT(CTYPE), 1, CATH(KHBUN))
      CALL MCREAT (FCVOL(5), FCCNO(5), IROW, IERR)
      IDSEQ(1) = CATBLK(KIIMS)
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
      CALL KEYPCP (FCVOL(1), FCCNO(1), FCVOL(5), FCCNO(5), 0, ' ', IERR)
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
         CLSOUT(2)(6:6) = 'N'
         CALL CHR2H (6, CLSOUT(2), KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = IDSEQ(2)
         CATBLK(KIIMU) = NLUSER
         CALL MCREAT (FCVOL(6), FCCNO(6), IROW, IERR)
         IDSEQ(2) = CATBLK(KIIMS)
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
         CALL KEYPCP (FCVOL(1), FCCNO(1), FCVOL(6), FCCNO(6), 0, ' ',
     *      IERR)
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
 1010 FORMAT ('COMBIN: COULD NOT GET PARMS.  IER=',I7)
 1020 FORMAT ('DO NOT RECOGNIZE OPCODE = ',A4)
 1025 FORMAT ('COMBIN: COULD NOT OPEN MAP # ',I1,'  IER=',I7)
 1075 FORMAT ('COMBIN: INPUT MAPS ARE NOT COINCIDENT ON AXIS',I2)
 1100 FORMAT ('WARNING: angle map output not Polarization Angle')
 1110 FORMAT ('COMBIN: COULD NOT CREATE OUTPUT MAP.  IER=',I7)
 1135 FORMAT ('COMBIN: ERROR ', I3,' COPYING KEYWORDS - CONTINUING')
 1136 FORMAT ('COMBIN: COULD NOT OPEN OUTPUT MAP.  IER=',I7)
      END
      SUBROUTINE PCOMBN (COMB, C, IER)
C-----------------------------------------------------------------------
C   PCOMBN writes output describing the algorithm, prepares default
C   values and obtains necessary parameters for COMBN.
C   Inputs:
C      COMB      I     The algorithm type
C      C(20)     R     The algorithm parameters
C   Outputs:
C      IER       I     Error code: 0 --> okay
C                                  1 --> not okay
C      FREQ1     R     Frequency of first map
C      FREQ2     R     Frequency of second map
C      LNFREQ    R     Log(FREQ1/FREQ2)
C      FBLANK    R     Blanking value used in scratch file
C      FMAX1     R     Error limit in MAP(1)
C      FMAX2     R     Error limit in MAP(2)
C-----------------------------------------------------------------------
      CHARACTER CHTMP*8
      INTEGER   COMB, IER
      INTEGER   INC, I, I1, I2
      REAL      C(20), TEMP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'COMB.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      FMAX1 = MAX (ABS(CATR1(KRDMX)), ABS(CATR1(KRDMN)))
      FMAX2 = MAX (ABS(CATR2(KRDMX)), ABS(CATR2(KRDMN)))
C                                       Initialize
      IER = 0
      XBLFLG = C(8).LE.0.0
C                                       Skip to algorithm type
      GO TO (20, 40, 60, 80, 100, 120, 140, 160, 180, 200, 220, 240,
     *   260, 280), COMB
C                                       Linear combination 'SUM '
 20   IF (C(1).EQ.0.0) C(1) = 1.0
      IF (C(2).EQ.0.0) C(2) = 1.0
      WRITE (MSGTXT,1020) C(1), C(2), C(3)
      CALL MSGWRT (2)
      GO TO 900
C                                       Division 'DIV '
 40   IF (C(1).EQ.0.0) C(1) = 1.0
      WRITE (MSGTXT,1040) C(1), C(2)
      CALL MSGWRT (2)
      GO TO 900
C                                       Spectral index 'SPIX'
 60   IF (C(1).EQ.0.0) C(1) = 1.0
C                                       Find frequencies
      INC = 2
      I1 = -1
      I2 = -1
      DO 65 I = 1,7
         IPOINT = KHCTP+(I-1)*INC
         IF (I.LE.NAX) THEN
            CALL H2CHR (8, 1, CATH1(IPOINT), CHTMP)
            IF (CHTMP(:4).EQ.'FREQ') I1 = I - 1
            END IF
         IF (I.LE.NAX2) THEN
            CALL H2CHR (8, 1, CATH2(IPOINT), CHTMP)
            IF (CHTMP(:4).EQ.'FREQ') I2 = I - 1
            END IF
 65      CONTINUE
      IF ((I1.LT.0) .OR. (I2.LT.0)) THEN
         WRITE (MSGTXT,1065)
         GO TO 980
         END IF
      FREQ1 = (CATD1(KDCRV+I1) + CATR1(KRCIC+I1) * (IBLC(I1+1,1) -
     *   CATR1(KRCRP+I1))) / 1.0E6
      FREQ2 = (CATD2(KDCRV+I2) + CATR2(KRCIC+I2) * (IBLC(I2+1,2) -
     *   CATR2(KRCRP+I2))) / 1.E6
      LNFREQ = LOG (FREQ1 / FREQ2)
      WRITE (MSGTXT,1070) FREQ1, FREQ2
      CALL MSGWRT (2)
      WRITE (MSGTXT,1071) C(1), C(2)
      CALL MSGWRT (2)
      C(3) = MAX (0.0, C(3))
      C(4) = MAX (0.0, C(4))
      WRITE (MSGTXT,1072) C(3), C(4)
      CALL MSGWRT (2)
      IF (ABS(LNFREQ).LT.1.0E-20) THEN
         MSGTXT = 'CANNOT COMPUTE SPECTRAL INDEX, MAPS AT' //
     *      ' SAME FREQUENCY'
         GO TO 980
         END IF
      GO TO 900
C                                       RMS sum of two maps 'POLI'
 80   IF (C(1).EQ.0.0) C(1) = 1.0
      WRITE (MSGTXT,1080) C(1), C(2)
      CALL MSGWRT (2)
      GO TO 900
C                                       Position angle 'POLA'
 100  TEMP = 90. / 3.14159
      IF (C(1).EQ.0.0) C(1) = TEMP
      WRITE (MSGTXT,1100) C(1), C(2)
      CALL MSGWRT (2)
      IF (C(3).GT.0.0) THEN
         WRITE (MSGTXT,1101) C(3)
         CALL MSGWRT (2)
         END IF
C                                       Not degrees
      IF ((ABS(C(1)-TEMP).GE.0.5) .AND. (ABS(C(1)-2.0*TEMP).GE.0.5))
     *   THEN
         MSGTXT = 'WARNING: angle map not in degrees!'
         CALL MSGWRT (6)
         END IF
      IF (C(14).GE.1.5) THEN
         MSGTXT = 'WARNING: S/N not meaningful in angle images'
         CALL MSGWRT (6)
         END IF
      GO TO 900
C                                       Product 'MULT'
 120  IF (C(1).EQ.0.0) C(1) = 1.0
      WRITE (MSGTXT,1120) C(1), C(2)
      CALL MSGWRT (2)
      GO TO 900
C                                       Log ratio 'OPTD'
 140  IF (C(1).EQ.0.0) C(1) = 1.0
      IF (C(3).EQ.0.0) C(3) = 1.0
      WRITE (MSGTXT,1140) C(1), C(3), C(4), C(2)
      CALL MSGWRT (2)
      WRITE (MSGTXT,1072) C(5), C(6)
      CALL MSGWRT (2)
      GO TO 900
C                                       Clipping 'CLIP'
 160  IF (C(1).EQ.C(2)) THEN
         C(1) = 0.05 * FMAX2
         C(2) = -0.05 * FMAX2
         END IF
      IF (C(1).GE.C(2)) THEN
         WRITE (MSGTXT,1170) C(1), C(2)
      ELSE
         WRITE (MSGTXT,1171) C(1), C(2)
         END IF
      CALL MSGWRT (2)
      FREQ1 = C(1)
      FREQ2 = C(2)
      GO TO 900
C                                       REAL
 180  IF (C(1).EQ.0.0) C(1) = 1.0
      IF (C(2).EQ.0.0) C(2) = 2.0
      WRITE (MSGTXT,1180) C(1), C(2), C(3)
      CALL MSGWRT (2)
      GO TO 900
C                                       IMAG
 200  IF (C(1).EQ.0.0) C(1) = 1.0
      IF (C(2).EQ.0.0) C(2) = 2.0
      WRITE (MSGTXT,1200) C(1), C(2), C(3)
      CALL MSGWRT (2)
      GO TO 900
C                                       MEAN
 220  IF (C(1).EQ.0.0) C(1) = 1.0
      IF (C(2).EQ.0.0) C(2) = 1.0
      TEMP = C(1) + C(2)
      IF (TEMP.LE.0.0) THEN
         C(1) = 0.5
         C(2) = 0.5
      ELSE
         C(1) = C(1) / TEMP
         C(2) = C(2) / TEMP
         END IF
      WRITE (MSGTXT,1220) C(1), C(2)
      CALL MSGWRT (2)
      MSGTXT = '      If either blanked, use the other'
      CALL MSGWRT (2)
      GO TO 900
C                                       Rotation Measure 'RM  '
 240  IF (C(1).EQ.0.0) C(1) = 1.0
      INC = 2
      I1 = -1
      I2 = -1
      DO 245 I = 1,7
         IPOINT = KHCTP+(I-1)*INC
         IF (I.LE.NAX) THEN
            CALL H2CHR (8, 1, CATH1(IPOINT), CHTMP)
            IF (CHTMP(:4).EQ.'FREQ') I1 = I - 1
            END IF
         IF (I.LE.NAX2) THEN
            CALL H2CHR (8, 1, CATH2(IPOINT), CHTMP)
            IF (CHTMP(:4).EQ.'FREQ') I2 = I - 1
            END IF
 245     CONTINUE
      IF ((I1.LT.0) .OR. (I2.LT.0)) THEN
         WRITE (MSGTXT,1065)
         GO TO 980
         END IF
      FREQ1 = (CATD1(KDCRV+I1) + CATR1(KRCIC+I1) * (IBLC(I1+1,1) -
     *   CATR1(KRCRP+I1))) / 1.0E6
      FREQ2 = (CATD2(KDCRV+I2) + CATR2(KRCIC+I2) * (IBLC(I2+1,2) -
     *   CATR2(KRCRP+I2))) / 1.E6
      WRITE (MSGTXT,1250) FREQ1, FREQ2
      CALL MSGWRT (2)
      WRITE (MSGTXT,1251) C(1), C(2)
      CALL MSGWRT (2)
      WRITE (MSGTXT,1252) C(3)
      CALL MSGWRT (2)
      GO TO 900
C                                       RMS sum corrected 'POLC'
 260  IF (C(1).EQ.0.0) C(1) = 1.0
      IF ((C(11).EQ.0.0) .AND. (C(12).EQ.0.0)) THEN
         MSGTXT = 'NOISE DATA MUST BE PROVIDED FOR POLC'
         GO TO 980
         END IF
      WRITE (MSGTXT,1260) C(1), C(2)
      CALL MSGWRT (2)
      GO TO 900
C                                       Linear combination 'SUMM'
 280  IF (C(1).EQ.0.0) C(1) = 1.0
      IF (C(2).EQ.0.0) C(2) = 1.0
      WRITE (MSGTXT,1020) C(1), C(2), C(3)
      CALL MSGWRT (2)
      GO TO 900
C                                       Any blanking
 900  FMAX1 = 0.0
      FMAX2 = 0.0
      IF (XBLFLG) THEN
         MSGTXT  = 'Magic blanking used for clipped & illegal values'
      ELSE
         MSGTXT = 'Zero replaces clipped & illegal values'
         END IF
      CALL MSGWRT (2)
C                                       Blanking is on input map values
      IF (C(14).LE.0.5) THEN
         FMAX1 = C(9)
         FMAX2 = C(10)
         IF ((C(9).GT.0.0) .OR. (C(10).GT.0.0)) THEN
            WRITE (MSGTXT,1902) C(9), C(10)
            CALL MSGWRT (2)
            END IF
C                                       Blanking is on output S/N
      ELSE IF (C(14).GE.1.5) THEN
         WRITE (MSGTXT,1905) C(15)
         IF (C(15).GT.0.0) CALL MSGWRT (2)
C                                       Blanking is on output sigma
      ELSE
         WRITE (MSGTXT,1910) C(15)
         IF (C(15).GT.0.0) CALL MSGWRT (2)
         END IF
C                                       Map noise numbers
      IF ((C(14).GT.0.5) .OR. (C(13).GT.0.0)) THEN
         IF (C(11).LT.0.0) THEN
            MSGTXT = 'MAP(1) noise based on MAP(3)'
         ELSE
            WRITE (MSGTXT,1920) 1, C(11)
            END IF
         CALL MSGWRT (2)
         IF (C(12).LT.0.0) THEN
            MSGTXT = 'MAP(2) noise based on MAP(4)'
         ELSE
            WRITE (MSGTXT,1920) 2, C(12)
            END IF
         CALL MSGWRT (2)
         END IF
      GO TO 999
C                                       Error return
 980  CALL MSGWRT (7)
      IER = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Sum:',1PE11.3,'*Map(1) +',1PE11.3,'*Map(2) +',
     *   1PE11.3)
 1040 FORMAT ('Division:',1PE11.3,'*Map(1)/Map(2) +',1PE11.3)
 1065 FORMAT ('PCOMBN: FREQUENCY AXIS NOT FOUND')
 1070 FORMAT ('Spectral index: freq1=',F12.1,'  freq2=',F12.1,' MHz')
 1071 FORMAT (8X,1PE11.3,'*Sp.index +',1PE11.3)
 1072 FORMAT ('where Map(1)>',1PE11.3,' and Map(2)>',1PE11.3)
 1080 FORMAT ('Pol. flux:',1PE11.3,'*Sqrt(Map(1)**2 + Map(2)**2) +',
     *   1PE11.3)
 1100 FORMAT ('Pol. angle:',1PE11.3,'*ATAN2(Map(2),Map(1)) +',1PE11.3)
 1101 FORMAT ('blanked when MODULUS(MAP(1),MAP(2)) <',1PE11.3)
 1120 FORMAT ('Product:',1PE11.3,'*Map(1)*Map(2) +',1PE11.3)
 1140 FORMAT ('Opacity:',1PE11.3,'*log(',1PE11.3,'*Map(1)/Map(2)+',
     *   1PE11.3,')  +',1PE11.3)
 1170 FORMAT ('Clipping Map(1) where',1PE11.3,' > Map(2) >',1PE11.3)
 1171 FORMAT ('Clipping Map(1) where Map(2) <',1PE11.3,' or >',1PE11.3)
 1180 FORMAT ('Real part:',1PE11.3,'*Map(1)*Cos(',1PE11.3,'*Map(2))+',
     *   1PE11.3)
 1200 FORMAT ('Imag part:',1PE11.3,'*Map(1)*Sin(',1PE11.3,'*Map(2))+',
     *   1PE11.3)
 1220 FORMAT ('Mean:',1PE11.3,'*Map(1) +',1PE11.3,'*Map(2)')
 1250 FORMAT ('Rotation measure; freq1 = ',F12.1,' freq2 = ',F12.1,
     *    ' MHz')
 1251 FORMAT (8X,1PE11.3,' * Rot.meas. +',1PE11.3)
 1252 FORMAT (8X,'Angle differences centered on',F8.2)
 1260 FORMAT ('Poli corr:',1PE11.3,'*Sqrt(Map(1)**2 + Map(2)**2) +',
     *   1PE11.3)
 1902 FORMAT ('Clipping based on',1PE11.3,' in Map(1) or ',1PE11.3,
     *   ' in Map(2)')
 1905 FORMAT ('Blanking done if output S/N is less than',F8.4)
 1910 FORMAT ('Blanking done if output sigma exceeds',F8.4)
 1920 FORMAT ('Using MAP(',I1,') noise level',1PE11.3)
      END
      SUBROUTINE COMBN (NX, V1, V2, COMB, C, R1)
C-----------------------------------------------------------------------
C   COMBN combines two pixel rows V1 and V2 to form result row R1   The
C   algorithm is specified by COMB and any parameters are stored in the
C   array C.
C   Inputs:
C      NX     I       Number of pixels in each vector.
C      V1     R(NX)   The pixel values of the input map 1
C      V2     R(NX)   The pixel values of the input map 2
C      COMB   I       The algorithm type: see Main pgm
C      C      R(20)   Up to 20 inputs parameters
C   Outputs:
C      R1     R(NX)   The result of the combination
C-----------------------------------------------------------------------
      INTEGER   NX, COMB
      REAL      V1(*), V2(*), R1(*), C(20)
C
      INTEGER   I
      REAL      R, DENOM, BRES, ST, CT, RLAM1, RLAM2, CONST, ROD, RES0,
     *   RES1, RES2, RES3, RES4
      LOGICAL   BLANK, BLANK1, BLANK2
      INCLUDE 'COMB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Value for blanked on output
      BRES = FBLANK
      IF (.NOT.XBLFLG) BRES = 0.0
      IF (CBMULT(2).NE.1.0) THEN
         DO 10 I = 1,NX
            IF (V2(I).NE.FBLANK) V2(I) = CBMULT(2) * V2(I)
 10         CONTINUE
         END IF
C                                       Skip to appropriate place
      GO TO (100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600,
     *   650, 700, 750), COMB
C                                       Linear combination 'SUM '
 100  DO 110 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2)
         IF (.NOT.BLANK) THEN
            R1(I) = C(1) * V1(I) + C(2) * V2(I) + C(3)
         ELSE
            R1(I) = FBLANK
            END IF
 110     CONTINUE
      GO TO 900
C                                       Division
 150  DO 160 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2) .OR.
     *      (V2(I).EQ.0.0)
         IF (.NOT.BLANK) THEN
            R1(I) = C(1) * V1(I) / V2(I) + C(2)
         ELSE
            R1(I) = FBLANK
            END IF
 160     CONTINUE
      GO TO 900
C                                       Spectral index 'SPIX'
 200  R = C(1) / LNFREQ
      DO 210 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2) .OR.
     *      (V1(I).LE.C(3)) .OR. (V2(I).LE.C(4))
         IF (.NOT.BLANK) THEN
            R1(I) = R * LOG (V1(I) / V2(I)) + C(2)
         ELSE
            R1(I) = FBLANK
            END IF
 210     CONTINUE
      GO TO 900
C                                       Amplitude 'POLI'
 250  DO 260 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2)
         IF (.NOT.BLANK) THEN
            R1(I) = C(1) * SQRT(V1(I)**2 + V2(I)**2) + C(2)
         ELSE
            R1(I) = FBLANK
            END IF
 260     CONTINUE
      GO TO 900
C                                       Angle 'POLA'
 300  DO 310 I = 1,NX
         DENOM = V1(I)**2 + V2(I)**2
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2) .OR.
     *      (DENOM.LT.1.E-15) .OR. (DENOM.LT.C(3)*C(3))
         IF (.NOT.BLANK) THEN
            R1(I) = C(1) * ATAN2 (V2(I), V1(I)) + C(2)
         ELSE
            R1(I) = FBLANK
            END IF
 310     CONTINUE
      GO TO 900
C                                       Product 'MULT'
 350  DO 360 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2)
         IF (.NOT.BLANK) THEN
            R1(I) = C(1) * V1(I) * V2(I) + C(2)
         ELSE
            R1(I) = FBLANK
            END IF
 360     CONTINUE
      GO TO 900
C                                       Log ratio (Opacity) 'OPTD'
 400  DO 410 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2) .OR.
     *      (V1(I).LE.C(5)) .OR. (V2(I).LE.C(6))
         IF (.NOT.BLANK) THEN
            R = C(3) * V1(I) / V2(I) + C(4)
            BLANK = (R.LE.0.0)
            END IF
         IF (.NOT.BLANK) THEN
            R1(I) = C(1) * LOG (R) + C(2)
         ELSE
            R1(I) = FBLANK
            END IF
 410     CONTINUE
      GO TO 900
C                                       Clipping 'CLIP'
 450  DO 460 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2)
         IF (.NOT.BLANK) THEN
            IF (FREQ1.LT.FREQ2) BLANK = (V2(I).GT.FREQ2) .OR.
     *         (V2(I).LT.FREQ1)
            IF ((FREQ1.GT.V2(I)) .AND. (V2(I).GT.FREQ2)) BLANK = .TRUE.
            END IF
         IF (.NOT.BLANK) THEN
            R1(I) = V1(I)
         ELSE
            R1(I) = FBLANK
            END IF
 460     CONTINUE
      GO TO 900
C                                       Real part 'REAL'
 500  R = C(2) * 0.01745329
      DO 510 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2)
         IF (.NOT.BLANK) THEN
            ST = R * V2(I)
            CT = COS (ST)
            ST = SIN (ST)
            R1(I) = C(1) * V1(I) * CT + C(3)
         ELSE
            R1(I) = FBLANK
            END IF
 510     CONTINUE
      GO TO 900
C                                       Imaginary part 'IMAG'
 550  R = C(2) * 0.01745329
      DO 560 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2)
         IF (.NOT.BLANK) THEN
            ST = R * V2(I)
            CT = COS (ST)
            ST = SIN (ST)
            R1(I) = C(1) * V1(I) * ST + C(3)
         ELSE
            R1(I) = FBLANK
            END IF
 560     CONTINUE
      GO TO 900
C                                       'MEAN'
 600  DO 610 I = 1,NX
         IF ((V1(I).EQ.FBLANK) .OR. (ABS(V1(I)).LT.FMAX1)) V1(I) =
     *      FBLANK
         IF ((V2(I).EQ.FBLANK) .OR. (ABS(V2(I)).LT.FMAX2)) V2(I) =
     *      FBLANK
         BLANK = (V1(I).EQ.FBLANK) .AND. (V2(I).EQ.FBLANK)
         IF (.NOT.BLANK) THEN
            IF (V1(I).EQ.FBLANK) THEN
               R1(I) = V2(I)
            ELSE IF (V2(I).EQ.FBLANK) THEN
               R1(I) = V1(I)
            ELSE
               R1(I) = C(1) * V1(I) + C(2) * V2(I)
               END IF
         ELSE
            R1(I) = FBLANK
            END IF
 610     CONTINUE
      GO TO 900
C                                       Rotation Measure 'RM  '
 650  RES0 = C(3)
      RLAM1 = 300. / FREQ1
      RLAM2 = 300. / FREQ2
      CONST = (RLAM1**2 - RLAM2**2) * 57.29578
      ROD = -C(2) * CONST
      CONST = C(1) / CONST
      DO 660 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2)
         IF (.NOT.BLANK) THEN
            R = V1(I) - V2(I) + ROD
            RES1 = R + 180.
            RES2 = R - 180.
            RES3 = R + 360.
            RES4 = R - 360.
            IF (ABS(RES1-RES0).LT.ABS(R-RES0)) R = RES1
            IF (ABS(RES2-RES0).LT.ABS(R-RES0)) R = RES2
            IF (ABS(RES3-RES0).LT.ABS(R-RES0)) R = RES3
            IF (ABS(RES4-RES0).LT.ABS(R-RES0)) R = RES4
            R1(I) = R * CONST + 2.0 * C(2)
         ELSE
            R1(I) = FBLANK
            END IF
 660     CONTINUE
      GO TO 900
C                                       Amplitude corrected 'POLC'
 700  MSGTXT = 'CANNOT DO POLARIZATION CORRECTION WITH NO NOISE'
      CALL MSGWRT (8)
      CALL RFILL (NX, FBLANK, R1)
      GO TO 900
C                                       Linear combination 'SUMM'
 750  DO 760 I = 1,NX
         BLANK1 = V1(I).EQ.FBLANK .OR. ABS(V1(I)).LT.FMAX1
         BLANK2 = V2(I).EQ.FBLANK .OR. ABS(V2(I)).LT.FMAX2
         BLANK = BLANK1 .AND. BLANK2
         IF (BLANK) THEN
            R1(I) = FBLANK
         ELSE
            IF(BLANK1) THEN
               R1(I) = C(2) * V2(I) + C(3)
            ELSE
               IF(BLANK2) THEN
                  R1(I) = C(1) * V1(I) + C(3)
               ELSE
                  R1(I) = C(1) * V1(I) + C(2) * V2(I) + C(3)
                  END IF
               END IF
            END IF
 760     CONTINUE
      GO TO 900
C                                       Common processing
 900  DO 910 I = 1,NX
         BLANK = (R1(I).EQ.FBLANK)
         IF (BLANK) R1(I) = BRES
C                                       Get max/min
         IF (R1(I).NE.FBLANK) THEN
            XX(1) = MAX (XX(1), R1(I))
            XN(1) = MIN (XN(1), R1(I))
         ELSE
            BFLAG = .TRUE.
            END IF
 910     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE XCOMBN (NX, V1, V2, S1, S2, COMB, C, R1, R2)
C-----------------------------------------------------------------------
C   XCOMBN combines two pixel rows V1 and V2 with noise rows S1 and S2
C   to form result row R1 and noise row R2.  The algorithm is specified
C   by COMB and any parameters are stored in the array C.
C   Inputs:
C      NX     I       Number of pixels in each vector.
C      V1     R(NX)   The pixel values of the input map 1
C      V2     R(NX)   The pixel values of the input map 2
C      S1     R(NX)   The noise values of the input map 1
C      S2     R(NX)   The noise values of the input map 2
C      COMB   I       The algorithm type: see Main pgm
C      C      R(20)   Up to 20 inputs parameters
C   Outputs:
C      R1     R(NX)   The result of the combination
C      R2     R(NX)   The noise of the combination
C-----------------------------------------------------------------------
      INTEGER   NX, COMB
      REAL      V1(*), V2(*), S1(*), S2(*), R1(*), R2(*), C(20)
C
      INTEGER   I
      REAL      R, DENOM, SN, BRES, ST, CT, RLAM1, RLAM2,
     *   CONST, ROD, RES0, RES1, RES2, RES3, RES4
      LOGICAL   CLPSIG, CLPSN, BLANK, BLANK1, BLANK2
      INCLUDE 'COMB.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      CLPSN  = (C(14).GE.1.5) .AND. (C(15).GT.0.0)
      CLPSIG = (C(14).LT.1.5) .AND. (C(14).GT.0.5) .AND. (C(15).GT.0.0)
C                                       Value for blanked on output
      BRES = FBLANK
      IF (.NOT.XBLFLG) BRES = 0.0
C                                       scale
      IF (CBMULT(2).NE.1.0) THEN
         DO 20 I = 1,NX
            IF (V2(I).NE.FBLANK) V2(I) = CBMULT(2) * V2(I)
 20         CONTINUE
         END IF
      IF (CBMULT(3).NE.1.0) THEN
         DO 30 I = 1,NX
            IF (S1(I).NE.FBLANK) S1(I) = CBMULT(3) * S1(I)
 30         CONTINUE
         END IF
      IF (CBMULT(4).NE.1.0) THEN
         DO 40 I = 1,NX
            IF (S2(I).NE.FBLANK) S2(I) = CBMULT(4) * S2(I)
 40         CONTINUE
         END IF
C                                       Skip to appropriate place
      GO TO (100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600,
     *   650, 700, 750), COMB
C                                       Linear combination 'SUM '
 100  DO 110 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (S1(I).EQ.FBLANK) .OR. (S2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2)
         IF (.NOT.BLANK) THEN
            R1(I) = C(1) * V1(I) + C(2) * V2(I) + C(3)
            R2(I) = SQRT ((C(1)*S1(I))**2 + (C(2)*S2(I))**2)
         ELSE
            R1(I) = FBLANK
            R2(I) = FBLANK
            END IF
 110     CONTINUE
      GO TO 900
C                                       Division
 150  DO 160 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (S1(I).EQ.FBLANK) .OR. (S2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2) .OR.
     *      (V2(I).EQ.0.0)
         IF (.NOT.BLANK) THEN
            R2(I) = C(1) * SQRT((S1(I)*V2(I))**2 + (S2(I)*V1(I))**2) /
     *         V2(I)**2
            R1(I) = C(1) * V1(I) / V2(I) + C(2)
         ELSE
            R1(I) = FBLANK
            R2(I) = FBLANK
            END IF
 160     CONTINUE
      GO TO 900
C                                       Spectral index 'SPIX'
 200  R = C(1) / LNFREQ
      DO 210 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (S1(I).EQ.FBLANK) .OR. (S2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2) .OR.
     *      (V1(I).LE.C(3)) .OR. (V2(I).LE.C(4))
         IF (.NOT.BLANK) THEN
            R2(I) = R * SQRT ((S1(I)/V1(I))**2 + (S2(I)/V2(I))**2)
            R1(I) = R * LOG (V1(I) / V2(I)) + C(2)
         ELSE
            R1(I) = FBLANK
            R2(I) = FBLANK
            END IF
 210     CONTINUE
      GO TO 900
C                                       Amplitude 'POLI'
 250  DO 260 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (S1(I).EQ.FBLANK) .OR. (S2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2)
         IF (.NOT.BLANK) THEN
            IF ((V1(I).EQ.0.0) .AND. (V2(I).EQ.0.0)) THEN
               R2(I) = C(1) * SQRT (S1(I)**2 + S2(I)**2)
            ELSE
               R2(I) = C(1) * SQRT (((S1(I)*V1(I))**2 +
     *            (S2(I)*V2(I))**2) / (V1(I)**2 + V2(I)**2))
               END IF
            R1(I) = C(1) * SQRT(V1(I)**2 + V2(I)**2) + C(2)
         ELSE
            R1(I) = FBLANK
            R2(I) = FBLANK
            END IF
 260     CONTINUE
      GO TO 900
C                                       Angle 'POLA'
 300  DO 310 I = 1,NX
         DENOM = V1(I)**2 + V2(I)**2
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (S1(I).EQ.FBLANK) .OR. (S2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2) .OR.
     *      (DENOM.LT.1.E-15) .OR. (DENOM.LT.C(3)*C(3))
         IF (.NOT.BLANK) THEN
            R2(I) = C(1) * SQRT ((S1(I)*V2(I))**2 + (S2(I)*V1(I))**2) /
     *         DENOM
            R1(I) = C(1) * ATAN2 (V2(I), V1(I)) + C(2)
         ELSE
            R1(I) = FBLANK
            R2(I) = FBLANK
            END IF
 310     CONTINUE
      GO TO 900
C                                       Product 'MULT'
 350  DO 360 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (S1(I).EQ.FBLANK) .OR. (S2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2)
         IF (.NOT.BLANK) THEN
            R2(I) = C(1) * SQRT ((S1(I)*V2(I))**2 + (S2(I)*V1(I))**2)
            R1(I) = C(1) * V1(I) * V2(I) + C(2)
         ELSE
            R1(I) = FBLANK
            R2(I) = FBLANK
            END IF
 360     CONTINUE
      GO TO 900
C                                       Log ratio (Opacity) 'OPTD'
 400  DO 410 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (S1(I).EQ.FBLANK) .OR. (S2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2) .OR.
     *      (V1(I).LE.C(5)) .OR. (V2(I).LE.C(6))
         IF (.NOT.BLANK) THEN
            R = C(3) * V1(I) / V2(I) + C(4)
            BLANK = (R.LE.0.0)
            END IF
         IF (.NOT.BLANK) THEN
            R2(I) = C(1) * SQRT ((S1(I)/V1(I))**2 + (S2(I)/V2(I))**2)
            R2(I) = R2(I) / (1.0 + (C(4) * V2(I)) / (V1(I) * C(3)))
            R1(I) = C(1) * LOG (R) + C(2)
         ELSE
            R1(I) = FBLANK
            R2(I) = FBLANK
            END IF
 410     CONTINUE
      GO TO 900
C                                       Clipping 'CLIP'
 450  DO 460 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (S1(I).EQ.FBLANK) .OR. (S2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2)
         IF (.NOT.BLANK) THEN
            IF (FREQ1.LT.FREQ2) BLANK = (V2(I).GT.FREQ2) .OR.
     *         (V2(I).LT.FREQ1)
            IF ((FREQ1.GT.V2(I)) .AND. (V2(I).GT.FREQ2)) BLANK = .TRUE.
            END IF
         IF (.NOT.BLANK) THEN
            R2(I) = C(1) * SQRT ((S1(I)*V2(I))**2 + (S2(I)*V1(I))**2)
            R1(I) = V1(I)
         ELSE
            R1(I) = FBLANK
            R2(I) = FBLANK
            END IF
 460     CONTINUE
      GO TO 900
C                                       Real part 'REAL'
 500  R = C(2) * 0.01745329
      DO 510 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (S1(I).EQ.FBLANK) .OR. (S2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2)
         IF (.NOT.BLANK) THEN
            ST = R * V2(I)
            CT = COS (ST)
            ST = SIN (ST)
            R2(I) = C(1) * SQRT ((S1(I)*CT)**2 + (R*V1(I)*S2(I)*ST)**2)
            R1(I) = C(1) * V1(I) * CT + C(3)
         ELSE
            R1(I) = FBLANK
            R2(I) = FBLANK
            END IF
 510     CONTINUE
      GO TO 900
C                                       Imaginary part 'IMAG'
 550  R = C(2) * 0.01745329
      DO 560 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (S1(I).EQ.FBLANK) .OR. (S2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2)
         IF (.NOT.BLANK) THEN
            ST = R * V2(I)
            CT = COS (ST)
            ST = SIN (ST)
            R2(I) = C(1) * SQRT ((S1(I)*ST)**2 + (R*V1(I)*S2(I)*CT)**2)
            R1(I) = C(1) * V1(I) * ST + C(3)
         ELSE
            R1(I) = FBLANK
            R2(I) = FBLANK
            END IF
 560     CONTINUE
      GO TO 900
C                                       'MEAN'
 600  DO 610 I = 1,NX
         IF ((V1(I).EQ.FBLANK) .OR. (ABS(V1(I)).LT.FMAX1) .OR.
     *      (S1(I).EQ.FBLANK)) V1(I) = FBLANK
         IF ((V2(I).EQ.FBLANK) .OR. (ABS(V2(I)).LT.FMAX2) .OR.
     *      (S1(I).EQ.FBLANK)) V2(I) = FBLANK
         BLANK = (V1(I).EQ.FBLANK) .AND. (V2(I).EQ.FBLANK)
         IF (.NOT.BLANK) THEN
            IF (V1(I).EQ.FBLANK) THEN
               R1(I) = V2(I)
               R2(I) = S2(I)
            ELSE IF (V2(I).EQ.FBLANK) THEN
               R1(I) = V1(I)
               R2(I) = S1(I)
            ELSE
               R1(I) = C(1) * V1(I) + C(2) * V2(I)
               R2(I) = SQRT ((C(1)*S1(I))**2 + (C(2)*S2(I))**2)
               END IF
         ELSE
            R1(I) = FBLANK
            R2(I) = FBLANK
            END IF
 610     CONTINUE
      GO TO 900
C                                       Rotation Measure 'RM  '
 650  RES0 = C(3)
      RLAM1 = 300. / FREQ1
      RLAM2 = 300. / FREQ2
      CONST = (RLAM1**2 - RLAM2**2) * 57.29578
      ROD = -C(2) * CONST
      CONST = C(1) / CONST
      DO 660 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (S1(I).EQ.FBLANK) .OR. (S2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2)
         IF (.NOT.BLANK) THEN
            R = V1(I) - V2(I) + ROD
            RES1 = R + 180.
            RES2 = R - 180.
            RES3 = R + 360.
            RES4 = R - 360.
            IF (ABS(RES1-RES0).LT.ABS(R-RES0)) R = RES1
            IF (ABS(RES2-RES0).LT.ABS(R-RES0)) R = RES2
            IF (ABS(RES3-RES0).LT.ABS(R-RES0)) R = RES3
            IF (ABS(RES4-RES0).LT.ABS(R-RES0)) R = RES4
            R1(I) = R * CONST + 2.0 * C(2)
            R2(I) = CONST * SQRT (S1(I)**2 + S2(I)**2)
         ELSE
            R1(I) = FBLANK
            R2(I) = FBLANK
            END IF
 660     CONTINUE
      GO TO 900
C                                       Amplitude corrected 'POLC'
 700  DO 710 I = 1,NX
         BLANK = (V1(I).EQ.FBLANK) .OR. (V2(I).EQ.FBLANK) .OR.
     *      (S1(I).EQ.FBLANK) .OR. (S2(I).EQ.FBLANK) .OR.
     *      (ABS(V1(I)).LT.FMAX1) .OR. (ABS(V2(I)).LT.FMAX2)
         IF (.NOT.BLANK) THEN
            IF ((V1(I).EQ.0.0) .AND. (V2(I).EQ.0.0)) THEN
               R2(I) = C(1) * SQRT (S1(I)**2 + S2(I)**2)
            ELSE
               R2(I) = C(1) * SQRT (((S1(I)*V1(I))**2 +
     *            (S2(I)*V2(I))**2) / (V1(I)**2 + V2(I)**2))
               END IF
            R1(I) = C(1) * SQRT(V1(I)**2 + V2(I)**2) + C(2)
            CALL PDBIAS (R1(I), R2(I))
         ELSE
            R1(I) = FBLANK
            R2(I) = FBLANK
            END IF
 710     CONTINUE
      GO TO 900
C                                       Linear combination 'SUMM'
 750  DO 760 I = 1,NX
         BLANK1 = V1(I).EQ.FBLANK .OR. ABS(V1(I)).LT.FMAX1
         BLANK2 = V2(I).EQ.FBLANK .OR. ABS(V2(I)).LT.FMAX2
         BLANK = BLANK1 .AND. BLANK2
         IF (BLANK) THEN
            R1(I) = FBLANK
            R2(I) = FBLANK
         ELSE
            IF(BLANK1) THEN
               R1(I) = C(2) * V2(I) + C(3)
               R2(I) = C(2) * S2(I)
            ELSE
               IF(BLANK2) THEN
                  R1(I) = C(1) * V1(I) + C(3)
                  R2(I) = C(1) * S1(I)
               ELSE
                  R1(I) = C(1) * V1(I) + C(2) * V2(I) + C(3)
                  R2(I) = SQRT ((C(1)*S1(I))**2 + (C(2)*S2(I))**2)
                  END IF
               END IF
            END IF
 760     CONTINUE
      GO TO 900
C                                       Common processing
 900  DO 910 I = 1,NX
         BLANK = (R1(I).EQ.FBLANK) .OR. (R2(I).EQ.FBLANK)
         IF (.NOT.BLANK) THEN
            R2(I) = ABS (R2(I))
            IF ((CLPSN) .AND. (R2(I).GT.0.0)) THEN
               SN = ABS (R1(I) / R2(I))
               BLANK = SN.LT.C(15)
            ELSE IF (CLPSIG) THEN
               BLANK = R2(I).GT.C(15)
               END IF
            IF (C(16).GT.0.0) R2(I) = MIN (R2(I), C(16))
            END IF
         IF (BLANK) THEN
            R1(I) = BRES
            R2(I) = BRES
            END IF
C                                       Get max/min
         IF (R1(I).NE.FBLANK) THEN
            XX(1) = MAX (XX(1), R1(I))
            XN(1) = MIN (XN(1), R1(I))
            XX(2) = MAX (XX(2), R2(I))
            XN(2) = MIN (XN(2), R2(I))
         ELSE
            BFLAG = .TRUE.
            END IF
 910     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE COMBHI (IRET)
C-----------------------------------------------------------------------
C   COMBHI creates and writes the HI file associated with task COMB.
C   Outputs:
C      IRET   I      > 0 => output all blanks
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER HILINE*72, ABUNIT(3)*8, WORD(14)*8, FTYPE(2)*8, NOTTYP*2
      INTEGER   NHISTF, IHDLUN, IHSLUN, I, IER, IERR, IBUFF1(256), J,
     *    IBUFF2(256), IPTR, ITEMP, FU, NONOT
      REAL      TEMP
      LOGICAL   TRUE, FALSE
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'COMB.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (IBUFF1(1),IROW(1,1)),  (IBUFF2(1),IROW(257,1))
      DATA NHISTF, IHDLUN, IHSLUN /2,27,28/
      DATA ABUNIT /'ANGLE   ', 'RADIANS ', 'ROT.MS. '/
      DATA WORD /'Lin.comb', 'Division', 'Spec.inx',
     *   'Pol.int ', 'Pol.ang ', 'Product ', 'Opacity ',
     *   'Clipping', 'Real    ', 'Imag    ', 'Mean    ',
     *   'Rot.meas', 'Poli Cor','Lin.blnk'/
      DATA TRUE, FALSE /.TRUE.,.FALSE./
      DATA FTYPE /'IMAGE','NOISE'/
      DATA NONOT, NOTTYP /1, 'CC'/
C-----------------------------------------------------------------------
C                                       Initialize HITAB
      CALL HIINIT (NHISTF)
      IRET = 0
      DO 200 J = 2,1,-1
         IF (DOFILE(4+J)) THEN
C                                       Test validity of result
            IF (XX(J).LT.XN(J)) THEN
               WRITE (MSGTXT,1000) FTYPE(J)
               CALL MSGWRT (6)
               BFLAG = .TRUE.
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
            IF ((CTYPE.EQ.5) .AND. (ABS(CPARM(1)-TEMP).GT.0.5) .AND.
     *         (ABS(CPARM(1)-2.*TEMP).GT.0.5)) FU = 1
            IF ((CTYPE.EQ.5) .AND. (ABS(CPARM(1)-1.0).LE.0.05)) FU = 2
            IF ((CTYPE.EQ.12) .AND. (ABS(CPARM(1)-1.0).GT.0.05)) FU = 3
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
 30                  WRITE (MSGTXT,1030) I, IERR
                     CALL MSGWRT (6)
                     WRITE (HILINE,1031)
                     CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
                     IF (IERR.NE.0) GO TO 190
               ELSE
                  WRITE (HILINE,1032) TSKNAM, I
                  CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
                  IF (IERR.NE.0) GO TO 190
                  END IF
 50            CONTINUE
            WRITE (HILINE,1050)
            CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            WRITE (HILINE,1055) TSKNAM, RLSNAM
            CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
C                                       Add input parameters
            CALL HENCO1 (TSKNAM, NAMIN(1), CLSIN(1), ISSEQ(1), FCVOL(1),
     *         IHDLUN, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            CALL HENCO2 (TSKNAM, NAMIN(2), CLSIN(2), ISSEQ(2), FCVOL(2),
     *         IHDLUN, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            CALL HENCOO (TSKNAM, NAMOUT(J), CLSOUT(J), IDSEQ(J),
     *         FCVOL(4+J), IHDLUN, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            WRITE (HILINE,1090) TSKNAM, IUSER
            CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            WRITE (HILINE,1092) TSKNAM, ALGO, WORD(CTYPE)
            CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
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
            IF ((CTYPE.EQ.1) .OR. (CTYPE.EQ.9) .OR. (CTYPE.EQ.10) .OR.
     *         (CTYPE.EQ.12)) THEN
               WRITE (HILINE,1100) TSKNAM, (I, CPARM(I), I = 1,3)
            ELSE
               WRITE (HILINE,1101) TSKNAM, (I, CPARM(I), I = 1,2)
               END IF
            CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            IF (CTYPE.EQ.7) THEN
               WRITE (HILINE,1101) TSKNAM, (I, CPARM(I), I = 3,4)
               CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
               IF (IERR.NE.0) GO TO 190
               WRITE (HILINE,1102) TSKNAM, (I, CPARM(I), I = 5,6)
               CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
               IF (IERR.NE.0) GO TO 190
               END IF
            IF (CTYPE.EQ.3) THEN
               WRITE (HILINE,1102) TSKNAM, (I, CPARM(I), I = 3,4)
               CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
               IF (IERR.NE.0) GO TO 190
               END IF
            IF (CTYPE.EQ.5) THEN
               WRITE (HILINE,1103) TSKNAM, (I, CPARM(I), I = 3,3)
               CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
               IF (IERR.NE.0) GO TO 190
               END IF
C                                       blanking
            IF (CPARM(8).LE.0.0) THEN
               WRITE (HILINE,1110) TSKNAM
            ELSE
               WRITE (HILINE,1111) TSKNAM
               END IF
            CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
C                                       Clean beam scaling
            IF (CBMULT(2).NE.1.0) THEN
               WRITE (HILINE,1112) TSKNAM, 2, CBMULT(2)
               CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
               IF (IERR.NE.0) GO TO 190
               MSGTXT = HILINE(7:)
               CALL MSGWRT (3)
               END IF
C                                       clip on inputs
            IF (CPARM(14).LE.0.5) THEN
               IF ((CPARM(9).GT.0.0) .OR. (CPARM(10).GT.0.0)) THEN
                  WRITE (HILINE,1113) TSKNAM, (I, CPARM(I), I = 9,10)
                  CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
                  IF (IERR.NE.0) GO TO 190
                  END IF
               END IF
C                                       Noise used
            IF (.NOT.PLAIN) THEN
               IF ((CPARM(13).GT.0.0) .AND. (J.EQ.2)) THEN
                  WRITE (HILINE,1120) TSKNAM
                  CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
                  IF (IERR.NE.0) GO TO 190
                  END IF
C                                       Clean beam scaling
               IF (CBMULT(3).NE.1.0) THEN
                  WRITE (HILINE,1112) TSKNAM, 3, CBMULT(3)
                  CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
                  IF (IERR.NE.0) GO TO 190
                  MSGTXT = HILINE(7:)
                  CALL MSGWRT (3)
                  END IF
               IF (CBMULT(4).NE.1.0) THEN
                  WRITE (HILINE,1112) TSKNAM, 4, CBMULT(4)
                  CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
                  IF (IERR.NE.0) GO TO 190
                  MSGTXT = HILINE(7:)
                  CALL MSGWRT (3)
                  END IF
C                                       Noise values
               IF (CPARM(11).LT.0.0) THEN
                  HILINE = TSKNAM // ' / Noise 1 based on image:'
                  CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
                  IF (IERR.NE.0) GO TO 190
                  CALL HENCO3 (TSKNAM, NAMIN(3), CLSIN(3), ISSEQ(3),
     *               FCVOL(3), IHDLUN, IBUFF2, IERR)
               ELSE
                  WRITE (HILINE,1130) TSKNAM, CPARM(11), 1
                  CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
                  END IF
               IF (IERR.NE.0) GO TO 190
               IF (CPARM(12).LT.0.0) THEN
                  HILINE = TSKNAM // ' / Noise 2 based on image:'
                  CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
                  IF (IERR.NE.0) GO TO 190
                  CALL HENCO4 (TSKNAM, NAMIN(4), CLSIN(4), ISSEQ(4),
     *               FCVOL(4), IHDLUN, IBUFF2, IERR)
               ELSE
                  WRITE (HILINE,1130) TSKNAM, CPARM(12), 2
                  CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
                  END IF
               IF (IERR.NE.0) GO TO 190
C                                       Noise limit
               IF ((CPARM(13).GT.0.0) .AND. (CPARM(16).GT.0.0)) THEN
                  WRITE (HILINE,1131) TSKNAM, CPARM(16)
                  CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
                  IF (IERR.NE.0) GO TO 190
                  END IF
C                                       Noise clipping
               IF ((CPARM(14).GT.0.5) .AND. (CPARM(15).GT.0.0)) THEN
                  WRITE (HILINE,1140) TSKNAM, CPARM(15)
                  IF (CPARM(14).GE.1.5) WRITE (HILINE,1141) TSKNAM,
     *               CPARM(15)
                  CALL HIADD (IHDLUN, HILINE, IBUFF2, IERR)
                  IF (IERR.NE.0) GO TO 190
                  END IF
               END IF
C                                       Close HI file
 190        CALL HICLOS (IHDLUN, TRUE, IBUFF2, IER)
            WRITE (MSGTXT,1900) FTYPE(J)
            IF ((IER.NE.0) .OR. (IERR.NE.0)) WRITE (MSGTXT,1901)
     *         FTYPE(J), IERR, IER
            CALL MSGWRT (2)
C                                       copy tables
            CALL ALLTAB (NONOT, NOTTYP, 44, 45, FCVOL(1), FCVOL(4+J),
     *         FCCNO(1), FCCNO(4+J), CATBLK, IBUFF1, IBUFF2, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1905) FTYPE(J), IERR
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
 1000 FORMAT ('OUTPUT ',A5,' FILE COMPLETELY BLANKED',
     *   ' -- saving it anyway')
 1001 FORMAT ('Output ',A5,' FILE constant at',1PE13.5,
     *   ' -- saving it anyway')
 1010 FORMAT ('COMBHI: could not create HISTORY FOR ',A5,' FILE.  IER=',
     *   I5)
 1020 FORMAT (31X,'/ History of input map', I2)
 1030 FORMAT ('WARNING COULD NOT OPEN/COPY INPUT HI FILE=',I2,
     *   '.  IER=',I7)
 1031 FORMAT (31X,'/ NO HISTORY TO BE FOUND')
 1032 FORMAT (A6,'/ No history copied from file',I2)
 1050 FORMAT (31X,'/ End of old histories')
 1055 FORMAT (A6,'RELEASE=''',A7,' ''')
 1090 FORMAT (A6,'USERID=',I5)
 1092 FORMAT (A6,'CTYPE=''',A,'''',18X,' /',A8)
 1094 FORMAT (A6,'BLC=',7I5,' / Bottom left corner')
 1096 FORMAT (A6,'TRC=',7I5,' / Top right corner')
 1098 FORMAT (A6,'/ Single plane of Map 2 applied to all planes of',
     *   ' Map 1')
 1100 FORMAT (A6,3('A(',I1,')=',1PE12.4,2X))
 1101 FORMAT (A6,2('A(',I1,')=',1PE12.4,2X))
 1102 FORMAT (A6,2('A(',I1,')=',1PE12.4,2X),' / clip limits')
 1103 FORMAT (A6,'A(',I1,')=',1PE12.4,2X,' / PPOL clip limits')
 1110 FORMAT (A6,'/ Undefined pixels magic-value blanked')
 1111 FORMAT (A6,'/ Undefined pixels set to zero')
 1112 FORMAT (A6,'CBMULT(',I1,')=',F9.5,'  / scaled to match image 1',
     *   ' units')
 1113 FORMAT (A6,2('A(',I2,')=',1PE12.4,2X),'/ Absolute clip levels')
 1120 FORMAT (A6,'/ Output is image of expected noise')
 1130 FORMAT (A6,'SIGMA=',1PE12.4,'  /Input image',I2,' noise level')
 1131 FORMAT (A6,'B(6)=',1PE12.4,'  / Output sigma limit')
 1140 FORMAT (A6,'B(5)=',1PE12.4,'  / Max unblanked noise')
 1141 FORMAT (A6,'B(5)=',1PE12.4,'  / Min unblanked abs(sig/noise)')
 1900 FORMAT ('History file created and written for ',A5,' file')
 1901 FORMAT ('HISTORY FILE FOR ',A5,' INCOMPLETE.  IERRS =',2I7)
 1905 FORMAT ('WARNING COMBHI COULD NOT COPY TABLES TO OUTPUT ',A5,
     *   ' FILE.  IER=',I7)
 1910 FORMAT ('WARNING COMBHI COULD NOT CLOSE OUTPUT ',A5,
     *   ' FILE.  IER=',I7)
      END
      SUBROUTINE PDBIAS (P, RMS)
C-----------------------------------------------------------------------
C   Estimates the polarization bias in a polarization amplitude, P,
C   measured in the presence of Q and U RMS noise, RMS.  Returns the
C   corrected value.
C      The bias correction is such that the average bias is removed;
C   thus the average in the absence of a signal is zero.  Does table
C   look of values calculated by J. Condon in the range of P/RMS of
C   1.253 (the no signal limit) and 4 (the high SNR regime).  Does
C   second order Lagrange interpolation.  At lower values of P/RMS the
C   bias is a constant 1.253*RMS. Above a signal-to-noise ratio of 4,
C   use the formula:
C        normalized bias = 1 / (2 * s) + 1 / (8 * s**3),
C   where s is the true normalized flux density, iterating once to
C   estimate s from the normalized map flux density.  "Normalized" means
C   divided by the rms noise in the q and u maps.
C   Inputs:
C      P     R     On input, P is the observed total polarized intensity
C      RMS   R     The standard deviation of the (assumed equal)
C                  Gaussian distributions of the Stokes Q or U maps.
C   Output:
C      P     R     On output, P is the estimated intrinsic total
C                  polarized intensity.
C   Thanks to Jim Condon, Bill Cotton and the NVSS for this routine.
C-----------------------------------------------------------------------
      REAL      P, RMS
C
      INTEGER   I, INDEX, I1, I2, I3
      REAL      TABLE(2,40), PNORM, BIAS, D1, D2, D3, WT1, WT2, WT3,
     *           SUM, SUMWT
C                                       (map_flux,map_bias) pairs
      DATA TABLE /
     *   1.253,1.253,  1.256,1.156,  1.266,1.066,  1.281,0.9814,
     *   1.303,0.9030, 1.330,0.8304, 1.364,0.7636, 1.402,0.7023,
     *   1.446,0.6462, 1.495,0.5951, 1.549,0.5486, 1.606,0.5064,
     *   1.668,0.4683, 1.734,0.4339, 1.803,0.4028, 1.875,0.3749,
     *   1.950,0.3498, 2.027,0.3273, 2.107,0.3070, 2.189,0.2888,
     *   2.272,0.2724, 2.358,0.2576, 2.444,0.2442, 2.532,0.2321,
     *   2.621,0.2212, 2.711,0.2112, 2.802,0.2021, 2.894,0.1938,
     *   2.986,0.1861, 3.079,0.1791, 3.173,0.1726, 3.267,0.1666,
     *   3.361,0.1610, 3.456,0.1557, 3.551,0.1509, 3.646,0.1463,
     *   3.742,0.1420, 3.838,0.1380, 3.934,0.1342, 4.031,0.1306/
C-----------------------------------------------------------------------
C                                       Check RMS
      IF (RMS.LE.0.0) GO TO 999
      PNORM = P / RMS
C                                       Which regime?
C                                       Low (no) SNR case
      IF (PNORM.LE.TABLE(1,1)) THEN
         BIAS = TABLE(2,1)
C                                       High SNR
      ELSE IF (PNORM.GE.TABLE(1,40)) THEN
         BIAS = 1.0 / (2.0 * PNORM) + 1.0 / (8.0 * PNORM**3)
         PNORM = PNORM - BIAS
         BIAS = 1.0 / (2.0 * PNORM) + 1.0 / (8.0 * PNORM**3)
C                                       Middle, interpolate in table
      ELSE
         INDEX = 2
         DO 20 I = 3,39
            IF (PNORM.LT.TABLE(1,I)) GO TO 30
            INDEX = I
 20         CONTINUE
C                                       Lagrange interpolation
 30      I1 = INDEX - 1
         I2 = INDEX
         I3 = INDEX + 1
         D1 = (TABLE(1,I1) - TABLE(1,I2)) * (TABLE(1,I1) - TABLE(1,I3))
         D2 = (TABLE(1,I2) - TABLE(1,I1)) * (TABLE(1,I2) - TABLE(1,I3))
         D3 = (TABLE(1,I3) - TABLE(1,I1)) * (TABLE(1,I3) - TABLE(1,I2))
         WT1 = (PNORM - TABLE(1,I2)) * (PNORM - TABLE(1,I3)) / D1
         WT2 = (PNORM - TABLE(1,I1)) * (PNORM - TABLE(1,I3)) / D2
         WT3 = (PNORM - TABLE(1,I1)) * (PNORM - TABLE(1,I2)) / D3
         SUM = TABLE(2,I1) * WT1 + TABLE(2,I2) * WT2 + TABLE(2,I3) * WT3
         SUMWT = WT1 + WT2 + WT3
         IF (SUMWT.GT.0.0) THEN
            BIAS = SUM / SUMWT
C                                       Shouldn't ever get here but do
C                                       something reasonable.
         ELSE
            BIAS = TABLE(2,I2)
            END IF
         END IF
C                                       Correct for bias
      P = P - BIAS * RMS
C
 999  RETURN
      END
