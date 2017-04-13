LOCAL INCLUDE 'IMVIM.INC'
      INCLUDE 'INCS:PMAD.INC'
      REAL     SEQ1, DISK1, SEQ2, DISK2, BLC(7), TRC(7), DOALIN,
     *   XINC, YINC, ZINC, DPARM(10), XLABEL, XDOTV, XGRCH
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAM2I(3), XCLA2I(2), XOPCOD,
     *   XFUTYP
      CHARACTER NAMEIN*12, CLAIN*6, NAM2IN*12, CLA2IN*6, OPCODE*4,
     *   FUTYPE*4
      DOUBLE PRECISION    XCATD(128), YCATD(128)
      REAL      XCATR(256), YCATR(256), BUFF1(MABFSS), BUFF2(MABFSS)
      HOLLERITH XCATH(256), YCATH(256)
      INTEGER   XCATI(256), YCATI(256), XSEQIN, YSEQIN, XDISK, YDISK,
     *   XCNO, YCNO, JBUFSZ, ICODE, YBLC(7), YTRC(7), XLUN, YLUN, XIND,
     *   YIND, IXINC, IYINC, IZINC, XBINS, YBINS, IVER, PBUFF(256),
     *   GRCHN, TVCHN, TVCORN(4), LABEL
      LOGICAL   DOTV
      COMMON /INPARM/ XNAMEI, XCLAIN, SEQ1, DISK1, XNAM2I, XCLA2I,
     *   SEQ2, DISK2, BLC, TRC, DOALIN, XINC, YINC, ZINC, XOPCOD,
     *   XFUTYP, DPARM, XLABEL, XDOTV, XGRCH
      COMMON /CHPARM/ NAMEIN, CLAIN, NAM2IN, CLA2IN, OPCODE, FUTYPE
      COMMON /PARMS/ XCATI, YCATI, PBUFF, XSEQIN, YSEQIN, XDISK,
     *   YDISK, XCNO, YCNO, JBUFSZ, ICODE, YBLC, YTRC, XLUN, YLUN, XIND,
     *   YIND, IXINC, IYINC, IZINC, XBINS, YBINS, IVER, DOTV, GRCHN,
     *    TVCHN, TVCORN, LABEL
      COMMON /IMBUF/ BUFF1, BUFF2
      EQUIVALENCE (XCATI, XCATR, XCATH, XCATD)
      EQUIVALENCE (YCATI, YCATR, YCATH, YCATD)
LOCAL END
      PROGRAM IMVIM
C-----------------------------------------------------------------------
C! Plots pixel values in one image against those in another
C# Map-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000, 2002, 2014-2015
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
C   IMVIM plots the pixel values in one image against the pixel values
C   in another.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         XNAMIN        Name of input image for X axis
C      INCLASS        XCLAIN        Class of input image for X axis.
C      INSEQ          XSEQIN        Seq. of input image for X axis
C      INDISK         XDISK         Disk number of input image X axis.
C      IN2NAME        YNAMIN        Name of the Y axis image
C      IN2CLASS       YCLAIN        Class of the Y axis image.
C      IN2SEQ         YSEQIN        Seq. number of Y axis image.
C      IN2DISK        YDISK         Disk number of the Y axis image.
C      BLC(7)         BLC           Bottom left corner of subimage
C                                   of X input image.
C      TRC(7)         TRC           Top right corner of subimage.
C      DOALIGN        DOALIN        How to align the images (-2 none,
C                                   -1 center offset, 0 center position
C                                   +1 fully)
C      XINC                         X increment
C      YINC                         Y increment
C      ZINC                         Z increment
C      OPTYPE         OPTYPE        Operation type: 'PNTS','BINC',
C                                   'BIN '
C      FUNCTYPE       FUTYPE        'LG' => plus signs proportional
C                                      to log of count ('BINC')
C      DPARM          DPARM         (1) X axis minimum
C                                   (2) X axis maximum
C                                   (3) Y axis minimum
C                                   (4) Y axis maximum
C                                   (5) X axis number bins (<= 512)
C                                   (6) Y axis number bins (<= 512)
C                                   (7) Multiplier of plotted +'s
C      DOTV     R      > 0 => TV, else plot file
C      GRCHAN   R      graphics channel to use
C      TVCORN   R(4)   TV pixel to use (both > 0 => pixel scale)
C   Programmer Eric W. Greisen:  December 1985
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'IMVIM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'IMVIM '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL IMVIMI (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the plotting routine
      IF (IRET.EQ.0) CALL IMVIMD (IRET)
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE IMVIMI (PRGN, IRET)
C-----------------------------------------------------------------------
C   IMVIMI gets input parameters for BLANK and creates an output file
C   IF requested for the residual map.
C   Inputs:  PRGN    C*6       Program name (2 chars/word)
C   Output:  IRET    I         Error code: 0 => ok
C                                4 => user routine detected error.
C                                5 => catalog troubles
C                                8 => can't start
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER   STAT*4, TEXT*132, PHFILE*48, PRGN*6, ATIME*8,
     *   NAMCLS*18, ADATE*12, CODES(3)*4, MTYPE*2
      INTEGER  IRET, IERR, NPARM, IROUND, INC, I, NCODE, J, K, NX1,
     *   NX2, IPSIZE, ITYPE, LUNPL, FINDPL, IDEPTH(5), INP, LTYPE,
     *   INCHAR, ID(3), IT(3)
      REAL       EPS, AXV, PBLC(2), PTRC(2), XYRATO, CH(4), DX, DY,
     *    T1, T2, XMULT, YMULT
      DOUBLE PRECISION    DAXV
      LOGICAL   EQUAL, GOOD, T, F
      INCLUDE 'IMVIM.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA NCODE, CODES /3, 'PTS ','BINC','BIN '/
      DATA T, F, LUNPL /.TRUE.,.FALSE., 26/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSS
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      CALL FILL (10, 0, IBAD)
C                                       Get input parameters.
      NPARM = 47
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAM2I, NAM2IN)
      CALL H2CHR (6, 1, XCLA2I, CLA2IN)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      CALL H2CHR (4, 1, XFUTYP, FUTYPE)
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
      LABEL = IROUND (XLABEL)
      LTYPE = MOD (ABS(LABEL), 100)
      IF ((LTYPE.EQ.0) .OR. (LTYPE.GT.10)) LTYPE = 3
      IF (LTYPE.GT.7) LTYPE = 7
      IF ((LTYPE.GE.4) .AND. (LTYPE.LE.6)) LTYPE = 3
      IF (LABEL.LT.0) THEN
         LABEL = (LABEL/100)*100 - LTYPE
      ELSE
         LABEL = (LABEL/100)*100 + LTYPE
         END IF
      XLABEL = LABEL
C                                       Find operation, transfer
      ICODE = 1
      DO 15 I = 1,NCODE
         IF (OPCODE.EQ.CODES(I)) ICODE = I
 15      CONTINUE
      OPCODE = CODES(ICODE)
      EQUAL = FUTYPE(1:2).EQ.'LG'
      IF (EQUAL) FUTYPE = 'LG'
      IF ((.NOT.EQUAL) .OR. (ICODE.NE.2)) FUTYPE = ' '
      CALL CHR2H (4, OPCODE, 1, XOPCOD)
      CALL CHR2H (4, FUTYPE, 1, XFUTYP)
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      XSEQIN = IROUND (SEQ1)
      XDISK = IROUND (DISK1)
      XLUN = 16
      YLUN = 17
C                                       Get CATBLK of Y axis file.
      YCNO = 1
      YDISK = IROUND (DISK2)
      YSEQIN = IROUND (SEQ2)
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', YDISK, YCNO, NAM2IN, CLA2IN, YSEQIN, MTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAM2IN, CLA2IN, YSEQIN, YDISK,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK and mark 'READ'.
      CALL CATIO ('READ', YDISK, YCNO, YCATI, 'READ', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = YDISK
      FCNO(NCFILE) = YCNO
      FRW(NCFILE) = 0
C                                       Get CATBLK X axis file
      XCNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', XDISK, XCNO, NAMEIN, CLAIN, XSEQIN, MTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, XSEQIN, XDISK,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK and mark 'READ'.
      STAT = 'WRIT'
      IF (DOTV) STAT = 'READ'
      CALL CATIO ('READ', XDISK, XCNO, XCATI, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = XDISK
      FCNO(NCFILE) = XCNO
      FRW(NCFILE) = 1
      IF (DOTV) FRW(NCFILE) = 0
C                                       Set defaults on BLC,TRC
      CALL WINDOW (XCATI(KIDIM), XCATI(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
      EPS = 0.2
      INC = 2
C                                       set corners, check alignment
      NX1 = XCATI(KIDIM)
      NX2 = YCATI(KIDIM)
      DO 75 I = 1,7
         YBLC(I) = 1
         YTRC(I) = 1
         J = I - 1
         IF (YCATI(KINAX+J).LT.1) YCATI(KINAX+J) = 1
         IF ((I.GT.NX2) .OR. (I.GT.NX1) .OR. (YCATI(KINAX+J)
     *      .LE.1)) GO TO 75
            K = I - 1
            AXV = YCATR(KRCRP+J) - XCATR(KRCRP+K) + BLC(K+1)
            IF (DOALIN.LT.-1.5) AXV = BLC(K+1)
            IF ((DOALIN.GT.-0.1) .AND. (YCATR(KRCIC+J).NE.0.0))
     *         THEN
               DAXV = XCATD(KDCRV+K) + XCATR(KRCIC+K)*(BLC(K+1) -
     *            XCATR(KRCRP+K))
               AXV = (DAXV - YCATD(KDCRV+J)) / YCATR(KRCIC+J) +
     *            YCATR(KRCRP+J)
               END IF
            YBLC(I) = IROUND(AXV)
            YTRC(I) = YBLC(I) + TRC(K+1) - BLC(K+1) + EPS
            IF (YTRC(I).LT.1) GO TO 80
            IF (YBLC(I).GT.YCATI(KINAX+J)) GO TO 80
C                                       smaller subim needed?
            IF (YBLC(I).LT.1) THEN
               BLC(K+1) = BLC(K+1) + 1 - YBLC(I)
               AXV = AXV + 1 - YBLC(I)
               YBLC(I) = 1
               END IF
            IF (YTRC(I).GT.YCATI(KINAX+J)) THEN
               TRC(K+1) = TRC(K+1) + YCATI(KINAX+J) - YTRC(I)
               YTRC(I) = YCATI(KINAX+J)
               END IF
            IF (YTRC(I).LT.YBLC(I)) GO TO 80
            IF (BLC(K+1).GT.XCATI(KINAX+K)) GO TO 80
            IF (TRC(K+1).LT.1) GO TO 80
C                                       full alignment check
            IF (DOALIN.GE.0.1) THEN
               IF (ABS(YBLC(I)-AXV).GT.EPS) GO TO 80
               EQUAL = YCATH(KHCTP+J*INC) .EQ.
     *            XCATH(KHCTP+K*INC)
               IF (.NOT.EQUAL) GO TO 80
               AXV = EPS * EPS * ABS(XCATR(KRCIC+K))
               IF (ABS(XCATR(KRCIC+K)-YCATR(KRCIC+J)).GT.AXV) GO TO 80
               IF (ABS(XCATR(KRCRT+K)-YCATR(KRCRT+J)).GT.1.0) GO TO 80
               END IF
 75         CONTINUE
         GO TO 100
C                                       Failure to align
 80   CONTINUE
         WRITE (MSGTXT,1080)
         GO TO 990
C                                       Parms defaults
 100  EQUAL = DPARM(1).EQ.DPARM(2)
      IF ((DPARM(1).LT.DPARM(2)) .AND. (XCATR(KRDMN).GE.DPARM(2)))
     *   EQUAL = .TRUE.
      IF ((DPARM(1).GT.DPARM(2)) .AND. (XCATR(KRDMN).GE.DPARM(1)))
     *   EQUAL = .TRUE.
      IF ((DPARM(1).LT.DPARM(2)) .AND. (XCATR(KRDMX).LE.DPARM(1)))
     *   EQUAL = .TRUE.
      IF ((DPARM(1).GT.DPARM(2)) .AND. (XCATR(KRDMX).LE.DPARM(2)))
     *   EQUAL = .TRUE.
      IF (EQUAL) DPARM(1) = XCATR(KRDMN)
      IF (EQUAL) DPARM(2) = XCATR(KRDMX)
      EQUAL = DPARM(3).EQ.DPARM(4)
      IF ((DPARM(3).LT.DPARM(4)) .AND. (YCATR(KRDMN).GE.DPARM(4)))
     *   EQUAL = .TRUE.
      IF ((DPARM(3).GT.DPARM(4)) .AND. (YCATR(KRDMN).GE.DPARM(3)))
     *   EQUAL = .TRUE.
      IF ((DPARM(3).LT.DPARM(4)) .AND. (YCATR(KRDMX).LE.DPARM(3)))
     *   EQUAL = .TRUE.
      IF ((DPARM(3).GT.DPARM(4)) .AND. (YCATR(KRDMX).LE.DPARM(4)))
     *   EQUAL = .TRUE.
      IF (EQUAL) DPARM(3) = YCATR(KRDMN)
      IF (EQUAL) DPARM(4) = YCATR(KRDMX)
      IF (ICODE.EQ.1) DPARM(5) = 128.
      IF (ICODE.EQ.1) DPARM(6) = 128.
      IF ((DPARM(5).LT.10.) .OR. (DPARM(5).GT.512.)) DPARM(5) = 128.0
      IF ((DPARM(6).LT.10.) .OR. (DPARM(6).GT.512.)) DPARM(6) = DPARM(5)
      IF ((DPARM(7).LE.0.1) .OR. (DPARM(7).GT.(DPARM(5)+DPARM(6))/20.))
     *   DPARM(7) = 1.0
      IF ((DPARM(8).LT.-10.) .OR. (DPARM(8).GT.10.)) DPARM(8) = 0
      IF ((DPARM(9).LT.-10.) .OR. (DPARM(9).GT.10.)) DPARM(9) = 0
      IF (XINC.LE.0.9) XINC = 1.0
      IF (YINC.LE.0.9) YINC = 1.0
      IF (ZINC.LE.0.9) ZINC = 1.0
      IXINC = IROUND (XINC)
      IYINC = IROUND (YINC)
      IZINC = IROUND (ZINC)
      XBINS = IROUND (DPARM(5))
      YBINS = IROUND (DPARM(6))
C                                       Offset corners
      I = IROUND (DPARM(8))
      DPARM(8) = 0.
      IF ((I.NE.0) .AND. (I.LT.YTRC(1)-YBLC(1))) THEN
         DPARM(8) = I
         YBLC(1) = YBLC(1) - I
         YTRC(1) = YTRC(1) - I
         IF (YBLC(1).LT.1) THEN
            BLC(1) = BLC(1) - YBLC(1) + 1.
            YBLC(1) = 1
            END IF
         IF (YTRC(1).GT.YCATI(KINAX)) THEN
            TRC(1) = TRC(1) - YTRC(1) + YCATI(KINAX)
            YTRC(1) = YCATI(KINAX)
            END IF
         END IF
      I = IROUND (DPARM(9))
      DPARM(9) = 0
      IF ((I.NE.0) .AND. (I.LT.YTRC(2)-YBLC(2))) THEN
         DPARM(9) = I
         YBLC(2) = YBLC(2) - I
         YTRC(2) = YTRC(2) - I
         IF (YBLC(2).LT.1) THEN
            BLC(2) = BLC(2) - YBLC(2) + 1.
            YBLC(2) = 1
            END IF
         IF (YTRC(2).GT.YCATI(KINAX+1)) THEN
            TRC(2) = TRC(2) - YTRC(2) + YCATI(KINAX+1)
            YTRC(2) = YCATI(KINAX+1)
            END IF
         END IF
      IRET = 7
      IVER = 0
C                                       make plot file
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', XDISK, XCNO, XCATI, BUFF1, T, 'READ',
     *      IVER, IERR)
         IF (IERR.NE.0) THEN
            NCFILE = NCFILE - 1
            GO TO 999
            END IF
         END IF
      FRW(NCFILE) = 0
C                                       fill in actual values
      CALL CHR2H (12, NAMEIN, 1, XNAMEI)
      CALL CHR2H (6, CLAIN, 1, XCLAIN)
      CALL CHR2H (12, NAM2IN, 1, XNAM2I)
      CALL CHR2H (6, CLA2IN, 1, XCLA2I)
      CALL ZPHFIL ('PL', XDISK, XCNO, IVER, PHFILE, IERR)
      IPSIZE = 0
      ITYPE = 14
      CALL GINIT (XDISK, XCNO, PHFILE, IPSIZE, ITYPE, NPARM, XNAMEI,
     *   DOTV, TVCHN, GRCHN, TVCORN, XCATI, PBUFF, LUNPL, FINDPL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1120) IERR
         GO TO 990
         END IF
C                                       basic plot parms
      PBLC(1) = 0.0
      PBLC(2) = 0.0
      PTRC(1) = XBINS + 1.0
      PTRC(2) = YBINS + 1.0
      XYRATO = REAL(YBINS) / REAL(XBINS)
C                                       set up location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      T1 = ABS (DPARM(4) - DPARM(3))
      T2 = T1
      CALL METSCL (LABEL, T1, CPREF(2,LOCNUM), GOOD)
      YMULT = T1 / T2
      T1 = ABS (DPARM(2) - DPARM(1))
      T2 = T1
      CALL METSCL (LABEL, T1, CPREF(1,LOCNUM), GOOD)
      XMULT = T1 / T2
      RPLOC(1,LOCNUM) = 0.5
      RPLOC(2,LOCNUM) = 0.5
      RPVAL(1,LOCNUM) = DPARM(1) * XMULT
      RPVAL(2,LOCNUM) = DPARM(3) * YMULT
      AXINC(1,LOCNUM) = XMULT * (DPARM(2) - DPARM(1)) / XBINS
      AXINC(2,LOCNUM) = YMULT * (DPARM(4) - DPARM(3)) / YBINS
      CALL H2CHR (8, 1, XCATH(KHBUN), CTYP(1,LOCNUM))
      CALL H2CHR (8, 1, YCATH(KHBUN), CTYP(2,LOCNUM))
C                                       characters around the edge
      CALL RFILL (4, 0.5, CH)
      IF (LTYPE.EQ.2) CH(1) = 3.0
      IF (LTYPE.GT.2) THEN
         CH(1) = 2.5
         CALL CHNTIC (PBLC, PTRC, I)
         IF (I.GT.0) CH(1) = 4.0 + I
         END IF
      IF (LTYPE.GT.1) CH(2) = CH(2) + 1.5
      IF (LTYPE.GT.2) CH(2) = CH(2) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CH(4) = CH(4) + 2.833
      IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) CH(4) = CH(4) + 1.333
      CALL FILL (5, 1, IDEPTH)
      CALL GINITL (PBLC, PTRC, XYRATO, CH, IDEPTH, PBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GLTYPE (1, PBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Plot border
      CALL GPOS (PBLC(1), PTRC(2), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (PBLC(1), PBLC(2), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (PTRC(1), PBLC(2), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (PTRC(1), PTRC(2), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (PBLC(1), PTRC(2), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Top labels: X name
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         DX = 0.0
         DY = 0.5
         CALL GPOS (PBLC(1), PTRC(2), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 980
         TEXT = 'X-axis image'
         INP = 14
         CALL H2CHR (18, 1, XCATH(KHIMN), NAMCLS)
         CALL NAMEST (NAMCLS, XCATI(KIIMS), TEXT(INP:), INCHAR)
         INCHAR = INCHAR + INP - 1
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Top label: Y axis
         DY = DY + 1.333
         CALL GPOS (PBLC(1), PTRC(2), PBUFF, IERR)
         IF (IERR.NE.0) GO TO 980
         TEXT = 'Y-axis image'
         INP = 14
         CALL H2CHR (18, 1, YCATH(KHIMN), NAMCLS)
         CALL NAMEST (NAMCLS, YCATI(KIIMS), TEXT(INP:), INCHAR)
         INCHAR = INCHAR + INP - 1
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       Top label: version, date
      DY = DY + 1.333
      CALL GPOS (PBLC(1), PTRC(2), PBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (TEXT,1131) IVER, ADATE, ATIME
      CALL REFRMT (TEXT, '_', INCHAR)
      IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) THEN
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PBUFF, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      MSGTXT = TEXT(:80)
      IF (.NOT.DOTV) CALL MSGWRT (3)
C                                       label axes
      CALL CLAB1 (PBLC, PTRC, CH, LABEL, XYRATO, F, PBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       OK !
      IRET = 0
      GO TO 999
C                                       Destroy bad plot file
 980  CALL ZCLOSE (LUNPL, FINDPL, IERR)
      CALL ZDESTR (XDISK, PHFILE, IERR)
      CALL DELEXT ('PL', XDISK, XCNO, 'READ', XCATI, BUFF1, IVER, IERR)
      NCFILE = NCFILE - 1
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMVIMI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I2,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' READING Y-AXIS IMAGE CATBLK')
 1050 FORMAT ('ERROR',I3,' READING X-AXIS IMAGE CATBLK')
 1080 FORMAT ('X-AXIS AND Y-AXIS IMAGES DO NOT OVERLAP: CHECK HEADERS')
 1120 FORMAT ('ERROR',I7,' CREATING PLOT FILE')
 1131 FORMAT ('Plot file version',I4,'__created ',A12,A8)
      END
      SUBROUTINE IMVIMD (IRET)
C-----------------------------------------------------------------------
C   IMVIMD sends image one row at a time to the blanking routine and
C   then writes the modified data.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER IFILE*48
      INTEGER   IRET, IROUND, IBIND1, IBIND2, NYI, NXI, WINI(4),
     *   BOI, BPI, LIM2, LIM3, LIM4, LIM5, LIM6, LIM7, I1, I2, I3,
     *   I4, I5, I6, I7, IPOS(7), CORN(7), BOTEMP, LIM1, LIM1C, LOFF,
     *   WINI2(4), IPLDO, I, NXI2, NYI2, IERR, PLTODO, PLDONE, I4TEMP
      DOUBLE PRECISION    XSCALE, XOFSET, YSCALE, YOFSET
      LOGICAL   T, F
      INCLUDE 'IMVIM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open and init for read
      CALL ZPHFIL ('MA', XDISK, XCNO, 1, IFILE, IRET)
      CALL ZOPEN (XLUN, XIND, XDISK, IFILE, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Init second input map
      CALL ZPHFIL ('MA', YDISK, YCNO, 1, IFILE, IRET)
      CALL ZOPEN (YLUN, YIND, YDISK, IFILE, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Setup for I/O
      NXI = XCATI(KINAX)
      NYI = XCATI(KINAX+1)
      NXI2 = YCATI(KINAX)
      NYI2 = YCATI(KINAX+1)
      WINI(1) = IROUND (BLC(1))
      WINI(2) = IROUND (BLC(2))
      WINI(3) = IROUND (TRC(1))
      WINI(4) = IROUND (TRC(2))
      WINI2(1) = YBLC(1)
      WINI2(2) = YBLC(2)
      WINI2(3) = YTRC(1)
      WINI2(4) = YTRC(2)
      LIM1C = YTRC(1) - YBLC(1) + 1
      XSCALE = DPARM(5) / (DPARM(2) - DPARM(1))
C            XOFSET = XCATD(KDBZE) * XSCALE + 0.5 - DPARM(1) * XSCALE
      XOFSET = 0.0D0 * XSCALE + 0.5 - DPARM(1) * XSCALE
C            XSCALE = XCATD(KDBSC) * XSCALE
      YSCALE = DPARM(6) / (DPARM(4) - DPARM(3))
C            YOFSET = YCATD(KDBZE) * YSCALE + 0.5 - DPARM(3) * YSCALE
      YOFSET = 0.0D0 * YSCALE + 0.5 - DPARM(3) * YSCALE
C            YSCALE = YCATD(KDBSC) * YSCALE
      BPI = 2
C                                       Setup for looping
      LIM1 = TRC(1) - BLC(1) + 1.01
      LIM2 = TRC(2) - BLC(2) + 1.01
      LIM3 = TRC(3) - BLC(3) + 1.01
      LIM4 = TRC(4) - BLC(4) + 1.01
      LIM5 = TRC(5) - BLC(5) + 1.01
      LIM6 = TRC(6) - BLC(6) + 1.01
      LIM7 = TRC(7) - BLC(7) + 1.01
      IPLDO = 0
      PLDONE = 0
      PLTODO = LIM7
      PLTODO = PLTODO * LIM4 * LIM5 * LIM6
      PLTODO = PLTODO * LIM3
      LOFF = KINAX - 1
C                                       First call: start plot
      IPOS(1) = 0
      CALL DO1IVI (IPOS, BUFF1(IBIND1), BUFF2(IBIND2), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1015) IRET
         GO TO 980
         END IF
C                                       Loop
      DO 700 I7 = 1,LIM7
         IPOS(7) = BLC(7) + I7 - 0.9
         CORN(7) = YBLC(7) + I7 - 2
         CORN(7) = MOD (CORN(7), YCATI(7+LOFF)) + 1
         DO 600 I6 = 1,LIM6
            IPOS(6) = BLC(6) + I6 - 0.9
            CORN(6) = YBLC(6) + I6 - 2
            CORN(6) = MOD (CORN(6), YCATI(6+LOFF)) + 1
            DO 500 I5 = 1,LIM5
               IPOS(5) = BLC(5) + I5 - 0.9
               CORN(5) = YBLC(5) + I5 - 2
               CORN(5) = MOD (CORN(5), YCATI(5+LOFF)) + 1
               DO 400 I4 = 1,LIM4
                  IPOS(4) = BLC(4) + I4 - 0.9
                  CORN(4) = YBLC(4) + I4 - 2
                  CORN(4) = MOD (CORN(4), YCATI(4+LOFF)) + 1
                  DO 300 I3 = 1,LIM3
                     IPOS(3) = BLC(3) + I3 - 0.9
                     CORN(3) = YBLC(3) + I3 - 2
                     CORN(3) = MOD (CORN(3), YCATI(3+LOFF))
     *                  + 1
                     PLDONE = PLDONE + 1
                     IPLDO = IPLDO + 1
                     WRITE (MSGTXT,1020) PLDONE, PLTODO
                     IF ((IPLDO.EQ.1) .AND. (PLTODO.GT.2))
     *                  CALL MSGWRT (1)
                     IF (IPLDO.GE.16) IPLDO = 0
                     I4TEMP = (PLDONE - 1) / IZINC
                     I4TEMP = I4TEMP * IZINC + 1
                     IF (I4TEMP.NE.PLDONE) GO TO 300
C                                       Init. files, first input.
                     CALL COMOFF (XCATI(KIDIM), XCATI(KINAX),
     *                  IPOS(3), BOTEMP, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1021) IRET
                        GO TO 990
                        END IF
                     BOI = BOTEMP + 1
                     CALL MINIT ('READ', XLUN, XIND, NXI, NYI, WINI,
     *                  BUFF1, JBUFSZ, BOI, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1100) IRET
                        GO TO 990
                        END IF
C                                       Init. files, first input.
                    CALL COMOFF (YCATI(KIDIM), YCATI(KINAX),
     *                  CORN(3), BOTEMP, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1110) IRET
                        GO TO 990
                        END IF
                     BOI = BOTEMP + 1
                     CALL MINIT ('READ', YLUN, YIND, NXI2, NYI2, WINI2,
     *                  BUFF2, JBUFSZ, BOI, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1115) IRET
                        GO TO 990
                        END IF
                     DO 250 I2 = 1,LIM2
                        IPOS(2) = BLC(2) + I2 - 0.9
                        IPOS(1) = IROUND (BLC(1))
C                                       Read.
                        CALL MDISK ('READ', XLUN, XIND, BUFF1, IBIND1,
     *                     IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1120) IRET
                           GO TO 990
                           END IF
C                                       read clip if needed
                        CALL MDISK ('READ', YLUN, YIND, BUFF2, IBIND2,
     *                     IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1130) IRET
                           GO TO 990
                           END IF
C                                       Copy/scale to buffer.
                        IF (MOD (I2-1, IYINC).EQ.0) THEN
                           DO 145 I1 = 1,LIM1
                              I = IBIND1 + I1 - 1
                              IF (BUFF1(I).NE.FBLANK) BUFF1(I) =
     *                           BUFF1(I) * XSCALE + XOFSET
 145                          CONTINUE
C                                       Copy/scale to buffer.
                           DO 150 I1 = 1,LIM1C
                              I = IBIND2 + I1 - 1
                              IF (BUFF2(I).NE.FBLANK) BUFF2(I) =
     *                           BUFF2(I) * YSCALE + YOFSET
 150                          CONTINUE
C                                       Call DO1IVI
                           CALL DO1IVI (IPOS, BUFF1(IBIND1),
     *                        BUFF2(IBIND2), IRET)
                           IF (IRET.NE.0) THEN
                              WRITE (MSGTXT,1150) IRET
                              GO TO 980
                              END IF
                           END IF
 250                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Last call: finish plot
      IPOS(1) = -1
      CALL DO1IVI (IPOS, BUFF1(IBIND1), BUFF2(IBIND2), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1700) IRET
         GO TO 980
         END IF
C                                       Close files
      CALL ZCLOSE (XLUN, XIND, IRET)
      CALL ZCLOSE (YLUN, YIND, IRET)
      IRET = 0
      GO TO 999
C                                       Error: file already killed
 980  CALL MSGWRT (8)
      GO TO 999
C                                       Error: kill file
 990  CALL MSGWRT (8)
      CALL ZCLOSE (XLUN, XIND, IERR)
      IF (.NOT.DOTV) THEN
         CALL ZCLOSE (GPHLUN, GPHIND, IERR)
         CALL ZDESTR (XDISK, GPHNAM, IERR)
         CALL DELEXT ('PL', XDISK, XCNO, 'READ', XCATI, BUFF1, IVER,
     *      IERR)
         NCFILE = NCFILE - 1
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMVIMD: ERROR',I3,' OPENING X-IMAGE FILE')
 1010 FORMAT ('IMVIMD: ERROR',I3,' OPENING Y-IMAGE FILE')
 1015 FORMAT ('IMVIMD: DO1IVI ERROR STARTING PLOT',I3)
 1020 FORMAT ('Begin plane',I8,' of',I8)
 1021 FORMAT ('IMVIMD: COMOF3 (X-IMAGE) ERROR',I3)
 1100 FORMAT ('IMVIMD: MINI3 (X-IMAGE) ERROR',I3)
 1110 FORMAT ('IMVIMD: COMOF3 (Y-IMAGE) ERROR',I3)
 1115 FORMAT ('IMVIMD: MINI3 (Y-IMAGE) ERROR',I3)
 1120 FORMAT ('IMVIMD: READ (X-IMAGE) ERROR',I3)
 1130 FORMAT ('IMVIMD: READ (Y-IMAGE) ERROR',I3)
 1150 FORMAT ('IMVIMD: DO1IVI ERROR',I3)
 1700 FORMAT ('IMVIMD: DO1IVI ERROR FINISHING PLOT',I3)
      END
      SUBROUTINE DO1IVI (IPOS, IND1, IND2, IRET)
C-----------------------------------------------------------------------
C   DO1IVI plots the data: ICODE = 1 each point as it is called
C   ICODE = 2, 3  sum into array, plot at end
C   Inputs:
C      IPOS(7)   I    BLC (input image) of first value in DATA
C                     (1) = 0  => init plot array
C                     (1) = -1 => plot the array
C      IND1(*)   R    X-axis image row
C      IND2(*)   R    Y-axis image row
C   Values from commons:
C      FBLANK    R    Value of blanked pixel.
C      BLC, TRC (7)  R     Window corners in original image
C   Output:
C      IRET      I    Return code   0 => OK
C                               >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), IRET
      REAL      IND1(*), IND2(*)
C
      INTEGER   NP, I, IX, IY, IERR, IROUND, NPOINT, DBIN(512,512), DMAX
      REAL      DX, XLIM, YLIM, X, Y, X1, X2, Y1, Y2, SCALE
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'IMVIM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      SAVE DBIN, NP, XLIM, YLIM, DX, NPOINT
C-----------------------------------------------------------------------
      IRET = 0
C                                       Initialize
      IF (IPOS(1).EQ.0) THEN
         NP = TRC(1) - BLC(1) + 1.01
         XLIM = XBINS + 0.499999
         YLIM = YBINS + 0.499999
         NPOINT = 0
         DX = 0.5 * DPARM(7)
C                                       zero bins
         IF (ICODE.GT.1) THEN
            DO 20 IY = 1,YBINS
               DO 15 IX = 1,XBINS
                  DBIN(IX,IY) = 0
 15               CONTINUE
 20            CONTINUE
            END IF
C                                       Deal with input row
C                                       Plot each point
      ELSE IF (IPOS(1).GT.0) THEN
         IF (ICODE.EQ.1) THEN
            CALL GLTYPE (4, PBUFF, IRET)
            IF (IRET.NE.0) GO TO 999
            DO 110 I = 1,NP,IXINC
               X = IND1(I)
               Y = IND2(I)
               IF ((X.NE.FBLANK) .AND. (X.GE.0.5) .AND. (X.LE.XLIM)
     *            .AND. (Y.NE.FBLANK) .AND. (Y.GE.0.5) .AND.
     *            (Y.LE.YLIM)) THEN
                  NPOINT = NPOINT + 1
                  X1 = X - DX
                  X2 = X + DX
                  Y1 = Y - DX
                  Y2 = Y + DX
                  CALL GPOS (X1, Y, PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 900
                  CALL GVEC (X2, Y, PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 900
                  CALL GPOS (X, Y1, PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 900
                  CALL GVEC (X, Y2, PBUFF, IERR)
                  IF (IERR.NE.0) GO TO 900
                  END IF
 110           CONTINUE
C                                       sum into bins
         ELSE
            DO 130 I = 1,NP,IXINC
               X = IND1(I)
               Y = IND2(I)
               IF ((X.NE.FBLANK) .AND. (X.GE.0.5) .AND. (X.LE.XLIM)
     *            .AND. (Y.NE.FBLANK) .AND. (Y.GE.0.5) .AND.
     *            (Y.LE.YLIM)) THEN
                  IX = IROUND (X)
                  IY = IROUND (Y)
                  DBIN(IX,IY) = DBIN(IX,IY) + 1
                  END IF
 130           CONTINUE
            END IF
C                                       Finish plots
      ELSE IF (IPOS(1).LT.0) THEN
C                                       'BIN '
         IF (ICODE.GT.2) THEN
            CALL GLTYPE (2, PBUFF, IRET)
            IF (IRET.NE.0) GO TO 999
            DX = 0.5 * DPARM(7)
            DO 210 IY = 1,YBINS
               Y = IY
               Y1 = Y - DX
               Y2 = Y + DX
               DO 205 IX = 1,XBINS
                  IF (DBIN(IX,IY).GE.1) THEN
                     NPOINT = NPOINT + 1
                     X = IX
                     X1 = X - DX
                     X2 = X + DX
                     CALL GPOS (X1, Y, PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 900
                     CALL GVEC (X2, Y, PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 900
                     CALL GPOS (X, Y1, PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 900
                     CALL GVEC (X, Y2, PBUFF, IERR)
                     IF (IERR.NE.0) GO TO 900
                     END IF
 205              CONTINUE
 210           CONTINUE

C                                       'BINC': find maximum count
         ELSE IF (ICODE.EQ.2) THEN
            CALL GLTYPE (2, PBUFF, IRET)
            IF (IRET.NE.0) GO TO 999
            DMAX = 0
            DO 230 IY = 1,YBINS
               DO 225 IX = 1,XBINS
                  IF (DMAX.LT.DBIN(IX,IY)) DMAX = DBIN(IX,IY)
 225              CONTINUE
 230           CONTINUE
            WRITE (MSGTXT,1230) DMAX
            IF (DMAX.LT.1) THEN
               CALL MSGWRT (8)
            ELSE
               CALL MSGWRT (2)
C                                       Linear plot
               IF (FUTYPE(1:2).NE.'LG') THEN
                  SCALE = DPARM(7) / DMAX
                  DO 250 IY = 1,YBINS
                     Y = IY
                     DO 245 IX = 1,XBINS
                        IF (DBIN(IX,IY).GE.1) THEN
                           DX = DBIN(IX,IY) * SCALE
                           Y1 = Y - DX
                           Y2 = Y + DX
                           NPOINT = NPOINT + 1
                           X = IX
                           X1 = X - DX
                           X2 = X + DX
                           CALL GPOS (X1, Y, PBUFF, IERR)
                           IF (IERR.NE.0) GO TO 900
                           CALL GVEC (X2, Y, PBUFF, IERR)
                           IF (IERR.NE.0) GO TO 900
                           CALL GPOS (X, Y1, PBUFF, IERR)
                           IF (IERR.NE.0) GO TO 900
                           CALL GVEC (X, Y2, PBUFF, IERR)
                           IF (IERR.NE.0) GO TO 900
                           END IF
 245                    CONTINUE
 250                 CONTINUE
C                                       Linear plot
               ELSE
                  SCALE = DPARM(7) / LOG (DMAX + 1.0)
                  DO 270 IY = 1,YBINS
                     Y = IY
                     DO 265 IX = 1,XBINS
                        IF (DBIN(IX,IY).GE.1) THEN
                           DX = SCALE * LOG (DBIN(IX,IY) + 1.0)
                           Y1 = Y - DX
                           Y2 = Y + DX
                           NPOINT = NPOINT + 1
                           X = IX
                           X1 = X - DX
                           X2 = X + DX
                           CALL GPOS (X1, Y, PBUFF, IERR)
                           IF (IERR.NE.0) GO TO 900
                           CALL GVEC (X2, Y, PBUFF, IERR)
                           IF (IERR.NE.0) GO TO 900
                           CALL GPOS (X, Y1, PBUFF, IERR)
                           IF (IERR.NE.0) GO TO 900
                           CALL GVEC (X, Y2, PBUFF, IERR)
                           IF (IERR.NE.0) GO TO 900
                           END IF
 265                    CONTINUE
 270                 CONTINUE
                  END IF
               END IF
            END IF
C                                       Normal finish
         WRITE (MSGTXT,1800) NPOINT
         CALL MSGWRT (2)
         CALL GFINIS (PBUFF, IERR)
         IF (IERR.NE.0) GO TO 920
         IF (.NOT.DOTV) CALL HIPLOT (XDISK, XCNO, IVER, BUFF1, IERR)
         END IF
      GO TO 999
C                                       Abnormal finish: try it
 900  WRITE (MSGTXT,1900) IERR
      CALL MSGWRT (8)
      CALL GFINIS (PBUFF, IERR)
      IF (IERR.NE.0) GO TO 920
         WRITE (MSGTXT,1800) NPOINT
         CALL MSGWRT (2)
         IF (.NOT.DOTV) CALL HIPLOT (XDISK, XCNO, IVER, BUFF1, IERR)
         GO TO 999
C                                       Give up and clean up
 920  CALL ZCLOSE (XLUN, XIND, IERR)
      IRET = 8
      IF (.NOT.DOTV) THEN
         CALL ZCLOSE (GPHLUN, GPHIND, IERR)
         CALL ZDESTR (XDISK, GPHNAM, IERR)
         CALL DELEXT ('PL', XDISK, XCNO, 'READ', XCATI, BUFF1, IVER,
     *      IERR)
         NCFILE = NCFILE - 1
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1230 FORMAT ('Peak bin count',I10)
 1800 FORMAT ('Finishing:',I10,' points plotted')
 1900 FORMAT ('ERROR',I5,' PLOTTING - TRY TO FINISH PARTIAL PLOT')
      END
