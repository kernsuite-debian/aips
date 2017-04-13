LOCAL INCLUDE 'KNTR.INC'
C                                       Local include for KNTR
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'INCS:PTVC.INC'
      REAL   DOCONT, DOGREY, DOVECT, SEQIN, DSKIN, SEQIN2, DSKIN2,
     *   SEQIN3, DSKIN3, SEQIN4, DSKIN4, XBLC(7), XTRC(7), XZINC,
     *   XYPANE, XYRATO, RANGE(2), INCOLR, TLABEL, DOGRID, PLEV, CLEV,
     *   LEVS(30), CON3C, FACTOR, PAROT, XINC, YINC, PCUT, ICUT, POL3C,
     *   DOBLNK, DOWDGE, DOCIRC, XINVER, STMULT, XHPBP, XLAB, XDOTV,
     *   XTVCH, XGRCH, DODARK, XDKLIN, RGBLEV(3,30), XTVCRN(2)
      HOLLERITH XNAMIN(3), XCLSIN(2), XNAMI2(3), XCLSI2(2), XNAMI3(3),
     *   XCLSI3(2), XNAMI4(3), XCLSI4(2), XFUN, XOMFIL(12), XINFIL(12)
      CHARACTER NAMIN(4)*12, CLSIN(4)*6, OFMFIL*48, TXTMSG*80, INFILE*48
      COMMON /INPARM/ DOCONT, DOGREY, DOVECT, XNAMIN, XCLSIN, SEQIN,
     *   DSKIN, XNAMI2, XCLSI2, SEQIN2, DSKIN2, XNAMI3, XCLSI3, SEQIN3,
     *   DSKIN3, XNAMI4, XCLSI4, SEQIN4, DSKIN4, XBLC, XTRC, XZINC,
     *   XYPANE, XYRATO, RANGE, XFUN, XOMFIL, INCOLR, TLABEL, DOGRID,
     *   PLEV, CLEV, LEVS, CON3C, FACTOR, PAROT, XINC, YINC, PCUT, ICUT,
     *   POL3C, DOBLNK, DOWDGE, DOCIRC, XINVER, STMULT, XHPBP, XLAB,
     *   XINFIL, XDOTV, XTVCH, XGRCH, DODARK, XDKLIN, RGBLEV, XTVCRN
      COMMON /CHRCOM/ NAMIN, CLSIN, OFMFIL, TXTMSG, INFILE
C
      REAL      CH(4), MULT, PEAK, YGAP, GFAC, GOFF, BLC(7,4), TRC(7,4),
     *   XOFF, YOFF, WRANGE(2), ROFM(TVMLOU), GOFM(TVMLOU),
     *   BOFM(TVMLOU), NBLC(2), NTRC(2), ABLC(2), ATRC(2), CCOL(3)
      LOGICAL   DOTV, DOOFM, DOCOLR, DOCONV, BMBLNK, FORCEC, NEDCMT
      INTEGER   IXWDGE, IYWDGE, ILPVAL, IHPVAL, ZINC, LHPVAL, LLPVAL,
     *   GRCHN, TVCHN, TVCORN(2), IGBUFF(256), MLUN(4), MIND(4), PG, PC,
     *   PI, ISEQ(4), IVOL(4), CNO(4), BMCORN, SCRTCH(256), NPARMS,
     *   IVER, DO3C, RGBLAB, NOFM
      COMMON /PLPRM/ CH, MULT, PEAK, YGAP, IXWDGE, IYWDGE, GFAC, GOFF,
     *   ILPVAL, IHPVAL, BLC, TRC, ZINC, XOFF, YOFF, LHPVAL, LLPVAL,
     *   WRANGE, GRCHN, TVCHN, TVCORN, DOTV, DOOFM, DOCOLR, ROFM, GOFM,
     *   BOFM, IGBUFF, MLUN, MIND, PG, PC, PI, ISEQ, IVOL, CNO, NBLC,
     *   NTRC, ABLC, ATRC, BMCORN, SCRTCH, DOCONV, NPARMS, IVER, BMBLNK,
     *   FORCEC, DO3C, CCOL, RGBLAB, NEDCMT, NOFM
C                                       Header blocks etc
      INTEGER   PCATI(256,4)
      REAL      PCATR(256,4)
      HOLLERITH PCATH(256,4)
      DOUBLE PRECISION PCATD(128,4)
      REAL      RBLK(MABFSS,4)
      INTEGER   IBLK(MABFSS,4)
      COMMON /MAPHDR/ PCATI
      INCLUDE 'INCS:DCNT.INC'
      EQUIVALENCE (PCATI, PCATR, PCATH, PCATD)
      EQUIVALENCE (IBLK, RBLK, IBUFF)
LOCAL END
      PROGRAM KNTR
C-----------------------------------------------------------------------
C! Contour image suitable for pen plotters plus grey scale
C# Map Plot
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2002-2004, 2006-2008, 2011-2012, 2014-2015
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
C   KNTR is a Radiophysics task based on CNTR with the difference that
C   KNSCAN-TRACE-CALC-PLOTPT routines are used to do the contouring.
C
C   CNTR will write commands to a plot file for the execution of a
C   contour plot for a cataloged image file.  The program runs as a
C   detached task initiated from AIPS. First a cataloged file is found
C   using data passed from AIPS.  The list of associated files is
C   searched for PLot files to find the highest version number.  Then a
C   PLot file is created for this map and the catalog header is
C   updated.  Next the graphics commands are written to the plot file.
C   TELL commands 'ABOR' and 'QUIT' are supported.
C   INPUTS:   (from AIPS)
C      DOCONT         -1.0         1.0    > 0 => do contours
C      DOGREY         -1.0         1.0    > 0 => do grey scale
C      INNAME                             Contour image name
C      INCLASS                            Contour image class
C      INSEQ           0.0      9999.0    Contour image seq. #
C      INDISK                             Contour image disk drive #
C      IN2NAME                            Grey scale image name
C      IN2CLASS                           Grey scale image class
C      IN2SEQ          0.0      9999.0    Grey scale image seq. #
C      IN2DISK                            Grey scale disk drive #
C      BLC             0.0     16384.0    Bottom left corner of image
C                                           0=>entire image
C      TRC             0.0     16384.0    Top right corner of image
C                                           0=>entire image
C                                         Multiple planes of a cube
C                                         will be plotted in panels.
C      XYRATIO         0.0        10.0    X to Y axis plot ratio. 0=>
C                                         header inc or window ratio
C      PIXRANGE                           Min,Max of image intensity
C                                           0 => entire range.
C      LTYPE         -10.0        10.0    Type of labeling: 1 border,
C                                         2 no ticks, 3 standard, 4 rel
C                                         to center, 5 rel to subim cen
C                                         6 pixels, 7-10 as 3-6 with
C                                         only tick labels
C                                         <0 -> no date/time
C      DOALIGN        -2.0         1.0    > 0 => images must line up
C                                            (see HELP DOALIGN)
C      PLEV          -99.0       100.0    Percent of peak for levs.
C      CLEV                               Absolute value for levs
C                                            (used only if PLEV = 0).
C      LEVS        -9999.0     99999.0    Contour levels (up to 30).
C      DOWEDGE        -1.0         2.0    > 0 => plot a wedge also.
C                                         > 1 => put on the right edge.
C      DOCIRCLE       -1.0         1.0    > 0 => extend ticks to form
C                                         coordinate grid
C      INVERS          0.0     46655.0    STar file version number.
C      STFACTOR    -9999.0      9999.0    Scale star sizes: 0 => none.
C                                         > 0 crosses with no labels
C                                         < 0 crosses with labels
C      OFFSET         -1.0         5.0    Position for beam plot:
C                                          -1: don't plot beam
C                                           1: lower left (default)
C                                           2: lower right
C                                           3: upper right
C                                          4: upper left
C                                           5: plot in separate pane
C      NCOUNT         -1.0         1.0     0->label each pane with the
C                                             pane number
C                                          1->label each with velocity
C                                             (if ALTSW has been run)
C                                         -1->do not label each pane
C      DOTV           -1.0         1.0    > 0 Do plot on the TV, else
C                                         make a plot file
C      GRCHAN          0.0         4.0    Graphics channel 0 => 1.
C      TVCORN          0.0     16384.0    TV pixel location of bottom
C                                         left corner of image 0=> self
C                                         scale, non 0 => pixel scale.
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGNAM*6
      INCLUDE 'KNTR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA PRGNAM /'KNTR  '/
C-----------------------------------------------------------------------
C                                       init inputs
      CALL KNTRIN (PRGNAM, IRET)
C                                       do plot
      IF (IRET.EQ.0) CALL KNTRPL (IRET)
C                                       Close down
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE KNTRIN (PRGNAM, IRET)
C-----------------------------------------------------------------------
C   Does init functions for KNTR
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   IRET
C
      CHARACTER TYPIN*2, STAT*4, DEFCLS(2)*6, STOKES*8, RGB*8
      INTEGER   IROUND, IERR, I, IUSER, J, QLOCS, ULOCS, RLOCS, ILOCS
      REAL      BMAJ, BMIN, BPA, WRANG2(2), RANGE2(2), TEMP
      INCLUDE 'KNTR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA TYPIN /'  '/
      DATA DEFCLS /'PPOL','PANG'/
      DATA STOKES, RGB /'STOKES','RGB'/
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      TSKNAM = PRGNAM
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = MABFSS * 2
      NCFILE = 0
      NSCR = 0
C                                       Get input values from AIPS.
      NPARMS = 222
      IRET = 0
      CALL GTPARM (PRGNAM, NPARMS, RQUICK, DOCONT, SCRTCH, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         IRET = 8
         END IF
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 8
C
      ISEQ(1) = SEQIN + .01
      IVOL(1) = DSKIN + .01
      ISEQ(2) = SEQIN2 + .01
      IVOL(2) = DSKIN2 + .01
      ISEQ(3) = SEQIN3 + .01
      IVOL(3) = DSKIN3 + .01
      ISEQ(4) = SEQIN4 + .01
      IVOL(4) = DSKIN4 + .01
      IUSER = NLUSER
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = IROUND (XTVCH)
      TVCHN = MAX (1, TVCHN)
      TVCORN(1) = IROUND (XTVCRN(1))
      TVCORN(2) = IROUND (XTVCRN(2))
      ZINC = IROUND (XZINC)
      IF (ZINC.EQ.0) ZINC = 1
      XZINC = ZINC
C                                       Characters
      CALL H2CHR (12, 1, XNAMIN, NAMIN(1))
      CALL H2CHR (6, 1, XCLSIN, CLSIN(1))
      CALL H2CHR (12, 1, XNAMI2, NAMIN(2))
      CALL H2CHR (6, 1, XCLSI2, CLSIN(2))
      CALL H2CHR (12, 1, XNAMI3, NAMIN(3))
      CALL H2CHR (6, 1, XCLSI3, CLSIN(3))
      CALL H2CHR (12, 1, XNAMI4, NAMIN(4))
      CALL H2CHR (6, 1, XCLSI4, CLSIN(4))
      CALL H2CHR (48, 1, XOMFIL, OFMFIL)
      CALL H2CHR (48, 1, XINFIL, INFILE)
      IF ((XDKLIN.LE.0.0) .OR. (XDKLIN.GT.1.0)) XDKLIN = 0.33
      IF (DOGREY.LE.0.0) DOWDGE = -1.0
C                                       Image pointers
      PG = IROUND (DOGREY)
      IF ((DOGREY.GT.0.0) .AND. (PG.EQ.0)) PG = 1
      PC = IROUND (DOCONT)
      IF ((DOCONT.GT.0.0) .AND. (PC.EQ.0)) PC = 1
      PI = IROUND (DOVECT)
      IF ((DOVECT.GT.0.0) .AND. (PI.EQ.0)) PI = 1
C                                       Image 2 -> 1 when 1 ignored
      IF ((PC.LE.0) .AND. (PG.LE.0) .AND. (PI.LE.0)) THEN
         MSGTXT = 'NO PLOTS REQUESTED'
         GO TO 990
      ELSE IF ((PC.NE.1) .AND. (PG.NE.1) .AND. (PI.NE.1)) THEN
         MSGTXT = 'WARNING: IMAGE 1 NOT SPECIFIED, SWITCHING'
         CALL MSGWRT (6)
         IF (PC.EQ.2) PC = 1
         IF (PG.EQ.2) PG = 1
         IF (PI.EQ.2) PI = 1
         NAMIN(1) = NAMIN(2)
         CLSIN(1) = CLSIN(2)
         ISEQ(1) = ISEQ(2)
         IVOL(1) = IVOL(2)
         END IF
C                                       Levels defaults
      IF ((CLEV.EQ.0.0) .AND. (PLEV.EQ.0.0)) PLEV = 10.0
      IF ((LEVS(1).EQ.0.0) .AND. (LEVS(2).LE.LEVS(1))) THEN
         DO 10 I = 1,10
            LEVS(I) = I-11
            LEVS(I+10) = I
            LEVS(I+20) = 0.
 10      CONTINUE
         END IF
C                                       Open map files & get headers
      IVER = 0
      STAT = 'HDWR'
      IF (DOTV) STAT = 'READ'
      ILOCS = -1
      QLOCS = -1
      ULOCS = -1
      DO 20 I = 1,4
         MLUN(I) = 0
         MIND(I) = 0
         IF ((PC.EQ.I) .OR. (PG.EQ.I) .OR. (PI.EQ.I) .OR. ((PI.GT.0)
     *      .AND. (I.GT.2))) THEN
            MLUN(I) = 15+I
C                                       defaults
            IF (I.GT.2) THEN
               IF (NAMIN(I).EQ.' ') NAMIN(I) = NAMIN(PI)
               IF (CLSIN(I).EQ.' ') CLSIN(I) = DEFCLS(I-2)
               END IF
            CALL MAPOPN (STAT, IVOL(I), NAMIN(I), CLSIN(I), ISEQ(I),
     *         TYPIN, IUSER, MLUN(I), MIND(I), CNO(I), PCATI(1,I),
     *         SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 999
            NCFILE = NCFILE + 1
            FVOL(NCFILE) = IVOL(I)
            FCNO(NCFILE) = CNO(I)
            FRW(NCFILE) = 0
C                                       Add extension file to header.
            IF ((I.EQ.1) .AND. (.NOT.DOTV)) THEN
               CALL MADDEX ('PL', IVOL(1), CNO(1), PCATI(1,1), SCRTCH,
     *            .TRUE., 'READ', IVER, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
            STAT = 'READ'
C                                       Ipol image an IQU cube?
            IF (I.EQ.PI) THEN
               CALL AXEFND (8, STOKES, PCATI(KIDIM,I), PCATH(KHCTP,I),
     *            ILOCS, IERR)
               IF (IERR.NE.0) ILOCS = -1
C                                       Yes
               IF ((ILOCS.GE.2) .AND. (PCATI(KINAX+ILOCS,I).GE.3) .AND.
     *            (ABS(PCATD(KDCRV+ILOCS,I)-1.0D0).LT.1.0D-2) .AND.
     *            (ABS(PCATR(KRCIC+ILOCS,I)-1.0).LE.0.01)) THEN
                  MSGTXT = 'IPOL image is IQU cube, ignore IN3NAME,'
     *               // ' IN4NAME'
                  CALL MSGWRT (1)
                  NAMIN(3) = NAMIN(I)
                  CLSIN(3) = CLSIN(I)
                  ISEQ(3) = ISEQ(I)
                  IVOL(3) = IVOL(I)
                  NAMIN(4) = NAMIN(I)
                  CLSIN(4) = CLSIN(I)
                  ISEQ(4) = ISEQ(I)
                  IVOL(4) = IVOL(I)
               ELSE
                  ILOCS = -1
                  END IF
               END IF
            END IF
 20      CONTINUE
C                                       INCOLR ?
      RLOCS = -1
      IF ((PG.GT.0) .AND. (INCOLR.GT.0.0)) THEN
         CALL AXEFND (8, RGB, PCATI(KIDIM,PG), PCATH(KHCTP,PG), RLOCS,
     *      IERR)
         IF ((IERR.EQ.0) .AND. ((RLOCS.NE.2) .OR.
     *      (PCATI(KINAX+2,PG).LT.3))) THEN
            MSGTXT = 'DOCOLR REQUESTED, BUT RGB AXIS NOT 3RD AXIS'
            CALL MSGWRT (7)
            RLOCS = 99
            END IF
         END IF
      IF (RLOCS.NE.2) INCOLR = -1.0
C                                       are some equal?
      IF (PC.GT.0) THEN
         IF ((PG.GT.0) .AND. (PG.NE.PC)) THEN
            IF ((CNO(PG).EQ.CNO(PC)) .AND. (IVOL(PG).EQ.IVOL(PC)))
     *         PG = PC
            END IF
         IF ((PI.GT.0) .AND. (PI.NE.PC)) THEN
            IF ((CNO(PI).EQ.CNO(PC)) .AND. (IVOL(PI).EQ.IVOL(PC)))
     *         PI = PC
            END IF
         END IF
      IF (PG.GT.0) THEN
         IF ((PI.GT.0) .AND. (PI.NE.PG)) THEN
            IF ((CNO(PI).EQ.CNO(PG)) .AND. (IVOL(PI).EQ.IVOL(PG)))
     *         PI = PG
            END IF
         END IF
      IF ((PI.EQ.2) .AND. (PC.EQ.2) .AND. (PG.EQ.2)) THEN
         PI = 1
         PC = 1
         PG = 1
         END IF
C                                       set alignment corners
      CALL ALIGN (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       check conversion
      DOCONV = .FALSE.
      IF (PI.GT.0) THEN
         CALL AXEFND (8, STOKES, PCATI(KIDIM,3), PCATH(KHCTP,3), QLOCS,
     *      IERR)
         IF (IERR.NE.0) QLOCS = -1
C                                       Check for Q pol
         IF (QLOCS.GE.2) THEN
            TEMP = PCATR(KRCRP+QLOCS,3) + (2.0D0 - PCATD(KDCRV+QLOCS,3))
     *         / MAX (0.001, PCATR(KRCIC+QLOCS,3))
            J = IROUND (TEMP)
            IF ((ABS(J-TEMP).LT.0.01) .AND. (J.GE.1) .AND.
     *         (J.LE.PCATI(KINAX+QLOCS,3))) THEN
               BLC(1+QLOCS,3) = J
               TRC(1+QLOCS,3) = J
               END IF
            END IF
         CALL AXEFND (8, STOKES, PCATI(KIDIM,4), PCATH(KHCTP,4), ULOCS,
     *      IERR)
         IF (IERR.NE.0) ULOCS = -1
C                                       Check for U pol
         IF (ULOCS.GE.2) THEN
            TEMP = PCATR(KRCRP+ULOCS,4) + (3.0D0 - PCATD(KDCRV+ULOCS,4))
     *         / MAX (0.001, PCATR(KRCIC+ULOCS,4))
            J = IROUND (TEMP)
            IF ((ABS(J-TEMP).LT.0.01) .AND. (J.GE.1) .AND.
     *         (J.LE.PCATI(KINAX+ULOCS,4))) THEN
               BLC(1+ULOCS,4) = J
               TRC(1+ULOCS,4) = J
               END IF
            END IF
C                                       are we converting?
         IF ((ULOCS.GE.0) .AND. (QLOCS.GE.0)) THEN
            TEMP = PCATD(KDCRV+QLOCS,3) +  PCATR(KRCIC+QLOCS,3) *
     *         (BLC(1+QLOCS,3) - PCATR(KRCRP+QLOCS,3))
            J = IROUND (TEMP)
            TEMP = PCATD(KDCRV+ULOCS,4) +  PCATR(KRCIC+ULOCS,4) *
     *         (BLC(1+ULOCS,4) - PCATR(KRCRP+ULOCS,4))
            I = IROUND (TEMP)
            DOCONV = (J.EQ.2) .AND. (I.EQ.3)
            END IF
C                                       code for EXTLIST
         IF (DOCONV) DOVECT = DOVECT + 10.0
         END IF
C                                       save adverbs for GINIT
      SEQIN = ISEQ(1)
      DSKIN = IVOL(1)
      SEQIN2 = ISEQ(2)
      DSKIN2 = IVOL(2)
      SEQIN3 = ISEQ(3)
      DSKIN3 = IVOL(3)
      SEQIN4 = ISEQ(4)
      DSKIN4 = IVOL(4)
      CALL CHR2H (12, NAMIN(1), 1, XNAMIN)
      CALL CHR2H (6, CLSIN(1), 1, XCLSIN)
      CALL CHR2H (12, NAMIN(2), 1, XNAMI2)
      CALL CHR2H (6, CLSIN(2), 1, XCLSI2)
      CALL CHR2H (12, NAMIN(3), 1, XNAMI3)
      CALL CHR2H (6, CLSIN(3), 1, XCLSI3)
      CALL CHR2H (12, NAMIN(4), 1, XNAMI4)
      CALL CHR2H (6, CLSIN(4), 1, XCLSI4)
C                                       check ST plot parms
      IF (XINVER.LT.0.0) STMULT = 0.0
      IF (STMULT.NE.0.0) THEN
         CALL FNDEXT ('ST', PCATI(1,1), I)
         IF (I.GT.0) THEN
            J = XINVER + 0.1
            IF (J.LE.0) J = I
            XINVER = J
         ELSE
            XINVER = 0.0
            STMULT = 0.0
            END IF
      ELSE
         XINVER = 0.0
         STMULT = 0.0
         END IF
C                                       Contour interval
      IF (PC.GT.0) THEN
         PEAK = MAX (ABS(PCATR(KRDMX,PC)), ABS(PCATR(KRDMN,PC)))
         IF ((CLEV.LE.0.0) .AND. (PLEV.LE.0.0)) PLEV = 10.0
         MULT = CLEV
         IF (PLEV.NE.0.0) MULT = PEAK * PLEV / 100.0
         CLEV = MULT
         PLEV = 0.0
         END IF
C                                       check Clean beam
      BMCORN = IROUND (XHPBP)
      BMBLNK = BMCORN.LT.0.0
      BMCORN = ABS (BMCORN)
      IF (BMCORN.GT.20) BMCORN = 1
      IF (BMCORN.GT.0) THEN
         BMAJ = ABS (PCATR(KRBMJ,1))
         BMIN = ABS (PCATR(KRBMN,1))
         BPA = PCATR(KRBPA,1)
         IF ((BMAJ.LE.1.E-9) .OR. (BMIN.LE.1.E-9)) THEN
            BMAJ = ABS (PCATR(KRBMJ,2))
            BMIN = ABS (PCATR(KRBMN,2))
            BPA = PCATR(KRBPA,2)
            END IF
         IF ((BMAJ.LE.1.E-9) .OR. (BMIN.LE.1.E-9)) THEN
            BMCORN = 0
         ELSE
            PCATR(KRBMJ,1) = BMAJ
            PCATR(KRBMN,1) = BMIN
            PCATR(KRBPA,1) = BPA
            END IF
         END IF
C                                       grey-scale range
      IF (PG.GT.0) THEN
         WRANGE(1) = 0.0
         WRANGE(2) = 0.0
         CALL RNGSET (RANGE, PCATR(KRDMX,PG), PCATR(KRDMN,PG), RANGE2)
         CALL RNGSET (WRANGE, PCATR(KRDMX,PG), PCATR(KRDMN,PG), WRANG2)
         GFAC = 2.0D0 ** MIN (30, NBITWD)  -  4.0D0
         IF (DOWDGE.LT.2.5) CALL RCOPY (2, RANGE2, WRANG2)
         GFAC = (WRANG2(2)-WRANG2(1)) / GFAC
         GOFF = (WRANG2(2)+WRANG2(1)) / 2.0D0
         RANGE2(2) = (RANGE2(2) - GOFF) / GFAC
         RANGE2(1) = (RANGE2(1) - GOFF) / GFAC
         IHPVAL = IROUND (RANGE2(2))
         ILPVAL = IROUND (RANGE2(1))
         RANGE(1) = (ILPVAL * GFAC + GOFF)
         RANGE(2) = (IHPVAL * GFAC + GOFF)
         WRANG2(2) = (WRANG2(2) - GOFF) / GFAC
         WRANG2(1) = (WRANG2(1) - GOFF) / GFAC
         LHPVAL = IROUND (WRANG2(2))
         LLPVAL = IROUND (WRANG2(1))
         WRANGE(1) = (LLPVAL * GFAC + GOFF)
         WRANGE(2) = (LHPVAL * GFAC + GOFF)
         END IF
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR GETTING PARAMETERS FROM AIPS. GTPARM ERR =',I5)
      END
      SUBROUTINE KNTRPL (IRET)
C-----------------------------------------------------------------------
C   Does the plot for KNTR
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER IGFILE*48, SPRTXT*8, LTYP(2)*20, LPREF(2)*5, XTXT(2)*80,
     *   LEVTXT*200
      INTEGER   IROUND, IGLUN, ILABEL, INVER, I, IGSIZE, ITYPE, J, K,
     *   IGFIND, IANGL, NCHAR, NXP, NXPANE, LXWDGE, LYWDGE, NXPIX, NYP,
     *   NYPANE, NYPIX, PLANE1(4), PLANE2(4), IDEPTH(5), IPLANE, PANLAB,
     *   NXT, NYT, MIDPAN, NPL(4), MAXP, NXTEXT, PLBUF(256), BLOCN,
     *   BBLC(2), BTRC(2), LTYPE
      LOGICAL   DOGR, BEDPAN, PFLAG, T, RED2B, ISRGBL
      REAL      DCX, DCY, CPOS(2), XCH(4), XGAP, X, Y
      DOUBLE PRECISION DPOS(3)
      INCLUDE 'KNTR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      NEDCMT = .FALSE.
C                                       get parameters of plot
      DOGR = DOCIRC.GT.0.0
      RGBLAB = IROUND (ABS(TLABEL))
      RGBLAB = MOD (RGBLAB,100)
      IF (RGBLAB.LE.0) RGBLAB = 3
      RGBLAB = (RGBLAB-1) / 10
      ILABEL = IROUND (TLABEL)
      LTYPE = MOD (ABS(ILABEL), 100)
      IF (LTYPE.LE.0) LTYPE = 3
      LTYPE = MOD (LTYPE-1,10) + 1
      IF (ILABEL.GT.0) THEN
         ILABEL = (ILABEL/100)*100 + LTYPE
      ELSE
         ILABEL = (ILABEL/100)*100 - LTYPE
         END IF
      BEDPAN = (MOD (BMCORN,5).EQ.0) .AND. (BMCORN.GT.0)
      PANLAB = IROUND (XLAB)
      IF (PANLAB.GT.2) PANLAB = 2
C                                       Build file name.
      CALL ZPHFIL ('PL', IVOL(1), CNO(1), IVER, IGFILE, IRET)
      IF (IRET.NE.0) GO TO 999
C
      IXWDGE = 0
      IYWDGE = 0
      IF (DOWDGE.GT.0.0) THEN
         IF ((DOWDGE.LT.1.5) .OR. ((DOWDGE.GE.2.5) .AND.
     *      (DOWDGE.LT.3.5))) THEN
            IYWDGE = MAX (1.0, (TRC(2,PG)-BLC(2,PG))/20.0) + 2.5
         ELSE
            IXWDGE = MAX (1.0, (TRC(1,PG)-BLC(1,PG))/20.) + 2.5
            END IF
         IF (INCOLR.GT.0.0) THEN
            IXWDGE = ((IXWDGE+2)/3) * 3
            IYWDGE = ((IYWDGE+2)/3) * 3
            END IF
         END IF
C                                       plane control ??
      NPL(1) = (TRC(3,1) - BLC(3,1)) / ABS (ZINC) + 1.1
      IF (ZINC.GT.0) THEN
         TRC(3,1) = BLC(3,1) + ZINC * (NPL(1)-1)
         PLANE1(1) = IROUND (BLC(3,1))
         PLANE2(1) = IROUND (TRC(3,1))
      ELSE
         BLC(3,1) = TRC(3,1) + ZINC * (NPL(1)-1)
         PLANE2(1) = IROUND (BLC(3,1))
         PLANE1(1) = IROUND (TRC(3,1))
         END IF
      DO 10 I = 2,4
         IF (ZINC.GT.0) THEN
            PLANE1(I) = IROUND (BLC(3,I))
            PLANE2(I) = IROUND (TRC(3,I))
         ELSE
            PLANE2(I) = IROUND (BLC(3,I))
            PLANE1(I) = IROUND (TRC(3,I))
            END IF
         IF (PLANE1(I).NE.PLANE2(I)) THEN
            NPL(I) = (TRC(3,I) - BLC(3,I)) / ABS (ZINC) + 1.1
            NPL(I) = MIN (NPL(1), NPL(I))
            IF (ZINC.GT.0) THEN
               TRC(3,I) = BLC(3,I) + ZINC * (NPL(I)-1)
               PLANE2(I) = IROUND (TRC(3,I))
            ELSE
               BLC(3,I) = TRC(3,I) + ZINC * (NPL(I)-1)
               PLANE2(I) = IROUND (BLC(3,I))
               END IF
            IF (BEDPAN) THEN
               PLANE2(I) = PLANE2(I) + ZINC
               NPL(I) = NPL(I) + 1
               END IF
         ELSE
            NPL(I) = 1
            END IF
 10      CONTINUE
      IF (BEDPAN) THEN
         PLANE2(1) = PLANE2(1) + ZINC
         NPL(1) = NPL(1) + 1
         END IF
      NYPANE = IROUND (XYPANE)
      IF (NYPANE.LE.0) THEN
         NYPANE = SQRT (REAL(NPL(1)))
         IF (NYPANE*NYPANE.LT.NPL(1)) NYPANE = NYPANE + 1
         END IF
      NXPANE = NPL(1) / NYPANE
      IF (NXPANE*NYPANE.LT.NPL(1)) NXPANE = NXPANE + 1
      NBLC(1) = BLC(1,1) - 1.0
      NBLC(2) = BLC(2,1) - 1.0
      NTRC(1) = TRC(1,1) + 1.0
      NTRC(2) = TRC(2,1) + 1.0
      ABLC(1) = NBLC(1)
      ABLC(2) = NBLC(2)
      NXPIX = NTRC(1) - NBLC(1) + 0.1
      NYPIX = NTRC(2) - NBLC(2) + 0.1
      MAXP = MAX (NXPIX*NXPANE, NYPIX*NYPANE)
      ATRC(1) = ABLC(1) + NXPANE*NXPIX + IXWDGE
      ATRC(2) = ABLC(2) + NYPANE*NYPIX + IYWDGE
      NXT = ATRC(1) - ABLC(1) + 0.1
      NYT = ATRC(2) - ABLC(2) + 0.1
C                                       fill in defaults in PARMS
      CALL RCOPY (4, BLC(4,1), TRC(4,1))
C                                       Default XYRATO: ratio of
C                                       incr if related.
      DO 20 I = 1,5
         IDEPTH(I) = BLC(I+2,1) + 0.01
 20      CONTINUE
      LOCNUM = 1
      CALL SETLOC (IDEPTH, T)
      IF ((XYRATO.LE.0.01) .OR. (XYRATO.GT.320.0)) THEN
         IF ((AXTYP(LOCNUM).EQ.1) .AND. (AXINC(2,LOCNUM).NE.0.0)) XYRATO
     *      = ABS (AXINC(1,LOCNUM) / AXINC(2,LOCNUM))
         IF (((XYRATO.LE.0.04) .OR. (XYRATO.GT.25.)) .AND.
     *      (TRC(1,1).NE.BLC(1,1))) XYRATO = (TRC(2,1)-BLC(2,1)) /
     *      (TRC(1,1)-BLC(1,1))
         IF ((XYRATO.LE.0.04) .OR. (XYRATO.GT.25.)) XYRATO = 1.0
         END IF
C                                       for extlist
      IF (PG.GT.0) THEN
         CALL H2CHR (2, 1, XFUN, GPHFUN)
         IF ((GPHFUN.NE.'LG') .AND. (GPHFUN.NE.'NG') .AND.
     *      (GPHFUN.NE.'SQ') .AND. (GPHFUN.NE.'NQ') .AND.
     *      (GPHFUN.NE.'NE') .AND. (GPHFUN.NE.'L2') .AND.
     *      (GPHFUN.NE.'N2')) GPHFUN = 'LN'
         CALL CHR2H (2, GPHFUN, 1, XFUN)
         END IF
C                                       Init graph file.
      IGSIZE = 1
      ITYPE = 29
      IF (INCOLR.GT.0.0) OFMFIL = ' '
      CALL GINIT (IVOL, CNO(1), IGFILE, IGSIZE, ITYPE, NPARMS, DOCONT,
     *   DOTV, TVCHN, GRCHN, TVCORN, PCATI(1,1), PLBUF, IGLUN, IGFIND,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         CALL MSGWRT (7)
         GO TO 970
         END IF
      GPHCUT = XDKLIN
      IF (DOGREY.GT.0.0) THEN
         IF (DOTV) GPHDOD = (DODARK.GT.0.0)
         CALL GETOFM (OFMFIL, GPHDOT, GPHTVC, DOOFM, DOCOLR, ROFM, GOFM,
     *      BOFM, NOFM, IRET)
         IF (IRET.NE.0) THEN
            DOOFM = .FALSE.
            IRET = 0
            END IF
         IF (.NOT.DOOFM) DOCOLR = .FALSE.
      ELSE
         INCOLR = -1.0
         GPHDOD = .FALSE.
         DOOFM = .FALSE.
         DOCOLR = .FALSE.
         END IF
      IF (INCOLR.GT.0.0) DOCOLR = .TRUE.
      IF (.NOT.DOOFM) OFMFIL = ' '
      CALL CHR2H (48, OFMFIL, 1, XOMFIL)
      IF (NXPANE*NYPANE.LE.1) CON3C = -1.0
C                                       color forced
      ISRGBL = .FALSE.
      DO3C = 0
      DO 25 K = 1,30
         DO 24 J = 1,3
            IF (RGBLEV(J,K).GT.0.0) ISRGBL = .TRUE.
 24         CONTINUE
 25      CONTINUE
      IF (ISRGBL) DO3C = 2
      IF (CON3C.GT.0.0) DO3C = 1
      FORCEC = ISRGBL .OR. (POL3C.GT.0.0) .OR. (CON3C.GT.0.0)
      IF (DOCOLR) FORCEC = .FALSE.
      IF ((DO3C.GT.0) .AND. (DODARK.GT.0.0)) THEN
         MSGTXT = 'DODARK not used for color contours'
         CALL MSGWRT (2)
         END IF
C                                       extra border chars
      CALL RFILL (4, 0.0, CH)
      YGAP = 0.0
C                                       Write axis labeling commands.
      IF ((DO3C.NE.2) .OR. (RGBLAB.GE.2)) ISRGBL = .FALSE.
      CALL KOMLAB (ILABEL, ISRGBL, LEVTXT, PLBUF, IRET)
      IF (IRET.NE.0) GO TO 960
      LTYP(1) = CTYP(1,1)
      LPREF(1) = CPREF(1,1)
      LTYP(2) = CTYP(2,1)
      LPREF(2) = CPREF(2,1)
      LOCNUM = 3
      CALL SETLOC (IDEPTH, T)
      LOCNUM = 1
      IANGL = 0
C                                       Loop for each plane.
      MIDPAN = (NXPANE/4)*2+1
      NXP = ABS (PLANE2(1) - PLANE1(1)) / ABS (ZINC) + 1 -
     *   (NYPANE-1) * NXPANE
      IF (MIDPAN.GT.NXP) MIDPAN = ((NXP + 1) / 2) * 2 - 1
      NXP = 0
      NYP = NYPANE
      RED2B = .TRUE.
      IF (DO3C.EQ.1) THEN
         CALL H2CHR (8, 1, PCATH(KHCTP+4,1), SPRTXT)
         IF (SPRTXT(:4).EQ.'FREQ') THEN
            RED2B = PCATR(KRCIC+2,1).GT.0.0
         ELSE IF (SPRTXT(:4).EQ.'VELO') THEN
            RED2B = PCATR(KRCIC+2,1).LT.0.0
         ELSE IF (SPRTXT(:4).EQ.'FELO') THEN
            RED2B = PCATR(KRCIC+2,1).LT.0.0
            END IF
         END IF
      IF (ZINC.LT.0) RED2B = .NOT.RED2B
      DO 100 IPLANE = PLANE1(1),PLANE2(1),ZINC
C                                       Set cube plane number.
         BLC(3,1) = IPLANE
         TRC(3,1) = IPLANE
         IDEPTH(1) = IPLANE
         CALL RFILL (4, 0.0, XCH)
         XGAP = 0.0
         CALL LABINI (BLC, TRC, IDEPTH, XCH, ILABEL, .FALSE., XGAP,
     *      XTXT, NXTEXT)
         DO 30 I = 2,4
            J = PLANE1(I) + IPLANE - PLANE1(1)
            IF (PLANE1(I).EQ.PLANE2(I)) J = PLANE1(I)
            BLC(3,I) = J
            TRC(3,I) = J
 30         CONTINUE
C                                       beam corners
         IF ((BMCORN.GT.0) .AND. (MOD(BMCORN,5).GT.0) .AND.
     *      (IPLANE.EQ.PLANE1(1)) .AND. (BMBLNK)) THEN
            CALL PLTBSZ (BMCORN, BLC, TRC, PCATR, BBLC, BTRC)
         ELSE
            BBLC(1) = 0
            BBLC(2) = 0
            BTRC(1) = 0
            BTRC(2) = 0
            END IF
C                                       Set panel offsets.
         NXP = NXP + 1
         IF (NXP.GT.NXPANE) THEN
            NXP = 1
            NYP = NYP - 1
            END IF
C                                       Reset the origin for the AIPS
C                                       plotting routines.
         GPHX1 = AINT(ABLC(1) + 0.999) - (NXP-1)*NXPIX
         GPHY1 = AINT(ABLC(2) + 0.999) - (NYP-1)*NYPIX
         GPHX2 = GPHX1 + NXT
         GPHY2 = GPHY1 + NYT
C                                       3-color contours
         IF (DO3C.EQ.1) THEN
            DCX = IPLANE - PLANE1(1)
            DCX = DCX / (PLANE2(1) - PLANE1(1))
            IF (.NOT.RED2B) DCX = 1.0 - DCX
            CALL COLOR3 (DCX, .FALSE., CCOL)
            CALL G3VCOL (CCOL(1), CCOL(2), CCOL(3), PLBUF, IRET)
            IF (IRET.NE.0) GO TO 960
            END IF
C                                       Grey scale
         IF (DOGREY.GT.0.0) THEN
            WRITE (TXTMSG,1050) IPLANE
            CALL GCOMNT (2, TXTMSG, PLBUF, IRET)
            IF (IRET.NE.0) GO TO 960
            LXWDGE = 0
            IF (NXP.EQ.NXPANE) LXWDGE = IXWDGE
            LYWDGE = 0
            IF (NYP.EQ.NYPANE) LYWDGE = IYWDGE
            PFLAG = (.NOT.BEDPAN) .OR. (IPLANE.NE.PLANE2(1))
            CALL GRDRW (ILABEL, LXWDGE, LYWDGE, PFLAG, BBLC, BTRC,
     *         PLBUF, IRET)
            IF (IRET.NE.0) GO TO 960
            END IF
C                                       Draw borders.
         CALL GLTYPE (1, PLBUF, IRET)
         IF (IRET.NE.0) GO TO 960
         CALL GPOS (NBLC(1), NBLC(2), PLBUF, IRET)
         IF (IRET.NE.0) GO TO 960
         CALL GVEC (NTRC(1), NBLC(2), PLBUF, IRET)
         IF (IRET.NE.0) GO TO 960
         CALL GVEC (NTRC(1), NTRC(2), PLBUF, IRET)
         IF (IRET.NE.0) GO TO 960
         CALL GVEC (NBLC(1), NTRC(2), PLBUF, IRET)
         IF (IRET.NE.0) GO TO 960
         CALL GVEC (NBLC(1), NBLC(2), PLBUF, IRET)
         IF (IRET.NE.0) GO TO 960
C                                       Label control for the X-axis.
         IF ((NYP.EQ.1) .AND. (MOD(NXP,2).EQ.1)) THEN
            CTYP(1,LOCNUM) = LTYP(1)
            IF (NXP.EQ.MIDPAN) THEN
               CPREF(1,LOCNUM)  = LPREF(1)
            ELSE
               CPREF(1,LOCNUM)  = '-1'
               END IF
         ELSE
            CPREF(1,LOCNUM) = ' '
            CTYP(1,LOCNUM)  = ' '
            END IF
C                                       Label control for the Y-axis.
         IF (NXP.EQ.1) THEN
            IF (NYP.EQ.(NYPANE+1)/2) THEN
               CPREF(2,LOCNUM) = LPREF(2)
            ELSE
               CPREF(2,LOCNUM) = '-1'
               END IF
            CTYP(2,LOCNUM)  = LTYP(2)
         ELSE
            CPREF(2,LOCNUM) = ' '
            CTYP(2,LOCNUM)  = ' '
            END IF
         CALL CLAB1 (NBLC, NTRC, CH, ILABEL, XYRATO, DOGR, PLBUF, IRET)
         IF (IRET.NE.0) GO TO 960
C                                       Draw HPBW
         IF ((BMCORN.GT.0) .AND. (MOD(BMCORN,5).GT.0) .AND.
     *      (IPLANE.EQ.PLANE1(1))) THEN
            LOCNUM = 3
            CALL PLTBEM (BMCORN, BLC, TRC, PCATR, PLBUF, IRET)
            LOCNUM = 1
            IF (IRET.GT.0) GO TO 960
            IF (IRET.NE.0) BMCORN = 0
C                                       Isolated beam plot.
         ELSE IF ((BEDPAN) .AND. (IPLANE.EQ.PLANE2(1))) THEN
            LOCNUM = 3
            CALL PLTBEM (BMCORN, BLC, TRC, PCATR, PLBUF, IRET)
            LOCNUM = 1
            IF (IRET.GT.0) GO TO 960
            IF (IRET.NE.0) BMCORN = -1
            DCY   = -1.5
            SPRTXT = 'Beam'
            CALL CHTRIM (SPRTXT, 8, SPRTXT, NCHAR)
            DCX = -NCHAR - 3
            CALL GPOS (NTRC(1), NTRC(2), PLBUF, IRET)
            IF (IRET.NE.0) GO TO 960
            CALL GICHAR (1, NCHAR, IANGL, DCX, DCY, SPRTXT, PLBUF,
     *         IRET)
            IF (IRET.NE.0) GO TO 960
            GO TO 100
            END IF
C                                       label plane coordinate
         IF ((PCATI(KIDIM,1).GE.3) .AND. (PCATI(KINAX+2,1).GT.1)) THEN
            IF (PANLAB.EQ.0) THEN
               DCY   = -1.5
               WRITE (SPRTXT,1060) IPLANE
               CALL CHTRIM (SPRTXT, 8, SPRTXT, NCHAR)
               DCX = -NCHAR - 1.3
               CALL GPOS (NTRC(1), NTRC(2), PLBUF, IRET)
               IF (IRET.NE.0) GO TO 960
               CALL GICHAR (1, NCHAR, IANGL, DCX, DCY, SPRTXT, PLBUF,
     *            IRET)
               IF (IRET.NE.0) GO TO 960
            ELSE IF (PANLAB.EQ.1) THEN
               DCY   = -1.5
               CPOS(1) = (BLC(1,1) + TRC(1,1)) / 2.0
               CPOS(2) = (BLC(2,1) + TRC(2,1)) / 2.0
               CALL XYVAL (CPOS(1), CPOS(2), DPOS(1), DPOS(2), DPOS(3),
     *            IRET)
               IF ((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3))
     *            CALL AXSTRN (CTYP(3,LOCNUM), DPOS(3), KLOCA(LOCNUM),
     *            NCHLAB(1,LOCNUM), SAXLAB(1,LOCNUM))
               NCHAR = NCHLAB(1,LOCNUM)
               DCX = -NCHAR - 1.5
               CALL GPOS (NTRC(1), NTRC(2), PLBUF, IRET)
               IF (IRET.NE.0) GO TO 960
               CALL GICHAR (1, NCHAR, IANGL, DCX, DCY, SAXLAB(1,LOCNUM),
     *            PLBUF, IRET)
               IF (IRET.NE.0) GO TO 960
            ELSE IF (PANLAB.EQ.2) THEN
               DCY   = -1.5
               DPOS(3) = (IPLANE - PCATR(KRCRP+2,1)) * PCATR(KRCIC+2,1)
               CALL AXSTRN (CTYP(3,LOCNUM), DPOS(3), 2,
     *            NCHLAB(1,LOCNUM), SAXLAB(1,LOCNUM))
               NCHAR = NCHLAB(1,LOCNUM)
               DCX = -NCHAR - 1.5
               CALL GPOS (NTRC(1), NTRC(2), PLBUF, IRET)
               IF (IRET.NE.0) GO TO 960
               CALL GICHAR (1, NCHAR, IANGL, DCX, DCY, SAXLAB(1,LOCNUM),
     *            PLBUF, IRET)
               IF (IRET.NE.0) GO TO 960
               END IF
            END IF
C                                       Draw stars
         INVER = IROUND (XINVER)
         IF (INVER.GT.0) THEN
            CALL GLTYPE (4, PLBUF, IRET)
            IF (IRET.NE.0) GO TO 950
            LOCNUM = 3
            IF (PG.GT.0) THEN
               BLOCN = 3
               IF (PG.EQ.2) BLOCN = 2
               CALL STARPL (STMULT, IVOL(1), CNO(1), INVER, BLC(1,1),
     *            TRC(1,1), MLUN(PG), MIND(PG), PCATI(1,PG), BLC(1,PG),
     *            BLOCN, PLBUF, IRET)
            ELSE
               CALL STARPL (STMULT, IVOL(1), CNO(1), INVER, BLC, TRC, 0,
     *            0, PCATI(1,1), BLC, 0, PLBUF, IRET)
               END IF
            LOCNUM = 1
            IF (IRET.GE.3) GO TO 950
            END IF
C                                       Draw contours.
         IF (DOCONT.GT.0.0) THEN
            CALL GLTYPE (2, PLBUF, IRET)
            IF (IRET.NE.0) GO TO 950
            XOFF = 1 + BLC(1,PC) - BLC(1,1)
            YOFF = 1 + BLC(2,PC) - BLC(2,1)
            WRITE (TXTMSG,1051) IPLANE
            CALL GCOMNT (2, TXTMSG, PLBUF, IRET)
            IF (IRET.NE.0) GO TO 960
            CALL KONDRW (MAXP, BBLC, BTRC, PLBUF, IRET)
            IF (IRET.GT.9) GO TO 960
            IF (IRET.NE.0) GO TO 950
            END IF
C                                       Draw polarization lines
         IF (DOVECT.GT.0.0) THEN
            CALL GLTYPE (3, PLBUF, IRET)
            IF (IRET.NE.0) GO TO 950
            WRITE (TXTMSG,1052) IPLANE
            CALL GCOMNT (2, TXTMSG, PLBUF, IRET)
            IF (IRET.NE.0) GO TO 960
            IF (PI.EQ.1) THEN
               LOCNUM = 3
            ELSE
               LOCNUM = 2
               CALL COPY (256, PCATI(1,1), SCRTCH)
               CALL COPY (256, PCATI(1,2), PCATI(1,1))
               IDEPTH(1) = BLC(3,2) + 0.01
               IDEPTH(2) = BLC(4,2) + 0.01
               IDEPTH(3) = BLC(5,2) + 0.01
               IDEPTH(4) = BLC(6,2) + 0.01
               IDEPTH(5) = BLC(7,2) + 0.01
               CALL SETLOC (IDEPTH, .TRUE.)
               CALL COPY (256, SCRTCH, PCATI(1,1))
               END IF
            CALL POLDRW (BBLC, BTRC, PLBUF, IRET)
            LOCNUM = 1
            IF (IRET.GT.9) GO TO 960
            IF (IRET.NE.0) GO TO 950
            END IF
C                                       holography overlay?
         IF (INFILE.NE.' ') THEN
            CALL GLTYPE (4, PLBUF, IRET)
            IF (IRET.NE.0) GO TO 950
            CALL LAYOUT (ABLC, ATRC, PCATR, INFILE, PLBUF, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'ERROR DRAWING HOLOGRAPHY LAYOUT'
               CALL MSGWRT (8)
               END IF
            END IF
 100     CONTINUE
C                                       rgblev display
      IF ((DO3C.EQ.2) .AND. (RGBLAB.LE.1) .AND. (DOCONT.GT.0.0)) THEN
         X = ABLC(1) + 0.04 * (ATRC(1) - ABLC(1))
         IF (RGBLAB.LE.0) THEN
            Y = ABLC(2) + 0.037 * (ATRC(2) - ABLC(2))
         ELSE
            Y = ATRC(2) - IYWDGE - 0.05 * (ATRC(2) - IYWDGE - ABLC(2))
            END IF
         TXTMSG = 'Draw LEVS values in RGB colors'
         CALL GCOMNT (2, TXTMSG, PLBUF, IRET)
         IF (IRET.NE.0) GO TO 960
         CALL TXRGBL (X, Y, ABLC, ATRC, LEVTXT, RGBLEV, PLBUF, IRET)
         END IF
C                                       Write sucessful finish message.
      CALL GFINIS (PLBUF, IRET)
      IF (IRET.NE.0) GO TO 960
         IRET = 0
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (IVOL(1), CNO(1), IVER, PLBUF, IRET)
            WRITE (MSGTXT,1300) IVER
            CALL MSGWRT (2)
            END IF
         GO TO 999
C-----------------------------------------------------------------------
C                                       Graph writing error.
 950  WRITE (MSGTXT,1950)
      CALL MSGWRT (8)
C                                       Try to do finish.
      CALL GFINIS (PLBUF, IRET)
      IF (IRET.NE.0) GO TO 960
         IRET = 0
         IF (.NOT.DOTV) CALL HIPLOT (IVOL(1), CNO(1), IVER, PLBUF,
     *      IRET)
         GO TO 999
C                                       Finish not sucessful. Destroy.
 960  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (IGLUN, IGFIND, IRET)
         CALL ZDESTR (IVOL, IGFILE, IRET)
         END IF
C                                       Do not save updated header.
 970  IRET = 16
      IF (.NOT.DOTV) CALL DELEXT ('PL', IVOL(1), CNO(1), 'READ',
     *   PCATI(1,1), PLBUF, IVER, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('GRAPH FILE INIT ERROR. GINIT ERR =',I5)
 1050 FORMAT ('Start grey-scale at plane',I5)
 1051 FORMAT ('Start contouring at plane',I5)
 1052 FORMAT ('Start vectors at plane',I5)
 1060 FORMAT (I8)
 1300 FORMAT ('Successful PLot file version',I5,'  created.')
 1950 FORMAT ('ERROR DURING GRAPHING WILL TRY TO FINISH PARTIAL GRAPH')
      END
      SUBROUTINE KOMLAB (LTYPE, ISRGBL, LEVTXT, PLBUF, IRET)
C-----------------------------------------------------------------------
C   KOMLAB is an axis drawing and labelling routine for use with the
C   common labeling for contour plots and pol vector plots.  It calls
C   GINITL and puts subsidiary labels (source, frequency, Stokes, image
C   name), (peak flux), (contour levels) in file.
C   Inputs:
C      LTYPE    I      label type: 1 none, 2 no ticks, 3 Ra/dec,
C                         4 center relative, 5 subimg center-rel,
C                         6 pixels, 7 as 3 no top labels
C                         < 0 => no date/time, else as positive
C      IVER     I      plot file version number
C   In/out:
C      PLBUF   I(256)   The updated graphics output buffer.
C   Output:
C      IRET     I        error indicator: 0 = No error.
C-----------------------------------------------------------------------
      INTEGER   LTYPE, PLBUF(256), IRET
      LOGICAL   ISRGBL
      CHARACTER LEVTXT*(*)
C
      CHARACTER SPRTXT(2)*80, ATIME*8, ADATE*12, CHTMP*8, NSTR*18,
     *   PREFIX*5, CHTEMP*20, LTEXT(5)*80, LABT(3)*6
      INTEGER   IDEPTH(5), I, IANGL, INCHAR, NTEXT, IT(3), ID(3), MCHAR,
     *   NL, IXL, IROUND, I2, I1, K, LABEL(3), J, ITRIM, LEVLEN
      REAL      DCX, DCY, RGAP, TEMP, Y0, Y1, X0, X1, BJUNK(2), VECTOR,
     *   RANGES(2,3)
      LOGICAL   SLICE, LFLAG
      INCLUDE 'KNTR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGPH.INC'
      DATA SLICE /.FALSE./
C-----------------------------------------------------------------------
C                                       Initial values.
      X0 = ABLC(1)
      X1 = ATRC(1)
      Y0 = ABLC(2)
      Y1 = ATRC(2)
      IDEPTH(1) = BLC(3,1) + .01
      IDEPTH(2) = BLC(4,1) + .01
      IDEPTH(3) = BLC(5,1) + .01
      IDEPTH(4) = BLC(6,1) + .01
      IDEPTH(5) = BLC(7,1) + .01
      LABEL(1) = 0
      LABEL(2) = 0
      LABEL(3) = 0
      LABT(1) = ' '
      LABT(2) = ' '
      LABT(3) = ' '
      K = 0
      IF (PC.GT.0) THEN
         LABEL(1) = PC
         LABT(1) = 'CONT: '
         K = 1
         END IF
      IF ((ABS(LTYPE).GT.1) .AND. (ABS(LTYPE).LT.7) .AND. (PG.GT.0.))
     *   THEN
         CH(2) = CH(2) + 1.333
         IF (PC.GT.0) THEN
            IF (PC.NE.PG) THEN
               CH(4) = CH(4) + 1.333
               LABEL(2) = PG
               LABT(2) = 'GREY: '
               K = 2
            ELSE
               LABT(1) = 'BOTH:'
               END IF
         ELSE
            K = 1
            LABT(1) = 'GREY: '
            LABEL(1) = PG
            END IF
         END IF
      IF ((ABS(LTYPE).GT.1) .AND. (ABS(LTYPE).LT.7) .AND. (PI.GT.0.))
     *   THEN
         CH(2) = CH(2) + 1.333
         IF (PAROT.NE.0.0) CH(2) = CH(2) + 1.333
         IF ((PI.NE.PG) .AND. (PI.NE.PC)) THEN
            CH(4) = CH(4) + 1.333
            K = K + 1
            LABEL(K) = PI
            LABT(K) = 'IPOL: '
         ELSE IF ((PI.EQ.PC) .AND. (PI.EQ.PG)) THEN
            LABT(1) = 'ALL:'
         ELSE IF (PI.EQ.PC) THEN
            LABT(1) = 'ICONT:'
         ELSE IF (PI.EQ.PG) THEN
            IF (PC.GT.0) THEN
               LABT(2) = 'IGREY:'
            ELSE
               LABT(1) = 'IGREY:'
               END IF
            END IF
         END IF
C                                       wedge labeling
      IF (ABS(LTYPE).GE.3) THEN
         IF (IXWDGE.GT.0) THEN
            CALL GTICNT (LTYPE, WRANGE, I)
            IF (I.GT.0) CH(3) = CH(3) + 0.5 + I
            END IF
         IF (IYWDGE.GT.0) CH(4) = CH(4) + 1.333
         END IF
      RGAP = YGAP
      CALL LABINI (BLC, TRC, IDEPTH, CH, LTYPE, SLICE, YGAP, SPRTXT,
     *   NTEXT)
C                                       Prepare LEVS lines
      NL = 0
      LEVTXT = ' '
      LEVLEN = 0
      IF ((ABS(LTYPE).LT.7) .AND. (PC.GT.0)) THEN
         NL = NL + 1
         WRITE (LTEXT(NL),1040) MULT
         INCHAR = 22
         WRITE (LEVTXT,1041) CLEV
         LEVLEN = 19
         DO 10 I = 1,30
            I2 = 11
            IXL = IROUND (LEVS(I))
            IF (ABS(IXL-LEVS(I)).GT.0.0001) THEN
               IF ((LEVS(I).GE.-99.90) .AND. (LEVS(I).LE.999.90))
     *            I2 = 13
               IF ((LEVS(I).GE.-9.990) .AND. (LEVS(I).LE.99.990))
     *            I2 = 14
               IF ((LEVS(I).GE.-0.9990) .AND. (LEVS(I).LE.9.9990))
     *            I2 = 15
               DCX = 10.0 ** (I2-12)
               IXL = IROUND (LEVS(I) * DCX)
               TEMP = IXL / DCX
            ELSE
               TEMP = IXL
               END IF
            WRITE (CHTEMP,1042,ERR=5) TEMP
 5          IF (CHTEMP(10:11).EQ. ' -') CHTEMP(10:11) = '-0'
            IF (CHTEMP(10:11).EQ. '  ') CHTEMP(10:11) = ' 0'
            CHTEMP(I2+1:) = ' '
            CALL CHTRIM (CHTEMP, 20, CHTEMP, IXL)
            LTEXT(NL)(INCHAR:) = CHTEMP(:IXL) // ', '
            INCHAR = INCHAR + 2 + IXL
            LEVTXT(LEVLEN+1:) = CHTEMP(:IXL)
            LEVLEN = LEVLEN + IXL + 2
C                                       Print out this line.
            IF (I.EQ.30) GO TO 15
            IF (LEVS(I+1).LE.LEVS(I)) GO TO 15
            IF (INCHAR.GE.50) THEN
               INCHAR = 1
               NL = NL + 1
               END IF
 10         CONTINUE
 15      INCHAR = INCHAR - 2
         LTEXT(NL)(INCHAR:INCHAR) = ')'
         LEVLEN = LEVLEN - 1
         LEVTXT(LEVLEN:LEVLEN) = ')'
         IF (ISRGBL) NL = 0
         CH(2) = CH(2) + (NL + 1) * 1.333
         END IF
C                                       Init for line drawing.
      CALL GINITL (ABLC, ATRC, XYRATO, CH, IDEPTH, PLBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) 'LINE DRAWING', IRET
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Init for grey scale.
      IF (PG.GT.0) THEN
         CALL H2CHR (2, 1, XFUN, GPHFUN)
         IF ((GPHFUN.NE.'LG') .AND. (GPHFUN.NE.'NG') .AND.
     *      (GPHFUN.NE.'SQ') .AND. (GPHFUN.NE.'NQ') .AND.
     *      (GPHFUN.NE.'NE') .AND. (GPHFUN.NE.'L2') .AND.
     *      (GPHFUN.NE.'N2')) GPHFUN = 'LN'
         CALL CHR2H (2, GPHFUN, 1, XFUN)
         IF ((DOCOLR) .OR.(FORCEC)) THEN
            RANGES(1,1) = RANGE(1)
            RANGES(2,1) = RANGE(2)
            RANGES(1,2) = RANGE(1)
            RANGES(2,2) = RANGE(2)
            RANGES(1,3) = RANGE(1)
            RANGES(2,3) = RANGE(2)
            CALL GINITC (ILPVAL, IHPVAL, RANGES, PLBUF, IRET)
         ELSE
            CALL GINITG (ILPVAL, IHPVAL, RANGE, PLBUF, IRET)
            END IF
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) 'GREY SCALE', IRET
            CALL MSGWRT (7)
            GO TO 999
            END IF
         END IF
      CALL GLTYPE (1, PLBUF, IRET)
      IF (IRET.NE.0) GO TO 980
C                                       Center-rel: true center pos
      IF ((NTEXT.GT.0) .AND. (ABS(LTYPE).LT.7) .AND. (ABS(LTYPE).GT.1))
     *   THEN
         DCX = 0.0
         IANGL = 0
         DO 25 I = 1,NTEXT
            CALL GPOS (X0, Y0, PLBUF, IRET)
            IF (IRET.NE.0) GO TO 980
            DCY = -YGAP
            CALL CHTRIM (SPRTXT(I), 80, SPRTXT(I), INCHAR)
            CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT(I), PLBUF,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
            YGAP = YGAP + 1.333
 25         CONTINUE
         END IF
      YGAP = YGAP + RGAP
C                                       Source name, stokes, freq.
      DCX = 0.0
      DCY = 0.5 - 1.333
      IF ((ABS(LTYPE).GE.3) .AND. (IYWDGE.GT.0)) DCY = DCY + 1.333
      IANGL = 0
      IF ((ABS(LTYPE).LT.7) .AND. (ABS(LTYPE).GT.1)) THEN
         DO 40 I = 1,K
            DCY = DCY + 1.333
            CALL GPOS (X0, Y1, PLBUF, IRET)
            IF (IRET.NE.0) GO TO 980
            SPRTXT(1) = LABT(I)
            J = LABEL(I)
            CALL H2CHR (8, 1, PCATH(KHOBJ,J), SPRTXT(1)(7:14))
            IF (J.NE.1) THEN
               CALL COPY (256, PCATI(1,1), SCRTCH)
               CALL COPY (256, PCATI(1,J), PCATI(1,1))
               LOCNUM = 2
               IDEPTH(1) = BLC(3,J) + .01
               IDEPTH(2) = BLC(4,J) + .01
               IDEPTH(3) = BLC(5,J) + .01
               IDEPTH(4) = BLC(6,J) + .01
               IDEPTH(5) = BLC(7,J) + .01
               CALL SETLOC (IDEPTH, .TRUE.)
               CALL COPY (256, SCRTCH, PCATI(1,1))
               END IF
            INCHAR = 17
            IF (NCHLAB(1,LOCNUM).GT.0) THEN
               SPRTXT(1)(INCHAR-1:INCHAR-1) = '_'
               SPRTXT(1)(INCHAR:) = SAXLAB(1,LOCNUM)(1:NCHLAB(1,LOCNUM))
               INCHAR = INCHAR + 3 + NCHLAB(1,LOCNUM)
               END IF
            IF (NCHLAB(2,LOCNUM).GT.0) THEN
               SPRTXT(1)(INCHAR-1:INCHAR-1) = '_'
               SPRTXT(1)(INCHAR:) = SAXLAB(2,LOCNUM)(1:NCHLAB(2,LOCNUM))
               INCHAR = INCHAR + 3 + NCHLAB(2,LOCNUM)
               END IF
            LOCNUM = 1
C                                       image name
            SPRTXT(1)(INCHAR-1:INCHAR-1) = '_'
            CALL H2CHR (12, KHIMNO, PCATH(KHIMN,J), NSTR(1:12))
            CALL H2CHR (6, KHIMCO, PCATH(KHIMC,J), NSTR(13:18))
            CALL NAMEST (NSTR, PCATI(KIIMS,J), SPRTXT(1)(INCHAR:),
     *         MCHAR)
            CALL REFRMT (SPRTXT(1), '_', INCHAR)
            CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT(1), PLBUF,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
 40         CONTINUE
C                                       Date/time version
         IF (LTYPE.GT.1) THEN
            DCY = DCY + 1.333
            CALL ZDATE (ID)
            CALL ZTIME (IT)
            CALL TIMDAT (IT, ID, ATIME, ADATE)
            WRITE (SPRTXT(1),1015) IVER, ADATE, ATIME
            CALL REFRMT (SPRTXT(1), '_', INCHAR)
            CALL GPOS (X0, Y1, PLBUF, IRET)
            IF (IRET.NE.0) GO TO 980
            CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT(1), PLBUF,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
            END IF
C                                       at bottom
         DCX = 0.0
         DCY = -YGAP
         IANGL = 0
C                                       Peak flux for grey scale
         IF (PG.GT.0) THEN
            CALL GPOS (X0, Y0, PLBUF, IRET)
            IF (IRET.NE.0) GO TO 980
            CALL H2CHR (8, 1, PCATH(KHBUN,PG), CHTMP)
            TEMP = RANGE(2) - RANGE(1)
            CALL METSCL (LTYPE, TEMP, PREFIX, LFLAG)
            IF (LFLAG) THEN
               INCHAR = 55
               WRITE (SPRTXT(1),1020) RANGE, CHTMP
            ELSE
               BJUNK(1) = TEMP * RANGE(1) / (RANGE(2) - RANGE(1))
               BJUNK(2) = TEMP * RANGE(2) / (RANGE(2) - RANGE(1))
               TEMP = MAX (ABS(BJUNK(1)), ABS(BJUNK(2)))
               IF (TEMP.LT.9.99) THEN
                  WRITE (SPRTXT(1),1021) BJUNK, PREFIX, CHTMP
                  I1 = 25
                  I2 = 33
               ELSE IF (TEMP.LT.99.9) THEN
                  WRITE (SPRTXT(1),1022) BJUNK, PREFIX, CHTMP
                  I1 = 26
                  I2 = 34
               ELSE IF (TEMP.LT.9999.) THEN
                  WRITE (SPRTXT(1),1023) BJUNK, PREFIX, CHTMP
                  I1 = 27
                  I2 = 35
               ELSE IF (TEMP.LT.9999999.) THEN
                  WRITE (SPRTXT(1),1024) BJUNK, PREFIX, CHTMP
                  I1 = 30
                  I2 = 40
               ELSE
                  WRITE (SPRTXT(1),1020) RANGE, CHTMP
                  I1 = -1
                  END IF
               IF (I1.GT.0) THEN
                  IF (SPRTXT(1)(I1:I1+1).EQ. ' -') SPRTXT(1)(I1:I1+1)
     *               = '-0'
                  IF (SPRTXT(1)(I1:I1+1).EQ. '  ') SPRTXT(1)(I1:I1+1)
     *               = ' 0'
                  IF (SPRTXT(1)(I2:I2+1).EQ. ' -') SPRTXT(1)(I2:I2+1)
     *               = '-0'
                  IF (SPRTXT(1)(I2:I2+1).EQ. '  ') SPRTXT(1)(I2:I2+1)
     *               = ' 0'
                  END IF
               END IF
            CALL REFRMT (SPRTXT(1), '_', INCHAR)
            CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT(1), PLBUF,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
            DCY = DCY - 1.333
            END IF
C                                       Peak flux.
         IF (PC.GT.0) THEN
            INCHAR = 37
            CALL GPOS (X0, Y0, PLBUF, IRET)
            IF (IRET.NE.0) GO TO 980
            CALL H2CHR (8, 1, PCATH(KHBUN,PC), CHTMP)
            TEMP = PCATR(KRDMX,PC)
            IF (ABS(PCATR(KRDMX,PC)).LT.ABS(PCATR(KRDMN,PC))) TEMP =
     *            PCATR(KRDMN,PC)
            WRITE (SPRTXT(1),1030) TEMP, CHTMP
            CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT(1), PLBUF,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
            DCY = DCY - 1.333
C                                       Write levels.
            IANGL = 0
            DO 50 I = 1,NL
               CALL GPOS (X0, Y0, PLBUF, IRET)
               IF (IRET.NE.0) GO TO 980
               CALL REFRMT (LTEXT(I), ' ', INCHAR)
               CALL GCHAR (INCHAR, IANGL, DCX, DCY, LTEXT(I), PLBUF,
     *            IRET)
               IF (IRET.NE.0) GO TO 980
               DCY = DCY - 1.333
 50            CONTINUE
            END IF
C                                       polarization
         IF (PI.GT.0) THEN
            CALL GPOS (X0, Y0, PLBUF, IRET)
            IF (IRET.NE.0) GO TO 980
            CALL H2CHR (8, 1, PCATH(KHBUN,PI), CHTMP)
            VECTOR = 0.5 / FACTOR
            WRITE (SPRTXT(1),1050) VECTOR, CHTMP
C                                       Arc seconds instead
            IF ((PCATR(KRCIC,PI).NE.0.0) .AND.
     *         (PCATR(KRCIC+1,PI).NE.0.0)) THEN
               TEMP = MAX (ABS(PCATR(KRCIC,PI)), ABS(PCATR(KRCIC+1,PI)))
     *            * 3600.0
               VECTOR = VECTOR / TEMP
               IF (TEMP.GT.0.01) THEN
                  WRITE (SPRTXT,1051) VECTOR, CHTMP
               ELSE
                  VECTOR = VECTOR / 1000.0
                  WRITE (SPRTXT,1052) VECTOR, CHTMP
                  END IF
               END IF
            INCHAR = ITRIM (SPRTXT(1))
            CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT(1), PLBUF,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
            DCY = DCY - 1.333
            IF (PAROT.NE.0.0) THEN
               CALL GPOS (X0, Y0, PLBUF, IRET)
               IF (IRET.NE.0) GO TO 980
               WRITE (SPRTXT(1),1053) PAROT
               INCHAR = ITRIM (SPRTXT(1))
               CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT(1), PLBUF,
     *            IRET)
               IF (IRET.NE.0) GO TO 980
               DCY = DCY - 1.333
               END IF
            END IF
         END IF
      IF (IRET.EQ.0) GO TO 999
C                                       Graph drawing error.
 980  WRITE (MSGTXT,1980) IRET
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GRAPH FILE INITIALIZATION FOR ',A,' ERR =',I5)
 1015 FORMAT ('PLot file version',I4,'__created ',2A)
 1020 FORMAT ('Grey scale flux range=',2(1PE12.4),1X,A8)
 1021 FORMAT ('Grey scale flux range=',2F8.3,1X,A5,A8)
 1022 FORMAT ('Grey scale flux range=',2F8.2,1X,A5,A8)
 1023 FORMAT ('Grey scale flux range=',2F8.1,1X,A5,A8)
 1024 FORMAT ('Grey scale flux range=',2F10.0,1X,A5,A8)
 1030 FORMAT ('Cont peak flux =',1PE12.4,1X,A8)
 1040 FORMAT ('Levs =',1PE11.3,' * (')
 1041 FORMAT ('LEVS=',1PE10.3,' * (')
 1042 FORMAT (F15.3)
 1050 FORMAT ('Pol line: 1 pixel =',1PE12.4,1X,A8)
 1051 FORMAT ('Pol line 1 arcsec =',1PE12.4,1X,A8)
 1052 FORMAT ('Pol line 1 milli arcsec =',1PE12.4,1X,A8)
 1053 FORMAT ('Pol. line rotated by',F7.1,' degrees')
 1980 FORMAT ('GRAPH LABEL WRITING ERROR. IRET =',I5)
      END
      SUBROUTINE GRDRW (ILABEL, LXWDGE, LYWDGE, PFLAG, BBLC, BTRC,
     *   PLBUF, IRET)
C-----------------------------------------------------------------------
C   Plot the grey scale plane
C   Inputs
C      ILABEL   I        Labeling type
C      LXWDGE   I        X wedge width this fram
C      LYWDGE   I        Y wedge width this fram
C      PFLAG    L        Plot the gray image
C      BBLC     I(2)     Beam plot corners to protect
C      BTRC     I(2)     Beam plot corners to protect
C   In/Out:
C      PLBUF   I(256)   Plot buffer
C   Output:
C      IRET     I        Error code
C-----------------------------------------------------------------------
      LOGICAL   PFLAG
      INTEGER   ILABEL, LXWDGE, LYWDGE, BBLC(2), BTRC(2), PLBUF(256),
     *   IRET
C
      INTEGER   I, LX, LY, IBLCX, ITRCX, IBLCY, ITRCY, BUFSZ, IROW,
     *   IPOS, IC(3), IANGL, JPOS, KPOS, JLUN, JIND, KLUN, KIND, J,
     *   BXOFF, BXSIZ
      REAL      X, Y, NATRC(2), NABLC(2), WBLC(2), WTRC(2), VMUL, TEMP,
     *   JBLC(7), JTRC(7), KBLC(7), KTRC(7)
      CHARACTER PHNAME*48
      LOGICAL   T
      INCLUDE 'KNTR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGPH.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      IANGL = 0
      JLUN = 41
      KLUN = 42
C                                       right-hand wedge
      IF (LXWDGE.GT.0) THEN
         NABLC(1) = NTRC(1)
         NABLC(2) = NBLC(2)
         NATRC(1) = NABLC(1) + LXWDGE
         NATRC(2) = NTRC(2)
         WBLC(1) = NABLC(1) + 1.0
         WBLC(2) = NABLC(2) + 1.0
         WTRC(1) = NATRC(1) - 1.0
         WTRC(2) = NATRC(2) - 1.0
         LX = NATRC(1) - NABLC(1) - 0.9
         LY = NATRC(2) - NABLC(2) - 0.9
         VMUL = (WRANGE(2)-WRANGE(1)) / (MAX (2,LY) - 1.0)
         X = NABLC(1) + 1.0
         Y = NABLC(2)
         CALL FILL (LX, GPHTLO, IBUFF)
         CALL FILL (LX, GPHTLO, IBBUFF)
         CALL FILL (LX, GPHTLO, IBLROW)
         DO 10 I = 1,LY
            Y = Y + 1.0
            CALL GPOS (X, Y, PLBUF, IRET)
            IF (IRET.NE.0) GO TO 999
            TEMP = (I - 1) * VMUL + WRANGE(1)
            CALL GSCALE (GPHFUN, RANGE, 1, 1, TEMP, IC(1))
            IF (DOOFM) CALL G3SCAL (1, IC(1), NOFM, ROFM, GOFM, BOFM,
     *         IC(1), IC(2), IC(3))
            IF (INCOLR.GT.0.0) THEN
               J = LX/3
               CALL FILL (J, IC(1), IBUFF(1))
               CALL FILL (J, IC(1), IBBUFF(1+J))
               CALL FILL (J, IC(1), IBLROW(1+J+J))
               CALL G3COLR (LX, IANGL, IBUFF, IBBUFF, IBLROW, PLBUF,
     *            IRET)
            ELSE IF (FORCEC) THEN
               CALL FILL (LX, IC(1), IBUFF)
               CALL FILL (LX, IC(1), IBBUFF)
               CALL FILL (LX, IC(1), IBLROW)
               CALL G3COLR (LX, IANGL, IBUFF, IBBUFF, IBLROW, PLBUF,
     *            IRET)
            ELSE IF (DOCOLR) THEN
               CALL FILL (LX, IC(1), IBUFF)
               CALL FILL (LX, IC(2), IBBUFF)
               CALL FILL (LX, IC(3), IBLROW)
               CALL G3COLR (LX, IANGL, IBUFF, IBBUFF, IBLROW, PLBUF,
     *            IRET)
            ELSE
               CALL FILL (LX, IC(1), IBUFF)
               CALL GRAYPX (LX, IANGL, IBUFF, PLBUF, IRET)
               END IF
            IF (IRET.NE.0) GO TO 999
 10         CONTINUE
         CALL GLTYPE (1, PLBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GPOS (NABLC(1), NABLC(2), PLBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (NATRC(1), NABLC(2), PLBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (NATRC(1), NATRC(2), PLBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (NABLC(1), NATRC(2), PLBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GTIC (ILABEL, WBLC, WTRC, NATRC, NTRC, WRANGE, PLBUF,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Top wedge
      IF (LYWDGE.GT.0) THEN
         NABLC(1) = NBLC(1)
         NABLC(2) = NTRC(2)
         NATRC(1) = NTRC(1)
         NATRC(2) = NABLC(2) + LYWDGE
         WBLC(1) = NABLC(1) + 1.0
         WBLC(2) = NABLC(2) + 1.0
         WTRC(1) = NATRC(1) - 1.0
         WTRC(2) = NATRC(2) - 1.0
         LX = NATRC(1) - NABLC(1) - 0.9
         LY = NATRC(2) - NABLC(2) - 0.9
         VMUL = (WRANGE(2) - WRANGE(1)) / (MAX (2,LX) - 1.0)
         X = NABLC(1) + 1.0
         Y = NABLC(2)
         DO 20 I = 1,LX
            RLROW(I) = (I - 1.) * VMUL + WRANGE(1)
 20         CONTINUE
         CALL GSCALE (GPHFUN, RANGE, LX, 1, RLROW, ILROW)
         IF (INCOLR.GT.0.0) THEN
            CALL FILL (LX, GPHTLO, IBBUFF)
            CALL FILL (LX, GPHTLO, IBLROW)
         ELSE IF (DOOFM) THEN
            CALL G3SCAL (LX, ILROW, NOFM, ROFM, GOFM, BOFM, ILROW,
     *         IBBUFF, IBLROW)
         ELSE IF (FORCEC) THEN
            CALL COPY (LX, ILROW, IBBUFF)
            CALL COPY (LX, ILROW, IBLROW)
            END IF
         DO 30 I = 1,LY
            Y = Y + 1.0
            CALL GPOS (X, Y, PLBUF, IRET)
            IF (IRET.NE.0) GO TO 999
            IF (INCOLR.GT.0.0) THEN
               IF (I.LE.LY/3) THEN
                  CALL G3COLR (LX, IANGL, ILROW, IBBUFF, IBLROW, PLBUF,
     *               IRET)
               ELSE IF (I.LE.(2*LY)/3) THEN
                  CALL G3COLR (LX, IANGL, IBBUFF, ILROW, IBLROW, PLBUF,
     *               IRET)
               ELSE
                  CALL G3COLR (LX, IANGL, IBBUFF, IBLROW, ILROW, PLBUF,
     *               IRET)
                  END IF
            ELSE IF ((DOCOLR) .OR.(FORCEC)) THEN
               CALL G3COLR (LX, IANGL, ILROW, IBBUFF, IBLROW, PLBUF,
     *            IRET)
            ELSE
               CALL GRAYPX (LX, IANGL, ILROW, PLBUF, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
 30         CONTINUE
         CALL GLTYPE (1, PLBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GPOS (NABLC(1), NABLC(2), PLBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (NABLC(1), NATRC(2), PLBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (NATRC(1), NATRC(2), PLBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (NATRC(1), NABLC(2), PLBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GTIC (ILABEL, WBLC, WTRC, NATRC, NTRC, WRANGE, PLBUF,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Plot the grey
      IF (PFLAG) THEN
C                                       open the grey image twice more
         IF (INCOLR.GT.0.0) THEN
            CALL ZPHFIL ('MA', IVOL(PG), CNO(PG), 1, PHNAME, IRET)
            CALL ZOPEN (JLUN, JIND, IVOL(PG), PHNAME, T, T, T, IRET)
            IF (IRET.EQ.0) CALL ZOPEN (KLUN, KIND, IVOL(PG), PHNAME, T,
     *         T, T, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT, 1020) 'OPEN EXTRA PLANES', IRET
               GO TO 990
               END IF
            CALL RCOPY (7, BLC(1,PG), JBLC)
            CALL RCOPY (7, BLC(1,PG), KBLC)
            CALL RCOPY (7, TRC(1,PG), JTRC)
            CALL RCOPY (7, TRC(1,PG), KTRC)
            BLC(3,PG) = (1.0D0 - PCATD(KDCRV+2,PG)) / PCATR(KRCIC+2,PG)
     *         + PCATR(KRCRP+2,PG)
            JBLC(3) = (2.0D0 - PCATD(KDCRV+2,PG)) / PCATR(KRCIC+2,PG)
     *         + PCATR(KRCRP+2,PG)
            KBLC(3) = (3.0D0 - PCATD(KDCRV+2,PG)) / PCATR(KRCIC+2,PG)
     *         + PCATR(KRCRP+2,PG)
            TRC(3,PG) = BLC(3,PG)
            JTRC(3) = JBLC(3)
            KTRC(3) = KBLC(3)
            END IF
         IBLCX  = BLC(1,PG) + 0.5
         IBLCY  = BLC(2,PG) + 0.5
         ITRCX  = TRC(1,PG) + 0.5
         ITRCY  = TRC(2,PG) + 0.5
         XOFF = BLC(1,PG) - BLC(1,1)
         YOFF = BLC(2,PG) - BLC(2,1)
         BXOFF = BBLC(1) - IBLCX
         BXSIZ = BTRC(1) - BBLC(1) + 1
C                                       Determine the number of panels,
C                                       and set up the looping.
         LX = ITRCX - IBLCX + 1
         LY = ITRCY - IBLCY + 1
C                                       Initialize the map file for
C                                       double-buffered IO.
         BUFSZ = MABFSS * 2
         CALL DBINIT (MLUN(PG), MIND(PG), PCATI(1,PG), BLC(1,PG),
     *      TRC(1,PG), BUFSZ, BUFF, IRET)
         IF ((IRET.EQ.0) .AND. (INCOLR.GT.0.0)) THEN
            CALL DBINIT (JLUN, JIND, PCATI(1,PG), JBLC, JTRC, BUFSZ,
     *         BBUFF, IRET)
            IF (IRET.EQ.0) CALL DBINIT (KLUN, KIND, PCATI(1,PG), KBLC,
     *         KTRC, BUFSZ, RLROW, IRET)
            END IF
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) 'INIT', IRET
            GO TO 980
            END IF
         DO 50 IROW = IBLCY,ITRCY
C                                       Read the next row.
            CALL MDISK ('READ', MLUN(PG), MIND(PG), BUFF, IPOS, IRET)
            IF ((IRET.EQ.0) .AND. (INCOLR.GT.0.0)) THEN
               CALL MDISK ('READ', JLUN, JIND, BBUFF, JPOS, IRET)
               IF (IRET.EQ.0) CALL MDISK ('READ', KLUN, KIND, RLROW,
     *            KPOS, IRET)
               END IF
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1020) 'READ', IRET
               GO TO 980
               END IF
            IF ((IROW.GE.BBLC(2)) .AND. (IROW.LE.BTRC(2))) THEN
               CALL RFILL (BXSIZ, FBLANK, BUFF(IPOS+BXOFF))
               IF (INCOLR.GT.0.0) THEN
                  CALL RFILL (BXSIZ, FBLANK, BBUFF(JPOS+BXOFF))
                  CALL RFILL (BXSIZ, FBLANK, RLROW(KPOS+BXOFF))
                  END IF
               END IF
            X = IBLCX - XOFF
            Y = IROW  - YOFF
            CALL GPOS (X, Y, PLBUF, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL GSCALE (GPHFUN, RANGE, LX, 1, BUFF(IPOS), IBUFF(IPOS))
            IF (INCOLR.GT.0.0) THEN
               CALL GSCALE (GPHFUN, RANGE, LX, 1, BBUFF(JPOS),
     *            IBBUFF(JPOS))
               CALL GSCALE (GPHFUN, RANGE, LX, 1, RLROW(KPOS),
     *            ILROW(KPOS))
            ELSE IF (DOOFM) THEN
               JPOS = IPOS
               KPOS = IPOS
               CALL G3SCAL (LX, IBUFF(IPOS), NOFM, ROFM, GOFM, BOFM,
     *            IBUFF(IPOS), IBBUFF(JPOS), ILROW(KPOS))
            ELSE IF (FORCEC) THEN
               JPOS = IPOS
               KPOS = IPOS
               CALL RCOPY (LX, IBUFF(IPOS), IBBUFF(JPOS))
               CALL RCOPY (LX, IBUFF(IPOS), ILROW(KPOS))
               END IF
            IF ((DOCOLR) .OR. (FORCEC)) THEN
               CALL G3COLR (LX, 0, IBUFF(IPOS), IBBUFF(JPOS),
     *            ILROW(KPOS), PLBUF, IRET)
            ELSE
               CALL GRAYPX (LX, 0, IBUFF(IPOS), PLBUF, IRET)
               END IF
            IF (IRET.NE.0) GO TO 999
 50         CONTINUE
         END IF
      GO TO 985
C
 980  CALL MSGWRT (7)
 985  IF (INCOLR.GT.0.0) THEN
         CALL ZCLOSE (JLUN, JIND, I)
         CALL ZCLOSE (KLUN, KIND, I)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('ERROR ON GREY-SCALE ',A,' IRET =',I4)
      END
      SUBROUTINE POLDRW (BBLC, BTRC, IGBLK, IRET)
C-----------------------------------------------------------------------
C   POLDRW will read the I, P, and A maps and add the polarization
C   vectors to the graph file.
C   In/out:
C      IGBLK   I(256)   Plot buffer
C   Output:
C      IRET    I        the error code. 0 = ok.
C-----------------------------------------------------------------------
      INTEGER   BBLC(2), BTRC(2), IGBLK(*), IRET
C
      REAL      CON, P, Q, X, X0, Y, Y0, IICUT, Z1, Z2, Z3, PLEN, PA,
     *   TEMP, COL(3), FACT
      INTEGER   I, IPOS(4), IM, INCOLS, INROWS, IXINC, IYINC, K, J,
     *   BCUT, BVAL, PX, LCORN, BXOFF, BXSIZ, IROUND, IROW
      DOUBLE PRECISION RRA, DDE, DZ
      LOGICAL   DOBACK, DOFIL(4)
      DOUBLE PRECISION PIE, DG2RAD
      PARAMETER (PIE = 3.14159265358979323846D0)
      PARAMETER (DG2RAD = PIE / 180.0D0)
      INCLUDE 'KNTR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGPH.INC'
C-----------------------------------------------------------------------
      DOBACK = (PG.GT.0) .AND. (DODARK.GT.0.0)
      IF (DOBACK) BCUT = GPHCUT * GPHTHI + (1.0-GPHCUT) * GPHTLO
      PX = 3 - PI
      IF ((DOBACK) .AND. (PG.NE.PI)) PX = PG
      DOFIL(1) = .FALSE.
      DOFIL(2) = .FALSE.
      DOFIL(3) = .TRUE.
      DOFIL(4) = .TRUE.
      IF (DOBACK) DOFIL(PG) = .TRUE.
      DOFIL(PI) = .TRUE.
      INROWS = TRC(2,1) - BLC(2,1) + 1.5
      INCOLS = TRC(1,1) - BLC(1,1) + 1.5
      BXOFF = BBLC(1) - IROUND (BLC(1,1))
      BXSIZ = BTRC(1) - BBLC(1) + 1
      IXINC = XINC + .5
      IYINC = YINC + .5
      IXINC = MAX (1, IXINC)
      IYINC = MAX (1, IYINC)
      IF (FACTOR.LE.0.0) FACTOR = 1.0
      CON = 3.14159 / 180.0
      IF (AXTYP(LOCNUM).NE.1) THEN
         MSGTXT = 'AXES NOT COORDINATE PAIR: ANGLES MEANINGLESS'
         CALL MSGWRT (8)
         IRET = 2
         GO TO 999
         END IF
C                                       Pixel I value value cut off.
      IICUT = ICUT
C                                       Scaling in degrees
      FACT = FACTOR * MAX (ABS(PCATR(KRCIC,PI)),
     *   ABS(PCATR(KRCIC+1,PI)))
C                                       Init for I/O
      DO 20 J = 1,4
         IF (DOFIL(J)) THEN
            CALL DBINIT (MLUN(J), MIND(J), PCATI(1,J), BLC(1,J),
     *         TRC(1,J), JBUFSZ, RBLK(1,J), IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 20      CONTINUE
C                                       Loop for all rows.
      DO 100 I = 1,INROWS
         IROW = I - 1 + IROUND (BLC(2,1))
         DO 25 J = 1,4
            IF (DOFIL(J)) THEN
               CALL MDISK ('READ', MLUN(J), MIND(J), RBLK(1,J),
     *            IPOS(J), IRET)
               IF (IRET.NE.0) GO TO 999
               IF ((IROW.GE.BBLC(2)) .AND. (IROW.LE.BTRC(2)))
     *            CALL RFILL (BXSIZ, FBLANK, RBLK(IPOS(J)+BXOFF,J))
               END IF
 25         CONTINUE
C                                        Do we do this line.
         IF ((MOD(I,IYINC).EQ.1) .OR. (IYINC.EQ.1)) THEN
            IF (DOBACK) CALL GSCALE (GPHFUN, GPHRNG, INCOLS, 1,
     *         RBLK(IPOS(PG),PG), IBLK(IPOS(PX),PX))
C                                        Yes. Do for all pixels.
            IM = -1
            DO 90 K = 1,INCOLS,IXINC
               IM = K - 1
C                                       Check for blanked pixels.
               Z1 = RBLK(IPOS(PI)+IM,PI)
               Z2 = RBLK(IPOS(3)+IM,3)
               Z3 = RBLK(IPOS(4)+IM,4)
               IF (DOBACK) BVAL = IBLK(IPOS(PX)+IM,PX)
               IF (Z1.EQ.FBLANK) GO TO 90
               IF (Z2.EQ.FBLANK) GO TO 90
               IF (Z3.EQ.FBLANK) GO TO 90
C                                       Check user supplied cut off.
               IF (DOCONV) THEN
                  P = SQRT (Z2*Z2 + Z3*Z3)
                  PA = 28.64789 * ATAN2 (Z3, Z2+1.0E-20) + PAROT
               ELSE
                  P = Z2
                  PA = Z3 + PAROT
                  END IF
C                                       Use these pixels.
               IF ((Z1.GE.IICUT) .AND. (P.GE.PCUT)) THEN
                  X0 = BLC(1,1) + K - 1
                  Y0 = BLC(2,1) + I - 1
                  Q = CON * PA
                  IF (CORTYP(LOCNUM).EQ.1) THEN
                     CALL XYVAL (X0, Y0, RRA, DDE, DZ, IRET)
                  ELSE
                     CALL XYVAL (X0, Y0, DDE, RRA, DZ, IRET)
                     END IF
                  IF (IRET.NE.0) GO TO 980
                  PLEN = FACT * P
                  RRA = RRA + PLEN * SIN (Q) / COS (DDE * CON)
                  DDE = DDE + PLEN * COS (Q)
                  IF (CORTYP(LOCNUM).EQ.1) THEN
                     CALL XYPIX (RRA, DDE, X, Y, IRET)
                  ELSE
                     CALL XYPIX (DDE, RRA, X, Y, IRET)
                     END IF
                  IF (IRET.NE.0) GO TO 980
                  CALL GPOS (X, Y, IGBLK, IRET)
                  IF (IRET.NE.0) GO TO 990
                  X = 2 * X0 - X
                  Y = 2 * Y0  - Y
                  IF ((DOBACK) .AND. (BVAL.GT.BCUT)) THEN
                     CALL GDVEC (X, Y, IGBLK, IRET)
                  ELSE IF (POL3C.GT.0.0) THEN
                     TEMP = PA - POL3C
                     TEMP = MOD (TEMP+3600.0, 180.0)
                     TEMP = TEMP / 180.0
                     CALL COLOR3 (TEMP, .TRUE., COL)
                     CALL G3VCOL (COL(1), COL(2), COL(3), IGBLK, IRET)
                     IF (IRET.NE.0) GO TO 999
                     CALL G3VEC (X, Y, IGBLK, IRET)
                  ELSE
                     CALL GVEC (X, Y, IGBLK, IRET)
                     END IF
                  IF (IRET.NE.0) GO TO 990
                  END IF
 90            CONTINUE
            END IF
 100     CONTINUE
C                                       POL color scale
      IF (POL3C.GT.0.0) THEN
C        K = MAX (1, INROWS / 20)
C        Y0 = BLC(2,1) + INROWS - K
C        Y = Y0 - K
C        K = MAX (1, INCOLS / 20)
C        X = BLC(1,1) + K
C        PA = 0.0
C        Z2 = 180.0 / (INCOLS - 2*K)
C        DO 120 I = 1,INCOLS-2*K+1
C           CALL GPOS (X, Y0, IGBLK, IRET)
C           IF (IRET.NE.0) GO TO 999
C           TEMP = PA - POL3C
C           TEMP = MOD (TEMP+3600.0, 180.0)
C           TEMP = TEMP / 180.0
C           CALL COLOR3 (TEMP, .TRUE., COLS)
C           CALL G3VCOL (COLS(1), COLS(2), COLS(3), IGBLK, IRET)
C           IF (IRET.NE.0) GO TO 999
C           CALL G3VEC (X, Y, IGBLK, IRET)
C           IF (IRET.NE.0) GO TO 999
C           X = X + 1.0
C           PA = PA + Z2
C120        CONTINUE
C                                       try a color wheel
         LCORN = MOD (BMCORN, 5)
         K = MAX (1, INROWS / 15)
         J = MAX (1, INCOLS / 15)
         P = K*K + J*J
         P = SQRT (P)
         IF (LCORN.LE.1) THEN
            Y0 = BLC(2,1) + INROWS - 3*K
            X0 = BLC(1,1) + INCOLS - 2*J
         ELSE IF (LCORN.EQ.2) THEN
            Y0 = BLC(2,1) + INROWS - 3*K
            X0 = BLC(1,1) + 2*J
         ELSE IF (LCORN.EQ.3) THEN
            P = -P
            Y0 = BLC(2,1) + 3*K
            X0 = BLC(1,1) + 2*J
         ELSE IF (LCORN.EQ.4) THEN
            P = -P
            Y0 = BLC(2,1) + 3*K
            X0 = BLC(1,1) + INCOLS - 2*J
            END IF
         DO 140 I = 1,180,2
            PA = (I - 90) * DG2RAD
            TEMP = I - 90 - POL3C
            TEMP = MOD (TEMP+3600.0, 180.0)
            TEMP = TEMP / 180.0
            CALL COLOR3 (TEMP, .TRUE., COL)
            CALL G3VCOL (COL(1), COL(2), COL(3), IGBLK, IRET)
            IF (IRET.NE.0) GO TO 990
            X = X0 - P * SIN (PA) / 3.0
            Y = Y0 + P * COS (PA) / 3.0
            CALL GPOS (X, Y, IGBLK, IRET)
            IF (IRET.NE.0) GO TO 990
            X = X0 - P * SIN (PA)
            Y = Y0 + P * COS (PA)
            CALL G3VEC (X, Y, IGBLK, IRET)
            IF (IRET.NE.0) GO TO 990
 140        CONTINUE
         END IF
      GO TO 999
C                                       coordinate error
 980  WRITE (MSGTXT,1980) IRET
      CALL MSGWRT (6)
      GO TO 999
C                                       plot error
 990  WRITE (MSGTXT,1990) IRET
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('POLDRW: COORDINATE ERROR',I3,' FACTOR TOO LARGE??')
 1990 FORMAT ('POLDRW: PLOT ROUTINE ERROR',I3,' QUITTING')
      END
      SUBROUTINE KONDRW (MAXPIX, BBLC, BTRC, IGBLK, IRET)
C-----------------------------------------------------------------------
C   KONDRW writes commands to a plot file for the execution of a
C   contour plot.
C   Inputs
C   Given and returned:
C      IGBLK(256)  I    I/O block for graph file.
C   Returned:
C      IRET        I    Error code. 0 = ok.
C                             9 => QUIT op received from TELL
C                            10 => ABOR op received from TELL
C   Commons:
C      MULT     R         Scale factor for contour levels.
C      CATBLK   I(256)    Map header.
C   Given via common CNTRBU:
C      IBUFF(MABFSS) I    Integer map IO buffer.
C      RLROW(MABFSS) R    Real map IO buffer.
C   Algorithm: Reads the image and breaks it up into blocks for KONTUR.
C   Author: Mark Calabretta, Australia Telescope.
C-----------------------------------------------------------------------
      INTEGER   MAXPIX, BBLC(2), BTRC(2), IGBLK(256), IRET
C
      LOGICAL   PERCNT
      INTEGER   FRINGE, I, IBLCX, IBLCX0, IBLCY, IBLCY0, IDASH(30), ILX,
     *   ILY, INPIXS, IPOS, IROW, ITRCX, ITRCY, LEV, BUFSZ, LOOPX,
     *   LOOPY, LX, LY, MX, NCNTR, NDX, NY, CONBLK(256), JPOS,
     *   JBLCX, JBLCY, JTRCX, JTRCY, JBLCX0, JBLCY0, BXOFF, BXSIZ,
     *   LBBLC(2), LBTRC(2)
      REAL      CONTRS(30), DSHL, DX, DY, PIX(129,129), BPIX(129,129),
     *   PIXMAX, SBLC(7), STRC(7), X0, Y0, INDE, CBLC(7), CTRC(7), TFACT
      INCLUDE 'KNTR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGPH.INC'
C-----------------------------------------------------------------------
C                                       Copy IGBLK into IGBUFF to be
C                                       passed to PLOT.
      DO 10 I = 1,256
         IGBUFF(I) = IGBLK(I)
 10      CONTINUE
C                                       Pixel blanking values.
      INDE = FBLANK
C                                       Parameters for KONTUR.
      PERCNT = .FALSE.
      FRINGE = 1
      IF (DOBLNK.LE.0.0) FRINGE = -1
      DX = 1.0
      DY = 1.0
C                                       Copy BLC and TRC: background
      IF ((DOGREY.GT.0.0) .AND. (DODARK.GT.0.0) .AND. (DO3C.LE.0)) THEN
         TFACT = 1.0 / (GPHCUT * (GPHTHI - GPHTLO))
         DO 20 I = 1,7
            CBLC(I) = MIN (BLC(I,PG), TRC(I,PG))
            CTRC(I) = MAX (BLC(I,PG), TRC(I,PG))
 20         CONTINUE
         CALL RCOPY (5, CBLC(3), CTRC(3))
         JBLCX  = CBLC(1) + 0.5
         JBLCY  = CBLC(2) + 0.5
         JTRCX  = CTRC(1) + 0.5
         JTRCY  = CTRC(2) + 0.5
         END IF
C                                       Copy BLC and TRC.
      DO 25 I = 1,7
         SBLC(I) = MIN (BLC(I,PC), TRC(I,PC))
         STRC(I) = MAX (BLC(I,PC), TRC(I,PC))
         PIXMAX = MAX (ABS (PCATR(KRDMX,PC)), ABS (PCATR(KRDMN,PC)))
         CALL COPY (256, PCATI(1,PC), CONBLK)
 25      CONTINUE
      IBLCX  = SBLC(1) + 0.5
      IBLCY  = SBLC(2) + 0.5
      ITRCX  = STRC(1) + 0.5
      ITRCY  = STRC(2) + 0.5
      INPIXS = ITRCX - IBLCX + 1
      LBBLC(1) = 0
      LBBLC(2) = 0
      LBTRC(1) = 0
      LBTRC(2) = 0
C                                       Determine the number of panels,
C                                       and set up the looping.
      LX = ITRCX - IBLCX + 1
      LY = ITRCY - IBLCY + 1
      LOOPX = LX/128
      LOOPY = LY/128
      IF (MOD(LX,128).NE.0) LOOPX = LOOPX + 1
      IF (MOD(LY,128).NE.0) LOOPY = LOOPY + 1
C                                       Determine the number of levels
C                                       and convert to absolute values.
      DO 30 LEV = 1,30
         CONTRS(LEV) = MULT*LEVS(LEV)
C                                       Define contour dashing.
         DSHL = 0.005*LX
         IF (CONTRS(LEV).LT.0.0) THEN
            IDASH(LEV) = 3
            IF (MAXPIX.GT.500) IDASH(LEV) = 10
            IF (MAXPIX.GT.1100) IDASH(LEV) = 1
         ELSE IF (CONTRS(LEV).EQ.0.0) THEN
            IDASH(LEV) = 2
         ELSE
            IDASH(LEV) = 0
            END IF
         IF (LEV.EQ.30) GO TO 40
         IF (LEVS(LEV).GE.LEVS(LEV+1)) GO TO 40
 30      CONTINUE
C                                       Loop over the panels.
 40   NCNTR = LEV
      IBLCY0 = IBLCY
      JBLCY0 = JBLCY
      DO 100 ILY = 1,LOOPY
         IBLCX0 = IBLCX
         JBLCX0 = JBLCX
         NY = MIN(128, (ITRCY-IBLCY0+1))
         DO 90 ILX = 1,LOOPX
            MX = MIN (128,(ITRCX-IBLCX0+1))
C                                       Reset the window.
            SBLC(1) = IBLCX0
            SBLC(2) = IBLCY0
            STRC(1) = IBLCX0 + MX - 1
            STRC(2) = IBLCY0 + NY - 1
            IF (BBLC(1).GT.0) THEN
               LBBLC(1) = BBLC(1) - SBLC(1) + 1
               LBBLC(2) = BBLC(2) - SBLC(2) + 1
               LBTRC(1) = BTRC(1) - SBLC(1) + 1
               LBTRC(2) = BTRC(2) - SBLC(2) + 1
               IF ((LBTRC(1).GE.1) .AND. (LBBLC(1).LE.MX) .AND.
     *            (LBTRC(2).GE.1) .AND. (LBBLC(2).LE.NY)) THEN
                  LBBLC(1) = MAX (1, LBBLC(1))
                  LBBLC(2) = MAX (1, LBBLC(2))
                  LBTRC(1) = MIN (MX, LBTRC(1))
                  LBTRC(2) = MIN (NY, LBTRC(2))
                  BXOFF = LBBLC(1) - 1
                  BXSIZ = LBTRC(1) - LBBLC(1) + 1
               ELSE
                  LBBLC(1) = 0
                  LBBLC(2) = 0
                  LBTRC(1) = 0
                  LBTRC(2) = 0
                  END IF
               END IF
C                                       Initialize the map file for
C                                       double-buffered IO.
            BUFSZ = MABFSS * 2
            CALL DBINIT (MLUN(PC), MIND(PC), CONBLK, SBLC, STRC, BUFSZ,
     *         BUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1050) 'INIT CONTOUR', IRET
               CALL MSGWRT (7)
               GO TO 999
               END IF
C                                       Background
            IF ((PG.GT.0) .AND. (DODARK.GT.0.0) .AND. (DO3C.LE.0) .AND.
     *         (MLUN(PG).NE.MLUN(PC))) THEN
               CBLC(1) = JBLCX0
               CBLC(2) = JBLCY0
               CTRC(1) = JBLCX0 + MX - 1
               CTRC(2) = JBLCY0 + NY - 1
               CALL DBINIT (MLUN(PG), MIND(PG), PCATI(1,PG), CBLC, CTRC,
     *            BUFSZ, BBUFF, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1050) 'INIT BACKGROUND', IRET
                  CALL MSGWRT (7)
                  GO TO 999
                  END IF
               END IF
            DO 80 IROW = 1,NY
C                                       Read the next row.
               CALL MDISK ('READ', MLUN(PC), MIND(PC), BUFF, IPOS, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1060) 'READ CONTOUR', IRET
                  CALL MSGWRT (7)
                  GO TO 999
                  END IF
               IF ((IROW.GE.LBBLC(2)) .AND. (IROW.LE.LBTRC(2)))
     *            CALL RFILL (BXSIZ, FBLANK, BUFF(IPOS+BXOFF))
C                                       Copy the data into the panel
C                                       array.
               DO 50 I = 1,MX
                  NDX = IPOS + I - 1
                  PIX(I,IROW) = BUFF(NDX)
 50               CONTINUE
C                                       Read the next row.
               IF ((DOGREY.GT.0.0) .AND. (DODARK.GT.0.0)
     *            .AND. (DO3C.LE.0)) THEN
                  IF (MLUN(PG).EQ.MLUN(PC)) THEN
                     JPOS = IPOS
                     CALL GSCALE (GPHFUN, GPHRNG, MX, 1, BUFF(JPOS),
     *                  IBBUFF(JPOS))
                  ELSE
                     CALL MDISK ('READ', MLUN(PG), MIND(PG), BBUFF,
     *                  JPOS, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1060) 'READ BACKGROUND', IRET
                        CALL MSGWRT (7)
                        GO TO 999
                        END IF
                     CALL GSCALE (GPHFUN, GPHRNG, MX, 1, BBUFF(JPOS),
     *                  IBBUFF(JPOS))
                     END IF
C                                       Copy the data into the panel
C                                       array.
                  DO 60 I = 1,MX
                     NDX = JPOS + I - 1
                     BPIX(I,IROW) = (IBBUFF(NDX) - GPHTLO) * TFACT
 60                  CONTINUE
               ELSE
                  DO 70 I = 1,MX
                     BPIX(I,IROW) = 0.0
 70                  CONTINUE
                  END IF
 80            CONTINUE
C                                       Contour this panel.
            X0 = IBLCX0
            Y0 = IBLCY0
            CALL KONTUR (PIX, BPIX, MX, NY, INDE, PIXMAX, PERCNT,
     *         CONTRS, NCNTR, DSHL, IDASH, FRINGE, X0, Y0, DX, DY, DO3C,
     *         RGBLEV, NEDCMT, TXTMSG, IRET)
            IF (IRET.GT.1) GO TO 999
            IBLCX0 = IBLCX0 + MX - 1
            JBLCX0 = JBLCX0 + MX - 1
 90         CONTINUE
         IBLCY0 = IBLCY0 + NY - 1
         JBLCY0 = JBLCY0 + NY - 1
 100     CONTINUE
C                                       Copy IGBUFF into IGBLK to be
C                                       returned by KONDRW.
      DO 110 I = 1,256
         IGBLK(I) = IGBUFF(I)
 110     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('KONDRW: ',A,' Error',I3,' returned from DBINIT.')
 1060 FORMAT ('KONDRW: ',A,' Error',I3,' returned from MDISK.')
      END
      SUBROUTINE KONTUR (PIX, BPIX, MX, MY, INDE, PIXMAX, PERCNT,
     *   CONTRS, NCNTR, DSHL, IDASH, FRINGE, X0, Y0, DX, DY, DO3C,
     *   RGBLEV, NEDCMT, TXTMSG, IRET)
C-----------------------------------------------------------------------
C   KONTUR contours a 129 x 129 array.
C   Given:
C      PIX      R(129,129)   Array containing the image pixels.
C      BPIX     R(129,129)   Array containing the bckground flag (0,1)
C      MX       I            Number of pixels in PIX in X
C      MY       I            Number of pixels in PIX in Y
C      INDE     R            Pixel blanking value.
C      PIXMAX   R            Maximum pixel value, Jansky.
C      PERCNT   L            If true, contour levels are percentages.
C      NCNTR    I            Number of contours to be plotted.
C      CONTRS   R(*)         The contour levels.
C      DSHL     R            Unit of dash length.
C      IDASH    I(*)         Contour dash codes.
C      FRINGE   I            Pen for plotting blanked areas.
C      X0,Y0    R            Physical coordinates of pixel (0,0)
C      DX,DY    R            Physical separation between pixels.
C      DO3C     I            1 => color contours same full panel
C                            2 => color contour change w level
C      RGBLEV   R(3,30)      RGB levels for contours
C   Returned:
C      IRET     I            Error status,
C                               0: success,
C                               1: too many points to plot,
C                               2: aborted by user.
C   Algorithm:
C      Acts as a driver for KNSCAN-TRACE-CALC-PLOTPT.
C
C   Notes:
C      Derived from the KONTR subroutine of task KONTR by Arnold Rots.
C   Author:
C      Mark Calabretta, Australia Telescope.
C      Origin; 1988/Jul/05  Code last modified; 1988/Oct/26
C-----------------------------------------------------------------------
      LOGICAL   PERCNT, NEDCMT
      INTEGER   MX, MY, NCNTR, IDASH(*), FRINGE, DO3C, IRET
      REAL      PIX(129,129), BPIX(129,129), INDE, PIXMAX, CONTRS(*),
     *   DSHL, X0, Y0, DX, DY, RGBLEV(3,*)
      CHARACTER TXTMSG*80
C
      INTEGER   IC, ID, IP, IPEN(15), IDTYP(15,0:12), LENP
      REAL      CONTR, COLORS(3)
      INCLUDE 'INCS:DMSG.INC'
C                                       Define dash patterns.
      DATA IDTYP /3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     *            3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     *            3, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     *            3, 3, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     *            3, 3, 4, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     *            3, 3, 3, 3, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     *            3, 3, 3, 3, 4, 3, 3, 4, 0, 0, 0, 0, 0, 0, 0,
     *            3, 3, 3, 3, 4, 4, 3, 3, 4, 4, 0, 0, 0, 0, 0,
     *            3, 3, 3, 3, 4, 3, 3, 4, 3, 3, 4, 0, 0, 0, 0,
     *            3, 3, 3, 3, 4, 4, 3, 3, 4, 4, 3, 3, 4, 4, 0,
     *            3, 3, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     *            3, 3, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     *            3, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
C-----------------------------------------------------------------------
C                                       Plot the boundary of the blanked
C                                       area.
      IF (FRINGE.GT.0) THEN
C                                       Solid contour.
         IPEN(1) = 3
         IF (DO3C.GT.0) IPEN(1) = 3 - DO3C
         LENP = 1
         CALL RFILL (6, 1.0, COLORS)
         CALL KNSCAN (PIX, BPIX, MX, MY, INDE, X0, Y0, DX, DY, INDE,
     *      DSHL, IPEN, LENP, COLORS, IRET)
         END IF
C                                       Loop over the contour levels.
      DO 50 IC = 1,NCNTR
C                                       Absolute contour level.
         IF (PERCNT) THEN
            CONTR = CONTRS(IC) * PIXMAX * 0.01
         ELSE
            CONTR = CONTRS(IC)
            END IF
         COLORS(1) = RGBLEV(1,IC)
         COLORS(2) = RGBLEV(2,IC)
         COLORS(3) = RGBLEV(3,IC)
         WRITE (TXTMSG,1000) CONTR
         NEDCMT = .TRUE.
C                                       Load the dash pattern.
         ID = IDASH(IC)
         DO 30 IP = 1,15
            IPEN(IP) = IDTYP(IP,ID)
            IF (IPEN(IP).NE.0) LENP = IP
            IF ((DO3C.GT.0) .AND. (IPEN(IP).EQ.3)) IPEN(IP) = 3 - DO3C
 30         CONTINUE
C                                       Contour label.
         CALL KNSCAN (PIX, BPIX, MX, MY, INDE, X0, Y0, DX, DY, CONTR,
     *      DSHL, IPEN, LENP, COLORS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1040) CONTR
            CALL MSGWRT (6)
            END IF
 50      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Begin contour',1PE13.5)
 1040 FORMAT ('KONTUR: Contour',1PE11.3,' terminated: too many points')
      END
      SUBROUTINE KNSCAN (PIX, BPIX, MX, MY, INDE, X0, Y0, DX, DY, CONTR,
     *   DSHL, IPEN, LENP, COLORS, IRET)
C-----------------------------------------------------------------------
C   KNSCAN searches for contours.
C   Given:
C      PIX(129,129) R     Array containing the image pixels.
C      BPIX(129,129) R     Array containing the background pixels.
C      MX,MY        I     Number of pixels in PIX in the X and Y
C                             directions.
C      INDE         R     Pixel blanking value.
C      X0,Y0        R     Physical coordinates of pixel (0,0)
C      DX,DY        R     Physical separation between pixels.
C      CONTR        R     Value of the contour being plotted.
C      DSHL         R     Unit of dash length.
C      IPEN(*)      I     Sequence of pen up/down codes for manual
C                             production of dashed lines.
C      LENP         I     Maximum index value into IPEN.
C      COLORS       R(6)  Colors used for model IPEN 1
C   Returned:
C      IRET         I     Error status,
C                                0: success,
C                                1: too many points to plot.
C   Algorithm:
C      Firstly scans the boundary of the image looking for open
C      contours.   These begin and end on the boundary.  It then scans
C      the inside for 'closed' contours.  Blanked pixels are treated as
C      pixels of infinite value.  They cause a pen up, unless
C      CONTR=INDE, in which case the boundary of the blanked pixels is
C      to be plotted
C   Notes:
C   1) Derived from the SCAN subroutine of task KONTR by Lew Taff,
C      via Arnold Rots.  This version is an extension of the
C      original algorithm in that it handles blanked pixels.
C   Author:
C      Mark Calabretta, Australia Telescope.
C      Origin; 1987/May/15  Code last modified; 1988/Jul/06
C-----------------------------------------------------------------------
      REAL      PIX(129,129), BPIX(129,129), INDE, X0, Y0, DX, DY,
     *   CONTR, DSHL, COLORS(3)
      INTEGER   MX, MY, IPEN(*), LENP, IRET
C
      INTEGER   I, IDX, IDY, IX, IY, J, MDX, MEM(3000), MEMTST, NDX
      REAL      V1, V2
C-----------------------------------------------------------------------
C                                       Initialize pointers.
      IRET = 0
      MDX  = 0
C                                       Search for all open contours.
C                                       Scan along the lower frame from
C                                       left to right.
      DO 10 I = 2,MX
         IX = I
         IY = 1
         V1 = PIX(IX,IY)
         V2 = PIX(IX-1,IY)
C                                       Test for a contour.
         IF (V1.EQ.INDE .OR. (V1.GE.CONTR .AND. CONTR.NE.INDE)) THEN
            IF (V2.NE.INDE .AND. (V2.LT.CONTR .OR. CONTR.EQ.INDE)) THEN
C                                       Contour it.
               IDX = -1
               IDY =  0
               CALL TRACE (PIX, BPIX, MX, MY, INDE, X0, Y0, DX, DY,
     *            CONTR, DSHL, IPEN, LENP, IX, IY, IDX, IDY, MDX, MEM,
     *            COLORS, IRET)
               IF (IRET.NE.0) RETURN
               END IF
            END IF
 10      CONTINUE
C                                       Scan along the right frame from
C                                       bottom to top.
      DO 20 J = 2,MY
         IX = MX
         IY = J
         V1 = PIX(IX,IY)
         V2 = PIX(IX,IY-1)
C                                       Test for a contour.
         IF (V1.EQ.INDE .OR. (V1.GE.CONTR .AND. CONTR.NE.INDE)) THEN
            IF (V2.NE.INDE .AND. (V2.LT.CONTR .OR. CONTR.EQ.INDE)) THEN
C                                       Contour it.
               IDX =  0
               IDY = -1
               CALL TRACE (PIX, BPIX, MX, MY, INDE, X0, Y0, DX, DY,
     *            CONTR, DSHL, IPEN, LENP, IX, IY, IDX, IDY, MDX, MEM,
     *            COLORS, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
            END IF
 20      CONTINUE
C                                       Scan along the upper frame from
C                                       right to left.
      DO 30 I = MX-1,1,-1
         IX = I
         IY = MY
         V1 = PIX(IX,IY)
         V2 = PIX(IX+1,IY)
C                                       Test for a contour.
         IF (V1.EQ.INDE .OR. (V1.GE.CONTR .AND. CONTR.NE.INDE)) THEN
            IF (V2.NE.INDE .AND. (V2.LT.CONTR .OR. CONTR.EQ.INDE)) THEN
C                                       Contour it.
               IDX = +1
               IDY =  0
               CALL TRACE (PIX, BPIX, MX, MY, INDE, X0, Y0, DX, DY,
     *            CONTR, DSHL, IPEN, LENP, IX, IY, IDX, IDY, MDX, MEM,
     *            COLORS, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
            END IF
 30      CONTINUE
C                                       Scan along the left frame from
C                                       top to bottom.
      DO 40 J = MY-1,1,-1
         IX = 1
         IY = J
         V1 = PIX(IX,IY)
         V2 = PIX(IX,IY+1)
C                                       Test for a contour.
         IF (V1.EQ.INDE .OR. (V1.GE.CONTR .AND. CONTR.NE.INDE)) THEN
            IF (V2.NE.INDE .AND. (V2.LT.CONTR .OR. CONTR.EQ.INDE)) THEN
C                                       Contour it.
               IDX =  0
               IDY = +1
               CALL TRACE (PIX, BPIX, MX, MY, INDE, X0, Y0, DX, DY,
     *            CONTR, DSHL, IPEN, LENP, IX, IY, IDX, IDY, MDX, MEM,
     *            COLORS, IRET)
               IF (IRET.NE.0) GO TO 999
            END IF
         END IF
 40   CONTINUE
C                                       Contour the inner portions of
C                                       the map.
C                                       Loop over the rows from bottom
C                                       to top.
      DO 70 J = 2,MY-1
C                                       Loop over columns from left to
C                                       right.
         DO 60 I = 2, MX
            IX = I
            IY = J
            V1 = PIX(IX,IY)
            V2 = PIX(IX-1,IY)
C                                       Test for a contour.
            IF (V1.EQ.INDE .OR. (V1.GE.CONTR .AND. CONTR.NE.INDE)) THEN
               IF (V2.NE.INDE .AND. (V2.LT.CONTR .OR.
     *          CONTR.EQ.INDE)) THEN
C                                       Has it already been done?
                  MEMTST = IX*200 + IY
                  DO 50 NDX = 1, MDX
                     IF (MEM(NDX).EQ.MEMTST) GO TO 60
 50                  CONTINUE
C                                       Contour it.
                  IDX = -1
                  IDY =  0
                  CALL TRACE (PIX, BPIX, MX, MY, INDE, X0, Y0, DX, DY,
     *               CONTR, DSHL, IPEN, LENP, IX, IY, IDX, IDY, MDX,
     *               MEM, COLORS, IRET)
                  IF (IRET.NE.0) GO TO 999
                  END IF
               END IF
 60         CONTINUE
 70      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE TRACE (PIX, BPIX, MX, MY, INDE, X0, Y0, DX, DY, CONTR,
     *   DSHL, IPEN, LENP, IX, IY, IDX, IDY, MDX, MEM, COLORS, IRET)
C-----------------------------------------------------------------------
C   TRACE follows a contour.
C   Given:
C      PIX     R(129,129)     Array containing the image pixels.
C      BPIX    R(129,129)     Array containing the background pixels.
C          MX,MY        I     Number of pixels in PIX in the X and Y
C                             directions.
C          INDE         R     Pixel blanking value.
C          X0,Y0        R     Physical coordinates of pixel (0,0)
C          DX,DY        R     Physical separation between pixels.
C          CONTR        R     Value of the contour being plotted.
C          DSHL         R     Unit of dash length.
C          IPEN(*)      I     Sequence of pen up/down codes for manual
C                             production of dashed lines.
C          LENP         I     Maximum index value into IPEN.
C     Given and returned:
C          IX,IY        I     Pixel coordinates of the reference pixel.
C          IDX,IDY      I     Pixel increments in X and Y.
C          MDX          I     Index into MEM.  Set to zero by KNSCAN at
C                             the start, thereafter updated by TRACE.
C
C     Returned:
C          MEM(3000)    I     Contour searching consists of finding
C                             pixels which are greater than or equal to
C                             CONTR, but whose neighbour to the left is
C                             less than CONTR.  These pixels are
C                             recorded in MEM by subroutine TRACE to
C                             indicate that the contour associated with
C                             them has been done.
C          IRET         I     Error status, 0: success,
C                                1: too many points to plot.
C
C     Called:
C          KNTR:   {CALC, PLOTPT}
C
C     Algorithm:
C          The basics:  The reference pixel, P0, is chosen to be greater
C          than or equal to CONTR.  Test pixels are any of the eight
C          pixels P1-P8 which surround P0.  These are examined in a
C          certain order to decide which is to become the new reference
C          pixel.  Further information is included with the code.
C
C     Notes:
C       1) Derived from the TRACE subroutine of task KONTR by Lew Taff,
C          via Arnold Rots.  This version is an extension of the
C          original algorithm in that it handles blanked pixels.
C
C       2) IS, the pointer to IDX and IDY, indicates the direction of
C          the test pixel (IX2,IY2) from the reference pixel (IX,IY) as
C          shown in the following diagram.
C                             +1   2  3  4
C                         IDY  0   1  0  5
C                             -1   8  7  6
C                                 -1  0 +1
C                                    IDX
C          The reference pixel is at IDX=0, IDY=0, that is, IS=0.  The
C          test pixels are at IS=1,2,3,4,5,6,7,8.  Incrementing IS is
C          always done in modulo 8.  An increment of 1 switches to the
C          adjacent pixel in the clockwise direction.  An increment of 4
C          corresponds to a reversal in direction.
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1987/May/15  Code last modified; 1988/Jul/08
C-----------------------------------------------------------------------
      INTEGER   MX, MY, IPEN(*), LENP, IX, IY, IDX, IDY, MDX, MEM(3000),
     *          IRET
      REAL      PIX(129,129), BPIX(129,129), INDE, X0, Y0, DX, DY,
     *   CONTR, DSHL, COLORS(3)
C
      LOGICAL   NEAR, FAR, FAR0, FLUSH, PENUP, PREVAL
      INTEGER   I, IB, INX(8), INY(8), IP, IPT(3,3), IS, IS0, IX0, IX2,
     *   IY0, IY2, MEMCNT, MEMTST
      REAL      ARCLEN, TEMP, X, XBUF(64), Y, YBUF(64), Z, ZBUF(64), DCP
      DATA IPT / 8,  7, 6, 1, 0,  5,  2,  3, 4/
      DATA INX /-1, -1, 0, 1, 1,  1,  0, -1/
      DATA INY / 0,  1, 1, 1, 0, -1, -1, -1/
C-----------------------------------------------------------------------
C                                       Initialize.
      IRET   = 0
      NEAR   = .FALSE.
      FAR0   = .FALSE.
      PENUP  = .TRUE.
      ARCLEN = 0.0
      IB     = 0
      IP     = 1
      FLUSH  = .FALSE.
C                                       The initial test pixel is that
C                                       specified by IDX and IDY.
      IS = IPT(IDX+2,IDY+2)
C                                       Record the starting pixel and
C                                       direction.
      IX0 = IX
      IY0 = IY
      IS0 = IS
C                                       Initialize the contour.
      CALL CALC (PIX, BPIX, INDE, X0, Y0, DX, DY, CONTR, IX, IY, IDX,
     *   IDY, NEAR, X, Y, Z, FAR)
      CALL PLOTPT (INDE, DSHL, IPEN, LENP, X, Y, Z, FLUSH, PENUP, XBUF,
     *   YBUF, ZBUF, IB, IP, ARCLEN, COLORS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Trace out the path of the
C                                       contour.
 10   CONTINUE
C                                       Cycle to a new test pixel.
         IS  = MOD (IS, 8) + 1
         IDX = INX(IS)
         IDY = INY(IS)
         IX2 = IX + IDX
         IY2 = IY + IDY
C                                       Check for closure of the
C                                       contour.
         IF (IX.EQ.IX0 .AND. IY.EQ.IY0 .AND. IS.EQ.IS0) THEN
            CALL CALC (PIX, BPIX, INDE, X0, Y0, DX, DY, CONTR, IX, IY,
     *         IDX, IDY, NEAR, X, Y, Z, FAR)
            CALL PLOTPT (INDE, DSHL, IPEN, LENP, X, Y, Z, FLUSH, PENUP,
     *         XBUF, YBUF, ZBUF, IB, IP, ARCLEN, COLORS, IRET)
            IF (IRET.NE.0) GO TO 999
            GO TO 50
            END IF
C                                       Have we hit a boundary?
         IF (IX2.LT.1 .OR. IX2.GT.MX) GO TO 40
         IF (IY2.LT.1 .OR. IY2.GT.MY) GO TO 40
C                                       Will this reference pixel
C                                       prevail.
         PREVAL = .FALSE.
         IF (CONTR.EQ.INDE) THEN
            IF (PIX(IX2,IY2).NE.INDE) PREVAL = .TRUE.
         ELSE IF (PIX(IX2,IY2).LT.CONTR .AND. PIX(IX2,IY2).NE.INDE) THEN
            PREVAL = .TRUE.
         ELSE IF (IDX.NE.0 .AND. IDY.NE.0) THEN
C                                       Test for a saddle point.
            IF (PIX(IX,IY).NE.INDE .AND. PIX(IX2,IY).NE.INDE .AND.
     *         PIX(IX,IY2).NE.INDE .AND. PIX(IX2,IY2).NE.INDE) THEN
C                                       DCP is taken as the saddle point
C                                       height.
               DCP = (PIX(IX,IY) + PIX(IX2,IY) + PIX(IX,IY2) +
     *            PIX(IX2,IY2))/4
C                                       The contour crosses the saddle
C                                       if DCP is less than CONTR.
               IF (DCP.LT.CONTR) PREVAL = .TRUE.
               END IF
            END IF
         IF (PREVAL) THEN
C                                       Continue with this reference
C                                       pixel.
            CALL CALC (PIX, BPIX, INDE, X0, Y0, DX, DY, CONTR, IX, IY,
     *         IDX, IDY, NEAR, X, Y, Z, FAR)
            IF (FAR .AND. FAR0) THEN
C                                       This and the previous point were
C                                       on the far side of a saddle
C                                       point.  Consequently, they must
C                                       be swapped.
               TEMP     = XBUF(IB)
               XBUF(IB) = X
               X        = TEMP
               TEMP     = YBUF(IB)
               YBUF(IB) = Y
               Y        = TEMP
               TEMP     = ZBUF(IB)
               ZBUF(IB) = Z
               Z        = TEMP
               END IF
            FAR0 = FAR
            CALL PLOTPT (INDE, DSHL, IPEN, LENP, X, Y, Z, FLUSH, PENUP,
     *         XBUF, YBUF, ZBUF, IB, IP, ARCLEN, COLORS, IRET)
            IF (IRET.NE.0) GO TO 999
         ELSE
C                                       We have a new reference pixel.
            IF (IDX.NE.0 .AND. IDY.NE.0) THEN
C                                       Temporarily reset the reference
C                                       pixel to be the previous test
C                                       pixel and force the contour to
C                                       pass between it and the saddle
C                                       point.
               NEAR = .TRUE.
               IF (INX(IS-1).NE.0) THEN
                  IX  = IX + IDX
                  IDX = -IDX
                  CALL CALC (PIX, BPIX, INDE, X0, Y0, DX, DY, CONTR, IX,
     *               IY, IDX, IDY, NEAR, X, Y, Z, FAR)
                  CALL PLOTPT (INDE, DSHL, IPEN, LENP, X, Y, Z, FLUSH,
     *               PENUP, XBUF, YBUF, ZBUF, IB, IP, ARCLEN, COLORS,
     *               IRET)
                  IF (IRET.NE.0) GO TO 999
                  IX  = IX + IDX
               ELSE
                  IY  = IY + IDY
                  IDY = -IDY
                  CALL CALC (PIX, BPIX, INDE, X0, Y0, DX, DY, CONTR, IX,
     *               IY, IDX, IDY, NEAR, X, Y, Z, FAR)
                  CALL PLOTPT (INDE, DSHL, IPEN, LENP, X, Y, Z, FLUSH,
     *               PENUP, XBUF, YBUF, ZBUF, IB, IP, ARCLEN, COLORS,
     *               IRET)
                  IF (IRET.NE.0) GO TO 999
                  IY  = IY + IDY
                  END IF
               END IF
            IF (PIX(IX-1,IY).NE.INDE .AND.
     *       (PIX(IX-1,IY).LT.CONTR .OR. CONTR.EQ.INDE)) THEN
C                                       This reference pixel must be
C                                       recorded.
               IF (MDX.GE.3000) THEN
C                                       We've run out of storage!
                  IRET = 1
                  GO TO 50
               END IF
C                                       One pixel may be independently
C                                       recorded as many as five times
C                                       if it is surrounded by four
C                                       saddle points.
               MEMTST = 200*IX + IY
               MEMCNT = -4
               DO 30 I = 1,MDX
                  IF (MEM(I).EQ.MEMTST) MEMCNT = MEMCNT + 1
 30               CONTINUE
               IF (MEMCNT.GT.0) GO TO 50
C                                       Record the reference pixel.
               MDX = MDX + 1
               MEM(MDX) = MEMTST
               END IF
C                                       Interchange the reference and
C                                       test pixels.
            IS = IS + 4
            IX = IX2
            IY = IY2
            END IF
      GO TO 10
40    IF (PIX(IX-1,IY).NE.INDE .AND.
     *   (PIX(IX-1,IY).LT.CONTR .OR. CONTR.EQ.INDE)) THEN
C                                       Record this reference pixel.
         MDX = MDX + 1
         MEM(MDX) = 200*IX + IY
         END IF
C                                       Close the contour.
50    FLUSH = .TRUE.
      CALL PLOTPT (INDE, DSHL, IPEN, LENP, X, Y, Z, FLUSH, PENUP, XBUF,
     *   YBUF, ZBUF, IB, IP, ARCLEN, COLORS, IRET)
C
 999  RETURN
      END
      SUBROUTINE CALC (PIX, BPIX, INDE, X0, Y0, DX, DY, CONTR, IX, IY,
     *   IDX, IDY, NEAR, X, Y, Z, FAR)
C-----------------------------------------------------------------------
C   CALC determines the path of a contour between two pixels.
C   Given:
C      PIX    R(129,129)     Array containing the image pixels.
C      BPIX   R(129,129)     Array containing the image pixels.
C          INDE         R     Pixel blanking value.
C          X0,Y0        R     Physical coordinates of pixel (0,0)
C          DX,DY        R     Physical separation between pixels.
C          CONTR        R     Value of the contour being plotted.
C          IX,IY        I     Coordinates of the reference pixel.
C          IDX,IDY      I     Pixel increments in X and Y.
C
C     Given and returned:
C          NEAR         L     A flag which will force the contour to
C                             pass between the reference pixel and the
C                             saddle point.
C
C   Outputs:
C      X,Y          R     Physical coordinates of the point on the
C                             contour.
C      Z            R     Background value (0 -> bright, 1 -> dark)
C      FAR          L     A flag which indicates that the contour
C                             has passed on the far side of a saddle
C                             point from the reference pixel.
C
C     Called:
C          none
C
C     Algorithm:
C          a) Linear interpolation is used for pixel pairs which are
C             offset vertically or horizontally.
C          b) Diagonally offset pixels are tested for a possible saddle
C             point.  The position of the candidate saddle point is
C             taken to be at the midpoint of the diagonal, and its
C             height as the mean value of the four surrounding pixels.
C             The contour is allowed to cross the diagonal only if the
C             contour level exceeds this height.
C
C     Notes:
C       1) Derived from the CALC subroutine of task KONTR by Lew Taff,
C          via Arnold Rots.  This version is an extension of the
C          original algorithm in that it handles blanked pixels
C          properly.
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1987/May/15  Code last modified; 1988/Jul/06
C-----------------------------------------------------------------------
      LOGICAL   NEAR, FAR
      INTEGER   IX, IY, IDX, IDY
      REAL      PIX(129,129), BPIX(129,129), INDE, X0, Y0, DX, DY,
     *   CONTR, X, Y, Z
C
      REAL      V, P1, P2, P3, P4, DCP, B1, B2, B3, B4
C-----------------------------------------------------------------------
      FAR = .FALSE.
      P1 = PIX(IX,IY)
      P2 = PIX(IX+IDX,IY)
      P3 = PIX(IX+IDX,IY+IDY)
      P4 = PIX(IX,IY+IDY)
      B1 = BPIX(IX,IY)
      B2 = BPIX(IX+IDX,IY)
      B3 = BPIX(IX+IDX,IY+IDY)
      B4 = BPIX(IX,IY+IDY)
      Z = 0.0
C                                       A vertical step from P1 to P4.
      IF (IDX.EQ.0) THEN
         X = IX
         IF (P1.EQ.INDE .OR. P4.EQ.INDE) THEN
            Y = IY + IDY/2.0
            IF (CONTR.NE.INDE) X = INDE
C                                       Linear interpolation between P1
C                                       and P4.
         ELSE
            IF (P1.NE.P4) THEN
               Y = IY + ((P1-CONTR)/(P1-P4))*IDY
               Z = (P1-CONTR)/(P1-P4)*(B4-B1) + B1
            ELSE
               Y = IY
               Z = B1
               END IF
            END IF
C                                       A horizontal step from P1 to P2.
      ELSE IF (IDY.EQ.0) THEN
         Y = IY
         IF (P1.EQ.INDE .OR. P2.EQ.INDE) THEN
            X = IX + IDX/2.0
            IF (CONTR.NE.INDE) X = INDE
C                                       Linear interpolation between P1
C                                       and P2.
         ELSE
            IF (P1.NE.P2) THEN
               X = IX + (CONTR-P1) / (P2-P1) * IDX
               Z = (CONTR-P1) / (P2-P1) * (B2-B1) + B1
            ELSE
               X = IX
               Z = B1
               END IF
            END IF
C                                       Stepped diagonally.
      ELSE
         IF ((P1.EQ.INDE) .OR. (P2.EQ.INDE) .OR. (P3.EQ.INDE) .OR.
     *      (P4.EQ.INDE)) THEN
            IF (P1.EQ.INDE) FAR = .TRUE.
            X = IX + IDX/2.0
            Y = IY + IDY/2.0
            IF (CONTR.NE.INDE) X = INDE
C                                       Value at the central point
C                                       between P1, P2, P3, and P4.
         ELSE
            DCP = (P1 + P2 + P3 + P4)/4.0
C                                       Send the contour between P1 and
C                                       the central point.
            IF (NEAR .OR. DCP.LE.CONTR) THEN
               V = 0.5
               IF (DCP.NE.P1) V = 0.5*(P1-CONTR)/(P1-DCP)
               IF ((V.LT.0.0) .OR. (V.GT.0.5)) THEN
                  V = 0.5
                  IF (DCP.NE.P3) V = 1.0 - 0.5*(P3-CONTR)/(P3-DCP)
                  IF ((V.LT.0.5) .OR. (V.GT. 1.0)) V = 0.5
                  END IF
C                                       Send the contour between the
C                                       central point and P3.
            ELSE
               V = 0.5
               IF (DCP.NE.P3) V = 1.0 - 0.5*(P3-CONTR)/(P3-DCP)
               IF ((V.LT.0.5) .OR. (V.GT. 1.0)) THEN
                  V = 0.5
                  IF (DCP.NE.P1) V = 0.5*(P1-CONTR)/(P1-DCP)
                  IF ((V.LT.0.0) .OR. (V.GT.0.5)) V = 0.5
                  END IF
               END IF
            FAR = (1.0.GE.V) .AND. (V.GT.0.5)
            X = IX + V*IDX
            Y = IY + V*IDY
            Z = V * (B3 - B1) + B1
            NEAR = .FALSE.
            END IF
         END IF
C                                       Convert from pixel coordinates
C                                       to linear coordinates.
      IF (X.NE.INDE) THEN
         X = X0 + X*DX
         Y = Y0 + Y*DY
         END IF
C
 999  RETURN
      END
      SUBROUTINE PLOTPT (INDE, DSHL, IPEN, LENP, X, Y, Z, FLUSH, PENUP,
     *   XBUF, YBUF, ZBUF, IB, IP, ARCLEN, COLORS, IRET)
C-----------------------------------------------------------------------
C   PLOTPT does plotting for KNSCAN-TRACE-CALC.
C   Given:
C      INDE         R     Pixel blanking value.
C      DSHL         R     Unit of dash length.
C      IPEN(*)      I     Sequence of pen up/down codes for manual
C                         production of dashed lines.
C      LENP         I     Maximum index value into IPEN.
C      X,Y          R     Coordinates of the point to be plotted.
C      FLUSH        L     Force flushing of the plot buffer.
C   Given and returned:
C      PENUP        L     If true, start with pen up when the buffer
C                         is flushed.
C      XBUF(64)     R     Plot coordinate buffer.
C      YBUF(64)     R
C      IB           I     Pointer into the plot buffer.
C      IP           I     Index into IPEN.
C      ARCLEN       R     Length of the contour plotted so far.
C   Output:
C      IRET         I     Error code
C   Algorithm:
C      The (X,Y) values are stored in a buffer (XBUF,YBUF) which is
C      used for dashing and contour labelling.  Even when the buffer
C      is flushed the current (X,Y) coordinates are not used for
C      plotting.  This delay scheme gives TRACE one chance to change
C      the previous point before pen has hit paper.
C   Notes:
C      1) Derived from the PLOTPT subroutine of task KONTR by Lew Taff,
C         via Arnold Rots.
C      2) Subroutine arguments for contour labels have been set up but
C         not implemented.
C   Author: Mark Calabretta, Australia Telescope.
C-----------------------------------------------------------------------
      LOGICAL   FLUSH, PENUP
      INTEGER   IPEN(*), LENP, IB, IP, IRET
      REAL      INDE, DSHL, X, Y, Z, XBUF(*), YBUF(*), ZBUF(*),
     *   ARCLEN, COLORS(3)
C
      INTEGER   I, J, K, KK
      REAL      DL, DS, DX, DY, FX, FY, XD, YD
C-----------------------------------------------------------------------
      IF (IB.EQ.0) THEN
C                                       Do nothing special.
      ELSE IF (IB.EQ.1) THEN
C                                       Ignore false starts.
         IF (XBUF(1).EQ.INDE) THEN
            IB = 0
            PENUP = .TRUE.
            END IF
      ELSE IF (FLUSH .OR. IB.EQ.64 .OR. XBUF(IB).EQ.INDE) THEN
C                                       Flush the buffer.
         IF (PENUP) THEN
C                                       Start (or restart) the contour.
            CALL PLOT (XBUF(1), YBUF(1), 4, COLORS, IRET)
            IF (IRET.NE.0) GO TO 999
            IP = 1
            PENUP = .FALSE.
            END IF
C                                       Follow the contour.
         DO 20 I = 2,IB
C                                       Break for blanked pixels.
            IF (XBUF(I).EQ.INDE) GO TO 30
C                                       Compute dash parameters.
            DX = XBUF(I) - XBUF(I-1)
            DY = YBUF(I) - YBUF(I-1)
            DS = SQRT (DX*DX + DY*DY)
C                                       Ignore movements of less than 1
C                                       micron.
            IF (DS.GE.0.0001) THEN
               FX = DX/DS
               FY = DY/DS
               DL = DSHL - MOD (ARCLEN, DSHL)
C                                       Step through the dash pattern.
               DO 10 J = 1,1000
                  K = IPEN(IP)
                  IF ((DL.GT.DS) .OR. (LENP.EQ.1)) THEN
                     KK = 1
                     IF ((ZBUF(I).GT.1.0) .AND. (K.EQ.3)) K = 5
                     CALL PLOT (XBUF(I), YBUF(I), K, COLORS, IRET)
                     IF (IRET.NE.0) GO TO 999
                     ARCLEN = ARCLEN + DS
                     IF (LENP.EQ.1) IP = 1
                     GO TO 20
                  ELSE
                     KK = 1
                     IF (K.EQ.3) THEN
                        XD = ZBUF(I-1) + DL * ZBUF(I)
                        IF (XD.GT.1.0) K = 5
                        END IF
                     XD = XBUF(I-1) + FX*DL
                     YD = YBUF(I-1) + FY*DL
                     CALL PLOT (XD, YD, K, COLORS, IRET)
                     IF (IRET.NE.0) GO TO 999
                     IP = IP + 1
                     IF (IP.GT.LENP) IP = 1
                     END IF
                  DL = DL + DSHL
 10               CONTINUE
               END IF
 20         CONTINUE
C                                       Reset the buffer.
 30      IF (XBUF(IB).EQ.INDE) THEN
C                                       Blanked pixel, flag pen up.
            PENUP = .TRUE.
            IB = 0
         ELSE
            XBUF(1) = XBUF(IB)
            YBUF(1) = YBUF(IB)
            ZBUF(1) = ZBUF(IB)
            IB = 1
            END IF
         END IF
      IB = IB + 1
      XBUF(IB) = X
      YBUF(IB) = Y
      ZBUF(IB) = Z
C
 999  RETURN
      END
      SUBROUTINE PLOT (X, Y, IMODE, COLOR, IRET)
C-----------------------------------------------------------------------
C   PLOT calls GPOS or GVEC as required.  Adds contour level comment
C   if this is the first call at this level.
C   Given:
C      X,Y        R      Plot coordinates.
C      IMODE      I      Operation code.
C      COLOR      R(3)   Used in mode 1 (color contour by level)
C-----------------------------------------------------------------------
      INTEGER   IMODE, IRET
      REAL      X, Y, COLOR(3)
C
      REAL      XX, YY
      INCLUDE 'KNTR.INC'
C-----------------------------------------------------------------------
      IF (NEDCMT) THEN
         CALL GCOMNT (-1, TXTMSG, IGBUFF, IRET)
         NEDCMT = .FALSE.
         END IF
      XX = X - XOFF
      YY = Y - YOFF
      IF (IMODE.EQ.3) THEN
         CALL GVEC (XX, YY, IGBUFF, IRET)
      ELSE IF (IMODE.EQ.4) THEN
         CALL GPOS (XX, YY, IGBUFF, IRET)
      ELSE IF (IMODE.EQ.5) THEN
         CALL GDVEC (XX, YY, IGBUFF, IRET)
      ELSE IF (IMODE.EQ.2) THEN
         CALL G3VEC (XX, YY, IGBUFF, IRET)
      ELSE IF (IMODE.EQ.1) THEN
         CALL G3VCOL (COLOR(1), COLOR(2), COLOR(3), IGBUFF, IRET)
         IF (IRET.EQ.0) CALL G3VEC (XX, YY, IGBUFF, IRET)
         END IF
C
 999  RETURN
      END
      SUBROUTINE ALIGN (IRET)
C-----------------------------------------------------------------------
C   ALIGN checks the alignment of the images
C   Output:
C      IRET   I   Error code: o okay, else die
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   NXX, I, J, IROUND, IPOINT, IP, LP
      REAL      X
      LOGICAL   REDUCE
      CHARACTER CHTMP*8, CHTMP1*8
      DOUBLE PRECISION DX
      INCLUDE 'KNTR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       basic window setting
      CALL RCOPY (7, XBLC, BLC(1,1))
      CALL RCOPY (7, XTRC, TRC(1,1))
      CALL WINDOW (PCATI(KIDIM,1), PCATI(KINAX,1), BLC(1,1), TRC(1,1),
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      CALL RCOPY (7, BLC(1,1), BLC(1,2))
      CALL RCOPY (7, BLC(1,1), BLC(1,3))
      CALL RCOPY (7, BLC(1,1), BLC(1,4))
      CALL RCOPY (7, TRC(1,1), TRC(1,2))
      CALL RCOPY (7, TRC(1,1), TRC(1,3))
      CALL RCOPY (7, TRC(1,1), TRC(1,4))
C                                       something to check
      IF (((PC.NE.PG) .AND. (PC.GT.0) .AND. (PG.GT.0)) .OR. (PI.GT.0))
     *   THEN
         IF (PCATI(KINAX+2,2).LE.1) THEN
            NXX = 2
         ELSE
            NXX = 3
            END IF
C                                       alignment
         DO 40 IP = 2,4
            IF (IP.EQ.2) THEN
               IF ((PI.NE.2) .AND. (PC.NE.2) .AND. (PG.NE.2)) GO TO 40
            ELSE
               IF (PI.LE.0) GO TO 40
               END IF
C                                       Set Map 2/3 corners
            DO 25 I = 1,NXX
               J = KRCRP + I - 1
               BLC(I,IP) = PCATR(J,IP) - PCATR(J,1) + BLC(I,1)
               TRC(I,IP) = PCATR(J,IP) - PCATR(J,1) + TRC(I,1)
               IF (DOGRID.LT.-1.5) BLC(I,IP) = BLC(I,1)
               IF (DOGRID.LT.-1.5) TRC(I,IP) = TRC(I,1)
               J = I - 1
               IF (DOGRID.GT.-0.1) THEN
                  DX = PCATD(KDCRV+J,1) + (BLC(I,1) - PCATR(KRCRP+J,1))
     *               * PCATR(KRCIC+J,1)
                  IF (PCATR(KRCIC+J,IP).EQ.0.0) GO TO 45
                  X = (DX - PCATD(KDCRV+J,IP)) / PCATR(KRCIC+J,IP) +
     *               PCATR(KRCRP+J,IP)
                  BLC(I,IP) = IROUND (X)
                  IF ((DOGRID.GE.0.1) .AND. (ABS(X-BLC(I,IP)).GT.0.2))
     *               GO TO 45
                  TRC(I,IP) = BLC(I,IP) + TRC(I,1) - BLC(I,1)
                  END IF
C                                       smaller subimage needed?
               IF (BLC(I,IP).LT.1.0) THEN
                  DO 10 LP = 1,IP
                     BLC(I,LP) = BLC(I,LP) + 1.0 - BLC(I,IP)
 10                  CONTINUE
                  REDUCE = .TRUE.
                  END IF
               IF (TRC(I,IP).GT.PCATI(KINAX+J,IP)) THEN
                  DO 15 LP = 1,IP
                     TRC(I,LP) = TRC(I,LP) + PCATI(KINAX+J,LP) -
     *                  TRC(I,IP)
 15                  CONTINUE
                  REDUCE = .TRUE.
                  END IF
               DO 20 LP = 1,IP
                  IF ((LP.GE.3) .OR. (LP.EQ.PI) .OR. (LP.EQ.PC) .OR.
     *               (LP.EQ.PG)) THEN
                     IF (I.LE.2) THEN
                        IF (BLC(I,LP).GE.TRC(I,LP)) GO TO 45
                     ELSE
                        IF (BLC(I,LP).GT.TRC(I,LP)) GO TO 45
                        END IF
                     END IF
 20               CONTINUE
 25            CONTINUE
C                                       Check true coincidence
            IF (DOGRID.GE.0.1) THEN
               DO 30 I = 1,NXX
                  J = I - 1
                  IPOINT = KHCTP + J*2
                  CALL H2CHR (8, 1, PCATH(IPOINT,1), CHTMP)
                  CALL H2CHR (8, 1, PCATH(IPOINT,IP), CHTMP1)
                  IF (CHTMP.NE.CHTMP1) GO TO 45
                  X = 0.2 * 0.2 * ABS (PCATR(KRCIC+J,1))
                  IF (ABS(PCATR(KRCIC+J,1)-PCATR(KRCIC+J,IP)).GT.X)
     *               GO TO 45
                  IF (ABS(PCATR(KRCRT+J,1)-PCATR(KRCRT+J,IP)).GT.1.)
     *               GO TO 45
 30               CONTINUE
               END IF
            CALL WINDOW (PCATI(KIDIM,IP), PCATI(KINAX,IP), BLC(1,IP),
     *         TRC(1,IP), IRET)
            IF (IRET.NE.0) GO TO 999
 40         CONTINUE
         END IF
      IF (REDUCE) THEN
         MSGTXT = 'Input maps coincident on reduced subimage only'
         CALL MSGWRT (6)
         END IF
      IF (INCOLR.GT.0.0) TRC(3,PG) = BLC(3,PG)
      IF ((PI.EQ.1) .OR. (PC.EQ.1) .OR. (PG.EQ.1)) THEN
         CALL RCOPY (7, BLC(1,1), XBLC)
         CALL RCOPY (7, TRC(1,1), XTRC)
      ELSE
         CALL RCOPY (7, BLC(1,2), XBLC)
         CALL RCOPY (7, TRC(1,2), XTRC)
         END IF
      GO TO 999
C                                        Maps not coincident
 45   WRITE (MSGTXT,1045) I
      CALL MSGWRT (7)
      IRET = I
C
 999  RETURN
C-----------------------------------------------------------------------
 1045 FORMAT ('INPUT MAPS ARE NOT COINCIDENT: AXIS',I2)
      END
      SUBROUTINE LINEPL (BLC, TRC, P1, P2, PLBUF, IERR)
C-----------------------------------------------------------------------
C   Draw a line segment
C   Inputs:
C      BLC      R(2)     BLC of image being contoured
C      TRC      R(2)     TRC of image being contoured
C      P1       R(2)     Location (X,Y pixels) of line end 1
C      P2       R(2)     Location (X,Y pixels) of line end 2
C   In/out:
C      PLBUF   I(256)   i/o buffer
C   Outputs:
C      IERR     I      0 => OK
C-----------------------------------------------------------------------
      REAL      P1(2), P2(2), BLC(2), TRC(2)
      INTEGER   IERR, PLBUF(256)
C
      REAL      CPX(2), CPY(2)
      INTEGER   I
C-----------------------------------------------------------------------
      CPX(1) = P1(1)
      CPY(1) = P1(2)
      CPX(2) = P2(1)
      CPY(2) = P2(2)
      CALL LINLIM (BLC, TRC, CPX, CPY, I)
      IERR = 0
      IF (I.EQ.0) THEN
         CALL GPOS (CPX(1), CPY(1), PLBUF, IERR)
         IF (IERR.EQ.0) CALL GVEC (CPX(2), CPY(2), PLBUF, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE LAYOUT (BLC, TRC, PCATR, INFILE, PLBUF, IERR)
C-----------------------------------------------------------------------
C   Plot the boxes
C   Input:
C      BLC      R(2)     BLC of image being contoured
C      TRC      R(2)     TRC of image being contoured
C      PCATR    R(256)   Image header
C      LAYOUT   C*48     In file name
C   In/out:
C      PLBUF   I(256)   i/o buffer
C   Output:
C      IERR     I        0 => OK
C-----------------------------------------------------------------------
      REAL      BLC(2), TRC(2), PCATR(256)
      INTEGER   PLBUF(256), IERR
      CHARACTER INFILE*(*)
C
      INTEGER   JTRIM, MSGSAV, INC, LUN, FIND, KBPLIM, KBP, NC, I, NP,
     *   J, IP, VLNPL(6,2), VLNUM
      CHARACTER LFILE*64, LINE*80
      DOUBLE PRECISION D, THETA, DTHETA
      REAL      R1, R2, X(5), Y(5), DX, DY, PX0, PY0, P1(2), P2(2),
     *   VLRPL(2,6,2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUN /3/
      DATA VLNPL /12,16,24,40,40,40, 20,20,40,40,40,40/
      DATA VLRPL /1.983,3.683, 3.683,5.563, 5.563,7.391, 7.391,9.144,
     *   9.144,10.87, 10.87,12.5,
     *   1.676,3.518, 3.518,5.423, 5.423,7.277, 7.277,9.081,
     *   9.081,10.808, 10.808,12.500/
C-----------------------------------------------------------------------
      IERR = 0
      IF ((PCATR(KRCIC).EQ.0.0) .OR. (PCATR(KRCIC+1).EQ.0.0)) GO TO 999
      PX0 = PCATR(KRCRP)
      PY0 = PCATR(KRCRP+1)
      DX = 1.0 / PCATR(KRCIC)
      DY = 1.0 / PCATR(KRCIC+1)
      MSGSAV = MSGSUP
      VLNUM = 0
      IF (INFILE.EQ.'VLA') VLNUM = 1
      IF (INFILE.EQ.'VLBA') VLNUM = 2
C                                       open layout file
      IF (VLNUM.LE.0) THEN
         INC = JTRIM (INFILE)
         LFILE = INFILE(:INC) // '.layout'
         MSGSUP = 32000
         CALL ZTXOPN ('QRED', LUN, FIND, LFILE, .FALSE., IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.0) THEN
            CALL ZTXCLS (LUN, FIND, IERR)
            CALL ZTXOPN ('READ', LUN, FIND, LFILE, .FALSE., IERR)
            END IF
C                                       panel layout
         IF (IERR.EQ.0) THEN
C                                       read number of cards
            CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ LAYOUT LINE 1'
               GO TO 990
               END IF
            KBPLIM = JTRIM (LINE)
            KBP = 1
            CALL GETNUM (LINE, KBPLIM, KBP, D)
            IF (D.EQ.DBLANK) GO TO 980
            NC = D
            DO 40 J = 1,NC
               CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ LAYOUT DATA LINE'
                  GO TO 990
                  END IF
               KBPLIM = JTRIM (LINE)
               KBP = 1
               CALL GETNUM (LINE, KBPLIM, KBP, D)
               IF (D.EQ.DBLANK) GO TO 980
               NP = D
               CALL GETNUM (LINE, KBPLIM, KBP, D)
               IF (D.EQ.DBLANK) GO TO 980
               R1 = D
               CALL GETNUM (LINE, KBPLIM, KBP, D)
               IF (D.EQ.DBLANK) GO TO 980
               R2 = D
               DTHETA = 360.0D0 / NP * DG2RAD
               THETA = 0.0
               DO 30 IP = 1,NP
                  X(1) = R1 * SIN (THETA)
                  X(2) = R2 * SIN (THETA)
                  X(3) = R2 * SIN (THETA+DTHETA)
                  X(4) = R1 * SIN (THETA+DTHETA)
                  X(5) = X(1)
                  Y(1) = R1 * COS (THETA)
                  Y(2) = R2 * COS (THETA)
                  Y(3) = R2 * COS (THETA+DTHETA)
                  Y(4) = R1 * COS (THETA+DTHETA)
                  Y(5) = Y(1)
                  DO 20 I = 1,4
                     P1(1) = PX0 + DX * X(I)
                     P2(1) = PX0 + DX * X(I+1)
                     P1(2) = PY0 - DY * Y(I)
                     P2(2) = PY0 - DY * Y(I+1)
                     CALL LINEPL (BLC, TRC, P1, P2, PLBUF, IERR)
 20                  CONTINUE
                  THETA = THETA + DTHETA
 30               CONTINUE
 40            CONTINUE
            CALL ZTXCLS (LUN, FIND, IERR)
            END IF
C                                       bolts file
C                                       open layout file
         LFILE = INFILE(:INC) // '.bolts'
         MSGSUP = 32000
         CALL ZTXOPN ('QRED', LUN, FIND, LFILE, .FALSE., IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.0) THEN
            CALL ZTXCLS (LUN, FIND, IERR)
            CALL ZTXOPN ('READ', LUN, FIND, LFILE, .FALSE., IERR)
            END IF
C                                       panel layout
         IF (IERR.NE.0) THEN
            IERR = 0
C                                       read number of cards
         ELSE
            CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ BOLT LINE 1'
               GO TO 990
               END IF
            KBPLIM = JTRIM (LINE)
            KBP = 1
            CALL GETNUM (LINE, KBPLIM, KBP, D)
            IF (D.EQ.DBLANK) GO TO 980
            NC = D
            CALL GETNUM (LINE, KBPLIM, KBP, D)
            IF (D.EQ.DBLANK) GO TO 980
            R2 = D
            IF (R2.LE.0.0) R2 = 0.25
            DO 140 J = 1,NC
               CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ BOLT DATA LINE'
                  GO TO 990
                  END IF
               KBPLIM = JTRIM (LINE)
               KBP = 1
               CALL GETNUM (LINE, KBPLIM, KBP, D)
               IF (D.EQ.DBLANK) GO TO 980
               R1 = D
               CALL GETNUM (LINE, KBPLIM, KBP, D)
               IF (D.EQ.DBLANK) GO TO 980
               NP = D
               CALL GETNUM (LINE, KBPLIM, KBP, D)
               IF (D.EQ.DBLANK) GO TO 980
               THETA = D * DG2RAD
               DTHETA = 360.0D0 / NP * DG2RAD
               DO 130 IP = 1,NP
                  X(1) = R1 * SIN (THETA)
                  Y(1) = R1 * COS (THETA)
                  X(2) = R2 * SIN (THETA + PI/4.0D0)
                  Y(2) = R2 * COS (THETA + PI/4.0D0)
                  P1(1) = PX0 + DX * (X(1) + X(2))
                  P1(2) = PY0 + DY * (Y(1) + Y(2))
                  P2(1) = PX0 + DX * (X(1) - X(2))
                  P2(2) = PY0 + DY * (Y(1) - Y(2))
                  CALL LINEPL (BLC, TRC, P1, P2, PLBUF, IERR)
                  P1(1) = PX0 + DX * (X(1) - Y(2))
                  P1(2) = PY0 + DY * (Y(1) + X(2))
                  P2(1) = PX0 + DX * (X(1) + Y(2))
                  P2(2) = PY0 + DY * (Y(1) - X(2))
                  CALL LINEPL (BLC, TRC, P1, P2, PLBUF, IERR)
                  THETA = THETA + DTHETA
 130              CONTINUE
 140           CONTINUE
            CALL ZTXCLS (LUN, FIND, IERR)
            END IF
C                                       known arrays
      ELSE
         NC = 6
         DO 240 J = 1,NC
            NP = VLNPL(J,VLNUM)
            THETA = 0.0
            DTHETA = 360.0D0 / NP * DG2RAD
            R1 = VLRPL(1,J,VLNUM)
            R2 = VLRPL(2,J,VLNUM)
            DO 230 IP = 1,NP
               X(1) = R1 * SIN (THETA)
               X(2) = R2 * SIN (THETA)
               X(3) = R2 * SIN (THETA+DTHETA)
               X(4) = R1 * SIN (THETA+DTHETA)
               X(5) = X(1)
               Y(1) = R1 * COS (THETA)
               Y(2) = R2 * COS (THETA)
               Y(3) = R2 * COS (THETA+DTHETA)
               Y(4) = R1 * COS (THETA+DTHETA)
               Y(5) = Y(1)
               DO 220 I = 1,4
                  P1(1) = PX0 + DX * X(I)
                  P2(1) = PX0 + DX * X(I+1)
                  P1(2) = PY0 - DY * Y(I)
                  P2(2) = PY0 - DY * Y(I+1)
                  CALL LINEPL (BLC, TRC, P1, P2, PLBUF, IERR)
 220              CONTINUE
               THETA = THETA + DTHETA
 230           CONTINUE
 240        CONTINUE
         END IF
      GO TO 999
C                                       bad value
 980  IERR = 2
      MSGTXT = 'ILLEGAL VALUE READ'
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('LAYOUT ERROR',I3,' ON ',A)
      END
