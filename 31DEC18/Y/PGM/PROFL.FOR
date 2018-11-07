LOCAL INCLUDE 'PROFL.INC'
      REAL      PRUSER, SEQIN, DSKIN, BLC(7), TRC(7), XYRATO, ZXRATO,
     *   PFROT, SKEW, ODIST, XINC, YINC, RANGE(2), TLABEL, DOCIRC,
     *   XINVER, STMULT, XDOTV, XGRCH, XTVCRN(2)
      HOLLERITH XNAMIN(3), XCLSIN(2)
      CHARACTER NAMIN*12, CLSIN*6
      COMMON /INPARM/ PRUSER, XNAMIN, XCLSIN, SEQIN, DSKIN, BLC, TRC,
     *   XYRATO, ZXRATO, PFROT, SKEW, ODIST, XINC, YINC, RANGE,
     *   TLABEL, DOCIRC, XINVER, STMULT, XDOTV, XGRCH, XTVCRN
      COMMON /CHPARM/ NAMIN, CLSIN
LOCAL INCLUDE 'PROFL2.INC'
      REAL      SINSKW, COSSKW, SINROT, COSROT, OX, OY, ZDIST, Z0, ZR,
     *   ZMAX, ZMIN, CSIZX, CSIZY, XL, YL, XCOSRO, XSINRO, XOX, YOX
      INTEGER   LOW, NBKTS
      COMMON /CPFI/ SINSKW, COSSKW, SINROT, COSROT, OX, OY, ZDIST,
     *   Z0, ZR, ZMAX, ZMIN, CSIZX, CSIZY, XL, YL, XCOSRO, XSINRO,
     *   XOX, YOX, LOW, NBKTS
LOCAL END
      PROGRAM PROFL
C-----------------------------------------------------------------------
C! Task to produce a profile plot of an image.
C# Map Map-util Plot-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2002-2003, 2009, 2011, 2014-2015
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
C   PROFL will write commands to a plot file for the execution
C   of a profile plot for a cataloged image file.  The program
C   runs as a detached task initiated from AIPS. First a cataloged
C   file is found using data passed from AIPS.  The list of
C   associated files is searched for PLot files to find the highest
C   version number.  Then a PLot file is created for this map and the
C   catalog header is updated.  Next the graphics commands are
C   written to the plot file.
C    Inputs:   (from AIPS)
C              USERID   R   user number, 0 means use logon user
C                       number, 32000 means any user can be accessed.
C              INNAME   R(3)   name of primary file.
C              INCLASS  R(2)   class of primary file.
C              INSEQ    R   sequence number of primary file.
C              INDISK   R   disk volume number. 0 means try all.
C              BLC      R(7)   the coordinate in the input file to
C                       become the left hand coordinate (1,1) of the
C                       contour plot.  BLC(1) is the X coordinate and
C                       BLC(2) is the Y coordinate.  The first
C                       coordinate in the input image is (1,1).
C              TRC      R(7)   the coordinate in the input file to
C                       become the top right hand corner of the
C                       contour plot. The conventions for BLC hold.
C              XYRATIO  R   the ratio between the scale factor to use
C                       for the X axis and the scale factor to use
C                       for the Y axis.
C              ZXRATIO  R   the ratio (Z/X) between the length of the
C                       unrotated X axis and the maximum height of the
C                       Z axis.
C              ROTATION R   the rotation in degrees of the X-Y plane
C                       about the Z axis counter clockwise.
C              SKEW     R   the rotation in degrees of the plot away
C                       from the initial X-Y plane.
C              DIST     Distance of the observer from the map center
C                       in X axis lengths.
C              XINC     R   draw a line along every XINCth column.  0
C                       means do not draw lines in the Y direction.
C              YINC     R   draw a line along every YINCth row.  0
C                       means do not draw lines in the X direction.
C              PIXRANGE R(2)   the maximum and minimum values allowed
C                       for the map.  All other values will be clipped.
C                       If PIXRANGE(1).GE.PIXRANGE(2) then the map max
C                       and min will be used.
C              LTYPE    R   the type of axis labeling to use for this
C                       plot: 1 = None, 2 = no ticks, 3 = Ra/Dec,
C                       4 = center-rel,  5 = subim center-rel
C              DOCIRCLE R   >0 draw coordinate grids, else just ticks
C              INVERS   R    ST file version number.
C              STFACTOR R    scale star sizes in file for plotting:
C                              0 => no plot of stars.
C      DOTV     R      > 0 => TV, else plot file
C      GRCHAN   R      graphics channel to use
C      TVCORN   R(2)   TV pixel to use (both > 0 => pixel scale)
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER IGFILE*48, PRGNAM*6, TYPIN*2, OPCODE*4
      REAL   BCKTS(6000), RANGE2(2), IBUFF(MABFSS), IWBUFF(MABFSS)
      INTEGER   IBLKOF, IDEPTH(5), NBYBUF, INVER, IGBUFF(256), IWIN(4),
     *   IROUND, IERR, IGFIND, IGLUN, IGSIZE, ILABEL, J, I, IMFIND,
     *   IMLUN, INPRMS, IRETCD, ISEQ, ISLOT, ITYPE, IUSER, IXINC, IYINC,
     *   IVER, IVOL, GRCHN, TVCHN, TVCORN(2)
      LOGICAL   NOSAVE, QUICK, SAVE, T, DOGRID, DOTV
      INCLUDE 'PROFL.INC'
      INCLUDE 'PROFL2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IMLUN, IGLUN /16, 26/
      DATA PRGNAM /'PROFL '/
      DATA NOSAVE, SAVE, T /.FALSE.,.TRUE.,.TRUE./
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Get input values from AIPS.
C                                       Dammit, CHECK INPARM!!!
C                                       Was 41, set to 39.
      INPRMS = 39
      IRETCD = 0
      CALL GTPARM (PRGNAM, INPRMS, QUICK, PRUSER, IWBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         IRETCD = 1
         END IF
      IF (QUICK) CALL RELPOP (IRETCD, IWBUFF, IERR)
      IF (IRETCD.NE.0) GO TO 999
      PRUSER = NLUSER
C
      NBKTS = 6000
      DO 25 I = 1,NBKTS
         BCKTS(I) = -32769.0
 25      CONTINUE
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
      IXINC = IROUND(XINC)
      IYINC = IROUND(YINC)
      ISEQ = IROUND(SEQIN)
      IVOL = IROUND(DSKIN)
      ILABEL = IROUND (TLABEL)
      I = MOD (ABS(ILABEL), 100)
      IF ((I.EQ.0) .OR. (I.GT.10)) THEN
         IF (ILABEL.GE.0) THEN
            ILABEL = (ILABEL/100)*100 + 3
         ELSE
            ILABEL = (ILABEL/100)*100 - 3
            END IF
         END IF
      TLABEL = ILABEL
      IUSER = NLUSER
      IBLKOF = 1
      IRETCD = 4
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      TVCORN(1) = IROUND (XTVCRN(1))
      TVCORN(2) = IROUND (XTVCRN(2))
C                                       Open map file & get header.
      TYPIN = 'MA'
      OPCODE = 'HDWR'
      IF (DOTV) OPCODE = 'READ'
      CALL MAPOPN (OPCODE, IVOL, NAMIN, CLSIN, ISEQ, TYPIN, IUSER,
     *   IMLUN, IMFIND, ISLOT, CATBLK, IWBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1015) IERR, NAMIN, CLSIN, ISEQ, IVOL, IUSER
         CALL MSGWRT (8)
         GO TO 995
         END IF
C                                       Add extension file to header.
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', IVOL, ISLOT, CATBLK, IWBUFF, SAVE, 'READ',
     *      IVER, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       check ST plot parms
      I = 0
      IF (XINVER.LT.0.0) STMULT = 0.0
      IF (STMULT.NE.0.0) CALL FNDEXT ('ST', CATBLK, I)
      IF (I.GT.0) THEN
         J = XINVER + 0.1
         IF (J.LE.0) J = I
         XINVER = J
      ELSE
         XINVER = 0
         STMULT = 0.0
         END IF
      INVER = IROUND (XINVER)
C                                       set window parms
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 965
      IDEPTH(1) = IROUND(BLC(3))
      IDEPTH(2) = IROUND(BLC(4))
      IDEPTH(3) = IROUND(BLC(5))
      IDEPTH(4) = IROUND(BLC(6))
      IDEPTH(5) = IROUND(BLC(7))
      LOCNUM = 1
C                                       Default XYRATO: ratio of
C                                       incr if related.
      IF ((XYRATO.LE.0.01) .OR. (XYRATO.GT.320.0)) THEN
         CALL SETLOC (IDEPTH, T)
         IF ((AXTYP(LOCNUM).EQ.1) .AND. (CATR(KRCIC+1).NE.0.0)) XYRATO =
     *      ABS (CATR(KRCIC) / CATR(KRCIC+1))
         IF (((XYRATO.LE.0.04) .OR. (XYRATO.GT.50.)) .AND.
     *      (TRC(1).NE.BLC(1))) XYRATO = (TRC(2)-BLC(2)) /
     *      (TRC(1)-BLC(1))
         IF ((XYRATO.LE.0.04) .OR. (XYRATO.GT.50.)) XYRATO = 1.0
         END IF
C                                       Build file name.
      CALL ZPHFIL ('PL', IVOL, ISLOT, IVER, IGFILE, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020)
         CALL MSGWRT (7)
         GO TO 965
         END IF
C                                       Init graph file.
C                                       Set approx file size.
      IGSIZE =1
      ITYPE = 4
      CALL GINIT (IVOL, ISLOT, IGFILE, IGSIZE, ITYPE, INPRMS, PRUSER,
     *   DOTV, TVCHN, GRCHN, TVCORN, CATBLK, IGBUFF, IGLUN, IGFIND,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (7)
         GO TO 965
         END IF
C                                       Calculate block offset.
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH, IBLKOF, IERR)
      IF (IERR.NE.0) GO TO 960
      IBLKOF = IBLKOF + 1
C                                       Find allowed max and min.
      CALL RNGSET (RANGE, CATR(KRDMX), CATR(KRDMN), RANGE2)
C                                           Set some common values.
      ZMAX = RANGE2(2)
      ZMIN = RANGE2(1)
      Z0 = 0.0D0
      ZR = (TRC(1) - BLC(1) + 1) / (ZMAX - Z0)  *  ZXRATO * XYRATO
      ZDIST = ODIST * (TRC(1) - BLC(1) + 1) * XYRATO
C                                       Write axis labeling commands.
      DOGRID = DOCIRC.GT.0.0
      CALL PFAXIS (IGBUFF, ILABEL, BLC, TRC, IDEPTH, XYRATO, PFROT,
     *   SKEW, IWIN, DOGRID, IVER, IERR)
      IF (IERR.NE.0) GO TO 950
C                                       Draw stars
      CALL PFSTAR (STMULT, IVOL, ISLOT, INVER, BLC, TRC, IGBUFF, IERR)
      IF (IERR.GE.3) GO TO 950
      CALL GLTYPE (2, IGBUFF, IERR)
      IF (IERR.NE.0) GO TO 950
C                                       Init for double buff read.
      NBYBUF = MABFSS * 2
      CALL MINIT ('READ', IMLUN, IMFIND, CATBLK(KINAX), CATBLK(KINAX+1),
     *   IWIN, IBUFF, NBYBUF, IBLKOF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         CALL MSGWRT (8)
         GO TO 960
         END IF
C                                       Draw 3D contour lines.
      CALL PFDRAW (IMLUN, IMFIND, IWIN, IXINC, IYINC, IBUFF,
     *   IWBUFF, IGBUFF, BCKTS, IERR)
      IF (IERR.NE.0) GO TO 950
C                                       Write 'finish graph' command.
      CALL GFINIS (IGBUFF, IERR)
C                                       Write sucessful finish message.
      IF (IERR.NE.0) GO TO 960
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (IVOL, ISLOT, IVER, IGBUFF, IERR)
            WRITE (MSGTXT,1070) IVER
            CALL MSGWRT (2)
            END IF
         IRETCD = 0
         GO TO 980
C-----------------------------------------------------------------------
C                                       Graph writing error.
 950  WRITE (MSGTXT,1950)
      CALL MSGWRT (8)
C                                       Try to do finish.
      CALL GFINIS (IGBUFF, IERR)
      IF (IERR.EQ.0) THEN
         IF (.NOT.DOTV) CALL HIPLOT (IVOL, ISLOT, IVER, IGBUFF, IERR)
         GO TO 980
         END IF
C                                       Finish not sucessful. Destroy.
 960  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (IGLUN, IGFIND, IERR)
         CALL ZDESTR (IVOL, IGFILE, IERR)
         END IF
 965  IF (.NOT.DOTV) CALL DELEXT ('PL', IVOL, ISLOT, 'READ', CATBLK,
     *   IWBUFF, IVER, IERR)
 970  CALL ZCLOSE (IMLUN, IMFIND, IERR)
      IRETCD = 8
      GO TO 995
C                                       Close map file.
 980  CALL MAPCLS ('READ', IVOL, ISLOT, IMLUN, IMFIND, CATBLK, NOSAVE,
     *   IWBUFF, IERR)
C
 995  CALL DIETSK (IRETCD, QUICK, IWBUFF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR GETTING PARAMETERS FROM AIPS. GTPARM ERR =',I5)
 1015 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' (MA) DISK=',I3,
     *   ' USID=',I5)
 1020 FORMAT ('COULD NOT BUILD GRAPH FILE NAME')
 1030 FORMAT ('GRAPH FILE INIT ERROR. GINIT ERR =',I5)
 1040 FORMAT ('MINIT ERROR =',I5)
 1070 FORMAT ('Successful plot file version',I5,'  created.')
 1950 FORMAT ('Error during graphing will try to finish partial graph')
      END
      SUBROUTINE PFAXIS (IBUFF, ILAB, BLC, TRC, IDEPTH, XYR, PFROT,
     *   SKEW, IWIN, DOGRID, IVER, IERR)
C-----------------------------------------------------------------------
C   PFAXIS is an axis drawing and labelling routine for use with
C   the profile program.  The routine will set the parameters for
C   proper scalling by calling PFSCAL and then draw the axis and
C   labeling.
C   Inputs:
C      IBUFF    I(256)   The output buffer used in writting
C                        graph commands to the graph file.
C      ILAB     I        the type of labeling to use.
C      BLC      R(7)     X and Y pixels to form bottom left hand
C                        corner of the graph.
C      TRC      R(7)     X and Y pixels to form the top right hand
C                        corner of the graph.
C      IDEPTH   I(5)     the depth of the plot plane along the
C                        other 5 axis.
C      XYR      R        ratio between the scale factor to use for
C                        for the X axis to the scale factor to user for
C                        the Y axis.
C      PFROT    R        rotation of X axis counter clockwise.
C      SKEW     R        rotation of X axis away from Y axis.
C      DOGRID   L        do full grid? or just ticks
C   Output:
C      IBUFF    I(256)   the updated graphics output buffer.
C      IWIN     I(4)     calculated map window.
C      IERR     I        error indicator: 0 = No error.
C-----------------------------------------------------------------------
      CHARACTER SPRTXT*80, PLABEL(9)*16, ELABEL(2)*8, LABTXT(2)*80,
     *   ATIME*8, ADATE*12, BJUNK*8
      REAL   BLC(7), CH(4), TRC(7), HBLC(2), HTRC(2), NBLC(2), NTRC(2),
     *   DCX, DCY, X, X0, X1, XBRAT, XR, XYR, Y, Y0, Y1, YGAP,
     *   DUMCH(4), PFROT, SKEW
      INTEGER   IWIN(4), NTEXT, IDEPTH(5), IBUFF(256), IERR, ILAB,
     *   NLABEL(9), IANGL, INCHAR, I, IERR2, IROUND, IE, IEPO, IT(3),
     *   ID(3), IVER, LTYPE
      LOGICAL   FWD, SLICE, DOGRID
      INCLUDE 'PROFL2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA NLABEL /14, 13, 14, 13, 15, 11, 12, 10, 11/
      DATA PLABEL /'Ecliptic long.  ', 'Ecliptic lat.   ',
     *   'Galactic long.  ', 'Galactic lat.   ', 'Right Ascension ',
     *   'Declination     ', 'Time (hours)    ', 'HA (hours)     ',
     *   'LST (hours)     '/
      DATA SLICE /.FALSE./
      DATA XBRAT, DUMCH /25.0, 4*0.5/
      DATA ELABEL /' (B1950)', ' (J2000)'/
C-----------------------------------------------------------------------
C                                       Initial values.
      XR = MAX (TRC(1) - BLC(1), TRC(2) - BLC(2)) / XBRAT
      NBLC(1) = BLC(1) - XR/XYR
      NBLC(2) = BLC(2) - XR
      NTRC(1) = TRC(1) + XR/XYR
      NTRC(2) = TRC(2) + XR
      X0 = NBLC(1)
      X1 = NTRC(1)
      Y0 = NBLC(2)
      Y1 = NTRC(2)
      IWIN(1) = BLC(1) + .5
      IWIN(2) = BLC(2) + .5
      IWIN(3) = TRC(1) + .5
      IWIN(4) = TRC(2) + .5
C                                       Set up labeling
      CH(1) = 0.0
      CH(2) = 0.0
      LTYPE = MOD (ABS(ILAB), 100)
      IF (LTYPE.LT.7) CH(2) = 1.333
      CH(3) = 0.0
      CH(4) = 0.0
      YGAP = 0.
      CALL LABINI (NBLC, NTRC, IDEPTH, CH, ILAB, SLICE, YGAP, LABTXT,
     *   NTEXT)
C                                       Scale and set common CPFI.
      CALL PFSCAL (PFROT, SKEW, XYR, NBLC, NTRC, CH, FWD)
C                                       Read, draw from top to keep
C                                       hidden line perspective right.
      IF (.NOT.FWD) THEN
         IWIN(2) = IWIN(4)
         IWIN(4) = BLC(2) + .5
         END IF
C                                       Init for line drawing.
      X = 1.0
      CALL GINITL (NBLC, NTRC, X, DUMCH, IDEPTH, IBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (7)
         GO TO 999
         END IF
      CALL GLTYPE (1, IBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Calculate profile coor.
      CALL PFXY (X0, Y0, Z0, X, Y)
C                                       Draw borders.
      CALL GPOS (X, Y, IBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL PFXY (X1, Y0, Z0, X, Y)
      CALL GVEC (X, Y, IBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL PFXY (X1, Y1, Z0, X, Y)
      CALL GVEC (X, Y, IBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL PFXY (X0, Y1, Z0, X, Y)
      CALL GVEC (X, Y, IBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL PFXY (X0, Y0, Z0, X, Y)
      CALL GVEC (X, Y, IBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      IF (LTYPE.EQ.1) GO TO 999
C                                       check epoch
      IEPO = IROUND (REPOCH(LOCNUM))
      IF (IEPO.EQ.1950) THEN
         IE = 1
      ELSE IF (IEPO.EQ.2000) THEN
         IE = 2
      ELSE
         IE = 0
         END IF
C                                       Text below
      IF ((NTEXT.GT.0) .AND. (LTYPE.LT.7)) THEN
         DCX = 0.0
         DO 45 I = 1,NTEXT
            DCY = -YGAP
            IANGL = 0
            CALL CHTRIM (LABTXT(I), 80, SPRTXT, INCHAR)
            CALL PFCHAR (INCHAR, IANGL, X0, Y0, DCX, DCY, SPRTXT,
     *         IBUFF, IERR)
            IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
            YGAP = YGAP + 1.333
 45         CONTINUE
         END IF
C                                       Source name, stokes, freq.
      IF (LTYPE.LT.7) THEN
         DCX = 0.0
         DCY = 0.5
         IANGL = 0
         SPRTXT = ' '
         CALL H2CHR (8, 1, CATH(KHOBJ), SPRTXT(1:8))
         INCHAR = 12
         IF (NCHLAB(1,LOCNUM).GT.0) THEN
            SPRTXT(INCHAR:) = SAXLAB(1,LOCNUM)(1:NCHLAB(1,LOCNUM))
            INCHAR = INCHAR + 3 + NCHLAB(1,LOCNUM)
            END IF
         IF (NCHLAB(2,LOCNUM).GT.0) THEN
            SPRTXT(INCHAR:) = SAXLAB(2,LOCNUM)(1:NCHLAB(2,LOCNUM))
            INCHAR = INCHAR + NCHLAB(2,LOCNUM)
            END IF
         CALL PFCHAR (INCHAR, IANGL, X0, Y1, DCX, DCY, SPRTXT, IBUFF,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         END IF
C                                       Date/time version
      IF ((ILAB.GT.1) .AND. (LTYPE.LT.7)) THEN
         DCY = DCY + 1.333
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, ATIME, ADATE)
         WRITE (SPRTXT,1050) IVER, ADATE, ATIME
         CALL REFRMT (SPRTXT, '_', INCHAR)
         CALL PFCHAR (INCHAR, IANGL, X0, Y1, DCX, DCY, SPRTXT, IBUFF,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         END IF
C                                       vertical axes
      I = LABTYP(LOCNUM) / 10
      IF (I.GT.9) I = 0
      IF (I.GT.0) THEN
         INCHAR = NLABEL(I)
         SPRTXT = PLABEL(I)
         IF ((IE.GT.0) .AND. ((I.EQ.5) .OR. (I.EQ.6))) THEN
            SPRTXT(INCHAR+1:) = ELABEL(IE)
            INCHAR = INCHAR + 8
            END IF
      ELSE
         SPRTXT = CPREF(2,LOCNUM) // CTYP(2,LOCNUM)
         CALL CHTRIM (SPRTXT, 25, SPRTXT, INCHAR)
         END IF
      Y = (Y1-Y0)/2.0 + Y0
      DCX = -CH(1) + 0.5
      DCY = INCHAR / 2.0 - 1.0
      IANGL = 1
      CALL PFCHAR (INCHAR, IANGL, X0, Y, DCX, DCY, SPRTXT, IBUFF, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
C                                       horizontal axes
      I = MOD (LABTYP(LOCNUM), 10)
      IF (I.GT.9) I = 0
      IF (I.GT.0) THEN
         INCHAR = NLABEL(I)
         SPRTXT = PLABEL(I)
         IF ((IE.GT.0) .AND. ((I.EQ.5) .OR. (I.EQ.6))) THEN
            SPRTXT(INCHAR+1:) = ELABEL(IE)
            INCHAR = INCHAR + 8
            END IF
      ELSE
         SPRTXT = CPREF(1,LOCNUM) // CTYP(1,LOCNUM)
         CALL CHTRIM (SPRTXT, 25, SPRTXT, INCHAR)
         END IF
      X = (X1-X0)/2.0 + X0
      DCX = -INCHAR / 2.0
      DCY = -2.833
      IF (LTYPE.EQ.2) DCY = -1.5
      IANGL = 0
      CALL PFCHAR (INCHAR, IANGL, X, Y0, DCX, DCY, SPRTXT, IBUFF, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
C                                       Peak flux.
      IF (LTYPE.LT.7) THEN
         DCX = 0.0
         DCY = -YGAP
         IANGL = 0
         IF (IERR.NE.0) GO TO 980
         CALL H2CHR (8, 1, CATH(KHBUN), BJUNK)
         WRITE (SPRTXT,1130) CATR(KRDMX), BJUNK
         CALL REFRMT (SPRTXT, '_', INCHAR)
         CALL PFCHAR (INCHAR, IANGL, X0, Y0, DCX, DCY, SPRTXT, IBUFF,
     *      IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         END IF
      IERR = 0
C                                       Tell tic marks position of
C                                       border, not pixels.
      HBLC(1) = X0
      HBLC(2) = Y0
      HTRC(1) = X1
      HTRC(2) = Y1
C                                       Figure out where tic marks go.
      IF (LTYPE.NE.2) THEN
         IF (AXINC(1,LOCNUM).NE.0) CALL PFTICS (1, HBLC, HTRC, XYR,
     *      DOGRID, IBUFF, IERR2)
         IF (AXINC(2,LOCNUM).NE.0) CALL PFTICS (2, HBLC, HTRC, XYR,
     *      DOGRID, IBUFF, IERR2)
         END IF
      GO TO 999
C                                       Graph drawing error.
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('GRAPH FILE INITIALIZATION ERROR. GINITL ERR =',I5)
 1050 FORMAT ('Plot',I4,' at ',A12,A8)
 1130 FORMAT ('Peak flux =',1PE11.3,1X,A8)
 1980 FORMAT ('GRAPH LABEL WRITING ERROR. IERR =',I5)
      END
      SUBROUTINE PFTICS (IAXIS, BLC, TRC, XYRATO, DOACRS, IBUFF, IERR)
C-----------------------------------------------------------------------
C   PFTICS writes tick marks and tick labels to a plot file
C   Inputs:  IAXIS    I   1 => horizontal,  2 => vertical
C            BLC      R(2)   X AND Y pixels to form bottom left hand
C                     corner of the graph.
C            TRC      R(2)   X and Y pixels to form the top right hand
C                     corner of the graph.
C            XYRATO   R      X to Y ratio
C            DOACRS   L      full coord. grid or just ticks
C            IBUFF    I(256)   buffer being used for output to
C                     the graphics file.
C   OUTPUTS: IBUFF    I(256)   updated graphics I/O buffer.
C            IERR     I   error code: 0 => ok
C                                     1 => bad IAXIS
C                                     2 => graph drawing error
C                                     3 => tic algorithm fails
C-----------------------------------------------------------------------
      CHARACTER SPRTXT*48, CHDL*4
      DOUBLE PRECISION DEG, DEGC, DTX, DX, DTY, DY, PT5SEC, TICX, TICY,
     *   DEGC0, DEGC1, DEG0, XDT, YDT, LDX, LDY, LLDX, LLDY, XTRY,
     *   UPLIM, LOLIM, AYX
      REAL  BLC(2), TRC(2), XYRATO, DCX, DSP, X, Y, XR, YR, XT, YT, DCY,
     *   JUNK, FRACT, PXT, PYT, TICL, XTL, YTL, TICT
      INTEGER   HML(2), IBUFF(256), IAXIS, AXISTP, I, IANGL, IERR, ILEN,
     *   ITRY, COOTYP, INOI, JERR, BAD, ICINC, NCINC, DONMSG, CHKCNT
      LOGICAL   NONUM, DOACRS, FIRST, DOCHK
      INCLUDE 'PROFL2.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA NCINC /512/
C-----------------------------------------------------------------------
C                                       Assign initial values.
      IERR = 0
      IANGL = 0
C                                       Determine axis type.
      IF (IAXIS.EQ.2) THEN
C                                       vertical
         AYX = 0.0D0
         CALL TICINC (IAXIS, BLC, TRC, XYRATO, AYX, DEGC, DEG, INOI,
     *      TICX, TICY, TICL, PT5SEC, ITRY, IERR)
         IF (IERR.NE.0) GO TO 999
         UPLIM = 1.E20
         LOLIM = -1.E20
         IF ((AXTYP(LOCNUM).EQ.1) .OR. (AXTYP(LOCNUM).EQ.3)) THEN
            IF ((CORTYP(LOCNUM).EQ.1) .OR. (CORTYP(LOCNUM).EQ.6)) THEN
               UPLIM = 90.0D0
               LOLIM = -90.0D0
               END IF
            IF ((CORTYP(LOCNUM).EQ.2) .OR. (CORTYP(LOCNUM).EQ.5)) THEN
               UPLIM = RPVAL(2,LOCNUM) + 180.0D0
               LOLIM = RPVAL(2,LOCNUM) - 180.0D0
               END IF
            END IF
         DCX = -1.0
         DCY = -0.5
         BAD = 0
         IF ((DOACRS) .AND. (CORTYP(LOCNUM).GE.1) .AND.
     *      (CORTYP(LOCNUM).LE.4)) BAD = 1
         IF (AXFUNC(1,LOCNUM).GE.2) BAD = 2 * BAD
         IF ((AXFUNC(1,LOCNUM).EQ.4) .OR. (AXFUNC(1,LOCNUM).EQ.6) .OR.
     *      (AXFUNC(1,LOCNUM).EQ.8)) BAD = 2 * BAD
         IF (ABS(AXINC(1,LOCNUM)).GE.0.1) BAD = BAD * 1.51
         IF (ABS(AXINC(1,LOCNUM)).LE.0.005) BAD = BAD / 2
         TICX = TICX / (2*BAD + 1.)
         TICL = TICL / (2*BAD + 1.)
         AXISTP = LABTYP(LOCNUM) / 10
C                                       horizontal
      ELSE IF (IAXIS.EQ.1) THEN
         DCX = 0.5
         DCY = -1.5
         AYX = 0.0D0
         CALL TICINC (IAXIS, BLC, TRC, XYRATO, AYX, DEGC, DEG, INOI,
     *      TICX, TICY, TICL, PT5SEC, ITRY, IERR)
         IF (IERR.NE.0) GO TO 999
         IF ((AXTYP(LOCNUM).EQ.1) .OR. (AXTYP(LOCNUM).EQ.2)) THEN
            IF ((CORTYP(LOCNUM).EQ.2) .OR. (CORTYP(LOCNUM).EQ.4)) THEN
               UPLIM = 90.0D0
               LOLIM = -90.0D0
               END IF
            IF ((CORTYP(LOCNUM).EQ.1) .OR. (CORTYP(LOCNUM).EQ.3)) THEN
               UPLIM = RPVAL(1,LOCNUM) + 180.0D0
               LOLIM = RPVAL(1,LOCNUM) - 180.0D0
               END IF
            END IF
         BAD = 0
         IF ((DOACRS) .AND. (CORTYP(LOCNUM).GE.1) .AND.
     *      (CORTYP(LOCNUM).LE.6)) BAD = 1
         IF (AXFUNC(2,LOCNUM).GE.2) BAD = 2 * BAD
         IF ((AXFUNC(2,LOCNUM).EQ.4) .OR. (AXFUNC(2,LOCNUM).EQ.6) .OR.
     *      (AXFUNC(2,LOCNUM).EQ.8)) BAD = 2 * BAD
         IF (ABS(AXINC(2,LOCNUM)).GE.0.1) BAD = BAD * 1.51
         IF (ABS(AXINC(2,LOCNUM)).LE.0.005) BAD = BAD / 2
         TICY = TICY / (2*BAD + 1.)
         TICL = TICL / (2*BAD + 1.)
         AXISTP = MOD (LABTYP(LOCNUM), 10)
      ELSE
         GO TO 990
         END IF
C                                       Determine possible tic intervls
      NONUM = (CPREF(IAXIS,LOCNUM).EQ.' ') .AND.
     *   (CTYP(IAXIS,LOCNUM).EQ.' ')
      COOTYP = 2
      IF ((AXISTP.EQ.5) .OR. (AXISTP.EQ.7)) COOTYP = 1
      LDX = -1.D10
      LLDX = -1.D10
      LDY = -1.D10
      LLDY = -1.D10
C                                       Draw tic marks and values.
      JUNK = -32000
      CALL FILL (2, JUNK, HML)
      DEGC0 = DEGC
      FIRST = .TRUE.
      CHDL = '$$'
      DEG0 = DEG
      DEGC1 = DEGC0 - DEG
      DO 290 I = 1,INOI
         IF (IAXIS.NE.1) THEN
            DY = DEGC
            CALL FNDX (BLC(1), DY, DX, JERR)
            IF (JERR.NE.0) GO TO 285
         ELSE
            DX = DEGC
            CALL FNDY (BLC(2), DX, DY, JERR)
            IF (JERR.NE.0) GO TO 285
            END IF
C                                       Convert degrees to pixels.
         CALL XYPIX (DX, DY, X, Y, JERR)
         IF (JERR.NE.0) GO TO 285
         IF ((X.LT.BLC(1)-0.01) .OR. (X.GT.TRC(1)+0.01)) GO TO 285
         IF ((Y.LT.BLC(2)-0.01) .OR. (Y.GT.TRC(2)+0.01)) GO TO 285
         IF (DX.NE.LDX) LLDX = LDX
         IF (DY.NE.LDY) LLDY = LDY
         LDX = DX
         LDY = DY
         CALL PFXY (X, Y, Z0, XR, YR)
         CALL GPOS (XR, YR, IBUFF, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Find end of tic.
         DTX = DX + SIGN (1.0, AXINC(1,LOCNUM)) * TICX
         DTY = DY + SIGN (1.0, AXINC(2,LOCNUM)) * TICY
         CALL XYPIX (DTX, DTY, XT, YT, JERR)
         IF (JERR.NE.0) GO TO 235
         TICT = SQRT ((XT-X)**2 + (YT-Y)**2)
         IF (TICL.LE.0.) TICL = 1.
         IF ((TICT.GT.TICL) .OR. (TICT.LT.0.1*TICL)) THEN
            IF (TICT.EQ.0.0) GO TO 235
            DTX = DX + SIGN (1.0, AXINC(1,LOCNUM)) * TICX * TICL / TICT
            DTY = DY + SIGN (1.0, AXINC(2,LOCNUM)) * TICY * TICL / TICT
            CALL XYPIX (DTX, DTY, XT, YT, JERR)
            IF (JERR.NE.0) GO TO 235
            END IF
         IF ((XT.LT.BLC(1)-0.01) .OR. (XT.GT.TRC(1)+0.01)) GO TO 235
         IF ((YT.LT.BLC(2)-0.01) .OR. (YT.GT.TRC(2)+0.01)) GO TO 235
C                                       Simple tick
         IF (.NOT.DOACRS) THEN
C                                       Position at end of tic.
            CALL PFXY (XT, YT, Z0, XR, YR)
            CALL GPOS (XR, YR, IBUFF, IERR)
            IF (IERR.NE.0) GO TO 980
C                                       Draw back to border.
            CALL PFXY (X, Y, Z0, XR, YR)
            CALL GVEC (XR, YR, IBUFF, IERR)
            IF (IERR.NE.0) GO TO 980
            DEGC1 = DEGC - DEG
            IF (FIRST) DEGC0 = DEGC
            FIRST = .FALSE.
C                                       Tick all way across
         ELSE
            DONMSG = IAXIS
 230        CONTINUE
               IF (DONMSG.EQ.1) WRITE (MSGTXT,1230) DEGC
               IF (DONMSG.EQ.2) WRITE (MSGTXT,1231) DEGC
               IF (DONMSG.GT.0) CALL MSGWRT (2)
               DONMSG = 0
               CALL PFXY (XT, YT, Z0, XR, YR)
               CALL GVEC (XR, YR, IBUFF, IERR)
               IF (IERR.NE.0) GO TO 980
               PXT = XT
               PYT = YT
               XDT = DTX
               YDT = DTY
               DTX = DTX + SIGN (1.0, AXINC(1,LOCNUM)) * TICX
               DTY = DTY + SIGN (1.0, AXINC(2,LOCNUM)) * TICY
               CALL XYPIX (DTX, DTY, XT, YT, JERR)
               IF (JERR.NE.0) GO TO 235
               TICT = SQRT ((XT-PXT)**2 + (YT-PYT)**2)
               IF ((TICT.GT.TICL) .OR. (TICT.LT.0.1*TICL)) THEN
                  IF (TICT.EQ.0.0) GO TO 235
                  DTX = XDT + SIGN (1.0, AXINC(1,LOCNUM)) * TICX *
     *               TICL/TICT
                  DTY = YDT + SIGN (1.0, AXINC(2,LOCNUM)) * TICY *
     *               TICL/TICT
                  CALL XYPIX (DTX, DTY, XT, YT, JERR)
                  IF (JERR.NE.0) GO TO 235
                  END IF
               IF ((XT.GE.BLC(1)) .AND. (XT.LE.TRC(1)) .AND.
     *            (YT.GE.BLC(2)) .AND. (YT.LE.TRC(2))) GO TO 230
            FRACT = 1.0
            IF (XT.LT.BLC(1)) FRACT = (BLC(1) - PXT) / (XT - PXT)
            IF (XT.GT.TRC(1)) FRACT = (TRC(1) - PXT) / (XT - PXT)
            IF (YT.LT.BLC(2)) FRACT = MIN (FRACT,
     *         (BLC(2) - PYT) / (YT - PYT))
            IF (YT.GT.TRC(2)) FRACT = MIN (FRACT,
     *         (TRC(2) - PYT) / (YT - PYT))
            XT = PXT + FRACT * (XT - PXT)
            YT = PYT + FRACT * (YT - PYT)
            CALL PFXY (XT, YT, Z0, XR, YR)
            CALL GVEC (XR, YR, IBUFF, IERR)
            IF (IERR.NE.0) GO TO 980
            IF ((IAXIS.EQ.2) .AND. ((XT.GT.TRC(1)) .OR. (XT.LT.BLC(1))))
     *         DEGC1 = DEGC - DEG
            IF ((IAXIS.EQ.1) .AND. ((YT.GT.TRC(2)) .OR. (YT.LT.BLC(2))))
     *         DEGC1 = DEGC - DEG
            IF (FIRST) DEGC0 = DEGC
            IF (FIRST) DEGC1 = DEGC - DEG
            FIRST = .FALSE.
            END IF
C                                       Label
 235     IF (.NOT.NONUM) THEN
            CALL PFXY (X, Y, Z0, XR, YR)
            CALL GPOS (XR, YR, IBUFF, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TICSTR (ITRY, DEGC, PT5SEC, AXISTP, COOTYP, CHDL, HML,
     *         SPRTXT, ILEN)
            DSP = DCX - ILEN
            CALL PFCHAR (ILEN, IANGL, X, Y, DSP, DCY, SPRTXT, IBUFF,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
 285     DEGC = DEGC - DEG
 290     CONTINUE
C                                       Draw tics for other side.
      IF (DOACRS) GO TO 395
C                                       Same intervals but not
C                                       necessarily same values.
C                                       vertical
      IF (IAXIS.NE.1) THEN
         CALL TICINC (6, BLC, TRC, XYRATO, AYX, DEGC, DEG, INOI,
     *      TICX, TICY, TICL, PT5SEC, ITRY, IERR)
         IF (IERR.NE.0) GO TO 395
C                                       horizontal
      ELSE
         CALL TICINC (5, BLC, TRC, XYRATO, AYX, DEGC, DEG, INOI,
     *      TICX, TICY, TICL, PT5SEC, ITRY, IERR)
         IF (IERR.NE.0) GO TO 395
         END IF
      IF (INOI.LE.0) GO TO 395
C                                       Loop for other border.
      DO 390 I= 1,INOI
         IF (IAXIS.NE.1) THEN
            DY = DEGC
            CALL FNDX (TRC(1), DY, DX, JERR)
            IF (JERR.NE.0) GO TO 380
         ELSE
            DX = DEGC
            CALL FNDY (TRC(2), DX, DY, JERR)
            IF (JERR.NE.0) GO TO 380
            END IF
C                                       Convert degrees to pixels.
         CALL XYPIX (DX, DY, X, Y, JERR)
         IF (JERR.NE.0) GO TO 380
         IF ((X.LT.BLC(1)-0.01) .OR. (X.GT.TRC(1)+0.01)) GO TO 380
         IF ((Y.LT.BLC(2)-0.01) .OR. (Y.GT.TRC(2)+0.01)) GO TO 380
C                                       Find end of tic.
         DTX = DX - SIGN (1.0, AXINC(1,LOCNUM)) * TICX
         DTY = DY - SIGN (1.0, AXINC(2,LOCNUM)) * TICY
         CALL XYPIX (DTX, DTY, XT, YT, JERR)
         IF (JERR.NE.0) GO TO 380
         TICT = SQRT ((XT-X)**2 + (YT-Y)**2)
         IF ((TICT.GT.TICL) .OR. (TICT.LT.0.1*TICL)) THEN
            IF (TICT.EQ.0.0) GO TO 380
            DTX = DX - SIGN (1.0, AXINC(1,LOCNUM)) * TICX * TICL/TICT
            DTY = DY - SIGN (1.0, AXINC(2,LOCNUM)) * TICY * TICL/TICT
            CALL XYPIX (DTX, DTY, XT, YT, JERR)
            IF (JERR.NE.0) GO TO 380
            END IF
C                                       Simple ticks ONLY
         IF ((XT.GE.BLC(1)-0.01) .AND. (XT.LE.TRC(1)+0.01) .AND.
     *      (YT.GE.BLC(2)-0.01) .AND. (YT.LE.TRC(2)+0.01)) THEN
C                                       Position at end of tic.
            CALL PFXY (XT, YT, Z0, XT, YT)
            CALL GPOS (XT, YT, IBUFF, IERR)
            IF (IERR.NE.0) GO TO 980
C                                       Draw back to border.
            CALL PFXY (X, Y, Z0, X, Y)
            CALL GVEC (X, Y, IBUFF, IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
 380     DEGC = DEGC - DEG
 390     CONTINUE
C                                       Full curves from top?
 395     IF (.NOT.DOACRS) GO TO 999
 396        DEGC = DEGC0 - DEG0
            ICINC = 3
            I = 1 - ICINC
            XTRY = 0.0D0
 399     I = I + ICINC
         IF (I.GT.NCINC) GO TO 450
 400     DEGC = DEGC + DEG0
            IF ((DEGC.LT.LOLIM) .AND. (DEG0.LT.0.0D0)) GO TO 450
            IF ((DEGC.GT.UPLIM) .AND. (DEG0.GT.0.0D0)) GO TO 450
            IF ((DEGC.LT.LOLIM) .AND. (DEG0.GT.0.0D0)) GO TO 400
            IF ((DEGC.GT.UPLIM) .AND. (DEG0.LT.0.0D0)) GO TO 400
            CHKCNT = 0
            DOCHK = .FALSE.
            PXT = ((NCINC+1.-I)*BLC(1) + I*TRC(1)) / (NCINC+1.0)
            PYT = ((NCINC+1.-I)*BLC(2) + I*TRC(2)) / (NCINC+1.0)
            IF (IAXIS.EQ.2) DY = DEGC
            IF (IAXIS.EQ.2) CALL FNDX (PXT, DY, DX, JERR)
            IF (IAXIS.EQ.1) DX = DEGC
            IF (IAXIS.EQ.1) CALL FNDY (PYT, DX, DY, JERR)
            IF (JERR.EQ.0) GO TO 404
               IF (IAXIS.EQ.1) DY = LDY
               IF (IAXIS.EQ.2) DX = LDX
               CALL XYPIX (DX, DY, X, Y, JERR)
               IF (JERR.NE.0) GO TO 445
               IF ((X.LE.TRC(1)) .AND. (X.GE.BLC(1)) .AND.
     *            (Y.LE.TRC(2)) .AND. (Y.GE.BLC(2))) GO TO 404
               XTRY = XTRY + 1.0D0
               IF (XTRY.GT.50.0D0) GO TO 450
               IF (IAXIS.EQ.1) DY = LDY + XTRY * (LDY - LLDY)
               IF (IAXIS.EQ.2) DX = LDX + XTRY * (LDX - LLDX)
 404        CALL XYPIX (DX, DY, X, Y, JERR)
            IF (JERR.NE.0) GO TO 445
            IF ((X.GT.TRC(1)) .OR. (X.LT.BLC(1))) GO TO 445
            IF ((Y.GT.TRC(2)) .OR. (Y.LT.BLC(2))) GO TO 445
            XTRY = 0.0D0
            IF (DX.NE.LDX) LLDX = LDX
            IF (DY.NE.LDY) LLDY = LDY
            LDX = DX
            LDY = DY
 405        DONMSG = IAXIS
            IF (DONMSG.EQ.1) WRITE (MSGTXT,1230) DEGC
            IF (DONMSG.EQ.2) WRITE (MSGTXT,1231) DEGC
            IF (DONMSG.GT.0) CALL MSGWRT (2)
            DONMSG = 0
            XT = X
            YT = Y
            DTX = DX
            DTY = DY
            CALL PFXY (X, Y, Z0, XR, YR)
            CALL GPOS (XR, YR, IBUFF, IERR)
            IF (IERR.NE.0) GO TO 980
 410        CONTINUE
               PXT = XT
               PYT = YT
               CALL PFXY (XT, YT, Z0, XR, YR)
               IF ((XT.NE.X) .OR. (YT.NE.Y)) CALL GVEC (XR, YR, IBUFF,
     *            IERR)
               IF (IERR.NE.0) GO TO 980
               XDT = DTX
               YDT = DTY
               DTX = DTX + SIGN (1.0, AXINC(1,LOCNUM)) * TICX
               DTY = DTY + SIGN (1.0, AXINC(2,LOCNUM)) * TICY
               CALL XYPIX (DTX, DTY, XT, YT, JERR)
               IF (JERR.NE.0) GO TO 415
               TICT = SQRT ((XT-PXT)**2 + (YT-PYT)**2)
               IF ((TICT.LE.TICL) .AND. (TICT.GE.0.1*TICL)) GO TO 411
                  IF (TICT.EQ.0.0) GO TO 415
                  DTX = XDT + SIGN (1.0, AXINC(1,LOCNUM)) * TICX *
     *               TICL/TICT
                  DTY = YDT + SIGN (1.0, AXINC(2,LOCNUM)) * TICY *
     *               TICL/TICT
                  CALL XYPIX (DTX, DTY, XT, YT, JERR)
                  IF (JERR.NE.0) GO TO 415
 411           IF ((XT.GE.BLC(1)) .AND. (XT.LE.TRC(1)) .AND.
     *            (YT.GE.BLC(2)) .AND. (YT.LE.TRC(2))) GO TO 410
            IF (IAXIS.EQ.1) DOCHK = (XT.LT.BLC(1)) .OR. (XT.GT.TRC(1))
            IF (IAXIS.EQ.2) DOCHK = (YT.LT.BLC(2)) .OR. (YT.GT.TRC(2))
            XTL = XT
            YTL = YT
            FRACT = 1.0
            IF (XT.LT.BLC(1)) FRACT = (BLC(1) - PXT) / (XT - PXT)
            IF (XT.GT.TRC(1)) FRACT = (TRC(1) - PXT) / (XT - PXT)
            IF (YT.LT.BLC(2)) FRACT = MIN (FRACT,
     *         (BLC(2) - PYT) / (YT - PYT))
            IF (YT.GT.TRC(2)) FRACT = MIN (FRACT,
     *         (TRC(2) - PYT) / (YT - PYT))
            XT = PXT + FRACT * (XT - PXT)
            YT = PYT + FRACT * (YT - PYT)
            CALL PFXY (XT, YT, Z0, XR, YR)
            CALL GVEC (XR, YR, IBUFF, IERR)
            IF (IERR.NE.0) GO TO 980
 415        XT = X
            YT = Y
            DTX = DX
            DTY = DY
            CALL PFXY (X, Y, Z0, XR, YR)
            CALL GPOS (XR, YR, IBUFF, IERR)
            IF (IERR.NE.0) GO TO 980
 420        CONTINUE
               PXT = XT
               PYT = YT
               CALL PFXY (XT, YT, Z0, XR, YR)
               IF ((XT.NE.X) .OR. (YT.NE.Y)) CALL GVEC (XR, YR, IBUFF,
     *            IERR)
               IF (IERR.NE.0) GO TO 980
               XDT = DTX
               YDT = DTY
               DTX = DTX - SIGN (1.0, AXINC(1,LOCNUM)) * TICX
               DTY = DTY - SIGN (1.0, AXINC(2,LOCNUM)) * TICY
               CALL XYPIX (DTX, DTY, XT, YT, JERR)
               IF (JERR.NE.0) GO TO 425
               TICT = SQRT ((XT-PXT)**2 + (YT-PYT)**2)
               IF ((TICT.LE.TICL) .AND. (TICT.GE.0.1*TICL)) GO TO 421
                  IF (TICT.EQ.0.0) GO TO 425
                  DTX = XDT - SIGN (1.0, AXINC(1,LOCNUM)) * TICX *
     *               TICL/TICT
                  DTY = YDT - SIGN (1.0, AXINC(2,LOCNUM)) * TICY *
     *               TICL/TICT
                  CALL XYPIX (DTX, DTY, XT, YT, JERR)
                  IF (JERR.NE.0) GO TO 425
 421           IF ((XT.GE.BLC(1)) .AND. (XT.LE.TRC(1)) .AND.
     *            (YT.GE.BLC(2)) .AND. (YT.LE.TRC(2))) GO TO 420
            IF (.NOT.DOCHK) XTL = XT
            IF (.NOT.DOCHK) YTL = YT
            IF (IAXIS.EQ.1) DOCHK = (XT.LT.BLC(1)) .OR. (XT.GT.TRC(1))
     *         .OR. DOCHK
            IF (IAXIS.EQ.2) DOCHK = (YT.LT.BLC(2)) .OR. (YT.GT.TRC(2))
     *         .OR. DOCHK
            FRACT = 1.0
            IF (XT.LT.BLC(1)) FRACT = (BLC(1) - PXT) / (XT - PXT)
            IF (XT.GT.TRC(1)) FRACT = (TRC(1) - PXT) / (XT - PXT)
            IF (YT.LT.BLC(2)) FRACT = MIN (FRACT,
     *         (BLC(2) - PYT) / (YT - PYT))
            IF (YT.GT.TRC(2)) FRACT = MIN (FRACT,
     *         (TRC(2) - PYT) / (YT - PYT))
            XT = PXT + FRACT * (XT - PXT)
            YT = PYT + FRACT * (YT - PYT)
            CALL PFXY (XT, YT, Z0, XR, YR)
            CALL GVEC (XR, YR, IBUFF, IERR)
            IF (IERR.NE.0) GO TO 980
 425        IF (.NOT.DOCHK) GO TO 400
               IF (CHKCNT.GT.0) GO TO 400
               IF (BAD.LE.1) GO TO 400
               CHKCNT = 1
               IF (IAXIS.EQ.1) PYT = (YTL + TRC(2)) / 2.0
               IF (IAXIS.EQ.2) PXT = (XTL + TRC(1)) / 2.0
               YTL = TRC(2) - (YTL - BLC(2)) / 2.0
               XTL = TRC(1) - (XTL - BLC(1)) / 2.0
               IF (IAXIS.EQ.1) PYT = MAX (PYT, YTL)
               IF (IAXIS.EQ.2) PXT = MAX (PXT, XTL)
               IF (IAXIS.EQ.2) CALL FNDX (PXT, DY, DX, JERR)
               IF (IAXIS.EQ.1) CALL FNDY (PYT, DX, DY, JERR)
               IF (JERR.NE.0) GO TO 400
               CALL XYPIX (DX, DY, X, Y, JERR)
               IF (JERR.NE.0) GO TO 400
               IF ((X.GT.TRC(1)) .OR. (X.LT.BLC(1))) GO TO 400
               IF ((Y.GT.TRC(2)) .OR. (Y.LT.BLC(2))) GO TO 400
               GO TO 405
 445     DEGC = DEGC - DEG0
         GO TO 399
C                                       Do other direction
 450  IF (DEGC0.EQ.DEGC1) GO TO 999
         DEGC0 = DEGC1
         DEG0 = -DEG0
         GO TO 396
C                                       Graph drawing error.
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (7)
      IERR = 2
      GO TO 999
C                                       Invalid axis type.
 990  WRITE (MSGTXT,1990)
      CALL MSGWRT (8)
      IERR = 1
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1230 FORMAT ('Start following longitude =',1PD13.5)
 1231 FORMAT ('Start following latitude =',1PD13.5)
 1980 FORMAT ('PFTICS: GRAPH LABEL WRITING ERROR. IERR =',I5)
 1990 FORMAT ('PFTICS: INVALID AXIS TYPE.')
      END
      SUBROUTINE PFSCAL (ROT, SKEW, XYR, BLC, TRC, CH, FWD)
C-----------------------------------------------------------------------
C   This routine will calculate scale factors (in common CPFI) to
C   project a 3 D image onto a rectangle.  These values will be used
C   by PFXY to convert an X,Y,Z to a corresponding X,Y.
C   INPUTS:  ROT     R   rotation of X axis (counter clockwise).
C            SKEW    R   rotation of Y axis away from X axis.
C            XYR     R   ratio of X pixel separation to Y pixel sep.
C            BLC     R(2)   X,Y bottom left hand corner of map.
C            TRC     R(2)   X,Y top right hand corner of map.
C            CH      R(4)   distance (before rotation) of border of
C                    plot in characters from edges of map. CH(1) =
C                    left, CH(2)=bottom, CH(3)=right, CH(4)=top.
C   OUTPUTS: BLC     R(2)   X,Y bottom left hand corner of plot after
C                    rotation.
C            TRC     R(2)   X,Y top right hand corner of plot after
C                    rotation.
C            FWD     L   true if map to be read forward false if map
C                    to be read backward.
C   COMMON:  COSSKW  R   cosine of the skew angle.
C            SINSKW  R   sine of the skew angle.
C            COSROT  R   cosine of the rotation angle.
C            SINROT  R   sine of the rotation angle.
C            OX      R   X center of map about which rotations are
C                    calculated.
C            OY      R   Y center of map.
C            CSIZEX  R   the X size in pixels of characters (unrotated)
C            CSIZEY  R   the Y size in pixels of characters.
C-----------------------------------------------------------------------
      REAL   BLC(2), TRC(2), CH(4), CMX(4), CMY(4), EX(10), EY(10),
     *   CRAT, CONV, ROT, SKEW, XYR, XLEN, YLEN, YMAX
      INTEGER   I
      LOGICAL   T, F, FWD
      INCLUDE 'PROFL2.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA CRAT /625.0/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C
      CONV = 3.14159 / 180.0
C                                       Find SINs, COSs of skew, rot.
      COSSKW = COS(SKEW * CONV)
      SINSKW = SIN(SKEW * CONV)
      COSROT = COS(ROT * CONV)
      SINROT = SIN(ROT * CONV)
      XCOSRO = XYR * COSROT
      XSINRO = XYR * SINROT
C                                       Center of plot in pixel coor.
      OX = (TRC(1) + BLC(1)) / 2.0
      OY = (TRC(2) + BLC(2)) / 2.0
      XOX = 0.0
      YOX = 0.0
C                                       Length of X, Y in pixels.
      XLEN = TRC(1) - BLC(1) + 1
      YLEN = TRC(2) - BLC(2) + 1
C                                       Calculate corners of map.
      CALL PFXY (BLC(1), BLC(2), ZMAX, CMX(1), CMY(1))
      CALL PFXY (TRC(1), BLC(2), ZMAX, CMX(2), CMY(2))
      CALL PFXY (TRC(1), TRC(2), ZMAX, CMX(3), CMY(3))
      CALL PFXY (BLC(1), TRC(2), ZMAX, CMX(4), CMY(4))
C                                       Find maximum Y coordinate and
C                                       lowest corner.
      LOW = 1
      YMAX = (CMY(1) + CMY(2) + CMY(3) + CMY(4)) / 4.0
      CALL PFXY (OX, OY, ZMAX, CMX(4), CMY(4))
      YMAX = MAX (YMAX, CMY(4))
C                                       zero plane
      CALL PFXY (BLC(1), BLC(2), Z0, CMX(1), CMY(1))
      CALL PFXY (TRC(1), BLC(2), Z0, CMX(2), CMY(2))
      CALL PFXY (TRC(1), TRC(2), Z0, CMX(3), CMY(3))
      CALL PFXY (BLC(1), TRC(2), Z0, CMX(4), CMY(4))
      DO 15 I = 2,4
         IF (CMY(I).LT.CMY(LOW)) LOW = I
 15      CONTINUE
C                                       Heuristic for finding character
C                                       size.
      CSIZX = CSIZPR(1) / CRAT * MAX(XLEN, YLEN)
      CSIZY = CSIZX * CSIZPR(2) / CSIZPR(1)
      CSIZX = CSIZX / XYR
C                                       Find edges of character borders
C                                       before rotation.
C                                       'DEC top and bottom.
      EX(1) = BLC(1) - CH(1) * CSIZX
      EY(1) = (TRC(2) - BLC(2)) / 2.0 + BLC(2) + 9.5 * CSIZY
      EX(2) = EX(1)
      EY(2) = (TRC(2) - BLC(2)) / 2.0 + BLC(2) - 9.5 * CSIZY
C                                       Left no.'s top & bottom.
      EX(3) = BLC(1) - 2.0 * CSIZX
      EY(3) = TRC(2) + .5 * CSIZY
      EX(4) = EX(3)
      EY(4) = BLC(2) - 1.5 * CSIZY
C                                       Numbers etc. at bottom
      EX(5) = BLC(1)
      EY(5) = BLC(2) - CH(2) * CSIZY
      EX(6) = TRC(1)
      EY(6) = EY(5)
C                                       Bottom right corner.
      EX(7) = TRC(1)
      EY(7) = BLC(2)
C                                       Source name, ect.
      EX(8) = BLC(1)
      EY(8) = TRC(2) + CH(4) * CSIZY
      EX(9) = BLC(1) + 31.0 * CSIZX
      IF (CH(4).LT.1.0) EX(9) = BLC(1)
      EY(9) = EY(8)
C                                       Top right corner.
      EX(10) = TRC(1)
      EY(10) = TRC(2)
C                                       Rotate and skew outside corners.
      DO 20 I = 1,10
         CALL PFXY (EX(I), EY(I), Z0, EX(I), EY(I))
 20      CONTINUE
C                                       Find 4 outside corners.
      BLC(1) = EX(1)
      TRC(1) = EX(1)
      BLC(2) = EY(1)
      TRC(2) = MAX (YMAX, EY(1))
      DO 30 I =2,10
         BLC(1) = MIN (BLC(1), EX(I))
         BLC(2) = MIN (BLC(2), EY(I))
         TRC(1) = MAX (TRC(1), EX(I))
         TRC(2) = MAX (TRC(2), EY(I))
 30      CONTINUE
      XOX = -BLC(1) + 1.
      YOX = -BLC(2) + 1.
      BLC(1) = BLC(1) + XOX
      TRC(1) = TRC(1) + XOX
      BLC(2) = BLC(2) + YOX
      TRC(2) = TRC(2) + YOX
C                                       Determine if fwd or bckwrd read
      FWD = T
      IF ((LOW.EQ.3) .OR. (LOW.EQ.4)) FWD = F
C
 999  RETURN
      END
      SUBROUTINE PFXY (X, Y, Z, X2D, Y2D)
C-----------------------------------------------------------------------
C  Given an X,Y,Z of a three dimensional image, PFXY will calculate the
C  X2D, Y2D, of the projection.  PFXY uses parameters in
C  common CPFI which are initialized by PFSCAL.
C  INPUTS:  X       R   X coordinate of 3 D image.
C           Y       R   Y coordinate of 3 d image.
C           Z       R   Z coordinate of 3 D image.
C  COMMON:  COSROT  R   cos of the rotation of the X axis.
C           COSSKW  R   cos of the rotation of the Y axis away from
C                       the X axis.
C           SINROT  R   sin of the rotation of the X axis.
C           SINSKW  R   sin of the rotation of the Y axis away from
C                       the X axis.
C           OX      R   the X origin in pixel coordinates.
C           OY      R   the Y origin in pixel coordinates.
C           ZR      R   the multiplier for the Z value, ie
C                       Height in pixels s = ZR * (Z value)
C           ZMAX    R   the highest allowable Z value.
C           ZMIN    R   the lowest allowable Z value.
C           Z0      R   the pixel value representing zero.
C           ZDIST   R   distance of the observer in pixel lengths.
C  OUTPUTS: X2D     R   the X value of the projection.
C           Y2D     R   the Y value of the projection.
C-----------------------------------------------------------------------
      REAL      X, X1, X2, X2D, Y, Y1, Y2, Y2D, Z, Z1, Z2
      INCLUDE 'PROFL2.INC'
C-----------------------------------------------------------------------
C                                       Clip Z values.
      Z = MIN(Z, ZMAX)
      Z = MAX(Z, ZMIN)
C                                       Do rotation around Z axis.
      X1 = (X - OX) * XCOSRO - (Y - OX) * SINROT
      Y1 = (X - OX) * XSINRO + (Y - OY) * COSROT
      Z1 = (Z - Z0) * ZR
C                                       Rotate around X1 axis.
      X2 = X1
      Y2 = Y1 * COSSKW + Z1 * SINSKW
      Z2 = - Y1 * SINSKW + Z1 * COSSKW
C                                       Do perspective
      X2D = X2 / (1 - Z2/ZDIST) + XOX
      Y2D = Y2 / (1 - Z2/ZDIST) + YOX
C                                       Save last absolute values.
      XL = X2D
      YL = Y2D
C
 999  RETURN
      END
      SUBROUTINE PFDRAW (ILUN, IFIND, IWIN, IXINC, IYINC, IBLK, ILBLK,
     *   IGBLK, BCKTS, IERR)
C-----------------------------------------------------------------------
C   PFDRAW will draw the profile lines for the map.
C   INPUTS:  ILUN    I   logical unit number of graph file.
C            IFIND   I   FTAB pointer for graph file.
C            IWIN    I(4)   window for read of map file.
C            IXINC   I   draw profile line every IXINCth column. Zero
C                    means do not draw profile lines for columns.
C            IYINC   I   draw profile line every IYINCth row. Zero
C                    means do not draw profile lines for rows.
C            IBLK    R(?)   I/O buffer for map.
C            ILBLK   R(?)   array to contain previous row of map.
C            IGBLK   I(256)   I/O buffer for graph file.
C            BCKTS   R(??)   'lowest visible point' buckets.
C   OUTPUT:  IERR    I   error code from GPOS or GVEC. 0=ok.
C-----------------------------------------------------------------------
      REAL      BCKTS(1), YTHR, Y, X, Z, XX, YY, Y1, Y2, IBLK(1),
     *   ILBLK(1)
      INTEGER   IWIN(4), IGBLK(256), ILUN, IFIND, IXINC, IYINC, IERR,
     *   I, J, JJ, IPOS, INROWS, IZERO, INPIXS, IXSTRT, INCX,
     *   INCY, ILEFT, IRIGHT, IMID, J2, IX, IJ
      INCLUDE 'PROFL2.INC'
C-----------------------------------------------------------------------
      IZERO = Z0
      INROWS = ABS (IWIN(4) - IWIN(2)) + 1
      INPIXS = IWIN(3) - IWIN(1) + 1
C                                       Decide which way to loop over
C                                       rows and pixels.
      IXSTRT = IWIN(1)
      INCX = 1
C                                       Read rows right to left.
      IF ((LOW.EQ.1) .OR. (LOW.EQ.4)) GO TO 20
         IXSTRT = IWIN(3)
         INCX = -1
C                                       Do we draw cols from top or bot
 20   CONTINUE
      INCY = 1
      IF ((LOW.EQ.3) .OR. (LOW.EQ.4)) INCY = -1
      ILEFT = MIN (IWIN(1), IWIN(3) )
      IRIGHT = MAX (IWIN(1), IWIN(3) )
      YTHR = OY - ZDIST * SINSKW
C                                       Read first line.
      CALL MDISK ('READ', ILUN, IFIND, IBLK, IPOS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       substitute for blanks
      CALL SNRVAL (INPIXS, Z0, IBLK(IPOS))
C                                       Loop over the rest of the rows.
      DO 100 I = 2,INROWS
         CALL RCOPY (INPIXS, IBLK(IPOS), ILBLK)
         CALL MDISK ('READ', ILUN, IFIND, IBLK, IPOS, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL SNRVAL (INPIXS, Z0, IBLK(IPOS))
C                                       Test for draw X profile.
         IF (IYINC.EQ.0) GO TO 50
         IF (MOD(I-2,IYINC).NE.0) GO TO 50
C                                       Draw along X axis.
            Y = IWIN(2) + (I-2) * INCY
            IF (Y.LT.YTHR) GO TO 42
C                                       Draw outside in.
               X = ILEFT
               Z = ILBLK(1)
               IMID = 1
               CALL PFXY (X, Y, Z, XX, YY)
               CALL GPOS (XX, YY, IGBLK, IERR)
               DO 30 J = 2,INPIXS
                  X = ILEFT + J - 1
                  IMID = J
                  Z = ILBLK(J)
                  CALL PFLINE (X, Y, Z, IGBLK, BCKTS, IERR)
                  IF (XL.GE.XOX) GO TO 35
 30               CONTINUE
C                                       Draw back toward middle.
 35            CONTINUE
               IF (IMID.EQ.INPIXS) GO TO 50
                  X = IRIGHT
                  Z = ILBLK(INPIXS)
                  CALL PFXY (X, Y, Z, XX, YY)
                  CALL GPOS (XX, YY, IGBLK, IERR)
                  J2 = INPIXS - IMID
                  DO 40 JJ = 1, J2
                     J = INPIXS - JJ
                     X = ILEFT + J - 1
                     Z = ILBLK(J)
                     CALL PFLINE (X, Y, Z, IGBLK, BCKTS, IERR)
 40                  CONTINUE
                  GO TO 50
C                                      Draw inside out.
 42            CONTINUE
               IMID = OX
               IMID = MAX (IMID, ILEFT)
               IF (IMID.GE.IRIGHT) GO TO 46
               X = IMID
               Z = ILBLK(IMID - ILEFT + 1)
               CALL PFXY (X, Y, Z, XX, YY)
               CALL GPOS (XX, YY, IGBLK, IERR)
               DO 44 J = IMID,IRIGHT
                  X = J
                  Z = ILBLK(J - ILEFT + 1)
                  CALL PFLINE (X, Y, Z, IGBLK, BCKTS, IERR)
 44               CONTINUE
C                                       Draw right to left.
 46            CONTINUE
               IF (IMID.LE.ILEFT) GO TO 50
                  X = IMID
                  Z = ILBLK(IMID - ILEFT + 1)
                  CALL PFXY (X, Y, Z, XX, YY)
                  CALL GPOS (XX, YY, IGBLK, IERR)
                  J2 = IMID - ILEFT
                  DO 48 JJ = 1, J2
                     J = IMID - JJ
                     X = J
                     Z = ILBLK(J - ILEFT + 1)
                     CALL PFLINE (X, Y, Z, IGBLK, BCKTS, IERR)
 48                  CONTINUE
C                                       Draw along Y axis
 50      IF (IXINC.LE.0) GO TO 100
            Y1 = IWIN(2) + (I-2) * INCY
            Y2 = Y1 + INCY
            DO 60 J = 1,INPIXS,IXINC
               IX = (J-1) * INCX + IXSTRT
               X = IX
               IJ = IX - IWIN(1) + 1
               Z = ILBLK(IJ)
C                                       Position.
               CALL PFXY (X, Y1, Z, XX, YY)
               CALL GPOS (XX, YY, IGBLK, IERR)
               IF (IERR.NE.0) GO TO 999
C                                       Draw up to next Y pixel.
               JJ = IPOS + IJ - 1
               Z = IBLK(JJ)
               CALL PFLINE (X, Y2, Z, IGBLK, BCKTS, IERR)
               IF (IERR.NE.0) GO TO 999
 60            CONTINUE
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE PFLINE (XX, YY, Z, IBLK, BCKTS, IERR)
C-----------------------------------------------------------------------
C  Given an X,Y,Z PFLINE will draw a line from the current position to
C  the new X,Y,Z.  This consists of determining what part of the line
C  is visible and calling GPOS and/or GVEC to draw a line consistent
C  with that determination.  BCKTS (an array containing lowest visible
C  points) is also updated.
C  INPUTS:  X       R   X position of endpoint.
C           Y       R   Y position of endpoint.
C           Z       R   Z position of endpoint.
C           IBLK    I(256)   I/O buffer for graph file.
C           BCKTS   R(??)   lowest visible points.
C  COMMON:  XLEN    R   length of X axis of map in pixels.
C           YLEN    R   length of Y axis of map in pixels.
C           GPHX1   R   BLC X value of plot.
C           GPHX2   R   TRC X value in pixel coordinates of plot.
C           GPHY1   R   BLC Y value of plot.
C           GPHY2   R   TRC Y value of plot.
C           XL      R   last X position in pixel coordinates.
C           YL      R   last Y position in pixel coordinates.
C  OUTPUTS: IERR    R   error code. 0 = ok; 1=error from GPOS or GVEC.
C-----------------------------------------------------------------------
      REAL   BCKTS(1), P1(4), X, Y, Z, XX, YY, XINC
      INTEGER   IBLK(256), IERR, INC, IBKT, IHIDE, I
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'PROFL2.INC'
C-----------------------------------------------------------------------
C                                       Find X,Y end points of line in
C                                       pixel coordinates.
      P1(1) = XL
      P1(2) = YL
      CALL PFXY (XX, YY, Z, P1(3), P1(4))
      INC = 1
      IF (P1(3).LT.P1(1)) INC = -1
      XINC = INC
C                                       Find bucket no. of far side of X
      IBKT = (P1(1) - GPHX1) * (NBKTS - 1) / (GPHX2 - GPHX1)  +  1.0
      IF (INC.LT.0) IBKT = IBKT + 1
      IHIDE = 1
      IF (BCKTS(IBKT).LT.P1(4)) IHIDE = -1
      IF ((BCKTS(IBKT).GT.P1(2)).AND.(IHIDE.LT.0)) P1(2) = BCKTS(IBKT)
C                                       Loop until we reach end of line.
      DO 30 I = 1,NBKTS
         IBKT = IBKT + INC
         X = (IBKT - 1) * (GPHX2 - GPHX1) / (NBKTS - 1) + GPHX1
         IF (XINC * P1(3) .LT. XINC * X) GO TO 40
C                                       Find Y value of line at bucket.
         Y = (X-P1(1)) * ( (P1(4)-P1(2)) / (P1(3)-P1(1)) )  +  P1(2)
C                                       Doing hidden line.
         IF (IHIDE.GE.0) THEN
C                                       Changing to visible line.
            IF (BCKTS(IBKT).LE.Y) THEN
               CALL GPOS ( X, Y, IBLK, IERR)
               IF (IERR.NE.0) GO TO 999
               BCKTS(IBKT) = Y
               IHIDE = -1
               END IF
C                                       Doing visible line.
C                                       Still visible.
         ELSE IF (BCKTS(IBKT).LE.Y) THEN
            BCKTS(IBKT) = Y
C                                       Changing to hidden line.
         ELSE
            CALL GVEC ( X, Y, IBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            IHIDE = 1
            END IF
C
 30      CONTINUE
C                                       Draw up to last point.
 40   IF (IHIDE.LT.0) CALL GVEC (P1(3), P1(4), IBLK, IERR)
      IF (IHIDE.GT.0) CALL GPOS (P1(3), P1(4), IBLK, IERR)
      XL = P1(3)
      YL = P1(4)
C
 999  RETURN
      END
      SUBROUTINE PFCHAR (ILEN, HORV, X, Y, DCX, DCY, STRING, IOBLK,
     *   IERR)
C-----------------------------------------------------------------------
C   PRCHAR will generate the vector commands for characters.
C   INPUTS:   ILEN     I   number of characters in the character string
C             HORV     I   0 = draw characters horizontally, 1=vertical
C             X        R   starting X position (see DCX).
C             Y        R   starting Y position (see DCY).
C             DCX      R   X distance from starting position in
C                      characters at which to start drawing.
C             DCY      R   Y distance from starting position in
C                      characters at which to start drawing.
C             STRING   I(ILEN+1/2)   array containing ASCII characters
C             IOBLK    I(??)   I/O block for bit map file.
C   COMMON:   CSIZX    R   chracter width in pixels.
C             CSIZY    R   character height in pixels.
C             XL       R   current X coordinate in pixels.
C             YL       R   current Y coordinate in pixels.
C   OUTPUTS:  IERR     I   error code.
C                          0 = ok.
C   COMMON:   XL       I   updated X position.
C             YL       I   updated Y position.
C             IOBLK    I   updated I/O block.
C-----------------------------------------------------------------------
      CHARACTER IT*2, STRING*(*)
      REAL      DCX, DCY, YSAV, Y, XSAV, X, XLP, YLP, YC, XC, YSPACE,
     *   LDCY, LASTY, HIGHXP, MAXC
      INTEGER   IOBLK(1), HORV, I, ICHAR, IERR, ILEN, KT,
     *   INCOM, INDEX, J, K, NIT, JT, LT, BITS(16)
      INTEGER   ICHTAB(931), INDTAB(95)
      INTEGER   SPACE ( 1), EXCLAM( 6), QUOTE ( 6), POUND (13)
     *        , DOLLAR(15), PERCNT(16), AMPERS(13), APOSTR( 8)
     *        , LPAREN( 6), RPAREN( 6), ASTER (13), PLUS  (07)
     *        , COMMA ( 8), MINUS ( 4), PERIOD( 7), SLASH ( 4)
     *        , ZERO  (14), ONE   ( 8), TWO   (12), THREE (16)
     *        , FOUR  ( 7), FIVE  (12), SIX   (13), SEVEN ( 8)
     *        , EIGHT (18), NINE  (13), COLON (13), SEMICO(14)
     *        , LESS  ( 5), EQUALS( 7), GREATR( 5), QUESTN(11)
      INTEGER   ATSIGN(15), AUPPER(10), BUPPER(17), CUPPER(10)
     *        , DUPPER(11), EUPPER( 9), FUPPER( 8), GUPPER(12)
     *        , HUPPER(10), IUPPER(10), JUPPER(10), KUPPER(10)
     *        , LUPPER( 5), MUPPER( 7), NUPPER(10), OUPPER(11)
     *        , PUPPER( 9), QUPPER(14), RUPPER(12), SUPPER(14)
     *        , TUPPER( 7), UUPPER( 8), VUPPER( 7), WUPPER( 7)
     *        , XUPPER(11), YUPPER( 8), ZUPPER( 8), LBRACK( 6)
     *        , BKSLSH( 4), RBRACK( 6), CARET ( 5), USCORE( 4)
     *        , ACCENT( 7), ALOWER(15), BLOWER(13), CLOWER(10)
     *        , DLOWER(13), ELOWER(12), FLOWER( 9), GLOWER(13)
     *        , HLOWER(10), ILOWER(10), JLOWER(10), KLOWER(11)
     *        , LLOWER( 8), MLOWER(14), NLOWER(10), OLOWER(11)
     *        , PLOWER(13), QLOWER(13), RLOWER( 9), SLOWER(11)
     *        , TLOWER(10), ULOWER(10), VLOWER( 7), WLOWER(14)
     *        , XLOWER( 7), YLOWER(13), ZLOWER( 6), LBRACE(14)
     *        , ORSIGN( 4), RBRACE(14), TILDE ( 6)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'PROFL2.INC'
      EQUIVALENCE (SPACE , ICHTAB(  1)), (EXCLAM, ICHTAB(  2))
     *          , (QUOTE , ICHTAB(  8)), (POUND , ICHTAB( 14))
     *          , (DOLLAR, ICHTAB( 27)), (PERCNT, ICHTAB( 42))
     *          , (AMPERS, ICHTAB( 58)), (APOSTR, ICHTAB( 71))
     *          , (LPAREN, ICHTAB( 79)), (RPAREN, ICHTAB( 85))
     *          , (ASTER , ICHTAB( 91)), (PLUS  , ICHTAB(104))
     *          , (COMMA , ICHTAB(111)), (MINUS , ICHTAB(119))
     *          , (PERIOD, ICHTAB(123)), (SLASH , ICHTAB(130))
     *          , (ZERO  , ICHTAB(134)), (ONE   , ICHTAB(148))
     *          , (TWO   , ICHTAB(156)), (THREE , ICHTAB(168))
     *          , (FOUR  , ICHTAB(184)), (FIVE  , ICHTAB(191))
     *          , (SIX   , ICHTAB(203)), (SEVEN , ICHTAB(216))
     *          , (EIGHT , ICHTAB(224)), (NINE  , ICHTAB(242))
     *          , (COLON , ICHTAB(255)), (SEMICO, ICHTAB(268))
     *          , (LESS  , ICHTAB(282)), (EQUALS, ICHTAB(287))
     *          , (GREATR, ICHTAB(294)), (QUESTN, ICHTAB(299))
      EQUIVALENCE (ATSIGN, ICHTAB(310)), (AUPPER, ICHTAB(325))
     *          , (BUPPER, ICHTAB(335)), (CUPPER, ICHTAB(352))
     *          , (DUPPER, ICHTAB(362)), (EUPPER, ICHTAB(373))
     *          , (FUPPER, ICHTAB(382)), (GUPPER, ICHTAB(390))
     *          , (HUPPER, ICHTAB(402)), (IUPPER, ICHTAB(412))
     *          , (JUPPER, ICHTAB(422)), (KUPPER, ICHTAB(432))
     *          , (LUPPER, ICHTAB(442)), (MUPPER, ICHTAB(447))
     *          , (NUPPER, ICHTAB(454)), (OUPPER, ICHTAB(464))
     *          , (PUPPER, ICHTAB(475)), (QUPPER, ICHTAB(484))
     *          , (RUPPER, ICHTAB(498)), (SUPPER, ICHTAB(510))
     *          , (TUPPER, ICHTAB(524)), (UUPPER, ICHTAB(531))
     *          , (VUPPER, ICHTAB(539)), (WUPPER, ICHTAB(546))
     *          , (XUPPER, ICHTAB(553)), (YUPPER, ICHTAB(564))
     *          , (ZUPPER, ICHTAB(572)), (LBRACK, ICHTAB(580))
     *          , (BKSLSH, ICHTAB(586)), (RBRACK, ICHTAB(590))
     *          , (CARET , ICHTAB(596)), (USCORE, ICHTAB(601))
      EQUIVALENCE (ACCENT, ICHTAB(605)), (ALOWER, ICHTAB(612))
     *          , (BLOWER, ICHTAB(627)), (CLOWER, ICHTAB(640))
     *          , (DLOWER, ICHTAB(650)), (ELOWER, ICHTAB(663))
     *          , (FLOWER, ICHTAB(675)), (GLOWER, ICHTAB(684))
     *          , (HLOWER, ICHTAB(697)), (ILOWER, ICHTAB(707))
     *          , (JLOWER, ICHTAB(717)), (KLOWER, ICHTAB(727))
     *          , (LLOWER, ICHTAB(738)), (MLOWER, ICHTAB(746))
     *          , (NLOWER, ICHTAB(760)), (OLOWER, ICHTAB(770))
     *          , (PLOWER, ICHTAB(781)), (QLOWER, ICHTAB(794))
     *          , (RLOWER, ICHTAB(807)), (SLOWER, ICHTAB(816))
     *          , (TLOWER, ICHTAB(827)), (ULOWER, ICHTAB(837))
     *          , (VLOWER, ICHTAB(847)), (WLOWER, ICHTAB(854))
     *          , (XLOWER, ICHTAB(868)), (YLOWER, ICHTAB(875))
     *          , (ZLOWER, ICHTAB(888)), (LBRACE, ICHTAB(894))
     *          , (ORSIGN, ICHTAB(908)), (RBRACE, ICHTAB(912))
     *          , (TILDE , ICHTAB(926))
      SAVE LDCY, LASTY, HIGHXP
      DATA LDCY, LASTY, HIGHXP /-1.E10,-1.E10,-1.E10/
C-----------------------------------------------------------------------
      DATA SPACE  /00/
      DATA EXCLAM /02, 38, 33, 01, 30, 00/
      DATA QUOTE  /02, 28, 26, 02, 48, 46/
      DATA POUND  /02, 10, 18, 02, 58, 50, 02, 62, 02, 02
     *           , 06, 66, 00/
      DATA DOLLAR /10, 01, 51, 62, 63, 54, 14, 05, 06, 17
     *           , 67, 02, 38, 30, 00/
      DATA PERCNT /05, 07, 18, 27, 16, 07, 02, 01, 67, 05
     *           , 50, 61, 52, 41, 50, 00/
      DATA AMPERS /11, 60, 06, 07, 18, 48, 46, 02, 01, 10
     *           , 30, 63, 00/
      DATA APOSTR /06, 24, 46, 48, 38, 37, 47, 00/
      DATA LPAREN /04, 40, 22, 26, 48, 00/
      DATA RPAREN /04, 20, 42, 46, 28, 00/
      DATA ASTER  /02, 01, 67, 02, 07, 61, 02, 04, 64, 02
     *           , 37, 31, 00/
      DATA PLUS   /02, 14, 54, 02, 36, 32, 00/
      DATA COMMA  /06, 20, 42, 44, 34, 33, 43, 00/
      DATA MINUS  /02, 14, 54, 00/
      DATA PERIOD /05, 20, 30, 31, 21, 20, 00/
      DATA SLASH  /02, 01, 67, 00/
      DATA ZERO   /09, 10, 50, 61, 67, 58, 18, 07, 01, 10
     *           , 02, 01, 67, 00/
      DATA ONE    /02, 10, 50, 03, 30, 38, 16, 00/
      DATA TWO    /10, 07, 18, 58, 67, 65, 54, 24, 02, 00
     *           , 60, 00/
      DATA THREE  /07, 07, 18, 58, 67, 65, 54, 34, 06, 54
     *           , 63, 61, 50, 10, 01, 00/
      DATA FOUR   /05, 50, 58, 03, 02, 72, 00/
      DATA FIVE   /10, 01, 10, 40, 62, 63, 45, 05, 08, 68
     *           , 67, 00/
      DATA SIX    /11, 04, 54, 63, 61, 50, 10, 01, 06, 28
     *           , 58, 67, 00/
      DATA SEVEN  /06, 20, 23, 67, 68, 08, 07, 00/
      DATA EIGHT  /16, 14, 03, 01, 10, 50, 61, 63, 54, 14
     *           , 05, 07, 18, 58, 67, 65, 54, 00/
      DATA NINE   /11, 01, 10, 40, 62, 67, 58, 18, 07, 05
     *           , 14, 64, 00/
      DATA COLON  /05, 22, 32, 33, 23, 22, 05, 26, 36, 37
     *           , 27, 26, 00/
      DATA SEMICO /06, 10, 32, 34, 24, 23, 33, 05, 26, 36
     *           , 37, 27, 26, 00/
      DATA LESS   /03, 50, 14, 58, 00/
      DATA EQUALS /02, 12, 52, 02, 16, 56, 00/
      DATA GREATR /03, 10, 54, 18, 00/
      DATA QUESTN /07, 06, 07, 18, 58, 67, 34, 33, 01, 31
     *           , 00/
      DATA ATSIGN /13, 54, 45, 34, 43, 54, 64, 66, 48, 28
     *           , 06, 02, 20, 50, 00/
      DATA AUPPER /05, 00, 05, 38, 65, 60, 02, 03, 63, 00/
      DATA BUPPER /06, 00, 50, 61, 63, 54, 14, 05, 08, 58
     *           , 67, 65, 54, 02, 18, 10, 00/
      DATA CUPPER /08, 67, 58, 28, 06, 02, 20, 50, 61, 00/
      DATA DUPPER /06, 00, 40, 62, 66, 48, 08, 02, 18, 10
     *           , 00/
      DATA EUPPER /04, 60, 00, 08, 68, 02, 34, 04, 00/
      DATA FUPPER /03, 00, 08, 68, 02, 34, 04, 00/
      DATA GUPPER /10, 67, 58, 28, 06, 02, 20, 50, 61, 64
     *           , 44, 00/
      DATA HUPPER /02, 00, 08, 02, 60, 68, 02, 04, 64, 00/
      DATA IUPPER /02, 10, 50, 02, 30, 38, 02, 18, 58, 00/
      DATA JUPPER /05, 01, 10, 20, 31, 38, 02, 18, 58, 00/
      DATA KUPPER /02, 00, 08, 02, 68, 02, 02, 24, 60, 00/
      DATA LUPPER /03, 08, 00, 60, 00/
      DATA MUPPER /05, 00, 08, 35, 68, 60, 00/
      DATA NUPPER /02, 00, 08, 02, 07, 61, 02, 60, 68, 00/
      DATA OUPPER /09, 20, 40, 62, 66, 48, 28, 06, 02, 20
     *           , 00/
      DATA PUPPER /07, 00, 08, 58, 67, 66, 55, 05, 00/
      DATA QUPPER /09, 20, 40, 62, 66, 48, 28, 06, 02, 20
     *           , 02, 33, 60, 00/
      DATA RUPPER /07, 00, 08, 58, 67, 66, 55, 05, 02, 15
     *           , 60, 00/
      DATA SUPPER /12, 01, 10, 50, 61, 63, 54, 14, 05, 07
     *           , 18, 58, 67, 00/
      DATA TUPPER /02, 30, 38, 02, 08, 68, 00/
      DATA UUPPER /06, 08, 01, 10, 50, 61, 68, 00/
      DATA VUPPER /05, 08, 03, 30, 63, 68, 00/
      DATA WUPPER /05, 08, 00, 33, 60, 68, 00/
      DATA XUPPER /04, 00, 01, 67, 68, 04, 08, 07, 61, 60
     *           , 00/
      DATA YUPPER /03, 08, 35, 68, 02, 35, 30, 00/
      DATA ZUPPER /06, 08, 68, 67, 01, 00, 60, 00/
      DATA LBRACK /04, 40, 20, 28, 48, 00/
      DATA BKSLSH /02, 07, 61, 00/
      DATA RBRACK /04, 20, 40, 48, 28, 00/
      DATA CARET  /03, 05, 38, 65, 00/
      DATA USCORE /02,-01,-61, 00/
      DATA ACCENT /05, 27, 28, 38, 37, 55, 00/
      DATA ALOWER /05, 06, 26, 35, 31, 40, 07, 31, 20, 10
     *           , 01, 02, 13, 33, 00/
      DATA BLOWER /02, 08, 00, 08, 02, 20, 30, 41, 44, 35
     *           , 25, 03, 00/
      DATA CLOWER /08, 41, 30, 10, 01, 04, 15, 35, 44, 00/
      DATA DLOWER /02, 48, 40, 08, 42, 20, 10, 01, 04, 15
     *           , 25, 43, 00/
      DATA ELOWER /10, 40, 10, 01, 04, 15, 35, 44, 43, 32
     *           , 02, 00/
      DATA FLOWER /04, 10, 17, 28, 37, 02, 04, 24, 00/
      DATA GLOWER /11, 40, 10, 01, 04, 15, 35, 44,-41,-23
     *           ,-13,-02, 00/
      DATA HLOWER /02, 00, 08, 05, 03, 25, 35, 44, 40, 00/
      DATA ILOWER /01, 37, 03, 25, 35, 30, 02, 20, 40, 00/
      DATA JLOWER /01, 37, 06, 35,-32,-23,-13,-02,-01, 00/
      DATA KLOWER /02, 08, 00, 02, 01, 45, 03, 40, 22, 23
     *           , 00/
      DATA LLOWER /02, 20, 40, 03, 30, 38, 28, 00/
      DATA MLOWER /06, 00, 04, 15, 25, 34, 30, 05, 34, 45
     *           , 55, 64, 60, 00/
      DATA NLOWER /02, 00, 05, 05, 03, 25, 35, 44, 40, 00/
      DATA OLOWER /09, 01, 04, 15, 35, 44, 41, 30, 10, 01
     *           , 00/
      DATA PLOWER /02,-03, 05, 08, 03, 25, 35, 44, 41, 30
     *           , 20, 02, 00/
      DATA QLOWER /02,-43, 45, 08, 43, 25, 15, 04, 01, 10
     *           , 20, 42, 00/
      DATA RLOWER /02, 00, 05, 04, 03, 25, 35, 44, 00/
      DATA SLOWER /09, 00, 30, 41, 42, 33, 13, 04, 15, 45
     *           , 00/
      DATA TLOWER /02, 06, 26, 05, 18, 11, 20, 30, 41, 00/
      DATA ULOWER /05, 05, 01, 10, 20, 42, 02, 40, 45, 00/
      DATA VLOWER /05, 05, 02, 20, 42, 45, 00/
      DATA WLOWER /06, 05, 01, 10, 20, 31, 35, 05, 31, 40
     *           , 50, 61, 65, 00/
      DATA XLOWER /02, 00, 55, 02, 05, 50, 00/
      DATA YLOWER /05, 05, 01, 10, 30, 41, 05, 45,-42,-33
     *           ,-23,-12, 00/
      DATA ZLOWER /04, 05, 55, 00, 50, 00/
      DATA LBRACE /06, 40, 30, 21, 23, 14, 04, 05, 14, 25
     *           , 27, 38, 48, 00/
      DATA ORSIGN /02, 30, 38, 00/
      DATA RBRACE /06, 20, 30, 41, 43, 54, 64, 05, 54, 45
     *           , 47, 38, 28, 00/
      DATA TILDE  /04, 06, 28, 46, 68, 00/
C
      DATA INDTAB /  1,   2,   8,  14,  27,  42,  58,  71,  79,  85,
     *              91, 104, 111, 119, 123, 130, 134, 148, 156, 168,
     *             184, 191, 203, 216, 224, 242, 255, 268, 282, 287,
     *             294, 299, 310, 325, 335, 352, 362, 373, 382, 390,
     *             402, 412, 422, 432, 442, 447, 454, 464, 475, 484,
     *             498, 510, 524, 531, 539, 546, 553, 564, 572, 580,
     *             586, 590, 596, 601, 605, 612, 627, 640, 650, 663,
     *             675, 684, 697, 707, 717, 727, 738, 746, 760, 770,
     *             781, 794, 807, 816, 827, 837, 847, 854, 868, 875,
     *             888, 894, 908, 912, 926 /
      DATA YSPACE /.8/
C-----------------------------------------------------------------------
      IERR = 0
      IF (ILEN.LE.0) GO TO 999
      YSAV = Y + CSIZY * DCY
      XSAV = X + CSIZX * DCX
      MAXC = -1.E10
      IF ((DCY.NE.LDCY) .OR. (Y.NE.LASTY)) HIGHXP = MAXC
C                                       Loop for each character.
      DO 100 I = 1,ILEN
C                                       Get standard ASCII char
C                                       in machine inde. fashion.
         JT = NBITWD / 8
         IT(1:1) = STRING(I:I)
         CALL ZCLC8 (1, IT, JT, LT)
         CALL ZI32IL (1, 1, LT, KT)
         NIT = NBITWD - (JT-1) * 8
         CALL ZGTBIT (NIT, KT, BITS)
         CALL ZPTBIT (8, ICHAR, BITS(NIT-7))
C                                       Set orgion & check validity.
         ICHAR = ICHAR - 31
         IF ((ICHAR.LT.1) .OR. (ICHAR.GT.95)) ICHAR = 32
C                                       Find index into table of vectrs
         INDEX = INDTAB(ICHAR)
C                                       DO WHILE no. of commands not 0.
         DO 50 J = 1,99
            INCOM = ICHTAB(INDEX)
            IF (INCOM.EQ.0) GO TO 60
C                                       Loop for all vector commands.
            XLP = XSAV
            YLP = YSAV
            DO 30 K = 1,INCOM
               INDEX = INDEX + 1
C                                       Calc X and Y values.
               YC = ((MOD(ICHTAB(INDEX), 10) * CSIZY) / 10.0) * YSPACE
     *            + YSAV
               XC = ABS (ICHTAB(INDEX)/10 * CSIZX) / 10.0  +  XSAV
               CALL PFXY (XC, YC, Z0, XC, YC)
               IF ((I.EQ.1) .AND. (LDCY.EQ.DCY) .AND. (LASTY.EQ.Y) .AND.
     *            (XC.LE.HIGHXP)) GO TO 900
               IF (K.EQ.1) CALL GPOS (XC, YC, IOBLK, IERR)
               IF (K.NE.1) CALL GVEC (XC, YC, IOBLK, IERR)
               IF (IERR.NE.0) GO TO 999
               MAXC = MAX (MAXC, XC)
 30            CONTINUE
            INDEX = INDEX + 1
 50         CONTINUE
C                                       Set last X and Y.
 60      IF (HORV.EQ.1) GO TO 70
C                                       Horizontal.
            XSAV = XSAV + CSIZX
            GO TO 100
C                                       Vertical.
 70         YSAV = YSAV - CSIZY
C
 100     CONTINUE
C
 900  HIGHXP = MAX (HIGHXP, MAXC)
      LDCY = DCY
      LASTY = Y
C
 999  RETURN
      END
      SUBROUTINE PFSTAR (FACTOR, IVOL, CNO, VERS, BLC, TRC, PLBUF, IERR)
C-----------------------------------------------------------------------
C   PFSTAR plots star positions in a plot file as given by an ST
C   extension file of version VERS.  The ST file contains the center
C   position (RA-DEC, GLON-GLAT, ELON-ELAT) of each star and the
C   "uncertainties" in those star positions.  The plotted plus signs
C   are scaled by these uncertainties and then further scaled by
C   multiplying by FACTOR.
C   Inputs: FACTOR  R         Star scaling factor: <= 0 => no plot.
C           VERS    I         Desired ST file version number: 0 => high
C           IVOL    I         File disk number
C           CNO     I         File catalog number
C           BLC     R(2)      Plot lower left corner (pixels)
C           TRC     R(2)      Plot upper right corner (pixels)
C   In/Out: PLBUF   I(256)    Plot IO buffer
C   Output: IERR    I         Error code: 0 => okay
C                                -1 => there was no ST file
C                                +1 => logical error in ST file
C                                +2 => IO error in ST file
C                                +3 => IO error in plotting
C   Common: /MAPHDR/ CATBLK   Image header having the ST file
C-----------------------------------------------------------------------
      REAL      FACTOR, BLC(2), TRC(2)
      INTEGER   IVOL, CNO, VERS, PLBUF(256), IERR
C
C                                       Max Numb Columns, Label length
      INTEGER MXSTCL, MXSTLB, MXLINE
      PARAMETER (MXSTCL=7, MXSTLB=24, MXLINE=72)
      CHARACTER KEYS(MXSTCL)*8
      HOLLERITH STLABL(6)
      INTEGER   DATP(128,2), IOBUF(512), IV, I, NKEY, NCOL, NREC,
     *   TABLUN, KOLS(MXSTCL), IER, ITYPE, NPL, SCRTCH(50), NST,
     *   IST, LRNO, STTYPE, IROUND
      REAL      DELJ, DELN, AX(5), AY(5), POSANG, TX
      REAL      DELX, DELY, XR, YR
      DOUBLE PRECISION XPOS, YPOS, DX, DY, COSPA, SINPA, COSDEC
      LOGICAL   LORDER
      CHARACTER*24 LABEL
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'PROFL2.INC'
      DATA TABLUN /27/
      DATA LORDER /.TRUE./
C-----------------------------------------------------------------------
      IERR = 0
      IF (FACTOR.LE.0.0) GO TO 999
C                                       Is there an ST file
      IERR = -1
      CALL FNDEXT ('ST', CATBLK, I)
      IF (I.LE.0) GO TO 999
      IV = VERS
      IF ((IV.LE.0) .OR. (IV.GT.I)) IV = I
C                                       Open ST table file
      NKEY = 0
      NCOL = 0
      NREC = 0
      CALL TABINI ('READ', 'ST', IVOL, CNO, IV, CATBLK, TABLUN, NKEY,
     *   NREC, NCOL, DATP, IOBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'ST', IV
         GO TO 990
         END IF
      NST = IOBUF(5)
C                                       Find needed columns
      I = 2
      CALL H2CHR (8, 1, CATH(KHCTP), KEYS(1))
      CALL H2CHR (8, 1, CATH(KHCTP+I), KEYS(2))
      KEYS(3) = 'MAJOR AX'
      KEYS(4) = 'MINOR AX'
      KEYS(5) = 'POSANG'
      KEYS(6) = 'STARTYPE'
      KEYS(7) = 'LABEL'
      CALL FNDCOL (NCOL, KEYS, 8, LORDER, IOBUF, KOLS, IER)
      IF (IER.GT.0) THEN
         IF (IER.LE.10) GO TO 975
            IERR = 1
            WRITE (MSGTXT,1020)
            GO TO 980
         END IF
C                                       I guess we can do it now
      NPL = 0
      LRNO = 0
      CALL GLTYPE (4, PLBUF, IER)
      IF (IER.NE.0) GO TO 985
      DO 100 IST = 1,NST
         CALL GETCOL (IST, KOLS(1), DATP, LRNO, IOBUF, ITYPE, XPOS,
     *      SCRTCH, IER)
         IF (IER.LT.0) GO TO 100
         IF (IER.NE.0) GO TO 975
         CALL GETCOL (IST, KOLS(2), DATP, LRNO, IOBUF, ITYPE, YPOS,
     *      SCRTCH, IER)
         IF (IER.NE.0) GO TO 975
         CALL GETCOL (IST, KOLS(3), DATP, LRNO, IOBUF, ITYPE, DELX,
     *      SCRTCH, IER)
         IF (IER.NE.0) GO TO 975
         CALL GETCOL (IST, KOLS(4), DATP, LRNO, IOBUF, ITYPE, DELY,
     *      SCRTCH, IER)
         IF (IER.NE.0) GO TO 975
         CALL GETCOL (IST, KOLS(3), DATP, LRNO, IOBUF, ITYPE, DELJ,
     *      SCRTCH, IER)
         IF (IER.NE.0) GO TO 975
         CALL GETCOL (IST, KOLS(4), DATP, LRNO, IOBUF, ITYPE, DELN,
     *      SCRTCH, IER)
         IF (IER.NE.0) GO TO 975
         POSANG = 0.0
         IF (NCOL.GT.4) THEN
            CALL GETCOL (IST, KOLS(5), DATP, LRNO, IOBUF, ITYPE, POSANG,
     *      SCRTCH, IER)
            END IF
         STTYPE = 0
C                                       If a Star type column
         IF (NCOL.GT.5) THEN
            CALL GETCOL (IST, KOLS(6), DATP, LRNO, IOBUF, ITYPE, TX,
     *         SCRTCH, IER)
            STTYPE = IROUND (TX)
            END IF
         LABEL = ' '
C                                       If a Star Label
         IF (NCOL.GT.6) THEN
            CALL GETCOL (IST, KOLS(7), DATP, LRNO, IOBUF, ITYPE, STLABL,
     *         SCRTCH, IER)
C                                       If label is right length
            IF (ITYPE.EQ.(MXSTLB*10)+3) THEN
               CALL H2CHR( MXSTLB, 1, STLABL, LABEL)
            ELSE
               LABEL = ' '
               END IF
            END IF
C                                       Convert to radians
         POSANG = POSANG*TWOPI/360.0
         COSPA = COS(POSANG)
         SINPA = SIN(POSANG)
         COSDEC = ABS (COS (YPOS*DG2RAD))
         CALL XYPIX (XPOS, YPOS, AX(1), AY(1), IER)
C                                       If star not in plot, get next
         IF ((BLC(1).GT.AX(1)) .OR. (BLC(2).GT.AY(1)) .OR.
     *       (TRC(1).LT.AX(1)) .OR. (TRC(2).LT.AY(1))) GO TO 100
C                                       Move and Put Text on plot
C                                       Calculate ends of star mark
         DELJ = 0.5 * DELJ * ABS(FACTOR)
         DELN = 0.5 * DELN * ABS(FACTOR)
         IF (CORTYP(LOCNUM).EQ.1) THEN
            COSDEC = ABS (COS (YPOS*DG2RAD))
            DX = XPOS + DELJ*SINPA/COSDEC
            DY = YPOS + DELJ*COSPA
         ELSE IF (CORTYP(LOCNUM).EQ.2) THEN
            COSDEC = ABS (COS (XPOS*DG2RAD))
            DX = XPOS + DELJ*COSPA
            DY = YPOS + DELJ*SINPA/COSDEC
         ELSE
            DX = XPOS
            DY = YPOS + DELJ
            END IF
         CALL XYPIX (DX, DY, AX(2), AY(2), IER)
         IF (IER.NE.0) GO TO 100
         DX = 2.0 * XPOS - DX
         DY = 2.0 * YPOS - DY
         CALL XYPIX (DX, DY, AX(3), AY(3), IER)
         IF (IER.NE.0) GO TO 100
         IF (CORTYP(LOCNUM).EQ.1) THEN
            DX = XPOS - DELN*COSPA/COSDEC
            DY = YPOS + DELN*SINPA
         ELSE IF (CORTYP(LOCNUM).EQ.2) THEN
            DX = XPOS + DELN*SINPA
            DY = YPOS - DELN*COSPA/COSDEC
         ELSE
            DX = XPOS - DELN
            DY = YPOS
            END IF
         CALL XYPIX (DX, DY, AX(4), AY(4), IER)
         IF (IER.NE.0) GO TO 100
         DX = 2.0 * XPOS - DX
         DY = 2.0 * YPOS - DY
         CALL XYPIX (DX, DY, AX(5), AY(5), IER)
         IF (IER.NE.0) GO TO 100
         CALL LINLIM (BLC, TRC, AX(2), AY(2), IER)
         IF (IER.NE.0) GO TO 100
         CALL LINLIM (BLC, TRC, AX(4), AY(4), IER)
         IF (IER.NE.0) GO TO 100
         IERR = 3
         CALL PFXY (AX(2), AY(2), Z0, XR, YR)
         CALL GPOS (XR, YR, PLBUF, IER)
         IF (IER.NE.0) GO TO 985
         CALL PFXY (AX(4), AY(4), Z0, XR, YR)
         CALL GVEC (XR, YR, PLBUF, IER)
         IF (IER.NE.0) GO TO 985
         CALL PFXY (AX(3), AY(3), Z0, XR, YR)
         CALL GVEC (XR, YR, PLBUF, IER)
         IF (IER.NE.0) GO TO 985
         CALL PFXY (AX(5), AY(5), Z0, XR, YR)
         CALL GVEC (XR, YR, PLBUF, IER)
         IF (IER.NE.0) GO TO 985
         CALL PFXY (AX(2), AY(2), Z0, XR, YR)
         CALL GVEC (XR, YR, PLBUF, IER)
         IF (IER.NE.0) GO TO 985
         NPL = NPL + 1
 100     CONTINUE
      IERR = 0
      WRITE (MSGTXT,1100) NPL, IV
      CALL MSGWRT (2)
      GO TO 985
C                                       Error print and close
 975  WRITE (MSGTXT,1975) IER
      IERR = 2
 980  CALL MSGWRT (8)
 985  CALL TABIO ('CLOS', I, IST, SCRTCH, IOBUF, IER)
      GO TO 999
C                                       Error print
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' OPENING ',A2,' FILE VERSION',I4)
 1020 FORMAT ('CANNOT FIND COLUMNS IN STAR TABLE: HAS IMAGE',
     *   ' BEEN TRANSPOSED?')
 1100 FORMAT ('Plotted',I5,' positions from ST table version',I4)
 1975 FORMAT ('IO ERROR',I5,' IN STAR TABLE FILE')
      END
