      PROGRAM CNTR
C-----------------------------------------------------------------------
C! Generates contour plot files for images
C# Graphics Map-util Plot-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2002-2004, 2006, 2009, 2011, 2014-2015
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
C   CNTR will write commands to a plot file for the execution
C   of a contour plot for a cataloged image file.  The program
C   runs as a detached task initiated from AIPS. First a cataloged
C   file is found using data passed from AIPS.  The list of
C   associated files is searched for PLot files to find the highest
C   version number.  Then a PLot file is created for this map and the
C   catalog header is updated.  Next the graphics commands are
C   written to the plot file.  TELL 'ABOR' and 'QUIT' codes supported.
C   Inputs:   (from AIPS)
C      USERID   R      user number, 0 means use logon user
C                      number, 32000 means any user can be accessed.
C      INNAME   H(3)   name of primary file.
C      INCLASS  H(2)   class of primary file.
C      INSEQ    R      sequence number of primary file.
C      INDISK   R      disk volume number. 0 means try all.
C      BLC      R(7)   bottom left corner of subimage: x, y, z, ...
C      TRC      R(7)   top right hand corner of plotted subimage
C      XYRATIO  R      the ratio between the scale factor to use for the
C                      X axis and the scale factor to use for the Y axis
C      LTYPE    R      the type of axis labeling to use for this plot.
C                          1 = no labels. Make map as big as possible.
C                          2 = no ticks, do rest of labels
C                          3 = RA - DEC coordinates & labels
C                          4 = Center-relative units
C      PLEV     R      the percentage of the peak value to use as the
C                      multiplier for the contour levels.  If 0 use CLEV
C      CLEV     R      The absolute value of the multiplier used for the
C                      contour levels.  Used only if PLEV is zero.
C      LEVS     R(30)  the contour levels.  An out of sequence level
C                      indicates 'end of levels'.  The real value of a
C                      particular level is the LEV value times CLEV or
C                      the value determined by PLEV.
C      DOCIRCLE R      > 0 Plot coord grid rather than just ticks
C      INVERS   R      ST file version number.
C      STFACTOR R      scale star sizes in file for plotting:
C                         0 => no plot of stars.
C      DOTV     R      > 0 => TV, else plot file
C      GRCHAN   R      graphics channel to use
C      TVCORN   R(2)   TV pixel to use (both > 0 => pixel scale)
C-----------------------------------------------------------------------
      CHARACTER IGFILE*48, PRGNAM*6, CHTM12*12, CHTM6*6, TYPIN*2,
     *   OPCODE*4, TXTMSG*80
      REAL      BLC(7), LEVS(30), TRC(7), DOCIRC, CH(4), CLEV, DSKIN,
     *   MULT, STMULT, PEAK, PLEV, PRUSER, SEQIN, XYRATO, TLABEL,
     *   YGAP, XINVER, XDOTV, XGRCH, XTVCRN(2), DUM
      HOLLERITH NAMIN(3), CLSIN(2)
      INTEGER   I, PLBUF(256), IGLUN, IGFIND, IGSIZE, ILABEL, IMFIND,
     *   IMLUN, IERR, IRETCD, ISEQ, J, INPRMS, ISLOT, ITYPE, IUSER,
     *   IVER, IVOL, IROUND, INVER, IDEPTH(5), GRCHN, TVCHN, TVCORN(2),
     *   BBLC(2), BTRC(2)
      LOGICAL   NOSAVE, QUICK, SAVE, T, DOGRID, DOTV
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DCNT.INC'
      COMMON /INPARM/ PRUSER, NAMIN, CLSIN, SEQIN, DSKIN, BLC, TRC,
     *   XYRATO, TLABEL, PLEV, CLEV, LEVS, DOCIRC, XINVER, STMULT,
     *   XDOTV, XGRCH, XTVCRN
      DATA IMLUN, IGLUN /16, 26/
      DATA PRGNAM /'CNTR  '/
      DATA TYPIN /'  '/
      DATA NOSAVE, SAVE, T /.FALSE.,.TRUE.,.TRUE./
      DATA BBLC, BTRC /4 * 0/
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Get input values from AIPS.
C                                       CHECK THIS WITH INPARM COMMON!
C                                       You HAVE to get this right.
C                                       It was incorrectly set to 65.
      INPRMS = 63
      IRETCD = 0
      CALL GTPARM (PRGNAM, INPRMS, QUICK, PRUSER, PLBUF, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         IRETCD = 8
 10   IF (QUICK) CALL RELPOP (IRETCD, PLBUF, IERR)
      IF (IRETCD.NE.0) GO TO 995
      IRETCD = 8
      PRUSER = NLUSER
C                                       Hollerith -> Char
      CALL H2CHR (12, 1, NAMIN, CHTM12)
      CALL H2CHR (6, 1, CLSIN, CHTM6)
C
      DOGRID = DOCIRC.GT.0.0
      ISEQ = IROUND (SEQIN)
      IVOL = IROUND (DSKIN)
      ILABEL = IROUND (TLABEL)
      I = MOD (ABS(ILABEL), 100)
      IF ((I.LE.0) .OR. (I.GT.10)) THEN
         IF (ILABEL.GE.0) THEN
            ILABEL = (ILABEL/100)*100 + 3
         ELSE
            ILABEL = (ILABEL/100)*100 - 3
            END IF
         END IF
      TLABEL = ILABEL
      IUSER = NLUSER
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      TVCORN(1) = IROUND (XTVCRN(1))
      TVCORN(2) = IROUND (XTVCRN(2))
C                                       Levels defaults
      IF ((CLEV.EQ.0.0) .AND. (PLEV.EQ.0.0)) PLEV = 10.0
      IF ((LEVS(1).EQ.0.0) .AND. (LEVS(2).LE.LEVS(1))) THEN
         DO 15 I = 1,10
            LEVS(I) = I-11
            LEVS(I+10) = I
            LEVS(I+20) = 0.
 15         CONTINUE
         END IF
C                                       Open map file & get header.
      OPCODE = 'HDWR'
      IF (DOTV) OPCODE = 'READ'
      CALL MAPOPN (OPCODE, IVOL, CHTM12, CHTM6, ISEQ, TYPIN, IUSER,
     *   IMLUN, IMFIND, ISLOT, CATBLK, PLBUF, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Add extension file to header.
      IVER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', IVOL, ISLOT, CATBLK, PLBUF, SAVE,
     *      'READ', IVER, IERR)
         IF (IERR.NE.0) GO TO 975
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
C                                       Get header info.
      PEAK = MAX (ABS(CATR(KRDMX)), ABS(CATR(KRDMN)))
      MULT = CLEV
      IF (PLEV.NE.0.0) MULT = PEAK * PLEV / 100.0
C                                       Build file name.
      CALL ZPHFIL ('PL', IVOL, ISLOT, IVER, IGFILE, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020)
         CALL MSGWRT (7)
         GO TO 970
         END IF
C                                       Init map for read.
      I = 2 * MABFSS
      CALL DBINIT (IMLUN, IMFIND, CATBLK, BLC, TRC, I, BUFF, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       fill in defaults in PARMS
      DSKIN = IVOL
      CALL RCOPY (5, BLC(3), TRC(3))
C                                       Default XYRATO: ratio of
C                                       incr if related.
      DO 30 I = 1,5
         IDEPTH(I) = BLC(I+2) + 0.01
 30      CONTINUE
      LOCNUM = 1
      IF ((XYRATO.LE.0.01) .OR. (XYRATO.GT.320.0)) THEN
         CALL SETLOC (IDEPTH, T)
         IF ((AXTYP(LOCNUM).EQ.1) .AND. (AXINC(2,LOCNUM).NE.0.0)) XYRATO
     *      = ABS (AXINC(1,LOCNUM) / AXINC(2,LOCNUM))
         IF (((XYRATO.LE.0.04) .OR. (XYRATO.GT.25.)) .AND.
     *      (TRC(1).NE.BLC(1))) XYRATO = (TRC(2)-BLC(2)) /
     *      (TRC(1)-BLC(1))
         IF ((XYRATO.LE.0.04) .OR. (XYRATO.GT.25.)) XYRATO = 1.0
         END IF
C                                       Init graph file.
      IGSIZE = 1
      ITYPE = 2
      CALL GINIT (IVOL, ISLOT, IGFILE, IGSIZE, ITYPE, INPRMS, PRUSER,
     *   DOTV, TVCHN, GRCHN, TVCORN, CATBLK, PLBUF, IGLUN, IGFIND,
     *   IERR)
      IF (IERR.EQ.0) GO TO 35
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (7)
         GO TO 970
C                                       extra border chars - none
 35   CALL RFILL (4, 0.0, CH)
      YGAP = 0.0
C                                       Write axis labeling commands.
      CALL COMLAB (BLC, TRC, ILABEL, IVER, YGAP, CH, MULT, LEVS, XYRATO,
     *   PLBUF, IERR)
      IF (IERR.NE.0) GO TO 960
      CALL CLAB1 (BLC, TRC, CH, ILABEL, XYRATO, DOGRID, PLBUF, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       Draw stars
      CALL SETLOC (IDEPTH, T)
      CALL GLTYPE (4, PLBUF, IERR)
      IF (IERR.NE.0) GO TO 960
      CALL STARPL (STMULT, IVOL, ISLOT, INVER, BLC, TRC, 0, 0, CATBLK,
     *   BLC, 1, PLBUF, IERR)
      IF (IERR.GE.3) GO TO 950
C                                       Draw contours.
      TXTMSG = 'End labeling, start contouring'
      CALL GCOMNT (2, TXTMSG, PLBUF, IERR)
      IF (IERR.NE.0) GO TO 950
      CALL GLTYPE (2, PLBUF, IERR)
      IF (IERR.NE.0) GO TO 960
      CALL CONDRW (IMLUN, IMFIND, 0, 0, MULT, BLC, TRC, LEVS, 0, BBLC,
     *   BTRC, DUM, PLBUF, IERR)
      IF (IERR.GT.9) GO TO 960
      IF (IERR.NE.0) GO TO 950
C                                       Write sucessful finish message.
      CALL GFINIS (PLBUF, IERR)
      IF (IERR.EQ.0) IRETCD = 0
      IF (IERR.NE.0) GO TO 960
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (IVOL, ISLOT, IVER, PLBUF, IERR)
            WRITE (MSGTXT,1300) IVER
            CALL MSGWRT (2)
            END IF
         GO TO 980
C-----------------------------------------------------------------------
C                                       Graph writing error.
 950  WRITE (MSGTXT,1950)
      CALL MSGWRT (8)
C                                       Try to do finish.
      CALL GFINIS (PLBUF, IERR)
      IF (IERR.NE.0) GO TO 960
         IRETCD = 0
         IF (.NOT.DOTV) CALL HIPLOT (IVOL, ISLOT, IVER, PLBUF, IERR)
         GO TO 980
C                                       Finish not sucessful. Destroy.
 960  CALL ZCLOSE (IGLUN, IGFIND, IERR)
      CALL ZDESTR (IVOL, IGFILE, IERR)
C                                       Do not save updated header.
 970  IRETCD = 16
      IF (.NOT.DOTV) CALL DELEXT ('PL', IVOL, ISLOT, 'READ', CATBLK,
     *   PLBUF, IVER, IERR)
 975  CALL ZCLOSE (IMLUN, IMFIND, IERR)
      GO TO 995
C                                       Close map file.
 980  CALL MAPCLS ('READ', IVOL, ISLOT, IMLUN, IMFIND, CATBLK, NOSAVE,
     *   PLBUF, IERR)
C
 995  CALL DIETSK (IRETCD, QUICK, PLBUF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR GETTING PARAMETERS FROM AIPS. GTPARM ERR =',I5)
 1020 FORMAT ('COULD NOT BUILD GRAPH FILE NAME')
 1030 FORMAT ('GRAPH FILE INIT ERROR. GINIT ERR =',I5)
 1300 FORMAT ('Successful plot file version',I5,'  created.')
 1950 FORMAT ('ERROR DURING GRAPHING WILL TRY TO FINISH PARTIAL GRAPH')
      END
