      PROGRAM CCNTR
C-----------------------------------------------------------------------
C! Generates contour plot files for images, overplots CC points
C# Graphics Map-util Plot-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2002-2004, 2006, 2009, 2012
C;  Copyright (C) 2014-2015
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
C   CCNTR will write commands to a plot file for the execution of a
C   contour plot for a cataloged image file.  The program runs as a
C   detached task initiated from AIPS. First a cataloged file is found
C   using data passed from AIPS.  The list of associated files is
C   searched for PLot files to find the highest version number.  Then a
C   PLot file is created for this map and the catalog header is updated.
C   Next the graphics commands are written to the plot file.  TELL
C   'ABOR' and 'QUIT' codes supported.  Differs from CNTR in that it
C   will overplot samples from a CC file.
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
     *  INEXTD*2, OPCODE*4, TXTMSG*80
      REAL      BLC(7), LEVS(30), TRC(7), DOCIRC, CH(4), CLEV, DSKIN,
     *   MULT, STMULT, PEAK, PLEV, PRUSER, SEQIN, XYRATO, TLABEL, YGAP,
     *   XINVER, XEXTD, XCTYPE(4), XDOTV, XGRCH, XTVCRN(2), PIXR(2),
     *   FCUT, FSCALE
      HOLLERITH NAMIN(3), CLSIN(2)
      INTEGER   I, PLBUF(256), IGLUN, IGFIND, IGSIZE, ILABEL, IMFIND,
     *   IMLUN, IERR, IRETCD, ISEQ, J, INPRMS, ISLOT, ITYPE, IUSER,
     *   IVER, IVOL, IROUND, INVER, CTYPE, GRCHN, TVCHN, TVCORN(2), NCC,
     *   IDEPTH(5), BBLC(2), BTRC(2), DUM
      LOGICAL   NOSAVE, QUICK, SAVE, T, DOGRID, DOTV
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DCNT.INC'
      COMMON /INPARM/ PRUSER, NAMIN, CLSIN, SEQIN, DSKIN, BLC, TRC,
     *   XYRATO, TLABEL, PLEV, CLEV, LEVS, DOCIRC, XEXTD, XINVER,
     *   XCTYPE, FCUT, FSCALE, STMULT, XDOTV, XGRCH, XTVCRN
      DATA IMLUN, IGLUN /16, 26/
      DATA PRGNAM /'CCNTR '/
      DATA TYPIN /'  '/
      DATA NOSAVE, SAVE, T /.FALSE.,.TRUE.,.TRUE./
      DATA BBLC, BTRC /4 * 0/
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Get input values from AIPS.
      INPRMS = 70
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
      CALL H2CHR (2, 1, XEXTD, INEXTD)
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
      CTYPE = IROUND (XCTYPE(1))
      CTYPE = MAX (0, CTYPE)
      IF (CTYPE.GT.21) CTYPE = 1
      XCTYPE(1) = CTYPE
      NCC = IROUND (XCTYPE(2))
      PIXR(1) = XCTYPE(3)
      PIXR(2) = XCTYPE(4)
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      TVCORN(1) = IROUND (XTVCRN(1))
      TVCORN(2) = IROUND (XTVCRN(2))
      FCUT = MAX (0.0, FCUT)
C                                       Levels defaults
      IF ((CLEV.EQ.0.0) .AND. (PLEV.EQ.0.0)) PLEV = 10.0
      IF ((LEVS(1).NE.0.0) .OR. (LEVS(2).GT.LEVS(1))) GO TO 20
         DO 15 I = 1,10
            LEVS(I) = I-11
            LEVS(I+10) = I
            LEVS(I+20) = 0.
 15         CONTINUE
C                                       Open map file & get header.
 20   OPCODE = 'HDWR'
      IF (DOTV) OPCODE = 'READ'
      CALL MAPOPN (OPCODE, IVOL, CHTM12, CHTM6, ISEQ, TYPIN, IUSER,
     *   IMLUN, IMFIND, ISLOT, CATBLK, PLBUF, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Add extension file to header.
      IVER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', IVOL, ISLOT, CATBLK, PLBUF, SAVE, 'READ',
     *      IVER, IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
C                                       Get header info.
      PEAK = MAX (ABS(CATR(KRDMX)), ABS(CATR(KRDMN)))
      MULT = CLEV
      IF (PLEV.NE.0.0) MULT = PEAK * PLEV / 100.0
      IF (MULT.LE.0.0) THEN
         MULT = PEAK / 10.0
         CLEV = MULT
         END IF
C                                       Build file name.
      CALL ZPHFIL ('PL', IVOL, ISLOT, IVER, IGFILE, IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1020)
         CALL MSGWRT (7)
         GO TO 970
C                                       Init map for read.
 30   I = 2 * MABFSS
      CALL DBINIT (IMLUN, IMFIND, CATBLK, BLC, TRC, I, BUFF, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       fill in defaults in PARMS
      DSKIN = IVOL
      CALL RCOPY (5, BLC(3), TRC(3))
C                                       Default XYRATO: ratio of
C                                       incr if related.
      LOCNUM = 1
      DO 31 I = 1,5
         IDEPTH(I) = BLC(I+2) + 0.01
 31      CONTINUE
      IF ((XYRATO.GT.0.01) .AND. (XYRATO.LE.320.0)) GO TO 32
         CALL SETLOC (IDEPTH, T)
         IF ((AXTYP(LOCNUM).EQ.1) .AND. (AXINC(2,LOCNUM).NE.0.0)) XYRATO
     *      = ABS (AXINC(1,LOCNUM) / AXINC(2,LOCNUM))
         IF (((XYRATO.LE.0.04) .OR. (XYRATO.GT.25.)) .AND.
     *      (TRC(1).NE.BLC(1))) XYRATO = (TRC(2)-BLC(2)) /
     *      (TRC(1)-BLC(1))
         IF ((XYRATO.LE.0.04) .OR. (XYRATO.GT.25.)) XYRATO = 1.0
C                                       Init graph file.
 32   IGSIZE = 1
      ITYPE = 37
      CALL GINIT (IVOL, ISLOT, IGFILE, IGSIZE, ITYPE, INPRMS, PRUSER,
     *   DOTV, TVCHN, GRCHN, TVCORN, CATBLK, PLBUF, IGLUN, IGFIND,
     *   IERR)
      IF (IERR.EQ.0) GO TO 35
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (7)
         GO TO 970
C                                       extra border chars
 35   CALL RFILL (4, 0.0, CH)
      YGAP = 0.0
C                                       Write axis labeling commands.
      CALL COMLAB (BLC, TRC, ILABEL, IVER, YGAP, CH, MULT, LEVS, XYRATO,
     *   PLBUF, IERR)
      IF (IERR.NE.0) GO TO 960
      CALL CLAB1 (BLC, TRC, CH, ILABEL, XYRATO, DOGRID, PLBUF, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       Draw stars
C                                       check ST or CC plot parms
      I = 0
      IF ((STMULT.GE.0.0) .AND. (STMULT.LT.0.1)) STMULT = 1.0
      IF ((STMULT.LT.0.0) .AND. (STMULT.GT.-0.1)) STMULT = -1.0
      IF (INEXTD.EQ.'ST') THEN
         CALL FNDEXT ('ST', CATBLK, I)
      ELSE IF ((INEXTD.EQ.'CC') .OR. (INEXTD.EQ.'MF')) THEN
         CALL FNDEXT (INEXTD, CATBLK, I)
         END IF
      IF (I.GT.0) THEN
         J = XINVER + 0.1
         IF (J.LE.0) J = I
         XINVER = J
         INVER = IROUND (XINVER)
         CALL SETLOC (IDEPTH, .TRUE.)
         CALL GLTYPE (4, PLBUF, IERR)
         IF (IERR.NE.0) GO TO 960
         IF (INEXTD.EQ.'ST') THEN
            CALL STARPL (STMULT, IVOL, ISLOT, INVER, BLC, TRC, 0, 0,
     *         CATBLK, BLC, 1, PLBUF, IERR)
            IF (IERR.GE.3) GO TO 950
         ELSE IF ((INEXTD.EQ.'CC') .OR. (INEXTD.EQ.'MF')) THEN
            TXTMSG = 'Start drawing Clean components'
            CALL GCOMNT (2, TXTMSG, PLBUF, IERR)
            IF (IERR.NE.0) GO TO 950
            CALL CCMFPL (INEXTD, STMULT, CTYPE, IVOL, ISLOT, INVER, NCC,
     *         PIXR, BLC, TRC, FCUT, FSCALE, PLBUF, IERR)
            IF (IERR.GE.3) GO TO 950
            END IF
         END IF
      WRITE (MSGTXT,1050)
      CALL MSGWRT (1)
C                                       Draw contours.
      TXTMSG = 'Start contouring'
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
      CALL DELEXT ('PL', IVOL, ISLOT, 'READ', CATBLK, PLBUF, IVER,
     *   IERR)
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
 1050 FORMAT ('End labeling, start contouring')
 1300 FORMAT ('Successful plot file version',I5,'  created.')
 1950 FORMAT ('ERROR DURING GRAPHING WILL TRY TO FINISH PARTIAL GRAPH')
      END
      SUBROUTINE CCMFPL (INEXTD, FACTOR, CTYPE, IVOL, CNO, VERS, NCC,
     *   PIXR, BLC, TRC, FCUT, FSCALE, PLBUF, IERR)
C-----------------------------------------------------------------------
C   CCMFPL plots CC positions in a plot file as given by an CC
C   extension file of version VERS.
C   Inputs:
C      INEXTD   C*2      'CC' or 'MF' - file type
C      FACTOR   R        Star scaling factor: <= 0 => no plot.
C      CTYPE    I        Symbol type
C      IVOL     I        File disk number
C      CNO      I        File catalog number
C      VERS     I        Desired ST file version number: 0 => high
C      NCC      I        Maximum number CC components
C      PIXR     R(2)     CC value range to scale symbol choice
C      BLC      R(2)     Plot lower left corner (pixels)
C      TRC      R(2)     Plot upper right corner (pixels)
C      FCUT     R        Only plot > FCUT in abs value
C      FSCALE   R        Scale relative to this flux if > 0
C   In/Out:
C      PLBUF    I(256)   Plot IO buffer
C   Output:
C      IERR    I         Error code: 0 => okay
C                                -1 => there was no ST file
C                                +1 => logical error in ST file
C                                +2 => IO error in ST file
C                                +3 => IO error in plotting
C   Common:
C      /MAPHDR/ CATBLK input   Image header having the ST file
C-----------------------------------------------------------------------
      CHARACTER INEXTD*2
      REAL      FACTOR, PIXR(2), BLC(2), TRC(2), FCUT, FSCALE
      INTEGER   CTYPE, IVOL, CNO, VERS, NCC, PLBUF(256), IERR
C
C                                       Max Numb Columns, Label length
      INTEGER   MXLINE
      PARAMETER (MXLINE=72)
      CHARACTER CCNUM*10
      HOLLERITH CATH(256)
      INTEGER   IOBUF(768), CATBLK(256), IV, I, NKEY, NCOL, NREC, IANG,
     *   TABLUN, IER, NPL, SCRTCH(30), NST, IST, STTYPE, NCHAR, NUMPNT,
     *   ICTYPE, FCOL, PCOL, WCOL
      REAL      CATR(256), AX(4), AY(4), RECORD(50), BMAJ, BMIN, BPA,
     *   TX, TY, PX(MXLINE), PY(MXLINE), DANGL, SINB, COSB, BX(2),
     *   BY(2), DX, DY, XPOS, YPOS, ANGL, XP(3), YP(3), BMJS, BMJC,
     *   BMNS, BMNC, F
      DOUBLE PRECISION XX(3), YY(3), ZZ(3)
      LOGICAL   DOPNT, XING, DOPIXR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATH, CATR)
      DATA TABLUN /27/
C-----------------------------------------------------------------------
      IERR = 0
      IF (FACTOR.EQ.0.0) GO TO 999
      STTYPE = MAX (0, CTYPE)
      IF (STTYPE.GT.21) STTYPE = 1
      DOPIXR = PIXR(2).GT.PIXR(1)
      ICTYPE = STTYPE
C                                       Is there a CC or MF file
      IERR = -1
      CALL FNDEXT (INEXTD, CATBLK, I)
      IF (I.LE.0) GO TO 999
      IV = VERS
      IF ((IV.LE.0) .OR. (IV.GT.I)) IV = I
C                                       Open CC or MF table file
      NKEY = 0
      NCOL = 0
      NREC = 0
      CALL TABINI ('READ', INEXTD, IVOL, CNO, IV, CATBLK, TABLUN, NKEY,
     *   NREC, NCOL, IOBUF(513), IOBUF, IERR)
      IF (IERR.EQ.0) GO TO 20
         WRITE (MSGTXT,1000) IERR, IV
         GO TO 990
 20   NST = IOBUF(5)
      IF (NCC.GT.0) NST = MIN (NST, NCC)
      DOPNT = NCOL.LT.7
      IF (INEXTD.EQ.'CC') THEN
         FCOL = 1
         PCOL = 2
         WCOL = 4
      ELSE
         CALL FNDCOL (1, 'I FLUX', 6, .FALSE., IOBUF, FCOL, IERR)
         CALL FNDCOL (1, 'DELTAX', 6, .FALSE., IOBUF, PCOL, IERR)
         CALL FNDCOL (1, 'MAJOR AX', 8, .FALSE., IOBUF, WCOL, IERR)
         IF ((FCOL.NE.3) .OR. (PCOL.NE.4) .OR. (WCOL.NE.6)) THEN
            WRITE (MSGTXT,4000) FCOL, PCOL, WCOL
 4000       FORMAT ('FNDCOL RESULTS: F, P, W = ',3I4)
            CALL MSGWRT (6)
            END IF
         END IF
C                                       I guess we can do it now
      NPL = 0
      DO 100 IST = 1,NST
         CALL TABIO ('READ', 0, IST, RECORD, IOBUF, IERR)
         IF (IERR.GT.0) THEN
            GO TO 975
C                                       row not flagged
         ELSE IF (IERR.EQ.0) THEN
            F = RECORD(FCOL)
            IF (ABS(F).LT.FCUT) GO TO 100
            IF (DOPIXR) THEN
               ICTYPE = STTYPE * (F - PIXR(1)) /
     *            (PIXR(2) - PIXR(1)) + 0.5
               ICTYPE = MAX (0, MIN (ICTYPE, STTYPE))
               END IF
            XPOS = RPLOC(1,LOCNUM) + RECORD(PCOL)/AXINC(1,LOCNUM)
            YPOS = RPLOC(2,LOCNUM) + RECORD(PCOL+1)/AXINC(2,LOCNUM)
C                                       If star not in plot, get next
            IF ((BLC(1).GT.XPOS) .OR. (BLC(2).GT.YPOS) .OR.
     *         (TRC(1).LT.XPOS) .OR. (TRC(2).LT.YPOS)) GO TO 100
C                                       Object dimensions
            IF (DOPNT) THEN
               BMAJ = ABS (FACTOR)
               BMIN = ABS (FACTOR)
               IF (FSCALE.GT.0.0) THEN
                  BMAJ = BMAJ * F / FSCALE
                  BMIN = BMIN * F / FSCALE
                  END IF
               BPA  = 0.0
               BMJS = 0.0
               BMNS = 0.0
               BMJC = BMAJ
               BMNC = BMIN
C                                        Correct BPA for coord rotation
            ELSE
               CALL XYVAL (XPOS, YPOS, XX(1), YY(1), ZZ(1), IERR)
               IF (IERR.NE.0) GO TO 100
               IF (CORTYP(LOCNUM).EQ.1) THEN
                  XX(2) = XX(1) + RECORD(WCOL) * SIN
     *               (DG2RAD*RECORD(WCOL+2)) / COS (DG2RAD * YY(1))
                  YY(2) = YY(1) + RECORD(WCOL) * COS
     *               (DG2RAD*RECORD(WCOL+2))
                  XX(3) = XX(1) - RECORD(WCOL+1) * COS
     *               (DG2RAD*RECORD(WCOL+2)) / COS (DG2RAD * YY(1))
                  YY(3) = YY(1) + RECORD(WCOL+1) * SIN
     *               (DG2RAD*RECORD(WCOL+2))
               ELSE IF (CORTYP(LOCNUM).EQ.2) THEN
                  YY(2) = YY(1) + RECORD(WCOL) * SIN
     *               (DG2RAD*RECORD(WCOL+2)) / COS (DG2RAD * XX(1))
                  XX(2) = XX(1) + RECORD(WCOL) * COS
     *               (DG2RAD*RECORD(WCOL+2))
                  YY(3) = YY(1) - RECORD(WCOL+1) * COS
     *               (DG2RAD*RECORD(WCOL+2)) / COS (DG2RAD * XX(1))
                  XX(3) = XX(1) + RECORD(WCOL+1) * SIN
     *               (DG2RAD*RECORD(WCOL+2))
               ELSE
                  XX(2) = XX(1) + RECORD(WCOL) * SIN
     *               (DG2RAD*RECORD(WCOL+2))
                  YY(2) = YY(1) + RECORD(WCOL) * COS
     *               (DG2RAD*RECORD(WCOL+2))
                  XX(3) = XX(1) - RECORD(WCOL+1) * COS
     *               (DG2RAD*RECORD(WCOL+2))
                  YY(3) = YY(1) + RECORD(WCOL+1) * SIN
     *               (DG2RAD*RECORD(WCOL+2))
                  END IF
               CALL XYPIX (XX(2), YY(2), XP(2), YP(2), IERR)
               IF (IERR.NE.0) GO TO 100
               CALL XYPIX (XX(3), YY(3), XP(3), YP(3), IERR)
               IF (IERR.NE.0) GO TO 100
               IF (FSCALE.LE.0.0) THEN
                  F = ABS (FACTOR)
               ELSE
                  F = F * ABS (FACTOR) / FSCALE
                  END IF
               BMJS = (XPOS - XP(2)) * 0.5 * F
               BMJC = (YP(2) - YPOS) * 0.5 * F
               BMNC = (XP(3) - XPOS) * 0.5 * F
               BMNS = (YP(3) - YPOS) * 0.5 * F
               END IF
            AX(1) = XPOS - BMJS
            AY(1) = YPOS + BMJC
            AX(2) = XPOS + BMJS
            AY(2) = YPOS - BMJC
            AX(3) = XPOS + BMNC
            AY(3) = YPOS + BMNS
            AX(4) = XPOS - BMNC
            AY(4) = YPOS - BMNS
C                                        If the label is requested
            IF (FACTOR.LT.0.0) THEN
               WRITE (CCNUM,1020) IST
               CALL CHTRIM (CCNUM, 10, CCNUM, NCHAR)
C                                        a box or no mark: TRC
               IF ((ICTYPE.EQ.2) .OR. (ICTYPE.LT.0)) THEN
                  TX = XPOS
                  TY = YPOS
                  DY = -0.5
C                                        Else right edge of mark
               ELSE
                  TX = XPOS
                  TY = AY(2)
                  DY = -1.1
                  END IF
               DX = -NCHAR / 2.0
C                                       If label starts in the plot
C                                       Move and Put Text on plot
               IF ((BLC(1).LE.TX) .AND. (BLC(2).LE.TY) .AND.
     *            (TRC(1).GE.TX) .AND. (TRC(2).GE.TY)) THEN
                  CALL GPOS (TX, TY, PLBUF, IER)
                  CALL GCHAR (NCHAR, 0, DX, DY, CCNUM, PLBUF, IERR)
                  END IF
               END IF
            IERR = 3
C                                       Assume No marking needed
            NUMPNT = 0
C                                       If star type is a cross
            IF (ICTYPE.EQ.0) THEN
               CALL RCOPY (4, AX, PX)
               CALL RCOPY (4, AY, PY)
               NUMPNT = 2
               XING = .TRUE.
C                                       Star type is a box
            ELSE IF (ICTYPE.EQ.2) THEN
               PX(1) =  XPOS + (AX(1) - AX(2) + AX(3) - AX(4))/2
               PY(1) =  YPOS + (AY(1) - AY(2) + AY(3) - AY(4))/2
               PX(2) =  XPOS + (AX(1) - AX(2) - AX(3) + AX(4))/2
               PY(2) =  YPOS + (AY(1) - AY(2) - AY(3) + AY(4))/2
               PX(3) =  XPOS + (-AX(1) + AX(2) - AX(3) + AX(4))/2
               PY(3) =  YPOS + (-AY(1) + AY(2) - AY(3) + AY(4))/2
               PX(4) =  XPOS + (-AX(1) + AX(2) + AX(3) - AX(4))/2
               PY(4) =  YPOS + (-AY(1) + AY(2) + AY(3) - AY(4))/2
               NUMPNT = 4
C                                       Star is cross with a gap
            ELSE IF (ICTYPE.GE.20) THEN
               PX(1)  = AX(1)
               PY(1)  = AY(1)
               PX(2)  = ((2.*AX(1)) + AX(2))/3.
               PY(2)  = ((2.*AY(1)) + AY(2))/3.
               PX(3)  = AX(2)
               PY(3)  = AY(2)
               PX(4)  = ((2.*AX(2)) + AX(1))/3.
               PY(4)  = ((2.*AY(2)) + AY(1))/3.
               NUMPNT = 2
               XING = .TRUE.
C                                       Star is a closed fig
            ELSE
               NUMPNT = ICTYPE
               IF (ICTYPE.EQ.1) NUMPNT = MXLINE/2
C                                       If making a crossing figure
               XING = ICTYPE.GT.9
C                                       Make crossing figures
               IF (XING) THEN
C                                       stars have sttype - 5 points
                  NUMPNT = NUMPNT - 5
C                                       choose crossing counts
                  DANGL = INT ((NUMPNT-2)/3.) + 1
                  IF (NUMPNT.GT.8) DANGL = DANGL + 1
               ELSE
                  DANGL = 1.
                  END IF
C                                       Make and N sided figure
               NUMPNT = MIN (NUMPNT, MXLINE/2)
C                                       Make a mark out of ?? points
               DO 50 IANG = 1,NUMPNT
                  ANGL = DANGL * (IANG-1.) * TWOPI / REAL(NUMPNT)
C                                       Compute rotation
                  SINB = SIN(ANGL)
                  COSB = COS(ANGL)
C                                       add offset to center pos.
                  PX(IANG) = XPOS + ((AX(1)-AX(2)) * COSB +
     *               (AX(3)-AX(4)) * SINB) / 2.
                  PY(IANG) = YPOS + ((AY(1)-AY(2)) * COSB +
     *               (AY(3)-AY(4)) * SINB) / 2.
C                                       Make complimentary figure
                  ANGL = ANGL + (TWOPI / REAL(NUMPNT))
                  SINB = SIN(ANGL)
                  COSB = COS(ANGL)
                  PX(IANG+NUMPNT) = XPOS + ((AX(1)-AX(2)) * COSB +
     *               (AX(3)-AX(4)) * SINB) / 2.
                  PY(IANG+NUMPNT) = YPOS + ((AY(1)-AY(2)) * COSB +
     *               (AY(3)-AY(4)) * SINB) / 2.
 50               CONTINUE
C                                       Turn off crossing for odd figs
               IF (XING .AND. (((NUMPNT)/2)*2.NE.NUMPNT)) XING =
     *            .FALSE.
               END IF
C                                       If a closed star mark
            IF ((ICTYPE.GE.0) .AND. (NUMPNT.GT.1)) THEN
C                                       Must move first before draw
               TX  = 0
               TY  = 0
C                                       For all line end points
               DO 70 I = 1,NUMPNT
C                                       get start point
                  BX(1) = PX(I)
                  BY(1) = PY(I)
C                                       If not last line
                  IF (I.LT.NUMPNT) THEN
                     BX(2) = PX(I+1)
                     BY(2) = PY(I+1)
C                                       Else close the figure
                  ELSE
                     BX(2) = PX(1)
                     BY(2) = PY(1)
                     END IF
C                                       draw one segment
                  CALL PLOTWO (BLC, TRC, BX, BY, TX, TY, .FALSE.,
     *               .FALSE., PLBUF, IERR)
                  IF (IERR.NE.0) GO TO 985
  70              CONTINUE
               END IF
C                                       If a crossing closed star mark
            IF (((ICTYPE.EQ.0) .OR. (ICTYPE.GT.9)) .AND. (NUMPNT.GT.1)
     *         .AND. (XING)) THEN
C                                       For all line end points
               DO 80 I = 1,NUMPNT
C                                       get start point
                  BX(1) = PX(I+NUMPNT)
                  BY(1) = PY(I+NUMPNT)
C                                       If not last line
                  IF (I.LT.NUMPNT) THEN
                     BX(2) = PX(I+NUMPNT+1)
                     BY(2) = PY(I+NUMPNT+1)
C                                       Else close the figure
                  ELSE
                     BX(2) = PX(NUMPNT+1)
                     BY(2) = PY(NUMPNT+1)
                     END IF
C                                       draw one segment
                  CALL PLOTWO (BLC, TRC, BX, BY, TX, TY, .FALSE.,
     *               .FALSE., PLBUF, IERR)
                  IF (IERR.NE.0) GO TO 985
  80              CONTINUE
               END IF
C                                       If cross with gap mark
            IF (ICTYPE.GE.20) THEN
C                                       get start point
               BX(1) = AX(3)
               BY(1) = AY(3)
               BX(2) = ((2.*AX(3)) + AX(4))/3.
               BY(2) = ((2.*AY(3)) + AY(4))/3.
C                                       draw one segment
               CALL PLOTWO (BLC, TRC, BX, BY, TX, TY, .FALSE., .FALSE.,
     *            PLBUF, IERR)
               IF (IERR.NE.0) GO TO 985
               BX(1) = AX(4)
               BY(1) = AY(4)
               BX(2) = ((2.*AX(4)) + AX(3))/3.
               BY(2) = ((2.*AY(4)) + AY(3))/3.
C                                       draw opposite segment
               CALL PLOTWO (BLC, TRC, BX, BY, TX, TY, .FALSE., .FALSE.,
     *            PLBUF, IERR)
               IF (IERR.NE.0) GO TO 985
C                                       End if cross with a gap
               END IF
            NPL = NPL + 1
            END IF
 100     CONTINUE
      IERR = 0
      WRITE (MSGTXT,1100) NPL, INEXTD, IV
      CALL MSGWRT (2)
      GO TO 985
C                                       Error print and close
 975  WRITE (MSGTXT,1975) IERR, INEXTD
      IERR = 2
      CALL MSGWRT (8)
 985  CALL TABIO ('CLOS', 0, IST, SCRTCH, IOBUF, IER)
      GO TO 999
C                                       Error print
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' OPENING CC FILE VERSION',I4)
 1020 FORMAT (I10)
 1100 FORMAT ('Plotted',I5,' positions from ',A,' table version',I4)
 1975 FORMAT ('IO ERROR',I5,' IN ',A,' TABLE FILE')
      END
