LOCAL INCLUDE 'PFPL1.INC'
C                                       PLTLAB declarations.
      REAL      CHOUT(4), YGAP
      INTEGER   NTEXT, NTEXT2, PCODES(5)
      CHARACTER TEXT(2)*80, TEXT2(2)*80, XUNIT*8, YUNIT*8, TITLE*80
C                                       Plot labeling parameters.
      COMMON /PLTLAB/ CHOUT, YGAP, NTEXT, NTEXT2, PCODES
      COMMON /PLCLAB/ TEXT, TEXT2, XUNIT, YUNIT, TITLE
C                                       INPARM declarations
      REAL      PRUSER, NAMIN(3), CLSIN(2), SEQIN, DSKIN, BLC(7),
     *   TRC(7), PIXRNG(2), XLTYPE, XYRATO, DOCIRC, XDOTV, XGRCH, XTVCH,
     *   XTVCRN(2)
C                                       Parameters from AIPS.
      COMMON /INPARM/ PRUSER, NAMIN, CLSIN, SEQIN, DSKIN, BLC, TRC,
     *   PIXRNG, XLTYPE, XYRATO, DOCIRC, XDOTV, XGRCH, XTVCH, XTVCRN
C
LOCAL END
      PROGRAM PFPL1
C-----------------------------------------------------------------------
C! Plot paraform task: 2 axes are 2 axes of the image, e.g. grey scale
C# Plot
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2002, 2009, 2014-2015
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
C   PFPL1 (paraform plot program 1) is designed to be a model for
C   writing tasks to do plotting in AIPS.  This program is designed
C   for reading a map and creating a plot file to be associated
C   with the map.  For labeling, the plot axes are assumed to be the
C   X and Y axes of the map.  When plotting, coordinates are assumed
C   to be given in pixels.  Tasks that use plots of this type are
C   CNTR and GREYS.  This task plots one plane of a cube only.
C   For other types of plots, see programs PFPL2 and PFPL3.
C   The program will do the following steps:
C     1. Open a map file corresponding to the user's inputs from AIPS.
C     2. Create an extension file of type PL (plot) to be associated
C        with the map file.  The header of the map file will be
C        updated to include this new extension file.
C     3. Write the plot file records to draw the borders and labels
C        of the plot.  The programmer can customize this section of
C        the program by changing data statements in subroutine PLTOR1.
C     4. Write the rest of the plot file records to the plot file.
C        This is done by subroutine PLTOR1.  The current routine
C        writes grey scale pixels for the map.  The programmer will
C        have to modify the code in PLTOR1 for his needs.
C     5. Do the necessary clean up functions, write end of plot
C        records, close all files, etc.
C
C   IMPORTANT NOTE: to avoid confusion, this task should be renamed.
C   To rename (max. 5 char) and install the new task:
C     1. Copy the source code to a new file with the name
C        NEWNAME.FOR.
C     2. Using the source editor, change all references to PFPL1 to
C        NEWNAME.  It is especially important to change the string
C        entered into array PRGM at or near line 61 to the new name.
C     3. Copy help/inputs file PFPL1.HLP to inputs file NEWNAME.HLP.
C        Update the inputs, help and explain sections.
C     4. Compile and link edit with the APL and NOTST subroutine
C        libraries from AIPS.
C     5. Modify the comments at the top of this program to help
C        others understand what the program does.
C     6. Change the assignment statements to array variable PCODES
C        and the data statements for NTEXT, TEXT, XUNIT, YUNIT and
C        TITLE in the main program to get the type of axis labeling
C        you want.  Then add code to PLTOR1 to do the plotting.
C     7. While debugging your program, set IDEBUG to 1 to have plot
C        files kept despite program failures.  DUMP routines can be
C        run on them to determine what did go to the plot file and
C        and to help figure out what went wrong.  Set IDEBUG to 0
C        before releasing the program to other users.  Or you can
C        set DOTV true to view the results on the TV.
C
C    Inputs:   (from AIPS)
C       USERID    R      user number, 0 means use logon user
C                        number, 32000 means any user can be accessed.
C       INNAME    R(3)   name of primary file.
C       INCLASS   R(2)   class of primary file.
C       INSEQ     R      sequence number of primary file.
C       NDISK     R      disk volume number. 0 means try all.
C       BLC       R(7)   the starting coordinates for reading
C                        the input file.  BLC(1) is the X coordinate and
C                        BLC(2) is the Y coordinate.  The first
C                        coordinate in the input image is (1,1).
C       TRC       R(7)   the ending coordinate for reading the
C                        input file.
C       PIXRANGE  R(2)   For grey scales only.  Sets the upper
C                        and lower limit on intensity of grey scale
C                        values.  0, 0 defaults to range in map header.
C       LTYPE     R      Label type.
C       XYRATO    R      ratio of X axis units to Y axis units.
C       DOCIRC    R      T => full coordinate grid, else just ticks
C       DOTV      R      > 0 => TV, else plot file
C       TVCHAN    R      grey-scale channel to use (0 => 1)
C       GRCHAN    R      graphics channel to use
C       TVCORN    R(2)   TV pixel to use (both > 0 => pixel scale)
C-----------------------------------------------------------------------
C                                       local declarations
      CHARACTER ATEXT(2)*80, AXUNIT*8, AYUNIT*8, ATITLE*80, PRGNAM*6
      INTEGER   IMLUN, NPARMS, IERR, INTEXT, IDEBUG, IROUND, IPTYPE, I
      LOGICAL   DOGRID
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'PFPL1.INC'
      DATA IMLUN /16/
      DATA PRGNAM /'PFPL1 '/
C                                       Data statements to control the
C                                       text at the bottom of the plot.
C                                       INTEXT is the number of text
C                                       lines.
      DATA INTEXT /2/
      DATA ATEXT /'This is some text at the bottom of the plot ',
     *     'This is the second line '/
C                                       Units to use if PCODES(2)
C                                       .NE. 0 and PCODES(3) .NE. 0.
      DATA AXUNIT /'XUNIT'/, AYUNIT /'YUNIT'/
C                                       Title if PCODES(4) .NE. 0.
      DATA ATITLE /'TITLE'/
C-----------------------------------------------------------------------
C                                       Copy from DATA to Commons
      IBLKSZ = MABFSS
      GPHIND = 0
      NTEXT = INTEXT
      XUNIT = AXUNIT
      YUNIT = AYUNIT
      TITLE = ATITLE
      TEXT(1) = ATEXT(1)
      TEXT(2) = ATEXT(2)
C                                       Don't delete plot file on error
      IDEBUG = 1
C                                       This is the number of REAL*4
C                                       words to get from AIPS.
C                                       Fixed PPM 1996.09.30: was 34
      NPARMS = 32
C                                       Plot type PFPL1 paraform
      IPTYPE = 11
C                                       Get parms from AIPS, open map
C                                       file, create plot file,
      CALL PF1INI (PRGNAM, NPARMS, IMLUN, IPTYPE, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       These are parameters to
C                                       control labeling.
      PCODES(1) = IROUND (XLTYPE)
      I = MOD (ABS(PCODES(1)), 100)
      IF ((I.EQ.0) .OR. (I.GT.10)) THEN
         IF (PCODES(1).GE.0) THEN
            PCODES(1) = (PCODES(1)/100)*100 + 3
         ELSE
            PCODES(1) = (PCODES(1)/100)*100 - 3
            END IF
         END IF
      XLTYPE = PCODES(1)
C                                       Use axis labels found in hdr.
      PCODES(2) = 0
      PCODES(3) = 0
C                                       Use "standard" title line
      PCODES(4) = 0
C                                       Grey scales values present.
      PCODES(5) = 1
      XY = XYRATO
C                                       Do plotting.  This routine must
C                                       be modified by the programmer.
      DOGRID = DOCIRC.GT.0.0
      CALL PLTOR1 (DOGRID, IERR)
C                                       Shutdown.
 900  CALL PLEND (IERR, IDEBUG)
C
 999  STOP
      END
      SUBROUTINE PF1INI (PRGNAM, NPARMS, IMLUN, IPTYPE, IERR)
C-----------------------------------------------------------------------
C  This routine does all the intial set up.  Get parms from AIPS,
C  open the map file, create the plot file and write the plot file
C  records to do the plot labeling.
C  Inputs:
C     PRGNAM  C*6    Name of this program.
C     NPARMS  I      Number of R words to get from AIPS.
C     IMLUN   I      The logical unit number to use for the map file.
C     IPTYPE  I      Plot file type: 1 misc., 2 CNTR, 3 GREYS, 4 PROFL,
C                    5 SL2PL, 6 PCNTR, 7 IMEAN (hist), 8 UVPLT,
C                    9 GNPLT, 10 VBPLT, 11 PFPLn, 12 GAPLT, 13 PLCUB,
C                    14 IMVIM, 15 TAPLT.  Use 1 unless your inputs
C                    match those of these tasks - or take a new number,
C                    but AIPSUB:AU8A will need to know about it too.
C  Output:
C     IERR    I      Error code. 0=ok.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   NPARMS, IMLUN, IPTYPE, IERR
C
      CHARACTER NAME*36, FNAME*12, FCLASS*6, FPTYPE*2, STAT*4
      REAL      TEMP
      INTEGER   IWORK(256), I, FSEQ, FVOL, FUSID, IROUND
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'PFPL1.INC'
C-----------------------------------------------------------------------
C                                       Get parms, set IO & hdr vals.
C                                       Get parameters from AIPS, init
C                                       AIPS I/O, other startup things.
      CALL SETUP (PRGNAM, NPARMS, PRUSER, IWORK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open the map file.
      CALL H2CHR (12, 1, NAMIN, FNAME)
      CALL H2CHR (6, 1, CLSIN, FCLASS)
      FSEQ = SEQIN + 0.01
      FVOL = DSKIN + 0.01
      PRUSER = NLUSER
      FUSID = NLUSER
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = XTVCH + 0.01
      TVCORN(1) = IROUND (XTVCRN(1))
      TVCORN(2) = IROUND (XTVCRN(2))
      STAT = 'HDWR'
      IF (DOTV) STAT = 'READ'
      CALL A2WAWA (FNAME, FCLASS, FSEQ, 'MA', FVOL, FUSID, NAME)
      CALL INTMIO (IMLUN, STAT, NAME, BLC, TRC, IBLKSZ, CATBLK,
     *   IMSTUF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Force just one plane
      DO 20 I = 3,7
         TRC(I) = BLC(I)
         IMSTUF(16+2*I) = IMSTUF(15+2*I)
 20      CONTINUE
      IMSTUF(32) = BLC(3) + .0001
      IMSTUF(32) = IMSTUF(32) - 1
C                                       Backwards not allowed!!!
C                                       X axis: I/O already fixed
      IF (BLC(1).GE.TRC(1)) THEN
         TEMP = BLC(1)
         BLC(1) = TRC(1)
         TRC(1) = TEMP
         END IF
      IF (BLC(2).GE.TRC(2)) THEN
         TEMP = BLC(2)
         BLC(2) = TRC(2)
         TRC(2) = TEMP
         IMSTUF(19) = BLC(2) + .0001
         IMSTUF(20) = TRC(2) + .9999
         IMSTUF(37) = 1
         IMSTUF(31) = IMSTUF(20)
         END IF
C                                       Find actual range of map values
      CALL RNGSET (PIXRNG, CATR(KRDMX), CATR(KRDMN), RANGE)
C                                       fill in adverbs w actual values
      CALL WAWA2A (NAME, FNAME, FCLASS, FSEQ, FPTYPE, FVOL, FUSID)
      CALL CHR2H (12, FNAME, 1, NAMIN)
      CALL CHR2H (6, FCLASS, 1, CLSIN)
      SEQIN = FSEQ
      DSKIN = FVOL
      PIXRNG(1) = RANGE(1)
      PIXRNG(2) = RANGE(2)
C                                       Create and open plot file.
      CALL PLMAKE (NPARMS, PRUSER, IPTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE PLTOR1 (DOGRID, IERR)
C-----------------------------------------------------------------------
C   This routine writes all of the plot commands (including the axis
C   drawing and labeling commands) into the plot file.
C   Input from common:
C      BLC       R(7)   Bottom left corner of plot.
C      TRC       R(7)   Top right corner of plot.
C   Input:
C      DOGRID    L      Plot full coordinate grid?
C   Output:
C      IERR      I      Error code.  0=ok.
C-----------------------------------------------------------------------
      INTEGER   IERR
      LOGICAL   DOGRID
C
      INCLUDE 'INCS:PMAD.INC'
      REAL      X, Y, ROW(MAXIMG)
      INTEGER   I, IHORIZ, NVAL
      LOGICAL   EOF
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'PFPL1.INC'
C-----------------------------------------------------------------------
C                                       Set up commons for plotting.
      CALL PLINI1 (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Do axis labeling.
      CALL PLABL1 (DOGRID, IERR)
      IF (IERR.NE.0) GO TO 999
C                                     ** Plot specific statements.
      IHORIZ = 0
      X = BLC(1)
      NVAL = IMSTUF(9)
      DO 20 I = 1,32000
C                                       Read a map row.
         CALL GETROW (IMSTUF, IOBLK, ROW, EOF, IERR)
         IF ((EOF) .OR. (IERR.NE.0)) GO TO 999
C                                       Position at beginning of row.
         Y = IMSTUF(31)
         CALL PLPOS (X, Y, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Write Grey scale pixels.
         CALL PLGRY (IHORIZ, NVAL, ROW, ROW, IERR)
         IF (IERR.NE.0) GO TO 999
 20      CONTINUE
C                                     ** End plot specific statements.
 999  RETURN
      END
      SUBROUTINE PLINI1 (IERR)
C-----------------------------------------------------------------------
C  This routine will set up the location commons for the plot file.
C  Inputs from common:
C     BLC   R(7)    Bottom left corner of plot. (from local include)
C     TRC   R(7)    Top right corner of plot.   (from local include)
C  Output:
C     IERR    I       Error code. 0=OK.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      REAL      BIGNUM, XHPVAL, XLPVAL
      INTEGER   LABEL, DEPT(5), ILPVAL, IHPVAL, I, IROUND, LTYPE
      LOGICAL   SLICE
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'PFPL1.INC'
C-----------------------------------------------------------------------
      LABEL = PCODES(1)
      LTYPE = MOD (ABS(LABEL), 100)
      LOCNUM = 1
C                                       Text borders at left, bottom,
C                                       right and top in characters.
      CALL RFILL (4, 0.0, CHOUT)
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CHOUT(2) = NTEXT * 1.333
C                                       Set up default scaling parms.
      XSCAL = 1.0
      YSCAL = 1.0
      XOFF = 0.0
      YOFF = 0.0
C                                       X  and Y axis of plot are same
C                                       as X and Y axis of map.
      CALL RCOPY (2, BLC, PBLC)
      CALL RCOPY (2, TRC, PTRC)
      DO 10 I = 1,5
         DEPT(I) = BLC(I+2) + 0.5
 10      CONTINUE
C                                       Provide needed border
      IF (PCODES(5).GT.0) THEN
         PBLC(1) = PBLC(1) - 0.7
         PBLC(2) = PBLC(2) - 0.7
         PTRC(1) = PTRC(1) + 0.7
         PTRC(2) = PTRC(2) + 0.7
         END IF
      SLICE = .FALSE.
      CALL LABINI (PBLC, PTRC, DEPT, CHOUT, LABEL, SLICE, YGAP, TEXT2,
     *   NTEXT2)
      IF ((XY.EQ.0.0) .AND. (CATR(KRCIC+1).NE.0.0) .AND.
     *   (AXTYP(LOCNUM).EQ.1)) XY = ABS (CATR(KRCIC) / CATR(KRCIC+1))
      IF (XY.EQ.0.0) XY = 1.0
C                                       Programmer may have specified
C                                       own X Y units.
      IF (PCODES(2).NE.0) CTYP(1,LOCNUM) = XUNIT
      IF (PCODES(3).NE.0) CTYP(2,LOCNUM) = YUNIT
C                                       Write initialization records
C                                       into plot file.
C                                       initialize line drawing
      CALL GINITL (PBLC, PTRC, XY, CHOUT, DEPT, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Grey scale init.
      IF (PCODES(5).NE.0) THEN
C                                       Calculate scale factor and
C                                       offset to get maximum range.
         BIGNUM = 2.0D0 ** MIN (30, NBITWD - 1)  -  8.0D0
         GFAC = 2.0D0 * BIGNUM / (RANGE(2) - RANGE(1))
         GOFF = BIGNUM - (GFAC * RANGE(2))
C                                       Range becomes range in plt file.
         XHPVAL = GFAC * RANGE(2) + GOFF
         XLPVAL = GFAC * RANGE(1) + GOFF
         IHPVAL = IROUND (XHPVAL)
         ILPVAL = IROUND (XLPVAL)
         CALL GINITG (ILPVAL, IHPVAL, PIXRNG, PLTBLK, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE PLABL1 (DOGRID, IERR)
C-----------------------------------------------------------------------
C   This program uses the values set in PLINI1 and passed through
C   commons LOCATI, LOCATC, and PLTCOM to do the axes labeling.
C   Commons:
C      /LOCATI/  (from incs:DLOC.INC)
C      /LOCATC/  (from incs:DLOC.INC)
C      /PLTCOM/  (from incs:DPLT.INC)
C   Inputs:
C      DOGRID   L      Plot full coordinate grid?
C   Output:
C      IERR     I      Error code. 0=ok.
C-----------------------------------------------------------------------
      LOGICAL   DOGRID
      INTEGER   IERR
C
      REAL      DCX, DCY
      INTEGER   I, IANGLE, INCHAR, IT(3), ID(3), LABEL, LTYPE
      CHARACTER WRKTXT*80, ATIME*8, ADATE*12, ANAME*18
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'PFPL1.INC'
C-----------------------------------------------------------------------
      LABEL = PCODES(1)
      LTYPE = MOD (ABS(LABEL), 100)
      CALL GLTYPE (1, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Tics and tic labels
      CALL CLAB1 (PBLC, PTRC, CHOUT, LABEL, XY, DOGRID, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        draw rectangle
      CALL GPOS (PBLC(1), PBLC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (PTRC(1), PBLC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (PTRC(1), PTRC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (PBLC(1), PTRC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (PBLC(1), PBLC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       title line.
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
C                                       Standard title
         IF (PCODES(4).EQ.0) THEN
            CALL H2CHR (8, 1, CATH(KHOBJ), TITLE)
            INCHAR = 12
            IF (TITLE.EQ.' ') INCHAR = 1
            IF (NCHLAB(1,LOCNUM).GT.0) THEN
               IF (INCHAR.GT.1) TITLE(INCHAR-1:INCHAR-1) = '_'
               TITLE(INCHAR:) = SAXLAB(1,LOCNUM)(1:NCHLAB(1,LOCNUM))
               INCHAR = INCHAR + 3 + NCHLAB(1,LOCNUM)
               END IF
            IF (NCHLAB(2,LOCNUM).GT.0) THEN
               IF (INCHAR.GT.1) TITLE(INCHAR-1:INCHAR-1) = '_'
               TITLE(INCHAR:) = SAXLAB(2,LOCNUM)(1:NCHLAB(2,LOCNUM))
               INCHAR = INCHAR + 3 + NCHLAB(2,LOCNUM)
               END IF
            IF (INCHAR.GT.1) TITLE(INCHAR-1:INCHAR-1) = '_'
            CALL H2CHR (18, 1, CATH(KHIMN), ANAME)
            CALL NAMEST (ANAME, CATBLK(KIIMS), TITLE(INCHAR:), I)
            CALL REFRMT (TITLE, '_', INCHAR)
            END IF
         CALL GPOS (PBLC(1), PTRC(2), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         DCX = 0.0
         DCY = 0.5
         IANGLE = 0
         CALL CHTRIM (TITLE, 80, TITLE, INCHAR)
         CALL GCHAR (INCHAR, IANGLE, DCX, DCY, TITLE, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Date/time version
      IF ((LABEL.GT.1) .AND. (LABEL.LT.7)) THEN
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, ATIME, ADATE)
         WRITE (WRKTXT,1020) IVER, ADATE, ATIME
         CALL REFRMT (WRKTXT, '_', INCHAR)
         DCY = DCY + 1.333
         CALL GPOS (PBLC(1), PTRC(2), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GCHAR (INCHAR, IANGLE, DCX, DCY, WRKTXT, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Label X axis extra lines
      IF ((NTEXT2.GT.0) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7))
     *   THEN
         DCX = 0.0
         IANGLE = 0
         DO 30 I = 1,NTEXT2
            WRKTXT = TEXT2(I)
            CALL CHTRIM (WRKTXT, 80, WRKTXT, INCHAR)
            DCY = -YGAP
            YGAP = YGAP + 1.333
            CALL GPOS (PBLC(1), PBLC(2), PLTBLK, IERR)
            CALL GCHAR (INCHAR, IANGLE, DCX, DCY, WRKTXT, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
 30         CONTINUE
         END IF
C                                       Text at bottom.
      IF ((NTEXT.GT.0) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7))
     *   THEN
         DCX = 0.0
         IANGLE = 0
         DO 40 I = 1,NTEXT
            WRKTXT = TEXT(I)
            CALL CHTRIM (WRKTXT, 80, WRKTXT, INCHAR)
            DCY = -YGAP
            YGAP = YGAP + 1.333
            CALL GPOS (PBLC(1), PBLC(2), PLTBLK, IERR)
            CALL GCHAR (INCHAR, IANGLE, DCX, DCY, WRKTXT, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
 40         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('PLOT FILE VERSION',I4,'__CREATED ',A12,A8)
      END
