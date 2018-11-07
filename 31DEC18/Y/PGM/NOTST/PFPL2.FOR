LOCAL INCLUDE 'PFPL2.INC'
C                                       PLTLAB declarations.
      REAL      CHOUT(4), YGAP
      INTEGER   NTEXT, NTEXT2, PCODES(5)
      CHARACTER TEXT(2)*80, TEXT2(2)*80, XUNIT*8, YUNIT*8, TITLE*80
C                                       Plot labeling parameters.
      COMMON /PLTLAB/ CHOUT, YGAP, NTEXT, NTEXT2, PCODES
      COMMON /PLCLAB/ TEXT, TEXT2, XUNIT, YUNIT, TITLE
C                                       INPARM declarations.
      REAL      PRUSER, NAMIN(3), CLSIN(2), SEQIN, DSKIN, BLC(7),
     *   TRC(7), PIXRNG(2), XLTYPE, XDOTV, XGRCH
C                                       Parameters from AIPS.
      COMMON /INPARM/ PRUSER, NAMIN, CLSIN, SEQIN, DSKIN, BLC, TRC,
     *   PIXRNG, XLTYPE, XDOTV, XGRCH
LOCAL END
      PROGRAM PFPL2
C-----------------------------------------------------------------------
C! Plot paraform task: X axis = slice of X-Y axes; Y = other plot value
C# Plot
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2002, 2009, 2012, 2014-2015
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
C   PFPL2 (paraform plot program 2) is designed to be a model for
C   writing tasks to do plotting in AIPS.  This program is designed
C   for reading a map and creating a plot file to be associated
C   with the map.  This task assumes that the X axis of the plot is a
C   slice of the X-Y axes of the map and the Y axis of the plot is
C   some other value such as intensity.  Plot labels for the Y axis
C   come from a data statement found in the main program.  Labeling
C   the X axis in a way that works for all types of data is a complex
C   problem and is handled automatically (with luck) by this program.
C   An example of this type of plot is the slice plot produced by
C   calling task SLICE to produce a slice file followed by using task
C   SL2PL to produce a plot file.  For other types of plots see
C   programs PFPL1 and PFPL3.
C   The program will do the following steps:
C     1. Open a map file corresponding to the user's inputs from AIPS.
C     2. Create an extension file of type PL (plot) to be associated
C        with the map file.  The header of the map file will be
C        updated to include this new extension file.
C     3. Write the plot file records to draw the borders and labels
C        of the plot.  The programmer can customize this section of
C        the program by changing data statements in subroutine PLTOR2.
C     4. Write the rest of the plot file records to the plot file.
C        This is done by subroutine PLTOR2.  The current routine
C        draws a plot of map intensities through a slice of the map.
C        This program uses no interpolation and is NOT adequate for
C        a production program.  See the code in program SLICE for a
C        good set of interpolation routines.  The programmer will
C        have to modify the code in subroutine PLTOR2 for his/her
C        needs.
C     5. Do the necessary clean up functions, write end of plot
C        records, close all files, etc.
C
C   IMPORTANT NOTE: to avoid confusion, this task should be renamed.
C   To rename (max. 5 char) and install the new task:
C     1. Copy the source code to a new file with the name
C        NEWNAME.FOR.
C     2. Using the source editor, change all references to PFPL2 to
C        NEWNAME.  It is especially important to change the string
C        entered into array PRGM at or near line 61 to the new name.
C     3. Copy help/inputs file PFPL2.HLP to inputs file NEWNAME.HLP.
C        Update the inputs, help and explain sections.
C     4. Compile and link edit with the APL and NOTST subroutine
C        libraries from AIPS.
C     5. Modify the comments at the top of this program to help
C        others understand what the program does.
C     6. Change the assignment statements to array variable PCODES
C        and the data statements for NTEXT, TEXT, XUNIT, YUNIT and
C        TITLE in the main program to get the type of axis labeling
C        you want.  Then add code to PLTOR2 to do the plotting.
C     7. While debugging your program, set IDEBUG to 1 to have plot
C        files kept despite program failures.  DUMP routines can be
C        run on them to determine what did go to the plot file and
C        and to help figure out what went wrong.  Set IDEBUG to 0
C        before releasing the program to other users.
C
C    Inputs:   (from AIPS)
C       USERID   R      user number, 0 means use logon user
C                       number, 32000 means any user can be accessed.
C       INNAME   R(3)   name of primary file.
C       INCLASS  R(2)   class of primary file.
C       INSEQ    R      sequence number of primary file.
C       NDISK    R      disk volume number. 0 means try all.
C       BLC      R(7)   the starting coordinates for the slice.
C                       BLC(1) is the X coordinate and
C                       BLC(2) is the Y coordinate.  The first
C                       coordinate in the input image is (1,1).
C       TRC      R(7)   the ending coordinate for the slice.
C       PIXRANGE R(2)   Sets the upper and lower limit on intensity
C                       of values to appear in the slice.  0, 0
C                       defaults to range in map header.
C       LTYPE    R      Label type.
C       DOTV     R      > 0 => TV, else plot file
C       GRCHAN   R      graphics channel to use (0 => 1)
C-----------------------------------------------------------------------
C                                       local declarations
      CHARACTER ATEXT(2)*80, AXUNIT*8, AYUNIT*8, ATITLE*80, PRGNAM*6
      INTEGER   IMLUN, NPARMS, IERR, INTEXT, IDEBUG, IROUND, IPTYPE, I
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'PFPL2.INC'
      DATA IMLUN /16/
      DATA PRGNAM /'PFPL2 '/
C                                       Data statements to control the
C                                       text at the bottom of the plot.
C                                       INTEXT is the number of text
C                                       lines.
      DATA INTEXT /2/
      DATA ATEXT /'This is some text at the bottom of the plot ',
     *     'This is the second line '/
C                                       Units to use if PCODES(2)
C                                       .NE. 0 and PCODES(3) .NE. 0.
      DATA AXUNIT, AYUNIT /'XUNIT','YUNIT'/
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
      NPARMS = 27
C                                       Plot type PFPL2 paraform
      IPTYPE = 11
C                                       Get parms from AIPS, open map
C                                       file, create plot file,
      CALL PF2INI (PRGNAM, NPARMS, IMLUN, IPTYPE, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       These are parameters to
C                                       control labeling.
C                                       Label type from user input.
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
C                                       Use "standard" labeling for
C                                       X axis based on map header.
      PCODES(2) = 0
C                                       Use intensity value in header
C                                       for Y axis label.
      PCODES(3) = 0
C                                       Use "standard" title line
      PCODES(4) = 0
C                                       Not now used
      PCODES(5) = 0
C                                       Do plotting.  This routine must
C                                       be modified by the programmer.
      CALL PLTOR2 (IERR)
C                                       Shutdown.
 900  CALL PLEND (IERR, IDEBUG)
C
 999  STOP
      END
      SUBROUTINE PF2INI (PRGNAM, NPARMS, IMLUN, IPTYPE, IERR)
C-----------------------------------------------------------------------
C   This routine does all the intial set up.  Get parms from AIPS,
C   open the map file, create the plot file and write the plot file
C   records to do the plot labeling.
C   Inputs:
C      PRGNAM  C*6   Name of this program.
C      NPARMS  I     Number of R words to get from AIPS.
C      IMLUN   I     The logical unit number to use for the map file.
C      IPTYPE  I     Plot file type: 1 misc., 2 CNTR, 3 GREYS, 4 PROFL,
C                    5 SL2PL, 6 PCNTR, 7 IMEAN (hist), 8 UVPLT,
C                    9 GNPLT, 10 VBPLT, 11 PFPLn, 12 GAPLT, 13 PLCUB,
C                    14 IMVIM, 15 TAPLT.  Use 1 unless your inputs match
C                    those of these tasks - or take a new number, but
C                    AIPSUB:AU8A will need to know about it too.
C   Output:
C      IERR    I     Error code. 0=ok.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   NPARMS, IMLUN, IPTYPE, IERR
C
      CHARACTER NAME*36, FNAME*12, FCLASS*6, FPTYPE*2, STAT*4
      INTEGER   IWORK(256), FSEQ, FVOL, FUSID
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'PFPL2.INC'
C-----------------------------------------------------------------------
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
      CALL A2WAWA (FNAME, FCLASS, FSEQ, 'MA', FVOL, FUSID, NAME)
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
      STAT = 'HDWR'
      IF (DOTV) STAT = 'READ'
      CALL INTMIO (IMLUN, STAT, NAME, BLC, TRC, IBLKSZ, CATBLK,
     *   IMSTUF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       fix to do one plane only
      CALL RCOPY (5, BLC(3), TRC(3))
      IMSTUF(22) = IMSTUF(21)
      IMSTUF(24) = IMSTUF(23)
      IMSTUF(26) = IMSTUF(25)
      IMSTUF(28) = IMSTUF(27)
      IMSTUF(30) = IMSTUF(29)
C                                       Find actual range of map values
      CALL RNGSET (PIXRNG, CATR(KRDMX), CATR(KRDMN), RANGE)
C                                       Fill in defaults
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
      SUBROUTINE PLTOR2 (IERR)
C-----------------------------------------------------------------------
C  This routine writes all of the plot commands (including the axis
C  drawing and labeling commands) into the plot file.
C  Input from common:
C     BLC      R(7)   Bottom left corner of plot.
C     TRC      R(7)   Top right corner of plot.
C  Output:
C    IERR      I      Error code.  0=ok.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'INCS:PMAD.INC'
      REAL      X, Y, ROW(MAXIMG), DX, DY, XVAL, YVAL
      INTEGER   INOSL, I, II, IY, IROUND, IX, ICUR, IXEND
      LOGICAL   EOF, DOWN
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'PFPL2.INC'
C-----------------------------------------------------------------------
C                                       Number of points in slice.
      INOSL = 500
C                                       Set up commons for plotting.
      CALL PLINI2 (INOSL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Do axis labeling.
      CALL PLABL2 (IERR)
      IF (IERR.NE.0) GO TO 999
C                                    ** Plot specific statements.
C                                       Calculate some counters.
      DX = (TRC(1) - BLC(1)) / (INOSL - 1)
      DY = (TRC(2) - BLC(2)) / (INOSL - 1)
C                                       Read 1st row.
      CALL GETROW (IMSTUF, IOBLK, ROW, EOF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Determine which end of buffer to
C                                       start from.
      IXEND = 1
      IF (BLC(1).GT.TRC(1)) IXEND = IMSTUF(9)
C                                       Position at 1st value.
      CALL GLTYPE (2, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      XVAL = 1.0
      YVAL = ROW(IXEND)
      CALL PLPOS (XVAL, YVAL, IERR)
      IF (IERR.NE.0) GO TO 999
      DOWN = YVAL.NE.FBLANK
C                                       Calculate X and Y
      DO 30 I = 2,INOSL
         X = (I - 1) * DX + IXEND
         Y = (I - 1) * DY + BLC(2)
C                                       Find nearest X and Y.
         IY = IROUND (Y)
         IX = IROUND (X)
C                                       See if correct row is in core,
C                                       if not read.
         DO 10 II = 1,100
C                                       IMSTUF(31) has Y pixel number
C                                       of current line in buffer ROW.
            ICUR = IMSTUF(31)
            IF (IY.EQ.ICUR) GO TO 20
               CALL GETROW (IMSTUF, IOBLK, ROW, EOF, IERR)
               IF (IERR.NE.0) GO TO 999
 10         CONTINUE
C                                       Read too many records.
         WRITE (MSGTXT,1010)
         CALL MSGWRT (8)
         IERR = 1
         GO TO 999
C                                       Draw line.
 20      YVAL = ROW(IX)
         IF (YVAL.NE.FBLANK) THEN
            XVAL = I
            IF (DOWN) THEN
               CALL PLVEC (XVAL, YVAL, IERR)
            ELSE
               CALL PLPOS (XVAL, YVAL, IERR)
               END IF
            IF (IERR.NE.0) GO TO 999
            END IF
         DOWN = YVAL.NE.FBLANK
 30      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('PLTOR2: MIX UP IN COUNTING ROWS.')
      END
      SUBROUTINE PLINI2 (INOSL, IERR)
C-----------------------------------------------------------------------
C   This routine will set up the location commons for the plot file.
C   Inputs:
C      INOSL   I       Number of points in slice.
C    Output:
C      IERR    I       Error code. 0=OK.
C   Common:
C      BLC     R(7)    Lower left corner
C      TRC     R(7)    Upper right corner
C      RANGE   R(2)    Plot range
C-----------------------------------------------------------------------
      INTEGER   INOSL, IERR
C
      REAL      YRANGE, XRANGE, HISCAL, FQFINC
      DOUBLE PRECISION FQFREQ
      INTEGER   IDROP(2), IDEPT(5), LTYPE
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'PFPL2.INC'
      DATA HISCAL /3200.0/
C-----------------------------------------------------------------------
C                                       Range completely specified by
C                                       input blc, trc, i.e., no drops.
      IDROP(1) = 0
      IDROP(2) = 0
C                                       Set up default scaling parms.
      XSCAL = 1.0
      YSCAL = 1.0
      XOFF = 0.0
      YOFF = 0.0
C                                       Probably necessary to rescale
C                                       Y to prevent integer overflow
C                                       in low-level graphics routines.
      YRANGE = RANGE(2) - RANGE(1)
      PTRC(2) = RANGE(2)
      PBLC(2) = RANGE(1)
C                                       Rescale Y values
      IF ((YRANGE.GE.HISCAL) .OR. (YRANGE.LE.10.0)) THEN
         YSCAL = HISCAL / YRANGE
         YOFF = - RANGE(1) * YSCAL
         PTRC(2) = HISCAL
         PBLC(2) = 0.0
         YRANGE = HISCAL
         END IF
C                                       Standard slice labeling init.
      LOCNUM = 1
      FQFINC = 0.0
      FQFREQ = 0.0D0
      CALL SLBINI (IDROP, INOSL, RANGE, PBLC, PTRC, BLC, TRC, FQFREQ,
     *   FQFINC, IDEPT, PCODES(1), YGAP, CHOUT, TEXT2, NTEXT2)
      LTYPE = MOD (ABS(PCODES(1)),100)
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7))
     *   CHOUT(2) = CHOUT(2) + NTEXT * 1.333
C                                       Change labeling to XUNIT if
C                                       programmer wants.
      IF (PCODES(2).GT.0) CTYP(1,LOCNUM) = XUNIT
C                                       Fill in units for YUNIT.
      IF (PCODES(3).GT.0) CTYP(2,LOCNUM) = YUNIT
C                                       Square plot.
      XRANGE = PTRC(1) - PBLC(1)
      XY = YRANGE / XRANGE
C                                       Write intialization records
C                                       into plot file.
C                                       initialize line drawing
      CALL GINITL (PBLC, PTRC, XY, CHOUT, IDEPT, PLTBLK, IERR)
C
 999  RETURN
      END
      SUBROUTINE PLABL2 (IERR)
C-----------------------------------------------------------------------
C   This program uses the values set in PLINI2 and passed through
C   commons LOCATI, LOCATC, and PLTCOM to do the axes labeling.
C   Commons:
C      /LOCATI/  (from incs:DLOC.INC)
C      /LOCATC/  (from incs:DLOC.INC)
C      /PLTCOM/  (from incs:DPLT.INC)
C   Output:
C      IERR    I      Error code. 0=ok.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      LOGICAL   F
      REAL      DCX, DCY
      INTEGER   I, IANGLE, INCHAR, IT(3), ID(3), LABEL, LTYPE
      CHARACTER WRKTXT*80, ATIME*8, ADATE*12, ANAME*18
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'PFPL2.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      LABEL = PCODES(1)
      LTYPE = MOD (ABS(PCODES(1)), 100)
      CALL GLTYPE (1, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Tics and tic labels
      CALL CLAB1 (PBLC, PTRC, CHOUT, LABEL, XY, F, PLTBLK, IERR)
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
C                                       title line
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
C                                       Standard map title line.
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
      IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) THEN
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, ATIME, ADATE)
         WRITE (WRKTXT,1020) IVER, ADATE, ATIME
         DCY = DCY + 1.333
         CALL GPOS (PBLC(1), PTRC(2), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL REFRMT (WRKTXT, '_', INCHAR)
         CALL GCHAR (INCHAR, IANGLE, DCX, DCY, WRKTXT, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Label X axis.
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
      IF ((NTEXT2.GT.0) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7))
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
 1020 FORMAT ('PLOT FILE VERSION',I4,'__CREATED ',A,A)
      END
