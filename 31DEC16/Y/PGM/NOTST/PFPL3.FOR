LOCAL INCLUDE 'PFPL3.INC'
C                                       PLTLAB declarations.
      REAL       CHOUT(4), YGAP
      INTEGER    NTEXT, PCODES(5)
      CHARACTER  TEXT(2)*80, XUNIT*8, YUNIT*8, TITLE*80
C                                       Plot labeling parameters.
      COMMON /PLTLAB/  CHOUT, YGAP, NTEXT, PCODES
      COMMON /PLCLAB/ TEXT, XUNIT, YUNIT, TITLE
C                                       INPARM declarations.
      REAL      PRUSER, NAMIN(3), CLSIN(2), SEQIN, DSKIN, BLC(7),
     *   TRC(7), PIXRNG(2), XLTYPE, XDOTV, XGRCH
C                                       Parameters from AIPS.
      COMMON /INPARM/ PRUSER, NAMIN, CLSIN, SEQIN, DSKIN, BLC, TRC,
     *   PIXRNG, XLTYPE, XDOTV, XGRCH
C
LOCAL END
      PROGRAM PFPL3
C-----------------------------------------------------------------------
C! Plot paraform task: plot axes have little or no relation to map axes.
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
C   PFPL3 (paraform plot program 3) is designed to be a model for
C   writing tasks to do plotting in AIPS.  This program is designed
C   for reading a map and creating a plot file to be associated
C   with the map.  This task assumes that the plot axes have little
C   or no relation to the axes of the map.  Plot labels come from
C   a data statement found in the main program.  An example of this
C   type of plot is the histogram produced by task IMEAN.  For other
C   types of plots see programs PFPL1 and PFPL2.
C   The program will do the following steps:
C     1. Open a map file corresponding to the user's inputs from AIPS.
C     2. Create an extension file of type PL (plot) to be associated
C        with the map file.  The header of the map file will be
C        updated to include this new extension file.
C     3. Write the plot file records to draw the borders and labels
C        of the plot.  The programmer can customize this section of
C        the program by changing data statements in subroutine PLTOR3.
C     4. Write the rest of the plot file records to the plot file.
C        This is done by subroutine PLTOR3.  The current routine
C        draws a simple histogram for map intensities.  The programmer
C        will have to modify the code in PLTOR3 for his/her needs.
C     5. Do the necessary clean up functions, write end of plot
C        records, close all files, etc.
C
C   IMPORTANT NOTE: to avoid confusion, this task should be renamed.
C   To rename (max. 5 char) and install the new task:
C     1. Copy the source code to a new file with the name
C        NEWNAME.FOR.
C     2. Using the source editor, change all references to PFPL3 to
C        NEWNAME.  It is especially important to change the string
C        entered into array PRGM at or near line 61 to the new name.
C     3. Copy help/inputs file PFPL3.HLP to inputs file NEWNAME.HLP.
C        Update the inputs, help and explain sections.
C     4. Compile and link edit with the APL and NOTST subroutine
C        libraries from AIPS.
C     5. Modify the comments at the top of this program to help
C        others understand what the program does.
C     6. Change the assignment statements to array variable PCODES
C        and the data statements for NTEXT, TEXT, XUNIT, YUNIT and
C        TITLE in the main program to get the type of axis labeling
C        you want.  Then add code to PLTOR3 to do the plotting.
C     7. While debugging your program, set IDEBUG to 1 to have plot
C        files kept despite program failures.  DUMP routines can be
C        run on them to determine what did go to the plot file and
C        and to help figure out what went wrong.  Set IDEBUG to 0
C        before releasing the program to other users.
C
C    Inputs:   (from AIPS)
C       USERID    R      user number, ignored
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
C       PIXRANGE  R(2)   Sets the upper and lower limit on intensity of
C                        values to appear on the histogram.  0, 0
C                        defaults to range in map header.
C       LTYPE     R      Label type.
C       DOTV     R      > 0 => TV, else plot file
C       GRCHAN   R      graphics channel to use (0 => 1)
C-----------------------------------------------------------------------
C                                       local declarations
      CHARACTER ATEXT(2)*80, AXUNIT*8, AYUNIT*8, ATITLE*80, PRGNAM*6
      INTEGER   IMLUN, NPARMS, IERR, INTEXT, IDEBUG, IPTYPE, IROUND, I
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'PFPL3.INC'
      DATA IMLUN /16/
      DATA PRGNAM /'PFPL3 '/
C                                       Data statements to control the
C                                       text at the bottom of the plot.
C                                       INTEXT is the number of text
C                                       lines.
      DATA INTEXT /2/
      DATA ATEXT /'This is some text at the bottom of the plot ',
     *     'This is the second line '/
C                                       Units to use for X and Y axis.
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
      NPARMS = 27
C                                       Plot type PFPL3 paraform
      IPTYPE = 11
C                                       Get parms from AIPS, open map
C                                       file, create plot file,
      CALL PF3INI (PRGNAM, NPARMS, IMLUN, IPTYPE, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       These are parameters to
C                                       control labeling.
C
C                                       Labeling type:
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
C                                       Codes 2 and 3 are not
C                                       used for this example program.
C                                       They are available for use
C                                       by the programmer.
      PCODES(2) = 0
      PCODES(3) = 0
C                                       Use "standard" title line
      PCODES(4) = 0
C                                       No Grey scales.
      PCODES(5) = 0
C                                       Do plotting.  This routine must
C                                       be modified by the programmer.
      CALL PLTOR3 (IERR)
C                                       Shutdown.
 900  CALL PLEND (IERR, IDEBUG)
C
 999  STOP
      END
      SUBROUTINE PF3INI (PRGNAM, NPARMS, IMLUN, IPTYPE, IERR)
C-----------------------------------------------------------------------
C   This routine does all the intial set up.  Get parms from AIPS,
C   open the map file, create the plot file and write the plot file
C   records to do the plot labeling.
C   Inputs:
C      PRGNAM C*6    Name of this program.
C      NPARMS I      Number of R words to get from AIPS.
C      IMLUN  I      The logical unit number to use for the map file.
C      IPTYPE I      Plot file type: 1 misc., 2 CNTR, 3 GREYS, 4 PROFL,
C                    5 SL2PL, 6 PCNTR, 7 IMEAN (hist), 8 UVPLT,
C                    9 GNPLT, 10 VBPLT, 11 PFPLn, 12 GAPLT, 13 PLCUB,
C                    14 IMVIM, 15 TAPLT.  Use 1 unless your inputs
C                    match those of these tasks - or take a new number,
C                    but AIPSUB:AU8A will need to know about it too.
C   Output:
C      IERR   I      Error code. 0=ok.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   NPARMS, IMLUN, IPTYPE, IERR
C
      CHARACTER NAME*36, FNAME*12, FCLASS*6, FPTYPE*2, STAT*4
      INTEGER   IWORK(256), FSEQ, FVOL, FUSID
      REAL      TEMP
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'PFPL3.INC'
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
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
      STAT = 'HDWR'
      IF (DOTV) STAT = 'READ'
      CALL A2WAWA (FNAME, FCLASS, FSEQ, 'MA', FVOL, FUSID, NAME)
      CALL INTMIO (IMLUN, STAT, NAME, BLC, TRC, IBLKSZ, CATBLK,
     *   IMSTUF, IERR)
      IF (IERR.NE.0) GO TO 999
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
      SUBROUTINE PLTOR3 (IERR)
C-----------------------------------------------------------------------
C   This routine writes all of the plot commands (including the axis
C   drawing and labeling commands) into the plot file.
C   ******* This version contains a sample (histogram) ********
C   Input from common:
C      BLC       R(7)   Bottom left corner of plot.
C      TRC       R(7)   Top right corner of plot.
C   Output: IERR    I    Error code.  0=ok.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'INCS:PMAD.INC'
      REAL      X, Y, ROW(MAXIMG), BUCKET(10), BINTVL, XINDEX, XMAX
      INTEGER   I, NVAL, II, INDEX, IROUND, IUNDER, IOVER
      LOGICAL   EOF
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'PFPL3.INC'
C-----------------------------------------------------------------------
C                                       ** Plot specific statements.
C                                       Zero counters for number of
C                                       values outside range.
      IUNDER = 0
      IOVER = 0
C                                       Zero buckets.
      DO 10 I = 1,10
         BUCKET(I) = 0.0
 10      CONTINUE
C                                       Find bucket interval.
      BINTVL = (RANGE(2) - RANGE(1)) / 10.0
C                                       Number of values in a map row.
      NVAL = IMSTUF(9)
C                                       Loop for all rows.
      DO 30 I = 1,32000
C                                       Read a map row.
         CALL GETROW (IMSTUF, IOBLK, ROW, EOF, IERR)
         IF (EOF) GO TO 50
C                                       Find no. of values in each
C                                       range for this row.
         DO 20 II = 1,NVAL
            IF (ROW(II).NE.FBLANK) THEN
               XINDEX = (ROW(II) - RANGE(1)) / BINTVL  +  .5
               INDEX = IROUND(XINDEX)
C                                       Special case when XINDEX is
C                                       exactly at high end.
               IF (XINDEX.EQ.10.5) INDEX = 10
               IF (INDEX.LT.1) IUNDER = IUNDER + 1
               IF (INDEX.GT.10) IOVER = IOVER + 1
               IF ((INDEX.GE.1) .AND. (INDEX.LE.10)) BUCKET(INDEX) =
     *            BUCKET(INDEX) + 1.0
               END IF
 20         CONTINUE
 30     CONTINUE
C                                       Find maximum value.
 50   XMAX = 10.0
      DO 55 I = 1,10
         XMAX = MAX (XMAX, BUCKET(I))
 55      CONTINUE
C                                       Set corner values. Constant 1.1
C                                       chosen for esthetics.
      BLC(1) = RANGE(1)
      TRC(1) = RANGE(2)
      BLC(2) = 0.0
      TRC(2) = 1.1 * XMAX
C                                       Fill in units for XUNIT & YUNIT.
      WRITE (YUNIT,1400)
      CALL H2CHR (8, 1, CATH(KHBUN), XUNIT(1:8))
C                                       Fill in TEXT at bottom of plot.
      WRITE (TEXT(1),1420) IUNDER
      WRITE (TEXT(2),1430) IOVER
      NTEXT = 2
C                                       Draw a square plot.
      XY = (TRC(2) - BLC(2)) / (TRC(1) - BLC(1))
C                                       ** End plot specific statements.
C                                       Set up commons for plotting.
      CALL PLINI3 (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Do axis labeling.
      CALL PLABL3 (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       ** Plot specific statements.
C                                       Draw histogram.
      CALL GLTYPE (2, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      X = BLC (1)
      Y = BLC (2)
      CALL PLPOS (X, Y, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 500 I = 1,10
         Y = BUCKET(I)
         CALL PLVEC (X, Y, IERR)
         IF (IERR.NE.0) GO TO 999
         X = X + BINTVL
         CALL PLVEC (X, Y, IERR)
         IF (IERR.NE.0) GO TO 999
         Y = BLC(2)
         CALL PLVEC (X, Y, IERR)
         IF (IERR.NE.0) GO TO 999
 500     CONTINUE
C                                       ** End plot specific statements.
 999  RETURN
C-----------------------------------------------------------------------
 1400 FORMAT ('PIXELS')
 1420 FORMAT ('PIXELS LESS    THAN PIXRANGE(1) = ',I8)
 1430 FORMAT ('PIXELS GREATER THAN PIXRANGE(2) = ',I8)
      END
      SUBROUTINE PLINI3 (IERR)
C-----------------------------------------------------------------------
C   This routine will set up the location commons for the plot file.
C   Inputs from common:
C      BLC     R(2)    Bottom left corner of plot.
C      TRC     R(2)    Top right corner of plot.
C   Output:
C      IERR    I       Error code. 0=OK.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      REAL      XRANGE, YRANGE, XR, YR, BIGNUM, XHPVAL, XLPVAL
      INTEGER   DEPT(5), ILPVAL, IHPVAL, I, IROUND, LABEL, LTYPE
      LOGICAL   PFLG, F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'PFPL3.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      LABEL = PCODES(1)
      LTYPE = MOD (ABS(LABEL), 100)
C                                       Set up default scaling parms.
      XSCAL = 1.0
      YSCAL = 1.0
      XOFF = 0.0
      YOFF = 0.0
C                                       X and Y plot axis not related
C                                       to map axis.
      IF (XY.EQ.0.0) XY = 1.0
      DO 20 I = 1,5
         DEPT(I) = 1
 20      CONTINUE
      LOCNUM = 1
      CALL SETLOC (DEPT, F)
      AXFUNC(1,LOCNUM) = 0
      AXFUNC(2,LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
C                                        proper scaling labels
      XRANGE = TRC(1) - BLC(1)
      YRANGE = TRC(2) - BLC(2)
C                                       Some kind of error
      IF ((XRANGE.LE.0.0) .OR. (YRANGE.LE.0.0)) THEN
         IERR = 5
         GO TO 999
         END IF
      XR = XRANGE
      CALL METSCL (LABEL, XR, CPREF(1,LOCNUM), PFLG)
      YR = YRANGE
      CALL METSCL (LABEL, YR, CPREF(2,LOCNUM), PFLG)
C                                        proceed filling /LOCATI/
      CALL RCOPY (2, BLC, PBLC)
      CALL RCOPY (2, TRC, PTRC)
C                                       See if we need to rescale to
C                                       prevent integer overflow in
C                                       plot routines (plot file values
C                                       are integers).
C                                       Rescale X values
      IF ((XRANGE.GE.16000.0) .OR. (XRANGE.LE.10.0)) THEN
         XSCAL = 16000.0 / XRANGE
         XOFF = - BLC(1) * XSCAL
         PTRC(1) = 16000.0
         PBLC(1) = 0.0
         END IF
C                                       Rescale Y values
      IF ((YRANGE.GE.16000.0) .OR. (YRANGE.LE.10.0)) THEN
         YSCAL = 16000.0 / YRANGE
         YOFF = - BLC(2) * YSCAL
         PTRC(2) = 16000.0
         PBLC(2) = 0.0
         END IF
      XY = XY * (YSCAL / XSCAL)
      RPLOC(1,LOCNUM) = PBLC(1)
      RPLOC(2,LOCNUM) = PBLC(2)
      RPVAL(1,LOCNUM) = BLC(1) * XR / XRANGE
      RPVAL(2,LOCNUM) = BLC(2) * YR / YRANGE
      AXINC(1,LOCNUM) = XR / XRANGE / XSCAL
      AXINC(2,LOCNUM) = YR / YRANGE / YSCAL
      CTYP(1,LOCNUM) = XUNIT
      CTYP(2,LOCNUM) = YUNIT
C                                       Left border in characters
      CHOUT(1) = 0.5
      IF (LABEL.EQ.2) CHOUT(1) = 2.5
      IF (LABEL.GT.2) THEN
         CHOUT(1) = 2.0
         CALL CHNTIC (PBLC, PTRC, I)
         IF (I.GT.0) CHOUT(1) = 3.333 + I
         END IF
C                                       Bottom border in characters
      CHOUT(2) = 0.5
      IF (LABEL.GT.1) CHOUT(2) = CHOUT(2) + 1.5
      IF (LABEL.GT.2) CHOUT(2) = CHOUT(2) + 1.333
      YGAP = CHOUT(2) - 0.5 + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CHOUT(2) =
     *    CHOUT(2) + NTEXT * 1.333
C                                       Right border in characters
      CHOUT(3) = 0.5
C                                       Top border in characters
      CHOUT(4) = 0.5
      IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) = CHOUT(4) + 1.5
      IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) =
     *   CHOUT(4) + 1.333
C                                       Write intialization records
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
C                                       Scale range for GINITG.
         XHPVAL = GFAC * RANGE(2) + GOFF
         XLPVAL = GFAC * RANGE(1) + GOFF
         IHPVAL = IROUND (XHPVAL)
         ILPVAL = IROUND (XLPVAL)
         CALL GINITG (ILPVAL, IHPVAL, PIXRNG, PLTBLK, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE PLABL3 (IERR)
C-----------------------------------------------------------------------
C   This program uses the values set in PLINI3 and passed through
C   commons LOCATI and PLTCOM to do the axes labeling.
C   Inputs from common:
C      /LOCATI/  (from incs:DLOC.INC)
C      /LOCATC/  (from incs:DLOC.INC)
C      /PLTCOM/  (from incs:DPLT.INC)
C   Output:
C      IERR   I      Error code. 0=ok.
C-----------------------------------------------------------------------
      INTEGER  IERR
C
      REAL      DCX, DCY
      INTEGER   I, IANGLE, INCHAR, IT(3), ID(3), LABEL, LTYPE
      CHARACTER WRKTXT*80, ATIME*8, ADATE*12, ANAME*18
      LOGICAL   F
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'PFPL3.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      LABEL = PCODES(1)
      LTYPE = MOD (ABS(LABEL), 100)
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
         DCY = .5
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
C                                       Text at bottom.
      IF ((NTEXT.GT.0) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7))
     *   THEN
         DCX = 0.0
         IANGLE = 0
         DO 50 I = 1,NTEXT
            WRKTXT = TEXT(I)
            CALL CHTRIM (WRKTXT, 80, WRKTXT, INCHAR)
            DCY = -YGAP
            YGAP = YGAP + 1.333
            CALL GPOS (PBLC(1), PBLC(2), PLTBLK, IERR)
            CALL GCHAR (INCHAR, IANGLE, DCX, DCY, WRKTXT, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
 50         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('PLOT FILE VERSION',I4,'__CREATED ',A,A)
      END
