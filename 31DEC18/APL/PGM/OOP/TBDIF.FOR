LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER   NPARMS
C                                       NPARMS=no. adverbs passed.
      PARAMETER (NPARMS=19)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                                       NOTE: Uses values in PAOOF.INC
C                                       Adverb names
C                     1         2          3        4         5
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INEXT',
C           6         7          8           9         10
     *   'INVERS', 'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK',
C           11         12        13        14         15        16
     *   'IN2VERS', 'BCOUNT', 'ECOUNT', 'COMPCOL', 'SELCOL', 'DOALL',
C           17       18          19
     *   'DOCRT', 'OUTPRINT', 'BADDISK'/
C                                       Adverb data types (PAOOF.INC)
C                    1       2       3       4       5
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOACAR,
C          6       7       8       9       10
     *   OOAINT, OOACAR, OOACAR, OOAINT, OOAINT,
C          11      12      13      14      15      16
     *   OOAINT, OOAINT, OOAINT, OOACAR, OOACAR, OOARE,
C          17     18      19
     *   OOARE, OOACAR, OOAINT/
C                                       Adverb dimensions (as 2D)
C                   1    2    3    4    5
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 2,1,
C         6     7    8    9    10
     *   1,1, 12,1, 6,1, 1,1, 1,1,
C         11   12   13    14     15    16
     *   1,1, 1,1, 1,1, 16,30, 16,30, 1,1,
C         17    18    19
     *   1,1, 48,1, 10,1/
LOCAL END
LOCAL INCLUDE 'TBDIF.INC'
C                                       Local include for TBDIF print
C                                       info.
C                                       MAXCOL = max number cols.
      INTEGER   MAXCOL
      PARAMETER (MAXCOL=128)
      INTEGER   NCOL, COLTYP(MAXCOL), COLDIM(MAXCOL), NPSEL,
     *   SELCOL(MAXCOL), SELSTR(MAXCOL), SELLEN(MAXCOL), ONESTR, ONELEN,
     *   TWOSTR, TWOLEN, DIFSTR, DIFLEN, MAXLIN
      CHARACTER TITLE(MAXCOL)*24, UNITS(MAXCOL)*8
C                                        Table I/O arrays
      INTEGER   IVAL1(1024), IVAL2(1024)
      CHARACTER CVAL1*1024, CVAL2*1024
      LOGICAL   LVAL1(1024), LVAL2(1024)
      REAL      RVAL1(1024), RVAL2(1024)
      DOUBLE PRECISION DVAL1(1024), DVAL2(1024)
      EQUIVALENCE (IVAL1, RVAL1, LVAL1, DVAL1),
     *   (IVAL2, RVAL2, LVAL2, DVAL2)
C                                       Summary info
      INTEGER   MAXSUM
C                                       MAXSUM = max entries in summary.
      PARAMETER (MAXSUM = 100)
      INTEGER   NUMSUM, SUMCNT(MAXSUM)
      DOUBLE PRECISION SUMMAX(MAXSUM), SUMMIN(MAXSUM), SUMSUM(MAXSUM),
     *   SUMSU2(MAXSUM)
C                                       Commons
      COMMON /LOCNUM/ DVAL1, DVAL2, NCOL, COLTYP, COLDIM, NPSEL, SELCOL,
     *   SELSTR, SELLEN, ONESTR, ONELEN, TWOSTR, TWOLEN, DIFSTR, DIFLEN,
     *   MAXLIN
      COMMON /SUMCOM/ SUMMAX, SUMMIN, SUMSUM, SUMSU2, NUMSUM, SUMCNT
      COMMON /LOCCHR/ TITLE, UNITS, CVAL1, CVAL2
LOCAL END
      PROGRAM TBDIF
C-----------------------------------------------------------------------
C! Compare selected columns and rows in two similar tables
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999, 2004, 2007, 2009, 2015
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
C   Compare selected columns and rows in two similar tables
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, INTAB*36, IN2TAB*36
      INTEGER   IRET, BUFF1(256)
      INCLUDE 'TBDIF.INC'
      DATA PRGM /'TBDIF '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL DIFTIN (PRGM, INTAB, IN2TAB, IRET)
C                                       Process table
      IF (IRET.EQ.0) CALL DIFTAB (INTAB, IN2TAB, IRET)
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE DIFTIN (PRGN, INTAB, IN2TAB, IRET)
C-----------------------------------------------------------------------
C   DIFTIN gets input parameters for TBDIF and creates the input objects
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IRET    I    Error code: 0 => ok
C                               4 => user routine detected error.
C                               5 => catalog troubles
C                               8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS file
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, INTAB*36, IN2TAB*36
C
      INTEGER   NKEY1, NKEY2, TYPE, DIM(7)
C                                       NKEY1=no. adverbs to copy to
C                                       INTAB
      PARAMETER (NKEY1=13)
C                                       NKEY2=no. adverbs to copy to
C                                       IN2TAB
      PARAMETER (NKEY2=6)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to INTAB
C                   1         2          3        4         5
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INEXT',
C           6         7         8         9          10        11
     *   'INVERS', 'BCOUNT', 'ECOUNT', 'COMPCOL', 'SELCOL', 'DOCRT',
C           12          13
     *   'OUTPRINT', 'DOALL'/
C                                       May rename adverbs to INTAB
C                    1       2        3        4       5
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'TBLTYPE',
C           6      7         8         9          10        11
     *   'VER', 'BCOUNT', 'ECOUNT', 'COMPCOL', 'SELCOL', 'DOCRT',
C           12          13
     *   'OUTPRINT', 'DOALL'/
C                                       Adverbs to copy to IN2TAB
C                   1          2           3         4          5
      DATA INK2 /'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK', 'INEXT',
C           6
     *   'IN2VERS'/
C                                       May rename adverbs to IN2TAB
C                    1       2        3        4       5
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'TBLTYPE',
C           6
     *   'VER'/
C-----------------------------------------------------------------------
C                                       Startup,  returns "Input" object
C                                       containing POPS adverbs
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BADDISK
      CALL OGET ('Input', 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create input object
      INTAB = 'First input table'
      CALL CREATE (INTAB, 'TABLE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, INTAB, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create Output Object
      IN2TAB = 'Second input table'
      CALL CREATE (IN2TAB, 'TABLE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, IN2TAB, IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE DIFTAB (INTAB, IN2TAB, IERR)
C-----------------------------------------------------------------------
C   Compare table.
C   Inputs:
C      INTAB   C*   Name of input table object.
C      IN2TAB  C*   Name of output table object.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*), IN2TAB*(*)
      INTEGER   IERR
C
      INTEGER   PLUN, PIND, BUFF(256), NACROS, IPCNT, PAGE, TYPE,
     *   DIM(3), ICOL, NCOL, CCOLS(30), NSEL, SCOLS(30), ROW, BC, EC,
     *   NROW, IN2, N2ROW, FSTAB2, FSTMP, DUMMY
      LOGICAL   MATCH, TBCOMP, PAST
      REAL      DOCRT, DOALL
      CHARACTER LPNAME*48, TITL1*132, TITL2*132, SCRTCH*132, LINE*132
      CHARACTER SELLAB(30)*16, CSLL*(16*30), CDUMMY*1
      EQUIVALENCE (SELLAB, CSLL)
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Sort tables
C                                       Lookup columns for selection.
      CALL OGET (INTAB, 'SELCOL', TYPE, DIM, DUMMY, CSLL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Sort by first selection column
      CALL TBLSRT (INTAB, SELLAB(1), SELLAB(1), IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TBLSRT (IN2TAB, SELLAB(1), SELLAB(1), IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open input table
      CALL OOPEN (INTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Look up relevant columns, CCOLS
C                                        are the columns to compare and
C                                        SCOLS are those for the
C                                        selection.
      CALL THEAD (INTAB, NCOL, CCOLS, NSEL, SCOLS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Open output table
      CALL OOPEN (IN2TAB, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open printer
      CALL OGET (INTAB, 'OUTPRINT', TYPE, DIM, DUMMY, LPNAME, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'DOALL', TYPE, DIM, DOALL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'DOCRT', TYPE, DIM, DOCRT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, PLUN, PIND, NACROS, BUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (INTAB, 'DOCRT', TYPE, DIM, DOCRT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get number of entries to compare
      CALL OGET (INTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'BCOUNT', TYPE, DIM, BC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      BC = MIN (MAX (BC, 1), NROW)
      CALL OGET (INTAB, 'ECOUNT', TYPE, DIM, EC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (EC.LE.0) EC = NROW
      EC = MIN (EC, NROW)
      CALL OGET (IN2TAB, 'NROW', TYPE, DIM, N2ROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      PAGE = 0
C                                       Loop over comparison columns
      DO 500 ICOL = 1,NCOL
C                                       Write header info
         CALL PRTHED (INTAB, IN2TAB, CCOLS(ICOL), NSEL, SCOLS, PLUN,
     *      PIND, DOCRT, NACROS, TITL1, TITL2, LINE, IPCNT, PAGE,
     *      SCRTCH, IERR)
C                                       Done?
         IF (IERR.LT.0) GO TO 600
         IF (IERR.NE.0) GO TO 999
C                                       Initialize beginning row in
C                                       second table
         FSTAB2 = 1
C                                       Compare tables
         DO 200 ROW = BC,EC
C                                       Get row number of first match in
C                                       first selection column.
            FSTMP = -1
C                                       Loop through second input table
            DO 100 IN2 = FSTAB2,N2ROW
C                                       Is this a match?
               MATCH = TBCOMP (INTAB, IN2TAB, ROW, IN2, NSEL, SCOLS,
     *            FSTMP, PAST, IERR)
               IF (IERR.NE.0) GO TO 999
C                                       Too far?
               IF (PAST) GO TO 150
               IF (MATCH) THEN
C                                       Display comparison
                  CALL PRTCOM (INTAB, IN2TAB, ROW, IN2, CCOLS(ICOL),
     *               NSEL,  PLUN, PIND, DOALL, DOCRT, NACROS, TITL1,
     *               TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
C                                       Done?
                  IF (IERR.LT.0) GO TO 600
                  IF (IERR.NE.0) GO TO 999
                  GO TO 150
                  END IF
 100           CONTINUE
C                                       Update first row for search in
C                                       second table.
 150           IF (FSTMP.GT.FSTAB2) FSTAB2 = FSTMP
 200        CONTINUE
C                                       Print comparison summary if
C                                       appropriate.
         CALL PRTSUM (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
C                                       Done?
         IF (IERR.LT.0) GO TO 600
         IF (IERR.NE.0) GO TO 999
C                                       End comparison col loop
 500     CONTINUE
C                                       Close printer
 600  IERR = 0
      CALL LPCLOS (PLUN, PIND, IPCNT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close tables
      CALL OCLOSE (INTAB, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OCLOSE (IN2TAB, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE THEAD (INTAB, NCOL, CCOLS, NSEL, SCOLS, IERR)
C-----------------------------------------------------------------------
C   Lookup comparison and selection columns
C   Inputs:
C      INTAB   C*?  Name of first input table object
C   Output:
C      NCOL    I    The number of columns to compare
C      CCOLS   I(*) List of column numbers to compare
C      NSEL    I    Number of selection rows.
C      SCOLS   I(*) Columns numbers of the selection columns
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*)
      INTEGER   NCOL, CCOLS(*), NSEL, SCOLS(*), IERR
C
C                                        MAXCOL = max number cols.
      INTEGER   MAXCOL
      PARAMETER (MAXCOL=128)
      CHARACTER COLLAB(30)*16, CLAB*(16*30), TLABEL(MAXCOL)*24,
     *   TLAB*(24*MAXCOL), COL*24, CDUMMY*1
      INTEGER   TYPE, DIM(7), I, J, NC, NICOL, IEND, JEND, DUMMY, ITRIM,
     *   IL
      LOGICAL   BADCOL
      INCLUDE 'INCS:DMSG.INC'
C                                       Need to equivalence character
C                                       arrays for OGET
      EQUIVALENCE (COLLAB, CLAB), (TLABEL, TLAB)
C-----------------------------------------------------------------------
      NCOL = 0
      NSEL = 0
      BADCOL = .FALSE.
C                                       Fetch Input table info
      CALL OGET (INTAB, 'NCOL', TYPE, DIM, NC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'COLABEL', TYPE, DIM, DUMMY, TLAB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Lookup columns for comparison.
      CALL OGET (INTAB, 'COMPCOL', TYPE, DIM, DUMMY, CLAB, IERR)
      IF (IERR.NE.0) GO TO 999
      NICOL = DIM(2)
C                                        Look up columns in input
      DO 200 I = 1,NICOL
C                                        Blank entry?
         IF (COLLAB(I).NE.' ') THEN
C                                        Given column number?
            IF (COLLAB(I)(1:5) .EQ. 'COL#(') THEN
C                                       Parse column number.
               JEND = INDEX (COLLAB(I), ')') - 1
C                                        Found closing )?
               IF (JEND.LE.0) GO TO 110
               IEND = MAX (JEND, 6)
               COL = '        '
               COL(14-IEND:8) = COLLAB(I)(6:IEND)
               READ (COL,1000,ERR=110) J
C                                       Valid column number?
               IF ((J.LE.0) .OR. (J.GT.NC)) THEN
C                                        Invalid column number
                  MSGTXT = 'INVALID COLUMN NUMBER: ' // COLLAB(I)
                  CALL MSGWRT (6)
                  BADCOL = .TRUE.
                  GO TO 200
                  END IF
C                                       Looks OK - process
               GO TO 120
            ELSE
C                                       Check list of column names
               DO 100 J = 1,NC
                  IF (COLLAB(I).EQ.TLABEL(J)(1:16)) GO TO 120
 100              CONTINUE
               IL = ITRIM (COLLAB(I))
               DO 105 J = 1,NC
                  IF (COLLAB(I)(:IL).EQ.TLABEL(J)(:IL)) GO TO 120
 105              CONTINUE
               END IF
C                                        If it got here the column
C                                        wasn't found.
            MSGTXT = 'COULD NOT FIND COLUMN: '// COLLAB(I)
            CALL MSGWRT (6)
            MSGTXT = 'CHECK SPELLING, IGNORING REQUEST'
            CALL MSGWRT (6)
            BADCOL = .TRUE.
            GO TO 200
C                                        Decode error for col.
 110        MSGTXT = 'ERROR DECODING COLUMN NUMBER: ' // COLLAB(I)
            CALL MSGWRT (6)
            BADCOL = .TRUE.
            GO TO 200
C                                        OK: Save column number
 120        NCOL = NCOL + 1
            CCOLS(NCOL) = J
            END IF
 200     CONTINUE
C                                        Better have some columns
      IF (NCOL.LE.0) THEN
         MSGTXT = 'ERROR: NO COMPARISON COLUMNS SELECTED'
         CALL MSGWRT (8)
         IERR = 5
         GO TO 999
         END IF
C                                       Lookup columns for selection.
      CALL OGET (INTAB, 'SELCOL', TYPE, DIM, DUMMY, CLAB, IERR)
      IF (IERR.NE.0) GO TO 999
      NICOL = DIM(2)
C                                        Look up columns in input
      DO 400 I = 1,NICOL
C                                        Blank entry?
         IF (COLLAB(I).NE.'     ') THEN
C                                        Given column number?
            IF (COLLAB(I)(1:5) .EQ. 'COL#(') THEN
C                                       Parse column number.
               JEND = INDEX (COLLAB(I), ')') - 1
C                                        Found closing )?
               IF (JEND.LE.0) GO TO 310
               IEND = MAX (JEND, 6)
               COL = '        '
               COL(14-IEND:8) = COLLAB(I)(6:IEND)
               READ (COL,1000,ERR=310) J
C                                       Valid column number?
               IF ((J.LE.0) .OR. (J.GT.NC)) THEN
C                                        Invalid column number
                  MSGTXT = 'INVALID COLUMN NUMBER: ' // COLLAB(I)
                  CALL MSGWRT (6)
                  BADCOL = .TRUE.
                  GO TO 400
                  END IF
C                                       Looks OK - process
               GO TO 320
            ELSE
C                                       Check list of column names
               DO 300 J = 1,NC
                  IF (COLLAB(I).EQ.TLABEL(J)(1:16)) GO TO 320
 300              CONTINUE
               IL = ITRIM (COLLAB(I))
               DO 305 J = 1,NC
                  IF (COLLAB(I)(:IL).EQ.TLABEL(J)(:IL)) GO TO 320
 305              CONTINUE
               END IF
C                                        If it got here the column
C                                        wasn't found.
            MSGTXT = 'COULD NOT FIND COLUMN: '// COLLAB(I)
            CALL MSGWRT (6)
            MSGTXT = 'CHECK SPELLING, IGNORING REQUEST'
            CALL MSGWRT (6)
            BADCOL = .TRUE.
            GO TO 400
C                                        Decode error for col.
 310        MSGTXT = 'ERROR DECODING COLUMN NUMBER: ' // COLLAB(I)
            CALL MSGWRT (6)
            BADCOL = .TRUE.
            GO TO 400
C                                        OK: Save column number
 320        NSEL = NSEL + 1
            SCOLS(NSEL) = J
            END IF
 400     CONTINUE
C                                        Better have some columns
      IF (NSEL.LE.0) THEN
         MSGTXT = 'ERROR: NO SELECTION COLUMNS SELECTED'
         CALL MSGWRT (8)
         IERR = 5
         GO TO 999
         END IF
C                                        Warn if bad columns detected.
      IF (BADCOL) THEN
         MSGTXT = 'Warning: Problems were encountered in selecting' //
     *      ' some columns'
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I8)
      END
      SUBROUTINE PRTHED (INTAB, IN2TAB, COL, NSEL, SCOLS, PLUN, PIND,
     *   DOCRT, NACROS, TITL1, TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
C-----------------------------------------------------------------------
C   Prints a header and prepares page titles for the comparison of a
C   given column.
C   Inputs:
C      INTAB   C*?  Name of first input table object
C      IN2TAB  C*?  Name of second input table object
C      COL     I    Column number to compare.
C      NSEL    I    Number of selection rows.
C      SCOLS   I(*) Columns numbers of the selection columns
C      PLUN    I       LUN for print device (open): 1 => line printer,
C                         3 => text (disk) file, 5 => terminal
C      PIND    I       FTAB pointer for print device
C      DOCRT   R       > 0. => use CRT, else line printer or disk
C      NC      I       Number characters in line
C      TITL1   C*132   Page title line 1, if ' ' then ignored
C      TITL2   C*132   Page title line 2, if ' ' then ignored
C      LINE    C*132   Text line, printed even if ' '
C   In/out:
C      IPCNT   I       Number lines so far on page
C                         > 1000 => just ask about continuing (DOCRT
C                                   true).  Do not print anything.
C                         =  999 => print new page title(s) and LINE
C                                   but do not ask the user about
C                                   whether he wants to continue
C      PAGE    I       Current page number
C                         = 0 => start of print job (user not asked on
C                                DOCRT true)
C   Output:
C      SCRTCH  C*(*)   Scratch core > 132
C      IERR    I       Error code: 0 => OK, -1 user asks to quit
C   Hints:
C      (1) First call: set IPAGE = 0, IPCNT = (e.g.) 990
C      (2) Thereafter, leave IPCNT and IPAGE alone in your code, unless
C      (3) to force a page break, reset IPCNT to (e.g.) 990
C   where 990 is an arbitrary number < 999 and > number of lines on the
C   printer page.
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*), IN2TAB*(*), TITL1*132, TITL2*132, SCRTCH*132,
     *   LINE*132
      INTEGER   COL, NSEL, PLUN, PIND, NACROS, IPCNT, PAGE, IERR
      INTEGER   SCOLS(NSEL)
      REAL      DOCRT
C
      CHARACTER NAME*12, CLASS*6, TTYPE*2, CDUMMY*1
      INTEGER   SEQ, DISK, VER, TYPE, DIM(7), I, J, DUMMY
      INCLUDE 'TBDIF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Setup columns in output
      CALL PRTSCL (INTAB, NACROS, COL, NSEL, SCOLS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Initialize summary
      CALL SUMINI (COL)
C                                       Labels
      TITL1 = ' '
      TITL2 = ' '
      LINE = ' Comparison of ' // TITLE(COL) // ' column'
      IPCNT = 990
      IF (DOCRT.GT.-2.5) THEN
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Tell which table being compared.
C                                       First table
         CALL OGET (INTAB, 'NAME', TYPE, DIM, DUMMY, NAME, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OGET (INTAB, 'CLASS', TYPE, DIM, DUMMY, CLASS, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OGET (INTAB, 'IMSEQ', TYPE, DIM, SEQ, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OGET (INTAB, 'DISK', TYPE, DIM, DISK, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OGET (INTAB, 'TBLTYPE', TYPE, DIM, DUMMY, TTYPE, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OGET (INTAB, 'VER', TYPE, DIM, VER, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         WRITE (LINE,1001) NAME, CLASS, SEQ, DISK, TTYPE, VER
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Second table
         CALL OGET (IN2TAB, 'NAME', TYPE, DIM, DUMMY, NAME, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OGET (IN2TAB, 'CLASS', TYPE, DIM, DUMMY, CLASS, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OGET (IN2TAB, 'IMSEQ', TYPE, DIM, SEQ, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OGET (IN2TAB, 'DISK', TYPE, DIM, DISK, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OGET (IN2TAB, 'TBLTYPE', TYPE, DIM, DUMMY, TTYPE, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OGET (IN2TAB, 'VER', TYPE, DIM, VER, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         WRITE (LINE,1002) NAME, CLASS, SEQ, DISK, TTYPE, VER
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       Column labels
C                                       Selection columns
      DO 20 I = 1,NPSEL
         J = SELCOL(I)
         TITL1(SELSTR(I):SELSTR(I)+SELLEN(I)-1) =
     *      TITLE(J)(1:MIN (SELLEN(I),24))
         TITL2(SELSTR(I):SELSTR(I)+SELLEN(I)-1) =
     *      UNITS(J)(1:MIN (SELLEN(I), 8))
 20      CONTINUE
C                                       Comparison
      TITL1(ONESTR:ONESTR+ONELEN-1) = TITLE(COL)(1:MIN (ONELEN,24))
      TITL2(ONESTR:ONESTR+ONELEN-1) =
     *   UNITS(COL)(1:MIN (ONELEN, 8))
      TITL1(TWOSTR:TWOSTR+TWOLEN-1) = 'Second'
      TITL2(TWOSTR:TWOSTR+TWOLEN-1) =
     *   UNITS(COL)(1:MIN (TWOLEN, 8))
      IF (DIFLEN.GT.0) THEN
         TITL1(DIFSTR:DIFSTR+DIFLEN-1) = 'Difference'
         TITL2(DIFSTR:DIFSTR+DIFLEN-1) =
     *      UNITS(COL)(1:MIN (DIFLEN, 8))
         END IF
C                                       Print blank line and col.
C                                       labels.
      IF (DOCRT.GT.-2.5) THEN
         LINE = ' '
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, TITL1,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, TITL2,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      GO TO 999
C                                       Error printing
 900  IF (IERR.GT.0) THEN
         MSGTXT = 'ERROR PRINTING TABLE COMPARISON'
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('First table  : ',A,'.',A,'.',I3,' Disk =', I2,1X,A,
     *   ' Table',I4)
 1002 FORMAT ('Second table : ',A,'.',A,'.',I3,' Disk =', I2,1X,A,
     *   ' Table',I4)
      END
      SUBROUTINE PRTSCL (INTAB, NACROS, COL, NSEL, SCOLS, IERR)
C-----------------------------------------------------------------------
C   Determines the number of characters required for each column.
C   Assumes formats D13.6, E10.3, I10, L8
C   Inputs:
C      INTAB   C*?  Name of input table object
C      NACROS  I    Number of columns avainable in output
C      COL     I    Column number to compare.
C      NSEL    I    Number of selection rows.
C      SCOLS   I(*) Columns numbers of the selection columns
C   Outputs in common:
C      NCOL    I     Number of columns in table
C      TITLE   C(*)*24 Column lables
C      UNITS   C(*)*8  Column Units
C      COLTYP  I(*)  type of column: 1-7 = dp, sp, ch, i, l, i
C                    11,12 = dp and sp time in D/HMS
C      COLDIM  I(*)  Dimensionality of column
C      NPSEL   I     Number of selection columns to print
C      SELCOL  I(*)  Column number of selection cols to be printed.
C      SELSTR  I(*)  Start column of selection columns
C      SELLEN  I(*)  Number of columns in output for selection cols.
C      ONESTR  I     Start column for comparison value in first table.
C      ONELEN  I     Number of columns for comparison value in first.
C      TWOSTR  I     Start column for comparison value in second table.
C      TWOLEN  I     Number of columns for comparison value in second.
C      DIFSTR  I     Start column for difference.
C      DIFLEN  I     Number of columns for difference; 0 => column type
C                    that is not suitable for subtraction.
C      MAXLIN  I     Maximum dimensionality (lines printed per
C                    comparison).
C   Output:
C      IERR    I     Error code
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*)
      INTEGER   COL, NACROS, NSEL, SCOLS(*), IERR
C
      INCLUDE 'TBDIF.INC'
      INTEGER   TYPE, DIM(7), NCH(MAXCOL), LCH(7),  I, J, LENGTH, NCOMP,
     *   NEXT, DUMMY
      CHARACTER CTITLE*(24*MAXCOL), CUNITS*(MAXCOL*8), CDUMMY*1
      EQUIVALENCE (TITLE, CTITLE), (UNITS, CUNITS)
      DATA LCH /15, 12, 0, 12, 10, 10, 0/
C-----------------------------------------------------------------------
C                                       Get table info
      CALL OGET (INTAB, 'NCOL', TYPE, DIM, NCOL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'COLUNIT', TYPE, DIM, DUMMY, CUNITS, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'COLABEL', TYPE, DIM, DUMMY, CTITLE, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'COLTYPE', TYPE, DIM, COLTYP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'COLDIM', TYPE, DIM, COLDIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get types and number of columns
C                                       needed for each column in the
C                                       table.
      DO 20 J = 1,NCOL
C                                       basics
         I = COLTYP(J)
         LENGTH = COLDIM(J)
         NCH(J) = LCH(I)
C                                       is it a time?
         IF ((COLTYP(J).LE.2) .AND. (UNITS(J)(1:4).EQ.'DAYS')) THEN
            UNITS(J) = 'd/h:m:s'
            COLTYP(J) = 10 + COLTYP(J)
            IF (COLTYP(J).EQ.11) NCH(J) = 16
            IF (COLTYP(J).EQ.12) NCH(J) = 14
            END IF
C                                       Get length of character or bit
C                                       arrays.
         IF ((I.EQ.3) .OR. (I.EQ.7)) NCH(J) = LENGTH
C                                       Minimum entry = 10 characters
         NCH(J) = MAX (NCH(J), 10)
 20      CONTINUE
C                                       Determine the maximum number of
C                                       lines needed for print
      IF (COLTYP(COL).EQ.3) THEN
         MAXLIN = 1
      ELSE
         MAXLIN = COLDIM(COL)
         END IF
C                                       Determine number of columns
C                                       needed for comparison.
C                                       Don't need difference column
C                                       except for numeric types.
      IF ((COLTYP(COL).EQ.1) .OR. (COLTYP(COL).EQ.2) .OR.
     *   (COLTYP(COL).EQ.4)) THEN
         NCOMP = 3 * NCH(COL)
      ELSE
         NCOMP = 2 * NCH(COL)
         END IF
C                                       Comparison starts and lengths
      ONESTR = NACROS - NCOMP + 1
      ONELEN = NCH(COL)
      TWOSTR = ONESTR + ONELEN
      TWOLEN = NCH(COL)
      DIFSTR = TWOSTR + TWOLEN
C                                       Only difference numeric types
      IF ((COLTYP(COL).EQ.1) .OR. (COLTYP(COL).EQ.2) .OR.
     *   (COLTYP(COL).EQ.4) .OR. (COLTYP(COL).GT.10)) THEN
         DIFLEN = NCH(COL)
      ELSE
         DIFLEN = 0
         END IF
C                                       Set up for all selection strings
C                                       that will fit.
      NPSEL = 0
C                                       Set start column for selection
C                                       columns.
      NEXT = 7
      DO 50 I = 1,NSEL
         J = SCOLS(I)
         IF ((NEXT+NCH(J)).LT.ONESTR) THEN
            NPSEL = NPSEL + 1
            SELSTR(NPSEL) = NEXT
            SELLEN(NPSEL) = NCH(J)
            SELCOL(NPSEL) = SCOLS(I)
            NEXT = NEXT + SELLEN(NPSEL)
C                                       Only one line for characters
            IF (COLTYP(J).NE.3) MAXLIN = MAX (MAXLIN, COLDIM(J))
            END IF
 50      CONTINUE
C                                       Compress any space in center of
C                                       listing.

      IF (ONESTR.GT.NEXT) THEN
         ONESTR = NEXT
         TWOSTR = ONESTR + ONELEN
         DIFSTR = TWOSTR + TWOLEN
         END IF
C
 999  RETURN
      END
      LOGICAL FUNCTION TBCOMP (INTAB, IN2TAB, ROW1, ROW2, NSEL, SCOLS,
     *   FSTMP, PAST, IERR)
C-----------------------------------------------------------------------
C   Compares rows in two tables to see if they match according to a set
C   of selection criteia.  For a match all data in the selected columns
C   must match according to the following criteria:
C     Double:    abs (difference)/abs (value1) < 1.0D-10
C     Real:      abs (difference)/abs (value1) < 1.0D-5
C     Character: exact
C     Integer:   exact
C     Logical:   exact
C     Other:     no comparison => always match
C   Note: the row number of the first match to the first selection
C   column is also set.  If this has not yet occurred then the input
C   value of FSTMP should be < 0.  If a match is found then the row
C   number in the second table is passed in FSTMP.
C      If FSTMP is true and the first selection column does not match
C   then the current record is past a possible match and PAST is set
C   true, else false.
C   Inputs:
C      INTAB   C*?  Name of first input table object
C      IN2TAB  C*?  Name of second input table object
C      ROW1    I    Row in first table
C      ROW2    I    Row in second table
C      NSEL    I    Number of selection rows.
C      SCOLS   I(*) Columns numbers of the selection columns
C   Output:
C      FSTMP   I    First row in second input table with a match on
C                   the first selection column
C      PAST    L    If true then this record is past the last possible
C                   match.
C      IERR    I    Error code, 0=OK else failed.
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*), IN2TAB*(*)
      INTEGER   ROW1, ROW2, NSEL, FSTMP, IERR
      LOGICAL   PAST
      INTEGER   SCOLS(NSEL)
C
      INCLUDE 'TBDIF.INC'
      INTEGER   I, TYPE, DIM(7), ISEL, LEN, TEST
      LOGICAL   LTEST
      REAL      RCOMP
      DOUBLE PRECISION DCOMP
C-----------------------------------------------------------------------
      TBCOMP = .FALSE.
C                                       Loop over selection criteria
      DO 500 ISEL = 1,NSEL
C                                       Get entries
         CALL TABDGT (INTAB, ROW1, SCOLS(ISEL), TYPE, DIM, IVAL1, CVAL1,
     *      IERR)
C                                       If blanked return
         IF (IERR.LT.0) THEN
            IERR = 0
            GO TO 999
            END IF
         IF (IERR.NE.0) GO TO 999
         CALL TABDGT (IN2TAB, ROW2, SCOLS(ISEL), TYPE, DIM, IVAL2,
     *      CVAL2, IERR)
C                                       If blanked return
         IF (IERR.LT.0) THEN
            IERR = 0
            GO TO 999
            END IF
         IF (IERR.NE.0) GO TO 999
         IF (IERR.NE.0) GO TO 999
C                                       Total number of values
         LEN = DIM(1)
         IF (DIM(2).GT.1) LEN = LEN * DIM(2)
         IF (DIM(3).GT.1) LEN = LEN * DIM(3)
C                                       Compare by type
         IF (TYPE.EQ.1) THEN
C                                       Double precision
            DO 110 I = 1,LEN
               DCOMP = ABS (DVAL1(I))
               IF (DCOMP.LT.1.0D-30) DCOMP = 1.0D0
               LTEST = (ABS (DVAL1(I)-DVAL2(I)) / DCOMP) .GT. 1.0D-10
C                                       Past match?
               IF ((ISEL.EQ.1) .AND. (I.EQ.1)) PAST = (FSTMP.GT.0) .AND.
     *            LTEST
               IF (LTEST) GO TO 999
 110           CONTINUE
         ELSE IF (TYPE.EQ.2) THEN
C                                       Real
            DO 120 I = 1,LEN
               RCOMP = ABS (RVAL1(I))
               IF (RCOMP.LT.1.0E-25) RCOMP = 1.0
               LTEST = (ABS (RVAL1(I)-RVAL2(I)) / RCOMP) .GT. 1.0E-5
C                                       Past match?
               IF ((ISEL.EQ.1) .AND. (I.EQ.1)) PAST = (FSTMP.GT.0) .AND.
     *            LTEST
               IF (LTEST) GO TO 999
 120           CONTINUE
         ELSE IF (TYPE.EQ.3) THEN
C                                       Character
C                                       Found first match of first
C                                       selection column, only test up
C                                       to 8 char.
            TEST = MIN (8, LEN)
            LTEST = (CVAL1(1:TEST).NE.CVAL2(1:TEST))
            IF ((ISEL.EQ.1) .AND. (FSTMP.LE.0) .AND. (.NOT.LTEST))
     *         FSTMP = ROW2
C                                       Past match?
            IF (ISEL.EQ.1) PAST = (FSTMP.GT.0) .AND. LTEST
            IF (CVAL1(1:LEN).NE.CVAL2(1:LEN)) GO TO 999
         ELSE IF (TYPE.EQ.4) THEN
C                                       Integer
            DO 140 I = 1,LEN
               LTEST = IVAL1(I).NE.IVAL2(I)
C                                       Past match?
               IF ((ISEL.EQ.1) .AND. (I.EQ.1)) PAST = (FSTMP.GT.0) .AND.
     *            LTEST
               IF (LTEST) GO TO 999
 140           CONTINUE
         ELSE IF (TYPE.EQ.5) THEN
C                                       Logical
            DO 150 I = 1,LEN
               IF (LVAL1(I).NEQV.LVAL2(I)) GO TO 999
 150           CONTINUE
         ELSE
C                                       Comparisons of other types
C                                       unimplemented.
            END IF
C                                       Found first match of first
C                                       selection column?
         IF ((ISEL.EQ.1) .AND. (FSTMP.LE.0)) FSTMP = ROW2
 500     CONTINUE
C                                       Everything seems to match
      TBCOMP = .TRUE.
C
 999  RETURN
      END
      SUBROUTINE  PRTCOM (INTAB, IN2TAB, ROW1, ROW2, COL, NSEL,
     *   PLUN, PIND, DOALL, DOCRT, NACROS, TITL1, TITL2, LINE, IPCNT,
     *   PAGE, SCRTCH, IERR)
C-----------------------------------------------------------------------
C   Prints a comparison of the contents of a selected row and column in
C   a pair of tables.
C   Also updates comparison summary arrays.
C   Note: uses common variables set by PRTSCL.
C   Inputs:
C      INTAB   C*?  Name of first input table object
C      IN2TAB  C*?  Name of second input table object
C      ROW1    I    Row in first table
C      ROW2    I    Row in second table
C      COL     I    Column number to compare.
C      NSEL    I    Number of selection rows.
C      PLUN    I    LUN for print device (open): 1 => line printer,
C                     3 => text (disk) file, 5 => terminal
C      PIND    I    FTAB pointer for print device
C      DOALL   R    > 0 print everything
C      DOCRT   R    > 0. => use CRT, else line printer or disk
C      NACROS  I    Number characters in line
C      TITL1   C*132   Page title line 1, if ' ' then ignored
C      TITL2   C*132   Page title line 2, if ' ' then ignored
C   In/out:
C      LINE    C*132   Text line
C      IPCNT   I       Number lines so far on page
C                         > 1000 => just ask about continuing (DOCRT
C                                   true).  Do not print anything.
C                         =  999 => print new page title(s) and LINE
C                                   but do not ask the user about
C                                   whether he wants to continue
C      PAGE    I       Current page number
C                         = 0 => start of print job (user not asked on
C                                DOCRT true)
C   Output:
C      SCRTCH  C*(*)   Scratch core > 132
C      IERR    I       Error code: 0 => OK, -1 user asks to quit
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*), IN2TAB*(*), TITL1*132, TITL2*132, SCRTCH*132,
     *   LINE*132
      INTEGER   ROW1, ROW2, COL, NSEL, PLUN, PIND, NACROS, IPCNT, PAGE,
     *   IERR
      REAL      DOALL, DOCRT
C
      INCLUDE 'TBDIF.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER   J, TYPE, DIM(7), ISEL, LEN, IB, IE, IB2, IE2, IBD,
     *   IED
      REAL      RCOMP
      DOUBLE PRECISION DCOMP, DSCALE
      LOGICAL   DIFFER
C-----------------------------------------------------------------------
C                                       Loop over number of lines per
C                                       comparison.
      DO 600 J = 1,MAXLIN
         LINE = ' '
         DIFFER = DOALL.GT.0.0
C                                       Row number
         WRITE (LINE(1:5),1000) ROW1
C                                       Loop over selection criteria
         DO 200 ISEL = 1,NSEL
C                                       Get value
            CALL TABDGT (INTAB, ROW1, SELCOL(ISEL), TYPE, DIM, IVAL1,
     *         CVAL1, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Total number of values
            LEN = DIM(1)
            IF (DIM(2).GT.1) LEN = LEN * DIM(2)
            IF (DIM(3).GT.1) LEN = LEN * DIM(3)
C                                       Is this column done?
            IF (LEN.LT.J) GO TO 200
C                                       Strings printed on top line only
            IF ((TYPE.EQ.3) .AND. (J.GT.1)) GO TO 200
C                                       Which columns?
            IB = SELSTR(ISEL)
            IE = SELSTR(ISEL) + SELLEN(ISEL) - 1
C                                       Print by type
            IF (TYPE.EQ.1) THEN
C                                       Double precision
               IF (DVAL1(J).EQ.DBLANK) THEN
                  LINE(IB:IE) = ' blanked'
               ELSE
C                                       Time?
                  IF (COLTYP(SELCOL(ISEL)).GT.10) THEN
                     CALL DTHMS (DVAL1(J), LINE(IB:IE))
                  ELSE
                     WRITE (LINE(IB:IE), 1100) DVAL1(J)
                     END IF
                  END IF
            ELSE IF (TYPE.EQ.2) THEN
C                                       Real
               IF (RVAL1(J).EQ.FBLANK) THEN
                  LINE(IB:IE) = ' blanked'
               ELSE
C                                       Time?
                  IF (COLTYP(SELCOL(ISEL)).GT.10) THEN
                     CALL RTHMS (RVAL1(J), LINE(IB:IE))
                  ELSE
                     WRITE (LINE(IB:IE), 1200) RVAL1(J)
                     END IF
                  END IF
            ELSE IF (TYPE.EQ.3) THEN
C                                       Character
               LINE(IB:IE) = CVAL1(1:MAX (LEN,SELLEN(ISEL)))
            ELSE IF (TYPE.EQ.4) THEN
C                                       Integer
               WRITE (LINE(IB:IE), 1400) IVAL1(J)
            ELSE IF (TYPE.EQ.5) THEN
C                                       Logical
               WRITE (LINE(IB:IE), 1500) LVAL1(J)
            ELSE
C                                       Printing of other types
C                                       unimplemented.
               END IF
 200        CONTINUE
C                                       Comparison
C                                       Get from first table
         CALL TABDGT (INTAB, ROW1, COL, TYPE, DIM, IVAL1, CVAL1, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Total number of values
         LEN = DIM(1)
         IF (DIM(2).GT.1) LEN = LEN * DIM(2)
         IF (DIM(3).GT.1) LEN = LEN * DIM(3)
C                                       Is this column done?
         IF (LEN.LT.J) GO TO 500
C                                       Strings printed on 1st line only
         IF ((TYPE.EQ.3) .AND. (J.GT.1)) GO TO 500
C                                       Get value from second table
         CALL TABDGT (IN2TAB, ROW2, COL, TYPE, DIM, IVAL2, CVAL2, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Which columns?
         IB = ONESTR
         IE = ONESTR + ONELEN - 1
         IB2 = TWOSTR
         IE2 = TWOSTR + TWOLEN - 1
         IBD = DIFSTR
         IED = DIFSTR + DIFLEN - 1
C                                       Print by type
         IF (TYPE.EQ.1) THEN
C                                       Double precision
C                                       First
            IF (DVAL1(J).EQ.DBLANK) THEN
               LINE(IB:IE) = ' blanked'
            ELSE
               WRITE (LINE(IB:IE), 1100) DVAL1(J)
               END IF
C                                       Second
            IF (DVAL2(J).EQ.DBLANK) THEN
               LINE(IB2:IE2) = ' blanked'
            ELSE
               WRITE (LINE(IB2:IE2), 1100) DVAL2(J)
               END IF
C                                       Difference
            IF ((DVAL1(J).EQ.DBLANK) .OR. (DVAL2(J).EQ.DBLANK)) THEN
               LINE(IBD:IED) = ' blanked'
               IF (DVAL1(J).NE.DVAL2(J)) DIFFER = .TRUE.
            ELSE
               DCOMP = DVAL1(J) - DVAL2(J)
               WRITE (LINE(IBD:IED), 1100) DCOMP
               DSCALE = MAX (ABS(DVAL1(J)), ABS(DVAL2(J)))
               IF (DSCALE.EQ.0.0D0) DSCALE = 1.0D0
               IF (ABS(DCOMP/DSCALE).GT.1.D-6) DIFFER = .TRUE.
C                                       Summary information
               IF (J.LE.MAXSUM) THEN
                  SUMCNT(J) = SUMCNT(J) + 1
                  SUMMAX(J) = MAX (SUMMAX(J), DCOMP)
                  SUMMIN(J) = MIN (SUMMIN(J), DCOMP)
                  SUMSUM(J) = SUMSUM(J) + DCOMP
                  SUMSU2(J) = SUMSU2(J) + DCOMP * DCOMP
                  END IF
               END IF
         ELSE IF (TYPE.EQ.2) THEN
C                                       Real
C                                       First
            IF (RVAL1(J).EQ.FBLANK) THEN
               LINE(IB:IE) = ' blanked'
            ELSE
               WRITE (LINE(IB:IE), 1200) RVAL1(J)
               END IF
C                                       Second
            IF (RVAL2(J).EQ.FBLANK) THEN
               LINE(IB2:IE2) = ' blanked'
            ELSE
               WRITE (LINE(IB2:IE2), 1200) RVAL2(J)
               END IF
C                                       Difference
            IF ((RVAL1(J).EQ.FBLANK) .OR. (RVAL2(J).EQ.FBLANK)) THEN
               LINE(IBD:IED) = ' blanked'
               IF (RVAL1(J).NE.RVAL2(J)) DIFFER = .TRUE.
            ELSE
               RCOMP = RVAL1(J) - RVAL2(J)
               DSCALE = MAX (ABS(RVAL1(J)), ABS(RVAL2(J)))
               IF (DSCALE.EQ.0.0D0) DSCALE = 1.0D0
               IF (ABS(RCOMP/DSCALE).GT.1.D-5) DIFFER = .TRUE.
C                                       If units are 'DEG' then reduce
C                                       to range (-180,180)
               IF (UNITS(COL)(1:3).EQ.'DEG') THEN
                  RCOMP = MOD (RCOMP, 360.0)
                  IF (RCOMP.GT. 180.0) RCOMP = RCOMP - 360.0
                  IF (RCOMP.LT.-180.0) RCOMP = RCOMP + 360.0
                  END IF
               WRITE (LINE(IBD:IED), 1200) RCOMP
C                                       Summary information
               IF (J.LE.MAXSUM) THEN
                  DCOMP = RCOMP
                  SUMCNT(J) = SUMCNT(J) + 1
                  SUMMAX(J) = MAX (SUMMAX(J), DCOMP)
                  SUMMIN(J) = MIN (SUMMIN(J), DCOMP)
                  SUMSUM(J) = SUMSUM(J) + DCOMP
                  SUMSU2(J) = SUMSU2(J) + DCOMP * DCOMP
                  END IF
               END IF
         ELSE IF (TYPE.EQ.3) THEN
C                                       Character
            LINE(IB:IE) = CVAL1(1:MAX (LEN,ONELEN))
            LINE(IB2:IE2) = CVAL2(1:MAX (LEN,TWOLEN))
            IF (LINE(IB:IE).NE.LINE(IB2:IE2)) DIFFER = .TRUE.
         ELSE IF (TYPE.EQ.4) THEN
C                                       Integer
            WRITE (LINE(IB:IE), 1400) IVAL1(J)
            WRITE (LINE(IB2:IE2), 1400) IVAL2(J)
            WRITE (LINE(IBD:IED), 1400) IVAL1(J)-IVAL2(J)
            RCOMP = IVAL1(J) - IVAL2(J)
            DSCALE = MAX (ABS(IVAL1(J)), ABS(IVAL2(J)))
            IF (DSCALE.EQ.0.0D0) DSCALE = 1.0D0
            IF (ABS(RCOMP/DSCALE).GT.1.D-5) DIFFER = .TRUE.
C                                       Summary information
            IF (J.LE.MAXSUM) THEN
               DCOMP = IVAL1(J)-IVAL2(J)
               SUMCNT(J) = SUMCNT(J) + 1
               SUMMAX(J) = MAX (SUMMAX(J), DCOMP)
               SUMMIN(J) = MIN (SUMMIN(J), DCOMP)
               SUMSUM(J) = SUMSUM(J) + DCOMP
               SUMSU2(J) = SUMSU2(J) + DCOMP * DCOMP
               END IF
         ELSE IF (TYPE.EQ.5) THEN
C                                       Logical
            WRITE (LINE(IB:IE), 1500) LVAL1(J)
            WRITE (LINE(IB2:IE2), 1500) LVAL2(J)
            IF (LVAL1(J).NEQV.LVAL2(J)) DIFFER = .TRUE.
         ELSE
C                                       Printing of other types
C                                       unimplemented.
            END IF
C                                       Print it
 500     IF (DIFFER) THEN
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
 600     CONTINUE
      GO TO 999
C                                       Error printing
 900  IF (IERR.GT.0) THEN
         MSGTXT = 'ERROR PRINTING TABLE COMPARISON'
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I5)
 1100 FORMAT (1PD13.6)
 1200 FORMAT (1PE10.3)
 1400 FORMAT (I10)
 1500 FORMAT (L8)
      END
      SUBROUTINE DTHMS (TIME, STRING)
C-----------------------------------------------------------------------
C   Routine to convert double precision time to string giving day, hour,
C   min, sec.
C   Inputs:
C      TIME   D    Time in days.
C   Outputs:
C      STRING C*?  Time as string "ddd/hh:mm:ss.sss".  Note: if STRING
C                  is shorter then it will be truncated from the right.
C-----------------------------------------------------------------------
      DOUBLE PRECISION TIME
      CHARACTER STRING*(*)
C
      INTEGER   DAY, HM(2), SL
      REAL      S
      CHARACTER CHSIGN*1, TSTR*16
      DOUBLE PRECISION T
C-----------------------------------------------------------------------
C                                       Convert time
      DAY = TIME
      T = (TIME - DAY) * 24.0D0
      CALL COORDD (2, T, CHSIGN, HM, S)
C                                       Encode into string
      WRITE (TSTR,1000) DAY, HM, S
      IF (TSTR(11:11).EQ.' ') TSTR(11:11) = '0'
C                                       Copy to output
      SL = MIN (LEN (STRING), 16)
      STRING = TSTR(1:SL)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I3,'/',I2.2,':',I2.2,':',F6.3)
      END
      SUBROUTINE RTHMS (TIME, STRING)
C-----------------------------------------------------------------------
C   Routine to convert single precision time to string giving day, hour,
C   min, sec.
C   Inputs:
C      TIME   R    Time in days.
C   Outputs:
C      STRING C*?  Time as string "ddd/hh:mm:ss.sss".  Note: if STRING
C                  is shorter then it will be truncated from the right.
C-----------------------------------------------------------------------
      REAL      TIME
      CHARACTER STRING*(*)
C
      INTEGER   DAY, HM(2), SL
      REAL      S
      CHARACTER CHSIGN*1, TSTR*16
      DOUBLE PRECISION T
C-----------------------------------------------------------------------
C                                       Convert time
      DAY = TIME
      T = (TIME - DAY) * 24.0D0
      CALL COORDD (2, T, CHSIGN, HM, S)
C                                       Encode into string
      WRITE (TSTR,1000) DAY, HM, S
      IF (TSTR(11:11).EQ.' ') TSTR(11:11) = '0'
C                                       Copy to output
      SL = MIN (LEN (STRING), 16)
      STRING = TSTR(1:SL)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I3,'/',I2.2,':',I2.2,':',F6.3)
      END
      SUBROUTINE SUMINI (COL)
C-----------------------------------------------------------------------
C   Initialize comparison summary.
C   Inputs:
C      COL    I    Column being compared
C   Inputs in common:
C      COLDIM I(*) Dimensionality of columns
C   Outputs in common:
C      NUMSUM I    Number of comparisons in the summary (basically the
C                  dimensionality of the column).
C      SUMCNT I(*) Number of counts in each bin
C      SUMMIN D(*) Minimum difference
C      SUMMAX D(*) Maximum difference
C      SUMSUM D(*) Sum of differences
C      SUMSU2 D(*) Sum of differences**2
C-----------------------------------------------------------------------
      INTEGER   COL
C
      INCLUDE 'TBDIF.INC'
      INTEGER   LOOP
C-----------------------------------------------------------------------
C                                       Dimensionality of comparison
      NUMSUM = MIN (MAXSUM, COLDIM(COL))
C                                       Initialize arrays
      DO 100 LOOP = 1,NUMSUM
         SUMCNT(LOOP) = 0
         SUMMIN(LOOP) = 1.0D20
         SUMMAX(LOOP) = -1.0D20
         SUMSUM(LOOP) = 0.0D0
         SUMSU2(LOOP) = 0.0D0
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE PRTSUM (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *   IPCNT, PAGE, SCRTCH, IERR)
C-----------------------------------------------------------------------
C   Print comparison summary.
C   Inputs:
C      PLUN    I       LUN for print device (open): 1 => line printer,
C                         3 => text (disk) file, 5 => terminal
C      PIND    I       FTAB pointer for print device
C      DOCRT   R       > 0. => use CRT, else line printer or disk
C      NACROS  I       Number characters in line
C   In/out:
C      TITL1   C*132   Page title line 1, if ' ' then ignored
C      TITL2   C*132   Page title line 2, if ' ' then ignored
C      LINE    C*132   Text line
C      IPCNT   I       Number lines so far on page
C                         > 1000 => just ask about continuing (DOCRT
C                                   true).  Do not print anything.
C                         =  999 => print new page title(s) and LINE
C                                   but do not ask the user about
C                                   whether he wants to continue
C      PAGE    I       Current page number
C                         = 0 => start of print job (user not asked on
C                                DOCRT true)
C   Output:
C      SCRTCH  C*(*)   Scratch core > 132
C      IERR    I       Error code: 0 => OK, -1 user asks to quit
C   Inputs in common:
C      DIFLEN  I       If difference comparison not made then don't
C                      print results.
C      NUMSUM I    Number of comparisons in the summary (basically the
C                  dimensionality of the column).
C      SUMCNT I(*) Number of counts in each bin
C      SUMMIN D(*) Minimum difference
C      SUMMAX D(*) Maximum difference
C      SUMSUM D(*) Sum of differences
C      SUMSU2 D(*) Sum of differences**2
C-----------------------------------------------------------------------
      INTEGER   PLUN, PIND, NACROS, IPCNT, PAGE, IERR
      REAL      DOCRT
      CHARACTER TITL1*132, TITL2*132, LINE*132, SCRTCH*132
C
      INCLUDE 'TBDIF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER   LOOP
      DOUBLE PRECISION AVG, SIGMA
C-----------------------------------------------------------------------
C                                       Anything to do?
      IF ((NUMSUM.LE.0) .OR. (DIFLEN.LE.0)) GO TO 999
C                                       Header and labels
      LINE = ' '
      TITL1 = ' '
      TITL2 = ' '
C                                       Blank line
      IF (DOCRT.GT.-2.5) THEN
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Header
         LINE = 'Comparison summary'
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       Column lables
      TITL1 = '    #  count    Average         Sigma         ' //
     *   'Minimum        Maximum'
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, TITL1,
     *   IPCNT, PAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Print comparison summary
      DO 100 LOOP = 1,NUMSUM
         IF (SUMCNT(LOOP).GT.0) THEN
            AVG = SUMSUM(LOOP) / SUMCNT(LOOP)
            IF (SUMCNT(LOOP).GT.1) THEN
               SIGMA = SQRT ((SUMSU2(LOOP) - (SUMSUM(LOOP)*SUMSUM(LOOP)
     *            / SUMCNT(LOOP))) / (SUMCNT(LOOP)-1))
            ELSE
               SIGMA = -1.0D0
               END IF
            WRITE (LINE,1100) LOOP, SUMCNT(LOOP), AVG, SIGMA,
     *         SUMMIN(LOOP), SUMMAX(LOOP)
C                                       Print line
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
 100     CONTINUE
      GO TO 999
C                                       Error printing
 900  IF (IERR.GT.0) THEN
         MSGTXT = 'ERROR PRINTING TABLE COMPARISON'
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (I5, I7, 1PD15.7, 3D15.7)
      END
