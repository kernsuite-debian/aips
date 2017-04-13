LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER   NPARMS
C                                       NPARMS=no. adverbs passed.
      PARAMETER (NPARMS=12)
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
C           6         7         8          9         10        11
     *   'INVERS', 'OUTEXT', 'OUTVERS', 'BCOUNT', 'ECOUNT', 'COLNAME',
C           12
     *   'OUTCOL'/
C                                       Adverb data types (PAOOF.INC)
C                    1       2       3       4       5
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOACAR,
C          6       7       8       9       10      11
     *   OOAINT, OOACAR, OOAINT, OOAINT, OOAINT, OOACAR,
c          12
     *   OOACAR/
C                                       Adverb dimensions (as 2D)
C                   1    2    3    4    5
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 2,1,
C         6    7    8    9    10    11
     *   1,1, 2,1, 1,1, 1,1, 1,1, 16,30,
C          12
     *   16,30/
LOCAL END
      PROGRAM TBSUB
C-----------------------------------------------------------------------
C! Select a subset of a table and write a new table.
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2009
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
C-----------------------------------------------------------------------
C   Copy a subset of one table to a new table
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, INTAB*36, OUTTAB*36
      INTEGER  IRET, BUFF1(256)
      DATA PRGM /'TBSUB '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL SUBTIN (PRGM, INTAB, OUTTAB, IRET)
C                                       Process table
      IF (IRET.EQ.0) CALL SUBTAB (INTAB, OUTTAB, IRET)
C                                       History
      IF (IRET.EQ.0) CALL SUBTHI (OUTTAB)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE SUBTIN (PRGN, INTAB, OUTTAB, IRET)
C-----------------------------------------------------------------------
C   SUBTIN gets input parameters for TBSUB and creates the input and
C   output objects
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IRET    I    Error code: 0 => ok
C                               4 => user routine detected error.
C                               5 => catalog troubles
C                               8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, INTAB*(*), OUTTAB*(*)
C
      INTEGER   NKEY1, NKEY2
C                                       NKEY1=no. adverbs to copy to
C                                       INTAB
      PARAMETER (NKEY1=9)
C                                       NKEY2=no. adverbs to copy to
C                                       OUTTAB
      PARAMETER (NKEY2=7)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to INTAB
C                   1         2          3        4         5
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INEXT',
C           6         7                   9
     *   'INVERS', 'BCOUNT', 'ECOUNT', 'COLNAME'/
C                                       May rename adverbs to INTAB
C                    1       2        3        4       5
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'TBLTYPE',
C           6      7         8         9
     *   'VER', 'BCOUNT', 'ECOUNT', 'COLNAME'/
C                                       Adverbs to copy to OUTTAB
C                   1         2          3        4         5
      DATA INK2 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'OUTEXT',
C           6         7
     *   'OUTVERS', 'OUTCOL'/
C                                       May rename adverbs to OUTTAB
C                    1       2        3        4       5
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'TBLTYPE',
C           6      7
     *   'VER', 'OUTCOL'/
C-----------------------------------------------------------------------
C                                       Startup,  returns "Input" object
C                                       containing POPS adverbs
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create input object
      INTAB = 'Input table'
      CALL CREATE (INTAB, 'TABLE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, INTAB, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create Output Object
      OUTTAB = 'Output table'
      CALL CREATE (OUTTAB, 'TABLE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, OUTTAB, IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE SUBTAB (INTAB, OUTTAB, IERR)
C-----------------------------------------------------------------------
C   Copy table.
C   Inputs:
C      INTAB   C*   Name of input table object.
C      OUTTAB  C*   Name of output table object.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*), OUTTAB*(*)
      INTEGER   IERR
C
      INTEGER   MAXSIZ
C                                       MAXSIZ = max table entry size as
C                                       reals or characters.
      PARAMETER (MAXSIZ = 5000)
      INTEGER   ROW, NCOMP, TYPE, DIM(7), NCOL, ICOLS(256), OCOLS(256),
     *   BC, EC, I, OROW
      REAL      NVALS(MAXSIZ)
      CHARACTER CVALS*(MAXSIZ), CDUMMY*1
C-----------------------------------------------------------------------
C                                       Open input table
      CALL OOPEN (INTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Deal with headers
      CALL THEAD (INTAB, OUTTAB, NCOL, ICOLS, OCOLS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Open output table
      CALL OOPEN (OUTTAB, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get number of entries
      CALL OGET (INTAB, 'NROW', TYPE, DIM, NCOMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'BCOUNT', TYPE, DIM, BC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      BC = MIN (MAX (BC, 1), NCOMP)
      CALL OGET (INTAB, 'ECOUNT', TYPE, DIM, EC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (EC.LE.0) EC = NCOMP
C                                       Convert tables
      OROW = 0
      DO 100 ROW = BC,EC
         OROW = OROW + 1
         DO 50 I = 1,NCOL
            CALL TABDGT (INTAB, ROW, ICOLS(I), TYPE, DIM, NVALS, CVALS,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TABDPT (OUTTAB, OROW, OCOLS(I), TYPE, DIM, NVALS,
     *         CVALS, IERR)
            IF (IERR.NE.0) GO TO 999
 50         CONTINUE
 100     CONTINUE
C                                       Close tables
      CALL OCLOSE (INTAB, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OCLOSE (OUTTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE SUBTHI (OUTTAB)
C-----------------------------------------------------------------------
C   Routine to write history file to output table object.
C   Inputs:
C      OUTTAB  C*?  Output table object
C-----------------------------------------------------------------------
      CHARACTER OUTTAB*(*)
C
      INTEGER   NADV
      PARAMETER (NADV=11)
      CHARACTER LIST(NADV)*8
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INEXT', 'INVERS',
     *   'BCOUNT', 'ECOUNT', 'COLNAME', 'OUTEXT', 'OUTVERS', 'OUTCOL'/
C-----------------------------------------------------------------------
C                                       Add task label to history
      CALL OHTIME (OUTTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy adverb values.
      CALL OHLIST ('Input', LIST, NADV, OUTTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // OUTTAB
      CALL MSGWRT (4)
 999  RETURN
      END
      SUBROUTINE THEAD (INTAB, OUTTAB, NCOL, ICOLS, OCOLS, IERR)
C-----------------------------------------------------------------------
C   Set header stuff
C   Inputs:
C      INTAB   C*?  Name of input table object
C      OUTTAB  C*?  Name of output table object
C   Output:
C      NCOL    I    The number of columns to copy
C      ICOLS   I    List of column numbers in input table
C      OCOLS   I    List of column numbers in output table
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*), OUTTAB*(*)
      INTEGER   NCOL, ICOLS(*), OCOLS(*), IERR
C
C                                        MAXCOL = max number cols.
      INTEGER   MAXCOL
      PARAMETER (MAXCOL=128)
      CHARACTER COL*8, COLLAB(MAXCOL)*16, CLAB*(16*MAXCOL), TTITLE*56,
     *   OUTLAB(MAXCOL)*16, OLAB*(16*MAXCOL),
     *   TITLE(MAXCOL)*24, CTITLE*(24*MAXCOL),
     *   UNITS(MAXCOL)*8, CUNITS*(MAXCOL*8),
     *   OUTT(MAXCOL)*24, COUTT*(MAXCOL*24),
     *   OUTU(MAXCOL)*8, COUTU*(MAXCOL*8), CDUMMY*1
      INTEGER   TYPE, DIM(7), COLTYP(MAXCOL), COLDIM(MAXCOL), I, J,
     *   OUTTYP(MAXCOL), OUTDIM(MAXCOL), NC, IEND, JEND, NICOL, DUMMY
      LOGICAL   BADCOL
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C                                       Need to equivalence character
C                                       arrays for OGET/OPUT
      EQUIVALENCE (COLLAB, CLAB), (TITLE, CTITLE), (UNITS, CUNITS)
      EQUIVALENCE (OUTT, COUTT), (OUTU, COUTU), (OUTLAB, OLAB)
C-----------------------------------------------------------------------
      NCOL = 0
      BADCOL = .FALSE.
C                                        Fetch Input table info
      CALL OGET (INTAB, 'COLNAME', TYPE, DIM, DUMMY, CLAB, IERR)
      IF (IERR.NE.0) GO TO 999
      NICOL = DIM(2)
      CALL OGET (INTAB, 'NCOL', TYPE, DIM, NC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'LABEL', TYPE, DIM, DUMMY, TTITLE, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'COLABEL', TYPE, DIM, DUMMY, CTITLE, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'COLUNIT', TYPE, DIM, DUMMY, CUNITS, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'COLTYPE', TYPE, DIM, COLTYP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'COLDIM', TYPE, DIM, COLDIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Fetch output col. labels
      CALL OGET (OUTTAB, 'OUTCOL', TYPE, DIM, DUMMY, OLAB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Look up columns in input
      DO 500 I = 1,NICOL
C                                        Blank entry?
         IF (COLLAB(I).NE.' ') THEN
C                                        Given column number?
            IF (COLLAB(I)(1:5) .EQ. 'COL#(') THEN
C                                       Parse column number.
               JEND = INDEX (COLLAB(I), ')') - 1
C                                        Found closing )?
               IF (JEND.LE.0) GO TO 310
               IEND = MAX (JEND, 6)
               COL = ' '
               COL(14-IEND:8) = COLLAB(I)(6:IEND)
               READ (COL,1000,ERR=310) J
C                                       Valid column number?
               IF ((J.LE.0) .OR. (J.GT.NC)) THEN
C                                        Invalid column number
                  MSGTXT = 'INVALID COLUMN NUMBER: ' // COLLAB(I)
                  CALL MSGWRT (6)
                  BADCOL = .TRUE.
                  GO TO 500
                  END IF
C                                       Looks OK - process
               GO TO 320
            ELSE
C                                       Check list of column names
               DO 300 J = 1,NC
                  IF (COLLAB(I).EQ.TITLE(J)(1:16)) GO TO 320
 300             CONTINUE
               END IF
C                                        If it got here the column
C                                        wasn't found.
            MSGTXT = 'COULD NOT FIND COLUMN: '// COLLAB(I)
            CALL MSGWRT (6)
            MSGTXT = 'CHECK SPELLING, IGNORING REQUEST'
            CALL MSGWRT (6)
            BADCOL = .TRUE.
            GO TO 500
C                                        Decode error for col.
 310        MSGTXT = 'ERROR DECODING COLUMN NUMBER: ' // COLLAB(I)
            CALL MSGWRT (6)
            BADCOL = .TRUE.
            GO TO 500
C                                        OK: Save column values
 320        NCOL = NCOL + 1
            ICOLS(NCOL) = J
            OCOLS(NCOL) = NCOL
            OUTT(NCOL) = TITLE(J)
C                                        Change label?
            IF (OUTLAB(I).NE.' ') OUTT(NCOL) = OUTLAB(I)
            OUTU(NCOL) = UNITS(J)
            OUTTYP(NCOL) = COLTYP(J)
            OUTDIM(NCOL) = COLDIM(J)
            END IF
 500     CONTINUE
C                                        Better have some columns
      IF (NCOL.LE.0) THEN
         MSGTXT = 'ERROR: NO COLUMNS SELECTED'
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
C                                        Set values for new table
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL  OPUT (OUTTAB, 'NCOL', OOAINT, DIM, NCOL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 56
      CALL OPUT (OUTTAB, 'LABEL', OOACAR, DIM, DUMMY, TTITLE, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 24
      DIM(2) = NCOL
      CALL OPUT (OUTTAB, 'COLABEL', OOACAR, DIM, DUMMY, COUTT, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 8
      CALL OPUT (OUTTAB, 'COLUNIT', OOACAR, DIM, DUMMY, COUTU, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = NCOL
      DIM(2) = 1
      CALL OPUT (OUTTAB, 'COLTYPE', OOAINT, DIM, OUTTYP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (OUTTAB, 'COLDIM', OOAINT, DIM, OUTDIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I8)
      END
