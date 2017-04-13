LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER   NPARMS
C                                       NPARMS=no. adverbs passed.
      PARAMETER (NPARMS=9)
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
C           6         7           8         9
     *   'INVERS', 'OUTVERS',  'BCOUNT' ,'ECOUNT'/
C                                       Adverb data types (PAOOF.INC)
C                    1       2       3       4       5
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOACAR,
C          6       7       8       9
     *   OOAINT, OOAINT, OOAINT, OOAINT/
C                                       Adverb dimensions (as 2D)
C                   1    2    3    4    5
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 2,1,
C         6    7    8    9
     *   1,1, 1,1, 1,1, 1,1/
LOCAL END
      PROGRAM TBTSK
C-----------------------------------------------------------------------
C! Paraform AIPS OOP task processing a table.
C# Calibration VLBI
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
C   Paraform AIPS OOP task processing a table.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, INTAB*36, OUTTAB*36
      INTEGER  IRET, BUFF1(256)
      DATA PRGM /'TBTSK '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL TABTIN (PRGM, INTAB, OUTTAB, IRET)
C                                       Process table
      IF (IRET.EQ.0) CALL TABTAB (INTAB, OUTTAB, IRET)
C                                       History
      IF (IRET.EQ.0) CALL TABTHI (OUTTAB)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE TABTIN (PRGN, INTAB, OUTTAB, IRET)
C-----------------------------------------------------------------------
C   TABTIN gets input parameters for TBTSK and creates the input and
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
      CHARACTER PRGN*6, INTAB*36, OUTTAB*36
C
      INTEGER   NKEY1, NKEY2
C                                       NKEY1=no. adverbs to copy to
C                                       INTAB
      PARAMETER (NKEY1=8)
C                                       NKEY2=no. adverbs to copy to
C                                       OUTTAB
      PARAMETER (NKEY2=6)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to INTAB
C                   1         2          3        4         5
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INEXT',
C           6         7         8
     *   'INVERS', 'BCOUNT', 'ECOUNT'/
C                                       May rename adverbs to INTAB
C                    1       2        3        4       5
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'TBLTYPE',
C           6      7         8
     *   'VER', 'BCOUNT', 'ECOUNT'/
C                                       Adverbs to copy to OUTTAB
C                   1         2          3        4         5
      DATA INK2 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INEXT',
C           6
     *   'OUTVERS'/
C                                       May rename adverbs to OUTTAB
C                    1       2        3        4       5
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'TBLTYPE',
C           6
     *   'VER'/
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
      SUBROUTINE TABTAB (INTAB, OUTTAB, IERR)
C-----------------------------------------------------------------------
C   Convert table.
C   This example simply copys one table to another
C   Inputs:
C      INTAB   C*   Name of input table object.
C      OUTTAB  C*   Name of output table object.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*), OUTTAB*(*)
      INTEGER   IERR
C
C
      INTEGER   MAXSIZ
C                                       MAXSIZ = max table entry size as
C                                       reals or characters
      PARAMETER (MAXSIZ = 5000)
      INTEGER   IROW, OROW, NROW, ICOL, NCOL, BC, EC, TYPE, DIM(3)
      REAL      NVALS(MAXSIZ)
      CHARACTER CVALS*(MAXSIZ), CDUMMY*1
C-----------------------------------------------------------------------
C                                       Example: copy a subset of one
C                                       table to another.
C
C                                       Create output table
C                                       This copies header stuff
C                                       including any keywords.
      CALL COPHED (INTAB, OUTTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open input table
      CALL OOPEN (INTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open output table
      CALL OOPEN (OUTTAB, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get number of entries
      CALL OGET (INTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Number of columns
      CALL OGET (INTAB, 'NCOL', TYPE, DIM, NCOL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get range of rows.
      CALL OGET (INTAB, 'BCOUNT', TYPE, DIM, BC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      BC = MIN (MAX (BC, 1), NROW)
      CALL OGET (INTAB, 'ECOUNT', TYPE, DIM, EC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (EC.LE.0) EC = NROW
C                                       Copy selected rows.
      OROW = 0
      DO 100 IROW = BC,EC
         OROW = OROW + 1
         DO 50 ICOL = 1,NCOL
            CALL TABDGT (INTAB, IROW, ICOL, TYPE, DIM, NVALS, CVALS,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            CALL TABDPT (OUTTAB, OROW, ICOL, TYPE, DIM, NVALS, CVALS,
     *         IERR)
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
      SUBROUTINE TABTHI (OUTTAB)
C-----------------------------------------------------------------------
C   Routine to write history file to output table object.  This assumes
C   that a previous history exists and merely adds the information from
C   the current task.
C   Inputs:
C      OUTTAB  C*?  Output table object
C-----------------------------------------------------------------------
      CHARACTER OUTTAB*(*)
C
      INTEGER   NADV
      PARAMETER (NADV=8)
      CHARACTER LIST(NADV)*8
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INEXT', 'INVERS',
     *   'OUTVERS',  'BCOUNT' ,'ECOUNT'/
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
C
 999  RETURN
      END
