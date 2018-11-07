LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER   NPARMS
C                                       NPARMS=no. adverbs passed.
      PARAMETER (NPARMS=10)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
      INCLUDE 'INCS:PAOOF.INC'
C                                       Adverb names
C                     1         2          3        4         5
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INVERS',
C           6           7         8         9         10
     *   'OUTVERS',  'BCOUNT' ,'ECOUNT', 'RADIUS', 'CUTOFF'/
C                                       Adverb data types (PAOOF.INC)
C                    1       2       3       4       5       6
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOAINT,
C          6       7       8       9      10
     *   OOAINT, OOAINT, OOAINT, OOARE, OOARE/
C                                       Adverb dimensions (as 2D)
C                   1    2    3    4    5
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 1,1,
C         6    7    8    9    10
     *   1,1, 1,1, 1,1, 1,1, 1,1/
LOCAL END
      PROGRAM CCSEL
C-----------------------------------------------------------------------
C! Select signifigant CC components.
C# Utility OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2009-2010
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
      CHARACTER PRGM*6, INTAB*32, OUTTAB*32
      INTEGER  IRET, BUFF1(256)
      DATA PRGM /'CCSEL '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL CSELIN (PRGM, INTAB, OUTTAB, IRET)
C                                       Process table
      IF (IRET.EQ.0) CALL CSELAB (INTAB, OUTTAB, IRET)
C                                       History
      IF (IRET.EQ.0) CALL CSELHI (OUTTAB)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE CSELIN (PRGN, INTAB, OUTTAB, IRET)
C-----------------------------------------------------------------------
C   CSELIN gets input parameters for CCSEL and creates the input and
C   output objects
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
      CHARACTER PRGN*6, INTAB*32, OUTTAB*32
C
      INTEGER   NKEY1, NKEY2
C                                       NKEY1=no. adverbs to copy to
C                                       INTAB
      PARAMETER (NKEY1=9)
C                                       NKEY2=no. adverbs to copy to
C                                       OUTTAB
      PARAMETER (NKEY2=5)
      INTEGER   DIM(3), DUMMY
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to INTAB
C                   1         2          3        4         5
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INVERS',
C           6         7         8         9
     *   'BCOUNT', 'ECOUNT', 'RADIUS', 'CUTOFF'/
C                                       May rename adverbs to INTAB
C                    1       2        3        4       5
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'VER',
C           6         7         8         9
     *   'BCOUNT', 'ECOUNT', 'RADIUS', 'CUTOFF'/
C                                       Adverbs to copy to OUTTAB
C                   1         2          3        4         5
      DATA INK2 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'OUTVERS'/
C                                       May rename adverbs to OUTTAB
C                    1       2        3        4       5
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'VER'/
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
C                                       Table type
      DIM(1) = 2
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (INTAB, 'TBLTYPE', OOACAR, DIM, DUMMY, 'CC', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create Output Object
      OUTTAB = 'Output table'
      CALL CREATE (OUTTAB, 'TABLE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, OUTTAB, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Table type
      CALL OPUT (OUTTAB, 'TBLTYPE', OOACAR, DIM, DUMMY, 'CC', IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE CSELAB (INTAB, OUTTAB, IERR)
C-----------------------------------------------------------------------
C   Sum and select components.
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
      INTEGER   MAXCC
C                                       MAXCC = max. no. CC entries.
      PARAMETER (MAXCC = 100000)
      INTEGER   IROW, OROW, NROW, BC, EC, TYPE, DIM(3), NCC, I, J
      REAL      XPOS(MAXCC), YPOS(MAXCC), FLUX(MAXCC), SFLUX(MAXCC),
     *   RADIUS, CUTOFF, MXDIS, SUM, DIS
      LOGICAL   DOMAGN
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Create output table
C                                       This copies header stuff
C                                       including any keywords.
      CALL COPHED (INTAB, OUTTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open input table
      CALL OOPEN (INTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get number of entries
      CALL OGET (INTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get range of rows.
      CALL OGET (INTAB, 'BCOUNT', TYPE, DIM, BC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      BC = MIN (MAX (BC, 1), NROW)
      CALL OGET (INTAB, 'ECOUNT', TYPE, DIM, EC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (EC.LE.0) EC = NROW
C                                       Check array sizes
      IF ((EC-BC+1) .GT. MAXCC) THEN
         WRITE (MSGTXT,1000) EC-BC+1, MAXCC
         CALL MSGWRT (8)
         IERR = 5
         GO TO 999
         END IF
C                                       Filtering parameters
      CALL OGET (INTAB, 'RADIUS', TYPE, DIM, RADIUS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'CUTOFF', TYPE, DIM, CUTOFF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Filter by magnitude?
      DOMAGN = CUTOFF.LT.0.0
      CUTOFF = ABS (CUTOFF)
C                                       Read table to internal arrays.
      NCC = 0
      DO 100 IROW = BC,EC
         NCC = NCC + 1
         CALL CCTGET (INTAB, IROW, XPOS(NCC), YPOS(NCC), FLUX(NCC),
     *      IERR)
         IF (IERR.GT.0) GO TO 999
C                                       Flagged?
         IF (IERR.LT.0) NCC = NCC - 1
 100     CONTINUE
C                                       Close Input
      CALL OCLOSE (INTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Sum fluxes within RADIUS
      MXDIS = (RADIUS / 3600.0) ** 2
      DO 400 I = 1,NCC
         SUM = 0.0
         DO 300 J = 1,NCC
            DIS = (XPOS(I)-XPOS(J)) ** 2 + (YPOS(I)-YPOS(J)) ** 2
            IF (DIS.LE.MXDIS) SUM = SUM + FLUX(J)
 300        CONTINUE
         IF (DOMAGN) SUM = ABS (SUM)
         SFLUX(I) = SUM
 400     CONTINUE
C                                       Open output table
      CALL OOPEN (OUTTAB, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Write selected components
      OROW = 0
      DO 500 IROW = 1,NCC
         IF (SFLUX(IROW).GT.CUTOFF) THEN
            OROW = OROW + 1
            CALL CCTPUT (OUTTAB, OROW, XPOS(IROW), YPOS(IROW),
     *         FLUX(IROW), IERR)
            IF (IERR.GT.0) GO TO 999
            END IF
 500     CONTINUE
C                                       Update number of rows
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (OUTTAB, 'NROW', OOAINT, DIM, OROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999

C                                       Close output
      CALL OCLOSE (OUTTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TOO MANY COMPONENTS FOR INTERNAL ARRAYS:',I8,'>',I8)
      END
      SUBROUTINE CSELHI (OUTTAB)
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
      PARAMETER (NADV=9)
      CHARACTER LIST(NADV)*8
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INVERS',
     *   'OUTVERS',  'BCOUNT' ,'ECOUNT', 'RADIUS', 'CUTOFF'/
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
      SUBROUTINE CCTGET (NAME, ROW, X, Y, FLUX, IERR)
C-----------------------------------------------------------------------
C   Get row from CC (CLEAN component) table object.
C   This assumes the structure of the CC table
C   Inputs:
C      NAME    C*?  CC table object name.
C      ROW     I    Row number
C   Output:
C      X       R    X coordinate
C      Y       R    Y coordinate
C      FLUX    R    Component flux density.
C      IERR    I    Return error code, 0=>OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   ROW, IERR
      REAL      X, Y, FLUX
C
      INTEGER   DIM(7), TYPE
      CHARACTER CDUMMY*1
C-----------------------------------------------------------------------
C                                       Read
      CALL TABDGT (NAME, ROW, 1, TYPE, DIM, FLUX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABDGT (NAME, ROW, 2, TYPE, DIM, X, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABDGT (NAME, ROW, 3, TYPE, DIM, Y, CDUMMY, IERR)
C
 999  RETURN
      END
      SUBROUTINE CCTPUT (NAME, ROW, X, Y, FLUX, IERR)
C-----------------------------------------------------------------------
C   Write row to CC (CLEAN component) table object.
C   This assumes the structure of the CC table
C   Inputs:
C      NAME    C*?  CC table object name.
C      ROW     I    Row number
C      X       R    X coordinate
C      Y       R    Y coordinate
C   Output:
C      IERR    I    Return error code, 0=>OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   ROW, IERR
      REAL      X, Y, FLUX
C
      INTEGER   DIM(7)
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
C                                       Write
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABDPT (NAME, ROW, 1, OOARE, DIM, FLUX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABDPT (NAME, ROW, 2, OOARE, DIM, X, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABDPT (NAME, ROW, 3, OOARE, DIM, Y, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
