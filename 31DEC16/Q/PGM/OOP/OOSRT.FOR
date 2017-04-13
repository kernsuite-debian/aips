LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLN.INC'
      INTEGER   NPARMS
      PARAMETER (NPARMS=11)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                      1        2         3          4
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
C           5          6           7          8         9       10
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK', 'SORT', 'ROTATE',
C           11
     *   'BADDISK'/
C                    1       2       3       4
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT,
C          5       6       7       8        9       10     11
     *   OOACAR, OOACAR, OOAINT, OOAINT,  OOACAR, OOARE, OOAINT/
C                   1     2     3     4
      DATA AVDIM /12,1,  6,1,  1,1,  1,1,
C          5     6     7     8     9     10     11
     *   12,1,  6,1,  1,1,  1,1,  2,1,  1,1,  10,1/
LOCAL END
      PROGRAM OOSRT
C-----------------------------------------------------------------------
C! Sort a UV data set with rotation
C# Task AP UV OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 2000, 2007, 2015
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
      CHARACTER PRGM*6, UVSORT*32, UVDATA*32
      INTEGER  IRET, BUFF1(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'OOSRT'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL OSRTIN (PRGM, UVDATA, UVSORT, IRET)
C                                       Sort them
      IF (IRET.EQ.0) CALL OUVSRT (UVDATA, UVSORT, IRET)
C                                       History
      IF (IRET.EQ.0) CALL OSRTHI (UVDATA, UVSORT)
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE OSRTIN (PRGN, UVDATA, UVSORT, IRET)
C-----------------------------------------------------------------------
C   OSRTIN gets input parameters for OOSRT and creates the UVSORT
C   object.
C   Inputs:
C      PRGN     C*6    Program name
C   Output:
C      UVDATA   C*32   Name of input uv data.
C      UVSORT   C*32   Nme of output sorted UV object
C      IRET     I      Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, UVSORT*(*), UVDATA*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NKEY1, NKEY2
C                                       NKEY1=no. adverbs to copy to
C                                       UVDATA object
      PARAMETER (NKEY1=10)
C                                       NKEY2 = no. adverb for UVSORT
      PARAMETER (NKEY2=4)
      INCLUDE 'INPUT.INC'
      INTEGER   DIM(7), TYPE, DUMMY
      LOGICAL   DOCOMP
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, SORT*2, CDUMMY*1, TINAME*12, TONAME*12
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs for UVDATA object
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'ROTATE',
     *   'SORT', 'OUTNAME', 'OUTCLASS', 'OUTDISK', 'OUTSEQ'/
C                                       Rename
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'ROTATE', 'SORT',
     *   'OUTNAME', 'OUTCLASS', 'OUTDISK', 'OUTSEQ'/
C                                       Adverbs for UVSORT object
C                    1          2          3         4
      DATA INK2 /'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK'/
C                                       Rename
C                    1       2       3         4
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK'/
C-----------------------------------------------------------------------
C                                       Startup - as interactive
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) THEN
         RQUICK = .FALSE.
         GO TO 999
         END IF
C                                       Default sort = 'XY'
      CALL OGET ('Input', 'SORT', TYPE, DIM, DUMMY, SORT, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (SORT.EQ.' ') THEN
         SORT = 'XY'
         CALL OPUT ('Input', 'SORT', TYPE, DIM, DUMMY, SORT, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       BADDISK
      CALL OGET ('Input', 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Default output Name = input
      CALL OGET ('Input', 'INNAME', TYPE, DIM, DUMMY, TINAME, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'OUTNAME', TYPE, DIM, DUMMY, TONAME, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (TONAME.EQ.' ') THEN
         TONAME = TINAME
         CALL OPUT ('Input', 'OUTNAME', TYPE, DIM, DUMMY, TONAME, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Create UVDATA object
      UVDATA = 'Input UVdata to be sorted'
      CALL CREATE (UVDATA, 'UVDATA', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open UVDATA to be sure it's OK.
      CALL OOPEN (UVDATA, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDGET (UVDATA, 'ISCOMP', TYPE, DIM, DOCOMP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OUVPUT (UVDATA, 'DOUVCOMP', TYPE, DIM, DOCOMP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create output objects
      UVSORT = 'Output sorted UVdata'
      CALL CREATE (UVSORT, 'UVDATA', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, UVSORT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Make output object
      CALL OUVCLN (UVDATA, UVSORT, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OUVOPN (UVSORT, 'WRIT', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy UVDESC
      CALL UVDSCP (UVDATA, UVSORT, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (UVSORT, IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE OSRTHI (UVDATA, UVSORT)
C-----------------------------------------------------------------------
C   Routine to write history file to output UVSORT image object.
C   Inputs:
C      UVDATA   C*?  Input UV data object name
C      UVSORT   C*?  Sorted UV data object name
C-----------------------------------------------------------------------
      CHARACTER UVSORT*(*), UVDATA*32
C
      INTEGER   NADV
      PARAMETER (NADV=8)
      CHARACTER LIST(NADV)*8, NOTYPE(3)*2
      INTEGER   IERR, DIM(7), DUMMY
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'OUTNAME', 'OUTCLASS',
     *   'OUTSEQ', 'SORT', 'ROTATE'/
      DATA NOTYPE /'AN', 'FQ', 'SU'/
C-----------------------------------------------------------------------
C                                       Copy old history
      CALL OHCOPY (UVDATA, UVSORT, IERR)
C                                       Copy base adverb values.
      IF (IERR.EQ.0) CALL OHLIST ('Input', LIST, NADV, UVSORT, IERR)
C                                       Error
      IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR WRITING HISTORY FOR ' // UVSORT
         CALL MSGWRT (6)
         END IF
C                                       copy all other tables
      DIM(1) = 2
      DIM(2) = 3
      DIM(3) = 1
      DIM(4) = 0
      CALL OPUT (UVDATA, 'DROPTABS', OOACAR, DIM, DUMMY, NOTYPE, IERR)
      CALL UVCALT (UVDATA, UVSORT, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'PROBLEM COPYING TABLES TO OUTPUT'
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
      END
