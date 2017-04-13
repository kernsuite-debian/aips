LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER   NPARMS
      PARAMETER (NPARMS=10)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                                       NOTE: Uses values in PAOOF.INC
C                                       Adverb names
C                     1         2          3        4         5
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INVER' ,
C           6          7       8       9      10
     *   'OUTVER ', 'BMAJ', 'BMIN', 'BPA', 'FACTOR'/
C                                       Adverb data types (PAOOF.INC)
C                    1       2       3       4       5
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOAINT,
C          6       7      8      9      10
     *   OOAINT, OOARE, OOARE, OOARE, OOARE/
C                   1    2    3    4    5
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 1,1,
C         6    7    8    9    10
     *   1,1, 1,1, 1,1, 1,1, 1,1/
LOCAL END
      PROGRAM CCGAU
C-----------------------------------------------------------------------
C! Convert point components in CC file to Gaussians
C# Ext-appl Object-Oriented
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
C   Convert point components in CC file to Gaussians
C   Inputs:
C                                   File
C   INNAME                                File name
C   INCLASS                               File class
C   INSEQ              0.0      9999.0    File name (seq. #)
C   INDISK             0.0         9.0    File disk drive #
C   INVER                                 Input CC version
C   OUTVER                                Output CC version
C   BMAJ                                  Gaussian major axis
C   BMIN                                  Gaussian minor axis
C   BPA                                   Gaussian position angle
C   FACTOR                                Factor for flux.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, INTAB*36, OUTTAB*36
      INTEGER  IRET, BUFF1(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'CCGAU '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL CCGIN (PRGM, INTAB, OUTTAB, IRET)
C                                       Convert components
      IF (IRET.EQ.0) CALL CCGAUS (INTAB, OUTTAB, IRET)
C                                       History
      IF (IRET.EQ.0) CALL CCGHIS (OUTTAB)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE CCGIN (PRGN, INTAB, OUTTAB, IRET)
C-----------------------------------------------------------------------
C   CCGIN gets input parameters for CCGAU and creates the input and
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
      CHARACTER PRGN*6, INTAB*36, OUTTAB*36
C
      INTEGER   NKEY1, NKEY2
C                                       NKEY1=no. adverbs to copy to
C                                       INTAB
      PARAMETER (NKEY1=9)
C                                       NKEY2=no. adverbs to copy to
C                                       OUTTAB
      PARAMETER (NKEY2=5)
      INTEGER   IERR, DIM(7), DUMMY
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to INTAB
C                   1         2          3        4         5
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INVER',
C           6       7       8      9
     *   'BMAJ', 'BMIN', 'BPA', 'FACTOR'/
C                    1       2        3        4       5
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'VER',
C           6       7       8      9
     *   'BMAJ', 'BMIN', 'BPA', 'FACTOR'/
C                                       Adverbs to copy to OUTTAB
C                   1         2          3        4         5
      DATA INK2 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'OUTVER'/
C                    1       2        3        4       5
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'VER'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create input object
      INTAB = 'Input CC table'
      CALL CREATE (INTAB, 'TABLE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, INTAB, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Table type
      DIM(1) = 2
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (INTAB, 'TBLTYPE', OOACAR, DIM, DUMMY, 'CC', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Create Output Object
      OUTTAB = 'Output CC table'
      CALL CREATE (OUTTAB, 'TABLE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, OUTTAB, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Table type
      CALL OPUT (OUTTAB, 'TBLTYPE', OOACAR, DIM, DUMMY, 'CC', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Init. table descriptive info
      CALL CCTSET (OUTTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE CCGAUS (INTAB, OUTTAB, IERR)
C-----------------------------------------------------------------------
C   Convert table
C   Inputs:
C      INTAB   C*   Name of input table object.
C   Output:
C      OUTTAB  C*   Name of output table object.
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*), OUTTAB*(*)
      INTEGER   IERR
C
      INTEGER   LOOP, NCOMP, TYPE, DIM(7)
      REAL      BMAJ, BMIN, BPA, FACTOR, X, Y, FLUX
      CHARACTER CDUMMY*1
C-----------------------------------------------------------------------
C                                       Get Gaussian parameters from
C                                       input object
      CALL CGINFO (INTAB, BMAJ, BMIN, BPA, FACTOR, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Convert to degrees
      BMAJ = BMAJ / 3600.0
      BMIN = BMIN / 3600.0
C                                       Open tables
      CALL OOPEN (INTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OOPEN (OUTTAB, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get number of components
      NCOMP = 1
      CALL OGET (INTAB, 'NROW', TYPE, DIM, NCOMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Convert tables
      DO 100 LOOP = 1,NCOMP
         CALL CCTGET (INTAB, LOOP, X, Y, FLUX, IERR)
         IF (IERR.NE.0) GO TO 999
C
         FLUX = FLUX * FACTOR
         CALL CCTPUT (OUTTAB, LOOP, X, Y, FLUX, BMAJ, BMIN, BPA, IERR)
         IF (IERR.NE.0) GO TO 999
 100     CONTINUE
C                                       Close tables
      CALL OCLOSE (INTAB, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OCLOSE (OUTTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE CCGHIS (OUTTAB)
C-----------------------------------------------------------------------
C   Routine to write history file to output table object.
C   Inputs:
C      OUTTAB  C*?  Output table object
C-----------------------------------------------------------------------
      CHARACTER OUTTAB*(*)
C
      INTEGER   NADV
      PARAMETER (NADV=6)
      CHARACTER LIST(NADV)*8
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INVER', 'BMAJ', 'BMIN', 'BPA', 'FACTOR', 'OUTVER'/
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
      SUBROUTINE CGINFO (INTAB, BMAJ, BMIN, BPA, FACTOR, IERR)
C-----------------------------------------------------------------------
C   Gets Gaussian information
C   Inputs:
C      INTAB   C*?  Name of table object
C   Output:
C      BMAJ    R    Gaussian major axis in sec.
C      BMIN    R    Gaussian minor axis in sec.
C      BPA     R    Gaussian position angle in degrees
C      FACTOR  R    Flux factor.
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*)
      INTEGER   IERR
      REAL      BMAJ, BMIN, BPA, FACTOR
C
      INTEGER   DIM(7), TYPE
      CHARACTER CDUMMY*1
C-----------------------------------------------------------------------
C                                       Gaussian
      CALL OGET (INTAB, 'BMAJ', TYPE, DIM, BMAJ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'BMIN', TYPE, DIM, BMIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'BPA', TYPE, DIM, BPA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       FACTOR
      CALL OGET (INTAB, 'FACTOR', TYPE, DIM, FACTOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (ABS(FACTOR).LE.1.0E-20) FACTOR = 1.0
C
 999  RETURN
      END
      SUBROUTINE CCTSET (NAME, IERR)
C-----------------------------------------------------------------------
C   Sets up descriptive info for CC table object containing Gaussians.
C   Inputs:
C      NAME    C*?  CC table object name.
C   Output:
C      IERR    I    Return error code, 0=>OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER MAXCXC
C                                       MAXCXC = number of columns.
      PARAMETER (MAXCXC = 7)
      CHARACTER TITLE(MAXCXC)*24, CTITLE*(MAXCXC*24), TTITLE*56,
     *   UNITS(MAXCXC)*8, CUNITS*(MAXCXC*8), CDUMMY*1
      INTEGER   DIM(7), COLTYP(MAXCXC), COLDIM(MAXCXC), NCOL, DUMMY
      EQUIVALENCE (TITLE, CTITLE), (UNITS, CUNITS)
      INCLUDE 'INCS:PAOOF.INC'
      DATA TTITLE /'AIPS CLEAN COMPONENT TABLE'/
      DATA COLTYP /OOARE, OOARE, OOARE, OOARE, OOARE, OOARE, OOARE/
      DATA COLDIM /1, 1, 1, 1, 1, 1, 1/
      DATA TITLE /'FLUX', 'DELTAX', 'DELTAY', 'MAJOR AX', 'MINOR AX',
     *   'POSANGLE','TYPE OBJ'/
      DATA UNITS /'Jy', 'Degrees', 'Degrees', 'Degrees', 'Degrees',
     *   'Degrees',  '    '/
C-----------------------------------------------------------------------
C                                       Structural information
      NCOL = MAXCXC
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (NAME, 'NCOL', OOAINT, DIM, NCOL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 56
      CALL OPUT (NAME, 'LABEL', OOACAR, DIM, DUMMY, TTITLE, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 24
      DIM(2) = NCOL
      CALL OPUT (NAME, 'COLABEL', OOACAR, DIM, DUMMY, CTITLE, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 8
      CALL OPUT (NAME, 'COLUNIT', OOACAR, DIM, DUMMY, CUNITS, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = NCOL
      DIM(2) = 1
      CALL OPUT (NAME, 'COLTYPE', OOAINT, DIM, COLTYP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (NAME, 'COLDIM', OOAINT, DIM, COLDIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE CCTGET (NAME, ROW, X, Y, FLUX, IERR)
C-----------------------------------------------------------------------
C   Get row from CC (CLEAN component) table object.
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
      SUBROUTINE CCTPUT (NAME, ROW, X, Y, FLUX, MAJ, MIN, PA, IERR)
C-----------------------------------------------------------------------
C   Write row to CC (CLEAN component) table object.
C   Inputs:
C      NAME    C*?  CC table object name.
C      ROW     I    Row number
C      X       R    X coordinate
C      Y       R    Y coordinate
C      FLUX    R    Component flux density
C      MAJ     R    Major axis size
C      MIN     R    Minor axis size
C      PA      R    Position angle
C   Output:
C      IERR    I    Return error code, 0=>OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   ROW, IERR
      REAL      X, Y, FLUX, MAJ, MIN, PA
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
      CALL TABDPT (NAME, ROW, 4, OOARE, DIM, MAJ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABDPT (NAME, ROW, 5, OOARE, DIM, MIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABDPT (NAME, ROW, 6, OOARE, DIM, PA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABDPT (NAME, ROW, 7, OOARE, DIM, 1.0, CDUMMY, IERR)
C
 999  RETURN
      END
