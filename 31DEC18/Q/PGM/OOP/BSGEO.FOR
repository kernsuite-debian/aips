LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER NPARMS
      PARAMETER (NPARMS=19)
      INTEGER AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
      INCLUDE 'INCS:PAOOF.INC'
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK',
     *   'BLC', 'TRC', 'IMSIZE', 'CELLSIZE', 'REWEIGHT', 'XTYPE',
     *   'YTYPE', 'XPARM', 'YPARM', 'OPTYPE', 'APARM'/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT,
     *   OOACAR, OOACAR, OOAINT, OOAINT,
     *   OOAINT, OOAINT, OOAINT, OOARE, OOARE, OOAINT,
     *   OOAINT, OOARE, OOARE, OOACAR, OOARE/
      DATA AVDIM /12,1, 6,1, 1,1, 1,1,
     *   12,1, 6,1, 1,1, 1,1,
     *   7,1, 7,1, 2,1, 2,1, 2,1, 1,1,
     *   1,1, 10,1, 10,1, 4,1, 10,1/
LOCAL END
      PROGRAM BSGEO
C-----------------------------------------------------------------------
C! Geometric interpolation from Az-el to Ra-dec
C# Task MAP-UTIL OOP SINGLEDISH
C-----------------------------------------------------------------------
C;  Copyright (C) 1997
C;  Associated Universities, Inc. Washington DC, USA.
C;  Eric W. Greisen
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
      CHARACTER PRGM*6, IN*32, OUT*32
      INTEGER   IRET, BUFF1(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'BSGEO '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL BSGEIN (PRGM, IN, OUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Interpolate
      CALL IMGRGR (IN, OUT, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL BEMCOP (IN, OUT, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL OCLOSE (OUT, IRET)
C                                       History
      CALL BSGEHI (IN, OUT)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE BSGEIN (PRGN, IN, OUT, IRET)
C-----------------------------------------------------------------------
C   BSGEIN gets input parameters for BSGEO and creates the output.
C   Inputs:
C      PRGN     C*6    Program name
C   Output:
C      IN       C*?    Input object
C      OUT      C*?    Output object
C      IRET     I      Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, IN*(*), OUT*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NKEY1, NKEY2
C                                       NKEY1=no. adverbs for inname
      PARAMETER (NKEY1=6)
      PARAMETER (NKEY2=7)
      INTEGER   DIM(7), TYPE, DISK, SEQ, DUMMY, IDDEG
      REAL      APARM(10), REWT(2)
      DOUBLE PRECISION COORD(2)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, NAME*12, CLASS*6, INAME*12, CDUMMY*1
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs for IN
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'BLC', 'TRC'/
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'BLC', 'TRC'/
C                                       Some of adverbs for OUT
      DATA INK2 /'XTYPE', 'YTYPE', 'XPARM', 'YPARM', 'CELLSIZE',
     *   'IMSIZE', 'OPTYPE'/
      DATA OUTK2 /'CTYPX', 'CTYPY', 'XPARM', 'YPARM', 'CELLSIZE',
     *   'IMSIZE', 'CPROJ'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create IN
      IN = 'Input image object'
      CALL CREATE (IN, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, IN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open and close to check
      CALL OOPEN (IN, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (IN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Clone output from IN2
      OUT = 'Output interpolated image'
      CALL CREATE (OUT, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy array descriptors
      CALL ARDCOP (IN, OUT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, OUT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Set names
      CALL OGET (IN, 'NAME', TYPE, DIM, DUMMY, INAME, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'OUTNAME', TYPE, DIM, DUMMY, NAME, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'OUTCLASS', TYPE, DIM, DUMMY, CLASS, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'OUTSEQ', TYPE, DIM, SEQ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'OUTDISK', TYPE, DIM, DISK, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NAME.EQ.' ') NAME = INAME
      IF (CLASS.EQ.' ') CLASS = PRGN
      DIM(1) = LEN (NAME)
      CALL OPUT (OUT, 'NAME', OOACAR, DIM, DUMMY, NAME, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = LEN (CLASS)
      CALL OPUT (OUT, 'CLASS', OOACAR, DIM, DUMMY, CLASS, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 1
      CALL OPUT (OUT, 'IMSEQ', OOAINT, DIM, SEQ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (OUT, 'DISK', OOAINT, DIM, DISK, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       center coordinates
      CALL OGET ('Input', 'APARM', TYPE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      COORD(1) = (APARM(1) + APARM(2)/60. + APARM(3)/3600.) * 15.0
      IDDEG = ABS (APARM(4))
      COORD(2) = (IDDEG + APARM(5)/60. + (APARM(6)/3600.)) *
     *   SIGN (1.0, APARM(4))
      DIM(1) = 2
      CALL OPUT (OUT, 'CCENTER', OOADP, DIM, COORD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Control parameters
      CALL OGET ('Input', 'REWEIGHT', TYPE, DIM, REWT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 1
      CALL OPUT (OUT, 'SDCUTOFF', OOARE, DIM, REWT(2), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE BSGEHI (IN, OUT)
C-----------------------------------------------------------------------
C   Routine to write history file to output.
C   Inputs:
C      IN      C*?  Input object
C      OUT     C*?  Output object
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
C
      INTEGER   NADV
      PARAMETER (NADV=9)
      CHARACTER LIST(NADV)*8
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'BLC', 'TRC', 'IMSIZE', 'REWEIGHT', 'APARM'/
C-----------------------------------------------------------------------
C                                        Copy old history
      CALL OHCOPY (IN, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        New additions - copy adverb
C                                        values.
      CALL OHLIST ('Input', LIST, NADV, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // OUT
      CALL MSGWRT (6)
 999  RETURN
      END
