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
C                                       Uses PAOOF.INC
C                      1        2          3         4        5
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'OUTNAME',
C            6           7          8       9        10
     *   'OUTCLASS', 'OUTDISK', 'OUTSEQ', 'APARM', 'BADDISK'/
C                    1       2       3       4       5       6
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOACAR, OOACAR,
C           7       8      9       10
     *   OOAINT, OOAINT, OOARE, OOAINT/
C                   1    2    3    4    5    6    7    8     9    10
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 12,1, 6,1, 1,1, 1,1, 10,1, 10,1/
C        11   12   13    14   15   16   17   18   19   20   21   22
LOCAL END
      PROGRAM PHASE
C-----------------------------------------------------------------------
C! Baseline dependent time averaging of uv data
C# UV Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C   Baseline dependent time averaging of uv data
C   INNAME                             Input UV data (name)
C   INCLASS                            Input UV data (class)
C   INSEQ                              Input UV data (seq. #)
C   INDISK                             Input UV data disk drive #
C   OUTNAME                            Output uvdata name (name)
C   OUTCLASS                           Output uv data class
C   OUTDISK                            Output uvdata disk drive #
C   OUTSEQ          -1.0    32000.0    Output seq. no.
C   APARM                              (1) max. time (sec)
C                                      (2) min. SNR acceptable
C                                      (3) Vector/Scalar Limit
C   BADDISK                            Disk drive #'s to avoid
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, UVIN*36, UVOUT*36
      INTEGER  IRET, BUFF(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'PHASE '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL PHSIN (PRGM, UVIN, UVOUT, IRET)
C                                       Average
      IF (IRET.EQ.0) CALL UVPHAS (UVIN, UVOUT, IRET)
C                                       History
      IF (IRET.EQ.0) CALL HISTRY (UVIN, UVOUT)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF)
C
 999  STOP
      END
      SUBROUTINE PHSIN (PRGN, UVIN, UVOUT, IRET)
C-----------------------------------------------------------------------
C   PHSIN gets input parameters for PHASE and creates the input and
C   output objects
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      UVIN  C*?  Input uv data object
C      BEAM    C*?  Output beam object
C      UVOUT   C*?  Output Image object
C      IRET    I    Error code: 0 => ok, else failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, UVIN*(*), UVOUT*(*)
C
      INTEGER   NKEY1
C                                       NKEY1=no. adverbs to copy to
C                                       UVIN
      PARAMETER (NKEY1=8)
      INTEGER   IERR, DIM(7), TYPE
      REAL      APARM(10)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Rename to object
C                   1        2       3      4         5          6
      DATA OUTK1 /'NAME', 'CLASS', 'SEQ', 'DISK', 'OUTNAME', 'OUTCLASS',
C            7          8
     *   'OUTDISK', 'OUTSEQ'/
C-----------------------------------------------------------------------
C                                       Copy Name adverbs
      CALL CCOPY ( NKEY1, AVNAME, INK1)
C                                       Startup
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BADDISK
      CALL OGET ('Input', 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IERR)
      IF (IRET.NE.0) GO TO 999
C                                       Check inputs and set defaults
C                                       Control array
      CALL OGET ('Input', 'APARM', TYPE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Max time (default infinity)
      IF (APARM(1).LE.0.0001) APARM(1) = 1.0E20
C                                       Min SNR default to reject
      IF (APARM(2).LE.0.0001) APARM(2) = 5.0
C                                       Self Cal SNR default
      IF (APARM(3).LE.0.0001) APARM(3) = 3.0
C                                       Vector/Scalar Cutoff
      IF (APARM(4).LE.0.0001) APARM(4) = .5
      CALL OPUT ('Input', 'APARM', TYPE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create input uv data object
      UVIN = 'Input uv data'
      CALL CREATE (UVIN, 'UVDATA', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, UVIN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Output
      UVOUT = 'Output averaged uv data'
C                                       Max averaging time
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 2
      CALL OPUT (UVIN, 'MAXATIME', OOARE, DIM, APARM(1), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Minimum SNR to pass
      CALL OPUT (UVIN, 'MINSNR', OOARE, DIM, APARM(2), CDUMMY, IERR)
C                                       Self-Cal SNR per baselin
      CALL OPUT (UVIN, 'SCLSNR', OOARE, DIM, APARM(3), CDUMMY, IERR)
C                                       Self-Cal SNR per baselin
      CALL OPUT (UVIN, 'VECSCL', OOARE, DIM, APARM(4), CDUMMY, IERR)
C                                       Coherent Average flag
      CALL OPUT (UVIN, 'COAVER', OOARE, DIM, APARM(5), CDUMMY, IERR)
C                                       Interval to write data
      CALL OPUT (UVIN, 'AVEOUT', OOARE, DIM, APARM(6), CDUMMY, IERR)
C                                       Print level
      CALL OPUT (UVIN, 'PRTLEV', OOARE, DIM, APARM(10), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE HISTRY (UVIN, UVOUT)
C-----------------------------------------------------------------------
C   Routine to write history file to output image object.
C   Inputs:
C      UVIN  C*?  UV data object
C      UVOUT   C*?  output image object
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
C
      INTEGER   NADV
      PARAMETER (NADV=5)
      CHARACTER LIST(NADV)*8
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'APARM'/
C-----------------------------------------------------------------------
C                                        Copy old history
      CALL OHCOPY (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        New additions - copy adverb
C                                        values.
      CALL OHLIST ('Input', LIST, NADV, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // UVOUT
      CALL MSGWRT (4)
C
 999  RETURN
      END
