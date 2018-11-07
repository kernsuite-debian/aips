LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER NPARMS
      PARAMETER (NPARMS=21)
      INTEGER AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                      1        2         3          4
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
C            5          6          7         8          9
     *   'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK', 'IN2VER',
C           10        11
     *   'NITER',  'PMODEL',
C           12         13           14        15
     *   'OUTNAME', 'OUTCLASS', 'OUTDISK', 'OUTSEQ',
C           16        17        18       19        20        21
     *   'SOLINT', 'APARM',  'ANTWT', 'UVRANGE', 'WTUV', 'BADDISK'/
C                    1       2       3       4       5       6
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOACAR, OOACAR,
C           7      8       9       10      11
     *   OOAINT, OOAINT, OOAINT, OOAINT, OOARE,
C          12      13      14      15
     *   OOACAR, OOACAR, OOAINT, OOAINT,
C          16    17      18     19     20     21
     *   OOARE, OOARE, OOARE, OOARE, OOARE, OOAINT/
C                   1    2    3    4     5    6    7    8    9    10
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 12,1, 6,1, 1,1, 1,1, 1,1, 1,1,
C         11
     *   7,1,
C          12   13   14   15   16    17    18   19   20    21
     *   12,1, 6,1, 1,1, 1,1, 1,1, 10,1, 30,1, 2,1, 1,1, 10,1/
LOCAL END
      PROGRAM FRCAL
C-----------------------------------------------------------------------
C! Faraday rotation self calibration task
C# Task AP OOP calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   Faraday rotation self calibration task
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, UVIN*32, UVOUT*32, UVSCR*32, MODIM(2)*32,
     *   SNTAB*32
      INTEGER  IRET, BUFF1(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'FRCAL '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL FRSCIN (PRGM, UVIN, UVOUT, UVSCR, MODIM, SNTAB, IRET)
C                                       Self calibrate
      IF (IRET.EQ.0) CALL FRSCAL (UVIN, UVOUT, UVSCR, MODIM, SNTAB,
     *   IRET)
C                                       History
      IF (IRET.EQ.0) CALL FRSCHI (UVIN, UVOUT)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE FRSCIN (PRGN, UVIN, UVOUT, UVSCR, MODIM, SNTAB, IRET)
C-----------------------------------------------------------------------
C   FRSCIN gets input parameters for FRCAL and creates objects.
C   Inputs:
C      PRGN     C*6  Program name
C   Output:
C      UVIN     C*32 Input uv object
C      UVOUT    C*32 Output uv object
C      UVSCR    C*32 Scratch uv object.  Necessary adverbs for
C                    controlling self calibration are copied.
C      MODIM(2) C*32 Input model image.
C      SNTAB    C*32 SN table object, associated with input uv data.
C      IRET     I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, UVIN*(*), UVOUT*(*), UVSCR*(*), MODIM(2)*(*),
     *   SNTAB*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NKEY1, NKEY2, NKEY3, NKEY4
C                                       NKEY1=no. adverbs to copy to
C                                       UVIN object
      PARAMETER (NKEY1=8)
C                                       NKEY2 = no. adverb for MODIM
      PARAMETER (NKEY2=6)
C                                       NKEY3 = no. adverb for SNTAB
      PARAMETER (NKEY3=4)
C                                       NKEY4 = no. adverb for UVSCR
      PARAMETER (NKEY4=4)
      INTEGER   DIM(7), TYPE, MINNO, IROUND, NVIS, PRTLEV, DUMMY
      LOGICAL   AVGIF, DIVMOD, DOPMOD, DIVDON
      REAL      APARM(10), PMODEL(6)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32,
     *   INK2(NKEY2)*8, OUTK2(NKEY2)*32, INK3(NKEY3)*8, OUTK3(NKEY3)*32,
     *   INK4(NKEY4)*8, OUTK4(NKEY4)*32, CLASS*6, CDUMMY*1
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to UVIN object
C                   1          2         3        4
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
C            5          6          7          8
     *   'OUTNAME', 'OUTCLASS', 'OUTDISK', 'OUTSEQ'/
C                                       Rename
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK',
C            5          6          7          8
     *   'OUTNAME', 'OUTCLASS', 'OUTDISK', 'OUTSEQ'/
C                                       Adverbs for MODIM object
C                    1           2         3          4         5
      DATA INK2 /'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK', 'IN2VER',
C           6
     *   'NITER'/
C                   1        2        3       4       5       6
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'VER', 'NCC'/
C                                       Adverbs for SNTAB object
C                    1        2         3         4
      DATA INK3 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK'/
C                   1        2        3       4
      DATA OUTK3 /'NAME', 'CLASS', 'IMSEQ', 'DISK'/
C                                       Adverbs for UVSCR
C                   1          2         3        4
      DATA INK4 /'SOLINT', 'ANTWT', 'UVRANGE', 'WTUV'/
C                   1          2         3         4
      DATA OUTK4 /'SOLINT', 'ANTWT', 'UVR_FULL', 'WTUV'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BADDISK
      CALL OGET ('Input', 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create MODIM object(s)
      MODIM(1) = 'Input model Q Image'
      CALL CREATE (MODIM(1), 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, MODIM(1), IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Set class
      CALL OGET (MODIM(1), 'CLASS', TYPE, DIM, DUMMY, CLASS, IRET)
      IF (IRET.NE.0) GO TO 999
      CLASS(1:1) = 'Q'
      CALL OPUT (MODIM(1), 'CLASS', TYPE, DIM, DUMMY, CLASS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create MODIM object(s)
      MODIM(2) = 'Input model U Image'
      CALL CREATE (MODIM(2), 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, MODIM(2), IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Set class
      CALL OGET (MODIM(2), 'CLASS', TYPE, DIM, DUMMY, CLASS, IRET)
      IF (IRET.NE.0) GO TO 999
      CLASS(1:1) = 'U'
      CALL OPUT (MODIM(2), 'CLASS', TYPE, DIM, DUMMY, CLASS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create UVIN
      UVIN = 'Input uv data'
      CALL CREATE (UVIN, 'UVDATA', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, UVIN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Point model?
      CALL OGET ('Input', 'PMODEL', TYPE, DIM, PMODEL, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DOPMOD = (ABS (PMODEL(2)).GT.0.0) .OR. (ABS (PMODEL(3)).GT.0.0)
      DIM(1) = 1
      CALL OPUT (UVIN, 'MODDOPT', OOALOG, DIM, DOPMOD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (DOPMOD) THEN
         MSGTXT = 'Using PMODEL polarization model'
         CALL MSGWRT (4)
         DIM(1) = 4
         CALL OPUT (UVIN, 'MODPTFLX', OOARE, DIM, PMODEL, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         DIM(1) = 1
         CALL OPUT (UVIN, 'MODPTXOF', OOARE, DIM, PMODEL(5), CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT (UVIN, 'MODPTYOF', OOARE, DIM, PMODEL(6), CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Open and close input to check
      CALL OOPEN (UVIN, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (UVIN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Number of visibilities
      CALL OGET (UVIN, 'UV_DESC.GCOUNT', TYPE, DIM, NVIS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create SNTAB Object
      SNTAB = 'SN table'
      CALL CREATE (SNTAB, 'TABLE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY3, INK3, OUTK3, SNTAB, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Table type
      DIM(1) = 2
      CALL OPUT (SNTAB, 'TBLTYPE', OOACAR, DIM, DUMMY, 'SN', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Force a new version
      DIM(1) = 1
      CALL OPUT (SNTAB, 'VER', OOAINT, DIM, 0, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Scratch uv object
      UVSCR = 'uv data divided by model'
      CALL OUVSCR (UVSCR, UVIN, NVIS, .FALSE., IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY4, INK4, OUTK4, UVSCR, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Digest APARM
      CALL OGET ('Input', 'APARM', TYPE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Min number of antennas.
      IF (APARM(1).LE.0.0) APARM(1) = 6.0
      MINNO = IROUND (APARM(1))
      DIM(1) = 1
      CALL OPUT (UVSCR, 'MINNO', OOAINT, DIM, MINNO, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Min SNR
      IF (APARM(2).LE.0.0) APARM(2) = 5.0
      CALL OPUT (UVSCR, 'SNRMIN', OOARE, DIM, APARM(2), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Min ratio
      IF (APARM(3).LE.0.0) APARM(3) = 0.75
      CALL OPUT (UVSCR, 'RATMIN', OOARE, DIM, APARM(3), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Max ratio
      IF (APARM(4).LE.0.0) APARM(4) = 1.50
      CALL OPUT (UVSCR, 'RATMAX', OOARE, DIM, APARM(4), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Average in IF?
      AVGIF = APARM(5).GT.0.01
      CALL OPUT (UVSCR, 'AVGIF', OOALOG, DIM, AVGIF, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Divided only?
      DIVMOD = APARM(6).GT.0.01
      CALL OPUT (UVIN, 'DIVONLY', OOALOG, DIM, DIVMOD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Show results?
      PRTLEV = APARM(7) + 0.1
      CALL OPUT (UVSCR, 'PRTLV', OOAINT, DIM, PRTLEV, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVIN, 'MODDOMSG', OOALOG, DIM, .TRUE., CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Already divided by model
      DIVDON = APARM(8).GT.0.01
      CALL OPUT (UVIN, 'DIVDONE', OOALOG, DIM, DIVDON, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Save APARM for history
      DIM(1) = 10
      CALL OPUT ('Input', 'APARM', OOARE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Clone output uvdata object
      UVOUT = 'Output uv data'
      IF (.NOT.DIVDON) THEN
         CALL OCLONE (UVIN, UVOUT, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C
 999  RETURN
      END
      SUBROUTINE FRSCAL (UVIN, UVOUT, UVSCR, MODIM, SNTAB, IERR)
C-----------------------------------------------------------------------
C   Does Self calibration
C   Inputs:
C      UVIN    C*32 Input uv object
C      UVOUT   C*32 Output uv object
C      UVSCR   C*32 Scratch uv object
C      MODIM(2)C*32 Input model images
C      SNTAB   C*32 SN table object, associated with input uv data.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*), UVSCR*(*), MODIM(2)*(*), SNTAB*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER UVTMP*32, CDUMMY*1
      INTEGER   TYPE, DIM(7), MFIELD, NCHAN, BCHAN, ECHAN, BIF, EIF,
     *   PCVER(2,MAXFLD), NPCC(2,MAXFLD), VER, DUMMY
      LOGICAL   DIVMOD, DIVDON, INCOMP, DOPMOD
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Divide data by model
      MFIELD = 1
C                                       Already divided?
      CALL OGET (UVIN, 'DIVDONE', TYPE, DIM, DIVDON, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Use PMODEL?
      CALL OGET (UVIN, 'MODDOPT', TYPE, DIM, DOPMOD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (.NOT.DOPMOD) THEN
C                                       Open Q model object
         CALL OOPEN (MODIM(1), 'READ', IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Get version number, no. CC
         CALL OGET (MODIM(1), 'VER', TYPE, DIM, PCVER(1,1), CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OGET (MODIM(1), 'NCC', TYPE, DIM, NPCC(1,1), CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Close model object
         CALL OCLOSE (MODIM(1), IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Open U model object
         CALL OOPEN (MODIM(2), 'READ', IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Get version number, no. CC
         CALL OGET (MODIM(2), 'VER', TYPE, DIM, PCVER(2,1), CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OGET (MODIM(2), 'NCC', TYPE, DIM, NPCC(2,1), CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Close model object
         CALL OCLOSE (MODIM(2), IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       If input is compressed expand it
C                                       to the output
      CALL UVDGET (UVIN, 'ISCOMP', TYPE, DIM, INCOMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (INCOMP .AND. (.NOT.DIVDON)) THEN
         CALL UVCOPY (UVIN, UVOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         UVTMP = UVOUT
      ELSE
         UVTMP = UVIN
         END IF
C                                       Save info on UVTMP
      IF (.NOT.DOPMOD) THEN
         DIM(1) = 2
         DIM(2) = 1
         DIM(3) = 0
         CALL OPUT (UVTMP, 'MODPCVER', OOAINT, DIM, PCVER, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OPUT (UVTMP, 'MODPCEND', OOAINT, DIM, NPCC, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Force using CC table
      DIM(1) = 4
      DIM(2) = 1
      CALL OPUT (UVTMP, 'MODMODEL', OOACAR, DIM, DUMMY, 'CC  ', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       All selected frequencies
C                                       Find number of channels
      CALL OGET (UVIN, 'CALEDIT.BCHAN', TYPE, DIM, BCHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (UVIN, 'CALEDIT.ECHAN', TYPE, DIM, ECHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      BCHAN = MAX (1, BCHAN)
      ECHAN = MAX (BCHAN, ECHAN)
C                                       Find number of IFs
      CALL OGET (UVIN, 'CALEDIT.BIF', TYPE, DIM, BIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (UVIN, 'CALEDIT.EIF', TYPE, DIM, EIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      BIF = MAX (1, BIF)
      EIF = MAX (BIF, EIF)
      NCHAN = (ECHAN - BCHAN + 1) * (EIF - BIF + 1)
      NCHAN = MAX (NCHAN, 1)
C                                       Only do division?
      CALL OGET (UVIN, 'DIVONLY', TYPE, DIM, DIVMOD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Already divided?
      IF (DIVDON) THEN
         MSGTXT = 'Data already divided by model'
         CALL MSGWRT (6)
         CALL UVCOPY (UVIN, UVSCR, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
C                                       Divide
         IF (DIVMOD) THEN
            MSGTXT = 'NOTE: Output is input divided by model only'
            CALL MSGWRT (6)
            CALL OUPDIV (UVTMP, UVOUT, MFIELD, MODIM, BCHAN, NCHAN,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            GO TO 800
         ELSE
            CALL OUPDIV (UVTMP, UVSCR, MFIELD, MODIM, BCHAN, NCHAN,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
C                                       Do solutions
      CALL UVFRSC (UVSCR, SNTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (.NOT.DIVDON) THEN
C                                       Apply to output data
C                                       Set up to apply calibration
         CALL OGET (SNTAB, 'VER', TYPE, DIM, VER, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OPUT (UVIN, 'CALEDIT.CLUSE', TYPE, DIM, VER, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OPUT (UVIN, 'CALEDIT.DOCAL', OOALOG, DIM, .TRUE., CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         DIM(1) = 4
         CALL OPUT (UVIN, 'CALEDIT.STOKES', OOACAR, DIM, DUMMY, '    ',
     *      IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Start at beginning of output
         DIM(1) = 1
         CALL OPUT (UVOUT, 'UV_DESC.VISOFF', OOAINT, DIM, 0, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         CALL UVCOPY (UVIN, UVOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Delete scratch file to avoid
C                                       messages.
 800  CALL OUVZAP (UVSCR, IERR)
      IERR = 0
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR FARADAY SELF CALIBRATING DATA'
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE FRSCHI (UVIN, UVOUT)
C-----------------------------------------------------------------------
C   Routine to write history file to output self calibrated data.
C   Inputs:
C      UVIN    C*32 Input uv object
C      UVOUT   C*32 Output uv object
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
C
      INTEGER   NADV
      PARAMETER (NADV = 16)
      CHARACTER LIST(NADV)*8, HILINE*72, CDUMMY*1
      INTEGER   IERR, TYPE, DIM(3)
      LOGICAL   DIVMOD, DIVDON
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK', 'IN2VER', 'PMODEL',
     *   'NITER', 'SOLINT', 'APARM', 'ANTWT', 'UVRANGE', 'WTUV'/
C-----------------------------------------------------------------------
C                                       No output for division
      CALL OGET (UVIN, 'DIVDONE', TYPE, DIM, DIVDON, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DIVDON) THEN
         CALL OHTIME (UVIN, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       New additions - copy adverb
C                                       values.
         CALL OHLIST ('Input', LIST, NADV, UVIN, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
C                                       Old history
         CALL OHCOPY (UVIN, UVOUT, IERR)
         IF (IERR.NE.0) GO TO 990
C                                        New additions - copy adverb
C                                       values.
         CALL OHLIST ('Input', LIST, NADV, UVOUT, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Divided only?
         CALL OGET (UVIN, 'DIVONLY', TYPE, DIM, DIVMOD, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (DIVMOD) THEN
            HILINE = '/ Data divided by model only'
            CALL OHWRIT (HILINE, UVOUT, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         END IF
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // UVOUT
      CALL MSGWRT (4)
 999  RETURN
      END
