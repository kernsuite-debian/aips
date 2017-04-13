LOCAL INCLUDE 'INPUT.INC'
      INCLUDE 'INCS:PCLN.INC'
C                                       Declarations for inputs
      INTEGER   NPARMS
C                                       NPARMS=no. adverbs passed.
      PARAMETER (NPARMS=15)
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
C           6           7         8         9        10         11
     *   'OUTVERS',  'BCOUNT' ,'ECOUNT', 'CUTOFF','BOXFILE', 'NBOXES',
C           12       13        14        15
     *   'CLBOX', 'NCCBOX', 'CCBOX',  'CPARM' /
C                                       Adverb data types (PAOOF.INC)
C                     1       2      3       4        5
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOAINT,
C          6       7       8       9      10      11
     *   OOAINT, OOAINT, OOAINT, OOARE, OOACAR, OOAINT,
C          12     13      14     15
     *   OOARE, OOAINT, OOARE, OOARE /
C                                       Adverb dimensions (as 2D)
C                   1    2    3    4    5
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 1,1,
C          6    7    8    9     10   11
     *    1,1, 1,1, 1,1, 1,1, 48,1, 1,1,
C         12        13    14    15
     *   4,MXCLBX, 1,1, 4,10, 10,1 /
LOCAL END
LOCAL INCLUDE 'CONSTANTS.INC'
      INTEGER MAXTAB
C                                       MAXTAB= max no. output tables
      PARAMETER (MAXTAB=30)
LOCAL END
LOCAL INCLUDE 'BOXES.INC'
      INCLUDE 'INCS:PCLN.INC'
      INTEGER   NCCBOX, ICCBOX
      REAL      CCBOX(4,MXNBOX)
      COMMON /BOXES/ CCBOX, NCCBOX, ICCBOX
LOCAL END
      PROGRAM CCEDT
C-----------------------------------------------------------------------
C! Select CC components in region and by flux.
C# Utility OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-2001, 2003, 2005-2006, 2008-2009, 2012
C;  Copyright (C) 2015
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
      INCLUDE 'CONSTANTS.INC'
      CHARACTER PRGM*6, INTAB*32, OUTTAB(MAXTAB)*32
      INTEGER   IRET, NOTAB, BUFF1(256), NROWS, NWORDS, NC
      LONGINT   IOFF
      REAL      CCDATA(2)
      INTEGER   ICDATA(2)
      EQUIVALENCE (ICDATA, CCDATA)
      DATA PRGM /'CCEDT '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL CEDTIN (PRGM, INTAB, OUTTAB, NOTAB, NROWS, IRET)
C                                       Process table
      IF (IRET.EQ.0) THEN
         NC = 8
         NWORDS = (NROWS * NC - 1) / 1024 + 1
         CALL ZMEMRY ('GET', 'CCEDT', NWORDS, CCDATA, IOFF, IRET)
         END IF
      IF (IRET.EQ.0) CALL CEDTAB (INTAB, OUTTAB, NOTAB, NROWS, NC,
     *   CCDATA(1+IOFF), ICDATA(1+IOFF), IRET)
C                                       History
      IF (IRET.EQ.0) CALL CEDTHI (OUTTAB(1))
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE CEDTIN (PRGN, INTAB, OUTTAB, NOTAB, NROWS, IRET)
C-----------------------------------------------------------------------
C   CEDTIN gets input parameters for CCEDT and creates the input and
C   output objects
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      INTAB   C    Name of the input table object
C      OUTTAB  C    Names of the output table objects
C      NOTAB   I    No. of output objects (tables)
C      NROWS   I    Number rows in CC
C      NCOLS   I    Number of Columns in CC
C      IRET    I    Error code: 0 => ok
C                               4 => user routine detected error.
C                               5 => catalog troubles
C                               8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS file
C-----------------------------------------------------------------------
      INCLUDE 'CONSTANTS.INC'
      INCLUDE 'INPUT.INC'
      INTEGER   NOTAB, NROWS, IRET
      REAL      CPARM(10)
      CHARACTER PRGN*6, INTAB*32, OUTTAB(MAXTAB)*32
C
      INTEGER   NKEY1, NKEY2
C                                       NKEY1=no. adverbs to copy to
C                                       INTAB
      PARAMETER (NKEY1=9)
C                                       NKEY2=no. adverbs to copy to
C                                       OUTTAB
      PARAMETER (NKEY2=5)
      INTEGER   DIM(3), DUMMY, TABLE, TYPE, BC, EC, IFIELD
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, OUTT*32, CDUMMY*1, BOXFIL*48, INCLAS*6
      INCLUDE 'BOXES.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to INTAB
C                   1         2          3        4         5
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INVERS',
C           6         7         8         9
     *   'BCOUNT', 'ECOUNT', 'CUTOFF', 'CPARM' /
C                                       May rename adverbs to INTAB
C                    1       2        3        4       5
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'VER',
C           6         7         8         9
     *   'BCOUNT', 'ECOUNT', 'CUTOFF', 'CPARM' /
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
      CALL OGET ('Input', 'NCCBOX', TYPE, DIM, NOTAB, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get automatic split pars
      CALL OGET (INTAB, 'CPARM', TYPE, DIM, CPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (CPARM(3).GT.MAXTAB) THEN
         WRITE (MSGTXT,1000) MAXTAB
         CALL MSGWRT (8)
         CPARM(3) = MAXTAB
         END IF
      IF (CPARM(4).LE.0.0) CPARM(4) = 0.95
C                                       Store CPARM
      CALL OPUT (INTAB, 'CPARM', OOARE, DIM, CPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Automatic model split requested
      IF (CPARM(3).GT.0) NOTAB = -INT(CPARM(3))
C                                       Number of output tables
      IF (NOTAB.LT.0) THEN
         NOTAB = -NOTAB
         IF (NOTAB.GT.MAXTAB) THEN
            WRITE (MSGTXT,1001) NOTAB, MAXTAB
            CALL MSGWRT (8)
            IRET = 8
            GO TO 999
            END IF
C                                       Multiple tables, zero OUTVERS
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL OPUT ('Input', 'OUTVERS', OOAINT, DIM, 0, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
      ELSE
         NOTAB = 1
         END IF
C                                       Create Output Objects
      OUTT = 'Output table'
      DO 100 TABLE = 1,NOTAB
C                                       Object name
         WRITE (OUTT(13:14),1010) TABLE
C
         CALL CREATE (OUTT, 'TABLE', IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
         CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, OUTT, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Table type
         DIM(1) = 2
         DIM(2) = 1
         DIM(3) = 0
         CALL OPUT (OUTT, 'TBLTYPE', OOACAR, DIM, DUMMY, 'CC', IRET)
         IF (IRET.NE.0) GO TO 999
C
         OUTTAB(TABLE) = OUTT
 100     CONTINUE
C                                       Open input table
      CALL OOPEN (INTAB, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get number of entries
      CALL OGET (INTAB, 'NROW', TYPE, DIM, NROWS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (INTAB, IRET)
C                                       Get windows
      CALL OGET ('Input', 'BOXFILE', TYPE, DIM, DUMMY, BOXFIL, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'NBOXES', TYPE, DIM, NCCBOX, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       read text file
      IF (BOXFIL.NE.' ') THEN
         ICCBOX = 1
         CALL OGET (INTAB, 'CLASS', TYPE, DIM, DUMMY, INCLAS, IRET)
         IF (IRET.NE.0) GO TO 999
         IF ((INCLAS(4:4).GE.'0') .AND. (INCLAS(4:4).LE.'9')) THEN
            READ (INCLAS(4:6),1020) IFIELD
         ELSE
            IF (INCLAS(5:6).EQ.' ') THEN
               IFIELD = 0
            ELSE
               CALL ZREHEX (2, INCLAS(5:6), IFIELD)
               END IF
            IFIELD = IFIELD + 1
            END IF
         CALL WINDF (IFIELD, BOXFIL, NCCBOX, CCBOX, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       NBOXES, CLBOX
      ELSE IF (NCCBOX.GT.0) THEN
         ICCBOX = 2
         CALL OGET ('Input', 'CLBOX', TYPE, DIM, CCBOX, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       NCCBOX, CCBOX
      ELSE
         ICCBOX = 3
         CALL OGET ('Input', 'NCCBOX', TYPE, DIM, NCCBOX, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OGET ('Input', 'CCBOX', TYPE, DIM, CCBOX, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Check multiple out
      NCCBOX = ABS (NCCBOX)
      IF ((NOTAB.GT.1) .AND. (NCCBOX.NE.NOTAB) .AND. (CPARM(3).LE.0.0))
     *   THEN
         WRITE (MSGTXT,1040) NCCBOX, NOTAB
         CALL MSGWRT (8)
         IRET = 8
         GO TO 999
         END IF
C                                       Store in input table object
C                                       Get range of rows.
      CALL OGET (INTAB, 'BCOUNT', TYPE, DIM, BC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      BC = MIN (MAX (BC, 1), NROWS)
      CALL OGET (INTAB, 'ECOUNT', TYPE, DIM, EC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF ((EC.LE.BC) .OR. (EC.GT.NROWS)) EC = NROWS
      CALL OPUT (INTAB, 'BCOUNT', TYPE, DIM, BC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (INTAB, 'ECOUNT', TYPE, DIM, EC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      NROWS = EC - BC + 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CEDTIN: MAX NUMBER OF OUTPUT TABLES ',I3)
 1001 FORMAT ('NUMBER OUTPUT TABLES',I5,' EXCEEDS MAXIMUM',I3)
 1010 FORMAT (I2)
 1020 FORMAT (I3)
 1040 FORMAT ('NUMBER BOXES',I4,' NOT EQUAL NUMBER OUTPUT CC FILES',I4)
      END
      SUBROUTINE CEDTAB (INTAB, OUTTAB, NOTAB, MAXCC, NC, CCD, ICD,
     *   IERR)
C-----------------------------------------------------------------------
C   Select components.
C   Inputs:
C      INTAB   C*      Name of input table object
C      OUTTAB  C*      Names of output table objects
C      NOTAB   I       Number of output table objects
C      MAXCC   I       Number of rows of CC data to read
C      NC      I       Other dimension of CCD
C   Output:
C      CCD     R(*)    Work buffer
C      ICD     I(*)    Work buffer - integer equiv to CCD
C      IERR    I       Error code: 0 => ok
C-----------------------------------------------------------------------
      INCLUDE 'CONSTANTS.INC'
      CHARACTER INTAB*32, OUTTAB(MAXTAB)*32
      INTEGER   NOTAB, MAXCC, NC, ICD(NC,MAXCC), IERR
      REAL      CCD(NC,MAXCC)
C
      INTEGER   MAXBOX, MAXITR
C                                       MAXBOX = max. # search boxes
      PARAMETER (MAXBOX = 10000)
C                                       MAXITR = max. # iterations
C                                       in a major split cycle
      PARAMETER (MAXITR = 5)
      INTEGER   XPOSI, YPOSI, FLUXI, XPOSO, YPOSO, FLUXO, FLUXT, OUTCC
      PARAMETER (XPOSI=1, YPOSI=2, FLUXI=3, XPOSO=4, YPOSO=5, FLUXO=6,
     *   FLUXT=7, OUTCC=8)
C
      INCLUDE 'BOXES.INC'
      INTEGER   IROW, OROW, NROW, BC, EC, TYPE, DIM(3), NCC, I, J, NMRG,
     *   BOX(MAXBOX), NOUTCC(MAXBOX), OUTVER, NCYCLS, CYCLE, JMAX,
     *   NOTABC, NX, NY, TOPBOX, ITER, TMPI, NCOL, IY, IX, LX, LY
      REAL      CUTOFF, XMIN, XMAX, YMIN, YMAX, FLXTOT, FLXOUT, FLXCYC,
     *   FLXTAB(MAXTAB), FLXBOX(MAXBOX), BMAJ, BMIN, BPA, DX, DY,
     *   CPARM(10), TOPFLX, TMPR, XINC, YINC, W0(4), W(4), R, TEMP
      LOGICAL   DUP, WARNED(MAXTAB,MAXTAB), WDWARN, ROUND, INSIDE
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      I = NC * MAXCC
      CALL RFILL (I, 0.0, CCD)
C                                       Open input table
      CALL OOPEN (INTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get number of entries
      CALL OGET (INTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'NCOL', TYPE, DIM, NCOL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get range of rows.
      CALL OGET (INTAB, 'BCOUNT', TYPE, DIM, BC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'ECOUNT', TYPE, DIM, EC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get automatic split pars
      CALL OGET (INTAB, 'CPARM', TYPE, DIM, CPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Filtering parameters
      CALL OGET (INTAB, 'CUTOFF', TYPE, DIM, CUTOFF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get CATBLK
      CALL OBHGET (INTAB, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Clean beam from header
      BMAJ = CATR(KRBMJ)
      BMIN = CATR(KRBMN)
      BPA = CATR(KRBPA) * DG2RAD
C                                       Set initial search box size
      DX = (ABS(BMAJ*SIN(BPA)) + ABS(BMIN*COS(BPA))) / 4.0
      DY = (ABS(BMAJ*COS(BPA)) + ABS(BMIN*SIN(BPA))) / 4.0
      XINC = ABS (CATR(KRCIC)) / 10.0
      YINC = ABS (CATR(KRCIC+1)) / 10.0
      CPARM(1) = CPARM(1) / 3600.0
      CPARM(2) = CPARM(2) / 3600.0
      ROUND = CPARM(1).LT.0.0
      IF (CPARM(3).LE.0.0) THEN
         IF (ROUND) THEN
            IF (CPARM(2).LE.0.) CPARM(2) = 25. * (XINC + YINC)
            CPARM(2) = CPARM(2) ** 2
         ELSE IF ((CPARM(1).LE.0.0) .AND. (CPARM(2).LE.0.0)) THEN
            CPARM(1) = 50.0 * XINC
            CPARM(2) = 50.0 * YINC
            MSGTXT = 'WARNING: CPARM(1  and 2) set to 5 cells'
            CALL MSGWRT (6)
         ELSE IF (CPARM(1).LE.0.0) THEN
            CPARM(1) = 50.0 * XINC
            MSGTXT = 'WARNING: CPARM(1) set to 5 cells'
            CALL MSGWRT (6)
         ELSE IF (CPARM(2).LE.0.0) THEN
            CPARM(2) = 50.0 * YINC
            MSGTXT = 'WARNING: CPARM(2) set to 5 cells'
            CALL MSGWRT (6)
            END IF
         END IF
C                                       Convert from pixels
      IF (ICCBOX.NE.3) THEN
         DO 10 J = 1,NCCBOX
            IF (CCBOX(1,J).EQ.-1.0) THEN
               CCBOX(1,J) = -600.
               CCBOX(2,J) = CCBOX(2,J) * ABS (CATR(KRCIC))
            ELSE
               CCBOX(1,J) = (CCBOX(1,J) - CATR(KRCRP)) * CATR(KRCIC)
               CCBOX(2,J) = (CCBOX(2,J) - CATR(KRCRP+1)) * CATR(KRCIC+1)
               END IF
            CCBOX(3,J) = (CCBOX(3,J) - CATR(KRCRP)) * CATR(KRCIC)
            CCBOX(4,J) = (CCBOX(4,J) - CATR(KRCRP+1)) * CATR(KRCIC+1)
 10         CONTINUE
C                                       Convert to degrees.
      ELSE
         DO 40 J = 1,NCCBOX
            IF (ABS(CCBOX(1,J)+32000.).GT.0.5) THEN
               CCBOX(1,J) = CCBOX(1,J) / 3600.0
            ELSE
               CCBOX(1,J) = -600.
               END IF
            CCBOX(2,J) = CCBOX(2,J) / 3600.0
            CCBOX(3,J) = CCBOX(3,J) / 3600.0
            CCBOX(4,J) = CCBOX(4,J) / 3600.0
 40         CONTINUE
         END IF
C                                       check ordering
C                                       do BLC/TRC in arc sec
      DO 50 J = 1,NCCBOX
         IF (CCBOX(1,J).NE.-600.0) THEN
            IF (CCBOX(1,J).GT.CCBOX(3,J)) THEN
               TEMP = CCBOX(1,J)
               CCBOX(1,J) = CCBOX(3,J)
               CCBOX(3,J) = TEMP
               END IF
            IF (CCBOX(2,J).GT.CCBOX(4,J))
     *         THEN
               TEMP = CCBOX(2,J)
               CCBOX(2,J) = CCBOX(4,J)
               CCBOX(4,J) = TEMP
               END IF
            END IF
 50      CONTINUE
C                                       Read table to internal arrays.
      NCC = 0
      WDWARN = (NCOL.LE.3)
      DO 100 IROW = BC,EC
         NCC = NCC + 1
         CALL CCTGET (INTAB, IROW, NCOL, CCD(XPOSI,NCC), CCD(YPOSI,NCC),
     *      CCD(FLUXI,NCC), W, IERR)
         IF (IERR.GT.0) GO TO 999
C                                       Flagged?
         IF (IERR.LT.0) THEN
            NCC = NCC - 1
         ELSE IF (NCC.EQ.1) THEN
            CALL RCOPY (4, W, W0)
         ELSE IF (.NOT.WDWARN) THEN
            IF ((ABS(W0(1)-W(1)).GT.XINC) .OR. (ABS(W0(2)-W(2)).GT.XINC)
     *         .OR. (ABS(W0(3)-W(3)).GT.0.1) .OR.
     *         (ABS(W0(4)-W(4)).GT.0.1)) THEN
               WDWARN = .TRUE.
               MSGTXT = 'COMPONENT SIZES ARE NOT ALL THE SAME:' //
     *            ' DATA ARE DESTROYED'
               CALL MSGWRT (8)
               END IF
            END IF
 100     CONTINUE
C                                       Close Input
      CALL OCLOSE (INTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Merge components.
C                                       Probably could use a utility
C                                       to do this - it would be
C                                       faster because of use of sort.
      XMIN = 1.E15
      XMAX = -1.E15
      YMIN = 1.E15
      YMAX = -1.E15
      FLXTOT = 0.0
      DO 250 I = 1, NCC
         IF (CCD(XPOSI,I).LT.XMIN) XMIN = CCD(XPOSI,I)
         IF (CCD(XPOSI,I).GT.XMAX) XMAX = CCD(XPOSI,I)
         IF (CCD(YPOSI,I).LT.YMIN) YMIN = CCD(YPOSI,I)
         IF (CCD(YPOSI,I).GT.YMAX) YMAX = CCD(YPOSI,I)
         FLXTOT = FLXTOT + CCD(FLUXI,I)
 250     CONTINUE
      CCD(XPOSO,1) = CCD(XPOSI,1)
      CCD(YPOSO,1) = CCD(YPOSI,1)
      CCD(FLUXO,1) = CCD(FLUXI,1)
      NMRG = 1
      DO 320 I = 2, NCC
         DUP = .FALSE.
         DO 300 J = 1, NMRG
            IF (ABS(CCD(XPOSI,I)-CCD(XPOSO,J)).LT.XINC .AND.
     *         ABS(CCD(YPOSI,I)-CCD(YPOSO,J)).LT.YINC) THEN
               CCD(FLUXO,J) = CCD(FLUXO,J) + CCD(FLUXI,I)
               DUP = .TRUE.
               END IF
 300        CONTINUE
         IF (.NOT.DUP) THEN
            NMRG = NMRG + 1
            CCD(XPOSO,NMRG) = CCD(XPOSI,I)
            CCD(YPOSO,NMRG) = CCD(YPOSI,I)
            CCD(FLUXO,NMRG) = CCD(FLUXI,I)
            END IF
 320     CONTINUE
      WRITE (MSGTXT,1400) NCC, NMRG
      CALL MSGWRT (4)
C                                       Filter.
C                                       Selected comps have OUTCC.NE.0.
C                                       Manual model splitting
      IF (CPARM(3).LE.0.0) THEN
C                                       Now find total flux within
C                                       CCRAD of each point.
         DO 360 I = 1,NMRG
            CCD(FLUXT,I) = 0.0
            IF (ROUND) THEN
               DO 340 J = 1,NMRG
                  IF ((CCD(XPOSO,I)-CCD(XPOSO,J))**2 +
     *               (CCD(YPOSO,I)-CCD(YPOSO,J))**2.LE.CPARM(2))
     *               CCD(FLUXT,I) = CCD(FLUXT,I) + CCD(FLUXO,J)
 340              CONTINUE
            ELSE
               DO 350 J = 1,NMRG
                  IF ((ABS(CCD(XPOSO,I)-CCD(XPOSO,J)).LE.CPARM(1)) .AND.
     *               ABS(CCD(YPOSO,I)-CCD(YPOSO,J)).LE.CPARM(2))
     *               CCD(FLUXT,I) = CCD(FLUXT,I) + CCD(FLUXO,J)
 350              CONTINUE
               END IF
 360        CONTINUE
         DO 420 I = 1,MAXTAB
            NOUTCC(I) = 0
            FLXTAB(I) = 0.0
            DO 410 J = 1,MAXTAB
               WARNED(I,J) = .FALSE.
 410           CONTINUE
 420        CONTINUE
C
         IF (NCCBOX.NE.0) THEN
            DO 450 I = 1,NMRG
               ICD(OUTCC,I) = 0
               IF (CCD(FLUXT,I).GE.CUTOFF) THEN
                  DO 430 J = 1,NCCBOX
                     IF (CCBOX(1,J).GT.-599.) THEN
                        INSIDE = (CCD(XPOSO,I).GE.CCBOX(1,J) .AND.
     *                     CCD(XPOSO,I).LE.CCBOX(3,J) .AND.
     *                     CCD(YPOSO,I).GE.CCBOX(2,J) .AND.
     *                     CCD(YPOSO,I).LE.CCBOX(4,J))
                     ELSE
                        R = SQRT ((CCD(XPOSO,I)-CCBOX(3,J))**2 +
     *                     (CCD(YPOSO,I)-CCBOX(4,J))**2)
                        INSIDE = R.LE.CCBOX(2,J)
                        END IF
                     IF (INSIDE) THEN
                        IF (NOTAB.EQ.1) THEN
                           ICD(OUTCC,I) = 1
                           NOUTCC(1) = NOUTCC(1) + 1
                           FLXTAB(1) = FLXTAB(1) + CCD(FLUXO,I)
                           GO TO 450
                        ELSE
                           IF (ICD(OUTCC,I).NE.0) THEN
                              IF (.NOT.WARNED(ICD(OUTCC,I),J)) THEN
                                 WRITE (MSGTXT,1430) ICD(OUTCC,I), J
                                 CALL MSGWRT (6)
                                 WARNED(ICD(OUTCC,I),J) = .TRUE.
                                 END IF
                           ELSE
                              ICD(OUTCC,I) = J
                              NOUTCC(J) = NOUTCC(J) + 1
                              FLXTAB(J) = FLXTAB(J) + CCD(FLUXO,I)
                              END IF
                           END IF
                        END IF
 430                 CONTINUE
                  END IF
 450           CONTINUE
         ELSE
            DO 455 I = 1,NMRG
               ICD(OUTCC,I) = 1
               NOUTCC(1) = NOUTCC(1) + 1
 455           CONTINUE
            END IF
C                                       What if none selected
         IF (NOTAB.EQ.1) THEN
            IF (NOUTCC(1).EQ.0) THEN
               WRITE (MSGTXT,1450)
               CALL MSGWRT (6)
               END IF
         ELSE
            DO 460 J = 1,NOTAB
               IF (NOUTCC(J).EQ.0) THEN
                  WRITE (MSGTXT,1460) J
                  CALL MSGWRT (6)
                  END IF
 460           CONTINUE
            END IF
C                                       Automatic model splitting
      ELSE
C                                       NCYCLS = # major cycles in
C                                       automatic model split
         NCYCLS = 1
         NOTAB = 0
         FLXOUT = 0.0
         DO 590 CYCLE = 1,NCYCLS
            ITER = 0
C                                       Split model into DX x DY
C                                       squares. One iteration
C                                       in a major cycle.
 500        ITER = ITER + 1
            NX = MAX (NINT ((XMAX-XMIN) / DX), 1)
            NY = MAX (NINT ((YMAX-YMIN) / DY), 1)
            IF (NX*NY.GT.MAXBOX) THEN
               TMPR = SQRT(REAL(NX*NY)/REAL(MAXBOX))
               NX = REAL(NX) / TMPR
               NY = REAL(NY) / TMPR
               DX = (XMAX-XMIN) / NX
               DY = (YMAX-YMIN) / NY
               END IF
C                                       Initialize for a new split
            NOTABC = 0
            FLXCYC = 0.0
            DO 510 J = 1,MAXBOX
               NOUTCC(J) = 0
               FLXBOX(J) = 0.0
               BOX(J) = J
 510           CONTINUE
C                                       Place unused comps into boxes
            JMAX = 1
            DO 520 I = 1,NMRG
C                                       Unused component?
               IF (ICD(OUTCC,I).LE.0) THEN
                  LY = (CCD(YPOSO,I)-YMIN)/DY - 0.0001
                  LX = (CCD(XPOSO,I)-XMIN)/DX - 0.0001
                  J = LY * NX + LX + 1
                  JMAX = MAX (J, JMAX)
                  ICD(OUTCC,I) = -J
                  FLXBOX(J) = FLXBOX(J) + CCD(FLUXO,I)
                  END IF
 520           CONTINUE
C                                       Find box with the highest flux
 530        TOPFLX = 0.0
            TOPBOX = 0
            DO 540 J = NOTABC+1,JMAX
               IF (FLXBOX(J).GT.TOPFLX) THEN
                  TOPFLX = FLXBOX(J)
                  TOPBOX = J
                  END IF
 540           CONTINUE
            IF (TOPBOX.EQ.0) THEN
C               MSGTXT = 'TOPBOX = 0: NO MORE CELLS > 0!!'
C               CALL MSGWRT (8)
               GO TO 545
               END IF
            NOTABC = NOTABC+1
C                                       Swap boxes
            IF (TOPBOX.NE.NOTABC) THEN
               TMPR = FLXBOX(NOTABC)
               FLXBOX(NOTABC) = FLXBOX(TOPBOX)
               FLXBOX(TOPBOX) = TMPR
               TMPI = BOX(NOTABC)
               BOX(NOTABC) = BOX(TOPBOX)
               BOX(TOPBOX) = TMPI
               END IF
C                                       See if enough flux accumulated
            FLXCYC = FLXCYC + FLXBOX(NOTABC)
            IF ((FLXOUT+FLXCYC)/FLXTOT.LT.CPARM(4)*CYCLE/NCYCLS)
     *         GO TO 530
C                                       Too many tables try bigger boxes
 545        IF (NOTAB+NOTABC.GT.NINT(CPARM(3)*CYCLE/NCYCLS)) THEN
               TEMP = SQRT (NOTABC*NCYCLS/CPARM(3)) * 1.05
               TEMP = MIN (1.1, TEMP)
               DX = DX * TEMP
               DY = DY * TEMP
               GO TO 500
               END IF
C                                       Skip next test if no convergence
            IF (ITER.LE.MAXITR) THEN
C                                       too much flux Try smaller boxes
               IF (FLXCYC/FLXTOT.GT.1.5*CPARM(4)/NCYCLS) THEN
                  TEMP = SQRT (CPARM(4)*FLXTOT/NCYCLS/FLXCYC) * 1.05
                  TEMP = MAX (0.9, TEMP)
                  DX = DX * TEMP
                  DY = DY * TEMP
                  GO TO 500
                  END IF
               END IF
C                                       Everything OK! Update box #'s
            DO 570 I = 1,NMRG
               DO 560 J = 1,NOTABC
                  IF (-ICD(OUTCC,I).EQ.BOX(J)) THEN
C                                       Component selected for output
C                                       -> positive final table #
                     ICD(OUTCC,I) = NOTAB + J
                     GO TO 570
                     END IF
 560              CONTINUE
               IF (ICD(OUTCC,I).LT.0) ICD(OUTCC,I) = 0
 570           CONTINUE
C                                       Store output table fluxes
            DO 580 J = 1,NOTABC
               FLXTAB(NOTAB+J) = FLXBOX(J)
               IY = (BOX(J) - 1) / NX + 1
               IX = BOX(J) - (IY-1)*NX
               CCBOX(1,NOTAB+J) = DX * (IX-1) + XMIN
               CCBOX(3,NOTAB+J) = CCBOX(1,NOTAB+J) + DX
               CCBOX(2,NOTAB+J) = DY * (IY-1) + YMIN
               CCBOX(4,NOTAB+J) = CCBOX(2,NOTAB+J) + DY
 580           CONTINUE
            NOTAB = NOTAB + NOTABC
            FLXOUT = FLXOUT + FLXCYC
 590        CONTINUE
         NCCBOX = NOTAB
         END IF
C                                       Writing the output
      FLXOUT = 0.0
      DO 650 I = 1,NOTAB
C                                       Create output table
C                                       This copies header stuff
C                                       including any keywords.
         CALL COPHED (INTAB, OUTTAB(I), IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Open output table
         CALL OOPEN (OUTTAB(I), 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Write selected components
         OROW = 0
         DO 610 IROW = 1,NMRG
            IF (ICD(OUTCC,IROW).EQ.I) THEN
               OROW = OROW + 1
               CALL CCTPUT (OUTTAB(I), OROW, NCOL, CCD(XPOSO,IROW),
     *            CCD(YPOSO,IROW), CCD(FLUXO,IROW), W, IERR)
               IF (IERR.GT.0) GO TO 999
               END IF
 610        CONTINUE
         FLXOUT = FLXOUT + FLXTAB(I)
C                                       Write message on output CC's
         CALL OGET (OUTTAB(I), 'VER', TYPE, DIM, OUTVER, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (NOTAB.EQ.1) THEN
            WRITE (MSGTXT,1500) OROW,OUTVER
         ELSE
            WRITE (MSGTXT,1520) OROW,I,OUTVER,FLXTAB(I)
            END IF
         CALL MSGWRT (4)
C                                       Update number of rows
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL OPUT (OUTTAB(I), 'NROW', OOAINT, DIM, OROW, CDUMMY, IERR)
         IF (IERR.NE.0.OR.OROW.EQ.0) GO TO 999
C                                       Close output
         CALL OCLOSE (OUTTAB(I), IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Sort
         CALL TBLSRT (OUTTAB(I), '-ABS:FLUX', '-ABS:FLUX', IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1470)
            CALL MSGWRT (8)
            GO TO 999
            END IF
C
 650     CONTINUE
      IF ((NCCBOX.EQ.0) .AND. (CPARM(3).LE.0.0)) FLXOUT = FLXTOT
C                                       Write summary line
      WRITE (MSGTXT,1570) FLXOUT, FLXTOT
      CALL MSGWRT (4)
C
 999  RETURN
C-----------------------------------------------------------------------
 1400 FORMAT ('Merged ', I6, ' components to make ', I6)
 1430 FORMAT ('CEDTAB: WARNING! WINDOWS ',I2, ' AND ',I2,' OVERLAP')
 1450 FORMAT ('CEDTAB WARNING: No components selected')
 1460 FORMAT ('CEDTAB WARNING: No components selected from window ',I2)
 1470 FORMAT ('CEDTAB: SORT PROBLEM')
 1500 FORMAT ('Wrote ', I6, ' filtered components to CC table ',I2)
 1520 FORMAT ('Wrote ', I5, ' comps from window ',I2,' to CC table ',
     *        I2,', ',F8.3,' Jy')
 1570 FORMAT ('Wrote ',F8.3,' Jy out of initial ',F8.3,' Jy')
      END
      SUBROUTINE CEDTHI (OUTTAB)
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
C
      INCLUDE 'BOXES.INC'
      INTEGER   IERR, I, J
      CHARACTER CTEMP*72, LINE*72
      REAL      TEMP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INVERS',
     *   'OUTVERS', 'BCOUNT', 'ECOUNT', 'CUTOFF', 'BOXFILE'/
C-----------------------------------------------------------------------
C                                       Add task label to history
      CALL OHTIME (OUTTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy adverb values.
      CALL OHLIST ('Input', LIST, NADV, OUTTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       boxes in asec
      LINE = 'CCBOX('
      DO 10 I = 1,NCCBOX
         CCBOX(2,I) = CCBOX(2,I) * 3600.0
         CCBOX(3,I) = CCBOX(3,I) * 3600.0
         CCBOX(4,I) = CCBOX(4,I) * 3600.0
C                                       circular
         IF (CCBOX(1,I).LT.-599.) THEN
            WRITE (CTEMP,1000) I, (CCBOX(J,I), J = 2,4)
C                                       rectangle
         ELSE
            CCBOX(1,I) = CCBOX(1,I) * 3600.0
            WRITE (CTEMP,1001) I, (CCBOX(J,I), J = 1,4)
            CCBOX(1,I) = CCBOX(1,I) / 3600.0
            END IF
         IF (I.LT.10) THEN
            J = 4
         ELSE IF (I.LT.100) THEN
            J = 3
         ELSE IF (I.LT.1000) THEN
            J = 2
         ELSE
            J = 1
            END IF
         LINE(7:) = CTEMP(J:)
         CALL OHWRIT (LINE, OUTTAB, IERR)
         IF (IERR.NE.0) GO TO 990
         CCBOX(2,I) = CCBOX(2,I) / 3600.0
         CCBOX(3,I) = CCBOX(3,I) / 3600.0
         CCBOX(4,I) = CCBOX(4,I) / 3600.0
 10      CONTINUE
C                                       boxes in asec
      LINE = 'CLBOX('
      DO 20 I = 1,NCCBOX
         CCBOX(3,I) = CCBOX(3,I) / CATR(KRCIC) + CATR(KRCRP)
         CCBOX(4,I) = CCBOX(4,I) / CATR(KRCIC+1) + CATR(KRCRP+1)
C                                       circular
         IF (CCBOX(1,I).LT.-599.) THEN
            CCBOX(2,I) = CCBOX(2,I) / ABS (CATR(KRCIC))
            WRITE (CTEMP,1010) I, (CCBOX(J,I), J = 2,4)
C                                       rectangle
         ELSE
            CCBOX(1,I) = CCBOX(1,I) / CATR(KRCIC) + CATR(KRCRP)
            CCBOX(2,I) = CCBOX(2,I) / CATR(KRCIC+1) + CATR(KRCRP+1)
            IF (CCBOX(1,I).GT.CCBOX(3,I)) THEN
               TEMP = CCBOX(1,I)
               CCBOX(1,I) = CCBOX(3,I)
               CCBOX(3,I) = TEMP
               END IF
            IF (CCBOX(2,I).GT.CCBOX(4,I)) THEN
               TEMP = CCBOX(2,I)
               CCBOX(2,I) = CCBOX(4,I)
               CCBOX(4,I) = TEMP
               END IF
            WRITE (CTEMP,1011) I, (CCBOX(J,I), J = 1,4)
            END IF
         IF (I.LT.10) THEN
            J = 4
         ELSE IF (I.LT.100) THEN
            J = 3
         ELSE IF (I.LT.1000) THEN
            J = 2
         ELSE
            J = 1
            END IF
         LINE(7:) = CTEMP(J:)
         CALL OHWRIT (LINE, OUTTAB, IERR)
         IF (IERR.NE.0) GO TO 990
 20      CONTINUE
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // OUTTAB
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I4,')=',F11.5,' at',F11.5,',',F11.5,' / asec circle')
 1001 FORMAT (I4,')=',3(F11.5,','),F11.5,' / asec')
 1010 FORMAT (I4,')=',F9.2,' at',F9.2,',',F9.2,' / pixels circle')
 1011 FORMAT (I4,')=',3(F9.2,','),F9.2,' / pixels')
      END
      SUBROUTINE CCTGET (NAME, ROW, NC, X, Y, FLUX, W, IERR)
C-----------------------------------------------------------------------
C   Get row from CC (CLEAN component) table object.
C   This assumes the structure of the CC table
C   Inputs:
C      NAME    C*?  CC table object name.
C      ROW     I    Row number
C      NC      I    Number of columns
C   Output:
C      X       R      X coordinate
C      Y       R      Y coordinate
C      FLUX    R      Component flux density.
C      W       R(4)   Width and pa, Type
C      IERR    I      Return error code, 0=>OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   ROW, NC, IERR
      REAL      X, Y, FLUX, W(4)
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
      IF (NC.GT.3) THEN
         IF (IERR.NE.0) GO TO 999
         CALL TABDGT (NAME, ROW, 4, TYPE, DIM, W(1), CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDGT (NAME, ROW, 5, TYPE, DIM, W(2), CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDGT (NAME, ROW, 6, TYPE, DIM, W(3), CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDGT (NAME, ROW, 7, TYPE, DIM, W(4), CDUMMY, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE CCTPUT (NAME, ROW, NC, X, Y, FLUX, W, IERR)
C-----------------------------------------------------------------------
C   Write row to CC (CLEAN component) table object.
C   This assumes the structure of the CC table
C   Inputs:
C      NAME    C*?    CC table object name.
C      ROW     I      Row number
C      NC      I      Number columns out
C      X       R      X coordinate
C      Y       R      Y coordinate
C      W       R(3)   widths, pa, type
C   Output:
C      IERR    I      Return error code, 0=>OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   ROW, NC, IERR
      REAL      X, Y, FLUX, W(4)
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
C                                       All widths are zero
      IF (NC.GT.3) THEN
         IF (IERR.NE.0) GO TO 999
         CALL TABDPT (NAME, ROW, 4, OOARE, DIM, W(1), CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDPT (NAME, ROW, 5, OOARE, DIM, W(2), CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDPT (NAME, ROW, 6, OOARE, DIM, W(3), CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDPT (NAME, ROW, 7, OOARE, DIM, W(4), CDUMMY, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE WINDF (NFIELD, BOXFIL, NBOXES, WIN, IERR)
C-----------------------------------------------------------------------
C   Fills the WIN array with clean box definitions taken from BOXFIL
C   Inputs:
C      BOXFIL   C*48        User provided file name containing box defs
C      NFIELD   I           Number of desired field
C   Outputs:
C      WIN      R(4,*,*)    clean boxes - defaulted on in (4,FIELD,BOX)
C      NBOXES   I*(*)       Array containing number of boxes/field
C      IERR     I           Error return code:
C                              0 => no error
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLN.INC'
      CHARACTER BOXFIL*48
      INTEGER   NFIELD, NBOXES, IERR
      REAL      WIN(4,*)
C
      INTEGER   LUN, I, J, IFIELD, FIND, KBP, IDD, LIMIT, JT, JTRIM
      CHARACTER LINE*132
      DOUBLE PRECISION X
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      LIMIT = MXNBOX
C                                        Open clean box file
      LUN = 11
      CALL ZTXOPN ('READ', LUN, FIND, BOXFIL, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                        Enter box parameters from file
      IDD = 0
      NBOXES = 0
      DO 50 I = 1,100000
         CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
         IF (IERR.EQ.2) GO TO 60
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN'
            CALL MSGWRT (6)
            GO TO 999
            END IF
         JT = JTRIM (LINE)
C                                       check for comments
         IF (LINE.EQ.' ') GO TO 50
         IF (LINE(:1).NE.' ') THEN
            IF (LINE(:1).LT.'0') GO TO 50
            IF (LINE(:1).GT.'9') GO TO 50
            END IF
C                                       parse for 5 numbers
C                                       field, blc, trc
         KBP = 1
         DO 30 J = 1,5
            CALL GETNUM (LINE, 132, KBP, X)
            IF (X.EQ.DBLANK) THEN
               IF (J.EQ.1) GO TO 50
               WRITE (MSGTXT,1020) I, J
               CALL MSGWRT (6)
               IERR = 1
               GO TO 999
            ELSE IF (J.EQ.1) THEN
               IFIELD = X + 0.50D0
               IF (IFIELD.NE.NFIELD) GO TO 50
               NBOXES = NBOXES + 1
            ELSE
               WIN(J-1,NBOXES) = X
               END IF
 30         CONTINUE
 50      CONTINUE
C
 60   CALL ZTXCLS (LUN, FIND, I)
      IERR = 0
      WRITE (MSGTXT,1050) NFIELD, NBOXES
      CALL MSGWRT (3)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WINDF: ERROR',I4,1X,A,'ING THE CLEAN BOXES TEXT FILE')
 1020 FORMAT ('WINDF: PARSING ERROR ON LINE',I4,' FIELD',I2)
 1050 FORMAT ('WINDF: Field',I4.2,':Nboxes',I5)
      END
