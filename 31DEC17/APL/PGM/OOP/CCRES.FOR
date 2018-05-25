LOCAL INCLUDE 'INPUT.INC'
      INCLUDE 'INCS:PCLN.INC'
C                                       Declarations for inputs
      INTEGER   NPARMS
C                                       NPARMS=no. adverbs passed.
      PARAMETER (NPARMS=22)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
      INCLUDE 'INCS:PAOOF.INC'
C                                       Adverb names
C                     1         2          3        4         5      6
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'BLC', 'TRC',
C           7          8           9         10         11
     *   'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK', 'INVERS',
C           12         13          14        15         16        17
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK', 'OPTYPE', 'BCOMP',
C           18      19      20      21     22
     *   'ECOMP','BMAJ', 'BMIN', 'BPA', 'FACTOR'/
C                                       Adverb data types (PAOOF.INC)
C                     1       2       3       4       5       6
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOAINT, OOAINT,
C           7       8       9       10      11      12      13      14
     *   OOACAR, OOACAR, OOAINT, OOAINT, OOAINT, OOACAR, OOACAR, OOAINT,
C           15      16      17      18      19     20     21     22
     *   OOAINT, OOACAR, OOAINT, OOAINT, OOARE, OOARE, OOARE, OOARE /
C                                       Adverb dimensions (as 2D)
C                   1    2    3    4    5    6    7    8    9     10
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 7,1, 7,1, 12,1, 6,1, 1,1, 1,1,
C         11    12   13   14   15   16   17   18   19   20   21   22
     *   1,1, 12,1, 6,1, 1,1, 1,1, 4,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1 /
LOCAL END
      PROGRAM CCRES
C-----------------------------------------------------------------------
C! Remove and/or restore Clean components
C# Utility OOP Imaging
C-----------------------------------------------------------------------
C;  Copyright (C) 2005, 2007-2010, 2012-2013, 2015, 2017
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
      CHARACTER PRGM*6, IMGIN*32, IMG2IN*32, IMGOUT*32, INTAB*32,
     *   OPTYPE*4
      INTEGER   IRET, BUFF1(256), NCCS, NWORDS, NC
      LONGINT   IOFF
      REAL      CCDATA(2)
      DATA PRGM /'CCRES '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL CRESIN (PRGM, IMGIN, IMG2IN, IMGOUT, INTAB, OPTYPE, NCCS,
     *   IRET)
C                                       Process table
      IF (IRET.EQ.0) THEN
         NC = 3
         IF (OPTYPE(2:4).EQ.'GAU') NC = 6
         NWORDS = (NCCS * NC - 1) / 1024 + 1
         CALL ZMEMRY ('GET', 'CCRES', NWORDS, CCDATA, IOFF, IRET)
         END IF
      IF (IRET.EQ.0) CALL CRESDO (IMGIN, IMGOUT, INTAB, NCCS, NC,
     *   CCDATA(1+IOFF), IRET)
C                                       History
      IF (IRET.EQ.0) CALL CRESHI (IMGIN, INTAB, IMGOUT, NCCS)
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE CRESIN (PRGN, IMGIN, IMG2IN, IMGOUT, INTAB, OPTYPE,
     *   NCCS, IRET)
C-----------------------------------------------------------------------
C   CRESIN gets input parameters for CCRES and creates the input and
C   output objects
C   Inputs:
C      PRGN     C*6   Program name
C   Output:
C      IMGIN    C     Name of input image
C      IMG2IN   C     Name of image with CC file
C      IMGOUT   C     Name of output image
C      INTAB    C     Name of the input table object
C      OUTTAB   C     Names of the output table objects
C      OPTYPE   C*4   Operation type
C      NCCS     I     Number rows in CC to actually be used
C      IRET     I     Error code: 0 => ok
C                               4 => user routine detected error.
C                               5 => catalog troubles
C                               8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS file
C-----------------------------------------------------------------------
      CHARACTER PRGN*6, IMGIN*(*), IMG2IN*(*), IMGOUT*(*), INTAB*(*),
     *   OPTYPE*4
      INTEGER   NCCS, IRET
C
      INCLUDE 'INPUT.INC'
      INTEGER   NKEY1, NKEY2, NKEY3
      PARAMETER (NKEY1=10)
      PARAMETER (NKEY2=4)
      PARAMETER (NKEY3=7)
C
      INTEGER   DIM(3), DUMMY, TYPE, BC, EC, P1, P2, BLC(7), TRC(7),
     *   NROWS, INS, IND
      REAL      B1(3), B2(3), B(3), RFACT, FACTOR
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, INK3(NKEY3)*8, OUTK3(NKEY3)*32,
     *   CDUMMY*1, INN*12, INC*6
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to IMGIN
C                   1         2          3        4         5      6
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'BLC', 'TRC',
C           7          8           9         10
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK'/
C                                       May rename adverbs to IMGIN
C                    1       2        3        4       5      6
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'BLC', 'TRC',
C           7          8           9         10
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK'/
C                                       Adverbs to copy to IMG2IN
C                   1          2           3         4
      DATA INK2 /'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK'/
C                                       May rename adverbs to IMG2IN
C                    1       2        3        4
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK'/
C                                       Adverbs to copy to INTAB
C                   1          2           3         4
      DATA INK3 /'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK',
C            5         6        7
     *    'INVERS', 'BCOMP', 'ECOMP'/
C                                       May rename adverbs to INTAB
C                    1       2        3        4       5      6
      DATA OUTK3 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'VER', 'BCOMP',
C           7
     *   'ECOMP'/
C-----------------------------------------------------------------------
C                                       Startup,  returns "Input" object
C                                       containing POPS adverbs
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create input image object
      IMGIN = 'Input image'
      CALL CREATE (IMGIN, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, IMGIN, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       get beam from header
      CALL IMGOPN (IMGIN, 'READ', IRET)
      IF (IRET.NE.0) GO TO 995
      CALL IMGET (IMGIN, 'BEAM.PRODUCT', TYPE, DIM, P1, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 995
      CALL IMGET (IMGIN, 'BEAM.BMAJ', TYPE, DIM, B1(1), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 995
      CALL IMGET (IMGIN, 'BEAM.BMIN', TYPE, DIM, B1(2), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 995
      CALL IMGET (IMGIN, 'BEAM.BPA', TYPE, DIM, B1(3), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 995
      CALL IMGET (IMGIN, 'BLC', TYPE, DIM, BLC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 995
      CALL IMGET (IMGIN, 'TRC', TYPE, DIM, TRC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       image header
      CALL OBHGET (IMGIN, CATBLK, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IMGCLO (IMGIN, IRET)
      IF (IRET.NE.0) GO TO 995
      CALL COPY (5, BLC(3), TRC(3))
      CALL IMPUT (IMGIN, 'TRC', TYPE, DIM, TRC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       default output class
      CALL OGET ('Input', 'OUTCLASS', TYPE, DIM, DUMMY, INC, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (INC.EQ.' ') THEN
         INC = TSKNAM
         CALL OPUT (IMGIN, 'OUTCLASS', TYPE, DIM, DUMMY, INC, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL OPUT ('Input', 'OUTCLASS', TYPE, DIM, DUMMY, INC, IRET)
         IF (IRET.NE.0) GO TO 995
         END IF
C                                       Defaults for 2nd image
      CALL OGET ('Input', 'IN2NAME', TYPE, DIM, DUMMY, INN, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (INN.EQ.' ') THEN
         CALL OGET (IMGIN, 'NAME', TYPE, DIM, DUMMY, INN, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT ('Input', 'IN2NAME', TYPE, DIM, DUMMY, INN, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      CALL OGET ('Input', 'IN2CLASS', TYPE, DIM, DUMMY, INC, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (INC.EQ.' ') THEN
         CALL OGET (IMGIN, 'CLASS', TYPE, DIM, DUMMY, INC, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT ('Input', 'IN2CLASS', TYPE, DIM, DUMMY, INC, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      CALL OGET ('Input', 'IN2SEQ', TYPE, DIM, INS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (INS.LE.0) THEN
         CALL OGET (IMGIN, 'IMSEQ', TYPE, DIM, INS, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT ('Input', 'IN2SEQ', TYPE, DIM, INS, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      CALL OGET ('Input', 'IN2DISK', TYPE, DIM, IND, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (IND.LE.0) THEN
         CALL OGET (IMGIN, 'DISK', TYPE, DIM, IND, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT ('Input', 'IN2DISK', TYPE, DIM, IND, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Create input CC image object
      IMG2IN = 'Input CC image'
      CALL CREATE (IMG2IN, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, IMG2IN, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       get beam from header
      CALL IMGOPN (IMG2IN, 'READ', IRET)
      IF (IRET.NE.0) GO TO 990
      CALL IMGET (IMG2IN, 'BEAM.PRODUCT', TYPE, DIM, P2, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL IMGET (IMG2IN, 'BEAM.BMAJ', TYPE, DIM, B2(1), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL IMGET (IMG2IN, 'BEAM.BMIN', TYPE, DIM, B2(2), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL IMGET (IMG2IN, 'BEAM.BPA', TYPE, DIM, B2(3), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL IMGCLO (IMG2IN, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Create output object
      IMGOUT = 'Output image'
      CALL IMGCLN (IMGIN, IMGOUT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create input table object
      INTAB = 'Input table'
      CALL CREATE (INTAB, 'TABLE', IRET)
      IF (IRET.NE.0) GO TO 985
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY3, INK3, OUTK3, INTAB, IRET)
      IF (IRET.NE.0) GO TO 985
C                                       Table type
      DIM(1) = 2
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (INTAB, 'TBLTYPE', OOACAR, DIM, DUMMY, 'CC', IRET)
      IF (IRET.NE.0) GO TO 985
C                                       Open input table
      CALL OOPEN (INTAB, 'READ', IRET)
      IF (IRET.NE.0) GO TO 985
C                                       Get number of entries
      CALL OGET (INTAB, 'NROW', TYPE, DIM, NROWS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 985
      CALL OCLOSE (INTAB, IRET)
      IF (IRET.NE.0) GO TO 985
C                                       User adverbs - range CCs
      CALL OGET ('Input', 'FACTOR', TYPE, DIM, FACTOR, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'BCOMP', TYPE, DIM, BC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'ECOMP', TYPE, DIM, EC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF ((BC.LT.1) .OR. (BC.GT.NROWS)) BC = 1
      IF ((EC.LT.BC) .OR. (EC.GT.NROWS)) EC = NROWS
      NCCS = EC - BC + 1
      CALL OPUT ('Input', 'BCOMP', TYPE, DIM, BC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT ('Input', 'ECOMP', TYPE, DIM, EC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (INTAB, 'BCOMP', TYPE, DIM, BC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (INTAB, 'ECOMP', TYPE, DIM, EC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       User adverbs - beam, type
      CALL OGET ('Input', 'OPTYPE', TYPE, DIM, DUMMY, OPTYPE, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'BMAJ', TYPE, DIM, B(1), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      B(1) = B(1) / 3600.0
      CALL OGET ('Input', 'BMIN', TYPE, DIM, B(2), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      B(2) = B(2) / 3600.0
      CALL OGET ('Input', 'BPA', TYPE, DIM, B(3), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       test and set defaults
      IF ((OPTYPE.NE.'SUB') .AND. (OPTYPE.NE.'ADD') .AND.
     *   (OPTYPE.NE.'S+A') .AND. (OPTYPE.NE.'SGAU') .AND.
     *   (OPTYPE.NE.'AGAU') .AND. (OPTYPE.NE.'ADDP') .AND.
     *   (OPTYPE.NE.'S+AP')) THEN
         IF (P1.EQ.1) OPTYPE = 'SUB '
         IF (P1.EQ.3) OPTYPE = 'ADD '
         END IF
      IF ((OPTYPE.EQ.'SUB ') .OR. (OPTYPE.EQ.'ADD ')) THEN
         IF ((B(1).LE.0.0) .AND. (B(2).LE.0.0)) THEN
            B(1) = B2(1)
            B(2) = B2(2)
            B(3) = B2(3)
            END IF
         IF (B(1).LE.0.0) B(1) = B2(1)
         IF (B(2).LE.0.0) B(2) = B2(2)
      ELSE IF ((OPTYPE.EQ.'SGAU') .OR. (OPTYPE.EQ.'AGAU') .OR.
     *      (OPTYPE.EQ.'ADDP') .OR. (OPTYPE.EQ.'S+AP')) THEN
         CALL RFILL (3, 0.0, B)
      ELSE IF (OPTYPE.EQ.'S+A') THEN
         IF ((B(1).LE.0.0) .OR. (B(2).LE.0.0)) THEN
            MSGTXT = 'BEAM VALUES TO ADD MUST BE SPECIFIED WITH ''S+A'''
            CALL MSGWRT (8)
            IRET = 8
            GO TO 999
            END IF
      ELSE
         MSGTXT = 'OPTYPE=''' // OPTYPE // ' NOT RECOGNIZED'
         CALL MSGWRT (8)
         IRET = 8
         GO TO 999
         END IF
C                                       residual factor
      RFACT = 1.0
      IF (FACTOR.EQ.0.0) FACTOR = 1.0
      IF (FACTOR.GT.0.0) THEN
         IF (B1(1)*B1(2).NE.0.0) THEN
            RFACT = ABS (B(1)*B(2)) / ABS (B1(1)*B1(2))
            IF (RFACT.EQ.0.0) RFACT = ABS (CATR(KRCIC) * CATR(KRCIC+1))
     *         / (1.1331 * ABS (B1(1) * B1(2)))
            END IF
         RFACT = RFACT * FACTOR
         END IF
      TYPE = OOARE
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (INTAB, 'RFACTOR', TYPE, DIM, RFACT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT ('Input', 'FACTOR', TYPE, DIM, FACTOR, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT ('Input', 'BMAJ', TYPE, DIM, B(1), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT ('Input', 'BMIN', TYPE, DIM, B(2), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT ('Input', 'BPA', TYPE, DIM, B(3), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      TYPE = OOACAR
      DIM(1) = 4
      CALL OPUT ('Input', 'OPTYPE', TYPE, DIM, DUMMY, OPTYPE, IRET)
      GO TO 999
C                                       errors
 985  MSGTXT = 'ERROR WITH ' // INTAB
      CALL MSGWRT (8)
      GO TO 999
 990  MSGTXT = 'ERROR WITH ' // IMG2IN
      CALL MSGWRT (8)
      GO TO 999
 995  MSGTXT = 'ERROR WITH ' // IMGIN
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE CRESDO (IMGIN, IMGOUT, INTAB, NCCS, NC, CCD, IERR)
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
      CHARACTER IMGIN*(*), IMGOUT*(*), INTAB*(*)
      INTEGER   NCCS, NC, IERR
      REAL      CCD(NC,*)
C
      INTEGER   XPOSI, YPOSI, FLUXI, WIDI
      PARAMETER (XPOSI=1, YPOSI=2, FLUXI=3, WIDI=4)
      INCLUDE 'INCS:PMAD.INC'
C
      INTEGER   IROW, NROW, BC, EC, TYPE, DIM(7), NCC, I, NCNV, DUMMY,
     *   NCOL, IY, IX, BLC(7), TRC(7), IY1, IY2, IX1, IX2, NAXIS(7),
     *   NNY, NNX
      REAL      ROW(MABFSS), BMAJ, BMIN, BPA, X, Y, D, BMAJ2, BMIN2,
     *   BPA2, AA, BB, CC, AA2, BB2, CC2, FACT, XYI(7), XYO(7),
     *   CELLSG(2), CRP(2), MAPR, RFACT, DSUM, DSUM2
      LOGICAL   POINT
      CHARACTER CDUMMY*1, OPTYPE*4
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA XYI, XYO /7*1.0, 7*1.0/
C-----------------------------------------------------------------------
      I = NC * NCCS
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
      CALL OGET (INTAB, 'BCOMP', TYPE, DIM, BC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'ECOMP', TYPE, DIM, EC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'RFACTOR', TYPE, DIM, RFACT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       image header
      CALL OBHGET (INTAB, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Clean beam from header
      BMAJ2 = CATR(KRBMJ)
      BMIN2 = CATR(KRBMN)
      BPA2 = CATR(KRBPA)
      CRP(1) = CATR(KRCRP)
      CRP(2) = CATR(KRCRP+1)
      CELLSG(1) = CATR(KRCIC)
      CELLSG(2) = CATR(KRCIC+1)
C                                       computing beam, type
      CALL OGET ('Input', 'BMAJ', TYPE, DIM, BMAJ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'BMIN', TYPE, DIM, BMIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'BPA', TYPE, DIM, BPA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'OPTYPE', TYPE, DIM, DUMMY, OPTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
      POINT = OPTYPE(4:4).EQ.'P'
      FACT = 1.0
      IF (OPTYPE(1:1).EQ.'S') FACT = -1.0
      IF (OPTYPE(:3).EQ.'S+A') FACT = 1.0
C                                       read in CC file
      NCC = 0
      DO 10 IROW = BC,EC
         NCC = NCC + 1
         CALL CCTGET (INTAB, IROW, NCOL, CCD(XPOSI,NCC), CCD(YPOSI,NCC),
     *      CCD(FLUXI,NCC), CCD(WIDI,NCC), IERR)
         IF (IERR.GT.0) GO TO 999
C                                       Flagged?
         IF (IERR.LT.0) THEN
            NCC = NCC - 1
C                                       convert to output pixel
         ELSE
            XYI(1) = CCD(XPOSI,NCC) / CELLSG(1) + CRP(1)
            XYI(2) = CCD(YPOSI,NCC) / CELLSG(2) + CRP(2)
            CALL PSNCVT (INTAB, XYI, IMGOUT, XYO, IERR)
            IF (IERR.NE.0) THEN
                NCC = NCC - 1
            ELSE
               CCD(XPOSI,NCC) = XYO(1)
               CCD(YPOSI,NCC) = XYO(2)
               END IF
            END IF
 10      CONTINUE
C                                       Close Input CC
      CALL OCLOSE (INTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       open images
      CALL IMGOPN (IMGIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGOPN (IMGOUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ARROPN (IMGIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ARROPN (IMGOUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBHGET (IMGOUT, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      MAPR = CATR(KRCRT+1)
      CELLSG(1) = CATR(KRCIC)
      CELLSG(2) = CATR(KRCIC+1)
      CALL ARRWIN (IMGIN, BLC, TRC, NAXIS, IERR)
      IF (IERR.NE.0) GO TO 999
      NNX = TRC(1) - BLC(1) + 1
      NNY = TRC(2) - BLC(2) + 1
C                                       Gaus parameters
      IF (OPTYPE(2:4).NE.'GAU') THEN
         IF (.NOT.POINT) CALL CBPARS (BMAJ, BMIN, BPA, CELLSG, MAPR, AA,
     *      BB, CC, NCNV)
         IF (OPTYPE(:3).EQ.'S+A') THEN
            CALL CBPARS (BMAJ2, BMIN2, BPA2, CELLSG, MAPR, AA2, BB2,
     *         CC2, IROW)
            NCNV = MAX (NCNV, IROW)
            END IF
C                                       what does Gaussian add up to?
         DSUM = 0.0
         DSUM2 = 0.0
         X = NCNV+1
         Y = NCNV+1
         IX1 = X - NCNV
         IX2 = X + NCNV
         IY1 = Y - NCNV
         IY2 = Y + NCNV
         DO 30 IY = IY1,IY2
            DO 20 IX = IX1,IX2
               IF (.NOT.POINT) THEN
                  D = (IX-X)*(IX-X)*AA + (IY-Y)*(IY-Y)*BB +
     *               (IX-X)*(IY-Y)*CC
                  DSUM = DSUM + EXP(-D)
                  END IF
               IF (OPTYPE(:3).EQ.'S+A') THEN
                  D = (IX-X)*(IX-X)*AA2 + (IY-Y)*(IY-Y)*BB2 +
     *               (IX-X)*(IY-Y)*CC2
                  DSUM2 = DSUM2 + EXP (-D)
                  END IF
 20            CONTINUE
 30         CONTINUE
         IF (POINT) THEN
            DSUM = 1.0
         ELSE
            DSUM = 1.1331 * BMAJ * BMIN / (DSUM * CELLSG(1)*CELLSG(2))
            END IF
         DSUM2 = 1.1331 * BMAJ2 * BMIN2 / (DSUM2 * CELLSG(1)*CELLSG(2))
         DSUM = ABS (DSUM)
         DSUM2 = ABS (DSUM2)
      ELSE
         DSUM = 1.0
         END IF
      FACT = FACT * DSUM
C                                       loop through image
      DO 70 IY = 1,NNY
         CALL ARREAD (IMGIN, DIM, ROW, IERR)
         IF (IERR.NE.0) GO TO 995
         IY1 = IY - NCNV
         IY2 = IY + NCNV
C                                       subtract to make residual
         IF ((OPTYPE.EQ.'SUB') .OR. (OPTYPE(:3).EQ.'S+A') .OR.
     *      (OPTYPE.EQ.'SGAU')) THEN
C                                       loop through comps
            DO 40 I = 1,NCC
               IF (OPTYPE(2:4).EQ.'GAU') THEN
                  CALL CBPARS (CCD(4,I), CCD(5,I), CCD(6,I), CELLSG,
     *               MAPR,AA, BB, CC, NCNV)
                  IY1 = IY - NCNV
                  IY2 = IY + NCNV
                  END IF
               IF ((CCD(YPOSI,I).GE.IY1) .AND. (CCD(YPOSI,I).LE.IY2))
     *            THEN
                  X = CCD(XPOSI,I)
                  Y = CCD(YPOSI,I)
                  IX1 = X - NCNV
                  IX2 = X + NCNV
                  IX1 = MAX (1, IX1)
                  IX2 = MIN (NNX, IX2)
                  DO 35 IX = IX1,IX2
                     IF (ROW(IX).NE.FBLANK) THEN
                        IF (OPTYPE(:3).EQ.'S+A') THEN
                           D = (IX-X)*(IX-X)*AA2 + (IY-Y)*(IY-Y)*BB2 +
     *                        (IX-X)*(IY-Y)*CC2
                           ROW(IX) = ROW(IX) - DSUM2 * CCD(FLUXI,I) *
     *                        EXP (-D)
                        ELSE
                           D = (IX-X)*(IX-X)*AA + (IY-Y)*(IY-Y)*BB +
     *                        (IX-X)*(IY-Y)*CC
                           ROW(IX) = ROW(IX) + FACT * CCD(FLUXI,I) *
     *                        EXP (-D)
                           END IF
                        END IF
 35                  CONTINUE
                  END IF
 40            CONTINUE
            END IF
C                                       scale residual
         IF (RFACT.NE.1.0) THEN
            DO 50 IX = 1,NNX
               IF (ROW(IX).NE.FBLANK) ROW(IX) = ROW(IX) * RFACT
 50            CONTINUE
            END IF
C                                       add to make image
         IF ((OPTYPE(:3).EQ.'ADD') .OR. (OPTYPE(:3).EQ.'S+A') .OR.
     *      (OPTYPE.EQ.'AGAU')) THEN
C                                       loop through comps
            DO 60 I = 1,NCC
               IF (OPTYPE(2:4).EQ.'GAU') THEN
                  CALL CBPARS (CCD(4,I), CCD(5,I), CCD(6,I), CELLSG,
     *               MAPR,AA, BB, CC, NCNV)
                  IY1 = IY - NCNV
                  IY2 = IY + NCNV
                  END IF
               IF ((CCD(YPOSI,I).GE.IY1) .AND. (CCD(YPOSI,I).LE.IY2))
     *            THEN
                  X = CCD(XPOSI,I)
                  Y = CCD(YPOSI,I)
                  IX1 = X - NCNV
                  IX2 = X + NCNV
                  IX1 = MAX (1, IX1)
                  IX2 = MIN (NNX, IX2)
                  DO 55 IX = IX1,IX2
                     IF (ROW(IX).NE.FBLANK) THEN
                        IF (POINT) THEN
                           IF ((ABS(IX-X).LT.0.5) .AND.
     *                        (ABS(IY-Y).LT.0.5)) ROW(IX) = ROW(IX) +
     *                        FACT * CCD(FLUXI,I)
                        ELSE
                           D = (IX-X)*(IX-X)*AA + (IY-Y)*(IY-Y)*BB +
     *                        (IX-X)*(IY-Y)*CC
                           ROW(IX) = ROW(IX) + FACT * CCD(FLUXI,I) *
     *                        EXP (-D)
                           END IF
                        END IF
 55                  CONTINUE
                  END IF
 60            CONTINUE
            END IF
         CALL ARRWRI (IMGOUT, DIM, ROW, IERR)
         IF (IERR.NE.0) GO TO 995
 70      CONTINUE
      CALL ARRCLO (IMGIN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ARRCLO (IMGOUT, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGCLO (IMGIN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGCLO (IMGOUT, IERR)
      IF (IERR.NE.0) GO TO 999
      GO TO 999
C
 995  WRITE (MSGTXT,1995) IERR, IY
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1995 FORMAT ('ERROR',I5,' AT ROW',I6)
      END
      SUBROUTINE CRESHI (IMGIN, INTAB, IMGOUT, NCCS)
C-----------------------------------------------------------------------
C   Routine to write history file to output table object.  This assumes
C   that a previous history exists and merely adds the information from
C   the current task.
C   Inputs:
C      IMGIN    C*?   Input image
C      INTAB    C*?   Input table object
C      IMGOUT   C*?   Output table object
C      NCCS     I     Number clean comps
C-----------------------------------------------------------------------
      CHARACTER IMGIN*(*), INTAB*(*), IMGOUT*(*)
      INTEGER   NCCS
C
      INTEGER   NADV
      PARAMETER (NADV=19)
      CHARACTER LIST(NADV)*8
C
      INTEGER   IERR, I, TYPE, DIM(7), DUMMY, P1, CCVER
      CHARACTER LINE*72, OPTYPE*4, FUNC(2)*10, CDUMMY*1, OUTTAB*32,
     *   CUNIT*8
      REAL      BMAJ, BMIN, BPA, BMAJH, BMINH, BPAH, RFACT, FACTOR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PAOOF.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'BLC', 'TRC',
     *   'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK', 'INVERS',
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK', 'OPTYPE', 'BCOMP',
     *   'ECOMP', 'FACTOR'/
      DATA FUNC /'added','subtracted'/
C-----------------------------------------------------------------------
C                                       Add task label to history
      CALL OHCOPY (IMGIN, IMGOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy adverb values.
      CALL OHLIST ('Input', LIST, NADV, IMGOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       units conversion
      CALL OGET ('Input', 'BMAJ', TYPE, DIM, BMAJ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'BMIN', TYPE, DIM, BMIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'BPA', TYPE, DIM, BPA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'OPTYPE', TYPE, DIM, DUMMY, OPTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'FACTOR', TYPE, DIM, FACTOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'RFACTOR', TYPE, DIM, RFACT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (FACTOR.GT.0.0) RFACT = RFACT / FACTOR
      IF (RFACT.NE.1.0) THEN
         WRITE (LINE,1010) RFACT
         CALL OHWRIT (LINE, IMGOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      I = 1
      IF (OPTYPE.EQ.'SUB') I = 2
      BMAJ = BMAJ * 3600.0
      BMIN = BMIN * 3600.0
      IF (OPTYPE(2:4).NE.'GAU') THEN
         WRITE (LINE,1000) 'BMAJ    ', BMAJ, FUNC(I)
         CALL OHWRIT (LINE, IMGOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         WRITE (LINE,1000) 'BMIN    ', BMIN, FUNC(I)
         CALL OHWRIT (LINE, IMGOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         WRITE (LINE,1001) 'BPA     ', BPA, FUNC(I)
         CALL OHWRIT (LINE, IMGOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       image CB needed
      IF (OPTYPE.EQ.'S+A') THEN
C                                       Open input table
         CALL OOPEN (INTAB, 'READ', IERR)
         IF (IERR.NE.0) GO TO 999
C                                       image header
         CALL OBHGET (INTAB, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Clean beam from header
         BMAJH = CATR(KRBMJ) * 3600.0
         BMINH = CATR(KRBMN) * 3600.0
         BPAH = CATR(KRBPA)
         I = 2
         WRITE (LINE,1000) 'BMAJH   ', BMAJH, FUNC(I)
         CALL OHWRIT (LINE, IMGOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         WRITE (LINE,1000) 'BMINH   ', BMINH, FUNC(I)
         CALL OHWRIT (LINE, IMGOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         WRITE (LINE,1001) 'BPAH    ', BPAH, FUNC(I)
         CALL OHWRIT (LINE, IMGOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       fix up header
      CALL IMGOPN (IMGOUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 999
      P1 = 1
      IF ((OPTYPE.EQ.'SGAU') .OR. (OPTYPE.EQ.'SUB ')) P1 = 3
      IF ((OPTYPE.EQ.'ADDP') .OR. (OPTYPE.EQ.'S+AP')) P1 = 4
      DIM(1) = 1
      DIM(2) = 1
      CALL IMPUT (IMGOUT, 'BEAM.PRODUCT', OOAINT, DIM, P1, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMPUT (IMGOUT, 'BEAM.NITER', OOAINT, DIM, NCCS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      BMAJ = BMAJ / 3600.
      BMIN = BMIN / 3600.
      CALL IMPUT (IMGOUT, 'BEAM.BMAJ', OOARE, DIM, BMAJ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMPUT (IMGOUT, 'BEAM.BMIN', OOARE, DIM, BMIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMPUT (IMGOUT, 'BEAM.BPA', OOARE, DIM, BPA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (OPTYPE(4:4).EQ.'P') THEN
         CUNIT = 'JY/PIXEL'
         DIM(1) = 8
         CALL IMPUT (IMGOUT, 'BUNIT', OOACAR, DIM, BPA, CUNIT, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
      CALL IMGCLO (IMGOUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       copy rest tables
      DIM(1) = 2
      CUNIT = 'CC'
      CALL IMPUT (IMGIN, 'DROPTABS', OOACAR, DIM, BPA, CUNIT, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMCALT (IMGIN, IMGOUT, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE COPYING OTHER TABLES'
         CALL MSGWRT (6)
         END IF
      IERR = 0
C                                       copy CC table
      OUTTAB = 'Output table'
      CCVER = 1
      CALL IM2TAB (IMGOUT, OUTTAB, 'CC', CCVER, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TBLCOP (INTAB, OUTTAB, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABDES (OUTTAB, IERR)
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // IMGOUT
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A8,'= ',F10.6,5X,'/ ',A)
 1001 FORMAT (A8,'= ',F7.2,8X,'/ ',A)
 1010 FORMAT ('BMFACTOR=',F8.3,3X,'/ scale factor for new units')
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
      SUBROUTINE CBPARS (BMAJ, BMIN, BPA, CELLSG, MROT, AA, BB, CC, NC)
C-----------------------------------------------------------------------
C   CBPARS converts the beam parameters and cell size to simple
C   computing parameters and range of rows/columns
C   Inputes
C      BMAJ     R      Major axis
C      BMIN     R      Minor axis
C      BPA      R      Position angle
C      CELLSG   R(2)   X, Y cells size
C      MROT     R      Map rotation
C   Outputs
C      AA       R
C      BB       R
C      CC       R
C      NC       I      Range in cells to use
C-----------------------------------------------------------------------
      REAL      BMAJ, BMIN, BPA, CELLSG(2), MROT, AA, BB, CC
      INTEGER   NC
C
      REAL      SR, CR
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      SR = SIN ((BPA+MROT) * DG2RAD)
      CR = COS ((BPA+MROT) * DG2RAD)
      AA = ((CR/BMIN)**2 + (SR/BMAJ)**2) * (CELLSG(1)**2) * 4.0 *
     *   LOG(2.0)
      BB = ((SR/BMIN)**2 + (CR/BMAJ)**2) * (CELLSG(2)**2) * 4.0 *
     *   LOG(2.0)
      CC = ((1.0/BMIN)**2 - (1.0/BMAJ)**2) * SR * CR * 8.0 * LOG(2.0) *
     *   ABS(CELLSG(1)*CELLSG(2))
      NC = 4.0 * BMAJ / MIN (ABS(CELLSG(1)), ABS(CELLSG(2))) + 0.75
      NC = NC / 2
C
 999  RETURN
      END
