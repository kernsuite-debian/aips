LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER   NPARMS
C                                       NPARMS=no. adverbs passed.
      PARAMETER (NPARMS=7)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                                       NOTE: Uses values in PAOOF.INC
C                                       Adverb names
C                     1         2          3        4         5
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INVERS',
C           6          7
     *   'OUTVERS', 'OPCODE'/
C                                       Adverb data types (PAOOF.INC)
C                    1       2       3       4       5
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOAINT,
C          6       7
     *   OOAINT, OOACAR/
C                                       Adverb dimensions (as 2D)
C                   1    2    3    4    5    6    7
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 1,1, 1,1, 4,1/
LOCAL END
      PROGRAM POLSN
C-----------------------------------------------------------------------
C! Make a SN table from cross polarized fringe fit
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
C   Copy a subset of one table to a new table
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, INTAB*36, OUTTAB*36
      INTEGER  IRET, BUFF1(256)
      DATA PRGM /'POLSN '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL POLSIN (PRGM, INTAB, OUTTAB, IRET)
C                                       Process table
      IF (IRET.EQ.0) CALL POLSAB (INTAB, OUTTAB, IRET)
C                                       History
      IF (IRET.EQ.0) CALL POLSHI (OUTTAB)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE POLSIN (PRGN, INTAB, OUTTAB, IRET)
C-----------------------------------------------------------------------
C   POLSIN gets input parameters for POLSN and creates the input and
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
      PARAMETER (NKEY1=6)
C                                       NKEY2=no. adverbs to copy to
C                                       OUTTAB
      PARAMETER (NKEY2=5)
      INTEGER   DIM(3), DUMMY
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to INTAB
C                   1         2          3        4         5
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INVERS',
C           6
     *   'OPCODE'/
C                                       May rename adverbs to INTAB
C                    1       2        3        4       5
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'VER',
C           6
     *   'OPCODE'/
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
      CALL OPUT (INTAB, 'TBLTYPE', OOACAR, DIM, DUMMY, 'SN', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create Output Object
      OUTTAB = 'Output table'
      CALL CREATE (OUTTAB, 'TABLE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, OUTTAB, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Table type
      CALL OPUT (OUTTAB, 'TBLTYPE', OOACAR, DIM, DUMMY, 'SN', IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE POLSAB (INTAB, OUTTAB, IERR)
C-----------------------------------------------------------------------
C   Convert table.
C   Inputs:
C      INTAB   C*   Name of input table object.
C      OUTTAB  C*   Name of output table object.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*), OUTTAB*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INTEGER NUMCOL, MAXSIZ
C                                       NUMCOL = number of colums to
C                                       lookup.
      PARAMETER (NUMCOL = 16)
C                                       MAXSIZ = max table entry size as
C                                       reals
      PARAMETER (MAXSIZ = 5000)
      CHARACTER COLLAB(NUMCOL)*24, OPCODE*4, CDUMMY*1
      INTEGER   ROW, NROW, ANT, ANTMAX, NPOLN,  I, OROW, COUNT(MAXIF),
     *   COLS(NUMCOL), NCHK, REFA, TANT, NUMIF,  REFS(MAXANT), TYPE,
     *   DIM(3), IROW, NCOL, ICOL, TRF1(MAXIF), TRF2(MAXIF),
     *   NVALS(MAXSIZ), VER, CNT, NSOL, GOODRO, DUMMY
      INTEGER   TIMKOL, ANTKOL, SOUKOL, RE1KOL, IM1KOL, DE1KOL, WT1KOL,
     *   RF1KOL, RE2KOL, IM2KOL, DE2KOL, WT2KOL, RF2KOL, RA1KOL, RA2KOL,
     *   INTKOL
      LOGICAL   AVIF
      DOUBLE PRECISION TTIME
      REAL      TTINT, CURTIM, CREAL1(MAXIF), CREAL2(MAXIF),
     *   CIMAG1(MAXIF), CIMAG2(MAXIF), DELAY1(MAXIF), DELAY2(MAXIF),
     *   RATE(MAXIF), WEIGHT(MAXIF), PHAS1, PHASIF, SUM, MAG,
     *   TRE1(MAXIF), TRE2(MAXIF), TIM1(MAXIF), TIM2(MAXIF),
     *   TDE1(MAXIF), TDE2(MAXIF), TWT1(MAXIF), TWT2(MAXIF),
     *   RPHAS(MAXIF), IPHAS(MAXIF)
      EQUIVALENCE (COLS(1), TIMKOL), (COLS(2), ANTKOL),
     *   (COLS(3), SOUKOL), (COLS(4), RE1KOL), (COLS(5), IM1KOL),
     *   (COLS(6), DE1KOL), (COLS(7), WT1KOL), (COLS(8), RF1KOL),
     *   (COLS(9), RE2KOL), (COLS(10), IM2KOL), (COLS(11), DE2KOL),
     *   (COLS(12), WT2KOL), (COLS(13), RF2KOL), (COLS(14), RA1KOL),
     *   (COLS(15), RA2KOL), (COLS(16), INTKOL)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA COLLAB /'TIME', 'ANTENNA NO.', 'SOURCE ID', 'REAL1',
     *   'IMAG1', 'DELAY 1', 'WEIGHT 1',  'REFANT 1', 'REAL2',
     *   'IMAG2','DELAY 2', 'WEIGHT 2', 'REFANT 2', 'RATE 1',
     *    'RATE 2', 'TIME INTERVAL'/
      DATA RATE /MAXIF * 0.0/
      DATA WEIGHT /MAXIF * 1.0/
C-----------------------------------------------------------------------
C                                       Find reference antenna
      CALL FNDREF (INTAB, REFA, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Create output table
      CALL COPHED (INTAB, OUTTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Save table version numbers for
C                                       history
      CALL OGET (INTAB, 'VER', TYPE, DIM, VER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT ('Input', 'INVERS', OOAINT, DIM, VER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (OUTTAB, 'VER', TYPE, DIM, VER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT ('Input', 'OUTVERS', OOAINT, DIM, VER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open input table
      CALL OOPEN (INTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get OPCODE and see if averaging
C                                       SB delays in IF
      CALL OGET (INTAB, 'OPCODE', TYPE, DIM, DUMMY, OPCODE, IERR)
      IF (IERR.NE.0) GO TO 999
      AVIF = OPCODE .EQ. 'AVIF'
C                                       Find number of antennas
      CALL OGET (INTAB, 'KEY.NO_ANT', TYPE, DIM, ANTMAX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Find number of IFs
      CALL OGET (INTAB, 'KEY.NO_IF', TYPE, DIM, NUMIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Find number of columns
      CALL OGET (INTAB, 'NCOL', TYPE, DIM, NCOL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Find column numbers
      NCHK = NUMCOL
      CALL TABCOL (INTAB, NCHK, COLLAB, COLS, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.10)) GO TO 999
C                                        Make sure all columns found
      DO 10 I = 1,NCHK
         IF (COLS(I).LE.0) THEN
            MSGTXT = 'SN TABLE MISSING COLUMN ' // COLLAB(I)
            CALL MSGWRT (9)
            IERR = 7
            END IF
 10      CONTINUE
      IF (IERR.NE.0) GO TO 999
C                                        Check number of poln (2)
      CALL OGET (INTAB, 'KEY.NO_POL', TYPE, DIM, NPOLN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (NPOLN.NE.2) THEN
C                                        Data is inappropriate
         MSGTXT = 'INSUFFICIENT POLARIZATION INFORMATION'
         CALL MSGWRT (9)
         IERR = 5
         GO TO 999
         END IF
C                                        Open output table
      CALL OOPEN (OUTTAB, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get number of entries
      CALL OGET (INTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Initialize accumulation arrays
      CALL FILL (MAXIF, 0, COUNT)
      CALL RFILL (MAXIF, 0.0, RPHAS)
      CALL RFILL (MAXIF, 0.0, IPHAS)
      CALL RFILL (MAXIF, 0.0, DELAY2)
      CURTIM = -999.0
      NSOL = 0
      OROW = 0
C
      DO 500 ROW = 1,NROW
C                                       Read time and solution interval
C                                       for this row
         CALL TABDGT (INTAB, ROW, TIMKOL, TYPE, DIM, TTIME, CDUMMY,
     *      IERR)
         IF (IERR.LT.0) GO TO 500
         IF (IERR.NE.0) GO TO 999
         CALL TABDGT (INTAB, ROW, INTKOL, TYPE, DIM, TTINT, CDUMMY,
     *      IERR)
         IF (IERR.LT.0) GO TO 500
         IF (IERR.NE.0) GO TO 999
C                                       First time tag?
         IF (CURTIM .EQ. -999.0) CURTIM = TTIME
C                                       All data for this sol. interval?
         IF (TTIME-CURTIM .GT. 0.5*TTINT) THEN
C                                       Averaging SB delays?
            IF (AVIF) THEN
               SUM = 0.0
               CNT = 0.0
               DO 110 I = 1,NUMIF
                  IF (COUNT(I).GT.0) THEN
                     SUM = SUM + DELAY2(I) / COUNT(I)
                     CNT = CNT + 1
                     END IF
 110              CONTINUE
               IF (CNT.GT.0) SUM = SUM / CNT
               DO 120 I = 1,NUMIF
                  DELAY2(I) = SUM * COUNT(I)
 120              CONTINUE
               END IF
C                                       Set IF phases and delays
            DO 200 I = 1,NUMIF
               IF (COUNT(I).GT.0) THEN
C                                       Apply correction to LCP
                  CREAL1(I) = 1.0
                  CIMAG1(I) = 0.0
                  DELAY1(I) = 0.0
C                                       Zero rates
                  RATE(I) = 0.0
                  WEIGHT(I) = 1.0
                  REFS(I) = REFA
C                                       Normalize gain amplitude
                  MAG = SQRT(RPHAS(I)*RPHAS(I)+IPHAS(I)*IPHAS(I))
                  CREAL2(I) = RPHAS(I) / MAG
                  CIMAG2(I) = IPHAS(I) / MAG
                  DELAY2(I) = DELAY2(I) / COUNT(I)
               ELSE
C                                       No data - skip writing
                  GO TO 350
                  END IF
 200           CONTINUE
C                                       Write to output, one entry for
C                                       each antenna
            NSOL = NSOL + 1
            IROW = GOODRO
            DO 300 ANT = 1,ANTMAX
               OROW = OROW + 1
               DO 250 ICOL = 1,NCOL
C                                       Substitute information as needed
                  TYPE = OOARE
                  DIM(1) = NUMIF
                  DIM(2) = 1
                  DIM(3) = 0
C                                       Antenna number
                  IF (ICOL.EQ.ANTKOL) THEN
                     TYPE = OOAINT
                     DIM(1) = 1
                     NVALS(1) = ANT
C                                       Real 1
                  ELSE IF (ICOL.EQ.RE1KOL) THEN
                     CALL RCOPY (NUMIF, CREAL1, NVALS)
C                                       Imag 1
                  ELSE IF (ICOL.EQ.IM1KOL) THEN
                     CALL RCOPY (NUMIF, CIMAG1, NVALS)
C                                       Rate 1
                  ELSE IF (ICOL.EQ.RA1KOL) THEN
                     CALL RCOPY (NUMIF, RATE, NVALS)
C                                       Delay 1
                  ELSE IF (ICOL.EQ.DE1KOL) THEN
                     CALL RCOPY (NUMIF, DELAY1, NVALS)
C                                       Weight 1
                  ELSE IF (ICOL.EQ.WT1KOL) THEN
                     CALL RCOPY (NUMIF, WEIGHT, NVALS)
C                                       Reference antenna 1
                  ELSE IF (ICOL.EQ.RF1KOL) THEN
                     TYPE = OOAINT
                     CALL COPY (NUMIF, REFS, NVALS)
C                                       Real 2
                  ELSE IF (ICOL.EQ.RE2KOL) THEN
                     CALL RCOPY (NUMIF, CREAL2, NVALS)
C                                       Imag 2
                  ELSE IF (ICOL.EQ.IM2KOL) THEN
                     CALL RCOPY (NUMIF, CIMAG2, NVALS)
C                                       Rate 2
                  ELSE IF (ICOL.EQ.RA2KOL) THEN
                     CALL RCOPY (NUMIF, RATE, NVALS)
C                                       Delay 2
                  ELSE IF (ICOL.EQ.DE2KOL) THEN
                     CALL RCOPY (NUMIF, DELAY2, NVALS)
C                                       Weight 2
                  ELSE IF (ICOL.EQ.WT2KOL) THEN
                     CALL RCOPY (NUMIF, WEIGHT, NVALS)
C                                       Reference antenna 2
                  ELSE IF (ICOL.EQ.RF2KOL) THEN
                     TYPE = OOAINT
                     CALL RCOPY (NUMIF, REFS, NVALS)
C                                       Something else
                  ELSE
                     CALL TABDGT (INTAB, IROW, ICOL, TYPE, DIM, NVALS,
     *                  CDUMMY, IERR)
                     IF (IERR.NE.0) GO TO 999
                     END IF
C                                       Write to new table
                  CALL TABDPT (OUTTAB, OROW, ICOL, TYPE, DIM, NVALS,
     *               CDUMMY, IERR)
                  IF (IERR.NE.0) GO TO 999
 250              CONTINUE
 300           CONTINUE
C                                       Initialize accumulation arrays
 350        CALL FILL (MAXIF, 0, COUNT)
            CALL RFILL (MAXIF, 0.0, RPHAS)
            CALL RFILL (MAXIF, 0.0, IPHAS)
            CALL RFILL (MAXIF, 0.0, DELAY2)
C                                       This solution finished
            CURTIM = TTIME
            END IF
C                                       Read data for this row
         CALL TABDGT (INTAB, ROW, ANTKOL, TYPE, DIM, TANT, CDUMMY, IERR)
         IF (IERR.LT.0) GO TO 500
         IF (IERR.NE.0) GO TO 999
C                                        If this is the reference
C                                        antenna skip it.
         IF (TANT.NE.REFA) THEN
            CALL TABDGT (INTAB, ROW, RE1KOL, TYPE, DIM, TRE1, CDUMMY,
     *         IERR)
            IF (IERR.LT.0) GO TO 500
            IF (IERR.NE.0) GO TO 999
            CALL TABDGT (INTAB, ROW, IM1KOL, TYPE, DIM, TIM1, CDUMMY,
     *         IERR)
            IF (IERR.LT.0) GO TO 500
            IF (IERR.NE.0) GO TO 999
            CALL TABDGT (INTAB, ROW, WT1KOL, TYPE, DIM, TWT1, CDUMMY,
     *         IERR)
            IF (IERR.LT.0) GO TO 500
            IF (IERR.NE.0) GO TO 999
            CALL TABDGT (INTAB, ROW, DE1KOL, TYPE, DIM, TDE1, CDUMMY,
     *         IERR)
            IF (IERR.LT.0) GO TO 500
            IF (IERR.NE.0) GO TO 999
            CALL TABDGT (INTAB, ROW, RF1KOL, TYPE, DIM, TRF1, CDUMMY,
     *         IERR)
            IF (IERR.LT.0) GO TO 500
            IF (IERR.NE.0) GO TO 999
            CALL TABDGT (INTAB, ROW, RE2KOL, TYPE, DIM, TRE2, CDUMMY,
     *         IERR)
            IF (IERR.LT.0) GO TO 500
            IF (IERR.NE.0) GO TO 999
            CALL TABDGT (INTAB, ROW, IM2KOL, TYPE, DIM, TIM2, CDUMMY,
     *         IERR)
            IF (IERR.LT.0) GO TO 500
            IF (IERR.NE.0) GO TO 999
            CALL TABDGT (INTAB, ROW, WT2KOL, TYPE, DIM, TWT2, CDUMMY,
     *         IERR)
            IF (IERR.LT.0) GO TO 500
            IF (IERR.NE.0) GO TO 999
            CALL TABDGT (INTAB, ROW, DE2KOL, TYPE, DIM, TDE2, CDUMMY,
     *         IERR)
            IF (IERR.LT.0) GO TO 500
            IF (IERR.NE.0) GO TO 999
            CALL TABDGT (INTAB, ROW, RF2KOL, TYPE, DIM, TRF2, CDUMMY,
     *         IERR)
            IF (IERR.LT.0) GO TO 500
            IF (IERR.NE.0) GO TO 999
            GOODRO = ROW
C                                        Reference phases to IF 1.
            IF ((TRE1(1).NE.FBLANK) .AND. (TWT1(1).GT.0.0)) THEN
               PHAS1 = ATAN2 (TIM1(1), TRE1(1)+1.0E-20)
               DO 400 I = 1,NUMIF
                  IF ((TWT1(I).GT.0.0) .AND. (TRE1(I).NE.FBLANK) .AND.
     *               (TRF1(I).EQ.REFA)) THEN
                     PHASIF = -(ATAN2 (TIM1(I),TRE1(I)+1.0E-20) - PHAS1)
                     RPHAS(I) = RPHAS(I) + COS (PHASIF)
                     IPHAS(I) = IPHAS(I) + SIN (PHASIF)
                     DELAY2(I) = DELAY2(I) - TDE1(I)
                     COUNT(I) = COUNT(I) + 1
                     END IF
 400              CONTINUE
               END IF
            IF ((TRE2(1).NE.FBLANK) .AND. (TWT2(1).GT.0.0)) THEN
               PHAS1 = ATAN2 (TIM2(1), TRE2(1)+1.0E-20)
               DO 450 I = 1,NUMIF
                  IF ((TWT2(I).GT.0.0) .AND. (TRE2(I).NE.FBLANK) .AND.
     *               (TRF2(I).EQ.REFA)) THEN
                     PHASIF = (ATAN2 (TIM2(I),TRE2(I)+1.0E-20) - PHAS1)
                     RPHAS(I) = RPHAS(I) + COS (PHASIF)
                     IPHAS(I) = IPHAS(I) + SIN (PHASIF)
                     DELAY2(I) = DELAY2(I) + TDE2(I)
                     COUNT(I) = COUNT(I) + 1
                     END IF
 450              CONTINUE
               END IF
            END IF
 500     CONTINUE
C                                       Write the last solution
C                                       Averaging SB delays?
      IF (AVIF) THEN
         SUM = 0.0
         CNT = 0.0
         DO 510 I = 1,NUMIF
            IF (COUNT(I).GT.0) THEN
               SUM = SUM + DELAY2(I) / COUNT(I)
               CNT = CNT + 1
               END IF
 510        CONTINUE
         IF (CNT.GT.0) SUM = SUM / CNT
         DO 520 I = 1,NUMIF
            DELAY2(I) = SUM * COUNT(I)
 520        CONTINUE
         END IF
C                                       Set IF phases and delays
      DO 550 I = 1,NUMIF
         IF (COUNT(I).GT.0) THEN
C                                       Apply correction to LCP
            CREAL1(I) = 1.0
            CIMAG1(I) = 0.0
            DELAY1(I) = 0.0
C                                       Zero rates
            RATE(I) = 0.0
            WEIGHT(I) = 1.0
            REFS(I) = REFA
C                                       Normalize gain amplitude
            MAG = SQRT(RPHAS(I)*RPHAS(I)+IPHAS(I)*IPHAS(I))
            CREAL2(I) = RPHAS(I) / MAG
            CIMAG2(I) = IPHAS(I) / MAG
            DELAY2(I) = DELAY2(I) / COUNT(I)
         ELSE
C                                       No data - skip writing
            GO TO 800
            END IF
 550     CONTINUE
C                                       Write to output, one entry for
C                                       each antenna
      NSOL = NSOL + 1
      IROW = GOODRO
      DO 700 ANT = 1,ANTMAX
         OROW = OROW + 1
         DO 650 ICOL = 1,NCOL
C                                       Substitute information as needed
            TYPE = OOARE
            DIM(1) = NUMIF
            DIM(2) = 1
            DIM(3) = 0
C                                       Antenna number
            IF (ICOL.EQ.ANTKOL) THEN
               TYPE = OOAINT
               DIM(1) = 1
               NVALS(1) = ANT
C                                       Real 1
            ELSE IF (ICOL.EQ.RE1KOL) THEN
               CALL RCOPY (NUMIF, CREAL1, NVALS)
C                                       Imag 1
            ELSE IF (ICOL.EQ.IM1KOL) THEN
               CALL RCOPY (NUMIF, CIMAG1, NVALS)
C                                       Rate 1
            ELSE IF (ICOL.EQ.RA1KOL) THEN
               CALL RCOPY (NUMIF, RATE, NVALS)
C                                       Delay 1
            ELSE IF (ICOL.EQ.DE1KOL) THEN
               CALL RCOPY (NUMIF, DELAY1, NVALS)
C                                       Weight 1
            ELSE IF (ICOL.EQ.WT1KOL) THEN
               CALL RCOPY (NUMIF, WEIGHT, NVALS)
C                                       Reference antenna 1
            ELSE IF (ICOL.EQ.RF1KOL) THEN
               TYPE = OOAINT
               CALL COPY (NUMIF, REFS, NVALS)
C                                       Real 2
            ELSE IF (ICOL.EQ.RE2KOL) THEN
               CALL RCOPY (NUMIF, CREAL2, NVALS)
C                                       Imag 2
            ELSE IF (ICOL.EQ.IM2KOL) THEN
               CALL RCOPY (NUMIF, CIMAG2, NVALS)
C                                       Rate 2
            ELSE IF (ICOL.EQ.RA2KOL) THEN
               CALL RCOPY (NUMIF, RATE, NVALS)
C                                       Delay 2
            ELSE IF (ICOL.EQ.DE2KOL) THEN
               CALL RCOPY (NUMIF, DELAY2, NVALS)
C                                       Weight 2
            ELSE IF (ICOL.EQ.WT2KOL) THEN
               CALL RCOPY (NUMIF, WEIGHT, NVALS)
C                                       Reference antenna 2
            ELSE IF (ICOL.EQ.RF2KOL) THEN
               TYPE = OOAINT
               CALL RCOPY (NUMIF, REFS, NVALS)
C                                       Something else
            ELSE
               CALL TABDGT (INTAB, IROW, ICOL, TYPE, DIM, NVALS,
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
C                                       Write to new table
            CALL TABDPT (OUTTAB, OROW, ICOL, TYPE, DIM, NVALS,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 999
 650        CONTINUE
 700     CONTINUE
C                                       No data written? - barf and die
 800  IF (NSOL .EQ. 0) THEN
         IERR = 5
         WRITE (MSGTXT,1800) I
         CALL MSGWRT (9)
         GO TO 999
         END IF
C                                       Close tables
      CALL OCLOSE (INTAB, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OCLOSE (OUTTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1800 FORMAT ('ERROR: NO DATA FOUND FOR IF ',I3)
      END
      SUBROUTINE POLSHI (OUTTAB)
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
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INVERS', 'OUTVERS',
     *   'OPCODE'/
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
      CALL MSGWRT (6)
 999  RETURN
      END
      SUBROUTINE FNDREF (INTAB, REFANT, IERR)
C-----------------------------------------------------------------------
C   Determine the most common reference antenna
C   Inputs:
C      INTAB   C*?  Name of input table object.
C   Output:
C      REFANT  I    Most common reference antenna
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*)
      INTEGER   REFANT, IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IROW, NROW, TYPE, DIM(3), RFA1(MAXIF), RFA2(MAXIF),
     *   COLS(2), RF1KOL, RF2KOL, NCHK, I, ANTCNT(MAXANT), MAXCNT
      CHARACTER COLLAB(2)*24, CDUMMY*1
      EQUIVALENCE (COLS(1), RF1KOL), (COLS(2), RF2KOL)
      INCLUDE 'INCS:DMSG.INC'
      DATA COLLAB /'REFANT 1', 'REFANT 2'/
C-----------------------------------------------------------------------
C                                       Open input table
      CALL OOPEN (INTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get number of entries
      CALL OGET (INTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Find column numbers
      NCHK = 2
      CALL TABCOL (INTAB, NCHK, COLLAB, COLS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Make sure all columns found
      DO 10 I = 1,NCHK
         IF (COLS(I).LT.0) THEN
            MSGTXT = 'MISSING COLUMN ' // COLLAB(I)
            CALL MSGWRT (9)
            IERR = 7
            END IF
 10      CONTINUE
      IF (IERR.NE.0) GO TO 999
      CALL FILL (MAXANT, 0, ANTCNT)
C                                       count useage of reference
C                                       antennas.
      DO 100 IROW = 1,NROW
         CALL TABDGT (INTAB, IROW, RF1KOL, TYPE, DIM, RFA1, CDUMMY,
     *      IERR)
         IF (IERR.LT.0) GO TO 100
         IF (IERR.NE.0) GO TO 999
         CALL TABDGT (INTAB, IROW, RF2KOL, TYPE, DIM, RFA2, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         IF (IERR.LT.0) GO TO 100
         DO 50 I = 1,DIM(1)
            IF ((RFA1(I).GT.0) .AND. (RFA1(I).LE.MAXANT))
     *         ANTCNT(RFA1(I)) = ANTCNT(RFA1(I)) + 1
            IF ((RFA2(I).GT.0) .AND. (RFA2(I).LE.MAXANT))
     *         ANTCNT(RFA2(I)) = ANTCNT(RFA2(I)) + 1
 50         CONTINUE
 100     CONTINUE
C                                       Find maximum
      MAXCNT = 0
      REFANT = 0
      DO 200 I = 1,MAXANT
         IF (ANTCNT(I).GT.MAXCNT) THEN
            MAXCNT = ANTCNT(I)
            REFANT = I
            END IF
 200     CONTINUE
C                                       Make sure one found
      IF (REFANT.LE.0) THEN
         MSGTXT = 'NO REFERENCE ANTENNAS FOUND'
         CALL MSGWRT (9)
         IERR = 2
         GO TO 999
         END IF
C                                       Close table
      CALL OCLOSE (INTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
