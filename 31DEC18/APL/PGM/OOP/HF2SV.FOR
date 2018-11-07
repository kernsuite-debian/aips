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
C                                       NOTE: Uses values in PAOOF.INC
C                                       Adverb names
C                     1         2          3        4         5
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INEXT',
C         6         7           8         9         10
     * 'INVERS', 'ANTENNAS', 'BCOUNT' ,'ECOUNT', 'OUTFILE'/
C                                       Adverb data types (PAOOF.INC)
C                    1       2       3       4       5
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOACAR,
C          6       7       8       9       10
     *   OOAINT, OOAINT, OOAINT, OOAINT, OOACAR/
C                                       Adverb dimensions (as 2D)
C                   1    2    3    4    5
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 2,1,
C         6     7    8    9    10
     *   1,1, 50,1, 1,1, 1,1, 48,1/
LOCAL END
      PROGRAM HF2SV
C-----------------------------------------------------------------------
C! Convert HF table to binary table for Mark III geodetic program DBEDIT
C# Calibration VLBI OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2001-2002, 2009, 2015
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
C   Paraform AIPS OOP task processing a table.  This version HF2SV
C   converts the HF table into a binary table for input into the
C   geodetic package Solve.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, INTAB*36, FNAME*256
      INTEGER  IRET, BUFF1(256), NCH
      DATA PRGM /'HF2SV '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL TABTIN (PRGM, INTAB, FNAME, NCH, IRET)
C                                       Process table
      IF (IRET.EQ.0) CALL TABTAB (INTAB, FNAME, NCH, IRET)
C                                       Close down files, etc.
C  For use in Batch jobs: If error, close down but continue
      IRET=0
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE TABTIN (PRGN, INTAB, FNAME, NCH, IRET)
C-----------------------------------------------------------------------
C   TABTIN gets input parameters for HF2SV and creates the input object.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      FNAME   C*256  Directory name
C      NCH     I    Character is director name (plus /)
C      IRET    I    Error code: 0 => ok
C                               1=>  FRING.LIST alread there
C                               2=>  Error writing/reading FRING.LIST
C                               4 => user routine detected error
C                               5 => catalog troubles
C                               8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, INTAB*36, FNAME*(*)
C
      INTEGER   NKEY1
C                                       NKEY1=no. adverbs to copy to
C                                       INTAB
      PARAMETER (NKEY1=10)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32
C                                       for reading to EOF, index file
      CHARACTER CDUMMY*1
      INTEGER   DIM(3), FDUM(16), TYPE, NCH, I
      CHARACTER LFILE*48, FILNAM*256
      LOGICAL EXISTS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to INTAB
C                   1         2          3        4         5
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INEXT',
C           6         7           8         9         10
     *   'INVERS', 'ANTENNAS', 'BCOUNT', 'ECOUNT', 'OUTFILE'/
C                                       May rename adverbs to INTAB
C                    1       2        3        4       5
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'TBLTYPE',
C           6      7         8         9         10
     *   'VER', 'REFANT', 'BCOUNT', 'ECOUNT', 'FILE'/
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
C                                       Get file name
      CALL OGET (INTAB, 'FILE', TYPE, DIM, FDUM, LFILE, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Crack name
      CALL ZFULLN(LFILE, ' ', '     ', FNAME, IRET)
      IF (IRET.NE.0) GO TO 999
      DO 50 I = 1,127
         IF (FNAME(I:I).EQ.' ') GO TO 60
 50      CONTINUE
 60   NCH = I
      FNAME(I:I) = '/'
C      print *, 'nch ', nch
      WRITE (MSGTXT, 1010) FNAME(1:NCH)
 1010 FORMAT ('Directory: ',A)
      CALL MSGWRT(4)
C                                       See if Fring index file exists
      FILNAM = FNAME(1:NCH) // 'FRING.LIST'
      INQUIRE (FILE=FILNAM, EXIST=EXISTS)
      IF (EXISTS) THEN
C                                       Yes, so open, read to end
         OPEN (UNIT=23, FILE=FILNAM, STATUS='OLD')
 100     READ (23, 1110,END=120) CDUMMY
         GO TO 100
 120     WRITE (MSGTXT, 1120)
         CALL MSGWRT(4)
        ELSE
         OPEN (UNIT=23, FILE=FILNAM, STATUS='NEW')
         WRITE (MSGTXT, 1121)
         CALL MSGWRT(4)
         ENDIF
C
 999  RETURN
C-----------------------------------------------------------------------
 1110 FORMAT (A1)
 1120 FORMAT ('Existing FRING.LIST file found, appending to it.')
 1121 FORMAT ('Created FRING.LIST')
      END
      SUBROUTINE TABTAB (INTAB, FNAME, NCH, IERR)
C-----------------------------------------------------------------------
C   Convert table.
C   Inputs:
C      INTAB   C*   Name of input table object.
C      FNAME   C*   Directory name
C      NCH     I    # characters in directory name
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*), FNAME*(*)
      INTEGER   IERR, NCH
C                                       MAXSIZ = max table entry size as
C                                       reals or characters
      INTEGER   MAXSIZ
      PARAMETER (MAXSIZ = 5000)
      INTEGER   IROW, OROW, NROW, ICOL, NCOL, BC, EC, TYPE, DIM(3)
      REAL      RVALS(MAXSIZ)
      CHARACTER CVALS*(MAXSIZ*4), CDUMMY*1
      CHARACTER*8 ANNAME(50), S1, S2
      INTEGER   IVALS(MAXSIZ)
      DOUBLE PRECISION DVALS(MAXSIZ/2)
      COMMON /HFTST/ RVALS
      COMMON /HFTSTC/ CVALS
      EQUIVALENCE  (RVALS(1), DVALS(1), IVALS(1))
      INTEGER   REFANT(50), NREF, NNREF, SUBARR
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Open input table
      CALL OOPEN (INTAB, 'READ', IERR)
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
C                                       Get antenna names
      SUBARR = 1
      CALL ANTIFO (INTAB, SUBARR, ANNAME, IERR)
C                                       Reference antennas
      CALL OGET (INTAB, 'REFANT', TYPE, DIM, REFANT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (REFANT(1) .EQ. 0) THEN
         NREF = 0
         GO TO 20
         ENDIF
         DO 10 IROW = 2, 50
            IF (REFANT(IROW) .NE. 0 .AND.
     *          REFANT(IROW) .NE. REFANT(IROW-1)) GO TO 10
            NREF = IROW - 1
            GO TO 20
 10         CONTINUE
C                                       Loop over selected rows.
 20   IF (NREF .EQ. 0) THEN
         WRITE (MSGTXT, 1020)
 1020    FORMAT ('All baselines written out')
         CALL MSGWRT(4)
         ELSE
         NNREF = MIN (5,NREF)
         WRITE (MSGTXT, 1021) (ANNAME(REFANT(IROW)),IROW=1,NNREF)
         CALL MSGWRT(4)
 1021    FORMAT ('Using reference antennas ', 5(A8,1X))
         ENDIF
C                                       HF table loop
      OROW = 0
      DO 100 IROW = BC,EC
C                                       Check refant
         IF (NREF .EQ. 0) GO TO 40
         CALL TABDGT (INTAB, IROW, 26, TYPE, DIM, RVALS, CVALS, IERR)
         IF (IERR.NE.0) GO TO 999
         S1 = CVALS(1:8)
         CALL TABDGT (INTAB, IROW, 27, TYPE, DIM, RVALS, CVALS, IERR)
         IF (IERR.NE.0) GO TO 999
         S2 = CVALS(1:8)
         DO 30 ICOL = 1, NREF
            IF (ANNAME(REFANT(ICOL)) .EQ. S1 .OR.
     *          ANNAME(REFANT(ICOL)) .EQ. S2) GO TO 40
 30         CONTINUE
         GO TO 100
C                                       HF table information
 40      OROW = OROW + 1
         DO 50 ICOL = 1,NCOL
            CALL TABDGT (INTAB, IROW, ICOL, TYPE, DIM, RVALS, CVALS,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Conversion Routine
            CALL CNSLV (IROW, NCOL, ICOL, DIM(1), FNAME, NCH, IERR)
            IF (IERR.NE.0) GO TO 999
 50         CONTINUE
         IF (MOD(OROW,500) .EQ. 1) THEN
            WRITE (MSGTXT, 1050) IROW, OROW
 1050       FORMAT ('Converting HF row ', I5,' into File ', I5)
            CALL MSGWRT(1)
            ENDIF
 100     CONTINUE
C                                       Close tables
      CALL OCLOSE (INTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE CNSLV (IROW, NCOL, ICOL, INUM, FNAME, NCH, IERR)
C-----------------------------------------------------------------------
C   This subroutine converts the HF data files into a binary form for
C   for input into the Mark III geodetic DBEDIT program. Most of the
C   conversion is done with EQUIVALENCE statments.
C-----------------------------------------------------------------------
C                                       General specifications
      INTEGER  IROW, NCOL, ICOL, INUM, IERR, I, ILENG,
     *   ITRIM, ISEC, JDATE(6), II, CH10, IOST, NTRY
      CHARACTER BLANK*1, CC*1, CF*1, CDOT*1, CLINE*1, N1CHAR*3,
     *   FPART*4, N2CHAR*4, LPART*6, DNAME*8, SDIR*10, SDIRBK*10,
     *   OUTFIL*86, COMAND*128
      LOGICAL   EXISTS
      CHARACTER FNAME*(*)
      INTEGER   NCH
C                                       Data arrays
      INTEGER   MAXSIZ
      PARAMETER (MAXSIZ = 5000)
      INTEGER   IVALS(MAXSIZ)
      REAL      RVALS(MAXSIZ)
      DOUBLE PRECISION DVALS(MAXSIZ/2)
      CHARACTER CVALS*(MAXSIZ*4)
      COMMON /HFTST/ RVALS
      COMMON /HFTSTC/ CVALS
      EQUIVALENCE (RVALS(1), DVALS(1), IVALS(1))
      INCLUDE 'INCS:DMSG.INC'
C                                       Record 0 conversions
      INTEGER*2 H0(128), H1(128), H2(128), H3(128), H4(128), H5(128)
      INTEGER*2 JUNK(128)
      CHARACTER C1*2
      INTEGER*2 I2(6), I3(6), I4(4), I9(28), I10(56), I12(6)
      INTEGER*2 I5, I6, I7, I8, I11, I13, I14, I15, I16, I17
C
      EQUIVALENCE (H0(3),  C1),     (H0(7),  I2(1)),  (H0(13), I3(1)),
     *            (H0(19), I4(1)),  (H0(23), I5),     (H0(24), I6),
     *            (H0(25), I7),     (H0(26), I8),     (H0(27), I9(1)),
     *            (H0(55), I10(1)), (H0(111),I11),    (H0(112),I12(1)),
     *            (H0(118),I13),    (H0(119),I14),    (H0(120),I15),
     *            (H0(121),I16),    (H0(122),I17)
C                                       Record 1,2,3 Conversion
      CHARACTER*2  C33, C34
      CHARACTER*6  C28, C31, C35, C36
      CHARACTER*8  C25, C26, C27, C29, C30, C32, C37, C38
C
      INTEGER*2  I18(28), I19(84)
      INTEGER*2  I20(28), I21(56), I22(28), I23, I24
C
      EQUIVALENCE (H1(7),  I18(1)), (H1(35), I19(1))
      EQUIVALENCE (H2(7),  I20(1)), (H2(35), I21(1)), (H2(91), I22(1)),
     1            (H2(119),I23),    (H2(120),I24)
      EQUIVALENCE (H3(7),  C25),    (H3(11), C26),    (H3(15), C27),
     1            (H3(19), C28),    (H3(22), C29),    (H3(26), C30),
     2            (H3(30), C31),    (H3(33), C32),    (H3(37), C33),
     3            (H3(38), C34),    (H3(39), C35),    (H3(42), C36),
     4            (H3(49), C37),    (H3(53), C38)
C                                       Record 4 Conversion
      DOUBLE PRECISION D39(14), D40, D41, D42, D43, D44, D45, D46,
     1       D47, D48, D49, D50
      EQUIVALENCE (H4(5),  D39(1)), (H4(61), D40),    (H4(65), D41),
     1            (H4(69), D42),    (H4(73), D43),    (H4(77), D44),
     2            (H4(81), D45),    (H4(85), D46),    (H4(89), D47),
     3            (H4(93), D48),    (H4(97), D49),    (H4(101),D50)
C                                       Record 5 Conversion
      REAL  R51(28), R52(2), R53, R54, R55, R56, R57, R58, R59(2),
     1      R60(2), R61, R62(2), R63(6), R64, R65, R66, R67, R68,
     2      R69, R70, R71, R72, R73, R74, R75, R76, R77
      EQUIVALENCE (H5(3),  R51(1)), (H5(59), R52(1)), (H5(63), R53),
     1            (H5(65), R54),    (H5(67), R55),    (H5(69), R56),
     2            (H5(71), R57),    (H5(73), R58),    (H5(75), R59(1)),
     3            (H5(79), R60(1)), (H5(83), R61),    (H5(85), R62(1)),
     4            (H5(89), R63(1)), (H5(101),R64),    (H5(103),R65),
     5            (H5(105),R66),    (H5(107),R67),    (H5(109),R68),
     6            (H5(111),R69),    (H5(113),R70),    (H5(115),R71),
     7            (H5(117),R72),    (H5(119),R73),    (H5(121),R74),
     8            (H5(123),R75),    (H5(125),R76),    (H5(127),R77)
C
      DATA  CH10 / 10 /
      DATA  CDOT, CLINE /'.', '_'/
      DATA  BLANK /' '/
      DATA  SDIRBK /'          '/
      DATA  JUNK / 128*0/
C-----------------------------------------------------------------------
C                                       Convert HF entries
      IF (ICOL.EQ.1) C1 = CVALS
      IF (ICOL.EQ.2) CALL IMANY (IVALS, INUM, I2)
      IF (ICOL.EQ.3) CALL IMANY (IVALS, INUM, I3)
      IF (ICOL.EQ.4) CALL IMANY (IVALS, INUM, I4)
      IF (ICOL.EQ.5) I5 = IVALS(1)
      IF (ICOL.EQ.6) I6 = IVALS(1)
      IF (ICOL.EQ.7) I7 = IVALS(1)
      IF (ICOL.EQ.8) I8 = IVALS(1)
      IF (ICOL.EQ.9) CALL IMANY (IVALS, INUM, I9)
      IF (ICOL.EQ.10) CALL IMANY (IVALS, INUM, I10)
      IF (ICOL.EQ.11) I11 = IVALS(1)
      IF (ICOL.EQ.12) CALL IMANY (IVALS, INUM, I12)
      IF (ICOL.EQ.13) I13 = IVALS(1)
      IF (ICOL.EQ.14) I14 = IVALS(1)
      IF (ICOL.EQ.15) I15 = IVALS(1)
      IF (ICOL.EQ.16) I16 = IVALS(1)
      IF (ICOL.EQ.17) I17 = IVALS(1)
      IF (ICOL.EQ.18) CALL IMANY (IVALS, INUM, I18)
      IF (ICOL.EQ.19) CALL IMANY (IVALS, INUM, I19)
      IF (ICOL.EQ.20) CALL IMANY (IVALS, INUM, I20)
      IF (ICOL.EQ.21) CALL IMANY (IVALS, INUM, I21)
      IF (ICOL.EQ.22) CALL IMANY (IVALS, INUM, I22)
      IF (ICOL.EQ.23) I23 = IVALS(1)
      IF (ICOL.EQ.24) I24 = IVALS(1)
      IF (ICOL.EQ.25) C25 = CVALS
      IF (ICOL.EQ.26) C26 = CVALS
      IF (ICOL.EQ.27) C27 = CVALS
      IF (ICOL.EQ.28) C28 = CVALS
      IF (ICOL.EQ.29) C29 = CVALS
      IF (ICOL.EQ.30) C30 = CVALS
      IF (ICOL.EQ.31) C31 = CVALS
      IF (ICOL.EQ.32) C32 = CVALS
      IF (ICOL.EQ.33) C33 = CVALS
      IF (ICOL.EQ.34) C34 = CVALS
      IF (ICOL.EQ.35) C35 = CVALS
      IF (ICOL.EQ.36) C36 = CVALS
      IF (ICOL.EQ.37) C37 = CVALS
      IF (ICOL.EQ.38) C38 = CVALS
      IF (ICOL.EQ.39) CALL DMANY(DVALS, INUM, D39)
      IF (ICOL.EQ.40) D40 = DVALS(1)
      IF (ICOL.EQ.41) D41 = DVALS(1)
      IF (ICOL.EQ.42) D42 = DVALS(1)
      IF (ICOL.EQ.43) D43 = DVALS(1)
      IF (ICOL.EQ.44) D44 = DVALS(1)
      IF (ICOL.EQ.45) D45 = DVALS(1)
      IF (ICOL.EQ.46) D46 = DVALS(1)
      IF (ICOL.EQ.47) D47 = DVALS(1)
      IF (ICOL.EQ.48) D48 = DVALS(1)
      IF (ICOL.EQ.49) D49 = DVALS(1)
      IF (ICOL.EQ.50) D50 = DVALS(1)
      IF (ICOL.EQ.51) CALL RMANY(RVALS, INUM, R51)
      IF (ICOL.EQ.52) CALL RMANY(RVALS, INUM, R52)
      IF (ICOL.EQ.53) R53 = RVALS(1)
      IF (ICOL.EQ.54) R54 = RVALS(1)
      IF (ICOL.EQ.55) R55 = RVALS(1)
      IF (ICOL.EQ.56) R56 = RVALS(1)
      IF (ICOL.EQ.57) R57 = RVALS(1)
      IF (ICOL.EQ.58) R58 = RVALS(1)
      IF (ICOL.EQ.59) CALL RMANY(RVALS, INUM, R59)
      IF (ICOL.EQ.60) CALL RMANY(RVALS, INUM, R60)
      IF (ICOL.EQ.61) R61 = RVALS(1)
      IF (ICOL.EQ.62) CALL RMANY(RVALS, INUM, R62)
      IF (ICOL.EQ.63) CALL RMANY(RVALS, INUM, R63)
      IF (ICOL.EQ.64) R64 = RVALS(1)
      IF (ICOL.EQ.65) R65 = RVALS(1)
      IF (ICOL.EQ.66) R66 = RVALS(1)
      IF (ICOL.EQ.67) R67 = RVALS(1)
      IF (ICOL.EQ.68) R68 = RVALS(1)
      IF (ICOL.EQ.69) R69 = RVALS(1)
      IF (ICOL.EQ.70) R70 = RVALS(1)
      IF (ICOL.EQ.71) R71 = RVALS(1)
      IF (ICOL.EQ.72) R72 = RVALS(1)
      IF (ICOL.EQ.73) R73 = RVALS(1)
C     IF (ICOL.EQ.73) print *, 'R73 ', R73
      IF (ICOL.EQ.74) R74 = RVALS(1)
      IF (ICOL.EQ.75) R75 = RVALS(1)
      IF (ICOL.EQ.76) R76 = RVALS(1)
      IF (ICOL.EQ.77) R77 = RVALS(1)
C
C                                       Print out (debug)
C       WRITE (*, 3050) IROW, ICOL, TYPE, INUM
C 3050 FORMAT (' ROW ', I4, '  COL ', I4, ' TYPE ', I2, '  # ', I4)
C       IF (TYPE.EQ.1) WRITE (*, 3051) (DVALS(I), I=1,INUM)
C       IF (TYPE.EQ.2) WRITE (*, 3052) (RVALS(I), I=1,INUM)
C       IF (TYPE.EQ.3) WRITE (*, 3053) CVALS
C       IF (TYPE.EQ.4) WRITE (*, 3054) (IVALS(I), I=1,INUM)
C 3051 FORMAT (1X, 5D22.15)
C 3052 FORMAT (1X, 5E22.15)
C 3053 FORMAT (1X, A8)
C 3054 FORMAT (1X, 15I7)
C
      IF (ICOL .LT. NCOL) RETURN
C                                       All columns done
C                                       Fix some things
C                                        Add in codes
      H0(1) = 4000
      H1(1) = 4100
      H2(1) = 4200
      H3(1) = 4300
      H4(1) = 4400
      H5(1) = 4500
C                                        Fix times
      IF (I2(6) .EQ. 60) THEN
         I2(6) = 0
         I2(5) = I2(5) + 1
         ENDIF
      IF (I3(6) .EQ. 60) THEN
         I3(6) = 0
         I3(5) = I3(5) + 1
         ENDIF
      IF (I12(6) .EQ. 60) THEN
         I12(6) = 0
         I12(5) = I12(5) + 1
         ENDIF
C                                        Fix years
      H0(7) = H0(7) - 1900
      H0(112) = H0(112) - 1900
C                                        Fix QuaL code 'X ' TO ' X'
      CC = C33
      C33 = BLANK // CC
C                                        Corlvr = 772
      H0(111) = 772
C                                        Subdirectory name
C                                        DDD-HHMMSS
      NTRY = 0
C
C      ISEC = MOD (IFIX(R70+0.5), 60)
CC  Fix for start time and obs time different hours
C       IF (ISEC .LT. 0) ISEC = ISEC + 60
CC
C 304  IF (ISEC .GT. 9) THEN
C         WRITE (SSEC, 3070) ISEC
C 3070    FORMAT (I2)
C         ELSE
C         WRITE (SSEC, 3071) ISEC
C 3071    FORMAT ('0',I1)
C         ENDIF
C      SDIR = C32 // SSEC
C
  304  CONTINUE
       WRITE (SDIR, 3072) C32(1:4), I2(4), I2(5), I2(6)
 3072  FORMAT (A4,3I2)
C   Beware of blanks in file name!
       DO 3073 I=1,10
        IF (SDIR(I:I) .EQ. ' ') SDIR(I:I) = '0'
 3073  CONTINUE
C      print *, ' SDIR, SDIRBK  ', SDIR, SDIRBK
C
      NTRY = NTRY+1
C                                        Create new subdirectory?
      IF (SDIR .EQ. SDIRBK) GO TO 500
C        CALL ZCRDIR (CH10, SDIR, IERR)
         COMAND = 'mkdir ' //FNAME(1:NCH)//SDIR
         CALL SYSTEM (COMAND)
         IERR = 0
         IF (IERR.NE.0) THEN
            COMAND = FNAME
            WRITE (MSGTXT,3080) COMAND(1:NCH)//SDIR
 3080       FORMAT (' could not make subdirectory ', A60)
            CALL MSGWRT (8)
            IERR = 1
            GO TO 999
           ELSE
            SDIRBK = SDIR
            ENDIF
C                                        Start time 6-char code
      JDATE(1) = I12(1)
      JDATE(2) = I12(2)
      JDATE(3) = I12(3)
      READ (C32, 3083) JDATE(4), JDATE(5)
 3083 FORMAT (4X, I2, I2)
      JDATE(6) = ISEC
      CALL JTIME (JDATE, LPART)
C                                        Make root filed
      DNAME = C25
      ILENG = ITRIM (DNAME)
C                                        Get rid of '.' in name
      DO 480  II = 1,ILENG
         IF (DNAME(II:II) .NE. CDOT) GO TO 480
            DNAME(II:II) = CLINE
 480     CONTINUE
      OUTFIL = FNAME(1:NCH) // SDIR // '/' // DNAME(1:ILENG)
     *         // '.' // LPART
C                                        Does file exist?
      INQUIRE (FILE=OUTFIL, EXIST=EXISTS)
      IF (.NOT. EXISTS) THEN
         OPEN (UNIT=22, STATUS='NEW', FORM='UNFORMATTED',
     *      ACCESS='DIRECT', RECL=256, FILE=OUTFIL,
     *      ERR=490, IOSTAT=IOST)
         GO TO 495
C                                        Problem?  Remake directory
 490     IF (NTRY .GT. 10) THEN
            IERR = 1
            WRITE (MSGTXT, 1490) OUTFIL, NTRY
 1490       FORMAT ('Cannot make ', A26, ' after ', I3, ' tries')
            CALL MSGWRT(4)
            WRITE (MSGTXT, 1491) IROW
 1491       FORMAT ('Restart HF2SV with BCOUNT ', I5)
            CALL MSGWRT(4)
            GO TO 999
            ENDIF
C                                        Remove bad dir, redo
        PRINT *, ' Removing directory ?? '
         CALL SYSTEM ('rmdir ' // SDIR // '?')
         SDIR = '          '
         SDIRBK = '          '
C                                        Try again
         GO TO 304
C                                        Sub dir,root file written
 495     WRITE (22,REC=1) JUNK
         CLOSE(22)
         WRITE (23, 5098)  OUTFIL
 5098    FORMAT (11X, 'Rootfile made in file ', A86)
         IF (NTRY .GT. 1) THEN
            WRITE (MSGTXT, 5097) OUTFIL, NTRY
 5097       FORMAT (A26, '  Root file created after ',I3,' tries')
            ENDIF
         ENDIF
C                                        Get fringe file name
 500  CONTINUE
      CF = C34
      FPART = C1 // '.' // CF
      DO 510 I = 1,99
         IF (I .LT. 10) THEN
            WRITE (N1CHAR, 5101) I
 5101       FORMAT ('.',I1,'.')
            OUTFIL = FNAME(1:NCH) // SDIR // '/' // FPART
     *               // N1CHAR // LPART
            ELSE
            WRITE (N2CHAR, 5102) I
 5102       FORMAT ('.',I2,'.')
            OUTFIL = FNAME(1:NCH) // SDIR // '/' // FPART
     *              // N2CHAR // LPART
            ENDIF
         INQUIRE (FILE=OUTFIL, EXIST=EXISTS)
         IF (.NOT. EXISTS) THEN
C                                        Successful open
            OPEN(UNIT=22, STATUS='NEW', FORM='UNFORMATTED',
     *         ACCESS='DIRECT', RECL=256, FILE=OUTFIL)
            WRITE (23, 5100) IROW, C25, C33, OUTFIL
 5100       FORMAT (I6, 2X, A8, '  QC= ',A2, ' in file ', A86)
            GO TO 520
            ELSE
C   File exists, make small change and try again
              IF(I.LE.8) GO TO 510
            WRITE(MSGTXT,5117) OUTFIL
            WRITE(6,5117) OUTFIL
 5117        FORMAT('HF2SV: File ',A86,' already exists!!!')
             CALL MSGWRT(4)
C             IF(I.LE.98) GO TO 510
            IERR = 1
            GO TO 999
            ENDIF
 510     CONTINUE
C                                        Too many duplicate files
      WRITE (MSGTXT,5110) OUTFIL, I
 5110 FORMAT (' could not open file ', A26, ' after ', I3, ' tries')
      CALL MSGWRT (8)
      IERR = 1
      GO TO 999
C                                        File opened.  Now write
 520  CONTINUE
      WRITE (22, REC=1) H0
      WRITE (22, REC=2) H1
      WRITE (22, REC=3) H2
      WRITE (22, REC=4) H3
      WRITE (22, REC=5) H4
      WRITE (22, REC=6) H5
      CLOSE (22)
      IERR = 0
C
 999  CONTINUE
      RETURN
      END
      SUBROUTINE IMANY (IVALS, INUM, IVAL)
C-----------------------------------------------------------------------
C   Variation on COPY.  Note conversion from I*4 to I*2
C-----------------------------------------------------------------------
      INTEGER   INUM, IVALS(*)
      INTEGER*2 IVAL(*)
C
      INTEGER   I
C-----------------------------------------------------------------------
      DO 10 I = 1,INUM
         IVAL(I) = IVALS(I)
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE DMANY (DVALS, INUM, DVAL)
C-----------------------------------------------------------------------
C   Variation on RCOPY for double precision
C-----------------------------------------------------------------------
      INTEGER   INUM
      DOUBLE PRECISION DVALS(*), DVAL(*)
C
      INTEGER   I
C-----------------------------------------------------------------------
      DO 10 I = 1,INUM
         DVAL(I) = DVALS(I)
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RMANY (RVALS, INUM, RVAL)
C-----------------------------------------------------------------------
C   Variation on RCOPY
C-----------------------------------------------------------------------
      INTEGER   INUM
      REAL      RVALS(*), RVAL(*)
C
      INTEGER   I
C-----------------------------------------------------------------------
      DO 10 I = 1,INUM
         RVAL(I) = RVALS(I)
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE JTIME (JDATE, LPART)
C-----------------------------------------------------------------------
C   Determines the duration time between Jan 1, 1979 and JDATE in the
C   form (YY,MM,DD,HH,MM,SS).  The difference is converted into units of
C   4 seconds and further converted into base 26 number with a=0, b=1,
C   c=2, ..., y=24, z=25.  This conversion is done in subroutine ALSTR.
C-----------------------------------------------------------------------
      INTEGER   JDATE(6)
      CHARACTER LPART*6
C
      INTEGER   ZDATE(6)
      DOUBLE PRECISION JDAY1, JDAY2, JDIFF
      DATA  ZDATE / 79, 1, 1, 0, 0, 0 /
C-----------------------------------------------------------------------
      CALL DAT2JD (ZDATE, JDAY1)
      CALL DAT2JD (JDATE, JDAY2)
      JDIFF = (JDAY2 - JDAY1) * 21600.0D0
C
      CALL ALSTR (JDIFF, LPART)
C
 999  RETURN
      END
      SUBROUTINE ALSTR (JDAY, TSTR)
C-----------------------------------------------------------------------
C   This subroutine converts JDAY into a six digit number to the base
C   26.
C-----------------------------------------------------------------------
      DOUBLE PRECISION JDAY
      CHARACTER TSTR*6
C
      DOUBLE PRECISION TDAY, F26(6)
      INTEGER   I, ITEMP
      CHARACTER FLET*26
      DATA FLET /'abcdefghijklmnopqrstuvwxyz'/
C-----------------------------------------------------------------------
      DO 100 I = 1,6
         F26(7-I) = 26.0 ** (I-1)
 100     CONTINUE
      TDAY = JDAY
      DO 200 I = 1,6
         ITEMP = TDAY / F26(I)
         TSTR(I:I) = FLET(ITEMP+1:ITEMP+1)
         TDAY = TDAY - ITEMP * F26(I)
 200     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE ANTIFO (INTAB, SUBARR, ANAME, IERR)
C-----------------------------------------------------------------------
C   Returns the names of the antennas in a given subarray with
C   associated table INTAB.
C   subarray at a given time.
C   Inputs:
C      INTAB   C*?  Input table object (assumed to have associated AN
C                   and SU tables)
C      SUBARR  I    Subarray number 0=> 1
C   Output:
C      ANAME   C(*)*8 Antenna names
C      IERR    I    Return code, O=OK, else failed.
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*), ANAME(*)*8
      INTEGER SUBARR, IERR
C
      CHARACTER ANTAB*36, COLLAB(2)*24, CDUMMY*1
      INTEGER   NROW, ANTNO, I, COLS(2), TYPE, DIM(3), DUMMY
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
      DATA COLLAB /'NOSTA', 'ANNAME'/
C-----------------------------------------------------------------------
      IERR = 0
C                                        Make AN table object
      ANTAB = 'AN table'
      CALL OCOPY (INTAB, ANTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Table type
      DIM(1) = 2
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (ANTAB, 'TBLTYPE', OOACAR, DIM, DUMMY, 'AN', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Version
      DIM(1) = 1
      CALL OPUT (ANTAB, 'VER', OOAINT, DIM, SUBARR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open AN table
      CALL OOPEN (ANTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Find column numbers
      CALL TABCOL (ANTAB, 2, COLLAB, COLS, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.10)) GO TO 999
C                                        Make sure all columns found
      DO 10 I = 1,2
         IF (COLS(I).LE.0) THEN
            MSGTXT = 'AN TABLE MISSING COLUMN ' // COLLAB(I)
            CALL MSGWRT (9)
            IERR = 7
            END IF
 10      CONTINUE
      IF (IERR.NE.0) GO TO 999
C                                       Get number of entries
      CALL OGET (ANTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Loop over table
      DO 100 I = 1,NROW
C                                       Read data for this row
         CALL TABDGT (ANTAB, I, COLS(1), TYPE, DIM, ANTNO, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         ANAME(ANTNO) = ' '
         CALL TABDGT (ANTAB, I, COLS(2), TYPE, DIM, DUMMY, ANAME(ANTNO),
     *      IERR)
         IF (IERR.NE.0) GO TO 999
 100     CONTINUE
C                                       Close AN table
         CALL OCLOSE (ANTAB, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Delete AN object
         CALL DESTRY (ANTAB, IERR)
         IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
