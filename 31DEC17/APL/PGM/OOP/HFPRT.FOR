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
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INVERS',
C           6         7        8          9          10
     *   'PRTLEV', 'DOCRT', 'OUTPRINT', 'BCOUNT', 'ECOUNT'/
C                                       Adverb data types (PAOOF.INC)
C                     1      2       3       4       5
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOAINT,
C           6      7      8       9       10
     *   OOAINT, OOARE, OOACAR, OOAINT, OOAINT/
C                                       Adverb dimensions (as 2D)
C                   1     2     3     4     5
      DATA AVDIM /12,1,  6,1,  1,1,  1,1,  1,1,
C         6     7     8     9     10
     *   1,1,  1,1, 48,1,  1,1,  1,1/
LOCAL END
      PROGRAM HFPRT
C-----------------------------------------------------------------------
C! Convert HF table to binary table for geodetic program SOLVE
C# Calibration VLBI OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999, 2004, 2007, 2009, 2015, 2017
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
C   Paraform AIPS OOP task processing a table.  This version HFPRT
C   converts the HF table into a binary table for input into the
C   geodetic package Solve.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, INTAB*36
      INTEGER  IRET, BUFF1(256)
      DATA PRGM /'HFPRT '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL TABTIN (PRGM, INTAB, IRET)
C                                       Process table
      IF (IRET.EQ.0) CALL TABTAB (INTAB, IRET)
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE TABTIN (PRGN, INTAB, IRET)
C-----------------------------------------------------------------------
C   TABTIN gets input parameters for HFPRT and creates the input object.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IRET    I    Error code: 0 => ok
C                               1=>  errpr
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, INTAB*36
C
      INTEGER   NKEY1

C                                       NKEY1=no. adverbs to copy to
C                                       INTAB
      PARAMETER (NKEY1=10)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32
C                                       for reading to EOF, index file
      INTEGER   DIM(3), DUMMY
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to INTAB
C                   1         2          3        4         5
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INVERS',
C           6         7        8           9         10
     *   'PRTLEV', 'DOCRT', 'OUTPRINT', 'BCOUNT', 'ECOUNT'/
C                                       May rename adverbs to INTAB
C                    1       2        3        4       5
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'VER',
C           6         7        8        9         10
     *   'PRTLEV', 'DOCRT', 'PRINT', 'BCOUNT', 'ECOUNT' /
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
C                                       Define as HF table
      DIM(1) = 2
      DIM(2) = 1
      DIM(3) = 1
      CALL OPUT (INTAB, 'TBLTYPE', OOACAR, DIM, DUMMY, 'HF', IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE TABTAB (INTAB, IERR)
C-----------------------------------------------------------------------
C   Convert table.
C   Inputs:
C      INTAB   C*   Name of input table object.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER INTAB*(*)
      INTEGER   IERR
C                                       MAXSIZ = max table entry size as
C                                       reals or characters
      INTEGER   MAXSIZ
      PARAMETER (MAXSIZ = 5000)
      INTEGER   IROW, OROW, NROW, ICOL, NCOL, BC, EC, TYPE, DIM(3)
      INTEGER   IDUM, LUN, PRTLEV
      REAL      RVALS(MAXSIZ)
      CHARACTER CVALS*(MAXSIZ*4), CDUMMY*1, OFILE*48
      INTEGER   IVALS(MAXSIZ)
      DOUBLE PRECISION DVALS(MAXSIZ/2)
      COMMON /HFTST/ RVALS
      COMMON /HFTSTC/ CVALS
      EQUIVALENCE  (RVALS(1), DVALS(1), IVALS(1))
C
      INTEGER   OUTLUN, FIND, NC, NLINE, IPAGE
      REAL      DOCRT
      CHARACTER*132  KLINE, T1, T2, SCRATC
C                                       Broke common block up into two
C                                       parts: character and numeric.
      COMMON /PRTHFC/ KLINE, T1, T2, SCRATC
      COMMON /PRTHFN/ OUTLUN, FIND, NC, NLINE, IPAGE, DOCRT
C
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
C                                       Get print level
      CALL OGET (INTAB, 'PRTLEV', TYPE, DIM, PRTLEV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      BC = MIN (MAX (BC, 1), NROW)
C                                       Get output file name
      CALL OGET (INTAB, 'PRINT', TYPE, DIM, IDUM, OFILE, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (INTAB, 'DOCRT', TYPE, DIM, DOCRT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open output file
      CALL LPOPEN (OFILE, DOCRT, OUTLUN, FIND, NC, SCRATC, IERR)
      IF (IERR.EQ.0) GO TO 30
      IF (IERR.NE.5) GO TO 999
C                                       File exists
         WRITE (MSGTXT,1020) OFILE
 1020    FORMAT ('Will add on to ', A48)
         CALL MSGWRT(1)
C                                       New file
C                                       Column Headings
 30   IF (PRTLEV.LE.0) THEN
         PRTLEV = 0
         WRITE (T1,1191)
         WRITE (T2,1192)
         KLINE = ' '
      ELSE IF (PRTLEV.EQ.1) THEN
         WRITE (T1,1194)
         WRITE (T2,1195)
         KLINE = ' '
      ELSE
         PRTLEV = 2
         WRITE (T1,1198)
         WRITE (T2,1199)
         KLINE = ' '
         END IF
      IPAGE = 0
      NLINE = 990
C                                       header
      IF (DOCRT.GT.-2.5) THEN
         CALL PRTLIN(OUTLUN, FIND, DOCRT, NC, T1, T2, KLINE, NLINE,
     *      IPAGE, SCRATC, IERR)
         IF (IERR.NE.0) GO TO 900
      ELSE
         CALL PRTLIN(OUTLUN, FIND, DOCRT, NC, T1, T2, T1, NLINE,
     *      IPAGE, SCRATC, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL PRTLIN(OUTLUN, FIND, DOCRT, NC, T1, T2, T2, NLINE,
     *      IPAGE, SCRATC, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       Loop over selected rows.
      OROW = 0
      DO 100 IROW = BC,EC
         OROW = OROW + 1
         DO 50 ICOL = 1,NCOL
            CALL TABDGT (INTAB, IROW, ICOL, TYPE, DIM, RVALS, CVALS,
     *         IERR)
            IF (IERR.NE.0) GO TO 900
C                                       Conversion Routine
            CALL CNSLV (PRTLEV, IROW, NCOL, ICOL, DIM(1), IERR)
            IF (IERR.NE.0) GO TO 900
 50         CONTINUE
 100     CONTINUE
C                                       Close tables
 900  CALL LPCLOS (LUN, FIND, NLINE, IERR)
      CALL OCLOSE (INTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
C----------------------------------------------------------------------
 1191 FORMAT ('         SUMMARY OF HF FILES')
 1192 FORMAT ('   HF#  SOURCE  BL B Q  yr mo dy hr mn sc')
 1194 FORMAT ('        OBS. TIME                       ',
     *  '               I-------RESIDIALS----------',
     *  'I------------------TOTALS------------------------I')
 1195 FORMAT ('   HF# DY HR MN SC INT  SOURCE  BL B Q NC',
     *  '   AMP    SNR   RATE     SBD     MBD  PHS',
     *  '       RATE           SBD           MBD        PHS')
 1198 FORMAT (' DUMP OF HF FILES')
 1199 FORMAT ('                 ')
      END
      SUBROUTINE CNSLV (PRTLEV, IROW, NCOL, ICOL, INUM, IERR)
C-----------------------------------------------------------------------
C   This subroutine converts the HF data files into a binary form which
C   is written in a binary form for input into the geodetic SOLVE
C   program.  Most of the conversion is done with EQUIVALENCE statments.
C-----------------------------------------------------------------------
C                                       General specifications
      INTEGER   IROW, NCOL, ICOL, INUM, IERR, PRTLEV
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
      INTEGER*2 IDR(768)
      CHARACTER C1*2
      INTEGER*2 I2(6), I3(6), I4(4), I9(28), I10(56), I12(6)
      INTEGER*2 I5, I6, I7, I8, I11, I13, I14, I15, I16, I17
C
      EQUIVALENCE (IDR(1), H0(1)), (IDR(129), H1(1)),
     .       (IDR(257), H2(1)), (IDR(385), H3(1)),
     .       (IDR(513), H4(1)), (IDR(641), H5(1))
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
      CHARACTER*132  KLINE, T1, T2, SCRATC
      INTEGER   OUTLUN, FIND, NC, NLINE, IPAGE
      REAL      DOCRT
C                                       Broke common block up into two
C                                       parts: character and numeric.
      COMMON /PRTHFC/ KLINE, T1, T2, SCRATC
      COMMON /PRTHFN/ OUTLUN, FIND, NC, NLINE, IPAGE, DOCRT
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
      IF (ICOL.EQ.74) R74 = RVALS(1)
      IF (ICOL.EQ.75) R75 = RVALS(1)
      IF (ICOL.EQ.76) R76 = RVALS(1)
      IF (ICOL.EQ.77) R77 = RVALS(1)
C                                       Loop for more
      IF (ICOL.GE.NCOL) THEN
C                                       Print out
C                                       Fix year, if necessary
         IF (I2(1).GT.100) I2(1)=I2(1)-1900
C                                       Other changes
         IF (R68.GT.180.0) R68 = R68 - 360.0
         IF (R68.LT.-180.0) R68 = R68 + 360.0
C                                       Begin printout
         IF (PRTLEV.EQ.0) THEN
            WRITE (KLINE,1100) IROW, C25, C1, C34, C33, I2
            CALL PHFPRT (IERR)
         ELSE IF (PRTLEV.EQ.1) THEN
            WRITE (T1(20:25), '(2I3)') I2(1), I2(2)
            WRITE (KLINE,1200) IROW, I2(3), I2(4), I2(5), I2(6),
     *         NINT(R73), C25, C1, C34, C33, I8, R61, NINT(R65),
     *         D48*1.0D6, R64*1.0D3, D47*1.0D3, NINT(R69), D42, D43,
     *         D41, NINT(R68)
            CALL PHFPRT (IERR)
         ELSE IF (PRTLEV.EQ.2) THEN
            CALL READHF (IDR, IERR)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (I6, 1X, A8, 1X, A2, 1X, A1, 1X, A1, 1X, 6I3)
 1200 FORMAT (I6, 4I3, I4, 1X,A8, 1X,A2, 1X,A1, 1X,A1, I3, E9.2, I4,
     *   F7.2, F8.1, F8.3, I5, F14.9, F14.6, F17.9, I5)
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
      SUBROUTINE PHFPRT (IERR)
C-----------------------------------------------------------------------
C     Subroutine PHFPRT is the printing command
C-----------------------------------------------------------------------
      INTEGER  IERR
      CHARACTER*132  KLINE, T1, T2, SCRATC
      INTEGER   OUTLUN, FIND, NC, NLINE, IPAGE
      REAL      DOCRT
C                                       Broke common block up into two
C                                       parts: character and numeric.
      COMMON /PRTHFC/ KLINE, T1, T2, SCRATC
      COMMON /PRTHFN/ OUTLUN, FIND, NC, NLINE, IPAGE, DOCRT
C-----------------------------------------------------------------------
      CALL PRTLIN (OUTLUN, FIND, DOCRT, NC, T1, T2, KLINE, NLINE, IPAGE,
     *   SCRATC, IERR)
C
 999  RETURN
      END
      SUBROUTINE READHF (IDM, IERR)
C-----------------------------------------------------------------------
C     Program READHF reads the HF Haystack format
C
C     Is IROW used in this routine?  I don't think so...
C-----------------------------------------------------------------------
      INTEGER   IERR, I
      INTEGER*2 IDR(768), IDM(768)
      INTEGER*2 H0(128), H1(128), H2(128), H3(128), H4(128), H5(128)
      INTEGER*2 CORNO, UTCTG(6), VLBUTC(6), IARCH(4), RECSTP(3),
     .          NOFAP(2,14), RECTRK(2,2,14), CORLVR, UTCMTG(6), DELFLG,
     .          RATFLG, DLTFLG, NDELS, NRATS, NAMBIG, VLB1X(2,14),
     .          CALFRQ(3,2,14), PRCUTC(2,14), ERRATE(2,2,14), CORCD,
     .          IFRERR, AUTOED, NCHAN, INDXNM(2,14)
      INTEGER*2 SOURCE(4), BASE(4,2), VLB1FL(3), TAPID(4,2),
     .          VLB2PG(3), RUNCOD(4), QALCOD, FRQGRP, ORIGFL(3),
     .          TPQCOD(3), OCCPNM(4,2)
      REAL   AMPBFQ(2,14), PHASCL(2), DELRES, DELSIG,
     .       RATRES, RATSIG, COHRCR, TOTPHS, UVFSEC(2), STRELV(2),
     .       INCHMP, URVR(2), SRCHPR(6), SBRES, SNRATO, FLSEDT,
     .       INCOH2, GCPHAS, GCRSPH, STRTSC, STOPSC, DLTEPO,
     .       EFFDUR, SBSIG, QBFACT, DISCRD, TOTPCT
      REAL*8 RFRQ(14), REFREQ, DELOBS, RATOBS, SBDEL, GPAMB,
     .       CLKFST, RFCLKR, DLOBSM, RATBSM, DLEPO1, DLEPO2
      EQUIVALENCE (IDR(2),CORNO),(IDR(3),CORCD),    (IDR(7),UTCTG),
     .       (IDR(13),VLBUTC),  (IDR(19),IARCH),   (IDR(23),RECSTP),
     .       (IDR(26),NCHAN),    (IDR(27),NOFAP),   (IDR(55),RECTRK),
     .       (IDR(111),CORLVR),  (IDR(112),UTCMTG), (IDR(119),DELFLG)
      EQUIVALENCE (IDR(120),RATFLG),(IDR(121),DLTFLG),(IDR(122),NDELS),
     .       (IDR(123),NRATS),   (IDR(124),NAMBIG), (IDR(135),VLB1X),
     .       (IDR(163),CALFRQ),  (IDR(263),PRCUTC), (IDR(291),ERRATE),
     .       (IDR(347),INDXNM),  (IDR(375),IFRERR), (IDR(376),AUTOED)
      EQUIVALENCE  (IDR(391),SOURCE),(IDR(395),BASE),(IDR(403),VLB1FL),
     .       (IDR(406),TAPID),   (IDR(414),VLB2PG),(IDR(417),RUNCOD),
     .       (IDR(421),QALCOD), (IDR(422),FRQGRP), (IDR(423),ORIGFL),
     .       (IDR(426),TPQCOD), (IDR(433),OCCPNM), (IDR(517),RFRQ)
      EQUIVALENCE (IDR(573),REFREQ),(IDR(577),DELOBS),(IDR(581),RATOBS),
     .       (IDR(585),SBDEL),   (IDR(589),GPAMB),  (IDR(593),CLKFST),
     .       (IDR(597),RFCLKR), (IDR(601),DLOBSM),(IDR(605),RATBSM),
     .       (IDR(609),DLEPO1),  (IDR(613),DLEPO2), (IDR(643),AMPBFQ)
      EQUIVALENCE(IDR(699),PHASCL),(IDR(703),DELRES),(IDR(705),DELSIG),
     .       (IDR(707),RATRES),  (IDR(709),RATSIG), (IDR(711),COHRCR),
     .       (IDR(713),TOTPHS), (IDR(715),UVFSEC),(IDR(719),STRELV),
     .       (IDR(723),INCHMP),(IDR(725),URVR),   (IDR(729),SRCHPR)
      EQUIVALENCE (IDR(741),SBRES),(IDR(743),SNRATO),(IDR(745),FLSEDT),
     .       (IDR(747),INCOH2),  (IDR(749),GCPHAS), (IDR(751),GCRSPH),
     .       (IDR(753),STRTSC), (IDR(755),STOPSC), (IDR(757),DLTEPO),
     .       (IDR(759),EFFDUR),  (IDR(761),SBSIG),  (IDR(763),QBFACT),
     .       (IDR(765),DISCRD),  (IDR(767),TOTPCT)
      EQUIVALENCE (IDR(1), H0(1)), (IDR(129), H1(1)),
     .       (IDR(257), H2(1)), (IDR(385), H3(1)),
     .       (IDR(513), H4(1)), (IDR(641), H5(1))
C
      CHARACTER*132  KLINE, T1, T2, SCRATC
      INTEGER   OUTLUN, FIND, NC, NLINE, IPAGE
      REAL      DOCRT
C                                       Broke common block up into two
C                                       parts: character and numeric.
      COMMON /PRTHFC/ KLINE, T1, T2, SCRATC
      COMMON /PRTHFN/ OUTLUN, FIND, NC, NLINE, IPAGE, DOCRT
C-----------------------------------------------------------------------
      DO 20 I = 1, 768
         IDR(I) = IDM(I)
 20      CONTINUE
C
      WRITE (KLINE,2001) CORNO
      CALL PHFPRT (IERR)
      WRITE (KLINE,2002) CORCD
      CALL PHFPRT (IERR)
      WRITE (KLINE,2003) UTCTG
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2004) VLBUTC
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2005) IARCH
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2006) RECSTP
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2007) NCHAN
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2008)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (NOFAP(1,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (NOFAP(2,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2009)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (RECTRK(1,1,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (RECTRK(2,1,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (RECTRK(1,2,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (RECTRK(2,2,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2010) CORLVR
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2011) UTCMTG
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2012) DELFLG
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2013) RATFLG
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2014) DLTFLG
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2015) NDELS
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2016) NRATS
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2017) NAMBIG
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2018)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (VLB1X(1,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (VLB1X(2,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2019)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (CALFRQ(1,1,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (CALFRQ(1,2,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (CALFRQ(2,1,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (CALFRQ(2,2,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (CALFRQ(3,1,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (CALFRQ(3,2,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2020)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (PRCUTC(1,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (PRCUTC(2,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2021)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (ERRATE(1,1,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (ERRATE(2,1,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (ERRATE(1,2,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (ERRATE(2,2,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2022) INDXNM
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (INDXNM(1,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20081) (INDXNM(2,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2023) IFRERR
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2024) AUTOED
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2025) SOURCE
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2026) BASE
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2027) VLB1FL
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2028) TAPID
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2029) VLB2PG
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2030) RUNCOD
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2031) QALCOD
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2032) FRQGRP
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2033) ORIGFL
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2034) TPQCOD
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2035) OCCPNM
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2036)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20361) (RFRQ(I), I = 1,7)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20361) (RFRQ(I), I = 8,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2037) REFREQ
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2038) DELOBS
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2039) RATOBS
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2040) SBDEL
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2041) GPAMB
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2042) CLKFST
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2043) RFCLKR
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2044) DLOBSM
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2045) RATBSM
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2046) DLEPO1
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2047) DLEPO2
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2048)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20481) (AMPBFQ(1,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,20482) (AMPBFQ(2,I), I = 1,14)
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2049) PHASCL
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2050) DELRES
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2051) DELSIG
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2052) RATRES
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2053) RATSIG
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2054) COHRCR
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2055) TOTPHS
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2056) UVFSEC
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2057) STRELV
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2058) INCHMP
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2059) URVR
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2060) SRCHPR
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2061) SBRES
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2062) SNRATO
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2063) FLSEDT
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2064) INCOH2
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2065) GCPHAS
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2066) GCRSPH
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2067) STRTSC
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2068) STOPSC
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2069) DLTEPO
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2070) EFFDUR
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2071) SBSIG
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2072) QBFACT
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2073) DISCRD
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
      WRITE (KLINE,2074) TOTPCT
      IF (IERR.EQ.0) CALL PHFPRT (IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 2001 FORMAT (' corno: baseline number:      ', I4)
 2002 FORMAT (' corcd: baseline code  :      ', 4X, A2)
 2003 FORMAT (' utctg: Y,M,D,H,M,S  UT Epoch for day and rate: ',
     *   I5,5I3)
 2004 FORMAT (' vlb2utc: Y,M,D,H,M,S UT for fringe processing: ',
     *   I5,5I3)
 2005 FORMAT (' iarch: archive ? ', 4I4)
 2006 FORMAT (' recstp: sample rat in kbytes/sec: ', 3I6)
 2007 FORMAT (' nchan: number of channels: ', I6)
 2008 FORMAT (' nofap: acc. periods by sideband/channel: ')
20081 FORMAT (20X, 14I6)
 2009 FORMAT (' rectrk: track table: ')
 2010 FORMAT (' corlvr: correlator version.  772 means IEEE fp:',I5)
 2011 FORMAT (' utcmtg: Y,M,D,H,M,S UT for central epoch: ', I5,5I3)
 2012 FORMAT (' delfig: reference tape drive: ', I6)
 2013 FORMAT (' ratfig: remote tape drive:    ', I6)
 2014 FORMAT (' deltfig: correlator option:   ', I6)
 2015 FORMAT (' ndels: delay rate offset:     ', I6)
 2016 FORMAT (' nrats: ?                      ', I6)
 2017 FORMAT (' nambig: ?                     ', I6)
 2018 FORMAT (' vlb1x:  ? ')
 2019 FORMAT (' calfrq: phasecal (amp, phase, freq)  ')
 2020 FORMAT ( ' prcutc: correlator processing time YMMDD ')
 2021 FORMAT (' errate: error rate (1000*logT)  ')
 2022 FORMAT (' indxnm: correlator index: ')
 2023 FORMAT (' ifrerr: frnge error code? ', I6)
 2024 FORMAT (' autoed: ? ', I6)
 2025 FORMAT (' source: star id                    ', 4A2)
 2026 FORMAT (' base: baseline names               ', 8A2)
 2027 FORMAT (' vlb1fil: correlated data file name ', 3A2)
 2028 FORMAT (' tapid: raw data tape names         ', 8A2)
 2029 FORMAT (' vlb2prg: Fringe ver data YYMMDD:   ', 3A2)
 2030 FORMAT (' runcod: Fringe run code:           ', 4A2)
 2031 FORMAT (' qualcod: quality code              ', A2)
 2032 FORMAT (' frqgrp: frequency group            ', A2)
 2033 FORMAT (' origfil: original file name?       ', 3A2)
 2034 FORMAT (' tapqcod: tape Q code?              ', 3A2)
 2035 FORMAT (' occprn: ref and remote occup code? ', 8A2)
 2036 FORMAT (' rfrq: LO frequencies (MHz)  ')
20361 FORMAT (7D18.9)
 2037 FORMAT (' refreq: reference LO frequency (MHz)  ', D22.14)
 2038 FORMAT (' delobs: observed group delay (usec)   ', D22.14)
 2039 FORMAT (' ratobs: observed delay rate (usec/sec)', D22.14)
 2040 FORMAT (' sbdel: narrow band delay (usec)       ', D22.14)
 2041 FORMAT (' gpamb: group delay ambiguity (usec)   ', D22.14)
 2042 FORMAT (' clkofst: apriori clock (usec)         ', D22.14)
 2043 FORMAT (' rfclker: reference station clock epoch', D22.14)
 2044 FORMAT (' delobsm: observed delay at cen ep     ', D22.14)
 2045 FORMAT (' ratobsm: observed delay rate cen ep   ', D22.14)
 2046 FORMAT (' dlepo1: phase delay at EPO+1 sec      ', D22.14)
 2047 FORMAT (' dlepo2: phase delay at EPO-1 sec      ', D22.14)
 2048 FORMAT (' ampbfrq: observed ampl and phase ')
20481 FORMAT (14(1PE9.2))
20482 FORMAT (14F9.1)
 2049 FORMAT (' phasecl: phase-cal rate by station ',2E17.9)
 2050 FORMAT (' delres: group delay residual       ',E17.9)
 2051 FORMAT (' delsig: group delay error          ',E17.9)
 2052 FORMAT (' ratres: delay rate residual        ',E17.9)
 2053 FORMAT (' ratsig: delay rate error           ',E17.9)
 2054 FORMAT (' cohrcr: coherence percent          ',E17.9)
 2055 FORMAT (' totphas: total obs fringe phase    ',E17.9)
 2056 FORMAT (' uvfasec: fringes/arcsec (ns,ew)    ',2E17.9)
 2057 FORMAT (' starelv: star elevations (deg)     ',2E17.9)
 2058 FORMAT (' incohamp: incoherent fringe ampl   ',E17.9)
 2059 FORMAT (' urvr: UV rate deriv mHz/arcsec     ',2E17.9)
 2060 FORMAT (' srchpar: fringe search parameters  ',6E15.7)
 2061 FORMAT (' sbres: avg sgl band delay (usec)   ',E17.9)
 2062 FORMAT (' snrato: SNR in sigmas              ',E17.9)
 2063 FORMAT (' falsedt: prob of false detection   ',E17.9)
 2064 FORMAT (' incoh2: incoh segment fringe ampl? ',E17.9)
 2065 FORMAT (' gcphas: toTAL phase at earth center',E17.9)
 2066 FORMAT (' gcresphs: res phase at earth cen   ',E17.9)
 2067 FORMAT (' startsc: start time (sec past hour)',E17.9)
 2068 FORMAT (' stopsc: stop time (sec past hour)  ',E17.9)
 2069 FORMAT (' deltepo: epoch offs from centr run ',E17.9)
 2070 FORMAT (' effdur: effective run dur (sec)    ',E17.9)
 2071 FORMAT (' sbsig: sngl ban delay error (usec) ',E17.9)
 2072 FORMAT (' qbfact: ratio of min/max data      ',E17.9)
 2073 FORMAT (' discrd: percent data discarded     ',E17.9)
 2074 FORMAT (' totpcnt: total phase at cent epoch ',E17.9)
      END
