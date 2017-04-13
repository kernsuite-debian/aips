LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLN.INC'
      INTEGER   NPARMS
      PARAMETER (NPARMS=78)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                     1         2          3        4
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
C           5          6         7          8           9
     *   'OUTNAME', 'OUTSEQ', 'OUTDISK', 'OUT2NAME', 'OUT2CLAS',
C           10         11          12          13        14
     *   'OUT2SEQ', 'OUT2DISK', 'CELLSIZE', 'IMSIZE', 'NFIELD',
C           15          16         17         18          19
     *   'DO3DIMAG', 'FLDSIZE', 'RASHIFT', 'DECSHIFT', 'UVTAPER',
C           20         21        22        23        24       25
     *   'UVRANGE', 'UVWTFN', 'UVSIZE', 'ROBUST', 'UVBOX', 'UVBXFN',
C           26        27        28       29       30       31
     *   'ROTATE', 'ZEROSP', 'XTYPE', 'YTYPE', 'XPARM', 'YPARM',
C           32       33        34       35         36          37
     *   'GUARD', 'NBOXES', 'CLBOX', 'BOXFILE', 'OBOXFILE', 'IM2PARM',
C           38      39      40          41       42,     43     44
     *   'GAIN', 'FLUX', 'MINPATCH', 'NITER', 'BMAJ', 'BMIN', 'BPA',
C           45         46         47        48          49         50
     *   'OVERLAP', 'ONEBEAM', 'FACTOR', 'MAXPIXEL', 'CMETHOD', 'NMAPS',
C           51        52        53         54       55         56
     *   'SMODEL', 'REFANT', 'SOLINT',  'APARM', 'SOLTYPE', 'SOLMODE',
C           57        58       59         60      61          62
     *   'SOLCON', 'ANTWT', 'GAINERR', 'WTUV', 'WEIGHTIT', 'DOCALIB',
C           63         64       65       66       67         68
     *   'GAINUSE', 'DOPOL', 'PDVER', 'BLVER', 'FLAGVER', 'OUTFGVER',
C           69        70       71        72      73       74
     *   'DOBAND', 'BPVER', 'SMOOTH', 'DOTV', 'LTYPE', 'DOWEIGHT',
C           75       76         77       78
     *   'DOTWO', 'BADDISK', 'SUBWT', 'SUBREFA'/
C                    1       2       3       4
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT,
C          5       6       7       8       9
     *   OOACAR, OOAINT, OOAINT, OOACAR, OOACAR,
C          10      11      12      13      14
     *   OOAINT, OOAINT, OOARE,  OOAINT, OOAINT,
C          15      16      17      18      19
     *   OOALOG, OOAINT, OOARE,  OOARE,  OOARE,
C          20      21      22      23      24      25
     *   OOARE,  OOACAR, OOAINT, OOARE, OOAINT, OOAINT,
C          26      27      28      29      31      31
     *   OOARE,  OOARE,  OOAINT, OOAINT, OOARE,  OOARE,
C          32      33      34      35      36      37
     *   OOARE,  OOAINT, OOAINT, OOACAR, OOACAR, OOARE,
C          38      39      40      41      42      43      44
     *   OOARE,  OOARE,  OOAINT, OOAINT, OOARE,  OOARE,  OOARE,
C          45      46      47      48      49      50
     *   OOAINT, OOALOG, OOARE,  OOAINT, OOACAR, OOAINT,
C          51      52      53      54      55      56
     *   OOARE,  OOAINT, OOARE,  OOARE,  OOACAR, OOACAR,
C          57      58      59      60      61      62
     *   OOARE,  OOARE,  OOARE,  OOARE,  OOAINT, OOARE,
C          63      64      65      66      67      68
     *   OOAINT, OOAINT, OOAINT, OOAINT, OOAINT, OOAINT,
C          69      70      71      72      73      74
     *   OOAINT, OOAINT, OOARE,  OOAINT, OOAINT, OOARE,
C          75      76      77      78
     *   OOALOG, OOAINT, OOARE,  OOAINT/
C                    1     2     3     4
      DATA AVDIM / 12,1,  6,1,  1,1,  1,1,
C         5      6     7     8     9
     *   12,1,  1,1,  1,1, 12,1,  6,1,
C          10    11    12    13    14
     *    1,1,  1,1,  2,1,  2,1,  1,1,
C          15    16             17        18    19
     *    1,1,  2,MAXAFL, MAXAFL,1, MAXAFL,1,  2,1,
C          20    21    22    23    24    25
     *    2,1,  2,1,  2,1,  1,1,  1,1,  1,1,
C          26    27    28    29    30    31
     *    1,1,  5,1,  1,1,  1,1, 10,1, 10,1,
C          32    33    34         35    36    37
     *    2,1,  1,1,  4,MXCLBX, 48,1, 48,1, 40,1,
C          38    39    40    41    42    43    44
     *    1,1,  1,1,  1,1,  1,1,  1,1,  1,1,  1,1,
C          45    46    47    48    49   50
     *    1,1,  1,1,  1,1,  1,1,  4,1,  1,1,
C          51    52    53    54    55    56
     *    7,1,  1,1,  1,1, 10,1,  4,1,  4,1,
C          57    58    59    60    61    62
     *    1,1, 30,1, 30,1,  1,1,  1,1,  1,1,
C          63    64    65    66    67    68
     *    1,1,  1,1,  1,1,  1,1,  1,1,  1,1,
C          69    70    71    72    73    74
     *    1,1,  1,1,  3,1,  1,1,  1,1,  1,1,
C          75    76    77    78
     *    1,1, 10,1, 10,1, 10,1/
LOCAL END
LOCAL INCLUDE 'INPUTT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLN.INC'
      INTEGER   NKEYT
C                                       NKEYT = no adverbs for CLEAN
C                                       TELL
      PARAMETER (NKEYT=24)
      INTEGER   TELDIM(2,NKEYT), TELTYP(NKEYT)
      CHARACTER TELK(NKEYT)*8
LOCAL END
LOCAL INCLUDE 'INPUTTATA.INC'
C                                       Adverbs to get via TELL
C                   1           2          3       4       5
      DATA TELK /'OBOXFILE', 'IM2PARM', 'GAIN', 'FLUX', 'MINPATCH',
C           6        7       8       9      10        11         12
     *   'NITER', 'BMAJ', 'BMIN', 'BPA', 'FACTOR', 'MAXPIXEL', 'NMAPS',
C           13        14        15         16         17
     *   'REFANT', 'SOLINT', 'SOLTYPE', 'SOLMODE', 'SOLCON',
C           18       19         20      21          22      23
     *   'ANTWT', 'GAINERR', 'WTUV', 'WEIGHTIT', 'DOTV', 'DOWEIGHT',
C           24
     *   'DOTWO'/
C                    1       2       3       4       5
      DATA TELTYP /OOACAR, OOARE,  OOARE,  OOARE,  OOAINT,
C          6       7       8       9       10      11      12
     *   OOAINT, OOARE,  OOARE,  OOARE,  OOARE,  OOAINT, OOAINT,
C          13      14      15      16      17
     *   OOAINT, OOARE,  OOACAR, OOACAR, OOARE,
C          18      19      20      21      22      23      24
     *   OOARE,  OOARE,  OOARE,  OOAINT, OOAINT, OOARE,  OOALOG/
C                     1     2    3    4    5    6
      DATA TELDIM / 48,1, 40,1, 1,1, 1,1, 1,1,
C         6    7    8    9    10   11   12
     *   1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1,
C         13   14   15   16   17
     *   1,1, 1,1, 4,1, 4,1, 1,1,
C          18    19   20   21   22   23   24
     *   30,1, 30,1, 1,1, 1,1, 1,1, 1,1, 1,1/
LOCAL END
LOCAL INCLUDE 'IMAGWIN.INC'
      LONGINT   WINP
      INTEGER   WINM, WINW, WIN(2)
      COMMON /IMAWIN/ WINP, WINM, WINW, WIN
LOCAL END
      PROGRAM SCIMG
C-----------------------------------------------------------------------
C! Imaging plus self calibration loop
C# Task AP Imaging calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2007, 2009-2010, 2012-2015
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
C   Self calibration - image - CLEAN loop
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, CLEAN*32, UVIN*32, SNTAB*32, UVSCR*32, UVOUT*32
      INTEGER   IRET, BUFF1(256)
      INCLUDE 'IMAGWIN.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'SCIMG'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL SCMIN (PRGM, CLEAN, UVIN, SNTAB, UVSCR, UVOUT, IRET)
C                                       CLEAN
      IF (IRET.EQ.0) CALL SCLOOP (CLEAN, UVIN, SNTAB, UVSCR, UVOUT,
     *   IRET)
C                                       History
      IF (IRET.EQ.0) CALL SCMHI (UVIN, UVOUT, CLEAN)
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE SCMIN (PRGN, CLEAN, UVIN, SNTAB, UVSCR, UVOUT, IERR)
C-----------------------------------------------------------------------
C   SCMIN gets input parameters for SCIMG and creates the various
C   objects.
C   Inputs:
C      PRGN    C*6    Program name
C   Output:
C      CLEAN   C*32   Name of CLEAN object
C      UVIN    C*32   Input uv object
C      SNTAB   C*32   SN table object, associated with input uv data.
C      UVOUT   C*32   Output uv object
C      UVSCR   C*32   Scratch uv object.  Necessary adverbs for
C                     controling self calibration are copied.
C      IERR    I      Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER PRGN*6, CLEAN*(*), UVIN*(*), SNTAB*(*), UVSCR*(*),
     *   UVOUT*(*)
C
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTT.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   NKEY1, NKEY2, NKEY3, NKEY4, NKEY5, MXSTAR
C                                       NKEY1=no. adverbs to copy to
C                                       CLEAN object
      PARAMETER (NKEY1=14)
C                                       NKEY2 = no. adverb for CLEANO
      PARAMETER (NKEY2=17)
C                                       NKEY3 = no. abverbs for BEAM
      PARAMETER (NKEY3=1)
C                                       NKEY4 = no. adverb for UVIN
      PARAMETER (NKEY4=26)
C                                       NKEY5 = no. adverbs for UVOUT
      PARAMETER (NKEY5=13)
C                                       max size of Stars
      PARAMETER (MXSTAR = 100)
C
      INTEGER   DIM(7), TYPE, BCHAN, ECHAN, BIF, EIF, IMSIZE(2,MAXFLD),
     *   NFIELD, NCHAV, MINNO, PRTLV, DISK, IMSEQ, IROUND, VER, BC, EC,
     *   BI, EI, SNVER, DUMMY, NPTWO, NITER, I, K, NBOXES(MAXFLD),
     *   FLDSIZ(2,MAXFLD), NXBEM(MAXFLD), NYBEM(MAXFLD), BFIELD,
     *   BCOMP(MAXFLD), MAXNX, MAXNY, MINNX, MINNY, IMSI(2), JLOCR,
     *   JLOCD, BMSIZ(2), NXFLD, NYFLD, NX, NY, IS, EDGEX, EDGEY,
     *   CLBOX(4,50), FGVERI, FGVERO, FGVERC, FGV, NWORDS, ILOCSU,
     *   JTRIM, NSTAR, TVGRCS(3), IDATE(3), ITIME(3)
      LONGINT   KP
      LOGICAL   AVGPOL, AVGIF, DOMGM, SWITCH, DOCAL, DO3DIM, INTROK,
     *   WTCLDO, ONEBEM, NEGOK
      REAL      APARM(10), SNRMIN, WTUV, RASH(MAXFLD), DECSH(MAXFLD),
     *   SMODEL(7), SIGMA, UVRA(2), BMAJ, BMIN, CELLS(2), DU, DV, UMAX,
     *   VMAX, GUARDB(2), GUAU, GUAV, UVTAPR(2), UTFACT(2), SMTIME,
     *   CROTAU(7), ROTATE, XDOCAL, IM2PRM(40), AUTOBX(6),
     *   STPARM(6,MXSTAR)
      DOUBLE PRECISION CRVAL(7), STPOS(2,MXSTAR)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, INK3(NKEY3)*8, OUTK3(NKEY3)*32, INK4(NKEY4)*8,
     *   OUTK4(NKEY4)*32, INK5(NKEY5)*8, OUTK5(NKEY5)*32, UVTYPE*2,
     *   BEAM(MAXFLD)*32, STOKES*4, CHTYPE*4, CLNTYP*8, NAME*12,
     *   CLASS*6, METH*4, SMOD*4, TINAME*12, TONAME*12, CDUMMY*1,
     *   TELOBJ*32, CLEANO(MAXFLD)*32, BOXFIL*48, FGNAME(4)*32, FGIN*32,
     *   OBXFIL*48
      INCLUDE 'IMAGWIN.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUTDATA.INC'
      INCLUDE 'INPUTTATA.INC'
C                                       Adverbs to copy to CLEAN object
C                    1       2       3           4        5
      DATA INK1 / 'GAIN', 'FLUX', 'MINPATCH', 'NITER', 'BPA',
C           6         7       8           9       10
     *   'FACTOR', 'DOTV', 'MAXPIXEL', 'DOTV', 'OBOXFILE',
C           11          12         13         14
     *   'DO3DIMAG', 'OVERLAP', 'ONEBEAM', 'LTYPE'/
C                                       Rename
C                     1       2          3           4        5
      DATA OUTK1 / 'GAIN', 'MINFLUX', 'MINPATCH', 'NITER', 'BEAM.BPA',
C           6         7          8          9          10
     *   'FACTOR', 'TVFIELD', 'MAXNRES', 'SCTVFLD', 'OBOXFILE',
C           11          12         13         14
     *   'DO3DIMAG', 'OVERLAP', 'ONEBEAM', 'LTYPE'/
C                                       Adverbs for CLEANO
C                    1         2         3          4           5
      DATA INK2 /'OUTNAME', 'OUTSEQ', 'OUTDISK', 'CELLSIZE', 'UVTAPER',
C           6         7        8         9         10
     *   'UVWTFN', 'UVBOX', 'UVSIZE', 'ROBUST', 'UVBXFN',
C           11        12       13       14       15       16
     *   'ZEROSP', 'XTYPE', 'YTYPE', 'XPARM', 'YPARM', 'DO3DIMAG',
C           17
     *   'ONEBEAM'/
C                    1       2       3        4           5
      DATA OUTK2 /'NAME', 'IMSEQ', 'DISK', 'CELLSIZE', 'WTTAPER',
C           6         7        8         9         10
     *   'UVWTFN', 'UVBOX', 'UVSIZE', 'ROBUST', 'UVBXFN',
C           11        12       13       14       15       16
     *   'ZEROSP', 'CTYPX', 'CTYPY', 'XPARM', 'YPARM', 'DO3DIMAG',
C           17
     *   'ONEBEAM'/
C                                       Adverbs for BEAM image object
      DATA INK3 /'CELLSIZE'/
      DATA OUTK3 /'CELLSIZE'/
C                                       Adverbs for UVIN
C                    1        2         3          4
      DATA INK4 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
C             5          6           7          8          9
     *   'OUT2NAME', 'OUT2CLAS', 'OUT2SEQ', 'OUT2DISK', 'NMAPS',
C           10         11         12         13
     *   'CMETHOD', 'GAINUSE', 'ZEROSP', 'FLAGVER',
C           14        15       16         17        18
     *   'REFANT', 'SUBWT', 'SUBREFA', 'ROTATE', 'SOLINT',
C           19          20       21       22       23        24
     *   'DOWEIGHT', 'DOTWO', 'DOPOL', 'BLVER', 'DOBAND', 'BPVER',
     *   'SMOOTH', 'PDVER'/
C                    1       2        3       4
      DATA OUTK4 /'NAME', 'CLASS', 'IMSEQ', 'DISK',
C            5          6           7         8          9
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK', 'NUMLOOP',
C            10        11               12               13
     *   'MODMETH', 'CALEDIT.CLUSE', 'ZEROSP', 'CALEDIT.FGVER',
C           14        15       16         17        18
     *   'REFANT', 'SUBWT', 'SUBREFA', 'ROTATE', 'SOLINT',
C            19         20         21               22
     *   'REWEIGHT', 'COMPARE', 'CALEDIT.DOPOL', 'CALEDIT.BLVER',
C            23                24
     *   'CALEDIT.DOBAND', 'CALEDIT.BPVER', 'CALEDIT.SMOOTH',
     *   'CALEDIT.PDVER'/
C                                       Adverbs for UVOUT (selfcal)
C                   1          2         3        4         5
      DATA INK5 /'REFANT', 'SOLINT', 'SOLTYPE', 'SOLMODE', 'SOLCON',
C           6         7        8       9         10
     *   'ANTWT', 'GAINERR', 'WTUV', 'SUBWT', 'SUBREFA', 'DOWEIGHT',
     *   'DOTWO', 'WEIGHTIT'/
C                   1          2         3         4         5
      DATA OUTK5 /'REFANT', 'SOLINT', 'SOLTYPE', 'SOLMODE', 'SOLCON',
C           6         7          8      9         10
     *   'ANTWT', 'GAINERR', 'WTUV', 'SUBWT', 'SUBREFA', 'REWEIGHT',
     *   'COMPARE', 'WEIGHTIT'/
      DATA FGNAME /'CALEDIT.FGVER', 'IN_FGVER', 'OUT_FGVER',
     *   'COP_FGVER'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL AV2INT (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IERR)
      IF (IERR.NE.0) GO TO 999
      RQUICK = .FALSE.
C                                       Default NFIELD=1
      CALL OGET ('Input', 'NFIELD', TYPE, DIM, NFIELD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF ((NFIELD.LE.0) .OR. (NFIELD.GT.MAXFLD)) THEN
         IF (NFIELD.LE.0) NFIELD = 1
         IF (NFIELD.GT.MAXFLD) NFIELD = MAXFLD
         CALL OPUT ('Input', 'NFIELD', TYPE, DIM, NFIELD, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       BADDISK
      CALL OGET ('Input', 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default output Name = input
      CALL OGET ('Input', 'INNAME', TYPE, DIM, DUMMY, TINAME, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'OUTNAME', TYPE, DIM, DUMMY, TONAME, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (TONAME.EQ.'    ') THEN
         CALL OPUT ('Input', 'OUTNAME', TYPE, DIM, DUMMY, TINAME, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       IMSIZE before it spreads
      CALL OGET ('Input', 'IMSIZE', TYPE, DIM, IMSI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IMSI(1) = MAX (64, IMSI(1))
      IMSI(2) = MAX (64, IMSI(2))
      IMSI(1) = NPTWO (IMSI(1))
      IMSI(2) = NPTWO (IMSI(2))
      CALL OPUT ('Input', 'IMSIZE', TYPE, DIM, IMSI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       NITER before it spreads
      CALL OGET ('Input', 'NITER', TYPE, DIM, NITER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (NITER.LE.0) THEN
         NITER = 300
         CALL OPUT ('Input', 'NITER', TYPE, DIM, NITER, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       3D imaging # beams
      CALL OGET ('Input', 'DO3DIMAG', TYPE, DIM, DO3DIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'ONEBEAM', TYPE, DIM, ONEBEM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       autoboxing
      CALL OGET ('Input', 'IM2PARM', TYPE, DIM, IM2PRM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      AUTOBX(1) = IROUND (IM2PRM(1))
      AUTOBX(2) = IM2PRM(2)
      AUTOBX(3) = IM2PRM(3)
      AUTOBX(4) = IM2PRM(4)
      AUTOBX(5) = IROUND (IM2PRM(5))
      AUTOBX(6) = IROUND (IM2PRM(6))
      AUTOBX(1) = MIN (50.0, AUTOBX(1))
      IF (AUTOBX(2).LE.1.5) AUTOBX(2) = 3.
      IF (AUTOBX(3).LT.AUTOBX(2)) AUTOBX(3) = AUTOBX(2) + 2.
      IF (AUTOBX(4).LT.0.01) AUTOBX(4) = 0.1
      IF (AUTOBX(4).GT.0.90) AUTOBX(4) = 0.1
      IF (AUTOBX(5).LT.-1.0) AUTOBX(5) = 1.0
      IF (AUTOBX(5).GT.6.0) AUTOBX(5) = 6.0
      IF (AUTOBX(6).LT.1.0) AUTOBX(6) = 5.0
      IF (AUTOBX(6).GT.IMSI(1)/20.0) AUTOBX(6) = 5.0
      CALL RCOPY (6, AUTOBX, IM2PRM)
      CALL OPUT ('Input', 'IM2PARM', OOARE, DIM, IM2PRM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Create CLEAN object
      CLEAN = 'CLEAN process object'
      CALL CREATE (CLEAN, 'CLEAN', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       OBOXFILE special usage
      CALL OGET ('Input', 'OBOXFILE', TYPE, DIM, DUMMY, OBXFIL, IERR)
      IF (IERR.NE.0) GO TO 999
      I = JTRIM (OBXFIL)
      IF ((I.GT.0) .AND. (OBXFIL(I:I).EQ.':')) THEN
         CALL ZDATE (IDATE)
         CALL ZTIME (ITIME)
         WRITE (OBXFIL(I+1:),1050) IDATE, ITIME
         CALL OPUT ('Input', 'OBOXFILE', TYPE, DIM, DUMMY, OBXFIL, IERR)
         IF (IERR.NE.0) GO TO 999
         I = JTRIM (OBXFIL)
         WRITE (MSGTXT,1051) OBXFIL(:I)
         CALL MSGWRT (2)
         END IF
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, CLEAN, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 1
C                                       temporary OBOXFILE
      IF (OBXFIL.EQ.' ') THEN
         CALL ZDATE (IDATE)
         CALL ZTIME (ITIME)
         WRITE (OBXFIL,1052) IDATE, ITIME
         DIM(1) = 48
         DIM(2) = 1
         CALL OPUT (CLEAN, 'OBOXFILE', OOACAR, DIM, DUMMY, OBXFIL, IERR)
         IF (IERR.NE.0) GO TO 999
         I = JTRIM (OBXFIL)
         WRITE (MSGTXT,1053) OBXFIL(:I)
         CALL MSGWRT (2)
         END IF
C                                       interaction always allowed
      INTROK = .NOT.RQUICK
      CALL OPUT (CLEAN, 'INTACTOK', OOALOG, DIM, INTROK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       plot labeling
      TVGRCS(1) = 5
      TVGRCS(2) = 7
      TVGRCS(3) = 6
      DIM(1) = 3
      CALL OPUT (CLEAN, 'TVGRCHAN', OOAINT, DIM, TVGRCS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Convert BMAJ, BMIN to degrees
      CALL OGET ('Input', 'BMAJ', TYPE, DIM, BMAJ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      BMAJ = BMAJ / 3600.0
      CALL OPUT (CLEAN, 'BEAM.BMAJ', TYPE, DIM, BMAJ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'BMIN', TYPE, DIM, BMIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      BMIN = BMIN / 3600.0
      CALL OPUT (CLEAN, 'BEAM.BMIN', TYPE, DIM, BMIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set CLEAN type
      CLNTYP = 'UV'
      DIM(1) = LEN (CLNTYP)
      DIM(2) = 1
      CALL OPUT (CLEAN, 'CLEANTYP', OOACAR, DIM, DUMMY, CLNTYP, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Create other objects
C                                       UVIN
      UVIN = 'Input UVdata'
      CALL CREATE (UVIN, 'UVDATA', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY4, INK4, OUTK4, UVIN, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Process IPOL
      STOKES = 'I'
      DIM(1) = LEN (STOKES)
      DIM(2) = 1
      CALL OPUT (UVIN, 'CALEDIT.STOKES', OOACAR, DIM, DUMMY, STOKES,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       See if restarting
C                                       DOCALIB
      CALL OGET ('Input', 'DOCALIB', TYPE, DIM, XDOCAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      WTCLDO = (XDOCAL.GT.0.0) .AND. (XDOCAL.LE.99.0)
      CALL OPUT (UVIN, 'CALEDIT.DOWTCL', OOALOG, DIM, WTCLDO, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      DOCAL = XDOCAL.GT.0.0
      CALL OPUT (UVIN, 'CALEDIT.DOCAL', OOALOG, DIM, DOCAL, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      IF (DOCAL) THEN
         CALL OGET ('Input', 'GAINUSE', TYPE, DIM, SNVER, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         SNVER = 0
         END IF
C                                       Point model for initial
C                                       calibration.
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (UVIN, 'MODDOPT', OOALOG, DIM, .FALSE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (.NOT.DOCAL) THEN
         CALL OGET ('Input', 'SMODEL', TYPE, DIM, SMODEL, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (SMODEL(1).GT.0.0) THEN
            DIM(1) = 1
            DIM(2) = 1
            CALL OPUT (UVIN, 'MODDOPT', OOALOG, DIM, .TRUE., CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            CALL OPUT (UVIN, 'MODPTFLX', OOARE, DIM, SMODEL(1), CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            CALL OPUT (UVIN, 'MODPTXOF', OOARE, DIM, SMODEL(2), CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            CALL OPUT (UVIN, 'MODPTYOF', OOARE, DIM, SMODEL(3), CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         END IF
C                                       Get IF channel selection
      CALL SECSLT (UVIN, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       All Stokes', IF, channels
      CALL SECSAV (UVIN, 1, 0, 1, 0, '    ', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open UVIN to be sure it's OK.
      CALL OOPEN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       make sure single-source
      CALL UVDFND (UVIN, 1, 'SOURCE', ILOCSU, IERR)
      IF (IERR.EQ.0) THEN
         MSGTXT = 'I WORK ONLY ON SINGLE-SOURCE FILES'
         CALL MSGWRT (8)
         IERR = 8
         GO TO 999
         END IF
      IF (IERR.EQ.1) IERR = 0
      IF (IERR.NE.0) GO TO 999
C                                       Check data type
      CALL UVDGET (UVIN, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (UVTYPE(:1).NE.'U') THEN
         MSGTXT = 'I DO NOT WORK FOR UV DATA OF TYPE ''' // UVTYPE //
     *      ''''
         IERR = 9
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       East-West not 3D
      IF ((UVTYPE.NE.'UV') .AND. (DO3DIM)) THEN
         IF (NFIELD.GT.MAXAFL) THEN
            MSGTXT = 'NFIELD > 64 NOT ALLOWED FOR -NCP DATA'
            CALL MSGWRT (7)
            IERR = 8
            GO TO 999
            END IF
         DO3DIM = .FALSE.
         DIM(1) = 1
         DIM(2) = 1
         CALL OPUT ('Input', 'DO3DIMAG', OOALOG, DIM, DO3DIM, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OPUT (CLEAN, 'DO3DIMAG', OOALOG, DIM, DO3DIM, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
      CALL OCLOSE (UVIN, IERR)
      IF (IERR.NE.0) GO TO 999
      BFIELD = NFIELD
      IF (ONEBEM) BFIELD = 1
C                                       Find highest FG table.
      FGIN = 'Temporary FG table for SCIMG'
      FGV = 1
      CALL UV2TAB (UVIN, FGIN, 'FG', FGV, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TBLHIV (FGIN, FGV, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABDES (FGIN, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set FLAGVER
      CALL OGET ('Input', 'FLAGVER', TYPE, DIM, FGVERI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'OUTFGVER', TYPE, DIM, FGVERO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      FGVERI = MIN (FGVERI, FGV)
      IF (FGVERI.EQ.0) FGVERI = FGV
      IF ((FGVERO.LE.0) .OR. (FGVERO.GT.FGV)) FGVERO = FGV + 1
      FGVERC = FGVERI
      IF (FGVERO.LE.FGV) FGVERC = - ABS (FGVERI)
      CALL OPUT (UVIN, FGNAME(1), TYPE, DIM, FGVERI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (UVIN, FGNAME(2), TYPE, DIM, FGVERI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (UVIN, FGNAME(3), TYPE, DIM, FGVERO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (UVIN, FGNAME(4), TYPE, DIM, FGVERC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       SNTAB
      SNTAB = 'SN table'
      CALL UV2TAB (UVIN, SNTAB, 'SN', SNVER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Give message if restarting
      IF (DOCAL) THEN
C                                       Check SN table
         CALL OOPEN (SNTAB, 'READ', IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OGET (SNTAB, 'VER', TYPE, DIM, SNVER, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OCLOSE (SNTAB, IERR)
         IF (IERR.NE.0) GO TO 999
         WRITE (MSGTXT,1000) SNVER
         CALL MSGWRT (4)
         END IF
C                                       UVSCR
      UVSCR = 'uv data scratch file'
      CALL CREATE (UVSCR, 'UVDATA', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Specify subtraction method.
      CALL OGET ('Input', 'CMETHOD', TYPE, DIM, DUMMY, METH, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (UVSCR, 'MODMETH', TYPE, DIM, DUMMY, METH, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       UVRANGE for imaging
      CALL OGET ('Input', 'UVRANGE', TYPE, DIM, UVRA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set defaults
      IF (UVRA(2).LE.0.0) UVRA(2)= 1.0E15
      CALL OPUT (UVSCR, 'UVRANGE', TYPE, DIM, UVRA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT ('Input', 'UVRANGE', TYPE, DIM, UVRA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       WTUV for self-cal
      CALL OGET ('Input', 'WTUV', TYPE, DIM, WTUV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set defaults
      IF (WTUV.LE.0.0) WTUV= 0.05
      CALL OPUT ('Input', 'WTUV', TYPE, DIM, WTUV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (UVSCR, 'CALEDIT.DOWTCL', OOALOG, DIM, WTCLDO, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Output uv data (UVOUT)
C                                       Clone
      UVOUT = 'Output UV data (and scratch)'
      CALL OUVCLN (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY5, INK5, OUTK5, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       SOLMODE trap for '    '
      CALL OGET (UVOUT, 'SOLMODE', TYPE, DIM, DUMMY, SMOD, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (SMOD.EQ.' ') THEN
         SWITCH = .TRUE.
         IF (SMOD.EQ.' ') SMOD = 'P'
         CALL OPUT (UVOUT, 'SOLMODE', TYPE, DIM, DUMMY, SMOD, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         SWITCH = .FALSE.
         END IF
C                                       Flag to allow switching from
C                                       phase to amplitude.
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (UVOUT, 'SWITCH', OOALOG, DIM, SWITCH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (UVOUT, 'CALEDIT.DOWTCL', OOALOG, DIM, WTCLDO, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Digest APARM
      CALL OGET ('Input', 'APARM', TYPE, DIM, APARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Average in poln?
      AVGPOL = APARM(3).GT.0.1
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (UVOUT, 'AVGPOL', OOALOG, DIM, AVGPOL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (AVGPOL) THEN
         APARM(3) = 1.0
      ELSE
         APARM(3) = -1.0
         END IF
C                                       Constrain flux scale?
      DOMGM = APARM(2).GT.0.1
      CALL OPUT (UVOUT, 'DOMGM', OOALOG, DIM, DOMGM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (DOMGM) THEN
         APARM(4) = 1.0
      ELSE
         APARM(4) = -1.0
         END IF
C                                       Min. no. antennas
      MINNO = IROUND (APARM(1))
      IF (MINNO.LE.2) MINNO = 4
      APARM(1) = MINNO
      CALL OPUT (UVOUT, 'MINNO', OOAINT, DIM, MINNO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Average in IF?
      AVGIF = APARM(5).GT.0.1
      CALL OPUT (UVOUT, 'AVGIF', OOALOG, DIM, AVGIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (AVGIF) THEN
         APARM(5) = 1.0
      ELSE
         APARM(5) = -1.0
         END IF
C                                       Print flag
      PRTLV = IROUND (APARM(6))
      CALL OPUT (UVOUT, 'PRTLV', OOAINT, DIM, PRTLV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Min SNR
      IF (APARM(7).LE.0.0) APARM(7) = 5.0
      SNRMIN = APARM(7)
      CALL OPUT (UVOUT, 'SNRMIN', OOARE, DIM, SNRMIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       edit average time (0 ok)
      CALL OPUT (UVOUT, 'EQU_TIME', OOARE, DIM, APARM(9), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (UVIN, 'EQU_TIME', OOARE, DIM, APARM(9), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       SIGMA
      IF (APARM(8).EQ.0.0) APARM(8) = 10.0
      SIGMA = APARM(8)
      NEGOK = SIGMA.LT.0.0
      SIGMA = ABS (SIGMA)
C                                       Set smoothing
      METH = 'BOX '
      DIM(1) = 4
      DIM(2) = 1
      CALL OPUT (UVOUT, 'SMOTYPE', OOACAR, DIM, DUMMY, METH, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 1
      SMTIME = 0.0
      CALL OPUT (UVOUT, 'SMOPHASE', OOARE, DIM, SMTIME, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      SMTIME = MAX (0.0, APARM(10))
      CALL OPUT (UVOUT, 'SMOAMP', OOARE, DIM, SMTIME, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Save defaults in APARM
      DIM(1) = 10
      DIM(2) = 1
      CALL OPUT ('Input', 'APARM', OOARE, DIM, APARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Reset selection
      STOKES = 'I'
      CALL SECSAV (UVIN, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       BEAM
      DO 40 I = 1,BFIELD
         IF (.NOT.ONEBEM) THEN
            WRITE (BEAM(I),1005) I
         ELSE
            BEAM(I) = 'BEAM image for CLEAN'
            END IF
         CALL CREATE (BEAM(I), 'IMAGE', IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
         CALL IN2OBJ ('Input', NKEY3, INK3, OUTK3, BEAM(I), IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Classes derived from Stokes'
         IF (I.LE.999) THEN
            WRITE (CLASS,1040) 'IBM', I
         ELSE
            WRITE (CLASS,1041) 'IB', I
            END IF
         DIM(1) = 6
         CALL OPUT (BEAM(I), 'CLASS', OOACAR, DIM, DUMMY, CLASS, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Continuum
         CHTYPE = 'SUM'
         DIM(1) = LEN (CHTYPE)
         DIM(2) = 1
         CALL OPUT (BEAM(I), 'CHTYPE', OOACAR, DIM, DUMMY, CHTYPE,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
 40      CONTINUE
C                                       CLEAN
      DO 50 I = 1,NFIELD
         WRITE (CLEANO(I),1006) I
         CALL CREATE (CLEANO(I), 'IMAGE', IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
         CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, CLEANO(I), IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Class from stokes
         IF (I.LE.999) THEN
            WRITE (CLASS,1040) 'ICL', I
         ELSE
            WRITE (CLASS,1041) 'IC', I
            END IF
         DIM(1) = LEN (CLASS)
         DIM(2) = 1
         CALL OPUT (CLEANO(I), 'CLASS', OOACAR, DIM, DUMMY, CLASS, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Save SIGMA on CLEANO(I)
         DIM(1) = 1
         CALL OPUT (CLEANO(I), 'SIGMA', OOARE, DIM, SIGMA, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OPUT (CLEANO(I), 'USENEGS', OOALOG, DIM, NEGOK, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Continuum
         CHTYPE = 'SUM'
         DIM(1) = LEN (CHTYPE)
         DIM(2) = 1
         CALL OPUT (CLEANO(I), 'CHTYPE', OOACAR, DIM, DUMMY, CHTYPE,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
 50      CONTINUE
C                                       Attach Images etc. to CLEAN
C                                       BEAM image
      DIM(1) = LEN (BEAM(1))
      DIM(2) = BFIELD
      DIM(3) = 1
      CALL OPUT (CLEAN, 'DIRTBEAM', OOACAR, DIM, DUMMY, BEAM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       CLEAN image(s)
      DIM(1) = LEN (CLEANO(1))
      DIM(2) = NFIELD
      CALL OPUT (CLEAN, 'CLEANI', OOACAR, DIM, DUMMY, CLEANO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       UVSCR
      DIM(1) = LEN (UVSCR)
      DIM(2) = 1
      CALL OPUT (CLEAN, 'UVDATA', OOACAR, DIM, DUMMY, UVSCR, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Clean type (UV)
      CLNTYP = 'UV'
      DIM(1) = LEN (CLNTYP)
      DIM(2) = 1
      CALL OPUT (CLEAN, 'CLEANTYP', OOACAR, DIM, DUMMY, CLNTYP, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Number of fields
      DIM(1) = 1
      CALL OPUT (CLEAN, 'NIMAGES', OOAINT, DIM, NFIELD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (CLEANO(1), 'NIMAGES', OOAINT, DIM, NFIELD, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Process other info
C                                       Field size
      CALL FILL (2*MAXFLD, 0, FLDSIZ)
      CALL OGET ('Input', 'FLDSIZE', TYPE, DIM, FLDSIZ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Min. image sizes
      CALL FILL (2*MAXFLD, 0, IMSIZE)
      CALL OGET ('Input', 'IMSIZE', TYPE, DIM, IMSI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL RFILL (MAXFLD, 0.0, RASH)
      CALL OGET ('Input', 'RASHIFT', TYPE, DIM, RASH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL RFILL (MAXFLD, 0.0, DECSH)
      CALL OGET ('Input', 'DECSHIFT', TYPE, DIM, DECSH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       BOXFILE option for field
C                                       parameters
      CALL OGET ('Input', 'BOXFILE', TYPE, DIM, DUMMY, BOXFIL, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (BOXFIL.NE.' ') THEN
C                                       Need coords and rotation
         CALL OUVGET (UVIN, 'ROTATE', TYPE, DIM, ROTATE, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL UVDGET (UVIN, 'CRVAL', TYPE, DIM, CRVAL, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL UVDGET (UVIN, 'CROTA', TYPE, DIM, CROTAU, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL UVDFND (UVIN, 2, 'RA', JLOCR, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL UVDFND (UVIN, 2, 'DEC', JLOCD, IERR)
         IF (IERR.NE.0) GO TO 999
         ROTATE = ROTATE + CROTAU(JLOCD)
         CALL FIELDF (BOXFIL, NFIELD, NFIELD, CRVAL(JLOCR),
     *      CRVAL(JLOCD), ROTATE, FLDSIZ, RASH, DECSH, BCOMP, IERR)
         IF (IERR.GT.0) THEN
            MSGTXT = 'IMALIN: Error in defining field parms from file'
            CALL MSGWRT (7)
            GO TO 999
            END IF
         END IF
C                                       put back in objects
      DIM(1) = MAXAFL
      DIM(2) = 1
      CALL OPUT ('Input', 'RASHIFT', OOARE, DIM, RASH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT ('Input', 'DECSHIFT', OOARE, DIM, DECSH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = MAXFLD
      DO 60 I = 1,NFIELD
         CALL OPUT (CLEANO(I), 'RASHIFT', OOARE, DIM, RASH, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OPUT (CLEANO(I), 'DECSHIFT', OOARE, DIM, DECSH, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
 60      CONTINUE
C                                       image size, field size
      IMSI(1) = MAX (IMSI(1), 64)
      IMSI(2) = MAX (IMSI(2), 64)
      IMSI(1) = NPTWO (IMSI(1))
      IMSI(2) = NPTWO (IMSI(2))
      MAXNX = 0
      MAXNY = 0
      MINNX = MAXIMG
      MINNY = MAXIMG
      DO 70 I = 1,MAXFLD
         IF ((FLDSIZ(1,I).LT.0) .AND. (FLDSIZ(2,I).LT.0)) THEN
            FLDSIZ(1,I) = -1
            FLDSIZ(2,I) = -1
            NXFLD = 0
            NYFLD = 0
         ELSE
            IF (FLDSIZ(1,I).LE.0) FLDSIZ(1,I) = IMSI(1) - 10
            IF (FLDSIZ(2,I).LE.0) FLDSIZ(2,I) = IMSI(2) - 10
C                                       Next largest power of two
            NXFLD = NPTWO (FLDSIZ(1,I))
            NYFLD = NPTWO (FLDSIZ(2,I))
            END IF
         IMSIZE(1,I) = MAX (IMSI(1), NXFLD)
         IMSIZE(2,I) = MAX (IMSI(2), NYFLD)
         MAXNX = MAX (MAXNX, IMSIZE(1,I))
         MAXNY = MAX (MAXNY, IMSIZE(2,I))
         MINNX = MIN (MINNX, IMSIZE(1,I))
         MINNY = MIN (MINNY, IMSIZE(2,I))
 70      CONTINUE
C                                       put back in objects
      DIM(1) = 2
      DIM(2) = MAXAFL
      CALL OPUT ('Input', 'FLDSIZE', OOAINT, DIM, FLDSIZ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(2) = MAXFLD
      CALL OPUT (CLEANO(1), 'IMSIZE', OOAINT, DIM, IMSIZE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(2) = 1
      DO 80 I = 2,NFIELD
         CALL OPUT (CLEANO(I), 'IMSIZE', OOAINT, DIM, IMSIZE(1,I),
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
 80      CONTINUE
      DIM(1) = 2
      DIM(2) = 1
      DO 85 I = 1,BFIELD
         IF (.NOT.ONEBEM) THEN
            NX = IMSIZE(1,I)
            NY = IMSIZE(2,I)
         ELSE
            NX = MAXNX
            NY = MAXNY
            END IF
         NX = MIN (512, NX)
         NY = MIN (512, NY)
         NXBEM(I) = NX
         NYBEM(I) = NY
         BMSIZ(1) = NXBEM(I)
         BMSIZ(2) = NYBEM(I)
         CALL OPUT (BEAM(I), 'IMSIZE', OOAINT, DIM, BMSIZ, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
 85      CONTINUE
C                                       Define the clean boxes
C                                       Create clean windows
      WINM = MIN (MXNBOX, MXNBFL/NFIELD)
      WINW = 4 * NFIELD * WINM
      NWORDS = (WINW - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'IMAGR', NWORDS, WIN(1), WINP, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Start with program inputs
      CALL FILL (MAXFLD, 1, NBOXES)
      CALL FILL (WINW, 0, WIN(1+WINP))
      CALL OGET ('Input', 'NBOXES', TYPE, DIM, NBOXES, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Window
      CALL OGET ('Input', 'CLBOX', TYPE, DIM, CLBOX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (NBOXES(1).GT.0) THEN
         IS = 2
         DO 87 K = 1,NBOXES(1)
            KP = (K-1) * NFIELD * 4 + 1 + WINP
            CALL COPY (4, CLBOX(1,K), WIN(KP))
 87         CONTINUE
      ELSE
         NBOXES(1) = 1
         IS = 1
         END IF
C                                       Fill in higher windows
C                                       or all if NBOXES <= 0
      DO 90 I = IS,NFIELD
         KP = 4 * (I-1) + WINP
C                                       Only if there are boxes
         IF (FLDSIZ(1,I).LT.0) THEN
            NBOXES(I) = 0
            WIN(1+KP) = 5
            WIN(2+KP) = 5
            WIN(3+KP) = IMSIZE(1,I) - 5
            WIN(4+KP) = IMSIZE(2,I) - 5
C                                       fill window from IMSIZE and
C                                       FLDSIZE
         ELSE
            EDGEX = IMSIZE(1,I) - FLDSIZ(1,I)
            EDGEY = IMSIZE(2,I) - FLDSIZ(2,I)
            WIN(1+KP) = EDGEX / 2
            WIN(2+KP) = EDGEY / 2
            WIN(3+KP) = IMSIZE(1,I) - EDGEX / 2
            WIN(4+KP) = IMSIZE(2,I) - EDGEY / 2
            END IF
         IF (AUTOBX(1).GT.0) NBOXES(I) = 0
 90      CONTINUE
C                                       BOXFILE option?
      IF (BOXFIL(1:1).NE.'  ') THEN
         CALL WINDF (WIN(1+WINP), NBOXES, NFIELD, IMSIZE, BOXFIL, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'IMALIN: Error in defining clean boxes from file'
            CALL MSGWRT(7)
            GO TO 999
            END IF
         END IF
C                                       put parms back
      DIM(1) = NFIELD
      DIM(2) = 1
      CALL OPUT (CLEAN, 'NBOXES', OOAINT, DIM, NBOXES, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 4 * NFIELD
      DIM(2) = WINM
      CALL OPUT (CLEAN, 'WINDOW', OOAINT, DIM, WIN(1+WINP), CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 6
      DIM(2) = 1
      CALL OPUT (CLEAN, 'AUTOBOX', OOARE, DIM, AUTOBX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Boxfile option for Stars
      NSTAR = 0
      IF (BOXFIL(1:1).NE.' ') THEN
         CALL STBOXF (BOXFIL, NSTAR, STPOS, STPARM, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'FAILED READING BOXFILE FOR STAR POSITIONS'
            CALL MSGWRT (8)
            END IF
         END IF
      IF (NSTAR.GT.0) THEN
         DIM(1) = 1
         DIM(2) = 1
         CALL OPUT (CLEAN, 'NSTARS', OOAINT, DIM, NSTAR, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         DIM(1) = 2
         DIM(2) = NSTAR
         CALL OPUT (CLEAN, 'STARPOSN', OOADP, DIM, STPOS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         DIM(1) = 4
         CALL OPUT (CLEAN, 'STARPARM', OOARE, DIM, STPARM, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         WRITE (MSGTXT,1200) NSTAR
         CALL MSGWRT (2)
         END IF
C                                       Copy BOXFILE to OBOXFILE
      IF (IERR.EQ.0) CALL CPBOXF (BOXFIL, OBXFIL, IERR)
C                                       Image all selected data
C                                       Access UVIn once
      CALL U2IDES (UVIN, CLEANO(1), .FALSE., IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get total no. IF, chann
      CALL SECSLT (UVIN, BI, EI, BC, EC, STOKES, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Channel numbers
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (CLEAN, 'UVCHAN', OOAINT, DIM, 1, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      NCHAV = (EI - BI + 1) * (EC - BC + 1)
      NCHAV = MAX (1, NCHAV)
      CALL OPUT (CLEAN, 'NCHAV', OOAINT, DIM, NCHAV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (UVIN, 'NCHAV', OOAINT, DIM, NCHAV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (MSGTXT,4000) NCHAV
 4000 FORMAT ('*****NCHAV',I6)
      CALL MSGWRT (8)
C                                       Copy UV descriptors to Images
      DO 100 I = 1,NFIELD
         IF (I.GT.1) THEN
            CALL U2IDES (UVIN, CLEANO(I), .FALSE., IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         CALL OPUT (CLEANO(I), 'NCHAV', OOAINT, DIM, NCHAV, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Force instantiation for naming
C                                       info.
         CALL OOPEN (CLEANO(I), 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OCLOSE (CLEANO(I), IERR)
         IF (IERR.NE.0) GO TO 999
 100     CONTINUE

C                                       Copy UV descriptors to BEAM
      DO 110 I = 1,BFIELD
         CALL U2IDES (UVIN, BEAM(I), .FALSE., IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Naming from CLEANO
         CALL OGET (CLEANO(I), 'NAME', TYPE, DIM, DUMMY, NAME, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OPUT (BEAM(I), 'NAME', TYPE, DIM, DUMMY, NAME, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OGET (CLEANO(I), 'IMSEQ', TYPE, DIM, IMSEQ, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OPUT (BEAM(I), 'IMSEQ', TYPE, DIM, IMSEQ, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OGET (CLEANO(I), 'DISK', TYPE, DIM, DISK, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OPUT (BEAM(I), 'DISK', TYPE, DIM, DISK, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Force full instantiation
         CALL OOPEN (BEAM(I), 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OCLOSE (BEAM(I), IERR)
         IF (IERR.NE.0) GO TO 999
 110     CONTINUE
C                                       Write CC version 1
      VER = 1
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (CLEAN, 'VERSION', OOAINT, DIM, VER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Guard band in uv grids.
      CALL OGET ('Input', 'CELLSIZE', TYPE, DIM, CELLS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       UV Cellsize for smallest image
      DU = RAD2AS / (MINNX * ABS (CELLS(1)))
      DV = RAD2AS / (MINNY * CELLS(2))
C                                       Set limits.
      UMAX = (MINNX/2-1.0) * DU
      VMAX = (MINNY/2-1.0) * DV
C                                       Add maximum of user specified
C                                       guardband or 7 cells.
      CALL OGET ('Input', 'UVTAPER', TYPE, DIM, UVTAPR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'GUARD', TYPE, DIM, GUARDB, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       default guard band widths
      UTFACT(1) = 1.0
      IF (UVTAPR(1).GT.0.0) UTFACT(1) =  EXP (LOG(0.3) *
     *   ((UMAX*0.7) / (1000. * UVTAPR(1)))**2)
      UTFACT(1) = 0.3 * SQRT (UTFACT(1))
      GUAU = 7.0 * DU
      IF (GUARDB(1).LT.-0.001) THEN
         GUARDB(1) = GUAU / UMAX
      ELSE IF ((GUARDB(1).LE.0.001) .OR. (GUARDB(1).GT.0.9)) THEN
         GUARDB(1) = UTFACT(1)
         END IF
      GUAU = MAX (GUAU, GUARDB(1)*UMAX)
      UTFACT(2) = 1.0
      IF (UVTAPR(2).GT.0.0) UTFACT(2) =  EXP (LOG(0.3) *
     *   ((VMAX*0.7) / (1000. * UVTAPR(2)))**2)
      UTFACT(2) = 0.3 * SQRT (UTFACT(2))
      GUAV = 7.0 * DV
      IF (GUARDB(2).LT.-0.001) THEN
         GUARDB(2) = GUAV / VMAX
      ELSE IF ((GUARDB(2).LE.0.001) .OR. (GUARDB(2).GT.0.9)) THEN
         GUARDB(2) = UTFACT(2)
         END IF
      GUAV = MAX (GUAV, GUARDB(2)*VMAX)
      GUARDB(1) = GUAU / UMAX
      GUARDB(2) = GUAV / VMAX
      CALL OUVPUT (UVIN, 'GUARDBND', TYPE, DIM, GUARDB, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OUVPUT (UVIN, 'GUARDEF', TYPE, DIM, UTFACT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OUVPUT(UVIN, 'CELLSIZE', TYPE, DIM, CELLS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      UMAX = UMAX - GUAU
      VMAX = VMAX - GUAV
      DIM(1) = 1
      DIM(2) = 1
      CALL OUVPUT (UVIN, 'UMAX', OOARE, DIM, UMAX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OUVPUT (UVIN, 'VMAX', OOARE, DIM, VMAX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Put in TELL adverb list
      TELOBJ = 'CLEAN Tell object'
      DIM(1) = LEN (TELOBJ)
      DIM(2) = 1
      CALL OPUT (CLEAN, 'TELLNAME', OOACAR, DIM, DUMMY, TELOBJ, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 8
      DIM(2) = NKEYT
      DIM(3) = 1
      CALL OPUT (CLEAN, 'TELADVRB', OOACAR, DIM, DUMMY, TELK, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 2
      CALL OPUT (CLEAN, 'TELLDIMS', OOAINT, DIM, TELDIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = NKEYT
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (CLEAN, 'TELLTYPE', OOAINT, DIM, TELTYP, CDUMMY, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Restarting with SN ver ', I3)
 1005 FORMAT ('BEAM field number ',I4.4)
 1006 FORMAT ('CLEAN field number ',I4.4)
 1040 FORMAT (A3,I3.3)
 1041 FORMAT (A2,I4.4)
 1050 FORMAT ('OBOXfile',I4,2I2.2,'.',3I2.2)
 1051 FORMAT ('Using OBOXFILE = ''',A,'''')
 1052 FORMAT ('HOME:OBOXtemp',I4,2I2.2,'.',3I2.2)
 1053 FORMAT ('Using temporary OBOXFILE = ''',A,'''')
 1200 FORMAT ('Will plot',I4,' star positions')
      END
      SUBROUTINE CPBOXF (BOXFIL, OBXFIL, IRET)
C-----------------------------------------------------------------------
C   Copy BOXFILE to OBOXFILE but only if OBOXFILE is a new file
C   Inputs:
C      BOXFIL   C*48    Input boxfile
C      OBXFIL   C*48    Output boxfile
C   Output:
C      IRET     I       Error code
C-----------------------------------------------------------------------
      CHARACTER BOXFIL*(*), OBXFIL*(*)
      INTEGER   IRET
C
      INTEGER   LUN1, LUN2, IND1, IND2, JTRIM, MSGSAV, K, I
      CHARACTER LINE*132
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF ((BOXFIL.NE.' ') .AND. (OBXFIL.NE.' ') .AND.
     *   (BOXFIL.NE.OBXFIL)) THEN
         MSGSAV = MSGSUP
         MSGSUP = 32000
         LUN1 = 3
         LUN2 = 11
         CALL ZTXOPN ('QWRT', LUN2, IND2, OBXFIL, .FALSE., IRET)
         MSGSUP = MSGSAV
C                                       file already exists
         IF (IRET.EQ.5) THEN
            IRET = 0
            MSGTXT = 'OBOXFILE already exists, BOXFILE not copied to'
     *         // ' it'
            CALL MSGWRT (3)
C                                       other error
         ELSE IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT'
            GO TO 990
C                                       do copy
         ELSE
            CALL ZTXOPN ('QRED', LUN1, IND1, BOXFIL, .FALSE., IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'OPEN INPUT'
               GO TO 990
               END IF
            DO 50 K = 1,100000
               CALL ZTXIO ('READ', LUN1, IND1, LINE, IRET)
               IF (IRET.EQ.2) GO TO 60
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READ'
                  GO TO 990
                  END IF
               I = JTRIM (LINE)
               CALL ZTXIO ('WRIT', LUN2, IND2, LINE(:I), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRIT'
                  GO TO 990
                  END IF
 50            CONTINUE
C                                       close them up
 60         CALL ZTXCLS (LUN1, IND1, IRET)
            CALL ZTXCLS (LUN2, IND2, IRET)
            K = K - 1
            WRITE (MSGTXT,1060) K
            CALL MSGWRT (3)
            IRET = 0
            END IF
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CPBOXF: ERROR',I5,' DOING ',A)
 1060 FORMAT ('CPBOXF: copied',I6,' lines from BOXFILE to OBOXFILE')
      END
      SUBROUTINE SCLOOP (CLEAN, UVIN, SNTAB, UVSCR, UVOUT, IERR)
C-----------------------------------------------------------------------
C   Does Selfcal-CLEAN loop.
C   If logical value SWITCH attached to UVOUT is true then after
C   convergence with phase only self cal amplitude self cal is enabled.
C   When this switch is made all IFs and polarizations selected are
C   averaged in the solutions.
C   Inputs:
C      CLEAN   C*?    Name of CLEAN process object
C      UVIN    C*32   Input uv object
C      SNTAB   C*32   SN table object, associated with input uv data.
C      UVSCR   C*32   Scratch uv object.  Necessary adverbs for
C                     controling self calibration are copied.
C      UVOUT   C*32   Output uv object
C   Output:
C      IERR    I      Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER CLEAN*(*), UVIN*(*), SNTAB*(*), UVSCR*(*), UVOUT*(*)
      INTEGER   IERR
C
      INTEGER  SIZAMP
C                                       SIZAMP = size of UVAMP table
      PARAMETER (SIZAMP = 1000)
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   TYPE, DIM(7), ILOOP, NAMP, NCCRES(MAXFLD), MXLOOP,
     *   BESTSN, DUMMY, MFIELD
      REAL      UVR(2), UVAMP(5,SIZAMP), FREUSE, ROS(3), ROSMIN(3),
     *   ROSLST(200)
      LOGICAL   DOPTMD, DONE, CONVG, FLIPED, EDITOK, DOSWT, DOSWT0,
     *   SWITCH, NEGOK
      CHARACTER CLEANO(MAXFLD)*32, UVDATA*32, UVWORK*32, BESTUV*32,
     *   CDUMMY*1, SMOD*4
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      DONE = .FALSE.
      EDITOK = .TRUE.
C                                       Open CLEAN
      CALL OOPEN (CLEAN, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       CLEAN image object
      CALL OGET (CLEAN, 'CLEANI', TYPE, DIM, DUMMY, CLEANO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Number of self cal loops
      CALL OGET (UVIN, 'NUMLOOP', TYPE, DIM, MXLOOP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Initial working data is input
      UVDATA = UVIN
C                                       number of fields
      CALL CLNGET (CLEAN, 'NIMAGES', TYPE, DIM, MFIELD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Initial model division scratch
C                                       file is the output.
      UVWORK = UVOUT
      FLIPED = .FALSE.
      ROSMIN(1) = 1.0E20
      ROSMIN(2) = 1.0E20
      ROSMIN(3) = 0.0
      ROS(1) = 0.0
      ROS(2) = 0.0
      ROS(3) = 0.0
      CALL FILL (MAXFLD, 0, NCCRES)
      BESTSN = 0
      BESTUV = UVDATA
C                                       Initial point source model?
      CALL OGET (UVIN, 'MODDOPT', TYPE, DIM, DOPTMD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Calibrate to UVSCR
      IF (DOPTMD) THEN
         CALL PTCAL (UVDATA, CLEANO, SNTAB, UVWORK, UVSCR, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Select data to UVSCR
      ELSE
         CALL SC2SCR (UVDATA, UVSCR, CLEANO(1), IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Self cal loop
      ILOOP = 1
 100     WRITE (MSGTXT,1000) ILOOP
         CALL MSGWRT (5)
         ROS(3) = ILOOP
C                                       Get UV data statistics
         NAMP = SIZAMP
         UVR(1) = 0.0
         UVR(2) = 0.0
         CALL SCUVST (UVSCR, UVR, NAMP, UVAMP, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Uniform weighting
         CALL OUNFWT (UVSCR, CLEANO(1), IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Init clean stuff
         CALL SCCLIN (CLEAN, .FALSE., MFIELD, NCCRES, ROS(1), IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Clean
         CALL CLNUV (CLEAN, .TRUE., IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Any new orders from higher ups?
         CALL SCTELL (CLEAN, UVWORK, MXLOOP, DONE, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Image statistics and determine
C                                       how much of this CLEAN to reuse.
         CALL SCSTAT (MFIELD, CLEANO, FREUSE, NEGOK, ROS, ILOOP, ROSLST,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Test if mode switching is
C                                       enabled.
         CALL OGET (UVWORK, 'SOLMODE', TYPE, DIM, DUMMY, SMOD, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OGET (UVWORK, 'SWITCH', TYPE, DIM, SWITCH, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (SMOD.EQ.' ') SWITCH = .TRUE.
C                                       TV interaction
         DOSWT = FLIPED .OR. (.NOT.SWITCH)
         DOSWT0 = DOSWT
         CALL SCMATV (UVDATA, UVSCR, CLEAN, CLEANO, MFIELD, EDITOK,
     *      DONE, UVWORK, MXLOOP, DOSWT, IERR)
         IF (IERR.GT.1) GO TO 999
         CONVG = DOSWT.NEQV.DOSWT0
C                                       Convergence test, possible mode
C                                       switch.
         CALL SCONVG (MFIELD, UVDATA, UVWORK, SNTAB, UVOUT, ROS, ROSMIN,
     *      BESTUV, BESTSN, CONVG, NCCRES, FLIPED, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Self cal
         IF ((.NOT.CONVG) .AND. (.NOT.DONE)) THEN
            CALL SCLIT (MFIELD, UVDATA, CLEANO, SNTAB, UVWORK, UVSCR,
     *         FREUSE, NEGOK, NAMP, UVAMP, UVR, NCCRES, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Any new orders from higher ups?
            CALL SCTELL (CLEAN, UVWORK, MXLOOP, DONE, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         IF (CONVG .OR. DONE .OR. (ILOOP.GE.MXLOOP)) GO TO 610
         ILOOP = ILOOP + 1
         GO TO 100
C                                       Final CLEAN
 610  MSGTXT = '******** CLEAN deeper and Restore **********'
      CALL MSGWRT (5)
C                                       Recalibrate data with best SN
      CALL DESTRY (SNTAB, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UV2TAB (BESTUV, SNTAB, 'SN', BESTSN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL SC2SCR (BESTUV, UVSCR, CLEANO(1), IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Uniform weighting
      CALL OUNFWT (UVSCR, CLEANO(1), IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Init clean stuff
      CALL SCCLIN (CLEAN, .TRUE., MFIELD, NCCRES, ROS(1), IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Clean
      CALL CLNUV (CLEAN, .TRUE., IERR)
      IF (IERR.NE.0) GO TO 999
C                                       TV final display
      DONE = .TRUE.
      CALL SCMATV (UVDATA, UVSCR, CLEAN, CLEANO, MFIELD, EDITOK, DONE,
     *   UVWORK, MXLOOP, FLIPED, IERR)
C                                       Close CLEAN
      CALL OCLOSE (CLEAN, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Calibrate output UVdata.
      CALL SCLOUT (BESTUV, SNTAB, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Delete scratch objects to avoid
C                                       messages.
      CALL OUVZAP (UVSCR, IERR)
C      IF (FLIPED) CALL OUVZAP (UVWORK, IERR)
C
      IERR = 0
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('******* Beginning self-calibration loop ',I4,' *****')
      END
      SUBROUTINE SC2SCR (UVDATA, UVSCR, CLEANO, IERR)
C-----------------------------------------------------------------------
C   Routine to Copy all subarrays to a scratch file
C   Inputs:
C      UVDATA  C*32   Input uv data
C      UVSCR   C*32   Scratch uv object.
C      CLEANO  C*32   Clean objst
C   Output:
C      IERR    I      Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), UVSCR*(*), CLEANO*(*)
      INTEGER   IERR
C
      INTEGER   TYPE, DIM(7), ANVER, NSUBA, ISUBA, COUNT, MSGSAV, LST,
     *   DUMMY
      REAL      ZEROSP(5), ZERO(2), UVMAXS(4)
      CHARACTER ANTAB*32, CDUMMY*1, STOKES*4
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
C                                       Number of subarrays
      MSGSAV = MSGSUP
      ANTAB = 'Temp AN for SCLOOP'
      ANVER = 1
      CALL UV2TAB (UVDATA, ANTAB, 'AN', ANVER, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TBLHIV (ANTAB, NSUBA, IERR)
      IF (IERR.NE.0) GO TO 990
      NSUBA = MAX (1, NSUBA)
C                                       Destroy temp object
      CALL TABDES (ANTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Start at beginning of output
      DIM(1) = 1
      DIM(2) = 1
      COUNT = 0
      CALL OPUT (UVSCR, 'UV_DESC.VISOFF', OOAINT, DIM, COUNT, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Zero spacing flux
      MSGSUP = 32000
      CALL OUVGET (UVDATA, 'ZEROSP', TYPE, DIM, ZEROSP, CDUMMY, IERR)
      MSGSUP = MSGSAV
C                                       Default = none
      IF (IERR.EQ.1) THEN
         CALL RFILL (5, 0.0, ZEROSP)
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       Do zero-spacing
      IF ((ZEROSP(1).GT.0.0) .AND. (ZEROSP(5).GT.0.0)) THEN
C                                       Stokes
         MSGSUP = 32000
         CALL OUVGET (UVDATA, 'STOKES', TYPE, DIM, DUMMY, STOKES, IERR)
         MSGSUP = MSGSAV
C                                       Default = none
         IF (IERR.EQ.1) THEN
            STOKES = 'I'
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 990
         LST = 1
         IF (STOKES(1:1).EQ.'Q') LST = 2
         IF (STOKES(1:1).EQ.'U') LST = 3
         IF (STOKES(1:1).EQ.'V') LST = 4
         ZERO(1) = ZEROSP(LST)
         ZERO(2) = ZEROSP(5)
      ELSE
         ZERO(1) = 0.0
         ZERO(2) = 0.0
         END IF
C                                       Copy
      CALL RFILL (4, 0.0, UVMAXS)
      DO 50 ISUBA = 1,NSUBA
         DIM(1) = 1
         DIM(2) = 1
         CALL OUVPUT (UVDATA, 'CALEDIT.SUBARR', OOAINT, DIM, ISUBA,
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL SCUV2S (UVDATA, UVSCR, CLEANO, ZERO, NSUBA, UVMAXS, IERR)
         IF (IERR.NE.0) GO TO 990
         ZERO(1) = 0.0
         ZERO(2) = 0.0
C                                       Set vis offset to append
         CALL OGET (UVSCR, 'UV_DESC.GCOUNT', TYPE, DIM, COUNT, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OPUT (UVSCR, 'UV_DESC.VISOFF', OOAINT, DIM, COUNT, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
 50      CONTINUE
C                                       Reset selection on objects
      DIM(1) = 1
      DIM(2) = 1
      COUNT = 0
      CALL OPUT (UVSCR, 'UV_DESC.VISOFF', OOAINT, DIM, COUNT, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      ISUBA = 0
      CALL OUVPUT (UVDATA, 'CALEDIT.SUBARR', OOAINT, DIM, ISUBA,
     *   CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVPUT (UVSCR, 'CALEDIT.SUBARR', OOAINT, DIM, ISUBA,
     *   CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'SC2SCR:ERROR COPYING UVDATA'
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE PTCAL (UVDATA, CLEANO, SNTAB, UVWORK, UVSCR, IERR)
C-----------------------------------------------------------------------
C   Routine to calibrate with a point model.  Calibrated, selected data
C   is written to an output scratch object
C   Point model parameters set in initialization.
C   Inputs:
C      UVDATA  C*32   Input uv object
C      CLEANO  C*32   CLEAN image (shouldn't actually be used)
C      SNTAB   C*32   SN table object, associated with input uv data.
C      UVWORK  C*32   Output file used for data divided by model
C      UVSCR   C*32   Scratch uv object.
C   Output:
C      IERR    I      Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), CLEANO(*)*(*), SNTAB*(*), UVWORK*(*),
     *   UVSCR*(*)
      INTEGER   IERR
C
      INTEGER   TYPE, DIM(7), BCHAN, ECHAN, NCHAN, BIF, EIF, REFANT(51),
     *   MFIELD, VER, NKCOPY, NSUBA, ISUBA, ANVER, DUMMY
      CHARACTER STOKES*4, METH*4, DFT*4, UVDOBJ*32, KEYCOP(4)*8,
     *   ANTAB*32, CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
      DATA NKCOPY /4/
      DATA KEYCOP /'MODDOPT', 'MODPTFLX', 'MODPTXOF', 'MODPTYOF'/
C-----------------------------------------------------------------------
C                                       Message
      MSGTXT = 'Determining new calibration from point model'
      CALL MSGWRT (5)
      UVDOBJ = UVWORK
C                                       Only 1 field
      MFIELD = 1
C                                       Use DFT method
      DFT = 'DFT '
      CALL OGET (UVDATA, 'MODMETH', TYPE, DIM, DUMMY, METH, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (UVDOBJ, 'MODMETH', OOACAR, DIM, DUMMY, DFT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       All selected frequencies
C                                       Find number of channels, IFs
      CALL SECSLT (UVDATA, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 990
      NCHAN = (ECHAN - BCHAN + 1) * (EIF - BIF + 1)
      NCHAN = MAX (NCHAN, 1)
C                                       uncompress to UVWORK before
C                                       divide, apply flags
C                                       All Stokes'
      CALL SECSAV (UVDATA, BIF, EIF, BCHAN, ECHAN, '    ', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVCOPY (UVDATA, UVWORK, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Reset selection
      CALL SECSAV (UVDATA, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy point model
      CALL IN2OBJ (UVDATA, NKCOPY, KEYCOP, KEYCOP, UVWORK, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Divide
      CALL OUMDIV (UVWORK, UVWORK, MFIELD, CLEANO, BCHAN, NCHAN, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Restore requested method
      DIM(1) = LEN (METH)
      CALL OPUT (UVDOBJ, 'MODMETH', OOACAR, DIM, DUMMY, METH, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Force new SN table
      CALL TBLHIV (SNTAB, VER, IERR)
      IF (IERR.NE.0) GO TO 990
      VER = VER + 1
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (SNTAB, 'VER', OOAINT, DIM, VER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Higher numbered subarrays
      CALL OGET (UVDATA, 'SUBREFA', TYPE, DIM, REFANT(1), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (UVDATA, 'REFANT', TYPE, DIM, REFANT(1), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Number of subarrays
      ANTAB = 'Temp AN for PTCAL'
      ANVER = 1
      CALL UV2TAB (UVWORK, ANTAB, 'AN', ANVER, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TBLHIV (ANTAB, NSUBA, IERR)
      IF (IERR.NE.0) GO TO 990
      NSUBA = MAX (1, NSUBA)
C                                       Destroy temp object
      CALL TABDES (ANTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Loop over subarrays
      DO 100 ISUBA = 1,NSUBA
         DIM(1) = 1
         DIM(2) = 1
         CALL OUVPUT (UVWORK, 'CALEDIT.SUBARR', OOAINT, DIM, ISUBA,
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OPUT (UVWORK, 'REFANT', OOAINT, DIM, REFANT(ISUBA),
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Do solutions
         CALL SLFCAL (UVWORK, SNTAB, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Refer to a common reference
C                                       antenna
         CALL SLFREF (SNTAB, ISUBA, REFANT(ISUBA), IERR)
         IF (IERR.NE.0) GO TO 990
C                                       smooth, interpolate
         CALL SLFSMO (UVWORK, SNTAB, ISUBA, IERR)
         IF (IERR.NE.0) GO TO 990
 100     CONTINUE
C                                       Apply to output data
C                                       Set up to apply calibration
      CALL OGET (SNTAB, 'VER', TYPE, DIM, VER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (UVDATA, 'CALEDIT.CLUSE', TYPE, DIM, VER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (UVDATA, 'CALEDIT.DOCAL', OOALOG, DIM, .TRUE., CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Message
      WRITE (MSGTXT,1000) VER
      CALL MSGWRT (4)
C                                       Calibrate/copy
      CALL SC2SCR (UVDATA, UVSCR, CLEANO(1), IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR CALIBRATING WITH POINT MODEL'
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('New calibration table is SN version ',I4)
      END
      SUBROUTINE SCUVST (UVSCR, UVR, NAMP, UVAMP, IERR)
C-----------------------------------------------------------------------
C   Determine uv data visibility function.  The size of the returned
C   UVAMP array (NAMP) is adjusted so that each cell has an average of
C   at least 10 samples.  Cells with fewer than 3 samples are zeroed.
C   Some amount of filtering of this function is done.
C   Inputs:
C      UVSCR   C*32   Input uv object
C   Input/output:
C      UVR     R(2)   Uv range in selected data (kilowavelengths), if 0
C                     in input then this array will be filled from the
C                     actual range in the data.  Baselines longer than
C                     those used for imaging will not be included.
C      NAMP    I      Number of entries in UVAMP, on input the maximum
C                     size, on output the size used.
C   Output:
C      UVAMP   R(5,*) Average visibility amplitudes as a function of UV
C                     distance.
C                      (1,*) = average amplitude
C                      (2,*) = RMS of average amplitude
C                      (3,*) = central uv distance (klamda),
C                      (4,*) = Number of amplitudes averaged.
C      IERR    I      Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVSCR*(*)
      REAL      UVR(2), UVAMP(5,*)
      INTEGER   NAMP, IERR
C
      REAL      SUM, SUMA, MRMS, MAVG, UVRI(2)
      INTEGER   TYPE, DIM(7), NIN, NVIS, LOOP, COUNT, COUNTA
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      NIN = NAMP
C                                       Get imaging uv range
      CALL OGET (UVSCR, 'UVRANGE', TYPE, DIM, UVRI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Number of cells depends on
C                                       number of vis.
      CALL UVDGET (UVSCR, 'GCOUNT', TYPE, DIM, NVIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      NAMP = MIN (NIN, NVIS/10)
C                                       Determine function
      CALL UVAMPS (UVSCR, UVR, NAMP, UVAMP, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Don't use baselines longer than
C                                       used for imaging
      UVR(2) = MIN (UVR(2), UVRI(2))
C                                       Discard cells with fewer than 3
C                                       samples and average of RMS of
C                                       cells with 10  or more samples.
      SUM = 0.0
      COUNT = 0
      SUMA = 0.0
      COUNTA = 0
      DO 100 LOOP = 1,NAMP
         IF (UVAMP(5,LOOP).LT.3.1) THEN
            UVAMP(1,LOOP) = 0.0
            UVAMP(2,LOOP) = 0.0
            UVAMP(4,LOOP) = 0.0
            UVAMP(5,LOOP) = 0.0
            END IF
         IF (UVAMP(5,LOOP).GT.9.9) THEN
            COUNT = COUNT + 1
            SUM = SUM + UVAMP(2,LOOP)
C                                       Average amplitude in first 10%
C                                       of range
            IF (LOOP.LT.NAMP/10) THEN
               COUNTA = COUNTA + 1
               SUMA = SUMA + UVAMP(1,LOOP)
               END IF
            END IF
 100     CONTINUE
C                                       Reset RMSes to no less than the
C                                       average and amplitudes to no
C                                       more than the average of the
C                                       first 10 % of the uv range.
      IF (COUNT.GT.1) THEN
         MRMS = SUM / COUNT
         IF (COUNTA.GE.1) THEN
            MAVG = SUMA / COUNTA
         ELSE
            MAVG = 1.0E20
            END IF
         DO 150 LOOP = 1,NAMP
            UVAMP(1,LOOP) = MIN (UVAMP(1,LOOP), MAVG)
            UVAMP(2,LOOP) = MAX (UVAMP(2,LOOP), MRMS)
 150        CONTINUE
         END IF
       GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR DETERMINING UVDATA STATISTICS'
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE SCCLIN (CLEAN, DONE, MFIELD, NCCRES, RMS, IERR)
C-----------------------------------------------------------------------
C   Initialize cleaning information, use CC ver 1.
C   The image is not restored except for the final CLEAN.
C   The CLEANing is to stop at 1.0 times the previous RMS residual
C   unless a previously lower value of MINFLUX on CLEAN was set.
C      On the final CLEAN the number of components to use for a restart
C   is the total number from the previous CLEAN (after compression).
C   Also, the minimum flux is removed and the components are restored.
C   Inputs:
C      CLEAN    C*32   CLEAN object
C      DONE     L      If true then this is the final clean.
C      MFIELD   I      Number of fields
C      NCCRES   I(*)   Number of CC to use for restart.
C      RMS      R      RMS residual.
C   Output:
C      IERR     I      Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER CLEAN*(*)
      LOGICAL   DONE
      INTEGER   MFIELD, NCCRES(*), IERR
      REAL      RMS
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   TYPE, DIM(7), VER, BCOMP(MAXFLD), CCSUM, I
      REAL      FMIN
      LOGICAL   NOREST
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
C                                       Write CC version 1
      VER = 1
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (CLEAN, 'VERSION', OOAINT, DIM, VER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Don't restore until done
      DIM(1) = 1
      NOREST = .NOT. DONE
      CALL OPUT (CLEAN, 'NORESTOR', OOALOG, DIM, NOREST, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Begin component
      CALL FILL (MAXFLD, 0, BCOMP)
      DIM(1) = MAXFLD
      CALL COPY (MFIELD, NCCRES, BCOMP)
      CALL ISUM (MFIELD, NCCRES, CCSUM)
      CALL OPUT (CLEAN, 'BCOMP', OOAINT, DIM, BCOMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       restart info
      IF ((MFIELD.GT.1) .AND. (CCSUM.GT.0)) THEN
         DO 10 I = 1,MFIELD
            WRITE (MSGTXT,1000) I, NCCRES(I)
            CALL MSGWRT (2)
 10         CONTINUE
         END IF
C                                       Min flux level
      IF (RMS.GT.1.0E-20) THEN
         CALL OGET (CLEAN, 'MINFLUX', TYPE, DIM, FMIN, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (FMIN.GT.0.0) FMIN = MIN (FMIN, RMS)
         IF (FMIN.EQ.0) FMIN = RMS
         IF (DONE) FMIN = 0.0
         CALL OPUT (CLEAN, 'MINFLUX', TYPE, DIM, FMIN, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR INITIALIZING CLEAN'
      CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Field',I5,' will restart using',I7,' components')
      END
      SUBROUTINE SCSTAT (MFIELD, CLEANO, FREUSE, NEGOK, ROS, ILOOP,
     *   ROSLST, IERR)
C-----------------------------------------------------------------------
C   Get residual image statistics (ignoring outer 5 pixels) and
C   determine the minimum brightness merged CLEAN component to use for
C   restarting the next CLEAN.  This value is SIGMA times the RMS
C   residual where SIGMA is a value attached to the CLEANO image.
C      Also determined is the RMS / sum of CC to use as a convergence
C   criteria.
C   Inputs:
C      MFIELD   I        Number of fields
C      CLEANO   C(*)*?   CLEAN residual object.
C      ILOOP    I        Subscript into ROSLST - iter #
C   Inputs attached to CLEANO:
C      SIGMA    R        Reuse merged CC brighter than SIGMA times the
C                        residual RMS.
C   In/out:
C      ROSLST   R(*)     List of RMS residuals
C   Output:
C      FREUSE   R        Minimum brightness merged CC to reuse.
C      ROS      R(2)     (1) = RMS residual. (2) = RMS / Sum flux
C      IERR     I        Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   MFIELD, ILOOP, IERR
      CHARACTER CLEANO(*)*(*)
      REAL      FREUSE, ROS(2), ROSLST(*)
      LOGICAL   NEGOK
C
      INTEGER   TYPE, DIM(7), NAXIS(7), BLC(7), TRC(7), CCVER, IL, I
      REAL      SIGMA, SUMCC, XT, XXT(5), RR, SS
      LOGICAL   PFLAG
      CHARACTER CCTAB*32, PREFIX*5, CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
C                                       Get statistics ignoring outer 5
C                                       pixels.
      ROS(1) = 0.0
      SUMCC = 0.0
      DO 100 I = 1,MFIELD
         CALL ARDGET (CLEANO(I), 'NAXIS', TYPE, DIM, NAXIS, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         CALL FILL (7, 1, BLC)
         CALL FILL (7, 1, TRC)
         BLC(1) = 5
         BLC(2) = 5
         TRC(1) = NAXIS(1) - 5
         TRC(2) = NAXIS(2) - 5
C                                       Set window
         DIM(1) = 7
         DIM(2) = 1
         CALL ARDPUT (CLEANO(I), 'BLC', OOAINT, DIM, BLC, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL ARDPUT (CLEANO(I), 'TRC', OOAINT, DIM, TRC, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Set array statistics
         CALL ARSSET (CLEANO(I), IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Get RMS
         CALL ARSGET (CLEANO(I), 'DATARMS', TYPE, DIM, RR, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         ROS(1) = ROS(1) + RR
C                                       Get sum of flux
C                                       Temporary CC object
         CCTAB = 'Temp CC table for SCSTAT'
         CCVER = 1
         CALL IM2TAB (CLEANO(I), CCTAB, 'CC', CCVER, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL CCSUM (CCTAB, SS, IERR)
         IF (IERR.NE.0) GO TO 990
         SUMCC = SUMCC + SS
C                                       Destroy temporary CC object
         CALL TABDES (CCTAB, IERR)
         IF (IERR.NE.0) GO TO 990
 100     CONTINUE
C                                       Set FREUSE
      CALL OGET (CLEANO(1), 'USENEGS', TYPE, DIM, NEGOK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (CLEANO(1), 'SIGMA', TYPE, DIM, SIGMA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      ROS(1) = ROS(1) / MFIELD
      FREUSE = SIGMA * ROS(1)
C                                       Tell user
      XT = FREUSE
      CALL METSCA (XT, PREFIX, PFLAG)
      WRITE (MSGTXT,1005) XT, PREFIX
      CALL MSGWRT (4)
      IF (SUMCC.GT.0.0) THEN
         ROS(2) = ROS(1) / SUMCC
      ELSE
         ROS(2) = ROS(1)
         END IF
C                                       Tell user
      XT = ROS(1)
      CALL METSCA (XT, PREFIX, PFLAG)
      WRITE (MSGTXT,1000) XT, PREFIX
      CALL MSGWRT (5)
      XT = ROS(2)
      CALL METSCA (XT, PREFIX, PFLAG)
      WRITE (MSGTXT,1001) XT, PREFIX
      CALL MSGWRT (5)
      IF ((ILOOP.GE.2) .AND. (ROS(1).NE.0.0)) THEN
         IL = MIN (5, ILOOP-1)
         DO 10 I = 1,IL
            XXT(I) = ROSLST(ILOOP-I) * XT / ROS(2)
 10         CONTINUE
         WRITE (MSGTXT,1002) IL, (XXT(I), I = 1,IL)
         CALL MSGWRT (4)
         END IF
      ROSLST(ILOOP) = ROS(2)
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR FINDING CLEAN RESIDUAL STATISTICS'
      CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RMS CLEAN residual = ',F8.3,1X,A,'Jy')
 1001 FORMAT ('RMS CLEAN residual/flux = ',F8.3,1X,A,'ratio')
 1002 FORMAT ('Last ',I1,' residuals =',5F9.3)
 1005 FORMAT ('Will reuse merged components brighter than ',F8.3,1X,A,
     *   'Jy')
      END
      SUBROUTINE SCONVG (MFIELD, UVDATA, UVWORK, SNTAB, UVOUT, ROS,
     *   ROSMIN, BESTUV, BESTSN, CONVG, NCCRES, FLIPED, IERR)
C-----------------------------------------------------------------------
C   Routine to check for convergence and if necessary switch from phase
C   only to amplitude and phase solutions.  The convergence criterion is
C   the clean residual RMS / (sum of the flux) called ROS.
C      Convergence is declared when ROS > ROSMIN * 1.10,  or more than 3
C   iterations have passed since the minimum ROS
C      A mode switch is done when phase only calibration has converged
C   and either the value of 'SWITCH' on UVOUT is true or SOLMODE is ' '
C   If this switch is done then a number of changes are made:
C   1) The previous calibration (SNTAB) is applied to UVDATA and written
C      to the UVOUT object.
C   2) A new scratch object is copied from UVOUT and returned as UVWORK.
C   3) UVOUT (output of the task) is assigned to UVDATA
C   4) All selected IFs and polarizations are averaged in self
C      calibration, the solution interval is doubled (30
C      sec if not previously set), the minimum number of antennas for a
C      valid solution is set to at least 4, and the flux scale is
C      constrained.
C   If a new UVWORK object cannot be created then the mode switch is
C   not attempted and convergence is declared.
C   Inputs:
C      MFIELD   I      Number fields
C      UVOUT    C*32   UV data with self cal control info
C      ROS      R(3)   (1) RMS residual for this iteration.
C                      (2) ROS for this iteration.
C                      (3) iteration number
C   Input/output:
C      UVDATA   C*32   Current working uv data; replaced with a
C                      calibrated version of itself if mode switching
C                      occurs.
C      UVWORK   C*32   Current scratch object for UVDATA divided by the
C                      model.
C      SNTAB    C*32   Previous SN table on input, on output the
C                      associated AIPS file will be the relevant one for
C                      UVDATA.
C      ROSMIN   R(2)   (1) Minimum RMS so far.
C                      (2) Minimum ROS so far
C                     (3) iteration number of min.
C      BESTUV   C*?    Name of UV data with best ROS
C      BESTSN   I      SN table version with best resultant ROS
C      FLIPED   L      Set to true if a mode switch has occured
C      NCCRES   I(*)   Number of components for restart, set to 0 if
C                      mode switched.
C      CONVG   L       True if convergence achieved. (to be forced by
C                      input or actually)
C   Output:
C      IERR    I       Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), UVWORK*(*), SNTAB*(*), UVOUT*(*), BESTUV*(*)
      INTEGER   MFIELD, BESTSN, NCCRES(*), IERR
      REAL      ROS(3), ROSMIN(3)
      LOGICAL   CONVG, FLIPED
C
      INTEGER   NKEYN
      PARAMETER (NKEYN = 25)
      INTEGER   TYPE, DIM(7), MINNO, VER, BCHAN, ECHAN, BIF, EIF, NVIS,
     *   DUMMY
      LOGICAL   SWITCH, AVG
      CHARACTER SMOD*4, STOKES*4, METH*4, CDUMMY*1, UVNEW*32, OUTNAM*12,
     *   OUTCLS*6, OUTK5(NKEYN)*32
      REAL      SOLINT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C
      DATA OUTK5 /'REFANT', 'SOLINT', 'SOLTYPE', 'SOLMODE', 'SOLCON',
     *   'ANTWT', 'GAINERR', 'WTUV', 'WEIGHTIT', 'SUBWT', 'SUBREFA',
     *   'REWEIGHT', 'COMPARE', 'SWITCH', 'AVGPOL', 'DOMGM', 'MINNO',
     *   'AVGIF', 'PRTLV', 'SNRMIN', 'EQU_TIME', 'SMOTYPE',
     *   'SMOPHASE', 'SMOAMP', 'MODMETH'/
C-----------------------------------------------------------------------
C                                       Converged?
      CONVG = ((ROS(2).GT.(ROSMIN(2) * 1.10)) .AND. (ROS(3).GT.2.0))
     *   .OR. CONVG
C                                       1st CLEAN done deeper so don't
C                                       keep it as the best.
      IF ((ROS(2).LT.ROSMIN(2)) .OR. (ABS (ROSMIN(3)-1.0) .LT. 0.1))
     *   THEN
C                                       Best ROS
         ROSMIN(1) = ROS(1)
         ROSMIN(2) = ROS(2)
         ROSMIN(3) = ROS(3)
C                                       Save uv data, SN ver.
         BESTUV = UVDATA
         CALL OGET (SNTAB, 'VER', TYPE, DIM, BESTSN, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      CONVG = CONVG .OR. ((ROS(3)-ROSMIN(3)).GE.3.0)
C                                       Test if mode switching is
C                                       enabled.
      CALL OGET (UVOUT, 'SWITCH', TYPE, DIM, SWITCH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (UVOUT, 'SOLMODE', TYPE, DIM, DUMMY, SMOD, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       SOLMODE may have been changed by
C                                       a TELL.
      IF (SMOD.EQ.' ') THEN
         SWITCH = .TRUE.
         IF (SMOD.EQ.' ') SMOD = 'P'
         CALL OPUT (UVOUT, 'SOLMODE', TYPE, DIM, DUMMY, SMOD, IERR)
         IF (IERR.NE.0) GO TO 990
         DIM(1) = 1
         DIM(2) = 1
         CALL OPUT (UVOUT, 'SWITCH', OOALOG, DIM, SWITCH, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Time to SWITCH?
      IF ((CONVG) .AND. (SWITCH)) THEN
C                                       Attempt to create scratch uv
C                                       data
         UVNEW = 'UV DATA after mode switch'
C                                       Don't do it again
         IF (UVDATA.EQ.UVNEW) GO TO 900
         CALL OGET (UVDATA, 'NAME', TYPE, DIM, DUMMY, OUTNAM, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OPUT (UVOUT, 'OUTNAME', TYPE, DIM, DUMMY, OUTNAM, IERR)
         IF (IERR.NE.0) GO TO 990
         DIM(1) = 6
         OUTCLS = 'SWTCHD'
         CALL OPUT (UVOUT, 'OUTCLASS', TYPE, DIM, DUMMY, OUTCLS, IERR)
         IF (IERR.NE.0) GO TO 990
         DIM(1) = 1
         TYPE = OOAINT
         DUMMY = 0
         CALL OPUT (UVOUT, 'OUTDISK', TYPE, DIM, DUMMY, OUTCLS, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OPUT (UVOUT, 'OUTSEQ', TYPE, DIM, DUMMY, OUTCLS, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Get IF channel selection
         CALL SECSLT (UVOUT, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       All Stokes', IF, channels
         CALL SECSAV (UVOUT, 1, 0, 1, 0, '    ', IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Create
         CALL OUVCLN (UVOUT, UVNEW, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Reset IF channel selection
         CALL SECSAV (UVOUT, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Copy info from UVOUT for self
C                                       calibration.
         CALL IN2OBJ (UVOUT, NKEYN, OUTK5, OUTK5, UVNEW, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Get IF channel selection
         CALL SECSLT (UVDATA, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       All Stokes', IF, channels
         CALL SECSAV (UVDATA, 1, 0, 1, 0, '    ', IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Set up to apply calibration
         CALL OGET (SNTAB, 'VER', TYPE, DIM, VER, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OPUT (UVDATA, 'CALEDIT.CLUSE', TYPE, DIM, VER, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OPUT (UVDATA, 'CALEDIT.DOCAL', OOALOG, DIM, .TRUE.,
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Start at beginning of output
         DIM(1) = 1
         CALL OPUT (UVNEW, 'UV_DESC.VISOFF', OOAINT, DIM, 0, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Calibrate old to new.
         CALL UVCOPY (UVDATA, UVNEW, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Create new scratch object.
C                                       number of vis.
         CALL UVDGET (UVDATA, 'GCOUNT', TYPE, DIM, NVIS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Reset IF channel selection
         CALL SECSAV (UVNEW, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Copy MODMETH
         CALL OGET (UVDATA, 'MODMETH', TYPE, DIM, DUMMY, METH, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OPUT (UVNEW, 'MODMETH', TYPE, DIM, DUMMY, METH, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       New working data
         UVDATA = UVNEW
C                                       Complete mode switch
         MSGTXT = 'Switching to amplitude and phase solutions'
         CALL MSGWRT (5)
         CONVG = .FALSE.
C                                       Set external mode switched flag
         FLIPED = .TRUE.
C                                       SNTAB should be attached to
C                                       UVDATA.
         CALL DESTRY (SNTAB, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL UV2TAB (UVDATA, SNTAB, 'SN', 0, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Reset SWITCH
         DIM(1) = 1
         DIM(2) = 1
         SWITCH = .FALSE.
         CALL OPUT (UVNEW, 'SWITCH', OOALOG, DIM, SWITCH, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OPUT (UVOUT, 'SWITCH', OOALOG, DIM, SWITCH, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OPUT (UVWORK, 'SWITCH', OOALOG, DIM, SWITCH, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Reset SOLMODE
         IF (SMOD.EQ.'P') SMOD = 'A&P '
         DIM(1) = 4
         DIM(2) = 1
         CALL OPUT (UVWORK, 'SOLMODE', OOACAR, DIM, DUMMY, SMOD, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Average all
         AVG = .TRUE.
         DIM(1) = 1
         CALL OPUT (UVWORK, 'AVGIF', OOALOG, DIM, AVG, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OPUT (UVWORK, 'AVGPOL', OOALOG, DIM, AVG, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Constrain flux scale.
         CALL OPUT (UVWORK, 'DOMGM', OOALOG, DIM, AVG, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       MINNO must be at least 4
         CALL OGET (UVWORK, 'MINNO', TYPE, DIM, MINNO, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         MINNO = MAX (MINNO, 4)
         CALL OPUT (UVWORK, 'MINNO', TYPE, DIM, MINNO, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Double SOLINT
         CALL OGET (UVWORK, 'SOLINT', TYPE, DIM, SOLINT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         SOLINT = SOLINT * 2.0
C                                       Use 30 sec. if unspecified.
         IF (SOLINT.LE.0.0) SOLINT = 0.5
         CALL OPUT (UVWORK, 'SOLINT', TYPE, DIM, SOLINT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         WRITE (MSGTXT,1012) SOLINT
         CALL MSGWRT (4)
C                                       Reset number of components to
C                                       reuse to 0.
         CALL FILL (MFIELD, 0, NCCRES)
         END IF
C                                       Give message if converged (or
C                                       diverged)
 900  IF (CONVG) THEN
         IF (ROS(2).GT.(ROSMIN(2) * 1.50)) THEN
            MSGTXT = 'SOLUTION DIVERGING'
            CALL MSGWRT (8)
            IERR = 5
            GO TO 999
         ELSE
            MSGTXT = 'Convergence achieved'
            CALL MSGWRT (5)
            END IF
         END IF
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR DETERMINING CONVERGENCE'
      CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1012 FORMAT ('New solution interval = ',F10.5, ' Min.')
      END
      SUBROUTINE SCLIT (MFIELD, UVIN, CLEANO, SNTAB, UVWORK, UVSCR,
     *   FREUSE, NEGOK, NAMP, UVAMP, UVRAN, NCCRES, IERR)
C-----------------------------------------------------------------------
C   Routine to calibrate with a CLEAN model.  Calibrated, selected data
C   are written to an output scratch object (UVSCR).
C   CC version 1 associated with CLEANO is assumed to be the table
C   version used.  This table is merged and information extracted which
C   is used for the UV range given full weight and the number of CLEAN
C   components to use in the self calibration.   The method is to
C   use CCs up to the one before the first negative merged component and
C   a UV_FULL outward from the maximum uv spacing with a flux in excess
C   of the sum of the CC fluxes up to the first negative.  An adjustment
C   is made in the latter test for scatter in the amplitude vs. baseline
C   length function.
C      Also, the number of CC to restart the CLEAN is returned.  This
C   component number is the last component number in excess of FREUSE.
C   Inputs:
C      MFIELD   I      Number of fields
C      UVIN     C*32   Input uv object
C      CLEANO   C*32   CLEAN image
C      SNTAB    C*32   SN table object, associated with input uv data.
C      UVWORK   C*32   Output file used for data divided by model
C      UVSCR    C*32   Scratch uv object.
C      FREUSE   R      Minimum brightness flux for CLEAN restart.
C      NAMP     I      Number of entries in UVAMP
C      UVAMP    R(5,*) Average visibility amplitudes as a function of UV
C                      distance.
C                       (1,*) = average amplitude
C                       (2,*) = RMS of average amplitude
C                       (3,*) = central uv distance (klamda),
C                       (4,*) = sum of weights
C                       (5,*) Number of amplitudes averaged.
C      UVRAN    R(2)   UV range in selected data (kilowavelengths)
C   Output:
C      NCCRES   I(*)   Number of CC when restarting next CLEAN
C      IERR     I      Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), CLEANO(*)*(*), SNTAB*(*), UVWORK*(*),
     *   UVSCR*(*)
      INTEGER   MFIELD, NAMP, NCCRES(*), IERR
      REAL      FREUSE, UVAMP(5,NAMP), UVRAN(2)
      LOGICAL   NEGOK
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   TYPE, DIM(7), BCHAN, ECHAN, NCHAN, BIF, EIF, REFANT(51),
     *   VER, CCVER, CCBEG(MAXFLD), CCEND(MAXFLD), NSKOL, I, ILAST,
     *   NVIN, NVOUT, NSUBA, ISUBA, ANVER, DUMMY
      REAL     TOLER(2), UVR(2), SUMPOS, CDELT(7), FLXMAX, SS
      CHARACTER MODEL*4, STOKES*4, CCTAB*32, MKOL(2)*32, UVDOBJ*32,
     *   SKOL(1)*32, SRTKOL(2)*32, ANTAB*32, CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
C                                       Message
      MSGTXT = 'Determining new calibration from CLEAN model'
      CALL MSGWRT (4)
      SUMPOS = 0.0
C                                       loop over fields
      DO 100 I = 1,MFIELD
C                                       Merge CC TABLE
C                                       Temporary CC object
         CCTAB = 'Temp CC table for SCLIT'
         CCVER = 1
         CALL IM2TAB (CLEANO(I), CCTAB, 'CC', CCVER, IERR)
         IF (IERR.NE.0) GO TO 990
         MKOL(1) = 'DELTAX'
         MKOL(2) = 'DELTAY'
C                                       Axis increments
         CALL IMDGET (CLEANO(I), 'CDELT', TYPE, DIM, CDELT, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         TOLER(1) = 0.5 * ABS (CDELT(1))
         TOLER(2) = 0.5 * ABS (CDELT(2))
         SKOL(1) = 'FLUX'
         NSKOL = 1
         SRTKOL(1) = '-ABS:FLUX'
         SRTKOL(2) = '-ABS:FLUX'
         CALL TBLMRG (CCTAB, MKOL, TOLER, SKOL, NSKOL, SRTKOL, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Save table as CC # 2
C         TMPTAB = 'Temporary CC table'
C         CCVER = 2
C         CALL IM2TAB (CLEANO(I), TMPTAB, 'CC', CCVER, IERR)
C         IF (IERR.NE.0) GO TO 990
C         CALL TBLCOP (CCTAB, TMPTAB, IERR)
C         IF (IERR.NE.0) GO TO 990
C                                       Destroy temporary object
C         CALL TABDES (TMPTAB, IERR)
C         IF (IERR.NE.0) GO TO 990
C                                       CC table info
         CALL CCINF (I, CCTAB, FREUSE, NEGOK, CCEND(I), SS, NCCRES(I),
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         SUMPOS = SUMPOS + SS
C                                       Destroy temporary CC object
         CALL TABDES (CCTAB, IERR)
         IF (IERR.NE.0) GO TO 990
 100     CONTINUE
C                                       All selected frequencies
C                                       Get IF channel selection
      CALL SECSLT (UVIN, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 999
      NCHAN = (ECHAN - BCHAN + 1) * (EIF - BIF + 1)
      NCHAN = MAX (NCHAN, 1)
      UVDOBJ = UVWORK
C                                       Specify model
C                                       NOT point
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (UVDOBJ, 'MODDOPT', OOALOG, DIM, .FALSE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Use CC
      MODEL = 'CC'
      DIM(1) = LEN (MODEL)
      CALL OPUT (UVDOBJ, 'MODMODEL', OOACAR, DIM, DUMMY, MODEL, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CCVER
      DIM(1) = 1
      CCVER = 1
      CALL OPUT (UVDOBJ, 'MODCCVER', OOAINT, DIM, CCVER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CCBEG (from beginning)
      CALL FILL (MFIELD, 1, CCBEG)
      DIM(1) = MFIELD
      CALL OPUT (UVDOBJ, 'MODCCBEG', OOAINT, DIM, CCBEG, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CCEND, thru first neg.
      CALL OPUT (UVDOBJ, 'MODCCEND', OOAINT, DIM, CCEND, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Have already removed negatives.
      CALL OPUT (UVDOBJ, 'MODNONEG', OOALOG, DIM, .FALSE., CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Don't calibrate UVIN here
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (UVIN, 'CALEDIT.DOCAL', OOALOG, DIM, .FALSE., CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Uncompress to UVWORK before
C                                       divide, apply flags
C                                       All Stokes'
      CALL SECSAV (UVIN, BIF, EIF, BCHAN, ECHAN, '    ', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL UVCOPY (UVIN, UVWORK, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Reset selection
      CALL SECSAV (UVIN, BIF, EIF, BCHAN, ECHAN, STOKES, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Divide by model
      CALL OUMDIV (UVWORK, UVWORK, MFIELD, CLEANO, BCHAN, NCHAN, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Force new SN table
      CALL TBLHIV (SNTAB, VER, IERR)
      IF (IERR.NE.0) GO TO 990
      VER = VER + 1
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (SNTAB, 'VER', OOAINT, DIM, VER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Find uv range appropriate for
C                                       SUMPOS
      FLXMAX = SUMPOS
      UVR(2) = UVRAN(2)
      ILAST = 0
C                                       Use 1 sigma below average for
C                                       test.
      DO 10 I = 1,NAMP
         IF ((UVAMP(5,I).GT.0.0) .AND.
     *      ((UVAMP(1,I) - UVAMP(2,I)).GT.FLXMAX)) ILAST = I
 10      CONTINUE
      IF (ILAST.GE.NAMP) ILAST = NAMP - 1
      UVR(1) = UVAMP(3,ILAST+1)
C                                       Set values on object
      DIM(1) = 2
      DIM(2) = 1
      CALL OPUT (UVWORK, 'UVR_FULL', OOARE, DIM, UVR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Tell UV_FULL
      UVR(1) = UVR(1) * 1.0E3
      UVR(2) = UVR(2) * 1.0E3
      WRITE (MSGTXT,1010) UVR
      CALL MSGWRT (4)
      UVR(1) = UVR(1) / 1.0E3
      UVR(2) = UVR(2) / 1.0E3
C                                       No calibration of UVWORK
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (UVWORK, 'CALEDIT.DOCAL', OOALOG, DIM, .FALSE., CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Higher numbered subarrays
      CALL OGET (UVIN, 'SUBREFA', TYPE, DIM, REFANT(1), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (UVIN, 'REFANT', TYPE, DIM, REFANT(1), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Number of subarrays
      ANTAB = 'Temp AN for SCLIT'
      ANVER = 1
      CALL UV2TAB (UVWORK, ANTAB, 'AN', ANVER, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TBLHIV (ANTAB, NSUBA, IERR)
      IF (IERR.NE.0) GO TO 990
      NSUBA = MAX (1, NSUBA)
C                                       Destroy temp object
      CALL TABDES (ANTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Loop over subarrays
      DO 110 ISUBA = 1,NSUBA
         DIM(1) = 1
         DIM(2) = 1
         CALL OUVPUT (UVWORK, 'CALEDIT.SUBARR', OOAINT, DIM, ISUBA,
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OPUT (UVWORK, 'REFANT', OOAINT, DIM, REFANT(ISUBA),
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Do solutions
         CALL SLFCAL (UVWORK, SNTAB, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Refer to a common reference
C                                       antenna
         CALL SLFREF (SNTAB, ISUBA, REFANT(ISUBA), IERR)
         IF (IERR.NE.0) GO TO 990
C                                       smooth, interpolate
         CALL SLFSMO (UVWORK, SNTAB, ISUBA, IERR)
         IF (IERR.NE.0) GO TO 990
 110     CONTINUE
C                                       Apply to output data
C                                       Set up to apply calibration
      CALL OGET (SNTAB, 'VER', TYPE, DIM, VER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (UVIN, 'CALEDIT.CLUSE', TYPE, DIM, VER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (UVIN, 'CALEDIT.DOCAL', OOALOG, DIM, .TRUE., CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Start at beginning of output
      DIM(1) = 1
      CALL OPUT (UVSCR, 'UV_DESC.VISOFF', OOAINT, DIM, 0, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Message
      WRITE (MSGTXT,1011) VER
      CALL MSGWRT (4)
      IF (VER.GT.20000) THEN
         MSGTXT = 'Warning: there is a maximum of 46655 SN tables'
         CALL MSGWRT (6)
         END IF
C                                       Calibrate/copy
      CALL SC2SCR (UVIN, UVSCR, CLEANO(1), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Tell how many vis left
      CALL UVDGET (UVIN, 'GCOUNT', TYPE, DIM, NVIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDGET (UVSCR, 'GCOUNT', TYPE, DIM, NVOUT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGTXT,1012) NVOUT, NVIN
      CALL MSGWRT (5)
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR CALIBRATING WITH CLEAN MODEL'
      CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Self-cal uv range =',1PE11.3,' to',E11.3,' wavelengths')
 1011 FORMAT ('New calibration table is SN version ',I4)
 1012 FORMAT ('Using ', I8, ' of ', I8, ' visibilities.')
      END
      SUBROUTINE SCLOUT (UVIN, SNTAB, UVOUT, IERR)
C-----------------------------------------------------------------------
C   Applies an SN table associated with the input uv data  and writes an
C   output uv data.  All channels, IFs and Stokes' are calibrated and
C   written to UVOUT.
C   All SN tables attached to UVOUT are deleted as they cannot be
C   correct.
C   Inputs:
C      UVIN    C*32   Input uv object
C      SNTAB   C*32   SN table object, associated with input uv data.
C      UVOUT   C*32   Output UV data
C   Output:
C      IERR    I      Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), SNTAB*(*), UVOUT*(*)
      INTEGER   IERR
C
      INTEGER   TYPE, DIM(7), VER
      LOGICAL   INOUT
      CHARACTER UVTMP*32, CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
C                                       Message
      MSGTXT = 'Applying final calibration to uv data.'
      CALL MSGWRT (5)
C                                       If input is output then make a
C                                       temporary object
      INOUT = UVIN .EQ. UVOUT
      IF (INOUT) THEN
         UVTMP = 'Temporary UVDATA for SCLOUT'
         CALL OUVCOP (UVOUT, UVTMP, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         UVTMP = UVOUT
         END IF
C                                       All Stokes', IF, channels
      CALL SECSAV (UVIN, 1, 0, 1, 0, '    ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Apply to output data
C                                       Set up to apply calibration
      CALL OGET (SNTAB, 'VER', TYPE, DIM, VER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (UVIN, 'CALEDIT.CLUSE', TYPE, DIM, VER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (UVIN, 'CALEDIT.DOCAL', OOALOG, DIM, .TRUE., CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Start at beginning of output
      DIM(1) = 1
      CALL OPUT (UVTMP, 'UV_DESC.VISOFF', OOAINT, DIM, 0, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVCOPY (UVIN, UVTMP, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Delete temporary object if
C                                       necessary
      IF (INOUT) THEN
         CALL OUVDES (UVTMP, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Attempt to delete any SN tables
C                                       on UVOUT.
      CALL DESTRY (SNTAB, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       DEBUG - leave SN tables
      CALL UV2TAB (UVOUT, SNTAB, 'SN', -1, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL TABRMV (SNTAB, IERR)
      IF (IERR.NE.0) GO TO 900
C
 900  IERR = 0
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR CALIBRATING OUTPUT UVDATA'
      CALL MSGWRT (8)
 999  RETURN
      END
      SUBROUTINE SCTELL (CLEAN, UVOUT, MXLOOP, DONE, IERR)
C-----------------------------------------------------------------------
C   Sees if there are new instructions from the boss.
C   New values of most control parameters are written to the object to
C   which they are attached.
C   Inputs:
C      CLEAN   C*?  Name of CLEAN process object
C      UVOUT   C*32 Output uv object (self cal info)
C   Output:
C      MXLOOP  I     Maximum self cal loop number
C      DONE    L     Stop now
C      IERR    I     0=> keep going, else die now.
C-----------------------------------------------------------------------
      CHARACTER CLEAN*(*), UVOUT*(*)
      INTEGER   MXLOOP, IERR
      LOGICAL   DONE
C
      INTEGER   TYPE, DIM(7), DUMMY, JERR, IROUND, MSGSAV, IMSI(2)
      REAL      WTUV, SOLINT, BMAJ, BMIN, IM2PRM(40), AUTOBX(6)
      CHARACTER OBTELL*32, OPTELL*4, CVALUE*48
      INTEGER   NKEY1, NKEY2, NKEYC
      LOGICAL   EXIST
C                                       NKEY1=no. adverbs to copy to
C                                       CLEAN object
      PARAMETER (NKEY1=9)
C                                       NKEY2 = no. adverbs for UVOUT
      PARAMETER (NKEY2=11)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUTT.INC'
      INCLUDE 'INPUTTATA.INC'
C                                       Adverbs to copy to CLEAN object
C                   1        2       3           4
      DATA INK1 / 'GAIN', 'FLUX', 'MINPATCH', 'NITER',
C           5      6         7       8           9
     *   'BPA', 'FACTOR', 'DOTV', 'MAXPIXEL', 'DOTV'/
C                                       Rename
C                     1       2          3           4
      DATA OUTK1 / 'GAIN', 'MINFLUX', 'MINPATCH', 'NITER',
C           5           6         7          8          9
     *   'BEAM.BPA', 'FACTOR', 'SCTVFLD', 'MAXNRES', 'TVFIELD'/
C                                       Adverbs for UVOUT (selfcal)
C                   1          2         3        4         5
      DATA INK2 /'REFANT', 'SOLINT', 'SOLTYPE', 'SOLMODE', 'SOLCON',
C           6         7        8
     *   'ANTWT', 'GAINERR', 'WTUV', 'DOWEIGHT', 'DOTWO', 'WEIGHTIT'/
C                   1          2         3         4         5
      DATA OUTK2 /'REFANT', 'SOLINT', 'SOLTYPE', 'SOLMODE', 'SOLCON',
C           6         7          8
     *   'ANTWT', 'GAINERR', 'WTUV', 'REWEIGHT', 'COMPARE', 'WEIGHTIT'/
C-----------------------------------------------------------------------
      DONE = .FALSE.
      EXIST = .TRUE.
C                                       Get CLEAN Tell object
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OGET (CLEAN, 'TELLNAME', TYPE, DIM, DUMMY, OBTELL, IERR)
      MSGSUP = MSGSAV
      IF ((IERR.NE.0) .OR. (OBTELL.EQ.'     ')) THEN
         OBTELL = 'TELL INPUT object for SCIMG'
         EXIST = .FALSE.
         IERR = 0
         END IF
C                                       If it exists - it was created
C                                       somewhere else - parse it.
      IF (EXIST) THEN
         CALL OBFEXS (OBTELL, EXIST, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       Use previously fetched TELL
      IF (EXIST) THEN
         MSGSUP = 32000
         CALL OGET (OBTELL, 'OPTELL', TYPE, DIM, DUMMY, OPTELL, IERR)
         MSGSUP = MSGSAV
         IF (IERR.GT.1) GO TO 990
         END IF
C                                       Check for new TELL
C                                       Previous one - leave TVFIELD out
      NKEYC = NKEY1 - 1
      IF ((.NOT.EXIST) .OR. (IERR.EQ.1)) THEN
         CALL INTELL (NKEYT, TELK, TELTYP, TELDIM, OBTELL, OPTELL, IERR)
         IF (IERR.NE.0) GO TO 990
         NKEYC = NKEY1
         END IF
C                                       Anything to do?
      IF (OPTELL.EQ.' ') THEN
         IERR = 0
      ELSE IF (OPTELL.EQ.'ABOR') THEN
         MSGTXT ='I will fall on my sword immediately!'
         CALL MSGWRT (8)
         IERR = 9
      ELSE IF (OPTELL.EQ.'QUIT') THEN
         MSGTXT ='Righto Boss, I will quit now.'
         CALL MSGWRT (5)
         IERR = 0
         DONE = .TRUE.
      ELSE IF (OPTELL.EQ.'CHAN') THEN
         MSGTXT ='Righto Boss, I got the message.'
         CALL MSGWRT (5)
C                                       Check adverbs re defaults
         CALL OGET (OBTELL, 'BMAJ', TYPE, DIM, BMAJ, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         BMAJ = BMAJ / 3600.0
         CALL OPUT (CLEAN, 'BEAM.BMAJ', TYPE, DIM, BMAJ, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OGET (OBTELL, 'BMIN', TYPE, DIM, BMIN, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         BMIN = BMIN / 3600.0
         CALL OPUT (CLEAN, 'BEAM.BMIN', TYPE, DIM, BMIN, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OGET (OBTELL, 'WTUV', TYPE, DIM, WTUV, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (WTUV.LE.0.0) WTUV = 0.05
         CALL OPUT (OBTELL, 'WTUV', TYPE, DIM, WTUV, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OGET (OBTELL, 'SOLINT', TYPE, DIM, SOLINT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (SOLINT.LE.0.0) THEN
            CALL OGET (UVOUT, 'SOLINT', TYPE, DIM, SOLINT, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         CALL OPUT (OBTELL, 'SOLINT', TYPE, DIM, SOLINT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       autoboxing
         CALL OGET (OBTELL, 'IM2PARM', TYPE, DIM, IM2PRM, CDUMMY, IERR)
         IF (IERR.EQ.0) THEN
            CALL OGET ('Input', 'IMSIZE', TYPE, DIM, IMSI, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            AUTOBX(1) = IROUND (IM2PRM(1))
            AUTOBX(2) = IM2PRM(2)
            AUTOBX(3) = IM2PRM(3)
            AUTOBX(4) = IM2PRM(4)
            AUTOBX(5) = IROUND (IM2PRM(5))
            AUTOBX(6) = IROUND (IM2PRM(6))
            AUTOBX(1) = MIN (25.0, AUTOBX(1))
            IF (AUTOBX(2).LE.1.5) AUTOBX(2) = 3.
            IF (AUTOBX(3).LT.AUTOBX(2)) AUTOBX(3) = AUTOBX(2) + 2.
            IF (AUTOBX(4).LT.0.01) AUTOBX(4) = 0.1
            IF (AUTOBX(4).GT.0.90) AUTOBX(4) = 0.1
            IF (AUTOBX(5).LT.0.0) AUTOBX(5) = 1.0
            IF (AUTOBX(5).GT.6.0) AUTOBX(5) = 6.0
            IF (AUTOBX(6).LT.1.0) AUTOBX(6) = 5.0
            IF (AUTOBX(6).GT.IMSI(1)/20.0) AUTOBX(6) = 5.0
            DIM(1) = 6
            CALL OPUT (CLEAN, 'AUTOBOX', OOARE, DIM, AUTOBX, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            CALL RCOPY (6, AUTOBX, IM2PRM)
            END IF
C                                       OBOXFILE
         CALL OGET (OBTELL, 'OBOXFILE', TYPE, DIM, DUMMY, CVALUE, IERR)
         IF (IERR.EQ.0) THEN
            IF (CVALUE.NE.' ') THEN
               CALL OBXFIX (CLEAN, CVALUE, IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            END IF
C                                       Copy adverbs to objects
         CALL IN2OBJ (OBTELL, NKEYC, INK1, OUTK1, CLEAN, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL IN2OBJ (OBTELL, NKEY2, INK2, OUTK2, UVOUT, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Update INPUT for history
         CALL IN2OBJ (OBTELL, NKEYT, TELK, TELK, 'Input', IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Get new maximum loop number
         CALL INGET (OBTELL, 'NMAPS', TYPE, DIM, MXLOOP, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Delete tell object
 900  CALL OBFREE (OBTELL, JERR)
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR SEEKING GUIDANCE FROM THE USER'
      CALL MSGWRT (6)
      CALL OBFREE (OBTELL, JERR)
      IERR = 0
C
 999  RETURN
      END
      SUBROUTINE SCPARM (CLEAN, CLEANO, UVDATA, UVOUT, MXLOOP, ITVFLD,
     *   IERR)
C-----------------------------------------------------------------------
C   Get new instructions from the boss "manually"
C   New values of most control parameters are written to the object to
C   which they are attached.
C   Inputs:
C      CLEAN    C*?   Name of CLEAN process object
C      CLEANO   C*?   Name of CLEAN output object
C      UVDATA   C*?   Input uv object (self cal info)
C      UVOUT    C*?   Output uv object (self cal info)
C   In/out:
C      ITVFLD   I     TV field to display during Clean
C   Output:
C      MXLOOP   I     Maximum self cal loop number
C      IERR     I     0=> keep going, else die now.
C-----------------------------------------------------------------------
      CHARACTER CLEAN*(*), UVOUT*(*), CLEANO(*)*(*), UVDATA*(*)
      INTEGER   MXLOOP, ITVFLD, IERR
C
      INTEGER   TYPE, DIM(7), DUMMY, ITEMP, TTY(2), KBL, KBP, I
      REAL      RTEMP
      CHARACTER MSGBUF*72, KB*72, CDUMMY*1, CTEMP*4
      DOUBLE PRECISION DX
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      TTY(1) = 5
      TTY(2) = 0
C                                       Open terminal for conversation
      CALL ZOPEN (TTY(1), TTY(2), 1, MSGBUF, .FALSE., .TRUE., .TRUE.,
     *   IERR)
      IF (IERR.NE.0) THEN
         TTY(2) = 0
         WRITE (MSGTXT,1005) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      TTY(2) = MAX (1, TTY(2))
      MSGBUF ='Enter Q to stop questions, <CR> to take current value'
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       SC iterations
      WRITE (MSGBUF,1000) 'Max self-cal cycles', MXLOOP
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      IF (KB.NE.' ') THEN
         CALL CHTRIM (KB, 72, KB, KBL)
         IF ((KB(1:1).EQ.'Q') .OR. (KB(1:1).EQ.'q')) GO TO 990
         KBP = 1
         CALL GETNUM (KB, KBL, KBP, DX)
         IF (DX.EQ.DBLANK) GO TO 990
         I = DX + 0.5
         IF (I.GT.0) MXLOOP = I
         END IF
C                                       DOTV
      MSGBUF = 'Do TV displays: 0 no, <0 SC only'
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      IF (KB.NE.' ') THEN
         CALL CHTRIM (KB, 72, KB, KBL)
         IF ((KB(1:1).EQ.'Q') .OR. (KB(1:1).EQ.'q')) GO TO 990
         KBP = 1
         CALL GETNUM (KB, KBL, KBP, DX)
         IF (DX.EQ.DBLANK) GO TO 990
         IF (DX.GT.0.0D0) THEN
            ITVFLD = DX + 0.5
         ELSE
            ITVFLD = DX - 0.5
            END IF
         DIM(1) = 1
         DIM(2) = 1
         CALL OPUT (CLEAN, 'SCTVFLD', OOAINT, DIM, ITVFLD, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       SOLINT
      CALL OGET (UVOUT, 'SOLINT', TYPE, DIM, RTEMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF,1001) 'Solution interval in min', RTEMP
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      IF (KB.NE.' ') THEN
         CALL CHTRIM (KB, 72, KB, KBL)
         IF ((KB(1:1).EQ.'Q') .OR. (KB(1:1).EQ.'q')) GO TO 990
         KBP = 1
         CALL GETNUM (KB, KBL, KBP, DX)
         IF (DX.EQ.DBLANK) GO TO 990
         IF (DX.GT.0.0D0) THEN
            RTEMP = DX
            CALL OPUT (UVOUT, 'SOLINT', TYPE, DIM, RTEMP, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
C                                       Smooth amp
      CALL OGET (UVOUT, 'SMOAMP', TYPE, DIM, RTEMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF,1001) 'Amp smoothing time in min', RTEMP
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      IF (KB.NE.' ') THEN
         CALL CHTRIM (KB, 72, KB, KBL)
         IF ((KB(1:1).EQ.'Q') .OR. (KB(1:1).EQ.'q')) GO TO 990
         KBP = 1
         CALL GETNUM (KB, KBL, KBP, DX)
         IF (DX.EQ.DBLANK) GO TO 990
         IF (DX.GT.0.0D0) THEN
            RTEMP = DX
            CALL OPUT (UVOUT, 'SMOAMP', TYPE, DIM, RTEMP, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
C                                       NITER
      CALL OGET (CLEAN, 'NITER', TYPE, DIM, ITEMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF,1000) 'Clean iterations', ITEMP
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      IF (KB.NE.' ') THEN
         CALL CHTRIM (KB, 72, KB, KBL)
         IF ((KB(1:1).EQ.'Q') .OR. (KB(1:1).EQ.'q')) GO TO 990
         KBP = 1
         CALL GETNUM (KB, KBL, KBP, DX)
         IF (DX.EQ.DBLANK) GO TO 990
         IF (DX.GE.1.0D0) THEN
            ITEMP = DX + 0.5
            CALL OPUT (CLEAN, 'NITER', TYPE, DIM, ITEMP, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
C                                       Edit average time
      CALL OGET (UVDATA, 'EQU_TIME', TYPE, DIM, RTEMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF,1001) 'Edit averaging time (min)', RTEMP
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      IF (KB.NE.' ') THEN
         CALL CHTRIM (KB, 72, KB, KBL)
         IF ((KB(1:1).EQ.'Q') .OR. (KB(1:1).EQ.'q')) GO TO 990
         KBP = 1
         CALL GETNUM (KB, KBL, KBP, DX)
         IF (DX.EQ.DBLANK) GO TO 990
         IF (DX.GE.0.0D0) THEN
            RTEMP = DX
            CALL OPUT (UVDATA, 'EQU_TIME', TYPE, DIM, RTEMP, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            CALL OPUT (UVOUT, 'EQU_TIME', TYPE, DIM, RTEMP, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
C                                       SIGMA
      CALL OGET (CLEANO(1), 'SIGMA', TYPE, DIM, RTEMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF,1001) 'Use comp > SIGMA*rms', RTEMP
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      IF (KB.NE.' ') THEN
         CALL CHTRIM (KB, 72, KB, KBL)
         IF ((KB(1:1).EQ.'Q') .OR. (KB(1:1).EQ.'q')) GO TO 990
         KBP = 1
         CALL GETNUM (KB, KBL, KBP, DX)
         IF (DX.EQ.DBLANK) GO TO 990
         IF (DX.GE.2.0D0) THEN
            RTEMP = DX
            CALL OPUT (CLEANO(1), 'SIGMA', TYPE, DIM, RTEMP, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
C                                       FLUX
      CALL OGET (CLEAN, 'MINFLUX', TYPE, DIM, RTEMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF,1001) 'Clean min flux', RTEMP
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      IF (KB.NE.' ') THEN
         CALL CHTRIM (KB, 72, KB, KBL)
         IF ((KB(1:1).EQ.'Q') .OR. (KB(1:1).EQ.'q')) GO TO 990
         KBP = 1
         CALL GETNUM (KB, KBL, KBP, DX)
         IF (DX.EQ.DBLANK) GO TO 990
         IF (DX.GE.0.0D0) THEN
            RTEMP = DX
            CALL OPUT (CLEAN, 'MINFLUX', TYPE, DIM, RTEMP, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
C                                       GAIN
      CALL OGET (CLEAN, 'GAIN', TYPE, DIM, RTEMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF,1001) 'Clean loop gain', RTEMP
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      IF (KB.NE.' ') THEN
         CALL CHTRIM (KB, 72, KB, KBL)
         IF ((KB(1:1).EQ.'Q') .OR. (KB(1:1).EQ.'q')) GO TO 990
         KBP = 1
         CALL GETNUM (KB, KBL, KBP, DX)
         IF (DX.EQ.DBLANK) GO TO 990
         IF (DX.GE.0.001D0) THEN
            RTEMP = DX
            CALL OPUT (CLEAN, 'GAIN', TYPE, DIM, RTEMP, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
C                                       FACTOR
      CALL OGET (CLEAN, 'FACTOR', TYPE, DIM, RTEMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF,1001) 'Clean quit FACTOR', RTEMP
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      IF (KB.NE.' ') THEN
         CALL CHTRIM (KB, 72, KB, KBL)
         IF ((KB(1:1).EQ.'Q') .OR. (KB(1:1).EQ.'q')) GO TO 990
         KBP = 1
         CALL GETNUM (KB, KBL, KBP, DX)
         IF (DX.EQ.DBLANK) GO TO 990
         RTEMP = DX
         CALL OPUT (CLEAN, 'FACTOR', TYPE, DIM, RTEMP, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       MINPATCH
      CALL OGET (CLEAN, 'MINPATCH', TYPE, DIM, ITEMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF,1000) 'Clean min beam patch', ITEMP
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      IF (KB.NE.' ') THEN
         CALL CHTRIM (KB, 72, KB, KBL)
         IF ((KB(1:1).EQ.'Q') .OR. (KB(1:1).EQ.'q')) GO TO 990
         KBP = 1
         CALL GETNUM (KB, KBL, KBP, DX)
         IF (DX.EQ.DBLANK) GO TO 990
         IF (DX.GE.51.0D0) THEN
            ITEMP = DX + 0.5
            CALL OPUT (CLEAN, 'MINPATCH', TYPE, DIM, ITEMP, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
C                                       MAXPIXEL
      CALL OGET (CLEAN, 'MAXNRES', TYPE, DIM, ITEMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF,1000) 'Clean max pixels searched/loop', ITEMP
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      IF (KB.NE.' ') THEN
         CALL CHTRIM (KB, 72, KB, KBL)
         IF ((KB(1:1).EQ.'Q') .OR. (KB(1:1).EQ.'q')) GO TO 990
         KBP = 1
         CALL GETNUM (KB, KBL, KBP, DX)
         IF (DX.EQ.DBLANK) GO TO 990
         IF (DX.GE.1.0D3) THEN
            ITEMP = DX + 0.5
            CALL OPUT (CLEAN, 'MAXNRES', TYPE, DIM, ITEMP, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
C                                       BMAJ
      CALL OGET (CLEAN, 'BEAM.BMAJ', TYPE, DIM, RTEMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF,1001) 'Restoring beam major axis', RTEMP
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      IF (KB.NE.' ') THEN
         CALL CHTRIM (KB, 72, KB, KBL)
         IF ((KB(1:1).EQ.'Q') .OR. (KB(1:1).EQ.'q')) GO TO 990
         KBP = 1
         CALL GETNUM (KB, KBL, KBP, DX)
         IF (DX.EQ.DBLANK) GO TO 990
         IF (DX.GE.0.0D0) THEN
            RTEMP = DX
            CALL OPUT (CLEAN, 'BEAM.BMAJ', TYPE, DIM, RTEMP, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
C                                       BMIN
      CALL OGET (CLEAN, 'BEAM.BMIN', TYPE, DIM, RTEMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF,1001) 'Restoring beam minor axis', RTEMP
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      IF (KB.NE.' ') THEN
         CALL CHTRIM (KB, 72, KB, KBL)
         IF ((KB(1:1).EQ.'Q') .OR. (KB(1:1).EQ.'q')) GO TO 990
         KBP = 1
         CALL GETNUM (KB, KBL, KBP, DX)
         IF (DX.EQ.DBLANK) GO TO 990
         IF (DX.GE.0.0D0) THEN
            RTEMP = DX
            CALL OPUT (CLEAN, 'BEAM.BMIN', TYPE, DIM, RTEMP, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
C                                       BPA
      CALL OGET (CLEAN, 'BEAM.BPA', TYPE, DIM, RTEMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF,1001) 'Restoring beam position angle', RTEMP
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      IF (KB.NE.' ') THEN
         CALL CHTRIM (KB, 72, KB, KBL)
         IF ((KB(1:1).EQ.'Q') .OR. (KB(1:1).EQ.'q')) GO TO 990
         KBP = 1
         CALL GETNUM (KB, KBL, KBP, DX)
         IF (DX.EQ.DBLANK) GO TO 990
         RTEMP = DX
         CALL OPUT (CLEAN, 'BEAM.BPA', TYPE, DIM, RTEMP, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       REFANT
      CALL OGET (UVOUT, 'REFANT', TYPE, DIM, ITEMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF,1000) 'Reference antenna', ITEMP
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      IF (KB.NE.' ') THEN
         CALL CHTRIM (KB, 72, KB, KBL)
         IF ((KB(1:1).EQ.'Q') .OR. (KB(1:1).EQ.'q')) GO TO 990
         KBP = 1
         CALL GETNUM (KB, KBL, KBP, DX)
         IF (DX.EQ.DBLANK) GO TO 990
         IF (DX.GE.0.0D0) THEN
            ITEMP = DX + 0.5
            CALL OPUT (UVOUT, 'REFANT', TYPE, DIM, ITEMP, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
C                                       WTUV
      CALL OGET (UVOUT, 'WTUV', TYPE, DIM, RTEMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF,1001) 'Weight outside UV range', RTEMP
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      IF (KB.NE.' ') THEN
         CALL CHTRIM (KB, 72, KB, KBL)
         IF ((KB(1:1).EQ.'Q') .OR. (KB(1:1).EQ.'q')) GO TO 990
         KBP = 1
         CALL GETNUM (KB, KBL, KBP, DX)
         IF (DX.EQ.DBLANK) GO TO 990
         IF (DX.GT.1.0D-4) THEN
            RTEMP = DX
            CALL OPUT (UVOUT, 'WTUV', TYPE, DIM, RTEMP, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
C                                       SOLTYPE
      CALL OGET (UVOUT, 'SOLTYPE', TYPE, DIM, DUMMY, CTEMP, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF,1002) 'Solution type (blank significant)', CTEMP
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      CALL CHTRIM (KB, 72, KB, KBL)
      IF ((KB(1:1).EQ.'Q') .OR. (KB(1:1).EQ.'q')) GO TO 990
      CTEMP = KB(:4)
      CALL CHLTOU (4, CTEMP)
      IF ((CTEMP.EQ.' ') .OR. (CTEMP.EQ.'L1') .OR. (CTEMP.EQ.'GCON')
     *   .OR. (CTEMP.EQ.'R') .OR. (CTEMP.EQ.'L1R') .OR.
     *   (CTEMP.EQ.'GCOR'))THEN
         CALL OPUT (UVOUT, 'SOLTYPE', TYPE, DIM, DUMMY, CTEMP, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       SOLMODE
      CALL OGET (UVOUT, 'SOLMODE', TYPE, DIM, DUMMY, CTEMP, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF,1002) 'Solution mode (blank significant)', CTEMP
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      CALL CHTRIM (KB, 72, KB, KBL)
      IF ((KB(1:1).EQ.'Q') .OR. (KB(1:1).EQ.'q')) GO TO 990
      CTEMP = KB(:4)
      CALL CHLTOU (4, CTEMP)
      IF ((CTEMP.EQ.' ') .OR. (CTEMP.EQ.'P') .OR. (CTEMP.EQ.'A&P').OR.
     *   (CTEMP.EQ.'P!A') .OR. (CTEMP.EQ.'GCON')) THEN
         CALL OPUT (UVOUT, 'SOLMODE', TYPE, DIM, DUMMY, CTEMP, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       SOLCON
      CALL OGET (UVOUT, 'SOLCON', TYPE, DIM, RTEMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF,1001) 'Gain constraint SOLCON', RTEMP
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      IF (KB.NE.' ') THEN
         CALL CHTRIM (KB, 72, KB, KBL)
         IF ((KB(1:1).EQ.'Q') .OR. (KB(1:1).EQ.'q')) GO TO 990
         KBP = 1
         CALL GETNUM (KB, KBL, KBP, DX)
         IF (DX.EQ.DBLANK) GO TO 990
         IF (DX.GT.1.0D-4) THEN
            RTEMP = DX
            CALL OPUT (UVOUT, 'SOLCON', TYPE, DIM, RTEMP, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
C                                       Error
 990  IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR SEEKING GUIDANCE FROM THE USER'
         CALL MSGWRT (6)
         END IF
      IF (TTY(2).GT.0) CALL ZCLOSE (TTY(1), TTY(2), IERR)
      IERR = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A,' (',I7,')')
 1001 FORMAT (A,' (',F11.5,')')
 1002 FORMAT (A,' (''',A,''')')
 1005 FORMAT ('ERROR',I5,' OPENING TERMINAL FOR INTERACTION')
      END
      SUBROUTINE SCISUB (NSUBA, ISUBA)
C-----------------------------------------------------------------------
C   Get the subarray number from the boss
C   Inputs:
C      NSUBA   I   Max subarray number
C   Output:
C      ISUBA   I   desired subarray number
C-----------------------------------------------------------------------
      INTEGER   NSUBA, ISUBA
C
      INTEGER   TTY(2), KBL, KBP, IERR
      CHARACTER MSGBUF*72, KB*72
      DOUBLE PRECISION DX
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      TTY(1) = 5
      TTY(2) = 0
C                                       Open terminal for conversation
      CALL ZOPEN (TTY(1), TTY(2), 1, MSGBUF, .FALSE., .TRUE., .TRUE.,
     *   IERR)
      IF (IERR.NE.0) THEN
         TTY(2) = 0
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         ISUBA = 1
         GO TO 999
         END IF
      TTY(2) = MAX (1, TTY(2))
 10   WRITE (MSGBUF,1010) NSUBA
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZTTYIO ('READ', TTY(1), TTY(2), 72, KB, IERR)
      ISUBA = 0
      IF (KB.NE.' ') THEN
         CALL CHTRIM (KB, 72, KB, KBL)
         KBP = 1
         CALL GETNUM (KB, KBL, KBP, DX)
         IF (DX.EQ.DBLANK) GO TO 990
         ISUBA = DX + 0.5
         END IF
      IF ((ISUBA.LT.1) .OR. (ISUBA.GT.NSUBA)) GO TO 10
C                                       Error
 990  IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR SEEKING GUIDANCE FROM THE USER'
         CALL MSGWRT (6)
         END IF
      IF (TTY(2).GT.0) CALL ZCLOSE (TTY(1), TTY(2), IERR)
      IERR = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' OPENING TERMINAL FOR INTERACTION')
 1010 FORMAT ('Enter subarray number from 1 through',I3)
      END
      SUBROUTINE CCINF (IFIELD, CCTAB, FREUSE, NEGOK, NCCPOS, SUMPOS,
     *   NCCFMX, IERR)
C-----------------------------------------------------------------------
C   Determines information about a CC table.
C   The number of CC (NCCPOS) prior to the first negative are returned
C   as well as  well as the sum of the fluxes thru that component
C   (SUMPOS).
C     Also the last component number with a flux exceeding FREUSE is
C   returned as NCCFMX.
C   Inputs:
C      IFIELD   I     Field number (for messages)
C      CCTAB    C*?   CC table object
C      FREUSE   R     Flux level to set NCCFMX
C   Output:
C      NCCPOS   I     Number of components before the first negative one.
C      SUMPOS   R     Sum of the CC fluxes thru NCCPOS
C      NCCFMX   I     Number of last component exceeding FREUSE
C      IERR     I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER CCTAB*(*)
      INTEGER   IFIELD, NCCPOS, NCCFMX, IERR
      REAL      FREUSE, SUMPOS
      LOGICAL   NEGOK
C
      INTEGER   TYPE, DIM(7), FLXKOL, NROW, LOOP
      REAL      FLUX, XT
      LOGICAL   GOTNEG, PFLAG
      CHARACTER PREFIX*5, CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
C                                       Open table
      CALL TABOPN (CCTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Find flux column
      CALL TABCOL (CCTAB, 1, 'FLUX   ', FLXKOL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Number of rows
      CALL TABGET (CCTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Initialize
      NCCPOS = NROW
      SUMPOS = 0.0
      NCCFMX = 0
      GOTNEG = .FALSE.
C                                       Loop through table
      DO 100 LOOP = 1,NROW
         CALL TABDGT (CCTAB, LOOP, FLXKOL, TYPE, DIM, FLUX, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         GOTNEG = (GOTNEG .OR. (FLUX.LT.0.0)) .AND. (.NOT.NEGOK)
         IF (.NOT.GOTNEG) THEN
            SUMPOS = SUMPOS + FLUX
            NCCPOS = LOOP
            END IF
         IF (ABS(FLUX).GT.FREUSE) NCCFMX = LOOP
 100     CONTINUE
C                                       Don't include neg. components in
C                                       NCCFMX
      NCCFMX = MIN (NCCFMX, NCCPOS)
C                                       Close table
      CALL TABCLO (CCTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Message
      XT = SUMPOS
      CALL METSCA (XT, PREFIX, PFLAG)
      WRITE (MSGTXT,1100) IFIELD, NCCPOS, XT, PREFIX
      CALL MSGWRT (4)
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR FINDING CC INFO FOR ' // CCTAB
      CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Field',I5,' using',I8,' components with flux=',F8.3,
     *   1X,A,'Jy')
      END
      SUBROUTINE CCSUM (CCTAB, SUMCC, IERR)
C-----------------------------------------------------------------------
C   Determines the sum of the fluxes in a CC table.
C   Inputs:
C      CCTAB   C*?  CC table object
C   Output:
C      SUMCC   R    Sum of the CC fluxes
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER CCTAB*(*)
      REAL      SUMCC
      INTEGER   IERR
C
      INTEGER   TYPE, DIM(7), FLXKOL, NROW, LOOP
      REAL      FLUX
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
C                                       Open table
      CALL TABOPN (CCTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Find flux column
      CALL TABCOL (CCTAB, 1, 'FLUX   ', FLXKOL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Number of rows
      CALL TABGET (CCTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Initialize
      SUMCC = 0.0
C                                       Loop through table
      DO 100 LOOP = 1,NROW
         CALL TABDGT (CCTAB, LOOP, FLXKOL, TYPE, DIM, FLUX, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         SUMCC = SUMCC + FLUX
 100     CONTINUE
C                                       Close table
      CALL TABCLO (CCTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR SUMMING FLUXES FOR ' // CCTAB
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE SCMHI (UVIN, UVOUT, CLEAN)
C-----------------------------------------------------------------------
C   Routine to write history file to output CLEAN image object and UV
C   data object
C   Inputs:
C      UVIN    C*?  Input UV data
C      UVOUT   C*?  Output UV data
C      CLEAN   C*?  CLEAN process object
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*), CLEAN*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLN.INC'
      INTEGER   NADV
      PARAMETER (NADV=46)
      CHARACTER CLEANO(MAXFLD)*32, BEAM(MAXFLD)*32, LIST(NADV)*8,
     *   ERROBJ*32, CDUMMY*1, LINE*72, OBXFIL*48
      INTEGER   IERR, TYPE, DIM(7), MFIELD, I, NBOXES(MAXFLD), DUMMY, J,
     *   NCMP, MSGSAV, IFLD(2,MAXFLD), IMSIZE(2,MAXFLD)
      LONGINT   KP
      REAL      RASH(MAXFLD), DECSH(MAXFLD), FFL, TFL, RESMAX
      LOGICAL   NOCLEN
      INCLUDE 'IMAGWIN.INC'
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'CELLSIZE', 'IMSIZE', 'NFIELD', 'UVTAPER', 'UVRANGE', 'UVWTFN',
     *   'UVSIZE', 'ROBUST', 'UVBOX', 'UVBXFN', 'ROTATE', 'ZEROSP',
     *   'XTYPE', 'YTYPE', 'XPARM', 'YPARM', 'GUARD',
     *   'GAIN', 'FLUX', 'MINPATCH', 'NITER', 'BMAJ', 'BMIN', 'BPA',
     *   'FACTOR',  'MAXPIXEL', 'CMETHOD',
     *   'NMAPS', 'SMODEL', 'REFANT', 'SOLINT', 'APARM', 'SOLTYPE',
     *   'SOLMODE', 'SOLCON', 'ANTWT', 'GAINERR', 'WTUV', 'SUBWT',
     *   'SUBREFA', 'FLAGVER', 'WEIGHTIT'/
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
      CALL OGET (CLEAN, 'OBOXFILE', TYPE, DIM, DUMMY, OBXFIL, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (OBXFIL(:13).EQ.'HOME:OBOXtemp') THEN
         I = 3
         CALL ZTXZAP (I, OBXFIL, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'IMAHIS FAILED TO DELETE TEMPORARY OBOXFILE'
            CALL MSGWRT (7)
            IERR = 0
            END IF
         END IF
C                                       CLEAN names
      ERROBJ = CLEAN
      CALL OGET (CLEAN, 'CLEANI', TYPE, DIM, DUMMY, CLEANO, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL CLNGET (CLEAN, 'NIMAGES', TYPE, DIM, MFIELD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Clean object things
      CALL OGET (CLEAN, 'NBOXES', TYPE, DIM, NBOXES, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (CLEAN, 'WINDOW', TYPE, DIM, WIN(1+WINP), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      MSGSUP = 32000
      CALL OGET (CLEAN, 'TFLUX', TYPE, DIM, TFL, CDUMMY, IERR)
      MSGSUP = MSGSAV
      NOCLEN = IERR.EQ.1
      IF (NOCLEN) IERR = 0
      IF (IERR.NE.0) GO TO 990
C                                       Get image-dep parms
      CALL OGET (CLEANO(1), 'IMSIZE', TYPE, DIM, IMSIZE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET ('Input', 'FLDSIZE', TYPE, DIM, IFLD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (CLEANO(1), 'RASHIFT', TYPE, DIM, RASH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (CLEANO(1), 'DECSHIFT', TYPE, DIM, DECSH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Loop over cleans
      DO 100 I = 1,MFIELD
C                                       Copy old history
         ERROBJ = CLEANO(I)
         CALL OHCOPY (UVIN, CLEANO(I), IERR)
         IF (IERR.NE.0) GO TO 990
C                                       New additions - copy adverb
C                                       values.
         CALL OHLIST ('Input', LIST, NADV, CLEANO(I), IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Special handling
C                                       Field-dependent parms
         WRITE (LINE,1000) I, IMSIZE(1,I), IMSIZE(2,I)
         CALL OHWRIT (LINE, CLEANO(I), IERR)
         IF (IERR.NE.0) GO TO 990
         WRITE (LINE,1001) I, IFLD(1,I), IFLD(2,I)
         CALL OHWRIT (LINE, CLEANO(I), IERR)
         IF (IERR.NE.0) GO TO 990
         WRITE (LINE,1002) I, RASH(I)
         CALL OHWRIT (LINE, CLEANO(I), IERR)
         IF (IERR.NE.0) GO TO 990
         WRITE (LINE,1003) I, DECSH(I)
         CALL OHWRIT (LINE, CLEANO(I), IERR)
         IF (IERR.NE.0) GO TO 990
         IF (NBOXES(I).GT.0) THEN
            WRITE (LINE,1004) I, NBOXES(I)
            CALL OHWRIT (LINE, CLEANO(I), IERR)
            IF (IERR.NE.0) GO TO 990
            DO 20 J = 1,NBOXES(I)
               KP = ((J - 1) * MFIELD + I - 1) * 4 + WINP
               WRITE (LINE,1005) J, I, WIN(1+KP), WIN(2+KP), WIN(3+KP),
     *            WIN(4+KP)
               CALL OHWRIT (LINE, CLEANO(I), IERR)
               IF (IERR.NE.0) GO TO 990
 20            CONTINUE
            END IF
         IF (.NOT.NOCLEN) THEN
            MSGSUP = 32000
            CALL OGET (CLEANO(I), 'BEAM.NITER', TYPE, DIM, NCMP, CDUMMY,
     *         IERR)
            MSGSUP = MSGSAV
            IF (IERR.EQ.1) THEN
               IERR = 0
               NCMP = 0
               END IF
            IF (IERR.NE.0) GO TO 990
            MSGSUP = 32000
            CALL OGET (CLEANO(I), 'CFLUX', TYPE, DIM, FFL, CDUMMY, IERR)
            MSGSUP = MSGSAV
            IF (IERR.EQ.1) THEN
               IERR = 0
               FFL = 0.0
               END IF
            IF (IERR.NE.0) GO TO 990
            MSGSUP = 32000
            CALL OGET (CLEANO(I), 'FIELDRES', TYPE, DIM, RESMAX, CDUMMY,
     *         IERR)
            MSGSUP = MSGSAV
            IF (IERR.EQ.1) THEN
               IERR = 0
               RESMAX = -1.0
               END IF
            IF (IERR.NE.0) GO TO 990
            WRITE (LINE,1007) TFL
            CALL OHWRIT (LINE, CLEANO(I), IERR)
            IF (IERR.NE.0) GO TO 990
            WRITE (LINE,1006) I, NCMP
            CALL OHWRIT (LINE, CLEANO(I), IERR)
            IF (IERR.NE.0) GO TO 990
            WRITE (LINE,1008) I, FFL
            CALL OHWRIT (LINE, CLEANO(I), IERR)
            IF (IERR.NE.0) GO TO 990
            WRITE (LINE,1009) I, RESMAX
            CALL OHWRIT (LINE, CLEANO(I), IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
 100     CONTINUE
      CALL ZMEMRY ('FREE', 'IMAGR', WINW, WIN(1), WINP, IERR)
C                                       Beam
      CALL OGET (CLEAN, 'DIRTBEAM', TYPE, DIM, DUMMY, BEAM, IERR)
      IF (IERR.NE.0) GO TO 990
      MFIELD = DIM(2)
      DO 200 I = 1,MFIELD
         ERROBJ = BEAM(I)
C                                       Old history
         CALL OHCOPY (UVIN, BEAM(I), IERR)
         IF (IERR.NE.0) GO TO 990
C                                        New additions - copy adverb
C                                       values.
         CALL OHLIST ('Input', LIST, NADV, BEAM(I), IERR)
         IF (IERR.NE.0) GO TO 990
 200     CONTINUE
C                                       UVOUT
      ERROBJ = UVOUT
C                                       Old history
      CALL OHCOPY (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       New additions - copy adverb
C                                       values.
      CALL OHLIST ('Input', LIST, NADV, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // ERROBJ
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMSIZE(*,',I2,')  = ',I5,' ,',I6)
 1001 FORMAT ('FLDSIZE(*,',I2,') = ',I5,' ,',I6)
 1002 FORMAT ('RASHIFT(',I2,')   = ',1PE12.5)
 1003 FORMAT ('DECSHIFT(',I2,')  = ',1PE12.5)
 1004 FORMAT ('NBOXES(',I2,')    = ',I2)
 1005 FORMAT ('WIN(*,',I2,',',I2,') =',3(I5,','),I5)
 1006 FORMAT ('NCOMP(',I2,') =',I8)
 1007 FORMAT ('TFLUX = ',1PE12.5,9X,'/ Total cleaned flux')
 1008 FORMAT ('CFLUX(',I2,') = ',1PE12.5,5X,'/ Cleaned flux in field')
 1009 FORMAT ('RESMAX(',I2,') = ',1PE12.5,4X,
     *   '/ Peak residual in field windows')
      END
      SUBROUTINE SCUV2S (UVDATA, UVSCR, CLEANO, ZERO, NSUBA, UVMAXS,
     *   IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C***********************************************************************
C   Local version of UV2SCR that applies subarray weights
C   and tests all rotations of u,v,w
C***********************************************************************
C   Copies A uv data file to a scratch file with application of any
C   calibration, editing, and selection criteria.  The vlbi '*V' data
C   weighting option can be selected.
C   Can only process a single subarray at a time.  If multiple subarrays
C   are desired then make a separate call for each subarray with
C   'CALEDIT.SUBARR'  set on UVDATA and 'UV_DESC.VISOFF' (zero relative
C   visibility offset) on UVSCR set to the current size of the object
C   ('UV_DESC.GCOUNT').
C   Inputs:
C      UVDATA  C*?  Name of uvdata object.
C      UVSCR   C*?  Name of table object.
C      CLEANO  C*?  Name of Clean object
C      ZERO    R(2) Vis and wt for current zero spacing
C      NSUBA   I    Max subarray number - helps control messages
C   Inout:
C      UVMAXS  R(4) Max U, V found so far
C   Inputs from UVDATA
C      UVWTFN  C*2  Data weighting option, if '*V' the weights are
C                   raised to the 0.25 power in the output object.
C                   Defaults to 'NA' (no weighting) if absent.
C      UMAX    R    Maximum acceptable U in wavelengths (default all)
C      VMAX    R    Maximum acceptable V in wavelengths (default all)
C      SUBWT   R(*) Subarray weight factors (default = 1.0)
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), UVSCR*(*), CLEANO*(*)
      INTEGER   NSUBA, IERR
      REAL      UVMAXS(4), ZERO(2)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NVIS, COUNT, TYPE, DIM(7), MSGSAV, NCORR, I, SUBA, K,
     *   INDXU, INDXV, INDXW, NCOR, DUMMY, NAXIS(7), NNSTOK, JLOCS,
     *   TCOUNT, JLOCD, NFIELD
      REAL      UMAX, VMAX, AUMAX, BUMAX, AVMAX, BVMAX, GUARDB(2),
     *   UTFACT(2), UU, VV, CELLS(2), SUBWT(51), CROT, SROT, ROTATE,
     *   CROTAU(7), RASH(MAXFLD), DECSH(MAXFLD)
      DOUBLE PRECISION FREQS(1024), UVFREQ, FRMULT
      LOGICAL   DOVLWT, EXIST, DOWARN, DOROT, IS3D
      CHARACTER UVWTFN*2, SORD*2, CDUMMY*1, CNAME*8, KEYW*8
C      INCLUDE 'UVSTUFF.INC'
C
C***   LOCAL COPY OF STUFF FROM UVSTUFF *********
      INTEGER   MXVS
C                                       MXVS = maximum no. correlations
C                                       in a record.
      PARAMETER (MXVS = 2048)
C                                        Local Info for uv util.
      REAL     RP(50), VS(3,MXVS)
C************************************************
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open input for descriptive info.
      CALL OUVOPN (UVDATA, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       UV WARNING
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OUVGET (UVDATA, 'DOWARNING', TYPE, DIM, DOWARN, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.NE.0) THEN
         IERR = 0
         DOWARN = .TRUE.
         END IF
C                                       VLBI weighting option?
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OUVGET (UVDATA, 'UVWTFN', TYPE, DIM, DUMMY, UVWTFN, IERR)
      MSGSUP = MSGSAV
      IF (IERR.NE.0) THEN
         IERR = 0
         UVWTFN = 'NA'
         END IF
      DOVLWT = UVWTFN(2:2) .EQ. 'V'
      IF (DOVLWT) THEN
         MSGTXT = 'SCUV2S: Applying VLBI weighting option'
         CALL MSGWRT (3)
         END IF
C                                       U,V limits
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OUVGET (UVDATA, 'UMAX', TYPE, DIM, UMAX, CDUMMY, IERR)
      MSGSUP = MSGSAV
C                                       Default = all
      IF (IERR.EQ.1) THEN
         DOWARN = .FALSE.
         UMAX = 1.0E30
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OUVGET (UVDATA, 'VMAX', TYPE, DIM, VMAX, CDUMMY, IERR)
      MSGSUP = MSGSAV
C                                       Default = all
      IF (IERR.EQ.1) THEN
         DOWARN = .FALSE.
         VMAX = 1.0E30
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Actual guardband
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OUVGET (UVDATA, 'GUARDBND', TYPE, DIM, GUARDB, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         GUARDB(1) = 0.0
         GUARDB(2) = 0.0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Default guardband
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OUVGET (UVDATA, 'GUARDEF', TYPE, DIM, UTFACT, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         UTFACT(1) = 0.0
         UTFACT(2) = 0.0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Intended cellsize
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OUVGET (UVDATA, 'CELLSIZE', TYPE, DIM, CELLS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CELLS(1) = 1.0
         CELLS(2) = 1.0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Subarray weights (def. 1.0)
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OGET (UVDATA, 'SUBWT', TYPE, DIM, SUBWT, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CALL RFILL (51, 1.0, SUBWT)
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
      K = DIM(1)
      DO 10 I = 1,K
         IF (SUBWT(I).GT.0.0) GO TO 15
 10      CONTINUE
      CALL RFILL (51, 1.0, SUBWT)
C                                       Additional rotation
 15   MSGSUP = 32000
      CALL OUVGET (UVDATA, 'ROTATE', TYPE, DIM, ROTATE, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         ROTATE = 0.0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
      DOROT = ROTATE.NE.0.0
C                                       3D Images?
      CALL OGET (CLEANO, 'DO3DIMAG', TYPE, DIM, IS3D, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (CLEANO, 'RASHIFT', TYPE, DIM, RASH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (CLEANO, 'DECSHIFT', TYPE, DIM, DECSH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (CLEANO, 'NIMAGES', TYPE, DIM, NFIELD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMC3DI (IS3D, ROTATE, NFIELD, RASH, DECSH, UMATS)
C                                       CROTA
      IF (DOROT) THEN
         CALL UVDGET (UVDATA, 'CROTA', TYPE, DIM, CROTAU, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL UVDFND (UVDATA, 2, 'DEC', JLOCD, IERR)
         IF (IERR.NE.0) GO TO 990
         CROTAU(JLOCD) = CROTAU(JLOCD) + ROTATE
         ROTATE = -ROTATE / 57.29578
         CROT = COS (ROTATE)
         SROT = SIN (ROTATE)
         END IF
C                                       Find number of correlations
      CALL OUVGET (UVDATA, 'UV_DESC.NCORR', TYPE, DIM, NCORR, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       UV axis descriptor
C                                       NAXIS
      CALL UVDGET (UVDATA, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDFND (UVDATA, 2, 'STOKES', JLOCS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Number of polarizations
      NNSTOK = NAXIS(JLOCS)
C                                       Freq array
      CALL UVFRQS (UVDATA, UVFREQ, FREQS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Average frequency
      K = 0
      FRMULT = 0.0D0
      DO 30 I = 1,NCORR,NNSTOK
         K = K + 1
         FRMULT = MAX (FRMULT, FREQS(K))
 30      CONTINUE
      FRMULT = FRMULT / UVFREQ
C                                       Uv data pointers
      CALL UVDFND (UVDATA, 1, 'UU-L', INDXU, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER UU-L'
         GO TO 995
         END IF
      CALL UVDFND (UVDATA, 1, 'VV-L', INDXV, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER VV-L'
         GO TO 995
         END IF
      CALL UVDFND (UVDATA, 1, 'WW-L', INDXW, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER WW-L'
         GO TO 995
         END IF
C                                       Create output if necessary
      CALL OBFEXS (UVSCR, EXIST, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (.NOT.EXIST) THEN
         NVIS = 1000
         CALL OUVSCR (UVSCR, UVDATA, NVIS, .FALSE., IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Copy data
      CALL OUVOPN (UVSCR, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy UVDESC
      CALL UVDSCP (UVDATA, UVSCR, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Subarray number
      CALL SECGET (UVDATA, 'SUBARR', TYPE, DIM, SUBA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (SUBA.LE.0) SUBA = 1
C                                       NCORR
      CALL UVDGET (UVDATA, 'NCORR', TYPE, DIM, NCOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Declare 'MAXBLINE' a header
C                                       keyword for the image class.
      CNAME = 'UVDATA'
      KEYW = 'MAXBLINE'
      CALL OBVHKW (CNAME, KEYW, OOARE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get current max u
      MSGSUP = 32000
      CALL OUVGET (UVSCR, 'MAXBLINE', TYPE, DIM, BUMAX, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         BUMAX = 0.0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Initialize visibility count
      COUNT = 0
      TCOUNT = 0
      AUMAX = UVMAXS(1)
      AVMAX = UVMAXS(2)
      BVMAX = UVMAXS(4)
      UMAX = UMAX / FRMULT
      VMAX = VMAX / FRMULT
      IF (SUBA.GT.1) THEN
         CALL UVDGET (UVSCR, 'GCOUNT', TYPE, DIM, COUNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Loop thru data
 100     CALL UVREAD (UVDATA, RP, VS, IERR)
         IF (IERR.LT.0) GO TO 200
         IF (IERR.GT.0) GO TO 990
C                                       Want this one?
         IF (DOROT) THEN
            UU = RP(INDXU)
            VV = RP(INDXV)
            RP(INDXU) = CROT * UU - SROT * VV
            RP(INDXV) = CROT * VV + SROT * UU
            END IF
         CALL IMC3DM (RP(INDXU), NFIELD, UMATS, UU, VV)
         AUMAX = MAX (UU, AUMAX)
         AVMAX = MAX (VV, AVMAX)
         TCOUNT = TCOUNT + 1
         IF ((UU.LE.UMAX) .AND. (VV.LE.VMAX)) THEN
            BUMAX = MAX (UU, BUMAX)
            BVMAX = MAX (VV, BVMAX)
            COUNT = COUNT + 1
C                                       Change weighting also apply
C                                       any subarray weights?
            IF (DOVLWT) THEN
               DO 150 I = 1,NCORR
                  IF (VS(3,I).GT.0.0) VS(3,I) =
     *               (SUBWT(SUBA) * VS(3,I)) ** 0.25
 150              CONTINUE
            ELSE IF (ABS(SUBWT(SUBA)-1.0).GT.1.0E-4) THEN
               DO 160 I = 1,NCORR
                  IF (VS(3,I).GT.0.0) VS(3,I) = SUBWT(SUBA) * VS(3,I)
 160              CONTINUE
               END IF
            CALL UVWRIT (UVSCR, RP, VS, IERR)
            IF (IERR.GT.0) GO TO 990
            END IF
         GO TO 100
 200     IERR = 0
      IF (ZERO(2).GT.0.0) THEN
         RP(INDXU) = 0.
         RP(INDXV) = 0.
         RP(INDXW) = 0.
         DO 210 I = 1,NCOR
            VS(1,I) = ZERO(1)
            VS(2,I) = 0.
            VS(3,I) = ZERO(2)
 210        CONTINUE
         TCOUNT = TCOUNT + 1
         COUNT = COUNT + 1
         CALL UVWRIT (UVSCR, RP, VS, IERR)
         IF (IERR.GT.0) GO TO 990
         IF (DOWARN) THEN
            WRITE (MSGTXT,1210) ZERO(1), ZERO(2)
            CALL MSGWRT (4)
            END IF
         END IF
C                                       Set amount of output data
      DIM(1) = 1
      DIM(2) = 1
      CALL OUVPUT (UVSCR, 'MAXBLINE', OOARE, DIM, BUMAX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      UVMAXS(1) = AUMAX
      UVMAXS(2) = AVMAX
      UVMAXS(3) = BUMAX
      UVMAXS(4) = BVMAX
C                                       rotation
      IF (DOROT) THEN
         ROTATE = 0.0
         CALL OUVPUT (UVSCR, 'ROTATE', OOARE, DIM, ROTATE, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         DIM(1) = 7
         CALL UVDPUT (UVSCR, 'CROTA', OOARE, DIM, CROTAU, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Warnings ?
      IF ((DOWARN) .AND. (SUBA.GE.NSUBA)) THEN
         IF (TCOUNT.GT.COUNT) THEN
            WRITE (MSGTXT,1220) COUNT, TCOUNT
            CALL MSGWRT (6)
         ELSE
            WRITE (MSGTXT,1221) COUNT
            CALL MSGWRT (3)
            END IF
C                                       data outside grid?
         UMAX = UMAX * FRMULT
         VMAX = VMAX * FRMULT
         AUMAX = AUMAX * FRMULT
         AVMAX = AVMAX * FRMULT
         BUMAX = BUMAX * FRMULT
         BVMAX = BVMAX * FRMULT
         IF ((AUMAX.GT.UMAX) .OR. (AVMAX.GT.VMAX)) THEN
            MSGTXT = '**** WARNING data discarded outside usable part'
     *         // ' of UV plane ****'
            CALL MSGWRT (6)
            IF (AUMAX.GT.UMAX) THEN
               UU = UMAX/AUMAX * ABS(CELLS(1))
               WRITE (MSGTXT,1230) 'U', AUMAX, UMAX
               CALL MSGWRT (6)
               WRITE (MSGTXT,1231) UU, ABS(CELLS(1))
               CALL MSGWRT (6)
               WRITE (MSGTXT,1232) 'U', GUARDB(1)
               CALL MSGWRT (6)
               END IF
            IF (AVMAX.GT.VMAX) THEN
               VV = VMAX/AVMAX * ABS(CELLS(2))
               WRITE (MSGTXT,1230) 'V', AVMAX, VMAX
               CALL MSGWRT (6)
               WRITE (MSGTXT,1231) VV, CELLS(2)
               CALL MSGWRT (6)
               WRITE (MSGTXT,1232) 'V', GUARDB(2)
               CALL MSGWRT (6)
               END IF
            END IF
         IF ((BUMAX.GT.UMAX*(1.-1.2*UTFACT(1))/(1.0-GUARDB(1))) .OR.
     *      (BVMAX.GT.VMAX*(1.-1.2*UTFACT(2))/(1.0-GUARDB(2)))) THEN
            MSGTXT = '**** WARNING data included out of inner portion'
     *         // ' of UV plane ****'
            CALL MSGWRT (6)
            MSGTXT = '**** Watch for high-frequency & other poor ' //
     *         'cleaning effects ****'
            CALL MSGWRT (6)
            END IF
C                                       set DOWARN away
         DOWARN = .FALSE.
         DIM(1) = 1
         DIM(2) = 1
         CALL OUVPUT (UVDATA, 'DOWARNING', OOALOG, DIM, DOWARN, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Better be some data
      IF (COUNT.LE.0) THEN
         IERR = 7
         MSGTXT = 'SCUV2S: NO DATA SELECTED'
         GO TO 995
         END IF
C                                       Set amount of output data
      DIM(1) = 1
      DIM(2) = 1
      CALL UVDPUT (UVSCR, 'GCOUNT', OOAINT, DIM, COUNT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy relevant tables (once)
      IF (SUBA.EQ.1) THEN
         CALL UVDTCO (UVDATA, UVSCR, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Sort order the same as input
      CALL UVDGET (UVDATA, 'SORTORD', TYPE, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (SUBA.GT.1) SORD = '  '
      IF ((DOROT) .AND. (SORD.NE.'TB') .AND. (SORD.NE.'BT')) SORD = '??'
      CALL UVDPUT (UVSCR, 'SORTORD', OOACAR, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close files, update disk
      CALL OUVCLO (UVDATA, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVCLO (UVSCR, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 995  CALL MSGWRT (7)
 990  MSGTXT = 'SCUV2S: ERROR COPYING ' // UVDATA
      CALL MSGWRT (7)
      MSGTXT = 'TO ' // UVSCR
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1210 FORMAT ('SCUV2S: append zero spacing',F10.4,' weight',F7.1)
 1220 FORMAT ('SCUV2S: Only',I9,' samples of',I9,' fell on UV grid')
 1221 FORMAT ('SCUV2S: Copied',I9,' visibilities to be imaged')
 1230 FORMAT ('**** Actual ',A,'max',1PE11.4,' exceeds limit',1PE11.4,
     *   8X,'****')
 1231 FORMAT ('**** Use cellsize <',F10.6,' not',F10.6,
     *   ' to get all data ****')
 1232 FORMAT ('**** using a ',A,' guard band of',F7.3,' of a radius',
     *   13X,'****')
      END
      SUBROUTINE IMC3DI (DO3D, ROTATE, NFIELD, RASH, DECSH, UMATS)
C-----------------------------------------------------------------------
C   Routine to prepare re-projection matrices for uvw's
C   Input:
C      DO3D     L      Use 3D or in-plane math
C      ROTATE   R      Rotation in degrees
C      NFIELD   I      Number of fields
C      RASH     R(*)   RA shift parameters
C      DECSH    R(*)   Dec shift parameters
C   Input from DUVH.INC: RA and DEC
C   Output:
C      UMATS    R(3,3,*)  uvw re-projection matrices
C-----------------------------------------------------------------------
      LOGICAL   DO3D
      INTEGER   NFIELD
      REAL      ROTATE, RASH(*), DECSH(*), UMATS(3,3,*)
C
      INTEGER   LFIELD
      REAL      PMAT(3,3)
      DOUBLE PRECISION XRA, XDEC
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      DO 20 LFIELD = 1,NFIELD
         CALL XYSHFT (RA, DEC, RASH(LFIELD), DECSH(LFIELD), ROTATE,
     *      XRA, XDEC)
         IF (DO3D)  THEN
            CALL PRJMAT (RA, DEC, ROTATE, XRA, XDEC, ROTATE,
     *         UMATS(1,1,LFIELD), PMAT)
C                                       EXPERIMENTAL ????
         ELSE
            CALL P2DMAT (RA, DEC, ROTATE, XRA, XDEC, ROTATE,
     *         UMATS(1,1,LFIELD), PMAT)
            END IF
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE IMC3DM (UVW, NFIELD, UMATS, UU, VV)
C-----------------------------------------------------------------------
C   Routine to find max abs U and V over all re-projections
C   Input:
C      UVW      R(3)      u,v,w allready rotated if needed
C      NFIELD   I         Number of fields
C      UMATS    R(3,3,*)  uvw re-projection matrices
C   Output
C      UU       R         Max (abs(u))
C      VV       R         Max (abs(v))
C-----------------------------------------------------------------------
      INTEGER   NFIELD
      REAL      UVW(3), UMATS(3,3,*), UU, VV
C
      INTEGER   LFIELD
      REAL      PRJU(3)
C-----------------------------------------------------------------------
      UU = ABS (UVW(1))
      VV = ABS (UVW(2))
      DO 20 LFIELD = 1,NFIELD
         CALL PRJMUL (1, UVW, UMATS(1,1,LFIELD), PRJU)
         UU = MAX (UU, ABS(PRJU(1)))
         VV = MAX (VV, ABS(PRJU(2)))
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SCMATV (UVDATA, UVSCR, CLEAN, CLEANO, MFIELD, EDITOK,
     *   DONE, UVOUT, MXLOOP, FLIPED, IERR)
C-----------------------------------------------------------------------
C   Private
C   Does a TV display and interaction
C   Inputs:
C      UVDATA   C*?      The name of the current UV data object
C      UVSCR    C*?      The name of the current UV residual object
C      CLEAN    C*?      The name of the CLEAN object.
C      CLEANO   C(*)*?   Names of residual/CLEAN images
C      MFIELD   I        Number of fields
C      UVOUT    C*?      The name of the output UV object
C   In/Out:
C      EDITOK   L        Okay to do editing still?
C      DONE     L        In: if T no interaction, no display of Clean
C                            display is on
C                        Out: T => user asks to end Self cal
C      MXLOOP   I        Max self-cal loops
C      FLIPED   L        True => already A&P on in, T on out => want A&P
C   Output:
C      IERR     I        0 ok, 1 TV error, 2 die now
C   Output to Clean object:
C      NBOXES   I(*)      Number of boxes given for field
C      WIN      I(4,*,*)  Boxes for fields
C                           WIN(1,*)=-1 indicates a round box of
C                           width WIN(2,*) pixels centered on
C                           pixel (WIN(3,*), WIN(4,*))
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), UVSCR*(*), CLEAN*(*), CLEANO(*)*(*),
     *   UVOUT*(*)
      INTEGER   MFIELD, MXLOOP, IERR
      LOGICAL   EDITOK, DONE, FLIPED
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLN.INC'
      CHARACTER CDUMMY*1, STATUS*4, TVNAME*32, CHOICS(27+MAXAFL)*16,
     *   ISHELP*6, EDNAME*32, CHOIC1(16)*16, CHOIC2(3)*16, ANTAB*32,
     *   OBXFIL*48, MSGBUF*72, TITLE*80
      INTEGER   TVFLD, MSGSAV, TYPE, DIM(7), GRCS(8,5), TVCS(16), I, J,
     *   NCOL, NROWS(3), TIMLIM, CHS, TVBUTT, JERR, TBLC(7), DUMMY,
     *   MTYPE, NGRY, NGRPH, MAXX(2), TVWND(4), CSIZE(2), NX, NY, INX,
     *   NBOXES(MAXFLD), IMSIZE(2,MAXFLD), IBR, ITVFLD, LERR, NSUBA,
     *   ISUBA, ANVER, TTY(2), KBP, NTITLE, TOPSEP, SIDSEP, TVGRCH(3),
     *   JTRIM
      REAL      AVGT, GAPT, SOLT, RASH(MAXFLD), DECSH(MAXFLD)
      DOUBLE PRECISION X
      LOGICAL   DOIT, LEAVE(27+MAXAFL), DOFLAG, REBOXD, SCONLY, INTROK
      INCLUDE 'IMAGWIN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
      SAVE TIMLIM
      DATA CHOIC1 /'ABORT TASK', 'TURN OFF TV', 'STOP SELFCAL',
     *   'OFFZOOM', 'OFFTRANS', 'OFFCOLOR', 'TVFIDDLE', 'TVTRAN',
     *   'TVPSEUDO', 'TVFLAME', 'TVZOOM', 'CURVALUE', 'SET WINDOW',
     *   'RESET WINDOW', 'SET PARAMETERS', 'SWITCH TO A&P'/
      DATA CHOIC2 /'TVBOX', 'REBOX', 'CONTINUE SELFCAL'/
      DATA LEAVE /6*.TRUE., 8*.FALSE., 2*.TRUE., 11*.FALSE.,
     *   MAXAFL*.TRUE./
      DATA DOFLAG /.TRUE./
      DATA TIMLIM /120/
C-----------------------------------------------------------------------
C                                       does the TV need to run?
      IERR = 0
      TITLE = ' '
      NTITLE = 0
      REBOXD = .FALSE.
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL CLNGET (CLEAN, 'SCTVFLD', TYPE, DIM, TVFLD, CDUMMY, LERR)
      MSGSUP = MSGSAV
      IF (LERR.NE.0) TVFLD = 0
      SCONLY = TVFLD.LT.0
      TVFLD = ABS (TVFLD)
      IF (TVFLD.LE.0) GO TO 999
      MSGSUP = 32000
      CALL CLNGET (CLEAN, 'INTACTOK', TYPE, DIM, INTROK, CDUMMY, LERR)
      MSGSUP = MSGSAV
      IF (LERR.NE.0) INTROK = .FALSE.
      MSGSUP = 32000
      CALL CLNGET (CLEAN, 'TVGRCHAN', TYPE, DIM, TVGRCH, CDUMMY, LERR)
      MSGSUP = MSGSAV
      IF (LERR.NE.0) CALL FILL (3, 8, TVGRCH)
      IF (TVGRCH(1).LT.4) TVGRCH(1) = 0
      IF (TVGRCH(1).GT.7) TVGRCH(1) = 5
      IF (TVGRCH(2).LT.4) TVGRCH(2) = 0
      IF (TVGRCH(2).GT.7) TVGRCH(2) = 7
      IF (TVGRCH(3).LT.4) TVGRCH(3) = 0
      IF (TVGRCH(3).GT.7) TVGRCH(3) = 6
C                                       get Clean TV field
      MSGSUP = 32000
      CALL CLNGET (CLEAN, 'TVFIELD', TYPE, DIM, ITVFLD, CDUMMY, LERR)
      MSGSUP = MSGSAV
      IF (LERR.NE.0) ITVFLD = 0
      IF (ITVFLD.GT.0) TVFLD = ITVFLD
      IF ((DONE) .AND. (ITVFLD.GT.0)) GO TO 999
C                                       open TTY
      TTY(1) = 5
      TTY(2) = 0
      CALL ZOPEN (TTY(1), TTY(2), 1, MSGBUF, .FALSE., .TRUE., .TRUE.,
     *   LERR)
      IF (LERR.NE.0) THEN
         TTY(2) = 0
         WRITE (MSGTXT,1035) LERR
         CALL MSGWRT (8)
         GO TO 980
         END IF
      TTY(2) = MAX (1, TTY(2))
C                                       Number of subarrays
      MSGSAV = MSGSUP
      ANTAB = 'Temp AN for SCLOOP'
      ANVER = 1
      CALL UV2TAB (UVDATA, ANTAB, 'AN', ANVER, LERR)
      IF (LERR.NE.0) GO TO 990
      CALL TBLHIV (ANTAB, NSUBA, LERR)
      IF (LERR.NE.0) GO TO 990
      NSUBA = MAX (1, NSUBA)
C                                       Destroy temp object
      CALL TABDES (ANTAB, LERR)
      IF (LERR.NE.0) GO TO 990
C                                       open TV device
      TVNAME = 'Self-cal TV display'
      CALL TVDCRE (TVNAME, LERR)
      IF (LERR.NE.0) GO TO 990
      CALL TVDOPN (TVNAME, STATUS, LERR)
      IF (LERR.NE.0) GO TO 985
C                                       get parameters
      CALL CLNGET (CLEAN, 'NBOXES', TYPE, DIM, NBOXES, CDUMMY, LERR)
      IF (LERR.NE.0) GO TO 980
      CALL CLNGET (CLEAN, 'WINDOW', TYPE, DIM, WIN(1+WINP), CDUMMY,
     *   LERR)
      IF (LERR.NE.0) GO TO 980
      DO 5 I = 1,MFIELD
         CALL ARDGET (CLEANO(I), 'NAXIS', TYPE, DIM, IMSIZE(1,I),
     *      CDUMMY, LERR)
         IF (LERR.NE.0) GO TO 980
 5       CONTINUE
      ISHELP = TSKNAM
C                                       choices
      IF (TVFLD.GT.MFIELD) TVFLD = 1
      NCOL = 2
      NROWS(1) = 15
      IF (.NOT.FLIPED) NROWS(1) = 16
      DO 6 I = 1,16
         CHOICS(I) = CHOIC1(I)
 6       CONTINUE
      NROWS(2) = 0
      IF ((NSUBA.LE.1) .OR. (NSUBA.GT.8)) THEN
         NROWS(2) = NROWS(2) + 1
         CHOICS(NROWS(1)+1) = 'EDIT DATA'
         LEAVE(NROWS(1)+1) = .FALSE.
      ELSE
         DO 7 I = 1,NSUBA
            WRITE (CHOICS(I+NROWS(2)),1006) I
            LEAVE(NROWS(1)+I) = .FALSE.
 7          CONTINUE
         NROWS(2) = NROWS(2) + NSUBA
         END IF
      DO 8 I = 1,3
         CHOICS(I+NROWS(2)+NROWS(1)) = CHOIC2(I)
         LEAVE(I+NROWS(1)+NROWS(2)) = .FALSE.
 8       CONTINUE
      NROWS(2) = NROWS(2) + 3
      NROWS(3) = 0
      J = NROWS(1) + NROWS(2)
      LEAVE(J) = .TRUE.
      IF (MFIELD.GT.64) THEN
         J = J + 1
         CHOICS(J) = 'SELECT NEW FIELD'
         LEAVE(J) = .TRUE.
         NROWS(2) = NROWS(2) + 1
      ELSE IF (MFIELD.GT.1) THEN
         IF (MFIELD.GT.16) THEN
            NROWS(1) = J
            NROWS(2) = 0
            END IF
         DO 10 I = 1,MFIELD
            J = J + 1
            WRITE (CHOICS(J),1000) I
            CALL REFRMT (CHOICS(J), ' ', INX)
            LEAVE(J) = .TRUE.
 10         CONTINUE
         IF (MFIELD.GT.32) THEN
            NCOL = 3
            NROWS(3) = MFIELD / 2
            NROWS(2) = MFIELD - NROWS(3)
            IF (NROWS(2)-NROWS(1).GT.5) THEN
               J = (NROWS(2) - NROWS(1) - 3) / 3
               NROWS(1) = NROWS(1) + 2 * J
               NROWS(2) = NROWS(2) - J
               NROWS(3) = NROWS(3) - J
               END IF
         ELSE
            NROWS(2) = NROWS(2) + MFIELD
            IF (NROWS(2)-NROWS(1).GT.5) THEN
               J = (NROWS(2) - NROWS(1) - 4) / 2
               NROWS(1) = NROWS(1) + J
               NROWS(2) = NROWS(2) - J
               END IF
            END IF
         END IF
C                                       learn about TV
      CALL OTVPRM (TVNAME, NGRY, NGRPH, MAXX, TVWND, CSIZE, LERR)
      IF (LERR.NE.0) GO TO 980
C                                       window too small?
      DOIT = .FALSE.
      NX = IMSIZE(1,TVFLD)
      NY = IMSIZE(2,TVFLD)
      I = MAXX(1) / NX
      J = MAXX(2) / NY
      IF ((I.GT.0) .AND. (J.GT.0)) THEN
         I = MIN (I, 3)
         J = MIN (J, 3)
         IF (I*NX.GT.520) I = MAX (1, I-1)
         IF (J*NY.GT.520) J = MAX (1, J-1)
         I = MAX (1, MIN (I, J))
         NX = I * NX
         NY = I * NY
      ELSE
         I = NX / MAXX(1) + 1
         J = NY / MAXX(2) + 1
         I = MAX (I, J)
         NX = NX / I
         NY = NY / I
         END IF
      IF ((TVWND(3)-TVWND(1)+1.LT.NX+61) .AND. (NX.LT.MAXX(1))) THEN
         DOIT = .TRUE.
         TVWND(1) = MAXX(1) / 2 - NX/2
         TVWND(1) = MAX (1, TVWND(1))
         IBR = MIN (TVWND(1), 61) - 1
         TVWND(3) = TVWND(1) + NX - 1 + IBR
         TVWND(1) = TVWND(1) - IBR
         TVWND(1) = MAX (1, TVWND(1))
         TVWND(3) = MIN (TVWND(3), MAXX(1))
         END IF
      IF ((TVWND(4)-TVWND(2)+1.LT.NY+61) .AND. (NY.LT.MAXX(2))) THEN
         DOIT = .TRUE.
         TVWND(2) = MAXX(2) / 2 - NY/2
         TVWND(2) = MAX (1, TVWND(2))
         IBR = MIN (TVWND(2), 61) - 1
         TVWND(4) = TVWND(2) + NY + IBR
         TVWND(2) = TVWND(2) - IBR
         TVWND(2) = MAX (1, TVWND(2))
         TVWND(4) = MIN (TVWND(4), MAXX(2))
         END IF
      IF (DOIT) THEN
         CALL TVDTVW (TVNAME, 'WRITE', TVWND, LERR)
         IF (LERR.NE.0) GO TO 980
         END IF
C                                       off graphics
      DO 11 I = 1,8
         GRCS(I,1) = I
 11      CONTINUE
      DIM(1) = 8
      DIM(2) = 1
      CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,1), CDUMMY,
     *   LERR)
      IF (LERR.NE.0) GO TO 980
      CALL OTVOFG (TVNAME, LERR)
      IF (LERR.NE.0) GO TO 980
C                                       desired ones
      CALL FILL (40, 0, GRCS)
      GRCS(1,1) = 1
      GRCS(2,1) = 2
      GRCS(1,2) = MIN (3, NGRPH-1)
      GRCS(1,3) = MIN (4, NGRPH-1)
      CALL COPY (3, TVGRCH, GRCS(1,5))
C                                       tell TV what chans, parent
      DIM(1) = 8
      DIM(2) = 1
      CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,1), CDUMMY,
     *   LERR)
      IF (LERR.NE.0) GO TO 980
C                                       off other channels, zoom, scroll
      CALL FILL (7, 0, TBLC)
      DO 12 I = 1,15
         TVCS(I) = I+1
 12      CONTINUE
      TVCS(16) = 0
      DIM(1) = 16
      CALL TVDPUT (TVNAME, 'TVCHANS', OOAINT, DIM, TVCS, CDUMMY, LERR)
      IF (LERR.NE.0) GO TO 980
      CALL OTVOFF (TVNAME, LERR)
      IF (LERR.NE.0) GO TO 980
      CALL OTVOFZ (TVNAME, LERR)
      IF (LERR.NE.0) GO TO 980
      CALL OTVOFS (TVNAME, .TRUE., LERR)
      IF (LERR.NE.0) GO TO 980
C                                       specify our channel
      CALL FILL (16, 0, TVCS)
      TVCS(1) = 1
      CALL TVDPUT (TVNAME, 'TVCHANS', OOAINT, DIM, TVCS, CDUMMY, LERR)
      IF (LERR.NE.0) GO TO 980
      DIM(1) = LEN (CLEAN)
      CALL TVDPUT (TVNAME, 'TVPARENT', OOACAR, DIM, DUMMY, CLEAN, LERR)
      IF (LERR.NE.0) GO TO 980
C                                       clear the TV channel
      CALL OTVCLC (TVNAME, LERR)
      IF (LERR.NE.0) GO TO 980
      CALL OTVCLG (TVNAME, LERR)
      IF (LERR.NE.0) GO TO 980
      CALL OTVON (TVNAME, LERR)
      IF (LERR.NE.0) GO TO 980
      IF ((SCONLY) .AND. (TIMLIM.GT.30)) THEN
         CALL OTVOFT (TVNAME, LERR)
         IF (LERR.NE.0) GO TO 980
         CALL OTVOFC (TVNAME, LERR)
         IF (LERR.NE.0) GO TO 980
         END IF
C                                       select and show TVFLD
      DIM(1) = LEN (CLEANO(1))
      DIM(2) = 1
      CALL TVDPUT (TVNAME, 'TVOBJECT', OOACAR, DIM, DUMMY,
     *   CLEANO(TVFLD), LERR)
      IF (LERR.NE.0) GO TO 980
      DIM(1) = 1
      CALL TVDPUT (TVNAME, 'WINLOAD', OOAINT, DIM, GRCS(1,2), CDUMMY,
     *   LERR)
      IF (LERR.NE.0) GO TO 980
      CALL TVDPUT (TVNAME, 'XWINLOAD', OOAINT, DIM, TVGRCH(3), CDUMMY,
     *   LERR)
      IF (LERR.NE.0) GO TO 980
      DIM(1) = 1
      DIM(2) = 1
      CALL CLNPUT (CLEAN, 'TVFIELD', OOAINT, DIM, TVFLD, CDUMMY, LERR)
      IF (LERR.NE.0) GO TO 980
      CALL SCHECK (CLEANO, MFIELD, MAXX, IMSIZE, NBOXES, WIN(1+WINP),
     *   TVFLD, LERR)
      IF (LERR.NE.0) GO TO 980
      DIM(1) = 8
      DIM(2) = 1
      CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,5), CDUMMY,
     *   LERR)
      IF (LERR.NE.0) GO TO 980
      CALL OTVLOD (TVNAME, LERR)
      IF (LERR.NE.0) GO TO 980
      IF (DONE) GO TO 980
C                                       stuff for title
      IF (MFIELD.GT.1) THEN
         CALL OGET (CLEANO(1), 'RASHIFT', TYPE, DIM, RASH, CDUMMY,
     *      LERR)
         IF (LERR.NE.0) GO TO 990
         CALL OGET (CLEANO(1), 'DECSHIFT', TYPE, DIM, DECSH, CDUMMY,
     *      LERR)
         IF (LERR.NE.0) GO TO 990
         END IF
C                                       loop to menu
 100  MTYPE = -1
C                                       title for >1 field
      IF (MFIELD.GT.1) THEN
         WRITE (TITLE,1100) TVFLD, RASH(TVFLD), DECSH(TVFLD)
         IF (TITLE(22:22).EQ.' ') TITLE(22:22) = '0'
         IF (TITLE(23:27).EQ.'.0000') TITLE(23:27) = ' '
         IF (TITLE(25:27).EQ.'000') TITLE(25:27) = ' '
         IF (TITLE(26:27).EQ.'00') TITLE(26:27) = ' '
         IF (TITLE(27:27).EQ.'0') TITLE(27:27) = ' '
         IF (TITLE(34:34).EQ.' ') TITLE(34:34) = '0'
         IF (TITLE(35:39).EQ.'.0000') TITLE(35:39) = ' '
         IF (TITLE(37:39).EQ.'000') TITLE(37:39) = ' '
         IF (TITLE(38:39).EQ.'00') TITLE(38:39) = ' '
         IF (TITLE(39:39).EQ.'0') TITLE(39:39) = ' '
         CALL REFRMT (TITLE, ' ', LERR)
         NTITLE = 1
         END IF
      TOPSEP = 5
      SIDSEP = 5
      CALL TVDMEN (TVNAME, MTYPE, NCOL, NROWS, GRCS, TOPSEP, SIDSEP,
     *   ISHELP, CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHS, TVBUTT,
     *   LERR)
      IF (LERR.EQ.0) THEN
         TIMLIM = 30
         IF (TVBUTT.LE.0) THEN
            MSGTXT = 'Menu read timed out'
            CALL MSGWRT (2)
            GO TO 980
            END IF
C                                       Call back: fiddle LUTs
C                                       OFFZOOM
         IF (CHOICS(CHS).EQ.'OFFZOOM') THEN
            CALL OTVOFZ (TVNAME, LERR)
C                                       OFFTRANS
         ELSE IF (CHOICS(CHS).EQ.'OFFTRANS') THEN
            CALL OTVOFT (TVNAME, LERR)
C                                       OFFCOLOR
         ELSE IF (CHOICS(CHS).EQ.'OFFCOLOR') THEN
            CALL OTVOFC (TVNAME, LERR)
C                                       TVFIDDLE
         ELSE IF (CHOICS(CHS).EQ.'TVFIDDLE') THEN
            CALL OTVFID (TVNAME, LERR)
C                                       TVTRAN
         ELSE IF (CHOICS(CHS).EQ.'TVTRAN') THEN
            CALL OTVTRA (TVNAME, LERR)
C                                       TVPSEUDO
         ELSE IF (CHOICS(CHS).EQ.'TVPSEUDO') THEN
            CALL OTVPSU (TVNAME, LERR)
C                                       TVPHLAME
         ELSE IF (CHOICS(CHS).EQ.'TVFLAME') THEN
            CALL OTVFLA (TVNAME, LERR)
C                                       TVZOOM
         ELSE IF (CHOICS(CHS).EQ.'TVZOOM') THEN
            CALL OTVZOM (TVNAME, LERR)
C                                       CURVALUE
         ELSE IF (CHOICS(CHS).EQ.'CURVALUE') THEN
            DIM(1) = 8
            DIM(2) = 1
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,3),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL OTVALU (TVNAME, LERR)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,1),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
C                                       set window
         ELSE IF (CHOICS(CHS).EQ.'SET WINDOW') THEN
            DIM(1) = 8
            DIM(2) = 1
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,2),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL OTVWIN (TVNAME, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL OTVOFG (TVNAME, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,1),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL OTVCLC (TVNAME, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL SCHECK (CLEANO, MFIELD, MAXX, IMSIZE, NBOXES,
     *         WIN(1+WINP), TVFLD, LERR)
            IF (LERR.NE.0) GO TO 980
            DIM(1) = 8
            DIM(2) = 1
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,5),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL OTVLOD (TVNAME, LERR)
C                                       reset window
         ELSE IF (CHOICS(CHS).EQ.'RESET WINDOW') THEN
            DIM(1) = 7
            DIM(2) = 1
            CALL OPUT (CLEANO(TVFLD), 'TBLC', OOAINT, DIM, TBLC, CDUMMY,
     *         LERR)
            IF (LERR.NE.0) GO TO 980
            CALL OPUT (CLEANO(TVFLD), 'TTRC', OOAINT, DIM, TBLC, CDUMMY,
     *         LERR)
            IF (LERR.NE.0) GO TO 980
            CALL SCHECK (CLEANO, MFIELD, MAXX, IMSIZE, NBOXES,
     *         WIN(1+WINP), TVFLD, LERR)
            IF (LERR.NE.0) GO TO 980
            DIM(1) = 8
            DIM(2) = 1
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,5),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL OTVLOD (TVNAME, LERR)
C                                       TVBOX
         ELSE IF (CHOICS(CHS).EQ.'TVBOX') THEN
            CDUMMY = 'N'
            IF ((NBOXES(TVFLD).GT.0) .AND. (INTROK)) THEN
               WRITE (MSGBUF,1105) NBOXES(TVFLD)
               J = JTRIM (MSGBUF)
               CALL INQSTR (TTY, MSGBUF, J, MSGTXT, IERR)
               IF (IERR.EQ.0) THEN
                  I = JTRIM (MSGTXT)
                  CALL CHTRIM (MSGTXT, I, MSGTXT, J)
                  IF ((MSGTXT(:1).EQ.'y') .OR. (MSGTXT(:1).EQ.'Y'))
     *               CDUMMY = 'Y'
                  END IF
               END IF
            IF (CDUMMY.EQ.'Y') NBOXES(TVFLD) = 0
            DIM(1) = MFIELD
            DIM(2) = 1
            CALL OPUT (CLEAN, 'NBOXES', OOAINT, DIM, NBOXES, CDUMMY,
     *         LERR)
            IF (LERR.NE.0) GO TO 980
            DIM(1) = 8
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,2),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
C                                       ignore error
            CALL OTVBOX (TVNAME, LERR)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,1),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL CLNGET (CLEAN, 'NBOXES', TYPE, DIM, NBOXES, CDUMMY,
     *         LERR)
            IF (LERR.NE.0) GO TO 980
            CALL CLNGET (CLEAN, 'WINDOW', TYPE, DIM, WIN(1+WINP),
     *         CDUMMY, LERR)
            REBOXD = .TRUE.
C                                       REBOX
         ELSE IF (CHOICS(CHS).EQ.'REBOX') THEN
            DIM(1) = 8
            DIM(2) = 1
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,2),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
C                                       ignore error
            CALL OTVBOX (TVNAME, LERR)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,1),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL CLNGET (CLEAN, 'NBOXES', TYPE, DIM, NBOXES, CDUMMY,
     *         LERR)
            IF (LERR.NE.0) GO TO 980
            CALL CLNGET (CLEAN, 'WINDOW', TYPE, DIM, WIN(1+WINP),
     *         CDUMMY, LERR)
            REBOXD = .TRUE.
C                                       Edit UV data (IBLED like)
         ELSE IF (CHOICS(CHS)(:5).EQ.'EDIT ') THEN
            IF (.NOT.EDITOK) THEN
               MSGTXT = 'EDITING HAS BEEN TURNED OFF ... SORRY'
               CALL MSGWRT (6)
            ELSE
               IF (NSUBA.LE.1) THEN
                  ISUBA = 1
               ELSE IF (NSUBA.LE.8) THEN
                  READ (CHOICS(CHS),1106) ISUBA
               ELSE
                  CALL SCISUB (NSUBA, ISUBA)
                  END IF
               DIM(1) = 1
               DIM(2) = 1
               CALL OUVPUT (UVDATA, 'CALEDIT.SUBARR', OOAINT, DIM,
     *            ISUBA, CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990
               CALL OUVPUT (UVSCR, 'CALEDIT.SUBARR', OOAINT, DIM, ISUBA,
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990
               EDNAME = 'Edit object for SCIMG'
               MSGSAV = MSGSUP
               MSGSUP = 32000
               CALL OUVGET (UVDATA, 'EQU_TIME', TYPE, DIM, AVGT, CDUMMY,
     *            JERR)
               IF (JERR.NE.0) AVGT = -1.0
               CALL OUVGET (UVDATA, 'SOLINT', TYPE, DIM, SOLT, CDUMMY,
     *            JERR)
               MSGSUP = MSGSAV
               IF (JERR.NE.0) THEN
                  TYPE = OOARE
                  DIM(1) = 1
                  DIM(2) = 1
               ELSE
                  IF (AVGT.LE.0.0) AVGT = SOLT
                  END IF
               IF (AVGT.LE.0.0) AVGT = 3.0
               CALL OUVPUT (UVDATA, 'EQU_TIME', TYPE, DIM, AVGT, CDUMMY,
     *            JERR)
               IF (JERR.NE.0) GO TO 120
               GAPT = 3 * AVGT
               CALL OUVPUT (UVDATA, 'GAP_TIME', TYPE, DIM, GAPT, CDUMMY,
     *            JERR)
               IF (JERR.NE.0) GO TO 120
               CALL EDICRE (EDNAME, JERR)
               IF (JERR.NE.0) GO TO 120
               CALL EDIOPN (EDNAME, STATUS, JERR)
               IF (JERR.NE.0) GO TO 115
C                                       Insert object info
               DIM(1) = LEN(UVDATA)
               DIM(2) = 1
               CALL EDIPUT (EDNAME, 'UVMASTER', OOACAR, DIM, DUMMY,
     *            UVDATA, JERR)
               IF (JERR.NE.0) GO TO 120
               DIM(1) = LEN(UVSCR)
               CALL EDIPUT (EDNAME, 'UVSECOND', OOACAR, DIM, DUMMY,
     *            UVSCR, JERR)
               IF (JERR.NE.0) GO TO 120
               DIM(1) = LEN(TVNAME)
               CALL EDIPUT (EDNAME, 'TVDEVICE', OOACAR, DIM, DUMMY,
     *            TVNAME, JERR)
               IF (JERR.NE.0) GO TO 120
               DIM(1) = 1
               CALL EDIPUT (EDNAME, 'DOUVFLAG', OOALOG, DIM, DOFLAG,
     *            CDUMMY, JERR)
               IF (JERR.NE.0) GO TO 120
C                                       do it
               CALL EDITUV ('INIT', EDNAME, JERR)
C                                       Clean up - no apply
               IF (JERR.NE.0) THEN
                  CALL EDITUV ('ABOR', EDNAME, JERR)
                  IF (JERR.LT.0) JERR = 0
C                                       apply FG and clean up
               ELSE
                  CALL EDITUV ('APPL', EDNAME, JERR)
                  END IF
C                                       Delete object
 115           IF (JERR.NE.0) EDITOK = .FALSE.
               CALL EDICLO (EDNAME, JERR)
 120           IF (JERR.NE.0) EDITOK = .FALSE.
               CALL EDIDES (EDNAME, JERR)
               IF (JERR.NE.0) EDITOK = .FALSE.
               IF (.NOT.EDITOK) THEN
                  MSGTXT = 'PROBLEM WITH EDITING ' // EDNAME
                  CALL MSGWRT (7)
                  END IF
C                                       restore no subarray
               ISUBA = 0
               DIM(1) = 1
               DIM(2) = 1
               CALL OUVPUT (UVDATA, 'CALEDIT.SUBARR', OOAINT, DIM,
     *            ISUBA, CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990
               CALL OUVPUT (UVSCR, 'CALEDIT.SUBARR', OOAINT, DIM, ISUBA,
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990
C                                       restore display: on gray
               CALL OTVON (TVNAME, LERR)
               IF (LERR.NE.0) GO TO 980
               CALL SCHECK (CLEANO, MFIELD, MAXX, IMSIZE, NBOXES,
     *            WIN(1+WINP), TVFLD, LERR)
               IF (LERR.NE.0) GO TO 980
               DIM(1) = 8
               DIM(2) = 1
               CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,5),
     *            CDUMMY, LERR)
               IF (LERR.NE.0) GO TO 980
               CALL OTVLOD (TVNAME, LERR)
               END IF
C                                       manual parameter entry
         ELSE IF (CHOICS(CHS).EQ.'SET PARAMETERS') THEN
            CALL SCPARM (CLEAN, CLEANO, UVDATA, UVOUT, MXLOOP, ITVFLD,
     *         LERR)
C                                       force mode switch
         ELSE IF (CHOICS(CHS).EQ.'SWITCH TO A&P') THEN
            MSGTXT = 'Switching to amp & phase'
            CALL MSGWRT (4)
            FLIPED = .TRUE.
C                                       Continue self cal
         ELSE IF (CHOICS(CHS).EQ.'CONTINUE SELFCAL') THEN
            MSGTXT = 'Self-cal continuing'
            CALL MSGWRT (2)
            GO TO 980
C                                       Stop self cal
         ELSE IF (CHOICS(CHS).EQ.'STOP SELFCAL') THEN
            MSGTXT = 'Stop self-cal - done enough I guess!'
            CALL MSGWRT (4)
            DONE = .TRUE.
            GO TO 980
C                                       Die!
         ELSE IF (CHOICS(CHS).EQ.'ABORT TASK') THEN
            MSGTXT = 'Aborting task - gone wrong I guess!'
            CALL MSGWRT (4)
            DONE = .TRUE.
            IERR = 2
            GO TO 980
C                                       Stop cleaning
         ELSE IF (CHOICS(CHS).EQ.'TURN OFF TV') THEN
            MSGTXT = 'Turning off DOTV option - use TELL to turn on'
            CALL MSGWRT (4)
            TVFLD = 0
            DIM(1) = 1
            DIM(2) = 1
            CALL CLNPUT (CLEAN, 'SCTVFLD', OOAINT, DIM, TVFLD, CDUMMY,
     *         LERR)
            GO TO 980
C                                       SELECT FIELD n
         ELSE IF (CHOICS(CHS)(:7).EQ.'SELECT ') THEN
            IF (CHOICS(CHS)(:12).EQ.'SELECT FIELD') THEN
               KBP = 13
               CALL GETNUM (CHOICS(CHS), 16, KBP, X)
               I = X + 0.001
            ELSE
               MSGBUF = 'Enter field number to be selected'
               CALL INQINT (TTY, MSGBUF, 1, I, LERR)
               IF (LERR.GT.0) GO TO 980
               END IF
            IF (TVFLD.NE.I) THEN
               CALL OTVCLC (TVNAME, LERR)
               IF (LERR.NE.0) GO TO 980
               TVFLD = I
               DIM(1) = LEN (CLEANO(TVFLD))
               DIM(2) = 1
               CALL TVDPUT (TVNAME, 'TVOBJECT', OOACAR, DIM, DUMMY,
     *            CLEANO(TVFLD), LERR)
               IF (LERR.NE.0) GO TO 980
               DIM(1) = 1
               CALL CLNPUT (CLEAN, 'TVFIELD', OOAINT, DIM, TVFLD,
     *            CDUMMY, LERR)
               CALL SCHECK (CLEANO, MFIELD, MAXX, IMSIZE, NBOXES,
     *            WIN(1+WINP), TVFLD, LERR)
               IF (LERR.NE.0) GO TO 980
               DIM(1) = 8
               DIM(2) = 1
               CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,5),
     *            CDUMMY, LERR)
               IF (LERR.NE.0) GO TO 980
               CALL OTVLOD (TVNAME, LERR)
               END IF
            END IF
         END IF
      IF (LERR.EQ.0) GO TO 100
C                                       close downs
 980  CALL TVDCLO (TVNAME, JERR)
C                                       save field IDs in correct places
C                                       turn off on error
      IF (LERR.GT.0) TVFLD = 0
      IF ((TVFLD.GT.0) .AND. (ITVFLD.GT.0)) ITVFLD = TVFLD
      DIM(1) = 1
      DIM(2) = 1
      CALL CLNPUT (CLEAN, 'SCTVFLD', OOAINT, DIM, TVFLD, CDUMMY, JERR)
      CALL CLNPUT (CLEAN, 'TVFIELD', OOAINT, DIM, ITVFLD, CDUMMY, JERR)
      IF (REBOXD) THEN
         MSGSUP = 32000
         CALL OGET (CLEAN, 'OBOXFILE', TYPE, DIM, DUMMY, OBXFIL, JERR)
         MSGSUP = MSGSAV
         IF ((JERR.EQ.0) .AND. (OBXFIL.NE.' ')) THEN
            CALL WRIBOX (OBXFIL, NBOXES, MFIELD, WIN(1+WINP), JERR)
            IF (JERR.NE.0) THEN
               OBXFIL = ' '
               CALL OPUT (CLEAN, 'OBOXFILE', TYPE, DIM, DUMMY, OBXFIL,
     *            JERR)
               END IF
            END IF
         END IF
C
 985  CALL TVDDES (TVNAME, JERR)
 990  IF (LERR.GT.0) THEN
         IERR = MAX (1, IERR)
         MSGTXT = 'SCMATV : ERROR DISPLAYING ' // CLEAN
         CALL MSGWRT (6)
         END IF
      IF (TTY(2).GT.0) CALL ZCLOSE (TTY(1), TTY(2), J)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SELECT FIELD',I4)
 1006 FORMAT ('EDIT SUBA',I3)
 1035 FORMAT ('ERROR',I6,' OPENING THE TERMINAL')
 1100 FORMAT ('Field',I5,' SHIFT',2F12.4)
 1105 FORMAT ('DELETE',I7,' CURRENT BOXES? ANSWER Y/N')
 1106 FORMAT (9X,I3)
      END
      SUBROUTINE SCHECK (CLEANO, MFIELD, MAXX, IMSIZE, NBOXES, WIN,
     *   TVFLD, IERR)
C-----------------------------------------------------------------------
C   Check TBLC, TTRC, TXINC, TYINC for image field against WIN and the
C   TV size.  It forces TBLC, TTRC to encompass the full set of windows
C   and then sets TXINC and TYINC to allow the full image to be loaded
C   to the tv.
C   Inputs:
C      CLEANO   C(*)*?      Field object names
C      MFIELD   I           Number of fields
C      MAXX     I(2)        TV max pixels in x,y
C      TVFLD    I           Field number
C      IMSIZE   I(2,*)      Field image size
C      NBOXES   I(*)        Number of boxes
C      WIN      I(4,*,*)    Clean boxes
C   Output:
C      IERR     I      Error code
C-----------------------------------------------------------------------
      CHARACTER CLEANO(*)*(*)
      INTEGER   MFIELD, TVFLD, MAXX(2), IMSIZE(2,*), NBOXES(*),
     *   WIN(4,MFIELD,*), IERR
C
      INTEGER   TYPE, DIM(7), TTRC(7), TBLC(7), TXINC, TYINC, MSGSAV, I,
     *   J, LTRC(2), LBLC(2)
      REAL      PIXRNG(2)
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
C                                       read current parms
      I = 2
      MSGSUP = 32000
      CALL OGET (CLEANO(TVFLD), 'TBLC', TYPE, DIM, TBLC, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CALL FILL (7, 1, TBLC)
         I = I - 1
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 999
      MSGSUP = 32000
      CALL OGET (CLEANO(TVFLD), 'TTRC', TYPE, DIM, TTRC, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CALL FILL (7, 1, TTRC)
         TTRC(1) = IMSIZE(1,TVFLD)
         TTRC(2) = IMSIZE(2,TVFLD)
         I = I - 1
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 999
C                                       check WIN
      TBLC(1) = MAX (1, TBLC(1))
      TBLC(2) = MAX (1, TBLC(2))
      IF (TTRC(1).LE.TBLC(1)) TTRC(1) = IMSIZE(1,TVFLD)
      IF (TTRC(2).LE.TBLC(2)) TTRC(2) = IMSIZE(2,TVFLD)
      J = NBOXES(TVFLD)
      IF ((I.GT.0) .AND. (J.GT.0)) THEN
         LTRC(1) = 0
         LTRC(2) = 0
         LBLC(1) = 100000
         LBLC(2) = 100000
         DO 20 I = 1,J
            IF (WIN(1,TVFLD,I).EQ.-1) THEN
               LBLC(1) = MIN (LBLC(1), WIN(3,TVFLD,I)-WIN(2,TVFLD,I))
               LBLC(2) = MIN (LBLC(2), WIN(4,TVFLD,I)-WIN(2,TVFLD,I))
               LTRC(1) = MAX (LTRC(1), WIN(3,TVFLD,I)+WIN(2,TVFLD,I))
               LTRC(2) = MAX (LTRC(2), WIN(4,TVFLD,I)+WIN(2,TVFLD,I))
            ELSE
               LBLC(1) = MIN (LBLC(1), WIN(1,TVFLD,I))
               LBLC(2) = MIN (LBLC(2), WIN(2,TVFLD,I))
               LTRC(1) = MAX (LTRC(1), WIN(3,TVFLD,I))
               LTRC(2) = MAX (LTRC(2), WIN(4,TVFLD,I))
               END IF
 20         CONTINUE
         IF ((LBLC(1).LT.TBLC(1)) .OR. (LBLC(2).LT.TBLC(2)) .OR.
     *      (LTRC(1).GT.TTRC(1)) .OR. (LTRC(2).GT.TTRC(2))) THEN
            MSGTXT = 'Warning: not all boxes fit on display screen'
            CALL MSGWRT (6)
            END IF
         END IF
C                                       set increment
      TXINC = (TTRC(1) - TBLC(1) + 1) / MAX (MAXX(1), 1) + 1
      TYINC = (TTRC(2) - TBLC(2) + 1) / MAX (MAXX(2), 1) + 1
      IF ((TXINC.EQ.1) .AND. (TYINC.EQ.1)) THEN
         TXINC = MAXX(1) / (TTRC(1) - TBLC(1) + 1)
         IF (TXINC.GT.1) THEN
            IF (TXINC*(TTRC(1)-TBLC(1)+1).GT.520) TXINC = MAX (1,
     *         TXINC-1)
            TXINC = - MIN (TXINC, 3)
            END IF
         TYINC = MAXX(2) / (TTRC(2) - TBLC(2) + 1)
         IF (TYINC.GT.1) THEN
            IF (TYINC*(TTRC(2)-TBLC(2)+1).GT.520) TYINC = MAX (1,
     *         TYINC-1)
            TYINC = - MIN (TYINC, 3)
            END IF
         END IF
      IF (TYINC.LT.TXINC) TYINC = TXINC
      IF (TXINC.LT.TYINC) TXINC = TYINC
C                                       check pixrange
      CALL ARSGET (CLEANO(TVFLD), 'DATAMIN', TYPE, DIM, PIXRNG(1),
     *   CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ARSGET (CLEANO(TVFLD), 'DATAMAX', TYPE, DIM, PIXRNG(2),
     *   CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Announce
      IF (TXINC*TYINC.EQ.1) THEN
         WRITE (MSGTXT,1020) TVFLD, PIXRNG
      ELSE
         WRITE (MSGTXT,1021) TVFLD, PIXRNG, TXINC
         END IF
      CALL MSGWRT (2)
C                                       put in object
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (CLEANO(TVFLD), 'TXINC', OOAINT, DIM, TXINC, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (CLEANO(TVFLD), 'TYINC', OOAINT, DIM, TYINC, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 2
      CALL OPUT (CLEANO(TVFLD), 'PIXRANGE', OOARE, DIM, PIXRNG, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 7
      CALL OPUT (CLEANO(TVFLD), 'TBLC', OOAINT, DIM, TBLC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (CLEANO(TVFLD), 'TTRC', OOAINT, DIM, TTRC, CDUMMY, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Loading field',I3,' to TV from',1PE11.3,' to',1PE11.3)
 1021 FORMAT ('Loading field',I3,' to TV from',1PE10.2,' to',1PE10.2,
     *   ' every',I2,' pixel')
      END
      INTEGER FUNCTION NPTWO (N)
C-----------------------------------------------------------------------
C   Function to determine the next highest power of two for an integer.
C   Input:
C      N       I  Integer
C   Output:
C      NPTWO   I  Next highest power of two
C-----------------------------------------------------------------------
      INTEGER   N
C
      INTEGER   IPOW
      REAL      POW
C-----------------------------------------------------------------------
      POW = LOG (1.0*N) / LOG (2.0) + 0.99999
      IPOW = POW
      NPTWO = 2 ** IPOW
C
 999  RETURN
      END
