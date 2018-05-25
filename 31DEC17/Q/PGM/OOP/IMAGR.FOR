LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLN.INC'
      INTEGER   NPARMS, NKEYT
      PARAMETER (NPARMS=97)
C                                       NKEYT = no adverbs for TELL
      PARAMETER (NKEYT=16)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS), TELDIM(2,NKEYT),
     *   TELTYP(NKEYT)
      CHARACTER AVNAME(NPARMS)*8, TELK(NKEYT)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                     1         2          3        4
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
C           5          6       7          8           9          10
     *   'SOURCES', 'QUAL', 'CALCODE', 'TIMERANG', 'SELBAND', 'SELFREQ',
C           11        12          13          14          15
     *   'FREQID', 'SUBARRAY', 'ANTENNAS', 'BASELINE', 'DOCALIB',
C           16         17       18       19       20         21
     *   'GAINUSE', 'DOPOL', 'PDVER', 'BLVER', 'FLAGVER', 'DOBAND',
C           22       23        24        25       26       27
     *   'BPVER', 'SMOOTH', 'STOKES', 'BCHAN', 'ECHAN', 'CHANNEL',
C           28       29       30     31     32         33         34
     *   'NCHAV', 'CHINC', 'BIF', 'EIF', 'OUTNAME', 'OUTDISK', 'OUTSEQ',
C           35        36         37          38        39
     *   'OUTVER', 'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK',
C           40          41        42        43          44
     *   'CELLSIZE', 'IMSIZE', 'NFIELD', 'DO3DIMAG', 'FLDSIZE',
C           45          46          47         48         49
     *   'RASHIFT',  'DECSHIFT', 'UVTAPER', 'UVRANGE', 'GUARD',
C           50        51        52        53        54        55
     *   'ROTATE', 'ZEROSP', 'UVWTFN', 'UVSIZE', 'ROBUST', 'UVBOX',
C           56        57       58       59       60
     *   'UVBXFN', 'XTYPE', 'YTYPE', 'XPARM', 'YPARM',
C           61       62       63         64        65       66
     *   'NITER', 'BCOMP', 'ALLOKAY', 'NBOXES', 'CLBOX', 'BOXFILE',
C           67          68      69      70          7 1     72      73
     *   'OBOXFILE', 'GAIN', 'FLUX', 'MINPATCH', 'BMAJ', 'BMIN', 'BPA',
C           74         75         76          77      78        79
     *   'OVERLAP', 'ONEBEAM', 'OVRSWTCH', 'PHAT', 'FACTOR', 'CMETHOD',
C           80          81         82        83        84
     *   'IMAGRPRM', 'IM2PARM', 'NGAUSS', 'WGAUSS', 'FGAUSS',
C           85          86         87          88        89
     *   'MAXPIXEL', 'IN3NAME', 'IN3CLASS', 'IN3SEQ', 'IN3DISK',
C           90         91          92        93         94       95
     *   'IN4NAME', 'IN4CLASS', 'IN4SEQ', 'IN4DISK', 'FQTOL', 'DOTV',
C           96       97
     *   'LTYPE', 'BADDISK'/
C                    1       2       3       4
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT,
C          5       6       7       8      9      10
     *   OOACAR, OOAINT, OOACAR, OOARE, OOARE, OOARE,
C          11      12      13     14     15
     *   OOAINT, OOAINT, OOARE, OOARE, OOARE,
C          16      17      18      19      20     21
     *   OOAINT, OOAINT, OOAINT, OOAINT, OOAINT, OOAINT,
C          22      23      24     25      26      27
     *   OOAINT, OOARE, OOACAR, OOAINT, OOAINT, OOAINT,
C          28      29      30      31      32      33      34
     *   OOAINT, OOAINT, OOAINT, OOAINT, OOACAR, OOAINT, OOAINT,
C          35      36      37      38      39
     *   OOAINT, OOACAR, OOACAR, OOAINT, OOAINT,
C          40     41      42      43      44
     *   OOARE, OOAINT, OOAINT, OOALOG, OOAINT,
C          45     46     47     48     49
     *   OOARE, OOARE, OOARE, OOARE, OOARE,
C          50     51     52      53      54     55
     *   OOARE, OOARE, OOACAR, OOAINT, OOARE, OOAINT,
C          56      57      58      59     60
     *   OOAINT, OOAINT, OOAINT, OOARE, OOARE,
C          61      62      63      54      65      66
     *   OOAINT, OOAINT, OOAINT, OOAINT, OOAINT, OOACAR,
C          67      68     69     70      71     72     73
     *   OOACAR, OOARE, OOARE, OOAINT, OOARE, OOARE, OOARE,
C          74      75      76     77     78     79
     *   OOAINT, OOALOG, OOARE, OOARE, OOARE, OOACAR,
C          80     81      82      83     84
     *   OOARE, OOARE,  OOAINT, OOARE, OOARE,
C          85      86      87      88      89
     *   OOAINT, OOACAR, OOACAR, OOAINT, OOAINT,
C          90      91      92      93       94     95
     *   OOACAR, OOACAR, OOAINT, OOAINT,  OOARE, OOAINT,
C          96      97
     *   OOAINT, OOAINT/
C                   1     2     3     4
      DATA AVDIM /12,1,  6,1,  1,1,  1,1,
C          5      6     7     8     9     10
     *   16,30,  1,1,  4,1,  8,1,  1,1,  1,1,
C          11   12     13     14     15
     *   1,1,  1,1,  50,1,  50,1,   1,1,
C         16    17    18    19    20    21
     *   1,1,  1,1,  1,1,  1,1,  1,1,  1,1,
C         22    23    24    25    26    27
     *   1,1,  3,1,  4,1,  1,1,  1,1,  1,1,
C         28    29    30    31    32    33    34
     *   1,1,  1,1,  1,1,  1,1, 12,1,  1,1,  1,1,
C         35    36    37    38    39
     *   1,1, 12,1,  6,1,  1,1,  1,1,
C         40    41    42    43    44
     *   2,1,  2,1,  1,1,  1,1,  2,MAXAFL,
C              45         46    47    48    49
     *    MAXAFL,1, MAXAFL,1,  2,1,  2,1,  2,1,
C         50   51    52    53    54    55
     *   1,1,  5,1, 2,1,  2,1,  1,1,  1,1,
C         56    57     58     59    60
     *   1,1,  1,1,  1,1,  10,1,  10,1,
C         61         62    63    64    65          66
     *   1,1,  MAXAFL,1,  1,1,  1,1,  4,MXCLBX,  48,1,
C          67   68    69    70    71    72    73
     *   48,1, 1,1,  1,1,  1,1,  1,1,  1,1,  1,1,
C         74    75    76    77    78    79
     *   1,1,  1,1,  1,1,  1,1,  1,1,  4,1,
C          80    81   82     83    84
     *   20,1, 40,1, 1,1,  10,1, 10,1,
C         85    86    87    88    89
     *   1,1, 12,1,  6,1,  1,1,  1,1,
C          90   91    92    93    94    95
     *   12,1, 6,1,  1,1,  1,1,  1,1,  1,1,
C         96    97
     *   1,1, 10,1/
C                                       Adverbs to get via TELL
C                   1        2       3       4           5
      DATA TELK /'NITER', 'OBOXFILE', 'GAIN', 'FLUX', 'MINPATCH',
C           6       7       8      9           10        11
     *   'BMAJ', 'BMIN', 'BPA', 'OVRSWTCH', 'FACTOR', 'CMETHOD',
C           12          13         14          15      16
     *   'IMAGRPRM', 'IM2PARM', 'FGAUSS', 'MAXPIXEL', 'DOTV'/
      DATA TELTYP / OOAINT, OOACAR, OOARE, OOARE, OOAINT,
     *   OOARE, OOARE, OOARE, OOARE, OOARE, OOACAR,
     *   OOARE, OOARE, OOARE, OOAINT, OOAINT/
      DATA TELDIM / 1,1, 48,1, 1,1, 1,1, 1,1,
     *   1,1, 1,1, 1,1, 1,1, 1,1, 4,1,
     *   20,1, 40,1, 10,1, 1,1, 1,1/
LOCAL END
LOCAL INCLUDE 'IMAGRW.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NWEGHT
      REAL      WEGHTS(MAXCIF)
      COMMON /WEIGHT/ WEGHTS, NWEGHT
LOCAL END
LOCAL INCLUDE 'IMAGWIN.INC'
      INCLUDE 'INCS:PUVD.INC'
      LONGINT   WINP
      LOGICAL   REWIN
      INTEGER   WINM, WINW, WIN(2), NBOXES(MAXFLD)
      COMMON /IMAWIN/ WINP, REWIN, WINM, WINW, NBOXES, WIN
LOCAL END
      PROGRAM IMAGR
C-----------------------------------------------------------------------
C! Wide field and/or widefrequency  CLEANing/imaging task.
C# Task AP Imaging OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2003, 2005-2015, 2017
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
C   Barry Clark and SDI CLEAN
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, CLEAN*32, UVDATA*32
      INTEGER   IRET, BUFF1(256), NSUBA, SUBA(100)
      INCLUDE 'IMAGWIN.INC'
      INCLUDE 'IMAGRW.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'IMAGR'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL IMALIN (PRGM, CLEAN, UVDATA, NSUBA, SUBA, IRET)
C                                       CLEAN
      IF (IRET.EQ.0) CALL IMLEAN (CLEAN, UVDATA, NSUBA, SUBA, IRET)
C                                       History
      IF (IRET.LE.0) CALL IMAHIS (CLEAN, UVDATA)
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE IMALIN (PRGN, CLEAN, UVDATA, NSUBA, SUBA, IRET)
C-----------------------------------------------------------------------
C   IMALIN gets input parameters for IMAGR and creates the CLEAN object.
C   Inputs:
C      PRGN    C*6    Program name
C   Output:
C      CLEAN   C*32   Name of CLEAN object (contains output objects)
C      UVDATA  C*32   Name of input uv data.
C      NSUBA   I      Number of subarrays to process
C      SUBA    I(*)   List of subarrays to process
C      IRET    I      Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   NSUBA, SUBA(*), IRET
      CHARACTER PRGN*6, CLEAN*(*), UVDATA*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   NKEY1, NKEY2, NKEY3, NKEY4, NKEY5, NKEY6, MXSTAR
C                                       NKEY1=no. adverbs to copy to
C                                       CLEAN object
      PARAMETER (NKEY1=20)
C                                       NKEY2 = no. adverb for UVDATA
      PARAMETER (NKEY2=38)
C                                       NKEY3 = no. adverb for UVWORK
      PARAMETER (NKEY3=8)
C                                       NKEY4 = no. adverbs for BEAM
      PARAMETER (NKEY4=4)
C                                       NKEY5 = no. adverbs for CLEANO
      PARAMETER (NKEY5=15)
C                                       NKEY6 = no. adverbs for SPIX
      PARAMETER (NKEY6=4)
C                                       max size of Stars
      PARAMETER (MXSTAR = 100)
C
      INCLUDE 'INPUT.INC'
      INTEGER   DIM(7), TYPE, J, BCHAN, ECHAN, NAXIS(7), I, IMSI(2),
     *   IMSIZE(2,MAXFLD), NPTWO, DUMMY, FLDSIZ(2,MAXFLD), NXFLD, NYFLD,
     *   BFIELD, NXBEM(MAXFLD), K, NYBEM(MAXFLD), MAXNX, MAXNY, MINNX,
     *   MINNY, NITER, EDGEX, EDGEY, NFIELD, CHINC, NCHAV, BMSIZ(2),
     *   BIF, EIF, IFINDX, FQINDX, II, BMSSZ(2), IROUND, IS, NX, NY,
     *   JLOCD, JLOCR, SRCIDX, BSUM, TSEQ, TDISK, BCOMP(MAXFLD), MFIELD,
     *   NUMRES, OVRLAP, CLBOX(4,50), ALLOK, LIMTIM(2), NWORDS, LSUB1,
     *   LSUB2, UNWIN(16*MAXFLD), UNBOXS(MAXFLD), JTRIM, NSTAR,
     *   TVGRCS(3), IDATE(3), ITIME(3), XDOTV
      LONGINT   KP
      LOGICAL   NOREST, DOSCAL, DO3DIM, INTROK, LTEMP, WASZER, ONEBEM,
     *   FILTRS
      REAL      BMAJ, BMIN, BPA, IMPARM(20), TAPER(2,20), RASH(MAXFLD),
     *   DECSH(MAXFLD), CELLS(2), DU, DV, UMAX, VMAX, GUARDB(2), GUAU,
     *   GUAV, UVTAPR(2), UTFACT(2), CROTAU(7), ROTATE, COMRES(10),
     *   FACTOR, XDOCAL, TMPRES(10), FLXRES(10), TMPFLX(10), MNFLUX,
     *   IM2PRM(40), AUTOBX(6), HDRBM(3), IGNORE(MAXFLD),
     *   STPARM(4,MXSTAR), INVERT(3)
      DOUBLE PRECISION CRVAL(7), STPOS(2,MXSTAR)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, INK3(NKEY3)*8, OUTK3(NKEY3)*32, INK4(NKEY4)*8,
     *   OUTK4(NKEY4)*32, INK5(NKEY5)*8, OUTK5(NKEY5)*32, INK6(NKEY6)*8,
     *   OUTK6(NKEY6)*32, TELOBJ*32, CLEANO(MAXFLD)*32, BEAM(MAXFLD)*32,
     *   UVWORK*32, SPIX*32, STOKES*4, CLASS*6, CLTYPE*2, CHTYPE*4,
     *   CLNTYP*8, CNAME*8, KEYW*8, TINAME*12, TONAME*12, CDUMMY*1,
     *   BOXFIL*48, UVTYPE*2, TCLASS*6, OBXFIL*48, INK6B(NKEY6)*8,
     *   SPIXC*32, ANTAB*32
      INCLUDE 'IMAGWIN.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'IMAGRW.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to CLEAN object
C                   1       2       3       4        5
      DATA INK1 /'GAIN', 'FLUX', 'PHAT', 'NITER', 'ONEBEAM',
C           6       7       8      9         10
     *   'BMAJ', 'BMIN', 'BPA', 'FACTOR', 'MINPATCH',
C           11        12          13          14      15
     *   'OUTVER', 'IMAGRPRM', 'MAXPIXEL', 'DOTV', 'DO3DIMAG',
C           16         17          18        19          20
     *   'OVERLAP', 'OBOXFILE', 'ALLOKAY','OVRSWTCH', 'LTYPE'/
C                                       Rename
C                    1       2          3       4        5
      DATA OUTK1 /'GAIN', 'MINFLUX', 'PHAT', 'NITER', 'ONEBEAM',
C           6            7            8           9         10
     *   'BEAM.BMAJ', 'BEAM.BMIN', 'BEAM.BPA', 'FACTOR', 'MINPATCH',
C           11         12        13         14         15
     *   'VERSION', 'IMPARM', 'MAXNRES', 'TVFIELD', 'DO3DIMAG',
C           16         17          18         19          20
     *   'OVERLAP', 'OBOXFILE', 'ALLOKAY', 'OVRSWTCH', 'LTYPE'/
C                                       Adverbs for UVDATA object
C                                       leave out UVWTFN to do all in
C                                       better places
      DATA INK2 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'CHINC',
     *   'CMETHOD', 'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK',
     *   'NCHAV', 'ROTATE', 'BCHAN', 'ECHAN', 'BIF', 'EIF',
     *   'STOKES', 'GAINUSE', 'ANTENNAS', 'BASELINE',
     *   'FLAGVER', 'TIMERANG', 'UVRANGE',
     *   'SOURCES', 'QUAL', 'SELBAND',
     *   'SELFREQ', 'FREQID', 'CALCODE',
     *   'SUBARRAY', 'DOPOL', 'BLVER',
     *   'DOBAND', 'BPVER', 'SMOOTH', 'PDVER', 'ZEROSP', 'FQTOL'/
C                                       Rename
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'CHINC',
     *   'MODMETH', 'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK',
     *   'NCHAV', 'ROTATE',
     *   'CALEDIT.BCHAN', 'CALEDIT.ECHAN', 'CALEDIT.BIF', 'CALEDIT.EIF',
     *   'CALEDIT.STOKES', 'CALEDIT.CLUSE', 'ANTENNAS', 'BASELINE',
     *   'CALEDIT.FGVER', 'CALEDIT.TIMRNG', 'CALEDIT.UVRNG',
     *   'CALEDIT.SOURCS', 'CALEDIT.SELQUA', 'CALEDIT.SELBAN',
     *   'CALEDIT.SELFRQ', 'CALEDIT.FRQSEL', 'CALEDIT.SELCOD',
     *   'CALEDIT.SUBARR', 'CALEDIT.DOPOL', 'CALEDIT.BLVER',
     *   'CALEDIT.DOBAND', 'CALEDIT.BPVER', 'CALEDIT.SMOOTH',
     *   'CALEDIT.PDVER', 'ZEROSP', 'FQTOL'/
C                                       Adverbs for UVWORK object
C                    1        2         3
      DATA INK3 /'CHINC', 'CMETHOD', 'GUARD', 'IN2NAME', 'IN2CLASS',
     *   'IN2SEQ', 'IN2DISK', 'FQTOL'/
C                                       Rename
C                    1         2          3
      DATA OUTK3 /'CHINC', 'MODMETH', 'GUARDBND', 'NAME', 'CLASS',
     *   'IMSEQ', 'DISK', 'FQTOL'/
C                                       Adverbs for BEAM image object
C                    1          2          3         4
      DATA INK4 /'OUTNAME', 'OUTSEQ', 'OUTDISK', 'CELLSIZE'/
C                                       Rename
C                    1       2       3         4
      DATA OUTK4 /'NAME', 'IMSEQ', 'DISK', 'CELLSIZE'/
C                                       Adverbs for CLEANO image object
C                    1         2         3          4
      DATA INK5 /'OUTNAME', 'OUTSEQ', 'OUTDISK', 'CELLSIZE',
C           5        6        7        8        9         10
     *   'XTYPE', 'YTYPE', 'XPARM', 'YPARM', 'UVWTFN', 'ONEBEAM',
C           11       12        13        14        15
     *   'UVBOX', 'UVSIZE', 'ROBUST', 'UVBXFN', 'DO3DIMAG'/
C                                       Rename
C                    1       2        3       4
      DATA OUTK5 /'NAME', 'IMSEQ', 'DISK', 'CELLSIZE',
C           5        6        7        8        9         10
     *   'CTYPX', 'CTYPY', 'XPARM', 'YPARM', 'UVWTFN', 'ONEBEAM',
C           11       12        13        14        15
     *   'UVBOX', 'UVSIZE', 'ROBUST', 'UVBXFN', 'DO3DIMAG'/
C                                       Adverbs for SPIX image object
C                    1          2          3         4
      DATA INK6 /'IN3NAME', 'IN3CLASS', 'IN3SEQ', 'IN3DISK'/
      DATA INK6B /'IN4NAME', 'IN4CLASS', 'IN4SEQ', 'IN4DISK'/
C                                       Rename
C                    1       2          3        4
      DATA OUTK6 /'NAME', 'CLASS', 'IMSEQ', 'DISK'/
C-----------------------------------------------------------------------
C                                       Startup - as interactive
      CALL AV2INT (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) THEN
         RQUICK = .FALSE.
         GO TO 999
         END IF
C                                       DOTV = RQUICK false
      CALL OGET ('Input', 'DOTV', TYPE, DIM, XDOTV, CDUMMY, IRET)
      IF (IRET.NE.0) THEN
         RQUICK = .FALSE.
         GO TO 999
         END IF
      IF (XDOTV.GT.0) RQUICK = .FALSE.
C                                       Default NFIELD=1
      CALL OGET ('Input', 'NFIELD', TYPE, DIM, NFIELD, CDUMMY, IRET)
      IF (IRET.NE.0) THEN
         RQUICK = .FALSE.
         GO TO 999
         END IF
      IF (NFIELD.LE.0) THEN
         NFIELD = 1
         CALL OPUT ('Input', 'NFIELD', OOAINT, DIM, NFIELD, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) THEN
            RQUICK = .FALSE.
            GO TO 999
            END IF
         END IF
      CALL OGET ('Input', 'NGAUSS', TYPE, DIM, NUMRES, CDUMMY, IRET)
      IF (IRET.NE.0) THEN
         RQUICK = .FALSE.
         GO TO 999
         END IF
      CALL OGET ('Input', 'ONEBEAM', TYPE, DIM, ONEBEM, CDUMMY, IRET)
      IF (IRET.NE.0) THEN
         RQUICK = .FALSE.
         GO TO 999
         END IF
      IF (NFIELD.EQ.1) THEN
         ONEBEM = .TRUE.
         CALL OPUT ('Input', 'ONEBEAM', TYPE, DIM, ONEBEM, CDUMMY, IRET)
         IF (IRET.NE.0) THEN
            RQUICK = .FALSE.
            GO TO 999
            END IF
         END IF
      CALL OGET ('Input', 'FLUX', TYPE, DIM, MNFLUX, CDUMMY, IRET)
      IF (IRET.NE.0) THEN
         RQUICK = .FALSE.
         GO TO 999
         END IF
      IF (NUMRES.LE.0) THEN
         NUMRES = 1
         CALL RFILL (10, 0.0, COMRES)
         CALL RFILL (10, MNFLUX, FLXRES)
         CALL OPUT ('Input', 'NGAUSS', OOAINT, DIM, NUMRES, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) THEN
            RQUICK = .FALSE.
            GO TO 999
            END IF
         DIM(1) = 10
         CALL OPUT ('Input', 'WGAUSS', OOARE, DIM, COMRES, CDUMMY, IRET)
         IF (IRET.NE.0) THEN
            RQUICK = .FALSE.
            GO TO 999
            END IF
         CALL OPUT ('Input', 'FGAUSS', OOARE, DIM, FLXRES, CDUMMY, IRET)
         IF (IRET.NE.0) THEN
            RQUICK = .FALSE.
            GO TO 999
            END IF
      ELSE
         CALL OGET ('Input', 'WGAUSS', TYPE, DIM, COMRES, CDUMMY, IRET)
         IF (IRET.NE.0) THEN
            RQUICK = .FALSE.
            GO TO 999
            END IF
         CALL OGET ('Input', 'FGAUSS', TYPE, DIM, FLXRES, CDUMMY, IRET)
         IF (IRET.NE.0) THEN
            RQUICK = .FALSE.
            GO TO 999
            END IF
C                                       check order: 0 first
         K = 0
         DO 5 I = 1,NUMRES
            IF (COMRES(I).LE.0.0) THEN
               IF (K.EQ.0) THEN
                  K = I
               ELSE
                  MSGTXT = 'WARNING: 2nd 0-width resolution dropped'
                  CALL MSGWRT (6)
                  END IF
               END IF
 5          CONTINUE
         WASZER = K.NE.0
         IF (WASZER) THEN
            CALL RCOPY (10, COMRES, TMPRES)
            CALL RCOPY (10, FLXRES, TMPFLX)
            K = 1
            CALL RFILL (10, 0.0, COMRES)
            CALL RFILL (10, MNFLUX, FLXRES)
            DO 10 I = 1,NUMRES
               IF (TMPRES(I).EQ.0.0) THEN
                  FLXRES(1) = TMPFLX(I)
                  IF (FLXRES(1).EQ.0.0) FLXRES(1) = MNFLUX
               ELSE IF (TMPRES(I).GT.0.0) THEN
                  DO 8 J = 1,K
                     IF (TMPRES(I).EQ.COMRES(J)) GO TO 10
 8                   CONTINUE
                  K = K + 1
                  COMRES(K) = TMPRES(I)
                  FLXRES(K) = TMPFLX(I)
                  IF (FLXRES(K).EQ.0.0) FLXRES(K) = MNFLUX
                  END IF
 10            CONTINUE
            CALL OPUT ('Input', 'WGAUSS', TYPE, DIM, COMRES, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) THEN
               RQUICK = .FALSE.
               GO TO 999
               END IF
            CALL OPUT ('Input', 'FGAUSS', TYPE, DIM, FLXRES, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) THEN
               RQUICK = .FALSE.
               GO TO 999
               END IF
            IF (K.NE.NUMRES) THEN
               WRITE (MSGTXT,1010) K
               CALL MSGWRT (6)
               NUMRES = K
               DIM(1) = 1
               CALL OPUT ('Input', 'NGAUSS', OOAINT, DIM, NUMRES,
     *            CDUMMY, IRET)
               IF (IRET.NE.0) THEN
                  RQUICK = .FALSE.
                  GO TO 999
                  END IF
               END IF
         ELSE
            MSGTXT = 'WARNING: NO ZERO-SIZE RESOLUTION'
            CALL MSGWRT (6)
            END IF
         DO 15 I = 1,10
            COMRES(I) = MAX (0.0, COMRES(I)) / 3600.0
 15         CONTINUE
         END IF
C                                       compute tapers to use
      CALL OGET ('Input', 'UVTAPER', TYPE, DIM, UVTAPR, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      UVTAPR(1) = MAX (0.0, UVTAPR(1))
      UVTAPR(2) = MAX (0.0, UVTAPR(2))
      FACTOR = SQRT (-4.0 * LOG (2.0) * LOG (0.3)) * 180.0 / (PI * PI *
     *   1000.0)
      J = 0
      DO 20 I = 1,NUMRES
         IF (COMRES(I).LE.0) THEN
            TAPER(1,I) = 0.0
            TAPER(2,I) = 0.0
            J = J + 1
         ELSE
            TAPER(1,I) = FACTOR / COMRES(I)
            TAPER(2,I) = TAPER(1,I)
            END IF
 20      CONTINUE
      IF (J.LE.0) THEN
         MSGTXT = 'WARNING: NO FIELD WITH POINT SOURCE MODEL'
         CALL MSGWRT (6)
      ELSE IF (J.GT.1) THEN
         IRET = 8
         MSGTXT = 'ERROR: MORE THAN ONE FIELD WITH POINT SOURCE MODEL'
         CALL MSGWRT (8)
         RQUICK = .FALSE.
         GO TO 999
         END IF
C                                       check max field number
      MFIELD = NFIELD * NUMRES
      IF (MFIELD.GT.MAXFLD) THEN
         MSGTXT = 'TOO MANY FIELDS * RESOLUTIONS REQUESTED'
         IRET = 8
         CALL MSGWRT (8)
         RQUICK = .FALSE.
         GO TO 999
         END IF
C                                       Resume if MFIELD < MAXAFL
      IF (MFIELD.GT.MAXAFL) RQUICK = .FALSE.
      IF (RQUICK) CALL RELPOP (0, SBUFF, IRET)
C                                       Declare 'PARANGLE' a header
C                                       keyword for the image class.
      CNAME = 'IMAGE'
      KEYW = 'PARANGLE'
      CALL OBVHKW (CNAME, KEYW, OOARE, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Declare 'ZENANGLE' a header
C                                       keyword for the image class.
      CNAME = 'IMAGE'
      KEYW = 'ZENANGLE'
      CALL OBVHKW (CNAME, KEYW, OOARE, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BADDISK
      CALL OGET ('Input', 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       CLEANing?
      CALL OGET ('Input', 'NITER', TYPE, DIM, NITER, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       ALLOKAY testing
      CALL OGET ('Input', 'ALLOKAY', TYPE, DIM, ALLOK, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Default output Name = input
      CALL OGET ('Input', 'INNAME', TYPE, DIM, DUMMY, TINAME, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'OUTNAME', TYPE, DIM, DUMMY, TONAME, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (TONAME.EQ.' ') THEN
         TONAME = TINAME
         CALL OPUT ('Input', 'OUTNAME', TYPE, DIM, DUMMY, TONAME, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      IF (ALLOK.GE.2) THEN
         CALL OGET ('Input', 'IN2NAME', TYPE, DIM, DUMMY, TINAME, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OGET ('Input', 'IN2CLASS', TYPE, DIM, DUMMY, TCLASS, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OGET ('Input', 'IN2SEQ', TYPE, DIM, TSEQ, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OGET ('Input', 'IN2DISK', TYPE, DIM, TDISK, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         IF ((TINAME.EQ.' ') .OR. (TCLASS.EQ.' ') .OR. (TSEQ.LE.0) .OR.
     *      (TDISK.LE.0)) ALLOK = 1
         END IF
      IF (ALLOK.GE.1) THEN
         CALL OGET ('Input', 'OUTSEQ', TYPE, DIM, TSEQ, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OGET ('Input', 'OUTDISK', TYPE, DIM, TDISK, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         IF ((TSEQ.LE.0) .OR. (TDISK.LE.0)) ALLOK = 0
         END IF
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT ('Input', 'ALLOKAY', OOAINT, DIM, ALLOK, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       3D imaging # beams
      CALL OGET ('Input', 'DO3DIMAG', TYPE, DIM, DO3DIM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Check multi-res
      IF (NUMRES.GT.1) THEN
         CALL OGET ('Input', 'OVERLAP', TYPE, DIM, OVRLAP, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (OVRLAP.LE.1) THEN
            MSGTXT = 'OVERLAP CHANGED TO 2 FOR MULTI-FIELD CLEAN'
            CALL MSGWRT (7)
            OVRLAP = 2
            CALL OPUT ('Input', 'OVERLAP', TYPE, DIM, OVRLAP, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         END IF
C                                       Create CLEAN object
      CLEAN = 'CLEAN process object'
      CALL CREATE (CLEAN, 'CLEAN', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       OBOXFILE special usage
      CALL OGET ('Input', 'OBOXFILE', TYPE, DIM, DUMMY, OBXFIL, IRET)
      IF (IRET.NE.0) GO TO 999
      I = JTRIM (OBXFIL)
      IF ((I.GT.0) .AND. (OBXFIL(I:I).EQ.':')) THEN
         CALL ZDATE (IDATE)
         CALL ZTIME (ITIME)
         WRITE (OBXFIL(I+1:),1050) IDATE, ITIME
         CALL OPUT ('Input', 'OBOXFILE', TYPE, DIM, DUMMY, OBXFIL, IRET)
         IF (IRET.NE.0) GO TO 999
         I = JTRIM (OBXFIL)
         WRITE (MSGTXT,1051) OBXFIL(:I)
         CALL MSGWRT (2)
         END IF
C                                       Copy adverbs to object
      CALL FILL (MAXFLD, 0, BCOMP)
      CALL OGET ('Input', 'BCOMP', TYPE, DIM, BCOMP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, CLEAN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       temporary OBOXFILE
      IF (OBXFIL.EQ.' ') THEN
         CALL ZDATE (IDATE)
         CALL ZTIME (ITIME)
         WRITE (OBXFIL,1052) IDATE, ITIME
         DIM(1) = 48
         DIM(2) = 1
         CALL OPUT (CLEAN, 'OBOXFILE', OOACAR, DIM, DUMMY, OBXFIL, IRET)
         IF (IRET.NE.0) GO TO 999
         I = JTRIM (OBXFIL)
         WRITE (MSGTXT,1053) OBXFIL(:I)
         CALL MSGWRT (2)
         END IF
      IF (MFIELD.GT.MAXAFL) THEN
         CALL ISUM (MAXAFL, BCOMP, BSUM)
         BSUM = BSUM / MAXAFL
         CALL FILL (MAXFLD-MAXAFL, BSUM, BCOMP(MAXAFL+1))
         END IF
      DIM(1) = 1
      INTROK = .NOT.RQUICK
      CALL OPUT (CLEAN, 'INTACTOK', OOALOG, DIM, INTROK, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (CLEAN, 'NUMRES', OOAINT, DIM, NUMRES, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      TVGRCS(1) = 5
      TVGRCS(2) = 7
      TVGRCS(3) = 6
      DIM(1) = 3
      CALL OPUT (CLEAN,'TVGRCHAN', OOAINT, DIM, TVGRCS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Convert BMAJ, BMIN to degrees
      CALL OGET ('Input', 'BMAJ', TYPE, DIM, BMAJ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      BMAJ = BMAJ / 3600.0
      CALL OPUT (CLEAN, 'BEAM.BMAJ', TYPE, DIM, BMAJ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'BMIN', TYPE, DIM, BMIN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      BMIN = BMIN / 3600.0
      CALL OPUT (CLEAN, 'BEAM.BMIN', TYPE, DIM, BMIN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Set CLEAN type
      CLNTYP = 'UV'
      DIM(1) = LEN (CLNTYP)
      DIM(2) = 1
      CALL OPUT (CLEAN, 'CLEANTYP', OOACAR, DIM, DUMMY, CLNTYP, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Set default stokes.
      CALL OGET ('Input', 'STOKES', TYPE, DIM, DUMMY, STOKES, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (STOKES.EQ.' ') STOKES = 'I'
      CALL OPUT ('Input', 'STOKES', TYPE, DIM, DUMMY, STOKES, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create other objects
C                                       UVDATA
      UVDATA = 'Input UVdata'
      CALL CREATE (UVDATA, 'UVDATA', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       DOCALIB
      CALL OGET ('Input', 'DOCALIB', TYPE, DIM, XDOCAL, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      LTEMP = XDOCAL.GT.0.0
      CALL OPUT (UVDATA, 'CALEDIT.DOCAL', OOALOG, DIM, LTEMP, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      LTEMP = (XDOCAL.GT.0.0) .AND. (XDOCAL.LE.99.0)
      CALL OPUT (UVDATA, 'CALEDIT.DOWTCL', OOALOG, DIM, LTEMP, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get IF channel selection
      CALL SECSLT (UVDATA, BIF, EIF, BCHAN, ECHAN, STOKES, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       All Stokes', IF, channels
      CALL SECSAV (UVDATA, 1, 0, 1, 0, '    ', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       all subarrays
      CALL SECGET (UVDATA, 'SUBARR', TYPE, DIM, SUBA, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       check out requested subarrays
      IF (SUBA(1).GT.0) THEN
         NSUBA = 1
      ELSE
         ANTAB = 'AN table for UVTAVG'
         LSUB1 = 1
         CALL UV2TAB (UVDATA, ANTAB, 'AN', LSUB1, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       How many antennas files
         CALL TBLHIV (ANTAB, LSUB2, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OUVDES (ANTAB, IRET)
         IF (IRET.NE.0) GO TO 999
         LSUB1 = 1
         LSUB2 = MAX (1, LSUB2)
         IF (LSUB2.GT.1) THEN
            NSUBA = 0
            DO 30 I = 1,LSUB2
               CALL SECPUT (UVDATA, 'SUBARR', TYPE, DIM, I, CDUMMY,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
               MSGSUP = 32000
               CALL OOPEN (UVDATA, 'READ', IRET)
               MSGSUP = 0
               IF (IRET.EQ.0) THEN
                  NSUBA = NSUBA + 1
                  SUBA(NSUBA) = I
                  CALL OCLOSE (UVDATA, IRET)
                  IF (IRET.NE.0) GO TO 999
                  END IF
 30            CONTINUE
            IF (NSUBA.LE.0) THEN
               IRET = 8
               MSGTXT = 'NO SUBARRAYS WITH DATA WERE FOUND'
               CALL MSGWRT (8)
               GO TO 999
               END IF
            I = SUBA(1)
         ELSE
            NSUBA = 1
            I = 1
            END IF
         CALL SECPUT (UVDATA, 'SUBARR', TYPE, DIM, I, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Open UVDATA to be sure it's OK.
      CALL OOPEN (UVDATA, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Check data type
      CALL UVDGET (UVDATA, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (UVTYPE(:1).NE.'U') THEN
         MSGTXT = 'I DO NOT WORK FOR UV DATA OF TYPE ''' // UVTYPE //
     *      ''''
         IRET = 9
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       East-West not 3D
      IF ((UVTYPE.NE.'UV') .AND. (DO3DIM)) THEN
         IF ((NUMRES.LE.0) .AND. (MFIELD.LE.MAXAFL)) DO3DIM = .FALSE.
         DIM(1) = 1
         DIM(2) = 1
         CALL OPUT ('Input', 'DO3DIMAG', OOALOG, DIM, DO3DIM, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT (CLEAN, 'DO3DIMAG', OOALOG, DIM, DO3DIM, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         IF (DO3DIM) THEN
            MSGTXT = 'Warning: using DO3DIM true for NCP data'
            CALL MSGWRT (7)
            END IF
         END IF
      CALL OCLOSE (UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
      BFIELD = MFIELD
C                                       Reset Selection
      CALL SECSAV (UVDATA, BIF, EIF, BCHAN, ECHAN, STOKES, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       check that only one source made
C                                       into the selected UV data file.
      CALL UVDFND (UVDATA, 1, 'SOURCE', SRCIDX, IRET)
C                                       The OOP system should have masked
C                                       the source random parameter if
C                                       this is a single source file or
C                                       only one source was selected.
C                                       If IRET is zero then a SOURCE
C                                       random parameter was found.
      IF (IRET.EQ.0) THEN
         MSGTXT = '**  ONE and ONLY ONE source must be specified'
         CALL MSGWRT (6)
         MSGTXT = '**  Use SPLIT & DBCON to image multiple sources'
         CALL MSGWRT (6)
         IRET = 16
         GO TO 999
         END IF
C                                       UVWORK
      UVWORK = 'UVdata work object'
      CALL CREATE (UVWORK, 'UVDATA', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY3, INK3, OUTK3, UVWORK, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BEAM
      DO 40 I = 1,BFIELD
         WRITE (BEAM(I),1002) I
         IF ((.NOT.ONEBEM) .OR. (MOD(I,NFIELD).EQ.1) .OR. (NFIELD.EQ.1))
     *      THEN
            CALL CREATE (BEAM(I), 'IMAGE', IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
            CALL IN2OBJ ('Input', NKEY4, INK4, OUTK4, BEAM(I), IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Classes derived from Stokes'
            CLTYPE = 'BM'
            IF (I.LE.999) THEN
               WRITE (CLASS,1040) STOKES(1:1), CLTYPE, I
            ELSE
               WRITE (CLASS,1041) STOKES(1:1), CLTYPE(1:1), I
               END IF
            DIM(1) = 6
            CALL OPUT (BEAM(I), 'CLASS', OOACAR, DIM, DUMMY, CLASS,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 40      CONTINUE
C                                       CLEAN
      J = 1
      CALL RFILL (3, 0.0, HDRBM)
      DO 50 I = 1,MFIELD
         WRITE (CLEANO(I),1000) I
         CALL CREATE (CLEANO(I), 'IMAGE', IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
         CALL IN2OBJ ('Input', NKEY5, INK5, OUTK5, CLEANO(I), IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Class from stokes
         IF (NITER.GT.0) THEN
            CLTYPE = 'CL'
         ELSE
            CLTYPE = 'IM'
            END IF
         IF (I.LE.999) THEN
            WRITE (CLASS,1040) STOKES(1:1), CLTYPE, I
         ELSE
            WRITE (CLASS,1041) STOKES(1:1), CLTYPE(1:1), I
            END IF
         DIM(1) = LEN (CLASS)
         DIM(2) = 1
         CALL OPUT (CLEANO(I), 'CLASS', OOACAR, DIM, DUMMY, CLASS, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Model component diameter
         DIM(1) = 1
         CALL OPUT (CLEANO(I), 'COMPDIAM', OOARE, DIM, COMRES(J),
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT (CLEANO(I), 'MINFLUX', OOARE, DIM, FLXRES(J),
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT (CLEANO(I), 'NUMRES', OOAINT, DIM, NUMRES, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
C                                       taper
         DIM(1) = 2
         CALL OPUT (CLEANO(I), 'WTTAPER', OOARE, DIM, UVTAPR, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT (CLEANO(I), 'UVTAPER', OOARE, DIM, TAPER(1,J),
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       header beam
         DIM(1) = 3
         CALL OPUT (CLEANO(I), 'HDRBEAM', OOARE, DIM, HDRBM, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
C                                       increment resolution number
         IF (MOD(I,NFIELD).EQ.0) J = J + 1
 50      CONTINUE
C                                       Attach Images etc. to CLEAN
C                                       BEAM image
      DIM(1) = LEN (BEAM(1))
      DIM(2) = BFIELD
      DIM(3) = 1
      CALL OPUT (CLEAN, 'DIRTBEAM', OOACAR, DIM, DUMMY, BEAM, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       CLEAN image(s)
      DIM(1) = LEN (CLEANO(1))
      DIM(2) = MFIELD
      CALL OPUT (CLEAN, 'CLEANI', OOACAR, DIM, DUMMY, CLEANO, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Number of fields
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (CLEAN, 'NIMAGES', OOAINT, DIM, MFIELD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       UVWORK
      DIM(1) = LEN (UVWORK)
      DIM(2) = 1
      CALL OPUT (CLEAN, 'UVDATA', OOACAR, DIM, DUMMY, UVWORK, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Process other info
C                                       Field size
      CALL FILL (2*MAXFLD, 0, FLDSIZ)
      CALL OGET ('Input', 'FLDSIZE', TYPE, DIM, FLDSIZ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Min. image sizes
      CALL FILL (2*MAXFLD, 0, IMSIZE)
      CALL OGET ('Input', 'IMSIZE', TYPE, DIM, IMSI, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL RFILL (MAXFLD, 0.0, RASH)
      CALL OGET ('Input', 'RASHIFT', TYPE, DIM, RASH, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL RFILL (MAXFLD, 0.0, DECSH)
      CALL OGET ('Input', 'DECSHIFT', TYPE, DIM, DECSH, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BOXFILE option for field
C                                       parameters
      CALL OGET ('Input', 'BOXFILE', TYPE, DIM, DUMMY, BOXFIL, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (BOXFIL.NE.' ') THEN
C                                       Need coords and rotation
         CALL OUVGET (UVDATA, 'ROTATE', TYPE, DIM, ROTATE, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL UVDGET (UVDATA, 'CRVAL', TYPE, DIM, CRVAL, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL UVDGET (UVDATA, 'CROTA', TYPE, DIM, CROTAU, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL UVDFND (UVDATA, 2, 'RA', JLOCR, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL UVDFND (UVDATA, 2, 'DEC', JLOCD, IRET)
         IF (IRET.NE.0) GO TO 999
         ROTATE = ROTATE + CROTAU(JLOCD)
         CALL FIELDF (BOXFIL, NFIELD, MFIELD, CRVAL(JLOCR),
     *      CRVAL(JLOCD), ROTATE, FLDSIZ, RASH, DECSH, BCOMP, IRET)
         IF (IRET.GT.0) THEN
            MSGTXT = 'IMALIN: Error in defining field parms from file'
            CALL MSGWRT (7)
            GO TO 999
            END IF
         END IF
C                                       put back in objects
      DO 60 I = 2,NUMRES
         J = (I-1) * NFIELD + 1
         CALL RCOPY (NFIELD, RASH(1), RASH(J))
         CALL RCOPY (NFIELD, DECSH(1), DECSH(J))
         CALL COPY (2*NFIELD, FLDSIZ(1,1), FLDSIZ(1,J))
 60      CONTINUE
      DIM(1) = MAXAFL
      DIM(2) = 1
      CALL OPUT ('Input', 'RASHIFT', OOARE, DIM, RASH, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT ('Input', 'DECSHIFT', OOARE, DIM, DECSH, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT ('Input', 'BCOMP', OOAINT, DIM, BCOMP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = MAXFLD
      CALL OPUT (CLEAN, 'BCOMP', OOAINT, DIM, BCOMP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (CLEANO(1), 'RASHIFT', OOARE, DIM, RASH, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (CLEANO(1), 'DECSHIFT', OOARE, DIM, DECSH, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       No images smaller than 64
C                                       and must be power of 2
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
      DIM(1) = 2
      DIM(2) = MAXAFL
      CALL OPUT ('Input', 'FLDSIZE', OOAINT, DIM, FLDSIZ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(2) = MAXFLD
      CALL OPUT (CLEANO(1), 'IMSIZE', OOAINT, DIM, IMSIZE, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(2) = 1
      DO 80 I = 2,MFIELD
         CALL OPUT (CLEANO(I), 'IMSIZE', OOAINT, DIM, IMSIZE(1,I),
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
 80      CONTINUE
C                                       Beam size
      CALL OGET ('Input', 'NITER', TYPE, DIM, NITER, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Control information.
      CALL OGET ('Input', 'IMAGRPRM', TYPE, DIM, IMPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'IM2PARM', TYPE, DIM, IM2PRM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       time limits
      LIMTIM(1) = IROUND (IM2PRM(8))
      LIMTIM(2) = IROUND (IM2PRM(9))
      DIM(1) = 2
      DIM(2) = 1
      CALL OPUT (CLEAN, 'TVTIMLIM', OOAINT, DIM, LIMTIM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       auto boxing
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
      IF (AUTOBX(6).GT.IMSIZE(1,1)/20.0) AUTOBX(6) = 5.0
      IM2PRM(1) = AUTOBX(1)
      IM2PRM(2) = AUTOBX(2)
      IM2PRM(3) = AUTOBX(3)
      IM2PRM(4) = AUTOBX(4)
      IM2PRM(5) = AUTOBX(5)
      IM2PRM(6) = AUTOBX(6)
      IF (IM2PRM(7).EQ.0.0) THEN
         IF (AUTOBX(1).GT.0.5) THEN
            IM2PRM(7) = 1.0
         ELSE
            IM2PRM(7) = -1.0
            END IF
      ELSE IF (IM2PRM(7).GT.0.0) THEN
         IM2PRM(7) = 1.0
      ELSE
         IM2PRM(7) = -1.0
         END IF
      REWIN = IM2PRM(7).GT.0.0
C                                       inverse taper
      INVERT(1) = IM2PRM(16)
      INVERT(2) = IM2PRM(17)
      INVERT(3) = IM2PRM(18)
      IF ((INVERT(1).LE.0.0) .OR. (INVERT(1).GE.1.0) .OR.
     *   (INVERT(2).LE.0.0)) THEN
         CALL RFILL (3, 0.0, INVERT)
      ELSE IF (INVERT(3).LE.0.0) THEN
         INVERT(3) = INVERT(2)
         END IF
C                                       set beam size
      DIM(1) = 2
      DIM(2) = 1
      DO 85 I = 1,BFIELD
         IF ((.NOT.ONEBEM) .OR. (MOD(I,NFIELD).EQ.1) .OR. (NFIELD.EQ.1))
     *      THEN
            IF (ONEBEM) THEN
               NX = MAXNX
               NY = MAXNY
            ELSE
               NX = IMSIZE(1,I)
               NY = IMSIZE(2,I)
               END IF
C                                       If not cleaning beam = field
            IF (NITER.LE.0) THEN
               NXBEM(I) = NX
               NYBEM(I) = NY
C                                       else large or smaller
            ELSE IF (IMPARM(10).LT.0.2) THEN
               NXBEM(I) = MIN (NX*2, 2048)
               NYBEM(I) = MIN (NY*2, 2048)
            ELSE IF (IMPARM(10).GT.1.5) THEN
               NXBEM(I) = MIN (NX*2, MAXIMG)
               NYBEM(I) = MIN (NY*2, MAXIMG)
            ELSE IF (IMPARM(10).GT.0.75) THEN
               NXBEM(I) = MIN (NX, 4096)
               NYBEM(I) = MIN (NY, 4096)
            ELSE IF (IMPARM(10).GT.0.37) THEN
               NXBEM(I) = MIN (NX/2, 2048)
               NYBEM(I) = MIN (NY/2, 2048)
            ELSE
               NXBEM(I) = MIN (NX/4, 2048)
               NYBEM(I) = MIN (NY/4, 2048)
               END IF
            BMSIZ(1) = NXBEM(I)
            BMSIZ(2) = NYBEM(I)
            CALL OPUT (BEAM(I), 'IMSIZE', OOAINT, DIM, BMSIZ, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 85      CONTINUE
C                                       Define the clean boxes
C                                       Create clean windows
      WINM = MIN (MXNBOX, MXNBFL/MFIELD)
      WINW = 4 * MFIELD * WINM
      NWORDS = (WINW - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'IMAGR', NWORDS, WIN(1), WINP, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Start with program inputs
      CALL FILL (NFIELD, 1, NBOXES)
      CALL FILL (WINW, 0, WIN(1+WINP))
      CALL OGET ('Input', 'NBOXES', TYPE, DIM, NBOXES, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Window
      CALL OGET ('Input', 'CLBOX', TYPE, DIM, CLBOX, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NBOXES(1).GT.0) THEN
         IS = 2
         DO 87 K = 1,NBOXES(1)
            KP = (K-1) * MFIELD * 4 + 1 + WINP
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
      IF (BOXFIL(1:1).NE.' ') THEN
         CALL WINDF (WIN(1+WINP), NBOXES, MFIELD, IMSIZE, BOXFIL, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'IMALIN: Error in defining clean boxes from file'
            CALL MSGWRT(7)
            GO TO 999
            END IF
         END IF
      DO 100 I = 1,NFIELD
         IF (NBOXES(I).GT.0) THEN
            J = I
            DO 95 II = 2,NUMRES
               J = J + NFIELD
               IF (NBOXES(J).LE.0) THEN
                  NBOXES(J) = NBOXES(I)
                  DO 94 K = 1,NBOXES(I)
                     KP = (K-1)*MFIELD*4 + WINP - 3
                     CALL COPY (4, WIN(KP+4*I), WIN(KP+4*J))
 94                  CONTINUE
                  END IF
 95            CONTINUE
            END IF
 100     CONTINUE
C                                       BOXFILE option?
      IF (BOXFIL(1:1).NE.' ') THEN
         CALL UWINDF (UNWIN, UNBOXS, MFIELD, IMSIZE, BOXFIL, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'IMALIN: Error in defining clean boxes from file'
            CALL MSGWRT(7)
            GO TO 999
            END IF
         END IF
      DO 110 I = 1,NFIELD
         IF (UNBOXS(I).GT.0) THEN
            J = I
            DO 105 II = 2,NUMRES
               J = J + NFIELD
               IF (UNBOXS(J).LE.0) THEN
                  UNBOXS(J) = UNBOXS(I)
                  DO 104 K = 1,UNBOXS(I)
                     KP = (K-1)*MFIELD*4 - 3
                     CALL COPY (4, UNWIN(KP+4*I), UNWIN(KP+4*J))
 104                 CONTINUE
                  END IF
 105           CONTINUE
            END IF
 110     CONTINUE
C                                       ignore too
      CALL RFILL (MFIELD, 1.0, IGNORE)
      IF (BOXFIL(1:1).NE.' ') THEN
         CALL IGNORF (BOXFIL, MFIELD, IGNORE, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'IMALIN: Error in defining clean boxes from file'
            CALL MSGWRT(7)
            GO TO 999
            END IF
         END IF
C                                       put parms back
      DIM(1) = MFIELD
      DIM(2) = 1
      CALL OPUT (CLEAN, 'IGNORE', OOARE, DIM, IGNORE, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (CLEAN, 'NBOXES', OOAINT, DIM, NBOXES, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 4 * MFIELD
      DIM(2) = WINW / DIM(1)
      CALL OPUT (CLEAN, 'WINDOW', OOAINT, DIM, WIN(1+WINP), CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = MFIELD
      DIM(2) = 1
      CALL OPUT (CLEAN, 'UNBOXES', OOAINT, DIM, UNBOXS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 4 * MFIELD
      DIM(2) = (4 * MAXFLD) / MFIELD
      DIM(2) = MIN (DIM(2), MXNBOX)
      CALL OPUT (CLEAN, 'UNWINDOW', OOAINT, DIM, UNWIN, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Restore?
      CALL OGET ('Input', 'BMAJ', TYPE, DIM, BMAJ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'BMIN', TYPE, DIM, BMIN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'BPA', TYPE, DIM, BPA, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      NOREST = BMAJ .LT. 0.0
      DIM(1) = 1
      CALL OPUT (CLEAN, 'NORESTORE', OOALOG, DIM, NOREST, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF ((.NOT.NOREST) .AND. (WASZER)) THEN
         CALL OPUT (CLEAN, 'NFLDRSTR', OOAINT, DIM, NFIELD, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Channel stuff
C                                       Default BCHAN, ECHAN
      CALL OGET ('Input', 'BCHAN', TYPE, DIM, BCHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'ECHAN', TYPE, DIM, ECHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Find size of freq. axis.
      CALL UVDFND (UVDATA, 2, 'FREQ', FQINDX, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (UVDATA, 'UV_DESC.NAXIS', TYPE, DIM, NAXIS, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      BCHAN = MAX (BCHAN, 1)
      BCHAN = MIN (BCHAN, NAXIS(FQINDX))
      IF (ECHAN.LT.BCHAN) ECHAN = NAXIS(FQINDX)
      ECHAN = MIN (ECHAN, NAXIS(FQINDX))
      NWEGHT = NAXIS(FQINDX)
      DIM(1) = 1
      CALL OPUT (CLEAN, 'BCHAN', OOAINT, DIM, BCHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (CLEAN, 'ECHAN', OOAINT, DIM, ECHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL SECPUT (UVDATA, 'BCHAN', OOAINT, DIM, BCHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL SECPUT (UVDATA, 'ECHAN', OOAINT, DIM, ECHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Save in Inputs for history
      CALL OPUT ('Input', 'BCHAN', OOAINT, DIM, BCHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT ('Input', 'ECHAN', OOAINT, DIM, ECHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       IF numbers
      CALL UVDFND (UVDATA, 2, 'IF  ', IFINDX, IRET)
      IRET = 0
      IF (IFINDX.GT.0) THEN
         CALL OGET ('Input', 'BIF', TYPE, DIM, BIF, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OGET ('Input', 'EIF', TYPE, DIM, EIF, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         BIF = MAX (1, BIF)
         BIF = MIN (BIF, NAXIS(IFINDX))
         IF (EIF.LT.BIF) EIF = NAXIS(IFINDX)
         EIF = MIN (EIF, NAXIS(IFINDX))
      ELSE
         BIF = 1
         EIF = 1
         END IF
C                                       Save in Inputs for history
      DIM(1) = 1
      CALL OPUT ('Input', 'BIF', OOAINT, DIM, BIF, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT ('Input', 'EIF', OOAINT, DIM, EIF, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL SECPUT (UVDATA, 'BIF', OOAINT, DIM, BIF, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL SECPUT (UVDATA, 'EIF', OOAINT, DIM, EIF, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Line or continuum
      CALL OGET ('Input', 'CHINC', TYPE, DIM, CHINC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'NCHAV', TYPE, DIM, NCHAV, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CHINC = MAX (1, CHINC)
      NCHAV = MAX (1, NCHAV)
      IF ((NCHAV.LE.ECHAN-BCHAN) .AND. ((ECHAN-BCHAN)/CHINC.GT.0)) THEN
         CHTYPE = 'LINE'
         IF ((ECHAN-BCHAN)/CHINC+1.GT.46655) THEN
            MSGTXT = 'CAN ONLY DO 46655 CHANNELS AT A TIME IN LINE'
            CALL MSGWRT (8)
            IRET = 5
            GO TO 999
            END IF
      ELSE
         CHTYPE = 'SUM '
         NCHAV = ECHAN - BCHAN + 1
         NCHAV = MAX (1, NCHAV)
         CHINC = NCHAV
         END IF
C                                       Save value
      CALL OPUT ('Input', 'NCHAV', TYPE, DIM, NCHAV, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT ('Input', 'CHINC', TYPE, DIM, CHINC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVDATA, 'NCHAV', TYPE, DIM, NCHAV, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVDATA, 'CHINC', TYPE, DIM, CHINC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (UVWORK, 'CHINC', TYPE, DIM, CHINC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy UV descriptors to BEAM
      DIM(1) = LEN (CHTYPE)
      DIM(2) = 1
      DO 190 I = 1,BFIELD
         IF ((.NOT.ONEBEM) .OR. (MOD(I,NFIELD).EQ.1) .OR. (NFIELD.EQ.1))
     *      THEN
            CALL OPUT (BEAM(I), 'CHTYPE', OOACAR, DIM, DUMMY, CHTYPE,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            CALL U2IDES (UVDATA, BEAM(I), .FALSE., IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 190     CONTINUE
C                                       Copy UV descriptors to Images
      DO 200 I = 1,MFIELD
         CALL OPUT (CLEANO(I), 'CHTYPE', OOACAR, DIM, DUMMY, CHTYPE,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         CALL U2IDES (UVDATA, CLEANO(I), .FALSE., IRET)
         IF (IRET.NE.0) GO TO 999
 200     CONTINUE
C                                       SDI Clean allowed
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (CLEAN, 'SDIGAIN', OOARE, DIM, IMPARM(4), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       CC filter allowed
      FILTRS = IMPARM(9).LT.0.0
      IMPARM(9) = ABS (IMPARM(9))
      IF (IMPARM(9).LT.1.1) IMPARM(9) = 3.1
      DIM(1) = 2
      DIM(2) = 1
      CALL OPUT (CLEAN, 'CCFILTER', OOARE, DIM, IMPARM(8), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 1
      CALL OPUT (CLEAN, 'CCFILTRS', OOALOG, DIM, FILTRS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Scale residuals?
      DOSCAL = IMPARM(5).GT.0.0
      DIM(1) = 1
      CALL OPUT (CLEAN, 'SCALERES', OOALOG, DIM, DOSCAL, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      BMSSZ(1) = IROUND (IMPARM(6))
      BMSSZ(2) = IROUND (IMPARM(7))
      IF (BMSSZ(1).LE.0) BMSSZ(1) = 5
      IF (BMSSZ(2).LE.0) BMSSZ(2) = 5
      DIM(1) = 2
      CALL OPUT (CLEAN, 'BMSCLSZ', OOAINT, DIM, BMSSZ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       auto box into objects
      DIM(1) = 40
      DIM(2) = 1
      CALL OPUT ('Input', 'IM2PARM', OOARE, DIM, IM2PRM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 6
      CALL OPUT (CLEAN, 'AUTOBOX', OOARE, DIM, AUTOBX, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 3
      CALL OPUT (CLEANO(1), 'INVERTAP', OOARE, DIM, INVERT, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Guard band in uv grids.
      CALL OGET ('Input', 'CELLSIZE', TYPE, DIM, CELLS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       UV Cellsize for smallest image
      DU = RAD2AS / (MINNX * ABS (CELLS(1)))
      DV = RAD2AS / (MINNY * CELLS(2))
C                                       Set limits.
      UMAX = (MINNX/2-1.0) * DU
      VMAX = (MINNY/2-1.0) * DV
C                                       Add maximum of user specified
C                                       guardband or 7 cells.
      CALL OGET ('Input', 'GUARD', TYPE, DIM, GUARDB, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
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
      CALL OUVPUT (UVDATA, 'GUARDBND', TYPE, DIM, GUARDB, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OUVPUT (UVDATA, 'GUARDEF', TYPE, DIM, UTFACT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OUVPUT(UVDATA, 'CELLSIZE', TYPE, DIM, CELLS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      UMAX = UMAX - GUAU
      VMAX = VMAX - GUAV
      DIM(1) = 1
      DIM(2) = 1
      CALL OUVPUT (UVDATA, 'UMAX', OOARE, DIM, UMAX, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OUVPUT (UVDATA, 'VMAX', OOARE, DIM, VMAX, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Put in TELL adverb list
      TELOBJ = 'IMAGR Tell object'
      DIM(1) = LEN (TELOBJ)
      DIM(2) = 1
      CALL OPUT (CLEAN, 'TELLNAME', OOACAR, DIM, DUMMY, TELOBJ, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 8
      DIM(2) = NKEYT
      DIM(3) = 1
      CALL OPUT (CLEAN, 'TELADVRB', OOACAR, DIM, DUMMY, TELK, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 2
      CALL OPUT (CLEAN, 'TELLDIMS', OOAINT, DIM, TELDIM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = NKEYT
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (CLEAN, 'TELLTYPE', OOAINT, DIM, TELTYP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BOXFILE option for weights
      I = NWEGHT * (EIF-BIF+1)
      CALL RFILL (I, 1.0, WEGHTS)
      IF (BOXFIL.NE.'  ') THEN
         CALL WTBOXF (BOXFIL, BIF, EIF, NWEGHT, WEGHTS, IRET)
         IF (IRET.GT.0) THEN
            MSGTXT = 'FAILED READING BOXFILE FOR WEIGHTS'
            CALL MSGWRT (8)
            END IF
         END IF
C                                       Boxfile option for Stars
      NSTAR = 0
      IF (BOXFIL(1:1).NE.' ') THEN
         CALL STBOXF (BOXFIL, NSTAR, STPOS, STPARM, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'FAILED READING BOXFILE FOR STAR POSITIONS'
            CALL MSGWRT (8)
            END IF
         END IF
      IF (NSTAR.GT.0) THEN
         DIM(1) = 1
         DIM(2) = 1
         CALL OPUT (CLEAN, 'NSTARS', OOAINT, DIM, NSTAR, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         DIM(1) = 2
         DIM(2) = NSTAR
         CALL OPUT (CLEAN, 'STARPOSN', OOADP, DIM, STPOS, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         DIM(1) = 4
         CALL OPUT (CLEAN, 'STARPARM', OOARE, DIM, STPARM, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         WRITE (MSGTXT,1200) NSTAR
         CALL MSGWRT (2)
         END IF
C                                       Copy BOXFILE to OBOXFILE
      IF (IRET.EQ.0) CALL CPBOXF (BOXFIL, OBXFIL, IRET)
C                                       spectral index image
      IF (IMPARM(17).GT.0.0) THEN
         CALL OGET ('Input', 'IN3NAME', TYPE, DIM, DUMMY, TINAME, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OGET ('Input', 'IN3CLASS', TYPE, DIM, DUMMY, TCLASS, IRET)
         IF (IRET.NE.0) GO TO 999
         IF ((TINAME.NE.' ') .AND. (TCLASS.NE.' ')) THEN
            SPIX = 'Spectral index image'
            CALL CREATE (SPIX, 'IMAGE', IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
            CALL IN2OBJ ('Input', NKEY6, INK6, OUTK6, SPIX, IRET)
            IF (IRET.NE.0) GO TO 999
            DIM(1) = 1
            DIM(2) = 1
            IMPARM(17) = IMPARM(17) - 0.5
            CALL OPUT (SPIX, 'SPIXRADIUS', OOARE, DIM, IMPARM(17),
     *         CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Open SPIX to be sure it's OK.
            CALL OOPEN (SPIX, 'READ', IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OCLOSE (SPIX, IRET)
            IF (IRET.NE.0) GO TO 999
            DIM(1) = LEN (SPIX)
            DIM(2) = 1
            DIM(3) = 1
            CALL OPUT (UVDATA, 'SPIXIMAGE', OOACAR, DIM, DUMMY, SPIX,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OPUT (UVWORK, 'SPIXIMAGE', OOACAR, DIM, DUMMY, SPIX,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
C                                       curvature?
            CALL OGET ('Input', 'IN4NAME', TYPE, DIM, DUMMY, TINAME,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OGET ('Input', 'IN4CLASS', TYPE, DIM, DUMMY, TCLASS,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            IF ((TINAME.NE.' ') .AND. (TCLASS.NE.' ')) THEN
               SPIXC = 'Spectral index curvature image'
               CALL CREATE (SPIXC, 'IMAGE', IRET)
               IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
               CALL IN2OBJ ('Input', NKEY6, INK6B, OUTK6, SPIXC, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       Open SPIXC to be sure it's OK.
               CALL OOPEN (SPIXC, 'READ', IRET)
               IF (IRET.NE.0) GO TO 999
               CALL OCLOSE (SPIXC, IRET)
               IF (IRET.NE.0) GO TO 999
               DIM(1) = LEN (SPIXC)
               DIM(2) = 1
               DIM(3) = 1
               CALL OPUT (UVDATA, 'SPIXCURV', OOACAR, DIM, DUMMY, SPIXC,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
               CALL OPUT (UVWORK, 'SPIXCURV', OOACAR, DIM, DUMMY, SPIXC,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLEAN field number ',I4.4)
 1002 FORMAT ('BEAM field number ',I4.4)
 1010 FORMAT ('Number scales reset to',I2,' dropping duplicates')
 1040 FORMAT (A1,A2,I3.3)
 1041 FORMAT (A1,A1,I4.4)
 1050 FORMAT ('OBOXfile',I4,2I2.2,'.',3I2.2)
 1051 FORMAT ('Using OBOXFILE = ''',A,'''')
 1052 FORMAT ('HOME:OBOXtemp',I4,2I2.2,'.',3I2.2)
 1053 FORMAT ('Using temporary OBOXFILE = ''',A,'''')
 1200 FORMAT ('Will plot',I4,' star positions')
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
      SUBROUTINE IMLEAN (CLEAN, UVDATA, NSUBA, SUBA, IERR)
C-----------------------------------------------------------------------
C   Does CLEAN, looping over channels
C   Inputs:
C      CLEAN   C*?   Name of CLEAN process object
C      UVDATA  C*?   Name of UV data input object
C      NSUBA   I     size of SUBA
C      SUBA    I(*)  User's requested subarray(s)
C   Output:
C      IERR    I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER CLEAN*(*), UVDATA*(*)
      INTEGER   NSUBA, SUBA(*), IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ICHAN, BCHAN, ECHAN, CHINC, CHANN, TYPE, DIM(7), NCHAV,
     *   I, NFIELD, JCHAN, DUMMY, LCHAN, NIF, BIF, EIF, NCHAVG, LECHAN,
     *   NUMCH, JERR, UVWSEQ, BCOMP(MAXFLD), ALLOK, NVIS, J, JJ, II,
     *   NGOOD, UVWDSK, UVWCNO, BLIMIC, IROUND, MMM, CHINCG, NCLNO,
     *   TCAT(256), SCRTCH(256)
      LOGICAL   DOPBFM, DOWARN, DOMAX, KEEP, LDUMMY, EXIST, WASBAD
      REAL      IMPARM(20), SPINDX, FSCAL, PBFSIZ, AVGTIM(2), PANGL,
     *   ZANGL, CCFILT(2), WTMUL(MAXCIF), IM2PRM(40), BLIMIT(3)
      DOUBLE PRECISION PBFREQ(MAXCIF), UVFREQ, AVFREQ
      CHARACTER UVWORK*32, CLEANO(MAXFLD)*32, CHTYPE*4, FTTYPE*4,
     *   CDUMMY*1, UVWNAM*12, UVWCLS*6, INDATA*32, NAMCTY*20
      HOLLERITH TCATH(256)
      EQUIVALENCE (TCAT, TCATH)
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'IMAGRW.INC'
      INCLUDE 'IMAGWIN.INC'
C-----------------------------------------------------------------------
      AVFREQ = 0.0D0
C                                       Uv work object
      CALL OGET (CLEAN, 'UVDATA', TYPE, DIM, DUMMY, UVWORK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       work file init name parms
      CALL OGET (UVDATA, 'OUTNAME', TYPE, DIM, DUMMY, UVWNAM, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (UVDATA, 'OUTCLASS', TYPE, DIM, DUMMY, UVWCLS, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (UVDATA, 'OUTSEQ', TYPE, DIM, UVWSEQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      KEEP = (UVWNAM.NE.' ') .OR. (UVWCLS.NE.' ') .OR. (UVWSEQ.NE.0)
      IF (UVWSEQ.GT.0) THEN
         IF (UVWCLS.EQ.' ') THEN
            UVWCLS = TSKNAM
            DIM(1) = 6
            DIM(2) = 1
            CALL OPUT (UVWORK, 'CLASS', OOACAR, DIM, DUMMY, UVWCLS,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         IF (UVWNAM.EQ.' ') THEN
            CALL OGET (UVDATA, 'NAME', TYPE, DIM, DUMMY, UVWNAM, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL OPUT (UVWORK, 'NAME', TYPE, DIM, DUMMY, UVWNAM, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         CALL OGET (UVWORK, 'DISK', TYPE, DIM, UVWDSK, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C
      AVGTIM(1) = -999.
      AVGTIM(2) = -999.
      NGOOD = 0
C                                       Control information.
C                                       3D imaging # beams
      CALL OGET (CLEAN, 'IMPARM', TYPE, DIM, IMPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      PBFSIZ = IMPARM(1)
      DOPBFM = PBFSIZ.GT.0.0
      SPINDX = IMPARM(2)
      FSCAL = IMPARM(3)
C                                       CLEAN image object
      CALL OGET (CLEAN, 'CLEANI', TYPE, DIM, DUMMY, CLEANO, IERR)
      IF (IERR.NE.0) GO TO 999
      NCLNO = DIM(2)
C                                       Line or continuum
      CALL OGET (CLEANO(1), 'CHTYPE', TYPE, DIM, DUMMY, CHTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Find number of channels
      CALL OGET (CLEAN, 'BCHAN', TYPE, DIM, BCHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (CLEAN, 'ECHAN', TYPE, DIM, ECHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      BCHAN = MAX (1, BCHAN)
      ECHAN = MAX (BCHAN, ECHAN)
      CALL OGET ('Input', 'CHINC', TYPE, DIM, CHINC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CHINC = MAX (1, CHINC)
      CALL OGET ('Input', 'CHANNEL', TYPE, DIM, CHANN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CHANN = MAX (BCHAN, CHANN)
      CALL OGET ('Input', 'NCHAV', TYPE, DIM, NCHAV, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      NCHAV = MAX (1, NCHAV)
      NUMCH = (ECHAN - BCHAN + 1 - NCHAV) / CHINC + 1
      LECHAN = BCHAN + CHINC*NUMCH - 1
      CALL OGET ('Input', 'BIF', TYPE, DIM, BIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'EIF', TYPE, DIM, EIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      NIF = MAX (1, EIF - BIF + 1)
      IF ((CHTYPE.NE.'LINE') .AND. (NCHAV.GT.CHINC)) CHINC = NCHAV
      CALL OPUT (UVDATA, 'CALEDIT.BCHAN', OOAINT, DIM, CHANN, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      JCHAN = CHANN + (ECHAN - BCHAN)
      JCHAN = MIN (ECHAN, CHANN+NCHAV-1)
      CALL OPUT (UVDATA, 'CALEDIT.ECHAN', OOAINT, DIM, JCHAN, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       baseline - averaging
      CALL OGET ('Input', 'IM2PARM', TYPE, DIM, IM2PRM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (IM2PRM(12).GT.0.0) THEN
         BLIMIT(1) = IM2PRM(11)
         IF (BLIMIT(1).LE.1.0) BLIMIT(1) = 1.E9
         BLIMIT(1) = BLIMIT(1) / (24.0 * 3600.0)
         BLIMIT(2) = 268.5 / IM2PRM(12)
         BLIMIT(3) = IROUND (IM2PRM(13))
C                                       program to find
         IF (ABS(BLIMIT(3)-1.0).LT.0.1) THEN
            CALL FINDAV (UVDATA, NSUBA, SUBA, BLIMIT, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL OGET (UVDATA, 'CHINC', TYPE, DIM, CHINC, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         BLIMIT(3) = MAX (1.0, BLIMIT(3))
      ELSE
         BLIMIT(1) = 1000.0
         BLIMIT(2) = -1.0
         BLIMIT(3) = 1.0
         END IF
      DIM(1) = 1
      DIM(2) = 1
      BLIMIC = BLIMIT(3) + 0.1
      CALL OPUT (UVDATA, 'AVERAGEF', OOAINT, DIM, BLIMIC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      NCHAVG = NCHAV * NIF / BLIMIC
C                                       Force create of UVWORK
      CALL OGET (CLEAN, 'ALLOKAY', TYPE, DIM, ALLOK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       does UV work file exist
      IF (UVWSEQ.GT.0) THEN
         CALL OBFEXS (UVWORK, EXIST, IERR)
C                                       seq 0 always new
      ELSE
         EXIST = .FALSE.
         END IF
C                                       it is expected to
      IF (ALLOK.GE.2) THEN
         IF (IERR.NE.0) GO TO 990
         IF (.NOT.EXIST) ALLOK = 1
C                                       we do not care
      ELSE IF (EXIST) THEN
         IF (UVWDSK.LE.0) THEN
            CALL FNDSKC (UVWORK, UVWDSK, UVWCNO, IERR)
            IF (IERR.NE.0) GO TO 999
            DIM(1) = 1
            DIM(2) = 1
            CALL OPUT (UVWORK, 'DISK', OOAINT, DIM, UVWDSK, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
         ELSE IF (UVWCNO.LE.0) THEN
            CALL FNDSKC (UVWORK, UVWDSK, UVWCNO, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
C                                       find out more
         CALL CATIO ('READ', UVWDSK, UVWCNO, TCAT, 'REST', SCRTCH, IERR)
         IF ((IERR.GT.0) .AND. (IERR.LT.4)) GO TO 999
         CALL FNAGET (UVWORK, 'NAMCLSTY', TYPE, DIM, DUMMY, NAMCTY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         WASBAD = (NAMCTY(19:20).NE.'UV')
         WASBAD = WASBAD .OR. (TCAT(KINAX).NE.3) .OR.
     *      (TCAT(KINAX+1).NE.1)
         CALL H2CHR (8, 1, TCATH(KHCTP), NAMCTY)
         WASBAD = WASBAD .OR. (NAMCTY(:8).NE.'COMPLEX')
         CALL H2CHR (8, 1, TCATH(KHCTP+2), NAMCTY)
         WASBAD = WASBAD .OR. (NAMCTY(:8).NE.'STOKES')
         CALL H2CHR (8, 1, TCATH(KHCTP+4), NAMCTY)
         WASBAD = WASBAD .OR. (NAMCTY(:8).NE.'FREQ')
         IF (WASBAD) THEN
            IERR = 10
            MSGTXT = 'IN2NAME ET AL DO NOT SPECIFY VALID WORK FILE'
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       okay to delete
         MSGTXT = 'Deleting old work file'
         CALL MSGWRT (2)
         CALL OUVZAP (UVWORK, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
      IF (ALLOK.GE.1) THEN
         CALL OBFEXS (CLEANO(1), EXIST, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (.NOT.EXIST) ALLOK = 0
         END IF
      IF (ALLOK.LT.2) THEN
         CALL OCLONE (UVDATA, UVWORK, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       UVWORK, beams already done
      CALL OGET (CLEAN, 'CCFILTER', TYPE, DIM, CCFILT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (CCFILT(1).EQ.0.0) THEN
         INDATA = ' '
      ELSE
         INDATA = 'Input non-residual UV data'
         END IF
      DOWARN = .TRUE.
C                                       averaging is done in UVWORK
      MMM = 1
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (UVWORK, 'AVERAGEF', OOAINT, DIM, MMM, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      CHINCG = MAX (1, CHINC / BLIMIC)
      CALL OPUT (UVWORK, 'CHINC', OOAINT, DIM, CHINCG, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       How many fields?
      CALL OGET (CLEAN, 'NIMAGES', TYPE, DIM, NFIELD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (CLEAN, 'BCOMP', TYPE, DIM, BCOMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Loop over channel
      IF (CHANN+CHINC.GT.LECHAN) REWIN = .FALSE.
      DO 600 ICHAN = CHANN,LECHAN,CHINC
         DOMAX = ICHAN+CHINC.GT.LECHAN
C                                       reset Clean windows?
         IF (REWIN) THEN
            DIM(1) = NFIELD
            DIM(2) = 1
            CALL OPUT (CLEAN, 'NBOXES', OOAINT, DIM, NBOXES, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            DIM(1) = 4 * NFIELD
            DIM(2) = WINW / DIM(1)
            CALL OPUT (CLEAN, 'WINDOW', OOAINT, DIM, WIN(1+WINP),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
C                                       Copy uv data to work object, set
C                                       channel.
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL OPUT (UVDATA, 'CALEDIT.BCHAN', OOAINT, DIM, ICHAN, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         JCHAN = ICHAN + NCHAV - 1
         CALL OPUT (UVDATA, 'CALEDIT.ECHAN', OOAINT, DIM, JCHAN, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Move weights
         II = 0
         DO 20 I = BIF,EIF
            DO 10 J = ICHAN,JCHAN
               JJ = J + (I-BIF) * NWEGHT
               II = II + 1
               WTMUL(II) = WEGHTS(JJ)
 10            CONTINUE
 20         CONTINUE
C                                       Progress message
         WRITE (MSGTXT,1000) ICHAN, JCHAN, NIF
         CALL MSGWRT (4)
C                                       Select/correct
         LDUMMY = ALLOK.GE.2
         CALL IMACPY (UVDATA, UVWORK, CLEANO, NFIELD, SPINDX, FSCAL,
     *      BLIMIT, DOWARN, NSUBA, SUBA, WTMUL, AVGTIM, AVFREQ, LDUMMY,
     *      IERR)
         IF (IERR.EQ.-1) THEN
            WASBAD = DOMAX
            JCHAN = (ICHAN - CHANN) / CHINC + 1
            CALL IMABLK (JCHAN, NFIELD, CLEANO, IERR)
            IERR = 0
            GO TO 600
            END IF
         IF (IERR.NE.0) GO TO 990
         IF ((AVFREQ.GT.0.0D0) .AND. (CHTYPE.NE.'LINE')) THEN
            DIM(1) = 1
            DO 21 I = 1,NFIELD
               CALL OPUT (CLEANO(I), 'AVERFREQ', OOADP, DIM, AVFREQ,
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990
 21            CONTINUE
            END IF
         NGOOD = NGOOD + 1
         DOWARN = .FALSE.
         DIM(1) = 1
         DIM(2) = 1
         CALL OPUT (CLEANO(1), 'NCHAV', OOAINT, DIM, NCHAVG, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         IF ((ALLOK.GE.2) .AND. (.NOT.LDUMMY)) ALLOK = 0
         CALL OPUT (CLEAN, 'ALLOKAY', OOAINT, DIM, ALLOK, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Unsubtracted copy
         IF ((ALLOK.LE.1) .AND. (INDATA.NE.' ')) INDATA = '-'
         DIM(1) = LEN (INDATA)
         DIM(2) = 1
         CALL OPUT (CLEAN, 'INDATA', OOACAR, DIM, DUMMY, INDATA, IERR)
         IF (IERR.NE.0) GO TO 999
         IF ((INDATA.NE.' ') .AND. (INDATA.NE.'-')) THEN
            CALL CREATE (INDATA, 'UVDATA', IERR)
            IF (IERR.NE.0) GO TO 990
            IF (ALLOK.GE.2) THEN
               MSGTXT = 'Copy of initial data for filtering:'
               CALL MSGWRT (2)
               LDUMMY = .FALSE.
               NVIS = 0
               CALL OUVSCR (INDATA, UVDATA, NVIS, LDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990
               IF (AVGTIM(1).EQ.0.0) AVGTIM(1) = -999.
               LDUMMY = .FALSE.
               CALL IMACPY (UVDATA, INDATA, CLEANO, NFIELD, SPINDX,
     *            FSCAL, BLIMIT, DOWARN, NSUBA, SUBA, WTMUL, AVGTIM,
     *            AVFREQ, LDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990
C                                       Copy of work file
            ELSE
               CALL UVRSCR (UVWORK, INDATA, .FALSE., IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            END IF
C                                       Special options
C                                       Primary beam corrections
C                                       Freq array
         CALL UVFRQS (UVDATA, UVFREQ, PBFREQ, IERR)
         IF (IERR.NE.0) GO TO 990
         DIM(1) = 1
         DIM(2) = 1
         CALL OPUT (UVWORK, 'DOPBFM', OOALOG, DIM, DOPBFM, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OPUT (UVWORK, 'PBFSIZ', OOARE, DIM, PBFSIZ, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         DIM(1) = NCHAVG
         CALL OPUT (UVWORK, 'PBFREQ', OOADP, DIM, PBFREQ, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       DFT or FFT
         FTTYPE = 'FFT'
         DIM(1) = LEN (FTTYPE)
         CALL OPUT (CLEANO(1), 'FTTYPE', OOACAR, DIM, DUMMY, FTTYPE,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Uniform weighting
C                                       Set channel in CLEAN
         DIM(1) = 1
         LCHAN = (ICHAN - BCHAN) / CHINC + 1
         CALL OPUT (CLEAN, 'CHANNEL', OOAINT, DIM, LCHAN, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OPUT (CLEAN, 'UVCHAN', OOAINT, DIM, 1, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OPUT (CLEAN, 'NCHAV', OOAINT, DIM, NCHAVG, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Set CC version, (line only)
         IF (CHTYPE.EQ.'LINE') THEN
            CALL OPUT (CLEAN, 'VERSION', OOAINT, DIM, LCHAN, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         DIM(1) = MAXFLD
         CALL OPUT (CLEAN, 'BCOMP', OOAINT, DIM, BCOMP, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Uniform weight/beam mod
         IF (ALLOK.LT.2) THEN
            CALL OUNFWT (UVWORK, CLEANO(1), IERR)
         ELSE IF ((INDATA.NE.' ') .AND. (INDATA.NE.'-')) THEN
            MSGTXT = 'Weight initial data for filtering:'
            CALL MSGWRT (2)
            CALL OUNFWT (INDATA, CLEANO(1), IERR)
            END IF
         IF (IERR.NE.0) GO TO 990
C                                       Open CLEAN
         IF (ICHAN.EQ.BCHAN) THEN
            CALL OOPEN (CLEAN, 'DEST', IERR)
         ELSE
            CALL OOPEN (CLEAN, 'WRIT', IERR)
            END IF
         IF (IERR.NE.0) GO TO 990
C                                       Clean
         CALL CLNUV (CLEAN, DOMAX, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       next not ok for sure
         ALLOK = 0
         DIM(1) = 1
         DIM(2) = 1
         CALL OPUT (CLEAN, 'ALLOKAY', OOAINT, DIM, ALLOK, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Close CLEAN
         CALL OCLOSE (CLEAN, IERR)
         IF (IERR.NE.0) GO TO 990
 600     CONTINUE
C                                       Any good?
      IF (NGOOD.LE.0) THEN
         MSGTXT = 'NO DATA FOUND'
         IERR = 7
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       ow many fields?
      CALL OGET (CLEAN, 'NIMAGES', TYPE, DIM, NFIELD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       max/min if last bad
      IF (WASBAD) CALL IMAMXN (NFIELD, CLEANO, IERR)
C                                       Observing geometry
C                                       Get average time
      IF (AVGTIM(1).LT.0.0) THEN
         MSGTXT = 'Finding the parallactic and zenith angles'
         CALL MSGWRT (2)
         CALL IMTAV (UVDATA, AVGTIM, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Get parallactic, zenith angles
      IF ((AVGTIM(1).GT.0.0) .AND. (AVGTIM(2).LT.0.01)) THEN
         CALL IMPARG (UVDATA, AVGTIM(1), PANGL, ZANGL, JERR)
         IF (JERR.NE.0) GO TO 990
C                                       Save observing geometry on CLEAN
C                                       images
         DO 700 I = 1,NFIELD
            DIM(1) = 1
            DIM(2) = 1
            CALL OPUT (CLEANO(I), 'PARANGLE', OOARE, DIM, PANGL, CDUMMY,
     *         JERR)
            IF (JERR.NE.0) GO TO 990
            CALL OPUT (CLEANO(I), 'ZENANGLE', OOARE, DIM, ZANGL, CDUMMY,
     *         JERR)
            IF (JERR.NE.0) GO TO 990
 700        CONTINUE
         END IF
C                                       dispose of UVWORK
 990  IF (KEEP) THEN
         CALL OUVDES (UVWORK, JERR)
      ELSE
         MSGTXT = 'Deleting UV work file:'
         CALL MSGWRT (6)
         CALL OUVZAP (UVWORK, JERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Beginning channel',I5,' through',I5,' with',I3,' IFs')
      END
      SUBROUTINE IMACPY (UVIN, UVOUT, CLEANO, NFIELD, SPINDX, FSCAL,
     *   BLIMIT, DOWARN, NSUBA, SUBA, WTMUL, AVGTIM, AVFREQ, ALLOK,
     *   IERR)
C-----------------------------------------------------------------------
C   Copies one uv data object to another with application of any
C   calibration editing, and selection criteria.
C   Data are scaled in frequency to remove a  spectral index of SPINDX
C   and corrected to the reference frequency.  Also, u, v and w are
C   scaled by FSCAL.
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.  All specified
C                   selection, editing and calibration are applied.
C      UVOUT   C*?  Name of output uvdata object.  May be UVIN.
C      CLEANO  C*?  Name of Clean object - get #fields, do3d, shifts
C      NFIELD  I    Number of fields
C      DOWARN  L    Do warning messages about data selection?
C      WTMUL   R(*) Weight multipliers
C   Inputs attached to UVIN
C      UMAX    R    Maximum acceptable U in wavelengths (default all)
C      VMAX    R    Maximum acceptable V in wavelengths (default all)
C   In/out:
C      AVGTIM  R(2) Average time: computed if input(1) < 0, (2) rms
C      ALLOK   L    Output already written (in: user, out: checked)
C   Output:
C      IERR    I    Error code: 0 => ok, -1 => no data this channel
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*), CLEANO(*)*(*)
      REAL      SPINDX, BLIMIT(3), FSCAL, WTMUL(*), AVGTIM(2)
      INTEGER   NFIELD, NSUBA, SUBA(*), IERR
      LOGICAL   DOWARN, ALLOK
      DOUBLE PRECISION AVFREQ
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   COUNT, TYPE, DIM(7), NCOR, NAXIS(7), NSTOKS, I, J,
     *   K, INDEX, JLOCS, INDXU, INDXV, INDXW, MSGSAV, DUMMY, TCOUNT,
     *   JLOCD, LST, LERR, TACNT, JLOCT, NANT, NBAS, IANT(50), IBAS(50),
     *   INDXB, IA1, IA2, NRPARM, NDATA, NWORDS, MAXAN, ACOUNT, LSUB,
     *   MAXA, BLIMIC, INDXA1, INDXA2, INDXSA
      CHARACTER SORD*2, CDUMMY*1, CNAME*8, KEYW*8, STOKES*4, TABLE*32,
     *   UVTYPE*2, ANTAB*32
      REAL      RP(50), VS(3,MAXCIF), FS(MAXCIF), FST, UMAX, VMAX,
     *   AUMAX, BUMAX, AVMAX, BVMAX, GUARDB(2), UTFACT(2), UU, VV,
     *   CELLS(2), ROTATE, CROTAU(7), CROT, SROT, ZEROSP(5),
     *   RASH(MAXFLD), DECSH(MAXFLD), XANT(50), XBASE(50), BAVG(2),
     *   CRPIX(7), CDELT(7), CLIMIT(2)
      DOUBLE PRECISION UVFREQ, DSUM, AVGFRQ, FRMULT, GSTIA0, DEGPDY,
     *   SUMR, SUMI, TIME, GST0, ROTRAT, SUMII, SUMRR, SUMC, SUMN, SUMT
      LOGICAL   EXIST, DOSCLU, DOROT, DOAVG, IS3D, DESEL, REQBAS, FIRST
      LONGINT   PBAVG
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      LSUB = SUBA(1)
      DOAVG = AVGTIM(1).LT.0.0
      BLIMIC = BLIMIT(3) + 0.1
      CLIMIT(1) = BLIMIT(1)
      CLIMIT(2) = BLIMIT(2)
C                                       Get time information from AN
C                                       table.
      IF (DOAVG) THEN
         TABLE = 'Temporary AN table for IMACPY'
         CALL UV2TAB (UVIN, TABLE, 'AN', 1, IERR)
         IF (IERR.EQ.0) CALL TABOPN (TABLE, 'READ', IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'FAILS to get AN table data for parallactic' //
     *         ' and zenith angles'
            CALL MSGWRT (6)
            AVGTIM(1) = 0.
            AVGTIM(2) = 999.
            DOAVG = .FALSE.
            CALL TABDES (TABLE, IERR)
         ELSE
            CALL TABGET (TABLE, 'KEY.GSTIA0', TYPE, DIM, GSTIA0, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 995
            CALL TABGET (TABLE, 'KEY.DEGPDY', TYPE, DIM, DEGPDY, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 995
            CALL TABCLO (TABLE, IERR)
            IF (IERR.NE.0) GO TO 995
C                                       Delete object
            CALL TABDES (TABLE, IERR)
            IF (IERR.NE.0) GO TO 995
C                                       Convert units to radians
            GST0 = GSTIA0 * DG2RAD
            ROTRAT = DEGPDY * DG2RAD
            END IF
         END IF
C                                       open core memory
      CALL APOBJ ('OPEN', 'IMACPY', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check data type
      CALL UVDGET (UVIN, 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (UVTYPE(:1).NE.'U') THEN
         MSGTXT = 'I DO NOT WORK FOR UV DATA OF TYPE ''' // UVTYPE //
     *      ''''
         IERR = 8
         GO TO 980
         END IF
C                                       Open input.
C      CALL OUVOPN (UVIN, 'READ', IERR)
C      IF (IERR.NE.0) GO TO 990
C                                       Create output if necessary
      CALL OBFEXS (UVOUT, EXIST, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (.NOT.EXIST) THEN
         ALLOK = .FALSE.
         CALL OUVCLN (UVIN, UVOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       U,V limits
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OUVGET (UVIN, 'UMAX', TYPE, DIM, UMAX, CDUMMY, IERR)
      MSGSUP = MSGSAV
C                                       Default = all
      IF (IERR.EQ.1) THEN
         UMAX = 1.0E30
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 980
      MSGSUP = 32000
      CALL OUVGET (UVIN, 'VMAX', TYPE, DIM, VMAX, CDUMMY, IERR)
      MSGSUP = MSGSAV
C                                       Default = all
      IF (IERR.EQ.1) THEN
         VMAX = 1.0E30
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 980
C                                       Stokes
      MSGSUP = 32000
      CALL OUVGET (UVIN, 'STOKES', TYPE, DIM, DUMMY, STOKES, IERR)
C                                       Default = none
      IF (IERR.EQ.1) THEN
         STOKES = 'I'
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 980
      LST = 1
      IF (STOKES(1:1).EQ.'Q') LST = 2
      IF (STOKES(1:1).EQ.'U') LST = 3
      IF (STOKES(1:1).EQ.'V') LST = 4
C                                       Zero spacing flux
      MSGSUP = 32000
      CALL OUVGET (UVIN, 'ZEROSP', TYPE, DIM, ZEROSP, CDUMMY, IERR)
      MSGSUP = MSGSAV
C                                       Default = none
      IF (IERR.EQ.1) THEN
         CALL RFILL (5, 0.0, ZEROSP)
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 980
C                                       Actual guardband
      MSGSUP = 32000
      CALL OUVGET (UVIN, 'GUARDBND', TYPE, DIM, GUARDB, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         GUARDB(1) = 0.0
         GUARDB(2) = 0.0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 980
C                                       Default guardband
      MSGSUP = 32000
      CALL OUVGET (UVIN, 'GUARDEF', TYPE, DIM, UTFACT, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         UTFACT(1) = 0.0
         UTFACT(2) = 0.0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 980
C                                       Intended cellsize
      MSGSUP = 32000
      CALL OUVGET (UVIN, 'CELLSIZE', TYPE, DIM, CELLS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CELLS(1) = 1.0
         CELLS(2) = 1.0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 980
C                                       Additional rotation
      MSGSUP = 32000
      CALL OUVGET (UVIN, 'ROTATE', TYPE, DIM, ROTATE, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         ROTATE = 0.0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 980
      DOROT = ROTATE.NE.0.0
C                                       3D Images?
      CALL OGET (CLEANO, 'DO3DIMAG', TYPE, DIM, IS3D, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (CLEANO, 'RASHIFT', TYPE, DIM, RASH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (CLEANO, 'DECSHIFT', TYPE, DIM, DECSH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMC3DI (IS3D, ROTATE, NFIELD, RASH, DECSH, UMATS)
C                                       NCORR
      CALL UVDGET (UVIN, 'NCORR', TYPE, DIM, NCOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CROTA
      IF (DOROT) THEN
         CALL UVDGET (UVIN, 'CROTA', TYPE, DIM, CROTAU, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL UVDFND (UVIN, 2, 'DEC', JLOCD, IERR)
         IF (IERR.NE.0) GO TO 990
         CROTAU(JLOCD) = CROTAU(JLOCD) + ROTATE
         ROTATE = -ROTATE / 57.29578
         CROT = COS (ROTATE)
         SROT = SIN (ROTATE)
         END IF
C                                       Frequency info
      DOSCLU = FSCAL.GT.0.0
C                                       Uv data pointers
C                                       Which random parameter
      IF (DOAVG) THEN
         CALL UVDFND (UVIN, 1, 'TIME', JLOCT, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'IMACPY:TROUBLE FINDING RANDOM PARAMETER TIME'
            CALL MSGWRT (7)
            GO TO 990
            END IF
         SUMR = 0.0D0
         SUMI = 0.0D0
         SUMRR = 0.0D0
         SUMII = 0.0D0
         TACNT = 0
         END IF
      SUMC = 0.0D0
      SUMN = 0.0D0
      SUMT = 0.0D0
      CALL UVDFND (UVIN, 2, 'STOKES', JLOCS, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDFND (UVIN, 1, 'BASELINE', INDXB, IERR)
      IF (IERR.NE.0) THEN
         INDXB = -1
         CALL UVDFND (UVIN, 1, 'ANTENNA1', INDXA1, IERR)
         IF (IERR.EQ.0) CALL UVDFND (UVIN, 1, 'ANTENNA2', INDXA2, IERR)
         IF (IERR.EQ.0) CALL UVDFND (UVIN, 1, 'SUBARRAY', INDXSA, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER BASELINE'
            GO TO 980
            END IF
         END IF
      CALL UVDGET (UVIN, 'NRPARM', TYPE, DIM, NRPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      NDATA = NRPARM + 3 * (NCOR / BLIMIC)
      CALL UVDFND (UVIN, 1, 'UU-L', INDXU, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER UU-L'
         GO TO 980
         END IF
      CALL UVDFND (UVIN, 1, 'VV-L', INDXV, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER VV-L'
         GO TO 980
         END IF
      CALL UVDFND (UVIN, 1, 'WW-L', INDXW, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING RANDOM PARAMETER WW-L'
         GO TO 980
         END IF
C                                       UV axis descriptor
C                                       NAXIS
      CALL UVDGET (UVIN, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Number of polarizations
      NSTOKS = NAXIS(JLOCS)
C                                       antenna/baseline selection
      CALL OGET (UVIN, 'ANTENNAS', TYPE, DIM, XANT, CDUMMY, IERR)
      IF (IERR.NE.0) THEN
         CALL RFILL (50, 0.0, XANT)
         IERR = 0
         END IF
      CALL OGET (UVIN, 'BASELINE', TYPE, DIM, XBASE, CDUMMY, IERR)
      IF (IERR.NE.0) THEN
         CALL RFILL (50, 0.0, XBASE)
         IERR = 0
         END IF
C                                       Initialize baseline selection.
      CALL SETANT (50, XANT, XBASE, NANT, NBAS, IANT, IBAS, DESEL)
C                                       Freq array - no average
      UVFREQ = -1000.0D0
      CALL UVFRQS (UVIN, UVFREQ, FREQG, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Average frequency
      COUNT = 0
      DSUM = 0.0D0
      K = 0
      FRMULT = 0.0D0
      DO 30 I = 1,NCOR,NSTOKS
         K = K + 1
         DSUM = DSUM + FREQG(K)
         FRMULT = MAX (FRMULT, FREQG(K))
         COUNT = COUNT + 1
 30      CONTINUE
      FRMULT = FRMULT / UVFREQ
      AVGFRQ = DSUM / COUNT
C                                       Frequency scaling: Stokes is
C                                       always before IF and Freq.
      INDEX = 0
      K = 0
      DO 50 I = 1,NCOR,NSTOKS
         K = K + 1
         FST = (AVGFRQ / FREQG(K)) ** SPINDX
         DO 40 J = 1,NSTOKS
            INDEX = INDEX + 1
            FS(INDEX) = FST
 40         CONTINUE
 50      CONTINUE
C                                       Declare 'MAXBLINE' a header
C                                       keyword for the UVDATA class.
      CNAME = 'UVDATA'
      KEYW = 'MAXBLINE'
      CALL OBVHKW (CNAME, KEYW, OOARE, IERR)
      IF (IERR.NE.0) GO TO 990
      KEYW = 'MAXABSU '
      CALL OBVHKW (CNAME, KEYW, OOARE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Initialize visibility count
      COUNT = 0
      TCOUNT = 0
      ACOUNT = 0
      AUMAX = 0.0
      AVMAX = 0.0
      BVMAX = 0.0
      UMAX = UMAX / FRMULT
      VMAX = VMAX / FRMULT
C                                       need MAXAN, memory for averaging
      IF ((CLIMIT(2).GT.0.0) .AND. (.NOT.ALLOK)) THEN
C                                       scale for freq, square
         CLIMIT(2) = (CLIMIT(2) / FRMULT) ** 2
         MAXAN = 0
         DO 60 LSUB = 1,NSUBA
            ANTAB = 'AN table for UVTAVG'
            CALL UV2TAB (UVIN, ANTAB, 'AN', SUBA(LSUB), IERR)
            IF (IERR.NE.0) GO TO 990
C                                       How many antennas?
            CALL ANTNO (ANTAB, SUBA(LSUB), MAXA, IERR)
            IF (IERR.NE.0) GO TO 990
            MAXAN = MAX (MAXAN, MAXA)
            CALL OUVDES (ANTAB, IERR)
            IF (IERR.NE.0) GO TO 990
 60         CONTINUE
         NWORDS = (((MAXAN * (MAXAN-1)) / 2) * NDATA - 1) / 1024 + 2
         CALL ZMEMRY ('GET ', 'IMACPY', NWORDS, BAVG, PBAVG, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'FAILED TO GET MEMORY FOR BASELINE-BASED AVERAGE'
            CALL MSGWRT (7)
            GO TO 990
            END IF
         CALL IMBAVG ('INIT', NRPARM, NDATA, MAXAN, UVOUT, IA1, IA2,
     *      RP, VS, BLIMIC, CLIMIT, BAVG(1+PBAVG), COUNT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Check output in ALLOK
      IF ((ALLOK) .OR. (SUBA(1).GT.1)) THEN
         MSGSUP = 32000
         CALL OUVOPN (UVOUT, 'READ', IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.0) THEN
            CALL UVDGET (UVOUT, 'GCOUNT', TYPE, DIM, COUNT,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            IF (COUNT.LE.0) ALLOK = .FALSE.
C                                       Get current max u
            MSGSUP = 32000
            CALL OUVGET (UVOUT, 'MAXBLINE', TYPE, DIM, BUMAX,
     *         CDUMMY, IERR)
            MSGSUP = MSGSAV
            IF (IERR.EQ.1) THEN
               BUMAX = 0.0
               IERR = 0
               END IF
            IF (IERR.NE.0) GO TO 990
         ELSE
            ALLOK = .FALSE.
            END IF
         MSGSUP = 32000
         CALL OUVCLO (UVOUT, IERR)
         MSGSUP = MSGSAV
         END IF
      IF ((.NOT.ALLOK) .AND. (SUBA(1).LE.1)) BUMAX = 0.0
      DOAVG = DOAVG .AND. (.NOT.ALLOK)
      IF ((AVGTIM(1).LT.0.0) .AND. (.NOT.DOAVG)) THEN
         AVGTIM(1) = 0.0
         AVGTIM(2) = 0.0
         END IF
C                                       loop over subarrays
      IF (.NOT.ALLOK) THEN
         FIRST = .TRUE.
C                                       ??????????????
         DO 200 LSUB = 1,NSUBA
C
            DIM(1) = 1
            DIM(2) = 1
            CALL SECPUT (UVIN, 'SUBARR', OOAINT, DIM, SUBA(LSUB),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
C                                       Open input.
            CALL OUVOPN (UVIN, 'READ', IERR)
            IF (IERR.NE.0) GO TO 990
C                                       Open output
            IF (FIRST) THEN
               CALL OUVOPN (UVOUT, 'WRIT', IERR)
               IF (IERR.NE.0) GO TO 990
               IF (BLIMIC.GT.1) THEN
                  CALL UVDGET (UVOUT, 'CRPIX', TYPE, DIM, CRPIX, CDUMMY,
     *               IERR)
                  IF (IERR.NE.0) GO TO 990
                  CALL UVDGET (UVOUT, 'CDELT', TYPE, DIM, CDELT, CDUMMY,
     *               IERR)
                  IF (IERR.NE.0) GO TO 990
                  END IF
C                                       Copy UVDESC
               CALL UVDSCP (UVIN, UVOUT, IERR)
               IF (IERR.NE.0) GO TO 990
               IF (BLIMIC.GT.1) THEN
                  CALL UVDPUT (UVOUT, 'CRPIX', TYPE, DIM, CRPIX, CDUMMY,
     *               IERR)
                  IF (IERR.NE.0) GO TO 990
                  CALL UVDPUT (UVOUT, 'CDELT', TYPE, DIM, CDELT, CDUMMY,
     *               IERR)
                  IF (IERR.NE.0) GO TO 990
                  END IF
               FIRST = .FALSE.
               END IF
C                                       Loop thru data
 100        CALL UVREAD (UVIN, RP, VS, IERR)
            IF (IERR.GT.0) GO TO 990
            IF (IERR.EQ.0) THEN
C                                       Want this one?
               IF (INDXB.GT.0) THEN
                  IA2 = RP(INDXB) + 0.001
                  IA1 = IA2 / 256
                  IA2 = MOD (IA2, 256)
               ELSE
                  IA1 = RP(INDXA1) + 0.1
                  IA2 = RP(INDXA2) + 0.1
                  END IF
               IF (.NOT.REQBAS (IA1, IA2, DESEL, IANT, NANT, IBAS,
     *            NBAS)) GO TO 100
C                                       adjust u,v,w
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
C                                       Scale u, v, w
                  IF (DOSCLU) THEN
                     RP(INDXU) = RP(INDXU) * FSCAL
                     RP(INDXV) = RP(INDXV) * FSCAL
                     RP(INDXW) = RP(INDXW) * FSCAL
                     END IF
C                                       Scale vis
                  SUMT = SUMT + 1.0D0
                  DO 110 I = 1,NCOR
                     IF (VS(3,I).GT.0.0) THEN
                        VS(1,I) = VS(1,I) * FS(I)
                        VS(2,I) = VS(2,I) * FS(I)
                        VS(3,I) = VS(3,I) * WTMUL(I)
                        K = (I-1)/NSTOKS + 1
                        SUMC = SUMC + FREQG(K)
                        SUMN = SUMN + 1.0D0
                        END IF
 110                 CONTINUE
                  COUNT = COUNT + 1
                  IF (CLIMIT(2).LE.0.0) THEN
                     CALL UVWRIT (UVOUT, RP, VS, IERR)
                  ELSE
                     CALL IMBAVG ('ADD', NRPARM, NDATA, MAXAN, UVOUT,
     *                  IA1, IA2, RP, VS, BLIMIC, CLIMIT, BAVG(1+PBAVG),
     *                  ACOUNT, IERR)
                     END IF
                  IF (IERR.GT.0) GO TO 990
C                                       average the time
                  IF (DOAVG) THEN
C                                       Convert time to GST(radians)
                     TIME = RP(JLOCT) * ROTRAT + GST0
C                                       Sum as sine and cosine
                     TACNT = TACNT + 1
                     SUMR = SUMR + COS (TIME)
                     SUMI = SUMI + SIN (TIME)
                     SUMRR = SUMRR + (COS (TIME)) ** 2
                     SUMII = SUMII + (SIN (TIME)) ** 2
                     END IF
                  END IF
               GO TO 100
               END IF
            IERR = 0
            IF (LSUB.LT.NSUBA) CALL OUVCLO (UVIN, IERR)
 200        CONTINUE
C                                       finish/free averaging memory
         IF (CLIMIT(2).GT.0.0) THEN
            CALL IMBAVG ('CLOS', NRPARM, NDATA, MAXAN, UVOUT, IA1, IA2,
     *         RP, VS, BLIMIC, CLIMIT, BAVG(1+PBAVG), ACOUNT, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL ZMEMRY ('FREE', 'IMACPY', NWORDS, BAVG, PBAVG, IERR)
            END IF
C                                       Better be some data
         IF (COUNT.LE.0) THEN
            CALL OUVCLO (UVIN, IERR)
            CALL OUVCLO (UVOUT, IERR)
            IERR = -1
            MSGTXT = 'IMACPY: NO DATA SELECTED'
            GO TO 980
            END IF
C                                       add the zero spacing
         IF ((ZEROSP(1).GT.0.0) .AND. (ZEROSP(5).GT.0.0)) THEN
            RP(INDXU) = 0.
            RP(INDXV) = 0.
            RP(INDXW) = 0.
            DO 210 I = 1,NCOR
               VS(1,I) = ZEROSP(LST)
               VS(2,I) = 0.0
               VS(3,I) = ZEROSP(5)
 210           CONTINUE
            TCOUNT = TCOUNT + 1
            COUNT = COUNT + 1
            IF (CLIMIT(2).GT.0.0) ACOUNT = ACOUNT + 1
            CALL UVWRIT (UVOUT, RP, VS, IERR)
            IF (IERR.GT.0) GO TO 990
            WRITE (MSGTXT,1210) ZEROSP(LST), ZEROSP(5), STOKES
            CALL MSGWRT (4)
            END IF
C                                       Set amount of output data
         DIM(1) = 1
         DIM(2) = 1
         CALL OUVPUT (UVOUT, 'MAXBLINE', OOARE, DIM, BUMAX, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUVPUT (UVOUT, 'MAXABSU ', OOARE, DIM, BUMAX, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
C                                       rotation
         IF (DOROT) THEN
            ROTATE = 0.0
            CALL OUVPUT (UVOUT, 'ROTATE', OOARE, DIM, ROTATE, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            DIM(1) = 7
            CALL UVDPUT (UVOUT, 'CROTA', OOARE, DIM, CROTAU, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
C                                       actual average frequency
         IF (SUMN.GT.0.0D0) THEN
            AVFREQ = SUMC / SUMN
            IF (SUMN.LT.SUMT*NCOR) THEN
               WRITE (MSGTXT,1215) SUMN, SUMT*NCOR
               CALL MSGWRT (3)
               WRITE (MSGTXT,1216) AVFREQ / 1.D6
               CALL MSGWRT (3)
               END IF
            END IF
C                                       average time
         IF (DOAVG) THEN
C                                       Average as cosine and sine to
C                                       deal with periodic nature of GST
            IF (TACNT.GT.0) THEN
               SUMI = SUMI / TACNT
               SUMR = SUMR / TACNT
               AVGTIM(1) = ATAN2 (SUMI, SUMR+1.0D-20)
               SUMR = SUMR * SUMR
               SUMI = SUMI * SUMI
               SUMRR = SUMRR / TACNT - SUMR
               SUMII = SUMII / TACNT - SUMI
               AVGTIM(2) = (SUMI*SUMRR + SUMR*SUMII) / (SUMR + SUMI)
            ELSE
               MSGTXT = 'IMACPY: NO VISIBILITIES'
               IERR = 5
               GO TO 980
               END IF
C                                       Convert time back to solar
            AVGTIM(1) = (AVGTIM(1) - GST0)
            IF (AVGTIM(1).LE.0.0) AVGTIM(1) = AVGTIM(1) + TWOPI
            IF (AVGTIM(1).LE.0.0) AVGTIM(1) = AVGTIM(1) + TWOPI
            IF (AVGTIM(1).LE.0.0) AVGTIM(1) = AVGTIM(1) + TWOPI
            IF (AVGTIM(1).LE.0.0) AVGTIM(1) = AVGTIM(1) + TWOPI
            AVGTIM(1) = AVGTIM(1) / ROTRAT
            AVGTIM(2) = SQRT (AVGTIM(2)) / ROTRAT
            END IF
C                                       Warnings ?
         IF (DOWARN) THEN
            IF (TCOUNT.GT.COUNT) THEN
               WRITE (MSGTXT,1220) COUNT, TCOUNT
               CALL MSGWRT (6)
               END IF
            IF (ACOUNT.GT.0) THEN
               WRITE (MSGTXT,1221) COUNT, ACOUNT
               CALL MSGWRT (3)
            ELSE
               WRITE (MSGTXT,1222) COUNT
               CALL MSGWRT (3)
               END IF
C                                       data outside grid?
            UMAX = UMAX * FRMULT
            VMAX = VMAX * FRMULT
            AUMAX = AUMAX * FRMULT
            AVMAX = AVMAX * FRMULT
            BVMAX = BVMAX * FRMULT
            IF ((AUMAX.GT.UMAX) .OR. (AVMAX.GT.VMAX)) THEN
               MSGTXT = '**** WARNING data discarded outside usable'
     *            // ' part of UV plane ****'
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
     *         (BVMAX.GT.VMAX*(1.-1.2*UTFACT(2))/(1.0-GUARDB(2)))) THEN
               MSGTXT = '**** WARNING data included out of inner'
     *            // ' portion of UV plane ****'
               CALL MSGWRT (6)
               MSGTXT = '**** Watch for high-frequency & other poor ' //
     *            'cleaning effects ****'
               CALL MSGWRT (6)
               END IF
            END IF
C                                       Set amount of output data
         DIM(1) = 1
         DIM(2) = 1
         CALL UVDPUT (UVOUT, 'GCOUNT', OOAINT, DIM, COUNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Sort order the same as input if
C                                       subarray 1, else undefined
         CALL UVDGET (UVIN, 'SORTORD', TYPE, DIM, DUMMY, SORD, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (NSUBA.GT.1) SORD = '  '
         IF ((DOROT) .AND. (SORD.NE.'TB') .AND. (SORD.NE.'BT'))
     *      SORD = '??'
         CALL UVDPUT (UVOUT, 'SORTORD', TYPE, DIM, DUMMY, SORD, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Copy relevant tables
         MSGSUP = 31999
         CALL UVDTCO (UVIN, UVOUT, IERR)
         MSGSUP = MSGSAV
         IF (IERR.NE.0) GO TO 990
C                                       Close files, update disk
         CALL OUVCLO (UVOUT, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Mark output as valid
         DIM(1) = 1
         CALL FSTPUT (UVOUT, 'VALID', OOALOG, DIM, .TRUE., CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL APOBJ ('CLOSE', 'IMACPY', LERR)
C                                       Close input
         DIM(1) = 1
         DIM(2) = 1
         I = MAX (1, SUBA(1))
         CALL SECPUT (UVIN, 'SUBARR', OOAINT, DIM, I, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUVCLO (UVIN, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      GO TO 999
C                                       Error
 980  CALL MSGWRT (7)
 990  CALL APOBJ ('CLOSE', 'IMACPY', LERR)
 995  MSGTXT = 'IMACPY: ERROR COPYING ' // UVIN
      CALL MSGWRT (7)
      MSGTXT = 'TO ' // UVOUT
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1210 FORMAT ('IMACPY: append zero spacing',F10.4,' weight',F7.1,
     *   ' Stokes ',A4)
 1215 FORMAT ('IMACPY: found',F13.0,' of',F13.0,' possible channels')
 1216 FORMAT ('IMACPY: average frequency is then',F11.3,' MHz')
 1220 FORMAT ('IMACPY: Only',I9,' samples of',I9,' fell on UV grid')
 1221 FORMAT ('IMACPY: Averaging',I9,' samples makes',I9,
     *   ' to be imaged')
 1222 FORMAT ('IMACPY: Copied',I9,' visibilities to be imaged')
 1230 FORMAT ('**** Actual ',A,'max',1PE11.4,' exceeds limit',1PE11.4,
     *   8X,'****')
 1231 FORMAT ('**** Use cellsize <',F10.5,' not',F10.5,
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
      SUBROUTINE WTBOXF (BOXFIL, BIF, EIF, NWEGHT, WEGHTS, IRET)
C-----------------------------------------------------------------------
C   WTBOXF reads the BOXFILE for W cards to set weights
C   Input:
C      BOXFIL   C*(*)   Box file name
C      BIF      I       First IF included
C      EIF      I       Last IF included
C      NWEGHT   I       Number of spectral channels
C   Output:
C      WEGHTS   R(*)    Weight multipliers (NWEGHT, BIF:EIF)
C      IRET     I       Error code
C-----------------------------------------------------------------------
      CHARACTER BOXFIL*(*)
      INTEGER   BIF, EIF, NWEGHT, IRET
      REAL      WEGHTS(*)
C
      INTEGER   LUN, I, J, FIND, IPARM(2), KBP, I1, I2, J1, J2, K
      REAL      WT
      CHARACTER LINE*132
      DOUBLE PRECISION X
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                        Open clean box file
      LUN = 11
      CALL ZTXOPN ('QRED', LUN, FIND, BOXFIL, .FALSE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN'
         CALL MSGWRT (6)
         GO TO 999
         END IF
      DO 50 K = 1,100000
         CALL ZTXIO ('READ', LUN, FIND, LINE, IRET)
         IF (IRET.EQ.2) GO TO 60
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ'
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                       A weight line
         CALL CHTRIM (LINE, 132, LINE, J)
         IF ((LINE(:1).EQ.'W') .OR. (LINE(:1).EQ.'w'))  THEN
            KBP = 2
            DO 10 J = 1,3
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) THEN
                  IF (J.EQ.1) GO TO 50
                  WRITE (MSGTXT,1020) K, J
                  CALL MSGWRT (6)
                  IRET = 1
                  GO TO 999
               ELSE IF (J.EQ.1) THEN
                  WT = X
               ELSE
                  IF (X.GE.0.0D0) THEN
                     IPARM(J-1) = X + 0.50D0
                  ELSE
                     IPARM(J-1) = X - 0.50D0
                     END IF
                  END IF
 10            CONTINUE
            IF (IPARM(1).LE.NWEGHT) THEN
               IF (IPARM(1).LE.0) THEN
                  J1 = 1
                  J2 = NWEGHT
               ELSE
                  J1 = IPARM(1)
                  J2 = J1
                  END IF
               IF (IPARM(2).EQ.0) THEN
                  I1 = BIF
                  I2 = EIF
               ELSE
                  I1 = IPARM(2)
                  I2 = I1
                  END IF
               DO 30 I = I1,I2
                  IF ((I.GE.BIF) .AND. (I.LE.EIF)) THEN
                     DO 20 J = J1,J2
                        WEGHTS(J+(I-BIF)*NWEGHT) = WT
 20                     CONTINUE
                     END IF
 30               CONTINUE
               END IF
            END IF
 50      CONTINUE
C
 60   CALL ZTXCLS (LUN, FIND, IRET)
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WTBOXF: ERROR',I4,1X,A,'ING THE CLEAN BOXES TEXT FILE')
 1020 FORMAT ('WTBOXF: PARSING ERROR ON LINE',I4,' FIELD',I2)
      END
      SUBROUTINE IGNORF (BOXFIL, MFIELD, IGNORE, IRET)
C-----------------------------------------------------------------------
C   IGNORF reads the BOXFILE for I cards to set fields to be ignored
C   Input:
C      BOXFIL   C*(*)   Box file name
C      MFIELD   I       maximum field number
C   Output:
C      IGNORE   R(*)    = -2.0 if field to be ignored
C      IRET     I       Error code
C-----------------------------------------------------------------------
      CHARACTER BOXFIL*(*)
      INTEGER   MFIELD, IRET
      REAL      IGNORE(*)
C
      INTEGER   LUN, I, J, FIND, IPARM(2), KBP, K
      CHARACTER LINE*132
      DOUBLE PRECISION X
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                        Open clean box file
      LUN = 11
      CALL ZTXOPN ('QRED', LUN, FIND, BOXFIL, .FALSE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN'
         CALL MSGWRT (6)
         GO TO 999
         END IF
      DO 50 K = 1,100000
         CALL ZTXIO ('READ', LUN, FIND, LINE, IRET)
         IF (IRET.EQ.2) GO TO 60
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ'
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                       A weight line
         CALL CHTRIM (LINE, 132, LINE, J)
         IF ((LINE(:1).EQ.'I') .OR. (LINE(:1).EQ.'i'))  THEN
            KBP = 2
            DO 10 J = 1,2
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) THEN
                  IPARM(J) = 0
               ELSE
                  IPARM(J) = X + 0.5D0
                  END IF
 10            CONTINUE
            IF ((IPARM(1).GT.0) .AND. (IPARM(1).LE.MFIELD)) THEN
               IF (IPARM(2).EQ.0) IPARM(2) = IPARM(1)
               IF (IPARM(2).GT.MFIELD) IPARM(2) = MFIELD
               DO 20 I = IPARM(1),IPARM(2)
                  IGNORE(I) = -2.0
 20               CONTINUE
               END IF
            END IF
 50      CONTINUE
C
 60   CALL ZTXCLS (LUN, FIND, IRET)
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IGNORF: ERROR',I4,1X,A,'ING THE CLEAN BOXES TEXT FILE')
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
      SUBROUTINE IMAHIS (CLEAN, UVDATA)
C-----------------------------------------------------------------------
C   Routine to write history file to output CLEAN image object.
C   Inputs:
C      CLEAN   C*?  CLEAN process object
C      UVDATA  C*?  Input UV data
C-----------------------------------------------------------------------
      CHARACTER CLEAN*(*), UVDATA*32
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLN.INC'
      INTEGER   NADV0, NADV1
      PARAMETER (NADV0=63, NADV1=14)
      CHARACTER CLEANO(MAXFLD)*32, BEAM(MAXFLD)*32, LIST(NADV0)*8,
     *   CDUMMY*1, CLIST(NADV1)*8, LINE*72, SPIX*32, CFLD*4, CBOX*5,
     *   OBXFIL*48
      INTEGER   IERR, TYPE, DIM(7), NFIELD, I, DUMMY, IFLD(2,MAXFLD),
     *   IMSIZE(2,MAXFLD), J, NCMP, MSGSAV, BFIELD, NUMRES, NADV, CP,
     *   BP
      LONGINT   KP
      REAL      RASH(MAXFLD), DECSH(MAXFLD), FFL, TFL, IMPARM(20),
     *   ROBUST, RESMAX
      LOGICAL   NOCLEN, DO3DIM, ONEBEM
      INCLUDE 'IMAGWIN.INC'
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'SOURCES', 'QUAL',
     *   'CALCODE', 'TIMERANG', 'SELBAND', 'SELFREQ', 'FREQID',
     *   'SUBARRAY', 'ANTENNAS', 'BASELINE', 'DOCALIB', 'GAINUSE',
     *   'DOPOL', 'PDVER', 'BLVER', 'FLAGVER', 'DOBAND', 'BPVER',
     *   'SMOOTH', 'STOKES', 'BCHAN', 'ECHAN', 'CHANNEL', 'NCHAV',
     *   'CHINC', 'BIF', 'EIF', 'OUTNAME','OUTSEQ', 'OUTVER',
     *   'CELLSIZE', 'NFIELD', 'UVTAPER', 'UVRANGE', 'GUARD', 'ROTATE',
     *   'ZEROSP','UVWTFN', 'UVSIZE', 'UVBOX', 'UVBXFN', 'XTYPE',
     *   'YTYPE', 'XPARM', 'YPARM', 'OVERLAP', 'NGAUSS', 'WGAUSS',
     *   'FGAUSS', 'FQTOL', 'BOXFILE', 'OBOXFILE', 'IN3NAME',
     *   'IN3CLASS', 'IN3SEQ', 'IN3DISK', 'IN4NAME', 'IN4CLASS',
     *   'IN4SEQ', 'IN4DISK'/
      DATA CLIST /'NITER', 'BCOMP', 'GAIN', 'FLUX', 'MINPATCH', 'BMAJ',
     *   'BMIN', 'BPA', 'PHAT', 'FACTOR', 'CMETHOD', 'IMAGRPRM',
     *   'IM2PARM', 'MAXPIXEL'/
C-----------------------------------------------------------------------
C                                       remove temp OBOXFILE ?
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

C                                       How many fields, all resolutions
      CALL OGET (CLEAN, 'NIMAGES', TYPE, DIM, NFIELD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET ('Input', 'NGAUSS', TYPE, DIM, NUMRES, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      NUMRES = MAX (1, NUMRES)
C                                       CLEAN names
      CALL OGET (CLEAN, 'CLEANI', TYPE, DIM, DUMMY, CLEANO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       3D imaging # beams
      CALL OGET (CLEAN, 'DO3DIMAG', TYPE, DIM, DO3DIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (CLEAN, 'ONEBEAM', TYPE, DIM, ONEBEM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      BFIELD = NFIELD
C                                       Get image-dep parms
      CALL OGET (CLEANO(1), 'IMSIZE', TYPE, DIM, IMSIZE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OGET ('Input', 'FLDSIZE', TYPE, DIM, IFLD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OGET (CLEANO(1), 'RASHIFT', TYPE, DIM, RASH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OGET (CLEANO(1), 'DECSHIFT', TYPE, DIM, DECSH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (.NOT.REWIN) THEN
         CALL OGET (CLEAN, 'NBOXES', TYPE, DIM, NBOXES, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL OGET (CLEAN, 'WINDOW', TYPE, DIM, WIN(1+WINP), CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
      MSGSUP = 32000
      CALL OGET (CLEAN, 'TFLUX', TYPE, DIM, TFL, CDUMMY, IERR)
      MSGSUP = MSGSAV
      NOCLEN = IERR.EQ.1
      IF (NOCLEN) IERR = 0
      IF (IERR.NE.0) GO TO 995
      CALL OGET (CLEAN, 'IMPARM', TYPE, DIM, IMPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       SPIX?
      MSGSUP = 32000
      CALL OGET (UVDATA, 'SPIXIMAGE', TYPE, DIM, DUMMY, SPIX, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         SPIX = ' '
         END IF
      NADV = NADV0
      IF (SPIX.EQ.' ') NADV = NADV - 8
C                                       Loop over cleans
      DO 100 I = 1,NFIELD
         WRITE (CFLD,2000) I
         CP = 1
         IF (I.LT.1000) CP = 2
         IF (I.LT.100) CP = 3
         IF (I.LT.10) CP = 4
C                                       Copy old history
         CALL OHCOPY (UVDATA, CLEANO(I), IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Copy base adverb values.
         CALL OHLIST ('Input', LIST, NADV, CLEANO(I), IERR)
         IF (IERR.NE.0) GO TO 990
C                                       ROBUST = 0 meaningful
         CALL OGET (CLEANO(I), 'ROBUST', TYPE, DIM, ROBUST, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
         WRITE (LINE,1010) ROBUST
         CALL OHWRIT (LINE, CLEANO(I), IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Copy cleaning adverbs
         CALL OHLIST ('Input', CLIST, NADV1, CLEANO(I), IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Field-dependent parms
         WRITE (LINE,1000) CFLD(CP:), IMSIZE(1,I), IMSIZE(2,I)
         CALL OHWRIT (LINE, CLEANO(I), IERR)
         IF (IERR.NE.0) GO TO 990
         WRITE (LINE,1001) CFLD(CP:), IFLD(1,I), IFLD(2,I)
         CALL OHWRIT (LINE, CLEANO(I), IERR)
         IF (IERR.NE.0) GO TO 990
         WRITE (LINE,1002) CFLD(CP:), RASH(I)
         CALL OHWRIT (LINE, CLEANO(I), IERR)
         IF (IERR.NE.0) GO TO 990
         WRITE (LINE,1003) CFLD(CP:), DECSH(I)
         CALL OHWRIT (LINE, CLEANO(I), IERR)
         IF (IERR.NE.0) GO TO 990
         IF (NBOXES(I).GT.0) THEN
            WRITE (LINE,1004) CFLD(CP:), NBOXES(I)
            CALL OHWRIT (LINE, CLEANO(I), IERR)
            IF (IERR.NE.0) GO TO 990
            BP = 1
            IF (NBOXES(I).LT.10000) BP = 2
            IF (NBOXES(I).LT.1000) BP = 3
            IF (NBOXES(I).LT.100) BP = 4
            IF (NBOXES(I).LT.10) BP = 5
            DO 20 J = 1,NBOXES(I)
               WRITE (CBOX,2001) J
               KP = ((J - 1) * NFIELD + I - 1) * 4 + WINP
               WRITE (LINE,1005) CBOX(BP:), CFLD(CP:), WIN(1+KP),
     *            WIN(2+KP), WIN(3+KP), WIN(4+KP)
               CALL OHWRIT (LINE, CLEANO(I), IERR)
               IF (IERR.NE.0) GO TO 990
 20            CONTINUE
            END IF
         IF (.NOT.NOCLEN) THEN
            MSGSUP = 32000
            CALL OGET (CLEANO(I), 'FIELDRES', TYPE, DIM, RESMAX, CDUMMY,
     *         IERR)
            MSGSUP = MSGSAV
            IF (IERR.EQ.1) THEN
               IERR = 0
               RESMAX = -1.0
               END IF
            IF (IERR.NE.0) GO TO 990
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
            WRITE (LINE,1007) TFL
            CALL OHWRIT (LINE, CLEANO(I), IERR)
            IF (IERR.NE.0) GO TO 990
            WRITE (LINE,1006) CFLD(CP:), NCMP
            CALL OHWRIT (LINE, CLEANO(I), IERR)
            IF (IERR.NE.0) GO TO 990
            WRITE (LINE,1008) CFLD(CP:), FFL
            CALL OHWRIT (LINE, CLEANO(I), IERR)
            IF (IERR.NE.0) GO TO 990
            WRITE (LINE,1009) CFLD(CP:), RESMAX
            CALL OHWRIT (LINE, CLEANO(I), IERR)
            IF (IERR.NE.0) GO TO 990
            IF (NUMRES.GT.1) THEN
               DO 30 J = 11,16
                  WRITE (LINE,1020) J, IMPARM(J)
                  IF (J.EQ.16) WRITE (LINE,1021) J, IMPARM(J)
                  CALL OHWRIT (LINE, CLEANO(I), IERR)
                  IF (IERR.NE.0) GO TO 990
 30               CONTINUE
               END IF
            IF (IMPARM(19).GT.0.0) THEN
               WRITE (LINE,1030) IMPARM(19)
               CALL OHWRIT (LINE, CLEANO(I), IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
            END IF
 100     CONTINUE
      CALL ZMEMRY ('FREE', 'IMAGR', WINW, WIN(1), WINP, IERR)
C                                       Beam
      CALL OGET (CLEAN, 'DIRTBEAM', TYPE, DIM, DUMMY, BEAM, IERR)
      IF (IERR.NE.0) GO TO 990
      DO 120 I = 1,BFIELD
         IF ((.NOT.ONEBEM) .OR. (MOD(I,NFIELD).EQ.1) .OR. (NFIELD.EQ.1))
     *      THEN
            CALL OGET (BEAM(I), 'IMSIZE', TYPE, DIM, IMSIZE, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
C                                       Old history
            CALL OHCOPY (UVDATA, BEAM(I), IERR)
            IF (IERR.NE.0) GO TO 980
C                                       copy base adverb values only
            CALL OHLIST ('Input', LIST, NADV0, BEAM(I), IERR)
            IF (IERR.NE.0) GO TO 980
C                                       ROBUST = 0 meaningful
            WRITE (LINE,1010) ROBUST
            CALL OHWRIT (LINE, BEAM(I), IERR)
            IF (IERR.NE.0) GO TO 980
C                                       Copy cleaning adverbs
            CALL OHLIST ('Input', CLIST, NADV1, BEAM(I), IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
 120     CONTINUE
      GO TO 999
C                                       Error
 980  MSGTXT = 'ERROR WRITING HISTORY FOR ' // BEAM(I)
      CALL MSGWRT (6)
      GO TO 999
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // CLEANO(I)
      CALL MSGWRT (6)
      GO TO 999
 995  MSGTXT = 'ERROR WRITING HISTORY FOR ' // CLEAN
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMSIZE(*,',A,')  = ',I5,' ,',I6)
 1001 FORMAT ('FLDSIZE(*,',A,') = ',I5,' ,',I6)
 1002 FORMAT ('RASHIFT(',A,')   = ',1PE12.5)
 1003 FORMAT ('DECSHIFT(',A,')  = ',1PE12.5)
 1004 FORMAT ('NBOXES(',A,')    = ',I2)
 1005 FORMAT ('WIN(*,',A,',',A,') =',3(I5,','),I5)
 1006 FORMAT ('NCOMP(',A,') =',I8)
 1007 FORMAT ('TFLUX = ',1PE12.5,9X,'/ Total cleaned flux')
 1008 FORMAT ('CFLUX(',A,') = ',1PE12.5,5X,'/ Cleaned flux in field')
 1009 FORMAT ('RESMAX(',A,') = ',1PE12.5,4X,
     *   '/ Peak residual in field windows')
 1010 FORMAT ('ROBUST =',F8.3,' / Weighting robustness parm')
 1020 FORMAT ('IMAGRPRM(',I2,') =',F8.5,' / MRClean control')
 1021 FORMAT ('IMAGRPRM(',I2,') =',F8.0,' / MRClean control')
 1030 FORMAT ('IMAGRPRM(19) =',F6.4,' / Dynamic range limit')
 2000 FORMAT (I4)
 2001 FORMAT (I5)
      END
      SUBROUTINE IMAMXN (MFIELD, CNAME, IRET)
C-----------------------------------------------------------------------
C   IMAMXN checks and sets the max min in the image header for the full
C   image.
C   Input
C      MFIELD     I         Number of fields present.
C      CNAME     C(*)*32   Names of associated clean (residual) images.
C   Output to common:
C   Output:
C      IRET       I    Return error code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   MFIELD, IRET
      CHARACTER CNAME(*)*(*)
C
      INTEGER   IFIELD, BLC(7), TRC(7), DIM(7), TYPE, LOOP7, LOOP6,
     *   LOOP5, LOOP4, LOOP3, LOOP2, LOOP1, LROW, NAXIS(7)
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:PMAD.INC'
      REAL      DATMAX, DATMIN, VALUE, ROW(MABFSS)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'Checking image max/min'
      CALL MSGWRT (3)
C                                       Loop over fields.
      DO 800 IFIELD = 1,MFIELD
C                                       Set window for residual/restored
         CALL ARDGET (CNAME(IFIELD), 'BLC', TYPE, DIM, BLC, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 995
         CALL ARDGET (CNAME(IFIELD), 'TRC', TYPE, DIM, TRC, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 995
         BLC(1) = 1
         BLC(2) = 1
         BLC(3) = 1
         TRC(1) = 0
         TRC(2) = 0
         TRC(3) = 0
         CALL ARDPUT (CNAME(IFIELD), 'BLC', OOAINT, DIM, BLC, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 995
         CALL ARDPUT (CNAME(IFIELD), 'TRC', OOAINT, DIM, TRC, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Open array and get I/O stream
         CALL ARROPN (CNAME(IFIELD), 'READ', IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Window
         CALL ARRWIN (CNAME(IFIELD), BLC, TRC, NAXIS, IRET)
         IF (IRET.NE.0) GO TO 995
         DATMAX = -1.0E25
         DATMIN = 1.0E25
C                                       Loop over array
         LROW = TRC(1) - BLC(1) + 1
         DO 700 LOOP7 = BLC(7),TRC(7)
            DO 600 LOOP6 = BLC(6),TRC(6)
               DO 500 LOOP5 = BLC(5),TRC(5)
                  DO 400 LOOP4 = BLC(4),TRC(4)
                     DO 300 LOOP3 = BLC(3),TRC(3)
                        DO 200 LOOP2 = BLC(2),TRC(2)
C                                       read row
         CALL ARREAD (CNAME(IFIELD), DIM, ROW, IRET)
         IF (IRET.GT.0) GO TO 995
C                                       Do statistics
         DO 120 LOOP1 = 1,LROW
            VALUE = ROW(LOOP1)
            IF (VALUE.NE.FBLANK) THEN
               DATMAX = MAX (DATMAX, VALUE)
               DATMIN = MIN (DATMIN, VALUE)
               END IF
 120        CONTINUE
 200                       CONTINUE
 300                    CONTINUE
 400                 CONTINUE
 500              CONTINUE
 600           CONTINUE
 700        CONTINUE
C                                       Close file
         CALL ARRCLO (CNAME(IFIELD), IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Save values in ARRAY_STAT
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL ARSPUT (CNAME(IFIELD), 'DATAMAX', OOARE, DIM, DATMAX,
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL ARSPUT (CNAME(IFIELD), 'DATAMIN', OOARE, DIM, DATMIN,
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
C                                       force max/min to disk
         CALL IMGOPN (CNAME(IFIELD), 'WRIT', IRET)
         IF (IRET.NE.0) GO TO 995
         CALL IMPUT (CNAME(IFIELD), 'DATAMAX', OOARE, DIM, DATMAX,
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL IMPUT (CNAME(IFIELD), 'DATAMIN', OOARE, DIM, DATMIN,
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL IMGCLO (CNAME(IFIELD), IRET)
         IF (IRET.NE.0) GO TO 995
 800     CONTINUE
      IRET = 0
      GO TO 999
C                                       Error
 995  MSGTXT = 'IMAMXN: ERROR DETERMING STAT. FOR ' // CNAME(IFIELD)
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE IMABLK (JCHAN, MFIELD, CNAME, IRET)
C-----------------------------------------------------------------------
C   IMABLK blanks a channel for all fields
C   Input
C      JCHAN    I         channel
C      MFIELD   I         Number of fields present.
C      CNAME    C(*)*32   Names of associated clean (residual) images.
C   Output to common:
C   Output:
C      IRET     I         Return error code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   JCHAN, MFIELD, IRET
      CHARACTER CNAME(*)*(*)
C
      INTEGER   IFIELD, BLC(7), TRC(7), DIM(7), TYPE, LOOP7, LOOP6,
     *   LOOP5, LOOP4, LOOP3, LOOP2, LROW, NAXIS(7)
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:PMAD.INC'
      REAL      ROW(MABFSS)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCRE.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      WRITE (MSGTXT,1000) JCHAN
      CALL MSGWRT (3)
C                                       Loop over fields.
      DO 800 IFIELD = 1,MFIELD
C                                       Set window for residual/restored
         CALL ARDGET (CNAME(IFIELD), 'BLC', TYPE, DIM, BLC, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 995
         CALL ARDGET (CNAME(IFIELD), 'TRC', TYPE, DIM, TRC, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 995
         BLC(1) = 1
         BLC(2) = 1
         BLC(3) = JCHAN
         TRC(1) = 0
         TRC(2) = 0
         TRC(3) = JCHAN
         CALL ARDPUT (CNAME(IFIELD), 'BLC', OOAINT, DIM, BLC, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 995
         CALL ARDPUT (CNAME(IFIELD), 'TRC', OOAINT, DIM, TRC, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Open array and get I/O stream
         CALL ARROPN (CNAME(IFIELD), 'WRIT', IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Window
         CALL ARRWIN (CNAME(IFIELD), BLC, TRC, NAXIS, IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Loop over array
         LROW = TRC(1) - BLC(1) + 1
         CALL RFILL (LROW, FBLANK, ROW)
         DIM(1) = LROW
         DIM(2) = 0
         DIM(3) = 0
         DO 700 LOOP7 = BLC(7),TRC(7)
            DO 600 LOOP6 = BLC(6),TRC(6)
               DO 500 LOOP5 = BLC(5),TRC(5)
                  DO 400 LOOP4 = BLC(4),TRC(4)
                     DO 300 LOOP3 = BLC(3),TRC(3)
                        DO 200 LOOP2 = BLC(2),TRC(2)
C                                       write row
                           CALL ARRWRI (CNAME(IFIELD), DIM, ROW, IRET)
                           IF (IRET.GT.0) GO TO 995
 200                       CONTINUE
 300                    CONTINUE
 400                 CONTINUE
 500              CONTINUE
 600           CONTINUE
 700        CONTINUE
C                                       Close file
         CALL ARRCLO (CNAME(IFIELD), IRET)
         IF (IRET.NE.0) GO TO 995
 800     CONTINUE
      IRET = 0
      GO TO 999
C                                       Error
 995  MSGTXT = 'IMABLK: ERROR BLANKING ' // CNAME(IFIELD)
      CALL MSGWRT (7)
C                                       if image does not exist
C                                       make sure filled with zeros
      QCREAT = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Blanking channel',I5,' image plane')
      END
      SUBROUTINE IMBAVG (OPER, NRPARM, NDATA, MAXAN, UVOUT, IA1, IA2,
     *   RP, VIS, BLIMIC, BLIMIT, BAVG, COUNT, IERR)
C-----------------------------------------------------------------------
C   IMBAVG averages data on a baseline-dependent time basis
C   Inputs:
C      OPER     C*4      Operation: INIT, ADD, CLOS
C      NRPARM   I        Number random parameters
C      NDATA    I        NRPARM + 3*(NCOR/BLIMIC)
C      MAXAN    I        Maximum antenna number
C      UVOUT    C*(*)    Output object
C      IA1      I        Lower antenna number
C      IA2      I        Higher antenna number
C      RP       R(*)     Random parameters
C      VIS      R(3,*)   Visibility data
C      BLIMIC   I        Number of channels to average together
C      BLIMIT   R(2)     Max time since start in days, Max BL squared
C                        separation allowed in average
C   In/Out:
C      BAVG     R(NDATA,*)   Summing/storage array
C      COUNT    I        Number records written
C   Output:
C      IERR     I        Error code
C-----------------------------------------------------------------------
      CHARACTER OPER*(*), UVOUT*(*)
      INTEGER   NRPARM, NDATA, MAXAN, IA1, IA2, BLIMIC, COUNT, IERR
      REAL      RP(*), VIS(3,*), BLIMIT(2), BAVG(NDATA,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NCOR, INDXU, INDXV, INDXW, INDXT, IBL, BCOUNT(MXBASE),
     *   NBL, I, J, JCOR
      REAL      U, V, R, UV(2,MXBASE), STIME(MXBASE)
      INCLUDE 'INCS:DMSG.INC'
      SAVE      NCOR, INDXU, INDXV, INDXW, INDXT, BCOUNT, UV, STIME,
     *   JCOR
C-----------------------------------------------------------------------
C                                       init
      IF (OPER.EQ.'INIT') THEN
         IBL = (MAXAN * (MAXAN - 1)) / 2
         CALL FILL (IBL, 0, BCOUNT)
         IBL = IBL * NDATA
         CALL RFILL (IBL, 0.0, BAVG)
         CALL UVDFND (UVOUT, 1, 'UU-L', INDXU, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'IMBAVG: TROUBLE FINDING RANDOM PARAMETER UU-L'
            GO TO 980
            END IF
         CALL UVDFND (UVOUT, 1, 'VV-L', INDXV, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'IMBAVG: TROUBLE FINDING RANDOM PARAMETER VV-L'
            GO TO 980
            END IF
         CALL UVDFND (UVOUT, 1, 'WW-L', INDXW, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'IMBAVG: TROUBLE FINDING RANDOM PARAMETER WW-L'
            GO TO 980
            END IF
         CALL UVDFND (UVOUT, 1, 'TIME', INDXT, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'IMBAVG: TROUBLE FINDING RANDOM PARAMETER TIME'
            GO TO 980
            END IF
         BLIMIC = MAX (1, BLIMIC)
         NCOR = ((NDATA - NRPARM) * BLIMIC) / 3
         JCOR = NCOR / BLIMIC
C                                       close
      ELSE IF (OPER.EQ.'CLOS') THEN
         NBL = (MAXAN * (MAXAN - 1)) / 2
         DO 100 IBL = 1,NBL
            IF (BCOUNT(IBL).GT.0) THEN
               CALL RCOPY (NRPARM, BAVG(1,IBL), RP)
               RP(INDXU) = RP(INDXU) / BCOUNT(IBL)
               RP(INDXV) = RP(INDXV) / BCOUNT(IBL)
               RP(INDXW) = RP(INDXW) / BCOUNT(IBL)
               RP(INDXT) = RP(INDXT) / BCOUNT(IBL)
               CALL RCOPY (3*JCOR, BAVG(NRPARM+1,IBL), VIS)
               DO 20 J = 1,JCOR
                  IF (VIS(3,J).GT.0.0) THEN
                     VIS(1,J) = VIS(1,J) / VIS(3,J)
                     VIS(2,J) = VIS(2,J) / VIS(3,J)
                     END IF
 20               CONTINUE
               COUNT = COUNT + 1
               CALL UVWRIT (UVOUT, RP, VIS, IERR)
               IF (IERR.NE.0) THEN
                  MSGTXT = 'IMBAVG: TROUBLE FINISHING OUTPUT'
                  GO TO 980
                  END IF
               END IF
 100        CONTINUE
C                                       add in
      ELSE
         IBL = IA1 * (MAXAN-1) - MAXAN - ((IA1 * (IA1-1)) / 2) + IA2
C                                       allowed to average in?
         IF (BCOUNT(IBL).GT.0) THEN
            U = UV(1,IBL) - RP(INDXU)
            V = UV(2,IBL) - RP(INDXV)
            R = U*U + V*V
C                                       no
            IF ((ABS(STIME(IBL)-RP(INDXT)).GT.BLIMIT(1)) .OR.
     *         (R.GT.BLIMIT(2))) THEN
               BAVG(INDXU,IBL) = BAVG(INDXU,IBL) / BCOUNT(IBL)
               BAVG(INDXV,IBL) = BAVG(INDXV,IBL) / BCOUNT(IBL)
               BAVG(INDXW,IBL) = BAVG(INDXW,IBL) / BCOUNT(IBL)
               BAVG(INDXT,IBL) = BAVG(INDXT,IBL) / BCOUNT(IBL)
               I = NRPARM
               COUNT = COUNT + 1
               DO 120 J = 1,JCOR
                  IF (BAVG(I+3,IBL).GT.0.0) THEN
                     BAVG(I+1,IBL) = BAVG(I+1,IBL) / BAVG(I+3,IBL)
                     BAVG(I+2,IBL) = BAVG(I+2,IBL) / BAVG(I+3,IBL)
                     END IF
                  I = I + 3
 120              CONTINUE
               CALL UVWRIT (UVOUT, BAVG(1,IBL), BAVG(NRPARM+1,IBL),
     *            IERR)
               IF (IERR.NE.0) THEN
                  MSGTXT = 'IMBAVG: TROUBLE WRITING INTERMEDIATE RECORD'
                  GO TO 980
                  END IF
               BCOUNT(IBL) = 0
               END IF
            END IF
C                                       insert new record
         IF (BCOUNT(IBL).LE.0) THEN
            BCOUNT(IBL) = 1
            STIME(IBL) = RP(INDXT)
            UV(1,IBL) = RP(INDXU)
            UV(2,IBL) = RP(INDXV)
            CALL RCOPY (NRPARM, RP, BAVG(1,IBL))
            I = NRPARM
            CALL RFILL (3*JCOR, 0.0, BAVG(I+1,IBL))
C                                       but average channels
            DO 140 J = 1,NCOR
               IF (VIS(3,J).GT.0) THEN
                  BAVG(I+1,IBL) = BAVG(I+1,IBL) + VIS(1,J) * VIS(3,J)
                  BAVG(I+2,IBL) = BAVG(I+2,IBL) + VIS(2,J) * VIS(3,J)
                  BAVG(I+3,IBL) = BAVG(I+3,IBL) + VIS(3,J)
                  END IF
               IF (MOD(J,BLIMIC).EQ.0) I = I + 3
 140           CONTINUE
C                                       average in
         ELSE
            BCOUNT(IBL) = BCOUNT(IBL) + 1
            BAVG(INDXU,IBL) = BAVG(INDXU,IBL) + RP(INDXU)
            BAVG(INDXV,IBL) = BAVG(INDXV,IBL) + RP(INDXV)
            BAVG(INDXW,IBL) = BAVG(INDXW,IBL) + RP(INDXW)
            BAVG(INDXT,IBL) = BAVG(INDXT,IBL) + RP(INDXT)
            I = NRPARM
            DO 160 J = 1,NCOR
               IF (VIS(3,J).GT.0.0) THEN
                  BAVG(I+1,IBL) = BAVG(I+1,IBL) + VIS(1,J) * VIS(3,J)
                  BAVG(I+2,IBL) = BAVG(I+2,IBL) + VIS(2,J) * VIS(3,J)
                  BAVG(I+3,IBL) = BAVG(I+3,IBL) + VIS(3,J)
                  END IF
               IF (MOD(J,BLIMIC).EQ.0) I = I + 3
 160           CONTINUE
            IERR = 0
            END IF
         END IF
C
      GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE FINDAV (UVDATA, NSUBA, SUBA, BLIMIT, IERR)
C-----------------------------------------------------------------------
C   FINDAV finds the number of channels that may be averaged
C   Inputs:
C      UVDATA   C*(*)   Data set object name
C      NSUBA    I       Number of SUBA
C      SUBA     I(*)    Subarrays to use
C      BLIMIT   R(3)    (1) ignored, (2) uv max change
C   Outputs:
C      BLIMIT   R(3)    (1,2) unchanged, (3) number of channels to avg
C      IERR     I       Error code
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*)
      REAL      BLIMIT(3)
      INTEGER   NSUBA, SUBA(*), IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LSUB, BCHAN, ECHAN, NCHAN, TYPE, DIM(7), BIF, EIF, NIF,
     *   I, II, J, ANROW, NUMORB, NOPCAL, NUMIF, ANFQID, IROW, NROW,
     *   NOSTA, MNTSTA
      CHARACTER ANTAB*32, CDUMMY, ANAME*8, TIMSYS*8, RDATE*8, XYZHAN*8,
     *   TFRAME*8, ANNAME*8, POLTYA*2, POLTYB*2
      DOUBLE PRECISION AX(MAXANT), AY(MAXANT), AZ(MAXANT), UVFREQ,
     *   FREQS(MAXCIF), FMAX, FNEXT, FINC, ARRAYC(3), GSTIA0, DEGPDY,
     *   SAFREQ, STAXYZ(3), ORBPRM(6), R, RMAX
      REAL      POLRXY(2), UT1UTC, DATUTC, STAXOF, DIAMAN,
     *   FWHMAN(MAXIF), POLAA, POLCA(MAXIF), POLAB, POLCB(MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
C                                       channels
      CALL OGET (UVDATA, 'BCHAN', TYPE, DIM, BCHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (UVDATA, 'ECHAN', TYPE, DIM, ECHAN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      NCHAN = ECHAN - BCHAN + 1
      CALL OGET (UVDATA, 'BIF', TYPE, DIM, BIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (UVDATA, 'EIF', TYPE, DIM, EIF, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      NIF = EIF - BIF + 1
C                                       frequencies
      UVFREQ = -1000.0D0
      CALL UVFRQS (UVDATA, UVFREQ, FREQS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       find highest and next highest
      FMAX = 0.0
      II = NCHAN * NIF
      DO 10 I = 1,II
         IF (FREQS(I).GT.FMAX) THEN
            FNEXT = FMAX
            FMAX = FREQS(I)
            END IF
 10      CONTINUE
      FINC = FMAX - FNEXT
C                                       get antennas 1 subarray at at
C                                       time
      RMAX = 0.0D0
      DO 100 LSUB = 1,NSUBA
         CALL UV2TAB (UVDATA, ANTAB, 'AN', SUBA(LSUB), IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OANINI (ANTAB, 'READ', ANROW, ARRAYC, GSTIA0, DEGPDY,
     *      SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME,
     *      XYZHAN, TFRAME, NUMORB, NOPCAL, NUMIF, ANFQID, IERR)
         CALL TABGET (ANTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         II = 0
         DO 20 IROW = 1,NROW
            CALL OTABAN (ANTAB, 'READ', ANROW, ANNAME, STAXYZ, ORBPRM,
     *         NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN, POLTYA, POLAA,
     *         POLCA, POLTYB, POLAB, POLCB, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((STAXYZ(1).NE.0.0D0) .OR. (STAXYZ(2).NE.0.0D0) .OR.
     *         (STAXYZ(3).NE.0.0D0)) THEN
               II = II + 1
               AX(II) = STAXYZ(1)
               AY(II) = STAXYZ(2)
               AZ(II) = STAXYZ(3)
               END IF
 20         CONTINUE
         CALL OTABAN (ANTAB, 'CLOS', ANROW, ANNAME, STAXYZ, ORBPRM,
     *      NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN, POLTYA, POLAA,
     *      POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) GO TO 990
         NROW = II
         DO 40 J = 1,NROW-1
            DO 30 I = J+1,NROW
               R = (AX(I)-AX(J))**2 + (AY(I)-AY(J))**2 +
     *            (AZ(I)-AZ(J))**2
               IF (R.GT.RMAX) RMAX = R
 30            CONTINUE
 40         CONTINUE
 100     CONTINUE
      RMAX = SQRT (RMAX)
C
      R = BLIMIT(2) * VELITE / (RMAX * FINC)
      WRITE (MSGTXT,1100) R
      CALL MSGWRT (4)
      J = R + 0.0001D0
      J = MIN (J, NCHAN)
      IF (J.GT.1) THEN
         IF ((NCHAN/J)*J.NE.NCHAN) THEN
            I = NCHAN/J
            II = ECHAN
            ECHAN = I * J + BCHAN - 1
            WRITE (MSGTXT,1105) II, ECHAN
            CALL MSGWRT (6)
            DIM(1) = 1
            DIM(2) = 1
            CALL OPUT (UVDATA, 'ECHAN', OOAINT, DIM, ECHAN, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
      BLIMIT(3) = J
      GO TO 999
C
 990  WRITE (MSGTXT,1990) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('FINDAV: recommends averaging every',F7.2,' channels')
 1105 FORMAT ('FINDAV: changing ECHAN from',I6,' to',I6)
 1990 FORMAT ('FINDAV: ERROR',I5,' FINDING MAXIMUM BASELINE =>',
     *   ' CHANNEL AVERAGING')
      END
      SUBROUTINE UWINDF (UNWIN, UNBOXS, NFIELD, IMSIZE, BOXFIL, IERR)
C-----------------------------------------------------------------------
C   Fills the UNWIN array with unclean box definitions taken from BOXFIL
C   Inputs:
C      BOXFIL   C*48        User provided file name containing box defs
C      NFIELD   I           Number of fields defined
C   In/Out:
C      UNWIN    I(4,*,*)    clean boxes - defaulted on in (4,FIELD,BOX)
C      UNBOXS   I*(*)       Array containing number of boxes/field
C   Outputs:
C      IERR     I           Error return code:
C                              0 => no error
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLN.INC'
      CHARACTER BOXFIL*48
      INTEGER   NFIELD, UNWIN(4,NFIELD,*), UNBOXS(*)
      INTEGER   IMSIZE(2,*), IERR
C
      INTEGER   LUN, I, J, ISUBF(MXNBOX), IFIELD, FIND, IPARM(5), KBP,
     *   I1, I2, IDD, LIMIT
      CHARACTER LINE*132
      DOUBLE PRECISION X
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      LIMIT = MIN (MXNBOX, (4*MAXFLD) / MAX(1,NFIELD))
C                                        Open clean box file
      LUN = 11
      CALL ZTXOPN ('READ', LUN, FIND, BOXFIL, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                        Enter box parameters from file
      CALL FILL (MXNBOX, 0, ISUBF)
      IDD = 0
      DO 50 I = 1,100000
         CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
         IF (IERR.EQ.2) GO TO 60
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN'
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                       check for comments
         CALL CHTRIM (LINE, 132, LINE, J)
         IF ((LINE(:1).NE.'u') .AND. (LINE(:1).NE.'U')) GO TO 50
C                                       parse for 5 integers
C                                       field, blc, trc
         KBP = 2
         CALL FILL (5, 0, IPARM)
         DO 30 J = 1,5
            CALL GETNUM (LINE, 132, KBP, X)
            IF (X.EQ.DBLANK) THEN
               IF (J.EQ.1) GO TO 50
               WRITE (MSGTXT,1020) I, J
               CALL MSGWRT (6)
               IERR = 1
               GO TO 999
            ELSE
               IF (X.GE.0.0D0) THEN
                  IPARM(J) = X + 0.50D0
               ELSE
                  IPARM(J) = X - 0.50D0
                  END IF
               END IF
 30         CONTINUE
         IFIELD = IPARM(1)
         IF ((IFIELD.GE.1) .AND. (IFIELD.LE.NFIELD)) THEN
            IDD = IDD + 1
            ISUBF(IFIELD) = MAX (ISUBF(IFIELD),0) + 1
            IF (ISUBF(IFIELD).LE.LIMIT) THEN
               J = ISUBF(IFIELD)
C                                       circular
               IF (IPARM(2).LT.0) THEN
                  UNWIN(1,IFIELD,J) = -1
                  UNWIN(3,IFIELD,J) = MAX (1+IPARM(3), MIN (IPARM(4),
     *               IMSIZE(1,IFIELD)-IPARM(3)))
                  UNWIN(4,IFIELD,J) = MAX (1+IPARM(3), MIN (IPARM(5),
     *               IMSIZE(2,IFIELD)-IPARM(3)))
                  UNWIN(2,IFIELD,J) = IPARM(3)
C                                       rectangular
               ELSE IF (IPARM(2).GT.0) THEN
                  UNWIN(1,IFIELD,J) = MAX (1, MIN (IPARM(2),
     *               IMSIZE(1,IFIELD)))
                  UNWIN(3,IFIELD,J) = MAX (1, MIN (IPARM(4),
     *               IMSIZE(1,IFIELD)))
                  IF (UNWIN(3,IFIELD,J).LT.UNWIN(1,IFIELD,J)) THEN
                     I1 = UNWIN(3,IFIELD,J)
                     UNWIN(3,IFIELD,J) = UNWIN(1,IFIELD,J)
                     UNWIN(1,IFIELD,J) = I1
                     END IF
                  UNWIN(2,IFIELD,J) = MAX (1, MIN (IPARM(3),
     *               IMSIZE(2,IFIELD)))
                  UNWIN(4,IFIELD,J) = MAX (1, MIN (IPARM(5),
     *               IMSIZE(2,IFIELD)))
                  IF (UNWIN(4,IFIELD,J).LT.UNWIN(2,IFIELD,J)) THEN
                     I1 = UNWIN(4,IFIELD,J)
                     UNWIN(4,IFIELD,J) = UNWIN(2,IFIELD,J)
                     UNWIN(2,IFIELD,J) = I1
                     END IF
C                                       no boxes this field
               ELSE
                  IF (ISUBF(IFIELD).GT.1) THEN
                     WRITE (MSGTXT,1030) IFIELD
                     CALL MSGWRT (6)
                  ELSE
                     ISUBF(IFIELD) = -1
                     END IF
                  END IF
               END IF
            END IF
 50      CONTINUE
 60   DO 70 I = 1,NFIELD
         IF (ISUBF(I).LT.0) UNBOXS(I) = 0
         IF (ISUBF(I).GT.0) UNBOXS(I) = MIN (LIMIT, ISUBF(I))
         IF (ISUBF(I).GT.LIMIT) THEN
            WRITE (MSGTXT,1060) I, LIMIT, ISUBF(I)
            CALL MSGWRT (6)
            END IF
 70      CONTINUE
      CALL ZTXCLS (LUN, FIND, I)
      IERR = 0
C
      IF (IDD.GT.0) THEN
         MSGTXT = 'UWINDF: Number of UNClean boxes/field read' //
     *      ' as follows:'
         CALL MSGWRT (3)
         DO 80 J = 1,NFIELD,4
            I1 = J
            I2 = MIN (NFIELD, I1+3)
            WRITE (MSGTXT,1070) ('Fld', I, UNBOXS(I), I = I1,I2)
            CALL MSGWRT (3)
 80         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UWINDF: ERROR',I4,1X,A,'ING THE CLEAN BOXES TEXT FILE')
 1020 FORMAT ('UWINDF: PARSING ERROR ON LINE',I4,' FIELD',I5)
 1030 FORMAT ('UWINDF: FIELD',I5,' ZERO BOXES CODE IGNORED DUE TO',
     *   ' REAL BOXES READ')
 1060 FORMAT ('WARNING: FIELD',I5,' USED FIRST',I5,' OF',I6,
     *   ' BOXES IN FILE')
 1070 FORMAT (4(A3,I5.2,':',I5,2X))
      END
