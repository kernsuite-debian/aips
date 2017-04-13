LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLN.INC'
      INTEGER NPARMS
      PARAMETER (NPARMS=53)
      INTEGER AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                      1        2         3          4
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
C            5          6          7         8
     *   'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK',
C            9       10        11        12       13       14      15
     *   'BCHAN', 'ECHAN', 'CHANNEL', 'NCHAV', 'CHINC', 'STOKES', 'BIF',
C          16       17         18        19         20         21
     *   'EIF', 'OUTNAME', 'OUTDISK', 'OUTSEQ', 'OUTVER', 'CELLSIZE',
C           22         23        24         25         26        27
     *   'IMSIZE', 'NFIELD', 'FLDSIZE', 'RASHIFT', 'DECSHIFT', 'NBOXES',
C           28       29         30        31       32        33
     *   'CLBOX', 'UVTAPER', 'UVRANGE', 'UVWTFN', 'UVBOX', 'ZEROSP',
C           34       35       36      37      38         39        40
     *   'XTYPE', 'YTYPE', 'XPARM', 'YPARM', 'GAIN', 'FLUX', 'MINPATCH',
C           41      42       43      44      45     46       47
     *   'NITER', 'BCOMP', 'BMAJ', 'BMIN', 'BPA', 'PHAT', 'FACTOR',
C            48         49      50        51          52     53
     *   'CMETHOD', 'CPARM', 'GUARD', 'MAXPIXEL', 'BADDISK', 'INFILE'/
C                    1       2       3       4       5       6
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOACAR, OOACAR,
C           7      8       9       10      11       12     13
     *   OOAINT, OOAINT, OOAINT, OOAINT, OOAINT, OOAINT, OOAINT,
C          14      15       16      17      18      19      20     21
     *   OOACAR, OOAINT,  OOAINT, OOACAR, OOAINT, OOAINT, OOAINT, OOARE,
C          22      23      24     25     26      27      28     29
     *    OOAINT,OOAINT, OOAINT, OOARE, OOARE, OOAINT, OOAINT, OOARE,
C          30      31      32     33      34      35     36     37
     *   OOARE, OOACAR, OOAINT, OOARE, OOAINT, OOAINT, OOARE, OOARE,
C          38     39      40      41      42     43     44     45
     *   OOARE, OOARE, OOAINT, OOAINT, OOAINT, OOARE, OOARE, OOARE,
C          46     47      48     49     50     51       52    53
     *   OOARE, OOARE, OOACAR, OOARE, OOARE, OOAINT, OOAINT, OOACAR/
C                   1    2    3    4     5    6    7    8    9    10
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 12,1, 6,1, 1,1, 1,1, 1,1, 1,1,
C         11   12   13   14   15   16    17   18   19   20   21   22
     *   1,1, 1,1, 1,1, 4,1, 1,1, 1,1, 12,1, 1,1, 1,1, 1,1, 2,1, 2,1,
C         23   24             25        26   27   28        29   30
     *   1,1, 2,MAXAFL, MAXAFL,1, MAXAFL,1, 1,1, 4,MXCLBX, 2,1, 2,1,
C         31   32   33   34    35    36   37   38   39   40    41
     *   2,1, 1,1, 5,1, 1,1, 1,1, 10,1, 10,1, 1,1, 1,1, 1,1, 1,1,
C              42   43   44   45   46   47   48   49    50   51    52
     *   MAXAFL,1, 1,1, 1,1, 1,1, 1,1, 1,1, 4,1, 10,1, 2,1, 1,1, 10,1,
C          53
     *   48,1/
LOCAL END
LOCAL INCLUDE 'IMAGWIN.INC'
      LONGINT   WINP
      INTEGER   WINM, WINW, WIN(2)
      COMMON /IMAWIN/ WINP, WINM, WINW, WIN
LOCAL END
      PROGRAM WFCLN
C-----------------------------------------------------------------------
C! Wide field and/or widefrequency  CLEANing/imaging task.
C# Task AP Imaging OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2002-2003, 2005-2006, 2008, 2012
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
C   Barry Clark CLEAN
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, CLEAN*32, UVDATA*32
      INTEGER  IRET, BUFF1(256)
      INCLUDE 'IMAGWIN.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'WFCLN'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL WFCLIN (PRGM, CLEAN, UVDATA, IRET)
C                                       CLEAN
      IF (IRET.EQ.0) CALL WFLEAN (CLEAN, UVDATA, IRET)
C                                       History
      IF (IRET.EQ.0) CALL WFCHIS (CLEAN, UVDATA)
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE WFCLIN (PRGN, CLEAN, UVDATA, IRET)
C-----------------------------------------------------------------------
C   WFCLIN gets input parameters for WFCLN and creates the CLEAN object.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      CLEAN   C*32 Name of CLEAN object (contains output objects)
C      UVDATA  C*32 Name of input uv data.
C      IRET    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, CLEAN*(*), UVDATA*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'INPUT.INC'
      INTEGER   NKEY1, NKEY2, NKEY3, NKEY4, NKEY5
C                                       NKEY1=no. adverbs to copy to
C                                       CLEAN object
      PARAMETER (NKEY1=13)
C                                       NKEY2 = no. adverb for UVDATA
      PARAMETER (NKEY2=18)
C                                       NKEY3 = no. adverb for UVWORK
      PARAMETER (NKEY3=3)
C                                       NKEY4 = no. adverbs for BEAM
      PARAMETER (NKEY4=4)
C                                       NKEY5 = no. adverbs for CLEANO
      PARAMETER (NKEY5=14)
      INTEGER   DIM(7), TYPE, NBOXES(MAXFLD), BCHAN, ECHAN, NWORDS,
     *   NAXIS(7), I, IMSI(2), IMSIZE(2,MAXFLD), NPTWO, IS,
     *   FLDSIZ(2,MAXFLD), NXFLD, NYFLD, NXBEM, NYBEM, MAXNX, MAXNY,
     *   MINNX, MINNY, NITER, EDGEX, EDGEY, NFIELD, CHINC, NCHAV,
     *   BMSIZ(2), BIF, EIF, IFINDX, FQINDX, BMSSZ(2), IROUND, DUMMY
      LONGINT   KP
      LOGICAL   NOREST, DODFT, DOSCAL
      REAL      BMAJ, BMIN, BPA, CPARM(10), RASH(MAXFLD), DECSH(MAXFLD),
     *   CELLS(2), DU, DV, UMAX, VMAX, GUARDB(2), GUAU, GUAV, UVTAPR(2),
     *   UTFACT(2)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32,
     *   INK2(NKEY2)*8, OUTK2(NKEY2)*32, INK3(NKEY3)*8, OUTK3(NKEY3)*32,
     *   INK4(NKEY4)*8, OUTK4(NKEY4)*32, INK5(NKEY5)*8, OUTK5(NKEY5)*32,
     *   CLEANO(MAXFLD)*32, BEAM*32, UVWORK*32, STOKES*4, CLASS*6,
     *   CLTYPE*2, CHTYPE*4, CLNTYP*8, CNAME*8, KEYW*8, TINAME*12,
     *   TONAME*12, CDUMMY*1, FILEIN*48
      INCLUDE 'IMAGWIN.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to CLEAN object
C                   1        2        3       4       5
      DATA INK1 /'GAIN', 'FLUX', 'PHAT', 'NITER', 'BCOMP',
C           6       7       8       9         10         11       12
     *   'BMAJ', 'BMIN', 'BPA', 'FACTOR', 'MINPATCH', 'OUTVER', 'CPARM',
C            13
     *   'MAXPIXEL'/
C                                       Rename
C                    1        2         3        4
      DATA OUTK1 /'GAIN', 'MINFLUX', 'PHAT', 'NITER',
C           5          6           7           8            9
     *   'BCOMP', 'BEAM.BMAJ', 'BEAM.BMIN', 'BEAM.BPA', 'FACTOR',
C           10          11         12       13
     *   'MINPATCH', 'VERSION', 'CPARM', 'MAXNRES'/
C                                       Adverbs for UVDATA object
C                    1         2         3         4        5
      DATA INK2 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'BCHAN',
C          6        7      8       9         10        11         12
     *   'ECHAN', 'BIF', 'EIF', 'STOKES', 'UVRANGE', 'CHINC', 'CMETHOD',
C          13           14         15         16       17         18
     *   'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK', 'UVWTFN', 'NCHAV'/
C                                       Rename
C                   1       2         3       4        5        6
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'BCHAN', 'ECHAN',
C          7      8       9         10         11        12
     *   'BIF', 'EIF', 'STOKES', 'UVRNG', 'CHINC', 'MODMETH',
C          13           14         15         16        17        18
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK', 'UVWTFN', 'NCHAV'/
C                                       Adverbs for UVWORK object
C                    1        2         3
      DATA INK3 /'CHINC', 'CMETHOD', 'GUARD'/
C                                       Rename
C                    1         2          3
      DATA OUTK3 /'CHINC', 'MODMETH', 'GUARDBND'/
C                                       Adverbs for BEAM image object
C                    1          2          3         4
      DATA INK4 /'OUTNAME', 'OUTSEQ', 'OUTDISK', 'CELLSIZE'/
C                                       Rename
C                    1       2       3         4
      DATA OUTK4 /'NAME', 'IMSEQ', 'DISK', 'CELLSIZE'/
C                                       Adverbs for CLEANO image object
C                    1         2         3           4          5
      DATA INK5 /'OUTNAME', 'OUTSEQ', 'OUTDISK', 'CELLSIZE', 'RASHIFT',
C            6          7        8        9       10        11
     *   'DECSHIFT', 'XTYPE', 'YTYPE', 'XPARM', 'YPARM', 'ZEROSP',
C            12        13        14
     *   'UVTAPER', 'UVWTFN' ,'UVBOX'/
C                                       Rename
C                   1        2       3          4          5
      DATA OUTK5 /'NAME', 'IMSEQ', 'DISK', 'CELLSIZE', 'RASHIFT',
C             6         7        8        9       10        11
     *   'DECSHIFT', 'CTYPX', 'CTYPY', 'XPARM', 'YPARM', 'ZEROSP',
C            12        13       14
     *   'UVTAPER', 'UVWTFN' ,'UVBOX'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
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
C                                       Default output Name = input
      CALL OGET ('Input', 'INNAME', TYPE, DIM, DUMMY, TINAME, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'OUTNAME', TYPE, DIM, DUMMY, TONAME, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (TONAME.EQ.'    ') THEN
         CALL OPUT ('Input', 'OUTNAME', TYPE, DIM, DUMMY, TINAME, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Default NFIELD=1
      CALL OGET ('Input', 'NFIELD', TYPE, DIM, NFIELD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF ((NFIELD.LE.0) .OR. (NFIELD.GT.MAXAFL)) THEN
         IF (NFIELD.LE.0) NFIELD = 1
         IF (NFIELD.GT.MAXAFL) NFIELD = MAXAFL
         CALL OPUT ('Input', 'NFIELD', TYPE, DIM, NFIELD, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Create CLEAN object
      CLEAN = 'CLEAN process object'
      CALL CREATE (CLEAN, 'CLEAN', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, CLEAN, IRET)
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
      IF (STOKES .EQ. '    ') STOKES = 'I    '
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
C                                       Get IF channel selection
      CALL SECSLT (UVDATA, BIF, EIF, BCHAN, ECHAN, STOKES, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       All Stokes', IF, channels
      CALL SECSAV (UVDATA, 1, 0, 1, 0, '    ', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open UVDATA to be sure it's OK.
      CALL OOPEN (UVDATA, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Reset Selection
      CALL SECSAV (UVDATA, BIF, EIF, BCHAN, ECHAN, STOKES, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       UVWORK
      UVWORK = 'UVdata work object'
      CALL CREATE (UVWORK, 'UVDATA', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY3, INK3, OUTK3, UVWORK, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BEAM
      BEAM = 'BEAM image for CLEAN'
      CALL CREATE (BEAM, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY4, INK4, OUTK4, BEAM, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Classes derived from Stokes'
      CLASS = STOKES(1:1) // 'BEAM'
      DIM(1) = 6
      CALL OPUT (BEAM, 'CLASS', OOACAR, DIM, DUMMY, CLASS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       CLEAN
C                                       Get number of fields
      CALL OGET ('Input', 'NFIELD', TYPE, DIM, NFIELD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DO 50 I = 1,NFIELD
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
         CLASS = STOKES(1:1) // CLTYPE
         IF (I.LE.999) THEN
            WRITE (CLASS(4:6),1040) I
         ELSE
            WRITE (CLASS(3:6),1041) I
            END IF
         DIM(1) = LEN (CLASS)
         DIM(2) = 1
         CALL OPUT (CLEANO(I), 'CLASS', OOACAR, DIM, DUMMY, CLASS, IRET)
         IF (IRET.NE.0) GO TO 999
 50      CONTINUE
C                                       Attach Images etc. to CLEAN
C                                       Number of fields
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (CLEAN, 'NIMAGES', OOAINT, DIM, NFIELD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       UVWORK
      DIM(1) = LEN (UVWORK)
      DIM(2) = 1
      CALL OPUT (CLEAN, 'UVDATA', OOACAR, DIM, DUMMY, UVWORK, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BEAM image
      DIM(1) = LEN (BEAM)
      CALL OPUT (CLEAN, 'DIRTBEAM', OOACAR, DIM, DUMMY, BEAM, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       CLEAN image(s)
      DIM(1) = LEN (CLEANO(1))
      DIM(2) = NFIELD
      CALL OPUT (CLEAN, 'CLEANI', OOACAR, DIM, DUMMY, CLEANO, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Process other info
C                                       Field size
      CALL OGET ('Input', 'FLDSIZE', TYPE, DIM, FLDSIZ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Min. image sizes
      CALL OGET ('Input', 'IMSIZE', TYPE, DIM, IMSI, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       No images smaller than 64
      IMSI(1) = MAX (IMSI(1), 64)
      IMSI(2) = MAX (IMSI(2), 64)
      MAXNX = 0
      MAXNY = 0
      MINNX = MAXIMG
      MINNY = MAXIMG
      DO 70 I = 1,MAXAFL
         IF (FLDSIZ(1,I).LE.0) FLDSIZ(1,I) = IMSI(1) - 10
         IF (FLDSIZ(2,I).LE.0) FLDSIZ(2,I) = IMSI(2) - 10
C                                       Next largest power of two
         NXFLD = NPTWO (FLDSIZ(1,I))
         NYFLD = NPTWO (FLDSIZ(2,I))
         IMSIZE(1,I) = MAX (IMSI(1), NXFLD)
         IMSIZE(2,I) = MAX (IMSI(2), NYFLD)
         MAXNX = MAX (MAXNX, IMSIZE(1,I))
         MAXNY = MAX (MAXNY, IMSIZE(2,I))
         MINNX = MIN (MINNX, IMSIZE(1,I))
         MINNY = MIN (MINNY, IMSIZE(2,I))
 70      CONTINUE
      DIM(1) = 2
      DIM(2) = MAXFLD
      CALL OPUT (CLEANO(1), 'IMSIZE', OOAINT, DIM, IMSIZE, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(2) = 1
      DO 80 I = 2,NFIELD
         CALL OPUT (CLEANO(I), 'IMSIZE', OOAINT, DIM, IMSIZE(1,I),
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
 80      CONTINUE
C                                       Beam size
      CALL OGET ('Input', 'NITER', TYPE, DIM, NITER, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Control information.
      CALL OGET ('Input', 'CPARM', TYPE, DIM, CPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DODFT = CPARM(4) .GT. 0.0
C                                       Beam same size as image for DFT.
      IF (DODFT) THEN
         NXBEM = MAXNX
         NYBEM = MAXNY
      ELSE
         NXBEM = MIN (MAXNX*2, 2048)
         NYBEM = MIN (MAXNY*2, 2048)
         END IF
C                                       If not cleaning make beam =
C                                       field 1.
      IF (NITER.LE.0) THEN
         NXBEM = IMSIZE(1,1)
         NYBEM = IMSIZE(2,1)
         END IF
      BMSIZ(1) = NXBEM
      BMSIZ(2) = NYBEM
      DIM(1) = 2
      DIM(2) = 1
      CALL OPUT (BEAM, 'IMSIZE', OOAINT, DIM, BMSIZ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Define the clean boxes from input
C                                       file or program inputs
C
      CALL OGET ('Input', 'INFILE', TYPE, DIM, DUMMY, FILEIN, IRET)
      IF (IRET.NE.0) GO TO 999
C
      WINM = MIN (MXNBOX, MXNBFL/NFIELD)
      WINW = 4 * NFIELD * WINM
      NWORDS = (WINW - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'WFCLN', NWORDS, WIN(1), WINP, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Use INFILE
      IF (FILEIN(1:1).NE.'  ') THEN
         CALL WINDF (WIN(1+WINP), NBOXES, NFIELD, IMSIZE, FLDSIZ,
     *      FILEIN, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'WFCLIN: Error in defining clean boxes from file'
            CALL MSGWRT(7)
         ENDIF
         DIM(1) = NFIELD
         DIM(2) = 1
         CALL OPUT (CLEAN, 'NBOXES', OOAINT, DIM, NBOXES, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         DIM(1) = 4 * NFIELD
         DIM(2) = WINM
         CALL OPUT (CLEAN, 'WINDOW', OOAINT, DIM, WIN(1+WINP), CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Use program inputs
      ELSE
         CALL FILL (MAXFLD, 1, NBOXES)
         CALL OGET ('Input', 'NBOXES', TYPE, DIM, NBOXES, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (NBOXES(1).GT.0) THEN
            IS = 2
         ELSE
            NBOXES(1) = 1
            IS = 1
            END IF
         DIM(1) = NFIELD
         DIM(2) = 1
         CALL OPUT (CLEAN, 'NBOXES', OOAINT, DIM, NBOXES, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Window
         CALL FILL (WINW, 0, WIN(1+WINP))
         CALL OGET ('Input', 'CLBOX', TYPE, DIM, WIN(1+WINP), CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Fill in higher windows
C                                       or all if NBOXES <= 0
         DO 100 I = IS,NFIELD
            KP = (I-1) * 4 + WINP
C                                       fill window from IMSIZE and
C                                       FLDSIZE
            EDGEX = IMSIZE(1,I) - FLDSIZ(1,I)
            EDGEY = IMSIZE(2,I) - FLDSIZ(2,I)
            WIN(1+KP) = EDGEX / 2
            WIN(2+KP) = EDGEY / 2
            WIN(3+KP) = IMSIZE(1,I) - EDGEX / 2
            WIN(4+KP) = IMSIZE(2,I) - EDGEY / 2
 100        CONTINUE
         DIM(1) = 4 * NFIELD
         DIM(2) = WINM
         CALL OPUT (CLEAN, 'WINDOW', OOAINT, DIM, WIN(1+WINP), CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Free dynamic memory
C                                       Really should go to HI but
      CALL ZMEMRY ('FREE', 'WFCLN', WINW, WIN(1), WINP, IRET)
C                                       Restore?
      CALL OGET ('Input', 'BMAJ', TYPE, DIM, BMAJ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'BMIN', TYPE, DIM, BMIN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'BPA', TYPE, DIM, BPA, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      NOREST = BMAJ .LT. 0.0
      DIM(1) = 1
      CALL OPUT (CLEAN, 'NORESTOR', OOALOG, DIM, NOREST, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
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
      IF (IRET.NE.0) GO TO 999
      BCHAN = MAX (BCHAN, 1)
      IF (ECHAN.LE.0) ECHAN = NAXIS(FQINDX)
      DIM(1) = 1
      IF ((ECHAN.GT.NAXIS(FQINDX)) .OR. (ECHAN.LT.BCHAN)) THEN
         IRET = 5
         WRITE (MSGTXT,1100) BCHAN, ECHAN, NAXIS(FQINDX)
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL OPUT (CLEAN, 'BCHAN', OOAINT, DIM, BCHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (CLEAN, 'ECHAN', OOAINT, DIM, ECHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Save in Inputs for history
      CALL OPUT ('Input', 'BCHAN', OOAINT, DIM, BCHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT ('Input', 'ECHAN', OOAINT, DIM, ECHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       IF numbers
C                                       Default BIF, EIF
      CALL OGET ('Input', 'BIF', TYPE, DIM, BIF, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'EIF', TYPE, DIM, EIF, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDFND (UVDATA, 2, 'IF  ', IFINDX, IRET)
      IRET = 0
      IF (IFINDX.GT.0) THEN
C                                       Find size of IF axis.
         CALL OGET (UVDATA, 'UV_DESC.NAXIS', TYPE, DIM, NAXIS, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         BIF = MAX (1, BIF)
         IF (EIF.LE.0) EIF = NAXIS(IFINDX)
         IF ((EIF.GT.NAXIS(IFINDX)) .OR. (EIF.LT.BIF)) THEN
            IRET = 5
            WRITE (MSGTXT,1101) BIF, EIF, NAXIS(IFINDX)
            CALL MSGWRT (8)
            GO TO 999
            END IF
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
C         NCHAV = NCHAV * (EIF - BIF + 1)
C         NCHAV = MAX (1, NCHAV)
         END IF
C                                       Save value
      CALL OPUT ('Input', 'NCHAV', TYPE, DIM, NCHAV, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy UV descriptors to BEAM
      DIM(1) = LEN (CHTYPE)
      DIM(2) = 1
      CALL OPUT (BEAM, 'CHTYPE', OOACAR, DIM, DUMMY, CHTYPE, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL U2IDES (UVDATA, BEAM, .FALSE., IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy UV descriptors to Images
      DO 200 I = 1,NFIELD
         CALL OPUT (CLEANO(I), 'CHTYPE', OOACAR, DIM, DUMMY, CHTYPE,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         CALL U2IDES (UVDATA, CLEANO(I), .FALSE., IRET)
         IF (IRET.NE.0) GO TO 999
 200     CONTINUE
C                                       Scale residuals?
      DOSCAL = CPARM(5) .GT. 0.0
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (CLEAN, 'SCALERES', OOALOG, DIM, DOSCAL, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      BMSSZ(1) = IROUND (CPARM(6))
      BMSSZ(2) = IROUND (CPARM(7))
      IF (BMSSZ(1).LE.0) BMSSZ(1) = 5
      IF (BMSSZ(2).LE.0) BMSSZ(2) = 5
      DIM(1) = 2
      CALL OPUT (CLEAN, 'BMSCLSZ', OOAINT, DIM, BMSSZ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Special treatment: DFT imaging
      IF (DODFT) THEN
C                                       Set shift for DFT
         CALL OGET ('Input', 'RASHIFT', TYPE, DIM, RASH, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OGET ('Input', 'DECSHIFT', TYPE, DIM, DECSH, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         DIM(1) = 1
         CALL OPUT (CLEANO(1), 'XSHIFT', OOARE, DIM, RASH(1), CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT (CLEANO(1), 'YSHIFT', OOARE, DIM, DECSH(1), CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
C                                       MINPATCH = 50
         CALL OPUT (CLEAN, 'MINPATCH', OOAINT, DIM, 50, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       MAXPATCH = 50
         CALL OPUT (CLEAN, 'MAXPATCH', OOAINT, DIM, 50, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
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
      CALL OGET ('Input', 'UVTAPER', TYPE, DIM, UVTAPR, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
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
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLEAN field number ',I5)
 1040 FORMAT (I3.3)
 1041 FORMAT (I4.4)
 1100 FORMAT ('BCHAN - ECHAN =',I5,' -',I5,' NOT IN RANGE 1 -',I5)
 1101 FORMAT ('BIF - EIF =',I4,' -',I4,' NOT IN RANGE 1 -',I4)
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
 999  RETURN
      END
      SUBROUTINE WFLEAN (CLEAN, UVDATA, IERR)
C-----------------------------------------------------------------------
C   Does CLEAN, looping over channels
C   Inputs:
C      CLEAN   C*?  Name of CLEAN process object
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER CLEAN*(*), UVDATA*32
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ICHAN, BCHAN, ECHAN, CHINC, CHANN, TYPE, DIM(7), NCHAV,
     *   I, NFIELD, JCHAN, DUMMY, LCHAN, NIF, BIF, EIF, NCHAVG, LECHAN,
     *   NUMCH
      LOGICAL   DOPBFM, DODFT, DOWARN, DOMAX
      REAL      CPARM(10), SPINDX, FSCAL, PBFSIZ, AVGTIM, PANGL, ZANGL
      DOUBLE PRECISION PBFREQ(MAXIF), UVFREQ
      CHARACTER UVWORK*32, CLEANO(MAXFLD)*32, CHTYPE*4, FTTYPE*4,
     *   CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Control information.
      CALL OGET (CLEAN, 'CPARM', TYPE, DIM, CPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      PBFSIZ = CPARM(1)
      DOPBFM = PBFSIZ .GT. 0.0
      SPINDX = CPARM(2)
      FSCAL = CPARM(3)
      DODFT = CPARM(4) .GT. 0.0
C                                       Uv work object
      CALL OGET (CLEAN, 'UVDATA', TYPE, DIM, DUMMY, UVWORK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       CLEAN image object
      CALL OGET (CLEAN, 'CLEANI', TYPE, DIM, DUMMY, CLEANO, IERR)
      IF (IERR.NE.0) GO TO 999
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
C                                       Force create of UVWORK
      CALL OCLONE (UVDATA, UVWORK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open CLEAN
      CALL OOPEN (CLEAN, 'DEST', IERR)
      IF (IERR.NE.0) GO TO 999
      DOWARN = .TRUE.
C                                       Loop over channel
      DO 600 ICHAN = CHANN,LECHAN,CHINC
         DOMAX = ICHAN+CHINC.GT.LECHAN
C                                       Copy uv data to work object, set
C                                       channel.
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL OPUT (UVDATA, 'CALEDIT.BCHAN', OOAINT, DIM, ICHAN, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         JCHAN = ICHAN + NCHAV - 1
         NCHAVG = NCHAV * NIF
         CALL OPUT (UVDATA, 'CALEDIT.ECHAN', OOAINT, DIM, JCHAN, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Progress message
         WRITE (MSGTXT,1000) ICHAN, JCHAN, NIF
         CALL MSGWRT (4)
C                                       Select/correct
         CALL WFCOPY (UVDATA, UVWORK, SPINDX, FSCAL, DOWARN, IERR)
         IF (IERR.NE.0) GO TO 999
         DOWARN = .FALSE.
C                                       Special options
C                                       Primary beam corrections
         IF (DOPBFM) THEN
C                                       Freq array
            CALL UVFRQS (UVDATA, UVFREQ, PBFREQ, IERR)
            IF (IERR.NE.0) GO TO 999
            DIM(1) = 1
            DIM(2) = 1
            CALL OPUT (UVWORK, 'DOPBFM', OOALOG, DIM, DOPBFM, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            CALL OPUT (UVWORK, 'PBFSIZ', OOARE, DIM, PBFSIZ, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            DIM(1) = NCHAVG
            CALL OPUT (UVWORK, 'PBFREQ', OOADP, DIM, PBFREQ, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
C                                       DFT or FFT
         IF (DODFT) THEN
            FTTYPE = 'DFT'
         ELSE
            FTTYPE = 'FFT'
            END IF
         DIM(1) = LEN (FTTYPE)
         CALL OPUT (CLEANO(1), 'FTTYPE', OOACAR, DIM, DUMMY, FTTYPE,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Uniform weighting
C                                       Set channel in CLEAN
         DIM(1) = 1
         LCHAN = (ICHAN - BCHAN) / CHINC + 1
         CALL OPUT (CLEAN, 'CHANNEL', OOAINT, DIM, LCHAN, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OPUT (CLEAN, 'UVCHAN', OOAINT, DIM, 1, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OPUT (CLEAN, 'NCHAV', OOAINT, DIM, NCHAVG, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Set CC version, (line only)
         IF (CHTYPE.EQ.'LINE') THEN
            CALL OPUT (CLEAN, 'VERSION', OOAINT, DIM, LCHAN, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
C                                       Uniform weight/beam mod
         CALL OUNFWT (UVWORK, CLEANO(1), IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Clean
         CALL CLNUV (CLEAN, DOMAX, IERR)
         IF (IERR.NE.0) GO TO 999
 600     CONTINUE
C                                       How many fields?
      CALL OGET (CLEAN, 'NIMAGES', TYPE, DIM, NFIELD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close CLEAN
      CALL OCLOSE (CLEAN, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Observing geometry
C                                       Get average time
      CALL WFTAV (UVDATA, AVGTIM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get parallactic, zenith angles
      CALL WFPARG (UVDATA, AVGTIM, PANGL, ZANGL, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 700 I = 1,NFIELD
C                                       Save observing geometry on CLEAN
C                                       images
         DIM(1) = 1
         DIM(2) = 1
         CALL OPUT (CLEANO(I), 'PARANGLE', OOARE, DIM, PANGL, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OPUT (CLEANO(I), 'ZENANGLE', OOARE, DIM, ZANGL, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
 700     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Beginning channel',I5,' through',I5,' with',I3,' IFs')
      END
      SUBROUTINE WFTAV (UVDATA, AVGTIM, IERR)
C-----------------------------------------------------------------------
C   Determines the average time of a data set. Averages time in GST and
C   then converts back to Solar time on day 0.  This avoids the problems
C   associated with observations spanning several days.
C      NOTE: this routine uses "AP" memory for scratch memory
C   Inputs:
C      UVDATA C*?  UV residual file from previous CLEAN
C   Output:
C      AVGTIM R Average time in days.
C      IERR   I Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*)
      REAL      AVGTIM
      INTEGER   IERR
C
      INTEGER   COUNT, JLOCT, TYPE, DIM(3), LERR
      REAL      RP(50), VIS(3,10000)
      DOUBLE PRECISION GSTIA0, DEGPDY, SUMR, SUMI, TIME, GST0, ROTRAT
      CHARACTER TABLE*32, CDUM*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      CALL APOBJ ('OPEN', 'WFTAV', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get time information from AN
C                                       table.
      TABLE = 'Temporary AN table for WFTAV'
      CALL UV2TAB (UVDATA, TABLE, 'AN', 1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABOPN (TABLE, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABGET (TABLE, 'KEY.GSTIA0', TYPE, DIM, GSTIA0, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABGET (TABLE, 'KEY.DEGPDY', TYPE, DIM, DEGPDY, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABCLO (TABLE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Delete object
      CALL TABDES (TABLE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Convert units to radians
      GST0 = GSTIA0 * DG2RAD
      ROTRAT = DEGPDY * DG2RAD
C                                       Open uv data
      CALL OUVOPN (UVDATA, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Which random parameter
      CALL UVDFND (UVDATA, 1, 'TIME', JLOCT, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'WFTAV:TROUBLE FINDING RANDOM PARAMETER TIME'
         CALL MSGWRT (7)
         GO TO 990
         END IF
      SUMR = 0.0
      SUMI = 0.0
      COUNT = 0
C                                       Loop thru data
 100  CALL UVREAD (UVDATA, RP, VIS, IERR)
         IF (IERR.GT.0) THEN
            GO TO 990
         ELSE IF (IERR.EQ.0) THEN
C                                       Convert time to GST(radians)
            TIME = RP(JLOCT) * ROTRAT + GST0
C                                       Sum as sine and cosine
            COUNT = COUNT + 1
            SUMR = SUMR + COS (TIME)
            SUMI = SUMI + SIN (TIME)
            GO TO 100
            END IF
C                                       Close
      IERR = 0
      CALL OUVCLO (UVDATA, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Average as cosine and sine to
C                                       deal with periodic nature of GST
      IF (COUNT.GT.0) THEN
         AVGTIM = ATAN2 (SUMI, SUMR+1.0D-20)
      ELSE
         MSGTXT = 'WFTAV: NO VISIBILITIES'
         CALL MSGWRT (7)
         IERR = 5
         GO TO 990
         END IF
C                                       Convert time back to solar
      AVGTIM = (AVGTIM - GST0)
      IF (AVGTIM.LE.0.0) AVGTIM = AVGTIM + TWOPI
      IF (AVGTIM.LE.0.0) AVGTIM = AVGTIM + TWOPI
      AVGTIM = AVGTIM / ROTRAT
      CALL APOBJ ('CLOSE', 'WFTAV', LERR)
      GO TO 999
C                                       Error
 990  CALL APOBJ ('CLOSE', 'WFTAV', LERR)
 995  MSGTXT = 'WFTAV: ERROR AVERAGING TIME FOR ' // UVDATA
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE WFPARG (UVDATA, AVGTIM, PANGL, ZANGL, IERR)
C-----------------------------------------------------------------------
C   Determines the observing geometry from the average time.
C   Inputs:
C      UVDATA C*?  UV residual file from previous CLEAN
C      AVGTIM R Average time in days.
C   Output:
C      PANGL  R Parallactic angle in degrees.
C      ZANGL  R Zenith angle in degrees.
C      IERR   I Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*)
      REAL      AVGTIM, PANGL, ZANGL
      INTEGER   IERR
C
      INTEGER   ANT, SUBARR, SOURID
      CHARACTER TABLE*32
      REAL      PANG, EL, HA, ZA
      DOUBLE PRECISION TIME
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      IERR = 0
      TABLE = 'Temporary table for WFPARG'
      CALL UV2TAB (UVDATA, TABLE, 'AN', 1, IERR)
C                                       Get parallactic angle
      ANT = 1
      SUBARR = 1
      SOURID = -1
      CALL OSUPAN (TABLE, ANT, SUBARR, SOURID, AVGTIM, PANG, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get elevation
      TIME = AVGTIM
      CALL OSUELV (TABLE, ANT, SUBARR, SOURID, TIME, HA, EL, ZA, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Delete object
      CALL TABDES (TABLE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Convert to degrees.
      PANGL = PANG * 57.296
C                                       Get zenith angle
      ZANGL = 90.0 - EL * 57.296
      GO TO 999
C                                       Error
 990  MSGTXT = 'WFPARG: ERROR DETERMINING OBSERVING GEOMETRY'
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE WFCOPY (UVIN, UVOUT, SPINDX, FSCAL, DOWARN, IERR)
C-----------------------------------------------------------------------
C   Copies one uv data object to another with application of any
C   calibration editing, and selection criteria.
C   Data are scaled in frequency to remove a  spectral index of SPINDX
C   and corrected to the reference frequency.  Also, u, v and w are
C   scaled by FSCAL.
C   NOTE: This routine will only handle up to 1024 correlations in a vis.
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.  All specified
C                   selection, editing and calibration are applied.
C      UVOUT   C*?  Name of output uvdata object.  May be UVIN.
C      DOWARN  L    Do warning messages about data selection?
C   Inputs attached to UVIN
C      UMAX    R    Maximum acceptable U in wavelengths (default all)
C      VMAX    R    Maximum acceptable V in wavelengths (default all)
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
      REAL      SPINDX, FSCAL
      INTEGER   IERR
      LOGICAL   DOWARN
C
      INTEGER   COUNT, TYPE, DIM(7), SUBA, NCOR, NAXIS(7), NSTOK, I, J,
     *   K, INDEX, JLOCS, INDXU, INDXV, INDXW, MSGSAV, DUMMY, TCOUNT,
     *   LERR
      CHARACTER SORD*2, CDUMMY*1
      REAL      RP(50), VS(3,1024), FS(1024), FST, UMAX, VMAX, AUMAX,
     *   BUMAX, AVMAX, BVMAX, GUARDB(2), UTFACT(2), UU, VV, CELLS(2)
      DOUBLE PRECISION FREQS(1024), UVFREQ, DSUM, AVGFRQ, FRMULT
      LOGICAL   EXIST, DOSCLU
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      CALL APOBJ ('OPEN', 'WFCOPY', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Open input.
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Create output if necessary
      CALL OBFEXS (UVOUT, EXIST, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (.NOT.EXIST) THEN
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
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OUVGET (UVIN, 'VMAX', TYPE, DIM, VMAX, CDUMMY, IERR)
      MSGSUP = MSGSAV
C                                       Default = all
      IF (IERR.EQ.1) THEN
         VMAX = 1.0E30
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 980
C                                       Actual guardband
      MSGSAV = MSGSUP
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
      MSGSAV = MSGSUP
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
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OUVGET (UVIN, 'CELLSIZE', TYPE, DIM, CELLS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CELLS(1) = 1.0
         CELLS(2) = 1.0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 980
C                                       Open output
      CALL OUVOPN (UVOUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy UVDESC
      CALL UVDSCP (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Subarray number
      CALL SECGET (UVIN, 'SUBARR', TYPE, DIM, SUBA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (SUBA.LE.0) SUBA = 1
C                                       NCORR
      CALL UVDGET (UVIN, 'NCORR', TYPE, DIM, NCOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Frequency info
      DOSCLU = FSCAL .GT. 0.0
C                                       Uv data pointers
      CALL UVDFND (UVIN, 2, 'STOKES', JLOCS, IERR)
      IF (IERR.NE.0) GO TO 990
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
      NSTOK = NAXIS(JLOCS)
C                                       Freq array
      CALL UVFRQS (UVIN, UVFREQ, FREQS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Average frequency
      COUNT = 0
      DSUM = 0.0D0
      K = 0
      FRMULT = 0.0D0
      DO 30 I = 1,NCOR,NSTOK
         K = K + 1
         DSUM = DSUM + FREQS(K)
         FRMULT = MAX (FRMULT, FREQS(K))
         COUNT = COUNT + 1
 30      CONTINUE
      FRMULT = FRMULT / UVFREQ
      AVGFRQ = DSUM / COUNT
C                                       Frequency scaling: Stokes is
C                                       always before IF and Freq.
      INDEX = 0
      K = 0
      DO 50 I = 1,NCOR,NSTOK
         K = K + 1
         FST = (AVGFRQ / FREQS(K)) ** SPINDX
         DO 40 J = 1,NSTOK
            INDEX = INDEX + 1
            FS(INDEX) = FST
 40         CONTINUE
 50      CONTINUE
C                                       Initialize visibility count
      COUNT = 0
      TCOUNT = 0
      AUMAX = 0.0
      AVMAX = 0.0
      BUMAX = 0.0
      BVMAX = 0.0
      UMAX = UMAX / FRMULT
      VMAX = VMAX / FRMULT
      IF (SUBA.GT.1) THEN
         CALL UVDGET (UVOUT, 'GCOUNT', TYPE, DIM, COUNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Loop thru data
 100     CALL UVREAD (UVIN, RP, VS, IERR)
         IF (IERR.LT.0) GO TO 200
         IF (IERR.GT.0) GO TO 990
C                                       Want this one?
         UU = ABS (RP(INDXU))
         VV = ABS (RP(INDXV))
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
            DO 150 I = 1,NCOR
               VS(1,I) = VS(1,I) * FS(I)
               VS(2,I) = VS(2,I) * FS(I)
 150           CONTINUE
            COUNT = COUNT + 1
            CALL UVWRIT (UVOUT, RP, VS, IERR)
            IF (IERR.GT.0) GO TO 990
            END IF
         GO TO 100
 200     IERR = 0
C                                       Warnings ?
      IF (DOWARN) THEN
         IF (TCOUNT.GT.COUNT) THEN
            WRITE (MSGTXT,1200) COUNT, TCOUNT
            CALL MSGWRT (6)
         ELSE
            WRITE (MSGTXT,1201) COUNT
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
               WRITE (MSGTXT,1210) 'U', AUMAX, UMAX
               CALL MSGWRT (6)
               WRITE (MSGTXT,1211) UU, ABS(CELLS(1))
               CALL MSGWRT (6)
               WRITE (MSGTXT,1212) 'U', GUARDB(1)
               CALL MSGWRT (6)
               END IF
            IF (AVMAX.GT.VMAX) THEN
               VV = VMAX/AVMAX * ABS(CELLS(2))
               WRITE (MSGTXT,1210) 'V', AVMAX, VMAX
               CALL MSGWRT (6)
               WRITE (MSGTXT,1211) VV, CELLS(2)
               CALL MSGWRT (6)
               WRITE (MSGTXT,1212) 'V', GUARDB(2)
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
         END IF
C                                       Better be some data
      IF (COUNT.LE.0) THEN
         IERR = 7
         MSGTXT = 'WFCOPY: NO DATA SELECTED'
         GO TO 980
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
      IF (SUBA.GT.1) SORD = '  '
      CALL UVDPUT (UVOUT, 'SORTORD', TYPE, DIM, DUMMY, SORD, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy relevant tables
      CALL UVDTCO (UVIN, UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close files, update disk
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVCLO (UVOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Mark output as valid
      DIM(1) = 1
      CALL FSTPUT (UVOUT, 'VALID', OOALOG, DIM, .TRUE., CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL APOBJ ('CLOSE', 'WFCOPY', LERR)
      GO TO 999
C                                       Error
 980  CALL MSGWRT (7)
 990  CALL APOBJ ('CLOSE', 'WFCOPY', LERR)
 995  MSGTXT = 'WFCOPY: ERROR COPYING ' // UVIN
      CALL MSGWRT (7)
      MSGTXT = 'TO ' // UVOUT
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('WFCOPY: Only',I9,' samples of',I9,' fell on UV grid')
 1201 FORMAT ('WFCOPY: Copied',I9,' visibilities to be imagied')
 1210 FORMAT ('**** Actual ',A,'max',1PE11.4,' exceeds limit',1PE11.4,
     *   8X,'****')
 1211 FORMAT ('**** Use cellsize <',F8.4,' not',F8.4,
     *   ' to get all the data ****')
 1212 FORMAT ('**** using a ',A,' guard band of',F7.3,' of a radius',
     *   13X,'****')
      END
      SUBROUTINE WFCHIS (CLEAN, UVDATA)
C-----------------------------------------------------------------------
C   Routine to write history file to output CLEAN image object.
C   Inputs:
C      CLEAN   C*?  CLEAN process object
C      UVDATA  C*?  Input UV data
C-----------------------------------------------------------------------
      CHARACTER CLEAN*(*), UVDATA*32
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NADV
      PARAMETER (NADV=45)
      CHARACTER CLEANO(MAXFLD)*32, BEAM*32, LIST(NADV)*8, CDUMMY*1
      INTEGER   IERR, TYPE, DIM(7), NFIELD, I, DUMMY
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'IN2NAME', 'IN2SEQ',
     *   'BCHAN', 'ECHAN', 'CHANNEL', 'NCHAV', 'CHINC', 'STOKES', 'BIF',
     *   'EIF', 'OUTNAME', 'OUTDISK', 'OUTSEQ', 'CELLSIZE', 'IMSIZE',
     *   'NFIELD', 'FLDSIZE', 'RASHIFT', 'DECSHIFT', 'NBOXES', 'CLBOX',
     *   'UVTAPER', 'UVRANGE', 'UVWTFN', 'UVBOX', 'ZEROSP', 'XTYPE',
     *   'YTYPE', 'XPARM', 'YPARM', 'GAIN', 'FLUX', 'MINPATCH', 'NITER',
     *   'BCOMP', 'BMAJ', 'BMIN', 'BPA', 'PHAT', 'FACTOR', 'CMETHOD',
     *   'CPARM'/
C-----------------------------------------------------------------------
C                                       How many fields
      CALL OGET ('Input', 'NFIELD', TYPE, DIM, NFIELD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       CLEAN names
      CALL OGET (CLEAN, 'CLEANI', TYPE, DIM, DUMMY, CLEANO, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Loop over cleans
      DO 100 I = 1,NFIELD
C                                        Copy old history
         CALL OHCOPY (UVDATA, CLEANO(I), IERR)
         IF (IERR.NE.0) GO TO 990
C                                        New additions - copy adverb
C                                       values.
         CALL OHLIST ('Input', LIST, NADV, CLEANO(I), IERR)
         IF (IERR.NE.0) GO TO 990
 100     CONTINUE
C                                       Beam
      CALL OGET (CLEAN, 'DIRTBEAM', TYPE, DIM, DUMMY, BEAM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Old history
      CALL OHCOPY (UVDATA, BEAM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        New additions - copy adverb
C                                       values.
      CALL OHLIST ('Input', LIST, NADV, BEAM, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // CLEANO(I)
      CALL MSGWRT (4)
C
 999  RETURN
      END
      SUBROUTINE WINDF (WIN, NBOXES, NFIELD, IMSIZE, FLDSIZ, INFILE,
     *   IERR)
C-----------------------------------------------------------------------
C   Fills the WIN array with clean box definitions taken from INFILE
C   Inputs:
C      INFILE   C*48        User provided file name containing box defs
C      NFIELD   I           Number of fields defined
C   Outputs:
C      WIN      I*4*MXNBOX*MAXFLD   Array containing clean boxes
C      NBOXES   I*MAXFLD    Array containing number of boxes/field
C      IERR     I           Error return code:
C                              0 => no error
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLN.INC'
      CHARACTER INFILE*48
      INTEGER   NFIELD, WIN(4,NFIELD,*), NBOXES(MAXFLD)
      INTEGER   IMSIZE(2,MAXFLD), FLDSIZ(2,MAXFLD)
      INTEGER   IERR, LUN, I, J, ISUBF, I1, I2
      INTEGER   EDGEX, EDGEY, IFIELD, IBLCX, IBLCY, ITRCX, ITRCY, LIMIT
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      LIMIT = MIN (MXNBOX, MXNBFL/NFIELD)
C                                        Open clean box file
      LUN = 78
      CALL CBOPEN (LUN, INFILE, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'WINDF: Error opening clean box file'
         CALL MSGWRT (6)
         GO TO 999
         END IF

C                                        Similar default conditions , i.e.
C                                        define "overall" box for each
C                                        field
      I = 4 * NFIELD * LIMIT
      CALL FILL (I, 0, WIN)
      DO 10 I = 1,NFIELD
         NBOXES(I) = 1
         EDGEX = IMSIZE(1,I) - FLDSIZ(1,I)
         EDGEY = IMSIZE(2,I) - FLDSIZ(2,I)
         WIN(1,I,1) = EDGEX / 2
         WIN(2,I,1) = EDGEY / 2
         WIN(3,I,1) = IMSIZE(1,I) - EDGEX / 2
         WIN(4,I,1) = IMSIZE(2,I) - EDGEY / 2
 10      CONTINUE
C                                        Enter box parameters from file
      DO 200 I = 1,NFIELD
         REWIND (LUN)
         ISUBF = 0
         DO 100 J = 1,50000
            READ (LUN,*,END=110,ERR=110) IFIELD, IBLCX, IBLCY, ITRCX,
     *         ITRCY
            IF ((IFIELD.EQ.I) .AND. (ISUBF.LT.LIMIT)) THEN
               ISUBF = ISUBF + 1
               WIN(1,I,ISUBF) = IBLCX
               WIN(2,I,ISUBF) = IBLCY
               WIN(3,I,ISUBF) = ITRCX
               WIN(4,I,ISUBF) = ITRCY
               END IF
 100        CONTINUE
 110     NBOXES(I) = MAX(ISUBF,1)
 200     CONTINUE
C
      MSGTXT = 'WINDF: Number of clean boxes/field read as follows:'
      CALL MSGWRT(3)
      I1 = 1
 210  I2 = MIN (I1+3, NFIELD)
         WRITE (MSGTXT,1000) (J, NBOXES(J), J = I1,I2)
         CALL MSGWRT(3)
         IF (I2.LT.NFIELD) GO TO 210
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (4('FLD',I3.2,':',I5,3X))
      END
      SUBROUTINE CBOPEN (LUN, INFILE, IERR)
C-----------------------------------------------------------------------
C   Opens the clean box information file.
C
C   Inputs:
C      LUN      I           Logical unit number
C      INFILE   C*48        User provided file name
C   Outputs:
C      IERR     I           Error return code:
C                              0 => no error
C                              1 => error
C
C   Generic version.
C-----------------------------------------------------------------------
      INTEGER   LUN
      CHARACTER INFILE*48
      INTEGER   IERR
      INTEGER   XLNB, ITRIM, IOSVAL
      LOGICAL   EXISTS
      CHARACTER FILSPC*256
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Assume success.
      IERR = 0
      EXISTS = .FALSE.
C                                       Determine full name
      CALL ZFULLN (INFILE, ' ', ' ', FILSPC, IERR)
      IF (IERR.NE.0) THEN
         IERR = 4
         GO TO 999
      END IF
      XLNB = ITRIM (FILSPC)
      WRITE (MSGTXT,1000)
      CALL MSGWRT (2)
      WRITE (MSGTXT,1001) FILSPC(1:MIN (56, XLNB))
      CALL MSGWRT (2)
      IF (XLNB.GT.56) THEN
         WRITE (MSGTXT,1001) FILSPC(57:XLNB)
         CALL MSGWRT (2)
      END IF
C                                       Check for existence.
      INQUIRE (FILE=FILSPC(1:XLNB), EXIST=EXISTS)
C                                       Found it?
      IF (.NOT.EXISTS) THEN
C                                       Can't find it.
         IERR = 1
         WRITE (MSGTXT,1030)
         CALL MSGWRT (6)
         WRITE (MSGTXT,1031) FILSPC(1:XLNB)
         CALL MSGWRT (6)
      ELSE
C                                       Found it, now open it.
         OPEN (UNIT=LUN, FILE=FILSPC(1:XLNB), STATUS='OLD',
     .         IOSTAT=IOSVAL)
C                                       OPEN error?
         IF (IOSVAL.NE.0) THEN
            IERR = 1
            WRITE (MSGTXT,1040) IOSVAL
            CALL MSGWRT (6)
            WRITE (MSGTXT,1031) FILSPC(1:XLNB)
            CALL MSGWRT (6)
         ELSE
C                                       Rewind.
            REWIND (LUN)
         END IF
      END IF
C                                       Exit.
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CBOPEN: Opening CLEAN box file =')
 1001 FORMAT ('CBOPEN: ',A)
 1030 FORMAT ('CBOPEN: UNABLE TO FIND FILE = ')
 1031 FORMAT ('CBOPEN: ',A48)
 1040 FORMAT ('CBOPEN: FORTRAN OPEN ERROR = ',I6,' FOR FILE =')
      END
