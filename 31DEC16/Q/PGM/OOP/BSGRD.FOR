LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NPARMS
      PARAMETER (NPARMS=29)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                      1        2         3          4
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
C           5           6          7          8          9
     *   'TIMERANG', 'DOCALIB', 'GAINUSE', 'FLAGVER', 'STOKES',
C           10         11          12        13         14        15
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK', 'OPTYPE', 'APARM',
C           16        17          18        19       20         21
     *   'IMSIZE', 'CELLSIZE', 'ROTATE', 'SHIFT', 'REWEIGHT', 'XTYPE',
C           22       23       24       25        26        27
     *   'YTYPE', 'XPARM', 'YPARM', 'FACTOR', 'NCOUNT', 'DPARM',
C           28
     *   'DOCAT', 'BADDISK'/
C                    1       2       3       4
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT,
C          5       6       7       8       9
     *   OOARE,  OOALOG, OOAINT, OOAINT, OOACAR,
C          10      11      12      13      14      15
     *   OOACAR, OOACAR, OOAINT, OOAINT, OOACAR, OOARE,
C          16      17      18      19      20      21
     *   OOAINT, OOARE,  OOARE,  OOARE,  OOARE,  OOAINT,
C          22      23      24      25      26      27
     *   OOAINT, OOARE,  OOARE,  OOARE,  OOAINT, OOARE,
C          28
     *   OOALOG, OOAINT/
C                   1     2     3     4
      DATA AVDIM /12,1,  6,1,  1,1,  1,1,
C         5     6     7     8     9
     *   8,1,  1,1,  1,1,  1,1,  4,1,
C         10    11    12    13    14    15
     *   12,1, 6,1,  1,1,  1,1,  4,1, 10,1,
C         16    17    18    19    20    21
     *   2,1,  2,1,  1,1,  2,1,  2,1,  1,1,
C         22    23    24    25    26    27   28    29
     *   1,1, 10,1, 10,1,  1,1,  1,1, 10,1, 1,1, 10,1/
LOCAL END
LOCAL INCLUDE 'BSGRD.INC'
      INTEGER   CTYPX, CTYPY, CTYPX2, CTYPY2
      REAL      XPARM(10), YPARM(10), XPARM2(10), YPARM2(10)
      COMMON /BSGRID/ CTYPX, CTYPY, CTYPX2, CTYPY2, XPARM, YPARM,
     *   XPARM2, YPARM2
LOCAL END
      PROGRAM BSGRD
C-----------------------------------------------------------------------
C! Singledish beam-switched continuum imaging
C# Task AP Imaging OOP SINGLEDISH
C-----------------------------------------------------------------------
C;  Copyright (C) 1997-2000, 2015
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
      CHARACTER PRGM*6, BSIMAG*32, UVDATA(2)*32, SDTEMP(2)*32
      INTEGER  IRET, BUFF1(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'BSGRD'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL BSGRIN (PRGM, UVDATA, SDTEMP, BSIMAG, IRET)
C                                       grid
      IF (IRET.EQ.0) CALL BSGRIT (UVDATA, SDTEMP, BSIMAG, IRET)
C                                       History
      IF (IRET.EQ.0) CALL BSGRHI (UVDATA(1), BSIMAG)
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE BSGRIN (PRGN, UVDATA, SDTEMP, BSIMAG, IRET)
C-----------------------------------------------------------------------
C   BSGRIN gets input parameters for BSGRD and creates the needed
C   objects.
C   Inputs:
C      PRGN     C*6    Program name
C   Output:
C      BSIMAG   C*32   Name of IMAGE object (contains output objects)
C      UVDATA   C*32   Name of input uv data.
C      IRET     I      Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, BSIMAG*(*), UVDATA(2)*(*), SDTEMP(2)*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NKEY1, NKEY2, NKEY3
C                                       NKEY1=no. adverbs to copy to
C                                       UVDATA object
      PARAMETER (NKEY1=13)
C                                       NKEY2 = no. adverb for SD image
      PARAMETER (NKEY2=6)
C                                       NKEY3 = # adverb for SD grid
      PARAMETER (NKEY3=6)
      INCLUDE 'INPUT.INC'
      INTEGER   DIM(7), TYPE, BCHAN, ECHAN, NAXIS(7), IMSI(2), IWT,
     *   DUMMY, BIF, EIF, FQINDX, IDDEG
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, INK3(NKEY3)*8, OUTK3(NKEY3)*32, STOKES*4,
     *   UVTYPE*2, CHTYPE*4, TNAME*12, TCLASS*6, CDUMMY*1, CNAME*8,
     *   KEYW*8
      REAL      APARM(10), RWT(2)
      DOUBLE PRECISION COORD(2)
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'BSGRD.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs for UVDATA object
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'OUTNAME',
     *   'OUTCLASS', 'OUTDISK', 'OUTSEQ', 'STOKES', 'DOCALIB',
     *   'GAINUSE', 'FLAGVER', 'TIMERANG'/
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'OUTNAME',
     *   'OUTCLASS', 'OUTDISK', 'OUTSEQ', 'CALEDIT.STOKES',
     *   'CALEDIT.DOCAL', 'CALEDIT.CLUSE', 'CALEDIT.FGVER',
     *   'CALEDIT.TIMRNG'/
C                                       Adverbs for BSIMAG image object
      DATA INK2 /'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK', 'CELLSIZE',
     *   'SHIFT'/
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'CELLSIZE',
     *   'SHIFT'/
C                                       Adverbs for +- image objects
      DATA INK3 /'OUTNAME', 'OUTSEQ', 'OUTDISK', 'CELLSIZE', 'XPARM',
     *   'YPARM'/
      DATA OUTK3 /'NAME', 'IMSEQ', 'DISK', 'CELLSIZE', 'XPARM', 'YPARM'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BADDISK
      CALL OGET ('Input', 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Grid types
      CALL OGET ('Input', 'XTYPE', TYPE, DIM, CTYPX, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'YTYPE', TYPE, DIM, CTYPY, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'XPARM', TYPE, DIM, XPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'YPARM', TYPE, DIM, YPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CTYPX2 = CTYPX/100
      CTYPY2 = CTYPY/100
      CTYPX = MOD (CTYPX, 100)
      CTYPY = MOD (CTYPY, 100)
      IF (CTYPX2.EQ.0) CTYPX2 = CTYPX
      IF (CTYPY2.EQ.0) CTYPY2 = CTYPY
      CALL RCOPY (10, XPARM, XPARM2)
      CALL RCOPY (10, YPARM, YPARM2)
C                                       Declare 'BSTHROW' a header
C                                       keyword for the image class.
C                                       Also TOTHROW as header word
      CNAME = 'IMAGE'
      KEYW = 'BSTHROW'
      CALL OBVHKW (CNAME, KEYW, OOARE, IRET)
      IF (IRET.NE.0) GO TO 999
      KEYW = 'TOTHROW'
      CALL OBVHKW (CNAME, KEYW, OOARE, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Default output Name = input
      CALL OGET ('Input', 'OUTNAME', TYPE, DIM, DUMMY, TNAME, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (TNAME.EQ.' ') THEN
         CALL OGET ('Input', 'INNAME', TYPE, DIM, DUMMY, TNAME, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT ('Input', 'OUTNAME', TYPE, DIM, DUMMY, TNAME, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Default output Class = TSKNAM
      CALL OGET ('Input', 'OUTCLASS', TYPE, DIM, DUMMY, TCLASS, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (TCLASS.EQ.' ') THEN
         TCLASS = TSKNAM
         CALL OPUT ('Input', 'OUTCLASS', TYPE, DIM, DUMMY, TCLASS, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Set default stokes.
      CALL OGET ('Input', 'STOKES', TYPE, DIM, DUMMY, STOKES, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (STOKES .EQ. ' ') STOKES = 'I'
      CALL OPUT ('Input', 'STOKES', TYPE, DIM, DUMMY, STOKES, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create BSIMAG object
      BSIMAG = 'BSIMAG process object'
      CALL CREATE (BSIMAG, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, BSIMAG, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (BSIMAG, 'CTYPX', OOAINT, DIM, CTYPX2, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (BSIMAG, 'CTYPY', OOAINT, DIM, CTYPY2, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 10
      DIM(2) = 1
      IF (CTYPX2.NE.CTYPX) CALL RFILL (10, 0.0, XPARM2)
      IF (CTYPY2.NE.CTYPY) CALL RFILL (10, 0.0, YPARM2)
      CALL OPUT (BSIMAG, 'XPARM', OOARE, DIM, XPARM2, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (BSIMAG, 'YPARM', OOARE, DIM, YPARM2, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create Plus grid object
      SDTEMP(1) = 'Plus throw image'
      CALL CREATE (SDTEMP(1), 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create Plus grid object
      SDTEMP(2) = 'Minus throw image'
      CALL CREATE (SDTEMP(2), 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY3, INK3, OUTK3, SDTEMP(1), IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IN2OBJ ('Input', NKEY3, INK3, OUTK3, SDTEMP(2), IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (SDTEMP(1), 'CTYPX', OOAINT, DIM, CTYPX, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDTEMP(1), 'CTYPY', OOAINT, DIM, CTYPY, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDTEMP(2), 'CTYPX', OOAINT, DIM, CTYPX, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDTEMP(2), 'CTYPY', OOAINT, DIM, CTYPY, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create other objects
C                                       UVDATA: plus
      UVDATA(1) = 'Plus throw UVdata'
      CALL CREATE (UVDATA(1), 'UVDATA', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, UVDATA(1), IRET)
      IF (IRET.NE.0) GO TO 999
C                                       UVDATA: minus
      UVDATA(2) = 'Minus throw UVdata'
      CALL CREATE (UVDATA(2), 'UVDATA', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, UVDATA(2), IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Fix class name
      CALL OGET ('Input', 'INCLASS', TYPE, DIM, DUMMY, TCLASS, IRET)
      IF (IRET.NE.0) GO TO 999
      TCLASS(6:6) = '+'
      CALL OPUT (UVDATA(1), 'CLASS', TYPE, DIM, DUMMY, TCLASS, IRET)
      IF (IRET.NE.0) GO TO 999
      TCLASS(6:6) = '-'
      CALL OPUT (UVDATA(2), 'CLASS', TYPE, DIM, DUMMY, TCLASS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get IF channel selection
      CALL SECSLT (UVDATA(1), BIF, EIF, BCHAN, ECHAN, STOKES, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       All Stokes', IF, channels
      CALL SECSAV (UVDATA(1), 1, 0, 1, 0, '    ', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL SECSAV (UVDATA(2), 1, 0, 1, 0, '    ', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open UVDATA to be sure it's OK.
      CALL OOPEN (UVDATA(1), 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get type of data
      CALL UVDGET (UVDATA(1), 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (UVTYPE.NE.'SB') THEN
         MSGTXT = 'I ONLY WORK ON SB DATA NOT ''' // UVTYPE // ''''
         CALL MSGWRT (8)
         IRET = 8
         GO TO 999
         END IF
      CALL OCLOSE (UVDATA(1), IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OOPEN (UVDATA(2), 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDGET (UVDATA(2), 'TYPEUVD', TYPE, DIM, DUMMY, UVTYPE, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (UVTYPE.NE.'SB') THEN
         MSGTXT = 'I ONLY WORK ON SB DATA NOT ''' // UVTYPE // ''''
         CALL MSGWRT (8)
         IRET = 8
         GO TO 999
         END IF
      CALL OCLOSE (UVDATA(2), IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Reset Selection: one IF
      EIF = BIF
      CALL SECSAV (UVDATA(1), BIF, EIF, BCHAN, ECHAN, STOKES, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL SECSAV (UVDATA(2), BIF, EIF, BCHAN, ECHAN, STOKES, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Attach UVDATA to image
      DIM(1) = LEN (UVDATA(1))
      DIM(2) = 1
      CALL OPUT (SDTEMP(1), 'UVDATA', OOACAR, DIM, DUMMY, UVDATA(1),
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDTEMP(2), 'UVDATA', OOACAR, DIM, DUMMY, UVDATA(2),
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Min. image sizes
      CALL OGET ('Input', 'IMSIZE', TYPE, DIM, IMSI, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       No images smaller than 32
C                                       and must be even
      IMSI(1) = MAX (IMSI(1), 32)
      IMSI(2) = MAX (IMSI(2), 32)
      IMSI(1) = ((IMSI(1) + 1) / 2) * 2
      IMSI(2) = ((IMSI(2) + 1) / 2) * 2
      CALL OPUT ('Input', 'IMSIZE', OOAINT, DIM, IMSI, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (BSIMAG, 'IMSIZE', OOAINT, DIM, IMSI, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDTEMP(1), 'IMSIZE', OOAINT, DIM, IMSI, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDTEMP(2), 'IMSIZE', OOAINT, DIM, IMSI, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Coordinates
      CALL OGET ('Input', 'APARM', TYPE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      COORD(1) = (APARM(1) + APARM(2)/60. + APARM(3)/3600.) * 15.0
      IDDEG = ABS (APARM(4))
      COORD(2) = (IDDEG + APARM(5)/60. + (APARM(6)/3600.)) *
     *   SIGN (1.0, APARM(4))
      DIM(1) = 2
      CALL OPUT (BSIMAG, 'CCENTER', OOADP, DIM, COORD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'OPTYPE', TYPE, DIM, DUMMY, CHTYPE, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (CHTYPE.EQ.' ') CHTYPE = '-SIN'
      CALL OPUT (BSIMAG, 'CPROJ', OOACAR, DIM, DUMMY, CHTYPE, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDTEMP(1), 'CPROJ', OOACAR, DIM, DUMMY, CHTYPE, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDTEMP(2), 'CPROJ', OOACAR, DIM, DUMMY, CHTYPE, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Image type, cutoff
      CALL OGET ('Input', 'REWEIGHT', TYPE, DIM, RWT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IWT = -1
      IF (RWT(2).EQ.0.0) RWT(2) = -0.01
      RWT(1) = MIN (0.05, RWT(2))
      DIM(1) = 1
C                                        SD type, cutoff
      CALL OPUT (BSIMAG, 'SDCUTOFF', OOARE, DIM, RWT(1), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDTEMP(1), 'SDTYPE', OOAINT, DIM, IWT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDTEMP(1), 'SDCUTOFF', OOARE, DIM, RWT(2), CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDTEMP(2), 'SDTYPE', OOAINT, DIM, IWT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDTEMP(2), 'SDCUTOFF', OOARE, DIM, RWT(2), CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Default BCHAN, ECHAN
C                                       Find size of freq. axis.
      CALL UVDFND (UVDATA(1), 2, 'FREQ', FQINDX, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (UVDATA(1), 'UV_DESC.NAXIS', TYPE, DIM, NAXIS, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      BCHAN = 1
      ECHAN = NAXIS(FQINDX)
      DIM(1) = 1
      CALL OPUT (SDTEMP(1), 'BCHAN', OOAINT, DIM, BCHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDTEMP(1), 'ECHAN', OOAINT, DIM, ECHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDTEMP(2), 'BCHAN', OOAINT, DIM, BCHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDTEMP(2), 'ECHAN', OOAINT, DIM, ECHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       two were for coordinates
      ECHAN = NAXIS(FQINDX) - 2
      CALL OPUT (BSIMAG, 'BCHAN', OOAINT, DIM, BCHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (BSIMAG, 'ECHAN', OOAINT, DIM, ECHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT ('Input', 'BCHAN', OOAINT, DIM, BCHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT ('Input', 'ECHAN', OOAINT, DIM, ECHAN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       IF numbers
      BIF = 1
      EIF = 1
C                                       Save in Inputs for history
      DIM(1) = 1
      CALL OPUT ('Input', 'BIF', OOAINT, DIM, BIF, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT ('Input', 'EIF', OOAINT, DIM, EIF, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       fix temp file names
      DIM(1) = LEN (TCLASS)
      DIM(2) = 1
      TCLASS = 'BSGRD+'
      CALL OPUT (SDTEMP(1), 'CLASS', OOACAR, DIM, DUMMY, TCLASS, IRET)
      IF (IRET.NE.0) GO TO 999
      TCLASS = 'BSGRD-'
      CALL OPUT (SDTEMP(2), 'CLASS', OOACAR, DIM, DUMMY, TCLASS, IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE BSGRIT (UVDATA, SDTEMP, BSIMAG, IRET)
C-----------------------------------------------------------------------
C   Routine to call OUVIMG to make image
C   Inputs:
C      UVDATA   C*?   Input UV data
C      BSIMAG   C*?   Image process object
C   Output:
C      IRET     I     > 0 => die
C-----------------------------------------------------------------------
      CHARACTER BSIMAG*(*), UVDATA(2)*(*), SDTEMP(2)*(*)
      INTEGER   IRET
C
      CHARACTER ROTEMP*32, CDUMMY*1, NAME*12, CLASS*6, KEYW*8, CNAME*8
      INTEGER   DIM(7), TYPE, DUMMY, I, DISK, HWIDTH, ORDER, WIN(4),
     *   IROUND, IMSIZE(2), IDISK, ICNO, ODISK, OCNO, BUFFER(512), IMSEQ
      REAL      ROTATE, SHIFT(2), CROTA(7), DPARM(10), FACTOR, MAXCWT,
     *   XNLIM, BMJ, BMN, BPA, THROWP, THROWM
      LOGICAL   DOCAT
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'BSGRD.INC'
C-----------------------------------------------------------------------
      CALL OGET ('Input', 'OUTSEQ', TYPE, DIM, IMSEQ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'DOCAT', TYPE, DIM, DOCAT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'ROTATE', TYPE, DIM, ROTATE, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (ROTATE.GT.0) THEN
C                                       Declare 'BSTHROW' a header
C                                       keyword for the uvdata class
         CNAME = 'UVDATA'
         KEYW = 'BSTHROW'
         CALL OBVHKW (CNAME, KEYW, OOARE, IRET)
         CALL OGET (UVDATA(1), 'BSTHROW', TYPE, DIM, THROWP, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         SHIFT(1) = -THROWP * (1.0 - 1.0 / COS (ROTATE * DG2RAD))
         SHIFT(2) = -THROWP * SIN (ROTATE * DG2RAD)
         DIM(1) = 2
         DIM(2) = 1
         CALL OPUT (SDTEMP(1), 'SHIFT', OOARE, DIM, SHIFT, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      CALL OSDIMG (UVDATA(1), SDTEMP(1), IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (SDTEMP(1), 'XPARM', TYPE, DIM, XPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (SDTEMP(1), 'YPARM', TYPE, DIM, YPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (ROTATE.GT.0) THEN
         CALL OGET (UVDATA(2), 'BSTHROW', TYPE, DIM, THROWM, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         SHIFT(1) = -THROWM * (1.0 - 1.0 / COS (ROTATE * DG2RAD))
         SHIFT(2) = -THROWM * SIN (ROTATE * DG2RAD)
         DIM(1) = 2
         DIM(2) = 1
         CALL OPUT (SDTEMP(2), 'SHIFT', OOARE, DIM, SHIFT, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      CALL OSDIMG (UVDATA(2), SDTEMP(2), IRET)
      IF (IRET.NE.0) GO TO 999
C                                       save for final output image
      CALL OGET (SDTEMP(1), 'MAXCWT', TYPE, DIM, MAXCWT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (SDTEMP(1), 'XNLIM', TYPE, DIM, XNLIM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (SDTEMP(1), 'BSTHROW', TYPE, DIM, THROWP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (SDTEMP(2), 'BSTHROW', TYPE, DIM, THROWM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDGET (UVDATA(1), 'BMAJ', TYPE, DIM, BMJ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDGET (UVDATA(1), 'BMIN', TYPE, DIM, BMN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL UVDGET (UVDATA(1), 'BPA', TYPE, DIM, BPA, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       rotate
      CALL OGET ('Input', 'ROTATE', TYPE, DIM, ROTATE, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (ROTATE.LT.0) THEN
         SHIFT(1) = THROWP * (1.0 - 1.0 / COS (ROTATE * DG2RAD))
         SHIFT(2) = THROWP * SIN (ROTATE * DG2RAD)
         ROTATE = 0.0
         HWIDTH = 1
         ROTEMP = 'Plus image rotated'
         DO 20 I = 1,2
            CALL CREATE (ROTEMP, 'IMAGE', IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OGET (SDTEMP(I), 'NAME', TYPE, DIM, DUMMY, NAME, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OPUT (SDTEMP(I), 'OUTNAME', TYPE, DIM, DUMMY, NAME,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OGET (SDTEMP(I), 'CLASS', TYPE, DIM, DUMMY, CLASS,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            CLASS(1:5) = 'BSROT'
            CALL OPUT (SDTEMP(I), 'OUTCLASS', TYPE, DIM, DUMMY, CLASS,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OGET (SDTEMP(I), 'DISK', TYPE, DIM, DISK, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OPUT (SDTEMP(I), 'OUTDISK', TYPE, DIM, DISK, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OPUT (SDTEMP(I), 'OUTSEQ', TYPE, DIM, IMSEQ, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            CALL IMGCLN (SDTEMP(I), ROTEMP, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OGET (SDTEMP(I), 'IMAGE_DESC.CROTA', TYPE, DIM, CROTA,
     *         CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
            CROTA(2) = CROTA(2) + ROTATE
            CALL OPUT (ROTEMP, 'IMAGE_DESC.CROTA', TYPE, DIM, CROTA,
     *         CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Interpolate
            CALL IMGINT (SDTEMP(I), SHIFT, ROTATE, HWIDTH, ROTEMP, IRET)
            IF (IRET.NE.0) GO TO 999
            SHIFT(1) = THROWM * SHIFT(1) / THROWP
            SHIFT(2) = THROWM * SHIFT(2) / THROWP
C                                       Copy catalog header keywords.
            CALL OBDSKC (SDTEMP(I), IDISK, ICNO, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OBDSKC (ROTEMP, ODISK, OCNO, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL KEYCOP (IDISK, ICNO, ODISK, OCNO, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       kill the old one
            IF (.NOT.DOCAT) THEN
               CALL IMGZAP (SDTEMP(I), IRET)
               IF (IRET.NE.0) GO TO 999
C                                       Try to move in the beam parms
            ELSE
               CALL BEMCOP (SDTEMP(I), ROTEMP, IRET)
               IF (IRET.NE.0) GO TO 999
               CALL OCLOSE (ROTEMP, IRET)
               END IF
            SDTEMP(I) = ROTEMP
            ROTEMP = 'Minus image rotated'
 20         CONTINUE
         END IF
C                                       Correction step
      ROTEMP = 'Corrected Az-El image'
      CALL CREATE (ROTEMP, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (SDTEMP(1), 'NAME', TYPE, DIM, DUMMY, NAME, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDTEMP(1), 'OUTNAME', TYPE, DIM, DUMMY, NAME, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (SDTEMP(1), 'CLASS', TYPE, DIM, DUMMY, CLASS, IRET)
      IF (IRET.NE.0) GO TO 999
      CLASS = 'BSGCOR'
      CALL OPUT (SDTEMP(1), 'OUTCLASS', TYPE, DIM, DUMMY, CLASS, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (SDTEMP(1), 'DISK', TYPE, DIM, DISK, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDTEMP(1), 'OUTDISK', TYPE, DIM, DISK, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SDTEMP(1), 'OUTSEQ', TYPE, DIM, IMSEQ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IMGCLN (SDTEMP(1), ROTEMP, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create
      CALL OOPEN (ROTEMP, 'DEST', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy beam parameters
      CALL BEMCOP (SDTEMP(1), ROTEMP, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Total throw
      DIM(1) = 1
      THROWP = THROWP - THROWM
      CALL OPUT (ROTEMP, 'TOTHROW', OOARE, DIM, THROWP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (ROTEMP, IRET)
C                                       Move adverbs from input
      CALL OGET ('Input', 'IMSIZE', TYPE, DIM, IMSIZE, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'FACTOR', TYPE, DIM, FACTOR, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (FACTOR.LE.0.0) FACTOR = 1.0
      CALL OPUT ('Input', 'FACTOR', TYPE, DIM, FACTOR, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'NCOUNT', TYPE, DIM, ORDER, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      ORDER = MAX (0, MIN (1, ORDER))
      CALL OPUT ('Input', 'NCOUNT', TYPE, DIM, ORDER, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'DPARM', TYPE, DIM, DPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (DPARM(1).LE.0.0) DPARM(1) = 1.0
      IF (DPARM(2).GT.0.0) THEN
         DPARM(2) = DPARM(1)
         DPARM(1) = 1.0
      ELSE
         DPARM(2) = 1.0
         END IF
      WIN(1) = IROUND (DPARM(3))
      WIN(2) = IROUND (DPARM(4))
      WIN(3) = IROUND (DPARM(5))
      WIN(4) = IROUND (DPARM(6))
      WIN(1) = MAX (1, WIN(1))
      IF (WIN(2).LT.WIN(1)) WIN(2) = IMSIZE(1)
      IF (WIN(4).LE.0) WIN(4) = IMSIZE(1)
      IF (WIN(3).LE.0) WIN(3) = IMSIZE(1) * 10
      DPARM(3) = WIN(1)
      DPARM(4) = WIN(2)
      DPARM(5) = WIN(3)
      DPARM(6) = WIN(4)
      CALL OPUT ('Input', 'DPARM', TYPE, DIM, DPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DO 30 I = 1,2
         DIM(1) = 1
         DIM(2) = 1
         CALL OPUT (SDTEMP(I), 'FACTOR', TYPE, DIM, FACTOR, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT (SDTEMP(I), 'BLORDER', OOAINT, DIM, ORDER, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         DIM(1) = 2
         CALL OPUT (SDTEMP(I), 'REWEIGHT', OOAINT, DIM, DPARM, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         DIM(1) = 4
         CALL OPUT (SDTEMP(I), 'BLWINDOW', OOAINT, DIM, WIN, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
 30      CONTINUE
C                                       Correct
      CALL IMGBSC (SDTEMP(1), SDTEMP(2), ROTEMP, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Clean up keywords
      CALL OBDSKC (ROTEMP, ODISK, OCNO, IRET)
      IF (IRET.NE.0) GO TO 999
      KEYW = 'BSTHROW'
      CALL REMKEY (ODISK, OCNO, KEYW, 1, BUFFER, IRET)
      IF (IRET.GT.0) GO TO 999
C                                       clean up
      IF (.NOT.DOCAT) THEN
         DO 40 I = 1,2
            CALL IMGZAP (SDTEMP(I), IRET)
            IF (IRET.NE.0) GO TO 999
 40         CONTINUE
         END IF
C                                       Regrid
      CALL IMGRGR (ROTEMP, BSIMAG, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      IF (.NOT.DOCAT) THEN
         CALL IMGZAP (ROTEMP, IRET)
         IF (IRET.NE.0) GO TO 999
      ELSE
         CALL OOPEN (ROTEMP, 'WRIT', IRET)
         IF (IRET.NE.0) GO TO 999
         CALL BEMCOP (SDTEMP(1), ROTEMP, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OCLOSE (ROTEMP, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      CALL OPUT (BSIMAG, 'MAXCWT', OOARE, DIM, MAXCWT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (BSIMAG, 'XNLIM', OOARE, DIM, XNLIM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (BSIMAG, 'BEAM.BMAJ', OOARE, DIM, BMJ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (BSIMAG, 'BEAM.BMIN', OOARE, DIM, BMN, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (BSIMAG, 'BEAM.BPA', OOARE, DIM, BPA, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (BSIMAG, IRET)
C
 999  RETURN
      END
      SUBROUTINE BSGRHI (UVDATA, BSIMAG)
C-----------------------------------------------------------------------
C   Routine to write history file to output BSIMAG image object.
C   Inputs:
C      UVDATA   C*?   Input UV data
C      BSIMAG   C*?   Image process object
C-----------------------------------------------------------------------
      CHARACTER BSIMAG*(*), UVDATA*32
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NADV0
      PARAMETER (NADV0=26)
      CHARACTER LIST(NADV0)*8, CDUMMY*1, CPROJ*4, LINE*64
      INTEGER   IERR, TYPE, DIM(7), I, DUMMY, MSGSAV
      REAL      PARM(10), MAXCWT, XNLIM, RWT
      DOUBLE PRECISION COORD(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'BSGRD.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'TIMERANG', 'DOCALIB',
     *   'GAINUSE', 'FLAGVER', 'STOKES', 'BCHAN', 'ECHAN', 'BIF',
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'IMSIZE', 'CELLSIZE', 'XTYPE',
     *   'YTYPE', 'XPARM', 'YPARM', 'OPTYPE', 'APARM', 'SHIFT',
     *   'FACTOR', 'ROTATE', 'DPARM'/
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
C                                       Move adverbs to Input
      CALL OGET (BSIMAG, 'XPARM', TYPE, DIM, XPARM2, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT ('Input', 'XPARM', TYPE, DIM, XPARM2, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (BSIMAG, 'YPARM', TYPE, DIM, YPARM2, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT ('Input', 'YPARM', TYPE, DIM, YPARM2, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (BSIMAG, 'CTYPX', TYPE, DIM, I, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT ('Input', 'XTYPE', TYPE, DIM, I, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (BSIMAG, 'CTYPY', TYPE, DIM, I, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT ('Input', 'YTYPE', TYPE, DIM, I, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (BSIMAG, 'CPROJ', TYPE, DIM, DUMMY, CPROJ, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT ('Input', 'OPTYPE', TYPE, DIM, DUMMY, CPROJ, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (BSIMAG, 'CCENTER', TYPE, DIM, COORD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      COORD(1) = COORD(1) / 15.0D0
      I = COORD(1)
      PARM(1) = I
      COORD(1) = 60.0D0 * (COORD(1) - I)
      I = COORD(1)
      PARM(2) = I
      PARM(3) = 60.0D0 * (COORD(1) - I)
      DUMMY = 1
      IF (COORD(2).LT.0) DUMMY = -1
      COORD(2) = ABS (COORD(2))
      I = COORD(2)
      PARM(4) = I * DUMMY
      COORD(2) = 60.0D0 * (COORD(2) - I)
      I = COORD(2)
      PARM(5) = I
      PARM(6) = 60.0D0 * (COORD(2) - I)
      IF ((DUMMY.LT.0) .AND. (PARM(4).EQ.0.0)) PARM(4) = -1.0E-10
      DIM(1) = 10
      DIM(2) = 1
      CALL OPUT ('Input', 'APARM', OOARE, DIM, PARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy old history
      CALL OHCOPY (UVDATA, BSIMAG, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy base adverb values.
      CALL OHLIST ('Input', LIST, NADV0, BSIMAG, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       First grid type:
      IF ((CTYPX2.NE.CTYPX) .OR. (CTYPY2.NE.CTYPY)) THEN
         LINE = '/ Convolution function for Az-El'
         CALL OHWRIT (LINE, BSIMAG, IERR)
         IF (IERR.NE.0) GO TO 990
         WRITE (LINE,1000) 'X', CTYPX
         CALL OHWRIT (LINE, BSIMAG, IERR)
         IF (IERR.NE.0) GO TO 990
         WRITE (LINE,1001) 'X', (XPARM(I), I = 1,5)
         CALL OHWRIT (LINE, BSIMAG, IERR)
         IF (IERR.NE.0) GO TO 990
         WRITE (LINE,1000) 'Y', CTYPY
         CALL OHWRIT (LINE, BSIMAG, IERR)
         IF (IERR.NE.0) GO TO 990
         WRITE (LINE,1001) 'Y', (YPARM(I), I = 1,5)
         CALL OHWRIT (LINE, BSIMAG, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       computed results
      CALL OGET (BSIMAG, 'SDCUTOFF', TYPE, DIM, RWT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (BSIMAG, 'MAXCWT', TYPE, DIM, MAXCWT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (BSIMAG, 'XNLIM', TYPE, DIM, XNLIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (RWT.GT.0.0) THEN
         WRITE (LINE,1010) XNLIM
      ELSE
         WRITE (LINE,1011) XNLIM
         END IF
      CALL OHWRIT (LINE, BSIMAG, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // BSIMAG
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A1,'TYPE  =',I3,'  /convolution type with parms:')
 1001 FORMAT (A1,'PARM  =',4(F8.4,','),F7.1)
 1010 FORMAT ('WEIGHT  =',F9.3,' / Scaled min sum of convolved weights')
 1011 FORMAT ('WEIGHT  =',F9.3,
     *   ' / Scaled min ABS(sum of convolved weights)')
      END
