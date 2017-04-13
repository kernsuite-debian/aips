LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INTEGER NPARMS
      PARAMETER (NPARMS=28)
      INTEGER AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPCOM.INC'
      INTEGER  NANTSL, ANTS(50), NREFSL, BASES(50)
      REAL TAVG, TSKP
      LOGICAL  DOCOMB, DOAWTN, DORMS, NOSCAL
      COMMON /INPP/ NANTSL, ANTS, NREFSL, BASES, TAVG, TSKP,
     *   DOCOMB, DOAWTN, DORMS, NOSCAL
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                                       Uses PAOOF.INC
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'SOURCES', 'QUAL', 'TIMERANG', 'BIF', 'EIF',
     *   'PMODEL', 'DOCALIB', 'GAINUSE', 'DOPOL', 'PDVER', 'BLVER',
     *   'FLAGVER', 'DOBAND', 'BPVER', 'SMOOTH', 'OUTNAME', 'OUTDISK',
     *   'OUTSEQ', 'IMSIZE', 'CELLSIZE',
     *   'APARM', 'ANTENNAS', 'BASELINE',
     *   'BADDISK'/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT,
     *   OOACAR, OOAINT, OOARE, OOAINT, OOAINT,
     *   OOARE, OOARE, OOAINT, OOAINT, OOAINT, OOAINT,
     *   OOAINT, OOAINT, OOAINT, OOARE, OOACAR, OOAINT,
     *   OOAINT, OOAINT, OOARE,
     *   OOARE, OOARE, OOARE,
     *   OOAINT/
      DATA AVDIM /12,1, 6,1, 1,1, 1,1,
     *   16,30, 1,1, 8,1, 1,1, 1,1,
     *   7,1, 1,1, 1,1, 1,1, 1,1, 1,1,
     *   1,1, 1,1, 1,1, 3,1, 12,1, 1,1,
     *   1,1, 2,1, 2,1,
     *   10,1, 50,1, 50,1,
     *   10,1/
LOCAL END
      PROGRAM MAPBM
C-----------------------------------------------------------------------
C! Map VLA beam polarization
C# TASK CALIBRATION POLARIZATION UV Image VLA OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2002-2003, 2006-2007, 2010, 2015
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
C   Map VLA beam polarization
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, UVDATA*32, IMAGE*32
      INTEGER  IRET, BUFF(256)
      INTEGER  KANTSL
      INTEGER  IANT
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPCOM.INC'
      INCLUDE 'INPUTDATA.INC'
      DATA PRGM /'MAPBM '/
C-----------------------------------------------------------------------
C                                       make selection of antenna list
C                                       and reference antenna list
      CALL MPBSL (PRGM, UVDATA, IMAGE, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Cycle over selected antennas
      IF (DOCOMB) THEN
         KANTSL = 1
      ELSE
         KANTSL = NANTSL
         END IF
      DO 100 IANT = 1, KANTSL
         CALL MPBIN (UVDATA, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Average and grid data
         CALL MPBDO (UVDATA, IMAGE, IANT, IRET)
         IF (IRET.NE.0) GO TO 990
  100    CONTINUE
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF)
C
 999  STOP
      END
      SUBROUTINE MPBSL (PRGN, UVDATA, IMAGE, IRET)
C-----------------------------------------------------------------------
C   MPBSL selects antenna and reference antenna list based on ANTENNA
C   and BASELINE
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      UVDATA  C*?  Input uv data object
C      IMAGE   C*?  Output image object
C      IRET    I    Error code: 0 => ok, else failed.
C   Output in common INPP
C      NANTSL  I    Number of selected antennas
C      ANTS(*) I    List of selected antennas
C      NREFSL  I    Number of selected references antennas
C      BASES(*)I    List of selected referenses antennas
C      DOAWTN  L    TRUE if all ANTENNAS are positive
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      CHARACTER UVDATA*(*), IMAGE*(*)
      INTEGER   IRET
      INTEGER  I, K, IROUND
      REAL  XANT(50), XBASE(50), APARM(10)
      LOGICAL   LTEMP
C
      INTEGER   NKEY1, NKEY2
C                                       NKEY1=no. adverbs to copy to
C                                       UVDATA
      PARAMETER (NKEY1=21)
C                                       NKEY2=no. adverbs to copy to
C                                       IMAGE
      PARAMETER (NKEY2=5)
C
      INTEGER   IERR, DIM(7), TYPE, IMSIZE(2), NAXIS(7), TDISK, TCNO,
     *   IDUM, BUFF(256)
      INTEGER  IANTSL, ANTI, ANTK, ANTSS(50)
      REAL     CRPIX(7), CDELT(7), CELLSI(2), XDOCAL
      DOUBLE PRECISION CRVAL(7)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32
      CHARACTER  CLASS*6, INNAME*12, OUTNAM*12, UNITS*8, CTYPE(7)*8,
     *   CNAME*8, KEYW*8, CDUM*1
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPCOM.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INPUTDATA.INC'
C
C                                       Adverbs to copy to uv data
C                    1         2         3         4        5
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'TIMERANG',
C          6       7        8         9        10
     *   'BIF', 'EIF', 'PMODEL', 'GAINUSE', 'DOPOL',
C          11         12       13        14         15          16
     *   'FLAGVER', 'APARM', 'QUAL', 'SOURCES', 'ANTENNAS', 'BASELINE',
     *   'BLVER', 'DOBAND', 'BPVER', 'SMOOTH', 'PDVER'/
C                                       Rename to object
C                  1       2         3       4         5
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'CALEDIT.TIMRNG',
C          6       7        8          9
     *   'BIF', 'EIF', 'PMODEL',  'CALEDIT.CLUSE',
C           10               11               12           13
     *   'CALEDIT.DOPOL', 'CALEDIT.FGVER',  'APARM', 'CALEDIT.SELQUA',
C           14         15          16
     *   'SOUSEL', 'ANTENNAS', 'BASELINE', 'CALEDIT.BLVER',
     *   'CALEDIT.DOBAND', 'CALEDIT.BPVER', 'CALEDIT.SMOOTH',
     *   'CALEDIT.PDVER'/
C                                       Adverbs to copy to IMAGE object
C                      1        2          3         4        5
      DATA INK2 /'OUTNAME', 'OUTDISK', 'OUTSEQ', 'IMSIZE', 'CELLSIZE'/
C                   1        2      3         4        5
      DATA OUTK2 /'NAME', 'DISK', 'IMSEQ', 'IMSIZE', 'CELLSIZE'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BADDISK
      CALL OGET ('Input', 'BADDISK', TYPE, DIM, IBAD, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Declare 'PARANGLE' a header
C                                       keyword for the image class.
      CNAME = 'IMAGE'
      KEYW = 'PARANGLE'
      CALL OBVHKW (CNAME, KEYW, OOARE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Declare 'ZENANGLE' a header
C                                       keyword for the image class.
      CNAME = 'IMAGE'
      KEYW = 'ZENANGLE'
      CALL OBVHKW (CNAME, KEYW, OOARE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Declare 'AZIMUTH' a header
C                                       keyword for the image class.
      CNAME = 'IMAGE'
      KEYW = 'AZIMUTH'
      CALL OBVHKW (CNAME, KEYW, OOARE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Declare SCALI a header
C                                       keyword for the image class.
      CNAME = 'IMAGE'
      KEYW = 'SCALI'
      CALL OBVHKW (CNAME, KEYW, OOARE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Declare SCALQ a header
C                                       keyword for the image class.
      CNAME = 'IMAGE'
      KEYW = 'SCALQ'
      CALL OBVHKW (CNAME, KEYW, OOARE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Declare SCALU a header
C                                       keyword for the image class.
      CNAME = 'IMAGE'
      KEYW = 'SCALU'
      CALL OBVHKW (CNAME, KEYW, OOARE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Declare SCALV a header
C                                       keyword for the image class.
      CNAME = 'IMAGE'
      KEYW = 'SCALV'
      CALL OBVHKW (CNAME, KEYW, OOARE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       default OUTNAME = INNAME
      CALL OGET ('Input', 'INNAME', TYPE, DIM, IDUM, INNAME, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'OUTNAME', TYPE, DIM, IDUM, OUTNAM, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (OUTNAM.EQ.'    ') OUTNAM = INNAME
      CALL OPUT ('Input', 'OUTNAME', TYPE, DIM, IDUM, OUTNAM, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Control parameters
      CALL OGET ('Input', 'APARM', TYPE, DIM, APARM, CDUM, IERR)
C                                       compine the selected antennas
C                                       data?
      DOCOMB = APARM(5) .GT. 0.1
      TAVG = APARM(1)
      TSKP = APARM(2)
      DORMS = APARM(3).GT.0.1
      NOSCAL = APARM(4).GT.0.1
C                                       Antennas
      CALL OGET ('Input', 'ANTENNAS', TYPE, DIM, XANT, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Reference antennas
      CALL OGET ('Input', 'BASELINE', TYPE, DIM, XBASE, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Check antennas desired.
      NANTSL = 0
      DOAWTN = .TRUE.
      DO 20 I = 1,50
         ANTS(I) = IROUND (XANT(I))
         IF (ANTS(I).LT.0) DOAWTN = .FALSE.
C                                       Make positive
         ANTS(I) = ABS (ANTS(I))
         IF (NANTSL.LT.1) GO TO 10
            DO 5 K = 1, NANTSL
               IF (ANTS(I).EQ.ANTS(K)) ANTS(I) = 0
    5          CONTINUE
C                                       Check for multiple entries
 10      IF (ABS (ANTS(I)).GE.1) NANTSL = I
 20      CONTINUE
C                                       Check reference antenna desired
      NREFSL = 0
      DO 40 I = 1,50
         BASES(I) = IROUND (XBASE(I))
         IF (NREFSL.LT.1) GO TO 30
            DO 25 K = 1,NREFSL
               IF (BASES(I).EQ.BASES(K)) BASES(I) = 0
   25          CONTINUE
C                                       Check for multiple entries
 30      IF (BASES(I).GE.1) NREFSL = I
 40      CONTINUE
C                                       Make sure not too many
      IF (NANTSL.GT.MAXANT) NANTSL = MAXANT
      IF (NREFSL.GT.MAXANT) NREFSL = MAXANT
C                                       Reference antenna list must
C                                       be given
      IF (NREFSL.EQ.0) THEN
         IRET = 1
         MSGTXT = 'BASELINE is not allowed to be zero!'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Now prepare reading AN table
C                                       by GETANT for the case
C                                       NANTSL = 0
C                                       Create input uv data object
      UVDATA = 'Input uv data'
      CALL CREATE (UVDATA, 'UVDATA', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999

C                                       DOCALIB
      CALL OGET ('Input', 'DOCALIB', TYPE, DIM, XDOCAL, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
      LTEMP = XDOCAL.GT.0.0
      CALL OPUT (UVDATA, 'CALEDIT.DOCAL', OOALOG, DIM, LTEMP, CDUM,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      LTEMP = (XDOCAL.GT.0.0) .AND. (XDOCAL.LE.99.0)
      CALL OPUT (UVDATA, 'CALEDIT.DOWTCL', OOALOG, DIM, LTEMP, CDUM,
     *   IRET)
      IF (IRET.NE.0) GO TO 999

C                                       I do not know why but the
C                                       following 3 cards about image
C                                       are required to make OBDSKC
C                                       work for GETANT (LK)
      IMAGE = 'Output image'
      CALL CREATE (IMAGE, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, IMAGE, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Add AIPS file class.
      DIM(1) = LEN (CLASS)
      DIM(2) = 1
      DIM(3) = 0
      CLASS = 'QUBEST'
      CALL OPUT (IMAGE, 'CLASS', OOACAR, DIM, IDUM, CLASS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy uv descriptors
      CALL U2IDES (UVDATA, IMAGE, .FALSE., IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Set axis labels
      CALL OGET (IMAGE, 'IMAGE_DESC.CTYPE', TYPE, DIM, IDUM, CTYPE,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      CTYPE(1) = 'AZIMUTH'
      CTYPE(2) = 'ELEVAT'
      CTYPE(3) = 'STOKES'
      CALL OPUT (IMAGE, 'IMAGE_DESC.CTYPE', TYPE, DIM, IDUM, CTYPE,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Set az, el ref. val
      CALL OGET (IMAGE, 'IMAGE_DESC.CRVAL', TYPE, DIM, CRVAL, CDUM,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      CRVAL(1) = 0.0D0
      CRVAL(2) = 0.0D0
      CRVAL(3) = 1.0D0
      CALL OPUT (IMAGE, 'IMAGE_DESC.CRVAL', TYPE, DIM, CRVAL, CDUM,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Set units (ratio)
      UNITS = 'RATIO'
      DIM(1) = LEN (UNITS)
      DIM(2) = 1
      CALL OPUT (IMAGE, 'IMAGE_DESC.BUNIT', OOACAR, DIM, IDUM, UNITS,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Set reference pixel
      CALL OGET ('Input', 'IMSIZE', TYPE, DIM, IMSIZE, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL RFILL (7, 1.0, CRPIX)
      CRPIX(1) = IMSIZE(1) / 2 + 1
      CRPIX(2) = IMSIZE(2) / 2 + 1
      CRPIX(3) = 1
      DIM(1) = 7
      DIM(2) = 1
      CALL IMDPUT (IMAGE, 'CRPIX', OOARE, DIM, CRPIX, CDUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set image size
      CALL FILL (7, 1, NAXIS)
      NAXIS(1) = IMSIZE(1)
      NAXIS(2) = IMSIZE(2)
      NAXIS(3) = 4
      DIM(1) = 7
      DIM(2) = 1
      CALL OPUT (IMAGE, 'NAXIS', OOAINT, DIM, NAXIS, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 1
      CALL OPUT (IMAGE, 'NDIM', OOAINT, DIM, 3, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Set axis increments
      CALL OGET (IMAGE, 'CELLSIZE', TYPE, DIM, CELLSI, CDUM, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL RFILL (7, 1.0, CDELT)
      CDELT(1) = CELLSI(1) / 3600.0
      CDELT(2) = CELLSI(2) / 3600.0
      CDELT(3) = 1.0
      DIM(1) = 7
      DIM(2) = 1
      CALL OPUT (IMAGE, 'CDELT', OOARE, DIM, CDELT, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Set up antenna array
      CALL OBDSKC (UVDATA, TDISK, TCNO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get CATBLK
      CALL OBHGET (UVDATA, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set antennas
      CALL GETANT (TDISK, TCNO, 1, CATBLK, BUFF, IERR)
      IF (IERR.NE.0) THEN
C                                       GETANT error
         MSGTXT = 'ERROR INITIALIZING ANTENNA INFORMATION'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       NANTSL = 0
      IF (NANTSL .EQ. 0) THEN
         NANTSL = NSTNS
         DO 60 I = 1, NANTSL
            ANTS(I) = TELNO(I)
   60       CONTINUE
         END IF
C                                       DOAWTN = .FALSE.
      IF (.NOT.DOAWTN) THEN

         DO 80 I = 1, NSTNS
            ANTI = TELNO(I)
            DO 70 K = 1, NANTSL
               ANTK = ANTS(K)
               IF (ANTI.EQ.ANTK) GO TO 80
   70          CONTINUE
            IANTSL = IANTSL + 1
            ANTSS(IANTSL) = ANTI
   80       CONTINUE
         NANTSL = IANTSL
         DO 90 I=1, NANTSL
            ANTS(I) = ANTSS(I)
   90       CONTINUE
         END IF
C
 999  RETURN
      END
C
      SUBROUTINE MPBIN (UVDATA, IRET)
C-----------------------------------------------------------------------
C   MPBIN puts couple of elements to the object UVDATA
C   Inputs:
C      UVDATA  C*?  Input uv data object
C   Output:
C      IRET    I    Error code: 0 => ok, else failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER UVDATA*(*)
      INTEGER   DIM(7)
      CHARACTER CDUM*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C-----------------------------------------------------------------------
C                                       Subarray 1
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (UVDATA, 'CALEDIT.SUBARR', OOAINT, DIM, 1, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Initialize "next source"
      DIM(1) = 1
      CALL OPUT (UVDATA, 'NEXTSID', OOAINT, DIM, 1, CDUM, IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE MPBDO (UVDATA, OIMAGE, IANT, IERR)
C-----------------------------------------------------------------------
C   Find pointings in raster, calibrate, average and grid data.
C   Inputs:
C      UVDATA  C*?  UV data object
C      OIMAGE  C*?  output image object
C      IANT    I    The antenna to reduce
C   Input in common INPP
C      NANTSL  I    Number of selected antennas
C      ANTS(*) I    List of selected antennas
C      NREFSL  I    Number of selected references antennas
C      BASES(*)I    List of selected referenses antennas
C      DOAWTN  L    TRUE if all ANTENNAS are positive
C   Output:
C      IERR    I    Return code, 0=> OK
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER UVDATA*(*), OIMAGE*(*)
C
      CHARACTER CLNAME*6
C
      INTEGER   GSIZE
C                                       Set maximum grid size
      PARAMETER (GSIZE=30)
      INTEGER   TYPE, DIM(7), NX, NY, NAXIS(7), I
      REAL   TTOL, CDELT(7)
C
      REAL   QUBIMA(GSIZE,GSIZE,4), QUBRMS(GSIZE,GSIZE,4)
      INTEGER K, IS
      INTEGER IANT, KANTS
C
      CHARACTER UVSCR*32, CDUM*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INPCOM.INC'
C-----------------------------------------------------------------------
      IERR = 0
      UVSCR = '    '
C                                       Cell spacing
      CALL OGET (OIMAGE, 'CDELT', TYPE, DIM, CDELT, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       U,V position tolerance 0.3 cells
      TTOL = 0.3 * MIN (ABS(CDELT(1)), ABS(CDELT(2)))
      TTOL = TTOL / 57.29577951
C                                       Size of image
      CALL OGET (OIMAGE, 'NAXIS', TYPE, DIM, NAXIS, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (OIMAGE, 'NAXIS', TYPE, DIM, NAXIS, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
      NX = NAXIS(1)
      NY = NAXIS(2)
C                                       Check size
      IF (NX*NY.LE.0) THEN
         IERR = 3
         MSGTXT = 'OUTPUT IMAGE SIZE MUST BE DEFINED'
         GO TO 980
         END IF
      IF (NX.GT.GSIZE .OR. NY.GT.GSIZE) THEN
         IERR = 4
         MSGTXT = 'OUTPUT IMAGE SIZE EXCEEDS INTERNAL ARRAYS'
         GO TO 980
         END IF
C                                       Initialize images
      DO 70 I = 1,NX
         DO 60 K = 1,NY
            DO 50 IS = 1, 4
               QUBIMA(I,K,IS) = FBLANK
               QUBRMS(I,K,IS) = FBLANK
   50          CONTINUE
   60       CONTINUE
   70    CONTINUE
C                                       Loop over "sources"
 100  CALL MBNEXT (UVDATA, UVSCR, IERR)
C                                       Done?
      IF (IERR.LT.0) GO TO 200
      IF (IERR.NE.0) GO TO 990
C                                       Averaging control.
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (UVSCR, 'TIMEAVG', OOARE, DIM, TAVG, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (UVSCR, 'TIMESKP', OOARE, DIM, TSKP, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (UVSCR, 'UVTOL', OOARE, DIM, TTOL, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Time average
C                                       KANTS is the selected antenna;
C                                       is used if common map for all
C                                       selected antenna
      KANTS = ANTS(IANT)
C
      CALL MBTAVG (UVSCR, UVSCR, KANTS, IERR)
C
      IF (IERR .EQ. 7) THEN
         IERR = 0
         GO TO 999
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       Average over baseline
      CALL MBMAVG (UVDATA, UVSCR, NX, NY, QUBIMA, QUBRMS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Next pointing
         GO TO 100
C                                       Done - write the qube image
 200  CONTINUE
C
      CLNAME(1:4) = 'ANT_'
      IF (DOCOMB .AND. NANTSL.GT.1) THEN
         CLNAME = 'ANT_**'
      ELSE
         WRITE(CLNAME(5:6), 1000) KANTS
         END IF
C
      CALL MBMOUT (OIMAGE, UVDATA, CLNAME, NX, NY, QUBIMA, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DORMS) THEN
         CLNAME(1:4) = 'RMS_'
         IF (DOCOMB .AND. NANTSL.GT.1) THEN
            CLNAME = 'RMS_**'
         ELSE
            WRITE(CLNAME(5:6), 1000) KANTS
            END IF
         CALL MBMOUT (OIMAGE, UVDATA, CLNAME, NX, NY, QUBRMS, IERR)
         END IF
      IF (IERR.NE.0) GO TO 990
C
      CALL OUVZAP (UVSCR, IERR)
C
      GO TO 999
C                                       Error
 980  CALL MSGWRT (8)
 990  MSGTXT = 'ERROR IMAGING ' // UVDATA
      CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT(I2)
      END
      SUBROUTINE MBNEXT (UVDATA, UVSCR, IERR)
C-----------------------------------------------------------------------
C   Routine to find next "source"
C   Inputs:
C      UVDATA  C*?  UV data object
C      UVSCR   C*?  Selected data in scratch object
C   Output:
C      IERR    I    Error code, 0=OK, -1 => no more data, else failed
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), UVSCR*(*)
      INTEGER   IERR
C
      CHARACTER SUTAB*32, NXTAB*32, COLNAM(3)*24, SOURCE(30)*16,
     *   SOUSEL(30)*16, CDUM*1
      REAL      TIMER(8), TBEG, TEND, TAVG
      INTEGER   TYPE, DIM(3), KOL(3), QUAL, QUAKOL, NAMKOL, IDKOL, I,
     *   TQUAL, SOUID, ISNEXT, NUMBSU, IS, NSOU, IDUM
      LOGICAL   OK, ALLSOU, WNTSOU
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (NAMKOL, KOL(1)), (QUAKOL, KOL(2)), (IDKOL, KOL(3))
      DATA COLNAM /'SOURCE', 'QUAL', 'ID. NO.'/
      DATA SOURCE /30*'    '/
C-----------------------------------------------------------------------
C                                       Time range
      CALL OGET (UVDATA, 'CALEDIT.TIMRNG', TYPE, DIM, TIMER, CDUM,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      TBEG = TIMER(1) + TIMER(2) / 24.0 + TIMER(3) / 1440.0 + TIMER(4) /
     *   86400.0
      TEND = TIMER(5) + TIMER(6) / 24.0 + TIMER(7) / 1440.0 + TIMER(8) /
     *   86400.0
      IF (TEND.LE.1.0E-10) TEND = 1.0E10
C                                       Source names
      CALL OGET (UVDATA, 'SOUSEL', TYPE, DIM, IDUM, SOUSEL, IERR)
      IF (IERR.NE.0) GO TO 990
      NSOU = 0
      DO 10 IS = 1,30
         IF (SOUSEL(IS).NE.'     ') NSOU = IS
 10      CONTINUE
      ALLSOU = NSOU.LE.0
C                                       Qualifier
      CALL OGET (UVDATA, 'CALEDIT.SELQUA', TYPE, DIM, QUAL, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get first SU table entry
      CALL OGET (UVDATA, 'NEXTSID', TYPE, DIM, ISNEXT, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Index table
      NXTAB = 'NX table for MBNEXT'
      CALL UV2TAB (UVDATA, NXTAB, 'NX', 1, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open
      CALL OOPEN (NXTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Source table
      SUTAB = 'SU table for MBNEXT'
      CALL UV2TAB (UVDATA, SUTAB, 'SU', 1, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open
      CALL OOPEN (SUTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Find highest number in SU
      CALL TABGET (SUTAB, 'NROW', TYPE, DIM, NUMBSU, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Find column numbers
      CALL TABCOL (SUTAB, 3, COLNAM, KOL, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Loop over source table
      DO 500 IS = ISNEXT,NUMBSU
C                                       Get QUAL
         CALL TABDGT (SUTAB, IS, QUAKOL, TYPE, DIM, TQUAL, CDUM, IERR)
         IF (IERR.NE.0) GO TO 990
         OK = (TQUAL.EQ.QUAL)
C                                       Check source
         CALL TABDGT (SUTAB, IS, NAMKOL, TYPE, DIM, IDUM, SOURCE, IERR)
         IF (IERR.NE.0) GO TO 990
         WNTSOU = .TRUE.
         IF (.NOT.ALLSOU) THEN
            WNTSOU = .FALSE.
            DO 110 I = 1,NSOU
               IF (SOURCE(1).EQ.SOUSEL(I)) WNTSOU = .TRUE.
 110           CONTINUE
            END IF
         OK = OK .AND. WNTSOU
         IF (OK) THEN
C                                       Get Source ID
            CALL TABDGT (SUTAB, IS, IDKOL, TYPE, DIM, SOUID, CDUM, IERR)
            IF (IERR.NE.0) GO TO 990
C                                       See if observed in timerange
            CALL MBMSOK (NXTAB, SOUID, TBEG, TEND, TAVG, OK, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
C                                       Use this one.
         IF (OK) GO TO 600
 500     CONTINUE
C                                       Close and get rid of tables.
      CALL OCLOSE (SUTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL DESTRY (SUTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OCLOSE (NXTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL DESTRY (NXTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Nothing more found
      IERR = -1
      GO TO 999
C                                       Save next su entry
 600  DIM(1) = 1
      DIM(2) = 1
      ISNEXT = IS + 1
      CALL OPUT (UVDATA, 'NEXTSID', OOAINT, DIM, ISNEXT, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Save this source ID
      CALL OPUT (UVDATA, 'CURSID', OOAINT, DIM, SOUID, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Tell what you're doing
      MSGTXT = ' Processing source: ' // SOURCE(1)
      CALL MSGWRT (5)
C                                       Close and get rid of tables.
      CALL OCLOSE (SUTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL DESTRY (SUTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OCLOSE (NXTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL DESTRY (NXTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Select "source"
      DIM(1) = LEN (SOURCE(1))
      DIM(2) = 30
      CALL OPUT (UVDATA, 'CALEDIT.SOURCS', OOACAR, DIM, IDUM, SOURCE,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Create scratch file if necessary
      IF (UVSCR.EQ.'     ') THEN
         UVSCR = 'Calibrated, selected uvdata'
         CALL OUVSCR (UVDATA, UVSCR, 2000, .FALSE., IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Copy, calibrate etc.
      CALL UV2SCR (UVDATA, UVSCR, .FALSE., IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Don't calibrate UVSCR
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (UVSCR, 'CALEDIT.DOCAL', OOALOG, DIM, .FALSE., CDUM,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (UVSCR, 'CALEDIT.DOPOL', OOAINT, DIM, -1, CDUM,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR FINDING NEXT POINTING '
      CALL MSGWRT (8)
 999  RETURN
      END
      SUBROUTINE MBTAVG (UVIN, UVOUT, IANT, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Time average time ordered holography data with optional blanking at
C   the beginning of each integration.  In holography mode the "U" and
C   "V" are the projected az and el offsets.
C   Can only process one subarray.
C   This routine uses an I/O buffer to hold the data being averaged.
C   Inputs:
C      UVIN    C*?   Name of input uvdata object.
C      UVOUT   C*?   Name of output uvdata object. (may be UVIN)
C      IANT     I    Selected antenna if
C                    Maps are created for each of the selected antennas
C   Inputs in common INPP
C      NANTSL   I    Number of antennas at the ANTENNAS list
C      ANTS(*)  I    List of selected antenna if the combine
C                    map is created for the all selected antennas
C      DOAWTN   L    True if all numbers at the ANTENNAS list positive
C      NREFSL   I    Number of antennas at the BASELINE list
C      BASES(*) I    List of antennas at the BASELINE list
C   Inputs from UVIN Object.
C      TIMEAVG  R    averaging time in seconds
C      TIMESKP  R    Time in seconds to skip at the beginning of each
C                    integration.
C      UVTOL    R    Tolerance for a change in u or u not to initiate a
C                    new integration (radians)
C   Output:
C       IERR   I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
      INTEGER   IANT
      INTEGER   IERR, KERR, K
C
      INTEGER   MXVS
C                                       MXVS = maximum no. correlations
C                                       in a record.
      PARAMETER (MXVS = 2048)
      REAL     RP(50), VS(3,MXVS)
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER SRTORD*2, TOUT*32, ANTAB*32, CDUM*1
      INTEGER   TYPE, DIM(3), COUNT, LREC, NRPARM, NCOR, ILOCU, ILOCV,
     *   ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ, ILOCA1, ILOCA2, ILOCSA,
     *   JLOCC, JLOCS, JLOCF, JLOCIF, JLOCR, JLOCD, INCS, INCF, INCIF,
     *   CNT, VSCNT, LOOP, BLPNT(MXBASE), BLCNT(MXBASE), ANTOFF(MAXANT),
     *   BUFNO, MAXAN, SUBARR, I, J, PNT, BLNO, IOFF, INDEX, NUMBL,
     *   NWORD, LBASE, ANT1, ANT2, INDXCT, CNTINT, IDUM
      REAL      RPS(50), VSS(3,MXVS), TTOL, TIME, SID, FQID,
     *   OLDU, OLDV, TGOOD
      REAL  TAVGOL, TSKPOL
      LOGICAL   NEXT, DOSU, DOFQ, EXIST, NEWPOS
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INPCOM.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open input
      CALL OUVOPN (UVIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check that sort = T*
      CALL UVDGET (UVIN, 'SORTORD', TYPE, DIM, IDUM, SRTORD, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (SRTORD(1:1).NE.'T') THEN
         MSGTXT = 'MBTAVG: DATA NOT IN T* ORDER, USE UVSRT'
         IERR = 5
         GO TO 995
         END IF
C                                       Subarray number
      CALL SECGET (UVIN, 'SUBARR', TYPE, DIM, SUBARR, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (SUBARR.LE.0) SUBARR = 1
C                                       Create AN table and use for
C                                       buffer.
      ANTAB = 'AN table for MBTAVG'
      CALL UV2TAB (UVIN, ANTAB, 'AN', SUBARR, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open
      CALL OBOPEN (ANTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get buffer number
      CALL OBINFO (ANTAB, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Create output if necessary
      CALL OBFEXS (UVOUT, EXIST, IERR)
      IF (IERR.GT.1) GO TO 990
      IERR = 0
      IF (.NOT.EXIST) THEN
         CALL OUVCLN (UVIN, UVOUT, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Temporary output in case in =
C                                       out.
      TOUT = 'Temporary output for MBTAVG'
      CALL OUVCOP (UVOUT, TOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open output
      CALL OUVOPN (TOUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy UVDESC
      CALL UVDSCP (UVIN, TOUT, IERR)
      IF (IERR.NE.0) GO TO 990
      COUNT = 0
C                                       Zero number of visibilities
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL UVDPUT (TOUT, 'GCOUNT', OOAINT, DIM, COUNT, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Averaging time (to day)
      CALL OUVGET (UVIN, 'TIMEAVG', TYPE, DIM, TAVG, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       store the TAVG in sec
      TAVGOL = TAVG
      TAVG = TAVG / (60.0 * 60.0 * 24.0)
C                                       Reduce by 1%
      TAVG = TAVG * 0.99
C                                       Blanking time (to day)
      CALL OUVGET (UVIN, 'TIMESKP', TYPE, DIM, TSKP, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       store the TSKP in sec
      TSKPOL = TSKP
      TSKP = TSKP / (60.0 * 60.0 * 24.0)
C                                       U,v tolerance for same average.
      CALL OUVGET (UVIN, 'UVTOL', TYPE, DIM, TTOL, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get info
C                                       Uv data pointers
      CALL UVDPNT (UVIN, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU,
     *   ILOCFQ, ILOCA1, ILOCA2, ILOCSA, JLOCC, JLOCS, JLOCF, JLOCR,
     *   JLOCD, JLOCIF, INCS, INCF, INCIF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Which random parameters to check
      DOSU = ILOCSU.GE.1
      DOFQ = ILOCFQ.GE.1
C                                       LREC
      CALL UVDGET (UVIN, 'LREC', TYPE, DIM, LREC, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       NRPARM
      CALL UVDGET (UVIN, 'NRPARM', TYPE, DIM, NRPARM, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       NCORR
      CALL UVDGET (UVIN, 'NCORR', TYPE, DIM, NCOR, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check that it fits
      IF (NCOR.GT.MXVS) THEN
         WRITE (MSGTXT,1000) NCOR, MXVS
         IERR = 5
         GO TO 995
         END IF
C                                       How many antennas?
      CALL ANTNO (ANTAB, SUBARR, MAXAN, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Length of a baseline entry with
C                                       count words at end
      LBASE = LREC + NCOR
C                                       Get baseline pointers
      PNT = 1
      IOFF = 1
      DO 20 I = 1,MAXAN-1
         ANTOFF(I) = IOFF - I - 1
         DO 10 J = I+1,MAXAN
            BLNO = ANTOFF(I) + J
            BLPNT(BLNO) = PNT
            PNT = PNT + LBASE
 10         CONTINUE
         IOFF = IOFF + (MAXAN - I)
 20      CONTINUE
      NUMBL = (MAXAN * (MAXAN - 1) ) / 2
C                                       Does it all fit in the BUFFER?
      IF (PNT.GT.BUFSIZ) THEN
         MSGTXT = 'MBTAVG: WORK ARRAY TOO SMALL'
         IERR = 5
         GO TO 995
         END IF
C                                       Clear accumulators
      NWORD = NUMBL * LBASE
      CALL RFILL (NWORD, 0.0, OBUFFR(1,BUFNO))
      CALL FILL (NUMBL, 0, BLCNT)
      CNT = 0
      TIME = 0.0
      TGOOD = -1.0E5
      SID = 0.0
      FQID = 0.0
      CNTINT = 0
C                                       First read
      CALL UVREAD (UVIN, RP, VS, IERR)
      IF (IERR.LT.0) GO TO 200
      IF (IERR.GT.0) GO TO 990
      TGOOD = RP(ILOCT) + TSKP
      GO TO 110
C                                       Loop through input data
 100     CALL UVREAD (UVIN, RP, VS, IERR)
         IF (IERR.LT.0) GO TO 200
         IF (IERR.GT.0) GO TO 990
C                                       Initial "old" u,v
         IF (CNT.LE.0) THEN
            OLDU = RP(ILOCU)
            OLDV = RP(ILOCV)
            END IF
C                                       Still in blanking period?
 110     IF (RP(ILOCT).LT.TGOOD) GO TO 100
C
         IF (ILOCB.GT.0) THEN
            ANT1 = (RP(ILOCB) / 256.0) + 0.001
            ANT2 = (RP(ILOCB) - ANT1 * 256) + 0.001
         ELSE
            ANT1 = RP(ILOCA1) + 0.001
            ANT2 = RP(ILOCA2) + 0.001
            END IF
C                                       check the selected baselines
         IF (DOCOMB) THEN
C                                       all selected antennas added
            DO 125 I = 1, NANTSL
               DO 120 K = 1, NREFSL
                  IF ((ANT1.EQ.ANTS(I) .AND. ANT2.EQ.BASES(K)) .OR.
     *               (ANT2.EQ.ANTS(I) .AND. ANT1.EQ.BASES(K)))
     *               GO TO 135
  120             CONTINUE
 125           CONTINUE
               GO TO 100
C
 135        CONTINUE
         ELSE
            DO 140 I = 1, NREFSL
            IF ((ANT1.EQ.IANT .AND. ANT2.EQ.BASES(I)) .OR.
     *         (ANT2.EQ.IANT .AND. ANT1.EQ.BASES(I))) GO TO 145
  140          CONTINUE
            GO TO 100
  145       CONTINUE
            END IF
C                                       Previous average done?
C                                       Exceed time?
         NEXT = (RP(ILOCT)-TIME) .GT. TAVG
C                                       New source
         NEXT = NEXT .OR. (DOSU .AND. (SID.NE.RPS(ILOCSU)))
C                                       New FQid
         NEXT = NEXT .OR. (DOFQ .AND. (FQID.NE.RPS(ILOCFQ)))
C                                       New pointing?
         NEWPOS = (ABS (OLDU-RP(ILOCU)).GT.TTOL) .OR.
     *      (ABS (OLDV-RP(ILOCV)).GT.TTOL)
         NEXT = NEXT .OR. NEWPOS
C                                       Need some data
         NEXT = NEXT .AND. (CNT .GT. 0)
         IF (NEXT) THEN
C                                       Set blanking for next
C                                       integration.
            IF (NEWPOS) TGOOD = RP(ILOCT) + TSKP
            CNTINT = CNTINT + 1
C                                       Average then write old
C                                       accumulations.
            DO 160 BLNO = 1,NUMBL
               IF (BLCNT(BLNO).GT.0) THEN
C                                       Pointer into OBUFFR
                  PNT = BLPNT(BLNO)
                  CALL COPY (NRPARM, OBUFFR(PNT,BUFNO), RPS)
C                                       U
                  RPS(ILOCU) = OBUFFR(PNT+ILOCU-1,BUFNO) / BLCNT(BLNO)
C                                       V
                  RPS(ILOCV) = OBUFFR(PNT+ILOCV-1,BUFNO) / BLCNT(BLNO)
C                                       W
                  RPS(ILOCW) = OBUFFR(PNT+ILOCW-1,BUFNO) / BLCNT(BLNO)
C                                       Time
                  RPS(ILOCT) = OBUFFR(PNT+ILOCT-1,BUFNO) / BLCNT(BLNO)
C                                       Vis
                  INDEX = PNT + NRPARM
                  INDXCT = PNT + LREC
                  DO 150 LOOP = 1,NCOR
                     VSCNT = OBUFFR(INDXCT,BUFNO) + 0.5
                     IF (VSCNT.GT.0) THEN
                        VSS(1,LOOP) = OBUFFR(INDEX,BUFNO) / VSCNT
                        VSS(2,LOOP) = OBUFFR(INDEX+1,BUFNO) / VSCNT
                        VSS(3,LOOP) = OBUFFR(INDEX+2,BUFNO)
                     ELSE
                        VSS(1,LOOP) = 0.0
                        VSS(2,LOOP) = 0.0
                        VSS(3,LOOP) = 0.0
                        END IF
                     INDEX = INDEX + 3
                     INDXCT = INDXCT + 1
 150                 CONTINUE
                  COUNT = COUNT + 1
                  CALL UVWRIT (TOUT, RPS, VSS, IERR)
                  IF (IERR.GT.0) GO TO 990
C                                       Clear accumulators
                  CALL RFILL (LBASE, 0.0, OBUFFR(PNT,BUFNO))
                  BLCNT(BLNO) = 0
                  END IF
 160           CONTINUE
C                                       Clear no. vis this integrations
C                                       count.
            CNT = 0
            END IF
C                                       Save position offsets
         OLDU = RP(ILOCU)
         OLDV = RP(ILOCV)
C                                       Still in blanking period?
         IF (RP(ILOCT).LT.TGOOD) GO TO 100
C                                       Accumulate
C                                       Antenna, baseline info
         IF (ILOCB.GT.0) THEN
            ANT1 = (RP(ILOCB) / 256.0) + 0.001
            ANT2 = (RP(ILOCB) - ANT1 * 256) + 0.001
         ELSE
            ANT1 = RP(ILOCA1) + 0.001
            ANT2 = RP(ILOCA2) + 0.001
            END IF
         BLNO = ANTOFF(ANT1) + ANT2
         PNT = BLPNT(BLNO)
         BLCNT(BLNO) = BLCNT(BLNO) + 1
         CNT = CNT + 1
         IF (CNT.LE.1) THEN
C                                       Save info for this integration
            TIME = RP(ILOCT)
            SID = 1
            FQID = 1
            IF (DOSU) SID =  RP(ILOCSU)
            IF (DOFQ) FQID =  RP(ILOCFQ)
            END IF
         IF (BLCNT(BLNO).LE.1) THEN
C                                       First vis for baseline
            CALL RCOPY (NRPARM, RP, OBUFFR(PNT,BUFNO))
         ELSE
            OBUFFR(PNT-1+ILOCU,BUFNO) = OBUFFR(PNT-1+ILOCU,BUFNO) +
     *         RP(ILOCU)
            OBUFFR(PNT-1+ILOCV,BUFNO) = OBUFFR(PNT-1+ILOCV,BUFNO) +
     *         RP(ILOCV)
            OBUFFR(PNT-1+ILOCW,BUFNO) = OBUFFR(PNT-1+ILOCW,BUFNO) +
     *         RP(ILOCW)
            OBUFFR(PNT-1+ILOCT,BUFNO) = OBUFFR(PNT-1+ILOCT,BUFNO) +
     *         RP(ILOCT)
            END IF
         INDEX = PNT + NRPARM
         INDXCT = PNT + LREC
         DO 180 LOOP = 1,NCOR
            IF (VS(3,LOOP).GT.0.0) THEN
               OBUFFR(INDXCT,BUFNO) = OBUFFR(INDXCT,BUFNO) + 1.0
               OBUFFR(INDEX,BUFNO) = OBUFFR(INDEX,BUFNO) +
     *            VS(1,LOOP)
               OBUFFR(INDEX+1,BUFNO) = OBUFFR(INDEX+1,BUFNO) +
     *            VS(2,LOOP)
               OBUFFR(INDEX+2,BUFNO) = OBUFFR(INDEX+2,BUFNO) +
     *            VS(3,LOOP)
               END IF
            INDEX = INDEX + 3
            INDXCT = INDXCT + 1
 180        CONTINUE
C                                       Loop for next vis
         GO TO 100
C                                       Done
 200  IERR = 0
      IF (CNT.GT.0) CNTINT = CNTINT + 1
C                                       Average and write last set.
      DO 260 BLNO = 1,NUMBL
         IF (BLCNT(BLNO).GT.0) THEN
C                                       Pointer into OBUFFR
            PNT = BLPNT(BLNO)
            CALL COPY (NRPARM, OBUFFR(PNT,BUFNO), RPS)
C                                       U
            RPS(ILOCU) = OBUFFR(PNT+ILOCU-1,BUFNO) / BLCNT(BLNO)
C                                       V
            RPS(ILOCV) = OBUFFR(PNT+ILOCV-1,BUFNO) / BLCNT(BLNO)
C                                       W
            RPS(ILOCW) = OBUFFR(PNT+ILOCW-1,BUFNO) / BLCNT(BLNO)
C                                       Time
            RPS(ILOCT) = OBUFFR(PNT+ILOCT-1,BUFNO) / BLCNT(BLNO)
C                                       Vis
            INDEX = PNT + NRPARM
            INDXCT = PNT + LREC
            DO 250 LOOP = 1,NCOR
               VSCNT = OBUFFR(INDXCT,BUFNO) + 0.5
               IF (VSCNT.GT.0) THEN
                  VSS(1,LOOP) = OBUFFR(INDEX,BUFNO) / VSCNT
                  VSS(2,LOOP) = OBUFFR(INDEX+1,BUFNO) / VSCNT
                  VSS(3,LOOP) = OBUFFR(INDEX+2,BUFNO)
               ELSE
                  VSS(1,LOOP) = 0.0
                  VSS(2,LOOP) = 0.0
                  VSS(3,LOOP) = 0.0
                  END IF
               INDEX = INDEX + 3
               INDXCT = INDXCT + 1
 250        CONTINUE
            COUNT = COUNT + 1
            CALL UVWRIT (TOUT, RPS, VSS, IERR)
            IF (IERR.GT.0) GO TO 990
            END IF
 260     CONTINUE
C                                       Better be some data
      IF (COUNT.LE.0) THEN
         WRITE (MSGTXT, 1200) IANT
         CALL MSGWRT (7)
         KERR = 7
      ELSE
         KERR = 0
         WRITE (MSGTXT,1250) COUNT, CNTINT
         CALL MSGWRT (4)
         END IF
C                                       Set sort order (unsorted)
      SRTORD = '  '
      DIM(1) = LEN (SRTORD)
      DIM(2) = 1
      CALL UVDPUT (TOUT, 'SORTORD', OOACAR, DIM, IDUM, SRTORD, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close files
      CALL OUVCLO (UVIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVCLO (TOUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBCLOS (ANTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy tables
      CALL UVDTCO (UVIN, TOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Mark output as valid
      DIM(1) = 1
      CALL FSTPUT (UVOUT, 'VALID', OOALOG, DIM, .TRUE., CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Delete temporary objects
      CALL OUVDES (TOUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OUVDES (ANTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C
      IERR = KERR
C                                       restore the TAVG, TSKP in sec
      TAVG = TAVGOL
      TSKP = TSKPOL
C
      GO TO 999
C                                       Error
 995  CALL MSGWRT (7)
 990  MSGTXT = 'MBTAVG: ERROR TIME AVERAGING ' // UVIN
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MBTAVG: TOO MANY CORRELATIONS ', I6,' > ', I6)
 1200 FORMAT ('NO DATA SELECTED FOR ANTENNA ', I2)
 1250 FORMAT ('MBTAVG: Wrote ',I7,' avg. vis. in ',I4,' integrations')
      END
      SUBROUTINE MBMAVG (UVMULT, UVDATA, NX, NY, QUBIMA, QUBRMS,
     *   IERR)
C-----------------------------------------------------------------------
C   Routine to Average data over baseline and divide by IPOL
C   Observing geometry (azimuth, zenith angle, parallactic angle) are of
C   the central pointing are attached to UVMULT.
C   Inputs:
C      UVMULT  C*?  Original Multi source UV data.
C      UVDATA  C*?  UV data object
C      NX      I    Number of columns in images
C      NY      I    Number of rows in images
C   Output:
C      QUBIMA  R(NX,NX,4) array of the qube image for all stokes
C      QUBRMS  R(NX,NX,4) array of the qube rms image for all stokes
C      IERR    I    Error code, 0=OK else failed
C-----------------------------------------------------------------------
      CHARACTER UVMULT*(*), UVDATA*(*)
      INTEGER   NX, NY, IERR
      REAL  QUBIMA(NX,NY,4), QUBRMS(NX,NY,4)
      REAL  SCALI, SCALQ, SCALU, SCALV
C
      INTEGER   LSIZE
C                                       Maximum list size
      PARAMETER (LSIZE=1000)
      INTEGER   I, TYPE, DIM(3), NDIM, NAXIS(7), INDXS, INDXT, INDXU,
     *   INDXV, LIMS(2,7), D(7), COUNT(4), IS, IX, IY, IROUND,
     *   ISDCEN, LSTCNT, IDUM
      LOGICAL   EOF
      LOGICAL   CENTE
      REAL      RP(50), VIS(3,1024), VS(3,4), PMODEL(6), FSTOK(4), VAL,
     *   IIPOL, IIRMS, QQPOL, QQRMS, UUPOL, UURMS, VVPOL, VVRMS,
     *   CELLSI(2), U, V, OLDU, OLDV, TOLU, TOLV, TCEN, AZC, ELC, PAC,
     *   ZA, USUM, VSUM, UCNT, VCNT, TU, TQ, SCHI, CCHI
      REAL      XPOS(LSIZE), YPOS(LSIZE), ILIST(LSIZE), IRLIST(LSIZE),
     *   QLIST(LSIZE), QRLIST(LSIZE), ULIST(LSIZE), URLIST(LSIZE),
     *   VLIST(LSIZE), VRLIST(LSIZE)
      COMPLEX   IPOL, XPOL
      CHARACTER IQUV*4, SUTAB*32, CDUM*1
      DOUBLE PRECISION SUM(4), SUM2(4)
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INPCOM.INC'
      DATA IQUV /'IQUV'/
C-----------------------------------------------------------------------
C                                       Get source polarization
      CALL OGET ('Input', 'PMODEL', TYPE, DIM, PMODEL, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (PMODEL(1).LE.0) THEN
         IERR = 2
         MSGTXT = 'YOU MUST SUPPLY PMODEL'
         GO TO 980
         END IF
      FSTOK(2) = PMODEL(2) / PMODEL(1)
      FSTOK(3) = PMODEL(3) / PMODEL(1)
      FSTOK(4) = PMODEL(4) / PMODEL(1)
C                                       Cell spacing
      CALL OGET ('Input', 'CELLSIZE', TYPE, DIM, CELLSI, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Must have non zero values for
C                                       CELLSIZE
      IF ((CELLSI(1).EQ.0.0) .OR. (CELLSI(2).EQ.0.0)) THEN
         IERR = 2
         MSGTXT = 'INVALID IMSIZE OR CELLSIZE'
         GO TO 980
         END IF
C                                       Convert to radians
C                                       Note first axis has been
C                                       reversed in u already.
      CELLSI(1) = CELLSI(1) / 206264.8062
      CELLSI(2) = CELLSI(2) / 206264.8062
C                                       Tolerance 0.3 cell for same
C                                       pointing.
      TOLU = 0.3 * ABS (CELLSI(1))
      TOLV = 0.3 * ABS (CELLSI(2))
C                                       Want IQUV
      DIM(1) = LEN (IQUV)
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (UVDATA, 'CALEDIT.STOKES', OOACAR, DIM, IDUM, IQUV,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open uvdata
      CALL OUVOPN (UVDATA, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get vis info
      CALL UVDGET (UVDATA, 'NDIM', TYPE, DIM, NDIM, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDGET (UVDATA, 'NAXIS', TYPE, DIM, NAXIS, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL UVDFND (UVDATA, 2, 'STOKES', INDXS, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING STOKES AXIS'
         GO TO 980
         END IF
      CALL UVDFND (UVDATA, 1, 'TIME1', INDXT, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING TIME1 RANDOM PARAMETER'
         GO TO 980
         END IF
      CALL UVDFND (UVDATA, 1, 'UU-L', INDXU, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING U RANDOM PARAMETER'
         GO TO 980
         END IF
      CALL UVDFND (UVDATA, 1, 'VV-L', INDXV, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE FINDING V RANDOM PARAMETER'
         GO TO 980
         END IF
      CALL FILL (5, 1, D)
      CALL FILL (10, 0, LIMS)
      DO 50 I = 1,NDIM
         D(I) = NAXIS(I)
         LIMS(1,I) = 1
         LIMS(2,I) = NAXIS(I)
 50   CONTINUE
      DO 60 IS = 1,4
         SUM(IS) = 0.0D0
         SUM2(IS) = 0.0D0
         COUNT(IS) = 0
 60      CONTINUE
      USUM = 0.0
      UCNT = 0.0
      VSUM = 0.0
      VCNT = 0.0
      LSTCNT = 0
C
      CENTE = .FALSE.
C                                       Source table
      SUTAB = 'SU table for MBMAVG'
      CALL UV2TAB (UVMULT, SUTAB, 'SU', 1, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get this source ID
C                                       This is set in MBNEXT
      CALL OGET (UVMULT, 'CURSID', TYPE, DIM, ISDCEN, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Initial read
      CALL UVREAD (UVDATA, RP, VIS, IERR)
      IF (IERR.GT.0) GO TO 990
C                                       Done?
      EOF = IERR.LT.0
      IF (IERR.LT.0) GO TO 300
      OLDU = RP(INDXU)
      OLDV = RP(INDXV)
      GO TO 110
C                                       Read datum
 100     CALL UVREAD (UVDATA, RP, VIS, IERR)
         IF (IERR.GT.0) GO TO 990
C                                       Done?
         EOF = IERR.LT.0
         IF (IERR.LT.0) GO TO 300
C                                       Pointing done?
 110     U = RP(INDXU)
         V = RP(INDXV)
         IF ((ABS (U-OLDU).GT.TOLU) .OR. (ABS (V-OLDV).GT.TOLV))
     *      GO TO 300
C                                       Accumulate U,V
         USUM = USUM + U
         UCNT = UCNT + 1.0
         VSUM = VSUM + V
         VCNT = VCNT + 1.0
C                                       Average vis by STOKES
         DO 150 IS = 1,4
            LIMS(1,INDXS) = IS
            LIMS(2,INDXS) = IS
            CALL AVGVIS (D(2), D(3), D(4), D(5), LIMS, VIS, VS(1,IS))
C                                       Sum
            IF ((VS(3,1).GT.0.0) .AND. (VS(3,IS).GT.0.0)) THEN
               IF (IS.EQ.1) THEN
                  VAL = SQRT (VS(1,1)*VS(1,1) + VS(2,1)*VS(2,1))
               ELSE
                  IPOL = CMPLX (VS(1,1), VS(2,1))
                  XPOL = CMPLX (VS(1,IS), VS(2,IS))
C                                       Take ratio
                  VAL = REAL (XPOL / IPOL)
                  END IF
               COUNT(IS) = COUNT(IS) + 1
               SUM(IS) = SUM(IS) + VAL
               SUM2(IS) = SUM2(IS) + VAL * VAL
               END IF
 150        CONTINUE
C                                       Loop until pointing done.
         GO TO 100
C                                       Finished pointing
 300  IERR = 0
      LSTCNT = LSTCNT + 1

C                                       Average U,V as x,y in cells
      IF (UCNT.GT.0.1) THEN
         XPOS(LSTCNT) = -(USUM / UCNT) / CELLSI(1) + NX/2 + 1
      ELSE
         XPOS(LSTCNT) = FBLANK
         END IF
      IF (VCNT.GT.0.1) THEN
         YPOS(LSTCNT) = (VSUM / VCNT) / CELLSI(2) + NY/2 + 1
      ELSE
         YPOS(LSTCNT) = FBLANK
         END IF
C                                       Average
      IF (COUNT(1).GT.0) THEN
         IIPOL = SUM(1) / COUNT(1)
      ELSE
         IIPOL = FBLANK
         END IF
      IF (COUNT(1).GT.1) THEN
         IIRMS = (SUM2(1) - ((SUM(1)**2) / COUNT(1))) / (COUNT(1) - 1.0)
         IIRMS = SQRT (ABS (IIRMS / COUNT(1)))
      ELSE
         IIRMS = FBLANK
         END IF
C
      IF (COUNT(2).GT.0) THEN
         QQPOL = SUM(2) / COUNT(2)
      ELSE
         QQPOL = FBLANK
         END IF
      IF (COUNT(2).GT.1) THEN
         QQRMS = (SUM2(2) - ((SUM(2)**2) / COUNT(2))) / (COUNT(2) - 1.0)
         QQRMS = SQRT (ABS (QQRMS / COUNT(2)))
      ELSE
         QQRMS = FBLANK
         END IF
C
      IF (COUNT(3).GT.0) THEN
         UUPOL = SUM(3) / COUNT(3)
      ELSE
         UUPOL = FBLANK
         END IF
      IF (COUNT(3).GT.1) THEN
         UURMS = (SUM2(3) - ((SUM(3)**2) / COUNT(3))) / (COUNT(3) - 1.0)
         UURMS = SQRT (ABS (UURMS / COUNT(3)))
      ELSE
         UURMS = FBLANK
         END IF
C
      IF (COUNT(4).GT.0) THEN
         VVPOL = SUM(4) / COUNT(4)
      ELSE
         VVPOL = FBLANK
         END IF
      IF (COUNT(4).GT.1) THEN
         VVRMS = (SUM2(4) - ((SUM(4)**2) / COUNT(4))) / (COUNT(4) - 1.0)
         VVRMS = SQRT (ABS (VVRMS / COUNT(4)))
      ELSE
         VVRMS = FBLANK
         END IF
C                                       Correct
      IF (IIPOL.NE.FBLANK) IIPOL = IIPOL / PMODEL(1)
      IF (IIRMS.NE.FBLANK) IIRMS = IIRMS / PMODEL(1)
      IF (QQPOL.NE.FBLANK) QQPOL = QQPOL - FSTOK(2)
      IF (UUPOL.NE.FBLANK) UUPOL = UUPOL - FSTOK(3)
      IF (VVPOL.NE.FBLANK) VVPOL = VVPOL - FSTOK(4)
C                                       Reset u,v stuff for next
C                                       pointing
      OLDU = U
      OLDV = V
C                                       Get geometry
      IX = IROUND (XPOS(LSTCNT))
      IY = IROUND (YPOS(LSTCNT))
      TCEN = RP(INDXT)
      CALL MBAZEL (SUTAB, ISDCEN, TCEN, AZC, ELC, PAC, IERR)
      IF (IERR.NE.0) GO TO 990
      IF ((IX.EQ.(NX/2+1)) .AND. (IY.EQ.(NY/2+1))) THEN
C                                       Attach to UVDATA, for center
C                                       pointing.
         DIM(1) = 1
         DIM(2) = 1
         ZA = 90.0 - ELC
         CALL OPUT (UVMULT, 'PARANGLE', OOARE, DIM, PAC, CDUM, IERR)
         CALL OPUT (UVMULT, 'ZENANGLE', OOARE, DIM, ZA, CDUM, IERR)
         CALL OPUT (UVMULT, 'AZIMUTH ', OOARE, DIM, AZC, CDUM, IERR)
C
         CENTE = .TRUE.
         END IF
C                                       Correct Q and U for the spurious
C                                       rotation of the instrumental
C                                       polarization in the calibration
      SCHI = SIN (2.0 * PAC * DG2RAD)
      CCHI = COS (2.0 * PAC * DG2RAD)
      IF ((UUPOL.NE.FBLANK) .AND. (QQPOL.NE.FBLANK)) THEN
         TQ = QQPOL
         TU = UUPOL
         QQPOL = CCHI * TQ + SCHI * TU
         UUPOL = CCHI * TU - SCHI * TQ
         TQ = QQRMS
         TU = UURMS
         QQRMS = CCHI * TQ + SCHI * TU
         UURMS = CCHI * TU - SCHI * TQ
         END IF
C                                       Save values in lists
      ILIST(LSTCNT) = IIPOL
      IRLIST(LSTCNT) = IIRMS
      QLIST(LSTCNT) = QQPOL
      QRLIST(LSTCNT) = QQRMS
      ULIST(LSTCNT) = UUPOL
      URLIST(LSTCNT) = UURMS
      VLIST(LSTCNT) = VVPOL
      VRLIST(LSTCNT) = VVRMS
C                                       Clear accumulators
      USUM = 0.0
      UCNT = 0.0
      VSUM = 0.0
      VCNT = 0.0
      DO 360 IS = 1,4
         SUM(IS) = 0.0D0
         SUM2(IS) = 0.0D0
         COUNT(IS) = 0
 360     CONTINUE
C                                       Next pointing
      IF (.NOT.EOF) GO TO 110
C                                       I have not passed through the
C                                       center of the image
      IF (.NOT. CENTE) THEN
         IERR = 2
         MSGTXT = 'the rastr does not include the center'
         GO TO 980
         END IF
C                                       Destroy SU table object
      CALL DESTRY (SUTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close uvdata
      CALL OUVCLO (UVDATA, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Grid the four stokes images
C                                       and merge them at one qube
      SCALI = 1.0
      SCALQ = 1.0
      SCALU = 1.0
      SCALV = 1.0
      DO 500 IS = 1,4
         GO TO (410, 420, 430, 440), IS
  410    CALL MBGRID (NX, NY, LSTCNT, XPOS, YPOS, ILIST, QUBIMA(1,1,1))
C                                       calculate the maximum of
C                                       QUBIMA(*,*,1)
         IF (NOSCAL) GO TO 500
         SCALI = 0
         DO 415 IX = 1, NX
            DO 412 IY = 1, NY
               SCALI = MAX(SCALI, QUBIMA(IX, IY, 1))
  412          CONTINUE
  415       CONTINUE
C                                       devide IPOL by its maximum
         IF (SCALI .GT. 0) THEN
            WRITE (MSGTXT, 1100) SCALI
            CALL MSGWRT (8)
            DO 417 IX = 1, NX
               DO 416 IY = 1, NY
                  QUBIMA(IX, IY, 1) = QUBIMA(IX, IY, 1) / SCALI
  416             CONTINUE
  417          CONTINUE
            END IF

         GO TO 500
  420    CALL MBGRID (NX, NY, LSTCNT, XPOS, YPOS, QLIST, QUBIMA(1,1,2))
C                                       calculate the maximum of
C                                       QPOL
         IF (NOSCAL) GO TO 500
         SCALQ = 0
         DO 425 IX = 1, NX
            DO 422 IY = 1, NY
               SCALQ = MAX(SCALQ, QUBIMA(IX, IY, 2))
  422          CONTINUE
  425       CONTINUE
C                                       devide QPOL by its maximum
         IF (SCALQ .GT. 0) THEN
            WRITE (MSGTXT, 1200) SCALQ
            CALL MSGWRT (8)
            DO 427 IX = 1, NX
               DO 426 IY = 1, NY
                  QUBIMA(IX, IY, 2) = QUBIMA(IX, IY, 2) / SCALQ
  426             CONTINUE
  427          CONTINUE
            END IF
         GO TO 500
  430    CALL MBGRID (NX, NY, LSTCNT, XPOS, YPOS, ULIST, QUBIMA(1,1,3))
C                                       calculate the maximum of
C                                       UPOL
         IF (NOSCAL) GO TO 500
         SCALU = 0
         DO 435 IX = 1, NX
            DO 432 IY = 1, NY
               SCALU = MAX(SCALU, QUBIMA(IX, IY, 3))
  432          CONTINUE
  435       CONTINUE
C                                       devide QPOL by its maximum
         IF (SCALU .GT. 0) THEN
            WRITE (MSGTXT, 1300) SCALU
            CALL MSGWRT (8)
            DO 437 IX = 1, NX
               DO 436 IY = 1, NY
                  QUBIMA(IX, IY, 3) = QUBIMA(IX, IY, 3) / SCALU
  436             CONTINUE
  437          CONTINUE
            END IF
         GO TO 500
  440    CALL MBGRID (NX, NY, LSTCNT, XPOS, YPOS, VLIST, QUBIMA(1,1,4))
C                                       calculate the maximum of
C                                       VPOL
         IF (NOSCAL) GO TO 500
         SCALV = 0
         DO 445 IX = 1, NX
            DO 442 IY = 1, NY
               SCALV = MAX(SCALV, QUBIMA(IX, IY, 4))
  442          CONTINUE
  445       CONTINUE
C                                       devide VPOL by its maximum
         IF (SCALV .GT. 0) THEN
            WRITE (MSGTXT, 1400) SCALV
            CALL MSGWRT (8)
            DO 447 IX = 1, NX
               DO 446 IY = 1, NY
                  QUBIMA(IX, IY, 4) = QUBIMA(IX, IY, 4) / SCALV
  446             CONTINUE
  447          CONTINUE
            END IF
         GO TO 500
 500     CONTINUE
C                                       put the SCALs to keywords
         CALL OPUT (UVMULT, 'SCALI   ', OOARE, DIM, SCALI, CDUM, IERR)
         CALL OPUT (UVMULT, 'SCALQ   ', OOARE, DIM, SCALQ, CDUM, IERR)
         CALL OPUT (UVMULT, 'SCALU   ', OOARE, DIM, SCALU, CDUM, IERR)
         CALL OPUT (UVMULT, 'SCALV   ', OOARE, DIM, SCALV, CDUM, IERR)
C                                       Grid the four stokes rms images
C                                       and merge them at one qube
      DO 600 IS = 1,4
         GO TO (510, 520, 530, 540), IS
  510    CALL MBGRID (NX, NY, LSTCNT, XPOS, YPOS, IRLIST, QUBRMS(1,1,1))
         GO TO 600
  520    CALL MBGRID (NX, NY, LSTCNT, XPOS, YPOS, QRLIST, QUBRMS(1,1,2))
         GO TO 600
  530    CALL MBGRID (NX, NY, LSTCNT, XPOS, YPOS, URLIST, QUBRMS(1,1,3))
         GO TO 600
  540    CALL MBGRID (NX, NY, LSTCNT, XPOS, YPOS, VRLIST, QUBRMS(1,1,4))
         GO TO 600
 600     CONTINUE
      GO TO 999
C                                       Error
 980  CALL MSGWRT (8)
 990  MSGTXT = 'ERROR IMAGING ' // UVDATA
      CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('!!! The IPOL map devided by ', F7.3, ' !!!')
 1200 FORMAT ('!!! The QPOL map devided by ', F7.3, ' !!!')
 1300 FORMAT ('!!! The UPOL map devided by ', F7.3, ' !!!')
 1400 FORMAT ('!!! The VPOL map devided by ', F7.3, ' !!!')
      END
      SUBROUTINE MBMOUT (OIMAGE, UVDATA, CLASS, NX, NY, ARRAY, IERR)
C-----------------------------------------------------------------------
C   Routine to write output array
C   Inputs:
C      OIMAGE  C*?  output image object
C      UVDATA  C*?  UV data object
C      CLASS   C*6  Class of array
C      NX      I    Number of columns in ARRAY
C      NY      I    Number of rows in ARRAY
C      ARRAY   R(*) Data array
C   Output:
C      IERR    I    Error code, 0=OK else failed
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), OIMAGE*(*), CLASS*6
      INTEGER   NX, NY, IERR
      REAL      ARRAY(NX,NY,4)
C
      CHARACTER ACCESS*8
      INTEGER   DIM(3), IDUM
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C  DEBUG
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Add AIPS file class.
      DIM(1) = LEN (CLASS)
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (OIMAGE, 'CLASS', OOACAR, DIM, IDUM, CLASS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open image
      CALL IMGOPN (OIMAGE, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Access by array
      ACCESS = 'ARRAY'
      DIM(1) = LEN(ACCESS)
      DIM(2) = 1
      CALL ARPPUT (OIMAGE, 'ACCESS', OOACAR, DIM, IDUM, ACCESS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Write image
      DIM(1) = NX
      DIM(2) = NY
      DIM(3) = 4
      CALL ARRWRI (OIMAGE, DIM, ARRAY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close array and image
      CALL ARRCLO (OIMAGE, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCLO (OIMAGE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       History
      CALL UVDHIS (UVDATA, OIMAGE)
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING OUTPUT IMAGE '
      CALL MSGWRT (8)
 999  RETURN
      END
      SUBROUTINE UVDHIS (UVDATA, OIMAGE)
C-----------------------------------------------------------------------
C   Routine to write history file to output image object.
C   Also copies observing geometry from uvdata; these should come out as
C   catalog header keywords in the AIPS files.
C   Inputs:
C      UVDATA  C*?  UV data object
C      OIMAGE  C*?  output image object
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), OIMAGE*(*)
C
      INTEGER   NADV
      PARAMETER (NADV=20)
      CHARACTER LIST(NADV)*8, OGEOM(7)*8
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'QUAL', 'TIMERANG', 'BIF', 'EIF', 'PMODEL',
     *   'DOCALIB', 'GAINUSE', 'DOPOL', 'PDVER', 'FLAGVER',
     *   'OUTNAME', 'OUTDISK', 'OUTSEQ',
     *   'IMSIZE', 'CELLSIZE',
     *   'APARM'/
C                                       Observing geometry
      DATA OGEOM /'PARANGLE', 'ZENANGLE', 'AZIMUTH', 'SCALI',
     *   'SCALQ', 'SCALU', 'SCALV'/
C-----------------------------------------------------------------------
C                                        Copy old history
      CALL OHCOPY (UVDATA, OIMAGE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        New additions - copy adverb
C                                        values.
      CALL OHLIST ('Input', LIST, NADV, OIMAGE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy observing geometry
      CALL OOPEN (OIMAGE, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
C
      CALL IN2OBJ (UVDATA, 7, OGEOM, OGEOM, OIMAGE, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OCLOSE (OIMAGE, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // OIMAGE
      CALL MSGWRT (4)
 999  RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE MBFSOU (UVDATA, SOURCE, SUID, IERR)
C-----------------------------------------------------------------------
C   Find source ID
C   Inputs:
C      UVDATA  C*?  UV data object
C      SOURCE  C*16 Source name
C   Output:
C      SUID    I    Source ID, < 0 => not found
C      IERR    I    Error code, 0=OK, 1 => not found, else failed
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*), SOURCE*16
      INTEGER   SUID, IERR
C
      CHARACTER SUTAB*32, SOUNAM*16, COLNAM(2)*24, CDUM*1
      INTEGER   TYPE, DIM(3), SUROW, NUMSU, KOL(2), NAMKOL, IDKOL, IDUM
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (NAMKOL, KOL(1)), (IDKOL, KOL(2))
      DATA COLNAM /'SOURCE', 'ID. NO.'/
C-----------------------------------------------------------------------
C                                       Make SU table
      SUTAB = 'SU table for MBFSOU'
      CALL UV2TAB (UVDATA, SUTAB, 'SU', 1, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open
      CALL OOPEN (SUTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Number of rows
      CALL TABGET (SUTAB, 'NROW', TYPE, DIM, NUMSU, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Find column numbers
      CALL TABCOL (SUTAB, 2, COLNAM, KOL, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Loop over table
      SUID = -1
      DO 100 SUROW = 1,NUMSU
         CALL TABDGT (SUTAB, SUROW, NAMKOL, TYPE, DIM, IDUM, SOUNAM,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         IF (SOUNAM.EQ.SOURCE) GO TO 200
 100     CONTINUE
C                                       Not found
      GO TO 300
C                                       Get number
 200  CALL TABDGT (SUTAB, SUROW, IDKOL, TYPE, DIM,  SUID, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close table
 300  CALL OCLOSE (SUTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Delete object
      CALL DESTRY (SUTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Did it work?
      IF (SUID.LE.0) IERR = 1
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR LOOKING UP SOURCE ' // SOURCE
      CALL MSGWRT (8)
 999  RETURN
      END
      SUBROUTINE MBMSOK (NXTAB, SOUID, TBEG, TEND, TAVG, OK, IERR)
C-----------------------------------------------------------------------
C   Check if a given source was observed in the given timerange; only
C   the centertime os the scan is used fro this test.
C   Note: no check is made to see if there are multiple scans.
C   Inputs:
C      NXTAB   C*?  NX table, already opened
C      SOUID   I    Source ID to check
C      TBEG    R    Start of timerange in days
C      TEND    R    End of timerange in days
C   Output:
C      TAVG    R    Average time of scan
C      OK      L    If true source observed in scan
C      IERR    I    Error code, 0=OK, 1 => not found, else failed
C-----------------------------------------------------------------------
      CHARACTER NXTAB*(*)
      INTEGER   SOUID, IERR
      REAL      TBEG, TEND, TAVG
      LOGICAL   OK
C
      CHARACTER COLNAM(2)*24, CDUM*1
      INTEGER   TYPE, DIM(3), NXROW, IDSOU, NUMNX, KOL(2), IDKOL,
     *   TIMKOL
      REAL      TIME
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (IDKOL, KOL(1)), (TIMKOL, KOL(2))
      DATA COLNAM /'SOURCE ID', 'TIME'/
C-----------------------------------------------------------------------
C                                       Number of rows
      CALL TABGET (NXTAB, 'NROW', TYPE, DIM, NUMNX, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Find column numbers
      CALL TABCOL (NXTAB, 2, COLNAM, KOL, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Loop over table
      OK = .FALSE.
      TAVG = -1.0E5
      DO 100 NXROW = 1,NUMNX
         CALL TABDGT (NXTAB, NXROW, IDKOL, TYPE, DIM,  IDSOU, CDUM,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         IF (IDSOU.EQ.SOUID) THEN
C                                       Check center time
            CALL TABDGT (NXTAB, NXROW, TIMKOL, TYPE, DIM,  TIME, CDUM,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((TIME.GE.TBEG) .AND. (TIME.LE.TEND)) GO TO 200
            END IF
 100     CONTINUE
C                                       Not found
      GO TO 999
C                                       Set scan time
 200  TAVG = TIME
      OK = .TRUE.
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR FINDING NEXT SOURCE IN INDEX TABLE'
      CALL MSGWRT (8)
 999  RETURN
      END
      SUBROUTINE MBAZEL (TABLE, SOURID, TIME, AZ, EL, PA, IERR)
C-----------------------------------------------------------------------
C   Returns the azimuth and elevation of a source at a given time.
C   (assumed to be the same for all antennas)
C   Note it assumes GETANT has been called.
C   Inputs:
C      TABLE   C*?  Input table object (assumed to have associated AN
C                   and SU tables)
C      SOURID  I    Source ID number, use -1 for single source data
C      TIME    R    Time in days
C   Output:
C      AZ      R    Source Azimuth in degrees
C      EL      R    Source elevation in degrees
C      PA      R    Parallactic angle in degrees
C      IERR    I    Return code, O=OK, else failed.
C-----------------------------------------------------------------------
      CHARACTER TABLE*(*)
      INTEGER    SOURID, IERR
      REAL       TIME, AZ, EL, PA
C
      INTEGER   TDISK, TCNO, TVER, LUN, ANT, SUBARR
      CHARACTER TTYPE*2
      DOUBLE PRECISION DTIME
C    *   SINDEC, COSDEC, HRANG, SINLAT, COSLAT,
      REAL       HA
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      REAL      PANGS(MAXANT)
C-----------------------------------------------------------------------
      IERR = 0
      ANT = 1
      SUBARR = 1
      DTIME = TIME
C                                       Look up table info
      CALL TBLKUP (TABLE, TDISK, TCNO, TTYPE, TVER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get LUN
      CALL OBLUN (LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Get CATBLK
      CALL OBHGET (TABLE, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Set source if necessary
      IF ((SOURID.NE.IDSOUR) .OR. (SOURID.LE.0)) CALL GETSOU (SOURID,
     *   TDISK, TCNO, CATBLK, LUN, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Free LUN
      CALL OBLUFR (LUN)
C                                       Determine elevation
      CALL SOUELV (ANT, DTIME, HA, EL, AZ)
C                                       Parallactic angle
      CALL PARANG (TIME, PANGS)
      PA = PANGS(ANT)
C                                       Azimuth
C     SINDEC = SIN (DECAPP)
C     COSDEC = COS (DECAPP)
C     HRANG = HA
C     COSLAT = COS (STNLAT(ANT))
C     SINLAT = SIN (STNLAT(ANT))
C     AZ = ATAN2 (-COSDEC*SIN (HRANG),
C    *   (SINDEC*COSLAT - COSDEC*COS (HRANG)*SINLAT))
C                                       Convert to degrees
      EL = EL * 57.29577951
      AZ = AZ * 57.29577951
      PA = PA * 57.29577951
      GO TO 999
C                                       Error
 900  MSGTXT = 'PROBLEM SOURCE GEOMETRY FOR ' // TABLE
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE MBGRID (NX, NY, COUNT, X, Y, LIST, IMAGE)
C-----------------------------------------------------------------------
C   Grid quasi regularly sampled values from a list into a regular grid.
C   A three term Lagrangian interpolation is used in each axis.
C   Inputs:
C      NX      I    Number of columns in image
C      NY      I    Number of rows in image
C      COUNT   I    Number of items in the list
C      X       R(*) "X" positions in cells of items in the list
C      Y       R(*) "Y" positions in cells of items in the list
C      LIST    R(*) List of values
C   Output:
C      IMAGE   R(*) Regularly gridded array.
C-----------------------------------------------------------------------
      INTEGER   NX, NY, COUNT
      REAL      X(COUNT), Y(COUNT), LIST(COUNT), IMAGE(NX,NY)
C
      INTEGER   IX, IY, I, J, K, XL(3,3), USED(9), USECNT, IPOS, JPOS,
     *   OFFSET(2,8), IXT, IYT
      LOGICAL   GOOD
      REAL      XC, YC, PX(3,3), PY(3), YCR(3), DIST, D2, XW, YW, NUMX,
     *   DENOMX, NUMY, DENOMY, SUM, SUMWT
      INCLUDE 'INCS:DDCH.INC'
      DATA OFFSET /-1,-1, -1,0, -1,1, 0,-1, 0,1, 1,-1, 1,0, 1,1/
C-----------------------------------------------------------------------
C                                       Loop over X
      DO 500 IX = 1,NX
         XC = IX
C                                       Loop over Y
         DO 490 IY = 1,NY
C                                       In case of no data
            IMAGE(IX,IY) = FBLANK
            YC = IY
            DO 20 I = 1,3
               DO 10 J = 1,3
                  XL(I,J) = -1
 10               CONTINUE
 20            CONTINUE
C                                       Find center cell
            DIST = 1.0E20
            DO 50 I = 1,COUNT
               D2 = 1.0E20
               IF ((X(I).NE.FBLANK) .AND. (Y(I).NE.FBLANK) .AND.
     *            (LIST(I).NE.FBLANK)) D2 = (X(I)-XC)**2 + (Y(I)-YC)**2
               IF (D2.LT.DIST) THEN
                  IPOS = I
                  DIST = D2
                  END IF
 50            CONTINUE
C                                       Must be within one cell
            IF (DIST.GT.0.95) GO TO 490
            XL(2,2) = IPOS
            USECNT = 1
            USED(USECNT) = IPOS
C                                       Look for 8 adjacent cells
            DO 100 J = 1,8
C                                       Take care of edge effects
               IXT = IX + OFFSET(1,J)
               IYT = IY + OFFSET(2,J)
               IF ((IXT.GE.1) .AND. (IXT.LE.NX) .AND. (IYT.GE.1) .AND.
     *            (IYT.LE.NY)) THEN
                  XW = X(IPOS) + OFFSET(1,J)
                  YW = Y(IPOS) + OFFSET(2,J)
                  DIST = 1.0E20
                  DO 60 I = 1,COUNT
                     D2 = 1.0E20
                     IF ((X(I).NE.FBLANK) .AND. (Y(I).NE.FBLANK) .AND.
     *                  (LIST(I).NE.FBLANK))
     *                  D2 = (X(I)-XW)**2 + (Y(I)-YW)**2
                     IF (D2.LT.DIST) THEN
                        JPOS = I
                        DIST = D2
                        END IF
 60                  CONTINUE
               ELSE
C                                       Off edge
                  JPOS = -1
                  END IF
C                                       Must be within one cell
               IF (DIST.GT.0.95) JPOS = -1
C                                       Can't be picked already
               DO 80 I = 1,USECNT
                  IF (JPOS.EQ.USED(I)) JPOS = -1
 80               CONTINUE
               XL(2+OFFSET(1,J),2+OFFSET(2,J)) = JPOS
               USECNT = USECNT + 1
               USED(USECNT) = JPOS
 100           CONTINUE
C                                       Compute Lagrangian coefficients
C                                       Interpolate in each row then
C                                       column.
            DO 200 I = 1,3
               DO 190 J = 1,3
                  NUMX = 1.0
                  DENOMX = 1.0
                  GOOD = .FALSE.
                  IF ((XL(I,J).GT.0) .AND. (LIST(XL(I,J)).NE.FBLANK))
     *               THEN
                     DO 130 K = 1,3
                        IF ((K.NE.I) .AND.
     *                     (XL(K,J).GT.0) .AND.
     *                     (LIST(XL(K,J)).NE.FBLANK) .AND.
     *                     (X(XL(K,J)).NE.FBLANK)) THEN
                           NUMX = NUMX * (XC - X(XL(K,J)))
                           DENOMX = DENOMX * (X(XL(I,J)) -
     *                        X(XL(K,J)))
                           GOOD = .TRUE.
                           END IF
 130                    CONTINUE
                     END IF
                  IF (GOOD .AND. (DENOMX.NE.0.0)) THEN
                     PX(I,J) = NUMX / DENOMX
                  ELSE
                     PX(I,J) = 0.0
                     END IF
 190           CONTINUE
 200        CONTINUE
C                                       Get central Y on each row, use
C                                       another if not avaliable.
         DO 210 J = 1,3
            YCR(J) = FBLANK
            IF (XL(2,J).GT.0) YCR(J) = Y(XL(2,J))
            IF ((YCR(J).EQ.FBLANK) .AND. (XL(1,J).GT.0))
     *         YCR(J) = Y(XL(1,J))
            IF ((YCR(J).EQ.FBLANK) .AND. (XL(3,J).GT.0))
     *         YCR(J) = Y(XL(3,J))
 210        CONTINUE
C                                       Interpolation factors in Y
         DO 250 J = 1,3
            NUMY = 1.0
            DENOMY = 1.0
            GOOD = .FALSE.
            IF (YCR(J).NE.FBLANK) THEN
               DO 240 K = 1,3
                  IF ((K.NE.J) .AND. (YCR(K).NE.FBLANK)) THEN
                     NUMY = NUMY * (YC - YCR(K))
                     DENOMY = DENOMY * (YCR(J) - YCR(K))
                     GOOD = .TRUE.
                     END IF
 240              CONTINUE
               END IF
               IF (GOOD .AND. (DENOMY.NE.0.0)) THEN
                  PY(J) = NUMY / DENOMY
               ELSE
                  PY(J) = 0.0
               END IF
 250        CONTINUE
C                                       Interpolate
            SUM = 0.0
            SUMWT = 0.0
            GOOD = .FALSE.
            DO 300 I = 1,3
               DO 290 J = 1,3
                  IF ((PX(I,J)*PY(J).GT.1.0E-10) .AND.
     *               (XL(I,J).GT.0) .AND. (LIST(XL(I,J)).NE.FBLANK))
     *               THEN
                     SUM = SUM + PX(I,J) * PY(J) * LIST(XL(I,J))
                     SUMWT = SUMWT + PX(I,J) * PY(J)
                     GOOD = .TRUE.
                     END IF
 290              CONTINUE
 300           CONTINUE
            IF (GOOD .AND. (ABS (SUMWT).GT.1.0E-20)) THEN
               IMAGE(IX,IY) = SUM / SUMWT
               END IF
 490        CONTINUE
 500     CONTINUE
C
 999  RETURN
      END
