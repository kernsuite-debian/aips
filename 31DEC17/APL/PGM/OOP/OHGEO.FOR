LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER NPARMS
      PARAMETER (NPARMS=18)
      INTEGER AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
      INCLUDE 'INCS:PAOOF.INC'
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK',
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK',
     *   'BLC', 'TRC', 'IMSIZE', 'REWEIGHT', 'AXREF', 'APARM'/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT,
     *   OOACAR, OOACAR, OOAINT, OOAINT,
     *   OOACAR, OOACAR, OOAINT, OOAINT,
     *   OOAINT, OOAINT, OOAINT, OOARE, OOARE, OOARE/
      DATA AVDIM /12,1, 6,1, 1,1, 1,1,
     *   12,1, 6,1, 1,1, 1,1,
     *   12,1, 6,1, 1,1, 1,1,
     *   7,1, 7,1, 2,1, 2,1, 2,1, 10,1/
LOCAL END
      PROGRAM OHGEO
C-----------------------------------------------------------------------
C! Geometric interpolation with correction for 3-D effects
C# Task MAP-UTIL OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2010
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
      CHARACTER PRGM*6, IN*32, OUT*32
      INTEGER  IRET, BUFF1(256), HWIDTH, NAXIS(7)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'OHGEO '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL OHGEIN (PRGM, IN, OUT, HWIDTH, NAXIS, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Interpolate
      CALL IMGHGE (IN, HWIDTH, 1, NAXIS(1), OUT, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL BEMCOP (IN, OUT, IRET)
      IRET = 0
C                                       History
      CALL OHGEHI (IN, OUT)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE OHGEIN (PRGN, IN, OUT, HWIDTH, NAXIS, IRET)
C-----------------------------------------------------------------------
C   OHGEIN gets input parameters for OHGEO and creates the output.
C   Inputs:
C      PRGN     C*6    Program name
C   Output:
C      IN       C*?    Input object
C      OUT      C*?    Output object
C      HWIDTH   I      Interpolation kernel half width.
C      NAXIS    I(7)   Output image size
C      IRET     I      Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   HWIDTH, NAXIS(7), IRET
      CHARACTER PRGN*6, IN*(*), OUT*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NKEY1, NKEY2
C                                       NKEY1=no. adverbs for inname
      PARAMETER (NKEY1=6)
C                                       NKEY2 = no. adverb for in2name
      PARAMETER (NKEY2=4)
      INTEGER   DIM(7), TYPE, DISK, SEQ, BLC(7), TRC(7), IMSIZE(2),
     *   MSGSAV, NAXIS2(7), IROUND, DUMMY, IDISK, ODISK, ICNO, OCNO,
     *   ICX, ICY
      LOGICAL   DO3D, SECOND
      REAL      APARM(10), PA, ZA, CRPIX(7), CRPIX2(7), RTEMP, C123(4),
     *   REWT(2), CROTA(7), CROTA2(7), CDELT(7), CDELT2(7), AXREF(2),
     *   PIX(7), PIX2(7)
      DOUBLE PRECISION OBSRA, OBSDEC, CRVAL(7), CRVAL2(7)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, IN2*32, NAME*12, CLASS*6, INAME*12,
     *   CNAME*8, KEYW*8, CDUMMY*1, CTYPE(7)*8, CTYPE2(7)*8
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs for IN
C                    1          2        3        4        5      6
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'BLC', 'TRC'/
C                                       Rename
C                    1       2       3        4       5      6
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'BLC', 'TRC'/
C                                       Adverbs for in2name
C                    1          2          3          4
      DATA INK2 /'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK'/
C                                       Rename
C                   1       2         3       4
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK'/
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
C                                       Create IN
      IN = 'Input image object'
      CALL CREATE (IN, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, IN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open and close to check
      CALL OOPEN (IN, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (IN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Control parameters
      CALL OGET ('Input', 'APARM', TYPE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'REWEIGHT', TYPE, DIM, REWT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Interpolation kernel half width.
      HWIDTH = IROUND (REWT(1))
      HWIDTH = MIN (4, MAX (1, HWIDTH))
      REWT(1) = HWIDTH
      IF ((REWT(2).LE.0.0) .OR. (REWT(2).GE.1.0)) REWT(2) = 0.3334
      CALL OPUT ('Input', 'REWEIGHT', TYPE, DIM, REWT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (IN, 'RELIABLE', OOARE, DIM, REWT(2), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       3-D corrections?
      DO3D = APARM(1).GT.0.0
      IF (DO3D) THEN
         DIM(1) = 1
         DIM(2) = 1
         CALL OPUT (IN, 'DO3DCOR', OOALOG, DIM, DO3D, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         MSGSAV = MSGSUP
C                                       Parallactic angle
         MSGSUP = 32000
         CALL OGET (IN, 'PARANGLE', TYPE, DIM, PA, CDUMMY, IRET)
         MSGSUP = MSGSAV
         IF (IRET.NE.0) THEN
C                                       Use input
            CALL OPUT (IN, 'PARANGLE', OOARE, DIM, APARM(2), CDUMMY,
     *         IRET)
            END IF
         IF (IRET.NE.0) GO TO 999
C                                       Zenith angle
         MSGSUP = 32000
         CALL OGET (IN, 'ZENANGLE', TYPE, DIM, ZA, CDUMMY, IRET)
         MSGSUP = MSGSAV
C                                       Use input
         IF (IRET.EQ.1) THEN
            CALL OPUT (IN, 'ZENANGLE', OOARE, DIM, APARM(3), CDUMMY,
     *         IRET)
            END IF
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Radial scaling
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (IN, 'PBFWHM', OOARE, DIM, APARM(4), CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Linear scaling
      IF (APARM(9).LE.0.0) APARM(9) = 1.0
      C123(1) = APARM(9)
C                                       Radial scaling?
      IF (APARM(4).GT.0.0) THEN
         CALL RCOPY (3, APARM(6), C123(2))
      ELSE
         CALL RFILL (3, 0.0, C123(2))
         APARM(5) = 0.0
         CALL RFILL (3, 0.0, APARM(6))
         END IF
      DIM(1) = 1
      RTEMP = APARM(5) * APARM(5)
      CALL OPUT (IN, 'FBWSQ', OOARE, DIM, RTEMP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 4
      CALL OPUT (IN, 'C123', OOARE, DIM, C123, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Save APARM for history
      DIM(1) = 10
      CALL OPUT ('Input', 'APARM', OOARE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       IN2
      IN2 = 'Image to emulate'
      CALL CREATE (IN2, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, IN2, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open and close to check
      CALL OOPEN (IN2, 'READ', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (IN2, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Clone output from IN2
      OUT = 'Output interpolated image'
      CALL CREATE (OUT, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy array descriptors
C                                       ERROR ?
      CALL ARDCOP (IN, OUT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Set names
      CALL OGET (IN, 'NAME', TYPE, DIM, DUMMY, INAME, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'OUTNAME', TYPE, DIM, DUMMY, NAME, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'OUTCLASS', TYPE, DIM, DUMMY, CLASS, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'OUTSEQ', TYPE, DIM, SEQ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'OUTDISK', TYPE, DIM, DISK, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NAME.EQ.'    ') NAME = INAME
      IF (CLASS.EQ.'    ') CLASS = PRGN
      DIM(1) = LEN (NAME)
      CALL OPUT (OUT, 'NAME', OOACAR, DIM, DUMMY, NAME, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = LEN (CLASS)
      CALL OPUT (OUT, 'CLASS', OOACAR, DIM, DUMMY, CLASS, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 1
      CALL OPUT (OUT, 'IMSEQ', OOAINT, DIM, SEQ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (OUT, 'DISK', OOAINT, DIM, DISK, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Size
      CALL OGET ('Input', 'IMSIZE', TYPE, DIM, IMSIZE, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'AXREF', TYPE, DIM, AXREF, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      SECOND = (IMSIZE(1).EQ.0) .AND. (IMSIZE(2).EQ.0) .AND.
     *   (AXREF(1).EQ.0) .AND. (AXREF(2).EQ.0)
C                                       Input subimage dimension
      CALL ARRWIN (IN, BLC, TRC, NAXIS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Reference image size
      CALL OGET (IN2, 'ARRAY.ARRAY_DESC.NAXIS', TYPE, DIM, NAXIS2,
     *   CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Output image size
      IF (IMSIZE(1).LE.0) IMSIZE(1) = NAXIS2(1)
      IF (IMSIZE(2).LE.0) IMSIZE(2) = NAXIS2(2)
      NAXIS(1) = IMSIZE(1)
      NAXIS(2) = IMSIZE(2)
      DIM(1) = 7
      CALL OPUT (OUT, 'ARRAY.ARRAY_DESC.NAXIS', OOAINT, DIM, NAXIS,
     *   CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Reference pixel
      CALL OGET (IN, 'IMAGE_DESC.CRPIX', TYPE, DIM, CRPIX, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (IN2, 'IMAGE_DESC.CRPIX', TYPE, DIM, CRPIX2, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      IF (SECOND) THEN
         AXREF(1) = CRPIX2(1)
         AXREF(2) = CRPIX2(2)
         END IF
C                                       Reference value
      CALL OGET (IN, 'IMAGE_DESC.CRVAL', TYPE, DIM, CRVAL, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (IN2, 'IMAGE_DESC.CRVAL', TYPE, DIM, CRVAL2, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Rotation
      CALL OGET (IN, 'IMAGE_DESC.CROTA', TYPE, DIM, CROTA, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (IN2, 'IMAGE_DESC.CROTA', TYPE, DIM, CROTA2, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Increment
      CALL OGET (IN, 'IMAGE_DESC.CDELT', TYPE, DIM, CDELT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (IN2, 'IMAGE_DESC.CDELT', TYPE, DIM, CDELT2, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Type
      CALL OGET (IN, 'IMAGE_DESC.CTYPE', TYPE, DIM, DUMMY, CTYPE, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (IN2, 'IMAGE_DESC.CTYPE', TYPE, DIM, DUMMY, CTYPE2,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Correct other axes for
C                                       subimaging.
      CRPIX(3) = CRPIX(3) - BLC(3) + 1.0
      CRPIX(4) = CRPIX(4) - BLC(4) + 1.0
      CRPIX(5) = CRPIX(5) - BLC(5) + 1.0
      CRPIX(6) = CRPIX(6) - BLC(6) + 1.0
      CRPIX(7) = CRPIX(7) - BLC(7) + 1.0
C                                       Image 2 in first 2 axes
      CTYPE(1) = CTYPE2(1)
      CTYPE(2) = CTYPE2(2)
      CDELT(1) = CDELT2(1)
      CDELT(2) = CDELT2(2)
      CROTA(1) = CROTA2(1)
      CROTA(2) = CROTA2(2)
      CRVAL(1) = CRVAL2(1)
      CRVAL(2) = CRVAL2(2)
C                                       Ref pixel
C                                       Center of subimage
      IF ((AXREF(1).EQ.0.0) .AND. (AXREF(2).EQ.0.0)) THEN
         PIX(1) = (TRC(1) - BLC(1) + 1) / 2 + BLC(1) - 1
         PIX(2) = (TRC(2) - BLC(2) + 1) / 2 + BLC(2)
         PIX(3) = BLC(3)
         PIX(4) = BLC(4)
         PIX(5) = BLC(5)
         PIX(6) = BLC(6)
         PIX(7) = BLC(7)
C                                       Find corresponding pixel in IN2
         CALL PSNCVT (IN, PIX, IN2, PIX2, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Center of image
         ICX = NAXIS(1) / 2
         ICY = NAXIS(2) / 2 + 1
C                                       Shift reference pixel.
         CRPIX(1) = CRPIX2(1) + (ICX - PIX2(1)) - BLC(1) + 1
         CRPIX(2) = CRPIX2(2) + (ICY - PIX2(2)) - BLC(2) + 1
      ELSE
         CRPIX(1) = AXREF(1)
         CRPIX(2) = AXREF(2)
         END IF
C                                       Force full instantiation
      CALL OOPEN (OUT, 'WRIT', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy image descriptors
      CALL IMDCOP (IN, OUT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy Convolving beam
      CALL BEMCOP (IN, OUT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy Observing position
      CALL PSNGET (IN, 'OBSRA', TYPE, DIM, OBSRA, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL PSNPUT (OUT, 'OBSRA', TYPE, DIM, OBSRA, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL PSNGET (IN, 'OBSDEC', TYPE, DIM, OBSDEC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL PSNPUT (OUT, 'OBSDEC', TYPE, DIM, OBSDEC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Save reference pixel.
      DIM(1) = 7
      CALL OPUT (OUT, 'IMAGE_DESC.CRVAL', OOADP, DIM, CRVAL, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (OUT, 'IMAGE_DESC.CRPIX', OOARE, DIM, CRPIX, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (OUT, 'IMAGE_DESC.CDELT', OOARE, DIM, CDELT, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (OUT, 'IMAGE_DESC.CROTA', OOARE, DIM, CROTA, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 8
      DIM(2) = 7
      CALL OPUT (OUT, 'IMAGE_DESC.CTYPE', OOACAR, DIM, DUMMY, CTYPE,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (OUT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy catalog header keywords.
      CALL OBDSKC (IN, IDISK, ICNO, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OBDSKC (OUT, ODISK, OCNO, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL KEYCOP (IDISK, ICNO, ODISK, OCNO, IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE OHGEHI (IN, OUT)
C-----------------------------------------------------------------------
C   Routine to write history file to output.
C   Inputs:
C      IN      C*?  Input object
C      OUT     C*?  Output object
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
C
      INTEGER   NADV
      PARAMETER (NADV=13)
      CHARACTER LIST(NADV)*8
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK', 'BLC', 'TRC',
     *   'IMSIZE', 'REWEIGHT', 'APARM'/
C-----------------------------------------------------------------------
C                                        Copy old history
      CALL OHCOPY (IN, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        New additions - copy adverb
C                                        values.
      CALL OHLIST ('Input', LIST, NADV, OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       copy tables
      CALL IMCALT (IN, OUT, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR COPYING TABLES'
         CALL MSGWRT (6)
         END IF
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // OUT
      CALL MSGWRT (6)
 999  RETURN
      END
