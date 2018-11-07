LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER NPARMS
      PARAMETER (NPARMS=13)
      INTEGER AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
      INCLUDE 'INCS:PAOOF.INC'
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK',
     *   'BLC', 'TRC', 'IMSIZE', 'REWEIGHT', 'APARM'/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT,
     *   OOACAR, OOACAR, OOAINT, OOAINT,
     *   OOAINT, OOAINT, OOAINT, OOARE, OOARE/
      DATA AVDIM /12,1, 6,1, 1,1, 1,1,
     *   12,1, 6,1, 1,1, 1,1,
     *   7,1, 7,1, 2,1, 2,1, 10,1/
LOCAL END
      PROGRAM OGEOM
C-----------------------------------------------------------------------
C! Geometric interpolation with correction for 3-D effects
C# Task MAP-UTIL OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1996-1997, 2010
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
      INTEGER  IRET, BUFF1(256), HWIDTH
      REAL     SHIFT(2), ROTATE
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'OGEOM '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL OGEOIN (PRGM, IN, OUT, SHIFT, ROTATE, HWIDTH, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Interpolate
      CALL IMGINT (IN, SHIFT, ROTATE, HWIDTH, OUT, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL BEMCOP (IN, OUT, IRET)
      IRET = 0
C                                       History
      CALL OGEOHI (IN, OUT)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE OGEOIN (PRGN, IN, OUT, SHIFT, ROTATE, HWIDTH, IRET)
C-----------------------------------------------------------------------
C   OGEOIN gets input parameters for OGEOM and creates the output.
C   Inputs:
C      PRGN     C*6    Program name
C   Output:
C      IN       C*?    Input object
C      OUT      C*?    Output object
C      SHIFT    R(2)   Coordinate shift in arc sec (actually changes
C                      object coordinates)
C      ROTATE   R      Image rotation in degrees
C      HWIDTH   I      Interpolation kernel half width.
C      IRET     I      Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   HWIDTH, IRET
      CHARACTER PRGN*6, IN*(*), OUT*(*)
      REAL      SHIFT(2), ROTATE
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NKEY1
C                                       NKEY1=no. adverbs for inname
      PARAMETER (NKEY1=6)
      INTEGER   DIM(7), TYPE, DISK, SEQ, BLC(7), TRC(7), IMSIZE(2),
     *   NAXIS(7), NAXIS2(7), IROUND, DUMMY, IDISK, INX, INY,
     *   ODISK, ICNO, OCNO, INAXIS(7), BLC2(7), TRC2(7), ROTAX
      REAL      APARM(10), CRPIX(7), CRPIX2(7), REWT(2), CDELT(7),
     *   CDELT2(7), CROTA(7), CROTA2(7), TEMP
      DOUBLE PRECISION OBSRA, OBSDEC, CRVAL(7)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, NAME*12, CLASS*6,
     *   INAME*12, CDUMMY*1
      LOGICAL   KEEPRP
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs for IN
C                    1          2        3        4        5      6
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'BLC', 'TRC'/
C                                       Rename
C                    1       2       3        4       5      6
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'BLC', 'TRC'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
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
C                                       Clone output from IN2
      OUT = 'Output interpolated image'
      CALL CREATE (OUT, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy array descriptors
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
      IF (NAME.EQ.' ') NAME = INAME
      IF (CLASS.EQ.' ') CLASS = PRGN
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
C                                       Input subimage dimension
      CALL ARRWIN (IN, BLC, TRC, NAXIS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Check coordinate type
      CALL OBHGET (IN, CATBLK, IRET)
      IF (IRET.NE.0) GO TO 999
      LOCNUM = 1
      CALL SETLOC (BLC(3), .FALSE.)
      ROTAX = 0
      IF (AXTYP(LOCNUM).EQ.1) THEN
         ROTAX = 2
         IF (CORTYP(LOCNUM).EQ.2) ROTAX = 1
         END IF
C                                       Output image size
      CALL COPY (7, NAXIS, NAXIS2)
      IF (IMSIZE(1).GT.0) NAXIS2(1) = IMSIZE(1)
      IF (IMSIZE(2).GT.0) NAXIS2(2) = IMSIZE(2)
      KEEPRP = (NAXIS2(1).EQ.NAXIS(1)) .AND. (NAXIS2(2).EQ.NAXIS(2))
      DIM(1) = 7
      CALL OPUT (OUT, 'ARRAY.ARRAY_DESC.NAXIS', OOAINT, DIM, NAXIS2,
     *   CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IMSIZE(1) = NAXIS2(1)
      IMSIZE(2) = NAXIS2(2)
C                                       Reference pixel, increment
      CALL OGET (IN, 'ARRAY.ARRAY_DESC.NAXIS', TYPE, DIM, INAXIS,
     *   CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (IN, 'IMAGE_DESC.CRPIX', TYPE, DIM, CRPIX, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (IN, 'IMAGE_DESC.CRVAL', TYPE, DIM, CRVAL, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (IN, 'IMAGE_DESC.CDELT', TYPE, DIM, CDELT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (IN, 'IMAGE_DESC.CROTA', TYPE, DIM, CROTA, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Correct for subimaging.
      CRPIX2(1) = CRPIX(1) - BLC(1) + 1.0
      CRPIX2(2) = CRPIX(2) - BLC(2) + 1.0
      CRPIX2(3) = CRPIX(3) - BLC(3) + 1.0
      CRPIX2(4) = CRPIX(4) - BLC(4) + 1.0
      CRPIX2(5) = CRPIX(5) - BLC(5) + 1.0
      CRPIX2(6) = CRPIX(6) - BLC(6) + 1.0
      CRPIX2(7) = CRPIX(7) - BLC(7) + 1.0
      CALL RCOPY (7, CDELT, CDELT2)
      CALL RCOPY (7, CROTA, CROTA2)
C                                       Check APARMN
      IF (APARM(4).EQ.0.0) APARM(4) = 1.0
      IF (APARM(5).EQ.0.0) APARM(5) = 1.0
C                                       Shift reference pixel.
      IF (.NOT.KEEPRP) THEN
         INX = INAXIS(1) / 2
         INY = INAXIS(2) / 2 + 1
         CRPIX2(1) = NAXIS2(1) / 2
         CRPIX2(2) = NAXIS2(2) / 2 + 1
         CRPIX2(1) = CRPIX2(1) + (CRPIX(1)-INX) * APARM(4)
     *      + APARM(1)
         CRPIX2(2) = CRPIX2(2) + (CRPIX(2)-INY) * (APARM(4) * APARM(5))
     *      + APARM(2)
      ELSE
         CRPIX2(1) = CRPIX2(1) + APARM(1)
         CRPIX2(2) = CRPIX2(2) + APARM(2)
         END IF
      CDELT2(1) = CDELT2(1) / APARM(4)
      CDELT2(2) = CDELT2(2) / APARM(4) / APARM(5)
      TEMP = 1.0
      IF (ROTAX.GT.0) TEMP = COS (DG2RAD * CRVAL(ROTAX))
      ROTATE = APARM(3)
      IF (ROTAX.EQ.2) THEN
         SHIFT(1) = APARM(6) * TEMP
         SHIFT(2) = APARM(7)
         CROTA2(2) = CROTA2(2) + ROTATE
      ELSE IF (ROTAX.EQ.1) THEN
         SHIFT(1) = APARM(6)
         SHIFT(2) = APARM(7) * TEMP
         CROTA2(1) = CROTA2(1) - ROTATE
      ELSE
         SHIFT(1) = APARM(6)
         SHIFT(2) = APARM(7)
         CROTA2(2) = CROTA2(2) + ROTATE
         MSGTXT = 'AIPS DOES NOT UNDERSTAND THIS KIND OF ROTATION'
         IF (ROTATE.NE.0.0) CALL MSGWRT (7)
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
      CALL OPUT (OUT, 'IMAGE_DESC.CRPIX', OOARE, DIM, CRPIX2, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (OUT, 'IMAGE_DESC.CDELT', OOARE, DIM, CDELT2, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (OUT, 'IMAGE_DESC.CROTA', OOARE, DIM, CROTA2, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      CALL FILL (7, 0, BLC2)
      CALL FILL (7, 0, TRC2)
      CALL OPUT (OUT, 'BLC', OOAINT, DIM, BLC2, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (OUT, 'TRC', OOAINT, DIM, TRC2, CDUMMY, IRET)
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
C                                       Save parmS for history
      DIM(1) = 10
      CALL OPUT ('Input', 'APARM', OOARE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 2
      CALL OPUT ('Input', 'IMSIZE', OOAINT, DIM, IMSIZE, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE OGEOHI (IN, OUT)
C-----------------------------------------------------------------------
C   Routine to write history file to output.
C   Inputs:
C      IN      C*?  Input object
C      OUT     C*?  Output object
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
C
      INTEGER   NADV
      PARAMETER (NADV=9)
      CHARACTER LIST(NADV)*8
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'BLC', 'TRC', 'IMSIZE', 'REWEIGHT', 'APARM'/
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
