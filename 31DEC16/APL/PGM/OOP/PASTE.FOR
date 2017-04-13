LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER   NPARMS
      PARAMETER (NPARMS=19)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
      INCLUDE 'INCS:PAOOF.INC'
C                      1        2         3          4        5
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'PIXXY',
C           6          7          8          9          10
     *   'OPCODE', 'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK',
C          11     12      13        14        15
     *   'BLC', 'TRC', 'PIX2XY', 'FACTOR', 'OFFSET',
C            16          17        18         19
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK'/
C                     1       2      3       4       5       6
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOAINT, OOACAR,
C           7       8      9       10      11      12      13
     *   OOACAR, OOACAR, OOAINT, OOAINT, OOAINT, OOAINT, OOAINT,
C          14     15      16       17      18      19
     *   OOARE,  OOARE, OOACAR, OOACAR, OOAINT, OOAINT/
C                   1    2    3    4    5    6    7     8    9
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 7,1, 4,1, 12,1, 6,1, 1,1,
C         10   11   12   13   14   15    16   17   18   19
     *   1,1, 7,1, 7,1, 7,1, 1,1, 1,1, 12,1, 6,1, 1,1, 1,1/
LOCAL END
      PROGRAM PASTE
C-----------------------------------------------------------------------
C! Pastes a selected subimage of one image into another.
C# Map-util Utility Object-Oriented OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-1999, 2010
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
C   Pastes a selected subimage of one image into another.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, SUBIM*32, IMIN*32, IMOUT*32
      INTEGER  IRET, BUFF1(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'PASTE '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL PASTIN (PRGM, SUBIM, IMIN, IMOUT, IRET)
C                                       Paste
      IF (IRET.EQ.0) CALL PASTIT (SUBIM, IMIN, IMOUT, IRET)
C                                       History
      IF (IRET.EQ.0) CALL PASTHI (IMIN, IMOUT)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE PASTIN (PRGN, SUBIM, IMIN, IMOUT, IRET)
C-----------------------------------------------------------------------
C   PASTIN gets input parameters for PASTE and creates the relevant
C   objects.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      SUBIM   C*32 Name of subimage object.
C      IMIN    C*32 Name of input image
C      IMOUT   C*32 Output image (same underlying files as IMIN)
C      IRET    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, SUBIM*(*), IMIN*(*), IMOUT*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NKEY1, NKEY2
C                                       NKEY1=no. adverbs to copy to
C                                       Subimage
      PARAMETER (NKEY1=9)
C                                       NKEY2 = no. adverb for IMIN
      PARAMETER (NKEY2=10)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to subimage
C                                       object
C                    1          1           2         4
      DATA INK1 /'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK',
C          5      6       7         8        9
     *   'BLC', 'TRC', 'PIX2XY', 'FACTOR', 'OFFSET'/
C                                       Rename
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK',
C          5      6       7         8        9
     *   'BLC', 'TRC', 'PIX2XY', 'FACTOR', 'OFFSET'/
C                                       Adverbs for IMIN image object
C                    1         2         3         4        5
      DATA INK2 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'PIXXY',
C           6          7          8           9        10
     *   'OPCODE', 'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK'/
C                    1         2       3       4        5         6
      DATA OUTK2 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'PIXXY', 'OPCODE',
C            7          8          9         10
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create Subimage object
      SUBIM = 'Subimage to paste'
      CALL CREATE (SUBIM, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, SUBIM, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create input image object
      IMIN = 'Input image'
      CALL CREATE (IMIN, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, IMIN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Clone output image object
      IMOUT = 'Output image'
      CALL OCLONE (IMIN, IMOUT, IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE PASTIT (SUBIM, IMIN, IMOUT, IERR)
C-----------------------------------------------------------------------
C   Paste subimage.
C   Inputs:
C      SUBIM   C*?   Subimage object
C      IMIN    C*?   Image into which SUBIM is to be inserted
C      IMOUT   C*?   Output image
C   Output:
C      IERR    I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER SUBIM*(*), IMIN*(*), IMOUT*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   I1, I2, I3, I4, I5, I6, I7, PIXSUB(7), PIXIM(7), TYPE,
     *   DIM(7), BLC(7), TRC(7), NDIM(7), BLCIM(7), TRCIM(7), NDIMIM(7),
     *   IDIM, SUBPNT, IMPNT, IROUND, DUMMY, LIM1, NDIMI
      LOGICAL   DO2, DO3, DO4, DO5, DO6, DO7
      REAL      ROWSUB(MAXIMG), ROWIM(MAXIMG), FACTOR, OFFSET, REFPS(7),
     *   REFPI(7)
      CHARACTER OPCODE*4, CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Get control parameters.
      CALL ARDGET (IMIN, 'NDIM', TYPE, DIM, NDIMI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (IMIN, 'OPCODE', TYPE, DIM, DUMMY, OPCODE, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (IMIN, 'PIXXY', TYPE, DIM, PIXIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (SUBIM, 'PIX2XY', TYPE, DIM, PIXSUB, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (SUBIM, 'FACTOR', TYPE, DIM, FACTOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (SUBIM, 'OFFSET', TYPE, DIM, OFFSET, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open objects
      CALL IMGOPN (SUBIM, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGOPN (IMOUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGOPN (IMIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ARRWIN (SUBIM, BLC, TRC, NDIM, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ARRWIN (IMIN, BLCIM, TRCIM, NDIMIM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default FACTOR
      IF (ABS (FACTOR).LE.1.0E-20) FACTOR = 1.0
C                                       Update Input for history
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT ('Input', 'FACTOR', OOARE, DIM, FACTOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Defaults on alignment pixels
C                                       Get reference pixels
      CALL IMDGET (IMIN, 'CRPIX', TYPE, DIM, REFPI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMDGET (SUBIM, 'CRPIX', TYPE, DIM, REFPS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (NDIMI.LT.7) THEN
         IDIM = 7 - NDIMI
         CALL FILL (IDIM, 1, PIXSUB(NDIMI+1))
         CALL FILL (IDIM, 1, PIXIM(NDIMI+1))
         END IF
      DO 20 IDIM = 1,7
         IF (PIXSUB(IDIM).LE.0) PIXSUB(IDIM) = IROUND (REFPS(IDIM))
         IF (PIXIM(IDIM).LE.0) PIXIM(IDIM) = IROUND (REFPI(IDIM))
 20      CONTINUE
C                                       Update Input for history
      DIM(1) = 7
      DIM(2) = 1
      CALL OPUT ('Input', 'PIXXY', OOAINT, DIM, PIXSUB, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT ('Input', 'PIX2XY', OOAINT, DIM, PIXIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set window in image, must read
C                                       entire planes from IMIN to
C                                       IMOUT.
      BLCIM(1) = 1
      TRCIM(1) = NDIMIM(1)
      BLCIM(2) = 1
      TRCIM(2) = NDIMIM(2)
      DO 50 IDIM = 3,7
         BLCIM(IDIM) = (PIXIM(IDIM) - PIXSUB(IDIM)) + BLC(IDIM)
         TRCIM(IDIM) = (PIXIM(IDIM) - PIXSUB(IDIM)) + TRC(IDIM)
 50      CONTINUE
C                                       Check array compatability
      IF (((PIXIM(1) - (PIXSUB(1) - BLC(1))).LT.1) .OR.
     *   ((PIXIM(1) - (PIXSUB(1) - TRC(1))).GT.NDIMIM(1)) .OR.
     *   ((PIXIM(2) - (PIXSUB(2) - BLC(2))).LT.1) .OR.
     *   ((PIXIM(2) - (PIXSUB(2) - TRC(2))).GT.NDIMIM(2))) THEN
C                                       Have to trim BLC, TRC
         MSGTXT = 'Warning: trimming BLC,TRC to fit output'
         CALL MSGWRT (5)
         IF ((PIXIM(1) - (PIXSUB(1) - BLC(1))) .LT. 1)
     *      BLC(1) = PIXSUB(1) - PIXIM(1) + 1
         IF ((PIXIM(2) - (PIXSUB(2) - BLC(2))) .LT. 1)
     *      BLC(2) = PIXSUB(2) - PIXIM(2) + 1
         IF ((PIXIM(1) - (PIXSUB(1) - TRC(1))).GT.NDIMIM(1))
     *      TRC(1) = PIXSUB(1) - PIXIM(1) + NDIMIM(1)
         IF ((PIXIM(2) - (PIXSUB(2) - TRC(2))).GT.NDIMIM(2))
     *      TRC(2) = PIXSUB(2) - PIXIM(2) + NDIMIM(2)
C                                       Reset BLC, TRC
         DIM(1) = 7
         DIM(2) = 1
         CALL OPUT (SUBIM, 'BLC', OOAINT, DIM, BLC, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OPUT (SUBIM, 'TRC', OOAINT, DIM, TRC, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
      DO 70 IDIM = 2,7
         IF ((BLCIM(IDIM).LT.1) .OR. (BLCIM(IDIM).GT.NDIMIM(IDIM)) .OR.
     *      (TRCIM(IDIM).LT.1) .OR. (TRCIM(IDIM).GT.NDIMIM(IDIM))) THEN
            IERR = 5
            WRITE (MSGTXT,1050) IDIM
            CALL MSGWRT (8)
            IF (IDIM.LE.NDIMI) GO TO 999
            END IF
 70      CONTINUE
C                                       Save BLC, TRC for history
      DIM(1) = 7
      DIM(2) = 1
      CALL OPUT ('Input', 'BLC', OOAINT, DIM, BLC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT ('Input', 'TRC', OOAINT, DIM, TRC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       subimage copying limit
      LIM1 = TRC(1) - BLC(1) + 1
C                                       Loop over image
      DO 700 I7 = 1,NDIMIM(7)
         DO7 = ((I7.GE.BLCIM(7)) .AND. (I7.LE.TRCIM(7)))
         DO 600 I6 = 1,NDIMIM(6)
            DO6 = ((I6.GE.BLCIM(6)) .AND. (I6.LE.TRCIM(6)))
            DO 500 I5 = 1,NDIMIM(5)
               DO5 = ((I5.GE.BLCIM(5)) .AND. (I5.LE.TRCIM(5)))
               DO 400 I4 = 1,NDIMIM(4)
                  DO4 = ((I4.GE.BLCIM(4)) .AND. (I4.LE.TRCIM(4)))
                  DO 300 I3 = 1,NDIMIM(3)
                     DO3 = ((I3.GE.BLCIM(3)) .AND. (I3.LE.TRCIM(3)))
                     DO 200 I2 = 1,NDIMIM(2)
C                                       Read inputs
      CALL ARREAD (IMIN, DIM, ROWIM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Modify this row?
      DO2 = (I2.GE.(PIXIM(2) - (PIXSUB(2) - BLC(2)))) .AND.
     *   (I2.LE.(PIXIM(2) - (PIXSUB(2) - TRC(2))))
      IF (.NOT.(DO2 .AND. DO3 .AND. DO4 .AND. DO5 .AND. DO6 .AND. DO7))
     *   GO TO 290
C                                       Something to do.
      CALL ARREAD (SUBIM, DIM, ROWSUB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Scaling of subimage
         DO 100 I1 = 1,LIM1
            IF (ROWSUB(I1).NE.FBLANK) THEN
               ROWSUB(I1) = ROWSUB(I1) * FACTOR + OFFSET
               END IF
 100        CONTINUE
C                                       Update by OPCODE
      SUBPNT = 1
      IMPNT = PIXIM(1) - (PIXSUB(1) - BLC(1))
C                                       Sum
      IF (OPCODE.EQ.'ADD ') THEN
         DO 110 I1 = 1,LIM1
            IF ((ROWSUB(SUBPNT).NE.FBLANK) .AND.
     *         (ROWIM(IMPNT).NE.FBLANK)) THEN
               ROWIM(IMPNT) = ROWIM(IMPNT) + ROWSUB(SUBPNT)
            ELSE IF (ROWSUB(SUBPNT).NE.FBLANK) THEN
               ROWIM(IMPNT) = ROWSUB(SUBPNT)
               END IF
            SUBPNT = SUBPNT + 1
            IMPNT = IMPNT + 1
 110        CONTINUE
C                                       Replace
      ELSE
         DO 120 I1 = 1,LIM1
            IF (ROWSUB(SUBPNT).NE.FBLANK) THEN
               ROWIM(IMPNT) = ROWSUB(SUBPNT)
               END IF
            SUBPNT = SUBPNT + 1
            IMPNT = IMPNT + 1
 120        CONTINUE
         END IF
 290  CALL ARRWRI (IMOUT, DIM, ROWIM, IERR)
      IF (IERR.NE.0) GO TO 999
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close Array objects
      CALL ARRCLO (SUBIM, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ARRCLO (IMIN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ARRCLO (IMOUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close Image objects
      CALL IMGCLO (SUBIM, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGCLO (IMIN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGCLO (IMOUT, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('IMAGE AND SUBIMAGE ARE INCOMPATIBLE ON AXIS ',I3)
      END
      SUBROUTINE PASTHI (IMIN, IMOUT)
C-----------------------------------------------------------------------
C   Routine to write history file to output CLEAN image object.
C   Inputs:
C      IMIN    C*32 Output image.
C      IMOUT   C*32 Output image.
C-----------------------------------------------------------------------
      CHARACTER IMIN*(*), IMOUT*(*)
C
      INTEGER   NADV
      PARAMETER (NADV=11)
      CHARACTER LIST(NADV)*8, NOTTYP*2
      INTEGER   IERR, DIM(7), DUM
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK', 'BLC',
     *   'TRC',  'PIXXY', 'FACTOR', 'OFFSET', 'PIX2XY', 'OPCODE'/
C-----------------------------------------------------------------------
C                                       Copy history
      CALL OHCOPY (IMIN, IMOUT, IERR)
C                                       Add task label to history
      CALL OHTIME (IMOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        New additions - copy adverb
C                                        values.
      CALL OHLIST ('Input', LIST, NADV, IMOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       copy rest tables
      DIM(1) = 2
      DIM(2) = 1
      NOTTYP = 'CC'
      CALL IMPUT (IMIN, 'DROPTABS', OOACAR, DIM, DUM, NOTTYP, IERR)
      IF (IERR.EQ.0) CALL IMCALT (IMIN, IMOUT, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TROUBLE COPYING OTHER TABLES'
         CALL MSGWRT (6)
         END IF
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // IMOUT
      CALL MSGWRT (4)
C
 999  RETURN
      END
