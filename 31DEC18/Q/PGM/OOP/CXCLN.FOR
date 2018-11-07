LOCAL INCLUDE 'CXCLN.INC'
C                                       Local include for CXCLN
      CHARACTER DIRTYR*36, DIRTYI*36, BEAMR*36, BEAMI*36,
     *   CLEANR*36 , CLEANI*36
      COMMON /CHPARM/ DIRTYR, DIRTYI, BEAMR, BEAMI, CLEANR, CLEANI
LOCAL END
LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER NPARMS
      PARAMETER (NPARMS=19)
      INTEGER AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                                       Uses PAOOF.INC
C                      1        2         3          4
      DATA AVNAME /'INNAME', 'INSEQ', 'INDISK', 'IN2NAME',
C           5          6          7          8        9          10
     *   'IN2SEQ', 'IN2DISK', 'OUTNAME', 'OUTSEQ', 'OUTDISK', 'INVERS',
C          11      12      13       14      15      16      17
     *   'GAIN', 'FLUX', 'NITER', 'BMAJ', 'BMIN', 'BPA', 'NBOXES',
C          18        19
     *   'CLBOX', 'BADDISK'/
     *
C                    1       2       3       4       5       6
      DATA AVTYPE /OOACAR, OOAINT, OOAINT, OOACAR, OOAINT, OOAINT,
C           7      8       9       10      11     12     13      14
     *   OOACAR, OOAINT, OOAINT, OOAINT, OOARE, OOARE, OOAINT, OOARE,
C          15     16     17      18      19
     *   OOARE, OOARE, OOAINT, OOAINT, OOAINT /
C                   1    2    3    4     5    6    7    8    9    10
      DATA AVDIM /12,1, 1,1, 1,1, 12,1, 1,1, 1,1, 12,1, 1,1, 1,1, 1,1,
C        11   12   13   14   15   16   17   18    19
     *   1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 1,1, 4,50, 10,1/
LOCAL END
      PROGRAM CXCLN
C-----------------------------------------------------------------------
C! Complex Hogbom CLEAN
C# Map-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2005-2006, 2008, 2012, 2015
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
C   Hogbom cleam of a complex image
C   Inputs:
C                                   Dirty image
C   INNAME                                Image name (name), class
C                                         must be 'QIM001' and 'UIM001'.
C   INSEQ              0.0      9999.0    Image name (seq. #)
C   INDISK             0.0         9.0    Image disk drive #
C                                      Beam image
C   IN2NAME                               Image name (name), class
C                                         must be 'QBM001' and
C                                         'UBM001'.
C   IN2SEQ             0.0      9999.0    Image name (seq. #)
C   IN2DISK            0.0         9.0    Image disk drive #
C                                      Clean image
C   OUTNAME                               Image name (name), classes
C                                         will be 'QCL001' and 'UCL001'.
C   OUTSEQ            -1.0      9999.0    Image name (seq. #)
C   OUTDISK            0.0         9.0    Image disk drive #
C   INVERS            -1.0       255.0 CX file ver. no.
C
C   GAIN               0.0         2.0 CLEAN loop gain
C   FLUX               0.0             Min. residual in window (Jy)
C   NITER                              Maximum # of CLEAN components
C   BMAJ            -999.9             FWHM(asec) maj. axis CLEAN
C                                      restoring beam. MUST BE
C                                      PROVIDED.
C   BMIN            -999.9             FWHM(asec) min. axis CLEAN
C                                      restoring beam.
C   BPA             -360.0       360.0 CLEAN beam position angle
C   NBOXES             0.0        50.0 Number of boxes for CLEAN
C   CLBOX              0.0      4096.0 Four coordinates for each box
C   BADDISK           -1.0      1000.0 Disks to avoid for scratch.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, DIRTY*36, BEAM*36, CLEAN*36
      INTEGER  IRET, BUFF1(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'CXCLN.INC'
      DATA PRGM /'CXCLN '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL CXCIN (PRGM, DIRTY, BEAM, CLEAN, IRET)
C                                       CLEAN
      IF (IRET.EQ.0) CALL CXLEAN (DIRTY, BEAM, CLEAN, IRET)
C                                       History
      IF (IRET.EQ.0) CALL CXCHIS (DIRTY, CLEAN)
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE CXCIN (PRGN, DIRTY, BEAM, CLEAN, IRET)
C-----------------------------------------------------------------------
C   CXCIN gets input parameters for CXCLN and creates the input and
C   output objects
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IRET    I    Error code: 0 => ok
C                               4 => user routine detected error.
C                               5 => catalog troubles
C                               8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, DIRTY*(*), BEAM*(*), CLEAN*(*)
C
      INTEGER   NKEY1, NKEY2
C                                       NKEY1=no. adverbs to copy to
C                                       DIRTY
      PARAMETER (NKEY1=15)
C                                       NKEY2=no. adverbs to copy to
C                                       BEAM
      PARAMETER (NKEY2=3)
      INTEGER   IERR, DIM(7), TYPE, NITER, INVER, DUMMY
      REAL      BMAJ
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INK2(NKEY2)*8,
     *   OUTK2(NKEY2)*32, CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'CXCLN.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to dirty objects
C                    1         2         3      4       5        6
      DATA INK1 /'INNAME', 'INSEQ', 'INDISK', 'GAIN', 'FLUX', 'NITER',
C           7        8       9     10         11
     *    'BMAJ', 'BMIN', 'BPA', 'NBOXES', 'CLBOX',
C            12         13         14        15
     *   'OUTNAME',  'OUTSEQ', 'OUTDISK', 'INVERS' /
C                   1        2       3
      DATA OUTK1 /'NAME', 'IMSEQ', 'DISK',
C           4       5       6        7        8        9
     *   'GAIN', 'FLUX', 'NCLEAN', 'BBMAJ', 'BBMIN', 'BBPA',
C          10         11       12          13         14       15
     *   'NBOXES', 'CLBOX', 'OUTNAME',  'OUTSEQ', 'OUTDISK', 'VER'/
C                                       Adverbs to copy to Beam objects
C                     1          2          3
      DATA INK2 /'IN2NAME', 'IN2SEQ', 'IN2DISK'/
C                    1       2       3
      DATA OUTK2 /'NAME', 'IMSEQ', 'DISK'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       BADDISK
      CALL OGET ('Input', 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IERR)
      IF (IRET.NE.0) GO TO 999
C                                       Check inputs and set defaults
C                                       Number of iterations.
      CALL OGET ('Input', 'NITER', TYPE, DIM, NITER, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NITER.LE.0) THEN
         MSGTXT = 'NO DEFAULT NUMBER OF ITERATIONS, SPECIFY NITER'
         CALL MSGWRT (8)
         IRET = 9
         GO TO 999
         END IF
C                                       CLEAN BEAM
      CALL OGET ('Input', 'BMAJ', TYPE, DIM, BMAJ, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (BMAJ.LE.0.0) THEN
         MSGTXT = 'NO DEFAULT RESTORING BEAM, SPECIFY BMAJ,BMIN,BPA'
         CALL MSGWRT (8)
         IRET = 9
         GO TO 999
         END IF
C                                       CX table version
      CALL OGET ('Input', 'INVERS', TYPE, DIM, INVER, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (INVER.LE.0) THEN
         INVER = 1
         CALL OPUT ('Input', 'INVERS', OOAINT, DIM, INVER, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Create input objects: Dirty
C                                       image parts.
      DIRTYR = 'Dirty Real'
      CALL CREATE (DIRTYR, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
      DIRTYI = 'Dirty Imaginary'
      CALL CREATE (DIRTYI, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, DIRTYR, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, DIRTYI, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Add AIPS file classes.
      DIM(1) = 6
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (DIRTYR, 'FILE_NAME.CLASS', OOACAR, DIM, DUMMY,
     *   'QIM001', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (DIRTYI, 'FILE_NAME.CLASS', OOACAR, DIM, DUMMY,
     *   'UIM001', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (DIRTYR, 'OUTCLASS', OOACAR, DIM, DUMMY, 'QCL001', IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (DIRTYI, 'OUTCLASS', OOACAR, DIM, DUMMY, 'UCL001', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Dirty beam
      BEAMR = 'Beam Real'
      CALL CREATE (BEAMR, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
      BEAMI = 'Beam Imaginary'
      CALL CREATE (BEAMI, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, BEAMR, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL IN2OBJ ('Input', NKEY2, INK2, OUTK2, BEAMI, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Add AIPS file classes.
      DIM(1) = 6
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (BEAMR, 'FILE_NAME.CLASS', OOACAR, DIM, DUMMY, 'QBM001',
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (BEAMI, 'FILE_NAME.CLASS', OOACAR, DIM, DUMMY, 'UBM001',
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create Output Object - attached
C                                       output file naming to DIRTY
      CLEANR = 'Clean real'
      CALL OCLONE (DIRTYR, CLEANR, IRET)
      IF (IRET.NE.0) GO TO 999
      CLEANI = 'Clean Imaginary'
      CALL OCLONE (DIRTYI, CLEANI, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create complex image objects
      DIRTY = 'Dirty image'
      CALL CIMCRE (DIRTY, DIRTYR, DIRTYI, IRET)
      IF (IRET.NE.0) GO TO 999
      BEAM = 'Beam'
      CALL CIMCRE (BEAM, BEAMR, BEAMI, IRET)
      IF (IRET.NE.0) GO TO 999
      CLEAN = 'Clean image'
      CALL CIMCRE (CLEAN, CLEANR, CLEANI, IRET)
      IF (IRET.NE.0) GO TO 999
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE CXLEAN (DIRTY, BEAM, CLEAN, IERR)
C-----------------------------------------------------------------------
C   Does a Hogbom complex deconvolution.
C   Inputs:
C      DIRTY   C*?  Name of Complex image object for the dirty image.
C      BEAM    C*?  Name of Complex image object for the dirty Beam.
C   Output:
C      DIRTY   C*?  Name of Complex image object for the CLEAN image.
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER DIRTY*(*), BEAM*(*), CLEAN*(*)
      INTEGER   IERR
C
      INTEGER   APRES, APBEAM, NAXIS(7), NX, NY, DIM(7), VER, NBOXES,
     *   CLBOX(4,50), NITER, I1, I2, I3, TYPE, DUMMY, LERR
      REAL      GAIN, BMAJ, BMIN, BPA, CELLS(2), FLUX, SUMQ, SUMU
      CHARACTER COMP*32, RCLEAN*32, CDUMMY
      LOGICAL   APOPEN
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DAPM.INC'
C-----------------------------------------------------------------------
      APOPEN = .FALSE.
C                                       Get cleaning parameters from
C                                       dirty object
      CALL CXINFO (DIRTY, VER, GAIN, NBOXES, CLBOX, BMAJ, BMIN, BPA,
     *   NITER, FLUX, NAXIS, CELLS, IERR)
      IF (IERR.NE.0) GO TO 990
      NX = NAXIS(1)
      NY = NAXIS(2)
C                                       Attach CX table to Real part of
C                                       clean image.
      CALL OGET (CLEAN, 'REALPART', TYPE, DIM, DUMMY, RCLEAN, IERR)
      IF (IERR.NE.0) GO TO 990
      COMP = 'CX table'
      CALL CXTSET (COMP, VER, RCLEAN, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Init "AP"
      I1 = 101 + 4 * NX * NY
      I1 = I1/1024 + 4
      CALL QINIT (I1, I2, I3)
      IF ((I3.EQ.0) .OR. (PSAPNW.LT.I1)) THEN
          IERR = 8
          MSGTXT = 'CXLEAN CANNOT GET REQUIRED MEMORY'
          CALL MSGWRT (8)
          GO TO 990
          END IF
      CALL APOBJ ('OPEN', 'CXLEAN', IERR)
      IF (IERR.NE.0) GO TO 990
      APOPEN = .TRUE.
C                                       Copy Dirty image to the "AP"
      APRES = 100
      CALL LIMAGE (DIRTY, APRES, NX, NY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy Dirty beam to the "AP"
      APBEAM = APRES + (2 * NX * NY) + 1
      CALL LIMAGE (BEAM, APBEAM, NX, NY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Clean
      CALL CCLEAN (COMP, APRES, APBEAM, NX, NY, GAIN, NBOXES, CLBOX,
     *   NITER, FLUX, CELLS, SUMQ, SUMU, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Restore if requested
      IF (BMAJ.GT.1.0E-20) THEN
         CALL CRESTR (COMP, APRES, APBEAM, NX, NY, BMAJ, BMIN, BPA,
     *      NITER, CELLS, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Store CLEAN info
         CALL BEMSET (CLEAN, BMAJ, BMIN, BPA, NITER, CELLS, IERR)
         END IF
C                                       Store restored image
      CALL SIMAGE (CLEAN, APRES, NX, NY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        Save Sum of Q and U components.
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (CLEAN, 'SUMQ', OOARE, DIM, SUMQ, CDUMMY, IERR)
      CALL OPUT (CLEAN, 'SUMU', OOARE, DIM, SUMU, CDUMMY, IERR)
      IERR = 0
C
C                                       "Release AP"
 990  IF (APOPEN) THEN
         CALL QRLSE
         CALL APOBJ ('FREE', 'CXLEAN', LERR)
         END IF
      IF (IERR.NE.0) THEN
         MSGTXT = 'CXLEAN: ERROR COMPLEX CLEANING ' // DIRTY
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
      END
      SUBROUTINE CXCHIS (DIRTY, CLEAN)
C-----------------------------------------------------------------------
C   Routine to write history file to output image object.
C   History is written to both Q and U components of CLEAN.
C   Inputs:
C      DIRTY   C*?  Complex dirty image
C      CLEAN   C*?  Complex CLEAN image.
C-----------------------------------------------------------------------
      CHARACTER DIRTY*(*), CLEAN*(*)
C
      INTEGER   NADV
      PARAMETER (NADV=13)
      CHARACTER RDIRTY*36, RCLEAN*36, ICLEAN*36, LIST(NADV)*8, CDUMMY*1,
     *   HILINE*72
      INTEGER   IERR, TYPE, DIM(7), DUMMY
      REAL      SUMQ, SUMU
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INSEQ', 'IN2NAME', 'IN2SEQ', 'INVERS',
     *   'GAIN', 'FLUX', 'NITER',  'BMAJ', 'BMIN', 'BPA', 'NBOXES',
     *   'CLBOX'/
C-----------------------------------------------------------------------
C                                        Get names of component parts.
      CALL OGET (DIRTY, 'REALPART', TYPE, DIM, DUMMY, RDIRTY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (CLEAN, 'REALPART', TYPE, DIM, DUMMY, RCLEAN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (CLEAN, 'IMAGPART', TYPE, DIM, DUMMY, ICLEAN, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        Copy old history to Q (real)
C                                        CLEAN
      CALL OHCOPY (RDIRTY, RCLEAN, IERR)
      IF (IERR.NE.0) GO TO 990
C                                        New additions - copy adverb
C                                        values.
      CALL OHLIST ('Input', LIST, NADV, RCLEAN, IERR)
      IF (IERR.NE.0) GO TO 500
C                                        Summed Q and U components
      CALL OGET (CLEAN, 'SUMQ', TYPE, DIM, SUMQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 500
      CALL OGET (CLEAN, 'SUMU', TYPE, DIM, SUMU, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 500
      WRITE(HILINE,1000) SUMQ
      CALL OHWRIT (HILINE, RCLEAN, IERR)
      IF (IERR.NE.0) GO TO 500
      WRITE(HILINE,1001) SUMU
      CALL OHWRIT (HILINE, RCLEAN, IERR)
      IF (IERR.NE.0) GO TO 500
C                                       Copy to imaginary (U) part
 500  CALL OHCOPY (RCLEAN, ICLEAN, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // CLEAN
      CALL MSGWRT (4)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (' / Sum of Q components = ',1PE12.5,' Jy')
 1001 FORMAT (' / Sum of U components = ',1PE12.5,' Jy')
      END
      SUBROUTINE CXINFO (IMAGE, VER, GAIN, NBOXES, CLBOX, BMAJ, BMIN,
     *   BPA, NITER, FLUX, NAXIS, CELLS, IERR)
C-----------------------------------------------------------------------
C   Gets cleaning info from image.  Looks at real component.
C   Any defaults that are set are also changed in the 'Input' object.
C   Inputs:
C      IMAGE   C*?  Name of Complex image object.
C   Output:
C      VER     I    CX Table version number
C      GAIN    R    Loop gain
C      NBOXES  I    Number of clean boxes
C      CLBOX   I(4,50) CLEAN boxes
C      BMAJ    R    Beam major axis in cells
C      BMIN    R    Beam minor axis in cells
C      BPA     R    Beam position angle in degrees
C      NITER   I    Number of iterations
C      FLUX    R    Minimum residual brightness
C      NAXIS   I(7) Array dimensions
C      CELLS   R(2) Image cell spacings on 1st 2 axes.
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER IMAGE*(*)
      INTEGER   VER, NITER, NBOXES, CLBOX(4,50), NAXIS(7), IERR
      REAL      GAIN, BMAJ, BMIN, BPA, FLUX, CELLS(2)
C
      INTEGER   DIM(7), TYPE, NX, NY
      REAL      TEMP(7)
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
C                                       CX version
      CALL OGET (IMAGE, 'REAL.VER', TYPE, DIM, VER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       GAIN
      CALL OGET (IMAGE, 'REAL.GAIN', TYPE, DIM, GAIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default GAIN
      IF (GAIN.LE.0.0) THEN
         GAIN = 0.1
         CALL OPUT ('Input', 'GAIN', OOARE, DIM, GAIN, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       BEAM
      CALL OGET (IMAGE, 'REAL.BBMAJ', TYPE, DIM, BMAJ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (IMAGE, 'REAL.BBMIN', TYPE, DIM, BMIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (IMAGE, 'REAL.BBPA', TYPE, DIM, BPA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       FLUX
      CALL OGET (IMAGE, 'REAL.FLUX', TYPE, DIM, FLUX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       NITER
      CALL OGET (IMAGE, 'REAL.NCLEAN', TYPE, DIM, NITER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Box
      CALL OGET (IMAGE, 'REAL.NBOXES', TYPE, DIM, NBOXES, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (IMAGE, 'REAL.CLBOX', TYPE, DIM, CLBOX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Dimensions
      CALL OGET (IMAGE, 'REAL.ARRAY.ARRAY_DESC.NAXIS', TYPE, DIM, NAXIS,
     *   CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default NBOXES, CLBOX
      IF ((NBOXES.LE.0) .OR. (CLBOX(1,1).LE.0) .OR. (CLBOX(2,1).LE.0)
     *   .OR. (CLBOX(3,1).LE.0) .OR. (CLBOX(4,1).LE.0)) THEN
         NX = NAXIS(1)
         NY = NAXIS(2)
         NBOXES = 1
         CLBOX(1,1) = 1 + NX/4
         CLBOX(2,1) = 1 + NY/4
         CLBOX(3,1) = NX - NX/4
         CLBOX(4,1) = NY - NY/4
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL OPUT ('Input', 'NBOXES', OOAINT, DIM, NBOXES, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         DIM(1) = 4
         DIM(2) = 50
         DIM(3) = 0
         CALL OPUT ('Input', 'CLBOX', OOAINT, DIM, CLBOX, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Cell spacing
      CALL OGET (IMAGE, 'REAL.IMAGE_DESC.CDELT', TYPE, DIM, TEMP,
     *   CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CELLS(1) = TEMP(1)
      CELLS(2) = TEMP(2)
      IF (ABS (CELLS(1)).LT.1.0E-20) CELLS(1) = 1.0
      IF (ABS (CELLS(2)).LT.1.0E-20) CELLS(2) = 1.0
C                                       Convert beam to cells on axis 1
      BMAJ = (BMAJ / 3600.0) / ABS (CELLS(1))
      BMIN = (BMIN / 3600.0) / ABS (CELLS(1))
C
 999  RETURN
      END
      SUBROUTINE BEMSET (IMAGE, BMAJ, BMIN, BPA, NITER, CELLS, IERR)
C-----------------------------------------------------------------------
C   Saves cleaning info to image.
C   Inputs:
C      IMAGE   C*?  Name of Complex image object.
C      BMAJ    R    Beam major axis in cells
C      BMIN    R    Beam ninoM axis in cells
C      BPA     R    Beam position angle in degrees
C      NITER   I    Number of iterations
C      CELLS   R(2) Image cell spacings on 1st 2 axes.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER IMAGE*(*)
      INTEGER   NITER, IERR
      REAL      BMAJ, BMIN, BPA, CELLS(2)
C
      INTEGER   DIM(7)
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
C                                       Open image
      CALL OOPEN (IMAGE, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Real part (Q)
C                                       BEAM
      BMAJ = BMAJ * ABS (CELLS(1))
      BMIN = BMIN * ABS (CELLS(1))
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (IMAGE, 'REAL.BEAM.BMAJ', OOARE, DIM, BMAJ, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (IMAGE, 'REAL.BEAM.BMIN', OOARE, DIM, BMIN, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (IMAGE, 'REAL.BEAM.BPA', OOARE, DIM, BPA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       NITER
      CALL OPUT (IMAGE, 'REAL.BEAM.NITER', OOAINT, DIM, NITER, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Imaginary part (U)
C                                       BEAM
      CALL OPUT (IMAGE, 'IMAG.BEAM.BMAJ', OOARE, DIM, BMAJ, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (IMAGE, 'IMAG.BEAM.BMIN', OOARE, DIM, BMIN, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (IMAGE, 'IMAG.BEAM.BPA', OOARE, DIM, BPA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       NITER
      CALL OPUT (IMAGE, 'IMAG.BEAM.NITER', OOAINT, DIM, NITER, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close image
      CALL OCLOSE (IMAGE, IERR)
C
 999  RETURN
      END
      SUBROUTINE CCLEAN (COMP, APRES, APBEAM, NX, NY, GAIN,
     *   NBOXES, CLBOX, NITER, FLUX, CELLS, SUMQ, SUMU, IERR)
C-----------------------------------------------------------------------
C   Does Hogbom complex clean of residual and beam in AP memory.
C   Inputs:
C      COMP    C*?  Name of CX table object.
C      APRES   I    AP base address of dirty/residual image.
C      APBEAM  I    AP base address of Beam
C      NX      I    Dimension of image in X direction.
C      NY      I    Dimension of image in Y direction.
C      GAIN    R    Clean loop gain
C      NBOXES  I    Number of clean boxes
C      CLBOX   I(4,50)  Windows of boxes
C      NITER   I    Number of iterations
C      FLUX    R    Minimum residual brightness
C      CELLS   R(2) Cell spacing in degrees
C   Output:
C      SUMQ    R    Sum of "Q" components
C      SUMU    R    Sum of "U" components
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER COMP*(*)
      INTEGER   APRES, APBEAM, NX, NY, NBOXES, CLBOX(4,50), NITER, IERR
      REAL      GAIN, FLUX, CELLS(2), SUMQ, SUMU
C
      INTEGER   LOOP, CXNUM
      REAL      X, Y, RE, IM, XT, YT, RESAMP
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'Finding and subtracting Complex CLEAN components'
      CALL MSGWRT (4)
      SUMQ = 0.0
      SUMU = 0.0
C                                       Open CX table
      CALL OOPEN (COMP, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 999
      DO 100 LOOP = 1,NITER
C                                       Find next max
         CALL CXCFND (APRES, NX, NBOXES, CLBOX, X, Y, RE, IM)
C                                       Save CX component
         CXNUM = LOOP
C                                       Minimum FLUX?
         RESAMP = SQRT (RE*RE + IM*IM)
         IF (RESAMP.LT.FLUX) THEN
            NITER = LOOP - 1
            GO TO 200
            END IF
C                                       Apply loop gain
         RE = RE * GAIN
         IM = IM * GAIN
         XT = (X - NX/2) * CELLS(1)
         YT = (Y - NY/2-1) * CELLS(2)
         CALL CXTPUT (COMP, CXNUM, XT, YT, RE, IM, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Accumulate
         SUMQ = SUMQ + RE
         SUMU = SUMU + IM
C                                       Subtract component
         CALL CXCSUB (APRES, APBEAM, NX, NY, X, Y, RE, IM)
C                                       Progress report every 100
         IF (MOD (LOOP, 100) .EQ. 0) THEN
            WRITE (MSGTXT,1100) LOOP, SUMQ, SUMU
            CALL MSGWRT (4)
            END IF
 100     CONTINUE
C                                       Close CX table
 200  CALL OCLOSE (COMP, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Tell results
      WRITE (MSGTXT,1200) SUMQ, SUMU
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('At ',I5,' comps, Sum Q=', 1PE12.5, ', Sum U=', E12.5,
     *   ' Jy')
 1200 FORMAT ('Sum Q = ', 1PE12.5, ', Sum U = ', E12.5, ' Jy')
      END
      SUBROUTINE CXCFND (APRES, NX, NBOXES, CLBOX, X, Y, RE, IM)
C-----------------------------------------------------------------------
C   Finds next maximum modulus residual in a set of boxes.
C   Inputs:
C      APRES   I    AP base address of dirty/residual image.
C      NX      I    Dimension of image in X direction.
C      NBOXES  I    Number of clean boxes
C      CLBOX   I(4,50)  Windows of boxes
C   Output:
C      X       R    X coordinate in pixels
C      Y       R    Y coordinate in pixels
C      RE      R    Real part of peak residual
C      IM      R    Imaginary part of peak residual
C-----------------------------------------------------------------------
      INTEGER   APRES, NX, NBOXES, CLBOX(4,50)
      REAL      X, Y, RE, IM
C
      INTEGER   BLOOP, YLOOP, APPOS, LROW, APWORK, TIMAX, IMAX, IX, IY
      REAL      TCXMAX, CXMAX, RES(2)
C-----------------------------------------------------------------------
      APWORK = 1
      CXMAX = -1.0
      IMAX = APRES
C                                       Loop over boxes
      DO 200 BLOOP = 1,NBOXES
         LROW = CLBOX(3,BLOOP) - CLBOX(1,BLOOP) + 1
         APPOS = APRES + (CLBOX(2,BLOOP)-1) * 2 * NX +
     *      (CLBOX(1,BLOOP) - 1) * 2
C                                       Loop over Y
         DO 100 YLOOP = CLBOX(2,BLOOP),CLBOX(4,BLOOP)
            CALL QCVMMA (APPOS, 2, APWORK, LROW)
            CALL QGET (TCXMAX, APWORK, 1, 2)
C                                       New max?
            IF (TCXMAX.GT.CXMAX) THEN
               CALL QGET (TIMAX, APWORK+1, 1, 1)
               CXMAX = TCXMAX
               IMAX = TIMAX
               END IF
            APPOS = APPOS + 2 * NX
 100        CONTINUE
 200     CONTINUE
C                                       Get residual
      CALL QGET (RES, IMAX, 2, 2)
      RE = RES(1)
      IM = RES(2)
      IMAX = ((IMAX - APRES) / 2) + 1
      IY = IMAX / NX
      IX = (IMAX - IY * NX)
      X = IX
      Y = IY + 1
C
 999  RETURN
      END
      SUBROUTINE CXCSUB (APRES, APBEAM, NX, NY, X, Y, RE, IM)
C-----------------------------------------------------------------------
C   Multiplies an image by a point and subtracts from another image.
C   May be use for either removing a compontent or restoring one.
C   Inputs:
C      APRES   I    AP base address of residual/restored image.
C      APBEAM  I    AP base address of dirty/restoring beam
C      NX      I    Dimension of image in X direction.
C      NY      I    Dimension of image in Y direction.
C      X       R    X coordinate in pixels
C      Y       R    Y coordinate in pixels
C      RE      R    Real part of peak residual
C      IM      R    Imaginary part of peak residual
C-----------------------------------------------------------------------
      INTEGER   APRES, APBEAM, NX, NY
      REAL      X, Y, RE, IM
C
      INTEGER   YLOOP, APPOSR, APPOSB, APWORK, IX, IY, IX1, IX2,
     *   LX, IY1, IY2, IYOFF, IXOFF, INC
      REAL      RES(2)
C-----------------------------------------------------------------------
C                                       Component to AP
      APWORK = 1
      RES(1) = -RE
      RES(2) = -IM
      CALL QPUT (RES, APWORK, 2, 2)
C                                       Setup for indexing
      IX = X + 0.5
      IY = Y + 0.5
      IYOFF = -(IY - NY/2 - 1)
      IXOFF = -(IX - NX/2)
      IY1 = MAX (1, (1 - IYOFF))
      IY2 = MIN (NY, (NY - IYOFF))
      IX1 = MAX (1, (1 - IXOFF))
      IX2 = MIN (NX, (NX - IXOFF))
      LX = IX2 - IX1 + 1
      INC = 2 * NX
      APPOSR = APRES + (IY1-1) * NX * 2 + (IX1-1) * 2
      APPOSB = APBEAM + (IY1-1 + IYOFF) * NX * 2 + (IX1-1 + IXOFF) * 2
C                                       Loop over Y
      DO 100 YLOOP = IY1,IY2
         CALL QCVSMA (APPOSB, 2, APWORK, APPOSR, 2, APPOSR, 2, LX)
         APPOSR = APPOSR + INC
         APPOSB = APPOSB + INC
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CRESTR (COMP, APRES, APBEAM, NX, NY, BMAJ, BMIN, BPA,
     *   NITER, CELLS, IERR)
C-----------------------------------------------------------------------
C   Restores a set of complex residuals with a real beam.
C   Inputs:
C      COMP    C*?  Name of CX table object.
C      APRES   I    AP base address of dirty/residual image.
C      APBEAM  I    AP base address to use for gaussian
C      NX      I    Dimension of image in X direction.
C      NY      I    Dimension of image in Y direction.
C      BMAJ    R    Restoring Gaussian major axis in cells.
C      BMIN    R    Restoring Gaussian minor axis in cells.
C      BPA     R    Restoring Gaussian position angle in degrees
C      NBOXES  I    Number of clean boxes
C      NITER   I    Number of iterations
C      CELLS   R(2) Cell spacing in degrees
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER COMP*(*)
      INTEGER   APRES, APBEAM, NX, NY, NITER, IERR
      REAL      BMAJ, BMIN, BPA, CELLS(2)
C
      INTEGER LOOP, CXNUM
      REAL X, Y, RE, IM
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'Restoring with Gaussian'
      CALL MSGWRT (4)
C                                       Make gaussian restoring image
      CALL QCGAUS (APBEAM, NX, NY, BMAJ, BMIN, BPA)
C                                       Open CX table
      CALL OOPEN (COMP, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
      DO 100 LOOP = 1,NITER
C                                       Fetch CX component
         CXNUM = LOOP
         CALL CXTGET (COMP, CXNUM, X, Y, RE, IM, IERR)
         IF (IERR.NE.0) GO TO 999
         X = X / CELLS(1) + NX/2
         Y = Y / CELLS(2) + NY/2+1
C                                       Add component
         RE = -RE
         IM = -IM
         CALL CXCSUB (APRES, APBEAM, NX, NY, X, Y, RE, IM)
 100     CONTINUE
C                                       Close CX table
      CALL OCLOSE (COMP, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE LIMAGE (IMAGE, APPOS, NX, NY, IERR)
C-----------------------------------------------------------------------
C   Loads image into AP.
C   Inputs:
C      IMAGE   C*?  Name of Complex image object.
C      APPOS   I    AP base address of image in AP
C      NX      I    Dimension of image in X direction.
C      NY      I    Dimension of image in Y direction.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER IMAGE*(*)
      INTEGER   APPOS, NX, NY, IERR
C
      INCLUDE 'INCS:PMAD.INC'
      REAL      ROW(2,MAXIMG)
      INTEGER   IAP, LOOP, DIM(7), NX2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                       Check AP size
      IF ((PSAPNW*1024).LT.(APPOS+(NX*NY*2))) THEN
         IERR = 5
         MSGTXT = 'AP SIZE TOO SMALL FOR PROBLEM'
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Open image
      CALL OOPEN (IMAGE, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
      IAP = APPOS
      NX2 = 2 * NX
      DO 100 LOOP = 1,NY
         CALL CIGETX (IMAGE, DIM, ROW, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL QPUT (ROW, IAP, NX2, 2)
         IAP = IAP + NX2
 100     CONTINUE
C                                       Close image
      CALL OCLOSE (IMAGE, IERR)
C
 999  RETURN
      END
      SUBROUTINE SIMAGE (IMAGE, APPOS, NX, NY, IERR)
C-----------------------------------------------------------------------
C   Stores image from AP.
C   Inputs:
C      IMAGE   C*?  Name of Complex image object.
C      APPOS   I    AP base address of image in AP
C      NX      I    Dimension of image in X direction.
C      NY      I    Dimension of image in Y direction.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER IMAGE*(*)
      INTEGER   APPOS, NX, NY, IERR
C
      INCLUDE 'INCS:PMAD.INC'
      REAL      ROW(2,MAXIMG)
      INTEGER   IAP, LOOP, DIM(7), NX2
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Open image
      CALL OOPEN (IMAGE, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 999
      IAP = APPOS
      DIM(1) = NX
      DIM(2) = 0
      NX2 = 2 * NX
      DO 100 LOOP = 1,NY
         CALL QGET (ROW, IAP, NX2, 2)
         CALL CIPUTX (IMAGE, DIM, ROW, IERR)
         IF (IERR.NE.0) GO TO 999
         IAP = IAP + NX2
 100     CONTINUE
C                                       Close image
      CALL OCLOSE (IMAGE, IERR)
C
 999  RETURN
      END
      SUBROUTINE CXTSET (NAME, VER, IMAGE, IERR)
C-----------------------------------------------------------------------
C   Sets up CX (Complex CLEAN component) table object.
C   Inputs:
C      NAME    C*?  CX table object name.
C      VER     I    CX table version number
C      IMAGE   C*?  Image object associated with CX table
C   Output:
C      IERR    I    Return error code, 0=>OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), IMAGE*(*)
      INTEGER   VER, IERR
C
      INTEGER MAXCXC
C                                       MAXCXC = number of columns.
      PARAMETER (MAXCXC = 4)
      CHARACTER TITLE(MAXCXC)*24, CTITLE*(MAXCXC*24), TTITLE*56,
     *   UNITS(MAXCXC)*8, CUNITS*(MAXCXC*8), TNAME*12, TCLASS*6,
     *   CDUMMY*1
      INTEGER   DIM(7), COLTYP(MAXCXC), COLDIM(MAXCXC), TSEQ, TDISK,
     *   NCOL, TYPE, DUMMY
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PAOOF.INC'
      EQUIVALENCE (TITLE, CTITLE), (UNITS, CUNITS)
      DATA TTITLE /'AIPS COMPLEX CLEAN COMPONENT TABLE'/
      DATA COLTYP /2, 2, 2, 2/
      DATA COLDIM /1, 1, 1, 1/
      DATA TITLE /'X', 'Y', 'REAL', 'IMAG'/
      DATA UNITS /'Degrees', 'Degrees', 'Jy', 'Jy'/
C-----------------------------------------------------------------------
C                                       Get info from IMAGE
      CALL OGET (IMAGE, 'FILE_NAME.NAME', TYPE, DIM, DUMMY, TNAME, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (IMAGE, 'FILE_NAME.CLASS', TYPE, DIM, DUMMY, TCLASS,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (IMAGE, 'FILE_NAME.DISK', TYPE, DIM, TDISK, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (IMAGE, 'FILE_NAME.IMSEQ', TYPE, DIM, TSEQ, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Create OBJECT
      CALL TABCRE (NAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy information to object
C                                       Naming info
      DIM(1) = 12
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (NAME, 'NAME', OOACAR, DIM, DUMMY, TNAME, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 6
      CALL OPUT (NAME, 'CLASS', OOACAR, DIM, DUMMY, TCLASS, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 2
      CALL OPUT (NAME, 'TBLTYPE', OOACAR, DIM, DUMMY, 'CX', IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 1
      CALL OPUT (NAME, 'IMSEQ', OOAINT, DIM, TSEQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (NAME, 'DISK', OOAINT, DIM, TDISK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (NAME, 'VER', OOAINT, DIM, VER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Structural information
      NCOL = MAXCXC
      CALL OPUT (NAME, 'NCOL', OOAINT, DIM, NCOL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 56
      CALL OPUT (NAME, 'LABEL', OOACAR, DIM, DUMMY, TTITLE, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 24
      DIM(2) = NCOL
      CALL OPUT (NAME, 'COLABEL', OOACAR, DIM, DUMMY, CTITLE, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 8
      CALL OPUT (NAME, 'COLUNIT', OOACAR, DIM, DUMMY, CUNITS, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = NCOL
      DIM(2) = 1
      CALL OPUT (NAME, 'COLTYPE', OOAINT, DIM, COLTYP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (NAME, 'COLDIM', OOAINT, DIM, COLDIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE CXTPUT (NAME, ROW, X, Y, RE, IM, IERR)
C-----------------------------------------------------------------------
C   Write row to CX (Complex CLEAN component) table object.
C   Inputs:
C      NAME    C*?  CX table object name.
C      ROW     I    Row number
C      X       R    X pixel coordinate
C      Y       R    Y pixel coordinate
C      RE      R    Real part of component.
C      IM      R    Imaginary part of component.
C   Output:
C      IERR    I    Return error code, 0=>OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   ROW, IERR
      REAL      X, Y, RE, IM
C
      INTEGER   DIM(7)
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
C                                       Write
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL TABDPT (NAME, ROW, 1, OOARE, DIM, X, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABDPT (NAME, ROW, 2, OOARE, DIM, Y, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABDPT (NAME, ROW, 3, OOARE, DIM, RE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABDPT (NAME, ROW, 4, OOARE, DIM, IM, CDUMMY, IERR)
C
 999  RETURN
      END
      SUBROUTINE CXTGET (NAME, ROW, X, Y, RE, IM, IERR)
C-----------------------------------------------------------------------
C   Write row to CX (Complex CLEAN component) table object.
C   Inputs:
C      NAME    C*?  CX table object name.
C      ROW     I    Row number
C   Output:
C      X       R    X pixel coordinate
C      Y       R    Y pixel coordinate
C      RE      R    Real part of component.
C      IM      R    Imaginary part of component.
C      IERR    I    Return error code, 0=>OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   ROW, IERR
      REAL      X, Y, RE, IM
C
      INTEGER   DIM(7), TYPE
      CHARACTER CDUMMY*1
C-----------------------------------------------------------------------
C                                       Read
      CALL TABDGT (NAME, ROW, 1, TYPE, DIM, X, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABDGT (NAME, ROW, 2, TYPE, DIM, Y, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABDGT (NAME, ROW, 3, TYPE, DIM, RE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABDGT (NAME, ROW, 4, TYPE, DIM, IM, CDUMMY, IERR)
C
 999  RETURN
      END
