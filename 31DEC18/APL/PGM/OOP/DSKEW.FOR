LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER   NPARMS
      PARAMETER (NPARMS=12)
C
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
      INCLUDE 'INCS:PAOOF.INC'
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK',
     *   'BLC', 'TRC', 'IMSIZE', 'REWEIGHT'/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT,
     *   OOACAR, OOACAR, OOAINT, OOAINT,
     *   OOAINT, OOAINT, OOAINT, OOARE/
      DATA AVDIM /12,1, 6,1, 1,1, 1,1,
     *   12,1, 6,1, 1,1, 1,1,
     *   7,1, 7,1, 2,1, 2,1/
LOCAL END
      PROGRAM DSKEW
C-----------------------------------------------------------------------
C! Geometric interpolation to correct for skew
C# Task MAP-UTIL OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 2013
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
      DOUBLE PRECISION CD(2,2)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'DSKEW '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL DSKEIN (PRGM, IN, OUT, HWIDTH, NAXIS, CD, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Interpolate
      CALL IMGSKF (IN, HWIDTH, 1, NAXIS, CD, OUT, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL BEMCOP (IN, OUT, IRET)
      IRET = 0
C                                       History
      CALL DSKEHI (IN, OUT)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE DSKEIN (PRGN, IN, OUT, HWIDTH, NAXIS, CD, IRET)
C-----------------------------------------------------------------------
C   DSKEIN gets input parameters for DSKEW and creates the output.
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
      DOUBLE PRECISION CD(2,2)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NKEY1, NKEY2
C                                       NKEY1=no. adverbs for inname
      PARAMETER (NKEY1=6)
C                                       NKEY2 = no. adverb for in2name
      PARAMETER (NKEY2=4)
      INTEGER   DIM(7), TYPE, DISK, SEQ, BLC(7), TRC(7), IMSIZE(2),
     *   IROUND, DUMMY, IDISK, ODISK, ICNO, OCNO, NAXIS1(2), INDSK,
     *   INCNO, I
      REAL      CRPIX(7), REWT(2), CROTA(7), CDELT(7), X, CDELTI(7)
      DOUBLE PRECISION OBSRA, OBSDEC, CRVAL(7)
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, NAME*12, CLASS*6,
     *   INAME*12, CNAME*8, KEYW*8, CDUMMY*1, CTYPE(7)*8
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
      CALL FNDSKC (IN, INDSK, INCNO, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OCLOSE (IN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Control parameters
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
C                                       Input subimage dimension
      CALL ARRWIN (IN, BLC, TRC, NAXIS, IRET)
      IF (IRET.NE.0) GO TO 999
      NAXIS1(1) = NAXIS(1)
      NAXIS1(2) = NAXIS(2)
      IF (IMSIZE(1).LE.0) IMSIZE(1) = NAXIS(1)
      IF (IMSIZE(2).LE.0) IMSIZE(2) = NAXIS(2)
      NAXIS(1) = IMSIZE(1)
      NAXIS(2) = IMSIZE(2)
      DIM(1) = 7
      CALL OPUT (OUT, 'ARRAY.ARRAY_DESC.NAXIS', OOAINT, DIM, NAXIS,
     *   CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Reference pixel
      CALL OGET (IN, 'IMAGE_DESC.CRPIX', TYPE, DIM, CRPIX, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Reference value
      CALL OGET (IN, 'IMAGE_DESC.CRVAL', TYPE, DIM, CRVAL, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Rotation
      CALL OGET (IN, 'IMAGE_DESC.CROTA', TYPE, DIM, CROTA, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Increment
      CALL OGET (IN, 'IMAGE_DESC.CDELT', TYPE, DIM, CDELT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL RCOPY (7, CDELT, CDELTI)
C                                       Type
      CALL OGET (IN, 'IMAGE_DESC.CTYPE', TYPE, DIM, DUMMY, CTYPE, IRET)
      IF (IRET.NE.0) GO TO 999
      IF ((CDELT(1).EQ.0.0) .OR. (CDELT(2).EQ.0.0)) THEN
         IRET = 10
         MSGTXT = 'AXIS INCREMENTS ARE ZERO, QUITTING'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Correct other axes for
C                                       subimaging.
      CRPIX(3) = CRPIX(3) - BLC(3) + 1.0
      CRPIX(4) = CRPIX(4) - BLC(4) + 1.0
      CRPIX(5) = CRPIX(5) - BLC(5) + 1.0
      CRPIX(6) = CRPIX(6) - BLC(6) + 1.0
      CRPIX(7) = CRPIX(7) - BLC(7) + 1.0
C                                       do axis 1/2 need this ???
      CRPIX(1) = CRPIX(1) - BLC(1) + 1.0
      CRPIX(2) = CRPIX(2) - BLC(2) + 1.0
C                                       shift by increase in size
      IF (IMSIZE(1).GT.NAXIS1(1)) CRPIX(1) = CRPIX(1) + (NAXIS(1)
     *   - NAXIS1(1)) / 2
      IF (IMSIZE(2).GT.NAXIS1(2)) CRPIX(2) = CRPIX(2) + (NAXIS(2)
     *   - NAXIS1(2)) / 2
C                                       equalize close axis increments
      X = ABS (CDELT(1)/CDELT(2))
      IF ((X.LE.1.1) .AND. (X.GT.0.9)) THEN
         X = SQRT (ABS(CDELT(1) * CDELT(2)))
         CDELT(1) = SIGN (1.0, CDELT(1)) * X
         CDELT(2) = SIGN (1.0, CDELT(2)) * X
         END IF
C                                       No rotation in output if small
      I = 2
      IF (CROTA(1).NE.0.0) I = 1
      IF ((ABS(CROTA(I)).LE.10.) .OR. (ABS(CROTA(I)-360.).LE.10.0))
     *   CROTA(I) = 0.0
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
C                                       get CD
      CALL HIGET (INDSK, INCNO, CDELTI, CD, IRET)
C
 999  RETURN
      END
      SUBROUTINE HIGET (DISK, CNO, CDELT, CD, IRET)
C-----------------------------------------------------------------------
C   HIGET tries to get the CD matrix from the history file
C   Inputs:
C      DISK    I        Disk number
C      CNO     I        Catalog number
C      CDELT   R(2)     Axis increments
C   Output
C      CD      D(2,2)   CD matrix
C      IRET    I        Error code
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, IRET
      REAL      CDELT(*)
      DOUBLE PRECISION CD(2,2)
C
      INTEGER   IHLUN, NREC, IHPTR, HIBUFF(256), ICUR, I1, I2, J1, J2,
     *   JTRIM, PCCNT, CDCNT
      CHARACTER LINE*72
      DOUBLE PRECISION XX, PC(2,2)
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      CALL DFILL (4, 0.0D0, CD)
      CALL DFILL (4, 0.0D0, PC)
      PCCNT = 0
      CDCNT = 0
C                                       open history file
      IHLUN = 27
C                                       Open history file.
      CALL HIINIT (3)
      CALL HIOPEN (IHLUN, DISK, CNO, HIBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RETURNED FROM HIOPEN'
         GO TO 990
         END IF
      CALL HILOCT ('SRCH', IHLUN, IHPTR, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Determine no. of hist. recs.
      NREC = HITAB(IHPTR+2)
      ICUR = 1
 10   IF (ICUR.LE.NREC) THEN
         CALL HIREAD (IHLUN, ICUR, LINE, HIBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RETURNED FROM HIREAD'
            GO TO 990
            END IF
C                                       desired CD card?
         IF ((LINE(:2).EQ.'CD') .AND. (LINE(4:4).EQ.'_')) THEN
            READ (LINE,1010) I1, I2
            IF ((I1.GE.1) .AND. (I2.LE.2) .AND.
     *         (I1.LE.2) .AND. (I2.GE.1)) THEN
               CDCNT = CDCNT + 1
               IF (CD(I1,I2).NE.0.0D0) THEN
                  WRITE (MSGTXT,1015) 'CD', I1, I2
                  CALL MSGWRT (6)
                  CDCNT = CDCNT - 1
                  END IF
               J1 = INDEX (LINE, '=')
               IF (J1.GT.5) THEN
                  J2 = INDEX (LINE, '/') - 1
                  IF (J2.LE.0) J2 = JTRIM (LINE)
                  J1 = J1 + 1
                  CALL GETNUM (LINE, J2, J1, XX)
                  IF (XX.NE.DBLANK) CD(I1,I2) = XX
                  END IF
               END IF
            END IF
C                                       desired CD card?
         IF ((LINE(:2).EQ.'PC') .AND. (LINE(4:4).EQ.'_')) THEN
            READ (LINE,1010) I1, I2
            IF ((I1.GE.1) .AND. (I2.LE.2) .AND.
     *         (I1.LE.2) .AND. (I2.GE.1)) THEN
               PCCNT = PCCNT + 1
               IF (CD(I1,I2).NE.0.0D0) THEN
                  WRITE (MSGTXT,1015) 'PC', I1, I2
                  CALL MSGWRT (6)
                  PCCNT = PCCNT - 1
                  END IF
               J1 = INDEX (LINE, '=')
               IF (J1.GT.5) THEN
                  J2 = INDEX (LINE, '/') - 1
                  IF (J2.LE.0) J2 = JTRIM (LINE)
                  J1 = J1 + 1
                  CALL GETNUM (LINE, J2, J1, XX)
                  IF (XX.NE.DBLANK) CD(I1,I2) = XX
                  END IF
               END IF
            END IF
         GO TO 10
         END IF
C                                       Close history file.
 100  CALL HICLOS (IHLUN, .FALSE., HIBUFF, I1)
C                                       change PC to CD
      IF (PCCNT.GT.0) THEN
         IF (CDCNT.GT.0) THEN
            MSGTXT = 'HIGET: PC AND CD CARDS FOUND, PC IGNORED'
            CALL MSGWRT (7)
         ELSE
            MSGTXT = 'HIGET: PC cards translated into CD cards'
            CALL MSGWRT (2)
            DO 120 I1 = 1,2
               DO 110 I2 = 1,2
                  CD(I1,I2) = PC(I1,I2) * CDELT(I1)
 110              CONTINUE
 120           CONTINUE
            END IF
         END IF
      DO 140 I1 = 1,2
         DO 130 I2 = 1,2
            WRITE (MSGTXT,1120) I1, I2, CD(I1,I2)
            CALL MSGWRT (3)
 130        CONTINUE
 140     CONTINUE
      XX = CD(1,1) * CD(2,2) - CD(1,2) * CD(2,1)
      IF (XX.EQ.0.0D0) THEN
         MSGTXT = 'DETERMINANT IS ZERO - QUITTING'
         IRET = 10
         END IF
C
 990  IF (IRET.GT.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('HIGET: ERROR',I4,' ON ',A)
 1010 FORMAT (2X,I1,1X,I1)
 1015 FORMAT ('HIGET WARNING: MORE THAN ONE VALUE FOR ',A2,I1,'_',I1)
 1120 FORMAT ('HIGET read CD',I1,'_',I1,' = ',1PE14.7)
      END
      SUBROUTINE DSKEHI (IN, OUT)
C-----------------------------------------------------------------------
C   Routine to write history file to output.
C   Inputs:
C      IN      C*?  Input object
C      OUT     C*?  Output object
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
C
      INTEGER   NADV
      PARAMETER (NADV=8)
      CHARACTER LIST(NADV)*8
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'BLC', 'TRC', 'IMSIZE', 'REWEIGHT'/
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
