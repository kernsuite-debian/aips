LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER NPARMS
      PARAMETER (NPARMS=24)
      INTEGER AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
      INCLUDE 'INCS:PAOOF.INC'
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'NFIELD',
     *   'NMAPS', 'OUTNAME', 'OUTCLASS', 'OUTSEQ', 'OUTDISK', 'IMSIZE',
     *   'COORDINA', 'COOTYPE', 'COOINC', 'COOREF', 'ROTATE',
     *   'REWEIGHT', 'DOWEIGHT', 'EDGSKP', 'OPTYPE', 'APARM', 'PBPARM',
     *   'NOISE', 'BADDISK'/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOAINT,
     *   OOAINT, OOACAR, OOACAR, OOAINT, OOAINT, OOAINT,
     *   OOARE, OOACAR, OOARE, OOARE, OOARE,
     *   OOARE, OOARE, OOAINT, OOACAR, OOARE, OOARE,
     *   OOARE, OOAINT/
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 1,1,
     *   1,1, 12,1, 6,1, 1,1, 1,1, 2,1,
     *   6,1, 4,1, 2,1, 2,1, 1,1,
     *   2,1, 1,1, 1,1, 4,1, 10,1, 7,1,
     *   64,1, 10,1/
LOCAL END
      PROGRAM FLATN
C-----------------------------------------------------------------------
C! Geometric interpolation of multiple facets/pointings to one image
C# Task IMAGING OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1997-2000, 2002-2003, 2006-2007, 2009-2013
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
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NMMAX
      PARAMETER (NMMAX = MAXFLD * MAXFLD / 16)
      CHARACTER PRGM*6, IN(NMMAX)*32, OUT*32, SCF(2)*32, ARRAY*8
      INTEGER   NIMAGE, IRET, BUFF1(256), HWIDTH, I
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'FLATN '/
C-----------------------------------------------------------------------
C                                       Startup
      I = NMMAX
      CALL FLTNIN (PRGM, I, IN, NIMAGE, OUT, SCF, HWIDTH, ARRAY, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Interpolate
      CALL FLATEN (ARRAY, IN, NIMAGE, HWIDTH, SCF, OUT, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL BEMCOP (IN, OUT, IRET)
      IRET = 0
C                                       History
      CALL FLTNHI (IN, OUT)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE FLTNIN (PRGN, NMMAX, IN, NIMAGE, OUT, SCF, HWIDTH,
     *   ARRAY, IRET)
C-----------------------------------------------------------------------
C   FLTNIN gets input parameters for FLATN and creates the output.
C   Inputs:
C      PRGN     C*6      Program name
C      NMMAX    I        Max dimension of IN
C   Output:
C      IN       C(*)*?   Input object
C      NIMAGE   I        Number of images in IN
C      OUT      C*?      Output object
C      SCF      C(2)*?   Scratch image objects
C      HWIDTH   I        Interpolation kernel half width.
C      ARRAY    C*8      Array name
C      IRET     I        Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   NMMAX, NIMAGE, HWIDTH, IRET
      CHARACTER PRGN*6, IN(*)*(*), OUT*(*), SCF(2)*(*), ARRAY*8
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NKEY1
C                                       NKEY1=no. adverbs for inname
      PARAMETER (NKEY1=4)
      INTEGER   DIM(7), TYPE, DISK, SEQ, BLC(7), TRC(7), IMSIZE(2),
     *   NAXIS(7), NAXIS2(7), IROUND, DUMMY, IDISK, ODISK, ICNO, OCNO,
     *   INAXIS(7), BLC2(7), TRC2(7), ROTAX, I, NFIELD, LFIELD, IDUMMY,
     *   NITOT, NITER, MSGSAV, EDGSKP, NDIM, J0, NMAPS, IMSEQ0, IMSEQ,
     *   LMAP, LLM, NNOISE, IPROJ
      REAL      CRPIX(7), CRPIX2(7), REWT(2), CDELT(7), CDELT2(7),
     *   CROTA(7), CROTA2(7), COORDS(6), XSHIFT, YSHIFT, APARM(10),
     *   C123(4), RTEMP, PA, ZA, NOISES(65), PBPARM(7), COOINC(2),
     *   COOREF(2), ROTATE, ACTN
      DOUBLE PRECISION OBSRA, OBSDEC, CRVAL(7), CRVAL2(7), OUTRA, OUTDEC
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, NAME*12, CLASS*6,
     *   INAME*12, CDUMMY*1, CFIELD*11, CNAME*8, KEYW*8, CRTYPE(7)*8,
     *   TEST*32, COOTYP*4, PROJ*4, LPROJ(11)*4, ICLASS*6, OPTYPE*4,
     *   CUNITS*8, NOTTAB*2
      LOGICAL   ISNEG, DO3D, OLDNAM, DOPBEM
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs for IN
C                    1          2        3           4
      DATA INK1 /'INNAME', 'INDISK', 'DOWEIGHT', 'PBPARM'/
C                                       Rename
C                    1       2        3           4
      DATA OUTK1 /'NAME', 'DISK', 'DOWEIGHT', 'PBPARM'/
      DATA LPROJ /'-TAN','-SIN', '-ARC', 'NCP', '-STG', '-AIT',
     *   '-GLS', '-MER','-CAR','-MOL','-PAR'/
C-----------------------------------------------------------------------
      NIMAGE = 0
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
C                                       Declare 'ACTNOISE' a header
C                                       keyword for the image class.
      CNAME = 'IMAGE'
      KEYW = 'ACTNOISE'
      CALL OBVHKW (CNAME, KEYW, OOARE, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       other inputs
      CALL OGET ('Input', 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'NFIELD', TYPE, DIM, NFIELD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      NFIELD = MAX (1, MIN (MAXFLD, NFIELD))
      CALL OGET ('Input', 'PBPARM', TYPE, DIM, PBPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'NMAPS', TYPE, DIM, NMAPS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      NMAPS = MAX (1, NMAPS)
      DOPBEM = NMAPS.GT.1
      IF ((PBPARM(1).GT.0.0) .AND. (PBPARM(2).GT.0.0) .AND.
     *   (PBPARM(3).EQ.0.0) .AND. (PBPARM(4).EQ.0.0) .AND.
     *   (PBPARM(5).EQ.0.0) .AND. (PBPARM(6).EQ.0.0) .AND.
     *   (PBPARM(7).EQ.0.0)) DOPBEM = .FALSE.
      CALL OGET ('Input', 'OPTYPE', TYPE, DIM, DUMMY, OPTYPE, IRET)
      IF (IRET.NE.0) GO TO 999
      IF ((OPTYPE.NE.'NOIS') .AND. (OPTYPE.NE.'WEIG')) OPTYPE = ' '
      IF (.NOT.DOPBEM) OPTYPE = ' '
      CALL OGET ('Input', 'INCLASS', TYPE, DIM, DUMMY, CLASS, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'EDGSKP', TYPE, DIM, EDGSKP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (EDGSKP.LT.0) EDGSKP = -EDGSKP - 1
      EDGSKP = MIN (1024, EDGSKP)
      CALL OGET ('Input', 'NOISE', TYPE, DIM, NOISES, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'INSEQ', TYPE, DIM, IMSEQ0, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IMSEQ0 = MAX (1, IMSEQ0)
C                                       Image coordinates
      CALL OGET ('Input', 'COOTYPE', TYPE, DIM, DUMMY, COOTYP, IRET)
      IF (IRET.NE.0) GO TO 999
      PROJ = ' '
      IPROJ = 0
      DO 15 I = 1,11
         IF (COOTYP.EQ.LPROJ(I)) THEN
            PROJ = LPROJ(I)
            IPROJ = I
            END IF
 15      CONTINUE
      CALL OGET ('Input', 'ROTATE', TYPE, DIM, ROTATE, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'COOINC', TYPE, DIM, COOINC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'COOREF', TYPE, DIM, COOREF, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'COORDINA', TYPE, DIM, COORDS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      ISNEG = (COORDS(1).LT.0.0) .OR. (COORDS(2).LT.0.0) .OR.
     *   (COORDS(3).LT.0.0)
      OUTRA = ABS (COORDS(1)) + ABS (COORDS(2))/60.0D0 + ABS
     *   (COORDS(3))/3600.0D0
      IF (ISNEG) OUTRA = -OUTRA
      OUTRA = OUTRA * 15.0D0
      ISNEG = (COORDS(4).LT.0.0) .OR. (COORDS(5).LT.0.0) .OR.
     *   (COORDS(6).LT.0.0)
      OUTDEC = ABS(COORDS(4)) + ABS(COORDS(5))/60.0D0 +
     *   ABS(COORDS(6))/3600.0D0
      IF (ISNEG) OUTDEC = -OUTDEC
      IF ((IPROJ.GT.5) .AND. (OUTDEC.NE.0.0D0)) THEN
         MSGTXT = 'You have requested an oblique coordinate system'
         CALL MSGWRT (6)
         MSGTXT = 'I hope that is what you wanted!'
         CALL MSGWRT (6)
         END IF
      CALL OGET ('Input', 'APARM', TYPE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Interpolation kernel half width.
      CALL OGET ('Input', 'REWEIGHT', TYPE, DIM, REWT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      HWIDTH = IROUND (REWT(1))
      HWIDTH = MIN (4, MAX (1, HWIDTH))
      REWT(1) = HWIDTH
      IF ((REWT(2).LE.0.0) .OR. (REWT(2).GE.1.0)) REWT(2) = 0.3334
      CALL OPUT ('Input', 'REWEIGHT', TYPE, DIM, REWT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create IN
      NITOT = 0
      DO3D = APARM(1).GT.0.0
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
      IF (CLASS.EQ.' ') CLASS = 'ICL001'
      OLDNAM = (CLASS(4:4).LT.'0') .OR. (CLASS(4:4).GT.'9') .OR.
     *   (CLASS(5:5).LT.'0') .OR. (CLASS(5:5).GT.'9') .OR.
     *   (CLASS(6:6).LT.'0') .OR. (CLASS(6:6).GT.'9')
      J0 = 1
      IF (.NOT.OLDNAM) READ (CLASS(4:6),1001) J0
      NOISES(65) = 0.0
      NNOISE = 0
      DO 30 LMAP = 1,NMAPS
         IMSEQ = IMSEQ0 - 1 + LMAP
         ICLASS = CLASS
         LLM = MIN (LMAP, 65)
         ACTN = 0.0
         IF (LMAP.EQ.65) THEN
            NNOISE = MAX (1, NNOISE)
            NOISES(65) = NOISES(65) / NNOISE
         ELSE IF (LMAP.LT.65) THEN
            ACTN = NOISES(LMAP)
            END IF
         DO 20 LFIELD = 1,NFIELD
            WRITE (CFIELD,1000) LFIELD - 1 + J0, IMSEQ
            TEST = 'Input image object ' // CFIELD
            CALL CREATE (TEST, 'IMAGE', IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
            CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, TEST, IRET)
            IF (IRET.NE.0) GO TO 999
            DIM(1) = 6
            DIM(2) = 1
            IF (OLDNAM) THEN
               CALL ZEHEX (LFIELD-1, 2, CFIELD(:2))
               IF (LFIELD.GT.1) ICLASS(5:6) = CFIELD(:2)
            ELSE IF (LFIELD.LT.1000) THEN
               ICLASS(4:6) = CFIELD(2:4)
            ELSE
               ICLASS(3:6) = CFIELD(1:4)
               END IF
            CALL OPUT (TEST, 'CLASS', OOACAR, DIM, IDUMMY, ICLASS, IRET)
            IF (IRET.NE.0) GO TO 999
            DIM(1) = 1
            CALL OPUT (TEST, 'IMSEQ', OOAINT, DIM, IMSEQ, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Open and close to check
            MSGSAV = MSGSUP
            MSGSUP = 32000
            CALL OOPEN (TEST, 'READ', IRET)
            MSGSUP = MSGSAV
C                                       skip missing ones
            IF (IRET.NE.0) THEN
               IRET = 0
C                                       got it - continue
            ELSE
               IF (NIMAGE.EQ.NMMAX) THEN
                  MSGTXT = 'TOO MANY FIELDS*POINTINGS !!!!'
                  CALL MSGWRT (8)
                  IRET = 8
                  GO TO 999
                  END IF
               NIMAGE = NIMAGE + 1
               IN(NIMAGE) = TEST
C                                       get noise
               IF (ACTN.LE.0.0) THEN
                  MSGSUP = 32000
                  CALL IMGET (TEST, 'ACTNOISE', TYPE, DIM, ACTN, CDUMMY,
     *               IRET)
                  MSGSUP = MSGSAV
                  IF ((IRET.NE.0) .OR. (ACTN.LE.0.0)) THEN
                     ACTN = 1.0
                     IF (LMAP.GE.65) ACTN = NOISES(65)
                     IRET = 0
                     END IF
                  IF ((LMAP.LT.65) .AND. (LFIELD.NE.1)) THEN
                     NOISES(65) = NOISES(65) + ACTN
                     NNOISE = NNOISE + 1
                     END IF
                  IF (LMAP.LT.65) NOISES(LMAP) = ACTN
                  END IF
               IF ((LMAP.LT.65) .AND. (LFIELD.EQ.1)) THEN
                  NOISES(65) = NOISES(65) + ACTN
                  NNOISE = NNOISE + 1
                  END IF
C                                       other input parms -> class
               DIM(1) = 4
               DIM(2) = 1
               CALL OPUT (TEST, 'OPTYPE', OOACAR, DIM, DUMMY, OPTYPE,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
               DIM(1) = 1
               DIM(2) = 1
               CALL OPUT (TEST, 'DOPBEAM', OOALOG, DIM, DOPBEM, CDUMMY,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
               CALL OPUT (TEST, 'NOISE', OOARE, DIM, ACTN, CDUMMY, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       sum clean iterations
               CALL BEMGET (TEST, 'NITER', TYPE, DIM, NITER, CDUMMY,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
               NITOT = NITOT + MAX (0, NITER)
C                                       Input subimage dimension
               CALL ARRWIN (TEST, BLC, TRC, NAXIS, IRET)
               IF (IRET.NE.0) GO TO 999
               BLC(1) = 1 + EDGSKP
               BLC(2) = 1 + EDGSKP
               TRC(1) = TRC(1) - EDGSKP
               TRC(2) = TRC(2) - EDGSKP
               DIM(1) = 7
               DIM(2) = 1
               CALL OPUT (TEST, 'BLC', OOAINT, DIM, BLC, CDUMMY, IRET)
               IF (IRET.NE.0) GO TO 999
               CALL OPUT (TEST, 'TRC', OOAINT, DIM, TRC, CDUMMY, IRET)
               IF (IRET.NE.0) GO TO 999
               CALL OCLOSE (TEST, IRET)
               IF (IRET.NE.0) GO TO 999
               DIM(1) = 1
               DIM(2) = 1
               CALL OPUT (TEST, 'RELIABLE', OOARE, DIM, REWT(2), CDUMMY,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
C                                       3-D corrections?
               IF (DO3D) THEN
                  MSGTXT = '*** WARNING: DOING 3-D CORRECTIONS. ***'
                  CALL MSGWRT (7)
                  MSGTXT = '*** I HOPE YOU KNOW WHAT YOU ARE DOING ***'
                  CALL MSGWRT (7)
                  DIM(1) = 1
                  DIM(2) = 1
                  CALL OPUT (TEST, 'DO3DCOR', OOALOG, DIM, DO3D, CDUMMY,
     *               IRET)
                  IF (IRET.NE.0) GO TO 999
                  MSGSAV = MSGSUP
C                                       Parallactic angle
                  MSGSUP = 32000
                  CALL OGET (TEST, 'PARANGLE', TYPE, DIM, PA, CDUMMY,
     *               IRET)
                  MSGSUP = MSGSAV
C                                       Use input
                  IF (IRET.NE.0) CALL OPUT (TEST, 'PARANGLE', OOARE,
     *               DIM, APARM(2), CDUMMY, IRET)
                  IF (IRET.NE.0) GO TO 999
C                                       Zenith angle
                  MSGSUP = 32000
                  CALL OGET (TEST, 'ZENANGLE', TYPE, DIM, ZA, CDUMMY,
     *               IRET)
                  MSGSUP = MSGSAV
C                                       Use input
                  IF (IRET.EQ.1) CALL OPUT (TEST, 'ZENANGLE', OOARE,
     *               DIM, APARM(3), CDUMMY, IRET)
                  IF (IRET.NE.0) GO TO 999
                  END IF
C                                       Radial scaling
               DIM(1) = 1
               DIM(2) = 1
               CALL OPUT (TEST, 'PBFWHM', OOARE, DIM, APARM(4), CDUMMY,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
               DIM(1) = 1
               RTEMP = APARM(5) * APARM(5)
               CALL OPUT (TEST, 'FBWSQ', OOARE, DIM, RTEMP, CDUMMY,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
               DIM(1) = 4
               CALL OPUT (TEST, 'C123', OOARE, DIM, C123, CDUMMY, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
 20         CONTINUE
 30      CONTINUE
C                                       Save APARM, NOISE for history
      DIM(1) = 10
      CALL OPUT ('Input', 'APARM', OOARE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 64
      CALL OPUT ('Input', 'NOISE', OOARE, DIM, NOISES, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Control parameters
C                                       Clone output from IN2
      OUT = 'Output interpolated image'
      CALL CREATE (OUT, 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy array descriptors
      CALL ARDCOP (IN, OUT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Set names
      CALL ARDGET (IN(1), 'NDIM', TYPE, DIM, NDIM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (IN(1), 'NAME', TYPE, DIM, DUMMY, INAME, IRET)
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
      CALL ARRWIN (IN(1), BLC, TRC, NAXIS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Check coordinate type
      CALL OBHGET (IN(1), CATBLK, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL H2CHR (8, 1, CATH(KHTEL), ARRAY)
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
      DIM(1) = 7
      CALL OPUT (OUT, 'ARRAY.ARRAY_DESC.NAXIS', OOAINT, DIM, NAXIS2,
     *   CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IMSIZE(1) = NAXIS2(1)
      IMSIZE(2) = NAXIS2(2)
C                                       Reference pixel, increment
      CALL OGET (IN(1), 'ARRAY.ARRAY_DESC.NAXIS', TYPE, DIM, INAXIS,
     *   CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (IN(1), 'IMAGE_DESC.CRPIX', TYPE, DIM, CRPIX, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (IN(1), 'IMAGE_DESC.CRVAL', TYPE, DIM, CRVAL, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (IN(1), 'IMAGE_DESC.CDELT', TYPE, DIM, CDELT, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (IN(1), 'IMAGE_DESC.CROTA', TYPE, DIM, CROTA, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (IN(1), 'IMAGE_DESC.CTYPE', TYPE, DIM, DUMMY, CRTYPE,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Correct for subimaging.
      CRPIX2(1) = NAXIS2(1) / 2
      CRPIX2(2) = NAXIS2(2) / 2 + 1
      CRPIX2(3) = CRPIX(3) - BLC(3) + 1.0
      CRPIX2(4) = CRPIX(4) - BLC(4) + 1.0
      CRPIX2(5) = CRPIX(5) - BLC(5) + 1.0
      CRPIX2(6) = CRPIX(6) - BLC(6) + 1.0
      CRPIX2(7) = CRPIX(7) - BLC(7) + 1.0
      CALL RCOPY (7, CDELT, CDELT2)
      CALL RCOPY (7, CROTA, CROTA2)
      CALL DPCOPY (7, CRVAL, CRVAL2)
      DO 35 I = NDIM+1,KICTPN
         CRPIX2(I) = 0.0
         CDELT2(I) = 0.0
         CRVAL2(I) = 0.0D0
         CROTA2(I) = 0.0
         CRTYPE(I) = ' '
 35      CONTINUE
C                                       Force full instantiation
      CALL OOPEN (OUT, 'WRIT', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy image descriptors
C                                       includes obsra, obsdec, beam
      CALL IMDCOP (IN(1), OUT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       do not copy CC tables
      DIM(1) = 2
      DIM(2) = 1
      NOTTAB = 'CC'
      CALL OPUT (IN(1), 'DROPTABS', OOACAR, DIM, DUMMY, NOTTAB, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       set total niter
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (OUT, 'BEAM.NITER', OOAINT, DIM, NITOT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get Observing position
      CALL PSNGET (IN(1), 'OBSRA', TYPE, DIM, OBSRA, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL PSNGET (IN(1), 'OBSDEC', TYPE, DIM, OBSDEC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF ((OUTRA.EQ.0.0D0) .AND. (OUTDEC.EQ.0.0D0)) THEN
         OUTRA = OBSRA
         OUTDEC = OBSDEC
         END IF
      IF ((OUTRA.NE.0.0) .OR. (OUTDEC.NE.0.0)) THEN
         CRVAL2(1) = OUTRA
         CRVAL2(2) = OUTDEC
      ELSE IF (IPROJ.GT.5) THEN
         CRVAL2(2) = 0.0D0
         END IF
      IF (PROJ.NE.' ') THEN
         CRTYPE(1)(5:) = PROJ
         CRTYPE(2)(5:) = PROJ
         END IF
      IF ((COOREF(1).NE.0.0) .OR. (COOREF(2).NE.0.0)) THEN
         CRPIX2(1) = COOREF(1)
         CRPIX2(2) = COOREF(2)
         END IF
      IF ((COOINC(1).NE.0.0) .AND. (COOINC(2).NE.0.0)) THEN
         CDELT2(1) = COOINC(1) / 3600.0
         CDELT2(2) = COOINC(2) / 3600.0
         END IF
      DO 40 I = 1,2
         IF (CRTYPE(I)(:4).EQ.'DEC-') CROTA2(I) = ROTATE
         IF (CRTYPE(I)(2:4).EQ.'LAT') CROTA2(I) = ROTATE
 40      CONTINUE
C                                       zero the shift parameter
      CALL PSNGET (IN(1), 'XSHIFT', TYPE, DIM, XSHIFT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      XSHIFT = 0.0
      CALL PSNPUT (OUT, 'XSHIFT', TYPE, DIM, XSHIFT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL PSNGET (IN(1), 'YSHIFT', TYPE, DIM, YSHIFT, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      YSHIFT = 0.0
      CALL PSNPUT (OUT, 'YSHIFT', TYPE, DIM, YSHIFT, CDUMMY, IRET)
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
      CALL OPUT (OUT, 'IMAGE_DESC.CRVAL', OOADP, DIM, CRVAL2, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      CALL FILL (7, 0, BLC2)
      CALL FILL (7, 0, TRC2)
      CALL OPUT (OUT, 'BLC', OOAINT, DIM, BLC2, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (OUT, 'TRC', OOAINT, DIM, TRC2, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 8
      DIM(2) = 7
      CALL OPUT (OUT, 'IMAGE_DESC.CTYPE', OOACAR, DIM, DUMMY, CRTYPE,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      IF (OPTYPE.EQ.'WEIG') THEN
         CUNITS = '(B/JY)^2'
         DIM(2) = 1
         CALL OPUT (OUT, 'IMAGE_DESC.BUNIT', OOACAR, DIM, DUMMY, CUNITS,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      CALL OCLOSE (OUT, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy catalog header keywords.
      CALL OBDSKC (IN(1), IDISK, ICNO, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OBDSKC (OUT, ODISK, OCNO, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL KEYCOP (IDISK, ICNO, ODISK, OCNO, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Save parms for history
      DIM(1) = 2
      DIM(2) = 1
      CALL OPUT ('Input', 'IMSIZE', OOAINT, DIM, IMSIZE, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       create scratch files
      SCF(1) = 'Scratch image'
      SCF(2) = 'Weight summing image'
      DO 50 I = 1,2
         CALL IMGSCR (SCF(I), NAXIS2, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OOPEN (SCF(I), 'WRIT', IRET)
         IF (IRET.NE.0) GO TO 999
         DIM(1) = 8
         DIM(2) = 7
         CALL OPUT (OUT, 'IMAGE_DESC.CTYPE', OOACAR, DIM, DUMMY, CRTYPE,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         DIM(1) = 7
         DIM(2) = 1
         CALL OPUT (SCF(I), 'CRPIX', OOARE, DIM, CRPIX2, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT (SCF(I), 'CDELT', OOARE, DIM, CDELT2, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT (SCF(I), 'CROTA', OOARE, DIM, CROTA2, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT (SCF(I), 'CRVAL', OOADP, DIM, CRVAL2, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT (SCF(I), 'BLC', OOAINT, DIM, BLC2, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT (SCF(I), 'TRC', OOAINT, DIM, TRC2, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         DIM(1) = 1
         CALL ARDPUT (SCF(I), 'NDIM', OOAINT, DIM, NDIM, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OCLOSE (SCF(I), IRET)
         IF (IRET.NE.0) GO TO 999
 50      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I4.4,1X,I6.5)
 1001 FORMAT (I3)
      END
      SUBROUTINE FLATEN (ARRAY, IN, NIMAGE, HWIDTH, SCF, OUT, IRET)
C-----------------------------------------------------------------------
C   Loops over input fields: re-grids each to SCF(1) then adds into OUT
C   with weight sum in SCF(2).  At end, divides SCF(2) into OUT.
C   Inputs:
C      IN       C(*)*?   Input image(s)
C      NIMAGE   I        Number images in IN
C      HWIDTH   I        Convolution half width
C      SCF      C(2)*?   Scratch files
C      OUT      C*?      Output file
C   Output:
C      IRET     I        Error code: 0 okay
C-----------------------------------------------------------------------
      CHARACTER ARRAY*8, IN(*)*(*), SCF(2)*(*), OUT*(*)
      INTEGER   NIMAGE, HWIDTH, IRET
C
      INTEGER   NAX2(7), I1, I2, I3, I4, I5, I6, I7, LFIELD, NFIELD,
     *   TYPE, DIM(7), IDUMMY, NY, BLC(7), TRC(7), XB, XE, LOCF, DUMMY,
     *   EDGSKP, NAX1(7), INBLC(7), INTRC(7)
      LOGICAL   DOPBEM, DORADI, SWITCH, OUTSID, DOELLI
      CHARACTER CDUMMY*1, WWT*32, WOUT*32, STATUS*4, CTYPE(7)*8,
     *   OPTYPE*4
      DOUBLE PRECISION RAPT, DECPT, RAREF, DECREF, XREF, YREF, ZREF,
     *   CRVAL(7), FREQ, LAMBDA, ANGLEP, ANGLER, DEPS
      INCLUDE 'INCS:PMAD.INC'
      REAL      ROWI(MABFSS), ROWO(MABFSS), ROWW(MABFSS), FMIN, FMAX,
     *   NOISE, WT, RFACT, CRPIX(7), CDELT(7), PBPARM(7), XPIX, YPIX,
     *   BMFACT, R, X, Y, XC, YC
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
C-----------------------------------------------------------------------
C                                       number of fields to do
      CALL OGET ('Input', 'NMAPS', TYPE, DIM, NFIELD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'EDGSKP', TYPE, DIM, EDGSKP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DOELLI = EDGSKP.GE.0
      IF (EDGSKP.LE.0) EDGSKP = 5
      NFIELD = MAX (1, MIN (MAXFLD, NFIELD))
      DEPS = 1.D-4
C                                       size of output
      CALL OGET (OUT, 'ARRAY.ARRAY_DESC.NAXIS', TYPE, DIM, NAX2, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Do first one
      IF (NIMAGE.EQ.1) THEN
         MSGTXT = 'Start field   1'
         CALL MSGWRT (2)
         XB = 1
         XE = NAX2(1)
         CALL IMGHGE (IN(1), HWIDTH, XB, XE, OUT, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Multi-field initialize
      ELSE
         CALL COPY (7, NAX2, TRC)
         CALL FILL (7, 1, BLC)
         CALL OOPEN (OUT, 'WRIT', IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OOPEN (SCF(2), 'WRIT', IRET)
         IF (IRET.NE.0) GO TO 999
         DIM(1) = 8
         DIM(2) = 1
         CALL OPUT (OUT, 'ARRAY.ARRAY_PNT.ACCESS', OOACAR, DIM, IDUMMY,
     *      'ROW', IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT (SCF(2), 'ARRAY.ARRAY_PNT.ACCESS', OOACAR, DIM,
     *      IDUMMY,'ROW', IRET)
         IF (IRET.NE.0) GO TO 999
         CALL RFILL (NAX2(1), FBLANK, ROWI)
         DO 30 I7 = 1,NAX2(7)
         DO 30 I6 = 1,NAX2(6)
         DO 30 I5 = 1,NAX2(5)
         DO 30 I4 = 1,NAX2(4)
         DO 30 I3 = 1,NAX2(3)
            DO 20 I2 = 1,NAX2(2)
               CALL ARRWRI (OUT, DIM, ROWI, IRET)
               IF (IRET.NE.0) GO TO 999
               CALL ARRWRI (SCF(2), DIM, ROWI, IRET)
               IF (IRET.NE.0) GO TO 999
 20            CONTINUE
 30         CONTINUE
         CALL ARRCLO (OUT, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ARRCLO (SCF(2), IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OCLOSE (OUT, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OCLOSE (SCF(2), IRET)
         IF (IRET.NE.0) GO TO 999
         STATUS = ' '
         CALL IMCDES (OUT, STATUS, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       dummy for read/write
         WWT = 'Shallow weight out'
         WOUT = 'Shallow image out'
         CALL OCOPY (OUT, WOUT, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OCOPY (SCF(2), WWT, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       what operation
         CALL OGET (IN(1), 'OPTYPE', TYPE, DIM, DUMMY, OPTYPE, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       loop over fields 1 -> N
         DO 200 LFIELD = 1,NIMAGE
            CALL OGET (IN(LFIELD), 'ARRAY.ARRAY_DESC.NAXIS', TYPE, DIM,
     *         NAX1, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
            XC = NAX1(1) / 2
            YC = NAX1(2) / 2 + 1
            R = (NAX1(1) - 2*EDGSKP) * (NAX1(2) - 2*EDGSKP) / 2.0
            R = R * R
C                                       switch to new file
            CALL OOPEN (IN(LFIELD), 'READ', IRET)
            IF (IRET.NE.0) GO TO 999
            CALL ARSGET (IN(LFIELD), 'DATAMIN', TYPE, DIM, FMIN, CDUMMY,
     *         IRET)
            CALL ARSGET (IN(LFIELD), 'DATAMAX', TYPE, DIM, FMAX, CDUMMY,
     *         IRET)
            CALL OCLOSE (IN(LFIELD), IRET)
            IF (IRET.NE.0) GO TO 999
            IF ((FMIN.GT.FMAX) .OR. ((FMIN.EQ.0.0) .AND. (FMAX.EQ.0.0)))
     *         THEN
               MSGTXT = 'Image all 0 or blank - skipping interpolation'
               CALL MSGWRT (6)
               GO TO 200
               END IF
C                                       get weighting parameters
            CALL OGET (IN(LFIELD), 'DOWEIGHT', TYPE, DIM, RFACT, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            DORADI = RFACT.GT.0.0
            RFACT = RFACT * 3600.0
            CALL OGET (IN(LFIELD), 'DOPBEAM', TYPE, DIM, DOPBEM, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            IF ((DOPBEM) .OR. (DORADI)) THEN
               CALL OGET (IN(LFIELD), 'CRPIX', TYPE, DIM, CRPIX, CDUMMY,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
               CALL PSNVAL (IN(LFIELD), XC, YC, XREF, YREF, ZREF, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       test image coord type
               IF ((AXTYP(LOCNUM).NE.1) .OR. (CORTYP(LOCNUM).LT.1)
     *            .OR. (CORTYP(LOCNUM).GT.2)) THEN
                  MSGTXT = 'PRIMARY BEAM AND DEFOCUSING WORK ONLY ON'
     *               // ' RA-DEC IMAGES'
                  CALL MSGWRT (8)
                  IRET = 8
                  GO TO 999
                  END IF
               SWITCH = CORTYP(LOCNUM).EQ.2
               IF (SWITCH) THEN
                  RAREF = YREF * DG2RAD
                  DECREF = XREF * DG2RAD
               ELSE
                  RAREF = XREF * DG2RAD
                  DECREF = YREF * DG2RAD
                  END IF
               IF (DOPBEM) THEN
                  CALL OGET (IN(LFIELD), 'CRVAL', TYPE, DIM, CRVAL,
     *               CDUMMY, IRET)
                  IF (IRET.NE.0) GO TO 999
                  CALL OGET (IN(LFIELD), 'CDELT', TYPE, DIM, CDELT,
     *               CDUMMY, IRET)
                  IF (IRET.NE.0) GO TO 999
                  CALL OGET (IN(LFIELD), 'CTYPE', TYPE, DIM, IDUMMY,
     *               CTYPE, IRET)
                  IF (IRET.NE.0) GO TO 999
                  CALL OGET (IN(LFIELD), 'NOISE', TYPE, DIM, NOISE,
     *               CDUMMY, IRET)
                  IF (IRET.NE.0) GO TO 999
                  CALL OGET (IN(LFIELD), 'PBPARM', TYPE, DIM, PBPARM,
     *               CDUMMY, IRET)
                  IF (IRET.NE.0) GO TO 999
                  IF (PBPARM(1).LE.0.0) PBPARM(1) = 0.1
                  CALL PSNGET (IN(LFIELD), 'OBSRA', TYPE, DIM, RAPT,
     *               CDUMMY, IRET)
                  IF (IRET.NE.0) GO TO 999
                  CALL PSNGET (IN(LFIELD), 'OBSDEC', TYPE, DIM, DECPT,
     *               CDUMMY, IRET)
                  IF (IRET.NE.0) GO TO 999
                  IF ((RAPT.EQ.0.0D0) .AND. (DECPT.EQ.0.0D0)) THEN
                     RAPT = CRVAL(1)
                     DECPT = CRVAL(2)
                     MSGTXT = 'POINTING POSITION 0 - SET TO' //
     *                  ' REFERENCE POSITION'
                     CALL MSGWRT (7)
                     END IF
                  RAPT = RAPT * DG2RAD
                  DECPT = DECPT * DG2RAD
C                                       find freq axis
                  LOCF = 0
                  DO 50 I1 = 1,7
                     IF (CTYPE(I1)(1:4).EQ.'FREQ') LOCF = I1
 50                  CONTINUE
                  IF (LOCF.LE.0) THEN
                     MSGTXT = 'THERE MUST BE A FREQ AXIS FOR PRIMARY'
     *                  // ' BEAM CORRECTION'
                     CALL MSGWRT (8)
                     IRET = 8
                     GO TO 999
                     END IF
                  END IF
               END IF
C                                       set window into output
            CALL INTWIN (IN(LFIELD), HWIDTH, SCF(1), BLC, TRC, INBLC,
     *         INTRC, IRET)
            IF (IRET.GT.0) GO TO 999
            IF (IRET.LT.0) THEN
               IF (IRET.EQ.-1) THEN
                  WRITE (MSGTXT,1030) LFIELD
               ELSE
                  WRITE (MSGTXT,1031) LFIELD
                  END IF
               CALL MSGWRT (6)
               GO TO 200
            ELSE
               WRITE (MSGTXT,1032) LFIELD, BLC(1), BLC(2), TRC(1),
     *            TRC(2)
               CALL MSGWRT (4)
               END IF
            XB = BLC(1)
            XE = TRC(1)
C                                       shift center match out's IX,IY
            XC = XC + XB - INBLC(1)
            YC = YC + 1 - INBLC(2)
            BLC(1) = 1
            TRC(1) = NAX2(1)
            DIM(1) = 7
            DIM(2) = 1
            CALL OPUT (SCF(1), 'BLC', OOAINT, DIM, BLC, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OPUT (SCF(1), 'TRC', OOAINT, DIM, TRC, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OPUT (SCF(2), 'BLC', OOAINT, DIM, BLC, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OPUT (SCF(2), 'TRC', OOAINT, DIM, TRC, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OPUT (OUT, 'BLC', OOAINT, DIM, BLC, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OPUT (OUT, 'TRC', OOAINT, DIM, TRC, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OPUT (WOUT, 'BLC', OOAINT, DIM, BLC, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OPUT (WOUT, 'TRC', OOAINT, DIM, TRC, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OPUT (WWT, 'BLC', OOAINT, DIM, BLC, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OPUT (WWT, 'TRC', OOAINT, DIM, TRC, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
            NY = TRC(2) - BLC(2) + 1
C                                       interp field LFIELD
            CALL IMGHGE (IN(LFIELD), HWIDTH, XB, XE, SCF(1), IRET)
            IF (IRET.NE.0) GO TO 999
C                                       open for weighting
            CALL OOPEN (OUT, 'READ', IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OOPEN (SCF(1), 'READ', IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OOPEN (SCF(2), 'READ', IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OOPEN (WOUT, 'WRIT', IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OOPEN (WWT, 'WRIT', IRET)
            IF (IRET.NE.0) GO TO 999
            DO 130 I7 = 1,NAX2(7)
            DO 130 I6 = 1,NAX2(6)
            DO 130 I5 = 1,NAX2(5)
            DO 130 I4 = 1,NAX2(4)
            DO 130 I3 = 1,NAX2(3)
               IF (DOPBEM) THEN
                  FREQ = CRVAL(LOCF) + (I3 - CRPIX(LOCF)) * CDELT(LOCF)
                  LAMBDA = VELITE / FREQ
                  END IF
               DO 120 I2 = 1,NY
                  YPIX = I2 + BLC(2) - 1.0
                  CALL ARREAD (OUT, DIM, ROWO, IRET)
                  IF (IRET.NE.0) GO TO 999
                  CALL ARREAD (SCF(1), DIM, ROWI, IRET)
                  IF (IRET.NE.0) GO TO 999
                  CALL ARREAD (SCF(2), DIM, ROWW, IRET)
                  IF (IRET.NE.0) GO TO 999
                  Y = (I2 - YC) * NAX1(1)
                  Y = Y * Y
                  DO 110 I1 = XB,XE
                     IF (ROWI(I1).NE.FBLANK) THEN
C                                       Primary beam weighting
C                                       Defocusing weighting
                        WT = 1.0
                        IF ((DOPBEM) .OR. (DORADI)) THEN
                           XPIX = I1
                           CALL PSNVAL (OUT, XPIX, YPIX, XREF, YREF,
     *                        ZREF, IRET)
                           IF (IRET.NE.0) WT = 0.0
                           IF (SWITCH) THEN
                              ZREF = XREF
                              XREF = YREF
                              YREF = ZREF
                              END IF
                           XREF = XREF * DG2RAD
                           YREF = YREF * DG2RAD
                           END IF
C                                       inscribed ellipse
                        IF (DOELLI) THEN
                           X = (I1 - XC) * NAX1(2)
                           X = X * X
                           IF (X+Y.GT.R) WT = 0.0001
                           END IF
C                                       primary beam
                        IF ((DOPBEM) .AND. (WT.GT.0.0)) THEN
                           ZREF = SIN (YREF) * SIN (DECPT) + COS (YREF)
     *                        * COS (DECPT) * COS (XREF - RAPT)
                           ANGLEP = ACOS (ZREF) * RAD2DG
                           CALL PBCALC (ANGLEP, LAMBDA, ARRAY,
     *                        PBPARM(2), BMFACT, OUTSID)
                           IF (BMFACT.LT.PBPARM(1)) THEN
                              WT = 0.0
                           ELSE
                              WT = WT * BMFACT * BMFACT / NOISE / NOISE
                              ROWI(I1) = ROWI(I1) / BMFACT
                              END IF
                           END IF
C                                       radial downweight
                        IF ((DORADI) .AND. (WT.GT.0.0)) THEN
                           ZREF = SIN (YREF) * SIN (DECREF) + COS (YREF)
     *                        * COS (DECREF) * COS (XREF - RAREF)
                           ANGLER = ACOS (ZREF) * RAD2DG
                           WT = WT * MAX (0.0001D0, 1.0D0-ANGLER*RFACT)
                           END IF
C                                       add it in finally
                        IF (ROWO(I1).NE.FBLANK) THEN
                           ROWO(I1) = ROWO(I1) + ROWI(I1) * WT
                           ROWW(I1) = ROWW(I1) + WT
                        ELSE
                           ROWO(I1) = ROWI(I1) * WT
                           ROWW(I1) = WT
                           END IF
                        END IF
 110                 CONTINUE
                  CALL ARRWRI (WWT, DIM, ROWW, IRET)
                  IF (IRET.NE.0) GO TO 999
                  CALL ARRWRI (WOUT, DIM, ROWO, IRET)
                  IF (IRET.NE.0) GO TO 999
 120              CONTINUE
 130           CONTINUE
            CALL ARRCLO (OUT, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OCLOSE (OUT, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL ARRCLO (WOUT, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OCLOSE (WOUT, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL ARRCLO (SCF(1), IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OCLOSE (SCF(1), IRET)
            IF (IRET.NE.0) GO TO 999
            CALL ARRCLO (SCF(2), IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OCLOSE (SCF(2), IRET)
            IF (IRET.NE.0) GO TO 999
            CALL ARRCLO (WWT, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OCLOSE (WWT, IRET)
            IF (IRET.NE.0) GO TO 999
 200        CONTINUE
C                                       divide by weights
         IF (OPTYPE.EQ.'NOIS') THEN
            MSGTXT = 'Now output noise image = 1 / ' //
     *         'SQRT (sum of the weights)'
         ELSE IF (OPTYPE.EQ.'WEIG') THEN
            MSGTXT = 'Now output weight image = (sum of the weights)'
         ELSE
            MSGTXT = 'Now output average image'
            END IF
         CALL MSGWRT (2)
         CALL COPY (7, NAX2, TRC)
         CALL FILL (7, 1, BLC)
         DIM(1) = 7
         DIM(2) = 1
         CALL OPUT (SCF(2), 'BLC', OOAINT, DIM, BLC, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT (SCF(2), 'TRC', OOAINT, DIM, TRC, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT (OUT, 'BLC', OOAINT, DIM, BLC, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT (OUT, 'TRC', OOAINT, DIM, TRC, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT (WOUT, 'BLC', OOAINT, DIM, BLC, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OPUT (WOUT, 'TRC', OOAINT, DIM, TRC, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OOPEN (OUT, 'READ', IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OOPEN (SCF(2), 'READ', IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OOPEN (WOUT, 'WRIT', IRET)
         IF (IRET.NE.0) GO TO 999
         DO 230 I7 = 1,NAX2(7)
         DO 230 I6 = 1,NAX2(6)
         DO 230 I5 = 1,NAX2(5)
         DO 230 I4 = 1,NAX2(4)
         DO 230 I3 = 1,NAX2(3)
            DO 220 I2 = 1,NAX2(2)
               CALL ARREAD (OUT, DIM, ROWO, IRET)
               IF (IRET.NE.0) GO TO 999
               CALL ARREAD (SCF(2), DIM, ROWW, IRET)
               IF (IRET.NE.0) GO TO 999
               DO 210 I1 = 1,NAX2(1)
                  IF (ROWW(I1).LE.0.0) THEN
                     ROWO(I1) = FBLANK
                  ELSE IF (ROWO(I1).NE.FBLANK) THEN
                     IF (OPTYPE.EQ.'NOIS') THEN
                        ROWO(I1) = 1.0 / SQRT (ROWW(I1))
                     ELSE IF (OPTYPE.EQ.'WEIG') THEN
                        ROWO(I1) = ROWW(I1)
                     ELSE
                        ROWO(I1) = ROWO(I1) / ROWW(I1)
                        END IF
                     END IF
 210              CONTINUE
               CALL ARRWRI (WOUT, DIM, ROWO, IRET)
               IF (IRET.NE.0) GO TO 999
 220           CONTINUE
 230        CONTINUE
         CALL ARRCLO (OUT, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OCLOSE (OUT, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ARRCLO (SCF(2), IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OCLOSE (SCF(2), IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ARRCLO (WOUT, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OCLOSE (WOUT, IRET)
         IF (IRET.NE.0) GO TO 999
C
         CALL DESTRY (WWT, IRET)
         CALL DESTRY (WOUT, IRET)
         END IF
C                                       delete scratch
      CALL IMGZAP (SCF(1), IRET)
      CALL IMGZAP (SCF(2), IRET)
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('Field',I6,' not included in output - outside image')
 1031 FORMAT ('Field',I6,' not included in output - geometry failure')
 1032 FORMAT ('Field',I6,' included blc,trc',4I6)
      END
      SUBROUTINE FLTNHI (IN, OUT)
C-----------------------------------------------------------------------
C   Routine to write history file to output.
C   Inputs:
C      IN    C(*)*?   Input object
C      OUT   C*?      Output object
C-----------------------------------------------------------------------
      CHARACTER IN(*)*(*), OUT*(*)
C
      INTEGER   NADV
      PARAMETER (NADV=12)
      CHARACTER LIST(NADV)*8
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'NFIELD',
     *   'NMAPS', 'IMSIZE', 'REWEIGHT', 'DOWEIGHT', 'APARM', 'PBPARM',
     *   'NOISE'/
C-----------------------------------------------------------------------
C                                        Copy old history
      CALL OHCOPY (IN(1), OUT, IERR)
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
C
 999  RETURN
      END
