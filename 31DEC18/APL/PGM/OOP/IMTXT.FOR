LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NPARMS
      PARAMETER (NPARMS=10)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'BLC', 'TRC', 'FACTOR', 'FORMAT', 'BLNK', 'OUTFILE'/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT,
     *   OOAINT, OOAINT, OOARE, OOACAR, OOARE, OOACAR/
      DATA AVDIM /12,1, 6,1, 1,1, 1,1,
     *   7,1, 7,1, 1,1, 14,1, 1,1, 48,1/
LOCAL END
      PROGRAM IMTXT
C-----------------------------------------------------------------------
C! Write an image to an external text file.
C# Task OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
      CHARACTER PRGM*6, IMAGE*32
      INTEGER  IRET, BUFF1(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PAOOF.INC'
      DATA PRGM /'IMTXT'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL IM2TIN (PRGM, IMAGE, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       Write header
      CALL IM2THD (IMAGE, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       Write data
      CALL IM2TCP (IMAGE, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE IM2TIN (PRGN, IMAGE, IERR)
C-----------------------------------------------------------------------
C   IM2TIN gets input parameters for IMTXT and creates the input object.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IMAGE   C*?  Input image object.
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER PRGN*6, IMAGE*(*)
      INTEGER   IERR
C
      INTEGER   NKEY1
C                                       NKEY1=no. adverbs to copy to
C                                       IMAGE
      PARAMETER (NKEY1=10)
      INTEGER   DIM(7), TYPE, DUMMY
      REAL      FACTOR
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, INPUT*32, FORMAT*14,
     *   OUTF*48, CDUMMY*1
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs for IMAGE
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK',
     *   'BLC', 'TRC', 'FACTOR', 'FORMAT', 'BLNK', 'OUTFILE'/
      DATA OUTK1 /'NAME', 'CLASS', 'IMSEQ', 'DISK',
     *   'BLC', 'TRC', 'FACTOR', 'FORMAT', 'BLNK', 'OUTFILE'/
C-----------------------------------------------------------------------
C                                       Startup
      INPUT = 'Task Input'
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, INPUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Defaults
C                                       FACTOR
      CALL OGET (INPUT, 'FACTOR', TYPE, DIM, FACTOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (ABS (FACTOR).LT.1.0E-20) FACTOR = 1.0
      CALL OPUT (INPUT, 'FACTOR', OOARE, DIM, FACTOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Check FORMAT
      CALL OGET (INPUT, 'FORMAT', TYPE, DIM, DUMMY, FORMAT, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (FORMAT.EQ.' ') THEN
         IERR = 2
         MSGTXT = 'YOU MUST SPECIFY A FORMAT (TRANSCOD)'
         GO TO 990
         END IF
C                                       Check OUTFILE
      CALL OGET (INPUT, 'OUTFILE', TYPE, DIM, DUMMY, OUTF, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (OUTF.EQ.' ') THEN
         IERR = 2
         MSGTXT = 'YOU MUST SPECIFY AN OUTFILE'
         GO TO 990
         END IF
C                                       Create IMAGE
      IMAGE = 'Input Image'
      CALL CREATE (IMAGE, 'IMAGE', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ (INPUT, NKEY1, INK1, OUTK1, IMAGE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open IMAGE to be sure it's OK.
      CALL OOPEN (IMAGE, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OCLOSE (IMAGE, IERR)
      IF (IERR.NE.0) GO TO 999
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE IM2THD (IMAGE, IERR)
C-----------------------------------------------------------------------
C   Write Header info to output file.
C   Inputs:
C      IMAGE    C*?  Input file.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER IMAGE*(*)
      INTEGER   IERR
C
      INTEGER  DIM(3), TYPE, LUN, FIND, NDIM, NAXIS(7), LROW, I, ITRIM,
     *   LSTR, BLC(7), TRC(7), DUMMY
      CHARACTER OUTF*48, FORMAT*14, LINE*256, OBJECT*8, TELES*8,
     *   INSTRU*8, OBS*8, UNITS*8, CTYPE(7)*8, FORM*20, CDUMMY*1
      DOUBLE PRECISION CRVAL(7)
      REAL     EPOCH, CDELT(7), CRPIX(7), CROTA(7), FACTOR, BLNK, OFF,
     *   XFACT
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Output file name
      CALL OGET (IMAGE, 'OUTFILE', TYPE, DIM, DUMMY, OUTF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Array size
      CALL ARDGET (IMAGE, 'NDIM', TYPE, DIM, NDIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ARRWIN (IMAGE, BLC, TRC, NAXIS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Image info
      CALL IMDGET (IMAGE, 'OBJECT', TYPE, DIM, DUMMY, OBJECT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMDGET (IMAGE, 'TELESCOP', TYPE, DIM, DUMMY, TELES, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMDGET (IMAGE, 'INSTRUME', TYPE, DIM, DUMMY, INSTRU, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMDGET (IMAGE, 'OBSERVER', TYPE, DIM, DUMMY, OBS, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMDGET (IMAGE, 'BUNIT', TYPE, DIM, DUMMY, UNITS, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMDGET (IMAGE, 'EPOCH', TYPE, DIM, EPOCH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMDGET (IMAGE, 'CTYPE', TYPE, DIM, DUMMY, CTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMDGET (IMAGE, 'CRVAL', TYPE, DIM, CRVAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMDGET (IMAGE, 'CDELT', TYPE, DIM, CDELT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMDGET (IMAGE, 'CRPIX', TYPE, DIM, CRPIX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMDGET (IMAGE, 'CROTA', TYPE, DIM, CROTA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Correct reference pixels
      DO 50 I = 1,NDIM
         CRPIX(I) = CRPIX(I) - BLC(I) + 1
 50      CONTINUE
C                                       Other
      CALL OGET (IMAGE, 'FORMAT', TYPE, DIM, DUMMY, FORMAT, IERR)
      IF (IERR.NE.0) GO TO 990
      LROW = NAXIS(1)
      IF (LROW.LT.10) THEN
         WRITE (FORM(1:1),1001) LROW
         FORM(2:) = FORMAT
      ELSE IF (LROW.LT.100) THEN
         WRITE (FORM(1:2),1002) LROW
         FORM(3:) = FORMAT
      ELSE IF (LROW.LT.1000) THEN
         WRITE (FORM(1:3),1003) LROW
         FORM(4:) = FORMAT
      ELSE IF (LROW.LT.10000) THEN
         WRITE (FORM(1:4), 1004) LROW
         FORM(5:) = FORMAT
         END IF
      CALL OGET (IMAGE, 'FACTOR', TYPE, DIM, FACTOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (IMAGE, 'BLNK', TYPE, DIM, BLNK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      OFF = 0.0
C                                       Save derived format
      DIM(1) = LEN (FORM)
      DIM(2) = 1
      CALL OPUT (IMAGE, 'FORM', OOACAR, DIM, DUMMY, FORM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open the output file
      LUN = 10
      CALL ZTXOPN ('WRIT', LUN, FIND, OUTF, .FALSE., IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Header info: NAXIS
      WRITE  (LINE,2000) NDIM
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(1:ITRIM(LINE)), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       DIM
      WRITE (LINE,2001) (NAXIS(I), I=1,NDIM)
      LSTR = ITRIM (LINE)
C                                       Remove last comma
      LINE(LSTR:LSTR) = ' '
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(1:ITRIM(LINE)), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       FORMAT
      LINE = 'FORMAT = ''' // FORM(1:ITRIM(FORM)) // ''''
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(1:ITRIM(LINE)), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       OBJECT
      LINE = 'OBJECT = ''' // OBJECT(1:ITRIM(OBJECT)) // ''''
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(1:ITRIM(LINE)), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       TELESCOP
      LINE = 'TELESCOP = ''' // TELES(1:ITRIM(TELES)) // ''''
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(1:ITRIM(LINE)), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       INSTRUME
      LINE = 'INSTRUME = ''' // INSTRU(1:ITRIM(INSTRU)) // ''''
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(1:ITRIM(LINE)), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       OBSERVER
      LINE = 'OBSERVER = ''' // OBS(1:ITRIM(OBS)) // ''''
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(1:ITRIM(LINE)), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       UNITS
      LINE = 'UNITS = ''' // UNITS(1:ITRIM(UNITS)) // ''''
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(1:ITRIM(LINE)), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       EPOCH
      WRITE (LINE,2002) EPOCH
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(1:ITRIM(LINE)), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CRTYPE
      WRITE (LINE,2003) (CTYPE(I), I=1,NDIM)
      LSTR = ITRIM (LINE)
      LINE(LSTR-1:LSTR) = '  '
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(1:ITRIM(LINE)), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CRVAL
      WRITE (LINE,2004) (CRVAL(I), I=1,NDIM)
      LSTR = ITRIM (LINE)
      LINE(LSTR:LSTR) = ' '
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(1:ITRIM(LINE)), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CRINC
      WRITE (LINE,2005) (CDELT(I), I=1,NDIM)
      LSTR = ITRIM (LINE)
      LINE(LSTR:LSTR) = ' '
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(1:ITRIM(LINE)), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CRREF
      WRITE (LINE,2006) (CRPIX(I), I=1,NDIM)
      LSTR = ITRIM (LINE)
      LINE(LSTR:LSTR) = ' '
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(1:ITRIM(LINE)), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CRROT
      WRITE (LINE,2007) (CROTA(I), I=1,NDIM)
      LSTR = ITRIM (LINE)
      LINE(LSTR:LSTR) = ' '
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(1:ITRIM(LINE)), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       SCALE
      XFACT = 1.0 / FACTOR
      WRITE (LINE,2008) XFACT
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(1:ITRIM(LINE)), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       OFFSET
      WRITE (LINE,2009) OFF
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(1:ITRIM(LINE)), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       BLANK
      WRITE (LINE,2010) BLNK
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(1:ITRIM(LINE)), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Terminator
      LINE = '  /  '
      CALL ZTXIO ('WRIT', LUN, FIND, LINE(1:ITRIM(LINE)), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close text file
      CALL ZTXCLS (LUN, FIND, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 985  CALL MSGWRT (7)
 990  MSGTXT = 'ERROR WRITING HEADER FOR ' // IMAGE
      CALL MSGWRT (7)
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT (I1)
 1002 FORMAT (I2)
 1003 FORMAT (I3)
 1004 FORMAT (I4)
 2000 FORMAT ('NAXIS = ', I2)
 2001 FORMAT ('DIM = ', 7(I5, ','))
 2002 FORMAT ('EPOCH = ', F8.1)
 2003 FORMAT ('CRTYPE = ', 7('''',A,''','))
 2004 FORMAT ('CRVAL = ', 7(1PD20.12,','))
 2005 FORMAT ('CRINC = ', 7(1PE15.7,','))
 2006 FORMAT ('CRREF = ', 7(F15.4,','))
 2007 FORMAT ('CRROT = ', 7(F15.4,','))
 2008 FORMAT ('SCALE = ', 1PE15.7)
 2009 FORMAT ('OFFSET = ', 1PE15.7)
 2010 FORMAT ('BLANK = ', 1PE15.7)
      END
      SUBROUTINE IM2TCP (IMAGE, IERR)
C-----------------------------------------------------------------------
C   Write Image pixels to output
C   Inputs:
C      IMAGE    C*?  Name of input uvdata object.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER IMAGE*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   TYPE, DIM(3), LUN, FIND, I, ITRIM, LSTR, I2, I3, I4, I5,
     *   I6, I7, BLC(7), TRC(7), NAXIS(7), IROW(8192), IROUND, DUMMY
      LOGICAL   DOINT
      REAL      ROW(MAXIMG), FACTOR, BLNK
      CHARACTER OUTF*48, FORM*20, FORMAT*22, LINE*32768, CDUMMY*1
      EQUIVALENCE (ROW, IROW)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Output file name
      CALL OGET (IMAGE, 'OUTFILE', TYPE, DIM, DUMMY, OUTF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Other
      CALL OGET (IMAGE, 'FORM', TYPE, DIM, DUMMY, FORM, IERR)
      IF (IERR.NE.0) GO TO 990
      FORMAT = '(' // FORM
      LSTR = ITRIM (FORMAT)
      FORMAT(LSTR+1:LSTR+1) = ')'
C                                       Integer?
      DOINT = INDEX (FORMAT, 'I') .GT. 0
      CALL OGET (IMAGE, 'FACTOR', TYPE, DIM, FACTOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (IMAGE, 'BLNK', TYPE, DIM, BLNK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ARRWIN (IMAGE, BLC, TRC, NAXIS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open the output file
      LUN = 10
      CALL ZTXOPN ('WRIT', LUN, FIND, OUTF, .TRUE., IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Loop over image
      DO 170 I7 = BLC(7),TRC(7)
         DO 160 I6 = BLC(6),TRC(6)
            DO 150 I5 = BLC(5),TRC(5)
               DO 140 I4 = BLC(4),TRC(4)
                  DO 130 I3 = BLC(3),TRC(3)
                     DO 120 I2 = BLC(2),TRC(2)
         CALL ARREAD (IMAGE, DIM, ROW, IERR)
         IF (IERR.GT.0) GO TO 990
C                                       Scale/blank
         DO 110 I = 1,DIM(1)
            IF (ROW(I).EQ.FBLANK) THEN
               ROW(I) = BLNK
            ELSE
               ROW(I) = ROW(I) * FACTOR
               END IF
C                                       Integerize?
            IF (DOINT) IROW(I) = IROUND (ROW(I))
 110        CONTINUE
C                                       Encode in LINE by type
         IF (DOINT) THEN
            WRITE (LINE,FORMAT,ERR=800) (IROW(I),I=1,DIM(1))
         ELSE
            WRITE (LINE,FORMAT,ERR=800) (ROW(I),I=1,DIM(1))
            END IF
         CALL ZTXIO ('WRIT', LUN, FIND, LINE(1:ITRIM(LINE)), IERR)
         IF (IERR.NE.0) GO TO 990
 120                  CONTINUE
 130               CONTINUE
 140            CONTINUE
 150         CONTINUE
 160      CONTINUE
 170   CONTINUE
C                                       Close text file
      CALL ZTXCLS (LUN, FIND, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close array
      CALL ARRCLO (IMAGE, IERR)
      IF (IERR.GT.0) GO TO 990
      GO TO 999
C                                       ENCODE error
 800  MSGTXT = 'ERROR WRITING OUTPUT TEXT LINE'
      IERR = 5
C                                       Error
 985  CALL MSGWRT (7)
 990  MSGTXT = 'IM2TCP: ERROR WRITING ' // IMAGE
      CALL MSGWRT (7)
C
 999  RETURN
      END
