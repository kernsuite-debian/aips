LOCAL INCLUDE 'STFUN.INC'
      INTEGER   CATIN(256), CATBLK(256), CATBL2(256)
      REAL      CATRI(256), CATR(256)
      HOLLERITH CATHI(256), CATH(256)
      DOUBLE PRECISION CATDI(128), CATD(256)
      EQUIVALENCE (CATIN, CATRI, CATHI, CATDI)
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      COMMON /HEADS/ CATIN, CATBLK, CATBL2
LOCAL END
      PROGRAM STFUN
C-----------------------------------------------------------------------
C! Computes the structure function of an image
C# Map Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999, 2005, 2008-2009, 2011-2012, 2015
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
C     STFUN calculates a structure function image.
C   AIPS INPUTS:
C
C 1         USERID               Owner of the image
C 2-4       INNAME(3)            Image name (name)
C 5-6       INCLASS(2)           Image name (class)
C 7         INSEQ                Image name (seq. #)
C 8         INDISK               Disk # of image
C 9-11      OUTNAME(3)           Output image name (name)
C 12-13     OUTCLASS(2)          Output image name (class)
C 14        OUTSEQ               Output image name (seq. #)
C 15        OUTDISK              Disk # of output image
C 16-22     BLC(7)               BLC of area to be affected
C 23-29     TRC(7)               TRC of area to be affected
C 30-31     IMSIZE(2)            Output image size
C 32-35     APARM(1:4)           Lag range, xlo, xhi, ylo, yhi
C 36        APARM(5)             Blank lag=(0,0) pixel or zero it
C 37        APARM(6)             Blank or zero inner square
C 38        APARM(7)             Blank or zero outer frame when padding
C                                image
C
C     Neil Killeen / John Simonetti, March 1987
C-----------------------------------------------------------------------
      REAL      RPARM(41)
      CHARACTER INNAM*36, OUTNAM(2)*36
      INTEGER   IDIM1, BLC(2), TRC(2), LAGS(4), IMSIZE(2)
      INTEGER   IERR, IER, LUNIN, LUNOUT(2), NPARMS, INSLO, OUTSLO(2)
      INCLUDE 'STFUN.INC'
      DATA IDIM1 /512/
      DATA LUNIN, LUNOUT, NPARMS /17, 18, 19, 41/
C-----------------------------------------------------------------------
C                                       Start up task and get inputs
      CALL START (NPARMS, RPARM, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Open input and output images
C                                       [and possibly increase IDIM1]
      CALL OPENIM (IDIM1, RPARM, LUNIN, LUNOUT, INNAM, OUTNAM, BLC,
     *   TRC, IMSIZE, LAGS, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Calculate the structure
C                                       function image
      CALL OUTPUT (RPARM, LUNIN, LUNOUT, BLC, TRC, IMSIZE, LAGS, INSLO,
     *   OUTSLO, IERR, IDIM1)
      IF (IERR.NE.0) GO TO 900
C                                       Add history file
      CALL STFUNH (INNAM, OUTNAM, INSLO, OUTSLO, BLC, TRC, IMSIZE, LAGS)
C
900   IF (IERR.EQ.0) THEN
         IER = 0
      ELSE
         IER = 1
         END IF
      CALL CLENUP
      CALL TSKEND (IER)
C
 999  STOP
      END
      SUBROUTINE START (NPARMS, RPARM, IERR)
C-----------------------------------------------------------------------
C     Start up task, get AIPS inputs and write initial message.
C     RPARM must NOT be declared RPARM(NPARMS) because NPARMS is I
C     Input:
C       NPARMS     I      number of AIPS inputs
C     Output:
C       RPARM      R      array containing AIPS parameters
C       IERR       I      error status, 0=> OK
C-----------------------------------------------------------------------
      REAL      RPARM(*)
      INTEGER   NPARMS, IERR
C
      CHARACTER PRGNAM*6
      INCLUDE 'INCS:DMSG.INC'
      DATA PRGNAM /'STFUN '/
C-----------------------------------------------------------------------
C                                       Start up task, get AIPS
C                                       parameters
      CALL TSKBEG (PRGNAM, NPARMS-1, RPARM(2), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
      ELSE
         WRITE (MSGTXT,2000)
         CALL MSGWRT (8)
         END IF
      RPARM(1) = NLUSER
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('START: COULD NOT GET INPUTS, IERR=', I3)
 2000 FORMAT ('STFUN IS A NON-STANDARD PROGRAM')
      END
      SUBROUTINE OPENIM (IDIM1, RPARM, LUNIN, LUNOUT, INNAM, OUTNAM,
     *   BLC, TRC, IMSIZE, LAGS, IERR)
C-----------------------------------------------------------------------
C     Open the input image, create and open the output image.
C     Input:
C         IDIM1     I       Maximum dimensions for input window
C         RPARM     R       Array of AIPS input parameters
C         LUNIN     I       Logical unit number for input image
C         LUNOUT    I       Logical unit number for output image
C     Output:
C         INNAM     C*36    Namestring for input image
C         OUTNAM    C*36    Namestring for output image
C         BLC,TRC   I       Window on input image, defaults filled in
C         IMSIZE    I       Output image size
C         LAGS      I       Lag range, xlo, xhi, ylo, yhi
C         IERR      I       Error status, 0=> OK
C
C-----------------------------------------------------------------------
      INTEGER   IDIM1, LUNIN, LUNOUT(2), BLC(2), TRC(2), IMSIZE(2),
     *   LAGS(4), IERR
      REAL      RPARM(*)
      CHARACTER INNAM*36, OUTNAM(2)*36
C-----------------------------------------------------------------------
C                                       Open input image (possibly
C                                        increase IDIM1)
      CALL OPENIN (IDIM1, RPARM, LUNIN, INNAM, BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Set lags
      CALL SETLAG (RPARM, BLC, TRC, LAGS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Create and open output image
C                                       [possibly increase IDIM1]
      CALL OPENOU (IDIM1, INNAM, RPARM, LUNOUT, LAGS, IMSIZE,
     *   OUTNAM, IERR)
C
 990  RETURN
      END
      SUBROUTINE OPENIN (IDIM1, RPARM, LUNIN, INNAM, BLC, TRC, IERR)
C-----------------------------------------------------------------------
C   Set up the input namestring, open and window the input image,
C   and get the header.
C   Input:
C      IDIM1     I       Maximum allowed dimension for input window
C      RPARM     R       Array of AIPS input parameters
C      LUNIN     I       Logical unit number for input image
C   Output:
C      INNAM     C*36    Namestring for input image
C      BLC,TRC   I       Input image window, defaults filled in
C      IERR      I       Error status, 0=> OK
C    RPARM PARAMETERS:
C 1         USERID               Owner of the image
C 2-4       INNAME(3)            Image name (name)
C 5-6       INCLASS(2)           Image name (class)
C 7         INSEQ                Image name (seq. #)
C 8         INDISK               Disk # of image
C 16-22     BLC(7)               BLC of area to be affected
C 23-29     TRC(7)               TRC of area to be affected
C-----------------------------------------------------------------------
      INTEGER   IDIM1, LUNIN, BLC(2), TRC(2), IERR
      REAL      RPARM(*)
      CHARACTER INNAM*36
C
      HOLLERITH MAP
      INCLUDE 'STFUN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Set up namestring
      CALL CHR2H (4, 'MA  ',1, MAP)
      CALL H2WAWA (RPARM(2), RPARM(5), RPARM(7), MAP, RPARM(8),
     *   RPARM(1), INNAM)
C                                       Open file
      CALL OPENCF (LUNIN, INNAM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 900
         END IF
C                                       Get header
      CALL GETHDR (LUNIN, CATIN, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,2000) IERR
         GO TO 900
         END IF
C                                       Set up input image window
      CALL WININP (LUNIN, RPARM, IDIM1, CATIN(KINAX), CATIN(KINAX+1),
     *   BLC, TRC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,3000) IERR
         GO TO 900
         END IF
C
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OPENIN: ERROR OPENING INPUT IMAGE, IERR=', I3)
 2000 FORMAT ('OPENIN: ERROR GETTING INPUT IMAGE HEADER', ' IERR=', I3)
 3000 FORMAT ('OPENIN: ERROR WINDOWING INPUT IMAGE, IERR=', I3)
      END
      SUBROUTINE WININP (LUNIN, RPARM, MAXDIM, NCOLIN, NROWIN, BLC,
     *   TRC, IERR)
C-----------------------------------------------------------------------
C   Set the input image window.
C   Input:
C      LUNIN     I       LUN for input image
C      RPARM     R       Array of AIPS input parameters
C      MAXDIM    I       Maximum allowed dimension of window
C      NCOLIN    I       Number of columns in input image
C      NROWIN    I       Number of rows in input image
C   Output:
C      BLC,TRC   I       Input image window, defaults filled in
C      IERR      I       error status, 0=> OK
C   RPARM PARAMETERS:
C 16-22     BLC(7)               BLC of area to be affected
C 23-29     TRC(7)               TRC of area to be affected
C-----------------------------------------------------------------------
      INTEGER   LUNIN, MAXDIM, NCOLIN, NROWIN, BLC(2), TRC(2), IERR
      REAL      RPARM(*)
C
      REAL      RWIN(4)
      INTEGER   COLWIN, ROWWIN
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Extract user supplied window
      BLC(1) = RPARM(16) + 0.01
      BLC(2) = RPARM(17) + 0.01
      TRC(1) = RPARM(23) + 0.01
      TRC(2) = RPARM(24) + 0.01
C                                       Default window is all of input
C                                       image
      IF (BLC(1).EQ.0) BLC(1) = 1
      IF (BLC(2).EQ.0) BLC(2) = 1
      IF (TRC(1).EQ.0) TRC(1) = NCOLIN
      IF (TRC(2).EQ.0) TRC(2) = NROWIN
C                                       Is window too big ?
      COLWIN = TRC(1) - BLC(1) + 1
      ROWWIN = TRC(2) - BLC(2) + 1
C
      MAXDIM = MAX (MAXDIM, COLWIN, ROWWIN)
C
      IF (COLWIN.GT.NCOLIN) THEN
         WRITE (MSGTXT,1000) COLWIN, NCOLIN
         CALL MSGWRT (8)
         IERR = 1
      ELSE IF (ROWWIN.GT.NROWIN) THEN
         WRITE (MSGTXT,2000) ROWWIN, NROWIN
         CALL MSGWRT (8)
         IERR = 2
C                                       ??????????
      ELSE IF ((COLWIN.GT.MAXDIM) .OR. (ROWWIN.GT.MAXDIM)) THEN
         WRITE (MSGTXT,3000) MAXDIM
         CALL MSGWRT (8)
         IERR = 3
C                                       Set the window
      ELSE
         RWIN(1) = REAL (BLC(1))
         RWIN(2) = REAL (BLC(2))
         RWIN(3) = REAL (TRC(1))
         RWIN(4) = REAL (TRC(2))
         CALL MAPXY (LUNIN, RWIN, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,4000) IERR
            CALL MSGWRT (8)
            END IF
         END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WININP: X-WINDOW=',I4, ' > INPUT IMAGE X-DIMENSION=', I4)
 2000 FORMAT ('WININP: Y-WINDOW=',I4, ' > INPUT IMAGE Y-DIMENSION=', I4)
 3000 FORMAT ('WININP: WINDOW TOO LARGE, REQUIRE SIDES < ', I3,
     *        ' PIXELS')
 4000 FORMAT ('WININP: ERROR IN MAPXY SETTING WINDOW, IERR=', I3)
      END
      SUBROUTINE SETLAG (RPARM, BLC, TRC, LAGS, IERR)
C-----------------------------------------------------------------------
C   Extract the lags from the input parameters and check that they are
C   sensible
C   Input:
C      RPARM    R     AIPS inputs
C      BLC,TRC  I     Input map window
C   Output:
C      LAGS     I     Lags, xlow, xhigh, ylow, yhigh
C      IERR     I     0 => OK
C   RPARM parameters:
C 32-35     APARM(1:4)           Lag range, xlo, xhi, ylo, yhi
C-----------------------------------------------------------------------
      REAL      RPARM(*)
      INTEGER   LAGS(4), BLC(2), TRC(2), NROWIN, NCOLIN, I
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Extract parameters
      DO 10 I = 1,4
         LAGS(I) = RPARM(I+31) + 0.01
 10      CONTINUE
C                                       Input map windows
      NROWIN = TRC(2) - BLC(2) + 1
      NCOLIN = TRC(1) - BLC(1) + 1
C                                       Set maximum lag default
      IF (LAGS(2).EQ.0) LAGS(2) = NCOLIN - 1
      IF (LAGS(4).EQ.0) LAGS(4) = NROWIN - 1
C                                       Check lags smaller than
C                                       dimensions
      IF (LAGS(1).GT.NCOLIN .OR. LAGS(2).GT.NCOLIN .OR.
     *    LAGS(3).GT.NROWIN .OR. LAGS(4).GT.NROWIN) THEN
         MSGTXT = 'SETLAG: LAGS EXCEED INPUT WINDOW'
         CALL MSGWRT (8)
         IERR = 1
C                                       Check lags positive
      ELSE IF (LAGS(1).LT.0 .OR. LAGS(2).LT.0 .OR. LAGS(3).LT.0 .OR.
     *         LAGS(4).LT.0) THEN
         MSGTXT = 'SETLAG: LAGS NEGATIVE'
         CALL MSGWRT (8)
         IERR = 2
C                                       Check lags in correct order
      ELSE IF (LAGS(2).LT.LAGS(1) .OR. LAGS(4).LT.LAGS(3)) THEN
         MSGTXT = 'SETLAG: LAG ORDER INCORRECT'
         CALL MSGWRT (8)
         IERR = 3
C                                       Make sure that they are not
C                                       equal
      ELSE IF (LAGS(1).EQ.LAGS(2) .OR. LAGS(3).EQ.LAGS(4)) THEN
         MSGTXT = 'SETLAG: X OR Y LAGS EQUAL'
         CALL MSGWRT (8)
         IERR = 4
      END IF
C
 999  RETURN
      END
      SUBROUTINE OPENOU (IDIM1, INNAM, RPARM, LUNOUT, LAGS, IMSIZE,
     *   OUTNAM, IERR)
C-----------------------------------------------------------------------
C   Create and open the output image
C   Inputs:
C      IDIM1     I       Maximum dimensions for input image
C      INNAM     C*36    Namestring for input image
C      RPARM     R       array of AIPS input parameters
C      LUNOUT    I       logical unit number for output image
C      LAGS      I       Lag array
C   Outputs:
C      IMSIZE    I       Output image size
C      OUTNAM    C*36    Namestring for output image
C      IERR      I       error status, 0=> OK
C   RPARM PARAMETERS:
C 1         USERID               Owner of the image
C 2-4       INNAME(3)            Image name (name)
C 5-6       INCLASS(2)           Image name (class)
C 7         INSEQ                Image name (seq. #)
C 8         INDISK               Disk # of image
C 9-11      OUTNAME(3)           Output image name (name)
C 12-13     OUTCLASS(2)          Output image name (class)
C 14        OUTSEQ               Output image name (seq. #)
C 15        OUTDISK              Disk # of output image
C 30-31     IMSIZE(2)            Output image size
C-----------------------------------------------------------------------
      INTEGER   IDIM1, LUNOUT(2), LAGS(4), IMSIZE(2), IERR
      CHARACTER INNAM*36, OUTNAM(2)*36
      REAL      RPARM(*)
C
      CHARACTER DEFNAM(2)*36, NAME*12, CLASS*6, PTYPE*2
      HOLLERITH MAP, HTEMP(2)
      INTEGER   SEQ, DISK, USID
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'STFUN.INC'
C-----------------------------------------------------------------------
C                                       Make output header
      CALL HEADER (IDIM1, RPARM, LAGS, IMSIZE, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'OPENOU: ERROR MAKING OUTPUT HEADER'
         GO TO 900
         END IF
C                                       Default output namestring
      CALL CHR2H (4, 'MA  ', 1, MAP)
      CALL WAWA2A (INNAM, NAME, CLASS, SEQ, PTYPE, DISK, USID)
      CLASS = 'STFUN '
      CALL A2WAWA (NAME, CLASS, SEQ, PTYPE, DISK, USID, DEFNAM(1))
      CLASS = 'STFUNR'
      CALL A2WAWA (NAME, CLASS, SEQ, PTYPE, DISK, USID, DEFNAM(2))
C                                       Output namestring
      CALL H2WAWA (RPARM(9), RPARM(12), RPARM(14), MAP, RPARM(15),
     *   RPARM(1), OUTNAM(1))
      CALL H2CHR (8, 1, RPARM(12), CLASS)
      IF (CLASS.EQ.' ') CLASS = TSKNAM
      CLASS(6:6) = 'R'
      CALL CHR2H (8, CLASS, 1, HTEMP)
      CALL H2WAWA (RPARM(9), HTEMP, RPARM(14), MAP, RPARM(15),
     *   RPARM(1), OUTNAM(2))
C                                       Create file
      CALL MAPCR (DEFNAM(1), OUTNAM(1), CATBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,2000) 1, IERR
         GO TO 900
         END IF
C                                       Open file
      CALL OPENCF (LUNOUT(1), OUTNAM(1), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,3000) 1, IERR
         END IF
C                                       Create file
      CALL COPY (256, CATBLK, CATBL2)
      CALL MAPCR (DEFNAM(2), OUTNAM(2), CATBL2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,2000) 2, IERR
         GO TO 900
         END IF
C                                       Open file
      CALL OPENCF (LUNOUT(2), OUTNAM(2), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,3000) 2, IERR
         END IF
C
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 2000 FORMAT ('OPENOU: COULD NOT CREATE OUTPUT IMAGE',I2,'  IERR=',I3)
 3000 FORMAT ('OPENOU: COULD NOT OPEN OUTPUT IMAGE',I2,'  IERR=',I3)
      END
      SUBROUTINE HEADER (IDIM1, RPARM, LAGS, IMSIZE, IERR)
C-----------------------------------------------------------------------
C   Make the output image header from input image header
C   Input:
C      IDIM1     I       Maximum dimensions for input image
C      RPARM     R       AIPS inputs
C      LAGS      I       Lag array, xlo, xhi, ylo, yhi
C   Output:
C      IMSIZE    I       Output image size
C      IERR      I       0 => OK
C   RPARM parameters
C 30-31     IMSIZE(2)            Output image size
C-----------------------------------------------------------------------
      INTEGER    IDIM1, LAGS(4), IMSIZE(2), IERR
      REAL       RPARM(*)
C
      CHARACTER UNITS*8
      INTEGER   I, NXSIZE, NYSIZE, IDIMO, JTMP
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'STFUN.INC'
      DATA UNITS /'SQRUNITS'/
C-----------------------------------------------------------------------
      JTMP = MAX (RPARM(30), RPARM(31)) + 0.01
      JTMP = (JTMP - 1) / 2
      IDIM1 = MAX (IDIM1, JTMP)
C                                       Copy header
      DO 100 I = 1,256
         CATBLK(I) = CATIN(I)
 100     CONTINUE
C                                       Work out output image size
      NXSIZE = 2 * LAGS(2) + 1
      NYSIZE = 2 * LAGS(4) + 1
      IDIMO = 2 * IDIM1 - 1
      CALL IMDIMS (1, IDIMO, RPARM(30), NXSIZE, IMSIZE(1), IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMDIMS (2, IDIMO, RPARM(31), NYSIZE, IMSIZE(2), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Fill new header values
      CATBLK(KIDIM) = 2
      CATBLK(KINAX) = IMSIZE(1)
      CATBLK(KINAX+1) = IMSIZE(2)
      CATBLK(KINAX+2) = 1
C
      CATR(KRCRP) = REAL ((IMSIZE(1) - 1) / 2 + 1)
      CATR(KRCRP+1) = REAL ((IMSIZE(2) - 1) / 2 + 1)
      CATR(KRBLK) = 0.0
      CALL CHR2H (8, UNITS, 1, CATH(KHBUN))
C
      CATD(KDCRV) = 0.0D0
      CATD(KDCRV+1) = 0.0D0
C
 990  RETURN
      END
      SUBROUTINE IMDIMS (AXCODE, MAXSIZ, RINSIZ, LAGSIZ, OUTSIZ, IERR)
C-----------------------------------------------------------------------
C   Check on the dimensions of the output image size and set the
C   default output size if required
C   Input:
C      AXCODE     I        1 or 2 for x or y dimension
C      MAXSIZ     I        Maximum allowed dimension
C      RINSIZ     R        User supplied axis dimension
C      LAGSIZ     I        Maximum lag
C   Input/output:
C      OUTSIZ     I        Output image dimension for current axis
C      IERR       I        0 => OK
C-----------------------------------------------------------------------
      INTEGER    AXCODE, MAXSIZ, LAGSIZ, OUTSIZ, IERR
      REAL       RINSIZ
C
      INTEGER    INSIZ
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      INSIZ = RINSIZ + 0.01
      IF ((MOD(INSIZ,2).EQ.0) .AND. (INSIZ.NE.0)) THEN
         IF (AXCODE.EQ.1) THEN
            MSGTXT = 'IMDIMS: WARNING, OUTPUT IMAGE X-DIMENSION' //
     *         ' INCREASED'
            CALL MSGWRT (6)
         ELSE
            MSGTXT = 'IMDIMS: WARNING, OUTPUT IMAGE Y-DIMENSION' //
     *         ' INCREASED'
            CALL MSGWRT (6)
            END IF
         MSGTXT = 'IMDIMS: BY ONE TO FORCE AN ODD NUMBER'
         CALL MSGWRT (6)
         INSIZ = INSIZ + 1
         END IF
C
      IF (INSIZ.EQ.0) THEN
         OUTSIZ = LAGSIZ
      ELSE IF (INSIZ.GT.MAXSIZ) THEN
         IF (AXCODE.EQ.1) THEN
            WRITE (MSGTXT,2000) INSIZ, MAXSIZ
         ELSE
            WRITE (MSGTXT,3000) INSIZ, MAXSIZ
            END IF
         CALL MSGWRT (8)
         IERR = 1
      ELSE IF (INSIZ.LT.LAGSIZ) THEN
         MSGTXT = 'IMDIMS: OUTPUT IMSIZE AND LAG RANGE CONFLICT'
         CALL MSGWRT (6)
         IF (AXCODE.EQ.1) THEN
            WRITE (MSGTXT,5000) INSIZ, LAGSIZ
         ELSE
            WRITE (MSGTXT,6000) INSIZ, LAGSIZ
            END IF
         CALL MSGWRT (6)
         OUTSIZ = LAGSIZ
      ELSE
         OUTSIZ = INSIZ
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 2000 FORMAT ('IMDIMS: OUTPUT X DIMENSION =',I5,' > ALLOWED MAX =',I5)
 3000 FORMAT ('IMDIMS: OUTPUT Y DIMENSION =',I5,' > ALLOWED MAX =',I5)
 5000 FORMAT ('IMDIMS: INCREASING X IMAGE SIZE FROM',I5,' TO',I5)
 6000 FORMAT ('IMDIMS: INCREASING Y IMAGE SIZE FROM',I5,' TO',I5)
      END
      SUBROUTINE OUTPUT (RPARM, LUNIN, LUNOUT, BLC, TRC, IMSIZE, LAGS,
     *   INSLO, OUTSLO, IERR, IDIM1)
C-----------------------------------------------------------------------
C   This subroutine only sets up the dynamic memory allocation for
C   the lag function arrays
C   Input:
C      RPARM    R       AIPS inoput parameters
C      LUNIN    I       LUN for input image
C      LUNOUT   I       LUN for output image
C      BLC,TRC  I       Input window
C      IMSIZE   I       Output image size
C      LAGS     I       Lag window, xlo, xhi, ylo, yhi
C   Output:
C      INSLO    I       Catalog slot of input image
C      OUTSLO   I       Catalog slot of output image
C      IERR     I       0 => all OK
C-----------------------------------------------------------------------
      REAL      RPARM(*)
      INTEGER   LUNIN, LUNOUT(2), BLC(2), TRC(2), IMSIZE(2), LAGS(4),
     *   INSLO, OUTSLO(2), IERR, IDIM1
C
      INTEGER   NMEM, D1, D2, I
      REAL      RDATAI(2), RDATAO(2), RLINOU(2), RW(2), RNODAT(2),
     *   RDATRO(2)
      LONGINT   ODATAI, ODATAO, OLINOU, OW, ONODAT, ODATRO
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                               DATAIN
      D1 = IDIM1
      D2 = 2*D1
      NMEM = D1*D1
      NMEM = (NMEM - 1) / 1024 + 1
      CALL ZMEMRY ('GET', 'DUTPUT', NMEM, RDATAI, ODATAI, IERR)
      IF (IERR.NE.0) GO TO 90
C                               DATAOUT, rms out
      NMEM = (D1+1)*(2*D1+1)
      NMEM = (NMEM - 1) / 1024 + 1
      CALL ZMEMRY ('GET', 'DUTPUT', NMEM, RDATAO, ODATAO, IERR)
      IF (IERR.NE.0) GO TO 90
      CALL ZMEMRY ('GET', 'DUTPUT', NMEM, RDATRO, ODATRO, IERR)
      IF (IERR.NE.0) GO TO 90
C                               LINOUT
      NMEM = (2*D2+1)
      NMEM = (NMEM - 1) / 1024 + 1
      CALL ZMEMRY ('GET', 'DUTPUT', NMEM, RLINOU, OLINOU, IERR)
      IF (IERR.NE.0) GO TO 90
C                               W
      NMEM = D1*D1
      NMEM = (NMEM - 1) / 1024 + 1
      CALL ZMEMRY ('GET', 'DUTPUT', NMEM, RW, OW, IERR)
      IF (IERR.NE.0) GO TO 90
C                               NODATA
      NMEM = (D1+1)*(2*D1+1)
      NMEM = (NMEM - 1) / 1024 + 1
      CALL ZMEMRY ('GET', 'DUTPUT', NMEM, RNODAT, ONODAT, IERR)
      IF (IERR.NE.0) GO TO 90
C
      CALL DUTPUT (RPARM, LUNIN, LUNOUT, BLC, TRC, IMSIZE, LAGS, INSLO,
     *   OUTSLO, IERR, RDATAI(1+ODATAI), RDATAO(1+ODATAO),
     *   RDATRO(1+ODATRO), RLINOU(1+OLINOU), RW(1+OW), RNODAT(1+ONODAT),
     *   D1, D2)
      GO TO 100
C                                       zmemry fails
 90   MSGTXT = 'FAILED TO GET NEEDED DYNAMIC MEMORY'
      CALL MSGWRT (8)
C                                       give memory back
 100  CALL ZMEMRY ('FRAL', 'DUTPUT', NMEM, RW, OW, I)
C
 999  RETURN
      END
      SUBROUTINE DUTPUT (RPARM, LUNIN, LUNOUT, BLC, TRC, IMSIZE, LAGS,
     *   INSLO, OUTSLO, IERR, DATAIN, DATAOU, DATROU, LINOUT, W, NODATA,
     *   D1, D2)
C-----------------------------------------------------------------------
C   Compute the structure function image.   The lag domain image is
C   computed only for the right hand half of the plane.  Inversion
C   symmetry  is used to fill the rest when outputting the final map.
C
C   Note that the arrays W and NODATA are declared REAL rather than
C   INTEGER in order to speed up the computations in subroutine
C   SQDIFF.
C   This avoids the conversion from INTEGER to REAL which on the
C   Convex C-1 is a sap sucker.
C   Input:
C      RPARM    R       AIPS inoput parameters
C      LUNIN    I       LUN for input image
C      LUNOUT   I       LUN for output image
C      BLC,TRC  I       Input window
C      IMSIZE   I       Output image size
C      LAGS     I       Lag window, xlo, xhi, ylo, yhi
C   Output:
C      INSLO    I       Catalog slot of input image
C      OUTSLO   I       Catalog slot of output image
C      IERR     I       0 => all OK
C-----------------------------------------------------------------------
      INTEGER    LUNIN, LUNOUT(2), BLC(2), TRC(2), IMSIZE(2), LAGS(4),
     *   INSLO, OUTSLO(2), IERR, D1, D2
      REAL       RPARM(*), DATAIN(1:D1,1:D1), DATAOU(0:D1,-D1:D1),
     *   DATROU(0:D1,-D1:D1), LINOUT(-D2:D2), W(1:D1,1:D1),
     *   NODATA(0:D1,-D1:D1)
C
      REAL       DMIN(2), DMAX(2)
      INTEGER    NCOLIN, NROWIN
      LOGICAL    BLANK, BLKCEN, BLKSQU, BLKFRA
      INCLUDE 'INCS:DMSG.INC'
      DATA DMIN, DMAX, BLANK /1.0E20, 1.E20, -1.0, -1.0, .FALSE./
C-----------------------------------------------------------------------
C                                       Extract blanking parameters
      CALL EBLANK (RPARM, BLKCEN, BLKSQU, BLKFRA)
C                                       Get input window
      CALL WININ (BLC, TRC, NCOLIN, NROWIN)
C                                       Zero output arrays
      CALL INITAR (D1, BLKSQU, LAGS, DATAOU, DATROU, NODATA)
C                                       Read input image and set weights
      CALL READIN (D1, NCOLIN, NROWIN, LUNIN, DATAIN, W, INSLO, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'OUTPUT: ERROR READING INPUT IMAGE'
         GO TO 900
         END IF
C                                       Close input file
      CALL FILCLS (LUNIN)
      MSGTXT = 'OUTPUT: Input file has been read'
      CALL MSGWRT (2)
C                                       Calculate sum of squared
C                                       differences
      CALL SQDIFF (D1, NCOLIN, NROWIN, LAGS, DATAIN, W, DATAOU,
     *   DATROU, NODATA)
      MSGTXT = 'OUTPUT: Output values have been summed'
      CALL MSGWRT (2)
C                                       Calculate means, max, min.
      CALL MEANEX (D1, LAGS, NODATA, DATAOU, DATROU, DMIN, DMAX, BLANK)
C                                       Blank or zero central pixel
      CALL BLKZER (BLKCEN, DATAOU(0,0), DMIN(1), BLANK)
      CALL BLKZER (BLKCEN, DATROU(0,0), DMIN(2), BLANK)
C                                       Write output image
      MSGTXT = 'OUTPUT: write output images'
      CALL MSGWRT (2)
      CALL WRITOU (D1, D2, BLKFRA, LAGS, IMSIZE, LUNOUT, LINOUT, DATAOU,
     *   DATROU, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'OUTPUT: ERROR WRITING OUTPUT IMAGE'
         GO TO 800
         END IF
C                                       Update header min and max
      CALL CATUP (LUNOUT, BLANK, DMIN, DMAX, OUTSLO, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'OUTPUT: ERROR UPDATING CATALOGUE'
         GO TO 800
         END IF
C                                       Close output file
 800  CALL FILCLS (LUNOUT(1))
      CALL FILCLS (LUNOUT(2))
C
 900  IF (IERR.NE.0) CALL MSGWRT (8)
 999  RETURN
      END
      SUBROUTINE EBLANK (RPARM, BLKCEN, BLKSQU, BLKFRA)
C-----------------------------------------------------------------------
C   Work out blanking options
C   Input:
C      RPARM     R       AIPS parameters
C   Output:
C      BLKCEN    L       .true. => blank lag(0,0) pixel else zero it
C      BLKSQU    L       .true. => blank inner square in lag image
C                          if lower lag limits non-zero, else zero it
C      BLKFRA    L       .true. => blank outer lag frame if output
C                          image size greater than maximum lag limit,
C                          else zero it
C    RPARM PARAMETERS
C 36        APARM(5)             Blank lag=(0,0) pixel or zero it
C 37        APARM(6)             Blank or zero inner square
C 38        APARM(7)             Blank or zero outer frame when padding
C                                image
C-----------------------------------------------------------------------
      REAL      RPARM(*)
      LOGICAL   BLKCEN, BLKSQU, BLKFRA
C-----------------------------------------------------------------------
C                                       Central pixel
      BLKCEN = RPARM(36).GE.0.0
C                                       Inner square
      BLKSQU = RPARM(37).GT.0.0
C                                       Outer frame
      BLKFRA = RPARM(38).GT.0.0
C
 999  RETURN
      END
      SUBROUTINE WININ (BLC, TRC, NCOLIN, NROWIN)
C-----------------------------------------------------------------------
C      Work out input image window size
C      Input:
C         BLC,TRC  I       Input image window
C      Output:
C         NCOLIN   I       Number of columns in input image window
C         NROWIN   I       Number of rows in input image window
C
C-----------------------------------------------------------------------
      INTEGER   BLC(2), TRC(2), NCOLIN, NROWIN
C-----------------------------------------------------------------------
      NCOLIN = TRC(1) - BLC(1) + 1
      NROWIN = TRC(2) - BLC(2) + 1
C
 999  RETURN
      END
      SUBROUTINE INITAR (IDIM1, BLKSQU, LAGS, DATAOU, DATROU, NODATA)
C-----------------------------------------------------------------------
C     Initialize arrays with zeros or blanks where required.
C     First completely initialize the arrays with zeros.   If necessary
C     then overwrite the inner lag square with 'INDE'.   This is
C     a little CPU expensive, but easier to get the logic right.
C     Input:
C       IDIM1    I       dimension variable
C       BLKSQU   L*4     .true. => blank inner square in lag space, else
C                        zero it;  only if LAGS(1) and LAGS(3) > 0
C       LAGS     I       Lag array, xlo, xhi, ylo, yhi
C     Output:
C       DATAOU   R       Difference array
C       NODATA   R       NODATA(I,J) is the number of points that have
C                        occured with LAG=I,J
C-----------------------------------------------------------------------
      INTEGER   IDIM1
      REAL      DATAOU(0:IDIM1,-IDIM1:IDIM1),
     *   DATROU(0:IDIM1,-IDIM1:IDIM1), NODATA(0:IDIM1,-IDIM1:IDIM1)
      INTEGER LAGS(4), ILAG, JLAG, JST, JEND
      LOGICAL   BLKSQU
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Fill arrays with zero
      DO 20 JLAG = -LAGS(4), LAGS(4)
         DO 10 ILAG = 0, LAGS(2)
            NODATA(ILAG,JLAG) = 0.0
            DATAOU(ILAG,JLAG) = 0.0
            DATROU(ILAG,JLAG) = 0.0
 10         CONTINUE
 20      CONTINUE
C                                       Now overwrite inner square
      IF (BLKSQU .AND. LAGS(1).GT.0) THEN
         IF (LAGS(3).EQ.0) THEN
            JST = 0
            JEND = 0
         ELSE
            JST = -LAGS(3) + 1
            JEND = LAGS(3) - 1
            END IF
         DO 40 JLAG = JST, JEND
            DO 30 ILAG = 0, LAGS(1) - 1
               DATAOU(ILAG,JLAG) = FBLANK
               DATROU(ILAG,JLAG) = FBLANK
 30            CONTINUE
 40         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE READIN (IDIM1, NCOLIN, NROWIN, LUNIN, DATAIN, W,
     *   INSLOT, IERR)
C-----------------------------------------------------------------------
C     Read in the input image and set weights array to 0 (data blanked)
C     or 1. Get catalog slot too.
C     Input:
C       IDIM1    I     array dimension
C       NCOLIN   I     Number of columns in input image window
C       NROWIN   I     Number of rows in input image window
C       LUNIN    I     LUN for input image
C     Output:
C       DATAIN   R     Input image
C       W        R     Weights array, 0.0 => image pixel blanked, else
C                      1.0
C       INSLOT   I     Catalog slot of input image
C       IERR     I     0 => OK
C-----------------------------------------------------------------------
      INTEGER IDIM1
      REAL   DATAIN(1:IDIM1,1:IDIM1), W(1:IDIM1,1:IDIM1)
      INTEGER I, J, NROWIN, NCOLIN
      INTEGER   LUNIN, IERR, INSLOT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Loop over number of rows in
C                                       the input window
      DO 20 J = 1, NROWIN
         CALL MAPIO ('READ', LUNIN, DATAIN(1,J), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       Assign zero weight to blanked
C                                       pixels
         DO 10 I = 1, NCOLIN
            IF (DATAIN(I,J).NE.FBLANK) THEN
               W(I,J) = 1.0
            ELSE
               W(I,J) = 0.0
               END IF
 10      CONTINUE
 20   CONTINUE
C                                          Find cat slot of input image
      IERR = 0
      INCLUDE 'INCS:ZVD.INC'
      DO 30 I = 1,EFIL
         IF (FILTAB(POLUN,I).EQ.LUNIN) THEN
            INSLOT = FILTAB(POCAT, I)
            GO TO 999
         ELSE IF (I.EQ.EFIL) THEN
            MSGTXT = 'READIN: ERROR FINDING CAT SLOT OF INPUT IMAGE'
            CALL MSGWRT (8)
            IERR = 2
            GO TO 999
            END IF
 30      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT('READIN: COULD NOT READ INPUT IMAGE LINE # ', I4, ' IERR=',
     *       I3)
      END
      SUBROUTINE SQDIFF (IDIM1, NCOLIN, NROWIN, LAGS, DATAIN, W,
     *   DATAOU, DATROU, NODATA)
C-----------------------------------------------------------------------
C   Loop over all pixels, difference each pixel (i,j) with all others
C   unless its blanked.    The duplicated code is included in line to
C   avoid the overhead of subroutine calls because they would be
C   called up to three times for each pixel.
C   Input:
C      IDIM1       I       Array dimension
C      NCOLIN      I       Number of columns in input image window
C      NROWIN      I       Number of rows in input image window
C      LAGS        I       Lag array
C      DATAIN      R       Input image data
C      W           R       Weights array
C   Output:
C      DATAOU      R       Differences array
C      NODATA      R       NODATA(I,J) is the number of times that the
C                          LAG=I,J has occurred
C-----------------------------------------------------------------------
      INTEGER   IDIM1, NCOLIN, NROWIN, LAGS(4)
      REAL      DATAIN(1:IDIM1,1:IDIM1), DATAOU(0:IDIM1,-IDIM1:IDIM1),
     *   DATROU(0:IDIM1,-IDIM1:IDIM1), NODATA(0:IDIM1,-IDIM1:IDIM1),
     *   W(1:IDIM1,1:IDIM1)
C
      REAL       WPROD, WDIFF
      INTEGER    I, J, JST, JEN, IST, IEN, IIN, JIN, IOUT, JOUT, JLAGAD
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Loop over all image pixels,
C                                       skip blanks
      IF (LAGS(3).EQ.0) THEN
         JLAGAD = 1
      ELSE
         JLAGAD = 0
         END IF
      DO 80 J = 1, NROWIN
         IF (MOD (J-1,10).EQ.0) THEN
            WRITE (MSGTXT,1000) J
            CALL MSGWRT (2)
            END IF
         DO 70 I = 1, NCOLIN
            IF (DATAIN(I,J).NE.FBLANK) THEN
C                                       Now loop over lag space for
C                                       each pixel
C                                       -ylagmax -> -ylagmin
               JEN = J - LAGS(3)
               IF (JEN.GE.1) THEN
                  IST = I
                  IEN = MIN (I + LAGS(2), NCOLIN)
                  JST = J - LAGS(4)
                  IF (JEN.EQ.1) THEN
                     JST = 1
                  ELSE
                     JST = MAX (JST, 1)
                     END IF
                  DO 20 JIN = JST, JEN
                     JOUT = JIN - J
                     DO 10 IIN = IST, IEN
                        IOUT = IIN - I
                        WPROD = W(I,J) * W(IIN,JIN)
                        IF (WPROD.GT.0.0) THEN
                           WDIFF = WPROD * (DATAIN(I,J)-DATAIN(IIN,JIN))
                           WDIFF = WDIFF * WDIFF
                           NODATA(IOUT,JOUT) = NODATA(IOUT,JOUT) + WPROD
                           DATAOU(IOUT,JOUT) = DATAOU(IOUT,JOUT) + WDIFF
                           DATROU(IOUT,JOUT) = DATROU(IOUT,JOUT) +
     *                        (WDIFF * WDIFF)
                           END IF
 10                     CONTINUE
 20                  CONTINUE
                  END IF
C
C                                       -ylagmin -> ylagmin
               IST = I + LAGS(1)
               IF (IST.LE.NCOLIN .AND. LAGS(3).GT.0) THEN
                  IEN = MIN (I + LAGS(2), NCOLIN)
                  JST = J - LAGS(3)
                  JEN = J + LAGS(3) - 1
                  IF (JST.LT.1) THEN
                     JST = 1
                  ELSE
                     JST = JST + 1
                     END IF
                  JEN = MIN (JEN, NROWIN)
                  DO 40 JIN = JST, JEN
                     JOUT = JIN - J
                     DO 30 IIN = IST, IEN
                        IOUT = IIN - I
                        WPROD = W(I,J) * W(IIN,JIN)
                        IF (WPROD.GT.0.0) THEN
                           WDIFF = WPROD * (DATAIN(I,J)-DATAIN(IIN,JIN))
                           WDIFF = WDIFF * WDIFF
                           NODATA(IOUT,JOUT) = NODATA(IOUT,JOUT) + WPROD
                           DATAOU(IOUT,JOUT) = DATAOU(IOUT,JOUT) + WDIFF
                           DATROU(IOUT,JOUT) = DATROU(IOUT,JOUT) +
     *                        (WDIFF * WDIFF)
                           END IF
 30                     CONTINUE
 40                  CONTINUE
                  END IF
C                                       ylagmin -> ylagmax
               JST = J + LAGS(3) + JLAGAD
               IF (JST.LE.NROWIN) THEN
                  IST = I
                  IEN = MIN (I + LAGS(2), NCOLIN)
                  JEN = MIN (J + LAGS(4), NROWIN)
                  DO 60 JIN = JST, JEN
                     JOUT = JIN - J
                     DO 50 IIN = IST, IEN
                        IOUT = IIN - I
                        WPROD = W(I,J) * W(IIN,JIN)
                        IF (WPROD.GT.0.0) THEN
                           WDIFF = WPROD * (DATAIN(I,J)-DATAIN(IIN,JIN))
                           WDIFF = WDIFF * WDIFF
                           NODATA(IOUT,JOUT) = NODATA(IOUT,JOUT) + WPROD
                           DATAOU(IOUT,JOUT) = DATAOU(IOUT,JOUT) + WDIFF
                           DATROU(IOUT,JOUT) = DATROU(IOUT,JOUT) +
     *                        (WDIFF * WDIFF)
                           END IF
 50                     CONTINUE
 60                  CONTINUE
                  END IF
               END IF
 70         CONTINUE
 80      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SQDIFF: at row',I5)
      END
      SUBROUTINE MEANEX (IDIM1, LAGS, NODATA, DATAOU, DATROU, DMIN,
     *   DMAX, BLANK)
C-----------------------------------------------------------------------
C   Find mean of structure function at each lag and also find the data
C   extrema.   Note the compiler directives that inhibit vectorization.
C   Input:
C      IDIM1       I       Array dimension
C      LAGS        I       Lag array, xlo, xhi, ylo, yhi
C      NODATA      R       NODATA(I,J) is the number of times LAG=I,J
C                          has occured
C   Output:
C      DATAOU      R       Differences array
C      DMIN        R       Data minimum
C      DMAX        R       Data maximum
C      BLANK       L       .true. if blanks in output image
C-----------------------------------------------------------------------
      INTEGER   IDIM1, LAGS(4)
      LOGICAL   BLANK
      REAL      NODATA(0:IDIM1,-IDIM1:IDIM1),
     *   DATAOU(0:IDIM1,-IDIM1:IDIM1), DATROU(0:IDIM1,-IDIM1:IDIM1),
     *   DMIN(2), DMAX(2)
C
      INTEGER   ILAG, JLAG, JST, JEN
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       -ylaghi -> -ylaglo
      INCLUDE 'INCS:ZVD.INC'
      DO 20 JLAG = -LAGS(4), -LAGS(3)
      INCLUDE 'INCS:ZVD.INC'
         DO 10 ILAG = 0, LAGS(2)
            IF (NODATA(ILAG,JLAG).GT.0.0) THEN
               DATAOU(ILAG,JLAG) = DATAOU(ILAG,JLAG) / NODATA(ILAG,JLAG)
               DMIN(1) = MIN (DMIN(1), DATAOU(ILAG,JLAG))
               DMAX(1) = MAX (DMAX(1), DATAOU(ILAG,JLAG))
               DATROU(ILAG,JLAG) = DATROU(ILAG,JLAG) / NODATA(ILAG,JLAG)
               DATROU(ILAG,JLAG) = DATROU(ILAG,JLAG) -
     *            DATAOU(ILAG,JLAG) * DATROU(ILAG,JLAG)
               DATROU(ILAG,JLAG) = SQRT (MAX (0.0, DATROU(ILAG,JLAG)))
               DMIN(2) = MIN (DMIN(2), DATROU(ILAG,JLAG))
               DMAX(2) = MAX (DMAX(2), DATROU(ILAG,JLAG))
            ELSE
               DATAOU(ILAG,JLAG) = FBLANK
               DATROU(ILAG,JLAG) = FBLANK
               BLANK = .TRUE.
               END IF
 10         CONTINUE
 20      CONTINUE
C                                       -ylaglo -> ylaglo
      IF (LAGS(3).NE.0) THEN
         JST = 1 - LAGS(3)
         JEN = LAGS(3) - 1
      INCLUDE 'INCS:ZVD.INC'
         DO 40 JLAG = JST, JEN
      INCLUDE 'INCS:ZVD.INC'
            DO 30 ILAG = LAGS(1), LAGS(2)
               IF (NODATA(ILAG,JLAG).GT.0.0) THEN
                  DATAOU(ILAG,JLAG) = DATAOU(ILAG,JLAG) /
     *               NODATA(ILAG,JLAG)
                  DMIN(1) = MIN (DMIN(1), DATAOU(ILAG,JLAG))
                  DMAX(1) = MAX (DMAX(1), DATAOU(ILAG,JLAG))
                  DATROU(ILAG,JLAG) = DATROU(ILAG,JLAG) /
     *               NODATA(ILAG,JLAG)
                  DATROU(ILAG,JLAG) = DATROU(ILAG,JLAG) -
     *               DATAOU(ILAG,JLAG) * DATROU(ILAG,JLAG)
                  DATROU(ILAG,JLAG) = SQRT (MAX(0.0,DATROU(ILAG,JLAG)))
                  DMIN(2) = MIN (DMIN(2), DATROU(ILAG,JLAG))
                  DMAX(2) = MAX (DMAX(2), DATROU(ILAG,JLAG))
               ELSE
                  DATAOU(ILAG,JLAG) = FBLANK
                  DATROU(ILAG,JLAG) = FBLANK
                  BLANK = .TRUE.
                  END IF
 30            CONTINUE
 40         CONTINUE
         END IF
C                                       ylaglo -> ylaghi
      IF (LAGS(3).EQ.0) THEN
         JST = 1
      ELSE
         JST = LAGS(3)
      END IF
      INCLUDE 'INCS:ZVD.INC'
      DO 60 JLAG = JST, LAGS(4)
      INCLUDE 'INCS:ZVD.INC'
         DO 50 ILAG = 0, LAGS(2)
            IF (NODATA(ILAG,JLAG).GT.0.0) THEN
               DATAOU(ILAG,JLAG) = DATAOU(ILAG,JLAG) / NODATA(ILAG,JLAG)
               DMIN(1) = MIN (DMIN(1), DATAOU(ILAG,JLAG))
               DMAX(1) = MAX (DMAX(1), DATAOU(ILAG,JLAG))
               DATROU(ILAG,JLAG) = DATROU(ILAG,JLAG) / NODATA(ILAG,JLAG)
               DATROU(ILAG,JLAG) = DATROU(ILAG,JLAG) -
     *            DATAOU(ILAG,JLAG) * DATROU(ILAG,JLAG)
               DATROU(ILAG,JLAG) = SQRT (MAX (0.0, DATROU(ILAG,JLAG)))
               DMIN(2) = MIN (DMIN(2), DATROU(ILAG,JLAG))
               DMAX(2) = MAX (DMAX(2), DATROU(ILAG,JLAG))
            ELSE
               DATAOU(ILAG,JLAG) = FBLANK
               DATROU(ILAG,JLAG) = FBLANK
               BLANK = .TRUE.
               END IF
 50         CONTINUE
 60      CONTINUE
 999  RETURN
      END
      SUBROUTINE BLKZER (BLKCEN, CENTER, DMIN, BLANK)
C-----------------------------------------------------------------------
C   Blank or zero the central (lag=[0,0]) pixel
C   Input:
C      BLKCEN  L       .true. => blank it, else zero it
C   Output:
C      CENTER  R       lag 0,0 output pixel
C   Input/Output:
C      DMIN    R       output data minimum
C      BLANK   L       .true. if blanks in output image
C-----------------------------------------------------------------------
      REAL      CENTER, DMIN
      LOGICAL   BLANK, BLKCEN
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (BLKCEN) THEN
         CENTER = FBLANK
         BLANK = .TRUE.
      ELSE
         CENTER = 0.0
         DMIN = 0.0
         END IF
C
 999  RETURN
      END
      SUBROUTINE WRITOU (IDIM1, IDIM2, BLKFRA, LAGS, IMSIZE, LUNOUT,
     *   LINOUT, DATAOU, DATROU, IERR)
C-----------------------------------------------------------------------
C     Write the output image to disk
C     Input:
C       IDIM1       I       Array dimension
C       IDIM2       I       Array dimension
C       BLKFRA      L       .true. => pad output image with blanks,
C                           else zero
C       LAGS        I       Lag array
C       IMSIZE      I       Output image size
C       LUNOUT      I       LUN for output image
C       LINOUT      R       Array for i/o to disk
C       DATAOU      R       Differences array
C     Output:
C       IERR        I       0 => OK
C-----------------------------------------------------------------------
      INTEGER   IDIM1, IDIM2, LAGS(4), IMSIZE(2), LUNOUT(2), IERR
      REAL      DATAOU(0:IDIM1,-IDIM1:IDIM1), LINOUT(-IDIM2:IDIM2),
     *   DATROU(0:IDIM1,-IDIM1:IDIM1), PADVAL
      LOGICAL   BLKFRA
C
      INTEGER   ILAG, JLAG, IMAX, JMAX, ILINE, J
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (BLKFRA) THEN
         PADVAL = FBLANK
      ELSE
         PADVAL = 0.0
         END IF
      IMAX = (IMSIZE(1) - 1) / 2
      JMAX = (IMSIZE(2) - 1) / 2
C                                       Loop over y range
      DO 50 J = 1,2
         ILINE = 1
         DO 40 JLAG = -JMAX, JMAX
            IF (JLAG.LT.-LAGS(4) .OR. JLAG.GT.LAGS(4)) THEN
C                                       Pad image with blanks or zeros
               DO 10 ILAG = -IMAX, IMAX
                  LINOUT(ILAG) = PADVAL
 10               CONTINUE
            ELSE
C                                       Fill -x lag plane; inversion
C                                       symmetry
               IF (J.EQ.1) THEN
                  DO 20 ILAG = 1, LAGS(2)
                     LINOUT(ILAG)  = DATAOU(ILAG,JLAG)
                     LINOUT(-ILAG) = DATAOU(ILAG,-JLAG)
 20                  CONTINUE
                  LINOUT(0) = DATAOU(0,JLAG)
               ELSE
                  DO 25 ILAG = 1, LAGS(2)
                     LINOUT(ILAG)  = DATROU(ILAG,JLAG)
                     LINOUT(-ILAG) = DATROU(ILAG,-JLAG)
 25                  CONTINUE
                  LINOUT(0) = DATROU(0,JLAG)
                  END IF
C                                       Pad image with blanks or zeros
               IF (IMAX.GT.LAGS(2)) THEN
                  DO 30 ILAG = LAGS(2) + 1, IMAX
                     LINOUT(ILAG) = PADVAL
                     LINOUT(-ILAG) = PADVAL
 30                  CONTINUE
                 END IF
               END IF
C                                       Do the i/o
            CALL MAPIO ('WRIT', LUNOUT(J), LINOUT(-IMAX), IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) ILINE, IERR
               CALL MSGWRT (8)
               GO TO 990
               END IF
            ILINE = ILINE + 1
 40         CONTINUE
 50      CONTINUE
C
 990  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT('WRITOU: COULD NOT WRITE IMAGE LINE #',I6,' IERR=',I3)
      END
      SUBROUTINE CATUP (LUNOUT, BLANK, DMIN, DMAX, OUTSLO, IERR)
C-----------------------------------------------------------------------
C     Update catalog block and get cat slots
C     Input:
C       LUNOUT   I       Logical unit number of output image
C       BLANK    L       .true. if blanks in image
C       DMIN     R       Data minimum of output image
C       DMAX     R       Data maximum of output image
C    Output:
C       OUTSLO   I       Cat slot of output image
C       IERR     I       Error status, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   LUNOUT(2), OUTSLO(2), IERR
      LOGICAL   BLANK
      REAL      DMIN(2), DMAX(2)
C
      INCLUDE 'STFUN.INC'
      INTEGER   OUTVOL(2), IBUFF(256), I, J, CATMP(256)
      REAL      CATRT(256)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATMP, CATRT)
C-----------------------------------------------------------------------
C                                       Find cat slot & vol of output
C                                       image
      INCLUDE 'INCS:ZVD.INC'
      CALL COPY (256, CATBLK, CATMP)
      DO 30 J = 1,2
         DO 10 I = 1,EFIL
            IF (FILTAB(POLUN,I).EQ.LUNOUT(J)) THEN
               OUTSLO(J) = FILTAB(POCAT, I)
               OUTVOL(J) = FILTAB(POVOL, I)
               GO TO 20
            ELSE IF (I.EQ.EFIL) THEN
               WRITE (MSGTXT,1000)
               CALL MSGWRT (8)
               IERR = 1
               GO TO 990
               END IF
 10         CONTINUE
C                                       Update disk header file
 20      CATRT(KRDMN) = DMIN(J)
         CATRT(KRDMX) = DMAX(J)
         IF (BLANK) CATRT(KRBLK) = FBLANK
         CALL CATIO ('UPDT', OUTVOL(J), OUTSLO(J), CATMP, 'REST',
     *      IBUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) IERR
            CALL MSGWRT (8)
            END IF
         CALL COPY (256, CATBL2, CATMP)
 30      CONTINUE
C
 990  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CATUP: ERROR FINDING CAT SLOT OF OUTPUT IMAGE')
 2000 FORMAT ('CATUP: ERROR UPDATING CATALOGUE BLOCK, IERR=', I3)
      END
      SUBROUTINE STFUNH (INNAM, OUTNAM, INSLO, OUTSLO, BLC, TRC,
     *   IMSIZE, LAGS)
C-----------------------------------------------------------------------
C     Write the history file.
C    Inputs:
C        INNAM          C*36  input image namestring
C        OUTNAM         C*36  output image namestring
C        INSLO          I     Slot number for input image
C        OUTSLO         I     Slot number for output image
C        BLC,TRC        I     Input image window
C        IMSIZE         I     Output image size
C        LAGS           I     Lag window
C-----------------------------------------------------------------------
      CHARACTER INNAM*36, OUTNAM(2)*36
      INTEGER   INSLO, OUTSLO(2), BLC(2), TRC(2), IMSIZE(2), LAGS(4)
C
      CHARACTER HILINE*72, INAME*12, ICLASS*6, IPTYPE*2,
     *   ONAME*12, OCLASS*6, OPTYPE*2
      INTEGER   IBUFF1(256), IBUFF2(256), CATBLK(256), IERR, NHISTF,
     *   LUNHIN, LUNHOU, ISEQ, IDISK, IUSID, OSEQ, ODISK, OUSID, J
      REAL      CATR(256)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATR, CATBLK)
      COMMON /MAPHDR/ CATBLK
      DATA LUNHIN, LUNHOU /27, 28/
C-----------------------------------------------------------------------
C                                       Init HI
      NHISTF = 2
      CALL HIINIT (NHISTF)
      CALL WAWA2A (INNAM, INAME, ICLASS, ISEQ, IPTYPE, IDISK, IUSID)
C                                       two outputs
      DO 100 J = 1,2
C                                       Work out some disks and
C                                       sequence nos etc.
         CALL WAWA2A (OUTNAM(J), ONAME, OCLASS, OSEQ, OPTYPE, ODISK,
     *      OUSID)
C                                       Get cat block of output image
         CALL CATIO ('READ', ODISK, OUTSLO(J), CATBLK, 'REST', IBUFF1,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, J
            CALL MSGWRT (8)
            GO TO 100
            END IF
C                                       copy keywords
         CALL KEYPCP (IDISK, INSLO, ODISK, OUTSLO, 0, ' ', IERR)
C                                       Copy old history file to new
         CALL HISCOP (LUNHIN, LUNHOU, IDISK, ODISK, INSLO, OUTSLO(J),
     *      CATBLK, IBUFF1, IBUFF2, IERR)
         IF (IERR.GE.3) GO TO 50
C                                       Add input image name
         CALL HENCO1 (TSKNAM, INAME, ICLASS, ISEQ, IDISK, LUNHOU,
     *      IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
C                                       Add output image name
         CALL HENCOO (TSKNAM, ONAME, OCLASS, OSEQ, ODISK,
     *      LUNHOU, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
C                                       Add input window
         WRITE (HILINE,5000) TSKNAM, BLC(1), BLC(2), TRC(1), TRC(2)
         CALL HIADD (LUNHOU, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
C                                       Add output image size
         WRITE (HILINE,7000) TSKNAM, IMSIZE(1), IMSIZE(2)
         CALL HIADD (LUNHOU, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
C                                       Add lag window
         WRITE (HILINE,9000) TSKNAM, LAGS(1), LAGS(2), LAGS(3), LAGS(4)
         CALL HIADD (LUNHOU, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
         GO TO 90
C                                       HI error
 50      WRITE (MSGTXT,1050) IERR, J
         CALL MSGWRT (6)
C                                       Close history
 90      CALL HICLOS (LUNHOU, .TRUE., IBUFF2, IERR)
 100     CONTINUE
C
      IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('STFUNH: COULD NOT GET CATALOGUE BLOCK, IERR,#=',I3,I2)
 1050 FORMAT ('STFUNH: IERR=',I3,' HISTORY FILE FOR #',I2)
 5000 FORMAT (A6,'BLC = ',I4,',',I4,'   TRC = ',I4,',',I4)
 7000 FORMAT (A6,'/ Output image size = ',I4,' BY ',I4,' pixels')
 9000 FORMAT (A6,'/ Xlag = ',I3,' - ',I3,'  Ylag = ',I3,' - ',I3)
      END
