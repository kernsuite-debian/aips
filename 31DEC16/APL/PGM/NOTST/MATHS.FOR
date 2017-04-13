      PROGRAM MATHS
C-----------------------------------------------------------------------
C! Performs mathematical functions on an image
C# Map-util math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 1999-2000, 2008-2010, 2015
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
C   MATHS operates on your image with one of a variety of simple
C   mathematical functions.
C RPARM    INPUTS:
C  1        USERID               Owner of the image
C  2-4      INNAME(3)            Image name (name)
C  5-6      INCLASS(2)           Image name (class)
C  7        INSEQ                Image name (seq. #)
C  8        INDISK               Disk # of image
C  9-11     OUTNAME(3)           Output image name (name)
C  12-13    OUTCLASS(2)          Output image name (class)
C  14       OUTSEQ               Output image name (seq. #)
C  15       OUTDISK              Disk # of output image
C  16-22    BLC(7)               BLC of input image
C  23-29    TRC(7)               TRC of input image
C  30       OPCODE               Operator, SIN, COS, TAN, ASIN, ACOS,
C                                ATAN, LOG, ALOG, LOGN, EXP, POLY, POWR,
C                                MOD, DIVP
C  31-40    CPARM(10)            User supplied parameters, 1-6 used
C     Neil Killeen, April 1987
C-----------------------------------------------------------------------
      INTEGER   NOP
      PARAMETER (NOP = 15)
C
      CHARACTER INNAM*36, OUTNAM*36, OPCODE*4, LIST(NOP)*4
      REAL      RPARM(40), CPARM(10), BLKVAL
      INTEGER   NROWS, NCOLS, BLC(7), TRC(7), LIN, LOUT, NPARMS, IERR,
     *   IER, INSL, OUTSL, INVOL, OUTVOL
      DATA LIN, LOUT, NPARMS /17, 18, 39/
      DATA LIST /'SIN ', 'COS ', 'TAN ', 'ASIN', 'ACOS', 'ATAN',
     *           'LOG ', 'LOGN', 'ALOG', 'EXP ', 'POLY', 'POWR',
     *           'MOD ', 'ABS ', 'DIVP'/
C-----------------------------------------------------------------------
C                                       Start up task & get inputs
      CALL START (NPARMS, RPARM, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Extract some user parameters
      CALL EXTRAC (RPARM, NOP, LIST, CPARM, OPCODE, BLKVAL, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Open input & output images
      CALL OPENIM (OPCODE, RPARM, LIN, LOUT, INNAM, OUTNAM, INSL,
     *             OUTSL, INVOL, OUTVOL, BLC, TRC, NCOLS, NROWS, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Compute output image
      CALL OUTPUT (LIST, OPCODE, CPARM, LIN, LOUT, NROWS, NCOLS,
     *             BLKVAL, OUTSL, OUTVOL, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Add history file
      CALL MATHHI (INNAM, OUTNAM, INSL, OUTSL, INVOL, OUTVOL,
     *             BLC, TRC, OPCODE, CPARM, LIST, IERR)
C
 900  IF (IERR.EQ.0) THEN
         IER = 0
      ELSE
         IER = 1
         END IF
      CALL CLENUP
      CALL TSKEND (IER)
C
      STOP
      END
      SUBROUTINE START (NPARMS, RPARM, IERR)
C-----------------------------------------------------------------------
C     Start up the task and get AIPS inputs
C     INPUT:
C       NPARMS   I       Number of AIPS parameters
C     OUTPUT:
C       RPARM    R       AIPS input parameters
C       IERR     I       0 => OK
C-----------------------------------------------------------------------
      REAL   RPARM(*)
      INTEGER   NPARMS, IERR
      CHARACTER PRGNAM*6
      INCLUDE 'INCS:DMSG.INC'
      DATA PRGNAM /'MATHS '/
C-----------------------------------------------------------------------
      CALL TSKBEG (PRGNAM, NPARMS, RPARM(2), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
      ELSE
         WRITE (MSGTXT,2000)
         END IF
      CALL MSGWRT (8)
      RPARM(1) = NLUSER
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('START: COULD NOT GET INPUTS, IERR=', I3)
 2000 FORMAT ('You are using a non-standard program')
      END
      SUBROUTINE EXTRAC (RPARM, NOP, LIST, CPARM, OPCODE, BLKVAL, IERR)
C-----------------------------------------------------------------------
C     Extract some of the user supplied parameters from the INPUTS
C     INPUT:
C        RPARM    R      AIPS inputs
C        NOP      I      Number of allowed operators
C        LIST     C(*)*4 List of allowed operators
C     OUTPUT:
C        CPARM    R      Scale and additive factors for image
C        OPCODE   C*4    Operator; SIN, COS, TAN, ASIN, ACOS, ATAN,
C                                  LOG, LOGN, ALOG, EXP, POLY, POWR,
C                                  MOD
C        BLKVAL   R      Value to use for undefined output pixels
C                        'INDE' or 0.0
C        IERR     I      0 => OK
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, LIST(*)*4
      REAL      CPARM(*), RPARM(*), BLKVAL
      INTEGER   I, NOP
      INTEGER   IERR
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Check if OPCODE acceptable
      CALL H2CHR (4, 1, RPARM(30), OPCODE)
      IERR = 0
      DO 100 I = 1,NOP
         IF (OPCODE.EQ.LIST(I)) THEN
            GO TO 200
            END IF
 100     CONTINUE
C                                       No match found
      WRITE (MSGTXT,1000) OPCODE
      CALL MSGWRT (8)
      IERR = 1
      GO TO 999
C                                       Fill CPARMS and set defaults
 200  CPARM(1) = RPARM(31)
      CPARM(2) = RPARM(32)
      IF ((RPARM(32).EQ.0.0) .AND. (OPCODE.NE.'POLY') .AND.
     *   (OPCODE.NE.'DIVP')) CPARM(2) = 1.0
      CPARM(3) = RPARM(33)
      IF ((RPARM(33).EQ.0.0) .AND. (OPCODE.NE.'POLY') .AND.
     *   (OPCODE.NE.'DIVP')) CPARM(3) = 1.0
      CPARM(4) = RPARM(34)
      CPARM(5) = RPARM(35)
C                                       Tell user what's going on
      WRITE (MSGTXT,2000)
      CALL MSGWRT (8)
      IF (OPCODE.EQ.'MOD') THEN
         WRITE (MSGTXT,3000) CPARM(1), CPARM(2), CPARM(3),
     *      CPARM(4), CPARM(5)
      ELSE IF (OPCODE.EQ.'POWR') THEN
         WRITE (MSGTXT,4000) CPARM(1), CPARM(2), CPARM(3),
     *      CPARM(4), CPARM(5)
      ELSE IF (OPCODE.EQ.'POLY') THEN
         WRITE (MSGTXT,5000) CPARM(1), CPARM(2), CPARM(3), CPARM(4)
      ELSE IF (OPCODE.EQ.'DIVP') THEN
         WRITE (MSGTXT,5010) CPARM(1), CPARM(2), CPARM(3), CPARM(4)
      ELSE
         WRITE (MSGTXT,6000) CPARM(1), CPARM(2), OPCODE, CPARM(3),
     *      CPARM(4)
         END IF
      CALL MSGWRT (8)
C                                       Blank with magic values or
C                                       zeroes
      CPARM(6) = RPARM(36)
      IF (CPARM(6).GT.0.0) THEN
         BLKVAL = 0.0
         WRITE (MSGTXT,7000)
      ELSE
         BLKVAL = FBLANK
         WRITE (MSGTXT,8000)
         END IF
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OPCODE ''', A4, ''' IS UNRECOGNIZED')
 2000 FORMAT ('Output pixel formed from input pixel according to:')
 3000 FORMAT (1PE10.3, '+', 1PE10.3, '* MOD[', 1PE10.3, '*in+',
     *        1PE10.3, ',', 1PE10.3, ']')
 4000 FORMAT (1PE10.3, '+', 1PE10.3, '*[', 1PE10.3, '*in+',
     *        1PE10.3, ']**', 1PE10.3)
 5000 FORMAT (1PE10.3, '+', 1PE10.3, '*in + ', 1PE10.3, '*in**2 + ',
     *        1PE10.3, '*in**3')
 5010 FORMAT (1PE10.3, '+', 1PE10.3, '/in + ', 1PE10.3, '/in**2 + ',
     *        1PE10.3, '/in**3')
 6000 FORMAT (1PE10.3, '+', 1PE10.3, '*', A4, '[', 1PE10.3, '*in+',
     *        1PE10.3, ']')
 7000 FORMAT ('EXTRAC: All undefined output pixels zeroed')
 8000 FORMAT ('EXTRAC: All undefined output pixels magic blanked')
      END
      SUBROUTINE OPENIM (OPCODE, RPARM, LIN, LOUT, INNAM, OUTNAM,
     *   INSL, OUTSL, INVOL, OUTVOL, BLC, TRC, NCOLS, NROWS, IERR)
C-----------------------------------------------------------------------
C     Open the input image, create and open the output image
C     Input:
C         OPCODE    C*4     Operator, determines default class of
C                           output image
C         RPARM     R       array of AIPS input parameters
C         LIN       I       logical unit number for input image
C         LOUT      I       logical unit number for output image
C     Output:
C         INNAM     C*36    Namestring for input image
C         OUTNAM    C*36    Namestring for output image
C         INSL      I       Cat slot of input image
C         OUTSL     I       Cat slot of output image
C         INVOL     I       Volume number of input image
C         OUTVOL    I       Volume number of output image
C         BLC       I       Input image BLC, defaults filled in
C         TRC       I       Input image TRC, defaults filled in
C         NCOLS     I       Number of columns in output image
C         NROWS     I       Number of rows in output image
C         IERR      I       Error status, 0=> OK
C-----------------------------------------------------------------------
      CHARACTER INNAM*36, OUTNAM*36, OPCODE*4
      REAL      RPARM(*)
      INTEGER   NROWS, NCOLS, BLC(7), TRC(7)
      INTEGER   LIN, LOUT, IERR, INSL, OUTSL, OUTVOL, INVOL
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Open input image
      CALL OPENIN (RPARM, LIN, INNAM, BLC, TRC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 900
         END IF
C                                       Get catalog slot
      CALL CATSLV (LIN, INSL, INVOL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,2000) IERR
         GO TO 900
         END IF
C                                       Create and open output image
      CALL OPENOU (OPCODE, RPARM, LOUT, BLC, TRC, OUTNAM, NCOLS, NROWS,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,3000) IERR
         GO TO 900
         END IF
C                                       Get catalog slot
      CALL CATSLV (LOUT, OUTSL, OUTVOL, IERR)
      IF (IERR.NE.0) WRITE (MSGTXT,4000) IERR
C
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OPENIM: COULD NOT OPEN INPUT IMAGE, IERR=', I3)
 2000 FORMAT ('OPENIM: COULD NOT FIND INPUT IMAGE CAT SLOT, IERR=', I3)
 3000 FORMAT ('OPENIM: COULD NOT OPEN OUTPUT IMAGE, IERR=', I3)
 4000 FORMAT ('OPENIM: COULD NOT FIND OUTPUT IMAGE CAT SLOT, IERR=', I3)
      END
      SUBROUTINE OPENIN (RPARM, LIN, INNAM, BLC, TRC, IERR)
C-----------------------------------------------------------------------
C     Set up the input namestring, open the input image, get the header
C     and window the input image
C     Input:
C         RPARM     R       array of AIPS input parameters
C         LIN       I       LUN of input image
C     Output:
C         INNAM     C*36    Namestring for input image
C         BLC,TRC   I       Window with defaults filled in
C         IERR      I       error status, 0=> OK
C-----------------------------------------------------------------------
      CHARACTER INNAM*36
      HOLLERITH MAP
      REAL   OLDR(256), RPARM(*), RBLC(7), RTRC(7)
      INTEGER   BLC(7), TRC(7)
      INTEGER   LIN, OLDI(256), IERR, DUM(256)
      INCLUDE 'INCS:DMSG.INC'
      COMMON /HEADS/ OLDI, DUM
      EQUIVALENCE (OLDI, OLDR)
C-----------------------------------------------------------------------
C                                       Set up input namestring
      CALL CHR2H (4, 'MA  ', 1, MAP)
      CALL H2WAWA (RPARM(2), RPARM(5), RPARM(7), MAP, RPARM(8),
     *   RPARM(1), INNAM)
      CALL H2WAWA (RPARM(2), RPARM(5), RPARM(7), MAP, RPARM(8),
     *   RPARM(1), INNAM)
C                                       Open input image
      CALL OPENCF (LIN, INNAM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 900
         END IF
C                                       Get header
      CALL GETHDR (LIN, OLDI, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,2000) IERR
         GO TO 900
         END IF
C                                       Set up image window
      CALL RCOPY (7, RPARM(16), RBLC)
      CALL RCOPY (7, RPARM(23), RTRC)
      CALL CHKWIN (RBLC, RTRC, OLDI, BLC, TRC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,3000) IERR
         GO TO 900
         END IF
      CALL MAPWIN (LIN, RBLC, RTRC, IERR)
      IF (IERR.NE.0) WRITE (MSGTXT,4000) IERR
C
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OPENIN: ERROR OPENING INPUT IMAGE, IERR=', I3)
 2000 FORMAT ('OPENIN: ERROR GETTING INPUT IMAGE HEADER, IERR=', I3)
 3000 FORMAT ('OPENIN: ILLEGAL WINDOW, IERR = ', I3)
 4000 FORMAT ('OPENIM: ERROR WINDOWING INPUT IMAGE, IERR = ', I3)
      END
      SUBROUTINE CHKWIN (BLC, TRC, HEADI, IBLC, ITRC, IERR)
C-----------------------------------------------------------------------
C     Check that a 2-D window is sensible, and set zero defaults to
C     (1,1) and (NCOLS,NROWS)  Both R   and I   versions of the
C     window, with defaults filled in, are returned.
C     Input/output:
C       BLC      R       BLC of window
C       TRC      R       TRC of window
C     Input:
C       HEADI    I       Header of associated image
C     Output:
C       IBLC     I       BLC of (corrected) window
C       ITRC     I       TRC of (corrected) window
C       IERR     I       0 => OK
C-----------------------------------------------------------------------
      REAL   BLC(7), TRC(7)
      INTEGER   IBLC(7), ITRC(7)
      INTEGER   HEADI(256), IERR, I
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Extract size and window
      CALL WINDOW (HEADI(KIDIM), HEADI(KINAX), BLC, TRC, IERR)
      IF (IERR.EQ.0) THEN
         DO 10 I = 1,7
           IBLC(I) = BLC(I) + 0.01
           ITRC(I) = TRC(I) + 0.01
 10        CONTINUE
         END IF
C
      RETURN
      END
      SUBROUTINE CATSLV (LUN, SLOT, VOL, IERR)
C-----------------------------------------------------------------------
C     Get catalog slot and volume number for open image file specified
C     by its LUN
C     Input:
C       LUN      I       Logical unit number of OPEN image
C    Output:
C       SLOT     I       Catalog slot of image
C       VOL      I       Volume number
C       IERR     I       Error status, 0 => OK
C
C-----------------------------------------------------------------------
      INTEGER   LUN, SLOT, VOL, IERR, I
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DITB.INC'
C-----------------------------------------------------------------------
C                                       Match LUN in FILTAB
      DO 100 I = 1,EFIL
         IF (FILTAB(POLUN,I).EQ.LUN) THEN
            SLOT = FILTAB(POCAT,I)
            VOL  = FILTAB(POVOL,I)
            IERR = 0
            GO TO 999
            END IF
 100     CONTINUE
C                                       Didn't find LUN in FILTAB
      WRITE (MSGTXT,1000) LUN
      CALL MSGWRT (8)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CATSLV: CAN''T FIND LUN = ', I3, ' IN FILTAB')
      END
      SUBROUTINE OPENOU (OPCODE, RPARM, LOUT, BLC, TRC, OUTNAM, NCOLS,
     *   NROWS, IERR)
C-----------------------------------------------------------------------
C     Create and open output image
C     Inputs:
C         OPCODE    C*4     Operator , determines default class
C         RPARM     R       array of AIPS input parameters
C         LOUT      I       LUN for output image
C         BLC,TRC   I       Input image window
C     OUTPUTS:
C         OUTNAM    C*36    Namestring for output image
C         NCOLS     I       Number of columns in output image
C         NROWS     I       Number of rows in output image
C         IERR      I       error status, 0=> OK
C
C-----------------------------------------------------------------------
      CHARACTER DEFNAM*36, OUTNAM*36, OPCODE*4
      REAL   RPARM(*)
      HOLLERITH MAP, XCLAS(2)
      INTEGER   BLC(7), TRC(7), NCOLS, NROWS
      INTEGER   LOUT, IERR, OLDI(256), CATBLK(256)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      COMMON /HEADS/ OLDI, CATBLK
C-----------------------------------------------------------------------
C                                       Output name string
      CALL CHR2H (4, 'MA  ', 1, MAP)
      CALL CHR2H (6, OPCODE, 1, XCLAS)
      CALL H2WAWA (RPARM(2), XCLAS, RPARM(7), MAP, RPARM(8),
     *   RPARM(1), DEFNAM)
      CALL H2WAWA (RPARM(9), RPARM(12), RPARM(14), MAP, RPARM(15),
     *   RPARM(1), OUTNAM)
C                                       Create output map header
      CALL HEADER (BLC, TRC, NCOLS, NROWS)
C                                       Create output image
      CALL MAPCR (DEFNAM, OUTNAM, CATBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 900
         END IF
C                                       Open output image
      CALL OPENCF (LOUT, OUTNAM, IERR)
      IF (IERR.NE.0) WRITE (MSGTXT,2000) IERR
C
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OPENOU: COULD NOT CREATE OUTPUT IMAGE, IERR=', I3)
 2000 FORMAT ('OPENOU: COULD NOT OPEN OUTPUT IMAGE, IERR=',  I3)
      END
      SUBROUTINE HEADER (BLC, TRC, NCOLS, NROWS)
C-----------------------------------------------------------------------
C     Make the output image header
C     Input:
C         BLC       I       BLC of input image
C         TRC       I       TRC of input image
C     Ouput:
C         NCOLS     I       Number of columns in output image
C         NROWS     I       Number of rows in output image
C-----------------------------------------------------------------------
      DOUBLE PRECISION OLDD(128), CATD(128)
      REAL   CATR(256)
      CHARACTER UNITS*8
      HOLLERITH CATH(256)
      INTEGER   BLC(7), TRC(7), NCOLS, NROWS, I
      INTEGER   OLDI(256), CATBLK(256), CATEMP(256)
      REAL      RBLC(7), RTRC(7)
      INCLUDE 'INCS:DHDR.INC'
      COMMON /HEADS/ OLDI, CATBLK
      COMMON /MAPHDR/ CATEMP
      EQUIVALENCE (OLDI, OLDD), (CATBLK, CATR, CATD, CATH)
      DATA UNITS /'UNDEFINE'/
C-----------------------------------------------------------------------
C                                              Make output image header
      CALL COPY (256, OLDI, CATEMP)
      NCOLS = TRC(1) - BLC(1) + 1
      NROWS = 1
      DO 10 I = 1,7
         RBLC(I) = BLC(I)
         RTRC(I) = TRC(I)
         NROWS = NROWS * (TRC(I) - BLC(I) + 1)
 10      CONTINUE
      NROWS = NROWS / NCOLS
      CALL SUBHDR (RBLC, RTRC, 1.0, 1.0)
      CALL COPY (256, CATEMP, CATBLK)
C
      CALL CHR2H (8, UNITS, 1, CATH(KHBUN))
      CATR(KRBLK) = 0.0
C
 999  RETURN
      END
      SUBROUTINE OUTPUT (LIST, OPCODE, CPARM, LIN, LOUT, NROWS,
     *   NCOLS, BLKVAL, OUTSL, OUTVOL, IERR)
C-----------------------------------------------------------------------
C   Main subroutine to operate on image with the desired scalpel
C   Input:
C       LIST(*)    C*4   List of allowed operators
C       OPCODE     C*4   Operator code
C       CPARM      R     Multiplicative and additive factors for input
C                        image
C       LIN,LOUT   I     LUNS for input and output files
C       NROWS      I     Number of rows in output image
C       NCOLS      I     Number of columns in output image
C       BLKVAL     R     Value assigned to undefined pixels, 'INDE' or
C                        0.0
C       OUTSL      I     Output image cat slot
C       OUTVOL     I     Output image volume number
C     OUTPUT:
C       IERR       I     0 => OK
C-----------------------------------------------------------------------
      CHARACTER LIST(*)*4, OPCODE*4
      INCLUDE 'INCS:PMAD.INC'
      REAL   CPARM(*), RBUF(MAXIMG), CATR(256), DMIN, DMAX, BLKVAL
      INTEGER   NROWS, NCOLS
      INTEGER   LIN, LOUT, IERR, OUTSL, OUTVOL, DUM2(256),
     *   CATBLK(256), IBUFF(256)
      LOGICAL BLANKS
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /HEADS/ DUM2, CATBLK
      EQUIVALENCE (CATBLK, CATR)
      DATA DMIN, DMAX, BLANKS /1.0E34, -1.0E34, .FALSE./
C-----------------------------------------------------------------------
C                                       Branch for each operator
      IF (OPCODE.EQ.'SIN') THEN
         CALL SINE (NROWS, NCOLS, LIN, LOUT, CPARM, BLKVAL,
     *      RBUF, DMIN, DMAX, BLANKS, IERR)
      ELSE IF (OPCODE.EQ.'COS') THEN
         CALL COSINE (NROWS, NCOLS, LIN, LOUT, CPARM, BLKVAL,
     *      RBUF, DMIN, DMAX, BLANKS, IERR)
      ELSE IF (OPCODE.EQ.'TAN') THEN
         CALL TANGEN (NROWS, NCOLS, LIN, LOUT, CPARM, BLKVAL,
     *      RBUF, DMIN, DMAX, BLANKS, IERR)
      ELSE IF (OPCODE.EQ.'ASIN') THEN
         CALL ASINE (NROWS, NCOLS, LIN, LOUT, CPARM, BLKVAL,
     *      RBUF, DMIN, DMAX, BLANKS, IERR)
      ELSE IF (OPCODE.EQ.'ACOS') THEN
         CALL ACOSIN (NROWS, NCOLS, LIN, LOUT, CPARM, BLKVAL,
     *      RBUF, DMIN, DMAX, BLANKS, IERR)
      ELSE IF (OPCODE.EQ.'ATAN') THEN
         CALL ATANGE (NROWS, NCOLS, LIN, LOUT, CPARM, BLKVAL,
     *      RBUF, DMIN, DMAX, BLANKS, IERR)
      ELSE IF (OPCODE.EQ.'LOG') THEN
         CALL LOGARI (NROWS, NCOLS, LIN, LOUT, CPARM, BLKVAL,
     *      RBUF, DMIN, DMAX, BLANKS, IERR)
      ELSE IF (OPCODE.EQ.'LOGN') THEN
         CALL NLOGAR (NROWS, NCOLS, LIN, LOUT, CPARM, BLKVAL,
     *      RBUF, DMIN, DMAX, BLANKS, IERR)
      ELSE IF (OPCODE.EQ.'ALOG') THEN
         CALL ALOGAR (NROWS, NCOLS, LIN, LOUT, CPARM, BLKVAL,
     *      RBUF, DMIN, DMAX, BLANKS, IERR)
      ELSE IF (OPCODE.EQ.'EXP') THEN
         CALL EXPONE (NROWS, NCOLS, LIN, LOUT, CPARM, BLKVAL,
     *      RBUF, DMIN, DMAX, BLANKS, IERR)
      ELSE IF (OPCODE.EQ.'POLY') THEN
         CALL POLYNO (NROWS, NCOLS, LIN, LOUT, CPARM, BLKVAL,
     *      RBUF, DMIN, DMAX, BLANKS, IERR)
      ELSE IF (OPCODE.EQ.'POWR') THEN
         CALL POWER (NROWS, NCOLS, LIN, LOUT, CPARM, BLKVAL,
     *      RBUF, DMIN, DMAX, BLANKS, IERR)
      ELSE IF (OPCODE.EQ.'MOD') THEN
         CALL MODUL (NROWS, NCOLS, LIN, LOUT, CPARM, BLKVAL,
     *      RBUF, DMIN, DMAX, BLANKS, IERR)
      ELSE IF (OPCODE.EQ.'ABS') THEN
         CALL ABSOL (NROWS, NCOLS, LIN, LOUT, CPARM, BLKVAL,
     *      RBUF, DMIN, DMAX, BLANKS, IERR)
      ELSE IF (OPCODE.EQ.'DIVP') THEN
         CALL DIVPOL (NROWS, NCOLS, LIN, LOUT, CPARM, BLKVAL,
     *      RBUF, DMIN, DMAX, BLANKS, IERR)
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 900
         END IF
C                                       Update header
      IF (BLANKS .AND. BLKVAL.NE.FBLANK) THEN
         DMIN = MIN (BLKVAL, DMIN)
         DMAX = MAX (BLKVAL, DMAX)
         END IF
      CATR(KRDMN) = DMIN
      CATR(KRDMX) = DMAX
C
      IF (BLANKS .AND. BLKVAL.EQ.FBLANK) CATR(KRBLK) = FBLANK
      CALL CATIO ('UPDT', OUTVOL, OUTSL, CATBLK, 'REST', IBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,2000) IERR
         GO TO 900
      END IF
C                                       Close image files
      CALL FILCLS (LIN)
      CALL FILCLS (LOUT)
C
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OUTPUT: ERROR COMPUTING OUTPUT IMAGE, IERR = ', I3)
 2000 FORMAT ('OUTPUT: COULD NOT UPATE HEADER, IERR=', I3)
      END
      SUBROUTINE SINE (NROWS, NCOLS, LIN, LOUT, C, BLKVAL, RBUF,
     *   DMIN, DMAX, BLANKS, IERR)
C-----------------------------------------------------------------------
C   Take SINE of image in DEGREES
C      OUT = C(1) + C(2) * SIN (C(3) * IN + C(4))
C   Input:
C      NROWS       I            Number of rows in output image
C      NCOLS       I            Number of columns in output image
C      LIN         I            LUN of input image
C      LOUT        I            LUN of output image
C      C           R            Scale and offset factors
C      BLKVAL      R            Value for undefined pixels
C   Input/output:
C      RBUF        R            Data buffer
C      DMIN,DMAX   R            Min and max in image
C      BLANKS      L            .true. if MAGIC blank in output image
C      IERR        I            0 => OK
C-----------------------------------------------------------------------
      REAL      RBUF(*), DMIN, DMAX,  C(*), FAC, DR, BLKVAL
      INTEGER   NCOLS, NROWS, I, J
      INTEGER   LIN, LOUT, IERR
      LOGICAL   BLANKS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA DR /1.7453293E-2/
C-----------------------------------------------------------------------
      DO 200 J = 1, NROWS
         CALL MAPIO ('READ', LIN, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         DO 100 I = 1, NCOLS
            IF (RBUF(I).EQ.FBLANK) THEN
               BLANKS = .TRUE.
               RBUF(I) = BLKVAL
            ELSE
               FAC = DR * (C(3) * RBUF(I) + C(4))
               RBUF(I) = C(1) + C(2) * SIN (FAC)
               DMIN = MIN (DMIN, RBUF(I))
               DMAX = MAX (DMAX, RBUF(I))
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SINE: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('SINE: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
      END
      SUBROUTINE COSINE (NROWS, NCOLS, LIN, LOUT, C, BLKVAL,
     *   RBUF, DMIN, DMAX, BLANKS, IERR)
C-----------------------------------------------------------------------
C     Take COSINE of image in DEGREES.
C         OUT = C(1) + C(2) * COS (C(3) * IN + C(4))
C     Input:
C       NROWS       I            Number of rows in output image
C       NCOLS       I            Number of columns in output image
C       LIN         I            LUN of input image
C       LOUT        I            LUN of output image
C       C           R            Scale and offset factors
C       BLKVAL      R            Value for undefined pixels
C     Input/output:
C       RBUF        R            Data buffer
C       DMIN,DMAX   R            Min and max in image
C       BLANKS      L            .true. if MAGIC blank in output image
C       IERR        I            0 => OK
C-----------------------------------------------------------------------
      REAL   RBUF(*), DMIN, DMAX,  C(*), FAC, DR, BLKVAL
      INTEGER   NCOLS, NROWS, I, J
      INTEGER   LIN, LOUT, IERR
      LOGICAL BLANKS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA DR /1.7453293E-2/
C-----------------------------------------------------------------------
      DO 200 J = 1, NROWS
         CALL MAPIO ('READ', LIN, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         DO 100 I = 1, NCOLS
            IF (RBUF(I).EQ.FBLANK) THEN
               BLANKS = .TRUE.
               RBUF(I) = BLKVAL
            ELSE
               FAC = DR * (C(3) * RBUF(I) + C(4))
               RBUF(I) = C(1) + C(2) * COS (FAC)
               DMIN = MIN (DMIN, RBUF(I))
               DMAX = MAX (DMAX, RBUF(I))
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('COSINE: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('COSINE: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
      END
      SUBROUTINE TANGEN (NROWS, NCOLS, LIN, LOUT, C, BLKVAL,
     *   RBUF, DMIN, DMAX, BLANKS, IERR)
C-----------------------------------------------------------------------
C     Take TANGENT of image in DEGREES
C         OUT = C(1) + C(2) * TAN (C(3) * IN + C(4))
C     Input:
C       NROWS       I            Number of rows in output image
C       NCOLS       I            Number of columns in output image
C       LIN         I            LUN of input image
C       LOUT        I            LUN of output image
C       C           R            Scale and offset factors
C       BLKVAL      R            Value for undefined pixels
C     Input/output:
C       RBUF        R            Data buffer
C       DMIN,DMAX   R            Min and max in image
C       BLANKS      L            .true. if MAGIC blank in output image
C       IERR        I            0 => OK
C-----------------------------------------------------------------------
      REAL   RBUF(*), DMIN, DMAX,  C(*), FAC, REM, DR, BLKVAL
      INTEGER   NCOLS, NROWS, I, J
      INTEGER   LIN, LOUT, IERR
      LOGICAL BLANKS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA DR /1.7453293E-2/
C-----------------------------------------------------------------------
      DO 200 J = 1, NROWS
         CALL MAPIO ('READ', LIN, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         DO 100 I = 1, NCOLS
            IF (RBUF(I).EQ.FBLANK) THEN
               BLANKS = .TRUE.
               RBUF(I) = BLKVAL
            ELSE
               FAC = C(3) * RBUF(I) + C(4)
               REM = ABS (MOD (FAC, 360.0))
               IF (REM.EQ.90.0 .OR. REM.EQ.270.0) THEN
                  RBUF(I) = BLKVAL
                  BLANKS = .TRUE.
               ELSE
                  RBUF(I) = C(1)  +  C(2) * TAN (DR * FAC)
                  DMIN = MIN (DMIN, RBUF(I))
                  DMAX = MAX (DMAX, RBUF(I))
                  END IF
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TANGEN: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('TANGEN: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
      END
      SUBROUTINE ASINE (NROWS, NCOLS, LIN, LOUT, C, BLKVAL,
     *   RBUF, DMIN, DMAX, BLANKS, IERR)
C-----------------------------------------------------------------------
C     Take ARCSINE of image, THETA returned in the range
C                       -90 < THETA < 90  degrees
C         OUT = C(1) + C(2) * ASIN (C(3) * IN + C(4))
C     Input:
C       NROWS       I            Number of rows in output image
C       NCOLS       I            Number of columns in output image
C       LIN         I            LUN of input image
C       LOUT        I            LUN of output image
C       C           R            Scale and offset factors
C       BLKVAL      R            Value for undefined pixels
C     Input/output:
C       RBUF        R            Data buffer
C       DMIN,DMAX   R            Min and max in image
C       BLANKS      L            .true. if MAGIC blank in output image
C       IERR        I            0 => OK
C-----------------------------------------------------------------------
      REAL   RBUF(*), DMIN, DMAX,  C(*), FAC, RD, BLKVAL
      INTEGER   NCOLS, NROWS, I, J
      INTEGER   LIN, LOUT, IERR
      LOGICAL BLANKS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA RD /5.72957795E1/
C-----------------------------------------------------------------------
      DO 200 J = 1, NROWS
         CALL MAPIO ('READ', LIN, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         DO 100 I = 1, NCOLS
            IF (RBUF(I).EQ.FBLANK) THEN
               BLANKS = .TRUE.
               RBUF(I) = BLKVAL
            ELSE
               FAC = C(3) * RBUF(I) + C(4)
               IF (ABS(FAC).GT.1.0) THEN
                  RBUF(I) = BLKVAL
                  BLANKS = .TRUE.
               ELSE
                  RBUF(I) = C(1) + C(2) * RD * ASIN (FAC)
                  DMIN = MIN (DMIN, RBUF(I))
                  DMAX = MAX (DMAX, RBUF(I))
                  END IF
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ASINE: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('ASINE: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
      END
      SUBROUTINE ACOSIN (NROWS, NCOLS, LIN, LOUT, C, BLKVAL,
     *   RBUF, DMIN, DMAX, BLANKS, IERR)
C-----------------------------------------------------------------------
C     Take ARCCOSINE of image, THETA returned in the range
C                         0 < THETA < 180 degrees
C         OUT = C(1) + C(2) * ACOS (C(3) * IN + C(4))
C     Input:
C       NROWS       I            Number of rows in output image
C       NCOLS       I            Number of columns in output image
C       LIN         I            LUN of input image
C       LOUT        I            LUN of output image
C       C           R            Scale and offset factors
C       BLKVAL      R            Value for undefined pixels
C     Input/output:
C       RBUF        R            Data buffer
C       DMIN,DMAX   R            Min and max in image
C       BLANKS      L            .true. if MAGIC blank in output image
C       IERR        I            0 => OK
C-----------------------------------------------------------------------
      REAL   RBUF(*), DMIN, DMAX,  C(*), FAC, RD, BLKVAL
      INTEGER   NCOLS, NROWS, I, J
      INTEGER   LIN, LOUT, IERR
      LOGICAL BLANKS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA RD /5.72957795E1/
C-----------------------------------------------------------------------
      DO 200 J = 1, NROWS
         CALL MAPIO ('READ', LIN, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         DO 100 I = 1, NCOLS
            IF (RBUF(I).EQ.FBLANK) THEN
               BLANKS = .TRUE.
               RBUF(I) = BLKVAL
            ELSE
               FAC = C(3) * RBUF(I) + C(4)
               IF (ABS(FAC).GT.1.0) THEN
                  RBUF(I) = BLKVAL
                  BLANKS = .TRUE.
               ELSE
                  RBUF(I) = C(1)  +  C(2) * RD * ACOS (FAC)
                  DMIN = MIN (DMIN, RBUF(I))
                  DMAX = MAX (DMAX, RBUF(I))
                  END IF
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ACOSIN: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('ACOSIN: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
      END
      SUBROUTINE ATANGE (NROWS, NCOLS, LIN, LOUT, C, BLKVAL,
     *   RBUF, DMIN, DMAX, BLANKS, IERR)
C-----------------------------------------------------------------------
C     Take ARCTANGENT of image, THETA returned in the range
C                       -90 < THETA < 90 degrees
C         OUT = C(1) + C(2) * ATAN (C(3) * IN + C(4))
C     Input:
C       NROWS       I            Number of rows in output image
C       NCOLS       I            Number of columns in output image
C       LIN         I            LUN of input image
C       LOUT        I            LUN of output image
C       C           R            Scale and offset factors
C       BLKVAL      R            Value for undefined pixels
C     Input/output:
C       RBUF        R            Data buffer
C       DMIN,DMAX   R            Min and max in image
C       BLANKS      L            .true. if MAGIC blank in output image
C       IERR        I            0 => OK
C-----------------------------------------------------------------------
      REAL   RBUF(*), DMIN, DMAX,  C(*), RD, BLKVAL
      INTEGER   NCOLS, NROWS, I, J
      INTEGER   LIN, LOUT, IERR
      LOGICAL BLANKS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA RD /5.72957795E1/
C-----------------------------------------------------------------------
      DO 200 J = 1, NROWS
         CALL MAPIO ('READ', LIN, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         DO 100 I = 1, NCOLS
            IF (RBUF(I).EQ.FBLANK) THEN
               BLANKS = .TRUE.
               RBUF(I) = BLKVAL
            ELSE
               RBUF(I) = C(1) + C(2) * RD * ATAN (C(3) * RBUF(I) + C(4))
               DMIN = MIN (DMIN, RBUF(I))
               DMAX = MAX (DMAX, RBUF(I))
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ATANGE: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('ATANGE: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
      END
      SUBROUTINE LOGARI (NROWS, NCOLS, LIN, LOUT, C, BLKVAL,
     *   RBUF, DMIN, DMAX, BLANKS, IERR)
C-----------------------------------------------------------------------
C     Take LOGARITHM (base 10) of image
C         OUT = C(1) + C(2) * LOG10 (C(3) * IN + C(4))
C     Input:
C       NROWS       I            Number of rows in output image
C       NCOLS       I            Number of columns in output image
C       LIN         I            LUN of input image
C       LOUT        I            LUN of output image
C       C           R            Scale and offset factors
C       BLKVAL      R            Value for undefined pixels
C     Input/output:
C       RBUF        R            Data buffer
C       DMIN,DMAX   R            Min and max in image
C       BLANKS      L            .true. if MAGIC blank in output image
C       IERR        I            0 => OK
C-----------------------------------------------------------------------
      REAL   RBUF(*), DMIN, DMAX,  C(*), FAC, BLKVAL
      INTEGER   NCOLS, NROWS, I, J
      INTEGER   LIN, LOUT, IERR
      LOGICAL BLANKS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      DO 200 J = 1, NROWS
         CALL MAPIO ('READ', LIN, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         DO 100 I = 1, NCOLS
            IF (RBUF(I).EQ.FBLANK) THEN
               BLANKS = .TRUE.
               RBUF(I) = BLKVAL
            ELSE
               FAC = C(3) * RBUF(I) + C(4)
               IF (FAC.GT.0.0) THEN
                  RBUF(I) = C(1) + C(2) * LOG10 (FAC)
                  DMIN = MIN (DMIN, RBUF(I))
                  DMAX = MAX (DMAX, RBUF(I))
               ELSE
                  RBUF(I) = BLKVAL
                  END IF
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('LOGARI: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('LOGARI: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
      END
      SUBROUTINE NLOGAR (NROWS, NCOLS, LIN, LOUT, C, BLKVAL,
     *   RBUF, DMIN, DMAX, BLANKS, IERR)
C-----------------------------------------------------------------------
C     Take LOGARITHM (natural-base e) of image
C         OUT = C(1) + C(2) * LOG (C(3) * IN + C(4))
C     Input:
C       NROWS       I            Number of rows in output image
C       NCOLS       I            Number of columns in output image
C       LIN         I            LUN of input image
C       LOUT        I            LUN of output image
C       C           R            Scale and offset factors
C       BLKVAL      R            Value for undefined pixels
C     Input/output:
C       RBUF        R            Data buffer
C       DMIN,DMAX   R            Min and max in image
C       BLANKS      L            .true. if MAGIC blank in output image
C       IERR        I            0 => OK
C-----------------------------------------------------------------------
      REAL   RBUF(*), DMIN, DMAX,  C(*), FAC, BLKVAL
      INTEGER   NCOLS, NROWS, I, J
      INTEGER   LIN, LOUT, IERR
      LOGICAL BLANKS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      DO 200 J = 1, NROWS
         CALL MAPIO ('READ', LIN, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         DO 100 I = 1, NCOLS
            IF (RBUF(I).EQ.FBLANK) THEN
               BLANKS = .TRUE.
               RBUF(I) = BLKVAL
            ELSE
               FAC = C(3) * RBUF(I) + C(4)
               IF (FAC.GT.0.0) THEN
                  RBUF(I) = C(1) + C(2) * LOG(FAC)
                  DMIN = MIN (DMIN, RBUF(I))
                  DMAX = MAX (DMAX, RBUF(I))
               ELSE
                  RBUF(I) = BLKVAL
                  END IF
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NLOGAR: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('NLOGAR: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
      END
      SUBROUTINE ALOGAR (NROWS, NCOLS, LIN, LOUT, C, BLKVAL,
     *   RBUF, DMIN, DMAX, BLANKS, IERR)
C-----------------------------------------------------------------------
C     Take ANTI-LOGARITHM (base 10) of image
C         OUT = C(1) + C(2) * EXP(C(3) * IN + C(4))
C     Input:
C       NROWS       I            Number of rows in output image
C       NCOLS       I            Number of columns in output image
C       LIN         I            LUN of input image
C       LOUT        I            LUN of output image
C       C           R            Scale and offset factors
C       BLKVAL      R            Value for undefined pixels
C     Input/output:
C       RBUF        R            Data buffer
C       DMIN,DMAX   R            Min and max in image
C       BLANKS      L            .true. if MAGIC blank in output image
C       IERR        I            0 => OK
C-----------------------------------------------------------------------
      REAL   RBUF(*), DMIN, DMAX,  C(*), BLKVAL
      INTEGER   NCOLS, NROWS, I, J
      INTEGER   LIN, LOUT, IERR
      LOGICAL BLANKS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      DO 200 J = 1, NROWS
         CALL MAPIO ('READ', LIN, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         DO 100 I = 1, NCOLS
            IF (RBUF(I).EQ.FBLANK) THEN
               BLANKS = .TRUE.
               RBUF(I) = BLKVAL
            ELSE
               RBUF(I) = C(1) + C(2) * 10**(C(3) * RBUF(I) + C(4))
               DMIN = MIN (DMIN, RBUF(I))
               DMAX = MAX (DMAX, RBUF(I))
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ALOGAR: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('ALOGAR: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
      END
      SUBROUTINE EXPONE (NROWS, NCOLS, LIN, LOUT, C, BLKVAL,
     *   RBUF, DMIN, DMAX, BLANKS, IERR)
C-----------------------------------------------------------------------
C     Take ANTI-LOGARITHM (natural-base e) of image
C        OUT = C(1) + C(2) * EXP(C(3) * IN + C(4))
C     Input:
C       NROWS       I            Number of rows in output image
C       NCOLS       I            Number of columns in output image
C       LIN         I            LUN of input image
C       LOUT        I            LUN of output image
C       C           R            Scale and offset factors
C       BLKVAL      R            Value for undefined pixels
C     Input/output:
C       RBUF        R            Data buffer
C       DMIN,DMAX   R            Min and max in image
C       BLANKS      L            .true. if MAGIC blank in output image
C       IERR        I            0 => OK
C-----------------------------------------------------------------------
      REAL   RBUF(*), DMIN, DMAX,  C(*), BLKVAL
      INTEGER   NCOLS, NROWS, I, J
      INTEGER   LIN, LOUT, IERR
      LOGICAL BLANKS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      DO 200 J = 1, NROWS
         CALL MAPIO ('READ', LIN, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         DO 100 I = 1, NCOLS
            IF (RBUF(I).EQ.FBLANK) THEN
               BLANKS = .TRUE.
               RBUF(I) = BLKVAL
            ELSE
               RBUF(I) = C(1) + C(2) * EXP(C(3) * RBUF(I) + C(4))
               DMIN = MIN (DMIN, RBUF(I))
               DMAX = MAX (DMAX, RBUF(I))
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('EXPONE: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('EXPONE: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
      END
      SUBROUTINE POLYNO (NROWS, NCOLS, LIN, LOUT, C, BLKVAL,
     *   RBUF, DMIN, DMAX, BLANKS, IERR)
C-----------------------------------------------------------------------
C     Form third order polynomial from image
C        OUT = C(1) + C(2)*IN + C(3)*IN**2 + C(4)*IN**3
C     Input:
C       NROWS       I            Number of rows in output image
C       NCOLS       I            Number of columns in output image
C       LIN         I            LUN of input image
C       LOUT        I            LUN of output image
C       C           R            Scale and offset factors
C       BLKVAL      R            Value for undefined pixels
C     Input/output:
C       RBUF        R            Data buffer
C       DMIN,DMAX   R            Min and max in image
C       BLANKS      L            .true. if MAGIC blank in output image
C       IERR        I            0 => OK
C-----------------------------------------------------------------------
      REAL   RBUF(*), DMIN, DMAX,  C(*), D, DSQ, DCUB, BLKVAL
      INTEGER   NCOLS, NROWS, I, J
      INTEGER   LIN, LOUT, IERR
      LOGICAL BLANKS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      DO 200 J = 1, NROWS
         CALL MAPIO ('READ', LIN, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         DO 100 I = 1, NCOLS
            IF (RBUF(I).EQ.FBLANK) THEN
               BLANKS = .TRUE.
               RBUF(I) = BLKVAL
            ELSE
               D = RBUF(I)
               DSQ = RBUF(I) * RBUF(I)
               DCUB = DSQ * D
               RBUF(I) = C(1) + C(2)*D + C(3)*DSQ + C(4)*DCUB
               DMIN = MIN (DMIN, RBUF(I))
               DMAX = MAX (DMAX, RBUF(I))
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('POLYNO: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('POLYNO: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
      END
      SUBROUTINE DIVPOL (NROWS, NCOLS, LIN, LOUT, C, BLKVAL,
     *   RBUF, DMIN, DMAX, BLANKS, IERR)
C-----------------------------------------------------------------------
C     Form third order polynomial from inverse of image
C        OUT = C(1) + C(2)/IN + C(3)/IN**2 + C(4)/IN**3
C     Input:
C       NROWS       I            Number of rows in output image
C       NCOLS       I            Number of columns in output image
C       LIN         I            LUN of input image
C       LOUT        I            LUN of output image
C       C           R            Scale and offset factors
C       BLKVAL      R            Value for undefined pixels
C     Input/output:
C       RBUF        R            Data buffer
C       DMIN,DMAX   R            Min and max in image
C       BLANKS      L            .true. if MAGIC blank in output image
C       IERR        I            0 => OK
C-----------------------------------------------------------------------
      REAL   RBUF(*), DMIN, DMAX,  C(*), D, DSQ, DCUB, BLKVAL
      INTEGER   NCOLS, NROWS, I, J
      INTEGER   LIN, LOUT, IERR
      LOGICAL BLANKS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      DO 200 J = 1, NROWS
         CALL MAPIO ('READ', LIN, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         DO 100 I = 1, NCOLS
            IF ((RBUF(I).EQ.FBLANK) .OR. (RBUF(I).EQ.0.0)) THEN
               BLANKS = .TRUE.
               RBUF(I) = BLKVAL
            ELSE
               D = RBUF(I)
               DSQ = RBUF(I) * RBUF(I)
               DCUB = DSQ * D
               RBUF(I) = C(1) + C(2)/D
               IF (C(3).NE.0.0) RBUF(I) = RBUF(I) + C(3)/DSQ
               IF (C(4).NE.0.0) RBUF(I) = RBUF(I) + C(4)/DCUB
               DMIN = MIN (DMIN, RBUF(I))
               DMAX = MAX (DMAX, RBUF(I))
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('POLYNO: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('POLYNO: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
      END
      SUBROUTINE POWER (NROWS, NCOLS, LIN, LOUT, C, BLKVAL,
     *   RBUF, DMIN, DMAX, BLANKS, IERR)
C-----------------------------------------------------------------------
C     Raise image to specified power
C        OUT = C(1) + C(2)*[C(3) * IN + C(4)]**C(5)
C     Note: if c(5) = 2.0 exactly, and the function in square brackets
C           is negative, then a special case is trapped and the
C           usual undefined exponentiation is avoided. square
C
C     Input:
C       NROWS       I            Number of rows in output image
C       NCOLS       I            Number of columns in output image
C       LIN         I            LUN of input image
C       LOUT        I            LUN of output image
C       C           R            Scale and offset factors
C       BLKVAL      R            Value for undefined pixels
C     Input/output:
C       RBUF        R            Data buffer
C       DMIN,DMAX   R            Min and max in image
C       BLANKS      L            .true. if MAGIC blank in output image
C       IERR        I            0 => OK
C-----------------------------------------------------------------------
      REAL   RBUF(*), DMIN, DMAX,  C(*), FAC, BLKVAL
      INTEGER   NCOLS, NROWS, I, J, I5, IROUND
      INTEGER   LIN, LOUT, IERR
      LOGICAL   BLANKS, POWOK
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      I5 = IROUND (C(5))
      POWOK = ABS(I5-C(5)).LT.0.0001
      DO 200 J = 1, NROWS
         CALL MAPIO ('READ', LIN, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         DO 100 I = 1, NCOLS
            IF (RBUF(I).EQ.FBLANK) THEN
               BLANKS = .TRUE.
               RBUF(I) = BLKVAL
            ELSE
               FAC = C(3) * RBUF(I) + C(4)
               IF ((POWOK) .AND. (FAC.NE.0.0)) THEN
                  RBUF(I) = C(1) + C(2) * FAC**I5
                  DMIN = MIN (DMIN, RBUF(I))
                  DMAX = MAX (DMAX, RBUF(I))
               ELSE IF ((FAC.LT.0.0) .OR. (FAC.EQ.0.0.AND.C(5).EQ.0.0))
     *            THEN
                  RBUF(I) = BLKVAL
                  BLANKS = .TRUE.
               ELSE
                  RBUF(I) = C(1) + C(2) * FAC**C(5)
                  DMIN = MIN (DMIN, RBUF(I))
                  DMAX = MAX (DMAX, RBUF(I))
                  END IF
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('POWER: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('POWER: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
      END
      SUBROUTINE MODUL (NROWS, NCOLS, LIN, LOUT, C, BLKVAL,
     *   RBUF, DMIN, DMAX, BLANKS, IERR)
C-----------------------------------------------------------------------
C     Find remainder of image with MOD function
C        OUT = C(1) + C(2) * MOD[C(3) * IN + C(4), C(5)]
C     Input:
C       NROWS       I            Number of rows in output image
C       NCOLS       I            Number of columns in output image
C       LIN         I            LUN of input image
C       LOUT        I            LUN of output image
C       C           R            Scale and offset factors
C       BLKVAL      R            Value for undefined pixels
C     Input/output:
C       RBUF        R            Data buffer
C       DMIN,DMAX   R            Min and max in image
C       BLANKS      L            .true. if MAGIC blank in output image
C       IERR        I            0 => OK
C-----------------------------------------------------------------------
      REAL   RBUF(*), DMIN, DMAX,  C(*), FAC, BLKVAL
      INTEGER   NCOLS, NROWS, I, J
      INTEGER   LIN, LOUT, IERR
      LOGICAL BLANKS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      DO 200 J = 1, NROWS
         CALL MAPIO ('READ', LIN, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         DO 100 I = 1, NCOLS
            IF (RBUF(I).EQ.FBLANK) THEN
               BLANKS = .TRUE.
               RBUF(I) = BLKVAL
            ELSE
               FAC = C(3) * RBUF(I) + C(4)
               IF (C(5).NE.0.0) THEN
                  RBUF(I) = C(1) + C(2) * MOD (FAC, C(5))
                  DMIN = MIN (DMIN, RBUF(I))
                  DMAX = MAX (DMAX, RBUF(I))
               ELSE
                  RBUF(I) = BLKVAL
                  BLANKS = .TRUE.
                  END IF
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MODUL: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('MODUL: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
      END
      SUBROUTINE ABSOL (NROWS, NCOLS, LIN, LOUT, C, BLKVAL,
     *   RBUF, DMIN, DMAX, BLANKS, IERR)
C-----------------------------------------------------------------------
C     Find remainder of image with ABS function
C        OUT = C(1) + C(2) * ABS[C(3) * IN + C(4)]
C     Input:
C       NROWS       I            Number of rows in output image
C       NCOLS       I            Number of columns in output image
C       LIN         I            LUN of input image
C       LOUT        I            LUN of output image
C       C           R            Scale and offset factors
C       BLKVAL      R            Value for undefined pixels
C     Input/output:
C       RBUF        R            Data buffer
C       DMIN,DMAX   R            Min and max in image
C       BLANKS      L            .true. if MAGIC blank in output image
C       IERR        I            0 => OK
C-----------------------------------------------------------------------
      REAL   RBUF(*), DMIN, DMAX,  C(*), FAC, BLKVAL
      INTEGER   NCOLS, NROWS, I, J
      INTEGER   LIN, LOUT, IERR
      LOGICAL BLANKS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       default is simple OUT = ABS[IN]
      IF (ABS(C(1)+C(2)+C(3)+C(4)) .LE. 0.0001) THEN
         C(1) = 0
         C(2) = 1
         C(3) = 1
         C(4) = 0
         END IF
      DO 200 J = 1, NROWS
         CALL MAPIO ('READ', LIN, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         DO 100 I = 1, NCOLS
            IF (RBUF(I).EQ.FBLANK) THEN
               BLANKS = .TRUE.
               RBUF(I) = BLKVAL
            ELSE
               FAC = C(3) * RBUF(I) + C(4)
               RBUF(I) = C(1) + C(2) * ABS (FAC)
               DMIN = MIN (DMIN, RBUF(I))
               DMAX = MAX (DMAX, RBUF(I))
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) J, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ABSOL: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('ABSOL: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
      END
      SUBROUTINE MATHHI (INNAM, OUTNAM, INSL, OUTSL, INVOL, OUTVOL,
     *   BLC, TRC, OPCODE, CPARM, LIST, IERR)
C-----------------------------------------------------------------------
C    MATHHI writes the history file for the task MATHS
C
C    Inputs:
C        INNAM            C*36 Input map namestring
C        OUTNAM           C*36  Output map namestring
C        INSL             I     Slot number for input map
C        OUTSL            I     Slot number for output map
C        INVOL            I     Input image volume number
C        OUTVOL           I     Output image volume number
C        BLC,TRC          I     Input image window
C        OPCODE           C*4   MATHS operator
C        CPARM            R     Scale factors
C        LIST(*)          C*4   Allowed OPCODEs
C   Outputs:
C        IERR             I     Error return
C                               0-> okay
C                               1-> no good
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, INNAM*36, OUTNAM*36, HILINE*72, OPCODE*4,
     *   LIST(*)*4, NAME*12, CLASS*6, TYPE*2, NOTTYP*2
      REAL   CPARM(*)
      INTEGER   BLC(7), TRC(7)
      INTEGER   IERR, NHISTF, LHIN, LHOUT, INSL, OUTSL, INVOL,
     *   OUTVOL, INSEQ, OUTSEQ, IBUFF1(256), IBUFF2(256), USID,
     *   OLDI(256), CATBLK(256)
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      COMMON /HEADS/ OLDI, CATBLK
      DATA NHISTF, LHIN, LHOUT /2, 27, 28/
      DATA PRGNAM /'MATHS '/
      DATA T /.TRUE./
      DATA NOTTYP /'CC'/
C-----------------------------------------------------------------------
C                                       copy most keywords
      CALL KEYPCP (INVOL, INSL, OUTVOL, OUTSL, 0, ' ', IERR)
C                                       Init HI
      CALL HIINIT (NHISTF)
C                                       Copy old to new
      CALL HISCOP (LHIN, LHOUT, INVOL, OUTVOL, INSL, OUTSL, CATBLK,
     *    IBUFF1, IBUFF2, IERR)
      IF (IERR.GE.3) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 900
      ELSE
         IERR = 0
         END IF
C                                       Add INNAME
      CALL WAWA2A (INNAM, NAME, CLASS, INSEQ, TYPE, INVOL, USID)
      CALL HENCO1 (PRGNAM, NAME, CLASS, INSEQ, INVOL, LHOUT,
     *    IBUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,2000) IERR
         GO TO 900
         END IF
C                                       Add OUTNAME
      CALL WAWA2A (OUTNAM, NAME, CLASS, OUTSEQ, TYPE, OUTVOL, USID)
      CALL HENCOO (PRGNAM, NAME, CLASS, OUTSEQ, OUTVOL, LHOUT,
     *    IBUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,3000) IERR
         GO TO 900
         END IF
C                                       Add window
      WRITE (HILINE,4000) PRGNAM, BLC(1), BLC(2), TRC(1), TRC(2)
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,5000) IERR
         GO TO 900
         END IF
C                                       Add OPCODE
      WRITE (HILINE,6000) PRGNAM, OPCODE
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,7000) IERR
         GO TO 900
         END IF
C                                       Add CPARM's
      IF (OPCODE.EQ.'POWR' .OR. OPCODE.EQ.'MOD') THEN
         WRITE (HILINE,8000) PRGNAM, CPARM(1), CPARM(2),
     *                           CPARM(3), CPARM(4), CPARM(5)
      ELSE
         WRITE (HILINE,8500) PRGNAM, CPARM(1), CPARM(2),
     *                           CPARM(3), CPARM(4)
         END IF
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) THEN
        WRITE (MSGTXT,9000) IERR
        GO TO 900
      END IF
C
      IF (CPARM(6).GT.0.0) THEN
         WRITE (HILINE,9200) PRGNAM, CPARM(6)
      ELSE
         WRITE (HILINE,9300) PRGNAM, CPARM(6)
         END IF
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,9400) IERR
         GO TO 900
         END IF
C                                       Close history file
      CALL HICLOS (LHOUT, T, IBUFF2, IERR)
      IF (IERR.NE.0) WRITE (MSGTXT,9500) IERR
C
900   IF (IERR.NE.0) CALL MSGWRT (8)
C                                        Copy tables
      CALL ALLTAB (1, NOTTYP, LHIN, LHOUT, INVOL, OUTVOL, INSL, OUTSL,
     *   CATBLK, IBUFF1, IBUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MATHHI: COULD NOT COPY OLD HISTORY FILE TO NEW, IERR=',
     *        I3)
 2000 FORMAT ('MATHHI: COULD NOT WRITE INNAME TO HISTORY FILE, IERR=',
     *        I3)
 3000 FORMAT ('MATHHI: COULD NOT WRITE OUTNAME TO HISTORY FILE, IERR=',
     *        I3)
 4000 FORMAT (A6, 'BLC = ', I4, ',', I4, '   TRC = ', I4, ',', I4)
 5000 FORMAT ('MATHHI: COULD NOT ADD INPUT WINDOW, IERR=', I3)
 6000 FORMAT (A6, 'OPCODE = ''', A4, '''')
 7000 FORMAT ('MATHHI: COULD NOT ADD OPCODE TO HISTORY, IERR=', I3)
 8000 FORMAT (A6, 'CPARM = ', 5(1PE10.3, 1X))
 8500 FORMAT (A6, 'CPARM(1:4) = ', 4(1PE11.4, 1X))
 9000 FORMAT ('MATHHI: COULD NOT ADD CPARMS(1:5) TO HISTORY, IERR=', I3)
 9200 FORMAT (A6, 'CPARM(6) = ', 1PE11.4, '/ Undefined pixels zeroed')
 9300 FORMAT (A6, 'CPARM(6) = ', 1PE11.4,
     *        '/ Undefined pixels magic blanked')
 9400 FORMAT ('MATHHI: COULD NOT ADD CPARM(6) TO HISTORY, IERR=', I3)
 9500 FORMAT ('MATHHI: COULD NOT CLOSE HISTORY FILE, IERR=', I3)
      END
