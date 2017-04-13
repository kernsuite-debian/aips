LOCAL INCLUDE 'SCLIM.INC'
      HOLLERITH XINNAM(3), XINCLS(2), XOUNAM(3), XOUCLS(2), XOPCOD
      REAL      XINSEQ, XINDSK, XOUSEQ, XOUDSK, XBLC(7), XTRC(7),
     *   PIXR(2), CPARM(10)
      COMMON /INPARM/ XINNAM, XINCLS, XINSEQ, XINDSK, XOUNAM, XOUCLS,
     *   XOUSEQ, XOUDSK, XBLC, XTRC, PIXR, XOPCOD, CPARM
C
      INTEGER   INVOL, OUTVOL, INSL, OUTSL, BLC(7), TRC(7), IOP, LIN,
     *   LOUT
      COMMON /SCLIMP/ INVOL, OUTVOL, INSL, OUTSL, BLC, TRC, IOP, LIN,
     *   LOUT
      INTEGER   OLDI(256), NEWI(256)
      REAL      OLDR(256)
      EQUIVALENCE (OLDI, OLDR)
      COMMON /HEADS/ OLDI, NEWI
LOCAL END
      PROGRAM SCLIM
C-----------------------------------------------------------------------
C! Preforms mathematical functions on an image
C# Map-util math
C-----------------------------------------------------------------------
C;  Copyright (C) 2003, 2008, 2010, 2015
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
C   SCLIM operates on your image with one of a variety of simple
C   mathematical functions.
C RPARM    INPUTS:
C  1-3      INNAME(3)            Image name (name)
C  4-5      INCLASS(2)           Image name (class)
C  6        INSEQ                Image name (seq. #)
C  7        INDISK               Disk # of image
C  8-10     OUTNAME(3)           Output image name (name)
C  11-12    OUTCLASS(2)          Output image name (class)
C  13       OUTSEQ               Output image name (seq. #)
C  14       OUTDISK              Disk # of output image
C  15-21    BLC(7)               BLC of input image
C  22-28    TRC(7)               TRC of input image
C  29-30    PIXRANGE             Range allowed
C  31       OPCODE               Operator, LOG, ALOG, LOGN, EXP, POWR,
C                                'LN', 'NE', 'SQ', 'NQ', 'LG', 'NG'
C  32-41    CPARM(10)            User supplied parameters, 1-6 used
C-----------------------------------------------------------------------
      INTEGER   NOP
      PARAMETER (NOP = 11)
C
      CHARACTER INNAM*36, OUTNAM*36, OPCODE*4, LIST(NOP)*4, PRGNAM*6
      INTEGER   NROWS, NCOLS, NPARMS, IERR, IER
      INCLUDE 'SCLIM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA NPARMS /41/
      DATA LIST /'LOG ', 'LOGN', 'ALOG', 'EXP ', 'POWR', 'LN', 'NE',
     *   'SQ', 'NQ', 'LG', 'NG'/
      DATA PRGNAM /'SCLIM '/
C-----------------------------------------------------------------------
      IER = 1
      LIN = 17
      LOUT = 18
C                                       Start up task & get inputs
      CALL TSKBEG (PRGNAM, NPARMS, XINNAM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 900
         END IF
C                                       extract OPCODE
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      IERR = 0
      DO 10 IOP = 1,NOP
         IF (OPCODE.EQ.LIST(IOP)) GO TO 20
 10      CONTINUE
C                                       No match found
      WRITE (MSGTXT,1010) OPCODE
      CALL MSGWRT (8)
      GO TO 900
C                                       Open input & output images
 20   CALL OPENIM (OPCODE, INNAM, OUTNAM, NCOLS, NROWS, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Compute output image
      CALL OUTPUT (OPCODE, NROWS, NCOLS, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Add history file
      CALL SCLIHI (INNAM, OUTNAM, OPCODE)
      IER = 0
C
 900  CALL CLENUP
      CALL TSKEND (IER)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('START: COULD NOT GET INPUTS, IERR=', I3)
 1010 FORMAT ('OPCODE ''', A4, ''' IS UNRECOGNIZED')
      END
      SUBROUTINE OPENIM (OPCODE, INNAM, OUTNAM, NCOLS, NROWS, IERR)
C-----------------------------------------------------------------------
C   Open the input image, create and open the output image.
C   Input:
C      OPCODE   C*4    Operator, determines default class of output
C   Output:
C      INNAM     C*36   Namestring for input image
C      OUTNAM    C*36   Namestring for output image
C      NCOLS     I      Number of columns in output image
C      NROWS     I      Number of rows in output image
C      IERR      I      Error status, 0=> OK
C-----------------------------------------------------------------------
      CHARACTER INNAM*36, OUTNAM*36, OPCODE*4
      INTEGER   NROWS, NCOLS, IERR
C
      HOLLERITH MAP, XCL(2)
      REAL      USERID
      INTEGER   I
      CHARACTER DEFNAM*36, LINE*132
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'SCLIM.INC'
C-----------------------------------------------------------------------
C                                       Open input image
      CALL CHR2H (4, 'MA  ', 1, MAP)
      USERID = NLUSER
      CALL H2WAWA (XINNAM, XINCLS, XINSEQ, MAP, XINDSK, USERID, INNAM)
      CALL OPENCF (LIN, INNAM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) 'OPEN INPUT', IERR
         GO TO 900
         END IF
C                                       get header
      CALL GETHDR (LIN, OLDI, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) 'GET HEADER', IERR
         GO TO 900
         END IF
C                                       check/set window
      CALL WINDOW (OLDI(KIDIM), OLDI(KINAX), XBLC, XTRC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) 'SET WINDOW', IERR
         GO TO 900
         END IF
      DO 10 I = 1,7
         BLC(I) = XBLC(I) + 0.01
         TRC(I) = XTRC(I) + 0.01
 10      CONTINUE
      CALL MAPWIN (LIN, XBLC, XTRC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) 'MAP WINDOW', IERR
         GO TO 900
         END IF
C                                       Get catalog slot
      CALL CATSLV (LIN, INSL, INVOL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       set pixrange
      CALL RNGSET (PIXR, OLDR(KRDMX), OLDR(KRDMN), PIXR)
C                                       check special OPCODEs
      IF (OPCODE.EQ.'LN') THEN
         OPCODE = 'POWR'
         CPARM(1) = 0.0
         CPARM(2) = 1.0
         CPARM(3) = 1.0 / (PIXR(2)-PIXR(1))
         CPARM(4) = -PIXR(1)
         CPARM(5) = 1.0
      ELSE IF (OPCODE.EQ.'NE') THEN
         OPCODE = 'POWR'
         CPARM(1) = 0.0
         CPARM(2) = 1.0
         CPARM(3) = -1.0 / (PIXR(2)-PIXR(1))
         CPARM(4) = -PIXR(2)
         CPARM(5) = 1.0
      ELSE IF (OPCODE.EQ.'SQ') THEN
         OPCODE = 'POWR'
         CPARM(1) = 0.0
         CPARM(2) = 1.0
         CPARM(3) = 1/(PIXR(2)-PIXR(1))
         CPARM(4) = -PIXR(1)
         CPARM(5) = 0.5
      ELSE IF (OPCODE.EQ.'NQ') THEN
         OPCODE = 'POWR'
         CPARM(1) = 0.0
         CPARM(2) = 1.0
         CPARM(3) = -1/(PIXR(2)-PIXR(1))
         CPARM(4) = -PIXR(2)
         CPARM(5) = 0.5
      ELSE IF (OPCODE.EQ.'LG') THEN
         OPCODE = 'LOGN'
         CPARM(1) = 0.0
         CPARM(2) = 1.0 / LOG (6501.0)
         CPARM(3) = 6500.0 / (PIXR(2)-PIXR(1))
         CPARM(4) = (PIXR(2)-PIXR(1))/ 6500.0  - PIXR(1)
         CPARM(5) = 0.0
      ELSE IF (OPCODE.EQ.'NG') THEN
         OPCODE = 'LOGN'
         CPARM(1) = 0.0
         CPARM(2) = 1.0 / LOG (6501.0)
         CPARM(3) = -6500.0 / (PIXR(2)-PIXR(1))
         CPARM(4) = -(PIXR(2)-PIXR(1))/ 6500.0  - PIXR(2)
         CPARM(5) = 0.0
         END IF
      IF (CPARM(2).EQ.0.0) CPARM(2) = 1.0
      IF (CPARM(3).EQ.0.0) CPARM(3) = 1.0
C                                       Tell user what's going on
      MSGTXT = 'Output pixel formed from input pixel according to:'
      CALL MSGWRT (8)
      IF (OPCODE.EQ.'POWR') THEN
         WRITE (LINE,1010) CPARM(1), CPARM(2), CPARM(3), CPARM(4),
     *      CPARM(5)
      ELSE
         WRITE (LINE,1011) CPARM(1), CPARM(2), OPCODE, CPARM(3),
     *      CPARM(4)
         END IF
      CALL FIXLIN (LINE, I)
      IF (I.LE.64) THEN
         MSGTXT = LINE
      ELSE IF (OPCODE.EQ.'POWR') THEN
         WRITE (MSGTXT,1015) CPARM(1), CPARM(2), CPARM(3), CPARM(4),
     *      CPARM(5)
      ELSE
         WRITE (MSGTXT,1016) CPARM(1), CPARM(2), OPCODE, CPARM(3),
     *      CPARM(4)
         END IF
      CALL MSGWRT (4)
C                                       Output name string
      CALL CHR2H (6, OPCODE, 1, XCL)
      CALL H2WAWA (XINNAM, XCL, XINSEQ, MAP, XINDSK, USERID, DEFNAM)
      CALL H2WAWA (XOUNAM, XOUCLS, XOUSEQ, MAP, XOUDSK, USERID, OUTNAM)
C                                       sub-arraying
      NCOLS = TRC(1) - BLC(1) + 1
      NROWS = 1
      DO 20 I = 2,7
         NROWS = NROWS * (TRC(I) - BLC(I) + 1)
 20      CONTINUE
      CALL COPY (256, OLDI, CATBLK)
      CALL SUBHDR (XBLC, XTRC, 1.0, 1.0)
      CALL CHR2H (8, 'UNDEFINE', 1, CATH(KHBUN))
      CATR(KRBLK) = 0.0
      CALL COPY (256, CATBLK, NEWI)
C                                       Create output image
      CALL MAPCR (DEFNAM, OUTNAM, NEWI, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) 'CREATE OUTPUT', IERR
         GO TO 900
         END IF
C                                       Open output image
      CALL OPENCF (LOUT, OUTNAM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) 'OPEN OUTPUT', IERR
         GO TO 900
         END IF
C                                       Get catalog slot
      CALL CATSLV (LOUT, OUTSL, OUTVOL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) 'GET OUTPUT CNO', IERR
         GO TO 900
         END IF
C                                       copy most keywords
      CALL KEYPCP (INVOL, INSL, OUTVOL, OUTSL, 0, ' ', IERR)
      IERR = 0
C
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OPENIM: ON ',A,' IERR=', I3)
 1010 FORMAT (1PE12.5,'+',1PE12.5, '*[',1PE12.5,'*(in+',1PE12.5,')]**',
     *   1PE12.5)
 1011 FORMAT (1PE12.5,'+',1PE12.5,'*',A4,'[',1PE12.5,'*(in+', 1PE12.5,
     *   ')]')
 1015 FORMAT (1PE10.3,'+',1PE10.3, '*[',1PE10.3,'*(in+',1PE10.3,')]**',
     *   1PE10.3)
 1016 FORMAT (1PE10.3,'+',1PE10.3,'*',A4,'[',1PE10.3,'*(in+', 1PE10.3,
     *   ')]')
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
      SUBROUTINE FIXLIN (LINE, N)
C-----------------------------------------------------------------------
C   FIXLIN cleans up the text line describing the algorithm
C   In/out:
C      LINE   C*132   Display line - format E12.5 assumed
C   Out:
C      N      I       Last nin-blank character in LINE
C-----------------------------------------------------------------------
      CHARACTER LINE*(*)
      INTEGER   N
C
      INTEGER   I, J, K, ITRIM
C-----------------------------------------------------------------------
C                                       E+00
 10   I = INDEX (LINE, 'E+00')
      IF (I.GT.0) THEN
         LINE(I:I+3) = '    '
         GO TO 10
         END IF
C                                       trailing zeros
      J = 0
 20   I = INDEX (LINE(J+1:), '.')
      IF (I.GT.0) THEN
         J = J + I
         K = J + 5
 25      IF (LINE(K:K).EQ.'0') THEN
            LINE(K:K) = ' '
            K = K - 1
            IF (K.GT.J) GO TO 25
            GO TO 20
         ELSE
            GO TO 20
            END IF
         END IF
C                                       +-
 30   I = INDEX (LINE, '+-')
      IF (I.GT.0) THEN
         LINE(I:I+1) = '- '
         GO TO 30
         END IF
C                                       squeeze blanks
      N = 0
      K = ITRIM (LINE)
      DO 50 I = 1,K
         IF (LINE(I:I).NE.' ') THEN
            N = N + 1
            LINE(N:N) = LINE(I:I)
            END IF
 50      CONTINUE
      LINE(N+1:) = ' '
C
 999  RETURN
      END
      SUBROUTINE OUTPUT (OPCODE, NROWS, NCOLS, IERR)
C-----------------------------------------------------------------------
C   Main subroutine to operate on image with the desired scalpel
C   Input:
C      OPCODE   C*4   Operator code
C      NROWS    I     Number of rows in output image
C      NCOLS    I     Number of columns in output image
C   Output:
C      IERR     I     0 => OK
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   NROWS, NCOLS, IERR
C
      INCLUDE 'INCS:PMAD.INC'
      REAL      RBUF(MAXIMG), NEWR(256), DMIN, DMAX, BLKVAL
      INTEGER   IBUFF(256)
      LOGICAL   BLANKS
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SCLIM.INC'
      EQUIVALENCE (NEWI, NEWR)
      DATA DMIN, DMAX, BLANKS /1.0E34, -1.0E34, .FALSE./
C-----------------------------------------------------------------------
      IF (CPARM(6).GT.0.0) THEN
         BLKVAL = 0.0
      ELSE
         BLKVAL = 1.0
         END IF
C                                       Branch for each operator
      IF (OPCODE.EQ.'LOG') THEN
         CALL LOGARI (NROWS, NCOLS, LIN, LOUT, CPARM, BLKVAL,
     *      PIXR, RBUF, DMIN, DMAX, BLANKS, IERR)
      ELSE IF (OPCODE.EQ.'LOGN') THEN
         CALL NLOGAR (NROWS, NCOLS, LIN, LOUT, CPARM, BLKVAL,
     *      PIXR, RBUF, DMIN, DMAX, BLANKS, IERR)
      ELSE IF (OPCODE.EQ.'ALOG') THEN
         CALL ALOGAR (NROWS, NCOLS, LIN, LOUT, CPARM, BLKVAL,
     *      PIXR, RBUF, DMIN, DMAX, BLANKS, IERR)
      ELSE IF (OPCODE.EQ.'EXP') THEN
         CALL EXPONE (NROWS, NCOLS, LIN, LOUT, CPARM, BLKVAL,
     *      PIXR, RBUF, DMIN, DMAX, BLANKS, IERR)
      ELSE IF (OPCODE.EQ.'POWR') THEN
         CALL POWER (NROWS, NCOLS, LIN, LOUT, CPARM, BLKVAL,
     *      PIXR, RBUF, DMIN, DMAX, BLANKS, IERR)
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 900
         END IF
C                                       Update header
      NEWR(KRDMN) = DMIN
      NEWR(KRDMX) = DMAX
C
      IF (BLANKS) NEWR(KRBLK) = FBLANK
      CALL CATIO ('UPDT', OUTVOL, OUTSL, NEWI, 'REST', IBUFF, IERR)
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
      SUBROUTINE LOGARI (NROWS, NCOLS, LIN, LOUT, C, BLKVAL, PIXR, RBUF,
     *   DMIN, DMAX, BLANKS, IERR)
C-----------------------------------------------------------------------
C   Take LOGARITHM (base 10) of image
C      OUT = C(1) + C(2) * LOG10 (C(3) * IN + C(4))
C   Input:
C      NROWS    I      Number of rows in output image
C      NCOLS    I      Number of columns in output image
C      LIN      I      LUN of input image
C      LOUT     I      LUN of output image
C      C        R      Scale and offset factors
C      BLKVAL   R      Value for undefined pixels
C      PIXR     R(2)   Allowed pixel value range
C   Input/output:
C      RBUF     R(*)   Data buffer
C      DMIN     R      Min in image
C      DMAX     R      Max in image
C      BLANKS   L      .true. if MAGIC blank in output image
C      IERR     I      0 => OK
C-----------------------------------------------------------------------
      INTEGER   NROWS, NCOLS, LIN, LOUT, IERR
      REAL      C(*), BLKVAL, PIXR(2), RBUF(*), DMIN, DMAX
      LOGICAL   BLANKS
C
      REAL      FAC, SS, OFF, SMAX, SMIN
      INTEGER   I, J
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 8
      MSGTXT = 'PARAMETERS MAKE ILLEGAL LOG'
      FAC = C(3) * (PIXR(1) + C(4))
      IF (FAC.LE.0.0) GO TO 900
      SMIN = C(1) + C(2) * LOG10 (FAC)
      FAC = C(3) * (PIXR(2) + C(4))
      IF (FAC.LE.0.0) GO TO 900
      SMAX = C(1) + C(2) * LOG10 (FAC)
      SS = MIN (SMIN, SMAX)
      SMAX = MAX (SMIN, SMAX)
      SMIN = SS
      SS = 1.0 / (SMAX - SMIN)
      OFF = -SMIN / (SMAX - SMIN)
C
      DO 200 J = 1,NROWS
         CALL MAPIO ('READ', LIN, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            GO TO 900
            END IF
         DO 100 I = 1,NCOLS
            IF (RBUF(I).EQ.FBLANK) THEN
               BLANKS = .TRUE.
            ELSE IF (RBUF(I).GT.PIXR(2)) THEN
               RBUF(I) = BLKVAL
               DMIN = MIN (DMIN, RBUF(I))
               DMAX = MAX (DMAX, RBUF(I))
            ELSE
               FAC = C(3) * (MAX (PIXR(1), RBUF(I)) + C(4))
               IF (FAC.GT.0.0) THEN
                  RBUF(I) = (C(1) + C(2) * LOG10 (FAC)) * SS + OFF
                  DMIN = MIN (DMIN, RBUF(I))
                  DMAX = MAX (DMAX, RBUF(I))
               ELSE
                  RBUF(I) = FBLANK
                  BLANKS = .TRUE.
                  END IF
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) J, IERR
            GO TO 900
            END IF
 200     CONTINUE
C
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('LOGARI: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('LOGARI: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
      END
      SUBROUTINE NLOGAR (NROWS, NCOLS, LIN, LOUT, C, BLKVAL, PIXR, RBUF,
     *   DMIN, DMAX, BLANKS, IERR)
C-----------------------------------------------------------------------
C   Take LOGARITHM (natural-base e) of image
C      OUT = C(1) + C(2) * LOG (C(3) * IN + C(4))
C   Input:
C      NROWS    I      Number of rows in output image
C      NCOLS    I      Number of columns in output image
C      LIN      I      LUN of input image
C      LOUT     I      LUN of output image
C      C        R      Scale and offset factors
C      BLKVAL   R      Value for undefined pixels
C      PIXR     R(2)   Allowed pixel value range
C   Input/output:
C      RBUF     R(*)   Data buffer
C      DMIN     R      Min in image
C      DMAX     R      Max in image
C      BLANKS   L      .true. if MAGIC blank in output image
C      IERR     I      0 => OK
C-----------------------------------------------------------------------
      INTEGER   NROWS, NCOLS, LIN, LOUT, IERR
      REAL      C(*), BLKVAL, PIXR(2), RBUF(*), DMIN, DMAX
      LOGICAL   BLANKS
C
      REAL      FAC, SS, OFF, SMAX, SMIN
      INTEGER   I, J
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 8
      MSGTXT = 'PARAMETERS MAKE ILLEGAL LOG'
      FAC = C(3) * (PIXR(1) + C(4))
      IF (FAC.LE.0.0) GO TO 900
      SMIN = C(1) + C(2) * LOG (FAC)
      FAC = C(3) * (PIXR(2) + C(4))
      IF (FAC.LE.0.0) GO TO 900
      SMAX = C(1) + C(2) * LOG (FAC)
      SS = MIN (SMIN, SMAX)
      SMAX = MAX (SMIN, SMAX)
      SMIN = SS
      SS = 1.0 / (SMAX - SMIN)
      OFF = -SMIN / (SMAX - SMIN)
C
      DO 200 J = 1,NROWS
         CALL MAPIO ('READ', LIN, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            GO TO 900
            END IF
         DO 100 I = 1,NCOLS
            IF (RBUF(I).EQ.FBLANK) THEN
               BLANKS = .TRUE.
            ELSE IF (RBUF(I).GT.PIXR(2)) THEN
               RBUF(I) = BLKVAL
               DMIN = MIN (DMIN, RBUF(I))
               DMAX = MAX (DMAX, RBUF(I))
            ELSE
               FAC = C(3) * (MAX (PIXR(1), RBUF(I)) + C(4))
               IF (FAC.GT.0.0) THEN
                  RBUF(I) = (C(1) + C(2) * LOG(FAC)) * SS + OFF
                  DMIN = MIN (DMIN, RBUF(I))
                  DMAX = MAX (DMAX, RBUF(I))
               ELSE
                  RBUF(I) = FBLANK
                  BLANKS = .TRUE.
                  END IF
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) J, IERR
            GO TO 900
            END IF
 200     CONTINUE
C
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NLOGAR: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('NLOGAR: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
      END
      SUBROUTINE ALOGAR (NROWS, NCOLS, LIN, LOUT, C, BLKVAL, PIXR, RBUF,
     *   DMIN, DMAX, BLANKS, IERR)
C-----------------------------------------------------------------------
C   Take ANTI-LOGARITHM (base 10) of image
C      OUT = C(1) + C(2) * EXP(C(3) * IN + C(4))
C   Input:
C      NROWS    I      Number of rows in output image
C      NCOLS    I      Number of columns in output image
C      LIN      I      LUN of input image
C      LOUT     I      LUN of output image
C      C        R      Scale and offset factors
C      BLKVAL   R      Value for undefined pixels
C      PIXR     R(2)   Allowed pixel value range
C   Input/output:
C      RBUF     R(*)   Data buffer
C      DMIN     R      Min in image
C      DMAX     R      Max in image
C      BLANKS   L      .true. if MAGIC blank in output image
C      IERR     I      0 => OK
C-----------------------------------------------------------------------
      INTEGER   NROWS, NCOLS, LIN, LOUT, IERR
      REAL      C(*), BLKVAL, PIXR(2), RBUF(*), DMIN, DMAX
      LOGICAL   BLANKS
C
      REAL      SS, OFF, SMAX, SMIN, FAC
      INTEGER   I, J
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      SMIN = C(1) + C(2) * 10**(C(3) * (PIXR(1) + C(4)))
      SMAX = C(1) + C(2) * 10**(C(3) * (PIXR(2) + C(4)))
      SS = MIN (SMIN, SMAX)
      SMAX = MAX (SMIN, SMAX)
      SMIN = SS
      SS = 1.0 / (SMAX - SMIN)
      OFF = -SMIN / (SMAX - SMIN)
C
      DO 200 J = 1, NROWS
         CALL MAPIO ('READ', LIN, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            GO TO 900
            END IF
         DO 100 I = 1, NCOLS
            IF (RBUF(I).EQ.FBLANK) THEN
               BLANKS = .TRUE.
            ELSE IF (RBUF(I).GT.PIXR(2)) THEN
               RBUF(I) = BLKVAL
               DMIN = MIN (DMIN, RBUF(I))
               DMAX = MAX (DMAX, RBUF(I))
            ELSE
               FAC = C(3) * (MAX(RBUF(I),PIXR(1)) + C(4))
               RBUF(I) = (C(1) + C(2) * 10**FAC) * SS + OFF
               DMIN = MIN (DMIN, RBUF(I))
               DMAX = MAX (DMAX, RBUF(I))
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) J, IERR
            GO TO 900
            END IF
 200     CONTINUE
C
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ALOGAR: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('ALOGAR: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
      END
      SUBROUTINE EXPONE (NROWS, NCOLS, LIN, LOUT, C, BLKVAL, PIXR, RBUF,
     *   DMIN, DMAX, BLANKS, IERR)
C-----------------------------------------------------------------------
C   Take ANTI-LOGARITHM (natural-base e) of image
C      OUT = C(1) + C(2) * EXP(C(3) * IN + C(4))
C   Input:
C      NROWS    I      Number of rows in output image
C      NCOLS    I      Number of columns in output image
C      LIN      I      LUN of input image
C      LOUT     I      LUN of output image
C      C        R      Scale and offset factors
C      BLKVAL   R      Value for undefined pixels
C      PIXR     R(2)   Allowed pixel value range
C   Input/output:
C      RBUF     R(*)   Data buffer
C      DMIN     R      Min in image
C      DMAX     R      Max in image
C      BLANKS   L      .true. if MAGIC blank in output image
C      IERR     I      0 => OK
C-----------------------------------------------------------------------
      INTEGER   NROWS, NCOLS, LIN, LOUT, IERR
      REAL      C(*), BLKVAL, PIXR(2), RBUF(*), DMIN, DMAX
      LOGICAL   BLANKS
C
      REAL      SS, OFF, SMAX, SMIN, FAC
      INTEGER   I, J
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      SMIN = C(1) + C(2) * EXP (C(3) * (PIXR(1) + C(4)))
      SMAX = C(1) + C(2) * EXP (C(3) * (PIXR(2) + C(4)))
      SS = MIN (SMIN, SMAX)
      SMAX = MAX (SMIN, SMAX)
      SMIN = SS
      SS = 1.0 / (SMAX - SMIN)
      OFF = -SMIN / (SMAX - SMIN)
C
      DO 200 J = 1,NROWS
         CALL MAPIO ('READ', LIN, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            GO TO 900
            END IF
         DO 100 I = 1, NCOLS
            IF (RBUF(I).EQ.FBLANK) THEN
               BLANKS = .TRUE.
            ELSE IF (RBUF(I).GT.PIXR(2)) THEN
               RBUF(I) = BLKVAL
               DMIN = MIN (DMIN, RBUF(I))
               DMAX = MAX (DMAX, RBUF(I))
            ELSE
               FAC = C(3) * (MAX(PIXR(1),RBUF(I)) + C(4))
               RBUF(I) = (C(1) + C(2) * EXP (FAC)) * SS + OFF
               DMIN = MIN (DMIN, RBUF(I))
               DMAX = MAX (DMAX, RBUF(I))
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) J, IERR
            GO TO 900
            END IF
 200     CONTINUE
C
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('EXPONE: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('EXPONE: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
      END
      SUBROUTINE POWER (NROWS, NCOLS, LIN, LOUT, C, BLKVAL, PIXR, RBUF,
     *   DMIN, DMAX, BLANKS, IERR)
C-----------------------------------------------------------------------
C   Raise image to specified power
C      OUT = C(1) + C(2)*[C(3) * IN + C(4)]**C(5)
C   Note: if c(5) = 2.0 exactly, and the function in square brackets is
C   negative, then a special case is trapped and the usual undefined
C   exponentiation is avoided.
C   Input:
C      NROWS    I      Number of rows in output image
C      NCOLS    I      Number of columns in output image
C      LIN      I      LUN of input image
C      LOUT     I      LUN of output image
C      C        R      Scale and offset factors
C      BLKVAL   R      Value for undefined pixels
C      PIXR     R(2)   Allowed pixel value range
C   Input/output:
C      RBUF     R(*)   Data buffer
C      DMIN     R      Min in image
C      DMAX     R      Max in image
C      BLANKS   L      .true. if MAGIC blank in output image
C      IERR     I      0 => OK
C-----------------------------------------------------------------------
      INTEGER   NROWS, NCOLS, LIN, LOUT, IERR
      REAL      C(*), BLKVAL, PIXR(2), RBUF(*), DMIN, DMAX
      LOGICAL   BLANKS
C
      REAL      FAC, SS, OFF, SMAX, SMIN
      INTEGER   I, J, IROUND, IP
      LOGICAL   INT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 8
      MSGTXT = 'PARAMETERS MAKE ILLEGAL POWER'
      IP = IROUND (C(5))
      INT = ABS(IP-C(5)).LT.0.001
      FAC = C(3) * (PIXR(1) + C(4))
      IF ((FAC.LT.0.0) .AND. (.NOT.INT)) GO TO 900
      SMIN = C(1) + C(2) * FAC**C(5)
      FAC = C(3) * (PIXR(2) + C(4))
      IF ((FAC.LT.0.0) .AND. (.NOT.INT)) GO TO 900
      SMAX = C(1) + C(2) * FAC**C(5)
      SS = MIN (SMIN, SMAX)
      SMAX = MAX (SMIN, SMAX)
      SMIN = SS
      SS = 1.0 / (SMAX - SMIN)
      OFF = -SMIN / (SMAX - SMIN)
C
      DO 200 J = 1, NROWS
         CALL MAPIO ('READ', LIN, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) J, IERR
            GO TO 900
            END IF
         DO 100 I = 1, NCOLS
            IF (RBUF(I).EQ.FBLANK) THEN
               BLANKS = .TRUE.
            ELSE IF (RBUF(I).GT.PIXR(2)) THEN
               RBUF(I) = BLKVAL
               DMIN = MIN (DMIN, RBUF(I))
               DMAX = MAX (DMAX, RBUF(I))
            ELSE
               FAC = C(3) * (MAX (PIXR(1), RBUF(I)) + C(4))
               IF (INT) THEN
                  RBUF(I) = (C(1) + C(2) * FAC**IP) * SS + OFF
                  DMIN = MIN (DMIN, RBUF(I))
                  DMAX = MAX (DMAX, RBUF(I))
               ELSE IF (FAC.LT.0.0) THEN
                  RBUF(I) = FBLANK
                  BLANKS = .TRUE.
               ELSE IF ((FAC.EQ.0.0) .AND. (C(5).LT.0.0)) THEN
                  RBUF(I) = FBLANK
                  BLANKS = .TRUE.
               ELSE
                  RBUF(I) = (C(1) + C(2) * FAC**C(5)) * SS + OFF
                  DMIN = MIN (DMIN, RBUF(I))
                  DMAX = MAX (DMAX, RBUF(I))
                  END IF
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', LOUT, RBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000) J, IERR
            GO TO 900
            END IF
 200     CONTINUE
C
 900  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('POWER: COULD NOT READ LINE # ', I4, ' IERR=', I3)
 2000 FORMAT ('POWER: COULD NOT WRITE LINE # ', I4, ' IERR=', I3)
      END
      SUBROUTINE SCLIHI (INNAM, OUTNAM, OPCODE)
C-----------------------------------------------------------------------
C   SCLIHI writes the history file for the task SCLIM
C   Inputs:
C      INNAM    C*36   Input map namestring
C      OUTNAM   C*36   Output map namestring
C      OPCODE   C*4    SCLIM operator
C-----------------------------------------------------------------------
      CHARACTER INNAM*36, OUTNAM*36, OPCODE*4
C
      CHARACTER HILINE*72, NAME*12, CLASS*6, TYPE*2, NOTTYP*2
      INTEGER   NHISTF, LHIN, LHOUT, INSEQ, OUTSEQ, IBUFF1(256),
     *   IBUFF2(256), USID, IERR
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'SCLIM.INC'
      DATA NHISTF, LHIN, LHOUT /2, 27, 28/
      DATA T /.TRUE./
      DATA NOTTYP /'CC'/
C-----------------------------------------------------------------------
C                                       Init HI
      CALL HIINIT (NHISTF)
C                                       Copy old to new
      CALL HISCOP (LHIN, LHOUT, INVOL, OUTVOL, INSL, OUTSL, NEWI,
     *   IBUFF1, IBUFF2, IERR)
      IF (IERR.GE.3) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Add INNAME
      CALL WAWA2A (INNAM, NAME, CLASS, INSEQ, TYPE, INVOL, USID)
      CALL HENCO1 (TSKNAM, NAME, CLASS, INSEQ, INVOL, LHOUT, IBUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Add OUTNAME
      CALL WAWA2A (OUTNAM, NAME, CLASS, OUTSEQ, TYPE, OUTVOL, USID)
      CALL HENCOO (TSKNAM, NAME, CLASS, OUTSEQ, OUTVOL, LHOUT, IBUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Add window
      WRITE (HILINE,1010) TSKNAM, BLC
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1011) TSKNAM, TRC
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1012) TSKNAM, PIXR
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Add OPCODE
      WRITE (HILINE,1015) TSKNAM, OPCODE
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Add CPARM's
      IF (OPCODE.EQ.'POWR') THEN
         WRITE (HILINE,1020) TSKNAM, CPARM(1), CPARM(2), CPARM(3),
     *      CPARM(4), CPARM(5)
      ELSE
         WRITE (HILINE,1025) TSKNAM, CPARM(1), CPARM(2), CPARM(3),
     *      CPARM(4)
         END IF
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C
      IF (CPARM(6).GT.0.0) THEN
         HILINE = TSKNAM // '/ overflow set to 0.0'
      ELSE
         HILINE = TSKNAM // '/ overflow set to 1.0'
         END IF
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.EQ.0) GO TO 200
C                                       HI ERROR
 100  WRITE (MSGTXT,1100) IERR
      CALL MSGWRT (8)
C                                       Close history file
 200  CALL HICLOS (LHOUT, T, IBUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (1, NOTTYP, LHIN, LHOUT, INVOL, OUTVOL, INSL, OUTSL,
     *   NEWI, IBUFF1, IBUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SCLIHI: COULD NOT COPY OLD HISTORY FILE TO NEW, IERR=',
     * I3)
 1100 FORMAT ('SCLIHI: HISTORY FILE ERROR=',I3)
 1010 FORMAT (A6,'BLC=',7I6,'/ Bottom left corner')
 1011 FORMAT (A6,'TRC=',7I6,'/ Top right corner')
 1012 FORMAT (A6,'PIXRANGE=',2(1PE12.5),' / Intensity range')
 1015 FORMAT (A6,'OPCODE = ''',A4,'''')
 1020 FORMAT (A6,'CPARM =',5(1PE11.3))
 1025 FORMAT (A6,'CPARM(1:4) =',4(1PE12.4))
      END
