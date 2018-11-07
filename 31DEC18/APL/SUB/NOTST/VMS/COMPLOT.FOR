CD COMPLOT     A general purpose graph plotting package for the ZETA pen
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C  plotter
C
C     +-----------------+
C     !  C O M P L O T  !
C     +-----------------+
C
C     Author: Arnold Rots        Date: 13 May 1983
C
C     Version: 4.0               Language: F77
C
C     Description:
C       This software package contains routines for making graphs on
C       the ZETA pen plotter.  It is included in ZETASUBS.OLB, on the
C       VAXes in DISK$RES:[PLOT], on the PDP-11s in [210,35].
C       It contains the following routines:
C            OPNPLT
C            OPNPLTA      for AIPS applications
C            LABELP
C            LABELPA      for AIPS applications
C            GRAFPL
C            NEXTPH
C            NEXTPV
C            CLSPLT
C            CLSPLTA      for AIPS applications
C       In addition these routines call the following ones contained
C       in the package:
C            FRAMEP
C            PLOTR
C            SYMBLR
C            NUMBRR
C
C     Included files:
C       DMSG.INC          for AIPS applications only
C       CMSG.INC          for AIPS applications only
C
C     Externals:
C       PLOTS
C       PLOT
C       FACTOR
C       SPEED
C       NEWPEN
C       SYMBOL
C       NUMBER
C       DASHDF
C
C     Commons:
C       GRAF
C       GRAF1
C
C     Revisions:
CE
C
      SUBROUTINE OPNPLT (TNAME)
CD OPNPLT      Plot initialization for COMPLOT
C
C     +---------------+
C     !  O P N P L T  !
C     +---------------+
C
C     Author: Arnold Rots        Date: 12 February 1987
C
C     Version: 1.0               Language: F77
C
C     Description:
C       This routine initializes a plot for the COMPLOT package.  The
C       plot is directly output to the plotter.
C
C     Calling sequence:
C       CALL OPNPLT
C
C     Parameters:
C       TNAME(2)   R        Name of calling program; to be displayed
C                           in the plot header.
C
C     Included files:
C       None
C
C     Externals:
C       PLOTS
C       PLOT
C       FACTOR
C       SPEED
C       NEWPEN
C       SYMBOL
C       DASHDF
C
C     Commons:
C       GRAF
C       GRAF1
C
C     Revisions:
C       830513 Version 1.0
C
C       870212 Version 1.1
C              Plot header.
C              Changed calling sequence to include TNAME.  -  ahr
CE
C-----------------------------------------------------------------------
C
      REAL   TNAME(2), HEAD(3), DAT(16)
      INTEGER   I, IRATE, NM3, N0, N1, N3, N5, N8, N12, N53, N64
C
C                                      GRAF
      REAL   CSIZE, XSIZE, YSIZE, XLAB(2), YLAB(2), MSIZE
      INTEGER   NP, NCHAR, NPEN, NANG, NDASH
      COMMON /GRAF/ NP, NANG, CSIZE, XSIZE, YSIZE, XLAB, YLAB,
     +              NCHAR, NPEN, NDASH, MSIZE
C                                      End GRAF
C
C
C                                      GRAF1
      REAL   XMIN, XMAX, YMIN, YMAX, DX, DY, X0, Y0
      INTEGER   NDEV
      LOGICAL   ADD
      COMMON /GRAF1/ XMIN, XMAX, YMIN, YMAX, DX, DY, X0, Y0, ADD, NDEV
C                                      End GRAF1
C
      DATA NM3, N0, N1, N3, N5, N8, N53 /-3, 0, 1, 3, 5, 8, 53/
      DATA N12, N64 /12, 64/
      DATA HEAD /'COMP', 'LOT ', ' -  '/
      DATA DAT /'COMP', 'LOT ', 'SPOO', 'L FI', 'LE G', 'ENER', 'ATED',
     +          ' ON ', ':   ', 'DD-M', 'MM-Y', 'Y   ', ' AT ', ':   ',
     +          'HH:M', 'M:SS'/
C
      NDEV = -1
      IRATE = 20
      CALL PLOTS (N53, N0, NDEV)
      CALL FACTOR (0.3937)
      CALL SPEED (IRATE)
      CALL DASHDF (N0, CSIZE, XSIZE, YSIZE)
      CALL PLOT (0.0, -30.0, N3)
      CALL PLOT (0.0, -29.7, NM3)
      DO 30 I=1,4
      CALL NEWPEN (I)
   30 CALL SYMBOL (0.0, REAL(5*I), 0.35, 'PRIME', 90.0, N5)
      CALL NEWPEN (1)
      IF ((TNAME(1) .NE. '    ') .AND. (TNAME(1) .NE. 0.0)) THEN
         HEAD(1) = TNAME(1)
         HEAD(2) = TNAME(2)
      END IF
      CALL SYMBOL (2.0, 3.0, 0.5, HEAD, 90.0, N12)
      CALL DATE (DAT(10))
      CALL TIME (DAT(15))
      CALL SYMBOL (3.0, 3.0, 0.35, DAT, 90.0, N64)
      CALL PLOT (6.0, 0.0, NM3)
      CALL PLOT (3.0, 0.0, NM3)
      IF (NANG .EQ. 0) CALL PLOT (0.0, 27.0, NM3)
      ADD = .FALSE.
C
      RETURN
      END
C
      SUBROUTINE OPNPLTA (TNAME, OUTN, ERROR)
CD OPNPLTA     AIPS version of OPNPLT; plot initialization for COMPLOT
C
C     +-----------------+
C     !  O P N P L T A  !
C     +-----------------+
C
C     Author: Arnold Rots        Date: 12 February 1987
C
C     Version: 1.1               Language: F77
C
C     Description:
C       This routine initializes a plot for the COMPLOT package in AIPS.
C       TNAME should contain the calling task name.
C       The file <OUTN>.QLT created in ZPLOTDIR:; this file is
C       used to store the plot instructions.  It can be unspooled by
C       running OUTPLT.
C
C     Calling sequence:
C       CALL OPNPLTA (TNAME, OUTN, ERROR)
C
C     Parameters:
C       TNAME(2)   R        Name of calling program; to be displayed
C                           in the plot header.
C       OUTN(2)    R        Name of plot file to be created; should
C                           contain up to 8 alphanumeric characters.
C                           If OUTN is blank, ACTPLT.QLT is created.
C       ERROR      I        Returns 0 if no errors occurred,
C                           40 if it could not open <OUTN>.QLT
C                           (ususally because OUTN contained an
C                           illegal string.
C
C     Included files:
C       DMSG.INC
C       CMSG.INC
C
C     Externals:
C       PLOTS
C       PLOT
C       FACTOR
C       SPEED
C       NEWPEN
C       SYMBOL
C       DASHDF
C       FILENM
C
C     Commons:
C       GRAF
C       GRAF1
C
C     Revisions:
C       830513 Version 1.0
C
C       870212 Version 1.1
C              Changed file name cracking to FILENM; plot header.
C              Changed calling sequence to include TNAME.  -  ahr
CE
C
C
      REAL   DAT(16), TNAME(2), HEAD(3)
      INTEGER   OUTN(1)
      CHARACTER*60 FILNAM
      INTEGER   I, IRATE, ERROR, NM3, N0, N1, N3, N4, N5, N6, N7,
     +          N8, N9, N12, N53, N64, I1, I2, I3
      LOGICAL*1 L1(2)
      EQUIVALENCE (L1(1), I1)
      INCLUDE 'INCS:DMSG.INC'
C
C                                      GRAF
      REAL   CSIZE, XSIZE, YSIZE, XLAB(2), YLAB(2), MSIZE
      INTEGER   NP, NCHAR, NPEN, NANG, NDASH
      COMMON /GRAF/ NP, NANG, CSIZE, XSIZE, YSIZE, XLAB, YLAB,
     +              NCHAR, NPEN, NDASH, MSIZE
C                                      End GRAF
C
C
C                                      GRAF1
      REAL   XMIN, XMAX, YMIN, YMAX, DX, DY, X0, Y0
      INTEGER   NDEV
      LOGICAL   ADD
      COMMON /GRAF1/ XMIN, XMAX, YMIN, YMAX, DX, DY, X0, Y0, ADD, NDEV
C                                      End GRAF1
C
      DATA NM3, N0, N1, N3, N4, N5, N6, N7 /-3, 0, 1, 3, 4, 5, 6, 7/
      DATA N8, N9, N12, N53, N64 /8, 9, 12, 53, 64/
      DATA HEAD /'COMP', 'LOT ', ' -  '/
      DATA DAT /'COMP', 'LOT ', 'SPOO', 'L FI', 'LE G', 'ENER', 'ATED',
     +          ' ON ', ':   ', 'DD-M', 'MM-Y', 'Y   ', ' AT ', ':   ',
     +          'HH:M', 'M:SS'/
C
      NDEV = 21
      IRATE = 20
      ERROR = 40
      CALL FILENM (OUTN, N8, '.QLT', FILNAM)
      OPEN (UNIT=NDEV, NAME=FILNAM, TYPE='NEW', ERR=900)
      ERROR = 0
      CALL PLOTS (N53, N0, NDEV)
      CALL FACTOR (0.3937)
      CALL SPEED (IRATE)
      CALL DASHDF (N0, CSIZE, XSIZE, YSIZE)
      CALL PLOT (0.0, -30.0, N3)
      CALL PLOT (0.0, -29.7, NM3)
      DO 30 I=1,4
      CALL NEWPEN (I)
   30 CALL SYMBOL (0.0, REAL(5*I), 0.35, 'PRIME', 90.0, N5)
      CALL NEWPEN (1)
      IF ((TNAME(1) .NE. '    ') .AND. (TNAME(1) .NE. 0.0)) THEN
         HEAD(1) = TNAME(1)
         HEAD(2) = TNAME(2)
      END IF
      CALL SYMBOL (2.0, 3.0, 0.5, HEAD, 90.0, N12)
      DO 40 I=1,30
         IF (FILNAM(I:I) .EQ. ' ') GOTO 50
         L1(1) = ICHAR(FILNAM(I:I))
         L1(2) = L1(1)
         CALL SYMBOL (999., 999., -0.5, I1, 90.0, N1)
 40   CONTINUE
 50   CONTINUE
      CALL DATE (DAT(10))
      CALL TIME (DAT(15))
      CALL SYMBOL (3.0, 3.0, 0.35, DAT, 90.0, N64)
      CALL PLOT (6.0, 0.0, NM3)
      IF (NANG .EQ. 0) CALL PLOT (0.0, 27.0, NM3)
      ADD = .FALSE.
      GOTO 990
C                                          ERROR
 900  CONTINUE
      WRITE (MSGTXT,9000) FILNAM
 9000 FORMAT (' FAILED TO OPEN PLOT FILE : ',24A1)
      CALL MSGWRT (8)
      WRITE (MSGTXT,9001)
 9001 FORMAT (' ***  REMEMBER: ONLY UP TO 8 ALPHANUMERIC CHARACTERS',
     +        '  ***')
      CALL MSGWRT (8)
C
 990  CONTINUE
      RETURN
      END
C
      SUBROUTINE FILENM (I2NAM, N, EXT, FILNAM)
C-----------------------------------------------------------------------
C
C   FILENM constructs a file name FILNAM from the directory information
C   in PLOG, the name in INTEGER*2 I2NAM (up to N characters), and the
C   extension EXT (should include the period).
C
C-----------------------------------------------------------------------
C
      LOGICAL*1 I2NAM(1)
      INTEGER   N, I, I1, I2, I3
      CHARACTER PLOG*60, EXT*(*), FILNAM*(*), STR*60, EXT2*60
C
C***   FOR VAX VMS USE THE FOLLOWING STATEMENT - IT ASSUMES ZPLOTDIR:
C              DEFINED
      DATA PLOG /'ZPLOTDIR:'/
C***   FOR UNIX USE THE FOLLOWING STATEMENT - IT ASSUMES A SUBDIRECTORY
C              "plot"
C      DATA PLOG /'/AIPS/plot/'/
C
C-----------------------------------------------------------------------
C                                      Crack 	I2NAM
C
      EXT2 = EXT
      STR = ' '
      DO 10 I=1,N
         IF (I2NAM(I) .EQ. 0) THEN
               GOTO 20
            ELSE
               STR(I:I) = CHAR(I2NAM(I))
         END IF
 10   CONTINUE
 20   CONTINUE
C
C                                      Concatenate
C
      I1 = INDEX(PLOG,' ') - 1
      I2 = INDEX(STR,' ') - 1
      I3 = INDEX(EXT2,' ') - 1
      IF (I2 .NE. 0) THEN
            IF (I3. NE. 0) THEN
               STR = STR(1:I2)//EXT2(1:I3)
            END IF
         ELSE
            STR = EXT2
      END IF
      IF (I2+I3 .NE. 0) THEN
            FILNAM = PLOG(1:I1)//STR(1:I2+I3)
         ELSE
            FILNAM = PLOG
      END IF
      RETURN
      END
C
      SUBROUTINE LABELP (TEXT1, NTEXT1, TEXT2, NTEXT2)
CD LABELP      Plots a label for COMPLOT
C
C     +---------------+
C     !  L A B E L P  !
C     +---------------+
C
C     Author: Arnold Rots        Date: 13 May 1983
C
C     Version: 1.0               Language: F77
C
C     Description:
C       This routine labels a plot for the COMPLOT package.
C
C     Calling sequence:
C       CALL LABELP (TEXT1, NTEXT1, TEXT2, NTEXT2)
C
C     Parameters:
C       TEXT1(*)    R        First Text Line to Be Added
C       NTEXT1      I        Length of TEXT1 in Characters
C                            If =0, no extra text lines are added
C       TEXT2(*)    R        Second Text Line to Be Added
C       NTEXT2      I        Length of TEXT2 in Characters
C                            If =0 no second extra text line is added
C
C     Included files:
C       None
C
C     Externals:
C       PLOTR
C       NUMBRR
C       SYMBLR
C
C     Commons:
C       GRAF
C       GRAF1
C
C     Revisions:
CE
C-----------------------------------------------------------------------
C
      REAL   X, X1, Y, Y1, SIZE, ANG, Z, X999
      INTEGER   TEXT1(1), TEXT2(1), NTEXT1, NTEXT2
      INTEGER   NM3, NM1, N1, N6, N8, N11, N12, N14
      INTEGER   I
C
C                                      GRAF
      REAL   CSIZE, XSIZE, YSIZE, XLAB(2), YLAB(2), MSIZE
      INTEGER   NP, NCHAR, NPEN, NANG, NDASH
      COMMON /GRAF/ NP, NANG, CSIZE, XSIZE, YSIZE, XLAB, YLAB,
     +              NCHAR, NPEN, NDASH, MSIZE
C                                      End GRAF
C
C
C                                      GRAF1
      REAL   XMIN, XMAX, YMIN, YMAX, DX, DY, X0, Y0
      INTEGER   NDEV
      LOGICAL   ADD
      COMMON /GRAF1/ XMIN, XMAX, YMIN, YMAX, DX, DY, X0, Y0, ADD, NDEV
C                                      End GRAF1
C
      DATA NM3, NM1, N6, N8, N11, N12, N14 /-3, -1, 6, 8, 11, 12, 14/
      DATA SIZE, ANG /0.5, 90.0/, N1, X999 /1, 999.0/
C
C-----------------------------------------------------------------------
C
      SIZE = CSIZE
      X = 1.0
      Y = SIZE
      IF (NTEXT1 .LE. 0) GOTO 20
         CALL SYMBLR (Y, X, SIZE, TEXT1, ANG, NTEXT1)
         IF (NTEXT2 .LE. 0) GOTO 20
            Y = Y + 2.0 * SIZE
            CALL SYMBLR (Y, X, SIZE, TEXT2, ANG, NTEXT2)
 20   CONTINUE
      Y = Y + 3.0 * SIZE
      CALL PLOTR (Y, 0.0, NM3)
      RETURN
      END
C
      SUBROUTINE LABELPA (NAME, SOURCE, IB, IT, TEXT1, NTEXT1,
     +                   TEXT2, NTEXT2)
CD LABELPA     Plots an AIPS label for COMPLOT
C
C     +-----------------+
C     !  L A B E L P A  !
C     +-----------------+
C
C     Author: Arnold Rots        Date: 13 May 1983
C
C     Version: 1.0               Language: F77
C
C     Description:
C       This routine labels a plot for the COMPLOT package.  The label
C       is an AIPS-type label.  LABELPA is the AIPS version of LABELP.
C
C   Calling sequence:
C     CALL LABELPA (NAME, SOURCE, IB, IT, TEXT1, NTEXT1, TEXT2, NTEXT2)
C
C     Parameters:
C       NAME(7)     R        Map File Name
C                            NAME(1:3): Name
C                            NAME(4:5): Class
C                            NAME(6):   Sequence number
C                            NAME(7):   User number
C       SOURCE(2)   R        Source (Object) Name
C       IB(7)       I        Image bounds (BLC)
C       IT(7)       I        Image bounds (TRC)
C       TEXT1(*)    R        First Text Line to Be Added
C       NTEXT1      I        Length of TEXT1 in Characters
C                            If =0, no extra text lines are added
C       TEXT2(*)    R        Second Text Line to Be Added
C       NTEXT2      I        Length of TEXT2 in Characters
C                            If =0 no second extra text line is added
C
C     Included files:
C       None
C
C     Externals:
C       PLOTR
C       NUMBRR
C       SYMBLR
C
C     Commons:
C       GRAF
C       GRAF1
C
C     Revisions:
CE
C-----------------------------------------------------------------------
C
      REAL   X, X1, Y, Y1, SIZE, ANG, Z, X999
      REAL   NAME(1), MAPN(3), SOURN(4), BLC(2), TRC(2)
      INTEGER   SOURCE(1), TEXT1(1), TEXT2(1), NTEXT1, NTEXT2
      INTEGER   IB(7), IT(7)
      INTEGER   NM3, NM1, N1, N6, N8, N9, N11, N12, N14
      INTEGER   I
C
C                                      GRAF
      REAL   CSIZE, XSIZE, YSIZE, XLAB(2), YLAB(2), MSIZE
      INTEGER   NP, NCHAR, NPEN, NANG, NDASH
      COMMON /GRAF/ NP, NANG, CSIZE, XSIZE, YSIZE, XLAB, YLAB,
     +              NCHAR, NPEN, NDASH, MSIZE
C                                      End GRAF
C
C
C                                      GRAF1
      REAL   XMIN, XMAX, YMIN, YMAX, DX, DY, X0, Y0
      INTEGER   NDEV
      LOGICAL   ADD
      COMMON /GRAF1/ XMIN, XMAX, YMIN, YMAX, DX, DY, X0, Y0, ADD, NDEV
C                                      End GRAF1
C
      DATA MAPN /'Map ','name',' :  '/
      DATA SOURN /'Sour','ce n','ame ',':   '/
      DATA BLC /'BLC ',':   '/
      DATA TRC /'TRC ',':   '/
      DATA NM3, NM1, N6, N8, N9, N11, N12, N14 /-3, -1,6,8,9,11,12,14/
      DATA SIZE, ANG /0.5, 90.0/, N1, X999 /1, 999.0/
C
C-----------------------------------------------------------------------
C
      SIZE = CSIZE
      X = 1.0
      Y = SIZE
      X1 = X + 14.0 * SIZE
      CALL SYMBLR (Y, X, SIZE, MAPN, ANG, N11)
      CALL SYMBLR (Y, X1, SIZE, NAME, ANG, N12)
      CALL SYMBLR (X999, X999, SIZE, '.', ANG, N1)
      CALL SYMBLR (X999, X999, SIZE, NAME(4), ANG, N6)
      CALL SYMBLR (X999, X999, SIZE, ';', ANG, N1)
      CALL NUMBRR (X999, X999, SIZE, NAME(6), ANG, NM1)
      CALL SYMBLR (X999, X999, SIZE, '  User : ', ANG, N9)
      CALL NUMBRR (X999, X999, SIZE, NAME(7), ANG, NM1)
      Y = Y + 2.0 * SIZE
      CALL SYMBLR (Y, X, SIZE, SOURN, ANG, N14)
      CALL SYMBLR (Y, X1, SIZE, SOURCE, ANG, N8)
      Y = Y + 2.0 * SIZE
      Y1 = Y + 2.0 * SIZE
      X1 = X
      CALL SYMBLR (Y, X, SIZE, BLC, ANG, N6)
      CALL SYMBLR (Y1, X, SIZE, TRC, ANG, N6)
      DO 10 I=1,7
         X1 = X1 + 6.0 * SIZE
         Z = IB(I)
         CALL NUMBRR (Y, X1, SIZE, Z, ANG, NM1)
         Z = IT(I)
         CALL NUMBRR (Y1, X1, SIZE, Z, ANG, NM1)
 10   CONTINUE
      IF (NTEXT1 .LE. 0) GOTO 20
         Y = Y + 5.0 * SIZE
         CALL SYMBLR (Y, X, SIZE, TEXT1, ANG, NTEXT1)
         IF (NTEXT2 .LE. 0) GOTO 20
            Y = Y + 2.0 * SIZE
            CALL SYMBLR (Y, X, SIZE, TEXT2, ANG, NTEXT2)
 20   CONTINUE
      Y = Y + 3.0 * SIZE
      CALL PLOTR (Y, 0.0, NM3)
      RETURN
      END
C
      SUBROUTINE GRAFPL (XAR, YAR, XMI, XMA, YMI, YMA)
CD GRAFPL      Plots the graph for COMPLOT
C
C     +---------------+
C     !  G R A F P L  !
C     +---------------+
C
C     Author: Arnold Rots        Date: 13 May 1983
C
C     Version: 1.0               Language: F77
C
C     Description:
C       This routine does the actual graph plotting for the COMPLOT
C       package.
C
C     Calling sequence:
C       CALL GRAFPL (XAR, YAR, XMI, XMA, YMI, YMA)
C
C     Parameters:
C       XAR(*)      R        Array with X Coordinates
C       YAR(*)      R        Array with Y Coordinates
C       XMI         R        Minimum X Value
C       XMA         R        Maximum X Value
C       YMI         R        Minimum Y Value
C       YMA         R        Maximum Y Value
C
C     Included files:
C       None
C
C     Externals:
C       PLOTR
C       SYMBLR
C       NEWPEN
C       FRAMEP
C
C     Commons:
C       GRAF
C       GRAF1
C
C     Revisions:
CE
C-----------------------------------------------------------------------
C
      REAL   XAR(1), YAR(1), XMI, XMA, YMI, YMA
C
      REAL   SIZE, ANG, X, Y, X1, Y1, X2, Y2
      INTEGER   I, NM3, NM1, N1, N2, N3, ND, MNDAS, NDAS
      LOGICAL   PLOT
C
C                                      GRAF
      REAL   CSIZE, XSIZE, YSIZE, XLAB(2), YLAB(2), MSIZE
      INTEGER   NP, NCHAR, NPEN, NANG, NDASH
      COMMON /GRAF/ NP, NANG, CSIZE, XSIZE, YSIZE, XLAB, YLAB,
     +              NCHAR, NPEN, NDASH, MSIZE
C                                      End GRAF
C
C
C                                      GRAF1
      REAL   XMIN, XMAX, YMIN, YMAX, DX, DY, X0, Y0
      INTEGER   NDEV
      LOGICAL   ADD
      COMMON /GRAF1/ XMIN, XMAX, YMIN, YMAX, DX, DY, X0, Y0, ADD, NDEV
C                                      End GRAF1
C
C
      DATA NM3, NM1, N1, N2, N3 /-3, -1, 1, 2, 3/
C
C-----------------------------------------------------------------------
C                                      Check bounds
      PLOT = ((NCHAR .LT. 0) .OR. (NCHAR .GT. 16))
      IF ((NDASH .GE. 4) .AND. (NDASH .LE. 9)) THEN
            NDAS = NDASH
         ELSE IF (PLOT) THEN
            NDAS = 2
         ELSE IF (NDASH .EQ. 0) THEN
            NDAS = 3
         ELSE
            NDAS = 2
      END IF
      MNDAS = -NDAS
      SIZE = CSIZE
      CALL PLOTR (0.0, 10.0*SIZE, NM3)
      IF (ADD) GOTO 90
      IF (NANG .EQ. 0) THEN
            YSIZE = MIN(YSIZE,27.0-5.0*SIZE)
         ELSE
            XSIZE = MIN(XSIZE,27.0-10.0*SIZE)
      END IF
      X1 = 1.0E10
      X2 = -X1
      Y1 = X1
      Y2 = -X1
      DO 10 I=1,NP
         X1 = MIN(X1,XAR(I))
         X2 = MAX(X2,XAR(I))
         Y1 = MIN(Y1,YAR(I))
         Y2 = MAX(Y2,YAR(I))
 10   CONTINUE
      IF (YMI .EQ. YMA) THEN
            Y = 0.1 * MAX(ABS(Y1),ABS(Y2))
            YMIN = Y1 - Y
            YMAX = Y2 + Y
         ELSE
            YMIN = YMI
            YMAX = YMA
      END IF
      IF (XMA .EQ. XMI) THEN
            XMIN = X1
            XMAX = X2
         ELSE
            XMIN = XMI
            XMAX = XMA
      END IF
C                                      Increments, origin
      DX = XSIZE / (XMAX - XMIN)
      DY = YSIZE / (YMAX - YMIN)
      X0 = DX * XMIN
      Y0 = DY * YMIN
      CALL PLOTR (YSIZE, 0.0, NM3)
C                                      Frame and Tick Marks
      CALL FRAMEP
C
C                                      Graph
 90   CONTINUE
      CALL NEWPEN (NPEN)
      ND = 3
      X = XAR(1) * DX - X0
      Y = YAR(1) * DY - Y0
      IF (((X .GE. -0.01) .AND. (X .LE. XSIZE+0.01)) .AND.
     +    ((Y .GE. -0.01) .AND. (Y .LE. YSIZE+0.01))) THEN
         IF (PLOT) THEN
               CALL PLOTR (-Y, X, ND)
            ELSE
               CALL SYMBLR (-Y, X, MSIZE, NCHAR, ANG, NM1)
         END IF
         ND = 2
      END IF
      X1 = X
      Y1 = Y
      DO 100 I=2,NP
         X = XAR(I) * DX - X0
         Y = YAR(I) * DY - Y0
         IF (((X .GE. -0.01) .AND. (X .LE. XSIZE+0.01)) .AND.
     +       ((Y .GE. -0.01) .AND. (Y .LE. YSIZE+0.01))) THEN
               IF (ND .EQ. 2) THEN
                     IF (PLOT) THEN
                           CALL PLOTR (-Y, X, NDAS)
                        ELSE
                           CALL SYMBLR (-Y, X, MSIZE, NCHAR, ANG, MNDAS)
                     END IF
                  ELSE
                     X2 = MAX(0.0,MIN(XSIZE,X1))
                     Y2 = MAX(0.0,MIN(YSIZE,Y1))
                     IF ((X2 .EQ. 0.0) .OR. (X2 .EQ. XSIZE))
     +                  Y2 = (Y-Y1)/(X-X1) * X2 + (X*Y1-X1*Y)/(X-X1)
                     Y2 = MAX(0.0,MIN(YSIZE,Y2))
                     IF ((Y2 .EQ. 0.0) .OR. (Y2 .EQ. YSIZE))
     +                  X2 = (X-X1)/(Y-Y1) * Y2 + (Y*X1-Y1*X)/(Y-Y1)
                     CALL PLOTR (-Y2, X2, ND)
                     ND = 2
                     IF (PLOT) THEN
                           CALL PLOTR (-Y, X, NDAS)
                        ELSE
                           CALL SYMBLR (-Y, X, MSIZE, NCHAR, ANG, MNDAS)
                     END IF
               END IF
            ELSE
               IF (ND .EQ. 2) THEN
                     X2 = MAX(0.0,MIN(XSIZE,X))
                     Y2 = MAX(0.0,MIN(YSIZE,Y))
                     IF ((X2 .EQ. 0.0) .OR. (X2 .EQ. XSIZE))
     +                  Y2 = (Y-Y1)/(X-X1) * X2 + (X*Y1-X1*Y)/(X-X1)
                     Y2 = MAX(0.0,MIN(YSIZE,Y2))
                     IF ((Y2 .EQ. 0.0) .OR. (Y2 .EQ. YSIZE))
     +                  X2 = (X-X1)/(Y-Y1) * Y2 + (Y*X1-Y1*X)/(Y-Y1)
                     CALL PLOTR (-Y2, X2, NDAS)
                     ND = 3
               END IF
         END IF
         X1 = X
         Y1 = Y
 100  CONTINUE
      CALL NEWPEN (N1)
      CALL PLOTR (0.0, -10.0*SIZE, NM3)
      ADD = .TRUE.
      RETURN
      END
C
      SUBROUTINE FRAMEP
CD FRAMEP       Plots the frame for COMPLOT
C
C     +---------------+
C     !  F R A M E P  !
C     +---------------+
C
C     Author: Arnold Rots        Date: 13 May 1983
C
C     Version: 1.0               Language: F77
C
C     Description:
C       This routine does the frame plotting (with annotation) for
C       the COMPLOT package; it is called by GRAFPL.
C
C     Calling sequence:
C       CALL FRAMEP
C
C     Parameters:
C       None
C
C     Included files:
C       None
C
C     Externals:
C       PLOTR
C       SYMBLR
C       NUMBRR
C
C     Commons:
C       GRAF
C       GRAF1
C
C     Revisions:
CE
C-----------------------------------------------------------------------
C
      REAL   X, Y, X1, Y1, X2, Y2, XM, YM, DM, Z, XL, YL, XL1, YL1, ANG
      REAL   X999
      INTEGER   NM3, NM1, N1, N2, N3, N5, N8, NI, ND, IM, IMX, IMY
C
C                                      GRAF
      REAL   CSIZE, XSIZE, YSIZE, XLAB(2), YLAB(2), MSIZE
      INTEGER   NP, NCHAR, NPEN, NANG, NDASH
      COMMON /GRAF/ NP, NANG, CSIZE, XSIZE, YSIZE, XLAB, YLAB,
     +              NCHAR, NPEN, NDASH, MSIZE
C                                      End GRAF
C
C
C                                      GRAF1
      REAL   XMIN, XMAX, YMIN, YMAX, DX, DY, X0, Y0
      INTEGER   NDEV
      LOGICAL   ADD
      COMMON /GRAF1/ XMIN, XMAX, YMIN, YMAX, DX, DY, X0, Y0, ADD, NDEV
C                                      End GRAF1
C
C
      DATA NM3, NM1, N1, N2, N3, N5, N8 /-3, -1, 1, 2, 3, 5, 8/
      DATA X999 /999.0/
C
C-----------------------------------------------------------------------
C                                      Frame
      CALL PLOTR (0.0, XSIZE, N2)
      CALL PLOTR (-YSIZE, XSIZE, N2)
      CALL PLOTR (-YSIZE, 0.0, N2)
      CALL PLOTR (0.0, 0.0, N2)
      IF (((YMIN .LT. 0.0) .AND. (YMAX .GT. 0.0)) .OR.
     +    ((YMIN .GT. 0.0) .AND. (YMAX .LT. 0.0))) THEN
         CALL PLOTR (Y0, 0.0, N3)
         CALL PLOTR (Y0, XSIZE, N2)
      END IF
C                                      X axis tick marks
      XM = ABS(XMAX-XMIN) / XSIZE * (7.0*CSIZE)
      DM = LOG10 (ABS(XM))
      IM = INT(DM)
      IF (DM .LT. 0.0) IM = IM - 1
      XM = XM * 10.0**(-IM)
      IF (XM .LT. 2.0) THEN
            XM = 2.0 * 10.0**IM
         ELSE IF (XM .LT. 5.0) THEN
            XM = 5.0 * 10.0**IM
         ELSE
            XM = 10.0**(IM+1)
      END IF
      X = MAX(ABS(XMAX),ABS(XMIN))
      DM = LOG10 (X)
      IM = INT(DM)
      IF (DM .LT. 0.0) THEN
            IM = ((IM/3) - 1) * 3
         ELSE
            IM = (IM/3) * 3
      END IF
      IF (X .LT. 9999.5) IM = MIN(IM,0)
      IMX = IM
      DM = 10.0**IM
      X = ABS(DM/XM)
      ND = LOG10 (X) + 1.0
      IF (X .LE. 1.0) ND = -1
      IF (XMAX .LT. XMIN) XM = -XM
      XL1 = REAL(INT(ABS(XMIN/XM))) * XM
      IF ((XMIN / XM) .LT. 0.0) XL1 = -XL1
      IF ((XL1 / XM) .LT. (XMIN / XM)) XL1 = XL1 + XM
      XL = XL1
C                                      Bottom tick marks
      Y = 0.0
      Y1 = CSIZE
      Y2 = -2.0 * CSIZE
 200  CONTINUE
         X = XL * DX - X0
         Z = XL / DM
         IF (ABS(Z) .GE. 1000.0) THEN
               NI = 5
            ELSE IF (ABS(Z) .GE. 100.0) THEN
               NI = 4
            ELSE IF (ABS(Z) .GE. 10.0) THEN
               NI = 3
            ELSE
               NI = 2
         END IF
         IF (Z .LT. 0.0) NI = NI + 1
         X1 = X - 0.5 * (ND + NI) * CSIZE
         CALL PLOTR (-Y, X, N3)
         CALL PLOTR (-Y1, X, N2)
         CALL NUMBRR (-Y2, X1, CSIZE, Z, ANG, ND)
         XL = XL + XM
         IF (((XM/ABS(XM)) * (XMAX-XL)) .GE. 0.0) GOTO 200
C                                      Top tick marks
      XL = XL1
      Y = YSIZE
      Y1 = Y - CSIZE
 210  CONTINUE
         X = XL * DX - X0
         Z = XL / DM
         CALL PLOTR (-Y, X, N3)
         CALL PLOTR (-Y1, X, N2)
         XL = XL + XM
         IF (((XM/ABS(XM)) * (XMAX-XL)) .GE. 0.0) GOTO 210
C                                      Y axis tick marks
      YM = ABS(YMAX-YMIN) / YSIZE * (7.0*CSIZE)
      DM = LOG10 (ABS(YM))
      IM = INT(DM)
      IF (DM .LT. 0.0) IM = IM - 1
      YM = YM * 10.0**(-IM)
      IF (YM .LT. 2.0) THEN
            YM = 2.0 * 10.0**IM
         ELSE IF (YM .LT. 5.0) THEN
            YM = 5.0 * 10.0**IM
         ELSE
            YM = 10.0**(IM+1)
      END IF
      Y = MAX(ABS(YMAX),ABS(YMIN))
      DM = LOG10 (Y)
      IM = INT(DM)
      IF (DM .LT. 0.0) THEN
            IM = ((IM/3) - 1) * 3
         ELSE
            IM = (IM/3) * 3
      END IF
      IF (Y .LT. 9999.5) IM = MIN(IM,0)
      IMY = IM
      DM = 10.0**IM
      Y = ABS(DM/YM)
      ND = LOG10 (Y) + 1.0
      IF (Y .LE. 1.0) ND = -1
      IF (YMAX .LT. YMIN) YM = -YM
      YL1 = REAL(INT(ABS(YMIN/YM))) * YM
      IF ((YMIN / YM) .LT. 0.0) YL1 = -YL1
      IF ((YL1 / YM) .LT. (YMIN / YM)) YL1 = YL1 + YM
      YL = YL1
C                                      Left tick marks
      X = 0.0
      X1 = CSIZE
      X2 = -8.0 * CSIZE
 300  CONTINUE
         Y = YL * DY - Y0
         Y1 = Y - 0.5 * CSIZE
         Z = YL / DM
         IF (ABS(Z) .GE. 1000.0) THEN
               NI = 6
            ELSE IF (ABS(Z) .GE. 100.0) THEN
               NI = 5
            ELSE IF (ABS(Z) .GE. 10.0) THEN
               NI = 4
            ELSE
               NI = 3
         END IF
         IF (Z .LT. 0.0) NI = NI + 1
         X2 = -(ND + NI) * CSIZE
         CALL PLOTR (-Y, X, N3)
         CALL PLOTR (-Y, X1, N2)
         CALL NUMBRR (-Y1, X2, CSIZE, Z, ANG, ND)
         YL = YL + YM
         IF (((YM/ABS(YM)) * (YMAX-YL)) .GE. 0.0) GOTO 300
C                                      Right tick marks
      YL = YL1
      X = XSIZE
      X1 = X - CSIZE
 310  CONTINUE
         Y = YL * DY - Y0
         Z = YL / DM
         CALL PLOTR (-Y, X, N3)
         CALL PLOTR (-Y, X1, N2)
         YL = YL + YM
         IF (((YM/ABS(YM)) * (YMAX-YL)) .GE. 0.0) GOTO 310
C                                      X axis label
      Y = -5.0 * CSIZE
      X = 0.5 * XSIZE - 8.0 * CSIZE
      Y1 = Y + 0.6 * CSIZE
      CALL SYMBLR (-Y, X, CSIZE, XLAB, ANG, N8)
      IF (IMX .NE. 0) THEN
         CALL SYMBLR (Y, X999, CSIZE, ' (*10', ANG, N5)
         Z = IMX
         CALL NUMBRR (Y1, X999, 0.6*CSIZE, Z, ANG, NM1)
         CALL SYMBLR (Y, X999, CSIZE, ')', ANG, N1)
      END IF
C                                      Y axis label
      X = -8.8 * CSIZE
      Y = 0.5 * YSIZE - 8.0 * CSIZE
      X1 = X - 0.6 * CSIZE
      IF (NANG .EQ. 0) THEN
            CALL SYMBOL (X, Y, CSIZE, YLAB, 90.0, N8)
            IF (IMY .NE. 0) THEN
               CALL SYMBOL (X, X999, CSIZE, ' (*10', 90.0, N5)
               Z = IMY
               CALL NUMBER (X1, X999, 0.6*CSIZE, Z, 90.0, NM1)
               CALL SYMBOL (X, X999, CSIZE, ')', 90.0, N1)
            END IF
         ELSE
            CALL SYMBOL (-Y, X, CSIZE, YLAB, 180.0, N8)
            IF (IMY .NE. 0) THEN
               CALL SYMBOL (X999, X, CSIZE, ' (*10', 180.0, N5)
               Z = IMY
               CALL NUMBER (X999, X1, 0.6*CSIZE, Z, 180.0, NM1)
               CALL SYMBOL (X999, X, CSIZE, ')', 180.0, N1)
            END IF
      END IF
C                                      Exit
      RETURN
      END
C
      SUBROUTINE PLOTR (Y, X, I)
CD PLOTR        Calls PLOT for COMPLOT, taking the angle into account
C
C     +-------------+
C     !  P L O T R  !
C     +-------------+
C
C     Author: Arnold Rots        Date: 13 May 1983
C
C     Version: 1.0               Language: F77
C
C     Description:
C       This routine calls PLOT with X and Y interchanged if the angle
C       is 0 degrees; it is part of COMPLOT
C
C     Calling sequence:
C       CALL FRAMEP (Y, X, I)
C
C     Parameters:
C       Y     R       Plot position along plotter X-axis
C       X     R       Plot position along plotter Y-axis
C       I     I       Pen code (up, down, etc.)
C
C     Included files:
C       None
C
C     Externals:
C       PLOT
C
C     Commons:
C       GRAF
C
C     Revisions:
CE
C-----------------------------------------------------------------------
      REAL   X, Y, Y1
      INTEGER   I, J, NANG
      COMMON /GRAF/ J, NANG
C-----------------------------------------------------------------------
      IF (NANG .EQ.0) THEN
            Y1 = -Y
            IF (Y .EQ. 999.0) Y1 = Y
            CALL PLOT (X, Y1, I)
         ELSE
            CALL PLOT (Y, X, I)
      END IF
      RETURN
      END
C
      SUBROUTINE SYMBLR (Y, X, S, T, Z, I)
CD SYMBLR       Calls SYMBOL for COMPLOT correcting for the plot angle
C
C     +---------------+
C     !  S Y M B L R  !
C     +---------------+
C
C     Author: Arnold Rots        Date: 13 May 1983
C
C     Version: 1.0               Language: F77
C
C     Description:
C       This routine calls SYMBOL with the X and Y axes interchanged
C       if the plot angle is 0 degrees; it is part of COMPLOT.
C
C     Calling sequence:
C       CALL SYMBLR (Y, X, S, T, Z, I)
C
C     Parameters:
C       Y      R        Plot position along plotter X axis
C       X      R        Plot position along plotter Y axis
C       S      R        Character size (in cm)
C       T       ?       Text string or marker symbol
C       Z      R        Dummy; present for compatibility
C       I      I        Length of T (see SYMBOL)
C
C     Included files:
C       None
C
C     Externals:
C       SYMBOL
C
C     Commons:
C       GRAF
C
C     Revisions:
CE
C-----------------------------------------------------------------------
      REAL   X, Y, S, T, Z, ANG0, ANG90, Y1
      INTEGER   I, J, NANG
      COMMON /GRAF/ J, NANG
      DATA ANG0, ANG90 /0.0, 90.0/
C-----------------------------------------------------------------------
      IF (NANG .EQ. 0) THEN
            Y1 = -Y
            IF (Y .EQ. 999.0) Y1 = Y
            CALL SYMBOL (X, Y1, S, T, ANG0, I)
         ELSE
            CALL SYMBOL (Y, X, S, T, ANG90, I)
      END IF
      RETURN
      END
C
      SUBROUTINE NUMBRR (Y, X, S, T, Z, I)
CD NUMBRR       Calls NUMBER for COMPLOT correcting for plot angle
C
C     +---------------+
C     !  N U M B R R  !
C     +---------------+
C
C     Author: Arnold Rots        Date: 13 May 1983
C
C     Version: 1.0               Language: F77
C
C     Description:
C       This routine calls NUMBER and interchanges the X- and Y-axes
C       if the plotting angle is 0 degrees; it is part of COMPLOT.
C
C     Calling sequence:
C       CALL NUMBRR (Y, X, S, T, Z, I)
C
C     Parameters:
C       Y     R         Plot position along plotter X axis
C       X     R         Plot position along plotter Y axis
C       S     R         Size of characters (in cm)
C       T     R         Number to be plotted
C       Z     R         Dummy; present for compatibility
C       I     I         Number of digits behind decimal point
C
C     Included files:
C       None
C
C     Externals:
C       NUMBER
C
C     Commons:
C       GRAF
C
C     Revisions:
CE
C-----------------------------------------------------------------------
      REAL   X, Y, S, T, Z, ANG0, ANG90, Y1
      INTEGER   I, J, NANG
      COMMON /GRAF/ J, NANG
      DATA ANG0, ANG90 /0.0, 90.0/
C-----------------------------------------------------------------------
      IF (NANG .EQ. 0) THEN
            Y1 = -Y
            IF (Y .EQ. 999.0) Y1 = Y
            CALL NUMBER (X, Y1, S, T, ANG0, I)
         ELSE
            CALL NUMBER (Y, X, S, T, ANG90, I)
      END IF
      RETURN
      END
C
      SUBROUTINE NEXTPH (SPACE)
CD NEXTPH       Moves plotter origin horizontally for next plot
C
C     +---------------+
C     !  N E X T P H  !
C     +---------------+
C
C     Author: Arnold Rots        Date: 13 May 1983
C
C     Version: 1.0               Language: F77
C
C     Description:
C       This routine moves the plotter origin horizontally by SPACE
C       for the next plot.  If the plotting angle is 90 degrees,
C       YSIZE will be subtracted from SPACE and the origin ends up
C       at the bottom of the paper; if the angle is 0 degrees, it
C       ends up at the top.  It is part of COMPLOT.
C
C     Calling sequence:
C       CALL NEXTPH (SPACE)
C
C     Parameters:
C       SPACE    R        Distance (in cm) between the top (or left
C                         side) of the last plot and the top (or
C                         left side) of the next plot
C
C     Included files:
C       None
C
C     Externals:
C       PLOT
C
C     Commons:
C       GRAF
C       GRAF1
C
C     Revisions:
CE
C-----------------------------------------------------------------------
      REAL   SPACE, X
      INTEGER   NM3, N3
C
C                                      GRAF
      REAL   CSIZE, XSIZE, YSIZE, XLAB(2), YLAB(2), MSIZE
      INTEGER   NP, NCHAR, NPEN, NANG, NDASH
      COMMON /GRAF/ NP, NANG, CSIZE, XSIZE, YSIZE, XLAB, YLAB,
     +              NCHAR, NPEN, NDASH, MSIZE
C                                      End GRAF
C
C
C                                      GRAF1
      REAL   XMIN, XMAX, YMIN, YMAX, DX, DY, X0, Y0
      INTEGER   NDEV
      LOGICAL   ADD
      COMMON /GRAF1/ XMIN, XMAX, YMIN, YMAX, DX, DY, X0, Y0, ADD, NDEV
C                                      End GRAF1
C
      DATA NM3, N3 /-3, 3/
C-----------------------------------------------------------------------
C
      X = SPACE
      IF (NANG .NE. 0) X = X - YSIZE
      CALL PLOT (X, 0.0, NM3)
      CALL PLOT (0.0, -30.0, N3)
      CALL PLOT (0.0, -29.7, NM3)
      IF (NANG .EQ. 0) CALL PLOT (0.0, 27.0, NM3)
      ADD = .FALSE.
C
      RETURN
      END
C
      SUBROUTINE NEXTPV (SPACE)
CD NEXTPV       Moves plotter origin vertically for next plot
C
C     +---------------+
C     !  N E X T P V  !
C     +---------------+
C
C     Author: Arnold Rots        Date: 13 May 1983
C
C     Version: 1.0               Language: F77
C
C     Description:
C       This routine moves the plotter origin vertically by SPACE
C       for the next plot.  If the plotting angle is 90 degrees,
C       the origin will be moved up by SPACE and to the left by
C       YSIZE; if the angle is 0 degrees, it will be moved down
C       by SPACE - YSIZE.  It is part of COMPLOT.
C
C     Calling sequence:
C       CALL NEXTPV (SPACE)
C
C     Parameters:
C       SPACE    R        Distance (in cm) between the top (or left
C                         side) of the last plot and the top (or
C                         left side) of the next plot
C
C     Included files:
C       None
C
C     Externals:
C       PLOT
C
C     Commons:
C       GRAF
C       GRAF1
C
C     Revisions:
CE
C-----------------------------------------------------------------------
      REAL   SPACE, X, Y
      INTEGER   NM3, N3
C
C                                      GRAF
      REAL   CSIZE, XSIZE, YSIZE, XLAB(2), YLAB(2), MSIZE
      INTEGER   NP, NCHAR, NPEN, NANG, NDASH
      COMMON /GRAF/ NP, NANG, CSIZE, XSIZE, YSIZE, XLAB, YLAB,
     +              NCHAR, NPEN, NDASH, MSIZE
C                                      End GRAF
C
C
C                                      GRAF1
      REAL   XMIN, XMAX, YMIN, YMAX, DX, DY, X0, Y0
      INTEGER   NDEV
      LOGICAL   ADD
      COMMON /GRAF1/ XMIN, XMAX, YMIN, YMAX, DX, DY, X0, Y0, ADD, NDEV
C                                      End GRAF1
C
      DATA NM3, N3 /-3, 3/
C-----------------------------------------------------------------------
C
      IF (NANG .EQ. 0) THEN
            Y = YSIZE - SPACE
            X = 0.0
         ELSE
            Y = SPACE
            X = -YSIZE
      END IF
      CALL PLOT (X, Y, NM3)
      ADD = .FALSE.
C
      RETURN
      END
C
      SUBROUTINE CLSPLT (SPACE)
CD CLSPLT       Closes a COMPLOT plot
C
C     +---------------+
C     !  C L S P L T  !
C     +---------------+
C
C     Author: Arnold Rots        Date: 13 May 1983
C
C     Version: 1.0               Language: F77
C
C     Description:
C       This routine moves the plotter origin horizontally by SPACE
C       and stops the plotter.  It is part of COMPLOT.
C
C     Calling sequence:
C       CALL CLSPLT (SPACE)
C
C     Parameters:
C       SPACE    R        Distance over which to move the pen to
C                         the right before closing the plot
C
C     Included files:
C       None
C
C     Externals:
C       PLOT
C
C     Commons:
C       GRAF
C       GRAF1
C
C     Revisions:
CE
C-----------------------------------------------------------------------
      REAL   SPACE, X
      INTEGER   N999
C
C                                      GRAF
      REAL   CSIZE, XSIZE, YSIZE, XLAB(2), YLAB(2), MSIZE
      INTEGER   NP, NCHAR, NPEN, NANG, NDASH
      COMMON /GRAF/ NP, NANG, CSIZE, XSIZE, YSIZE, XLAB, YLAB,
     +              NCHAR, NPEN, NDASH, MSIZE
C                                      End GRAF
C
C
C                                      GRAF1
      REAL   XMIN, XMAX, YMIN, YMAX, DX, DY, X0, Y0
      INTEGER   NDEV
      LOGICAL   ADD
      COMMON /GRAF1/ XMIN, XMAX, YMIN, YMAX, DX, DY, X0, Y0, ADD, NDEV
C                                      End GRAF1
C
      DATA N999 /999/
C-----------------------------------------------------------------------
C
      X = SPACE
      CALL PLOT (X, 0.0, N999)
C
      RETURN
      END
C
      SUBROUTINE CLSPLTA (SPACE)
CD CLSPLTA      Closes a COMPLOT plot in AIPS
C
C     +-----------------+
C     !  C L S P L T A  !
C     +-----------------+
C
C     Author: Arnold Rots        Date: 13 May 1983
C
C     Version: 1.0               Language: F77
C
C     Description:
C       This routine moves the plotter origin horizontally by SPACE
C       and stops the plotter.  It is part of COMPLOT.
C       It is the AIPS version of CLSPLT and closes the spool file.
C
C     Calling sequence:
C       CALL CLSPLTA (SPACE)
C
C     Parameters:
C       SPACE    R        Distance over which to move the pen to
C                         the right before closing the plot
C
C     Included files:
C       None
C
C     Externals:
C       PLOT
C
C     Commons:
C       GRAF
C       GRAF1
C
C     Revisions:
CE
C-----------------------------------------------------------------------
      REAL   SPACE, X
      INTEGER   N999
C
C                                      GRAF
      REAL   CSIZE, XSIZE, YSIZE, XLAB(2), YLAB(2), MSIZE
      INTEGER   NP, NCHAR, NPEN, NANG, NDASH
      COMMON /GRAF/ NP, NANG, CSIZE, XSIZE, YSIZE, XLAB, YLAB,
     +              NCHAR, NPEN, NDASH, MSIZE
C                                      End GRAF
C
C
C                                      GRAF1
      REAL   XMIN, XMAX, YMIN, YMAX, DX, DY, X0, Y0
      INTEGER   NDEV
      LOGICAL   ADD
      COMMON /GRAF1/ XMIN, XMAX, YMIN, YMAX, DX, DY, X0, Y0, ADD, NDEV
C                                      End GRAF1
C
      DATA N999 /999/
C-----------------------------------------------------------------------
C
      X = SPACE
      CALL PLOT (X, 0.0, N999)
      CLOSE (UNIT=NDEV)
C
      RETURN
      END
