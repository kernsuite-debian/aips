LOCAL INCLUDE 'MANDL.INC'
C                                       Local Include for MANDL
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   SEQOUT, DISKO, NEWCNO, NUMHIS, JBUFSZ, ICODE
      HOLLERITH XSOURC(2), XNAMOU(3), XCLAOU(2)
      CHARACTER SOURCE*8, NAMOUT*12, CLAOUT*6, HISCRD(10)*64
      REAL      XMSIZE(2), CELLS(2), XSEQO, XDISKO,
     *   CPARM(10), BUFFER(MABFSS)
      COMMON /INPARM/ XSOURC, XMSIZE, CELLS, XNAMOU, XCLAOU, XSEQO,
     *   XDISKO, CPARM
      COMMON /PARMS/ SEQOUT, DISKO, NEWCNO, JBUFSZ, ICODE, NUMHIS
      COMMON /BUFRS/ BUFFER
      COMMON /CHRCOM/ SOURCE, NAMOUT, CLAOUT, HISCRD
LOCAL END
      PROGRAM MANDL
C-----------------------------------------------------------------------
C! Makes an image of a portion of the Mandelbrot set
C# Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000, 2015
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
C   MANDL Creates a setion of the Mandelbrot set.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      SOURCE         SOURCE        Object name.
C      IMSIZE         XMSIZE        Image size in pixels.
C      CELLSIZE       CELLS         Pixel size in first two dim.
C      OUTNAME        NAMOUT        Name of the output image
C                                   Default output is input image.
C      OUTCLASS       CLAOUT        Class of the output image.
C                                   Default is input class.
C      OUTSEQ         SEQOUT        Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      CPARM(10)      CPARM         User specified array.
C                                   1:Xmin;2:Ymin;3:Xmax;4:Ymax
C                                   5:Max Value for Set (0=> 256)
C   Programmer Glen Langston, October 1987
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'MANDL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'MANDL '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL MANIN (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL SENDMA (IRET)
C                                       History
      IF (IRET.EQ.0) CALL CANHIS
C                                       Close down files, etc.
      CALL DIE (IRET, BUFFER)
C
      STOP
      END
      SUBROUTINE MANIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   MANIN gets input parameters for MANDL and creates an output file.
C   Inputs:  PRGN    C*6       Program name
C   Output:  IRET    I         Error code: 0 => ok
C                                4 => user routine detected error.
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in MANDL for more details.
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, OLDNAM*12, BLANK*6
      INTEGER   IERR, NPARM, IROUND
      LOGICAL   T, F
      INCLUDE 'MANDL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA BLANK /'      '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSS
      IRET = 0
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
C     NPARM = 2 + 2 + 2 + 3 + 2 + 1 + 1+ 10 = 23
      NPARM = 23
      CALL GTPARM (PRGN, NPARM, RQUICK, XSOURC, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (IRET, BUFFER, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQOUT = IROUND (XSEQO)
      DISKO = IROUND (XDISKO)
      CALL H2CHR (8, 1, XSOURC, SOURCE)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C                                       Init CATBLK.
      CALL CATINI (CATBLK)
C                                       Create new file.
C                                       Put values in CATBLK.
      OLDNAM = 'MANDLELBROT '
      CALL MAKOUT (OLDNAM, BLANK, 0, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = SEQOUT
C                                       Get user modification to CATBLK
      IRET = 4
      CALL NEWHED (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create output file.
      NEWCNO = 1
      IRET = 4
      CALL MCREAT (DISKO, NEWCNO, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 70
         WRITE (MSGTXT,1060) IERR
         GO TO 990
 70   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 2
      IRET = 0
      SEQOUT = CATBLK(KIIMS)
C                                       Set obs. date=current date.
      CALL CHCOPY (8, 1, CATH(KHDMP), 1, CATH(KHDOB))
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MANIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1060 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE SENDMA (IRET)
C-----------------------------------------------------------------------
C   SENDMA accepts an image one row at a time from the user supplied
C   routine.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER IFILE*48
      INTEGER   IRET, LUNO,
     *   NXO, NYO, WINO(4), BOO, LIM2, LIM3, LIM4,
     *   LIM5, LIM6, LIM7, I1, I2, I3, I4, I5, I6, I7, IPOS(7),
     *   LIMO, LIMIT, OBIND, LUN1, LUN2, INDO, LIM1
      REAL      OUTMAX, OUTMIN
      LOGICAL   T, F, BLNKD
      INCLUDE 'MANDL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNO, LUN1, LUN2 /16,17,18/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Create scratch file.
C                                       Open vis file for write
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, IFILE, T, F, F, IRET)
      IF (IRET.LE.0) GO TO 30
         WRITE (MSGTXT,1020) IRET
         GO TO 990
C                                       Setup for I/O
 30   NXO = CATBLK(KINAX)
      NYO = CATBLK(KINAX+1)
      WINO(1) = 1
      WINO(2) = 1
      WINO(3) = NXO
      WINO(4) = NYO
      OUTMAX = -1.0E20
      OUTMIN = 1.0E20
      BLNKD = F
C                                       Setup for looping
      LIM1 = MAX (1, CATBLK(KINAX))
      LIM2 = MAX (1, CATBLK(KINAX+1))
      LIM3 = MAX (1, CATBLK(KINAX+2))
      LIM4 = MAX (1, CATBLK(KINAX+3))
      LIM5 = MAX (1, CATBLK(KINAX+4))
      LIM6 = MAX (1, CATBLK(KINAX+5))
      LIM7 = MAX (1, CATBLK(KINAX+6))
      IPOS(1) = 1
      LIMO = CATBLK(KINAX) - 1
C                                       Loop
      DO 700 I7 = 1,LIM7
         IPOS(7) = I7
         DO 600 I6 = 1,LIM6
            IPOS(6) = I6
            DO 500 I5 = 1,LIM5
               IPOS(5) = I5
               DO 400 I4 = 1,LIM4
                  IPOS(4) = I4
                  DO 300 I3 = 1,LIM3
      IPOS(3) = I3
C                                       Init output file.
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IPOS(3), BOO, IRET)
      BOO = BOO + 1
      CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFFER, JBUFSZ,
     *   BOO, IRET)
      IF (IRET.EQ.0) GO TO 120
         WRITE (MSGTXT,1100) 'WRIT', IRET
         GO TO 990
 120  DO 250 I2 = 1,LIM2
         IPOS(2) = I2
C                                       Write.
         CALL MDISK ('WRIT', LUNO, INDO, BUFFER, OBIND, IRET)
         OBIND = OBIND - 1
         IF (IRET.EQ.0) GO TO 180
            WRITE (MSGTXT,1120) 'WRIT', IRET
            GO TO 990
C                                       Call MAKMAP
 180     OBIND = OBIND + 1
         CALL MAKMAP (IPOS, BUFFER(OBIND), IRET)
         IF (IRET.EQ.0) GO TO 190
            WRITE (MSGTXT,1180) IRET
            GO TO 990
C                                       Check max, min, blanking.
 190     LIMIT = OBIND + LIMO
         DO 200 I1 = OBIND,LIMIT
            BLNKD = BLNKD .OR. (BUFFER(I1).EQ.FBLANK)
            IF (BUFFER(I1).EQ.FBLANK) GO TO 200
               OUTMAX = MAX (OUTMAX, BUFFER(I1))
               OUTMIN = MIN (OUTMIN, BUFFER(I1))
 200           CONTINUE
 250     CONTINUE
C                                       Dump plane to output.
C                                       Flush buffer.
      CALL MDISK ('FINI', LUNO, INDO, BUFFER, OBIND, IRET)
      IF (IRET.EQ.0) GO TO 260
         WRITE (MSGTXT,1120) 'FINI', IRET
         GO TO 990
C                                       Update CATBLK.
 260  CATR(KRDMX) = OUTMAX
      CATR(KRDMN) = OUTMIN
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', BUFFER, IRET)
      IF (IRET.EQ.0) GO TO 300
         WRITE (MSGTXT,1260) IRET
         GO TO 990
 300  CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Mark blanking in CATBLK.
      CATR(KRBLK) = 0.0
      IF (BLNKD) CATR(KRBLK) = FBLANK
C                                       Final call to MAKMAP
      IPOS(1) = -1
      CALL MAKMAP (IPOS, BUFFER, IRET)
      IF (IRET.EQ.0) GO TO 710
         WRITE (MSGTXT,1180) IRET
         GO TO 990
 710  CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('SENDMA: ERROR',I3,' OPENING SCRATCH FILE')
 1100 FORMAT ('SENDMA: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('SENDMA: ',A4,' ERROR',I3)
 1180 FORMAT ('SENDMA: MAKMAP ERROR',I3)
 1260 FORMAT ('SENDMA: CATIO ERROR',I3,' UPDATING CATBLK')
      END
      SUBROUTINE CANHIS
C-----------------------------------------------------------------------
C   CANHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, ATIME*8, ADATE*12, LABEL*8
      INTEGER   LUN, IERR, I, TIME(3), DATE(3)
      LOGICAL   T, F
      INCLUDE 'MANDL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN /27/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Create/open hist. file.
      CALL HICREA (LUN, DISKO, NEWCNO, CATBLK, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
C                                       Get current date/time.
 10   CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
C                                       Write first record.
      WRITE (MSGTXT,1010) TSKNAM, NLUSER, ADATE, ATIME
      HILINE = MSGTXT
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       New history
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN,
     *   BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       SOURCE
      WRITE (HILINE,2000) TSKNAM, SOURCE
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       IMSIZE
      WRITE (HILINE,2001) TSKNAM, XMSIZE
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       CELLSIZE
      WRITE (HILINE,2002) TSKNAM, CELLS
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Area
      WRITE (HILINE,2003) TSKNAM, CPARM(1), CPARM(2),CPARM(3),CPARM(4)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       AIPS release
      WRITE (HILINE,2004) TSKNAM, RLSNAM
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      Add any user supplied history.
      IF (NUMHIS.LE.0) GO TO 20
         WRITE (LABEL,1011) TSKNAM
         DO 15 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 20
 15         CONTINUE
C                                       Close HI file
 20   CALL HICLOS (LUN, T, BUFFER, IERR)
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CANHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ Image created by user',I5,' at ',A12,2X,A8)
 1011 FORMAT (A6,' /')
 2000 FORMAT (A6,' SOURCE = ',1H',A8,1H')
 2001 FORMAT (A6,' IMSIZE = ', 2F8.0)
 2002 FORMAT (A6,' CELLSIZE = ',2F10.5)
 2003 FORMAT (A6,' Min = ( ',F8.5,',',F8.5,'); Max = ( ',
     *                        F8.5,',',F8.5,')')
 2004 FORMAT (A6,' RELEASE = ''',A7,' ''')
      END
      SUBROUTINE NEWHED (IRET)
C-----------------------------------------------------------------------
C   NEWHED is a routine in which the user performs several operations
C   associated with beginning the task.
C   Input:
C      CATBLK(256)    I     Output catalog header, also CATR, CATD, CATH
C                           The OUTNAME, OUTCLASS, OUTSEQ are entered
C                           elsewhere.
C   Output:
C      CATBLK(256)    I     Modified output catalog header.
C      IRET           I     Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER FCHARS(3)*4, UNITS(10)*8, ATYPES(7)*8
      INTEGER   I, NAXIS, IRET, IROUND, NX, NY, INDEX, MAXVAL
      REAL      XMIN, YMIN, XMAX, YMAX
      REAL      XRANGE, YRANGE, XSCALE, YSCALE
      INCLUDE 'MANDL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA FCHARS /'FREQ','VELO','FELO'/
C                                       Output units for each OPCODE.
      DATA UNITS /'COUNTS  ',9*'        '/
C                                       Number of axes and types.
C                                       (Set for two axes = Ra, Dec.)
      DATA NAXIS  /2/
      DATA ATYPES /' X      ', ' Y      ', 'STOKES  ', 'FREQ    ',
     *   3*'        '/
C-----------------------------------------------------------------------
C                                       Set default OPCODE
      ICODE = 1
C                                       Set output units.
      CALL CHR2H (8, UNITS(ICODE), 1, CATH(KHBUN))
C                                       Fill axis type from ATYPES
      DO 30 I = 1,NAXIS
         INDEX = KHCTP + (I-1) * 2
         CALL CHR2H (8, ATYPES(I), 1, CATH(INDEX))
 30      CONTINUE
C
C                                       Fill in values.
      IF (XMSIZE(1).LT.2) XMSIZE(1) = 256
      IF (XMSIZE(2).LT.2) XMSIZE(2) = 256
      CATBLK(KINAX) = MAX (IROUND (XMSIZE(1)), 1)
      CATBLK(KINAX+1) = MAX (IROUND (XMSIZE(2)), 1)
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX+1)
C                                       Calculate mandlebort specific
C                                       Values, scale etc
C                                       RANGEs must be greater than zero
      XMIN = CPARM(1)
      YMIN = CPARM(2)
      XMAX = CPARM(3)
      YMAX = CPARM(4)
      IF (XMAX-XMIN.EQ.0.0) CPARM(1) = -2.
      IF (XMAX-XMIN.EQ.0.0) CPARM(3) =  1.
      IF (YMAX-YMIN.EQ.0.0) CPARM(2) = -1.5
      IF (YMAX-YMIN.EQ.0.0) CPARM(4) =  1.5
      XMIN = CPARM(1)
      YMIN = CPARM(2)
      XMAX = CPARM(3)
      YMAX = CPARM(4)
C                                       IF YMAX not Set, make a square
      IF (YMAX.EQ.0) YMAX = YMIN + XMAX - XMIN
      CPARM(4) = YMAX
C                                       Max Number of iterations must be
C                                       greater than 1 and less than i*2
      MAXVAL = CPARM(5)
      IF (MAXVAL.LT.2) MAXVAL = 255
      CPARM(5) = MAXVAL
C                                       Calculate cell size from max
C                                       and min use the smaller cell
C                                       size.
      XRANGE = XMAX - XMIN
      YRANGE = YMAX - YMIN
      XSCALE = 3 / 256.
      YSCALE = 3 / 256.
      IF (XRANGE.NE.0.) XSCALE = XRANGE / NX
      IF (YRANGE.NE.0.) YSCALE = YRANGE / NY
C                                       if cells is not specified
C                                       use calculation from range
      IF (CELLS(1).EQ.0) CELLS(1) = XSCALE
      IF (CELLS(2).EQ.0) CELLS(2) = YSCALE
C                                       Subsequent calls.
      CATR(KRCIC) = CELLS(1)
      CATR(KRCIC+1) = CELLS(2)
C                                       Init. ref value for pixel
C                                       location.
      CATD(KDCRV) =   ((XMAX+XMIN)/2.) - (CELLS(1)*NX/2.)
      CATD(KDCRV+1) = ((YMAX+YMIN)/2.) - (CELLS(2)*NY/2.)
C                                       Fill other character strings.
C                                       Object.
      CALL CHR2H (8, SOURCE, 1, CATH(KHOBJ))
C                                       Set number of axes.
      CATBLK(KIDIM) = NAXIS
C                                       Miscellaneous items.
C                                       Epoch.
      CATR(KREPO) = 0.0
C                                       Finished.
      IRET = 0
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE MAKMAP (IPOS, RESULT, IRET)
C-----------------------------------------------------------------------
C   Returns Mandelbrot image.
C   Inputs:
C      IPOS(7)   I    BLC (input image) of first value in DATA
C   Values from commons:
C      ICODE     I    Opcode number from list in NEWHED.
C      CPARM(10) R    Input adverb array.
C      DPARM(10) R    Input adverb array.
C      CATBLK    I    Output catalog header (also CATR, CATD)
C   Output:
C      RESULT(*) R    Output row.
C      IRET      I    Return code   0 => OK
C                               >0 => error, terminate.
C   Output in COMMON
C      NUMHIS     I    # history entries (max. 10)
C      HISCRD(NUMHIS) C*64   History records
C      CATBLK     I    Catalog header block
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), NX, I, IRET
      REAL      RESULT(*)
      REAL   XMIN, YMIN, XSCALE, YSCALE, X,Y
      INTEGER   M, MAXVAL
      INCLUDE 'MANDL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check if last call
      IF (IPOS(1).LT.0) GO TO 999
C                                       Set length of a row.
      NX = CATBLK(KINAX)
      XMIN = CPARM(1)
      YMIN = CPARM(2)
      XSCALE = CELLS(1)
      YSCALE = CELLS(2)
      MAXVAL = CPARM(5)
C                                       Subsequent calls.
C                                       Calc Y coordinate for this row
      Y = (YSCALE*IPOS(2)) + YMIN
C                                       Create map row by calling MAND
      DO 300 I = 1,NX
         X = (XSCALE*I) + XMIN
         CALL MAND (X, Y, MAXVAL, M)
         RESULT(I) = M
 300  CONTINUE
C
 999  RETURN
      END
      SUBROUTINE MAND (X, Y, MAXVAL, KOUNT)
C-----------------------------------------------------------------------
C   Calculates the mandelbrot integer for the point x,y in the plane.
C   Input:
C    X,Y      R   Complex coordinate of point for calculation
C    MAXVAL   I   Max Value returned by MAND = Number of iterations
C Output
C    KOUNT    I   Number between 1 and some limit
C-----------------------------------------------------------------------
      REAL      X, Y
      INTEGER   KOUNT, MAXVAL, LOOP
      REAL      Z1, Z2, ZNEXT1, ZMAG2
C --------------------------------------------------------------
C                               initialize squared value
      Z1 = 0.0
      Z2 = 0.0
C                               for a limited number of times
      DO 100 LOOP = 1,MAXVAL-1
         KOUNT = LOOP
         Z1 = Z1 + X
         Z2 = Z2 + Y
C                               complex squaring
         ZNEXT1 = (Z1*Z1) - (Z2*Z2)
         Z2 = 2 * Z1 * Z2
         Z1 = ZNEXT1
C                               calcuate magnitude squared
         ZMAG2 = (Z1*Z1) + (Z2*Z2)
C                               if zmag is greater than 2, exit
         IF (ZMAG2.GT.4) GO TO 999
100      CONTINUE
C
999   RETURN
      END
