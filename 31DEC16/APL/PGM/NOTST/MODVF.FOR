LOCAL INCLUDE 'MODVF.INC'
C                                       Local include for MODVF
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   SEQOUT, DISKO, NEWCNO, NUMHIS, JBUFSZ, ICODE, LUNIN,
     *   FINDIN, NRING, NANNU
      CHARACTER HISCRD(10)*64, FILEIN*48, SOURCE*8, NAMOUT*12,
     *   CLAOUT*6
      HOLLERITH XFILEI(12), XSOURC(2), XNAMOU(3), XCLAOU(2)
      REAL      XMSIZ(2), CELLS(2), XSEQO, XDISKO, APARM(10),
     *          BUFFER(MABFSS), XINC
C                                       Program commons
      COMMON /INPARM/ XFILEI, XSOURC, XMSIZ, CELLS, XNAMOU, XCLAOU,
     *   XSEQO, XDISKO, APARM, XINC
      COMMON /PARMS/ SEQOUT, DISKO, NEWCNO, JBUFSZ, ICODE, LUNIN,
     *               FINDIN, NUMHIS, NRING, NANNU
      COMMON /BUFRS/ BUFFER
      COMMON /CHRCOM/ HISCRD, FILEIN, SOURCE, NAMOUT, CLAOUT
LOCAL END
LOCAL INCLUDE 'TABEL.INC'
      INTEGER   XDIM, PDIM
      PARAMETER (XDIM=1024)
      PARAMETER (PDIM=200)
      INTEGER   NVELO(XDIM,XDIM)
      REAL      VELOC(XDIM,XDIM)
      REAL      RING(PDIM), VROT(PDIM), WARP(PDIM), OMEG(PDIM),
     *   RNGM(PDIM)
      COMMON /TABEL/ RING, VROT, WARP, OMEG, RNGM
      COMMON /FIELD/ NVELO, VELOC
LOCAL END
      PROGRAM MODVF
C-----------------------------------------------------------------------
C! task to create a warped velocity field. Based on paraform task CANDY.
C# Task Imaging Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000, 2009
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
C
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INFILE         FILEIN        Input file.
C      SOURCE         SOURCE        Object name.
C      IMSIZE         XMSIZ         Image size in pixels.
C      CELLSIZE       CELLS         Pixel size in first two dim.
C      OUTNAME        NAMOUT        Name of the output image
C                                   Default output is input image.
C      OUTCLASS       CLAOUT        Class of the output image.
C                                   Default is input class.
C      OUTSEQ         SEQOUT        Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      APARM(10)      APARM         User specified array.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'MODVF.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'MODVF '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file.
      CALL MODIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Populates array with velocities
      CALL MODFIL (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Writes array as AIPS image.
      CALL MODMAP (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       History
      CALL MODHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE MODIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   MODIN gets input parameters for MODVF and creates an output file.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IRET    I    Error code: 0 => ok
C                     4 => user routine detected error.
C                     5 => catalog troubles
C                     8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6
C
      CHARACTER BLANK*6, DEFNAM*12, OLDNAM*12
      INTEGER   IERR, NPARM, IROUND
      LOGICAL   T, F
      INCLUDE 'MODVF.INC'
      INCLUDE 'TABEL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA BLANK /'      '/
      DATA T, F /.TRUE.,.FALSE./
C
C                                       Change this default output name
      DATA DEFNAM /'MODVF MAP   '/
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
C                                       Init CATBLK.
      CALL FILL (256, 0, CATBLK)
C                                       Get input parameters.
      NPARM = 36
      CALL GTPARM (PRGN, NPARM, RQUICK, XFILEI, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFFER, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQOUT = IROUND (XSEQO)
      DISKO = IROUND (XDISKO)
C                                       Characters
      CALL H2CHR (48, 1, XFILEI, FILEIN)
      CALL H2CHR (8, 1, XSOURC, SOURCE)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C                                       image size, central position
      IF ((XMSIZ(1).LT.2.) .OR. (XMSIZ(1).GT.XDIM)) XMSIZ(1) = XDIM
      IF ((XMSIZ(2).LT.2.) .OR. (XMSIZ(2).GT.XDIM)) XMSIZ(2) = XDIM
      IF (APARM(1).LT.1.0) APARM(1) = 0.5 * XMSIZ(1)
      IF (APARM(2).LT.1.0) APARM(2) = 0.5 * XMSIZ(2) + 1
      IF (XINC.EQ.0.0) THEN
         MSGTXT = 'XINC *has* to be specified'
         IERR = 10
         GO TO 999
         END IF
C                                       Initialize arrays
      NPARM = XDIM * XDIM
      CALL FILL  (NPARM, 0,      NVELO)
      CALL RFILL (NPARM, FBLANK, VELOC)
C                                       Create new file.
C                                       Put values in CATBLK.
      OLDNAM = DEFNAM
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
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 2
      IRET = 0
      SEQOUT = CATBLK(KIIMS)
C                                       Set obs. date=current date.
      CATH(KHDOB) = CATH(KHDMP)
      CATH(KHDOB+1) = CATH(KHDMP+1)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MODIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1060 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE MODMAP (IRET)
C-----------------------------------------------------------------------
C   MODMAP fills an AIPS image with data from the array VELOC
C   Output:
C      IRET   I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   LUNO, BOTEMP, NXO, NYO, WINO(4), BOO, LIM2, LIM3, LIM4,
     *   LIM5, LIM6, LIM7, IB, I1, I2, I3, I4, I5, I6, I7, IPOS(7),
     *   LIMO, LIMIT, OBIND, LUN1, LUN2, INDO, LIM1
      REAL      OUTMAX, OUTMIN
      CHARACTER IFILE*48, REST*4
      LOGICAL   T, F, BLNKD
      INCLUDE 'MODVF.INC'
      INCLUDE 'TABEL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA LUNO, LUN1, LUN2 /16,17,18/
      DATA REST /'REST'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Create scratch file.
C                                       Open vis file for write
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Setup for I/O
      NXO = CATBLK(KINAX)
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
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IPOS(3), BOTEMP, IRET)
      BOO = BOTEMP + 1
      CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFFER, JBUFSZ,
     *   BOO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) 'WRIT', IRET
         GO TO 990
         END IF
      DO 250 I2 = 1,LIM2
         IPOS(2) = I2
C                                       Write.
         CALL MDISK ('WRIT', LUNO, INDO, BUFFER, OBIND, IRET)
         OBIND = OBIND - 1
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1120) 'WRIT', IRET
            GO TO 990
            END IF
C                                       Call MAKMAP
         OBIND = OBIND + 1
C         CALL MAKMAP (IPOS, BUFFER(OBIND), IRET)
C         IF (IRET.NE.0) THEN
C            WRITE (MSGTXT,1180) IRET
C            GO TO 990
C            END IF
C
C                                       Check max, min, blanking.
         LIMIT = OBIND + LIMO
         DO 200 IB = OBIND,LIMIT
            I1 = IB - OBIND + 1
            BUFFER(IB) = VELOC(I1,I2)
            BLNKD = BLNKD .OR. (BUFFER(IB).EQ.FBLANK)
            IF (BUFFER(IB).EQ.FBLANK) GO TO 200
               OUTMAX = MAX (OUTMAX, BUFFER(IB))
               OUTMIN = MIN (OUTMIN, BUFFER(IB))
 200           CONTINUE
 250     CONTINUE
C                                       Dump plane to output.
C                                       Flush buffer.
      CALL MDISK ('FINI', LUNO, INDO, BUFFER, OBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1120) 'FINI', IRET
         GO TO 990
         END IF
C                                       Update CATBLK.
 260  CATR(KRDMX) = OUTMAX
      CATR(KRDMN) = OUTMIN
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, REST, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1260) IRET
         GO TO 990
         END IF
 300  CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Mark blanking in CATBLK.
      CATR(KRBLK) = 0.0
      IF (BLNKD) CATR(KRBLK) = FBLANK
C                                       Close files
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('MODMAP: ERROR',I3,' OPENING SCRATCH FILE')
 1100 FORMAT ('MODMAP: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('MODMAP: ',A4,' ERROR',I3)
 1180 FORMAT ('MODMAP: MAKMAP ERROR',I3)
 1260 FORMAT ('MODMAP: CATIO ERROR',I3,' UPDATING CATBLK')
      END
      SUBROUTINE MODHIS
C-----------------------------------------------------------------------
C   MODHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER ATIME*8, ADATE*12, HILINE*72, LABEL*8, REST*4
      INTEGER   LUN, IERR, I, TIME(3), DATE(3)
      LOGICAL   T, F
      INCLUDE 'MODVF.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN /27/
      DATA REST /'REST'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Create/open hist. file.
      CALL HICREA (LUN, DISKO, NEWCNO, CATBLK, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Get current date/time.
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
C                                       Write first record.
      WRITE (HILINE,1010) TSKNAM, NLUSER, ADATE, ATIME
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       New history
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN,
     *   BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       SOURCE
      WRITE (HILINE,2000) TSKNAM, SOURCE
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       IMSIZE
      WRITE (HILINE,2001) TSKNAM, XMSIZ
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       CELLSIZE
      WRITE (HILINE,2002) TSKNAM, CELLS
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       AIPS release
      WRITE (HILINE,2004) TSKNAM, RLSNAM
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any user supplied history.
      IF (NUMHIS.GT.0) THEN
         WRITE (LABEL,1011) TSKNAM
         DO 50 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 200
 50         CONTINUE
         END IF
C                                       Close HI file
 200  CALL HICLOS (LUN, T, BUFFER, IERR)
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, REST, BUFFER,
     *   IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MODHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ Image created by user',I5,' at ',A12,2X,A8)
 1011 FORMAT (A6,' /')
 2000 FORMAT (A6,' SOURCE = ''',A,'''')
 2001 FORMAT (A6,' IMSIZE = ', 2F8.0)
 2002 FORMAT (A6,' CELLSIZE = ',2F10.5)
 2004 FORMAT (A6,' RELEASE = ''',A7,' ''')
      END
      SUBROUTINE NEWHED (IRET)
C-----------------------------------------------------------------------
C   NEWHED is a routine in which the user performs several operations
C   associated with beginning the task.  For many purposes simply
C   changing some of the values in the DATA statments will be all that
C   is necessary.  The following functions are/can be preformed
C   in NEWHED:
C       1) Creating the catalog header block to represent the
C   output file.  The MINIMUM information required here is that
C   required to define the size of the output file; ie.
C      CATBLK(KIDIM)= the number of axes,
C      CATBLK(KINAX+i) = the dimension of each axis, and
C      CATBLK(KIBPX) => 2 = real*4 pixel values.
C   Other changes can be made either here or in MAKMAP; the
C   catalog block will be updated when the history file is
C   written.
C
C    Input:
C     CATBLK    I(256)  Output catalog header, also CATR, CATD
C                       The OUTNAME, OUTCLASS, OUTSEQ are entered
C                       elsewhere.
C    Output:
C     CATBLK    I(256)  Modified output catalog header.
C     IRET      I       Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER FCHARS(3)*4, BLANK*8, ATYPES(7)*8
      INTEGER   I, NAXIS, IROUND, INDEX
      INCLUDE 'MODVF.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA FCHARS /'FREQ','VELO','FELO'/
      DATA  BLANK /'        '/
C                                       User definable values
C                                       Number of axes and types.
C                                       (Set for two axes = Ra, Dec.)
      DATA NAXIS  /2/
      DATA ATYPES /'RA---SIN', 'DEC--SIN',
     *   'STOKES  ', 'FREQ    ', 3*'       '/
C-----------------------------------------------------------------------
C                                       Fill axis arrays.
      DO 30 I = 1,KICTPN
C                                       Init dimension
         CATBLK(KINAX+I-1) = 1
C                                       Init. increment.
         CATR(KRCIC+I-1) = 0.0
C                                       Init. ref pixel.
         CATR(KRCRP+I-1) = 1.0
C                                       Init. ref value.
         CATD(KDCRV+I-1) = 0.0D0
C                                       Fill axis type from
C                                       ATYPES or BLANK.
         INDEX = KHCTP + (I-1) * 2
         CALL CHR2H (8, BLANK, 1, CATH(INDEX))
         IF (I.LE.NAXIS)
     *      CALL CHR2H (8, ATYPES(I), 1, CATH(INDEX))
C                                       Blank Random axes.
         INDEX = KHPTP + (I-1) * 2
         CALL CHR2H (8, BLANK, 1, CATH(INDEX))
 30      CONTINUE
C                                       Fill in values.
      CATBLK(KINAX)   = MAX (IROUND (XMSIZ(1)), 1)
      CATBLK(KINAX+1) = MAX (IROUND (XMSIZ(2)), 1)
C                                       Assume CELLSIZE in sec.
C                                       NOTE: Ra decreases with
C                                       grid number.
      CATR(KRCIC)   = - CELLS(1) / 3600.
      CATR(KRCIC+1) =   CELLS(2) / 3600.
C                                       Fill other character strings.
C                                       Object.
      CALL CHR2H (8, SOURCE, 1, CATH(KHOBJ))
C                                       Observation date.
      CALL CHR2H (8, BLANK, 1, CATH(KHDOB))
C                                       Telescope.
      CALL CHR2H (8, BLANK, 1, CATH(KHTEL))
C                                       Receiver
      CALL CHR2H (8, BLANK, 1, CATH(KHINS))
C                                       Observer's name.
      CALL CHR2H (8, BLANK, 1, CATH(KHOBS))
C                                       Set number of axes.
      CATBLK(KIDIM) = NAXIS
      CATBLK(KIPCN) = 0
C                                       User ID
      CATBLK(KIIMU) = NLUSER
C                                       Miscellaneous items.
C                                       Epoch.
      CATR(KREPO) = 0.0
C                                       Convolving beam
      CATR(KRBMJ) = 0.0
      CATR(KRBMN) = 0.0
      CATR(KRBPA) = 0.0
C                                       Max. min.
      CATR(KRDMX) = 0.0
      CATR(KRDMN) = 0.0
C                                       Put other checks here.
C                                       Finished.
      IRET = 0
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE MODFIL (IERR)
C-----------------------------------------------------------------------
C MODFIL reads INFILE and calls CTS.
C-----------------------------------------------------------------------
      INTEGER I, KBP, IERR, TXLUN, JTRIM, KSIZ
      REAL    RNGL
      DOUBLE PRECISION XIN
      CHARACTER LINE*132
      LOGICAL F
      INCLUDE 'MODVF.INC'
      INCLUDE 'TABEL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DERR.INC'
C
      DATA TXLUN /10/
C-----------------------------------------------------------------------
C                                       Open input file
      CALL ZTXOPN ('READ', TXLUN, FINDIN, FILEIN, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'Opening'
         GO TO 990
         END IF
      I    = 0
      RNGL = 0.0
 10      CALL ZTXIO ('READ', TXLUN, FINDIN, LINE, IERR)
         IF (IERR.EQ.2) GO TO 50
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'Reading'
            GO TO 990
            END IF
         KSIZ = JTRIM (LINE)
         IF ((LINE.EQ.' ') .OR. (LINE(:1).EQ.'$') .OR.
     *      (LINE(:1).EQ.'#')) GO TO 10
         I = I + 1
         IF (I.LE.PDIM) GO TO 20
            I = I - 1
            WRITE (MSGTXT,1020)
            GO TO 50
C                                   Process LINE
 20      KBP = 1
         ERRNUM = 0
         CALL GETNUM (LINE, KSIZ, KBP, XIN)
         IF (ERRNUM.NE.0) GO TO 980
         RING(I) = XIN
         RNGM(I) = (RING(I) + RNGL) / 2.0
         RNGL    = RING(I)
         IF (KBP.LE.KSIZ) THEN
            CALL GETNUM (LINE, KSIZ, KBP, XIN)
            IF (ERRNUM.NE.0) GO TO 980
            VROT(I) = XIN
         ELSE
            VROT(I) = 0.0
            END IF
         IF (KBP.LE.KSIZ) THEN
            CALL GETNUM (LINE, KSIZ, KBP, XIN)
            IF (ERRNUM.NE.0) GO TO 980
            WARP(I) = XIN
         ELSE
            WARP(I) = 0.0
            END IF
         IF (KBP.LE.KSIZ) THEN
            CALL GETNUM (LINE, KSIZ, KBP, XIN)
            IF (ERRNUM.NE.0) GO TO 980
            OMEG(I) = XIN
         ELSE
            OMEG(I) = 0.0
            END IF
         WRITE (MSGTXT,1020) I, RING(I), VROT(I), WARP(I), OMEG(I)
         CALL MSGWRT (2)
         GO TO 10

 50   CALL ZTXCLS (TXLUN, FINDIN, IERR)
      NRING      = I
      NANNU      = RING(NRING) / XINC
C                                   Now populate the velocity field
      CALL CTS
      GO TO 990
C                                    problems
 980  WRITE (MSGTXT,1980) I
      CALL MSGWRT (8)
      MSGTXT = 'LINE=''' // LINE
      CALL MSGWRT (8)
      IERR = ERRNUM
C
 990  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR ',I3,1X,A,' Text File')
 1020 FORMAT ('ERROR: list longer than 500. Using first 500')
 1980 FORMAT ('ERROR PARSING INFILE LINE',I3)
      END
      SUBROUTINE CTS
C-----------------------------------------------------------------------
C This subroutine uses the algorithm by Christodoulou, Tohline, and
C Steiman-Cameron to create a velocity field of a galaxy based on a
C tilted-ring model.
C-----------------------------------------------------------------------
C
C
C                                      Declarations
      INTEGER IX, IY
      INTEGER IANNU, ISEGM, NSEGM, IRING
      REAL    PAREF, INREF, RAANN, VCRNG, PARNG, WARNG, PASEG, DPSEG,
     *        CPARF, SPARF, CINRF, SINRF, CPARN, SPARN, CWARN, SWARN,
     *        CPASG, SPASG, TERM1, TERM2, MSEGM, RINGL, RINGU, FACTR,
     *        RRANG, VRANG, WRANG, ORANG, VOFFS, WOFFS, OOFFS, VROTL,
     *        WARPL, OMEGL
      REAL    SPCW,  CPCW,  KSI, ETA, VEL
      DOUBLE PRECISION A2R
      INCLUDE 'MODVF.INC'
      INCLUDE 'TABEL.INC'
      DATA A2R  /1.7453292519943296D-02/
C
C Two concepts are important: 1) annuli, 2) rings. Annuli are thin
C rings the thickness of which is defined by XINC. One annulus and
C one segment define a pixel in the plane of the tilted ring. This
C pixel is mapped onto the sky plane. Rings are - usually - much
C wider annuli in which the crucial variables VROT, WARP, and OMEG
C are assumed to be constant. In fact, MODVF will do a linear inter-
C polation to avoid jagged velocity contours. The current routine
C loops over annuli, and updates VROT, WARP, and OMEG whenever a new
C annulus has reached a new RING. The inner ring - which is a small
C disk - is referred to as the reference disk.
C                                      Define reference-disk variables
      PAREF = (APARM(3) + 180.0) * A2R
      INREF = APARM(4)* A2R
      CPARF = COS (PAREF)
      SPARF = SIN (PAREF)
      CINRF = COS (INREF)
      SINRF = SIN (INREF)
      MSEGM = 2.0 * 3.14159265 / XINC
C                                      # and size of inner annulus of
C                                      current ring.
      IRING = 0
      RINGL = 0.0
      RINGU = 0.0
      VROTL = VROT(1)
      WARPL = WARP(1)
      OMEGL = OMEG(1)
C                                      Initialize arrays
C                                      Begin loop over all annuli
      DO 200 IANNU = 1, NANNU
C                                      Variables for annulus: radius,
C                                      # segments, radians/segment
         RAANN = IANNU * XINC
         NSEGM = NINT (MSEGM * RAANN)
         DPSEG = 360.0 / NSEGM * A2R
C                                      Is annulus in new ring? If so,
C                                      update common variables. For last
C                                      ring, use extrapolation.
         IF (RAANN.GT.RINGU.AND.IRING.NE.NRING) THEN
            IRING = IRING + 1
            RINGL = RINGU
            RINGU = RNGM(IRING)
            RRANG = RNGM(IRING) - RINGL
            VRANG = VROT(IRING) - VROTL
            WRANG = WARP(IRING) - WARPL
            ORANG = OMEG(IRING) - OMEGL
            VOFFS = VROTL
            WOFFS = WARPL
            OOFFS = OMEGL
            VROTL = VROT(IRING)
            WARPL = WARP(IRING)
            OMEGL = OMEG(IRING)
            END IF
         FACTR = (RAANN - RINGL) / RRANG
         VCRNG = (FACTR * VRANG + VOFFS)
         WARNG = (FACTR * WRANG + WOFFS) * A2R
         PARNG = (FACTR * ORANG + OOFFS) * A2R
         CPARN = COS (PARNG)
         SPARN = SIN (PARNG)
         CWARN = COS (WARNG)
         SWARN = SIN (WARNG)
C                                      'popular' products
         SPCW  = SPARN * CWARN
         CPCW  = CPARN * CWARN
C                                      Begin loop ring segments
         DO 100 ISEGM = 1, NSEGM
C                                      Variables for this segment
            PASEG = (ISEGM - 1) * DPSEG
            CPASG = COS (PASEG)
            SPASG = SIN (PASEG)
C                                      Build up formulas
            TERM1 =  CPASG * CPARN - SPASG * SPCW
            TERM2 = (CPASG * SPARN + SPASG * CPCW) * CINRF -
     1               SPASG * SWARN * SINRF
C
            KSI = RAANN *  (CPARF * TERM1 - SPARF * TERM2) / CELLS(2)
            ETA = RAANN *  (SPARF * TERM1 + CPARF * TERM2) / CELLS(1)
            VEL = 1000.0 * (VCRNG * ((SPASG * SPARN - CPASG * CPCW)
     1            * SINRF - CPASG * SWARN * CINRF) + APARM(5))
C                                      ETA, KSI in map?
            IX =  NINT (APARM(1) - ETA)
            IY =  NINT (APARM(2) + KSI)
            IF (IX.LE.XMSIZ(1).AND.IX.GE.1.0 .AND.
     1          IY.LE.XMSIZ(2).AND.IY.GE.1.0) THEN
C                                      add value to array
               IF (NVELO(IX,IY).EQ.0) THEN
                  VELOC(IX,IY) = VEL
               ELSE
                  VELOC(IX,IY) = (VELOC(IX,IY) * NVELO(IX,IY)
     1               + VEL) / (NVELO(IX,IY) + 1)
                  END IF
               NVELO(IX,IY) = NVELO(IX,IY) + 1
               END IF
C                                      End loop ring segments
 100        CONTINUE
C                                      End loop over all rings
 200     CONTINUE
C
      RETURN
      END
