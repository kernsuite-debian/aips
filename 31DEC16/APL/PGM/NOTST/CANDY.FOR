LOCAL INCLUDE 'CANDY.INC'
      INCLUDE 'INCS:PMAD.INC'
C                                       Local include for CANDY
      INTEGER   SEQOUT, DISKO, NEWCNO, NUMHIS, JBUFSZ, ICODE, LUNIN,
     *   FINDIN
      CHARACTER HISCRD(10)*64, FILEIN*48, SOURCE*8, OPCODE*4, NAMOUT*12,
     *   CLAOUT*6
      HOLLERITH XFILEI(12), XSOURC(2), XNAMOU(3), XCLAOU(2), XOPCOD,
     *   SCRTCH(256)
      REAL      XMSIZE(2), CELLS(2), XSEQO, XDISKO, CPARM(10),
     *   DPARM(10), BUFFER(MABFSS)
C                                       Program commons
      COMMON /INPARM/ XFILEI, XSOURC, XMSIZE, CELLS, XNAMOU, XCLAOU,
     *   XSEQO, XDISKO, XOPCOD, CPARM, DPARM
      COMMON /PARMS/ SEQOUT, DISKO, NEWCNO, JBUFSZ, ICODE, LUNIN,
     *   FINDIN, NUMHIS
      COMMON /BUFRS/ BUFFER, scrtch
      COMMON /CHRCOM/ HISCRD, FILEIN, SOURCE, OPCODE, NAMOUT, CLAOUT
LOCAL END
      PROGRAM CANDY
C-----------------------------------------------------------------------
C! Paraform task to create an AIPS image.
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
C   CANDY allows a user to create an image a row at a time.
C   Example code (mostly commented out) shows how to use CANDY.
C   IMPORTANT NOTE: to aviod confusion this task should be renamed.
C   To rename (max. 5 char) and install the new task :
C     1) copy the source code to a new file with the name newname.
C        then add desired code to subroutines NEWHED and MAKMAP.
C             NEWHED allows modification of the catalog header before
C        the output file is created. If the size of the output file
C        is to be different from the input then the necessary
C        changes must be made to NEWHED.  Validity checks on the
C        input parameters or the input image may be made in NEWHED.
C             MAKMAP is passed the image one row at a time.  Any
C        operation desired is made and MAKMAP returns the result.
C        Blanking is fully supported.
C     2) using the source editor change all references to CANDY to
C        newname.  It is especially important to change the string
C        entered into array PRGM at or near line 61 to the new name.
C     3) copy inputs file for CANDY to inputs file for newname.
C     4) compile and link edit with the APL  subroutine
C        library from AIPS.
C     5) copy and modify as appropriate the CANDY HELP file.
C
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INFILE         FILEIN        Input file.
C      SOURCE         SOURCE        Object name.
C      IMSIZE         XMSIZE        Image size in pixels.
C      CELLSIZE       CELLS         Pixel size in first two dim.
C      OUTNAME        NAMOUT        Name of the output image
C                                   Default output is input image.
C      OUTCLASS       CLAOUT        Class of the output image.
C                                   Default is input class.
C      OUTSEQ         SEQOUT        Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      OPCODE         OPCODE        User specified opcode.
C      CPARM(10)      CPARM         User specified array.
C      DPARM(10)      DPARM         User specified array.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'CANDY.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C
C   CHANGE THIS DATA STATMENT. WHEN CHANGING THE PROGRAM NAME.
C
      DATA PRGM /'CANDY '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL CANIN (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL SENDMA (IRET)
C                                       History
      IF (IRET.EQ.0) CALL CANHIS
C                                       Close down files, etc.
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE CANIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   CANIN gets input parameters for CANDY and creates an output file.
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
C   See prologue comments in CANDY for more details.
C
C   To change the adverb list sent to this task change:
C   1)  the inputs file.
C   2)  the contents of COMMON /INPARM/.  Remember all adverbs are sent
C       as R,    OUTNAME etc. are 12 char. 3 words;
C       OUTCLASS etc. are 6 char., 2 words.
C       Values will be filled into COMMON /INPARM/ in the order
C       specified in the inputs file.
C   3)  If the first adverb is not FILEIN then replace
C       XFILEI in the call to GTPARM with the name of the first
C       adverb.
C   4)  Change the value of NPARM sent to GTPARM to the number of
C       R   words desired.
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6
C
      CHARACTER BLANK*6, DEFNAM*12, OLDNAM*12
      INTEGER   IERR, NPARM, IROUND
      LOGICAL   T, F
      INCLUDE 'CANDY.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA BLANK /'      '/
      DATA T, F /.TRUE.,.FALSE./
C
C                                       Change this default output name
      DATA DEFNAM /'CANDY MAP   '/
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
      NPARM = 46
      CALL GTPARM (PRGN, NPARM, RQUICK, XFILEI, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQOUT = IROUND (XSEQO)
      DISKO = IROUND (XDISKO)
C                                       Characters
      CALL H2CHR (48, 1, XFILEI, FILEIN)
      CALL H2CHR (8, 1, XSOURC, SOURCE)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C                                       Init CATBLK.
      CALL CATINI (CATBLK)
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
      CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
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
 1000 FORMAT ('CANIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1060 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE SENDMA (IRET)
C-----------------------------------------------------------------------
C   SENDMA accepts an image one row at a time from the user supplied
C   routine.
C   Output:
C      IRET   I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   LUNO, BOTEMP, NXO, NYO, WINO(4), BOO, LIM2, LIM3, LIM4,
     *   LIM5, LIM6, LIM7, I1, I2, I3, I4, I5, I6, I7, IPOS(7), LIMO,
     *   LIMIT, OBIND, LUN1, LUN2, INDO, LIM1
      REAL      OUTMAX, OUTMIN
      CHARACTER IFILE*48, REST*4
      LOGICAL   T, F, BLNKD
      INCLUDE 'CANDY.INC'
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
         CALL MAKMAP (IPOS, BUFFER(OBIND), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1180) IRET
            GO TO 990
            END IF
C                                       Check max, min, blanking.
         LIMIT = OBIND + LIMO
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
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1120) 'FINI', IRET
         GO TO 990
         END IF
C                                       Update CATBLK.
      CATR(KRDMX) = OUTMAX
      CATR(KRDMN) = OUTMIN
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, REST, SCRTCH, IRET)
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
C                                       Final call to MAKMAP
      IPOS(1) = -1
      CALL MAKMAP (IPOS, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1180) IRET
         GO TO 990
         END IF
C                                       Close files
      CALL ZCLOSE (LUNO, INDO, IRET)
C*********************************************************************
C  Example code:
C      CALL ZTXCLS (LUNIN, FINDIN, IRET)
C*********************************************************************
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
      CHARACTER ATIME*8, ADATE*12, HILINE*72, LABEL*8, REST*4
      INTEGER   LUN, IERR, I, TIME(3), DATE(3)
      LOGICAL   T, F
      INCLUDE 'CANDY.INC'
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
      CALL HICREA (LUN, DISKO, NEWCNO, CATBLK, SCRTCH, IERR)
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
      CALL HIADD (LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       New history
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN,
     *   SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       SOURCE
      WRITE (HILINE,2000) TSKNAM, SOURCE
      CALL HIADD (LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       IMSIZE
      WRITE (HILINE,2001) TSKNAM, XMSIZE
      CALL HIADD (LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       CELLSIZE
      WRITE (HILINE,2002) TSKNAM, CELLS
      CALL HIADD (LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       OPCODE
      WRITE (HILINE,2003) TSKNAM, OPCODE
      CALL HIADD (LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       AIPS release
      WRITE (HILINE,2004) TSKNAM, RLSNAM
      CALL HIADD (LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any user supplied history.
      IF (NUMHIS.GT.0) THEN
         WRITE (LABEL,1011) TSKNAM
         DO 50 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN, HILINE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 200
 50         CONTINUE
         END IF
C                                       Close HI file
 200  CALL HICLOS (LUN, T, SCRTCH, IERR)
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, REST, SCRTCH, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CANHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ Image created by user',I5,' at ',A12,2X,A8)
 1011 FORMAT (A6,'/')
 2000 FORMAT (A6,'SOURCE = ''',A,'''')
 2001 FORMAT (A6,'IMSIZE = ', 2F8.0)
 2002 FORMAT (A6,'CELLSIZE = ',2F10.5)
 2003 FORMAT (A6,'OPCODE = ''',A,'''')
 2004 FORMAT (A6,'RELEASE = ''',A7,' ''')
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
C       2) Setting default values of some of the input parameters
C   As currently set the default OPCODE is the first value in the
C   array CODES which is set in a data statment.
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
      CHARACTER FCHARS(3)*4, BLANK*8, CODES(10)*4, UNITS(10)*8,
     *   ATYPES(7)*8
C      CHARACTER LINE*80
      INTEGER   I, NAXIS, IROUND, NCODE, INDEX
C      INTEGER   IERR, NX, NY
      INCLUDE 'CANDY.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA FCHARS /'FREQ','VELO','FELO'/
      DATA  BLANK /'        '/
C                                       User definable values
C                                       # and value of OPCODEs
      DATA NCODE /0/
      DATA CODES /10*'    '/
C                                       Output units for each OPCODE.
      DATA UNITS /'UNDEFINE',9*'        '/
C                                       Number of axes and types.
C                                       (Set for two axes = Ra, Dec.)
      DATA NAXIS  /2/
      DATA ATYPES /'RA---SIN', 'DEC--SIN',
     *   'STOKES  ', 'FREQ    ', 3*'       '/
C-----------------------------------------------------------------------
C                                       Set default OPCODE
      ICODE = 1
      IF (NCODE.GT.0) THEN
         DO 10 I = 1,NCODE
            ICODE = I
            IF (OPCODE.EQ.CODES(I)) GO TO 20
 10         CONTINUE
C                                       Default OPCODE is first.
         OPCODE = CODES(1)
         ICODE = 1
         END IF
C                                       Set output units.
 20   CALL CHR2H (8, UNITS(ICODE), 1, CATH(KHBUN))
C                                       Fill axis arrays.
      DO 30 I = 1,KICTPN
C                                       Fill axis type from
C                                       ATYPES or BLANK.
         INDEX = KHCTP + (I-1) * 2
         IF (I.LE.NAXIS)
     *      CALL CHR2H (8, ATYPES(I), 1, CATH(INDEX))
 30      CONTINUE
C***********************************************************************
C   In this example, INFILE is a formatted file of 80 byte records
C   and the number of rows and columns of the two dimensional image
C   are read from the first record of INFILE as 2I5.
C
C                                       Open the file
C      CALL ZTXOPN ('READ', LUNIN, FINDIN, FILEIN, F, IERR)
C      IF (IERR.NE.0) GO TO 999
C                                       Read first line
C      CALL ZTXIO ('READ', LININ, FINDIN, LINE, IERR)
C      IF (IERR.NE.0) GO TO 999
C                                       Note: list directed internal
C                                       reads are not legal.
C      READ (LINE,2001) NX, NY
C 2001 FORMAT (2I5)
C      XMSIZE(1) = NX
C      XMSIZE(2) = NY
C
C    Other information about the file may be entered here.
C
C***********************************************************************
C                                       Fill in values.
      CATBLK(KINAX) = MAX (IROUND (XMSIZE(1)), 1)
      CATBLK(KINAX+1) = MAX (IROUND (XMSIZE(2)), 1)
C                                       Assume CELLSIZE in sec.
C                                       NOTE: Ra decreases with
C                                       grid number.
      CATR(KRCIC) = - CELLS(1) / 3600.
      CATR(KRCIC+1) = CELLS(2) / 3600.
C                                       Fill other character strings.
C                                       Object.
      CALL CHR2H (8, SOURCE, 1, CATH(KHOBJ))
C                                       Set number of axes.
      CATBLK(KIDIM) = NAXIS
C                                       User ID
      CATBLK(KIIMU) = NLUSER
C                                       Miscellaneous items.
C                                       Put other checks here.
C                                       Finished.
      IRET = 0
      GO TO 999
CC
 999  RETURN
C-----------------------------------------------------------------------
C                                       FORMATs go here
      END
      SUBROUTINE MAKMAP (IPOS, RESULT, IRET)
C-----------------------------------------------------------------------
C  This is a skeleton version of subroutine MAKMAP which allows
C  the user to create an image, one row at a time.
C  Output values may  be blanked.
C  The calling routine keeps of max., min. and to occurence of blanking.
C  CATBLK(KINAX) values per call are expected returned.
C  NOTE: blanked values are denoted by the value of the common variable
C  FBLANK
C
C       Up to 10 history entries can be written by using WRITE to
C  record up to 64 characters per entry into array HISCRD. Ex:
C     WRITE (HISCRD(entry #),format #,) list
C  TRC, BLC and OPCODE are already taken care of.
C  The history is written after the last call to MAKMAP.
C
C       Messages can be written to the monitor/logfile by writing
C  the message (up to 80 char) into array MSGTXT in COMMON /MSGCOM/
C  and then issuing a call:
C     CALL MSGWRT (priority #)
C
C       Unit 1 is the line printer
C
C       If IRET .GT. 0 then the output file will be destroyed.
C
C       After all data has been processed a final call will be made to
C  MAKMAP with IPOS(1)=-1.  This is to allow for the completion of
C  pending operations, i.e. preparation of HIstory cards.
C
C       LUN's 16-18 are open and not available to MAKMAP.
C
C       The current contents of CATBLK will be written back to the
C  catalog after the last call to MAKMAP.
C
C  Inputs:
C   IPOS   I(7)    BLC (input image) of first value in DATA
C  Values from commons:
C   ICODE   I      Opcode number from list in NEWHED.
C   FBLANK  R      Value of blanked pixel.
C   CPARM   R(10)  Input adverb array.
C   DPARM   R(10)  Input adverb array.
C   CATBLK  I(256) Output catalog header (also CATR, CATD)
C  Output:
C   RESULT   R(*)  Output row.
C   IRET     I     Return code   0 => OK
C                               >0 => error, terminate.
C  Output in COMMON:
C  NUMHIS  I          # history entries (max. 10)
C  HISCRD  C(NUMHIS)  History records
C  CATBLK  I          Catalog header block
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), IRET
      REAL      RESULT(*)
C
      INTEGER   NX, I
C      CHARACTER LINE*80
      INCLUDE 'CANDY.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      SAVE NX
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check if last call
      IF (IPOS(1).LT.0) GO TO 900
C                                       Check if first call on plane.
      IF (IPOS(2).GT.1) GO TO 50
C                                       First call on plane.
C                                       Set length of a row.
      NX = CATBLK(KINAX)
C                                       Subsequent calls.
 50   CONTINUE
C
C    USER CODE GOES HERE
C***********************************************************************
C   SAMPLE CODE
C   In this example the image is stored as REAL values as 14F5.0
C   with as many records as necessary being used for a row; each row
C   begins at the start of a record.  Values of less than CPARM(1)
C   are considered to be blanked.
C      IRET = 1
C      CALL ZTXIO ('READ', LUNIN, FINDIN, LINE, IERR)
C      IF (IERR.NE.0) GO TO 999
C      READ (LINE,2000,ERR=999) (RESULT(I), I=1,NX)
C      IRET = 0
C 2000 FORMAT (14F5.0)
C      DO 100 I = 1,NX
C         IF (RESULT(I).LT.CPARM(1)) RESULT(I) = FBLANK
C 100     CONTINUE
C***********************************************************************
C
C
C
C
C
C
C
C***********************************************************************
C                                       Zero fill for testing -
C                                       Be sure to remove before use.
      DO 300 I = 1,NX
         RESULT(I) = 0.0
 300     CONTINUE
C***********************************************************************
C                                       Finished
      GO TO 999
C                                       Last call - do history etc.
 900  CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
C   FORMAT STATEMENTS GO HERE
      END

