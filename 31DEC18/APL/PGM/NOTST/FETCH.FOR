LOCAL INCLUDE 'FETCH.INC'
      INCLUDE 'INCS:PMAD.INC'
C                                       Local include for FETCH
      INTEGER   SEQOUT, DISKO, NEWCNO, NUMHIS, JBUFSZ, ICODE, LUNIN,
     *   FINDIN, SCRTCH(256)
      LOGICAL   ISINT
      CHARACTER HISCRD(10)*64, FILEIN*48, NAMOUT*12, CLAOUT*6, FORMAT*10
      HOLLERITH XFILEI(12), XNAMOU(3), XCLAOU(2)
      REAL      XSEQO, XDISKO, BUFFER(MABFSS)
      DOUBLE PRECISION SCALE, OFFSET, BLANK
C                                       Program commons
      COMMON /INPARM/ XFILEI, XNAMOU, XCLAOU, XSEQO, XDISKO
      COMMON /PARMS/ SCALE, OFFSET, BLANK, SEQOUT, DISKO, NEWCNO,
     *   JBUFSZ, ICODE, LUNIN, FINDIN, NUMHIS, ISINT
      COMMON /BUFRS/ BUFFER, SCRTCH
      COMMON /CHRCOM/ HISCRD, FILEIN, NAMOUT, CLAOUT, FORMAT
LOCAL END
      PROGRAM FETCH
C-----------------------------------------------------------------------
C!  Task to create an AIPS image from a text file.
C# Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000, 2015, 2017
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
C   FETCH allows a user to create an image a row at a time.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INFILE         FILEIN        Input file.
C      OUTNAME        NAMOUT        Name of the output image
C                                   Default output is input image.
C      OUTCLASS       CLAOUT        Class of the output image.
C                                   Default is input class.
C      OUTSEQ         SEQOUT        Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'FETCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'FETCH '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file.
      CALL FETIN (PRGM, IRET)
C                                       Call routine that reads data.
      IF (IRET.EQ.0) CALL FETMA (IRET)
C                                       History
      IF (IRET.EQ.0) CALL FETHIS
C                                       Close down files, etc.
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE FETIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   FETIN gets input parameters for FETCH and creates an output file.
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
C   See prologue comments in FETCH for more details.
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6
C
      CHARACTER CBLANK*6, DEFNAM*12, OLDNAM*12
      INTEGER   IERR, NPARM, IROUND
      LOGICAL   T, F
      INCLUDE 'FETCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA CBLANK /'      '/
      DATA T, F /.TRUE.,.FALSE./
      DATA DEFNAM /'FETCH MAP   '/
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
      LUNIN = 10
C                                       Init CATBLK.
      CALL FILL (256, 0, CATBLK)
C                                       Get input parameters.
      NPARM = 19
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
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C                                       Create new file.
C                                       Put values in CATBLK.
      OLDNAM = DEFNAM
      CALL MAKOUT (OLDNAM, CBLANK, 0, CBLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = SEQOUT
C                                       Get user modification to CATBLK
      IRET = 4
      CALL FETHED (IRET)
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
 1000 FORMAT ('FETIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1060 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE FETMA (IRET)
C-----------------------------------------------------------------------
C   FETMA accepts an image one row at a time from GETMA and writes to an
C   AIPS disk file.
C   Output:
C      IRET   I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   LUNO, BOTEMP, NXO, NYO, WINO(4), BOO, LIM2, LIM3, LIM4,
     *   LIM5, LIM6, LIM7, I1, I2, I3, I4, I5, I6, I7, IPOS(7), LIMO,
     *   LIMIT, OBIND, LUN1, LUN2, INDO, LIM1
      REAL      OUTMAX, OUTMIN
      CHARACTER OFILE*48, REST*4
      LOGICAL   T, F, BLNKD
      INCLUDE 'FETCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNO, LUN1, LUN2 /16,17,18/
      DATA REST /'REST'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open output file for write
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
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
C                                       Call GETMA
         OBIND = OBIND + 1
         CALL GETMA (IPOS, BUFFER(OBIND), IRET)
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
C                                       Final call to GETMA
      IPOS(1) = -1
      CALL GETMA (IPOS, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1180) IRET
         GO TO 990
         END IF
C                                       Close files
      CALL ZCLOSE (LUNO, INDO, IRET)
      CALL ZTXCLS (LUNIN, FINDIN, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('FETMA: ERROR',I3,' OPENING SCRATCH FILE')
 1100 FORMAT ('FETMA: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('FETMA: ',A4,' ERROR',I3)
 1180 FORMAT ('FETMA: GETMA ERROR',I3)
 1260 FORMAT ('FETMA: CATIO ERROR',I3,' UPDATING CATBLK')
      END
      SUBROUTINE FETHIS
C-----------------------------------------------------------------------
C   FETHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER ATIME*8, ADATE*12, HILINE*72, LABEL*8, REST*4
      INTEGER   LUN, IERR, I, TIME(3), DATE(3)
      LOGICAL   T, F
      INCLUDE 'FETCH.INC'
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
C                                       AIPS release
      WRITE (HILINE,2004) TSKNAM, RLSNAM
      CALL HIADD (LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       INFILE
      WRITE (HILINE,1012) TSKNAM, FILEIN
      CALL HIADD (LUN, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any other history.
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
 1000 FORMAT ('FETHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ Image created by user',I5,' at ',A12,2X,A8)
 1011 FORMAT (A6,' /')
 1012 FORMAT (A6, 'INFILE =''',A,'''')
 2004 FORMAT (A6,'RELEASE = ''',A7,' ''')
      END
      SUBROUTINE FETHED (IRET)
C-----------------------------------------------------------------------
C   FETHED is a routine in which the catalog header for the file is
C   prepared.  Default values are filled in and then REDHED is called to
C   read the header section of the file and fill in the values found
C   there.
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
      CHARACTER CBLANK*8, ATYPES(7)*8, TFORM*8, TOBJ*8, TTELS*8,
     *   TINST*8, TOBSR*8, TUNIT*8, CRTYPE(7)*8
      INTEGER   I, NAXIS, IERR, INDX, NUMAX, DIM(7)
      DOUBLE PRECISION TEPOCH, CRVAL(8), CRINC(8), CRREF(8), CRROT(8)
      INCLUDE 'FETCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA  CBLANK /'        '/
C                                       Number of axes and types.
      DATA NAXIS  /0/
      DATA ATYPES /7*'        '/
C-----------------------------------------------------------------------
C                                       Scaling and offset
      SCALE = 1.0
      OFFSET = 0.0
C                                       Set output units.
      CALL CHR2H (8, 'UNDEFINE', 1, CATH(KHBUN))
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
C                                       Init. rotation.
         CATD(KRCRT+I-1) = 0.0D0
C                                       Blank fill axis type.
         INDX = KHCTP + (I-1) * 2
         CALL CHR2H (8, CBLANK, 1, CATH(INDX))
C                                       Blank Random axes.
         INDX = KHPTP + (I-1) * 2
         CALL CHR2H (8, CBLANK, 1, CATH(INDX))
 30      CONTINUE
C                                       Fill other character strings.
C                                       Object.
      CALL CHR2H (8, CBLANK, 1, CATH(KHOBJ))
C                                       Observation date.
      CALL CHR2H (8, CBLANK, 1, CATH(KHDOB))
C                                       Telescope.
      CALL CHR2H (8, CBLANK, 1, CATH(KHTEL))
C                                       Receiver
      CALL CHR2H (8, CBLANK, 1, CATH(KHINS))
C                                       Observer's name.
      CALL CHR2H (8, CBLANK, 1, CATH(KHOBS))
C                                       Epoch
      CATR(KREPO) = 0.0
      TFORM = '        '
C                                       Open the input file
      CALL ZTXOPN ('READ', LUNIN, FINDIN, FILEIN, .FALSE., IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Read header section of file.
      CALL REDHED (NUMAX, DIM, TFORM, TOBJ, TTELS, TINST, TOBSR, TUNIT,
     *   TEPOCH, CRTYPE, CRVAL, CRINC, CRREF, CRROT, BLANK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Fill in values.
      CATBLK(KIDIM) = NUMAX
      DO 50 I = 1,NUMAX
         CATBLK(KINAX+I-1) = MAX (DIM(I), 1)
         CATD(KDCRV+I-1) = CRVAL(I)
         CATR(KRCIC+I-1) = CRINC(I)
         CATR(KRCRP+I-1) = CRREF(I)
         CATR(KRCRT+I-1) = CRROT(I)
         INDX = KHCTP + (I-1) * 2
         CALL CHR2H (8, CRTYPE(I), 1, CATH(INDX))
 50      CONTINUE
C                                       Fill other character strings.
C                                       Object.
      CALL CHR2H (8, TOBJ, 1, CATH(KHOBJ))
C                                       Telescope.
      CALL CHR2H (8, TTELS, 1, CATH(KHTEL))
C                                       Receiver
      CALL CHR2H (8, TINST, 1, CATH(KHINS))
C                                       Observer's name.
      CALL CHR2H (8, TOBSR, 1, CATH(KHOBS))
C                                       Units
      CALL CHR2H (8, TUNIT, 1, CATH(KHBUN))
      CATBLK(KIPCN) = 0
C                                       User ID
      CATBLK(KIIMU) = NLUSER
C                                       Miscellaneous items.
C                                       Epoch.
      CATR(KREPO) = TEPOCH
C                                       Convolving beam
      CATR(KRBMJ) = 0.0
      CATR(KRBMN) = 0.0
      CATR(KRBPA) = 0.0
C                                       Max. min.
      CATR(KRDMX) = 0.0
      CATR(KRDMN) = 0.0
C                                       Checks.
      IF (CATBLK(KIDIM).LE.0) THEN
         IRET = 1
         MSGTXT = 'NO AXES DEFINED'
         GO TO 990
         END IF
      IF (CATBLK(KINAX).LE.0) THEN
         IRET = 1
         MSGTXT = 'NO PIXELS ON FIRST AXIS'
         GO TO 990
         END IF
      IF (TFORM.EQ.'        ') THEN
         IRET = 1
         MSGTXT = 'NO FORMAT SPECIFIED'
         GO TO 990
         END IF
C                                       Is data to be read in integer
C                                       form?
      ISINT = INDEX (TFORM, 'I') .GT. 0
      FORMAT = '(' // TFORM // ')'
C                                       Finished.
      IRET = 0
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE REDHED (NUMAX, DIM, TFORM, TOBJ, TTELS, TINST, TOBSR,
     *   TUNIT, TEPOCH, CRTYPE, CRVAL, CRINC, CRREF, CRROT, VBLANK,
     *   IRET)
C-----------------------------------------------------------------------
C   REDHED reads the header section of the external text file, assumed
C   to be open and returns the values.  The default values are tahen
C   from the catalog header in DCAT.INC
C   Output:
C      NUMAX   I      Number of axes
C      DIM     I(7)   Dimension of each axis.
C      TFORM   C*8    Format
C      TOBJ    C*8    Object name
C      TTELS   C*8    Telescope name
C      TINST   C*8    Instrument (receiver) name
C      TOBSR   C*8    Instrument name
C      TUNIT   C*8    Units of image
C      TEPOCH  D      Epoch of coordinates
C      CRTYPE  C(7)*8 Axes labels
C      CRVAL   D(7)   Coordinate reference value.
C      CRINC   D(7)   Coordinate increment
C      CRREF   D(7)   Coordinate reference pixel.
C      CRROT   D(7)   Coordinate rotation
C      VBLANK  D      Value of blanked pixel before scaling
C      IRET    I      Error code, 0=OK, else failed.
C   Output in common:
C      SCALE   D      Image scaling factor
C      OFFSET  D      Image offset value.
C-----------------------------------------------------------------------
      INTEGER   NUMAX, DIM(7), IRET
      CHARACTER TFORM*8, TOBJ*8, TTELS*8, TINST*8, TOBSR*8, TUNIT*8,
     *   CRTYPE(7)*8
      DOUBLE PRECISION TEPOCH, CRVAL(7), CRINC(7), CRREF(7), CRROT(7),
     *   VBLANK
C
      CHARACTER PARS(53)*8, ENDMRK*8, VALCH(53)*8
      INTEGER   KMODE, NPARS, I, INDEX,
     *   RNAXIS, RDIM, REPO, RCVAL, RCINC, RCREF, RCROT, RSCAL, ROFF,
     *   RBLNK,
     *   CFORM, COBJ, CTEL, CINS, COBS, CUNI, CTYP
      DOUBLE PRECISION VALS(53)
      INCLUDE 'FETCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C                                       Real data
      DATA PARS/'NAXIS   ','DIM     ','DIM     ','DIM     ', 'DIM     ',
     *   'DIM     ', 'DIM     ', 'DIM     ', 'EPOCH',
     *   'CRVAL   ', 'CRVAL   ', 'CRVAL   ', 'CRVAL   ', 'CRVAL   ',
     *   'CRVAL   ', 'CRVAL   ',
     *   'CRINC   ', 'CRINC   ', 'CRINC   ', 'CRINC   ', 'CRINC   ',
     *   'CRINC   ', 'CRINC   ',
     *   'CRREF   ', 'CRREF   ', 'CRREF   ', 'CRREF   ', 'CRREF   ',
     *   'CRREF   ', 'CRREF   ',
     *   'CRROT   ', 'CRROT   ', 'CRROT   ', 'CRROT   ', 'CRROT   ',
     *   'CRROT   ', 'CRROT   ', 'SCALE   ', 'OFFSET  ', 'BLANK   ',
C                                       Character data
     *   'FORMAT  ', 'OBJECT  ', 'TELESCOP', 'INSTRUME', 'OBSERVER',
     *   'UNITS   ', 'CRTYPE  ', 'CRTYPE  ', 'CRTYPE  ', 'CRTYPE  ',
     *   'CRTYPE  ', 'CRTYPE  ', 'CRTYPE  '/
      DATA ENDMRK /'/       '/
      DATA NPARS, KMODE /53, 0/
C                                       Pointers
      DATA RNAXIS, RDIM, REPO, RCVAL, RCINC, RCREF, RCROT, RSCAL, ROFF,
     *   RBLNK
     *   /   1,      2,   9,    10,    17,    24,     31,   38,    39,
     *   40/
      DATA CFORM, COBJ, CTEL, CINS, COBS, CUNI, CTYP
     *   /   41,   42,   43,   44,   45,   46,   47/
C-----------------------------------------------------------------------
C                                       Get defaults from CATBLK
C                                       Telescope.
      CALL H2CHR (8, 1, CATH(KHTEL), VALCH(CTEL))
C                                       Receiver
      CALL H2CHR (8, 1, CATH(KHINS), VALCH(CINS))
C                                       Observer's name.
      CALL H2CHR (8, 1, CATH(KHOBS), VALCH(COBS))
C                                       Units
      CALL H2CHR (8, 1, CATH(KHBUN), VALCH(CUNI))
C                                       Object name
      CALL H2CHR (8, 1, CATH(KHOBJ), VALCH(COBJ))
C                                       Number of axes
      VALS(RNAXIS) = CATBLK(KIDIM)
C                                       Epoch
      VALS(REPO) = CATR(KREPO)
C                                       Scaling and offset.
      VALS(RSCAL) = SCALE
      VALS(ROFF) = OFFSET
C                                       Blanking
      VALS(RBLNK) = 123456789
C                                       Format
      VALCH(CFORM) = '    '
      DO 10 I = 1,7
C                                       Axis dimension
         VALS(RDIM+I-1) = CATBLK(KINAX+I-1)
C                                       Coordinate reference value
         VALS(RCVAL+I-1) = CATD(KDCRV+I-1)
C                                       Coordinate increment
         VALS(RCINC+I-1) = CATD(KRCIC+I-1)
C                                       Coordinate reference pixel
         VALS(RCREF+I-1) = CATR(KRCRP+I-1)
C                                       Coordinate rotation
         VALS(RCROT+I-1) = CATR(KRCRT+I-1)
C                                       Axis type (label)
         INDEX = KHCTP + (I-1) * 2
         CALL H2CHR (8, 1, CATH(INDEX), VALCH(CTYP+I-1))
 10      CONTINUE
C                                       Read file.
      CALL KEYIN (PARS, VALS, VALCH, NPARS, ENDMRK, KMODE, LUNIN,
     *   FINDIN, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Set output values.
      TTELS = VALCH(CTEL)
      TINST = VALCH(CINS)
      TOBSR = VALCH(COBS)
      TUNIT = VALCH(CUNI)
      TOBJ = VALCH(COBJ)
      TFORM = VALCH(CFORM)
      NUMAX = VALS(RNAXIS) + 0.5
      TEPOCH = VALS(REPO)
      SCALE = VALS(RSCAL)
      OFFSET = VALS(ROFF)
      VBLANK = VALS (RBLNK)
      DO 50 I = 1,7
         DIM(I) = VALS(RDIM+I-1) + 0.5
         CRVAL(I) = VALS(RCVAL+I-1)
         CRINC(I) = VALS(RCINC+I-1)
         CRREF(I) = VALS(RCREF+I-1)
         CRROT(I) = VALS(RCROT+I-1)
         CRTYPE(I) = VALCH(CTYP+I-1)
 50      CONTINUE
C                                       Error
      GO TO 999
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('REDHED: ERROR =',I3,' FROM KEYIN')
      END
      SUBROUTINE GETMA (IPOS, RESULT, IRET)
C-----------------------------------------------------------------------
C   Routine to read and optionally scale the data from the external
C   file.
C   Inputs:
C      IPOS   I(7)    BLC (input image) of first value in DATA
C  Values from commons:
C      SCALE   D   Scaling factor
C      OFFSET  D   Offset
C      ISINT   L   If true then data in integer form.
C   Output:
C      RESULT  R(*)  Output row.
C     IRET     I     Return code   0 => OK
C                               >0 => error, terminate.
C  Output in COMMON:
C  NUMHIS  I          # history entries (max. 10)
C  HISCRD  C(NUMHIS)  History records
C  CATBLK  I          Catalog header block
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), IRET
      REAL      RESULT(*)
C
      INCLUDE 'FETCH.INC'
      INTEGER   I, NX, IBUFF(MABFSS), LINMAX, IBLANK
      REAL      RBLANK
      CHARACTER LINE*65536
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LINMAX /MAXIMG/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check if last call
      IF (IPOS(1).LT.0) GO TO 900
C                                       Set length of a row.
      NX = CATBLK(KINAX)
C                                       Blanking
      IF (BLANK.GT.0.0D0) THEN
         IBLANK = BLANK + 0.5
      ELSE
         IBLANK = BLANK - 0.5
         END IF
      RBLANK = BLANK
C                                       Check that we can read all.
      IF (NX.GT.LINMAX) THEN
         WRITE (MSGTXT, 2000) NX, LINMAX
         CALL MSGWRT (8)
         IRET = 1
         GO TO 999
         ENDIF
C
      IRET = 1
      CALL ZTXIO ('READ', LUNIN, FINDIN, LINE, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Integer?
      IF (ISINT) THEN
         READ (LINE,FORMAT,ERR=200) (IBUFF(I), I=1,NX)
         DO 50 I = 1,NX
            IF (IBUFF(I).NE.IBLANK) THEN
               RESULT(I) = IBUFF(I)
            ELSE
               RESULT(I) = FBLANK
               END IF
 50         CONTINUE
C                                       Real
      ELSE
         READ (LINE,FORMAT,ERR=200) (RESULT(I), I=1,NX)
         DO 60 I = 1,NX
            IF (RESULT(I).EQ.RBLANK) RESULT(I) = FBLANK
 60         CONTINUE
         END IF
      IRET = 0
C                                       Scaling?
      IF ((ABS (SCALE-1.0D0).GT.1.0D-5) .OR.
     *   (ABS (OFFSET).GT.1.0D-5)) THEN
         DO 100 I = 1,NX
            IF (RESULT(I).NE.FBLANK) RESULT(I) = RESULT(I) * SCALE +
     *         OFFSET
 100        CONTINUE
         END IF
C                                       Finished
      GO TO 999
C                                       Error decoding line
 200  IRET = 9
      MSGTXT = 'ERROR DECODING LINE:'
      CALL MSGWRT (8)
      MSGTXT = LINE(1:50)
      CALL MSGWRT (8)
      MSGTXT = LINE(51:100)
      CALL MSGWRT (8)
      GO TO 999
C                                       Last call - do history etc.
 900  CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 2000 FORMAT ('GETMA: ROW LENGTH = ',I5,
     *            ' IS LARGER THAN ALLOWED (= ',I5,')')
      END
