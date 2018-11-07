LOCAL INCLUDE 'UVFIL.INC'
C                                       Local include UVFIL
      INCLUDE 'INCS:ZPBUFSZ.INC'
C                                       Global variables and commons
C                                       are defined here.
      INTEGER   SEQOUT, DISKO, NUMHIS, JBUFSZ, NANT
      HOLLERITH XINFIL(12), XIN2FI(12), XNAMOU(3), XCLAOU(2)
      CHARACTER INFILE*48, IN2FIL*48, NAMOUT*12, CLAOUT*6,
     *   HISCRD(10)*64, ANTNAM(30)*8
      REAL      XSOUT, XDISO, APARM(10), BPARM(10), BUFFER(UVBFSS),
     *   XIAT, XUT1
      DOUBLE PRECISION ANTLOC(3,30), GST0
C                                       Antenna information (ex char.)
      COMMON /ANTS/ ANTLOC, GST0, XIAT, XUT1, NANT
C                                       I/O buffer
      COMMON /BUFRS/ BUFFER, JBUFSZ
C                                       AIPS adverbs passed
      COMMON /INPARM/ XINFIL, XIN2FI, XNAMOU, XCLAOU, XSOUT, XDISO,
     *   APARM, BPARM, SEQOUT, DISKO, NUMHIS
C                                       Various CHARACTER variables
      COMMON /CHRCOM/ INFILE, IN2FIL, NAMOUT, CLAOUT, HISCRD, ANTNAM
LOCAL END
      PROGRAM UVFIL
C-----------------------------------------------------------------------
C! User redefinable task to create an uv data file from external data.
C# UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2000, 2009, 2015
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
C   UVFIL creates an AIPS uv data base and fills the data base thru
C   calls to a user provided routine.  See the UVFIL EXPLAIN file
C   or Going AIPS  for better descriptions of the UV data
C   file and catalog header structure and the contents and structure
C   of the antenna (AN) table.  There are sufficient comments in
C   NEWHED and FIDDLE to describe simple cases.
C       The current version of UVFIL has a sample version of
C   statments to read control information and data from
C   text files as a guide for other implementations.  These sections
C   are marked 'SAMPLE CODE' and delimited by a row of '*''s
C
C   IMPORTANT NOTE: to avoid confusion this task should be renamed.
C   To rename (max. 5 char) and install the new task :
C     1) copy the source code to a new file with the name newname.
C        then add desired code to subroutines FIDDLE and NEWHED
C     2) using the source editor change all references to UVFIL to
C        newname.  It is especially important to change the string
C        DATAed into array PRGM in the main routine to the new name.
C     3) copy .HLP file for UVFIL to .HLP file for newname.
C     4) compile and link edit with the APL and NOTST subroutine
C        libraries from AIPS.
C
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INFILE         INFILE        Input file name.
C      IN2FILE        IN2FIL        Name of aux. input file.
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is source name
C      OUTCLASS       CLAOUT        Class of the output uv file.
C                                   Default 'UVDATA'
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      APARM(10)      APARM         User specified array.
C      BPARM(10)      BPARM         User specified array.
C-----------------------------------------------------------------------
      INTEGER  IRET
      CHARACTER PRGM*6
      INCLUDE 'UVFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
C
C                 Change this DATA stmt. if changing the PROGRAM name.
C
      DATA PRGM /'UVFIL '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UVFILN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL GETUV (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL FILHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE UVFILN (PRGN, JERR)
C-----------------------------------------------------------------------
C   UVFILN gets input parameters for UVFIL and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                               4 => error creating output file.
C                               8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in UVFIL for more details.
C
C   To change the adverb list sent to this task change:
C   1)  the inputs file (task_name.HLP).
C   2)  the contents of COMMON /INPARM/.  Remember all adverbs are sent
C       as reals; character data is sent as HOLLERITH variables and
C       needs to be converted to CHARACTER using H2CHR.
C       Values will be filled into COMMON /INPARM/ in the order
C       specified in the inputs file.
C   3)  If the first adverb is not XINFIL then replace
C       XINFIL in the call to GTPARM with the name of the first
C       adverb.
C   4)  Change the value of NPARM sent to GTPARM to the number of
C       words desired.
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER OLDNAM*12, BLANK*6, DEFNAM*12
      INTEGER   IERR, NPARM, IROUND
      INCLUDE 'UVFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA BLANK /'      '/
      DATA DEFNAM /'UV DATA FILE'/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 51
      CALL GTPARM (PRGN, NPARM, RQUICK, XINFIL, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
      IF (JERR.NE.0) GO TO 999
C                                       Crunch input parameters.
      SEQOUT = IROUND (XSOUT)
      DISKO = IROUND (XDISO)
C                                       Convert AIPS character adverbs
C                                       from HOLLERITH to CHARACTER.
      CALL H2CHR (48, 1, XINFIL, INFILE)
      CALL H2CHR (48, 1, XIN2FI, IN2FIL)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C                                       Create new header.
      CALL NEWHED (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Get uv header info and
C                                       verify header structure.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Put new values in CATBLK.
C                                       Get naming defaults
      OLDNAM = DEFNAM
      CALL MAKOUT (OLDNAM, BLANK, 0, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
C                                       File type ='UV'
      CALL CHR2H (2, 'UV', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = SEQOUT
C                                       Create output file.
      CCNO = 1
      CALL UVCREA (DISKO, CCNO, BUFFER, IERR)
      IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR
            JERR = 4
            GO TO 990
            END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = 2
C                                       Get SEQ. no. used.
      SEQOUT = CATBLK(KIIMS)
      JERR = 0
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVFILN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE GETUV (IRET)
C-----------------------------------------------------------------------
C   GETUV reads uv data one point at a time from the user supplied
C   routine FIDDLE and then writes the data.
C   Output:
C      IRET   I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER OFILE*48
      INTEGER   IPTRO, LUNO, LENBU, IA1, IA2, NIOUT, INDO, LRECO, KBIND,
     *   VO, BO, NUMVIS, XCOUNT
      REAL      DUM
      LOGICAL   T, F
      INCLUDE 'UVFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNO /16/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
C                                       LRECO = length of output rec.
      LRECO = LREC
      LENBU = 1
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, LENBU, JBUFSZ,
     *   BUFFER, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NUMVIS = 0
      XCOUNT = 0
C                                       ant numbers should be set by
C                                       FIDDLE but just in case:
      IA1 = 1
      IA2 = 2
C                                       Loop
 100  CONTINUE
         NUMVIS = NUMVIS + 1
C                                      Call user routine.
         CALL FIDDLE (NUMVIS, BUFFER(IPTRO+ILOCU),
     *      BUFFER(IPTRO+ILOCV), BUFFER(IPTRO+ILOCW),
     *      BUFFER(IPTRO+ILOCT), IA1, IA2, BUFFER(IPTRO+NRPARM),
     *      BUFFER(IPTRO), IRET)
C                                       Fill in baseline.
         IF (ILOCB.GE.0) THEN
            BUFFER(IPTRO+ILOCB) = IA1 * 256 + IA2
         ELSE
            BUFFER(IPTRO+ILOCA1) = IA1
            BUFFER(IPTRO+ILOCA2) = IA2
            BUFFER(IPTRO+ILOCSA) = 1.0
            END IF
C                                       Error (fatal)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1120) IRET
            GO TO 990
C                                       Got datum
         ELSE IF (IRET.EQ.0) THEN
            XCOUNT = XCOUNT + 1
            IPTRO = IPTRO + LRECO
            NIOUT = NIOUT + 1
C                                       Write vis record.
            CALL UVDISK ('WRIT', LUNO, INDO, BUFFER, NIOUT, KBIND, IRET)
C                                       Check for end.
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1150) IRET
               GO TO 990
               END IF
            IPTRO = KBIND
            NIOUT = 0
C                                       Next vis.
            GO TO 100
            END IF
C                                       Final call to FIDDLE.
      NUMVIS = -1
      CALL FIDDLE (NUMVIS, DUM, DUM, DUM, DUM, IA1, IA2, BUFFER,
     *   BUFFER, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFFER, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       Close file
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('GETUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1120 FORMAT ('GETUV: FIDDLE ERROR',I3)
 1150 FORMAT ('GETUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE FILHIS
C-----------------------------------------------------------------------
C   FILHIS creates and fills a history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, LABEL*8, ATIME*8, ADATE*12, TELE*8, OBSR*8
      INTEGER   LUN, I, IERR, TIME(3), DATE(3)
      LOGICAL   T
      INCLUDE 'UVFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA LUN /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Create/open hist. file.
      CALL HICREA (LUN, DISKO, CCNO, CATBLK, BUFFER, IERR)
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
      WRITE (HILINE,1011) TSKNAM, INFILE
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,1012) TSKNAM, IN2FIL
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN,
     *   BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       SOURCE
      WRITE (HILINE,2000) TSKNAM, SOURCE
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Number of visibilities
      WRITE (HILINE,2001) TSKNAM, CATBLK(KIGCN)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Telescope, observer name.
      CALL H2CHR (8, 1, CATH(KHTEL), TELE)
      CALL H2CHR (8, 1, CATH(KHOBS), OBSR)
      WRITE (HILINE,2002) TSKNAM, TELE, OBSR
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       AIPS release
      WRITE (HILINE,2003) TSKNAM, RLSNAM
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any user supplied history.
      IF (NUMHIS.GT.0) THEN
         WRITE (LABEL,1050) TSKNAM
         DO 50 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 200
 50         CONTINUE
         END IF
C                                       Close HI file
 200   CALL HICLOS (LUN, T, BUFFER, IERR)
C                                       Write ANtenna file.
      CALL ANTFIL
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISKO, CCNO, CATBLK, 'REST', BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FILHIS: ERROR',I3,' CREATE/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ Image created by user',I5,' at ',A12,2X,A8)
 1011 FORMAT (A6,'INFILE =''',A,'''')
 1012 FORMAT (A6,'IN2FILE=''',A,'''')
 1050 FORMAT (A6,' /')
 2000 FORMAT (A6,' SOURCE = ''',A,'''')
 2001 FORMAT (A6,' / Number of visibilities copied=',I9)
 2002 FORMAT (A6,' / Telescope = ',A8,' Observer = ',A8)
 2003 FORMAT (A6,' Release = ''',A7,' ''')
      END
      SUBROUTINE ANTFIL
C-----------------------------------------------------------------------
C   ANTFIL creates and fills the antenna file.
C-----------------------------------------------------------------------
      INTEGER   IERR, VER, IABUF(1), LUN, I
      DOUBLE PRECISION JD, GMSTM, GASTM, RATE
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (BUFFER, IABUF)
      DATA LUN /27/
C-----------------------------------------------------------------------
C                                       Make sure there is antenna info
      IF (NANT.LE.0) GO TO 999
C                                       Setup for AN table initization
         NUMORB = 0
         NOPCAL = 2
C                                       Position of the earth's pole
         POLRXY(1) = 0.0
         POLRXY(2) = 0.0
         UT1UTC = XUT1
         DATUTC = XIAT
C                                       Array name
         ANAME = '        '
C                                       Array center (rel to center of
C                                       earth)
         ARRAYC(1) = 0.0D0
         ARRAYC(2) =  0.0D0
         ARRAYC(3) =  0.0D0
C                                       Get GST0 and Earth rotation rate
         CALL H2CHR (8, 1, CATH(KHDOB), RDATE)
         IF (ABS (GST0).LT.1.0E-20) CALL JULDAY (RDATE, JD)
         IF (ABS (GST0).LT.1.0E-20)
     *      CALL GSTROT (JD, GMSTM, GASTM, RATE)
         GSTIA0 = GST0
         IF (ABS (GST0).LT.1.0E-20) GSTIA0 = GMSTM
         DEGPDY = RATE
         SAFREQ = FREQ
         ANFQID = -1
         TIMSYS = 'UTC'
         VER = 1
         XYZHAN = ' '
         TFRAME = ' '
         ANTNIF = 1
C                                       Create/init file
         CALL ANTINI ('WRIT', BUFFER, DISKO, CCNO, VER, CATBLK, LUN,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       init basic AN record
         ANNAME = ' '
         STAXOF = 0.0
         STAXYZ(1) = 0.0D0
         STAXYZ(2) = 0.0D0
         STAXYZ(3) = 0.0D0
         ORBPRM(1) = 0.0D0
         NOSTA = 0
         MNTSTA = 0
         POLAA = 0.0
         POLAB = 0.0
         CALL RFILL (3, 0.0, POLCA)
         CALL RFILL (3, 0.0, POLCB)
         POLTYA = 'R'
         POLTYB = 'L'
         DIAMAN = 0.0
         FWHMAN(1) = 0.0
C                                       AN records
         DO 20 I = 1,NANT
            STAXYZ(1) = ANTLOC(1,I)
            STAXYZ(2) = ANTLOC(2,I)
            STAXYZ(3) = ANTLOC(3,I)
            NOSTA = I
            ANNAME = ANTNAM(I)
            IANRNO = I
            CALL TABAN ('WRIT', BUFFER, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
            IF (IERR.NE.0) GO TO 990
 20         CONTINUE
C                                       Fill in header and close
         CALL TABIO ('CLOS', 1, IANRNO, ANNAME, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 990
         GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1020) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('ERROR ',I3,' OCCURED WRITING ANTENNA FILE')
      END
      SUBROUTINE NEWHED (IRET)
C-----------------------------------------------------------------------
C   NEWHED is a routine in which the catalog header is constructed.
C   Necessary values can be read in in the areas markes "USER CODE
C   GOES HERE".
C
C   NOTE: the AIPS convention for the coordinate reference value
C   for the STOKES axis is that 1,2,3,4 represent I, Q, U, V
C   stokes' parameters and -1,-2,-3,-4 represent RR, LL, RL and
C   LR correlator values.  Currently set for R and L polarization
C   ie Ref. value = -1 and increment = -1.
C
C   The MINIMUM information required here is that
C   required to define the size of the output file; ie.
C      CATBLK(KIGCN)   = I   number of visibility records
C      CATBLK(KIPCN) = Number of random parameters.
C      CATBLK(KIDIM)= the number of axes,
C      CATBLK(KINAX+i) = the dimension of each axis.
C   Other changes can be made either here or in FIDDLE; the
C   catalog block will be updated when the history file is
C   written.
C      The antenna information can also be entered in this
C   routine.  It is possible to put much more information in the
C   ANtenna file, see Going AIPS for details.
C
C    Input:
C     CATBLK(256)    I     Output catalog header, also CATR, CATH, CATD
C                          The OUTNAME, OUTCLASS, OUTSEQ are entered
C                          elsewhere.
C    Output:
C     CATBLK(256)    I     Modified output catalog header.
C     IRET           I     Return error code, 0=>OK, otherwise abort.
C    Also the antenna informtion can be filled into a common.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER RTYPES(9)*8, TYPES(7)*8, UNITS*8, TELE*8, OBSR*8,
     *   INSTR*8, OBSDAT*8
C     CHARACTER LINE*80
      INTEGER   I, NAXIS, NRAN, NCHAN, NPOLN, NDIM(7), INDEX, XCOUNT,
     *   LUN, FIND
      LOGICAL   APPEND
      REAL      CRINC(7), CRPIX(7), EPOCH, BANDW
      DOUBLE PRECISION CRVAL(7)
      INCLUDE 'UVFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
C                                       User definable values
C                                       Random parameters.
C                                       No. random parameters.
      DATA NRAN /7/
C                                       Rand. parm. names.
C                                       with encoded baseline
C     DATA RTYPES /'UU-L-SIN','VV-L-SIN','WW-L-SIN','TIME1',
c    *   'BASELINE', 2*' '/
C                                       with separate antennas, subarray
      DATA RTYPES /'UU-L-SIN', 'VV-L-SIN', 'WW-L-SIN', 'TIME1',
     *   'SUBARRAY', 'ANTENNA1', 'ANTENNA2', 2*' '/
C                                       Uniform axes.
C                                         No. axes.
      DATA NAXIS /5/
C                                         Axes names.
      DATA TYPES /'COMPLEX ','STOKES  ','FREQ    ',
     *   'RA      ','DEC     ',2*'        '/
C                                         Axis dimensions
      DATA NDIM /3,1,1,1,1,0,0/
C                                         Reference values
      DATA CRVAL /1.0D0, -1.0D0, 5*0.0D0/
C                                         Reference pixel.
      DATA CRPIX /7*1.0/
C                                         Coordinate increment.
      DATA CRINC /1.0, -1.0, 0.0, 0.0, 0.0, 2*0.0/
C                                       Epoch of position.
      DATA EPOCH /1950.0/
C                                       Units
      DATA UNITS /'JY      '/
C-----------------------------------------------------------------------
      NANT = 0
      GST0 = 0.0D0
      XIAT = 0.0
      XUT1 = 0.0
C                                       Zero fill CATBLK
      CALL CATINI (CATBLK)
C                                       Fill axis arrays.
C                                       Random axis names
      DO 10 I = 1,NRAN
         INDEX = KHPTP + (I-1) * 2
         CALL CHR2H (8, RTYPES(I), 1, CATH(INDEX))
 10      CONTINUE
C                                       Uniform axes
      DO 30 I = 1,KICTPN
C                                       Init dimension
         CATBLK(KINAX+I-1) = NDIM(I)
C                                       Init. increment.
         CATR(KRCIC+I-1) = CRINC(I)
C                                       Init. rotation.
         CATR(KRCRT+I-1) = 0.0
C                                       Init. ref pixel.
         CATR(KRCRP+I-1) = CRPIX(I)
C                                       Init. ref value.
         CATD(KDCRV+I-1) = CRVAL(I)
C                                       Fill axis type from
C                                       TYPES
         INDEX = KHCTP + (I-1) * 2
         CALL CHR2H (8, TYPES(I), 1, CATH(INDEX))
 30      CONTINUE
C                                       Fill in values.
C                                       Fill other character strings.
C                                       Source name
      SOURCE = '        '
C                                       Observation date. 'DD/MM/YY'
      OBSDAT = '        '
C                                       Telescope.
      TELE = '        '
C                                       Receiver
      INSTR = '        '
C                                       Observer's name.
      OBSR = '        '
C                                       Set number of axes.
      CATBLK(KIDIM) = NAXIS
      CATBLK(KIPCN) = NRAN
C                                       Miscellaneous items.
C                                       Epoch.
      CATR(KREPO) = EPOCH
C                                       Sort order ('**'=>unsorted)
      CALL CHR2H (2, '**', 1, CATH(KITYP))
C                                       Units
      CALL CHR2H (8, UNITS, 1, CATH(KHBUN))
C-----------------------------------------------------------------------
C                                       Enter values for data:
C                                       XCOUNT = number of vis. record.
C                                       RA = Right ascension (1950)
C                                          in degrees.
C                                       DEC = Declination in degrees.
C                                       FREQ = frequency of obs in Hz.
C                                       BANDW = bandwidth or channel
C                                           separation.
C                                       NCHAN = Number of freq chan.
C                                       NPOLN = number of polarization
C                                            correlators.
C                                       OBSDAT = Reference date of time
C                                          tags for data as "yyyymmdd"
C                                       SOURCE = source name (8 char)
C                                       TELE = telescope name (8 char)
C                                       INSTR = Receiver name (8char)
C                                       OBSR = Observers name (8 char)
C                                       Just in case you forget.
      XCOUNT = 1
      RA = 0.0D0
      DEC = 0.0D0
      FREQ = 1.0D0
      BANDW = 0.0
      NCHAN = 1
      NPOLN = 1
C                                       Open INFILE on lun 10
      LUN = 10
      APPEND = .FALSE.
      CALL ZTXOPN ('READ', LUN, FIND, INFILE, APPEND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
C
C
C      USER CODE GOES HERE
C
C***********************************************************************
C   SAMPLE CODE
C
C      CALL ZTXIO ('READ', LUN, FIND, LINE, IRET)
C      IF (IRET.NE.0) THEN
C         WRITE (MSGTXT,1031) IRET
C         GO TO 990
C         END IF
C      READ (LINE,2001) XCOUNT, RA, DEC, FREQ, OBSDAT, BANDW
C 2001 FORMAT (I10,3E12.3,4X,A8,E12.3)
C      CALL ZTXIO ('READ', LUN, FIND, LINE, IRET)
C      IF (IRET.NE.0) THEN
C         WRITE (MSGTXT,1031) IRET
C         GO TO 990
C         END IF
C      READ (LINE,2002) NCHAN, NPOLN, SOURCE, TELE, INSTR, OBSR
C 2002 FORMAT (2I5, 4(2X,A8))
C***********************************************************************
C                                       Insert values in header.
C                                       Number of vis.
      CATBLK(KIGCN) = XCOUNT
C                                       Position.
      CATD(KDCRV+3) = RA
      CATD(KDCRV+4) = DEC
C                                       Frequency
      CATD(KDCRV+2) = FREQ
C                                       Bandwidth.
      CATR(KRCIC+2) = BANDW
C                                       Number of frequencies.
      CATBLK(KINAX+2) = NCHAN
C                                       Number of polarizations.
      CATBLK(KINAX+1) = NPOLN
C                                       Observing date.
      CALL CHR2H (8, OBSDAT, 1, CATH(KHDOB))
C                                       Object.
      CALL CHR2H (8, SOURCE, 1, CATH(KHOBJ))
C                                       Telescope.
      CALL CHR2H (8, TELE, 1, CATH(KHTEL))
C                                       Receiver
      CALL CHR2H (8, INSTR, 1, CATH(KHINS))
C                                       Observer's name.
      CALL CHR2H (8, OBSR, 1, CATH(KHOBS))
C-----------------------------------------------------------------------
C                                       Enter antenna / time info
C                                       GST0 = Greenwich sidereal time
C                                         (in degrees) at midnight
C                                         on the reference date
C                                         (OBSDAT)
C                                       XIAT = 0 (time in UTC)
C                                       XUT1 = The UT1-UTC time
C                                          correction for ref. date.
C                                       NANT = number of antennas.
C                                       ANTLOC(i,n) is the position of
C                                          antenna n in meters from
C                                          the array reference center.
C                                          i=1=X, i=2=Y, I=3=Z.
C                                          (see AIPS manual Vol 2)
C                                       ANTNAM(n) is the antenna
C                                          name, up to 8 char.
C
C
C
C     USER CODE GOES HERE
C***********************************************************************
C   SAMPLE CODE
C
C      CALL ZTXIO ('READ', LUN, FIND, LINE, IRET)
C      IF (IRET.NE.0) THEN
C         WRITE (MSGTXT,1031) IRET
C         GO TO 990
C         END IF
C      READ (LINE,2003) NANT, GST0, XIAT, XUT1
C 2003 FORMAT (I5,3F15.5)
C      DO 3000 I = 1,NANT
C         CALL ZTXIO ('READ', LUN, FIND, LINE, IRET)
C         IF (IRET.NE.0) THEN
C            WRITE (MSGTXT,1031) IRET
C            GO TO 990
C            END IF
C         READ (LINE,2004) ANTNAM(I),
C     *      ANTLOC(1,I), ANTLOC(2,I), ANTLOC(3,I)
C 3000    CONTINUE
C 2004 FORMAT (A8,3E15.5)
C***********************************************************************
C                                       Close INFILE
      CALL ZTXCLS (LUN, FIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1032) IRET
         GO TO 990
         END IF
C
C                                       Finished.
      IRET = 0
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('NEWHED: ERROR',I3,' OPENING INPUT FILE')
C1031 FORMAT ('NEWHED: ERROR',I3,' READING INPUT FILE')
 1032 FORMAT ('NEWHED: ERROR',I3,' CLOSING INPUT FILE')
      END
      SUBROUTINE FIDDLE (NUMVIS, U, V, W, T, IA1, IA2, VIS, RPARM, IRET)
C-----------------------------------------------------------------------
C  This is a skeleton version of subroutine FIDDLE which allows the
C  user to create a UV data base.  Visibilities are returned one at
C  a time and are written on the output file.
C
C       Up to 10 history entries can be written by using WRITE to
C  record up to 64 characters per entry into array HISCRD. Ex:
C     WRITE (HISCRD(entry #),format #) list
C  The history is written after the last call to FIDDLE.
C
C       Messages can be written to the monitor/logfile by writing
C  the message (up to 80 char) into array MSGTXT in COMMON /MSGCOM/
C  and then issuing a call:
C     CALL MSGWRT (priority #)
C
C       Unit 1 is the line printer
C
C       If IRET .GT. 0 then the output file will be destroyed.
C       A value of IRET .lt. 0 indicates the end of the data.
C
C
C       See the precursor comments for UVPGET for a description
C  of the contents of COMMON /UVHDR/ which allows easy access to
C  much of the information from the catalog header (CATBLK) and
C  which describes the order in which the data is being written.
C
C       After all data has been processed a final call will be made to
C  FIDDLE with NUMVIS = -1.  This is to allow for the completion of
C  pending operations, i.e. preparation of HIstory cards.
C
C     AIPS I/O  LUN 16 is open and not available to FIDDLE.
C  FORTRAN unit numbers greater than 50 will probably not get the
C  AIPS routines confused.  (Any unit numbers other that 1 and 5
C  will probably also work.)
C
C       The current contents of CATBLK will be written back to the
C  catalog after the last call to FIDDLE.
C
C  Inputs:
C     NUMVIS     I    Visibility number, -1 => final call, no data
C                     passed but allows any operations to be completed.
C
C  Inputs from COMMON:
C     IN2FIL  C*48  Name of the aux. file
C     APARM   R(10) User array.
C     BPARM   R(10) User array.
C     RA         D  Right ascension (1950) of phase center. (deg)
C     DEC        D  Declination (1950) of phase center. (deg)
C     FREQ       D  Frequency of observation (Hz)
C     NRPARM     I  # random parameters.
C     NCOR       I  # correlators
C     CATBLK(256)I  Catalog header record. See [DOC]HEADER for details
C
C  Output:
C     U       R       U in wavelengths at the reference frequency.
C     V       R       V in wavelengths
C     W       R       W in wavelengths
C     T       R       Time in days since the midnight at the start of
C                     the reference date.
C     IA1     I       Antenna number of the first antenna.
C     IA2     I       Antenna number of the second antenna.
C                     NOTE: IA2 MUST be greater that IA1
C     RPARM   R       Modified random parameter array. NB U,V,W,
C                     time and baseline should not be modified in RPARM
C     VIS     R(3,*)  Visibilities.  The first dimension is the COMPLEX
C                     axis in the order Real part, Imaginary part,
C                     weight. The order of the following visibilities is
C                     defined by variables in COMMOM /UVHDR/ (originally
C                     specified in NEWHDR).  The order number for Stokes
C                     parameters is JLOCS and the order number for
C                     frequency is given by JLOCF.  The lower order
C                     number  increases faster in the array.
C                     See precursor comments in UVPGET for more details.
C     IRET    I       Return code  -1 => End of data.
C                             0 => OK
C                            >0 => error, terminate.
C
C  Output in COMMON:
C     NUMHIS   I          # history entries (max. 10)
C     HISCRD   C(NUMHIS)  History records
C     CATBLK   I          Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IA1, IA2, IRET
      REAL      U, V, W, T, VIS(3,*), RPARM(*)
C
C      CHARACTER LINE*80
C      INTEGER   LUN, FIND
C      LOGICAL   APPEND
      INCLUDE 'UVFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
C***********************************************************************
C   SAMPLE CODE
C
C      CHARACTER ANTSYM*20
C      INTEGER   IDAY0, IDAY, IYR, IYR0, IUT, IHR, IMN, IS1, IS2,
C     *   I, IT
C      REAL      AMP, PHASE
C      SAVE LUN, FIND
C      DATA ANTSYM /'ABCDEFGHIJKLNMOPQRST'/
C***********************************************************************
C-----------------------------------------------------------------------
      IRET = 0
C
C    USER CODE GOES HERE
C
C
C
C
C
C
C***********************************************************************
C   SAMPLE CODE
C
C                                       Check NUMVIS
C      IF (NUMVIS-1) 500, 10, 100
C                                       Initialize I/O etc.
C                                       Open IN2FIL on lun 10
C 10   LUN = 10
C      APPEND = .FALSE.
C      CALL ZTXOPN ('READ', LUN, FIND, INFILE, APPEND, IRET)
C      IF (IRET.NE.0) THEN
C         WRITE (MSGTXT,1030) IRET
C         GO TO 990
C         END IF
C      CALL ZTXIO ('READ', LUN, FIND, LINE, IRET)
C      IF (IRET.NE.0) THEN
C         WRITE (MSGTXT,1031) IRET
C         GO TO 990
C         END IF
C         READ (LINE,2000) IYR0,IDAY0
C                                       Read and crunch data.
C 100  CALL ZTXIO ('READ', LUN, FIND, LINE, IRET)
C      IF (IRET.NE.0) THEN
C         WRITE (MSGTXT,1031) IRET
C         GO TO 990
C         END IF
C      READ (LINE,2001,END=600) IYR,IDAY,IUT,IS1,IS2,U,V,AMP,PHASE
C      IHR = IUT / 100
C      IMN = IUT - IHR * 100
C      T = (IDAY - IDAY0) + IHR/24. + IMN/1440. + XIAT
C                                       Find antenna numbers.
C      IA1 = NANT
C      IA2 = NANT+1
C      DO 110 I = 1,NANT
C         IF (IS1.EQ.ANTSYM(I:I)) IA1 = I
C         IF (IS2.EQ.ANTSYM(I:I)) IA2 = I
C 110     CONTINUE
C      U = U * 1.0E6
C      V = V * 1.0E6
C      W = 0.0
C      PHASE = PHASE * 1.74533E-2
C      VIS(1,1) = AMP * COS (PHASE)
C      VIS(2,1) = AMP * SIN (PHASE)
C      VIS(3,1) = 1.0
C                                       Check if need to reverse bl.
C      IF (IA2.GT.IA1) GO TO 999
C                                       Flip baseline.
C         IT = IA1
C         IA1 = IA2
C         IA2 = IT
C         U = -U
C         V = -V
C         VIS(2,1) = - VIS(2,1)
C         GO TO 999
C                                       Last call, do history.
C 500  CONTINUE
C      NUMHIS = 1
C      WRITE (HISCRD(NUMHIS),1500) NANT
C      GO TO 600
C                                       End of data.
C 600  IRET = -1
C                                       Close IN2FIL
C      CALL ZTXCLS (LUN, FIND, IRET)
C      IF (IRET.NE.0) THEN
C         WRITE (MSGTXT,1032) IRET
C         GO TO 990
C         END IF
C      GO TO 999
C
C
 999  RETURN
C-----------------------------------------------------------------------
C   FORMAT STATEMENTS GO HERE
C 1030 FORMAT ('FIDDLE: ERROR',I3,' OPENING INPUT FILE')
C 1031 FORMAT ('FIDDLE: ERROR',I3,' READING INPUT FILE')
C 1032 FORMAT ('FIDDLE: ERROR',I3,' CLOSING INPUT FILE')
C 2000 FORMAT (3X,I3,I3)
C 2001 FORMAT (3X,I3,I3,I5,7X,2A1,1X,2F8.2,13X,F6.3,7X,F5.0)
C 1500 FORMAT (' Number of antennas =',I5)
      END
