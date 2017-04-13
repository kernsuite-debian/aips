LOCAL INCLUDE 'UVSIM.INC'
C                                       Local include for UVSIM
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   NMAX
      PARAMETER (NMAX = 100)
      INTEGER   CATBLK(256), SEQOUT, DISKO, NUMHIS, JBUFSZ, N
      HOLLERITH XINFIL(12), XNAMOU(3), XCLAOU(2)
      CHARACTER INFILE*48, NAMOUT*12, CLAOUT*6,
     *   HISCRD(NMAX+11)*64
      REAL      XSOUT, XDISO, APARM(10), BUFFER(UVBFSS)
      DOUBLE PRECISION SITELA, DELTA, HMIN, HMAX, ELMIN, BLKMAX,
     *   TAU, BLKREJ, FREQNM, LX(NMAX), LY(NMAX), LZ(NMAX), DIAM(NMAX)
      COMMON /BUFRS/ BUFFER, JBUFSZ
      COMMON /INPARM/ XINFIL, XNAMOU, XCLAOU, XSOUT, XDISO,
     *   APARM, SEQOUT, DISKO, NUMHIS
      COMMON /CHRCOM/ INFILE, NAMOUT, CLAOUT, HISCRD
      COMMON /MAPHDR/ CATBLK
      COMMON /FRED/ SITELA, DELTA, HMIN, HMAX, ELMIN, BLKMAX, TAU,
     *   LX, LY, LZ, DIAM, BLKREJ, FREQNM, N
LOCAL END
      PROGRAM UVSIM
C-----------------------------------------------------------------------
C! Simulates uv coverage of an arbitrary interferometer array.
C# UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999-2000, 2003, 2007, 2009, 2012, 2015
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
C   UVSIM creates an AIPS uv data base and fills with simulated data.
C   See the UVSIM EXPLAIN file or Going AIPS for better descriptions
C   of the UV data file and catalog header structure and the contents
C   and structure of the antenna (AN) table.
C   There are sufficient comments in NEWHED to describe simple cases.
C   See Going AIPS for assistance in compiling and linking this task
C   onn your system.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INFILE         INFILE        Input file name. (antenna info.)
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is source name
C      OUTCLASS       CLAOUT        Class of the output uv file.
C                                   Default 'UVDATA'
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      APARM(10)      APARM         User specified array.
C                                   1:  nominal frequency  (optional)
C                                   2:  nominal wavelength      "
C                                   3-4: Bx of array
C                                   5-6: By of array
C                                   7-8: Bz of array
C   Programmer W. D. Cotton and F. R. Schwab, March 1988.
C   Updated by A. J. Mioduszewski, March 2003
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'UVSIM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'UVSIM '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UVFILN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Get the array parameters and
C                                       write antenna table.
      CALL ANTFIL
C                                       Calculate and write out UV data
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
C   UVFILN gets input parameters for UVSIM and creates an output file.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                4 => error creating output file.
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in UVSIM for more details.
C
C-----------------------------------------------------------------------
      CHARACTER PRGN*6, OLDNAM*12, DEFNAM*12
      HOLLERITH CATH(256)
      INTEGER  JERR, IERR, IROUND, NPARM
      REAL      CATR(256)
      INCLUDE 'UVSIM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATR, CATH, CATBLK)
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
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Characters
 10   CALL H2CHR (48, 1, XINFIL, INFILE)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C                                       Get other input
      CALL GETINP (IERR)
      IF (IERR.NE.0) JERR = IERR
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
      IF (JERR.NE.0) GO TO 999
C                                       Crunch input parameters.
      SEQOUT = IROUND (XSOUT)
      DISKO = IROUND (XDISO)
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
      CALL MAKOUT (OLDNAM, '      ', 0, '      ', NAMOUT, CLAOUT,
     *   SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
C                                       Image type ='UV'
      CALL CHR2H (2, 'UV', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = SEQOUT
C                                       Create output file.
      CCNO = 1
      CALL UVCREA (DISKO, CCNO, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 70
         WRITE (MSGTXT,1050) IERR
         JERR = 4
         GO TO 990
 70   NCFILE = NCFILE + 1
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
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER OFILE*48
      INTEGER   IRET, IPTRO, LUNO, LENBU, IA1, IA2, NIOUT, INDO, LRECO,
     *   KBIND, VO, BO, NUMVIS, XCOUNT
      LOGICAL   T, F
      INCLUDE 'UVSIM.INC'
C                                       Local variables
      DOUBLE PRECISION H, ANBMAX(NMAX), AZ, EL, PARANG, FI, FJ, TWOPI,
     *   UANT(NMAX), VANT(NMAX), WANT(NMAX), SITELO, TIME, GSTIAT
      REAL      U, V, W
      INTEGER   I, J
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANT.INC'
      DATA LUNO /16/
      DATA VO, BO, LENBU /0, 1, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Convert GSTIA0 to rad/day
      GSTIAT=GSTIA0*4.0D0*ATAN(1.0)/180.0D0
C                                       Calculate site lat and long(east)
      IF(ARRAYC(1) .NE. 0.0D0) THEN
         SITELO = -ATAN2(-ARRAYC(2),ARRAYC(1))
         SITELA = SQRT (ARRAYC(1)*ARRAYC(1) + ARRAYC(2)*ARRAYC(2))
         SITELA = ATAN2 (ARRAYC(3), SITELA)
         WRITE (MSGTXT,1160) SITELA/3.14159265*180.0D0
         CALL MSGWRT (9)
      ELSE
         WRITE (MSGTXT,1170)
         CALL MSGWRT (9)
         WRITE (MSGTXT,1175)
         CALL MSGWRT (9)
      ENDIF
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.LE.0) GO TO 20
         WRITE (MSGTXT,1000) IRET
         GO TO 990
C                                       Init vis file for write
C                                       LRECO = length of output rec.
 20   LRECO = LREC
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, LENBU, JBUFSZ,
     *   BUFFER, BO, KBIND, IRET)
      IF (IRET.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IRET
         GO TO 990
 30   IPTRO = KBIND
      NIOUT = 0
      NUMVIS = 0
      XCOUNT = 0
      TWOPI = 8.0D0 * ATAN (1.0D0)
      H = HMIN - TAU
 40   H = H + TAU
      IF (H.LE.HMAX) THEN
         DO 50 I = 1,N
            ANBMAX(I) = 0.0D0
 50         CONTINUE
         CALL GETANG (H, DELTA, SITELA, AZ, EL, PARANG)
         IF (EL.GE.ELMIN) THEN
            CALL UVANT (H, DELTA, N, LX, LY, LZ, UANT, VANT, WANT)
            DO 70 I = 1,N-1
               DO 60 J = I+1,N
                  CALL BLOCK (I, J, UANT, VANT, WANT, DIAM, FI, FJ)
                  ANBMAX(I) = MAX (ANBMAX(I), FI)
                  ANBMAX(J) = MAX (ANBMAX(J), FJ)
 60               CONTINUE
 70            CONTINUE
            DO 90 I = 1,N-1
               DO 80 J = I+1,N
                  IF ((ANBMAX(I).LE.BLKREJ) .AND.
     *               (ANBMAX(J).LE.BLKREJ)) THEN
                     U = (UANT(I)-UANT(J)) * FREQNM/2.997924562D8
                     V = (VANT(I)-VANT(J)) * FREQNM/2.997924562D8
                     W = (WANT(I)-WANT(J)) * FREQNM/2.997924562D8
                     NUMVIS = NUMVIS+1
                     IF(ARRAYC(1) .NE. 0.0D0) THEN
                        TIME = (H-GSTIAT-SITELO) * 360.0D0/TWOPI/DEGPDY
                     ELSE
                        TIME = 1.5D0 + H/TWOPI/1.00274D0
                     ENDIF
                     BUFFER(IPTRO+ILOCU) = U
                     BUFFER(IPTRO+ILOCV) = V
                     BUFFER(IPTRO+ILOCW) = W
                     BUFFER(IPTRO+ILOCT) = 1.D0 + TIME
                     IA1 = I
                     IA2 = J
                     BUFFER(IPTRO+NRPARM) = 1.0
                     BUFFER(IPTRO+NRPARM+1) = 0.0
                     BUFFER(IPTRO+NRPARM+2) = 1.0
                     BUFFER(IPTRO+ILOCA1) = IA1
                     BUFFER(IPTRO+ILOCA2) = IA2
                     BUFFER(IPTRO+ILOCSA) = 1.0
                     XCOUNT = XCOUNT + 1
                     IPTRO = IPTRO + LRECO
                     NIOUT = NIOUT + 1
                     CALL UVDISK('WRIT',LUNO,INDO,BUFFER,NIOUT,KBIND,
     *                  IRET)
                     IPTRO = KBIND
                     NIOUT = 0
                     END IF
 80               CONTINUE
 90            CONTINUE
            END IF
         GO TO 40
         END IF
C                                       Do history file entries:
      NUMHIS = N + 11
      WRITE (HISCRD(1),2001) N
      WRITE (HISCRD(2),2002) DELTA/TWOPI*360.0D0
      WRITE (HISCRD(3),2003) HMIN/TWOPI*48.0D0, HMAX/TWOPI*48.0D0
      WRITE (HISCRD(4),2004) TAU/TWOPI*86400.0D0
      WRITE (HISCRD(5),2005) ELMIN/TWOPI*360.0D0
      WRITE (HISCRD(6),2006) BLKREJ
      WRITE (HISCRD(7),2007) SITELA/TWOPI*360.0D0
      WRITE (HISCRD(8),2008) FREQNM
      WRITE (HISCRD(9),2009) 2.997924562D8/FREQNM
      WRITE (HISCRD(10),2012)
      WRITE (HISCRD(11),2010)
      DO  200 I = 1,N
         WRITE (HISCRD(I+11),2011) I, LX(I), LY(I), LZ(I), DIAM(I)
 200     CONTINUE
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFFER, NIOUT, KBIND, IRET)
      IF (IRET.EQ.0) GO TO 210
         WRITE (MSGTXT,1150) IRET
         GO TO 990
C                                       Compress output file.
 210  NVIS = XCOUNT
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
 1150 FORMAT ('GETUV: ERROR',I3,' WRITING VIS FILE')
 1160 FORMAT ('SITE LATITUDE HAS BEEN RECALULATED. NEW LAT=',F10.5)
 1170 FORMAT ('THE TIME STAMPS IN THE DATAFILE ARE INCORRECT')
 1175 FORMAT ('FOR CORRECT TIME STAMPS INPUT APARM(3-8)')
 2001 FORMAT (' Number of antennas:  ',I3)
 2002 FORMAT (' Declination:  ',F10.5,' degrees')
 2003 FORMAT (' Hour-angle coverage:  ',F10.5,' to ',F10.5,' hours')
 2004 FORMAT (' Integration time:  ',F10.3,' seconds')
 2005 FORMAT (' Minimum allowed elevation angle:  ',F10.5,' degrees')
 2006 FORMAT (' Maximum allowed fractional blockage:  ',F10.5)
 2007 FORMAT (' Site latitude:  ',F20.14,' degrees')
 2008 FORMAT (' Nominal frequency:  ',1PE20.12,' Hz')
 2009 FORMAT (' Nominal wavelength:  ',1PE20.12,' meters')
 2010 FORMAT (' Antenna x-y-z coordinates and diameters (all in ',
     *   'meters):')
 2011 FORMAT (I3,3G16.7,G12.5)
 2012 FORMAT (' Points are spaced at uniform time increments.')
      END
      SUBROUTINE FILHIS
C-----------------------------------------------------------------------
C   FILHIS creates and fills a history file.
C-----------------------------------------------------------------------
      CHARACTER   HILINE*72, ATIME*8, ADATE*12, LABEL*8, TELE*8, OBSR*8
      HOLLERITH CATH(256)
      INTEGER   LUN, I, IERR, TIME(3), DATE(3)
      REAL      CATR(256)
      DOUBLE PRECISION CATD(128)
      LOGICAL   T
      INCLUDE 'UVSIM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATH, CATR, CATD)
      DATA LUN /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Create/open hist. file.
      CALL HICREA (LUN, DISKO, CCNO, CATBLK, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
C                                       Get current date/time.
 10   CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
C                                       Write first record.
      WRITE (HILINE,1010) TSKNAM, NLUSER, ADATE, ATIME
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       New history
      WRITE (HILINE,1011) TSKNAM, INFILE
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN,
     *   BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       SOURCE
      WRITE (HILINE,2000) TSKNAM, SOURCE
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Number of visibilities
      WRITE (HILINE,2001) TSKNAM, CATBLK(KIGCN)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Telescope, observer name.
      CALL H2CHR (8, 1, CATH(KHTEL), TELE)
      CALL H2CHR (8, 1, CATH(KHOBS), OBSR)
      WRITE (HILINE,2002) TSKNAM, TELE, OBSR
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       AIPS release
      WRITE (HILINE,2003) TSKNAM, RLSNAM
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      Add any other history.
      IF (NUMHIS.LE.0) GO TO 20
         WRITE (LABEL,1015) TSKNAM
         DO 15 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 20
 15         CONTINUE
C                                       Close HI file
 20   CALL HICLOS (LUN, T, BUFFER, IERR)
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISKO, CCNO, CATBLK, 'REST', BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FILHIS: ERROR',I3,' CREATE/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ Image created by user',I5,' at ',A12,2X,A8)
 1011 FORMAT (A6,'INFILE =''',A,'''')
 1015 FORMAT (A6,' /')
 2000 FORMAT (A6,' SOURCE = ''',A8,'''')
 2001 FORMAT (A6,' / Number of visibilities copied=',I9)
 2002 FORMAT (A6,' / Telescope = ',A8,' Observer = ',A8)
 2003 FORMAT (A6,' RELEASE = ''',A7,' ''')
      END
      SUBROUTINE ANTFIL
C-----------------------------------------------------------------------
C   ANTFIL creates and fills the antenna file.
C-----------------------------------------------------------------------
      HOLLERITH CATH(256)
      INTEGER   IERR, VER, LUN, I
      REAL      CATR(256)
      DOUBLE PRECISION JD, GMSTM, GASTM, RATE
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVSIM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATH, CATR)
      DATA LUN /27/
C-----------------------------------------------------------------------
C                                       Make sure there is antenna info
      IF (N.LE.0) GO TO 999
C                                       Setup for AN table initization
         NUMORB = 0
         NOPCAL = 2
         ANTNIF = 1
C                                       Position of the earth's pole
         POLRXY(1) = 0.0
         POLRXY(2) = 0.0
         UT1UTC = 0.0
         DATUTC = 0.0
         TIMSYS = 'UTC'
C                                       Array name
         ANAME = '        '
C                                       Array center (rel to center of
C                                       earth)
         ARRAYC(1) = APARM(3)+APARM(4)
         ARRAYC(2) = APARM(5)+APARM(6)
         ARRAYC(3) = APARM(7)+APARM(8)
C                                       Get GST0 and Earth rotation rate
         CALL H2CHR (8, 1, CATH(KHDOB), RDATE)
         CALL JULDAY (RDATE, JD)
         CALL GSTROT (JD, GMSTM, GASTM, RATE)
         GSTIA0 = GMSTM
         DEGPDY = RATE
         SAFREQ = FREQ
         ANFQID = -1
         VER = 1
         XYZHAN = ' '
         TFRAME = ' '
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
         CALL RFILL (MAXIF, 0.0, FWHMAN)
C                                       AN records
         DO 20 I = 1,N
            STAXYZ(1) = LX(I)
            STAXYZ(2) = LY(I)
            STAXYZ(3) = LZ(I)
            NOSTA = I
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
C    Input:
C     CATBLK(256)    I     Output catalog header, also CATR, CATD
C                          The OUTNAME, OUTCLASS, OUTSEQ are entered
C                          elsewhere.
C    Output:
C     CATBLK(256)    I     Modified output catalog header.
C     IRET           I     Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER RTYPES(9)*8, TYPES(7)*8, UNITS*8, OBSDAT*8,
     *   TELE*8, OBSR*8, INSTR*8
      HOLLERITH CATH(256)
      INTEGER   I, NAXIS, NRAN, NCHAN, NPOLN, NDIM(7), INDEX, IRET,
     *   XCOUNT
      REAL      CATR(256), CRPIX(7), CRINC(7), BANDW, EPOCH
      DOUBLE PRECISION    CATD(128), CRVAL(7), TWOPI
      INCLUDE 'UVSIM.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE  (CATBLK, CATH, CATR, CATD)
C                                       User definable values
C                                       Random parameters.
C                                         No. random parameters.
      DATA NRAN /7/
C                                         Rand. parm. names.
      DATA RTYPES /'UU-L', 'VV-L', 'WW-L', 'TIME1', 'SUBARRAY',
     *   'ANTENNA1', 'ANTENNA2', 2*' '/
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
      DATA EPOCH /2000.0/
C                                       Units
      DATA UNITS /'JY      '/
C                                       "Observing" date
      DATA OBSDAT /'20000101'/
C-----------------------------------------------------------------------
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
      SOURCE = ' '
C                                       Telescope.
      TELE = ' '
C                                       Receiver
      INSTR = ' '
C                                       Observer's name.
      OBSR = ' '
C                                       Set number of axes.
      CATBLK(KIDIM) = NAXIS
      CATBLK(KIPCN) = NRAN
C                                       User ID
      CATBLK(KIIMU) = NLUSER
C                                       Miscellaneous items.
C                                       Epoch.
      CATR(KREPO) = EPOCH
C                                       Convolving beam
      CATR(KRBMJ) = 0.0
      CATR(KRBMN) = 0.0
      CATR(KRBPA) = 0.0
      CATBLK(KINIT) = 0
C                                       Max. min.
      CATR(KRDMX) = 0.0
      CATR(KRDMN) = 0.0
C                                       Shift
      CATR(KRXSH) = 0.0
      CATR(KRYSH) = 0.0
C                                       "Old" (observed) position.
      CATD(KDORA) = 0.0D0
      CATD(KDODE) = 0.0D0
C                                       Rest Frequency
      CATD(KDRST) = 0.0D0
C                                       Alternate ref. value & pixel
      CATD(KDARV) = 0.0D0
      CATR(KRARP) = 0.0
      CATBLK(KIALT) = 0
C                                       Sort order ('**'=>unsorted)
      CALL CHR2H (2, 'TB', 1, CATH(KITYP))
C                                       No magic value blanking.
      CATR(KRBLK) = 0.0
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
C                                       INSTR = Receiver name (8 char)
C                                       OBSR = Observers name (8 char)
      FREQ = 1.0D0
      BANDW = 0.0
      NCHAN = 1
      NPOLN = 1
      TWOPI = 8.0D0 * ATAN (1.0D0)
C                                       Figure out file size:
      XCOUNT = ((HMAX-HMIN) / TAU + 1.0D0) * (N * (N-1) / 2)
      RA = 0.0D0
      DEC = DELTA / TWOPI * 360.0D0
C                                       Use a nominal wavelength of 1mm
C                                       if both APARM(1) and APARM(2)
C                                       are less than or equal to zero.
C                                       Otherwise, if APARM(1) is
C                                       greater than zero then use a
C                                       nominal frequency of APARM(1)
C                                       Hz; otherwise, if APARM(2) is
C                                       greater than zero then use a
C                                       nominal wavelength of APARM(2)
C                                       meters:
      IF ((APARM(1).LE.0.0) .AND. (APARM(2).LE.0.0)) THEN
         FREQNM = 299.7924562D9
      ELSE
         IF (APARM(1).GT.0.) THEN
            FREQNM = APARM(1)
         ELSE
            FREQNM = 2.997924562D8 / APARM(2)
            END IF
         END IF
      FREQ = FREQNM
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
C
C                                       Finished.
      IRET = 0
C
 999  RETURN
      END
      SUBROUTINE GETINP (IERR)
C-----------------------------------------------------------------------
C   Routine to ask user for information
C   Input from common:  INFILE  C*48 File name.
C   Output: IERR                I    Return code 0=>OK
C-----------------------------------------------------------------------
      CHARACTER PHNAME*48, LINE*80
      INTEGER   IERR
      INTEGER   LUN, FIND, NBYTES, KBP
      DOUBLE PRECISION    X, TWOPI, A, B
      LOGICAL   T, F
      INCLUDE 'UVSIM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      TWOPI = 8.0D0 * ATAN(1.0D0)
      N = 0
C                                       Read antenna info from file
      CALL GETANT (INFILE, N, LX, LY, LZ, DIAM, SITELA, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Ask user for info.
C                                       Defaults
      DELTA = 45.0
      HMIN = -6.0
      HMAX = 6.0
      ELMIN = 10.0
      TAU = 60.0
      BLKREJ = 0.0
C                                       Open terminal
      LUN = 5
      CALL ZOPEN (LUN, FIND, 1, PHNAME, F, F, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Declination
 100  WRITE (LINE,2000)
      NBYTES = 72
      CALL ZTTYIO ('WRIT', LUN, FIND, NBYTES, LINE, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL ZTTYIO ('READ', LUN, FIND, NBYTES, LINE, IERR)
      IF (IERR.EQ.3) GO TO 100
      IF (IERR.NE.0) GO TO 970
C                                       Decode LINE
      KBP = 1
      CALL GETNUM (LINE, NBYTES, KBP, X)
      IF (X.EQ.DBLANK) GO TO 975
      DELTA = X
      WRITE (LINE,2001) DELTA
      NBYTES = 72
      CALL ZTTYIO ('WRIT', LUN, FIND, NBYTES, LINE, IERR)
      IF (IERR.NE.0) GO TO 970
      DELTA = TWOPI * DELTA / 360D0
C                                       Hour angle range
 150  WRITE (LINE,2002)
      NBYTES = 72
      CALL ZTTYIO ('WRIT', LUN, FIND, NBYTES, LINE, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL ZTTYIO ('READ', LUN, FIND, NBYTES, LINE, IERR)
      IF (IERR.EQ.3) GO TO 150
      IF (IERR.NE.0) GO TO 970
C                                       Decode LINE
      KBP = 1
      CALL GETNUM (LINE, NBYTES, KBP, X)
      IF (X.EQ.DBLANK) GO TO 975
      A = X
      CALL GETNUM (LINE, NBYTES, KBP, X)
      IF (X.EQ.DBLANK) GO TO 975
      B = X
      WRITE (LINE,2003) A, B
      NBYTES = 72
      CALL ZTTYIO ('WRIT', LUN, FIND, NBYTES, LINE, IERR)
      IF (IERR.NE.0) GO TO 970
      HMIN = MIN (A,B) * TWOPI / 24.0D0
      HMAX = MAX (A,B) * TWOPI / 24.0D0
C                                       Elevation limit
 200  WRITE (LINE,2004)
      NBYTES = 72
      CALL ZTTYIO ('WRIT', LUN, FIND, NBYTES, LINE, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL ZTTYIO ('READ', LUN, FIND, NBYTES, LINE, IERR)
      IF (IERR.EQ.3) GO TO 200
      IF (IERR.NE.0) GO TO 970
C                                       Decode LINE
      KBP = 1
      CALL GETNUM (LINE, NBYTES, KBP, X)
      IF (X.EQ.DBLANK) GO TO 975
      ELMIN = X
      WRITE (LINE,2005) ELMIN
      NBYTES = 72
      CALL ZTTYIO ('WRIT', LUN, FIND, NBYTES, LINE, IERR)
      IF (IERR.NE.0) GO TO 970
      ELMIN = TWOPI * ELMIN / 360.0D0
C                                       Integration time
 250  WRITE (LINE,2006)
      NBYTES = 72
      CALL ZTTYIO ('WRIT', LUN, FIND, NBYTES, LINE, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL ZTTYIO ('READ', LUN, FIND, NBYTES, LINE, IERR)
      IF (IERR.EQ.3) GO TO 250
      IF (IERR.NE.0) GO TO 970
C                                       Decode LINE
      KBP = 1
      CALL GETNUM (LINE, NBYTES, KBP, X)
      IF (X.EQ.DBLANK) GO TO 975
      TAU = X
      WRITE (LINE,2007) TAU
      NBYTES = 72
      CALL ZTTYIO ('WRIT', LUN, FIND, NBYTES, LINE, IERR)
      IF (IERR.NE.0) GO TO 970
      TAU = TAU * TWOPI / 86400.0D0
C                                       Blockage limit
 300  WRITE (LINE,2008)
      NBYTES = 72
      CALL ZTTYIO ('WRIT', LUN, FIND, NBYTES, LINE, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL ZTTYIO ('READ', LUN, FIND, NBYTES, LINE, IERR)
      IF (IERR.EQ.3) GO TO 300
      IF (IERR.NE.0) GO TO 970
C                                       Decode LINE
      KBP = 1
      CALL GETNUM (LINE, NBYTES, KBP, X)
      IF (X.EQ.DBLANK) GO TO 975
      BLKREJ = X
      WRITE (LINE,2009) BLKREJ
      NBYTES = 72
      CALL ZTTYIO ('WRIT', LUN, FIND, NBYTES, LINE, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Close terminal
      CALL ZCLOSE (LUN, FIND, IERR)
      GO TO 999
C                                       ZTTYIO write error
 970  WRITE (MSGTXT,1970) IERR
      GO TO 990
 975  MSGTXT = 'NUMBER PARSING ERROR ONN ' // LINE
      IERR = 1
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETINP: ERROR',I3,' OPENING TERMINAL FOR INSTRUCTIONS')
 1970 FORMAT ('GETINP: ERROR',I3,' WRITING TO TERMINAL')
 2000 FORMAT ('Type source declination, in degrees:')
 2001 FORMAT ('Source declination = ',F8.4,' degrees')
 2002 FORMAT ('Type the two extremes of the hour-angle coverage, in',
     *   ' hours:')
 2003 FORMAT ('Hour-angle coverage is',F9.4,' hours to ',F8.4,' hours.')
 2004 FORMAT ('What is the minimum allowable elevation angle, in deg.?')
 2005 FORMAT ('Minimum elevation angle=',F8.4,' degrees.')
 2006 FORMAT ('Specify the integration time, in seconds.')
 2007 FORMAT ('Integration time=',F12.4,' seconds.')
 2008 FORMAT ('What is the maximum allowable fractional blockage of',
     *   ' an antenna?')
 2009 FORMAT ('Maximum fractional blockage=',F8.4)
      END
      SUBROUTINE GETANT (FILE, N, LX, LY, LZ, DIAM, SITELA, IERR)
C-----------------------------------------------------------------------
C  This subroutine reads, from an input file specified by name "fln",
C  the antenna information.
C  The first (ascii) data record should specify n, the number of
C  elements.
C  The second data record should specify the site latitude, in degrees.
C  The third record must specify two conversion factors: first,
C  a multiplicative factor for converting antenna coordinates to meters,
C  and, second, a factor for converting antenna diameters to meters.
C  (E.g., if the station coordinates are in nanoseconds and the antenna
C  diameters are in feet, then the two factors that need to be supplied
C  are 0.2997924562 (approximately) and 0.3048 (exactly).)
C  Then, n records must follow, each with four numbers:  the three
C  earth-based Cartesian coordinates of the antenna location, and the
C  antenna diameter.
C   Inputs:
C    FILE     C*48  File name
C   Outputs:
C    N        I     Number of antennas
C    LX(*)    D     "X" coordinate of antennas (m)
C    LY(*)    D     "Y" coordinate of antennas (m)
C    LZ(*)    D     "Z" coordinate of antennas (m)
C    DIAM(*)  D     Antenna diameters (m)
C    SITELA   D     Site latidude (rad?)
C    IERR     I     Return code, 0=>OK
C-----------------------------------------------------------------------
      CHARACTER FILE*48
      INTEGER   N, IERR
      DOUBLE PRECISION LX(*), LY(*), LZ(*), DIAM(*), SITELA
      INTEGER   LUN, FIND, NBYTES, KBP, I, JT, JTRIM
      LOGICAL   F
      CHARACTER LINE*80
      DOUBLE PRECISION CONST1, CONST2, X
      INCLUDE 'INCS:DMSG.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Open text file
      LUN = 10
      CALL ZTXOPN ('READ', LUN, FIND, FILE, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1001)
         GO TO 990
         END IF
C                                       Get number of antennas
      CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
      IF (IERR.NE.0) GO TO 980
      JT = JTRIM (LINE)
C                                       Get value
      KBP = 1
      NBYTES = 80
      CALL GETNUM (LINE, NBYTES, KBP, X)
      N = X + 0.1
C                                       Tell user
      WRITE (MSGTXT,2000) N
      CALL MSGWRT (6)
C                                       get site latitude
      CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
      IF (IERR.NE.0) GO TO 980
      JT = JTRIM (LINE)
C                                       Get value
      KBP = 1
      CALL GETNUM (LINE, NBYTES, KBP, X)
      SITELA = X
C                                       Tell user
      WRITE (MSGTXT,2001) SITELA
      CALL MSGWRT (6)
      SITELA = SITELA * ATAN (1.0D0) / 45.0D0
C                                       Get conversion constants
      CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
      IF (IERR.NE.0) GO TO 980
      JT = JTRIM (LINE)
C                                       Get values
      KBP = 1
      CALL GETNUM (LINE, NBYTES, KBP, X)
      CONST1 = X
      CALL GETNUM (LINE, NBYTES, KBP, X)
      CONST2 = X
C                                       Tell user
      WRITE (MSGTXT,2002) CONST1
      CALL MSGWRT (6)
      WRITE (MSGTXT,2003) CONST2
      CALL MSGWRT (6)
C                                       Read antenna info
      DO 100 I = 1,N
         CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
         IF (IERR.NE.0) GO TO 980
         JT = JTRIM (LINE)
C                                       Get values
         KBP = 1
         CALL GETNUM (LINE, NBYTES, KBP, X)
         LX(I) = X * CONST1
         CALL GETNUM (LINE, NBYTES, KBP, X)
         LY(I) = X * CONST1
         CALL GETNUM (LINE, NBYTES, KBP, X)
         LZ(I) = X * CONST1
         CALL GETNUM (LINE, NBYTES, KBP, X)
         DIAM(I) = X * CONST2
         WRITE (MSGTXT,2004) I, LX(I), LY(I), LZ(I), DIAM(I)
         CALL MSGWRT (6)
 100     CONTINUE
      CALL ZTXCLS (LUN, FIND, IERR)
      GO TO 999
C                                       Read error
 980  WRITE (MSGTXT,1980) IERR
      GO TO 990
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('ERROR ',I3,' OPENING ANTENNA INFO TEXT FILE')
 1980 FORMAT ('ERROR ',I3,' READING ANTENNA INFO TEXT FILE')
 2000 FORMAT ('Number of antennas = ',I4)
 2001 FORMAT ('Site latitude = ',F8.3,' degrees')
 2002 FORMAT ('Coordinate conversion factor to meters =',1PE12.5)
 2003 FORMAT ('Diameter conversion factor to meters =',1PE12.5)
 2004 FORMAT ('Ant(',I2,')=',1PE13.5,2E13.5,', dia=',0PF7.2,' m')
      END
      SUBROUTINE GETANG (H, DELTA, SLAT, AZ, EL, PARANG)
C-----------------------------------------------------------------------
C   Given the hour-angle h, the source declination delta, and the
C   geographic latitude, slat, of the site, this subroutine computes
C   the azimuth, the elevation, and the parallactic angle.
C   Inputs:
C    H        D    Hour angle (rad)
C    DELTA    D    Source declination (rad)
C    SLAT     D    Site latitude (rad)
C   Outputs:
C    AZ       D    Azimuth (rad)
C    EL       D    Elevation (rad)
C    PARANG   D    Parallactic angle (rad)
C-----------------------------------------------------------------------
      DOUBLE PRECISION H, DELTA, SLAT, AZ, EL, PARANG
      DOUBLE PRECISION PI, Z, SINQ, COSQ, SINA, COSA
C-----------------------------------------------------------------------
      PI = 4.0D0 * ATAN (1.0D0)
      Z = ACOS (SIN (DELTA) * SIN (SLAT) + COS (DELTA) * COS (SLAT) *
     *   COS (H))
      EL = PI / 2.0D0 - Z
      IF (Z.EQ.0D0) THEN
         PARANG = 0.0D0
         AZ = 0.0D0
      ELSE
         SINQ = SIN (H) * COS (SLAT) / SIN (Z)
         COSQ = SIN (SLAT) / SIN (Z) / COS (DELTA) - TAN (DELTA) /
     *      TAN (Z)
         PARANG = ATAN2 (SINQ, COSQ)
         SINA = -COS (DELTA) * SIN (H) / SIN (Z)
         COSA = (SIN (DELTA) * COS (SLAT) - COS (DELTA) * COS (H) *
     *      SIN (SLAT)) / SIN (Z)
         AZ = ATAN2 (SINA, COSA)
         IF (AZ.LT.0.0D0) AZ = AZ + 2.0D0 * PI
         END IF
C
 999  RETURN
      END
      SUBROUTINE BLOCK (I, J, UANT, VANT, WANT, DIAM, FI, FJ)
C-----------------------------------------------------------------------
C  Subroutine to compute geometric shadowing.
C  Given i and j, antenna-based (u,v,w), and the antenna diameters,
C  this subroutine computes the fractional blockage fi of antenna i by
C  antenna j, and the fractional blockage fj of antenna j by antenna i.
C  Whenever blockage occurs, if w<0 then it is the case that antenna j
C  is blocked by antenna i, if w>0 then antenna i has been blocked by
C  antenna j, and if w = 0 then the antennas have run into each other.
C  So generally, on return, one - but not both - of fi and fj may be
C  nonzero.
C  The antenna profiles are assumed to be circular, and the antennas
C  are assumed to be pointed the same way (a reasonably valid
C  assumption for the case of a compact array configuration, which
C  is the only case where one generally worries about shadowing).
C   Inputs:
C    I       I    First antenna number
C    J       I    Second antenna number
C    UANT(*) D    Array of U values (m)
C    VANT(*) D    Array of V values (m)
C    WANT(*) D    Array of W values (m)
C    DIAM(*) D    Array of diameters (m)
C   Outputs:
C    FI      D    Fractional blockage of antenna I
C    FJ      D    Fractional blockage of antenna J
C-----------------------------------------------------------------------
      INTEGER   I, J
      DOUBLE PRECISION    UANT(*), VANT(*), WANT(*), DIAM(*), FI, FJ
      DOUBLE PRECISION    PI, U, V, W, SEP, RMIN, RMAX, SINA, SINB,
     *   AREA, S, C, A, B
C-----------------------------------------------------------------------
      PI = 4.0D0 * ATAN(1.0D0)
      U = UANT(J)-UANT(I)
      V = VANT(J)-VANT(I)
      W = WANT(J)-WANT(I)
      SEP = SQRT (U**2 + V**2)
      RMIN = 0.5D0 * MIN (DIAM(I), DIAM(J))
      RMAX = 0.5D0 * MAX (DIAM(I), DIAM(J))
      IF (SEP.GE.RMIN+RMAX) THEN
         FI = 0.0D0
         FJ = 0.0D0
      ELSE IF (SEP+RMIN.LE.RMAX) THEN
         FI = MIN (1.0D0, (DIAM(J) / DIAM(I))**2)
         FJ = MIN (1.0D0, (DIAM(I) / DIAM(J))**2)
      ELSE
         C = SEP / (0.5D0*DIAM(I))
         S = DIAM(J) / DIAM(I)
         SINB = SQRT ((S+C+1.0D0) * (S+C-1.0D0) * (S-C+1.0D0) *
     *      (C-S+1.0D0)) / (2.0D0*C)
C  Or,
C        SINB = SQRT(2D0*((C*S)**2+C**2+S**2)-C**4-S**4-1D0)/(2D0*C)
C
         SINA = SINB / S
C                                       Due to roundoff, sina or sinb
C                                       might be ever so slightly larger
C                                       than 1. In the case of unequal
C                                       radii, with the center of one
C                                       antenna pattern inside the
C                                       other:
         SINB = MIN (1.0D0, SINB)
         SINA = MIN (1.0D0, SINA)
C
         B = ASIN (SINB)
         A = ASIN (SINA)
         AREA = (S**2*A+B) - (S**2*SINA*COS (A)+SINB*COS (B))
         FI = AREA / PI
         FJ = FI / S**2
      END IF
      IF (W.LT.0.0D0) FI = 0.0D0
      IF (W.GT.0.0D0) FJ = 0.0D0
 999  RETURN
      END
      SUBROUTINE UVANT (H, DELTA, N, LX, LY, LZ, UANT, VANT, WANT)
C-----------------------------------------------------------------------
C  Given the hour-angle h, the source declination delta, the number
C  of elements n, and the element locations (lx(i),ly(i),lz(i)),
C  i = 1,...,n, this subroutine computes the antenna-based spatial
C  frequency coordinates (uant(i),vant(i),want(i)), i = 1,...,n.
C   Inputs:
C    H        D    Hour angle (rad)
C    DELTA    D    Source declination (rad)
C    N        I    Number of antennas
C    LX(*)    D    "X" component of antenna position (m)
C    LY(*)    D    "Y" component of antenna position (m)
C    LZ(*)    D    "Z" component of antenna position (m)
C   Outputs:
C    UANT(*)  D    "U" component of project coordinate (m)
C    VANT(*)  D    "V" component of project coordinate (m)
C    WANT(*)  D    "W" component of project coordinate (m)
C-----------------------------------------------------------------------
      INTEGER   N
      DOUBLE PRECISION H, DELTA, LX(*), LY(*), LZ(*), UANT(*),
     *   VANT(*), WANT(*)
      INTEGER   I
      DOUBLE PRECISION SH, CH, SD, CD
C-----------------------------------------------------------------------
      SH = SIN (H)
      CH = COS (H)
      SD = SIN (DELTA)
      CD = COS (DELTA)
      DO 100 I = 1,N
         UANT(I) = SH * LX(I) + CH * LY(I)
         VANT(I) = -SD * (CH * LX(I) - SH * LY(I)) + CD * LZ(I)
         WANT(I) = CD * (CH * LX(I) - SH * LY(I)) + SD * LZ(I)
 100     CONTINUE
C
 999  RETURN
      END
