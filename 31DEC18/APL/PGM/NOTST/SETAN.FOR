LOCAL INCLUDE 'SETAN.INC'
C                                       Local include for SETAN
      INTEGER   SEQOUT, OUTVER, DISKO, NEWCNO, NUMHIS, JBUFSZ, ICODE,
     *   LUNIN,  FINDIN, BUFFER(4096)
      CHARACTER HISCRD(10)*64, FILEIN*48, NAMOUT*12, CLAOUT*6
      HOLLERITH XFILEI(12), XNAMOU(3), XCLAOU(2)
      REAL      XSEQO, XOVER, XDISKO
C                                       Program commons
      COMMON /INPARM/ XFILEI, XNAMOU, XCLAOU, XSEQO, XOVER, XDISKO
      COMMON /PARMS/ SEQOUT, OUTVER, DISKO, NEWCNO,
     *   JBUFSZ, ICODE, LUNIN, FINDIN, NUMHIS
      COMMON /BUFRS/ BUFFER
      COMMON /CHRCOM/ HISCRD, FILEIN, NAMOUT, CLAOUT
LOCAL END
      PROGRAM SETAN
C-----------------------------------------------------------------------
C! Task read AN file info from a text file
C# UV utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2008-2009, 2012
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
C   SETAN reads AN table information from an external text file.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INFILE         FILEIN        Input file.
C      OUTNAME        NAMOUT        Name of the output file
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output file.
C                                   Default is input class.
C      OUTSEQ         SEQOUT        Seq. number of output file.
C      OUTVER         OUTVER        Output AN table version number.
C      OUTDISK        DISKO         Disk number of the output file.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'SETAN.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'SETAN '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      CALL STAIN (PRGM, IRET)
C                                       Read antenna data.
      IF (IRET.EQ.0) CALL STAMA (IRET)
C                                       History
      IF (IRET.EQ.0) CALL STAHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE STAIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   STAIN gets input parameters for SETAN.
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
C   See prologue comments in SETAN for more details.
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6
C
      CHARACTER PTYPE*2, STAT*4
      INTEGER   IERR, NPARM, IROUND, JERR
      LOGICAL   T
      INCLUDE 'SETAN.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T, BUFFER)
      CALL VHDRIN
      JBUFSZ = 2 * 4096
      IRET = 0
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      LUNIN = 10
C                                       Init CATBLK.
      CALL FILL (256, 0, CATBLK)
C                                       Get input parameters.
      NPARM = 20
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
      OUTVER = IROUND (XOVER)
      DISKO = IROUND (XDISKO)
C                                       Characters
      CALL H2CHR (48, 1, XFILEI, FILEIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C                                       Find file
      NEWCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKO, NEWCNO, NAMOUT, CLAOUT, SEQOUT,
     *   PTYPE, NLUSER, STAT, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMOUT, CLAOUT, SEQOUT, DISKO,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKO, NEWCNO, CATBLK, 'WRIT', BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FCNO(NCFILE) = NEWCNO
      FVOL(NCFILE) = DISKO
      FRW(NCFILE) = 1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Open the input file
      CALL ZTXOPN ('READ', LUNIN, FINDIN, FILEIN, .FALSE., IERR)
      IF (IERR.NE.0) GO TO 999
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('STAIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
      END
      SUBROUTINE STAMA (IRET)
C-----------------------------------------------------------------------
C   STAMA accepts an file one row at a time from GETMA and writes to an
C   AIPS disk file.
C   Output:
C      IRET   I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   LUNO, NBYTES, KBP, NCHAR, IERR, LOOP, NREC, MOSTA,
     *   MAXRNO, NIF, JT, JTRIM
      LOGICAL   TABLE, EXIST, FITASC, OLD
      DOUBLE PRECISION X, CTAXYZ(3)
      CHARACTER LINE*132, STRING*8, CNNAME*8
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SETAN.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA LUNO /25/
C-----------------------------------------------------------------------
      NIF = 1
      IF (JLOCIF.GE.0) NIF = MAX (1, CATBLK(KINAX+JLOCIF))
      ARRAYC(1) = 0.0
      ARRAYC(2) = 0.0
      ARRAYC(3) = 0.0
      GSTIA0 = 0.0D0
      DEGPDY = 0.0D0
      SAFREQ = 1.0D0
      RDATE = '19900101'
      POLRXY(1) = 0.0D0
      POLRXY(2) = 0.0D0
      UT1UTC = 0.0
      DATUTC = 0.0
      TIMSYS = 'UTC'
      ANAME = ' '
      NUMORB = 0
      NOPCAL = 2
      ANFQID = -1
      ANTNIF = NIF
C                                       Open AN file read to get
C                                       keywords.
      CALL ISTAB ('AN', DISKO, NEWCNO, OUTVER, LUNO, BUFFER, TABLE,
     *   EXIST, FITASC, IERR)
      OLD = EXIST .AND. (IERR.EQ.0)
      IF (OLD) THEN
         CALL ANTINI ('READ', BUFFER, DISKO, NEWCNO, OUTVER, CATBLK,
     *      LUNO, IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY,
     *      SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME,
     *      XYZHAN, TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IRET)
         NREC = BUFFER(5)
C                                       Close table
         CALL TABIO ('CLOS', 0, IANRNO, BUFFER, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Read table header stuff
      CALL ANTHED (ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE, POLRXY,
     *   UT1UTC, DATUTC, ANAME, XYZHAN, TFRAME, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open output table
      CALL ANTINI ('WRIT', BUFFER, DISKO, NEWCNO, OUTVER, CATBLK, LUNO,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IRET)
      IF (IRET.NE.0) GO TO 999
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
      MAXRNO = 0
C                                       Loop to end of file
 100     CALL ZTXIO ('READ', LUNIN, FINDIN, LINE, IRET)
         IF (IRET.EQ.2) GO TO 200
         IF (IRET.NE.0) GO TO 999
         JT = JTRIM (LINE)
C                                       Get values
         KBP = 1
         NBYTES = 132
         CALL GETNUM (LINE, NBYTES, KBP, X)
         IF (X.EQ.DBLANK) GO TO 980
         NOSTA = X + 0.5
         CALL GETNUM (LINE, NBYTES, KBP, X)
         IF (X.EQ.DBLANK) GO TO 980
         STAXYZ(1) = X
         CALL GETNUM (LINE, NBYTES, KBP, X)
         IF (X.EQ.DBLANK) GO TO 980
         STAXYZ(2) = X
         CALL GETNUM (LINE, NBYTES, KBP, X)
         IF (X.EQ.DBLANK) GO TO 980
         STAXYZ(3) = X
         CALL GETSTR (LINE, NBYTES, 8, KBP, STRING, NCHAR)
         IF (STRING.NE.'        ') ANNAME = STRING(1:8)
C                                       Find any old entry.
         IF (OLD) THEN
            DO 50 LOOP = 1,NREC
               IANRNO = LOOP
               CALL TABAN ('READ', BUFFER, IANRNO, ANKOLS, ANNUMV,
     *            CNNAME, CTAXYZ, ORBPRM, MOSTA, MNTSTA, STAXOF,
     *            DIAMAN, FWHMAN, POLTYA, POLAA, POLCA, POLTYB, POLAB,
     *            POLCB, IRET)
               IF (IRET.NE.0) GO TO 999
               IANRNO = LOOP
               IF (MOSTA.EQ.NOSTA) GO TO 60
 50            CONTINUE
            IANRNO = MAXRNO + 1
            END IF
C                                       Write entry
 60      MAXRNO = MAX (MAXRNO, IANRNO)
         IF (NOSTA.GT.0) THEN
            CALL TABAN ('WRIT', BUFFER, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         GO TO 100
C                                       Close files
 200  CALL TABIO ('CLOS', 0, IANRNO, BUFFER, BUFFER, IRET)
      CALL ZTXCLS (LUNIN, FINDIN, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 980  MSGTXT = 'ERROR PARSING ' // LINE
      IRET = 1
 990  CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE STAHIS
C-----------------------------------------------------------------------
C   STAHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER ATIME*8, ADATE*12, HILINE*72, LABEL*8, REST*4
      INTEGER   LUN, IERR, I, TIME(3), DATE(3)
      LOGICAL   T
      INCLUDE 'SETAN.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN /27/
      DATA REST /'REST'/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Create/open hist. file.
      CALL HIOPEN (LUN, DISKO, NEWCNO, BUFFER, IERR)
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
      WRITE (HILINE,1010) TSKNAM, ADATE, ATIME
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       AIPS release
      WRITE (HILINE,2004) TSKNAM, RLSNAM
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       INFILE
      WRITE (HILINE,1012) TSKNAM, FILEIN
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       OUTVER
      WRITE (HILINE,1013) TSKNAM, OUTVER
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any other history.
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
 1000 FORMAT ('STAHIS: ERROR',I3,' OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ Run at ',A12,2X,A8)
 1011 FORMAT (A6,' /')
 1012 FORMAT (A6, 'INFILE =''',A,'''')
 1013 FORMAT (A6, 'OUTVER = ', I4,' / Output AN table number')
 2004 FORMAT (A6,'RELEASE = ''',A7,' ''')
      END
      SUBROUTINE ANTHED (ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE, POLRXY,
     *   UT1UTC, DATUTC, ANAME, XYZHAN, TFRAME, IRET)
C-----------------------------------------------------------------------
C   ANTHED reads the header section of the external text file.
C   to be open and returns the values.
C   Input/Output:
C      ARRAYC  D(3)   Array center in m.
C      GSTIA0  D      GST at IAT0
C      DEGPDY  D      Earth rotation rate
C      SAFREQ  D      Subarray reference frequency
C      RDATE   C*8    Subarray reference date
C      POLRXY  R(2)   Position of pole (arc seconds)
C      UT1UTC  R      UT1-UTC
C      DATUTC  R      data time - UTC
C      ANAME   C*8    Array name
C      XYZHAN  C*8    Handedness of Bx, By, Bz
C      TFRME   C*8    Terrestrial reference frame
C   Output:
C      IRET    I      Return code, 0=OK
C-----------------------------------------------------------------------
      DOUBLE PRECISION ARRAYC(3), GSTIA0, DEGPDY, SAFREQ
      CHARACTER RDATE*8, ANAME*8, XYZHAN*8, TFRAME*8
      REAL      POLRXY(2), UT1UTC, DATUTC
      INTEGER   IRET
C
      INTEGER   NP
      PARAMETER (NP=14)
      CHARACTER PARS(NP)*8, ENDMRK*8, VALCH(NP)*24
      INTEGER   KMODE, NPARS, RARR, RGST, RDEG, RSAF, RPOL, RUT1, RIAT,
     *   CRDA, CANA, CXYZ, CTFR
      DOUBLE PRECISION VALS(NP)
      INCLUDE 'SETAN.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C                                       Real data
      DATA PARS /'ARRAYX', 'ARRAYY', 'ARRAYZ', 'GSTIAT', 'DEGPDY',
     *   'FREQ', 'POLARX', 'POLARY', 'UT1UTC', 'DATUTC',
C                                       Character data
     *   'ARRNAM', 'RDATE', 'XYZHAN', 'TFRAME'/
      DATA ENDMRK /'/       '/
      DATA NPARS, KMODE /12, 0/
C                                       Pointers
      DATA RARR, RGST, RDEG, RSAF, RPOL, RUT1, RIAT
     *   /  1,    4,    5,    6,    7,    9,    10/
      DATA CANA, CRDA, CXYZ, CTFR /11, 12, 13, 14/
C-----------------------------------------------------------------------
C                                       Set defaults
C                                       Array center
      VALS(RARR) = ARRAYC(1)
      VALS(RARR+1) = ARRAYC(2)
      VALS(RARR+2) = ARRAYC(3)
C                                       GST at time=0
      VALS(RGST) = GSTIA0
C                                       Earth rotation rate
      VALS(RDEG) = DEGPDY
C                                       Reference frequency
      VALS(RSAF) = SAFREQ
C                                       Polar position
      VALS(RPOL) = POLRXY(1)
      VALS(RPOL+1) = POLRXY(2)
C                                       UT1 - UTC
      VALS(RUT1) = UT1UTC
C                                       data time - UTC
      VALS(RIAT) = DATUTC
C                                       Array name
      VALCH(CANA) = ANAME
C                                       Reference date
      VALCH(CRDA) = RDATE
C                                       handedness
      VALCH(CXYZ) = XYZHAN
C                                       Frame
      VALCH(CTFR) = TFRAME
C                                       Read file.
      CALL KEYIN (PARS, VALS, VALCH, NPARS, ENDMRK, KMODE, LUNIN,
     *   FINDIN, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Set output values
      ARRAYC(1) = VALS(RARR)
      ARRAYC(2) = VALS(RARR+1)
      ARRAYC(3) = VALS(RARR+2)
      GSTIA0 = VALS(RGST)
      DEGPDY = VALS(RDEG)
      SAFREQ = VALS(RSAF)
      POLRXY(1) = VALS(RPOL)
      POLRXY(2) = VALS(RPOL+1)
      UT1UTC = VALS(RUT1)
      DATUTC = VALS(RIAT)
      ANAME = VALCH(CANA)
      CALL DATFST ('F2L', VALCH(CRDA))
      RDATE = VALCH(CRDA)
      XYZHAN = VALCH(CXYZ)
      TFRAME = VALCH(CTFR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ANTHED: ERROR =',I3,' FROM KEYIN')
 1020 FORMAT (6X,I2)
      END
