C   AIPS Object Oriented utility module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran utility module.
C# Ext-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2008
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
C  Public functions:
C    OBSCNF (name, scrno, ierr)
C       Fills in file description info for a scratch file created
C       outside of the OOP package.
C    OBFEXS (name, exist, ierr)
C       Determines if an object exists, for objects with an underlying
C       disk file the existance of this file is also checked. ierr=0
C       mean it exists else not.
C    OBFSIZ (name, exist, size, ierr)
C       Determines if an object exists, for objects with an underlying
C       disk file the existance of this file is also checked and the
C       size returned,  ierr=0 mean it exists else not.
C    OBACF (disk, cno, index)
C       Determines if a specified AIPS disk and catalog slot already has
C       an AIPS catalog flags set and registered in the DFIL.INC common.
C    OBCSYN (name, ierr)
C       Synchronizes the memory resident CATBLK with the one on disk,
C       i.e. reads from disk.
C    FNDSKC (name, disk, cno, ierr)
C       Looks up disk, cno number; object need not have been previously
C       opened.
C-----------------------------------------------------------------------
      SUBROUTINE OBSCNF (NAME, SCRNO, IERR)
C-----------------------------------------------------------------------
C   OOP utility routine
C   Fills in file descriptive information for an object associated with
C   an AIPS scratch file created outside of the OOP package.  It is
C   passed the scratch file number in the common in DFIL.INC.
C   Also reads disk catalog header into object memory.
C   Inputs:
C      NAME    C*?  Name of input object with associated scratch file.
C      SCRNO   I    DFIL.INC scratch file number
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   SCRNO, IERR
      CHARACTER NAME*(*)
C
      INTEGER   OBJNUM, DIM(7), CSEQ, CUSID, DUMMY
      CHARACTER CNAME*12, CCLASS*6, CPTYPE*2, STAT*4, CDUMMY*1
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Object number
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Scratch file number
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL OBPUT (OBJNUM, 'SCRCNO', OOAINT, DIM, SCRNO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Disk
      CALL OBPUT (OBJNUM, 'DISK', OOAINT, DIM, SCRVOL(SCRNO), CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       CNO
      CALL OBPUT (OBJNUM, 'CNO', OOAINT, DIM, SCRCNO(SCRNO), CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Names
      CALL CATDIR ('INFO', SCRVOL(SCRNO), SCRCNO(SCRNO), CNAME, CCLASS,
     *   CSEQ, CPTYPE, CUSID, STAT, SBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 995
         END IF
C                                       Save name
      DIM(1) = LEN (CNAME)
      CALL OBPUT (OBJNUM, 'NAME', OOACAR, DIM, DUMMY, CNAME, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Save class
      DIM(1) = LEN (CCLASS)
      CALL OBPUT (OBJNUM, 'CLASS', OOACAR, DIM, DUMMY, CCLASS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Save sequence
      DIM(1) = 1
      CALL OBPUT (OBJNUM, 'IMSEQ', OOAINT, DIM, CSEQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Save CATBLK
      CALL CATIO ('READ', SCRVOL(SCRNO), SCRCNO(SCRNO), CATBLK, 'REST',
     *   SBUFF, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.4)) THEN
         WRITE (MSGTXT,1001) IERR
         GO TO 995
         END IF
      IERR = 0
      CALL OBHPUT (NAME, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 995  CALL MSGWRT (6)
 990  MSGTXT = 'OBSCNF: ERROR SPECIFYING ' // NAME
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OBSCNF: CATDIR ERROR ', I4)
 1001 FORMAT ('OBSCNF: CATIO ERROR ', I4)
      END
      SUBROUTINE OBFEXS (NAME, EXIST, IERR)
C-----------------------------------------------------------------------
C   OOP utility routine
C   Determines if and object exists, for objects with an underlying
C   disk file the existance of this file is also checked.
C   Inputs:
C      NAME    C*?  Name of input object to be tested
C   Output:
C      EXIST   L    True if object fully instantiated
C      IERR    I    Error code: 0 => ok 1 => usually means doesn't exist
C-----------------------------------------------------------------------
      INTEGER   IERR
      LOGICAL   EXIST
      CHARACTER NAME*(*)
C
      INTEGER   NDSK
C                                       NDSK = lisk of classes with
C                                       underlying disk files.
      PARAMETER (NDSK=3)
      INTEGER   OBJNUM, CLASNO, LOOP, DISK, CNO, VER, TYPE, DIM(7),
     *   ISIZE, MSGSAV, DUMMY
      CHARACTER CNAME*8, DSKCLA(NDSK)*8, NAMCLT*20, FNAME*48, FTYPE*2
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA DSKCLA /'IMAGE', 'UVDATA', 'TABLE'/
C-----------------------------------------------------------------------
      EXIST = .FALSE.
      MSGSAV = MSGSUP
C                                       Is the object extant
      MSGSUP = 32000
      CALL OBNAME (NAME, OBJNUM, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         GO TO 999
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Object class
      MSGSUP = 32000
      CALL OBCLAS (OBJNUM, CLASNO, CNAME, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         GO TO 999
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Check for disk?
      DO 100 LOOP = 1,NDSK
         IF (CNAME.EQ.DSKCLA(LOOP)) GO TO 200
 100     CONTINUE
C                                       Nope?
      EXIST = .TRUE.
      GO TO 999
C                                       Check disk file
 200  MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL FNDSKC (NAME, DISK, CNO, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         DISK = 0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
      IF ((DISK.LE.0) .OR. (CNO.LE.0)) GO TO 999
C                                       Tables are different
      IF (CNAME.EQ.'TABLE') THEN
         CALL TBLKUP (NAME, DISK, CNO, FTYPE, VER, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CALL FNAGET (NAME, 'NAMCLSTY', TYPE, DIM, DUMMY, NAMCLT, IERR)
         IF (IERR.NE.0) GO TO 990
         FTYPE = NAMCLT(19:20)
         VER = 1
         END IF
C                                       Does file exist?
      CALL ZPHFIL (FTYPE, DISK, CNO, VER, FNAME, IERR)
      CALL ZEXIST (DISK, FNAME, ISIZE, IERR)
      EXIST = IERR .EQ. 0
      IF (IERR.EQ.1) IERR = 0
      GO TO 999
C                                       Error
 995  CALL MSGWRT (7)
 990  MSGTXT = 'OBFEXS: DETERMINING EXISTANCE OF ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OBFSIZ (NAME, EXIST, SIZE, IERR)
C-----------------------------------------------------------------------
C   OOP utility routine
C   Determines if and object exists, for objects with an underlying
C   disk file the existance of this file is also checked.
C   Inputs:
C      NAME    C*?  Name of input object to be tested
C   Output:
C      EXIST   L    True if object fully instantiated
C      SIZE    I    size of file in AIPS blocks (256-word records)
C      IERR    I    Error code: 0 => ok 1 => usually means doesn't exist
C-----------------------------------------------------------------------
      INTEGER   SIZE, IERR
      LOGICAL   EXIST
      CHARACTER NAME*(*)
C
      INTEGER   NDSK
C                                       NDSK = lisk of classes with
C                                       underlying disk files.
      PARAMETER (NDSK=3)
      INTEGER   OBJNUM, CLASNO, LOOP, DISK, CNO, VER, TYPE, DIM(7),
     *   MSGSAV, DUMMY
      CHARACTER CNAME*8, DSKCLA(NDSK)*8, NAMCLT*20, FNAME*48, FTYPE*2
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA DSKCLA /'IMAGE', 'UVDATA', 'TABLE'/
C-----------------------------------------------------------------------
      EXIST = .FALSE.
      SIZE = 0
      MSGSAV = MSGSUP
C                                       Is the object extant
      MSGSUP = 32000
      CALL OBNAME (NAME, OBJNUM, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         GO TO 999
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Object class
      MSGSUP = 32000
      CALL OBCLAS (OBJNUM, CLASNO, CNAME, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         GO TO 999
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Check for disk?
      DO 100 LOOP = 1,NDSK
         IF (CNAME.EQ.DSKCLA(LOOP)) GO TO 200
 100     CONTINUE
C                                       Nope?
      EXIST = .TRUE.
      GO TO 999
C                                       Check disk file
 200  MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL FNDSKC (NAME, DISK, CNO, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         DISK = 0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
      IF ((DISK.LE.0) .OR. (CNO.LE.0)) GO TO 999
C                                       Tables are different
      IF (CNAME.EQ.'TABLE') THEN
         CALL TBLKUP (NAME, DISK, CNO, FTYPE, VER, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         CALL FNAGET (NAME, 'NAMCLSTY', TYPE, DIM, DUMMY, NAMCLT, IERR)
         IF (IERR.NE.0) GO TO 990
         FTYPE = NAMCLT(19:20)
         VER = 1
         END IF
C                                       Does file exist?
      CALL ZPHFIL (FTYPE, DISK, CNO, VER, FNAME, IERR)
      CALL ZEXIST (DISK, FNAME, SIZE, IERR)
      EXIST = IERR.EQ.0
      IF (IERR.EQ.1) IERR = 0
      GO TO 999
C                                       Error
 995  CALL MSGWRT (7)
 990  MSGTXT = 'OBFSIZ: DETERMINING SIZE OF ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OBACF (DISK, CNO, INDEX)
C-----------------------------------------------------------------------
C   Determines if a specified AIPS disk and catalog slot already has
C   an AIPS catalog flags set and registered in the DFIL.INC common and
C   if so which entry in the CFIL.INC arrays correspond to this AIPS
C   catalog entry.
C   Inputs:
C      DISK   I  AIPS disk number
C      CNO    I  AIPS catalog slot number
C   Output:
C      INDEX  I  Corresponding index in DFIL.INC arrays, 0 => not found.
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, INDEX
C
      INTEGER   LOOP
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      INDEX = 0
      DO 100 LOOP = 1,NCFILE
         IF ((FVOL(LOOP).EQ.DISK) .AND. (FCNO(LOOP).EQ.CNO)) THEN
            INDEX = LOOP
            GO TO 999
            END IF
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE OBCSYN (NAME, IERR)
C-----------------------------------------------------------------------
C   Reads a new version of the catalog header for NAME from disk.
C   Inputs:
C      NAME    C*?  Name of input object.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER   DISK, CNO
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:CLASSIO.INC'
C-----------------------------------------------------------------------
C                                       Find disk, cno
      CALL OBDSKC (NAME, DISK, CNO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Fetch  CATBLK
      CALL CATIO ('READ', DISK, CNO, CATBLK, 'REST', SBUFF, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.4)) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Svae CATBLK
      CALL OBHPUT (NAME, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (7)
 995  MSGTXT = 'OBCSYN: ERROR SYNCING CATBLK FOR  ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' READING SCRATCH FILE CATBLK')
      END
      SUBROUTINE FNDSKC (NAME, DISK, CNO, IERR)
C-----------------------------------------------------------------------
C   Return Disk and slot information for object, works for any class and
C   the object need not have been previously opened.
C      Also saves FILE_NAME NAMCLSTY and CNO.
C   Inputs:
C      NAME   C*32  Name of object
C   Outputs:
C      DISK   I     Disk number
C      CNO    I     Catalog slot number
C      IERR   I     Error code, 0=OK. 1= file not found.
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   DISK, CNO, IERR
C
      INTEGER   TYPE, DIM(3), CSEQ, CUSID, DUMMY
      CHARACTER CNAME*12, CCLASS*6, CPTYPE*2, STAT*4, FNCT*20, CDUMMY*1
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Find disk  and slot
      CALL FNAGET (NAME, 'DISK', TYPE, DIM, DISK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL FNAGET (NAME, 'NAME', TYPE, DIM, DUMMY, CNAME, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL FNAGET (NAME, 'CLASS', TYPE, DIM, DUMMY, CCLASS, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL FNAGET (NAME, 'IMSEQ', TYPE, DIM, CSEQ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CNO = 0
      CPTYPE = '  '
      CUSID = 0
      CALL CATDIR ('SRCH', DISK, CNO, CNAME, CCLASS, CSEQ, CPTYPE,
     *   CUSID, STAT, SBUFF, IERR)
      IF (IERR.EQ.5) THEN
         IERR = 1
         GO TO 999
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 995
         END IF
C                                       Save values
      FNCT = CNAME // CCLASS // CPTYPE
      DIM(1) = LEN (FNCT)
      DIM(2) = 1
      CALL FNAPUT (NAME, 'NAMCLSTY', OOACAR, DIM, DUMMY, FNCT, IERR)
      IF (IERR.NE.0) GO TO 990
      DIM(1) = 1
      CALL FNAPUT (NAME, 'CNO', OOAINT, DIM, CNO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 995  CALL MSGWRT (6)
 990  MSGTXT = 'FNDSKC: ERROR WITH ' // NAME
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FNDSKC: CATDIR ERROR ', I4)
      END
