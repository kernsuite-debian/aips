      SUBROUTINE ZDIR (TYPE, RVERSN, FILE, FULNAM, IERR)
C-----------------------------------------------------------------------
C! build a full path name to files in AIPS-standard areas (HE, RU, ...)
C# Text IO-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2002
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
C   Given a file type (e.g., 'HE' for HELP, 'RU' for RUN, etc.), a
C   version (e.g., 'OLD:', 'NEW:', 'TST:', 'MYAREA' as a logical,
C   /aippgmr/khilldru as a literal, etc.) and a file name (e.g., UVSRT,
C   MX, etc.), build the full path name of the file (e.g.,
C   /AIPS/15JUL87/HELP/MX.HLP).
C   Inputs:
C      TYPE     C*2    File type
C      RVERSN   C*48   Version - directory path name)
C      FILE     C*(*)  File name (if a '.' is not found in the name, a
C                      default extension determined from TYPE)
C   Output:
C      FULNAM   C*(*)  Full path name of the file
C      IERR     I      Error return code: 0 => no error
C                         1 => invalid file type
C                         2 => no such logical
C                         3 => error in directory name
C   Generic UNIX version.
C-----------------------------------------------------------------------
      CHARACTER TYPE*2, RVERSN*48, FILE*(*), FULNAM*(*)
      INTEGER   IERR
C
      INTEGER   I, NTYPES, IT, FLEN, SLEN, LLEN, XLEN, XLNB, IX, ITRIM
      CHARACTER CVERSN*48, FILNAM*64, SUBDIR*10, LOGNAM*48, XLATED*256,
     *   STDTYP(6)*2, STDEXT(5)*4, STDSUB(4)*10
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA NTYPES, STDTYP /6, 'HE', 'DO', 'EX', 'SO', 'RU', 'RT'/
      DATA STDSUB /'/HELP/', '/DOC/TEXT/', '/LOAD/', 'XXXX'/
      DATA STDEXT /'.HLP', '.', '.EXE', '.FOR', '.RUN'/
C-----------------------------------------------------------------------
      FULNAM = ' '
      FILNAM = FILE
      FLEN = INDEX (FILNAM, ' ') - 1
      LLEN = LEN (LOGNAM)
      XLEN = LEN (XLATED)
      IERR = 1
C                                       Check file type.
      DO 10 IT = 1,NTYPES
         IF (STDTYP(IT).EQ.TYPE) GO TO 20
 10      CONTINUE
C                                       Unknown type.
      WRITE (MSGTXT,1010) TYPE
      CALL MSGWRT (6)
      GO TO 999
C                                       Branch on type.
 20   IERR = 0
C                                       "Standard" file.
      SUBDIR = '/'
      IF (IT.LE.4) THEN
C                                       Add default extension to FILNAM
C                                       if needed.
         IF (INDEX(FILNAM,'.').EQ.0) FILNAM(FLEN+1:) = STDEXT(IT)
C                                       VERSION: if blank, use VERNAM.
         CVERSN = RVERSN
         IF (CVERSN.EQ.' ') CVERSN = VERNAM
         I = ITRIM (CVERSN)
         IF (CVERSN(I:I).EQ.':') CVERSN(I:I) = ' '
C                                       Recognized version, use standard
C                                       subdirectory.
         IF ((CVERSN(1:4).EQ.'CVX ') .OR. (CVERSN(1:4).EQ.'TST ') .OR.
     *      (CVERSN(1:4).EQ.'NEW ') .OR. (CVERSN(1:4).EQ.'OLD ') .OR.
     *      (CVERSN(1:6).EQ.'LOCAL ') .OR. (CVERSN.EQ.'AIPS_VERSION '))
     *      THEN
            SUBDIR = STDSUB(IT)
            LOGNAM = CVERSN
C                                       Pseudo AP (special rules).
         ELSE IF (((CVERSN(1:8).EQ.'CVXPSAP ') .OR.
     *      (CVERSN(1:8).EQ.'TSTPSAP ') .OR. (CVERSN(1:8).EQ.'NEWPSAP ')
     *      .OR. (CVERSN(1:8).EQ.'OLDPSAP ')) .AND. (TYPE.NE.'EX')) THEN
            SUBDIR = STDSUB(IT)
            LOGNAM = CVERSN(1:3)
C                                       Literal directory path name.
         ELSE IF (INDEX (CVERSN, '/').GT.0) THEN
            I = INDEX (CVERSN, ' ')
            IF (I.EQ.0) I = LEN (CVERSN)
            FULNAM = CVERSN(1:I) // '/' // FILNAM
            SUBDIR = ' '
C                                       CVERSN is user defined logical
         ELSE
            LOGNAM = CVERSN
            END IF
C                                       RUN File.
      ELSE IF (IT.EQ.5) THEN
C                                       Form hexadecimal user id
C                                       extension from user number.
         IX = FLEN + 1
         FILNAM(IX:IX) = '.'
         IX = IX + 1
         CALL ZEHEX (NLUSER, 3, FILNAM(IX:IX+2))
C                                       Local RUN file?
         CVERSN = RVERSN
         IF (CVERSN(1:6).EQ.'LOCAL ') THEN
            LOGNAM = 'LOCRUN'
C                                       "Standard" RUN file.
         ELSE IF ((CVERSN.EQ.'CVX') .OR.(CVERSN.EQ.'TST') .OR.
     *      (CVERSN.EQ.'NEW') .OR. (CVERSN.EQ.'OLD') .OR.
     *      (CVERSN.EQ.' ')) THEN
            LOGNAM = 'RUNFIL'
C                                       user area instead
         ELSE
            LOGNAM = CVERSN
            END IF
C                                       "Root" File.
      ELSE IF (IT.EQ.6) THEN
         LOGNAM = 'AIPS_ROOT'
         END IF
C                                       Translate
      SLEN = INDEX (SUBDIR, ' ') - 1
      IF (SLEN.LT.0) SLEN = LEN (SUBDIR)
      IF (SLEN.GT.0) THEN
         CALL ZTRLOG (LLEN, LOGNAM, XLEN, XLATED, XLNB, IERR)
         IF (IERR.EQ.0) THEN
            FULNAM = XLATED(1:XLNB) // SUBDIR(1:SLEN) // FILNAM
C                                       Bad logical name.
         ELSE
            IERR = 2
            WRITE (MSGTXT,1990)
            CALL MSGWRT (6)
            WRITE (MSGTXT,1991) LOGNAM
            CALL MSGWRT (6)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ZDIR: INVALID FILE TYPE = ',A2)
 1990 FORMAT ('ZDIR: COULD NOT TRANSLATE LOGICAL = ')
 1991 FORMAT ('ZDIR: ',A)
      END
