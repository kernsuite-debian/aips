      SUBROUTINE ZTPOPD (LUN, FCB, ANAME, MODE, IERR)
C-----------------------------------------------------------------------
C! open a pseudo-tape, sequential disk file for FITS
C# Z2 Tape FITS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999, 2001-2002
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
C   Open a logical, sequential (pseudo-tape) disk file (exclusive use
C   and wait for availability are assumed) using random access Fortran
C   I/O.
C   Inputs:
C      FCB     I(*)   File I/O control block
C      ANAME   H*256  Physical file name
C      MODE    I      0 = 'READ' disk, 2 = 'WRIT' or read disk
C   Output:
C      IERR    I      Error return code: 0 = no error
C                        2 = file not found
C                        6 = other open errors
C                        7 = illegal file name (translation fails)
C                        8 = 'write' file already exists
C   Generic version -  LRECL in bytes, supports Digital in words too
C   Uses Fortran I/O - replace with C I/O for UNIX which will allow
C   files > 2 Gbyte.
C-----------------------------------------------------------------------
      INTEGER   LUN, FCB(*), MODE, IERR
      HOLLERITH ANAME(64)
C
      INTEGER   ITRIM, FLEN, IOSVAL, LREC
      LOGICAL   EXISTS
      CHARACTER PNAME*256, FILNAM*256, LSTAT*3
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
C-----------------------------------------------------------------------
      CALL H2CHR (256, 1, ANAME, PNAME)
C                                       Form full path name of file.
      CALL ZFULLN (PNAME, 'FITS', ' ', FILNAM, IERR)
      IF (IERR.NE.0) THEN
         IERR = 7
         GO TO 999
         END IF
      FLEN = ITRIM (FILNAM)
      IF (MSGSUP.NE.32000) THEN
         WRITE (MSGTXT,1000)
         CALL MSGWRT (2)
         WRITE (MSGTXT,1001) FILNAM(1:MIN (56, FLEN))
         CALL MSGWRT (2)
         IF (FLEN.GT.56) THEN
            WRITE (MSGTXT,1001) FILNAM(57:FLEN)
            CALL MSGWRT (2)
            END IF
         END IF
C                                       Check for existence of file.
      INQUIRE (FILE=FILNAM(:FLEN), EXIST=EXISTS)
C                                       Read only
      IF (MODE.EQ.0) THEN
         LSTAT = 'OLD'
C                                       File should already exist.
         IF (.NOT.EXISTS) THEN
            IERR = 2
            WRITE (MSGTXT,1010)
            END IF
C                                       Read and write
      ELSE
         LSTAT = 'NEW'
C                                       File should NOT already exist.
         IF (EXISTS) THEN
            IERR = 8
            WRITE (MSGTXT,1015)
            END IF
         END IF
C                                       Open the file for unformatted,
C                                       random access - RECL in bytes
      IF (IERR.EQ.0) THEN
         LREC = 2880
         IF ((SYSTYP.EQ.'VMS') .OR. (SYSTYP.EQ.'DEC') .OR.
     *      (SYSTYP.EQ.'ULTR')) LREC = LREC / 4
         OPEN (UNIT=LUN, FILE=FILNAM(:FLEN), ACCESS='DIRECT', RECL=LREC,
     *      FORM='UNFORMATTED', STATUS=LSTAT, IOSTAT=IOSVAL)
         IF (IOSVAL.NE.0) THEN
            IERR = 6
            FCB(1+FCBERR) = IOSVAL
            WRITE (MSGTXT,1020) IOSVAL
            END IF
         END IF
C                                       messages
      IF (IERR.NE.0) CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTPOPD: using translated file name =')
 1001 FORMAT ('ZTPOPD: ',A)
 1010 FORMAT ('ZTPOPD: FILE NOT FOUND')
 1015 FORMAT ('ZTPOPD: FILE ALREADY EXISTS')
 1020 FORMAT ('ZTPOPD: FORTRAN OPEN ERROR = ',I6)
      END
