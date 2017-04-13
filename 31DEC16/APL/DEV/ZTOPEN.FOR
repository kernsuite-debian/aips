      SUBROUTINE ZTOPEN (LUN, FIND, IVOL, PNAME, MNAME, VERSON, WAIT,
     *   IERR)
C-----------------------------------------------------------------------
C! open text file - logical area, version, member name as arguments
C# Z Text
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999, 2002, 2013
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
C   Open a text file - logical area, version, member name as arguments
C   Inputs:
C      LUN      I      Logical unit number
C      IVOL     I      Disk volume containing file, (not used)
C      PNAME    C*48   Physical file name, only used to determine file
C                      type or logical area
C      MNAME    C*(*)  Text file name  (<= 12 chars)
C      VERSON   C*48   Logical name for directory or version of
C                      directory to search (for file-specific
C                      directories)
C      WAIT     L      T => wait until file is available (not used)
C   Outputs:
C      FIND     I      Index in FTAB for LUN
C      IERR     I      Error return code: 0 => no error
C                         1 => LUN already in use
C                         2 => file not found
C                         3 => volume not found
C                         4 => file locked
C                         5 => no room for LUN in FTAB
C                         6 => other open errors
C   Generic version - uses ZDIR, ZFULLN, and Fortran functions INQUIRE
C   and then calls ZTOPE2.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IVOL, IERR
      CHARACTER VERSON*48, PNAME*48, MNAME*(*)
      LOGICAL   WAIT
C
      INTEGER   I, J, JERR, IOSVAL, ITRIM, LN, LF
      LOGICAL   MAP, EXISTS
      CHARACTER FILTYP*2, FILSPC*256, XLATED*256
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA MAP /.FALSE./
C-----------------------------------------------------------------------
C                                       Check inputs.
      IERR = 6
C                                       Valid LUN?
      IF ((LUN.LE.0) .OR. (LUN.GT.99)) THEN
         WRITE (MSGTXT,1000) LUN
         GO TO 995
         END IF
C                                       Valid device type?
      IF (DEVTAB(LUN).NE.3) THEN
         WRITE (MSGTXT,1010) LUN, DEVTAB(LUN)
         GO TO 995
         END IF
C                                       Allocate area in FTAB for LUN.
      CALL LSERCH ('OPEN', LUN, FIND, MAP, JERR)
      IF (JERR.NE.0) THEN
C                                       LUN already in use.
         IF (JERR.EQ.2) THEN
            IERR = 1
            WRITE (MSGTXT,1022) LUN
C                                       No room in FTAB for LUN.
         ELSE IF (JERR.EQ.3) THEN
            IERR = 5
            WRITE (MSGTXT,1023) LUN
         ELSE
            WRITE (MSGTXT,1020) JERR, 'OPEN', LUN
            END IF
         GO TO 995
         END IF
C                                       Get file type ('HE','RU', etc.).
      I = INDEX(PNAME,':')
      FILTYP = PNAME(I+1:I+2)
C                                       Create complete file
C                                       specification (ZDIR will append
C                                       proper extension, if any).
      IERR = 0
      CALL ZDIR (FILTYP, VERSON, MNAME, FILSPC, IERR)
C                                       Check for existence and open.
      IF (IERR.NE.0) GO TO 990
      LF = ITRIM (FILSPC)
      LN = ITRIM (MNAME)
      INQUIRE (FILE=FILSPC(1:LF), EXIST=EXISTS)
C                                       Not found
      IF (.NOT.EXISTS) THEN
C                                       Check system RUN file directory
         IF (FILTYP.EQ.'RU') THEN
            IF ((VERSON.EQ.'CVX') .OR. (VERSON.EQ.'TST') .OR.
     *         (VERSON.EQ.'NEW') .OR. (VERSON.EQ.'OLD') .OR.
     *         (VERSON.EQ.' ')) THEN
               XLATED = 'RUNSYS:' // MNAME(1:LN) // '.001'
            ELSE
               XLATED = VERSON(:ITRIM(VERSON)) // ':' // MNAME(1:LN) //
     *            '.001'
               END IF
            CALL ZFULLN (XLATED, ' ', ' ', FILSPC, JERR)
            IF (JERR.NE.0) THEN
               WRITE (MSGTXT,1032) 'RUNSYS'
               CALL MSGWRT (6)
C                                       Does this version exist?
            ELSE
               LF = ITRIM (FILSPC)
               INQUIRE (FILE=FILSPC(1:LF), EXIST=EXISTS)
               END IF
            END IF
C                                       Can't find it.
         IF (.NOT.EXISTS) THEN
            IERR = 2
            WRITE (MSGTXT,1050) FILTYP, MNAME(:LN)
            GO TO 985
            END IF
         END IF
C                                       Found it, now open it.
      CALL ZTOPE2 (LUN, FILSPC, WAIT, IOSVAL, IERR)
C                                       OPEN error?
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IOSVAL
         CALL MSGWRT (6)
         DO 70 J = 1,I,56
            WRITE (MSGTXT,1061) FILSPC(J:J+55)
            CALL MSGWRT (6)
 70         CONTINUE
         CALL ZERROR ('ZTOPE2', IOSVAL, ' ', -999, .FALSE.)
         GO TO 990
         END IF
C                                       Rewind.
      REWIND (LUN)
C                                       store file name
      CALL FSERCH (FIND, I, J, JERR)
      IF (JERR.EQ.0) THEN
         IF (I.EQ.1) DEVNAM(J) = FILSPC(1:48)
         IF (I.EQ.2) NONNAM(J) = FILSPC(1:48)
         IF (I.EQ.3) MAPNAM(J) = FILSPC(1:48)
         END IF
      GO TO 999
C
 985  CALL MSGWRT (6)
C                                       Error.  Remove FTAB entry.
 990  CALL LSERCH ('CLOS', LUN, FIND, MAP, JERR)
      IF (JERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1020) JERR, 'CLOS', LUN
C
 995  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTOPEN: INVALID LUN = ',I2)
 1010 FORMAT ('ZTOPEN: INVALID DEVTAB(',I2,') = ',I1)
 1020 FORMAT ('ZTOPEN: LSERCH ERROR ',I2,' ON ',A4,' LUN = ',I2)
 1022 FORMAT ('ZTOPEN: LUN = ',I2,' ALREADY OPENED IN FTAB')
 1023 FORMAT ('ZTOPEN: NO ROOM IN FTAB FOR LUN = ',I2)
 1032 FORMAT ('ZTOPEN: TROUBLE TRANSLATING LOGICAL = ',A)
 1050 FORMAT ('ZTOPEN: CAN''T FIND ',A,' FILE =''',A,'''')
 1060 FORMAT ('ZTOPEN: FORTRAN OPEN ERROR = ',I6,' FOR FILE =')
 1061 FORMAT ('ZTOPEN: ',A)
      END
