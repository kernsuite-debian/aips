      SUBROUTINE ZLASIO (OP, LUN, OUTFIL, NCHAR, CBUFF, IERR)
C-----------------------------------------------------------------------
C! open, write to, close and spool a laser printer print/plot file
C# Z Printer Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002, 2012
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
C   Open, write to or close and spool a file for printing a plot on a
C   laser printer.  Does not spool the file if an explicit output
C   file name is given.
C   Inputs:
C      OP       C*4    Operation code: 'OPEN', 'POPN', 'WRIT' or 'CLOS'
C      LUN      I      Logical unit number for the laser printer
C      OUTFIL   C*48   Output file name (used by opcode 'OPEN', 'POPN',
C                      'CLOS')
C      NCHAR    I      Number of characters to print in CBUFF
C                      (used by opcode 'WRIT' only)
C      CBUFF    C*(*)  I/O buffer (used by opcode 'WRIT' only)
C   Output:
C      IERR     I      Error return code: 0 => no error
C                         1 => invalid opcode
C                         2 => trouble translating logical
C                         3 => I/O error
C   Generic version - uses ZFULLN, ZLASOP, ZLASCL
C-----------------------------------------------------------------------
      CHARACTER OP*4, OUTFIL*(*), CBUFF*(*)
      INTEGER   LUN, NCHAR, IERR
C
      CHARACTER FILNAM*256
      INTEGER   FLEN, SYSERR, DELFIL, IOSVAL, ITRIM
      LOGICAL   F, EXISTS
      INCLUDE 'INCS:DMSG.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       OP = 'OPEN' or 'POPN'
      IERR = 0
      IF ((OP.EQ.'OPEN') .OR. (OP.EQ.'POPN')) THEN
C                                       get a full file name
         CALL ZFULLN (OUTFIL, 'PLOTTER', 'ZLASIO', FILNAM, IERR)
         FLEN = ITRIM (FILNAM)
         IF ((IERR.NE.0) .OR. (FLEN.LE.0)) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (6)
            IERR = 2
            GO TO 999
            END IF
C                                       Make sure file name does not
C                                       already exist.
         INQUIRE (FILE=FILNAM, EXIST=EXISTS)
         IF (EXISTS) THEN
            IERR = 3
            MSGTXT = 'ZLASIO: plotter file = ' // FILNAM(1:MIN(FLEN,40))
            CALL MSGWRT (6)
            IF (FLEN.GT.40) THEN
               MSGTXT =  FILNAM(41:MIN(FLEN,104))
               CALL MSGWRT (6)
               END IF
            IF (FLEN.GT.104) THEN
               MSGTXT =  FILNAM(105:MIN(FLEN,168))
               CALL MSGWRT (6)
               END IF
            IF (FLEN.GT.168) THEN
               MSGTXT =  FILNAM(169:MIN(FLEN,232))
               CALL MSGWRT (6)
               END IF
            MSGTXT = 'ZLASIO: ALREADY EXISTS'
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                       Open the file.
         CALL ZLASOP (OP, LUN, FILNAM, IOSVAL)
         IF (IOSVAL.NE.0) THEN
            IERR = 3
            SYSERR = IOSVAL
            WRITE (MSGTXT,1020) SYSERR
            CALL MSGWRT (6)
            MSGTXT = FILNAM(1:MIN (FLEN, 64))
            CALL MSGWRT (6)
            IF (FLEN.GT.64) THEN
               MSGTXT = FILNAM(65:MIN(FLEN,128))
               CALL MSGWRT (6)
               END IF
            IF (FLEN.GT.128) THEN
               MSGTXT = FILNAM(129:MIN(FLEN,192))
               CALL MSGWRT (6)
               END IF
            CALL ZERROR ('OPEN', SYSERR, ' ', -999, F)
         ELSE
            MSGTXT = 'ZLASIO: plotter file = ' // FILNAM(1:MIN(FLEN,40))
            CALL MSGWRT (2)
            IF (FLEN.GT.40) THEN
               MSGTXT = FILNAM(41:MIN(FLEN,104))
               CALL MSGWRT (2)
               END IF
            IF (FLEN.GT.104) THEN
               MSGTXT = FILNAM(105:MIN(FLEN,168))
               CALL MSGWRT (2)
               END IF
            IF (FLEN.GT.168) THEN
               MSGTXT = FILNAM(169:MIN(FLEN,232))
               CALL MSGWRT (2)
               END IF
            END IF
C                                       OP = 'WRIT'
C                                       Dump buffer to the file.
      ELSE IF (OP.EQ.'WRIT') THEN
         WRITE (LUN,1011,IOSTAT=IOSVAL) CBUFF(1:NCHAR)
         IF (IOSVAL.NE.0) THEN
            IERR = 3
            SYSERR = IOSVAL
            WRITE (MSGTXT,1100) SYSERR
            CALL MSGWRT (6)
            CALL ZERROR ('ZLASIO', SYSERR, ' ', -999, F)
            END IF
C                                       OP = 'CLOS'
      ELSE IF (OP.EQ.'CLOS') THEN
C                                       Get file name opened on LUN.
         INQUIRE (UNIT=LUN, NAME=FILNAM, IOSTAT=IOSVAL)
         IF (IOSVAL.NE.0) THEN
            IERR = 3
            SYSERR = IOSVAL
            WRITE (MSGTXT,1200) SYSERR
            CALL MSGWRT (6)
            CALL ZERROR ('ZLASIO', SYSERR, ' ', -999, F)
C                                       Close and print the file.
C                                       if OUTFIL is blank, delete it.
         ELSE
            DELFIL = -1
            IF (OUTFIL.EQ.' ') DELFIL = 1
            CALL ZLASCL (FILNAM, LUN, DELFIL, SYSERR, IERR)
            IF (IERR.NE.0) THEN
               IF (IERR.EQ.2) CALL ZERROR ('ZLASCL', SYSERR, ' ',
     *            -999, F)
               IERR = 3
               END IF
            END IF
C                                       Invalid operation code.
      ELSE
         IERR = 1
         WRITE (MSGTXT,1300) OP
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZLASIO: TROUBLE BUILDING OUTPUT FILE NAME',I5)
 1011 FORMAT (A)
 1020 FORMAT ('ZLASIO: FORTRAN OPEN ERROR = ',I6,' FOR PLOTTER FILE =')
 1100 FORMAT ('ZLASIO: FORTRAN WRITE ERROR = ',I6)
 1200 FORMAT ('ZLASIO: FORTRAN INQUIRE ERROR = ',I6)
 1210 FORMAT ('ZLASIO: FORTRAN CLOSE ERROR = ',I6)
 1300 FORMAT ('ZLASIO: INVALID OPERATION CODE = ',A4)
      END
