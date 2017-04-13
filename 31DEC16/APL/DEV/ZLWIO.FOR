      SUBROUTINE ZLWIO (OP, LUN, FILIN, NCHAR, CBUFF, IERR)
C-----------------------------------------------------------------------
C! open, write to, close and spool a PostScript print/plot file
C# Z Printer Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999, 2001-2003, 2012, 2014
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
C   Open a temporary file for printing a plot on a PostScript printer
C   using the using the name ZLWIO.XXXXXX where XXXXXX is a unique
C   extension (OP = 'OPEN'),  write data to the temporary file (OP =
C   'WRIT'), or close, print and delete the file (OP='CLOS').
C   Permanet files are allowed as well.
C   Inputs:
C      OP      C*4     Operation code ('OPEN', 'WRIT' or 'CLOS')
C                      OPEN will append.
C      LUN     I       Logical unit number
C      FILIN   C*48    Unexpanded filename
C      NCHAR   I(5)    Number of characters to print ('WRIT' only)
C      CBUFF   C*(*)   I/O buffer ('WRIT' only)
C   Output:
C      NCHAR   I(5)    Open in/out: init page number, page bounding
C      IERR    I       Error return code: 0 => no error
C                         1 => input error
C                         3 => no such logical device or
C                              forming temporary file name
C                         6 => I/O error
C   Generic version - calls ZFULLN, ZLWOP, ZLASCL
C-----------------------------------------------------------------------
      CHARACTER OP*4, FILIN*(*), CBUFF*(*)
      INTEGER   LUN, NCHAR(5), IERR
C
      CHARACTER FILNAM*256, FILTMP*256, LINE*132
      INTEGER   FLEN, SYSERR, DELFIL, IOSVAL, JTRIM, LUN2, NC, I, J,
     *   BB(4), IND2, JT
      LOGICAL   T, F, EXISTS, ISFILE
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE., .FALSE./
      DATA LUN2 /11/
C-----------------------------------------------------------------------
C                                       OP = 'OPEN' or 'POPN'
      LUN = 3
      IERR = 0
C
      IF (FILIN.NE.' ') ISFILE = T
C
      IF ((OP.EQ.'OPEN') .OR. (OP.EQ.'POPN')) THEN
C                                       get a full file name
         CALL ZFULLN (FILIN, 'PLOTTER', 'ZLWIO', FILNAM, IERR)
         FLEN = JTRIM (FILNAM)
         IF ((IERR.NE.0) .OR. (FLEN.LE.0)) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (6)
            IERR = 2
            GO TO 999
            END IF
C                                       Make sure file name does not
C                                       already exist.
         INQUIRE (FILE=FILNAM, EXIST=EXISTS)
         IF ((EXISTS) .AND. (.NOT.ISFILE)) THEN
            IERR = 3
            WRITE (MSGTXT,1010) FILNAM(1:MIN (FLEN, 40))
            CALL MSGWRT (6)
            IF (FLEN.GT.40) THEN
               WRITE (MSGTXT,1011) FILNAM(41:FLEN)
               CALL MSGWRT (6)
               END IF
            WRITE (MSGTXT,1012)
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                       Append to file
         IF (EXISTS) THEN
C                                       get a full temp name
            CALL ZFULLN (' ', 'PLOTTER', 'ZLWIO', FILTMP, IERR)
            IF ((IERR.NE.0) .OR. (JTRIM(FILTMP).LE.0)) THEN
               WRITE (MSGTXT,1001) IERR
               CALL MSGWRT (6)
               IERR = 2
               GO TO 999
               END IF
            CALL ZTXREN (LUN, FILNAM, LUN2, FILTMP, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1005) 'RENAME', 'OLD', IERR
               CALL MSGWRT (6)
               GO TO 999
               END IF
            CALL ZTXOPN ('READ', LUN2, IND2, FILTMP, .FALSE., IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1005) 'OPEN', 'TEMP', IERR
               CALL MSGWRT (6)
               GO TO 999
               END IF
C                                       Open the file.
            CALL ZLWOP (OP, LUN, FILNAM, IOSVAL)
            IF (IOSVAL.NE.0) THEN
               IERR = 3
               SYSERR = IOSVAL
               WRITE (MSGTXT,1020) SYSERR
               CALL MSGWRT (6)
               WRITE (MSGTXT,1011) FILNAM(1:MIN (FLEN, 48))
               CALL MSGWRT (6)
               CALL ZERROR ('OPEN', SYSERR, ' ', -999, F)
            ELSE
               WRITE (MSGTXT,1010) FILNAM(1:MIN (FLEN, 40))
               CALL MSGWRT (3)
               IF (FLEN.GT.40) THEN
                  WRITE (MSGTXT,1011) FILNAM(41:MIN(FLEN,102))
                  CALL MSGWRT (3)
                  END IF
               IF (FLEN.GT.102) THEN
                  WRITE (MSGTXT,1011) FILNAM(103:MIN(FLEN,164))
                  CALL MSGWRT (3)
                  END IF
               END IF
            NCHAR(1) = 0
            DO 90 I = 1,1000000000
               CALL ZTXIO ('READ', LUN2, IND2, LINE, IERR)
               IF (IERR.EQ.2) GO TO 100
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1005) 'READ', 'TEMP', IERR
                  CALL MSGWRT (6)
                  GO TO 999
                  END IF
               JT = JTRIM (LINE)
               IF (LINE.EQ.'%!PS-Adobe-3.0 EPSF-3.0') LINE =
     *            '%!PS-Adobe-3.0'
               NC = JTRIM (LINE)
               IF (LINE(:8).EQ.'%%Page:') THEN
                  READ (LINE,1006) J
                  NCHAR(1) = MAX (NCHAR(1), J)
               ELSE IF (LINE(:19).EQ.'%%PageBoundingBox: ') THEN
                  READ (LINE,1007) BB
                  NCHAR(2) = MIN (NCHAR(2), BB(1))
                  NCHAR(3) = MIN (NCHAR(3), BB(2))
                  NCHAR(4) = MAX (NCHAR(4), BB(3))
                  NCHAR(5) = MAX (NCHAR(5), BB(4))
               ELSE IF (LINE(:9).EQ.'%%Trailer') THEN
                  GO TO 100
                  END IF
               WRITE (LUN,'(A)',IOSTAT=IOSVAL) LINE(1:NC)
               IF (IOSVAL.NE.0) THEN
                  IERR = 3
                  SYSERR = IOSVAL
                  WRITE (MSGTXT,1100) SYSERR
                  CALL MSGWRT (6)
                  CALL ZERROR ('ZLWIO', SYSERR, ' ', -999, F)
                  GO TO 999
                  END IF
 90            CONTINUE
 100        CALL ZTXCLS (LUN2, IND2, IERR)
            CALL ZTXZAP (LUN2, FILTMP, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1005) 'ZAP', 'TEMP', IERR
               CALL MSGWRT (6)
               END IF
C                                       Open the file.
         ELSE
            CALL ZLWOP (OP, LUN, FILNAM, IOSVAL)
            IF (IOSVAL.NE.0) THEN
               IERR = 3
               SYSERR = IOSVAL
               WRITE (MSGTXT,1020) SYSERR
               CALL MSGWRT (6)
               WRITE (MSGTXT,1011) FILNAM(1:MIN (FLEN, 48))
               CALL MSGWRT (6)
               CALL ZERROR ('OPEN', SYSERR, ' ', -999, F)
            ELSE
               WRITE (MSGTXT,1010) FILNAM(1:MIN (FLEN, 40))
               CALL MSGWRT (6)
               END IF
            END IF
C                                       OP = 'WRIT'
C                                       Dump buffer to the file
      ELSE IF (OP.EQ.'WRIT') THEN
         WRITE (LUN,'(A)',IOSTAT=IOSVAL) CBUFF(1:NCHAR(1))
         IF (IOSVAL.NE.0) THEN
            IERR = 3
            SYSERR = IOSVAL
            WRITE (MSGTXT,1100) SYSERR
            CALL MSGWRT (6)
            CALL ZERROR ('ZLWIO', SYSERR, ' ', -999, F)
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
            CALL ZERROR ('ZLWIO', SYSERR, ' ', -999, F)
C                                       Close and print the file.
         ELSE
            DELFIL = 1
            IF (ISFILE) DELFIL = -1
            CALL ZLASCL (FILNAM, LUN, DELFIL, SYSERR, IERR)
            IF (IERR.NE.0) THEN
               IERR = 3
               CALL ZERROR ('ZLASCL', SYSERR, ' ', -999, F)
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
 1000 FORMAT ('ZLWIO: TROUBLE BUILDING OUTPUT FILE NAME',I5)
 1001 FORMAT ('ZLWIO: TROUBLE BUILDING TEMPORARY FILE NAME',I5)
 1005 FORMAT ('ZLWIO: TROUBLE IN ',A,' ON ',A,' FILE IERR=',I5)
 1006 FORMAT (15X,I7)
 1007 FORMAT (19X,4I7)
 1010 FORMAT ('ZLWIO: plotter file = ',A)
 1011 FORMAT (A)
 1012 FORMAT ('ZLWIO: ALREADY EXISTS')
 1020 FORMAT ('ZLWIO: FORTRAN OPEN ERROR = ',I6,' FOR PLOTTER FILE =')
 1100 FORMAT ('ZLWIO: FORTRAN WRITE ERROR = ',I6)
 1200 FORMAT ('ZLWIO: FORTRAN INQUIRE ERROR = ',I6)
 1210 FORMAT ('ZLWIO: FORTRAN CLOSE ERROR = ',I6)
 1300 FORMAT ('ZLWIO: INVALID OPERATION CODE = ',A4)
      END
