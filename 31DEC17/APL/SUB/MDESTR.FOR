      SUBROUTINE MDESTR (IVOL, ISLOT, CATBLK, IWBLK, INDEST, IERR)
C-----------------------------------------------------------------------
C! deletes a catalog entry and all files assocated with it
C# Catalog IO-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2001, 2015
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
C   MDESTR will delete a catalog entry for a file, delete all extension
C   files for that file, and then delete the file.  The file must be in
C   the REST state.
C   Inputs:
C      IVOL    I       disk volume number of the file.
C      ISLOT   I       catalog slot number.
C   In/out:
C      INDEST  I       number of extension files destroyed.
C                      (if = -32000 on in, suppress normal msg)
C   Output:
C      CATBLK  I(256)  the header block for this file.
C      IWBLK   I(256)  work buffer.
C      IERR    I       error code: 0 no error
C                           1 = disk error
C                           2 = map too busy
C                           3 = destroy failed somehow
C-----------------------------------------------------------------------
      INTEGER   IVOL, ISLOT, CATBLK(256), IWBLK(256), INDEST, IERR
C
      CHARACTER STATUS*4, PFILE*48, IT*2, NAME*12, CLASS*6, PTYPE*2,
     *   ALPH*26, NONAME(17)*2
      INTEGER   ISEQ, IUSER, IECNT, ID, IE, IVER, INPIND, I, J, J2,
     *   ISIZE, ECOUNT, MSGSAV
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA ALPH /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA NONAME /'AC','BA','BQ','HE','IC','ID','IN','ME','SP','TP',
     *   'TD','GR','PW','MT','TC','TK','TV'/
C-----------------------------------------------------------------------
      INPIND = INDEST
      INDEST = 0
      MSGSAV = MSGSUP
C                                       Find info for this slot.
      CALL CATDIR ('INFO', IVOL, ISLOT, NAME, CLASS, ISEQ, PTYPE, IUSER,
     *   STATUS, IWBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000)
         CALL MSGWRT (7)
         GO TO 970
         END IF
C                                       Does header exist of sufficient
C                                       size?
      CALL ZPHFIL ('CB', IVOL, ISLOT, 1, PFILE, IERR)
      CALL ZEXIST (IVOL, PFILE, ISIZE, IERR)
C                                       Header exists - do normal
      IF ((IERR.EQ.0) .AND. (ISIZE.GE.1)) THEN
C                                       Busy ?
         IF (STATUS.NE.'REST') GO TO 980
C                                       Only login on public catalogs
         IF ((IUSER.NE.NLUSER) .AND. (UCTSIZ.LE.0)) THEN
            WRITE (MSGTXT,1010)
            CALL MSGWRT (6)
            GO TO 990
            END IF
         CALL CATIO ('READ', IVOL, ISLOT, CATBLK, 'WRIT', IWBLK, IERR)
         IF ((IERR.GT.0) .AND. (IERR.LT.4)) GO TO 970
         IF (IERR.NE.0) GO TO 980
C                                        Do extension files first
C                                        suppress ZDESTR messages
         CALL FXHDEX (CATBLK)
         MSGSUP = 32000
         IECNT = 0
         DO 30 I = 1,KIEXTN
            J2 = MIN (46655, CATBLK (KIVER+KIEXTN-I))
            IF (J2.GT.0) THEN
               CALL H2CHR(2, 1, CATBLK (KHEXT+KIEXTN-I), IT)
               ID = 0
               IE = 0
               DO 25 J = 1,J2
                  IVER = J2 - J + 1
                  CALL ZPHFIL (IT, IVOL, ISLOT, IVER, PFILE, IERR)
                  CALL ZDESTR (IVOL, PFILE, IERR)
                  IF (IERR.EQ.0) ID = ID + 1
                  IF (IERR.GE.2) THEN
                     IE = MAX (IE, IVER)
                     WRITE (MSGTXT,1020) IT, IVER
                     CALL MSGWRT (8)
                     IECNT = IECNT + 1
                     END IF
 25               CONTINUE
               CATBLK(KIVER+KIEXTN-I) = IE
               IF ((ID.NE.0) .AND. (INPIND.NE.-32000) .AND.
     *            (PTYPE.NE.'SC')) THEN
                  INDEST = INDEST + ID
                  WRITE (MSGTXT,1025) ID, IT
                  CALL MSGWRT (4)
                  END IF
               END IF
 30         CONTINUE
         MSGSUP = MSGSAV
C                                       update & clear cat
C                                       in case some left
         CALL CATIO ('UPDT', IVOL, ISLOT, CATBLK, 'CLWR', IWBLK, IERR)
C                                       Destroy main file
         CALL ZPHFIL (PTYPE, IVOL, ISLOT, 1, PFILE, IERR)
         CALL ZDESTR (IVOL, PFILE, IERR)
         IF (IERR.LT.2) THEN
            WRITE (MSGTXT,1030) PTYPE, ISLOT, IVOL
         ELSE
            WRITE (MSGTXT,1031) PTYPE, ISLOT, IVOL
            END IF
         IF ((IERR.GE.2) .OR. ((INPIND.NE.-32000) .AND.
     *      (PTYPE.NE.'SC'))) CALL MSGWRT (4)
C                                        Clear cat if no errors
         IF ((IERR.LE.1) .AND. (IECNT.LE.0)) THEN
            CALL CATDIR ('CLOS', IVOL, ISLOT, NAME, CLASS, ISEQ, PTYPE,
     *         IUSER, STATUS, IWBLK, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
C                                       ZEXIST real error
      ELSE IF (IERR.GT.1) THEN
         WRITE (MSGTXT,1050) IERR
         CALL MSGWRT (8)
         GO TO 970
C                                       Header missing recovery
      ELSE
         MSGTXT = 'HEADER FILE MISSING OR EMPTY: BRING OUT THE CANNONS'
         CALL MSGWRT (6)
         DO 120 I = 1,26
            DO 110 J = 1,26
               IT = ALPH(I:I) // ALPH(J:J)
               DO 100 J2 = 1,17
                  IF (IT.EQ.NONAME(J2)) GO TO 110
 100              CONTINUE
               J2 = 1
               CALL ZPHFIL (IT, IVOL, ISLOT, J2, PFILE, IERR)
               CALL ZEXIST (IVOL, PFILE, ISIZE, IERR)
               IF (IERR.EQ.0) THEN
                  CALL ZDESTR (IVOL, PFILE, IERR)
                  IF (IERR.EQ.0) THEN
                     WRITE (MSGTXT,1100) IT, IVOL, ISLOT, J2
                     CALL MSGWRT (2)
                     INDEST = INDEST + 1
                  ELSE
                     WRITE (MSGTXT,1110) IT, IVOL, ISLOT, J2, IERR
                     CALL MSGWRT (6)
                     END IF
                  ECOUNT = 0
                  DO 105 J2 = 2,46655
                     CALL ZPHFIL (IT, IVOL, ISLOT, J2, PFILE, IERR)
                     CALL ZEXIST (IVOL, PFILE, ISIZE, IERR)
                     IF (IERR.EQ.0) THEN
                        CALL ZDESTR (IVOL, PFILE, IERR)
                        IF (IERR.EQ.0) THEN
                           WRITE (MSGTXT,1100) IT, IVOL, ISLOT, J2
                           CALL MSGWRT (2)
                           INDEST = INDEST + 1
                           ECOUNT = 0
                        ELSE
                           WRITE (MSGTXT,1110) IT, IVOL, ISLOT, J2, IERR
                           CALL MSGWRT (6)
                           END IF
                     ELSE
                        ECOUNT = ECOUNT + 1
                        IF (ECOUNT.GT.10) GO TO 110
                        END IF
 105                 CONTINUE
                  END IF
 110           CONTINUE
 120        CONTINUE
         MSGSUP = 32000
         IF (STATUS.EQ.'WRIT') THEN
            CALL CATDIR ('CLWR', IVOL, ISLOT, NAME, CLASS, ISEQ, PTYPE,
     *         IUSER, STATUS, IWBLK, IERR)
            CALL CATDIR ('INFO', IVOL, ISLOT, NAME, CLASS, ISEQ, PTYPE,
     *         IUSER, STATUS, IWBLK, IERR)
            END IF
         IF (STATUS.EQ.'READ') CALL CATDIR ('CLRD', IVOL, ISLOT, NAME,
     *      CLASS, ISEQ, PTYPE, IUSER, STATUS, IWBLK, IERR)
         CALL CATDIR ('CLOS', IVOL, ISLOT, NAME, CLASS, ISEQ, PTYPE,
     *      IUSER, STATUS, IWBLK, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.4) THEN
            MSGTXT = 'SLOT TOO BUSY: DO A CLRSTAT'
            CALL MSGWRT (7)
            END IF
         IERR = 0
         END IF
      GO TO 999
C                                       Error: disk
 970  IERR = 1
      MSGSUP = MSGSAV
      GO TO 999
C                                       Error: busy
 980  IERR = 2
      MSGSUP = MSGSAV
      WRITE (MSGTXT,1980) NAME, CLASS, ISEQ, IVOL
      CALL MSGWRT (7)
      GO TO 999
C                                       Error: destroy fails
 990  IERR = 3
      MSGSUP = MSGSAV
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MDESTR: COULD NOT FIND CATALOG ENTRY')
 1010 FORMAT ('MDESTR CAN ONLY DESTROY DATA BELONGING TO LOGON USER')
 1020 FORMAT (A2,' FILE VERSION',I3,' DID NOT SELF-DESTRUCT ON ',
     *   'COMMAND')
 1025 FORMAT ('Destroyed',I3,' extension files of type ',A2)
 1030 FORMAT ('Destroyed ',A2,' image file: catno=',I7,' disk=',I2)
 1031 FORMAT ('FAILED TO DESTROY ',A2,' IMAGE FILE: CATNO=',I7,
     *   ' DISK=',I2)
 1050 FORMAT ('ZEXIST ERROR',I5)
 1100 FORMAT ('Destroyed ',A2,I3,I6,' ver',I6)
 1110 FORMAT ('Destroy   ',A2,I3,I6,' ver',I6,' failed error',I3)
 1980 FORMAT (A12,'.',A6,'.',I4,' ON VOL',I2,' TOO BUSY')
      END
