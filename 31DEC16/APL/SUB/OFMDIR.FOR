      SUBROUTINE OFMDIR (OPER, NAME, N, OFM, IERR)
C-----------------------------------------------------------------------
C! read, create/write, delete, or directory-list OFM save files
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2002, 2009, 2012
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
C   Does operations on files used to save OFMs - namely list directory,
C   read from disk, create and save to disk, and destroy.  The OFM file
C   is an AIPS ZTX... file normally save in the logical area 'AIPSOFM'
C   (system user # 1) or 'OFMFIL' (normal users).  Each record has the
C   form input intensity, ':', blue out, green out, red out.  All are
C   integers and the outs are scaled from 0 through 10000.  The file
C   names are xxxxxxxx.uuu where uuu is the user number in hex.  User 1
C   files may be read also from the AIPSOFM area.
C   Inputs:
C      OPER     C*4      'DIR', 'GET', 'SAVE', 'ZAP'
C      NAME     C*48     File name (without the ".uuu").  If a ':'
C                        appears, the name is assumed to be exact
C                        and may be anywhere on the disk.  Not used
C                        by 'DIR'.  Min match is used in 'GET' only.
C      N        I        First dimension of OFM
C   In/out:
C      OFM      R(N,3)   current OFM values - ignored in 'DIR', 'ZAP'
C                           'GET' returns the contents of a file
C                           'SAVE' writes the OFM to a file
C      IERR     I        error code: 0 okay
C                           50 'DISK PROBLEM'
C                           31 'UNAVAILABLE!' - bad OPER
C                           45 'FILE MISSING' - problem w NAME
C                          101 - file missing (GET) or present (SAVE)
C   The routine does no IO to the TV - that is the job of the calling
C   routine (if appropriate).
C-----------------------------------------------------------------------
      CHARACTER OPER*(*), NAME*48
      INTEGER   N, IERR
      REAL      OFM(N,3)
C
      INTEGER   POTERR, JERR, NNAM, NNAM2, NMAX, JTRIM, I, FLEN, J, J2,
     *   NCH, ICH, ILIS(400), IT, OFMLUN, OFMIND, IB, IG, IR, NMSG,
     *   NLEVS, IDATE(3), NGNU
      PARAMETER (NGNU = 23)
      LOGICAL   F, ISPEC, EXISTS
      CHARACTER HUSER*3, XLATED*256, FILSPC*256, ANAME*10, LINE*132,
     *   GPLBRK(2)*35, GPLDAT*16, GPLTXT(2,NGNU)*35, GPLTX1(2,10)*35,
     *   GPLTX2(2,10)*35, GPLTX3(2,NGNU-20)*35
      HOLLERITH NAMES(3,400)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      EQUIVALENCE (GPLTXT(1,1), GPLTX1(1,1))
      EQUIVALENCE (GPLTXT(1,11), GPLTX2(1,1))
      EQUIVALENCE (GPLTXT(1,21), GPLTX3(1,1))
      DATA F /.FALSE./
      DATA OFMLUN /11/
      DATA GPLDAT /'  Copyright (C) '/
      DATA GPLBRK /'-----------------------------------',
     *             '-----------------------------------'/
      DATA GPLTX1 /'  Associated Universities, Inc. Was',
     *             'hington DC, USA.                   ',
     *             '                                   ',
     *             '                                   ',
     *             '  This program is free software; yo',
     *             'u can redistribute it and/or       ',
     *             '  modify it under the terms of the ',
     *             'GNU General Public License as      ',
     *             '  published by the Free Software Fo',
     *             'undation; either version 2 of      ',
     *             '  the License, or (at your option) ',
     *             'any later version.                 ',
     *             '                                   ',
     *             '                                   ',
     *             '  This program is distributed in th',
     *             'e hope that it will be useful,     ',
     *             '  but WITHOUT ANY WARRANTY; without',
     *             ' even the implied warranty of      ',
     *             '  MERCHANTABILITY or FITNESS FOR A ',
     *             'PARTICULAR PURPOSE.  See the       '/
      DATA GPLTX2 /'  GNU General Public License for mo',
     *             're details.                        ',
     *             '                                   ',
     *             '                                   ',
     *             '  You should have received a copy o',
     *             'f the GNU General Public           ',
     *             '  License along with this program; ',
     *             'if not, write to the Free          ',
     *             '  Software Foundation, Inc., 675 Ma',
     *             'ssachusetts Ave, Cambridge,        ',
     *             '  MA 02139, USA.                   ',
     *             '                                   ',
     *             '                                   ',
     *             '                                   ',
     *             '  Correspondence concerning AIPS sh',
     *             'ould be addressed as follows:      ',
     *             '         Internet email: aipsmail@n',
     *             'rao.edu.                           ',
     *             '         Postal address: AIPS Proje',
     *             'ct Office                          '/
      DATA GPLTX3 /'                         National R',
     *             'adio Astronomy Observatory         ',
     *             '                         520 Edgemo',
     *             'nt Road                            ',
     *             '                         Charlottes',
     *             'ville, VA 22903-2475 USA           '/
C-----------------------------------------------------------------------
      POTERR = 50
      IF ((OPER.NE.'DIR') .AND. (OPER.NE.'GET') .AND. (OPER.NE.'SAVE')
     *   .AND. (OPER.NE.'ZAP')) THEN
         MSGTXT = 'OFMDIR: UNKNOWN OPER ' // OPER
         POTERR = 31
         GO TO 985
         END IF
      ISPEC = F
      IF (OPER.NE.'DIR') ISPEC = INDEX(NAME,':').GT.0
      CALL ZEHEX (NLUSER, 3, HUSER)
C                                       get directory list
      IF ((OPER.EQ.'DIR') .OR. ((OPER.EQ.'GET') .AND. (.NOT.ISPEC)))
     *   THEN
         POTERR = 45
C                                       Look for user's files
         IF (OPER.EQ.'GET') THEN
            NCH = JTRIM (NAME)
            IF (NCH.LE.0) THEN
               MSGTXT = 'A FILE NAME MUST BE SPECIFIED'
               GO TO 985
               END IF
            IF (NLUSER.NE.1) THEN
               XLATED = 'OFMFIL:' // NAME(:NCH) // '*.' // HUSER
            ELSE
               XLATED = 'AIPSOFM:' // NAME(:NCH) // '*.' // HUSER
               END IF
         ELSE
            IF (NLUSER.NE.1) THEN
               XLATED = 'OFMFIL:*.' // HUSER
            ELSE
               XLATED = 'AIPSOFM:*.' // HUSER
               END IF
            END IF
         CALL ZFULLN (XLATED, ' ', ' ', FILSPC, JERR)
         IF (JERR.NE.0) GO TO 980
         FLEN = JTRIM (FILSPC)
         NMAX = 400
         CALL ZTXMA2 (FLEN, FILSPC, NMAX, 0, NNAM, NAMES(1,1), JERR)
         IF (JERR.GT.1) GO TO 970
         IF (JERR.EQ.1) NNAM = 0
         NNAM2 = 0
C                                       Look for system's files
         IF (NLUSER.NE.1) THEN
            IF (OPER.EQ.'GET') THEN
               XLATED = 'AIPSOFM:' // NAME(:NCH) // '*.000'
            ELSE
               XLATED = 'AIPSOFM:*.000'
               END IF
            CALL ZFULLN (XLATED, ' ', ' ', FILSPC, JERR)
            IF (JERR.NE.0) GO TO 980
            FLEN = JTRIM (FILSPC)
            NMAX = NMAX - NNAM
            I = NNAM + 1
            CALL ZTXMA2 (FLEN, FILSPC, NMAX, 0, NNAM2, NAMES(1,I),
     *         JERR)
            IF (JERR.GT.1) GO TO 970
            IF (JERR.EQ.1) NNAM2 = 0
            NNAM = NNAM + NNAM2
            END IF
         IF (NNAM.LE.0) THEN
            MSGTXT = 'NO OFM FILES FOUND'
            GO TO 985
            END IF
         END IF
C                                       Directory listing
      IF (OPER.EQ.'DIR') THEN
         NMAX = NNAM - NNAM2
         IF (NMAX.GT.0) THEN
            WRITE (MSGTXT,1000) NLUSER
            CALL MSGWRT (3)
            DO 20 I = 1,NMAX,5
               J2 = MIN (NMAX, I+4)
               MSGTXT = ' '
               DO 10 J = I,J2
                  CALL H2CHR (10, 1, NAMES(1,J), ANAME)
                  MSGTXT(13*(J-I)+1:) = ANAME
 10               CONTINUE
               CALL MSGWRT (3)
 20            CONTINUE
            END IF
         IF (NNAM2.GT.0) THEN
            MSGTXT = 'System-provided OFM files:'
            CALL MSGWRT (3)
            NMAX = NMAX + 1
            DO 40 I = NMAX,NNAM,5
               J2 = MIN (NNAM, I+4)
               MSGTXT = ' '
               DO 30 J = I,J2
                  CALL H2CHR (10, 1, NAMES(1,J), ANAME)
                  MSGTXT(13*(J-I)+1:) = ANAME
 30               CONTINUE
               CALL MSGWRT (3)
 40            CONTINUE
            END IF
         POTERR = 0
         GO TO 990
         END IF
C                                       GET: do min-match on names
      IF ((OPER.EQ.'GET') .AND. (.NOT.ISPEC)) THEN
         NCH = JTRIM (NAME)
         FLEN = 0
         DO 110 I = 1,NNAM
            CALL H2CHR (10, 1, NAMES(1,I), ANAME)
            ICH = JTRIM (ANAME)
            IT = I
            IF ((ICH.EQ.NCH) .AND. (ANAME.EQ.NAME)) GO TO 130
            IF ((ICH.GT.NCH) .AND. (ANAME(:NCH).EQ.NAME(:NCH))) THEN
               FLEN = FLEN + 1
               ILIS(FLEN) = I
               END IF
 110        CONTINUE
         IF (FLEN.LE.0) THEN
            MSGTXT = 'FILE ' // NAME(:NCH) // ' NOT FOUND'
            POTERR = 45
            GO TO 985
         ELSE IF (FLEN.EQ.1) THEN
            IT = ILIS(1)
            CALL H2CHR (10, 1, NAMES(1,IT), ANAME)
         ELSE
            MSGTXT = 'FILE NAME MIN-MATCH FAILURE; COULD BE:'
            CALL MSGWRT (6)
            POTERR = 101
            DO 120 I = 1,FLEN,5
               MSGTXT = ' '
               J2 = MIN (FLEN, I+4)
               DO 115 J = I,J2
                  CALL H2CHR (10, 1, NAMES(1,ILIS(J)), ANAME)
                  MSGTXT (13*(J-I)+1:) = ANAME
 115              CONTINUE
               CALL MSGWRT (6)
 120           CONTINUE
            GO TO 990
            END IF
 130     ICH = JTRIM (ANAME)
         IF ((IT.LE.NNAM-NNAM2) .AND. (NLUSER.NE.1)) THEN
            XLATED = 'OFMFIL:' // ANAME(:ICH) // '.' // HUSER
         ELSE
            XLATED = 'AIPSOFM:' // ANAME(:ICH) // '.000'
            END IF
      ELSE IF (ISPEC) THEN
         XLATED = NAME
      ELSE
         POTERR = 45
         NCH = JTRIM (NAME)
         IF (NCH.LE.0) THEN
            MSGTXT = 'A FILE NAME MUST BE SPECIFIED'
            GO TO 985
            END IF
         IF (NLUSER.NE.1) THEN
            XLATED = 'OFMFIL:' // NAME(:NCH) // '.' // HUSER
         ELSE
            XLATED = 'AIPSOFM:' // NAME(:NCH) // '.' // HUSER
            END IF
         END IF
C                                       Translate logical
      CALL ZFULLN (XLATED, ' ', ' ', FILSPC, JERR)
      IF (JERR.NE.0) GO TO 980
      FLEN = JTRIM (FILSPC)
C                                       does it exist?
      INQUIRE (FILE=FILSPC(:FLEN), EXIST=EXISTS)
      POTERR = 101
      IF (EXISTS) THEN
         IF (OPER.EQ.'SAVE') THEN
            MSGTXT = 'FILE ALREADY EXISTS: ' // FILSPC
            GO TO 985
            END IF
      ELSE
         IF (OPER.NE.'SAVE') THEN
            MSGTXT = 'FILE MISSING: ' // FILSPC
            IF (OPER.EQ.'ZAP') POTERR = 0
            GO TO 985
            END IF
         END IF
      J2 = OFMINP + 1
      IF (OPER.EQ.'GET') THEN
         CALL ZTXOPN ('READ', OFMLUN, OFMIND, XLATED, F, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1200) JERR, XLATED
            GO TO 985
            END IF
         NMSG = 0
         DO 210 I = 1,J2
 205        CALL ZTXIO ('READ', OFMLUN, OFMIND, LINE, JERR)
            IF (JERR.NE.0) THEN
               WRITE (MSGTXT,1210) JERR, 'READ', I
               IF (JERR.NE.2) GO TO 985
               J = 0
               IF (NLEVS.GT.0) J = J2 / NLEVS
               IF (NLEVS*J.NE.J2) GO TO 985
               GO TO 211
               END IF
            J = JTRIM (LINE)
C                                       skip comments
            IF (LINE(:1).EQ.';') GO TO 205
            IF (LINE(6:6).EQ.':') THEN
               READ (LINE,1215) J, IB, IG, IR
            ELSE
               READ (LINE,1216) J, IB, IG, IR
               END IF
            J = J + 1
            IF ((NMSG.LT.5) .AND. (I.NE.J)) THEN
               WRITE (MSGTXT,1220) J, I
               CALL MSGWRT (6)
               NMSG = NMSG + 1
               END IF
            NLEVS = J
            OFM(J,1) = IB / 10000.0
            OFM(J,2) = IG / 10000.0
            OFM(J,3) = IR / 10000.0
 210        CONTINUE
 211     CALL ZTXCLS (OFMLUN, OFMIND, JERR)
      ELSE IF (OPER.EQ.'SAVE') THEN
         CALL ZTXOPN ('WRIT', OFMLUN, OFMIND, XLATED, F, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1200) JERR, XLATED
            GO TO 985
            END IF
C                                       comments at start
         NCH = JTRIM (NAME)
         LINE = ';;  ' // NAME(:NCH)
         NCH = JTRIM (LINE)
         CALL ZTXIO ('WRIT', OFMLUN, OFMIND, LINE(:NCH), JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1205) JERR, 'WRIT'
            GO TO 985
            END IF
         LINE = ';;' // GPLBRK(1) // GPLBRK(2)
         NCH = JTRIM (LINE)
         CALL ZTXIO ('WRIT', OFMLUN, OFMIND, LINE(:NCH), JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1205) JERR, 'WRIT'
            GO TO 985
            END IF
         CALL ZDATE (IDATE)
         IF (IDATE(1).LT.200) IDATE(1) = IDATE(1) + 1900
         WRITE (LINE,1300) GPLDAT, IDATE(1)
         NCH = JTRIM (LINE)
         CALL ZTXIO ('WRIT', OFMLUN, OFMIND, LINE(:NCH), JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1205) JERR, 'WRIT'
            GO TO 985
            END IF
         DO 310 J = 1,NGNU
            LINE = ';;' // GPLTXT(1,J) // GPLTXT(2,J)
            NCH = JTRIM (LINE)
            CALL ZTXIO ('WRIT', OFMLUN, OFMIND, LINE(:NCH), JERR)
            IF (JERR.NE.0) THEN
               WRITE (MSGTXT,1205) JERR, 'WRIT'
               GO TO 985
               END IF
 310        CONTINUE
         LINE = ';;' // GPLBRK(1) // GPLBRK(2)
         NCH = JTRIM (LINE)
         CALL ZTXIO ('WRIT', OFMLUN, OFMIND, LINE(:NCH), JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1205) JERR, 'WRIT'
            GO TO 985
            END IF
         NMSG = 0
         DO 320 J = 1,J2
            IB = OFM(J,1) * 10000.0 + 0.5
            IG = OFM(J,2) * 10000.0 + 0.5
            IR = OFM(J,3) * 10000.0 + 0.5
            I = J - 1
            IF (I.LE.9999) THEN
               WRITE (LINE,1310) I, IB, IG, IR
               CALL ZTXIO ('WRIT', OFMLUN, OFMIND, LINE(:24), JERR)
            ELSE
               WRITE (LINE,1311) I, IB, IG, IR
               CALL ZTXIO ('WRIT', OFMLUN, OFMIND, LINE(:25), JERR)
               END IF
            IF (JERR.NE.0) THEN
               WRITE (MSGTXT,1210) JERR, 'WRIT', J
               GO TO 985
               END IF
 320        CONTINUE
         CALL ZTXCLS (OFMLUN, OFMIND, JERR)
      ELSE IF (OPER.EQ.'ZAP') THEN
         CALL ZTXZAP (OFMLUN, XLATED, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1400) JERR, XLATED
            GO TO 985
            END IF
         END IF
      POTERR = 0
      GO TO 990
C                                       Error messages
 970  WRITE (MSGTXT,1970) JERR, FILSPC
      GO TO 985
 980  WRITE (MSGTXT,1980) JERR, XLATED
C
 985  CALL MSGWRT (6)
C                                       return an error code
 990  IERR = POTERR
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OFM files belonging to user',I5)
 1200 FORMAT ('ERROR',I5,' OPENING ',A60)
 1205 FORMAT ('ERROR',I5,' ',A4,'ING COMMENT')
 1210 FORMAT ('ERROR',I5,' ',A4,'ING LINE',I5)
 1215 FORMAT (I4,2X,3I6)
 1216 FORMAT (I5,2X,3I6)
 1220 FORMAT ('WARNING: READ LINE ',I5,' EXPECTED LINE',I5)
 1300 FORMAT (';;',A,I4)
 1310 FORMAT (I4,' :',3I6)
 1311 FORMAT (I5,' :',3I6)
 1400 FORMAT ('ERROR',I5,' DELETING ',A60)
 1970 FORMAT ('ERROR',I5,' MATCHING ',A60)
 1980 FORMAT ('ERROR',I5,' TRANSLATING ',A57)
      END
