      SUBROUTINE AU5F (BRANCH)
C-----------------------------------------------------------------------
C! verbs to delet boxes
C# POPS-appl TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2006, 2008, 2012
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
C   AU5C performs additional TV image formation functions:
C   BRANCH = 1   DELBOX    delete boxes interactively
C          = 2   DFILEBOX  delete boxes from a box file
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      INCLUDE 'INCS:PCLN.INC'
      CHARACTER LINTYP*2, PRGNAM*6, CDUM*1, BOXFIL*48, TXLINE*132,
     *   ATIME*8, ADATE*12, SCNAME*256, CLASS*6, ULOG*24, UNAME*48,
     *   TMLINE*132
      LOGICAL   UNIQUE, SAVEBU
      INTEGER   J, POTERR, IERR, IPLAN, IPL, I, IROUND, IBUF(1280), NBO,
     *   IGR(2), IDUM, INBO, IFIELD, TXLUN, TXIND, TXLUN2, TXIND2,
     *   IBX(4), ITIME(3),IDATE(3), MSGSAV, KBP, JTRIM, IOFF, COLP
      REAL      BB(7,MXNBOX), TT(7,MXNBOX), BXX(100), BYY(100), RDUM,
     *   RIBUF(4096), BOX(4,MXCLBX)
      DOUBLE PRECISION X
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PUVD.INC'
      COMMON /AIPSCR/ RIBUF
      EQUIVALENCE (BXX, BB),  (BYY, TT)
      EQUIVALENCE (IBUF, RIBUF)
      DATA PRGNAM /'AU5C  '/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.2)) GO TO 999
C                                        Open the TV
 20   POTERR = 101
      CALL TVOPEN (IBUF, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Get a plane and image
 40   LINTYP = 'MA'
      CALL TVFIND (NGRAY, LINTYP, IPLAN, UNIQUE, CATBLK, IBUF, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('GRCHAN',  'I', 1, 0, IGR(1), RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      CALL ADVERB ('GR2CHAN',  'I', 1, 0, IGR(2), RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      IF (IGR(1).EQ.0) IGR(1) = 3
      IF (IGR(2).EQ.0) IGR(2) = MIN (7, IGR(1) + 4)
C                                        Branch to OP
 50   GO TO (100, 200), BRANCH
C-----------------------------------------------------------------------
C                                       DELBOX
C                                       reset NBOXES boxes with graphics
C-----------------------------------------------------------------------
 100  CALL ADVERB ('NBOXES',  'I', 1, 0, INBO, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      NBO = INBO
      IF (NBO.LE.0) THEN
         MSGTXT = 'DELBOX MUST START WITH SOME BOXES!'
         CALL MSGWRT (8)
         POTERR = 101
         GO TO 975
         END IF
C                                       pick up old Boxes & convert
      CALL ADVERB ('CLBOX', 'R', 200, 0, IDUM, BOX, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      DO 110 I = 1,NBO
         BB(1,I) = BOX(1,I)
         BB(2,I) = BOX(2,I)
         TT(1,I) = BOX(3,I)
         TT(2,I) = BOX(4,I)
 110     CONTINUE
C                                       init image catalog
      IPL = NGRAY + IGR(1)
      CALL YCINIT (IPL, IBUF)
      IPL = NGRAY + IGR(2)
      CALL YCINIT (IPL, IBUF)
      CALL GRBOXD (IGR, NBO, BB, TT, IBUF, IERR)
      IF (IERR.NE.0) THEN
         POTERR = 49
         GO TO 975
         END IF
      IF (NBO.NE.INBO) THEN
         WRITE (MSGTXT,1100) INBO, NBO
         CALL MSGWRT (6)
         CALL ADVRBS ('NBOXES',  'I', 1, 0, NBO, RDUM, CDUM)
         END IF
      CALL RFILL (200, 0.0, BOX)
      DO 120 I = 1,NBO
         BOX(1,I) = BB(1,I)
         BOX(2,I) = BB(2,I)
         BOX(3,I) = TT(1,I)
         BOX(4,I) = TT(2,I)
         WRITE (MSGTXT,1101) I, (BOX(J,I), J = 1,4)
         CALL MSGWRT (2)
 120     CONTINUE
      CALL ADVRBS ('CLBOX', 'R', 200, 0, IDUM, BOX, CDUM)
      POTERR = 0
      GO TO 975
C-----------------------------------------------------------------------
C                                       DFILEBOX
C                                       delete boxes with graphics
C                                       write to text file
C-----------------------------------------------------------------------
C                                       open file in FILEBOX
 200  CALL ADVERB ('NFIELD', 'I', 1, 0, IFIELD, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      IF ((IFIELD.LT.1) .OR. (IFIELD.GT.MAXFLD)) THEN
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), CLASS)
         IF ((CLASS(4:4).LT.'0') .OR. (CLASS(4:4).GT.'9')) THEN
            IF (CLASS(5:6).EQ.'  ') THEN
               IFIELD = 1
            ELSE
               CALL ZREHEX (2, CLASS(5:6), IFIELD)
               IFIELD = IFIELD + 1
               END IF
         ELSE IF ((CLASS(5:5).LT.'0') .OR. (CLASS(5:5).GT.'9') .OR.
     *      (CLASS(6:6).LT.'0') .OR. (CLASS(6:6).GT.'9')) THEN
            IFIELD = 0
         ELSE
            READ (CLASS(4:6),1200) IFIELD
            END IF
         IF ((IFIELD.GE.1) .AND. (IFIELD.LE.MAXFLD)) THEN
            WRITE (MSGTXT,1201) CLASS, IFIELD
         ELSE
            IFIELD = 1
            MSGTXT = 'Class no help: Field 1 assumed'
            END IF
         CALL MSGWRT (2)
         END IF
      CALL ADVERB ('BOXFILE', 'C', 1, 48, IDUM, RDUM, BOXFIL)
      IF (ERRNUM.NE.0) GO TO 975
      TXLUN = 3
      TXLUN2 = 11
      NBO = 0
      IOFF = 0
      SAVEBU = .FALSE.
      SCNAME = ' '
C                                       Read in pre-existing
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL ZTXOPN ('QRED', TXLUN, TXIND, BOXFIL, .TRUE., IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.0) THEN
C                                       do a backup file
         COLP = INDEX (BOXFIL, ':')
         IF (COLP.LE.1) THEN
            MSGTXT = 'WARNING: NO LOGICAL: BACKUP FILE IN $FITS'
            CALL MSGWRT (6)
            CALL ZFULLN (' ', 'FITS', 'FILEBOX', SCNAME, IERR)
         ELSE
            ULOG = BOXFIL(:COLP-1)
            UNAME = BOXFIL(COLP+1:)
            CALL ZFULLN (' ', ULOG, UNAME, SCNAME, IERR)
            END IF
         POTERR = 55
         IF (IERR.NE.0) GO TO 970
         CALL ZTXOPN ('QWRT', TXLUN2, TXIND2, SCNAME, .FALSE., IERR)
         IF (IERR.NE.0) GO TO 970
         DO 210 I = 1,100000
            CALL ZTXIO ('READ', TXLUN, TXIND, TXLINE, IERR)
            IF (IERR.EQ.2) GO TO 215
            IF (IERR.NE.0) GO TO 965
            J = JTRIM (TXLINE)
            J = MAX (1, J)
            CALL ZTXIO ('WRIT', TXLUN2, TXIND2, TXLINE(:J), IERR)
            IF (IERR.NE.0) GO TO 965
 210        CONTINUE
 215     SAVEBU = .TRUE.
C                                       close both
         CALL ZTXCLS (TXLUN, TXIND, IERR)
         CALL ZTXCLS (TXLUN2, TXIND2, IERR)
         POTERR = 55
C                                       this is less dangerous!
         CALL ZTXZAP (TXLUN, BOXFIL, IERR)
         CALL ZTXOPN ('WRIT', TXLUN, TXIND, BOXFIL, .FALSE., IERR)
         IF (IERR.NE.0) GO TO 971
         CALL ZTXOPN ('QRED', TXLUN2, TXIND2, SCNAME, .FALSE., IERR)
         IF (IERR.NE.0) GO TO 970
         POTERR = 50
C                                       read back backup to new orig.
         DO 230 I = 1,100000
            CALL ZTXIO ('READ', TXLUN2, TXIND2, TXLINE, IERR)
            IF (IERR.EQ.2) GO TO 235
            IF (IERR.NE.0) GO TO 965
            CALL CHTRIM (TXLINE, 132, TMLINE, J)
            KBP = 1
            IF (TMLINE(:1).EQ.' ') GO TO 220
            IF (TMLINE(:1).LT.'0') GO TO 220
            IF (TMLINE(:1).GT.'9') GO TO 220
            CALL GETNUM (TMLINE, 132, KBP, X)
            IF ((ERRNUM.NE.0) .OR. (X.EQ.DBLANK)) GO TO 965
            RDUM = X
            J = IROUND (RDUM)
            IF (J.NE.IFIELD) GO TO 220
C                                       window to read in
            CALL GETNUM (TMLINE, 132, KBP, X)
            IF ((ERRNUM.NE.0) .OR. (X.EQ.DBLANK)) GO TO 965
            RDUM = X
            IBX(1) = IROUND (RDUM)
            CALL GETNUM (TMLINE, 132, KBP, X)
            IF ((ERRNUM.NE.0) .OR. (X.EQ.DBLANK)) GO TO 965
            RDUM = X
            IBX(2) = IROUND (RDUM)
            CALL GETNUM (TMLINE, 132, KBP, X)
            IF ((ERRNUM.NE.0) .OR. (X.EQ.DBLANK)) GO TO 965
            RDUM = X
            IBX(3) = IROUND (RDUM)
            CALL GETNUM (TMLINE, 132, KBP, X)
            IF ((ERRNUM.NE.0) .OR. (X.EQ.DBLANK)) GO TO 965
            RDUM = X
            IBX(4) = IROUND (RDUM)
C                                       keep this
            NBO = NBO + 1
            BB(1,NBO) = IBX(1)
            BB(2,NBO) = IBX(2)
            TT(1,NBO) = IBX(3)
            TT(2,NBO) = IBX(4)
            GO TO 230
C                                       otherwise write to scratch
 220        J = JTRIM (TXLINE)
            J = MAX (1, J)
            CALL ZTXIO ('WRIT', TXLUN, TXIND, TXLINE(:J), IERR)
            IF (IERR.NE.0) GO TO 965
 230        CONTINUE
 235     CALL ZTXCLS (TXLUN2, TXIND2, IERR)
C                                       create a new BOXFILE
      ELSE
         POTERR = 55
         CALL ZTXOPN ('WRIT', TXLUN, TXIND, BOXFIL, .FALSE., IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
C                                       add the time
      CALL ZTIME (ITIME)
      CALL ZDATE (IDATE)
      CALL TIMDAT (ITIME, IDATE, ATIME, ADATE)
      TXLINE = '#  FILEBOX: ' // ADATE // ATIME
      J = JTRIM (TXLINE)
      POTERR = 50
      CALL ZTXIO ('WRIT', TXLUN, TXIND, TXLINE(:J), IERR)
      IF (IERR.NE.0) GO TO 970
      POTERR = 31
      IPL = NGRAY + IGR(1)
      CALL YCINIT (IPL, IBUF)
      IPL = NGRAY + IGR(2)
      CALL YCINIT (IPL, IBUF)
      INBO = NBO
      IF (INBO.LE.0) THEN
         MSGTXT = 'NO BOXES FOUND - DOING NOTHING'
         CALL MSGWRT (7)
      ELSE
         WRITE (MSGTXT,1240) NBO
         CALL MSGWRT (3)
         CALL GRBOXD (IGR, NBO, BB, TT, IBUF, IERR)
         IF (IERR.NE.0) THEN
            POTERR = 49
            MSGTXT = 'TRYING TO SAVE BOXES ANYWAY - YOU MAY HAVE TO '
     *         // 'EDIT THE FILE'
            CALL MSGWRT (6)
            END IF
         IF (NBO.NE.INBO) THEN
            WRITE (MSGTXT,1100) INBO, NBO
            CALL MSGWRT (6)
            END IF
         POTERR = 50
         DO 250 I = 1,NBO
C                                       round
            IF (BB(1,I).LT.-0.5) THEN
               IBX(1) = IROUND (BB(1,I))
               IBX(2) = IROUND (BB(2,I))
               IBX(3) = IROUND (TT(1,I))
               IBX(4) = IROUND (TT(2,I))
            ELSE
               IBX(1) = BB(1,I) + 0.01
               IBX(2) = BB(2,I) + 0.01
               IBX(3) = TT(1,I) + 0.99
               IBX(4) = TT(2,I) + 0.99
               END IF
            WRITE (MSGTXT,1245) IFIELD, I, IBX
            CALL MSGWRT (2)
            WRITE (TXLINE,1246) IFIELD, IBX
            CALL CHTRIM (TXLINE, 132, TXLINE, J)
            CALL ZTXIO ('WRIT', TXLUN, TXIND, TXLINE(:J), IERR)
            IF (IERR.NE.0) GO TO 970
 250        CONTINUE
         END IF
      SAVEBU = .FALSE.
      POTERR = 0
      GO TO 970
C-----------------------------------------------------------------------
C                                        Close downs
 965  CALL ZTXCLS (TXLUN2, TXIND2, IERR)
 970  CALL ZTXCLS (TXLUN, TXIND, IERR)
 971  IF (SCNAME.NE.' ') THEN
         IF (SAVEBU) THEN
            MSGTXT = 'BOXFILE backup saved in file:'
            CALL MSGWRT (8)
            MSGTXT = SCNAME
            CALL MSGWRT (8)
         ELSE
            CALL ZTXZAP (TXLUN2, SCNAME, IERR)
            END IF
         END IF
 975  CALL TVCLOS (IBUF, IERR)
C
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.EQ.0) GO TO 999
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('NBOXES reset from',I4,' to',I4)
 1101 FORMAT ('BOX(',I3,') = ',4F8.2)
 1200 FORMAT (I3)
 1201 FORMAT ('Class ',A,' implies field number',I3)
 1240 FORMAT (I4,' boxes to be reset - some may not be visible')
 1241 FORMAT (I4,' boxes to be displayed - some may not be visible')
 1245 FORMAT ('FIELD',I3.2,' BOX(',I3.3,') = ',4I7.4)
 1246 FORMAT (I4,4I7)
      END
