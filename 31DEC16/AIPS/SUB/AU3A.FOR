      SUBROUTINE AU3A (BRANCH)
C-----------------------------------------------------------------------
C! verbs for disk management: FREE, ALLDEST, TIMDEST, etc.
C# POPS-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999, 2003, 2007-2008, 2012-2013, 2015
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
C   AU3A contains the disk management verbs ALLDEST (verb to destroy
C   all or most of a users data files) and SCRDEST (destroy all
C   scratch files created by a specific task).  SAVDEST destroy user's
C   SAVE/GET files.
C   Inputs:
C      BRANCH I   1 = FREESPAC (print total freespace on disks)
C                 2 = ALLDEST
C                 3 = TIMDEST  (destroy all old files)
C                 4 = SAVDEST
C                 5 = SCRDEST
C                 6 = M2CAT   **** NOW IN AU4A ***
C                 7 = U2CAT   **** NOW IN AU4A ***
C                 8 = GETVERS   get max vers of extension
C                 9 = QGETVERS  ditto but quietly
C                10 = DAYNUMBR
C   Common:  /CAPL/
C      INNAME   H(3)   (ALLDEST) image name.
C      INCLASS  H(2)   (ALLDEST) image class.
C      INSEQ    R      (ALLDEST) image seq no.
C      NLUSER   I      (ALLDEST, SAVDEST, SCRDEST) logon user #.
C      TASK     H(2)   (SCRDEST) task name.
C      DETIME   R      (ALLDEST, TIMDEST) min age for destroy
C      PRTIME   R      (TIMDEST) min age message file destroy
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER LOCNAM*12, LOCCLS*6, LOCTYP*2, PFILE*48, PRGNAM*6,
     *   DNAME*12, DCLAS*6, DTYPE*2, EXTYPE*2, ANAME*16, NOTASK*6,
     *   NOLP*48, CDUM*1, XXTYPE*2, STAT*4, OBSDAT*8
      DOUBLE PRECISION    JD, JD0, DTIME(36)
      REAL      TIMRUL(36), TIMSG, MTIME, TIMSC, TIMCA, MANAGR, RDUM,
     *   DELTIM
      INTEGER   IUSERS(400), ITIME(6), IBLK(256), ISCR(256), I, I4T,
     *   IBVOL, ICLUN, ICNO, ICUR, IERR, IEVOL, IFIND, IMAX, IMOD, J1,
     *   INUSER, LOCSEQ, IUSER, IVER, IVOL, J, POTERR, LVOL, NCLS, NN,
     *   NNAME, NPTY, NSEQ, NSTAT, XLUSER, ILUSER, XLUSE1, XLUSE2, IE,
     *   IECNT, IER, IKEEP, INO, NLEFT, J2, NDEST, NLPR, NWPL, NPAT(12),
     *   CPAT(6), NDONE, HLUN, HIND, LREC, IREC, IHREC, IDUM, QUICK,
     *   SASSGN(280), CONF, IDATE(3), DAYN
      LOGICAL   EQUAL, ALLDEF, T, F, PRIVAT, SEQUAL
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /AIPSCR/ IBLK, ISCR, IUSERS
      DATA T, F /.TRUE.,.FALSE./
      DATA PRGNAM /'AU3A  '/
      DATA MANAGR /3.0/
      DATA ICLUN, HLUN /15, 16/
      DATA NCLS, NSEQ, NSTAT, NPTY, NNAME /13, 4, 1, 19, 5/
C-----------------------------------------------------------------------
      NOTASK = ' '
      NOLP = ' '
      CALL COPY (280, DASSGN, SASSGN)
C                                       TIMDEST limits from common
C                                       data: disks 1 - 15, Save/Gets
      CALL RCOPY (35, TIMEDA, TIMRUL)
      TIMRUL(36) = TIMESG
C                                       messages (in days)
      TIMSG = TIMEMS
C                                       Scratch files limit
      TIMSC = TIMESC
C                                       Empty catalog files limit
      TIMCA = TIMECA
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.10)) GO TO 995
C                                       block TIMDEST
      IF (BRANCH.EQ.3) THEN
         MSGTXT = 'TIMDEST IS NO LONGER ALLOWED'
         CALL MSGWRT (8)
         GO TO 995
         END IF
      POTERR = 33
      NWPL = 10
      NLPR = 256 / NWPL
C                                       Determine defaults for disk.
      IF ((BRANCH.NE.6) .AND. (BRANCH.NE.7)) THEN
         CALL ADVERB ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      ELSE
         CALL ADVERB ('IN2DISK', 'I', 1, 0, IVOL, RDUM, CDUM)
         END IF
      IF (ERRNUM.NE.0) GO TO 980
C                                       Only one disk.
      IF ((IVOL.NE.0) .AND. (BRANCH.NE.3)) THEN
         IBVOL = IVOL
         IEVOL = IVOL
      ELSE
         IBVOL = 1
         IEVOL = NVOL
         END IF
C                                       Check confirm on ALLDEST
      IF ((BRANCH.EQ.2) .OR. (BRANCH.EQ.4)) THEN
         CALL ADVERB ('DOCONFRM', 'I', 1, 0, CONF, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         IF (CONF.GE.-1) THEN
            IF ((IUNIT.NE.1) .AND. (IUNIT.NE.4)) THEN
               WRITE (MSGTXT,1010)
               CALL MSGWRT (8)
               POTERR = 53
               IF (IUNIT.EQ.3) POTERR = 60
               GO TO 980
               END IF
            END IF
         END IF
C                                       Branch on opcode.
      GO TO (100, 200, 200, 400, 500, 600, 600, 800, 800, 800), BRANCH
C-----------------------------------------------------------------------
C                                       FREESPAC
C                                       available disk space display
C-----------------------------------------------------------------------
 100  CONTINUE
         I = 1
         IF ((IUNIT.EQ.2) .OR. (IUNIT.EQ.3)) THEN
            I = 2
         ELSE
            CALL ADVERB ('PRTLEV', 'R', 1, 0, IDUM, RDUM, CDUM)
            IF (ERRNUM.NE.0) GO TO 980
            IF (RDUM.GE.1.0) I = 2
            END IF
         CALL ZFREE (I, IERR)
         GO TO 995
C-----------------------------------------------------------------------
C                                       ALLDEST
C                                       Destroy all images matching
C                                       adverbs
C                                       TIMDEST
C                                       Destroy all old images despite
C                                       owner
C-----------------------------------------------------------------------
 200  CALL ZDATE (ITIME(1))
      CALL ZTIME (ITIME(4))
      CALL DAT2JD (ITIME, JD0)
      CALL ADVERB ('DETIME', 'R', 1, 0, IDUM, DELTIM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      DO 201 I = 1,36
         DTIME(I) = DELTIM
         IF (BRANCH.EQ.3) DTIME(I) = MAX (DELTIM, TIMRUL(I))
         IF (DTIME(I).LE.0.0) DTIME(I) = 0.0
 201     CONTINUE
      ALLDEF = .TRUE.
      CALL ADVERB ('INSEQ', 'I', 1, 0, LOCSEQ, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
C                                       Private/public catlgs
      XLUSER = NLUSER
      XLUSE1 = NLUSER
      XLUSE2 = NLUSER
      PRIVAT = .FALSE.
      IF ((BRANCH.EQ.3) .AND. (UCTSIZ.GT.0)) THEN
         PRIVAT = .TRUE.
         XLUSE1 = 1
         XLUSE2 = USELIM
         END IF
C                                       Check for all default values.
      IF (BRANCH.NE.3) THEN
         CALL ADVERB ('INNAME', 'C', 1, 12, IDUM, RDUM, LOCNAM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('INCLASS', 'C', 1, 6, IDUM, RDUM, LOCCLS)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('INTYPE', 'C', 1, 2, IDUM, RDUM, LOCTYP)
         IF (ERRNUM.NE.0) GO TO 980
         IF ((LOCNAM.NE.' ') .OR. (LOCCLS.NE.' ') .OR.
     *      (LOCTYP.NE.' ') .OR. (LOCSEQ.NE.0)) THEN
            ALLDEF = .FALSE.
            WRITE (MSGTXT,1200) LOCTYP, LOCNAM, LOCCLS, LOCSEQ
            CALL MSGWRT (4)
            CALL PSFORM (12, LOCNAM, NPAT)
            CALL PSFORM (6, LOCCLS, CPAT)
            END IF
         END IF
      IF (DTIME(1).GT.0.0) THEN
         J1 = 1
         J2 = MIN (5, NVOL)
         WRITE (MSGTXT,1205) (DTIME(I), I = J1,J2)
         CALL MSGWRT (4)
         ALLDEF = .FALSE.
 206     J1 = J2 + 1
         J2 = MIN (NVOL, J1 + 9)
         IF (J1.LE.NVOL) THEN
            WRITE (MSGTXT,1206) (DTIME(I), I = J1,J2)
            CALL MSGWRT (4)
            GO TO 206
            END IF
         END IF
      IF (BRANCH.EQ.2) WRITE (MSGTXT,1207) NLUSER, IBVOL, IEVOL
      IF (BRANCH.EQ.3) WRITE (MSGTXT,1208)
      CALL MSGWRT (4)
C                                       Chance to back out
      POTERR = 39
      IF (CONF.GE.-1) THEN
         CALL CONFRM (IERR)
         IF (IERR.EQ.1) GO TO 995
         IF (IERR.GT.1) GO TO 980
         END IF
      IF (BRANCH.EQ.3) CALL FILL (280, 0, DASSGN)
      POTERR = 33
      INUSER = 0
      IUSERS(1) = -99
C                                       Loop through all volumes.
      DO 290 ILUSER = XLUSE1,XLUSE2
      DO 290 IVOL = IBVOL,IEVOL
         NDEST = 0
         NLUSER = ILUSER
         IF (PRIVAT) MSGSUP = 32000
         LVOL = -IVOL
         CALL CATOPN (LVOL, IFIND, IBLK, IMAX, IERR)
         MSGSUP = 0
         NLUSER = XLUSER
         IF (IERR.NE.0) GO TO 290
C                                       get catalog access time
         IKEEP = 0
         CALL  DAT2JD (IBLK(10), JD)
         IF (JD0-JD.LT.TIMCA) IKEEP = 10
         IF ((PRIVAT) .AND. (ILUSER.EQ.1). AND. (JD0-JD.LT.MANAGR*TIMCA)
     *      .AND. (BRANCH.EQ.3)) IKEEP = 10
C                                       Loop through all cat entries.
         ICUR = 995
         IREC = 1
         DO 270 ICNO = 1,IMAX
            IMOD = (ICNO - 1) / NLPR
            NN = 1 + NWPL * (ICNO - NLPR*IMOD - 1)
            ICUR = ICUR + NWPL
            IF (ICUR.LT.NWPL*NLPR) GO TO 215
C                                       Save last record.
               IF (NDEST.EQ.0) GO TO 210
                  CALL ZFIO ('WRIT', ICLUN, IFIND, IREC, IBLK, IERR)
                  IF (IERR.NE.0) GO TO 280
                  NDEST = 0
C                                       Read new buffer.
 210           CONTINUE
                  IREC = IREC + 1
                  CALL ZFIO ('READ', ICLUN, IFIND, IREC, IBLK, IERR)
                  IF (IERR.NE.0) GO TO 280
                  ICUR = 1
C                                       See if entry matches values.
C                                       Time
 215        IF (IBLK(NN).EQ.-1) GO TO 270
            IKEEP = IKEEP + 1
            CALL H2CHR (6, NCLS, IBLK(NN+NNAME), DCLAS)
            CALL H2CHR (12, 1, IBLK(NN+NNAME), DNAME)
            CALL H2CHR (2, NPTY, IBLK(NN+NNAME), DTYPE)
            SEQUAL = 'SC'.EQ.DTYPE
            CALL CATIME (2, IBLK(NN+2), ITIME)
            CALL DAT2JD (ITIME, JD)
            IF ((SEQUAL) .AND. (JD0-JD.LT.TIMSC)) GO TO 270
            IF ((.NOT.SEQUAL) .AND. (JD0-JD.LT.DTIME(IVOL))) GO TO 270
            IF ((.NOT.SEQUAL) .AND. (JD0-JD.LT.MANAGR*DTIME(IVOL)) .AND.
     *         (IBLK(NN).EQ.1) .AND. (BRANCH.EQ.3)) GO TO 270
            IF (BRANCH.EQ.3) GO TO 230
C                                       User #: ALLDEST only
               IF (NLUSER.NE.IBLK(NN)) GO TO 270
C                                       Test name.
               IF (LOCNAM.EQ.' ') GO TO 220
                  CALL CHWMAT (12, LOCNAM, NPAT, 1, DNAME, EQUAL)
                  IF (.NOT.EQUAL) GO TO 270
C                                       Test class.
 220           IF (LOCCLS.EQ.' ') GO TO 225
                  CALL CHWMAT (6, LOCCLS, CPAT, 1, DCLAS, EQUAL)
                  IF (.NOT.EQUAL) GO TO 270
C                                       Test seq #, phys type
 225           IF ((LOCSEQ.NE.0) .AND. (IBLK(NN+NSEQ).NE.LOCSEQ))
     *               GO TO 270
               IF (LOCTYP.EQ.' ') GO TO 230
                  IF (LOCTYP.NE.DTYPE) GO TO 270
C                                       Do this one hopefully
C                                       File busy: NOW A REMARK ONLY
 230        IF ((IBLK(NN+NSTAT).EQ.0) .OR. (SEQUAL)) GO TO 235
               WRITE (MSGTXT,1230) DTYPE, DNAME, DCLAS, IBLK(NN+NSEQ),
     *            IVOL, IBLK(NN)
               CALL MSGWRT (3)
C                                       Confirm deletion
 235        IF (CONF.GT.0) THEN
               WRITE (MSGTXT,1235) DTYPE, DNAME, DCLAS, IBLK(NN+NSEQ),
     *            IVOL, IBLK(NN)
               CALL MSGWRT (3)
               CALL CONFRM (IERR)
               IF (IERR.GT.0) GO TO 270
C                                       Progress report
            ELSE
               WRITE (MSGTXT,1237) DTYPE, DNAME, DCLAS, IBLK(NN+NSEQ),
     *            IVOL, IBLK(NN)
               CALL MSGWRT (3)
               END IF
C                                       Get header.
            IHREC = 1
            NLUSER = ILUSER
            CALL ZPHFIL ('CB', IVOL, ICNO, 1, PFILE, IERR)
            NLUSER = XLUSER
            IER = 0
            CALL ZOPEN (HLUN, HIND, IVOL, PFILE, F, F, T, IERR)
            IF (IERR.NE.0) GO TO 270
            CALL ZFIO ('READ', HLUN, HIND, IHREC, CATBLK, IERR)
            IF (IERR.NE.0) CALL ZCLOSE (HLUN, HIND, IER)
            IF (IERR.NE.0) GO TO 270
C                                       Do extension files.
            IECNT = 0
            CALL FXHDEX (CATBLK)
            DO 250 I = 1,KIEXTN
               J2 = MIN (46655, CATBLK(KIVER+KIEXTN-I))
               IF (J2.LE.0) GO TO 250
                  CALL H2CHR (2, 1, CATH(KHEXT-I+KIEXTN), EXTYPE)
                  INO = 0
                  IE = 0
C                                       Destroy all possible versions.
                  DO 240 J = 1,J2
                     IVER = J2 - J + 1
                     NLUSER = ILUSER
                     CALL ZPHFIL (EXTYPE, IVOL, ICNO, IVER, PFILE, IERR)
                     NLUSER = XLUSER
                     CALL ZEXIST (1, PFILE, I4T, IERR)
                     IF (IERR.EQ.0) CALL ZDESTR (IVOL, PFILE, IERR)
                     IF (IERR.EQ.0) INO = INO + 1
                     IF (IERR.GE.2) THEN
                        IE = MIN (IE, IVER)
                        WRITE (MSGTXT,1238) EXTYPE, IVER
                        CALL MSGWRT (8)
                        IECNT = IECNT + 1
                        END IF
 240                 CONTINUE
                  CATBLK(KIVER+KIEXTN-I) = IE
C                                       Print message of success
                  IF (INO.GT.0) THEN
                     WRITE (MSGTXT,1240) INO, EXTYPE
                     CALL MSGWRT (2)
                     END IF
 250           CONTINUE
C                                       Put header back
            IERR = 0
            IF (IECNT.GT.0) CALL ZFIO ('WRIT', HLUN, HIND, IHREC,
     *         CATBLK, IERR)
            CALL ZCLOSE (HLUN, HIND, IER)
            IF (IERR.NE.0) GO TO 270
C                                       Destroy main file.
            NLUSER = ILUSER
            CALL ZPHFIL (DTYPE, IVOL, ICNO, 1, PFILE, IERR)
            NLUSER = XLUSER
            CALL ZDESTR (IVOL, PFILE, IERR)
            IF (IERR.LT.2) GO TO 260
               WRITE (MSGTXT,1250) IERR
               CALL MSGWRT (6)
               IECNT = IECNT + 1
C                                       Remove from catalog.
 260        IF (IECNT.GT.0) GO TO 270
               IUSER = IBLK(NN)
               IBLK(NN) = -1
               NDEST = NDEST + 1
               IKEEP = IKEEP - 1
C                                       destroy header file
               NLUSER = ILUSER
               CALL ZPHFIL ('CB', IVOL, ICNO, 1, PFILE, IERR)
               NLUSER = XLUSER
               CALL ZDESTR (IVOL, PFILE, IERR)
               IF (IERR.LT.2) GO TO 264
                  WRITE (MSGTXT,1260) IERR
                  CALL MSGWRT (6)
C                                       Accumulate user #'s
 264           DO 265 J = 1,INUSER
                  IF (IUSERS(J).EQ.IUSER) GO TO 270
 265              CONTINUE
               INUSER = INUSER + 1
               IUSERS(INUSER) = IUSER
 270        CONTINUE
C                                       Save last record.
         IF (NDEST.EQ.0) GO TO 280
            CALL ZFIO ('WRIT', ICLUN, IFIND, IREC, IBLK, IERR)
C                                       Close catalog
 280     CALL ZCLOSE (ICLUN, IFIND, IER)
C                                       Destroy Catlg file?
         IF ((IERR.NE.0) .OR. (IKEEP.GT.0) .OR. (BRANCH.NE.3) .OR.
     *      (ILUSER.EQ.XLUSER)) GO TO 290
            NLUSER = ILUSER
            CALL ZPHFIL ('CA', IVOL, 0, 0, PFILE, IERR)
            NLUSER = XLUSER
            CALL ZDESTR (IVOL, PFILE, IERR)
            IF (IERR.GT.1) GO TO 290
               WRITE (MSGTXT,1280) ILUSER, IVOL
               CALL MSGWRT (2)
 290     CONTINUE
C                                       Continue with others ?
      IF (BRANCH.EQ.3) GO TO 300
         IF (ALLDEF) GO TO 400
         GO TO 995
C-----------------------------------------------------------------------
C                                       TIMDEST
C                                       Continue by killing SG, TS
C-----------------------------------------------------------------------
C                                       Loop over users so far deleted
 300  DO 390 IUSER = 1,USELIM
         IVOL = 1
C                                       Save/get directory file
         NLUSER = IUSER
         CALL ZPHFIL ('SG', IVOL, IUSER, 0, PFILE, IERR)
         NLUSER = XLUSER
         CALL ZEXIST (1, PFILE, I4T, IERR)
         IF (IERR.EQ.0) CALL ZOPEN (ICLUN, IFIND, IVOL, PFILE, F, T, T,
     *      IERR)
         IBVOL = 0
         IEVOL = 0
         IF (IERR.NE.0) GO TO 350
            NWPL = 7
            NLPR = 256 / NWPL
            LREC = 1
            CALL ZFIO ('READ', ICLUN, IFIND, LREC, IBLK, IERR)
            IF (IERR.NE.0) GO TO 345
C                                       Check time
            CALL CATIME (2, IBLK(3), ITIME)
            CALL DAT2JD (ITIME, JD)
            IF (JD0-JD.LT.DTIME(36)) GO TO 345
            IF ((IUSER.EQ.1) .AND. (JD0-JD.LT.MANAGR*DTIME(36)) .AND.
     *         (BRANCH.EQ.3)) GO TO 345
            CALL CATIME (2, IBLK(5), ITIME)
            CALL DAT2JD (ITIME, JD)
            IF (JD0-JD.LT.DTIME(36)) GO TO 345
            IF ((IUSER.EQ.1) .AND. (JD0-JD.LT.MANAGR*DTIME(36)) .AND.
     *         (BRANCH.EQ.3)) GO TO 345
            LOCSEQ = IBLK(1)
            IF (LOCSEQ.LE.0) GO TO 330
               DO 320 I = 1,LOCSEQ
                  IREC = I / NLPR + 1
                  IF (IREC.EQ.LREC) GO TO 310
                     CALL ZFIO ('WRIT', ICLUN, IFIND, LREC, IBLK, IERR)
                     IF (IERR.NE.0) GO TO 330
                     LREC = IREC
                     CALL ZFIO ('READ', ICLUN, IFIND, LREC, IBLK, IERR)
                     IF (IERR.NE.0) GO TO 330
 310              IREC = MOD (I, NLPR) * NWPL + 1
                  IF (IBLK(IREC).LE.0) GO TO 320
                     NLUSER = IUSER
                     CALL ZPHFIL ('SG', IVOL, IUSER, I, PFILE, IERR)
                     NLUSER = XLUSER
                     CALL ZDESTR (IVOL, PFILE, IERR)
                     CALL H2CHR (16,1, IBLK(IREC+3), ANAME)
                     IF (IERR.LT.2) WRITE (MSGTXT,1310) IUSER, ANAME
                     IF (IERR.GE.2) WRITE (MSGTXT,1311) IUSER, ANAME
                     CALL MSGWRT (2)
                     IF (IERR.GE.2) IEVOL = IEVOL + 1
                     IF (IERR.GE.2) GO TO 320
                        IBVOL = IBVOL + 1
                        IBLK(IREC) = 0
 320              CONTINUE
               CALL ZFIO ('WRIT', ICLUN, IFIND, LREC, IBLK, IERR)
 330        CALL ZCLOSE (ICLUN, IFIND, IERR)
C                                       Kill directory
            IF (IEVOL.GT.0) GO TO 340
               NLUSER = IUSER
               CALL ZPHFIL ('SG', IVOL, IUSER, 0, PFILE, IERR)
               NLUSER = XLUSER
               CALL ZDESTR (IVOL, PFILE, IERR)
               IF (IERR.LT.2) WRITE (MSGTXT,1330) IUSER
               IF (IERR.GE.2) WRITE (MSGTXT,1331) IUSER
               IF (IERR.NE.1) CALL MSGWRT (2)
C                                       Print summary also
 340     IF (IBVOL+IEVOL.LE.0) GO TO 350
            WRITE (MSGTXT,1340) IBVOL, IUSER
            CALL MSGWRT (2)
            IF (IEVOL.LE.0) GO TO 350
               WRITE (MSGTXT,1341) IEVOL, IUSER
               CALL MSGWRT (2)
               GO TO 350
 345     CALL ZCLOSE (ICLUN, IFIND, IERR)
C                                       Destroy Task-save file
 350     DO 365 IVER = 1,36
            DO 364 J1 = 1,2
               NLUSER = IUSER
               J2 = 400 * J1
               CALL ZPHFIL ('TG', IVOL, J2, IVER-1, PFILE, IERR)
               NLUSER = XLUSER
               CALL ZEXIST (1, PFILE, I4T, IERR)
               IF (IERR.EQ.0) CALL ZOPEN (ICLUN, IFIND, 1, PFILE, F, T,
     *            T, IERR)
               IF (IERR.EQ.0) THEN
                  LREC = 1
                  CALL ZFIO ('READ', ICLUN, IFIND, LREC, IBLK, IERR)
                  CALL ZCLOSE (ICLUN, IFIND, IDUM)
                  IF (IERR.EQ.0) THEN
                     CALL CATIME (2, IBLK(3), ITIME)
                     CALL DAT2JD (ITIME, JD)
                     IF ((JD0-JD.GE.DTIME(36)) .AND. ((IUSER.NE.1) .OR.
     *                  (JD0-JD.GE.MANAGR*DTIME(36)) .OR.
     *                  (BRANCH.NE.3))) THEN
                        CALL ZDESTR (IVOL, PFILE, IERR)
                        IF (IERR.LT.2) WRITE (MSGTXT,1350) IUSER
                        IF (IERR.GE.2) WRITE (MSGTXT,1351) IUSER
                        IF ((IERR.NE.1) .AND. (IVER.EQ.1))
     *                     CALL MSGWRT (2)
                        END IF
                     END IF
                  END IF
 364           CONTINUE
 365        CONTINUE
C                                       Clear/Destroy message file
         NLUSER = IUSER
         CALL ZPHFIL ('MS', IVOL, IUSER, 0, PFILE, IERR)
         NLUSER = XLUSER
         CALL ZEXIST (1, PFILE, I4T, IERR)
         IF (IERR.NE.0) GO TO 390
         CALL ADVERB ('PRTIME', 'R', 1, 0, IDUM, MTIME, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         IF (BRANCH.EQ.3) MTIME = MAX (MTIME, TIMSG)
         IF (MTIME.LE.0.0) MTIME = 0.0
         CALL PRTMSG ('DELE', IUSER, 0, 0, NOTASK, MTIME, -1, NOLP,
     *      NDONE, NLEFT, IERR)
         ERRNUM = 0
         IF (IERR.NE.0) GO TO 390
         WRITE (MSGTXT,1370) NDONE, IUSER
         IF (NDONE.GT.0) CALL MSGWRT (2)
         IF ((NLEFT.GT.4) .OR. (IUSER.EQ.XLUSER) .OR. (IUSER.EQ.1))
     *      GO TO 390
            CALL ZDESTR (1, PFILE, IERR)
            IF (IERR.LT.2) WRITE (MSGTXT,1371) IUSER
            IF (IERR.GE.2) WRITE (MSGTXT,1372) IUSER
            IF (IERR.NE.1) CALL MSGWRT (2)
 390     CONTINUE
      GO TO 995
C-----------------------------------------------------------------------
C                                       SAVDEST
C-----------------------------------------------------------------------
C                                       Ask permision.
 400  WRITE (MSGTXT,1400) NLUSER
      CALL MSGWRT (4)
      POTERR = 39
      IF (CONF.GE.-1) THEN
         CALL CONFRM (IERR)
         IF (IERR.EQ.1) GO TO 995
         IF (IERR.GT.1) GO TO 980
         END IF
C                                       Delete all save files.
      IVOL = 1
C                                       Get directory
      CALL ZPHFIL ('SG', IVOL, NLUSER, 0, PFILE, IERR)
      CALL ZEXIST (1, PFILE, I4T, IERR)
      IF (IERR.EQ.0) CALL ZOPEN (ICLUN, IFIND, IVOL, PFILE, F, T, T,
     *   IERR)
      IBVOL = 0
      IEVOL = 0
      IF (IERR.EQ.0) THEN
         NWPL = 7
         NLPR = 256 / NWPL
         LREC = 1
         CALL ZFIO ('READ', ICLUN, IFIND, LREC, IBLK, IERR)
         LOCSEQ = IBLK(1)
         IF ((IERR.EQ.0) .AND. (LOCSEQ.GT.0)) THEN
            DO 420 I = 1,LOCSEQ
               IREC = I / NLPR + 1
               IF (IREC.NE.LREC) THEN
                  CALL ZFIO ('WRIT', ICLUN, IFIND, LREC, IBLK, IERR)
                  IF (IERR.NE.0) GO TO 430
                  LREC = IREC
                  CALL ZFIO ('READ', ICLUN, IFIND, LREC, IBLK, IERR)
                  IF (IERR.NE.0) GO TO 430
                  END IF
               IREC = MOD (I, NLPR) * NWPL + 1
               IF (IBLK(IREC).GT.0) THEN
                  CALL ZPHFIL ('SG', IVOL, NLUSER, I, PFILE, IERR)
                  CALL ZDESTR (IVOL, PFILE, IERR)
                  CALL H2CHR (16, 1, IBLK(IREC+3), ANAME)
                  IF (IERR.LT.2) WRITE (MSGTXT,1310) NLUSER, ANAME
                  IF (IERR.GE.2) WRITE (MSGTXT,1311) NLUSER, ANAME
                  CALL MSGWRT (2)
                  IF (IERR.LT.2) THEN
                     IBVOL = IBVOL + 1
                     IBLK(IREC) = 0
                  ELSE
                     IEVOL = IEVOL + 1
                     END IF
                  END IF
 420           CONTINUE
            CALL ZFIO ('WRIT', ICLUN, IFIND, LREC, IBLK, IERR)
            END IF
 430     CALL ZCLOSE (ICLUN, IFIND, IERR)
C                                       Kill directory
         IF (IEVOL.LE.0) THEN
            CALL ZPHFIL ('SG', IVOL, NLUSER, 0, PFILE, IERR)
            CALL ZDESTR (IVOL, PFILE, IERR)
            IF (IERR.LT.2) WRITE (MSGTXT,1330) NLUSER
            IF (IERR.GE.2) WRITE (MSGTXT,1331) NLUSER
            IF (IERR.NE.1) CALL MSGWRT (2)
            END IF
         END IF
C                                       Print summary also
      WRITE (MSGTXT,1340) IBVOL, NLUSER
      CALL MSGWRT (2)
      IF (IEVOL.GT.0) THEN
         WRITE (MSGTXT,1341) IEVOL, NLUSER
         CALL MSGWRT (2)
         END IF
C                                       Destroy Task-save file
C                                       Ask permision.
      WRITE (MSGTXT,1450) NLUSER
      CALL MSGWRT (4)
      POTERR = 39
      IF (CONF.GE.-1) THEN
         CALL CONFRM (IERR)
         IF (IERR.EQ.1) GO TO 995
         IF (IERR.GT.1) GO TO 980
         END IF
C                                       Do it
      CALL ZPHFIL ('TS', IVOL, NLUSER, 0, PFILE, IERR)
      CALL ZDESTR (IVOL, PFILE, IERR)
      IF (IERR.LT.2) WRITE (MSGTXT,1350) 'TS', NLUSER
      IF (IERR.GE.2) WRITE (MSGTXT,1351) 'TS', NLUSER
      IF (IERR.NE.1) CALL MSGWRT (2)
      CALL ZPHFIL ('TG', IVOL, 400, 0, PFILE, IERR)
      CALL ZDESTR (IVOL, PFILE, IERR)
      IF (IERR.LT.2) WRITE (MSGTXT,1350) 'old TG', NLUSER
      IF (IERR.GE.2) WRITE (MSGTXT,1351) 'old TG', NLUSER
      IF (IERR.NE.1) CALL MSGWRT (2)
      CALL ZPHFIL ('TG', IVOL, 800, 0, PFILE, IERR)
      CALL ZDESTR (IVOL, PFILE, IERR)
      IF (IERR.LT.2) WRITE (MSGTXT,1350) 'new TG', NLUSER
      IF (IERR.GE.2) WRITE (MSGTXT,1351) 'new TG', NLUSER
      IF (IERR.NE.1) CALL MSGWRT (2)
      GO TO 995
C-----------------------------------------------------------------------
C                                       SCRDEST
C-----------------------------------------------------------------------
 500  CALL DESCR (ISCR)
      GO TO 995
C-----------------------------------------------------------------------
C                                       M2CAT, U2CAT
C                                       quick listing maps, UV disk 2
C-----------------------------------------------------------------------
 600  MSGTXT = '**** Do a COMPRESS to update your vocabulary ****'
      CALL MSGWRT (3)
      QUICK = 1
      CALL ADVERB ('DOALPHA', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF (RDUM.GT.0.0) QUICK = 2 * QUICK
      I = 0
      IF (BRANCH.EQ.6) XXTYPE = 'MA'
      IF (BRANCH.EQ.7) XXTYPE = 'UV'
      LOCNAM = ' '
      LOCCLS = ' '
      LOCSEQ = 0
      CALL CATLST (IVOL, LOCNAM, LOCCLS, LOCSEQ, XXTYPE, IUSER, QUICK,
     *   I, IBLK, ISCR, IERR)
      GO TO 999
C-----------------------------------------------------------------------
C                                       GETVERS, QGETVERS
C                                       get extension file version max
C                                       DAYNUMBR
C                                       get day number of data file
C-----------------------------------------------------------------------
 800  IMAX = 0
      J1 = 1
      CALL ADVERB ('INNAME', 'C', 1, 12, IDUM, RDUM, LOCNAM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INCLASS', 'C', 1, 6, IDUM, RDUM, LOCCLS)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INSEQ', 'I', 1, 0, LOCSEQ, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INEXT', 'C', 1, 2, IDUM, RDUM, XXTYPE)
      IF (ERRNUM.NE.0) GO TO 980
      ICNO = 1
      LOCTYP = ' '
      CALL CATDIR ('SRNH', IVOL, ICNO, LOCNAM, LOCCLS, LOCSEQ, LOCTYP,
     *    IUSER, STAT, IBLK, I)
C                                       one found: read catlg block
      IF (I.EQ.0) CALL CATIO ('READ', IVOL, ICNO, CATBLK, 'REST', IBLK,
     *   IERR)
C                                       DAYNUMBR
      IF (BRANCH.EQ.10) THEN
         IF ((I.EQ.0) .AND. ((IERR.LE.0) .OR. (IERR.GT.5))) THEN
            CALL H2CHR (8, 1, CATH(KHDOB), OBSDAT)
            CALL DATEST (OBSDAT, IDATE)
            CALL DAYNUM (IDATE(1), IDATE(3), IDATE(2), DAYN)
            WRITE (MSGTXT,1900) OBSDAT, DAYN
            CALL MSGWRT (5)
            POTERR = 0
            END IF
         GO TO 980
         END IF

      IF ((I.EQ.0) .AND. ((IERR.LE.0) .OR. (IERR.GT.5)) .AND.
     *   (XXTYPE.NE.' ')) THEN
         CALL FNDEXT (XXTYPE, CATBLK, IMAX)
         IF (IMAX.GT.0) J1 = -1
         END IF
      CALL ADVRBS ('MAXVERS', 'I', 1, 1, IMAX, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVRBS ('ERROR', 'I', 1, 1, J1, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
C                                       be noisy
      IF (BRANCH.EQ.8) THEN
         IF (J1.GT.0) THEN
            WRITE (MSGTXT,1800) LOCNAM, LOCCLS, LOCSEQ, XXTYPE
            CALL MSGWRT (8)
         ELSE
            WRITE (MSGTXT,1801) LOCNAM, LOCCLS, LOCSEQ, XXTYPE, IMAX
            CALL MSGWRT (3)
            END IF
         END IF
      GO TO 999
C-----------------------------------------------------------------------
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.GT.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 995  CALL COPY (280, SASSGN, DASSGN)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CONFIRMATION ALLOWED ONLY IN INTERACTIVE MODE')
 1200 FORMAT ('Limited to ',A2,' files matching name parms ',A12,
     *   '.',A6,'.',I4)
 1205 FORMAT ('Limited to files last used >',5(F5.1,','))
 1206 FORMAT (10(F5.1,','))
 1207 FORMAT ('Destroy all files for user',I5,' disks',I3,'-',I2,
     *   '?   enter YES or NO')
 1208 FORMAT ('Destroy all files last used too many days ago?',
     *   '   Enter YES or NO')
 1230 FORMAT (A2,' FILE ',A12,'.',A6,'.',I4,' DISK',I2,' USER',I5,
     *   ' MARKED BUSY')
 1235 FORMAT ('Destroy ',A2,' file ',A12,'.',A6,'.',I4,' disk',
     *   I2,' user',I5,' Y or N')
 1237 FORMAT ('Destroying ',A2,' file ',A12,'.',A6,'.',I4,' disk',
     *   I3,' user',I5)
 1238 FORMAT ('FAILED TO DESTROY ',A2,' EXT. FILE VERSION',I7)
 1240 FORMAT ('Destroyed',I6,' extension files of type ',A2)
 1250 FORMAT ('FAILED TO DESTROY MAIN FILE, ERROR',I5)
 1260 FORMAT ('FAILED TO DESTROY HEADER FILE, ERROR',I5)
 1280 FORMAT ('Destroyed catalog file for user',I5,' disk',I3)
 1310 FORMAT ('Destroyed user',I5,' SAVE/GET file ',A16)
 1311 FORMAT ('FAILED TO DESTROY USER',I5,' SAVE/GET FILE ',A16)
 1330 FORMAT ('Destroyed user',I5,' SAVE/GET directory too')
 1331 FORMAT ('FAILED TO DESTROY USER',I5,' SAVE/GET DIRECTORY')
 1340 FORMAT ('Destroyed',I3,' SAVE/GET files for user',I5)
 1341 FORMAT ('FAILED TO DESTROY',I3,' SAVE/GET FILES FOR USER',I5)
 1350 FORMAT ('Destroyed task adverb ',A,' save file for user',I5)
 1351 FORMAT ('FAILED TO DESTROY TASK ADVERB ',A,' SAVE FILE FOR USER',
     *   I5)
 1370 FORMAT ('Cleared',I7,' messages from user',I5)
 1371 FORMAT ('Destroyed message file for user',I5)
 1372 FORMAT ('FAILED TO DESTROY MESSAGE FILE FOR USER',I5)
 1400 FORMAT ('Destroy all SAVE files for user',I5,'?  enter YES or NO')
 1450 FORMAT ('Destroy TPUT/TGET files for user',I5,
     *   '?  enter YES or NO')
 1800 FORMAT (A12,'.',A6,'.',I6,2X,A2,'  NOT FOUND')
 1801 FORMAT (A12,'.',A6,'.',I6,2X,A2,'  maxvers=',I6)
 1900 FORMAT ('Date ',A,'  has day number',I4)
      END
