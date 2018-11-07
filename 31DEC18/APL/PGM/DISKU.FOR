      PROGRAM DISKU
C-----------------------------------------------------------------------
C! Reports the AIPS disk space utilization.
C# Service System Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999, 2001, 2011, 2014, 2017
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
C   DISKU reports the AIPS disk space utilization by file type and
C   user number.  It has been made a task since it was too slow as a
C   verb.  On the MODCOMP this task will need to be privileged.
C   Input adverbs:
C      USERID   R  User no. 0 -> LOGIN, 32000 => all.
C      INDISK   R  Disk, 0 = all.
C      XSORT    R  sort type: ' '=none, 'U'=user ID, 'B'=no of blocks
C                  used.
C      DOALL    R  > 0. => print each file type, name, size
C      DETIME   R  list only files older than DETIME days
C-----------------------------------------------------------------------
      INTEGER   MAXDSK
      PARAMETER (MAXDSK=35)
C
      CHARACTER PRGNAM*6, PFILE*48, PTYPE*2, IT*2, TITL(2)*4, SORT*1,
     *   PREFIX(4)*1, NAME*12, CLASS*6, LPNAME*48, TITL1*132, TITL2*132,
     *   LINE*132, SCRTCH*132
      REAL      INDSK, USERID, DOALL, DETIME, DOCRT
      HOLLERITH XSORT, HBLK(256), XLPNAM(12)
      DOUBLE PRECISION    JD, JD0, MSIZE, ESIZE, XFACT, BLOCKS(4,500),
     *   DBLKS(4,MAXDSK), VAL0, VAL1, MBLOCK(4), MBLKS, DPBLKS
      INTEGER   ICOUNT(4,500), IUSERS(500), IPERM(500), IN, IBLK(256),
     *   IDOUNT(4,MAXDSK), ISCR(256), NWPL, NLPR, IVOL, LVOL, MS, ES,
     *   IBVOL, IEVOL, ICNO, IUSER, INUSER, XLUSER, XLUSE1, XLUSE2,
     *   ICUR, NPTY, IMAX, IFIND, IERR, ICLUN, NNAME, JT, IVER, IMAXV,
     *   INO, IRETCD, IROUND, ISORT, ILAST, IPLUS, I, J, I0, J0, ILUSER,
     *   ITEMP, ITIME(6), HLUN, HIND, IER, IREC, IHREC, BLKS, IP,
     *   DROUND, NPARMS, LUNP, FINDP, NACROS, PAGE, IPCNT, PAGMAX,
     *   IBLKCS(4)
      LOGICAL   QUICK, T, F, PRIVAT, ISSC, FIRST, LPOPN
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (IBLK, HBLK)
      COMMON /INPARM/ USERID, INDSK, XSORT, DOALL, DETIME, DOCRT,
     *   XLPNAM
      DATA T, F /.TRUE.,.FALSE./
      DATA PRGNAM /'DISKU '/
      DATA NNAME, NPTY, ICLUN /5, 19, 15/
      DATA HLUN /16/
      DATA TITL /'User', 'Disk'/
      DATA PREFIX /'k', 'M', 'G', 'T'/
C-----------------------------------------------------------------------
C                                       AIPS inits
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      LPOPN = .FALSE.
      NPARMS = 18
      CALL GTPARM (PRGNAM, NPARMS, QUICK, USERID, ISCR, IERR)
      IRETCD = 0
      IF (IERR.NE.0) IRETCD = 8
      IF ((IERR.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   THEN
         IF (DOCRT.NE.0.0) DOCRT = MIN (-1.0, DOCRT)
         END IF
      QUICK = (QUICK) .AND. (DOCRT.LE.0.0)
      IF (QUICK) CALL RELPOP (IRETCD, ISCR, IERR)
      IF (IRETCD.NE.0) GO TO 990
C                                       Sort order as char.
      CALL H2CHR (1, 1, XSORT, SORT)
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
C                                       structure of catlg records
C                                       IWPC (N20) = 5
      NWPL = 10
      NLPR = 256 / NWPL
C                                       Determine defaults for disk.
      IVOL = IROUND (INDSK)
      IBVOL = 1
      IEVOL = NVOL
C                                       Only one disk.
      IF (IVOL.NE.0) THEN
         IBVOL = IVOL
         IEVOL = IVOL
         END IF
C-----------------------------------------------------------------------
C                                       DISKUSE
C                                       Report disk space, # files
C-----------------------------------------------------------------------
C                                       Open "line printer"
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      IF (DOCRT.NE.0.0) THEN
         CALL LPOPEN (LPNAME, DOCRT, LUNP, FINDP, NACROS, ISCR, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'FAILED TO OPEN PRINTER'
            GO TO 980
            END IF
         LPOPN = .TRUE.
         TITL1 = ' '
         TITL2 = ' '
         IF (DOCRT.GT.0.0) THEN
            PAGMAX = ABS(CRTMAX) - 7
         ELSE
            PAGMAX = PRTMAX - 6
            END IF
      ELSE
         LPOPN = .FALSE.
         NACROS = 64
         END IF
      PAGE = 0
      IPCNT = 998
C                                       user range, time
      IUSER = ABS(USERID) + .5
      IF (IUSER.LE.0) IUSER = NLUSER
      INUSER = 0
      IUSERS(1) = -99
      CALL ZDATE (ITIME(1))
      CALL ZTIME (ITIME(4))
      CALL DAT2JD (ITIME, JD0)
C                                       Private/public catlgs stuff
      XLUSER = NLUSER
      XLUSE1 = XLUSER
      XLUSE2 = XLUSER
      PRIVAT = .FALSE.
      IF ((UCTSIZ.GT.0) .AND. (IUSER.NE.NLUSER)) THEN
         XLUSE1 = IUSER
         XLUSE2 = IUSER
         IF (IUSER.EQ.32000) THEN
            XLUSE1 = 1
            XLUSE2 = USELIM
            PRIVAT = .TRUE.
            CALL FILL (280, 0, DASSGN)
            END IF
         END IF
C                                       Loop for all volumes.
      DO 160 IVOL = IBVOL,IEVOL
C                                       Init volume totals.
         DBLKS(1,IVOL) = 0
         DBLKS(2,IVOL) = 0
         DBLKS(3,IVOL) = 0
         DBLKS(4,IVOL) = 0
         CALL FILL (4, 0, IDOUNT(1,IVOL))
         DO 160 ILUSER = XLUSE1,XLUSE2
C                                       Open proper catalog file.
            NLUSER = ILUSER
            IF (PRIVAT) MSGSUP = 32000
            LVOL = -IVOL
            CALL CATOPN (LVOL, IFIND, IBLK, IMAX, IERR)
            MSGSUP = 0
            NLUSER = XLUSER
            IF (IERR.NE.0) GO TO 160
C                                       Loop through all cat entries.
            ICUR = 999
            IREC = 2
            FIRST = .TRUE.
            DO 140 ICNO = 1,IMAX
               ICUR = ICUR + NWPL
C                                       Read new buffer.
               IF (ICUR.GE.NLPR*NWPL) THEN
                  CALL ZFIO ('READ', ICLUN, IFIND, IREC, IBLK, IERR)
                  IF (IERR.NE.0) GO TO 145
                  IREC = IREC + 1
                  ICUR = 1
                  END IF
C                                       See if valid entry
               IF (IBLK(ICUR).LE.0) GO TO 140
               IF ((IBLK(ICUR).NE.IUSER) .AND. (IUSER.NE.32000))
     *            GO TO 140
C                                       Check time
               CALL CATIME (2, IBLK(ICUR+2), ITIME)
               CALL DAT2JD (ITIME, JD)
               CALL H2CHR (2, NPTY, HBLK(ICUR+NNAME), PTYPE)
               ISSC = PTYPE.EQ.'SC'
               IF ((.NOT.ISSC) .AND. (JD0-JD.LT.DETIME)) GO TO 140
               IF ((ISSC) .AND. (JD0-JD.LT.3.0) .AND.
     *            (JD0-JD.LT.DETIME)) GO TO 140
C                                       Search user list
               DO 115 I = 1,INUSER
                  IF (IBLK(ICUR).EQ.IUSERS(I)) GO TO 120
 115              CONTINUE
C                                       New user ID, init.
               INUSER = INUSER + 1
               I = INUSER
               CALL FILL (4, 0, ICOUNT(1,I))
               BLOCKS(1,I) = 0.0D0
               BLOCKS(2,I) = 0.0D0
               BLOCKS(3,I) = 0.0D0
               BLOCKS(4,I) = 0.0D0
               IUSERS(I) = IBLK(ICUR)
               IPERM(INUSER) = INUSER
C                                       Get header
 120           IHREC = 1
               NLUSER = ILUSER
               CALL ZPHFIL ('CB', IVOL, ICNO, 1, PFILE, IERR)
               NLUSER = XLUSER
               CALL ZOPEN (HLUN, HIND, IVOL, PFILE, F, F, T, IERR)
               IF (IERR.NE.0) GO TO 140
               CALL ZFIO ('READ', HLUN, HIND, IHREC, CATBLK, IERR)
               CALL ZCLOSE (HLUN, HIND, IER)
               IF (IERR.NE.0) GO TO 140
               MSIZE = 0.0D0
               ESIZE = 0.0D0
C                                       header file size
               NLUSER = ILUSER
               CALL ZEXIST (IVOL, PFILE, BLKS, IERR)
               NLUSER = XLUSER
               DPBLKS = BLKS
               ESIZE = ESIZE + DPBLKS
               JT = 3
               DBLKS(JT,IVOL) = DBLKS(JT,IVOL) + DPBLKS
               BLOCKS(JT,I) = BLOCKS(JT,I) + DPBLKS
               ICOUNT(JT,I) = ICOUNT(JT,I) + 1
               IDOUNT(JT,IVOL) = IDOUNT(JT,IVOL) + 1
C                                       Main file size
               NLUSER = ILUSER
               CALL ZPHFIL (PTYPE, IVOL, ICNO, 1, PFILE, IERR)
               CALL ZEXIST (IVOL, PFILE, BLKS, IERR)
               NLUSER = XLUSER
               DPBLKS = BLKS
               IVER = 1
               IF (IERR.NE.0) IVER = 0
               MSIZE = DPBLKS
C                                       Update values
               JT = 4
               IF (PTYPE.EQ.'UV') JT = 2
               IF (PTYPE.EQ.'MA') JT = 1
               ICOUNT(JT,I) = ICOUNT(JT,I) + IVER
               IDOUNT(JT,IVOL) = IDOUNT(JT,IVOL) + IVER
               BLOCKS(JT,I) = BLOCKS(JT,I) + DPBLKS
               DBLKS(JT,IVOL) = DBLKS(JT,IVOL) + DPBLKS
C                                       Count extension files.
               JT = 3
               CALL FXHDEX (CATBLK)
               DO 135 J = 1,KIEXTN
                  IF (CATBLK(KIVER+J-1).GT.0) THEN
                     IMAXV = MIN (46655, CATBLK(KIVER+J-1))
                     CALL H2CHR (2, 1, CATH(KHEXT+J-1), IT)
C                                       Try all version nos.
                     INO = 0
                     NLUSER = ILUSER
                     DO 130 IVER = 1,IMAXV
                        CALL ZPHFIL (IT, IVOL, ICNO, IVER, PFILE,
     *                     IERR)
                        CALL ZEXIST (IVOL, PFILE, BLKS, IERR)
                        IF (IERR.EQ.0) THEN
                           DPBLKS = BLKS
                           INO = INO + 1
                           ESIZE = ESIZE + DPBLKS
                           DBLKS(JT,IVOL) = DBLKS(JT,IVOL) + DPBLKS
                           BLOCKS(JT,I) = BLOCKS(JT,I) + DPBLKS
                           END IF
 130                    CONTINUE
                     NLUSER = XLUSER
                     ICOUNT(JT,I) = ICOUNT(JT,I) + INO
                     IDOUNT(JT,IVOL) = IDOUNT(JT,IVOL) + INO
                     END IF
 135              CONTINUE
               ESIZE = ESIZE / 1024.0D0
               MSIZE = MSIZE / 1024.0D0
               IF ((DOALL.GT.0.0) .AND. (DOALL.LT.ESIZE+MSIZE)) THEN
                  IF (FIRST) THEN
                     FIRST = .FALSE.
                     IF (NACROS.LT.80) THEN
                        WRITE (TITL1,1135) DOALL, ILUSER, IVOL
                        WRITE (TITL2,1136)
                     ELSE IF (NACROS.LT.100) THEN
                        WRITE (TITL1,1135) DOALL, ILUSER, IVOL
                        WRITE (TITL2,2136)
                     ELSE
                        WRITE (TITL1,1135) DOALL, ILUSER, IVOL
                        WRITE (TITL2,3136)
                        END IF
                     IF (DOCRT.EQ.0.0) THEN
                        MSGTXT = TITL1
                        CALL MSGWRT (3)
                        MSGTXT = TITL2
                        CALL MSGWRT (3)
                     ELSE IF ((PAGE.NE.0) .AND. (IPCNT.LT.PAGMAX)) THEN
                        LINE = ' '
                        CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                     TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                        IF (IERR.NE.0) GO TO 960
                        LINE = TITL1
                        CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                     TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                        IF (IERR.NE.0) GO TO 960
                        LINE = TITL2
                        CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                     TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                        IF (IERR.NE.0) GO TO 960
                        LINE = ' '
                        CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                     TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                        IF (IERR.NE.0) GO TO 960
                     ELSE
                        IPCNT = PAGMAX + 10
                        LINE = ' '
                        CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                     TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                        IF (IERR.NE.0) GO TO 960
                        END IF
                     END IF
                  CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAME)
                  CALL H2CHR (6, KHIMCO, CATH(KHIMC), CLASS)
                  ES = DROUND (ESIZE)
                  MS = DROUND (MSIZE)
                  IF (NACROS.LT.80) THEN
                     WRITE (LINE,1137) ICNO, PTYPE, NAME, CLASS,
     *                  CATBLK(KIIMS), MS, ES, ITIME(1), ITIME(2),
     *                  ITIME(3)
                  ELSE IF (NACROS.LT.100) THEN
                     WRITE (LINE,2137) ICNO, PTYPE, NAME, CLASS,
     *                  CATBLK(KIIMS), MSIZE, ESIZE, ITIME(1), ITIME(2),
     *                  ITIME(3)
                  ELSE
                     WRITE (LINE,3137) ICNO, PTYPE, NAME, CLASS,
     *                  CATBLK(KIIMS), MSIZE, ESIZE, ITIME
                     END IF
                  IF (DOCRT.EQ.0.0) THEN
                     MSGTXT = LINE
                     CALL MSGWRT (3)
                  ELSE
                     CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *                  TITL2, LINE, IPCNT, PAGE, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 960
                     END IF
                  END IF
 140           CONTINUE
C                                       Close catalog file.
 145        CALL ZCLOSE (ICLUN, IFIND, IERR)
C                                       Include private CA files
            IF (UCTSIZ.GT.0) THEN
               NLUSER = ILUSER
               CALL ZPHFIL ('CA', IVOL, 0, 0, PFILE, IERR)
               CALL ZEXIST (IVOL, PFILE, BLKS, IERR)
               NLUSER = XLUSER
               IF (IERR.NE.0) GO TO 160
               DPBLKS = BLKS
C                                       Search user list
               DO 150 I = 1,INUSER
                  IF (ILUSER.EQ.IUSERS(I)) GO TO 155
 150              CONTINUE
C                                       New user ID, init.
               INUSER = INUSER + 1
               I = INUSER
               CALL FILL (4, 0, ICOUNT(1,I))
               BLOCKS(1,I) = 0.0D0
               BLOCKS(2,I) = 0.0D0
               BLOCKS(3,I) = 0.0D0
               BLOCKS(4,I) = 0.0D0
               IUSERS(I) = ILUSER
               IPERM(INUSER) = INUSER
C                                       store it
 155           JT = 4
               DBLKS(JT,IVOL) = DBLKS(JT,IVOL) + DPBLKS
               BLOCKS(JT,I) = BLOCKS(JT,I) + DPBLKS
               ICOUNT(JT,I) = ICOUNT(JT,I) + 1
               IDOUNT(JT,IVOL) = IDOUNT(JT,IVOL) + 1
               END IF
 160     CONTINUE
C                                       Find save files.
      IF (IBVOL.EQ.1) THEN
         JT = 4
         IVOL = 1
         XLUSE1 = 1
         XLUSE2 = USELIM
         IF ((IUSER.GE.1) .AND. (IUSER.LE.USELIM)) THEN
            XLUSE1 = IUSER
            XLUSE2 = IUSER
            END IF
         DO 200 ILUSER = XLUSE1,XLUSE2
C                                       Message file
            NLUSER = ILUSER
            CALL ZPHFIL ('MS', IVOL, ILUSER, 0, PFILE, IERR)
            CALL ZEXIST (IVOL, PFILE, BLKS, IERR)
            NLUSER = XLUSER
            IF (IERR.NE.0) GO TO 168
            DPBLKS = BLKS
C                                       Search user list
            DO 166 I = 1,INUSER
               IF (ILUSER.EQ.IUSERS(I)) GO TO 167
 166           CONTINUE
C                                       New user ID, init.
            INUSER = INUSER + 1
            I = INUSER
            CALL FILL (4, 0, ICOUNT(1,I))
            BLOCKS(1,I) = 0.0D0
            BLOCKS(2,I) = 0.0D0
            BLOCKS(3,I) = 0.0D0
            BLOCKS(4,I) = 0.0D0
            IUSERS(I) = ILUSER
            IPERM(INUSER) = INUSER
C                                       store it
 167        ICOUNT(JT,I) = ICOUNT(JT,I) + 1
            IDOUNT(JT,1) = IDOUNT(JT,1) + 1
            BLOCKS(JT,I) = BLOCKS(JT,I) + DPBLKS
            DBLKS(JT,1) = DBLKS(JT,1) + DPBLKS
C                                       Task save file
 168        DO 178 J = 1,36
               J0 = J - 1
               NLUSER = ILUSER
               CALL ZPHFIL ('TS', IVOL, 400, J0, PFILE, IERR)
               CALL ZEXIST (IVOL, PFILE, BLKS, IERR)
               NLUSER = XLUSER
               DPBLKS = BLKS
C                                       Search user list
               IF (IERR.EQ.0) THEN
                  DO 170 I = 1,INUSER
                     IF (ILUSER.EQ.IUSERS(I)) GO TO 175
 170                 CONTINUE
C                                       New user ID, init.
                  INUSER = INUSER + 1
                  I = INUSER
                  CALL FILL (4, 0, ICOUNT(1,I))
                  BLOCKS(1,I) = 0.0D0
                  BLOCKS(2,I) = 0.0D0
                  BLOCKS(3,I) = 0.0D0
                  BLOCKS(4,I) = 0.0D0
                  IUSERS(I) = ILUSER
                  IPERM(INUSER) = INUSER
C                                       store it
 175              ICOUNT(JT,I) = ICOUNT(JT,I) + 1
                  IDOUNT(JT,1) = IDOUNT(JT,1) + 1
                  BLOCKS(JT,I) = BLOCKS(JT,I) + DPBLKS
                  DBLKS(JT,1) = DBLKS(JT,1) + DPBLKS
                  END IF
 178           CONTINUE
C                                       Get # by brute force
C                                       tasks don't know ME size
            DO 195 J = 1,100
               J0 = J - 1
               NLUSER = ILUSER
               CALL ZPHFIL ('SG', IVOL, ILUSER, J0, PFILE, IERR)
               CALL ZEXIST (IVOL, PFILE, BLKS, IERR)
               NLUSER = XLUSER
               IF ((J.EQ.1) .AND. (IERR.NE.0)) GO TO 200
               IF (IERR.NE.0) GO TO 195
               DPBLKS = BLKS
C                                       Search user list
               IF (J.LE.1) THEN
                  DO 185 I = 1,INUSER
                     IF (ILUSER.EQ.IUSERS(I)) GO TO 190
 185                 CONTINUE
C                                       New user ID, init.
                  INUSER = INUSER + 1
                  I = INUSER
                  CALL FILL (4, 0, ICOUNT(1,I))
                  BLOCKS(1,I) = 0.0D0
                  BLOCKS(2,I) = 0.0D0
                  BLOCKS(3,I) = 0.0D0
                  BLOCKS(4,I) = 0.0D0
                  IUSERS(I) = ILUSER
                  IPERM(INUSER) = INUSER
                  END IF
C                                       store it
 190           ICOUNT(JT,I) = ICOUNT(JT,I) + 1
               IDOUNT(JT,1) = IDOUNT(JT,1) + 1
               BLOCKS(JT,I) = BLOCKS(JT,I) + DPBLKS
               DBLKS(JT,1) = DBLKS(JT,1) + DPBLKS
 195           CONTINUE
 200        CONTINUE
         END IF
C                                       Determine sort type.
      ISORT = 3
      IF (SORT.EQ.'U') ISORT = 1
      IF (SORT.EQ.'B') ISORT = 2
C                                       Sort.
      IF (ISORT.NE.0) THEN
         ILAST = INUSER - 1
         DO 210 I = 1,ILAST
            IPLUS = I + 1
            DO 205 J = IPLUS,INUSER
               I0 = IPERM(I)
               J0 = IPERM(J)
C                                       Sort by user ID.
               IF (ISORT.EQ.1) THEN
                  VAL0 = IUSERS(I0)
                  VAL1 = IUSERS(J0)
C                                       Sort by blocks used.
               ELSE IF (ISORT.EQ.2) THEN
                  VAL0 = BLOCKS(1,J0) + BLOCKS(2,J0) + BLOCKS(3,J0)
     *               + BLOCKS(4,J0)
                  VAL1 = BLOCKS(1,I0) + BLOCKS(2,I0) + BLOCKS(3,I0)
     *               + BLOCKS(4,I0)
C                                       sort by number of files
               ELSE
                  VAL0 = ICOUNT(1,J0) + ICOUNT(2,J0) + ICOUNT(3,J0)
     *               + ICOUNT(4,J0)
                  VAL1 = ICOUNT(1,I0) + ICOUNT(2,I0) + ICOUNT(3,I0)
     *               + ICOUNT(4,I0)
                  END IF
C                                       Out of order switch.
               IF (VAL0.GT.VAL1) THEN
                  ITEMP = IPERM(I)
                  IPERM(I) = IPERM(J)
                  IPERM(J) = ITEMP
                  END IF
 205           CONTINUE
 210        CONTINUE
         END IF
C                                       find extrema
      MBLKS = 0
      CALL FILL (4, 0, MBLOCK)
      DO 220 I = 1,INUSER
         IN = IPERM(I)
         DPBLKS = BLOCKS(1,IN) + BLOCKS(2,IN) + BLOCKS(3,IN) +
     *      BLOCKS(4,IN)
         MBLKS = MAX (MBLKS, DPBLKS)
         DO 215 J = 1,4
            MBLOCK(J) = MAX (MBLOCK(J), BLOCKS(J,IN))
 215        CONTINUE
 220     CONTINUE
      DO 230 IVOL = IBVOL,IEVOL
         DPBLKS = DBLKS(1,IVOL) + DBLKS(2,IVOL) + DBLKS(3,IVOL) +
     *      DBLKS(4,IVOL)
         MBLKS = MAX (MBLKS, DPBLKS)
         DO 225 J = 1,4
            MBLOCK(J) = MAX (MBLOCK(J), DBLKS(J,IIVOL))
 225        CONTINUE
 230     CONTINUE
      XFACT = 1.0D0
      IP = 1
 235  IF ((MBLKS.GT.999999999) .OR. (MBLOCK(1).GT.9999999) .OR.
     *   (MBLOCK(2).GT.9999999) .OR. (MBLOCK(3).GT.999999) .OR.
     *   (MBLOCK(4).GT.999999)) THEN
         XFACT = XFACT * 1024.0D0
         IP = IP + 1
         MBLKS = MBLKS / 1024.0D0
         MBLOCK(1) = MBLOCK(1)/1024.0D0
         MBLOCK(2) = MBLOCK(2)/1024.0D0
         MBLOCK(3) = MBLOCK(3)/1024.0D0
         MBLOCK(4) = MBLOCK(4)/1024.0D0
         GO TO 235
         END IF
C                                       Print messages
      IF (NACROS.LT.80) THEN
         WRITE (TITL1,1230) TITL(1)
         WRITE (TITL2,1231) (PREFIX(IP), J = 1,5)
      ELSE
         WRITE (TITL1,2230) TITL(1)
         WRITE (TITL2,2231) (PREFIX(IP), J = 1,5)
         END IF
      IF (DOCRT.EQ.0.0) THEN
         MSGTXT = TITL1
         CALL MSGWRT (5)
         MSGTXT = TITL2
         CALL MSGWRT (5)
      ELSE IF ((PAGE.NE.0) .AND. (IPCNT.LT.PAGMAX)) THEN
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 960
         LINE = TITL1
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 960
         LINE = TITL2
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 960
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 960
      ELSE
         IPCNT = PAGMAX + 10
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 960
         END IF
C                                       User summary
      DO 240 I = 1,INUSER
         IN = IPERM(I)
         DPBLKS = BLOCKS(1,IN) + BLOCKS(2,IN) + BLOCKS(3,IN) +
     *      BLOCKS(4,IN)
         BLKS = DROUND (DPBLKS/XFACT)
         DO 238 J = 1,4
            IBLKCS(J) = DROUND (BLOCKS(J,IN)/XFACT)
 238        CONTINUE
         IF (NACROS.LT.80) THEN
            WRITE (LINE,1232,ERR=239) IUSERS(IN), (ICOUNT(J,IN),
     *         IBLKCS(J), J = 1,4), BLKS
         ELSE
            WRITE (LINE,2232,ERR=239) IUSERS(IN), (ICOUNT(J,IN),
     *         IBLKCS(J), J = 1,4), BLKS
            END IF
 239     IF (DOCRT.EQ.0.0) THEN
            MSGTXT = LINE
            CALL MSGWRT (5)
         ELSE
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 960
            END IF
 240     CONTINUE
C                                       Disk summary.
      IF (NACROS.LT.80) THEN
         WRITE (TITL1,1230) TITL(2)
         WRITE (TITL2,1231) (PREFIX(IP), J = 1,5)
      ELSE
         WRITE (TITL1,2230) TITL(2)
         WRITE (TITL2,2231) (PREFIX(IP), J = 1,5)
         END IF
      IF (DOCRT.EQ.0.0) THEN
         MSGTXT = TITL1
         CALL MSGWRT (5)
         MSGTXT = TITL2
         CALL MSGWRT (5)
      ELSE IF ((PAGE.NE.0) .AND. (IPCNT.LT.PAGMAX)) THEN
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 960
         LINE = TITL1
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 960
         LINE = TITL2
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 960
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 960
      ELSE
         IPCNT = PAGMAX + 10
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 960
         END IF
      DO 245 IVOL = IBVOL,IEVOL
         DPBLKS = DBLKS(1,IVOL) + DBLKS(2,IVOL) + DBLKS(3,IVOL) +
     *      DBLKS(4,IVOL)
         BLKS = DROUND (DPBLKS/XFACT)
         DO 243 J = 1,4
            IBLKCS(J) = DROUND (DBLKS(J,IVOL)/XFACT)
 243        CONTINUE
         IF (NACROS.LT.80) THEN
            WRITE (LINE,1232,ERR=244) IVOL, (IDOUNT(J,IVOL),
     *         IBLKCS(J), J = 1,4), BLKS
         ELSE
            WRITE (LINE,2232,ERR=244) IVOL, (IDOUNT(J,IVOL),
     *         IBLKCS(J), J = 1,4), BLKS
            END IF
 244     IF (DOCRT.EQ.0.0) THEN
            MSGTXT = LINE
            CALL MSGWRT (5)
         ELSE
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         IPCNT, PAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 960
            END IF
 245     CONTINUE
      GO TO 990
C
 960  IF (IERR.GT.0) THEN
         MSGTXT = 'ERROR DOING I/O TO PRINT FILE'
      ELSE
         GO TO 990
         END IF
 980  CALL MSGWRT (8)
      IRETCD = IERR
C                                       End AIPS task
 990  IF (LPOPN) CALL LPCLOS (LUNP, FINDP, IPCNT, J)
      CALL DIETSK (IRETCD, QUICK, ISCR)
C
 999  STOP
C-----------------------------------------------------------------------
 1135 FORMAT ('Listing of files >',F10.3,' Mbytes for user',I6,' disk',
     *   I3)
 1136 FORMAT ('Cat # Type Name',9X,'Class   Seq # File Mb',
     *   ' Ext Mb    Last use')
 1137 FORMAT (I5,2X,A2,2X,A12,1X,A6,I7,I8,I7,I6,2('/',I2.2))
 2136 FORMAT (' Cat # Type  Name',10X,'Class    Seq #  File Mbytes',
     *   '  Ext Mbytes     Last use')
 2137 FORMAT (I6,2X,A2,3X,A12,2X,A6,I8,F13.3,F12.3,I7,2('/',I2.2))
 3136 FORMAT ('  Cat #  Type  Name',10X,'Class    Seq #   File Mbytes',
     *   '  Ext Mbytes   Last use date & time')
 3137 FORMAT (I7,3X,A2,3X,A12,2X,A6,I8,F14.3,F12.3,I7,2('/',I2.2),
     *   I4.2,2(':',I2.2))
 1230 FORMAT (A4,4X,'Images',3X,4X,'UV files',1X,2X,'Extensions',
     *   4X,'Other',3X,5X,'Total')
 1231 FORMAT (3X,'#',2(4X,'#  ',A,'bytes'),2(4X,'# ',A,'bytes'),4X,
     *   A,'bytes')
 1232 FORMAT (I4,2(I5,I8),2(I5,I7),I10)
 2230 FORMAT (1X,A4,6X,'Images',5X,6X,'UV files',3X,4X,'Extensions',
     *   5X,'Other',3X,7X,'Total')
 2231 FORMAT (4X,'#',2(6X,'#    ',A,'bytes'),6X,'#   ',A,'bytes',
     *   4X,'# ',A,'bytes',5X,A,'bytes')
 2232 FORMAT (I5,2(I7,I10),I7,I9,I5,I7,I11)
      END
