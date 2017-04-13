      SUBROUTINE AU3B (BRANCH)
C-----------------------------------------------------------------------
C! verbs to rearrange the entries in the catalog file: RECAT, RENUMBER
C# POPS-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004-2005, 2007, 2009-2010, 2012-2013
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
C   AU3B is a subroutine of the AIPS program to do the verbs:
C   BRANCH = 1  RENUMBER   Move file from 1 cat slot to another
C            2  RECAT      Compress the catalog
C            3  SYSTEM     Send a command to the operating system
C            4  VLA        return list of VLA antennas
C            5  EVLA       return list of EVLA antennas
C            4  VLBA       return list of VLBA antennas
C            4  HSA        return list of HSA antennas
C            8  DELAY      delays aips the specified number seconds
C            9  ADDDISK    add REMHOST's disks to usable set
C           10  REMDISK    drop REMHOST's disks from usable set
C           11  IM2HEAD    IMHEADER of image 2
C           12  Q2HEADER   QHEADER of image 2
C           13  IM3HEAD    IMHEADER of image 3
C           14  Q3HEADER   QHEADER of image 3
C           15  IM4HEAD    IMHEADER of image 4
C           16  Q4HEADER   QHEADER of image 4
C           17  IMOHEAD    IMHEADER of image out
C           18  QOHEADER   QHEADER of image out
C           19  IM5HEAD    IMHEADER of image 5
C           20  Q5HEADER   QHEADER of image 5
C   RENUMBER uses adverbs INNAME, INCLASS, INSEQ, INDISK, SLOT.
C   RECAT uses adverb INDISK.
C   SYSTEM uses SYSOUT and SYSCOM, sets ERROR.
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER LOCNAM*12, LOCCLS*6, LOCTYP*2, STAT*4, PRGNAM*6, CDUM*1,
     *   SYSCOM*72, SYSOUT*72, SYS2C(8)*64, COMMND*1024, VLBA(10)*8,
     *   HSA(4)*8, RHOST*24, KEYWOD(50)*8, KEYCHR*8
      INTEGER   POTERR, LOCSEQ, IVOL, LUSER, ICNO, OCNO, CLUN, IDUM, I,
     *   CIND, CMAX, BUF1(256), BUF2(256), BUF3(256), IBVOL, IEVOL,
     *   IERR, IER, NR, JTRIM, WERSOM, IIP, LSAVE, VER, IABUF(512),
     *   LUNA, J, JJ, SCAT, NUMKEY, LOCS(50), IVALUE(100), KEYTS(50)
      HOLLERITH HVAL, HNAME
      REAL      RDUM, DTIME, VALUE(100)
      LOGICAL   IAMOK, NORMAL, LVALUE(100)
      DOUBLE PRECISION BSC, BZE
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DISK.INC'
      EQUIVALENCE (IABUF, BUF2)
      EQUIVALENCE (IVALUE, LVALUE, VALUE)
      COMMON /AIPSCR/ BUF1, BUF2, BUF3
      DATA CLUN, LUNA /15,29/
      DATA PRGNAM /'AU3B  '/
      DATA VLBA /'BR','FD','HN','KP','NL','OV','PT','LA','SC','MK'/
      DATA HSA /'GB','Y','AR','EB'/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.20)) GO TO 999
      LUSER = NLUSER
      POTERR = 33
      CALL ADVERB ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
C                                       branch to header displays
      IF (BRANCH.GT.10) GO TO 700
C                                       other functions
      GO TO (100, 200, 300, 400, 400, 400, 400, 500, 600, 600), BRANCH
C-----------------------------------------------------------------------
C                                       RENUMBER
C                                       slot change
C-----------------------------------------------------------------------
C                                       find input
 100  LOCTYP = ' '
      CALL ADVERB ('INSEQ', 'I', 1, 0, LOCSEQ, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INNAME', 'C', 1, 12, IDUM, RDUM, LOCNAM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INCLASS', 'C', 1, 6, IDUM, RDUM, LOCCLS)
      IF (ERRNUM.NE.0) GO TO 980
      CALL CATDIR ('SRCH', IVOL, ICNO, LOCNAM, LOCCLS, LOCSEQ, LOCTYP,
     *   LUSER, STAT, BUF1, IERR)
      IF (IERR.EQ.0) GO TO 110
         WRITE (MSGTXT,1100) IERR
         CALL MSGWRT (6)
         GO TO 980
 110  CALL ADVERB ('SLOT', 'I', 1, 0, OCNO, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
C                                       open catalog
      CALL CATOPN (IVOL, CIND, BUF1, CMAX, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       do renumber
      CALL RENUMB (IVOL, CLUN, CIND, CMAX, ICNO, OCNO, BUF1, BUF2, BUF3,
     *   IERR)
C                                       close catalog
      CALL ZCLOSE (CLUN, CIND, IER)
      IF (IERR.NE.0) GO TO 900
      GO TO 999
C-----------------------------------------------------------------------
C                                       RECAT
C                                       compress catalog
C-----------------------------------------------------------------------
 200  NR = 0
      IF ((IVOL.LE.0) .OR. (IVOL.GT.NVOL)) THEN
         IBVOL = 1
         IEVOL = NVOL
      ELSE
         IBVOL = IVOL
         IEVOL = IVOL
         END IF
      DO 230 IVOL = IBVOL,IEVOL
         OCNO = 1
         WERSOM = 0
C                                       open catalog
         IF (.NOT.IAMOK(IVOL,'CA')) GO TO 230
         CALL CATOPN (IVOL, CIND, BUF1, CMAX, IERR)
         IF (IERR.EQ.0) GO TO 210
            WRITE (MSGTXT,1200) IVOL, IERR
            CALL MSGWRT (6)
            GO TO 230
 210     DO 220 ICNO = 1,CMAX
C                                       do renumber
            CALL RENUMB (IVOL, CLUN, CIND, CMAX, ICNO, OCNO, BUF1, BUF2,
     *         BUF3, IERR)
            IF (IERR.EQ.1) GO TO 220
               IF (IERR.EQ.0) THEN
                  IF (ICNO.NE.OCNO) THEN
                     WRITE (MSGTXT,1210) IVOL, ICNO, OCNO
                     CALL MSGWRT (2)
                     WERSOM = -1
                  ELSE
                     IF (WERSOM.GE.0) WERSOM = WERSOM + 1
                     END IF
               ELSE IF (IERR.GT.1) THEN
                  IF (IERR.EQ.2) WRITE (MSGTXT,1211) IVOL, ICNO, OCNO
                  IF (IERR.EQ.3) WRITE (MSGTXT,1212) IVOL, ICNO
                  IF (IERR.EQ.4) WRITE (MSGTXT,1213) IVOL, ICNO, OCNO
                  IF (IERR.EQ.5) WRITE (MSGTXT,1214) IVOL, ICNO, OCNO
                  IF (IERR.EQ.6) WRITE (MSGTXT,1215) IVOL, ICNO, OCNO
                  CALL MSGWRT (6)
                  GO TO 225
                  END IF
               OCNO = OCNO + 1
 220        CONTINUE
C                                       close catalog
 225     CALL ZCLOSE (CLUN, CIND, IERR)
         NR = NR + OCNO - 1
         IF (WERSOM.GT.0) THEN
            WRITE (MSGTXT,1225) IVOL
            CALL MSGWRT (2)
            END IF
 230     CONTINUE
      WRITE (MSGTXT,1230)
      IF (NR.LE.0) CALL MSGWRT (6)
      GO TO 999
C-----------------------------------------------------------------------
C                                       SYSTEM
C                                       send a command to the OS
C-----------------------------------------------------------------------
 300  CALL ADVERB ('SYSOUT', 'C', 1, 72, IDUM, RDUM, SYSOUT)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('SYSCOM', 'C', 1, 72, IDUM, RDUM, SYSCOM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('SYS2COM', 'C', 8, 64, IDUM, RDUM, SYS2C)
      IF (ERRNUM.NE.0) GO TO 980
      IBVOL = JTRIM (SYSCOM)
      COMMND = SYSCOM
      IIP = IBVOL + 1
      DO 310 I = 1,8
         IBVOL = JTRIM (SYS2C(I))
         IF (IBVOL.GT.0) THEN
            COMMND(IIP:) = SYS2C(I)
            IIP = IIP + IBVOL
            END IF
 310     CONTINUE
      I = INDEX (COMMND, ' >')
      IF (I.EQ.0) THEN
         IF (SYSOUT.EQ.' ') SYSOUT = '/dev/tty'
         IEVOL = JTRIM (SYSOUT)
         COMMND(IIP:) = ' >> ' // SYSOUT
         END IF
      IBVOL = JTRIM (COMMND)
      MSGTXT = 'SYSTEM command to be executed is:'
      CALL MSGWRT (3)
      DO 320 I = 1,16
         IEVOL = 1 + (I-1)*64
         MSGTXT = COMMND(IEVOL:IEVOL+63)
         IF (MSGTXT.NE.' ') CALL MSGWRT (3)
 320     CONTINUE
      IEVOL = 0
      CALL ZSHCMD (IBVOL, COMMND, IEVOL, SYSOUT, IERR)
      NR = -1
      IF (IERR.GT.0) NR = 1
      CALL ADVRBS ('ERROR', 'I', 1, 0, NR, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      GO TO 999
C-----------------------------------------------------------------------
C                                       VLA, EVLA, VLBA, hsa
C-----------------------------------------------------------------------
C                                       find input
 400  LOCTYP = 'UV'
      CALL ADVERB ('DOINVERS', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      NORMAL = RDUM.LE.0.0
      CALL ADVERB ('SUBARRAY', 'I', 1, 0, VER, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INSEQ', 'I', 1, 0, LOCSEQ, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INNAME', 'C', 1, 12, IDUM, RDUM, LOCNAM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INCLASS', 'C', 1, 6, IDUM, RDUM, LOCCLS)
      IF (ERRNUM.NE.0) GO TO 980
      CALL CATDIR ('SRCH', IVOL, ICNO, LOCNAM, LOCCLS, LOCSEQ, LOCTYP,
     *   LUSER, STAT, BUF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         CALL MSGWRT (6)
         GO TO 980
         END IF
      CALL CATIO ('READ', IVOL, ICNO, CATBLK, 'REST', BUF1, IERR)
      IF (IERR.NE.0) THEN
         POTERR = 33
         WRITE (MSGTXT,1400) IERR
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                        Init AN file.
      POTERR = 45
      IF (VER.LE.0) VER = 1
      CALL ANTINI ('READ', IABUF, IVOL, ICNO, VER, CATBLK, LUNA, IANRNO,
     *   ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE, POLRXY,
     *   UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB, NOPCAL,
     *   ANTNIF, ANFQID, IERR)
      IF (IERR.NE.0) GO TO 980
      NR = IABUF(5)
      LX = 0
      POTERR = 50
      DO 420 I = 1,NR
         IANRNO = I
         CALL TABAN ('READ', IABUF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA,POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) GO TO 970
         IF ((ANNAME(6:8).EQ.'OUT') .OR. (ANNAME(5:8).EQ.'OUT '))
     *      GO TO 420
         LX = LX + 1
         X(LX) = NOSTA
         IF (BRANCH.EQ.4) THEN
            IF (NORMAL) THEN
               IF (ANNAME(1:4).NE.'VLA:') LX = LX - 1
            ELSE
               IF (ANNAME(1:4).EQ.'VLA:') LX = LX - 1
               END IF
         ELSE IF (BRANCH.EQ.5) THEN
            IF (NORMAL) THEN
               IF (ANNAME(1:5).NE.'EVLA:') LX = LX - 1
            ELSE
               IF (ANNAME(1:5).EQ.'EVLA:') LX = LX - 1
               END IF
         ELSE IF (BRANCH.EQ.6) THEN
            DO 410 IIP = 1,10
               IF (ANNAME.EQ.VLBA(IIP)) THEN
                  IF (.NOT.NORMAL) LX = LX - 1
                  GO TO 420
                  END IF
 410           CONTINUE
            IF (NORMAL) LX = LX - 1
         ELSE IF (BRANCH.EQ.7) THEN
            DO 415 IIP = 1,4
               IF (ANNAME.EQ.HSA(IIP)) THEN
                  IF (.NOT.NORMAL) LX = LX - 1
                  GO TO 420
                  END IF
 415           CONTINUE
            IF (NORMAL) LX = LX - 1
            END IF
 420     CONTINUE
C                                       list onto stack
      SYTYPE = 12
      LSAVE = L
      CALL LTSTOR
      L = LSAVE
      STACK(SP+1) = 12
      STACK(SP+2) = LX
      STACK(SP+3) = TAG
      STACK(SP+4) = 2
      SP = SP + 4
      POTERR = 0
      GO TO 970
C-----------------------------------------------------------------------
C                                       DELAY
C                                       delay s seconds
C-----------------------------------------------------------------------
 500  CALL ADVERB ('DETIME', 'R', 1, 0, IDUM, DTIME, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
C                                       doit
      IF (DTIME.GT.0.0) CALL ZDELAY (DTIME, IDUM)
      POTERR = 0
      GO TO 980
C-----------------------------------------------------------------------
C                                       ADDDISK, REMDISK
C                                       add or remove disk assignements
C-----------------------------------------------------------------------
 600  IBVOL = NVOL
      CALL ADVERB ('REMHOST', 'C', 1, 24, IDUM, RDUM, RHOST)
      IF (ERRNUM.NE.0) GO TO 980
      IF (RHOST.EQ.' ') THEN
         MSGTXT = 'REMHOST MUST BE SPECIFIED TO ADD OR REM DISKS'
         CALL MSGWRT (8)
         POTERR = 24
         GO TO 980
         END IF
      J = JTRIM (RHOST)
      DO 610 I = 1,36
         IF (RHOST.EQ.DSKHST(I)) THEN
            IF (BRANCH.EQ.9) THEN
               MSGTXT = RHOST(:J) // ' ALREADY IN DISK LIST'
               CALL MSGWRT (8)
               POTERR = 14
               GO TO 980
            ELSE
               IF (I.GT.MINDSK) GO TO 620
               END IF
            END IF
 610     CONTINUE
      IF (BRANCH.EQ.10) THEN
         MSGTXT = RHOST(:J) // ' NOT ALREADY IN DISK LIST'
         CALL MSGWRT (6)
         POTERR = 0
         GO TO 980
         END IF
C                                       Do it
 620  J = BRANCH - 8
      CALL DSKASS (J, RHOST, IER)
      POTERR = 0
      IF (IER.NE.0) THEN
         POTERR = 50
      ELSE IF (NVOL.NE.IBVOL) THEN
         WRITE (MSGTXT,1620) IBVOL, NVOL
         CALL MSGWRT (2)
         WRITE (MSGTXT,1621) NVOL
         CALL CHR2H (4, 'NVOL', 1, HVAL)
         IF (NVOL.LE.9) THEN
            JJ = 1
            CALL CHR2H (JJ, MSGTXT(2:2), 1, HNAME)
         ELSE
            JJ = 2
            CALL CHR2H (JJ, MSGTXT(1:2), 1, HNAME)
            END IF
         CALL ZCRLOG (4, HVAL, JJ, HNAME, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'RESETTING LOGICAL NVOL FAILS'
            CALL MSGWRT (7)
            END IF
         END IF
      GO TO 980
C-----------------------------------------------------------------------
C                                       IMxHEAD
C                                       QxHEADER
C                                       display  header information
C-----------------------------------------------------------------------
 700  MSGTXT = 'Header for'
      IF (BRANCH.LE.12) THEN
         CALL ADVERB ('IN2NAME', 'C', 1, 12, IDUM, RDUM, LOCNAM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('IN2CLASS', 'C', 1, 6, IDUM, RDUM, LOCCLS)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('IN2SEQ', 'I', 1, 0, LOCSEQ, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('IN2DISK', 'I', 1, 0, IVOL, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         MSGTXT(12:) = 'second input image or data set'
      ELSE IF (BRANCH.LE.14) THEN
         CALL ADVERB ('IN3NAME', 'C', 1, 12, IDUM, RDUM, LOCNAM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('IN3CLASS', 'C', 1, 6, IDUM, RDUM, LOCCLS)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('IN3SEQ', 'I', 1, 0, LOCSEQ, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('IN3DISK', 'I', 1, 0, IVOL, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         MSGTXT(12:) = 'third input image or data set'
      ELSE IF (BRANCH.LE.16) THEN
         CALL ADVERB ('IN4NAME', 'C', 1, 12, IDUM, RDUM, LOCNAM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('IN4CLASS', 'C', 1, 6, IDUM, RDUM, LOCCLS)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('IN4SEQ', 'I', 1, 0, LOCSEQ, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('IN4DISK', 'I', 1, 0, IVOL, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         MSGTXT(12:) = 'fourth input image or data set'
      ELSE IF (BRANCH.LE.18) THEN
         CALL ADVERB ('OUTNAME', 'C', 1, 12, IDUM, RDUM, LOCNAM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('OUTCLASS', 'C', 1, 6, IDUM, RDUM, LOCCLS)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('OUTSEQ', 'I', 1, 0, LOCSEQ, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('OUTDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         MSGTXT(12:) = 'first output image or data set'
      ELSE IF (BRANCH.LE.20) THEN
         CALL ADVERB ('IN5NAME', 'C', 1, 12, IDUM, RDUM, LOCNAM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('IN5CLASS', 'C', 1, 6, IDUM, RDUM, LOCCLS)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('IN5SEQ', 'I', 1, 0, LOCSEQ, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('IN5DISK', 'I', 1, 0, IVOL, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         MSGTXT(12:) = 'fifth input image or data set'
         END IF
      CALL MSGWRT (2)
      LOCTYP = ' '
      LUSER = NLUSER
      SCAT = 1
      POTERR = 45
      CALL CATDIR ('SRNH', IVOL, SCAT, LOCNAM, LOCCLS, LOCSEQ,
     *   LOCTYP, LUSER, STAT, BUF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                        one found: read header
      CALL CATIO ('READ', IVOL, SCAT, CATBLK, 'REST', BUF1, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LT.5)) GO TO 980
C                                       IMxHEAD
      IF (MOD(BRANCH,2).EQ.1) THEN
C                                       read keywords on scaling
         NUMKEY = 2
         KEYWOD(1) = 'ISCALE'
         KEYWOD(2) = 'IZERO'
         CALL CATKEY ('REED', IVOL, SCAT, KEYWOD, NUMKEY, LOCS, VALUE,
     *      KEYTS, BUF1, IERR)
         BSC = 1.0D0
         BZE = 0.0D0
         IF ((IERR.LE.0) .OR. (IERR.GT.20)) THEN
            IF (LOCS(1).GT.0) CALL RCOPY (NWDPDP, VALUE(LOCS(1)), BSC)
            IF (LOCS(2).GT.0) CALL RCOPY (NWDPDP, VALUE(LOCS(2)), BZE)
            END IF
C                                       finally list the header
         CALL LSTHDR (CATBLK, CATH, CATR, CATD, BSC, BZE)
C                                       All other keywords
         NUMKEY = 50
         CALL CATKEY ('ALL ', IVOL, SCAT, KEYWOD, NUMKEY, LOCS, VALUE,
     *      KEYTS, BUF1, IERR)
         IF ((IERR.NE.0) .OR. (NUMKEY.LE.0)) GO TO 999
         DO 720 I = 1,NUMKEY
            J = LOCS(I)
            IF ((KEYWOD(I).NE.'ISCALE') .AND. (KEYWOD(I).NE.'IZERO')
     *         .AND. (J.LE.0)) THEN
               IF (KEYTS(I).EQ.1) THEN
                  CALL RCOPY (NWDPDP, VALUE(J), BSC)
                  WRITE (MSGTXT,1710) KEYWOD(I), BSC
               ELSE IF (KEYTS(I).EQ.2) THEN
                  WRITE (MSGTXT,1711) KEYWOD(I), VALUE(J)
               ELSE IF (KEYTS(I).EQ.3) THEN
                  CALL H2CHR (8, 1, VALUE(J), KEYCHR)
                  WRITE (MSGTXT,1712) KEYWOD(I), KEYCHR
               ELSE IF (KEYTS(I).EQ.4) THEN
                  WRITE (MSGTXT,1713) KEYWOD(I), IVALUE(J)
               ELSE IF (KEYTS(I).EQ.5) THEN
                  WRITE (MSGTXT,1714) KEYWOD(I), LVALUE(J)
               ELSE
                  WRITE (MSGTXT,1715) KEYWOD(I), KEYTS(I)
                  END IF
               CALL MSGWRT (2)
               END IF
 720        CONTINUE
C                                       QxHEADER
      ELSE
         CALL KWIKHD
         END IF
      POTERR = 0
      GO TO 980
C-----------------------------------------------------------------------
C                                       error messages from RENUMB
 900  IF (IERR.EQ.2) WRITE (MSGTXT,1211) IVOL, ICNO, OCNO
      IF (IERR.EQ.3) WRITE (MSGTXT,1212) IVOL, ICNO
      IF (IERR.EQ.4) WRITE (MSGTXT,1213) IVOL, ICNO, OCNO
      IF (IERR.EQ.5) WRITE (MSGTXT,1214) IVOL, ICNO, OCNO
      IF (IERR.EQ.6) WRITE (MSGTXT,1215) IVOL, ICNO, OCNO
      IF (IERR.EQ.1) WRITE (MSGTXT,1216) IVOL, ICNO
      CALL MSGWRT (6)
      GO TO 980
C                                       close an file
 970  CALL TABAN ('CLOS', IABUF, IANRNO, ANKOLS, ANNUMV, ANNAME, STAXYZ,
     *   ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN, POLTYA, POLAA,
     *   POLCA, POLTYB, POLAB, POLCB, IERR)
C                                       AIPS error management.
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('CAN''T FIND REQUESTED INPUT IMAGE.  ERROR =',I5)
 1200 FORMAT ('DISK',I3,' ERROR',I5,' OPENING THE CATALOG FILE')
 1210 FORMAT ('Disk',I3,' in-CNO',I5,' renumbered as CNO',I5)
 1211 FORMAT ('DISK',I3,' IN-CNO',I5,' OUT-CNO',I5,
     *   ' CALLING PARM ERROR')
 1212 FORMAT ('DISK',I3,' IN-CNO',I5,' TOO BUSY')
 1213 FORMAT ('DISK',I3,' IN-CNO',I5,' OUT-CNO',I5,' NOT EMPTY')
 1214 FORMAT ('DISK',I3,' IN-CNO',I5,' OUT-CNO',I5,
     *   ' CATALOG I/O ERROR')
 1215 FORMAT ('DISK',I3,' IN-CNO',I5,' OUT-CNO',I5,
     *   ' RENUMBERING PROBLEM')
 1216 FORMAT ('DISK',I3,' IN-CNO',I5,' SLOT EMPTY')
 1225 FORMAT ('Disk',I3,' No files needed renumbering')
 1230 FORMAT ('NO FILES RENUMBERED')
 1400 FORMAT ('ERROR',I5,' READING IMAGE HEADER FROM MAIN CATALOG')
 1620 FORMAT ('Number of disks changed from',I3,' to',I3)
 1621 FORMAT (I2)
 1710 FORMAT ('Keyword = ''',A8,'''  value = ',1PD15.8)
 1711 FORMAT ('Keyword = ''',A8,'''  value = ',1PE13.6)
 1712 FORMAT ('Keyword = ''',A8,'''  value = ''',A8,'''')
 1713 FORMAT ('Keyword = ''',A8,'''  value = ',I12)
 1714 FORMAT ('Keyword = ''',A8,'''  value = ',L1)
 1715 FORMAT ('KEYWORD = ''',A8,'''  INVALID KEYTYPE = ',I12)
      END
