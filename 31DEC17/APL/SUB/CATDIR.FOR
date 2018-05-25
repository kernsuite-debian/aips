      SUBROUTINE CATDIR (OP, IVOL, CNO, CNAME, CCLASS, SEQ, PTYPE, USID,
     *   STAT, BUFF, IERR)
C-----------------------------------------------------------------------
C! manipulates the catalog directory: OPEN, CLOS, various SRCHs, ...
C# Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2004
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
C   CATDIR manipulates catalog directory
C   Inputs:
C      OP     C*4   searches find entry with specified data:
C                   'SRCH' high seq # (if SEQ 0), return things
C                   'SRNH' high seq # (if SEQ 0), NOT return things
C                   'SRCN' next match, return things
C                   'SRNN' next match, NOT return things
C                   'OPEN' = create a new slot (and init header file)
C                   'CLOS' = destroy a slot
C                   'INFO' = return contents of a slot
C                   'CSTA' = modify status of a slot
C      IVOL   I     Disk volume containing catalog
C                   0 => all on searches, OPEN
C      CNO    I     Slot number to begin: SRCN, SRNN, OPEN
C                   Ignored if IVOL = 0 : searches, OPEN
C                   Slot number to examine (solely): CLOS, INFO, CSTA
C      CNAME  C*12  Map name: searches, OPEN, CLOS
C      CCLASS C*6   Map type: searches, OPEN, CLOS
C      SEQ    I     Map sequence number: searches, OPEN, CLOS
C      PTYPE  C*2   Map physical type (2 chars): searches, OPEN, CLOS
C      USID   I     User identification #: searches, OPEN, CLOS
C      STAT   C*4   Status (OP=CSTA): READ, WRIT, CLRD, or CLWR
C   Outputs:
C      CNO    I     Slot number found: searches, OPEN
C      IVOL   I     If 0 on input, value actually used: searches, OPEN
C      CNAME  C*12  Map name: SRCH, SRCN, INFO
C      CCLASS C*6   Map type: SRCH, SRCN, INFO
C      SEQ    I     Map sequence number: SRCH, SRCN, INFO
C      PTYPE  C*2   Map physical file type: SRCH, SRCN, INFO
C      USID   I     User identification #: SRCH, SRCN, INFO
C      STAT   R     Status: SRCH, SRCN, INFO
C      BUFF   I(256)    Working buffer
C      IERR   I     Error return
C                   1 =>  can't open cat file or header file
C                   2 =>  input error
C                   3 =>  can't read catalog or header file
C                   4 =>  CLOSE blocked by non-REST status
C                   5 =>  end of catalog on OPEN or SRCH i.e.
C                         no available slots or slot not found
C                   6 =>  on INFO, CSTA requested slot not occupied
C                   7 =>  can't use WRIT status because now READ
C                   8 =>  on CLOSE the ID's don't match
C                   9 =>  Warning: read status added on a file
C                         being written
C                  10 =>  Clear read/write when didn't exist warning
C-----------------------------------------------------------------------
      CHARACTER OP*4, CNAME*12, CCLASS*6, PTYPE*2, STAT*4
      INTEGER   IVOL, CNO, SEQ, USID, BUFF(256), IERR
C
      INTEGER   NREC, LREC, ISIZE, LSIZE, CLUN, CIND, IOP, CMAX, CMOD,
     *   NIND, NCLAS, NSEQ, NSTAT, LVOL, I, J, NTIME, NOP,
     *   SWRITE, CLEAR, IER, ICLOS, ITIME(6), XVOL, NWPL, NLPR,
     *   NNAME, INERR, RDERR, IDCLSR, MISSER, STCLSR, NPTYPE, HVOL,
     *   HCNO, HSEQ, NPAT(12), CPAT(6), HUSE
      LOGICAL   LR, LPVOL, LCNAME, LCLAS, LSEQ, LPTYPE, LUSID, T, F,
     *   IAMOK
      CHARACTER PHNAME*48, READ*4, WRITE*4, OPCODE(8)*4, NOTYPE*6,
     *   HNAME*12, HCLASS*6, HPTYPE*2, TNAME*12, TCLASS*6, TTYPE*2,
     *   HSTAT*4
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DDRC.INC'
      DATA NOP, OPCODE /8, 'SRCH','SRNH','SRCN','SRNN','OPEN','CLOS',
     *   'INFO','CSTA'/
      DATA READ, WRITE /'READ','WRIT'/
      DATA INERR, RDERR, IDCLSR, MISSER, STCLSR /2, 3, 8, 5, 4/
      DATA NOTYPE /'NOTYPE'/
      DATA CLUN /15/
      DATA T, F /.TRUE.,.FALSE./
      DATA ICLOS, SWRITE, CLEAR /6,-1,-1/
      DATA NCLAS, NSEQ, NSTAT, NPTYPE, NTIME, NNAME
     *   /13, 4, 1, 19, 2, 5/
C-----------------------------------------------------------------------
C                                       Check OPcode
      IERR = INERR
      DO 5 IOP = 1,NOP
         IF (OPCODE(IOP).EQ.OP) GO TO 10
 5       CONTINUE
      WRITE (MSGTXT,1005) OP
      CALL MSGWRT (6)
      GO TO 999
C                                       do we loop over volumes?
C                                       OK on SRCH,OPEN
 10   LPVOL = (IVOL.LE.0) .AND. (IOP.LT.ICLOS)
      LVOL = IVOL
      IF (LPVOL) LVOL = 1
      NWPL = 10
      NLPR = 256 / NWPL
C                                       Check for default use
C                                       on input selection
      IF (IOP.LE.ICLOS) THEN
         LCNAME = CNAME.NE.' '
         LCLAS = CCLASS.NE.' '
         LSEQ  = SEQ.GT.0
         LPTYPE = PTYPE.NE.' '
         LUSID = USID.GT.0
         END IF
C                                       init wild card, seq #
      IF (IOP.LE.2) CNO = 1
      HSEQ = 0
      IF ((IOP.LE.2) .AND. (LSEQ)) IOP = IOP + 2
      IF ((IOP.LE.4) .AND. (LCNAME)) CALL PSFORM (12, CNAME, NPAT)
      IF ((IOP.LE.4) .AND. (LCLAS)) CALL PSFORM (6, CCLASS, CPAT)
C                                       open catalog
 15   CIND = 0
      IF ((LPVOL) .AND. (.NOT.IAMOK(LVOL,'CA'))) GO TO 16
      IF ((DSKLOK(LVOL).EQ.1) .AND. (IOP.GE.5) .AND. (IOP.LE.6)) THEN
         IF (LPVOL) GO TO 16
         WRITE (MSGTXT,1015) LVOL
         CALL MSGWRT (8)
         IERR = 3
         GO TO 999
         END IF
      XVOL = LVOL
      IF (IOP.LE.4) XVOL = -LVOL
      IF (DSKLOK(LVOL).EQ.1) XVOL = -LVOL
      CALL CATOPN (XVOL, CIND, BUFF, CMAX, IER)
      IERR = IER
      IF (IER.EQ.4) IERR = 1
      IF ((IERR.EQ.0) .OR. (IERR.EQ.2) .OR. (.NOT.LPVOL)) GO TO 17
         IF (IERR.EQ.3) CALL ZCLOSE (CLUN, CIND, IER)
 16      IERR = 0
         CIND = 0
         IF (LVOL.GE.NVOL) GO TO 260
            LVOL = LVOL + 1
            GO TO 15
 17   IF (IER.EQ.3) GO TO 980
      IF (IER.NE.0) GO TO 999
C                                       Check validity of CNO
      IF (LPVOL) CNO = 1
      IF ((0.LT.CNO) .AND. (CMAX.GE.CNO)) GO TO 20
         IERR = INERR
         WRITE (MSGTXT,1017) CNO
         GO TO 970
C                                       initial disk pointers
 20   CMOD = (CNO - 1) / NLPR
      NREC = 2 + CMOD
      NIND = 1 + NWPL * (CNO - NLPR*CMOD -1)
C                                       directory rec read loop
 30   CALL ZFIO (READ, CLUN, CIND, NREC, BUFF, IER)
      IF (IER.NE.0) GO TO 960
      IERR = 0
      GO TO (100, 100, 100, 100, 200, 400, 500, 600), IOP
C                                       Search
 100     CONTINUE
C                                       compare parameters for match
            IF (BUFF(NIND).EQ.CLEAR) GO TO 250
            IF ((BUFF(NIND).NE.USID) .AND. (LUSID)) GO TO 250
            IF ((BUFF(NIND+NSEQ).NE.SEQ) .AND. (LSEQ)) GO TO 250
            CALL H2CHR (2, NPTYPE, BUFF(NIND+NNAME), TTYPE)
            LR = PTYPE.EQ.TTYPE
            IF ((LPTYPE) .AND. (.NOT.LR)) GO TO 250
            IF (.NOT.LCNAME) GO TO 110
               CALL H2CHR (12, 1, BUFF(NIND+NNAME), TNAME)
               CALL CHWMAT (12, CNAME, NPAT, 1, TNAME, LR)
               IF (.NOT.LR) GO TO 250
 110        IF (.NOT.LCLAS) GO TO 120
               CALL H2CHR (6, NCLAS, BUFF(NIND+NNAME), TCLASS)
               CALL CHWMAT (6, CCLASS, CPAT, 1, TCLASS, LR)
               IF (.NOT.LR) GO TO 250
C                                       return parms
 120        IF (IOP.LE.2) GO TO 130
               IVOL = LVOL
               IF (IOP.NE.4) THEN
                  USID = BUFF(NIND)
                  SEQ = BUFF(NIND+NSEQ)
                  CALL H2CHR (12, 1, BUFF(NIND+NNAME), CNAME)
                  CALL H2CHR (6, NCLAS, BUFF(NIND+NNAME), CCLASS)
                  CALL H2CHR (2, NPTYPE, BUFF(NIND+NNAME), PTYPE)
                  CALL STXT (BUFF(NIND+NSTAT), STAT)
                  END IF
               GO TO 980
C                                       looking for highest
 130        IF (BUFF(NIND+NSEQ).LE.HSEQ) GO TO 250
               HSEQ = BUFF(NIND+NSEQ)
               HCNO = CNO
               HVOL = LVOL
               IF (IOP.NE.2) THEN
                  HUSE = BUFF(NIND)
                  CALL H2CHR (12, 1, BUFF(NIND+NNAME), HNAME)
                  CALL H2CHR (6, NCLAS, BUFF(NIND+NNAME), HCLASS)
                  CALL H2CHR (2, NPTYPE, BUFF(NIND+NNAME), HPTYPE)
                  CALL STXT (BUFF(NIND+NSTAT), HSTAT)
                  END IF
               GO TO 250
C                                       To OPEN check for clear slot
 200     CONTINUE
            IF (BUFF(NIND).NE.CLEAR) GO TO 250
C                                       default CNAME is error
            IF (LCNAME) GO TO 210
               IERR = INERR
               WRITE (MSGTXT,1200)
               GO TO 970
 210        IF (LPTYPE) GO TO 215
               IERR = INERR
               WRITE (MSGTXT,1210)
               GO TO 970
 215        CALL CHR2H (12, CNAME, 1, BUFF(NIND+NNAME))
C                                       USID default = NLUSER
            BUFF(NIND) = NLUSER
            IF (LUSID) BUFF(NIND) = USID
C                                       CCLASS default is NOTYPE
            IF (LCLAS) THEN
               CALL CHR2H (6, CCLASS, NCLAS, BUFF(NIND+NNAME))
            ELSE
               CALL CHR2H (6, NOTYPE, NCLAS, BUFF(NIND+NNAME))
               END IF
C                                       Default SEQ = 1
 225        BUFF(NIND+NSEQ) = 1
            IF (LSEQ) BUFF(NIND+NSEQ) = SEQ
            BUFF(NIND+NSTAT) = SWRITE
            CALL CHR2H (2, PTYPE, NPTYPE, BUFF(NIND+NNAME))
            CALL CATIME (1, BUFF(NIND+NTIME), ITIME)
            IVOL = LVOL
C                                       Create header file
            CALL ZFIO (WRITE, CLUN, CIND, NREC, BUFF, IER)
            IF (IER.NE.0) GO TO 950
            CALL ZCLOSE (CLUN, CIND, IER)
            CIND = 0
            ISIZE = 2
            CALL ZPHFIL ('CB', IVOL, CNO, 1, PHNAME, IER)
            CALL ZCREAT (IVOL, PHNAME, ISIZE, F, LSIZE, IER)
            IF (IER.EQ.0) GO TO 235
               IF (IER.NE.1) GO TO 230
                  WRITE (MSGTXT,1225)
                  IERR = 0
                  GO TO 242
 230           CONTINUE
                  WRITE (MSGTXT,1230) IER
                  IERR = 1
                  GO TO 242
 235        CALL ZOPEN (CLUN, CIND, IVOL, PHNAME, F, T, T, IER)
            IF (IER.EQ.0) GO TO 240
               WRITE (MSGTXT,1235) IER
               IERR = 1
               GO TO 241
 240        NREC = 2
            CALL FILL (256, 0, BUFF)
            BUFF(1) = LSIZE
            IERR = 0
            CALL ZFIO (WRITE, CLUN, CIND, NREC, BUFF, IER)
C                                       CB file has failed! kill
            IF (IER.EQ.0) GO TO 980
               WRITE (MSGTXT,1950) WRITE, IER
               IERR = RDERR
 241           CALL MSGWRT (6)
               CALL ZDESTR (IVOL, PHNAME, IER)
               IF (IER.EQ.0) GO TO 243
                  WRITE (MSGTXT,1241) IER
 242              CALL MSGWRT (6)
C                                       and clear the entry
 243           CALL CATOPN (IVOL, CIND, BUFF, CMAX, IER)
               IF (IER.EQ.0) GO TO 244
                  WRITE (MSGTXT,1243) IER
                  GO TO 970
 244           CMOD = (CNO - 1) / NLPR
               NREC = 2 + CMOD
               NIND = 1 + NWPL * (CNO - NLPR*CMOD -1)
               CALL ZFIO (READ, CLUN, CIND, NREC, BUFF, IER)
               IF (IER.NE.0) GO TO 960
               BUFF(NIND) = CLEAR
               GO TO 900
C                                       increment search loop
 250     CONTINUE
            CNO = CNO + 1
            IF (CNO.LE.CMAX) GO TO 290
C                                       loop on volume if wanted
               IF ((.NOT.LPVOL) .OR. (LVOL.GE.NVOL)) GO TO 260
                  LVOL = LVOL + 1
                  CALL ZCLOSE (CLUN, CIND, IER)
                  GO TO 15
C                                       answer on high search
 260        IF ((IOP.GT.2) .OR. (HSEQ.LE.0)) GO TO 265
               IVOL = HVOL
               CNO = HCNO
               IF (IOP.NE.2) THEN
                  USID = HUSE
                  SEQ = HSEQ
                  CNAME = HNAME
                  CCLASS = HCLASS
                  PTYPE = HPTYPE
                  STAT = HSTAT
                  END IF
               GO TO 980
C                                       error: not found
 265           IF ((IOP.EQ.5) .AND. (CMAX.LT.46655)) GO TO 270
                  IERR = MISSER
                  GO TO 980
C                                       expand catalog, make more slots
 270           CALL ZPHFIL ('CA', IVOL, 0, 0, PHNAME, IER)
               I = 4
               CALL ZEXPND (CLUN, IVOL, PHNAME, I, IER)
               IF (IER.EQ.0) GO TO 275
                  WRITE (MSGTXT,1270) IER
                  IERR = MISSER
                  GO TO 970
 275           CMAX = CMAX + I * NLPR
               IF (CMAX.GT.46655) CMAX = 46655
               LREC = NREC
               NREC = 1
               CALL ZFIO (READ, CLUN, CIND, NREC, BUFF, IER)
               IF (IER.NE.0) GO TO 960
               BUFF(3) = CMAX
               CALL ZFIO (WRITE, CLUN, CIND, NREC, BUFF, IER)
               IF (IER.NE.0) GO TO 950
               NREC = LREC
               CALL FILL (256, 0, BUFF)
               DO 280 J = 1,256,NWPL
                  BUFF(J) = CLEAR
 280              CONTINUE
               DO 285 J = 1,I
                  NREC = NREC + 1
                  CALL ZFIO (WRITE, CLUN, CIND, NREC, BUFF, IER)
                  IF (IER.NE.0) GO TO 950
 285              CONTINUE
               NREC = LREC
 290        NIND = NIND + NWPL
            IF ((NIND.LT.NWPL*NLPR) .AND. (IOP.EQ.5)) GO TO 200
            IF (NIND.LT.NWPL*NLPR) GO TO 100
               NREC = NREC + 1
               NIND = 1
               GO TO 30
C                                       CLOSE module
 400     CONTINUE
            IERR = 0
            IF (USID.NE.BUFF(NIND)) GO TO 415
            CALL H2CHR (12, 1, BUFF(NIND+NNAME), TNAME)
            IF (CNAME.NE.TNAME) GO TO 415
            CALL H2CHR (6, NCLAS, BUFF(NIND+NNAME), TCLASS)
            IF ((CCLASS.NE.' ') .AND. (CCLASS.NE.TCLASS)) GO TO 415
 402        IF (SEQ.LE.0) GO TO 405
            IF (SEQ.NE.BUFF(NIND+NSEQ)) GO TO 415
            CALL H2CHR (2, NPTYPE, BUFF(NIND+NNAME), TTYPE)
            IF (PTYPE.NE.TTYPE) GO TO 415
C                                       it's ok
 405        IF (0.NE.BUFF(NIND+NSTAT)) GO TO 410
               BUFF(NIND) = CLEAR
C                                       so destroy header file
               CALL ZPHFIL ('CB', IVOL, CNO, 1, PHNAME, IER)
               CALL ZDESTR (IVOL, PHNAME, IER)
               IF (IER.EQ.0) GO TO 900
                  WRITE (MSGTXT,1241) IER
                  CALL MSGWRT (6)
                  GO TO 900
C                                       too active to close
 410        CONTINUE
               IERR = STCLSR
               WRITE (MSGTXT,1410) CNO
               GO TO 970
C                                       id's don't match
 415        CONTINUE
               IERR = IDCLSR
               WRITE (MSGTXT,1415) CNO
               GO TO 970
C                                       Info module
 500     CONTINUE
C                                       Check if slot in use
            USID = BUFF(NIND)
            IF (USID.NE.CLEAR) GO TO 510
               IERR = 6
               WRITE (MSGTXT,1500) CNO
               GO TO 970
 510        CALL H2CHR (12, 1, BUFF(NIND+NNAME), CNAME)
            CALL H2CHR (6, NCLAS, BUFF(NIND+NNAME), CCLASS)
            SEQ = BUFF(NIND+NSEQ)
            CALL STXT (BUFF(NIND+NSTAT), STAT)
            CALL H2CHR (2, NPTYPE, BUFF(NIND+NNAME), PTYPE)
            IERR = 0
            GO TO 980
C                                       Change status
 600     CONTINUE
C                                       Check if slot in use
            IF (BUFF(NIND).EQ.CLEAR) THEN
               IERR = 6
               GO TO 980
               END IF
            IF (DSKLOK(LVOL).EQ.1) GO TO 980
            CALL CHSTAT (STAT, BUFF(NIND+NSTAT), IER)
            IF (IER.EQ.0) GO TO 900
C                                       warning
            IF (IER.LE.2) THEN
               IERR = 8 + IER
               GO TO 900
C                                       READ busy: can't WRIT
            ELSE IF (IER.EQ.3) THEN
               IERR = 7
               WRITE (MSGTXT,1610) CNO
               GO TO 970
C                                       illegal status code
            ELSE
               IERR = INERR
               WRITE (MSGTXT,1615) STAT
               GO TO 970
               END IF
C                                       close down section
C                                       Output modified directory
 900  CALL ZFIO (WRITE, CLUN, CIND, NREC, BUFF, IER)
      IF (IER.EQ.0) GO TO 980
 950     WRITE (MSGTXT,1950) WRITE, IER
         IERR = RDERR
         GO TO 970
 960     WRITE (MSGTXT,1950) READ, IER
         IERR = RDERR
 970  CALL MSGWRT (6)
 980  IF (CIND.GT.0) CALL ZCLOSE (CLUN, CIND, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1005 FORMAT ('CATDIR: ILLEGAL OPCODE ',A4)
 1015 FORMAT ('CATDIR: OPEN AND CLOSE ON DISK',I3,' NOT ALLOWED')
 1017 FORMAT ('CATDIR: CATNO ',I6,' OUT OF RANGE')
 1200 FORMAT ('CATDIR: CAN''T USE DEFAULT NAME TO OPEN')
 1210 FORMAT ('CATDIR: CAN''T USE DEFAULT PHYSICAL TYPE TO OPEN')
 1225 FORMAT ('CATDIR: WARNING - AN OLD HEADER FILE WAS PRESENT')
 1230 FORMAT ('CATDIR: UNABLE TO CREATE HEADER FILE, ERROR',I5)
 1235 FORMAT ('CATDIR: UNABLE TO OPEN HEADER FILE, ERROR',I5)
 1241 FORMAT ('CATDIR: UNABLE TO DESTROY HEADER FILE, ERROR',I5)
 1243 FORMAT ('CATDIR: UNABLE TO REOPEN CATALOG FILE, ERROR',I5)
 1270 FORMAT ('CATDIR: UNABLE TO EXPAND DIRECTORY FILE, ERROR',I5)
 1410 FORMAT ('CATDIR: CAN''T CLOSE SLOT ',I6,' STATUS NONZERO')
 1415 FORMAT ('CATDIR: INCOMPLETE ID ON CLOSE OF CATNO',I6)
 1500 FORMAT ('CATDIR: INFO, CATNO ',I6,' EMPTY')
 1610 FORMAT ('CATDIR: CAN''T WRITE ON CATNO ',I6,' NONZERO STATUS')
 1615 FORMAT ('CATDIR: ',A4, ' ILLEGAL STATUS CODE')
 1950 FORMAT ('CATDIR: I/O ERROR ',A4,'ING CATALOG FILES:',I5)
      END
