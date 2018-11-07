      SUBROUTINE CATLST (IVOL, CNAME, CCLASS, SEQ, PTYPE, USID, QUICK,
     *   SLOT, IBLK, BUFFER, IERR)
C-----------------------------------------------------------------------
C! List the contents of the catalog directory file
C# Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999-2001, 2006, 2010, 2016, 2018
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
C   CATLST (catalog list) will list the catalog for verbs CATALOG and
C   MCAT/UCAT.  Wild card characters supported in CNAME and CCLASS.
C   Inputs:
C      IVOL    I       disk volume number, 0 = all volumes.
C      CNAME   C*12    (CATALOG only) Map name. Blank = any.
C      CCLASS  C*6     (CATALOG only) Map class.
C      SEQ     I       (CATALOG only) Map sequence number.
C      PTYPE   C*2     Map type.
C      USID    I       user number. 0=login user, 32000=all users.
C      QUICK   I       1 => catlg order quick print 2 => alphabetize
C                      -1 => catlg order to message file  -2 =>
C                      alphabetize to message file
C      SLOT    I       starting slot ( < 0 -> max slot number)
C   Outputs:
C      IBLK    I(256)  work buffer.
C      BUFFER  I(256)  work buffer.
C      IERR    I       error code, 0=ok.
C-----------------------------------------------------------------------
      CHARACTER CNAME*12, CCLASS*6, PTYPE*2
      INTEGER   IVOL, SEQ, USID, QUICK, SLOT, IBLK(256), BUFFER(256),
     *   IERR
C
      INTEGER   LMAX
      PARAMETER (LMAX=5120)
C
      CHARACTER NAME*12, CLASS*6, TYPE*2, LNAMES(LMAX)*25, TEMP*5,
     *   STATOT*4, TIME*8, DATE*12
      INTEGER   ITIME(6), LNAMEV(25,LMAX), IBKREC, NWPL, NLPR, MSGPRI,
     *   LINE, CLUN, CLEAR, CIND, NCLASS, NSEQ, NSTAT, NPTYPE, NTIME, I,
     *   IC, ICMAX, IENTRY, NIND, NNAME, NPAT(12), CPAT(6), LC, NC,
     *   COUNT, LSLOT(LMAX), AMAX, IV, ISL1, ISL2, ISL3, LVOL, IVBEG,
     *   IVEND, IRRN, LRRN, JERR, SKIP(512), NCOL
      LOGICAL   QUIT, LCNAME, LCLASS, LSEQ, LPTYPE, LUSID, LR, T, F,
     *   ALSOSC, IAMOK, HDROUT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /AIPSCR/ SKIP, LNAMEV, LSLOT
      DATA CLUN, CLEAR /15, -1/
      DATA T, F /.TRUE.,.FALSE./
      DATA NCLASS, NSEQ, NSTAT, NPTYPE, NTIME, NNAME /13,4,1,19,2,5/
C-----------------------------------------------------------------------
C                                       Initialize variables.
      LINE = 0
      IERR = 0
      NWPL = 10
      NLPR = 256 / NWPL
      AMAX = LMAX
      IF (ABS(QUICK).LE.1) AMAX = 1
C                                       Set values for CATALOG
      MSGPRI = 3
      LCNAME = CNAME.NE.' '
      LCLASS = CCLASS.NE.' '
      LSEQ  = SEQ.NE.0
      LPTYPE = PTYPE.NE.' '
      ALSOSC = PTYPE.EQ.'SC'
      LUSID = (USID.GT.0) .AND. (USID.NE.32000)
C                                       Set values for CAT.
      IF (QUICK.LE.0) GO TO 20
         MSGPRI = 1
         LCNAME = F
         LCLASS = F
         LSEQ = F
         ALSOSC = T
C                                       Set volume loop.
 20   IVBEG = IVOL
      IVEND = IVOL
      IF (LCNAME) CALL PSFORM (12, CNAME, NPAT)
      IF (LCLASS) CALL PSFORM (6, CCLASS, CPAT)
C                                       Set default volumes. (all).
      IF (IVOL.GT.0) GO TO 30
         IVBEG = 1
         IVEND = NVOL
C                                       Slot range
 30   ISL1 = 1
      ISL2 = 960000
      IF (SLOT.LT.0) ISL2 = -SLOT
      IF (SLOT.GT.0) ISL1 = SLOT
C                                       Loop over specified volumes.
      DO 130 IV = IVBEG,IVEND
         HDROUT = .FALSE.
C                                       Open catalog
         IF ((IVBEG.LT.IVEND) .AND. (.NOT.IAMOK(IV,'CA'))) GO TO 130
         LVOL = -IV
         CALL CATOPN (LVOL, CIND, BUFFER, ICMAX, IERR)
         IF (IERR.NE.0) THEN
            IF (IERR.EQ.3) CALL ZCLOSE (CLUN, CIND, JERR)
            IF (IVBEG.LT.IVEND) GO TO 130
            GO TO 999
            END IF
         LRRN = 0
         COUNT = 0
C                                       Loop for all entries in catalog
         ISL3 = MIN (ICMAX, ISL2)
         DO 120 IENTRY = ISL1,ISL3
            IRRN = 2 + (IENTRY - 1) / NLPR
            IBKREC = MOD (IENTRY - 1, NLPR) + 1
C                                       Read next block.
            IF (IRRN.NE.LRRN) THEN
               LRRN = IRRN
               CALL ZFIO ('READ', CLUN, CIND, IRRN, IBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
C                                       Compare for match.
            NIND = 1 + NWPL * (IBKREC - 1)
            IF (IBLK(NIND).EQ.CLEAR) GO TO 80
            IF ((IBLK(NIND).NE.USID) .AND. (LUSID)) GO TO 80
            CALL H2CHR (12, 1, IBLK(NIND+NNAME), NAME)
            CALL H2CHR (12, NCLASS, IBLK(NIND+NNAME), CLASS)
C                                       Force SC in some cases
            CALL H2CHR (2, NPTYPE, IBLK(NIND+NNAME), TYPE)
            IF ((ALSOSC) .AND. (TYPE.EQ.'SC')) GO TO 70
C                                       Check other name parameters
            IF ((IBLK(NIND+NSEQ).NE.SEQ) .AND. (LSEQ)) GO TO 80
            IF ((LPTYPE) .AND. (PTYPE.NE.TYPE)) GO TO 80
            IF (LCNAME) THEN
               CALL CHWMAT (12, CNAME, NPAT, 1, NAME, LR)
               IF (.NOT.LR) GO TO 80
               END IF
            IF (LCLASS) THEN
               CALL CHWMAT (6, CCLASS, CPAT, 1, CLASS, LR)
               IF (.NOT.LR) GO TO 80
               END IF
C                                       Match found.
 70         COUNT = COUNT + 1
            WRITE (TEMP,1070,ERR=75) IBLK(NIND+NSEQ)
 75         LNAMES(COUNT) = NAME // CLASS // TEMP // TYPE
            LSLOT(COUNT) = IENTRY
            DO 76 I = 1,25
               LNAMEV(I,COUNT) = ICHAR (LNAMES(COUNT)(I:I))
 76            CONTINUE
C                                       Print it now
 80         IF ((COUNT.LT.AMAX) .AND. (IENTRY.LT.ICMAX)) GO TO 120
            IF (COUNT.LE.0) GO TO 120
               NC = 0
C                                       Sort in simple loop
 85            LC = 0
                  DO 100 IC = 1,COUNT
                     IF (LSLOT(IC).LE.0) GO TO 100
                     IF (LC.EQ.0) LC = IC
                     DO 90 I = 1,25
                        IF (LNAMEV(I,IC)-LNAMEV(I,LC)) 95, 90, 100
 90                     CONTINUE
                     GO TO 100
 95                  LC = IC
 100                 CONTINUE
                  IC = LSLOT(LC)
                  LSLOT(LC) = 0
C                                       get info on entry
                  IRRN = 2 + (IC - 1) / NLPR
                  IF (IRRN.EQ.LRRN) GO TO 105
                     LRRN = IRRN
                     CALL ZFIO ('READ', CLUN, CIND, IRRN, IBLK, IERR)
                     IF (IERR.NE.0) GO TO 980
 105              IBKREC = MOD (IC - 1, NLPR) + 1
                  NIND = 1 + NWPL * (IBKREC - 1)
C                                       Print header.
                  IF (.NOT.HDROUT) THEN
                     WRITE (MSGTXT,1040) IV
                     CALL MSGWRT (MSGPRI)
                     WRITE (MSGTXT,1042)
                     CALL MSGWRT (MSGPRI)
                     LINE = LINE + 2
                     HDROUT = .TRUE.
                     END IF
                  LINE = LINE + 1
C                                       Screen full. Wait for user
C                                       response.
                  IF (LINE.LT.(ABS(CRTMAX)-2)) GO TO 110
                     CALL ZCLOSE (CLUN, CIND, JERR)
                     CALL SCHOLD (QUIT)
                     CALL ZWINC (NCOL)
                     IF (QUIT) GO TO 999
                     LINE = 0
                     CALL CATOPN (IV, CIND, BUFFER, ICMAX, IERR)
C                                       Print this line.
 110              CALL STXT (IBLK(NIND+NSTAT), STATOT)
                  IF (STATOT.EQ.'REST') STATOT = ' '
                  CALL CATIME (2, IBLK(NIND+NTIME), ITIME)
                  ITIME(1) = -ITIME(1)
                  CALL TIMDAT (ITIME(4), ITIME(1), TIME, DATE)
                  WRITE (MSGTXT,1110) IC, IBLK(NIND),
     *               LNAMES(LC)(1:12), LNAMES(LC)(13:18),
     *               LNAMES(LC)(19:23), LNAMES(LC)(24:25), DATE,
     *               TIME, STATOT
                  CALL MSGWRT (MSGPRI)
                  NC = NC + 1
                  IF (NC.LT.COUNT) GO TO 85
               COUNT = 0
 120        CONTINUE
C                                       Close catalog file.
         CALL ZCLOSE (CLUN, CIND, JERR)
 130     CONTINUE
      GO TO 999
C                                       Close catalog file on error.
 980  CALL ZCLOSE (CLUN, CIND, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('Catalog on disk ',I2)
 1042 FORMAT ('  Cat  Usid Mapname      Class   Seq  Pt    Last acc',
     *   'ess     Stat')
 1070 FORMAT (I5)
 1110 FORMAT (I5,1X,I5,1X,A12,'.',A6,'.',A5,1X,A2,1X,A10,A8,1X,A4)
      END
