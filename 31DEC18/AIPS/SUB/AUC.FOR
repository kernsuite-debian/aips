      SUBROUTINE AUC (BRANCH)
C-----------------------------------------------------------------------
C! verbs to enter, list, drop gripes, enter password, max ap size
C# POPS-appl Gripes
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2002, 2004, 2008, 2012-2013
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
C   AUC performs operations related to the Gripe and Password files.
C   Branch = 1 : GRIPE    enter a new Gripe.
C            2 : GRINDEX  index the Gripes now in the file.
C            3 : GRLIST   list an individual gripe.
C            4 : PASSWORD change the current password.
C            5 : GRDROP   drop the most recent gripe
C            6 : SETMAXAP set the maximum AP size
C   The Gripe file is named GR100000. and starts with one record of
C   index information as:
C   Word   1   : Number of 512-byte records now in file.
C          2   : Number of Gripes now in file.
C          3   : Record number for next Gripe.
C          4   : Character position in that record for next Gripe.
C        5 - 7 : Reserved.
C          8   : Gripe #1 logon user number.
C          9   : Gripe #1 start record number.
C         10   : Gripe #1 start character position.
C         ...  : Gripes #2 - 83.
C   The individual entries are packed character strings having the
C   form: ' {date   time}  {system ID string}  {logon user #}
C   {...name...}  {...address...}  {...phone...}
C   {...gripe contents...} {one line description } {E-mail address }
C   { }'.
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      LOGICAL   F, T
      INTEGER   HBUF(256), I, IERR, ILP, IND(2), IPC, IPOS, IRNO, JL,
     *   IT(6), ITRIM, J, J0, J1, J2, KPOS, KREC, LENSTR, LLP, IDUM,
     *   LLPOS, LLREC, LPC, LPOS, LREC, LUN(2), NCH, NCHPR, NUMBER,
     *   POTERR, PWUSER, IROUND, OPOS, JERR, ADDRSZ
      REAL      RBUF(256), RDUM, APSIZ
      HOLLERITH CBUF(256), WBUF(256)
      CHARACTER APASS*12, BPASS*12, DATE*12, EMAIL*80, PHNAME*48,
     *   PRGNAM*6, QUIT*4, TIME*8, CDUM*1, SCNAME*256
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PAPC.INC'
      EQUIVALENCE (HBUF(1), RBUF(1))
      DATA T, F /.TRUE.,.FALSE./
      DATA QUIT /'_END'/
      DATA PRGNAM /'AUC '/
C-----------------------------------------------------------------------
C                                       Check status: interactive only
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.6)) GO TO 999
      POTERR = 60
      IF ((NPOPS.GT.NINTRN) .OR. (TSKNAM(1:5).EQ.'AIPSC')) GO TO 980
      IF (ISBTCH.EQ.32000) GO TO 980
      POTERR = 53
      IF ((IUNIT.NE.1) .AND. (BRANCH.EQ.1)) GO TO 980
      SCNAME = ' '
      LUN(1) = 13
      LUN(2) = 0
C                                       Open GR disk and read
      IF ((BRANCH.NE.4) .AND. (BRANCH.NE.6)) THEN
         POTERR = 50
         CALL ZPHFIL ('GR', 1, 0, 0, PHNAME, IERR)
         CALL ZOPEN (LUN, IND, 1, PHNAME, F, T, T, IERR)
         IF (IERR.NE.0) THEN
            IND(1) = 0
            GO TO 980
            END IF
         CALL ZFIO ('READ', LUN, IND, 1, HBUF, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       Branch to operation
 20   GO TO (100, 200, 300, 400, 300, 600), BRANCH
C-----------------------------------------------------------------------
C                                       GRIPE
C                                       Enter a new gripe.
C-----------------------------------------------------------------------
 100  IF (HBUF(2).GE.83) THEN
         WRITE (MSGTXT,1100)
         GO TO 960
         END IF
C                                       Create temporary mail file
      CALL ZFULLN (' ', 'FITS', 'gripemail', SCNAME, IERR)
      POTERR = 55
      IF (IERR.NE.0) GO TO 970
      LUN(2) = 3
      CALL ZTXOPN ('QWRT', LUN(2), IND(2), SCNAME, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         LUN(2) = 0
         SCNAME = ' '
         END IF
C                                       Set up new directory entry
      IPC = 8 + 3 * HBUF(2)
      HBUF(IPC)   = NLUSER
      HBUF(IPC+1) = HBUF(3)
      HBUF(IPC+2) = HBUF(4)
      IPT = '!'
C                                       Read first Gripe record
      IRNO = HBUF(3)
      CALL ZFIO ('READ', LUN, IND, IRNO, CBUF, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Date and time
      CALL ZDATE (IT(1))
      CALL ZTIME (IT(4))
      CALL TIMDAT (IT(4), IT(1), TIME, DATE)
      JBUFF = DATE // TIME
      CALL CHGRIP (0, LUN, IND, HBUF, CBUF, JBUFF(1:20), IERR)
      IF (IERR.NE.0) GO TO 190
C                                       System ID
      JBUFF = HSTNAM // ' ' // SYSNAM
      CALL CHGRIP (0, LUN, IND, HBUF, CBUF, JBUFF(1:33), IERR)
      IF (IERR.NE.0) GO TO 190
C                                       User # ID
      WRITE (JBUFF,1105) NLUSER, RLSNAM, SYSTYP
      CALL CHGRIP (0, LUN, IND, HBUF, CBUF, JBUFF(1:20), IERR)
      IF (IERR.NE.0) GO TO 190
C                                       General instructions
 110  MSGTXT = 'Please limit each GRIPE to one bug report and/or ' //
     *   'suggestion.'
      CALL MSGWRT (1)
      MSGTXT = 'Please give details and use lower case where ' //
     *   'appropriate.'
      CALL MSGWRT (1)
C                                       Name
      CALL ADVERB ('GRNAME', 'C', 1, 20, IDUM, RDUM, JBUFF)
      IF (ERRNUM.NE.0) GO TO 970
C                                       I've seen this one before
      IF (JBUFF.NE.' ') THEN
         MSGTXT = 'Hello again ' // JBUFF(1:20)
         CALL MSGWRT (1)
C                                       Don't know his name
      ELSE
         MSGTXT ='What is your name?'
         CALL MSGWRT (1)
         CALL PREAD (KARBUF)
         IF (ERRNUM.NE.0) GO TO 970
         CALL ADVRBS ('GRNAME', 'C', 1, 20, IDUM, RDUM, JBUFF)
         IF (ERRNUM.NE.0) GO TO 970
         END IF
      CALL CHGRIP (0, LUN, IND, HBUF, CBUF, JBUFF(1:20), IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Address - get it if I don't have
C                                       it.
      CALL ADVERB ('GRADDRES', 'C', 1, 48, IDUM, RDUM, JBUFF)
      IF (ERRNUM.NE.0) GO TO 970
      IF (JBUFF.EQ.' ') THEN
         MSGTXT = 'In one line, what is your address?'
         CALL MSGWRT (1)
         CALL PREAD (KARBUF)
         IF (ERRNUM.NE.0) GO TO 970
         CALL ADVRBS ('GRADDRES', 'C', 1, 48, IDUM, RDUM, JBUFF)
         IF (ERRNUM.NE.0) GO TO 970
         END IF
      CALL CHGRIP (0, LUN, IND, HBUF, CBUF, JBUFF(1:48), IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Phone - may already have it.
      CALL ADVERB ('GRPHONE', 'C', 1, 16, IDUM, RDUM, JBUFF)
      IF (ERRNUM.NE.0) GO TO 970
      IF (JBUFF.EQ.' ') THEN
         MSGTXT = 'What is your phone number?'
         CALL MSGWRT (1)
         CALL PREAD (KARBUF)
         IF (ERRNUM.NE.0) GO TO 970
         CALL ADVRBS ('GRPHONE', 'C', 1, 16, IDUM, RDUM, JBUFF)
         IF (ERRNUM.NE.0) GO TO 970
         END IF
      CALL CHGRIP (0, LUN, IND, HBUF, CBUF, JBUFF(1:16), IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Get E-mail address
      CALL ADVERB ('GREMAIL', 'C', 1, 48, IDUM, RDUM, JBUFF)
      IF (ERRNUM.NE.0) GO TO 970
      IF (JBUFF.EQ.' ') THEN
         MSGTXT = 'At what E-Mail address can you be reached?'
         CALL MSGWRT (1)
         CALL PREAD (KARBUF)
         IF (ERRNUM.NE.0) GO TO 970
         CALL ADVRBS ('GREMAIL', 'C', 1, 48, IDUM, RDUM, JBUFF)
         IF (ERRNUM.NE.0) GO TO 970
         END IF
      EMAIL = JBUFF
C                                       Show system, version
 140  MSGTXT = 'Gripe from version ' // RLSNAM // ' System name: '//
     *   HSTNAM
      CALL MSGWRT (1)
C                                       Gripe itself
 150  MSGTXT = 'Enter your comment 1 line at a time.  Type _END ' //
     *   'or _end to stop.'
      CALL MSGWRT (1)
      MSGTXT = 'Type _FORGET or _forget to exit without recording the'
     *   // ' gripe'
      CALL MSGWRT (1)
      DO 160 I = 1,64
         CALL PREAD (KARBUF)
         IF (ERRNUM.NE.0) GO TO 970
         IPC = 2
         IF (I.EQ.1) IPC = 1
         CALL CHGRIP (IPC, LUN, IND, HBUF, CBUF, JBUFF(1:NBYTES), IERR)
         IF (IERR.EQ.99) GO TO 170
         IF (IERR.NE.0) GO TO 190
 160     CONTINUE
      MSGTXT = 'Enough already!!!'
      CALL MSGWRT (1)
      JBUFF = QUIT
      CALL CHGRIP (IPC, LUN, IND, HBUF, CBUF, JBUFF(1:4), IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.99)) GO TO 970
C                                       Get one line summary
 170  MSGTXT = 'Give a one line summary of your problem/suggestion:'
      CALL MSGWRT (1)
      CALL PREAD (KARBUF)
      IF (ERRNUM.NE.0) GO TO 970
C                                       Write one-liner, email final { }
      IP = 0
      LENSTR = ITRIM (JBUFF(:80))
      CALL CHGRIP (IP, LUN, IND, HBUF, CBUF, JBUFF(:LENSTR), IERR)
      IF (IERR.NE.0) GO TO 970
      LENSTR = ITRIM (EMAIL)
      CALL CHGRIP (IP, LUN, IND, HBUF, CBUF, EMAIL(1:LENSTR), IERR)
      IF (IERR.NE.0) GO TO 970
      CALL CHGRIP (IP, LUN, IND, HBUF, CBUF, '  ', IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Done : write data, dir.
      IRNO = HBUF(3)
      CALL ZFIO ('WRIT', LUN, IND, IRNO, CBUF, IERR)
      IF (IERR.NE.0) GO TO 970
      HBUF(2) = HBUF(2) + 1
      CALL ZFIO ('WRIT', LUN, IND, 1, HBUF, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       send e-mail
      IF (LUN(2).GT.0) THEN
         CALL ZTXCLS (LUN(2), IND(2), IERR)
         CALL ZGMAIL (SCNAME, IERR)
         IF (IERR.NE.0) CALL ZTXZAP (3, SCNAME, IERR)
         SCNAME = ' '
         END IF
      GO TO 990
C                                       _forget occurred
 190  IF (IERR.GT.0) GO TO 970
         MSGTXT = 'Partial gripe has been ''FORGOTTEN'''
         CALL MSGWRT (3)
         IF (LUN(2).GT.0) CALL ZTXCLS (LUN(2), IND(2), IERR)
         IF (SCNAME.NE.' ') CALL ZTXZAP (3, SCNAME, IERR)
         GO TO 990
C-----------------------------------------------------------------------
C                                       GRINDEX
C                                       Index contents: #, date, names
C-----------------------------------------------------------------------
 200  WRITE (MSGTXT,1200)
      CALL MSGWRT (2)
      LPC = 5
      NUMBER = HBUF(2)
      HBUF(3) = 0
      IF (NUMBER.LE.0) GO TO 990
      DO 230 I = 1,NUMBER
         LPC = LPC + 3
C                                       Read rec 1 of gripe
         IF (HBUF(LPC+1).EQ.HBUF(3)) GO TO 210
            HBUF(3) = HBUF(LPC+1)
            IRNO = HBUF(3)
            CALL ZFIO ('READ', LUN, IND, IRNO, CBUF, IERR)
            IF (IERR.NE.0) GO TO 970
 210     HBUF(4) = HBUF(LPC+2)
C                                       Seq and user #s
         WRITE (MSGTXT,1210) I, HBUF(LPC)
C                                       Time
         CALL CHGRIP (3, LUN, IND, HBUF, CBUF, JBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         MSGTXT(12:31) = JBUFF(1:20)
C                                       Skip System ID
         CALL CHGRIP (3, LUN, IND, HBUF, CBUF, JBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       user #, version, systype
         CALL CHGRIP (3, LUN, IND, HBUF, CBUF, JBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       find version
         J1 = ITRIM (JBUFF)
         J0 = 0
 215     J0 = J0 + 1
         IF (JBUFF(J0:J0).NE.' ') GO TO 215
         J0 = J0 + 2
         MSGTXT(34:40) = JBUFF(J0:J0+6)
         J0 = J0 + 9
         J1 = MIN (J1, J0+3)
         IF (J0.LE.J1) MSGTXT(42:45) = JBUFF(J0:J1)
C                                       User name
         CALL CHGRIP (3, LUN, IND, HBUF, CBUF, JBUFF, IERR)
         IF (IERR.NE.0) GO TO 970
         MSGTXT(48:80) = JBUFF(1:33)
         CALL MSGWRT (2)
 230     CONTINUE
      GO TO 990
C-----------------------------------------------------------------------
C                                       GRLIST
C                                       List a gripe
C                                       GRDROP
C                                       Drop a gripe: e-mail part
C-----------------------------------------------------------------------
 300  CALL ADVERB ('JOBNUM', 'I', 1, 0, LPC, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF ((LPC.LT.1) .OR. (LPC.GT.HBUF(2))) LPC = HBUF(2)
      IF (LPC.LE.0) GO TO 990
C                                       Create temporary mail file
      IPC = 5 + 3 * LPC
      LUN(2) = 0
      IF (BRANCH.EQ.5) THEN
         IF ((HBUF(IPC).NE.NLUSER) .AND. (NLUSER.NE.1)) THEN
            WRITE (MSGTXT,1500) LPC
            POTERR = 101
            GO TO 960
            END IF
         IF (HBUF(IPC).EQ.NLUSER) THEN
            CALL ZFULLN (' ', 'FITS', 'gripemail', SCNAME, JERR)
            POTERR = 55
            IF (JERR.NE.0) GO TO 970
            LUN(2) = 3
            CALL ZTXOPN ('QWRT', LUN(2), IND(2), SCNAME, .FALSE., JERR)
            IF (JERR.NE.0) THEN
               LUN(2) = 0
               SCNAME = ' '
               END IF
            END IF
         END IF
      I = 5 + 3 * LPC
      HBUF(3) = HBUF(I+1)
      HBUF(4) = HBUF(I+2)
      IRNO = HBUF(3)
      CALL ZFIO ('READ', LUN, IND, IRNO, CBUF, IERR)
      IF (IERR.NE.0) GO TO 970
      WRITE (MSGTXT,1300) LPC, HBUF(I)
      IF (BRANCH.EQ.5) WRITE (MSGTXT,1301) LPC, HBUF(I)
      CALL MSGWRT (3)
      DO 310 I = 1,72
         CALL CHGRIP  (3, LUN, IND, HBUF, CBUF, JBUFF, IERR)
         IF ((IERR.GT.0) .AND. (IERR.NE.100)) GO TO 970
         IF ((BRANCH.EQ.5) .AND. (LUN(2).GT.0)) THEN
            CALL CHTRIM (JBUFF, 80, JBUFF, JL)
            JL = MAX (1, JL)
            CALL ZTXIO ('WRIT', LUN(2), IND(2), JBUFF(:JL), JERR)
            IF (JERR.NE.0) THEN
               CALL ZTXCLS (LUN(2), IND(2), JERR)
               LUN(2) = 0
               END IF
         ELSE
            MSGTXT = JBUFF
            IF (I.NE.2) CALL MSGWRT (3)
            END IF
         IF ((I.GT.6) .AND. (IERR.NE.100)) GO TO 320
 310     CONTINUE
C                                       Summary
 320  MSGTXT = 'Summary:'
      IF (LUN(2).LE.0) CALL MSGWRT (3)
      CALL CHGRIP  (3, LUN, IND, HBUF, CBUF, JBUFF, IERR)
      IF (IERR.NE.0) GO TO 970
      IF ((BRANCH.EQ.5) .AND. (LUN(2).GT.0)) THEN
         CALL CHTRIM (JBUFF, 80, JBUFF, JL)
         JL = MAX (1, JL)
         CALL ZTXIO ('WRIT', LUN(2), IND(2), JBUFF(:JL), JERR)
         IF (JERR.NE.0) THEN
            CALL ZTXCLS (LUN(2), IND(2), JERR)
            LUN(2) = 0
            END IF
      ELSE
         MSGTXT = JBUFF
         CALL MSGWRT (3)
         END IF
C                                       E-Mail address
      CALL CHGRIP  (3, LUN, IND, HBUF, CBUF, JBUFF, IERR)
      IF (IERR.NE.0) GO TO 970
      IF ((BRANCH.EQ.5) .AND. (LUN(2).GT.0)) THEN
         CALL CHTRIM (JBUFF, 80, JBUFF, JL)
         JL = MAX (1, JL)
         CALL ZTXIO ('WRIT', LUN(2), IND(2), JBUFF(:JL), JERR)
         IF (JERR.NE.0) THEN
            CALL ZTXCLS (LUN(2), IND(2), JERR)
            LUN(2) = 0
            END IF
      ELSE
         MSGTXT = 'E-Mail address:' // JBUFF(1:50)
         CALL MSGWRT (3)
         END IF
C                                       send e-mail
      IF (BRANCH.EQ.5) THEN
         IF (LUN(2).GT.0) THEN
            JBUFF = '*****  DROP THE ABOVE GRIPE *****'
            CALL CHTRIM (JBUFF, 80, JBUFF, JL)
            JL = MAX (1, JL)
            CALL ZTXIO ('WRIT', LUN(2), IND(2), JBUFF(:JL), JERR)
            IF (JERR.NE.0) THEN
               CALL ZTXCLS (LUN(2), IND(2), JERR)
               LUN(2) = 0
            ELSE
               CALL ZTXCLS (LUN(2), IND(2), JERR)
               CALL ZGMAIL (SCNAME, JERR)
               IF (JERR.NE.0) THEN
                  CALL ZTXZAP (3, SCNAME, JERR)
                  LUN(2) = 0
                  END IF
               SCNAME = ' '
               END IF
            END IF
         IF (LUN(2).LE.0) THEN
            MSGTXT = 'Warning: no message sent to aipsmail@nrao.edu'
            CALL MSGWRT (6)
            END IF
         GO TO 500
         END IF
      GO TO 990
C-----------------------------------------------------------------------
C                                       PASSWORD
C                                       Enter a new password.
C-----------------------------------------------------------------------
 400  POTERR = 50
      CALL ZPHFIL ('PW', 1, 0, 0, PHNAME, IERR)
      CALL ZOPEN (LUN, IND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         IND(1) = 0
         GO TO 980
         END IF
C                                       AIPS manager (user number 1) can
C                                       change passwords for others.
      PWUSER = NLUSER
C                                       Alternate user number on stack?
      IF (NLUSER.EQ.1) THEN
         IF ((SP.GE.1) .AND. (STACK(SP).NE.2)) THEN
            PWUSER = IROUND(V(SP))
            SP = SP - 1
            END IF
         END IF
      WRITE (MSGTXT,1400) PWUSER
      CALL MSGWRT (8)
      MSGTXT= 'Enter the desired new password (<= 12 characters)'
      CALL MSGWRT (1)
      CALL ZPASWD (APASS, HBUF, IERR)
      MSGTXT = 'Please verify'
      CALL MSGWRT (1)
      CALL ZPASWD (BPASS, HBUF, IERR)
      IF (BPASS.NE.APASS) THEN
         MSGTXT = 'Passwords don''t match!  try again'
         CALL MSGWRT (8)
         GO TO 990
         END IF
      IF (IERR.NE.0) GO TO 970
         I = 3
         J = 256 / I
         I = MOD (PWUSER-1, J) * 3 + 1
         IRNO = (PWUSER - 1) / J + 1
         CALL ZFIO ('READ', LUN, IND, IRNO, HBUF, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL PASENC (APASS, RBUF(I))
         CALL ZFIO ('WRIT', LUN, IND, IRNO, HBUF, IERR)
         IF (IERR.NE.0) GO TO 970
         GO TO 990
C-----------------------------------------------------------------------
C                                       GRDROP
C                                       Delete a gripe
C-----------------------------------------------------------------------
 500  CALL ADVERB ('JOBNUM', 'I', 1, 0, LPC, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF ((LPC.LT.1) .OR. (LPC.GT.HBUF(2))) LPC = HBUF(2)
      IF (LPC.LE.0) GO TO 990
      IPC = 5 + 3 * LPC
      IF ((HBUF(IPC).EQ.NLUSER) .OR. (NLUSER.EQ.1)) GO TO 510
         WRITE (MSGTXT,1500) LPC
         POTERR = 101
         GO TO 960
C                                       gripe is last one
 510  IF (LPC.LT.HBUF(2)) GO TO 520
         HBUF(3) = HBUF(IPC+1)
         HBUF(4) = HBUF(IPC+2)
         GO TO 580
C                                       Gripe is intermediate one
 520  CONTINUE
         LREC = HBUF(IPC+1)
         KREC = HBUF(IPC+4)
         LPOS = HBUF(IPC+2)
         KPOS = HBUF(IPC+5)
         LLREC = HBUF(3)
         LLPOS = HBUF(4)
         NCHPR = 1024
         CALL ZFIO ('READ', LUN, IND, LREC, WBUF, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL ZFIO ('READ', LUN, IND, KREC, CBUF, IERR)
         IF (IERR.NE.0) GO TO 970
         OPOS = LPOS
         IPOS = KPOS
 530     J1 = NCHPR + 1 - OPOS
            J2 = NCHPR + 1 - IPOS
            IF (KREC.EQ.LLREC) J2 = LLPOS - IPOS
            J0 = MIN (J1, J2)
            CALL CHCOPY (J0, IPOS, CBUF, OPOS, WBUF)
            OPOS = OPOS + J0
            IPOS = IPOS + J0
C                                       output record full
            IF (OPOS.LE.NCHPR) GO TO 540
               CALL ZFIO ('WRIT', LUN, IND, LREC, WBUF, IERR)
               IF (IERR.NE.0) GO TO 970
               LREC = LREC + 1
               OPOS = 1
C                                       input record exhausted
 540        IF ((KREC.EQ.LLREC) .AND. (IPOS.GE.LLPOS)) GO TO 550
            IF (IPOS.LE.NCHPR) GO TO 530
               KREC = KREC + 1
               IF (KREC.GT.LLREC) GO TO 550
               CALL ZFIO ('READ', LUN, IND, KREC, CBUF, IERR)
               IF (IERR.NE.0) GO TO 970
               IPOS = 1
               GO TO 530
 550     IF (OPOS.LE.1) GO TO 560
            LLP = NCHPR + 1 - OPOS
            CALL CHFILL (LLP, HBLANK, OPOS, WBUF)
            CALL ZFIO ('WRIT', LUN, IND, LREC, WBUF, IERR)
            IF (IERR.NE.0) GO TO 970
C                                       fix directory
 560     LLP = HBUF(2) - 1
         LPC = LPC + 1
         IPC = IPC + 3
         IF (LPC.GT.LLP) GO TO 570
            DO 565 ILP = LPC,LLP
               HBUF(IPC-3) = HBUF(IPC)
               NCH = HBUF(IPC+5) - HBUF(IPC+2) + HBUF(IPC-1) - 1
     *            + NCHPR * (HBUF(IPC+4) - HBUF(IPC+1))
               HBUF(IPC+1) = HBUF(IPC-2) + NCH / NCHPR
               HBUF(IPC+2) = MOD (NCH, NCHPR) + 1
               IPC = IPC + 3
 565           CONTINUE
 570     NCH = HBUF(4) - HBUF(IPC+2) + HBUF(IPC-1) - 1
     *      + NCHPR * (HBUF(3) - HBUF(IPC+1))
         HBUF(3) = HBUF(IPC-2) + NCH / NCHPR
         HBUF(4) = MOD (NCH, NCHPR) + 1
         HBUF(IPC-3) = HBUF(IPC)
 580  HBUF(2) = HBUF(2) - 1
      CALL ZFIO ('WRIT', LUN, IND, 1, HBUF, IERR)
      IF (IERR.NE.0) GO TO 970
      GO TO 990
C-----------------------------------------------------------------------
C                                       SETMAXAP
C                                       examine/set a max AP size
C-----------------------------------------------------------------------
 600  RDUM = 0.0
      IF ((SP.GE.1) .AND. (STACK(SP).NE.2)) THEN
         RDUM = V(SP)
         SP = SP - 1
         END IF
      APSIZ = KAPWRD / 128.0
      WRITE (MSGTXT,1600) APSIZ
      CALL MSGWRT (4)
      J = RDUM * 128.0 + 0.5
      CALL ZADRSZ (ADDRSZ)
      J1 = 2 * 128 * 8 - 48
      IF (ADDRSZ.EQ.8) J1 = 6 * 256 * 8
C                                       message
      IF ((J.LT.PSAPMN) .OR. (RDUM.GT.J1)) THEN
         J = PSAPMN / 256
         WRITE (MSGTXT,1601) J, J1
         CALL MSGWRT (4)
C                                       change the SP file!
      ELSE
         APSIZ = J / 128.0
         WRITE (MSGTXT,1602) APSIZ
         CALL MSGWRT (4)
         CALL ZPHFIL ('SP', 1, 0, 0, PHNAME, IERR)
         CALL ZOPEN (LUN, IND, 1, PHNAME, F, T, T, IERR)
         IF (IERR.NE.0) THEN
            IND(1) = 0
            GO TO 980
            END IF
         CALL ZFIO ('READ', LUN, IND, 1, HBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         HBUF(11) = J
         HBUF(69) = 0
         CALL ZFIO ('WRIT', LUN, IND, 1, HBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL ZCLOSE (LUN, IND, IERR)
         KAPWRD = J
         KAP2WD = 0
         END IF
      GO TO 999
C-----------------------------------------------------------------------
C                                       Errors
 960  CALL MSGWRT (8)
 970  IF (SCNAME.NE.' ') CALL ZTXZAP (3, SCNAME, IERR)
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 990  IPT = '>'
      IF (IND(1).GT.0) CALL ZCLOSE (LUN, IND, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('GRIPE FILE FULL - CALL AIPS MANAGER')
 1105 FORMAT (I5,2X,A7,2X,A4)
 1200 FORMAT ('Seq User',6X,'Date',7X,'Time',4X,'Version OS  ',2X,
     *   'Name')
 1210 FORMAT (I3,I5)
 1300 FORMAT ('Gripe number',I3,'  from user number',I5)
 1301 FORMAT ('Dropping gripe number',I3,'  from user number',I5)
 1400 FORMAT ('Changing password for user',I5)
 1500 FORMAT ('GRIPE',I3,' DOES NOT BELONG TO YOU')
 1600 FORMAT ('SETMAXAP: current max dynamic AP is',F10.3,' Mbytes')
 1601 FORMAT ('SETMAXAP: allowed range is',I4,' to',I6,' Mbytes')
 1602 FORMAT ('SETMAXAP: setting max dynamic AP to',F10.3,' Mbytes')
      END
