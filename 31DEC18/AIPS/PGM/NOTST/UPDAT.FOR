      PROGRAM UPDAT
C-----------------------------------------------------------------------
C! Stand-alone task to update AIPS data files to a new format.
C# Service Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2002
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
C  UPDAT will update AIPS data files starting with the 15OCT85 AIPS
C  to the latest AIPS data format.  The AIPS data file versions are
C  now encoded in the AIPS file names.  This permits UPDAT to be
C  run more than once on a set of data without causing files to be
C  updated twice.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   USERS(1000), DISKS(2), VERNUM(2), IERR
      INTEGER   TTY(2)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA TTY /5,0/, PRGNAM /'UPDAT '/
C-----------------------------------------------------------------------
C                                       Standard AIPS inits.
      CALL AIPINI (TTY, PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get users input.
      CALL GETRNG (TTY, USERS, DISKS, VERNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Change all data.
      CALL DOCHNG (USERS, DISKS, VERNUM, IERR)
C
 999  STOP
      END
      SUBROUTINE GETRNG (TTY, USERS, DISKS, VERNUM, IERR)
C-----------------------------------------------------------------------
C  Get user input for the range of users, disks and version updates.
C  Input:
C     TTY     I(2)   LUN and FTAB of open terminal.
C  Output:
C     USERS   I(3)   USERS(1) is type of user number entry, 1 means
C                    use a range of user numbers, the first one is in
C                    USERS(2) and the last user number is in USERS(3).
C                    If USERS(1) is a 2, then the user numbers are in
C                    a text file HLPFIL:USERLIST.HLP.  In this 2nd case
C                    USERS(4) through USERS(N) contains the user
C                    numbers.  USERS(3) is N.
C     DISKS   I(2)   First and last disk for update.
C     VERNUM  I(2)   First and last version numbers for update.
C     IERR    I      Error code. 0 = ok.
C-----------------------------------------------------------------------
      INTEGER   TTY(2), USERS(1000), DISKS(2), VERNUM(2), IERR
C
      CHARACTER PROMPT*80, OLDDAT*8, PHFILE*48, MNAME*8, VERSON*48,
     *   LINE*80, DEFDAT*8, BLANK*4
      INTEGER   IANS, LUN, IND, IEOF, I, I1, I2
      LOGICAL   F
      INCLUDE 'INCS:DDCH.INC'
      DATA I1, I2 /1, 2/
      DATA IEOF, LUN /2, 10/
      DATA DEFDAT, MNAME, VERSON /'15JUL93', 'USERLIST', ' '/
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Loop until user gets it right.
 100  CONTINUE
C                                       How do they want user numbers
C                                       determined.
         WRITE (PROMPT,1100)
         CALL INQINT (TTY, PROMPT, I1, USERS, IERR)
         IF (IERR.LT.0) GO TO 100
         IF (IERR.GT.0) GO TO 999
C                                       Get users from a text file.
         IF (USERS(1).EQ.2) THEN
             CALL ZPHFIL ('HE', 1, 0, 0, PHFILE, IERR)
             CALL ZTOPEN (LUN, IND, 1, PHFILE, MNAME, VERSON, F, IERR)
             IF (IERR.NE.0) GO TO 999
C                                       Get range of user numbers.
         ELSE IF (USERS(1).EQ.1) THEN
 105        WRITE (PROMPT,1105) 1, USELIM
            CALL INQINT (TTY, PROMPT, I2, USERS(2), IERR)
            IF (IERR.LT.0) GO TO 105
            IF (IERR.GT.0) GO TO 999
            IF (USERS(2).LE.0) USERS(2) = 1
            IF (USERS(3).LE.0) USERS(3) = USELIM
         ELSE
            GO TO 100
            END IF
C                                       Disks.
 120     WRITE (PROMPT,1120) 1, NVOL
         CALL INQINT (TTY, PROMPT, I2, DISKS, IERR)
         IF (IERR.LT.0) GO TO 120
         IF (IERR.GT.0) GO TO 999
         IF (DISKS(1).LE.0) DISKS(1) = 1
         IF (DISKS(2).LE.0) DISKS(2) = NVOL
C                                       Oldest data.
 140     WRITE (PROMPT,1140) DEFDAT
            CALL INQSTR (TTY, PROMPT, 7, OLDDAT, IERR)
            IF (IERR.NE.0) GO TO 999
            IF (OLDDAT(1:1).EQ.' ') OLDDAT = DEFDAT
            CALL FNDDAT (TTY, OLDDAT, VERNUM, IERR)
            IF (IERR.NE.0) GO TO 140
C                                       tell choices
         BLANK = ' '
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 1, BLANK, IERR)
         IF (USERS(1).EQ.1) WRITE (PROMPT,1150) USERS(2), USERS(3)
         IF (USERS(1).EQ.2) WRITE (PROMPT,1155)
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, PROMPT, IERR)
         WRITE (PROMPT,1160) DISKS
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, PROMPT, IERR)
         WRITE (PROMPT,1170) OLDDAT
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, PROMPT, IERR)
C                                       Chance to change.
 180     WRITE (PROMPT,1180)
            CALL INQINT (TTY, PROMPT, I1, IANS, IERR)
            IF (IERR.LT.0) GO TO 180
            IF (IERR.GT.0) GO TO 999
            IF (IANS.EQ.1) GO TO 100
            IF (IANS.NE.2) GO TO 180
C                                       Read user numbers.
      IF (USERS(1).EQ.2) THEN
         IERR = 1
         DO 250 I = 4,1000
            CALL ZTREAD (LUN, IND, LINE, IERR)
            IF (IERR.EQ.IEOF) THEN
               IERR = 0
               USERS(2) = 4
               USERS(3) = I - 1
               CALL ZTCLOS (LUN, IND, IERR)
               GO TO 999
               END IF
            IF (IERR.NE.0) GO TO 970
            READ (LINE,1200,ERR=980) USERS(I)
 250        CONTINUE
C                                       Too large a help file.
         WRITE (PROMPT,1250)
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, PROMPT, IERR)
         END IF
      GO TO 999
C                                       Error reading file.
 970  WRITE (PROMPT,1970)
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, PROMPT, IERR)
      GO TO 999
C                                       Bad number in file.
 980  WRITE (PROMPT,1980)
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, PROMPT, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Enter : 1=range of users, 2=user nums in text file :')
 1105 FORMAT ('Enter user number range.(Default=',2I5,')  :')
 1120 FORMAT ('Enter AIPS disk num range (Default=',2I3,')  :')
 1140 FORMAT ('Enter oldest version date as 15MMMYY (Default= ',
     *   A8,') :')
 1150 FORMAT ('User number range : ',2I5)
 1155 FORMAT ('Get user numbers from help file "USERLIST"')
 1160 FORMAT ('Disk range        : ',2I5)
 1170 FORMAT ('Oldest data       : ',A8)
 1180 FORMAT ('Enter : 1=I made a mistake, reenter; 2=continue :')
 1200 FORMAT (I4)
 1250 FORMAT ('Too many users in the help file (Max=100)')
 1970 FORMAT ('ERROR READING HELP FILE "USERLIST"')
 1980 FORMAT ('ERROR DECODING USER NUMBER : ',A4)
      END
      SUBROUTINE DOCHNG (USERS, DISKS, VERNUM, IERR)
C-----------------------------------------------------------------------
C  Loop through all version updates for all users on all disks.
C  Inputs:
C     USERS  I(2)    If USERS(1) is 1 then USERS(2) is the first user
C                    number and USERS(3) is the last user number.  If
C                    USERS(1) is 2 then USERS(2) is 4 and USERS(3) is N
C                    and USERS(4) through USERS(N) contain the valid
C                    user numbers.
C                    open text file containing user numbers.
C     DISKS  I(2)    First and last disks.
C     VERNUM I(2)    First and last version numbers.
C     IERR   I       Error code. 0=ok.
C-----------------------------------------------------------------------
      INTEGER   USERS(1000), DISKS(2), VERNUM(2)
      INTEGER   IERR
C
      INTEGER   IVER, ISU, IEU, ISV, IEV, ISD, IED, IDISK, IUSER, I
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      ISU = USERS(2)
      IEU = USERS(3)
      ISV = VERNUM(1)
      IEV = VERNUM(2)
      ISD = DISKS(1)
      IED = DISKS(2)
      DO 400 IVER = ISV,IEV
         DO 300 IDISK = ISD,IED
            IUSER = USERS(2) - 1
            DO  200 I = ISU,IEU
               IF (USERS(1).EQ.1) IUSER = I
               IF (USERS(1).EQ.2) IUSER = USERS(I)
C                                       1990
               GO TO (400, 400, 400, 400,
C                                       1991
     *            400, 400,
C                                       1992
     *            400, 400,
C                                       1993
     *            400,
C                                       1994
     *            400, 100,
C                                       1995
     *            400), IVER
C                                       Convert 15JUL94 to 15JAN95
C                                       (ver C -> ver D)
 100              CALL JAN95 (IUSER, IDISK, IERR)
                  GO TO 200
C
 200           CONTINUE
 300        CONTINUE
 400     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE FNDDAT (TTY, OLDDAT, VERNUM, IERR)
C-----------------------------------------------------------------------
C  FNDDAT figures out what changes are needed to update a given AIPS
C  release.
C  Inputs:
C     TTY     I(2)     LUN and FTAB of open terminal.
C     OLDDAT  C*8      Old release date such as 15OCT85.
C  Output:
C     VERNUM  I(2)     Element one is the number of first update needed
C                      (15JAN90 is 1).  Element two is the number of
C                      the latest update.
C     IERR    I        Error code, 0 = ok.
C-----------------------------------------------------------------------
      INTEGER   TTY(2), VERNUM(2), IERR
      CHARACTER OLDDAT*8
C
      INTEGER   I, NUMDAT
      PARAMETER (NUMDAT=12)
      CHARACTER MSG*80, DATES(NUMDAT)*8
      DATA DATES /'15JAN90 ', '15APR90 ', '15JUL90 ', '15OCT90 ',
     *   '15JAN91 ', '15APR91 ', '15APR92 ', '15OCT92 ',
     *   '15JUL93 ', '15JAN94 ', '15JUL94 ', '15JAN95 '/
C-----------------------------------------------------------------------
C                                       Find OLDDAT in list of dates.
      VERNUM(2) = NUMDAT
      DO 100 I = 1,NUMDAT
         IF (DATES(I).EQ.OLDDAT) THEN
            VERNUM(1) = I
            IERR = 0
            GO TO 999
            END IF
 100     CONTINUE
C                                       Unknown date.
      WRITE (MSG,1100) OLDDAT
      CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSG, IERR)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('VERSION ',A8,' CANNOT BE UPDATED BY THIS PROGRAM')
      END
      SUBROUTINE JAN95 (IUSER, IDISK, IERR)
C-----------------------------------------------------------------------
C  Update 15JUL94 to 15JAN95: version C goes to D, the number base goes
C  from 16 to 36, and the file name length increases by 1.
C  Inputs:
C     IUSER  I   User number to update.
C     IDISK  I   Disk to update.
C  Output:
C     IERR   I   Error code. 0=ok.
C-----------------------------------------------------------------------
      INTEGER   IUSER, IDISK, IERR
C
      CHARACTER PHNAME*48, OLNAME*48, EXTYP*2, ATYPE*2
      REAL      CBBLKR(256), PCMAT(7,7)
      INTEGER   ICALUN, ICAIND, ICBLUN, ICBIND, CABLK(256), IMLUN, ISEQ,
     *   IMIND, LREC, IVER, CBBLK(256), IMBLK(256), I, NVER, NSEQ, NTSS,
     *   NWPL, NLPR, NMSG, ICMAX, IREC, NN, NMAX, ICNO, NSGS,
     *   NCERR, NCUPD, NEUPD
      LOGICAL   T, F
      DOUBLE PRECISION CBBLKD(128)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CBBLK, CBBLKR, CBBLKD)
      DATA ICALUN, ICBLUN, IMLUN /15, 27, 28/
      DATA T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
      CALL RFILL (49, 0.0, PCMAT)
      DO 10 I = 1,7
         PCMAT(I,I) = 1.0
 10      CONTINUE
C                                       Open old catlg file
      NLUSER = IUSER
      CALL ZPHOLV ('C', 'C', 'CA', IDISK, 0, 0, PHNAME, IERR)
      NLUSER = 1
C                                       Turn off error messages. A not
C                                       found catalog means no user.
      MSGSUP = 32000
      CALL ZOPEN (ICALUN, ICAIND, IDISK, PHNAME, F, T, T, IERR)
      MSGSUP = 0
C                                       Err 2 must be not found.
      IF (IERR.EQ.2) GO TO 200
      IF (IERR.NE.0) GO TO 980
C
      NWPL = 10
      NLPR = 256 / NWPL
      NMAX = NWPL * NLPR
C                                       Read 1st cat record.
      IREC = 1
      CALL ZFIO ('READ', ICALUN, ICAIND, IREC, CABLK, IERR)
      IF (IERR.NE.0) GO TO 980
      ICMAX = CABLK(3)
      NN = 999
      NCERR = 0
      NCUPD = 0
      NEUPD = 0
C                                       Loop over all catalog entries
      DO 170 ICNO = 1,ICMAX
         NN = NN + NWPL
C                                       Read another cat dir rec.
         IF (NN.LT.NMAX) GO TO 120
C                                       put back catalog rec
            IF (IREC.GT.1) THEN
               CALL ZFIO ('WRIT', ICALUN, ICAIND, IREC, CABLK, IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
C                                       point to next
            NN = 1
            IREC = IREC + 1
            CALL ZFIO ('READ', ICALUN, ICAIND, IREC, CABLK, IERR)
            IF (IERR.NE.0) GO TO 980
C                                       See if this slot empty/illegal.
 120     IF (CABLK(NN).EQ.IUSER) THEN
C                                       Open catalog block.
            NLUSER = CABLK(NN)
            CALL ZPHOLV ('C', 'C', 'CB', IDISK, ICNO, 1, PHNAME, IERR)
            NLUSER = 1
            MSGSUP = 32000
            CALL ZOPEN (ICBLUN, ICBIND, IDISK, PHNAME, F, T, T, IERR)
            MSGSUP = 0
            IF (IERR.NE.0) THEN
               NCERR = NCERR + 1
               IF (IERR.EQ.2) THEN
                  WRITE (MSGTXT,1120) ICNO, IUSER
                  CABLK(NN) = 0
               ELSE
                  WRITE (MSGTXT,1121) ICNO, CABLK(NN), 'OPEN'
                  END IF
               CALL MSGWRT (7)
               GO TO 170
               END IF
            CALL ZFIO ('READ', ICBLUN, ICBIND, 1, CBBLK, IERR)
            IF (IERR.NE.0) THEN
               NCERR = NCERR + 1
               IF (IERR.EQ.4) THEN
                  WRITE (MSGTXT,1120) ICNO, IUSER
                  CABLK(NN) = 0
               ELSE
                  WRITE (MSGTXT,1121) ICNO, CABLK(NN), 'READ'
                  END IF
               CALL MSGWRT (7)
               CALL ZCLOSE (ICBLUN, ICBIND, IERR)
               GO TO 170
               END IF
C                                       Update header new WCS
C            CBBLKR(KRCOK) = 32000.0
C            CBBLKD(KDLON) = 180.0D0
C            CALL DFILL (9, 0.0D0, CBBLKD(KDPRJ))
C            CALL RCOPY (49, PCMAT, CBBLKR(KRPCM))
            CALL FXHDEX (CBBLK)
            CALL ZFIO ('WRIT', ICBLUN, ICBIND, 1, CBBLK, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL ZCLOSE (ICBLUN, ICBIND, IERR)
            CALL H2CHR (2, KHPTYO, CBBLKR(KHPTY), ATYPE)
C                                       Update tables
            NLUSER = IUSER
C                                       Rename extension files
            DO 155 I = 1,KIEXTN
               NVER = MIN (255, CBBLK(KIVER+I-1))
               IF (NVER.LE.0) GO TO 155
                  CALL H2CHR (2, 1, CBBLK(KHEXT+I-1), EXTYP)
                  DO 150 IVER = 1,NVER
                     NLUSER = IUSER
                     CALL ZPHOLV ('C', 'C', EXTYP, IDISK, ICNO, IVER,
     *                  OLNAME, IERR)
                     CALL ZPHOLV ('D', 'D', EXTYP, IDISK, ICNO, IVER,
     *                  PHNAME, IERR)
                     NLUSER = 1
C                                       Rename extension file if exists.
                     MSGSUP = 32000
                     CALL ZRENAM (IDISK, OLNAME, PHNAME, IERR)
                     MSGSUP = 0
                     IF (IERR.EQ.0) NEUPD = NEUPD + 1
 150                 CONTINUE
 155           CONTINUE
C                                       Rename cat block file (header).
            NLUSER = IUSER
            CALL ZPHOLV ('C', 'C', 'CB', IDISK, ICNO, 1, OLNAME, IERR)
            CALL ZPHOLV ('D', 'D', 'CB', IDISK, ICNO, 1, PHNAME, IERR)
            NLUSER = 1
            CALL ZRENAM (IDISK, OLNAME, PHNAME, IERR)
            IF (IERR.NE.0) GO TO 170
C                                       Rename data file.
            NLUSER = IUSER
            CALL ZPHOLV ('C', 'C', ATYPE, IDISK, ICNO, 1, OLNAME, IERR)
            CALL ZPHOLV ('D', 'D', ATYPE, IDISK, ICNO, 1, PHNAME, IERR)
            NLUSER = 1
            CALL ZRENAM (IDISK, OLNAME, PHNAME, IERR)
            IF (IERR.EQ.0) NCUPD = NCUPD + 1
            END IF
 170     CONTINUE
C                                       update and close CA file
      CALL ZFIO ('WRIT', ICALUN, ICAIND, IREC, CABLK, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL ZCLOSE (ICALUN, ICAIND, IERR)
      ICAIND = 0
C                                       Rename Catalog file, this user
      NLUSER = IUSER
      CALL ZPHOLV ('D', 'D', 'CA', IDISK, 0, 0, PHNAME, IERR)
      CALL ZPHOLV ('C', 'C', 'CA', IDISK, 0, 0, OLNAME, IERR)
      NLUSER = 1
      CALL ZRENAM (IDISK, OLNAME, PHNAME, IERR)
      IF (IERR.NE.0) GO TO 980
      WRITE (MSGTXT,1175) IUSER, IDISK, NCUPD, NEUPD, NCERR
      CALL MSGWRT (7)
C-----------------------------------------------------------------------
C                                       Do system files - disk one only
 200  IF (IDISK.NE.1) GO TO 999
         NSGS = 0
         NTSS = 0
         NMSG = 0
C                                       Rename Message file.
         NLUSER = IUSER
         CALL ZPHOLV ('C', 'C', 'MS', 1, IUSER, 0, OLNAME, IERR)
         CALL ZPHOLV ('D', 'D', 'MS', 1, IUSER, 0, PHNAME, IERR)
         NLUSER = 1
         MSGSUP = 32000
         CALL ZRENAM (1, OLNAME, PHNAME, IERR)
         MSGSUP = 0
         IF (IERR.EQ.0) NMSG = 1
C                                       Rename Save/Get file.
         NLUSER = IUSER
         CALL ZPHOLV ('C', 'C', 'SG', 1, IUSER, 0, OLNAME, IERR)
         NLUSER = 1
         MSGSUP = 32000
         CALL ZOPEN (IMLUN, IMIND, 1, OLNAME, F, T, T, IERR)
         MSGSUP = 0
         IF (IERR.NE.0) GO TO 300
         CALL ZFIO ('READ', IMLUN, IMIND, 1, IMBLK, IERR)
         IF (IERR.NE.0) GO TO 300
            LREC = 1
            NSEQ = IMBLK(1)
            IF (NSEQ.LE.0) GO TO 300
               NWPL = 7
               NLPR = 256 / NWPL
               DO 220 ISEQ = 1,NSEQ
                  IREC = ISEQ/NLPR + 1
                  IF (IREC.EQ.LREC) GO TO 210
                     LREC = IREC
                     CALL ZFIO ('READ', IMLUN, IMIND, IREC, IMBLK, IERR)
                     IF (IERR.NE.0) GO TO 240
 210              IREC = MOD (ISEQ, NLPR) * NWPL + 1
                  IF (IMBLK(IREC).LE.0) GO TO 220
                     NLUSER = IUSER
                     CALL ZPHOLV ('C', 'C', 'SG', IDISK, IUSER, ISEQ,
     *                  OLNAME, IERR)
                     CALL ZPHOLV ('D', 'D', 'SG', IDISK, IUSER, ISEQ,
     *                  PHNAME, IERR)
                     NLUSER = 1
                     CALL ZRENAM (IDISK, OLNAME, PHNAME, IERR)
                     IF (IERR.EQ.0) NSGS = NSGS + 1
 220              CONTINUE
 230        CALL ZCLOSE (IMLUN, IMIND, IERR)
            NLUSER = IUSER
            CALL ZPHOLV ('C', 'C', 'SG', IDISK, IUSER, 0, OLNAME, IERR)
            CALL ZPHOLV ('D', 'D', 'SG', IDISK, IUSER, 0, PHNAME, IERR)
            NLUSER = 1
            CALL ZRENAM (IDISK, OLNAME, PHNAME, IERR)
            IF (IERR.EQ.0) NSGS = NSGS + 1
            GO TO 300
 240        CALL ZCLOSE (IMLUN, IMIND, IERR)
 300     CONTINUE
C                                       Rename TGet file.
 350     CONTINUE
         NLUSER = IUSER
         CALL ZPHOLV ('C', 'C', 'TS', 1, IUSER, 0, OLNAME, IERR)
         CALL ZPHOLV ('D', 'D', 'TS', 1, IUSER, 0, PHNAME, IERR)
         NLUSER = 1
         MSGSUP = 32000
         CALL ZRENAM (1, OLNAME, PHNAME, IERR)
         MSGSUP = 0
         IF (IERR.EQ.0) NTSS = 1
 360     IF (NSGS+NTSS+NMSG.LE.0) GO TO 370
            WRITE (MSGTXT,1360) IUSER, NMSG, NSGS, NTSS
            CALL MSGWRT (2)
 370     CONTINUE
      GO TO 999
C                                       Serious error.
 980  IF (ICAIND.GT.0) CALL ZCLOSE (ICALUN, ICAIND, IERR)
      WRITE (MSGTXT,1980) IUSER, IDISK
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1120 FORMAT ('FILE FOR CATALOG SLOT ',I5,' USER',I5,
     *   ' IS MISSING OR EMPTY')
 1121 FORMAT ('ERROR UPDATING SLOT ',I5,' USER',I5,' OPER= ',A)
 1175 FORMAT ('Updated user',I5,' disk',I3,' with',I5,' files,',I5,
     *   ' exts,',I3,' errors')
 1360 FORMAT ('For user',I5,' renamed',I2,' message,',I4,' save/get,',
     *   I2,' tget files')
 1980 FORMAT ('SERIOUS ERROR CONVERTING USER',I5,' ON DISK',I3)
      END
