      PROGRAM RECAT
C-----------------------------------------------------------------------
C! RECAT recovers entries in map catalogs.
C# Utility Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2004, 2007
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
C   A service routine to recover entries in map catalogs.  It uses a
C   user's Catalog Block files to rebuild a CAtalog directory.
C   Any existing catalog is destroyed.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   TTY(2), I, IUSER, IVOL, RANGE(2), IERR, NPARM,
     *   BUFFER(256)
      LOGICAL   RQUICK
      REAL      XXX
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA TTY /5,0/, PRGNAM /'RECAT '/
C-----------------------------------------------------------------------
C                                       Standard AIPS inits.
      CALL AIPINI (TTY, PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       check for running as task
C                                       not allowed!!
      NPARM = 1
      CALL GTPARM (PRGNAM, NPARM, RQUICK, XXX, BUFFER, IERR)
      IF (IERR.EQ.0) THEN
         I = 8
         CALL RELPOP (I, BUFFER, IERR)
         MSGTXT = 'YOU HAVE INVOKED THE STAND-ALONE PROGRAM RECAT AS A'
     *      // ' TASK'
         CALL MSGWRT (9)
         MSGTXT = 'THIS IS NOT ALLOWED.'
         CALL MSGWRT (9)
         MSGTXT = 'TO RUN THE *VERB* RECAT SIMPLY SAY RECAT WITHOUT A'
     *      // ' GO'
         CALL MSGWRT (9)
         GO TO 990
         END IF
C                                       continue as intended
      NPOPS = 1
      DO 100 I = 1,32000
         CALL GETUSR (TTY, IUSER, IVOL, RANGE)
         IF (IUSER.EQ.0) GO TO 990
         CALL REBILD (TTY, IUSER, IVOL, RANGE)
 100     CONTINUE
C
 990  CALL ACOUNT (2)
C
 999  STOP
      END
      SUBROUTINE GETUSR (TTY, IUSER, IVOL, RANGE)
C-----------------------------------------------------------------------
C   Get a user number, a disk volume, and a range of catalog blocks.
C   Input:
C      TTY     I(2)   LUN and FTAB index of open TTY.
C   Output:
C      IUSER   I      User number. 0 means quit.
C      IVOL    I      Volume number.
C      RANGE   I(2)   Range of catalog slot numbers.
C-----------------------------------------------------------------------
      INTEGER   TTY(2), IUSER, IVOL, RANGE(2), IANS, IERR
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
 10   CONTINUE
         IUSER = 0
 15      WRITE (MSGTXT,1010)
         CALL INQINT (TTY, MSGTXT, 1, IUSER, IERR)
         IF ((IERR.NE.0) .OR. (IUSER.EQ.0)) GO TO 900
 20      WRITE (MSGTXT,1020)
         CALL INQINT (TTY, MSGTXT, 1, IVOL, IERR)
         IF (IERR.LT.0) GO TO 20
         IF (IERR.GT.0) GO TO 900
 25      WRITE (MSGTXT,1025) 1, UCTSIZ
         CALL INQINT (TTY, MSGTXT, 2, RANGE, IERR)
         IF (IERR.LT.0) GO TO 25
         IF (IERR.GT.0) GO TO 900
         IF (RANGE(1).EQ.0) RANGE(1) = 1
         IF (RANGE(2).EQ.0) RANGE(2) = UCTSIZ
 30      WRITE (MSGTXT,1030)
         CALL INQINT (TTY, MSGTXT, 1, IANS, IERR)
         IF (IERR.LT.0) GO TO 30
         IF (IERR.GT.0) GO TO 900
         IF (IANS.NE.2) GO TO 10
C
 900  IF (IERR.NE.0) IUSER = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Enter user number to rebuild catalog (0 or Q => quit)')
 1020 FORMAT ('Enter disk volume number of catalog')
 1025 FORMAT ('Enter range of catalog slots (e.g. ',I2,I4,')')
 1030 FORMAT ('Enter: 1=I need to re-enter, 2=continue')
      END
      SUBROUTINE REBILD (TTY, IUSER, IVOL, RANGE)
C-----------------------------------------------------------------------
C  Rebuild a catalog from catalog block files.
C  Input:
C     TTY     I(2)   LUN and FTAB index of open terminal device.
C     IUSER   I      User number.
C     IVOL    I      Disk volume number of bad catalog.
C     RANGE   I(2)   Range of catalog slots to check for existence of
C                    catalog blocks.
C-----------------------------------------------------------------------
      CHARACTER  PNAME*48,  CTEMP*18
      INTEGER   TTY(2), IUSER, IVOL, RANGE(2), ISLOT, IR0, IR1,
     *   IERR, MAXSIZ, BUFFER(256), LUNCB, INDCB, ISAVE,
     *   NCHAR
      LOGICAL   T, F, WUPS
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T /.TRUE./, F /.FALSE./, LUNCB /15/
C-----------------------------------------------------------------------
C                                       Create or replace catalog file.
      MAXSIZ = MAX (RANGE(2), UCTSIZ)
      CALL MAKCAT (TTY, IUSER, IVOL, MAXSIZ, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 999
      IR0 = RANGE(1)
      IR1 = RANGE(2)
      WUPS = F
      DO 100 ISLOT = IR0, IR1
C                                       Check on existence of CB.
         ISAVE = NLUSER
         NLUSER = IUSER
         CALL ZPHFIL ('CB', IVOL, ISLOT, 1, PNAME, IERR)
         NLUSER = ISAVE
         MSGSUP = 32000
         CALL ZOPEN (LUNCB, INDCB, IVOL, PNAME, F, T, T, IERR)
         MSGSUP = 0
         IF (IERR.EQ.2) THEN
            MSGTXT = ' '
         ELSE IF (IERR.EQ.0) THEN
C                                       Insert Catalog block in catalog.
            CALL ZFIO ('READ', LUNCB, INDCB, 1, CATBLK, IERR)
C                                       Check if the CB file is empty
C                                       If so, skip and warn the user
            IF (IERR.EQ.4) THEN
               WRITE (MSGTXT, 1010) ISLOT
               CALL ZCLOSE (LUNCB, INDCB, IERR)
               WUPS = T
C                                       Also skip if other error occurs
            ELSE IF (IERR.NE.0) THEN
               WRITE (MSGTXT, 1020) IERR, ISLOT
               CALL ZCLOSE (LUNCB, INDCB, IERR)
               WUPS = T
C                                       The coast SHOULD be clear...
            ELSE
               CALL ZCLOSE (LUNCB, INDCB, IERR)
               CALL CATINS (IUSER, IVOL, ISLOT, BUFFER, IERR)
               WRITE (MSGTXT,1000) ISLOT
               CALL H2CHR (18, 1, CATH(KHIMN), CTEMP)
               CALL NAMEST (CTEMP, CATBLK(KIIMS), MSGTXT(5:24), NCHAR)
               END IF
         ELSE
            WUPS = T
            END IF
         IF (MSGTXT.NE.' ') CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72,
     *      MSGTXT, IERR)
C
 100     CONTINUE
      IF (WUPS) THEN
         WRITE (MSGTXT, 1015)
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGTXT, IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Put                       in slot ',I4)
 1010 FORMAT ('Catalog Block file for slot ',I4,' is EMPTY!  Skipping.')
 1015 FORMAT ('WARNING!!  AT LEAST ONE FILE MAY NEED DELETED BY HAND!')
 1020 FORMAT ('Error ',I4,' reading CB file for slot ',I4,'! Skipping.')
      END
      SUBROUTINE MAKCAT (TTY, IUSER, IVOL, MAXSIZ, BUFFER, IERR)
C-----------------------------------------------------------------------
C  Create a new catalog file for a user.  Destroy any existing catalog.
C  Inputs:
C     TTY     I(2)     LUN and FTAB index for open terminal.
C     IUSER   I        User number.
C     IVOL    I        Disk Volume.
C     MAXSIZ  I        Size of catalog in slots.
C  Output:
C     BUFFER  I        Work Buffer.
C     IERR    I        Error code, 0=ok.
C-----------------------------------------------------------------------
      CHARACTER PNAME*48
      INTEGER   TTY(2), IUSER, IVOL, MAXSIZ, BUFFER(256), IERR, ISIZE,
     *   ISAVE
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      ISAVE = NLUSER
      NLUSER = IUSER
      CALL ZPHFIL ('CA', IVOL, 0, 0, PNAME, IERR)
      NLUSER = ISAVE
      CALL ZEXIST (IVOL, PNAME, ISIZE, IERR)
      IF (IERR.NE.0) GO TO 100
         WRITE (MSGTXT,1000) IUSER
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGTXT, IERR)
         CALL ZDESTR (IVOL, PNAME, IERR)
         IF (IERR.NE.0) GO TO 999
 100  CONTINUE
      ISAVE = NLUSER
      NLUSER = IUSER
      CALL CATCR (IVOL, MAXSIZ, BUFFER, IERR)
      NLUSER = ISAVE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Destroying existing catalog for user',I5)
      END
      SUBROUTINE CATINS (IUSER, IVOL, CNO, BUFF, IERR)
C-----------------------------------------------------------------------
C   Put an entry in a catalog directory using information in a
C   catalog block found in common /MAPHDR/.
C   Inputs:  IUSER  I        User number.
C            IVOL   I        Disk volume containing catalog (1 rel)
C            CNO    I        Slot number of interest
C                            change of status is desired
C            BUFF   I(256)   Working buffer
C            IERR   I        Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER   CTEMP*20
      INTEGER   IUSER, IVOL, CNO, BUFF(256), IERR, NREC, IER, CMAX,
     *   CMOD, NIND, NSEQ, NNAME, CIND, NSTAT, CLUN, INERR, RDERR, NWPL,
     *   ISAVE, NLPR, NTIME, IT(6)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA CLUN /15/
      DATA NSTAT, NTIME, NSEQ, NNAME /1,2,4,5/
      DATA INERR, RDERR /2, 3/
C-----------------------------------------------------------------------
      ISAVE = NLUSER
      NLUSER = IUSER
      CALL CATOPN (IVOL, CIND, BUFF, CMAX, IERR)
      NLUSER = ISAVE
      IF (IERR.EQ.3) GO TO 980
      IF (IERR.NE.0) GO TO 999
C                                       check validity of CNO
      IF ((0.LT.CNO) .AND. (CMAX.GE.CNO)) GO TO 20
         IERR = INERR
         WRITE (MSGTXT,1015) CNO
         GO TO 970
C                                       get directory block
 20   NWPL = 10
      NLPR = 256 / NWPL
      CMOD = (CNO - 1)/NLPR
      NREC = 2 + CMOD
      NIND = 1 + NWPL*(CNO - NLPR*CMOD -1 )
      CALL ZFIO ('READ', CLUN, CIND, NREC, BUFF, IER)
      IF (IER.EQ.0) GO TO 25
         IERR = RDERR
         GO TO 980
 25   CONTINUE
C                                       Add fields from CATBLK
      BUFF(NIND) = IUSER
      BUFF(NIND+NSTAT) = 0
      IF (CATBLK(KIIMS).LE.0) CATBLK(KIIMS) = 1000 + CNO
      BUFF(NIND+NSEQ) = CATBLK(KIIMS)
      CALL H2CHR (20, 1, CATH(KHIMN), CTEMP)
      CALL CHR2H (20, CTEMP, 1, BUFF(NIND+NNAME))
C                                       return directory to disk
C                                       with latest time
 50   CALL CATIME (1, BUFF(NIND+NTIME), IT)
      CALL ZFIO ('WRIT', CLUN, CIND, NREC, BUFF, IER)
      CALL ZCLOSE (CLUN, CIND, IER)
      GO TO 999
C                                       error message out
 970  CALL MSGWRT (6)
C                                       close & free catalog file
 980  CALL ZCLOSE (CLUN, CIND, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1015 FORMAT ('CATIO: Catno ',I6,' out of range')
      END
