      PROGRAM REUSE
C-----------------------------------------------------------------------
C! REUSE converts from public to private catalog files
C# Utility Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 2005, 2015
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
C   A service routine to convert an installation from private to public
C   catalog files.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, PHNAME*48
      INTEGER   TTY(2), I, IERR, SCRTCH(256), ISIZE, IVOL, USERS(100),
     *   MUSER
      LOGICAL   WUPS, DOSG
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA TTY /5,0/, PRGNAM /'REUSE '/
C-----------------------------------------------------------------------
C                                       Standard AIPS inits.
      CALL AIPINI (TTY, PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Does password file exist
      CALL ZPHFIL ('PW', 1, 0, 0, PHNAME, IERR)
      CALL ZEXIST (1, PHNAME, ISIZE, IERR)
C                                       Ask for password
      IF (IERR.EQ.0) THEN
         CALL PASWRD (SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      WUPS = .FALSE.
      DOSG = .FALSE.
      MUSER = 0
C                                       appropriate?
      IF (UCTSIZ.GT.0) THEN
         WUPS = .TRUE.
         MSGTXT = 'SP FILE SAYS THAT CATALOGS ARE ALREADY PRIVATE'
         CALL MSGWRT (8)
         MSGTXT = 'THIS PROGRAM IS PROBABLY NOT APPROPRIATE'
         CALL MSGWRT (8)
         MSGTXT = 'I WILL RUN ANYWAY IN CASE THERE ARE PUBLIC CATALOGS'
         CALL MSGWRT (8)
         END IF
      DO 100 I = 1,32000
 10      WRITE (MSGTXT,1010)
         CALL INQINT (TTY, MSGTXT, 1, IVOL, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.GT.0) GO TO 900
         IF (IVOL.LE.0) GO TO 900
         CALL REBILD (IVOL, MUSER, USERS, IERR)
         IF (IERR.GT.0) GO TO 990
         IF (IERR.LT.0) WUPS = .TRUE.
         IF (IVOL.EQ.1) DOSG = .TRUE.
 100     CONTINUE
C                                       Save get files
 900  IF (DOSG) CALL RESQ (MUSER, USERS)
C                                       reset the SP file
      IF ((I.GT.1) .AND. (.NOT.WUPS)) CALL RESP (TTY, SCRTCH)
C
 990  CALL ACOUNT (2)
C
 999  STOP
C-----------------------------------------------------------------------
 1010 FORMAT ('Enter disk volume number of catalog: = 0 -> quit')
      END
      SUBROUTINE REBILD (IVOL, MUSER, USERS, IRET)
C-----------------------------------------------------------------------
C  Rebuild a catalog from catalog block files.
C  Input:
C     IVOL    I      Disk volume number.
C  Output:
C     MUSER   I      Number users found
C     USERS   I(*)   Up to 100 user numbers found
C     IRET    I      error code
C-----------------------------------------------------------------------
      INTEGER   IVOL, USERS(100), MUSER, IRET
C
      CHARACTER PNAME*48,  CTEMP*18, CNAME*48
      INTEGER   IUSER, CNO, IERR, MAXSIZ, BUFFER(256), LUNCB, INDCB,
     *   ISAVE, NCHAR, UCTSAV, CLUN,CIND, CMAX, CBUF(256), I, CREC,
     *   NWPL, NLPR, CMOD, NN
      LOGICAL   T, F, WUPS
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T /.TRUE./, F /.FALSE./, CLUN, LUNCB /16,17/
C-----------------------------------------------------------------------
      UCTSAV = UCTSIZ
C                                       open public catalog
      UCTSIZ = 0
      CALL ZPHFIL ('CA', IVOL, 0, 0, CNAME, IERR)
      CALL ZOPEN (CLUN, CIND, IVOL, CNAME, F, T, T, IERR)
      IF (IERR.EQ.2) THEN
         IRET = -1
         MSGTXT = 'PUBLIC CATALOG NOT FOUND'
         GO TO 990
      ELSE IF (IERR.NE.0) THEN
         IRET = IERR
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       catalog size
      CREC = 1
      CALL ZFIO ('READ', CLUN, CIND, CREC, CBUF, IERR)
      IF (IERR.NE.0) THEN
         IRET = -2
         GO TO 980
         END IF
      CMAX = CBUF(3)
      NWPL = 10
      NLPR = 256 / NWPL
C                                       Loop over input catalog
      DO 100 CNO = 1,CMAX
         CMOD = (CNO - 1) / NLPR
         I = 2 + CMOD
         NN = 1 + NWPL * (CNO - NLPR*CMOD - 1)
         IF (I.NE.CREC) THEN
            CALL ZFIO ('WRIT', CLUN, CIND, CREC, CBUF, IERR)
            IF (IERR.NE.0) THEN
               IRET = -3
               GO TO 980
               END IF
            CREC = I
            CALL ZFIO ('READ', CLUN, CIND, CREC, CBUF, IERR)
            IF (IERR.NE.0) THEN
               IRET = -2
               GO TO 980
               END IF
            END IF
C                                       there is an entry
         IF (CBUF(NN).GT.0) THEN
            IUSER = CBUF(NN)
            MAXSIZ = CMAX
C                                       list all users
            DO 10 I = 1,MUSER
               IF (IUSER.EQ.USERS(I)) GO TO 20
 10            CONTINUE
            MUSER = MUSER + 1
            USERS(MUSER) = IUSER
C                                       read header
 20         ISAVE = NLUSER
            NLUSER = IUSER
            CALL ZPHFIL ('CB', IVOL, CNO, 1, PNAME, IERR)
            NLUSER = ISAVE
            MSGSUP = 32000
            CALL ZOPEN (LUNCB, INDCB, IVOL, PNAME, F, T, T, IERR)
            MSGSUP = 0
            IF (IERR.EQ.2) THEN
               MSGTXT = ' '
            ELSE IF (IERR.EQ.0) THEN
C                                       Insert Catalog block in catalog.
               CALL ZFIO ('READ', LUNCB, INDCB, 1, CATBLK, IERR)
               CALL ZCLOSE (LUNCB, INDCB, I)
C                                       Check if the CB file is empty
C                                       If so, skip and warn the user
               IF (IERR.EQ.4) THEN
                  WRITE (MSGTXT,1010) CNO
                  WUPS = T
C                                       Also skip if other error occurs
               ELSE IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1020) IERR, CNO
                  WUPS = T
C                                       The coast SHOULD be clear...
               ELSE
C                                       Create user catalog file
C                                       make entry in the catalog
                  CALL MAKCAT (CNO, IVOL, MAXSIZ, CBUF(NN), BUFFER,
     *               IRET)
                  IF (IRET.NE.0) GO TO 980
C                                       rename the files
                  CALL NAMEIT (IUSER, IVOL, CNO, CBUF(NN), IERR)
                  WRITE (MSGTXT,1000) CNO, IVOL, IUSER
                  CALL H2CHR (18, 1, CATH(KHIMN), CTEMP)
                  CALL NAMEST (CTEMP, CATBLK(KIIMS), MSGTXT(5:24),
     *               NCHAR)
                  END IF
            ELSE
               WUPS = T
               END IF
            IF (MSGTXT.NE.' ') CALL MSGWRT (5)
            END IF
 100     CONTINUE
      IF (WUPS) THEN
         WRITE (MSGTXT,1100)
         CALL MSGWRT (6)
         END IF
C                                       close downs
 980  UCTSIZ = UCTSAV
      CALL ZCLOSE (CLUN, CIND, IERR)
      GO TO 999
 990  UCTSIZ = UCTSAV
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Put',22X,'in slot ',I4,' disk',I3,' user',I5)
 1010 FORMAT ('Catalog Block file for slot ',I4,' is EMPTY!  Skipping.')
 1020 FORMAT ('Error ',I4,' reading CB file for slot ',I4,'! Skipping.')
 1100 FORMAT ('WARNING!!  AT LEAST ONE FILE MAY NEED DELETING BY HAND')
      END
      SUBROUTINE MAKCAT (CNO, IVOL, MAXSIZ, CATL, BUFFER, IERR)
C-----------------------------------------------------------------------
C  Create a new catalog file for a user if neeeded, then add entry to it
C  Inputs:
C     CNO      I       Catalog number
C     IVOL     I       Disk Volume.
C     MAXSIZ   I       Size of catalog in slots.
C     CATL     I(10)   Catalog entry data
C  Output:
C     BUFFER   I(256)  Work Buffer.
C     IERR     I       Error code, 0=ok.
C-----------------------------------------------------------------------
      INTEGER   CNO, IVOL, MAXSIZ, CATL(*), BUFFER(*), IERR
C
      CHARACTER PNAME*48
      INTEGER   ISIZE, ISAVE, NWPL, NLPR, IREC, CLUN, CIND, NN, IUSER
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA CLUN /18/
C-----------------------------------------------------------------------
      ISAVE = NLUSER
      IUSER = CATL(1)
      NLUSER = IUSER
      UCTSIZ = 100
      CALL ZPHFIL ('CA', IVOL, 0, 0, PNAME, IERR)
      NLUSER = ISAVE
      UCTSIZ = 0
      CALL ZEXIST (IVOL, PNAME, ISIZE, IERR)
      IF (IERR.GT.1) THEN
         WRITE (1000,MSGTXT) 'ZEXIST', IERR, IUSER
         CALL MSGWRT (8)
         GO TO 999
C                                       create the catalog
      ELSE IF (IERR.EQ.1) THEN
         NLUSER = IUSER
         UCTSIZ = 100
         CALL CATCR (IVOL, MAXSIZ, BUFFER, IERR)
         UCTSIZ = 0
         NLUSER = ISAVE
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) 'CATCR', IERR, IUSER
            CALL MSGWRT (8)
            GO TO 999
            END IF
         END IF
C                                       add record to it
      NWPL = 10
      NLPR = 256 / NWPL
      IREC = 2 + (CNO - 1) / NLPR
      NN = 1 + NWPL * (CNO - 1 - NLPR * (IREC-2))
      NLUSER = IUSER
      UCTSIZ = 100
      CALL ZPHFIL ('CA', IVOL, 0, 0, PNAME, IERR)
      UCTSIZ = 0
      NLUSER = ISAVE
      CALL ZOPEN (CLUN, CIND, IVOL, PNAME, .FALSE., .TRUE., .TRUE.,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) 'ZOPEN', IERR, IUSER
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL ZFIO ('READ', CLUN, CIND, IREC, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) 'READ', IERR, IUSER
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL COPY (NWPL, CATL, BUFFER(NN))
      CALL ZFIO ('WRIT', CLUN, CIND, IREC, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) 'WRIT', IERR, IUSER
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL ZCLOSE (CLUN, CIND, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MAKCAT: ',A,' ERROR',I3,' USER',I5)
      END
      SUBROUTINE NAMEIT (IUSER, IVOL, CNO, CATL, IRET)
C-----------------------------------------------------------------------
C   Put an entry in a catalog directory using information in a
C   catalog block found in common /MAPHDR/.
C   Inputs:
C      IUSER   I        User number.
C      IVOL    I        Disk volume containing catalog (1 rel)
C      CNO     I        Slot number of interest
C      CATL    I(10)    Catalog entry
C   Outputs:
C      IRET    I        Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IUSER, IVOL, CNO, CATL(*), IRET
C
      CHARACTER  INAME*48, ONAME*48, CTYPE*2
      INTEGER   NNAME, IERR, I, J, NV, ISAVE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'DDCH.INC'
      DATA NNAME /6/
C-----------------------------------------------------------------------
      ISAVE = NLUSER
C                                       main file
      CALL H2CHR (2, 19, CATL(NNAME), CTYPE)
      UCTSIZ = 0
      NLUSER = IUSER
      CALL ZPHFIL (CTYPE, IVOL, CNO, 1, INAME, IERR)
      UCTSIZ = 100
      CALL ZPHFIL (CTYPE, IVOL, CNO, 1, ONAME, IERR)
      UCTSIZ = 0
      NLUSER = ISAVE
      CALL ZRENAM (IVOL, INAME, ONAME, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       header file
      CTYPE = 'CB'
      NLUSER = IUSER
      CALL ZPHFIL (CTYPE, IVOL, CNO, 1, INAME, IERR)
      UCTSIZ = 100
      CALL ZPHFIL (CTYPE, IVOL, CNO, 1, ONAME, IERR)
      UCTSIZ = 0
      NLUSER = ISAVE
      CALL ZRENAM (IVOL, INAME, ONAME, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       extensions
      CALL FXHDEX (CATBLK)
      DO 20 I = 1,KIEXTN
         CALL H2CHR (2, 1, CATH(KHEXT+I-1), CTYPE)
         NV = CATBLK(KIVER+I-1)
         IF (NV.GT.0) THEN
            IF ((CTYPE(1:1).GE.'A') .AND. (CTYPE(1:1).LE.'Z') .AND.
     *         (CTYPE(2:2).GE.'A') .AND. (CTYPE(2:2).LE.'Z')) THEN
               DO 10 J = 1,NV
                  NLUSER = IUSER
                  CALL ZPHFIL (CTYPE, IVOL, CNO, J, INAME, IERR)
                  UCTSIZ = 100
                  CALL ZPHFIL (CTYPE, IVOL, CNO, J, ONAME, IERR)
                  UCTSIZ = 0
                  NLUSER = ISAVE
                  CALL ZRENAM (IVOL, INAME, ONAME, IERR)
 10               CONTINUE
               END IF
            END IF
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RESQ (MUSER, USERS)
C-----------------------------------------------------------------------
C   Renames Save/Get files
C   Inputs:
C      MUSER   I      Number entries in USERS
C      USERS   I(*)   User numbers to fix
C-----------------------------------------------------------------------
      INTEGER   MUSER, USERS(*)
C
      INTEGER   I, ISAVE, ISIZE, IUSER, IERR, NC, J, IVOL
      CHARACTER INAME*48, ONAME*48, CTYPE*2
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      ISAVE = NLUSER
      CTYPE = 'SG'
      IVOL = 1
C                                       Loop over users
      DO 100 I = 1,MUSER
         IUSER = USERS(I)
         UCTSIZ = 0
C                                       does S/G catalog exist?
         NLUSER = IUSER
         CALL ZPHFIL (CTYPE, IVOL, IUSER, 0, INAME, IERR)
         NLUSER = ISAVE
         CALL ZEXIST (IVOL, INAME, ISIZE, IERR)
         IF (IERR.EQ.0) THEN
C                                       rename catalog
            UCTSIZ = 100
            NLUSER = IUSER
            CALL ZPHFIL (CTYPE, IVOL, IUSER, 0, ONAME, IERR)
            UCTSIZ = 0
            NLUSER = ISAVE
            CALL ZRENAM (IVOL, INAME, ONAME, IERR)
            IF (IERR.NE.0) GO TO 100
            NC = 1
C                                       look for more
            DO 30 J = 1,100
C                                       does S/G catalog exist?
               NLUSER = IUSER
               CALL ZPHFIL (CTYPE, IVOL, IUSER, J, INAME, IERR)
               NLUSER = ISAVE
               CALL ZEXIST (IVOL, INAME, ISIZE, IERR)
               IF (IERR.EQ.0) THEN
C                                       rename catalog
                  UCTSIZ = 100
                  NLUSER = IUSER
                  CALL ZPHFIL (CTYPE, IVOL, IUSER, J, ONAME, IERR)
                  UCTSIZ = 0
                  NLUSER = ISAVE
                  CALL ZRENAM (IVOL, INAME, ONAME, IERR)
                  IF (IERR.NE.0) GO TO 100
                  NC = NC + 1
                  END IF
 30            CONTINUE
            IF (NC.GT.0) THEN
               WRITE (MSGTXT,1030) NC, IUSER
               CALL MSGWRT (4)
               END IF
            END IF
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('Renamed',I4,' SAVE/GET files for user',I5)
      END
      SUBROUTINE RESP (TTY, BUFF)
C-----------------------------------------------------------------------
C   RESP will ask the user about changing the SP file to private
C   catalogs and do so if the answer is yes
C   Inputs:
C      TTY   I(2)   Lun and IND for user query terminal
C-----------------------------------------------------------------------
      INTEGER   TTY(2), BUFF(*)
C
      CHARACTER ANSWER*12, PHNAME*48
      INTEGER   LUN, IND, IERR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN /20/
C-----------------------------------------------------------------------
      MSGTXT = 'Shall I update the SP file to private catalogs (Y/N)?'
      CALL INQSTR (TTY, MSGTXT, 12, ANSWER, IERR)
      IF (IERR.NE.0) GO TO 999
      IF ((INDEX(ANSWER,'Y').GT.0) .OR. (INDEX(ANSWER,'y').GT.0)) THEN
         UCTSIZ = 0
         CALL ZPHFIL ('SP', 1, 0, 0, PHNAME, IERR)
         CALL ZOPEN (LUN, IND, 1, PHNAME, .FALSE., .TRUE., .TRUE., IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) 'OPEN', IERR
            CALL MSGWRT (8)
         ELSE
            CALL ZFIO ('READ', LUN, IND, 1, BUFF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) 'READ', IERR
               CALL MSGWRT (8)
            ELSE
               BUFF(66) = 100
               CALL ZFIO ('WRIT', LUN, IND, 1, BUFF, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) 'WRIT', IERR
                  CALL MSGWRT (8)
                  END IF
               END IF
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RESP ATTEMPTING ',A,' GETS ERROR',I5)
      END
