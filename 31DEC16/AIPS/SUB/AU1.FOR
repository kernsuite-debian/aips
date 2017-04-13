      SUBROUTINE AU1 (BRANCH)
C-----------------------------------------------------------------------
C! prints and clears the message file, sets up for EXIT and RESTART
C# POPS-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2016
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
C   AU1 prints and compresses the message file.  This routine is
C   called from pops by using the verb PRTMSG.
C   BRANCH = 1  Normal PRTMSG
C            2  CLRMSG (older than TIMEMS days) followed by EXIT
C            3  CLRMSG (older than TIMEMS days) followed by RESTART
C            4  CLRMSG normal
C            5  Set printer
C            6  CLRMSG, Kill servers, EXIT
C   On EXIT and RESTART, deletes any empty, user-owned catalog files
C   and sets ERRNUM appropriately to cause the main routine to react.
C   INPUTS:  (through adverbs in common)
C      PRIORITY  Messages printed will have a priority >= PRIORITY
C      PRTIME    Messages printed more recent than PRTIME days, deleted
C                older than PRTIME days
C      PRNUMBER  Messages printed for this POPS number (< 0 => all)
C      PRTASK    Messages printed/deleted only for tasks whose names
C                match whatever chars are in PRTASK
C            (through the stack)
C      PRINTER   Printer number
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER ITASK*8, PHNAME*48, PRGNAM*6, CDUM*1
      INTEGER   PRIOR, IVOL, IFIND, IMAX, NDONE, NLEFT, IERR, IBLK(256),
     *   ICUR, IMOD, NN, ICNO, NLPR, NWPL, ICLUN, LVOL, IREC, IDUM, IPRT
      REAL       TIMSG, ONCRT, RDUM
      LOGICAL   IAMOK
      COMMON /AIPSCR/ IBLK
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DPOP.INC'
      DATA PRGNAM /'AU1   '/
      DATA ICLUN /15/
      DATA IPRT /0/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.6)) GO TO 999
C                                       get adverbs
      CALL ADVERB ('PRIORITY', 'I', 1, 0, PRIOR, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('PRNUMBER', 'I', 1, 0, ICNO, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('PRTIME', 'R', 1, 0, IDUM, TIMSG, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('DOCRT', 'R', 1, 0, IDUM, ONCRT, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
C                                       PRTASK changed size
      CALL ADVERB ('PRTASK', 'C', 1, 8, IDUM, RDUM, ITASK)
      IF (ERRNUM.NE.0) THEN
         ERRNUM = 0
         CALL ADVERB ('PRTASK', 'C', 1, 5, IDUM, RDUM, ITASK)
         IF (ERRNUM.NE.0) GO TO 980
         END IF
C
      GO TO (100, 200, 200, 400, 500, 200), BRANCH
C-----------------------------------------------------------------------
C                                       PRTMSG
C                                       print message file
C-----------------------------------------------------------------------
C                                       Get adverbs.
 100  CALL ADVERB ('OUTPRINT', 'C', 1, 48, IDUM, RDUM, PHNAME)
      IF (ERRNUM.NE.0) GO TO 980
      IMAX = NLUSER
      IF (ICNO.EQ.0) ICNO = NPOPS
      CALL PRTMSG ('PRIN', IMAX, ICNO, PRIOR, ITASK, TIMSG, ONCRT,
     *   PHNAME, NDONE, NLEFT, IERR)
      IF (IERR.EQ.0) THEN
         WRITE (MSGTXT,1100) NDONE
         CALL MSGWRT (1)
         END IF
      GO TO 980
C-----------------------------------------------------------------------
C                                       EXIT, RESTART
C                                       clear old messages, set exit
C-----------------------------------------------------------------------
C                                       Delete scratch files
 200  CALL DESCR (IBLK)
C                                       Check user catalogs
      IF ((UCTSIZ.GT.0) .AND. (NPOPS.LE.NINTRN) .AND.
     *   (TSKNAM(:5).NE.'AIPSC')) THEN
         NWPL = 10
         NLPR = 256 / NWPL
         DO 240 IVOL = 1,NVOL
            IF (.NOT.IAMOK(IVOL,'CA')) GO TO 240
            LVOL = -IVOL
            CALL CATOPN (LVOL, IFIND, IBLK, IMAX, IERR)
            IF (IERR.NE.0) GO TO 240
            ICUR = 999
            IREC = 1
            DO 220 ICNO = 1,IMAX
               IMOD = (ICNO-1) / NLPR
               NN = 1 + NWPL * (ICNO - NLPR*IMOD - 1)
               ICUR = ICUR + NWPL
C                                       Read next directory record
               IF (ICUR.GE.NWPL*NLPR) THEN
                  IREC = IREC + 1
                  CALL ZFIO ('READ', ICLUN, IFIND, IREC, IBLK, IERR)
                  IF (IERR.NE.0) GO TO 230
                  ICUR = 1
                  END IF
               IF (IBLK(NN).GT.0) GO TO 230
 220           CONTINUE
C                                       Destroy file
            CALL ZCLOSE (ICLUN, IFIND, IERR)
            CALL ZPHFIL ('CA', IVOL, 0, 0, PHNAME, IERR)
            CALL ZDESTR (IVOL, PHNAME, IERR)
            IF (IERR.GT.1) GO TO 240
               WRITE (MSGTXT,1220) IVOL
               CALL MSGWRT (3)
               GO TO 240
C                                       Keep file
 230        CALL ZCLOSE (ICLUN, IFIND, IERR)
 240        CONTINUE
         END IF
C                                       No print on Batch EXIT, RESTART
C                                       This is done elsewhere
 250  IF ((NPOPS.LE.NINTRN) .AND. (TSKNAM(:5).NE.'AIPSC')) THEN
         IMAX = NLUSER
         ONCRT = -1.0
         TIMSG = TIMEMS
         ITASK = ' '
         PHNAME = ' '
         CALL PRTMSG ('DELE', IMAX, 0, 0, ITASK, TIMSG, ONCRT, PHNAME,
     *      NDONE, NLEFT, IERR)
         IF (IERR.EQ.0) THEN
            WRITE (MSGTXT,1250) NDONE
            CALL MSGWRT (1)
            END IF
         CALL SGLAST (2)
         END IF
C                                       Kill servers on KLEENEX
      IF (BRANCH.EQ.6) CALL KILSRV
C                                       Set exit or restart codes
      ERRNUM = -2
      IF (BRANCH.EQ.3) ERRNUM = -1
      GO TO 980
C-----------------------------------------------------------------------
C                                       CLRMSG
C                                       Clear message file
C-----------------------------------------------------------------------
C                                       Get adverbs.
 400  IF (ICNO.EQ.0) ICNO = NPOPS
      IMAX = NLUSER
      PHNAME = ' '
      CALL PRTMSG ('DELE', IMAX, ICNO, PRIOR, ITASK, TIMSG, ONCRT,
     *   PHNAME, NDONE, NLEFT, IERR)
      IF (IERR.EQ.0) THEN
         WRITE (MSGTXT,1250) NDONE
         CALL MSGWRT (1)
         END IF
      GO TO 980
C-----------------------------------------------------------------------
C                                       PRINTER
C                                       Change printer number
C-----------------------------------------------------------------------
 500  IPRT = 0
      IF (SP.GT.0) THEN
         IF ((SP.GE.4) .AND. (STACK(SP).NE.2)) GO TO 980
         IPRT = V(SP) + 0.01
         SP = SP - 1
         END IF
      CALL PRINTR (IPRT, IERR)
      GO TO 980

C-----------------------------------------------------------------------
C                                       Close down
 980  IF (ERRNUM.EQ.0) GO TO 999
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Printed',I7,' messages')
 1220 FORMAT ('Destroyed empty user catalog on disk',I4)
 1250 FORMAT ('Deleted',I7,' messages')
      END
