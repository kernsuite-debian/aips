      PROGRAM DELSG
C-----------------------------------------------------------------------
C! DELSG deletes out-dated SAVE/GET files.
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   DELSG deletes out-dated SAVE/GET files
C-----------------------------------------------------------------------
      CHARACTER SGNAME*48, PHNAME*48, PRGNAM*6, MSGBUF*80, ANAME*16
      INTEGER  NWPL, NLPR, LOWVER, IBLK(256), IERR, LUN, NUSE1, NUSE2,
     *   IU, FIND, I, J, IV, IP, TTYLUN, TTYIND, NDIR, NFIL, NF, NMF,
     *   NRF, LREC, IREC, TTY(2), NUSE(2)
      LOGICAL  T, F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (TTY(1), TTYLUN),  (TTY(2), TTYIND)
      EQUIVALENCE (NUSE(1), NUSE1),  (NUSE(2), NUSE2)
      DATA PRGNAM, LUN, TTYLUN /'DELSG ', 26, 5/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       AIPS inits
      CALL AIPINI (TTY, PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Password required
      CALL PASWRD (IBLK, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get lowest ok version number
 5    WRITE (MSGBUF,1000)
      CALL INQINT (TTY, MSGBUF, 1, LOWVER, IERR)
      IF (IERR.GT.0) GO TO 990
      IF (IERR.LT.0) GO TO 5
C                                       get range user numbers
 10   WRITE (MSGBUF,1010) USELIM
      CALL INQINT (TTY, MSGBUF, 2, NUSE, IERR)
      IF (IERR.GT.0) GO TO 990
      IF (IERR.LT.0) GO TO 10
      CALL ZCLOSE (TTYLUN, TTYIND, IERR)
C                                       parms, counters
      IF ((NUSE2.GE.NUSE1) .AND. (NUSE1.GT.0) .AND. (NUSE2.LE.USELIM))
     *   GO TO 20
         NUSE1 = 1
         NUSE2 = USELIM
 20   NWPL = 8
      NLPR = 256 / NWPL
      NDIR = 0
      NFIL = 0
C                                       Loop over user numbers
      DO 100 IU = NUSE1,NUSE2
C                                       Open user SG directory
         NLUSER = IU
         CALL ZPHFIL ('SG', 1, IU, 0, PHNAME, IERR)
         NLUSER = 1
         MSGSUP = 32000
         CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
         MSGSUP = 0
         IF (IERR.EQ.0) GO TO 25
            WRITE (MSGTXT,1020) IU, IERR
            IF (IERR.NE.2) CALL MSGWRT (8)
            GO TO 100
C                                       First record pointers
 25      LREC = 1
         CALL ZFIO ('READ', LUN, FIND, LREC, IBLK, IERR)
         IF (IERR.NE.0) GO TO 95
         NF = IBLK(1)
         NMF = NF
         NRF = IBLK(2)
C                                       Loop over all entries
         DO 50 I = 1,NF
            IV = NF + 1 - I
            IREC = (IV+1) / NLPR + 1
C                                       Swap in core dir blocks
            IF (IREC.EQ.LREC) GO TO 30
               CALL ZFIO ('WRIT', LUN, FIND, LREC, IBLK, IERR)
               IF (IERR.NE.0) GO TO 95
               CALL ZFIO ('READ', LUN, FIND, IREC, IBLK, IERR)
               IF (IERR.NE.0) GO TO 95
               LREC = IREC
C                                       Check entry
 30         IP = MOD (IV+1, NLPR) * NWPL + 1
C                                       In use: check version
            IF (IBLK(IP).EQ.0) GO TO 40
               J = IBLK(IP) / 32
C                                       Old: delete
               IF (J.GE.LOWVER) GO TO 50
                  NLUSER = IU
                  CALL ZPHFIL ('SG', 1, IU, IV, SGNAME, IERR)
                  NLUSER = 1
                  CALL ZDESTR (1, SGNAME, IERR)
                  CALL H2CHR (16, 1, IBLK(IP+4), ANAME)
                  IF (IERR.LE.1) WRITE (MSGTXT,1030) IU, ANAME
                  IF (IERR.GT.1) WRITE (MSGTXT,1031) IERR, IU, ANAME
                  CALL MSGWRT (4)
                  IF (IERR.LE.1) NFIL = NFIL + 1
C                                       Remove directory entry
 40            CONTINUE
                  IF (IBLK(IP).EQ.0) NRF = NRF - 1
                  IF (IV.LT.NMF) NRF = NRF + 1
                  IF (IV.EQ.NMF) NMF = NMF - 1
                  IBLK(IP) = 0
 50         CONTINUE
C                                       Update record 1 (now in core)
         IBLK(1) = NMF
         IBLK(2) = NRF
         CALL ZFIO ('WRIT', LUN, FIND, LREC, IBLK, IERR)
         IF (IERR.NE.0) GO TO 95
         CALL ZCLOSE (LUN, FIND, IERR)
C                                       Destroy directory too
         IF (NMF.GT.0) GO TO 100
            CALL ZDESTR (1, PHNAME, IERR)
            IF (IERR.LE.1) WRITE (MSGTXT,1050) IU
            IF (IERR.GT.1) WRITE (MSGTXT,1051) IERR, IU
            CALL MSGWRT (4)
            IF (IERR.LE.1) NDIR = NDIR + 1
            GO TO 100
C                                       Close on error
 95      CALL ZCLOSE (LUN, FIND, IERR)
 100     CONTINUE
      WRITE (MSGTXT,1100) NDIR, NFIL
      CALL MSGWRT (4)
      GO TO 995
C                                       TTY error
 990  WRITE (MSGTXT,1990) IERR
      CALL MSGWRT (8)
C                                       Close accounting
 995  CALL ACOUNT (2)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('Lowest still usable save/get version number (I)')
 1010 FORMAT ('User number range to do (2 of I) default=   0',I5)
 1020 FORMAT ('OPEN SG DIRECTORY USER',I5,' ERROR',I7)
 1030 FORMAT ('User',I5,' deleted ',A16)
 1031 FORMAT ('ERROR',I7,' USER',I5,' DELETING ',A16)
 1050 FORMAT ('Also delete SG directory: user',I5)
 1051 FORMAT ('ERROR',I7,' DELETING SG DIRECTORY USER',I5)
 1100 FORMAT ('Deleted a total of',I4,' dirs and',I7,' files')
 1990 FORMAT ('ERROR',I7,' WITH TERMINAL IO')
      END
