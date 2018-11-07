      PROGRAM FIXUSR
C-----------------------------------------------------------------------
C! FIXUSR recovers entries in map catalogs.
C# Utility Catalog
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
C   A service routine to recover entries in map catalogs.
C   It changes all user numbers to the logon one.
C-----------------------------------------------------------------------
      CHARACTER   PHNAME*48, MSGBUF*80, PRGNAM*6
      INTEGER   IVOL, ICNO, IBUF(256), TTYLUN, TTYIND, IERR, ICNO1,
     *   ICNO2, NWPL, NLPR, IMAX, ISCR(256), IFIND, NN, ICLUN, IT(6),
     *   HLUN, HIND, IER, IREC, LREC, TTY(2), IARR(2)
      LOGICAL   T, F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (TTY(1), TTYLUN),  (TTY(2), TTYIND)
      DATA PRGNAM /'FXUXR '/
      DATA T, F /.TRUE.,.FALSE./
      DATA TTYLUN, ICLUN, HLUN /5, 15, 16/
C-----------------------------------------------------------------------
      CALL AIPINI (TTY, PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (UCTSIZ.LE.0) GO TO 980
      NWPL = 10
      NLPR = 256 / NWPL
      WRITE (MSGTXT,1000)
      CALL MSGWRT (2)
C                                       Loop to ask for vol, cat #s
 10   WRITE (MSGBUF,1010)
      CALL INQINT (TTY, MSGBUF, 2, IARR, IERR)
      IF (IERR.GT.0) GO TO 990
      IF (IERR.LT.0) GO TO 995
      IVOL = IARR(1)
      NLUSER = IARR(2)
      IF ((IVOL.LE.0) .OR. (NLUSER.LE.0)) GO TO 995
C                                       Open catalog file
      CALL CATOPN (IVOL, IFIND, ISCR, IMAX, IERR)
      IF (IERR.EQ.0) GO TO 20
         WRITE (MSGTXT,1012) IERR
         CALL MSGWRT (7)
         GO TO 10
 20   LREC = 0
      ICNO1 = 1
      ICNO2 = IMAX
      DO 50 ICNO = ICNO1,ICNO2
C                                       get directory record
         IREC = 2 + (ICNO-1)/NLPR
         IF (IREC.EQ.LREC) GO TO 30
            IF (LREC.LE.0) GO TO 25
               CALL ZFIO ('WRIT', ICLUN, IFIND, LREC, IBUF, IERR)
               IF (IERR.EQ.0) GO TO 25
                  WRITE (MSGTXT,1020) LREC, IERR
                  CALL MSGWRT (8)
                  GO TO 995
 25         CONTINUE
               CALL ZFIO ('READ', ICLUN, IFIND, IREC, IBUF, IERR)
               LREC = IREC
               IF (IERR.EQ.0) GO TO 30
                  WRITE (MSGTXT,1025) IREC, IERR
                  CALL MSGWRT (8)
                  GO TO 995
C                                       get header record
 30      IREC = 1
         NN = 1 + MOD (ICNO-1, NLPR) * NWPL
         IF ((IBUF(NN).LT.0) .OR. (IBUF(NN).EQ.NLUSER)) GO TO 50
            CALL ZPHFIL ('CB', IVOL, ICNO, 1, PHNAME, IERR)
            CALL ZOPEN (HLUN, HIND, IVOL, PHNAME, F, T, T, IERR)
            IF (IERR.EQ.0) GO TO 35
               WRITE (MSGTXT,1030) ICNO, IERR
               CALL MSGWRT (8)
               IF (IERR.NE.2) GO TO 995
                  IBUF(NN) = -1
                  GO TO 50
 35         CALL ZFIO ('READ', HLUN, HIND, IREC, CATBLK, IERR)
            IF (IERR.EQ.0) GO TO 40
               WRITE (MSGTXT,1035) ICNO, IERR
               CALL MSGWRT (8)
               GO TO 995
 40         IBUF(NN) = NLUSER
            CALL CATIME (1, IBUF(NN+2), IT)
            CATBLK(KIIMU) = NLUSER
            CALL ZFIO ('WRIT', HLUN, HIND, IREC, CATBLK, IERR)
            CALL ZCLOSE (HLUN, HIND, IER)
            IF (IERR.EQ.0) GO TO 50
               WRITE (MSGTXT,1040) ICNO, IERR
               CALL MSGWRT (8)
               GO TO 995
 50      CONTINUE
      IF (LREC.LE.0) GO TO 60
         CALL ZFIO ('WRIT', ICLUN, IFIND, LREC, IBUF, IERR)
         IF (IERR.EQ.0) GO TO 60
            WRITE (MSGTXT,1020) LREC, IERR
            CALL MSGWRT (8)
            GO TO 995
 60   CALL ZCLOSE (ICLUN, IFIND, IERR)
      GO TO 10
C                                       not on public catlgs
 980  WRITE (MSGTXT,1980)
      CALL MSGWRT (8)
      GO TO 995
C                                       Terminal error
 990  WRITE (MSGTXT,1990) IERR
      CALL MSGWRT (8)
C
 995  CALL ZCLOSE (TTYLUN, TTYIND, IERR)
      NLUSER = 1
      CALL ACOUNT (2)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('I repair catalog entries')
 1010 FORMAT ('Enter Vol#, User (2 of I)  Q to quit')
 1012 FORMAT ('CATOPN ERROR',I7)
 1020 FORMAT ('WRITE DIR REC',I7,' ERROR',I7)
 1025 FORMAT ('READ DIR REC',I7,' ERROR',I7)
 1030 FORMAT ('OPEN HDR CNO',I7,' ERROR',I7)
 1035 FORMAT ('READ HDR CNO',I7,' ERROR',I7)
 1040 FORMAT ('WRITE HDR CNO',I7,' ERROR',I7)
 1980 FORMAT ('I WORK ONLY ON PRIVATE CATALOG SYSTEMS')
 1990 FORMAT ('ERROR',I7,' IN TERMINAL IO')
      END
