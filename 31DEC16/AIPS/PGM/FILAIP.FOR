      PROGRAM FILAIP
C-----------------------------------------------------------------------
C! stand-alone program to create/initialize all basic AIPS files
C# Service
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2002-2003, 2005, 2007-2008
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
C   FILAIP is a service program designed to create and initialize the
C   files needed to run AIPS.  It may be used to alter the AIPS
C   configuration already in use as well.  In either case, it
C   initializes all basic AIPS files.
C-----------------------------------------------------------------------
      CHARACTER MPASS*12, PHNAME*48, FTYP(13)*2, MSGBUF*80, PRGNAM*6
      INTEGER   I, J, CMAX, IC, NREC, ZBUF(256), IT, IERR, IAERR, IVOL,
     *   BUFFER(256), LUN, FIND, NL, IN, IQ, TTYLUN, TTYIND,
     *   SCRTCH(256), ISIZE, LSIZE, IRNO, TTY(2), IARR(5)
      REAL      ROBUFF(256), RBUFFR(256)
      HOLLERITH HPASS(3), HBUFFR(256)
      LOGICAL   T, F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PKSZ.INC'
      EQUIVALENCE (BUFFER, RBUFFR, HBUFFR),    (BUFFER, ROBUFF)
      EQUIVALENCE (TTY(1), TTYLUN),  (TTY(2), TTYIND)
      DATA MPASS /'AMANAGER '/
      DATA T, F /.TRUE.,.FALSE./
      DATA PRGNAM /'FILAIP'/
      DATA FTYP /'BQ','BA','CA','TD','IC','MS','ME','SP','AC','TP',
     *   'GR','PW','TC'/
      DATA LUN, TTYLUN  /15, 5/
C-----------------------------------------------------------------------
C                                       basic init
      CALL AIPINI (TTY, PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZPHFIL ('AC', 1, 0, 0, PHNAME, IERR)
      CALL ZEXIST (1, PHNAME, ISIZE, IAERR)
      CALL YTVCIN
C                                       Is this job ok?
C                                       Does password file exist
      CALL ZPHFIL ('PW', 1, 0, 0, PHNAME, IERR)
      CALL ZEXIST (1, PHNAME, ISIZE, IERR)
C                                       Error
      IF (IERR.GT.1) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (8)
C                                       Ask for password
      ELSE IF (IERR.EQ.0) THEN
         CALL PASWRD (SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Ask for basic data
C                                       # disks, cat size
      CMAX = 400
      NVOL = 35
      UCTSIZ = 100
C                                       # interactive, batch AIPS
 30   WRITE (MSGBUF,1030)
      CALL INQINT (TTY, MSGBUF, 2, IARR, IERR)
      IF (IERR.GT.0) GO TO 990
      IF (IERR.LT.0) GO TO 30
      I = IARR(1)
      J = IARR(2)
      IF (I.LE.0) I = NINTRN
      IF (J.LT.0) J = NBATQS
      IC = I
      IF (J.GT.0) IC = I + 1 + J
      IF ((I.LE.0) .OR. (IC.GT.35)) GO TO 30
      NINTRN = I
      NBATQS = J
C                                       # interactive devices:
C                                       obsolete
      CALL FILL (256, 0, ZBUF)
C                                       # of tape drives
      NTAPED = 10
C-----------------------------------------------------------------------
C                                       Batch queuing
 100  IF (NBATQS.LE.0) GO TO 150
         IT = 1
         ISIZE = NBATQS
         CALL ZPHFIL (FTYP(IT), 1, 0, 0, PHNAME, IERR)
         CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
         IF (IERR.LE.1) GO TO 110
            WRITE (MSGTXT,1100) FTYP(IT), IERR
            CALL MSGWRT (8)
            GO TO 150
 110     CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
         IF (IERR.EQ.0) GO TO 120
            WRITE (MSGTXT,1110) FTYP(IT), IERR
            CALL MSGWRT (8)
            GO TO 150
 120     DO 125 I = 1,NBATQS
            IRNO = I
            CALL ZFIO ('WRIT', LUN, FIND, IRNO, ZBUF, IERR)
            IF (IERR.EQ.0) GO TO 125
               WRITE (MSGTXT,1120) FTYP(IT), IERR, I
               CALL MSGWRT (8)
               GO TO 130
 125        CONTINUE
 130     CALL ZCLOSE (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C                                       Batch work files
 150  IF (NBATQS.LE.0) GO TO 200
         IT = 2
         NL = 252 / 22
         ISIZE = 100/NL + 1
         CALL FILL (256, 0, BUFFER)
         BUFFER(2) = 2
         BUFFER(3) = 1
         NL = NINTRN + 1
         DO 180 IN = 1,NL
            DO 170 IQ = 1,NBATQS
               CALL ZPHFIL (FTYP(IT), 1, IQ, IN, PHNAME, IERR)
               CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
               IF (IERR.LE.1) GO TO 160
                  WRITE (MSGTXT,1100) FTYP(IT), IERR, IN, IQ
                  CALL MSGWRT (8)
                  GO TO 170
 160           IF (IERR.EQ.1) BUFFER(4) = ISIZE
               IF (IERR.EQ.0) BUFFER(4) = LSIZE
               CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
               IF (IERR.EQ.0) GO TO 165
                  WRITE (MSGTXT,1110) FTYP(IT), IERR, IN, IQ
                  CALL MSGWRT (8)
                  GO TO 170
 165           CALL ZFIO ('WRIT', LUN, FIND, 1, BUFFER, IERR)
               WRITE (MSGTXT,1120) FTYP(IT), IERR, IN, IQ
               IF (IERR.NE.0) CALL MSGWRT (8)
               CALL ZCLOSE (LUN, FIND, IERR)
 170           CONTINUE
 180        CONTINUE
C-----------------------------------------------------------------------
C                                       File catalog
C                                       This code never executed now
C                                       was used in the old days
 200  IF (UCTSIZ.GT.0) GO TO 250
         IT = 3
         NL = 10
         ISIZE = 2 + (CMAX-1) / (256/NL)
         DO 240 IVOL = 1,NVOL
            CALL ZPHFIL (FTYP(IT), IVOL, 0, 0, PHNAME, IERR)
            CALL ZCREAT (IVOL, PHNAME, ISIZE, F, LSIZE, IERR)
            IF (IERR.EQ.0) GO TO 210
C                                       Creation error other than
C                                       the file already exists.
               IF (IERR.EQ.1) GO TO 205
                  WRITE (MSGTXT,1100) FTYP(IT), IERR, IVOL
                  CALL MSGWRT (8)
                  GO TO 240
C                                       CA file exist. Do not init.
 205           CONTINUE
                  WRITE (MSGTXT,1205) FTYP(IT)
                  CALL MSGWRT (2)
                  GO TO 240
C
 210        CALL ZOPEN (LUN, FIND, IVOL, PHNAME, F, T, T, IERR)
            IF (IERR.EQ.0) GO TO 215
               WRITE (MSGTXT,1110) FTYP(IT), IERR, IVOL
               CALL MSGWRT (8)
               GO TO 240
C
 215        CALL FILL (256, 0, BUFFER)
            BUFFER(1) = IVOL
            BUFFER(3) = CMAX
            CALL ZDATE (BUFFER(4))
            CALL ZTIME (BUFFER(7))
            CALL ZDATE (BUFFER(10))
            CALL ZTIME (BUFFER(13))
            CALL ZFIO ('WRIT', LUN, FIND, 1, BUFFER, IERR)
            IF (IERR.EQ.0) GO TO 220
               WRITE (MSGTXT,1120) FTYP(IT), IERR, IVOL, 1
               CALL MSGWRT (8)
               GO TO 235
 220        CALL FILL (16, 0, BUFFER)
            DO 225 I = 1,256,NL
               BUFFER(I) = -1
 225           CONTINUE
            NREC = LSIZE
            DO 230 IRNO = 2,NREC
               CALL ZFIO ('WRIT', LUN, FIND, IRNO, BUFFER, IERR)
               IF (IERR.EQ.0) GO TO 230
                  WRITE (MSGTXT,1120) FTYP(IT), IERR, IVOL, IRNO
                  CALL MSGWRT (8)
                  GO TO 235
 230           CONTINUE
 235        CALL ZCLOSE (LUN, FIND, IERR)
 240        CONTINUE
C-----------------------------------------------------------------------
C                                       Task data communications
 250  IT = 4
      ISIZE = 4 * 35 + 1
      CALL ZPHFIL (FTYP(IT), 1, 0, 4, PHNAME, IERR)
      CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
      IF (IERR.LE.1) GO TO 255
         WRITE (MSGTXT,1100) FTYP(IT), IERR
         CALL MSGWRT (8)
         GO TO 300
 255  CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.EQ.0) GO TO 260
         WRITE (MSGTXT,1110) FTYP(IT), IERR
         CALL MSGWRT (8)
         GO TO 300
 260  ZBUF(256) = 4
      CALL ZFIO ('WRIT', LUN, FIND, 1, ZBUF, IERR)
      ZBUF(256) = 0
      WRITE (MSGTXT,1120) FTYP(IT), IERR
      IF (IERR.NE.0) CALL MSGWRT (8)
      CALL ZCLOSE (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C                                       Image catalogs
C                                       Television devices
C                                       obsolete - no files needed
 300  IT = 5
C                                       Graphics devices
      NREC = 1295
      ISIZE = NREC
      CALL ZPHFIL (FTYP(IT), 1, 0, 0, PHNAME, IERR)
      CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
C                                       leave pre-existing alone
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1100) FTYP(IT), IERR
         IF (IERR.GT.1) CALL MSGWRT (8)
         GO TO 350
         END IF
 330  CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1110) FTYP(IT), IERR
         CALL MSGWRT (8)
         GO TO 350
         END IF
 335  DO 340 I = 1,1295
         CALL ZFIO ('WRIT', LUN, FIND, I, ZBUF, IERR)
         IF (IERR.NE.0)  THEN
            WRITE (MSGTXT,1120) FTYP(IT), IERR, I
            CALL MSGWRT (8)
            GO TO 345
            END IF
 340     CONTINUE
 345  CALL ZCLOSE (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C                                       Message files: user #1
 350  IT = 6
      ISIZE = 4
      CALL FILL (256, 0, BUFFER)
      CALL ZPHFIL (FTYP(IT), 1, NLUSER, 0, PHNAME, IERR)
      CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
C                                       Creation error
      IF (IERR.NE.0) THEN
         IF (IERR.NE.1) THEN
            WRITE (MSGTXT,1100) FTYP(IT), IERR
            CALL MSGWRT (8)
C                                       MS file exists. Do not init.
         ELSE
            WRITE (MSGTXT,1205) FTYP(IT)
            CALL MSGWRT (2)
            END IF
         GO TO 400
         END IF
C
 355  BUFFER(2) = LSIZE
      CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.EQ.0) GO TO 360
         WRITE (MSGTXT,1110) FTYP(IT), IERR
         CALL MSGWRT (8)
         GO TO 400
 360  CALL ZFIO ('WRIT', LUN, FIND, 1, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1120) FTYP(IT), IERR
         CALL MSGWRT (8)
         END IF
      CALL ZCLOSE (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C                                       Memory files: create only
 400  NREC = KBLOCK + LBLOCK
      IC = NINTRN + 1
      IF (NBATQS.GT.0) IC = NINTRN + 1 + NBATQS + 1
      ISIZE = NREC
      IT = 7
      DO 410 I = 1,IC
         J = I - 1
         CALL ZPHFIL (FTYP(IT), 1, 0, J, PHNAME, IERR)
         CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
         IF (IERR.LE.1) GO TO 410
            WRITE (MSGTXT,1100) FTYP(IT), IERR, J
            CALL MSGWRT (8)
 410     CONTINUE
      WRITE (MSGTXT,1410) IC
      CALL MSGWRT (2)
C-----------------------------------------------------------------------
C                                       System parameters file
 450  IT = 8
      NREC = 1
      ISIZE = NREC
      CALL ZPHFIL (FTYP(IT), 1, 0, 0, PHNAME, IERR)
      CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
C                                       leave pre-existing alone
      IF (IERR.EQ.0) GO TO 455
         WRITE (MSGTXT,1100) FTYP(IT), IERR
         IF (IERR.NE.1) CALL MSGWRT (8)
         GO TO 500
 455  CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.EQ.0) GO TO 460
         WRITE (MSGTXT,1110) FTYP(IT), IERR
         CALL MSGWRT (8)
         GO TO 500
 460  CALL FILL (256, 0, BUFFER)
      BUFFER(1) = NVOL
      BUFFER(2) = NTAPED
      BUFFER(3) = CRTMAX
      BUFFER(4) = PRTMAX
      BUFFER(5) = NBATQS
      BUFFER(6) = MAXXPR(1)
      BUFFER(7) = MAXXPR(2)
      BUFFER(8) = CSIZPR(1)
      BUFFER(9) = CSIZPR(2)
      BUFFER(10) = NINTRN
      BUFFER(11) = KAPWRD
      BUFFER(12) = NTVDEV
      BUFFER(13) = NTKDEV
      BUFFER(64) = NTVACC
      BUFFER(65) = NTKACC
      BUFFER(66) = UCTSIZ
      BUFFER(67) = USELIM
      BUFFER(68) = NCHPRT
      BUFFER(69) = 0
C                                       Floating pt values.
      ROBUFF(71) = XPRDMM
      ROBUFF(72) = XTKDMM
      CALL CHR2H (20, SYSNAM, 1, ROBUFF(73))
      CALL RCOPY (15, TIMEDA, ROBUFF(78))
      ROBUFF(93) = TIMESG
      ROBUFF(94) = TIMEMS
      ROBUFF(95) = TIMESC
      ROBUFF(96) = TIMECA
      CALL RCOPY (4, TIMEBA, ROBUFF(97))
      CALL RCOPY (3, TIMEAP, ROBUFF(101))
      CALL COPY (120, DASSGN, BUFFER(104))
      BUFFER(224) = MAXXTK(1)
      BUFFER(225) = MAXXTK(2)
      BUFFER(226) = CSIZTK(1)
      BUFFER(227) = CSIZTK(2)
      BUFFER(228) = PRDELE
      ROBUFF(238) = AIPSMK
      IF (ROBUFF(238).LE.0.0) ROBUFF(238) = 10.0
C                                       Set device table.
      CALL COPY (50, DEVTAB, BUFFER(14))
      CALL ZFIO ('WRIT', LUN, FIND, 1, BUFFER, IERR)
      IF (IERR.NE.0)  THEN
         WRITE (MSGTXT,1120) FTYP(IT), IERR
         CALL MSGWRT (8)
         END IF
      CALL ZCLOSE (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C                                       Accounting
 500  IT = 9
      NREC = 100
      ISIZE = NREC
      CALL ZPHFIL (FTYP(IT), 1, 0, 0, PHNAME, IERR)
      CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
C                                       Creation error
      IF (IERR.NE.0) THEN
         IF (IERR.NE.1) THEN
            WRITE (MSGTXT,1100) FTYP(IT), IERR
            CALL MSGWRT (8)
C                                       AC file exists. Do not init.
         ELSE
            WRITE (MSGTXT,1205) FTYP(IT)
            CALL MSGWRT (2)
            END IF
         GO TO 550
         END IF
C
 510  NREC = LSIZE
      CALL FILL (256, 0, BUFFER)
      BUFFER(3) = 9
      BUFFER(4) = 256 / BUFFER(3)
      BUFFER(2) = NREC * BUFFER(4) - 1
      CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.EQ.0) GO TO 520
         WRITE (MSGTXT,1110) FTYP(IT), IERR
         CALL MSGWRT (8)
         GO TO 550
 520  CALL ZFIO ('WRIT', LUN, FIND, 1, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1120) FTYP(IT), IERR
         CALL MSGWRT (8)
         END IF
      CALL ZCLOSE (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C                                       Tape drive dummy files
 550  IF (NTAPED.LE.0) GO TO 600
         NREC = 0
         ISIZE = 1
         IT = 10
         DO 560 I = 1,NTAPED
            CALL ZPHFIL (FTYP(IT), 1, 1, I, PHNAME, IERR)
            CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
            IF (IERR.LE.1) GO TO 560
               WRITE (MSGTXT,1100) FTYP(IT), IERR
               CALL MSGWRT (8)
 560        CONTINUE
C-----------------------------------------------------------------------
C                                       Gripes
 600  IT = 11
      NREC = 50
      ISIZE = NREC
      CALL FILL (256, 0, BUFFER)
      BUFFER(3) = 2
      BUFFER(4) = 1
      CALL ZPHFIL (FTYP(IT), 1, 0, 0, PHNAME, IERR)
      CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
C                                       Creation error
      IF (IERR.NE.0) THEN
         IF (IERR.NE.1) THEN
            WRITE (MSGTXT,1100) FTYP(IT), IERR
            CALL MSGWRT (8)
C                                       GR file exists. Do not init.
         ELSE
            WRITE (MSGTXT,1205) FTYP(IT)
            CALL MSGWRT (2)
            END IF
         GO TO 650
         END IF
C
      BUFFER(1) = LSIZE
      CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1110) FTYP(IT), IERR
         CALL MSGWRT (8)
      ELSE
         CALL ZFIO ('WRIT', LUN, FIND, 1, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1120) FTYP(IT), IERR
            CALL MSGWRT (8)
            END IF
         CALL ZCLOSE (LUN, FIND, IERR)
         END IF
C-----------------------------------------------------------------------
C                                       Password file
 650  IT = 12
      I = 3
      J = 256 / I
      NREC = (USELIM-1) / J + 1
      ISIZE = NREC
      CALL ZPHFIL (FTYP(IT), 1, 0, 0, PHNAME, IERR)
      CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
C                                       Creation error
      IF (IERR.NE.0) THEN
         IF (IERR.NE.1) THEN
            WRITE (MSGTXT,1100) FTYP(IT), IERR
            CALL MSGWRT (8)
C                                       PW file exists. Do not init.
         ELSE
            WRITE (MSGTXT,1205) FTYP(IT)
            CALL MSGWRT (2)
            END IF
         GO TO 700
         END IF
C                                       Encode the blank password
      MSGBUF = ' '
      CALL PASENC (MSGBUF, HPASS)
      J = 3 * J
      DO 665 I = 1,J,3
         CALL RCOPY (3, HPASS, RBUFFR(I))
 665     CONTINUE
C                                       and the default AIPS Manager
      CALL PASENC (MPASS, HBUFFR)
      CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.EQ.0) GO TO 670
         WRITE (MSGTXT,1110) FTYP(IT), IERR
         CALL MSGWRT (8)
         GO TO 700
 670  DO 680 I = 1,NREC
         CALL ZFIO ('WRIT', LUN, FIND, I, BUFFER, IERR)
         IF (I.EQ.1) CALL RCOPY (3, HPASS, RBUFFR(1))
         IF (IERR.EQ.0) GO TO 680
            WRITE (MSGTXT,1120) FTYP(IT), IERR
            CALL MSGWRT (8)
            GO TO 690
 680     CONTINUE
 690  CALL ZCLOSE (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C                                       Task communications (SHOW, TELL)
 700  IT = 13
      IC = NINTRN + 1
      IF (NBATQS.GT.0) IC = IC + NBATQS + 1
      ISIZE = 18 + 16 * IC
      CALL ZPHFIL (FTYP(IT), 1, 0, 1, PHNAME, IERR)
      CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
      IF (IERR.LE.1) GO TO 705
         WRITE (MSGTXT,1100) FTYP(IT), IERR
         CALL MSGWRT (8)
         GO TO 750
 705  CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.EQ.0) GO TO 710
         WRITE (MSGTXT,1110) FTYP(IT), IERR
         CALL MSGWRT (8)
         GO TO 750
 710  DO 715 IRNO = 1,18
         CALL ZFIO ('WRIT', LUN, FIND, IRNO, ZBUF, IERR)
         IF (IERR.EQ.0) GO TO 715
            WRITE (MSGTXT,1120) FTYP(IT), IERR
            CALL MSGWRT (8)
            GO TO 720
 715     CONTINUE
 720  CALL ZCLOSE (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C                                       Enough for now!!
 750  GO TO 900
C
 900  WRITE (MSGTXT,1900)
      CALL MSGWRT (2)
      GO TO 995
 990  WRITE (MSGTXT,1990) IERR
      CALL MSGWRT (8)
C
 995  CALL ZCLOSE (TTYLUN, TTYIND, IERR)
      IF (IAERR.EQ.0) CALL ACOUNT (2)
C
 999  STOP
C-----------------------------------------------------------------------
 1010 FORMAT ('** ERROR',I7,' FROM ZEXIST ON PASSWORD FILE ***')
 1030 FORMAT ('# interactive AIPS, # batch queues (2 I)')
 1100 FORMAT ('FILE TYPE ',A2,' CREATE ERROR',4I7)
 1110 FORMAT ('FILE TYPE ',A2,' OPEN ERROR',4I7)
 1120 FORMAT ('FILE TYPE ',A2,' WRITE ERROR',4I7)
 1205 FORMAT (A2,' FILE NOT INITIALIZED.',
     *   ' USE FILINI IF YOU REALLY MEAN IT')
 1300 FORMAT ('Image device No.',I2,' #gray, #graph, #img/gray, #ISUs',
     *   ' (4 I)')
 1410 FORMAT ('Init POPS memory files 1 through ',I2,
     *   ' with program POPSGN')
 1900 FORMAT ('Done!')
 1990 FORMAT ('ERROR',I7,' IN TERMINAL IO')
      END
