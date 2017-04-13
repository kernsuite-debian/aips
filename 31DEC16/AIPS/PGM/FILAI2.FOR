      PROGRAM FILAI2
C-----------------------------------------------------------------------
C! FILAI2 creates and initializes files needed to run AIPS.
C# Service
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2002, 2005, 2007-2008
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
C   FILAI2 is a service program designed to create and initialize
C   the files needed to run AIPS.  It may be used to alter the AIPS
C   configuration already in use as well.  In either case, it
C   creates and initializes all basic AIPS files except the memory file
C   (created in this program but initialized by POPSGN).  If a catalog
C   file already exists it will not be re-initialized. This program is
C   similiar to FILAIP except inputs are read from a text file SYSPARM.
C-----------------------------------------------------------------------
      CHARACTER MPASS*12, PHNAME*48, FTYP(13)*2, PRGNAM*6, MNAME*8,
     *   VER*20, MSGBUF*80, LPASS*12
      HOLLERITH APASS(3)
      INTEGER  I, J, CMAX, IC, NREC, ZBUF(256), IT, IERR, TTY(2),
     *   BUFFER(256), LUN, FIND, NL, IN, IQ, SCRTCH(256), IVOL, IRNO,
     *   TXTLUN, TXTIND, TTYLUN, TTYIND, LTVDEV, I4T, ISIZE, LSIZE
      REAL      ANSWER(22), TKANS(2), TVANS(20), ROBUFF(256)
      LOGICAL   T, F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PKSZ.INC'
      EQUIVALENCE (BUFFER, ROBUFF)
      EQUIVALENCE (TTY(1), TTYLUN),  (TTY(2), TTYIND)
      DATA MPASS /'AMANAGER    '/
      DATA T, F /.TRUE.,.FALSE./
      DATA PRGNAM /'FILAI2'/
      DATA FTYP /'BQ','BA','CA','TD','IC','MS','ME','SP','AC','TP',
     *   'GR','PW','TC'/
      DATA MNAME /'SYSPARM '/
      DATA LUN, TXTLUN, TTYLUN /15, 11, 6/
C-----------------------------------------------------------------------
C                                       basic init
      CALL AIPINI (TTY, PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 990
      MSGSUP = 32000
      CALL YTVCIN
C                                       Open text file
      CALL ZPHFIL ('RT', 1, 0, 1, PHNAME, IERR)
      VER = 'NEW'
      CALL ZTOPEN (TXTLUN, TXTIND, 1, PHNAME, MNAME, VER, T, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
         GO TO 999
 10   CALL FILL (20, 0, TVANS)
      CALL RFILL (2, 0.0, TKANS)
C                                       Read so far unused character
C                                       stuff from front of file.
      CALL ZTREAD (TXTLUN, TXTIND, MSGBUF, IERR)
      CALL ZTREAD (TXTLUN, TXTIND, MSGBUF, IERR)
      CALL ZTREAD (TXTLUN, TXTIND, MSGBUF, IERR)
      CALL ZTREAD (TXTLUN, TXTIND, MSGBUF, IERR)
C                                       Read system name.
      CALL ZTREAD (TXTLUN, TXTIND, MSGBUF, IERR)
      SYSNAM = MSGBUF
C                                       Get basic data from text
C                                       file.
      DO 15 I = 1,19
         CALL ZTREAD (TXTLUN, TXTIND, MSGBUF, IERR)
         ANSWER(I) = 0.0
         READ (MSGBUF,1001,ERR=15) ANSWER(I)
 15      CONTINUE
C                                       Read TK parameters.
      IF (ANSWER(17).EQ.0) GO TO 18
         DO 16 I = 1,2
            CALL ZTREAD (TXTLUN, TXTIND, MSGBUF, IERR)
            READ (MSGBUF,1001,ERR=16) TKANS(I)
 16         CONTINUE
 18   CONTINUE
C                                       Read tv parameters.
      IF (ANSWER(18).EQ.0) GO TO 21
         IC = 2 + (2 * ANSWER(18) )
         DO 20 I = 1,IC
            CALL ZTREAD (TXTLUN, TXTIND, MSGBUF, IERR)
            READ (MSGBUF,1001,ERR=20) TVANS(I)
 20         CONTINUE
 21   CONTINUE
      I = ANSWER(2)
      IF ((I.GT.0) .AND. (I.LE.9)) NVOL = I
      UCTSIZ = ABS (ANSWER(6))
      IF (UCTSIZ.GT.46655) UCTSIZ = 46655
      UCTSIZ = MAX (100, UCTSIZ)
      CMAX = UCTSIZ
C                                       # interactive, batch AIPS
      I = ANSWER(4) + .01
      J = ANSWER(5) + .01
      IF (I.LE.0) I = 1
      I = MIN (I, 35)
      IC = I
      IF (J.GT.0) IC = I + 1 + J
      IF ((I.LE.0) .OR. (IC.GT.35)) J = 0
      NINTRN = I
      NBATQS = J
C                                       # interactive devices
      NTVDEV = ANSWER(18) + .01
      IF (J.GE.0) NTKDEV = ANSWER(17) + .01
      CALL FILL (256, 0, ZBUF)
      LTVDEV = NTVDEV
C                                       # of tape drives
      I = ANSWER(3) + .01
      IF ((I.GT.0) .AND. (I.LE.9)) NTAPED = I
C                                       Is this job ok?
C                                       Does password file exist
      CALL ZPHFIL (FTYP(12), 1, 0, 0, PHNAME, IERR)
      CALL ZEXIST (1, PHNAME, ISIZE, IERR)
      IF (IERR.EQ.1) GO TO 100
C                                       Ask for password
         IF (IERR.NE.0) GO TO 30
            CALL PASWRD (SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 995
            GO TO 100
 30      CONTINUE
            WRITE (MSGTXT,1030) IERR
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT,IERR)
C-----------------------------------------------------------------------
C                                       Batch queuing
 100  IF (NBATQS.LE.0) GO TO 150
         IT = 1
         ISIZE = NBATQS
         CALL ZPHFIL (FTYP(IT), 1, 0, 0, PHNAME, IERR)
         CALL ZDESTR (1, PHNAME, IERR)
         CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
         IF (IERR.EQ.0) GO TO 110
            WRITE (MSGTXT,1100) FTYP(IT), IERR
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
            GO TO 150
 110     CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
         IF (IERR.EQ.0) GO TO 120
            WRITE (MSGTXT,1110) FTYP(IT), IERR
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
            GO TO 150
 120     DO 125 I = 1,NBATQS
            IRNO = I
            CALL ZFIO ('WRIT', LUN, FIND, IRNO, ZBUF, IERR)
            IF (IERR.EQ.0) GO TO 125
               WRITE (MSGTXT,1120) FTYP(IT), IERR, I
               CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
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
               CALL ZDESTR (1, PHNAME, IERR)
               CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
               IF (IERR.EQ.0) GO TO 160
                  WRITE (MSGTXT,1100) FTYP(IT), IERR, IN, IQ
                  CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT,IERR)
                  GO TO 170
 160           BUFFER(4) = LSIZE
               CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
               IF (IERR.EQ.0) GO TO 165
                  WRITE (MSGTXT,1110) FTYP(IT), IERR, IN, IQ
                  CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT,IERR)
                  GO TO 170
 165           CALL ZFIO ('WRIT', LUN, FIND, 1, BUFFER, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1120) FTYP(IT), IERR, IN, IQ
                  CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT,IERR)
                  END IF
               CALL ZCLOSE (LUN, FIND, IERR)
 170           CONTINUE
 180        CONTINUE
C-----------------------------------------------------------------------
C                                       File catalog
C                                       This portion not used now
C                                       Inits public catalog files
 200  IF (UCTSIZ.GT.0) GO TO 250
      IT = 3
      NL = 256 / 11
      ISIZE = 2 + CMAX + (CMAX-1)/NL
      DO 240 IVOL = 1,NVOL
         CALL ZPHFIL (FTYP(IT), IVOL, 0, 0, PHNAME, IERR)
         CALL ZCREAT (IVOL, PHNAME, ISIZE, F, LSIZE, IERR)
         IF (IERR.EQ.0) GO TO 210
            IF (IERR.EQ.1) GO TO 205
               WRITE (MSGTXT,1100) FTYP(IT), IERR, IVOL
               CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
               GO TO 240
 205        CONTINUE
               WRITE (MSGTXT,1205) IVOL
               CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
               GO TO 240
C
 210     CALL ZOPEN (LUN, FIND, IVOL, PHNAME, F, T, T, IERR)
         IF (IERR.EQ.0) GO TO 215
            WRITE (MSGTXT,1110) FTYP(IT), IERR, IVOL
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
            GO TO 240
 215     CALL FILL (256, 0, BUFFER)
         BUFFER(1) = IVOL
         BUFFER(3) = CMAX
         CALL ZFIO ('WRIT', LUN, FIND, 1, BUFFER, IERR)
         IF (IERR.EQ.0) GO TO 220
            WRITE (MSGTXT,1120) FTYP(IT), IERR, IVOL, 1
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
            GO TO 235
 220     IC = 2 + CMAX
         BUFFER(3) = 0
         DO 225 I = 1,256,NL
            BUFFER(I) = -1
 225        CONTINUE
         DO 230 I = IC,NREC
            IRNO = I
            CALL ZFIO ('WRIT', LUN, FIND, IRNO, BUFFER, IERR)
            IF (IERR.EQ.0) GO TO 230
               WRITE (MSGTXT,1120) FTYP(IT), IERR, IVOL, I
               CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
               GO TO 235
 230        CONTINUE
 235     CALL ZCLOSE (LUN, FIND, IERR)
 240     CONTINUE
C-----------------------------------------------------------------------
C                                       Task data communications
 250  IT = 4
      ISIZE = 4 * 35 + 1
      CALL ZPHFIL (FTYP(IT), 1, 0, 4, PHNAME, IERR)
      CALL ZDESTR (1, PHNAME, IERR)
      CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
      IF (IERR.EQ.0) GO TO 255
         WRITE (MSGTXT,1100) FTYP(IT), IERR
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
         GO TO 300
 255  CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.EQ.0) GO TO 260
         WRITE (MSGTXT,1110) FTYP(IT), IERR
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
         GO TO 300
 260  ZBUF(256) = 4
      CALL ZFIO ('WRIT', LUN, FIND, 1, ZBUF, IERR)
      ZBUF(256) = 0
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1120) FTYP(IT), IERR
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
         END IF
      CALL ZCLOSE (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C                                       Image catalogs
C                                       Television devices: no files
C                                       needed
 300  IT = 5
      IC = NTVDEV
C                                       Graphics devices
 325  CONTINUE
         NREC = 1295
         ISIZE = NREC
         CALL ZPHFIL (FTYP(IT), 1, 0, 0, PHNAME, IERR)
         CALL ZDESTR (1, PHNAME, IERR)
         CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
         IF (IERR.EQ.0) GO TO 330
            WRITE (MSGTXT,1100) FTYP(IT), IERR
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
            GO TO 350
 330     CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
         IF (IERR.EQ.0) GO TO 335
            WRITE (MSGTXT,1110) FTYP(IT), IERR
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
            GO TO 350
 335     DO 340 I = 1,1295
            CALL ZFIO ('WRIT', LUN, FIND, I, ZBUF, IERR)
            IF (IERR.EQ.0) GO TO 340
               WRITE (MSGTXT,1120) FTYP(IT), IERR, I
               CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
               GO TO 345
 340        CONTINUE
 345     CALL ZCLOSE (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C                                       Message files: user #1
 350  IT = 6
      NL = 255 / 31
      NREC = 4
      ISIZE = NREC
      CALL FILL (256, 0, BUFFER)
      IVOL = 1
      CALL ZPHFIL (FTYP(IT), IVOL, NLUSER, 0, PHNAME, IERR)
      CALL ZCREAT (IVOL, PHNAME, ISIZE, F, LSIZE, IERR)
      IF (IERR.EQ.0) GO TO 355
         IF (IERR.EQ.1) GO TO 353
            WRITE (MSGTXT,1100) FTYP(IT), IERR
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
            GO TO 400
 353     CONTINUE
            WRITE (MSGTXT,1353)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
            GO TO 400
C
 355  BUFFER(2) = LSIZE
      CALL ZOPEN (LUN, FIND, IVOL, PHNAME, F, T, T, IERR)
      IF (IERR.EQ.0) GO TO 360
         WRITE (MSGTXT,1110) FTYP(IT), IERR
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
         GO TO 400
 360  CALL ZFIO ('WRIT', LUN, FIND, 1, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) FTYP(IT), IERR
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
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
         CALL ZDESTR (1, PHNAME, IERR)
         CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
         IF (IERR.EQ.0) GO TO 410
            WRITE (MSGTXT,1100) FTYP(IT), IERR, J
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
 410     CONTINUE
C-----------------------------------------------------------------------
C                                       System parameters file
 450  IT = 8
      NREC = 1
      ISIZE = NREC
      CALL ZPHFIL (FTYP(IT), 1, 0, 0, PHNAME, IERR)
      CALL ZDESTR (1, PHNAME, IERR)
      CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
      IF (IERR.EQ.0) GO TO 455
         WRITE (MSGTXT,1100) FTYP(IT), IERR
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
         GO TO 500
 455  CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.EQ.0) GO TO 460
         WRITE (MSGTXT,1110) FTYP(IT), IERR
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
         GO TO 500
 460  CALL FILL (256, 0, BUFFER)
      BUFFER(1) = NVOL
      BUFFER(2) = NTAPED
C                                       Num of lines/CRT page
      BUFFER(3) = ANSWER(8) + .01
C                                       Num of lines/printer page.
      BUFFER(4) = ANSWER(9) + .01
      BUFFER(5) = NBATQS
C                                       Plotter num of X dots/page
      BUFFER(6) = ANSWER(10) + .01
C                                       Plotter num of Y dots/page
      BUFFER(7) = ANSWER(11) + .01
C                                       Plotter num of X dots/character
      BUFFER(8) = ANSWER(12) + .01
C                                       Plotter num of Y dots/character
      BUFFER(9) = ANSWER(13) + .01
      BUFFER(10) = NINTRN
C                                       Words in AP.
C                                       Zero won't work even for pseudo
      BUFFER(11) = ANSWER(16) + .01
      IF (BUFFER(11).EQ.0) BUFFER(11) = KAPWRD
      BUFFER(11) = MIN (BUFFER(11), KAPWRD)
      BUFFER(12) = NTVDEV
      BUFFER(13) = NTKDEV
C                                       Num users allowed acc to TVs
      BUFFER(64) = TVANS(1)
C                                       Num users allowed acc to TKs
      BUFFER(65) = TKANS(1) + .01
      BUFFER(66) = UCTSIZ
C                                       Max user number.
      BUFFER(67) = ANSWER(7) + .01
      BUFFER(68) = 132
C                                       words in 2nd AP memory
      BUFFER(69) = 0
C                                       Floating pt values.
      XPRDMM = ANSWER(14)
      XTKDMM = TKANS(2)
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
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1120) FTYP(IT), IERR
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
         END IF
      CALL ZCLOSE (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C                                       Accounting
 500  IT = 9
      NREC = 100
      ISIZE = NREC
      CALL FILL (256, 0, BUFFER)
      BUFFER(3) = 9
      BUFFER(4) = 256 / BUFFER(3)
      CALL ZPHFIL (FTYP(IT), 1, 0, 0, PHNAME, IERR)
      CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
      IF (IERR.EQ.0) GO TO 510
         IF (IERR.EQ.1) GO TO 505
            WRITE (MSGTXT,1100) FTYP(IT), IERR
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
            GO TO 550
 505     CONTINUE
            WRITE (MSGTXT,1505)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
            GO TO 550
 510  NREC = LSIZE
      I4T = NREC * BUFFER(4) - 1
      BUFFER(2) = I4T
      CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.EQ.0) GO TO 520
         WRITE (MSGTXT,1110) FTYP(IT), IERR
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
         GO TO 550
 520  CALL ZFIO ('WRIT', LUN, FIND, 1, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1120) FTYP(IT), IERR
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
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
            CALL ZDESTR (1, PHNAME, IERR)
            CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
            IF (IERR.EQ.0) GO TO 560
               WRITE (MSGTXT,1100) FTYP(IT), IERR
               CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
 560        CONTINUE
C-----------------------------------------------------------------------
C                                       Gripes
 600  IT = 11
      NREC = 100
      ISIZE = NREC
      CALL FILL (256, 0, BUFFER)
      BUFFER(3) = 2
      BUFFER(4) = 1
      CALL ZPHFIL (FTYP(IT), 1, 0, 0, PHNAME, IERR)
      CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
      IF (IERR.EQ.0) GO TO 610
         IF (IERR.EQ.1) GO TO 605
            WRITE (MSGTXT,1100) FTYP(IT), IERR
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
            GO TO 650
 605     CONTINUE
            WRITE (MSGTXT,1605)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
            GO TO 650
 610  BUFFER(1) = LSIZE
      CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.EQ.0) GO TO 620
         WRITE (MSGTXT,1110) FTYP(IT), IERR
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
         GO TO 650
 620  CALL ZFIO ('WRIT', LUN, FIND, 1, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1120) FTYP(IT), IERR
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
         END IF
      CALL ZCLOSE (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C                                       Password file
 650  IT = 12
      I = 3
      J = 256 / I
      NREC = (USELIM-1) / J + 1
      ISIZE = NREC
      CALL ZPHFIL (FTYP(IT), 1, 0, 0, PHNAME, IERR)
      CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IERR)
      IF (IERR.EQ.0) GO TO 660
C                                       Creation error other than
C                                       the file already exists.
         IF (IERR.EQ.1) GO TO 655
            WRITE (MSGTXT,1100) FTYP(IT), IERR
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
            GO TO 700
C                                       PW file exists. Do not init.
 655     CONTINUE
            WRITE (MSGTXT,1655)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
            GO TO 700
C
 660  LPASS = ' '
      CALL PASENC (LPASS, APASS)
      J = 3 * J
      DO 665 I = 1,J,3
         CALL RCOPY (3, APASS, ROBUFF(I))
 665     CONTINUE
      CALL PASENC (MPASS, ROBUFF(1))
      CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.EQ.0) GO TO 670
         WRITE (MSGTXT,1110) FTYP(IT), IERR
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
         GO TO 700
 670  DO 680 I = 1,NREC
         IRNO = I
         CALL ZFIO ('WRIT', LUN, FIND, IRNO, BUFFER, IERR)
         IF (I.EQ.1) CALL RCOPY (3, APASS, ROBUFF(1))
         IF (IERR.EQ.0) GO TO 680
            WRITE (MSGTXT,1120) FTYP(IT), IERR
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
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
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
         GO TO 750
 705  CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.EQ.0) GO TO 710
         WRITE (MSGTXT,1110) FTYP(IT), IERR
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
         GO TO 750
 710  DO 715 IRNO = 1,18
         CALL ZFIO ('WRIT', LUN, FIND, IRNO, ZBUF, IERR)
         IF (IERR.EQ.0) GO TO 715
            WRITE (MSGTXT,1120) FTYP(IT), IERR
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
            GO TO 720
 715     CONTINUE
 720  CALL ZCLOSE (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C                                       Enough for now!!
 750  GO TO 900
C
 900  WRITE (MSGTXT,1900)
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
      GO TO 995
 990  WRITE (MSGTXT,1990) IERR
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGTXT, IERR)
C
 995  CALL ZCLOSE (TTYLUN, TTYIND, IERR)
      CALL ACOUNT (2)
C
 999  STOP
C-----------------------------------------------------------------------
 1001 FORMAT (F10.4)
 1000 FORMAT ('** ERROR **. CANNOT FIND FILE SYSPARM.  IERR=', I7)
 1030 FORMAT ('** ERROR',I7,' FROM ZEXIST ON PASSWORD FILE ***')
 1100 FORMAT ('FILE TYPE ',A2,' CREATE ERR',4I7)
 1110 FORMAT ('FILE TYPE ',A2,' OPEN ERR',4I7)
 1120 FORMAT ('FILE TYPE ',A2,' WRITE ERR',4I7)
 1205 FORMAT ('Catalog file on vol', I3,
     *   ' already exists. not superseded.')
 1353 FORMAT ('Message file user 1 already exists. Not superseded.')
 1505 FORMAT ('Accounting file already exists, not superseded.')
 1605 FORMAT ('Gripe file already exists, not superseded.')
 1655 FORMAT ('Password file already exists. Not superseded.')
 1900 FORMAT ('FILAI2: File creation/init complete')
 1990 FORMAT ('ERROR',I7,' IN TERMINAL IO')
      END
