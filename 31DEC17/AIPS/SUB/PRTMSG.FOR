      SUBROUTINE PRTMSG (OP, USER, PPOPS, PRIOR, PTASK, PTIME, DOCRT,
     *   LPNAME, NDONE, NLEFT, IERR)
C-----------------------------------------------------------------------
C! prints and deletes messages from the MS file
C# POPS-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2004, 2007, 2009, 2015
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
C   PRTMSG prints and compresses the message file.
C   Inputs:
C      OP      C*4   'PRIN' : print selected messages
C                    'DELE' : delete selected messages
C                    'SUMM' : type summary
C      USER    I     User number for whom OP is performed.
C      PPOPS   I     Restrict to POPS number (0 -> all)
C      PRIOR   I     Print messages of priority >= PRIOR only (on <= 5,
C                    print level 0 also) (not on DELE)
C                    22 taken as 0 on PRIN and DELE (old QEXIT)
C      PTASK   C*6   Print/delete only msgs from task(s) whose first
C                    chars match PTASK (non-blanks)
C      PTIME   R     Print only within PTIME days, delete only before
C                    PTIME days
C      DOCRT   R     >0 => print on CRT not line printer, >72 => width
C      LPNAME  C*48  File name to save line printer output.  ' ' means
C                    use a scratch file.  Ignored if DOCRT > 0.
C   Output:
C      NDONE   I     Number messages printed/deleted
C      NLEFT   I     Number messages left in file
C      IERR    I     Error code: 0 -> ok
C                       1 -> error on printer
C                       2 -> error on CRT
C                       3 -> error on disk: open
C                       4 -> error on disk: read
C                       5 -> error on disk: write
C-----------------------------------------------------------------------
      CHARACTER OP*4, PTASK*6, LPNAME*(*)
      INTEGER   USER, PPOPS, PRIOR, NDONE, NLEFT, IERR
      REAL      PTIME, DOCRT
C
      CHARACTER  TITL1*132, TITL2*132, LINE*132, SCRTCH*132, PRGNAM*6,
     *   ATIME*8, ADATE*12, PHNAME*48, LDATE*12, ATASK*6, TNAME*6,
     *   POPSNO*1
      INTEGER   IOP, I, POTERR, JERR, INREC, INBUF(256), II, ITRIM,
     *   MSGNOW, MSGSIZ, NWPR, NLPR, PFIND, MSGFIL, FIND, INPOS, PAGE,
     *   CTR, OUTBUF(256), IT(6), INACT, RLOOP, JJJ, MSGCUR, MSGLEV,
     *   INPOPS, OUTREC, OUTPOS, FCNT, AC, LNUSER, IPRIOR, JERR2,
     *   NACROS, PLUN, NCH, IPCH, ISIZE, NCOL
      REAL      DDCRT
      DOUBLE PRECISION    JD0, JD
      LOGICAL   NOSAVE, F, LONG, QUIT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DIO.INC'
      DATA PRGNAM /'PRTMSG'/
      DATA MSGFIL /12/
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      IPRIOR = PRIOR
      IF (IPRIOR.EQ.22) IPRIOR = 0
      LDATE = ' '
C                                       Check OP
      LNUSER = NLUSER
      IOP = 1
      IF (OP.EQ.'PRIN') IOP = 2
      IF (OP.EQ.'DELE') IOP = 3
C                                       Make sure file closed & ready
      NLUSER = USER
      CALL MSGWRT (-1)
      NLUSER = LNUSER
      I = NINTRN
      IF (NBATQS.GT.0) I = NINTRN + 1 + NBATQS
      IERR = 6
      IF ((IOP.GT.1) .AND. (PPOPS.GT.I)) GO TO 999
C                                       Open the message file.
      POTERR = 52
      IERR = 3
      NLUSER = USER
      CALL ZPHFIL ('MS', 1, NLUSER, 0, PHNAME, JERR)
      NLUSER = LNUSER
      CALL ZMSGOP (MSGFIL, FIND, 1, PHNAME, JERR)
      IF (JERR.NE.0) GO TO 980
C                                       Read 1st rec to get no of msgs.
      INREC = 1
      POTERR = 50
      IERR = 4
      CALL ZMSGDK ('READ', MSGFIL, FIND, INREC, INBUF, JERR)
      IF (JERR.NE.0) GO TO 950
      MSGNOW = INBUF(1)
      MSGSIZ = INBUF(2)
      NDONE = 0
      NLEFT = MSGNOW
      NWPR = 25
      NLPR = 255 / NWPR
C                                       Get current time
      CALL ZDATE (IT(1))
      CALL ZTIME (IT(4))
      CALL DAT2JD (IT, JD0)
      PFIND = 0
C                                       Branch to operation
      GO TO (100, 200, 300), IOP
C-----------------------------------------------------------------------
C                                       SUMMary
C                                       number of entries...
C-----------------------------------------------------------------------
 100  CALL ZMSGCL (MSGFIL, FIND, JERR)
 105  IERR = 0
      INPOS = 4
      IF (MSGNOW.GT.0) THEN
         MSGNOW = MSGNOW + 1
         CALL CATIME (2, INBUF(INPOS), IT)
         CALL TIMDAT (IT(4), IT(1), ATIME, ADATE)
         WRITE (MSGTXT,1100) USER, MSGNOW, ADATE, ATIME
         CALL MSGWRT (5)
         CALL DAT2JD (IT, JD)
         IF (JD0-JD.GE.TIMEMS) THEN
            WRITE (MSGTXT,1101)
            CALL MSGWRT (5)
            END IF
      ELSE
         WRITE (MSGTXT,1110) USER
         CALL MSGWRT (1)
         END IF
      GO TO 999
C-----------------------------------------------------------------------
C                                       PRINt
C                                       print messages on some device
C-----------------------------------------------------------------------
 200  IERR = 6
      IF (IPRIOR.GT.10) GO TO 950
      IF (MSGNOW.LE.0) GO TO 100
      PAGE = 0
      CTR = 900
      POTERR = 57
C                                       Open the line printer.
      DDCRT = DOCRT
      IF ((ISBTCH.EQ.32000) .OR. (TSKNAM(1:5).EQ.'AIPSC'))
     *   DDCRT = MIN (-1.0, DOCRT)
      IF (PTIME.LE.0.0) JD0 = 0.0D0
      JD0 = JD0 - PTIME
      CALL ZMSGCL (MSGFIL, FIND, JERR)
      IF (DDCRT.LE.0.0) THEN
         IERR = 1
         IF (LPNAME.EQ.' ') DDCRT = -1.0
C                                       double check large jobs
         IF ((LPNAME.EQ.' ') .AND. (MSGNOW.GT.500) .AND. ((IUNIT.EQ.1)
     *      .OR. (IUNIT.EQ.4))) THEN
C                                       count to see job size
            NDONE = 0
            RLOOP = (MSGNOW-1) / NLPR + 1
            RLOOP = MIN (RLOOP, MSGSIZ)
            POTERR = 50
            CALL ZMSGOP (MSGFIL, FIND, 1, PHNAME, JERR)
            IF (JERR.NE.0) GO TO 980
            DO 220 INREC = 1,RLOOP
C                                       Read message file
               CALL ZMSGDK ('READ', MSGFIL, FIND, INREC, INBUF, JERR)
               IF (JERR.NE.0) GO TO 950
C                                       Loop on all recs in sector.
               DO 210 II = 1,NLPR
                  MSGCUR = MSGCUR + 1
                  IF (MSGCUR.GT.MSGNOW) GO TO 230
                  INPOS = (II-1) * NWPR + 3
                  MSGLEV = MOD (INBUF(INPOS), 16)
                  IF ((MSGLEV.LT.IPRIOR) .AND. (MSGLEV.NE.0)) GO TO 210
                  IF ((MSGLEV.EQ.0) .AND. (IPRIOR.GT.5)) GO TO 210
                  INPOPS = INBUF(INPOS) / 16
                  CALL ZEHEX (INPOPS, 1, POPSNO)
                  IF ((PPOPS.GT.0) .AND. (PPOPS.NE.INPOPS)) GO TO 210
                  CALL CATIME (2, INBUF(INPOS+1), IT)
                  CALL DAT2JD (IT, JD)
                  IF (JD-JD0.LT.0.0) GO TO 210
                  CALL H2CHR (6, 1, INBUF(INPOS+3), TNAME)
                  IF (INACT.GT.0) THEN
                     IF (TNAME(1:INACT).NE.ATASK(1:INACT)) GO TO 210
                     END IF
C                                       We want to print it
                  NDONE = NDONE + 1
 210              CONTINUE
 220           CONTINUE
 230        CALL ZMSGCL (MSGFIL, FIND, JERR)
            IF (NDONE.GT.400) THEN
               WRITE (MSGTXT,1230) NDONE
               CALL MSGWRT (3)
               CALL CONFRM (JERR)
               IF (JERR.GT.0) GO TO 999
               END IF
            END IF
C                                       open print device
         IF (LPNAME.EQ.' ') DDCRT = -1.0
         CALL LPOPEN (LPNAME, DDCRT, PLUN, PFIND, NACROS, OUTBUF, JERR)
         IF (JERR.NE.0) PFIND = 0
         IF (JERR.NE.0) GO TO 950
C                                       Open terminal instead
      ELSE
         IERR = 2
         PLUN = 5
         CALL LSERCH ('SRCH', PLUN, PFIND, F, JERR)
         IF (JERR.NE.0) PFIND = 0
         IF (JERR.NE.0) GO TO 950
         CALL ZWINC (NCOL)
         NACROS = DDCRT + 0.5
         IF ((NACROS.LT.72) .OR. (NACROS.GT.NCOL)) NACROS = NCOL
         END IF
C                                       Init other parms
      LONG = (NACROS.GE.128)
      IF (LONG) THEN
         WRITE (TITL1,1242) USER
      ELSE IF (NACROS.LT.80) THEN
         WRITE (TITL1,1240) USER
      ELSE
         WRITE (TITL1,1241) USER
         END IF
      TITL2 = ' '
      ATASK = PTASK
      INACT = ITRIM (ATASK)
C                                       Loop to print
      MSGCUR = 0
      RLOOP = (MSGNOW-1) / NLPR + 1
      RLOOP = MIN (RLOOP, MSGSIZ)
      POTERR = 50
      CALL ZMSGOP (MSGFIL, FIND, 1, PHNAME, JERR)
      IF (JERR.NE.0) GO TO 980
C                                       print header once for -3
      IF (DDCRT.LE.-2.5) THEN
         CALL PRTALN (PLUN, PFIND, DDCRT, NACROS, TITL1, TITL2, TITL1,
     *      CTR, PAGE, SCRTCH, QUIT)
         IF ((QUIT) .OR. (ERRNUM.GT.0)) GO TO 260
         END IF
      NDONE = 0
      DO 250 INREC = 1,RLOOP
C                                       Read message file
         CALL ZMSGDK ('READ', MSGFIL, FIND, INREC, INBUF, JERR)
         IF (JERR.NE.0) GO TO 950
C                                       Loop on all recs in sector.
         DO 240 II = 1,NLPR
            MSGCUR = MSGCUR + 1
            IF (MSGCUR.GT.MSGNOW) GO TO 260
            INPOS = (II-1) * NWPR + 3
            MSGLEV = MOD (INBUF(INPOS), 16)
            IF ((MSGLEV.LT.IPRIOR) .AND. (MSGLEV.NE.0)) GO TO 240
            IF ((MSGLEV.EQ.0) .AND. (IPRIOR.GT.5)) GO TO 240
            INPOPS = INBUF(INPOS) / 16
            CALL ZEHEX (INPOPS, 1, POPSNO)
            IF ((PPOPS.GT.0) .AND. (PPOPS.NE.INPOPS)) GO TO 240
            CALL CATIME (2, INBUF(INPOS+1), IT)
            CALL DAT2JD (IT, JD)
            IF (JD-JD0.LT.0.0) GO TO 240
            CALL H2CHR (6, 1, INBUF(INPOS+3), TNAME)
            IF (INACT.GT.0) THEN
               IF (TNAME(1:INACT).NE.ATASK(1:INACT)) GO TO 240
               END IF
            CALL TIMDAT (IT(4), IT(1), ATIME, ADATE)
C                                       We want to print it
            NDONE = NDONE + 1
C                                       Unpack buffer into values.
            IF (LONG) THEN
               WRITE (LINE,1245) INPOPS, MSGLEV, ADATE, ATIME, TNAME
               CALL H2CHR (80, 1, INBUF(INPOS+5), LINE(49:132))
            ELSE IF (NACROS.GE.80) THEN
               IF (ADATE.NE.LDATE) THEN
                  IF (DDCRT.GT.-2.5) THEN
                     WRITE (LINE,1250) ADATE
                     CALL PRTALN (PLUN, PFIND, DDCRT, NACROS, TITL1,
     *                  TITL2, LINE, CTR, PAGE, SCRTCH, QUIT)
                     IF ((QUIT) .OR. (ERRNUM.GT.0)) GO TO 260
                     END IF
                  LDATE = ADATE
                  END IF
               WRITE (LINE,1252) TNAME(1:5), POPSNO, ATIME
               NCH = NACROS - 16
               IF (NCH.GT.80) NCH = 80
               CALL H2CHR (NCH, 1, INBUF(INPOS+5), LINE(17:132))
               CALL PRTALN (PLUN, PFIND, DDCRT, NACROS, TITL1, TITL2,
     *            LINE, CTR, PAGE, SCRTCH, QUIT)
               IF ((QUIT) .OR. (ERRNUM.GT.0)) GO TO 260
               IPCH = NCH + 1
               NCH = 80 - NCH
               IF (NCH.LE.0) GO TO 240
                  LINE = ' '
                  CALL H2CHR (NCH, IPCH, INBUF(INPOS+5), LINE(17:132))
                  AC = ITRIM (LINE)
                  IF (AC.LE.16) GO TO 240
            ELSE
               LINE = TNAME
               LINE(6:8) = POPSNO
               CALL H2CHR (64, 1, INBUF(INPOS+5), LINE(9:))
               IERR = 2
               END IF
            CALL PRTALN (PLUN, PFIND, DDCRT, NACROS, TITL1, TITL2,
     *         LINE, CTR, PAGE, SCRTCH, QUIT)
            IF ((QUIT) .OR. (ERRNUM.GT.0)) GO TO 260
 240        CONTINUE
 250     CONTINUE
C                                       Done: message summary
 260  CALL ZMSGDK ('READ', MSGFIL, FIND, 1, INBUF, JERR)
      IERR = 4
      IF (JERR.NE.0) GO TO 950
C                                       close MSG before LP close
      CALL ZMSGCL (MSGFIL, FIND, JERR)
      IF (DDCRT.LE.0.0) CALL LPCLOS (PLUN, PFIND, CTR, JERR)
      GO TO 105
C-----------------------------------------------------------------------
C                                       DELEte
C                                       delete some/all messages
C-----------------------------------------------------------------------
 300  ATASK = PTASK
      INACT = ITRIM (ATASK)
C                                       Loop to print & compress.
      IF (MSGNOW.LE.0) GO TO 100
      OUTREC = 1
      MSGCUR = 0
      FCNT = 0
      NOSAVE = .FALSE.
      OUTPOS = 3
      RLOOP = (MSGNOW-1) / NLPR + 1
      RLOOP = MIN (RLOOP, MSGSIZ)
      DO 350 INREC = 1,RLOOP
C                                       Read message file
         IERR = 4
         CALL ZMSGDK ('READ', MSGFIL, FIND, INREC, INBUF, JERR)
         IF (JERR.NE.0) GO TO 950
C                                       Loop on all recs in sector.
         DO 340 II = 1,NLPR
            MSGCUR = MSGCUR + 1
            IF (MSGCUR.GT.MSGNOW) GO TO 350
            INPOS = (II-1) * NWPR + 3
C                                       Check if record should be saved
            INPOPS = INBUF(INPOS) / 16
            NDONE = NDONE + 1
            IF ((INPOPS.LE.0) .OR. (INPOPS.GT.35)) GO TO 330
            IF ((PPOPS.GT.0) .AND. (PPOPS.NE.INPOPS)) GO TO 320
            CALL CATIME (2, INBUF(INPOS+1), IT)
            CALL DAT2JD (IT, JD)
            IF (JD0-JD.LT.PTIME) GO TO 320
            IF (INACT.LE.0) GO TO 330
               CALL H2CHR (6, 1, INBUF(INPOS+3), TNAME)
               IF (TNAME(1:INACT).EQ.ATASK(1:INACT)) GO TO 330
C                                       Save this record.
C                                       No copy required
 320        IF ((INREC.EQ.OUTREC) .AND. (INPOS.EQ.OUTPOS)) THEN
               FCNT = FCNT + 1
               NDONE = NDONE - 1
               OUTPOS = OUTPOS + NWPR
C                                       Write full output buffer
               IF (OUTPOS.GE.NWPR*NLPR) THEN
                  OUTREC = OUTREC + 1
                  OUTPOS = 3
                  END IF
C                                       Copy required
            ELSE
               CALL COPY (NWPR, INBUF(INPOS), OUTBUF(OUTPOS))
               FCNT = FCNT + 1
               NDONE = NDONE - 1
               OUTPOS = OUTPOS + NWPR
C                                       Write full output buffer
               IF (OUTPOS.GE.NWPR*NLPR) THEN
                  IERR = 5
                  CALL ZMSGDK ('WRIT', MSGFIL, FIND, OUTREC, OUTBUF,
     *               JERR)
                  OUTREC = OUTREC + 1
                  OUTPOS = 3
                  IF (JERR.NE.0) GO TO 950
                  END IF
               END IF
            GO TO 340
C                                       On 1st delete move old rec
 330        IF (NOSAVE) GO TO 340
               NOSAVE = .TRUE.
               CALL COPY (256, INBUF, OUTBUF)
 340        CONTINUE
 350     CONTINUE
C                                       Write if output buf not empty.
      IF ((OUTPOS.NE.3) .AND. (NOSAVE)) CALL ZMSGDK ('WRIT', MSGFIL,
     *   FIND, OUTREC, OUTBUF, JERR)
      IERR = 5
      IF (JERR.NE.0) GO TO 950
C                                       Update MSGNOW in first record.
      CALL ZMSGDK ('READ', MSGFIL, FIND, 1, INBUF, JERR)
      IF (JERR.NE.0) GO TO 950
      INBUF(1) = FCNT
      INBUF(2) = MSGSIZ
      NLEFT = FCNT
      MSGNOW = FCNT
C                                       Compress MS files
      II = (NBPS * 8) / 512
      JJJ = II * NLPR
      ISIZE = ((MAX (JJJ, FCNT)-1) / NLPR + 1) / II + 1
      ISIZE = II * ISIZE
      IF (ISIZE.LT.INBUF(2)) THEN
         CALL ZCMPRS (1, PHNAME, MSGFIL, ISIZE, JERR)
         IF (JERR.EQ.0) INBUF(2) = ISIZE
         END IF
      IERR = 5
      CALL ZMSGDK ('WRIT', MSGFIL, FIND, 1, INBUF, JERR)
      IF (JERR.NE.0) GO TO 950
      GO TO 100
C-----------------------------------------------------------------------
C                                       Close all files: ERROR
 950  CALL ZMSGCL (MSGFIL, FIND, JERR2)
      IF (IERR.EQ.1) WRITE (MSGTXT,1950) JERR
      IF (IERR.EQ.2) WRITE (MSGTXT,1951) JERR
      IF (IERR.EQ.3) WRITE (MSGTXT,1952) JERR
      IF (IERR.EQ.4) WRITE (MSGTXT,1953) JERR
      IF (IERR.EQ.5) WRITE (MSGTXT,1954) JERR
      IF (IERR.EQ.6) WRITE (MSGTXT,1955) IPRIOR, PPOPS
      CALL MSGWRT (8)
C                                       Error
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('User',I5,':',I6,' messages, oldest written ',A12,1X,A8)
 1101 FORMAT ('Some of these will be deleted on EXIT or KLEENEX')
 1110 FORMAT ('User',I5,' message file is empty')
 1230 FORMAT ('Printer job of',I7,' lines: do you mean it?')
 1240 FORMAT ('Task',5X,'messages for user',I5)
 1241 FORMAT ('task',5X,'time',3X,'messages for user',I5)
 1242 FORMAT ('Pops  Prior    Date',8X,'Time',7X,'Task',10X,'Messages',
     *   ' for user',I5)
 1245 FORMAT (I4,I5,3X,A12,1X,A8,5X,A6)
 1250 FORMAT ('******  ',A12,' ******')
 1252 FORMAT (A5,A1,1X,A8)
 1950 FORMAT ('PRINTER OPEN ERROR',I7)
 1951 FORMAT ('CRT ERROR',I7)
 1952 FORMAT ('MESSAGE FILE OPEN ERROR',I7)
 1953 FORMAT ('MESSAGE FILE READ ERROR',I7)
 1954 FORMAT ('MESSAGE FILE WRITE ERROR',I7)
 1955 FORMAT ('PARAMETER ERROR: PRIO, POPS #',2I7)
      END
