      PROGRAM GR2TEX
C-----------------------------------------------------------------------
C! converts gripe file or GRITP tape to TEX-like grip file on disk
C# Service
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2004
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
C   GR2TEX is a service program to read either the local AIPS Gripe file
C   or the tape prepared by GRITP from other computers.  The input is
C   converted to a Fortran text file suitable for input to TEX. The file
C   is written to a directory which is specified by the value of the
C   logical symbol "TARGET".  Under VMS, TARGET can even include a
C   DECnet specification, e.g.
C        Define TARGET "CVAX""DON""::UMA0:[DON.AIPGRIP]"
C-----------------------------------------------------------------------
      CHARACTER PHNAME*48, IBSL*2, ICR*1, ILF*1, TEXT*80, PRGNAM*6,
     *   STR*12, FNAME*128, LINE*80, CBUF*2880, SAVEIT*80, SAVEID*80,
     *   ANSW*1, MSGBUF*72, ATIME*8, ADATE*12, SBUF*2880, REMHOS*24,
     *   LMSG*80
      HOLLERITH HBUF(720)
      INTEGER   SCR(256), TVOL, TLUN, TIND, DIND, DLUN, IERR, II, I, IC,
     *   IBUF(720), LREC, LPOS, LBYTES, BRTY, NMAX, INPO, ITRIM, TXLUN,
     *   TXIND, ITY, OUPO, LSAVE, ISAVE, NGRIPE, IREC, TTY(2), ITIME(3),
     *   IDATE(3), IDENS, REMTAP, SYSERR
      LOGICAL   T, F, TAPE, QUIT, NOANSW
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (IBUF, HBUF)
      DATA TXLUN, DLUN /10,27/
      DATA IBSL /'\\'/
      DATA T, F /.TRUE.,.FALSE./
      DATA PRGNAM /'GR2TEX'/
C-----------------------------------------------------------------------
      ICR = CHAR (10)
      ILF = CHAR (13)
C                                       AIPS init
      TIND = 0
      DIND = 0
      TTY(1) = 5
      CALL AIPINI (TTY, PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Talk to user
 10   WRITE (MSGBUF,1000)
      CALL INQINT (TTY, MSGBUF, 1, TVOL, IERR)
      IF (IERR.GT.0) GO TO 980
      IF (IERR.LT.0) GO TO 10
      TAPE = TVOL.GT.0
      QUIT = .FALSE.
C                                       Open disk
      IF (.NOT.TAPE) THEN
         WRITE (MSGBUF,1010)
         CALL INQSTR (TTY, MSGBUF, 48, PHNAME, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL CHLTOU (48, PHNAME)
         IF (PHNAME.EQ.' ') CALL ZPHFIL ('GR', 1, 0, 0, PHNAME, IERR)
         CALL ZOPEN (DLUN, DIND, 1, PHNAME, F, T, T, IERR)
         IF (IERR.NE.0) THEN
            DIND = 0
            GO TO 980
            END IF
         CALL ZFIO ('READ', DLUN, DIND, 1, IBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         LREC = IBUF(3)
         LPOS = IBUF(4) - 1
         IF (IBUF(2).LE.0) THEN
            WRITE (MSGTXT,1011)
            CALL MSGWRT (2)
            GO TO 980
            END IF
         IREC = 1
         IF (LPOS.LE.0) THEN
            LPOS = 1024
            LREC = LREC - 1
            END IF
C                                       Open tape
      ELSE
         TLUN = 129 - TVOL
         CALL ZPHFIL ('MT', TVOL, 0, 0, PHNAME, IERR)
         CALL ZTPOPN (TLUN, TIND, TVOL, PHNAME, 'READ', IERR)
         IF (IERR.NE.0) THEN
            TIND = 0
            GO TO 980
            END IF
         IREC = 0
C                                       Mount tape
 20      MSGBUF = 'Mount at density = ????;   0 or N => don''t mount'
         CALL INQINT (TTY, MSGBUF, 1, IDENS, IERR)
         IF (IERR.GT.0) GO TO 980
         IF ((IERR.EQ.0) .AND. (IDENS.GT.0)) THEN
           IF ((IDENS.NE.800) .AND. (IDENS.NE.1600) .AND.
     *        (IDENS.NE.6250)) GO TO 20
            MSGBUF = 'Enter remote host name - just hit return if local'
            CALL INQSTR (TTY, MSGBUF, 24, REMHOS, IERR)
            IF (IERR.GT.0) GO TO 980
            IF (REMHOS(1:1).NE.' ') THEN
               MSGBUF = 'Remote host tape drive number:'
               CALL INQINT (TTY, MSGBUF, 1, REMTAP, IERR)
               IF (IERR.GT.0) GO TO 980
               IF (IERR.LT.0) THEN
                  REMHOS = ' '
                  REMTAP = 0
                  END IF
               END IF
C                                       Mount
            CALL ZMOUNT (.TRUE., TVOL, IDENS, REMHOS, REMTAP, LMSG,
     *         SYSERR, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1015) IERR
               CALL MSGWRT (8)
               GO TO 980
               END IF
            END IF
C                                       Position tape
 25      MSGBUF = 'Tape file number to read;  0 => current'
         CALL INQINT (TTY, MSGBUF, 1, I, IERR)
         IF (IERR.LT.0) GO TO 25
         IF (IERR.GT.0) GO TO 980
         IC = 1
         IF (I.GT.0) THEN
            CALL ZTAPE ('REWI', TLUN, TIND, IC, IERR)
            IF (IERR.NE.0) GO TO 980
            I = I - 1
            IF (I.GT.0) CALL ZTAPE ('ADVF', TLUN, TIND, I, IERR)
         ELSE
            CALL ZTAPE ('BAKF', TLUN, TIND, IC, IERR)
            END IF
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       More inits
      BRTY = 1
C                                       Create output file
      MSGBUF = 'Output file name, without the .TEX, <= 12 characters'
      CALL INQSTR (TTY, MSGBUF, 12, STR, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL CHLTOU (12, STR)
      IC = ITRIM (STR)
      FNAME = 'TARGET:' // STR(:IC) // '.TEX'
      CALL ZTXOPN ('WRIT', TXLUN, TXIND, FNAME, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         IC = ITRIM (FNAME)
         IC = MIN (IC, 58)
         WRITE (MSGTXT,1040) IERR, FNAME(:IC)
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                       TEX initial lines
      NGRIPE = 10000
      TEXT = IBSL(1:1) // 'input docgrip:gripemac5.tex'
      IC = ITRIM (TEXT)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, TEXT(:IC), IERR)
      IF (IERR.NE.0) GO TO 980
      TEXT = IBSL(1:1) // 'def' // IBSL(1:1) // 'finalrun{F}'
      IC = ITRIM (TEXT)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, TEXT(:IC), IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Initialize date string:
      CALL ZTIME (ITIME)
      CALL ZDATE (IDATE)
      CALL TIMDAT (ITIME, IDATE, ATIME, ADATE)
C                                       READ: internal procedure
 50   IREC = IREC + 1
      INPO = 0
      IF (QUIT) GO TO 200
      IF (.NOT.TAPE) THEN
         CALL ZFIO ('READ', DLUN, DIND, IREC, IBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         QUIT = IREC.EQ.LREC
         NMAX = 1024
         IF (QUIT) NMAX = LPOS
         CALL H2CHR (NMAX, 1, HBUF, CBUF)
C                                       Tape read
      ELSE
         CALL ZTPMIO ('READ', TLUN, TIND, 2880, IBUF, 1, IERR)
         IF (IERR.EQ.0) CALL ZTPWAT (TLUN, TIND, 1, LBYTES, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL ZC8CL (2880, 1, IBUF, SBUF)
         NMAX = 0
         DO 80 I = 1,2880,80
            TEXT = SBUF(I:I+79)
            IF (TEXT(1:1).NE.' ') THEN
               QUIT = TEXT(:8).EQ.'END '
               IF ((QUIT) .AND. (NMAX.EQ.0)) GO TO 200
               IF (QUIT) GO TO 90
            ELSE
               DO 70 II = 9,80
                  NMAX = NMAX + 1
                  CBUF(NMAX:NMAX) = TEXT(II:II)
                  IF ((NMAX.EQ.2) .AND. (CBUF(2:2).NE.IBSL(1:1)) .AND.
     *               (CBUF(1:1).EQ.IBSL(1:1))) CBUF(1:1) = ILF
                  IF ((NMAX.GT.1) .AND. (CBUF(NMAX:NMAX).EQ.IBSL(1:1)))
     *               THEN
                     IF ((I+II.EQ.2881) .AND. (CBUF(NMAX-1:NMAX-1)
     *                  .NE.IBSL(1:1))) CBUF(NMAX:NMAX) = ICR
                     IF (CBUF(NMAX-1:NMAX-1).EQ.IBSL(1:1)) THEN
                        CBUF(NMAX-1:NMAX-1) = ICR
                        CBUF(NMAX:NMAX) = ILF
                        END IF
                     END IF
 70               CONTINUE
               END IF
 80         CONTINUE
         END IF
C-----------------------------------------------------------------------
C                                       Branch by type of record out
 90   GO TO (100, 115, 125, 145), BRTY
C                                       New gripe
 100  CONTINUE
         BRTY = 1
         ITY = 0
C                                       New argument
 110     ITY = ITY + 1
         IF (ITY.GE.11) GO TO 100
C                                       Find first char
 115  CONTINUE
         BRTY = 2
         INPO = INPO + 1
         IF (INPO.GT.NMAX) GO TO 50
         IF (CBUF(INPO:INPO).EQ.' ') GO TO 115
         OUPO = 1
         LINE = ' '
         BRTY = 4
         IF (ITY.EQ.7) GO TO 140
         BRTY = 3
C                                       Normal argument
 120     LINE(OUPO:OUPO) = CBUF(INPO:INPO)
         IF (CBUF(INPO:INPO).EQ.'}') GO TO 130
C                                       Advance to next character
 125  CONTINUE
         INPO = INPO + 1
         IF (INPO.GT.NMAX) GO TO 50
         OUPO = MIN (OUPO+1, 80)
         GO TO 120
C                                       write out new gripe
 130  IF (ITY.EQ.1) THEN
         NGRIPE = NGRIPE + 1
         TEXT = ' '
         CALL ZTXIO ('WRIT', TXLUN, TXIND, TEXT(:2), IERR)
         IF (IERR.NE.0) GO TO 980
         WRITE (TEXT,1130) IBSL(1:1) // 'count0=', NGRIPE
         IC = ITRIM (TEXT)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, TEXT(:IC), IERR)
         IF (IERR.NE.0) GO TO 980
         TEXT = IBSL(1:1) // 'def' // IBSL(1:1) // 'rda{' // ADATE(:11)
     *      // '}'
         IC = ITRIM (TEXT)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, TEXT(:IC), IERR)
         IF (IERR.NE.0) GO TO 980
         TEXT = IBSL(1:1) // 'gripe'
         IC = ITRIM (TEXT)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, TEXT(:IC), IERR)
         IF (IERR.NE.0) GO TO 980
         SAVEIT = LINE(:OUPO)
         LSAVE  = OUPO
         END IF
      IF (ITY.EQ.6) THEN
         TEXT = '{' // IBSL(1:1) // 'phone' // LINE(:OUPO) //
     *      SAVEID(:ISAVE) // '}'
         IC = ITRIM (TEXT)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, TEXT(:IC), IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      NOANSW = (ITY.EQ.10) .AND. (LINE(:OUPO).EQ.'{ }')
      IF ((ITY.NE.3) .AND. (ITY.NE.6) .AND. (.NOT.NOANSW)) THEN
         TEXT = LINE(:OUPO)
         IC = ITRIM (TEXT)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, TEXT(:IC), IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      IF (NOANSW) THEN
         TEXT = '{' // IBSL(1:1) // 'secret{}%%'
         IC = ITRIM (TEXT)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, TEXT(:IC), IERR)
         IF (IERR.NE.0) GO TO 980
         TEXT = IBSL(1:1) // 'newgripe}'
         IC = ITRIM (TEXT)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, TEXT(:IC), IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      IF (ITY.EQ.3) THEN
         SAVEID = LINE(:OUPO)
         ISAVE  = OUPO
         END IF
      IF (ITY.EQ.4) THEN
         WRITE (MSGTXT,1131) NGRIPE, SAVEIT(:LSAVE), LINE(:OUPO)
         CALL MSGWRT (4)
         END IF
      GO TO 110
C                                       Gripe text
 140  IF (CBUF(INPO:INPO).EQ.ICR) GO TO 150
      IF (CBUF(INPO:INPO).EQ.ILF) OUPO = OUPO - 1
      IF (CBUF(INPO:INPO).EQ.ILF) GO TO 145
      LINE(OUPO:OUPO) = CBUF(INPO:INPO)
      IF (CBUF(INPO:INPO).EQ.'}') GO TO 150
C                                       advance to next character
 145  IF (OUPO.LT.80) THEN
         INPO = INPO + 1
         IF (INPO.GT.NMAX) GO TO 50
         OUPO = OUPO + 1
         GO TO 140
         END IF
C                                       write text
 150  IF (CBUF(INPO:INPO).EQ.ICR) OUPO = OUPO - 1
      TEXT = LINE(:OUPO)
      IC = ITRIM (TEXT)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, TEXT(:IC), IERR)
      IF (IERR.NE.0) GO TO 980
      IF (CBUF(INPO:INPO).EQ.'}') GO TO 110
         OUPO = 0
         GO TO 145
C                                       Done: \end
 200  TEXT = ' '
      CALL ZTXIO ('WRIT', TXLUN, TXIND, TEXT(:2), IERR)
      IF (IERR.NE.0) GO TO 980
      TEXT = IBSL(1:1) // 'end'
      IC = ITRIM (TEXT)
      CALL ZTXIO ('WRIT', TXLUN, TXIND, TEXT(:IC), IERR)
      IF (IERR.NE.0) GO TO 980
      CALL ZTXCLS (TXLUN, TXIND, IERR)
      IF (IERR.LE.0) THEN
         MSGTXT = 'Gripe output file CLOSEd. Safe to init.'
         CALL MSGWRT (4)
      ELSE
         MSGTXT = 'Error CLOSEing gripe output file; try again.'
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                       Init disk file?
 300  IF (TAPE) THEN
C                                       Dismount tape
         MSGBUF = 'Do you wish to dismount the tape? Y or y, else => no'
         CALL INQSTR (TTY, MSGBUF, 1, ANSW, IERR)
         CALL CHLTOU (1, ANSW)
         IF ((IERR.EQ.0) .AND. (ANSW.EQ.'Y')) THEN
            CALL ZMOUNT (.FALSE., TVOL, 0, ' ', 0, LMSG, SYSERR, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1300) IERR
               CALL MSGWRT (8)
               END IF
            END IF
      ELSE
         MSGBUF = 'Initialize the disk file? Y or y,' //
     *      '  anything else => no'
         CALL INQSTR (TTY, MSGBUF, 1, ANSW, IERR)
         CALL CHLTOU (1, ANSW)
C                                       require password
         IF ((IERR.EQ.0) .AND. (ANSW.EQ.'Y')) THEN
            CALL PASWRD (SCR, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL ZFIO ('READ', DLUN, DIND, 1, IBUF, IERR)
            IF (IERR.NE.0) GO TO 980
            IBUF(2) = 0
            IBUF(3) = 2
            IBUF(4) = 1
            CALL ZFIO ('WRIT', DLUN, DIND, 1, IBUF, IERR)
            END IF
         END IF
C
 980  IF (TIND.GT.0) CALL ZTPCLS (TLUN, TIND, IERR)
      IF (DIND.GT.0) CALL ZCLOSE (DLUN, DIND, IERR)
      CALL ACOUNT (2)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('Tape drive number;  0 => disk')
 1010 FORMAT ('Disk file name (default = "DA00:GR100000.;1")')
 1011 FORMAT ('GRIPE FILE IS EMPTY: QUITTING')
 1015 FORMAT ('ZMOUNT RETURNS ERROR',I6)
 1040 FORMAT ('ERROR',I3,' OPENING FILE ',A)
 1130 FORMAT (A,I5.5)
 1131 FORMAT (I5,1X,A,1X,A)
 1300 FORMAT ('ERROR',I5,' DISMOUNTING THE TAPE')
      END
