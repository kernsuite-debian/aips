      SUBROUTINE AU4 (BRANCH)
C-----------------------------------------------------------------------
C! verbs to handle basic tape operations: TPHEAD, MOUNT, AVFILE, ...
C# POPS-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-1999, 2003-2004, 2008
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
C   Subroutine to do some tape utility movements
C   Verbs: 1 - TPHEAD:  print header of next file
C          2 - AVFILE:  advance chosen tape drive NFILES files
C                       if NFILES<0 do back file operation
C          3 - AVMAP:   advance chosen tape over NMAPS maps
C          4 - REWIND   rewind tape
C          5 - AVEOT    advance to end-of-tape.
C          6 - MOUNT    Software mount a tape.
C          7 - DISMOUNT Software dismount a tape.
C          8 - TAPES    Show list of tapes
C   Adverbs:
C      INTAPE - tape unit (1,2,...) on which to do operation
C      NFILES - number of files to advance or back-up
C      NMAPS - number of maps to skip
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER PRGNAM*6, FITFIL*48, FITBLK*2880, MACHIN*24, LMSG*80,
     *   CDUM*1, TRYTWO*48
      INTEGER   IC, LTAPE, TBIND, IERR, I, NT, D2NY, IERBOM, IMAP,
     *   POTERR, FITS, IDENS, ITAPE, FDVEC(50), TAPBUF(14592), IDUM,
     *   IRDRIV, IEREOF, SYSERR, ITRIM
      REAL      RDUM
      LOGICAL   DOTAPE
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /AIPSCR/ TAPBUF
      COMMON /AIPCCR/ FITBLK
      DATA IEREOF, IERBOM /4, 5/
      DATA D2NY /18/
      DATA PRGNAM /'AU4   '/
C-----------------------------------------------------------------------
      IF ((BRANCH.LE.0) .OR. (BRANCH.GT.8)) GO TO 999
C                                       Don't need to have tape opened
C                                       for showing list.
      IF (BRANCH.EQ.8) GO TO 20
      CALL ADVERB ('INTAPE', 'I', 1, 0, NT, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 970
      IF (NT.LE.0) NT = 1
      LTAPE = 129 - NT
      POTERR = 32
      IF (NT.GT.NTAPED) GO TO 980
C                                       Mount takes place before open.
C                                       Open tape file
      TRYTWO = ' '
      IF (BRANCH.GT.5) GO TO 20
         CALL FILL (50, 0, FDVEC)
         FDVEC(1) = LTAPE
         FDVEC(2) = 2880
         FDVEC(3) = (14592 * NBITWD) / 8
         FDVEC(5) = NT
         FDVEC(6) = 10
         FDVEC(31) = 0
         FDVEC(32) = 0
         IF (BRANCH.EQ.1) THEN
            CALL ADVERB ('DATAIN', 'C', 1, 48, IDUM, RDUM, FITFIL)
            IF (ERRNUM.NE.0) GO TO 970
            DOTAPE = FITFIL.EQ.' '
            IF (.NOT.DOTAPE) THEN
               FDVEC(1) = 25
               CALL CHR2H (48, FITFIL, 1, FDVEC(7))
               FDVEC(5) = 1
               MSGSUP = 32000
               CALL TAPIO ('OPRD', FDVEC, TAPBUF, TBIND, IERR)
               IF (IERR.EQ.0) THEN
                  CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
               ELSE
                  TRYTWO = FITFIL
                  I = ITRIM (FITFIL)
                  FITFIL(I+1:) = '1'
                  CALL CHR2H (48, FITFIL, 1, FDVEC(7))
                  END IF
               MSGSUP = 0
               END IF
            END IF
 10      CALL TAPIO ('OPRD', FDVEC, TAPBUF, TBIND, IERR)
         POTERR = 47
         IF (IERR.NE.0) THEN
            IF (TRYTWO.NE.' ') THEN
               MSGTXT = 'ALSO COULD NOT OPEN ' // TRYTWO
               CALL MSGWRT (8)
               END IF
            GO TO 980
            END IF
         ITAPE = FDVEC(40)
C
 20   POTERR = 0
      GO TO (100, 200, 300, 400, 500, 600, 700, 800), BRANCH
C-----------------------------------------------------------------------
C                                       TPHEAD
C                                       list header, backspace
C-----------------------------------------------------------------------
 100  CALL TPHEAD (FDVEC, 128, FITS, TBIND, TAPBUF, IERR)
      IF (IERR.NE.0) THEN
         POTERR = 48
         IC = 1
         IF (IERR.EQ.IEREOF) IC = 2
         CALL ZTAPE ('BAKF', LTAPE, ITAPE, IC, IERR)
         GO TO 970
         END IF
C                                       FITS header
      IF (FITS.EQ.2) THEN
         CALL DWRITE (TAPBUF)
      ELSE IF (FITS.EQ.1) THEN
         CALL FWRITE (FDVEC, TBIND, TAPBUF, FITBLK, IERR)
      ELSE IF (FITS.EQ.3) THEN
         CALL UWRITE (FDVEC, TBIND, TAPBUF, TAPBUF(8000), IERR)
      ELSE
         POTERR = 47
         END IF
      IF (IERR.NE.0) POTERR = 48
      CALL TAPIO ('BAKF', FDVEC, TAPBUF, TBIND, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.IERBOM)) POTERR = 48
      GO TO 970
C-----------------------------------------------------------------------
C                                        AVFILE
C                                        advance/back files
C-----------------------------------------------------------------------
 200  CALL ADVERB ('NFILES', 'I', 1, 0, IC, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 970
      IF (IC.LT.1) GO TO 210
         CALL ZTAPE ('ADVF', LTAPE, ITAPE, IC, IERR)
         IF (IERR.NE.0) POTERR = 48
         GO TO 970
C                                        back
 210  CONTINUE
         IC = -IC + 1
         CALL ZTAPE ('BAKF', LTAPE, ITAPE, IC, IERR)
         IF (IERR.NE.0) POTERR = 48
         GO TO 970
C-----------------------------------------------------------------------
C                                        AVMAP
C                                        advance n maps
C-----------------------------------------------------------------------
 300  CALL ADVERB ('NMAPS', 'I', 1, 0, IC, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 970
      POTERR = 48
      DO 330 IMAP = 1,IC
         CALL TPHEAD (FDVEC, 128, FITS, TBIND, TAPBUF, IERR)
         IF (IERR.NE.0) GO TO 970
C                                        DEC header & map
         IF (FITS.NE.2) GO TO 310
            CALL DWRITE (TAPBUF)
            I = TAPBUF(D2NY)
            CALL ZTAPE ('ADVR', LTAPE, ITAPE, I, IERR)
            IF (IERR.NE.0) GO TO 970
            FTAB(ITAPE+13) = 0
            GO TO 330
C                                        FITS info & file skip
 310     IF (FITS.NE.1) GO TO 320
            CALL FWRITE (FDVEC, TBIND, TAPBUF, FITBLK, IERR)
            IF (IERR.NE.0) GO TO 970
            I = 1
            CALL ZTAPE ('ADVF', LTAPE, ITAPE, I, IERR)
            IF (IERR.NE.0) GO TO 970
            FTAB(ITAPE+13) = 0
            GO TO 330
C                                       UV Export format: error
 320     CONTINUE
            WRITE (MSGTXT,1320)
            CALL MSGWRT (3)
            I = 1
            CALL ZTAPE ('BAKR', LTAPE, ITAPE, I, IERR)
            IF (IERR.NE.0) GO TO 970
            FTAB(ITAPE+13) = 0
            GO TO 335
 330     CONTINUE
 335  POTERR = 0
      GO TO 970
C-----------------------------------------------------------------------
C                                        REWIND
C                                        rewind tape
C-----------------------------------------------------------------------
 400  I = 1
      CALL ZTAPE ('REWI', LTAPE, ITAPE, I, IERR)
      IF (IERR.NE.0) POTERR = 48
      GO TO 970
C-----------------------------------------------------------------------
C                                        AVEOT
C                                        advance tape to end-of-data
C-----------------------------------------------------------------------
 500  IC = 0
      CALL ZTAPE ('AEOI', LTAPE, ITAPE, IC, IERR)
      IF (IERR.NE.0) THEN
         POTERR = 48
      ELSE
         I = ABS (IC) - 1
         IF (IC.GT.0) WRITE (MSGTXT,1500) I
         IF (IC.LT.0) WRITE (MSGTXT,1501) I
         IF (IC.NE.0) CALL MSGWRT (3)
         END IF
      GO TO 970
C-----------------------------------------------------------------------
C                                       MOUNT
C-----------------------------------------------------------------------
 600  CALL ADVERB ('DENSITY', 'I', 1, 0, IDENS, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 970
      CALL ADVERB ('REMTAPE', 'I', 1, 0, IRDRIV, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 970
      CALL ADVERB ('REMHOST', 'C', 1, 24, IDUM, RDUM, MACHIN)
      IF (ERRNUM.NE.0) GO TO 970
      IF ((IDENS.NE.800) .AND. (IDENS.NE.6250) .AND. (IDENS.NE.22500))
     *   IDENS = 6250
      IF (IRDRIV.LE.0) IRDRIV = 1
      CALL ZMOUNT (.TRUE., NT, IDENS, MACHIN, IRDRIV, LMSG, SYSERR,
     *   IERR)
      IF (IERR.NE.0) POTERR = 48
      GO TO 980
C-----------------------------------------------------------------------
C                                       DISMOUNT
C-----------------------------------------------------------------------
 700  CALL ADVERB ('REMTAPE', 'I', 1, 0, IRDRIV, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 970
      CALL ADVERB ('REMHOST', 'C', 1, 24, IDUM, RDUM, MACHIN)
      IF (ERRNUM.NE.0) GO TO 970
      IF (IRDRIV.LE.0) IRDRIV = 1
      CALL ZMOUNT (.FALSE., NT, 0, MACHIN, IRDRIV, LMSG, SYSERR, IERR)
      IF (IERR.NE.0) POTERR = 48
      GO TO 980
C-----------------------------------------------------------------------
C                                       TAPES
C-----------------------------------------------------------------------
 800  CALL ADVERB ('REMHOST', 'C', 1, 24, IDUM, RDUM, MACHIN)
      IF (ERRNUM.NE.0) GO TO 970
      CALL TAPSHO (MACHIN, IERR)
C     IF (IERR.NE.0) POTERR = 48
      GO TO 980
C-----------------------------------------------------------------------
C                                        Close tape & errors
 970  FTAB(ITAPE+4) = 0
      CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) POTERR = 48
C
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.GT.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1320 FORMAT ('FILE IS UV-EXPORT DATA: USE TASK EXIND, NOT VERB AVMAP')
 1500 FORMAT ('Advanced to end-of-information after file',I5)
 1501 FORMAT ('Advanced to end-of-information after skipping',I6,
     *   ' files')
      END
