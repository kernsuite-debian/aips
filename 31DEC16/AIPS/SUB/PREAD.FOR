      SUBROUTINE PREAD (KB)
C-----------------------------------------------------------------------
C! reads an input line from current input source (CRT, RUN file, batch)
C# POPS-lang
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2004, 2007, 2013
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
C   PREAD read input from CRT (IUNIT=1), text editor (IUNIT=2), or
C   batch input file (IUNIT=3).
C   Output:
C      KB   C*80     input line, unpacked, blank filled
C   Commons alterred:
C      /IO/  JBUFF, NBYTES
C-----------------------------------------------------------------------
      CHARACTER  KB*80
C
      CHARACTER  PRGNAM*6, SCNAME*24
      INTEGER    JTRIM, POTERR, IREC, LREC, NWDLIN, I, IFIND, IER,
     *   IEOF, LUNCRT, NLPR, ICNT, IERR, IF, IMAX, NERR, LUN
      LOGICAL   T, F
      INCLUDE 'INCS:DBAT.INC'
      INCLUDE 'INCS:DBWT.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DRUN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA PRGNAM /'PREAD '/
      DATA LUNCRT /5/
      DATA IEOF /2/
C-----------------------------------------------------------------------
      NWDLIN = (NCHLIN + 3) / 4
      NLPR = 252 / (2 + NWDLIN)
 5    NERR = 0
      POTERR = 100
      IF ((IUNIT.LT.1) .OR. (IUNIT.GT.5)) IUNIT = 1
C                                       advance typed line from SCHOLD
      IF (IUNIT.EQ.4) THEN
         JBUFF = HOLDUF
         IUNIT = 1
C                                       CRT: open
      ELSE IF (IUNIT.EQ.1) THEN
 10      CALL LSERCH ('SRCH', LUNCRT, IF, F, IERR)
         IF (IERR.NE.0) THEN
            CALL ZOPEN (LUNCRT, IF, 1, KB, F, T, T, IERROR(1))
            IF (IERROR(1).NE.0) THEN
               NERR = NERR + 1
               IF (NERR.GT.5) GO TO 900
               GO TO 10
               END IF
            END IF
C                                       prompted read
         CALL ZPROMP (IPT, JBUFF, IERROR(2))
         IF (IERROR(2).NE.0) THEN
            NERR = NERR + 1
            IF (NERR.GT.5) GO TO 900
            GO TO 10
            END IF
C                                       text editor (RUN) file
C                                       COMPRESS temporary file
      ELSE IF ((IUNIT.EQ.2) .OR. (IUNIT.EQ.5)) THEN
         LUN = LUNRUN(NUMRUN)
         CALL LSERCH ('SRCH', LUN, IFIND, F, IERROR(1))
         IERROR(2) = 0
         IF (IERROR(1).EQ.0) CALL ZTREAD (LUN, IFIND, JBUFF,
     *      IERROR(2))
C                                       close file on error -> CRT
         IF ((IERROR(1).NE.0) .OR. (IERROR(2).NE.0)) THEN
            CALL ZTCLOS (LUN, IFIND, IER)
            IF (IUNIT.EQ.5) THEN
               SCNAME = 'RUNFIL:COMPRESS.N.UUU'
               SCNAME(12:15) = HSTNAM(:4)
               CALL ZEHEX (NPOPS, 1, SCNAME(17:17))
               CALL ZEHEX (NLUSER, 3, SCNAME(19:21))
               CALL ZTXZAP (LUN, SCNAME, IERR)
               END IF
            NUMRUN = NUMRUN - 1
            IF (NUMRUN.LE.0) THEN
               IUNIT = 1
               IF (NPOPS.GT.NINTRN) IUNIT = 3
               IF (TSKNAM(1:5).EQ.'AIPSC') IUNIT = 3
            ELSE
               IUNIT = 2
               END IF
            POTERR = 59
            IF (IERROR(2).EQ.IEOF) GO TO 5
            GO TO 900
            END IF
C                                       batch input file
      ELSE IF (IUNIT.EQ.3) THEN
         LREC = (BATREC-1)/NLPR + 1
         IF (BATREC.LE.0) THEN
            BATREC = 1
         ELSE
            I = 5 + (2 + NWDLIN) * MOD (BATREC-1,NLPR)
            BATREC = BATDAT(I)
C                                        EOF: force exit/endbatch
            IF (BATREC.LE.0) THEN
               JBUFF = 'EXIT'
               IMAX = NINTRN
               IF (NBATQS.GT.0) IMAX = IMAX + 1
C                                        special end for UNQUE in
C                                        AIPS and BATER.
               IF (((NPOPS.LE.NINTRN) .OR. (TSKNAM(1:5).EQ.'BATER'))
     *            .AND. (TSKNAM(1:5).NE.'AIPSC')) THEN
                  IUNIT = 1
                  JBUFF = 'ENDBATCH'
                  END IF
               GO TO 70
               END IF
            END IF
C                                        read new disk rec if needed
         IREC = (BATREC-1)/NLPR + 1
         IF (IREC.NE.LREC) THEN
            POTERR = 100
            IF (NPOPS.EQ.1) POTERR = 59
            CALL ZFIO ('READ', BATLUN, BATIND, IREC, BATDAT, IERROR(1))
            IF (IERROR(1).NE.0) GO TO 900
            END IF
         I = 7 + (2+NWDLIN) * MOD(BATREC-1, NLPR)
         CALL H2CHR (NCHLIN, 1, BATDAT(I), JBUFF)
         END IF
C                                       fill with blanks
 70   NBYTES = JTRIM (JBUFF)
      CALL STLTOU (NBYTES, JBUFF)
      KB = JBUFF
C                                        Checker writes to Batch file
      IF (WASERR) GO TO 999
C                                        insert forward reference
         IF (BWTREC.GT.0) THEN
            I = 5 + (2+NWDLIN) * MOD(BWTREC-1, NLPR)
            BWTDAT(I) = BWTREC + 1
C                                        write it actually
            IF (MOD(BWTREC,NLPR).EQ.0) THEN
               IREC = (BWTREC-1) / NLPR + 1
               IF (IREC.GT.BWTNUM) THEN
                  ICNT = 49 / NLPR + 1
                  CALL ZEXPND (BWTLUN, 1, BWTNAM, ICNT, IERROR)
                  IF (IERROR(1).NE.0) THEN
                     POTERR = 50
                     WASERR = .TRUE.
                     WRITE (MSGTXT,1070) IERROR(1)
                     CALL MSGWRT (8)
                     GO TO 980
                     END IF
                  BWTNUM = BWTNUM + ICNT
                  END IF
               CALL ZFIO ('WRIT', BWTLUN, BWTIND, IREC, BWTDAT,
     *            IERROR(1))
               IF (IERROR(1).NE.0) THEN
                  POTERR = 50
                  WASERR = .TRUE.
                  WRITE (MSGTXT,1070) IERROR(1)
                  CALL MSGWRT (8)
                  GO TO 980
                  END IF
               END IF
            END IF
C                                        set up new record
 80      BWTREC = BWTREC + 1
         I = 5 + (2+NWDLIN) * MOD (BWTREC-1, NLPR)
         BWTDAT(I) = 0
         BWTDAT(I+1) = BWTREC - 1
         CALL CHR2H (NCHLIN, JBUFF, 1, BWTDAT(I+2))
         GO TO 999
C                                       sever errors: ABORT
 900  WRITE (MSGTXT,1900) IERROR(1), IERROR(2)
      CALL MSGWRT (10)
 980  ERRNUM = POTERR
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
C-----------------------------------------------------------------------
 1070 FORMAT ('ERROR WRITING TO BATCH FILE',I7)
 1900 FORMAT ('SEVERE INPUT ERROR: ',2I7)
      END
