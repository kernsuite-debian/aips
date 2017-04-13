      SUBROUTINE OERROR
C-----------------------------------------------------------------------
C! gives user error message, resets parameters to read next input line
C# POPS-lang
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2009, 2013
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
C  OERROR displays the error message on the users terminal.  It then
C  re-initializes AIPS variables and returns to main AIPS.
C-----------------------------------------------------------------------
      CHARACTER MESAGE(100)*12, MES1(50)*12, MES2(50)*12, KPAC*20,
     *   SCNAME*24
      LOGICAL   F
      INTEGER   IERR, FIND, KBP, KD, KD1, IR, LUN
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DRUN.INC'
      EQUIVALENCE (MESAGE(1), MES1(1)),    (MESAGE(51), MES2(1))
C-----------------------------------------------------------------------
      DATA MES1
C          1               2               3
     *    /'BLEW CORE!  ', 'SYMBOL?     ', 'BAD ( OR )  ',
     *     'LINE SIZE   ', 'SYMBOL SIZE!', 'ARRAY LIMITS',
     *     'STACK LIMITS', 'SYNTAX!     ', 'CHARACTER?  ',
     *     'PRINT       ', 'NO PROGRAM  ', 'ARG LIST?   ',
C         13              14              15
     *     'STRING SIZE ', 'ALREADY DF  ', 'CONTROL     ',
     *     'LOGIC EXP?  ', 'FOR---END?  ', 'INF LOOP?   ',
     *     'NO OPERATOR!', 'DIVIDE BY 0 ', 'IF OR LOOP! ',
     *     'READ        ', 'DATA TYPE?  ', 'USING WHAT? ',
C         25              26              27
     *     'PROTECTED!  ', 'SQRT NEGTIVE', 'NUMBER SIZE ',
     *     'RUN IN A RUN', 'LOG NEGATIVE', 'VERS TOO OLD',
     *     'UNAVAILABLE!', 'BOUNDARY LIM', 'CTLG PROBLEM',
     *     'HISTORY FILE', 'FIT FAILS   ', 'NO PROC MODE',
C         37              38              39
     *     'TEKS IN USE ', 'VERS TOO NEW', 'NOT YES / NO',
     *     'BATCH ERROR ', 'NO RET CODE ', 'TASK ACTIVE ',
     *     'NOT TASK    ', 'SYNC. FAILS ', 'FILE MISSING',
     *     'NO DESTROY  ', 'INVALID TAPE', 'TAPE PROBLEM',
     *     'TV PROBLEM  ', 'DISK PROBLEM'/
      DATA MES2
C         51              52              53
     *    /'TV UNAVAILAB', 'OPEN FILE?  ', 'NOT IN RUN  ',
     *     'NOT INPUTS  ', 'CREATE FILE?', 'CLOSE FILE? ',
     *     'PRINTER ERR.', 'FILE NOT OPN', 'TEXT READER ',
     *     'NOT IN BATCH', 'DISK PROBLEM', 'BAD EXPONENT',
C         63              64              65
     *     'TXT WRIT ERR', '????????????', '????????????',
     *     '????????????', '????????????', '????????????',
     *     '????????????', 'ONLY IN PROC', 'NOT IN PROC ',
     *     'ADVERB TYPE!', 'ADVERB SIZE!', 'BLEW TEMP K!',
C         75              76              77
     *     'BLEW TEMP C!', 'NOT PSEUDO #', 'TV ROAM MODE',
     *     '????????????', '????????????', '????????????',
     *     '????????????', '????????????', '????????????',
     *     '????????????', '????????????', '????????????',
C         87              88              89
     *     '????????????', '????????????', '????????????',
     *     '????????????', '????????????', '????????????',
     *     '????????????', '????????????', '????????????',
     *     '????????????', '????????????', '????????????',
     *     '????????????', 'ABORT!!!    '/
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       basic message
      IF ((ERRNUM.GE.1) .AND. (ERRNUM.LE.100)) THEN
C                                       SYMBOL special case
         IF ((ERRNUM.EQ.2) .OR. (ERRNUM.EQ.72) .OR. (ERRNUM.EQ.73)) THEN
            KPAC = ' '
            CALL H2CHR (NKAR, 1, KPAK, KPAC)
            WRITE (MSGTXT,1000) MESAGE(ERRNUM), KPAC
C                                       regular OR internal codes
 10      ELSE IF ((ERRNUM.GE.61) .AND. (ERRNUM.LT.70)) THEN
            WRITE (MSGTXT,1015) MESAGE(ERRNUM), IERROR(1), IERROR(2)
         ELSE
            WRITE (MSGTXT,1000) MESAGE(ERRNUM)
            END IF
         CALL MSGWRT (7)
         END IF
C                                       debug traceback
      IF (IDEBUG.GE.0) THEN
         KD1 = MIN (5, ERRLEV)
         WRITE (MSGTXT,1025) ERRNUM, ERRLEV, (PNAME(KD), KD = 1,KD1)
         CALL MSGWRT (6)
         END IF
C                                       set to abort
      IF ((ERRNUM.NE.100) .AND. (ISBTCH.NE.32000)) GO TO 40
      IF ((NPOPS.LE.NINTRN) .AND. (ISBTCH.NE.32000) .AND.
     *   (TSKNAM(1:5).NE.'AIPSC')) GO TO 40
         ERRNUM = -2
         GO TO 999
C                                       close RUN file
 40   IF ((IUNIT.EQ.2) .OR. (IUNIT.EQ.5)) THEN
         DO 50 IR = 1,NUMRUN
            KD = NUMRUN + 1 - IR
            LUN = LUNRUN(KD)
            CALL LSERCH ('SRCH', LUN, FIND, F, IERR)
            IF (IERR.EQ.0) CALL ZTCLOS (LUN, FIND, IERR)
 50         CONTINUE
         IF (IUNIT.EQ.5) THEN
            SCNAME = 'RUNFIL:COMPRESS.N.UUU'
            SCNAME(12:15) = HSTNAM(:4)
            CALL ZEHEX (NPOPS, 1, SCNAME(17:17))
            CALL ZEHEX (NLUSER, 3, SCNAME(19:21))
            MSGTXT = 'RUN FILE: $RUNFIL/' // SCNAME(8:)
            CALL MSGWRT (8)
            MSGTXT = 'AFTER FIXING PROBLEMS AND RENAMIMG TO DROP PERIOD'
            CALL MSGWRT (8)
            MSGTXT = 'PROTECTED => REDEFINING STANDARD PROC/ADVERB'
            CALL MSGWRT (8)
            END IF
         END IF
C                                       reset pointers
      IUNIT = 1
      IF ((NPOPS.GT.NINTRN) .OR. (TSKNAM(1:5).EQ.'AIPSC')) IUNIT = 3
      NUMRUN = 0
      MODE = 0
      L = 1
      KBP = 1
      ERRLEV = 0
      ERRNUM = 0
      IPT = '>'
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A12,2X,A20)
 1015 FORMAT (A12,3X,'CODES: ',2I7)
 1025 FORMAT ('TRACE: #',I6,' LEVS',I4,' NAMES',5(1X,A6))
      END
