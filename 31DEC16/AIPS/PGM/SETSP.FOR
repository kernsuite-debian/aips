      PROGRAM SETSP
C-----------------------------------------------------------------------
C! set system parameters in SP files for several AIPS hosts
C# Service
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2002, 2006, 2008-2009, 2012, 2015
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
C     SETSP may be used to change system parameters for a collection of
C     AIPS hosts.
C
C     Batch operation is possible, input being taken from an environment
C     variable called SETSPARM which has the form
C
C        SETSPARM: option value1 value2...
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1991/May/16  Code last modified; 1991/May/31 (ATNF)
C          Modified [PPM] for NRAO 1992/June/5: remove explicit filename
C          Modified [PPM] 1994.Nov.17 and later for nefarious purposes..
C          Modified [PPM] 1996.May.21: handle hostnames with $ARCH
C                                      embedded in them, e.g. SOL123
C                                      Also allow -1 for no change on
C                                      all parameters.
C-----------------------------------------------------------------------
      LOGICAL   GOTPWD, NEOF, INDIV
      INTEGER   IBUFF(256), IERR, IHOST, IOPT, NDX, NHOST, JHOST, JSITE,
     *   TTY(2), TTYLUN, TTYIND, JTXT, JTRIM, HLIND, NETLUN, I, J, K, JT
      DOUBLE PRECISION DP
      CHARACTER MSGBUF*72, PRGNAM*6, SPNAME(512)*48, TEXT*72, JDENT*48,
     *   HOST*48, HENTRY*80, ARCH*40, SITE*40, MYSITE*40, RSP*8,
     *   TXTDUM*72
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DERR.INC'
      EQUIVALENCE (TTY(1), TTYLUN), (TTY(2), TTYIND)
      COMMON /SETSPC/ RSP
      DATA PRGNAM /'SETSP'/
      DATA NETLUN /11/
C-----------------------------------------------------------------------
C                                       Initialize.
      GOTPWD = .FALSE.
C
      TTY(1) = 5
      CALL AIPINI (TTY, PRGNAM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 985
         END IF
      RSP = RLSNAM
C                                       Get SITE name
      CALL ZTRLOG (4, 'SITE', 40, MYSITE, NDX, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'SETSP: CANNOT FIND SITE NAME; ENVIRONMENT NOT SET?'
         GO TO 985
         END IF
      JSITE = JTRIM(MYSITE)
C                                       Open the HOSTS.LIST file
C                                       (can't use HOSTS env. variable
C                                        as it may be >MAXAPATH long)
      TEXT = 'AIPS_ROOT:HOSTS.LIST'
      CALL ZTXOPN ('QRED', NETLUN, HLIND, TEXT, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'SETSP: CANNOT OPEN HOSTS.LIST FILE, MUST STOP NOW!'
         GO TO 985
         END IF
C                                       Get DA00:SPf... name
      TXTDUM = ' '
      CALL ZPHFIL ('SP', 1, 0, 0, TXTDUM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 985
         END IF
C                                       Get rid of the DA00: part
      TEXT = TXTDUM(INDEX(TXTDUM, ':')+1:)
      JTXT = JTRIM(TEXT)
C                                       Start loop over all hosts
      IHOST = 1
      NEOF = .FALSE.
 25   IF (NEOF) GO TO 50
         CALL ZTXIO ('READ', NETLUN, HLIND, HENTRY, IERR)
C                                       End of file reached
         IF (IERR.EQ.2) THEN
            IERR = 0
            NEOF = .TRUE.
            CALL ZTXCLS (NETLUN, HLIND, IERR)
C                                       Other error; bail out
         ELSE IF (IERR.NE.0) THEN
            CALL ZTXCLS (NETLUN, HLIND, IERR)
            MSGTXT = 'SETSP: ERROR READING AIPS_ROOT:HOSTS.LIST FILE'
            GO TO 985
C                                       Plus means a valid AIPS host
         ELSE IF (HENTRY(1:1).EQ.'+') THEN
            JT = JTRIM (HENTRY)
C                                       Get the first word following it
            CALL WORD (HENTRY(4:), HOST)
            JHOST = JTRIM(HOST)
C                                       Check the SITE; skip ARCH...
            CALL WORD (HENTRY(JHOST+4:), ARCH)
            J = JTRIM(ARCH)
C                                       Account for white space...
            I = INDEX(HENTRY(JHOST+4:), ARCH(:J))
C                                       Extract SITE
            CALL WORD (HENTRY(3+JHOST+I+J:), SITE)
C                                       This is unix-specific :-(
C                                       [because of the slashes]
            IF (SITE(:JTRIM(SITE)).EQ.MYSITE(:JSITE)) THEN
               SPNAME(IHOST) = 'AIPS_ROOT:DA00/' //
     *                         HOST(:JHOST) // '/' //
     *                         TEXT(:JTXT)
               IHOST = IHOST + 1
               IF (IHOST.LE.512) GO TO 25
               CALL ZTXCLS (NETLUN, HLIND, IERR)
               MSGTXT = 'TOO MANY HOSTS (>512) IN LIST, SKIPPING REST'
               CALL MSGWRT (8)
               GO TO 50
               END IF
            END IF
            GO TO 25
C                                       End of loop through HOSTS.LIST
 50   NHOST = IHOST - 1
      IF (NHOST.LE.0) THEN
         MSGTXT = 'NO VALID ENTRIES in AIPS_ROOT:HOSTS.LIST'
         GO TO 985
         END IF
C                                       Get batch input if any.
      CALL ZTRLOG (8, 'SETSPARM', 72, TEXT, NDX, IERR)
      IF ((IERR.EQ.0) .AND. (NDX.GT.0)) THEN
         NDX = 1
         CALL GETNUM (TEXT, 72, NDX, DP)
         IF (ERRLEV.GT.0) THEN
            MSGTXT = 'INVALID INPUT.'
            GO TO 985
            END IF
         IOPT = NINT(DP)
C
         IF (IOPT.LE.0) THEN
            CALL SPSHOW (TTYLUN, TTYIND, SPNAME(IHOST), 0, IERR)
         ELSE IF (TEXT(NDX:).EQ.' ') THEN
            CALL SPSHOW (TTYLUN, TTYIND, SPNAME(IHOST), -IOPT, IERR)
            DO 60 IHOST = 1,NHOST
               CALL SPSHOW (TTYLUN, TTYIND, SPNAME(IHOST), IOPT, IERR)
 60            CONTINUE
         ELSE
            IF (IOPT.NE.19) THEN
               MSGTXT = 'This operation not supported in batch mode'
               GO TO 985
               END IF
            DO 70 IHOST = 1,NHOST
               CALL SPSET (SPNAME(IHOST), IOPT, TEXT(NDX:), IERR)
 70            CONTINUE
            END IF
         GO TO 990
         END IF
C
      CALL SPSHOW (TTYLUN, TTYIND, ' ', 0, IERR)
 80   CONTINUE
         ERRLEV = 0
C                                       User prompt.
         MSGBUF = ' '
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         MSGBUF = 'Enter <OPTION> to print,'
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         MSGBUF = '      <OPTION> and <VALUE(S)> to change,'
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         MSGBUF = '      <OPTION> and "CHANGE" to change individually,'
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         MSGBUF = '       0 to list index'
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         MSGBUF = '      -1 to quit'
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, TEXT, IERR)
         IF (IERR.LT.0) GO TO 80
C                                       Decode option.
         NDX = 1
         CALL GETNUM (TEXT, 72, NDX, DP)
         IF (ERRLEV.GT.0) THEN
            MSGBUF = 'What?'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            GO TO 80
            END IF
         IOPT = NINT(DP)
C                                       Exit?
         IF (IOPT.LT.0) GO TO 990
C
         MSGBUF = ' '
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Print description.
         CALL SPSHOW (TTYLUN, TTYIND, SPNAME(IHOST), -IOPT, IERR)
         IF (IOPT.EQ.0) GO TO 80
C                                       Print values.
         IF (TEXT(NDX:).EQ.' ') THEN
            DO 90 IHOST = 1, NHOST
               CALL SPSHOW (TTYLUN, TTYIND, SPNAME(IHOST), IOPT, IERR)
 90            CONTINUE
C                                       Change values, make sure.
         ELSE
            IF (INDEX(TEXT(NDX:), 'CHANGE').GT.0) THEN
               INDIV = .TRUE.
            ELSE
               INDIV = .FALSE.
               MSGBUF = '   New value: ' // TEXT(NDX:)
               CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
               MSGBUF = ' '
               CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
               MSGBUF = 'Change this for all AIPS systems (y|n)?'
               CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
               CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, IERR)
               IF ((MSGBUF(1:1).NE.'y') .AND. (MSGBUF(1:1).NE.'Y'))
     *             GO TO 80
C                                       Get password.
               END IF
            IF (.NOT.GOTPWD) THEN
               CALL PASWRD (IBUFF, IERR)
               IF (IERR.NE.0) GO TO 990
               GOTPWD = .TRUE.
               END IF
C                                       Print description again.
            MSGBUF = ' '
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            CALL SPSHOW (TTYLUN, TTYIND, SPNAME(IHOST), -IOPT, IERR)
C                                       Set and show new values.
            DO 100 IHOST = 1,NHOST
               IF (INDIV) THEN
                  CALL SPSHOW (TTYLUN, TTYIND, SPNAME(IHOST), IOPT,
     *               IERR)
                  IF (IERR.NE.0) GO TO 100
                  HOST = 'BAD HOST NAME?'
                  J = INDEX(SPNAME(IHOST), 'DA00/')
                  IF (J.GT.0) THEN
                     JDENT = SPNAME(IHOST)(J+5:)
                     K = INDEX(JDENT, '/SP') - 1
                     IF (K.GT.0) HOST = JDENT(:K)
                     END IF
                  WRITE (MSGBUF, 1040) HOST(1:JTRIM(HOST))
                  CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
                  CALL ZTTYIO ('READ', TTYLUN, TTYIND, 73-NDX,
     *                         TEXT(NDX:), IERR)
                  END IF
               CALL SPSET (SPNAME(IHOST), IOPT, TEXT(NDX:), IERR)
               IF (IERR.EQ.0) CALL SPSHOW (TTYLUN, TTYIND,
     *            SPNAME(IHOST), IOPT, IERR)
 100           CONTINUE
            END IF
      GO TO 80
C
 985  CALL MSGWRT (8)
 990  CALL ACOUNT (2)
C
 999  STOP
C-----------------------------------------------------------------------
 1010 FORMAT ('SETSP: INITIALIZATION ERROR',I4)
 1030 FORMAT ('ERROR ',I5,' GETTING SP PHYSICAL FILE NAME')
 1040 FORMAT ('Enter value for ',A,', -1 to leave unchanged:')
      END
      SUBROUTINE SPSHOW (TTYLUN, TTYIND, SPNAME, IOPT, IERR)
C-----------------------------------------------------------------------
C   SPSHOW lists system parameters.
C   Given:
C      SPNAME       C*48  Full pathname of the SP file.
C      IOPT         I     SP option number.
C      IERR         I     Error status, 0 means OK.
C   Mark Calabretta, Australia Telescope.
C-----------------------------------------------------------------------
      INTEGER   TTYLUN, TTYIND, IOPT, IERR
      CHARACTER SPNAME*(*)
C
      LOGICAL   NOMAP, EXCL, WAIT
      INTEGER   IDISK, IOBLK(256), J, K, POTERR, SPIND, SPLUN
      REAL      ROBLK(256)
      CHARACTER IDENT*12, JDENT*15, MSGBUF*72, TSKNM2*6, HOST*48,
     *   ANAME*20
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (IOBLK, ROBLK)
      DATA SPLUN, NOMAP, EXCL, WAIT /27, .FALSE., .TRUE., .FALSE./
C-----------------------------------------------------------------------
      POTERR = 1
      TSKNM2 = 'SETSP '
C                                       Print summary.
      IF (IOPT.LE.0) THEN
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-1)) THEN
            MSGBUF = '    1  (1I)  No. of AIPS data disks (1 - 35)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-2)) THEN
            MSGBUF = '    2  (1I)  No. of tape drives (0 - 35)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-3)) THEN
            MSGBUF = '    3  (1I)  No. of lines per CRT page ' //
     *        '(0, 9 - 200)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-4)) THEN
            MSGBUF = '    4  (1I)  No. of lines per print page ' //
     *               ' (10 - 300)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-5)) THEN
            MSGBUF = '    5  (1I)  No. of batch queues ' //
     *               '(0 - 34-NPOPS)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-6)) THEN
            MSGBUF = '    6  (1I)  Plotter no. of X dots per page ' //
     *               '(10 - 200000)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-7)) THEN
            MSGBUF = '    7  (1I)  Plotter no. of Y dots per page ' //
     *               '(10 - 200000)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-8)) THEN
            MSGBUF = '    8  (1I)  Plotter: number of X dots per ' //
     *               'character (3 - 200)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-9)) THEN
            MSGBUF = '    9  (1I)  Plotter: number of Y dots per ' //
     *               'character (3 - 200)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-10)) THEN
            MSGBUF = '   10  (1I)  No. of interactive AIPS (1 - 35)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-11)) THEN
            MSGBUF = '   11  (1I)  Max no. words in pseudo AP ' //
     *               '(in 1024s)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-12)) THEN
            MSGBUF = '   12  (1I)  No. of TV devices available ' //
     *               '(0 - 1295)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-13)) THEN
            MSGBUF = '   13  (1I)  No. of graphics devices ' //
     *               'available (0 - 1295)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-14)) THEN
            MSGBUF = '   14  (1R)  No. of X dots per mm on printer ' //
     *               '(0.1 - 100.0)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-15)) THEN
            MSGBUF = '   15  (1R)  No. of X dots per mm on ' //
     *               'Tektronix (0.1 - 100.0)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-16)) THEN
            MSGBUF = '   16  (1I)  No. of POPS allowed access to ' //
     *               'TVs (0 - 35)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-17)) THEN
            MSGBUF = '   17  (1I)  No. of POPS allowed access to ' //
     *               'TKs (0 - 35)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-18)) THEN
            MSGBUF = '   18  (1I)  No. entries in private ' //
     *               'catalogues (1 - 46655)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-19)) THEN
            MSGBUF =
     * '   19  (A20) Local system name'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-20)) THEN
            MSGBUF = '   20  (1I)  Maximum user number (1 - 46655)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-21)) THEN
            MSGBUF = '   21  (nR)  TIMDEST minima for each disk ' //
     *               '(0.1 - 365.0 days)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-22)) THEN
            MSGBUF = '   22  (1R)  TIMDEST limit: SAVE/GET files ' //
     *               '(3.0 - 365.0 days)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-23)) THEN
            MSGBUF = '   23  (1R)  TIMDEST/EXIT limit: messages ' //
     *               '(1.0 - 365.0 days)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-24)) THEN
            MSGBUF = '   24  (1R)  TIMDEST limit: scratch files ' //
     *               '(1.0 - 35.0 days)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-25)) THEN
            MSGBUF = '   25  (1R)  TIMDEST limit: empty CA files ' //
     *               '(0.1 - 5.0 days)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-26)) THEN
            MSGBUF = '   26  (4R)  No AP batch starts: weekend / ' //
     *               'weekday (0.0 - 24.0 hours)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-27)) THEN
            MSGBUF = '   27  (3R)  AP roll interval / ' //
     *               'patience (1)*N+(2)*N*N (>1, >1, * min)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-28)) THEN
            MSGBUF = '   28  (1I)  Line printer width (72 - 132)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-30)) THEN
            MSGBUF = '   30  (1I)  Max length of "short" vector ' //
     *               '(0 - 2000)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-31)) THEN
            MSGBUF = '   31  (2I)  Graphics (TK) screen size: x, y ' //
     *               '(300 - 16384)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-32)) THEN
            MSGBUF = '   32  (2I)  Graphics (TK) character size: ' //
     *               'x, y (5 - 100)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         POTERR = 0
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-33)) THEN
            MSGBUF = '   33  (9I)  Disk & reserved users, or -1 for ' //
     *               'scratch'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-34)) THEN
            MSGBUF = '   34  (1I)  Delay time for print file delete' //
     *         ' (10-36000 secs)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         IF ((IOPT.EQ.0) .OR. (IOPT.EQ.-35)) THEN
            MSGBUF = '   35  (1R)  Computer speed rating' //
     *         ' (0.01-500 AIPSmarks)'
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
C                                       Get the current system
C                                       parameters.
      ELSE
         CALL ZOPEN (SPLUN, SPIND, 1, SPNAME, NOMAP, EXCL, WAIT, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 985
            END IF
         CALL ZFIO ('READ', SPLUN, SPIND, 1, IOBLK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) IERR
            GO TO 985
            END IF
         CALL ZCLOSE (SPLUN, SPIND, IERR)
C                                       Get system identification.
         IDENT = 'BAD IDENT'
         J = INDEX(SPNAME, 'DA00/')
         IF (J.GT.0) THEN
            JDENT = SPNAME(J+5:)
            K = INDEX(JDENT, '/SP') - 1
            IF (K.GT.0) THEN
               IDENT = JDENT(:K)
               HOST = IDENT
               END IF
            END IF

C                                       List the selected parameter.
C                                       Number of data disks.
         IF (IOPT.EQ.1) THEN
            WRITE (MSGBUF,1023) IDENT, IOBLK(1)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Number of tape drives.
         ELSE IF (IOPT.EQ.2) THEN
            WRITE (MSGBUF,1023) IDENT, IOBLK(2)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Number of lines per CRT page.
         ELSE IF (IOPT.EQ.3) THEN
            WRITE (MSGBUF,1023) IDENT, IOBLK(3)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Number of lines per print page.
         ELSE IF (IOPT.EQ.4) THEN
            WRITE (MSGBUF,1023) IDENT, IOBLK(4)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Number of of batch queues.
         ELSE IF (IOPT.EQ.5) THEN
            WRITE (MSGBUF,1023) IDENT, IOBLK(5)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Plotter X dots per page.
         ELSE IF (IOPT.EQ.6) THEN
            WRITE (MSGBUF,1023) IDENT, IOBLK(6)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Plotter Y dots per page.
         ELSE IF (IOPT.EQ.7) THEN
            WRITE (MSGBUF,1023) IDENT, IOBLK(7)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Plotter X dots per character.
         ELSE IF (IOPT.EQ.8) THEN
            WRITE (MSGBUF,1023) IDENT, IOBLK(8)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Plotter Y dots per character.
         ELSE IF (IOPT.EQ.9) THEN
            WRITE (MSGBUF,1023) IDENT, IOBLK(9)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Number of of interactive AIPS.
         ELSE IF (IOPT.EQ.10) THEN
            WRITE (MSGBUF,1023) IDENT, IOBLK(10)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Number words in AP (in 1024's).
         ELSE IF (IOPT.EQ.11) THEN
            WRITE (MSGBUF,1023) IDENT, IOBLK(11)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Number of TV devices available.
         ELSE IF (IOPT.EQ.12) THEN
            WRITE (MSGBUF,1023) IDENT, IOBLK(12)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Number of graphics (TK) devices.
         ELSE IF (IOPT.EQ.13) THEN
            WRITE (MSGBUF,1023) IDENT, IOBLK(13)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Number X dots / mm on printer.
         ELSE IF (IOPT.EQ.14) THEN
            WRITE (MSGBUF,1024) IDENT, ROBLK(71)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Number of lines / mm on TK.
         ELSE IF (IOPT.EQ.15) THEN
            WRITE (MSGBUF,1024) IDENT, ROBLK(72)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Number POPS allowed TV access.
         ELSE IF (IOPT.EQ.16) THEN
            WRITE (MSGBUF,1023) IDENT, IOBLK(64)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Number POPS allowed TK access.
         ELSE IF (IOPT.EQ.17) THEN
            WRITE (MSGBUF,1023) IDENT, IOBLK(65)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       # entries in private catalogs.
         ELSE IF (IOPT.EQ.18) THEN
            WRITE (MSGBUF,1023) IDENT, IOBLK(66)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       System name/Registration status.
         ELSE IF (IOPT.EQ.19) THEN
            MSGBUF = '   ' // IDENT
            CALL H2CHR(20, 1, ROBLK(73), ANAME)
            IF (ANAME.EQ.'UN-REGISTERED!!!!') CALL ZTRLOG (4, 'SITE',
     *         20, ANAME, J, K)
            MSGBUF(19:) = ANAME
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Maximum user number.
         ELSE IF (IOPT.EQ.20) THEN
            WRITE (MSGBUF,1023) IDENT, IOBLK(67)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Disk data TIMDEST limits.
         ELSE IF (IOPT.EQ.21) THEN
            K = MIN (8, IOBLK(1))
            WRITE (MSGBUF,1027) IDENT, (ROBLK(J), J = 78,77+K)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            IF (IOBLK(1).GT.8) THEN
               K = IOBLK(1) - 8
               WRITE (MSGBUF,1028) (ROBLK(J), J = 86,85+K)
               CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
               END IF
C                                       TIMDEST limit: SAVE/GET
         ELSE IF (IOPT.EQ.22) THEN
            WRITE (MSGBUF,1027) IDENT, ROBLK(93)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       TIMDEST/EXIT limit messages.
         ELSE IF (IOPT.EQ.23) THEN
            WRITE (MSGBUF,1027) IDENT, ROBLK(94)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       TIMDEST limit: scratch files.
         ELSE IF (IOPT.EQ.24) THEN
            WRITE (MSGBUF,1027) IDENT, ROBLK(95)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       TIMDEST limit: empty CA files.
         ELSE IF (IOPT.EQ.25) THEN
            WRITE (MSGBUF,1027) IDENT, ROBLK(96)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Time ranges for which AP batch
C                                       starts are denied.
         ELSE IF (IOPT.EQ.26) THEN
            WRITE (MSGBUF,1024) IDENT, (ROBLK(J), J = 97,100)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       AP: roll time, delay formula.
         ELSE IF (IOPT.EQ.27) THEN
            WRITE (MSGBUF,1024) IDENT, (ROBLK(J), J = 101,103)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Maximum line printer width.
         ELSE IF (IOPT.EQ.28) THEN
            WRITE (MSGBUF,1023) IDENT, IOBLK(68)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Secondary AP space.
         ELSE IF (IOPT.EQ.29) THEN
C                                       Max length short vector.
         ELSE IF (IOPT.EQ.30) THEN
            WRITE (MSGBUF,1023) IDENT, IOBLK(70)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       TK device screen size.
         ELSE IF (IOPT.EQ.31) THEN
            WRITE (MSGBUF,1023) IDENT, (IOBLK(J), J = 224,225)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       TK device char size.
         ELSE IF (IOPT.EQ.32) THEN
            WRITE (MSGBUF,1023) IDENT, (IOBLK(J), J = 226,227)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Disk booking assignments.
         ELSE IF (IOPT.EQ.33) THEN
            JDENT = IDENT
            DO 40 IDISK = 2,IOBLK(1)
               K = 104 + 8*(IDISK-1)
               WRITE (MSGBUF,1030) JDENT, IDISK, (IOBLK(J), J = K,K+7)
               CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
               JDENT = ' '
 40            CONTINUE
C                                       Printer file deletion delay
         ELSE IF (IOPT.EQ.34) THEN
            WRITE (MSGBUF,1023) IDENT, IOBLK(228)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Speed rating
         ELSE IF (IOPT.EQ.35) THEN
            WRITE (MSGBUF,1025) IDENT, ROBLK(238)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
C                                       Invalid option.
         ELSE
            WRITE (MSGTXT,1050) IOPT
            GO TO 985
            END IF
         END IF
C                                       Everything's just fine.
      POTERR = 0
      GO TO 999

 985  CALL MSGWRT (8)
      IF (POTERR.NE.0) THEN
         MSGTXT = 'SPSHOW: INPUT SPECIFICATION ERROR, REQUEST IGNORED.'
         CALL  MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SPSHOW: SYSTEM PARAMETER FILE OPEN ERROR',I4)
 1020 FORMAT ('SPSHOW: ERROR',I4,' READING SP FILE.')
 1023 FORMAT (3X,A,2I6)
 1024 FORMAT (3X,A,4F6.3)
 1025 FORMAT (3X,A,F7.2)
 1027 FORMAT (3X,A,8(F7.1))
 1028 FORMAT (15X,7(F7.1))
 1030 FORMAT (3X,A,'Disk',I3,' Users:',8I5)
 1050 FORMAT ('SPSHOW: Unrecognized option',I4)
      END
      SUBROUTINE SPSET (SPNAME, IOPT, TEXT, IERR)
C-----------------------------------------------------------------------
C   SPSET changes system parameters.
C   Given:
C      SPNAME       C*48  Full pathname of the SP file.
C      IOPT         I     SP option number.
C      TEXT         C**   Option values.
C   Returned:
C      IERR         I     Error status, 0 means success.
C   Mark Calabretta, Australia Telescope.
C-----------------------------------------------------------------------
      CHARACTER SPNAME*(*), TEXT*(*)
      INTEGER   IOPT, IERR
C
      LOGICAL   NOMAP, EXCL, WAIT, TTOPN
      INTEGER   IDISK, IOBLK(256), J, K, LENTXT, NDX, POTERR, SPIND,
     *   SPLUN, TTYIND, TTYLUN, ADDRSZ, KMAX
      REAL      ROBLK(256)
      DOUBLE PRECISION DP
      CHARACTER HOST*48, SYSNAM*20, RSP*8
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (IOBLK, ROBLK)
      COMMON /TTYCOM/ TTYLUN, TTYIND
      COMMON /SETSPC/ RSP
      SAVE TTOPN
      DATA SPLUN, NOMAP, EXCL, WAIT /27, .FALSE., .TRUE., .FALSE./
      DATA TTOPN /.FALSE./
C-----------------------------------------------------------------------
      TTYLUN = 5
      IF (.NOT.TTOPN) THEN
         CALL ZTTOPN (TTYLUN, TTYIND, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'FAILED TO INITIALISE TTY I/O'
            CALL MSGWRT(8)
            GO TO 999
            END IF
         TTOPN = .TRUE.
         END IF
      ERRLEV = 0
      IF (TEXT.EQ.' ') THEN
         ERRLEV = 1
         GO TO 990
         END IF
C                                       Open parameter file.
      CALL ZOPEN (SPLUN, SPIND, 1, SPNAME, NOMAP, EXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
         SPIND = 0
         WRITE (MSGTXT,1020) IERR
         GO TO 985
         END IF
C                                       Read the current parameters.
      CALL ZFIO ('READ', SPLUN, SPIND, 1, IOBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 985
         END IF
      K = INDEX (SPNAME, '/')
      IF (K.GT.0) THEN
         K = K + 1
         J = INDEX (SPNAME(K:), '/')
         IF (J.GT.0) J = K + J - 2
      ELSE
         J = 0
         END IF
      IF (K.GT.0 .AND. J.GT.0) THEN
         HOST = SPNAME(K:J)
      ELSE
         MSGTXT = 'MALFORMED SP FILE NAME: ' // SPNAME
         CALL MSGWRT (8)
         IERR = 1
         GO TO 990
         END IF
C                                       Error escape value.
      POTERR = 1
      NDX = 1
      LENTXT = LEN(TEXT)
C                                       Number of data disks.
      IF (IOPT.EQ.1) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.1) .OR. (K.GT.35)) GO TO 990
            IOBLK(1) = K
            END IF
C                                       Number of tape drives.
      ELSE IF (IOPT.EQ.2) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.0) .OR. (K.GT.35)) GO TO 990
            IOBLK(2) = K
            END IF
C                                       Number of lines per CRT page.
      ELSE IF (IOPT.EQ.3) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF (((K.LT.9) .AND. (K.GT.0)) .OR. (K.GT.200)) GO TO 990
            IOBLK(3) = K
            END IF
C                                       Number of lines per print page.
      ELSE IF (IOPT.EQ.4) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.10) .OR. (K.GT.300)) GO TO 990
            IOBLK(4) = K
            END IF
C                                       Number of of batch queues.
      ELSE IF (IOPT.EQ.5) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.0) .OR. (K.GT.34-MAX(1,IOBLK(10)))) GO TO 990
            IOBLK(5) = K
            END IF
C                                       Plotter X dots per page.
      ELSE IF (IOPT.EQ.6) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.10) .OR. (K.GT.200000)) GO TO 990
            IOBLK(6) = K
            END IF
C                                       Plotter Y dots per page.
      ELSE IF (IOPT.EQ.7) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.10) .OR. (K.GT.200000)) GO TO 990
            IOBLK(7) = K
            END IF
C                                       Plotter X dots per character.
      ELSE IF (IOPT.EQ.8) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.3) .OR. (K.GT.200)) GO TO 990
            IOBLK(8) = K
            END IF
C                                       Plotter Y dots per character.
      ELSE IF (IOPT.EQ.9) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.3) .OR. (K.GT.200)) GO TO 990
            IOBLK(9) = K
            END IF
C                                       Number of interactive AIPS.
      ELSE IF (IOPT.EQ.10) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.1) .OR. (K.GT.35)) GO TO 990
            IOBLK(10) = K
            END IF
C                                       Max # words in pseudo AP
C                                       (in 1024's).
      ELSE IF (IOPT.EQ.11) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            CALL ZADRSZ (ADDRSZ)
            KMAX = 3 * 128 * 1024
            IF (ADDRSZ.EQ.8) KMAX = 6 * 256 * 1024
            IF ((K.LT.0) .OR. (K.GT.KMAX)) GO TO 990
            IOBLK(11) = K
            END IF
C                                       Number of TV devices available.
      ELSE IF (IOPT.EQ.12) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.0) .OR. (K.GT.1295)) GO TO 990
            IOBLK(12) = K
            END IF
C                                       Number of graphics (TK) devices
      ELSE IF (IOPT.EQ.13) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.0) .OR. (K.GT.1295)) GO TO 990
            IOBLK(13) = K
            END IF
C                                       Number X dots / mm on printer.
      ELSE IF (IOPT.EQ.14) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            IF ((DP.LT.0.1D0) .OR. (DP.GT.100D0)) GO TO 990
            ROBLK(71) = DP
            END IF
C                                       Number of X lines per mm on TK.
      ELSE IF (IOPT.EQ.15) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            IF ((DP.LT.0.1D0) .OR. (DP.GT.100D0)) GO TO 990
            ROBLK(72) = DP
            END IF
C                                       Number POPS allowed TV access.
      ELSE IF (IOPT.EQ.16) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.0) .OR. (K.GT.35)) GO TO 990
            IOBLK(64) = K
            END IF
C                                       Number POPS allowed TK access.
      ELSE IF (IOPT.EQ.17) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.0) .OR. (K.GT.35)) GO TO 990
            IOBLK(65) = K
            END IF
C                                       # entries in private catalogs.
      ELSE IF (IOPT.EQ.18) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.1) .OR. (K.GT.46655)) GO TO 990
            IOBLK(66) = K
            END IF
C                                       System name/Registration Status.
      ELSE IF (IOPT.EQ.19) THEN
         IF (TEXT.NE.'-1') THEN
            SYSNAM = TEXT(1:20)
            CALL CHR2H (20, SYSNAM, 1, ROBLK(73))
            END IF
C                                       Maximum user number.
      ELSE IF (IOPT.EQ.20) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.1) .OR. (K.GT.46655)) GO TO 990
            IOBLK(67) = K
            END IF
C                                       Disk data TIMDEST limits.
      ELSE IF (IOPT.EQ.21) THEN
         IF (TEXT.NE.'-1') THEN
            DO 50 J = 1,IOBLK(1)
               CALL GETNUM (TEXT, LENTXT, NDX, DP)
               IF (ERRLEV.GT.0) GO TO 990
               IF ((DP.LT.0.1D0) .OR. (DP.GT.365D0)) GO TO 990
               ROBLK(78+J-1) = DP
 50            CONTINUE
            END IF
C                                       TIMDEST limit: SAVE/GET
      ELSE IF (IOPT.EQ.22) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            IF ((DP.LT.3D0) .OR. (DP.GT.365D0)) GO TO 990
            ROBLK(93) = DP
            END IF
C                                       TIMDEST/EXIT limit messages.
      ELSE IF (IOPT.EQ.23) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            IF ((DP.LT.1D0) .OR. (DP.GT.365D0)) GO TO 990
            ROBLK(94) = DP
            END IF
C                                       TIMDEST limit: scratch files.
      ELSE IF (IOPT.EQ.24) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            IF ((DP.LT.1D0) .OR. (DP.GT.35D0)) GO TO 990
            ROBLK(95) = DP
            END IF
C                                       TIMDEST limit: empty CA files.
      ELSE IF (IOPT.EQ.25) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            IF ((DP.LT.0.1D0) .OR. (DP.GT.5D0)) GO TO 990
            ROBLK(96) = DP
            END IF
C                                       Time ranges for which AP batch
C                                       starts are denied.
      ELSE IF (IOPT.EQ.26) THEN
         IF (TEXT.NE.'-1') THEN
            DO 60 J = 97, 100
               CALL GETNUM (TEXT, LENTXT, NDX, DP)
               IF (ERRLEV.GT.0) GO TO 990
               IF ((DP.LT.0D0) .OR. (DP.GT.24D0)) GO TO 990
               ROBLK(J) = DP
 60            CONTINUE
            END IF
C                                       AP: roll time, delay formula.
      ELSE IF (IOPT.EQ.27) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            IF (DP.LT.1D0) GO TO 990
            ROBLK(101) = DP
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            IF (DP.LT.1D0) GO TO 990
            ROBLK(102) = DP
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            ROBLK(103) = DP
            END IF
C                                       Maximum line printer width.
      ELSE IF (IOPT.EQ.28) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.72) .OR. (K.GT.132)) GO TO 990
            IOBLK(68) = K
            END IF
C                                       Secondary AP space.
      ELSE IF (IOPT.EQ.29) THEN
C                                       Max length short vector.
      ELSE IF (IOPT.EQ.30) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.0) .OR. (K.GT.2000)) GO TO 990
            IOBLK(70) = NINT(DP)
            END IF
C                                       TK device screen size.
      ELSE IF (IOPT.EQ.31) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.300) .OR. (K.GT.16384)) GO TO 990
            IOBLK(224) = K
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.300) .OR. (K.GT.16384)) GO TO 990
            IOBLK(225) = K
            END IF
C                                       TK device char size.
      ELSE IF (IOPT.EQ.32) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.5) .OR. (K.GT.100)) GO TO 990
            IOBLK(226) = K
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.5) .OR. (K.GT.100)) GO TO 990
            IOBLK(227) = K
            END IF
C                                       Disk booking assignments.
      ELSE IF (IOPT.EQ.33) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            IDISK = NINT(DP)
            IF ((IDISK.LT.2) .OR. (IDISK.GT.IOBLK(1))) GO TO 990
            K = 104 + 8*(IDISK-1)
            DO 70 J = K,K+7
               CALL GETNUM (TEXT, LENTXT, NDX, DP)
               IF (ERRLEV.GT.0) GO TO 990
               IOBLK(J) = NINT(DP)
C                                       Pad out with zeroes.
               IF (TEXT(NDX:).EQ.' ') TEXT(NDX:) = ' 0'
 70            CONTINUE
            END IF
C                                       Printer file deletion delay
      ELSE IF (IOPT.EQ.34) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            K = NINT(DP)
            IF ((K.LT.10) .OR. (K.GT.36000)) GO TO 990
            IOBLK(228) = K
            END IF
C                                       Computer speed rating
      ELSE IF (IOPT.EQ.35) THEN
         IF (TEXT.NE.'-1') THEN
            CALL GETNUM (TEXT, LENTXT, NDX, DP)
            IF (ERRLEV.GT.0) GO TO 990
            IF ((DP.LT.0.01) .OR. (DP.GT.500.0)) GO TO 990
            ROBLK(238) = DP
            END IF
C                                       Invalid option.
      ELSE
         WRITE (MSGTXT,1080) IOPT
         GO TO 985
         END IF
C                                       Everything's just fine.
      POTERR = 0
C                                       Save the updated record.
      CALL ZFIO ('WRIT', SPLUN, SPIND, 1, IOBLK, IERR)
      IF (IERR.EQ.0) GO TO 990
      WRITE (MSGTXT,1090) IERR
C
 985  CALL MSGWRT (8)
 990  IF ((ERRLEV.GT.0) .OR. (POTERR.NE.0)) THEN
         MSGTXT = 'SPSET: INPUT SPECIFICATION ERROR, REQUEST IGNORED.'
         CALL MSGWRT (8)
         END IF
C                                       Close the SP file.
      IF (SPIND.NE.0) CALL ZCLOSE (SPLUN, SPIND, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('SPSET: SYSTEM PARAMETER FILE OPEN ERROR',I4)
 1030 FORMAT ('SPSET: ERROR',I4,' READING SP FILE.')
 1080 FORMAT ('SPSET: Unrecognized option',I4)
 1090 FORMAT ('SPSET: Error',I4,' saving parameters.')
      END
