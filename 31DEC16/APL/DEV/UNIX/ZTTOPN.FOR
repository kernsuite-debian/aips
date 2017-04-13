      SUBROUTINE ZTTOPN (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C! open a terminal device
C# Z2 Terminal
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002, 2004
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
C   Open a terminal - does not use MSGWRT because it is called by
C   ZMSGOP.  The FTAB open is done here (not to be done in ZOPEN).
C   Inputs:
C      LUN    I   Logical unit number (must be 5 or 6)
C   Output:
C      FIND   I   Position in FTAB
C      IERR   I   Error return code: 0 => no error
C                    1 => Fortran open error
C                    2 => input error
C   Generic UNIX version - assume unit will be used for terminal I/O.
C   If a message terminal is specified via the environment variable
C   TASKTT<npops>, or TTDEV and the latter not set to "TTDEV00", ZTTOP2
C   is called.  This will either redirect the stdout and stderr streams
C   to a physical terminal, or will open a socket (Berkeley but
C   supported on most systems) to the message window via service
C   "msgserv".  Any error with msgserver code causes TTDEV to be reset
C   to TTDEV00.  Note that the ZTTOP2 code for TASKTT<npops> has not
C   been implemented in the generic case.
C
C   This version remembers if the logical/environment variable
C   translation has already been done and skips unnecessary translation.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      INTEGER   XLEN, XLNB, JERR, I
      LOGICAL   SALONE
      CHARACTER DEVTTY*16, TASKTT*8, XLATED*256, XLATD2*256
      HOLLERITH HLATED(64)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
C                                       Keep this around
      SAVE XLATED, XLATD2
      INCLUDE 'INCS:VSAT.INC'
      DATA XLATD2, XLATED  /2 * ' '/
C-----------------------------------------------------------------------
C                                       Check inputs.
      FIND = 0
      IF ((LUN.NE.5) .AND. (LUN.NE.6)) THEN
         IERR = 2
         GO TO 999
         END IF
C                                       If the LUN is not marked as open
C                                       in the file table then open it,
C                                       otherwise do nothing:
      CALL LSERCH ('SRCH', LUN, FIND, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         IERR = 0
C                                       Open in FTAB
         CALL LSERCH ('OPEN', LUN, FIND, .FALSE., JERR)
         IF (JERR.NE.0) FIND = 0
C                                       If LUN designates standard
C                                       output then, if the current
C                                       program is not in the list of
C                                       stand-alone AIPS tasks look for
C                                       a translation of TASKTTn where n
C                                       is the current POPS number:
         IF (LUN.EQ.6) THEN
            SALONE = .FALSE.
            DO 15 I = 1,NSANAM
               IF (TSKNAM.EQ.SATASK(I)) SALONE = .TRUE.
 15            CONTINUE
            IF (.NOT.SALONE) THEN
C                                       Check COMMON variable in case
C                                       the translation is already done.
               IF (MSGRMB.EQ.1) THEN
C                                       We already did the translation
C                                       so skip straight to ZTTOP2
                  CALL CHR2H (256, XLATD2, 1, HLATED)
                  CALL ZTTOP2 (FTAB(FIND), HLATED, JERR)
                  IF (JERR.EQ.6) THEN
                     IERR = 0
                     XLATD2 = ' '
                     MSGRMB = 0
                  ELSE IF (JERR.EQ.3) THEN
                     WRITE (*, 1002) JERR, DEVTTY
                     IERR = 1
                     XLATD2 = ' '
                     MSGRMB = 0
                     GO TO 999
                  ELSE IF (JERR.NE.0) THEN
                     WRITE (*, 1000) JERR, DEVTTY
                     IERR = 1
                     XLATD2 = ' '
                     MSGRMB = 0
                     GO TO 999
                     END IF
               ELSE
C                                       Coming in cold so check for a
C                                       physical message terminal
C                                       definition in TASKTTn.
                  TASKTT = 'TASKTT'
                  CALL ZEHEX (NPOPS, 1, TASKTT(7:7))
                  XLEN = LEN (XLATED)
                  CALL ZTRLOG (7, TASKTT, XLEN, XLATED, XLNB, JERR)
C                                       If TASKTTn is defined then more
C                                       action is needed that can't be
C                                       done here.  On a real message
C                                       terminal, a freopen of stdout
C                                       and stderr will be done.
                  IF (JERR.EQ.0) THEN
                     DEVTTY = XLATED(1:XLNB)
                     CALL CHR2H (256, DEVTTY, 1, HLATED)
                     CALL ZTTOP2 (FTAB(FIND), HLATED, JERR)
                     IF (JERR.NE.0) THEN
                        WRITE (*, 1000) JERR, DEVTTY
                        IERR = 1
                        GO TO 999
                        END IF
                  ELSE
C                                       TASKTTn not defined, but check
C                                       for TTDEV and TTDEVnn for the
C                                       Message Server X11/socket stuff
                     TASKTT = 'TTDEV'
                     XLEN = LEN (XLATED)
                     CALL ZTRLOG (5, TASKTT, XLEN, XLATED, XLNB, JERR)
                     IF (JERR.NE.0) THEN
C                                       For compatibility, silently
C                                       ignore case where not defined.
                        IERR = 0
                     ELSE
C                                       That should be TTDEVxx, so pass
C                                       it directly to ZTTOP2 unless it
C                                       is TTDEV00.
                        DEVTTY = XLATED(1:XLNB)
                        IF (DEVTTY .NE. 'TTDEV00') THEN
                           CALL CHR2H (256, DEVTTY, 1, HLATED)
                           CALL ZTTOP2 (FTAB(FIND), DEVTTY, JERR)
C                                       If there were problems, revert
C                                       to normal output silently.
                           IF (JERR.EQ.6) THEN
C                                       Problems with socket or connect.
C                                       Most likely the message window
C                                       died so just go back to normal
C                                       mode without a fuss.
                              IERR = 0
                              XLATD2 = ' '
                              MSGRMB = 0
C                                       get{host/serv}byname failed
                           ELSE IF (JERR.EQ.3) THEN
                              WRITE (*, 1002) JERR, DEVTTY
                              XLATD2 = ' '
                              MSGRMB = 0
                              IERR = 1
                              GO TO 999
                           ELSE IF (JERR.NE.0) THEN
                              WRITE (*, 1000) JERR, DEVTTY
                              XLATD2 = ' '
                              MSGRMB = 0
                              IERR = 1
                              GO TO 999
C                                       Remember for next time.
                           ELSE
                              XLATD2 = DEVTTY
                              MSGRMB = 1
C                                       end check on ZTTOP2 status
                              END IF
C                                       end check on not TTDEV00
                           END IF
C                                       end check on TTDEV xlation stat
                        END IF
C                                       end check on TASKTT xlation stat
                     END IF
C                                       end check on XLATD2 not blank
                  END IF
C                                       end check on standalone
               END IF
C                                       end check on LUN = 6
            END IF
C                                       end check on LSERCH status
         END IF
C                                       Success. Clear error status of
C                                       LUN in file table:
      FTAB(FIND+NFCBER) = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR ', I2, ' ASSIGNING OUTPUT TO ', A)
 1002 FORMAT ('ERROR ', I2, ' ON UNIT ', A, ', GETTING HOST/SERVICE BY',
     *   ' NAME')
      END
