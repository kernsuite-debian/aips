      SUBROUTINE ZCLOSE (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C! closes open devices: disk, line printer, terminal
C# Z IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2004, 2015
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
C   Close the file associated with LUN removing any exclusive use state
C   and clear the FTAB entry for the LUN.
C   Inputs:
C      LUN      I           Logical unit number
C      FIND     I           Index in FTAB to file control block for LUN
C   Output:
C      IERR     I           Error return code: 0 => no error
C                              1 => close error
C                              2 => file already closed in FTAB
C                              3 => both errors
C                              4 => erroneous LUN
C   Generic version.
C   No longer closes TV devices as of the 15OCT87 release.
C   No longer closes tape devices as of the 15APR87 release.
C   No longer closes Tektronix devices as of the 15OCT87 release.
C   No longer closes text files as of the 15OCT87 release.
C   No longer closes line printer as of the 15JAN91 release.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      INTEGER   ERRLUN, ERRTER, JERR, FCBOFF, IMAP, I, J
      LOGICAL   T, F, MAP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA T, F /.TRUE., .FALSE./
      DATA ERRLUN, ERRTER /12, 6/
C-----------------------------------------------------------------------
C                                       Check inputs.
      IERR = 4
C                                       Valid LUN?
      IF ((LUN.LE.0) .OR. (LUN.GT.120) .OR. (LUN.EQ.ERRLUN) .OR.
     *   (LUN.EQ.ERRTER)) THEN
         WRITE (MSGTXT,1000) LUN
         GO TO 995
         END IF
C                                       Does not handle TV
      IF (DEVTAB(LUN).EQ.4) THEN
         MSGTXT = 'ZCLOSE: DOES NOT PERFORM TV CLOSES AS OF 15OCT87'
         GO TO 995
         END IF
C                                       Does not handle tape
      IF (LUN.GE.129-NTAPED) THEN
         MSGTXT = 'ZCLOSE: DOES NOT PERFORM TAPE CLOSES AS OF 15APR87'
         GO TO 995
         END IF
C                                       Does not handle Tektronix
      IF (LUN.EQ.7) THEN
         MSGTXT = 'ZCLOSE: DOES NOT PERFORM TEKTRONIX CLOSES AS OF '
     *      // '15OCT87'
         GO TO 995
         END IF
C                                       not line printer 15JAN91
      IF (LUN.EQ.1) THEN
         MSGTXT = 'ZCLOSE: DOES NOT PERFORM LINE PRINTER CLOSES AS OF '
     *      // '15JAN91'
         GO TO 995
         END IF
C                                       Does not handle text files
      IF (DEVTAB(LUN).EQ.3) THEN
         MSGTXT = 'ZCLOSE: DOES NOT PERFORM TEXT FILE CLOSES AS OF '
     *      // '15OCT87'
         GO TO 995
         END IF
C                                       Locate LUN in FTAB.
      IERR = 0
      MAP = F
      CALL LSERCH ('SRCH', LUN, I, MAP, JERR)
      IF (JERR.EQ.0) THEN
         FCBOFF = NMOFF
         IMAP = 0
      ELSE
         MAP = T
         CALL LSERCH ('SRCH', LUN, I, MAP, JERR)
         IF (JERR.EQ.0) THEN
            FCBOFF = MOFF
            IMAP = 1
C                                       LUN already closed in FTAB.
         ELSE
            IERR = 2
            WRITE (MSGTXT,1050) LUN
            GO TO 995
            END IF
         END IF
      IF (I.NE.FIND) THEN
         WRITE (MSGTXT,1055) LUN, I, FIND
         CALL MSGWRT (6)
         FIND = I
         END IF
C                                       Close a binary disk file here.
      IF (MOD (DEVTAB(LUN), 2).EQ.0) THEN
         FCBOFF = FCBOFF + FIND
         CALL ZDACLS (FTAB(FCBOFF), IMAP, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1060) IERR
            CALL MSGWRT (7)
            CALL ZERROR ('ZDACLS', FTAB(FCBOFF+FCBERR), ' ',
     *         FTAB(FCBOFF), MAP)
            END IF
C                                       Close a Fortran device here.
C                                       Terminal?
      ELSE IF (LUN.EQ.5) THEN
         CALL ZTTCLS (LUN, FIND, IERR)
         IF (IERR.NE.0) THEN
            IF (FTAB(FIND+1+NFCBER).GT.0) THEN
               CALL ZERROR ('ZTTCLS', FTAB(FIND+1+NFCBER), ' ', -999,
     *            .FALSE.)
            ELSE
               WRITE (MSGTXT,1080) IERR, LUN
               CALL MSGWRT (7)
               END IF
            IERR = 1
            END IF
C                                       Unknown Fortran device LUN.
      ELSE
         WRITE (MSGTXT,1090) LUN
         IERR = 4
         GO TO 995
         END IF
C                                       Clear FTAB entry for LUN.
      CALL LSERCH ('CLOS', LUN, FIND, MAP, JERR)
      IF (JERR.NE.0) THEN
         IERR = IERR + 2
         WRITE (MSGTXT,1100) LUN
         GO TO 995
         END IF
      CALL FSERCH (FIND, I, J, JERR)
      IF (JERR.EQ.0) THEN
         IF (I.EQ.1) DEVNAM(J) = ' '
         IF (I.EQ.2) NONNAM(J) = ' '
         IF (I.EQ.3) MAPNAM(J) = ' '
         END IF
      GO TO 999
C
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZCLOSE: INVALID LUN = ',I2)
 1050 FORMAT ('ZCLOSE: LUN = ',I2,' ALREADY CLOSED IN FTAB ON SEARCH')
 1055 FORMAT ('ZCLOSE: LUN = ',I2,' FIND VALUES',2I5,' DISAGREE')
 1060 FORMAT ('ZCLOSE: ZDACLS RETURNS ERROR = ',I1)
 1080 FORMAT ('ZCLOSE: ZTTCLS RETURNS ERROR ',I2,' FOR LUN = ',I6)
 1090 FORMAT ('ZCLOSE: UNKNOWN FORTRAN DEVICE.  LUN = ',I2)
 1100 FORMAT ('ZCLOSE: LUN = ',I2,' ALREADY CLOSED IN FTAB ON CLOSE')
      END
