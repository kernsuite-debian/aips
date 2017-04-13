      SUBROUTINE ZMKTMP (FLEN, FILNAM, IERR)
C-----------------------------------------------------------------------
C! convert a "temporary" file name into a unique name
C# Z IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004
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
C   Form a unique, fully qualified, temporary file name.
C   Inputs:
C      FLEN     I       Length of "filnam"
C   In/Out:
C      FILNAM   H*(*)   File name with the extension .XXXXXX, (e.g.,
C                       "ZXLPRT.XXXXXX").  The extension will be
C                       transformed by 'mktemp' to make "filnam" a
C                       unique file name.
C   Output:
C      IERR     I       Error return code: 0 => no error
C                          1 => inputs wrong
C                          2 => system service fails
C                          3 => filnam too short
C   VMS version
C-----------------------------------------------------------------------
      INTEGER   FLEN, IERR
      HOLLERITH FILNAM*(*)
C
      INTEGER   ITEM(4), SYS$GETJPIW, JERR, ILOC, LBUFF, BUFF, LLEN,
     *   ITRIM, I
      INTEGER*2 ITEM2(8)
      CHARACTER CTEMP*5, LFILN*256
      LOGICAL   EXISTS
      INCLUDE '($JPIDEF)'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (ITEM2, ITEM)
C-----------------------------------------------------------------------
      IERR = 0
      CALL H2CHR (FLEN, 1, FILNAM, LFILN)
C                                       Locate XXXXXXs
      ILOC = INDEX (LFILN, 'XXXXXX')
      IF (ILOC.LE.0) THEN
         IERR = 1
         MSGTXT = 'ZMKTMP: IMPROPER TEMPLATE FOR TEMPORARY FILE NAME'
         GO TO 995
         END IF
C                                       build structure
      ITEM2(1) = 4
      ITEM2(2) = JPI$_PID
      ITEM(2) = %LOC (BUFF)
      ITEM(3) = %LOC (LBUFF)
      ITEM(4) = 0
C                                       call service
      JERR = SYS$GETJPIW ( , , , ITEM, , , )
      IF (JERR.NE.1) THEN
         IERR = 2
         WRITE (MSGTXT,1010) JERR
         GO TO 995
         END IF
C                                       Build file name
      BUFF = MOD (BUFF-1, 1000000) + 1
      CALL ZHEX (BUFF, 5, CTEMP)
      LFILN(ILOC+1:ILOC+5) = CTEMP
      LLEN = ITRIM (LFILN)
C                                       make unique
      DO 20 I = 1,16
         LBUFF = I - 1
         CALL ZHEX (LBUFF, 1, LFILN(ILOC:ILOC))
         INQUIRE (FILE=LFILN(:LLEN), EXIST=EXISTS)
         IF (.NOT.EXISTS) THEN
            CALL CHR2H (LLEN, LFILN, 1, FILNAM)
            GO TO 999
            END IF
 20      CONTINUE
      IERR = 3
      MSGTXT = 'ZMKTMP: UNABLE TO BUILD UNIQUE NAME'
C
 995  CALL MSGWRT (6)
      MSGTXT = 'ZMKTMP: ' // LFILN
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ZMKTMP: GETJPI RETURNS ERROR CODE',I8)
      END
