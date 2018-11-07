      SUBROUTINE CHKCHR (STRING, IERR)
C-----------------------------------------------------------------------
C! Checks that a character string contains valid characters.
C# Character
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   Checks that all characters in a string are allowed characters:
C   A-Z, a-z, blank, 0-9 and +-*?_=!@#$%^&(){}[]:";~`,./<>|
C   Inputs:
C      STRING  C*(*)  String to be checked
C   Output:
C      IERR    I      0=OK, 1= contains bad characters
C-----------------------------------------------------------------------
      CHARACTER STRING*(*)
      INTEGER   IERR
C
      CHARACTER C*1, GOODCH*30
      INTEGER   NUMCHR, LOOP, BADCHR
      LOGICAL   OKCHAR
      INCLUDE 'INCS:DMSG.INC'
      DATA GOODCH /'+-*?_=!@#$%^&(){}[]:";~`,./<>|'/
C-----------------------------------------------------------------------
      IERR = 0
C                                       How many characters?
      NUMCHR = LEN (STRING)
      DO 100 LOOP = 1,NUMCHR
C                                       Check characters one at a time
         C = STRING(LOOP:LOOP)
         OKCHAR = ((C.GE.'A') .AND. (C.LE.'Z')) .OR. (C.EQ.' ')
         OKCHAR = OKCHAR .OR. ((C.GE.'0') .AND. (C.LE.'9'))
         OKCHAR = OKCHAR .OR. (INDEX (GOODCH, C) .GT. 0)
         OKCHAR = OKCHAR .OR. ((C.GE.'a') .AND. (C.LE.'z'))
         BADCHR = LOOP
         IF (.NOT. OKCHAR) GO TO 900
 100     CONTINUE
C                                       String OK
      GO TO 999
C                                       Bad string
 900  WRITE (MSGTXT,1100) BADCHR, STRING
      CALL MSGWRT (7)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('CHARACTER',I5,' BAD IN ',A)
      END
