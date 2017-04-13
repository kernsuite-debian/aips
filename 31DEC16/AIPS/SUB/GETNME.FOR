      SUBROUTINE GETNME (DEFNAM, NCLIM, OUTNAM)
C-----------------------------------------------------------------------
C! gets the next name in the input character buffer
C# POPS-lang
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
C   GETNME parses a name from input buffer KARBUF.
C   Inputs:
C      DEFNAM   C*(*)   default name (' ' => none)
C      NCLIM    I       max no. of characters allowed.
C   Output:
C      OUTNAM   C*(*)   output name.
C-----------------------------------------------------------------------
      CHARACTER DEFNAM*(*), OUTNAM*(*)
      INTEGER   NCLIM
C
      CHARACTER PRGNAM*6, M*1
      INTEGER   POTERR, J
      LOGICAL   WASQ, WASP
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DSMS.INC'
      DATA PRGNAM /'GETNME'/
C-----------------------------------------------------------------------
C                                       Insert default.
      OUTNAM = DEFNAM
C                                       Parse.
      WASP = .FALSE.
      WASQ = .FALSE.
      NKAR = 0
C                                       Skip leading blanks.
 10   M = KARBUF(KBPTR:KBPTR)
         IF (M.NE.' ') GO TO 15
            KBPTR = KBPTR + 1
            IF (KBPTR.LE.KARLIM) GO TO 10
            GO TO 999
 15   IF ((.NOT.WASQ) .AND. (.NOT.WASP) .AND. (M.EQ.';')) GO TO 999
      IF (M.NE.'''') GO TO 20
         KBPTR = KBPTR + 1
         IF (WASQ) GO TO 999
         WASQ = .TRUE.
         GO TO 10
 20   IF (M.NE.'(') GO TO 25
         KBPTR = KBPTR + 1
         WASP = .TRUE.
         GO TO 10
 25   IF (M.NE.')') GO TO 30
         KBPTR = KBPTR + 1
         IF (WASP) GO TO 999
         POTERR = 9
         GO TO 980
C                                       Test for alpha characters.
 30   POTERR = 9
      IF (((M.LT.'A') .OR. (M.GT.'Z')) .AND. ((M.LT.'a') .OR.
     *   (M.GT.'z'))) GO TO 980
C                                       Find trailing blank or ;
      NKAR = 0
      J = KBPTR
 35   NKAR = NKAR + 1
         J = J + 1
         POTERR = 4
         IF (J.GT.KARLIM) GO TO 980
         M = KARBUF(J:J)
         IF ((WASQ) .AND. (M.EQ.'''')) GO TO 40
         IF ((WASP) .AND. (M.EQ.')')) GO TO 40
         IF ((WASP) .OR. (WASQ)) GO TO 35
         IF ((M.NE.' ') .AND. (M.NE.';')) GO TO 35
C
 40   POTERR = 5
      IF (NKAR.GT.NCLIM) GO TO 980
      OUTNAM = KARBUF(KBPTR:KBPTR+NKAR-1)
      KBPTR = KBPTR + NKAR
      CALL CHLTOU (NKAR, OUTNAM)
      IF ((WASQ) .AND. (M.EQ.'''')) KBPTR = KBPTR + 1
      IF ((WASP) .AND. (M.EQ.')')) KBPTR = KBPTR + 1
      IF ((WASP) .AND. (WASQ) .AND. (M.EQ.'''')) KBPTR = KBPTR + 1
      GO TO 999
C                                       Errors.
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      ERRLEV = ERRLEV+1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
      END
