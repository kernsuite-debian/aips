      SUBROUTINE VERMAT (IVOL, PNAME, MNAME, VERSON, IVER, AVERS, IERR)
C-----------------------------------------------------------------------
C! min match handling for text file names incl sequence of directories
C# Text
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2007
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
C   VERMAT performs min match handling for text file names via call(s)
C   to TXTMAT.  It handles a sequence of directory names conveyed via
C   VERSON and the local default in DCHCOM.  All messages are done.
C   Inputs:
C      IVOL    I        Volume number
C      PNAME   C*48     File name: 24 packed chars
C      VERSON  C*48     Directory code: 48 packed chars
C   In/out:
C      MNAME   C*(*)    In: test member name (<= 12 chars)
C                       Out: actual name found
C   Output:
C      IVER    I        Actual version "number": 1 - 10 =>
C                          OLD, NEW, TST, CVX, OLDPSAP, NEWPSAP,
C                          TSTPSAP, CVXPSAP, LOCAL,   other = 10
C      AVERS   R(*)     Actual VERSION string where found
C      IERR    I        Error code: 0 => ok
C                           1 => none
C                           2 => more than one match found
C                           3 => IO error
C-----------------------------------------------------------------------
      CHARACTER PNAME*48, MNAME*(*), AVERS*48, VERSON*(*)
      INTEGER   IVOL, IVER, IERR
C
      INTEGER   LOLD, LNEW, VSIZE, NC, I, ITRIM
      LOGICAL   EQUAL
      CHARACTER VERVER(9)*8
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA VERVER /'OLD ', 'NEW ', 'TST ', 'CVX', 'OLDPSAP', 'NEWPSAP',
     *   'TSTPSAP', 'CVXPSAP', 'LOCAL'/
C-----------------------------------------------------------------------
C                                       Loop over requested directories
      LOLD = 1
      VSIZE = LEN (VERSON)
      DO 20 I = 1,12
         LNEW = 0
         IF (LOLD.LT.VSIZE) LNEW = INDEX (VERSON(LOLD:), '|')
         IF (LNEW.GT.0) LNEW = LNEW + LOLD - 1
         IF (LNEW.EQ.0) LNEW = VSIZE + 1
         NC = LNEW - LOLD
         AVERS = ' '
         IF (LOLD.LE.VSIZE) AVERS(1:NC) = VERSON(LOLD:LOLD+NC-1)
         LOLD = LNEW + 1
         EQUAL = AVERS.EQ.' '
         IF (EQUAL) AVERS(1:3) = VERNAM(1:3)
         IF (EQUAL) LOLD = VSIZE + 2
         CALL TXTMAT (IVOL, PNAME, MNAME, AVERS, IERR)
         IF ((IERR.NE.1) .OR. (EQUAL)) GO TO 30
 20      CONTINUE
C                                       Done trying
 30   IF (IERR.GT.1) GO TO 999
      IF (IERR.EQ.0) GO TO 40
         I = ITRIM (MNAME)
         WRITE (MSGTXT,1030) MNAME(:I)
         CALL MSGWRT (7)
         GO TO 999
C                                       Got it: message?
 40   EQUAL = VERNAM(1:3).EQ.AVERS(1:3)
      IF (EQUAL) GO TO 50
         WRITE (MSGTXT,1040) AVERS
         CALL MSGWRT (2)
C                                       Integer type
 50   IVER = 10
      DO 55 I = 1,9
         IF (AVERS(1:8).EQ.VERVER(I)) IVER = I
 55      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('TEXT FOR ',A,' UNAVAILABLE')
 1040 FORMAT ('Found in Version=',A)
      END
