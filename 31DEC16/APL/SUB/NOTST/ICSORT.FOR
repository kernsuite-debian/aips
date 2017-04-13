      SUBROUTINE ICSORT (A, LEN, N, KEY1, KEY2, IERR)
C-----------------------------------------------------------------------
C! Two key in memory sort by one of several methods
C# Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2008-2009
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
C   ICSORT does an incore two key sort of data into descending order.
C   The method used depends on LEN and the success of a previously
C   attempted method.
C   Input:
C      LEN     I          Length of record in words.
C      N       I          Number of records.
C      KEY1    I          Location of slower varying key in record
C      KEY2    I          Location of faster varying key in record
C   In/out:
C      A       R(LEN,N)   Data array to be sorted.
C   Output:
C      IERR    I          Return code: 0 = OK
C                            1 = Bad input
C                            2 = exceeds internal buffer
C-----------------------------------------------------------------------
      INTEGER   LEN, N, KEY1, KEY2, IERR
      REAL      A(LEN,N)
C
      INTEGER   JERR, IP(204802), NP2
      REAL      WK(2,204802)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Check that input OK.
      IERR = 0
      IF ((LEN.LE.0) .OR. (N.LE.0) .OR. (KEY1.GT.LEN) .OR.
     *   (KEY2.GT.LEN)) IERR = 1
      IF ((KEY1.LT.0) .OR. (KEY2.LT.0)) IERR = 1
      IF (N.GT.204800) IERR = 2
      IF (IERR.NE.0) GO TO 999
C                                       If N = 1 return
      IF (N.LE.1) GO TO 999
      NP2 = N + 2
C                                       Check if long record, if so
C                                       use min. switch method.
C                                       Try quick sort
      IF (LEN.GT.2048) GO TO 100
         CALL OSORT (A, N, NP2, KEY1, KEY2, LEN, WK, IP, JERR)
         IF (JERR.EQ.0) GO TO 999
            WRITE (MSGTXT,1000) JERR
            CALL MSGWRT (6)
C                                       Try shell sort
            CALL SHSORT (A, N, KEY1, KEY2, LEN, JERR)
            IF (JERR.EQ.0) GO TO 999
               WRITE (MSGTXT,1001) JERR
               CALL MSGWRT (6)
C                                       Long record sort.
 100     CALL LSORT (A, N, KEY1, KEY2, LEN)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ICSORT: QUICK SORT ERROR',I3,' TRY SHELL SORT')
 1001 FORMAT ('ICSORT: SHELL SORT ERROR',I3,' TRY LONG SORT')
      END
