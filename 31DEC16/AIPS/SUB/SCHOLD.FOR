      SUBROUTINE SCHOLD (QUIT)
C-----------------------------------------------------------------------
C! wait for user input on screen full, allows type ahead, quit, continue
C# POPS-util
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
C   SCHOLD (screen hold) will wait for the user to press return or enter
C   a 'Q' for quit, thus giving the user time to read his screen before
C   CRT printout continues.  This also gives the user a chance to stop a
C   long printout without waiting for it to finish.
C   Output:
C      QUIT   L    F if Q not entered, T if Q entered by user
C-----------------------------------------------------------------------
      LOGICAL   QUIT
C
      CHARACTER LPT*1
      INTEGER   LIUNIT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DIO.INC'
C-----------------------------------------------------------------------
      QUIT = .FALSE.
C                                       meaningful only in non-batch
      IF (NPOPS.GT.NINTRN+1) GO TO 999
      IF (ISBTCH.EQ.32000) GO TO 999
      IF ((NPOPS.EQ.NINTRN+1) .AND. (TSKNAM.NE.'BATER') .AND.
     *   (TSKNAM.NE.'GRIPR')) GO TO 999
      IF (TSKNAM(1:5).EQ.'AIPSC') GO TO 999
C                                      Stop until user reads this page.
      IF ((IUNIT.NE.1) .AND. (IUNIT.NE.4)) GO TO 999
         WRITE (MSGTXT,1000)
         CALL MSGWRT (1)
         LPT = IPT
         LIUNIT = IUNIT
         IUNIT = 1
         IPT = '#'
         CALL PREAD (KARBUF)
         IUNIT = LIUNIT
         IPT = LPT
C                                       See if we need to quit print.
         IF (NBYTES.EQ.0) GO TO 999
            QUIT = .TRUE.
            CALL CHLTOU (4, JBUFF)
            IF ((JBUFF(1:2).EQ.'Q ') .OR. (JBUFF(1:5).EQ.'QUIT ') .OR.
     *         (IUNIT.EQ.4)) GO TO 999
               HOLDUF = JBUFF
               IUNIT = 4
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('** press RETURN for more, enter Q or next line ',
     *   'to quit print **')
      END
