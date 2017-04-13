      SUBROUTINE ZWAIT (LUN, FIND, IBUFF, IERR)
C-----------------------------------------------------------------------
C! wait for asynchronous ("MAP") IO to finish
C# IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2004
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
C   Wait until an asynchronous I/O operation completes.
C   Inputs:
C      LUN     I   Logical unit number
C      FIND    I   Index in FTAB to file control block for LUN
C      IBUFF   I   Buffer # to wait for (1 or 2)
C   Output:
C      IERR    I   Error return code: 0 => no error
C                     1 => LUN not open in FTAB
C                     2 => error in inputs
C                     3 => I/O error
C                     4 => end of file
C                     7 => wait service error
C   UNIX version - does not actually do double buffering since lower
C   level routines are pure wait mode routines anyway.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IBUFF, IERR
C
      INTEGER   FCBOFF, IEREOF
      LOGICAL   T
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA T /.TRUE./
      DATA IEREOF /4/
C-----------------------------------------------------------------------
      IERR = 0
C                                       LUN open in FTAB?
      IF (FTAB(FIND).EQ.LUN) GO TO 10
         IERR = 1
         WRITE (MSGTXT,1000) LUN
         GO TO 995
C                                       Valid buffer number?
 10   IF ((IBUFF.EQ.1) .OR. (IBUFF.EQ.2)) GO TO 20
         IERR = 2
         WRITE (MSGTXT,1010) IBUFF
         GO TO 995
C                                       Does not handle tapes (as of
C                                       the 15APR87 release).
 20   IF (LUN.LT.129-NTAPED) GO TO 30
         IERR = 2
         WRITE (MSGTXT,1020)
         GO TO 995
C                                       Calculate offset in FTAB to
C                                       file control block for buffer 1
 30   FCBOFF = FIND + MOFF
      CALL ZWAI2 (FTAB(FCBOFF), IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.IEREOF)) THEN
         WRITE (MSGTXT,1030) IERR, LUN, IBUFF
         CALL MSGWRT (7)
         CALL ZERROR ('ZWAI2 ', FTAB(FCBOFF+FCBERR), ' ',
     *      FTAB(FIND+MOFF), T)
         END IF
      GO TO 999
C
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZWAIT: LUN = ',I2,' NOT OPEN IN FTAB')
 1010 FORMAT ('ZWAIT: INVALID BUFFER NUMBER = ',I2)
 1020 FORMAT ('ZWAIT: DOES NOT PERFORM TAPE I/O AS OF 15APR87')
 1030 FORMAT ('ZWAIT: ZWAI2 RETURNS ERROR ',I1,' FOR LUN = ',I2,
     *   ' BUFFER = ',I1)
      END
