      SUBROUTINE ZCMPRS (IVOL, PNAME, LUN, LSIZE, IERR)
C-----------------------------------------------------------------------
C! release space from the end of an open disk file
C# Z IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997
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
C   ZCMPRS releases unused disk space from the end of an open disk file.
C   "Byte" defined as 1/2 of a small integer.
C   Inputs:
C      IVOL    I        volume number
C      PNAME   C*48     physical file name
C      LUN     I        logical unit number under which file is open.
C   In/Out:
C      LSIZE   I        (In) desired final size in AIPS blocks
C                       (Out) actual final size in AIPS blocks
C   Outputs:
C      IERR    I        error code: 0 => ok
C                                   1 => input data error
C                                   2 => compress error
C   Generic version - uses ZCMPR2.
C-----------------------------------------------------------------------
      CHARACTER PNAME*48
      INTEGER   IVOL, LUN, LSIZE, IERR
C
      INTEGER   TOFF, FIND, SYSERR, IMAP, BSIZE
      LOGICAL   MAP
      HOLLERITH ANAME(12)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
C-----------------------------------------------------------------------
C                                       build file descriptor
C                                       Find the FTAB entry.
      TOFF = NMOFF
      MAP = .FALSE.
      IMAP = 0
      CALL LSERCH ('SRCH', LUN, FIND, MAP, IERR)
      IF (IERR.EQ.0) GO TO 10
         MAP = .TRUE.
         TOFF = MOFF
         IMAP = 1
         CALL LSERCH ('SRCH', LUN, FIND, MAP, IERR)
         IF (IERR.EQ.0) GO TO 10
            WRITE (MSGTXT,1000) LUN
            CALL MSGWRT (7)
            GO TO 999
C                                       Truncate file
 10   BSIZE = LSIZE
      CALL CHR2H (48, PNAME, 1, ANAME)
      CALL ZCMPR2 (FTAB(FIND+TOFF), ANAME, BSIZE, IMAP, SYSERR, IERR)
      IF (IERR.EQ.0) THEN
         LSIZE = BSIZE
      ELSE IF (IERR.EQ.1) THEN
         WRITE (MSGTXT,1010) PNAME
         CALL MSGWRT (7)
      ELSE
         CALL ZERROR ('ZCMPR2', SYSERR, PNAME, -999, MAP)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZCMPRS: LUN NOT OPEN ',I6)
 1010 FORMAT ('ZCMPRS: CAN''T TRANSLATE NAME =',A)
      END
