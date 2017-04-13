      SUBROUTINE TXTMAT (IVOL, PNAME, MNAME, VERSON, IERR)
C-----------------------------------------------------------------------
C! min match handling for text files (calls ZTXMAT, does messages)
C# Text
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2007
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
C   TXTMAT performs min match handling for text file names.  Does all
C   needed messages as well, EXCEPT IERR = 1 message.
C   Inputs:
C      IVOL    I       Volume number
C      PNAME   C*48    File name
C      VERSON  C*48    Directory code
C   In/out:
C      MNAME   C*(*)   In: test member name  (<= 12 chars)
C                      Out: actual name found
C   Output:
C      IERR    I       Error code: 0 => ok
C                             1 => none
C                             2 => more than one match found
C                             3 => IO error
C-----------------------------------------------------------------------
      CHARACTER PNAME*(*), MNAME*(*), VERSON*(*)
      INTEGER   IVOL, IERR
C
      INTEGER   NCH, I1, IT, NNAM, I, ITRIM
      CHARACTER LNAME*12
      HOLLERITH NAMES(3,64)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       How many chars?
      NCH = ITRIM (MNAME)
C                                       Null name not allowed
      IF (MNAME.EQ.' ') THEN
         WRITE (MSGTXT,1000)
         CALL MSGWRT (6)
         IERR = 2
         GO TO 999
         END IF
C                                       Get list of possibles
 20   NNAM = 64
      CALL ZTXMAT (IVOL, PNAME, MNAME, NCH, VERSON, NAMES, NNAM, IERR)
      IF (IERR.GE.2) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (7)
         IERR = 3
C                                       None
      ELSE IF (NNAM.LE.0) THEN
         IERR = 1
C                                       Only one
      ELSE IF (NNAM.EQ.1) THEN
         I = LEN (MNAME)
         I = MIN (12, I)
         CALL H2CHR (I, 1, NAMES, MNAME)
         IERR = 0
C                                       Multiple: is there exact
      ELSE
         IT = 0
         I1 = 0
         DO 60 I = 1,NNAM
            CALL H2CHR (12, 1, NAMES(1,I), LNAME)
            IF (LNAME.EQ.MNAME) THEN
               I1 = I
               IT = IT + 1
               END IF
 60         CONTINUE
C                                       Exact found
         IF (IT.EQ.1) THEN
            I = LEN (MNAME)
            I = MIN (12, I)
            CALL H2CHR (I, 1, NAMES(1,I1), MNAME)
            IERR = 0
C                                       Error: multiple match
         ELSE
            IERR = 2
            DO 70 I = 1,NNAM
               CALL H2CHR (12, 1, NAMES(1,I), LNAME)
               WRITE (MSGTXT,1060) LNAME
               CALL MSGWRT (1)
 70            CONTINUE
            WRITE (MSGTXT,1070) MNAME(:NCH)
            CALL MSGWRT (6)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TXTMAT: NO MEMBER NAME GIVEN')
 1020 FORMAT ('ZTXMAT ERROR =',I7)
 1060 FORMAT ('NAME NOT UNIQUE: COULD BE ',A)
 1070 FORMAT ('NAME: ',A,' INCORRECTLY SPECIFIED')
      END
