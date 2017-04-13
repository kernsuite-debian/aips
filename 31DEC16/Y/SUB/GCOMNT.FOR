      SUBROUTINE GCOMNT (ML, COMNT, BUFF, IERR)
C-----------------------------------------------------------------------
C! writes a comment command in a plot file
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2006
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
C   GCOMNT writes a comment command into a plot file and to the message
C   file.
C   The output record contains:
C             I      opcode, 18 in this command
C             I      nchar - number characters in string
C             H(*)   the comment string in holleriths
C   Inputs:
C      ML      I        message level to use -1 => no
C      COMNT   C*(*)    Comment
C   In/out:
C      BUFF    I(256)   Plot buffer
C   Output:
C      IERR    I        Error code: 1 disk error
C-----------------------------------------------------------------------
      CHARACTER COMNT*(*)
      INTEGER   ML, BUFF(256), IERR
C
      INTEGER   OPCODE, NC, JTRIM, NEED, NL
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA OPCODE /18/
C-----------------------------------------------------------------------
      IERR = 0
      NC = JTRIM (COMNT)
      NC = MIN (80, NC)
      IF (NC.GT.0) THEN
C                                       Plot files
         IF (.NOT.GPHDOT) THEN
            NEED = 257 - 2 - (NC+3)/4
C                                       See if enough room in buffer.
            IF (GPHPOS.GE.NEED) THEN
               CALL GPHWRT (BUFF, IERR)
               IF (IERR.NE.0) THEN
                  IERR = 1
                  GO TO 999
                  END IF
               END IF
C                                       Put data in buffer.
            BUFF(GPHPOS+1) = OPCODE
            BUFF(GPHPOS+2) = NC
            CALL CHR2H (NC, COMNT, 1, BUFF(GPHPOS+3))
            GPHPOS = GPHPOS + 2 + (NC+3)/4
            END IF
         IF (ML.GE.0) THEN
            NL = MAX (2, MIN (5, ML))
            MSGTXT = COMNT
            CALL MSGWRT (NL)
            END IF
         END IF
C
 999  RETURN
      END
