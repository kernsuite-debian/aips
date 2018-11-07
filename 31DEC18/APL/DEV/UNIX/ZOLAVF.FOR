      SUBROUTINE ZOLAVF (FCB, NFILES, IERR)
C-----------------------------------------------------------------------
C! advance files on an on-line "tape" device
C# Z2 IO-basic Tape
C-----------------------------------------------------------------------
C;  Copyright (C) 2003
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
C   Advance files for on-line "tape devices"
C   Inputs:
C      FCB      I(*)   File control block for opened tape drive
C      NFILES   I      = 0 - start today's files
C                      < 0 - start yesterday's file
C                      > 0 - end of todays's file
C   Output:
C      IERR    I      Error return code: 0 => no error
C                        1 => at start today's despite NFILES < 0
C                        2 => no such operation
C                              3 => device not found
C                              6 => other open error
C   Unix version - actually calls oladvf
C-----------------------------------------------------------------------
      INTEGER   FCB(*), NFILES, IERR
C
      INTEGER   JERR, OLADVF
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      JERR = OLADVF (FCB, NFILES)
      IF (JERR.LT.0) THEN
         MSGTXT = 'OLADVF RETURNS ILLEGAL OPERATION ERROR'
         CALL MSGWRT (8)
         IERR = 2
      ELSE IF (JERR.GT.0) THEN
         MSGTXT = 'OLADVF COULD NOT POSITION IN PREVIOUS DAY''S FILE'
         CALL MSGWRT (8)
         IERR = 1
      ELSE
         IERR = 0
         IF (NFILES.EQ.-1) THEN
            MSGTXT = 'OLADVF: begin at start of yesterday''s file'
         ELSE IF (NFILES.LT.0) THEN
            JERR = -NFILES
            WRITE (MSGTXT,1000) JERR
         ELSE IF (NFILES.EQ.0) THEN
            MSGTXT = 'OLADVF: begin at start of today''s file'
         ELSE
            MSGTXT = 'OLADVF: begin at current time in today''s file'
            END IF
         CALL MSGWRT (3)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OLADVF: begin at start of file from',I3,' days ago')
      END
