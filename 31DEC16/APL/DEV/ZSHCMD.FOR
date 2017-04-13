      SUBROUTINE ZSHCMD (LCOM, COMMND, LTASK, OUTDEV, IERR)
C-----------------------------------------------------------------------
C! execute a job-control level command string
C# Z2 System
C-----------------------------------------------------------------------
C;  Copyright (C) 2004-2005, 2011
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
C   Execute a "shell" (DCL or sh) command string below present task.
C   Inputs:
C      LCOM     I       Length of commnd
C      COMMND   C*(*)   The command line string
C      LTASK    I       Length of taskout
C      OUTDEV   C*(*)   File/device for standard output/error
C   Output:
C      IERR     I       Error return: 0 means okay
C                          1 input command too long
C                          2 some system error occurred
C   Generic version: calls ZSHCM2 with HOLLERITH conversion for all
C   but VMS
C-----------------------------------------------------------------------
      INTEGER   LCOM, LTASK, IERR
      CHARACTER COMMND*(*), OUTDEV*(*)
C
      HOLLERITH HCOM(256), HDEV(256)
      INTEGER   I
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (SYSTYP.EQ.'VMS') THEN
         CALL ZSHCM2 (LCOM, COMMND, LTASK, OUTDEV, IERR)
      ELSE
         I = LEN (COMMND)
         CALL CHR2H (I, COMMND, 1, HCOM)
         I = LEN (OUTDEV)
         CALL CHR2H (I, OUTDEV, 1, HDEV)
         CALL ZSHCM2 (LCOM, HCOM, LTASK, HDEV, IERR)
         END IF
C
 990  RETURN
      END
