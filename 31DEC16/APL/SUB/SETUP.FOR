      SUBROUTINE SETUP (PRGNAM, NPARMS, FIRST, IWBLK, IERR)
C-----------------------------------------------------------------------
C! does several task start up chores for non-interactive tasks
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   This routine does the initial things usually needed for an AIPS
C   task.  It initializes the device characteristics, header pointer,
C   and file commons, obtains the identification and user parameters
C   from the starting program (AIPS, AIPSB), and resumes the calling
C   program if requested.
C   NOTE: interactive tasks must not use this routine due to that
C   automatic resumption.
C   Use of this subroutine requires that (at least) the includes DDCH,
C   DMSG, DFIL, and DHDR appear also in the MAIN portion of the program.
C   Inputs:
C      PRGNAM   C*6      Name for this task.
C      NPARMS   I        Number of REAL*4 parameters comming from AIPS.
C      FIRST    R        First variable in inputs common block.
C      IWBLK    I(256)   Work buffer.
C   Output:
C      IERR     I        Error code. 0=ok
C                           1=couldn't get parms from AIPS.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      REAL      FIRST(*)
      INTEGER   NPARMS, IWBLK(256), IERR
C
      INTEGER   IERR2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      TSKNAM = PRGNAM
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE.)
C                                       Initialize header index values.
      CALL VHDRIN
C                                       Zero counters for DIE.
      NCFILE = 0
      NSCR = 0
      CALL FILL (10, 0, IBAD)
C                                       Get input values from AIPS.
      IERR = 0
      CALL GTPARM (PRGNAM, NPARMS, RQUICK, FIRST, IWBLK, IERR)
      IF (IERR.EQ.0) GO TO 20
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         IERR = 1
 20   IF (RQUICK) CALL RELPOP (IERR, IWBLK, IERR2)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR GETTING PARAMETERS FROM AIPS. GTPARM ERR =',I5)
      END
