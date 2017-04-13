      SUBROUTINE MAPSNC (NAX, NP, KVOL, LUN, IND, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! creates a scratch image file of specified dimensionality
C# Map-util
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
C   MAPSNC creates a scratch file of correct size to hold an NAX-
C   dimensional map image and opens it.   Uses SCREAT.
C   Inputs:
C      NAX    I         Number of axes
C      NP     I(NAX)    Number of points on each axis
C      KVOL   I         Disk to avoid if possible
C      LUN    I         LUN to use
C   Output:
C      IND    I         FTAB pointer to open file
C      SCRTCH I(512)    Scratch buffer
C      IERR   I         0 => file created, others => failure
C   Common:
C      /CFILES/ file description area - new scratch file entered there
C               in count (NSCR), cat number (SCRCNO), volume (SCRVOL).
C               The routine also uses the bad-disk list IBAD.
C      /MAPHDR/ also used for dummy CATBLK of cataloged scratch
C-----------------------------------------------------------------------
      INTEGER   NAX, NP(*), KVOL, LUN, IND, SCRTCH(512), IERR
C
      CHARACTER PHNAME*48
      INTEGER   ISIZE
      LOGICAL   T
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Map size: sectors/row
      CALL MAPSIZ (NAX, NP, ISIZE)
C                                       Lie to SCREAT
      IF (NSCR.GE.SCRLST-1) THEN
         MSGTXT = 'MAPSNC: SCRATCH FILE LIST OVERFLOWS'
         CALL MSGWRT (8)
         IERR = 9
         GO TO 999
         END IF
      NSCR = NSCR + 1
      IF (NSCR.LE.0) NSCR = 1
      SCRVOL(NSCR) = KVOL
C                                       Do create
      CALL SCREAT (ISIZE, SCRTCH, IERR)
C                                       Undo lie
      NSCR = NSCR - 1
      IF (NSCR.LE.0) GO TO 999
      SCRVOL(NSCR) = SCRVOL(NSCR+1)
      SCRCNO(NSCR) = SCRCNO(NSCR+1)
      IF (IERR.NE.0) GO TO 999
C                                       Do open
      CALL ZPHFIL ('SC', SCRVOL(NSCR), SCRCNO(NSCR), 1, PHNAME, IERR)
      CALL ZOPEN (LUN, IND, SCRVOL(NSCR), PHNAME, T, T, T, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1000)
         CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UNABLE TO OPEN SCRATCH FILE: LUN, IER =',2I7)
      END
