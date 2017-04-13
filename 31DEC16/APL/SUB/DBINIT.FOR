      SUBROUTINE DBINIT (LUN, IND, CATBLK, BLC, TRC, ISIZE, RBUF, IERR)
C-----------------------------------------------------------------------
C! checks map window and initializes for map double buffer IO
C# IO-util Map-util
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
C   DBINIT will initialize a map file for double buffered read I/O.
C   It checks and resets the window parms if needed.
C   Inputs:
C      LUN     I      logical unit number to use for the file.
C      CATBLK  I      header block for map file.
C      ISIZE   I      the size of the map I/O buffer in AIPS bytes.
C      RBUF    R(ISIZE/2)   the map I/O buffer.
C   In/out:
C      BLC     R(7)   bottom left hand corner of map   0's => 1's.
C      TRC     R(7)   top right hand corner of map.    0's => max.
C   Outputs:
C      IND     I      the index into the FTAB.
C      IERR    I      the error code from MINIT. 0 = ok.
C-----------------------------------------------------------------------
      INTEGER   LUN, IND, CATBLK(256), ISIZE, IERR
      REAL      BLC(7), TRC(7), RBUF(*)
C
      INTEGER   RBUFOF, IDEPTH(5), IWIN(4), ISCOLS, ISROWS
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       set window parms
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
      IWIN(1) = BLC(1) + 0.01
      IWIN(2) = BLC(2) + 0.01
      IWIN(3) = TRC(1) + 0.01
      IWIN(4) = TRC(2) + 0.01
      ISROWS = CATBLK(KINAX+1)
      ISCOLS = CATBLK(KINAX)
C                                       get offset in file
      IDEPTH(1) = BLC(3) + 0.01
      IDEPTH(2) = BLC(4) + 0.01
      IDEPTH(3) = BLC(5) + 0.01
      IDEPTH(4) = BLC(6) + 0.01
      IDEPTH(5) = BLC(7) + 0.01
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH, RBUFOF, IERR)
      IF (IERR.NE.0) GO TO 999
      RBUFOF = RBUFOF + 1
C                                       Init for double buff read.
      CALL MINIT ('READ', LUN, IND, ISCOLS, ISROWS, IWIN, RBUF, ISIZE,
     *   RBUFOF, IERR)
C
 999  RETURN
      END
