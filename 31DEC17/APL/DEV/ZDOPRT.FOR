      SUBROUTINE ZDOPRT (IVOL, LUN, NCOPY, PNAME, ISIZE, INBLK, IERR)
C-----------------------------------------------------------------------
C! reads bit file and causes it to be plotted on printer/plotter
C# Printer Plot-util
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
C   Read a bit map such as produced by PRTDRW and convert it into a
C   file that can be spooled to a Versatec printer/plotter.
C   Inputs:
C      IVOL     I      Disk volume containing file, 1,2,3,...
C      LUN      I      Logical unit number
C      NCOPY    I      Number of copies of the plot to make
C      PNAME    C*48   Physical file name (left justified)
C      ISIZE    I      Size of INBLK in words
C   In/Out:
C      INBLK    I(*)   Scratch buffer
C   Output:
C      IERR     I      Error return code: 0 => no error
C                         1 => error
C   Generic version - a stub
C-----------------------------------------------------------------------
      INTEGER   IVOL, LUN, NCOPY, ISIZE, INBLK(*), IERR
      CHARACTER PNAME*48
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Stubbed pending local
C                                       development.
      IERR = 1
      WRITE (MSGTXT,1000)
      CALL MSGWRT (8)
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZDOPRT: ROUTINE STUBBED PENDING LOCAL DEVELOPMENT')
      END
