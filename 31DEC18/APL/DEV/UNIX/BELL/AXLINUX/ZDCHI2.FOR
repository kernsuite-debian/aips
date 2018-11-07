      SUBROUTINE ZDCHI2
C-----------------------------------------------------------------------
C! initialize device and Z-routine characteristics commons - local vals
C# Z2 System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   Initialize the device characteristics common - ZDCHIN does the
C   generic ones and ZDCHI2 does the few peculiar ones.
C
C   Linux on AXP (Alpha) systems
C-----------------------------------------------------------------------
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
C-----------------------------------------------------------------------
C                                       Byte flip indicator code:
C                                          1 => bytes flipped
C                                          2 => words flipped (I*4 only)
C                                          3 => both
C                                          (3 for VAXes, 0 almost
C                                          everywhere else).
      BYTFLP = 3
C                                       Floating-point format code
C                                          0 => OTHER
C                                          1 => IEEE
C                                          2 => VAX F
C                                          3 => VAX G
C                                          4 => IBM (not supported yet)
C                                       Single precision
      SPFRMT = 1
C                                       Double precision
      DPFRMT = 1
C                                       # iterations considered a short
C                                       loop (if less than this, branch
C                                       to scalar code).
      NSHORT = 3
C                                       1 => TTY i/o uses carriage cntrl
C                                       0 => TTY writes with no carriage
C                                            control character
      TTYCAR = 0
C                                       System type ('UNIX', 'VMS')
      SYSTYP = 'ALNX'
C                                       System version ('4.5','BSD 4.2')
      SYSVER = 'Linux 2.0.34'
C
 999  RETURN
      END
