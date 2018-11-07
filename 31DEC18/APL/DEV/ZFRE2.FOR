      SUBROUTINE ZFRE2 (NVOL, VOLNAM, VLEN, TOTBLK, PCTFUL, FRBLKS)
C-----------------------------------------------------------------------
C! return AIPS data disk free space information
C# Z2 IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004, 2009
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
C   Get AIPS disk free space information.
C   Inputs:
C      NVOL     I      Number of disks
C   Output:
C      VOLNAM   H(*)   Volume names
C      VLEN     I(*)   Length in characters per volume name
C      TOTBLK   I(*)   Total number of Mbytes on device
C      PCTFUL   R(*)   Percentage of device full
C      FRBLKS   I(*)   Number of free Mbytes remaining
C   Generic (stubbed) version
C-----------------------------------------------------------------------
      INTEGER   NVOL, VLEN, TOTBLK(*), FRBLKS(*)
      HOLLERITH VOLNAM(*)
      REAL      PCTFUL(*)
C
      INTEGER   I
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'ZFRE2: REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (9)
      I = NVOL * VLEN
      CALL CHR2H (I, ' ', 1, VOLNAM)
      DO 10 I = 1,NVOL
         TOTBLK(I) = 0
         PCTFUL(I) = 100.0
         FRBLKS(I) = 0
 10      CONTINUE
C
 999  RETURN
      END
