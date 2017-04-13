      SUBROUTINE FILSWP (FIL1, FIL2)
C-----------------------------------------------------------------------
C! Tim Cornwell routine: switch file info
C# IO-util
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
C   Swap the data in the VM file info arrays between FIL1 and FIL2,
C   effectively swapping the files.  Note that the value of FIL1
C   and FIL2 do not change.
C   Inputs:
C      FIL1    I      Index to first file info
C      FIL2    I      Index to second file info
C   In/out Common DTESS.INC :
C      SCRNM   I(*)   Scratch file common index ? - swapped
C   In/out Common DTCIO.INC :
C      VMVOL   I(*)   File disk volume number - swapped
C      VMFILE  C*(*)  File name - swapped
C      VMBO    I(*)   File disk block offset - swapped
C   Programmer =  T.J. Cornwell      December 1987
C-----------------------------------------------------------------------
      INTEGER    FIL1, FIL2
C
      INTEGER    ITEMP
      CHARACTER  CHTEMP*48
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
C-----------------------------------------------------------------------
      IERR = 0
      ITEMP = VMVOL(FIL1)
      VMVOL(FIL1) = VMVOL(FIL2)
      VMVOL(FIL2) = ITEMP
C
      CHTEMP = VMFILE(FIL1)
      VMFILE(FIL1) = VMFILE(FIL2)
      VMFILE(FIL2) = CHTEMP
C
      ITEMP = SCRNM(FIL1)
      SCRNM(FIL1) = SCRNM(FIL2)
      SCRNM(FIL2) = ITEMP
C
      ITEMP = VMBO(FIL1)
      VMBO(FIL1) = VMBO(FIL2)
      VMBO(FIL2) = ITEMP
C
 999  RETURN
      END
