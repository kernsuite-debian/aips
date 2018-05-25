C                                                    Include IMGERR.INC
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
C                                       PARAMETER include for use with
C                                       IMGERR
C                                       FDVEC pointers:
      INTEGER   FDDISK, FDCNO, FDLUNI, FDLUNE, FDBLC, FDTRC, FDSZIB,
     *   FDSZEB, FDFTI, FDFTE, FDERR, FDPOS, FDNLEF, FDRW, FDISER,
     *   FDIMSI, FDIMPT, FDERPL, FDCCAT, FDMAX, FDMIN, FDBLK
C                                       FDDISK = disk number
      PARAMETER (FDDISK = 1)
C                                       FDCNO = catalog slot number
      PARAMETER (FDCNO = 2)
C                                       FDLUNI = image LUN
      PARAMETER (FDLUNI = 3)
C                                       FDLUNE = error LUN
      PARAMETER (FDLUNE = 4)
C                                       FDBLC = BLC
      PARAMETER (FDBLC = 5)
C                                       FDTRC = TRC
      PARAMETER (FDTRC = 12)
C                                       FDSZIB = Size of IBUFF
      PARAMETER (FDSZIB = 19)
C                                       FDSZEB = Size of EBUFF
      PARAMETER (FDSZEB = 20)
C                                       FDFTI = FTAB pointer for image
      PARAMETER (FDFTI = 30)
C                                       FDFTE = FTAB pointer for errors
      PARAMETER (FDFTE = 31)
C                                       FDERR = subroutine error code
      PARAMETER (FDERR = 32)
C                                       FDPOS = current position vector
      PARAMETER (FDPOS = 33)
C                                       FDNLEF = number of rows left
      PARAMETER (FDNLEF = 40)
C                                       FDRW = read/write flag
      PARAMETER (FDRW = 41)
C                                       FDISER = 0 if error plane
C                                       exists.
      PARAMETER (FDISER = 42)
C                                       FDIMSI = Image size
      PARAMETER (FDIMSI = 43)
C                                       FDIMPT = Current position
C                                       pointer
      PARAMETER (FDIMPT = 50)
C                                       FDERPL = order number of error
C                                       plane.
      PARAMETER (FDERPL = 57)
C                                       FDCCAT = 1 if OK to clear status
      PARAMETER (FDCCAT = 58)
C                                       FDMAX = Image max as REAL
      PARAMETER (FDMAX = 59)
C                                       FDMIN = Image min as REAL
      PARAMETER (FDMIN = 60)
C                                       FDBLK = 1 if blanked else 0
      PARAMETER (FDBLK = 61)
C                                       End of IMGERR.INC
