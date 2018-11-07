      SUBROUTINE PRMUPD
C-----------------------------------------------------------------------
C! Updates some parameters to represent true state of scratch file
C# UV Modeling AP-appl
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
C Routine to update a few crucial parameters to represent the true
C state of a scratch file in the model division routines, these are 
C changed since the scratch file (which should never be compressed),
C is larger then the input file in order to hold the FFT of the model
C alongside the data.
C-----------------------------------------------------------------------
      INTEGER IT1, IT2, IT3, IERR
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IT1 = LREC
      IT2 = NRPARM
      IT3 = CATBLK(KINAX)
      CATBLK(KINAX) = 3
      CALL UVPGET (IERR)
      CATBLK(KINAX) = IT3
      LREC = IT1
      NRPARM = IT2
      RETURN
      END
