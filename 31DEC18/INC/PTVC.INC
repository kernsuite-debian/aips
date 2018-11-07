C                                                          Include PTVC.
C-----------------------------------------------------------------------
C;  Copyright (C) 2008
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
      INTEGER   TVMTVC, TVMGRC, TVMLUT, TVMLOU, TVMOFM
C                                       Max number grey channels
      PARAMETER (TVMTVC = 16)
C                                       Max number graphics channels
      PARAMETER (TVMGRC = 8)
C                                       Size of LUT function
C                                       = max int + 1 into LUT
      PARAMETER (TVMLUT = 8192)
C                                       Max intensity out of LUT + 1
C                                       controls what OFM must be
      PARAMETER (TVMLOU = 2046)
C                                       Size of OFM function
C                                       255 is max out of OFM
      PARAMETER (TVMOFM = TVMTVC * TVMLOU)
C                                                          End PTVC.
