C                                                    Include ZPBUFSZ
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2008, 2010-2012
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
C                                       Parameters for buffer sizes
C                                       in words.
C                                       Generic version
C                                       Uv data buffers.
      INTEGER   UVBFSS, UVBFSL
C                                       UVBFSS = small uv data buffer,
C                                       3 complex * MAXCIF + extra for
C                                       random parm & UVDISK overhead.
      PARAMETER (UVBFSS = 9*132096+24*256)
C                                       UVBFSL = large uv data buffer
      PARAMETER (UVBFSL = 36*132096+24*256)
C                                                        End ZPBUFSZ
