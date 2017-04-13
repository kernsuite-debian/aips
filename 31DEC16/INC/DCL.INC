C                                                          Include DCL.
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
      INTEGER   NTANT(30), NTSYS(30), IGAIN(30), ISTART, NSKPDY,
     *          NSTEPS, NDAYS(20), SKIPDY(20)
      REAL      GACOEF(6,30), GAOFFS(30), DPFU(30),
     *          TANTS(2,30,200), TSYS(2,1500),
     *          BFAC(30,30), TBOUND(2,30), FLUX, BDEF
      DOUBLE PRECISION
     *      CALSTN(30)
      COMMON /CALIB/ CALSTN, GACOEF, GAOFFS, DPFU, TANTS, TSYS, BFAC,
     *               TBOUND, FLUX, BDEF, NTANT, NTSYS, IGAIN, ISTART,
     *               SKIPDY, NDAYS, NSKPDY, NSTEPS
C                                                          End DCL.
