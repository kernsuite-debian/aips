C                                                          Include DSDD
C-----------------------------------------------------------------------
C;  Copyright (C) 1996, 1999
C;  Associated Universities, Inc. Washington DC, USA.
C;  Eric W. Greisen
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
C                                       SDD format pointers
      INTEGER   SDLEN, SSCAN, SOBSI, STELE, SOBJE, SOBSM, SFRNT, SUTDA,
     *   SSAMP, SEPOC, SXSOU, SYSOU, SOFFS, SVELD, SGAIN, SOBSF, SRSTF,
     *   SFRQR, STSYS, SREFP, SX0, SINTT, SNOIN, SFRAM, SPBEM, SMBEM,
     *   SETAA, SETAF, SRTSY, SAPPE
      COMMON /SDPTRS/ SDLEN, SSCAN, SOBSI, STELE, SOBJE, SOBSM, SFRNT,
     *   SUTDA, SSAMP, SEPOC, SXSOU, SYSOU, SOFFS, SVELD, SGAIN, SOBSF,
     *   SRSTF, SFRQR, STSYS, SREFP, SX0, SINTT, SNOIN, SFRAM, SPBEM,
     *   SMBEM, SETAA, SETAF, SRTSY, SAPPE
C     PARAMETER (SDLEN = 6, SSCAN = 7, SOBSI = 8, STELE = 11,
C    *   SOBJE = 13, SOBSM = 15, SFRNT = 16, SUTDA = 32, SSAMP = 39,
C    *   SEPOC = 41, SXSOU = 42, SYSOU = 43, SOFFS = 76, SVELD = 80,
C    *   SGAIN = 100, SOBSF = 158, SRSTF = 159, SFRQR = 160,
C    *   STSYS = 164,  SREFP = 168, SX0 = 169, SINTT = 171, SNOIN = 172,
C    *   SFRAM = 74, SPBEM = 101, SMBEM = 103, SETAA = 85, SETAF = 86,
C    *   SRTSY = 165, SAPPE = 82)
C                                       end DSDD.INC
