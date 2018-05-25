C                                                          Include DGPH.
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002-2003, 2008
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
      INCLUDE 'INCS:PMAD.INC'
C                                       Plotting common
      INTEGER   GPHSIZ, GPHLUN, GPHIND, GPHPOS, GPHRRN, GPHVOL,
     *   GPHTLO, GPHTHI
      REAL      GPHX1, GPHX2, GPHY1, GPHY2, SCALEF, GPHRNG(2,3)
      CHARACTER GPHNAM*48, TVSUBR*6, GPHFUN*2
C
      LOGICAL   GPHDOT, GPHPAG, GPHDOD
      INTEGER   GPHCRN(2), GPHTVC(3), GPHTVG(8), GPHIX0, GPHIX1, GPHIX2,
     *   GPHIXL, GPHIY0, GPHIY1, GPHIY2, GPHIYL, GPHTVW(4), GPHCHL,
     *   GPHCHB, GPHCHR, GPHCHT, GPHNGR, GPHCAT(256), GPHSCR(MAXIMG),
     *   GPHERO, GPHERG, GPHERC, GPHERV, GPHPLX(2), GPHPLY(2), GPHCNT,
     *   GPHLTY, GPHCON(32), GPHCOL(3)
      REAL      GPHSCX, GPHSCY, GPHRX1, GPHRX2, GPHRY1, GPHRY2, GPHCUT
C
      COMMON /GPHCOM/ GPHX1, GPHX2, GPHY1, GPHY2, SCALEF, GPHSCX,
     *   GPHSCY, GPHRX1, GPHRX2, GPHRY1, GPHRY2, GPHDOT, GPHPAG,
     *   GPHSIZ, GPHLUN, GPHIND, GPHPOS, GPHRRN, GPHVOL, GPHTLO,
     *   GPHTHI, GPHCRN, GPHTVC, GPHTVG, GPHIX0, GPHIX1, GPHIX2,
     *   GPHIXL, GPHIY0, GPHIY1, GPHIY2, GPHIYL, GPHTVW, GPHCHL,
     *   GPHCHB, GPHCHR, GPHCHT, GPHNGR, GPHCAT, GPHSCR, GPHERO,
     *   GPHERG, GPHERC, GPHERV, GPHPLX, GPHPLY, GPHCNT, GPHLTY,
     *   GPHCON, GPHDOD, GPHRNG, GPHCUT, GPHCOL
      COMMON /GPHCHR/ GPHNAM, TVSUBR, GPHFUN
C                                                          End DGPH.
