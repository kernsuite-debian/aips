C                                                          Include DANT.
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2009
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
C                                       NOTE: uses PARAMETER in PUVD.INC
C                                       Declarations for ANTINI
      INTEGER   ANKOLS(MAXANC), ANNUMV(MAXANC), NUMORB, NOPCAL,
     *   IANRNO, ANTNIF, ANFQID
      CHARACTER ANAME*8, TIMSYS*8, RDATE*8, XYZHAN*8, TFRAME*8
      REAL      POLRXY(2), UT1UTC, DATUTC
      DOUBLE PRECISION  ARRAYC(3), GSTIA0, DEGPDY, SAFREQ
C                                       Declarations for TABAN
      INTEGER   NOSTA, MNTSTA
      CHARACTER ANNAME*8, POLTYA*2, POLTYB*2
      REAL      STAXOF, DIAMAN, FWHMAN(MAXIF), POLAA, POLCA(2*MAXIF),
     *   POLAB, POLCB(2*MAXIF)
      DOUBLE PRECISION  STAXYZ(3), ORBPRM(6)
C                                       Commons
      COMMON /ANCOMN/ ARRAYC, GSTIA0, DEGPDY, SAFREQ, STAXYZ, ORBPRM,
     *   POLRXY, UT1UTC, DATUTC, STAXOF, DIAMAN, FWHMAN, POLAA, POLCA,
     *   POLAB, POLCB, ANKOLS, ANNUMV, NUMORB, NOPCAL, IANRNO, ANFQID,
     *   NOSTA, MNTSTA, ANTNIF
      COMMON /ANCOMC/ ANAME, TIMSYS, RDATE, ANNAME, XYZHAN, TFRAME,
     *   POLTYA, POLTYB
C                                                          End DANT.
