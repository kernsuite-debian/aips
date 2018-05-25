C                                                          Include DFRC.
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
      INTEGER   CATIN(256), SEQIN, SEQ2, SEQOUT, DISKIN, DISK2, DISOUT,
     *   CNOIN, CNOOUT, JBUFSZ, BUFFS(1024), NANT, CNOIN2, DRBLK(256),
     *   NFREQ, AVGFRQ, NIF, NVAL
      REAL   NAMEIN(3), CLAIN(2), NAMOUT(3), CLAOUT(2),
     *   NAME2(3), CLAS2(2), XD2, XS2,
     *   XDI, XDO, XSI, XSO, BPARM(10), TIMSMO(30),
     *   BUFF1(1600), BUFF2(1600)
      COMMON /CINFO/ CATIN, DRBLK,
     *   CNOIN, CNOOUT, NANT, NFREQ, NIF, NVAL, CNOIN2,
     *   AVGFRQ
      COMMON /BUFRS/ BUFF1, BUFF2, BUFFS, JBUFSZ
      COMMON /XINPUT/ NAMEIN, CLAIN, XSI, XDI,
     *   NAME2, CLAS2, XS2, XD2,
     *   NAMOUT, CLAOUT, XSO, XDO, BPARM, TIMSMO,
     *   DISKIN, DISK2, DISOUT, SEQIN, SEQ2, SEQOUT
C                                                          End DFRC.
