C                                                          Include DTCIO
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2002
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
      INCLUDE 'INCS:PTESS.INC'
C
      CHARACTER ACCTYP(MATFLD)*4, VMFILE(MATFLD)*48
      REAL      BUFFR1(XBUFSZ), BUFFR2(XBUFSZ), BUFFR3(XBUFSZ),
     *   BUFFR4(XBUFSZ), BUFFR5(XBUFSZ), BUFFR6(XBUFSZ)
      INTEGER   VMVOL(MATFLD), VMBO(MATFLD), VMSZ(2,MATFLD),
     *   BUFNUM(MATFLD), BIND(6), FIND(6),  BLC(2), TRC(2), NX, NY, HNX,
     *   HNY, XBEG, XEND, YBEG, YEND, BUFSZ(6)
      COMMON /TCIO/ VMVOL, VMBO, VMSZ, BUFNUM, BIND, FIND,  BLC, TRC,
     *   NX, NY, HNX, HNY, XBEG, XEND, YBEG, YEND, BUFSZ
      COMMON /TCIOBF/ BUFFR1, BUFFR2, BUFFR3, BUFFR4, BUFFR5, BUFFR6
      COMMON /TCIOCH/ ACCTYP, VMFILE
C                                                          End DTCIO
