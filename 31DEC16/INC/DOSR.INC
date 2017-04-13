C                                                          Include DOSR.
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
      INTEGER   BUFFER(257), CATBLK(256), SEQIN, SEQOUT, DISKIN,
     *   DISOUT, SORT1, SORT2, OUTSRT, LENBU, LREC, NBUFF,
     *   NSORT, NVIS(2), IN, OUT, KEY1, KEY2
      REAL   NAMEIN(3), NAMOUT(3), CLAIN(2), CLAOUT(2),
     *   XSIN, XSOUT, XDISIN, XDISO, SORT, BADD(10), ROTATE
      DOUBLE PRECISION
     *   BUFSZ
      COMMON /BUFRS/ BUFSZ,IN,OUT,BUFFER
      COMMON /UVINF/ SEQIN,SEQOUT,DISKIN,DISOUT,SORT1,SORT2,
     *   NVIS,LENBU,LREC,NBUFF,NSORT,KEY1,KEY2,CATBLK
      COMMON /UVPARM/ NAMEIN, CLAIN, XSIN, XDISIN, NAMOUT, CLAOUT,
     *   XSOUT, XDISO, BADD, SORT, ROTATE
C                                                          End DOSR.
