C                                                          Include DRNX
C-----------------------------------------------------------------------
C;  Copyright (C) 2007, 2010-2011, 2014-2016
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
C                                       for making NX file on fly
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXSUB, MAXSCN
      PARAMETER (MAXSUB=512)
C                                       nearly 3 hours of 1-sec scans
      PARAMETER (MAXSCN=10000)
C
      INTEGER   RNXKOL(MAXNXC), RNXNUM(MAXNXC), RNXBUF(512),
     *   RNXANT(MAXSUB), VSTART(MAXSUB), VEND(MAXSUB), SRC(MAXSUB),
     *   FQID(MAXSUB), RNXVIS, RNXRNO, NGAPS, RNXNOS(MAXSUB),
     *   RNXFIR(MAXSUB), RNXCUR(MAXSUB)
      REAL      STIME(MAXSUB), ETIME(MAXSUB), MAXGAP, MAXLEN, SGAPS,
     *   RNXEPS, RNXTSC(MAXSCN)
C
      COMMON /RNXCOM/ RNXBUF, STIME, ETIME, MAXGAP, MAXLEN, RNXKOL,
     *   RNXNUM, RNXANT, VSTART, VEND, SRC, FQID, RNXVIS, RNXRNO,
     *   SGAPS, NGAPS, RNXEPS, RNXNOS, RNXFIR, RNXCUR, RNXTSC
C                                                          End DRNX
