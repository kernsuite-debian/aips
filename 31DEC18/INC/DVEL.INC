C                                                          Include DVEL
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
C                                       CVEL information
      REAL       NAMEIN(3), CLAIN(2), XSIN, XDISIN,
     *   NAMOUT(3), CLAOUT(2), XSOUT, XDSOUT,
     *   XSOUR(4,30), XTIME(8), XFLAG,  XDOBND, XBPVER,
     *   APARM(10), XBADD(10)
      INTEGER   SEQIN, CNOIN, DISKIN, SEQOUT, CNOOUT, DISOUT
C                                       Program control
      DOUBLE PRECISION
     *       FOFF(MAXIF), RSTFRQ(MAXIF), VEL(MAXIF),
     *   REFFRQ(MAXIF), TBW
      REAL      IATUT, BUFFER(8192), SHFSRC(4,30)
      INTEGER   APBEG, NCMPLX, APTYPE
      INTEGER   CVLSOU, OLDSOU, ISBAND(MAXIF),
     *   IYEAR, IRDAY, NFREQ, ISLUN, NCSOU, CSUWAN(30),
     *   CSUWTB(30), NXTTWO
      LOGICAL   HELIO, POWRTO, PRIME, RADIO, SINGLE, DOCSOU,
     *   USEAP
C                                                          End DVEL
C                                                          Include CVEL
C                                       CVEL information
      COMMON /INPARM/ NAMEIN, CLAIN, XSIN, XDISIN, NAMOUT,
     *   CLAOUT, XSOUT, XDSOUT, XSOUR, XTIME, XFLAG,
     *   XDOBND, XBPVER, APARM, XBADD,
     *   SEQIN, CNOIN, DISKIN, SEQOUT, CNOOUT, DISOUT
C                                       Program control
      COMMON /CVELCM/ FOFF, RSTFRQ, VEL, REFFRQ, TBW,
     *   IATUT, BUFFER, SHFSRC,
     *   CVLSOU, OLDSOU, ISBAND, IYEAR, IRDAY,
     *   NFREQ, ISLUN, NCSOU, CSUWAN, CSUWTB, NXTTWO,
     *   HELIO, POWRTO, PRIME, RADIO, SINGLE, DOCSOU,
     *   USEAP
C                                       AP Control
      COMMON /CVELAP/ APBEG, NCMPLX, APTYPE
C                                                          End CVEL
