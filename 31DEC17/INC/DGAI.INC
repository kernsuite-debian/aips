C                                                          Include DGAI.
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
C                                                          for VBANT
      INTEGER   IFLG(4), BFLG(48), IREF, SPARGA(5),
     *          IGVER,LUNG,INDG,GLREC,GBP,GNREC,GRNO,
     *          NBIT,NWORD,IFCHN,
     *          GBUFF(1024),GDUM(512)
      INTEGER   KV
      REAL      T1, T2, GACAL(28,2,2), GMEAN
      DOUBLE PRECISION
     *      EIGHT
      COMPLEX   GAIN(28,2)
C                                                          for VBANT
      COMMON /GAPRM/ LUNG, INDG, GLREC, GBP, GNREC, IFCHN,
     *               GRNO, GBUFF
C                                                          for VBANT
      EQUIVALENCE  (GBUFF(255), GMEAN)
      EQUIVALENCE  (GDUM(1),  EIGHT)
      EQUIVALENCE  (GDUM(1),   GAIN),   (GDUM(225),  IFLG),
     *             (GDUM(229), BFLG),   (GDUM(277),    KV),
     *             (GDUM(279),   T1),   (GDUM(281),    T2),
     *             (GDUM(283), GACAL(1,1,1))
C                                                          End DGAI.
