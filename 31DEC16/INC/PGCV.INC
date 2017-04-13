C                                       Include for gain curve table
C                                       handling routines (GC).
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2015-2016
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
C                                       Requires INCS:PUVD.INC
      INTEGER MAXGCC, NKEYGC, NKYGC2, MXTBGC, MXSPGC, MXDPGC
C                                       #cols in GC table
      PARAMETER (MAXGCC = 19)
C                                       #keywords in GC table
      PARAMETER (NKEYGC = 11)
C                                       2 x #keywords
      PARAMETER (NKYGC2 = 2 * NKEYGC)
C                                       Max. no. tabulated val. per row
      PARAMETER (MXTBGC = 200)
C                                       Logical record length in
C                                       single precision words
      PARAMETER (MXSPGC = 3 + 12*MAXIF + 4*MAXIF*MXTBGC)
C                                       in double precision words.
      PARAMETER (MXDPGC = MXSPGC/2)
C                                       Current GC revision no.
      INTEGER IGCREV
      PARAMETER (IGCREV = 2)
C                                       Parameters specifying the
C                                       position of variables within
C                                       a table row. Third character
C                                       gives var. type (D=double;
C                                       R=real;I=integer)
      INTEGER GCIANT, GCISUB, GCIFQD, GCITP1, GCINT1, GCIXT1, GCIYT1,
     *   GCRXV1, GCRYV1, GCRGA1, GCRSE1, GCITP2, GCINT2, GCIXT2, GCIYT2,
     *   GCRXV2, GCRYV2, GCRGA2, GCRSE2
C                                       GCIANT = Antenna no.
      PARAMETER (GCIANT = 1)
C                                       GCISUB = Subarray
      PARAMETER (GCISUB = GCIANT + 1)
C                                       GCIFQD = Freq. ID
      PARAMETER (GCIFQD = GCISUB + 1)
C                                       GCITP1 = Curve type 1
      PARAMETER (GCITP1 = GCIFQD + 1)
C                                       GCINT1 = No. terms 1
      PARAMETER (GCINT1 = GCITP1 + 1)
C                                       GCIXT1 = X-variable type 1
      PARAMETER (GCIXT1 = GCINT1 + 1)
C                                       GCIYT1 = Y-variable type 1
      PARAMETER (GCIYT1 = GCIXT1 + 1)
C                                       GCRXV1 = X-value 1
      PARAMETER (GCRXV1 = GCIYT1 + 1)
C                                       GCRYV1 = Y-values 1
      PARAMETER (GCRYV1 = GCRXV1 + 1)
C                                       GCRGA1 = Gain values 1
      PARAMETER (GCRGA1 = GCRYV1 + 1)
C                                       GCRSE1 = Sensitivity 1
      PARAMETER (GCRSE1 = GCRGA1 + 1)
C                                       GCITP2 = Curve type 2
      PARAMETER (GCITP2 = GCRSE1 + 1)
C                                       GCINT2 = No. terms 2
      PARAMETER (GCINT2 = GCITP2 + 1)
C                                       GCIXT2 = X-variable type 2
      PARAMETER (GCIXT2 = GCINT2 + 1)
C                                       GCIYT2 = Y-variable type 2
      PARAMETER (GCIYT2 = GCIXT2 + 1)
C                                       GCRXV2 = X-value 2
      PARAMETER (GCRXV2 = GCIYT2 + 1)
C                                       GCRYV2 = Y-values 2
      PARAMETER (GCRYV2 = GCRXV2 + 1)
C                                       GCRGA2 = Gain values 2
      PARAMETER (GCRGA2 = GCRYV2 + 1)
C                                       GCRSE2 = Sensitivity 2
      PARAMETER (GCRSE2 = GCRGA2 + 1)
C                                       End Include


