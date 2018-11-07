C                                                          Include DUVH.
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2004, 2015
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
C                                       If you change this include you
C                                       must also change common
C                                       /CATHDR/ in DBCON
C                                       Include for uv header info
      INTEGER   NVIS, ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ,
     *   ILOCIT, ILOCID, ILOCSA, ILOCA1, ILOCA2, JLOCC, JLOCS, JLOCF,
     *   JLOCR, JLOCD, JLOCIF, NRPARM, LREC, NCOR, INCS, INCF, INCIF,
     *   ICOR0, TYPUVD
      REAL      DEQUIN
      CHARACTER SOURCE*8, ISORT*2
      DOUBLE PRECISION FREQ, RA, DEC
      COMMON /UVHDR/ FREQ, RA, DEC, NVIS, ILOCU, ILOCV, ILOCW, ILOCT,
     *   ILOCB, ILOCSU, ILOCFQ, ILOCIT, ILOCID, ILOCSA, ILOCA1, ILOCA2,
     *   JLOCC, JLOCS, JLOCF, JLOCR, JLOCD, JLOCIF, INCS, INCF, INCIF,
     *   ICOR0, NRPARM, LREC, NCOR, TYPUVD, DEQUIN
      COMMON /UVHCHR/ SOURCE, ISORT
C                                                          End DUVH.
