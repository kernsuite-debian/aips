C                                                          Include DSOU.
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2012
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
C                                       Include for source info common.
C                                       NOTE: uses PARAMETER in PUVD.INC
      INTEGER   IDSOUR, QUAL, SUFQID
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION FREQO(MAXIF), BANDW, RAEPO, DECEPO, EPOCH, RAAPP,
     *   DECAPP, RAOBS, DECOBS, LSRVEL(MAXIF), RESTFQ(MAXIF), PMRA,
     *   PMDEC
      CHARACTER SNAME*16, CALCOD*4, VELTYP*8, VELDEF*8
      COMMON /SRCDAT/ FREQO, BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA, PMDEC, FLUX, IDSOUR, QUAL,
     *   SUFQID
      COMMON /SCRCHR/ SNAME, CALCOD, VELTYP, VELDEF
C                                                          End DSOU.
