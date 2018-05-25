      SUBROUTINE SETDEF
C-----------------------------------------------------------------------
C! fills FITS reader area for table-file extensions with defaults
C# FITS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2007
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
C  This routine will set up the default values for a FITS extension file
C  of type 'TABLE'.
C  Common (output):
C     /THDR/   COMMON
C     /EXTHDR/   COMMON
C-----------------------------------------------------------------------
C
      INTEGER   I
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DBHD.INC'
C-----------------------------------------------------------------------
C                                       Fits extensions
      EXTNAM = ' '
      EXTVER = 1
      EXTLEV = 1
C                                       tables extensions
      ITYPE = 'UK'
      ITANKY = 0
      ITNCOL = 0
      DO 10 I = 1,60
         TBCOL(I) = 0
         TFCODE(I) = 0
         TNULL(I) = '********'
         TTYPE(I) = ' '
         TFORM(I) = ' '
         TUNIT(I) = ' '
         TSCAL(I) = 1.0D0
         TZERO(I) = 0.0D0
         TMATX(I) = .FALSE.
 10      CONTINUE
C                                       VLBA extensions
      NMATRX = 0
      MAXIS = 0
      DO 20 I = 1, 20
         MAXISI(I) = 0
         CTYPE(I) = ' '
         CDELT(I) = 0.0D0
         CRPIX(I) = 1.0D0
         CRVAL(I) = 0.0D0
         CROTA(I) = 0.0
 20      CONTINUE
C                                       misc
      DATOBS = ' '
      TELESC = ' '
      OBSRVR = ' '
      BUNIT = ' '
      OBJECT = ' '
      CEPOCH = 0
C
 999  RETURN
      END
