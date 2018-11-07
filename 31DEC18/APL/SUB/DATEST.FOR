      SUBROUTINE DATEST (FDATE, ID)
C-----------------------------------------------------------------------
C! converts 'DD/MM/YY' or 'YYYYMMDD' to date numbers
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1997
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
C   Convert ALPHA forms of date to integers
C   Inputs:
C      FDATE  C*8    FITS packed string: 'DD/MM/YY' or 'YYYYMMDD'
C   Output:
C      ID     I(3)   ID(1)   year since 0.
C                    ID(2)   month (1-12).
C                    ID(3)   day   (1-31).
C-----------------------------------------------------------------------
      CHARACTER FDATE*(*)
      INTEGER   ID(3)
C
      INTEGER   IDATE(3)
C-----------------------------------------------------------------------
C                                       Get date: old format
      IF (FDATE(3:3).EQ.'/') THEN
         READ (FDATE,1000,ERR=10) IDATE
         ID(1) = IDATE(3)
         ID(2) = IDATE(2)
         ID(3) = IDATE(1)
         IF (ID(1).LE.20) ID(1) = ID(1) + 100
         ID(1) = ID(1) + 1900
C                                       get date new format
      ELSE
         READ (FDATE,1010,ERR=10) ID
         END IF
      GO TO 999
C                                       bad string
 10   CALL FILL (3, -1, ID)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I2,1X,I2,1X,I2)
 1010 FORMAT (I4,I2,I2)
      END
