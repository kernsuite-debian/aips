      SUBROUTINE DATDAT (FDATE, ADATE)
C-----------------------------------------------------------------------
C! converts 'DD/MM/YY' or 'YYYYMMDD' dates to 'dd-mmm-yyyy' for printing
C# Printer Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
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
C   Convert FITS ALPHA form of date to expanded alpha form for printing.
C   Inputs:
C      FDATE  C*8    FITS packed string: 'DD/MM/YY'
C   Output:
C      ADATE  C*12   expanded character string of form 'dd-MMM-yyyy'
C-----------------------------------------------------------------------
      CHARACTER FDATE*(*), ADATE*(*)
C
      CHARACTER MONTH(12)*3, BADAT*11
      INTEGER   IDATE(3)
      DATA MONTH /'JAN','FEB','MAR','APR','MAY','JUN','JUL',
     *   'AUG','SEP','OCT','NOV','DEC'/
      DATA BADAT /'BAD DATE '/
C-----------------------------------------------------------------------
C                                       Get date
      CALL DATEST (FDATE, IDATE)
C                                       Do date.
      IF ((IDATE(1).GE.1900) .AND. (IDATE(1).LE.2100) .AND.
     *   (IDATE(3).GT.0) .AND. (IDATE(3).LE.31) .AND. (IDATE(2).GT.0)
     *   .AND. (IDATE(2).LE.12)) THEN
         WRITE (ADATE,1000) IDATE(3), MONTH(IDATE(2)), IDATE(1)
C                                       Bad input string
      ELSE
         ADATE = BADAT
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I2.2,'-',A3,'-',I4)
      END
