      SUBROUTINE DATFST (OP, FDATE)
C-----------------------------------------------------------------------
C! converts 'DD/MM/YY' or 'YYYY-MM-DD' to 'YYYYMMDD' form
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1997-1998
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
C   Convert 2 FITS forms of date to internal form ('YYYYMMDD')
C   Inputs:
C      OP     C*?    'F2L' FITS -> local, 'L2F' local -> FITS
C   In/Out
C      FDATE  C*?    FITS packed string: 'DD/MM/YY' or 'YYYY-MM-DD'
C-----------------------------------------------------------------------
      CHARACTER OP*(*), FDATE*(*)
C
      INTEGER   IDATE(3)
      CHARACTER LDATE*10
C-----------------------------------------------------------------------
C                                       Get date: old format
      IF (OP(:3).EQ.'F2L') THEN
         LDATE = FDATE(:8)
         IF (FDATE(3:3).EQ.'/') THEN
            READ (FDATE,1000,ERR=10) IDATE
            IF (IDATE(3).LE.40) IDATE(3) = IDATE(3) + 100
            IDATE(3) = IDATE(3) + 1900
            WRITE (LDATE,1010) IDATE(3), IDATE(2), IDATE(1)
C                                       get date new format
         ELSE IF (FDATE(5:5).EQ.'-') THEN
            LDATE = FDATE(:4) // FDATE(6:7) // FDATE(9:10)
            END IF
C                                       Put date
      ELSE
C                                       when is it now?
         CALL ZDATE (IDATE)
C                                       Old FITS
         IF (IDATE(1).LE.1998) THEN
C                                       already old FITS
            IF (FDATE(3:3).EQ.'/') THEN
               LDATE = FDATE
C                                       new internal
            ELSE
               LDATE = FDATE(7:8) // '/' // FDATE(5:6) // '/' //
     *            FDATE(3:4)
               END IF
C                                       New FITS form
         ELSE
C                                       old FITS internal
            IF (FDATE(3:3).EQ.'/') THEN
               LDATE = '19' // FDATE(7:8) // '-' // FDATE(4:5) // '-' //
     *            FDATE(1:2)
C                                       new internal
            ELSE
               LDATE = FDATE(:4) // '-' // FDATE(5:6) // '-' //
     *            FDATE(7:8)
               END IF
            END IF
         END IF
C                                       and return
 10   FDATE = LDATE
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I2,1X,I2,1X,I2)
 1010 FORMAT (I4.4,I2.2,I2.2)
      END
