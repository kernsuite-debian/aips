      SUBROUTINE FNDEXT (ITYPE, IHBLK, IVER)
C-----------------------------------------------------------------------
C! returns latest version number of specified extension file type
C# EXT-util
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
C   FNDEXT will find the latest version number of a particular
C   extension file type.  If no type exists then zero is returned.
C   Inputs:
C      ITYPE   C*2     extension file type, example PL, HI.
C      IHBLK   I(256)  current catalog block.
C   Output:
C      IVER    I       latest version number of extension file.
C-----------------------------------------------------------------------
      CHARACTER ITYPE*2
      INTEGER   IHBLK(256), IVER
C
      CHARACTER TTYPE*2
      INTEGER   I, NEXTF
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IVER = 0
C                                       Find extension ITYPE.
      CALL  FXHDEX (IHBLK)
      NEXTF = KHEXT + KIEXTN - 1
      DO 20 I = KHEXT,NEXTF
         CALL H2CHR (2, 1, IHBLK(I), TTYPE)
         IF (TTYPE.EQ.ITYPE) THEN
            IVER = IHBLK(KIVER+I-KHEXT)
            GO TO 999
            END IF
 20      CONTINUE
C
 999  RETURN
      END
