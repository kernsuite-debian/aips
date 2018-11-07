      SUBROUTINE ALSKEY (ALIST, KEYWRD, KEYPNT, IERR)
C-----------------------------------------------------------------------
C! PRIVATE: Lookup keyword in value from Associative list (ALIST)
C# Utility 
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
C-----------------------------------------------------------------------
C   Private to ALIST manager
C   Lookup keyword in ALIST; called by ALSGET and ALSPUT.
C   See ALSINI for details of the package.
C   Inputs:
C      ALIST   I(*)  ALIST
C      KEYWRD C*16   Keyword
C   Outputs:
C      KEYPNT  I     Address in ALIST array for keyword
C      IERR    I     Error code, 0=OK, 1=couldn't find
C-----------------------------------------------------------------------
      CHARACTER KEYWRD*(*)
      INTEGER   ALIST(*), KEYPNT, IERR
C
      INTEGER   ADDR, ITEST(4)
      HOLLERITH TEST(4)
      EQUIVALENCE (TEST, ITEST)
      INCLUDE 'INCS:PALIST.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Don't look if none exist.
      IF (ALIST(ALSEND).GT.1) THEN
C                                       Convert KEYWRD to HOLLERITH
         CALL CHR2H (16, KEYWRD, 1, TEST)
C                                       Find KEYWORD in ALIST, loop
C                                       until end of list.
         ADDR = ALIST(ALSBEG)
 50         KEYPNT = ADDR
            IF ((ITEST(1).EQ.ALIST(ADDR+ALSKWD))
     *         .AND. (ITEST(2).EQ.ALIST(ADDR+ALSKWD+1)).AND.
     *         (ITEST(3).EQ.ALIST(ADDR+ALSKWD+2)).AND.
     *         (ITEST(4).EQ.ALIST(ADDR+ALSKWD+3)))  GO TO 999 
C                                       End of list?
            ADDR = ALIST(ADDR)
            IF ((ADDR.GT.0) .AND. (ADDR.LT.ALIST(ALSIZE)) .AND.
     *         (ALIST(ADDR).GT.ADDR)) GO TO 50
            END IF
C                                       Not found
      IERR = 1
      GO TO 999
C
 999  RETURN
      END
