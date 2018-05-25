      SUBROUTINE ALSDEL (ALIST, KEYWRD, IERR)
C-----------------------------------------------------------------------
C! Delete Associative list (ALIST) entry
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
C   Public function.
C   Delete Keyword entry from an ALIST and recover the freed space.
C   See ALSINI for details of the package.
C   Inputs:
C      ALIST  I(*)  ALIST array
C      KEYWRD C*16  Keyword
C   Outputs:
C      IERR    I     Error code, 0=OK, 1=couldn't find
C-----------------------------------------------------------------------
      INTEGER   ALIST(*), IERR
      CHARACTER KEYWRD*(*)
C
      CHARACTER ALNAME*32
      HOLLERITH TEST(4)
      INTEGER   ADDR, NDEL, LOOP, INDEX, NEXT, LAST, ITEST(4)
      EQUIVALENCE (TEST, ITEST)
      INCLUDE 'INCS:PALIST.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Validity test
      IF (ALIST(ALSCHK).NE.ALSCVL) THEN
         MSGTXT = 'ALSDEL: DAMAGED OR UNINITIALIZED ALIST'
         IERR = 2
         GO TO 990
         END IF
C                                       Don't look if none exist.
      IF (ALIST(ALSEND).GT.1) THEN
C                                       Convert KEYWRD to HOLLERITH
         CALL CHR2H (16, KEYWRD, 1, TEST)
C                                       Find KEYWORD in ALIST, loop
C                                       until end of list.
         ADDR = ALIST(ALSBEG)
         LAST = ADDR
 50         IF ((ITEST(1).EQ.ALIST(ADDR+ALSKWD))
     *         .AND. (ITEST(2).EQ.ALIST(ADDR+ALSKWD+1)).AND.
     *         (ITEST(3).EQ.ALIST(ADDR+ALSKWD+2)).AND.
     *         (ITEST(4).EQ.ALIST(ADDR+ALSKWD+3)))  GO TO 100
C                                       Update index
            LAST = ADDR
            ADDR = ALIST(ADDR)
C                                       End of list?
            IF ((ADDR.GT.0) .AND. (ADDR.LT.ALIST(ALSIZE)) .AND.
     *         (ALIST(ADDR).GT.ADDR)) GO TO 50
C                                       If it get here then it was not
C                                       found
            IERR = 1
            GO TO 999
C                                       Found entry - compress list
C                                       Does anything follow? 
 100        IF (ALIST(ADDR).GT.0) THEN
C                                       Following links updated
               NDEL = ALIST(ADDR) - ADDR
               INDEX = ADDR
C                                       Loop over list updating links. 
 150           IF (ALIST(INDEX).GT.0) THEN
                  NEXT = ALIST(INDEX)
                  ALIST(INDEX) = ALIST(INDEX) - NDEL
                  INDEX = NEXT
                  GO TO 150
                  END IF
C                                       Shift ALIST
               INDEX = ADDR
               DO 200 LOOP = ADDR+NDEL, ALIST(ALSEND)
                  ALIST(INDEX) = ALIST(LOOP)
                  INDEX = INDEX + 1
 200              CONTINUE
C                                       Reset last word pointer
               ALIST(ALSEND) = INDEX - 1
               GO TO 999
            ELSE
C                                       No following entries, reset end
C                                       of list pointer.
               IF (LAST.GT.0) ALIST(LAST) = -1
               GO TO 999
C                                       End of ALIST compression
            END IF
C                                       End of valid entries
         END IF
C                                       Not found
      IERR = 1
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
      CALL H2CHR (32, 1, ALIST(ALSNAM), ALNAME)
      MSGTXT = 'PROBLEM DUMPING ALIST: ' // ALNAME
      CALL MSGWRT (6)
C
 999  RETURN
      END
