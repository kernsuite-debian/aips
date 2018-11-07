      SUBROUTINE LTSTOR
C-----------------------------------------------------------------------
C! allocate storage for literal if needed, return pointer in any case
C# POPS-lang
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
C   LTSTOR searches the literal list.  If a matching literal is found,
C   the TAG is returned.  If not found, a new one is generated and
C   linked to the literal list.
C   Commons altered:
C      L         point to storage location
C      TAG       point to storage location as real variable
C      LOCSYM    point to storage location
C      KKT or K  area of literal storage
C   Note: uses KKT only if MODE = 0, does not check permanent area
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   IX(1), KKT(10)
      INTEGER   MK, N, NT, LLOCAT
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      EQUIVALENCE (X(1), IX(1)),    (KKT(1), K(51))
      DATA PRGNAM /'LTSTOR'/
C-----------------------------------------------------------------------
C                                       Literal look-up & define.
      NT = LX
      N = LX + 2
C                                       Temporary Literals.
      IF (MODE.NE.0) GO TO 15
         L = KKT(4)
C                                       Leave L pointing to the entry.
         CALL HUNT (KKT, L, NT, 2, IX)
         IF (L.NE.0) GO TO 10
            L = LLOCAT (N, KKT, KKT(10))
            IF (ERRNUM.NE.0) GO TO 980
C                                       Make L refer to the K array.
 10      L = L + KT - 1
         GO TO 20
C                                       Permanent literals.
 15   CONTINUE
         L = K(4)
         CALL HUNT (K, L, NT, 2, IX)
         IF (L.NE.0) GO TO 30
            L = LLOCAT (N, K, K(10))
            IF (ERRNUM.NE.0) GO TO 980
C                                       set in type, etc. in new one
 20      CONTINUE
            MK = L + 2
            K(L+1) = 16*NT + SYTYPE
            CALL COPY (NT, IX(1), K(MK))
 30   TAG = L + 2
      LOCSYM = L
      GO TO 999
C                                       Error return.
 980  ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
      END
