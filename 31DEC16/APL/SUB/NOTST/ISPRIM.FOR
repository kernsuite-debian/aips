      SUBROUTINE ISPRIM (N, PRIME, NXTTWO)
C-----------------------------------------------------------------------
C! Is a number a prime number?
C# General
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
C  Routine to determine if a number is a prime number and if
C  so what is the next higher power of two.
C-----------------------------------------------------------------------
      INTEGER   IFACT(32), N, IF, NPART, ID, IDIV, IQUOT,
     *   NFACT, NXTTWO, GAMMA
      LOGICAL   PRIME
C-----------------------------------------------------------------------
      PRIME = .FALSE.
      NXTTWO = 0
C                                       factor n into its prime
C                                       factors, nfact in number.
C                                       for example, for n = 1960,
C                                       nfact = 6 and ifact(if) = 2,
C                                       2, 2, 5, 7 and 7.
      IF = 0
      NPART = N
      DO 50 ID = 1, N, 2
         IDIV = ID
         IF (ID-1) 10,10,20
 10      IDIV = 2
 20      IQUOT = NPART / IDIV
         IF (NPART - IDIV * IQUOT) 40,30,40
 30      IF = IF + 1
         IFACT(IF) = IDIV
         NPART = IQUOT
         GO TO 20
 40      IF (IQUOT-IDIV) 60,60,50
 50      CONTINUE
 60   IF (NPART-1) 80,80,70
 70   IF = IF + 1
      IFACT(IF) = NPART
 80   NFACT = IF
C                                       Prime?
      IF (NFACT .GT. 1) GO TO 999
      PRIME = .TRUE.
C                                       Find next power of two
      DO 100 GAMMA = 1, 15
         NXTTWO = 2**GAMMA
         IF (NXTTWO .GT. N) GO TO 999
 100     CONTINUE
C
 999  RETURN
      END
