      SUBROUTINE CHWMAT (NPM, PS, IPT, NTS, TS, EQUAL)
C-----------------------------------------------------------------------
C! matches a pattern string having wild-card chars with a test string
C# Character Catalog
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
C   CHWMAT matches a pattern string containing "wild-card" characters
C   with a test string.  The wild cards '*' for any number and ? for
C   exactly 1 of any character are supported.
C   Inputs: NPM    I         Length of test string (not incl NTS-1
C                            characters)
C           PS     C*(*)     Pattern string
C           IPT    I(NPM)    Pattern array prepared by PSFORM
C           NTS    I         Start char position in TS for testing
C           TS     C*(*)     Test string
C   Output: EQUAL  L         T => they match
C-----------------------------------------------------------------------
      CHARACTER PS*(*), TS*(*)
      INTEGER   NPM, NTS, IPT(*)
      LOGICAL   EQUAL
C
      INTEGER   I, NPA, IP, IT, J, NX, LT, LP, NPX, ITRIM
C-----------------------------------------------------------------------
      IP = 1
      IT = NTS
      NPX = NPM + NTS - 1
      NPA = ITRIM (PS)
      IF (NPA.GT.NPM) NPA = NPM
C                                       LOOP over pattern
C                                       real characters, blanks
 10   IF (IPT(IP).LT.0) GO TO 20
         I = MAX (IPT(IP), 1)
         EQUAL = PS(IP:IP+I-1) .EQ. TS(IT:IT+I-1)
         IF (.NOT.EQUAL) GO TO 999
         IP = IP + I
         IT = IT + I
         IF (IT.GT.NPX) GO TO 970
         GO TO 10
C                                       any 1 character
 20   IF (IPT(IP).EQ.-2) GO TO 30
         IP = IP + 1
         IT = IT + 1
         IF (IT.GT.NPX) GO TO 970
         GO TO 10
C                                       any number of characters
 30   CONTINUE
C                                       any more real ones?
         LP = IP + 1
         DO 35 I = LP,NPA
            IF (IPT(I).GT.0) GO TO 40
 35         CONTINUE
         GO TO 970
C                                       Yes: find 1st blank or other
 40      DO 45 J = LP,NPA
            IF (IPT(J).GE.0) GO TO 50
 45         CONTINUE
C                                       search loop for reals
 50      IP = J
         I = IPT(I) + I - J
         NX = NPX - IT - I + 2
         IF (NX.LT.1) GO TO 980
         DO 55 J = 1,NX
            LT = IT + J - 1
            EQUAL = PS(IP:IP+I-1) .EQ. TS(LT:LT+I-1)
            IF (EQUAL) GO TO 60
 55         CONTINUE
         GO TO 980
C                                       Got one
 60      IT = LT + I
         IP = IP + I
         IF (IT.LE.NPX) GO TO 10
C                                       Answers
 970  EQUAL = .TRUE.
      GO TO 999
 980  EQUAL = .FALSE.
C
 999  RETURN
      END
