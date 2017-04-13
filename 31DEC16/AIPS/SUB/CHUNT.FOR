      SUBROUTINE CHUNT (K, LL, NT, ISTART, KHUNT)
C-----------------------------------------------------------------------
C! searches symbol table for character string accepting min match
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
C   CHUNT searches a linked list usually the symbol table
C   It goes by characters and will take min match if MODE not 69
C   Inputs:
C      K       I(*)  contains linked tables
C      NT      I     number of characters to match
C      ISTART  I     offset within each table entry for matching
C      KHUNT   H(*)  string to match
C   In/Outputs:
C      LL      I     in : start address in K for tables
C                    out: pointer to entry found (0 if none)
C-----------------------------------------------------------------------
      INTEGER   K(*), LL, NT, ISTART
      HOLLERITH KHUNT(*)
C
      CHARACTER PRMSG*28
      INTEGER   IMSP, IMSTK(20), LENGTH, J, I
      LOGICAL   EQUAL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DPOP.INC'
      DATA PRMSG /'SYMBOL NOT UNIQUE COULD BE  '/
C-----------------------------------------------------------------------
      IMSP = 0
      IF (NT.LE.0) LL = 0
C                                        zero link -> none
 10   IF (LL.EQ.0) GO TO 900
C                                        compare
         LENGTH = K(LL+1)/16
         IF (LENGTH.LT.NT) GO TO 30
            J = LL + ISTART
            CALL CHCOMP (NT, 1, K(J), 1, KHUNT, EQUAL)
C                                       got something
            IF (.NOT.EQUAL) GO TO 30
C                                       min match applies
               IF (LENGTH.EQ.NT) GO TO 999
                  IF (IMSP.GE.20) GO TO 30
                     IMSP = IMSP + 1
                     IMSTK(IMSP) = LL
C                                       keep looking
 30      LL = K(LL)
         GO TO 10
C                                       what now: any match?
 900  IF ((IMSP.LE.0) .OR. (MODE.EQ.69)) GO TO 999
C                                       unique min match
      IF (IMSP.GT.1) GO TO 910
         LL = IMSTK(1)
         GO TO 999
C                                       not unique: error messages
 910  DO 920 I = 1,IMSP
         J = IMSTK(I)
         LENGTH = K(J+1)/16
         MSGTXT = PRMSG
         J = J + ISTART
         CALL H2CHR (LENGTH, 1, K(J), MSGTXT(29:))
         CALL MSGWRT (1)
 920     CONTINUE
C
 999  RETURN
      END
