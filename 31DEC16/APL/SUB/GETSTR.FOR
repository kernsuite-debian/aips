      SUBROUTINE GETSTR (KB, KBPLIM, NMAX, KBP, ISTR, NCHAR)
C-----------------------------------------------------------------------
C! returns a string value (was enclosed by quotes) from character buffer
C# Parsing FITS POPS-util
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
C   GETSTR obtains a character value from a buffer
C   Inputs:
C      KB      C*80     character buffer
C      KBPLIM  I        size of buffer
C      NMAX    I        max string length in characters
C      KBP     I        start position in KB
C   Outputs:
C      KBP     I        start position in KB next field
C      ISTR    C*(*)    packed string, blank filled
C      NCHAR   I        # characters (0 => no string found)
C-----------------------------------------------------------------------
      CHARACTER KB*80, ISTR*(*)
      INTEGER   NMAX, KBPLIM, KBP, NCHAR
C
      INTEGER   JB, JJ, LMAX
      INCLUDE 'INCS:DERR.INC'
C-----------------------------------------------------------------------
      NCHAR = 0
      LMAX = MIN (NMAX, LEN (ISTR))
      ISTR = ' '
      IF (KBP.GT.KBPLIM) GO TO 999
C                                        skip leading blanks
 10   IF (KB(KBP:KBP).EQ.'''') GO TO 15
         KBP = KBP + 1
         IF (KBP.GT.KBPLIM) GO TO 999
         GO TO 10
C                                        find end
 15   JB = KBP + 1
 20   KBP = KBP + 1
         IF (KBP.GT.KBPLIM) GO TO 25
         IF (KB(KBP:KBP).EQ.'''') GO TO 25
         GO TO 20
C                                        got it
 25   NCHAR = KBP - JB
      KBP = KBP + 1
      JJ = MIN (NCHAR, LMAX)
      IF (NCHAR.NE.0) ISTR(1:JJ) = KB(JB:JB+JJ-1)
      IF (NCHAR.GT.LMAX) GO TO 980
C                                        make null string ok
      IF (NCHAR.EQ.0) NCHAR = 1
      GO TO 999
C                                        error: too long
 980  ERRNUM = 13
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.10) PNAME(ERRLEV) = 'GETSTR'
C
 999  RETURN
      END
