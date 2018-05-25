       SUBROUTINE POWER2 (WORDS, P2SIZE)
C-----------------------------------------------------------------------
C! returns largest power of 2 not exceeding first argument
C# AP-fft utility
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
C   POWER2 determines the largest power of 2 not exceeding WORDS.
C   Input:
C      WORDS    I   Size not to exceed
C   Output:
C      P2SIZE   I   Largest power of 2 not exceeding WORDS
C-----------------------------------------------------------------------
       INTEGER   WORDS, P2SIZE, I, SIZE
C-----------------------------------------------------------------------
       SIZE = 1
       DO 10 I = 1,22
          IF ((SIZE*2).GT.WORDS) GO TO 20
             SIZE = SIZE * 2
 10       CONTINUE
 20   P2SIZE = SIZE
C
 999  RETURN
      END
