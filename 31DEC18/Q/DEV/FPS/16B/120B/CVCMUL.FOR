      SUBROUTINE CVCMUL (A, I, B, C, J, N)
C-----------------------------------------------------------------------
C! FPS AP 120B load module
C# AP-appl
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
C   MICROCODE FOR FPS AP-120B ARRAY PROCESSOR
C   CVCMUL multiplies a scalar complex value times the complex
C   conjugate of a complex vector producing a real vector
C
C    CALLING SEQUENCE - CALL CVCMUL (A, I, B, C, J, N)
C       A = Source complex vector base address
C       I = Increment of A
C       B = Address of scalar (real part)
C       C = Destination real vector base address
C       J = Increment for C
C       N = Element count (number of reals)
C
C    C(K) = REAL(B) * A(K) + IMAG(B) * A(K+1)
C
C    Timing = 1.0 N + 3.0 microseconds
C    Should work on either fast or slow AP memory.
C    Programmer = W. D. Cotton, July 1980.
C-----------------------------------------------------------------------
      INTEGER*2 CODE(105), SLIST(16), A, I, B, C, J, N, J1, J2, J3,
     *   J4, J5, J6
      COMMON /SPARY/SLIST
      EQUIVALENCE (J1, SLIST( 1))
      EQUIVALENCE (J2, SLIST( 2))
      EQUIVALENCE (J3, SLIST( 3))
      EQUIVALENCE (J4, SLIST( 4))
      EQUIVALENCE (J5, SLIST( 5))
      EQUIVALENCE (J6, SLIST( 6))
      DATA CODE(1) /  26/
      DATA CODE(    2),CODE(    3),CODE(    4),CODE(    5)/
     *   Z4088,Z0000,Z0000,Z0030/
      DATA CODE(    6),CODE(    7),CODE(    8),CODE(    9)/
     *   Z310C,Z0000,Z0000,Z0000/
      DATA CODE(   10),CODE(   11),CODE(   12),CODE(   13)/
     *   Z0294,Z0000,Z0000,Z0010/
      DATA CODE(   14),CODE(   15),CODE(   16),CODE(   17)/
     *   Z4154,Z0000,Z4A04,Z0000/
      DATA CODE(   18),CODE(   19),CODE(   20),CODE(   21)/
     *   Z4000,Z01F2,Z0000,Z0030/
      DATA CODE(   22),CODE(   23),CODE(   24),CODE(   25)/
     *   Z0000,Z00E0,Z0000,Z0000/
      DATA CODE(   26),CODE(   27),CODE(   28),CODE(   29)/
     *   Z0000,Z0000,Z1A00,Z8000/
      DATA CODE(   30),CODE(   31),CODE(   32),CODE(   33)/
     *   Z0000,Z0000,Z0000,Z0010/
      DATA CODE(   34),CODE(   35),CODE(   36),CODE(   37)/
     *   Z0000,Z0000,Z0100,Z1700/
      DATA CODE(   38),CODE(   39),CODE(   40),CODE(   41)/
     *   Z0294,Z0000,Z0000,Z1F00/
      DATA CODE(   42),CODE(   43),CODE(   44),CODE(   45)/
     *   Z0000,Z01D2,Z0020,Z1B00/
      DATA CODE(   46),CODE(   47),CODE(   48),CODE(   49)/
     *   Z0003,ZE000,Z0000,Z0000/
      DATA CODE(   50),CODE(   51),CODE(   52),CODE(   53)/
     *   Z2040,Z0000,ZC005,Z1F30/
      DATA CODE(   54),CODE(   55),CODE(   56),CODE(   57)/
     *   Z0000,Z0000,Z0000,Z1F00/
      DATA CODE(   58),CODE(   59),CODE(   60),CODE(   61)/
     *   Z0001,Z9400,Z0140,Z0010/
      DATA CODE(   62),CODE(   63),CODE(   64),CODE(   65)/
     *   Z0295,Z8000,Z0100,Z1700/
      DATA CODE(   66),CODE(   67),CODE(   68),CODE(   69)/
     *   Z210C,Z01D2,Z0000,Z1F70/
      DATA CODE(   70),CODE(   71),CODE(   72),CODE(   73)/
     *   Z0000,Z0052,Z0000,Z0000/
      DATA CODE(   74),CODE(   75),CODE(   76),CODE(   77)/
     *   Z0000,Z004A,Z0020,Z1B00/
      DATA CODE(   78),CODE(   79),CODE(   80),CODE(   81)/
     *   Z0000,Z0000,Z0020,Z1B00/
      DATA CODE(   82),CODE(   83),CODE(   84),CODE(   85)/
     *   Z0000,Z0000,ZC005,Z1F00/
      DATA CODE(   86),CODE(   87),CODE(   88),CODE(   89)/
     *   Z0000,Z0000,Z0000,Z1F00/
      DATA CODE(   90),CODE(   91),CODE(   92),CODE(   93)/
     *   Z0001,Z9400,Z0140,Z0000/
      DATA CODE(   94),CODE(   95),CODE(   96),CODE(   97)/
     *   Z1031,Z8012,Z0000,Z0000/
      DATA CODE(   98),CODE(   99),CODE(  100),CODE(  101)/
     *   Z210C,Z0000,Z0000,Z0070/
      DATA CODE(  102),CODE(  103),CODE(  104),CODE(  105)/
     *   Z0003,ZE8E0,Z0000,Z0000/
C-----------------------------------------------------------------------
      J1 = A
      J2 = I
      J3 = B
      J4 = C
      J5 = J
      J6 = N
      CALL APEX (CODE, 0, SLIST, 6)
C
 999  RETURN
      END
