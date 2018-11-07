      SUBROUTINE MAXMIN (A, I, MAX, MIN, N)
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
C     MICROCODE PROGRAM FOR FPS AP-120B ARRAY PROCESSOR
C
C    MAXMIN searches the given vector for maximum and minimum values
C    CALLIN SEQUENCE: CALL MAXMIN(A,I,MAX,MIN,N)
C               A = Source vector base address
C               I = Increment for A
C               MAX = Location for maximum.
C               MIN = Location for minimum
C               N = Element count
C    The routine initializes MAX and MIN to zero before starting on
C    the vector.
C    MAXMIN should work for both fast and slow AP memory.
C
C    Timing = 1.0 + 0.7 N microseconds ( add 0.7 for each new min.
C                                        and 0.5 for each new max.)
C
C    Programmer = W. D. Cotton, July 1980.
C-----------------------------------------------------------------------
      INTEGER*2 CODE(173), SLIST(16), A, I, MAX, MIN, N, J1, J2,
     *   J3, J4, J5
      COMMON /SPARY/ SLIST
      EQUIVALENCE (J1, SLIST( 1))
      EQUIVALENCE (J2, SLIST( 2))
      EQUIVALENCE (J3, SLIST( 3))
      EQUIVALENCE (J4, SLIST( 4))
      EQUIVALENCE (J5, SLIST( 5))
      DATA CODE(1) /  43/
      DATA CODE(    2),CODE(    3),CODE(    4),CODE(    5)/
     *   Z4000,Z0000,Z0000,Z0030/
      DATA CODE(    6),CODE(    7),CODE(    8),CODE(    9)/
     *   Z0290,Z0000,Z0000,Z0000/
      DATA CODE(   10),CODE(   11),CODE(   12),CODE(   13)/
     *   Z0000,Z01F2,Z0000,Z0000/
      DATA CODE(   14),CODE(   15),CODE(   16),CODE(   17)/
     *   Z0000,Z00E0,Z0000,Z0000/
      DATA CODE(   18),CODE(   19),CODE(   20),CODE(   21)/
     *   Z0001,ZDA00,Z4A04,Z0000/
      DATA CODE(   22),CODE(   23),CODE(   24),CODE(   25)/
     *   Z0001,Z8000,Z1A00,Z8000/
      DATA CODE(   26),CODE(   27),CODE(   28),CODE(   29)/
     *   Z2040,Z0000,Z0000,Z0030/
      DATA CODE(   30),CODE(   31),CODE(   32),CODE(   33)/
     *   Z0290,Z0000,Z0000,Z0000/
      DATA CODE(   34),CODE(   35),CODE(   36),CODE(   37)/
     *   Z0000,Z01F2,Z0000,Z0000/
      DATA CODE(   38),CODE(   39),CODE(   40),CODE(   41)/
     *   Z1208,Z0000,Z0000,Z0010/
      DATA CODE(   42),CODE(   43),CODE(   44),CODE(   45)/
     *   Z2040,ZA800,Z0100,Z0030/
      DATA CODE(   46),CODE(   47),CODE(   48),CODE(   49)/
     *   Z0291,Z3800,Z0020,Z0000/
      DATA CODE(   50),CODE(   51),CODE(   52),CODE(   53)/
     *   Z0000,Z01F2,Z4A05,Z0000/
      DATA CODE(   54),CODE(   55),CODE(   56),CODE(   57)/
     *   Z1208,Z0000,Z0000,Z000C/
      DATA CODE(   58),CODE(   59),CODE(   60),CODE(   61)/
     *   Z2040,ZA975,Z1B00,ZC030/
      DATA CODE(   62),CODE(   63),CODE(   64),CODE(   65)/
     *   Z0001,Z3800,Z0020,Z0000/
      DATA CODE(   66),CODE(   67),CODE(   68),CODE(   69)/
     *   Z0290,Z0178,Z0000,Z0000/
      DATA CODE(   70),CODE(   71),CODE(   72),CODE(   73)/
     *   Z0000,Z01ED,Z4835,Z0000/
      DATA CODE(   74),CODE(   75),CODE(   76),CODE(   77)/
     *   Z0000,Z0057,Z0000,Z0000/
      DATA CODE(   78),CODE(   79),CODE(   80),CODE(   81)/
     *   Z0000,Z0000,Z4744,Z0000/
      DATA CODE(   82),CODE(   83),CODE(   84),CODE(   85)/
     *   Z4000,ZA600,Z4936,Z0030/
      DATA CODE(   86),CODE(   87),CODE(   88),CODE(   89)/
     *   Z0291,Z3400,Z01A0,Z0000/
      DATA CODE(   90),CODE(   91),CODE(   92),CODE(   93)/
     *   Z0000,Z01E8,Z4835,Z0000/
      DATA CODE(   94),CODE(   95),CODE(   96),CODE(   97)/
     *   Z0000,Z0052,Z0000,Z0000/
      DATA CODE(   98),CODE(   99),CODE(  100),CODE(  101)/
     *   Z0250,Z004C,Z1740,Z8000/
      DATA CODE(  102),CODE(  103),CODE(  104),CODE(  105)/
     *   Z0000,ZA974,Z1B00,ZC000/
      DATA CODE(  106),CODE(  107),CODE(  108),CODE(  109)/
     *   Z0001,Z3800,Z0020,Z0000/
      DATA CODE(  110),CODE(  111),CODE(  112),CODE(  113)/
     *   Z0000,Z0176,Z4A05,Z0000/
      DATA CODE(  114),CODE(  115),CODE(  116),CODE(  117)/
     *   Z0000,Z0056,Z0000,Z0000/
      DATA CODE(  118),CODE(  119),CODE(  120),CODE(  121)/
     *   Z0000,Z0000,Z4744,Z0000/
      DATA CODE(  122),CODE(  123),CODE(  124),CODE(  125)/
     *   Z0000,ZA600,Z4936,Z0000/
      DATA CODE(  126),CODE(  127),CODE(  128),CODE(  129)/
     *   Z0001,Z3400,Z01A0,Z0000/
      DATA CODE(  130),CODE(  131),CODE(  132),CODE(  133)/
     *   Z0000,Z0049,Z4835,Z0000/
      DATA CODE(  134),CODE(  135),CODE(  136),CODE(  137)/
     *   Z0000,Z004D,Z1740,Z8000/
      DATA CODE(  138),CODE(  139),CODE(  140),CODE(  141)/
     *   Z0000,Z0173,Z0000,Z0000/
      DATA CODE(  142),CODE(  143),CODE(  144),CODE(  145)/
     *   Z0000,Z0173,Z0000,Z0000/
      DATA CODE(  146),CODE(  147),CODE(  148),CODE(  149)/
     *   Z0000,Z0053,Z0000,Z0000/
      DATA CODE(  150),CODE(  151),CODE(  152),CODE(  153)/
     *   Z0000,Z0052,Z4744,Z0000/
      DATA CODE(  154),CODE(  155),CODE(  156),CODE(  157)/
     *   Z0000,Z0000,Z1740,Z8000/
      DATA CODE(  158),CODE(  159),CODE(  160),CODE(  161)/
     *   Z4088,Z0000,Z0700,Z00F0/
      DATA CODE(  162),CODE(  163),CODE(  164),CODE(  165)/
     *   Z0000,Z0000,Z0000,Z0000/
      DATA CODE(  166),CODE(  167),CODE(  168),CODE(  169)/
     *   Z40CC,Z0000,Z0820,Z00F0/
      DATA CODE(  170),CODE(  171),CODE(  172),CODE(  173)/
     *   Z0000,Z00E0,Z0000,Z0000/
C-----------------------------------------------------------------------
      J1 = A
      J2 = I
      J3 = MAX
      J4 = MIN
      J5 = N
      CALL APEX (CODE, 0, SLIST, 5)
C
 999  RETURN
      END
