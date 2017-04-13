      SUBROUTINE DIRADD (I1, I2, I3, I4)
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
C
C
C
C
C
C
C
C
C-----------------------------------------------------------------------
      INTEGER*2 CODE(113), I1, I2, I3, I4, J1, J2, J3, J4, SLIST(16)
      COMMON /SPARY/ SLIST
      EQUIVALENCE (J1, SLIST( 1))
      EQUIVALENCE (J2, SLIST( 2))
      EQUIVALENCE (J3, SLIST( 3))
      EQUIVALENCE (J4, SLIST( 4))
C
      DATA CODE(1) /  28/
      DATA CODE(    2),CODE(    3),CODE(    4),CODE(    5)/
     *   Z4000,Z0000,Z0000,Z0030/
      DATA CODE(    6),CODE(    7),CODE(    8),CODE(    9)/
     *   Z0000,Z0000,Z0000,Z0010/
      DATA CODE(   10),CODE(   11),CODE(   12),CODE(   13)/
     *   Z0000,Z0000,Z0000,Z0010/
      DATA CODE(   14),CODE(   15),CODE(   16),CODE(   17)/
     *   Z0390,Z0000,Z0A00,Z0000/
      DATA CODE(   18),CODE(   19),CODE(   20),CODE(   21)/
     *   Z028C,Z0000,Z4A04,Z0000/
      DATA CODE(   22),CODE(   23),CODE(   24),CODE(   25)/
     *   Z0000,Z019E,Z1A00,Z8000/
      DATA CODE(   26),CODE(   27),CODE(   28),CODE(   29)/
     *   Z2090,Z0000,Z0000,Z0030/
      DATA CODE(   30),CODE(   31),CODE(   32),CODE(   33)/
     *   Z0000,Z0000,Z0000,Z0010/
      DATA CODE(   34),CODE(   35),CODE(   36),CODE(   37)/
     *   Z2040,Z0000,Z0000,Z0030/
      DATA CODE(   38),CODE(   39),CODE(   40),CODE(   41)/
     *   Z028D,ZA800,Z0100,Z0010/
      DATA CODE(   42),CODE(   43),CODE(   44),CODE(   45)/
     *   Z4115,ZB996,Z0020,Z0010/
      DATA CODE(   46),CODE(   47),CODE(   48),CODE(   49)/
     *   Z0390,Z0000,Z0A00,Z0000/
      DATA CODE(   50),CODE(   51),CODE(   52),CODE(   53)/
     *   Z4155,Z8000,Z4A04,Z0070/
      DATA CODE(   54),CODE(   55),CODE(   56),CODE(   57)/
     *   Z0000,Z0000,Z1A00,Z8050/
      DATA CODE(   58),CODE(   59),CODE(   60),CODE(   61)/
     *   Z2090,Z0000,Z0000,Z0030/
      DATA CODE(   62),CODE(   63),CODE(   64),CODE(   65)/
     *   Z0000,Z0049,Z0000,Z0010/
      DATA CODE(   66),CODE(   67),CODE(   68),CODE(   69)/
     *   Z0390,Z0000,Z0A00,Z0000/
      DATA CODE(   70),CODE(   71),CODE(   72),CODE(   73)/
     *   Z4155,Z8000,Z4A04,Z0070/
      DATA CODE(   74),CODE(   75),CODE(   76),CODE(   77)/
     *   Z0000,Z0000,Z1A00,Z8050/
      DATA CODE(   78),CODE(   79),CODE(   80),CODE(   81)/
     *   Z2090,Z0000,Z0000,Z0030/
      DATA CODE(   82),CODE(   83),CODE(   84),CODE(   85)/
     *   Z0000,Z0000,Z0000,Z0010/
      DATA CODE(   86),CODE(   87),CODE(   88),CODE(   89)/
     *   Z0000,Z0000,Z0000,Z0000/
      DATA CODE(   90),CODE(   91),CODE(   92),CODE(   93)/
     *   Z0001,ZA800,Z0100,Z0000/
      DATA CODE(   94),CODE(   95),CODE(   96),CODE(   97)/
     *   Z4115,ZB800,Z0020,Z0000/
      DATA CODE(   98),CODE(   99),CODE(  100),CODE(  101)/
     *   Z0000,Z0000,Z0000,Z0000/
      DATA CODE(  102),CODE(  103),CODE(  104),CODE(  105)/
     *   Z4155,Z8000,Z0000,Z0070/
      DATA CODE(  106),CODE(  107),CODE(  108),CODE(  109)/
     *   Z0000,Z0000,Z0000,Z0050/
      DATA CODE(  110),CODE(  111),CODE(  112),CODE(  113)/
     *   Z0000,Z00E0,Z0000,Z0000/
C-----------------------------------------------------------------------
      J1 = I1
      J2 = I2
      J3 = I3
      J4 = I4
      CALL APEX (CODE, 0, SLIST, 4)
 999  RETURN
      END
