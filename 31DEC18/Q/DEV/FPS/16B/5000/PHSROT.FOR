      SUBROUTINE PHSROT(I
     *    1,I 2,I 3,I 4,I 5,I 6,I
     *    7)
C-----------------------------------------------------------------------
C! FPS AP 5xxx load module.
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
      INTEGER*2 CODE(  133)
      INTEGER*2 I 1,J 1
      INTEGER*2 I 2,J 2
      INTEGER*2 I 3,J 3
      INTEGER*2 I 4,J 4
      INTEGER*2 I 5,J 5
      INTEGER*2 I 6,J 6
      INTEGER*2 I 7,J 7
      INTEGER*2 N   0
      INTEGER*2 N   7
      INTEGER*2 SLIST(16)
      COMMON /SPARY/SLIST
      EQUIVALENCE (J 1,SLIST( 1))
      EQUIVALENCE (J 2,SLIST( 2))
      EQUIVALENCE (J 3,SLIST( 3))
      EQUIVALENCE (J 4,SLIST( 4))
      EQUIVALENCE (J 5,SLIST( 5))
      EQUIVALENCE (J 6,SLIST( 6))
      EQUIVALENCE (J 7,SLIST( 7))
      DATA CODE(1) /  33/
      DATA CODE(    2),CODE(    3),CODE(    4),CODE(    5)/
     *  Z4154, Z0000, Z0000, Z0030/
      DATA CODE(    6),CODE(    7),CODE(    8),CODE(    9)/
     *  Z0000, Z0000, Z0000, Z0000/
      DATA CODE(   10),CODE(   11),CODE(   12),CODE(   13)/
     *  Z0000, Z0000, Z0000, Z0010/
      DATA CODE(   14),CODE(   15),CODE(   16),CODE(   17)/
     *  Z4198, Z0000, Z5A07, ZE000/
      DATA CODE(   18),CODE(   19),CODE(   20),CODE(   21)/
     *  Z4110, Z01F2, Z0000, Z0030/
      DATA CODE(   22),CODE(   23),CODE(   24),CODE(   25)/
     *  Z1208, Z0000, Z0000, Z001B/
      DATA CODE(   26),CODE(   27),CODE(   28),CODE(   29)/
     *  Z0000, Z0000, Z5A06, ZC000/
      DATA CODE(   30),CODE(   31),CODE(   32),CODE(   33)/
     *  Z0000, Z0000, Z0000, Z0010/
      DATA CODE(   34),CODE(   35),CODE(   36),CODE(   37)/
     *  Z0000, Z0000, Z1A00, ZA000/
      DATA CODE(   38),CODE(   39),CODE(   40),CODE(   41)/
     *  Z0000, Z0000, Z0000, Z0000/
      DATA CODE(   42),CODE(   43),CODE(   44),CODE(   45)/
     *  Z0000, Z0000, Z4A3D, Z1B00/
      DATA CODE(   46),CODE(   47),CODE(   48),CODE(   49)/
     *  Z4000, Z0000, Z01A8, Z1930/
      DATA CODE(   50),CODE(   51),CODE(   52),CODE(   53)/
     *  Z30C8, Z0000, Z01E8, Z1900/
      DATA CODE(   54),CODE(   55),CODE(   56),CODE(   57)/
     *  Z0000, Z0000, ZC173, Z1610/
      DATA CODE(   58),CODE(   59),CODE(   60),CODE(   61)/
     *  Z0001, Z9400, Z00E8, Z1B00/
      DATA CODE(   62),CODE(   63),CODE(   64),CODE(   65)/
     *  Z0001, Z8000, Z3140, Z7700/
      DATA CODE(   66),CODE(   67),CODE(   68),CODE(   69)/
     *  Z0000, Z9600, Z815D, Z1700/
      DATA CODE(   70),CODE(   71),CODE(   72),CODE(   73)/
     *  Z0299, Z9A00, Z0028, Z1B00/
      DATA CODE(   74),CODE(   75),CODE(   76),CODE(   77)/
     *  Z0001, Z9BF2, Z2178, ZB600/
      DATA CODE(   78),CODE(   79),CODE(   80),CODE(   81)/
     *  Z1208, Z0000, Z0000, Z0009/
      DATA CODE(   82),CODE(   83),CODE(   84),CODE(   85)/
     *  Z2040, Z9200, Z01A8, Z1930/
      DATA CODE(   86),CODE(   87),CODE(   88),CODE(   89)/
     *  Z0001, Z9200, Z01E8, Z1900/
      DATA CODE(   90),CODE(   91),CODE(   92),CODE(   93)/
     *  Z0000, Z0000, ZC173, Z1610/
      DATA CODE(   94),CODE(   95),CODE(   96),CODE(   97)/
     *  Z0001, Z9400, Z20E8, Z7B00/
      DATA CODE(   98),CODE(   99),CODE(  100),CODE(  101)/
     *  Z20C9, ZD200, Z3958, Z77F0/
      DATA CODE(  102),CODE(  103),CODE(  104),CODE(  105)/
     *  Z0000, Z9600, Z815D, Z1700/
      DATA CODE(  106),CODE(  107),CODE(  108),CODE(  109)/
     *  Z0299, Z9A00, Z0028, Z1B50/
      DATA CODE(  110),CODE(  111),CODE(  112),CODE(  113)/
     *  Z0001, Z9BE9, Z2178, ZB600/
      DATA CODE(  114),CODE(  115),CODE(  116),CODE(  117)/
     *  Z0000, Z9200, Z0000, Z1F00/
      DATA CODE(  118),CODE(  119),CODE(  120),CODE(  121)/
     *  Z0001, Z9200, Z0000, Z0000/
      DATA CODE(  122),CODE(  123),CODE(  124),CODE(  125)/
     *  Z4001, Z8000, Z0000, Z0070/
      DATA CODE(  126),CODE(  127),CODE(  128),CODE(  129)/
     *  Z0000, Z0000, Z0000, Z0050/
      DATA CODE(  130),CODE(  131),CODE(  132),CODE(  133)/
     *  Z0000, Z00E0, Z0000, Z0000/
      DATA N   0 /   0/
      DATA N   7 /   7/
C-----------------------------------------------------------------------
      J 1 = I 1
      J 2 = I 2
      J 3 = I 3
      J 4 = I 4
      J 5 = I 5
      J 6 = I 6
      J 7 = I 7
      CALL APEX (CODE, N   0, SLIST, N   7)
      RETURN
      END
