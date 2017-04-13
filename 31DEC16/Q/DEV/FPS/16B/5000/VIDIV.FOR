      SUBROUTINE VIDIV (I
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
      INTEGER*2 CODE(  249)
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
      DATA CODE(1) /  62/
      DATA CODE(    2),CODE(    3),CODE(    4),CODE(    5)/
     *  Z41BC, Z0000, Z0000, Z0000/
      DATA CODE(    6),CODE(    7),CODE(    8),CODE(    9)/
     *  Z0000, Z01F2, Z0000, Z0000/
      DATA CODE(   10),CODE(   11),CODE(   12),CODE(   13)/
     *  Z0000, Z00E0, Z0000, Z0000/
      DATA CODE(   14),CODE(   15),CODE(   16),CODE(   17)/
     *  Z120C, Z0000, Z0000, Z0033/
      DATA CODE(   18),CODE(   19),CODE(   20),CODE(   21)/
     *  Z40BC, Z0000, Z4746, Z0000/
      DATA CODE(   22),CODE(   23),CODE(   24),CODE(   25)/
     *  Z120C, Z0000, Z0000, Z0031/
      DATA CODE(   26),CODE(   27),CODE(   28),CODE(   29)/
     *  Z40FC, Z0000, Z1740, ZC000/
      DATA CODE(   30),CODE(   31),CODE(   32),CODE(   33)/
     *  Z120C, Z0000, Z0000, Z002F/
      DATA CODE(   34),CODE(   35),CODE(   36),CODE(   37)/
     *  Z0000, Z0000, Z0170, Z1600/
      DATA CODE(   38),CODE(   39),CODE(   40),CODE(   41)/
     *  Z3150, Z0000, Z0000, Z1F00/
      DATA CODE(   42),CODE(   43),CODE(   44),CODE(   45)/
     *  Z0000, Z0000, Z1E00, Z9F00/
      DATA CODE(   46),CODE(   47),CODE(   48),CODE(   49)/
     *  Z0000, Z0000, ZDE04, ZA000/
      DATA CODE(   50),CODE(   51),CODE(   52),CODE(   53)/
     *  Z120C, Z0000, Z0000, Z000E/
      DATA CODE(   54),CODE(   55),CODE(   56),CODE(   57)/
     *  Z4001, Z2600, Z01A8, Z0030/
      DATA CODE(   58),CODE(   59),CODE(   60),CODE(   61)/
     *  Z0001, Z80DB, Z0000, Z0000/
      DATA CODE(   62),CODE(   63),CODE(   64),CODE(   65)/
     *  Z0000, Z0000, Z8006, Z0000/
      DATA CODE(   66),CODE(   67),CODE(   68),CODE(   69)/
     *  Z0000, Z0172, Z0100, Z1700/
      DATA CODE(   70),CODE(   71),CODE(   72),CODE(   73)/
     *  Z0000, Z0055, Z0000, Z0000/
      DATA CODE(   74),CODE(   75),CODE(   76),CODE(   77)/
     *  Z2041, Z2600, Z01A8, Z1F30/
      DATA CODE(   78),CODE(   79),CODE(   80),CODE(   81)/
     *  Z0001, Z8000, Z0000, Z1F00/
      DATA CODE(   82),CODE(   83),CODE(   84),CODE(   85)/
     *  Z2150, Z0000, Z8006, Z00B0/
      DATA CODE(   86),CODE(   87),CODE(   88),CODE(   89)/
     *  Z0000, Z016D, Z0100, Z1700/
      DATA CODE(   90),CODE(   91),CODE(   92),CODE(   93)/
     *  Z0000, Z0000, Z0000, Z1F00/
      DATA CODE(   94),CODE(   95),CODE(   96),CODE(   97)/
     *  Z0000, Z0000, Z0000, Z1F00/
      DATA CODE(   98),CODE(   99),CODE(  100),CODE(  101)/
     *  Z2150, Z0000, Z0000, Z00B0/
      DATA CODE(  102),CODE(  103),CODE(  104),CODE(  105)/
     *  Z0000, Z00E0, Z0000, Z0000/
      DATA CODE(  106),CODE(  107),CODE(  108),CODE(  109)/
     *  Z03BC, Z5400, Z0500, Z1002/
      DATA CODE(  110),CODE(  111),CODE(  112),CODE(  113)/
     *  Z0001, Z8000, Z1820, Z8000/
      DATA CODE(  114),CODE(  115),CODE(  116),CODE(  117)/
     *  Z03B7, Z8600, Z8404, Z1000/
      DATA CODE(  118),CODE(  119),CODE(  120),CODE(  121)/
     *  Z03F8, Z0119, Z0700, Z0000/
      DATA CODE(  122),CODE(  123),CODE(  124),CODE(  125)/
     *  Z23BC, Z0000, Z0000, Z0003/
      DATA CODE(  126),CODE(  127),CODE(  128),CODE(  129)/
     *  Z023E, ZCC00, Z0100, Z0000/
      DATA CODE(  130),CODE(  131),CODE(  132),CODE(  133)/
     *  Z0001, Z8000, Z0020, Z1E00/
      DATA CODE(  134),CODE(  135),CODE(  136),CODE(  137)/
     *  Z0274, Z0000, Z0000, Z1C03/
      DATA CODE(  138),CODE(  139),CODE(  140),CODE(  141)/
     *  Z0378, Z0000, Z0700, Z1F00/
      DATA CODE(  142),CODE(  143),CODE(  144),CODE(  145)/
     *  Z1010, Z9A1E, Z7E04, Z9F00/
      DATA CODE(  146),CODE(  147),CODE(  148),CODE(  149)/
     *  Z02F8, Z9400, ZC104, Z0000/
      DATA CODE(  150),CODE(  151),CODE(  152),CODE(  153)/
     *  Z027D, Z8055, Z0100, Z1500/
      DATA CODE(  154),CODE(  155),CODE(  156),CODE(  157)/
     *  Z033F, Z9C00, Z4404, Z03FF/
      DATA CODE(  158),CODE(  159),CODE(  160),CODE(  161)/
     *  Z0040, Z0000, Z1700, Z8000/
      DATA CODE(  162),CODE(  163),CODE(  164),CODE(  165)/
     *  Z03B8, Z0000, Z0400, Z2000/
      DATA CODE(  166),CODE(  167),CODE(  168),CODE(  169)/
     *  Z63BF, Z8C5A, Z0D20, Z1600/
      DATA CODE(  170),CODE(  171),CODE(  172),CODE(  173)/
     *  Z0278, Z0000, Z4C25, Z1800/
      DATA CODE(  174),CODE(  175),CODE(  176),CODE(  177)/
     *  Z027D, Z5E00, Z1F40, Z9F00/
      DATA CODE(  178),CODE(  179),CODE(  180),CODE(  181)/
     *  Z0001, Z9600, Z0020, Z1F00/
      DATA CODE(  182),CODE(  183),CODE(  184),CODE(  185)/
     *  Z0001, Z8000, Z0000, Z1000/
      DATA CODE(  186),CODE(  187),CODE(  188),CODE(  189)/
     *  Z0000, Z0000, Z0000, Z1F00/
      DATA CODE(  190),CODE(  191),CODE(  192),CODE(  193)/
     *  Z0000, Z0000, Z0000, Z1F00/
      DATA CODE(  194),CODE(  195),CODE(  196),CODE(  197)/
     *  Z0000, Z0053, Z0000, Z1000/
      DATA CODE(  198),CODE(  199),CODE(  200),CODE(  201)/
     *  Z02F8, Z9400, ZC104, Z0000/
      DATA CODE(  202),CODE(  203),CODE(  204),CODE(  205)/
     *  Z027D, Z8048, Z2100, Z9500/
      DATA CODE(  206),CODE(  207),CODE(  208),CODE(  209)/
     *  Z0000, Z0000, Z0000, Z1F00/
      DATA CODE(  210),CODE(  211),CODE(  212),CODE(  213)/
     *  Z0000, Z0000, Z0000, Z1F00/
      DATA CODE(  214),CODE(  215),CODE(  216),CODE(  217)/
     *  Z0000, Z00E0, ZC004, Z0000/
      DATA CODE(  218),CODE(  219),CODE(  220),CODE(  221)/
     *  Z43FC, Z0000, Z4C05, Z0000/
      DATA CODE(  222),CODE(  223),CODE(  224),CODE(  225)/
     *  Z03BC, Z01D3, Z0400, Z001B/
      DATA CODE(  226),CODE(  227),CODE(  228),CODE(  229)/
     *  Z12CC, Z0000, Z1004, Z0005/
      DATA CODE(  230),CODE(  231),CODE(  232),CODE(  233)/
     *  Z43FD, ZBC52, Z0160, Z0000/
      DATA CODE(  234),CODE(  235),CODE(  236),CODE(  237)/
     *  Z43FD, ZDC00, Z0140, Z0000/
      DATA CODE(  238),CODE(  239),CODE(  240),CODE(  241)/
     *  Z0001, Z8000, Z0000, Z0000/
      DATA CODE(  242),CODE(  243),CODE(  244),CODE(  245)/
     *  Z0003, Z86E0, Z8405, Z1001/
      DATA CODE(  246),CODE(  247),CODE(  248),CODE(  249)/
     *  Z0000, Z0021, Z1400, Z0000/
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
