      SUBROUTINE CVSDIV(I
     *    1,I 2,I 3,I 4,I 5,I
     *    6)
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
      INTEGER*2 CODE(  369)
      INTEGER*2 I 1,J 1
      INTEGER*2 I 2,J 2
      INTEGER*2 I 3,J 3
      INTEGER*2 I 4,J 4
      INTEGER*2 I 5,J 5
      INTEGER*2 I 6,J 6
      INTEGER*2 N   0
      INTEGER*2 N   6
      INTEGER*2 SLIST(16)
      COMMON /SPARY/SLIST
      EQUIVALENCE (J 1,SLIST( 1))
      EQUIVALENCE (J 2,SLIST( 2))
      EQUIVALENCE (J 3,SLIST( 3))
      EQUIVALENCE (J 4,SLIST( 4))
      EQUIVALENCE (J 5,SLIST( 5))
      EQUIVALENCE (J 6,SLIST( 6))
      DATA CODE(1) /  92/
      DATA CODE(    2),CODE(    3),CODE(    4),CODE(    5)/
     *  Z4088, Z0000, Z0000, Z0030/
      DATA CODE(    6),CODE(    7),CODE(    8),CODE(    9)/
     *  Z0000, Z0000, Z0000, Z0000/
      DATA CODE(   10),CODE(   11),CODE(   12),CODE(   13)/
     *  Z0000, Z0000, Z0000, Z0010/
      DATA CODE(   14),CODE(   15),CODE(   16),CODE(   17)/
     *  Z0000, Z0000, Z4A07, Z0000/
      DATA CODE(   18),CODE(   19),CODE(   20),CODE(   21)/
     *  Z3040, Z0000, Z01C0, Z1700/
      DATA CODE(   22),CODE(   23),CODE(   24),CODE(   25)/
     *  Z0000, Z0000, Z1A00, ZE000/
      DATA CODE(   26),CODE(   27),CODE(   28),CODE(   29)/
     *  Z0000, Z0000, Z0038, Z1B00/
      DATA CODE(   30),CODE(   31),CODE(   32),CODE(   33)/
     *  Z310C, Z0000, Z0000, Z1F00/
      DATA CODE(   34),CODE(   35),CODE(   36),CODE(   37)/
     *  Z024C, Z0000, ZC004, Z1F00/
      DATA CODE(   38),CODE(   39),CODE(   40),CODE(   41)/
     *  Z024D, Z9400, Z0100, Z0000/
      DATA CODE(   42),CODE(   43),CODE(   44),CODE(   45)/
     *  Z0001, Z8000, Z17C0, Z8000/
      DATA CODE(   46),CODE(   47),CODE(   48),CODE(   49)/
     *  Z0000, Z0000, Z8003, Z0000/
      DATA CODE(   50),CODE(   51),CODE(   52),CODE(   53)/
     *  Z0000, Z0000, Z8004, Z0000/
      DATA CODE(   54),CODE(   55),CODE(   56),CODE(   57)/
     *  Z120C, Z0000, Z0000, Z0033/
      DATA CODE(   58),CODE(   59),CODE(   60),CODE(   61)/
     *  Z0000, Z0000, Z4707, Z0000/
      DATA CODE(   62),CODE(   63),CODE(   64),CODE(   65)/
     *  Z0000, Z0000, Z46C4, Z0000/
      DATA CODE(   66),CODE(   67),CODE(   68),CODE(   69)/
     *  Z0000, Z0000, Z1838, Z8000/
      DATA CODE(   70),CODE(   71),CODE(   72),CODE(   73)/
     *  Z120C, Z0000, Z0000, Z002F/
      DATA CODE(   74),CODE(   75),CODE(   76),CODE(   77)/
     *  Z0000, Z0000, Z1700, ZE000/
      DATA CODE(   78),CODE(   79),CODE(   80),CODE(   81)/
     *  Z0000, Z0000, Z46C4, Z0000/
      DATA CODE(   82),CODE(   83),CODE(   84),CODE(   85)/
     *  Z120C, Z0000, Z0000, Z0010/
      DATA CODE(   86),CODE(   87),CODE(   88),CODE(   89)/
     *  Z0000, Z0000, Z4703, Z0000/
      DATA CODE(   90),CODE(   91),CODE(   92),CODE(   93)/
     *  Z2040, Z0000, Z0000, Z0030/
      DATA CODE(   94),CODE(   95),CODE(   96),CODE(   97)/
     *  Z0000, Z0000, Z0000, Z0000/
      DATA CODE(   98),CODE(   99),CODE(  100),CODE(  101)/
     *  Z0000, Z0000, Z0000, Z0010/
      DATA CODE(  102),CODE(  103),CODE(  104),CODE(  105)/
     *  Z0000, Z0000, Z01C0, Z1700/
      DATA CODE(  106),CODE(  107),CODE(  108),CODE(  109)/
     *  Z0000, Z0000, Z0038, Z1B10/
      DATA CODE(  110),CODE(  111),CODE(  112),CODE(  113)/
     *  Z0000, Z0000, Z01C0, Z1700/
      DATA CODE(  114),CODE(  115),CODE(  116),CODE(  117)/
     *  Z0000, Z0000, ZC03C, Z1B00/
      DATA CODE(  118),CODE(  119),CODE(  120),CODE(  121)/
     *  Z0000, Z0000, Z30C0, Z9700/
      DATA CODE(  122),CODE(  123),CODE(  124),CODE(  125)/
     *  Z0001, Z1600, Z0020, Z1F00/
      DATA CODE(  126),CODE(  127),CODE(  128),CODE(  129)/
     *  Z0001, Z9400, Z0100, Z1F00/
      DATA CODE(  130),CODE(  131),CODE(  132),CODE(  133)/
     *  Z210D, Z8000, Z8004, Z00B0/
      DATA CODE(  134),CODE(  135),CODE(  136),CODE(  137)/
     *  Z0294, Z0000, Z0700, Z00E0/
      DATA CODE(  138),CODE(  139),CODE(  140),CODE(  141)/
     *  Z0000, Z01E4, Z0000, Z0060/
      DATA CODE(  142),CODE(  143),CODE(  144),CODE(  145)/
     *  Z0000, Z00E0, Z0000, Z0000/
      DATA CODE(  146),CODE(  147),CODE(  148),CODE(  149)/
     *  Z03BD, ZD400, Z0500, Z0801/
      DATA CODE(  150),CODE(  151),CODE(  152),CODE(  153)/
     *  Z03F9, ZD400, Z0700, Z0000/
      DATA CODE(  154),CODE(  155),CODE(  156),CODE(  157)/
     *  Z0237, Z8600, Z0400, Z1082/
      DATA CODE(  158),CODE(  159),CODE(  160),CODE(  161)/
     *  Z4FB9, Z8154, Z0000, Z0000/
      DATA CODE(  162),CODE(  163),CODE(  164),CODE(  165)/
     *  Z0000, Z7400, Z0100, Z0000/
      DATA CODE(  166),CODE(  167),CODE(  168),CODE(  169)/
     *  Z0001, Z8000, Z0000, Z0000/
      DATA CODE(  170),CODE(  171),CODE(  172),CODE(  173)/
     *  Z0000, Z004A, Z8004, Z0000/
      DATA CODE(  174),CODE(  175),CODE(  176),CODE(  177)/
     *  Z02F6, ZCC00, Z0100, Z0000/
      DATA CODE(  178),CODE(  179),CODE(  180),CODE(  181)/
     *  Z27BD, Z8132, Z0000, Z0003/
      DATA CODE(  182),CODE(  183),CODE(  184),CODE(  185)/
     *  Z0000, Z00E0, Z4004, Z0000/
      DATA CODE(  186),CODE(  187),CODE(  188),CODE(  189)/
     *  Z03BF, Z8600, Z0400, Z1086/
      DATA CODE(  190),CODE(  191),CODE(  192),CODE(  193)/
     *  Z0370, Z0000, Z0700, Z1C02/
      DATA CODE(  194),CODE(  195),CODE(  196),CODE(  197)/
     *  Z4F30, Z0000, Z4C04, Z1F00/
      DATA CODE(  198),CODE(  199),CODE(  200),CODE(  201)/
     *  Z100C, Z0012, Z4E05, Z1F02/
      DATA CODE(  202),CODE(  203),CODE(  204),CODE(  205)/
     *  Z0000, Z0052, Z3140, Z9102/
      DATA CODE(  206),CODE(  207),CODE(  208),CODE(  209)/
     *  Z0000, Z0000, Z3140, Z9100/
      DATA CODE(  210),CODE(  211),CODE(  212),CODE(  213)/
     *  Z4375, Z5E00, Z0120, Z1A00/
      DATA CODE(  214),CODE(  215),CODE(  216),CODE(  217)/
     *  Z23BD, Z8000, Z0000, Z1F03/
      DATA CODE(  218),CODE(  219),CODE(  220),CODE(  221)/
     *  Z0001, Z9400, Z0140, Z1C00/
      DATA CODE(  222),CODE(  223),CODE(  224),CODE(  225)/
     *  Z0001, Z8000, Z3020, ZB200/
      DATA CODE(  226),CODE(  227),CODE(  228),CODE(  229)/
     *  Z0000, ZB200, Z1E28, ZBF00/
      DATA CODE(  230),CODE(  231),CODE(  232),CODE(  233)/
     *  Z0001, Z8000, Z0028, Z1200/
      DATA CODE(  234),CODE(  235),CODE(  236),CODE(  237)/
     *  Z0001, Z9200, Z0000, Z1F00/
      DATA CODE(  238),CODE(  239),CODE(  240),CODE(  241)/
     *  Z0001, Z8000, Z0000, Z1F00/
      DATA CODE(  242),CODE(  243),CODE(  244),CODE(  245)/
     *  Z0000, Z0000, Z0000, Z1000/
      DATA CODE(  246),CODE(  247),CODE(  248),CODE(  249)/
     *  Z0000, Z0000, Z0000, Z1F00/
      DATA CODE(  250),CODE(  251),CODE(  252),CODE(  253)/
     *  Z0000, Z0000, Z0000, Z1F00/
      DATA CODE(  254),CODE(  255),CODE(  256),CODE(  257)/
     *  Z0000, Z00E0, ZC004, Z0000/
      DATA CODE(  258),CODE(  259),CODE(  260),CODE(  261)/
     *  Z03BC, Z5400, Z0500, Z1002/
      DATA CODE(  262),CODE(  263),CODE(  264),CODE(  265)/
     *  Z0001, Z8000, Z1820, Z8000/
      DATA CODE(  266),CODE(  267),CODE(  268),CODE(  269)/
     *  Z03B7, Z8600, Z8404, Z1000/
      DATA CODE(  270),CODE(  271),CODE(  272),CODE(  273)/
     *  Z03F8, Z0119, Z0700, Z0000/
      DATA CODE(  274),CODE(  275),CODE(  276),CODE(  277)/
     *  Z23BC, Z0000, Z0000, Z0003/
      DATA CODE(  278),CODE(  279),CODE(  280),CODE(  281)/
     *  Z023E, ZCC00, Z0100, Z0000/
      DATA CODE(  282),CODE(  283),CODE(  284),CODE(  285)/
     *  Z0001, Z8000, Z0020, Z1E00/
      DATA CODE(  286),CODE(  287),CODE(  288),CODE(  289)/
     *  Z0274, Z0000, Z0000, Z1C03/
      DATA CODE(  290),CODE(  291),CODE(  292),CODE(  293)/
     *  Z0378, Z0000, Z0700, Z1F00/
      DATA CODE(  294),CODE(  295),CODE(  296),CODE(  297)/
     *  Z1010, Z9A1E, Z7E04, Z9F00/
      DATA CODE(  298),CODE(  299),CODE(  300),CODE(  301)/
     *  Z02F8, Z9400, ZC104, Z0000/
      DATA CODE(  302),CODE(  303),CODE(  304),CODE(  305)/
     *  Z027D, Z8055, Z0100, Z1500/
      DATA CODE(  306),CODE(  307),CODE(  308),CODE(  309)/
     *  Z033F, Z9C00, Z4404, Z03FF/
      DATA CODE(  310),CODE(  311),CODE(  312),CODE(  313)/
     *  Z0040, Z0000, Z1700, Z8000/
      DATA CODE(  314),CODE(  315),CODE(  316),CODE(  317)/
     *  Z03B8, Z0000, Z0400, Z2000/
      DATA CODE(  318),CODE(  319),CODE(  320),CODE(  321)/
     *  Z63BF, Z8C5A, Z0D20, Z1600/
      DATA CODE(  322),CODE(  323),CODE(  324),CODE(  325)/
     *  Z0278, Z0000, Z4C25, Z1800/
      DATA CODE(  326),CODE(  327),CODE(  328),CODE(  329)/
     *  Z027D, Z5E00, Z1F40, Z9F00/
      DATA CODE(  330),CODE(  331),CODE(  332),CODE(  333)/
     *  Z0001, Z9600, Z0020, Z1F00/
      DATA CODE(  334),CODE(  335),CODE(  336),CODE(  337)/
     *  Z0001, Z8000, Z0000, Z1000/
      DATA CODE(  338),CODE(  339),CODE(  340),CODE(  341)/
     *  Z0000, Z0000, Z0000, Z1F00/
      DATA CODE(  342),CODE(  343),CODE(  344),CODE(  345)/
     *  Z0000, Z0000, Z0000, Z1F00/
      DATA CODE(  346),CODE(  347),CODE(  348),CODE(  349)/
     *  Z0000, Z0053, Z0000, Z1000/
      DATA CODE(  350),CODE(  351),CODE(  352),CODE(  353)/
     *  Z02F8, Z9400, ZC104, Z0000/
      DATA CODE(  354),CODE(  355),CODE(  356),CODE(  357)/
     *  Z027D, Z8048, Z2100, Z9500/
      DATA CODE(  358),CODE(  359),CODE(  360),CODE(  361)/
     *  Z0000, Z0000, Z0000, Z1F00/
      DATA CODE(  362),CODE(  363),CODE(  364),CODE(  365)/
     *  Z0000, Z0000, Z0000, Z1F00/
      DATA CODE(  366),CODE(  367),CODE(  368),CODE(  369)/
     *  Z0000, Z00E0, ZC004, Z0000/
      DATA N   0 /   0/
      DATA N   6 /   6/
C-----------------------------------------------------------------------
      J 1 = I 1
      J 2 = I 2
      J 3 = I 3
      J 4 = I 4
      J 5 = I 5
      J 6 = I 6
      CALL APEX (CODE, N   0, SLIST, N   6)
      RETURN
      END
