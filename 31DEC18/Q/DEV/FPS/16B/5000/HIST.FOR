      SUBROUTINE HIST  (I
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
      INTEGER*2 CODE(  605)
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
      DATA CODE(1) / 151/
      DATA CODE(    2),CODE(    3),CODE(    4),CODE(    5)/
     *  Z40E0, Z0000, Z0000, Z0000/
      DATA CODE(    6),CODE(    7),CODE(    8),CODE(    9)/
     *  Z414C, Z01F2, Z0000, Z0030/
      DATA CODE(   10),CODE(   11),CODE(   12),CODE(   13)/
     *  Z0000, Z00E0, Z0000, Z0000/
      DATA CODE(   14),CODE(   15),CODE(   16),CODE(   17)/
     *  Z413C, Z0000, Z0000, Z0000/
      DATA CODE(   18),CODE(   19),CODE(   20),CODE(   21)/
     *  Z40B0, Z0000, Z0000, Z0000/
      DATA CODE(   22),CODE(   23),CODE(   24),CODE(   25)/
     *  Z02BC, Z0000, Z0000, Z0000/
      DATA CODE(   26),CODE(   27),CODE(   28),CODE(   29)/
     *  Z4188, Z0000, Z1A00, ZC030/
      DATA CODE(   30),CODE(   31),CODE(   32),CODE(   33)/
     *  Z401C, Z0000, Z0000, Z0000/
      DATA CODE(   34),CODE(   35),CODE(   36),CODE(   37)/
     *  Z4010, Z0000, Z0000, Z0000/
      DATA CODE(   38),CODE(   39),CODE(   40),CODE(   41)/
     *  Z0001, Z3800, Z0030, Z0000/
      DATA CODE(   42),CODE(   43),CODE(   44),CODE(   45)/
     *  Z4055, Z8000, Z0000, Z0000/
      DATA CODE(   46),CODE(   47),CODE(   48),CODE(   49)/
     *  Z4218, Z0000, Z8006, Z0000/
      DATA CODE(   50),CODE(   51),CODE(   52),CODE(   53)/
     *  Z120C, Z0000, Z0000, Z003F/
      DATA CODE(   54),CODE(   55),CODE(   56),CODE(   57)/
     *  Z120C, Z0000, Z0000, Z0085/
      DATA CODE(   58),CODE(   59),CODE(   60),CODE(   61)/
     *  Z4088, Z0000, Z4784, Z0030/
      DATA CODE(   62),CODE(   63),CODE(   64),CODE(   65)/
     *  Z0000, Z0000, Z1740, Z8000/
      DATA CODE(   66),CODE(   67),CODE(   68),CODE(   69)/
     *  Z120C, Z0000, Z0000, Z0066/
      DATA CODE(   70),CODE(   71),CODE(   72),CODE(   73)/
     *  Z023C, Z0000, Z0000, Z0000/
      DATA CODE(   74),CODE(   75),CODE(   76),CODE(   77)/
     *  Z323C, Z0000, Z4706, Z0000/
      DATA CODE(   78),CODE(   79),CODE(   80),CODE(   81)/
     *  Z120C, Z0000, Z0000, Z007F/
      DATA CODE(   82),CODE(   83),CODE(   84),CODE(   85)/
     *  Z0003, Z8600, Z0400, Z1001/
      DATA CODE(   86),CODE(   87),CODE(   88),CODE(   89)/
     *  Z41C0, Z0000, Z0000, Z0030/
      DATA CODE(   90),CODE(   91),CODE(   92),CODE(   93)/
     *  Z4308, Z0000, Z1A00, Z8000/
      DATA CODE(   94),CODE(   95),CODE(   96),CODE(   97)/
     *  Z0001, ZC400, Z0140, Z0000/
      DATA CODE(   98),CODE(   99),CODE(  100),CODE(  101)/
     *  Z0000, ZB800, Z47A4, Z0000/
      DATA CODE(  102),CODE(  103),CODE(  104),CODE(  105)/
     *  Z0001, Z8000, Z8005, Z0000/
      DATA CODE(  106),CODE(  107),CODE(  108),CODE(  109)/
     *  Z1000, Z0012, Z0100, Z1400/
      DATA CODE(  110),CODE(  111),CODE(  112),CODE(  113)/
     *  Z0003, ZE000, Z0000, Z0000/
      DATA CODE(  114),CODE(  115),CODE(  116),CODE(  117)/
     *  Z2041, ZC400, Z0140, Z1F30/
      DATA CODE(  118),CODE(  119),CODE(  120),CODE(  121)/
     *  Z0001, Z8000, Z0000, Z1F00/
      DATA CODE(  122),CODE(  123),CODE(  124),CODE(  125)/
     *  Z0000, Z0000, ZB005, ZA000/
      DATA CODE(  126),CODE(  127),CODE(  128),CODE(  129)/
     *  Z1000, ZB812, Z0020, Z0000/
      DATA CODE(  130),CODE(  131),CODE(  132),CODE(  133)/
     *  Z0003, ZE200, Z0000, Z0000/
      DATA CODE(  134),CODE(  135),CODE(  136),CODE(  137)/
     *  Z0000, Z1600, Z0028, Z0000/
      DATA CODE(  138),CODE(  139),CODE(  140),CODE(  141)/
     *  Z0001, Z8000, Z0100, Z1400/
      DATA CODE(  142),CODE(  143),CODE(  144),CODE(  145)/
     *  Z2041, ZC400, Z8146, Z1F30/
      DATA CODE(  146),CODE(  147),CODE(  148),CODE(  149)/
     *  Z03A5, Z8000, Z0780, Z1F00/
      DATA CODE(  150),CODE(  151),CODE(  152),CODE(  153)/
     *  Z20A4, Z0000, ZB005, ZA030/
      DATA CODE(  154),CODE(  155),CODE(  156),CODE(  157)/
     *  Z1000, ZB812, Z0020, Z0000/
      DATA CODE(  158),CODE(  159),CODE(  160),CODE(  161)/
     *  Z0003, ZE400, Z0000, Z0000/
      DATA CODE(  162),CODE(  163),CODE(  164),CODE(  165)/
     *  Z0000, Z1600, Z0028, Z0000/
      DATA CODE(  166),CODE(  167),CODE(  168),CODE(  169)/
     *  Z4269, ZC800, Z0100, Z1400/
      DATA CODE(  170),CODE(  171),CODE(  172),CODE(  173)/
     *  Z2041, ZC400, Z8146, Z1F30/
      DATA CODE(  174),CODE(  175),CODE(  176),CODE(  177)/
     *  Z03A5, Z8000, Z8787, Z1F00/
      DATA CODE(  178),CODE(  179),CODE(  180),CODE(  181)/
     *  Z20A4, Z0000, ZB005, ZA030/
      DATA CODE(  182),CODE(  183),CODE(  184),CODE(  185)/
     *  Z3268, ZB954, Z17E0, ZC000/
      DATA CODE(  186),CODE(  187),CODE(  188),CODE(  189)/
     *  Z2268, Z1792, Z07E8, Z00F0/
      DATA CODE(  190),CODE(  191),CODE(  192),CODE(  193)/
     *  Z4269, ZC84B, Z0100, Z1400/
      DATA CODE(  194),CODE(  195),CODE(  196),CODE(  197)/
     *  Z4269, ZC64A, Z0130, Z1400/
      DATA CODE(  198),CODE(  199),CODE(  200),CODE(  201)/
     *  Z2268, Z1792, Z07E8, Z00F0/
      DATA CODE(  202),CODE(  203),CODE(  204),CODE(  205)/
     *  Z4269, ZC852, Z0100, Z1400/
      DATA CODE(  206),CODE(  207),CODE(  208),CODE(  209)/
     *  Z4269, ZC600, Z0130, Z1400/
      DATA CODE(  210),CODE(  211),CODE(  212),CODE(  213)/
     *  Z1030, Z0016, Z8006, Z1F00/
      DATA CODE(  214),CODE(  215),CODE(  216),CODE(  217)/
     *  Z03A4, Z0000, Z0780, Z1F00/
      DATA CODE(  218),CODE(  219),CODE(  220),CODE(  221)/
     *  Z20A4, Z0000, Z3000, ZA030/
      DATA CODE(  222),CODE(  223),CODE(  224),CODE(  225)/
     *  Z3269, Z8000, Z0000, Z0000/
      DATA CODE(  226),CODE(  227),CODE(  228),CODE(  229)/
     *  Z2268, Z1793, Z2028, ZC070/
      DATA CODE(  230),CODE(  231),CODE(  232),CODE(  233)/
     *  Z4269, ZC853, Z0000, Z0000/
      DATA CODE(  234),CODE(  235),CODE(  236),CODE(  237)/
     *  Z0000, Z005C, Z0000, Z0000/
      DATA CODE(  238),CODE(  239),CODE(  240),CODE(  241)/
     *  Z4269, ZC600, Z0030, Z0000/
      DATA CODE(  242),CODE(  243),CODE(  244),CODE(  245)/
     *  Z1034, Z001A, Z8006, Z0000/
      DATA CODE(  246),CODE(  247),CODE(  248),CODE(  249)/
     *  Z03A4, Z0000, Z0780, Z0000/
      DATA CODE(  250),CODE(  251),CODE(  252),CODE(  253)/
     *  Z20A4, Z0000, Z0000, Z0030/
      DATA CODE(  254),CODE(  255),CODE(  256),CODE(  257)/
     *  Z3269, Z8000, Z0000, Z0000/
      DATA CODE(  258),CODE(  259),CODE(  260),CODE(  261)/
     *  Z2268, Z0192, Z2000, ZC070/
      DATA CODE(  262),CODE(  263),CODE(  264),CODE(  265)/
     *  Z0001, ZC852, Z0000, Z0000/
      DATA CODE(  266),CODE(  267),CODE(  268),CODE(  269)/
     *  Z0001, ZC600, Z0030, Z0000/
      DATA CODE(  270),CODE(  271),CODE(  272),CODE(  273)/
     *  Z1039, Z8013, Z0000, Z0000/
      DATA CODE(  274),CODE(  275),CODE(  276),CODE(  277)/
     *  Z4264, Z0000, Z0000, Z0070/
      DATA CODE(  278),CODE(  279),CODE(  280),CODE(  281)/
     *  Z0000, Z00E0, Z0000, Z0000/
      DATA CODE(  282),CODE(  283),CODE(  284),CODE(  285)/
     *  Z0003, ZE800, Z0000, Z0000/
      DATA CODE(  286),CODE(  287),CODE(  288),CODE(  289)/
     *  Z0003, ZEA00, Z0000, Z0000/
      DATA CODE(  290),CODE(  291),CODE(  292),CODE(  293)/
     *  Z0003, ZECE0, Z0000, Z0000/
      DATA CODE(  294),CODE(  295),CODE(  296),CODE(  297)/
     *  Z03BC, Z0000, Z0400, Z0062/
      DATA CODE(  298),CODE(  299),CODE(  300),CODE(  301)/
     *  Z120C, Z0000, Z0000, Z0011/
      DATA CODE(  302),CODE(  303),CODE(  304),CODE(  305)/
     *  Z4088, Z0000, Z0000, Z0030/
      DATA CODE(  306),CODE(  307),CODE(  308),CODE(  309)/
     *  Z40CC, Z0000, Z0000, Z0030/
      DATA CODE(  310),CODE(  311),CODE(  312),CODE(  313)/
     *  Z0003, Z8600, Z0400, Z1001/
      DATA CODE(  314),CODE(  315),CODE(  316),CODE(  317)/
     *  Z4000, Z0000, Z4A04, Z0030/
      DATA CODE(  318),CODE(  319),CODE(  320),CODE(  321)/
     *  Z4198, Z0000, Z1A00, Z8000/
      DATA CODE(  322),CODE(  323),CODE(  324),CODE(  325)/
     *  Z3150, Z019A, Z0000, Z0000/
      DATA CODE(  326),CODE(  327),CODE(  328),CODE(  329)/
     *  Z2040, ZA800, Z0100, Z1F30/
      DATA CODE(  330),CODE(  331),CODE(  332),CODE(  333)/
     *  Z0001, Z3800, Z0020, Z1F00/
      DATA CODE(  334),CODE(  335),CODE(  336),CODE(  337)/
     *  Z0001, Z8000, Z0000, Z1F00/
      DATA CODE(  338),CODE(  339),CODE(  340),CODE(  341)/
     *  Z2040, ZA954, ZC105, Z1F30/
      DATA CODE(  342),CODE(  343),CODE(  344),CODE(  345)/
     *  Z0299, Z3800, Z4725, Z1F00/
      DATA CODE(  346),CODE(  347),CODE(  348),CODE(  349)/
     *  Z2151, Z81AE, Z0740, Z1FF0/
      DATA CODE(  350),CODE(  351),CODE(  352),CODE(  353)/
     *  Z0000, Z0053, Z0000, Z0000/
      DATA CODE(  354),CODE(  355),CODE(  356),CODE(  357)/
     *  Z0299, Z394E, Z0020, Z1F00/
      DATA CODE(  358),CODE(  359),CODE(  360),CODE(  361)/
     *  Z2151, Z81AB, Z0820, Z1FF0/
      DATA CODE(  362),CODE(  363),CODE(  364),CODE(  365)/
     *  Z0000, Z00E0, Z0000, Z0000/
      DATA CODE(  366),CODE(  367),CODE(  368),CODE(  369)/
     *  Z43FC, Z0000, Z1C00, Z8000/
      DATA CODE(  370),CODE(  371),CODE(  372),CODE(  373)/
     *  Z0040, Z0000, Z1404, Z021A/
      DATA CODE(  374),CODE(  375),CODE(  376),CODE(  377)/
     *  Z4000, Z0000, Z0000, Z0030/
      DATA CODE(  378),CODE(  379),CODE(  380),CODE(  381)/
     *  Z407F, Z8600, Z0400, Z1001/
      DATA CODE(  382),CODE(  383),CODE(  384),CODE(  385)/
     *  Z03B8, Z0000, Z0400, Z001B/
      DATA CODE(  386),CODE(  387),CODE(  388),CODE(  389)/
     *  Z43FC, Z0000, Z4C04, Z0000/
      DATA CODE(  390),CODE(  391),CODE(  392),CODE(  393)/
     *  Z43B9, ZDC00, Z0100, Z0000/
      DATA CODE(  394),CODE(  395),CODE(  396),CODE(  397)/
     *  Z4045, Z8000, Z0000, Z0000/
      DATA CODE(  398),CODE(  399),CODE(  400),CODE(  401)/
     *  Z023C, ZC394, Z0000, Z0000/
      DATA CODE(  402),CODE(  403),CODE(  404),CODE(  405)/
     *  Z0381, Z8000, Z0A00, Z0010/
      DATA CODE(  406),CODE(  407),CODE(  408),CODE(  409)/
     *  Z027C, ZC200, Z4C04, Z0000/
      DATA CODE(  410),CODE(  411),CODE(  412),CODE(  413)/
     *  Z0003, Z832E, Z0700, Z0000/
      DATA CODE(  414),CODE(  415),CODE(  416),CODE(  417)/
     *  Z12CC, Z0000, Z4004, Z000E/
      DATA CODE(  418),CODE(  419),CODE(  420),CODE(  421)/
     *  Z03BC, Z0000, Z0400, Z001A/
      DATA CODE(  422),CODE(  423),CODE(  424),CODE(  425)/
     *  Z43FE, ZBC00, Z0120, Z0000/
      DATA CODE(  426),CODE(  427),CODE(  428),CODE(  429)/
     *  Z0001, Z8000, Z0000, Z0000/
      DATA CODE(  430),CODE(  431),CODE(  432),CODE(  433)/
     *  Z0000, Z0000, Z2000, Z8000/
      DATA CODE(  434),CODE(  435),CODE(  436),CODE(  437)/
     *  Z037C, Z0118, Z0820, Z0000/
      DATA CODE(  438),CODE(  439),CODE(  440),CODE(  441)/
     *  Z43FE, ZBC00, Z4D25, Z0000/
      DATA CODE(  442),CODE(  443),CODE(  444),CODE(  445)/
     *  Z0003, Z8200, Z0740, Z0000/
      DATA CODE(  446),CODE(  447),CODE(  448),CODE(  449)/
     *  Z0381, Z8000, Z0000, Z0030/
      DATA CODE(  450),CODE(  451),CODE(  452),CODE(  453)/
     *  Z0000, Z0000, Z2000, Z8000/
      DATA CODE(  454),CODE(  455),CODE(  456),CODE(  457)/
     *  Z0003, Z8200, Z0740, Z0000/
      DATA CODE(  458),CODE(  459),CODE(  460),CODE(  461)/
     *  Z0380, Z0000, Z0A00, Z0000/
      DATA CODE(  462),CODE(  463),CODE(  464),CODE(  465)/
     *  Z037C, Z012A, Z0820, Z0000/
      DATA CODE(  466),CODE(  467),CODE(  468),CODE(  469)/
     *  Z0000, Z00E0, Z0000, Z0000/
      DATA CODE(  470),CODE(  471),CODE(  472),CODE(  473)/
     *  Z0000, Z0000, Z03FF, ZFFFF/
      DATA CODE(  474),CODE(  475),CODE(  476),CODE(  477)/
     *  Z03BC, Z5400, Z0500, Z1002/
      DATA CODE(  478),CODE(  479),CODE(  480),CODE(  481)/
     *  Z0001, Z8000, Z1820, Z8000/
      DATA CODE(  482),CODE(  483),CODE(  484),CODE(  485)/
     *  Z03B7, Z8600, Z8404, Z1000/
      DATA CODE(  486),CODE(  487),CODE(  488),CODE(  489)/
     *  Z03F8, Z0119, Z0700, Z0000/
      DATA CODE(  490),CODE(  491),CODE(  492),CODE(  493)/
     *  Z23BC, Z0000, Z0000, Z0003/
      DATA CODE(  494),CODE(  495),CODE(  496),CODE(  497)/
     *  Z023E, ZCC00, Z0100, Z0000/
      DATA CODE(  498),CODE(  499),CODE(  500),CODE(  501)/
     *  Z0001, Z8000, Z0020, Z1E00/
      DATA CODE(  502),CODE(  503),CODE(  504),CODE(  505)/
     *  Z0274, Z0000, Z0000, Z1C03/
      DATA CODE(  506),CODE(  507),CODE(  508),CODE(  509)/
     *  Z0378, Z0000, Z0700, Z1F00/
      DATA CODE(  510),CODE(  511),CODE(  512),CODE(  513)/
     *  Z1010, Z9A1E, Z7E04, Z9F00/
      DATA CODE(  514),CODE(  515),CODE(  516),CODE(  517)/
     *  Z02F8, Z9400, ZC104, Z0000/
      DATA CODE(  518),CODE(  519),CODE(  520),CODE(  521)/
     *  Z027D, Z8055, Z0100, Z1500/
      DATA CODE(  522),CODE(  523),CODE(  524),CODE(  525)/
     *  Z033F, Z9C00, Z4404, Z03FF/
      DATA CODE(  526),CODE(  527),CODE(  528),CODE(  529)/
     *  Z0040, Z0000, Z1700, Z8000/
      DATA CODE(  530),CODE(  531),CODE(  532),CODE(  533)/
     *  Z03B8, Z0000, Z0400, Z2000/
      DATA CODE(  534),CODE(  535),CODE(  536),CODE(  537)/
     *  Z63BF, Z8C5A, Z0D20, Z1600/
      DATA CODE(  538),CODE(  539),CODE(  540),CODE(  541)/
     *  Z0278, Z0000, Z4C25, Z1800/
      DATA CODE(  542),CODE(  543),CODE(  544),CODE(  545)/
     *  Z027D, Z5E00, Z1F40, Z9F00/
      DATA CODE(  546),CODE(  547),CODE(  548),CODE(  549)/
     *  Z0001, Z9600, Z0020, Z1F00/
      DATA CODE(  550),CODE(  551),CODE(  552),CODE(  553)/
     *  Z0001, Z8000, Z0000, Z1000/
      DATA CODE(  554),CODE(  555),CODE(  556),CODE(  557)/
     *  Z0000, Z0000, Z0000, Z1F00/
      DATA CODE(  558),CODE(  559),CODE(  560),CODE(  561)/
     *  Z0000, Z0000, Z0000, Z1F00/
      DATA CODE(  562),CODE(  563),CODE(  564),CODE(  565)/
     *  Z0000, Z0053, Z0000, Z1000/
      DATA CODE(  566),CODE(  567),CODE(  568),CODE(  569)/
     *  Z02F8, Z9400, ZC104, Z0000/
      DATA CODE(  570),CODE(  571),CODE(  572),CODE(  573)/
     *  Z027D, Z8048, Z2100, Z9500/
      DATA CODE(  574),CODE(  575),CODE(  576),CODE(  577)/
     *  Z0000, Z0000, Z0000, Z1F00/
      DATA CODE(  578),CODE(  579),CODE(  580),CODE(  581)/
     *  Z0000, Z0000, Z0000, Z1F00/
      DATA CODE(  582),CODE(  583),CODE(  584),CODE(  585)/
     *  Z0000, Z00E0, ZC004, Z0000/
      DATA CODE(  586),CODE(  587),CODE(  588),CODE(  589)/
     *  Z03B8, Z0000, Z0400, Z001B/
      DATA CODE(  590),CODE(  591),CODE(  592),CODE(  593)/
     *  Z43FC, Z0000, Z4C04, Z0000/
      DATA CODE(  594),CODE(  595),CODE(  596),CODE(  597)/
     *  Z43B9, ZDC00, Z0100, Z0000/
      DATA CODE(  598),CODE(  599),CODE(  600),CODE(  601)/
     *  Z0001, Z8000, Z0000, Z0000/
      DATA CODE(  602),CODE(  603),CODE(  604),CODE(  605)/
     *  Z0003, Z86E0, Z8405, Z1001/
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
