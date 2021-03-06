      SUBROUTINE AP1FIN(I1,I2,I3,I4)
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
      INTEGER*2 CODE( 1257)
      INTEGER*2 I 1,J 1
      INTEGER*2 I 2,J 2
      INTEGER*2 I 3,J 3
      INTEGER*2 I 4,J 4
      INTEGER*2 N   5
      INTEGER*2 N   4
      INTEGER*2 SLIST(16)
      COMMON /SPARY/SLIST
      EQUIVALENCE (J 1,SLIST( 1))
      EQUIVALENCE (J 2,SLIST( 2))
      EQUIVALENCE (J 3,SLIST( 3))
      EQUIVALENCE (J 4,SLIST( 4))
      DATA CODE(1) / 314/
      DATA CODE(    2),CODE(    3),CODE(    4),CODE(    5)/
     *  Z0000, Z0000, Z0000, Z0000/
      DATA CODE(    6),CODE(    7),CODE(    8),CODE(    9)/
     *  Z0000, Z0000, Z0000, Z0000/
      DATA CODE(   10),CODE(   11),CODE(   12),CODE(   13)/
     *  Z0000, Z0000, Z0000, Z0000/
      DATA CODE(   14),CODE(   15),CODE(   16),CODE(   17)/
     *  Z0000, Z0000, Z0000, Z0000/
      DATA CODE(   18),CODE(   19),CODE(   20),CODE(   21)/
     *  Z0000, Z0000, Z0000, Z0000/
      DATA CODE(   22),CODE(   23),CODE(   24),CODE(   25)/
     *  Z120C, Z0000, Z0000, Z00F4/
      DATA CODE(   26),CODE(   27),CODE(   28),CODE(   29)/
     *  Z0000, Z0004, Z0000, ZFFFA/
      DATA CODE(   30),CODE(   31),CODE(   32),CODE(   33)/
     *  Z120C, Z0000, Z0000, Z011A/
      DATA CODE(   34),CODE(   35),CODE(   36),CODE(   37)/
     *  Z0400, ZFFF9, Z0400, ZFFF9/
      DATA CODE(   38),CODE(   39),CODE(   40),CODE(   41)/
     *  Z120C, Z0000, Z0000, Z012F/
      DATA CODE(   42),CODE(   43),CODE(   44),CODE(   45)/
     *  Z4024, Z0000, Z0000, Z0000/
      DATA CODE(   46),CODE(   47),CODE(   48),CODE(   49)/
     *  Z120C, Z0000, Z0000, Z0116/
      DATA CODE(   50),CODE(   51),CODE(   52),CODE(   53)/
     *  Z0400, ZFFF5, Z0800, Z0001/
      DATA CODE(   54),CODE(   55),CODE(   56),CODE(   57)/
     *  Z120C, Z0000, Z0000, Z012C/
      DATA CODE(   58),CODE(   59),CODE(   60),CODE(   61)/
     *  Z120C, Z0000, Z0000, Z00FD/
      DATA CODE(   62),CODE(   63),CODE(   64),CODE(   65)/
     *  Z0000, ZFFF5, Z0000, Z0000/
      DATA CODE(   66),CODE(   67),CODE(   68),CODE(   69)/
     *  Z120C, Z0000, Z0000, Z0111/
      DATA CODE(   70),CODE(   71),CODE(   72),CODE(   73)/
     *  Z0000, ZFFEF, Z0000, ZFFF0/
      DATA CODE(   74),CODE(   75),CODE(   76),CODE(   77)/
     *  Z120C, Z0000, Z0000, Z0127/
      DATA CODE(   78),CODE(   79),CODE(   80),CODE(   81)/
     *  Z1004, Z0192, Z0000, Z0000/
      DATA CODE(   82),CODE(   83),CODE(   84),CODE(   85)/
     *  Z1208, Z0000, Z0000, Z0043/
      DATA CODE(   86),CODE(   87),CODE(   88),CODE(   89)/
     *  Z120C, Z0000, Z0000, Z010C/
      DATA CODE(   90),CODE(   91),CODE(   92),CODE(   93)/
     *  Z0400, ZFFEA, Z0800, Z0002/
      DATA CODE(   94),CODE(   95),CODE(   96),CODE(   97)/
     *  Z120C, Z0000, Z0000, Z0121/
      DATA CODE(   98),CODE(   99),CODE(  100),CODE(  101)/
     *  Z120C, Z0000, Z0000, Z00F3/
      DATA CODE(  102),CODE(  103),CODE(  104),CODE(  105)/
     *  Z0000, ZFFE9, Z0000, Z0000/
      DATA CODE(  106),CODE(  107),CODE(  108),CODE(  109)/
     *  Z4240, Z0000, Z0000, Z0000/
      DATA CODE(  110),CODE(  111),CODE(  112),CODE(  113)/
     *  Z4008, Z0000, Z0000, Z0000/
      DATA CODE(  114),CODE(  115),CODE(  116),CODE(  117)/
     *  Z120C, Z0000, Z0000, Z0105/
      DATA CODE(  118),CODE(  119),CODE(  120),CODE(  121)/
     *  Z0000, ZFFE3, Z0000, ZFFE3/
      DATA CODE(  122),CODE(  123),CODE(  124),CODE(  125)/
     *  Z120C, Z0000, Z0000, Z011A/
      DATA CODE(  126),CODE(  127),CODE(  128),CODE(  129)/
     *  Z400C, Z0000, Z0000, Z0000/
      DATA CODE(  130),CODE(  131),CODE(  132),CODE(  133)/
     *  Z4080, Z0000, Z0000, Z0000/
      DATA CODE(  134),CODE(  135),CODE(  136),CODE(  137)/
     *  Z40C4, Z0000, Z0000, Z0000/
      DATA CODE(  138),CODE(  139),CODE(  140),CODE(  141)/
     *  Z120C, Z0000, Z0000, Z00BD/
      DATA CODE(  142),CODE(  143),CODE(  144),CODE(  145)/
     *  Z4014, Z0000, Z0000, Z0000/
      DATA CODE(  146),CODE(  147),CODE(  148),CODE(  149)/
     *  Z4144, Z0000, Z0000, Z0000/
      DATA CODE(  150),CODE(  151),CODE(  152),CODE(  153)/
     *  Z120C, Z0000, Z0000, Z00F1/
      DATA CODE(  154),CODE(  155),CODE(  156),CODE(  157)/
     *  Z0000, Z0001, Z0400, ZFFDA/
      DATA CODE(  158),CODE(  159),CODE(  160),CODE(  161)/
     *  Z120C, Z0000, Z0000, Z0112/
      DATA CODE(  162),CODE(  163),CODE(  164),CODE(  165)/
     *  Z120C, Z0000, Z0000, Z00E3/
      DATA CODE(  166),CODE(  167),CODE(  168),CODE(  169)/
     *  Z0000, ZFFDA, Z0000, Z0000/
      DATA CODE(  170),CODE(  171),CODE(  172),CODE(  173)/
     *  Z120C, Z0000, Z0000, Z00F7/
      DATA CODE(  174),CODE(  175),CODE(  176),CODE(  177)/
     *  Z0000, ZFFD5, Z0800, Z0000/
      DATA CODE(  178),CODE(  179),CODE(  180),CODE(  181)/
     *  Z120C, Z0000, Z0000, Z010D/
      DATA CODE(  182),CODE(  183),CODE(  184),CODE(  185)/
     *  Z1004, Z0192, Z0000, Z0000/
      DATA CODE(  186),CODE(  187),CODE(  188),CODE(  189)/
     *  Z1208, Z0000, Z0000, Z0010/
      DATA CODE(  190),CODE(  191),CODE(  192),CODE(  193)/
     *  Z4084, Z0000, Z0000, Z0000/
      DATA CODE(  194),CODE(  195),CODE(  196),CODE(  197)/
     *  Z120C, Z0000, Z0000, Z00E6/
      DATA CODE(  198),CODE(  199),CODE(  200),CODE(  201)/
     *  Z0000, Z0001, Z0400, ZFFCF/
      DATA CODE(  202),CODE(  203),CODE(  204),CODE(  205)/
     *  Z120C, Z0000, Z0000, Z0107/
      DATA CODE(  206),CODE(  207),CODE(  208),CODE(  209)/
     *  Z120C, Z0000, Z0000, Z00D8/
      DATA CODE(  210),CODE(  211),CODE(  212),CODE(  213)/
     *  Z0000, ZFFCF, Z0000, Z0000/
      DATA CODE(  214),CODE(  215),CODE(  216),CODE(  217)/
     *  Z4080, Z0000, Z0000, Z0000/
      DATA CODE(  218),CODE(  219),CODE(  220),CODE(  221)/
     *  Z120C, Z0000, Z0000, Z00D5/
      DATA CODE(  222),CODE(  223),CODE(  224),CODE(  225)/
     *  Z0400, ZFFCB, Z0000, Z0000/
      DATA CODE(  226),CODE(  227),CODE(  228),CODE(  229)/
     *  Z120C, Z0000, Z0000, Z00DE/
      DATA CODE(  230),CODE(  231),CODE(  232),CODE(  233)/
     *  Z0000, Z0005, Z0400, ZFFC7/
      DATA CODE(  234),CODE(  235),CODE(  236),CODE(  237)/
     *  Z0800, Z0001, Z0000, ZFFC9/
      DATA CODE(  238),CODE(  239),CODE(  240),CODE(  241)/
     *  Z0800, Z0001, Z0400, ZFFC7/
      DATA CODE(  242),CODE(  243),CODE(  244),CODE(  245)/
     *  Z120C, Z0000, Z0000, Z00B1/
      DATA CODE(  246),CODE(  247),CODE(  248),CODE(  249)/
     *  Z120C, Z0000, Z0000, Z0070/
      DATA CODE(  250),CODE(  251),CODE(  252),CODE(  253)/
     *  Z4244, Z0000, Z0000, Z0000/
      DATA CODE(  254),CODE(  255),CODE(  256),CODE(  257)/
     *  Z120C, Z0000, Z0000, Z00D7/
      DATA CODE(  258),CODE(  259),CODE(  260),CODE(  261)/
     *  Z0000, Z0001, Z0000, ZFFC3/
      DATA CODE(  262),CODE(  263),CODE(  264),CODE(  265)/
     *  Z120C, Z0000, Z0000, Z00F7/
      DATA CODE(  266),CODE(  267),CODE(  268),CODE(  269)/
     *  Z120C, Z0000, Z0000, Z00C9/
      DATA CODE(  270),CODE(  271),CODE(  272),CODE(  273)/
     *  Z0400, ZFFC0, Z0000, Z0000/
      DATA CODE(  274),CODE(  275),CODE(  276),CODE(  277)/
     *  Z120C, Z0000, Z0000, Z00DD/
      DATA CODE(  278),CODE(  279),CODE(  280),CODE(  281)/
     *  Z0400, ZFFBE, Z0800, Z0002/
      DATA CODE(  282),CODE(  283),CODE(  284),CODE(  285)/
     *  Z120C, Z0000, Z0000, Z00F3/
      DATA CODE(  286),CODE(  287),CODE(  288),CODE(  289)/
     *  Z120C, Z0000, Z0000, Z00C4/
      DATA CODE(  290),CODE(  291),CODE(  292),CODE(  293)/
     *  Z0400, ZFFBB, Z0000, Z0000/
      DATA CODE(  294),CODE(  295),CODE(  296),CODE(  297)/
     *  Z120C, Z0000, Z0000, Z00CD/
      DATA CODE(  298),CODE(  299),CODE(  300),CODE(  301)/
     *  Z0000, Z0007, Z0000, ZFFB8/
      DATA CODE(  302),CODE(  303),CODE(  304),CODE(  305)/
     *  Z0800, Z0002, Z0400, ZFFB8/
      DATA CODE(  306),CODE(  307),CODE(  308),CODE(  309)/
     *  Z0800, ZFFFE, Z0000, ZFFB6/
      DATA CODE(  310),CODE(  311),CODE(  312),CODE(  313)/
     *  Z0800, Z0002, Z0000, ZFFB7/
      DATA CODE(  314),CODE(  315),CODE(  316),CODE(  317)/
     *  Z120C, Z0000, Z0000, Z009F/
      DATA CODE(  318),CODE(  319),CODE(  320),CODE(  321)/
     *  Z120C, Z0000, Z0000, Z0023/
      DATA CODE(  322),CODE(  323),CODE(  324),CODE(  325)/
     *  Z120C, Z0000, Z0000, Z00C6/
      DATA CODE(  326),CODE(  327),CODE(  328),CODE(  329)/
     *  Z0000, Z0007, Z0400, ZFFAF/
      DATA CODE(  330),CODE(  331),CODE(  332),CODE(  333)/
     *  Z0800, Z0002, Z0000, ZFFB1/
      DATA CODE(  334),CODE(  335),CODE(  336),CODE(  337)/
     *  Z0800, Z0002, Z0400, ZFFAD/
      DATA CODE(  338),CODE(  339),CODE(  340),CODE(  341)/
     *  Z0800, Z0002, Z0800, Z0001/
      DATA CODE(  342),CODE(  343),CODE(  344),CODE(  345)/
     *  Z120C, Z0000, Z0000, Z0098/
      DATA CODE(  346),CODE(  347),CODE(  348),CODE(  349)/
     *  Z120C, Z0000, Z0000, Z001C/
      DATA CODE(  350),CODE(  351),CODE(  352),CODE(  353)/
     *  Z120C, Z0000, Z0000, Z00CA/
      DATA CODE(  354),CODE(  355),CODE(  356),CODE(  357)/
     *  Z0400, ZFFA8, Z0400, ZFFA9/
      DATA CODE(  358),CODE(  359),CODE(  360),CODE(  361)/
     *  Z120C, Z0000, Z0000, Z00DF/
      DATA CODE(  362),CODE(  363),CODE(  364),CODE(  365)/
     *  Z120C, Z0000, Z0000, Z00B1/
      DATA CODE(  366),CODE(  367),CODE(  368),CODE(  369)/
     *  Z0000, ZFFA8, Z0000, Z0000/
      DATA CODE(  370),CODE(  371),CODE(  372),CODE(  373)/
     *  Z120C, Z0000, Z0000, Z00BA/
      DATA CODE(  374),CODE(  375),CODE(  376),CODE(  377)/
     *  Z0000, Z0005, Z0400, ZFFA3/
      DATA CODE(  378),CODE(  379),CODE(  380),CODE(  381)/
     *  Z0800, Z0001, Z0000, ZFFA5/
      DATA CODE(  382),CODE(  383),CODE(  384),CODE(  385)/
     *  Z0800, Z0001, Z0400, ZFFA2/
      DATA CODE(  386),CODE(  387),CODE(  388),CODE(  389)/
     *  Z120C, Z0000, Z0000, Z008D/
      DATA CODE(  390),CODE(  391),CODE(  392),CODE(  393)/
     *  Z120C, Z0000, Z0000, Z0056/
      DATA CODE(  394),CODE(  395),CODE(  396),CODE(  397)/
     *  Z120C, Z0000, Z0000, Z00B4/
      DATA CODE(  398),CODE(  399),CODE(  400),CODE(  401)/
     *  Z0000, Z0007, Z0400, ZFF9D/
      DATA CODE(  402),CODE(  403),CODE(  404),CODE(  405)/
     *  Z0800, Z0002, Z0400, ZFF9C/
      DATA CODE(  406),CODE(  407),CODE(  408),CODE(  409)/
     *  Z0800, Z0002, Z0800, Z0000/
      DATA CODE(  410),CODE(  411),CODE(  412),CODE(  413)/
     *  Z0800, Z0002, Z0400, ZFF9B/
      DATA CODE(  414),CODE(  415),CODE(  416),CODE(  417)/
     *  Z120C, Z0000, Z0000, Z0086/
      DATA CODE(  418),CODE(  419),CODE(  420),CODE(  421)/
     *  Z120C, Z0000, Z0000, Z0022/
      DATA CODE(  422),CODE(  423),CODE(  424),CODE(  425)/
     *  Z120C, Z0000, Z0000, Z00AD/
      DATA CODE(  426),CODE(  427),CODE(  428),CODE(  429)/
     *  Z0000, Z0007, Z0800, Z0000/
      DATA CODE(  430),CODE(  431),CODE(  432),CODE(  433)/
     *  Z0800, Z0002, Z0800, Z0000/
      DATA CODE(  434),CODE(  435),CODE(  436),CODE(  437)/
     *  Z0800, Z0002, Z0800, Z0004/
      DATA CODE(  438),CODE(  439),CODE(  440),CODE(  441)/
     *  Z0800, Z0006, Z0800, Z0001/
      DATA CODE(  442),CODE(  443),CODE(  444),CODE(  445)/
     *  Z120C, Z0000, Z0000, Z007F/
      DATA CODE(  446),CODE(  447),CODE(  448),CODE(  449)/
     *  Z120C, Z0000, Z0000, Z001B/
      DATA CODE(  450),CODE(  451),CODE(  452),CODE(  453)/
     *  Z0000, Z0000, Z0000, Z0000/
      DATA CODE(  454),CODE(  455),CODE(  456),CODE(  457)/
     *  Z0000, Z00E0, Z0000, Z0000/
      DATA CODE(  458),CODE(  459),CODE(  460),CODE(  461)/
     *  Z4198, Z0000, Z0000, Z0000/
      DATA CODE(  462),CODE(  463),CODE(  464),CODE(  465)/
     *  Z3040, Z01F2, Z0000, Z0000/
      DATA CODE(  466),CODE(  467),CODE(  468),CODE(  469)/
     *  Z1208, Z0000, Z0000, Z0015/
      DATA CODE(  470),CODE(  471),CODE(  472),CODE(  473)/
     *  Z30C8, Z0000, Z0000, Z0000/
      DATA CODE(  474),CODE(  475),CODE(  476),CODE(  477)/
     *  Z3150, Z0000, Z0000, Z0000/
      DATA CODE(  478),CODE(  479),CODE(  480),CODE(  481)/
     *  Z2040, Z0000, Z0000, Z0030/
      DATA CODE(  482),CODE(  483),CODE(  484),CODE(  485)/
     *  Z0000, Z0000, Z0000, Z0010/
      DATA CODE(  486),CODE(  487),CODE(  488),CODE(  489)/
     *  Z20C8, Z0000, Z0000, Z0030/
      DATA CODE(  490),CODE(  491),CODE(  492),CODE(  493)/
     *  Z0000, Z0000, Z4A04, Z0010/
      DATA CODE(  494),CODE(  495),CODE(  496),CODE(  497)/
     *  Z0298, Z0000, Z1A00, Z8000/
      DATA CODE(  498),CODE(  499),CODE(  500),CODE(  501)/
     *  Z0001, ZA9F2, Z0100, Z0000/
      DATA CODE(  502),CODE(  503),CODE(  504),CODE(  505)/
     *  Z1208, Z0000, Z0000, Z0009/
      DATA CODE(  506),CODE(  507),CODE(  508),CODE(  509)/
     *  Z2041, Z3800, Z0020, Z0030/
      DATA CODE(  510),CODE(  511),CODE(  512),CODE(  513)/
     *  Z0000, Z0000, Z0000, Z0010/
      DATA CODE(  514),CODE(  515),CODE(  516),CODE(  517)/
     *  Z20C8, Z0000, Z0000, Z0030/
      DATA CODE(  518),CODE(  519),CODE(  520),CODE(  521)/
     *  Z0298, Z0000, Z4A04, Z0010/
      DATA CODE(  522),CODE(  523),CODE(  524),CODE(  525)/
     *  Z2151, Z81F2, Z1A00, Z8070/
      DATA CODE(  526),CODE(  527),CODE(  528),CODE(  529)/
     *  Z0000, Z0052, Z4A04, Z0000/
      DATA CODE(  530),CODE(  531),CODE(  532),CODE(  533)/
     *  Z0001, ZA84A, Z0100, Z0050/
      DATA CODE(  534),CODE(  535),CODE(  536),CODE(  537)/
     *  Z0001, ZB400, Z0120, Z0050/
      DATA CODE(  538),CODE(  539),CODE(  540),CODE(  541)/
     *  Z0001, Z3800, Z0020, Z0000/
      DATA CODE(  542),CODE(  543),CODE(  544),CODE(  545)/
     *  Z2151, Z8000, Z0000, Z0070/
      DATA CODE(  546),CODE(  547),CODE(  548),CODE(  549)/
     *  Z0000, Z0000, Z0000, Z0050/
      DATA CODE(  550),CODE(  551),CODE(  552),CODE(  553)/
     *  Z0000, Z00E0, Z0000, Z0000/
      DATA CODE(  554),CODE(  555),CODE(  556),CODE(  557)/
     *  Z4154, Z0000, Z0000, Z0030/
      DATA CODE(  558),CODE(  559),CODE(  560),CODE(  561)/
     *  Z0000, Z0000, Z0000, Z0000/
      DATA CODE(  562),CODE(  563),CODE(  564),CODE(  565)/
     *  Z0000, Z0000, Z0000, Z0010/
      DATA CODE(  566),CODE(  567),CODE(  568),CODE(  569)/
     *  Z4198, Z0000, Z5A07, ZE000/
      DATA CODE(  570),CODE(  571),CODE(  572),CODE(  573)/
     *  Z4110, Z01F2, Z0000, Z0030/
      DATA CODE(  574),CODE(  575),CODE(  576),CODE(  577)/
     *  Z1208, Z0000, Z0000, Z001B/
      DATA CODE(  578),CODE(  579),CODE(  580),CODE(  581)/
     *  Z0000, Z0000, Z5A06, ZC000/
      DATA CODE(  582),CODE(  583),CODE(  584),CODE(  585)/
     *  Z0000, Z0000, Z0000, Z0010/
      DATA CODE(  586),CODE(  587),CODE(  588),CODE(  589)/
     *  Z0000, Z0000, Z1A00, ZA000/
      DATA CODE(  590),CODE(  591),CODE(  592),CODE(  593)/
     *  Z0000, Z0000, Z0000, Z0000/
      DATA CODE(  594),CODE(  595),CODE(  596),CODE(  597)/
     *  Z0000, Z0000, Z4A3D, Z1B00/
      DATA CODE(  598),CODE(  599),CODE(  600),CODE(  601)/
     *  Z4000, Z0000, Z01A8, Z1930/
      DATA CODE(  602),CODE(  603),CODE(  604),CODE(  605)/
     *  Z30C8, Z0000, Z01E8, Z1900/
      DATA CODE(  606),CODE(  607),CODE(  608),CODE(  609)/
     *  Z0000, Z0000, ZC173, Z1610/
      DATA CODE(  610),CODE(  611),CODE(  612),CODE(  613)/
     *  Z0001, Z9400, Z00E8, Z1B00/
      DATA CODE(  614),CODE(  615),CODE(  616),CODE(  617)/
     *  Z0001, Z8000, Z3140, Z7700/
      DATA CODE(  618),CODE(  619),CODE(  620),CODE(  621)/
     *  Z0000, Z9600, Z815D, Z1700/
      DATA CODE(  622),CODE(  623),CODE(  624),CODE(  625)/
     *  Z0299, Z9A00, Z0028, Z1B00/
      DATA CODE(  626),CODE(  627),CODE(  628),CODE(  629)/
     *  Z0001, Z9BF2, Z2178, ZB600/
      DATA CODE(  630),CODE(  631),CODE(  632),CODE(  633)/
     *  Z1208, Z0000, Z0000, Z0009/
      DATA CODE(  634),CODE(  635),CODE(  636),CODE(  637)/
     *  Z2040, Z9200, Z01A8, Z1930/
      DATA CODE(  638),CODE(  639),CODE(  640),CODE(  641)/
     *  Z0001, Z9200, Z01E8, Z1900/
      DATA CODE(  642),CODE(  643),CODE(  644),CODE(  645)/
     *  Z0000, Z0000, ZC173, Z1610/
      DATA CODE(  646),CODE(  647),CODE(  648),CODE(  649)/
     *  Z0001, Z9400, Z20E8, Z7B00/
      DATA CODE(  650),CODE(  651),CODE(  652),CODE(  653)/
     *  Z20C9, ZD200, Z3958, Z77F0/
      DATA CODE(  654),CODE(  655),CODE(  656),CODE(  657)/
     *  Z0000, Z9600, Z815D, Z1700/
      DATA CODE(  658),CODE(  659),CODE(  660),CODE(  661)/
     *  Z0299, Z9A00, Z0028, Z1B50/
      DATA CODE(  662),CODE(  663),CODE(  664),CODE(  665)/
     *  Z0001, Z9BE9, Z2178, ZB600/
      DATA CODE(  666),CODE(  667),CODE(  668),CODE(  669)/
     *  Z0000, Z9200, Z0000, Z1F00/
      DATA CODE(  670),CODE(  671),CODE(  672),CODE(  673)/
     *  Z0001, Z9200, Z0000, Z0000/
      DATA CODE(  674),CODE(  675),CODE(  676),CODE(  677)/
     *  Z4001, Z8000, Z0000, Z0070/
      DATA CODE(  678),CODE(  679),CODE(  680),CODE(  681)/
     *  Z0000, Z0000, Z0000, Z0050/
      DATA CODE(  682),CODE(  683),CODE(  684),CODE(  685)/
     *  Z0000, Z00E0, Z0000, Z0000/
      DATA CODE(  686),CODE(  687),CODE(  688),CODE(  689)/
     *  Z03BC, Z0000, Z0400, Z001A/
      DATA CODE(  690),CODE(  691),CODE(  692),CODE(  693)/
     *  Z120C, Z0000, Z0000, Z0018/
      DATA CODE(  694),CODE(  695),CODE(  696),CODE(  697)/
     *  Z413C, Z0000, Z0000, Z0000/
      DATA CODE(  698),CODE(  699),CODE(  700),CODE(  701)/
     *  Z120C, Z0000, Z0000, Z0043/
      DATA CODE(  702),CODE(  703),CODE(  704),CODE(  705)/
     *  Z4000, Z0115, Z0000, Z0030/
      DATA CODE(  706),CODE(  707),CODE(  708),CODE(  709)/
     *  Z30C8, ZC400, Z0140, Z0000/
      DATA CODE(  710),CODE(  711),CODE(  712),CODE(  713)/
     *  Z0001, Z8000, Z0000, Z0000/
      DATA CODE(  714),CODE(  715),CODE(  716),CODE(  717)/
     *  Z2040, ZC200, Z0000, Z0030/
      DATA CODE(  718),CODE(  719),CODE(  720),CODE(  721)/
     *  Z20C9, Z816F, Z0A00, Z00F0/
      DATA CODE(  722),CODE(  723),CODE(  724),CODE(  725)/
     *  Z0000, Z00E0, Z0000, Z0000/
      DATA CODE(  726),CODE(  727),CODE(  728),CODE(  729)/
     *  Z03BC, Z0000, Z0400, Z001A/
      DATA CODE(  730),CODE(  731),CODE(  732),CODE(  733)/
     *  Z120C, Z0000, Z0000, Z000E/
      DATA CODE(  734),CODE(  735),CODE(  736),CODE(  737)/
     *  Z413C, Z0000, Z0000, Z0000/
      DATA CODE(  738),CODE(  739),CODE(  740),CODE(  741)/
     *  Z120C, Z0000, Z0000, Z0039/
      DATA CODE(  742),CODE(  743),CODE(  744),CODE(  745)/
     *  Z4000, Z011A, Z0000, Z0030/
      DATA CODE(  746),CODE(  747),CODE(  748),CODE(  749)/
     *  Z4088, Z0000, Z0000, Z0030/
      DATA CODE(  750),CODE(  751),CODE(  752),CODE(  753)/
     *  Z0000, Z0000, Z0000, Z0000/
      DATA CODE(  754),CODE(  755),CODE(  756),CODE(  757)/
     *  Z0001, ZD800, Z0000, Z0000/
      DATA CODE(  758),CODE(  759),CODE(  760),CODE(  761)/
     *  Z2041, ZD800, Z0000, Z0030/
      DATA CODE(  762),CODE(  763),CODE(  764),CODE(  765)/
     *  Z20C9, ZD200, Z0000, Z0030/
      DATA CODE(  766),CODE(  767),CODE(  768),CODE(  769)/
     *  Z3040, ZC420, Z0140, Z0070/
      DATA CODE(  770),CODE(  771),CODE(  772),CODE(  773)/
     *  Z30C9, ZD820, Z0000, Z0070/
      DATA CODE(  774),CODE(  775),CODE(  776),CODE(  777)/
     *  Z2041, ZD800, Z8005, Z0030/
      DATA CODE(  778),CODE(  779),CODE(  780),CODE(  781)/
     *  Z20C9, ZD36D, Z0000, Z0030/
      DATA CODE(  782),CODE(  783),CODE(  784),CODE(  785)/
     *  Z0000, Z00E0, Z0000, Z0000/
      DATA CODE(  786),CODE(  787),CODE(  788),CODE(  789)/
     *  Z43FC, Z0000, Z1C00, Z8000/
      DATA CODE(  790),CODE(  791),CODE(  792),CODE(  793)/
     *  Z0040, Z0000, Z1404, Z021A/
      DATA CODE(  794),CODE(  795),CODE(  796),CODE(  797)/
     *  Z4000, Z0000, Z0000, Z0030/
      DATA CODE(  798),CODE(  799),CODE(  800),CODE(  801)/
     *  Z407F, Z8600, Z0400, Z1001/
      DATA CODE(  802),CODE(  803),CODE(  804),CODE(  805)/
     *  Z03B8, Z0000, Z0400, Z001B/
      DATA CODE(  806),CODE(  807),CODE(  808),CODE(  809)/
     *  Z43FC, Z0000, Z4C04, Z0000/
      DATA CODE(  810),CODE(  811),CODE(  812),CODE(  813)/
     *  Z43B9, ZDC00, Z0100, Z0000/
      DATA CODE(  814),CODE(  815),CODE(  816),CODE(  817)/
     *  Z4045, Z8000, Z0000, Z0000/
      DATA CODE(  818),CODE(  819),CODE(  820),CODE(  821)/
     *  Z023C, ZC394, Z0000, Z0000/
      DATA CODE(  822),CODE(  823),CODE(  824),CODE(  825)/
     *  Z0381, Z8000, Z0A00, Z0010/
      DATA CODE(  826),CODE(  827),CODE(  828),CODE(  829)/
     *  Z027C, ZC200, Z4C04, Z0000/
      DATA CODE(  830),CODE(  831),CODE(  832),CODE(  833)/
     *  Z0003, Z832E, Z0700, Z0000/
      DATA CODE(  834),CODE(  835),CODE(  836),CODE(  837)/
     *  Z12CC, Z0000, Z4004, Z000E/
      DATA CODE(  838),CODE(  839),CODE(  840),CODE(  841)/
     *  Z03BC, Z0000, Z0400, Z001A/
      DATA CODE(  842),CODE(  843),CODE(  844),CODE(  845)/
     *  Z43FE, ZBC00, Z0120, Z0000/
      DATA CODE(  846),CODE(  847),CODE(  848),CODE(  849)/
     *  Z0001, Z8000, Z0000, Z0000/
      DATA CODE(  850),CODE(  851),CODE(  852),CODE(  853)/
     *  Z0000, Z0000, Z2000, Z8000/
      DATA CODE(  854),CODE(  855),CODE(  856),CODE(  857)/
     *  Z037C, Z0118, Z0820, Z0000/
      DATA CODE(  858),CODE(  859),CODE(  860),CODE(  861)/
     *  Z43FE, ZBC00, Z4D25, Z0000/
      DATA CODE(  862),CODE(  863),CODE(  864),CODE(  865)/
     *  Z0003, Z8200, Z0740, Z0000/
      DATA CODE(  866),CODE(  867),CODE(  868),CODE(  869)/
     *  Z0381, Z8000, Z0000, Z0030/
      DATA CODE(  870),CODE(  871),CODE(  872),CODE(  873)/
     *  Z0000, Z0000, Z2000, Z8000/
      DATA CODE(  874),CODE(  875),CODE(  876),CODE(  877)/
     *  Z0003, Z8200, Z0740, Z0000/
      DATA CODE(  878),CODE(  879),CODE(  880),CODE(  881)/
     *  Z0380, Z0000, Z0A00, Z0000/
      DATA CODE(  882),CODE(  883),CODE(  884),CODE(  885)/
     *  Z037C, Z012A, Z0820, Z0000/
      DATA CODE(  886),CODE(  887),CODE(  888),CODE(  889)/
     *  Z0000, Z00E0, Z0000, Z0000/
      DATA CODE(  890),CODE(  891),CODE(  892),CODE(  893)/
     *  Z0000, Z0000, Z03FF, ZFFFF/
      DATA CODE(  894),CODE(  895),CODE(  896),CODE(  897)/
     *  Z4000, Z0000, Z4C04, Z0000/
      DATA CODE(  898),CODE(  899),CODE(  900),CODE(  901)/
     *  Z0380, Z0000, Z0400, Z001B/
      DATA CODE(  902),CODE(  903),CODE(  904),CODE(  905)/
     *  Z4001, ZDC00, Z0100, Z0000/
      DATA CODE(  906),CODE(  907),CODE(  908),CODE(  909)/
     *  Z4045, Z8000, Z4C04, Z0000/
      DATA CODE(  910),CODE(  911),CODE(  912),CODE(  913)/
     *  Z4001, ZDC00, Z8104, Z0000/
      DATA CODE(  914),CODE(  915),CODE(  916),CODE(  917)/
     *  Z0001, Z8000, Z0000, Z0000/
      DATA CODE(  918),CODE(  919),CODE(  920),CODE(  921)/
     *  Z0000, Z0000, Z0100, Z1400/
      DATA CODE(  922),CODE(  923),CODE(  924),CODE(  925)/
     *  Z0000, Z0000, Z0000, Z1F00/
      DATA CODE(  926),CODE(  927),CODE(  928),CODE(  929)/
     *  Z0000, Z0000, Z0000, Z1F00/
      DATA CODE(  930),CODE(  931),CODE(  932),CODE(  933)/
     *  Z0000, Z0000, ZC004, Z0000/
      DATA CODE(  934),CODE(  935),CODE(  936),CODE(  937)/
     *  Z0000, Z1400, Z0100, Z0000/
      DATA CODE(  938),CODE(  939),CODE(  940),CODE(  941)/
     *  Z0001, Z8000, Z0000, Z0000/
      DATA CODE(  942),CODE(  943),CODE(  944),CODE(  945)/
     *  Z0000, Z0000, Z8004, Z0000/
      DATA CODE(  946),CODE(  947),CODE(  948),CODE(  949)/
     *  Z0380, Z00E0, Z0700, Z0000/
      DATA CODE(  950),CODE(  951),CODE(  952),CODE(  953)/
     *  Z0000, Z0000, Z4004, Z0000/
      DATA CODE(  954),CODE(  955),CODE(  956),CODE(  957)/
     *  Z0001, ZA400, Z0100, Z1500/
      DATA CODE(  958),CODE(  959),CODE(  960),CODE(  961)/
     *  Z0001, ZA400, Z0100, Z1500/
      DATA CODE(  962),CODE(  963),CODE(  964),CODE(  965)/
     *  Z0003, Z8CE0, Z0100, Z1500/
      DATA CODE(  966),CODE(  967),CODE(  968),CODE(  969)/
     *  Z43FC, Z0000, Z4C05, Z0000/
      DATA CODE(  970),CODE(  971),CODE(  972),CODE(  973)/
     *  Z03BC, Z01D3, Z0400, Z001B/
      DATA CODE(  974),CODE(  975),CODE(  976),CODE(  977)/
     *  Z12CC, Z0000, Z1004, Z0005/
      DATA CODE(  978),CODE(  979),CODE(  980),CODE(  981)/
     *  Z43FD, ZBC52, Z0160, Z0000/
      DATA CODE(  982),CODE(  983),CODE(  984),CODE(  985)/
     *  Z43FD, ZDC00, Z0140, Z0000/
      DATA CODE(  986),CODE(  987),CODE(  988),CODE(  989)/
     *  Z0001, Z8000, Z0000, Z0000/
      DATA CODE(  990),CODE(  991),CODE(  992),CODE(  993)/
     *  Z0003, Z86E0, Z8405, Z1001/
      DATA CODE(  994),CODE(  995),CODE(  996),CODE(  997)/
     *  Z0000, Z0021, Z1400, Z0000/
      DATA CODE(  998),CODE(  999),CODE( 1000),CODE( 1001)/
     *  Z033F, ZB000, Z0000, Z0000/
      DATA CODE( 1002),CODE( 1003),CODE( 1004),CODE( 1005)/
     *  Z43FC, Z0000, Z0000, Z0003/
      DATA CODE( 1006),CODE( 1007),CODE( 1008),CODE( 1009)/
     *  Z12D0, Z0000, Z4004, Z0000/
      DATA CODE( 1010),CODE( 1011),CODE( 1012),CODE( 1013)/
     *  Z03B4, Z0000, Z0700, Z0000/
      DATA CODE( 1014),CODE( 1015),CODE( 1016),CODE( 1017)/
     *  Z12D4, Z0000, Z4004, Z0001/
      DATA CODE( 1018),CODE( 1019),CODE( 1020),CODE( 1021)/
     *  Z03B8, Z0000, Z0700, Z0000/
      DATA CODE( 1022),CODE( 1023),CODE( 1024),CODE( 1025)/
     *  Z1314, Z0000, Z0000, Z0000/
      DATA CODE( 1026),CODE( 1027),CODE( 1028),CODE( 1029)/
     *  Z23BC, Z0000, Z0000, Z0003/
      DATA CODE( 1030),CODE( 1031),CODE( 1032),CODE( 1033)/
     *  Z03B8, Z0000, Z0400, ZFFFF/
      DATA CODE( 1034),CODE( 1035),CODE( 1036),CODE( 1037)/
     *  Z0E74, Z0000, Z0000, Z0000/
      DATA CODE( 1038),CODE( 1039),CODE( 1040),CODE( 1041)/
     *  Z027B, Z8397, Z0C00, Z0000/
      DATA CODE( 1042),CODE( 1043),CODE( 1044),CODE( 1045)/
     *  Z0380, Z0000, Z4C04, Z0000/
      DATA CODE( 1046),CODE( 1047),CODE( 1048),CODE( 1049)/
     *  Z12F0, Z0000, Z0700, Z0000/
      DATA CODE( 1050),CODE( 1051),CODE( 1052),CODE( 1053)/
     *  Z027B, Z8200, Z0C00, Z0000/
      DATA CODE( 1054),CODE( 1055),CODE( 1056),CODE( 1057)/
     *  Z0380, Z0000, Z4C04, Z0000/
      DATA CODE( 1058),CODE( 1059),CODE( 1060),CODE( 1061)/
     *  Z12F4, Z0000, Z0700, Z0001/
      DATA CODE( 1062),CODE( 1063),CODE( 1064),CODE( 1065)/
     *  Z02B4, Z004A, Z0000, Z0000/
      DATA CODE( 1066),CODE( 1067),CODE( 1068),CODE( 1069)/
     *  Z0000, Z00E0, Z0000, Z0000/
      DATA CODE( 1070),CODE( 1071),CODE( 1072),CODE( 1073)/
     *  Z4000, Z0000, Z1C00, Z8000/
      DATA CODE( 1074),CODE( 1075),CODE( 1076),CODE( 1077)/
     *  Z033F, ZB000, Z0000, Z0000/
      DATA CODE( 1078),CODE( 1079),CODE( 1080),CODE( 1081)/
     *  Z43FC, Z0000, Z0000, Z0003/
      DATA CODE( 1082),CODE( 1083),CODE( 1084),CODE( 1085)/
     *  Z12D0, Z0000, Z4004, Z0000/
      DATA CODE( 1086),CODE( 1087),CODE( 1088),CODE( 1089)/
     *  Z03B8, Z0000, Z0700, Z0001/
      DATA CODE( 1090),CODE( 1091),CODE( 1092),CODE( 1093)/
     *  Z1314, Z0000, Z0000, Z0000/
      DATA CODE( 1094),CODE( 1095),CODE( 1096),CODE( 1097)/
     *  Z23F8, Z0000, Z0700, Z0003/
      DATA CODE( 1098),CODE( 1099),CODE( 1100),CODE( 1101)/
     *  Z1014, Z0012, Z0000, Z0000/
      DATA CODE( 1102),CODE( 1103),CODE( 1104),CODE( 1105)/
     *  Z12F4, Z0052, Z0820, Z0000/
      DATA CODE( 1106),CODE( 1107),CODE( 1108),CODE( 1109)/
     *  Z12F0, Z0000, Z0820, Z0000/
      DATA CODE( 1110),CODE( 1111),CODE( 1112),CODE( 1113)/
     *  Z0000, Z00E0, Z0000, Z0000/
      DATA CODE( 1114),CODE( 1115),CODE( 1116),CODE( 1117)/
     *  Z033F, ZB000, Z0000, Z0000/
      DATA CODE( 1118),CODE( 1119),CODE( 1120),CODE( 1121)/
     *  Z43FC, Z0000, Z0000, Z0003/
      DATA CODE( 1122),CODE( 1123),CODE( 1124),CODE( 1125)/
     *  Z12D0, Z0000, Z4004, Z0000/
      DATA CODE( 1126),CODE( 1127),CODE( 1128),CODE( 1129)/
     *  Z03B4, Z0000, Z0700, Z0000/
      DATA CODE( 1130),CODE( 1131),CODE( 1132),CODE( 1133)/
     *  Z03F8, Z0000, Z0700, Z0000/
      DATA CODE( 1134),CODE( 1135),CODE( 1136),CODE( 1137)/
     *  Z4F70, Z0000, Z0000, Z0000/
      DATA CODE( 1138),CODE( 1139),CODE( 1140),CODE( 1141)/
     *  Z0270, Z0000, Z0000, Z0000/
      DATA CODE( 1142),CODE( 1143),CODE( 1144),CODE( 1145)/
     *  Z23F0, Z0000, Z0000, Z0003/
      DATA CODE( 1146),CODE( 1147),CODE( 1148),CODE( 1149)/
     *  Z1314, Z0000, Z0000, Z0000/
      DATA CODE( 1150),CODE( 1151),CODE( 1152),CODE( 1153)/
     *  Z02B8, Z0000, Z0000, Z0000/
      DATA CODE( 1154),CODE( 1155),CODE( 1156),CODE( 1157)/
     *  Z4374, Z005D, Z0000, Z0000/
      DATA CODE( 1158),CODE( 1159),CODE( 1160),CODE( 1161)/
     *  Z033F, ZB000, Z0000, Z0000/
      DATA CODE( 1162),CODE( 1163),CODE( 1164),CODE( 1165)/
     *  Z027C, Z0020, Z0000, Z0003/
      DATA CODE( 1166),CODE( 1167),CODE( 1168),CODE( 1169)/
     *  Z03B4, Z0000, Z0400, Z0002/
      DATA CODE( 1170),CODE( 1171),CODE( 1172),CODE( 1173)/
     *  Z03B8, Z0000, Z0400, ZFFFF/
      DATA CODE( 1174),CODE( 1175),CODE( 1176),CODE( 1177)/
     *  Z1314, Z0055, Z0000, Z0002/
      DATA CODE( 1178),CODE( 1179),CODE( 1180),CODE( 1181)/
     *  Z12D4, Z0000, Z4004, Z0000/
      DATA CODE( 1182),CODE( 1183),CODE( 1184),CODE( 1185)/
     *  Z0000, Z0000, Z0000, Z0000/
      DATA CODE( 1186),CODE( 1187),CODE( 1188),CODE( 1189)/
     *  Z120C, Z0000, Z0700, Z0007/
      DATA CODE( 1190),CODE( 1191),CODE( 1192),CODE( 1193)/
     *  Z027C, Z0195, Z0000, Z0003/
      DATA CODE( 1194),CODE( 1195),CODE( 1196),CODE( 1197)/
     *  Z12D0, Z0000, Z4004, Z0000/
      DATA CODE( 1198),CODE( 1199),CODE( 1200),CODE( 1201)/
     *  Z0000, Z0000, Z0000, Z0000/
      DATA CODE( 1202),CODE( 1203),CODE( 1204),CODE( 1205)/
     *  Z120C, Z0000, Z0700, Z0003/
      DATA CODE( 1206),CODE( 1207),CODE( 1208),CODE( 1209)/
     *  Z43FC, Z01A9, Z0000, Z0003/
      DATA CODE( 1210),CODE( 1211),CODE( 1212),CODE( 1213)/
     *  Z0000, Z00E0, Z0000, Z0000/
      DATA CODE( 1214),CODE( 1215),CODE( 1216),CODE( 1217)/
     *  Z1010, Z0016, Z0000, Z0000/
      DATA CODE( 1218),CODE( 1219),CODE( 1220),CODE( 1221)/
     *  Z03B0, Z0000, Z0700, Z0000/
      DATA CODE( 1222),CODE( 1223),CODE( 1224),CODE( 1225)/
     *  Z23F0, Z0000, Z0700, Z0003/
      DATA CODE( 1226),CODE( 1227),CODE( 1228),CODE( 1229)/
     *  Z1014, Z0012, Z0000, Z0000/
      DATA CODE( 1230),CODE( 1231),CODE( 1232),CODE( 1233)/
     *  Z12D4, Z0052, Z4004, Z0000/
      DATA CODE( 1234),CODE( 1235),CODE( 1236),CODE( 1237)/
     *  Z12D0, Z0000, Z4004, Z0000/
      DATA CODE( 1238),CODE( 1239),CODE( 1240),CODE( 1241)/
     *  Z027B, Z8200, Z0C00, Z0000/
      DATA CODE( 1242),CODE( 1243),CODE( 1244),CODE( 1245)/
     *  Z0380, Z0000, Z0700, Z0000/
      DATA CODE( 1246),CODE( 1247),CODE( 1248),CODE( 1249)/
     *  Z02B4, Z00E0, Z0000, Z0000/
      DATA CODE( 1250),CODE( 1251),CODE( 1252),CODE( 1253)/
     *  Z2040, Z00E0, Z0000, Z0000/
      DATA CODE( 1254),CODE( 1255),CODE( 1256),CODE( 1257)/
     *  Z3040, Z00E0, Z0000, Z0000/
      DATA N   5 /   5/
      DATA N   4 /   4/
C-----------------------------------------------------------------------
      J 1 = I 1
      J 2 = I 2
      J 3 = I 3
      J 4 = I 4
      CALL APEX (CODE, N   5, SLIST, N   4)
      RETURN
      END
