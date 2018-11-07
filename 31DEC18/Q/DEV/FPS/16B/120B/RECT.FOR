      SUBROUTINE RECT (I1, I2, I3, I4, I5)
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
C     "PATCHED UP" VERSION OF FPS ROUTINE.
C-----------------------------------------------------------------------
      INTEGER*2 CODE(  317)
      INTEGER*2 I1, J1
      INTEGER*2 I2, J2
      INTEGER*2 I3, J3
      INTEGER*2 I4, J4
      INTEGER*2 I5, J5
      INTEGER*2 SLIST(16)
      COMMON /SPARY/ SLIST
      EQUIVALENCE (J1, SLIST( 1))
      EQUIVALENCE (J2, SLIST( 2))
      EQUIVALENCE (J3, SLIST( 3))
      EQUIVALENCE (J4, SLIST( 4))
      EQUIVALENCE (J5, SLIST( 5))
      DATA CODE(1) /  79/
      DATA CODE( 2), CODE( 3), CODE( 4), CODE( 5)/
     *   Z03BC, Z0000, Z0400, Z001A/
      DATA CODE( 6), CODE( 7), CODE( 8), CODE( 9)/
     *   Z120C, Z0000, Z0000, Z0033/
      DATA CODE(10), CODE(11), CODE(12), CODE(13)/
     *   Z4030, Z0000, Z0000, Z0000/
      DATA CODE(14), CODE(15), CODE(16), CODE(17)/
     *   Z0240, Z0000, Z0000, Z0030/
      DATA CODE(18), CODE(19), CODE(20), CODE(21)/
     *   Z12CC, Z0000, Z4003, Z002F/
      DATA CODE(22), CODE(23), CODE(24), CODE(25)/
     *   Z0003, Z8600, Z0400, Z0801/
      DATA CODE(26), CODE(27), CODE(28), CODE(29)/
     *   Z12CC, Z0000, Z1003, Z002A/
      DATA CODE(30), CODE(31), CODE(32), CODE(33)/
     *   Z12CC, Z0000, Z4000, Z002A/
      DATA CODE(34), CODE(35), CODE(36), CODE(37)/
     *   Z0248, Z0000, Z4E02, Z0000/
      DATA CODE(38), CODE(39), CODE(40), CODE(41)/
     *   Z30C8, Z0000, Z0000, Z0000/
      DATA CODE(42), CODE(43), CODE(44), CODE(45)/
     *   Z12CC, Z0000, Z4001, Z0028/
      DATA CODE(46), CODE(47), CODE(48), CODE(49)/
     *   Z033F, Z9C00, Z00C0, Z1700/
      DATA CODE(50), CODE(51), CODE(52), CODE(53)/
     *   Z3070, Z0000, Z0000, Z1F00/
      DATA CODE(54), CODE(55), CODE(56), CODE(57)/
     *   Z03B8, Z0000, Z0400, Z0018/
      DATA CODE(58), CODE(59), CODE(60), CODE(61)/
     *   Z63BF, Z8C00, Z0C00, Z1F00/
      DATA CODE(62), CODE(63), CODE(64), CODE(65)/
     *   Z03B4, Z0000, ZC404, Z0003/
      DATA CODE(66), CODE(67), CODE(68), CODE(69)/
     *   Z0000, Z0000, Z1700, Z4000/
      DATA CODE(70), CODE(71), CODE(72), CODE(73)/
     *   Z0279, Z9400, Z68AE, Z0003/
      DATA CODE(74), CODE(75), CODE(76), CODE(77)/
     *   Z03BC, Z0000, Z29E0, ZBD00/
      DATA CODE(78), CODE(79), CODE(80), CODE(81)/
     *   Z0000, Z2400, Z3128, ZDA00/
      DATA CODE(82), CODE(83), CODE(84), CODE(85)/
     *   Z02B8, Z0000, Z0180, Z1603/
      DATA CODE(86), CODE(87), CODE(88), CODE(89)/
     *   Z47F9, Z9A00, Z9E07, Z1C00/
      DATA CODE(90), CODE(91), CODE(92), CODE(93)/
     *   Z2041, ZA600, Z6B76, Z8030/
      DATA CODE(94), CODE(95), CODE(96), CODE(97)/
     *   Z0000, ZB200, Z3008, ZD100/
      DATA CODE(98), CODE(99), CODE(100), CODE(101)/
     *   Z0001, Z9A00, Z2000, Z1200/
      DATA CODE(102), CODE(103), CODE(104), CODE(105)/
     *   Z0000, Z0000, ZC185, Z1600/
      DATA CODE(106), CODE(107), CODE(108), CODE(109)/
     *   Z0001, ZD600, ZE0E4, Z1700/
      DATA CODE(110), CODE(111), CODE(112), CODE(113)/
     *   Z0000, Z0000, Z3180, Z3600/
      DATA CODE(114), CODE(115), CODE(116), CODE(117)/
     *   Z0000, Z0000, Z0070, Z1600/
      DATA CODE(118), CODE(119), CODE(120), CODE(121)/
     *   Z02B5, ZA600, ZC11C, Z1C00/
      DATA CODE(122), CODE(123), CODE(124), CODE(125)/
     *   Z0001, Z33A3, Z1710, Z5F00/
      DATA CODE(126), CODE(127), CODE(128), CODE(129)/
     *   Z0279, Z9400, Z68AE, Z0003/
      DATA CODE(130), CODE(131), CODE(132), CODE(133)/
     *   Z03BC, Z0000, Z29E0, ZBD00/
      DATA CODE(134), CODE(135), CODE(136), CODE(137)/
     *   Z2070, Z2400, Z3128, ZDA30/
      DATA CODE(138), CODE(139), CODE(140), CODE(141)/
     *   Z02B8, Z0000, Z0180, Z1603/
      DATA CODE(142), CODE(143), CODE(144), CODE(145)/
     *   Z47F9, Z9A00, Z9E07, Z1C00/
      DATA CODE(146), CODE(147), CODE(148), CODE(149)/
     *   Z2041, ZA600, Z6B76, Z8030/
      DATA CODE(150), CODE(151), CODE(152), CODE(153)/
     *   Z0000, ZB200, Z3008, ZD100/
      DATA CODE(154), CODE(155), CODE(156), CODE(157)/
     *   Z0001, Z9A00, Z2000, Z1200/
      DATA CODE(158), CODE(159), CODE(160), CODE(161)/
     *   Z4110, Z0000, ZC185, Z1600/
      DATA CODE(162), CODE(163), CODE(164), CODE(165)/
     *   Z0001, ZD795, ZE0E4, Z1700/
      DATA CODE(166), CODE(167), CODE(168), CODE(169)/
     *   Z0000, Z0000, Z3180, Z3600/
      DATA CODE(170), CODE(171), CODE(172), CODE(173)/
     *   Z20C8, Z0000, Z0070, Z16B0/
      DATA CODE(174), CODE(175), CODE(176), CODE(177)/
     *   Z0291, ZA600, ZC11C, Z1C00/
      DATA CODE(178), CODE(179), CODE(180), CODE(181)/
     *   Z0001, Z33A3, Z1710, Z5FA0/
      DATA CODE(182), CODE(183), CODE(184), CODE(185)/
     *   Z033F, Z9C00, Z0000, Z0000/
      DATA CODE(186), CODE(187), CODE(188), CODE(189)/
     *   Z03B8, Z0000, Z0400, ZFFE0/
      DATA CODE(190), CODE(191), CODE(192), CODE(193)/
     *   Z53BF, Z8CE0, Z0C00, Z0000/
      DATA CODE(194), CODE(195), CODE(196), CODE(197)/
     *   Z0000, Z001F, Z6648, Z7ED5/
      DATA CODE(198), CODE(199), CODE(200), CODE(201)/
     *   Z0000, Z001D, ZFAD5, Z10C6/
      DATA CODE(202), CODE(203), CODE(204), CODE(205)/
     *   Z0000, Z001E, ZBB10, ZB0CE/
      DATA CODE(206), CODE(207), CODE(208), CODE(209)/
     *   Z0000, Z0020, ZB517, ZCC1B/
      DATA CODE(210), CODE(211), CODE(212), CODE(213)/
     *   Z43FC, Z0000, Z1C00, Z8000/
      DATA CODE(214), CODE(215), CODE(216), CODE(217)/
     *   Z0040, Z0000, Z1404, Z021A/
      DATA CODE(218), CODE(219), CODE(220), CODE(221)/
     *   Z4000, Z0000, Z0000, Z0030/
      DATA CODE(222), CODE(223), CODE(224), CODE(225)/
     *   Z407F, Z8600, Z0400, Z0801/
      DATA CODE(226), CODE(227), CODE(228), CODE(229)/
     *   Z03B8, Z0000, Z0400, Z001B/
      DATA CODE(230), CODE(231), CODE(232), CODE(233)/
     *   Z43FC, Z0000, Z4C04, Z0000/
      DATA CODE(234), CODE(235), CODE(236), CODE(237)/
     *   Z43B9, ZDC00, Z0100, Z0000/
      DATA CODE(238), CODE(239), CODE(240), CODE(241)/
     *   Z4045, Z8000, Z0000, Z0000/
      DATA CODE(242), CODE(243), CODE(244), CODE(245)/
     *   Z023C, ZC394, Z0000, Z0000/
      DATA CODE(246), CODE(247), CODE(248), CODE(249)/
     *   Z0381, Z8000, Z0A00, Z0010/
      DATA CODE(250), CODE(251), CODE(252), CODE(253)/
     *   Z027C, ZC200, Z4C04, Z0000/
      DATA CODE(254), CODE(255), CODE(256), CODE(257)/
     *   Z0003, Z832E, Z0700, Z0000/
      DATA CODE(258), CODE(259), CODE(260), CODE(261)/
     *   Z12CC, Z0000, Z4004, Z000E/
      DATA CODE(262), CODE(263), CODE(264), CODE(265)/
     *   Z03BC, Z0000, Z0400, Z001A/
      DATA CODE(266), CODE(267), CODE(268), CODE(269)/
     *   Z43FE, ZBC00, Z0120, Z0000/
      DATA CODE(270), CODE(271), CODE(272), CODE(273)/
     *   Z0001, Z8000, Z0000, Z0000/
      DATA CODE(274), CODE(275), CODE(276), CODE(277)/
     *   Z0000, Z0000, Z2000, Z8000/
      DATA CODE(278), CODE(279), CODE(280), CODE(281)/
     *   Z037C, Z0118, Z0820, Z0000/
      DATA CODE(282), CODE(283), CODE(284), CODE(285)/
     *   Z43FE, ZBC00, Z4D25, Z0000/
      DATA CODE(286), CODE(287), CODE(288), CODE(289)/
     *   Z0003, Z8200, Z0740, Z0000/
      DATA CODE(290), CODE(291), CODE(292), CODE(293)/
     *   Z0381, Z8000, Z0000, Z0030/
      DATA CODE(294), CODE(295), CODE(296), CODE(297)/
     *   Z0000, Z0000, Z2000, Z8000/
      DATA CODE(298), CODE(299), CODE(300), CODE(301)/
     *   Z0003, Z8200, Z0740, Z0000/
      DATA CODE(302), CODE(303), CODE(304), CODE(305)/
     *   Z0380, Z0000, Z0A00, Z0000/
      DATA CODE(306), CODE(307), CODE(308), CODE(309)/
     *   Z037C, Z012A, Z0820, Z0000/
      DATA CODE(310), CODE(311), CODE(312), CODE(313)/
     *   Z0000, Z00E0, Z0000, Z0000/
      DATA CODE(314), CODE(315), CODE(316), CODE(317)/
     *   Z0000, Z0000, Z03FF, ZFFFF/
C-----------------------------------------------------------------------
      J1 = I1
      J2 = I2
      J3 = I3
      J4 = I4
      J5 = I5
      CALL APEX (CODE, 2, SLIST, 5)
C
 999  RETURN
      END
