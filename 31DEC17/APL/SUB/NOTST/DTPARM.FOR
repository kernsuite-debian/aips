      SUBROUTINE DTPARM (LFILE, JLUN, IPRT, IRET)
C----------------------------------------------------------------------
C! Read simulation parameters from an external KEYIN text file
C# Utility Parsing
C----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2007, 2012, 2016
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
C----------------------------------------------------------------------
C   Read data simulation parameters from an external text file
C   Input:
C      LFILE     C*48      External text file name.
C      JLUN      I         LUN for text file I/O.
C      IPRT      I         Print level.
C   Output:
C      IRET      I         Return code (0=> ok; else error)
C----------------------------------------------------------------------
      CHARACTER LFILE*48
      INTEGER JLUN, IPRT, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXIF2
      PARAMETER (MAXIF2 = 2 * MAXIF)
      INCLUDE 'INCS:DTPM.INC'
      INTEGER   NPARS, NANTAB, NFQTAB, NSUTAB, NCATAB, NCETAB,
     *   NCEKEY, NSMTAB, NSHTAB, NFGTAB, NBPTAB, NMAX
      PARAMETER (NPARS = 14, NANTAB = 20, NFQTAB = 2 + 4*MAXIF)
      PARAMETER (NSUTAB = 6 + 2*MAXIF, NCATAB = 10)
      PARAMETER (NCEKEY = 8, NCETAB = 3 + 2*MAXIF*MAXERP*NCEKEY)
      PARAMETER (NSMTAB = 21, NSHTAB = 11, NFGTAB = 16)
      PARAMETER (NBPTAB = 18 + 4*MAXIF)
      PARAMETER (NMAX = NCETAB)
      LOGICAL   WFALSE, WTRUE, WEOF, WERROR, WEND, WSERR
      CHARACTER LPARS(NPARS)*8, LVALS(NMAX)*8, LANTAB(NANTAB)*8,
     *   LFQTAB(NFQTAB)*8, LSUTAB(NSUTAB)*8, LCATAB(NCATAB)*8,
     *   LCETAB(NCETAB)*8, LCEKEY(NCEKEY)*4, LCESTK(2),
     *   LSMTAB(NSMTAB)*8, LSHTAB(NSHTAB)*8, LFGTAB(NFGTAB)*8,
     *   LBPTAB(NBPTAB)*8, LMARK*8, LKEY*8, LWORK*4, LPRE*1
      CHARACTER ANTNAM*8, VNAME(87)*8
      DOUBLE PRECISION VCPOS(3,87), ALLPOS(261)
      INTEGER   YARRAY(9,4), CSARRY(27)
      EQUIVALENCE (VCPOS(1,1), ALLPOS(1))
      DOUBLE PRECISION DVALS(NMAX), DJD, DTLONG, DTLAT, DTELEV
      INTEGER   IFINDF, IERR, KMODE, J, ISUBA, N, K, I, NPP, JJ, KK, JA,
     *   IDAY, IMTH, IYEAR, M, NFILL, NCH, NCHA, IT(6), IOFF, OFF, II
      INTEGER IKEY, ITRIM
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LMARK /'/       '/
      DATA LPARS /'AN_TABLE', 'SU_TABLE', 'FQ_TABLE', 'CL_TABLE',
     *   'SN_TABLE', 'VER     ', 'CAT_HDR ', 'CAL_ERR ', 'CORREL  ',
     *   'SU_MODEL', 'SCHEDULE', 'FLAG    ', 'BP_TABLE', 'LINPOL  '/
      DATA LANTAB /'STATION ', 'LONG    ', 'LAT     ', 'MNT     ',
     *   'NO      ', 'ELEV    ', 6*'ORBPARM ', 'KEY     ', 'ARRNAM  ',
     *   'CAL_ERR ', 'POLTYPE ', 'END     ', 3*'CARTPOS '/
      DATA LFQTAB /'FQID    ', MAXIF*'FREQ    ', MAXIF*'TOTBW   ',
     *   MAXIF*'CHANBW  ', MAXIF*'SIDE    ', 'END     '/
      DATA LSUTAB /'SU_ID   ', 'NAME    ', 'RAEPO   ', 'DECEPO  ',
     *   'EPOCH   ', MAXIF*'REST_FQ ', MAXIF*'LSR_VEL ', 'END     ' /
      DATA LCATAB /'REF_FREQ', 'NO_IF   ', 'NO_CHAN ',
     *   4 * 'STOKES  ', 'REF_DATE', 'COMPRESS', 'REST_FQ '/
      DATA LCEKEY /'BP', 'RATE', 'DELA', 'PHAS', 'FEED', 'GAIN',
     *   'TSYS', 'GERR'/
      DATA LCESTK /'R', 'L'/
      DATA LSMTAB /'SOURCE  ', 12*'TYPE    ', 5*'SPEC    ',
     *   2*'VEL     ', 'END     '/
      DATA LSHTAB /'DAY     ', 'MONTH   ', 'YEAR    ', 'START   ',
     *   'STOP    ', 'SOURCE  ', 'SUBA    ', 'FQID    ',
     *   'TINT    ', 'SELECT  ', 'END     '/
      DATA LFGTAB /'DAY     ', 'MONTH   ', 'YEAR    ', 'START   ',
     *   'STOP    ', 'ANT_1   ', 'ANT_2   ', 'BIF     ', 'EIF     ',
     *   'BCHAN   ', 'ECHAN   ', 'SOURCE  ', 'FQID    ', 'SUBA    ',
     *   'PFLAG   ', 'END     '/
      DATA LBPTAB /'DAY     ', 'MONTH   ', 'YEAR    ', 'BPTYPE  ',
     *   'XC      ', 'AC      ', 'NANT    ', 'STATION ', 'FQID    ',
     *   'SUBA    ', 'START   ', 'STOP    ', 'INTERV  ', 'TINT    ',
     *   'CAL_ERR ', MAXIF*'FLAGPOL ', MAXIF*'FLAGIF  ',
     *   MAXIF2*'FLAGCHAN', 'KEY     ', 'NUMSHF  ', 'END     '/
      DATA WFALSE /.FALSE./, WTRUE /.TRUE./
C                                             hard code VLBA,VLA names
C                                             positions [VLBA memo 644]
      DATA VNAME / 'VLBA_BR ', 'VLBA_FD ', 'VLBA_HN ', 'VLBA_KP ',
     *   'VLBA_LA ', 'VLBA_MK ', 'VLBA_NL ', 'VLBA_OV ', 'VLBA_PT ',
     *   'VLBA_SC ',
     *   'VLBA_GB ', 'VLBA_AR ', 'VLBA_EB ', 'VLBA_Y  ', 'VLBA_Y1 ',
     *     'VLA_1N  ','VLA_2N  ','VLA_3N  ','VLA_4N  ','VLA_5N ',
     *     'VLA_6N  ','VLA_7N  ','VLA_8N  ','VLA_9N  ','VLA_10N',
     *     'VLA_12N ','VLA_14N ','VLA_16N ','VLA_18N ','VLA_20N',
     *     'VLA_24N ','VLA_28N ','VLA_32N ','VLA_36N ','VLA_40N',
     *     'VLA_48N ','VLA_56N ','VLA_64N ','VLA_72N ',
     *     'VLA_1E  ','VLA_2E  ','VLA_3E  ','VLA_4E  ','VLA_5E ',
     *     'VLA_6E  ','VLA_7E  ','VLA_8E  ','VLA_9E  ','VLA_10E',
     *     'VLA_12E ','VLA_14E ','VLA_16E ','VLA_18E ','VLA_20E',
     *     'VLA_24E ','VLA_28E ','VLA_32E ','VLA_36E ','VLA_40E',
     *     'VLA_48E ','VLA_56E ','VLA_64E ','VLA_72E ',
     *     'VLA_1W  ','VLA_2W  ','VLA_3W  ','VLA_4W  ','VLA_5W ',
     *     'VLA_6W  ','VLA_7W  ','VLA_8W  ','VLA_9W  ','VLA_10W',
     *     'VLA_12W ','VLA_14W ','VLA_16W ','VLA_18W ','VLA_20W',
     *     'VLA_24W ','VLA_28W ','VLA_32W ','VLA_36W ','VLA_40W',
     *     'VLA_48W ','VLA_56W ','VLA_64W ','VLA_72W ' /
C     ALL THESE POSITIONS ARE SPECIFIED IN A RIGHT-HANDED-COORDINATE
C     SYSTEM.
C                                       VLBA positions
C                                       GSFC 26-Apr-2011
      DATA (ALLPOS(I), I=1,30) /
     *    -2112065.1313 , -3705356.5045 ,  4726813.7063 ,
     *    -1324009.2612 , -5332181.9533 ,  3231962.4105 ,
     *     1446374.9448 , -4447939.6659 ,  4322306.1584 ,
     *    -1995678.7713 , -5037317.6975 ,  3357328.0501 ,
     *    -1449752.5106 , -4975298.5744 ,  3709123.8629 ,
     *    -5464075.1116 , -2495248.4197 ,  2148297.1963 ,
     *     -130872.4205 , -4762317.0969 ,  4226851.0067 ,
     *    -2409150.3086 , -4478573.1493 ,  3838617.3503 ,
     *    -1640953.8646 , -5014816.0199 ,  3575411.8106 ,
     *     2607848.6046 , -5488069.5793 ,  1932739.6674 /
C                                       HSA etc Y1 = E8
      DATA (ALLPOS(I), I=31,45) /
     *      882589.4924 , -4924872.3457 ,  3943729.4016 ,
     *     2390486.9000 , -5564731.4400 ,  1994720.4500 ,
     *     4033947.3416 ,   486990.7044 ,  4900430.9260 ,
     *    -1601185.3299 , -5041977.1588 ,  3554875.6285 ,
     *    -1600799.7785 , -5042219.6974 ,  3554706.6858 /
C                                       VLA inner north arm
      DATA (ALLPOS(I), I=46,81) /
     *    -1601185.5812,  -5041977.8184,   3554876.1628,
     *    -1601180.8273,  -5041947.1012,   3554921.3595,
     *    -1601177.3375,  -5041924.7150,   3554954.3143,
     *    -1601173.9462,  -5041902.3088,   3554987.2614,
     *    -1601168.7298,  -5041868.7140,   3555036.6760,
     *    -1601162.5571,  -5041828.6487,   3555095.6305,
     *    -1601155.5900,  -5041783.4940,   3555162.1179,
     *    -1601147.8964,  -5041733.4948,   3555235.6912,
     *    -1601139.4359,  -5041678.7067,   3555316.2864,
     *    -1601130.2876,  -5041619.4312,   3555403.4716,
     *    -1601110.0050,  -5041487.7382,   3555597.1744,
     *    -1601087.1199,  -5041339.5191,   3555815.6049/
C    *     -1601162.375 ,  -5042002.500 ,   3554915.500 ,
C    *     -1601148.500 ,  -5042007.000 ,   3554960.750 ,
C    *     -1601138.375 ,  -5042010.500 ,   3554993.750 ,
C    *     -1601128.250 ,  -5042014.000 ,   3555026.750 ,
C    *     -1601113.125 ,  -5042019.000 ,   3555076.000 ,
C    *     -1601095.000 ,  -5042025.500 ,   3555135.000 ,
C    *     -1601074.625 ,  -5042032.500 ,   3555201.500 ,
C    *     -1601052.125 ,  -5042040.000 ,   3555275.250 ,
C    *     -1601027.250 ,  -5042048.500 ,   3555355.750 ,
C    *     -1601000.625 ,  -5042057.500 ,   3555443.000 ,
C    *     -1600941.250 ,  -5042078.000 ,   3555636.750 ,
C    *     -1600874.375 ,  -5042101.000 ,   3555855.250 /
C                                       VLA outer north arm
      DATA (ALLPOS(I), I=82,117) /
     *    -1601061.9090,  -5041175.5469,   3556057.7648,
     *    -1601034.3510,  -5040996.2032,   3556322.6715,
     *    -1601004.6596,  -5040802.4683,   3556609.8682,
     *    -1600930.0335,  -5040316.0602,   3557330.1202,
     *    -1600863.6317,  -5039884.9779,   3557965.0546,
     *    -1600780.9844,  -5039347.1157,   3558761.2772,
     *    -1600690.5491,  -5038758.3945,   3559631.7967,
     *    -1600592.6887,  -5038121.0119,   3560574.5855,
     *    -1600374.8345,  -5036703.8597,   3562667.6277,
     *    -1600128.3280,  -5035103.8101,   3565024.4053,
     *    -1599855.5942,  -5033332.0436,   3567636.3647,
     *    -1599557.8510,  -5031396.0417,   3570494.4958/
C    *     -1600800.250 ,  -5042126.000 ,   3556097.750 ,
C    *     -1600719.250 ,  -5042153.500 ,   3556362.750 ,
C    *     -1600631.750 ,  -5042183.500 ,   3556650.000 ,
C    *     -1600412.000 ,  -5042258.000 ,   3557370.500 ,
C    *     -1600217.375 ,  -5042324.500 ,   3558005.750 ,
C    *     -1599974.500 ,  -5042407.000 ,   3558802.250 ,
C    *     -1599708.250 ,  -5042497.000 ,   3559673.250 ,
C    *     -1599420.625 ,  -5042595.500 ,   3560616.500 ,
C    *     -1598780.750 ,  -5042813.500 ,   3562710.500 ,
C    *     -1598058.500 ,  -5043060.000 ,   3565068.250 ,
C    *     -1597258.750 ,  -5043333.000 ,   3567681.250 ,
C    *     -1596384.500 ,  -5043630.500 ,   3570540.750 /
C                                       VLA inner east arm
      DATA (ALLPOS(I), I=118,153) /
     *    -1601192.4234,  -5042022.5130,   3554810.1800,
     *    -1601150.0244,  -5042000.2842,   3554860.4667,
     *    -1601114.3092,  -5042022.8016,   3554844.6845,
     *    -1601068.7433,  -5042051.5795,   3554824.5717,
     *    -1601014.4053,  -5042085.8973,   3554800.5249,
     *    -1600951.5325,  -5042125.5656,   3554772.7441,
     *    -1600880.5328,  -5042170.0473,   3554741.1933,
     *    -1600801.8818,  -5042219.0267,   3554706.1834,
     *    -1600715.9086,  -5042272.8495,   3554667.9202,
     *    -1600622.8853,  -5042331.4260,   3554626.7754,
     *    -1600416.4667,  -5042462.1118,   3554535.7763,
     *    -1600183.9146,  -5042609.6470,   3554433.3630/
C    *     -1601182.500 ,  -5041995.500 ,   3554849.500 ,
C    *     -1601204.375 ,  -5042038.000 ,   3554899.750 ,
C    *     -1601246.500 ,  -5042073.500 ,   3554884.000 ,
C    *     -1601300.375 ,  -5042119.000 ,   3554863.750 ,
C    *     -1601364.625 ,  -5042173.500 ,   3554839.750 ,
C    *     -1601438.875 ,  -5042236.500 ,   3554812.000 ,
C    *     -1601522.500 ,  -5042307.500 ,   3554780.500 ,
C    *     -1601615.125 ,  -5042386.000 ,   3554745.500 ,
C    *     -1601716.375 ,  -5042472.000 ,   3554707.250 ,
C    *     -1601826.250 ,  -5042565.000 ,   3554666.000 ,
C    *     -1602070.000 ,  -5042771.500 ,   3554575.000 ,
C    *     -1602345.500 ,  -5043004.500 ,   3554472.500 /
C                                       VLA outer east arm
      DATA (ALLPOS(I), I=154,189) /
     *    -1599926.0619,  -5042772.6307,   3554319.5244,
     *    -1599644.8216,  -5042953.3317,   3554196.7704,
     *    -1599340.7593,  -5043150.6305,   3554064.9546,
     *    -1598663.0503,  -5043581.0597,   3553766.7640,
     *    -1597899.8656,  -5044068.3468,   3553432.1801,
     *    -1597053.0803,  -5044604.3606,   3553058.7225,
     *    -1596127.6963,  -5045193.4207,   3552652.1544,
     *    -1595124.8912,  -5045829.1386,   3552210.4204,
     *    -1592894.0481,  -5047228.8143,   3551220.9383,
     *    -1590380.5737,  -5048809.9468,   3550108.1957,
     *    -1587600.1820,  -5050575.5756,   3548885.1338,
     *    -1584460.8785,  -5052385.3277,   3547599.7478/
C    *     -1602650.250 ,  -5043262.500 ,   3554358.750 ,
C    *     -1602984.375 ,  -5043543.500 ,   3554235.750 ,
C    *     -1603346.750 ,  -5043848.000 ,   3554104.000 ,
C    *     -1604149.000 ,  -5044526.000 ,   3553805.500 ,
C    *     -1605053.875 ,  -5045289.500 ,   3553471.000 ,
C    *     -1606055.250 ,  -5046136.500 ,   3553097.250 ,
C    *     -1607151.375 ,  -5047062.500 ,   3552690.500 ,
C    *     -1608337.750 ,  -5048065.500 ,   3552248.500 ,
C    *     -1610968.500 ,  -5050297.500 ,   3551258.750 ,
C    *     -1600360.750 ,  -5033712.000 ,   3559007.250 ,
C    *     -1617226.125 ,  -5055593.500 ,   3548921.750 ,
C    *     -1620836.000 ,  -5058734.500 ,   3547636.000 /
C                                       VLA inner west arm
      DATA (ALLPOS(I), I=190,225) /
     *    -1601188.9836,  -5042000.1464,   3554843.1582,
     *    -1601225.2315,  -5041980.0245,   3554855.4252,
     *    -1601265.1006,  -5041982.2205,   3554834.5976,
     *    -1601315.8491,  -5041984.9771,   3554808.0410,
     *    -1601376.9668,  -5041988.3305,   3554776.1345,
     *    -1601447.1640,  -5041992.1443,   3554739.4378,
     *    -1601526.3193,  -5041996.5036,   3554698.0586,
     *    -1601614.0472,  -5042001.3101,   3554652.2461,
     *    -1601709.9765,  -5042006.5803,   3554602.0873,
     *    -1601814.0100,  -5042012.5537,   3554547.9577,
     *    -1602044.8585,  -5042025.4792,   3554427.5721,
     *    -1602304.8793,  -5042039.4921,   3554291.4700/
C    *     -1601172.375 ,  -5041999.000 ,   3554882.500 ,
C    *     -1601131.250 ,  -5041962.500 ,   3554894.750 ,
C    *     -1601099.875 ,  -5041922.500 ,   3554874.000 ,
C    *     -1601060.000 ,  -5041872.000 ,   3554847.250 ,
C    *     -1601012.000 ,  -5041811.000 ,   3554815.500 ,
C    *     -1600956.875 ,  -5041740.500 ,   3554778.750 ,
C    *     -1600894.625 ,  -5041661.500 ,   3554737.250 ,
C    *     -1600825.750 ,  -5041573.500 ,   3554691.500 ,
C    *     -1600750.375 ,  -5041477.500 ,   3554641.250  ,
C    *     -1600750.375 ,  -5041477.500 ,   3554641.250 ,
C    *     -1600668.875 ,  -5041373.500 ,   3554587.250 ,
C    *     -1600487.625 ,  -5041142.500 ,   3554466.750 /
C                                       VLA outer west arm
      DATA (ALLPOS(I), I=226,261) /
     *    -1602592.8098,  -5042054.6569,   3554140.4391,
     *    -1602908.1440,  -5042071.9526,   3553975.6447,
     *    -1603249.6404,  -5042091.0551,   3553797.5387,
     *    -1604008.6973,  -5042135.4759,   3553403.4420,
     *    -1604865.6058,  -5042189.6940,   3552962.1040,
     *    -1605808.6140,  -5042229.7332,   3552458.9416,
     *    -1606841.9203,  -5042279.3368,   3551912.7595,
     *    -1607962.4132,  -5042337.8463,   3551324.6843,
     *    -1610451.8862,  -5042470.7602,   3550020.7970,
     *    -1613255.3656,  -5042612.7083,   3548545.6456,
     *    -1616361.5422,  -5042770.1300,   3546911.1800,
     *    -1619757.2674,  -5042937.2680,   3545120.1265/
C    *     -1600283.250 ,  -5040882.500 ,   3554330.500 ,
C    *     -1600056.750 ,  -5040594.500 ,   3554179.500 ,
C    *     -1599809.125 ,  -5040279.000 ,   3554014.500 ,
C    *     -1599541.000 ,  -5039937.500 ,   3553836.500 ,
C    *     -1598946.375 ,  -5039178.000 ,   3553442.250 ,
C    *     -1598277.500 ,  -5038320.500 ,   3553000.500 ,
C    *     -1597530.000 ,  -5037377.000 ,   3552497.250 ,
C    *     -1596714.375 ,  -5036343.500 ,   3551950.750 ,
C    *     -1595832.500 ,  -5035222.500 ,   3551362.500 ,
C    *     -1593875.000 ,  -5032732.000 ,   3550058.000 ,
C    *     -1591666.125 ,  -5029927.000 ,   3548582.250 ,
C    *     -1589218.750 ,  -5026819.500 ,   3546947.000 /
C     specify antennas corresponding to VLA arm position ordering
      DATA YARRAY / 8, 13, 16, 18, 20, 21, 22, 23, 24,
     *              4,  8, 11, 13, 15, 16, 17, 18, 19,
     *              2,  4,  6,  8, 10, 11, 12, 13, 14,
     *              1,  2,  3,  4,  5,  6,  7,  8,  9 /
C  this is just C array, need to move some in to make CS, ask
C  MRUPEN about this....
      DATA CSARRY /12, 14, 16, 18, 20, 21, 22, 23, 24,
     *             36, 38, 40, 42, 44, 45, 46, 47, 48,
     *             60, 62, 64, 66, 68, 69, 70, 71, 72 /
C---------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Intialize DTPM.INC
      NDTSUB = 0
      NDTMSB = 0
      NDTFQD = 0
      NDTSID = 0
      WDTCAT = WFALSE
      NDTAER = 0
      NDTSMD = 0
      NDTSCH = 0
      NDTFG = 0
      NDTBPD = 0
C                                       Fill CAL_ERR keyword array
      LCETAB(1) = 'STATION'
      LCETAB(2) = 'FQID'
      DO 15 J = 1, NCEKEY
         DO 10 K = 1, 2
            DO 7 M = 1, MAXIF
               DO 5 KK = 1, MAXERP
                  JJ = (J - 1) * 2 * MAXPRD + (K - 1) * MAXPRD +
     *               (M - 1) * MAXERP + KK + 2
                  IF (M.LE.10) WRITE (LWORK,9007) M, LCESTK(K)
                  IF (M.GE.10) WRITE (LWORK,9010) M, LCESTK(K)
                  LCETAB(JJ) = LCEKEY(J)(1:ITRIM(LCEKEY(J))) // LWORK
5                 CONTINUE
7              CONTINUE
10          CONTINUE
15       CONTINUE
      LCETAB(JJ+1) = 'END'
C                                       Open the external text file
      CALL ZTXOPN ('READ', JLUN, IFINDF, LFILE, WFALSE, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR, LFILE
         IRET = 1
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Print level
      KMODE = 0
      IF (IPRT.GT.0) KMODE = 1
C                                       Repeat (read keyin_record)
C                                       until (EOF or ERROR)
C                                       Set defaults
50    DO 60 J = 1, NMAX
         DVALS(J) = DBLANK
         LVALS(J) = '        '
60       CONTINUE
C
      CALL KEYIN (LPARS, DVALS, LVALS, NPARS, LMARK, KMODE, JLUN,
     *   IFINDF, IERR)
      WEOF = (IERR.EQ.1)
      WERROR = (IERR.NE.0).AND.(.NOT.WEOF)
      IF (WERROR) WRITE (MSGTXT,1060) IERR
C                                       If (NOT EOF or ERROR) then
C                                       process keyin_record.
      IF (WEOF.OR.WERROR) GO TO 1900
C                                       Case group_id of:
C======================================================================
C                                       1: AN_TABLE entry
      LKEY = 'AN_TABLE'
      J = IKEY (LKEY, LPARS, NPARS)
      IF (DVALS(J).NE.DBLANK) THEN
C                                       Process AN table data:
C                                       Extract table version no.
         LKEY = 'VER'
         J = IKEY (LKEY, LPARS, NPARS)
         NDTSUB = NDTSUB + 1
         IF (DVALS(J).EQ.DBLANK) THEN
            NDTMSB = NDTMSB + 1
            IDTSUB(NDTSUB) = NDTMSB
         ELSE
            IDTSUB(NDTSUB) = DVALS(J)
            NDTMSB = MAX (NDTMSB, IDTSUB(NDTSUB))
            END IF
         ISUBA = IDTSUB(NDTSUB)
C                                       Reset antenna count and
C                                       keyword values
         NDTANT(ISUBA) = 0
         LDTARR(ISUBA) = ' '
         WDTPOL(ISUBA) = WFALSE
         LDTPOL(ISUBA) = ' '
C                                       Repeat (read keyin_record)
C                                       until (EOF or ERROR or ENDCAT)
C                                       Set KEYIN defaults.
75       DO 80 J = 1, NMAX
            DVALS(J) = DBLANK
            LVALS(J) = '        '
80          CONTINUE
C
         CALL KEYIN (LANTAB, DVALS, LVALS, NANTAB, LMARK, KMODE, JLUN,
     *      IFINDF, IERR)
         WEOF = (IERR.EQ.1)
         WERROR = (IERR.NE.0).AND.(.NOT.WEOF)
         IF (WERROR) WRITE (MSGTXT,1060) IERR
C                                       ENDCAT ?
         LKEY = 'END'
         J = IKEY (LKEY, LANTAB, NANTAB)
         WEND = (DVALS(J).NE.DBLANK)
C                                       If NOT (ENDCAT or ERROR or EOF)
C                                       then process record
         IF (.NOT.(WEND.OR.WERROR.OR.WEOF)) THEN
C                                       Case record_type of:
C                                       i) KEY record
            LKEY = 'KEY'
            J = IKEY (LKEY, LANTAB, NANTAB)
            IF (DVALS(J).NE.DBLANK) THEN
C                                       Include polzn. errors ?
               LKEY = 'CAL_ERR'
               J = IKEY (LKEY, LANTAB, NANTAB)
               WDTPOL(ISUBA) = (DVALS(J).NE.DBLANK)
C                                       Polzn. solution type
               LKEY = 'POLTYPE'
               J = IKEY (LKEY, LANTAB, NANTAB)
               IF (DVALS(J).NE.DBLANK) LDTPOL(ISUBA) = LVALS(J)
C                                       Keyword record
C                                       Extract array name
               LKEY = 'ARRNAM'
               J = IKEY (LKEY, LANTAB, NANTAB)
               LDTARR(ISUBA) = ' '
               IF (DVALS(J).NE.DBLANK) LDTARR(ISUBA) = LVALS(J)
C                                       ii) Antenna record
            ELSE
               LKEY = 'STATION'
               J = IKEY (LKEY, LANTAB, NANTAB)
               IF (DVALS(J).NE.DBLANK) THEN
C                                          Get Antenna name
                  ANTNAM = LVALS(J)
C                                          Load default array configurations
                  IF ((ANTNAM(1:4).EQ.'ALL_') .OR.
     *               (ANTNAM(:5).EQ.'VLA_Y'))THEN
C     entire VLBA is specified using 'ALL_VLBA'
                     IF ((ANTNAM(:8).EQ.'ALL_VLBA') .OR.
     *                  (ANTNAM(:7).EQ.'ALL_HSA')) THEN
                        II = 10
                        IF (ANTNAM(:7).EQ.'ALL_HSA') II = 14
                        DO 86 I = 1,II
                           NDTANT(ISUBA) = NDTANT(ISUBA)+1
                           N = NDTANT(ISUBA)
C     set antenna number
                           IDTANO(N,ISUBA) = N
                           DTCPOS(1,N,ISUBA) = VCPOS(1,I)
                           DTCPOS(2,N,ISUBA) = VCPOS(2,I)
                           DTCPOS(3,N,ISUBA) = VCPOS(3,I)
C     zero orbital elements
                           CALL DFILL (6, 0.0D0, DTORB(1,N,ISUBA))
C     set mount type to AZEL
                           IDTMNT(N,ISUBA) = 0
C     use default station name
                           LDTANT(N,ISUBA) = VNAME(I)
 86                        CONTINUE
                     ELSE IF (ANTNAM(5:8).EQ.'YCS') THEN
                        DO 2010 M = 1,27
C     pick up CS array antennas only
                           JJ = 15 + CSARRY(M)
                           NDTANT(ISUBA) = NDTANT(ISUBA)+1
                           N=NDTANT(ISUBA)
                           IDTANO(N,ISUBA) = N
                           DTCPOS(1,N,ISUBA) = VCPOS(1,JJ)
                           DTCPOS(2,N,ISUBA) = VCPOS(2,JJ)
                           DTCPOS(3,N,ISUBA) = VCPOS(3,JJ)
                           CALL DFILL (6, 0.0D0, DTORB(1,N,ISUBA))
                           IDTMNT(N,ISUBA) = 0
                           LDTANT(N,ISUBA) = VNAME(JJ)
 2010                      CONTINUE
C     VLA arrays are specified using 'ALL_Y*', where '*' = A,B,C,D
                     ELSE IF ((ANTNAM(5:6).EQ.'YA') .OR.
     *                  (ANTNAM(5:6).EQ.'YB') .OR. (ANTNAM(5:6).EQ.'YC')
     *                  .OR. (ANTNAM(5:6).EQ.'YD'))  THEN
                        IF (ANTNAM(5:8).EQ.'YA') OFF = 1
                        IF (ANTNAM(5:8).EQ.'YB') OFF = 2
                        IF (ANTNAM(5:8).EQ.'YC') OFF = 3
                        IF (ANTNAM(5:8).EQ.'YD') OFF = 4
                        DO 2000, M=1,9
C     north arm antenna
                           JJ = 15 + YARRAY(M,OFF)
                           NDTANT(ISUBA) = NDTANT(ISUBA)+1
                           N=NDTANT(ISUBA)
                           IDTANO(N,ISUBA) = N
                           DTCPOS(1,N,ISUBA) = VCPOS(1,JJ)
                           DTCPOS(2,N,ISUBA) = VCPOS(2,JJ)
                           DTCPOS(3,N,ISUBA) = VCPOS(3,JJ)
                           CALL DFILL (6, 0.0D0, DTORB(1,N,ISUBA))
                           IDTMNT(N,ISUBA) = 0
                           LDTANT(N,ISUBA) = VNAME(JJ)
C     east arm antenna
                           JJ = 39 + YARRAY(M,OFF)
                           NDTANT(ISUBA) = NDTANT(ISUBA)+1
                           N=NDTANT(ISUBA)
                           IDTANO(N,ISUBA) = N
                           DTCPOS(1,N,ISUBA) = VCPOS(1,JJ)
                           DTCPOS(2,N,ISUBA) = VCPOS(2,JJ)
                           DTCPOS(3,N,ISUBA) = VCPOS(3,JJ)
                           CALL DFILL (6, 0.0D0, DTORB(1,N,ISUBA))
                           IDTMNT(N,ISUBA) = 0
                           LDTANT(N,ISUBA) = VNAME(JJ)
C     west arm antenna
                           JJ = 63 + YARRAY(M,OFF)
                           NDTANT(ISUBA) = NDTANT(ISUBA)+1
                           N=NDTANT(ISUBA)
                           IDTANO(N,ISUBA) = N
                           DTCPOS(1,N,ISUBA) = VCPOS(1,JJ)
                           DTCPOS(2,N,ISUBA) = VCPOS(2,JJ)
                           DTCPOS(3,N,ISUBA) = VCPOS(3,JJ)
                           CALL DFILL (6, 0.0D0, DTORB(1,N,ISUBA))
                           IDTMNT(N,ISUBA) = 0
                           LDTANT(N,ISUBA) = VNAME(JJ)
 2000                      CONTINUE
                        END IF
                  ELSE
                     NDTANT(ISUBA) = NDTANT(ISUBA) + 1
                     N = NDTANT(ISUBA)
C                                       Station name.
                     LDTANT(N,ISUBA) = ANTNAM
C                                       Default station info
                     IDTANO(N,ISUBA) = N
                     DTCPOS(1,N,ISUBA) = 0.0D0
                     DTCPOS(2,N,ISUBA) = 0.0D0
                     DTCPOS(3,N,ISUBA) = 0.0D0
                     CALL DFILL (6, 0.0D0, DTORB(1,N,ISUBA))
                     IDTMNT(N,ISUBA) = 0
C                                       Try to match individual
C                                       VLBA or VLA antennas
                     DO 87 I = 1,87
                        IF (ANTNAM.EQ.VNAME(I)) THEN
                           DTCPOS(1,N,ISUBA) = VCPOS(1,I)
                           DTCPOS(2,N,ISUBA) = VCPOS(2,I)
                           DTCPOS(3,N,ISUBA) = VCPOS(3,I)
                           END IF
 87                     CONTINUE
C                                       Antenna number
                     LKEY = 'NO'
                     J = IKEY (LKEY, LANTAB, NANTAB)
                     IF (DVALS(J).NE.DBLANK) IDTANO(N,ISUBA) = DVALS(J)
C                                       Mount type
                     LKEY = 'MNT'
                     J = IKEY (LKEY, LANTAB, NANTAB)
                     IF (DVALS(J).NE.DBLANK) THEN
                        IF (LVALS(J).EQ.'AZEL') IDTMNT(N,ISUBA) = 0
                        IF (LVALS(J).EQ.'HADC') IDTMNT(N,ISUBA) = 1
                        IF (LVALS(J).EQ.'ORB') IDTMNT(N,ISUBA) = 2
                        END IF
C                                       Longitude and latitude
                     LKEY = 'LONG'
                     J = IKEY (LKEY, LANTAB, NANTAB)
                     DTLONG = 0.0D0
                     IF (DVALS(J).NE.DBLANK) DTLONG = DVALS(J)
C
                     LKEY = 'LAT'
                     J = IKEY (LKEY, LANTAB, NANTAB)
                     DTLAT = 0.0D0
                     IF (DVALS(J).NE.DBLANK) DTLAT = DVALS(J)
C                                       Elevation
                     LKEY = 'ELEV'
                     J = IKEY (LKEY, LANTAB, NANTAB)
                     DTELEV = 0.0D0
                     IF (DVALS(J).NE.DBLANK) DTELEV = DVALS(J)
                     LKEY = 'CARTPOS'
                     J = IKEY (LKEY, LANTAB, NANTAB)
C                                       If one element of CARTPOS is
C                                       specified, assume all are valid
                     IF (DVALS(J).NE.DBLANK) THEN
                        DTCPOS(1,N,ISUBA) = DVALS(J)
                        DTCPOS(2,N,ISUBA) = DVALS(J+1)
                        DTCPOS(3,N,ISUBA) = DVALS(J+2)
C                                       Convert Long, Lat, and Elev into
C                                       XYZ positions
                     ELSE IF ((DTLAT.NE.0.0D0) .OR. (DTLONG.NE.0.0D0)
     *                  .OR. (DTELEV.NE.0.0D0)) THEN
                        CALL GEOPOS (DTLONG, DTLAT, DTELEV,
     *                     DTCPOS(1,N,ISUBA), DTCPOS(2,N,ISUBA),
     *                     DTCPOS(3,N,ISUBA))
                        END IF
C                                       Orbital elements
                     LKEY = 'ORBPARM'
                     J = IKEY (LKEY, LANTAB, NANTAB)
                     DO 88 K = 1, 6
                        I = J + K - 1
                        IF (DVALS(I).NE.DBLANK)
     *                     DTORB(K,N,ISUBA) = DVALS(I)
 88                     CONTINUE
                     END IF
C                                       iii) other: error
               ELSE
                  WERROR = WTRUE
                  WRITE (MSGTXT,1085)
                  END IF
               END IF
C                                       Endcase (AN record_type)
            END IF
C                                       Read next record
         IF (.NOT.(WEOF.OR.WERROR.OR.WEND)) GO TO 75
         GO TO 1890
         END IF
C======================================================================
C                                       2: FQ_TABLE entry
      LKEY = 'FQ_TABLE'
      J = IKEY (LKEY, LPARS, NPARS)
      IF (DVALS(J).NE.DBLANK) THEN
C                                       Process FQ table data
         NDTFID = 0
C                                       Repeat (read keyin_record)
C                                       until (EOF or ERROR or ENDCAT)
110      DO 115 J = 1, NMAX
            DVALS(J) = DBLANK
            LVALS(J) = '        '
115         CONTINUE
C
         CALL KEYIN (LFQTAB, DVALS, LVALS, NFQTAB, LMARK, KMODE,
     *     JLUN, IFINDF, IERR)
         WEOF = (IERR.EQ.1)
         WERROR = (IERR.NE.0).AND.(.NOT.WEOF)
         IF (WERROR) WRITE (MSGTXT,1060) IERR
C                                       ENDCAT ?
         LKEY = 'END'
         J = IKEY (LKEY, LFQTAB, NFQTAB)
         WEND = (DVALS(J).NE.DBLANK)
C                                       If (not(END or ERR or EOF))
C                                       then process record.
         IF (.NOT.(WEND.OR.WERROR.OR.WEOF)) THEN
            NDTFQD = NDTFQD + 1
            NPP = NDTFQD
C                                       Initialize FQ record
            IDTFQD(NPP) = NPP
            CALL DFILL (MAXIF, 0.0D0, DTFREQ(1,NPP))
            CALL RFILL (MAXIF, 0.0, RDTCHW(1,NPP))
            CALL RFILL (MAXIF, 0.0, RDTTBW(1,NPP))
            CALL FILL (MAXIF, 0, IDTSDB(1,NPP))
C                                       Extract freqid.
            LKEY = 'FQID'
            J = IKEY (LKEY, LFQTAB, NFQTAB)
            IF (DVALS(J).NE.DBLANK) IDTFQD(NPP) = DVALS(J)
C                                       Frequency (Hz)
            LKEY = 'FREQ'
            J = IKEY (LKEY, LFQTAB, NFQTAB)
            DO 120 K = 1, MAXIF
               I = J + (K - 1)
               IF (DVALS(I).NE.DBLANK) THEN
                  DTFREQ(K,NPP) = DVALS(I) * 1.0D6
                  NDTFID = MAX (NDTFID, K)
                  END IF
120            CONTINUE
C                                       Total bandwidth
            LKEY = 'TOTBW'
            J = IKEY (LKEY, LFQTAB, NFQTAB)
            DO 125 K = 1, MAXIF
               I = J + (K - 1)
               IF (DVALS(I).NE.DBLANK) RDTTBW(K,NPP) = DVALS(I) * 1.0D6
125            CONTINUE
C                                       Channel bandwidth (Hz)
            LKEY = 'CHANBW'
            J = IKEY (LKEY, LFQTAB, NFQTAB)
            DO 130 K = 1, MAXIF
               I = J + (K - 1)
               IF (DVALS(I).NE.DBLANK) RDTCHW(K,NPP) = DVALS(I) * 1.0D6
130            CONTINUE
C                                       Sideband
            LKEY = 'SIDE'
            J = IKEY (LKEY, LFQTAB, NFQTAB)
            DO 135 K = 1, MAXIF
               I = J + (K - 1)
               IF (DVALS(I).NE.DBLANK) IDTSDB(K,NPP) = DVALS(I)
135            CONTINUE
            END IF
C                                       Read next FQ record
         IF (.NOT.(WEND.OR.WERROR.OR.WEOF)) GO TO 110
         GO TO 1890
         END IF
C======================================================================
C                                       3: SU_TABLE entry
      LKEY = 'SU_TABLE'
      J = IKEY (LKEY, LPARS, NPARS)
      IF (DVALS(J).NE.DBLANK) THEN
C                                       Repeat (read keyin_record)
C                                       until (EOF or ERR or ENDCAT)
210      DO 215 J = 1, NMAX
            DVALS(J) = DBLANK
            LVALS(J) = '        '
215         CONTINUE
C
         CALL KEYIN (LSUTAB, DVALS, LVALS, NSUTAB, LMARK, KMODE,
     *      JLUN, IFINDF, IERR)
         WEOF = (IERR.EQ.1)
         WERROR = (IERR.NE.0).AND.(.NOT.WEOF)
         IF (WERROR) WRITE (MSGTXT,1060) IERR
C                                       ENDCAT ?
         LKEY = 'END'
         J = IKEY (LKEY, LSUTAB, NSUTAB)
         WEND = (DVALS(J).NE.DBLANK)
C                                       If (NOT (END or ERR or EOF))
C                                       then process record
         IF (.NOT.(WEND.OR.WERROR.OR.WEOF)) THEN
C                                       Initialize SU record
            NDTSID = NDTSID + 1
            NPP = NDTSID
            IDTSID(NPP) = NPP
            LDTSOU(NPP) = ' '
            DTSRA(NPP) = 0.0D0
            DTSDEC(NPP) = 0.0D0
            DTEPOC(NPP) = 0.0D0
            NFILL = MAXIF * MAXSID
            CALL DFILL (NFILL, 0.0D0, DTSUFQ)
            CALL DFILL (NFILL, 0.0D0, DTSLSR)
C                                       Source ID
            LKEY = 'SU_ID'
            J = IKEY (LKEY, LSUTAB, NSUTAB)
            IF (DVALS(J).NE.DBLANK) IDTSID(NPP) = DVALS(J)
C                                       Source name
            LKEY = 'NAME'
            J = IKEY (LKEY, LSUTAB, NSUTAB)
            IF (DVALS(J).NE.DBLANK) LDTSOU(NPP) = LVALS(J)
C                                       RA of epoch
            LKEY = 'RAEPO'
            J = IKEY (LKEY, LSUTAB, NSUTAB)
            IF (DVALS(J).NE.DBLANK) DTSRA(NPP) = DVALS(J)*15.0D0
C                                       DEC of epoch
            LKEY = 'DECEPO'
            J = IKEY (LKEY, LSUTAB, NSUTAB)
            IF (DVALS(J).NE.DBLANK) DTSDEC(NPP) = DVALS(J)
C                                       Coordinate epoch
            LKEY = 'EPOCH'
            J = IKEY (LKEY, LSUTAB, NSUTAB)
            IF (DVALS(J).NE.DBLANK) DTEPOC(NPP) = DVALS(J)
C                                       Rest frequency (Hz)
            LKEY = 'REST_FQ'
            J = IKEY (LKEY, LSUTAB, NSUTAB)
            DO 230 K = 1, MAXIF
               I = J + (K - 1)
               IF (DVALS(I).NE.DBLANK) THEN
                  DTSUFQ(K,NPP) = DVALS(I) * 1.0D6
                  END IF
230            CONTINUE
C                                       LSR velocity (m/s)
            LKEY = 'LSR_VEL'
            J = IKEY (LKEY, LSUTAB, NSUTAB)
            DO 240 K = 1, MAXIF
               I = J + (K - 1)
               IF (DVALS(I).NE.DBLANK) THEN
                  DTSLSR(K,NPP) = DVALS(I) * 1.0D3
                  END IF
240            CONTINUE
C                                       Valid record ? Check for
C                                       non-blank source name.
            CALL CHBLNK (16, 1, LDTSOU(NPP), NCH)
            IF (NCH.EQ.0) THEN
               WRITE (MSGTXT,1220)
               WERROR = WTRUE
               END IF
            END IF
C                                       Read next SU table record
         IF (.NOT.(WEND.OR.WERROR.OR.WEOF)) GO TO 210
         GO TO 1890
         END IF
C======================================================================
C                                       4: Catalog header
      LKEY = 'CAT_HDR'
      J = IKEY (LKEY, LPARS, NPARS)
      IF (DVALS(J).NE.DBLANK) THEN
C                                       Read keyin_record
         DO 315 J = 1, NMAX
            DVALS(J) = DBLANK
            LVALS(J) = '        '
315         CONTINUE
C
         CALL KEYIN (LCATAB, DVALS, LVALS, NCATAB, LMARK, KMODE, JLUN,
     *      IFINDF, IERR)
         WEOF = (IERR.EQ.1)
         WERROR = (IERR.NE.0).AND.(.NOT.WEOF)
         IF (WERROR) WRITE (MSGTXT,1060) IERR
C                                       If (NOT (ERROR or EOF))
C                                       then process record.
         IF (.NOT.(WERROR.OR.WEOF)) THEN
C                                       Initialize catalog hdr. record
            DTRFRQ = 0.0D0
            DTLRFQ = 0.0D0
            NDTIF = 0
            NDTCHN = 0
            NDTSTK = 0
            CALL FILL (4, 0, IDTSTK)
            IDTCMP = 0
            LDTDAT = ' '
C                                       Reference frequency (Hz)
            LKEY = 'REF_FREQ'
            J = IKEY (LKEY, LCATAB, NCATAB)
            IF (DVALS(J).NE.DBLANK) DTRFRQ = DVALS(J) * 1.0D6
C                                       Line rest frequency (Hz)
            LKEY = 'REST_FQ'
            J = IKEY (LKEY, LCATAB, NCATAB)
            IF (DVALS(J).NE.DBLANK) DTLRFQ = DVALS(J) * 1.0D6
C                                       No. IF's
            LKEY = 'NO_IF'
            J = IKEY (LKEY, LCATAB, NCATAB)
            IF (DVALS(J).NE.DBLANK) NDTIF = DVALS(J)
C                                       No. channels
            LKEY = 'NO_CHAN'
            J = IKEY (LKEY, LCATAB, NCATAB)
            IF (DVALS(J).NE.DBLANK) NDTCHN = DVALS(J)
C                                       Stokes
            LKEY = 'STOKES'
            J = IKEY (LKEY, LCATAB, NCATAB)
            DO 325 K = 1, 4
               M = J + (K - 1)
               IF (DVALS(M).NE.DBLANK) THEN
                  NDTSTK = NDTSTK + 1
                  IF (LVALS(M).EQ.'RR') IDTSTK(NDTSTK) = -1
                  IF (LVALS(M).EQ.'LL') IDTSTK(NDTSTK) = -2
                  IF (LVALS(M).EQ.'RL') IDTSTK(NDTSTK) = -3
                  IF (LVALS(M).EQ.'LR') IDTSTK(NDTSTK) = -4
                  IF (LVALS(M).EQ.'I') IDTSTK(NDTSTK) = 1
                  IF (LVALS(M).EQ.'Q') IDTSTK(NDTSTK) = 2
                  IF (LVALS(M).EQ.'U') IDTSTK(NDTSTK) = 3
                  IF (LVALS(M).EQ.'V') IDTSTK(NDTSTK) = 4
                  END IF
325            CONTINUE
C                                       Sort Stokes array in order
C                                       of increasing absolute value
            DO 335 I = 1, (NDTSTK - 1)
               DO 330 J = (I + 1), NDTSTK
                  IF (ABS(IDTSTK(I)).GT.ABS(IDTSTK(J))) THEN
                     M = IDTSTK(I)
                     IDTSTK(I) = IDTSTK(J)
                     IDTSTK(J) = M
                     END IF
330               CONTINUE
335            CONTINUE
C                                       Reference date
            LKEY = 'REF_DATE'
            J = IKEY (LKEY, LCATAB, NCATAB)
            IF (DVALS(J).NE.DBLANK) LDTDAT = LVALS(J)
C                                       Compressed data ?
            LKEY = 'COMPRESS'
            J = IKEY (LKEY, LCATAB, NCATAB)
            IF (DVALS(J).NE.DBLANK) IDTCMP = 1
C                                       Valid record ?
            CALL CHBLNK (8, 1, LDTDAT, NCH)
            IF ((NCH.EQ.0).OR.(NDTIF.EQ.0).OR.(NDTCHN.EQ.0)) THEN
               WRITE (MSGTXT,1325)
               WERROR = WTRUE
            ELSE
               WDTCAT = WTRUE
               END IF
            END IF
         GO TO 1890
         END IF
C======================================================================
C                                       5: CAL_ERR entry
      LKEY = 'CAL_ERR'
      J = IKEY (LKEY, LPARS, NPARS)
      IF (DVALS(J).NE.DBLANK) THEN
C                                       Check if linearized polzn.
C                                       model is requested
         LKEY = 'LINPOL'
         J = IKEY (LKEY, LPARS, NPARS)
         WDTLIN = (DVALS(J).NE.DBLANK)
C                                       Repeat (read keyin_record)
C                                       until (EOF or ERR or ENDCAT)
410      DO 415 J = 1, NMAX
            DVALS(J) = DBLANK
            LVALS(J) = '        '
415         CONTINUE
C
         CALL KEYIN (LCETAB, DVALS, LVALS, NCETAB, LMARK, KMODE,
     *      JLUN, IFINDF, IERR)
         WEOF = (IERR.EQ.1)
         WERROR = (IERR.NE.0).AND.(.NOT.WEOF)
         IF (WERROR) WRITE (MSGTXT,1060) IERR
C                                       END CAT ?
         LKEY = 'END'
         J = IKEY (LKEY, LCETAB, NCETAB)
         WEND = (DVALS(J).NE.DBLANK)
C                                       If (not (ERR or EOF or END))
C                                       then process record
         IF (.NOT.(WEOF.OR.WERROR.OR.WEND)) THEN
C                                       Initialize record
            NDTAER = NDTAER + 1
            NPP = NDTAER
            LDTAER(NPP) = ' '
            IDTEFQ(NPP) = 0
            NFILL = 2 * MAXPRD
            CALL RFILL (NFILL, 0.0, RDTBP(1,1,1,NPP))
            CALL RFILL (NFILL, 0.0, RDTRAT(1,1,1,NPP))
            CALL RFILL (NFILL, 0.0, RDTDEL(1,1,1,NPP))
            CALL RFILL (NFILL, 0.0, RDTPHS(1,1,1,NPP))
            CALL RFILL (NFILL, 0.0, RDTFED(1,1,1,NPP))
            CALL RFILL (NFILL, 0.0, RDTGAN(1,1,1,NPP))
            CALL RFILL (NFILL, 0.0, RDTTSY(1,1,1,NPP))
            CALL RFILL (NFILL, 0.0, RDTGER(1,1,1,NPP))
C                                       Extract ant. name, FQ_ID
            LKEY = 'STATION'
            J = IKEY (LKEY, LCETAB, NCETAB)
            IF (DVALS(J).NE.DBLANK) LDTAER(NPP) = LVALS(J)
C
            LKEY = 'FQID'
            J = IKEY (LKEY, LCETAB, NCETAB)
            IF (DVALS(J).NE.DBLANK) IDTEFQ(NPP) = DVALS(J)
C                                       Extract the other fields
C                                       in the cal. error record.
            DO 430 J = 1, NCEKEY
               LKEY = LCEKEY(J)(1:ITRIM(LCEKEY(J))) // '_1R'
               JA = IKEY (LKEY, LCETAB, NCETAB)
C                                       Loop over Stokes (R,L)
               DO 425 K = 1, 2
C                                       Loop over IF
                  DO 422 M = 1, MAXIF
C                                       Loop over parameter index
                     DO 420 KK = 1, MAXERP
                        JJ = (JA - 1) + (K - 1) * MAXPRD +
     *                     (M - 1) * MAXERP + KK
                        IF (DVALS(JJ).NE.DBLANK) THEN
                           IF (LCEKEY(J).EQ.'BP')
     *                        RDTBP(KK,M,K,NPP) = DVALS(JJ)
                           IF (LCEKEY(J).EQ.'RATE')
     *                        RDTRAT(KK,M,K,NPP) = DVALS(JJ)
                           IF (LCEKEY(J).EQ.'DELA')
     *                        RDTDEL(KK,M,K,NPP) = DVALS(JJ)
                           IF (LCEKEY(J).EQ.'PHAS')
     *                        RDTPHS(KK,M,K,NPP) = DVALS(JJ)
                           IF (LCEKEY(J).EQ.'FEED')
     *                        RDTFED(KK,M,K,NPP) = DVALS(JJ)
                           IF (LCEKEY(J).EQ.'GAIN')
     *                        RDTGAN(KK,M,K,NPP) = DVALS(JJ)
                           IF (LCEKEY(J).EQ.'TSYS')
     *                        RDTTSY(KK,M,K,NPP) = DVALS(JJ)
                           IF (LCEKEY(J).EQ.'GERR')
     *                        RDTGER(KK,M,K,NPP) = DVALS(JJ)
                           END IF
420                     CONTINUE
422                  CONTINUE
425               CONTINUE
430            CONTINUE
C                                       Valid record ?
            CALL CHBLNK (16, 1, LDTAER(NPP), NCH)
            IF ((NCH.EQ.0).OR.(IDTEFQ(NPP).EQ.0)) THEN
               WERROR = WTRUE
               WRITE (MSGTXT,1430)
               END IF
C
            END IF
C                                       Read next keyin_record
         IF (.NOT.(WERROR.OR.WEOF.OR.WEND)) GO TO 410
         GO TO 1890
         END IF
C======================================================================
C                                       6: SU_MODEL entry
      LKEY = 'SU_MODEL'
      J = IKEY (LKEY, LPARS, NPARS)
      IF (DVALS(J).NE.DBLANK) THEN
C                                       Repeat (read keyin_record)
C                                       until (EOF or ERROR or END)
510      DO 515 J = 1, NMAX
            DVALS(J) = DBLANK
            LVALS(J) = '        '
515         CONTINUE
C
         CALL KEYIN (LSMTAB, DVALS, LVALS, NSMTAB, LMARK, KMODE, JLUN,
     *      IFINDF, IERR)
         WEOF = (IERR.EQ.1)
         WERROR = (IERR.NE.0).AND.(.NOT.WEOF)
         IF (WERROR) WRITE (MSGTXT,1060) IERR
C                                       END ?
         LKEY = 'END'
         J = IKEY (LKEY, LSMTAB, NSMTAB)
         WEND = (DVALS(J).NE.DBLANK)
C                                       If (NOT (EOF or ERR or END))
C                                       then process record
         IF (.NOT.(WEOF.OR.WEND.OR.WERROR)) THEN
C                                       Initialize SU_MODEL record
            NDTSMD = NDTSMD + 1
            NPP = NDTSMD
            LDTSMD(NPP) = ' '
            CALL RFILL (12, 0.0, RDTSPM(1,NPP))
            CALL RFILL (5, 0.0, RDTSPC(1,NPP))
            CALL RFILL (2, 0.0, RDTVEL(1,NPP))
C                                       Extract source name
            LKEY = 'SOURCE'
            J = IKEY (LKEY, LSMTAB, NSMTAB)
            IF (DVALS(J).NE.DBLANK) LDTSMD(NPP) = LVALS(J)
C                                       Component parameters
            LKEY = 'TYPE'
            J = IKEY (LKEY, LSMTAB, NSMTAB)
            DO 520 K = 1, 12
               M = J + (K - 1)
               IF (DVALS(M).NE.DBLANK) RDTSPM(K,NPP) = DVALS(M)
520            CONTINUE
C                                       Spectral index
            LKEY = 'SPEC'
            J = IKEY (LKEY, LSMTAB, NSMTAB)
            DO 530 K = 1, 5
               M = J + (K - 1)
               IF (DVALS(M).NE.DBLANK) RDTSPC(K,NPP) = DVALS(M)
530            CONTINUE
C                                       Velocity information
            LKEY = 'VEL'
            J = IKEY (LKEY, LSMTAB, NSMTAB)
            DO 540 K = 1, 2
               M = J + (K - 1)
               IF (DVALS(M).NE.DBLANK) RDTVEL(K,NPP) = DVALS(M)
540            CONTINUE
C                                       Valid record ?
            CALL CHBLNK (16, 1, LDTSMD(NPP), NCH)
            IF (NCH.EQ.0) THEN
               WERROR = WTRUE
               WRITE (MSGTXT,1540)
               END IF
            END IF
C                                       Read next keyin record
         IF (.NOT.(WEOF.OR.WERROR.OR.WEND)) GO TO 510
         GO TO 1890
         END IF
C======================================================================
C                                       7: Schedule entry
      LKEY = 'SCHEDULE'
      J = IKEY (LKEY, LPARS, NPARS)
      IF (DVALS(J).NE.DBLANK) THEN
C
         IDAY = DDAY
         IMTH = DMONTH
         IYEAR = DYEAR
C                                       Repeat (read keyin_record)
C                                       until (EOF or ERROR or END)
610      DO 615 J = 1, NMAX
            DVALS(J) = DBLANK
            LVALS(J) = '        '
615         CONTINUE
C
         CALL KEYIN (LSHTAB, DVALS, LVALS, NSHTAB, LMARK, KMODE,
     *      JLUN, IFINDF, IERR)
         WEOF = (IERR.EQ.1)
         WERROR = (IERR.NE.0).AND.(.NOT.WEOF)
         IF (WERROR) WRITE (MSGTXT,1060) IERR
C                                       END CAT ?
         LKEY = 'END'
         J = IKEY (LKEY, LSHTAB, NSHTAB)
         WEND = (DVALS(J).NE.DBLANK)
C                                       If (NOT (ERR or EOF or END))
C                                       then process record
         IF (.NOT.(WEND.OR.WERROR.OR.WEOF)) THEN
C                                       Initialize schedule record
            NDTSCH = NDTSCH + 1
            NPP = NDTSCH
            LDTSCH(NPP) = ' '
            LDTSEL(NPP) = ' '
            IDTDAY(NPP) = IDAY
            IDTMTH(NPP) = IMTH
            IDTYR(NPP) = IYEAR
            IDTSSB(NPP) = 1
            IDTSFQ(NPP) = 1
            RDTSTT(NPP) = 0.0
            RDTEND(NPP) = 0.0
            RDTINT(NPP) = 1.0
C                                       Day, month, year
            LKEY = 'DAY'
            J = IKEY (LKEY, LSHTAB, NSHTAB)
            IF (DVALS(J).NE.DBLANK) THEN
               IDTDAY(NPP) = DVALS(J)
               IDAY = IDTDAY(NPP)
               END IF
C
            LKEY = 'MONTH'
            J = IKEY (LKEY, LSHTAB, NSHTAB)
            IF (DVALS(J).NE.DBLANK) THEN
               IDTMTH(NPP) = DVALS(J)
               IMTH = IDTMTH(NPP)
               END IF
C
            LKEY = 'YEAR'
            J = IKEY (LKEY, LSHTAB, NSHTAB)
            IF (DVALS(J).NE.DBLANK) THEN
               IDTYR(NPP) = DVALS(J)
               IYEAR = IDTYR(NPP)
               END IF
C                                       Start time / days
            LKEY = 'START'
            J = IKEY (LKEY, LSHTAB, NSHTAB)
            IF (DVALS(J).NE.DBLANK) RDTSTT(NPP) = DVALS(J) / 24.0D0
C                                       Stop time / days
            LKEY = 'STOP'
            J = IKEY (LKEY, LSHTAB, NSHTAB)
            IF (DVALS(J).NE.DBLANK) RDTEND(NPP) = DVALS(J) / 24.0D0
C                                       Source name
            LKEY = 'SOURCE'
            J = IKEY (LKEY, LSHTAB, NSHTAB)
            IF (DVALS(J).NE.DBLANK) LDTSCH(NPP) = LVALS(J)
C                                       Subarray
            LKEY = 'SUBA'
            J = IKEY (LKEY, LSHTAB, NSHTAB)
            IF (DVALS(J).NE.DBLANK) IDTSSB(NPP) = DVALS(J)
C                                       Freqid
            LKEY = 'FQID'
            J = IKEY (LKEY, LSHTAB, NSHTAB)
            IF (DVALS(J).NE.DBLANK) IDTSFQ(NPP) = DVALS(J)
C                                       Integration time / days
            LKEY = 'TINT'
            J = IKEY (LKEY, LSHTAB, NSHTAB)
            IF (DVALS(J).NE.DBLANK) RDTINT(NPP) = DVALS(J) / 86400.0D0
C                                       Baseline selection
            WSERR = WFALSE
            LKEY = 'SELECT'
            J = IKEY (LKEY, LSHTAB, NSHTAB)
            IF (DVALS(J).NE.DBLANK) THEN
               LDTSEL(NPP) = LVALS(J)
               LPRE = LVALS(J)(1:1)
               WSERR = ((LPRE.NE.'*').AND.(LPRE.NE.'-').AND.
     *            (LPRE.NE.' '))
               END IF
C                                       Valid record ?
            CALL CHBLNK (16, 1, LDTSCH(NPP), NCH)
            IF ((RDTSTT(NPP).GE.RDTEND(NPP)) .OR. (NCH.EQ.0) .OR.
     *         (IDAY.EQ.0) .OR. (IMTH.EQ.0) .OR. (IYEAR.EQ.0) .OR.
     *         WSERR) THEN
               WERROR = WTRUE
               WRITE (MSGTXT,1680)
               END IF
            END IF
C                                       Read next keyin_record
         IF (.NOT.(WERROR.OR.WEOF.OR.WEND)) GO TO 610
         GO TO 1890
         END IF
C======================================================================
C                                       8: FLAG entry
      LKEY = 'FLAG'
      J = IKEY (LKEY, LPARS, NPARS)
      IF (DVALS(J).NE.DBLANK) THEN
C
         IDAY = DDAY
         IMTH = DMONTH
         IYEAR = DYEAR
C                                       Repeat (read keyin_record)
C                                       until (EOF or ERROR or END)
710      DO 715 J = 1, NMAX
            DVALS(J) = DBLANK
            LVALS(J) = '        '
715         CONTINUE
C
         CALL KEYIN (LFGTAB, DVALS, LVALS, NFGTAB, LMARK, KMODE,
     *      JLUN, IFINDF, IERR)
         WEOF = (IERR.EQ.1)
         WERROR = (IERR.NE.0).AND.(.NOT.WEOF)
         IF (WERROR) WRITE (MSGTXT,1060) IERR
C                                       END CAT ?
         LKEY = 'END'
         J = IKEY (LKEY, LFGTAB, NFGTAB)
         WEND = (DVALS(J).NE.DBLANK)
C                                       If (not (ERR or EOF or END))
C                                       then process record
         IF (.NOT.(WEND.OR.WEOF.OR.WERROR)) THEN
C                                       Initialize flag record
            NDTFG = NDTFG + 1
            NPP = NDTFG
            LDTFGA(NPP) = ' '
            LDTFGB(NPP) = ' '
            LDTFGS(NPP) = ' '
            IDTFDY(NPP) = IDAY
            IDTFMH(NPP) = IMTH
            IDTFYR(NPP) = IYEAR
            RDTSFG(NPP) = 0.0
            RDTEFG(NPP) = 0.0
            IDTBIF(NPP) = 0
            IDTEIF(NPP) = 0
            IDTBCH(NPP) = 0
            IDTECH(NPP) = 0
            IDTFFQ(NPP) = 0
            IDTSBF(NPP) = 0
            CALL FILL (4, 0, IDTSKF(1,NPP))
C                                       Ant_1
            LKEY = 'ANT_1'
            J = IKEY (LKEY, LFGTAB, NFGTAB)
            IF (DVALS(J).NE.DBLANK) LDTFGA(NPP) = LVALS(J)
C                                       Ant_2
            LKEY = 'ANT_2'
            J = IKEY (LKEY, LFGTAB, NFGTAB)
            IF (DVALS(J).NE.DBLANK) LDTFGB(NPP) = LVALS(J)
C                                       Source
            LKEY = 'SOURCE'
            J = IKEY (LKEY, LFGTAB, NFGTAB)
            IF (DVALS(J).NE.DBLANK) LDTFGS(NPP) = LVALS(J)
C                                       Day, month, year
            LKEY = 'DAY'
            J = IKEY (LKEY, LFGTAB, NFGTAB)
            IF (DVALS(J).NE.DBLANK) THEN
               IDTFDY(NPP) = DVALS(J)
               IDAY = IDTFDY(NPP)
               END IF
            LKEY = 'MONTH'
            J = IKEY (LKEY, LFGTAB, NFGTAB)
            IF (DVALS(J).NE.DBLANK) THEN
               IDTFMH(NPP) = DVALS(J)
               IMTH = IDTFMH(NPP)
               END IF
            LKEY = 'YEAR'
            J = IKEY (LKEY, LFGTAB, NFGTAB)
            IF (DVALS(J).NE.DBLANK) THEN
               IDTFYR(NPP) = DVALS(J)
               IYEAR = IDTFYR(NPP)
               END IF
C                                       Start, stop time / days
            LKEY = 'START'
            J = IKEY (LKEY, LFGTAB, NFGTAB)
            IF (DVALS(J).NE.DBLANK) RDTSFG(NPP) = DVALS(J) / 24.0D0
            LKEY = 'STOP'
            J = IKEY (LKEY, LFGTAB, NFGTAB)
            IF (DVALS(J).NE.DBLANK) RDTEFG(NPP) = DVALS(J) / 24.0D0
C                                       BIF, EIF
            LKEY = 'BIF'
            J = IKEY (LKEY, LFGTAB, NFGTAB)
            IF (DVALS(J).NE.DBLANK) IDTBIF(NPP) = DVALS(J)
            LKEY = 'EIF'
            J = IKEY (LKEY, LFGTAB, NFGTAB)
            IF (DVALS(J).NE.DBLANK) IDTEIF(NPP) = DVALS(J)
C                                       BCHAN, ECHAN
            LKEY = 'BCHAN'
            J = IKEY (LKEY, LFGTAB, NFGTAB)
            IF (DVALS(J).NE.DBLANK) IDTBCH(NPP) = DVALS(J)
            LKEY = 'ECHAN'
            J = IKEY (LKEY, LFGTAB, NFGTAB)
            IF (DVALS(J).NE.DBLANK) IDTECH(NPP) = DVALS(J)
C                                       Frequency ID
            LKEY = 'FQID'
            J = IKEY (LKEY, LFGTAB, NFGTAB)
            IF (DVALS(J).NE.DBLANK) IDTFFQ(NPP) = DVALS(J)
C                                       Subarray
            LKEY = 'SUBA'
            J = IKEY (LKEY, LFGTAB, NFGTAB)
            IF (DVALS(J).NE.DBLANK) IDTSBF(NPP) = DVALS(J)
C                                       Stokes PFLAG
            LKEY = 'PFLAG'
            J = IKEY (LKEY, LFGTAB, NFGTAB)
            IF (DVALS(J).NE.DBLANK) THEN
               DO 740 K = 1, MIN (4, ITRIM(LVALS(J)))
                  IF (LVALS(J)(K:K).EQ.'1') IDTSKF(K,NPP) = 1
740            CONTINUE
               END IF
C                                       Valid record ?
            CALL CHBLNK (8, 1, LDTFGA(NPP), NCH)
            CALL CHBLNK (8, 1, LDTFGS(NPP), NCHA)
            IF ((RDTSFG(NPP).GE.RDTEFG(NPP)).OR.(NCH.EQ.0).OR.
     *         (NCHA.EQ.0)) THEN
               WERROR = WTRUE
               WRITE (MSGTXT,1740)
               END IF
            END IF
C                                       Read next keyin_record
         IF (.NOT.(WERROR.OR.WEOF.OR.WEND)) GO TO 710
         GO TO 1890
         END IF
C======================================================================
C                                       9: BP_TABLE entry
      LKEY = 'BP_TABLE'
      J = IKEY (LKEY, LPARS, NPARS)
      IF (DVALS(J).NE.DBLANK) THEN
C                                       Initialize BP table keywords
         LDTBPT = '        '
         NDTBPA = 0
         IDTBPN = 0
         IDTBPV = 0
         IDAY = DDAY
         IMTH = DMONTH
         IYEAR = DYEAR
C                                       Extract table version no.
         LKEY = 'VER'
         J = IKEY (LKEY, LPARS, NPARS)
         IF (DVALS(J).NE.DBLANK) IDTBPV = DVALS(J)
C                                       Repeat (read keyin_record)
C                                       until (EOF or ERROR or END)
 810     DO 815 J = 1, NMAX
            DVALS(J) = DBLANK
            LVALS(J) = '        '
 815        CONTINUE
C
         CALL KEYIN (LBPTAB, DVALS, LVALS, NBPTAB, LMARK, KMODE, JLUN,
     *      IFINDF, IERR)
         WEOF = (IERR.EQ.1)
         WERROR = (IERR.NE.0).AND.(.NOT.WEOF)
         IF (WERROR) WRITE (MSGTXT,1060) IERR
C                                       END CAT ?
         LKEY = 'END'
         J = IKEY (LKEY, LBPTAB, NBPTAB)
         WEND = (DVALS(J).NE.DBLANK)
C                                       If (not (ERR or EOF or END))
C                                       then process record.
         IF (.NOT.(WEND.OR.WEOF.OR.WERROR)) THEN
C                                       Case record_type of:
C                                       i) KEY record
            LKEY = 'KEY'
            J = IKEY (LKEY, LBPTAB, NBPTAB)
            IF (DVALS(J).NE.DBLANK) THEN
C                                       Keyword record:
C                                       Extract bp_type
               LKEY = 'BPTYPE'
               J = IKEY (LKEY, LBPTAB, NBPTAB)
               IF (DVALS(J).NE.DBLANK) LDTBPT = LVALS(J)
C                                       Extract no_ant
               LKEY = 'NANT'
               J = IKEY (LKEY, LBPTAB, NBPTAB)
               IF (DVALS(J).NE.DBLANK) NDTBPA = DVALS(J)
C                                       Numshf
               LKEY = 'NUMSHF'
               J = IKEY (LKEY, LBPTAB, NBPTAB)
               IF (DVALS(J).NE.DBLANK) IDTBPN = DVALS(J)
C                                       ii) BP entry
            ELSE
               LKEY = 'STATION'
               J = IKEY (LKEY, LBPTAB, NBPTAB)
               IF (DVALS(J).NE.DBLANK) THEN
C                                       Initialization
                  NDTBPD = NDTBPD + 1
                  NPP = NDTBPD
                  IDTBPY(NPP) = IYEAR
                  IDTBPM(NPP) = IMTH
                  IDTBPD(NPP) = IDAY
                  RDTBPT(NPP) = 0.0
                  RDTBPI(NPP) = 0.0
                  RDTBPS(NPP) = 0.0
                  IDTBPX(NPP) = 0
                  IDTBPC(NPP) = 0
                  IDTBFQ(NPP) = 0
                  IDTBSB(NPP) = 0
                  CALL FILL (MAXIF, 0, IDTBFI(1,NPP))
                  CALL FILL (MAXIF, 0, IDTBFP(1,NPP))
                  CALL FILL (2*MAXIF, 0, IDTBFC(1,NPP))
C                                       Station name
                  LDTBAN(NPP) = LVALS(J)
C                                       Freq. id.
                  LKEY = 'FQID'
                  J = IKEY (LKEY, LBPTAB, NBPTAB)
                  IF (DVALS(J).NE.DBLANK) IDTBFQ(NPP) = DVALS(J)
C                                       Subarray
                  LKEY = 'SUBA'
                  J = IKEY (LKEY, LBPTAB, NBPTAB)
                  IF (DVALS(J).NE.DBLANK) IDTBSB(NPP) = DVALS(J)
C                                       Day, month, year
                  LKEY = 'DAY'
                  J = IKEY (LKEY, LBPTAB, NBPTAB)
                  IF (DVALS(J).NE.DBLANK) THEN
                     IDTBPD(NPP) = DVALS(J)
                     IDAY = IDTBPD(NPP)
                     END IF
                  LKEY = 'MONTH'
                  J = IKEY (LKEY, LBPTAB, NBPTAB)
                  IF (DVALS(J).NE.DBLANK) THEN
                     IDTBPM(NPP) = DVALS(J)
                     IMTH = IDTBPM(NPP)
                     END IF
                  LKEY = 'YEAR'
                  J = IKEY (LKEY, LBPTAB, NBPTAB)
                  IF (DVALS(J).NE.DBLANK) THEN
                     IDTBPY(NPP) = DVALS(J)
                     IYEAR = IDTBPY(NPP)
                     END IF
C                                       Start time (days)
                  LKEY = 'START'
                  J = IKEY (LKEY, LBPTAB, NBPTAB)
                  IF (DVALS(J).NE.DBLANK) RDTBPT(NPP) = DVALS(J)/24.0D0
C                                       End time
                  LKEY = 'STOP'
                  J = IKEY (LKEY, LBPTAB, NBPTAB)
                  IF (DVALS(J).NE.DBLANK) RDTBPE(NPP) = DVALS(J)/24.0D0
C                                       Integration time of each entry
C                                       (convert from sec to days)
                  LKEY = 'TINT'
                  J = IKEY (LKEY, LBPTAB, NBPTAB)
                  IF (DVALS(J).NE.DBLANK) RDTBPS(NPP) = DVALS(J)
                  RDTBPS(NPP) = RDTBPS(NPP) / 86400.0D0
C                                       BP spacing in time (convert
C                                       to days from minutes)
                  LKEY = 'INTERV'
                  J = IKEY (LKEY, LBPTAB, NBPTAB)
                  IF (DVALS(J).NE.DBLANK) RDTBPI(NPP) = DVALS(J)
                  RDTBPI(NPP) = RDTBPI(NPP) / 1440.0D0
C                                       BP type (only calibration
C                                       errors supported at present)
                  LKEY = 'CAL_ERR'
                  J = IKEY (LKEY, LBPTAB, NBPTAB)
                  IF (DVALS(J).NE.DBLANK) IDTBPC(NPP) = 1
C                                       XC or AC ?
                  LKEY = 'AC'
                  J = IKEY (LKEY, LBPTAB, NBPTAB)
                  IF (DVALS(J).NE.DBLANK) IDTBPX(NPP) = 1
                  LKEY = 'XC'
                  J = IKEY (LKEY, LBPTAB, NBPTAB)
                  IF (DVALS(J).NE.DBLANK) IDTBPX(NPP) = 0
C                                       Flagged channels/IF/polzn.
                  LKEY = 'FLAGPOL'
                  J = IKEY (LKEY, LBPTAB, NBPTAB)
                  DO 830 K = 1,MAXIF
                     I = J + K - 1
                     IF (DVALS(I).NE.DBLANK) IDTBFP(K,NPP) = DVALS(I)
 830                 CONTINUE
C
                  LKEY = 'FLAGIF'
                  J = IKEY (LKEY, LBPTAB, NBPTAB)
                  DO 835 K = 1, MAXIF
                     I = J + K - 1
                     IF (DVALS(I).NE.DBLANK) IDTBFI(K,NPP) = DVALS(I)
 835                 CONTINUE
C
                  LKEY = 'FLAGCHAN'
                  J = IKEY (LKEY, LBPTAB, NBPTAB)
                  DO 840 K = 1, 2*MAXIF
                     I = J + K - 1
                     IF (DVALS(I).NE.DBLANK) IDTBFC(K,NPP) = DVALS(I)
 840                 CONTINUE
C                                       iii) Other: error
               ELSE
                  WERROR = WTRUE
                  WRITE (MSGTXT,1870)
                  END IF
C                                       Endcase (BP record_type)
               END IF
            END IF
C                                       Read next keyin_record
         IF (.NOT.(WERROR.OR.WEOF.OR.WEND)) GO TO 810
         GO TO 1890
         END IF
C======================================================================
C                                       10: Other: error
      WERROR = WTRUE
      WRITE (MSGTXT, 1810)
C                                       Endcase
 1890 CONTINUE
C                                       Until (EOF or ERROR)
 1900 IF (.NOT.(WEOF.OR.WERROR)) GO TO 50
C                                       Print any error messages
      IF (WERROR) THEN
         CALL MSGWRT (8)
         GO TO 1999
         END IF
C                                       Check for missing data
C                                       in DTPM.INC.
      WERROR = ((NDTSUB.EQ.0).OR.(NDTFQD.EQ.0).OR.
     *   (NDTSID.EQ.0).OR.(.NOT.WDTCAT))
      IF (WERROR) THEN
         WRITE (MSGTXT,1905)
      ELSE
C                                       Consolidate the common
C                                       block data in DTPM.INC.
C                                       Julian day no. of ref. date
         READ (LDTDAT,1915) IT(3), IT(2), IT(1)
         CALL FILL (3, 0, IT(4))
         CALL DAT2JD (IT, DTJDAT)
C                                       Adjust all schedule times
C                                       wrt reference date.
         DO 1910 K = 1, NDTSCH
            IT(1) = IDTYR(K)
            IT(2) = IDTMTH(K)
            IT(3) = IDTDAY(K)
            CALL FILL (3, 0, IT(4))
            CALL DAT2JD (IT, DJD)
            IOFF = DJD - DTJDAT + 0.5
            RDTSTT(K) = RDTSTT(K) + IOFF
            RDTEND(K) = RDTEND(K) + IOFF
 1910       CONTINUE
C                                       Consolidate FLAG records
         DO 1920 K = 1, NDTFG
C                                       Adjust all FLAG times wrt
C                                       reference date.
            IT(1) = IDTFYR(K)
            IT(2) = IDTFMH(K)
            IT(3) = IDTFDY(K)
            CALL FILL (3, 0, IT(4))
            CALL DAT2JD (IT, DJD)
            IOFF = DJD - DTJDAT + 0.5D0
            RDTSFG(K) = RDTSFG(K) + IOFF
            RDTEFG(K) = RDTEFG(K) + IOFF
C                                       Convert BIF,EIF defaults.
            IF (IDTBIF(K).LE.0) IDTBIF(K) = 1
            IF (IDTEIF(K).LE.0) IDTEIF(K) = NDTIF
C                                       Convert BCHAN, ECHAN defaults.
            IF (IDTBCH(K).LE.0) IDTBCH(K) = 1
            IF (IDTECH(K).LE.0) IDTECH(K) = NDTCHN
 1920       CONTINUE
C                                       Consolidate BP records.
         DO 1925 K = 1, NDTBPD
C                                       Adjust all BP times wrt
C                                       reference date
            IT(1) = IDTBPY(K)
            IT(2) = IDTBPM(K)
            IT(3) = IDTBPD(K)
            CALL FILL (3, 0, IT(4))
            CALL DAT2JD (IT, DJD)
            IOFF = DJD - DTJDAT + 0.5D0
            RDTBPT(K) = RDTBPT(K) + IOFF
            RDTBPE(K) = RDTBPE(K) + IOFF
 1925       CONTINUE
         END IF
C                                       Exit
C                                       Hard close: text file
 1999 CALL ZTXCLS (JLUN, IFINDF, IERR)
C
 999  RETURN
C----------------------------------------------------------------------
1050  FORMAT ('DTPARM: ERR',I4,' OPENING ',A48)
1060  FORMAT ('DTPARM: ERR',I4,' READING TEXT FILE')
1085  FORMAT ('DTPARM: INVALID AN TABLE ENTRY')
1220  FORMAT ('DTPARM: INVALID SU TABLE ENTRY')
1325  FORMAT ('DTPARM: INVALID CATALOG HDR RECORD')
1430  FORMAT ('DTPARM: INVALID CAL_ERR RECORD')
1540  FORMAT ('DTPARM: INVALID SU_MODEL RECORD')
1680  FORMAT ('DTPARM: INVALID SCHEDULE RECORD')
1740  FORMAT ('DTPARM: INVALID FLAG RECORD')
1810  FORMAT ('DTPARM: UNIDENTIFIED GROUP')
1870  FORMAT ('DTPARM: INVALID BP TABLE ENTRY')
1905  FORMAT ('DTPARM: MISSING GROUPS IN EXTERNAL FILE')
1915  FORMAT (I2,1X,I2,1X,I2)
9007  FORMAT ('_',I1,A1)
9010  FORMAT ('_',I2,A1)
      END
