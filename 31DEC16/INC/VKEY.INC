C                                                          Include VKEY
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2003, 2007
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
C                                       FITS Keyword info
C                                       Required keywords.
      DATA RWORD /'SIMPLE  ','BITPIX  ','NAXIS   ','NAXIS1  ',
     *            'NAXIS2  ','NAXIS3  ','NAXIS4  ','NAXIS5  ',
     *            'NAXIS6  ','NAXIS7  ','NAXIS8  '/
C                                       normal keywords.
      DATA NWORD /'OBJECT  ','TELESCOP','INSTRUME','OBSERVER',
     *            'DATE-OBS','DATE-MAP','BSCALE  ','BZERO   ',
     *            'BUNIT   ','EQUINOX ','BLANK   ','VELREF  ',
     *            'ALTRVAL ','ALTRPIX ','OBSRA   ','OBSDEC  ',
     *            'RESTFREQ','XSHIFT  ','YSHIFT  ','DATAMAX ',
     *            'DATAMIN '/
C                                       Axis keywords.
      DATA AWORD /'CTYPE1  ','CRVAL1  ','CDELT1  ','CRPIX1  ',
     *            'CROTA1  ','CTYPE2  ','CRVAL2  ','CDELT2  ',
     *            'CRPIX2  ','CROTA2  ','CTYPE3  ','CRVAL3  ',
     *            'CDELT3  ','CRPIX3  ','CROTA3  ','CTYPE4  ',
     *            'CRVAL4  ','CDELT4  ','CRPIX4  ','CROTA4  ',
     *            'CTYPE5  ','CRVAL5  ','CDELT5  ','CRPIX5  ',
     *            'CROTA5  ','CTYPE6  ','CRVAL6  ','CDELT6  ',
     *            'CRPIX6  ','CROTA6  ','CTYPE7  ','CRVAL7  ',
     *            'CDELT7  ','CRPIX7  ','CROTA7  ','CTYPE8  ',
     *            'CRVAL8  ','CDELT8  ','CRPIX8  ','CROTA8  '/
C                                       Group keywords.
      DATA GWORD /'GROUPS  ','GCOUNT  ','PCOUNT  ','PTYPE1  ',
     *            'PSCAL1  ','PZERO1  ','PTYPE2  ','PSCAL2  ',
     *            'PZERO2  ','PTYPE3  ','PSCAL3  ','PZERO3  ',
     *            'PTYPE4  ','PSCAL4  ','PZERO4  ','PTYPE5  ',
     *            'PSCAL5  ','PZERO5  ','PTYPE6  ','PSCAL6  ',
     *            'PZERO6  ','PTYPE7  ','PSCAL7  ','PZERO7  ',
     *            'PTYPE8  ','PSCAL8  ','PZERO8  ','PTYPE9  ',
     *            'PSCAL9  ','PZERO9  ','PTYPE10 ','PSCAL10 ',
     *            'PZERO10 ','PTYPE11 ','PSCAL11 ','PZERO11 ',
     *            'PTYPE12 ','PSCAL12 ','PZERO12 ','PTYPE13 ',
     *            'PSCAL13 ','PZERO13 ','PTYPE14 ','PSCAL14 ',
     *            'PZERO14 ','PSCAL15 ','PZERO15 ','PSCAL16 ',
     *            'PZERO16 ','PSCAL17 ','PZERO17 ','PSCAL18 ',
     *            'PZERO18 ','PSCAL19 ','PZERO19 ','PSCAL20 ',
     *            'PZERO20 '/
C                                       1=Logical variable
C                                       2=Number
C                                       3=String on real boundary
C                                       4=String on integer boundary
C                                       Special cases
C                                       5=scaling parameters
C                                       6=Rand parm scalling factors.
C                                       7=Rand parm offsets.
C                                       8=Rand parm names.
C                                       9=blanking value.
C                                       10=value = pointer.
C                                       11=NUMERIC, omit if zero.
      DATA RTYPE /1, 2, 2, 2,    2, 2, 2, 2,    2, 2/
      DATA NTYPE /3, 3, 3, 3,    3, 3, 5, 5,    3, 2, 9, 8*11,
     *            2, 2/
      DATA ATYPE /3, 2, 2, 2,    2, 3, 2, 2,    2, 2, 3, 2,
     *            2, 2, 2, 3,    2, 2, 2, 2,    3, 2, 2, 2,
     *            2, 3, 2, 2,    2, 2, 3, 2,    2, 2, 2/
      DATA GTYPE /1, 2, 2, 8,    6, 7, 8, 6,    7, 8, 6, 7,
     *            8, 6, 7, 8,    6, 7, 8, 6,    7, 8, 6, 7,
     *            8, 6, 7, 8,    6, 7, 8, 6,    7, 8, 6, 7,
     *            8, 6, 7, 8,    6, 7, 8, 6,    7, 6, 7, 6,
     *            7, 6, 7, 6,    7, 6, 7, 6,    7/
C                                       1000*nbytes + 100*offset +
C                                       position of pointer in common
C                       "KIBPX"KIDIM KINAX
      DATA RPOINT / 1000, 2000, 2040, 2041,   2141, 2241, 2341, 2441,
     *              2541, 2641/
C                  KHOBJ KHTEL KHINS KHOBS   KHDOB KHDMP"KDBSC"KDBZE"
      DATA NPOINT / 8001, 8002, 8003, 8004,   8005, 8006, 8000, 8000,
C                  KHBUN KREPO KRBLK KIALT   KDARV KRARP KDORA KDODE
     *              8007, 4013, 4016, 2045,   8033, 4023, 8030, 8031,
C                  KDRST KRXSH KRYSH KRDMX   KRDMN
     *              8032, 4024, 4025, 4014,   4015/
C                  KHCTP KDCRV KRCIC KRCRP   KRCRT
      DATA APOINT / 8009, 8029, 4010, 4011,   4012, 8109, 8129, 4110,
     *              4111, 4112, 8209, 8229,   4210, 4211, 4212, 8309,
     *              8329, 4310, 4311, 4312,   8409, 8429, 4410, 4411,
     *              4412, 8509, 8529, 4510,   4511, 4512, 8609, 8629,
     *              4610, 4611, 4612/
C                        KIGCN KIPCN PTYPE   PSCAL  POFF
      DATA GPOINT / 1000, 2034, 2039, 8001,   4001, 4001, 8002, 4002,
     *              4002, 8003, 4003, 4003,   8004, 4004, 4004, 8005,
     *              4005, 4005, 8006, 4006,   4006, 8007, 4007, 4007,
     *              8008, 4008, 4008, 8009,   4009, 4009, 8010, 4010,
     *              4010, 8011, 4011, 4011,   8012, 4012, 4012, 8013,
     *              4013, 4013, 8014, 4014,   4014, 4015, 4015, 4016,
     *              4016, 4017, 4017, 4018,   4018, 4019, 4019, 4020,
     *              4020/
C                                                  Comment fields.
      DATA NCOM /
     *   'Source name                     ',
     *   3*'                                ',
     *   'Obs start date YYYY-MM-DD       ',
     *   'Last processing date YYYY-MM-DD ',
     *   'REAL = TAPE * BSCALE + BZERO    ',
     *   '                                ',
     *   'Units of flux                   ',
     *   'Epoch of RA DEC                 ',
     *   'Tape value of BLANK pixel       ',
     *   '>256 RADIO, 1 LSR 2 HEL 3 OBS   ',
     *   'Altenate FREQ/VEL ref value     ',
     *   'Altenate FREQ/VEL ref pixel     ',
     *   'Antenna pointing RA             ',
     *   'Antenna pointing DEC            ',
     *   'Rest frequency                  ',
     *   'Net shift of phase center in X  ',
     *   'Net shift of phase center in Y  ',
     *   'Maximum pixel value             ',
     *   'Minimum pixel value             '/
C                                                          End VKEY.
