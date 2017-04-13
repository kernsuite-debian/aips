C                                                          Include VFUV.
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999-2000, 2003, 2009
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
C                                       FITS parsing, esp. with uv data
      DATA CWORD/'SIMPLE  ','BITPIX  ','NAXIS   ','NAXIS1  ',
     *         'NAXIS2  ','NAXIS3  ','NAXIS4  ','NAXIS5  ',
     *         'NAXIS6  ','NAXIS7  ','NAXIS8  '/
      DATA K1 /'OBJECT  ','TELESCOP','INSTRUME','OBSERVER',
     *         'DATE-OBS','DATE-MAP','BSCALE  ','BZERO   ',
     *         'BUNIT   ','CTYPE1  ','CTYPE2  ','CTYPE3  ',
     *         'CTYPE4  ','CTYPE5  ','CTYPE6  ','CTYPE7  ',
     *         'CTYPE8  ','CRVAL1  ','CRVAL2  ','CRVAL3  ',
     *         'CRVAL4  ','CRVAL5  ','CRVAL6  ','CRVAL7  ',
     *         'CRVAL8  ','CDELT1  ','CDELT2  ','CDELT3  ',
     *         'CDELT4  ','CDELT5  ','CDELT6  ','CDELT7  ',
     *         'CDELT8  ','CRPIX1  ','CRPIX2  ','CRPIX3  ',
     *         'CRPIX4  ','CRPIX5  ','CRPIX6  ','CRPIX7  ',
     *         'CRPIX8  ','CROTA1  ','CROTA2  ','CROTA3  ',
     *         'CROTA4  ','CROTA5  ','CROTA6  ','CROTA7  ',
     *         'CROTA8  ','EPOCH   ','DATAMAX ','DATAMIN ',
     *         'BLANK   ','IMNAME  ','IMCLASS ','IMSEQ   ',
     *         'USERNO  ','PRODUCT ','NITER   ','BMAJ    ',
     *         'BMIN    ','BPA     ','VELREF  ','ALTRVAL ',
     *         'ALTRPIX ','OBSRA   ','OBSDEC  ','RESTFREQ',
     *         'XSHIFT  ','YSHIFT  ','DATE    ','ORIGIN  ',
     *         'ISCALE  ','IZERO   ','EQUINOX '/
      DATA K2 /'GROUPS  ','GCOUNT  ','PCOUNT  ','PTYPE1  ',
     *         'PTYPE2  ','PTYPE3  ','PTYPE4  ','PTYPE5  ',
     *         'PTYPE6  ','PTYPE7  ','PTYPE8  ','PTYPE9  ',
     *         'PTYPE10 ','PTYPE11 ','PTYPE12 ','PTYPE13 ',
     *         'PTYPE14 ','PTYPE15 ','PTYPE16 ','PTYPE17 ',
     *         'PTYPE18 ','PTYPE19 ','PTYPE20 ','PSCAL1  ',
     *         'PSCAL2  ','PSCAL3  ','PSCAL4  ','PSCAL5  ',
     *         'PSCAL6  ','PSCAL7  ','PSCAL8  ','PSCAL9  ',
     *         'PSCAL10 ','PSCAL11 ','PSCAL12 ','PSCAL13 ',
     *         'PSCAL14 ','PSCAL15 ','PSCAL16 ','PSCAL17 ',
     *         'PSCAL18 ','PSCAL19 ','PSCAL20 ','PZERO1  ',
     *         'PZERO2  ','PZERO3  ','PZERO4  ','PZERO5  ',
     *         'PZERO6  ','PZERO7  ','PZERO8  ','PZERO9  ',
     *         'PZERO10 ','PZERO11 ','PZERO12 ','PZERO13 ',
     *         'PZERO14 ','PZERO15 ','PZERO16 ','PZERO17 ',
     *         'PZERO18 ','PZERO19 ','PZERO20 ','TABLES  ',
     *         'SORTORDR','SORTORD ','WTSCAL  '/
      DATA K3 /'TTYPE1  ','TTYPE2  ','TTYPE3  ','TTYPE4  ',
     *         'TTYPE5  ','TTYPE6  ','TTYPE7  ','TTYPE8  ',
     *         'TTYPE9  ','TTYPE10 ','TTYPE11 ','TTYPE12 ',
     *         'TTYPE13 ','TTYPE14 ','TTYPE15 ','TTYPE16 ',
     *         'TTYPE17 ','TTYPE18 ','TTYPE19 ','TTYPE20 ',
     *         'TSCAL1  ','TSCAL2  ','TSCAL3  ','TSCAL4  ',
     *         'TSCAL5  ','TSCAL6  ','TSCAL7  ','TSCAL8  ',
     *         'TSCAL9  ','TSCAL10 ','TSCAL11 ','TSCAL12 ',
     *         'TSCAL13 ','TSCAL14 ','TSCAL15 ','TSCAL16 ',
     *         'TSCAL17 ','TSCAL18 ','TSCAL19 ','TSCAL20 ',
     *         'TZERO1  ','TZERO2  ','TZERO3  ','TZERO4  ',
     *         'TZERO5  ','TZERO6  ','TZERO7  ','TZERO8  ',
     *         'TZERO9  ','TZERO10 ','TZERO11 ','TZERO12 ',
     *         'TZERO13 ','TZERO14 ','TZERO15 ','TZERO16 ',
     *         'TZERO17 ','TZERO18 ','TZERO19 ','TZERO20 ',
     *         'IPIECE  ','NPIECE  ','FIRSTVIS','LASTVIS '/
C                                       1=Logical variable
C                                       2=Number
C                                       3=String
      DATA CTYPE /1,2,2,2, 2,2,2,2, 2,2,2/
      DATA KTYPE /3,3,3,3, 3,3,2,2, 3,3,3,3, 3,3,3,3, 3,2,2,2,
     *            2,2,2,2, 2,2,2,2, 2,2,2,2, 2,2,2,2, 2,2,2,2,
     *            2,2,2,2, 2,2,2,2, 2,2,2,2, 2,3,3,2, 2,2,2,2,
     *            2,2,2,2, 2,2,2,2, 2,2,3,3, 2,2,2,   1,2,2,
     *            20*3, 20*2, 20*2, 2,3,3,2, 20*3, 20*2, 20*2,
     *            2,2,2,2/
C                                       nbytes=2 => 4-byte integer
C                                       1000*nbytes + 100*offset +
C                                       position of pointer in common
C                      "KIBPX"KIDIM KINAX =>
      DATA CPOINT /   0, 2000, 2040, 2041, 2141, 2241, 2341, 2441,
     *             2541, 2641, 2741/
C                 KHOBJ KHTEL KHINS KHOBS KHDOB KHDMP"KDBSC"KDBZE"
      DATA POINT / 8001, 8002, 8003, 8004, 8005, 8006, 8000, 8000,
C                 KHBUN KHCTP =>
     *             8007, 8009, 8109, 8209, 8309, 8409, 8509, 8609,
C                       KDCRV = >
     *             8709, 8029, 8129, 8229, 8329, 8429, 8529, 8629,
C                       KRCIC =>
     *             8729, 4010, 4110, 4210, 4310, 4410, 4510, 4610,
C                       KRCRP =>
     *             4710, 4011, 4111, 4211, 4311, 4411, 4511, 4611,
C                       KRCRT =>
     *             4711, 4012, 4112, 4212, 4312, 4412, 4512, 4612,
C                       KREPO KRDMX KRDMN KRBLK KHIMN KHIMC
     *             4712, 4013, 4014, 4015, 4016,12017, 6218,
C                 KIIMS KIIMU KITYP KINIT KRBMJ KRBMN KRBPA KIALT
     *             2042, 2043, 2044, 2035, 4020, 4021, 4022, 2045,
C                 KDARV KRARP KDORA KDODE KDRST KRXSH KRYSH
     *             8033, 4023, 8030, 8031, 8032, 4024, 4025,    0,
C                                   KREPO       KIGCN KIPCN
     *                0, 8000, 8000, 4013,       1001, 2034, 2039,
     *             20*8008, 20*8000, 20*8000, 2000, 2044, 2044, 4000,
     *             20*8008, 20*8000, 20*8000, 4*2000/
C                                                          End VFUV.
