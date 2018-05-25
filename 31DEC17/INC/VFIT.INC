C                                                          Include VFIT
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2003, 2005
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
      DATA CWORD /'SIMPLE  ','BITPIX  ','NAXIS   ','NAXIS1',
     *            'NAXIS2  ','NAXIS3  ','NAXIS4  ','NAXIS5  ',
     *            'NAXIS6  ','NAXIS7  '/
      DATA KWORD /'OBJECT  ','TELESCOP','INSTRUME','OBSERVER',
     *            'DATE-OBS','DATE-MAP','BSCALE  ','BZERO   ',
     *            'BUNIT   ','CTYPE1  ','CTYPE2  ','CTYPE3  ',
     *            'CTYPE4  ','CTYPE5  ','CTYPE6  ','CTYPE7  ',
     *            'CRVAL1  ','CRVAL2  ','CRVAL3  ','CRVAL4  ',
     *            'CRVAL5  ','CRVAL6  ','CRVAL7  ','CDELT1  ',
     *            'CDELT2  ','CDELT3  ','CDELT4  ','CDELT5  ',
     *            'CDELT6  ','CDELT7  ','CRPIX1  ','CRPIX2  ',
     *            'CRPIX3  ','CRPIX4  ','CRPIX5  ','CRPIX6  ',
     *            'CRPIX7  ','CROTA1  ','CROTA2  ','CROTA3  ',
     *            'CROTA4  ','CROTA5  ','CROTA6  ','CROTA7  ',
     *            'EPOCH   ','DATAMAX ','DATAMIN ','BLANK   ',
     *            'IMNAME  ','IMCLASS ','IMSEQ   ','USERNO  ',
     *            'PRODUCT ','NITER   ','BMAJ    ','BMIN    ',
     *            'BPA     ','VELREF  ','ALTRVAL ','ALTRPIX ',
     *            'OBSRA   ','OBSDEC  ','RESTFREQ','XSHIFT  ',
     *            'YSHIFT  ','DATE    ','ORIGIN  ','TABLES  ',
     *            'OPHRAE11','OPHDCE11','MAPNAM11','ISCALE  ',
     *            'IZERO   ','EQUINOX ', 'ITYPE  ','XPOFF   ',
     *            'YPOFF   '/
C                                       1=Logical variable
C                                       2=Number
C                                       3=String
      DATA CTYPE /1,2,2,2, 2,2,2,2, 2,2/
      DATA KTYPE /3,3,3,3, 3,3,2,2, 3,3,3,3, 3,3,3,3, 2,2,2,2,
     *            2,2,2,2, 2,2,2,2, 2,2,2,2, 2,2,2,2, 2,2,2,2,
     *            2,2,2,2, 2,2,2,2, 3,3,2,2, 2,2,2,2, 2,2,2,2,
     *            2,2,2,2, 2,3,3,2, 2,2,3,2, 2,2,2,2, 2/
C                                       1000*nbytes + 100*offset +
C                                       position of pointer in common
C                      "KIBPX"KIDIM KINAX
      DATA CPOINT /   0, 2000, 2040, 2041, 2141, 2241, 2341, 2441,
     *             2541, 2641/
C                 KHOBJ KHTEL KHINS KHOBS KHDOB KHDMP"KDBSC"KDBZE"
      DATA POINT / 8001, 8002, 8003, 8004, 8005, 8006, 8000, 8000,
C                 KHBUN KHCTP =>
     *             8007, 8009, 8109, 8209, 8309, 8409, 8509, 8609,
C                 KDCRV =>                                  KRCIC =>
     *             8029, 8129, 8229, 8329, 8429, 8529, 8629, 4010,
C                                                     KRCRP =>
     *             4110, 4210, 4310, 4410, 4510, 4610, 4011, 4111,
C                                               KRCRT = >
     *             4211, 4311, 4411, 4511, 4611, 4012, 4112, 4212,
C                                         KREPO KRDMX KRDMN KRBLK
     *             4312, 4412, 4512, 4612, 4013, 4014, 4015, 4016,
C                 KHIMN KHIMC KIIMS KIIMU KITYP  KINIT KRBMJ KRBMN
     *            12017, 6218, 2042, 2043, 2044, 2035, 4020, 4021,
C                 KRBPA KIALT KDARV KRARP KDORA KDODE KDRST KRXSH
     *             4022, 2045, 8033, 4023, 8030, 8031, 8032, 4024,
C                 KRYSH
     *             4025,    0,    0, 2000, 4100, 4200,12017, 8000,
C                       KREPO KIITY KRXPO KRYPO
     *             8000, 4013, 2058, 4059, 4060/
C                                                          End VFIT.
