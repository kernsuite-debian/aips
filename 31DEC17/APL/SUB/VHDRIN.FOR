      SUBROUTINE VHDRIN
C-----------------------------------------------------------------------
C! computes pointers (subscripts) to address components of the header
C# Header
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2005
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
C   VHDRIN computes the pointers required to address catalog records
C   (both normal and image).  The routine sets common /HDRVAL/, which
C   contains 60 integers.  They are then machine independent symbols
C   which have machine dependent values.
C-----------------------------------------------------------------------
      INTEGER   PI, PH, PR, PD, INC4C8
      REAL      X, EPS
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PHDR.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA INC4C8, EPS /2, 0.01/
C-----------------------------------------------------------------------
C                                       set address increments
      PH = 1
C                                       array sizes
      KIPTPN = 14
      KICTPN = 7
      KIEXTN = NIEXTN
C                                       character of 8
      KHOBJ = PH
      KHTEL = PH +   INC4C8
      KHINS = PH + 2*INC4C8
      KHOBS = PH + 3*INC4C8
      KHDOB = PH + 4*INC4C8
      KHDMP = PH + 5*INC4C8
      KHBUN = PH + 6*INC4C8
      KHPTP = PH + 7*INC4C8
      KHCTP = PH + (7+KIPTPN)*INC4C8
      PH = PH + INC4C8 * (7 + KIPTPN + KICTPN)
      PI = PH
C                                       reals: double precision
      X = (PI - 1.0) / NWDPDP + 1.0
      PD = X + EPS
      IF (ABS(PD-X).GT.EPS) THEN
         PD = X + 1.0 - EPS
         PI = (PD-1)*NWDPDP + 1
         END IF
      KDCRV = PD
      PI = PI + NWDPDP * KICTPN
C                                       reals: single precision
      PR = PI
      KRCIC = PR
      KRCRP = PR +   KICTPN
      KRCRT = PR + 2*KICTPN
      PR = PR + 3 * KICTPN
      KREPO = PR
      KRDMX = PR + 1.0
      KRDMN = PR + 2.0
      KRBLK = PR + 3.0
      PR = PR + 4
      PI = PR
C                                       GCN now I, other integers
      KIGCN = PI
      KIPCN = PI + 1
      KIDIM = PI + 2
      KINAX = PI + 3
      KIIMS = PI + (3+KICTPN)
      PI = PI + (4+KICTPN)
C                                       name \\ class \\ phystype
C                                       character(20)
      PH = PI
      KHIMN = PH
      KHIMC = KHIMN
      KHPTY = KHIMN
      KHIMNO = 1
      KHIMCO = 13
      KHPTYO = 19
      PI = PH + 5
C                                       user #, clean parms
      KIIMU = PI
      KIALT = PI + 1
      PI = PI + 2
      PR = PI
      KRBMJ = PR
      KRBMN = PR + 1
      KRBPA = PR + 2
      PI = PI + 3
C                                       iterations
      KINIT = PI
      KITYP = PI + 1
      PI = PI + 2
C                                       Pointing position, alt freq
      X = (PI - 1.0) / NWDPDP + 1.0
      PD = X + EPS
      IF (ABS(X-PD).GT.EPS) THEN
         PD = X + 1.0 - EPS
         PI = (PD-1) * NWDPDP + 1
         END IF
      KDORA = PD
      KDODE = PD + 1
      KDRST = PD + 2
      KDARV = PD + 3
      PD = PD + 4
      PI = (PD - 1) * NWDPDP + 1
      PR = PI
      KRARP = PR
      KRXSH = PR + 1
      KRYSH = PR + 2
      PI = PI + 3
C                                       extension files
      KHEXT = PI
      KIVER = PI + KIEXTN
      PI = PI + 2*KIEXTN
C                                       G&C wcs removed from mainn
C                                       header
C     KRCOK = PI
C     KDLON = PI / NWDPDP + 1
C     KDPRJ = KDLON + 1
C     PD = KDPRJ + 9
C     PI = (PD-1) * NWDPDP + 1
C     KRPCM = PI
C                                       UV weight normalization
      KRWTN = PI
C                                       Imaging type, xpoff, ypoff
      KIITY = PI + 1
      KRXPO = PI + 2
      KRYPO = PI + 3
C                                       residual space
      KIRES = PI + 4
      KIRESN = 257 - KIRES
C                                       replace extensions and extra
C                                       position info with image
C                                       parameters in image catalog
      PD = KDORA
      PI = (PD - 1) * NWDPDP + 1
      PR = PI
      IRRAN = PR
C                                       skip alternate axis parms
      PR = KRXSH
      PI = PR
      IIVOL = PI
      IICNO = PI + 1
      IIWIN = PI + 2
      IIDEP = PI + 6
      PI = PI + (6 + KICTPN - 2)
      IICOR = PI
      IITRA = PI + 4
      IIPLT = PI + 5
      IIOTH = PI + 6
C
 999  RETURN
      END
