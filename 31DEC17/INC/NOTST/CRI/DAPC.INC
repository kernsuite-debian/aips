C                                                          Include DAPC
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
C                                       Include for AP memory and work
      INCLUDE 'INCS:PAPC.INC'
C                                       CRI Version
      REAL      MEMERY(OFF2+12*OFFDEL), HIGHWD, APCORE(APSIZE+1),
     *   WKVEC1(OFFDEL), WKVEC2(OFFDEL), WKVEC3(OFFDEL), WKVEC4(OFFDEL),
     *   WKVEC5(OFFDEL), WKVEC6(OFFDEL), WKVEC7(OFFDEL), WKVEC8(OFFDEL),
     *   WKVEC9(OFFDEL), WKVECA(OFFDEL), WKVECB(OFFDEL), WKVECC(OFFDEL),
     *   WRKFFT(5*MAXMAP)
      INTEGER   APCORI(APSIZE+1), SPAD(16), FFTDIR, FFTSZE, MEMSIZ,
     *   IWVEC1(OFFDEL), IWVEC2(OFFDEL), IWVEC3(OFFDEL),
     *   IWVEC4(OFFDEL), IWVEC5(OFFDEL), IWVEC6(OFFDEL), IWVEC7(OFFDEL),
     *   IWVEC8(OFFDEL), IWVEC9(OFFDEL),  IWVECA(OFFDEL),
     *   IWVECB(OFFDEL), IWVECC(OFFDEL)
C                                       AP stuff in blank common
      COMMON MEMERY
      COMMON /SPF/ SPAD, FFTDIR, FFTSZE, MEMSIZ
      EQUIVALENCE (MEMERY, WRKFFT),
     *   (MEMERY(OFF1), APCORE, APCORI),
     *   (MEMERY(OFF2), WKVEC1, IWVEC1),
     *   (MEMERY(OFF2+OFFDEL), WKVEC2, IWVEC2),
     *   (MEMERY(OFF2+2*OFFDEL), WKVEC3, IWVEC3),
     *   (MEMERY(OFF2+3*OFFDEL), WKVEC4, IWVEC4),
     *   (MEMERY(OFF2+4*OFFDEL), WKVEC5, IWVEC5),
     *   (MEMERY(OFF2+5*OFFDEL), WKVEC6, IWVEC6),
     *   (MEMERY(OFF2+6*OFFDEL), WKVEC7, IWVEC7),
     *   (MEMERY(OFF2+7*OFFDEL), WKVEC8, IWVEC8),
     *   (MEMERY(OFF2+8*OFFDEL), WKVEC9, IWVEC9),
     *   (MEMERY(OFF2+9*OFFDEL), WKVECA, IWVECA),
     *   (MEMERY(OFF2+10*OFFDEL), WKVECB, IWVECB),
     *   (MEMERY(OFF2+11*OFFDEL), WKVECC, IWVECC),
     *   (MEMERY(OFF2+12*OFFDEL), HIGHWD)
C                                                          End DAPC
