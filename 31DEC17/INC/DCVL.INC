C                                                          Include DCVL
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C                                       Include for the frequency
C                                       shifting routines.
      CHARACTER LTYPBP*8
      INTEGER   IYEAR, IRDAY, SOUDUN(500), NDELIN(MAXANT,MAXIF),
     *   MSTNS, OLDCSU
      REAL      IATUT,  AVDELI(MAXANT,MAXIF)
      DOUBLE PRECISION RSTFRQ(MAXIF), VEL(MAXIF), REFFRQ(MAXIF),
     *   OBSFRQ(MAXIF), ANTX(MAXANT), ANTY(MAXANT), ANTZ(MAXANT)
      LOGICAL   HELIO, RADIO, ISVLA, ISVLBA, CFIRST(MAXANT)
      COMMON /CVELCM/ RSTFRQ, VEL, REFFRQ, OBSFRQ, ANTX, ANTY, ANTZ,
     *   IATUT, AVDELI, IYEAR, IRDAY, SOUDUN, NDELIN, MSTNS, OLDCSU,
     *   HELIO, RADIO, ISVLA, ISVLBA, CFIRST
      COMMON /CHRCVL/ LTYPBP
C                                       FQ stuff
      INTEGER   CNNIF, CSBAND(MAXIF)
      REAL      CFINC(MAXIF)
      DOUBLE PRECISION CFOFF(MAXIF)
      COMMON /CVELFQ/ CFOFF, CFINC, CNNIF, CSBAND
C                                       CL stuff
      DOUBLE PRECISION CURXLT(MAXANT)
      REAL      CURXLI(MAXANT), CURLOO(MAXANT,MAXIF)
      INTEGER   ICXLUN, IXLRNO, NXLINR, XLKOLS(MAXCLC), XLNUMV(MAXCLC),
     *   XLBUFF(512), CURRNO(MAXANT)
      COMMON /CURXL/ CURXLT, CURXLI, CURLOO, XLBUFF, ICXLUN, IXLRNO,
     *   NXLINR, XLKOLS, XLNUMV, CURRNO
C                                       AP stuff
      INTEGER   APBEG, NCMPLX, APTYPE, NROLL, NBYTES, APNUM, NXTTWO
      LOGICAL   USEAP, POWRTO, PRIME
      COMMON /CVELAP/ APBEG, NCMPLX, APTYPE, NROLL, NBYTES, APNUM,
     *   NXTTWO, USEAP, POWRTO, PRIME

