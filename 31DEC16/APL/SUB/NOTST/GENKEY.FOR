      SUBROUTINE GENKEY (CATBLK)
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
C  Routine to generate a set of 'standard' keywords from the catalogue
C  header.
C    Inputs:
C      CATBLK        I(256)        Catalogue header
C    Outputs in common /GLOB/:
C      OBSCOD        C*8           Observing code
C      NOSTKD        I             # stokes in data
C      STK1          I             Value of 1st Stokes
C      NOBAND        I             # of band's (IF's)
C      NOCHAN        I             # spectral channels
C      REFFRQ        D             Reference freq. (Hz)
C      CHNBW         R             Bandwidth of freq. channel (Hz)
C      REFPIX        R             Reference pixel
C-----------------------------------------------------------------------
      INTEGER   CATBLK(256)
      INCLUDE 'INCS:DGLB.INC'
C
      INTEGER   CATTMP(256)
      HOLLERITH CATH(256)
      REAL      CATR(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATTMP, CATR, CATH, CATD)
C-----------------------------------------------------------------------
      CALL COPY (256, CATBLK, CATTMP)
C                                       Sensible defaults
      OBSCOD = ' '
      NOSTKD = 1
      STK1 = -1
      NOBAND = 1
      NOCHAN = 1
      REFFRQ = 0.D0
      CHNBW = 0.0
      REFPIX = 1.0
C                                       Look in header
      CALL H2CHR (8, 1, CATH(KHOBS), OBSCOD)
      NOSTKD = NCOR
      STK1 = ICOR0
      IF (JLOCIF.GT.0) NOBAND = CATBLK(KINAX+JLOCIF)
      IF (JLOCF.GT.0) THEN
         NOCHAN = CATBLK(KINAX+JLOCF)
         REFFRQ = CATD(KDCRV+JLOCF)
         CHNBW  = CATR(KRCIC+JLOCF)
         REFPIX = CATR(KRCRP+JLOCF)
         END IF
         
C
      RETURN
      END
