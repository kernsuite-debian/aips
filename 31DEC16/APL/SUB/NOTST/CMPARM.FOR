      SUBROUTINE CMPARM (BIF, EIF, BCHAN, ECHAN, BCHANS, ECHANS,
     *   NDECMP, DECMP)
C-----------------------------------------------------------------------
C! Determines blocks of data in a vis. record to decompress
C# Calibration
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
C   Determines the number of blocks of data in a compressed visibility
C   record to decompress and the length and offsets of these blocks.
C   It is assumed that UVPGET has been called with the relevant CATBLK
C   and the values in DUVH.INC are correct.
C   Inputs:
C      BIF      I       First IF number
C      EIF      I       Highest IF number
C      BCHAN    I       First channel number
C      ECHAN    I       Highest channel number
C      BCHANS   I       First channel number if smoothing
C      ECHANS   I       Last channel number if smoothing
C   Inputs from common DCAT.INC:
C      CATBLK   I(256)  Catalogue header record of compressed file
C   Output:
C      NDECMP   I        Number of entries in DECMP
C      DECMP    I(2,*)   (1,*) = number of packed correlator values
C                        (2,*) = 0-rel offset in vis data.
C                        (from beginning of vis data NOT ran. parms.)
C-----------------------------------------------------------------------
      INTEGER   BIF, EIF, BCHAN, ECHAN, NDECMP, DECMP(2,*), BCHANS,
     *   ECHANS
C
      INTEGER   I, J, LIMS, NIF, NCHAN, IBCHAN, IECHAN
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Packed data?
      IF (CATBLK(KINAX).GT.1) GO TO 999
C                                       Smoothed = use wider range
C                                       of channels
      IF ((BCHANS.GT.0) .AND. (ECHANS.GT.0)) THEN
         IBCHAN = BCHANS
         IECHAN = ECHANS
      ELSE
         IBCHAN = BCHAN
         IECHAN = ECHAN
         END IF
C                                       Check if all requested.
      NIF = 1
      IF (JLOCIF.GT.0) NIF = CATBLK(KINAX+JLOCIF)
      NCHAN = CATBLK(KINAX+JLOCF)
      IF (((BIF.EQ.1) .AND. (EIF.EQ.NIF)) .AND.
     *   ((IBCHAN.EQ.1) .AND. (IECHAN.EQ.NCHAN))) THEN
         NDECMP = 1
         DECMP(1,1) = LREC - NRPARM
         DECMP(2,1) = 0
         GO TO 999
         END IF
C                                       Continuum
      IF (NCHAN.LE.1) THEN
         NDECMP = 1
         DECMP(1,NDECMP) = (EIF-BIF+1) * CATBLK(KINAX+JLOCS)
         DECMP(2,NDECMP) = (BIF-1) * INCIF
         GO TO 999
         END IF
C                                       Line
      IF ((JLOCS.LT.JLOCF) .AND. ((JLOCIF.GE.3) .OR. (JLOCIF.LE.0)))
     *   THEN
C                                       Stokes most rapid var.
         NDECMP = 0
         DO 200 I = BIF,EIF
            NDECMP = NDECMP + 1
            DECMP(1,NDECMP) = (IECHAN-IBCHAN+1) * CATBLK(KINAX+JLOCS)
            DECMP(2,NDECMP) = (IBCHAN-1) * INCF + (I-1) * INCIF
 200        CONTINUE
         GO TO 999
         END IF
      IF ((JLOCF.LT.JLOCS) .AND. ((JLOCIF.GE.3) .OR. (JLOCIF.LE.0)))
     *   THEN
C                                       Frequency most rapid var.
         LIMS = CATBLK(KINAX+JLOCS)
         NDECMP = 0
         DO 300 I = BIF,EIF
            DO 300 J = 1,LIMS
               NDECMP = NDECMP + 1
               DECMP(1,NDECMP) = IECHAN - IBCHAN + 1
               DECMP(2,NDECMP) = (IBCHAN-1) * INCF + (I-1) * INCIF +
     *            (J-1) * INCS
 300           CONTINUE
         GO TO 999
         END IF
C                                       Anything else - do all
 
      NDECMP = 1
      DECMP(1,1) = LREC - NRPARM
      DECMP(2,1) = 0
C
 999  RETURN
      END
