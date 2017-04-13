      SUBROUTINE UVAPAS(NCHAVG, MAXU, MDELTA, FREQG, FREQUV,
     *   INPASS, NPASS, LOCHAN, NCHAN, FREQLO, FREQHI, FRQTAB)
C-----------------------------------------------------------------------
C! Deterines number of passes needed for bandwidth synthesis
C# AP-util UV
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
C UVAPAS calculates the number of channels to average in a gridding
C or interpolation pass for bandwidth synthesis
C handels two functions, deterimining the number of passes
C and returning the number of channels to average in a given pass.
C INPUTS:
C  NCHAVG   I    Number of channels to average
C  MAXU     I    Maximum baseline in cells
C  MDELTA   I    Number of cells in AP
C  FREQG    R(*) Frequencies to average
C  FREQUV   R    Frequency of observation
C  INPASS   I    Pass number to determine LOCHAN and NCHAN for.
C OUTPUT :
C  NPASS    I    Number of passes, set only if IPASS=0
C  LOCHAN   I    Index of first channel to average
C  NCHAN    I    Number of channels to average (includes LOCHAN)
C  FREQLO   R    Lowest frequency to average
C  FREQHI   R    Highest frequency to average
C  FRQTAB   R(*) Frequency scaling table
C-----------------------------------------------------------------------
      INTEGER NCHAVG, MAXU, MDELTA, INPASS, NPASS, LOCHAN, NCHAN
      REAL FRQTAB(255)
      DOUBLE PRECISION FREQG(255), FREQUV, FREQLO, FREQHI
C-----------------------------------------------------------------------
      INTEGER ICHAN, ISTART, IPASS
      DOUBLE PRECISION FREQI, FREQS, FREQT, BWMAX
C-----------------------------------------------------------------------
C                                       init counts of chans and passes
      ISTART = 1
      NCHAN  = 0
      IPASS  = 1
         IF ((MAXU-MDELTA) .GT. 0) THEN
            BWMAX = FREQUV/(MAXU-MDELTA)
         ELSE
            BWMAX = FREQUV
            END IF
         BWMAX = ABS(BWMAX)
C                                       For all chanels to average
      DO 100 ICHAN = 1, NCHAVG
C                                       set freq of cur. and start chan
         FREQI  = FREQG(ICHAN)
         FREQS  = FREQG(ISTART)
C                                       get min freq in current range
         FREQT  = MIN (FREQS, FREQI)
C                                       Bandwidth MAX is set by freq.
C                                       shift U or V by 1 row
C        1 = (MAXU-MDELTA)*(FREQS+BWMAX)/FREQS
C        FREQS+BWMAX = FREQT/(MAXU-MDELTA)
C         BWMAX = FREQT * MDELTA / (MAXU * (FREQT/FREQUV))
C                                       if bandwidth exceeded
         IF (ABS (FREQS-FREQI).GT.BWMAX) THEN
C                                       must start new averaging pass
            IPASS = IPASS + 1
C                                       record start of pass
            ISTART = ICHAN
            END IF
C                                       if in pass of interest
         IF (IPASS .EQ. INPASS) THEN
C                                       count number of channels
            NCHAN = NCHAN + 1
C                                       if first channel
            IF (NCHAN.EQ.1) THEN
C                                       then keep Low Channel
               LOCHAN = ICHAN
C                                       init search for hi and lo freq
               FREQLO = FREQI
               FREQHI = FREQLO
            ELSE
C                                       find hi and lo freq in pass
               FREQLO = MIN(FREQI, FREQLO)
               FREQHI = MAX(FREQI, FREQHI)
               FRQTAB(ICHAN-LOCHAN) = 0
               IF (FREQG(ICHAN-1) .NE. 0)
     *            FRQTAB(ICHAN-LOCHAN) = (FREQI/FREQG(ICHAN-1)) - 1.0D0
               END IF
            END IF
C                                       end all channels to average
 100        CONTINUE
C                                       if counting passes
      IF (INPASS .EQ. 0) NPASS = IPASS
      RETURN
      END




