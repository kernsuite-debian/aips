      SUBROUTINE MOVIST (OP, ICHAN, NFR, NFRPCH, MAG, IERR)
C-----------------------------------------------------------------------
C! sets/resets the movie status parameters in the TV common
C# TV-util
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
C   MOVIST sets and resets the movie status parameters in the TV common.
C   Inputs:
C      OP      C*4   'ONNN' when turning on a movie
C                    'OFFF' when clearing channel(s)
C      ICHAN   I     Bit pattern of channels involved (OFFF)
C                    Actual first channel number (1-NGRAY, ONNN)
C      NFR     I     Number of frames in movie total (ONNN)
C      NFRPCH  I     Number of frames per TV channel (ONNN)
C                    < 0 => in display rather than movie order
C      MAG     I     Magnification number (0 - 3, ONNN)
C   Output:
C      IERR    I     Error = 2 => bad input, else ok
C   The code: bit 1 (lsb) 1 (on) = display order, 0 => movie order
C                 2       1 => frame starts movie
C                 3-6     magnification step
C                 7-15    # frames
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   ICHAN, NFR, NFRPCH, MAG, IERR
C
      INTEGER   ICH, IC, ICS, ICN, IL, ILS, ILM, ILN, I, ILT
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                       check inputs
      IERR = 2
      IF (('ONNN'.NE.OP) .AND. ('OFFF'.NE.OP)) GO TO 999
      ICH = ICHAN
C                                       Turn OFFF channel(s)
      IF (OP.EQ.'ONNN') GO TO 50
         IF (ICHAN.LE.0) ICH = 2 ** NGRAY - 1
         IERR = 0
         DO 40 I = 1,NGRAY
            IC = MOD (ICH, 2)
            IF (IC.EQ.0) GO TO 30
            IC = I
            IF (TYPMOV(IC).EQ.0) GO TO 30
C                                       channel is part of a movie
            IL = IC
            ICS = MOD (TYPMOV(IC)/2, 2)
            ICN = TYPMOV(IC) / 64
            TYPMOV(IC) = 0
C                                       next higher starts sub-movie
            ILT = MOD(TYPMOV(IC+1), 2)
            IF ((IC.LT.NGRAY) .AND. (TYPMOV(IC+1).NE.0))
     *         TYPMOV(IC+1) = 4 * (TYPMOV(IC+1) / 4)  +  2 + ILT
C                                       lower smaller movies now
            IF (ICS.EQ.1) GO TO 30
 20            IL = IL - 1
               IF (IL.LE.0) GO TO 30
                  ILT = MOD(TYPMOV(IL), 2)
                  ILS = MOD (TYPMOV(IL)/2, 2)
                  ILM = MOD (TYPMOV(IL)/4, 16)
                  ILN = TYPMOV(IL) / 64 - ICN
                  IF (ILN.LE.0) TYPMOV(IL) = 0
                  IF (ILN.GT.0) TYPMOV(IL) = ILT + 2*ILS + 4*ILM +
     *               64*ILN
                  IF ((ILN.GT.0) .AND. (ILS.EQ.0)) GO TO 20
 30         ICH = ICH / 2
            IF (ICH.EQ.0) GO TO 999
 40         CONTINUE
         GO TO 999
C                                       Turn on a channel: check
 50   CONTINUE
         IF ((NFR.LE.0) .OR. (NFRPCH.EQ.0)) GO TO 999
         IF ((MAG.LT.0) .OR. (MAG.GT.15) .OR.
     *      (MAG.GT.IABS(MXZOOM))) GO TO 999
         IF ((ICHAN.LT.1) .OR. (ICHAN.GT.NGRAY)) GO TO 999
         IERR = 0
C                                       channel is part of a movie
         IF (TYPMOV(ICHAN).EQ.0) GO TO 60
            IL = ICHAN
            ILT = MOD (TYPMOV(ICHAN+1), 2)
            ICS = MOD (TYPMOV(ICHAN)/2, 2)
            ICN = TYPMOV(ICHAN) / 64
C                                       next higher starts sub-movie
            IF ((ICHAN.LT.NGRAY) .AND. (TYPMOV(ICHAN+1).NE.0))
     *         TYPMOV(ICHAN+1) = 4 * (TYPMOV(ICHAN+1) / 4) + 2 + ILT
C                                       lower smaller movies now
            IF (ICS.EQ.1) GO TO 60
 55            IL = IL - 1
               IF (IL.LE.0) GO TO 60
                  ILT = MOD (TYPMOV(IL), 2)
                  ILS = MOD (TYPMOV(IL)/2, 2)
                  ILM = MOD (TYPMOV(IL)/4, 16)
                  ILN = TYPMOV(IL) / 64 - ICN
                  IF (ILN.LE.0) TYPMOV(IL) = 0
                  IF (ILN.GT.0) TYPMOV(IL) = ILT + 2*ILS + 4*ILM +
     *               64*ILN
                  IF ((ILN.GT.0) .AND. (ILS.EQ.0)) GO TO 55
C                                       Insert new movie
 60      IL = ICHAN
         IC = NFR
         DO 70 I = IL,NGRAY
            TYPMOV(I) = 4 * MAG + 64 * IC
            IF (I.EQ.IL) TYPMOV(I) = TYPMOV(I) + 2
            IF (NFRPCH.LT.0) TYPMOV(I) = TYPMOV(I) + 1
            IC = IC - ABS(NFRPCH)
            IF (IC.LE.0) GO TO 999
 70         CONTINUE
C
 999  RETURN
      END
