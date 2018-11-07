      SUBROUTINE BASLST (NUMAN, ISUB, ANTENN, BASELN, NBASE, ANT1, ANT2)
C-----------------------------------------------------------------------
C! Build a list of antenna pairs from adverbs ANTENNA, BASELINE
C# UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   Determine a list of selected antenna pairs from the standard
C   ANTENNA and BASELINE adverbs.  BASLST differs from AN10RS in that
C   BASLST will always produce a full list of antenna pairs while
C   AN10RS will produce an abbreviated list if there are no entries in
C   ANTENNAS or BASELINE.
C   Inputs:
C      NUMAN    I(51)   NUMAN(1) is the number of subarrays present.
C                       NUMAN(i) is the maximum antenna number for
C                       subarray i-1, for i > 1.
C      ISUB     I       The subarray number for which a list of
C                       baselines is required: must be greater than
C                       zero (and is not checked!).
C      ANTENN   R(50)   The values of the ANTENNAS adverb.
C      BASELN   R(50)   The values of the BASELINE adverb.
C
C   Input/output:
C      NBASE    I       The maximum number of baselines to list on
C                       input.  Either the number of baselines found
C                       or zero or zero on output.  A value of zero
C                       indicates that more than the maximum number of
C                       baselines met the selection criteria.
C   Outputs:
C      ANT1     I(*)    List of first elements of antenna pairs.
C      ANT2     I(*)    List of second elements of antenna pairs.
C                       For all i, 1 <= i <= NBASE, ANT2(i) > ANT1(i).
C-----------------------------------------------------------------------
      INTEGER   NUMAN(51), ISUB
      REAL      ANTENN(50), BASELN(50)
      INTEGER   NBASE, ANT1(*), ANT2(*)
C                                        DOSEL is true if baselines
C                                        listed in the adverbs are
C                                        to be deselected rather
C                                        selected.
      LOGICAL   DESEL
C                                        ANT lists the absolute values
C                                        of entries in the ANTENNAS
C                                        adverb, NANT is the number
C                                        of non-zero entries.
      INTEGER   ANT(50), NANT
C                                        BAS lists the absolute values
C                                        of entries in the BASELINE
C                                        adverb, NBAS is the number
C                                        of non-zero entries.
      INTEGER   BAS(50), NBAS
C                                        NREQ is the number of
C                                        baselines that match the
C                                        request criteria that have
C                                        been found so far.
      INTEGER   NREQ
C
      INTEGER   I, J, K, IROUND
      LOGICAL   REQBAS
      EXTERNAL  REQBAS, IROUND
C-----------------------------------------------------------------------
      IF ((ISUB.GT.0) .AND. (ISUB.LE.NUMAN(1))) THEN
         CALL SETANT (50, ANTENN, BASELN, NANT, NBAS, ANT, BAS, DESEL)
C                                       make full list w REQBAS
         K = NUMAN(ISUB+1)
         NREQ = 0
         DO 50 I = 1,K-1
            DO 40 J = I+1,K
               IF (REQBAS (I, J, DESEL, ANT, NANT, BAS, NBAS)) THEN
C                                        Too many baselines.
                  IF (NREQ.EQ.NBASE) THEN
                     NBASE = 0
                     GO TO 999
                  ELSE
                     NREQ = NREQ + 1
                     ANT1(NREQ) = I
                     ANT2(NREQ) = J
                     END IF
                  END IF
 40            CONTINUE
 50         CONTINUE
         NBASE = NREQ
      ELSE
C                                        Non-existent subarray.
         NBASE = 0
         END IF
C
  999 RETURN
      END
