      SUBROUTINE AN10RS (NUMAN, ISUB, ANTENN, BASELN, NBASE, ANT1, ANT2)
C-----------------------------------------------------------------------
C! Determines a list of antenna pairs from adverbs ANTENNA, BASELINE
C# UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2006, 2016
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
C   Determine a list of selected antenna pairs from the standard ANTENNA
C   and BASELINE adverbs.  There are a number of cases.
C
C   1 - All elements of both ANTENN and BASELN are zero.  All baselines
C       are selected.  In this case NBASE is set to 1 and the first
C       elements of both ANT1 and ANT2 are set to 0.
C
C   2 - All elements of ANTENN are positive or zero with at least one
C       positive value and all elements of BASELN are zero.  All
C       baselines containing antennae with numbers corresponding to the
C       non-zero elements of ANTENN are selected.  NBASE is set to the
C       number of unique non-zero entries in ANTENN, the first NBASE
C       entries of ANT1 list the unique non-zero elements of ANTENN and
C       the first NBASE elements of ANT2 are zero.  If all ANTENN are 0
C       but some of BASELN are not, their roles are reversed.
C
C   3 - All elements of ANTENN are positive or zero with at least one
C       positive value and at least one element of BASELN is non-zero.
C       All baselines that can be formed from one non-zero element of
C       ANTENN and the absolute value of one non-zero element of BASELN
C       are selected.  NBASE is the number of unique baselines that can
C       be formed in this way and the first NBASE elements of ANT1 and
C       ANT2 list the first and second elements of each of these
C       baselines respectively.
C
C   4 - At least one element of ANTENN is negative and all elements of
C       BASELN are zero.  All baselines that do not include antennae
C       corresponding to the absolute values of the non-zero elements of
C       ANTENN are selected.  NBASE is the number of such baseline and
C       the first NBASE elements of ANT1 and ANT2 list the first and
C       second elements of each of these baselines respectively.
C
C   5 - At least one element of ANTENN is negative and at least one
C       element of BASELN is non-zero.  All baselines that can not be
C       formed from the absolute value of one non-zero element of ANTENN
C       and the absolute value of one non-zero element of BASELN are
C       selected.  NBASE is the number of such baselines and the first
C       NBASE elements of ANT1 and ANT2 list the first and second
C       elements of each of these baselines respectively.
C
C   6 - The number of selected baselines, determined by any of the above
C       rules, exceeds the size of ANT1 and ANT2.  An error message is
C       emitted and NBASE is set to zero.
C
C   On entry NBASE should be the dimension of ANT1 and ANT2 and should
C   be greater than 50.
C
C   If neither ANT1(i) nor ANT2(i) are zero for any i in the range 1 to
C   NBASE inclusive than ANT2(i) > ANT1(i).
C
C   If NUMAN contains no useful information a default maximum antenna
C   number of 28 is assumed.
C   Inputs:
C      NUMAN    I(513)   NUMAN(1) is the number of subarrays present.
C                        NUMAN(i) where i > 1 is the maximum antenna
C                        number for subarray i - 1.
C      ISUB     I        the subarray number for which a list of
C                        baselines is required; if ISUB is less than or
C                        equal to zero generate a list valid for all
C                        subarrays.
C      ANTENN   R(50)    the value list of the ANTENNA adverb; assumed
C                        to contain integer values.
C      BASELN   R(50)    the value list of the BASELINE adverb; assumed
C                         to contain integer values.
C   Input/Output:
C      NBASE    I        on input: the maximum number of baselines to
C                                  list;
C                        on output: the number of valid elements in ANT1
C                                   and ANT2.
C   Output:
C      ANT1     I(*)     see above.
C      ANT2     I(*)     see above.
C   Local variables:
C      NUNANT  I        number of unique antennae in ANTENN
C      UNQANT  I(50)    list of unique antennae in ANTENN
C
C      NUNBAS  I        number of unique antennae in BASELN
C      UNQBAS  I(50)    list of unique antennae in BASELN
C
C      REJECT  L        true if listed baselines are to be rejected
C                       (case 4 and 5 above); false otherwise.
C
C      MAXANT  I        maximum antenna number for subarray ISUB or, if
C                       ISUB is zero or negative, the largest maximum
C                       antenna number for all subarrays.
C
C      MXBASE  I        maximum number of baselines allowed in output
C
C-----------------------------------------------------------------------
      INTEGER   NUMAN(*), ISUB, NBASE, ANT1(*), ANT2(*)
      REAL      ANTENN(50), BASELN(50)
C
      INTEGER   NUNANT, UNQANT(50), NUNBAS, UNQBAS(50), MAXANT, MXBASE,
     *   I, J, K
      LOGICAL   REJECT, REQBAS, LT
      EXTERNAL  REQBAS
      INCLUDE   'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Establish the definitions of
C                                       local variables NUNANT, UNQANT
C                                       and REJECT:
      CALL SETANT (50, ANTENN, BASELN, NUNANT, NUNBAS, UNQANT, UNQBAS,
     *   REJECT)
C                                       Make lists
C                                       Case 1.
      IF ((NUNANT.EQ.0) .AND. (NUNBAS.EQ.0)) THEN
         NBASE   = 1
         ANT1(1) = 0
         ANT2(1) = 0
      ELSE IF (.NOT.REJECT) THEN
C                                       Case 2.
         IF (NUNBAS.EQ.0) THEN
            NBASE = NUNANT
            DO 100 I = 1, NUNANT
               ANT1(I) = UNQANT(I)
               ANT2(I) = 0
 100           CONTINUE
         ELSE IF (NUNANT.EQ.0) THEN
            NBASE = NUNBAS
            DO 110 I = 1,NUNBAS
               ANT1(I) = UNQBAS(I)
               ANT2(I) = 0
 110           CONTINUE
C                                       Case 3.
         ELSE
C                                       Construct the list of baselines
C                                       that can be made by pairing
C                                       elements of UNQANT with elements
C                                       of UNQBAS excluding null
C                                       baselines:
            NBASE = 0
            DO 210 I = 1, NUNANT
               DO 200 J = 1,NUNBAS
                  NBASE = NBASE + 1
                  ANT1(NBASE) = MIN (UNQANT(I), UNQBAS(J))
                  ANT2(NBASE) = MAX (UNQANT(I), UNQBAS(J))
 200              CONTINUE
 210           CONTINUE
C                                       NBASE is at least one.
C
C                                       Remove duplicate baselines:
            K = 1
            DO 230 I = 2,NBASE
               DO 220 J = 1,I-1
                  IF ((ANT1(I).EQ.ANT1(J)) .AND. (ANT2(I).EQ.ANT2(J)))
     *               GO TO 230
 220              CONTINUE
               K = K + 1
               ANT1(K) = ANT1(I)
               ANT2(K) = ANT2(I)
 230           CONTINUE
C                                       recount NBASE
            NBASE = K
            END IF
C                                       Case 4 or 5.
      ELSE
C                                       Find the maximum antenna number
C                                       for subarray ISUB or, if ISUB is
C                                       zero or negative find the
C                                       largest maximum antenna number:
         IF (ISUB.GT.0) THEN
            MAXANT = NUMAN(ISUB+1)
         ELSE
            MAXANT = 0
            DO 300 I = 2, NUMAN(1) + 1
               MAXANT = MAX (MAXANT, NUMAN(I))
 300           CONTINUE
            IF (MAXANT.EQ.0) THEN
               MAXANT = 28
               END IF
            END IF

C                                       Save the maximum number of
C                                       allowed baselines:
         MXBASE = NBASE
C                                       (The fact that this is necessary
C                                       indicates some degree of
C                                       sloppiness in the specifications
C                                       for this routine.)

C                                       Generate a complete list of
C                                       baselines and save those that
C                                       meet our criteria:
         NBASE = 0
         DO 320 I = 1,MAXANT
            DO 310 J = I,MAXANT
               LT = REQBAS (I, J, .TRUE., UNQANT, NUNANT, UNQBAS,
     *            NUNBAS)
               IF (LT) THEN
                  IF (NBASE.LT.MXBASE) THEN
                     NBASE = NBASE + 1
                     ANT1(NBASE) = I
                     ANT2(NBASE) = J
C                                       Case 6 (overflow)
                  ELSE
                     NBASE = 0
                     MSGTXT = 'AN10RS: TOO MANY BASELINES IN ' //
     *                  'SELECTION LIST'
                     CALL MSGWRT (6)
                     WRITE (MSGTXT,1300) MXBASE
                     CALL MSGWRT (6)
                     GO TO 999
                     END IF
                  END IF
 310           CONTINUE
 320        CONTINUE
         END IF

 999  RETURN
C-----------------------------------------------------------------------
 1300 FORMAT (8X,'BREAK INTO SUBSETS OF ', I5, ' BASELINES OR LESS')
      END
