      SUBROUTINE MWFBSM (WIDTH, X, Y, S, BLANK, N, YS)
C-----------------------------------------------------------------------
C! Median window smoothing of an irregularly spaced array with blanking
C# Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2003
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
C   MWFBSM does a median window filter smoothing of irregularly
C   spaced points.  Returns blanked values if no valid data found in the
C   window surrounding each sample.  Does not smooth different source
C   numbers together.  Therefore, make S(*) constant if you wish to
C   smooth all samples.
C   Inputs:
C      WIDTH   R      Width of window in same units as X
C      X       R(N)   Absissae of points to be smoothed.
C      Y       R(N)   Values to be smoothed.
C      S       I(N)   Source numbers - do not smooth across numbers
C                     If S(1) < 0, then S is not otherwise used
C      BLANK   R      Value used for blanking.
C      N       I      Number of points to smooth.
C   Output:
C      YS      R(N)   Smoothed values.
C-----------------------------------------------------------------------
      INTEGER   N, S(N)
      REAL      WIDTH, X(N), Y(N), BLANK, YS(N)
C                                       MWSIZE = size of local array.
      INTEGER   MWSIZE
      PARAMETER (MWSIZE = 5000)
      INTEGER   COUNT, I, J, K, L, IPOS
      REAL      XLAST, XFIRST, ORDER(MWSIZE), HWIDTH
      LOGICAL   OVRFLW, NOSORC
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      OVRFLW = .FALSE.
C                                       no width - just copy
      IF (WIDTH.LE.0.0) THEN
         CALL RCOPY (N, Y, YS)
C                                       Sum around each sample
      ELSE
         NOSORC = S(1).LT.0
         HWIDTH = WIDTH * 0.5
         DO 100 J = 1,N
            IF (Y(J).EQ.BLANK) THEN
               COUNT = 0
            ELSE
               COUNT = 1
               ORDER(COUNT) = Y(J)
               END IF
            XFIRST = X(J) - HWIDTH
            XLAST = X(J) + HWIDTH
C                                       no sources examined
            IF (NOSORC) THEN
               I = J
 10            I = I - 1
               IF (I.GT.0) THEN
                  IF (X(I).GE.XFIRST) THEN
C                                       insert in ordered list
                     IF (Y(I).NE.BLANK) THEN
                        IF (COUNT.EQ.MWSIZE) GO TO 90
                        DO 15 K = 1,COUNT
                           IPOS = K
                           IF (ORDER(K).GT.Y(I)) GO TO 20
 15                        CONTINUE
C                                       add at end
                        IPOS = COUNT + 1
C                                       shift up if needed
 20                     DO 25 K = IPOS,COUNT
                           L = COUNT - K + IPOS
                           ORDER(L+1) = ORDER(L)
 25                        CONTINUE
                        ORDER(IPOS) = Y(I)
                        COUNT = COUNT + 1
                        END IF
                     GO TO 10
                     END IF
                  END IF
               I = J
 30            I = I + 1
               IF (I.LE.N) THEN
                  IF (X(I).LT.XLAST) THEN
C                                       insert in ordered list
                     IF (Y(I).NE.BLANK) THEN
                        IF (COUNT.EQ.MWSIZE) GO TO 90
                        DO 35 K = 1,COUNT
                           IPOS = K
                           IF (ORDER(K).GT.Y(I)) GO TO 40
 35                        CONTINUE
C                                       add at end
                        IPOS = COUNT + 1
C                                       shift up if needed
 40                     DO 45 K = IPOS,COUNT
                           L = COUNT - K + IPOS
                           ORDER(L+1) = ORDER(L)
 45                        CONTINUE
                        ORDER(IPOS) = Y(I)
                        COUNT = COUNT + 1
                        END IF
                     GO TO 30
                     END IF
                  END IF
C                                       Pay attention to source
            ELSE
               I = J
 50            I = I - 1
               IF (I.GT.0) THEN
                  IF (X(I).GE.XFIRST) THEN
C                                       insert in ordered list
                     IF ((S(I).EQ.S(J)) .AND. (Y(I).NE.BLANK)) THEN
                        IF (COUNT.EQ.MWSIZE) GO TO 90
                        DO 55 K = 1,COUNT
                           IPOS = K
                           IF (ORDER(K).GT.Y(I)) GO TO 60
 55                        CONTINUE
C                                       add at end
                        IPOS = COUNT + 1
C                                       shift up if needed
 60                     DO 65 K = IPOS,COUNT
                           L = COUNT - K + IPOS
                           ORDER(L+1) = ORDER(L)
 65                        CONTINUE
                        ORDER(IPOS) = Y(I)
                        COUNT = COUNT + 1
                        END IF
                     GO TO 50
                     END IF
                  END IF
               I = J
 70            I = I + 1
               IF (I.LE.N) THEN
                  IF (X(I).LT.XLAST) THEN
C                                       insert in ordered list
                     IF ((S(I).EQ.S(J)) .AND. (Y(I).NE.BLANK)) THEN
                        IF (COUNT.EQ.MWSIZE) GO TO 90
                        DO 75 K = 1,COUNT
                           IPOS = K
                           IF (ORDER(K).GT.Y(I)) GO TO 80
 75                        CONTINUE
C                                       add at end
                        IPOS = COUNT + 1
C                                       shift up if needed
 80                     DO 85 K = IPOS,COUNT
                           L = COUNT - K + IPOS
                           ORDER(L+1) = ORDER(L)
 85                        CONTINUE
                        ORDER(IPOS) = Y(I)
                        COUNT = COUNT + 1
                        END IF
                     GO TO 70
                     END IF
                  END IF
               END IF
C                                       answer
 90         IF (COUNT.GE.MWSIZE) OVRFLW = .TRUE.
            IF (COUNT.GT.0) THEN
               IPOS = COUNT / 2 + 1
               IF (MOD(COUNT,2).EQ.1) THEN
                  YS(J) = ORDER(IPOS)
               ELSE
                  YS(J) = (ORDER(IPOS-1) + ORDER(IPOS)) / 2.0
                  END IF
            ELSE
               YS(J) = BLANK
               END IF
 100        CONTINUE
         END IF
      IF (OVRFLW) THEN
         MSGTXT = 'MWFBSM: RESULTS INACCURATE DUE TO EXCESS WIDTH'
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
      END
