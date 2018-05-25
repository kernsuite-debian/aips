      SUBROUTINE METSCL (LTYPE, X, PREFIX, PFLAG)
C-----------------------------------------------------------------------
C! scale a value to deired range and provide a metric prefix to match
C# Utility Coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 2014
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
C   Scale the real value X to a value 0.99995 - 999.95 and pick a
C   standard metric prefix to match.  If X is too large or small to
C   scale, leave it unchanged and set PFLAG to TRUE.  Note that this
C   result should be displayed with formats F6.1, F7.2, etc.  DO NOT
C   use format Fn.0 with n < 6 - an overflow could occur.
C   Inputs:
C      LTYPE   I     Plot type: j = abs(ltype)/100 -> move the
C                       scaling closer to none as
c                    j = 1 -> one step if only 1 or 2 away
c                    j = 2 -> one or two steps if only 1 or 2 away
c                    j = 3 -> one step
c                    j = 4 -> one or two steps
c                    within 2
C   In/Out:
C      X       R     input value
C   Outputs:
C      PREFIX  C*5   4 character/word (<= 5 chars total) metric prefix
C      PFLAG   L     if .FALSE. scaling worked;
C                    if .TRUE. X has been left unchanged
C-----------------------------------------------------------------------
      INTEGER   LTYPE
      CHARACTER PREFIX*5
      REAL      X
      LOGICAL   PFLAG
C                                       Declare max prefix number,blank
      INTEGER   MXPREF, NF
      PARAMETER (MXPREF=17, NF=9)
      CHARACTER PLIST(MXPREF)*5
      REAL      Y
      INTEGER   I, J, K
      DATA PLIST /'Yocto', 'Zepto', 'Atto ', 'Femto', 'Pico ', 'Nano ',
     *   'Micro', 'Milli', '     ', 'Kilo ', 'Mega ', 'Giga ', 'Tera ',
     *   'Peta ', 'Exa  ', 'Zetta', 'Yotta'/
C-----------------------------------------------------------------------
C                                       check for zero
      PFLAG = .FALSE.
      PREFIX = PLIST(NF)
      IF (X.NE.0.0) THEN
         J = ABS (LTYPE) / 100
C                                       get log10 in 3's, check rng
         Y = NF + (LOG10 (ABS (X)) / 3.0)
         PFLAG = ((Y.LT.1.0) .OR. (Y.GT.MXPREF))
C                                       do scaling, check
         IF (.NOT.PFLAG) THEN
            I = Y
            X = X * (10.0 ** (3*(NF-I)))
            IF (X.GT.999.949) THEN
               X = X / 1000.0
               I = I + 1
               END IF
            PFLAG = ((I.LT.1) .OR. (I.GT.MXPREF))
            END IF
C                                       move closer?
         IF (.NOT.PFLAG) THEN
            K = I - NF
            IF ((J.GT.0) .AND. (K.NE.0)) THEN
               IF ((ABS(K).EQ.1) .OR. ((J.EQ.1) .AND. (ABS(K).LE.2))
     *            .OR. (J.EQ.3)) THEN
                  IF (K.GT.0) THEN
                     I = I - 1
                     X = X * 1000.0
                  ELSE
                     I = I + 1
                     X = X / 1000.0
                     END IF
               ELSE IF (((ABS(K).LE.2) .AND. (J.EQ.2)) .OR. (J.EQ.4))
     *            THEN
                  IF (K.GT.0) THEN
                     I = I - 2
                     X = X * 1000000.0
                  ELSE
                     I = I + 2
                     X = X / 1000000.0
                     END IF
                  END IF
               END IF
C                                       set prefix
            PREFIX = PLIST(MAX(1,MIN(I,MXPREF)))
            END IF
         END IF
C
 999  RETURN
      END
