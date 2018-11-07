      SUBROUTINE ISCALE (TYP, OMAX, RANGE, NPIX, NINC, RBUF, OUTBUF)
C-----------------------------------------------------------------------
C! scale a buffer by various functions to an integer buffer (ie for TV)
C# TV-util Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-1999, 2002, 2006
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
C   Rescale a line of map data in preparation for output device
C   Inputs:
C      TYP      C*2    Type of scaling function '  ' = linear
C                      'SQ' square root,      'NQ' negative sqrt
C                      'LG' log               'NG' negtive log
C                      'LN' linear (default)  'NL' negative linear
C      OMAX     I      Maximum value accepted by output device
C      RANGE    R(2)   Map range to transfer; RANGE(1) and (2)
C                      should be transfered to output 0 and OMAX
C                      RANGE(1) < RANGE(2) forced inside routine.
C      NPIX     I      Pixels/line
C      NINC     I      Increment between included pixels
C      RBUF     R(*)   Input buffer
C   Outputs:
C      OUTBUF   I(*)   output buffer with scaled data
C-----------------------------------------------------------------------
      CHARACTER TYP*2
      INTEGER   OMAX, NPIX, NINC, OUTBUF(*)
      REAL      RANGE(2), RBUF(*)
C
      INTEGER   I, NC, LINC
      LOGICAL   NEGATV
      REAL      SCL, RMIN, RMAX, RZERO, X, Y, XRANGE(2), RLOFF,
     *   XMAX, XMIN, RSCALE(2)
      INCLUDE 'INCS:DDCH.INC'
      SAVE RSCALE
      DATA RSCALE /9.0, 99.0/
C-----------------------------------------------------------------------
      NC = 0
      LINC = MAX (1, NINC)
      XRANGE(1) = RANGE(1)
      XRANGE(2) = RANGE(2)
      IF (XRANGE(2).EQ.XRANGE(1)) THEN
         XRANGE(1) = XRANGE(1) - 0.5
         XRANGE(2) = XRANGE(2) + 0.5
         END IF
C                                       Choose transfer type
      XMAX = MAX (XRANGE(1), XRANGE(2))
      XMIN = MIN (XRANGE(1), XRANGE(2))
      NEGATV = TYP(1:1).EQ.'N'
      RMAX = OMAX + 0.99
      RMIN = 1.01
C                                       Square root
      IF ((TYP.EQ.'SQ') .OR. (TYP.EQ.'NQ')) THEN
         SCL = (OMAX-0.02) / SQRT (ABS(XRANGE(2) -XRANGE(1)))
         DO 110 I = 1,NPIX,LINC
            Y = 0.0
            X = RBUF(I)
            IF (X.NE.FBLANK) THEN
               X = MIN (XMAX, MAX (XMIN, X))
               IF (NEGATV) THEN
                  Y = SCL * SQRT (ABS(X-XRANGE(2))) + RMIN
               ELSE
                  Y = SCL * SQRT (ABS(X-XRANGE(1))) + RMIN
                  END IF
               Y = MAX (RMIN, MIN (RMAX, Y))
               END IF
            NC = NC + 1
            OUTBUF(NC) = Y
 110        CONTINUE
C                                       logarithmic transfer function
      ELSE IF ((TYP.EQ.'LG') .OR. (TYP.EQ.'NG') .OR. (TYP.EQ.'L2') .OR.
     *   (TYP.EQ.'N2')) THEN
         RLOFF = RSCALE(1) / (XRANGE(2) - XRANGE(1))
         IF (TYP(2:2).EQ.'2') RLOFF = RSCALE(2) / (XRANGE(2)-XRANGE(1))
         RZERO = RLOFF * XRANGE(1) - 1.0
         IF (NEGATV) RZERO = RLOFF * XRANGE(2) + 1.0
         SCL = (OMAX-0.02) / LOG (1.0 + RLOFF * (XRANGE(2) -XRANGE(1)))
         RMAX = OMAX + 0.99
         RMIN = 1.01
         DO 210 I = 1,NPIX,LINC
            Y = 0.0
            X = RBUF(I)
            IF (X.NE.FBLANK) THEN
               X = RLOFF * MIN (XMAX, MAX (XMIN, X))
               IF (NEGATV) THEN
                  Y = SCL * LOG (RZERO - X) + RMIN
               ELSE
                  Y = SCL * LOG (X - RZERO) + RMIN
                  END IF
               Y = MAX (RMIN, MIN (RMAX, Y))
               END IF
            NC = NC + 1
            OUTBUF(NC) = Y
 210        CONTINUE
C                                       linear transfers
      ELSE
         SCL = (OMAX - 0.02) / (XRANGE(2) - XRANGE(1))
         RZERO = XRANGE(1) * SCL - 1.01
         RMAX = OMAX + 0.99
         RMIN = 1.01
         DO 310 I = 1,NPIX,LINC
            Y = 0.0
            IF (RBUF(I).NE.FBLANK) THEN
               Y = SCL * RBUF(I) - RZERO
               Y = MAX (RMIN, MIN (RMAX, Y))
               IF (NEGATV) Y = RMAX + RMIN - Y
               END IF
            NC = NC + 1
            OUTBUF(NC) = Y
 310        CONTINUE
         END IF
C
 999  RETURN
      END
