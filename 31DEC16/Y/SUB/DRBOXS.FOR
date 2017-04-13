      SUBROUTINE DRBOXS (ICH, TVAREA, CIRCLE, IL, IB, IT, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! draws boxes, circles, diagonal lines for GRBOXS
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2009
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
C   DRBOXS uses a graphics plane to show the user rectangular or
C   circular boxes as they are input or set with the cursor.
C   Inputs:
C      ICH      I      graphics plane to use + NGRAY
C      TVAREA   I(4)   Box must fit in this TV pixel area
C      CIRCLE   L      This is a circle
C      IL       I      Progress in setting: 1 first blc, 2, trc, 3 2nd
C                      blc, 0 => okay box
C                      < 0 => turn off, else turn on
C   In/Out - get fixed (IL > 0) to fit in TVAREA in TV pixels
C      IB       I(2)   Bottom left corner or (1,2) = X,Y radius
C      IT       I(2)   Top right corner or center
C   Output:
C      SCRTCH   I(*)   Scratch buffer: > X dimension (>1280)
C      IERR     I      Error code: > 0 real error
C                         -1 -> box fit partially into display
C                         -2 -> box not visible in this display
C-----------------------------------------------------------------------
      INTEGER   ICH, TVAREA(4), IL, IB(2), IT(2), SCRTCH(*), IERR
      LOGICAL   CIRCLE
C
      CHARACTER OPCODE*4
      INTEGER   IX(33), IY(33), JL, NP, I, IXX, IXN, IYX, IYN, NO, NPO,
     *   LX(2), LY(2), IROUND
      REAL      ANG, BLC(2), TRC(2), XL, YL, XU, YU, XO(2), YO(2)
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      JL = ABS (IL)
      OPCODE = 'ONNN'
      IF (IL.LT.0) OPCODE = 'OFFF'
      BLC(1) = TVAREA(1)
      BLC(2) = TVAREA(2)
      TRC(1) = TVAREA(3)
      TRC(2) = TVAREA(4)
C                                       Fix ups for partial setting
      IF (CIRCLE) THEN
         IF (IL.EQ.1) THEN
            IT(1) = MAX (TVAREA(1)+1, MIN (TVAREA(3)-1, IT(1)))
            IT(2) = MAX (TVAREA(2)+1, MIN (TVAREA(4)-1, IT(2)))
         ELSE IF (IL.EQ.2) THEN
            I = IT(1) - IB(1) - TVAREA(1)
            IF (I.LT.0) IB(1) = IB(1) + I
            I = IT(1) + IB(1) - TVAREA(3)
            IF (I.GT.0) IB(1) = IB(1) - I
            I = IT(2) - IB(2) - TVAREA(2)
            IF (I.LT.0) IB(2) = IB(2) + I
            I = IT(2) + IB(2) - TVAREA(4)
            IF (I.GT.0) IB(2) = IB(2) - I
            IB(1) = MAX (1, IB(1))
            IB(2) = MAX (1, IB(2))
         ELSE IF (IL.EQ.3) THEN
            IT(1) = MAX (TVAREA(1)+1, MIN (TVAREA(3)-1, IT(1)))
            IT(2) = MAX (TVAREA(2)+1, MIN (TVAREA(4)-1, IT(2)))
            I = IT(1) - IB(1) - TVAREA(1)
            IF (I.LT.0) IT(1) = IT(1) + I
            I = IT(1) + IB(1) - TVAREA(3)
            IF (I.GT.0) IT(1) = IT(1) - I
            I = IT(2) - IB(2) - TVAREA(2)
            IF (I.LT.0) IT(2) = IT(2) + I
            I = IT(2) + IB(2) - TVAREA(4)
            IF (I.GT.0) IT(2) = IT(2) - I
            END IF
C                                       fix box
      ELSE IF (IL.GT.0) THEN
         IB(1) = MAX (TVAREA(1), MIN (TVAREA(3), IB(1)))
         IB(2) = MAX (TVAREA(2), MIN (TVAREA(4), IB(2)))
         IF (IL.EQ.1) THEN
            IT(1) = TVAREA(1)
            IT(2) = TVAREA(2)
         ELSE
            IT(1) = MAX (TVAREA(1), MIN (TVAREA(3), IT(1)))
            IT(2) = MAX (TVAREA(2), MIN (TVAREA(4), IT(2)))
            END IF
         END IF
C                                       Box
      IF (.NOT.CIRCLE) THEN
C                                       normal box
         IF (JL.NE.1) THEN
            IX(1) = IB(1)
            IX(2) = IT(1)
            IX(3) = IT(1)
            IX(4) = IB(1)
            IX(5) = IB(1)
            IY(1) = IB(2)
            IY(2) = IB(2)
            IY(3) = IT(2)
            IY(4) = IT(2)
            IY(5) = IB(2)
            NP = 5
C                                       initial BLC
         ELSE
            IX(1) = IB(1)
            IX(2) = IB(1)
            IX(3) = TVAREA(3)
            IY(1) = TVAREA(4)
            IY(2) = IB(2)
            IY(3) = IB(2)
            NP = 3
            END IF
C                                       circle
      ELSE
C                                       draw
         IF (JL.EQ.1) THEN
            IX(1) = IT(1)
            IX(2) = IT(1)
            IX(3) = TVAREA(1)
            IX(4) = TVAREA(3)
            IY(1) = TVAREA(4)
            IY(2) = TVAREA(2)
            IY(3) = IT(2)
            IY(4) = IT(2)
            NP = 2
            CALL IMVECT (OPCODE, ICH, NP, IX(3), IY(3), SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 999
         ELSE
            NP = 32
            DO 20 I = 1,NP
               ANG = (I-1) * DG2RAD * 360.0 / NP
               IX(I) = IT(1) + IB(1) * SIN(ANG) + 0.5
               IY(I) = IT(2) + IB(2) * COS(ANG) + 0.5
 20            CONTINUE
            NP = NP + 1
            IX(NP) = IX(1)
            IY(NP) = IY(1)
            END IF
         END IF
C                                       All on, all off, or partial
      IF (CIRCLE) THEN
         XL = IT(1) - IB(1)
         YL = IT(2) - IB(2)
         XU = IT(1) + IB(1)
         YU = IT(2) + IB(2)
      ELSE
         XL = IB(1)
         YL = IB(2)
         XU = IT(1)
         YU = IT(2)
         END IF
      CALL INTEDG (BLC, TRC, XL, YL, XU, YU, NO, XO, YO)
C                                       so draw all inside
      IF (NO.EQ.-1) THEN
         CALL IMVECT (OPCODE, ICH, NP, IX, IY, SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            IXX = IX(1)
            IXN = IX(1)
            IYN = IY(1)
            IYX = IY(1)
            DO 30 I = 2,NP
               IXX = MAX (IXX, IX(I))
               IYX = MAX (IYX, IY(I))
               IXN = MIN (IXN, IX(I))
               IYN = MIN (IYN, IY(I))
 30            CONTINUE
            WRITE (MSGTXT,1030) IERR, IXN, IYN, IXX, IYX
            CALL MSGWRT (6)
            END IF
C                                       none inside
      ELSE IF (NO.EQ.0) THEN
         IERR = -2
C                                       some inside
      ELSE
         NPO = NP
         NP = 2
         DO 50 I = 2,NPO
            XL = IX(I-1)
            YL = IY(I-1)
            XU = IX(I)
            YU = IY(I)
            CALL INTEDG (BLC, TRC, XL, YL, XU, YU, NO, XO, YO)
            IF (NO.EQ.-1) THEN
               LX(1) = IX(I-1)
               LX(2) = IX(I)
               LY(1) = IY(I-1)
               LY(2) = IY(I)
            ELSE IF (NO.EQ.2) THEN
               LX(1) = MAX (TVAREA(1), MIN (TVAREA(3), IROUND(XO(1))))
               LX(2) = MAX (TVAREA(1), MIN (TVAREA(3), IROUND(XO(2))))
               LY(1) = MAX (TVAREA(2), MIN (TVAREA(4), IROUND(YO(1))))
               LY(2) = MAX (TVAREA(2), MIN (TVAREA(4), IROUND(YO(2))))
            ELSE IF (NO.EQ.1) THEN
               IF ((XL.GE.BLC(1)) .AND. (XL.LE.TRC(1)) .AND.
     *            (YL.GE.BLC(2)) .AND. (YL.LE.TRC(2))) THEN
                  LX(1) = IX(I-1)
                  LY(1) = IY(I-1)
                  LX(2) = MAX (TVAREA(1), MIN (TVAREA(3),
     *               IROUND(XO(1))))
                  LY(2) = MAX (TVAREA(2), MIN (TVAREA(4),
     *               IROUND(YO(1))))
               ELSE
                  LX(1) = MAX (TVAREA(1), MIN (TVAREA(3),
     *               IROUND(XO(1))))
                  LY(1) = MAX (TVAREA(2), MIN (TVAREA(4),
     *               IROUND(YO(1))))
                  LX(2) = IX(I)
                  LY(2) = IY(I)
                  END IF
               END IF
            IF (NO.NE.0) THEN
               CALL IMVECT (OPCODE, ICH, NP, LX, LY, SCRTCH, IERR)
               IF (IERR.NE.0) THEN
                  IXN = MIN (LX(1), LX(2))
                  IYN = MIN (LY(1), LY(2))
                  IXX = MAX (LX(1), LX(2))
                  IYX = MAX (LY(1), LY(2))
                  WRITE (MSGTXT,1030) IERR, IXN, IYN, IXX, IYX
                  CALL MSGWRT (6)
                  GO TO 999
                  END IF
               END IF
 50         CONTINUE
         IERR = -1
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('DRBOXS IMVECT ERROR:',I4,' ON WINDOW',4I5)
      END
