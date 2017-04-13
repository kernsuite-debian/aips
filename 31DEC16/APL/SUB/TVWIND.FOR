      SUBROUTINE TVWIND (TYPE, PXINC, BLC, TRC, ICHAN, ITVC, IWIN, IERR)
C-----------------------------------------------------------------------
C! determines image windows for TV, including for interpolation & Roam
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004, 2008, 2010
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
C   TVWIND sets windows for normal and split screen TV loads.
C   In/out:
C      TYPE   I       In: <0 -> 1 plane, other -> split method
C                     Out: 0 -> 1 plane, other = 100 * (#planes in X) +
C                                                (# planes in Y)
C      PXINC  I(2)    X, Y increments
C      BLC    R(7)    User requested bot left corner
C      TRC    R(7)    User requested top right corner
C      ICHAN  I       User requested TV chan (decimal form)
C      ITVC   I(4)    IN: first 2 user req. TVCORN
C                     Out: full "pseudo-TV" corners
C   Output:
C      IWIN   I(4)    Window into map
C      IERR   I       error code: 0 -> ok, else fatal
C   Common:
C      /MAPHDR/ CATBLK image header used extensively, the depth array is
C                                   set here
C-----------------------------------------------------------------------
      INTEGER   TYPE, PXINC(2), ICHAN, ITVC(4), IWIN(4), IERR
      REAL      BLC(7), TRC(7)
C
      INTEGER   I, J, IX(2), INTYPE, NFR(2), NX, NY, PLINC(2), NINT(2),
     *   JJ
      LOGICAL   DOSPLT(2)
      REAL      EPS, XFR(2)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA EPS /0.01/
C-----------------------------------------------------------------------
C                                       Check, set image window
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 10 I = 1,5
         CATBLK(IIDEP+I-1) = BLC(I+2) + EPS
 10      CONTINUE
C                                       Check increments
      PLINC(1) = MAX (1, PXINC(1))
      PLINC(2) = MAX (1, PXINC(2))
C                                       Best guess windows to start
      IWIN(1) = BLC(1)
      IWIN(2) = BLC(2)
      IWIN(3) = TRC(1)
      IWIN(4) = TRC(2)
      IX(1) = MAXXTV(1)
      IX(2) = MAXXTV(2)
C                                       Determine type: a mess
      INTYPE = TYPE
      TYPE = 0
C                                       Roam type requested
      IF ((INTYPE.GE.0) .AND. (NGRAY.GT.1)) THEN
C                                       Which are large axes?
         DO 15 I = 1,2
            J = (IWIN(I+2)-IWIN(I)+1)/PLINC(I)
            IF (ITVC(I).GT.0) J = J + ITVC(I)
            DOSPLT(I) = J .GT. IX(I)
            NFR(I) = (J-1) / IX(I) + 1
            XFR(I) = REAL(J) / REAL(IX(I))
 15         CONTINUE
         DOSPLT(1) = (DOSPLT(1)) .AND. (TYPSPL.GT.0) .AND.
     *      (TYPSPL.NE.2)
         DOSPLT(2) = (DOSPLT(2)) .AND. (TYPSPL.GT.1)
C                                       Which requested ?
         IF (.NOT.DOSPLT(1)) NFR(1) = 1
         IF (.NOT.DOSPLT(2)) NFR(2) = 1
         IF (INTYPE.GT.0) THEN
            NX = INTYPE / 100
            NY = MOD (INTYPE, 100)
            IF ((NX.GE.1) .AND. (NX.LE.NGRAY) .AND. (NY.GE.1) .AND.
     *         (NY.LE.NGRAY)) THEN
               NFR(1) = MIN (NX, NFR(1))
               NFR(2) = MIN (NY, NFR(2))
               DOSPLT(1) = NFR(1).GT.1
               DOSPLT(2) = NFR(2).GT.1
               END IF
            END IF
 20      I = 2
         IF (XFR(2).GT.XFR(1)) I = 1
         IF ((DOSPLT(1)) .AND. (DOSPLT(2)) .AND. ((TYPSPL.NE.4)
     *      .OR. (NGRAY.LT.4))) DOSPLT(I) = .FALSE.
         IF (.NOT.DOSPLT(I)) NFR(I) = 1
         IF ((.NOT.DOSPLT(1)) .AND. (.NOT.DOSPLT(2))) THEN
            ICHAN = MAX (1, MIN (NGRAY, ICHAN))
            GO TO 50
            END IF
         IF (NFR(1)*NFR(2).GT.NGRAY) THEN
            NFR(1) = XFR(1) + 0.9
            NFR(2) = XFR(2) + 0.9
            NFR(1) = MIN (NGRAY, NFR(1))
            NFR(2) = MIN (NGRAY, NFR(2))
 21         IF (NFR(1)*NFR(2).GT.NGRAY) THEN
               NFR(3-I) = MAX (1, NFR(3-I)-1)
               IF (NFR(1)*NFR(2).GT.NGRAY) NFR(I) = MAX (NFR(I)-1, 1)
               GO TO 21
               END IF
            END IF
         DOSPLT(1) = NFR(1).GT.1
         DOSPLT(2) = NFR(2).GT.1
 30      IX(1) = IX(1) * NFR(1)
         IX(2) = IX(2) * NFR(2)
         JJ = NFR(1) * NFR(2)
         TYPE = 100 * NFR(1) + NFR(2)
C                                       Forget ICHAN
C                                       Use the default
         ICHAN = (2**JJ) - 1
C                                       single channel instead
      ELSE
         ICHAN = MAX (1, MIN (NGRAY, ICHAN))
         END IF
C                                       Interpolation?
 50   NINT(1) = 1
      NINT(2) = 1
      IF ((PXINC(1).LT.0) .OR. (PXINC(2).LT.0)) THEN
         IF (PXINC(1).LE.-2) THEN
            NINT(1) = -PXINC(1)
         ELSE IF (PXINC(1).EQ.-1) THEN
            NINT(1) = (IX(1) - 1) / (IWIN(3) - IWIN(1))
            NINT(1) = MAX (1, NINT(1))
            END IF
         IF (PXINC(2).LE.-2) THEN
            NINT(2) = -PXINC(2)
         ELSE IF (PXINC(2).EQ.-1) THEN
            NINT(2) = (IX(2) - 1) / (IWIN(4) - IWIN(2))
            NINT(2) = MAX (1, NINT(2))
            END IF
         IF ((PXINC(2).EQ.-1) .AND. (NINT(1).EQ.NINT(2)+1)) NINT(1) =
     *      NINT(2)
         IF ((PXINC(1).EQ.-1) .AND. (NINT(2).EQ.NINT(1)+1)) NINT(2) =
     *      NINT(1)
         END IF
C                                       Check & set windows
 65   DO 90 I = 1,2
         IWIN(I+2) = IWIN(I+2) - MOD (IWIN(I+2)-IWIN(I), PLINC(I))
         IF (ITVC(I).GT.0) GO TO 70
            ITVC(I) = (IX(I) - (NINT(I)*(IWIN(I+2)-IWIN(I)))/PLINC(I)+1)
     *         / 2
            IF (ITVC(I).GE.1) GO TO 70
               ITVC(I) = 1
               IWIN(I) = (IWIN(I+2) + IWIN(I) - IX(I)*PLINC(I)/NINT(I)
     *            + 1) / 2
               GO TO 80
 70      ITVC(I+2) = ITVC(I) + (NINT(I) * (IWIN(I+2)-IWIN(I)))/PLINC(I)
         IF (ITVC(I+2).LE.IX(I)) GO TO 90
 80         ITVC(I+2) = IX(I)
            IWIN(I+2) = IWIN(I) + (ITVC(I+2)-ITVC(I))*PLINC(I)/NINT(I)
 90      CONTINUE
      IF ((PXINC(1).LT.0) .AND. (NINT(1).GT.1)) PLINC(1) = -NINT(1)
      IF ((PXINC(2).LT.0) .AND. (NINT(2).GT.1)) PLINC(2) = -NINT(2)
      PXINC(1) = PLINC(1)
      PXINC(2) = PLINC(2)
C
 999  RETURN
      END
