      SUBROUTINE QGRIDA (UV, VIS, WT, L, G, CX, CY, CONI, NO2, M, LROW,
     *   CNT, TY)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Grid visibility data.
C# AP-appl UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002, 2006-2007, 2012
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
C   Vectorizing compiler version (also OK for Scalar).
C   QGRIDA grids visibility data that has been loaded into the AP
C   previously.  If requested, positions are shifted and then
C   a taper is applied to the weights if requested before gridding.
C   when taper is requested and TY=2 or 3 locations VIS-2 and
C   VIS-1 are used for work space. If TY=1 locations VIS+2 and
C   VIS+3 are used.
C   Inputs:
C      UV    I  Base address of U,V vector
C      VIS   I  Base address of visibility
C      WTP   I  Base address of weights
C      L     I  Length of visibility record
C      G     I  Base address of grid
C      CX    I  Base address of row convolving fn. (Y on sky)
C      CY    I  Base address of col. convolving fn. (X on sky)
C               Convolving fns. tabulated every 1/CONI cell
C      CONI  I  Increment of conv function tabulation (assumed 100 for
C               true visibility data)
C      NO2   I  INT ( (no. cells used on a row)/2 )
C      M     I  Number of rows kept in AP (must be odd)
C      LROW  I  Length of a row (V)
C      CNT   I  Number of visibility points.
C      TY    I  Type of visibility data.
C                      1 = I maps
C                      2 = Q,U maps, or two line maps
C                      3 = V maps
C                      4 = SD data (imaginary part 0)
C                      5 = SD data - sigma**2 image wanted
C                If TY is negative then tapering is requested.
C                If CNT is neg, do not shift data.
C   Also expects necessary constants in following AP locations:
C           8 = -SIG(U)**2 (CELLS**2) for taper
C           9 = -SIG(V)**2 (CELLS**2) for taper
C          10 = U scaling to cells
C          11 = V scaling to cells
C          12 = W scaling to cells
C          13 = DXC = -2 * Pi * delta RA (in 1/cells)
C          14 = DYC = -2 * Pi * delta DEC
C          15 = DZC = -2 * Pi * delta Z
C-----------------------------------------------------------------------
      INTEGER   UV, VIS, WT, L, G, CX, CY, CONI, NO2, M, LROW, CNT, TY
C
      INTEGER   IWORK1, IWORK2, VIS2, NOSHF, NOTAP, JCNT, II, IUV, IVIS,
     *   IWT, ITY, LOOP
      LONGINT   JA, JC, IWRK
      DOUBLE PRECISION VALUE1, VALUE2, VALUE3, TEMPR1, TEMPI1, TEMPR2,
     *   TEMPI2, TEXP
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                       Save values to be changed
      IUV = UV
      IVIS = VIS
      IWT = WT
      JCNT = ABS (CNT)
C                                       Check shift
      NOSHF = CNT
      IF (JCNT.LE.0) GO TO 999
C                                       Scale u,v,w to cells.
      JA = UV + PSAPOF
      VALUE1 = APCORE(10+PSAPOF)
      VALUE2 = APCORE(11+PSAPOF)
      VALUE3 = APCORE(12+PSAPOF)
      IF ((VALUE1.NE.1.0D0) .OR. (VALUE2.NE.1.0D0) .OR.
     *   (VALUE3.NE.1.0D0)) THEN
      INCLUDE 'INCS:ZVND.INC'
         DO 20 LOOP = 1,JCNT
            APCORE (JA) = APCORE(JA) * VALUE1
            APCORE (JA+1) = APCORE(JA+1) * VALUE2
            APCORE (JA+2) = APCORE(JA+2) * VALUE3
            JA = JA + L
 20         CONTINUE
         END IF
C                                       Check if tapered and set work
C                                       pointers
      NOTAP = TY
      ITY = ABS (TY)
C                                       Set addresses of work array
      IF ((ITY.EQ.1) .OR. (ITY.EQ.4) .OR. (ITY.EQ.5)) THEN
         IWORK1 = VIS + 2
      ELSE
         IWORK1 = VIS - 2
         END IF
      IWORK2 = IWORK1 + 1
C                                       Taper
      IF (NOTAP.LE.0) THEN
         JA = UV + PSAPOF
         JC = WT + PSAPOF
         VALUE1 = APCORE(8+PSAPOF)
         VALUE2 = APCORE(9+PSAPOF)
      INCLUDE 'INCS:ZVND.INC'
         DO 80 LOOP = 1,JCNT
            TEXP = VALUE1 * APCORE(JA) * APCORE(JA) +
     *         VALUE2 * APCORE(JA+1) * APCORE(JA+1)
            IF (TEXP.GT.-14.0D0) THEN
               APCORE(JC) = APCORE(JC) * EXP (TEXP)
            ELSE
               APCORE(JC) = 0.0D0
               END IF
            JA = JA + L
            JC = JC + L
 80         CONTINUE
C                                       end if taper
         END IF
C                                       Shift position
      IF (NOSHF.GE.0) THEN
         VALUE1 = APCORE(13+PSAPOF)
         VALUE2 = APCORE(14+PSAPOF)
         VALUE3 = APCORE(15+PSAPOF)
         JA = UV + PSAPOF
C                                       put phase in second element
         IWRK = IWORK1 + 1 + PSAPOF
      INCLUDE 'INCS:ZVND.INC'
         DO 150 LOOP = 1,JCNT
C                                       Set phase to shift
            APCORE(IWRK) = APCORE(JA) * VALUE1 +
     *         APCORE(JA+1) * VALUE2 + APCORE(JA+2) * VALUE3
            JA = JA + L
            IWRK = IWRK + L
 150        CONTINUE
C                                       Convert to polar
         CALL QRECT1 (IWORK1, L, IWORK1, L, JCNT)
         JC = VIS + PSAPOF
         IWRK = IWORK1 + PSAPOF
      INCLUDE 'INCS:ZVND.INC'
         DO 160 LOOP = 1,JCNT
C                                       Shift positions
            TEMPR1 = APCORE(IWRK)
            TEMPI1 = APCORE(IWRK+1)
            TEMPR2 = APCORE(JC)
            TEMPI2 = APCORE(JC+1)
            APCORE(JC) = TEMPR1 * TEMPR2 - TEMPI1 * TEMPI2
            APCORE(JC+1) = TEMPR1 * TEMPI2 + TEMPI1 * TEMPR2
            IWRK = IWRK + L
            JC = JC + L
 160        CONTINUE
C                                       end if shift
         END IF
C                                       single dish
      IF (ITY.EQ.4) THEN
         DO 200 II = 1,JCNT
            CALL QGRD5 (IUV, IVIS, IWT, G, CX, CY, CONI, NO2, M, LROW)
C                                       Update pointers
            IUV = IUV + L
            IVIS = IVIS + L
            IWT = IWT + L
 200        CONTINUE
C                                       single dish sigma**2
      ELSE IF (ITY.EQ.5) THEN
         DO 210 II = 1,JCNT
            CALL QGRD8 (IUV, IVIS, IWT, G, CX, CY, CONI, NO2, M, LROW)
C                                       Update pointers
            IUV = IUV + L
            IVIS = IVIS + L
            IWT = IWT + L
 210        CONTINUE
C                                       IPOL (and beam) or VPOL
      ELSE IF (ITY.NE.2) THEN
         DO 250 II = 1,JCNT
            CALL QGRD3 (IUV, IVIS, IWT, G, CX, CY, NO2, M, LROW)
C                                       Update pointers
            IUV = IUV + L
            IVIS = IVIS + L
            IWT = IWT + L
 250        CONTINUE
      ELSE
C                                       Two map gridding. (Q-UPOL-line)
C                                       Shift positions (vis2)
         IF (NOSHF.GE.0) THEN
            VIS2 = VIS + 2
            JC = VIS2 + PSAPOF
            IWRK = IWORK1 + PSAPOF
      INCLUDE 'INCS:ZVND.INC'
            DO 350 LOOP = 1,JCNT
C                                       Shift positions
               TEMPR1 = APCORE(IWRK)
               TEMPI1 = APCORE(IWRK+1)
               TEMPR2 = APCORE(JC)
               TEMPI2 = APCORE(JC+1)
               APCORE(JC) = TEMPR1 * TEMPR2 - TEMPI1 * TEMPI2
               APCORE(JC+1) = TEMPR1 * TEMPI2 + TEMPI1 * TEMPR2
               IWRK = IWRK + L
               JC = JC + L
 350           CONTINUE
C                                       end if shift
            END IF
         DO 450 II = 1,JCNT
            CALL QGRD2 (IUV, IVIS, IWT, G, CX, CY, NO2, M, LROW)
C                                       Update pointers
            IUV = IUV + L
            IVIS = IVIS + L
            IWT = IWT + L
 450        CONTINUE
C                                       end if IPOL or VPOL or Q-UPOL
         END IF
C
 999  RETURN
      END
