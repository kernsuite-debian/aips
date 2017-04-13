      SUBROUTINE Q1GRD (UV, VIS, WT, L, INCF, NT2, M, LROW, CNT, NFREQ)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Grid a uv data.
C# AP-appl UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002, 2006, 2012
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
C   Vectorizing compiler version
C     Grids visibility data that has been loaded into the AP previously,
C   If requested, positions are shifted and then a taper is applied to
C   the weights if requested before gridding. Multiple frequency
C   channels may be gridded together for bandwidth synthesis.
C      Two work words are assumed before the first visibility.
C   Inputs:
C      UV    I  base address of u,v vector
C      VIS   I  base address of visibility
C      WT    I  base address of weights
C      L     I  length of visibility record
C      INCF  I  Increment between visibilities and weights,
C               stepping in frequency.
C      NT2   I  INT ( (no. cells used on a row)/2 )
C      M     I  number of rows kept in AP (MUST be odd)
C      LROW  I  length of a row (V)
C      CNT   I  number of visibility points.
C      NFREQ I  number of frequencies to grid.
C               If NFREQ is negative then tapering is requested.
C               If CNT is neg, do not shift data.
C   Expects necessary constants in following AP locations:
C           8 = Row number of lowest central row to grid
C           9 = -SIG(U)**2 (CELLS**2) for taper
C          10 = -SIG(V)**2 (CELLS**2) for taper
C          11 = U scaling to cells
C          12 = V scaling to cells
C          13 = W scaling to cells
C          14 = DXC = -2 * PI * delta RA (in 1/cells) for 1ST
C          15 = DYC = -2 * PI * delta DEC         frequency channel
C          16 = DZC = -2 * PI * delta Z
C          17 = AP address containing integer-valued address of base
C               address of row convolving fn. (Y on sky)
C          18 = AP address containing integer-valued address of base
C               address of col. convolving fn. (X on sky)
C          19 = AP address containing integer-valued address of base
C               address of grid.
C          20...20+NFREQ-2 = differential frequency scaling table
C               for channels after the first one.
C               FREQ(n) = (1+MD(16+n-1)) * FREQ(n-1) for n>1
C   NOTE: Input addresses are 0 relative, must be converted to
C   1 relative before use.
C-----------------------------------------------------------------------
      INTEGER   UV, VIS, WT, L, INCF, NT2, M, LROW, CNT, NFREQ
C
      INTEGER   NOTAP, NOSHF, VVIS, WWT, IFRQ, JNFREQ, JCNT, LOOP, WORK,
     *   ITEMP1, ITEMP2, ITEMP3
      LONGINT   JA, JC, IWRK
      DOUBLE PRECISION VALUE1, VALUE2, VALUE3, TEMPR1, TEMPR2, TEMPI1,
     *   TEMPI2, TEXP
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                       Save variables to be changed
      JCNT = CNT
      JNFREQ = NFREQ
      VVIS = VIS
      WWT = WT
      WORK = VIS - 2
C                                       Check if tapered
      NOTAP = JNFREQ
      IF (JNFREQ.LT.0) JNFREQ = - JNFREQ
C                                       Check shift
      NOSHF = JCNT
      IF (JCNT.EQ.0) GO TO 999
      IF (JCNT.LT.0) JCNT = -JCNT
C                                       Scale u,v,w to cells.
      JA = UV + PSAPOF
      VALUE1 = APCORE(PSAPOF+11)
      VALUE2 = APCORE(PSAPOF+12)
      VALUE3 = APCORE(PSAPOF+13)
      INCLUDE 'INCS:ZVND.INC'
      DO 20 LOOP = 1,JCNT
         APCORE (JA) = APCORE(JA) * VALUE1
         APCORE (JA+1) = APCORE(JA+1) * VALUE2
         APCORE (JA+2) = APCORE(JA+2) * VALUE3
         JA = JA + L
 20      CONTINUE
C                                       Frequency looping, rescale uv
      DO 300 IFRQ = 1,JNFREQ
C                                       Don't rescale on first pass.
         IF (IFRQ.NE.1) THEN
            VALUE1 = APCORE(IFRQ+PSAPOF+18)
            JA = UV + PSAPOF
      INCLUDE 'INCS:ZVND.INC'
            DO 100 LOOP = 1,JCNT
               APCORE(JA) = (VALUE1 * APCORE(JA)) + APCORE(JA)
               APCORE(JA+1) = (VALUE1 * APCORE(JA+1)) + APCORE(JA+1)
               APCORE(JA+2) = (VALUE1 * APCORE(JA+2)) + APCORE(JA+2)
               JA = JA + L
 100           CONTINUE
C                                       end if first pass
            END IF
C                                       Taper
         IF (NOTAP.LE.0) THEN
            JA = UV + PSAPOF
            JC = WWT + PSAPOF
            VALUE1 = APCORE(PSAPOF+9)
            VALUE2 = APCORE(PSAPOF+10)
      INCLUDE 'INCS:ZVND.INC'
            DO 120 LOOP = 1,JCNT
               TEXP = VALUE1 * APCORE(JA) * APCORE(JA) +
     *            VALUE2 * APCORE(JA+1) * APCORE(JA+1)
               IF (TEXP.GT.-14.0) THEN
                  APCORE(JC) = APCORE(JC) * EXP (TEXP)
               ELSE
                  APCORE(JC) = 0.0
                  END IF
               JA = JA + L
               JC = JC + L
 120           CONTINUE
C                                       end if taper
            END IF
C                                       Shift position.
         IF (NOSHF.GT.0) THEN
            VALUE1 = APCORE(PSAPOF+14)
            VALUE2 = APCORE(PSAPOF+15)
            VALUE3 = APCORE(PSAPOF+16)
            JA = UV + PSAPOF
            IWRK = WORK + PSAPOF
      INCLUDE 'INCS:ZVND.INC'
            DO 150 LOOP = 1,JCNT
C                                       Set phase to shift
               APCORE(IWRK+1) = APCORE(JA) * VALUE1 +
     *            APCORE(JA+1) * VALUE2 + APCORE(JA+2) * VALUE3
               JA = JA + L
               IWRK = IWRK + L
 150           CONTINUE
C                                       Calc Cos and Sin of Phase
            CALL QRECT1 (WORK, L, WORK, L, JCNT)
            JC = VVIS + PSAPOF
            IWRK = WORK + PSAPOF
      INCLUDE 'INCS:ZVND.INC'
            DO 160 LOOP = 1,JCNT
C                                       Shift positions
               TEMPR1 = APCORE(IWRK)
               TEMPI1 = APCORE(IWRK+1)
               TEMPR2 = APCORE(JC)
               TEMPI2 = APCORE(JC+1)
               APCORE(JC)   = TEMPR1 * TEMPR2 - TEMPI1 * TEMPI2
               APCORE(JC+1) = TEMPR1 * TEMPI2 + TEMPI1 * TEMPR2
               IWRK = IWRK + L
               JC = JC + L
 160           CONTINUE
C                                       end if shifting
            END IF
C                                       Grid, Correct row number to
C                                       lowest central row.
         ITEMP1 = APCORI(PSAPOF+19)
         ITEMP2 = APCORI(PSAPOF+17)
         ITEMP3 = APCORI(PSAPOF+18)
         CALL QGRD4 (UV, VVIS, WWT, ITEMP1, ITEMP2, ITEMP3, NT2, M,
     *      LROW, 8, L, JCNT)
         VVIS = VVIS + INCF
         WWT = WWT + INCF
 300     CONTINUE
C
 999  RETURN
      END
