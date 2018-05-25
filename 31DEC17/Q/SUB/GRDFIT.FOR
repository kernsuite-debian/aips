      SUBROUTINE GRDFIT (TYPE, LREC, FREQS, MXABSU, METHOD, BSIZE, IERR)
C-----------------------------------------------------------------------
C! Decides if weighting and gridding fit in the AP or need a sort
C# AP-appl Imaging
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999, 2006, 2008
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
C   Decides if the imaging parameters and uniform weighting parameters
C   will fit in the AP with or without a sort.  Also considers the
C   gridded model subtraction needs.
C   Inputs:
C      TYPE     I      1 -> unif only, 2 -> grid only, 3 -> gridded
C                      Clean subtraction only, 0 -> all
C      LREC     I      Words/record in UV data
C      FREQS    D(*)   Frequencies of channels averaged
C      MXABSU   R      Max abs(U) including all freq scaling
C      METHOD   C*4    CC subtraction method 'DFT ' => no need to sort
C                      for the subtraction stage
C   Inputs via common DMPR.INC
C      DOUNIF   L      Do uniform weighting or no
C      FREQUV   D      Reference frequency
C      NXUNF    I      Size in U direction for uniform weighting array
C      NYUNF    I      Size in V direction for uniform weighting array
C      UNFBOX   I      Radius of uniform weight support
C      NXMAX    I      X points in image and beams
C      NYMAX    I      Y points in image and beams
C      ROBUST   R      -3 to +3 => do robust counting
C      CTYPX    I      X convolution function type
C      CTYPY    I      Y convolution function type
C   In/Out via DMPR:
C      XPARM    R(10)  X convolution function parameters
C      YPARM    R(10)  Y convolution function parameters
C   Output:
C      BSIZE    I      Number words to fit w no sort
C      IERR     I      0 => okay without sort, -1 okay with sort
C                      1 => does not fit in any case, 2 => bad parms
C-----------------------------------------------------------------------
      INTEGER   TYPE, LREC, BSIZE, IERR
      REAL      MXABSU
      DOUBLE PRECISION FREQS(*)
      CHARACTER METHOD*4
C
      INTEGER   NEED(2,2), I, K, L, M, N, NROW, ULIM, NEEDGS, J, APSIZ,
     *   MSGSAV
      DOUBLE PRECISION FMAX, FMIN
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF (FREQUV.LE.0.0D0) GO TO 999
      CALL FILL (4, 0, NEED)
      FMAX = FREQS(1)
      FMIN = FREQS(1)
      DO 10 I = 1,NCHAVG
         FMAX = MAX (FMAX, FREQS(I))
         FMIN = MIN (FMIN, FREQS(I))
 10      CONTINUE
      ULIM = MXABSU * FMAX / FREQUV * ABS(CELLSG(1)) * NXUNF / RAD2AS +
     *   0.9
      APSIZ = PSAPNW * 1024
C                                       Uniform weighting
      IF ((DOUNIF) .AND. ((TYPE.EQ.0) .OR. (TYPE.EQ.1))) THEN
         IF ((NXUNF.LE.1) .OR. (NYUNF.LE.1)) GO TO 999
         N = 1
         IF ((ROBUST.LE.7.) .AND. (ROBUST.GE.-7.)) N = 2
         M = UNFBOX + 1
         I = 20 * LREC + M*M + 20 + NCHAVG
         M = 2 * UNFBOX + 1
         NROW = NXUNF/2 + 1 + UNFBOX
         IF (ULIM.GT.0.0) NROW = MIN (ULIM + M, NROW)
         NEED(1,1) = I + N * NYUNF * NROW
         NROW = MIN (ULIM, NXUNF/2+1)
         NROW = ((FMAX - FMIN) / FMAX) * NROW + 2.99  + M
         NEED(2,1) = I + N * NYUNF * NROW
         END IF
C                                       gridding
C                                       Create Convolving function
      IF ((TYPE.EQ.0) .OR. (TYPE.EQ.2)) THEN
         MSGSAV = MSGSUP
         MSGSUP = MAX (MSGSUP, 31999)
         CALL GRDFLT (CTYPX, CTYPY, XPARM, YPARM)
         MSGSUP = MSGSAV
         M = MAX (XPARM(1), 1.0) * 2 + 1
         N = MAX (YPARM(1), 1.0) * 2 + 1
         K = NXMAX
         L = NYMAX
         J = MIN (2048, (APSIZ/40)/LREC) + 5
         J = MAX (J * LREC, UVBFSS)
C                                       QNGRD requires 2 copies of data
         I = 100 * (M + N + 1) + NCHAVG + 2*J
         IF ((K.LE.1) .OR. (L.LE.1)) GO TO 999
         NROW = K/2
         ULIM = MXABSU * FMAX / FREQUV * ABS(CELLSG(1)) * K / RAD2AS +
     *      2.0
         IF (ULIM.GT.0.0) NROW = MIN (NROW, ULIM)
C                                       add 2 rows for safety
         NEED(1,2) = 2 * L * (NROW + 2 + M + 2) + I
         NROW = ((FMAX - FMIN) / FMAX) * NROW + 2.99 + M
         NEED(2,2) = 2 * L * NROW + I
         END IF
C                                       answer
      IERR = 1
      IF ((NEED(2,1).LT.APSIZ) .AND. (NEED(2,2).LT.APSIZ)) IERR = -1
      IF ((NEED(1,1).LT.APSIZ) .AND. (NEED(1,2).LT.APSIZ)) IERR = 0
      BSIZE = MAX (NEED(1,1), NEED(1,2))
      IF (IERR.EQ.1) THEN
         IF (NEED(2,1).GT.APSIZ) THEN
            WRITE (MSGTXT,1010) 'WEIGHTING', NEED(2,1), APSIZ
            CALL MSGWRT (6)
            END IF
         IF (NEED(2,2).GT.APSIZ) THEN
            WRITE (MSGTXT,1010) 'GRIDDING', NEED(2,2), APSIZ
            CALL MSGWRT (6)
            END IF
C                                       CC gridded subtraction
      ELSE IF ((METHOD.NE.'DFT ') .AND. ((TYPE.EQ.0) .OR. (TYPE.EQ.3)))
     *   THEN
         K = NXMAX
         L = NYMAX
         NROW = K/2 + 12
         J = MIN (2048, (APSIZ/30)/(LREC+4))
         J = MAX (10, J) + 5
         NEEDGS = 4 * L * NROW + 2230 + NCHAVG + J * (LREC+4)
         IF (NEEDGS.GT.APSIZ) IERR = -1
         BSIZE = MAX (BSIZE, NEEDGS)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('FOR ',A,' NEED',I9,' HAVE ONLY',I9,' WORDS IN ''AP''')
      END


