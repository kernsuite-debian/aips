      SUBROUTINE BMSHP (IN, IERR)
C-----------------------------------------------------------------------
C! Tim Cornwell routine to fit an elliptical Gaussian to a dirty beam
C# Modeling Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999, 2002, 2007
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
C   BMSHP fits an eliptical Gaussian to the dirty beam and checks that
C   the peak of the beam is 1.0.  If peak of beam is too narrow to fit a
C   default circular Gaussian is used.  A grid of up to 5 X 11 points is
C   used for the fit; only points within the half power points are used.
C   To avoid degenerate cases some of the allowed points are ignored.
C   Solution is by least squares to a linearized gaussian.
C   Input:  The beam map centered at (NX/2+1,NY/2+1)
C      IN      I   ?
C      CELLX   R   Grid spacing in RA (asec.)
C      CELLY   R   Grid spacing in Dec (asec.)
C   Output:
C      IERR    I   ?
C      BMAJ    R   Major axis size (FWHP in sec).
C      BMIN    R   Minor axis size (FWHP in sec).
C      BPA     R   Position angle of major axis (degrees).
C-----------------------------------------------------------------------
      INTEGER   WIN(4), IN, TRY, I, IFLIP, IJK, ILAST, LBIND, LFIND,
     *   IROW, J, K, L, LUN
      REAL      X(3,3), Y(3), P(3), DX, DY, XFACT, BABS, BDIF
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      MAP = .TRUE.
      EXCL = .FALSE.
      WAIT = .FALSE.
      TRY = 0
      IFLIP = 1
      XFACT = ABS (CELLX)
      LUN = 16
C                                       Open beam file.
      CALL ZOPEN (LUN, LFIND, VMVOL(IN), VMFILE(IN), MAP,
     *   EXCL, WAIT, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
C                                       Zero work arrays.
 10   DO 20 I = 1,3
         Y(I) = 0.0
         DO 20 J = 1,3
            X(I,J) = 0.0
 20         CONTINUE
C                                       Set window,only need half map
      WIN(1) = NX / 2 - 5 + 1
      WIN(2) = NY / 2 + 1
      WIN(3) = NX / 2 + 5 + 1
      WIN(4) = NY / 2 + 5 + 1
      CALL MINIT ('READ', LUN, LFIND, NX, NY, WIN, BUFFR1, BUFSZ(1),
     *   VMBO(IN), IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         GO TO 999
C                                       Loop through rows.
 30   DO 70 I = 1,6
C                                       Read row.
         CALL MDISK ('READ', LUN, LFIND, BUFFR1, LBIND, IERR)
         IF (IERR.EQ.0) GO TO 40
            WRITE (MSGTXT,1030) IERR, I
            CALL MSGWRT (8)
            GO TO 999
C                                       Make sure center is 1.0
 40      IF (I.NE.1) GO TO 41
            BDIF = BUFFR1(LBIND+5) - 1.0
            BABS = ABS (BDIF)
            IF (BABS.GT.0.01) GO TO 980
 41      CONTINUE
C                                       Loop down row doing alternate
C                                       halves. go only to first
C                                       decending 0.35 from center.
         DO 65 IJK = 1,2
            IFLIP = - IFLIP
            ILAST = LBIND + 5 - IFLIP
            DO 60 J = IJK,6
               IROW = LBIND + 5 + (J-1) * IFLIP
               IF ((BUFFR1(IROW).LT.0.35) .AND. (BUFFR1(IROW).LT.
     *            BUFFR1(ILAST))) GO TO 65
               IF (BUFFR1(IROW).LT.0.35) GO TO 60
               ILAST = IROW
C                                       Compute displacements from
C                                       center.
                  DX = IFLIP * (J-1.0) * CELLX / XFACT
                  DY = (1.0-I) * CELLY / XFACT
C                                       Compute partials WRT C1,C2,C3
                  P(1) = DX * DX
                  P(2) = DY * DY
                  P(3) = DX * DY
C                                       Sum partials into X matrix and
C                                       Y vector.
                  DO 50 K = 1,3
                     Y(K) = Y(K) - LOG (BUFFR1(IROW)) * P(K)
                     DO 50 L = 1,3
                        X(K,L) = X(K,L) + P(K) * P(L)
 50                     CONTINUE
 60            CONTINUE
 65         CONTINUE
 70      CONTINUE
C                                       Fit beam
      WRITE (MSGTXT,1070) BMAJ
      CALL MSGWRT (4)
      CALL FITBM (0, X, Y, CELLX, 0.0, BMAJ, BMIN, BPA)
C                                       Close beam file
 220  CALL ZCLOSE (LUN, LFIND, IERR)
      GO TO 999
C                                       Beam center not 1.000.
 980  WRITE (MSGTXT,1980) BUFFR1(LBIND+5)
      CALL MSGWRT (8)
      CALL ZCLOSE (LUN, LFIND, IERR)
      IERR = 8
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BMSHP: ERROR ',I3,' OPENING FILE ')
 1020 FORMAT ('BMSHP: ERROR ',I3,' INIT FILE ')
 1030 FORMAT ('BMSHP: READ ERROR ',I3,' ROW ',I5)
 1070 FORMAT ('BMAJ BEFORE FITBM  ', F10.1)
 1980 FORMAT ('BMSHP ERROR: CENTER OF BEAM = ',E12.5)
      END
