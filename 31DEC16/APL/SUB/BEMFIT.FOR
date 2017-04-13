      SUBROUTINE BEMFIT (IFIELD, JBUFSZ, BUFFR, BEAM, IERR)
C-----------------------------------------------------------------------
C! Fits elliptical Gaussian to beam image
C# Modeling Map
C-----------------------------------------------------------------------
C;  Copyright (C) 2008
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
C   Input:
C      IFIELD   I      Field number
C      JBUFSZ   I      Size of BUFFR in AIPS bytes
C   Output:
C      BUFFR    R(*)   Scratch buffer
C      BEAM     R(3)   Major, minor, PA
C      IERR     I      Error code (disk IO troubles)
C   Uses COMMONs DMPR, DGDS to define cell size, beam size and disk
C   location
C-----------------------------------------------------------------------
      INTEGER   IFIELD, JBUFSZ, IERR
      REAL      BUFFR(*), BEAM(3)
C
      CHARACTER PHNAME*48
      INTEGER   IFLIP, ICENX, ICENY, LUN, FIND, LFIELD, WIN(4), NX, NY,
     *   I, J, BIND, IJK, IROW, ILAST, K, L
      REAL      X(3,3), Y(3), P(3), DX, DY, XFACT, CELLS(2)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DGDS.INC'
C-----------------------------------------------------------------------
      LFIELD = MAX (1, ABS(IFIELD))
      XFACT = ABS (CELLSG(1))
      CELLS(1) = - XFACT
      CELLS(2) = CELLSG(2)
      IFLIP = 1
      NX = NXBEM(LFIELD)
      NY = NYBEM(LFIELD)
      ICENX = NX / 2
      ICENY = NY / 2 + 1
      LUN = 18
C                                       Open beam file.
      CALL ZPHFIL ('MA', BEMVOL(LFIELD), CNOBEM(LFIELD), 1, PHNAME,
     *   IERR)
      CALL ZOPEN (LUN, FIND, BEMVOL(LFIELD), PHNAME, .TRUE., .FALSE.,
     *   .TRUE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Zero work arrays.
      DO 20 I = 1,3
         Y(I) = 0.0
         DO 10 J = 1,3
            X(I,J) = 0.0
 10         CONTINUE
 20      CONTINUE
C                                       Set window,only need half map
      WIN(1) = NX / 2 - 5
      WIN(2) = NY / 2 + 1
      WIN(3) = NX / 2 + 5
      WIN(4) = NY / 2 + 5 + 1
      CALL MINIT ('READ', LUN, FIND, NX, NY, WIN, BUFFR, JBUFSZ,
     *   BOBEM(LFIELD), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INIT'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Loop through rows.
      DO 70 I = 1,6
C                                       Read row.
         CALL MDISK ('READ', LUN, FIND, BUFFR, BIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) IERR, (I + WIN(1) - 1)
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       Loop down row doing alternate
C                                       halves. go only to first
C                                       decending 0.35 from center.
         DO 65 IJK = 1,2
            IFLIP = - IFLIP
            ILAST = BIND + 5 - IFLIP
            DO 60 J = IJK,6
               IROW = BIND + 5 + (J-1) * IFLIP
               IF ((BUFFR(IROW).LT.0.35) .AND. (BUFFR(IROW).LT.
     *            BUFFR(ILAST))) GO TO 65
               IF (BUFFR(IROW).GE.0.35) THEN
                  ILAST = IROW
C                                       Compute displacements from
C                                       center.
                  DX = IFLIP * (J-1.0) * CELLS(1) / XFACT
                  DY = (1.0-I) * CELLS(2) / XFACT
C                                       Compute partials WRT C1,C2,C3
                  P(1) = DX * DX
                  P(2) = DY * DY
                  P(3) = DX * DY
C                                       Sum partials into X matrix and
C                                       Y vector.
                  DO 40 K = 1,3
                     Y(K) = Y(K) - LOG (BUFFR(IROW)) * P(K)
                     DO 30 L = 1,3
                        X(K,L) = X(K,L) + P(K) * P(L)
 30                     CONTINUE
 40                  CONTINUE
                  END IF
 60            CONTINUE
 65         CONTINUE
 70      CONTINUE
C                                       Fit beam
      CALL FITBM (0, X, Y, CELLS, MAPROT, BEAM(1), BEAM(2), BEAM(3))
C                                       Close beam file
      CALL ZCLOSE (LUN, FIND, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BEMFIT: ERROR ',I3,1X,A,'ING FILE ')
 1020 FORMAT ('BEMFIT: READ ERROR ',I3,' ROW ',I5)
      END
