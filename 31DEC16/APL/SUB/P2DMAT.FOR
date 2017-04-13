      SUBROUTINE P2DMAT (RA, DEC, UROTAT, XRA, XDEC, MROTAT, UMAT, PMAT)
C-----------------------------------------------------------------------
C! returns UV projection matrix for gridding in main image plane
C# Map Header coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 2009
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
C   Returns re-projection matrix to convert (u,v,w) to (u',v',w') for
C   shifted gridding in the same geometry as the central facet
C   UMAT(3,1) = -L, UMAT(3,2) = -M, UMAT(3,3) = -1
C   Input:
C      RA       D        Initial RA in degrees
C      DEC      D        Initial Declination in degrees
C      UROTAT   R        UV rotation in degrees
C      XRA      D        RA of shifted point in degrees
C      XDEC     D        Declination of shifted point in degrees
C      MROTAT   R        Rotation in degrees at new tangent point
C   Output
C      UMAT     R(3,3)   uvw -> uvw prime matrix
C      PMAT     R(3,3)   xyz -> xyz prime (in non-3D they are the same)
C-----------------------------------------------------------------------
      DOUBLE PRECISION RA, DEC, XRA, XDEC
      REAL      UROTAT, MROTAT, UMAT(3,3), PMAT(3,3)
C
      INTEGER   I, J, K
      DOUBLE PRECISION RM(3,3), SA, CA, SD, CD, SD0, CD0, T(3,3), R,
     *   X(3,3), LL, MM, DD
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       sin's and cos's
      SA = SIN (DG2RAD * (XRA-RA))
      CA = COS (DG2RAD * (XRA-RA))
      SD = SIN (DG2RAD * XDEC)
      CD = COS (DG2RAD * XDEC)
      SD0 = SIN (DG2RAD * DEC)
      CD0 = COS (DG2RAD * DEC)
      LL = CD * SA
      MM = SD * CD0 - CD * SD0 * CA
      DD = SQRT (1.0D0 - LL*LL - MM*MM)
C                                       rotation matrix
      CALL DFILL (9, 0.0D0, RM)
      RM(3,3) = 1.0
C                                       Map +
      R = DG2RAD * MROTAT
      RM(1,1) = COS (R)
      RM(2,2) = RM(1,1)
      RM(1,2) = -SIN (R)
      RM(2,1) = SIN (R)
C                                       forward matrix
      CALL DFILL (9, 0.0D0, X)
      X(1,1) = 1.0
      X(2,2) = 1.0
      X(3,3) = -1.0
      X(3,1) = -LL / DD
      X(3,2) = -MM / DD
C                                       multiply
      DO 20 I = 1,3
         DO 15 J = 1,3
            T(I,J) = 0.0
            DO 10 K = 1,3
               T(I,J) = T(I,J) + X(I,K) * RM(K,J)
 10            CONTINUE
 15         CONTINUE
 20      CONTINUE
C                                       uv -
      R = DG2RAD * UROTAT
      RM(1,1) = COS (R)
      RM(2,2) = RM(1,1)
      RM(1,2) = SIN (R)
      RM(2,1) = -SIN (R)
C                                       multiply
      DO 40 I = 1,3
         DO 35 J = 1,3
            R = 0.0
            DO 30 K = 1,3
               R = R + RM(I,K) * T(K,J)
 30            CONTINUE
            UMAT(I,J) = R
 35         CONTINUE
 40      CONTINUE
C                                       uv +
      R = DG2RAD * UROTAT
      RM(1,1) = COS (R)
      RM(2,2) = RM(1,1)
      RM(1,2) = -SIN (R)
      RM(2,1) = SIN (R)
C                                       x,y,x unchanged by PMAT
      CALL DFILL (9, 0.0D0, X)
      X(1,1) = 1.0
      X(2,2) = 1.0
      X(3,3) = 1.0
C                                       multiply
      DO 60 I = 1,3
         DO 55 J = 1,3
            T(I,J) = 0.0
            DO 50 K = 1,3
               T(I,J) = T(I,J) + X(I,K) * RM(K,J)
 50            CONTINUE
 55         CONTINUE
 60      CONTINUE
C                                       Map -
      R = DG2RAD * MROTAT
      RM(1,1) = COS (R)
      RM(2,2) = RM(1,1)
      RM(1,2) = SIN (R)
      RM(2,1) = -SIN (R)
C                                       multiply
      DO 80 I = 1,3
         DO 75 J = 1,3
            R = 0.0
            DO 70 K = 1,3
               R = R + RM(I,K) * T(K,J)
 70            CONTINUE
            PMAT(I,J) = R
 75         CONTINUE
 80      CONTINUE
C
 999  RETURN
      END
