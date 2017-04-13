C    Utility module for interpolating Images
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran utility module for interpolation.
C# MAP-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2005-2007, 2009, 2012-2014
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
C   This is a  package of image interpolation routines.  The top level
C   routines are IMGINT, IMGIAE and IMGHGE which interpolate one image
C   to another.  Unlike the *GEOM tasks these routines will interpolate
C   over blanked pixels and can fill in small blanked regions.  At the
C   edges of the image being interpolated the interpolation kernal is
C   adjusted to reduce edge effects.
C
C   Public functions:
C   IMGINT (in, shift, rotate, hwidth, out, ierr)
C      Interpolate in to out with shift and rotate.
C   IMGHGE (in, hwidth, xb, xe, out, ierr)
C      Interpolate in to out with full geometry calculation and optional
C      W-corrections and radial scaling.
C   IMGSKF (in, hwidth, xb, xe, cd, out, ierr)
C      Interpolate in to out with partial coordinate calculations to
c      "intermediate world coordinates".  The purpose is to undo skew.
C   IMGIAE (in, parang, el, hwidth, out, ierr)
C      Interpolate an az-el image at the positions of the output image.
C   INTWIN (in, hwidth, out, blc, trc, ierr)
C      Finds the window in the output affected by the input for full
C      header-based interpolations.
C
C   Private functions:
C   IMINST (in, plane, hwidth, ierr)
C      Set up for interpolation, read input plane.
C   IMINTR (x, y, val)
C      Returns interpolation value at coordinate (x,y)
C   IMINTP (xpix, ypix, val)
C      Returns interpolation value at pixel (xpix,ypix)
C   IMINFN (pos, hwidth, int, cen)
C      Return interpolation function
C-----------------------------------------------------------------------
LOCAL INCLUDE 'ITRSTUF.INC'
C                                       Local include for QINTER
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   NXAP, NYAP, IXCEN, IYCEN, IWID, IHALF, BLCI(7), TRCI(7),
     *   JERR, IRELY
      LONGINT   PTIMAG
      LOGICAL   APOPEN
      REAL      ICDELT(7), ICRPIX(7), XINT(10), YINT(10), DENOM(10),
     *   INIMAG(2), OUTROW(MABFSS)
      COMMON /ITRLCM/ OUTROW, PTIMAG, INIMAG, ICDELT, ICRPIX, XINT,
     *   YINT, DENOM, NXAP, NYAP, IXCEN, IYCEN, IWID, IHALF, BLCI, TRCI,
     *   JERR, APOPEN, IRELY
LOCAL END
      SUBROUTINE IMGINT (IN, SHIFT, ROTATE, HWIDTH, OUT, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Shift, rotate and interpolate image IN at positions in OUT.
C   This routine assumes that both images have Euclidian geometries
C   (i.e. ignored projective geometries). Positions used are offsets
C   from the reference pixel rather than from the coordinate reference
C   value.  Shift is applied before rotation.
C      Interpolation uses Lagrange's formula and will attempt to
C   interpolate blanked regions if they are not too large.  Regions in
C   OUT not included in IN are blanked.
C   Inputs:
C      IN      C*?  Input image (real only)
C      SHIFT   R(2) Shift to be applied to IN in arcseconds. (1/3600 of
C                   axis increment units).
C                   Pos. moves an object to appear at higher coordinate
C                   values.
C      ROTATE  R    Rotation of IN in deg, from N thru E.
C      HWIDTH  I    Half width in pixels in interpolation kernal (1-4)
C   Input/Output:
C      OUT     C*?  Output image.  Geometry used on input.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      REAL      SHIFT(2), ROTATE
      INTEGER   HWIDTH, IERR
C
      INTEGER   TYPE, DIM(7), NAXIS(7), BLC(7), TRC(7),  PLANE(7),
     *   I1, I2, I3, I4, I5, I6, I7, LROW, DUMMY
      REAL      X, Y, XR, YR, CR, SR, CRPIX(7), CDELT(7), SHD(2)
      LOGICAL   INOUT
      CHARACTER TIN*32, ACCESS*8, CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'ITRSTUF.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      APOPEN = .FALSE.
C                                       Are input and output the same?
      INOUT = IN.EQ.OUT
C                                       Temporary input
      IF (INOUT) THEN
         TIN = 'Temporary object for IMGINT'
         CALL IMGCOP (IN, TIN, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         TIN = IN
         END IF
C                                       Access output by row
      ACCESS = 'ROW'
      DIM(1) = LEN(ACCESS)
      DIM(2) = 1
      CALL ARPPUT (OUT, 'ACCESS', OOACAR, DIM, DUMMY, ACCESS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open output
      CALL IMGOPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Window
      CALL ARRWIN (OUT, BLC, TRC, NAXIS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Geometry
      CALL IMDGET (OUT, 'CDELT', TYPE, DIM, CDELT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMDGET (OUT, 'CRPIX', TYPE, DIM, CRPIX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (ABS (CDELT(1)).LT.1.0E-12) CDELT(1) = 1.0
      IF (ABS (CDELT(2)).LT.1.0E-12) CDELT(2) = 1.0
C                                       Shift to deg and negate
      SHD(1) = -SHIFT(1) / 3600.0
      SHD(2) = -SHIFT(2) / 3600.0
C                                       Rotation
      CR = COS (DG2RAD * ROTATE)
      SR = SIN (DG2RAD * ROTATE)
C                                       Degree of desperation
C                                       Loop over array
      LROW = TRC(1) - BLC(1) + 1
      DO 700 I7 = BLC(7),TRC(7)
         PLANE(7) = I7
         DO 600 I6 = BLC(6),TRC(6)
            PLANE(6) = I6
            DO 500 I5 = BLC(5),TRC(5)
               PLANE(5) = I5
               DO 400 I4 = BLC(4),TRC(4)
                  PLANE(4) = I4
                  DO 300 I3 = BLC(3),TRC(3)
                     PLANE(3) = I3
C                                       Read input plane to memory
      CALL IMINST (IN, PLANE, HWIDTH, IERR)
      IF (IERR.NE.0) GO TO 990
      DO 200 I2 = BLC(2),TRC(2)
         Y = (I2 - CRPIX(2)) * CDELT(2) + SHD(2)
         DO 100 I1 = 1,LROW
C                                       Determine position in IN
            X = (I1 + BLC(1)-1 - CRPIX(1)) * CDELT(1) + SHD(1)
            XR = X * CR - Y * SR
            YR = X * SR + Y * CR
            CALL IMINTR (XR, YR, OUTROW(I1))
 100        CONTINUE
C                                       Write
         DIM(1) = LROW
         CALL ARRWRI (OUT, DIM, OUTROW, IERR)
         IF (IERR.NE.0) GO TO 990
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close output array: leave image
C                                       open for more header adjustments
      CALL ARRCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Delete temporary object
      IF (INOUT) THEN
         CALL IMGDES (TIN, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      IF (APOPEN) CALL ZMEMRY ('FREE', 'INTERP', I1, INIMAG, PTIMAG,
     *   JERR)
      GO TO 999
C                                       Error
 990  IF (APOPEN) CALL ZMEMRY ('FREE', 'INTERP', I1, INIMAG, PTIMAG,
     *   JERR)
      MSGTXT = 'ERROR INTERPOLATING ' // IN
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE IMGHGE (IN, HWIDTH, XB, XE, OUT, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Interpolate image IN at positions in OUT with full geometric
C   calculations.  Optional corrections can be made for simple
C   W-distortions, i.e. a coplanar array not normal to the zenith.
C      Optional corrections can also be made for a radial, nonlinear
C   distortion of the input image geometry due to primary beam
C   variations across the averaged bandpass.
C      Interpolation uses Lagrange's formula and will attempt to
C   interpolate blanked regions if they are not too large.  Regions in
C   OUT not included in IN are blanked.
C   Inputs:
C      IN      C*?   Input image (real only)
C      HWIDTH  I     Half width in pixels in interpolation kernal (1-4)
C      XB      I     First pixel of output image to compute
C      XE      I     Last X pixel of output to compute
C   Inputs attached to IN:
C      DO3DCOR  L    If present and true correct for noncoplanarity,
C                    requires PARANGLE and ZENANGLE
C      PARANGLE R    Parallactic angle of observation in degrees
C      ZENANGLE R    Zenith angle of observations in degrees.
C      PBFWHM   R    Primary beam FWHM at nomimal frequency (deg)
C                    If absent or zero then no correction is done.
C      FBWSQ    R    Square of fractional bandpass.
C      C123     R(4) Primary beam shape parameters, C0, C1, C2, C3
C   Input/Output:
C      OUT     C*?  Output image.  Geometry used on input.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      INTEGER   HWIDTH, XB, XE, IERR
C
      INTEGER   TYPE, DIM(7), NAXIS(7), BLC(7), TRC(7),  PLANE(7),
     *   I1, I2, I3, I4, I5, I6, I7, LROW, MSGSAV, DUMMY
      REAL      XYZI(7), XYZO(7), PA, ZA, TZA, SPA, CPA, PBFWHM, FBWSQ,
     *   C123(4)
      LOGICAL   DO3D, DOFULL, DOSCAL
      CHARACTER ACCESS*8, CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'ITRSTUF.INC'
C-----------------------------------------------------------------------
      APOPEN = .FALSE.
C                                       3-D correction?
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL IMGET (IN, 'DO3DCOR', TYPE, DIM, DO3D, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         DO3D = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 985
      IF (DO3D) THEN
         CALL IMGET (IN, 'PARANGLE', TYPE, DIM, PA, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL IMGET (IN, 'ZENANGLE', TYPE, DIM, ZA, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         PA = 0.0
         ZA = 0.0
         END IF
      TZA = TAN (ZA / 57.29577951)
      CPA = COS (PA / 57.29577951)
      SPA = SIN (PA / 57.29577951)
C                                       Radial and/or linear scaling?
      MSGSUP = 32000
      CALL IMGET (IN, 'PBFWHM', TYPE, DIM, PBFWHM, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         PBFWHM = 0.0
         END IF
      IF (IERR.NE.0) GO TO 985
      MSGSUP = 32000
      CALL IMGET (IN, 'C123', TYPE, DIM, C123, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         CALL FILL (4, 0.0, C123)
         END IF
      IF (IERR.NE.0) GO TO 985
      DOSCAL = ((C123(1).GT.0.0) .AND. (C123(1).NE.1.0)) .OR.
     *   (PBFWHM.GT.0.0)
      IF (PBFWHM.GT.0.0) THEN
         CALL IMGET (IN, 'FBWSQ', TYPE, DIM, FBWSQ, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         FBWSQ = 0.0
         CALL RFILL (3, 0.0, C123(2))
         END IF
      DOFULL = DO3D .OR. DOSCAL
C                                       Access output by row
      ACCESS = 'ROW'
      DIM(1) = LEN (ACCESS)
      DIM(2) = 1
      CALL ARPPUT (OUT, 'ACCESS', OOACAR, DIM, DUMMY, ACCESS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open output
      CALL IMGOPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Window
      CALL ARRWIN (OUT, BLC, TRC, NAXIS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       NOTE: BLC/TRC is window
C                                       into output - almost
C                                       always the full image
C                                       Loop over array
      LROW = TRC(1) - BLC(1) + 1
      DO 700 I7 = BLC(7),TRC(7)
         PLANE(7) = I7
         DO 600 I6 = BLC(6),TRC(6)
            PLANE(6) = I6
            DO 500 I5 = BLC(5),TRC(5)
               PLANE(5) = I5
               DO 400 I4 = BLC(4),TRC(4)
                  PLANE(4) = I4
                  DO 300 I3 = BLC(3),TRC(3)
                     PLANE(3) = I3
                     DO 90 I1 = 3,7
                        XYZI(I1) = PLANE(I1)
                        XYZO(I1) = PLANE(I1)
 90                     CONTINUE
C                                       Read input plane to memory
      CALL IMINST (IN, PLANE, HWIDTH, IERR)
      IF (IERR.NE.0) GO TO 990
      DO 200 I2 = BLC(2),TRC(2)
         XYZO(2) = I2
         CALL RFILL (LROW, FBLANK, OUTROW)
C                                       3-D or scaling correction
         IF (DOFULL) THEN
            DO 100 I1 = XB,XE
C                                       Determine position
               XYZO(1) = I1 + BLC(1) - 1
               MSGSUP = 32000
               CALL PSNCV3 (OUT, XYZO, IN, .TRUE., TZA, CPA, SPA,
     *            PBFWHM, FBWSQ, C123, XYZI, IERR)
               MSGSUP = MSGSAV
               IF (IERR.EQ.0) CALL IMINTP (XYZI(1), XYZI(2), OUTROW(I1))
 100        CONTINUE
C                                       Geometry only
         ELSE
            DO 110 I1 = XB,XE
C                                       Determine position
               MSGSUP = 32000
               XYZO(1) = I1 + BLC(1) - 1
               CALL PSNCVT (OUT, XYZO, IN, XYZI, IERR)
               MSGSUP = MSGSAV
               IF (IERR.EQ.0) CALL IMINTP (XYZI(1), XYZI(2), OUTROW(I1))
 110        CONTINUE
            END IF
C                                       Write
         DIM(1) = LROW
         CALL ARRWRI (OUT, DIM, OUTROW, IERR)
         IF (IERR.NE.0) GO TO 990
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close output array: leave image
C                                       open for more header adjustments
      CALL ARRCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (APOPEN) CALL ZMEMRY ('FREE', 'INTERP', I1, INIMAG, PTIMAG,
     *   JERR)
      GO TO 999
C                                       Error
 985  CALL MSGWRT (8)
 990  IF (APOPEN) CALL ZMEMRY ('FREE', 'INTERP', I1, INIMAG, PTIMAG,
     *   JERR)
      MSGTXT = 'ERROR INTERPOLATING ' // IN
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE IMGSKF (IN, HWIDTH, XB, XE, CD, OUT, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Interpolate image IN at positions in OUT with partial coordinate
C   calculations to "intermediate world coordinates".  The purpose is
C   to apply a 2x2 matrix to the input which includes skew and to
C   make an output which is a pure rotation with no skew.
C      Interpolation uses Lagrange's formula and will attempt to
C   interpolate blanked regions if they are not too large.  Regions in
C   OUT not included in IN are blanked.
C   Inputs:
C      IN      C*?   Input image (real only)
C      HWIDTH  I     Half width in pixels in interpolation kernal (1-4)
C      XB      I     First pixel of output image to compute
C      XE      I     Last X pixel of output to compute
C      CD      D(2,2)   rotation matrix appropriate to the IN data
C   Input/Output:
C      OUT     C*?  Output image.  Geometry used on input.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      INTEGER   HWIDTH, XB, XE, IERR
      DOUBLE PRECISION CD(2,2)
C
      INTEGER   TYPE, DIM(7), NAXIS(7), BLC(7), TRC(7),  PLANE(7),
     *   I1, I2, I3, I4, I5, I6, I7, LROW, DUMMY
      REAL      XYZI(7), XYZO(7), CRPIXI(7), CRPIXO(7), CDELTO(7),
     *   CROTAO(7)
      CHARACTER ACCESS*8, CDUMMY*1
      DOUBLE PRECISION CDT(2,2), OCD(2,2), FULL(2,2), DET
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'ITRSTUF.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      APOPEN = .FALSE.
C                                       Access output by row
      ACCESS = 'ROW'
      DIM(1) = LEN (ACCESS)
      DIM(2) = 1
      CALL ARPPUT (OUT, 'ACCESS', OOACAR, DIM, DUMMY, ACCESS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open output
      CALL IMGOPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Coordinate info
C                                       Reference pixel
      CALL OGET (IN, 'IMAGE_DESC.CRPIX', TYPE, DIM, CRPIXI, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (OUT, 'IMAGE_DESC.CRPIX', TYPE, DIM, CRPIXO, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Rotation
      CALL OGET (OUT, 'IMAGE_DESC.CROTA', TYPE, DIM, CROTAO, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Increment
      CALL OGET (OUT, 'IMAGE_DESC.CDELT', TYPE, DIM, CDELTO, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      I1 = 2
      IF (CROTAO(1).NE.0.0) I1 = 1
C                                       from WCSII p1101 (25)
      OCD(1,1) = CDELTO(1) * COS (DG2RAD * CROTAO(I1))
      OCD(1,2) = -CDELTO(2) * SIN (DG2RAD * CROTAO(I1))
      OCD(2,1) = CDELTO(1) * SIN (DG2RAD * CROTAO(I1))
      OCD(2,2) = CDELTO(2) * COS (DG2RAD * CROTAO(I1))
C                                       invert CD
      DET = CD(1,1) * CD(2,2) - CD(1,2) * CD(2,1)
      CDT(1,1) = CD(2,2) / DET
      CDT(1,2) = -CD(1,2) / DET
      CDT(2,1) = -CD(2,1) / DET
      CDT(2,2) = CD(1,1) / DET
C                                       multiply
      FULL(1,1) = CDT(1,1) * OCD(1,1) + CDT(1,2) * OCD(2,1)
      FULL(1,2) = CDT(1,1) * OCD(1,2) + CDT(1,2) * OCD(2,2)
      FULL(2,1) = CDT(2,1) * OCD(1,1) + CDT(2,2) * OCD(2,1)
      FULL(2,2) = CDT(2,1) * OCD(1,2) + CDT(2,2) * OCD(2,2)
C                                       Window
C                                       NOTE: BLC/TRC is window
C                                       into output - almost
C                                       always the full image
      CALL ARRWIN (OUT, BLC, TRC, NAXIS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Loop over array
      LROW = TRC(1) - BLC(1) + 1
      DO 700 I7 = BLC(7),TRC(7)
         PLANE(7) = I7
         DO 600 I6 = BLC(6),TRC(6)
            PLANE(6) = I6
            DO 500 I5 = BLC(5),TRC(5)
               PLANE(5) = I5
               DO 400 I4 = BLC(4),TRC(4)
                  PLANE(4) = I4
                  DO 300 I3 = BLC(3),TRC(3)
                     PLANE(3) = I3
                     DO 90 I1 = 3,7
                        XYZI(I1) = PLANE(I1)
                        XYZO(I1) = PLANE(I1)
 90                     CONTINUE
C                                       Read input plane to memory
      CALL IMINST (IN, PLANE, HWIDTH, IERR)
      IF (IERR.NE.0) GO TO 990
      DO 200 I2 = BLC(2),TRC(2)
         XYZO(2) = I2 - CRPIXO(2)
         CALL RFILL (LROW, FBLANK, OUTROW)
         DO 110 I1 = XB,XE
C                                       Determine position
            XYZO(1) = I1 + BLC(1) - 1 - CRPIXO(1)
            XYZI(1) = FULL(1,1) * XYZO(1) + FULL(1,2) * XYZO(2) +
     *         CRPIXI(1)
            XYZI(2) = FULL(2,1) * XYZO(1) + FULL(2,2) * XYZO(2) +
     *         CRPIXI(2)
            CALL IMINTP (XYZI(1), XYZI(2), OUTROW(I1))
 110        CONTINUE
C                                       Write
         DIM(1) = LROW
         CALL ARRWRI (OUT, DIM, OUTROW, IERR)
         IF (IERR.NE.0) GO TO 990
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close output array: leave image
C                                       open for more header adjustments
      CALL ARRCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (APOPEN) CALL ZMEMRY ('FREE', 'INTERP', I1, INIMAG, PTIMAG,
     *   JERR)
      GO TO 999
C                                       Error
 985  CALL MSGWRT (8)
 990  IF (APOPEN) CALL ZMEMRY ('FREE', 'INTERP', I1, INIMAG, PTIMAG,
     *   JERR)
      MSGTXT = 'ERROR INTERPOLATING ' // IN
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE IMGIAE (IN, PARANG, EL, HWIDTH, OUT, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Interpolate positions in a projected azimuth-elevation image, IN, at
C   positions in RA-dec image OUT with observing geometry given by
C   parallactic angle PARANG adn elevation EL.  This function is useful
C   for calibration of interferometer images from primary beam
C   calibration.
C      Interpolation uses Lagrange's formula and will attempt to
C   interpolate blanked regions if they are not too large.  Regions in
C   OUT not included in IN are blanked.
C   Inputs:
C      IN      C*?  Input image (real only)
C      PARANG  R    Parallactic angle of observation in degrees.
C      EL      R    Elevation of observation in degrees.
C      HWIDTH  I    Half width in pixels in interpolation kernal (1-4)
C   Input/Output:
C      OUT     C*?  Output image.  Geometry used on input.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      REAL      PARANG, EL
      INTEGER   HWIDTH, IERR
C
      INTEGER   TYPE, DIM(7), NAXIS(7), BLC(7), TRC(7),  PLANE(7),
     *   I1, I2, I3, I4, I5, I6, I7, LROW, DUMMY
      REAL      X, Y, XR, YR, CR, SR, CRPIX(7), CDELT(7)
      LOGICAL   INOUT
      CHARACTER TIN*32, ACCESS*8, CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'ITRSTUF.INC'
C-----------------------------------------------------------------------
      APOPEN = .FALSE.
C                                       Are input and output the same?
      INOUT = IN.EQ.OUT
C                                       Temporary input
      IF (INOUT) THEN
         TIN = 'Temporary object for IMGINT'
         CALL IMGCOP (IN, TIN, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
            TIN = IN
            END IF
C                                       Access output by row
      ACCESS = 'ROW'
      DIM(1) = LEN(ACCESS)
      DIM(2) = 1
      CALL ARPPUT (OUT, 'ACCESS', OOACAR, DIM, DUMMY, ACCESS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open output
      CALL IMGOPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Window
      CALL ARRWIN (OUT, BLC, TRC, NAXIS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Geometry
      CALL IMDGET (OUT, 'CDELT', TYPE, DIM, CDELT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMDGET (OUT, 'CRPIX', TYPE, DIM, CRPIX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (ABS (CDELT(1)).LT.1.0E-12) CDELT(1) = 1.0
      IF (ABS (CDELT(2)).LT.1.0E-12) CDELT(2) = 1.0
C                                       Rotation
      CR = COS (PARANG / 57.29577951)
      SR = -SIN (PARANG / 57.29577951)
C***??? need test to decide if fast of slow calculation
C                                       Loop over array
      LROW = TRC(1) - BLC(1) + 1
      DO 700 I7 = BLC(7),TRC(7)
         PLANE(7) = I7
         DO 600 I6 = BLC(6),TRC(6)
            PLANE(6) = I6
            DO 500 I5 = BLC(5),TRC(5)
               PLANE(5) = I5
               DO 400 I4 = BLC(4),TRC(4)
                  PLANE(4) = I4
                  DO 300 I3 = BLC(3),TRC(3)
                     PLANE(3) = I3
C                                       Read input plane to memory
      CALL IMINST (IN, PLANE, HWIDTH, IERR)
      IF (IERR.NE.0) GO TO 990
      DO 200 I2 = BLC(2),TRC(2)
         Y = (I2 - CRPIX(2)) * CDELT(2)
         DO 100 I1 = 1,LROW
C                                       Determine position in IN
            X = (I1 + BLC(1)-1 - CRPIX(1)) * CDELT(1)
C                                       Mike Kestevan's transformation
            XR = -X * CR - Y * SR
            YR = -X * SR + Y * CR
            CALL IMINTR (XR, YR, OUTROW(I1))
 100        CONTINUE
C                                       Write
         DIM(1) = LROW
         CALL ARRWRI (OUT, DIM, OUTROW, IERR)
         IF (IERR.NE.0) GO TO 990
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close output array: leave image
C                                       open for more header adjustments
      CALL ARRCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Delete temporary object
      IF (INOUT) THEN
         CALL IMGDES (TIN, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      IF (APOPEN) CALL ZMEMRY ('FREE', 'INTERP', I1, INIMAG, PTIMAG,
     *   JERR)
      GO TO 999
C                                       Error
 990  IF (APOPEN) CALL ZMEMRY ('FREE', 'INTERP', I1, INIMAG, PTIMAG,
     *   JERR)
      MSGTXT = 'ERROR INTERPOLATING ' // IN
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE INTWIN (IN, HWIDTH, OUT, BLC, TRC, INBLC, INTRC, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Finds the window in the output affected by the input for full
C   header-based interpolations.
C   Inputs:
C      IN      C*?  Input image (real only)
C      HWIDTH  I    Half width in pixels in interpolation kernal (1-4)
C      OUT     C*?  Output image
C   Inputs attached to IN:
C      DO3DCOR  L    If present and true correct for noncoplanarity,
C                    requires PARANGLE and ZENANGLE
C      PARANGLE R    Parallactic angle of observation in degrees
C      ZENANGLE R    Zenith angle of observations in degrees.
C      PBFWHM   R    Primary beam FWHM at nomimal frequency (deg)
C                    If absent or zero then no correction is done.
C      FBWSQ    R    Square of fractional bandpass.
C      C123     R(4) Primary beam shape parameters, C0, C1, C2, C3
C   Input/Output:
C      BLC      I(7)  BLC of out affected by in (1,2 changed on output)
C      TRC      I(7)  TRC of out affected by in (1,2 changed on output)
C      INBLC    I(7)  BLC of in affected by in (1,2 changed on output)
C      INTRC    I(7)  TRC of in affected by in (1,2 changed on output)
C   Output:
C      IERR    I    Error code: 0 => ok, -1 => no window.(all outside),
C                      -2 no window due to geometry errors
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      INTEGER   HWIDTH, BLC(7), TRC(7), INBLC(7), INTRC(7), IERR
C
      INTEGER   TYPE, DIM(7), NAXIS(7),  PLANE(7), NAXI(7), I1, I2,
     *   LROW, MSGSAV, BLCIN(7), TRCIN(7), IB(2), IT(2), NGOOD, KERR
      REAL      XYZI(7), XYZO(7), PA, ZA, TZA, SPA, CPA, PBFWHM, FBWSQ,
     *   C123(4), XYZOI(7,4)
      LOGICAL   DO3D, DOFULL, DOSCAL, INSIDE
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'ITRSTUF.INC'
C-----------------------------------------------------------------------
C                                       3-D correction?
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL IMGET (IN, 'DO3DCOR', TYPE, DIM, DO3D, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         DO3D = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 985
      IF (DO3D) THEN
         CALL IMGET (IN, 'PARANGLE', TYPE, DIM, PA, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL IMGET (IN, 'ZENANGLE', TYPE, DIM, ZA, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         MSGTXT = 'INTWIN WARNING: DOING 3D CORRECTIONS'
         CALL MSGWRT (8)
      ELSE
         PA = 0.0
         ZA = 0.0
         END IF
      TZA = TAN (ZA / 57.29577951)
      CPA = COS (PA / 57.29577951)
      SPA = SIN (PA / 57.29577951)
C                                       Radial and/or linear scaling?
      MSGSUP = 32000
      CALL IMGET (IN, 'PBFWHM', TYPE, DIM, PBFWHM, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         PBFWHM = 0.0
         END IF
      IF (IERR.NE.0) GO TO 985
      MSGSUP = 32000
      CALL IMGET (IN, 'C123', TYPE, DIM, C123, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         CALL FILL (4, 0.0, C123)
         END IF
      IF (IERR.NE.0) GO TO 985
      DOSCAL = ((C123(1).GT.0.0) .AND. (C123(1).NE.1.0)) .OR.
     *   (PBFWHM.GT.0.0)
      IF (PBFWHM.GT.0.0) THEN
         CALL IMGET (IN, 'FBWSQ', TYPE, DIM, FBWSQ, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE
         FBWSQ = 0.0
         CALL RFILL (3, 0.0, C123(2))
         END IF
      DOFULL = DO3D .OR. DOSCAL
C                                       size of input
      CALL ARRWIN (IN, BLCIN, TRCIN, NAXI, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       size of output
      CALL OGET (OUT, 'ARRAY.ARRAY_DESC.NAXIS', TYPE, DIM, NAXIS,
     *   CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Loop over array
      LROW = NAXI(1)
      DO 90 I1 = 3,7
         PLANE(I1) = (NAXI(I1) + 1) / 2
         XYZI(I1) = PLANE(I1)
         XYZO(I1) = PLANE(I1)
 90      CONTINUE
      BLC(1) = NAXIS(1)
      BLC(2) = NAXIS(2)
      TRC(1) = 1
      TRC(2) = 1
C                                       3 outer corners
      NGOOD = 0
      INSIDE = .FALSE.
      XYZI(2) = BLCIN(2)
      XYZI(1) = TRCIN(1)
      MSGSUP = 32000
      IF (DOFULL) THEN
         CALL PSNCV3 (IN, XYZI, OUT, .TRUE., TZA, CPA, SPA, PBFWHM,
     *      FBWSQ, C123, XYZO, IERR)
      ELSE
         CALL PSNCVT (IN, XYZI, OUT, XYZO, IERR)
         END IF
      MSGSUP = MSGSAV
      IF (IERR.EQ.0) THEN
         NGOOD = NGOOD + 1
         IF ((XYZO(1).GE.1) .AND. (XYZO(1).LE.NAXIS(1)) .AND.
     *      (XYZO(2).GE.1) .AND. (XYZO(2).LE.NAXIS(2))) INSIDE = .TRUE.
         IB(1) = XYZO(1)
         IB(2) = XYZO(2)
         IT(1) = XYZO(1) + 0.9999
         IT(2) = XYZO(2) + 0.9999
         BLC(1) = MIN (BLC(1), IB(1))
         BLC(2) = MIN (BLC(2), IB(2))
         TRC(1) = MAX (TRC(1), IT(1))
         TRC(2) = MAX (TRC(2), IT(2))
         END IF
      XYZI(2) = TRCIN(2)
      XYZI(1) = TRCIN(1)
      MSGSUP = 32000
      IF (DOFULL) THEN
         CALL PSNCV3 (IN, XYZI, OUT, .TRUE., TZA, CPA, SPA, PBFWHM,
     *      FBWSQ, C123, XYZO, IERR)
      ELSE
         CALL PSNCVT (IN, XYZI, OUT, XYZO, IERR)
         END IF
      MSGSUP = MSGSAV
      IF (IERR.EQ.0) THEN
         NGOOD = NGOOD + 1
         IF ((XYZO(1).GE.1) .AND. (XYZO(1).LE.NAXIS(1)) .AND.
     *      (XYZO(2).GE.1) .AND. (XYZO(2).LE.NAXIS(2))) INSIDE = .TRUE.
         IB(1) = XYZO(1)
         IB(2) = XYZO(2)
         IT(1) = XYZO(1) + 0.9999
         IT(2) = XYZO(2) + 0.9999
         BLC(1) = MIN (BLC(1), IB(1))
         BLC(2) = MIN (BLC(2), IB(2))
         TRC(1) = MAX (TRC(1), IT(1))
         TRC(2) = MAX (TRC(2), IT(2))
         END IF
      XYZI(2) = TRCIN(2)
      XYZI(1) = BLCIN(1)
      MSGSUP = 32000
      IF (DOFULL) THEN
         CALL PSNCV3 (IN, XYZI, OUT, .TRUE., TZA, CPA, SPA, PBFWHM,
     *      FBWSQ, C123, XYZO, IERR)
      ELSE
         CALL PSNCVT (IN, XYZI, OUT, XYZO, IERR)
         END IF
      MSGSUP = MSGSAV
      IF (IERR.EQ.0) THEN
         NGOOD = NGOOD + 1
         IF ((IB(1).GE.1) .AND. (IB(1).LE.NAXIS(1)) .AND. (IB(2).GE.1)
     *      .AND. (IB(2).LE.NAXIS(2))) INSIDE = .TRUE.
         IF ((XYZO(1).GE.1) .AND. (XYZO(1).LE.NAXIS(1)) .AND.
     *      (XYZO(2).GE.1) .AND. (XYZO(2).LE.NAXIS(2))) INSIDE = .TRUE.
         IB(1) = XYZO(1)
         IB(2) = XYZO(2)
         IT(1) = XYZO(1) + 0.9999
         IT(2) = XYZO(2) + 0.9999
         BLC(1) = MIN (BLC(1), IB(1))
         BLC(2) = MIN (BLC(2), IB(2))
         TRC(1) = MAX (TRC(1), IT(1))
         TRC(2) = MAX (TRC(2), IT(2))
         END IF
      DO 200 I2 = BLCIN(2),TRCIN(2),5
         XYZI(2) = I2
         DO 100 I1 = BLCIN(1),TRCIN(1),5
            XYZI(1) = I1

C                                       3-D or scaling correction
            MSGSUP = 32000
            IF (DOFULL) THEN
               CALL PSNCV3 (IN, XYZI, OUT, .TRUE., TZA, CPA, SPA,
     *            PBFWHM, FBWSQ, C123, XYZO, IERR)
            ELSE
               CALL PSNCVT (IN, XYZI, OUT, XYZO, IERR)
               END IF
            MSGSUP = MSGSAV
            IF (IERR.EQ.0) THEN
               NGOOD = NGOOD + 1
               IF ((XYZO(1).GE.1) .AND. (XYZO(1).LE.NAXIS(1)) .AND.
     *            (XYZO(2).GE.1) .AND. (XYZO(2).LE.NAXIS(2)))
     *            INSIDE = .TRUE.
               IB(1) = XYZO(1)
               IB(2) = XYZO(2)
               IT(1) = XYZO(1) + 0.9999
               IT(2) = XYZO(2) + 0.9999
               BLC(1) = MIN (BLC(1), IB(1))
               BLC(2) = MIN (BLC(2), IB(2))
               TRC(1) = MAX (TRC(1), IT(1))
               TRC(2) = MAX (TRC(2), IT(2))
               END IF
 100        CONTINUE
 200     CONTINUE
      IERR = 0
      IF (.NOT.INSIDE) IERR = -1
      IF (NGOOD.LE.0) IERR = -2
      BLC(1) = MAX (BLC(1)-HWIDTH, 1)
      BLC(2) = MAX (BLC(2)-HWIDTH, 1)
      TRC(1) = MIN (TRC(1)+HWIDTH, NAXIS(1))
      TRC(2) = MIN (TRC(2)+HWIDTH, NAXIS(2))
C                                       input corners
      XYZO(1) = BLC(1)
      XYZO(2) = BLC(2)
      CALL PSNCVT (OUT, XYZO, IN, XYZOI(1,1), KERR)
      XYZO(1) = BLC(1)
      XYZO(2) = TRC(2)
      CALL PSNCVT (OUT, XYZO, IN, XYZOI(1,2), KERR)
      XYZO(1) = TRC(1)
      XYZO(2) = TRC(2)
      CALL PSNCVT (OUT, XYZO, IN, XYZOI(1,3), KERR)
      XYZO(1) = TRC(1)
      XYZO(2) = BLC(2)
      CALL PSNCVT (OUT, XYZO, IN, XYZOI(1,4), KERR)
      IB(1) = MIN (XYZOI(1,1), XYZOI(1,2), XYZOI(1,3), XYZOI(1,4))
      IB(2) = MIN (XYZOI(2,1), XYZOI(2,2), XYZOI(2,3), XYZOI(2,4))
      IT(1) = MAX (XYZOI(1,1), XYZOI(1,2), XYZOI(1,3), XYZOI(1,4)) +
     *   0.999
      IT(2) = MAX (XYZOI(2,1), XYZOI(2,2), XYZOI(2,3), XYZOI(2,4)) +
     *   0.999
      INBLC(1) = MAX (1, IB(1))
      INTRC(1) = MIN (TRCIN(1), IT(1))
      INBLC(2) = MAX (1, IB(2))
      INTRC(2) = MIN (TRCIN(2), IT(2))
      GO TO 999
C                                       Error
 985  CALL MSGWRT (8)
 990  MSGTXT = 'ERROR FINDING WINDOW ' // IN
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE IMINST (IN, PLANE, HWIDTH, IERR)
C-----------------------------------------------------------------------
C   Private function
C   Set up for interpolation.  Read input plane to memory
C   Image is padded with blanks for half the interpolation width on each
C   side of the image and stored in the memory.
C   Inputs:
C      IN      C*?  Input image (real only)
C      PLANE   I(7) Plane desired
C      HWIDTH  I    Half width in pixels in interpolation kernal (1-4)
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER IN*(*)
      INTEGER   PLANE(7), HWIDTH, IERR
C
      INTEGER   TYPE, DIM(7), NAXIS(7), I, J, BLC(7), TRC(7), WORDS,
     *   DUMMY, MSGSAV
      LONGINT   IAP
      CHARACTER ACCESS*8, CDUMMY*1
      REAL      PROD, X
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'ITRSTUF.INC'
C-----------------------------------------------------------------------
C                                       Determine image size in memory
      CALL ARRWIN (IN, BLCI, TRCI, NAXIS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       HWIDTH must be in (1,4)
      IWID = HWIDTH * 2 + 1
      IWID = MAX (3, MIN (IWID, 9))
      IHALF = IWID / 2
      IWID = HWIDTH * 2 + 1
      IWID = MAX (3, MIN (IWID, 9))
C                                       desperation
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OGET (IN, 'RELIABLE', TYPE, DIM, X, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         X = 0.3334
         END IF
      IF (IERR.NE.0) GO TO 990
      IF ((X.LE.0.0) .OR. (X.GE.1.0)) X = 0.3334
      IRELY = X * IWID * IWID + 0.5
      IRELY = MAX (1, IRELY)
C                                       Make sure AP big enough
      IF (.NOT.APOPEN) THEN
         NXAP = NAXIS(1)
         NYAP = NAXIS(2)
         WORDS = (NAXIS(1) * NAXIS(2) - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', 'INTERP', WORDS, INIMAG, PTIMAG, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, WORDS
            CALL MSGWRT (8)
            GO TO 990
            END IF
         APOPEN = .TRUE.
         END IF
C                                       Load
C                                       Set image array access to plane
      ACCESS = 'PLANE'
      DIM(1) = LEN (ACCESS)
      DIM(2) = 1
      DIM(3) = 0
      CALL ARPPUT (IN, 'ACCESS', OOACAR, DIM, DUMMY, ACCESS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Set plane
      CALL COPY (7, BLCI, BLC)
      CALL COPY (7, TRCI, TRC)
      DO 10 I = 3,7
         BLC(I) = BLC(I) - 1 + PLANE(I)
         TRC(I) = BLC(I)
 10      CONTINUE
C                                       Save new window
      DIM(1) = 7
      DIM(2) = 1
      CALL ARDPUT (IN, 'BLC', OOAINT, DIM, BLC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ARDPUT (IN, 'TRC', OOAINT, DIM, TRC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Transfer
      IAP = PTIMAG + 1
      CALL ARREAD (IN, DIM, INIMAG(IAP), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close array
      CALL ARRCLO (IN, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Reset window
      DIM(1) = 7
      DIM(2) = 1
      CALL ARDPUT (IN, 'BLC', OOAINT, DIM, BLCI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ARDPUT (IN, 'TRC', OOAINT, DIM, TRCI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Reset image array access
      ACCESS = 'ROW'
      DIM(1) = LEN (ACCESS)
      DIM(2) = 1
      CALL ARPPUT (IN, 'ACCESS', OOACAR, DIM, DUMMY, ACCESS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Geometry
      CALL IMDGET (IN, 'CDELT', TYPE, DIM, ICDELT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMDGET (IN, 'CRPIX', TYPE, DIM, ICRPIX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (ABS (ICDELT(1)).LT.1.0E-12) ICDELT(1) = 1.0
      IF (ABS (ICDELT(2)).LT.1.0E-12) ICDELT(2) = 1.0
C                                       Initialize interpolation fn.
      XINT(1) = FBLANK
      YINT(1) = FBLANK
C                                       Calculate Lagrangian
C                                       denominators
      DO 210 J = 1,IWID
         PROD = 1.0
         DO 200 I = 1,IWID
            IF (I.NE.J) PROD = PROD * (J - I)
 200        CONTINUE
         DENOM(J) = 1.0 / PROD
 210     CONTINUE
      GO TO 999
C                                       Error
 990  MSGTXT = 'IMINST: ERROR INITIALIZING ' // IN
      CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMINST: ERROR',I4,' ALLOCATING',I10,
     *   ' KILOWORDS OF MEMORY')
      END
      SUBROUTINE IMINTR (X, Y, VAL)
C-----------------------------------------------------------------------
C   Private function
C   Interpolate value.  Returned value blanked if outside image or most
C   data blanked.
C   Inputs:
C      X       R    "X" coordinate
C      Y       R    "Y" coordinate
C   Output:
C      VAL     R    Interpolated value, possibly blanked.
C-----------------------------------------------------------------------
      REAL      X, Y, VAL
C
      INTEGER   I, J, K, GOOD
      LONGINT   IAP
      REAL      XPIX, YPIX, SUM, SUMWT, WT, WTY, ROW(10), PROD, DEN, XP,
     *   YP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'ITRSTUF.INC'
C-----------------------------------------------------------------------
C                                       Convert position to pixels in
C                                       subimage
      XPIX = (X / ICDELT(1)) + ICRPIX(1) - BLCI(1) + 1
      YPIX = (Y / ICDELT(2)) + ICRPIX(2) - BLCI(2) + 1
      VAL = FBLANK
C                                       Outside image?
      IF ((XPIX.LT.0.5) .OR. (YPIX.LT.0.5) .OR.
     *   (XPIX.GT.NXAP+0.5) .OR. (YPIX.GT.NYAP+0.5)) GO TO 999
C                                       Get interpolation fn
      IF (XPIX.NE.XINT(1)) CALL IMINFN (XPIX, NXAP, XINT, IXCEN)
      IF (YPIX.NE.YINT(1)) CALL IMINFN (YPIX, NYAP, YINT, IYCEN)
      SUM = 0.0
      SUMWT = 0.0
      GOOD = 0
      DO 20 J = 1,IWID
         WTY = YINT(J+1)
         DO 10 I = 1,IWID
            IAP = PTIMAG + IXCEN + I + ((IYCEN + J - 1) * NXAP)
            WT = XINT(I+1) * WTY
            IF (INIMAG(IAP).NE.FBLANK) THEN
               SUMWT = SUMWT + WT
               SUM = SUM + INIMAG(IAP) * WT
               GOOD = GOOD + 1
               END IF
 10         CONTINUE
 20      CONTINUE
C                                       Normalize sum
      IF (SUMWT.GT.0.90) THEN
         VAL = SUM / SUMWT
         GO TO 999
         END IF
C                                       Too much blanked data; try again
C                                       using knowledge of blanks if
C                                       sufficient data.
      IF (GOOD.LT.IRELY) GO TO 999
C                                       Set "x" at first pixel to 1.0
      XP = XPIX - IXCEN
      YP = YPIX - IYCEN
      DO 200 J = 1,IWID
C                                       Interpolate in rows
         SUM = 0.0
         SUMWT = 0.0
         DO 120 I = 1,IWID
            DEN = 1.0
            PROD = 1.0
            DO 110 K = 1,IWID
               IAP = PTIMAG + IXCEN + K + ((IYCEN + J - 1) * NXAP)
               IF (INIMAG(IAP).NE.FBLANK) THEN
                  IF (I.NE.K) THEN
                     DEN = DEN * (I - K)
                     PROD = PROD * (XP - K)
                     END IF
                  END IF
 110           CONTINUE
            IAP = PTIMAG + IXCEN + I + ((IYCEN + J - 1) * NXAP)
C                                       Accumulate
            IF (INIMAG(IAP).NE.FBLANK) THEN
               IF (ABS (DEN).GT.1.0E-10) THEN
                  WT = PROD / DEN
               ELSE
                  WT = 0.0
                  END IF
               SUMWT = SUMWT + WT
               SUM = SUM + WT * INIMAG(IAP)
               END IF
 120        CONTINUE
C                                       Interpolate column value
         IF (SUMWT.GT.0.5) THEN
            ROW(J) = SUM / SUMWT
         ELSE
            ROW(J) = FBLANK
            END IF
 200     CONTINUE
C                                       Interpolate in column
         SUM = 0.0
         SUMWT = 0.0
         DO 220 I = 1,IWID
            DEN = 1.0
            PROD = 1.0
            DO 210 K = 1,IWID
               IF (ROW(K).NE.FBLANK) THEN
                  IF (I.NE.K) THEN
                     DEN = DEN * (I - K)
                     PROD = PROD * (YP - K)
                     END IF
                  END IF
 210           CONTINUE
C                                       Accumulate
            IF (ROW(I).NE.FBLANK) THEN
               IF (ABS (DEN).GT.1.0E-10) THEN
                  WT = PROD / DEN
               ELSE
                  WT = 0.0
                  END IF
               SUMWT = SUMWT + WT
               SUM = SUM + WT * ROW(I)
               END IF
 220        CONTINUE
C                                       Interpolate value
         IF (SUMWT.GT.0.5) THEN
            VAL = SUM / SUMWT
         ELSE
            VAL = FBLANK
            END IF
C
 999  RETURN
      END
      SUBROUTINE IMINTP (XPX, YPX, VAL)
C-----------------------------------------------------------------------
C   Private function
C   Interpolate value.  Returned value blanked if outside image or most
C   data blanked.  Arguments are in pixels.
C   Inputs:
C      XPX     R    "X" pixel number
C      YPX     R    "Y" pixel number
C   Output:
C      VAL     R    Interpolated value, possibly blanked.
C-----------------------------------------------------------------------
      REAL      XPX, YPX, VAL
C
      INTEGER   I, J, K, GOOD
      LONGINT   IAP
      REAL      SUM, SUMWT, WT, WTY, ROW(10), PROD, DEN, XP, YP, XPIX,
     *   YPIX
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'ITRSTUF.INC'
C-----------------------------------------------------------------------
      VAL = FBLANK
C                                       Correct pixels for subimaging
      XPIX = XPX - BLCI(1) + 1
      YPIX = YPX - BLCI(2) + 1
C                                       Outside image?
      IF ((XPIX.LT.0.5) .OR. (YPIX.LT.0.5) .OR.
     *   (XPIX.GT.NXAP+0.5) .OR. (YPIX.GT.NYAP+0.5)) GO TO 999
C                                       Get interpolation fn
      IF (XPIX.NE.XINT(1)) CALL IMINFN (XPIX, NXAP, XINT, IXCEN)
      IF (YPIX.NE.YINT(1)) CALL IMINFN (YPIX, NYAP, YINT, IYCEN)
      SUM = 0.0
      SUMWT = 0.0
      GOOD = 0
      DO 20 J = 1,IWID
         WTY = YINT(J+1)
         DO 10 I = 1,IWID
            IAP = PTIMAG + IXCEN + I + ((IYCEN + J - 1) * NXAP)
            WT = XINT(I+1) * WTY
            IF (INIMAG(IAP).NE.FBLANK) THEN
               SUMWT = SUMWT + WT
               SUM = SUM + INIMAG(IAP) * WT
               GOOD = GOOD + 1
               END IF
 10         CONTINUE
 20      CONTINUE
C                                       Normalize sum
      IF (SUMWT.GT.0.90) THEN
         VAL = SUM / SUMWT
         GO TO 999
         END IF
C                                       Too much blanked data; try again
C                                       using knowledge of blanks if
C                                       sufficient data.
      IF (GOOD.LT.IRELY) GO TO 999
C                                       Set "x" at first pixel to 1.0
      XP = XPIX - IXCEN
      YP = YPIX - IYCEN
      DO 200 J = 1,IWID
C                                       Interpolate in rows
         SUM = 0.0
         SUMWT = 0.0
         DO 120 I = 1,IWID
            DEN = 1.0
            PROD = 1.0
            DO 110 K = 1,IWID
               IAP = PTIMAG + IXCEN + K + ((IYCEN + J - 1) * NXAP)
               IF (INIMAG(IAP).NE.FBLANK) THEN
                  IF (I.NE.K) THEN
                     DEN = DEN * (I - K)
                     PROD = PROD * (XP - K)
                     END IF
                  END IF
 110           CONTINUE
            IAP = PTIMAG + IXCEN + I + ((IYCEN + J - 1) * NXAP)
C                                       Accumulate
            IF (INIMAG(IAP).NE.FBLANK) THEN
               IF (ABS (DEN).GT.1.0E-10) THEN
                  WT = PROD / DEN
               ELSE
                  WT = 0.0
                  END IF
               SUMWT = SUMWT + WT
               SUM = SUM + WT * INIMAG(IAP)
               END IF
 120        CONTINUE
C                                       Interpolate column value
         IF (SUMWT.GT.0.5) THEN
            ROW(J) = SUM / SUMWT
         ELSE
            ROW(J) = FBLANK
            END IF
 200     CONTINUE
C                                       Interpolate in column
         SUM = 0.0
         SUMWT = 0.0
         DO 220 I = 1,IWID
            DEN = 1.0
            PROD = 1.0
            DO 210 K = 1,IWID
               IF (ROW(K).NE.FBLANK) THEN
                  IF (I.NE.K) THEN
                     DEN = DEN * (I - K)
                     PROD = PROD * (YP - K)
                     END IF
                  END IF
 210           CONTINUE
C                                       Accumulate
            IF (ROW(I).NE.FBLANK) THEN
               IF (ABS (DEN).GT.1.0E-10) THEN
                  WT = PROD / DEN
               ELSE
                  WT = 0.0
                  END IF
               SUMWT = SUMWT + WT
               SUM = SUM + WT * ROW(I)
               END IF
 220        CONTINUE
C                                       Interpolate value
         IF (SUMWT.GT.0.5) THEN
            VAL = SUM / SUMWT
         ELSE
            VAL = FBLANK
            END IF
C
 999  RETURN
      END
      SUBROUTINE IMINFN (POS, N, INT, CEN)
C-----------------------------------------------------------------------
C   Private function
C   Returns Lagrangian interpolation weights.  Adjustments are made to
C   account for the ends of the data.
C   Inputs:
C      POS     R    Coordinate
C      N       I    Maximum pixel number
C   Output:
C      INT     R(*) Interpolation array
C                   (1) = POS
C                   (2...) interpolation weights.
C      CEN     I    (First - 1) pixel number for use
C-----------------------------------------------------------------------
      REAL      POS, INT(*)
      INTEGER   N, CEN
C
      INTEGER   IPOS, I, J
      REAL      PROD, XX
      INCLUDE 'ITRSTUF.INC'
C-----------------------------------------------------------------------
      INT(1) = POS
C                                       Fractional pixel
      IPOS = POS + 0.5
C                                       Set first pixel
      CEN = IPOS - IHALF
      CEN = MAX (1, MIN (CEN, (N-IWID+1)))
C                                       Make 0 rel
      CEN = CEN - 1
C                                       Set "x" at first pixel to 1.0
      XX = POS - CEN
C                                       Compute interpolating kernal
      DO 50 J = 1,IWID
         PROD = DENOM(J)
         DO 30 I = 1,IWID
            IF (I.NE.J) PROD = PROD * (XX - I)
 30         CONTINUE
         INT(J+1) = PROD
 50      CONTINUE
C
 999  RETURN
      END
