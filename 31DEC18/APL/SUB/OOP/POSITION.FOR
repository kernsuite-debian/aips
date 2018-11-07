C   Observing position class: name = 'POSITION'
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "Observing position" class library
C# Map-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-1999, 2004, 2006, 2015
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
C   Contains pointing position and phase center shift information.
C
C   Class Public members:
C      OBSRA     D   Pointing position: RA (degrees)
C      OBSDEC    D   Pointing position: Dec (degrees)
C      XSHIFT    R   Phase shift in RA (degrees)
C      YSHIFT    R   Phase shift in Dec (degrees)
C
C   Public functions:
C      PSNGET (name, keywrd, type, dim, value, valuec, ierr)
C         Return position member.
C      PSNPUT (name, keywrd, type, dim, value, valuec, ierr)
C         Store position member.
C      PSNCVT (in, xyzi, out, xyzo, ierr)
C         Convert pixel position in one image to pixel position in
C         another.
C      PSNCV3 (in, xyzi, out, which, tza, cpa, spa, pbfwhm, fbwsq, c123,
C         xyzo, ierr)
C         Convert pixel positions correcting for array plane
C         misorientation and radial scaling.
C      PSNPIX (in, x, y, xpix, ypix, ierr)
C         Determine the pixel location for a given x,y coordinate.
C      PSNVAL (in, xpix, ypix, x, y, z, ierr)
C         Determine the location for a given pixel.
C      PSNANG (in, x, y, radius, ierr)
C         Determine the distance to the pointing position
C
LOCAL INCLUDE 'POSITION.INC'
C                                       Position info
      CHARACTER IMAGE1*32, IMAGE2*32
      INTEGER   ITI, ITO
      REAL      EPOK, CDELT1(7), CDELT2(7), CRPIX1(7), CRPIX2(7)
      DOUBLE PRECISION RA0, DEC0, RAC, DECC
      COMMON /PSNCOM/ RA0, DEC0, RAC, DECC,
     *   EPOK, CDELT1, CDELT2, CRPIX1, CRPIX2,
     *   ITI, ITO
      COMMON /PSNCMC/ IMAGE1, IMAGE2
LOCAL END
LOCAL INCLUDE 'POSMEMS.INC'
C                                       POSITION class include
      INTEGER   NMEML
      PARAMETER (NMEML = 4)
      CHARACTER MEMS(NMEML)*8, THSCLS*16
      DATA MEMS /'OBSRA','OBSDEC','XSHIFT','YSHIFT'/
      DATA THSCLS /'POSITION'/
LOCAL END
      SUBROUTINE PSNGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public
C   Return position member.
C   Inputs:
C      NAME    C*?    Object name
C      KEYWRD  C*(*)  Keyword in form 'mem1.mem2...'
C   Outputs:
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of the array.
C      VALUE   ?     associated value (non character)
C      VALUEC  C*?   associated value (character)
C      IERR    I     Error code, 0=OK.  1=> didn't find.,
C                    2= Input error.
C-----------------------------------------------------------------------
      INTEGER   TYPE, DIM(*), IERR
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
      DOUBLE PRECISION VALUE(*)
C
      INTEGER   IMEM, LOOP, OBJNUM, POINT
      CHARACTER MEMBER*16
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'POSITION.INC'
      INCLUDE 'POSMEMS.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup NAME
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Look for base class.member name
C                                       in KEYWRD.
      POINT = INDEX (KEYWRD, '.')
C                                       No base classes exist for this
C                                       class.
      IF (POINT.GE.1) THEN
         IERR = 2
         MSGTXT = 'NO BASE CLASSES FOR CLASS ' // THSCLS
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Save member name
      IF (POINT.LE.0) THEN
         MEMBER = KEYWRD
      ELSE
         MEMBER = KEYWRD(1:POINT-1)
         END IF
C                                       Search list of recognized
C                                       members.
      IMEM = -1
      DO 10 LOOP = 1,NMEML
         IF (MEMBER.EQ.MEMS(LOOP)) IMEM = LOOP
 10      CONTINUE
C                                       Find it?
      IF (IMEM.LE.0) THEN
         IERR = 2
         MSGTXT = 'UNRECOGNIZED ' // THSCLS // ' MEMBER ' // MEMBER
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Fetch value:
         CALL OBGET (OBJNUM, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      IERR)
C                                       Message if not found
         IF (IERR.EQ.1) THEN
            MSGTXT = 'MEMBER ' // MEMBER // ' NOT FOUND'
            CALL MSGWRT (6)
            MSGTXT = 'OBJECT =' // NAME
            CALL MSGWRT (6)
            END IF
C
 999  RETURN
      END
      SUBROUTINE PSNPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public
C   Store position member.
C   Inputs:
C      NAME    C*?    Object name
C      KEYWRD  C*(*)  Keyword in form 'mem1.mem2...'
C   Outputs:
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of the array.
C      VALUE   ?     associated value (non character)
C      VALUEC  C*?   associated value (character)
C      IERR    I     Error code, 0=OK.  1=> didn't find.,
C                    2= Input error.
C-----------------------------------------------------------------------
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
C
      INTEGER   IMEM, LOOP, OBJNUM, POINT
      CHARACTER MEMBER*16
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'POSITION.INC'
      INCLUDE 'POSMEMS.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup NAME
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Look for base class.member name
C                                       in KEYWRD.
      POINT = INDEX (KEYWRD, '.')
C                                       No base classes exist for this
C                                       class.
      IF (POINT.GE.1) THEN
         IERR = 2
         MSGTXT = 'NO BASE CLASSES FOR CLASS ' // THSCLS
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Save member name
      IF (POINT.LE.0) THEN
         MEMBER = KEYWRD
      ELSE
         MEMBER = KEYWRD(1:POINT-1)
         END IF
C                                       Search list of recognized
C                                       members.
      IMEM = -1
      DO 10 LOOP = 1,NMEML
         IF (MEMBER.EQ.MEMS(LOOP)) IMEM = LOOP
 10      CONTINUE
C                                       Find it?
      IF (IMEM.LE.0) THEN
         IERR = 2
         MSGTXT = 'UNRECOGNIZED ' // THSCLS // ' MEMBER ' // MEMBER
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Fetch value:
         CALL OBPUT (OBJNUM, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      IERR)
C                                       Message if not stored
         IF (IERR.NE.0) THEN
            MSGTXT = 'MEMBER ' // MEMBER // ' COULD NOT BE STORED'
            CALL MSGWRT (6)
            MSGTXT = 'OBJECT =' // NAME
            CALL MSGWRT (6)
            END IF
C
 999  RETURN
      END
      SUBROUTINE PSNCVT (IN, XYZI, OUT, XYZO, IERR)
C-----------------------------------------------------------------------
C   Public
C   Convert pixel position in one image to pixel position in another.
C   Inputs:
C      IN      C*?    Input image object
C      XYZI    R(7)   Pixel position in IN
C      OUT     C*?    Output image object
C   Outputs:
C      XYZO    R(7)   Pixel position in OUT.
C      IERR    I      Error code, 0=OK.
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      REAL      XYZI(7), XYZO(7)
      INTEGER   IERR
C
      INTEGER   TYPE, DIM(7), I, IDEP(5), IROUND
      LOGICAL   SWAPOK
      CHARACTER CDUMMY*1
      DOUBLE PRECISION X, Y, Z
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'POSITION.INC'
      DATA SWAPOK /.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get info for input image if
C                                       necessary.
      IF (IN.NE.IMAGE1) THEN
         IMAGE1 = IN
C                                       Get catalog header
         CALL OBHGET (IN, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 990
         DO 10 I = 1,5
            IDEP(I) = IROUND (XYZI(I+2))
 10         CONTINUE
         LOCNUM = 1
         CALL SETLOC (IDEP, SWAPOK)
         ITI = 1
         IF (LABTYP(LOCNUM).LT.44) ITI = 2
         IF (LABTYP(LOCNUM).LT.22) ITI = 3
         RA0 = 0.0D0
         DEC0 = 0.0D0
         END IF
C                                       Get info for input image if
C                                       necessary.
      IF (OUT.NE.IMAGE2) THEN
         IMAGE2 = OUT
C                                       Get catalog header
         CALL OBHGET (OUT, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 990
         DO 20 I = 1,5
            IDEP(I) = IROUND (XYZO(I+2))
 20         CONTINUE
         LOCNUM = 2
         CALL SETLOC (IDEP, SWAPOK)
         ITO = 1
         IF (LABTYP(LOCNUM).LT.44) ITO = 2
         IF (LABTYP(LOCNUM).LT.22) ITO = 3
         CALL IMDGET (OUT, 'EPOCH', TYPE, DIM, EPOK, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       appropriate?
         IF ((CORTYP(1).LT.1) .OR. (CORTYP(2).LT.1) .OR.
     *      (CORTYP(1).GT.2) .OR. (CORTYP(2).GT.2)) THEN
            IF (CORTYP(2).NE.CORTYP(1)) THEN
               MSGTXT = 'PSNCVT: IMPROPER COMBINATION OF COORDINATES'
               CALL MSGWRT (8)
               IERR = 8
               GO TO 990
               END IF
            IF (ITI.NE.ITO) THEN
               MSGTXT = 'PSNCVT: IMPROPER COORDINATES FOR CONVERSION'
               CALL MSGWRT (8)
               IERR = 8
               GO TO 990
               END IF
            END IF
         END IF
C                                       Get position of input
      LOCNUM = 1
      CALL XYVAL (XYZI(1), XYZI(2), X, Y, Z, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Convert types if necessary
      IF (ITI.NE.ITO) THEN
         IF (CORTYP(LOCNUM).EQ.1) THEN
            CALL COORDT (ITI, ITO, X, Y, EPOK, X, Y, IERR)
         ELSE
            CALL COORDT (ITI, ITO, Y, X, EPOK, Y, X, IERR)
            END IF
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       same order
      IF (CORTYP(1).EQ.CORTYP(2)) THEN
C                                       Get corresponding pixel in OUT
         LOCNUM = 2
         CALL XYPIX (X, Y, XYZO(1), XYZO(2), IERR)
         IF (IERR.NE.0) GO TO 990
C                                       reverse order
      ELSE
         LOCNUM = 2
         CALL XYPIX (Y, X, XYZO(1), XYZO(2), IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  MSGTXT = 'PSNCVT: ERROR CONVERTING PIXEL POSITIONS FOR ' // IN
      CALL MSGWRT (7)
      MSGTXT = '  TO ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE PSNCV3 (IN, XYZI, OUT, WHICH, TZA, CPA, SPA, PBFWHM,
     *   FBWSQ, C123, XYZO, IERR)
C-----------------------------------------------------------------------
C   Public
C   Convert pixel position in one image to pixel position in another
C   correcting for array plane misorientation and radial scaling.  The
C   pixel coordinate is scaled radially from the antenna pointing center
C   to allow correction for primary beam variation across the bandpass.
C   The scaling is in the sense of from true to apparent positions.
C      The image scaling will be done if either PBFWHM or C0 is greater
C   than 0; C0 is the constant scaling term.  The full scaling factor
C   is:
C      factor = (C0 - FBWSQ * rnsq * (C1 + rnsq * (C2 + rnsq * C3)))
C   where rnsq = (radial_distance / PBFWHM) ** 2
C   Note: the 3-D correction is only defined if the output image can
C   accurately described by a single pair of parallactic and zenith
C   angles.  If no 3-D corrections are desired use TZA = 0, CPA = 1 and
C   SPA = 0.
C   Inputs:
C      IN      C*?    Input image object
C      XYZI    R(7)   Pixel position in IN
C      OUT     C*?    Output image object
C      WHICH   L      TRUE => use coordinates in OUT to compute the
C                     3D and radial corrections, else use IN.
C      TZA     R      Tan (Zenith angle) of output image
C      CPA     R      Cos (Paralactic angle) of output image
C      SPA     R      Sin (Paralactic angle) of output image
C      PBFWHM  R      Primary beam FWHM at nomimal frequency (deg)
C                     If zero then no correction is done.
C      FBWSQ   R      Square of fractional bandpass.
C      C123    R(4)   Primary beam shape and scaling parameters: C0, C1,
C                     C2, C3
C   Outputs:
C      XYZO    R(7)   Pixel position in OUT.
C      IERR    I      Error code, 0=OK.
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      REAL      XYZI(7), TZA, CPA, SPA, XYZO(7), PBFWHM, FBWSQ, C123(4)
      LOGICAL   WHICH
      INTEGER   IERR
C
      INTEGER   TYPE, DIM(7), I, IDEP(5), IROUND
      LOGICAL   SWAPOK, DO3D, DORAD, SAME
      REAL      RSC, RAD, RADR, RNORM, RNSQ
      CHARACTER CDUMMY*1
      DOUBLE PRECISION X, Y, Z, LL, MM, LLP, MMP, DD, XR, YR, CRVAL(7),
     *   DDUMMY(1)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'POSITION.INC'
      DATA SWAPOK /.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
      DO3D = TZA.NE.0.0
      DORAD = (PBFWHM.GT.0.0) .OR. (C123(1).GT.0.0)
C                                       Get info for input image if
C                                       necessary.
      IF (IN.NE.IMAGE1) THEN
         IMAGE1 = IN
C                                       Get catalog header
         CALL OBHGET (IN, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 990
         DO 10 I = 1,5
            IDEP(I) = IROUND (XYZI(I+2))
 10         CONTINUE
         LOCNUM = 1
         CALL SETLOC (IDEP, SWAPOK)
         IF ((AXTYP(LOCNUM).NE.1) .OR. (CORTYP(LOCNUM).LT.1) .OR.
     *      (CORTYP(LOCNUM).GT.2)) THEN
            IERR = 1
            MSGTXT = 'COORDINATES INAPPROPRIATE FOR 3D CORRECTION'
            CALL MSGWRT (7)
            GO TO 990
            END IF
         RA0 = 0.0D0
         DEC0 = 0.0D0
         ITI = 1
         IF (LABTYP(LOCNUM).LT.44) ITI = 2
         IF (LABTYP(LOCNUM).LT.22) ITI = 3
         IF (.NOT.WHICH) THEN
            CALL IMDGET (IN, 'CRVAL', TYPE, DIM, CRVAL, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            RAC = CRVAL(CORTYP(LOCNUM))
            DECC = CRVAL(3-CORTYP(LOCNUM))
            RAC = COND2R * RAC
            DECC = COND2R * DECC
            IF ((ABS (RAC).LE.1.0D-10) .AND. (ABS (DECC).LE.1.0D-10))
     *         THEN
               IERR = 1
               MSGTXT = 'TANGENT POSITION NOT IN HEADER'
               CALL MSGWRT (7)
               GO TO 990
               END IF
            CALL IMDGET (IN, 'EPOCH', TYPE, DIM, EPOK, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL PSNGET (IN, 'OBSRA', TYPE, DIM, DDUMMY, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            RA0 = DDUMMY(1)
            CALL PSNGET (IN, 'OBSDEC', TYPE, DIM, DDUMMY, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            DEC0 = DDUMMY(1)
            RA0 = COND2R * RA0
            DEC0 = COND2R * DEC0
            IF ((ABS (RA0).LE.1.0D-10) .AND. (ABS (DEC0).LE.1.0D-10))
     *         THEN
               IERR = 1
               MSGTXT = 'POINTING POSITION NOT IN HEADER'
               CALL MSGWRT (7)
               GO TO 990
               END IF
            END IF
C                                       Other geometry
         CALL IMDGET (IN, 'CDELT', TYPE, DIM, CDELT1, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL IMDGET (IN, 'CRPIX', TYPE, DIM, CRPIX1, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Get info for input image if
C                                       necessary.
      IF (OUT.NE.IMAGE2) THEN
         IMAGE2 = OUT
C                                       Get catalog header
         CALL OBHGET (OUT, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 990
         DO 20 I = 1,5
            IDEP(I) = IROUND (XYZO(I+2))
 20         CONTINUE
         LOCNUM = 2
         CALL SETLOC (IDEP, SWAPOK)
         IF ((AXTYP(LOCNUM).NE.1) .OR. (CORTYP(LOCNUM).LT.1) .OR.
     *      (CORTYP(LOCNUM).GT.2)) THEN
            IERR = 1
            MSGTXT = 'COORDINATES INAPPROPRIATE FOR 3D CORRECTION'
            CALL MSGWRT (7)
            GO TO 990
            END IF
         ITO = 1
         IF (LABTYP(LOCNUM).LT.44) ITO = 2
         IF (LABTYP(LOCNUM).LT.22) ITO = 3
         IF (WHICH) THEN
            CALL IMDGET (OUT, 'CRVAL', TYPE, DIM, CRVAL, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            RAC = CRVAL(CORTYP(LOCNUM))
            DECC = CRVAL(3-CORTYP(LOCNUM))
            RAC = COND2R * RAC
            DECC = COND2R * DECC
            IF ((ABS (RAC).LE.1.0D-10) .AND. (ABS (DECC).LE.1.0D-10))
     *         THEN
               IERR = 1
               MSGTXT = 'TANGENT POSITION NOT IN HEADER'
               CALL MSGWRT (7)
               GO TO 990
               END IF
            CALL IMDGET (OUT, 'EPOCH', TYPE, DIM, EPOK, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL PSNGET (OUT, 'OBSRA', TYPE, DIM, DDUMMY, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            RA0 = DDUMMY(1)
            CALL PSNGET (OUT, 'OBSDEC', TYPE, DIM, DDUMMY, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            DEC0 = DDUMMY(1)
            RA0 = COND2R * RA0
            DEC0 = COND2R * DEC0
            IF ((ABS (RA0).LE.1.0D-10) .AND. (ABS (DEC0).LE.1.0D-10))
     *         THEN
               IERR = 1
               MSGTXT = 'POINTING POSITION NOT IN HEADER'
               CALL MSGWRT (7)
               GO TO 990
               END IF
            END IF
C                                       Other geometry
         CALL IMDGET (OUT, 'CDELT', TYPE, DIM, CDELT2, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL IMDGET (OUT, 'CRPIX', TYPE, DIM, CRPIX2, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       two positions or one
      SAME = (ABS(RAC-RA0).LT.4.D-9) .AND. (ABS(DECC-DEC0).LT.4.D-9)
C                                       Get position of input
      LOCNUM = 1
      CALL XYVAL (XYZI(1), XYZI(2), X, Y, Z, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (CORTYP(LOCNUM).EQ.2) THEN
         Z = X
         X = Y
         Y = Z
         END IF
C                                       Convert types if necessary
      IF (ITI.NE.ITO) THEN
         CALL COORDT (ITI, ITO, X, Y, EPOK, X, Y, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       output image
      IF (WHICH) LOCNUM = 2
      X = X * COND2R
      Y = Y * COND2R
C                                       Convert to direction cosines
C                                       wrt to tangent point
      IF (DO3D) THEN
         CALL DIRCOS (AXFUNC(KLOCL(LOCNUM)+1,LOCNUM), RAC, DECC, X, Y,
     *      LL, MM, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Get apparent position
         DD = SQRT (1.0D0 - LL*LL - MM*MM) - 1.0D0
         LLP = LL - DD * TZA * SPA
         MMP = MM - DD * TZA * CPA
         IF ((.NOT.SAME) .OR. (.NOT.DORAD)) THEN
            CALL NEWPOS (AXFUNC(KLOCL(LOCNUM)+1,LOCNUM), RAC, DECC, LLP,
     *         MMP, X, Y, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
C                                       Make radial scaling corrections.
      IF (DORAD) THEN
         IF ((.NOT.SAME) .OR. (.NOT.DO3D)) THEN
            CALL DIRCOS (AXFUNC(KLOCL(LOCNUM)+1,LOCNUM), RA0, DEC0, X,
     *         Y, LLP, MMP, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         XR = LLP
         YR = MMP
         RADR = SQRT (XR*XR + YR*YR)
         RAD = RADR / COND2R
         IF (PBFWHM.GT.0.0) THEN
            RNORM = RAD / PBFWHM
            RNSQ = RNORM * RNORM
         ELSE
            RNSQ = 0.0
            END IF
         RSC = (C123(1) - FBWSQ * RNSQ * (C123(2) + RNSQ * (C123(3) +
     *      RNSQ * C123(4))))
         LLP = LLP * RSC
         MMP = MMP * RSC
C                                       Convert back to position.
         CALL NEWPOS (AXFUNC(KLOCL(LOCNUM)+1,LOCNUM), RA0, DEC0, LLP,
     *      MMP, X, Y, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      X = X / COND2R
      Y = Y / COND2R
C                                       Get corresponding pixel in OUT
      LOCNUM = 2
      IF (CORTYP(LOCNUM).EQ.2) THEN
         Z = X
         X = Y
         Y = X
         END IF
      CALL XYPIX (X, Y, XYZO(1), XYZO(2), IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'PSNCV3: ERROR CONVERTING PIXEL POSITIONS FOR ' // IN
      CALL MSGWRT (7)
      MSGTXT = '  TO ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE PSNPIX (IN, X, Y, XPIX, YPIX, IERR)
C-----------------------------------------------------------------------
C   Public
C   Determine pixel position of a given x,y coordinate (e.g. RA, Dec)
C   Inputs:
C      IN      C*?    Input image object
C      X       D      First axis coordinate in header units (degrees)
C      Y       D      Sedond axis coordinate in header units (degrees)
C   Outputs:
C      XPIX    R      First axis pixel number corresponding to (X,Y)
C      YPIX    R      Second axis pixel number corresponding to (X,Y)
C      IERR    I      Error code, 0=OK.
C-----------------------------------------------------------------------
      CHARACTER IN*(*)
      DOUBLE PRECISION X, Y
      REAL      XPIX, YPIX
      INTEGER   IERR
C
      INTEGER   IDEP(5)
      LOGICAL   SWAPOK
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'POSITION.INC'
      DATA SWAPOK /.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get info for input image if
C                                       necessary.
      IF (IN.NE.IMAGE1) THEN
         IMAGE1 = IN
C                                       Get catalog header
         CALL OBHGET (IN, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL FILL (5, 1, IDEP)
         LOCNUM = 1
         CALL SETLOC (IDEP, SWAPOK)
         ITI = 1
         IF (LABTYP(LOCNUM).LT.44) ITI = 2
         IF (LABTYP(LOCNUM).LT.22) ITI = 3
         RA0 = 0.0D0
         DEC0 = 0.0D0
         END IF
C                                       Get pixel value
      LOCNUM = 1
      CALL XYPIX (X, Y, XPIX, YPIX, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'PSNPIX: ERROR FINDING PIXEL IN ' // IN
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE PSNVAL (IN, XPIX, YPIX, X, Y, Z, IERR)
C-----------------------------------------------------------------------
C   Public
C   Determine the coordinate value of a given pixel in an image.
C   Inputs:
C      IN      C*?    Input image object
C      XPIX    R      Pixel location, x-coordinate
C      YPIX    R      Pixel location, y-coordinate
C   Outputs:
C      X       D      X-coordinate value at pixel location
C      Y       D      Y-coordinate value at pixel location
C      Z       D      Z-coordinate value (if part of a position
C                     pair with either X or Y)
C      IERR    I      Error code, 0=OK.
C-----------------------------------------------------------------------
      CHARACTER IN*(*)
      DOUBLE PRECISION X, Y, Z
      REAL      XPIX, YPIX
      INTEGER   IERR
C
      INTEGER   IDEP(5)
      LOGICAL   SWAPOK
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'POSITION.INC'
      DATA SWAPOK /.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get info for input image if
C                                       necessary.
      IF (IN.NE.IMAGE1) THEN
         IMAGE1 = IN
C                                       Get catalog header
         CALL OBHGET (IN, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL FILL (5, 1, IDEP)
         LOCNUM = 1
         CALL SETLOC (IDEP, SWAPOK)
         ITI = 1
         IF (LABTYP(LOCNUM).LT.44) ITI = 2
         IF (LABTYP(LOCNUM).LT.22) ITI = 3
         RA0 = 0.0D0
         DEC0 = 0.0D0
         END IF
C                                       Get coordinate value
      LOCNUM = 1
      CALL XYVAL (XPIX, YPIX, X, Y, Z, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'PSNVAL: ERROR FINDING COORDINATES IN ' // IN
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE PSNANG (IN, X, Y, RADIUS, IERR)
C-----------------------------------------------------------------------
C   Public
C   Determine the angle from a point to the pointing position
C   Inputs:
C      IN       C*?   Input image object
C      X        R     Point offset in arc sec from ref pixel in X
C      Y        R     Point offset in arc sec from ref pixel in Y
C   Outputs:
C      RADIUS   R     Separation from pointing position
C      IERR     I     Error code, 0=OK.
C-----------------------------------------------------------------------
      CHARACTER IN*(*)
      REAL      X, Y, RADIUS
      INTEGER   IERR
C
      REAL      XPIX, YPIX
      DOUBLE PRECISION XX, YY, ZZ, DDUMMY(1)
      INTEGER   IDEP(5), DIM(7), TYPE
      LOGICAL   SWAPOK
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'POSITION.INC'
      DATA SWAPOK /.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get info for input image if
C                                       necessary.
      IF ((IN.NE.IMAGE1) .OR. ((RA0.EQ.0.0D0) .AND. (DEC0.EQ.0.0D0)))
     *   THEN
         IMAGE1 = IN
C                                       Get catalog header
         CALL OBHGET (IN, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL FILL (5, 1, IDEP)
         LOCNUM = 1
         CALL SETLOC (IDEP, SWAPOK)
         ITI = 1
         IF (LABTYP(LOCNUM).LT.44) ITI = 2
         IF (LABTYP(LOCNUM).LT.22) ITI = 3
         IF ((AXTYP(LOCNUM).LT.1) .OR. (AXTYP(LOCNUM).GT.3)) THEN
            IERR = 2
            MSGTXT = 'CELESTIAL COORDINATES NOT IN FIRST AXES'
            CALL MSGWRT (7)
            GO TO 990
            END IF
         CALL IMDGET (IN, 'EPOCH', TYPE, DIM, EPOK, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL PSNGET (IN, 'OBSRA', TYPE, DIM, DDUMMY, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         RA0 = DDUMMY(1)
         CALL PSNGET (IN, 'OBSDEC', TYPE, DIM, DDUMMY, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         DEC0 = DDUMMY(1)
         IF ((ABS (RA0).LE.1.0D-10) .AND. (ABS (DEC0).LE.1.0D-10)) THEN
            MSGTXT = 'POINTING POSITION NOT IN HEADER'
            CALL MSGWRT (7)
            IF (CORTYP(LOCNUM).EQ.1) THEN
               RA0 = RPVAL(1,LOCNUM)
               DEC0 = RPVAL(2,LOCNUM)
            ELSE IF (CORTYP(LOCNUM).EQ.2) THEN
               RA0 = RPVAL(2,LOCNUM)
               DEC0 = RPVAL(1,LOCNUM)
            ELSE
               IERR = 1
               GO TO 990
               END IF
            END IF
C                                       Convert types if necessary
         IF (ITI.NE.1) THEN
            CALL COORDT (1, ITI, RA0, DEC0, EPOK, RA0, DEC0, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         RA0 = COND2R * RA0
         DEC0 = COND2R * DEC0
         END IF
C                                       convert to pixel
      LOCNUM = 1
      XPIX = RPLOC(1,LOCNUM) + X / AXINC(1,LOCNUM)
      YPIX = RPLOC(2,LOCNUM) + Y / AXINC(2,LOCNUM)
      CALL XYVAL (XPIX, YPIX, XX, YY, ZZ, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       fix transpositions
      IF (CORTYP(LOCNUM).EQ.2) THEN
         ZZ = YY
         YY = XX
         XX = YY
      ELSE IF (CORTYP(LOCNUM).EQ.3) THEN
         YY = ZZ
      ELSE IF (CORTYP(LOCNUM).EQ.4) THEN
         YY = XX
         XX = ZZ
      ELSE IF (CORTYP(LOCNUM).EQ.5) THEN
         XX = YY
         YY = ZZ
      ELSE IF (CORTYP(LOCNUM).EQ.6) THEN
         XX = ZZ
         END IF
      XX = COND2R * XX
      YY = COND2R * YY
      ZZ = SIN (YY) * SIN (DEC0) + COS (YY) * COS (DEC0) * COS (XX-RA0)
      RADIUS = ACOS (ZZ) / COND2R
      GO TO 999
C                                       Error
 990  MSGTXT = 'PSNANG: ERROR FINDING COORDINATES IN ' // IN
      CALL MSGWRT (7)
      IMAGE1 = ' '
C
 999  RETURN
      END
