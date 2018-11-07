C    ARRAY Class "Q" routine Module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "ARRAY" class "Q" routine library
C# Map-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2006, 2008
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
C   The "Q" routine library for the "ARRAY" object class contains only 1
C   module to perform Fast Fourier Transforms on arrays.
C
C   Public functions:
C      ARRFFT (direction, input, scratch, output, ierr)
C         FFT an array, multi plane arrays are done one plane at a time.
C-----------------------------------------------------------------------
      SUBROUTINE ARRFFT (DIR, IN, SCR, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   FFT an array, multi plane arrays are done one plane at a time.
C   Inputs:
C      DIR   I     Direction, 1 = forward, keep real
C                             2 = forward, keep amplitude
C                             3 = forward, keep complex
C                             -1 = reverse.
C      IN    C*?   The name of the input object.
C      SCR   C*?   The name of a scratch object, large enough for 1
C                  plane.
C      OUT   C*?   The name of the output object
C   Output:
C      IERR  I     Error return code, 0=OK, else failed
C-----------------------------------------------------------------------
      CHARACTER IN*(*), SCR*(*), OUT*(*)
      INTEGER   DIR, IERR
C
      INTEGER   BLC(7), TRC(7), NAXISI(7), NAXISO(7), LOOP3, LOOP4,
     *   LOOP5, LOOP6, LOOP7, NDIM, DIM(7), TYPE, BUFNOI, BUFNOO,
     *   BUFSCR, LUN(3), VOL(3), BO(3), NX, NY, JERR, DEP(7), DUMMY,
     *   I1, I2, I3, JBUFSZ
      CHARACTER DATYPE*8, FIL(3)*48, BADOBJ*32, OUTYPE*8, CDUMMY*1
      REAL      BLANK, FMAX, FMIN, SMAX, SMIN
      LOGICAL   INCPX, OUTCPX, ISBLNK, FULL, APOPEN
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DAPM.INC'
C-----------------------------------------------------------------------
      IERR = 0
      APOPEN = .FALSE.
C                                       Open array and get I/O stream
C                                       pointers
      CALL ARROPN (IN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (IN, BUFNOI, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Scratch array
      BADOBJ = SCR
      CALL ARROPN (SCR, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (SCR, BUFSCR, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Output
      BADOBJ = OUT
      CALL ARROPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (OUT, BUFNOO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       IN Complex?
      CALL ARDGET (IN, 'DATATYPE', TYPE, DIM, DUMMY, DATYPE, IERR)
      INCPX = (DATYPE.EQ.'COMPLEX') .AND. (IERR.EQ.0)
      IERR = 0
C                                       OUT Complex?
      CALL ARDGET (OUT, 'DATATYPE', TYPE, DIM, DUMMY, DATYPE, IERR)
      OUTCPX = (DATYPE.EQ.'COMPLEX') .AND. (IERR.EQ.0)
C                                       Scratch array must be complex
      DATYPE = 'COMPLEX '
      BADOBJ = SCR
      CALL ARDPUT (SCR, 'DATATYPE', OOACAR, DIM, DUMMY, DATYPE, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Full or real to Complex
      FULL = (OUTCPX.AND.INCPX)
C                                       Get dimension of input array
      BADOBJ = IN
      CALL ARRWIN (IN, BLC, TRC, NAXISI, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       (NX, NY) are the dimensions of
C                                       the image plane in pixels
      IF (DIR.LT.0) THEN
C                                       Reverse (uv->image)
         IF (OUTCPX) THEN
            NX = NAXISI(2)
            NY = NAXISI(1)
         ELSE
            NX = (NAXISI(2)-1) * 2
            NY = NAXISI(1)
            END IF
      ELSE
C                                       Forward (image->uv)
         IF (INCPX) THEN
            NX = NAXISI(1) / 2
         ELSE
            NX = NAXISI(1)
            END IF
         NY = NAXISI(2)
         END IF
C                                       Set output image Geometry
      BADOBJ = OUT
      CALL ARDGET (OUT, 'NAXIS', TYPE, DIM, NAXISO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Depends on type of transform
      IF (FULL) THEN
         NAXISO(1) = NAXISI(2)
         NAXISO(2) = NAXISI(1)
C                                       Reverse transform
      ELSE IF (DIR.LT.0) THEN
         NAXISO(1) = NX
         NAXISO(2) = NY
      ELSE
C                                       Forward
         IF (INCPX) THEN
            NAXISO(2) = NX
         ELSE
            NAXISO(2) = NX / 2 + 1
            END IF
C                                       keep real or amplitude
         NAXISO(1) = NY
C                                       keep complex
         IF (DIR.EQ.3) NAXISO(1) = NY
         END IF
      CALL ARDPUT (OUT, 'NAXIS', OOAINT, DIM, NAXISO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Clear output window
      CALL ARRCWI (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Output real or complex?
      IF (FULL .OR. (DIR.EQ.3)) THEN
         OUTYPE = 'COMPLEX '
      ELSE
         OUTYPE = 'REAL '
         END IF
      DIM(1) = 8
      CALL ARDPUT (OUT, 'DATATYPE', OOACAR, DIM, DUMMY, OUTYPE, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Blanking?
      BADOBJ = IN
      CALL ARDGET (IN, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = (BLANK.NE.0.0) .AND. (IERR.EQ.0)
C                                       Not allowed
      IF (ISBLNK) THEN
         IERR = 2
         MSGTXT = 'FFT OF BLANKED ARRAYS NOT ALLOWED'
         GO TO 990
         END IF
      IERR = 0
C                                       Number of dimensions
      BADOBJ = IN
      CALL ARDGET (IN, 'NDIM', TYPE, DIM, NDIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get dimension of output array
      BADOBJ = OUT
      CALL ARRWIN (OUT, BLC, TRC, NAXISO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Set disk numbers
      BADOBJ = IN
      CALL ARDGET (IN, 'FDISK', TYPE, DIM, VOL(1), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      BADOBJ = SCR
      CALL ARDGET (SCR, 'FDISK', TYPE, DIM, VOL(2), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      BADOBJ = OUT
      CALL ARDGET (OUT, 'FDISK', TYPE, DIM, VOL(3), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       LUNs
      CALL OBLUN (LUN(1), IERR)
      CALL OBLUN (LUN(2), IERR)
      CALL OBLUN (LUN(3), IERR)
C                                       Filenames
      BADOBJ = IN
      CALL ARDGET (IN, 'FNAME', TYPE, DIM, DUMMY, FIL(1), IERR)
      IF (IERR.NE.0) GO TO 995
      BADOBJ = SCR
      CALL ARDGET (SCR, 'FNAME', TYPE, DIM, DUMMY, FIL(2), IERR)
      IF (IERR.NE.0) GO TO 995
      BADOBJ = OUT
      CALL ARDGET (OUT, 'FNAME', TYPE, DIM, DUMMY, FIL(3), IERR)
      IF (IERR.NE.0) GO TO 995
      SMAX = -1.0E20
      SMIN = 1.0E20
C                                       take the AP
      I1 = MAX (NAXISI(1)*NAXISI(2), NAXISO(1)*NAXISO(2))
      I1 = (2 * I1) / 1024
      CALL QINIT (I1, I2, I3)
      IF ((I3.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         MSGTXT = 'ARRFFT COULD NOT GET NEEDED MEMORY'
         CALL MSGWRT (8)
         IERR = 8
         GO TO 995
         END IF
      CALL APOBJ ('OPEN', 'ARRFFT', IERR)
      IF (IERR.NE.0) GO TO 995
      APOPEN = .TRUE.
C                                       Loop over planes
      DO 700 LOOP7 = 1,NAXISI(7)
         DEP(7) = LOOP7
         DO 600 LOOP6 = 1,NAXISI(6)
            DEP(6) = LOOP6
            DO 500 LOOP5 = 1,NAXISI(5)
               DEP(5) = LOOP5
               DO 400 LOOP4 = 1,NAXISI(4)
                  DEP(4) = LOOP4
                  DO 300 LOOP3 = 1,NAXISI(3)
                     DEP(3) = LOOP3
C                                       Block offset for IN and OUT
      CALL COMOFF (NDIM, NAXISI, DEP(3), BO(1), JERR)
      BO(1) = BO(1) + 1
      BO(2) = 1
      CALL COMOFF (NDIM, NAXISO, DEP(3), BO(3), JERR)
      BO(3) = BO(3) + 1
C                                       FFT
      BADOBJ = 'Doing the FFT'
      JBUFSZ = 2 * BUFSIZ
      CALL PASS1 (DIR, FULL, LUN, VOL, FIL, BO, OBUFFR(1,BUFNOI),
     *   JBUFSZ, OBUFFR(1,BUFNOO), JBUFSZ, NX, NY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL PASS2 (DIR, FULL, LUN, VOL, FIL, BO, OBUFFR(1,BUFNOI),
     *   JBUFSZ, OBUFFR(1,BUFNOO), JBUFSZ, NX, NY, FMAX, FMIN, IERR)
      IF (IERR.NE.0) GO TO 995
      SMAX = MAX (SMAX, FMAX)
      SMIN = MIN (SMIN, FMIN)
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       free AP
      CALL QRLSE
      CALL APOBJ ('FREE', 'ARRFFT', I1)
      APOPEN = .FALSE.
C                                       Free LUNs
      CALL OBLUFR (LUN(1))
      CALL OBLUFR (LUN(2))
      CALL OBLUFR (LUN(3))
C                                       Save MAX, MIN
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      BADOBJ = OUT
      CALL ARSPUT (OUT, 'DATAMAX', OOARE, DIM, SMAX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARSPUT (OUT, 'DATAMIN', OOARE, DIM, SMIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Close arrays
      BADOBJ = IN
      CALL ARRCLO (IN, IERR)
      IF (IERR.NE.0) GO TO 995
      BADOBJ = SCR
      CALL ARRCLO (SCR, IERR)
      IF (IERR.NE.0) GO TO 995
      BADOBJ = OUT
      CALL ARRCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
 995  IF (APOPEN) THEN
         CALL QRLSE
         CALL APOBJ ('FREE', 'ARRFFT', I1)
         END IF
      MSGTXT = 'ARRFFT: PROBLEM FFTING ' // IN
      CALL MSGWRT (7)
      MSGTXT = 'ARRFFT: PROBLEM WITH ' // BADOBJ
      CALL MSGWRT (7)
C
 999  RETURN
      END
