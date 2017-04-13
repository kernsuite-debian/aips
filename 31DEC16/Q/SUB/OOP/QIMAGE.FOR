C   Image Class Q-routine using utility module
C-----------------------------------------------------------------------
C! Image class Q utilities (FFT, re-grid)
C# Task AP OOP IMAGE
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999-1998
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
C
C   IMGFFT (dir, in, out, ierr)
C      FFT an image, multi plane images are done one plane at a time.
C      IN and OUT should be labeled with DATATYPE 'REAL' or 'COMPLEX' as
C      desired.
C   IMGCVL (in1, in2, factor, out, ierr)
C      Convolves two images
C   FFTPAD (in, out, ierr)
C      Creates a scratch image suitable for FFTing an image and copies
C      the selected subset of the input image into the scratch image
C      with zero padding around the edges.  The scratch image is made
C      twice the size of the input image if possible.
C   IMGRGR (in, out, ierr)
C      Re-grids in based on images of the ra, dec coordinates
C
C-----------------------------------------------------------------------
      SUBROUTINE IMGFFT (DIR, IN, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   FFT an image, multi plane images are done one plane at a time.
C   IN and OUT should be labeled with DATATYPE 'REAL' or 'COMPLEX' as
C   desired.
C   Inputs:
C      DIR   I     Direction, 1 = forward, keep real
C                             2 = forward, keep amplitude
C                             3 = forward, keep complex
C                             -1 = reverse.
C      IN    C*?   The name of the input image object.
C      OUT   C*?   The name of the output image object
C   Output:
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      INTEGER   DIR, IERR
C
      INTEGER   IBLC(7), ITRC(7), INAX(7)
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open Images
      CALL IMGOPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGOPN (IN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Make Scratch file
      CALL IMGWIN (IN, IBLC, ITRC, INAX, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Only need one plane
      INAX(2) = INAX(2) + 5
      CALL FILL (5, 1, INAX(3))
      CALL IMGSCR ('SCRATCH FFT', INAX, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGOPN ('SCRATCH FFT', 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       FFT arrays
      CALL ARRFFT (DIR, IN, 'SCRATCH FFT', OUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close Images
      CALL IMGCLO (IN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGCLO ('SCRATCH FFT', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Delete Scratch file
      CALL IMGZAP ('SCRATCH FFT', IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE IMGCVL (IN1, IN2, FACTOR, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Convolves two images
C   image.
C   Inputs:
C      IN1    C*?   The name of the first input image.
C      IN2    C*?   The name of the second input image.
C      FACTOR R     Normalization factor (0.0 => 1.0)
C      OUT    C*?   The name of the output image.
C   Output:
C      IERR   I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN1*(*), IN2*(*), OUT*(*)
      REAL      FACTOR
      INTEGER   IERR
C
      INTEGER   INAX(7), IDIM(7), BLC(7), TRC(7), NAXISI(7), BLCI(7),
     *   TRCI(7), NX, NY, DUMMY
      REAL      FACTR
      CHARACTER DATYPE*8, CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Input image, copy to
C                                       scratch file with zero padding
C                                       for FFT.
      CALL FFTPAD (IN1, 'Copy 1', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get size of FFT for
C                                       normalization
      CALL IMGWIN ('Copy 1', BLC, TRC, INAX, IERR)
      IF (IERR.NE.0) GO TO 999
      NX = INAX(1)
      NY = INAX(2)
C                                       Make copy of file for FFT
C                                       output.
      CALL IMGCOP ('Copy 1', 'FFT 1', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default datatype = REAL
      IDIM(1) = 8
      IDIM(2) = 1
      IDIM(3) = 0
      DATYPE = 'COMPLEX'
      CALL IMPUT ('FFT 1', 'ARRAY.ARRAY_DESC.DATATYPE', OOACAR, IDIM,
     *   DUMMY, DATYPE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Convolving image, copy to
C                                       scratch file with zero padding
C                                       for FFT.
      CALL FFTPAD (IN2, 'Copy 2', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Make copy of file for FFT
C                                       output.
      CALL IMGCOP ('Copy 2', 'FFT 2', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMPUT ('FFT 2', 'ARRAY.ARRAY_DESC.DATATYPE', OOACAR, IDIM,
     *   DUMMY, DATYPE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Forward FFT
      CALL IMGFFT (3, 'Copy 1', 'FFT 1', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGFFT (3, 'Copy 2', 'FFT 2', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy FFT1 so it can be read and
C                                       written. (will use same files).
      CALL IMGCOP ('FFT 1', 'FFT 3', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Multiply
      CALL IMGMUL ('FFT 1', 'FFT 2', 'FFT 3', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       FFT back
      CALL IMGFFT (-1, 'FFT 3', 'Copy 1', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get size and window in original
C                                       Image
      CALL IMGWIN (IN1, BLCI, TRCI, NAXISI, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get window in intermediate
C                                       image.
      CALL COPY (7, BLCI, BLC)
      CALL COPY (7, TRCI, TRC)
      BLC(1) = NX/2 + 1 - NAXISI(1)/2
      TRC(1) = BLC(1) + NAXISI(1) - 1
      BLC(2) = NY/2 + 1 - NAXISI(2)/2
      TRC(2) = BLC(2) + NAXISI(2) - 1
      IDIM(1) = 7
      IDIM(2) = 1
      IDIM(3) = 0
      CALL IMPUT ('Copy 1', 'ARRAY.ARRAY_DESC.BLC', OOAINT, IDIM, BLC,
     *   CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMPUT ('Copy 1', 'ARRAY.ARRAY_DESC.TRC', OOAINT, IDIM, TRC,
     *   CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Scale
      FACTR = FACTOR
      IF (ABS (FACTR).LT.1.0E-20) FACTR = 1.0
C                                       Scale by the number of
C                                       pixels in the image FFTed.  Also
C                                       subimage to original field
      FACTR = NX * NY * FACTR
      CALL IMGSCL ('Copy 1', FACTR,  OUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Delete temporary objects and
C                                       scratch files.
      CALL IMGZAP ('Copy 2', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGZAP ('Copy 1', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGDES ('FFT 1', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGDES ('FFT 2', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGDES ('FFT 3', IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE FFTPAD (IN, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Creates a scratch image suitable for FFTing an image and copies the
C   selected subset of the input image into the scratch image with zero
C   padding around the edges.  The scratch image is made twice the size
C   of the input image if possible.
C   Inputs:
C      IN     C*?   The name of the input image.
C      OUT    C*?   The name of the output scratch image.
C   Output:
C      IERR   I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      INTEGER   IERR
C
      INTEGER   INAX(7), IBLC(7), ITRC(7), IDIM(7), LOOP, ITEMP
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PMAD.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Scratch files
      CALL IMGATT (IN, .FALSE., IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGWIN (IN, IBLC, ITRC, INAX, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Make large enough for padding to
C                                       next highest power of 2
      DO 200 LOOP = 1,2
         ITEMP = (LOG (1.0*INAX(LOOP)) / LOG (2.0)) + 0.999
         ITEMP = 2 ** (ITEMP+1)
         INAX(LOOP) = MIN (MAXIMG, ITEMP)
 200     CONTINUE
      INAX(2) = INAX(2) + 5
      CALL IMGSCR (OUT, INAX, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Reset to image size
      INAX(2) = INAX(2) - 5
      IDIM(1) = 7
      IDIM(2) = 1
      IDIM(3) = 0
      CALL IMGOPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMPUT (OUT, 'ARRAY.ARRAY_DESC.NAXIS', OOAINT, IDIM, INAX,
     *   CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Subimage and padding
      CALL IMGPAD (IN, OUT, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE IMGRGR (INIM, OUTIM, IERR)
C-----------------------------------------------------------------------
C   Public
C   Makes an RA-dec image of the data contained in INIM (last 2 planes
C   of which are images of the desired coordinates of the pixels)
C   Inputs:
C      INIM     C*(*)    Name of UV data input object
C      OUTIM    C*(*)    Name of image output object
C   Outputs:
C      IERR     I        Error code: 0 okay
C-----------------------------------------------------------------------
      CHARACTER INIM*(*), OUTIM*(*)
      INTEGER   IERR
C
      INTEGER   TYPE, DIM(7), MSGSAV, DISKI, CNOI, SCRNO, LUNL,
     *   LERR, DISKO, CNOO, IMSI(2), BUFNO1, BUFNO2, BUFNO3, LUNO,
     *   BUFNO4, LBUFSZ, BLC(7), TRC(7), CATIN(256), NAXIS(7)
      REAL      RWT, CELLSZ(2), CATMAR(256), MAXCWT, XNLIM, BEMSZ(2),
     *   CATINR(256)
      CHARACTER TEMPUV*32, TEMPIM*32, CDUMMY*1
      LOGICAL   APOPEN
      INCLUDE 'INCS:DSDG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      EQUIVALENCE (CATMAP, CATMAR)
      EQUIVALENCE (CATIN, CATINR)
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
      APOPEN = .FALSE.
      LBUFSZ = 2 * BUFSIZ
C                                       Open for buffer, update info
      CALL IMGOPN (INIM, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBHGET (INIM, CATIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCLO (INIM, IERR)
      IF (IERR.NE.0) GO TO 990
      BEMSZ(1) = CATINR(KRBMJ) * 3600.
      BEMSZ(2) = CATINR(KRBMN) * 3600.
C                                       Disk, CNO
C                                       DFIL.INC scratch file number.
      MSGSUP = 32000
      CALL IMGET (INIM, 'SCRCNO', TYPE, DIM, SCRNO, CDUMMY, IERR)
      MSGSUP = MSGSAV
C                                       Cataloged file
      IF (IERR.EQ.1) THEN
         CALL OBDSKC (INIM, DISKI, CNOI, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Scratch file
      ELSE IF (IERR.EQ.0) THEN
         DISKI = 0
         CNOI = SCRNO
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Check axis description
C                                       Get size and window in original
C                                       Image
      CALL IMGWIN (INIM, BLC, TRC, NAXIS, IERR)
      IF (IERR.NE.0) GO TO 999
      TRC(3) = MIN (TRC(3), CATIN(KINAX+2)-2)
      NAXIS(3) = TRC(3) - BLC(3) + 1
      DIM(1) = 7
      DIM(2) = 1
      CALL ARDPUT (INIM, 'BLC', OOAINT, DIM, BLC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARDPUT (INIM, 'TRC', OOAINT, DIM, TRC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       STEP 2: create the image
C                                       build header
      CALL I2IDES (INIM, OUTIM, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Create
      CALL OOPEN (OUTIM, 'DEST', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       temps for more buffers
      TEMPUV = 'Temporary buffer for IMGRGR'
      CALL OBCREA (TEMPUV, 'INIM  ', IERR)
      IF (IERR.NE.0) GO TO 995
      TEMPIM = 'Temporary buffer for Osdimg'
      CALL OBCREA (TEMPIM, 'OUTIM   ', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Open for buffers
      CALL OBOPEN (OUTIM, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBOPEN (INIM, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBOPEN (TEMPUV, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBOPEN (TEMPIM, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get buffer numbers
      CALL OBINFO (OUTIM, BUFNO1, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (INIM, BUFNO2, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (TEMPUV, BUFNO3, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (TEMPIM, BUFNO4, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Disk, CNO
C                                       DFIL.INC scratch file number.
      MSGSUP = 32000
      CALL IMGET (OUTIM, 'SCRCNO', TYPE, DIM, SCRNO, CDUMMY, IERR)
      MSGSUP = MSGSAV
C                                       Cataloged file
      IF (IERR.EQ.1) THEN
         CALL OBDSKC (OUTIM, DISKO, CNOO, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Scratch file
      ELSE IF (IERR.EQ.0) THEN
         DISKO = 0
         CNOO = SCRNO
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       STEP 3: init the common
C                                        SD type, cutoff
      CALL OGET (OUTIM, 'SDCUTOFF', TYPE, DIM, RWT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (OUTIM, 'IMSIZE', TYPE, DIM, IMSI, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      NX = IMSI(1)
      NY = IMSI(2)
      CALL OGET (OUTIM, 'CELLSIZE', TYPE, DIM, CELLSZ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (OUTIM, 'CTYPX', TYPE, DIM, CXTYPE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (OUTIM, 'CTYPY', TYPE, DIM, CYTYPE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (OUTIM, 'XPARM', TYPE, DIM, XPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (OUTIM, 'YPARM', TYPE, DIM, YPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Catalog header
      CALL OBHGET (OUTIM, CATMAP, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       apply defaults and store back
      CALL GRDFIX (CXTYPE, CYTYPE, XPARM, YPARM, CELLSZ, BEMSZ)
      DIM(1) = 1
      CALL OPUT (OUTIM, 'CTYPX', OOAINT, DIM, CXTYPE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (OUTIM, 'CTYPY', OOAINT, DIM, CYTYPE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      DIM(1) = 10
      CALL OPUT (OUTIM, 'XPARM', OOARE, DIM, XPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (OUTIM, 'YPARM', OOARE, DIM, YPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       STEP 4: do it
      CALL APOBJ ('OPEN', 'IMGRGR', IERR)
      IF (IERR.NE.0) GO TO 990
      APOPEN = .TRUE.
      CALL OBLUN (LUNL, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBLUN (LUNO, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL AEGRID (LUNO, LUNL, DISKI, CNOI, DISKO, CNOO, RWT, BLC, TRC,
     *   CATIN, LBUFSZ, OBUFFR(1,BUFNO1), OBUFFR(1,BUFNO2), LBUFSZ,
     *   OBUFFR(1,BUFNO3), LBUFSZ, OBUFFR(1,BUFNO4), MAXCWT, XNLIM,
     *   IERR)
      CALL OBLUFR (LUNL)
      CALL OBLUFR (LUNO)
      CALL APOBJ ('FREE', 'IMGRGR', LERR)
      APOPEN = .FALSE.
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (OUTIM, 'MAXCWT', OOARE, DIM, MAXCWT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (OUTIM, 'XNLIM', OOARE, DIM, XNLIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ARSPUT (OUTIM, 'DATAMAX', OOARE, DIM, CATMAR(KRDMX), CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ARSPUT (OUTIM, 'DATAMIN', OOARE, DIM, CATMAR(KRDMN), CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ARDPUT (OUTIM, 'BLANK', OOARE, DIM, CATMAR(KRBLK), CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 990
C                                       copy keywords
      IF (DISKO.LE.0) THEN
         DISKO = SCRVOL(CNOO)
         CNOO = SCRCNO(CNOO)
         END IF
      IF (DISKI.LE.0) THEN
         DISKI = SCRVOL(CNOI)
         CNOI = SCRCNO(CNOI)
         END IF
      CALL KEYCOP (DISKI, CNOI, DISKO, CNOO, LERR)
C                                       clear destroy status
      CALL IMCDES (OUTIM, 'WRIT', LERR)
C                                       close other buffers
      CALL OBCLOS (INIM, LERR)
      CALL OBCLOS (TEMPUV, LERR)
      CALL OBCLOS (TEMPIM, LERR)
C                                       destroy temporary object
      CALL OBFREE (TEMPUV, LERR)
      CALL OBFREE (TEMPIM, LERR)
      GO TO 999
C                                       Reveal suppressed message
 995  MSGSUP = MSGSAV
      CALL MSGWRT (7)
 990  MSGSUP = MSGSAV
      IF (APOPEN) THEN
         CALL QRLSE
         CALL APOBJ ('FREE', 'IMGRGR', LERR)
         END IF
      MSGTXT = 'IMGRGR: MAKING OUTIM ' // OUTIM
      CALL MSGWRT (7)
      MSGTXT = 'IMGRGR: FROM ' // INIM
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE I2IDES (INIM, OUTIM, IERR)
C-----------------------------------------------------------------------
C   Private
C   Copies descriptive info from a Uvdata object to an image and
C   initializes the image descriptors.
C   Inputs:
C      INIM    C*?  Name of input image paraform object.
C      OUTIM   C*?  Name of output Image object.
C   Inputs from OUTIM object:
C      IMSIZE   I(2) Image size in pixels
C      CELLSIZE R(2) Cell size in arc seconds.
C      SHIFT    R(2) Shift in arcsec
C   Inputs from INIM object: (defaults enforced).
C      BIF      I    First IF selected
C      EIF      I    Highest IF selected.
C      BCHAN    I    First channel selected
C      ECHAN    I    Highest channel selected.
C      CHINC    I    Channel increment
C      STOKES   C*4  Stokes selected
C   Output:
C      IERR    I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER INIM*(*), OUTIM*(*)
      INTEGER   IERR
C
      INTEGER   NDESC
C                                       NDESC = number of descriptors to
C                                       copy.
      PARAMETER (NDESC = 7)
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   TYPE, DIM(7), DUMMY, I, IMSIZE(2), MSGSAV, DEPTH(5),
     *   BLC(7), TRC(7), SEQO, CATOI(256)
      REAL      CELSIZ(2), SHIFT(2), XSHFT, YSHFT, XPIX, YPIX, RBLC(7),
     *   RTRC(7), CATOR(256)
      DOUBLE PRECISION  RA, DEC, DG2RAD, COORD(2), CATOD(128)
      HOLLERITH CATOH(256)
      CHARACTER CDUMMY*1, PROJ*4
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (CATOI, CATOR, CATOH, CATOD)
C-----------------------------------------------------------------------
      IERR = 0
      MSGSAV = MSGSUP
C                                       Open and close uvdata to fully
C                                       define object.
      CALL IMGOPN (INIM, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBHGET (INIM, CATOI, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMGCLO (INIM, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (INIM, 'BLC', TYPE, DIM, BLC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OGET (INIM, 'TRC', TYPE, DIM, TRC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       build output header
      DO 10 I = 1,7
         RBLC(I) = BLC(I)
         RTRC(I) = TRC(I)
 10      CONTINUE
      CALL COPY (256, CATOI, CATBLK)
      CALL SUBHDR (RBLC, RTRC, 1.0, 1.0)
      CALL COPY (256, CATBLK, CATOI)
C                                       sequence number
      CALL OGET (OUTIM, 'IMSEQ', TYPE, DIM, SEQO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CATOI(KIIMS) = SEQO
C                                       First 2 axes
      MSGSUP = 32000
      CALL OGET (OUTIM, 'CPROJ', TYPE, DIM, DUMMY, PROJ, IERR)
      MSGSUP = MSGSAV
      IF ((IERR.EQ.0) .AND. (PROJ(1:1).EQ.'-')) THEN
         CALL CHR2H (4, PROJ, 5, CATOH(KHCTP))
         CALL CHR2H (4, PROJ, 5, CATOH(KHCTP+2))
         END IF
C                                       Image size
      MSGSUP = 32000
      CALL OGET (OUTIM, 'IMSIZE', TYPE, DIM, IMSIZE, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CALL FILL (2, 0, IMSIZE)
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       Cellsize
      MSGSUP = 32000
      CALL OGET (OUTIM, 'CELLSIZE', TYPE, DIM, CELSIZ, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CALL RFILL (2, 0.0, CELSIZ)
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       SHIFT
      MSGSUP = 32000
      CALL OGET (OUTIM, 'SHIFT', TYPE, DIM, SHIFT, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         SHIFT(1) = 0.0
         SHIFT(2) = 0.0
         DIM(1) = 2
         DIM(2) = 1
         DIM(3) = 0
         CALL OPUT (OUTIM, 'SHIFT', OOARE, DIM, SHIFT, CDUMMY, IERR)
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       Coordinate
      MSGSUP = 32000
      CALL OGET (OUTIM, 'CCENTER', TYPE, DIM, COORD, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         COORD(1) = 0.0D0
         COORD(2) = 0.0D0
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       defaults
      IF (IMSIZE(1).LT.32) IMSIZE(1) = CATOI(KINAX)
      IF (IMSIZE(2).LT.32) IMSIZE(2) = CATOI(KINAX+1)
      IF (CELSIZ(1).LE.0.0) CELSIZ(1) = ABS (CATOR(KRCIC)) * 3600.0
      IF (CELSIZ(2).LE.0.0) CELSIZ(2) = ABS (CATOR(KRCIC+1)) * 3600.0
      IF ((COORD(1).EQ.0.0D0) .AND. (COORD(2).EQ.0.0)) THEN
         COORD(1) = CATOD(KDORA)
         COORD(2) = CATOD(KDODE)
         END IF
      DIM(1) = 2
      DIM(2) = 1
      CALL OPUT (OUTIM, 'IMSIZE', OOAINT, DIM, IMSIZE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (OUTIM, 'CELLSIZE', OOARE, DIM, CELSIZ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OPUT (OUTIM, 'CCENTER', OOADP, DIM, COORD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Ra Axis
      XSHFT = SHIFT(1) / 3600.
      CATOI(KINAX) = IMSIZE(1)
      CATOD(KDCRV) = COORD(1)
      CATOR(KRCIC) = - ABS (CELSIZ(1)) / 3600.0
      CATOR(KRCRP) = IMSIZE(1)/2 - XSHFT / CATOR(KRCIC)
      CATOR(KRCRT) = 0.0
C                                       Dec Axis
      YSHFT = SHIFT(2) / 3600.
      CATOI(KINAX+1) = IMSIZE(2)
      CATOD(KDCRV+1) = COORD(2)
      CATOR(KRCIC+1) = ABS (CELSIZ(2)) / 3600.0
      CATOR(KRCRP+1) = IMSIZE(2)/2 + 1 - YSHFT / CATOR(KRCIC+1)
      CATOR(KRCRT+1) = 0.0
C                                       Set for shift
      CALL OBHPUT (OUTIM, CATOI, IERR)
      IF (IERR.NE.0) GO TO 990
      DG2RAD = 1.745329252D-2
      RA = COORD(1)
      DEC = COORD(2)
C                                       SHIFT is -GLS type now
      XSHFT = - XSHFT / COS (DEC * DG2RAD)
      DIM(1) = 1
      CALL PSNPUT (OUTIM, 'XSHIFT', OOARE, DIM, XSHFT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL PSNPUT (OUTIM, 'YSHIFT', OOARE, DIM, YSHFT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       determine ref pixel for
C                                       conventional ref value
      IF ((PROJ.EQ.'-GLS') .OR. (PROJ.EQ.'-AIT') .OR. (PROJ.EQ.'-MER'))
     *   THEN
         CALL FILL (5, 1, DEPTH)
         CALL OBHGET (OUTIM, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 990
         CATD(KDCRV+1) = 0.0D0
         IF ((LOCNUM.LE.0) .OR. (LOCNUM.GT.NUMLOC)) LOCNUM = 1
         CALL SETLOC (DEPTH, .TRUE.)
         CALL XYPIX (RA, DEC, XPIX, YPIX, IERR)
         IF (IERR.EQ.0) THEN
            CATR(KRCRP)   = 2.0 * CATR(KRCRP)   - XPIX
            CATR(KRCRP+1) = 2.0 * CATR(KRCRP+1) - YPIX
            CALL OBHPUT (OUTIM, CATBLK, IERR)
            IF (IERR.NE.0) GO TO 990
         ELSE
            CATD(KDCRV+1) = DEC
            IERR = 0
            END IF
         END IF
      GO TO 999
C                                       Error
 990  MSGTXT = 'I2IDES: ERROR SPECIFYING ' // OUTIM
      CALL MSGWRT (7)
C
 999  RETURN
      END
