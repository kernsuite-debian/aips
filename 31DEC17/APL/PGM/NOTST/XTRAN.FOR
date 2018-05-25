LOCAL INCLUDE 'XTRAN.INC'
C                                       Local include for XTRAN
      INTEGER   NPNTS, NLINES, X0, Y0, NLSQ, NPAR
      REAL      XSEQI, XDISKI, XSEQO, XDISKO, BLC(7), TRC(7), XPAR,
     *   DOUTP, CSN, SN, DX, DY, XINC, YINC, DMAX, DMIN, ANGL
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2), XINFIL(12)
      CHARACTER INFILE*48, INSTR*36, OUTSTR*36
      DOUBLE PRECISION A(6), B(6)
      COMMON /PARAM/ XNAMEI, XCLAIN, XSEQI, XDISKI, XNAMOU, XCLAOU,
     *   XSEQO, XDISKO, BLC, TRC, XPAR, DOUTP, XINFIL
      COMMON /RTN1/ CSN, SN, DX, DY, DMAX, DMIN, ANGL, NPAR
      COMMON /RTN2/ NPNTS, NLINES, X0, Y0
      COMMON /QUAD/ A, B, XINC, YINC, NLSQ
      COMMON /CHRCOM/ INFILE, INSTR, OUTSTR
LOCAL END
      PROGRAM XTRAN
C-----------------------------------------------------------------------
C! Find and apply coordinate transformation given pixel positions.
C# Map Coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999-2000, 2004-2005, 2008-2010, 2012
C;  Copyright (C) 2015
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
C   Generalized coordinate transform routine.  Adapted from program
C   by C.B. Opal.
C   INPUTS:  Adverbs from AIPS
C     INNAME    H(3)   Input image name.
C     INCLASS   H(2)   Input image class.
C     INSEQ     R   Input image sequence.
C     INDISK    R   Disk number for input image.
C     OUTNAME   H(3)   Name of output image.
C     OUTCLASS  H(2)   Class of the output image.
C     OUTSEQ    R   Sequence number of the output image.
C     OUTDISK   R   Disk number for output image.
C     BLC       R(7)   Bottom left corner of input cube. 0=> 1,1,1
C     TRC       R(7)   Top right corner of input cube. 0=> max.
C     NPOINTS   R   Number of parameters (3 or 6) to be fit.
C     DOUTP     R   If true, do transform.
C     STRC2     H(12) Name of file with standard star postions.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   INLUN, OUTLUN, IHOTL, IHINL, IERR, BUF1(256), BUF2(256)
      LONGINT   NEWOFF, WTOFF
      REAL      NEWIM(2), NEWWT(2)
      INCLUDE 'XTRAN.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA PRGNAM /'XTRAN '/
      DATA INLUN, OUTLUN, IHOTL, IHINL /16,17,27,28/
C-----------------------------------------------------------------------
      DMAX = 0.0
C                                       Set up, get parms, open, create
C                                       all files.
      CALL XTRINI (PRGNAM, INLUN, OUTLUN, NEWIM, NEWOFF, NEWWT, WTOFF,
     *   IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Check coordinates
      IF ((DOUTP.LE.0.0) .OR. (DOUTP.GT.1.5)) CALL XTRCHK (IERR)
C                                       Transform coordinates.
      IF (DOUTP.GT.0.0) THEN
         CALL ROTATE (INLUN, OUTLUN, NPNTS, NLINES, NEWIM(1+NEWOFF),
     *      NEWWT(1+WTOFF), IERR)
         IF (IERR.NE.0) GO TO 980
C                                       New max, min
         CALL GETHDR (INLUN, CATBLK, IERR)
         CALL OPENCF (OUTLUN, OUTSTR, IERR)
         CALL GETHDR (OUTLUN, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 980
         CATR(KRDMX) = DMAX
         CATR(KRDMN) = DMIN
         CALL SAVHDR (OUTLUN, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Create, copy, update hist file.
         CALL HISTRY (PRGNAM, INLUN, OUTLUN, IHINL, IHOTL, BUF1, BUF2)
         END IF
C                                       Release AIPS if necessary,
C                                       close, clear files, delete any
C                                       new files if error.
 980  CALL TSKEND (IERR)
C
 999  STOP
      END
      SUBROUTINE XTRINI (PRGNAM, INLUN, OUTLUN, NEWIM, NEWOFF, NEWWT,
     *   WTOFF, IERR)
C-----------------------------------------------------------------------
C   XTRINI will initialize the WAWA IO package, open the input image,
C   create the output image , determine the name strings of the
C   output image and initialize the output image.
C   Inputs:
C      PRGNAM   C*6    Program name.
C      INLUN    I      Input image logical unit numbers.
C      OUTLUN   I      New image input logical unit number.
C      NEWIM    R(*)   Base address of output image
C      NEWWT    R(*)   Base address of weight image
C   Outputs:
C      NEWOFF   LI     Offset to allocated memory
C      WTOFF    LI     Offset to allocated memory
C      IERR     I      Standard WAWA IO error message.
C   (in common block)
C      BLC      R(7)   bottom left hand corner of input map.
C      TRC      R(7)   top right hand corner of input map.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      REAL      NEWIM(*), NEWWT(*)
      INTEGER   INLUN, OUTLUN, IERR
      LONGINT   NEWOFF, WTOFF
C
      CHARACTER OUTCL*6
      INTEGER   NOPARM, NWORDS, IROUND
      HOLLERITH MAP
      REAL      XUSID
      INCLUDE 'XTRAN.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      NOPARM = 42
      CALL TSKBEG (PRGNAM, NOPARM, XNAMEI, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL H2CHR (48, 1, XINFIL, INFILE)
      XUSID = NLUSER
C                                       Fix the input image name string
      CALL CHR2H (4, 'MA  ', 1, MAP)
      CALL H2WAWA (XNAMEI, XCLAIN, XSEQI, MAP, XDISKI, XUSID, INSTR)
C                                       Open the old file. Get header.
      CALL OPENCF (INLUN, INSTR, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GETHDR (INLUN, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Fix BLC, TRC.
      CALL HDRWIN (BLC, TRC, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      NPNTS = TRC(1) - BLC(1) + 1
      NLINES = TRC(2) - BLC(2) + 1
      X0 = 0.5*NPNTS
      Y0 = 0.5*NLINES
      NPAR = IROUND (XPAR)
      IF (NPAR.EQ.6) THEN
         IF (ABS(DOUTP).GT.1.5) THEN
            MSGTXT = 'Warning: ORDER=6 not appropriate for simple'
     *         // ' header change'
            CALL MSGWRT (6)
            END IF
      ELSE IF (NPAR.NE.3) THEN
         NPAR = 3
         MSGTXT = 'ORDER SET TO 3'
         CALL MSGWRT (6)
         END IF
C                                       Fill in defaults for INSTR.
      CALL GTNAME (INLUN, INSTR, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Check out class
      CALL H2CHR (6, 1, XCLAOU, OUTCL)
C                                       If blank use program name
      IF (OUTCL.EQ.' ') OUTCL = PRGNAM
C                                       Pack class back
      CALL CHR2H (6, OUTCL, 1, XCLAOU)
C                                       Set up output name string.
      CALL H2WAWA (XNAMOU, XCLAOU, XSEQO, MAP, XDISKO, XUSID, OUTSTR)
      CALL FILDEF (INSTR, OUTSTR)
C                                       Find coordinate transform
      CALL XY2AD (IERR)
      IF (IERR.NE.0) GO TO 990
C                                       If not making new image, exit
      IF (DOUTP.GT.0.0) THEN
C                                       allocate memory for output
         NWORDS = NPNTS * NLINES
         IF (DOUTP.GT.1.5) NWORDS = (2 * NPNTS - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', TSKNAM, NWORDS, NEWIM, NEWOFF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL ZMEMRY ('GET ', TSKNAM, NWORDS, NEWWT, WTOFF, IERR)
         IF (IERR.NE.0) GO TO 999
C                                      No history file
         CATR(KRBLK) = FBLANK
         CALL CATCLR (CATBLK)
C                                       Create output image.
         CALL MAPCR (OUTSTR, OUTSTR, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
      GO TO 999
C                                       error handling.
 970  CALL PRTERR (IERR, OUTLUN)
      GO TO 999
 980  CALL PRTERR (IERR, INLUN)
      GO TO 999
 990  CALL PRTERR (IERR, 0)
C
 999  RETURN
      END
      SUBROUTINE XY2AD (IERR)
C-----------------------------------------------------------------------
C   Finds the xy to RA,Dec transformation using a user supplied list
C   of standard star positions.  The function that is fit is a linear
C   function (NPAR=3) or a linear plus quadratic (NPAR=6) in the tangent
C   plane.  Axis increments, rotation angle, and axis offets are found.
C   If the user requests the transformation to be applied, these values
C   are put in the image header.
C   Inputfrom common:
C      INFILE   C*48   Name of file in with star postions.
C   Output:
C      IERR     I      Error return code
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   MXSTAR
      PARAMETER (MXSTAR=1000)
      CHARACTER LINE*80, AXRA*4, AXDEC*4, EXTEND*4, NAME(MXSTAR)*20,
     *   DECSIN*1, PTYPE*4, INNAME*20
      INTEGER   LUNSTR, I, IVOL, NSEQ, IVER, NSTDS, FND, ICHR, IER
      REAL      INXY(2), XY(2,MXSTAR), AD(2,MXSTAR), RES(2,MXSTAR),
     *   HR(3), DEG(3)
      DOUBLE PRECISION REFVAL(2), REFINC(2), REFPIX(2), ANGLE
      INCLUDE 'XTRAN.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNSTR /10/
      DATA AXRA /'RA--'/, AXDEC /'DEC-'/, EXTEND /'-TAN'/
C-----------------------------------------------------------------------
      IVOL = 1
      NSEQ = 1
      IVER = 0
      IERR = 1
C                                       Open text file
      CALL ZTXOPN ('READ', LUNSTR, FND, INFILE, .FALSE., IER)
      IF (IER.NE.0) THEN
         FND = 0
         GO TO 960
         END IF
C                                       Read in reference center
 10   CALL ZTXIO ('READ', LUNSTR, FND, LINE, IER)
      IF (IER.NE.0) GO TO 960
      IF (LINE(1:1).EQ.'$') GO TO 10
      READ (LINE,1000,ERR=960) HR, DEG
C                                       Is Dec negative ?
      DECSIN = ' '
      DO 20 ICHR = 31,44
         IF (LINE(ICHR:ICHR).EQ.'-') DECSIN = '-'
 20      CONTINUE
C                                       Image center: Convert to radians
      REFVAL(1) = (HR(1) + (HR(2) + HR(3)/60.)/6.D1)*15.
      REFVAL(2) = ABS(DEG(1)) + (ABS(DEG(2)) + ABS(DEG(3))/60.)/6.D1
      IF (DECSIN.EQ.'-') REFVAL(2) = -REFVAL(2)
C                              Read standard star position
      NSTDS = 0
      DO 100 I = 1,MXSTAR
 30      CALL ZTXIO ('READ', LUNSTR, FND, LINE, IER)
         IF (IER.EQ.2) GO TO 120
         IF (IER.NE.0) GO TO 960
         IF (LINE(1:1).EQ.'$') GO TO 30
         READ (LINE,1001,ERR=960) INNAME, INXY(1), INXY(2), HR, DEG
         IF ((INXY(1).GE.BLC(1)) .AND. (INXY(1).LE.TRC(1)) .AND.
     *      (INXY(2).GE.BLC(2)) .AND. (INXY(2).LE.TRC(2))) THEN
            NSTDS = NSTDS + 1
            NAME(NSTDS) = INNAME
            AD(1,NSTDS) = (HR(1) + (HR(2) + HR(3)/60.)/6.D1)*15.
C                                       Is Dec negative ?
            DECSIN = ' '
            DO 50 ICHR = 31, 44
               IF (LINE(ICHR:ICHR).EQ.'-') DECSIN = '-'
 50            CONTINUE
            AD(2,NSTDS) = ABS(DEG(1)) + (ABS(DEG(2)) +
     *         ABS(DEG(3))/60.)/6.D1
            IF (DECSIN.EQ.'-') AD(2,NSTDS) = -AD(2,NSTDS)
            XY(1,NSTDS) = INXY(1)
            XY(2,NSTDS) = INXY(2)
            END IF
 100     CONTINUE
C
 120  CALL ZTXCLS (LUNSTR, FND, IER)
      FND = 0
      IF ((CATR(KRCRP).GE.BLC(1)) .AND. (CATR(KRCRP).LE.TRC(1)) .AND.
     *   (CATR(KRCRP+1).GE.BLC(2)) .AND. (CATR(KRCRP+1).LE.TRC(2))
     *   .AND. (ABS(DOUTP).GT.1.5)) THEN
         REFPIX(1) = CATR(KRCRP)
         REFPIX(2) = CATR(KRCRP+1)
      ELSE
         REFPIX(1) = (BLC(1)+TRC(1))/2.
         REFPIX(2) = (BLC(2)+TRC(2))/2.
         END IF
C                                       If not 6 pameters, do 3
C                                       Then do a 3 parameter fit
      IF (NPAR.NE.6) THEN
         NLSQ = 1
C                                       6 parms requires 2 passes
      ELSE
         NLSQ = 2
         END IF
C                                       Transform and print results
      CALL XTRFIT (NPAR, NSTDS, XY, AD, NAME, .TRUE., REFPIX, REFVAL,
     *   REFINC, ANGLE, A, B, RES, IERR)
C                                       Put results in common
      IF (IERR.NE.0) THEN
         IERR = 2
C                                       Put ref. params in header
      ELSE
C                                       Reference value (degrees)
         CATD(KDCRV)   = REFVAL(1)
         CATD(KDCRV+1) = REFVAL(2)
C                                       Coordinate Increment (degrees)
         CATR(KRCIC)   = REFINC(1)
         CATR(KRCIC+1) = REFINC(2)
C                                       Reference pixel
         CATR(KRCRP)   = REFPIX(1) - BLC(1) + 1.
         CATR(KRCRP+1) = REFPIX(2) - BLC(2) + 1.
         CATR(KRCRT) = 0.0
         IF ((DOUTP.GT.1.5) .OR.(DOUTP.LE.0.0)) THEN
            CATR(KRCRT+1) = ANGLE
            ANGL = 0.0
         ELSE
            CATR(KRCRT+1) = 0.0
            ANGL = -ANGLE * DG2RAD
            END IF
         CALL H2CHR (4, 1, CATH(KHCTP+1), PTYPE)
         CALL CHR2H (4, AXRA, 1, CATH(KHCTP))
         IF (PTYPE(:1).NE.'-') CALL CHR2H (4, EXTEND, 1, CATH(KHCTP+1))
         CALL CHR2H (4, AXDEC, 1, CATH(KHCTP+2))
         IF (PTYPE(:1).NE.'-') CALL CHR2H (4, EXTEND, 1, CATH(KHCTP+3))
         XINC = REFINC(1) * DG2RAD
         YINC = REFINC(2) * DG2RAD
C                                       Setup output header
         CATBLK(KINAX) = TRC(1) - BLC(1) + 1
         CATBLK(KINAX+1) = TRC(2) - BLC(2) + 1
         CATBLK(KINAX+2) = 1
         END IF
C                                       Error handling
 960  IF (IERR.EQ.1) THEN
         MSGTXT = 'ERROR OPENING OR READING FILE '// INFILE
         CALL MSGWRT (5)
         END IF
      IF (FND.GT.0) CALL ZTXCLS (LUNSTR, FND, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (18X,2F3.0,F7.3,2F3.0,F6.2)
 1001 FORMAT (A4,2F7.2,2F3.0,F7.3,2F3.0,F6.2)
      END
      SUBROUTINE XTRCHK (IERR)
C-----------------------------------------------------------------------
C   Input from common:
C      INFILE   C*48   Name of file in with star postions.
C   Output:
C      IERR     I      Error return code
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   MXSTAR
      PARAMETER (MXSTAR=1000)
      CHARACTER LINE*80, NAME*20, DECSIN*1
      INTEGER   LUNSTR, I, IVOL, NSEQ, IVER, FND, ICHR, IER, DEPTH(5)
      LOGICAL   SWAPOK
      REAL      HR(3), DEG(3), XP, YP, XSP, YSP, XI, YI
      DOUBLE PRECISION XD, XR
      INCLUDE 'XTRAN.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      DATA LUNSTR /10/
      DATA SWAPOK, DEPTH /.FALSE., 5*1/
C-----------------------------------------------------------------------
      MSGTXT = 'Residuals computed from image header'
      CALL MSGWRT (2)
      MSGTXT = 'If these are larger, you may have skew'
      CALL MSGWRT (2)
      IVOL = 1
      NSEQ = 1
      IVER = 0
      IERR = 1
      LOCNUM = 1
      CALL SETLOC (DEPTH, SWAPOK)
C                                       Open text file
      CALL ZTXOPN ('READ', LUNSTR, FND, INFILE, .FALSE., IER)
      IF (IER.NE.0) GO TO 999
C                                       Read in reference center
 10   CALL ZTXIO ('READ', LUNSTR, FND, LINE, IERR)
      IF (IER.NE.0) GO TO 980
      IF (LINE(1:1).EQ.'$') GO TO 10
      READ (LINE,1000,ERR=980) HR, DEG
C                                       Is Dec negative ?
      DECSIN = ' '
      DO 20 ICHR = 31,44
         IF (LINE(ICHR:ICHR).EQ.'-') DECSIN = '-'
 20      CONTINUE
C                                       Image center: Convert to radians
      XR = (HR(1) + (HR(2) + HR(3)/60.)/6.D1)*15.
      XD = ABS(DEG(1)) + (ABS(DEG(2)) + ABS(DEG(3))/60.)/6.D1
      IF (DECSIN.EQ.'-') XD = -XD
      CALL XYPIX (XR, XD, XP, YP, IERR)
      XI = CATR(KRCIC) * 3600.0
      YI = CATR(KRCIC+1) * 3600.0
      XSP = CATR(KRCRP)
      YSP = CATR(KRCRP+1)
      XP = (XP - XSP) * XI * COS (XD * DG2RAD)
      YP = (YP - YSP) * YI
      NAME = 'REF PIX'
      MSGTXT = 'TEST COORDINATES WITH HEADER'
      CALL MSGWRT (3)
      WRITE (MSGTXT,1010)
      CALL MSGWRT(3)
      WRITE (MSGTXT,1015) NAME(:8), XSP, XP, YSP, YP
      CALL MSGWRT (3)
C                              Read standard star positions
      DO 100 I = 1,MXSTAR
 30      CALL ZTXIO ('READ', LUNSTR, FND, LINE, IER)
         IF (IER.EQ.2) GO TO 980
         IF (IER.NE.0) GO TO 980
         IF (LINE(1:1).EQ.'$') GO TO 30
         READ (LINE,1001,ERR=980) NAME, XSP, YSP, HR, DEG
         IF ((XSP.GE.BLC(1)) .AND. (XSP.LE.TRC(1)) .AND. (YSP.GE.BLC(2))
     *      .AND. (YSP.LE.TRC(2))) THEN
            XR = (HR(1) + (HR(2) + HR(3)/60.)/6.D1)*15.
C                                       Is Dec negative ?
            DECSIN = ' '
            DO 50 ICHR = 31, 44
               IF (LINE(ICHR:ICHR).EQ.'-') DECSIN = '-'
 50            CONTINUE
            XD = ABS(DEG(1)) + (ABS(DEG(2)) + ABS(DEG(3))/60.)/6.D1
            IF (DECSIN.EQ.'-') XD = -XD
            CALL XYPIX (XR, XD, XP, YP, IERR)
            XSP = XSP - BLC(1) + 1.
            YSP = YSP - BLC(2) + 1.
            XP = (XP - XSP) * XI * COS (XD * DG2RAD)
            YP = (YP - YSP) * YI
            WRITE (MSGTXT,1015) NAME(:8), XSP, XP, YSP, YP
            CALL MSGWRT (3)
            END IF
 100     CONTINUE
C
 980  CALL ZTXCLS (LUNSTR, FND, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (18X,2F3.0,F7.3,2F3.0,F6.2)
 1001 FORMAT (A4,2F7.2,2F3.0,F7.3,2F3.0,F6.2)
 1010 FORMAT ('NAME',8X,'XPIX',4X,'DX_ASEC',6X,'YPIX',4X,'DY_ASEC')
 1015 FORMAT (A,F10.3,F9.3,F12.3,F9.3)
      END
      SUBROUTINE ROTATE (INLUN, OUTLUN, NX, NY, NEWIM, NEWWT, IERR)
C-----------------------------------------------------------------------
C   Sets up parameters, and feeds the transform alogorithm one line of
C   the input map at a time.
C   Inputs:
C      INLUN    I      Input image logical unit numbers.
C      OUTLUN   I      New image input logical unit number.
C      NX       I      X axis dim of output image
C      NY       I      Y axis dim of output image
C   (from common block)
C      BLC      R(7)   bottom left hand corner of input map.
C      TRC      R(7)   top right hand corner of input map.
C   Output:
C      IERR     I      Standard WAWA IO error message.
C-----------------------------------------------------------------------
      INTEGER   INLUN, OUTLUN, NX, NY, IERR
      REAL      NEWIM(NX,NY), NEWWT(NX,NY)
      INTEGER   IL, I, IINC
      REAL      ZBLC(7), ZTRC(7)
      INCLUDE 'XTRAN.INC'
      INCLUDE 'INCS:PMAD.INC'
      REAL      DATA(MAXIMG)
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      CSN = COS (ANGL)
      SN  = SIN (ANGL)
      DX = CSN
      DY = DX
C                                       all in core for regridded!
      IF (DOUTP.LE.1.5) THEN
C                                       Initialize the new image array
         I = NX * NY
         CALL RFILL (I, 0.0, NEWIM)
         CALL RFILL (I, 0.0, NEWWT)
         IINC = (8192 * 512) / NX
C                                       Init old image for read
         CALL MAPWIN (INLUN, BLC, TRC, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Feed rows to rotation algorithm
         DO 20 IL = 1,NY
            IF (MOD(IL-1,IINC).EQ.0) THEN
               WRITE (MSGTXT,1000) IL
               CALL MSGWRT (2)
               END IF
            CALL MAPIO ('READ', INLUN, DATA, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL MAPROT (DATA, IL, NX, NY, NEWIM, NEWWT)
 20         CONTINUE
C                                       Write out new image
         CALL OPENCF (OUTLUN, OUTSTR, IERR)
         IF (IERR.NE.0) GO TO 990
         ZBLC(1) = 1.
         ZBLC(2) = 1.
         ZTRC(1) = NX
         ZTRC(2) = NY
         CALL MAPWIN (OUTLUN, ZBLC, ZTRC, IERR)
         IF (IERR.NE.0) GO TO 990
         DMIN = 1.E10
         DMAX = -DMIN
         MSGTXT = 'Writing the regridded image to disk'
         CALL MSGWRT (2)
         DO 40 IL = 1,NY
            DO 30 I = 1,NX
               IF (NEWWT(I,IL).EQ.0.0) NEWIM(I,IL) = FBLANK
               IF (NEWIM(I,IL).NE.FBLANK) THEN
                  NEWIM(I,IL) = NEWIM(I,IL) / NEWWT(I,IL)
                  DMAX = MAX (DMAX, NEWIM(I,IL))
                  DMIN = MIN (DMIN, NEWIM(I,IL))
                  END IF
 30            CONTINUE
            CALL MAPIO ('WRIT', OUTLUN, NEWIM(1,IL), IERR)
            IF (IERR.NE.0) GO TO 990
 40        CONTINUE
C                                       read/write as go for copy
      ELSE
C                                       Init old image for read
         CALL MAPWIN (INLUN, BLC, TRC, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Write out new image
         CALL OPENCF (OUTLUN, OUTSTR, IERR)
         IF (IERR.NE.0) GO TO 990
         ZBLC(1) = 1.
         ZBLC(2) = 1.
         ZTRC(1) = NX
         ZTRC(2) = NY
         CALL MAPWIN (OUTLUN, ZBLC, ZTRC, IERR)
         IF (IERR.NE.0) GO TO 990
         DMIN = 1.E10
         DMAX = -DMIN
C                                       Feed rows to rotation algorithm
         DO 140 IL = 1,NY
            CALL MAPIO ('READ', INLUN, DATA, IERR)
            IF (IERR.NE.0) GO TO 980
            DO 130 I = 1,NX
               IF (DATA(I).NE.FBLANK) THEN
                  DMAX = MAX (DMAX, DATA(I))
                  DMIN = MIN (DMIN, DATA(I))
                  END IF
 130           CONTINUE
            CALL MAPIO ('WRIT', OUTLUN, DATA, IERR)
            IF (IERR.NE.0) GO TO 990
 140       CONTINUE
         END IF
      CALL FILCLS (OUTLUN)
      GO TO 999
C                                       error handling
 980  CALL PRTERR (IERR, INLUN)
      GO TO 999
 990  CALL PRTERR (IERR, OUTLUN)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Regridding image now at row',I6)
      END
      SUBROUTINE MAPROT (DATA, LINE, NX, NY, NEWIM, NEWWT)
C-----------------------------------------------------------------------
C   Does the actual transform of map coordinates.
C   If NLSQ = 2, then NPAR = 6 and the map is subjected to a nonlinear
C   stretch in addition to rotation.
C   Inputs:
C      DATA   R(*)       Line of data from input map.
C      LINE   I          Row number of line to work on
C      NX     I          X dimension of output
C      NY     I          Y dimension of output
C   Output:
C      NEWIM  R(NX,NY)   Output image
C      NEWWT  R(NX,NY)   Output sum of weights image
C-----------------------------------------------------------------------
      INTEGER   LINE, NX, NY
      REAL      DATA(*), NEWIM(NX,NY), NEWWT(NX,NY)
C
      INTEGER   IX, IY, KK, OX, OY, NPM2, NLM2, ITEMP, I, J, JJ, K, XA,
     *   YA, JMAX, KMAX
      REAL      XOFF, YOFF, Y1, Y2, X1, X2, XMIN, XMAX, YMIN,
     *   YMAX, EPSX(3), EPSY(3), AREA, PPIX, XOFF2, YOFF2, YCSN, YSN,
     *   CXINC, CYINC, WPIX
      INCLUDE 'XTRAN.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      CALL RFILL (7, 1.0, BLC)
      CALL RFILL (7, 1.0, TRC)
      NPM2 = NPNTS - 2
      NLM2 = NLINES - 2
      ITEMP = NPNTS - 1
      OX = X0
      OY = Y0
C                                     Calc offset from middle
      YOFF  = LINE - OY - 0.5
      YOFF2 = LINE - OY
      YSN   = YOFF * SN
      YCSN  = YOFF2 * CSN
      CXINC = 1.0/XINC
      CYINC = 1.0/YINC
C                                     For all points but 2
      DO 900 I = 2,ITEMP
         IF (DATA(I).NE.FBLANK) THEN
            XOFF  = I - OX
            XOFF2 = I - OX - 0.5
C                                          Else stretch and rotate
            X1 = OX + CXINC*(XOFF*A(3) + YOFF*A(2)
     *         + (XOFF**2)*A(4) + (YOFF**2)*A(5) + XOFF*YOFF*A(6))
            DX = CXINC*(A(3) + A(6)*YOFF + A(4)*(2.*XOFF + 1.))
            Y1 = OY + CYINC*(XOFF2*B(3) + YOFF2*B(2)
     *         + (XOFF2**2)*B(4) + (YOFF2**2)*B(5) + XOFF2*YOFF2*B(6))
            DY = CYINC*(B(2) + B(6)*XOFF2 + B(5)*(2.*YOFF2+1.))
C                                          Calc Compression area
            X2 = X1 + DX
            Y2 = Y1 + DY
            XMIN = MIN (X1, X2)
            XMAX = MAX (X1, X2)
            YMIN = MIN (Y1, Y2)
            YMAX = MAX (Y1, Y2)
            IX = XMIN
            IY = YMIN
            IF ((IX.LT.1) .OR. (IX.GT.NPM2)) GO TO 900
            IF ((IY.LT.1) .OR. (IY.GT.NLM2)) GO TO 900
C                                     Track lowest point to rotate
            XA = IX
            YA = IY
C                                     Init expansion factors
            EPSX(2) = 0.
            EPSX(3) = 0.
            EPSY(2) = 0.
            EPSY(3) = 0.
C                                     Assume rotation of into 9 pixels
            IF (XMAX.LE.(XA+1.)) THEN
               EPSX(1) = XMAX - XMIN
C                                     X rotation stays in 1 pixel
               KMAX = 1
            ELSE
               EPSX(1) = (XA+1.) - XMIN
               IF (XMAX.LE.(XA+2.)) THEN
                  EPSX(2) = XMAX - (XA+1.)
                  KMAX = 2
               ELSE
                  EPSX(2) = 1.
                  EPSX(3) = XMAX - (XA+2.)
                  KMAX = 3
                  END IF
               END IF
C                                       Calc Y expansion factor
            IF (YMAX.LE.(YA+1.)) THEN
               EPSY(1) = YMAX - YMIN
C                                       Y rotation stays in 1 pixel
               JMAX = 1
            ELSE
               EPSY(1) = (YA+1.) - YMIN
               IF (YMAX.LE.(YA+2.)) THEN
                  EPSY(2) = YMAX - (YA+1.)
                  JMAX = 2
               ELSE
                  EPSY(2) = 1.
                  EPSY(3) = YMAX - (YA+2.)
                  JMAX = 3
                  END IF
               END IF
            AREA = ABS ((XMAX-XMIN)*(YMAX-YMIN))
            PPIX = DATA(I) / AREA
            WPIX = 1.0 / AREA
C                                     For 9 pixels around data
            DO 700 J = 1,JMAX
               JJ = IY + J - 1
               DO 690 K = 1,KMAX
                  KK = IX + K - 1
C                                     If inside region
                  IF ((KK.GE.1) .AND. (JJ.GE.1)) THEN
                     NEWIM(KK,JJ) = NEWIM(KK,JJ)+PPIX*EPSX(K)*EPSY(J)
                     NEWWT(KK,JJ) = NEWWT(KK,JJ)+WPIX*EPSX(K)*EPSY(J)
                     END IF
 690              CONTINUE
 700           CONTINUE
            END IF
 900     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE HISTRY (PRGNAM, INLUN, OUTLUN, IHINL, IHOTL, IDATA,
     *   IWORK)
C-----------------------------------------------------------------------
C  HISTRY will create and write the history file for the new image.
C  Inputs:  PRGNAM  C*6 program name.
C           INLUN   I   the LUN for the input map.
C           OUTLUN  I   the LUN for the output map.
C           IHINL   I   the LUN to use for the input history file.
C           IHOTL   I   the LUN to use for the new output history file.
C           IDATA   I(256)   History I/O buffer.
C           IWORK   I(256)   work buffer.
C-----------------------------------------------------------------------
      CHARACTER NAMSTR*36, PRGNAM*6, HILINE*72, NAME*12, CLASS*6,
     *   PTYPE*2
      INTEGER   IWORK(256), IDATA(256), IERR2, SEQ, DISK, USID
      INTEGER   IBLC(7), ITRC(7), INPTR, IOPTR, IERR, IPTR, I
      INTEGER   INLUN, OUTLUN, IHINL, IHOTL, NC, ITRIM
      LOGICAL   UPDATE
      INCLUDE 'XTRAN.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA UPDATE /.TRUE./
C-----------------------------------------------------------------------
C                                       Get additional data from common
      INPTR = 0
      IOPTR = 0
      DO 20 IPTR = 1,EFIL
         IF (INLUN.EQ.FILTAB(POLUN,IPTR)) INPTR = IPTR
         IF (OUTLUN.EQ.FILTAB(POLUN,IPTR)) IOPTR = IPTR
 20      CONTINUE
C                                       Check for file not open error.
      IF ((INPTR.EQ.0) .OR. (IOPTR.EQ.0)) GO TO 330
C                                       copy keywords
      CALL KEYCOP ( FILTAB(POVOL,INPTR), FILTAB(POCAT,INPTR),
     *   FILTAB(POVOL,IOPTR), FILTAB(POCAT,IOPTR), IERR)
C                                       Create history file and
C                                       copy HI of INSEQ
      CALL HIINIT (2)
      CALL HISCOP (IHINL, IHOTL, FILTAB(POVOL,INPTR),
     *   FILTAB(POVOL,IOPTR), FILTAB(POCAT,INPTR), FILTAB(POCAT,IOPTR),
     *   CATBLK, IWORK, IDATA, IERR)
      IF (IERR.NE.0) GO TO 330
C                                       Add new history.
C                                       Input map.
      CALL GTNAME (INLUN, NAMSTR, IERR)
      IF (IERR.NE.0) GO TO 320
      CALL WAWA2A (NAMSTR, NAME, CLASS, SEQ, PTYPE, DISK, USID)
      CALL HENCO1 (PRGNAM, NAME, CLASS, SEQ,
     *   FILTAB(POVOL,INPTR), IHOTL, IDATA, IERR)
      IF (IERR.NE.0) GO TO 320
C                                       Output map.
      CALL GTNAME (OUTLUN, NAMSTR, IERR)
      IF (IERR.NE.0) GO TO 320
      CALL WAWA2A (NAMSTR, NAME, CLASS, SEQ, PTYPE, DISK, USID)
      CALL HENCOO (PRGNAM,  NAME, CLASS, SEQ,
     *   FILTAB(POVOL,IOPTR), IHOTL, IDATA, IERR)
      IF (IERR.NE.0) GO TO 320
C                                       BLC, TRC
      DO 40 I = 1,7
         IBLC(I) = BLC(I) + .5
         ITRC(I) = TRC(I) + .5
 40      CONTINUE
      WRITE (HILINE,1010) PRGNAM, IBLC
      CALL HIADD (IHOTL, HILINE, IDATA, IERR)
      IF (IERR.NE.0) GO TO 320
      WRITE (HILINE,1011) PRGNAM, ITRC
      CALL HIADD (IHOTL, HILINE, IDATA, IERR)
      IF (IERR.NE.0) GO TO 320
      WRITE (HILINE,1012) PRGNAM, NPAR
      CALL HIADD (IHOTL, HILINE, IDATA, IERR)
      IF (IERR.NE.0) GO TO 320
      WRITE (HILINE,1013) PRGNAM, ANGL*57.29577951
      CALL HIADD (IHOTL, HILINE, IDATA, IERR)
      IF (IERR.NE.0) GO TO 320
      NC = ITRIM (INFILE)
      HILINE = PRGNAM // 'INFILE = ''' // INFILE(:NC) // ''''
      CALL HIADD (IHOTL, HILINE, IDATA, IERR)
      IF (IERR.NE.0) GO TO 320
      DO 50 I = 1,6
         WRITE (HILINE,1015) PRGNAM, 'A', I, A(I), 'X'
         CALL HIADD (IHOTL, HILINE, IDATA, IERR)
         IF (IERR.NE.0) GO TO 320
 50      CONTINUE
      DO 55 I = 1,6
         WRITE (HILINE,1015) PRGNAM, 'B', I, B(I), 'Y'
         CALL HIADD (IHOTL, HILINE, IDATA, IERR)
         IF (IERR.NE.0) GO TO 320
 55      CONTINUE
C                                       Close HI file
 320  CALL HICLOS (IHOTL, UPDATE, IDATA, IERR2)
      IF ((IERR.NE.0) .OR. (IERR2.NE.0)) GO TO 330
C                                        Copy tables
      CALL ALLTAB (1, '  ', IHINL, IHOTL,  FILTAB(POVOL,INPTR),
     *   FILTAB(POVOL,IOPTR), FILTAB(POCAT,INPTR), FILTAB(POCAT,IOPTR),
     *   CATBLK, IWORK, IDATA, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
      GO TO 999
C                                       Error comment
 330  WRITE (MSGTXT,1030)
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (A6,'BLC =',7I6)
 1011 FORMAT (A6,'TRC =',7I6)
 1012 FORMAT (A6,'/ NUMBER OF PARAMETERS FIT =',I2)
 1013 FORMAT (A6,'/ ROTATION ANGLE =',F8.3,' DEGREES')
 1015 FORMAT (A6,A,'(',I1,') = ',1PD12.5,4X,'/ ',A,'_fit parameter')
 1030 FORMAT('WARNING. ERROR WRITING HISTORY FILE.')
      END
