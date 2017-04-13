      PROGRAM BDEPO
C-----------------------------------------------------------------------
C! Predicts beam depolarization from rotation measure images
C# Map Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2008-2009
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
C   BDEPO predicts beam depolarization from Rotation Measure gradient
C   images (created by NINER say).
C       Inputs:
C 1         USERID               Owner of the image
C 2-4       INNAME(3)            First RM gradient image name
C 5-6       INCLASS(2)           First RM gradient image class
C 7         INSEQ                First RM gradient image seq. no.
C 8         INDISK               First RM gradient image disk no.
C 9-11      I2NNAME(3)           Second RM gradient image name
C 12-13     IN2CLASS(2)          Second RM gradient image class
C 14        IN2SEQ               Second RM gradient image seq. no.
C 15        IN2DISK              Second RM gradient image disk no.
C 16-22     BLC(7)               Input image(s) BLC
C 23-29     TRC(7)               Input image(s) TRC
C 30-32     OUTNAME(3)           Output image name
C 33-34     OUTCLASS(2)          Output image class
C 35        OUTSEQ               Output image seq. no.
C 36        OUTDISK              Output image disk no.
C 37-46     CPARM(10)            Frequencies between which to compute
C                                depolarization in CPARM(1:2)
C                                Multiplicative factors for gradient
C   images
C                                in CPARM(3) and CPARM(4)
C     Neil Killeen, Feb 1987
C-----------------------------------------------------------------------
      INTEGER   NPARMS
      PARAMETER (NPARMS = 46)
      CHARACTER RGANAM*36, RGBNAM*36, PDPNAM*36
      REAL      RPARM(NPARMS), FR1, FR2, GRFACA, GRFACB
      INTEGER   IERR, IER, RGASLO, PDPSLO, LUNRGA, LUNRGB, LUNPDP,
     *   BLC(2), TRC(2)
      LOGICAL   OPENB
      DATA IER, LUNRGA, LUNRGB, LUNPDP /0, 17, 18, 19/
C-----------------------------------------------------------------------
C                                       start up task, get AIPS
C                                       parameters
      CALL START (RPARM, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       open image files
      CALL OPENIM (RPARM, LUNRGA, LUNRGB, LUNPDP, RGANAM, RGBNAM,
     *   PDPNAM, OPENB, BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       write output image
      CALL OUTPUT (RPARM, LUNRGA, LUNRGB, LUNPDP, OPENB, RGASLO,
     *   PDPSLO, FR1, FR2, GRFACA, GRFACB, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       close files
      CALL CLOSUP (OPENB, LUNRGA, LUNRGB, LUNPDP)
C                                       write history
      CALL DEPOHI (OPENB, RGANAM, RGBNAM, PDPNAM, RGASLO, PDPSLO,
     *   BLC, TRC, FR1, FR2, GRFACA, GRFACB)
      CALL CLENUP
      GO TO 990
C                                       error return
 900  IER = 1
      CALL CLENUP
C                                       normal return
 990  CALL TSKEND (IER)
C-----------------------------------------------------------------------
      STOP
      END
      SUBROUTINE START (RPARM, IERR)
C-----------------------------------------------------------------------
C     Start up task, get input parameters and write initial message
C     OUTPUT:
C       RPARM   R      array containing AIPS parameters
C       IERR    I      error status, 0=> OK
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      REAL      RPARM(*)
      INTEGER   NPARMS, IERR
      INCLUDE 'INCS:DMSG.INC'
      DATA PRGNAM /'BDEPO '/
C-----------------------------------------------------------------------
C                                       start up TASK, get AIPS
C                                       parameters except USERID
      NPARMS = 45
      CALL TSKBEG (PRGNAM, NPARMS, RPARM(2), IERR)
      IF (IERR.EQ.0) THEN
         WRITE (MSGTXT,1000)
         CALL MSGWRT(8)
      ELSE
         WRITE (MSGTXT,2000) IERR
         CALL MSGWRT (8)
         END IF
      RPARM(1) = NLUSER
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('You are using a non-linear program')
 2000 FORMAT ('START: COULD NOT GET INPUTS, IERR=', I4)
      END
      SUBROUTINE OPENIM (RPARM, LUNRGA, LUNRGB, LUNPDP, RGANAM, RGBNAM,
     *   PDPNAM, OPENB, BLC, TRC, IERR)
C-----------------------------------------------------------------------
C   Open the input image(s) and create and open the output image.
C     Input:
C         RPARM     R       array of AIPS input parameters
C         LUNRGA    I       logical unit number for first (x) RM
C                            gradient image
C         LUNRGB    I       logical unit number for second (y) RM
C                           gradient image
C         LUNPDP    I       logical unit number for output DPR image
C     Output:
C         RGANAM    C*36    Namestring for first RM gradient image
C         RGBNAM    C*36    Namestring for second RM gradient image
C         PDPNAM    C*36    Namestring for output DPR image
C         OPENB     L       .true. if second RM image opened
C         BLC,TRC   I       Window on input images, defaults filled in
C         IERR      I       error status, 0=> OK
C-----------------------------------------------------------------------
      CHARACTER RGANAM*36, RGBNAM*36, PDPNAM*36
      REAL      RPARM(*)
      INTEGER   LUNRGA, LUNRGB, LUNPDP, IERR, BLC(*), TRC(*)
      LOGICAL   OPENB
C-----------------------------------------------------------------------
C                                        open input image(s)
      CALL OPENIN (RPARM, LUNRGA, LUNRGB, RGANAM, RGBNAM, OPENB, IERR)
C                                        open output AIPS image
      IF (IERR.EQ.0) CALL OPENOU (RPARM, LUNPDP, PDPNAM, BLC, TRC, IERR)
C
      RETURN
      END
      SUBROUTINE OPENIN (RPARM, LUNRGA, LUNRGB, RGANAM, RGBNAM,
     *   OPENB, IERR)
C-----------------------------------------------------------------------
C   Set up the input namestring(s), open and window the input image(s)
C   and get the header(s).   Make some simple checks about
C   compatability between the images if both are opened.
C     Input:
C         RPARM     R       array of AIPS input parameters
C         LUNRGA    I       logical unit number for first (x) RM
C                           gradient image
C         LUNRGB    I       logical unit number for second (y) RM
C                           gradient image
C     Output:
C         RGANAM    C*36    Namestring for first RM gradient image
C         RGBNAM    C*36    Namestring for second RM gradient image
C         OPENB     L       .true. if second RM image opened
C         IERR      I       error status, 0=> OK
C RPARM PARAMETERS:
C       Inputs:
C 1         USERID               Owner of the image
C 2-4       INNAME(3)            First RM gradient image name
C 5-6       INCLASS(2)           First RM gradient image class
C 7         INSEQ                First RM gradient image seq. no.
C 8         INDISK               First RM gradient image disk no.
C 9-11      I2NNAME(3)           Second RM gradient image name
C 12-13     IN2CLASS(2)          Second RM gradient image class
C 14        IN2SEQ               Second RM gradient image seq. no.
C 15        IN2DISK              Second RM gradient image disk no.
C 16-22     BLC(7)               Input image(s) BLC
C 23-29     TRC(7)               Input image(s) TRC
C
C-----------------------------------------------------------------------
      REAL      RPARM(*)
      CHARACTER RGANAM*36, RGBNAM*36
      INTEGER   LUNRGA, LUNRGB, IERR
      LOGICAL   OPENB
C-----------------------------------------------------------------------
C                                              first RM gradient image
      CALL OPENFI (RPARM, LUNRGA, RGANAM, IERR)
C                                              optional second RM
C                                       gradient image
      IF (IERR.EQ.0) CALL OPENSE (RPARM, LUNRGB, RGBNAM, OPENB, IERR)
C
      RETURN
      END
      SUBROUTINE OPENFI (RPARM, LUNRGA, RGANAM, IERR)
C-----------------------------------------------------------------------
C     For the the first input RM gradient image, set up the input
C   namestring,
C     open and window the image and get the header.
C     Input:
C         RPARM     R       array of AIPS input parameters
C         LUNRGA    I       logical unit number for first (x) RM
C                           gradient image
C     Output:
C         RGANAM    R       Namestring for first RM gradient image
C         IERR      I       error status, 0=> OK
C
C RPARM PARAMETERS:
C
C       INPUTS:
C 1         USERID               Owner of the image
C 2-4       INNAME(3)            First RM gradient image name
C 5-6       INCLASS(2)           First RM gradient image class
C 7         INSEQ                First RM gradient image seq. no.
C 8         INDISK               First RM gradient image disk no.
C 16-22     BLC(7)               Input image(s) BLC
C 23-29     TRC(7)               Input image(s) TRC
C
C-----------------------------------------------------------------------
      CHARACTER RGANAM*36
      HOLLERITH MAP
      DOUBLE PRECISION HRGA8(128), DUM1(128), DUM2(128)
      REAL   RPARM(*)
      INTEGER   LUNRGA, HRGA2(256), IERR
      INCLUDE 'INCS:DMSG.INC'
      COMMON /HEADS/ HRGA8, DUM1, DUM2
      EQUIVALENCE (HRGA2, HRGA8)
C-----------------------------------------------------------------------
C                                           set up namestring
      CALL CHR2H (4, 'MA  ', 1, MAP)
      CALL H2WAWA (RPARM(2), RPARM(5), RPARM(7), MAP, RPARM(8),
     *   RPARM(1), RGANAM)
C                                           open file
      CALL OPENCF (LUNRGA, RGANAM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 900
         END IF
C                                           get header
      CALL GETHDR (LUNRGA, HRGA2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,2000) IERR
         GO TO 900
         END IF
C                                           set up window
      CALL MAPWIN (LUNRGA, RPARM(16), RPARM(23), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,3000) IERR
         GO TO 900
         END IF
      GO TO 990
C
 900  CALL MSGWRT (8)
C
 990  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OPENFI: ERROR OPENING FIRST GRADIENT IMAGE, IERR=', I3)
 2000 FORMAT ('OPENFI: ERROR GETTING FIRST GRADIENT IMAGE HEADER,',
     *   ' IERR=', I4)
 3000 FORMAT ('OPENFI: ERROR WINDOWING FIRST GRADIENT IMAGE, IERR=',
     *     I3)
      END
      SUBROUTINE OPENSE (RPARM, LUNRGB, RGBNAM, OPENB, IERR)
C-----------------------------------------------------------------------
C     For the optional second RM gradient image, set up the input
C   namestring,
C     open and window the image and get the header.  Make some simple
C   checks
C     about compatability between the input images if both are opened.
C     Input:
C         RPARM     R       array of AIPS input parameters
C         LUNRGB    I       logical unit number for second (y) RM
C                           gradient image
C     Output:
C         RGBNAM    C*36    Namestring for second RM gradient image
C         OPENB     L       .true. if second RM image opened
C         IERR      I       error status, 0=> OK
C RPARM PARAMETERS:
C       INPUTS:
C 1         USERID               Owner of the image
C 9-11      I2NNAME(3)           Second RM gradient image name
C 12-13     IN2CLASS(2)          Second RM gradient image class
C 14        IN2SEQ               Second RM gradient image seq. no.
C 15        IN2DISK              Second RM gradient image disk no.
C 16-22     BLC(7)               Input image(s) BLC
C 23-29     TRC(7)               Input image(s) TRC
C-----------------------------------------------------------------------
      CHARACTER RGBNAM*36
      HOLLERITH MAP
      DOUBLE PRECISION  DUM1(128), HRGB8(128), DUM2(128)
      REAL   RPARM(*)
      INTEGER   LUNRGB, HRGB2(256), IERR
      LOGICAL   OPENB
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /HEADS/ DUM1, HRGB8, DUM2
      EQUIVALENCE (HRGB2, HRGB8)
C-----------------------------------------------------------------------
C                                       set up name string
      IF (RPARM(9).NE.HBLANK) THEN
         CALL CHR2H (4, 'MA  ', 1, MAP)
         CALL H2WAWA (RPARM(9), RPARM(12), RPARM(14), MAP, RPARM(15),
     *      RPARM(1), RGBNAM)
C                                        open file
        CALL OPENCF (LUNRGB, RGBNAM, IERR)
        IF (IERR.NE.0) THEN
           WRITE (MSGTXT,1000) IERR
           GO TO 900
           END IF
C                                        get header
        CALL GETHDR (LUNRGB, HRGB2, IERR)
        IF (IERR.NE.0) THEN
           WRITE (MSGTXT,2000) IERR
           GO TO 900
           END IF
C                                        set up window
        CALL MAPWIN (LUNRGB, RPARM(16), RPARM(23), IERR)
        IF (IERR.NE.0) THEN
           WRITE (MSGTXT,3000) IERR
           GO TO 900
           END IF
C
         CALL IMACOM (IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,4000) IERR
            GO TO 900
            END IF
         OPENB = .TRUE.
      ELSE
         OPENB = .FALSE.
         END IF
      GO TO 990
C
 900  CALL MSGWRT (8)
 990  RETURN
C
C-----------------------------------------------------------------------
 1000 FORMAT ('OPENSE: ERROR OPENING SECOND GRADIENT IMAGE, IERR=', I3)
 2000 FORMAT ('OPENSE: ERROR GETTING SECOND GRADIENT IMAGE HEADER,',
     *        ' IERR=', I3)
 3000 FORMAT ('OPENSE: ERROR WINDOWING SECOND GRADIENT IMAGE, IERR=',
     *        I3)
 4000 FORMAT ('OPENSE: RM GRADIENT IMAGES NOT COMPATABLE, IERR=', I3)
      END
      SUBROUTINE IMACOM (IERR)
C-----------------------------------------------------------------------
C     Make some simple compatability checks between the two input images
C     Output:
C          IERR    I       0 => OK
C-----------------------------------------------------------------------
      DOUBLE PRECISION HRGA8(128), HRGB8(128), DUM(128)
      REAL   PIXRAT, PIXDIF, HRGA4(256), HRGB4(256)
      INTEGER   HRGA2(256), HRGB2(256), IERR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      COMMON /HEADS/ HRGA8, HRGB8, DUM
      EQUIVALENCE (HRGA2, HRGA4, HRGA8), (HRGB2, HRGB4, HRGB8)
C-----------------------------------------------------------------------
C                                         input images must have square
C                                       pixels
      IERR = 0
      PIXRAT = ABS (HRGA4(KRCIC) / HRGA4(KRCIC+1))
      PIXDIF = ABS (PIXRAT - 1.0)
      IF (PIXDIF.GT.1.0E-5) THEN
         WRITE (MSGTXT,1000)
         IERR = 1
         GO TO 900
         END IF
C
      PIXRAT = ABS(HRGB4(KRCIC) / HRGB4(KRCIC+1))
      PIXDIF = ABS(PIXRAT - 1.0)
      IF (PIXDIF.GT.1.0E-5) THEN
         WRITE (MSGTXT,2000)
         IERR = 1
         GO TO 900
         END IF
C                                       try and make sure increments are
C                                       the same.
      PIXRAT = ABS(HRGA4(KRCIC) / HRGB4(KRCIC))
      PIXDIF = ABS(PIXRAT - 1.0)
      IF (PIXDIF.GT.1.0E-5) THEN
         WRITE (MSGTXT,3000)
         IERR = 2
         GO TO 900
         END IF
C
      PIXRAT = ABS (HRGA4(KRCIC+1) / HRGB4(KRCIC+1))
      PIXDIF = ABS(PIXRAT - 1.0)
      IF (PIXDIF.GT.1.0E-5) THEN
         WRITE (MSGTXT,4000)
         IERR = 2
         GO TO 900
         END IF
C                                               check image sizes
      IF ( (HRGA2(KINAX) .NE. HRGB2(KINAX)) .OR.
     *     (HRGA2(KINAX+1) .NE. HRGB2(KINAX+1))  ) THEN
          WRITE (MSGTXT,5000)
          IERR = 3
          GO TO 900
         END IF
      GO TO 990
 900  CALL MSGWRT (8)
C
 990  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMACOM: PIXELS ARE NOT SQUARE IN FIRST GRADIENT IMAGE')
 2000 FORMAT ('IMACOM: PIXELS ARE NOT SQUARE IN SECOND GRADIENT IMAGE')
 3000 FORMAT ('IMACOM: GRADIENT IMAGE X-AXIS INCREMENTS ARE NOT EQUAL')
 4000 FORMAT ('IMACOM: GRADIENT IMAGE Y-AXIS INCREMENTS ARE NOT EQUAL')
 5000 FORMAT ('IMACOM: GRADIENT IMAGE DIMENSIONS ARE NOT EQUAL')
      END
      SUBROUTINE OPENOU (RPARM, LUNPDP, PDPNAM, BLC, TRC, IERR)
C-----------------------------------------------------------------------
C     Create and open output image
C     Inputs:
C         RPARM     R       array of AIPS input parameters
C         LUNPDP    I       logical unit number for predicted DPR image
C     Outputs:
C         PDPNAM    R       Namestring for predicted DPR image
C         BLC       I       BLC of input image
C         TRC       I       TRC of input image
C         IERR      I       error status, 0=> OK
C
C  RPARM PARAMETERS:
C
C 1         USERID               Owner of the image
C 2-4       INNAME(3)            First RM gradient image name
C 5-6       INCLASS(2)           First RM gradient image class
C 7         INSEQ                First RM gradient image seq. no.
C 8         INDISK               First RM gradient image disk no.
C 30-32     OUTNAME(3)           Output image name
C 33-34     OUTCLASS(2)          Output image class
C 35        OUTSEQ               Output image seq. no.
C 36        OUTDISK              Output image disk no.
C
C-----------------------------------------------------------------------
      CHARACTER DEFNAM*36, PDPNAM*36, BDEP*6
      HOLLERITH MAP, HTEMP(2)
      DOUBLE PRECISION HRGA8(128), HPDP8(128), DUM(128)
      REAL   RPARM(*)
      INTEGER   LUNPDP, IERR, HRGA2(256), HPDP2(256), BLC(*), TRC(*)
      INCLUDE 'INCS:DMSG.INC'
      COMMON /HEADS/ HRGA8, DUM, HPDP8
      EQUIVALENCE (HRGA2, HRGA8), (HPDP2, HPDP8)
      DATA BDEP /'BDEPO '/
C-----------------------------------------------------------------------
      CALL HEADER (RPARM, BLC, TRC)
C                                       default output namestring
      CALL CHR2H (4, 'MA  ', 1, MAP)
      CALL CHR2H (6, BDEP, 1, HTEMP)
      CALL H2WAWA (RPARM(2), HTEMP, RPARM(7), MAP, RPARM(8),
     *   RPARM(1), DEFNAM)
C                                        output namestring
      CALL H2WAWA (RPARM(30), RPARM(33), RPARM(35), MAP, RPARM(36),
     *   RPARM(1), PDPNAM)
C                                        create file
      CALL MAPCR (DEFNAM, PDPNAM, HPDP2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 900
         END IF
C                                         open file
      CALL OPENCF (LUNPDP, PDPNAM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,2000) IERR
         GO TO 900
         END IF
      GO TO 990
 900  CALL MSGWRT (8)
C
 990  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OPENOU: COULD NOT CREATE OUTPUT IMAGE, IERR=', I3)
 2000 FORMAT ('OPENOU: COULD NOT OPEN OUTPUT IMAGE, IERR=',  I3)
      END
      SUBROUTINE HEADER (RPARM, BLC, TRC)
C-----------------------------------------------------------------------
C  Make the output image header from the first RM gradient image
C     Input:
C         RPARM     R       array of AIPS input parameters
C     Ouput:
C         BLC       I       BLC of input image
C         TRC       I       TRC of input image
C  RPARM PARAMETERS:
C 16-22     BLC(7)           Input image(s) BLC
C 23-29     TRC(7)           Input image(s) TRC
C-----------------------------------------------------------------------
      CHARACTER UNITS*8
      HOLLERITH HPDPH(256)
      DOUBLE PRECISION HRGA8(128), HPDP8(128), DUM8(128)
      REAL   RPARM(*), HRGA4(256), HPDP4(256)
      INTEGER   BLC(*), TRC(*), HRGA2(256), HPDP2(256), NCOLS, NROWS,
     *   I, IROUND
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /HEADS/ HRGA8, DUM8, HPDP8
      EQUIVALENCE (HRGA2,  HRGA4, HRGA8) , (HPDP2, HPDP4, HPDP8, HPDPH)
      DATA UNITS /'RATIO   '/
C-----------------------------------------------------------------------
C                                       work out output image
C                                       dimensions
      BLC(1) = IROUND (RPARM(16))
      BLC(2) = IROUND (RPARM(17))
      TRC(1) = IROUND (RPARM(23))
      TRC(2) = IROUND (RPARM(24))
C                                       fill in window defaults
      IF (BLC(1).EQ.0) BLC(1) = 1
      IF (BLC(2).EQ.0) BLC(2) = 1
      IF (TRC(1).EQ.0) TRC(1) = HRGA2(KINAX)
      IF (TRC(2).EQ.0) TRC(2) = HRGA2(KINAX+1)
C                                       copy header
      DO 100 I = 1, 128
         HPDP8(I) = HRGA8(I)
 100     CONTINUE
      NCOLS = TRC(1) - BLC(1) + 1
      NROWS = TRC(2) - BLC(2) + 1
C                                        fill new header values
      CALL CHR2H (8, UNITS, 1, HPDPH(KHBUN))
      HPDP4(KRCRP) = HPDP4(KRCRP) - BLC(1) + 1
      HPDP4(KRCRP+1) = HPDP4(KRCRP+1) - BLC(2) + 1
      HPDP4(KRBLK) = 0.0
      HPDP2(KIDIM) = 2
      HPDP2(KINAX) = NCOLS
      HPDP2(KINAX+1) = NROWS
      HPDP2(KINAX+2) = 1
C
      RETURN
      END
      SUBROUTINE OUTPUT (RPARM, LUNRGA, LUNRGB, LUNPDP, OPENB, RGASLO,
     *   PDPSLO, FR1, FR2, GRFACA, GRFACB, IERR)
C-----------------------------------------------------------------------
C     Compute the predicted beam depolarization from the RM gradient
C   images(s) and frequencies and write the output image.
C     Input:
C         RPARM    R       AIPS parameters
C         LUNRGA   I       LUN for first RM gradient image
C         LUNRGB   I       LUN for second (optional) RM gradient image
C         LUNPDP   I       LUN for predicted DPR image
C         OPENB    L      .true. if second gradient image opened
C     Output:
C         RGASLO   I       Catalog slot of first gradient image
C         PDPSLO   I       Catalog slot of output image
C         FR1      R       Lower frequency (Hz)
C         FR2      R       Higher frequency (Hz)
C         GRFACA   R       Multiplicative factor for first gradient
C                          image
C         GRFACB   R       Multiplicative factor for second gradient
C                          image
C         IERR     I       0 => all OK
C
C-----------------------------------------------------------------------
      REAL   RPARM(*), FR1, FR2, GRFACA, GRFACB, DMIN, DMAX, NBLANK,
     *   RGADAT(2048), RGBDAT(2048), PDPDAT(2048), DPFACS(5)
      INTEGER   NCOLS, NROWS, LUNRGA, LUNRGB, LUNPDP, IERR, J,
     *   PDPSLO, RGASLO
      LOGICAL   OPENB
      INTEGER   IFIL, OFIL
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA NBLANK, DMIN, DMAX / 0.0, +1.0E30, -1.0E30/
C-----------------------------------------------------------------------
C                                       extract pars from inputs and
C                                       header
      CALL EXTPAR (RPARM, OPENB, FR1, FR2, GRFACA, GRFACB, NCOLS,
     *    NROWS, DPFACS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       find input file
      DO 10 IFIL = 1,EFIL
         IF (FILTAB(1,IFIL).EQ.LUNRGA) GO TO 15
 10      CONTINUE
      GO TO 90
C                                       find output file
 15   DO 20 OFIL = 1,EFIL
         IF (FILTAB(1,OFIL).EQ.LUNPDP) GO TO 30
 20      CONTINUE
      GO TO 90
C                                       copy most keywords
 30   CALL KEYPCP (FILTAB(POVOL,IFIL), FILTAB(POCAT,IFIL),
     *   FILTAB(POVOL,OFIL), FILTAB(POCAT,OFIL), 0, ' ', IERR)
C                                        loop over image
 90   DO 100 J = 1, NROWS
C                                        read in line(s) of data
         CALL READIN (OPENB, LUNRGA, LUNRGB, RGADAT, RGBDAT, IERR)
         IF (IERR.NE.0) THEN
           WRITE (MSGTXT,1000) J, IERR
           GO TO 900
           END IF
C                                         calculate current output DPR
C                                       line
        CALL LOOPIN (OPENB, NCOLS, RGADAT, RGBDAT, GRFACA, GRFACB,
     *     DPFACS, PDPDAT, DMIN, DMAX, NBLANK)
C                                         write line to output image
        CALL MAPIO ('WRIT', LUNPDP, PDPDAT, IERR)
        IF (IERR.NE.0) THEN
           WRITE (MSGTXT,2000) J, IERR
           GO TO 900
           END IF
 100     CONTINUE
C                                       update header min and max and
C                                       get slots
      CALL CATUP (LUNRGA, LUNPDP, NBLANK, DMIN, DMAX, RGASLO, PDPSLO,
     *   IERR)
      GO TO 990
 900  CALL MSGWRT (8)
C
 990  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OUTPUT: I/O ERROR READING GRADIENT IMAGE(S) AT LINE ',
     *   I4, ' IERR=', I3)
 2000 FORMAT ('OUTPUT: I/O ERROR WRITING DPR IMAGE AT LINE = ', I4,
     *   ' IERR=', I3)
      END
      SUBROUTINE EXTPAR (RPARM, OPENB, FR1, FR2, GRFACA, GRFACB, NCOLS,
     *   NROWS, DPFACS, IERR)
C-----------------------------------------------------------------------
C   Extract parameters from the AIPS inputs, from the header and then
C   calculate some factors for the depolarization calculation
C     Input:
C        RPARM    R      AIPS input parameters
C        OPENB    L      .true. if second gradient image opened
C     Output:
C        FR1      R      Lower frequency, Hz
C        FR2      R      Higher frequency, Hz
C        GRFACA   R      Factor by which first gradient image pixels are
C                        to be multiplied by.
C        GRFACB   R      Factor by which second gradient image pixels
C                        are to be multiplied by.
C        NCOLS    I      Number of columns in output image
C        NROWS    I      Number of rows in output image
C        DPFACS   R      5 element array of factors for depolarization
C        IERR     I      0 => OK
C-----------------------------------------------------------------------
      REAL   RPARM(*), FR1, FR2, GRFACA, GRFACB, DPFACS(*), BMAJ, BMIN,
     *   BPA
      INTEGER   NCOLS, NROWS, IERR
      LOGICAL   OPENB
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                        extract inputs parameters
      CALL EXTINP (RPARM, OPENB, FR1, FR2, GRFACA, GRFACB, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000)
         GO TO 900
         END IF
C                                        extract some parameters from
C                                       header
      CALL EXTHED (NCOLS, NROWS, BMAJ, BMIN, BPA, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,2000)
         GO TO 900
         END IF
C                                       work out factors for beam
C                                       depolarization
      CALL DPFAC (FR1, FR2, BMAJ, BMIN, BPA, OPENB, DPFACS)
      GO TO 990
 900  CALL MSGWRT (8)
C
 990  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('EXTPAR: ERROR EXTRACTING PARAMETERS FROM AIPS INPUTS')
 2000 FORMAT ('EXTPAR: ERROR EXTRACTING PARAMETERS FROM HEADER')
      END
      SUBROUTINE EXTINP (RPARM, OPENB, FR1, FR2, GRFACA, GRFACB, IERR)
C-----------------------------------------------------------------------
C     Extract the frequencies and gradient scale factors out of the AIPS
C     input parameters
C     Inputs:
C        RPARM    R      AIPS input parameters
C        OPENB    L      .true. if second gradient inage opened
C     Outputs:
C        FR1      R      Lower frequency, Hz
C        FR2      R      Higher frequency, Hz
C        GRFACA   R      Factor by which first gradient image pixels are
C                        to be multiplied by.   This defaults to the
C                        factor
C                        1 / (8 * sqrt(2)) which is appropriate to a
C                        gradient image produced by NINER with the
C                        operator 'SOBL' when there is no second
C                        gradient image,  else 1.0
C        GRFACB   R      Factor by which second gradient image pixels
C                        are to be multiplied by.   Defaults to 1.0
C        IERR     I      0 => OK
C AIPS Inputs:
C
C 37-46     CPARM(10)            Frequencies between which to compute
C                                depolarization in CPARM(1:2)
C                                Multiplicative factors for gradient
C                                images in CPARM(3) and CPARM(4)
C-----------------------------------------------------------------------
      REAL   RPARM(*), FR1, FR2, GRFACA, GRFACB, T1, T2
      INTEGER   IERR
      LOGICAL   OPENB
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                              frequencies
      T1 = RPARM(37)
      T2 = RPARM(38)
      IF (T1.GT.0.0 .AND. T2.GT.0.0) THEN
         FR1 = MIN (T1, T2)
         FR2 = MAX (T1, T2)
         IERR = 0
      ELSE
         WRITE (MSGTXT,1000)
         CALL MSGWRT (8)
         IERR = 1
         GO TO 900
         END IF
C                                              gradient scale factors
      IF (.NOT.OPENB) THEN
         IF (RPARM(39).NE.0.0) THEN
           GRFACA = RPARM(39)
           WRITE (MSGTXT,2000) GRFACA
         ELSE
           GRFACA = 1.0 / (8.0 * SQRT(2.0))
           WRITE (MSGTXT,2000) GRFACA
           CALL MSGWRT (8)
           WRITE (MSGTXT,3000)
           END IF
         CALL MSGWRT (8)
      ELSE
         IF (RPARM(39).NE.0.0) THEN
           GRFACA = RPARM(39)
           WRITE (MSGTXT,2000) GRFACA
         ELSE
            GRFACA = 1.0
            WRITE (MSGTXT,2000) GRFACA
            END IF
         CALL MSGWRT (8)
C
         IF (RPARM(40).NE.0.0) THEN
            GRFACB = RPARM(40)
         ELSE
            GRFACB = 1.0
            END IF
         WRITE (MSGTXT,4000) GRFACB
         CALL MSGWRT (8)
         END IF
C
 900  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('EXTINP: FREQUENCIES MUST BE > 0')
 2000 FORMAT ('EXTINP:  First gradient image factor = ', 1PE12.5)
 3000 FORMAT ('EXTINP:  This is appropriate to a niner ''SOBL'' image')
 4000 FORMAT ('EXTINP: Second gradient image factor = ', 1PE12.5)
      END
      SUBROUTINE EXTHED (NCOLS, NROWS, BMAJ, BMIN, BPA, IERR)
C-----------------------------------------------------------------------
C     Extract some header values from depolarization ratio image
C     Outputs:
C        NCOLS     I       number of columns
C        NROWS     I       number of rows
C        BMAJ      R       beam major axis in pixels
C        BMIN      R       beam minor axis in pixels
C        BPA       R       beam position angle (degrees)
C        IERR      I       0 => OK, else no beam
C-----------------------------------------------------------------------
      DOUBLE PRECISION DUM1(128), DUM2(128), HPDP8(128)
      REAL   BMAJ, BMIN, BPA, HPDP4(256), XINC, YINC
      INTEGER   HPDP2(256), NCOLS, NROWS, IERR
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /HEADS/ DUM1, DUM2, HPDP8
      EQUIVALENCE (HPDP2, HPDP4, HPDP8)
C-----------------------------------------------------------------------
C                                       dimensions and increments
      NCOLS = HPDP2(KINAX)
      NROWS = HPDP2(KINAX+1)
      XINC = HPDP4(KRCIC)
      YINC = HPDP4(KRCIC+1)
C                                       extract beam in pixels
      BMAJ = HPDP4(KRBMJ)
      BMIN = HPDP4(KRBMN)
      BPA  = HPDP4(KRBPA)
C
      IF (BMAJ.GT.0.0 .AND. BMIN.GT.0.0) THEN
         BMAJ = BMAJ / XINC
         BMIN = BMIN / YINC
         IERR = 0
      ELSE
         WRITE (MSGTXT,1000)
         CALL MSGWRT (8)
         IERR = 1
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('EXTHED: NO BEAM IN HEADER, WE ARE GOING TO STOP')
      END
      SUBROUTINE DPFAC (FR1, FR2, BMAJ, BMIN, BPA, OPENB, FAC)
C-----------------------------------------------------------------------
C     Compute factors for the beam depolarization calculation.
C      Inputs:
C         FR1      R      Lower frequency Hz
C         FR2      R      Higher frequency Hz
C         BMAJ     R      Beam major axis in pixels
C         BMIN     R      Beam minor axis in pixels
C         BPA      R      Beam position angle (degrees), positive N-E
C         OPENB    L      .true. if second gradient image opened
C      Outputs:
C         FAC(1:4) R      used if two input images
C         FAC(5)   R      used if one input image
C-----------------------------------------------------------------------
      REAL   FAC(*), ALPHA, BETA, GAMMA, FR1, FR2, BMAJ, BMIN, BPA,
     *   LAM1, LAM2, LIGHT, LAMFAC, THETA, R180, A, B, ASQ, BSQ,
     *   BMAJSQ, BMINSQ, BMMSQ, LOGFAC, GAMMSQ, RATFAC
      LOGICAL   OPENB
      DATA LIGHT, R180 /3.0E8, 180.0/
C-----------------------------------------------------------------------
C                                        work out wavelengths in metres
      LAM1 = LIGHT / FR1
      LAM2 = LIGHT / FR2
      LAMFAC = LAM1**4 - LAM2**4
C                                        theta = angle between + x-axis
C                                       and beam
C                                        major axis, positive
C                                       c-clockwise, 0-180
      BPA = MOD (BPA, R180)
      IF (BPA.LT.0.0) BPA = BPA + 180.0
      THETA = BPA + 90.0
      THETA = MOD (THETA, R180)
C                                        angle factors
      A = COS (THETA * 1.7453293E-2)
      ASQ = A * A
      B = SIN (THETA * 1.7453293E-2)
      BSQ = B * B
C                                        beam factors
      BMAJSQ = BMAJ * BMAJ
      BMINSQ = BMIN * BMIN
      BMMSQ = BMAJSQ * BMINSQ
      ALPHA = (ASQ*BMINSQ + BSQ*BMAJSQ) / BMMSQ
      BETA  = (ASQ*BMAJSQ + BSQ*BMINSQ) / BMMSQ
      GAMMA = A * B * (BMAJSQ - BMINSQ) / BMMSQ
      GAMMSQ = GAMMA * GAMMA
      LOGFAC = 4.0 * LOG (2.0)
C                                         depolarization factors
      IF (OPENB) THEN
         FAC(1) = -LAMFAC / (LOGFAC * ALPHA)
         FAC(2) = ALPHA
         FAC(3) = GAMMA
         FAC(4) = (ALPHA * BETA - GAMMSQ)
      ELSE
         RATFAC = (ALPHA + GAMMA)**2 / (ALPHA * BETA - GAMMSQ)
         FAC(5) = -LAMFAC * (1.0 + RATFAC) / (LOGFAC * ALPHA)
         END IF
C
 999  RETURN
      END
      SUBROUTINE READIN (OPENB, LUNRGA, LUNRGB, RGADAT, RGBDAT, IERR)
C-----------------------------------------------------------------------
C     Read in a line from the input image(s)
C     Inputs:
C       OPENB     L        .true. if secons RM gradient image open
C       LUNRGA    I        LUN for first input image
C       LUNRGB    I        LUN for second input image
C     Outputs:
C       RGADAT    R        Array containing first image data
C       RGBDAT    R        Array containing second image data
C       IERR      I        0 => OK, else i/o error
C-----------------------------------------------------------------------
      REAL   RGADAT(*), RGBDAT(*)
      INTEGER   LUNRGA, LUNRGB, IERR
      LOGICAL   OPENB
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       read line from first
C                                       input image
      CALL MAPIO ('READ', LUNRGA, RGADAT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000)
         GO TO 900
         END IF
C                                              read line from second
C                                       input image
      IF (OPENB) THEN
         CALL MAPIO ('READ', LUNRGB, RGBDAT, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,2000)
            GO TO 900
            END IF
         END IF
      GO TO 990
 900  CALL MSGWRT (8)
C
 990  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('READIN: ERROR READING FROM FIRST GRADIENT IMAGE')
 2000 FORMAT ('READIN: ERROR READING FROM SECOND GRADIENT IMAGE')
      END
      SUBROUTINE LOOPIN (OPENB, NCOLS, RGADAT, RGBDAT, GRFACA, GRFACB,
     *   DPFACS, PDPDAT, DMIN, DMAX, NBLANK)
C-----------------------------------------------------------------------
C     Do the inner loop over the columns on the current row and
C     calculate the beam depolarization
C     Inputs:
C       OPENB      L       .true. if second RM image open
C       NCOLS      I       number of columns in output image
C       RGADAT     R       first RM image line of data
C       RGBDAT     R       second RM image line of data
C       GRFACA     R       scale factor for first RM image
C       GRFACB     R       scale factor for second RM image
C       DPFACS     R       5 element array of factors for depolarization
C     Outputs:
C       PDPDAT     R       output line of data
C       DMIN       R       updated data minimum
C       DMAX       R       updated data maximum
C     Input/Outputs:
C       NBLANK     R       cumulative no. of blanks
C-----------------------------------------------------------------------
      REAL   RGADAT(*), RGBDAT(*), PDPDAT(*), DPFACS(*), GRFACA, GRFACB,
     *   DMIN, DMAX, NBLANK, INDEF
      INTEGER   NCOLS, I
      LOGICAL   OPENB
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      INDEF = FBLANK
C                                           only one input image
      IF (.NOT.OPENB) THEN
         DO 100, I = 1, NCOLS
            IF (RGADAT(I).NE.INDEF) THEN
               CALL DPMM (OPENB, GRFACA, GRFACB, DPFACS, RGADAT(I),
     *                    RGBDAT(I), PDPDAT(I), DMIN, DMAX)
            ELSE
               PDPDAT(I) = INDEF
               NBLANK = NBLANK + 1.0
               END IF
 100        CONTINUE
      ELSE
C                                           two input images
         DO 200, I = 1, NCOLS
            IF (RGADAT(I).NE.INDEF .AND. RGBDAT(I).NE.INDEF) THEN
               CALL DPMM (OPENB, GRFACA, GRFACB, DPFACS, RGADAT(I),
     *                    RGBDAT(I), PDPDAT(I), DMIN, DMAX)
            ELSE
               PDPDAT(I) = INDEF
               NBLANK = NBLANK + 1.0
               END IF
 200        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE DPMM (OPENB, GRFACA, GRFACB, FACS, RGADAT, RGBDAT,
     *   PDPDAT, DMIN, DMAX)
C-----------------------------------------------------------------------
C     Find the predicted beam depolarization ratio and update the new
C   data minimum and maximum.
C
C     Inputs:
C       OPENB      L        .true. if second RM image open
C       GRFACA     R        Multiplicative factor for first RM gradient
C                           image
C       GRFACB     R        Multiplicative factor for second RM
C                           gradient image
C       FACS(1:4)  R        Factors used if second gradient image open
C       FACS(5)    R        Factor used if only first gradient image
C                           open
C       RGADAT     R        First gradient image pixel value
C       RGBDAT     R        Second gradient image pixel value
C     Outputs:
C       PDPDAT     R        Predicted beam depolarization ratio
C       DMIN       R        Updated data minimum
C       DMAX       R        Updated data maximum
C-----------------------------------------------------------------------
      REAL   GRFACA, GRFACB, FACS(*), RGADAT, RGBDAT, PDPDAT, DMIN,
     *   DMAX, WORK1, WORK2
      LOGICAL   OPENB
C-----------------------------------------------------------------------
C                                       compute DPR for 1 and 2 input
C                                       images
      IF (.NOT.OPENB) THEN
        PDPDAT = EXP ( (GRFACA * RGADAT)**2 * FACS(5) )
      ELSE
        WORK1 = FACS(2) * GRFACB * RGBDAT / (GRFACA * RGADAT)
        WORK2 = 1.0 + ( (WORK1 + FACS(3))**2 / FACS(4) )
        PDPDAT = EXP ( FACS(1) * (GRFACA * RGADAT)**2 * WORK2)
        END IF
C                                        update data min and max
      DMAX = MAX (DMAX, PDPDAT)
      DMIN = MIN (DMIN, PDPDAT)
C
 999  RETURN
      END
      SUBROUTINE CATUP (LUNRGA, LUNPDP, NBLANK, DMIN, DMAX, RGASLO,
     *   PDPSLO, IERR)
C-----------------------------------------------------------------------
C     Update catalog block and get cat slots.
C     Inputs:
C       LUNRGA   I       Logical unit number of input image
C       LUNPDP   I       Logical unit number of output image
C       NBLANK   R       Number of output pixels that are blanked
C       DMIN     R       Data minimum of output image
C       DMAX     R       Data maximum of output image
C    Outputs:
C       RGASLO   I       Cat slot of input image
C       PDPSLO   I       Cat slot of output image
C       IERR     I       Error status, 0 => OK
C-----------------------------------------------------------------------
      DOUBLE PRECISION HRGA8(128), HPDP8(128), DUM(128)
      REAL   HPDP4(256), DMIN, DMAX, NBLANK
      INTEGER   IERR, HPDP2(256), I, LUNPDP, PDPSLO, PDPVOL, IBUFF(256),
     *   RGASLO, LUNRGA
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DITB.INC'
      COMMON /HEADS/ HRGA8, DUM, HPDP8
      EQUIVALENCE (HPDP2, HPDP4, HPDP8)
C-----------------------------------------------------------------------
C                                          find cat slot of input image
      IERR = 0
      DO 100 I = 1,EFIL
         IF (FILTAB(POLUN, I).EQ.LUNRGA) THEN
            RGASLO = FILTAB(POCAT, I)
            GO TO 200
         ELSE IF (I.EQ.EFIL) THEN
            WRITE (MSGTXT,1000)
            CALL MSGWRT (8)
            IERR = 1
            GO TO 990
            END IF
 100     CONTINUE
C                                           find cat slot of output
C                                       image
 200  DO 300 I = 1,EFIL
         IF (FILTAB(POLUN, I).EQ.LUNPDP) THEN
            PDPSLO = FILTAB(POCAT, I)
            PDPVOL = FILTAB(POVOL, I)
            GO TO 400
         ELSE IF (I.EQ.EFIL) THEN
            WRITE (MSGTXT,2000)
            CALL MSGWRT (8)
            IERR = 1
            GO TO 990
            END IF
 300     CONTINUE
C                                               update disk header file
 400  HPDP4(KRDMN) = DMIN
      HPDP4(KRDMX) = DMAX
      IF (NBLANK.GT.0.0) HPDP4(KRBLK) = INDEF
      CALL CATIO ('UPDT', PDPVOL, PDPSLO, HPDP2, 'REST', IBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,3000) IERR
         CALL MSGWRT (8)
         END IF
C
 990  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CATUP: ERROR FINDING CAT SLOT OF FIRST INPUT IMAGE')
 2000 FORMAT ('CATUP: ERROR FINDING CAT SLOT OF OUTPUT IMAGE')
 3000 FORMAT ('CATUP: ERROR UPDATING CATALOGUE BLOCK, IERR=', I3)
      END
      SUBROUTINE CLOSUP (OPENB, LUNRGA, LUNRGB, LUNPDP)
C-----------------------------------------------------------------------
C     Close all open files
C     Inputs:
C       OPENB     L        .true. if second gradient image open
C       LUNRGA    I        LUN for first RM gradient image
C       LUNRGB    I        LUN for second RM gradient image
C       LUNPDP    I        LUN for predicted DPR image
C-----------------------------------------------------------------------
      INTEGER   LUNRGA, LUNRGB, LUNPDP
      LOGICAL   OPENB
C-----------------------------------------------------------------------
      CALL FILCLS (LUNRGA)
      IF (OPENB) CALL FILCLS (LUNRGB)
      CALL FILCLS (LUNPDP)
C
 999  RETURN
      END
      SUBROUTINE DEPOHI (OPENB, RGANAM, RGBNAM, PDPNAM, RGASLO,
     *   PDPSLO, BLC, TRC, FR1, FR2, GRFACA, GRFACB)
C-----------------------------------------------------------------------
C     Write the history file.  Only the history file from the first RM
C     gradient image is copied, as the other should be almost identical.
C    Inputs:
C        OPENB          L     .true. if second gradient inage open
C        RGANAM         C*36  RM gradient image namestring
C        RGBNAM         C*36  RM gradient image namestring
C        PDPNAM         C*36  Predicted DPR image namestring
C        RGASLO         I     Slot number for input image
C        PDPSLO         I     Slot number for output image
C        BLC,TRC        I     Input image window
C        FR1,FR2        R     Frequencies for DPR
C        GRFACA         R     Multiplicative factor for first gradient
C                             image
C        GRFACB         R     Multiplicative factor for second gradient
C                             image
C-----------------------------------------------------------------------
      CHARACTER   PRGNAM*6, RGANAM*36, RGBNAM*36, PDPNAM*36, HILINE*72,
     *   RANAME*12, RACLAS*6, RATYPE*2, RBNAME*12, RBCLAS*6, RBTYPE*2,
     *   DPNAME*12, DPCLAS*6, DPTYPE*2
      REAL   FR1, FR2, GRFACA, GRFACB
      INTEGER   RGASLO, PDPSLO, RGAVOL, RGBVOL, PDPVOL, IBUFF1(256),
     *   IBUFF2(256), RGASEQ, RGBSEQ, PDPSEQ, HPDP2(256), USID,
     *   IERR, NHISTF, LUNDPH, LUNRMH, BLC(*), TRC(*)
      LOGICAL   OPENB
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      COMMON /MAPHDR/ HPDP2
      DATA PRGNAM, LUNDPH, LUNRMH /'BDEPO ', 27, 28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       init HI
      NHISTF = 2
      CALL HIINIT (NHISTF)
C                                       work out some disks and
C                                       sequence nos.
      CALL WAWA2A (RGANAM, RANAME, RACLAS, RGASEQ, RATYPE, RGAVOL, USID)
      CALL WAWA2A (RGBNAM, RBNAME, RBCLAS, RGBSEQ, RBTYPE, RGBVOL, USID)
      CALL WAWA2A (PDPNAM, DPNAME, DPCLAS, PDPSEQ, DPTYPE, PDPVOL, USID)
C                                        get cat block of output image
      CALL CATIO ('READ', PDPVOL, PDPSLO, HPDP2, 'REST', IBUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 900
         END IF
C                                        copy old history file to new
      CALL HISCOP (LUNRMH, LUNDPH, RGAVOL, PDPVOL, RGASLO, PDPSLO,
     *   HPDP2, IBUFF1, IBUFF2, IERR)
      IF (IERR.GE.3) THEN
         WRITE (MSGTXT,2000) IERR
         GO TO 900
      END IF
C                                       add first RM gradient image
C                                       name
      CALL HENCO1 (PRGNAM, RANAME, RACLAS, RGASEQ, RGAVOL,
     *   LUNDPH, IBUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,3000) IERR
         GO TO 900
         END IF
C                                       add second RM gradient image
C                                       name
      IF (OPENB) THEN
         CALL HENCO1 (PRGNAM, RBNAME, RBCLAS, RGBSEQ, RGBVOL,
     *      LUNDPH, IBUFF2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,3500) IERR
            GO TO 900
            END IF
         END IF
C                                        add predicted DPR image name
      CALL HENCOO (PRGNAM, DPNAME, DPCLAS, PDPSEQ, PDPVOL,
     *   LUNDPH, IBUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,4000) IERR
         GO TO 900
         END IF
C                                        add input window
      WRITE (HILINE,5000) PRGNAM, BLC(1), BLC(2), TRC(1), TRC(2)
      CALL HIADD (LUNDPH, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,6000) IERR
         GO TO 900
         END IF
C                                        add frequencies
      WRITE (HILINE,7000) PRGNAM, FR1, FR2
      CALL HIADD (LUNDPH, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,8000) IERR
         GO TO 900
         END IF
C                                        add gradient factors
      WRITE (HILINE,9000) PRGNAM, GRFACA
      CALL HIADD (LUNDPH, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,9500) IERR
         GO TO 900
         END IF
C
      IF (OPENB) THEN
         WRITE (HILINE,9600) PRGNAM, GRFACB
         CALL HIADD (LUNDPH, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,9700) IERR
            GO TO 900
            END IF
         END IF
C                                        close history
      CALL HICLOS (LUNDPH, T, IBUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,9800) IERR
         GO TO 900
         END IF
      GO TO 990
 900  CALL MSGWRT (8)
C
 990  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DEPOHI: COULD NOT GET CATALOG BLOCK, IERR=', I3)
 2000 FORMAT ('DEPOHI: SERIOUS ERROR COPYING OLD HISTORY, IERR=', I3)
 3000 FORMAT ('DEPOHI: COULD NOT ADD FIRST RM GRADIENT IMAGE, IERR=',
     *        I3)
 3500 FORMAT ('DEPOHI: COULD NOT ADD SECOND RM GRADIENT IMAGE, IERR=',
     *        I3)
 4000 FORMAT ('DEPOHI: COULD NOT ADD PREDICTED DPR IMAGE, IERR=', I3)
 5000 FORMAT (A6, 'BLC = ', I4, ',', I4, '   TRC = ', I4, ',', I4)
 6000 FORMAT ('DEPOHI: COULD NOT ADD INPUT WINDOW, IERR=', I3)
 7000 FORMAT (A6, 'FREQ1 = ', 1PE12.5, '   FREQ2 = ', 1PE12.5)
 8000 FORMAT ('DEPOHI: COULD NOT ADD FREQUENCIES, IERR=', I3)
 9000 FORMAT (A6, '/ First rm gradient factor = ', 1PE12.5)
 9500 FORMAT ('DEPOHI: COULD NOT ADD FIRST GRADIENT FACTOR, IERR=',I3)
 9600 FORMAT (A6, '/ Second rm gradient factor = ', 1PE12.5)
 9700 FORMAT ('DEPOHI: COULD NOT ADD SECOND GRADIENT FACTOR, IERR=',
     *         I3)
 9800 FORMAT ('DEPOHI: COULD NOT CLOSE HISTORY FILE, IERR=', I3)
      END
