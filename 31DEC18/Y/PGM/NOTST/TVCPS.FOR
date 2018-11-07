LOCAL INCLUDE 'TVCPS.INC'
C                                                        Include TVCPS.
C                                       Local include for TVCPS.
C                                       Dimensions of work arrays.
C                                        MAXROW = maximum row size in
C                                                 pixels
C                                        MAXPLN = maximum number of
C                                                 grey-scale planes
C                                        MXGPLN = maximum number of
C                                                 graphics planes
C                                        MAXLUT = maximum number of
C                                                 entries in an LUT
C                                        MAXOFM = maximum number of
C                                                 entries in an OFM
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'INCS:PTVC.INC'
      INTEGER MAXROW, MAXPLN, MXGPLN, MAXLUT, MAXOFM, COLOUR
      PARAMETER (MAXROW = (MAXIMG))
      PARAMETER (MAXPLN = 16)
      PARAMETER (MXGPLN = 8)
      PARAMETER (MAXLUT = (TVMLUT)+1)
      PARAMETER (MAXOFM = (TVMOFM)+1)
C                                       WIDTH = width of output media
C                                               in points
C                                       HEIGHT = height of output media
C                                                in points
C                                       MARGIN = margin to leave at edge
C                                                of output media in
C                                                points
C                                       AGAMMA = gamma corrections for
C                                                red, green and blue
C                                                channels
C                                       ASPPT = the number of arcseconds
C                                               per printers point (set
C                                               from ASPMM); if zero
C                                               or negative then the
C                                               plot will be scaled to
C                                               fit the page.
C                                       BLC = Botton-left corner of
C                                             image if reading from
C                                             disk.
C                                       TRC = Top-right hand corner of
C                                             image if reading from
C                                             disk.
C                                       DX = column increment when
C                                            reading from disk.
C                                       DY = row increment when reading
C                                            from disk.
C                                       CATBLK = image header
C                                       IPLANE = the plane containing
C                                                the selected disk
C                                                image (if any).

      REAL      WIDTH, HEIGHT, MARGIN, AGAMMA(3), ASPPT, BLC(7), TRC(7)
      INTEGER   IPLANE, COPIES
C                                       OUTPRT = name of output file
C                                       REASON = text to add to AOC
C                                                admin. info.
      CHARACTER OUTPRT*48, REASON*40
C                                       EPSF = true if writing
C                                              encapsulated PostScript
C                                       COLOUR = true if writing a
C                                                colour image.
C                                       DOLAB = true if a label
C                                               containing the AIPS
C                                               user number is
C                                               required.
C                                       DOINV = true if the grey-
C                                               scale should be
C                                               inverted.
C                                       DODISK = true if reading image
C                                                data from disk.
      LOGICAL   EPSF, DOLAB, DOINV, DODISK, ROTATE, DOROT, ISON(MAXPLN)
C                                       LUT for image file.
      INTEGER   PSLUN
      PARAMETER (PSLUN = 10)
C
      INTEGER   LUTDAT(MAXLUT,3,MAXPLN), TVWIND(4), CATBLK(256,MAXPLN)
      INTEGER   RBITS(MAXPLN), GBITS(MAXPLN), BBITS(MAXPLN), DX, DY
      REAL      OFMDAT(MAXOFM,3)
      INTEGER   GPMASK(MXGPLN)
C
      COMMON /PSDATA/ CATBLK, AGAMMA, WIDTH, HEIGHT, MARGIN, EPSF,
     *   COLOUR, DOLAB, DOINV, DODISK, ASPPT, BLC, TRC, DX, DY, IPLANE,
     *   ROTATE, DOROT, COPIES, ISON
      COMMON /CHPARM/ OUTPRT, REASON
      COMMON /TVDATA/ LUTDAT, OFMDAT, TVWIND, RBITS, GBITS, BBITS,
     *   GPMASK
C                                                           End TVCPS.
LOCAL END
      PROGRAM TVCPS
C-----------------------------------------------------------------------
C! Copies a TV image to a PostScript file.
C# TV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000, 2002-2003, 2005, 2007-2008
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
C   TVCPS reads the current contents of an AIPS TV and writes them
C   to a PostScript file.
C-----------------------------------------------------------------------
      INTEGER   IRET, WBUFF(256)
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'TVCPS.INC'
C-----------------------------------------------------------------------
C                                       Initialize AIPS commons and
C                                       read and check input adverbs:
      CALL TVPSIN (IRET)
C
C                                       Open output file and TV device:
      IF (IRET.EQ.0) CALL PSINIT (IRET)
C                                       Copy screen image to output
C                                       file:
      IF (IRET.EQ.0) CALL CPSCRN (IRET)
C                                       Close output file and TV
C                                       device:
      IF (IRET.EQ.0) CALL PSFIN (IRET)
C                                       Terminate task:
      CALL DIE (IRET, WBUFF)
C
 999  STOP
      END
      SUBROUTINE TVPSIN (IRET)
C-----------------------------------------------------------------------
C   Initialize AIPS commons and read and check input adverbs.  Restart
C   AIPS if the DOWAIT adverb is true.
C   Output parameters:
C      IRET     I      Return status.
C                       0 -> success
C                       1 -> failed to read adverbs
C                       2 -> failed to restart AIPS
C                       3 -> bad input adverb
C   Output in /PSDATA/ (TVCPS.INC):
C      WIDTH    R      Media width in points
C      HEIGHT   R      Media width in points
C      MARGIN   R      Margin to leave around image in points
C      EPSF     L      Generate EPSF?
C      COLOUR   I      Colour image?
C   Output in /CHPARM/ (TVCPS.INC)
C      OUTPRT   C*48  Name of the image file or blank (if blank
C                      image file will not be saved).
C   Output in /TVDATA/ (TVCPS.INC)
C      GPMASK   I(MXGPLN)   GPMASK(I) is true if graphics plane (I)
C                           should be in the output image.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   SCRTCH(256), NPARM, JRET, IROUND
      CHARACTER PRGM*6
      REAL      APARM(10), ASPMM, XDOINV, XDOTV, XBLC(7), XTRC(7),
     *   XXINC, XYINC, XCOPYS
      HOLLERITH XOUTPR(12), XCODE, XREASN(6)
C
      PARAMETER (NPARM = 12 +1 +10 +1 +1 +1 +1 +7 +7 +1 +1 +6)
      PARAMETER (PRGM = 'TVCPS ')
C                                        UNITS = units of APARM(1:3)
C                                         0 = inches
C                                         1 = cm
C                                         2 = points
C                                         anything else = inches
      INTEGER   UNITS
C                                        SCALE = scaling factors for
C                                                UNITS
      REAL   SCALE(0:2)
C
      CHARACTER OPCODE*4
C
      COMMON /ADVERB/ XOUTPR, XCOPYS, APARM, XCODE, ASPMM, XDOINV,
     *   XDOTV, XBLC, XTRC, XXINC, XYINC, XREASN
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'TVCPS.INC'
      DATA SCALE /72.0, 28.35, 1.0/
C-----------------------------------------------------------------------
C                                       Initialize device
C                                       characteristics common:
      CALL ZDCHIN (.TRUE.)
C                                       Initialize catalogue pointer
C                                       common:
      CALL VHDRIN
C                                       Initialize files common:
      NSCR = 0
      NCFILE = 0
C                                       Assume success:
      IRET = 0
C                                       Get input adverbs:
      CALL GTPARM (PRGM, NPARM, RQUICK, XOUTPR, SCRTCH, JRET)
      IF (JRET.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1000) JRET
         CALL MSGWRT (8)
         END IF
C
      IF (RQUICK) THEN
C                                       Restart AIPS:
         CALL RELPOP (IRET, SCRTCH, JRET)
         IF (JRET.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT,1001) JRET
            CALL MSGWRT (8)
            END IF
         END IF
C                                       Unpack input parameters:
      IF (IRET.EQ.0) THEN
         IF (APARM(6).EQ.0.0) APARM(6) = APARM(5)
         IF (APARM(7).EQ.0.0) APARM(7) = APARM(5)
         AGAMMA(1) = APARM(5)
         AGAMMA(2) = APARM(6)
         AGAMMA(3) = APARM(7)
         CALL H2CHR (48, 1, XOUTPR, OUTPRT)
         COPIES = XCOPYS + 0.01
         IF ((COPIES.LE.0) .OR. (OUTPRT.NE.' ')) COPIES = 1
         UNITS = NINT (APARM(4))
         IF ((UNITS.LT.0).OR.(UNITS.GT.2)) UNITS = 0
         IF (APARM(1).LE.0.0) THEN
            WIDTH = 8.5 * SCALE(0)
         ELSE
            WIDTH = APARM(1) * SCALE(UNITS)
            END IF
         IF (APARM(2).LE.0.0) THEN
            HEIGHT = 11.0 * SCALE(0)
         ELSE
            HEIGHT = APARM(2) * SCALE(UNITS)
            END IF
         IF (APARM(3).LE.0.0) THEN
            MARGIN = 0.5 * SCALE(0)
         ELSE
            MARGIN = APARM(3) * SCALE(UNITS)
            END IF
         MARGIN = MIN (MARGIN, WIDTH/3)
         MARGIN = MIN (MARGIN, HEIGHT/3)
         CALL H2CHR (4, 1, XCODE, OPCODE)
         IF ((OPCODE.EQ.'GREY') .OR. (OPCODE.EQ.'GRAY') .OR.
     *      (OPCODE.EQ.'B/W ')) THEN
            COLOUR = 0
         ELSE
            COLOUR = 1
            IF ((OPCODE.EQ.'COLB') .AND. (XDOINV.LE.0.0)) COLOUR = 2
            END IF
         EPSF = .TRUE.
         ASPPT = 10.0 * ASPMM / SCALE(1)
C                                       Numeric user label, inverse
         CALL H2CHR (24, 1, XREASN, REASON)
         REASON(25:) = ' '
         DOLAB = REASON.NE.' '
         DOINV = XDOINV.GT.0.0
         DOROT = APARM(9).LE.0.0
         IF ((APARM(10).GT.0.0) .AND. (COLOUR.GT.0)) COLOUR = COLOUR + 2
C                                       Disk or TV image
         DODISK = XDOTV.LT.-1.0
         CALL RCOPY (7, XBLC, BLC)
         CALL RCOPY (7, XTRC, TRC)
         DX = IROUND (XXINC)
         DY = IROUND (XYINC)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FAILED TO READ ADVERBS: GTPARM ERROR ', I7)
 1001 FORMAT ('FAILED TO RESTART AIPS: RELPOP ERROR ', I2)
      END
      SUBROUTINE PSINIT (IRET)
C-----------------------------------------------------------------------
C   Open the TV, read the TV parameters, open the image file and write
C   the image file header.
C   Output:
C      IRET      I      Return status
C                          0 -> success
C                          1 -> could not open TV
C                          2 -> internal array too small
C                          3 -> could not read current window
C                          4 -> could not read LUTs
C                          5 -> could not read OFMs
C                          6 -> could not read active planes
C                          7 -> could not open PostScript file
C                          8 -> error locating disk image
C   Outputs in /TVDATA/ (TVCPS.INC)
C      TVWIND    I(4)   Current BLC (elements 1 and 2) and TRC
C                       (elements 3 and 4) of current TV viewport
C                       or, for disk, the image BLC (1,2) and the number
C                       of pixels (+BLC) after DX, DY in (3,4)
C      LUTDAT    I(MAXLUT, 3, MAXPLN)
C                       Red (,1,), green (,2,), and blue (,3,)
C                       LUTs for each grey-scale plane.
C      OFMDAT    R(MAXOFM, 3)
C                       Red (,1), green (,2) and blue (,3) OFMs.
C      RBITS     I(MAXPLN)
C      GBITS     I(MAXPLN)
C      BBITS     I(MAXPLN)
C                       Red, green and blue plane bit masks.
C-----------------------------------------------------------------------
      INTEGER   IRET, SCRTCH(256)
C
      INTEGER   JRET, BBOX(2,2), IPL, ICHAN, XSPLT, YSPLT, RCHANS(4),
     *   GCHANS(4), BCHANS(4), LSC, I, IWIN(4), MAG
      REAL      IGAMMA, CATR(256), DELTAX, DELTAY
      LOGICAL   UNIQUE
      EQUIVALENCE (CATBLK, CATR)
      INCLUDE   'INCS:DDCH.INC'
      INCLUDE   'INCS:DHDR.INC'
      INCLUDE   'INCS:DMSG.INC'
      INCLUDE   'INCS:DTVC.INC'
      INCLUDE   'TVCPS.INC'
C-----------------------------------------------------------------------
C                                       Assume success:
      IRET = 0
C                                       Open the TV device:
      CALL TVOPEN (SCRTCH, JRET)
      IF (JRET.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1000) JRET
         GO TO 990
         END IF
C                                       Check that the internal arrays
C                                       are large enough for this
C                                       TV device:
      IF (NGRAY.GT.MAXPLN) THEN
         IRET = 2
         WRITE (MSGTXT,1001) MAXPLN
         GO TO 990
         END IF
      IF (NGRAPH.GT.MXGPLN) THEN
         IRET = 2
         WRITE (MSGTXT,1002) MXGPLN
         GO TO 990
         END IF
      IF ((MAXINT+1).GT.MAXLUT) THEN
         IRET = 2
         WRITE (MSGTXT,1003) MAXLUT
         GO TO 990
         END IF
      IF ((OFMINP+1).GT.MAXOFM) THEN
         IRET = 2
         WRITE (MSGTXT,1004) MAXOFM
         GO TO 990
         END IF
C                                       Read current TV window:
      CALL YWINDO ('READ', TVWIND, JRET)
      IF (JRET.NE.0) THEN
         IRET = 3
         WRITE (MSGTXT,1005) JRET
         GO TO 990
         END IF
C                                       Check that the line buffer
C                                       is large enough to hold one
C                                       row from the current window:
      IF ((TVWIND(3)-TVWIND(1)+1).GT.MAXROW) THEN
         IRET = 2
         WRITE (MSGTXT,1006) MAXROW
         GO TO 990
         END IF
C                                       Read look-up table for each
C                                       grey-scale plane:
      DO 10 IPL = 1,NGRAY
         ICHAN = 2 ** (IPL-1)
         CALL YLUT ('READ', ICHAN, 4, .TRUE., LUTDAT(1,1,IPL), JRET)
         IF (JRET.NE.0) THEN
            IRET = 4
            WRITE (MSGTXT,1007) JRET
            GO TO 990
            END IF
         CALL YLUT ('READ', ICHAN, 2, .TRUE., LUTDAT(1,2,IPL), JRET)
         IF (JRET.NE.0) THEN
            IRET = 4
            WRITE (MSGTXT,1007) JRET
            GO TO 990
            END IF
         CALL YLUT ('READ', ICHAN, 1, .TRUE., LUTDAT(1,3,IPL), JRET)
         IF (JRET.NE.0) THEN
            IRET = 4
            WRITE (MSGTXT,1007) JRET
            GO TO 990
            END IF
 10      CONTINUE
C                                       set gamma corrections
      IGAMMA = TVGAMA
      IF ((AGAMMA(1).GT.0.1) .AND. (AGAMMA(1).LE.10.)) THEN
         TVGAMA = IGAMMA / AGAMMA(1)
      ELSE IF (AGAMMA(1).GE.0.0) THEN
         TVGAMA = 1.0
      ELSE
         TVGAMA = IGAMMA
         END IF
      AGAMMA(1) = TVGAMA
C                                       Read OFMs:
      CALL YOFM ('READ', 4, .TRUE., OFMDAT(1, 1), JRET)
      IF (JRET.NE.0) THEN
         IRET = 5
         WRITE (MSGTXT,1010) JRET
         GO TO 990
         END IF
      IF ((AGAMMA(2).GT.0.1) .AND. (AGAMMA(2).LE.10.)) THEN
         TVGAMA = IGAMMA / AGAMMA(2)
      ELSE IF (AGAMMA(2).GE.0.0) THEN
         TVGAMA = 1.0
      ELSE
         TVGAMA = IGAMMA
         END IF
      AGAMMA(2) = TVGAMA
      CALL YOFM ('READ', 2, .TRUE., OFMDAT(1, 2), JRET)
      IF (JRET.NE.0) THEN
         IRET = 5
         WRITE (MSGTXT,1010) JRET
         GO TO 990
         END IF
      IF ((AGAMMA(3).GT.0.1) .AND. (AGAMMA(3).LE.10.)) THEN
         TVGAMA = IGAMMA / AGAMMA(3)
      ELSE IF (AGAMMA(3).GE.0.0) THEN
         TVGAMA = 1.0
      ELSE
         TVGAMA = IGAMMA
         END IF
      AGAMMA(3) = TVGAMA
      CALL YOFM ('READ', 1, .TRUE., OFMDAT(1, 3), JRET)
      IF (JRET.NE.0) THEN
         IRET = 5
         WRITE (MSGTXT,1010) JRET
         GO TO 990
         END IF
      TVGAMA = IGAMMA
C                                       Find out which grey-scale
C                                       planes are enabled.
      CALL YSPLIT ('READ', XSPLT, YSPLT, RCHANS, GCHANS, BCHANS, .TRUE.,
     *   JRET)
      IF (JRET.NE.0) THEN
         IRET = 6
         WRITE (MSGTXT,1011) JRET
         GO TO 990
         END IF
      CALL ZGTBIT (NGRAY, RCHANS(1), RBITS)
      CALL ZGTBIT (NGRAY, GCHANS(1), GBITS)
      CALL ZGTBIT (NGRAY, BCHANS(1), BBITS)
C                                       which graphics planes
      I = TVLIMG(1) / (2 ** NGRAY)
      CALL ZGTBIT (NGRAPH, I, GPMASK)
C                                       which scroll
      LSC = 0
      DO 20 IPL = 1,NGRAY
         I = NGRAY + 1 - IPL
         ISON(I) = (RBITS(I).NE.0) .OR. (GBITS(I).NE.0) .OR.
     *      (BBITS(I).NE.0)
         IF (ISON(I)) LSC = I
         IF (RBITS(I).EQ.0) CALL FILL (MAXINT+1, 0, LUTDAT(1,1,I))
         IF (GBITS(I).EQ.0) CALL FILL (MAXINT+1, 0, LUTDAT(1,2,I))
         IF (BBITS(I).EQ.0) CALL FILL (MAXINT+1, 0, LUTDAT(1,3,I))
 20      CONTINUE
      IF (LSC.EQ.0) LSC = NGRAY + 1
C                                       zoom correction
      CALL COPY (4, TVWIND, IWIN)
      IF (TVZOOM(1).NE.0) THEN
         MAG = 1 + TVZOOM(1)
         IF (MXZOOM.GT.0) MAG = 2 ** TVZOOM(1)
         IWIN(1) = IWIN(1) - (MAG-1)/2
         IWIN(1) = (IWIN(1) - TVZOOM(2)) / MAG + TVZOOM(2)
         IWIN(3) = IWIN(3) - (MAG-1)/2
         IWIN(3) = (IWIN(3) - TVZOOM(2)) / MAG + TVZOOM(2)
         IWIN(2) = IWIN(2) + 1 - (MAG-1)/2
         IWIN(2) = (IWIN(2) - TVZOOM(3)) / MAG + TVZOOM(3)
         IWIN(4) = IWIN(4) + 1 - (MAG-1)/2
         IWIN(4) = (IWIN(4) - TVZOOM(3)) / MAG + TVZOOM(3)
         END IF
      IF (LSC.LE.NGRAY) THEN
         TVWIND(1) = IWIN(1) - TVSCRX(LSC)
         TVWIND(3) = IWIN(3) - TVSCRX(LSC)
         TVWIND(2) = IWIN(2) - TVSCRY(LSC)
         TVWIND(4) = IWIN(4) - TVSCRY(LSC)
      ELSE
         TVWIND(1) = IWIN(1) - TVSCGX
         TVWIND(3) = IWIN(3) - TVSCGX
         TVWIND(2) = IWIN(2) - TVSCGY
         TVWIND(4) = IWIN(4) - TVSCGY
         END IF
      IF (TVWIND(1).LT.1) THEN
         TVWIND(1) = TVWIND(1) + MAXXTV(1)
         TVWIND(3) = TVWIND(3) + MAXXTV(1)
         END IF
      IF (TVWIND(1).GT.MAXXTV(1)) THEN
         TVWIND(1) = TVWIND(1) - MAXXTV(1)
         TVWIND(3) = TVWIND(3) - MAXXTV(1)
         END IF
      IF (TVWIND(2).LT.1) THEN
         TVWIND(2) = TVWIND(2) + MAXXTV(2)
         TVWIND(4) = TVWIND(4) + MAXXTV(2)
         END IF
      IF (TVWIND(2).GT.MAXXTV(2)) THEN
         TVWIND(2) = TVWIND(2) - MAXXTV(2)
         TVWIND(4) = TVWIND(4) - MAXXTV(2)
         END IF
C                                       Get disk header parms
      IF (DODISK) THEN
         CALL IMFIND (ISON, BLC, TRC, DX, DY, TVWIND, DODISK, IPLANE,
     *      CATBLK, JRET)
         IF (JRET.NE.0) THEN
            IRET = 8
            WRITE (MSGTXT, 1012) JRET
            GO TO 990
            END IF
         END IF
      IF ((.NOT.DODISK) .AND. (ASPPT.GT.0.0)) THEN
C                                       Also need CATBLK for explicit
C                                       scaling.
         CALL TVFIND (NGRAY, 'MA', IPLANE, UNIQUE, CATBLK, SCRTCH,
     *         JRET)
         IF (JRET.NE.0) THEN
            IRET = 8
            WRITE (MSGTXT, 1012) JRET
            GO TO 990
            END IF
C                                       Figure out the TXINC and TYINC
C                                       used when the map was loaded:
         DELTAX = (CATBLK(IIWIN+2,1) - CATBLK(IIWIN+0,1) + 1.0)
     *      / (CATBLK(IICOR+2,1) - CATBLK(IICOR + 0,1) + 1.0)
         DELTAY = (CATBLK(IIWIN+3,1) - CATBLK(IIWIN+1,1) + 1.0)
     *      / (CATBLK(IICOR+3,1) - CATBLK(IICOR + 1,1) + 1.0)
C                                       Negative DX or DY indicates
C                                       dilation as with the TXINC and
C                                       TYINC adverbs.
         IF (DELTAX.GT.1) THEN
            DX = NINT (DELTAX)
         ELSE
            DX = -NINT (1.0/DELTAX)
            END IF
         IF (DELTAY.GT.1) THEN
            DY = NINT (DELTAY)
         ELSE
            DY = -NINT (1.0/DELTAY)
            END IF
         END IF
C                                       Calculate bounding box:
      CALL CALCBB (ASPPT, CATR, DX, DY, TVWIND, WIDTH, HEIGHT, MARGIN,
     *   DOLAB, DOROT, BBOX, ROTATE, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open PostScript file and write
C                                       prologue:
      CALL PSPLOG (PSLUN, OUTPRT, EPSF, BBOX, COLOUR, DOLAB, ROTATE,
     *   JRET)
      IF (JRET.NE.0) THEN
         IRET = 7
         WRITE (MSGTXT,1013) JRET
         GO TO 990
         END IF
C                                        Begin output page:
      CALL PSPSET (PSLUN, COLOUR, TVWIND, BBOX, DOLAB, REASON, ROTATE,
     *   JRET)
      IF (JRET.NE.0) THEN
         IRET = 7
         WRITE (MSGTXT,1014) JRET
         END IF
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CANNOT OPEN TV: TVOPEN ERROR ',I3)
 1001 FORMAT ('TOO MANY GREY-SCALE PLANES FOR TVCPS (> ',I2,')')
 1002 FORMAT ('TOO MANY GRAPHICS PLANES FOR TVCPS (> ',I2,')')
 1003 FORMAT ('LUTS TOO LARGE FOR TVCPS (> ',I4,' ENTRIES)')
 1004 FORMAT ('OFMS TOO LARGE FOR TVCPS (> ',I4,' ENTRIES)')
 1005 FORMAT ('COULD NOT READ WINDOW: YWINDO ERROR ',I2)
 1006 FORMAT ('TV WINDOW TOO WIDE (> ',I5,' PIXELS)')
 1007 FORMAT ('CANNOT READ LUT: YLUT ERROR ',I2)
 1010 FORMAT ('CANNOT READ OFM: YOFM ERROR ',I2)
 1011 FORMAT ('CANNOT READ PLANE MASK: YSPLIT ERROR ',I2)
 1012 FORMAT ('CANNOT FIND TV IMAGE CATBLK, ERROR ',I2)
 1013 FORMAT ('CANNOT OPEN OUTPUT FILE: PSPLOG ERROR ',I2)
 1014 FORMAT ('CANNOT WRITE PAGE SETUP: PSPSET ERROR ',I2)
      END
      SUBROUTINE CPSCRN (IRET)
C-----------------------------------------------------------------------
C   Copy the image on the screen to the output file.
C   Output:
C      IRET   I   Return status 0 -> success
C                    1 -> cannot read TV
C                    2 -> error writing file
C                    3 -> cannot read image file
C   The colours of graphics planes that are set for a given pixel are
C   combined by a bitwise exclusive-or operation on graphics channels 1
C   through 4.  If none of these channels is on (and requested) at a
C   pixel, the color of the lowest of graphics channels 5-8 that is on
C   and requested at that pixel is the color used.  Only if no requested
C   graphics channel is on at that pixel is the image (grey-scale) value
C   used.  The array GCOLOR holds a lookup table for the graphics
C   colours indexed by the bitmask of all requested planes with bits
C   set.  For example, if graphics planes 1 and 4 are requested and both
C   are set at a particular pixel location the red component of the
C   colour at that pixel will be GCOLOR(1, 2 ** (4 - 1) + 2 ** (1 - 1) +
C   1) which will contain the exclusive-or of the red components of the
C   colours of planes 1 and 4.  If planes 1,4,5,6 are requested, but
C   only planes 5 and 6 are on at that pixel, then the color of graphics
C   plane 5 is used.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   JRET, I, J, IROW, WWIDTH, WHT, PIXELS, OLINES, DELBLK,
     *   ITRIM, WIN(4), BUFSZ, LR, LG, LB, NPIX, ZEOR, L, IPL, IW,
     *   BITS(32), LCOLOR(3), LGR, MGR, LROW, NP(2), IX0(2), NW, IX1(2)
      REAL      RED, GREEN, BLUE
      LOGICAL   T
C                                       PSBUFF = PostScript buffer
      CHARACTER PSBUFF*80, PHNAME*48
C                                       Scaling factor from OFM output
C                                       to film recorder input.
      REAL      SCALE
      PARAMETER (SCALE = 255.0)
      INCLUDE 'TVCPS.INC'
      INTEGER   ROUT(MAXROW), GOUT(MAXROW), BOUT(MAXROW),
     *   GCOLOR(3,2**MXGPLN), GROW(MAXROW), BCOLOR(3,MXGPLN),
     *   LUN(MAXPLN), IND(MAXPLN), ININD(MAXPLN), ROW(MAXROW,MAXPLN)
      REAL      TEMP, BUFF(MABFSS,MAXPLN), CATR(256,MAXPLN)
      HOLLERITH CATH(256,MAXPLN)
      CHARACTER TRANFN(MAXPLN)*2
      INCLUDE   'INCS:DDCH.INC'
      INCLUDE   'INCS:DMSG.INC'
      INCLUDE   'INCS:DTVC.INC'
      INCLUDE   'INCS:DHDR.INC'
      EQUIVALENCE (CATBLK, CATR, CATH)
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      IRET = 0
      BUFSZ = MABFSS * 2
C                                       Calculate window width and
C                                       height:
      WWIDTH = TVWIND(3) - TVWIND(1) + 1
      WHT = TVWIND(4) - TVWIND(2) + 1
C                                       Calculate number of pixels:
      PIXELS = WWIDTH * WHT
C                                       Calculate number of lines of
C                                       output (if we tell the
C                                       PostScript interpreter the
C                                       number of lines of data it
C                                       can avoid parsing the data):
      IF (COLOUR.GT.0) THEN
         OLINES = (PIXELS + 11) / 12 + 1
      ELSE
         OLINES = (PIXELS + 35) / 36 + 1
         END IF
C                                       Write out PostScript
C                                       operators:
      WRITE (PSBUFF,5000) WWIDTH, WHT, WWIDTH, WHT
      CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      PSBUFF = '{currentfile picst readhexstring pop}'
      CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (COLOUR.GT.0) THEN
         PSBUFF = 'false 3'
         IF (COLOUR.GT.2) PSBUFF = 'false 4'
         CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      WRITE (PSBUFF,5001) OLINES
      CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (COLOUR.GT.0) THEN
         PSBUFF = 'colorimage'
      ELSE
         PSBUFF = 'image'
         END IF
      CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Image from the TV
      IF (.NOT.DODISK) THEN
         NW = 1
         IX0(1) = TVWIND(1)
         IX1(1) = 1
         NP(1) = TVWIND(3) - TVWIND(1) + 1
         IF (TVWIND(3).GT.MAXXTV(1)) THEN
            NW = 2
            NP(1) = MAXXTV(1) - IX0(1) + 1
            IX0(2) = 1
            IX1(2) = 1 + NP(1)
            NP(2) = TVWIND(3) - MAXXTV(1)
            END IF
C                                       Set graphics plane colours:
         DO 10 I = 1,NGRAPH
            CALL YGRAFX ('READ', I, RED, GREEN, BLUE, JRET)
            IF (JRET.NE.0) THEN
               IRET = 1
               WRITE (MSGTXT,1000) JRET
               GO TO 990
               END IF
            BCOLOR(1,I) = SCALE * RED
            BCOLOR(2,I) = SCALE * GREEN
            BCOLOR(3,I) = SCALE * BLUE
 10         CONTINUE
C                                       Do ex-or colors
         LGR = 2**NGRAPH
         MGR = 3 * LGR
         CALL FILL (MGR, 0, GCOLOR)
         MGR = NGRAPH
         IF (NGRAPH.EQ.8) MGR = 4
         DO 13 I = 1,LGR
            J = I - 1
            CALL ZGTBIT (NGRAPH, J, BITS)
            LCOLOR(1) = 0
            LCOLOR(2) = 0
            LCOLOR(3) = 0
C                                       limit to planes 1-4 for XAS
            DO 12 J = 1,MGR
               IF (BITS(J).EQ.1) THEN
                  LCOLOR(1) = ZEOR (LCOLOR(1), BCOLOR(1,J))
                  LCOLOR(2) = ZEOR (LCOLOR(2), BCOLOR(2,J))
                  LCOLOR(3) = ZEOR (LCOLOR(3), BCOLOR(3,J))
                  END IF
 12            CONTINUE
            TEMP = (LCOLOR(1) / SCALE) ** AGAMMA(1)
            TEMP = MAX (0.0, MIN (1.0, TEMP))
            GCOLOR(1,I) = SCALE * TEMP + 0.5
            TEMP = (LCOLOR(2) / SCALE) ** AGAMMA(2)
            TEMP = MAX (0.0, MIN (1.0, TEMP))
            GCOLOR(2,I) = SCALE * TEMP + 0.5
            TEMP = (LCOLOR(3) / SCALE) ** AGAMMA(3)
            TEMP = MAX (0.0, MIN (1.0, TEMP))
            GCOLOR(3,I) = SCALE * TEMP + 0.5
 13         CONTINUE
C                                       other colors planes 5-8
         IF (MGR.NE.NGRAPH) THEN
            DO 15 I = 17,LGR,16
               J = I - 1
               CALL ZGTBIT (NGRAPH, J, BITS)
               DO 14 J = 5,8
                  IF (BITS(13-J).EQ.1) THEN
                     LCOLOR(1) = BCOLOR(1,13-J)
                     LCOLOR(2) = BCOLOR(2,13-J)
                     LCOLOR(3) = BCOLOR(3,13-J)
                     END IF
 14               CONTINUE
               TEMP = (LCOLOR(1) / SCALE) ** AGAMMA(1)
               TEMP = MAX (0.0, MIN (1.0, TEMP))
               GCOLOR(1,I) = SCALE * TEMP + 0.5
               TEMP = (LCOLOR(2) / SCALE) ** AGAMMA(2)
               TEMP = MAX (0.0, MIN (1.0, TEMP))
               GCOLOR(2,I) = SCALE * TEMP + 0.5
               TEMP = (LCOLOR(3) / SCALE) ** AGAMMA(3)
               TEMP = MAX (0.0, MIN (1.0, TEMP))
               GCOLOR(3,I) = SCALE * TEMP + 0.5
 15            CONTINUE
            END IF
C                                       Adjust to bright white or black
C                                       if producing a greyscale image.
         IF (COLOUR.LE.0) THEN
            J = SCALE/2
            DO 18 I = 1,LGR
               IF ((GCOLOR(1,I).GT.J) .OR. (GCOLOR(2,I).GT.J) .OR.
     *            (GCOLOR(3,I).GT.J)) THEN
                  GCOLOR(1,I) = SCALE
                  GCOLOR(2,I) = SCALE
                  GCOLOR(3,I) = SCALE
                  END IF
 18            CONTINUE
            END IF
C                                       Read each row in the window:
         DO 100 IROW = TVWIND(2),TVWIND(4)
            LROW = IROW
            IF (LROW.LT.1) LROW = LROW + MAXXTV(2)
            IF (LROW.GT.MAXXTV(2)) LROW = LROW - MAXXTV(2)
C                                       Clear accumulators:
            DO 20 I = 1,WWIDTH
               ROUT(I) = 0
               GOUT(I) = 0
               BOUT(I) = 0
               GROW(I) = 0
 20            CONTINUE
C                                       Read grey-scale planes:
C                                       (Inner loop should vectorize.)
C                                       (Outer loop may be
C                                       parallelized.)
            DO 40 I = 1,NGRAY
               IF (ISON(I)) THEN
                  DO 25 IW = 1,NW
                     CALL YIMGIO ('READ', I, IX0(IW), LROW, 0, NP(IW),
     *                  ROW(IX1(IW),1), JRET)
                     IF (JRET.NE.0) THEN
                        IRET = 1
                        WRITE (MSGTXT,1020) JRET
                        GO TO 990
                        END IF
 25                  CONTINUE
                  DO 30 J = 1,WWIDTH
                     ROUT(J) = ROUT(J) + LUTDAT(ROW(J,1)+1,1,I)
                     GOUT(J) = GOUT(J) + LUTDAT(ROW(J,1)+1,2,I)
                     BOUT(J) = BOUT(J) + LUTDAT(ROW(J,1)+1,3,I)
 30                  CONTINUE
                  END IF
 40            CONTINUE
C                                       Apply OFM (vectorizable):
            DO 50 I = 1,WWIDTH
               ROUT(I) = NINT (SCALE * OFMDAT(ROUT(I)+1, 1))
               GOUT(I) = NINT (SCALE * OFMDAT(GOUT(I)+1, 2))
               BOUT(I) = NINT (SCALE * OFMDAT(BOUT(I)+1, 3))
 50            CONTINUE
C                                       Add in requested graphics
C                                       overlays:
            DO 60 I = 1,NGRAPH
               IF (GPMASK(I).GT.0) THEN
                  DO 52 IW = 1,NW
                     CALL YIMGIO ('READ', I+NGRAY, IX0(IW), LROW, 0,
     *                  NP(IW), ROW(IX1(IW),1), JRET)
                     IF (JRET.NE.0) THEN
                        IRET = 1
                        WRITE (MSGTXT,1020) JRET
                        GO TO 990
                        END IF
 52                  CONTINUE
                  L = 2 ** (I-1)
                  DO 55 J = 1,WWIDTH
                     GROW(J) = GROW(J) + ROW(J,1) * L
 55                  CONTINUE
                  END IF
 60            CONTINUE
            DO 70 J = 1,WWIDTH
               IF (GROW(J).NE.0) THEN
                  ROUT(J) = GCOLOR(1,GROW(J)+1)
                  GOUT(J) = GCOLOR(2,GROW(J)+1)
                  BOUT(J) = GCOLOR(3,GROW(J)+1)
                  END IF
 70            CONTINUE
C                                       Write out the finished row:
            CALL WRLINE ('WRIT', PSLUN, COLOUR, ROUT, GOUT, BOUT,
     *         WWIDTH, DOINV, JRET)
            IF (JRET.NE.0) THEN
               IRET = 2
               WRITE (MSGTXT,1070) JRET
               GO TO 990
               END IF
 100        CONTINUE
C                                       Image from disk
      ELSE
         WIN(1) = BLC(1) + 0.0001
         WIN(2) = BLC(2) + 0.0001
         WIN(3) = TRC(1) + 0.0001
         WIN(4) = TRC(2) + 0.0001
         NPIX = 1 + WIN(3) - WIN(1)
         WWIDTH = 1 + (WIN(3) - WIN(1)) / DX
         DO 105 IPL = 1,NGRAY
            IF (ISON(IPL)) THEN
               CALL COMOFF (CATBLK(KIDIM,IPL), CATBLK(KINAX,IPL),
     *            CATBLK(IIDEP,IPL), DELBLK, JRET)
               DELBLK = DELBLK + 1
               CALL H2CHR (2, 1, CATH(IITRA,IPL), TRANFN(IPL))
               CALL ZPHFIL ('MA', CATBLK(IIVOL,IPL), CATBLK(IICNO,IPL),
     *            1, PHNAME, JRET)
               LUN(IPL) = 28 + IPL
               CALL ZOPEN (LUN(IPL), IND(IPL), CATBLK(IIVOL,IPL),
     *            PHNAME, T, T, T, JRET)
               IF (JRET.NE.0) THEN
                  WRITE (MSGTXT,1105) JRET, 'OPEN'
                  GO TO 980
                  END IF
               CALL MINIT ('READ', LUN(IPL), IND(IPL),
     *            CATBLK(KINAX,IPL), CATBLK(KINAX+1,IPL), WIN,
     *            BUFF(1,IPL), BUFSZ, DELBLK, JRET)
               IF (JRET.NE.0) THEN
                  WRITE (MSGTXT,1105) JRET, 'INIT'
                  GO TO 980
                  END IF
               END IF
 105        CONTINUE
         DO 130 J = WIN(2),WIN(4)
            DO 110 IPL = 1,NGRAY
               IF (ISON(IPL)) THEN
                  CALL MDISK ('READ', LUN(IPL), IND(IPL), BUFF(1,IPL),
     *               ININD(IPL), JRET)
                  IF (JRET.NE.0) THEN
                     WRITE (MSGTXT,1105) JRET, 'READ'
                     GO TO 980
                     END IF
                  IF (MOD(J-WIN(2),DY).EQ.0) CALL ISCALE (TRANFN(IPL),
     *               MAXINT, CATR(IRRAN,IPL), NPIX, DX,
     *               BUFF(ININD(IPL),IPL), ROW(1,IPL))
                  END IF
 110           CONTINUE
            IF (MOD(J-WIN(2),DY).EQ.0) THEN
               DO 120 I = 1,WWIDTH
                  LR = 0
                  LG = 0
                  LB = 0
                  DO 115 IPL = 1,NGRAY
                     IF (ISON(IPL)) THEN
                        LR = LR + LUTDAT(ROW(I,IPL)+1,1,IPL)
                        LG = LG + LUTDAT(ROW(I,IPL)+1,2,IPL)
                        LB = LB + LUTDAT(ROW(I,IPL)+1,3,IPL)
                        END IF
 115                 CONTINUE
                  ROUT(I) = NINT (SCALE * OFMDAT(LR+1, 1))
                  GOUT(I) = NINT (SCALE * OFMDAT(LG+1, 2))
                  BOUT(I) = NINT (SCALE * OFMDAT(LB+1, 3))
 120              CONTINUE
C                                       Write out the finished row:
               CALL WRLINE ('WRIT', PSLUN, COLOUR, ROUT, GOUT, BOUT,
     *            WWIDTH, DOINV, JRET)
               IF (JRET.NE.0) THEN
                  IRET = 2
                  WRITE (MSGTXT,1070) JRET
                  GO TO 990
                  END IF
               END IF
 130        CONTINUE
         END IF
C                                       Flush image data:
      CALL WRLINE ('FLSH', PSLUN, COLOUR, ROUT, GOUT, BOUT, WWIDTH,
     *   DOINV, JRET)
      IF (JRET.NE.0) THEN
         IRET = 2
         WRITE (MSGTXT,1070) JRET
         GO TO 990
         END IF
      PSBUFF = '%%EndData'
      CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
      GO TO 999
C
 980  IRET = 3
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CANNOT READ TV: YGRAFX ERROR ',I4)
 1020 FORMAT ('CANNOT READ TV: YIMGIO ERROR ',I2)
 1070 FORMAT ('CANNOT WRITE OUTPUT: WRLINE ERROR ',I2)
 1105 FORMAT ('ERROR',I5,' ',A,'ING DISK FILE')
 5000 FORMAT (I4,1X,I4,' 8 [',I4,' 0 0 ',I4,' 0 0 ]')
 5001 FORMAT ('%%BeginData: ',I10,' decimal lines')
      END
      SUBROUTINE PSFIN (IRET)
C-----------------------------------------------------------------------
C   Close the output file and TV device.
C
C   Output:
C      IRET        I         Return status
C                              0 -> success
C                              1 -> could not close TV
C                              2 -> could not close output file
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   JRET, SCRTCH(256), I
      CHARACTER PSBUFF*80
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'TVCPS.INC'
C
      INTEGER   ITRIM
      EXTERNAL  ITRIM
C-----------------------------------------------------------------------
C                                       Close TV:
      CALL TVCLOS (SCRTCH, JRET)
      IF (JRET.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1000) JRET
         CALL MSGWRT (8)
         END IF
C                                       Finish off the PostScript file
      PSBUFF = 'pgsave restore'
      CALL ZLASIO('WRIT', PSLUN, OUTPRT, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (COPIES.GT.1) THEN
         PSBUFF = 'copypage'
         DO 20 I = 2,COPIES
            CALL ZLASIO('WRIT', PSLUN, OUTPRT, ITRIM(PSBUFF), PSBUFF,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
 20         CONTINUE
         END IF
      PSBUFF = 'showpage'
      CALL ZLASIO('WRIT', PSLUN, OUTPRT, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      PSBUFF = 'userdict /eop-hook known {eop-hook} if'
      CALL ZLASIO('WRIT', PSLUN, OUTPRT, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      PSBUFF = '%%Trailer'
      CALL ZLASIO('WRIT', PSLUN, OUTPRT, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      PSBUFF = 'userdict /end-hook known {end-hook} if'
      CALL ZLASIO('WRIT', PSLUN, OUTPRT, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      PSBUFF = '%%EOF'
      CALL ZLASIO('WRIT', PSLUN, OUTPRT, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL ZLASIO('CLOS', PSLUN, OUTPRT, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CAN NOT CLOSE TV: TVCLOS ERROR ', I3)
      END
      SUBROUTINE IMFIND (ISON, BLC, TRC, DX, DY, TVWIND, DODISK, IPLANE,
     *   CATBLK, IRET)
C-----------------------------------------------------------------------
C   find which image is on and get its catalog block (with the TV load
C   modifications).
C   Input/Outputs:
C      ISON     L(*)       Which planes are on
C      BLC      R(7)       BLC of image window
C      TRC      R(7)       TRC of image window
C      DX       I          Increment in X pixels
C      DY       I          Increment in Y pixels
C      TVWIND   I(4)       Returns 1,1,nx,ny to give display size IF
C                          DODISK is returned true.
C   Outputs:
C      DODISK   L          Returns False if TV image has no disk file
C      IPLANE   I          TV plane to use for image LUTs
C      CATBLK   I(256,*)   Image header
C      IRET     I          Error code: 0 okay
C                            1 - can't find an image
C                            2 - requested window bad
C-----------------------------------------------------------------------
      LOGICAL   ISON(*)
      REAL      BLC(7), TRC(7)
      INTEGER   DX, DY, TVWIND(4), IPLANE, CATBLK(256,*), IRET
      LOGICAL   DODISK
C
      INTEGER   IX, IY, INBUF(256), JRET, TSTCAT(256), MSGSAV, I, IPL,
     *   IERR
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       find the image header(s)
      DODISK = .FALSE.
      IPLANE = 1000
      DO 20 IPL = 1,NGRAY
         IF (ISON(IPL)) THEN
            IX = (TVWIND(1) + TVWIND(3)) / 2
            IY = (TVWIND(2) + TVWIND(4)) / 2
            CALL YCREAD (IPL, IX, IY, CATBLK(1,IPL), IERR)
            IF (IERR.NE.0) ISON(IPL) = .FALSE.
            END IF
         IF (ISON(IPL)) THEN
C                                       test the image catalog
            IF ((CATBLK(IIVOL,IPL).GT.0) .AND.
     *         (CATBLK(IIVOL,IPL).LE.NVOL)) THEN
               MSGSAV = MSGSUP
               MSGSUP = 32000
               CALL CATIO ('READ', CATBLK(IIVOL,IPL), CATBLK(IICNO,IPL),
     *            TSTCAT, 'REST', INBUF, JRET)
               MSGSUP = MSGSAV
C                                       test header
               IX = 112
               IF ((JRET.EQ.0) .OR. (JRET.GT.5)) THEN
                  DO 10 I = 1,112
                     IF (TSTCAT(I).EQ.CATBLK(I,IPL)) IX = IX - 1
 10                  CONTINUE
                  IF (IX.GT.0) THEN
                     MSGTXT = 'Disk header does not match image catalog'
     *                  // ' header - USING TV IMAGE'
                     CALL MSGWRT (6)
                     ISON(IPL) = .FALSE.
                     END IF
               ELSE
                  MSGTXT = 'Unable to read specified disk header' //
     *               ' - USING TV IMAGE'
                  CALL MSGWRT (6)
                  ISON(IPL) = .FALSE.
                  END IF
C                                       wasn't on disk, use TV
            ELSE
               MSGTXT = 'REQUESTED IMAGE DOES NOT COME FROM DISK' //
     *            ' - USING TV IMAGE'
               CALL MSGWRT (6)
               ISON(IPL) = .FALSE.
               END IF
            END IF
         IF (ISON(IPL)) THEN
            DODISK = .TRUE.
            IPLANE = MIN (IPLANE, IPL)
            END IF
 20      CONTINUE
C                                       set windows for disk file
      IF (DODISK) THEN
         CALL WINDOW (CATBLK(KIDIM,IPLANE), CATBLK(KINAX,IPLANE), BLC,
     *      TRC, JRET)
         IF (JRET.NE.0) THEN
            IRET = 2
            GO TO 999
            END IF
         CALL RCOPY (5, BLC(3), TRC(3))
         DX = MAX (1, DX)
         DY = MAX (1, DY)
         TVWIND(1) = 1
         TVWIND(2) = 1
         TVWIND(3) = (TRC(1) - BLC(1)) / DX + 1.001
         TVWIND(4) = (TRC(2) - BLC(2)) / DY + 1.001
C                                       compare multiple images
         DO 50 IPL = 1,NGRAY
            IF ((ISON(IPL)) .AND. (IPL.NE.IPLANE)) THEN
               IF ((CATBLK(KINAX,IPL).NE.CATBLK(KINAX,IPLANE)) .OR.
     *            (CATBLK(KINAX+1,IPL).NE.CATBLK(KINAX+1,IPLANE))) THEN
                  MSGTXT = 'IMAGES ARE NOT OF SAME SIZE'
                  CALL MSGWRT (8)
                  DODISK = .FALSE.
                  END IF
               END IF
 50         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE CALCBB (ASPPT, CATR, DX, DY, TVWIND, WIDTH, HEIGHT,
     *   MARGIN, DOLAB, DOROT, BBOX, ROTATE, IRET)
C-----------------------------------------------------------------------
C   Calculate the largest centered bounding box that will hold a scaled
C   image of the current TV window with a mimimum margin of MARGIN on
C   each side while preserving the TV aspect ratio.
C
C   Inputs:
C      ASPPT    R        Requested scale in arcsec per pt; if negative
C                        scale to fit page.
C      CATBLK   R(256)   Header for image
C      DX       I        X-axis pixel increment
C      DY       I        Y-axis pixel increment
C      TVWIND   I(4)     Current TV window (from YWINDO)
C      WIDTH    R        Width of output media (points)
C      HEIGHT   R        Height of output media (points)
C      MARGIN   R        Minimum margin (points)
C      DOLAB    L        Should space be reserved for a label.
C
C   Outputs:
C      BBOX     I(2, 2)  BLC and TRC of bounding box (points)
C      IRET     I        Return status: 0 => box OK
C                                       1 => box won't fit page
C-----------------------------------------------------------------------
      INTEGER   DX, DY, TVWIND(4), BBOX(2, 2), IRET
      REAL      ASPPT, CATR(256), WIDTH, HEIGHT, MARGIN, XSZ, YSZ
      LOGICAL   DOLAB, ROTATE, DOROT
C                                        ASPECT = TV aspect ratio
      REAL      ASPECT
C                                        MAXWID = maximum image width
C                                        MAXHT  = maximum image height
      REAL      MAXWID, MAXHT, PCH
C                                        SCALE  = scaling factor
      REAL      SCALE
C                                       XINC = X-axis arcsec/pixel
C                                       YINC = Y-axis arcsec/pixel
      REAL      XINC, YINC
C
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      MAXWID = WIDTH - 2.0 * MARGIN
      MAXHT  = HEIGHT - 2.0 * MARGIN
C                                        Reserve space for a label, if
C                                        required:
      PCH = 0
      IF (DOLAB) PCH = 25
C
      IF (ASPPT.GT.0.0) THEN
C                                       Explicit scaling.
C
C                                       Assume first two axes of image
C                                       are the x and y axes:
         XINC = ABS (CATR(KRCIC)) * 3600.0
         YINC = ABS (CATR(KRCIC + 1)) * 3600.0
C                                       Compensate for sampling:
         IF (DX.GT.0) THEN
            XINC = XINC * DX
         ELSE
            XINC = -XINC / DX
            END IF
         IF (DY.GT.0) THEN
            YINC = YINC * DX
         ELSE
            YINC = -YINC / DX
            END IF
C                                       Check that image will fit page:
         XSZ = XINC * (TVWIND(3) - TVWIND(1)) / ASPPT
         YSZ = YINC * (TVWIND(4) - TVWIND(2)) / ASPPT
         IF ((XSZ.GT.MAXWID) .OR. (YSZ.GT.MAXHT-PCH)) THEN
            IF ((XSZ.GT.MAXHT) .OR. (YSZ.GT.MAXWID-PCH) .OR.
     *         (.NOT.DOROT)) THEN
               MSGTXT = 'IMAGE WILL NOT FIT ON PAGE - REDUCE ASPMM'
               IRET = 1
               CALL MSGWRT (10)
               GO TO 999
C                                       Only rotated fits
            ELSE
               ROTATE = .TRUE.
C                                       Calculate BLC of bounding box:
               BBOX(1,1) = INT ((WIDTH - YSZ) / 2.0)
               BBOX(2,1) = INT ((HEIGHT - XSZ) / 2.0)
               END IF
C                                       Normal positioning
         ELSE
            ROTATE = .FALSE.
C                                       Calculate BLC of bounding box:
            BBOX(1,1) = INT ((WIDTH - XSZ) / 2.0)
            BBOX(2,1) = INT ((HEIGHT - YSZ) / 2.0)
            END IF
      ELSE
C                                       Scale to fit page:
         XSZ = TVWIND(3) - TVWIND(1) + 1
         YSZ = TVWIND(4) - TVWIND(2) + 1
C                                        Calculate scaling factor:
         SCALE = MAXWID / XSZ
         IF (YSZ*SCALE.GT.MAXHT-PCH) SCALE = (MAXHT-PCH) / YSZ
         ASPECT = (MAXWID-PCH) / YSZ
         IF (XSZ*ASPECT.GT.MAXHT) ASPECT = MAXHT / XSZ
C                                        Calculate BLC of bounding box:
         IF ((ASPECT.GT.SCALE) .AND. (DOROT)) THEN
            ROTATE = .TRUE.
            BBOX(1,1) = INT ((WIDTH - ASPECT*YSZ) / 2.0)
            BBOX(2,1) = INT ((HEIGHT - ASPECT*XSZ) / 2.0)
         ELSE
            ROTATE = .FALSE.
            BBOX(1,1) = INT ((WIDTH - SCALE*XSZ) / 2.0)
            BBOX(2,1) = INT ((HEIGHT - SCALE*YSZ) / 2.0)
            END IF
         END IF
C                                        Calculate TRC of bounding box:
      BBOX(1,2) = INT (WIDTH - BBOX(1,1))
      BBOX(2,2) = INT (HEIGHT - BBOX(2,1))
C                                        Adjust vertical positioning
C                                        for label.
      IF (DOLAB) THEN
         IF (ROTATE) THEN
            BBOX(1,1) = BBOX(1,1) - 12.5
            BBOX(1,2) = BBOX(1,2) - 12.5
         ELSE
            BBOX(2,1) = BBOX(2,1) + 12.5
            BBOX(2,2) = BBOX(2,2) + 12.5
            END IF
         END IF
C
  999 RETURN
      END
      SUBROUTINE PSPLOG (LUN, OUTFIL, ENCAP, BBOX, COLOUR, DOLAB,
     *   ROTATE, IRET)
C-----------------------------------------------------------------------
C   Open a print file with name OUTFIL on logical unit number LUN and
C   write a PostScript prologue to it. The prologue conforms to version
C   3.0 of the PostScript document structuring conventions.
C
C   Inputs:
C      LUN       I           Logical unit number
C      OUTFIL    C*48        File name
C      ENCAP     L           True if and only if writing encapsulated
C                             PostScript
C      BBOX      I(2,2)      BLC and TRC or bounding box in points
C      COLOUR    I           True if and only if the document will use
C                             CMYK extensions (eg. colorimage)
C      DOLAB     L           True if and only if a label is to be
C                             added to the plot.
C
C   Outputs:
C      IRET      I           Status code: 0 => OK
C-----------------------------------------------------------------------
      INTEGER   LUN, BBOX(2,2), COLOUR, IRET
      CHARACTER OUTFIL*48
      LOGICAL   ENCAP, DOLAB, ROTATE
C                                        CBUFF = line buffer
      CHARACTER CBUFF*80
C                                        DATE = current date (year,
C                                               month and day)
C                                        TIME = current time (hours,
C                                               minutes and seconds)
      INTEGER   DATE(3), TIME(3)
C                                        NEWBOX = bounding box after
C                                                 allowing for a
C                                                 label.
      INTEGER   NEWBOX(2, 2)
C
      INTEGER   I, J
C
      INTEGER   ITRIM
      EXTERNAL  ITRIM
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                        Open the output file:
      CALL ZLASIO ('OPEN', LUN, OUTFIL, 0, CBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Write %!PS-Adobe comment:
      CBUFF = '%!PS-Adobe-3.0'
      IF (ENCAP) CBUFF = '%!PS-Adobe-3.0 EPSF-3.0'
      CALL ZLASIO ('WRIT', LUN, OUTFIL, ITRIM(CBUFF), CBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        If a label will be added
C                                        note that the Helvetica
C                                        font will be needed:
      IF (DOLAB) THEN
         CBUFF = '%%DocumentNeededResources: font Helvetica'
         CALL ZLASIO ('WRIT', LUN, OUTFIL, ITRIM(CBUFF), CBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                        Write bounding box comment:
      DO 20 I = 1,2
         DO 10 J = 1,2
            NEWBOX(I,J) = BBOX(I,J) + 5 * (2*J-3)
   10       CONTINUE
   20    CONTINUE
      IF (DOLAB) THEN
         IF (ROTATE) THEN
            NEWBOX(1,2) = NEWBOX(1,2) + 25
         ELSE
            NEWBOX(2,1) = NEWBOX(2,1) - 25
            END IF
         END IF
      WRITE (CBUFF, 1000) NEWBOX
      CALL ZLASIO ('WRIT', LUN, OUTFIL, ITRIM(CBUFF), CBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Write creator comment:
      CBUFF = '%%Creator: AIPS task TVCPS'
      CALL ZLASIO ('WRIT', LUN, OUTFIL, ITRIM(CBUFF), CBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Write creation date comment:
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      WRITE (CBUFF, 1001) DATE, TIME
      CALL ZLASIO ('WRIT', LUN, OUTFIL, ITRIM(CBUFF), CBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Write document data comment:
      CBUFF = '%%DocumentData: Clean7Bit'
      CALL ZLASIO ('WRIT', LUN, OUTFIL, ITRIM(CBUFF), CBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Write extensions comment (if
C:                                       required):
      IF (COLOUR.GT.0) THEN
         CBUFF = '%%Extensions: CMYK'
         CALL ZLASIO ('WRIT', LUN, OUTFIL, ITRIM(CBUFF), CBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         CBUFF = '%%Requirements: color'
         CALL ZLASIO ('WRIT', LUN, OUTFIL, ITRIM(CBUFF), CBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                        Write for comment:
      WRITE (CBUFF, 1002) NLUSER
      CALL ZLASIO ('WRIT', LUN, OUTFIL, ITRIM(CBUFF), CBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Write pages comment:
      CBUFF = '%%Pages: 1'
      CALL ZLASIO ('WRIT', LUN, OUTFIL, ITRIM(CBUFF), CBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        If a label is required then
C                                        the label font must be
C                                        included in the document
C                                        setup.
      IF (DOLAB) THEN
         CBUFF = '%%BeginSetup'
         CALL ZLASIO ('WRIT', LUN, OUTFIL, ITRIM(CBUFF), CBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         CBUFF = '%%IncludeResource: font Helvetica'
         CALL ZLASIO ('WRIT', LUN, OUTFIL, ITRIM(CBUFF), CBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         CBUFF = '%%EndSetup'
         CALL ZLASIO ('WRIT', LUN, OUTFIL, ITRIM(CBUFF), CBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                        Finish header section:
      CBUFF = '%%EndComments'
      CALL ZLASIO ('WRIT', LUN, OUTFIL, ITRIM(CBUFF), CBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Write a null prologue:
      CBUFF = '%%EndProlog'
      CALL ZLASIO ('WRIT', LUN, OUTFIL, ITRIM(CBUFF), CBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C
      IRET = 0
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('%%BoundingBox: ', 4I5)
 1001 FORMAT ('%%CreationDate: ', I4.4, '-', I2.2, '-', I2.2, 'T',
     *   I2.2, ':', I2.2, ':', I2.2)
 1002 FORMAT ('%%For: AIPS user number ', I4)
      END
      SUBROUTINE PSPSET (LUN, COLOUR, TVWIND, BBOX, DOLAB, REASON,
     *   ROTATE, IRET)
C-----------------------------------------------------------------------
C   Begin new page and write out everything except for the sctual
C   image.
C   Inputs:
C      LUN      I        LUN of open printer file
C      COLOUR   I        true if producing RGB output
C      TVWIND   I(4)     TV window (from YWINDO)
C      BBOX     I(2,2)   Bounding box for image
C      DOLAB    L        True if a label should be added.
C      REASON   C*40     Character label to put on plot
C   Output
C      IERR     I        Status: 0 => OK
C                                non-zero => ZLASIO error
C-----------------------------------------------------------------------
      INTEGER   LUN, TVWIND(4), BBOX(2,2), IRET, COLOUR
      LOGICAL   DOLAB, ROTATE
      CHARACTER REASON*(*)
C
C                                        CBUFF = character buffer
      CHARACTER CBUFF*80
C                                        FSIZE = font size for label
C                                                in points
      INTEGER   FSIZE, ITRIM, I
C                                        LWIDTH = width of the label
C                                                 in points for 20pt
C                                                 Helvetica
      REAL      LWIDTH, XWIDTH
      PARAMETER (LWIDTH = 155.0)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                        Start page:
      CBUFF = '%%Page 1 1'
      CALL ZLASIO ('WRIT', LUN, ' ', ITRIM(CBUFF), CBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      CBUFF = 'userdict /start-hook known {start-hook} if'
      CALL ZLASIO ('WRIT', LUN, ' ', ITRIM(CBUFF), CBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      CBUFF = 'userdict /bop-hook known {bop-hook} if'
      CALL ZLASIO ('WRIT', LUN, ' ', ITRIM(CBUFF), CBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Save state:
      CBUFF = '/pgsave save def                       % save state'
      CALL ZLASIO ('WRIT', LUN, ' ', ITRIM(CBUFF), CBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Rotation
      IF (ROTATE) THEN
         CBUFF = ' 90 rotate '
         CALL ZLASIO ('WRIT', LUN, ' ', ITRIM(CBUFF), CBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         WRITE (CBUFF,1004) 0, -BBOX(1,2)-BBOX(1,1)
         CALL ZLASIO ('WRIT', LUN, ' ', ITRIM(CBUFF), CBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       If outputting a label,
C                                       calculate the font size, and
C                                       draw the label in black
      IF (DOLAB) THEN
         I = ITRIM (REASON)
         XWIDTH = (I * LWIDTH) / 14
         FSIZE = INT (20.0 * (BBOX(1,2) - BBOX(1,1)) / XWIDTH)
         FSIZE = MAX (1, MIN (20, FSIZE))
         WRITE (CBUFF,1000) FSIZE
         CALL ZLASIO ('WRIT', LUN, ' ', ITRIM(CBUFF), CBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (ROTATE) THEN
            WRITE (CBUFF,1001) BBOX(2,1) + 2, BBOX(1,1) - 23
         ELSE
            WRITE (CBUFF,1001) BBOX(1,1) + 2, BBOX(2,1) - 23
            END IF
         CALL ZLASIO ('WRIT', LUN, ' ', ITRIM(CBUFF), CBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         WRITE (CBUFF,1002) REASON(1:I)
         CALL ZLASIO ('WRIT', LUN, ' ', ITRIM(CBUFF), CBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                        Define a string capable of
C                                        holding one line of the image
      IF (COLOUR.GT.2) THEN
         I = 4
      ELSE IF (COLOUR.GT.0) THEN
         I = 3
      ELSE
         I = 1
         END IF
      WRITE (CBUFF,1003) I * (TVWIND(3) - TVWIND(1) + 1)
      CALL ZLASIO ('WRIT', LUN, ' ', ITRIM(CBUFF), CBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Set up a transformation matrix
C                                        that maps the unit square to
C                                        the bounding box
      IF (ROTATE) THEN
         WRITE (CBUFF,1004) BBOX(2,1), BBOX(1,1)
      ELSE
         WRITE (CBUFF,1004) BBOX(1,1), BBOX(2,1)
         END IF
      CALL ZLASIO ('WRIT', LUN, ' ', ITRIM(CBUFF), CBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (ROTATE) THEN
         WRITE (CBUFF,1005) BBOX(2,2)-BBOX(2,1), BBOX(1,2)-BBOX(1,1)
      ELSE
         WRITE (CBUFF,1005) BBOX(1,2)-BBOX(1,1), BBOX(2,2)-BBOX(2,1)
         END IF
      CALL ZLASIO ('WRIT', LUN, ' ', ITRIM(CBUFF), CBUFF, IRET)
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('/Helvetica findfont ', I2, ' scalefont setfont')
 1001 FORMAT (I4, ' ', I4, ' moveto')
 1002 FORMAT ('(',A,') show')
 1003 FORMAT ('/picst ', I6, ' string def')
 1004 FORMAT (I8, I8, ' translate')
 1005 FORMAT (I8, I8, ' scale')
      END
      SUBROUTINE WRLINE (OPCODE, LUN, COLOUR, RED, GREEN, BLUE, WIDTH,
     *   DOINV, IRET)
C-----------------------------------------------------------------------
C   Write a line of image data to a PostScript file.  The data are
C   encoded in hexadecimal with 72 characters per output line.  If
C   COLOUR is false then only the red component is written.
C
C   Note that one pixel requires 2 output bytes for greyscale and 6
C   output bytes for colour so line breaks always occur on pixel
C   boundaries.
C   Inputs:
C      OPCODE    C*4           'WRIT' - write a line of data
C                              'FLSH' - flush buffer
C      LUN       I             LUN of open PostScript file
C      COLOUR    I             True if writing an RGB image
C      RED       I(*)          Red values (0-255)
C      GREEN     I(*)          Green values (0-255)
C      BLUE      I(*)          Blue values (0-255)
C      WIDTH     I             Number of pixels in row
C      DOINV     L             Invert all numbers
C   Output:
C      IRET      I             Status: 0 => OK
C                                      anything else =>  error
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   COLOUR, LUN, RED(*), BLUE(*), GREEN(*), WIDTH, IRET
      LOGICAL   DOINV
C                                        BUFF = output buffer
C                                        MARK = next space in buffer
      CHARACTER BUFF*72
      INTEGER   MARK, PMARK
      SAVE      BUFF, MARK
C
      INTEGER   PIX, LC
      REAL      TEMP, GAMMA
C
      INCLUDE 'INCS:DMSG.INC'
C
      DATA      MARK /1/
      DATA      GAMMA /12.0/
C-----------------------------------------------------------------------
C                                        Flush buffer
      IF (OPCODE.EQ.'FLSH') THEN
         CALL ZLASIO ('WRIT', LUN, ' ', MARK-1, BUFF, IRET)
         MARK = 1
         IF (IRET.NE.0) GO TO 999
      ELSE IF (OPCODE.EQ.'WRIT') THEN
         IF (COLOUR.GT.0) THEN
            IF (((DOINV) .AND. (COLOUR.LT.3)) .OR. ((.NOT.DOINV) .AND.
     *         (COLOUR.GT.2))) THEN
               DO 10 PIX = 1,WIDTH
                  RED(PIX) = 255 - RED(PIX)
                  GREEN(PIX) = 255 - GREEN(PIX)
                  BLUE(PIX) = 255 - BLUE(PIX)
 10               CONTINUE
               END IF
            DO 20 PIX = 1,WIDTH
C                                        Flush buffer
               IF (MARK.EQ.73) THEN
                  CALL ZLASIO ('WRIT', LUN, ' ', 72, BUFF, IRET)
                  MARK = 1
                  IF (IRET.NE.0) GO TO 999
                  END IF
               PMARK = MARK
               IF (COLOUR.GT.2) THEN
                  LC = MIN (RED(PIX), GREEN(PIX))
                  LC = MIN (LC, BLUE(PIX))
                  TEMP = MAX (0.0, MIN (1.0, LC/255.0))
                  LC = 255 * (TEMP ** GAMMA)
C                  RED(PIX) = MAX (0, RED(PIX)-LC)
C                  GREEN(PIX) = MAX (0, GREEN(PIX)-LC)
C                  BLUE(PIX) = MAX (0, BLUE(PIX)-LC)
                  END IF
               CALL ZHEX (RED(PIX), 2, BUFF(MARK:MARK+1))
               MARK = MARK + 2
               CALL ZHEX (GREEN(PIX), 2, BUFF(MARK:MARK+1))
               MARK = MARK + 2
               CALL ZHEX (BLUE(PIX), 2, BUFF(MARK:MARK+1))
               MARK = MARK + 2
               IF ((COLOUR.EQ.2) .AND.
     *            (BUFF(PMARK:PMARK+5).EQ.'000000'))
     *            BUFF(PMARK:PMARK+5) = 'FFFFFF'
               IF ((COLOUR.EQ.4) .AND.
     *            (BUFF(PMARK:PMARK+5).EQ.'FFFFFF'))
     *            BUFF(PMARK:PMARK+5) = '000000'
               IF (COLOUR.GT.2) THEN
                  CALL ZHEX (LC, 2, BUFF(MARK:MARK+1))
                  MARK = MARK + 2
                  IF (BUFF(PMARK:PMARK+5).EQ.'FFFFFF')
     *               BUFF(PMARK+6:PMARK+7) = 'FF'
                  IF (BUFF(PMARK:PMARK+5).EQ.'FEFEFE')
     *               BUFF(PMARK+6:PMARK+7) = 'FE'
                  END IF
 20            CONTINUE
         ELSE
            IF (DOINV) THEN
               DO 30 PIX = 1,WIDTH
                  RED(PIX) = 255 - RED(PIX)
 30               CONTINUE
               END IF
            DO 40 PIX = 1,WIDTH
C                                        Flush buffer
               IF (MARK.EQ.73) THEN
                  CALL ZLASIO ('WRIT', LUN, ' ', 72, BUFF, IRET)
                  MARK = 1
                  IF (IRET.NE.0) GO TO 999
                  END IF
               CALL ZHEX (RED(PIX), 2, BUFF(MARK:MARK+1))
               MARK = MARK + 2
 40            CONTINUE
            END IF
      ELSE
         IRET = 1
         MSGTXT = 'WRLINE: UNRECOGNIZED OPCODE ''' // OPCODE // ''''
         CALL MSGWRT (8)
         GO TO 999
         END IF
C
      IRET = 0
C
  999 RETURN
      END
