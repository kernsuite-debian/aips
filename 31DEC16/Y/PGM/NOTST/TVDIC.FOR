LOCAL INCLUDE 'TVDIC.INC'
C                                                        Include TVDIC.
C                                       Local include for TVDIC.
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'INCS:PTVC.INC'
C                                       Input parameters (see header
C                                       comments).
      INTEGER   FLMSIZ, FLMTYP, COPIES
      CHARACTER OUTPRT*48,COMMNT*48
C                                       Dimensions of work arrays.
C                                        MAXROW: maximum row size in
C                                                pixels
C                                        MAXPLN: maximum number of
C                                                grey-scale planes
C                                        MXGPLN: maximum number of
C                                                graphics planes
C                                        MAXLUT: maximum number of
C                                                entries in an LUT
C                                        MAXOFM: maximum number of
C                                                entries in an OFM
      INTEGER MAXROW, MAXPLN, MXGPLN, MAXLUT, MAXOFM
      PARAMETER (MAXROW = (MAXIMG))
      PARAMETER (MAXPLN = 4)
      PARAMETER (MXGPLN = 4)
      PARAMETER (MAXLUT = (TVMLUT)+1)
      PARAMETER (MAXOFM = (TVMOFM)+1)
C                                       LUT for image file.
      INTEGER DICLUN
      PARAMETER (DICLUN = 10)
C
      INTEGER   LUTDAT(MAXLUT, 3, MAXPLN), WINDOW(4)
      INTEGER   RBITS(MAXPLN), GBITS(MAXPLN), BBITS(MAXPLN)
      REAL      OFMDAT(MAXOFM, 3)
      INTEGER   GPMASK(MXGPLN)
C
      COMMON /PARM/   FLMSIZ, FLMTYP, COPIES
      COMMON /CHPARM/ OUTPRT, COMMNT
      COMMON /TVDATA/ LUTDAT, OFMDAT, WINDOW, RBITS, GBITS, BBITS,
     *                GPMASK
C                                                           End TVDIC.
LOCAL END
      PROGRAM TVDIC
C-----------------------------------------------------------------------
C! Copies a TV image to a Dicomed file recorder.
C# TV Film-Recorder
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002, 2008
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
C   TVDIC reads the current contents of an AIPS TV and writes them
C   to an NRAO-format Dicomed image file.
C
C   Adverbs:
C      Adverb name   Prgm. name   Description
C      OUTPRINT      OUTPRT       Name of output file
C      GRCHAN        GRCHAN       Graphics channels to read.
C      APARM(1)      FLMSIZ       Film size (enumerated)
C                                  0 -> 35mm
C                                  1 -> 2"x2"
C                                  2 -> 4"x5"
C      APARM(2)      FLMTYP       Film type (enumerated)
C                                  0 -> colour positive
C                                  1 -> colour negative
C                                  2 -> B/W negative
C      APARM(3)      COPIES       Number of copies (default: 1)
C-----------------------------------------------------------------------
      INTEGER   IRET, WBUFF(256)
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'TVDIC.INC'
C-----------------------------------------------------------------------
C                                       Initialize AIPS commons and
C                                       read and check input adverbs:
      CALL TVDCIN (IRET)
C
      IF (IRET.EQ.0) THEN
C                                       Open output file and TV device:
         CALL DCINIT (IRET)
         END IF
      IF (IRET.EQ.0) THEN
C                                       Copy screen image to output
C                                       file:
         CALL CPSCRN (IRET)
         END IF
      IF (IRET.EQ.0) THEN
C                                       Close output file and TV
C                                       device:
         CALL TVDFIN (IRET)
         END IF
C                                       Terminate task:
      CALL DIE (IRET, WBUFF)
      STOP
      END
      SUBROUTINE TVDCIN (IRET)
C-----------------------------------------------------------------------
C   Initialize AIPS commons and read and check input adverbs.  Restart
C   AIPS if the DOWAIT adverb is true.
C
C   Output parameters:
C      IRET     I      Return status.
C                       0 -> success
C                       1 -> failed to read adverbs
C                       2 -> failed to restart AIPS
C                       3 -> bad input adverb
C
C   Output in /PARM/ (TVDIC.INC):
C      FLMSIZ   I      Film size
C                       0 -> 35mm
C                       1 -> 2"x2"
C                       2 -> 4"x5"
C      FLMTYP   I      Film type
C                      0 -> Colour positive
C                      1 -> colour negative
C                      2 -> B/W negative
C      COPIES   I     Number of copies to make (COPIES > 0)
C
C   Output in /CHPARM/ (TVDIC.INC)
C      OUTPRT   C*48  Name of the image file or blank (if blank
C                      image file will not be saved).
C      COMMNT   C*48  User comment for image.
C
C   Output in /TVDATA/ (TVDIC.INC)
C      GPMASK   I(MXGPLN)   GPMASK(I) is true if graphics plane (I)
C                           should be in the output image.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   SCRTCH(256), NPARM, JRET
      CHARACTER PRGM*6
      REAL      APARM(10), XCOPIS
      HOLLERITH XOUTPR(12), XCMMNT(12)
C
      PARAMETER (NPARM = 12 + 1 + 10 + 1 + 12)
      PARAMETER (PRGM = 'TVDIC')
C
      COMMON /ADVERB/ XOUTPR, APARM, XCOPIS, XCMMNT
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'TVDIC.INC'
C-----------------------------------------------------------------------
C                                       Initialize device
C                                       characteristics common:
      CALL ZDCHIN (.TRUE., SCRTCH)
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
         WRITE (MSGTXT, 1000) JRET
         CALL MSGWRT (8)
         END IF
C
      IF (RQUICK) THEN
C                                       Restart AIPS:
         CALL RELPOP (IRET, SCRTCH, JRET)
         IF (JRET.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT, 1001) JRET
            CALL MSGWRT (8)
            END IF
         END IF
C                                       Unpack input parameters:
      IF (IRET.EQ.0) THEN
         CALL H2CHR (48, 1, XOUTPR, OUTPRT)
         FLMSIZ = NINT (APARM(1))
         IF ((FLMSIZ.LT.0) .OR. (FLMSIZ.GT.2)) THEN
            IRET = 3
            WRITE (MSGTXT, 1002) FLMSIZ
            CALL MSGWRT(8)
            END IF
         FLMTYP = NINT (APARM(2))
         IF ((FLMTYP.LT.0) .OR. (FLMTYP.GT.2)) THEN
            IRET = 3
            WRITE (MSGTXT, 1003) FLMTYP
            CALL MSGWRT (8)
            END IF
         COPIES = NINT (XCOPIS)
         IF (COPIES.LT.1) THEN
            COPIES = 1
         ELSE IF (COPIES.GT.999) THEN
            IRET = 3
            WRITE (MSGTXT, 1004) 999
            CALL MSGWRT (8)
            END IF
         CALL H2CHR (48, 1, XCMMNT, COMMNT)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FAILED TO READ ADVERBS: GTPARM ERROR ', I7)
 1001 FORMAT ('FAILED TO RESTART AIPS: RELPOP ERROR ', I2)
 1002 FORMAT ('INVALID FILM SIZE (APARM(1) = ', I5, ')')
 1003 FORMAT ('INVALID FILM TYPE (APARM(2) = ', I5, ')')
 1004 FORMAT ('TOO MANY COPIES REQUESTED (<', I4, ')')
      END
      SUBROUTINE DCINIT (IRET)
C-----------------------------------------------------------------------
C   Open the TV, read the TV parameters, open the image file and
C   write the image file header.
C
C   Output:
C      IRET      I      Return status
C                          0 -> success
C                          1 -> could not open TV
C                          2 -> internal array too small
C                          3 -> could not read current window
C                          4 -> could not read LUTs
C                          5 -> could not read OFMs
C                          6 -> could not read active planes
C                          6 -> could not open Dicomed file
C
C   Outputs in /TVDATA/ (TVDIC.INC)
C      WINDOW    I(4)   Current BLC (elements 1 and 2) and TRC
C                       (elements 3 and 4) of current TV viewport
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
      INTEGER   JRET, IPLANE, ICHAN, BPPIX, XSPLT, YSPLT, RCHANS(4),
     *   GCHANS(4), BCHANS(4), I
      INCLUDE   'INCS:DDCH.INC'
      INCLUDE   'INCS:DMSG.INC'
      INCLUDE   'INCS:DTVC.INC'
      INCLUDE   'TVDIC.INC'
C-----------------------------------------------------------------------
C                                       Assume success:
      IRET = 0
C                                       Open the TV device:
      CALL TVOPEN (SCRTCH, JRET)
      IF (JRET.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT, 1000) JRET
         CALL MSGWRT (8)
         END IF
C
      IF (IRET.EQ.0) THEN
C                                       Check that the internal arrays
C                                       are large enough for this
C                                       TV device:
         IF (NGRAY.GT.MAXPLN) THEN
            IRET = 2
            WRITE (MSGTXT, 1001) MAXPLN
            CALL MSGWRT (8)
            END IF
         IF (NGRAPH.GT.MXGPLN) THEN
            IRET = 2
            WRITE (MSGTXT, 1002) MXGPLN
            CALL MSGWRT (8)
            END IF
         IF ((MAXINT+1).GT.MAXLUT) THEN
            IRET = 2
            WRITE (MSGTXT, 1003) MAXLUT
            CALL MSGWRT (8)
            END IF
         IF ((OFMINP+1).GT.MAXOFM) THEN
            IRET = 2
            WRITE (MSGTXT, 1004) MAXOFM
            CALL MSGWRT (8)
            END IF
         END IF
C
      IF (IRET.EQ.0) THEN
C                                       Read current TV window:
         CALL YWINDO ('READ', WINDOW, JRET)
         IF (JRET.NE.0) THEN
            IRET = 3
            WRITE (MSGTXT, 1005) JRET
            CALL MSGWRT (8)
            END IF
         END IF
C
      IF (IRET.EQ.0) THEN
C                                       Check that the line buffer
C                                       is large enough to hold one
C                                       row from the current window:
         IF ((WINDOW(3)-WINDOW(1)+1).GT.MAXROW) THEN
            IRET = 2
            WRITE (MSGTXT, 1006) MAXROW
            CALL MSGWRT (8)
            END IF
         END IF
C
      IF (IRET.EQ.0) THEN
C                                       Read look-up table for each
C                                       grey-scale plane:
         DO 10 IPLANE = 1, NGRAY
            ICHAN = 2 ** (IPLANE-1)
            CALL YLUT ('READ', ICHAN, 4, .TRUE., LUTDAT(1, 1, IPLANE),
     *                 JRET)
            IF (JRET.NE.0) THEN
               IRET = 4
               WRITE (MSGTXT, 1007) JRET
               CALL MSGWRT (8)
               GO TO 999
               END IF
            CALL YLUT ('READ', ICHAN, 2, .TRUE., LUTDAT(1, 2, IPLANE),
     *                 JRET)
            IF (JRET.NE.0) THEN
               IRET = 4
               WRITE (MSGTXT, 1007) JRET
               CALL MSGWRT (8)
               GO TO 999
               END IF
            CALL YLUT ('READ', ICHAN, 1, .TRUE., LUTDAT(1, 3, IPLANE),
     *                 JRET)
            IF (JRET.NE.0) THEN
               IRET = 4
               WRITE (MSGTXT, 1007) JRET
               CALL MSGWRT (8)
               GO TO 999
               END IF
 10         CONTINUE
         END IF
C
      IF (IRET.EQ.0) THEN
C                                       Read OFMs:
         CALL YOFM ('READ', 4, .TRUE., OFMDAT(1, 1), JRET)
         IF (JRET.NE.0) THEN
            IRET = 5
            WRITE (MSGTXT, 1010) JRET
            CALL MSGWRT (8)
            GO TO 999
            END IF
         CALL YOFM ('READ', 2, .TRUE., OFMDAT(1, 2), JRET)
         IF (JRET.NE.0) THEN
            IRET = 5
            WRITE (MSGTXT, 1010) JRET
            CALL MSGWRT (8)
            GO TO 999
            END IF
         CALL YOFM ('READ', 1, .TRUE., OFMDAT(1, 3), JRET)
         IF (JRET.NE.0) THEN
            IRET = 5
            WRITE (MSGTXT, 1010) JRET
            CALL MSGWRT (8)
            GO TO 999
            END IF
         END IF
C
      IF (IRET.EQ.0) THEN
C                                       Find out which grey-scale
C                                       planes are enabled.
         CALL YSPLIT ('READ', XSPLT, YSPLT, RCHANS, GCHANS, BCHANS,
     *                .TRUE., JRET)
         IF (JRET.NE.0) THEN
            IRET = 6
            WRITE (MSGTXT, 1011) JRET
            CALL MSGWRT (8)
         ELSE
            CALL ZGTBIT(NGRAY, RCHANS(1), RBITS)
            CALL ZGTBIT(NGRAY, GCHANS(1), GBITS)
            CALL ZGTBIT(NGRAY, BCHANS(1), BBITS)
            END IF
         END IF
C                                       which graphics planes
      I = TVLIMG(1) / (2 ** NGRAY)
      CALL ZGTBIT (NGRAPH, I, GPMASK)
C
      IF (IRET.EQ.0) THEN
C                                       Open image file:
         IF (FLMTYP.EQ.2) THEN
            BPPIX = 8
         ELSE
            BPPIX = 24
            END IF
         CALL ZDICOP (DICLUN, SYSNAM, NLUSER, FLMSIZ, FLMTYP,
     *                COPIES, BPPIX, (WINDOW(3)-WINDOW(1)+1),
     *                (WINDOW(4)-WINDOW(2)+1), COMMNT, JRET)
         IF (JRET.NE.0) THEN
            IRET = 7
            WRITE (MSGTXT, 1012) JRET
            CALL MSGWRT (8)
            END IF
         END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CAN NOT OPEN TV: TVOPEN ERROR ', I3)
 1001 FORMAT ('TOO MANY GREY-SCALE PLANES FOR TVDIC (> ', I2, ')')
 1002 FORMAT ('TOO MANY GRAPHICS PLANES FOR TVDIC (> ', I2, ')')
 1003 FORMAT ('LUTS TOO LARGE FOR TVDIC (> ', I4, ' ENTRIES)')
 1004 FORMAT ('OFMS TOO LARGE FOR TVDIC (> ', I4, ' ENTRIES)')
 1005 FORMAT ('COULD NOT READ WINDOW: YWINDO ERROR ', I2)
 1006 FORMAT ('TV WINDOW TOO WIDE (> ', I5, ' PIXELS)')
 1007 FORMAT ('CAN NOT READ LUT: YLUT ERROR ', I2)
 1010 FORMAT ('CAN NOT READ OFM: YOFM ERROR ', I2)
 1011 FORMAT ('CAN NOT READ PLANE MASK: YSPLIT ERROR ', I2)
 1012 FORMAT ('CAN NOT OPEN OUTPUT FILE: ZDICOP ERROR ', I2)
      END
      SUBROUTINE CPSCRN (IRET)
C-----------------------------------------------------------------------
C   Copy the image on the screen to the output file.
C
C   Output:
C      IRET      I           Return status
C                              0 -> success
C                              1 -> can not read TV
C                              2 -> error writing file
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   JRET
      INTEGER   I, J, IROW, WIDTH
      REAL      RED, GREEN, BLUE
C                                       Scaling factor from OFM output
C                                       to film recorder input.
      REAL      SCALE
      PARAMETER (SCALE = 255.0)
C
      INCLUDE   'INCS:DDCH.INC'
      INCLUDE   'INCS:DMSG.INC'
      INCLUDE   'INCS:DTVC.INC'
      INCLUDE   'TVDIC.INC'
C
      INTEGER   ROUT(MAXROW), GOUT(MAXROW), BOUT(MAXROW)
      INTEGER   ROW(MAXROW)
      INTEGER   GCOLOR(3, MXGPLN)
C-----------------------------------------------------------------------
      IRET = 0
C                                       Calculate window width:
      WIDTH = WINDOW(3) - WINDOW(1) + 1
C                                       Set graphics plane colours:
      DO 10 I = 1, NGRAPH
         CALL YGRAFX ('READ', I, RED, GREEN, BLUE, JRET)
         IF (JRET.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT, 1000) JRET
            CALL MSGWRT (8)
            GO TO 999
            END IF
         GCOLOR(1, I) = SCALE * RED
         GCOLOR(2, I) = SCALE * GREEN
         GCOLOR(3, I) = SCALE * BLUE
C                                       Adjust to bright white or black
C                                       if producing a greyscale image.
         IF ((FLMTYP.EQ.2) .AND. ((RED.GT.0.5).OR.(GREEN.GT.0.5).OR.
     *                            (BLUE.GT.0.5))) THEN
            GCOLOR(1, I) = SCALE
            GCOLOR(2, I) = SCALE
            GCOLOR(3, I) = SCALE
            END IF
   10    CONTINUE
C                                       Read each row in the window:
      DO 100 IROW = WINDOW(4), WINDOW(2), -1
C                                       Rows are reversed for efficiency
C                                       in writing Dicomed file.
C
C                                       Clear accumulators:
         DO 20 I = 1, WIDTH
            ROUT(I) = 0
            GOUT(I) = 0
            BOUT(I) = 0
   20       CONTINUE
C                                       Read grey-scale planes:
C                                       (Inner loop should vectorize.)
C                                       (Outer loop may be
C                                       parallelized.)
         DO 40 I = 1, NGRAY
            IF ((RBITS(I).NE.0) .OR. (GBITS(I).NE.0)
     *          .OR. (BBITS(I).NE.0)) THEN
               CALL YIMGIO('READ', I, WINDOW(1), IROW, 0, WIDTH, ROW,
     *                    JRET)
               IF (JRET.NE.0) THEN
                  IRET = 1
                  WRITE (MSGTXT, 1020) JRET
                  CALL MSGWRT (8)
                  GO TO 999
                  END IF
               DO 30 J = 1, WIDTH
                  ROUT(J) = ROUT(J) + RBITS(I) * LUTDAT(ROW(J), 1, I)
                  GOUT(J) = GOUT(J) + GBITS(I) * LUTDAT(ROW(J), 2, I)
                  BOUT(J) = BOUT(J) + BBITS(I) * LUTDAT(ROW(J), 3, I)
   30             CONTINUE
               END IF
   40       CONTINUE
C                                       Apply OFM (vectorizable):
         DO 50 I = 1, WIDTH
            ROUT(I) = NINT (SCALE * OFMDAT(ROUT(I), 1))
            GOUT(I) = NINT (SCALE * OFMDAT(GOUT(I), 2))
            BOUT(I) = NINT (SCALE * OFMDAT(BOUT(I), 3))
   50       CONTINUE
C                                       Add in requested graphics
C                                       overlays:
C                                       Planes are read in reverse order
C                                       since plane i should overlay
C                                       plane i+1.
C                                       (Inner loop should vectorize)
C                                       (Outer loop can NOT be made
C                                       parallel)
         DO 70 I = NGRAPH, 1, -1
            IF (GPMASK(I).GT.0) THEN
               CALL YIMGIO('READ', I + NGRAY, WINDOW(1), IROW, 0, WIDTH,
     *                     ROW, JRET)
               IF (JRET.NE.0) THEN
                  IRET = 1
                  WRITE (MSGTXT, 1020) JRET
                  CALL MSGWRT (8)
                  GO TO 999
                  END IF
               DO 60 J = 1, WIDTH
                  IF (ROW(J).NE.0) THEN
                     ROUT(J) = GCOLOR(1, I)
                     GOUT(J) = GCOLOR(2, I)
                     BOUT(J) = GCOLOR(3, I)
                     END IF
   60             CONTINUE
               END IF
   70       CONTINUE
C                                       Write out the finished row:
            CALL ZDICIO(DICLUN, FLMTYP, IROW-WINDOW(2)+1, ROUT, GOUT,
     *                  BOUT, WIDTH, JRET)
         IF (JRET.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT, 1070) JRET
            CALL MSGWRT (8)
            GO TO 999
            END IF
C
  100    CONTINUE
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT('CAN NOT READ TV: YGRAFX ERROR ', I4)
 1020 FORMAT('CAN NOT READ TV: YIMGIO ERROR ', I2)
 1070 FORMAT('CAN NOT WRITE OUTPUT: ZDICIO ERROR ', I2)
      END
      SUBROUTINE TVDFIN (IRET)
C-----------------------------------------------------------------------
C   Close Dicomed output file and TV device and spool the Dicomed file
C   if necessary.
C
C   Output:
C      IRET        I         Return status
C                              0 -> success
C                              1 -> could not close TV
C                              2 -> could not close output file
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   JRET, SCRTCH(256)
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'TVDIC.INC'
C-----------------------------------------------------------------------
C                                       Close TV:
      CALL TVCLOS (SCRTCH, JRET)
      IF (JRET.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT, 1000) JRET
         CALL MSGWRT (8)
         END IF
C                                       If the program ran successfully
C                                       then close Dicomed file and
C                                       spool it to the film recorder if
C                                       that was requested.
      IF (IRET.EQ.0) THEN
         CALL ZDICCL (DICLUN, OUTPRT, JRET)
         IF (JRET.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT, 1001) JRET
            CALL MSGWRT (8)
            END IF
         END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CAN NOT CLOSE TV: TVCLOS ERROR ', I3)
 1001 FORMAT ('CAN NOT CLOSE OUTPUT FILE: ZDICCL ERROR ', I2)
      END
