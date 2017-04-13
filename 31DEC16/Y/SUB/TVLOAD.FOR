      SUBROUTINE TVLOAD (LUN, IND, IPL, PXINC, IMAWIN, WIN, BUFSZ,
     *   MBUFF, IERR)
C-----------------------------------------------------------------------
C! load image to a TV memory from open MA file
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2002, 2014
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
C   Subroutine to load a map from an already opened map file to one
C   TV memory plane.  TVLOAD puts TV and map windows in the image
C   header and writes it in the image catalog.  It assumes that the
C   other parts of the image header are already filled in (and uses
C   them) and that the windows are all computed.
C   Inputs:
C      LUN      I      Logical unit # of map file
C      IND      I      FTAB pointer for map file
C      IPL      I      Channel to load
C      PXINC    I(2)   Increment in x,y between included pixels
C      IMAWIN   I(4)   TV corners: BLC x,y TRC x,y
C      WIN      I(4)   Map window: ""
C      BUFSZ    I      Buffer size in bytes
C   Outputs
C      MBUFF    R(*)   Buffer
C      IERR     I      Error code: 0 => ok
C                                  1 => input errors
C                                  2 => MINIT errors
C                                  3 => MDISK errors
C   Commons: /MAPHDR/ CATBLK  image header
C-----------------------------------------------------------------------
      INTEGER   LUN, IND, IPL, PXINC(2), IMAWIN(4), WIN(4), BUFSZ, IERR
      REAL      MBUFF(*)
C
      CHARACTER TRANFN*2
      INTEGER   JERR, ININD, DELBLK, I, IYTV, MPIX, NPIX, IM1, IM2,
     *   HORIZ, IX, PLINC(2), NXINT, NYINT, LPIX, IY
      REAL      X, Y
      LOGICAL   WASYNC
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DCNT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INTEGER   JBUFF(MAXIMG), TBUFF(MAXIMG)
      EQUIVALENCE (JBUFF, IBBUFF), (TBUFF, ILROW)
      DATA HORIZ /0/
C-----------------------------------------------------------------------
      IERR = 1
      IF ((IPL.LT.1) .OR. (IPL.GT.NGRAY)) GO TO 999
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CATBLK(IIDEP), DELBLK,
     *   JERR)
      IF (JERR.NE.0) GO TO 999
      WASYNC = ISYNCH.EQ.0
      DELBLK = DELBLK + 1
C                                       fix up img cat block
      CALL COPY (4, WIN, CATBLK(IIWIN))
      CALL COPY (4, IMAWIN, CATBLK(IICOR))
      CALL H2CHR (2, 1, CATH(IITRA), TRANFN)
C                                       write img cat block
      IERR = 2
      CALL YCWRIT (IPL, IMAWIN, CATBLK, IBUFF, JERR)
      IF (JERR.NE.0) GO TO 999
      IF (WASYNC) CALL YHOLD ('ONNN', IERR)
C                                       start map read
      PLINC(1) = MAX (1, PXINC(1))
      PLINC(2) = MAX (1, PXINC(2))
      NXINT = MAX (1, -PXINC(1))
      NYINT = MAX (1, -PXINC(2))
      NPIX = 1 + WIN(3) - WIN(1)
      MPIX = 1 + (IMAWIN(3) - IMAWIN(1)) / NXINT
      LPIX = 1 + IMAWIN(3) - IMAWIN(1)
      CALL MINIT ('READ', LUN, IND, CATBLK(KINAX), CATBLK(KINAX+1), WIN,
     *   MBUFF, BUFSZ, DELBLK, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1010) JERR
         GO TO 980
         END IF
C                                       set up TV writing
      IM1 = WIN(2)
      IM2 = WIN(4)
      IERR = 3
      IYTV = IMAWIN(2)-1
C                                       read 1st row
      CALL MDISK ('READ', LUN, IND, MBUFF, ININD, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1020) JERR
         GO TO 980
         END IF
      CALL ISCALE (TRANFN, MAXINT, CATR(IRRAN), NPIX, PLINC,
     *   MBUFF(ININD), TBUFF)
      CALL LINTER (MPIX, NXINT, TBUFF, IBUFF)
      DO 70 IY = IM1,IM2
C                                       End: write row only
         IF (IY.GE.IM2) THEN
            IYTV = IYTV + 1
            CALL YIMGIO ('WRIT', IPL, IMAWIN(1), IYTV, HORIZ, LPIX,
     *         IBUFF, JERR)
            IF (JERR.NE.0) THEN
               WRITE (MSGTXT,1025) JERR
               GO TO 980
            ELSE
               IERR = 0
               GO TO 990
               END IF
            END IF
         CALL MDISK ('READ', LUN, IND, MBUFF, ININD, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1020) JERR
            GO TO 980
            END IF
         IF (MOD(IY-IM1,PLINC(2)).EQ.0) THEN
            CALL ISCALE (TRANFN, MAXINT, CATR(IRRAN), NPIX, PLINC,
     *         MBUFF(ININD), TBUFF)
            CALL LINTER (MPIX, NXINT, TBUFF, JBUFF)
            IYTV = IYTV + 1
            CALL YIMGIO ('WRIT', IPL, IMAWIN(1), IYTV, HORIZ, LPIX,
     *         IBUFF, JERR)
            IF (JERR.NE.0) THEN
               WRITE (MSGTXT,1025) JERR
               GO TO 980
               END IF
C                                       interpolate rows
            IF (NYINT.GT.1) THEN
               X = 1.0 / NYINT
               DO 60 I = 2,NYINT
                  Y = (I-1) * X
                  DO 55 IX = 1,LPIX
                     TBUFF(IX) = IBUFF(IX) + Y * (JBUFF(IX) - IBUFF(IX))
     *                  + 0.4999
 55                  CONTINUE
                  IYTV = IYTV + 1
                  CALL YIMGIO ('WRIT', IPL, IMAWIN(1), IYTV, HORIZ,
     *               LPIX, TBUFF, JERR)
                  IF (JERR.NE.0) THEN
                     WRITE (MSGTXT,1025) JERR
                     GO TO 980
                     END IF
 60               CONTINUE
               END IF
            CALL COPY (LPIX, JBUFF, IBUFF)
            END IF
 70      CONTINUE
      IERR = 0
      GO TO 990
C                                       write error message
 980  CALL MSGWRT (7)
C
 990  IF (WASYNC) CALL YHOLD ('OFFF', JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('TVLOAD: MINIT DISK I/O ERROR =',I6)
 1020 FORMAT ('TVLOAD: MDISK DISK I/O ERROR =',I6)
 1025 FORMAT ('TVLOAD: MDISK TV I/O ERROR =',I6)
      END
