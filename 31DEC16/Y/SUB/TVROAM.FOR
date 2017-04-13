      SUBROUTINE TVROAM (TYPE, CHANS, BUFF, IERR)
C-----------------------------------------------------------------------
C! does interactive multi-channel "ROAM" display on pre-loaded images
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2008, 2011, 2015
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
C   TVROAM performs a "roam" operation on a TV: shows 2 horizontal,
C   2 vertical, or a 2 X 2 set of planes.  User may vary zoom with
C   buttons and portion of image actually visible with cursor.
C   Inputs:
C      TYPE   I       100 * (#planes in X) + (#planes in Y)
C      CHANS  I(*)    list of channel numbers
C   Outputs:
C      BUFF   I(*)    Scratch buffer (used?)
C      IERR   I       Error code of ZM70XF: 0 - ok;  2 -> input error
C   Commons:
C      this routine updates the TV status parms having to do with
C      scroll, zoom, split, and which images are on.
C-----------------------------------------------------------------------
      INTEGER   TYPE, CHANS(*), BUFF(*), IERR
C
      INTEGER   IMAG, IXZ, IYZ, IT, CHCOD, ITW(3), IBUT, NFR(2), I, IDL,
     *   NPIX(2), FMAG, IWIN(4), ISCR(2), IER, XCHAN(16), QUAD, IDS, J,
     *   J1, J2, NPLX, NPLY, LMAGL, IB, IC
      LOGICAL   T, F, DOIT
      REAL      PPOS(2), RPOS(2), XMUL(2), XPOS(2)
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      QUAD = -1
      IERR = 2
      NPLX = TYPE / 100
      NPLY = MOD (TYPE, 100)
      IF (NPLX*NPLY.GT.NGRAY) GO TO 999
      IF ((NPLX.LE.0) .OR. (NPLY.LE.0)) GO TO 999
C                                       Off zoom
      CALL YHOLD ('ONNN', IERR)
      IMAG = 0
      IXZ = MAXXTV(1)/2 + 1
      IYZ = MAXXTV(2)/2
      CALL YZOOMC (IMAG, IXZ, IYZ, F, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Turn off graphics
      J1 = NGRAY + 1
      J2 = NGRAY + NGRAPH
      DO 10 I = J1,J2
         CALL YSLECT ('OFFF', I, 0, BUFF, IERR)
         IF (IERR.NE.0) GO TO 999
 10      CONTINUE
      CALL YHOLD ('OFFF', IERR)
C                                       set up chan masks
      IT = NPLX * NPLY
      CALL FILL (16, 0, XCHAN)
      CHCOD = 0
      DO 15 I = 1,IT
         XCHAN(I) = 2 ** (CHANS(I)-1)
         CHCOD = CHCOD + XCHAN(I)
 15      CONTINUE
C                                       Instructions to user
      WRITE (MSGTXT,1020)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1021)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1022)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1023)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1024)
      CALL MSGWRT (1)
C                                       Basic parms
      NFR(1) = NPLX
      NFR(2) = NPLY
      NPIX(1) = NFR(1) * MAXXTV(1)
      NPIX(2) = NFR(2) * MAXXTV(2)
      XMUL(1) = (MAXXTV(1)+1.0) / MAXXTV(1)
      XMUL(2) = (MAXXTV(2)+1.0) / MAXXTV(2)
C                                       Turn on cursor
      CALL ZTIME (ITW)
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      RPOS(1) = MAXXTV(1)/2.0
      RPOS(2) = MAXXTV(2)/2.0
      CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
      LMAGL = ABS(MXZOOM) + 1
C                                       Read loop
 50   CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IERR)
      IF ((IERR.NE.0) .OR. (IBUT.GT.7)) GO TO 990
         CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
         IF (.NOT.DOIT) GO TO 50
C                                       do changes
C                                       enhance
         IF ((IBUT.GT.0) .AND. (IBUT.LT.2)) THEN
            MSGTXT = 'Hit button C to reverse slope'
            CALL MSGWRT (1)
            MSGTXT = 'Hit button A, B, or D to return to ROAM'
            CALL MSGWRT (1)
            I = NPLX * NPLY
            I = (2 ** I) - 1
            IC = 7
            IB = 2
            CALL IENHNS (I, IC, IB, XPOS, BUFF, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
            IF (IERR.NE.0)  GO TO 990
            GO TO 50
            END IF
C                                       Increment magn. on button
         IF ((IBUT.GT.1) .AND. (IBUT.LT.4)) IMAG = IMAG + 1
         IF (IBUT.GE.4) IMAG = IMAG - 1
         IF (IMAG.LT.0) IMAG = LMAGL - 1
         IMAG = MOD (IMAG, LMAGL)
         FMAG = IMAG + 1
         IF (MXZOOM.GT.0) FMAG = 2 ** IMAG
C                                       Find corners
         DO 70 I = 1,2
            IWIN(I) = (NFR(I)-1.0/FMAG) * (RPOS(I)-1) * XMUL(I) + 1
            IWIN(I+2) = IWIN(I) + MAXXTV(I)/FMAG - 1
            IF (IWIN(I+2).LE.NPIX(I)) GO TO 60
               IWIN(I+2) = NPIX(I)
               IWIN(I) = IWIN(I+2) - MAXXTV(I)/FMAG + 1
 60         J = SCXINC
            IF (I.EQ.2) J = SCYINC
            IWIN(I) = ((IWIN(I)-1)/J) * J + 1
            IWIN(I+2) = IWIN(I) + MAXXTV(I)/FMAG - 1
C                                       Compute scroll, split
            J = MOD (IWIN(I)-1, MAXXTV(I))
            IF (I.EQ.1) ISCR(I) = (MAXXTV(I)/2) * (FMAG-1.0)/FMAG + 0.5
            IF (I.EQ.2) ISCR(I) = (MAXXTV(I)/2) * (FMAG+1.0)/FMAG + 0.5
            IF (I.EQ.2) ISCR(I) = MAXXTV(I) - ISCR(I)
            ISCR(I) = ISCR(I) - J
            TVSPLT(I) = FMAG * (MAXXTV(I) - MOD (IWIN(I)-1, MAXXTV(I)))
            IF (I.EQ.1) TVSPLT(I) = TVSPLT(I) + 1
            TVSPLT(I) = MAX (1, MIN (TVSPLT(I), MAXXTV(I)))
 70         CONTINUE
C                                       Which planes are on?
         IDL = (NPIX(2) - IWIN(4))/MAXXTV(2)
         IDS = (NPIX(1) - IWIN(3))/MAXXTV(1)
         TVLIMG(1) = XCHAN(NFR(1)*IDL+IDS+1)
         IDS = (NPIX(1) - IWIN(1))/MAXXTV(1)
         TVLIMG(2) = XCHAN(NFR(1)*IDL+IDS+1)
         IDL = (NPIX(2) - IWIN(2))/MAXXTV(2)
         TVLIMG(3) = XCHAN(NFR(1)*IDL+IDS+1)
         IDS = (NPIX(1) - IWIN(3))/MAXXTV(1)
         TVLIMG(4) = XCHAN(NFR(1)*IDL+IDS+1)
C                                       Send scrol, split, zoom to TV
         CALL YHOLD ('ONNN', IERR)
         CALL YSCROL (CHCOD, ISCR(1), ISCR(2), T, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL YSPLIT ('WRIT', TVSPLT(1), TVSPLT(2), TVLIMG, TVLIMG,
     *      TVLIMG, F, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (IBUT.GT.0) CALL YZOOMC (IMAG, IXZ, IYZ, F, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL YHOLD ('OFFF', IERR)
         GO TO 50
C                                       Turn off cursor
 990  CALL YCURSE ('OFFF', F, F, RPOS, QUAD, IBUT, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Cursor controls center of roam')
 1021 FORMAT ('Hit button A to change transfer function')
 1022 FORMAT ('Hit button B to increment magnification')
 1023 FORMAT ('Hit button C to decrement magnification')
 1024 FORMAT ('Hit button D to exit')
      END
