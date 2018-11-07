      SUBROUTINE AU6C (BRANCH)
C-----------------------------------------------------------------------
C! verb to alter zoom and enhance image in standard way: TVFIDDLE
C# TV-appl POPS-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2008-2009
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
C   AU6C does an interactive run with button A selecting alternately
C   TVTRANSF and TVPSEUDO (color contour type 2 only), button B
C   incrementing the zoom and C decrementing the zoom.
C   BRANCH =  2 :  TVFIDDLE            Does only interactive part
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      INCLUDE 'INCS:PTVC.INC'
      CHARACTER PRGNAM*6, CDUM*1
      INTEGER   POTERR, ICHAN, IERR, INBUF(4096), NLEVS, JERR, LCHAN,
     *   ZAND, ZOR, INCHAN, LUTBUF(TVMLUT)
      REAL      RDUM
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (LUTBUF, INBUF)
      COMMON /AIPSCR/ INBUF
      DATA PRGNAM /'AU6C '/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.2)) GO TO 999
      CALL TVOPEN (INBUF, JERR)
      POTERR = 101
      IF (JERR.NE.0) GO TO 980
C                                       check TV state
      IF ((TVLIMG(1).NE.TVLIMG(2)) .OR. (TVLIMG(1).NE.TVLIMG(3)) .OR.
     *   (TVLIMG(1).NE.TVLIMG(4))) THEN
         POTERR = 77
         GO TO 960
         END IF
C
      POTERR = 49
      CALL ADVERB ('TVCHAN', 'I', 1, 0, ICHAN, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 960
      IF ((ICHAN.LE.0) .OR. (ICHAN.GT.NGRAY)) ICHAN = 1
      INCHAN = ICHAN
      ICHAN = 2 ** (ICHAN-1)
      CALL ADVERB ('TVLEVS', 'I', 1, 0, NLEVS, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 960
      IF (NLEVS.LT.10) NLEVS = LUTOUT + 1
      IF (NLEVS.LE.LUTOUT) NLEVS = LUTOUT + 1
      IF (NLEVS.GT.OFMINP) NLEVS = OFMINP + 1
C
      GO TO (100, 200), BRANCH
C-----------------------------------------------------------------------
C                                       TVALL
C                                       clear, load, fiddle
C                                       replaced by procedure!
C-----------------------------------------------------------------------
 100  CONTINUE
C-----------------------------------------------------------------------
C                                       TVFIDDLE
C                                       Interactive section
C-----------------------------------------------------------------------
C                                       set channel number
 200  LCHAN = ZOR (TVLIMG(1), TVLIMG(2))
      LCHAN = ZOR (LCHAN, TVLIMG(3))
      LCHAN = ZOR (LCHAN, TVLIMG(4))
      LCHAN = ZAND (LCHAN, ICHAN)
      IF (LCHAN.GT.0) GO TO 210
         IERR = 1
         POTERR = 101
         WRITE (MSGTXT,1200) INCHAN
         CALL MSGWRT (7)
         GO TO 960
C                                       Do it
 210  CALL TVFIDL (LCHAN, NLEVS, INBUF, IERR)
      GO TO 960
C                                       Close down
 960  CALL TVCLOS (INBUF, JERR)
      IF (IERR.EQ.0) GO TO 999
C                                       AIPS error handling
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.GT.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('REQUESTED CHANNEL',I7,' NOT ON')
      END
