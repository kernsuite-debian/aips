      SUBROUTINE PRINTR (IPRT, IERR)
C-----------------------------------------------------------------------
C! Changes or shows printer selection
C# Print
C-----------------------------------------------------------------------
C;  Copyright (C) 1997
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
C   PRINTR will do several things.  If the argument IPRT is zero, it
C   merely prints the name (and number and type) of the current printer.
C   If it's too large, it prints a list of all printers similar to the
C   AIPS startup message.  Finally, if it corresponds to a valid
C   printer, the LPDEV environment variable is changed accordingly and
C   a warning message issued.
C
C   Input:
C      IPRT   I*4    Printer number; see above
C   Output:
C      IERR   I*4    Zero if all is well, 1 otherwise
C
C   PPM 19970326: First cut.
C-----------------------------------------------------------------------
      INTEGER    IPRT, IERR, IER2, LPLEN, XLNB, IKB, IVAL, ILP, ILP2,
     &           ICLP, LUN, FIND, TLEN, JTRIM, IKE
      LOGICAL    DUM
      DOUBLE PRECISION X
      CHARACTER  LPNAM*8, LPDEV*20, LPDESC*50, LPVAL*8,
     &           LINE*80, LPTYPE*7, LPOPTS*40
C
      INCLUDE   'INCS:DMSG.INC'
      INCLUDE   'INCS:DERR.INC'
C                                       --------------------------------
C                                       Get current printer number
      LPNAM = 'LPDEV'
      LPLEN = 5
      CALL ZTRLOG(LPLEN, LPNAM, LPLEN, LPVAL, XLNB, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 105) IERR
         CALL MSGWRT(7)
         GO TO 999
         END IF
C                                       and make sure we got it
      IF (XLNB.LE.0) THEN
         WRITE (MSGTXT, 110) LPNAM, XLNB
         CALL MSGWRT(7)
         GO TO 999
         END IF
C                                       Extract integer from it
      IKB = 1
      IVAL = 0
      ERRNUM = 0
      X = 0.D0
      CALL GETNUM (LPVAL, XLNB, IKB, X)
      IF (ERRNUM.EQ.27) THEN
         WRITE (MSGTXT, 115) LPNAM, LPVAL
         CALL MSGWRT(7)
         GO TO 999
         END IF
      ICLP = X
C                                       OK.  First, get max printer #
      LPNAM = 'LPDEVS'
      LPLEN = 8
      CALL ZTRLOG(LPLEN, LPNAM, LPLEN, LPVAL, XLNB, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 120) IERR
         IERR = 1
         GO TO 999
         END IF
C                                       Make sure that really worked...
      IF (XLNB.LE.0) THEN
         WRITE (MSGTXT, 110) LPNAM, XLNB
         CALL MSGWRT(7)
         GO TO 999
         END IF
      IKB = 1
C                                       Decode string into an integer
      ERRNUM = 0
      CALL GETNUM (LPVAL, 8, IKB, X)
      IF (ERRNUM.EQ.27) THEN
         WRITE (MSGTXT, 115) LPNAM, LPVAL
         CALL MSGWRT(7)
         GO TO 999
         END IF
      ILP = X
C                                       Out of range means show'em all
      IF (IPRT.GT.ILP) THEN
         IPRT = -1
         MSGTXT = 'The available printers are:'
         CALL MSGWRT(3)
         MSGTXT = '---------------------------'
         CALL MSGWRT(3)
      ELSE IF (IPRT.EQ.0) THEN
         MSGTXT = 'Your current printer is:'
         CALL MSGWRT(3)
         MSGTXT = '---------------------------'
         CALL MSGWRT(3)
         END IF
C                                       Now see what we've got to do.
C                                       Set the printer number?
      IF (IPRT.GT.0) THEN
C                                       Is it what the user asked for?
         IF (ICLP.EQ.IPRT) THEN
            WRITE (MSGTXT, 130) IPRT
            CALL MSGWRT(3)
            IERR = 0
            GO TO 999
            END IF
C                                       Set it, do rest in loop below.
         LPNAM = 'LPDEV'
         LPLEN = 5
         LPVAL = ' '
         WRITE (LPVAL, 133) IPRT
 133     FORMAT (I2)
         CALL ZCRLOG (LPLEN, LPNAM, 8, LPVAL, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 140) IERR
            CALL MSGWRT(7)
            IERR = 1
            GOTO 999
            END IF
         WRITE (MSGTXT, 142) ICLP, IPRT
         CALL MSGWRT(3)
         END IF
C                                       For all requests, we get info
C                                       from PRDEVS.LIST in NET0:
      LUN = 3
      CALL ZTXOPN('QRED', LUN, FIND, 'NET0:PRDEVS.LIST', DUM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 145) IERR
         CALL MSGWRT(7)
         IERR = 1
         GO TO 999
         END IF
C                                       Initialise line counter
      ILP2 = 0
 200  CONTINUE
         LINE = ' '
         CALL ZTXIO('READ', LUN, FIND, LINE, IERR)
C                                       End of file?
         IF (IERR.EQ.2) GO TO 299
C                                       Other problem?
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 205) 1 + ILP2
            CALL MSGWRT(7)
            GO TO 299
            END IF
C                                       Get rid of nulls...
         TLEN = JTRIM(LINE)
C                                       Skip white space and comments
         IF (LINE.EQ.' ') GO TO 200
         IF (LINE(1:1).EQ.'#') GO TO 200
C                                       Increment line counter
         ILP2 = ILP2 + 1
C                                       If printing current details and
C                                       this is not the right one, skip
         IF ((IPRT.EQ.0) .AND. (ILP2.LT.ICLP)) GO TO 200
C                                       If setting a new one and this is
C                                       not it, skip too
         IF ((IPRT.GT.0) .AND. (ILP2.LT.IPRT)) GO TO 200
C                                       Otherwise, print details
C                                       GETSTR expects quotes, so must
C                                       parse string the hard way here.
C                                       First, skip leading blanks
         IKB = 1
 210     IF (LINE(IKB:IKB).NE.' ') GO TO 215
            IKB = IKB + 1
            IF (IKB.GT.TLEN) GO TO 290
            GO TO 210
C                                       Should be printer name; find end
 215     IKE = IKB
 220     IKE = IKE + 1
            IF (LINE(IKE:IKE).EQ.' ') GO TO 225
            IF (IKE.GT.TLEN) GO TO 225
            GO TO 220
 225     IKE = IKE - 1
         LPDEV = LINE(IKB:IKE)
C                                       Now on to the printer type
         IKB = IKE + 1
C                                       Skip spaces
 230     IF (LINE(IKB:IKB).NE.' ') GO TO 235
            IKB = IKB + 1
            IF (IKB.GT.TLEN) GO TO 290
            GO TO 230
C                                       should be printer type; find end
 235     IKE = IKB
 240     IKE = IKE + 1
            IF (LINE(IKE:IKE).EQ.' ') GO TO 245
            IF (IKE.GT.TLEN) GO TO 245
            GO TO 240
 245     IKE = IKE - 1
         LPTYPE = LINE(IKB:IKE)
C                                       And the options...
         IKB = IKE + 1
C                                       Skip spaces
 250     IF (LINE(IKB:IKB).NE.' ') GO TO 255
            IKB = IKB + 1
            IF (IKB.GT.TLEN) GO TO 290
            GO TO 250
C                                       should be printer type; find end
 255     IKE = IKB
 260     IKE = IKE + 1
            IF (LINE(IKE:IKE).EQ.' ') GO TO 265
            IF (IKE.GT.TLEN) GO TO 265
            GO TO 260
 265     IKE = IKE - 1
         LPOPTS = LINE(IKB:IKE)
C                                       Everything else is a comment,
C                                       just skip past the spaces.
         IKB = IKE + 1
 270     IF (LINE(IKB:IKB).NE.' ') GO TO 275
            IKB = IKB + 1
            IF (IKB.GT.TLEN) GO TO 290
            GO TO 270
 275     LPDESC = LINE(IKB:TLEN)
         WRITE (MSGTXT, 277) ILP2, LPTYPE, LPDESC
         CALL MSGWRT(3)
C                                       All done if printing status
         IF (IPRT.EQ.0) GO TO 299
C                                       Continue to EOF if showing all
         IF (IPRT.LT.0) GO TO 200
C                                       If changing printer, need to set
C                                       these environment variables
         IER2 = 0
         IERR = 0
         LPNAM = 'LPNAM'
         LPLEN = 5
         TLEN = JTRIM(LPDEV)
         CALL ZCRLOG (LPLEN, LPNAM, TLEN, LPDEV, IERR)
         IF (IERR.NE.0) IER2 = IERR
         LPNAM = 'PRTYPE'
         LPLEN = 6
         TLEN = JTRIM(LPTYPE)
         CALL ZCRLOG (LPLEN, LPNAM, TLEN, LPTYPE, IERR)
         IF (IERR.NE.0) IER2 = IERR
         LPNAM = 'LPOPT'
         LPLEN = 5
         TLEN = JTRIM(LPOPTS)
         CALL ZCRLOG (LPLEN, LPNAM, TLEN, LPOPTS, IERR)
         IF (IERR.NE.0) IER2 = IERR
         LPNAM = 'LPDESC'
         LPLEN = 6
         TLEN = JTRIM(LPDESC)
         CALL ZCRLOG (LPLEN, LPNAM, TLEN, LPDESC, IERR)
         IF (IERR.NE.0) IER2 = IERR
         IF (IER2.NE.0) THEN
            WRITE (MSGTXT, 295) IER2
            CALL MSGWRT(7)
            IERR = 1
            END IF
         GO TO 299
C                                       Problems in parsing go here
 290     WRITE (MSGTXT, 297)
         CALL MSGWRT(7)
         IERR = 1
C                                       We're done now.
C                                       End of loop: close the file
 299     CONTINUE
      IER2 = 0
      CALL ZTXCLS(LUN, FIND, IER2)
      IF (IER2.NE.0) THEN
         WRITE (MSGTXT, 305) IER2
         CALL MSGWRT(7)
         END IF
C                                       Formats ------------------------
 105  FORMAT ('Cannot translate logical LPDEV?  Error ',I5)
 110  FORMAT ('Bad string length from logical ',A,': ',I5)
 115  FORMAT ('Unexpected non-numeric value in ',A,'?: ',A)
 120  FORMAT ('Cannot translate LPDEVS (max printer)?  Error ',I5)
 130  FORMAT ('Printer ',I2,' was already selected!')
 140  FORMAT ('Cannot change logical LPDEV, error code ',I5)
 142  FORMAT ('Default printer changed from ',I2,' to ',I2)
 145  FORMAT ('Cannot open PRDEVS.LIST file in NET0, error ',I5)
 205  FORMAT ('Error reading PRDEVS.LIST line ',I5,' code ',I5)
 277  FORMAT (1X,I5,'. [',A7,']   ',A)
 295  FORMAT ('Problem setting one of printer logicals, code ',I5)
 297  FORMAT ('Error in PRDEVS.LIST: long lines or missing items?')
 305  FORMAT ('Error ',I5,' closing PRDEVS.LIST, hope this is ok')

C                                       End of Formats -----------------
 999  RETURN
      END
