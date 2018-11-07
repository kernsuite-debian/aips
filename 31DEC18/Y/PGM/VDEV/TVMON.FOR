      PROGRAM TVMON
C-----------------------------------------------------------------------
C! drives true TV device from a distant computer which uses VTV routines
C# TV-IO TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2008
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
C   TVMON is a program to run stand-alone in the CPU which has a TV
C   connected directly to it.  TVMON receives TV commands over a
C   communication line (e.g. via a mailbox driven from some other
C   process perhaps on some other computer), parses them, calls the Y
C   routines appropriate to the local TV device, and returns over the
C   communication lines the requested parameters and an error code.
C   It chooses a local "TV number" by the process name under which it
C   finds itself running, i.e., if the process name is TVMON3 then it
C   calls for AIPS TV number 3.
C-----------------------------------------------------------------------
      INTEGER   IRET, LUN, FIND
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DVTV.INC'
      DATA LUN /16/
C-----------------------------------------------------------------------
C                                       Create communication channel
      BUFSW = 0
      BUFSR = 0
      CALL TVMINI (LUN, FIND, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Do the TV driving
      CALL TVMONI (LUN, FIND, IRET)
C                                       quit
 990  CALL TVMEXI (LUN, FIND, IRET)
C
 999  STOP
      END
      SUBROUTINE TVMINI (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C   TVMINI starts up TVMON setting up the commons and the communication
C   channel (e.g. creating and opening the mailbox).
C   Inputs:
C      LUN    I      LUN to use for socket to remote machine
C   Output:
C      FIND   I      FTAB location reserved
C      IERR   I      Error code: 0 => ok, keep going
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      CHARACTER PGM*6
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DVTV.INC'
      DATA PGM /'TVMON '/
C-----------------------------------------------------------------------
C                                       AIPS init
      CALL ZDCHIN (.TRUE.)
      TSKNAM = PGM
      CALL ACOUNT (1)
      CALL VHDRIN
C                                       message
      WRITE (MSGTXT,1000)
      CALL MSGWRT (3)
      BUFSW = 0
      BUFSR = 0
C                                       call local, machine-dependent
C                                       routine: setup TV #, mailbox
      CALL ZVTVRO (LUN, FIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL YTVCIN
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Begins')
      END
      SUBROUTINE TVMEXI (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C   TVMEXI is a simple close down routine for TVMON.
C   Inputs:
C      LUN    I      LUN in use for socket to remote machine
C      FIND   I      FTAB location reserved
C      IERR   I      Error code from other main subroutines
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      INTEGER   NLEV
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
C                                       message
      NLEV = 4
      IF (IERR.NE.0) THEN
         NLEV = 8
         WRITE (MSGTXT,1000) IERR
      ELSE
         NLEV = 4
         WRITE (MSGTXT,1001)
         END IF
      CALL MSGWRT (NLEV)
      CALL ZVTVRC (LUN, FIND, IERR)
      BUFSW = 0
      BUFSR = 0
C                                       accounting
      CALL ACOUNT (2)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DIES OF UNNATURAL CAUSE NUMBER',I7)
 1001 FORMAT ('Appears to have ended successfully')
      END
      SUBROUTINE TVMONI (LUN, FIND, IRET)
C-----------------------------------------------------------------------
C   TVMONI actually drives the TV device: read (wait) for command;
C   parse the opcode and routine name; call the appropriate routine to
C   decode the parameters, to call the Y routine, and to encode the
C   return parameters; send the parameters back, and loop.
C   Inputs:
C      LUN    I      LUN in use for socket to remote machine
C      FIND   I      FTAB location reserved
C   Output:
C      IRET   I      Error code: 0 => ok
C                       else transmission has failed
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IRET
C
      INCLUDE 'INCS:PTVC.INC'
      CHARACTER XT*10, NAMES(34)*6, SUBNAM*6, OP*4
      INTEGER   IERR, IBUF(8192), INAM, NNAM, I
      REAL      RBUF(TVMOFM)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DVTV.INC'
      DATA NNAM, NAMES /34, 'YALUCT', 'YCHRW ', 'YCINIT', 'YCNECT',
     *   'YCONST', 'YCOVER', 'YCRCTL', 'YCREAD', 'YCUCOR', 'YCURSE',
     *   'YCWRIT', 'YFDBCK', 'YFILL ', 'YFIND ', 'YGRAFX', 'YGRAPH',
     *   'YIFM  ', 'YIMGIO', 'YINIT ', 'YLOCAT', 'YLUT  ', 'YMNMAX',
     *   'YOFM  ', 'YRHIST', 'YSCROL', 'YSHIFT', 'YSLECT', 'YSPLIT',
     *   'YTVCLS', 'YTVMC ', 'YTVOPN', 'YWINDO', 'YZERO ', 'YZOOMC'/
C-----------------------------------------------------------------------
C                                       get assigned TV (TVMONn => n)
      CALL WHOAMI (TSKNAM, INAM, IERR)
      IF (IERR.EQ.0) THEN
         NTVDEV = INAM
      ELSE
         NTVDEV = 1
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (7)
         END IF
C                                       First time: skip return
      GO TO 50
C                                       return buffer, IERR
 10   CONTINUE
         I = BUFSR + 8
         CALL ZILI16 (1, IERR, 8, HEADER)
         CALL VTVRX (FIND, I, HEADER, IRET)
         IF (IRET.EQ.0) GO TO 50
            WRITE (MSGTXT,1010) SUBNAM, OP, IRET
            CALL MSGWRT (8)
            GO TO 999
C                                       read transmission
 50   I = 0
      CALL VTVRX (FIND, I, HEADER, IRET)
      IF (IRET.EQ.0) GO TO 60
         WRITE (MSGTXT,1050) IRET
         CALL MSGWRT (8)
         GO TO 999
C                                       unpack
 60   CALL ZI16IL (1, 6, HEADER, BUFSW)
      CALL ZI16IL (1, 7, HEADER, BUFSR)
      IF (BUFSW.GT.0) CALL ZI16IL (BUFSW, 9, HEADER, BUFFER)
      CALL ZC8CL (10, 1, HEADER, XT)
      OP = XT(1:4)
      SUBNAM = XT(5:)
      DO 70 INAM = 1,NNAM
         IF (SUBNAM.EQ.NAMES(INAM)) GO TO 80
 70      CONTINUE
      WRITE (MSGTXT,1070) SUBNAM, OP
      CALL MSGWRT (6)
      IERR = 2
      GO TO 10
C                                       branch to interpret each
 80   GO TO (110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210,
     *   220, 230, 240, 250, 260, 270, 280, 290, 300, 310, 320, 330,
     *   340, 350, 360, 370, 380, 390, 400, 410, 420, 430, 440), INAM
C-----------------------------------------------------------------------
 110  CONTINUE
         CALL AALUCT (OP, BUFSW, BUFSR, IERR)
         GO TO 10
 120  CONTINUE
         CALL ACHRW (OP, BUFSW, BUFSR, IBUF, IERR)
         GO TO 10
 130  CONTINUE
         CALL ACINIT (OP, BUFSW, BUFSR, IBUF, IERR)
         GO TO 10
 140  CONTINUE
         CALL ACNECT (OP, BUFSW, BUFSR, IBUF, IERR)
         GO TO 10
 150  CONTINUE
         CALL ACONST (OP, BUFSW, BUFSR, IERR)
         GO TO 10
 160  CONTINUE
         CALL ACOVER (OP, BUFSW, BUFSR, IBUF, IERR)
         GO TO 10
 170  CONTINUE
         CALL ACRCTL (OP, BUFSW, BUFSR, IERR)
         GO TO 10
 180  CONTINUE
         CALL ACREAD (OP, BUFSW, BUFSR, IERR)
         GO TO 10
 190  CONTINUE
         CALL ACUCOR (OP, BUFSW, BUFSR, IERR)
         GO TO 10
 200  CONTINUE
         CALL ACURSE (OP, BUFSW, BUFSR, IERR)
         GO TO 10
 210  CONTINUE
         CALL ACWRIT (OP, BUFSW, BUFSR, IBUF, IERR)
         GO TO 10
 220  CONTINUE
         CALL AFDBCK (OP, BUFSW, BUFSR, IERR)
         GO TO 10
 230  CONTINUE
         CALL AFILL (OP, BUFSW, BUFSR, IBUF, IERR)
         GO TO 10
 240  CONTINUE
         CALL AFIND (OP, BUFSW, BUFSR, IBUF, IERR)
         GO TO 10
 250  CONTINUE
         CALL AGRAFX (OP, BUFSW, BUFSR, IERR)
         GO TO 10
 260  CONTINUE
         CALL AGRAPH (OP, BUFSW, BUFSR, IBUF, IERR)
         GO TO 10
 270  CONTINUE
         CALL AIFM (OP, BUFSW, BUFSR, IERR)
         GO TO 10
 280  CONTINUE
         CALL AIMGIO (OP, BUFSW, BUFSR, IERR)
         GO TO 10
 290  CONTINUE
         CALL AINIT (OP, BUFSW, BUFSR, IBUF, IERR)
         GO TO 10
 300  CONTINUE
         CALL ALOCAT (OP, BUFSW, BUFSR, IBUF, RBUF, IERR)
         GO TO 10
 310  CONTINUE
         CALL ALUT (OP, BUFSW, BUFSR, IERR)
         GO TO 10
 320  CONTINUE
         CALL AMNMAX (OP, BUFSW, BUFSR, IERR)
         GO TO 10
 330  CONTINUE
         CALL AOFM (OP, BUFSW, BUFSR, RBUF, IERR)
         GO TO 10
 340  CONTINUE
         CALL ARHIST (OP, BUFSW, BUFSR, IBUF, IERR)
         GO TO 10
 350  CONTINUE
         CALL ASCROL (OP, BUFSW, BUFSR, IERR)
         GO TO 10
 360  CONTINUE
         CALL ASHIFT (OP, BUFSW, BUFSR, IERR)
         GO TO 10
 370  CONTINUE
         CALL ASLECT (OP, BUFSW, BUFSR, IBUF, IERR)
         GO TO 10
 380  CONTINUE
         CALL ASPLIT (OP, BUFSW, BUFSR, IERR)
         GO TO 10
 390  CONTINUE
         CALL ATVCLS (OP, BUFSW, BUFSR, IBUF, IERR)
         GO TO 10
 400  CONTINUE
         CALL ATVMC (OP, BUFSW, BUFSR, IERR)
         GO TO 10
 410  CONTINUE
         CALL ATVOPN (OP, BUFSW, BUFSR, IBUF, IERR)
         GO TO 10
 420  CONTINUE
         CALL AWINDO (OP, BUFSW, BUFSR, IERR)
         GO TO 10
 430  CONTINUE
         CALL AZERO (OP, BUFSW, BUFSR, IERR)
         GO TO 10
 440  CONTINUE
         CALL AZOOMC (OP, BUFSW, BUFSR, IERR)
         GO TO 10
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WHOAMI ERROR',I5,' MUST USE DEFAULTS INCL TV NUMBER 1')
 1010 FORMAT ('ON ',A6,' OP= ',A4,' VTV-WRITE ERROR',I7)
 1050 FORMAT ('VTV-READ ERROR',I7)
 1070 FORMAT ('ROUTINE ',A6,'  OP= ',A4,' UNKNOWN - CONTINUING')
      END
      SUBROUTINE AALUCT (OP, BUFSW, BUFSR, IERR)
C-----------------------------------------------------------------------
C   YALUCT reads / writes the TV arithmetic logic unit control
C   registers.  The actual feedback-ALU computation is performed only
C   upon a call to YFDBCK.
C   Inputs:
C      OP      C*4     'READ' from TV or 'WRIT' to TV
C   In/Out:
C      ARMODE  L       T => arithmetic mode F => logic mode
C      BFUNC   I       function number (1-16) in blotch
C      NFUNC   I       function number (1-16) outside blotch
C      CONSTS  I(8)    constant array (may select as ALU output)
C      OUTSEL  I(8)    lookup table selects output based on carry
C                      (lsb), equal, ROI (msb) input. values -
C                      0 - 7 : constants 1 - 8
C                      8     : accumulator channel pair
C                      9     : selected OFM
C                      10    : ALU
C                      11    : external
C      EXTOFM  L       T => extend sign of OFM on input to ALU
C      ESHIFT  L       T => extend sign of ALU output if SHIFT
C      SHIFT   L       T => right shift ALU output
C      CARYIN  L       T => add one to arithmetic results
C   Output:
C      CARRY   L       T => carry condition occurred in frame
C      EQUAL   L       T => equal condition occurred in frame
C      IERR    I       error code of Z...XF : 0 - ok
C                                             2 - input error
C   Virtual TV version
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      LOGICAL   ARMODE, EXTOFM, ESHIFT, SHIFT, CARYIN, CARRY, EQUAL
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
C                                       Bad op if no ALU units
      IERR = 2
      IF (TVALUS.LE.0) GO TO 999
      IF ((BUFSR.NE.25) .AND. (BUFSW.NE.25)) GO TO 999
C                                       interpret BUFFER
      IF (OP.EQ.'WRIT') THEN
         ARMODE = BUFFER(1).EQ.1
         EXTOFM = BUFFER(20).EQ.1
         ESHIFT = BUFFER(21).EQ.1
         SHIFT = BUFFER(22).EQ.1
         CARYIN = BUFFER(23).EQ.1
         CARRY = BUFFER(24).EQ.1
         EQUAL = BUFFER(25).EQ.1
         END IF
C                                       call Y
      CALL YALUCT (OP, ARMODE, BUFFER(2), BUFFER(3), BUFFER(4),
     *   BUFFER(12), EXTOFM, ESHIFT, SHIFT, CARYIN, CARRY, EQUAL, IERR)
C                                       read: return control parms
      IF (IERR.EQ.0) THEN
         BUFFER(24)  = 0
         IF (CARRY)  BUFFER(24) = 1
         BUFFER(25) = 0
         IF (EQUAL) BUFFER(25) = 1
C                                       set buffer
         IF (OP.NE.'WRIT') THEN
            BUFFER(1) = 0
            IF (ARMODE) BUFFER(1) = 1
            BUFFER(20) = 0
            IF (EXTOFM) BUFFER(20) = 1
            BUFFER(21) = 0
            IF (ESHIFT) BUFFER(21) = 1
            BUFFER(22) = 0
            IF (SHIFT) BUFFER(22) = 1
            BUFFER(23) = 0
            IF (CARYIN) BUFFER(23) = 1
            END IF
C                                       reset buffer
         CALL MKYBUF (OP, 'YALUCT', BUFSR, BUFSW, BUFFER, HEADER)
         END IF
C
 999  RETURN
      END
      SUBROUTINE ACHRW (OP, BUFSW, BUFSR, SCRTCH, IERR)
C-----------------------------------------------------------------------
C   YCHRW  writes characters into image planes of the TV.  The format is
C   5 by 7 with one blank all around: net 7 in X by 9 in Y. This version
C   will work on all TVs which allow horizontal writing to the right. It
C   is a Y routine to allow for hardware character generators on the TV.
C   Inputs:
C      CHAN    I       channel select  (1 to NGRAY + NGRAPH)
C      X       I       X position lower left corner first char.
C      Y       I       Y position lower left corner first char.
C      STRING  C*(*)   character string - length passed from Fortran
C                      VTV - passes length in BUFFER(4) **
C   Output:
C      SCRTCH  I(>)    scratch buffer (dim = 14*count+8 < 1031)
C      IERR    I       error code of Z...XF:0 - ok
C                                           2 - input error
C   Virtual TV version is for TV by COMMUNICATION.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR, SCRTCH(*)
C
      REAL      XT(20)
      CHARACTER STRING*256
      INTEGER   IXT(20), I
      INCLUDE 'INCS:DVTV.INC'
      EQUIVALENCE (XT, IXT)
C-----------------------------------------------------------------------
      IERR = 2
      IF ((BUFSW.LT.4) .OR. (OP.NE.'WRIT')) GO TO 999
C                                       get string
      I = (BUFFER(4) - 1) / 2 + 1
      IF (BUFSW.NE.4+I) GO TO 999
      CALL ZILI16 (I, BUFFER(5), 1, IXT)
      I = BUFFER(4)
      CALL ZC8CL (I, 1, XT, STRING)
C                                       do Y function
      CALL YCHRW (BUFFER(1), BUFFER(2), BUFFER(3), STRING(:I), SCRTCH,
     *   IERR)
C
 999  RETURN
      END
      SUBROUTINE ACINIT (OP, BUFSW, BUFSR, IBUF, IERR)
C-----------------------------------------------------------------------
C   Initialize image catalog for plane IPLANE - TK now done with TKCATL
C   Input:
C      IPLANE   I        Image plane to initialize
C   Output:
C      BUFF     I(256)   Working buffer
C   VIRTUAL TV version for TV by communication.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IBUF(*), IERR
C
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF ((OP.NE.'WRIT') .OR. (BUFSW.NE.1)) GO TO 999
C
      CALL YCINIT (BUFFER(1), IBUF)
      IERR = 0
C
 999  RETURN
      END
      SUBROUTINE ACNECT (OP, BUFSW, BUFSR, LBUFF, IERR)
C-----------------------------------------------------------------------
C   YCNECT writes a line segment on the TV.  This version will work on
C   all TVs.  It is called a Y routine to allow the use of hardware
C   vector generators on those TVs equiped with them.
C   Inputs:
C      X1      I      start X position
C      Y1      I      start Y position
C      X2      I      end X position
C      Y2      I      end Y position
C      IC      I      Channel (1 to NGRAY+NGRAPH)
C      LBUFF   I(*)   Buffer contains desired intensity (size > max
C                     horizontal or vertical line, e.g., 1280)
C   Output:
C      IERR    I      error code : 0 => ok
C   Virtual TV version for TV by COMMUNICATION
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   LBUFF(*), IERR
C
      REAL      R
      INTEGER   I
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF (BUFSW.NE.6) GO TO 999
      IF (OP.NE.'WRIT') GO TO 999
C                                       do Y function
      R = (BUFFER(3)-BUFFER(1)) ** 2 + (BUFFER(4) - BUFFER(2)) ** 2 + 1.
      I = SQRT (ABS(R)) + 0.99
      CALL FILL (I, BUFFER(6), LBUFF)
      CALL YCNECT (BUFFER(1), BUFFER(2), BUFFER(3), BUFFER(4),
     *   BUFFER(5), LBUFF, IERR)
C
 999  RETURN
      END
      SUBROUTINE ACONST (OP, BUFSW, BUFSR, IERR)
C-----------------------------------------------------------------------
C   YCONST reads/writes the constants which are added to the 3 sum
C   channels on the TV.
C   Inputs:
C      OP      C*4   'READ' from TV, 'WRIT' to TV
C   In/out:
C      RCONST  I     constant for red channel
C      GCONST  I     constant for green channel
C      BCONST  I     constant for blue channel
C      VRTRTC  L     (could be Vertical retrace)
C   Output:
C      IERR    I     error code of Z...XF : 0 => ok
C   Virtual TV version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      LOGICAL   VRTRTC
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF ((BUFSR.NE.4) .AND. (BUFSW.NE.4)) GO TO 999
C                                       do it
      VRTRTC = BUFFER(1).EQ.1
      CALL YCONST (OP, BUFFER(2), BUFFER(3), BUFFER(4), VRTRTC, IERR)
C                                        header & IO
      CALL MKYBUF (OP, 'YCONST', BUFSR, BUFSW, BUFFER, HEADER)
C
 999  RETURN
      END
      SUBROUTINE ACOVER (OP, BUFSW, BUFSR, IBUF, IERR)
C-----------------------------------------------------------------------
C   YCOVER checks to see if there are partially replaced images in any
C   of the TV planes currently visible by quadrant
C   Outputs:
C      OVER   L(4)     T => there are in quadr. I
C      BUF    I(512)   scratch
C      IERR   I        Error code: 0 => ok, other catlg IO error
C   Virtual TV version for TV by communication.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IBUF(*), IERR
C
      LOGICAL   OVER(4)
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF ((OP.NE.'READ') .OR. (BUFSR.NE.4)) GO TO 999
      CALL YCOVER (OVER, IBUF, IERR)
      BUFFER(1) = 0
      IF (OVER(1)) BUFFER(1) = 1
      BUFFER(2) = 0
      IF (OVER(2)) BUFFER(2) = 1
      BUFFER(3) = 0
      IF (OVER(3)) BUFFER(3) = 1
      BUFFER(4) = 0
      IF (OVER(4)) BUFFER(4) = 1
      CALL MKYBUF ('READ', 'YCOVER', BUFSR, BUFSW, BUFFER, HEADER)
C
 999  RETURN
      END
      SUBROUTINE ACRCTL (OP, BUFSW, BUFSR, IERR)
C-----------------------------------------------------------------------
C   YCRCTL reads/writes the cursor/trackball control register of TV
C   Inputs:
C      OP      C*4   'READ' from TV or 'WRIT' to TV
C      VRTRTC  L     T => do on vertical retrace only
C   In/out:
C      ON      L     T => cursor visible, F => off
C      X       I     X position cursor center (1-512, 1 => LHS)
C      Y       I     Y position cursor center (1-512, 1 => bot)
C      LINKX   L     T => trackball moves cursor in X
C      LINKY   L     T => trackball moves cursor in Y
C      RBLINK  I     rate of cursor blink: 0-3 no-fast blink
C   Output:
C      BUTTON  I     button value (0 - 15)
C      IERR    I     error code of Z...XF : 0 => ok
C                                           2 => input error
C   Virtual TV (by communication) version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      LOGICAL   ON, LINKX, LINKY, VRTRTC
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF ((BUFSW.NE.8) .AND. (BUFSR.NE.8)) GO TO 999
C                                       parse input buffer
      VRTRTC = BUFFER(1).EQ.1
      IF (OP.EQ.'WRIT') THEN
         ON = BUFFER(2).EQ.1
         LINKX = BUFFER(5).EQ.1
         LINKY = BUFFER(6).EQ.1
         END IF
C                                       do Y function
      CALL YCRCTL (OP, ON, BUFFER(3), BUFFER(4), LINKX, LINKY,
     *   BUFFER(7), BUFFER(8), VRTRTC, IERR)
C                                        return answers
      IF ((IERR.EQ.0) .AND. (OP.NE.'WRIT')) THEN
         BUFFER(2) = 0
         IF (ON) BUFFER(2) = 1
         BUFFER(5) = 0
         IF (LINKX) BUFFER(5) = 1
         BUFFER(6) = 0
         IF (LINKY) BUFFER(6) = 1
         CALL MKYBUF (OP, 'YCRCTL', BUFSR, BUFSW, BUFFER, HEADER)
         END IF
C
 999  RETURN
      END
      SUBROUTINE ACREAD (OP, BUFSW, BUFSR, IERR)
C-----------------------------------------------------------------------
C   Read image catalog block into CATBLK - TV only (TK in TKCATL)
C   Inputs:
C      IPLANE  I       plane containing image whose block is wanted
C      IX      I       X pixel coordinate of a point within image
C      IY      I       Y pixel coordinate of point within image
C   Outputs:
C      CATBLK  I(256)  Image catalog block
C      IERR    I       error codes: 0 => ok
C                         1 => IX, IY lies outside image
C                         2 => Catalog i/o errors
C                         3 => refers to TK device
C   VERSION for TV by COMMUNICATION
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      INTEGER   CATI(256)
      HOLLERITH CATH(256)
      REAL      CATR(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DVTV.INC'
      EQUIVALENCE (CATI, CATH, CATR, CATD, BUFFER(1025))
C-----------------------------------------------------------------------
      IERR = 2
      IF ((OP.NE.'READ') .OR. (BUFSR.NE.302)) GO TO 999
C                                       do request
      CALL YCREAD (BUFFER(1), BUFFER(2), BUFFER(3), CATI, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       pack header
      CALL HDRBUF (1, CATI, CATH, CATR, CATD, BUFFER(4))
      CALL MKYBUF ('READ', 'YCREAD', BUFSR, BUFSW, BUFFER, HEADER)
C
 999  RETURN
      END
      SUBROUTINE ACUCOR (OP, BUFSW, BUFSR, IERR)
C-----------------------------------------------------------------------
C   YCUCOR takes a cursor position (corrected for zoom, but not scroll)
C   corrects it for scroll, determines the quadrant of the TV, and gets
C   the corresponding image header in common /MAPHDR/ and returns the
C   image coordinates.
C   NOTE WELL: RPOS ON INPUT MUST BE CORRECTED FOR ZOOM AND NOT SCROLL.
C   To get this from a raw TV position call YCURSE with OPCODE 'FXIT'
C   (or, of course, 'READ') and quadrant set to -1.    *************
C   Inputs:
C      RPOS    R(2)    X,Y screen pos before  zoom & scroll
C   Output:
C      QUAD    I       TV quadrant to use for scrolls
C                      Out: if in=-1, no scroll, else find quadrant
C                           (needs real TV pos)
C      CORN    R(7)    Image coordinates (pixels)
C      IERR    I       error code of Z...XF : 0 - ok
C                                             2 - input error
C   VIRTUAL TV version for TV by communication.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      REAL      RPOS(2), CORN(7)
      INTEGER   I, J
      INCLUDE 'INCS:DVTV.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF ((OP.NE.'READ') .OR. (BUFSR.NE.318)) GO TO 999
      RPOS(1) = BUFFER(1) + BUFFER(2) / 1000.0
      RPOS(2) = BUFFER(3) + BUFFER(4) / 1000.0
      CALL YCUCOR (RPOS, BUFFER(5), CORN, IERR)
C                                       send back
      IF (IERR.EQ.0) THEN
         DO 10 I = 1,7
            J = 2 * I + 4
            BUFFER(J) = CORN(I)
            BUFFER(J+1) = 1000.0 * (CORN(I) - BUFFER(J))
 10         CONTINUE
         CALL HDRBUF (1, CATBLK, CATH, CATR, CATD, BUFFER(20))
         CALL MKYBUF ('READ', 'YCUCOR', BUFSR, BUFSW, BUFFER, HEADER)
         END IF
C
 999  RETURN
      END
      SUBROUTINE ACURSE (OP, BUFSW, BUFSR, IERR)
C-----------------------------------------------------------------------
C   YCURSE reads cursor positions and controls the blink and visibility
C   of the TV cursor.
C   Inputs:
C      OP      C*4     'READ' read cursor position
C                      'ONNN' place cursor at RPOS & leave on
C                      'OFFF' turn cursor off
C                      'BLNK' reverse sense of cursor blink
C                      'FXIT' fix RPOS for zoom scroll, no IO
C      WAIT    L       wait for event; then return RPOS & BUTTON
C                      (done on all OPs)
C      CORR    L       T => correct RPOS for zoom & scroll
C   In/Out:
C      RPOS    R(2)    X,Y screen pos before  zoom & scroll
C      QUAD    I       TV quadrant to use for scrolls
C                         In: if <1 >4, no scroll
C                         Out: if in=-1, no scroll, else find
C                              quadrant (needs real TV pos)
C   Output:
C      BUTTON  I       event # (0 none, 1-7 low buttons,
C                      8-15 the "quit" button)
C      IERR    I       error code of Z...XF : 0 - ok
C                                             2 - input error
C   Virtual TV version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      CHARACTER ROP*4, OPCODE(5)*4
      REAL      RPOS(2), T
      INTEGER   IROUND
      LOGICAL   WAIT, CORR
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DVTV.INC'
      DATA OPCODE /'READ','ONNN','OFFF','BLNK','FXIT'/
C-----------------------------------------------------------------------
      IERR = 2
      IF ((OP.NE.'READ') .OR. (BUFSR.NE.9) .OR. (BUFSW.NE.9)) GO TO 999
C                                       do call
      ROP = OPCODE(BUFFER(1)+1)
      WAIT = BUFFER(2).GT.0
      CORR = BUFFER(3).GT.0
      RPOS(1) = BUFFER(4) + BUFFER(5) / 1000.0
      RPOS(2) = BUFFER(6) + BUFFER(7) / 1000.0
      CALL YCURSE (ROP, WAIT, CORR, RPOS, BUFFER(8), BUFFER(9), IERR)
      IF (IERR.NE.0) GO TO 999
C                                       returns
      BUFFER(4) = RPOS(1)
      T = 1000.0 * (RPOS(1) - BUFFER(4))
      BUFFER(5) = IROUND (T)
      BUFFER(6) = RPOS(2)
      T = 1000.0 * (RPOS(2) - BUFFER(6))
      BUFFER(7) = IROUND (T)
      CALL MKYBUF ('READ', 'YCURSE', BUFSR, BUFSW, BUFFER, HEADER)
C
 999  RETURN
      END
      SUBROUTINE ACWRIT (OP, BUFSW, BUFSR, IBUF, IERR)
C-----------------------------------------------------------------------
C   Write image catalog block in CATBLK into image catalog
C   Inputs:
C      IPLANE   I        image plane involved
C      IMAWIN   I(4)     Corners of image on screen
C      CATBLK   I(256)   Image catalog block
C  Outputs:
C      BUFF     I(256)   working buffer
C      IERR     I        error code: 0 => ok
C                           1 => no room in catalog
C                           2 => IO problems
C   Virtual TV version for TV by communication.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IBUF(256), IERR
C
      REAL      CATR(256)
      INTEGER   CATI(256), I
      DOUBLE PRECISION CATD(128)
      HOLLERITH CATH(256)
      INCLUDE 'INCS:DVTV.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (CATI, CATH, CATR, CATD, BUFFER(1025))
C-----------------------------------------------------------------------
      IERR = 2
      IF ((OP.NE.'WRIT') .OR. (BUFSW.NE.304)) GO TO 999
C                                       unpack header
      CALL HDRBUF (2, CATI, CATH, CATR, CATD, BUFFER(6))
      CALL YCWRIT (BUFFER(1), BUFFER(2), CATI, IBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) (BUFFER(I), I = 1,5)
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ACWRIT: IPLANE, IMAWIN =',I5,2X,4I7)
      END
      SUBROUTINE AFDBCK (OP, BUFSW, BUFSR, IERR)
C-----------------------------------------------------------------------
C   YFDBCK sends a feedback command to the TV
C   Inputs:
C      COLOR   I    bit map of color to be fedback (RGB = 4,2,1)
C      CHANNL  I    bit map of channels to receive feedback
C      BITPL   I    bit map of bit planes to receive feedback
C      PIXOFF  I    offset fedback image to left by 0 - 1 pixels
C      BYPIFM  L    F => image goes thru IFM lookup before store
C      EXTERN  L    T => image from external input (iedigitizer)
C      ZERO    L    T => feed back all zeros
C      ACCUM   L    T => use 16-bit accumulator mode then CHANNL must
C                        give even-odd pair lsbyte goes to even (lower)
C                        # channel
C      ADDWRT  L    T => additive write  F => replace old data
C   Outputs:
C      IERR    I    error code of Z...XF: 0 -> ok
C                                         2 -> input error
C   Virtual TV version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      LOGICAL   BYPIFM, EXTERN, ZERO, ACCUM, ADDWRT
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF ((OP.NE.'WRIT') .OR. (BUFSW.NE.9)) GO TO 999
      BYPIFM = BUFFER(5).GT.0
      EXTERN = BUFFER(6).GT.0
      ZERO = BUFFER(7).GT.0
      ACCUM = BUFFER(8).GT.0
      ADDWRT = BUFFER(9).GT.0
      CALL YFDBCK (BUFFER(1), BUFFER(2), BUFFER(3), BUFFER(4),
     *   BYPIFM, EXTERN, ZERO, ACCUM, ADDWRT, IERR)
C
 999  RETURN
      END
      SUBROUTINE AFILL (OP, BUFSW, BUFSR, IBLK, IERR)
C-----------------------------------------------------------------------
C   YFILL will write a constant in a given rectangle in a given graphics
C   or image plane.  It will use fast methods if full screen requested
C   with IVAL 0.
C   Inputs:
C      CHAN     I   Channel (1 to NGRAY+NGRAPH)
C      IX0      I   lower left X pixel (1 relative) of rectangle
C      IY0      I   lower left Y pixel of rectangle.
C      IXT      I   top right X pixel of rectangle.
C      IYT      I   top right Y pixel of rectangle.
C      IVAL     I   desired value: for graphics = 0 or 1
C                                  for grey scale = 0 - MAXINT
C   In/out:
C      IBLK     I(IXT-IX0+1)   work buffer.
C   Output:
C      IERR     I   error code of Z...XF: 0 ok, 2 input error
C   Virtual TV (by communication) version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IBLK(*), IERR
C
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF ((OP.NE.'WRIT') .OR. (BUFSR.NE.0) .OR. (BUFSW.NE.6)) GO TO 999
C
      CALL YFILL (BUFFER(1), BUFFER(2), BUFFER(3), BUFFER(4), BUFFER(5),
     *   BUFFER(6), IBLK, IERR)
C
 999  RETURN
      END
      SUBROUTINE AFIND (OP, BUFSW, BUFSR, SCRTCH, IERR)
C-----------------------------------------------------------------------
C   YFIND determines which of the visible TV images the user wishes
C   to select.  The TV must already be open.
C   Inputs:
C      MAXPL   I        Highest plane number allowed (i.e. do graphics
C                       count?)
C      TYPE    C*2      2-char image type to restrict search
C   Output:
C      IPL     I        Plane number found
C      UNIQUE  L        T => only one image visible now
C                           (all types except zeroed ones ('ZZ'))
C      CATBLK  I(256)   Image catalog block found
C      SCRTCH  I(256)   Scratch buffer
C      IERR    I        Error code: 0 => ok
C                          1 => no image
C                          2 => IO error in image catalog
C                          3 => TV error
C                         10 => > 1 image of requested type
C   Virtual TV version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   SCRTCH(*), IERR
C
      CHARACTER TYPE*2
      LOGICAL   UNIQUE
      INTEGER   CATI(256), IXT
      REAL      CATR(256)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DVTV.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (CATI, CATH, CATR, CATD, BUFFER(1025))
C-----------------------------------------------------------------------
      IERR = 2
      IF ((OP.NE.'READ') .OR. (BUFSR.NE.303)) GO TO 999
C                                       set up write
      CALL ZILI16 (1, BUFFER(2), 1, IXT)
      CALL ZC8CL (2, 1, IXT, TYPE)
      CALL YFIND (BUFFER(1), TYPE, BUFFER(3), UNIQUE, CATI, SCRTCH,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) BUFFER(1), BUFFER(2), BUFFER(2), TYPE
         CALL MSGWRT (7)
C                                       return answers
      ELSE
         BUFFER(4) = 0
         IF (UNIQUE) BUFFER(4) = 1
         CALL HDRBUF (1, CATI, CATH, CATR, CATD, BUFFER(5))
         CALL MKYBUF ('READ', 'YFIND', BUFSR, BUFSW, BUFFER, HEADER)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AFIND ERR ON ',2I7,2(2X,'''',A2,''''))
      END
      SUBROUTINE AGRAFX (OP, BUFSW, BUFSR, IERR)
C-----------------------------------------------------------------------
C   YGRAFX modifies the colour of the graphics or cursor.
C   Inputs:
C      OP       C*4   READ or WRITE graphics colour assignment.
C      GRFX     I     Cursor (0), or graphics plane number.
C   In/Out:
C      RED      R     Red,   0.0 - 1.0
C      GREEN    R     Green, 0.0 - 1.0
C      BLUE     R     Blue,  0.0 - 1.0
C   Output:
C      IERR     I     Error status, 0 means success.
C   Version for Virtual TV devices.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      REAL      BLUE, GREEN, RED
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      IF (OP.EQ.'WRIT') THEN
         RED = BUFFER(2) / 10000.0
         GREEN = BUFFER(3) / 10000.0
         BLUE = BUFFER(4) / 10000.0
         END IF
C                                       do it
      CALL YGRAFX (OP, BUFFER(1), RED, GREEN, BLUE, IERR)
C                                       return
      IF (IERR.EQ.0) THEN
         IF (OP.EQ.'WRIT') THEN
            BUFFER(2) = RED * 10000.0 + 0.5
            BUFFER(3) = GREEN * 10000.0 + 0.5
            BUFFER(4) = BLUE * 10000.0 + 0.5
            END IF
         CALL MKYBUF (OP, 'YGRAFX', BUFSR, BUFSW, BUFFER, HEADER)
         END IF
C
 999  RETURN
      END
      SUBROUTINE AGRAPH (OP, BUFSW, BUFSR, IBUF, IERR)
C-----------------------------------------------------------------------
C   YGRAPH is used to turn graphics overlay planes on & off by
C   altering the graphics color look up table.  The color pattern is:
C      CHAN = 1    insert   yellow            drawing plots
C             2    insert   green+.05 red     axis labels
C             3    insert   blue + 0.6 green  blotch
C                           + red
C             4    insert   black             label backgrounds
C             5-7  add      nothing           null channels
C             8    insert   purple            cursor
C   Inputs:
C      OP      C*4       'ONNN' or 'OFFF'
C                        'INIT' also allowed, though should only be
C                        secondary and hence not used in VTV
C      CHAN    I         channel number (1 - 8)
C   Output:
C      SCRTCH  I(256)    scratch buffer
C      IERR    I         error code of Z...XF: 0 => ok
C                                              2 => input error
C   Virtual TV version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IBUF(256), IERR
C
      CHARACTER ROP*4
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
C                                        check input
      IERR = 2
      IF ((OP.NE.'WRIT') .OR. (BUFSW.NE.2)) GO TO 999
C                                        do it
      IF (BUFFER(1).EQ.1) THEN
         ROP = 'ONNN'
      ELSE IF (BUFFER(1).EQ.2) THEN
         ROP = 'INIT'
      ELSE
         ROP = 'OFFF'
         END IF
      CALL YGRAPH (ROP, BUFFER(2), IBUF, IERR)
C
 999  RETURN
      END
      SUBROUTINE AIFM (OP, BUFSW, BUFSR, IERR)
C-----------------------------------------------------------------------
C   YIFM reads/writes a section of TV input function memory.  This
C   look up table takes 13 bits in and gives 8 bits out.
C   Inputs:
C      OP      C*4    'READ' from TV or 'WRIT' to TV
C      START   I      start address of IFM (1 - 8192)
C      COUNT   I      # elements of IFM to transfer (1-8192)
C      PACK    L      T => 2 values/word, F => 1 value/word
C      VRTRTC  L      T => do it only on vertical retrace
C   In/Out:
C      IFM     I(*)   function values (0-255)
C   Output:
C      IERR    I      error code of Z...XF: 0 - ok
C                                           2 - input error
C   Virtual TV version (TV by communication)
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      LOGICAL   PACK, VRTRTC
      INTEGER   I
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
C                                       check
      IERR = 2
C                                       Get parms
      PACK = BUFFER(3).EQ.1
      VRTRTC = BUFFER(4).EQ.1
      I = BUFFER(2) - BUFFER(1) + 1
      IF (PACK) I = (I + 1) / 2
      IF ((OP.EQ.'WRIT') .AND. (BUFSW.NE.4+I)) GO TO 999
      IF ((OP.NE.'WRIT') .AND. (BUFSR.NE.4+I)) GO TO 999
C                                       do it
      CALL YIFM (OP, BUFFER(1), BUFFER(2), PACK, VRTRTC, BUFFER(5),
     *   IERR)
C                                       return parms
      IF (IERR.EQ.0) CALL MKYBUF (OP, 'YIFM', BUFSR, BUFSW, BUFFER,
     *   HEADER)
C
 999  RETURN
      END
      SUBROUTINE AIMGIO (OP, BUFSW, BUFSR, IERR)
C-----------------------------------------------------------------------
C   YIMGIO reads/writes a line of image data to the TV screen.  For
C   graphics overlay planes, the data are solely 0's and 1's in the
C   least significant bit of IMAGE after a READ.  For 'WRIT', all bits
C   of each word should be equal (i.e. all 1's or all 0's for graphics).
C   NOTE***** on 'WRIT', the buffer may be altered by this routine for
C   some IANGLs.
C   Inputs:
C      OP      C*4       'READ' from TV or 'WRIT' to TV
C      CHAN    I         channel number (1 to NGRAY+NGRAPH)
C      X       I         start pixel position
C      Y       I         end pixel position
C      IANGL   I         = 0 => horizontal (to right)
C                        = 1 => vertical (up the screen)
C                        = 2 => horizontal (to left)
C                        = 3 => vertical (down the screen)
C      NPIX    I         number of pixels
C   In/Out:
C      IMAGE   I(NPIX)   data (only no header)
C   Output:
C      IERR    I         error code of Z...XF - 0 => ok
C                                                  2 => input err
C   Virtual TV version (for TV by communication).
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
C                                        check input
      IERR = 2
      IF ((OP.NE.'READ') .AND. (OP.NE.'WRIT')) GO TO 999
      IF ((OP.EQ.'WRIT') .AND. (BUFSW.NE.5+BUFFER(5))) GO TO 999
      IF ((OP.EQ.'READ') .AND. (BUFSR.NE.5+BUFFER(5))) GO TO 999
C                                       do I/O
      CALL YIMGIO (OP, BUFFER(1), BUFFER(2), BUFFER(3), BUFFER(4),
     *   BUFFER(5), BUFFER(6), IERR)
      IF ((IERR.EQ.0) .AND. (OP.NE.'WRIT')) CALL MKYBUF (OP, 'YIMGIO',
     *   BUFSR, BUFSW, BUFFER, HEADER)
C
 999  RETURN
      END
      SUBROUTINE AINIT (OP, BUFSW, BUFSR, IBUF, IERR)
C-----------------------------------------------------------------------
C   YINIT initializes the TV subunits: doing everything
C   Output:
C      SCRTCH  I(1024)    scratch buffer
C      IERR    I          error code of Z...XF - 0 => ok
C                                                2 => input error
C   Virtual TV version for TV by communication
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IBUF(*), IERR
C
      INCLUDE 'INCS:DVTV.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                       do it first
      IERR = 2
      IF ((BUFSR.NE.256) .OR. (OP.NE.'READ')) GO TO 999
      CALL YINIT (IBUF, IERR)
C                                       return the common
      CALL MKYBUF (OP, 'YINIT', BUFSR, BUFSW, NGRAY, HEADER)
C
 999  RETURN
      END
      SUBROUTINE ALOCAT (OP, BUFSW, BUFSR, IBUF, RBUF, IERR)
C-----------------------------------------------------------------------
C   YLOCAT locates a set of image pixel positions on the TV for a
C   specified image using only those grey planes that are turned on.
C   Inputs:
C      NP      I         Number of pixel positions
C      XP      R(NP)     X image pixel positions
C      YP      R(NP)     Y image pixel positions
C      NAME    C*12      Packed image name (name) '    ' => any
C      CLASS   C*6       Packed image name (class) '    ' => any
C      SEQ     I         Image name (sequence #)  0 => any
C      DISK    I         Image file disk 0 => any
C      PTYP    C*2       Image type  '  ' => any
C   Output:
C      IX      I(NP)     TV x positions
C      IY      I(NP)     TV y positions
C      IQ      I(NP)     TV channels  0 => none this position
C      IERR    I         Error code: 0 -> ok
C                           2 -> input error
C                           3 -> IO error
C                          11 -> some positions bad
C                          12 -> no positions found
C   Uses common /MAPHDR/ results unpredictable except on IERR = 0
C   then = image catlg header of last position found.
C   Virtual TV version (TV by communication).
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IBUF(*), IERR
      REAL      RBUF(*)
C
      CHARACTER NAME*12, CLASS*6, XT*20, PTYP*2
      INTEGER   NP, I, IXT(10), IP
      INCLUDE 'INCS:DVTV.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF (OP.NE.'READ') GO TO 999
      NP = BUFFER(1)
      IF (BUFSR.NE.13+299+7*NP) GO TO 999
C                                       unpack inputs
      CALL ZILI16 (10, BUFFER(2), 1, IXT)
      CALL ZC8CL (20, 1, IXT, XT)
      NAME = XT(1:12)
      CLASS = XT(13:18)
      PTYP = XT(19:20)
      IP = 14
      DO 20 I = 1,NP
         RBUF(I) = BUFFER(IP) + BUFFER(IP+1) / 1000.0
         RBUF(I+NP) = BUFFER(IP+2) + BUFFER(IP+3) / 1000.0
         IP = IP + 4
 20      CONTINUE
      CALL YLOCAT (NP, RBUF(1), RBUF(NP+1), NAME, CLASS, BUFFER(12),
     *   BUFFER(13), PTYP, BUFFER(IP), BUFFER(IP+NP), BUFFER(IP+2*NP),
     *   IBUF, IERR)
C                                       pack returns
      IF ((IERR.EQ.0) .OR. (IERR.EQ.11)) THEN
         IP = IP + 3 * NP
         CALL HDRBUF (1, CATBLK, CATH, CATR, CATD, BUFFER(IP))
         CALL MKYBUF ('READ', 'YLOCAT', BUFSR, BUFSW, BUFFER, HEADER)
         END IF
C
 999  RETURN
      END
      SUBROUTINE ALUT (OP, BUFSW, BUFSR, IERR)
C-----------------------------------------------------------------------
C   YLUT reads/writes full channel look up tables to TV.
C   Inputs:
C      OP      C*4    'READ' from TV, 'WRIT' to TV
C      CHANNL  I      channel select bit mask
C      COLOR   I      color select bit mask (RGB <-> 421)
C      VRTRTC  L      T => do it only during vertical retrace
C   In/Out:
C      LUT     I(*)   look up table (dimension = MAXINT+1, values to
C                     LUTOUT are used)
C   Out:
C      IERR    I      error code of Z...XF : 0 => ok, 2 => input error
C   VIRTUAL TV (by communication) version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      INTEGER   I
      LOGICAL   VRTRTC
      INCLUDE 'INCS:DVTV.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      IERR = 2
      I = MAXINT + 1
      IF ((OP.NE.'READ') .AND. (BUFSW.NE.3+I)) GO TO 999
      IF ((OP.EQ.'READ') .AND. (BUFSR.NE.3+I)) GO TO 999
C                                       do it
      VRTRTC = BUFFER(3).EQ.1
      CALL YLUT (OP, BUFFER(1), BUFFER(2), VRTRTC, BUFFER(4), IERR)
C                                       return
      IF (IERR.EQ.0) CALL MKYBUF (OP, 'YLUT', BUFSR, BUFSW, BUFFER,
     *   HEADER)
C
 999  RETURN
      END
      SUBROUTINE AMNMAX (OP, BUFSW, BUFSR, IERR)
C-----------------------------------------------------------------------
C   YMNMAX reads the min & max values put out by the 3 summers (before
C   application of constants, shifts and OFM) from the TV
C   Inputs:
C      VRTRTC  L    do it on vertical retrace only
C   Output:
C      RMIN    I    red minimum
C      RMAX    I    red maximum
C      GMIN    I    green minimum
C      GMAX    I    green maximum
C      BMIN    I    blue minimum
C      BMAX    I    blue maximum
C      IERR    I    error code of Z...XF : 0 => ok, 2 => input error
C   Virtual TV (by communication) version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      LOGICAL   VRTRTC
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF ((OP.NE.'READ') .OR. (BUFSR.NE.7)) GO TO 999
C                                       do it
      VRTRTC = BUFFER(1).EQ.1
      CALL YMNMAX (BUFFER(2), BUFFER(3), BUFFER(4), BUFFER(5),
     *   BUFFER(6), BUFFER(7), VRTRTC, IERR)
      CALL MKYBUF ('READ', 'YMNMAX', BUFSR, BUFSW, BUFFER, HEADER)
C
 999  RETURN
      END
      SUBROUTINE AOFM (OP, BUFSW, BUFSR, RBUF, IERR)
C-----------------------------------------------------------------------
C   YOFM reads/writes full OFM look up tables to TV.
C   Inputs:
C      OP      C*4    'READ' from TV, 'WRIT' to TV
C      COLOR   I      color select bit mask (RGB <-> 421)
C      VRTRTC  L      T => do it only during vertical retrace
C   In/Out:
C      OFM     R(*)   look up table (dimension = OFMINP+1, values 0
C                     through 1 are used, others clipped)
C   Output:
C      IERR    I      error code of Z...XF : 0 => ok, 2 => input error
C   Virtual TV (by communication) version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
      REAL      RBUF(*)
C
      INTEGER   I, J
      LOGICAL   VRTRTC
      INCLUDE 'INCS:DVTV.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      IERR = 2
      J = OFMINP + 1
      IF ((OP.EQ.'WRIT') .AND. (BUFSW.NE.J+2)) GO TO 999
      IF ((OP.NE.'WRIT') .AND. (BUFSR.NE.J+2)) GO TO 999
C                                       do it
      VRTRTC = BUFFER(2).EQ.1
      IF (OP.EQ.'WRIT') THEN
         DO 10 I = 1,J
            RBUF(I) = BUFFER(I+2) / 10000.0
 10         CONTINUE
         END IF
      CALL YOFM (OP, BUFFER(1), VRTRTC, RBUF, IERR)
      IF ((IERR.EQ.0) .AND. (OP.NE.'WRIT')) THEN
         DO 20 I = 1,J
             BUFFER(I+2) = MAX (0.0, MIN (1.0, RBUF(I))) * 10000.0
 20          CONTINUE
         CALL MKYBUF (OP, 'YOFM', BUFSR, BUFSW, BUFFER, HEADER)
         END IF
C
 999  RETURN
      END
      SUBROUTINE ARHIST (OP, BUFSW, BUFSR, SCRTCH, IERR)
C-----------------------------------------------------------------------
C   YRHIST reads the histogram of the output of a selected OFM of the
C   TV.   Intensity values 0 through OFMOUT may be read.
C   Inputs:
C      MODE    I         selects area to histogram: 0 blotch,
C                        1 not blotch, 2 all, 3 external blotch
C      COLOR   I         bit map of single color (RGB - 4,2,1)
C      INITI   I         first intensity to histo (1 - 1024)
C      NINT    I         # values to get
C   Output:
C      HISTOG  I(NINT)   histogram
C      IERR    I         error code of Z...XF : 0 => ok, 2 => input err
C   Virtual TV version (TV by communication).  Requires 2 words per
C   value in the transmission part since 16-bit integers are used there.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   SCRTCH(*), IERR
C
      INTEGER   J, ISC, NINT, I
      INCLUDE 'INCS:DVTV.INC'
      DATA ISC /16384/
C-----------------------------------------------------------------------
C                                        check errors
      IERR = 2
      NINT = BUFFER(4)
      IF ((OP.NE.'READ') .OR. (BUFSR.NE.2*NINT+4)) GO TO 999
      CALL YRHIST (BUFFER(1), BUFFER(2), BUFFER(3), NINT, SCRTCH, IERR)
C                                       return values (go in 16-bit int)
      IF (IERR.EQ.0) THEN
         J = 5
         DO 10 I = 1,NINT
            BUFFER(J) = SCRTCH(I) / ISC
            BUFFER(J+1) = MOD (SCRTCH(I), ISC)
            J = J + 2
 10         CONTINUE
         CALL MKYBUF ('READ', 'YRHSIT', BUFSR, BUFSW, BUFFER, HEADER)
         END IF
C
 999  RETURN
      END
      SUBROUTINE ASCROL (OP, BUFSW, BUFSR, IERR)
C-----------------------------------------------------------------------
C   YSCROL writes the scroll registers on the TV.
C   Inputs:
C      CHANNL   I   bit map channel select: bits 1-NGRAY gray channels,
C                   bit NGRAY+1 => all graphics
C      VRTRTC   L   T => do it on vertical retrace only
C   In/Out:
C      SCROLX   I   amount of X scroll (>0 to right)
C      SCROLY   I   amount of Y scroll (>0 upwards)
C   Output:
C      IERR     I   error from Z...XF : 0 => ok, 2 => input error
C   YSCROL updates the scroll variables in /TVDEV/ common
C   Virtual TV version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      LOGICAL   VRTRTC
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF ((OP.NE.'READ') .OR. (BUFSR.NE.4)) GO TO 999
C                                        header & IO
      VRTRTC = BUFFER(4).EQ.1
      CALL YSCROL (BUFFER(1), BUFFER(2), BUFFER(3), VRTRTC, IERR)
      IF (IERR.EQ.0) CALL MKYBUF ('READ', 'YSCROL', BUFSR, BUFSW,
     *   BUFFER, HEADER)
C
 999  RETURN
      END
      SUBROUTINE ASHIFT (OP, BUFSW, BUFSR, IERR)
C-----------------------------------------------------------------------
C   YSHIFT reads/writes the TV shift registers - which shift the
C   13-bit output of the adders before entry into the 10-bit OFM
C   Inputs:
C      OP      C*4   'READ' from TV or 'WRIT' to TV
C      VRTRTC  L     T => do on vertical retrace only
C   In/Out:
C      SHIFTR  I     # bits to shift (right) red channel
C      SHIFTG  I     # bits to shift green channel
C      SHIFTB  I     # bits to shift blue channel
C   Output:
C      IERR    I     error code of Z...XF : 0 - ok, 2 - input error
C   TV by communication version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      LOGICAL   VRTRTC
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF ((OP.EQ.'WRIT') .AND. (BUFSW.NE.4)) GO TO 999
      IF ((OP.NE.'WRIT') .AND. (BUFSR.NE.4)) GO TO 999
C                                       do it
      VRTRTC = BUFFER(1).EQ.1
      CALL YSHIFT (OP, BUFFER(2), BUFFER(3), BUFFER(4), VRTRTC, IERR)
      IF (IERR.EQ.0) CALL MKYBUF (OP, 'YSHIFT', BUFSR, BUFSW, BUFFER,
     *   HEADER)
C
 999  RETURN
      END
      SUBROUTINE ASLECT (OP, BUFSW, BUFSR, LBUFF, IERR)
C-----------------------------------------------------------------------
C   YSLECT enables and disables gray and graphics planes
C   Inputs:
C      OP       C*4      'ONNN' or 'OFFF'
C      CHAN     I        channel number ( 1 to NGRAY+NGRAPH)
C      COLOR    I        0 - all,   1,2,3 = R,G,B, resp.
C   Output:
C      LBUFF    I(256)   scratch buffer (for graphics only)
C      IERR     I        error code of Z...XF: 0 - ok, 2 - input error
C   YSLECT sets TVLIMG in the TV device parms common /TVDEV/
C   Virtual TV version (TV by communication).
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   LBUFF(*), IERR
C
      CHARACTER OPR*4
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
C                                        check input
      IERR = 2
      IF ((OP.NE.'WRIT') .OR. (BUFSW.NE.3)) GO TO 999
C                                       send parms
      OPR = 'OFFF'
      IF (BUFFER(1).EQ.1) OPR = 'ONNN'
      CALL YSLECT (OPR, BUFFER(2), BUFFER(3), LBUFF, IERR)
C
 999  RETURN
      END
      SUBROUTINE ASPLIT (OP, BUFSW, BUFSR, IERR)
C-----------------------------------------------------------------------
C   YSPLIT reads/writes the look up table/ split screen control
C   registers of the TV - turns channels on/off by quadrant
C   Inputs:
C      OP       C*4    'READ' from TV, 'WRIT' to TV
C      VRTRTC   L      T => do on vertical retrace only
C   In/Out:
C      XSPLT    I      X position of split (1-512, 1 => LHS)
C      YSPLT    I      Y position of split (1-512, 1 => bot)
C      RCHANS   I(4)   chan select bit mask 4 quadrants : red
C      GCHANS   I(4)   chan select bit mask 4 quadrants : green
C      BCHANS   I(4)   chan select bit mask 4 quadrants : blue
C   Output:
C      IERR     I      error code of Z...XF: 0 => ok, 2 => input error
C   Quadrants are numbered CCW from top right!!!!
C   Version for TV by communication
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      LOGICAL   VRTRTC
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF ((OP.EQ.'WRIT') .AND. (BUFSW.NE.15)) GO TO 999
      IF ((OP.NE.'WRIT') .AND. (BUFSR.NE.15)) GO TO 999
C                                       do it
      VRTRTC = BUFFER(1).EQ.1
      CALL YSPLIT (OP, BUFFER(2), BUFFER(3), BUFFER(4), BUFFER(8),
     *   BUFFER(12), VRTRTC, IERR)
      IF ((IERR.EQ.0) .AND. (OP.NE.'WRIT')) CALL MKYBUF (OP, 'YSPLIT',
     *   BUFSR, BUFSW, BUFFER, HEADER)
C
 999  RETURN
      END
      SUBROUTINE ATVCLS (OP, BUFSW, BUFSR, BUF, IERR)
C-----------------------------------------------------------------------
C   YTVCLS closes the TV device and the TV status disk file, updating
C   the information on the disk.  Actual device call done by YTVCL2.
C   Outputs:
C      BUF   I(256)     Scratch buffer
C      IERR  I          Error code : 0 => ok
C                          else as returned by ZFIO
C                          11 => close disk error
C                          12 => close device error
C   Virtual TV (by communication) version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   BUF(*), IERR
C
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF ((OP.NE.'WRIT') .OR. (BUFSW.NE.0)) GO TO 999
C                                       close disk and device
      CALL TVCLOS (BUF, IERR)
C
 999  RETURN
      END
      SUBROUTINE ATVMC (OP, BUFSW, BUFSR, IERR)
C-----------------------------------------------------------------------
C   YTVMC issues a "master clear" to the TV.  This resets the TV IO
C   system (if necessary) to expect a command record next.
C   YTVMC gets all needed parameters from the TV device common.
C   The TV must already be open.
C   Virtual TV (by communications) version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF ((OP.NE.'WRIT') .OR. (BUFSR.NE.0) .OR. (BUFSW.NE.0)) GO TO 999
      CALL YTVMC
      IERR = 0
C
 999  RETURN
      END
      SUBROUTINE ATVOPN (OP, BUFSW, BUFSR, BUF, IERR)
C-----------------------------------------------------------------------
C   YTVOPN opens the TV including TV lock/parameter file and reads the
C   parameters, placing them in commons.
C   Outputs:
C      BUF   I(256)   Scratch buffer
C      IERR  I        Error return from YTVOP2
C                        = 10 TV unavailable to this version
C   Virtual TV version - from the wire.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   BUF(*), IERR
C
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF ((OP.NE.'READ') .OR. (BUFSR.NE.256)) GO TO 999
      CALL TVOPEN (BUF, IERR)
      IF (IERR.EQ.0) CALL MKYBUF ('READ', 'YTVOPN', BUFSR, BUFSW, NGRAY,
     *   HEADER)
C
 999  RETURN
      END
      SUBROUTINE AWINDO (OP, BUFSW, BUFSR, IERR)
C-----------------------------------------------------------------------
C   YWINDO reads the current viewport into the TV memory.  It is hoped
C   that someday we will also offer writing to force the size.
C   Inputs:
C      OPER   C*4    'READ', 'WRIT'
C   In/out:
C      WIND   I(4)   BLC x,y, TRC x,y of window in TV pixels
C                    In: desired window ('WRIT' only)
C                    Out: actual window given
C   Output:
C      IERR    I     error code of Z...XF: 0 -> ok, 2 -> input error
C   Virtual TV version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF ((OP.NE.'READ') .AND. (OP.NE.'WRIT')) GO TO 999
      IF ((BUFSR.NE.4) .OR. (BUFSW.NE.4)) GO TO 999
C                                       do it
      CALL YWINDO (OP, BUFFER, IERR)
      IF (IERR.EQ.0) CALL MKYBUF (OP, 'YWINDO', BUFSR, BUFSW, BUFFER,
     *   HEADER)
C
 999  RETURN
      END
      SUBROUTINE AZERO (OP, BUFSW, BUFSR, IERR)
C-----------------------------------------------------------------------
C   YZERO fills an TV TV memory plane with zeros the fast way.
C   Inputs:
C      CHAN   I     channel # ( 1 - NGRAY+NGRAPH), 0 => all
C   Outputs:
C      IERR   I     error code of Z...XF: 0 - ok, 2 - input error
C   Virtual TV version
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF ((OP.NE.'WRIT') .OR. (BUFSW.NE.1)) GO TO 999
C
      CALL YZERO (BUFFER(1), IERR)
C
 999  RETURN
      END
      SUBROUTINE AZOOMC (OP, BUFSW, BUFSR, IERR)
C-----------------------------------------------------------------------
C   YZOOM writes (ONLY!!!!) the zoom control registers of the TV
C   Inputs:
C      MAG     I     0-3 for magnification 1,2,4,8 times, resp.
C      XZOOM   I     X center of expansion (1-512, 1 => LHS)
C      YZOOM   I     Y center of expansion (1-512, 1 => bot)
C      VRTRTC  L     Do on vertical retrace only?
C   Output:
C      IERR    I     error code of Z...XF: 0 -> ok, 2 -> input error
C   YZOOMC updates the /TVDEV/ common TVZOOM parameter
C   Virtual TV version (for TV by communication).
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      LOGICAL   VRTRTC
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF ((OP.NE.'WRIT') .OR. (BUFSW.NE.4)) GO TO 999
C                                        header + IO
      VRTRTC = BUFFER(1).EQ.1
      CALL YZOOMC (BUFFER(2), BUFFER(3), BUFFER(4), VRTRTC, IERR)
C
 999  RETURN
      END
