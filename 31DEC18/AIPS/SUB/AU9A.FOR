      SUBROUTINE AU9A (BRANCH)
C-----------------------------------------------------------------------
C! verbs to read TEK cursor and display pixel, sky, image values
C# POPS-appl Graphics
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000, 2002, 2007, 2009
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
C   Activate, read, interpret TEKTRONIX 4012 cursor for the following
C   verbs:
C      BRANCH   I      1 TKXY     find map pixel coordinate for cursor.
C                      2 TKPOS    find sky coordinate for cursor.
C                      3 TKVAL    find Y value.
C                      4 TKERASE  Erase screen
C   Outputs:
C      PIXXY    R(2)   map pixel positions set by TKXY.
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER UNITS*8, SPTXT*20, PREFIX*5, PRGNAM*6, CDUM*1
      DOUBLE PRECISION SKYPOS(3)
      REAL      RPOS(2), XFAC, XOFF, PIXMAX, PIXMIN, VY0, VY1, VX0, VX1,
     *   PY0, PY1, DX, DY, S, YVAL, PIXX(7), TKPAWS, VZ0, VZ1
      INTEGER   IOBLK(256), I, I4XTRA, ICH, IERR, IERR2, ILEN, IPX0,
     *   IPX1, IPY0, IPY1, ITX0, ITX1, ITY0, ITY1, ITYPE, IX, IY, IT,
     *   INOSL, IDROP(2), I2TMP1, IDUM
      LOGICAL   FLAG, TKOPEN, SPECTR
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTKS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGNAM /'AU9A '/
      DATA TKPAWS /1.5/
C-----------------------------------------------------------------------
      TKOPEN = .FALSE.
      ERRNUM = 15
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.4)) GO TO 980
      ERRNUM = 37
      IF (NTKDEV.LE.0) GO TO 980
      ERRNUM = 0
C                                       Open the TEK
      CALL ZTKOPN (IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TEKTRONIX OPEN ERROR'
         CALL MSGWRT (6)
         GO TO 970
         END IF
      TKOPEN = .TRUE.
      IF (BRANCH.EQ.4) GO TO 90
C                                       Read TEK image catalog header.
      IX = 0
      IY = 0
      CALL TKCATL ('READ', IX, IY, CATBLK, IERR)
      IF ((IERR.GT.0) .AND. (IERR.NE.11)) THEN
         MSGTXT = 'TEKTRONIX CATALOG ERROR'
         GO TO 960
         END IF
C                                       Set some header values.
      ITYPE = CATBLK(IIPLT)
      ITX0 = CATBLK(IICOR)
      ITY0 = CATBLK(IICOR+1)
      ITX1 = CATBLK(IICOR+2)
      ITY1 = CATBLK(IICOR+3)
      IPX0 = CATBLK(IIWIN)
      IPY0 = CATBLK(IIWIN+1)
      IPX1 = CATBLK(IIWIN+2)
      IPY1 = CATBLK(IIWIN+3)
C                                       Treat KNTR plots like CNTR
      IF (ITYPE.EQ.18) ITYPE = 2
C                                       Test for invalid plot type.
C                                       TKVAL and not slice plot.
      IF ((BRANCH.EQ.2) .AND. (ITYPE.NE.5)) THEN
         MSGTXT = 'TKVAL CAN BE USED ONLY WITH SLICE PLOTS.'
         GO TO 960
         END IF
C
 10   GO TO (15, 40, 40, 15, 40, 40), ITYPE
C                                       Invalid plot type.
         WRITE (MSGTXT,1010) ITYPE
         GO TO 960
C                                       Misc plot type.
 15      WRITE (MSGTXT,1015)
         GO TO 960
C                                       Profile plot type.
 20      WRITE (MSGTXT,1020)
         GO TO 960
C                                       Open TEKTRONIX device.
C                                       read cursor & close
 40   CALL TKCURS (IOBLK, IX, IY, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TEKTRONIX READ CURSOR ERROR'
         GO TO 960
         END IF
C                                       Check for pixel off plot.
      IF (((IX.LT.ITX0) .OR. (IX.GT.ITX1)) .OR.
     *   ((IY.LT.ITY0) .OR. (IY.GT.ITY1))) THEN
         WRITE (MSGTXT,1040) IX, IY, ITX0, ITX1, ITY0, ITY1
         GO TO 960
         END IF
C                                       Find the map pixel position.
C                                       Determine type of plot.
 50   GO TO (15, 60, 60, 20, 70, 60), ITYPE
C                                       Contour or grey scale
C                                       or polarization vector.
 60      RPOS(1) = REAL(IX - ITX0) / REAL(ITX1 - ITX0)
     *      * (IPX1 - IPX0)   +  IPX0
         RPOS(2) = REAL(IY - ITY0) / REAL(ITY1 - ITY0)
     *      * (IPY1 - IPY0)   +  IPY0
         GO TO 90
C                                       Slice file.
C                                       Set slice vector pixel coord.
 70   CONTINUE
         I4XTRA = IIOTH + 3
         IDROP(1) = CATBLK(IIOTH+1)
         IDROP(2) = CATBLK(IIOTH+2)
         INOSL = IPX1 - IPX0 + IDROP(1) + IDROP(2) + 1
         VX0 = CATR(I4XTRA  )
         VY0 = CATR(I4XTRA+1)
         VX1 = CATR(I4XTRA+2)
         VY1 = CATR(I4XTRA+3)
         VZ0 = CATR(I4XTRA+4)
         VZ1 = CATR(I4XTRA+5)
         SPECTR = ABS(VZ1-VZ0).GE.1.0
         IF (SPECTR) THEN
            DX = (VZ1 - VZ0) / INOSL
            VZ0 = VZ0 + DX * IDROP(1)
            VZ1 = VZ1 - DX * IDROP(2)
            RPOS(1) = REAL(IX - ITX0) / REAL(ITX1 - ITX0)
     *         * (VZ1 - VZ0) + VZ0
            RPOS(2) = 0.0
         ELSE
            DX = (VX1 - VX0) / INOSL
            DY = (VY1 - VY0) / INOSL
            VX0 = VX0 + DX * IDROP(1)
            VY0 = VY0 + DY * IDROP(1)
            VX1 = VX1 - DX * IDROP(2)
            VY1 = VY1 - DY * IDROP(2)
            RPOS(1) = REAL(IX - ITX0) / REAL(ITX1 - ITX0)
     *         * (VX1 - VX0) + VX0
            RPOS(2) = REAL(IX - ITX0) / REAL(ITX1 - ITX0)
     *         * (VY1 - VY0) + VY0
            END IF
C                                       Branch to correct verb.
 90   GO TO (100, 200, 300, 400), BRANCH
C-----------------------------------------------------------------------
C                                       TKPOS
C                                       Calc & print sky coordinates.
C-----------------------------------------------------------------------
C                                       print freq position for spectra
 100     IF (SPECTR) THEN
            CALL H2CHR (8, 1, CATH(KHCTP+4), UNITS)
            SKYPOS(1) = CATD(KDCRV+2) + (RPOS(1)-CATR(KRCRP+2)) *
     *         CATR(KRCIC+2)
            CALL AXSTRN (UNITS, SKYPOS(1), 2, I, MSGTXT)
            CALL MSGWRT (5)
            RPOS(1) = (VX0 + VX1) / 2.0
            RPOS(2) = (VY0 + VY1) / 2.0
            MSGTXT = 'Centered on XY coordinate:'
            CALL MSGWRT (5)
            END IF
C                                       then do sky coordinate
         LOCNUM = 1
         CALL MP2SKY (RPOS, SKYPOS, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR
            GO TO 960
            END IF
         IF ((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3)) CALL AXSTRN
     *      (CTYP(3,LOCNUM), SKYPOS(3), KLOCA(LOCNUM), NCHLAB(1,LOCNUM),
     *      SAXLAB(1,LOCNUM))
         WRITE (MSGTXT,1105)
         ICH = 9
         DO 110 I = 1,2
            I2TMP1 = I - 1
            CALL AXSTRN (CTYP(I,LOCNUM), SKYPOS(I), I2TMP1, ILEN, SPTXT)
            MSGTXT(ICH:) = SPTXT(:ILEN)
            ICH = ILEN + ICH + 2
 110        CONTINUE
         CALL MSGWRT (5)
         WRITE (MSGTXT,1105)
         ICH = 9
         DO 120 I = 1,2
            IF (NCHLAB(I,LOCNUM).LE.0) GO TO 120
               MSGTXT(ICH:) = SAXLAB(I,LOCNUM)(:NCHLAB(I,LOCNUM))
               ICH = NCHLAB(I,LOCNUM) + ICH + 2
 120        CONTINUE
         IF (ICH.GT.9) CALL MSGWRT (5)
         GO TO 980
C-----------------------------------------------------------------------
C                                       TKVAL
C-----------------------------------------------------------------------
C                                       Slice plot only
 200     PIXMAX = CATR(IRRAN+1)
         PIXMIN = CATR(IRRAN)
         XFAC = 39999.0 / (PIXMAX - PIXMIN)
         XOFF = 40000.0 - XFAC * PIXMAX
         PY0 = (IPY0 - XOFF) / XFAC
         PY1 = (IPY1 - XOFF) / XFAC
         YVAL = REAL(IY - ITY0) / REAL(ITY1 - ITY0)
     *      * (PY1 - PY0)  +  PY0
         CALL ADVRBS ('PIXVAL', 'R', 1, 0, IDUM, YVAL, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
C                                       Get rational units.
         S = YVAL
         CALL METSCA (S, PREFIX, FLAG)
         CALL H2CHR (8, 1, CATH(KHBUN), UNITS)
C                                       Print Y value.
         IF (FLAG) WRITE (MSGTXT,1200) S, UNITS
         IF (.NOT.FLAG) WRITE (MSGTXT,1202) S, PREFIX, UNITS
         CALL MSGWRT (5)
         GO TO 980
C-----------------------------------------------------------------------
C                                       TKXY
C-----------------------------------------------------------------------
 300     IF (SPECTR) THEN
            PIXX(3) = RPOS(1)
            RPOS(1) = (VX0 + VX1) / 2.0
            RPOS(2) = (VY0 + VY1) / 2.0
            WRITE (MSGTXT,1300) RPOS, PIXX(3)
         ELSE
            PIXX(3) = CATBLK(IIDEP)
            WRITE (MSGTXT,1300) RPOS
            END IF
         PIXX(1) = RPOS(1)
         PIXX(2) = RPOS(2)
         CALL MSGWRT (5)
         DO 310 I = 4,7
            PIXX(I) = CATBLK(IIDEP+I-3)
 310        CONTINUE
         CALL ADVRBS ('PIXXY', 'R', 7, 0, IDUM, PIXX, CDUM)
         GO TO 980
C-----------------------------------------------------------------------
C                                       TKERASE
C-----------------------------------------------------------------------
C                                       init the TEK catalog file
 400  CALL TKCATL ('INIT', IX, IY, CATBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1400) 'INIT CATALOG', IERR
         GO TO 960
         END IF
C                                       Clear screen.
      IT = 27
      CALL ZTKBUF (IT, 1, IERR)
      IT = 12
      IF (IERR.EQ.0) CALL ZTKBUF (IT, 1, IERR)
      IF (IERR.EQ.0) CALL TEKFLS (IERR)
      CALL ZDELAY (TKPAWS, IERR)
      IF (IERR.EQ.0) GO TO 980
      WRITE (MSGTXT,1400) 'PAGE SCREEN', IERR
C-----------------------------------------------------------------------
C                                       message and close
 960  CALL MSGWRT (6)
C
 970  ERRNUM = 101
 980  IF (TKOPEN) THEN
         CALL ZTKCLS (IERR2)
         IF (IERR2.NE.0) THEN
            MSGTXT = 'TEKTRONIX CLOSE ERROR'
            CALL MSGWRT (6)
            END IF
         END IF
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('INVALID MAP TYPE =',I7)
 1015 FORMAT ('INVALID PLOT TYPE = MISCELLANEOUS')
 1020 FORMAT ('INVALID PLOT TYPE = PROFILE')
 1040 FORMAT ('X,Y',2I6,' OUT OF RNG X0,X1,Y0,Y1=',4I6)
 1100 FORMAT ('TKPOS: ERROR',I3,' CONVERTING TO SKY COORDINATES')
 1105 FORMAT ('Skypos:   ')
 1200 FORMAT ('Y value =',E12.4,1X,A8)
 1202 FORMAT ('Y value =',F8.3,1X,A5,1X,A8)
 1300 FORMAT ('IMXY: ',3F7.2)
 1400 FORMAT ('ON ',A,' ERROR',I6)
      END
